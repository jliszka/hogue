{-# LANGUAGE GADTs, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric, TypeOperators, FlexibleContexts #-}

module Models.Query where

import GHC.Generics
import Data.Word (Word32)
import Data.Aeson (ToJSON, toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.List as L
import qualified Data.Text as T
import qualified Database.MongoDB as Mongo
import Database.MongoDB ((=:), Val, Value(Int32))
import qualified Data.Bson as Bson

-- !#$%&*+./<=>?@\^|-~:

data Id a = Id a
instance Functor Id where
  fmap f (Id a) = Id $ f a

($.) :: a -> (a -> b) -> b
($.) a f = f a
infixl 9 $.

data DB = DB {
  dbPipe :: Mongo.Pipe,
  dbName :: T.Text
}

getDB :: T.Text -> IO DB
getDB dbName = do
  pipe <- Mongo.connect $ Mongo.host "127.0.0.1"
  return $ DB pipe dbName


-- QUERY

data Query m r = Query {
  clauses :: [Clause m],
  lim :: Word32,
  srt :: [Sort],
  sel :: Select m r
}

data Clause m =
  forall a . (Show a, Val a) =>
  Clause {
    clauseFieldName :: T.Text,
    clauseCond :: Cond a
  }

data (Show a, Val a) => Cond a =
    Eq a       -- $=
  | Neq a      -- $/=
  | In [a]     -- $=*
  | NotIn [a]  -- $/=*
  | Gt a       -- $>
  | Lt a       -- $<
  | GtEq a     -- $>=
  | LtEq a     -- $<=
  | Contains a -- $*=
  | All [a]    -- $*=*
  | Size Int   -- $#
  | Type MongoType -- $:
  | Exists Bool    -- $?
  deriving Show

data MongoType =
  TDouble | TString | TObject | TArray | TBinary | TObjectId | TBoolean
  | TDate | TNull | TRegex | TSymbol | TInt32 | TTimestamp | TInt64
  deriving (Show, Eq)

instance Val MongoType where
  val t = case t of
    TDouble -> Int32 1
    TString -> Int32 2
    TObject -> Int32 3
    TArray -> Int32 4
    TBinary -> Int32 5
    TObjectId -> Int32 7
    TBoolean -> Int32 8
    TDate -> Int32 9
    TNull -> Int32 10
    TRegex -> Int32 11
    TSymbol -> Int32 14
    TInt32 -> Int32 16
    TTimestamp -> Int32 17
    TInt64 -> Int32 18
  cast' doc = undefined

data Sort =
    Asc T.Text
  | Desc T.Text
  deriving Show


-- MODIFY

data Modify m = Modify {
  query :: Query m m,
  modifyClauses :: [ModifyClause m]
}

data ModifyClause m =
  forall a . Val a =>
  ModifyClause {
    modifyClauseOp :: ModOp,
    modifyClauseFieldName :: T.Text,
    modifyClauseVal :: a
  }

data ModOp =
    Inc         -- $+
  | Mul         -- $*
  | CurrentDate -- currentDate
  | Set         -- $:=
  | SetOnInsert -- $!:=
  | Unset       -- unset
  | Push        -- >>$
  | PushAll     -- *>>$
  | PopFirst    -- popFirst
  | PopLast     -- popLast
  | Pull        -- $<<
  | PullAll     -- $<<*
  | AddToSet    -- $>=
  | AddAllToSet -- $>=*
  deriving Eq


-- FIELDS

type Field m a = QField m a (Id a)
type OptField m a = QField m a (Maybe a)

data QField m a b where
  QField :: T.Text -> FieldValue a -> QField m a (Id a)
  OptQField :: T.Text -> Maybe (FieldValue a) -> QField m a (Maybe a)

data FieldValue a where
  FieldValueVal :: Val a => a -> FieldValue a
  FieldValueSchema :: Schema a => a -> FieldValue a

getFieldName :: QField m a b -> T.Text
getFieldName (QField name _) = name
getFieldName (OptQField name _) = name

getFieldValue :: QField m a b -> b
getFieldValue (QField _ v) = Id $ getFieldValueValue v
getFieldValue (OptQField _ v) = fmap getFieldValueValue v

getFieldValueValue :: FieldValue a -> a
getFieldValueValue (FieldValueVal a) = a
getFieldValueValue (FieldValueSchema a) = a


-- SELECT

data Select m r where
  SelectAll :: (Generic m, GParse (Rep m)) => Select m m
  Select1 :: Val b => QField m a (Id b) -> Select m b
  Select1Opt :: Val b => QField m a (Maybe b) -> Select m (Maybe b)

class Selectable r where
  type Sel r
  mkSelect :: Val a => QField m a r -> Select m (Sel r)
  select :: (Schema m, Val a) => (m -> QField m a r) -> Query m m -> Query m (Sel r)
  select fld q = q { sel = mkSelect $ fld schema }

instance Selectable (Id r) where
  type Sel (Id r) = r
  mkSelect f@(QField _ _) = Select1 f

instance Selectable (Maybe r) where
  type Sel (Maybe r) = Maybe r
  mkSelect f@(OptQField _ _) = Select1Opt f


-- SUBFIELDS

class Subselectable b b' where
  type Comb b b'
  mkField :: T.Text -> QField m a b -> QField a a' b' -> QField m a' (Comb b b')
  (/.) :: (Schema m, Schema a) => (m -> QField m a b) -> (a -> QField a a' b') -> m -> QField m a' (Comb b b')
  fld1 /. fld2 = \m -> mkField nn f1 f2
    where
      f1 = fld1 schema
      f2 = fld2 schema
      nn = T.concat [ getFieldName f1, ".", getFieldName f2 ]

instance Subselectable (Id a) (Id b) where
  type Comb (Id a) (Id b) = Id b
  mkField nn (QField _ _) (QField _ b) = QField nn $ b

instance Subselectable (Id a) (Maybe b) where
  type Comb (Id a) (Maybe b) = Maybe b
  mkField nn (QField _ _) (OptQField _ mb) = OptQField nn $ mb

instance Subselectable (Maybe a) (Id b) where
  type Comb (Maybe a) (Id b) = Maybe b
  mkField nn (OptQField _ ma) (QField _ b) = OptQField nn $ fmap (\a -> b) ma

instance Subselectable (Maybe a) (Maybe b) where
  type Comb (Maybe a) (Maybe b) = Maybe b
  mkField nn (OptQField _ _) (OptQField _ mb) = OptQField nn $ mb


-- GENERIC PARSING

class GParse f where
  gparse :: Bson.Document -> f m -> f m

instance (GParse a, GParse b) => GParse (a :*: b) where
  gparse doc (a :*: b) = gparse doc a :*: gparse doc b

instance GParse a => GParse (M1 i c a) where
  gparse doc (M1 a) = M1 $ gparse doc a

instance GLookup a => GParse (K1 i a) where
  gparse doc (K1 a) = K1 $ glookup doc a


class GLookup f where
  glookup :: Bson.Document -> f -> f

instance GLookup (QField m a b) where
  glookup doc (QField n (FieldValueVal _)) = QField n $ FieldValueVal $ maybe err id $ Bson.lookup n doc
    where
      err = undefined -- TODO: error "friendly message"
  glookup doc (QField n (FieldValueSchema _)) = QField n $ FieldValueSchema $ fromBson subdoc
    where
      Bson.Doc subdoc = Bson.valueAt n doc
  glookup doc (OptQField n (Just (FieldValueVal _))) = OptQField n $ fmap FieldValueVal $ Bson.lookup n doc
  glookup doc (OptQField n (Just (FieldValueSchema _))) = OptQField n $ fmap FieldValueSchema $ fmap fromBson subdoc
    where
      subdoc = case Bson.lookup n doc of
        Just (Bson.Doc d) -> Just d
        Nothing -> Nothing


-- SCHEMA

class MkField f a b where
  field :: Val a => T.Text -> f a b
  efield :: Schema a => T.Text -> f a b

instance MkField (QField m) a (Id a) where
  field name = QField name (FieldValueVal undefined)
  efield name = QField name (FieldValueSchema undefined)

instance MkField (QField m) a (Maybe a) where
  field name = OptQField name (Just (FieldValueVal undefined))
  efield name = OptQField name (Just (FieldValueSchema undefined))

class (ToJSON m, Generic m, GParse (Rep m)) => Schema m where
  schema :: m

  (~.) :: Show a => m -> (m -> QField m a b) -> b
  m ~. fld = getFieldValue $ fld m

  fromBson :: Bson.Document -> m
  fromBson doc = fromBson' doc schema
    where
      fromBson' :: Schema m => Bson.Document -> m -> m
      fromBson' doc schema = to $ gparse doc $ from schema


class Schema m => Queryable m where
  collection :: Query m r -> T.Text

  find :: [Clause m] -> Query m m
  find cls = Query cls 0 [] SelectAll

  modify :: [ModifyClause m] -> Query m m -> Modify m
  modify cls q = Modify q cls


  -- QUERY OPERATORS

  mkClause :: Val c => (m -> QField m a b) -> Cond c -> Clause m
  mkClause fld cond = Clause (getFieldName $ fld schema) cond

  (~>) :: (Show a, Val a) => (m -> QField m a b) -> Cond a -> Clause m
  (~>) = mkClause

  eqs :: (Show a, Val a) => (m -> QField m a b) -> a -> Clause m
  eqs fld a = mkClause fld $ Eq a
  ($=) :: (Show a, Val a) => (m -> QField m a b) -> a -> Clause m
  ($=) = eqs

  neq :: (Show a, Val a) => (m -> QField m a b) -> a -> Clause m
  neq fld a = mkClause fld $ Neq a
  ($/=) :: (Show a, Val a) => (m -> QField m a b) -> a -> Clause m
  ($/=) = neq

  gt :: (Show a, Val a) => (m -> QField m a b) -> a -> Clause m
  gt fld a = mkClause fld $ Gt a
  ($>) :: (Show a, Val a) => (m -> QField m a b) -> a -> Clause m
  ($>) = gt

  lt :: (Show a, Val a) => (m -> QField m a b) -> a -> Clause m
  lt fld a = mkClause fld $ Lt a
  ($<) :: (Show a, Val a) => (m -> QField m a b) -> a -> Clause m
  ($<) = lt

  gte :: (Show a, Val a) => (m -> QField m a b) -> a -> Clause m
  gte fld a = mkClause fld $ GtEq a
  ($>=) :: (Show a, Val a) => (m -> QField m a b) -> a -> Clause m
  ($>=) = gte

  lte :: (Show a, Val a) => (m -> QField m a b) -> a -> Clause m
  lte fld a = mkClause fld $ LtEq a
  ($<=) :: (Show a, Val a) => (m -> QField m a b) -> a -> Clause m
  ($<=) = lte

  isIn :: (Show a, Val a) => (m -> QField m a b) -> [a] -> Clause m
  isIn fld a = mkClause fld $ In a
  ($=*) :: (Show a, Val a) => (m -> QField m a b) -> [a] -> Clause m
  ($=*) = isIn

  notIn :: (Show a, Val a) => (m -> QField m a b) -> [a] -> Clause m
  notIn fld a = mkClause fld $ NotIn a
  ($/=*) :: (Show a, Val a) => (m -> QField m a b) -> [a] -> Clause m
  ($/=*) = notIn

  contains :: (Show a, Val a) => (m -> QField m [a] b) -> a -> Clause m
  contains fld a = mkClause fld $ Contains a
  ($*=) :: (Show a, Val a) => (m -> QField m [a] b) -> a -> Clause m
  ($*=) = contains

  containsAll :: (Show a, Val a) => (m -> QField m [a] b) -> [a] -> Clause m
  containsAll fld a = mkClause fld $ All a
  ($*=*) :: (Show a, Val a) => (m -> QField m [a] b) -> [a] -> Clause m
  ($*=*) = containsAll

  exists :: (Show a, Val a) => (m -> QField m a b) -> Bool -> Clause m
  exists fld b = mkClause fld $ (Exists b :: Cond Bool)
  ($?) :: (Show a, Val a) => (m -> QField m a b) -> Bool -> Clause m
  ($?) = exists

  size :: (Show a, Val a) => (m -> QField m a b) -> Int -> Clause m
  size fld n = mkClause fld $ (Size n :: Cond Int)
  ($#) :: (Show a, Val a) => (m -> QField m a b) -> Int -> Clause m
  ($#) = size

  hasType :: (Show a, Val a) => (m -> QField m a b) -> MongoType -> Clause m
  hasType fld t = mkClause fld $ (Type t :: Cond MongoType)
  ($:) :: (Show a, Val a) => (m -> QField m a b) -> MongoType -> Clause m
  ($:) = hasType


  -- UPDATE OPERATORS

  mkModClause :: Val c => ModOp -> (m -> QField m a b) -> c -> ModifyClause m
  mkModClause op fld a = ModifyClause op (getFieldName $ fld schema) a

  inc :: (Num a, Val a) => (m -> QField m a b) -> a -> ModifyClause m
  inc fld n = mkModClause Inc fld n
  ($+) :: (Num a, Val a) => (m -> QField m a b) -> a -> ModifyClause m
  ($+) = inc

  mul :: (Num a, Val a) => (m -> QField m a b) -> a -> ModifyClause m
  mul fld n = mkModClause Mul fld n
  ($*) :: (Num a, Val a) => (m -> QField m a b) -> a -> ModifyClause m
  ($*) = mul

  setTo :: Val a => (m -> QField m a b) -> a -> ModifyClause m
  setTo fld a = mkModClause Set fld a
  ($:=) :: Val a => (m -> QField m a b) -> a -> ModifyClause m
  ($:=) = setTo

  setOnInsert :: Val a => (m -> QField m a b) -> a -> ModifyClause m
  setOnInsert fld a = mkModClause SetOnInsert fld a
  ($!:=) :: Val a => (m -> QField m a b) -> a -> ModifyClause m
  ($!:=) = setOnInsert

  unset :: (m -> QField m a b) -> ModifyClause m
  unset fld = mkModClause Unset fld True

  currentDate :: (m -> QField m a b) -> ModifyClause m
  currentDate fld = mkModClause CurrentDate fld True

  popFirst :: (m -> QField m a b) -> ModifyClause m
  popFirst fld = mkModClause PopFirst fld True

  popLast :: (m -> QField m a b) -> ModifyClause m
  popLast fld = mkModClause PopLast fld True

  push :: Val a => (m -> QField m [a] b) -> a -> ModifyClause m
  push fld a = mkModClause Push fld a

  pushAll :: Val a => (m -> QField m [a] b) -> [a] -> ModifyClause m
  pushAll fld a = mkModClause PushAll fld [a]

  pull :: Val a => (m -> QField m [a] b) -> a -> ModifyClause m
  pull fld a = mkModClause Pull fld a

  pullAll :: Val a => (m -> QField m [a] b) -> [a] -> ModifyClause m
  pullAll fld a = mkModClause PullAll fld [a]

  addToSet :: Val a => (m -> QField m [a] b) -> a -> ModifyClause m
  addToSet fld a = mkModClause AddToSet fld a

  addAllToSet :: Val a => (m -> QField m [a] b) -> [a] -> ModifyClause m
  addAllToSet fld a = mkModClause AddToSet fld [a]


  -- QUERY OPTIONS

  limit :: Word32 -> Query m r -> Query m r
  limit n q = q { lim = n }

  asc :: Val a => (m -> QField m a b) -> Query m r -> Query m r
  asc fld q = q { srt = Asc (getFieldName $ fld schema) : srt q }

  desc :: Val a => (m -> QField m a b) -> Query m r -> Query m r
  desc fld q = q { srt = Desc (getFieldName $ fld schema) : srt q }


  -- QUERY EXECUTION

  fetch :: Queryable m => DB -> Query m r -> IO [r]
  fetch db q = fmap (fmap $ applySelect $ sel q) $ fetchBson db q

  fetchOne :: Queryable m => DB -> Query m r -> IO (Maybe r)
  fetchOne db q = fmap (fmap $ applySelect $ sel q) $ doQuery db q Mongo.findOne

  count :: Queryable m => DB -> Query m m -> IO Int
  count db q = doQuery db q Mongo.count

  updateOne :: DB -> Modify m -> IO ()
  updateOne db mod = doModify db mod Mongo.replace

  upsertOne :: DB -> Modify m -> IO ()
  upsertOne db mod = doModify db mod Mongo.upsert

  updateMulti :: DB -> Modify m -> IO ()
  updateMulti db mod = doModify db mod Mongo.modify

  delete :: Queryable m => DB -> Query m m -> IO ()
  delete db q = doQuery db q (Mongo.delete . Mongo.selection)

  deleteOne :: Queryable m => DB -> Query m m -> IO ()
  deleteOne db q = doQuery db q (Mongo.deleteOne . Mongo.selection)

  applySelect :: Schema m => Select m r -> Bson.Document -> r
  applySelect SelectAll doc = fromBson doc
  applySelect (Select1 f) doc = maybe undefined id $ doc Bson.!? (getFieldName f)
  applySelect (Select1Opt f) doc = doc Bson.!? (getFieldName f)

  doModify :: DB -> Modify m -> (Mongo.Selection -> Bson.Document -> Mongo.Action IO ()) -> IO ()
  doModify db (Modify q modClauses) f = exec db $ f selection (toBson modClauses)
    where
      selection = Mongo.select cls coll
      cls = toBson $ clauses q
      coll = collection q

  fetchBson :: Queryable m => DB -> Query m r -> IO [Bson.Document]
  fetchBson db q = doQuery db q (\q -> Mongo.find q >>= Mongo.rest)

  doQuery :: Queryable m => DB -> Query m r -> (Mongo.Query -> Mongo.Action IO a) -> IO a
  doQuery db q f = exec db $ f (mkQuery q)
    where
      mkQuery :: Queryable m => Query m r -> Mongo.Query
      mkQuery q = (Mongo.select cls coll) {
        Mongo.limit = limit,
        Mongo.sort = sort,
        Mongo.project = project
      }
        where
          coll = collection q
          cls = toBson $ clauses q
          sort = toBson $ reverse $ srt q
          project = toBson $ sel q
          limit = lim q

exec :: DB -> Mongo.Action IO a -> IO a
exec db act = Mongo.access (dbPipe db) Mongo.master (dbName db) $ act

-- SHOW INSTANCES

instance Show a => Show (QField m a b) where
  show (QField name val) = show val
  show (OptQField name val) = show val

instance Show a => Show (FieldValue a) where
  show (FieldValueVal a) = show a
  show (FieldValueSchema a) = show a

instance Show (Clause m) where
  show (Clause n c) = show n ++ ": " ++ show c

instance Queryable m => Show (Query m r) where
  show q = T.concat (intro ++ query ++ select ++ outro ++ sort ++ limit) $. T.unpack
    where
      intro = [ "db.", collection q, ".find(" ]
      outro = [ ")" ]
      query = [ clauses q $. toBson $. Bson.Doc $. bsonToText ]
      ifNotEmpty [] f = []
      ifNotEmpty xs f = f xs
      select = mkSelect $ sel q
        where
          mkSelect SelectAll = []
          mkSelect (Select1 f) = [ ", ", Select1 f $. toBson $. Bson.Doc $. bsonToText ]
      sort = ifNotEmpty (srt q) $ \srts -> [ ".sort(" , srts $. reverse $. toBson  $. Bson.Doc $. bsonToText, ")" ]
      limit = if lim q > 0 then [ ".limit(" , T.pack $ show $ lim q, ")" ] else []
      bsonToText :: Bson.Value -> T.Text
      bsonToText (Bson.Doc fields) = T.concat [ "{ ", fieldsToText fields, " }" ]
        where
          fieldsToText fields = T.intercalate ", " $ map fieldToText fields
          fieldToText (label Bson.:= value) = T.concat [ label, ": ", bsonToText value ]
      bsonToText value = T.pack $ show value



-- BSON SERIALIZATION

class ToBson a where
  toBson :: a -> Bson.Document

class ToBsonField a where
  toBsonField :: a -> Bson.Field

instance ToBson [Clause m] where
  toBson cls = map mkGroup groups
    where
      groups = L.groupBy (\cl1 cl2 -> clauseFieldName cl1 == clauseFieldName cl2) cls
      isEqClause :: Clause m -> Bool
      isEqClause (Clause _ (Eq a)) = True
      isEqClause (Clause _ (Contains a)) = True
      isEqClause _ = False
      mkGroup :: [Clause m] -> Bson.Field
      mkGroup cls = case L.find isEqClause cls of
          Just (Clause fieldName (Eq a)) -> fieldName =: Bson.val a
          Just (Clause fieldName (Contains a)) -> fieldName =: Bson.val a
          Nothing -> (clauseFieldName $ head cls) =: map toBsonField cls

instance ToBson [ModifyClause m] where
  toBson cls = map mkGroup groups
    where
      groups = L.groupBy (\cl1 cl2 -> modifyClauseOp cl1 == modifyClauseOp cl2) cls
      mkGroup :: [ModifyClause m] -> Bson.Field
      mkGroup cls = mkGroupWithOp (modifyClauseOp $ head cls) cls

      mkGroupWithOp :: ModOp -> [ModifyClause m] -> Bson.Field
      mkGroupWithOp Inc cls = "$inc" =: map toFieldValue cls
      mkGroupWithOp Mul cls = "$mul" =: map toFieldValue cls
      mkGroupWithOp Set cls = "$set" =: map toFieldValue cls
      mkGroupWithOp Push cls = "$push" =: map toFieldValue cls
      mkGroupWithOp PushAll cls = "$push" =: map (toFieldValuesWith "$each") cls
      mkGroupWithOp PopFirst cls = "$pop" =: map (toFieldWith (-1 :: Int)) cls
      mkGroupWithOp PopLast cls = "$pop" =: map (toFieldWith (1 :: Int)) cls
      mkGroupWithOp Pull cls = "$pull" =: map toFieldValue cls
      mkGroupWithOp PullAll cls = "$pullAll" =: map toFieldValue cls
      mkGroupWithOp AddToSet cls = "$addToSet" =: map toFieldValue cls
      mkGroupWithOp AddAllToSet cls = "$addToSet" =: map (toFieldValuesWith "$each") cls
      mkGroupWithOp Unset cls = "$unset" =: map (toFieldWith True) cls
      mkGroupWithOp SetOnInsert cls = "$setOnInsert" =: map toFieldValue cls
      mkGroupWithOp CurrentDate cls = "$currentDate" =: map (toFieldWith True) cls


      toFieldValue :: ModifyClause m -> Bson.Field
      toFieldValue (ModifyClause _ name val) = name =: val

      toFieldWith :: Val a => a -> ModifyClause m -> Bson.Field
      toFieldWith val (ModifyClause _ name _) = name =: val

      toFieldValuesWith :: T.Text -> ModifyClause m -> Bson.Field
      toFieldValuesWith t (ModifyClause _ name vals) = name =: [ t =: vals ]


instance ToBson (Select m a) where
  toBson SelectAll = []
  toBson (Select1 f) = [ getFieldName f =: (1 :: Int) ]
  toBson (Select1Opt f) = [ getFieldName f =: (1 :: Int) ]

instance ToBson [Sort] where
  toBson fields = map toBsonField fields

instance ToBsonField (Clause m) where
  toBsonField (Clause _ cond) = toBsonField cond

instance Val a => ToBsonField (Cond a) where
  toBsonField cond = mkCond cond
    where
      mkCond :: Val a => Cond a -> Bson.Field
      mkCond (Neq a) = "$neq" =: Bson.val a
      mkCond (In a) = "$in" =: Bson.val a
      mkCond (NotIn a) = "$nin" =: Bson.val a
      mkCond (Gt a) = "$gt" =: Bson.val a
      mkCond (Lt a) = "$lt" =: Bson.val a
      mkCond (GtEq a) = "$gte" =: Bson.val a
      mkCond (LtEq a) = "$lte" =: Bson.val a
      mkCond (All a) = "$all" =: Bson.val a
      mkCond (Exists b) = "$exists" =: Bson.val b
      mkCond (Size n) = "$size" =: Bson.val n
      mkCond (Type t) = "$type" =: Bson.val t

instance ToBsonField Sort where
  toBsonField (Asc fieldName) = fieldName =: (1 :: Int)
  toBsonField (Desc fieldName) = fieldName =: (-1 :: Int)


-- JSON SERIALIZATION

instance (ToJSON a) => ToJSON (QField m a b) where
  toJSON (QField m a) = toJSON a
  toJSON (OptQField m a) = toJSON a

instance (ToJSON a) => ToJSON (FieldValue a) where
  toJSON (FieldValueVal a) = toJSON a
  toJSON (FieldValueSchema a) = toJSON a

instance ToJSON Bson.ObjectId where
  toJSON oid = Aeson.String $ T.pack $ show oid

