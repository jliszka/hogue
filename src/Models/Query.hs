{-# LANGUAGE GADTs, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric, TypeOperators, FlexibleContexts #-}

module Models.Query where

import GHC.Generics
import Data.Word (Word32)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Database.MongoDB as Mongo
import Database.MongoDB ((=:), Val, Value(Int32))
import qualified Data.Bson as Bson

-- !#$%&*+./<=>?@\^|-~:

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

data (Show a, Val a) => Mod a =
    Inc Int         -- $+
  | Mul Int         -- $*
  | CurrentDate     -- currentDate
  | Set a           -- $:=
  | SetOnInsert a   -- $!:=
  | Unset           -- unset
  | Push a          -- >>$
  | PushAll [a]     -- *>>$
  | PopFirst        -- popFirst
  | PopLast         -- popLast
  | Pull a          -- $<<
  | PullAll [a]     -- $<<*
  | AddToSet a      -- $>=
  | AddAllToSet [a] -- $>=*


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

data Select m r where
  SelectAll :: (Generic m, GParse (Rep m)) => Select m m
  Select1 :: Val b => Field m a (Id b) -> Select m b
  Select1Opt :: Val b => Field m a (Maybe b) -> Select m (Maybe b)

data FieldValue a where
  FieldValueVal :: Val a => a -> FieldValue a
  FieldValueSchema :: Schema a => a -> FieldValue a

getFieldValueValue :: FieldValue a -> a
getFieldValueValue (FieldValueVal a) = a
getFieldValueValue (FieldValueSchema a) = a

data Id a = Id a
instance Functor Id where
  fmap f (Id a) = Id $ f a

data Field m a b where
  Field :: T.Text -> FieldValue a -> Field m a (Id a)
  OptField :: T.Text -> Maybe (FieldValue a) -> Field m a (Maybe a)

class Subselectable b b' where
  type Comb b b'
  mkField :: T.Text -> Field m a b -> Field a a' b' -> Field m a' (Comb b b')
  (/.) :: (Schema m, Schema a) => (m -> Field m a b) -> (a -> Field a a' b') -> m -> Field m a' (Comb b b')
  fld1 /. fld2 = \m -> mkField nn f1 f2
    where
      f1 = fld1 schema
      f2 = fld2 schema
      nn = T.concat [ getFieldName f1, ".", getFieldName f2 ]

instance Subselectable (Id a) (Id b) where
  type Comb (Id a) (Id b) = Id b
  mkField nn (Field _ _) (Field _ b) = Field nn $ b

instance Subselectable (Id a) (Maybe b) where
  type Comb (Id a) (Maybe b) = Maybe b
  mkField nn (Field _ _) (OptField _ mb) = OptField nn $ mb

instance Subselectable (Maybe a) (Id b) where
  type Comb (Maybe a) (Id b) = Maybe b
  mkField nn (OptField _ ma) (Field _ b) = OptField nn $ fmap (\a -> b) ma

instance Subselectable (Maybe a) (Maybe b) where
  type Comb (Maybe a) (Maybe b) = Maybe b
  mkField nn (OptField _ _) (OptField _ mb) = OptField nn $ mb


class Selectable r where
  type Sel r
  mkSelect :: Val a => Field m a r -> Select m (Sel r)
  select :: (Schema m, Val a) => (m -> Field m a r) -> Query m m -> Query m (Sel r)
  select fld q = q { sel = mkSelect $ fld schema }

instance Selectable (Id r) where
  type Sel (Id r) = r
  mkSelect f@(Field _ _) = Select1 f

instance Selectable (Maybe r) where
  type Sel (Maybe r) = Maybe r
  mkSelect f@(OptField _ _) = Select1Opt f

type QField m a = Field m a (Id a)
type OptQField m a = Field m a (Maybe a)

getFieldName :: Field m a b -> T.Text
getFieldName (Field name _) = name
getFieldName (OptField name _) = name

getFieldValue :: Field m a b -> b
getFieldValue (Field _ v) = Id $ getFieldValueValue v
getFieldValue (OptField _ v) = fmap getFieldValueValue v

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

instance GLookup (Field m a b) where
  glookup doc (Field n (FieldValueVal _)) = Field n $ FieldValueVal $ maybe err id $ Bson.lookup n doc
    where
      err = undefined -- TODO: error "friendly message"
  glookup doc (Field n (FieldValueSchema _)) = Field n $ FieldValueSchema $ fromBson subdoc
    where
      Bson.Doc subdoc = Bson.valueAt n doc
  glookup doc (OptField n (Just (FieldValueVal _))) = OptField n $ fmap FieldValueVal $ Bson.lookup n doc
  glookup doc (OptField n (Just (FieldValueSchema _))) = OptField n $ fmap FieldValueSchema $ fmap fromBson subdoc
    where
      subdoc = case Bson.lookup n doc of
        Just (Bson.Doc d) -> Just d
        Nothing -> Nothing

{-
withValue :: Field m a b -> c -> Field m c d
withValue (Field name _) a = Field name a
withValue (OptField name _) a = OptField name a
withValue (EField name _) a = EField name a
withValue (OptEField name _) a = OptEField name a

data DummyModel a where
  DummyModel :: Field (DummyModel a) a (Maybe a) -> DummyModel a

data Id a = Id a

class Collapse a b where
  type C a b
  collapse :: a -> b -> C a b

instance Collapse (Maybe a) (Maybe b) where
  type C (Maybe a) (Maybe b) = Maybe b
  collapse a mb = mb

instance Collapse (Id a) (Maybe b) where
  type C (Id a) (Maybe b) = Maybe b
  collapse a mb = mb

instance Collapse (Maybe a) (Id b) where
  type C (Maybe a) (Id b) = Maybe b
  collapse ma (Id b) = Just b

instance Collapse (Id a) (Id b) where
  type C (Id a) (Id b) = b
  collapse a (Id b) = b
-}


-- SCHEMA

class (Generic m, GParse (Rep m)) => Schema m where
  schema :: m

  field :: Val a => T.Text -> Field m a (Id a)
  field name = Field name (FieldValueVal undefined)

  optfield :: Val a => T.Text -> Field m a (Maybe a)
  optfield name = OptField name (Just (FieldValueVal undefined))

  efield :: Schema a => T.Text -> Field m a (Id a)
  efield name = Field name (FieldValueSchema undefined)

  optefield :: Schema a => T.Text -> Field m a (Maybe a)
  optefield name = OptField name (Just (FieldValueSchema undefined))

  (~.) :: Show a => m -> (m -> Field m a b) -> b
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

  -- QUERY OPERATORS

  mkClause :: Val c => (m -> Field m a b) -> Cond c -> Clause m
  mkClause fld cond = Clause (getFieldName $ fld schema) cond

  (~>) :: (Show a, Val a) => (m -> Field m a b) -> Cond a -> Clause m
  fld ~> cond = mkClause fld cond

  ($=) :: (Show a, Val a) => (m -> Field m a b) -> a -> Clause m
  fld $= a = mkClause fld $ Eq a

  ($/=) :: (Show a, Val a) => (m -> Field m a b) -> a -> Clause m
  fld $/= a = mkClause fld $ Neq a

  ($>) :: (Show a, Val a) => (m -> Field m a b) -> a -> Clause m
  fld $> a = mkClause fld $ Gt a

  ($<) :: (Show a, Val a) => (m -> Field m a b) -> a -> Clause m
  fld $< a = mkClause fld $ Lt a

  ($>=) :: (Show a, Val a) => (m -> Field m a b) -> a -> Clause m
  fld $>= a = mkClause fld $ GtEq a

  ($<=) :: (Show a, Val a) => (m -> Field m a b) -> a -> Clause m
  fld $<= a = mkClause fld $ LtEq a

  ($=*) :: (Show a, Val a) => (m -> Field m a b) -> [a] -> Clause m
  fld $=* a = mkClause fld $ In a

  ($/=*) :: (Show a, Val a) => (m -> Field m a b) -> [a] -> Clause m
  fld $/=* a = mkClause fld $ NotIn a

  ($*=) :: (Show a, Val a) => (m -> Field m [a] b) -> a -> Clause m
  fld $*= a = mkClause fld $ Contains a

  ($*=*) :: (Show a, Val a) => (m -> Field m [a] b) -> [a] -> Clause m
  fld $*=* a = mkClause fld $ All a

  ($?) :: (Show a, Val a) => (m -> Field m a b) -> Bool -> Clause m
  fld $? b = mkClause fld $ (Exists b :: Cond Bool)

  ($#) :: (Show a, Val a) => (m -> Field m a b) -> Int -> Clause m
  fld $# n = mkClause fld $ (Size n :: Cond Int)

  ($:) :: (Show a, Val a) => (m -> Field m a b) -> MongoType -> Clause m
  fld $: t = mkClause fld $ (Type t :: Cond MongoType)

  -- UPDATE OPERATORS


  -- SUBFIELDS

{-
  (/.) :: (Schema m, Schema e, Val a) => (m -> Field m e e') -> (e -> Field e a a') -> m -> Field m a a'
  outer /. inner =
    let
      f1 = outer schema
      f2 = inner schema
      n1 = getFieldName f1
      n2 = getFieldName f2
      nn = T.concat [ n1, ".", n2 ]
    in \m -> case (f1, f2) of
      (Field _ _, Field _ v) -> Field nn v
      (OptField _ _, Field _ v) -> OptField nn (Just v)
      (_, OptField _ v) -> OptField nn v
-}

  -- QUERY OPTIONS

  limit :: Word32 -> Query m r -> Query m r
  limit n q = q { lim = n }

  asc :: Val a => (m -> Field m a b) -> Query m r -> Query m r
  asc fld q = q { srt = Asc (getFieldName $ fld schema) : srt q }

  desc :: Val a => (m -> Field m a b) -> Query m r -> Query m r
  desc fld q = q { srt = Desc (getFieldName $ fld schema) : srt q }


  -- QUERY EXECUTION

  fetch :: Queryable m => DB -> Query m r -> IO [r]
  fetch db q = fmap (fmap $ applySelect $ sel q) docs
    where
      docs = fetchBson db q
      applySelect :: Schema m => Select m r -> Bson.Document -> r
      applySelect SelectAll doc = fromBson doc
      applySelect (Select1 f) doc = maybe undefined id $ doc Bson.!? (getFieldName f)
      applySelect (Select1Opt f) doc = doc Bson.!? (getFieldName f)

  fetchBson :: Queryable m => DB -> Query m r -> IO [Bson.Document]
  fetchBson db q = Mongo.access (dbPipe db) Mongo.master (dbName db) $ Mongo.find (mkQuery q) >>= Mongo.rest
    where
      mkQuery :: Queryable m => Query m r -> Mongo.Query
      mkQuery q = (Mongo.select cls coll) {
        Mongo.limit = limit,
        Mongo.sort = sort,
        Mongo.project = project
      }
        where
          coll = collection q
          cls = clauses q $. toBson
          sort = srt q $. reverse $. toBson
          project = sel q $. toBson
          limit = lim q




-- SHOW INSTANCES

instance Show a => Show (Field m a b) where
  show (Field name val) = T.unpack $ T.concat [ name, "=", T.pack $ show val ]
  show (OptField name val) = T.unpack $ T.concat [ name, "=", T.pack $ show val ]

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
  toBson cls = mkClauses cls
    where
      mkClauses :: [Clause m] -> [Bson.Field]
      mkClauses cls = map mkGroup groups
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

