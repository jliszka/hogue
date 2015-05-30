{-# LANGUAGE GADTs #-}
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
  | Desc T.Text deriving Show

data Select m r where
  SelectAll :: (Generic m, GParse (Rep m)) => Select m m
  Select1 :: Val r => Field m r -> Select m r

data Field m a where
  Field :: Val a => T.Text -> a -> Field m a
  EField :: (Schema a, Generic a, GParse (Rep a)) => T.Text -> a -> Field m a

instance Show a => Show (Field m a) where
  show (Field name val) = T.unpack $ T.concat [ name, "=", T.pack $ show val ]
  show (EField name val) = T.unpack $ T.concat [ name, "=", T.pack $ show val ]

instance Show (Clause m) where
  show (Clause n c) = show n ++ ": " ++ show c


-- GENERIC PARSING

class GFields f where
  gfields :: f m -> [String]

instance (GFields a, GFields b) => GFields (a :*: b) where
  gfields (a :*: b) = gfields a ++ gfields b

instance GFields a => GFields (M1 i c a) where
  gfields (M1 a) = gfields a

instance GField a => GFields (K1 i a) where
  gfields (K1 a) = maybe [] (:[]) $ gfield a

class GField f where
  gfield :: f -> Maybe String

instance GField (Field m a) where
  gfield (Field n _) = Just (T.unpack n)


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

instance GLookup (Field m a) where
  glookup doc (Field n _) = Field n $ maybe undefined id $ Bson.cast' $ Bson.valueAt n doc
  glookup doc (EField n _) = EField n $ to $ gparse subdoc $ from (schema :: Schema a => a)
    where
      Bson.Doc subdoc = Bson.valueAt n doc


-- SCHEMA

class (Generic m, GParse (Rep m)) => Schema m where
  schema :: m

  field :: Val a => T.Text -> Field m a
  field name = Field name undefined

  efield :: Schema a => T.Text -> Field m a
  efield name = EField name undefined

  (~.) :: Show a => m -> (m -> Field m a) -> a
  m ~. fld = let Field _ a = fld m in a

  val :: Val a => a -> Field m a
  val a = Field undefined a

  fromBson :: Bson.Document -> m
  fromBson doc = to $ gparse doc $ from (schema :: m)


class Schema m => Queryable m where
  collection :: Query m r -> T.Text

  find :: [Clause m] -> Query m m
  find cls = Query cls 0 [] SelectAll

  -- QUERY OPERATORS

  mkClause :: Val b => (m -> Field m a) -> Cond b -> Clause m
  mkClause fld cond = Clause (mkName $ fld schema) cond
    where
      mkName :: Field m a -> T.Text
      mkName (Field name _) = name
      mkName (EField name _) = name

  (~>) :: (Show a, Val a) => (m -> Field m a) -> Cond a -> Clause m
  fld ~> cond = mkClause fld cond

  ($=) :: (Show a, Val a) => (m -> Field m a) -> a -> Clause m
  fld $= a = mkClause fld $ Eq a

  ($/=) :: (Show a, Val a) => (m -> Field m a) -> a -> Clause m
  fld $/= a = mkClause fld $ Neq a

  ($>) :: (Show a, Val a) => (m -> Field m a) -> a -> Clause m
  fld $> a = mkClause fld $ Gt a

  ($<) :: (Show a, Val a) => (m -> Field m a) -> a -> Clause m
  fld $< a = mkClause fld $ Lt a

  ($>=) :: (Show a, Val a) => (m -> Field m a) -> a -> Clause m
  fld $>= a = mkClause fld $ GtEq a

  ($<=) :: (Show a, Val a) => (m -> Field m a) -> a -> Clause m
  fld $<= a = mkClause fld $ LtEq a

  ($=*) :: (Show a, Val a) => (m -> Field m a) -> [a] -> Clause m
  fld $=* a = mkClause fld $ In a

  ($/=*) :: (Show a, Val a) => (m -> Field m a) -> [a] -> Clause m
  fld $/=* a = mkClause fld $ NotIn a

  ($*=) :: (Show a, Val a) => (m -> Field m [a]) -> a -> Clause m
  fld $*= a = mkClause fld $ Contains a

  ($*=*) :: (Show a, Val a) => (m -> Field m [a]) -> [a] -> Clause m
  fld $*=* a = mkClause fld $ All a

  ($?) :: (Show a, Val a) => (m -> Field m a) -> Bool -> Clause m
  fld $? b = mkClause fld $ (Exists b :: Cond Bool)

  ($#) :: (Show a, Val a) => (m -> Field m a) -> Int -> Clause m
  fld $# n = mkClause fld $ (Size n :: Cond Int)

  ($:) :: (Show a, Val a) => (m -> Field m a) -> MongoType -> Clause m
  fld $: t = mkClause fld $ (Type t :: Cond MongoType)

  -- UPDATE OPERATORS


  -- SUBFIELDS

  (/.) :: (Schema e, Val a) => (m -> Field m e) -> (e -> Field e a) -> m -> Field m a
  outer /. inner =
    let
      EField n1 _ = outer schema
      Field n2 _ = inner schema
    in \m -> field $ T.concat [ n1, ".", n2 ]


  -- QUERY OPTIONS

  limit :: Word32 -> Query m r -> Query m r
  limit n q = q { lim = n }

  asc :: Val a => (m -> Field m a) -> Query m r -> Query m r
  asc fld q = let Field name _ = fld schema in q { srt = Asc name : srt q }

  desc :: Val a => (m -> Field m a) -> Query m r -> Query m r
  desc fld q = let Field name _ = fld schema in q { srt = Desc name : srt q }

  select :: Val r => (m -> Field m r) -> Query m m -> Query m r
  select fld q = q { sel = Select1 $ fld schema }


  -- QUERY EXECUTION

  fetch :: Queryable m => DB -> Query m r -> IO [r]
  fetch db q = case sel q of
    SelectAll -> fmap (fmap fromBson) docs
    Select1 f -> fmap (fmap $ fieldFromBson f) docs
    where
      docs = fetchBson db q
      fieldFromBson :: Val r => Field m r -> Bson.Document -> r
      fieldFromBson (Field n _) doc = maybe undefined id $ doc Bson.!? n

  fetchBson :: Queryable m => DB -> Query m r -> IO [Bson.Document]
  fetchBson db q = run $ act >>= Mongo.rest
    where
      run act = Mongo.access (dbPipe db) Mongo.master (dbName db) act
      act = Mongo.find (mkQuery q)

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

instance Queryable m => Show (Query m r) where
  show q = T.concat (intro ++ query ++ select ++ outro ++ sort ++ limit)  $. T.unpack
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
  toBson (Select1 (Field fieldName _)) = [ fieldName =: (1 :: Int) ]

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

