{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Models.Query where

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
  sel :: [Select]
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

data Select = Select T.Text deriving Show

data Show a => Field m a = Field {
  name :: T.Text,
  value :: a
} deriving (Show, Eq)

instance Show (Clause m) where
  show (Clause n c) = show n ++ ": " ++ show c



class Schema m where
  schema :: m

  field :: Show a => T.Text -> Field m a
  field name = Field name undefined

  (~.) :: Show a => m -> (m -> Field m a) -> a
  m ~. fld = let Field _ a = fld m in a

  val :: Show a => a -> Field m a
  val a = Field undefined a


class Schema m => Queryable m where
  collection :: Query m r -> T.Text

  find :: [Clause m] -> Query m m
  find cls = Query cls 0 [] []

  -- QUERY OPERATORS

  mkClause :: (Show a, Val a, Show b, Val b) => (m -> Field m a) -> Cond b -> Clause m
  mkClause fld cond = let Field name _ = fld schema in Clause name cond

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

  (/.) :: (Schema e, Show a, Show e) => (m -> Field m e) -> (e -> Field e a) -> m -> Field m a
  outer /. inner =
    let
      Field n1 _ = outer schema
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
  select fld q = let Field name _ = fld schema in q { sel = [Select name] }


  -- QUERY EXECUTION

  fetch :: Queryable m => DB -> Query m r -> IO [Mongo.Document]
  fetch db q = run $ act >>= Mongo.rest
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
          project = sel q $. reverse $. toBson
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
      select = ifNotEmpty (sel q) $ \sels -> [ ", ", sels $. toBson  $. Bson.Doc $. bsonToText ]
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

instance ToBson [Select] where
  toBson fields = map toBsonField fields

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

instance ToBsonField Select where
  toBsonField (Select fieldName) = fieldName =: (1 :: Int)

