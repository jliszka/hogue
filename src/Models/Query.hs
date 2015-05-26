{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Models.Query where

import Data.Word (Word32)
import qualified Data.Text as T
import qualified Database.MongoDB as Mongo
import Database.MongoDB ((=:))
import qualified Data.Bson as Bson

-- !#$%&*+./<=>?@\^|-~:

($.) :: a -> (a -> b) -> b
($.) a f = f a
infixl 9 $.

data DB = DB {
  dbPipe :: Mongo.Pipe,
  dbName :: T.Text }

getDB :: T.Text -> IO DB
getDB dbName = do
  pipe <- Mongo.connect $ Mongo.host "127.0.0.1"
  return $ DB pipe dbName

data Query m r = Query {
  clauses :: [Clause m],
  lim :: Word32,
  srt :: [Sort],
  sel :: [Select]
} deriving Show

data Clause m = forall a . (Show a, Mongo.Val a) => Clause T.Text (Cond a)

data (Show a, Mongo.Val a) => Cond a =
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
  | Mod Int Int    -- $%
  | Exists Bool    -- $?
  deriving Show

data (Show a, Mongo.Val a) => Mod a =
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


data MongoType = MDouble | MString | MObject deriving Show

data Sort =
    Asc T.Text
  | Desc T.Text deriving Show

data Select = Select T.Text deriving Show

data (Show a, Mongo.Val a) => Field m a = Field {
  name :: T.Text,
  value :: a
} deriving (Show, Eq)

instance Show (Clause m) where
  show (Clause n c) = show n ++ ": " ++ show c

class Embeddable m where
  schema2 :: m
  field2 :: Mongo.Val a => T.Text -> Field m a
  field2 name = Field name undefined
  (~..) :: (Show a, Mongo.Val a) => m -> (m -> Field m a) -> a
  m ~.. fld = let Field _ a = fld m in a


{-
instance (Eq e, Show e, Embeddable e) => Mongo.Val e where
  val a = Bson.Doc [ ]
  cast' doc = undefined
-}

class Queryable m where
  collection :: Query m r -> T.Text
  schema :: m

  find :: [Clause m] -> Query m m
  find cls = Query cls 0 [] []

  mkClause :: (Show a, Mongo.Val a, Show b, Mongo.Val b) => (m -> Field m a) -> Cond b -> Clause m
  mkClause fld cond = let Field name _ = fld schema in Clause name cond

  (~>) :: (Show a, Mongo.Val a) => (m -> Field m a) -> Cond a -> Clause m
  fld ~> cond = mkClause fld cond

  ($=) :: (Show a, Mongo.Val a) => (m -> Field m a) -> a -> Clause m
  fld $= a = mkClause fld $ Eq a

  ($/=) :: (Show a, Mongo.Val a) => (m -> Field m a) -> a -> Clause m
  fld $/= a = mkClause fld $ Neq a

  ($=*) :: (Show a, Mongo.Val a) => (m -> Field m a) -> [a] -> Clause m
  fld $=* a = mkClause fld $ In a

  ($*=) :: (Show a, Mongo.Val a) => (m -> Field m [a]) -> a -> Clause m
  fld $*= a = mkClause fld $ Contains a

  ($>) :: (Show a, Mongo.Val a) => (m -> Field m a) -> a -> Clause m
  fld $> a = mkClause fld $ Gt a

  ($<) :: (Show a, Mongo.Val a) => (m -> Field m a) -> a -> Clause m
  fld $< a = mkClause fld $ Lt a

  ($?) :: (Show a, Mongo.Val a) => (m -> Field m a) -> Bool -> Clause m
  fld $? a = mkClause fld $ (Exists a :: Cond Bool)

  (~.) :: (Show a, Mongo.Val a) => m -> (m -> Field m a) -> a
  m ~. fld = let Field _ a = fld m in a

  (/.) :: (Embeddable e, Mongo.Val e, Show a, Mongo.Val a) => (m -> Field m e) -> (e -> Field e a) -> m -> Field m a
  outer /. inner =
    let
      Field n1 _ = outer schema
      Field n2 _ = inner schema2
    in \m -> field $ T.concat [ n1, ".", n2 ]

  field :: Mongo.Val a => T.Text -> Field m a
  field name = Field name undefined

  val :: Mongo.Val a => a -> Field m a
  val a = Field undefined a

  limit :: Word32 -> Query m r -> Query m r
  limit n q = q { lim = n }

  asc :: Mongo.Val a => (m -> Field m a) -> Query m r -> Query m r
  asc fld q = let Field name _ = fld schema in q { srt = Asc name : srt q }

  desc :: Mongo.Val a => (m -> Field m a) -> Query m r -> Query m r
  desc fld q = let Field name _ = fld schema in q { srt = Desc name : srt q }

  select :: Mongo.Val r => (m -> Field m r) -> Query m m -> Query m r
  select fld q = let Field name _ = fld schema in q { sel = [Select name] }

  fetch :: DB -> Query m r -> IO [Mongo.Document]
  fetch db q = run $ act >>= Mongo.rest
    where
      run act = Mongo.access (dbPipe db) Mongo.master (dbName db) act
      act = Mongo.find (Mongo.select cls coll) {
        Mongo.limit = limit,
        Mongo.sort = sort,
        Mongo.project = project
      }
      cls = map mkClause $ clauses q
      coll = collection q
      limit = lim q
      sort = reverse $ map mkSort $ srt q
      project = map mkProject $ sel q

      mkClause :: Clause m -> Bson.Field
      mkClause (Clause fieldName (Eq a)) = fieldName =: a
      mkClause (Clause fieldName (Contains a)) = fieldName =: a
      mkClause (Clause fieldName cond) = fieldName =: (mkCond cond)

      mkCond :: Bson.Val a => Cond a -> Bson.Field
      mkCond (Neq a) = "$neq" =: a
      mkCond (In a) = "$in" =: a
      mkCond (Gt a) = "$gt" =: a
      mkCond (Lt a) = "$lt" =: a
      mkCond (Exists a) = "$exists" =: a

      mkSort :: Sort -> Bson.Field
      mkSort (Asc fieldName) = fieldName =: (1 :: Int)
      mkSort (Desc fieldName) = fieldName =: (-1 :: Int)

      mkProject :: Select -> Bson.Field
      mkProject (Select fieldName) = fieldName =: (1 :: Int)


  -- deserialize :: BSON -> m

{-
class FromBSON m where
  fromBSON m :: BSON -> m -> m

instance FromBSON (Field m a) where
  fromBSON b (Field n _) = Field "" ((b ?! n) :: a)
  -}