{-# LANGUAGE ExistentialQuantification #-}

module Models.Query where

data Query m = Query { clauses :: [Clause m], lim :: Maybe Int, sort :: [Sort m] } deriving Show
data Clause m = forall a . Show a => Clause String (Cond a)
data Cond a = Eq a | Neq a | Gt a | Lt a | Between a a deriving Show

data Sort m = Asc String | Desc String deriving Show

data Field m a = Field { name :: String, value :: a }

instance Show (Clause m) where
  show (Clause n c) = show n ++ ": " ++ show c

class Queryable m where
  collection :: [Clause m] -> String
  schema :: m

  find :: [Clause m] -> Query m
  find cls = Query cls Nothing []

  (~>) :: Show a => (m -> Field m a) -> Cond a -> Clause m
  fld ~> c = case fld schema of (Field n _) -> Clause n c

  (~.) :: Show a => m -> (m -> Field m a) -> a
  m ~. fld = case fld m of (Field _ a) -> a

  field :: String -> Field m a
  field name = Field name undefined

  val :: a -> Field m a
  val a = Field undefined a

  limit :: Int -> Query m -> Query m
  limit n q = q { lim = Just n }

  asc :: (m -> Field m a) -> Query m -> Query m
  asc fld q = case fld schema of (Field n _) -> q { sort = Asc n : sort q }

  desc :: (m -> Field m a) -> Sort m
  desc fld = case fld schema of (Field n _) -> Desc n

  -- deserialize :: BSON -> m

{-
class FromBSON m where
  fromBSON m :: BSON -> m -> m

instance FromBSON (Field m a) where
  fromBSON b (Field n _) = Field "" ((b ?! n) :: a)
  -}