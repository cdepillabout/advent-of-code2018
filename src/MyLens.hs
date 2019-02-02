{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module MyLens where

data Name = Name
  { nameFirst :: String
  , nameLast :: String
  } deriving Show

data User = User
  { glucoseLevel :: Int
  , userName :: Name
  } deriving Show

eric :: User
eric =
  User
    { glucoseLevel = 400
    , userName =
        Name
          { nameFirst = "Eric"
          , nameLast = "Jelliffe"
          }
    }

-- data Lens s a = Lens
--   { getter :: s -> a
--   , setter :: s -> a -> s
--   }

-- userGlucoseLens :: Lens User Int
-- userGlucoseLens = Lens
--   { getter = \user -> glucoseLevel user
--   , setter =
--       \user newGlucose ->
--         user { glucoseLevel = newGlucose }
--   }

-- ericsGlucoseLevel :: Int
-- ericsGlucoseLevel = (getter userGlucoseLens) eric

-- setEricsGlucose :: User
-- setEricsGlucose = (setter userGlucoseLens) eric 100

-- userNameLens :: Lens User Name
-- userNameLens = Lens
--   { getter = \user -> userName user
--   , setter =
--       \user newFirstName ->
--         user { userName = newFirstName }
--   }

-- nameFirstLens :: Lens Name String
-- nameFirstLens = Lens
--   { getter = \name -> nameFirst name
--   , setter =
--       \name newFirstName ->
--         name { nameFirst = newFirstName }
--   }

-- composeLens :: Lens s a -> Lens a b -> Lens s b
-- composeLens lensSA lensAB = Lens
--   { getter = \s ->
--       let a = (getter lensSA) s
--       in (getter lensAB) a
--   , setter = \s b ->
--       let oldA = (getter lensSA) s
--           newA = (setter lensAB) oldA b
--       in (setter lensSA) s newA
--   }

-- userFirstNameLens :: Lens User String
-- userFirstNameLens = composeLens userNameLens nameFirstLens

-- ericFirstName :: String
-- ericFirstName = (getter userFirstNameLens) eric

-- updatedEricFirstName :: User
-- updatedEricFirstName =
--   (setter userFirstNameLens) eric "EEERRRRRIIIICCCCCC"

type Lens s a =
  forall f. Functor f => (a -> f a) -> s -> f s

-- userGlucoseLens :: Lens User Int
userGlucoseLens
  :: forall f. Functor f
  => (Int -> f Int) -> User -> f User
userGlucoseLens = undefined

-- forallExample1 :: forall a. (a -> a) -> Int
-- -- forallExample1 a2a = a2a @Int 3
-- forallExample1 _ = 4

-- callForallExample1 :: Int
-- callForallExample1 = forallExample1 @Int succ

-- forallExample2 :: (forall a. a -> a) -> Int
-- forallExample2 a2a = a2a @Int 3

-- callForallExample2 :: Int
-- -- callForallExample2 = forallExample2 (id @Int)
-- callForallExample2 = forallExample2 id


-- myid :: forall a. a -> a
-- myid a = a

-- myid :: forall a. a -> a
-- myidString :: String -> String

-- typeApplicationsExample :: Int
-- typeApplicationsExample =
--   -- (myid :: Int -> Int) (3 :: Int)
--   -- myid 3
--   (myid @String) 3


-- -- forallExample1
-- --   :: forall a. Show a => (a -> String) -> String
-- forallExample1
--   :: forall a. (a -> String) -> (a -> String) -> String
-- forallExample1 = undefined
-- -- forallExample1 a2a = a2a @Int 3
-- -- forallExample1 a2String = a2String show

-- -- forallExample1'
-- --   :: forall a. Show a => (String -> a) -> String
-- forallExample1'
--   :: forall a. (a -> String) -> (String -> a) -> String
-- forallExample1' = undefined

-- callForallExample1 :: String
-- callForallExample1 = forallExample1 @String succ

-- forallExample2
--   :: (forall a. Show a => a -> String) -> String
-- forallExample2 a2a = undefined -- a2a @Int 3

-- callForallExample2 :: String
-- -- callForallExample2 = forallExample2 (id @Int)
-- callForallExample2 = undefined -- forallExample2 id


-- myFunction :: forall a. Show a => a -> String
-- myFunction a = show a ++ show a


