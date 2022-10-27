module RecordTypes where

-- store multiple values together in a named structure

--      type       constructor
data UserProfile = UserProfile
  --  fields
  { username :: String,
    age :: Int,
    active :: Bool,
    interests :: [String]
  }
  deriving (Eq, Show)

-- The constructor name can be different than the type name, but this is comparatively rare.

rickard :: UserProfile
rickard =
  UserProfile
    { username = "rickard",
      age = 34,
      active = True,
      interests = ["Programming", "Problem Solving", "Teaching"]
    }

-- :type age
-- age :: UserProfile -> Int
-- age rickard -> 34

data Skill = MkSkill
  { power :: Int,
    health :: Int
  }

isOp :: Skill -> Int -> Bool
isOp MkSkill {power, health} x = x > power

-- ghci> isOp MkSkill {power = 5, health =  4} 3
-- False
