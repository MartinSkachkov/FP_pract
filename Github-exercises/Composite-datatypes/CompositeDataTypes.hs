import Data.List qualified as List
import Data.Time (Day)
import Data.Time qualified as Time
import Prelude

data UserProfile = UserProfile
  { username :: String,
    age :: Int,
    active :: Bool,
    interests :: [String],
    relationshipStatus :: RelationshipStatus
  }
  deriving (Eq, Show)

data RelationshipStatus
  = MarriedTo MarriageInfo
  | EngagedTo RelationshipStatus
  | ItsComplicated
  | Single
  deriving (Eq, Show)

data MarriageInfo = MarriageInfo {spouse :: Spouse, date :: Day}
  deriving (Eq, Show)

data Spouse
  = SpouseProfile UserProfile
  | SpouseName String
  deriving (Eq, Show)

marto :: UserProfile
marto =
  UserProfile
    { username = "Marto",
      age = 20,
      active = True,
      interests = ["Prog", "Sports"],
      relationshipStatus =
        MarriedTo
          MarriageInfo
            { spouse = SpouseName "Jena",
              date = Time.fromGregorian 2016 06 04
            }
    }

profileToString :: UserProfile -> String
profileToString UserProfile {age, active, interests, relationshipStatus, username} =
  let ageString = show age
      activeString = if active then "active" else "not active"
      interestsString = intercalate ", " interests
      relationshipStatusString = case relationshipStatus of
        MarriedTo MarriageInfo {spouse, date} ->
          let dateString = Time.showGregorian date
           in -- `unwords` takes a `[String]` and joins them into a string with spaces inbetween
              unwords ["Married to:", spouse, "on", dateString]
        EngagedTo UserProfile {username = spouseUsername} ->
          unwords ["Engaged to:", spouseUsername]
        ItsComplicated -> "It's complicated"
        Single -> "Single"
   in mconcat
        [ username,
          " (",
          ageString,
          "y, ",
          activeString,
          ", ",
          relationshipStatusString,
          ") is interested in: ",
          interestsString
        ]

-- | Inserts a given string between every entry in the list of strings
intercalate :: String -> [String] -> String
intercalate between strings =
  mconcat $ List.intersperse between strings
