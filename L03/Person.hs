module L03.Person where

-- Suppose we have a data structure to represent a person. The person data structure has these attributes:
--     * Age: positive integer
--     * First Name: non-empty string that starts with a capital letter
--     * Surname: string that starts with a capital letter and is followed by 5 or more lower-case letters
--     * Gender: character that must be 'm' or 'f'
--     * Phone: string of digits, dots or hyphens but must start with a digit and end with a hash (#)
data Person = Person {
  age :: Int,
  firstName :: String,
  surname :: String,
  gender :: Char,
  phone :: String
} deriving (Eq, Show)

