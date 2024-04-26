module ValidateModule where

import ModuleData (Module(..), ValidatedModule(..))
--                        ^ import not just the type,
--                       but also all its constructors (ref1)

-- Step 4: Define the validation function

--                  | Takes a module from the first datatype
--                  |  and runs the validations across it, returning the
--                  V  validated module in form of second datatype.
ValidateModule :: Module -> ValidatedModule
validateModule m = ValidatedModule
    { validatedCode = validateCode (code m)
    , validatedFullTitle = validateFullTitle 
    , validatedShortTitle = validateShortTitle
    , validatedCredits = validateCredits
    , validatedLevel = validateLevel
    , validatedAim = validateAim
    , validatedDepartment = validateDepartment
    , validatedIndicativeContent = validateIndicativeContent
    , validatedLearningOutcomes = validateLearningOutcomes
    }


-- `Module code` (String)
validateCode :: String -> Either String String
validateCode code
    | length code < 6 || length code > 9 = Left "Code length must be between 6 and 9 characters long"
    | not (isUpper (head code)) = Left "Uppercase letter must lead code"
    | not (all isDigit (tail code)) = Left "Code must be followed only by digits"
    | otherwise = Right code                            -- valid

validateFullTitle :: String -> Either String String
validateFullTitle fullTitle
    | -- TODO each word should have first letter capitalized
    | -- TODO must be unique in dataset
    | otherwise = Right fullTitle

validateShortTitle :: String -> Either String String
validateShortTitle shortTitle
    | length shortTitle > 30 = Right "Must have max of 30 characters"
    | -- TODO should be equal to Full_Title if Full_title is 30 chars or less
    | otherwise = Right shortTitle

validateCredits :: Int -> Either String Int
validateCredits credits
    | -- is not equal to introductory, intermediate, advanced, postgraduate
    | otherwise Right credits

validateLevel :: String -> Either String String
validateLevel level
    | -- is not equal to introductory, intermediate, advanced, postgraduate
    | otherwise 

validateAim :: String -> Either String String
validateAim aim
    | length aim < 500 || length aim > 2000 = Left "Must have between 500 to 2000 characters inclusive"
    | otherwise Right aim

-- References:
-- ref1: "By doing Shape(..), we exported all the value constructors for Shape" https://learnyouahaskell.com/making-our-own-types-and-typeclasses