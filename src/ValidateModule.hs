module ValidateModule (processModules) where

import ModuleData (Module(..), ValidatedModule(..))
--                        ^ import not just the type,
--                       but also all its constructors (ref1)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Char (isUpper, isDigit, isLower)
import qualified Data.Vector as V

-- Step 4: Define the validation function

--                  | Takes a module from the first datatype
--                  |  and runs the validations across it, returning the
--                  V  validated module in form of second datatype.
validateModule :: Module -> ValidatedModule
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
    | not (all isTitleCase wordsList) = Left "Must be in title case"
    -- | TODO: handle uniqueness
    | otherwise = Right fullTitle
  where
    wordsList = words fullTitle
    isTitleCase :: String -> Bool
    isTitleCase [] = False -- Empty string automatically false
    isTitleCase (x:xs) = isUpper x && all isLower xs -- First char upper, rest lower

validateShortTitle :: String -> String -> Either String String
validateShortTitle shortTitle fullTitle
    | length shortTitle > 30 = Left "Must have max of 30 characters"
    | length fullTitle <= 30 && shortTitle /= fullTitle = Left "Must be equal to full title if full title is 30 chars or less"
    | otherwise = Right shortTitle

validateCredits :: Int -> Either String Int
validateCredits credits
    | credits <= 0 || credits > 30 || mod credits 5 /= 0 = Left "Must be > 0, max 30, multiple of 5"
    | otherwise = Right credits

validateLevel :: String -> Either String String
validateLevel level
    | level not (`elem` ["Introductory", "Intermediate", "Advanced", "Postgraduate"]) = Left "Level must be one of Introductory, Intermediate, Advanced, Postgraduate"
    | otherwise = Right level

validateAim :: String -> Either String String
validateAim aim
    | length aim < 500 || length aim > 2000 = Left "Must have between 500 to 2000 characters inclusive"
    | otherwise = Right aim

validateDepartment :: String -> Either String String
validateDepartment dep
    | dep not (`elem` ["Science", "Computing and Mathematics", "Engineering Technology"]) = Left "Department must be one of Science, C&M, ET"
    | otherwise = Right dep

validateIndicativeContent :: String -> Either String String
validateIndicativeContent ic
    | not (all (any isUpper) (splitIntoSentences ic)) = Left "Not all sentences contain a capital letter"
    | otherwise = Right ic
  where
    splitIntoSentences :: String -> [String]
    splitIntoSentences text = filter (not . null) $ map (filter (/= '.')) $ groupBy customGroup text -- using the GroupBy function (ref4)
    customGroup :: Char -> Char -> Bool
    customGroup _ '.' = False  -- Do not group if the current character is a full stop
    customGroup _ _ = True

validateLearningOutcomes :: String -> Either String String
validateLearningOutcomes lo
    -- | TODO: number of outcomes dependant on module level
    | otherwise = Right lo

validateAssessmentCriteria :: String -> Either String String
validateAssessmentCriteria ac
    -- | TODO: if less than 4 categories (lines)
    -- | TODO: if less than 1 occurence of % in each lines
    | otherwise = Right ac





-- References:
-- ref1: "By doing Shape(..), we exported all the value constructors for Shape" https://learnyouahaskell.com/making-our-own-types-and-typeclasses
-- ref2: https://hackage.haskell.org/package/Cassava-0.5.1.0/docs/Data-Csv.html
-- ref3: https://hackage.haskell.org/package/bytestring-0.12.1.0/docs/Data-ByteString.html#g:3
-- ref4: "This code with groupBy from Data.List..." https://codereview.stackexchange.com/questions/6992/approach-to-string-split-by-character-in-haskell