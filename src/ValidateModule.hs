module ValidateModule (processModules, generateMarkdown) where

import ModuleData (Module(..), ValidatedModule(..))
--                        ^ import not just the type,
--                       but also all its constructors (ref1)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Char (isUpper, isDigit, isLower)
import Data.Either (isRight, isLeft)
import Data.List (groupBy, isInfixOf, partition) -- error #1 fixed
import qualified Data.Vector as V

-- Step 4: Define the validation function

--                  | Takes a module from the first datatype
--                  |  and runs the validations across it, returning the
--                  V  validated module in form of second datatype.
validateModule :: Module -> ValidatedModule
validateModule m = ValidatedModule
    { validatedCode = validateCode (code m)
    , validatedFullTitle = validateFullTitle (fullTitle m) -- added remaining calls
    , validatedShortTitle = validateShortTitle (shortTitle m) (fullTitle m) -- forgot to add second argument
    , validatedCredits = validateCredits (credits m)
    , validatedLevel = validateLevel (level m)
    , validatedAim = validateAim (aim m)
    , validatedDepartment = validateDepartment (department m)
    , validatedIndicativeContent = validateIndicativeContent (indicativeContent m)
    , validatedLearningOutcomes = validateLearningOutcomes (learningOutcomes m) (level m) -- forgot to add second argument
    , validatedAssessmentCriteria = validateAssessmentCriteria (assessmentCriteria m)
    }


-- `Module code` (String)
validateCode :: String -> Either String String
validateCode code
    | length code < 6 || length code > 9 = Left "Code length must be between 6 and 9 characters long"
    | not (isUpper (head code)) = Left "Uppercase letter must lead code"
    | not (all isDigit (tail code)) = Left "Code must be followed only by digits"
    | otherwise = Right code                            -- valid

-- `Full Title` (String)
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

-- `Short Title` (String)
validateShortTitle :: String -> String -> Either String String
validateShortTitle shortTitle fullTitle
    | length shortTitle > 30 = Left "Must have max of 30 characters"
    | length fullTitle <= 30 && shortTitle /= fullTitle = Left "Must be equal to full title if full title is 30 chars or less"
    | otherwise = Right shortTitle

-- `Module Credits` (Int)
validateCredits :: Int -> Either String Int
validateCredits credits
    | credits <= 0 || credits > 30 || mod credits 5 /= 0 = Left "Must be > 0, max 30, multiple of 5"
    | otherwise = Right credits

-- `Module Level` (String)
validateLevel :: String -> Either String String
validateLevel level -- L-- use of notElem instead of syntactically incorrect not(elem)
    | level `notElem` ["Introductory", "Intermediate", "Advanced", "Postgraduate"] = Left "Level must be one of Introductory, Intermediate, Advanced, Postgraduate"
    | otherwise = Right level

-- `Module Aim` (String)
validateAim :: String -> Either String String
validateAim aim
    | length aim < 500 || length aim > 2000 = Left "Must have between 500 to 2000 characters inclusive"
    | otherwise = Right aim

-- `Department` (String)
validateDepartment :: String -> Either String String
validateDepartment dep
    | dep `notElem` ["Science", "Computing and Mathematics", "Engineering Technology"] = Left "Department must be one of Science, C&M, ET" -- once again using notElem
    | otherwise = Right dep

-- `Indicative Content` (String)
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

-- `Learning Outcomes` (String)
validateLearningOutcomes :: String -> String -> Either String String -- fixed incorrect function signature
validateLearningOutcomes lo level
    | level == "Introductory" || level == "Intermediate" && length lo < 5 = Left "At least 5 Learning Outcomes needed"
    | level == "Advanced" && length lo < 7 = Left "At least 7 Learning Outcomes needed"
    | level == "Postgraduate" && length lo < 8 = Left "At least 8 Learning Outcomes needed"
    | otherwise = Right lo

-- `Assessment Criteria` (String)
validateAssessmentCriteria :: String -> Either String String
validateAssessmentCriteria ac
    | length (lines ac) < 4 = Left "Must have at least 4 categories" -- used lines funtion (ref5) - thought I could maybe use this for Indicative Content but this relies on terminating \n characters
    | any (not . isInfixOf "%") (lines ac) = Left "Each category must contain a '%' character" -- iSInFixOf checks if one string is contained within another.
    | otherwise = Right ac

-- Step 5: write a function/s that can create two collections of validated modules
-- this functions checks if all fields of a ValidatedModule are Right and returns a boolean
isFullyValidated :: ValidatedModule -> Bool
isFullyValidated vm =
    all isRight [
        validatedCode vm,
        validatedFullTitle vm,
        validatedShortTitle vm,
        -- validatedCredits vm, -- had a type mismatch where the type from validatedCredits int was not matching expected Either String String
        validatedLevel vm,
        validatedAim vm,
        validatedDepartment vm,
        validatedIndicativeContent vm,
        validatedLearningOutcomes vm,
        validatedAssessmentCriteria vm
    ] && isRight (validatedCredits vm) -- checking validatedCredits separately

-- Step 6: Generate Documents
-- Generate Markdown file taken from lab code (books)
generateMarkdown :: [ValidatedModule] -> String
generateMarkdown validatedModules = unlines $
    [ "# Modules"   -- a header in markdown
    , ""
    ] ++ map generateModule validatedModules

generateModule :: ValidatedModule -> String
generateModule vm =                         -- takes a validated module as an input
    unlines                                 -- unlines concatenates a list of strings, separating each with a \n
        [ "### Code:"                       -- markdown header
        , "- " ++ case validatedCode vm of  -- case expression pattern matches the result of the field's validation
            Right code -> code              --  if the result is Right (valid), the validated data is displayed
            Left err -> "Error: " ++ err    -- if Left (error), the appropriate error message is displayed
        , ""                                -- (ref6)
        , "### Full Title:"
        , "- " ++ case validatedFullTitle vm of
            Right title -> title
            Left err -> "Error: " ++ err
        , ""
        , "### Short Title:"
        , "- " ++ case validatedShortTitle vm of
            Right shortTitle -> shortTitle
            Left err -> "Error: " ++ err
        , ""
        , "### Credits:"
        , "- " ++ case validatedCredits vm of
            Right credits -> show credits
            Left err -> "Error: " ++ err
        , ""
        , "### Level:"
        , "- " ++ case validatedLevel vm of
            Right level -> level
            Left err -> "Error: " ++ err
        , ""
        , "### Aim:"
        , "- " ++ case validatedAim vm of
            Right aim -> aim
            Left err -> "Error: " ++ err
        , ""
        , "### Department:"
        , "- " ++ case validatedDepartment vm of
            Right department -> department
            Left err -> "Error: " ++ err
        , ""
        , "### Indicative Content:"
        , "- " ++ case validatedIndicativeContent vm of
            Right content -> content
            Left err -> "Error: " ++ err
        , ""
        , "### Learning Outcomes:"
        , "- " ++ case validatedLearningOutcomes vm of
            Right outcomes -> outcomes
            Left err -> "Error: " ++ err
        , ""
        , "### Assessment Criteria:"
        , "- " ++ case validatedAssessmentCriteria vm of
            Right criteria -> criteria
            Left err -> "Error: " ++ err
        , ""
        , "---"
        ]

-- Process Modules function is called in Main.hs
processModules :: FilePath -> IO ()
processModules filePath = do
    csvData <- BL.readFile filePath
    case decodeByName csvData of
        Left err -> putStrLn $ "Error parsing CSV: " ++ err
        Right (_, modules) -> do
            let validatedModules = map validateModule (V.toList modules)
            writeFile "modules2.md" (generateMarkdown validatedModules) -- all the modules, with error messages where data is invalid.

-- References:
-- ref1: "By doing Shape(..), we exported all the value constructors for Shape" https://learnyouahaskell.com/making-our-own-types-and-typeclasses
-- ref2: https://hackage.haskell.org/package/Cassava-0.5.1.0/docs/Data-Csv.html
-- ref4: "This code with groupBy from Data.List..." https://codereview.stackexchange.com/questions/6992/approach-to-string-split-by-character-in-haskell
-- ref5: https://hackage.haskell.org/package/base-4.19.1.0/docs/Prelude.html#v:lines
-- ref6: https://chat.openai.com/share/85470ab5-4e35-4983-8130-f2f81497f5e5