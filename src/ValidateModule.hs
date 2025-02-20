module ValidateModule (processModules, generateMarkdown) where

import ModuleData (Module(..), ValidatedModule(..))
--                        ^ import not just the type,
--                       but also all its constructors (ref1)
import qualified Data.ByteString.Lazy as BL -- module for reading CSV files
import Data.Csv
import Data.Char (isUpper, isDigit, isLower)
import Data.Either (isRight)
import Data.List (groupBy, isInfixOf)
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
validateCode moduleCode
    | length moduleCode < 6 || length moduleCode > 9 = Left "Code length must be between 6 and 9 characters long"
    | not (isUpper (head moduleCode)) = Left "Uppercase letter must lead code"
    | not (all isDigit (tail moduleCode)) = Left "Code must be followed only by digits"
    | otherwise = Right moduleCode                            -- valid

-- `Full Title` (String)
validateFullTitle :: String -> Either String String
validateFullTitle title
    | not (all isTitleCase wordsList) = Left "Must be in title case"
    -- | TODO: handle uniqueness
    | otherwise = Right title
  where
    wordsList = words title
    isTitleCase :: String -> Bool
    isTitleCase [] = False -- Empty string automatically false
    isTitleCase (x:xs) = isUpper x && all isLower xs -- First char upper, rest lower

-- `Short Title` (String)
validateShortTitle :: String -> String -> Either String String
validateShortTitle sTitle fTitle
    | length sTitle > 30 = Left "Must have max of 30 characters"
    | length fTitle <= 30 && sTitle /= fTitle = Left "Must be equal to full title if full title is 30 chars or less"
    | otherwise = Right sTitle

-- `Module Credits` (Int)
validateCredits :: Int -> Either String Int
validateCredits moduleCredits
    | moduleCredits <= 0 || moduleCredits > 30 || mod moduleCredits 5 /= 0 = Left "Must be > 0, max 30, multiple of 5"
    | otherwise = Right moduleCredits

-- `Module Level` (String)
validateLevel :: String -> Either String String
validateLevel moduleLevel -- L-- use of notElem instead of syntactically incorrect not(elem)
    | moduleLevel `notElem` ["Introductory", "Intermediate", "Advanced", "Postgraduate"] = Left "Level must be one of Introductory, Intermediate, Advanced, Postgraduate"
    | otherwise = Right moduleLevel

-- `Module Aim` (String)
validateAim :: String -> Either String String
validateAim moduleAim
    | length moduleAim < 500 || length moduleAim > 2000 = Left "Must have between 500 to 2000 characters inclusive"
    | otherwise = Right moduleAim

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
validateLearningOutcomes lo moduleLevel
    | moduleLevel == "Introductory" || moduleLevel == "Intermediate" && length lo < 5 = Left "At least 5 Learning Outcomes needed"
    | moduleLevel == "Advanced" && length lo < 7 = Left "At least 7 Learning Outcomes needed"
    | moduleLevel == "Postgraduate" && length lo < 8 = Left "At least 8 Learning Outcomes needed"
    | otherwise = Right lo

-- `Assessment Criteria` (String)
validateAssessmentCriteria :: String -> Either String String
validateAssessmentCriteria ac
    | length (lines ac) < 4 = Left "Must have at least 4 categories" -- used lines funtion (ref5) - thought I could maybe use this for Indicative Content but this relies on terminating \n characters
    | any (not . isInfixOf "%") (lines ac) = Left "Each category must contain a '%' character" -- iSInFixOf checks if one string is contained within another.
    | otherwise = Right ac

-- Step 5: write a function/s that can create two collections of validated modules

-- Process Modules function is called in Main.hs
-- This function reads the csv file using BL.readfile (lazy bytestring)
-- Two collections of validated modules are created by filtering modules when generating the markdown files, but they are
--  not separated into their own structures, being created 'on-the-fly' instead.
processModules :: FilePath -> IO ()
processModules filePath = do
    csvData <- BL.readFile filePath
    case decodeByName csvData of -- decodeByName used to convert the CSV data into a list of Module objects
        Left err -> putStrLn $ "Error parsing CSV: " ++ err
        Right (_, modules) -> do
            let validatedModules = map validateModule (V.toList modules) -- each module is validated, producing a list of ValidatedModule objects.
            writeFile "validated_with_errors.md" (generateMarkdown validatedModules True) -- all modules with error messages for invalid fields
            writeFile "validated_no_errors.md" (generateMarkdown validatedModules False) -- only modules that pass all validation checks.

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
-- This function generates the markdown header and the markdown string based on
--  the input modules and the includeErrors parameter.
generateMarkdown :: [ValidatedModule] -> Bool -> String
generateMarkdown validatedModules includeErrors  = unlines $
    [ "# Modules"   -- a header in markdown
    , ""
    ] ++ map generateModule (if includeErrors then validatedModules else filter isFullyValidated validatedModules)

-- This function converts a single `ValidatedModule` into its markdown representation,
--  displaying either the valid value or appropriate error based on validation status.
generateModule :: ValidatedModule -> String
generateModule vm =                         -- takes a validated module as an input
    unlines                                 -- unlines concatenates a list of strings, separating each with a \n
        [ "### Code:"                       -- markdown header
        , "- " ++ case validatedCode vm of  -- case expression pattern matches the result of the field's validation
            Right validCode -> validCode               --  if the result is Right (valid), the validated data is displayed
            Left err -> "Error: " ++ err    -- if Left (error), the appropriate error message is displayed
        , ""                                -- (ref6)
        , "### Full Title:"
        , "- " ++ case validatedFullTitle vm of
            Right validFullTitle -> validFullTitle
            Left err -> "Error: " ++ err
        , ""
        , "### Short Title:"
        , "- " ++ case validatedShortTitle vm of
            Right validShortTitle -> validShortTitle
            Left err -> "Error: " ++ err
        , ""
        , "### Credits:"
        , "- " ++ case validatedCredits vm of
            Right validCredits -> show validCredits
            Left err -> "Error: " ++ err
        , ""
        , "### Level:"
        , "- " ++ case validatedLevel vm of
            Right validLevel -> validLevel
            Left err -> "Error: " ++ err
        , ""
        , "### Aim:"
        , "- " ++ case validatedAim vm of
            Right validAim -> validAim
            Left err -> "Error: " ++ err
        , ""
        , "### Department:"
        , "- " ++ case validatedDepartment vm of
            Right validDepartment -> validDepartment
            Left err -> "Error: " ++ err
        , ""
        , "### Indicative Content:"
        , "- " ++ case validatedIndicativeContent vm of
            Right validContent -> validContent
            Left err -> "Error: " ++ err
        , ""
        , "### Learning Outcomes:"
        , "- " ++ case validatedLearningOutcomes vm of
            Right validOutcomes -> validOutcomes
            Left err -> "Error: " ++ err
        , ""
        , "### Assessment Criteria:"
        , "- " ++ case validatedAssessmentCriteria vm of
            Right validCriteria -> validCriteria
            Left err -> "Error: " ++ err
        , ""
        , "---"
        ]

-- References:
-- ref1: "By doing Shape(..), we exported all the value constructors for Shape" https://learnyouahaskell.com/making-our-own-types-and-typeclasses
-- ref2: https://hackage.haskell.org/package/Cassava-0.5.1.0/docs/Data-Csv.html
-- ref3: https://hackage.haskell.org/package/base-4.19.1.0/docs/Data-Either.html
-- ref4: "This code with groupBy from Data.List..." https://codereview.stackexchange.com/questions/6992/approach-to-string-split-by-character-in-haskell
-- ref5: https://hackage.haskell.org/package/base-4.19.1.0/docs/Prelude.html#v:lines
-- ref6: https://chat.openai.com/share/85470ab5-4e35-4983-8130-f2f81497f5e5