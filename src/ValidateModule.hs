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

-- TODO create the rest of the validation functions
    | 




-- References:
-- ref1: "By doing Shape(..), we exported all the value constructors for Shape" https://learnyouahaskell.com/making-our-own-types-and-typeclasses