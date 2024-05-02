{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ModuleData (Module(..), ValidatedModule(..)) where
--                  ^ exports the readModules function
--                  ^ also export the datatypes
import Data.Csv(FromNamedRecord, parseNamedRecord, (.:))
import GHC.Generics (Generic)

-- Step 1: Define a model

data Module = Module
    { code :: String
    , fullTitle :: String
    , shortTitle :: String
    , credits :: Int
    , level :: String
    , aim :: String
    , department :: String
    , indicativeContent :: String
    , learningOutcomes :: String
    , assessmentCriteria :: String
    } deriving (Show, Generic)

-- Step 2: Parse CSV

-- this is needed for decodeByName in the validateModule.hs file
instance FromNamedRecord Module where
    parseNamedRecord r = do -- this syntax came from the CSVcheck code
        code'               <- r .: "Code"
        fullTitle'          <- r .: "Full_Title"
        shortTitle'         <- r .: "Short_Title"
        credits'            <- r .: "Credits"
        level'              <- r .: "Level"
        aim'                <- r .: "Aim"
        department'         <- r .: "Department"
        indicativeContent'  <- r .: "Indicative_Content"
        learningOutcomes'   <- r .: "Learning_Outcomes"
        assessmentCriteria' <- r .: "Assessment_Criteria"
        return $ Module code' fullTitle' shortTitle' credits' level' aim' department' indicativeContent' learningOutcomes' assessmentCriteria'

-- Step 3: Define a second datatype
-- uses Either type (notes 9:13)
data ValidatedModule = ValidatedModule
    -- Left (Either String) = Error, Right = Valid
    { validatedCode :: Either String String
    , validatedFullTitle :: Either String String
    , validatedShortTitle :: Either String String
    , validatedCredits :: Either String Int
    , validatedLevel :: Either String String
    , validatedAim :: Either String String
    , validatedDepartment :: Either String String
    , validatedIndicativeContent :: Either String String
    , validatedLearningOutcomes :: Either String String
    , validatedAssessmentCriteria :: Either String String
    }

