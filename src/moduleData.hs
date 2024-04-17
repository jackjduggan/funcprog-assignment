{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ModuleData where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
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

-- Define FromNamedRecord instance for Student
instance FromNamedRecord Module where
    parseNamedRecord r = do
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

readModules :: FilePath -> IO ()
readModules filePath = do
    csvData <- BL.readFile filePath
    case decodeByName csvData of
        Left err -> putStrLn $ "Error parsing CSV: " ++ err
        Right (_, v) -> V.forM_ v $ \m -> do
            putStrLn $ "Code: " ++ code m ++
                       ", Full Title: " ++ fullTitle m ++
                       ", Short Title: " ++ shortTitle m ++
                       ", Credits: " ++ show (credits m) ++
                       ", Level: " ++ level m ++
                       ", Aim: " ++ aim m ++
                       ", Department: " ++ department m ++
                       ", Indicative Content: " ++ indicativeContent m ++
                       ", Learning Outcomes: " ++ learningOutcomes m ++
                       ", Assessment Criteria: " ++ assessmentCriteria m