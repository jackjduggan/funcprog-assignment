module ModuleData where
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics (Generic)

-- Step 1: Define a model

data Module = Module
    { code :: Text
    , fullTitle :: Text
    , shortTitle :: Text
    , credits :: Int
    , level :: Text
    , aim :: Text
    , department :: Text
    , indicativeContent :: Text
    , learningOutcomes :: Text
    , assessmentCriteria :: Text
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