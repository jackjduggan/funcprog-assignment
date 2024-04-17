module ModuleData where
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics (Generic)

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