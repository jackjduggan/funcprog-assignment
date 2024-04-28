module Main (main) where

import ModuleData (readModules)
import ValidateModule (processModules)
import Data.Csv (decodeByName, FromNamedRecord, Header)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

main :: IO ()
main = do
    putStrLn "Reading Module Descriptors..."
    csvData <- BL.readFile "data/Module_Descriptors.csv"
    putStrLn "Modules Read"
