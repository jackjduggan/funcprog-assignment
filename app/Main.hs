module Main (main) where

import ModuleData (ValidatedModule)
import ValidateModule (processModules, generateMarkdown)
import Data.Csv (decodeByName, FromNamedRecord, Header)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

main :: IO ()
main = do
    putStrLn "Reading Module Descriptors..."
    processModules "data/Module_Descriptors.csv"
    putStrLn "Modules Processed..."