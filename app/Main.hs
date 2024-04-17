module Main (main) where

import ModuleData

main :: IO ()
main = do
    putStrLn "Reading Module Descriptors..."
    readModules "data/Module_Descriptors.csv"
