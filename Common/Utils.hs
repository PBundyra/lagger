module Common.Utils where

import Lagger.Abs


-- Methods related to predefined functions --
predefinedFuncs :: [String]
predefinedFuncs = ["println", "error"]

isPredefined :: Ident -> Bool
isPredefined (Ident name) = name `elem` predefinedFuncs

-- Misc methods --
showPos :: BNFC'Position -> String
showPos (Just (line, column)) = concat ["line ", show line, ", column ", show column]
showPos _ = "unknown"
