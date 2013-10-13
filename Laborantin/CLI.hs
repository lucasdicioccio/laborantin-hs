
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module Laborantin.CLI (defaultMain) where

import Control.Monad
import Control.Monad.IO.Class
import Laborantin
import Laborantin.Types
import Laborantin.Implementation
import System.Environment
import System.Console.CmdLib hiding (run)
import Data.Map (toList)
import Data.List (intercalate)

defaultMain xs = getArgs >>= dispatchR [] >>= runLabor xs

unlines' :: [String] -> String
unlines' = intercalate "\n"

describeScenario :: ScenarioDescription m -> String
describeScenario sc = unlines [
    "# Scenario: " ++ sName sc
  , "    " ++ sDesc sc
  , "    " ++ (show . length . paramSets $ sParams sc) ++ " parameter combinations by default"
  , "## Parameters:"
  , unlines' $ paramLines
  ]
  where paramLines = map (uncurry paramLine) pairs
        pairs = toList $ sParams sc
        paramLine n p = unlines' [
                          "### " ++ n
                        , describeParameter p
                        ]

describeParameter :: ParameterDescription -> String
describeParameter p = unlines' [
    "(" ++ pName p ++ ")"
  , "    " ++ pDesc p
  , "    " ++ (show . length $ concatMap expandValue $ pValues p) ++ " values:"
  , unlines $ map (("    - " ++) . show) (pValues p)
  ]

data Labor = Run        { scenarii   :: [String] , params :: [String] , continue :: Bool} 
           | Describe   { scenarii   :: [String] } 
           | Find       { scenarii   :: [String] , params :: [String] } 
           | Analyze    { scenarii   :: [String] , params :: [String] } 
           | Rm         { scenarii   :: [String] , params :: [String] , force :: Bool , failed :: Bool , successful :: Bool} 
    deriving (Typeable, Data, Show, Eq)

instance Attributes Labor where
    attributes _ = group "Options" [
                        scenarii %> [ Short "s"
                                    , Long ["scenario"]
                                    , Help "Restrict to the scenarios in parameter."
                                    , ArgHelp "SCENARIOS"
                                    ] 
                    ,   params   %> [ Short "p"
                                    , Long ["param"]
                                    , Help "Restrict a parameter, format name=type:val."
                                    , ArgHelp "PARAMS"
                                    ]
                    ,   continue %> [ Short "c"
                                    , Long ["continue"]
                                    , Default False
                                    , Invertible True
                                    , Help "Continue execution (skip known)"
                                    ]
                    ,   force    %> [ Short "f"
                                    , Long ["force"]
                                    , Help "Force flag"
                                    ]
                    ,   failed   %> [ Long ["failed"]
                                    , Help "Failed only"
                                    ]
                    ,   successful %> [ Long ["successful"]
                                    , Help "Successful only"
                                    ]
                    ]

instance RecordCommand Labor where
    mode_summary _ = "Laborantin command-line interface"

runLabor :: [ScenarioDescription EnvIO] -> Labor -> IO ()
runLabor xs (Describe {})   =  forM_ xs (putStrLn . describeScenario)
runLabor xs (Find {})       =  (runEnvIO $ mapM (load defaultBackend) xs) >>= print
runLabor xs (Run { continue = False }) = do
    void . runEnvIO . forM_ xs $ executeExhaustive defaultBackend
runLabor xs (Run { continue = True }) = do
    void . runEnvIO . forM_ xs $ executeMissing defaultBackend
