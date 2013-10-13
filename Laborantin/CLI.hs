{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module Laborantin.CLI (defaultMain) where

import Control.Monad
import Control.Monad.IO.Class
import Laborantin
import Laborantin.Types
import Laborantin.Implementation
import System.Environment
import System.Console.CmdLib hiding (run)
import qualified Data.Map as M
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
        pairs = M.toList $ sParams sc
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

data DescriptionQuery = ScenarioName [String]
    deriving (Show)

data ExecutionQuery = Parameter String ParameterValue
    deriving (Show)

parseParamQuery :: String -> Maybe ParameterValue
parseParamQuery str = undefined

filterDescriptions :: DescriptionQuery -> [ScenarioDescription m] -> [ScenarioDescription m]
filterDescriptions (ScenarioName []) xs = xs
filterDescriptions (ScenarioName ns) xs = filter ((flip elem ns) . sName) xs

filterExecutions :: ExecutionQuery -> [Execution m] -> [Execution m]
filterExecutions (Parameter k v) xs = filter ((== Just v) . M.lookup k . eParamSet) xs

runLabor :: [ScenarioDescription EnvIO] -> Labor -> IO ()
runLabor xs (Describe scii)   =  forM_ xs' (putStrLn . describeScenario)
    where xs' = filterDescriptions (ScenarioName scii) xs
runLabor xs fi@(Find {})       =  (runEnvIO $ mapM (load defaultBackend) xs') >>= print
    where xs' = filterDescriptions (ScenarioName $ scenarii fi) xs
runLabor xs fi@(Rm {})         =  void $ runEnvIO (mapM (load defaultBackend) xs' >>= mapM (remove defaultBackend) . concat)
    where xs' = filterDescriptions (ScenarioName $ scenarii fi) xs
runLabor xs r@(Run { continue = False }) = do
    void . runEnvIO . forM_ xs' $ executeExhaustive defaultBackend
    where xs' = filterDescriptions (ScenarioName $ scenarii r) xs
runLabor xs r@(Run { continue = True }) = do
    void . runEnvIO . forM_ xs' $ executeMissing defaultBackend
    where xs' = filterDescriptions (ScenarioName $ scenarii r) xs
