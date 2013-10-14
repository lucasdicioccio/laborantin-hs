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
import Data.Aeson (encode)
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List.Split (splitOn)

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

describeExecution :: Execution m -> String
describeExecution e = intercalate " " [ ePath e
                                      , sName (eScenario e)
                                      , "(" ++ show (eStatus e) ++ ")"
                                      , C.unpack $ encode (eParamSet e)
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

type ExecutionQuery = M.Map String [ParameterValue]

parseParamQuery :: String -> Maybe (String,[ParameterValue])
parseParamQuery str = let vals = splitOn ":" str in
    case vals of
    [k,"int",v]      -> Just (k, [NumberParam . toRational $ read v])
    [k,"double",v]   -> Just (k, [NumberParam . toRational $ (read v :: Double)])
    [k,"rational",v] -> Just (k, [NumberParam $ read v])
    [k,"str",v]      -> Just (k, [StringParam v])
    _                -> Nothing

paramsToQuery :: [String] -> ExecutionQuery
paramsToQuery xs = let pairs = catMaybes (map parseParamQuery xs) in
    M.fromListWith (++) pairs

filterDescriptions :: DescriptionQuery -> [ScenarioDescription m] -> [ScenarioDescription m]
filterDescriptions (ScenarioName []) xs = xs
filterDescriptions (ScenarioName ns) xs = filter ((flip elem ns) . sName) xs

matchQuery :: ExecutionQuery -> ParameterSet -> Bool
matchQuery m params = all id $ map snd $ M.toList $ M.intersectionWith elem params m

runLabor :: [ScenarioDescription EnvIO] -> Labor -> IO ()
runLabor xs (Describe scii)   =  forM_ xs' (putStrLn . describeScenario)
    where xs' = filterDescriptions (ScenarioName scii) xs
runLabor xs fi@(Rm {})         =  void $ runEnvIO (mapM (load defaultBackend) xs' >>= mapM (remove defaultBackend) . concat)
    where xs' = filterDescriptions (ScenarioName $ scenarii fi) xs
runLabor xs r@(Run { continue = False }) = do
    void . runEnvIO . forM_ xs' $ executeExhaustive defaultBackend
    where xs' = filterDescriptions (ScenarioName $ scenarii r) xs
runLabor xs r@(Run { continue = True }) = do
    void . runEnvIO . forM_ xs' $ executeMissing defaultBackend
    where xs' = filterDescriptions (ScenarioName $ scenarii r) xs
runLabor xs fi@(Find {})       =  do
    results <- (runEnvIO $ mapM (load defaultBackend) xs')
    let execs = filter (matchQuery query . eParamSet) $ concat $ fst results 
    mapM_ (putStrLn . describeExecution) execs
    where xs' = filterDescriptions (ScenarioName $ scenarii fi) xs
          query = paramsToQuery $ params fi
