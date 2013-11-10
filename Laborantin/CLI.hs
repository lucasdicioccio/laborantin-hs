{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module Laborantin.CLI (defaultMain) where

import Control.Monad
import Control.Applicative
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
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

defaultMain xs = getArgs >>= dispatchR [] >>= runLabor xs

unlines' :: [Text] -> Text
unlines' = T.intercalate "\n"

describeScenario :: ScenarioDescription m -> Text
describeScenario sc = T.unlines [
    T.append "# Scenario: " (sName sc)
  , T.append "    " (sDesc sc)
  , T.concat ["    ", (T.pack . show . length . paramSets $ sParams sc), " parameter combinations by default"]
  , "## Parameters:"
  , unlines' $ paramLines
  ]
  where paramLines = map (uncurry paramLine) pairs
        pairs = M.toList $ sParams sc
        paramLine n p = unlines' [
                          T.append "### " n
                        , describeParameter p
                        ]

describeParameter :: ParameterDescription -> Text
describeParameter p = unlines' [
    T.concat ["(", pName p , ")"]
  , T.concat ["    ", pDesc p]
  , T.concat ["    ", (T.pack . show . length $ concatMap expandValue $ pValues p), " values:"]
  , T.pack $ unlines $ map (("    - " ++) . show) (pValues p)
  ]

describeExecution :: Execution m -> Text
describeExecution e = T.pack $ intercalate " " [ ePath e
                                      , T.unpack $ sName (eScenario e)
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
    run' = error "should not arrive here"

data DescriptionQuery = ScenarioName [Text]
    deriving (Show)

parseParamQuery :: Text -> Maybe (QExpr Bool)
parseParamQuery str = let vals = T.splitOn ":" str in
    case vals of
    [k,"int",v]      -> Just (Eq (NCoerce (ScParam k)) (N . toRational $ unsafeReadText v))
    [k,"str",v]      -> Just (Eq (SCoerce (ScParam k)) (S v))
    _                -> Nothing

    where   unsafeReadText :: (Read a) => Text -> a 
            unsafeReadText = read . T.unpack

paramsToQuery :: [Text] -> QExpr Bool
paramsToQuery xs = let atoms = catMaybes (map parseParamQuery xs) in
  conjunctionQueries atoms

conjunctionQueries :: [QExpr Bool] -> QExpr Bool
conjunctionQueries []     = B True
conjunctionQueries (q:qs) = And q (conjunctionQueries qs)

filterDescriptions :: DescriptionQuery -> [ScenarioDescription m] -> [ScenarioDescription m]
filterDescriptions (ScenarioName []) xs = xs
filterDescriptions (ScenarioName ns) xs = filter ((flip elem ns) . sName) xs


runLabor :: [ScenarioDescription EnvIO] -> Labor -> IO ()
runLabor xs labor =
    case labor of
    (Describe scii)                 -> forM_ xs' (T.putStrLn . describeScenario)
    Find {}                         -> do (execs,_) <- runEnvIO loadMatching
                                          mapM_ (T.putStrLn . describeExecution) execs
    (Rm {})                         -> runSc loadAndRemove
    (Run { continue = False })      -> runSc execAll
    (Run { continue = True })       -> runSc execRemaining
    Analyze {}                      -> runSc loadAndAnalyze

    where xs'           = filterDescriptions (ScenarioName $ map T.pack $ scenarii labor) xs
          query         = paramsToQuery $ map T.pack $ params labor
          runSc         = void . runEnvIO
          loadMatching  = load defaultBackend xs' query
          loadAndRemove = loadMatching >>= mapM (remove defaultBackend)
          loadAndAnalyze= loadMatching >>= mapM (executeAnalysis defaultBackend)
          execAll       = forM_ xs' $ executeExhaustive defaultBackend
          execRemaining = forM_ xs' $ executeMissing defaultBackend
           
