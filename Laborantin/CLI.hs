{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module Laborantin.CLI (defaultMain) where

import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async
import Laborantin
import Laborantin.Types
import Laborantin.Implementation
import Laborantin.Query
import Laborantin.Query.Parse
import Laborantin.Query.Interpret
import System.Environment
import System.Console.CmdLib hiding (run)
import qualified Data.Map as M
import Data.List (intercalate)
import Data.Aeson (encode)
import Data.Maybe (catMaybes)
import Data.Either (rights)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List.Split (splitOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time (UTCTime(..), getCurrentTime)
import System.Locale

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


data Labor = Run        { scenarii   :: [String]
                        , params :: [String]
                        , matcher :: [String]
                        , concurrency :: Int
                        }
           | Continue   { scenarii   :: [String]
                        , params :: [String]
                        , matcher :: [String]
                        , successful :: Bool
                        , today :: Bool, matcher :: [String]
                        }
           | Describe   { scenarii   :: [String]
                        }
           | Find       { scenarii   :: [String]
                        , params :: [String]
                        , successful :: Bool
                        , today :: Bool, matcher :: [String]
                        }
           | Analyze    { scenarii   :: [String]
                        , params :: [String]
                        , successful :: Bool
                        , today :: Bool, matcher :: [String]
                        }
           | Rm         { scenarii   :: [String]
                        , params :: [String]
                        , successful :: Bool
                        , today :: Bool, matcher :: [String]
                        }
           | Query      { scenarii   :: [String]
                        , params :: [String]
                        , successful :: Bool
                        , today :: Bool, matcher :: [String]
                        }
           | Params     { scenarii   :: [String]
                        , params :: [String]
                        , matcher :: [String]
                        }
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
                    ,   successful %> [ Long ["successful"]
                                    , Help "Successful only"
                                    , Invertible True
                                    , Default True
                                    ]
                    ,   today %> [ Long ["today"]
                                    , Help "Today only (currently uses UTC day!)"
                                    , Invertible True
                                    , Default False
                                    ]
                    ,   matcher  %> [ Short "m"
                                    , Long ["matcher"]
                                    , Help "Restrict to a matching expression"
                                    , ArgHelp "MATCHER EXPRESSION"
                                    ]
                    ,   concurrency  %> [ Short "c"
                                    , Long ["concurrency"]
                                    , Help "Number of experiments to run at a same time"
                                    , ArgHelp "CONCURRENT RUNS"
                                    , Default (1::Int)
                                    ]
                    ]

instance RecordCommand Labor where
    mode_summary _ = "Laborantin command-line interface"
    run' = error "should not arrive here"

data DescriptionTExpr = ScenarioName [Text]
    deriving (Show)

parseParamTExpr :: Text -> Maybe (TExpr Bool)
parseParamTExpr str = let vals = T.splitOn ":" str in
    case vals of
    [k,"ratio",v]    -> Just (Eq (NCoerce (ScParam k)) (N $ unsafeReadText v))
    [k,"int",v]      -> Just (Eq (NCoerce (ScParam k)) (N . toRational $ unsafeReadText v))
    [k,"float",v]    -> Just (Eq (NCoerce (ScParam k)) (N $ toRational (unsafeReadText v :: Float)))
    [k,"str",v]      -> Just (Eq (SCoerce (ScParam k)) (S v))
    _                -> Nothing

    where   unsafeReadText :: (Read a) => Text -> a 
            unsafeReadText = read . T.unpack

paramsToTExpr :: [Text] -> TExpr Bool
paramsToTExpr xs = let atoms = catMaybes (map parseParamTExpr xs) in
  conjunctionQueries atoms

-- if no scenario: True, otherwise any of the scenarios
scenarsToTExpr :: [Text] -> TExpr Bool
scenarsToTExpr [] = B True
scenarsToTExpr scii = let atoms = map (\name -> (Eq ScName (S name))) scii in
    disjunctionQueries atoms

statusToTExpr :: Bool -> TExpr Bool
statusToTExpr True  =     (Eq ScStatus (S "success"))
statusToTExpr False = Not (Eq ScStatus (S "success"))

todayToTExpr :: Bool -> UTCTime -> TExpr Bool
todayToTExpr True today  = (Or (Eq ScTimestamp (T today)) (Gt ScTimestamp (T today)))
todayToTExpr False _     = B True

conjunctionQueries :: [TExpr Bool] -> TExpr Bool
conjunctionQueries []     = B True
conjunctionQueries [q]    = q
conjunctionQueries (q:qs) = And q (conjunctionQueries qs)

disjunctionQueries :: [TExpr Bool] -> TExpr Bool
disjunctionQueries []     = B False
disjunctionQueries [q]    = q
disjunctionQueries (q:qs) = Or q (disjunctionQueries qs)

filterDescriptions :: DescriptionTExpr -> [ScenarioDescription m] -> [ScenarioDescription m]
filterDescriptions (ScenarioName []) xs = xs
filterDescriptions (ScenarioName ns) xs = filter ((flip elem ns) . sName) xs

concurrentmapM_ :: Int -> (a -> IO b) -> [a] -> IO ()
concurrentmapM_ n f xs = do
    goChan <- newChan :: IO (Chan ())
    joinChan <- newChan :: IO (Chan ())
    let f' a = readChan goChan >> f a >> writeChan goChan () >> writeChan joinChan ()
    mapM_ (async . f') xs
    replicateM_ n (writeChan goChan ()) 
    mapM_ (\_ -> readChan joinChan) xs


runLabor :: [ScenarioDescription EnvIO] -> Labor -> IO ()
runLabor xs labor = do
    now <- getCurrentTime
    case labor of
        (Describe scii)                 -> forM_ xs' (T.putStrLn . describeScenario)
        Find {}                         -> do execs <- runEnvIO (loadMatching now)
                                              mapM_ (T.putStrLn . describeExecution) execs
        Rm {}                           -> runSc (loadAndRemove now)
        Run {}                          -> do concurrentmapM_ (concurrency labor) runSc (targetExecs [])
        Continue {}                     -> do execs <- runEnvIO (loadMatching now)
                                              mapM_ runSc (targetExecs execs)
        Analyze {}                      -> runSc (loadAndAnalyze now)
        Query {}                        -> do let expr = simplifyOneBoolLevel $ query now
                                              print expr
        Params {}                       -> do let expr = simplifyOneBoolLevel expander 
                                              print expr

        where xs'           = filterDescriptions (ScenarioName $ map T.pack $ scenarii labor) xs
              prefs         = ParsePrefs defaultTimeLocale
              matcherUExprs = rights $ map (parseUExpr prefs) (matcher labor)
              matcherTExprs = map (toTExpr (B True)) matcherUExprs
              paramsTExpr   = paramsToTExpr $ map T.pack $ params labor
              scenarioTExpr = scenarsToTExpr $ map T.pack $ scenarii labor
              statusTExpr   = statusToTExpr $ successful labor
              dateTExpr tst = todayToTExpr (today labor) (tst {utctDayTime = 0})
              query tst     = conjunctionQueries (paramsTExpr:scenarioTExpr:statusTExpr:dateTExpr':matcherTExprs)
                              where dateTExpr' = dateTExpr tst
              expander      = conjunctionQueries (paramsTExpr:scenarioTExpr:matcherTExprs)
              runSc         = void . runEnvIO
              loadMatching  tst = load defaultBackend xs' (query tst)
              loadAndRemove  tst = loadMatching tst >>= mapM (remove defaultBackend)
              loadAndAnalyze tst = loadMatching tst >>= mapM (runAnalyze defaultBackend)
              targetExecs existing = concatMap (prepare defaultBackend expander existing) xs'
