{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Laborantin.CLI (defaultMain) where

import Control.Exception (finally)
import Options.Applicative
import Data.Time (UTCTime(..), getCurrentTime)
import System.Locale (defaultTimeLocale)
import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import Data.Either (rights)
import Data.Maybe (catMaybes)
import Control.Concurrent (readChan, writeChan, Chan (..), newChan)
import Control.Concurrent.Async (async)
import Control.Monad (replicateM_, forM_, void, (<=<))
import Data.Monoid
import Data.Aeson (encode)

import Data.List (intercalate)
import qualified Data.ByteString.Lazy.Char8 as C

import Laborantin.Types (UExpr (..), TExpr (..), ScenarioDescription (..), Execution (..), ParameterDescription (..), expandValue, paramSets, ResultDescription (..), FlowDirection (..), Dependency (..))
import Laborantin.Implementation (EnvIO, runEnvIO, defaultBackend, executionResultPath)
import Laborantin (load, remove, runAnalyze, prepare)
import Laborantin.Query.Interpret (toTExpr)
import Laborantin.Query.Parse (parseUExpr, ParsePrefs (..))
import Laborantin.Query (simplifyOneBoolLevel)

data Run = Run
  { runScenarios    :: [String]
  , runParams       :: [String]
  , runMatchers     :: [String]
  , runConcurrency  :: Int
  } deriving (Show)

data Continue = Continue
  { continueScenarios   :: [String]
  , continueParams      :: [String]
  , continueMatchers    :: [String]
  , continueFailed  :: Bool
  , continueTodayOnly   :: Bool
  , continueConcurrency :: Int
  } deriving (Show)

data Describe = Describe
  { describeScenarios :: [String]
  } deriving (Show)

data Find = Find
  { findScenarios   :: [String]
  , findParams      :: [String]
  , findMatchers    :: [String]
  , findFailed      :: Bool
  , findTodayOnly   :: Bool
  } deriving (Show)

data Results = Results
  { resultsScenarios   :: [String]
  , resultsNames       :: [String]
  , resultsParams      :: [String]
  , resultsMatchers    :: [String]
  , resultsFailed      :: Bool
  , resultsTodayOnly   :: Bool
  } deriving (Show)

data Analyze = Analyze
  { analyzeScenarios    :: [String]
  , analyzeParams       :: [String]
  , analyzeMatchers     :: [String]
  , analyzeFailed       :: Bool
  , analyzeTodayOnly    :: Bool
  , analyzeConcurrency  :: Int
  } deriving (Show)

data Rm = Rm
  { rmScenarios   :: [String]
  , rmParams      :: [String]
  , rmMatchers    :: [String]
  , rmFailed      :: Bool
  , rmTodayOnly   :: Bool
  } deriving (Show)

data Params = Params
  { paramsScenarios   :: [String]
  , paramsParams      :: [String]
  , paramsMatchers    :: [String]
  } deriving (Show)

data Query = Query
  { queryScenarios   :: [String]
  , queryParams      :: [String]
  , queryMatchers    :: [String]
  , queryFailed      :: Bool
  , queryTodayOnly   :: Bool
  } deriving (Show)

data Command = RunCommand Run
  | ContinueCommand Continue
  | DescribeCommand Describe
  | AnalyzeCommand Analyze
  | FindCommand Find
  | ResultsCommand Results
  | RmCommand Rm
  | ParamsCommand Params
  | QueryCommand Query
  deriving (Show)

scenariosOpt scii = many $ strOption (
     long "scenario"
  <> short 's'
  <> metavar "SCENARIOS"
  <> help "Names of the scenarios to run."
  <> completeWith scii)

paramsOpt = many $ strOption (
     long "param"
  <> short 'p'
  <> metavar "PARAMS"
  <> help "name:type:value tuple for parameter.")

resultNamesOpt = many $ strOption (
     long "result-names"
  <> short 'r'
  <> metavar "RESULTS"
  <> help "name of the result(s) to show")

matchersOpt = many $ strOption (
     long "matcher"
  <> short 'm'
  <> metavar "MATCHERS"
  <> help "Matcher queries to specify the parameter space.")

concurrencyLeveLOpt = option auto (
     long "concurrency"
  <> short 'C'
  <> value 1
  <> help "Max concurrent runs.")

failedFlag = switch (
     long "failed-only"
  <> help "Only account for failed runs.")

todayFlag = switch (
     long "today"
  <> help "Only account for today's runs.")

run :: [String] -> Parser Run
run scii = Run <$> scenariosOpt scii <*> paramsOpt <*> matchersOpt <*> concurrencyLeveLOpt

continue :: [String] -> Parser Continue
continue scii = Continue <$> scenariosOpt scii <*> paramsOpt <*> matchersOpt <*> failedFlag <*> todayFlag <*> concurrencyLeveLOpt

describe :: [String] -> Parser Describe
describe scii = Describe <$> scenariosOpt scii

find :: [String] -> Parser Find
find scii = Find <$> scenariosOpt scii <*> paramsOpt <*> matchersOpt <*> failedFlag <*> todayFlag

results :: [String] -> Parser Results
results scii = Results <$> scenariosOpt scii <*> resultNamesOpt <*> paramsOpt <*> matchersOpt <*> failedFlag <*> todayFlag

analyze :: [String] -> Parser Analyze
analyze scii = Analyze <$> scenariosOpt scii <*> paramsOpt <*> matchersOpt <*> failedFlag <*> todayFlag <*> concurrencyLeveLOpt

rm :: [String] -> Parser Rm
rm scii = Rm <$> scenariosOpt scii <*> paramsOpt <*> matchersOpt <*> failedFlag <*> todayFlag

query :: [String] -> Parser Query
query scii = Query <$> scenariosOpt scii <*> paramsOpt <*> matchersOpt <*> failedFlag <*> todayFlag

params :: [String] -> Parser Params
params scii = Params <$> scenariosOpt scii <*> paramsOpt <*> matchersOpt

runOpts :: [String] -> ParserInfo Run
runOpts scii = info (helper <*> run scii)
          ( fullDesc
         <> progDesc "Executes experiment scenarios."
         <> header "runs a scenario")

continueOpts :: [String] -> ParserInfo Continue
continueOpts scii = info (helper <*> continue scii)
          ( fullDesc
         <> progDesc "Executes missing scenarios."
         <> header "continues scenarios")

describeOpts :: [String] -> ParserInfo Describe
describeOpts scii = info (helper <*> describe scii)
          ( fullDesc
         <> progDesc "Describe scenarios in this project."
         <> header "describes scenarios")

findOpts :: [String] -> ParserInfo Find
findOpts scii = info (helper <*> find scii)
          ( fullDesc
         <> progDesc "Find scenarios executions."
         <> header "finds scenarios")

resultsOpts :: [String] -> ParserInfo Results
resultsOpts scii = info (helper <*> results scii)
          ( fullDesc
         <> progDesc "Find results from scenarios executions."
         <> header "finds results")

analyzeOpts :: [String] -> ParserInfo Analyze
analyzeOpts scii = info (helper <*> analyze scii)
          ( fullDesc
         <> progDesc "Analyze scenarios runs by replaying the 'analyze' hook."
         <> header "analyzes scenarios")

rmOpts :: [String] -> ParserInfo Rm
rmOpts scii = info (helper <*> rm scii)
          ( fullDesc
         <> progDesc "Deletes scenario runs, use carefully."
         <> header "removes scenarios")

queryOpts :: [String] -> ParserInfo Query
queryOpts scii = info (helper <*> query scii)
          ( fullDesc
         <> progDesc "Prints the query (for find-like commands) given other program args."
         <> header "removes scenarios")

paramsOpts :: [String] -> ParserInfo Params
paramsOpts scii = info (helper <*> params scii)
          ( fullDesc
         <> progDesc "Prints the params expansion (for run-like commands) given other program args."
         <> header "removes scenarios")

cmd :: [String] -> Parser Command
cmd scii = subparser ( command "run" (RunCommand <$> runOpts scii)
               <> command "continue" (ContinueCommand <$> continueOpts scii)
               <> command "describe" (DescribeCommand <$> describeOpts scii)
               <> command "find" (FindCommand <$> findOpts scii)
               <> command "results" (ResultsCommand <$> resultsOpts scii)
               <> command "analyze" (AnalyzeCommand <$> analyzeOpts scii)
               <> command "rm" (RmCommand <$> rmOpts scii)
               <> command "params" (ParamsCommand <$> paramsOpts scii)
               <> command "query" (QueryCommand <$> queryOpts scii))

mainCmd :: [String] -> ParserInfo Command
mainCmd scii = info (helper <*> cmd scii)
          ( fullDesc
         <> progDesc "Use subcommands to work with your Laborantin experiments."
         <> header "default Laborantin main script")

defaultMain :: [ScenarioDescription EnvIO] -> IO ()
defaultMain xs = do
   command <- execParser $ mainCmd (map T.unpack $ map sName xs)
   case command of
    RunCommand y      -> runMain xs y
    ContinueCommand y -> continueMain xs y
    DescribeCommand y -> describeMain xs y
    FindCommand y     -> findMain xs y
    ResultsCommand y  -> resultsMain xs y
    AnalyzeCommand y  -> analyzeMain xs y
    RmCommand y       -> rmMain xs y
    ParamsCommand y   -> paramsMain xs y
    QueryCommand y    -> queryMain xs y

-- double-plus non-good helper, should use a "saferead" version instead
unsafeReadText :: (Read a) => Text -> a 
unsafeReadText = read . T.unpack

-- concurrency helper
concurrentmapM_ :: Int -> (a -> IO b) -> [a] -> IO ()
concurrentmapM_ n f xs = do
    goChan <- newChan :: IO (Chan ())
    joinChan <- newChan :: IO (Chan ())
    let f' a = readChan goChan >> f a `finally` (writeChan goChan () >> writeChan joinChan ())
    mapM_ (async . f') xs
    replicateM_ n (writeChan goChan ()) 
    mapM_ (\_ -> readChan joinChan) xs

-- handy types to match Laborantin Scenario executions
newtype Conjunction a = Conjunction {unConjunction ::  a}
newtype Disjunction a = Disjunction {unDisjunction ::  a}

type QueryExpr = TExpr Bool

instance Monoid (Conjunction QueryExpr) where
  mempty                                  = Conjunction (B True)
  mappend (Conjunction x) (Conjunction y) = Conjunction (And x y)

instance Monoid (Disjunction QueryExpr) where
  mempty                                  = Disjunction (B False)
  mappend (Disjunction x) (Disjunction y) = Disjunction (Or x y)

allQueries :: [QueryExpr] -> QueryExpr
allQueries = unConjunction . mconcat . map Conjunction

anyQuery :: [QueryExpr] -> QueryExpr
anyQuery = unDisjunction . mconcat . map Disjunction

-- class and types to turn CLI parameters into Laborantin queries
class ToQueryExpr a where
  toQuery :: ParsePrefs -> a -> QueryExpr

instance ToQueryExpr QueryExpr where
  toQuery _ = id

newtype Params'     = Params' {unParams :: [String]}
newtype Scenarios'  = Scenarios' {unScenarios :: [String]}
newtype Matchers'   = Matchers' {unMatchers :: [String]}
newtype Failed'     = Failed' {unFailed :: Bool}
newtype TodayOnly'  = TodayOnly' {unTodayOnly :: (Bool, UTCTime)}

instance ToQueryExpr Params' where
  toQuery _ = paramsToTExpr . unParams
          where paramsToTExpr :: [String] -> QueryExpr
                paramsToTExpr xs =
                  let atoms = catMaybes (map (parseParamTExpr . T.pack) xs)
                  in allQueries atoms

                parseParamTExpr :: Text -> Maybe QueryExpr
                parseParamTExpr str =
                  let vals = T.splitOn ":" str in
                  case vals of
                    [k,"str",v]      -> Just (Eq (SCoerce (ScParam k))
                                                 (S v))
                    [k,"int",v]      -> Just (Eq (NCoerce (ScParam k))
                                                 (N . toRational $ unsafeReadText v))
                    [k,"ratio",v]    -> Just (Eq (NCoerce (ScParam k))
                                                 (N $ unsafeReadText v))
                    [k,"float",v]    -> Just (Eq (NCoerce (ScParam k))
                                                 (N $ toRational
                                                      (unsafeReadText v :: Float)))
                    _                -> Nothing


instance ToQueryExpr Scenarios' where
  toQuery _ = scenarsToTExpr . unScenarios
          where scenarsToTExpr :: [String] -> QueryExpr
                scenarsToTExpr [] = B True
                scenarsToTExpr scii =
                  let atoms = map (\name -> (Eq ScName (S $ T.pack name))) scii
                  in anyQuery atoms


instance ToQueryExpr Matchers' where
  toQuery prefs = allQueries
                . map (toTExpr (B True))
                . rights
                . map (parseUExpr prefs)
                . unMatchers

instance ToQueryExpr Failed' where
  toQuery _ = statusToTExpr . unFailed    
    where statusToTExpr :: Bool -> TExpr Bool
          statusToTExpr True  = Not (Eq ScStatus (S "success"))
          statusToTExpr False =     (Eq ScStatus (S "success"))

instance ToQueryExpr TodayOnly' where
  toQuery _ = uncurry todayToTExpr . unTodayOnly
    where todayToTExpr :: Bool -> UTCTime -> TExpr Bool
          todayToTExpr True today  = (Or (Eq ScTimestamp (T today))
                                         (Gt ScTimestamp (T today)))
          todayToTExpr False _     = B True

instance (ToQueryExpr a) => ToQueryExpr (Conjunction a) where
  toQuery prefs (Conjunction x) = toQuery prefs x

instance (ToQueryExpr a) => ToQueryExpr (Disjunction a) where
  toQuery prefs (Disjunction x) = toQuery prefs x

instance ToQueryExpr Run where
  toQuery prefs args = let
    wrap :: ToQueryExpr a => a -> Conjunction QueryExpr
    wrap a = Conjunction $ toQuery prefs $ a
    params'       = wrap $ Params'   $ runParams args
    scenarios'    = wrap $ Scenarios'$ runScenarios args
    matchers'     = wrap $ Matchers' $ runMatchers args
    in toQuery prefs (params' <> scenarios' <> matchers')

instance ToQueryExpr (Continue, UTCTime) where
  toQuery prefs (args, tst) = let
    wrap :: ToQueryExpr a => a -> Conjunction QueryExpr
    wrap a = Conjunction $ toQuery prefs $ a
    params'       = wrap $ Params'     $ continueParams args
    scenarios'    = wrap $ Scenarios'  $ continueScenarios args
    matchers'     = wrap $ Matchers'   $ continueMatchers args
    status'       = wrap $ Failed'     $ continueFailed args
    date'         = wrap $ TodayOnly'  $ (continueTodayOnly args, tst)
    in toQuery prefs (params' <> scenarios' <> matchers' <> status' <> date')

instance ToQueryExpr (Find, UTCTime) where
  toQuery prefs (args, tst) = let
    wrap :: ToQueryExpr a => a -> Conjunction QueryExpr
    wrap a = Conjunction $ toQuery prefs $ a
    params'       = wrap $ Params'     $ findParams args
    scenarios'    = wrap $ Scenarios'  $ findScenarios args
    matchers'     = wrap $ Matchers'   $ findMatchers args
    status'       = wrap $ Failed'     $ findFailed args
    date'         = wrap $ TodayOnly'  $ (findTodayOnly args, tst)
    in toQuery prefs (params' <> scenarios' <> matchers' <> status' <> date')

instance ToQueryExpr (Results, UTCTime) where
  toQuery prefs (args, tst) = let
    wrap :: ToQueryExpr a => a -> Conjunction QueryExpr
    wrap a = Conjunction $ toQuery prefs $ a
    params'       = wrap $ Params'     $ resultsParams args
    scenarios'    = wrap $ Scenarios'  $ resultsScenarios args
    matchers'     = wrap $ Matchers'   $ resultsMatchers args
    status'       = wrap $ Failed'     $ resultsFailed args
    date'         = wrap $ TodayOnly'  $ (resultsTodayOnly args, tst)
    in toQuery prefs (params' <> scenarios' <> matchers' <> status' <> date')

instance ToQueryExpr (Analyze, UTCTime) where
  toQuery prefs (args, tst) = let
    wrap :: ToQueryExpr a => a -> Conjunction QueryExpr
    wrap a = Conjunction $ toQuery prefs $ a
    params'       = wrap $ Params'     $ analyzeParams args
    scenarios'    = wrap $ Scenarios'  $ analyzeScenarios args
    matchers'     = wrap $ Matchers'   $ analyzeMatchers args
    status'       = wrap $ Failed'     $ analyzeFailed args
    date'         = wrap $ TodayOnly'  $ (analyzeTodayOnly args, tst)
    in toQuery prefs (params' <> scenarios' <> matchers' <> status' <> date')

instance ToQueryExpr (Rm, UTCTime) where
  toQuery prefs (args, tst) = let
    wrap :: ToQueryExpr a => a -> Conjunction QueryExpr
    wrap a = Conjunction $ toQuery prefs $ a
    params'       = wrap $ Params'     $ rmParams args
    scenarios'    = wrap $ Scenarios'  $ rmScenarios args
    matchers'     = wrap $ Matchers'   $ rmMatchers args
    status'       = wrap $ Failed'     $ rmFailed args
    date'         = wrap $ TodayOnly'  $ (rmTodayOnly args, tst)
    in toQuery prefs (params' <> scenarios' <> matchers' <> status' <> date')

instance ToQueryExpr Params  where
  toQuery prefs args = let
    wrap :: ToQueryExpr a => a -> Conjunction QueryExpr
    wrap a = Conjunction $ toQuery prefs $ a
    params'       = wrap $ Params'     $ paramsParams args
    scenarios'    = wrap $ Scenarios'  $ paramsScenarios args
    matchers'     = wrap $ Matchers'   $ paramsMatchers args
    in toQuery prefs (params' <> scenarios' <> matchers')

instance ToQueryExpr (Query, UTCTime) where
  toQuery prefs (args, tst) = let
    wrap :: ToQueryExpr a => a -> Conjunction QueryExpr
    wrap a = Conjunction $ toQuery prefs $ a
    params'       = wrap $ Params'     $ queryParams args
    scenarios'    = wrap $ Scenarios'  $ queryScenarios args
    matchers'     = wrap $ Matchers'   $ queryMatchers args
    status'       = wrap $ Failed'     $ queryFailed args
    date'         = wrap $ TodayOnly'  $ (queryTodayOnly args, tst)
    in toQuery prefs (params' <> scenarios' <> matchers' <> status' <> date')


-- Extra helpers

cliScenarios :: [String] -> [ScenarioDescription m] -> [ScenarioDescription m]
cliScenarios [] scii    = scii
cliScenarios names scii = [sc | sc <- scii, sName sc `elem` map T.pack names]

-- | Main program for the 'run' command.
runMain :: [ScenarioDescription EnvIO] -> Run -> IO ()
runMain scii args = do
  let scenarios = cliScenarios (runScenarios args) scii
      query     = toQuery (ParsePrefs defaultTimeLocale) args
      execs     = concatMap (prepare defaultBackend query []) scenarios
  concurrentmapM_ (runConcurrency args) runEnvIO execs

-- | Main program for the 'continue' command.
continueMain :: [ScenarioDescription EnvIO] -> Continue -> IO ()
continueMain scii args = do
  now <- getCurrentTime
  let scenarios = cliScenarios (continueScenarios args) scii
      query = toQuery (ParsePrefs defaultTimeLocale) (args, now)
      loadMatching  = load defaultBackend scenarios query
  matching <- runEnvIO loadMatching
  let execs     = concatMap (prepare defaultBackend query matching) scenarios
  concurrentmapM_ (continueConcurrency args) runEnvIO execs

-- | Main program for the 'describe' command.
-- TODO: use the query information to expand parameter values
describeMain :: [ScenarioDescription EnvIO] -> Describe -> IO ()
describeMain scii args = do
  let scenarios = cliScenarios (describeScenarios args) scii
  forM_ scenarios (T.putStrLn . describeScenario)

  where describeScenario :: ScenarioDescription m -> Text
        describeScenario sc = T.unlines [
            T.append "# Scenario: " (sName sc)
          , T.append "    " (sDesc sc)
          , T.concat ["    ", (T.pack . show . length . paramSets $ sParams sc), " parameter combinations by default"]
          , "## Dependencies:"
          , unlines' $ map depLine $ sDeps sc
          , "## Produces:"
          , unlines' $ map productLine $ sProduced sc
          , "## Consumes:"
          , unlines' $ map consumableLine $ sConsumed sc
          , "## Parameters:"
          , unlines' $ map (uncurry paramLine) $ M.toList $ sParams sc
          ]

        unlines' :: [Text] -> Text
        unlines' = T.intercalate "\n"

        depLine d = T.concat [dName d, " (", dDesc d, ")"]

        paramLine n p = unlines' [
                          T.append "### " n
                        , describeParameter p
                        ]

        productLine p = describeProduct p

        consumableLine p = describeConsumedResult p

        describeParameter :: ParameterDescription -> Text
        describeParameter p = unlines' [
            T.concat ["(", pName p , ")"]
          , T.concat ["    ", pDesc p]
          , T.concat ["    ", (T.pack . show . length $ concatMap expandValue $ pValues p), " values:"]
          , T.pack $ unlines $ map (("    - " ++) . show) (pValues p)
          ]

        describeProduct :: ResultDescription Produced -> Text
        describeProduct (RDesc path) = T.pack path

        describeConsumedResult :: ResultDescription Consumed -> Text
        describeConsumedResult (RDescC (SDesc {sName=n}) path) = T.concat [T.pack path, " from ", n]



-- | Main program for the 'find' command.
findMain :: [ScenarioDescription EnvIO] -> Find -> IO ()
findMain scii args = do
  now <- getCurrentTime
  let scenarios = cliScenarios (findScenarios args) scii
      query = toQuery (ParsePrefs defaultTimeLocale) (args, now)
      loadMatching  = load defaultBackend scenarios query
  matching <- runEnvIO loadMatching
  forM_ matching (T.putStrLn . describeExecution)
  where describeExecution :: Execution m -> Text
        describeExecution e = T.pack $ intercalate " " [ ePath e
                                      , T.unpack $ sName (eScenario e)
                                      , "(" ++ show (eStatus e) ++ ")"
                                      , C.unpack $ encode (eParamSet e)
                                      ]

-- | Main program for the 'results' command.
resultsMain :: [ScenarioDescription EnvIO] -> Results -> IO ()
resultsMain scii args = do
  now <- getCurrentTime
  let scenarios = cliScenarios (resultsScenarios args) scii
      resultnames = resultsNames args
      query = toQuery (ParsePrefs defaultTimeLocale) (args, now)
      loadMatching  = load defaultBackend scenarios query
  if null resultnames
  then T.putStrLn "needs at least one result name" >> exitFailure
  else do
          matching <- runEnvIO loadMatching
          forM_ matching (T.putStrLn <=< describeResults resultnames)
  where describeResults :: [FilePath] -> Execution m -> IO Text
        describeResults names e = do
          let rPaths = map (executionResultPath e) names
          validPaths <- mapM doesFileExist rPaths
          return $ T.pack $ unlines $ zipWith (\x y -> showSuccess x ++ (' ':y)) validPaths rPaths
        showSuccess :: Bool -> String
        showSuccess True = "(ok)"
        showSuccess _rue = "(KO)"

-- | Main program for the 'analyze' command.
analyzeMain :: [ScenarioDescription EnvIO] -> Analyze -> IO ()
analyzeMain scii args = do
  now <- getCurrentTime
  let scenarios = cliScenarios (analyzeScenarios args) scii
      query = toQuery (ParsePrefs defaultTimeLocale) (args, now)
      loadMatching  = load defaultBackend scenarios query
  matching <- runEnvIO loadMatching
  let analyses = map (runAnalyze defaultBackend) matching
  concurrentmapM_ (analyzeConcurrency args) runEnvIO analyses

-- | Main program for the 'rm' command.
rmMain :: [ScenarioDescription EnvIO] -> Rm -> IO ()
rmMain scii args = do
  now <- getCurrentTime
  let scenarios = cliScenarios (rmScenarios args) scii
      query = toQuery (ParsePrefs defaultTimeLocale) (args, now)
      loadMatching  = load defaultBackend scenarios query
  matching <- runEnvIO loadMatching
  let deletions = map (remove defaultBackend) matching
  forM_ deletions runEnvIO

-- | Main program for the 'params' command.
paramsMain :: [ScenarioDescription EnvIO] -> Params -> IO ()
paramsMain scii args = do
  let query = toQuery (ParsePrefs defaultTimeLocale) args
  print $ simplifyOneBoolLevel $ query

-- | Main program for the 'query' command.
queryMain :: [ScenarioDescription EnvIO] -> Query -> IO ()
queryMain scii args = do
  now <- getCurrentTime
  let query = toQuery (ParsePrefs defaultTimeLocale) (args, now)
  print $ simplifyOneBoolLevel $ query
