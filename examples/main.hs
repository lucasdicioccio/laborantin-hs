{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
 
import Laborantin
import Laborantin.Types
import Laborantin.DSL
import Laborantin.Implementation
import Control.Monad
import Control.Monad.IO.Class
import System.Environment
import Data.Map (toList)
import Data.List (intercalate)

{- 
 - Example
 -}

ping :: ScenarioDescription EnvIO
ping = scenario "ping" $ do
  describe "ping to a remote server"
  parameter "destination" $ do
    describe "a destination server (host or ip)"
    values [str "example.com", str "probecraft.net", str "nonexistent"]
  parameter "packet-size" $ do
    describe "packet size in bytes"
    values [num 50, num 1500] 
  parameter "burst-length" $ do
    describe "number of back-to-back packets to send"
    values [range 1 100 10] 
  setup $ do
      setVar "hello" ("world"::String)
      dbg "setting up scenario"
  run $ do
    (Just str :: Maybe String) <- getVar "hello"
    liftIO . print . ("hello "++) $ str
    (StringParam srv) <- param "destination"
    case srv of
        "nonexistent" -> err "noooo"
        str           -> dbg $ "mimic sending ping to " ++ str
    writeResult "raw-result" "a sort of result stored as a separate file"
  teardown $ dbg "here we could run some teardown action"
  recover $ \err -> dbg $ "here we could recover from error: " ++ show err
  analyze $ dbg "analyze action"

defaultMain scenarii = do
    args <- getArgs
    case args of
        ["describe"]    -> forM_ scenarii (putStrLn . describeScenario)
        ["find"]        -> (runEnvIO $ mapM (load defaultBackend) scenarii) >>= print
        ["run"]         -> void $runEnvIO $ forM_ scenarii $ executeExhaustive defaultBackend
        ["continue"]    -> void $runEnvIO $ forM_ scenarii $ executeMissing defaultBackend
        _ -> print "<describe|find|run|continue>"

unlines' :: [String] -> String
unlines' = intercalate "\n"

describeScenario :: ScenarioDescription m -> String
describeScenario sc = unlines [
    "# Scenario: " ++ sName sc
  , "    " ++ sDesc sc
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
  , "    default values:"
  , unlines $ map (("    - " ++) . show) (pValues p)
  ]

main = defaultMain [ping]
