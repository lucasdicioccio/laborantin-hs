{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
 
import Laborantin.DSL
import Laborantin.Types
import Laborantin.Implementation
import Laborantin.CLI
import qualified Data.Text as T
import Data.Monoid ((<>))

{- 
 - Example
 -}

hello :: ScenarioDescription EnvIO
hello = scenario "hello" $ do
  helloOut <- produces "hello-result"
  parameter "greeting" $ do
    describe "greeting with which we say hello"
    values [str "hello", str "hi"]
  parameter "who" $ do
    describe "who should we say hello to"
    values [str "world", str "laborantin"]
  run $ do
    StringParam how <- param "greeting"
    StringParam who <- param "who"
    writeResult helloOut $ how <> " " <> who

ping :: ScenarioDescription EnvIO
ping = scenario "ping" $ do
  describe "ping to a remote server"
  pingOut <- produces "raw-result"
  parameter "destination" $ do
    describe "a destination server (host or ip)"
    values [str "example.com", str "probecraft.net"]
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
    ancestors "hello" >>= liftIO . print . (\n ->  show n ++ " ancestor(s)") . length
    (Just who :: Maybe String) <- getVar "hello"
    liftIO . print . ("hello "++) $ who
    (StringParam srv) <- param "destination"
    case srv of
      "failure.example" -> err "noooo"
      host              -> do
        dbg $ "mimic sending ping to " <> host
        writeResult pingOut $ "ping " <> srv
  teardown $ dbg "here we could run some teardown action"
  recover $ \e -> dbg $ "here we could recover from error: " <> (T.pack $ show e)
  analyze $ liftIO . print $ ("analyze action" :: String)

pingAggregates :: ScenarioDescription EnvIO
pingAggregates = scenario "pingAggregates" $ do
  describe "compares ping results"
  pingOutputs <- consumes ping "raw-result" ["destination", "packet-size"]
  run $ do
    readResults pingOutputs >>= liftIO . print . map (\(x,y) -> (eParamSet x, y))

main :: IO ()
main = defaultMain [hello, ping, pingAggregates]
