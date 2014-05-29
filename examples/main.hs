{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
 
import Laborantin.DSL
import Laborantin.Types
import Laborantin.Implementation
import Laborantin.CLI
import qualified Data.Text as T

{- 
 - Example
 -}

pingDep :: ScenarioDescription EnvIO
pingDep = scenario "pingDep" $ do
  pingDepOut <- produces "raw-result"
  parameter "foo" $ do
    describe "foo"
    values [str "foo"]
  run $ do
    writeResult pingDepOut "a sort of result stored as a separate file"

ping :: ScenarioDescription EnvIO
ping = scenario "ping" $ do
  describe "ping to a remote server"
  pingDepOut <- consumes pingDep "raw-result" ["foo"]
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
    ancestors "pingDep" >>= liftIO . print . (\n ->  show n ++ " ancestor(s)") . length
    (Just who :: Maybe String) <- getVar "hello"
    liftIO . print . ("hello "++) $ who
    (StringParam srv) <- param "destination"
    readResults pingDepOut >>= liftIO . print . map snd
    case srv of
      "failure.example" -> err "noooo"
      host              -> dbg $ T.append "mimic sending ping to " host
  teardown $ dbg "here we could run some teardown action"
  recover $ \e -> dbg $ T.append "here we could recover from error: " (T.pack $ show e)
  analyze $ liftIO . print $ ("analyze action" :: String)

main :: IO ()
main = defaultMain [ping, pingDep]
