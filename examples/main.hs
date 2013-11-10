{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
 
import Laborantin
import Laborantin.Types
import Laborantin.DSL
import Laborantin.Implementation
import Laborantin.CLI
import Laborantin.Query
import Control.Monad.IO.Class
import qualified Data.Text as T

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
        str           -> dbg $ T.append "mimic sending ping to " str
    writeResult "raw-result" "a sort of result stored as a separate file"
  teardown $ dbg "here we could run some teardown action"
  recover $ \err -> dbg $ T.append "here we could recover from error: " (T.pack $ show err)
  analyze $ liftIO . print $ "analyze action"

main = defaultMain [ping]
