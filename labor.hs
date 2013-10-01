{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Labor where
 
import Laborantin
import Laborantin.Types
import Laborantin.DSL
import Laborantin.Implementation
import Control.Monad
import Control.Monad.IO.Class

{- 
 - Example
 -}

ping :: ScenarioDescription EnvIO
ping = scenario "ping" $ do
  describe "ping to a remote server"
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
    (Just str :: Maybe String) <- getVar "hello"
    liftIO . print . ("hello "++) $ str
    (StringParam srv) <- param "destination"
    dbg $ "mimic sending ping to " ++ srv
    writeResult "raw-result" "a sort of result stored as a separate file"
  teardown $ dbg "here we could run some teardown action"
  recover $ dbg "here we could recover from error"
  analyze $ dbg "analyze action"

defaultMain scenarii = runEnvIO $ forM_ scenarii $ executeExhaustive defaultBackend
