{-# LANGUAGE OverloadedStrings #-}

module Gui.Gui
       ( runGui
       ) where

import           Control.Concurrent
import           Control.Exception
import           Data.IORef
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Graphics.QML
import           Prelude
import           System.FilePath

runGui :: IO ()
runGui = do
    state <- newIORef ""
    skey  <- newSignalKey
    clazz <- newClass [
        defPropertySigRO' "result" skey (\_ ->
            readIORef state),
        defMethod' "factorial" (\obj txt -> do
            let n = read $ T.unpack txt :: Integer
            writeIORef state "Working..."
            fireSignal skey obj
            _ <- forkIO $ do
                let out = T.take 1000 . T.pack . show $ product [1..n]
                _ <- evaluate out
                writeIORef state out
                fireSignal skey obj
            return ())]
    ctx <- newObject clazz ()
    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument qmlDoc,
        contextObject = Just $ anyObjRef ctx}
  where qmlDoc = "qml" </> "factorial2.qml"
