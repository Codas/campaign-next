{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TupleSections     #-}
module Handler.Home where

import           Import

-- import           Control.Concurrent.STM
import           Conduit
import           Control.Monad              (forever)
import           Control.Monad.Trans.Reader
import           Data.Text.IO               (hGetContents)
import           System.Exit                (ExitCode (..))
import           System.FilePath
import           System.Process
import           Yesod.WebSockets


compileElm :: String -> IO (ExitCode, Text)
compileElm file = do (_, outHandle, _, processHandle) <- createProcess elmCommand
                     status <- waitForProcess processHandle
                     out <- maybe (pure ("" :: Text)) hGetContents outHandle
                     return (status, out)
  where elmCommand = (proc "elm" options) { cwd = Just "elm" }
        options = ["-m", "--set-runtime=static/js/elm-runtime.js", "-b"
                  ,"../static/js/", "-c", "../static/tmp/", "--only-js", file]

sendElm :: String -> Handler Html
sendElm file = sendFile "text/html" (elmPath </> file ++ ".html")
                  -- (status, out) <- liftIO $ compileElm (file ++ ".elm")
                  -- case status of
                  --     ExitFailure 1 -> defaultLayout [whamlet|#{out}|]
                  --       _ -> sendFile "text/html" (elmPath </> file ++ ".html")
  where elmPath = "static" </> "html"

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do webSockets testSocket
              sendElm "index"

testSocket :: WebSocketsT Handler ()
testSocket = do
    sendTextData ("Welcome to the chat server, please enter your name." :: Text)
    name <- receiveData
    sendTextData $ "Welcome, " <> name
    app <- getYesod
    let writeChan = socketEvents app
    readChan <- atomically $ do
        writeTChan writeChan $ name <> " has joined the chat"
        dupTChan writeChan
    race_
        (forever $ atomically (readTChan readChan) >>= sendTextData)
        (sourceWS $$ mapM_C (\msg ->
            atomically $ writeTChan writeChan $ name <> ": " <> msg))
