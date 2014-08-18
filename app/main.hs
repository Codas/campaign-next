{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
import           Application                (makeApplication)
import           Control.Concurrent         (forkIO)
import           Control.Monad              (when)
import           Control.Monad.Logger       (LogLevel (LevelError), liftLoc)
import           Gui.Gui                    (runGui)
import           Language.Haskell.TH.Syntax (qLocation)
import           Network.Wai.Handler.Warp   (defaultSettings, runSettings,
                                             settingsHost, settingsOnException,
                                             settingsPort)
import qualified Network.Wai.Handler.Warp   as Warp
import           Prelude                    (IO, const, show, ($), (++), (>>=))
import           Settings                   (parseExtra)
import           System.Log.FastLogger      (toLogStr)
import           Yesod.Default.Config
-- import           Yesod.Default.Main         (defaultMainLog)

main :: IO ()
main = do
    config <- load
    (application, logFunc, app) <- getApp config
    _ <- forkIO $ runSettings defaultSettings
        { settingsPort = appPort config
        , settingsHost = appHost config
        , settingsOnException = const $ \e -> when (shouldLog' e) $ logFunc
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e)
        } application
    runGui
  where shouldLog' = Warp.defaultShouldDisplayException
        load = fromArgs parseExtra
        getApp = makeApplication
