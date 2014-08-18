{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import           Control.Monad.Logger                 (runLoggingT)
import           Data.Default                         (def)
import qualified Database.Persist
import           Database.Persist.Sql                 (runMigration)
import           Import
import           Network.HTTP.Client.Conduit          (newManager)
import           Network.Wai.Logger                   (clockDateCacher)
import           Network.Wai.Middleware.RequestLogger (IPAddrSource (..),
                                                       OutputFormat (..),
                                                       destination,
                                                       mkRequestLogger,
                                                       outputFormat)
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import           Settings
import           System.Log.FastLogger                (defaultBufSize,
                                                       newStdoutLoggerSet)
import           Yesod.Auth
import           Yesod.Core.Types                     (Logger (Logger),
                                                       loggerSet)
import           Yesod.Default.Config
import           Yesod.Default.Handlers
import           Yesod.Default.Main

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import           Handler.Home

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO (Application, LogFunc, App)
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    let logFunc = messageLoggerSource foundation (appLogger foundation)
    return (logWare $ defaultMiddlewaresNoLogging app, logFunc, foundation)

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager
    s <- staticSite
    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)

    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, _) <- clockDateCacher

    eventChannel <- atomically newBroadcastTChan
    let logger = Yesod.Core.Types.Logger loggerSet' getter
        foundation = App { settings = conf
                         , getStatic = s
                         , connPool = p
                         , httpManager = manager
                         , persistConfig = dbconf
                         , appLogger = logger
                         , socketEvents = eventChannel}

    -- Perform database migration using our application's logging settings.
    runLoggingT
        (Database.Persist.runPool dbconf (runMigration migrateAll) p)
        (messageLoggerSource foundation logger)

    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader (fmap fst . makeApp)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
    makeApp :: AppConfig DefaultEnv Extra -> IO (Application, LogFunc)
    makeApp conf = do (app, logFunc, _) <- makeApplication conf
                      return (app, logFunc)
