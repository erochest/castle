{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Castle.Commands
    ( castle
    ) where


import           Control.Applicative
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                 as T
import           Filesystem.Path.CurrentOS
import           Network.URI
import           Prelude                   hiding (FilePath)
import           Shelly                    hiding (fromText, (</>))

import           Castle.Paths
import           Castle.Shell
import           Castle.Types
import           Castle.Utils

castle :: CastleCmd -> Sh ()
castle ListCmd = mapM_ echo =<< listCastles

castle NewCmd{..} = withSandbox castleName
    (const $ errorExit $ "Sandbox " <> castleName <> " already exists.")
    (\d -> mkdir_p d >> chdir d
            (sandbox_ "init" ["--sandbox=" <> toTextIgnore d]))

castle UseCmd{..} = withSandbox castleName
    (\d -> pwd >>= cp (d </> "cabal.sandbox.config"))
    (const $ errorExit $ "Sandbox " <> castleName <> " does not exist.\
                         \ Create it with 'sandbox new'.")

castle CurrentCmd = do
    configFile <- getConfigFile
    whenM (not <$> test_f configFile) $
        errorExit "No sandbox in this directory."
    config <- T.lines <$> readfile configFile
    maybe (errorExit "No 'prefix:' line in configuration file.")
          (echo . toTextIgnore . basename . fromText . T.drop 10)
          . listToMaybe
          $ filter (T.isPrefixOf "  prefix: ") config

castle RemoveCmd = do
    configFile <- getConfigFile
    whenM (not <$> test_f configFile) $
        errorExit "No sandbox in this directory."
    rm configFile

castle DeleteCmd{..} = withSandbox castleName
    rm_rf
    (const $ errorExit $ "Sandbox " <> castleName <> " does not exist.")

castle ClearCmd{..} =
    withSandbox' castleName rm_rf >> castle (NewCmd castleName)

castle SearchCmd{..} =
    mapM_ echo =<< filter (T.isInfixOf searchQuery) <$> listCastles

castle AddSrcCmd{..} =
    inspect . parseURI $ T.unpack sourceUri


