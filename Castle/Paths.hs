{-# LANGUAGE OverloadedStrings #-}


module Castle.Paths
    ( castleDir
    , getSandboxDir
    , getConfigFile
    ) where


import           Control.Applicative
import           Data.Text                 (Text)
import           Filesystem
import           Filesystem.Path.CurrentOS hiding (fromText)
import           Prelude                   hiding (FilePath)
import           Shelly                    (Sh, fromText, liftIO, pwd)


castleDir :: IO FilePath
castleDir = (</> ".castle") <$> getHomeDirectory

getSandboxDir :: Text -> Sh FilePath
getSandboxDir name = liftIO $ fmap subdir castleDir
    where subdir base = base </> "castles" </> fromText name

getConfigFile :: Sh FilePath
getConfigFile = (</> "cabal.sandbox.config") <$> pwd

