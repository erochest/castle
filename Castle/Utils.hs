{-# LANGUAGE OverloadedStrings #-}

module Castle.Utils
    ( installCastle
    , withSandbox
    , withSandbox'
    , listCastles
    ) where


import           Control.Monad
import           Data.Text                 hiding (map)
import           Data.Tree
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)
import           Shelly                    hiding ((</>))

import           Castle.Paths


installCastle :: Sh ()
installCastle = do
    castle <- liftIO castleDir
    chdir (parent castle) $ do
        mkdirTree $ (filename castle) # leaves ["castles"]
    where (#)    = Node
          leaves = map (# [])

withSandbox :: Text -> (FilePath -> Sh ()) -> (FilePath -> Sh ()) -> Sh ()
withSandbox name onExists onFail = do
    sandboxDir <- getSandboxDir name
    exists     <- test_d sandboxDir
    if exists
        then onExists sandboxDir
        else onFail   sandboxDir

withSandbox' :: Text -> (FilePath -> Sh ()) -> Sh ()
withSandbox' name onExists = withSandbox name onExists noop
    where noop = const $ return ()

listCastles :: Sh [Text]
listCastles =   liftIO (fmap (</> "castles") castleDir)
            >>= ls
            >>= fmap (map (toTextIgnore . basename)) . filterM test_d

