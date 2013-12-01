{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Tree
import           Filesystem
import           Filesystem.Path.CurrentOS as FS
import           Options.Applicative
import qualified Options.Applicative       as O
import           Prelude                   hiding (FilePath)
import           Shelly


castleDir :: IO FilePath
castleDir = (FS.</> ".castle") <$> getHomeDirectory

castleList :: Sh ()
castleList =   liftIO (fmap (FS.</> "castles") castleDir)
           >>= ls
           >>= mapM_ (echo . toTextIgnore . basename)

installCastle :: Sh ()
installCastle = do
    castle <- liftIO castleDir
    chdir (parent castle) $ do
        mkdirTree $ (filename castle) # leaves ["castles"]
    where (#)    = Node
          leaves = map (# [])


main :: IO ()
main = do
    cfg <- execParser opts

    shelly $ verbosely $ do
        installCastle

        case mode cfg of
            ListCmd -> castleList

    where
        opts' =   CastleOpts
              <$> subparser (O.command "list" listCmd)
        listCmd = pinfo (pure ListCmd) "List sand castles." mempty
        opts    = pinfo opts' "Manage shared cabal sandboxes."
                        (header "castle - manage shared cabal sandboxes.")

-- | This is a builder utility for ParserInfo instances.
pinfo :: Parser a -> String -> InfoMod a -> ParserInfo a
pinfo p desc imod = info (helper <*> p)
                         (fullDesc <> progDesc desc <> imod)

data CastleOpts
        = CastleOpts
        { mode :: CastleMode
        } deriving (Show)

data CastleMode
        = ListCmd
        deriving (Show)

