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
import qualified Shelly                    as S


-- Settings

castleDir :: IO FilePath
castleDir = (FS.</> ".castle") <$> getHomeDirectory

-- Shell utilities

cabal_ :: T.Text -> [T.Text] -> Sh ()
cabal_ = command1_ "cabal" []

sandbox_ :: T.Text -> [T.Text] -> Sh ()
sandbox_ cmd = cabal_ "sandbox" . (cmd:)

-- Workflow and utility functions

installCastle :: Sh ()
installCastle = do
    castle <- liftIO castleDir
    chdir (parent castle) $ do
        mkdirTree $ (filename castle) # leaves ["castles"]
    where (#)    = Node
          leaves = map (# [])

getSandboxDir :: T.Text -> Sh FilePath
getSandboxDir name =
    liftIO $ fmap ((FS.</> S.fromText name) . (FS.</> "castles")) castleDir

-- Command functions

castleList :: Sh ()
castleList =   liftIO (fmap (FS.</> "castles") castleDir)
           >>= ls
           >>= mapM_ (echo . toTextIgnore . basename)

castleNew :: T.Text -> Sh ()
castleNew castleName = do
    sandboxDir <- getSandboxDir castleName
    exists     <- test_d sandboxDir
    if exists
        then errorExit $ "Sandbox " <> castleName <> " already exists."
        else
            mkdir_p sandboxDir >> chdir sandboxDir
                (sandbox_ "init" ["--sandbox=" <> toTextIgnore sandboxDir])

castleUse :: T.Text -> Sh ()
castleUse castleName = do
    sandboxDir <- getSandboxDir castleName
    exists     <- test_d sandboxDir
    if exists
        then pwd >>= cp (sandboxDir FS.</> "cabal.sandbox.config")
        else errorExit $ "Sandbox " <> castleName <> " does not exist.\
                         \ Create it with 'sandbox new'."

-- Main

main :: IO ()
main = do
    cfg <- execParser opts

    shelly $ verbosely $ do
        installCastle

        case mode cfg of
            ListCmd           -> castleList
            NewCmd castleName -> castleNew castleName
            UseCmd castleName -> castleUse castleName

    where
        opts' =   CastleOpts
              <$> subparser (  O.command "list" listCmd
                            <> O.command "new"  newCmd
                            <> O.command "use"  useCmd
                            )

        listCmd = pinfo (pure ListCmd) "List sand castles." mempty
        newCmd  = pinfo (NewCmd <$> castleNameArg "The name of the castle to create.")
                        "Create a new castle." mempty
        useCmd  = pinfo (UseCmd <$> castleNameArg "The name of the castle to use.")
                        "Use an existing castle." mempty

        opts    = pinfo opts' "Manage shared cabal sandboxes."
                        (header "castle - manage shared cabal sandboxes.")

-- Command-line parsing

-- | This is a builder utility for ParserInfo instances.
pinfo :: Parser a -> String -> InfoMod a -> ParserInfo a
pinfo p desc imod = info (helper <*> p)
                         (fullDesc <> progDesc desc <> imod)

textOption :: Mod OptionFields T.Text -> Parser T.Text
textOption fields = nullOption (reader (pure . T.pack) <> fields)

fileOption :: Mod OptionFields FilePath -> Parser FilePath
fileOption fields = nullOption (reader (pure . decodeString) <> fields)

textArg :: String -> String -> Parser T.Text
textArg meta helpText = argument (Just . T.pack) (metavar meta <> help helpText)

castleNameArg :: String -> Parser T.Text
castleNameArg = textArg "CASTLE_NAME"

data CastleOpts
        = CastleOpts
        { mode :: CastleCmd
        } deriving (Show)

data CastleCmd
        = ListCmd
        | NewCmd { castleName :: T.Text }
        | UseCmd { castleName :: T.Text }
        deriving (Show)

