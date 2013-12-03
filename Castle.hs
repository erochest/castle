{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Data.Maybe
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

withSandbox :: T.Text -> (FilePath -> Sh ()) -> (FilePath -> Sh ()) -> Sh ()
withSandbox name onExists onFail = do
    sandboxDir <- getSandboxDir name
    exists     <- test_d sandboxDir
    if exists
        then onExists sandboxDir
        else onFail sandboxDir

getConfigFile :: Sh FilePath
getConfigFile = (FS.</> "cabal.sandbox.config") <$> pwd

-- Command function

castle :: CastleCmd -> Sh ()
castle ListCmd =   liftIO (fmap (FS.</> "castles") castleDir)
               >>= ls
               >>= mapM_ (echo . toTextIgnore . basename)

castle NewCmd{..} = withSandbox
    castleName
    (const $ errorExit $ "Sandbox " <> castleName <> " already exists.")
    (\d -> mkdir_p d >> chdir d
            (sandbox_ "init" ["--sandbox=" <> toTextIgnore d]))

castle UseCmd{..} = withSandbox
    castleName
    (\d -> pwd >>= cp (d FS.</> "cabal.sandbox.config"))
    (const $ errorExit $ "Sandbox " <> castleName <> " does not exist.\
                         \ Create it with 'sandbox new'.")

castle CurrentCmd = do
    configFile <- getConfigFile
    whenM (not <$> test_f configFile) $
        errorExit "No sandbox in this directory."
    config <- T.lines <$> readfile configFile
    maybe (errorExit "No 'prefix:' line in configuration file.")
          (echo . toTextIgnore . FS.basename . FS.fromText . T.drop 10)
          . listToMaybe
          $ filter (T.isPrefixOf "  prefix: ") config

castle RemoveCmd = do
    configFile <- getConfigFile
    whenM (not <$> test_f configFile) $
        errorExit "No sandbox in this directory."
    rm configFile

castle DeleteCmd{..} = withSandbox
    castleName
    rm_rf
    (const $ errorExit $ "Sandbox " <> castleName <> " does not exist.")

-- Main

main :: IO ()
main = do
    CastleOpts{..} <- execParser opts

    shelly $ verbosely $ do
        installCastle
        castle mode

    where
        opts' =   CastleOpts
              <$> subparser (  O.command "list"    listCmd
                            <> O.command "new"     newCmd
                            <> O.command "use"     useCmd
                            <> O.command "current" currCmd
                            <> O.command "remove"  rmCmd
                            <> O.command "delete"  delCmd
                            )

        listCmd = pinfo (pure ListCmd) "List sand castles." mempty
        newCmd  = pinfo (NewCmd <$> castleNameArg "The name of the castle to create.")
                        "Create a new castle." mempty
        useCmd  = pinfo (UseCmd <$> castleNameArg "The name of the castle to use.")
                        "Use an existing castle." mempty
        currCmd = pinfo (pure CurrentCmd) "Display the current castle name."
                        mempty
        rmCmd   = pinfo (pure RemoveCmd) "Removes the sandbox from the current directory."
                        mempty
        delCmd  = pinfo (DeleteCmd <$> castleNameArg "The name of the castle to delete.")
                        "Deletes the castle." mempty

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
        | NewCmd    { castleName :: T.Text }
        | UseCmd    { castleName :: T.Text }
        | CurrentCmd
        | RemoveCmd
        | DeleteCmd { castleName :: T.Text }
        deriving (Show)

