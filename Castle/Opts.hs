module Castle.Opts
    ( CastleOpts(..)
    , execParser
    , opts
    )
    where


import           Control.Applicative
import           Data.Monoid
import qualified Data.Text           as T
import           Options.Applicative
import qualified Options.Applicative as O

import           Castle.Types


data CastleOpts
        = CastleOpts
        { mode :: CastleCmd
        } deriving (Show)


-- | This is a builder utility for ParserInfo instances.
pinfo :: Parser a -> String -> InfoMod a -> ParserInfo a
pinfo p desc imod = info (helper <*> p) (fullDesc <> progDesc desc <> imod)

-- textOption :: Mod OptionFields T.Text -> Parser T.Text
-- textOption fields = nullOption (reader (pure . T.pack) <> fields)

-- fileOption :: Mod OptionFields FS.FilePath -> Parser FS.FilePath
-- fileOption fields = nullOption (reader (pure . decodeString) <> fields)

textArg :: String -> String -> Parser T.Text
textArg meta helpText = argument (Just . T.pack) (metavar meta <> help helpText)

castleNameArg :: String -> Parser T.Text
castleNameArg = textArg "CASTLE_NAME"

opts' :: Parser CastleOpts
opts' =   CastleOpts
      <$> subparser (  O.command "list"    listCmd
                    <> O.command "new"     newCmd
                    <> O.command "use"     useCmd
                    <> O.command "current" currCmd
                    <> O.command "remove"  rmCmd
                    <> O.command "delete"  delCmd
                    <> O.command "clear"   clrCmd
                    <> O.command "search"  srchCmd
                    )

listCmd, newCmd, useCmd, currCmd, rmCmd, delCmd, clrCmd
       , srchCmd :: ParserInfo CastleCmd

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
clrCmd  = pinfo (ClearCmd <$> castleNameArg "The name of the castle to clear.")
                "Clears a castle by deleting and re-creating it." mempty
srchCmd = pinfo (SearchCmd <$> textArg "QUERY" "Search the castles\
                                                \ for one matching the name.")
                "Searches for a castle with a name containing the QUERY." mempty

opts :: ParserInfo CastleOpts
opts    = pinfo opts' "Manage shared cabal sandboxes."
                (header "castle - manage shared cabal sandboxes.")



