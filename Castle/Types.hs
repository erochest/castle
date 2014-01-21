module Castle.Types
    ( CastleCmd(..)
    ) where


import           Data.Text (Text)


data CastleCmd
        = ListCmd
        | NewCmd    { castleName :: Text }
        | UseCmd    { castleName :: Text }
        | CurrentCmd
        | RemoveCmd
        | DeleteCmd { castleName :: Text }
        | ClearCmd  { castleName :: Text }
        | SearchCmd { searchQuery :: Text }
        deriving (Show)
