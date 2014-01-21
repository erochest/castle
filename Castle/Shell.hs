{-# LANGUAGE OverloadedStrings #-}


module Castle.Shell where


import           Data.Text (Text)
import           Shelly    hiding (cmd)


cabal_ :: Text -> [Text] -> Sh ()
cabal_ = command1_ "cabal" []

sandbox_ :: Text -> [Text] -> Sh ()
sandbox_ cmd = cabal_ "sandbox" . (cmd:)

