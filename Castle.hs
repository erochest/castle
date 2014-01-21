{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Prelude
import           Shelly

import           Castle.Commands
import           Castle.Opts
import           Castle.Utils


main :: IO ()
main = do
    CastleOpts{..} <- execParser opts

    shelly $ verbosely $ do
        installCastle
        castle mode

