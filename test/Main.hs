module Main (main) where

import           Data.IORef
import           Yesod.Core

import           Application () -- YesodDispatch instance
import           Foundation

main :: IO ()
main = do
  clientIdRef <- newIORef Nothing
  portalIdRef <- newIORef Nothing
  authRef     <- newIORef Nothing
  warp 3000 App{..}
