{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application where

import Yesod.Core

import Auth
import Forms
import Foundation
import Home

mkYesodDispatch "App" resourcesApp
