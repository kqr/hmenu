{-# LANGUAGE TemplateHaskell #-}

module MenuConf where

import Control.Lens

data MenuConf = MenuConf
  { _fgc :: String
  , _bgc :: String
  }
$(makeLenses ''MenuConf)


data AppEvent
  = Abort
  | Insert Char
  | Backspace
  | Confirm

