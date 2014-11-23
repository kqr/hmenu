module Main where

import Control.Concurrent (threadDelay)
import Control.Lens
import System.Exit        (exitWith, ExitCode(..))

import X
import Graphics
import MenuConf


main :: IO ()
main =
  -- withXConf sets up the necessary basic X configuration
  -- withMenuBar sets up the X window for the menu bar
  withXConf . withMenuBar $
    run "" (MenuConf "#000000" "#ff0077")


run :: String -> MenuConf -> WinConf -> IO ()
run str mc wc = do
  drawMenu mc wc
  ev <- parseXEvent (wc^.xc)
  case ev of
    Just Abort -> return ()
    _ -> run str mc wc

