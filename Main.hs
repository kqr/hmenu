module Main where

import Control.Concurrent (threadDelay)
import Control.Lens
import System.Exit        (exitWith, ExitCode(..))

import X
import Graphics
import MenuConf


strs = ["räksmörgåßαr", "hello, world!", "conjure me", "and more"]

main :: IO ()
main =
  -- withXConf sets up the necessary basic X configuration
  -- withMenuBar sets up the X window for the menu bar
  withXConf . withMenuBar $
    run strs (MenuConf "#000000" "#ffff77")


run :: [String] -> MenuConf -> WinConf -> IO ()
run str mc wc = do
  drawMenu mc wc str
  ev <- parseXEvent (wc^.xc.dpy) (xicGetKeySym (wc^.xic))
  case ev of
    Just Abort -> return ()
    _ -> run str mc wc

