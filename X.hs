{-# LANGUAGE TemplateHaskell,MultiWayIf #-}

module X where

import           Control.Applicative
import           Control.Lens
import           Control.Monad                 (when)
import           Data.Maybe                    (fromMaybe)
import qualified Graphics.X11.Xim         as X
import qualified Graphics.X11.Xlib        as X
import qualified Graphics.X11.Xlib.Extras as X

import MenuConf


data XConf = XConf
  { _dpy   :: X.Display
  , _scr   :: X.ScreenNumber
  , _font  :: X.FontSet
  , _color :: String -> IO X.Pixel
  }
$(makeLenses ''XConf)


withXConf :: (XConf -> IO ()) -> IO ()
withXConf proc = do
  dpy <- X.openDisplay ""
  (_, _, fontData) <- X.createFontSet dpy "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
  proc (XConf dpy (X.defaultScreen dpy) fontData (getColor dpy))
  X.freeFontSet dpy fontData


getColor :: X.Display -> String -> IO X.Pixel
getColor dpy colstr = do
  let colormap = X.defaultColormap dpy (X.defaultScreen dpy)
  (color, _) <- X.allocNamedColor dpy colormap colstr
  return (X.color_pixel color)


getFontHeight :: X.FontSet -> X.Dimension
getFontHeight fs = 10   -- TODO: Obviously temporary...


parseXEvent
  :: X.Display
  -> (X.XEventPtr -> IO (Maybe X.KeySym))
  -> IO (Maybe AppEvent)
parseXEvent dpy xicGetKeySym =
  X.allocaXEvent $ \evptr -> do
    X.nextEvent dpy evptr
    kev <- xicGetKeySym evptr
    return $ case kev of
      Just ksym | ksym == X.xK_Escape -> Just Abort
      _ -> Nothing



