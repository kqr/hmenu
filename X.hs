{-# LANGUAGE TemplateHaskell,MultiWayIf #-}

module X where

import           Control.Applicative
import           Control.Lens
import           Control.Monad                 (when)
import           Data.Maybe                    (fromMaybe)
import qualified Graphics.X11.Xlib        as X
import qualified Graphics.X11.Xlib.Extras as X

import MenuConf


data XConf = XConf
  { _dpy   :: X.Display
  , _scr   :: X.ScreenNumber
  , _font  :: X.FontStruct
  , _color :: String -> IO X.Pixel
  }
$(makeLenses ''XConf)


withXConf :: (XConf -> IO ()) -> IO ()
withXConf proc = do
  dpy <- X.openDisplay ""
  fontData <- X.loadQueryFont dpy "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
  proc (XConf dpy (X.defaultScreen dpy) fontData (getColor dpy))
  X.freeFont dpy fontData


getColor :: X.Display -> String -> IO X.Pixel
getColor dpy colstr = do
  let colormap = X.defaultColormap dpy (X.defaultScreen dpy)
  (color, _) <- X.allocNamedColor dpy colormap colstr
  return (X.color_pixel color)


getFontHeight :: X.FontStruct -> X.Dimension
getFontHeight fs =
  sum [ fromIntegral (X.ascentFromFontStruct fs)
      , fromIntegral (X.descentFromFontStruct fs)]


parseXEvent :: XConf -> IO (Maybe AppEvent)
parseXEvent xc =
  X.allocaXEvent $ \evptr -> do
    X.nextEvent (xc^.dpy) evptr
    kev <- getKeySym evptr
    return $ case kev of
      Just ksym | ksym == X.xK_Escape -> Just Abort
      Nothing -> Nothing


getKeySym :: X.XEventPtr -> IO (Maybe X.KeySym)
getKeySym evptr = do
  evtype <- X.get_EventType evptr
  if evtype == X.keyPress
    then fst <$> X.lookupString (X.asKeyEvent evptr)
    else return Nothing

