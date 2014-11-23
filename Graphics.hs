{-# LANGUAGE TemplateHaskell #-}

module Graphics where

import           Control.Lens
import           Data.Bits
import qualified Graphics.X11.Xlib        as X
import qualified Graphics.X11.Xlib.Extras as X

import           X
import           MenuConf


data WinConf = WinConf
  { _xc         :: XConf
  , _window     :: X.Window
  , _gc         :: X.GC
  , _wwidth     :: X.Dimension
  , _lineheight :: X.Dimension
  , _linecount  :: X.Dimension
  }
$(makeLenses ''WinConf)


withMenuBar :: (WinConf -> IO ()) -> XConf -> IO ()
withMenuBar proc xc = do
  -- Get a window and a graphics context
  menuwin <- createMenuBarWindow xc width (lineh * lc)
  newGC <- X.createGC (xc^.dpy) menuwin

  -- Call the program with the graphics context.
  -- We're doing it inversion of control style
  -- to ensure that the GC gets free'd by us.
  proc (WinConf xc menuwin newGC width lineh lc)

  X.freeGC (xc^.dpy) newGC
  where
    width  = fromIntegral (X.displayWidth (xc^.dpy) (xc^.scr))
    lineh = getFontHeight (xc^.font)
    lc = 5


drawMenu :: MenuConf -> WinConf -> IO ()
drawMenu mc wc = do
  X.setForeground (wc^.xc.dpy) (wc^.gc) =<< (wc^.xc.color) (mc^.bgc)
  X.fillRectangle (wc^.xc.dpy) (wc^.window) (wc^.gc)
                  0 0 (wc^.wwidth) (wc^.linecount * wc^.lineheight)
  X.setForeground (wc^.xc.dpy) (wc^.gc) =<< (wc^.xc.color) (mc^.fgc)
  X.setBackground (wc^.xc.dpy) (wc^.gc) =<< (wc^.xc.color) "#ffffff"
  drawString wc "Hello, world!"


drawString :: WinConf -> String -> IO ()
drawString wc str = do
  X.drawImageString (wc^.xc.dpy) (wc^.window) (wc^.gc) 0 10 str


createMenuBarWindow
  :: XConf
  -> X.Dimension
  -> X.Dimension
  -> IO X.Window
createMenuBarWindow xc width height = do
  -- The root window is the future parent of our window
  rootw <- X.rootWindow (xc^.dpy) (xc^.scr)
  X.grabKeyboard (xc^.dpy) rootw True X.grabModeAsync X.grabModeAsync X.currentTime

  -- We need to set some window attributes, so we wrap
  -- in an allocation function that deals with the low-
  -- level crap.
  menuwin <- X.allocaSetWindowAttributes $ \swa -> do

    -- The attributes we set
    X.set_override_redirect swa $ True
    X.set_event_mask        swa $ X.exposureMask .|. X.keyPressMask .|. X.visibilityChangeMask

    -- Create the window. Remember to set flags like
    -- CWEventMask if you have set the corresponding
    -- attributes.
    X.createWindow (xc^.dpy) rootw 0 0 width height 0
                   (X.defaultDepth (xc^.dpy) (xc^.scr)) X.inputOutput (X.defaultVisual (xc^.dpy) (xc^.scr))
                   (X.cWOverrideRedirect .|. X.cWEventMask) swa

  -- We want our new window on top
  X.mapRaised (xc^.dpy) menuwin
  -- Flush all events to X11 to force it to display
  -- the new window synchronously
  X.sync (xc^.dpy) False

  return menuwin
  

