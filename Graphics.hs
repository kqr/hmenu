{-# LANGUAGE TemplateHaskell #-}

module Graphics where

import           Control.Applicative
import           Control.Lens
import           Data.Bits
import qualified Graphics.X11.Xim         as X
import qualified Graphics.X11.Xlib        as X
import qualified Graphics.X11.Xlib.Extras as X

import           X
import           TextRendering
import           MenuConf


data WinConf = WinConf
  { _xc         :: XConf
  , _window     :: X.Window
  , _xic        :: X.XIC
  , _gc         :: X.GC
  , _wwidth     :: X.Dimension
  }
$(makeLenses ''WinConf)


withMenuBar :: (WinConf -> IO ()) -> XConf -> IO ()
withMenuBar proc xc = do
  -- Get a window, input controller and a graphics context
  menuwin <- createMenuBarWindow xc (getScreenWidth xc) 1
  xinp    <- getInputController (xc^.dpy) menuwin
  newGC   <- X.createGC (xc^.dpy) menuwin

  -- Call the program with the graphics context.
  -- We're doing it inversion of control style
  -- to ensure that the GC gets free'd by us.
  proc (WinConf xc menuwin xinp newGC (getScreenWidth xc))

  X.freeGC (xc^.dpy) newGC


drawMenu :: MenuConf -> WinConf -> [String] -> IO ()
drawMenu mc wc strs = do
  X.resizeWindow (wc^.xc.dpy) (wc^.window) (wc^.wwidth) (renderinfo^.totalheight)
  X.setForeground (wc^.xc.dpy) (wc^.gc) =<< (wc^.xc.color) (mc^.bgc)
  X.fillRectangle (wc^.xc.dpy) (wc^.window) (wc^.gc)
                  0 0 (wc^.wwidth) (renderinfo^.totalheight)
  X.setForeground (wc^.xc.dpy) (wc^.gc) =<< (wc^.xc.color) (mc^.fgc)
  renderStrings (wc^.xc) (wc^.window) (wc^.gc) renderinfo
  where
    renderinfo = renderInfo (wc^.xc.font) strs



createMenuBarWindow
  :: XConf
  -> X.Dimension
  -> X.Dimension
  -> IO X.Window
createMenuBarWindow xc width height = do
  -- The root window is the future parent of our window
  rootw <- X.rootWindow (xc^.dpy) (xc^.scr)

  -- Grab keyboard and open input methods
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
                   (X.defaultDepth (xc^.dpy) (xc^.scr)) X.inputOutput
                   (X.defaultVisual (xc^.dpy) (xc^.scr))
                   (X.cWOverrideRedirect .|. X.cWEventMask) swa

  -- We want our new window on top
  X.mapRaised (xc^.dpy) menuwin
  -- Flush all events to X11 to force it to display
  -- the new window synchronously
  X.sync (xc^.dpy) False

  return menuwin
  

getInputController :: X.Display -> X.Window -> IO X.XIC
getInputController dpy win = do
  xim <- X.openIM dpy Nothing Nothing Nothing
  X.createIC xim [X.XIMPreeditNothing, X.XIMStatusNothing] win


xicGetKeySym :: X.XIC -> X.XEventPtr -> IO (Maybe X.KeySym)
xicGetKeySym xinp evptr =
  snd <$> X.utf8LookupString xinp evptr

