{-# LANGUAGE TemplateHaskell #-}

module TextRendering where

import           Control.Arrow
import           Control.Lens
import           Control.Monad                 (join, forM_)
import qualified Graphics.X11.Xim         as X
import qualified Graphics.X11.Xlib        as X
import qualified Graphics.X11.Xlib.Extras as X

import X


data StringInfo = StringInfo
  { _content  :: String
  , _position :: X.Position
  }
$(makeLenses ''StringInfo)

data RenderInfo = RenderInfo
  { _totalheight :: X.Dimension
  , _textlines   :: [StringInfo]
  }
$(makeLenses ''RenderInfo)


renderStrings :: XConf -> X.Window -> X.GC -> RenderInfo -> IO ()
renderStrings xc win gc renderinfo =
  forM_ (renderinfo^.textlines) $ \(StringInfo str pos) ->
    renderAt 0 pos str
  where
    renderAt :: X.Position -> X.Position -> String -> IO ()
    renderAt x y =
      X.utf8DrawString (xc^.dpy) win (xc^.font) gc (fromIntegral x) (fromIntegral y)


renderInfo :: X.FontSet -> [String] -> RenderInfo
renderInfo fs strs = go strs 0 []
  where
    go :: [String] -> X.Position -> [StringInfo] -> RenderInfo
    go [] height accum = RenderInfo (fromIntegral height) accum
    go (str:strs) prev accum =
      let (offset, lineheight) = getStringOffsets fs str
          (pos, next) = nextStringPos offset lineheight prev
      in  go strs next (StringInfo str pos : accum)


nextStringPos :: X.Position -> X.Dimension -> X.Position -> (X.Position, X.Position)
nextStringPos offset lineheight prev =
  (prev + offset, prev + fromIntegral lineheight)


getStringOffsets :: X.FontSet -> String -> (X.Position, X.Dimension)
getStringOffsets fs str =
  (negate . X.rect_y &&& X.rect_height)
    (snd (X.utf8TextExtents fs str))




