{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Lib
    ( runfb
    ) where

import qualified Control.Exception as E
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import qualified Graphics.Vty as V

import qualified Data.Text as Text
import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import Brick.AttrMap (AttrName)
import Brick.Types
  ( Widget
  , BrickEvent(..)
  )
import Brick.Widgets.Center
  ( center
  , hCenter
  )
import Brick.Widgets.Border
  ( borderWithLabel
  )
import Brick.Widgets.Core
  ( vBox, (<=>), padTop, hBox
  , hLimit, vLimit, txt, hLimitPercent
  , withDefAttr, emptyWidget
  )
import qualified Brick.AttrMap as A
import Brick.Util (on, fg)
import qualified Brick.Types as T
import EventHandler (handleEvent)
import ShareLib (Name(FileBrowser1), FBState, FBState(FBState), fileBrowser)
import qualified Brick.Widgets.FileBrowser as FB


drawUI :: FBState -> [Widget Name]
drawUI state = [center $ ui <=> help]
    where
        b = fileBrowser state
        ui = hCenter $
             vLimit 20 $
             hLimitPercent 75 $
             borderWithLabel (txt "Browse files") $
             FB.renderFileBrowser True b
        help = padTop (T.Pad 1) $
               vBox [ case FB.fileBrowserException b of
                          Nothing -> emptyWidget
                          Just e -> hCenter $ withDefAttr errorAttr $
                                    txt $ Text.pack $ E.displayException e
                    , hCenter $ txt "Up/Down: select"
                    , hCenter $ txt "/: search, Ctrl-C or Esc: cancel search"
                    , hCenter $ txt "Enter: change directory or select file"
                    , hCenter $ txt "Esc: quit"
                    ]

errorAttr :: AttrName
errorAttr = "error"


theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listSelectedFocusedAttr, V.black `on` V.cyan)
    , (FB.fileBrowserSelectionInfoAttr, V.white `on` V.blue)
    , (FB.fileBrowserCurrentDirectoryAttr, V.white `on` V.blue)
    , (FB.fileBrowserBlockDeviceAttr, fg V.magenta)
    , (FB.fileBrowserDirectoryAttr, fg (V.rgbColor 0 0 255))
    , (FB.fileBrowserNamedPipeAttr, fg V.yellow)
    , (FB.fileBrowserCharacterDeviceAttr, fg V.green)
    , (FB.fileBrowserUnixSocketAttr, fg V.red)
    , (FB.fileBrowserSymbolicLinkAttr, fg V.cyan)
    , (FB.fileBrowserSelectedAttr, V.white `on` V.magenta)
    , (FB.fileBrowserAttr, V.white `on` V.blue)
    , (errorAttr, fg V.red)
    ]

theApp :: M.App FBState e Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = handleEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }


runfb :: IO ()
runfb = do
  fb <- (FB.newFileBrowser FB.selectNonDirectories FileBrowser1 Nothing)
  M.defaultMain theApp (FBState fb False)
  print "done"