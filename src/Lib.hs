{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( runfb,
  )
where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif

import Brick (padTopBottom, str)
import Brick.AttrMap (AttrName)
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types
  ( BrickEvent (..),
    Widget,
  )
import qualified Brick.Types as T
import Brick.Util (bg, fg, on)
import Brick.Widgets.Border
  ( border,
    borderWithLabel,
  )
import Brick.Widgets.Center
  ( center,
    hCenter,
  )
import Brick.Widgets.Core
  ( emptyWidget,
    hBox,
    hLimit,
    hLimitPercent,
    padTop,
    txt,
    vBox,
    vLimit,
    withDefAttr,
    (<=>),
  )
import Brick.Widgets.Dialog as D
import Brick.Widgets.Edit as E
import qualified Brick.Widgets.FileBrowser as FB
import qualified Brick.Widgets.List as L
import qualified Control.Exception as E
import qualified Data.Text as Text
import EventHandler (handleEvent)
import qualified Graphics.Vty as V
import ShareLib (FBState (FBState, filenameEditor), Name (Editor1, FileBrowser1), creatingFile, fileBrowser)

data Choice = Red | Blue | Green
  deriving (Show)

drawUI :: FBState -> [Widget Name]
drawUI state
  | not $ creatingFile state = [center $ ui <=> help]
  | otherwise = [fdialog, center $ ui <=> help]
  where
    editor = hCenter $ hLimitPercent 80 $ renderEditor (str . unlines) True (filenameEditor state)
    d = D.dialog (Just "Enter File Name") (Just (0, [("Ok", Red)])) 50
    fdialog = D.renderDialog d (padTopBottom 1 editor)
    b = fileBrowser state
    ui =
      hCenter $
        vLimit 20 $
          hLimitPercent 75 $
            hLimit 70 $
              borderWithLabel (txt "Browse files") $ FB.renderFileBrowser True b
    help =
      padTop (T.Pad 1) $
        vBox
          [ case FB.fileBrowserException b of
              Nothing -> emptyWidget
              Just e -> hCenter $ withDefAttr errorAttr $ txt $ Text.pack $ E.displayException e,
            hCenter $ txt "Up/Down: select",
            hCenter $ txt "/: search, Ctrl-C or Esc: cancel search",
            hCenter $ txt "Enter: change directory or select file",
            hCenter $ txt "Esc: quit"
          ]

errorAttr :: AttrName
errorAttr = "error"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listSelectedFocusedAttr, V.black `on` V.cyan),
      (FB.fileBrowserSelectionInfoAttr, V.white `on` V.blue),
      (FB.fileBrowserCurrentDirectoryAttr, V.white `on` V.blue),
      (FB.fileBrowserBlockDeviceAttr, fg V.magenta),
      (FB.fileBrowserDirectoryAttr, fg (V.rgbColor 0 0 255)),
      (FB.fileBrowserNamedPipeAttr, fg V.yellow),
      (FB.fileBrowserCharacterDeviceAttr, fg V.green),
      (FB.fileBrowserUnixSocketAttr, fg V.red),
      (FB.fileBrowserSymbolicLinkAttr, fg V.cyan),
      (FB.fileBrowserSelectedAttr, V.white `on` V.magenta),
      (FB.fileBrowserAttr, V.white `on` V.blue),
      (D.dialogAttr, V.white `on` V.blue),
      (E.editAttr, bg V.cyan),
      (errorAttr, fg V.red)
    ]

theApp :: M.App FBState e Name
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = handleEvent,
      M.appStartEvent = return,
      M.appAttrMap = const theMap
    }

runfb :: IO ()
runfb = do
  fb <- FB.newFileBrowser FB.selectNonDirectories FileBrowser1 Nothing
  let initialState = FBState fb False (E.editor Editor1 (Just 1) "")
   in M.defaultMain theApp initialState
  print "done"