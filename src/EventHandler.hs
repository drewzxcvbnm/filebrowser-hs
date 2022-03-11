module EventHandler where

import qualified Brick.Main as M
import Brick.Types (BrickEvent (..))
import qualified Brick.Types as T
import Brick.Widgets.Edit (applyEdit)
import qualified Brick.Widgets.Edit as E
import Brick.Widgets.FileBrowser (FileInfo (fileInfoFilePath))
import qualified Brick.Widgets.FileBrowser as FB
import Control.Exception (throw)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Data.Text.Zipper (clearZipper)
import Debug.Trace (trace)
import GHC.IO.Handle (Handle)
import Graphics.Vty
import qualified Graphics.Vty as V
import ShareLib (FBState (FBState, filenameEditor), Name, creatingFile, fileBrowser)
import System.Directory (removePathForcibly)
import System.IO (IOMode (ReadMode, ReadWriteMode, WriteMode), openFile)
import System.Process

handleEvent :: FBState -> BrickEvent Name e -> T.EventM Name (T.Next FBState)
handleBrowserEvent :: FBState -> BrickEvent Name e -> T.EventM Name (T.Next FBState)
handleDialogEvent :: FBState -> BrickEvent Name e -> T.EventM Name (T.Next FBState)
handleEvent st e
  | creatingFile st = handleDialogEvent st e
  | otherwise = handleBrowserEvent st e

openFileForBrowser fi = createProcess ((shell $ "xdg-open " <> FB.fileInfoFilePath fi <> " >> /dev/null") {std_err = NoStream})

createFile :: String -> IO Handle
createFile filename = openFile filename ReadWriteMode

deleteFile :: FilePath -> IO ()
deleteFile = removePathForcibly

shouldOpen fi = case FB.fileInfoFileType fi of
  Just FB.RegularFile -> True
  _ -> False

handleBrowserEvent s (VtyEvent (V.EvKey V.KDel [])) = do
  liftIO $ (deleteFile . fileInfoFilePath . fromJust . FB.fileBrowserCursor) fb
  fb <- liftIO $ FB.setWorkingDirectory (FB.getWorkingDirectory fb) fb
  M.continue s {fileBrowser = fb}
  where
    fb = fileBrowser s
handleBrowserEvent state (VtyEvent (V.EvKey V.KEnter [])) = do
  case FB.fileBrowserCursor b of
    Just fi ->
      if shouldOpen fi
        then liftIO (openFileForBrowser fi) >> M.continue (stateBuilder b)
        else M.continue . stateBuilder =<< FB.handleFileBrowserEvent (V.EvKey V.KEnter []) b
    Nothing -> M.continue $ stateBuilder b
  where
    b = fileBrowser state
    areCreatingFile = creatingFile state
    stateBuilder = \br -> state {fileBrowser = br}
handleBrowserEvent b (VtyEvent (V.EvKey V.KEsc [])) = M.halt b
handleBrowserEvent state (VtyEvent (V.EvKey (V.KChar 'n') [V.MCtrl])) = M.continue state {creatingFile = True}
handleBrowserEvent state (VtyEvent ev) = do
  b' <- FB.handleFileBrowserEvent ev b
  case FB.fileBrowserSelection b' of
    [] -> M.continue state {fileBrowser = b'}
    _ -> M.halt state {fileBrowser = b'}
  where
    b = fileBrowser state
    x = creatingFile state
handleBrowserEvent b _ = M.continue b

handleDialogEvent s (VtyEvent (V.EvKey V.KEnter [])) = do
  liftIO (createFile $ head $ E.getEditContents (filenameEditor s))
  fb <- liftIO $ FB.setWorkingDirectory (FB.getWorkingDirectory fb) fb
  M.continue s {creatingFile = False, fileBrowser = fb, filenameEditor = ed}
  where
    fb = fileBrowser s
    ed = applyEdit clearZipper (filenameEditor s)
handleDialogEvent s (VtyEvent e) = do
  ed <- E.handleEditorEvent e (filenameEditor s)
  M.continue s {filenameEditor = ed}
  where
    fb = fileBrowser s
handleDialogEvent b _ = M.continue b
