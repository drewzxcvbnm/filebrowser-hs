module EventHandler
where


import System.Process
import Control.Monad.IO.Class (liftIO)
import qualified Brick.Main as M
import qualified Brick.Widgets.FileBrowser as FB
import qualified Graphics.Vty as V
import Brick.Types (BrickEvent(..))
import qualified Brick.Types as T
import Graphics.Vty
import ShareLib (Name, FBState, FBState(FBState), fileBrowser, creatingFile)
import Data.Functor ((<&>))

handleEvent :: FBState -> BrickEvent Name e -> T.EventM Name (T.Next (FBState))
handleBrowserEvent :: FBState -> BrickEvent Name e -> T.EventM Name (T.Next (FBState))
handleDialogEvent :: FBState -> BrickEvent Name e -> T.EventM Name (T.Next (FBState))

handleEvent st e
    -- | creatingFile st = handleDialogEvent st e
    | otherwise = handleBrowserEvent st e

openFile fi = createProcess ((shell $ "xdg-open " <> FB.fileInfoFilePath fi <> " >> /dev/null") {std_err=NoStream})

shouldOpen fi = case FB.fileInfoFileType fi of
    Nothing -> False
    Just FB.RegularFile -> True
    Just _ -> False

handleBrowserEvent state (VtyEvent (V.EvKey V.KEnter [])) = do
    case FB.fileBrowserCursor b of
        Just fi ->  if shouldOpen fi then liftIO (openFile fi) >> M.continue (stateBuilder b)
                    else M.continue . stateBuilder =<< FB.handleFileBrowserEvent (V.EvKey V.KEnter []) b
        Nothing -> M.continue $ stateBuilder b
    where
        b = fileBrowser state
        areCreatingFile = creatingFile state
        stateBuilder = \br -> state{fileBrowser=br}

handleBrowserEvent b (VtyEvent (V.EvKey V.KEsc [])) = M.halt b

handleBrowserEvent state (VtyEvent (V.EvKey (V.KChar 'n') [V.MCtrl])) =  M.continue state{creatingFile=True}

handleBrowserEvent state (VtyEvent ev) = do
    b' <- FB.handleFileBrowserEvent ev b
    case FB.fileBrowserSelection b' of
        [] -> M.continue state{fileBrowser=b'}
        _ -> M.halt state{fileBrowser=b'}
    where
        b = fileBrowser state
        x = creatingFile state


handleBrowserEvent b _ = M.continue b

handleDialogEvent b _ = M.continue b