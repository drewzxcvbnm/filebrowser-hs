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

handleEvent :: FBState -> BrickEvent Name e -> T.EventM Name (T.Next (FBState))

openFile fi = createProcess ((shell $ "xdg-open " <> (FB.fileInfoFilePath fi) <> " >> /dev/null") {std_err=NoStream})

shouldOpen fi = case FB.fileInfoFileType fi of
    Nothing -> False
    Just FB.RegularFile -> True
    Just _ -> False

handleEvent state (VtyEvent (V.EvKey V.KEnter [])) = do
    case FB.fileBrowserCursor b of
        Just fi ->  if (shouldOpen fi) then liftIO (openFile fi) >> M.continue (stateBuilder b)
                    else M.continue =<< ( (FB.handleFileBrowserEvent (V.EvKey V.KEnter []) b) >>= return.stateBuilder )
        Nothing -> M.continue $ stateBuilder b 
    where 
        b = fileBrowser state
        areCreatingFile = creatingFile state
        stateBuilder = (\br -> FBState br areCreatingFile)

handleEvent b (VtyEvent (V.EvKey V.KEsc [])) = M.halt b

handleEvent state (VtyEvent ev) = do
    b' <- FB.handleFileBrowserEvent ev b
    case FB.fileBrowserSelection b' of
        [] -> M.continue (FBState b' x)
        _ -> M.halt (FBState b' x)
    where 
        b = fileBrowser state
        x = creatingFile state
    

handleEvent b _ = M.continue b
