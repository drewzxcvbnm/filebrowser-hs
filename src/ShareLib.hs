module ShareLib
where

import qualified Brick.Widgets.FileBrowser as FB
data Name = FileBrowser1
          deriving (Eq, Show, Ord)

data FBState = FBState {
    fileBrowser :: FB.FileBrowser Name,
    creatingFile :: Bool
}