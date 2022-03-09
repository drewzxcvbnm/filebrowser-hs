module ShareLib where

import Brick.Widgets.Edit (Editor)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.FileBrowser as FB

data Name
  = FileBrowser1
  | Editor1
  deriving (Eq, Show, Ord)

data FBState = FBState
  { fileBrowser :: FB.FileBrowser Name,
    creatingFile :: Bool,
    filenameEditor :: E.Editor String Name
  }