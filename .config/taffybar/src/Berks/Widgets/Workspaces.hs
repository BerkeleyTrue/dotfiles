module Berks.Widgets.Workspaces
  ( workspacesWidget,
  )
where

import Data.Default (Default (def))
import GI.Gtk (Widget)
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Widget
  ( Workspace (..),
    WorkspaceState (..),
    WorkspacesConfig (..),
    workspacesNew,
  )

filterHiddenAndNSP :: Workspace -> Bool
filterHiddenAndNSP Workspace {workspaceState = Empty} = False
filterHiddenAndNSP Workspace {workspaceName = "NSP"} = False
filterHiddenAndNSP _ = True

workspaceConfig :: WorkspacesConfig
workspaceConfig =
  def {minIcons = 1, widgetGap = 1, showWorkspaceFn = filterHiddenAndNSP}

workspacesWidget :: TaffyIO Widget
workspacesWidget = workspacesNew workspaceConfig
