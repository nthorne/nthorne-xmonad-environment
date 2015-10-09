import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace (onWorkspaces)
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.CopyWindow
import XMonad.Prompt
import XMonad.Prompt.Workspace
import XMonad.Prompt.Shell

myManageHook = composeAll
    [ title =? "Run Application" --> doFloat  -- cause the app runner to float
    ]

-- Main pallete
colLight = "#d8d8d8"
colDark = "#343d55"
colVeryDark = "#000000"
colTextLight = "#ffffff"
colTextDark = "#bbbbbb"
colBorderLight = colLight
colBorderDark = colDark

-- Same font as for a console
--myFont = "-xos4-terminus-medium-r-*-*-14-*-*-*-*-*-*-*"

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
  { bgColor     = colLight
  , fgColor     = colVeryDark
  , borderColor = colLight
  }

myDefaultLayout = avoidStruts (Grid ||| Mirror tiled ||| tiled ||| simpleTabbed) ||| Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    delta = 3/100
    ratio = 1/2

myCustomSizedLayout = avoidStruts (Mirror tiled ||| tiled ||| simpleTabbed ||| Full ||| Grid)
  where
    tiled = Tall nmaster tiledDelta tiledRatio
    nmaster = 1
    tiledDelta = 3/100
    tiledRatio = 1/5

myLayoutHook = onWorkspaces ["1", "2", "3"] myCustomSizedLayout $
  myDefaultLayout

	

myLogHook = dynamicLogWithPP $ defaultPP

main = do
  xmonad $ defaultConfig
    { manageHook = manageDocks <+> myManageHook
		    <+> manageHook defaultConfig
    , XMonad.workspaces = myWorkspaces
    , terminal = "xterm"
    , layoutHook = myLayoutHook
    , logHook = myLogHook
    }
    `additionalKeysP` myKeys
    `additionalMouseBindings` myMouse


myKeys =
  [ ("M-p", spawn "dmenu_run")
  , ("M-S-g", spawn "xterm -e ssh -t gbguxs10 'source current_project.zsh ; test -n $CURRENT_PROJECT_ROOT && cd $CURRENT_PROJECT_ROOT ; /usr/bin/zsh -l'")
  , ("M-S-l", spawn "xterm -e ssh -t interflo@gbgmullvad 'cd log/10300 ; /bin/bash -l'")
  , ("M-S-h", spawn "xterm -e ssh interflo@hermelin")
  , ("M-S-a", spawn "xterm -e 'source current_project.zsh && test -n $CURRENT_PROJECT_ROOT && cd $CURRENT_PROJECT_ROOT; /usr/bin/zsh -l'")
  , ("M-S-t", spawn "xterm -e 'source current_project.zsh && test -n $CURRENT_PROJECT_ROOT && cd $CURRENT_PROJECT_ROOT/**/TCC_SW; /usr/bin/zsh -l'")
  , ("M-S-i", spawn "xterm -e /usr/bin/zsh -l -c '~/bin/_irssi.sh'")
  , ("M-S-d", spawn "xterm -e 'cd /media/sf_MyDocuments/; /usr/bin/zsh -l'")
  , ("M-S-s", spawn "xterm -e /usr/bin/zsh -l -c 'tmux new-session -s shared'")
  , ("M-S-x", spawn "xterm -e /usr/bin/zsh -l -c 'tmux new-session -t shared -s second'")
  , ("M-S-f", spawn "firefox")
  , ("M-d", workspacePrompt myXPConfig (windows . copy))
  , ("M-S-d", killAllOtherCopies)
  , ("M-x", shellPrompt myXPConfig)
  ]

myMouse =
  [ ((shiftMask, button1), (\w -> focus w >> Flex.mouseResizeWindow w))]

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
