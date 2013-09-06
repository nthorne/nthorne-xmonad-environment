import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace (onWorkspaces)

myManageHook = composeAll
    [ title =? "Run Application" --> doFloat  -- cause the app runner to float
    ]

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
    , workspaces = myWorkspaces
    , terminal = "xterm"
    , layoutHook = myLayoutHook
    , logHook = myLogHook
    }
    `additionalKeysP` myKeys


myKeys =
  [ ("M-p", spawn "dmenu_run")
  , ("M-S-g", spawn "xterm -ls -e ssh -t gbguxs10 'source current_project.zsh ; test -n $CURRENT_PROJECT_ROOT && cd $CURRENT_PROJECT_ROOT ; /usr/bin/zsh -l'")
  , ("M-S-l", spawn "xterm -ls -e ssh -t interflo@lemmel 'cd Users/nthorne ; /bin/bash -l'")
  , ("M-S-h", spawn "xterm -ls -e ssh interflo@hermelin")
  , ("M-S-a", spawn "xterm -ls -e 'source current_project.zsh && test -n $CURRENT_PROJECT_ROOT && cd $CURRENT_PROJECT_ROOT; /usr/bin/zsh'")
  , ("M-S-t", spawn "xterm -ls -e 'source current_project.zsh && test -n $CURRENT_PROJECT_ROOT && cd $CURRENT_PROJECT_ROOT/Implementation/TCC_SW; /usr/bin/zsh'")
  , ("M-S-i", spawn "xterm -ls -e ssh -t gbguxs04 'bin/irssi -c localhost'")
  , ("M-d", spawn "notify-send \"$(date)\"")
  , ("M-S-s", spawn "xterm -ls -e screen -S shared")
  , ("M-S-x", spawn "xterm -ls -e screen -x shared")
  ]

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
