import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace (onWorkspaces)
import qualified XMonad.Actions.FlexibleResize as Flex

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
    `additionalMouseBindings` myMouse


myKeys =
  [ ("M-p", spawn "dmenu_run")
  , ("M-S-g", spawn "xterm -e ssh -t gbguxs10 'source current_project.zsh ; test -n $CURRENT_PROJECT_ROOT && cd $CURRENT_PROJECT_ROOT ; /usr/bin/zsh -l'")
  , ("M-S-l", spawn "xterm -e ssh -t interflo@lemmel 'cd Users/nthorne ; /bin/bash -l'")
  , ("M-S-h", spawn "xterm -e ssh interflo@hermelin")
  , ("M-S-a", spawn "xterm -e 'source current_project.zsh && test -n $CURRENT_PROJECT_ROOT && cd $CURRENT_PROJECT_ROOT; /usr/bin/zsh -l'")
  , ("M-S-t", spawn "xterm -e 'source current_project.zsh && test -n $CURRENT_PROJECT_ROOT && cd $CURRENT_PROJECT_ROOT/Implementation/TCC_SW; /usr/bin/zsh -l'")
  , ("M-S-i", spawn "xterm -e irssi -c gbguxs04 -p 6697")
  , ("M-d", spawn "xterm -e bin/dual")
  , ("M-S-s", spawn "xterm -e /usr/bin/zsh -l -c 'screen -S shared'")
  , ("M-S-x", spawn "xterm -e /usr/bin/zsh -l -c 'screen -x shared'")
  , ("M-S-f", spawn "firefox")
  ]

myMouse =
  [ ((shiftMask, button1), (\w -> focus w >> Flex.mouseResizeWindow w))]

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
