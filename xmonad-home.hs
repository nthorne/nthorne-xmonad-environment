import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig

myManageHook = composeAll
    [ title =? "Run Application" --> doFloat  -- cause the app runner to float
    ]

myLogHook = dynamicLogWithPP $ defaultPP

main = do
  xmonad $ gnomeConfig
    { manageHook = manageDocks <+> myManageHook
		    <+> manageHook gnomeConfig
    , workspaces = myWorkspaces
    , modMask = mod4Mask
    , terminal = "xterm"
    , logHook = myLogHook
    }
    `additionalKeysP` myKeys


myKeys =
  [ ("M-S-q", spawn "gnome-session-quit")
  , ("M-S-h", spawn "gnome-session-quit --power-off")
  , ("M-z", spawn "gnome-screensaver-command --lock") 
  , ("M-p", spawn "dmenu_run")
  , ("M-S-f", spawn "firefox")
  ]

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
