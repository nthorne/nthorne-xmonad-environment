import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Util.Run

myManageHook = composeAll
    [ title =? "Run Application" --> doFloat  -- cause the app runner to float
    ]


main = do
  dzenLeftBar <- spawnPipe "dzen2 -x '0' -w '770' -ta 'l'"
  dzenRightBar <- spawnPipe "conky | dzen2 -x '770' -w '770' -ta 'r'"
  xmonad $ gnomeConfig
    { manageHook = manageDocks <+> myManageHook
		    <+> manageHook gnomeConfig
    , workspaces = myWorkspaces
    , modMask = mod4Mask
    , terminal = "xterm"
    , logHook = dynamicLogWithPP $ defaultPP { ppOutput = hPutStrLn dzenLeftBar }
    }
    `additionalKeysP` myKeys


myKeys =
  [ ("M-S-q", spawn "pkill xmonad")
  , ("M-S-h", spawn "gksudo pm-hibernate")
  , ("M-S-d", spawn "notify-send -t 5000 \"$(date)\"")
  , ("M-z", spawn "gnome-screensaver-command --lock") 
  , ("M-p", spawn "dmenu_run")
  , ("M-S-f", spawn "firefox")
  ]

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
