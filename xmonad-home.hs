import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Util.Run
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
myFont = "-xos4-terminus-medium-r-*-*-14-*-*-*-*-*-*-*"

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
  { font        = myFont
  , bgColor     = colLight
  , fgColor     = colVeryDark
  , borderColor = colLight
  }

main = do
  dzenLeftBar <- spawnPipe ("dzen2 -x '0' -w '770' -ta 'l' -fn " ++ myFont)
  dzenRightBar <- spawnPipe ("conky | dzen2 -x '770' -w '770' -ta 'r' -fn " ++ myFont)
  xmonad $ gnomeConfig
    { manageHook = manageDocks <+> myManageHook
		    <+> manageHook gnomeConfig
    , XMonad.workspaces = myWorkspaces
    , modMask = mod4Mask
    , terminal = "xterm"
    , logHook = dynamicLogWithPP $ defaultPP { ppOutput = hPutStrLn dzenLeftBar }
    }
    `additionalKeysP` myKeys


myKeys =
  [ ("M-S-q", spawn "pkill xmonad")
  , ("M-S-h", spawn "gksudo pm-hibernate")
  , ("M-z", spawn "gnome-screensaver-command --lock") 
  , ("M-p", spawn ("dmenu_run -fn " ++ myFont))
  , ("M-S-f", spawn "firefox")
  , ("M-d", workspacePrompt myXPConfig (windows . copy))
  , ("M-S-d", killAllOtherCopies)
  , ("M-x", shellPrompt myXPConfig)
  ]

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
