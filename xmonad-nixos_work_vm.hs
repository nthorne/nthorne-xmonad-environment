import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicLog (dzen)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace (onWorkspaces)
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.CopyWindow
import XMonad.Prompt
import XMonad.Prompt.Workspace
import XMonad.Prompt.Shell
import XMonad.Util.Run
import XMonad.Config.Desktop

myManageHook = composeAll
    [ title =? "Run Application" --> doFloat  -- cause the app runner to float
    , manageDocks
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
myFont = "-*-terminus-medium-r-*-*-16-*-*-*-*-*-*-*"

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
  { bgColor     = colLight
  , fgColor     = colVeryDark
  , borderColor = colLight
  }

myDefaultLayout = (Grid ||| Mirror tiled ||| tiled ||| simpleTabbed) ||| Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    delta = 3/100
    ratio = 1/2

myCustomSizedLayout = (Mirror tiled ||| tiled ||| simpleTabbed ||| Full ||| Grid)
  where
    tiled = Tall nmaster tiledDelta tiledRatio
    nmaster = 1
    tiledDelta = 3/100
    tiledRatio = 1/5

myLayoutHook = onWorkspaces ["1", "2", "3"] myCustomSizedLayout $
  myDefaultLayout


main = do
  dzenLeftBar <- spawnPipe ("dzen2 -dock -x '0' -ta 'l' -fn " ++ myFont)
  xmonad $ desktopConfig
    { manageHook = myManageHook <+> manageDocks <+> manageHook desktopConfig
    , handleEventHook = docksEventHook <+> handleEventHook desktopConfig
    , terminal = "xterm"
    , XMonad.workspaces = myWorkspaces
    , layoutHook = avoidStruts $ myLayoutHook
    , logHook = dynamicLogWithPP $ defaultPP { ppOutput = hPutStrLn dzenLeftBar }
    }
    `additionalKeysP` myKeys
    `additionalMouseBindings` myMouse


myKeys =
  [ ("M-p", spawn "dmenu_run")
  , ("M-S-i", spawn "xterm -e /usr/bin/env zsh -l -c 'irssi_wrapper.sh'")
  , ("M-S-s", spawn "xterm -e /usr/bin/env zsh -l -c 'tmux new-session -s shared'")
  , ("M-S-f", spawn "firefox")
  , ("M-b", sendMessage ToggleStruts)
  , ("M-d", workspacePrompt myXPConfig (windows . copy))
  , ("M-S-d", killAllOtherCopies)
  , ("M-x", shellPrompt myXPConfig)
  ]

myMouse =
  [ ((shiftMask, button1), (\w -> focus w >> Flex.mouseResizeWindow w))]

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
