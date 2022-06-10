import System.Exit

import XMonad
import XMonad.Actions.CopyWindow
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicLog (dzen)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace (onWorkspaces)
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Workspace
import XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run


myManageHook = composeAll
    [ title =? "Run Application" --> doFloat  -- cause the app runner to float
    , className =? "Thunderbird" --> doShift "5:mail"
    , className =? "Slack"       --> doShift "6:slack"
    , className =? "Microsoft Teams - Preview" --> doShift "7:teams"
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

myFontSize = 24
myFont = "-*-terminus-medium-r-*-*-"++(show myFontSize) ++"-*-*-*-*-*-*-*"
myXFTFont = "Terminus-"++(show myFontSize)

myXPConfig :: XPConfig
myXPConfig = def
  { bgColor     = colLight
  , fgColor     = colVeryDark
  , borderColor = colLight
  , font        = "xft:" ++ myXFTFont
  , height      = myFontSize + (myFontSize `div` 3)
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


myStatsBarWidth = 800

main = do
  dzenStatsBar <- spawnPipe ("conky | dzen2 -x '0' -w '"++(show myStatsBarWidth)++"' -ta 'l' -fn " ++ myFont)
  dzenDesktopsBar <- spawnPipe ("dzen2 -dock -x '"++(show myStatsBarWidth)++"' -ta 'l' -fn " ++ myFont)
  xmonad $ desktopConfig
    { manageHook = myManageHook <+> manageDocks <+> manageHook desktopConfig
    , handleEventHook = docksEventHook <+> handleEventHook desktopConfig
    , terminal = "kitty"
    -- , terminal = "xterm"
    , XMonad.workspaces = myWorkspaces
    , layoutHook = avoidStruts $ myLayoutHook
    , logHook = dynamicLogWithPP $ def { ppOutput = hPutStrLn dzenDesktopsBar }
    }
    `additionalKeysP` myKeys
    `additionalMouseBindings` myMouse

myKeys =
  [ ("M-p", safeSpawn "dmenu_run" ["-fn", myXFTFont])
  , ("M-b", sendMessage ToggleStruts)
  , ("M-d", workspacePrompt myXPConfig (windows . copy))
  , ("M-S-d", killAllOtherCopies)
  , ("M-x", shellPrompt myXPConfig)
  , ("M-S-f", spawn "firefox")
  , ("M-S-l", spawn "sudo slock")
  , ("M-S-s", spawn "nvidia-offload slack")
  , ("M-S-t", spawn "nvidia-offload teams")
  , ("<XF86AudioRaiseVolume>",  spawn "amixer sset Master 3%+")
  , ("<XF86AudioLowerVolume>",  spawn "amixer sset Master 3%-")
  , ("<XF86AudioMute>",         spawn "amixer sset Master toggle")
  , ("<XF86MonBrightnessUp>",   safeSpawn "xbacklight" ["-inc", "10"])
  , ("<XF86MonBrightnessDown>", safeSpawn "xbacklight" ["-dec","10"])
  , ("M-S-q", io (exitWith ExitSuccess))
  , ("M-q", restart "xmonad" True)
  ]
  ++
  [ (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
       | (key, scr)  <- zip "wer" [2,1,0] -- was [0..] *** change to match your screen order ***
       , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]
  ]

myMouse =
  [ ((shiftMask, button1), (\w -> XMonad.focus w >> Flex.mouseResizeWindow w))]

myWorkspaces = ["1", "2", "3", "4", "5:mail", "6:slack", "7:teams", "8:web", "9:web"]
