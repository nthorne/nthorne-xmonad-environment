import XMonad
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings)
import XMonad.Hooks.DynamicLog (ppOutput, dynamicLogWithPP, defaultPP)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace (onWorkspaces)
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.CopyWindow (copy, killAllOtherCopies)
import XMonad.Prompt
import XMonad.Prompt.Workspace
import XMonad.Prompt.Shell
import XMonad.Util.Run


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
myFont = "-*-terminus-medium-r-*-*-16-*-*-*-*-*-*-*"


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


myKeys =
  [ ("M-p", spawn "dmenu_run")
  , ("M-S-f", spawn "firefox")
  , ("M-b", sendMessage ToggleStruts)
  , ("M-d", workspacePrompt myXPConfig (windows . copy))
  , ("M-S-d", killAllOtherCopies)
  , ("M-x", shellPrompt myXPConfig)
  ]

myMouse =
  [ ((shiftMask, button1), (\w -> focus w >> Flex.mouseResizeWindow w))]


myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]


main = do
  -- dzenLeftBar <- spawnPipe ("dzen2 -dock -x '0' -ta 'l' -fn " ++ myFont)
  dzenLeftBar <- spawnPipe ("dzen2 -dock -x '0' -w '600' -ta 'l' -fn " ++ myFont)
  dzenRightBar <- spawnPipe ("conky | dzen2 -dock -x '600' -w '766' -ta 'r' -fn " ++ myFont)
  xmonad $ defaultConfig
    { manageHook = manageDocks <+> myManageHook
		     -- <+> manageHook defaultConfig
    , XMonad.workspaces = myWorkspaces
    , terminal = "kitty"
    , layoutHook = myLayoutHook
    , logHook = dynamicLogWithPP $ defaultPP { ppOutput = hPutStrLn dzenLeftBar }
    , handleEventHook = docksEventHook
    }
    `additionalKeysP` myKeys
    `additionalMouseBindings` myMouse
