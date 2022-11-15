import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.StackSet
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab

myLayout = smartBorders tiled ||| Mirror tiled ||| fullscreenFocus Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 59 / 100 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

myManageHook =
  composeAll
    [ className =? "SpeedCrunch" --> doRectFloat (RationalRect (1 / 3) (1 / 3) (1 / 3) (1 / 3))
    ]

main :: IO ()
main = xmonad $ fullscreenSupportBorder $ ewmhFullscreen $ ewmh $ xmobarProp $ myConfig

myConfig =
  def
    { modMask = mod4Mask, -- Mod is Super
      handleEventHook = XMonad.Layout.Fullscreen.fullscreenEventHook,
      manageHook = fullscreenManageHook <+> myManageHook,
      layoutHook = myLayout,
      terminal = "alacritty",
      borderWidth = 2,
      normalBorderColor = "#504945",
      focusedBorderColor = "#458588",
      focusFollowsMouse = True,
      startupHook = do spawn "autorandr --change; nitrogen --restore"
    }
    `additionalKeys` [ ((mod4Mask, xK_d), spawn "rofi -show run -show-icons"),
                       ((mod4Mask, xK_slash), spawn "emacs"),
                       ((0, xF86XK_Calculator), spawn "pgrep speedcrunch > /dev/null && pkill speedcrunch || setsid speedcrunch & > /dev/null")
                     ]

-- `additionalKeysP` [ ("M-/", spawn "emacs"),
--                     ("M-d", spawn "rofi -show run -show-icons"),
--                     -- ("M-c", spawn "speedcrunch")
--                   ]

-- TODO Add volume, brightness keys.
