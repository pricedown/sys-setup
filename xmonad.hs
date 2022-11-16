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
    [ className =? "Qalculate-gtk" --> doRectFloat (RationalRect (1 / 3) (1 / 3) (1 / 3) (1 / 3)),
      className =? "SpeedCrunch" --> doRectFloat (RationalRect (1 / 3) (1 / 3) (1 / 3) (1 / 3)),
      className =? "Steam" --> doShift "3"
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
      startupHook = do spawn "autorandr --change; nitrogen --restore; brightnessctl --restore"
    }
    `additionalKeys` [ ((mod4Mask, xK_d), spawn "rofi -show run -show-icons"),
                       ((mod4Mask, xK_slash), spawn "emacs"),
                       ((mod4Mask .|. shiftMask, xK_p), spawn "flameshot screen -r --path ~/Pictures/Screenshots --clipboard"),
                       -- Extra keyboard functionality
                       ((0, xF86XK_Calculator), spawn "pgrep qalculate-gtk > /dev/null && pkill qalculate-gtk || setsid qalculate-gtk & > /dev/null"),
                       ((0, xK_Print), spawn "flameshot full --path ~/Pictures/Screenshots --clipboard"),
                       ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl s +10% --save"),
                       ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl s 10%- --save"),
                       ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2-"),
                       ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 2+"),
                       ((0, xF86XK_AudioMute), spawn "amixer set Master toggle"),
                       ((0, xF86XK_AudioPlay), spawn "playerctl play-pause")
                     ]

-- TODO Add volume, brightness keys, secondary printscreen key
