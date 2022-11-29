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

-- Mod is super
myModMask = mod4Mask

-- Layouts
myLayout = smartBorders tiled ||| Mirror tiled ||| fullscreenFocus Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 59 / 100 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

-- Window rules
myManageHook =
  composeAll
    [ className =? "Qalculate-gtk" --> doRectFloat (RationalRect (1 / 3) (1 / 3) (1 / 3) (1 / 3)),
      className =? "Steam" --> doShift "3",
      className =? "Lutris" --> doShift "3",
      -- className =? "Navigator" --> doShift "2",
      className =? "Discord" --> doShift "2"
    ]

main :: IO ()
main = xmonad $ fullscreenSupportBorder $ ewmhFullscreen $ ewmh $ xmobarProp $ myConfig

myConfig =
  def
    { modMask = myModMask,
      handleEventHook = XMonad.Layout.Fullscreen.fullscreenEventHook,
      manageHook = fullscreenManageHook <+> myManageHook,
      layoutHook = myLayout,
      logHook = dynamicLog,
      terminal = "alacritty",
      borderWidth = 2,
      normalBorderColor = "#504945",
      focusedBorderColor = "#458588",
      focusFollowsMouse = True,
      startupHook = do spawn "autorandr --change; nitrogen --restore; brightnessctl --restore"
    }
    `additionalKeys` [ ((myModMask, xK_d), spawn "rofi -show run -show-icons"), -- Mod d => Run program
                       ((myModMask, xK_slash), spawn "emacs"), -- Mod / => Open editor
                       ((myModMask .|. shiftMask, xK_p), spawn "flameshot screen -r --path ~/Pictures/Screenshots --clipboard"), -- Mod Shift P => Screenshot desktop
                       -- Large keyboard functionality
                       ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"),
                       ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"),
                       ((0, xF86XK_AudioPlay), spawn "playerctl play-pause"),
                       ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"),
                       ((0, xF86XK_Calculator), spawn "pgrep qalculate-gtk > /dev/null && pkill qalculate-gtk || setsid qalculate-gtk & > /dev/null"),
                       ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl s 10%- --save"),
                       ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl s +10% --save"),
                       ((0, xK_Print), spawn "flameshot full --path ~/Pictures/Screenshots --clipboard")
                     ]
