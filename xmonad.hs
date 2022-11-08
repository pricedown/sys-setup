import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab

myLayout = smartBorders tiled ||| Mirror tiled ||| fullscreenFocus Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

main :: IO ()
main = xmonad $ fullscreenSupportBorder $ ewmhFullscreen $ ewmh $ xmobarProp $ myConfig

myConfig =
  def
    { modMask = mod4Mask, -- Mod is Super
      handleEventHook = XMonad.Layout.Fullscreen.fullscreenEventHook,
      manageHook = fullscreenManageHook,
      layoutHook = myLayout,
      terminal = "alacritty",
      borderWidth = 2,
      normalBorderColor = "#504945",
      focusedBorderColor = "#458588",
      focusFollowsMouse = True
    }
    `additionalKeysP` [ ("M-/", spawn "emacs"),
                        ("M-d", spawn "rofi -show run -show-icons")
                      ]
