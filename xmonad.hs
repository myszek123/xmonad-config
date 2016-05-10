-- xmonad config used by Vic Fryzel
-- Author: Vic Fryzel
-- http://github.com/vicfryzel/xmonad-config
 
-- LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, TypeSynonymInstances, MultiParamTypeClasses #-}
-- LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, MultiParamTypeClasses, ImplicitParams #-}

import System.IO
import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageDocks (ToggleStruts(ToggleStruts))
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive

import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.Circle
import XMonad.Layout.Accordion
import XMonad.Layout.Magnifier
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace

import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.Util.Themes as Theme

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.AppendFile

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Data.List (isInfixOf, isPrefixOf)

import XMonad.Actions.UpdatePointer
import XMonad.Actions.RotSlaves
import qualified XMonad.Actions.FlexibleManipulate as Flex

import XMonad.ManageHook


scratchpads = [
     NS "term" "mate-terminal --role myterm" (role =? "myterm")
         (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)),
     NS "notes" "mate-terminal --role mynotes -x vim ~/notes/notes.txt" (role =? "mynotes")
         (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)),
     NS "mytodo" "mate-terminal --role mytodo -x vim ~/notes/home/todo.txt" (role =? "mytodo")
         (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)),
     NS "worktodo" "mate-terminal --role worktodo -x vim ~/notes/work/todo.txt" (role =? "worktodo")
         (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)),
     NS "htop" "mate-terminal --role myhtop -x htop" (role =? "myhtop") defaultFloating
 ] where role = stringProperty "WM_WINDOW_ROLE"

myTabTheme = (Theme.theme Theme.xmonadTheme)

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "gnome-terminal"
 
-- Width of the window border in pixels.
--
myBorderWidth   = 1
 
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod1Mask
 
-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
myNumlockMask   = mod2Mask
 
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1:code","2:web","3:msg","4:office","5","6","7","8","9"]
 
-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"
 
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- mod-button1, Set the window to floating mode and move by dragging
    {-
     -[ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
     -}
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
 
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    -- , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    , ((modMask, button3), (\w -> focus w >> Flex.mouseWindow Flex.discrete w))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
 
------------------------------------------------------------------------
-- Layouts:
 
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myTabConfig = defaultTheme {   activeBorderColor = "#7C7C7C"
                             , activeTextColor = "#CEFFAC"
                             , activeColor = "#000000"
                             , inactiveBorderColor = "#7C7C7C"
                             , inactiveTextColor = "#EEEEEE"
                             , inactiveColor = "#000000" }

myLayout = onWorkspace "2:web" (avoidStruts $ tabbed shrinkText myTabTheme ) $ 
           onWorkspace "1:code" (avoidStruts $ tabbed shrinkText myTabTheme ||| spiral (6/7) ) $ 
           onWorkspace "4:office" (avoidStruts $ tabbed shrinkText myTabTheme||| spiral (6/7) ) $ 
           onWorkspace "3:msg" (avoidStruts $ spiral (5/6) ) $ 
           onWorkspace "9" (avoidStruts $ tabbed shrinkText myTabTheme) $ 
           avoidStruts $ tiled ||| Mirror tiled ||| tabbed shrinkText myTabTheme ||| Full ||| spiral (6/7) {-||| noBorders (fullscreenFull Full)-} 
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100
 
------------------------------------------------------------------------
-- Window rules:
 
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
{-myManageHook = composeAll-}
    {-, resource  =? "compose"        --> doFloat-}
 
myManageHook = (composeAll . concat $
            [[ className =? f --> doFloat | f <- myFloats]
            ,[ isFullscreen                  --> doFullFloat]
            ,[ className =? i --> doIgnore | i <- myIgnores ]
            ,[ resource =? i --> doIgnore | i <- myResourceIgnores ]
            ,[ className =? c --> doShift "1:code" | c <- myCode ]
            ,[ className =? c --> doShift "2:web" | c <- myBrowsers ]
            ,[ className =? c --> doShift "3:msg" | c <- myComms ]
            ,[ className =? c --> doShift "4:office" | c <- myOffice ]
            ,[ resource  =? c --> doShift "4:office" | c <- myOffice ]
            ,[ prefixTitle "libreoffice" <||> prefixTitle "LibreOffice" --> doShift "4:office" ]
            ,[ className =? c --> doShift "5" | c <- myMedia ]
            ,[ className =? c --> doShift "6" | c <- myVMs ]
            ])
    where
    prefixTitle prefix = fmap (prefix `isPrefixOf`) title
    myFloats = ["Gimp", "MPlayer", "Galculator", "Yakuake","mailterm"]
    myIgnores = [""]
    myResourceIgnores =  ["desktop_window", "Do", "kdesktop","mailterm"]

    myCode = ["Gnome-terminal","gnome-panel","Gedit", "Pgadmin3", "mate-terminal", "Mate-terminal"]
    myBrowsers = ["Firefox", "Google-chrome"]
    myComms = ["Pidgin","Thunderbird"]
    myOffice = ["OpenOffice.org 3.2","libreoffice-calc","libreoffice-writer","libreoffice-startcenter","LibreOffice 3.3","LibreOffice 3.4","soffice","Evince","mysql-workbench-bin"]
    myMedia = ["Rhythmbox","vlc"]
    myVMs = ["VirtualBox"]
 
-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
 
 
------------------------------------------------------------------------
-- Status bars and logging
 
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
 
------------------------------------------------------------------------
-- Startup hook
 
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------
{-xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar"-}
{-topStatusBar <- spawnPipe myTopStatusBar-}
  {-spawnPipe myTopStatusBar-}
  {-spawnPipe "conky -c ~/.xmonad/conky/dzen | dzen2 -p -xs 1 -ta l -e 'onstart=lower'"-}
  {-h <- spawnPipe "dzen2 -p -xs 1 -ta l -fn Verdana-12 -e 'onstart=lower'"-}
  {-xmproc <- spawnPipe "dzen2 -p -xs 1 -ta l -fn Verdana-12 -e 'onstart=lower'"-}
  {-xmproc <- spawnPipe "xmobar /home/myszka/.xmonad/xmobar.config"-}
  {-spawnPipe "/home/myszka/.xmonad/statusbar.sh"-}
  --xmonad $ ewmh $ (defaults {
    {-logHook            =  updatePointer (Relative 0.5 0.5),-}
    {-logHook            =  updatePointer (0.5, 0.5) (0, 0),-}
    {-logHook            = dynamicLogWithPP $ defaultPP { ppOutput = hPutStrLn h  } <+> updatePointer (Relative 0.5 0.5),-}
    {-logHook            = dynamicLogWithPP $ defaultPP { ppOutput = hPutStrLn h  },-}
    {-logHook            = dynamicLogWithPP dzenPP-}
    {-logHook            = dynamicLogWithPP xmobarPP-}
                              {-{ ppOutput = hPutStrLn xmproc , ppTitle = xmobarColor "green" "" . shorten 50 },-}
------------------------------------------------------------------------

main :: IO ()
main = do
  dzenLeftBar <- spawnPipe myXmonadBar
  dzenRightBar <- spawnPipe myStatusBar
  xmonad $ withUrgencyHookC dzenUrgencyHook { args = ["-bg", "red", "fg", "black", "-xs", "1", "-y", "25"] } urgencyConfig { remindWhen = Every 15 } $  (defaults {
    logHook = myLogHook dzenLeftBar >> fadeInactiveLogHook 0xdddddddd, 
    manageHook         = manageDocks <+> myManageHook <+> namedScratchpadManageHook scratchpads,
    startupHook        = setWMName "LG3D"
  }
    `additionalKeys` [ ((myModMask .|. controlMask, xK_l     ), spawn "gnome-screensaver-command  --lock")
        , ((mod4Mask, xK_h), namedScratchpadAction scratchpads "htop")
        , ((mod4Mask, xK_n), namedScratchpadAction scratchpads "notes")
        , ((mod4Mask, xK_t), namedScratchpadAction scratchpads "term")
        , ((mod4Mask, xK_d), namedScratchpadAction scratchpads "mytodo")
        , ((mod4Mask, xK_w), namedScratchpadAction scratchpads "worktodo")
        , ((mod4Mask, xK_r), rotSlavesUp)
        , ((mod4Mask, xK_p ), spawn "gnome-do")
        , ((mod4Mask .|. controlMask, xK_x), shellPrompt defaultXPConfig)
        , ((mod4Mask .|. controlMask, xK_n), appendFilePrompt defaultXPConfig "/home/myszka/xmonad.notes")
        , ((mod4Mask .|. controlMask, xK_t), sendMessage ToggleStruts)
        , ((mod4Mask .|. controlMask, xK_p), spawn "dmenu_run")
    ])

defaults = defaultConfig {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        mouseBindings      = myMouseBindings,
        layoutHook         = smartBorders $ myLayout,
        manageHook         = myManageHook ,
        startupHook        = myStartupHook
    }

myXmonadBar = "dzen2 -xs '0' -y '0' -h '14' -w '1440' -ta 'l' -fg '#FFFFFF' -bg '#1B1D1E'"
myStatusBar = "conky -c /home/myszka/.conkyrc | dzen2 -xs '2' -w '1440' -h '14' -ta 'r' -bg '#1B1D1E' -fg '#FFFFFF' -y '0'"
myBitmapsDir = "/home/myszka/.xmonad/dzen2"

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
      ppCurrent           =   dzenColor "#ebac54" "#1B1D1E" . pad
        , ppVisible           =   dzenColor "white" "#1B1D1E" . pad
        , ppHidden            =   dzenColor "white" "#1B1D1E" . pad
        , ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#1B1D1E" . pad
        , ppUrgent            =   dzenColor "black" "red" . pad
        , ppWsSep             =   " "
        , ppSep               =   "  |  "
        , ppLayout            =   dzenColor "#ebac54" "#1B1D1E" .
                                    (\x -> case x of
                                     "ResizableTall"             ->      "^i(" ++ myBitmapsDir ++ "/tall.xbm)"
                                     "Mirror ResizableTall"      ->      "^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
                                     "Full"                      ->      "^i(" ++ myBitmapsDir ++ "/full.xbm)"
                                     "Simple Float"              ->      "~"
                                     _                           ->      x
                                    )
        , ppTitle             =   (" " ++) . dzenColor "white" "#1B1D1E" . dzenEscape
        , ppOutput            =   hPutStrLn h
    }

