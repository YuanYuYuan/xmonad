import XMonad hiding ( (|||) )
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.MouseResizableTile
import XMonad.Actions.FloatKeys
import XMonad.Layout.WindowArranger
import Data.Monoid
import Data.Char
import Data.Ratio ((%))
import Data.Maybe (isJust)
import System.Exit
import XMonad.Util.NamedScratchpad
import XMonad.Actions.CycleWS
import XMonad.Actions.GroupNavigation
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.WorkspaceCompare
import XMonad.Actions.MouseResize
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Actions.DwmPromote

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: [Char]
myTerminal = "alacritty"

-- Width of the window border in pixels.
--
myBorderWidth :: Dimension
myBorderWidth = 2
myModMask :: KeyMask
myModMask = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#1E90FF"

-- { Custom Scripts } {{{
myScriptDir :: [Char]
myScriptDir = "$HOME/Workings/scripts/"

myVolumeCtrl :: MonadIO m => [Char] -> m ()
myVolumeCtrl arg = spawn $ myScriptDir ++ "control-volume.sh " ++ arg

myBrightnessCtrl :: MonadIO m => [Char] -> m ()
myBrightnessCtrl arg = spawn $ myScriptDir ++ "control-brightness.sh " ++ arg

myMonitorCtrl :: MonadIO m => [Char] -> m ()
myMonitorCtrl arg = spawn $ myScriptDir ++ "control-monitors.sh " ++ arg
-- }}}

-- { Keybindings } {{{
myKeys :: [([Char], X())]
myKeys =
    -- { Basics } {{{
    [ ("M-<Return>" , spawn "alacritty --working-directory $(xcwd)")
    , ("`"          , namedScratchpadAction myScratchPads "terminal")
    , ("M1-<Space>" , spawn "$HOME/.config/rofi/j4-dmenu-script.sh")
    , ("<Print>"    , spawn "$HOME/Workings/scripts/screenshot.sh")
    , ("M-q"        , kill)
    , ("M-<Space>"  , sendMessage NextLayout)
    , ("M-b"        , sendMessage ToggleStruts)
    , ("M-S-q"      , io exitSuccess)
    , ("M-S-r"        , spawn "xmonad --recompile; xmonad --restart")
    , ("M1-<F1>"    , sendMessage $ JumpToLayout "vtile")
    , ("M1-<F2>"    , sendMessage $ JumpToLayout "tabbed")
    , ("M1-<F3>"    , sendMessage $ JumpToLayout "htile")
    , ("M1-<F4>"    , sendMessage $ JumpToLayout "full")
    , ("<Pause>"    , spawn "systemctl suspend")
    ] ++
    -- }}}

    -- { Screen } {{{
    [ ("M-S-d"      , nextScreen)
    , ("M-S-a"      , prevScreen)
    , ("M-o"        , swapNextScreen)
    ] ++
    -- }}}

    -- { Window } {{{

    -- focus
    [ ("M-j"           , windows W.focusDown)
    , ("M-<Page_Down>" , windows W.focusDown)
    , ("M-k"           , windows W.focusUp)
    , ("M-<Page_Up>"   , windows W.focusUp)
    , ("M-e"           , windows W.focusMaster)
    , ("M1-<Tab>"      , nextMatch History (return True))

    -- swap
    , ("M-S-j"         , windows W.swapDown)
    , ("M-S-k"         , windows W.swapUp)
    , ("M-m"           , dwmpromote)
    , ("M-S-m"         , windows W.swapMaster)

    -- resize
    , ("M-h"           , sendMessage Shrink)
    , ("M-l"           , sendMessage Expand)
    , ("M-S-h"         , sendMessage MirrorExpand)
    , ("M-S-l"         , sendMessage MirrorShrink)
    , ("M-<L>"         , sendMessage Shrink)
    , ("M-<R>"         , sendMessage Expand)
    , ("M-<U>"         , sendMessage MirrorExpand)
    , ("M-<D>"         , sendMessage MirrorShrink)

    , ("M-f"           , withFocused $ keysResizeWindow (-20, -20) (1%2, 1%2))
    , ("M-t"           , withFocused $ windows . W.sink)
    , ("M-,"           , sendMessage (IncMasterN 1))
    , ("M-."           , sendMessage (IncMasterN (-1)))
    ] ++

    -- }}}

    -- { Workspace } {{{

    -- navigation
    [ ("M-d"      , moveTo Next hiddenNonEmptyNonNSP)
    , ("M-a"      , moveTo Prev hiddenNonEmptyNonNSP)
    , ("M-g"      , sendMessage ToggleGaps)
    , ("M-<Tab>"  , toggleWS' ["NSP"])
    ] ++

    -- }}}

    -- { Control } {{{

    -- volume control
    [ ("M-" ++ k, myVolumeCtrl a)
        | (k, a) <-
        [ ("="           , "up")
        , ("-"           , "down")
        , ("<Backspace>" , "mute")
        ]
    ] ++

    -- brightness
    [ ("M-" ++ k, myBrightnessCtrl a)
        | (k, a) <-
        [ ("0", "up")
        , ("9", "down")
        ]
    ] ++

    -- display
    [ ("M-p " ++ k, myMonitorCtrl a)
        | (k, a) <-
        [ ("m", "mirror")
        , ("n", "normal")
        , ("a", "arandr")
        ]
    ]

    -- }}}

    where
        -- getSortByIndexNoSP = fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex
        -- nonNSP             = WSIs (return (\ws -> W.tag ws /= "nsp"))
        -- nonEmptyNonNSP     = WSIs $ return $ \ws -> isJust (W.stack ws) && W.tag ws /= "nsp"
        hiddenNonEmptyNonNSP = WSIs $
            do hiddenWS <- gets (map W.tag . W.hidden . windowset)
               return $ \ws -> W.tag ws `elem` hiddenWS
                            && isJust (W.stack ws)
                            && W.tag ws /= "nsp"
-- }}}

-- { Spacing } {{{
mySpacing :: l a -> ModifiedLayout Spacing l a
mySpacing = spacingRaw
    True                -- Only for >1 window
    (Border 0 15 10 10) -- Size of screen edge gaps
    True                -- Enable screen edge gaps
    (Border 5 5 5 5)    -- Size of window gaps
    True                -- Enable window gaps
-- }}}

-- { Layout } {{{
myLayout = avoidStruts $ mouseResize $ windowArrange $ mySpacing $ smartBorders
        myVTiledLayout
    ||| myTabbedLayout
    ||| myHTiledLayout
    ||| myFullLayout

    where
        myVTiledLayout = renamed [Replace "vtile"]
            $ ResizableTall
                1        -- number of master panes
                (3/100)  -- % of screen to increment by when resizing
                (1/2)    -- ratio of the master pane
                []

        myHTiledLayout = renamed [Replace "htile"]
            $ Mirror myVTiledLayout

        myTabbedLayout = renamed [Replace "tabbed"]
            $ tabbed shrinkText tabConfig
                where tabConfig = def { fontName = "xft:Monospace-12" }

        myFullLayout = renamed [Replace "full"] Full
-- }}}

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
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "mpv"            --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , isFullscreen                  --> doFullFloat
    ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = historyHook

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spwanTerm findTerm manageTerm ]
    where
        spwanTerm = myTerminal ++ " --class scratchpad"
        findTerm = resource =? "scratchpad"
        manageTerm = customFloating $ W.RationalRect l t w h
            where
                h = 1.0
                w = 1.0
                t = 0.0
                l = 0.0

-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main :: IO()
main = xmonad $ docks (additionalKeysP defaults myKeys)

defaults = def {
  -- simple stuff
    terminal           = myTerminal,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

  -- key bindings
    -- keys               = myKeys,
    -- mouseBindings      = myMouseBindings,

  -- hooks, layouts
    -- layoutHook         = myLayout,
    layoutHook         = myLayout,
    manageHook         = myManageHook <+> namedScratchpadManageHook myScratchPads,
    handleEventHook    = myEventHook,
    logHook            = myLogHook,
    startupHook        = myStartupHook
}
