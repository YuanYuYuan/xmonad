-- { Imports } {{{
-- import Data.Char
-- import Data.Monoid
-- import XMonad.Layout.Fullscreen
-- import XMonad.Layout.Gaps
-- import XMonad.Layout.MouseResizableTile
-- import XMonad.Util.WorkspaceCompare
-- import qualified Data.Map        as M
import qualified Data.Map.Strict             as Map
import XMonad.Hooks.FadeWindows
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace)
import XMonad.Layout.TrackFloating
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Combo
import XMonad.Layout.TwoPane
import Control.Monad ( liftM2, unless, when )
import Data.Maybe ( isJust )
import Data.Ratio ( (%) )
import Graphics.X11.ExtraTypes
import System.Exit
import XMonad hiding ( (|||) )
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.FloatKeys
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.UrgencyHook
import XMonad.Actions.GroupNavigation
  ( historyHook
  , nextMatch
  , Direction ( History )
  )
import XMonad.Actions.MouseResize
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowArranger
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Paste (sendKey)
import XMonad.Hooks.RefocusLast
import qualified XMonad.StackSet as W
import qualified Data.Map as M
-- }}}

myTerminal :: [Char]
myTerminal = "alacritty"

-- { Workspaces } {{{
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces = [ "\xf0ac"        -- 1: globe
               , "\xf121"        -- 2: code
               , "\xf04b"        -- 3: play
               , "\xf075"        -- 4: comment
               , "\xf02d"        -- 5: book
               , "\xf0e4"        -- 6: dashboard
               ]
-- }}}


-- { Keybindings } {{{
myKeys :: [([Char], X ())]
myKeys =
  -- { Basics } {{{
  -- [ ("M-<Return>" , spawn "alacritty --working-directory $(xcwd)")
  [ ("M-<Return>" , spawn "alacritty")
  , ("`"          , namedScratchpadAction myScratchPads "terminal")
  , ("M-e"        , namedScratchpadAction myScratchPads "neovide")
  , ("M-r"        , namedScratchpadAction myScratchPads "ranger")
  , ("M1-<Space>" , spawn "$HOME/.config/rofi/bin/launcher_misc")
  , ("<Print>"    , spawn "$HOME/Workings/scripts/screenshot.sh")
  , ("M-q"        , kill)
  , ("M-<Space>"  , sendMessage NextLayout)
  -- , ("M-<Space>"  , sendMessage NextLayout >> (dynamicLogString def >>= \d->spawn $ "xmessage "++d))
  , ("M-S-b"      , spawn "polybar-msg cmd toggle")
  , ("M-b"        , sendMessage ToggleStruts)
  , ("M-S-q"      , io exitSuccess)
  , ("M-S-r"      , spawn "xmonad --recompile; xmonad --restart")
  , ("<Pause>"    , spawn "systemctl suspend")
  ] ++
  -- }}}

  -- { Layout } {{{
  [ ("M1-<F1>"    , sendMessage $ JumpToLayout "vtile")
  , ("M1-<F2>"    , sendMessage $ JumpToLayout "tabbed")
  , ("M1-<F3>"    , sendMessage $ JumpToLayout "dualTab")
  , ("M1-<F4>"    , sendMessage $ JumpToLayout "htile")
  , ("M1-<F5>"    , sendMessage $ JumpToLayout "full")
  ] ++
  -- }}}

  -- { Screen } {{{
  [ ("M-S-d"      , nextScreen)
  , ("M-S-a"      , prevScreen)
  , ("M-i"        , swapNextScreen)
  ] ++
  -- }}}

  -- { Window } {{{

  -- TODO: simplify the codes
  -- Focus
  [ ("M-j"           , windows W.focusDown)
  , ("M-s"           , windows W.focusDown)
  , ("M-<Page_Down>" , windows W.focusDown)
  , ("M1-<Tab>"      , windows W.focusDown)
  , ("M-k"           , windows W.focusUp)
  , ("M-w"           , windows W.focusUp)
  , ("M-<Page_Up>"   , windows W.focusUp)
  , ("M1-S-<Tab>"    , windows W.focusUp)
  -- , ("M-e"           , focusMaster)
  , ("M1-`"          , nextMatch History (return True))
  -- , ("M1-<Tab>"      , toggleFocus)

  -- Swap
  , ("M-S-j"         , windows W.swapDown)
  , ("M-S-k"         , windows W.swapUp)
  , ("M-m"           , dwmpromote)
  , ("M-S-m"         , windows W.swapMaster)

  -- -- Move
  -- , ("C-M1-h"         , sendMessage $ Move L)
  -- , ("C-M1-j"         , sendMessage $ Move D)
  -- , ("C-M1-k"         , sendMessage $ Move U)
  -- , ("C-M1-l"         , sendMessage $ Move R)

  -- Resize
  , ("M-h"           , sendMessage Shrink)
  , ("M-l"           , sendMessage Expand)
  , ("M-S-h"         , sendMessage MirrorExpand)
  , ("M-S-l"         , sendMessage MirrorShrink)
  , ("M-<L>"         , sendMessage Shrink)
  , ("M-<R>"         , sendMessage Expand)
  , ("M-<U>"         , sendMessage MirrorExpand)
  , ("M-<D>"         , sendMessage MirrorShrink)

  -- , ("M-S-f"         , sendMessage $ Toggle FULL)
  , ("M-t"           , withFocused $ windows . W.sink)
  , ("M-,"           , sendMessage (IncMasterN 1))
  , ("M-."           , sendMessage (IncMasterN (-1)))

  , ("C-M1-h"         , sendMessage $ pullGroup L)
  , ("C-M1-j"         , sendMessage $ pullGroup D)
  , ("C-M1-k"         , sendMessage $ pullGroup U)
  , ("C-M1-l"         , sendMessage $ pullGroup R)
  ] ++

  keysForFloating ++
  keysForControl ++
  -- }}}

  -- { Workspace } {{{

  -- navigation
  [ ("M-d"      , moveTo Next hiddenNonEmptyNonNSP)
  , ("M-a"      , moveTo Prev hiddenNonEmptyNonNSP)
  -- , ("M-g"      , sendMessage ToggleGaps)
  , ("M-<Tab>"  , toggleWS' ["NSP"])
  ] ++

  -- }}}

  -- { Clipboard } {{{
  [ ("M-c"  , clipboardCopy)
  , ("M-v"  , clipboardPaste)
  , ("M-x"  , clipboardCut)
  , ("M1-a" , ctrlA)
  , ("M1-c" , spawn "rofi -show clipboard -modi 'run,window,clipboard:greenclip print'")
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
                            && W.tag ws /= "NSP"

        -- TODO simplify these codes
        clipboardCopy = withFocused $ \w -> do
            b <- isTerminal w
            if b
               then sendKey noModMask xF86XK_Copy
               else sendKey controlMask xK_c

        clipboardPaste = withFocused $ \w -> do
            b <- isTerminal w
            if b
                then sendKey noModMask xF86XK_Paste
                else sendKey controlMask xK_v

        clipboardCut = withFocused $ \w -> do
            b <- isTerminal w
            if b
                then sendKey noModMask xF86XK_Cut
                else sendKey controlMask xK_x

        ctrlA = withFocused $ \w -> do
            b <- isTerminal w
            if b
                then sendKey noModMask xK_Home
                else sendKey mod1Mask xK_a

        isTerminal = fmap (== "Alacritty") . runQuery className

        keysForControl =
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

            -- Ouput display
            [ ("M-o " ++ k, myMonitorCtrl a)
                | (k, a) <-
                [ ("m", "mirror")
                , ("n", "normal")
                , ("a", "arandr")
                ]
            ]
            where
                myScriptDir = "$HOME/Workings/scripts/"
                myVolumeCtrl arg = spawn $ myScriptDir ++ "control-volume.sh " ++ arg
                myBrightnessCtrl arg = spawn $ myScriptDir ++ "control-brightness.sh " ++ arg
                myMonitorCtrl arg = spawn $ myScriptDir ++ "control-monitors.sh " ++ arg

        keysForFloating =
            [ ("M-f"  , toggleCentreFloat)
            , ("M-g"  , toggleMiniFloat)
            , ("M-S-f", withFocused $ windows . flip W.float (W.RationalRect 0 0 1 1))
            ] where
                floatOrNot f n = withFocused $ \windowId -> do
                    floats <- gets (W.floating . windowset)
                    if windowId `M.member` floats
                    then f
                    else n

                centreFloat w = windows $ W.float w (W.RationalRect 0.25 0.25 0.5 0.5)
                miniFloat w = windows $ W.float w (W.RationalRect 0.58 0.55 0.4 0.4)
                toggleCentreFloat = floatOrNot (withFocused $ windows . W.sink) (withFocused centreFloat)
                toggleMiniFloat = floatOrNot (withFocused $ windows . W.sink) (withFocused miniFloat)
-- }}}

-- { Border } {{{
myBorderWidth :: Dimension
myBorderWidth = 0

-- Border colors for unfocused and focused windows, respectively.
-- myNormalBorderColor  = "#dddddd"
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#1E90FF"
-- }}}

-- { Spacing } {{{
mySpacing :: l a -> ModifiedLayout Spacing l a
mySpacing = spacingRaw
  True                -- Only for >1 window
  (Border 1 1 1 1) -- Size of screen edge gaps (T B R L)
  True                -- Enable screen edge gaps
  (Border 3 3 3 3)    -- Size of window gaps
  True                -- Enable window gaps
-- }}}

-- { Hooks } {{{

-- { Layout } {{{
myTabConfig = def
  { fontName            = "xft:Monospace-10"
  , activeColor         = "#46d9ff"
  , activeBorderColor   = "#46d9ff"
  , activeTextColor     = "#282c34"
  , inactiveColor       = "#313846"
  , inactiveBorderColor = "#282c34"
  , inactiveTextColor   = "#d0d0d0"
  }

commonLayoutSetting =
  windowNavigation
  . avoidStruts
  . mouseResize
  . windowArrange
  . trackFloating

-- apply commonLayoutSetting beforehand to resolve the conflict between tall & tabbed layouts
myLayoutHook = commonLayoutSetting
    myVTiledLayout
    ||| myHTiledLayout
    ||| myTabbedLayout
    ||| myDualTabLayout
    ||| myFullLayout

    -- We need to place spacing after renamed switch the layouts normally
    where
        myVTiledLayout =
          renamed [Replace "vtile"]
          $ mkToggle (NOBORDERS ?? FULL ?? EOT)
          $ addTabs shrinkText myTabConfig
          $ subLayout [] (smartBorders Simplest)
          $ mySpacing
          $ ResizableTall
            1        -- number of master panes
            (3/100)  -- % of screen to increment by when resizing
            (1/2)    -- ratio of the master pane
            []

        myHTiledLayout =
          renamed [Replace "htile"]
          $ Mirror myVTiledLayout

        -- Don't add spacing, otherwise the movement of focus would fail
        myTabbedLayout =
          renamed [Replace "tabbed"]
          $ addTabs shrinkText myTabConfig
          $ tabbedBottom shrinkText myTabConfig

        myFullLayout = renamed [Replace "full"] Full

        myDualTabLayout =
          renamed [Replace "dualTab"]
          $ mySpacing
          $ combineTwo (TwoPane 0.03 0.5) myTabbedLayout myTabbedLayout
-- }}}

-- { Manage Hook } {{{
ruleManageHook = composeAll
  [ className =? "MPlayer"        --> doFloat
  , className =? "Gimp"           --> doFloat
  , title =? "Mozilla Firefox"    --> viewShift ( myWorkspaces !! 0 )
  , className =? "mpv"            --> viewShift ( myWorkspaces !! 2 )
  , title =? "Messenger"          --> viewShift ( myWorkspaces !! 3 )
  , title =? "LINE"               --> viewShift ( myWorkspaces !! 3 )
  , resource  =? "desktop_window" --> doIgnore
  , resource  =? "kdesktop"       --> doIgnore
  , isFullscreen                  --> doFullFloat
  , isDialog                      --> doCenterFloat
  ]
  where
    viewShift = doF . liftM2 (.) W.greedyView W.shift

myManageHook = ruleManageHook
  <+> namedScratchpadManageHook myScratchPads
-- }}}

-- { Event Hook } {{{
myEventHook = refocusLastWhen myPred
    <+> fadeWindowsEventHook
    <+> fullscreenEventHook
    where
        myPred = refocusingIsActive <||> isFloat
-- }}}

-- { Log Hook } {{{
myFadeHook = composeAll
    [ opaque -- default to opaque
    , isUnfocused --> opacity 0.9
    , (className =? "Alacritty") <&&> isUnfocused --> opacity 0.85
    -- , fmap ("Google" `isPrefixOf`) className --> opaque
    , isDialog --> opaque
    --, isUnfocused --> opacity 0.55
    --, isFloating  --> opacity 0.75
    ]
myLogHook = refocusLastLogHook
  <+> historyHook
  <+> hideOnFocusChange myScratchPads
  <+> fadeWindowsLogHook myFadeHook
  -- <+> updatePointer (0.5, 0.5) (0, 0)
-- }}}

-- { Startup Hook } {{{
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "$HOME/.config/polybar/spwan.sh"
  setWMName "LG3D"
-- }}}

-- }}}

-- { Scratch Pad } {{{
myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ NS "terminal" spwanTerm (resource =? "TermScratchpad") fullSize
  , NS "ranger" spwanRanger (resource =? "RangerScratchpad") halfSize
  , NS "neovide" "neovide" (className =? "neovide") fullSize
  ] where
    spwanTerm = "alacritty --class=TermScratchpad"
    spwanRanger = "alacritty --class=RangerScratchpad -e ranger"
    halfSize = customFloating $ W.RationalRect 0.2 0.2 0.6 0.6
    fullSize = customFloating $ W.RationalRect 0.0 0.0 1.0 1.0

-- Ref: https://www.reddit.com/r/xmonad/comments/o3i7st/hiding_scratchpads_when_loosing_focus/
hideOnFocusChange :: NamedScratchpads -> X ()
scratchpadWorkspaceTag = "NSP"
hideOnFocusChange scratches = withWindowSet $ \winSet -> do
    let cur = W.currentTag winSet
    withRecentsIn cur () $ \lastFocus _ ->
        when (lastFocus `elem` W.index winSet && cur /= scratchpadWorkspaceTag) $
            whenX (isNS lastFocus) $
                shiftToNSP (W.workspaces winSet) ($ lastFocus)
  where
    isNS :: Window -> X Bool
    isNS w = or <$> traverse ((`runQuery` w) . query) scratches

    withRecentsIn :: WorkspaceId -> a -> (Window -> Window -> X a) -> X a
    withRecentsIn tag dflt f = maybe (return dflt) (\(Recent lw cw) -> f lw cw)
                             . Map.lookup tag
                             . (\(RecentsMap m) -> m)
                           =<< XS.get

    shiftToNSP :: [WindowSpace] -> ((Window -> X ()) -> X ()) -> X ()
    shiftToNSP ws f = do
        unless (any ((scratchpadWorkspaceTag ==) . W.tag) ws) $
            addHiddenWorkspace scratchpadWorkspaceTag
        f (windows . W.shiftWin scratchpadWorkspaceTag)
-- }}}

-- { Main } {{{
main :: IO()
main = xmonad $
  withUrgencyHook NoUrgencyHook $
  ewmh $
  docks (additionalKeysP defaults myKeys)

defaults = def
  { terminal             = myTerminal
  ,   borderWidth        = myBorderWidth
  ,   modMask            = mod4Mask
  ,   workspaces         = myWorkspaces
  -- ,   normalBorderColor  = myNormalBorderColor
  -- ,   focusedBorderColor = myFocusedBorderColor
  ,   layoutHook         = myLayoutHook
  ,   manageHook         = myManageHook
  ,   handleEventHook    = myEventHook
  ,   logHook            = myLogHook
  ,   startupHook        = myStartupHook
  ,   focusFollowsMouse  = False
  -- ,   keys               = myKeys
  -- ,   mouseBindings      = myMouseBindings
  }
-- }}}

-- vim:foldmethod=marker
