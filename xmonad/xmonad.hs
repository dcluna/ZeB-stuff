----------------------------
-- ZeBordoada's xmonad.hs --
----------------------------
import XMonad
import XMonad.Config.Gnome

-- imports from template (used in myKeys definition)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import System.Exit

-- imports from tutorial
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

-- IM layout
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import Data.Ratio ((%))

-- Layouts
import XMonad.Layout.Accordion
import XMonad.Layout.Spiral 
import XMonad.Layout.Tabbed

import XMonad.Util.Loggers -- ppExtras: battery
import XMonad.Actions.CycleWS -- cycling through workspaces
--import XMonad.Actions.Volume -- for volume control through my keys

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
 
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
 
    -- launch dmenu
    , ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
 
    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
 
    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)
 
     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
 
    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
 
    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )
 
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
 
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
 
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)
 
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
      
    -- Screenshot  
    , ((0                 , xK_Print ), spawn "gnome-screenshot")
      
    -- Volume  
    -- , ((0                 , xK_F8    ), lowerVolume 4 >> return ())
    -- , ((0                 , xK_F9    ), raiseVolume 4 >> return ())
    ]
    ++
 
    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    -- Change to adjacent workspaces  
    [((modm              , xK_Left   ), prevWS                  )
    ,((modm              , xK_Right  ), nextWS                  )
    ,((modm .|. shiftMask, xK_Right  ), shiftToNext          )
    ,((modm .|. shiftMask, xK_Left   ), shiftToPrev          )]
    ++
    
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myWorkspaces :: [String]
myWorkspaces =
  [ "1-net", "2-terminal", "3-editor", "4-musica","5-docs","6-IM","7-IRC","8","9"]

myManageHook = composeAll . concat $
  [  [resource =? f --> doFloat | f <- floatApps ]
    ,[resource =? m --> doShift (myWorkspaces !! 3) | m <- musicApps ]
    ,[resource =? n --> doShift (myWorkspaces !! 0) | n <- netApps ]
    ,[resource =? t --> doShift (myWorkspaces !! 1) | t <- terminalApps ]
    ,[resource =? e --> doShift (myWorkspaces !! 2) | e <- editorApps ]
    ,[resource =? d --> doShift (myWorkspaces !! 4) | d <- editorApps ]
    ,[className =? im --> doShift (myWorkspaces !! 5) | im <- imApps ]  
    ,[resource =? irc --> doShift (myWorkspaces !! 6) | irc <- ircApps ]
  ]
netApps = ["chromium-browser","firefox","opera"] -- works
terminalApps = ["xterm","gnome-terminal"] -- does not work (yet)
editorApps = ["emacs","vim"] -- dnw
musicApps = ["banshee","alsamixer"] -- dnw
docApps = ["evince","lowriter"] -- dnt
imApps = ["Pidgin"] -- dnt
ircApps = ["xchat"] -- dnt
floatApps = ["gimp","aquaria"] -- didn't test

startupApps = [("gnome-terminal","gnome-terminal")
              ,("emacs","emacs") 
              ,("xchat","xchat")
              ,("pidgin","pidgin")] 

cmdIfNotRunning :: String -> String -> String
cmdIfNotRunning command psName = "if [[ \"$(pgrep " ++ psName ++ ")\" =~ \"[0-9]+\" ]] ; then " ++ command ++ "; fi" -- check if pgrep's output is a number; if not (empty string), program is not running

-----------------
-- Layout hook --
-----------------
-- defining layouts on a per-workspace basis
myLayoutHook = onWorkspace (myWorkspaces !! 0) simpleTabbed $ -- browser
               avoidStruts $ onWorkspace (myWorkspaces !! 1) (spiral (6/7) ||| Accordion ||| simpleTabbed) $ -- terminal
                             onWorkspace (myWorkspaces !! 3) simpleTabbed $ -- banshee + alsamixer
                             onWorkspace (myWorkspaces !! 5) (withIM (1%7) (Title "Buddy List") simpleTabbed) $ -- IM layout
                             onWorkspace (myWorkspaces !! 2) (Mirror tall) $ -- emacs
                             onWorkspace (myWorkspaces !! 6) Full $
                             standardLayouts
                               where tall = Tall 1 0.03 0.5 -- applying default args here
                                     standardLayouts = tall ||| Mirror tall ||| Full ||| Accordion ||| spiral (6/7) ||| simpleTabbed
-- onWorkspace (myWorkspaces !! 1) (avoidStruts (spiral (6/7) ||| Accordion ||| simpleTabbed)) $ -- terminal
-- onWorkspace (myWorkspaces !! 3) (avoidStruts simpleTabbed) $ -- banshee + alsamixer
-- onWorkspace (myWorkspaces !! 5) (avoidStruts (withIM (1%7) (Title "Buddy List") simpleTabbed)) $ -- IM layout
-- onWorkspace (myWorkspaces !! 2) (avoidStruts (Mirror tall)) $ -- emacs
-- onWorkspace (myWorkspaces !! 6) (avoidStruts Full) $
-- standardLayouts                                     
                       
main = do
  spawn "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x191970 --height 12 &" -- system tray, configured to get only 10% of the screen
  xmproc <- spawnPipe "/usr/bin/xmobar /home/dancluna/.xmobarrc" -- xmobar (90% of screen, see .xmobarrc for details)
  spawn "xmodmap /home/dancluna/.xmodmaprc"
  spawn "nm-applet --sm-disable &" -- network manager applet
  sequence (map (spawn . fst) startupApps)
--sequence (map (spawn . (\x -> cmdIfNotRunning (fst x) (snd x))) startupApps) -- start some always-used programs
  xmonad gnomeConfig {
  workspaces = myWorkspaces,
  modMask = mod4Mask,
  keys = myKeys,
  manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig,
  layoutHook = myLayoutHook,
  logHook = dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "green" "" . shorten 50
                , ppExtras = [ battery ]
                }
                          }