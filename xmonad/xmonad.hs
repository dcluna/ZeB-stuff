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
    ,((modm              , xK_Right  ), nextWS                  )]
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
  [ "net", "terminal", "editor", "musica","docs","IM","IRC","8","9"
  ]

myManageHook = composeAll . concat $
  [  [resource =? f --> doFloat | f <- floatApps ]
    ,[resource =? m --> doShift "musica" | m <- musicApps ]
    ,[resource =? n --> doShift "net" | n <- netApps ]
    ,[resource =? t --> doShift "terminal" | t <- terminalApps ]
    ,[resource =? e --> doShift "editor" | e <- editorApps ]
    ,[resource =? d --> doShift "doc" | d <- editorApps ]
    ,[resource =? im --> doShift "IM" | im <- imApps ]  
    ,[resource =? irc --> doShift "IRC" | irc <- ircApps ]
  ]
netApps = ["chromium-browser","firefox","opera"] -- works
terminalApps = ["xterm","gnome-terminal"] -- does not work (yet)
editorApps = ["emacs","vim"] -- dnw
musicApps = ["banshee","alsamixer"] -- dnw
docApps = ["evince"] -- dnt
imApps = ["pidgin"] -- dnt
ircApps = ["xchat"] -- dnt
floatApps = ["gimp","aquaria"] -- didn't test

startupApps = [("trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x191970 --height 12 &","trayer") -- system tray, configured to get only 10% of the screen
              ,("nm-applet --sm-disable &","nm-applet") -- network manager applet
              ,("chromium-browser","chromium-browse")
              ,("gnome-terminal","gnome-terminal")
              ,("emacs","emacs") 
              ,("xchat","xchat")
              ,("pidgin","pidgin")] 

cmdIfNotRunning :: String -> String -> String
cmdIfNotRunning command psName = "if [[ \"$(pgrep " ++ psName ++ ")\" =~ \"[0-9]+\" ]] ; then " ++ command ++ "; fi" -- check if pgrep's output is a number; if not (empty string), program is not running

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/dancluna/.xmobarrc" -- xmobar (90% of screen, see .xmobarrc for details)
  spawn "xmodmap /home/dancluna/.xmodmaprc"
  sequence (map (spawn . (\x -> cmdIfNotRunning (fst x) (snd x))) startupApps) -- start some always-used programs
  xmonad gnomeConfig {
  workspaces = myWorkspaces,
  modMask = mod4Mask,
  keys = myKeys,
  manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig,
  logHook = dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "green" "" . shorten 50
                , ppExtras = [ battery ]
                }
                          }