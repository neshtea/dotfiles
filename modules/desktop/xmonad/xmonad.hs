import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier
import XMonad.Layout.Spacing
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import           XMonad.Hooks.FadeInactive             ( fadeInactiveLogHook )
import           XMonad.Actions.DynamicProjects        ( Project(..)
                                                       , dynamicProjects
                                                       , switchProjectPrompt
                                                       )
-- Imports for Polybar --
import qualified Codec.Binary.UTF8.String              as UTF8
import qualified DBus                                  as D
import qualified DBus.Client                           as D
import           XMonad.Hooks.DynamicLog

main :: IO ()
main = mkDbusClient >>= main'

myTerminal = "kitty"
appLauncher = "rofi -disable-history -show run"

main' :: D.Client -> IO ()
main' dbus = xmonad . docks . ewmhFullscreen . ewmh . dynProjects $ def
    { modMask    = mod4Mask  -- Rebind Mod to the Super key
    , terminal = myTerminal
    , layoutHook = myLayout
    , borderWidth = 3
    , workspaces = myWs
    , normalBorderColor = "#1d2021"
    , focusedBorderColor = "#fbf1c7"
    , logHook = myLogHook
    } `additionalKeysP`
    [ ("M-S-z" , spawn "xscreensaver-command -lock" )
    , ("M-f"   , spawn "firefox"                    )
    , ("M-r"   , spawn appLauncher                  )
    ]
  where
    dynProjects = dynamicProjects projects

myLayout = avoidStruts $ mySpacing $ tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    mySpacing = spacingRaw False 
                           (Border 5 5 5 5)
                           True
                           (Border 5 5 5 5)
                           True
    threeCol = ThreeColMid nmaster delta ratio
    tiled = Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes

mkDbusClient :: IO D.Client
mkDbusClient = do
    dbus <- D.connectSession
    D.requestName dbus (D.busName_ "org.xmonad.log") opts
    return dbus
  where
    opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str =
  let opath = D.objectPath_ "/org/xmonad/Log"
      iname = D.interfaceName_ "org.xmonad.Log"
      mname = D.memberName_ "Update"
      signal = D.signal opath iname mname
      body = [D.toVariant $ UTF8.decodeString str]
  in D.emit dbus $ signal { D.signalBody = body }

polybarHook :: D.Client -> PP
polybarHook dbus =
  let wrapper c s | s /= "NSP" = wrap ("%{F" <> c <> "} ") " %{F-}" s
                  | otherwise  = mempty
      blue   = "#2E9AFE"
      gray   = "#7F7F7F"
      orange = "#ea4300"
      purple = "#9058c7"
      red    = "#722222"
  in  def { ppOutput          = dbusOutput dbus
          , ppCurrent         = wrapper blue
          , ppVisible         = wrapper gray
          , ppUrgent          = wrapper orange
          , ppHidden          = wrapper gray
          , ppHiddenNoWindows = wrapper red
          , ppTitle           = wrapper purple . shorten 90
          }

myPolybarLogHook dbus = myLogHook <+> dynamicLogWithPP (polybarHook dbus)

myLogHook = fadeInactiveLogHook 0.9

-- Workspaces

homWs = "[1] home"
devWs = "[2] dev"
wrkWs = "[3] work"
chtWs = "[4] chat"
mscWs = "[5] misc"

myWs :: [WorkspaceId]
myWs = [ homWs, devWs, wrkWs, chtWs, mscWs ]

-- Projects
projects :: [Project]
projects =
  [ Project { projectName = homWs
            , projectDirectory = "~/"
            , projectStartHook = Nothing
            }
  , Project { projectName = devWs
            , projectDirectory = "~/Repos"
            , projectStartHook = Just $ do spawn "kitty"
                                           spawn "emacs"
            }
  , Project { projectName = wrkWs
            , projectDirectory = "~/Repos"
            , projectStartHook = Just $ do spawn "thunderbird"
                                           spawn "mattermost-desktop"
            }
  , Project { projectName = chtWs
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "signal-desktop"
                                           -- spawn "element-desktop"
                                           -- spawn "telegram-desktop"
            }
  , Project { projectName = mscWs
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawn "kitty"
            }
  ]
