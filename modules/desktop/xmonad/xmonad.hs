import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier
import XMonad.Layout.Spacing
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive             ( fadeInactiveLogHook )
import XMonad.Actions.DynamicProjects        ( Project(..)
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

main' :: D.Client -> IO ()
main' dbus = xmonad . docks . ewmhFullscreen . ewmh . dynProjects $ def
    { modMask    = mod4Mask  -- Rebind Mod to the Super key
    , terminal = "kitty"
    , layoutHook = myLayout
    , borderWidth = 3
    , workspaces = myWs
    , normalBorderColor = "#1d2021"
    , focusedBorderColor = "#fbf1c7"
    , logHook = myPolybarLogHook dbus
    } `additionalKeysP`
    [ ("M-f"   , spawn "firefox"                    )
    , ("M-r"   , spawn "rofi -disable-history -show run" )
    , ("M-w"   , spawn "rofi -disable-history -show window" )
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

-- Workspaces
homeWs, devWs, workWs, chatWs, musicWs, systemWs, miscWs :: WorkspaceId
homeWs   = "[1] home"
devWs    = "[2] dev"
workWs   = "[3] work"
chatWs   = "[4] chat"
musicWs  = "[5] media"
systemWs = "[6] system"
miscWs   = "[7] misc"

myWs :: [WorkspaceId]
myWs = [ homeWs, devWs, workWs, chatWs, musicWs, systemWs, miscWs ]

-- Projects
projects :: [Project]
projects =
  [ Project { projectName = homeWs
            , projectDirectory = "~/"
            , projectStartHook = Nothing
            }
  , Project { projectName = devWs
            , projectDirectory = "~/Repos"
            , projectStartHook = Just $ do spawn "kitty"
                                           spawn "emacs"
            }
  , Project { projectName = workWs
            , projectDirectory = "~/Repos"
            , projectStartHook = Just $ do spawn "thunderbird"
                                           spawn "mattermost-desktop"
            }
  , Project { projectName = chatWs
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "signal-desktop"
                                           spawn "element-desktop"
                                           spawn "telegram-desktop"
            }
  , Project { projectName = musicWs
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawn "spotify"
            }
  , Project { projectName = systemWs
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "kitty htop"
                                           spawn "pavucontrol"
            }
  , Project { projectName = miscWs
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawn "keepassxc"
            }
  ]

mkDbusClient :: IO D.Client
mkDbusClient = do
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.log") opts
  return dbus
 where
  opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str =
  let opath  = D.objectPath_ "/org/xmonad/Log"
      iname  = D.interfaceName_ "org.xmonad.Log"
      mname  = D.memberName_ "Update"
      signal = (D.signal opath iname mname)
      body   = [D.toVariant $ UTF8.decodeString str]
  in  D.emit dbus $ signal { D.signalBody = body }

polybarHook :: D.Client -> PP
polybarHook dbus =
  let wrapper c s | s /= "NSP" = wrap ("%{F" <> c <> "} ") " %{F-}" s
                  | otherwise  = mempty
      normal = "#fbf1c7"
      gray   = "#7F7F7F"
      orange = "#ea4300"
      purple = "#9058c7"
      red    = "#722222"
  in  def { ppOutput          = dbusOutput dbus
          , ppCurrent         = wrapper normal
          , ppVisible         = wrapper gray
          , ppUrgent          = wrapper orange
          , ppHidden          = wrapper gray
          , ppHiddenNoWindows = wrapper red
          -- i dont know how else to hide the window title
          , ppTitle           = const "" -- shorten 100 . wrapper normal
          }

myPolybarLogHook dbus = myLogHook <+> dynamicLogWithPP (polybarHook dbus)

myLogHook = fadeInactiveLogHook 0.9
