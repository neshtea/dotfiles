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

main :: IO ()
main = xmonad . docks . ewmhFullscreen . ewmh . dynProjects $ def
    { modMask    = mod4Mask  -- Rebind Mod to the Super key
    , terminal = "kitty"
    , layoutHook = myLayout
    , borderWidth = 3
    , workspaces = myWs
    , normalBorderColor = "#1d2021"
    , focusedBorderColor = "#fbf1c7"
    } `additionalKeysP`
    [ ("M-S-z" , spawn "xscreensaver-command -lock" )
    , ("M-f"   , spawn "firefox"                    )
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
