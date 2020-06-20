module Main where
import Data.Bits
import Graphics.X11.Xlib
import System.Exit (exitWith, ExitCode(..))
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  dpy <- openDisplay ""
  let dflt = defaultScreen dpy
      scr = defaultScreenOfDisplay dpy
  rootw <- rootWindow dpy dflt
  win <- mkUnmanagedWindow dpy scr rootw 0 0 100 100
  setTextProperty dpy win "Hello World" wM_NAME
  mapWindow dpy win
  drawInWin dpy win
  sync dpy False
  threadDelay (10 * 1000000)
  exitWith ExitSuccess

drawInWin :: Display -> Window -> IO ()
drawInWin dpy win = do
  bgcolor <- initColor dpy "green"
  fgcolor <- initColor dpy "blue"
  gc <- createGC dpy win
  setForeground dpy gc bgcolor
  fillRectangle dpy win gc 0 0 100 100
  setForeground dpy gc fgcolor
  fillRectangle dpy win gc 2 2 96 96
  freeGC dpy gc

mkUnmanagedWindow :: Display
 -> Screen
 -> Window
 -> Position
 -> Position
 -> Dimension
 -> Dimension
 -> IO Window
mkUnmanagedWindow dpy scr rw x y w h = do
  let visual = defaultVisualOfScreen scr
  win <- allocaSetWindowAttributes $ \attributes -> do
    set_override_redirect attributes True
    createWindow dpy rw x y w h 0 (defaultDepthOfScreen scr) inputOutput visual cWOverrideRedirect attributes
  return win

initColor :: Display -> String -> IO Pixel
initColor dpy color = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  (apros,real) <- allocNamedColor dpy colormap color
  return $ color_pixel apros
