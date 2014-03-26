-- | SDL2 OpenGL contexts for Caramia.
--
-- This module provides functions to initialize SDL2 and use it to provide an
-- OpenGL context to Caramia library.
--

module Caramia.SDL2Context
    (
    -- * Running contexts
      runSDL2Context
    , ContextCreation(..)
    , simpleContext
    -- * Swapping
    , swapBuffers
    -- * Exceptions
    , SDLError(..)
    , Sizing(..) )
    where

import Caramia.Context
import Caramia.Internal.Error
import Control.Exception
import Control.Lens
import Control.Concurrent
import Control.Monad
import Control.Applicative
import Foreign.Ptr
import Foreign.C.String
import Data.Typeable
import Data.Bits
import System.Environment
import Graphics.UI.SDL
import qualified Graphics.UI.SDL as SDL
import qualified Data.Text as T

-- | Specifies the resolution and window/fullscreen mode.
data Sizing =
    Fullscreen !Int !Int    -- ^ Fullscreen with specific size.
  | FullscreenNativeRes     -- ^ Fullscreen with native resolution.
  | Windowed !Int !Int      -- ^ Windowed with specific size.
  deriving ( Eq, Ord, Show, Read, Typeable )

-- | Specifies some details on how you want your initialization to be.
data ContextCreation = ContextCreation
    {  sizing :: Sizing
    -- ^ Desired sizing of the application window.
    , allowResizing :: Bool
    -- ^ Allow resizing the window by user after context creation? Resizing can
    -- still be done afterwards.
    , title :: T.Text
    -- ^ Window title. Pick one that will strike fear into the hearts of your
    -- puny users.
    }
    deriving ( Eq, Ord, Show, Read, Typeable )

newtype SDLContextLocal = SDLContextLocal { window :: SDL.Window }
                          deriving ( Typeable )

-- | Simple, windowed context. Resolution is set to 800x600.
simpleContext :: ContextCreation
simpleContext = ContextCreation
    { sizing = Windowed 800 600
    , allowResizing = False
    , title = "Caramia SDL2" }


-- | Runs a Caramia context with OpenGL context created by the SDL2 library.
--
-- This initializes the SDL2 library (SDL_Init()) and then quits it after
-- action is done (SDL_Quit()).
--
-- If something goes wrong, `SDLError` exception is thrown.
runSDL2Context :: ContextCreation
               -> IO a
               -> IO a
runSDL2Context creation action = mask $ \restore -> runInBoundThread $ do
    is_it_zero <- SDL.init initFlagEverything
    unless (is_it_zero == 0) $ throwSDLError

    finally ?? SDL.quit $ do
        -- Set the OpenGL attributes; we need them.
        _ <- glSetAttribute glAttrContextMajorVersion 3
        _ <- glSetAttribute glAttrContextMinorVersion 2
        _ <- glSetAttribute glAttrContextProfileMask glProfileCore
        maybe (return ())
              (\_ -> void $
                  glSetAttribute glAttrContextFlags glContextFlagDebug)
                =<< lookupEnv "CARAMIA_OPENGL_DEBUG"

        window <- withCString (T.unpack (title creation)) $ \cstr ->
            createWindow cstr
                         windowPosUndefined
                         windowPosUndefined
                         (fromIntegral w)
                         (fromIntegral h)
                         (windowFlagOpenGL .|.
                          windowFlagShown .|.
                          moreFlags)

        when (nullPtr == window) throwSDLError

        finally ?? SDL.destroyWindow window $ do
            context <- glCreateContext window
            when (nullPtr == context) throwSDLError

            storeContextLocalData (SDLContextLocal window)
            finally ?? glDeleteContext context $ giveContext (restore action)

  where
    moreFlags = (case sizing creation of
        Fullscreen _ _ -> windowFlagFullscreen
        FullscreenNativeRes -> windowFlagFullscreenDesktop
        Windowed _ _ -> 0) .|.
                (if allowResizing creation
                  then windowFlagResizable
                  else 0)
    (w, h) = case sizing creation of
        Fullscreen w h -> (w, h)
        Windowed w h   -> (w, h)
        FullscreenNativeRes -> (1, 1)

-- | Swaps buffers in the current context.
--
-- This also runs pending finalizers `Caramia.Context.runPendingFinalizers`.
swapBuffers :: IO ()
swapBuffers = do
    _ <- currentContextID  -- check that we are in a context
    runPendingFinalizers
    window <- retrieveContextLocalData (error "impossible")
    glSwapWindow window

