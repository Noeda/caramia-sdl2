{-# LANGUAGE DeriveDataTypeable #-}

module Caramia.Internal.Error
    ( throwSDLError
    , SDLError(..) )
    where

import Data.Typeable
import Control.Exception
import qualified Data.Text as T
import Foreign.C.String
import qualified Graphics.UI.SDL as SDL

throwSDLError :: IO a
throwSDLError = do
    str <- peekCString =<< SDL.getError
    throwIO $ SDLError $ T.pack str

-- | Thrown when something goes wrong with SDL2.
--
-- The text is (hopefully) human-readable description of what happened.
data SDLError = SDLError T.Text
                deriving ( Eq, Ord, Show, Read, Typeable )

instance Exception SDLError

