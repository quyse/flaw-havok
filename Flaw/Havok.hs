{-|
Module: Flaw.Havok
Description: Havok integration.
License: MIT
-}

{-# LANGUAGE FlexibleContexts #-}

module Flaw.Havok
	( Havok(..)
	, havokInit
	, havokRunVisualDebugger
	) where

import Control.Concurrent
import Control.Monad
import Foreign.Ptr

import Flaw.FFI.COM
import Flaw.FFI.Win32
import qualified Flaw.Havok.FFI as FFI
import Flaw.Resource

newtype Havok = Havok FFI.Havok

havokInit :: ResourceIO m => m (ReleaseKey, Havok)
havokInit = allocate create destroy where
	create = do
		havokFunc <- liftM mkHavok $ loadLibraryAndGetProcAddress "flaw-havok.dll" "havok"
		oHavok <- peekCOMObject =<< havokFunc
		ok <- FFI.m_Havok_init oHavok
		if ok then return $ Havok oHavok
		else fail "failed to initialize Havok"
	destroy (Havok oHavok) = FFI.m_Havok_quit oHavok

type HavokProc = IO (Ptr FFI.Havok)

foreign import stdcall safe "dynamic" mkHavok :: FunPtr HavokProc -> HavokProc

havokRunVisualDebugger :: Havok -> IO ()
havokRunVisualDebugger (Havok oHavok) = do
	FFI.m_Havok_runVisualDebugger oHavok
	_ <- forkIO $ forever $ FFI.m_Havok_stepVisualDebugger oHavok 0.1
	return ()
