{-|
Module: Flaw.Havok
Description: Havok integration.
License: MIT
-}

module Flaw.Havok
	( Havok(..)
	, havokRun
	, havokForkOS
	, havokCreateVDB
	, havokStepVDB
	) where

import Control.Concurrent
import Control.Monad
import Foreign.Ptr

import Flaw.FFI.COM
import Flaw.FFI.Win32
import qualified Flaw.Havok.FFI as FFI

newtype Havok = Havok FFI.Havok

havokRun :: (Havok -> IO a) -> IO a
havokRun p = do
	havokFunc <- liftM mkHavok $ loadLibraryAndGetProcAddress "flaw-havok.dll" "havok"
	oHavok <- peekCOMObject =<< havokFunc
	ok <- FFI.m_Havok_init oHavok
	if ok then return ()
	else fail "failed to initialize Havok"
	r <- p $ Havok oHavok
	FFI.m_Havok_quit oHavok
	return r

type HavokProc = IO (Ptr FFI.Havok)

foreign import stdcall safe "dynamic" mkHavok :: FunPtr HavokProc -> HavokProc

havokForkOS :: Havok -> IO () -> IO ()
havokForkOS (Havok oHavok) p = do
	_ <- forkOS $ do
		FFI.m_Havok_initThread oHavok
		p
		FFI.m_Havok_quitThread oHavok
	return ()

havokCreateVDB :: Havok -> IO (IO ())
havokCreateVDB (Havok oHavok) = do
	FFI.m_Havok_createVDB oHavok
	return $ FFI.m_Havok_destroyVDB oHavok

havokStepVDB :: Havok -> Float -> IO ()
havokStepVDB (Havok oHavok) time = FFI.m_Havok_stepVDB oHavok time
