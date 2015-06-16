{-|
Module: Flaw.Havok.Physics
Description: Integration of Havok Physics.
License: MIT
-}

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Flaw.Havok.Physics
	( HavokWorld(..)
	, havokCreateWorld
	) where

import Control.Monad
import Foreign.Marshal.Array
import Foreign.Ptr

import Flaw.FFI.COM
import Flaw.Havok
import qualified Flaw.Havok.FFI as FFI
import Flaw.Math
import Flaw.Physics
import Flaw.Resource

data HavokWorld = HavokWorld FFI.HavokPhysics (Ptr FFI.HkpWorld)

instance World HavokWorld where
	newtype Shape HavokWorld = HavokShape (Ptr FFI.HkpShape)
	newtype Body HavokWorld = HavokBody (Ptr FFI.HkpRigidBody)

	createBoxShape (HavokWorld oPhysics _pWorld) (Vec3 halfSizeX halfSizeY halfSizeZ) = allocate create destroy where
		create = liftM HavokShape $ FFI.m_HavokPhysics_createBoxShape oPhysics halfSizeX halfSizeY halfSizeZ
		destroy (HavokShape pShape) = FFI.m_HavokPhysics_destroyShape oPhysics pShape

	createSphereShape (HavokWorld oPhysics _pWorld) radius = allocate create destroy where
		create = liftM HavokShape $ FFI.m_HavokPhysics_createSphereShape oPhysics radius
		destroy (HavokShape pShape) = FFI.m_HavokPhysics_destroyShape oPhysics pShape

	createBody (HavokWorld oPhysics pWorld) (HavokShape pShape) motion (Vec3 px py pz) (Quaternion (Vec4 qx qy qz qw)) = allocate create destroy where
		create = withArray [px, py, pz, 0, qx, qy, qz, qw] $ \pTransform -> do
			liftM HavokBody $ case motion of
				MotionStatic -> FFI.m_HavokPhysics_createStaticBody oPhysics pWorld pShape pTransform
				MotionDynamic mass -> FFI.m_HavokPhysics_createDynamicBody oPhysics pWorld pShape mass pTransform
		destroy (HavokBody pBody) = FFI.m_HavokPhysics_deleteBody oPhysics pBody

	getBodyTransform (HavokWorld oPhysics _pWorld) (HavokBody pBody) = allocaArray 8 $ \pTransform -> do
		FFI.m_HavokPhysics_getBodyTransform oPhysics pBody pTransform
		[px, py, pz, _pw, qx, qy, qz, qw] <- peekArray 8 pTransform
		return (Vec3 px py pz, Quaternion (Vec4 qx qy qz qw))

	stepWorld (HavokWorld oPhysics pWorld) time = FFI.m_HavokPhysics_step oPhysics pWorld time

havokCreateWorld :: ResourceIO m => Havok -> m (ReleaseKey, HavokWorld)
havokCreateWorld (Havok oHavok) = allocate create destroy where
	create = do
		oPhysics <- peekCOMObject =<< FFI.m_Havok_getPhysics oHavok
		pWorld <- FFI.m_HavokPhysics_createWorld oPhysics
		return $ HavokWorld oPhysics pWorld
	destroy (HavokWorld oPhysics pWorld) = FFI.m_HavokPhysics_destroyWorld oPhysics pWorld
