{-|
Module: Flaw.Havok.Physics
Description: Integration of Havok Physics.
License: MIT
-}

{-# LANGUAGE TypeFamilies #-}

module Flaw.Havok.Physics
	( HavokWorld(..)
	, havokCreateWorld
	) where

import Foreign.Marshal.Array
import Foreign.Ptr

import Flaw.FFI.COM
import Flaw.Havok
import qualified Flaw.Havok.FFI as FFI
import Flaw.Math
import Flaw.Physics

data HavokWorld = HavokWorld FFI.HavokPhysics (Ptr FFI.HkpWorld)

instance World HavokWorld where
	newtype Shape HavokWorld = HavokShape (Ptr FFI.HkpShape)
	newtype Body HavokWorld = HavokBody (Ptr FFI.HkpRigidBody)

	createBoxShape (HavokWorld oPhysics _pWorld) (Vec3 halfSizeX halfSizeY halfSizeZ) = do
		pShape <- FFI.m_HavokPhysics_createBoxShape oPhysics halfSizeX halfSizeY halfSizeZ
		let destroy = FFI.m_HavokPhysics_destroyShape oPhysics pShape
		return (HavokShape pShape, destroy)

	createSphereShape (HavokWorld oPhysics _pWorld) radius = do
		pShape <- FFI.m_HavokPhysics_createSphereShape oPhysics radius
		let destroy = FFI.m_HavokPhysics_destroyShape oPhysics pShape
		return (HavokShape pShape, destroy)

	createBody (HavokWorld oPhysics pWorld) (HavokShape pShape) motion (Vec3 px py pz) (Quaternion (Vec4 qx qy qz qw)) = do
		pBody <- withArray [px, py, pz, 0, qx, qy, qz, qw] $ \pTransform -> do
			case motion of
				MotionStatic -> FFI.m_HavokPhysics_createStaticBody oPhysics pWorld pShape pTransform
				MotionDynamic mass -> FFI.m_HavokPhysics_createDynamicBody oPhysics pWorld pShape mass pTransform
		let destroy = FFI.m_HavokPhysics_deleteBody oPhysics pBody
		return (HavokBody pBody, destroy)

	getBodyTransform (HavokWorld oPhysics _pWorld) (HavokBody pBody) = allocaArray 8 $ \pTransform -> do
		FFI.m_HavokPhysics_getBodyTransform oPhysics pBody pTransform
		[px, py, pz, _pw, qx, qy, qz, qw] <- peekArray 8 pTransform
		return (Vec3 px py pz, Quaternion (Vec4 qx qy qz qw))

	stepWorld (HavokWorld oPhysics pWorld) time = FFI.m_HavokPhysics_step oPhysics pWorld time

havokCreateWorld :: Havok -> IO (HavokWorld, IO ())
havokCreateWorld (Havok oHavok) = do
	oPhysics <- peekCOMObject =<< FFI.m_Havok_getPhysics oHavok
	pWorld <- FFI.m_HavokPhysics_createWorld oPhysics
	let destroy = FFI.m_HavokPhysics_destroyWorld oPhysics pWorld
	return (HavokWorld oPhysics pWorld, destroy)
