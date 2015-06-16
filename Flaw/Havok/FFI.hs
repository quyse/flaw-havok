{-|
Module: Flaw.Havok.FFI
Description: Havok FFI definitions.
License: MIT
-}

{-# LANGUAGE TemplateHaskell #-}

module Flaw.Havok.FFI
	( Havok(..), Havok_Class(..)
	, HavokPhysics(..), HavokPhysics_Class(..)
	, HkpWorld()
	, HkpShape()
	, HkpRigidBody()
	) where

import Control.Monad
import Foreign.Ptr

import Flaw.FFI
import Flaw.FFI.COM.TH

data HkpWorld
data HkpShape
data HkpRigidBody

liftM concat $ sequence
	[ genCOMInterface "Havok" "00000000-0000-0000-0000-000000000000" []
		[ ([t| IO Bool |], "init")
		, ([t| IO () |], "quit")
		, ([t| IO () |], "initThread")
		, ([t| IO () |], "quitThread")
		, ([t| IO () |], "createVDB")
		, ([t| IO () |], "destroyVDB")
		, ([t| Float -> IO () |], "stepVDB")
		, ([t| IO (Ptr $(forwardRef "HavokPhysics")) |], "getPhysics")
		]
	, genCOMInterface "HavokPhysics" "00000000-0000-0000-0000-000000000000" []
		[ ([t| IO (Ptr HkpWorld) |], "createWorld")
		, ([t| Ptr HkpWorld -> IO () |], "destroyWorld")
		, ([t| Float -> IO (Ptr HkpShape) |], "createSphereShape")
		, ([t| Float -> Float -> Float -> IO (Ptr HkpShape) |], "createBoxShape")
		, ([t| Ptr HkpShape -> IO () |], "destroyShape")
		, ([t| Ptr HkpWorld -> Ptr HkpShape -> Ptr Float -> IO (Ptr HkpRigidBody) |], "createStaticBody")
		, ([t| Ptr HkpWorld -> Ptr HkpShape -> Float -> Ptr Float -> IO (Ptr HkpRigidBody) |], "createDynamicBody")
		, ([t| Ptr HkpRigidBody -> IO () |], "deleteBody")
		, ([t| Ptr HkpRigidBody -> Ptr Float -> IO () |], "getBodyTransform")
		, ([t| Ptr HkpWorld -> Float -> IO () |], "step")
		]
	]
