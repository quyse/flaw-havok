#include <Physics2012/Collide/Shape/Convex/Box/hkpBoxShape.h>
#include <Physics2012/Collide/Shape/Convex/Sphere/hkpSphereShape.h>
#include <Physics2012/Dynamics/Entity/hkpRigidBody.h>
#include <Physics2012/Dynamics/World/hkpWorld.h>
#include <Physics2012/Utilities/Dynamics/Inertia/hkpInertiaTensorComputer.h>
#include <Physics2012/Utilities/VisualDebugger/hkpPhysicsContext.h>

class HavokPhysics
{
private:
	hkpPhysicsContext* context;

public:
	HavokPhysics(hkArray<hkProcessContext*>& contexts)
	{
		context = new hkpPhysicsContext();
		hkpPhysicsContext::registerAllPhysicsProcesses();
		contexts.pushBack(context);
	}

	virtual hkpWorld* __stdcall createWorld()
	{
		hkpWorldCinfo info;
		info.setBroadPhaseWorldSize(1000.0f);
		info.setupSolverInfo(hkpWorldCinfo::SOLVER_TYPE_4ITERS_MEDIUM);
		hkpWorld* world = new hkpWorld(info);
		context->addWorld(world); // context will automatically remove world when it's deleted
		return world;
	}

	virtual void __stdcall destroyWorld(hkpWorld* world)
	{
		world->removeReference();
	}

	virtual hkpShape* __stdcall createSphereShape(float radius)
	{
		return new hkpSphereShape(radius);
	}

	virtual hkpShape* __stdcall createBoxShape(float halfSizeX, float halfSizeY, float halfSizeZ)
	{
		hkVector4 halfExtents(halfSizeX, halfSizeY, halfSizeZ);
		return new hkpBoxShape(halfExtents);
	}

	virtual void __stdcall destroyShape(hkpShape* shape)
	{
		shape->removeReference();
	}

	virtual hkpRigidBody* __stdcall createBody(hkpWorld* world, hkpShape* shape, float mass, float* transform)
	{
		hkpRigidBodyCinfo info;
		info.m_shape = shape;
		hkMassProperties massProperties;
		hkpInertiaTensorComputer::computeShapeVolumeMassProperties(shape, mass, massProperties);
		info.setMassProperties(massProperties);
		info.m_position.set(transform[0], transform[1], transform[2], 0);
		info.m_rotation.set(transform[4], transform[5], transform[6], transform[7]);
		hkpRigidBody* body = new hkpRigidBody(info);
		world->addEntity(body);
		body->removeReference();
		return body;
	}

	virtual void __stdcall deleteBody(hkpWorld* world, hkpRigidBody* body)
	{
		world->removeEntity(body);
	}

	virtual void __stdcall getBodyTransform(hkpRigidBody* body, float* outTransform)
	{
		hkTransform transform = body->getTransform();
		const hkVector4& translation = transform.getTranslation();
		hkQuaternion rotation(transform.getRotation());
		outTransform[0] = translation(0);
		outTransform[1] = translation(1);
		outTransform[2] = translation(2);
		outTransform[3] = 0;
		outTransform[4] = rotation(0);
		outTransform[5] = rotation(1);
		outTransform[6] = rotation(2);
		outTransform[7] = rotation(3);
	}

	virtual void __stdcall step(hkpWorld* world, float time)
	{
		world->stepDeltaTime(time);
	}
};
