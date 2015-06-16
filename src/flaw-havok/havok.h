#include <Common/Base/hkBase.h>
#include <Common/Base/Ext/hkBaseExt.h>
#include <Common/Base/keycode.cxx>
#include <Common/Base/Config/hkProductFeatures.cxx>

#include <Common/Base/Memory/System/Util/hkMemoryInitUtil.h>
#include <Common/Visualize/hkVisualDebugger.h>

#include "physics.h"

class Havok
{
private:
	hkMallocAllocator baseAllocator;
	hkArray<hkProcessContext*> contexts;
	hkRefPtr<hkVisualDebugger> vdb;


	HavokPhysics* physics;

	static void HK_CALL errorReportFunction(const char* str, void*)
	{
#ifdef _DEBUG
		OutputDebugStringA(str);
#endif
	}

public:
	Havok() : physics(nullptr) {}
	~Havok()
	{
		if(physics) delete physics;
	}

	virtual bool __stdcall init()
	{
		hkMemoryRouter* memoryRouter;
		hkMemorySystem::FrameInfo frameInfo(0);

#ifdef _DEBUG
		hkMemoryInitUtil::initMemoryTracker();

		memoryRouter = hkMemoryInitUtil::initChecking(&baseAllocator, frameInfo);
		extAllocator::initChecking();
#else
		memoryRouter = hkMemoryInitUtil::initFreeListLargeBlock(&baseAllocator, frameInfo);
		extAllocator::initDefault();
#endif

		if(!memoryRouter) return false;

		if(hkBaseSystem::init(memoryRouter, &errorReportFunction) != HK_SUCCESS) return false;

		return true;
	}

	virtual void __stdcall quit()
	{
		// deallocate itself first, as we use Havok memory system
		delete this;

		// shutdown
		hkBaseSystem::quit();
#ifdef _DEBUG
		hkMemoryInitUtil::quitMemoryTracker();
#endif
		hkMemoryInitUtil::quit();
		extAllocator::quit();
	}

	virtual void __stdcall initThread()
	{
		static hkMemoryRouter routers[16];
		static int routerIndex = 0;
		hkMemoryRouter& router = routers[routerIndex++];
		hkMemorySystem::getInstance().threadInit(router, "flawThread");
		hkBaseSystem::initThread(&router);
	}

	virtual void __stdcall quitThread()
	{
		hkMemoryRouter& router = hkMemoryRouter::getInstance();
		hkBaseSystem::quitThread();
		hkMemorySystem::getInstance().threadQuit(router);
	}

	/// Run VDB.
	/** Should be called after getPhysics(), etc. */
	virtual void __stdcall runVisualDebugger()
	{
		vdb = new hkVisualDebugger(contexts);
		vdb->serve();
	}

	virtual void __stdcall stepVisualDebugger(float time)
	{
		if(vdb)
		{
			vdb->step(time * 1000.0f);
		}
	}

	virtual HavokPhysics*  __stdcall getPhysics()
	{
		if(!physics)
		{
			physics = new HavokPhysics(contexts);
		}
		return physics;
	}
};
