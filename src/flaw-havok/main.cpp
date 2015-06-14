#include "Havok.h"

Havok globalHavok;

Havok* havok()
{
	return &globalHavok;
}

BOOL WINAPI DllMain(HINSTANCE hInstance, DWORD reason, VOID* reserved)
{
	return TRUE;
}
