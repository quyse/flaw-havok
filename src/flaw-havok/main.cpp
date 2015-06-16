#include "Havok.h"

Havok* havok()
{
	return new Havok();
}

BOOL WINAPI DllMain(HINSTANCE hInstance, DWORD reason, VOID* reserved)
{
	return TRUE;
}
