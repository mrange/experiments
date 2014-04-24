#pragma once

#ifdef VERSIONED_COMPONENT_EXPORTS
#define VERSIONED_COMPONENT_API __declspec(dllexport)
#else
#define VERSIONED_COMPONENT_API __declspec(dllimport)
#endif

// This class is exported from the versioned_component.dll
class VERSIONED_COMPONENT_API Cversioned_component {
public:
	Cversioned_component(void);
	// TODO: add your methods here.
};

extern VERSIONED_COMPONENT_API int nversioned_component;

VERSIONED_COMPONENT_API int fnversioned_component(void);
