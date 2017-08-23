#pragma once
#ifndef _TYPE_DEF
#define _TYPE_DEF

#include "DataStruct.h"

typedef int (__stdcall *FPTR_FLINK)(FREEWAY_LINK*);
typedef int (__stdcall *FPTR_SLINK)(STREET_LINK*);
typedef int (__stdcall *FPTR_FN)(FILENAME*);
typedef int (__stdcall *FPTR_NETWORK_INPUTS)(NETWORK_INPUTS);
typedef int (__stdcall *FPTR_NETWORK_INPUTSPTR)(NETWORK_INPUTS*);
typedef int (__stdcall *FPTR_FREEWAY_NETWORK_INPUTS)(FREEWAY_NETWORK_INPUTS*);
typedef int (__stdcall *FPTR_STREET_NETWORK_INPUTS)(STREET_NETWORK_INPUTS*);
typedef int (__stdcall *FPTR_RM_DATA)(RM_DATA*);
typedef int (__stdcall *FPTR_VOID)(void);
typedef int (__stdcall *FPTR_INT)(int);
typedef int (__stdcall *FPTR_INTPTR)(int*);
typedef int (__stdcall *FPTR_2INTS)(int, int);
typedef int (__stdcall *FPTR_2INTSREF)(int*, int*);
typedef int (__stdcall *FPTR_3INTS)(int, int, int);
typedef int (__stdcall *FPTR_4INTS)(int, int, int, int);
typedef int (__stdcall *FPTR_5INTS)(int, int, int, int, int);
typedef int (__stdcall *FPTR_6INTS)(int, int, int, int, int, int);
typedef int (__stdcall *FPTR_8INTS)(int, int, int, int, int, int, int, int);
typedef int (__stdcall *FPTR_9INTS)(int, int, int, int, int, int, int, int, int);
typedef int (__stdcall *FPTR_10INTS)(int, int, int, int, int, int, int, int, int, int);
typedef int (__stdcall *FPTR_ENTRYNODES)(int, int, float, ENTRYNODES_DATA*);
typedef int (__stdcall *FPTR_ENTRYNODES2)(int*, int*, float*, ENTRYNODES_DATA*);
typedef int (__stdcall *FPTR_FTC)(FTC_DATA*);
typedef int (__stdcall *FPTR_FTC_INT)(int, FTC_DATA*);
typedef int (__stdcall *FPTR_AC_INPUTS)(AC_INPUTS*);
typedef int (__stdcall *FPTR_BUSROUTE)(BUSROUTE_DATA*);
typedef int (__stdcall *FPTR_BUSSTATION)(BUSSTATION_DATA*);
typedef int (__stdcall *FPTR_DETECTOR_IN)(DETECTOR_INPUTS*);
typedef int (__stdcall *FPTR_DETECTOR_OUT)(DETECTOR_OUTPUTS*);
typedef int (__stdcall *FPTR_EVENT_DATA)(EVENT_DATA*);
typedef int (__stdcall *FPTR_PARKING_DATA)(PARKING_DATA*);
typedef int (__stdcall *FPTR_INCIDENT_DATA)(INCIDENT_DATA*);
typedef int (__stdcall *FPTR_NODE_LOCATION_DATA)(NODE_LOCATION_DATA*);
typedef int (__stdcall *FPTR_VEHICLE_TYPE_DATA)(VEHICLE_TYPE_DATA*);
typedef int (__stdcall *FPTR_NEW_ENTRYNODE_DATA)(ENTRYNODES_DATA);
typedef int (__stdcall *FPTR_COND_TURNPCTS)(COND_TURNPCTS*);
typedef int (__stdcall *FPTR_FLOAT)(float*);
typedef int (__stdcall *FPTR_VFD)(VFData*);
typedef int (__stdcall *FPTR_VSD)(VSData*);
typedef int (__stdcall *FPTR_DIVERSION)(DIVERSION_DATA*);
typedef int (__stdcall *FPTR_INTINTP)(int, int*);//Changed 4/29/15
typedef int (__stdcall *FPTR_INTFLTP)(int, float*);//Added 4/29/15
typedef int (__stdcall *FPTR_INTFLT)(int, float);//Added 4/29/15
typedef int (__stdcall *FPTR_INTINTP2FLTP)(int, int*, float*, float*);//Added 4/29/15
typedef int (__stdcall *FPTR_2INT2FLTP)(int, float*, float*);//Added 4/29/15  //not used 
typedef int (__stdcall *FPTR_INT3INTP)(int, int*, int*, int*);//Added 4/29/15
typedef int (__stdcall *FPTR_LOGICAL)(bool); //not used
typedef int (__stdcall *FPTR_CHAR)(char*);
typedef int (__stdcall *FPTR_ANIMATION_DATA) (ANIMATION_DATA*);
typedef int (__stdcall *FPTR_TIMING_PLAN)(TIMING_PLAN*);
typedef int (__stdcall *FPTR_COORDINATION)(int*);
typedef int(__stdcall *FPTR_INTERSECTIONDIM)(int, int, INTERSECTION_DIMENSIONS*);

typedef float (__stdcall *FLOATPTR_INT)(int); //Added 4/29/15
typedef int (__stdcall *FPTR_2INT2FLT)(int, int, float, float); //Added 4/29/15
typedef float (__stdcall *FPTR_2INTSF)(int, int);

typedef int (__stdcall *FPTR_MOES)(int, int*, int*, int*);
typedef float (__stdcall *FPTR_REAL)();         //July27
typedef int (__stdcall *FPTR_DCS_ENTRIES)(int*, int*, int*, float*, float*);         //July28

typedef int(__stdcall *FPTR_INTERSECTIONDIM)(int, int, INTERSECTION_DIMENSIONS*);

//SCOPE functions
typedef int (__stdcall *FPTR_DEFINE_LINKS)(int, STREET_LINK*);
typedef int (__stdcall *FPTR_DEFINE_DETECTOR_INPUTS)(int, DETECTOR_INPUTS*);
typedef int (__stdcall *FPTR_UPDATE_DETECTORS)(int, DETECTOR_OUTPUTS*);
typedef int (__stdcall *FPTR_ACS)(int, int, AC_INPUTS*);
typedef int (__stdcall *FPTR_FLOATS)(float, float);
//typedef int (__stdcall *FPTR_PHASE_STATES)(int, int, int);
typedef int (__stdcall *FPTR_PHASE_STATES)(int, int*, int*);//Oct20
typedef int (__stdcall *FPTR_GET_PHASE_STATES)(int, int*, int*);
typedef int (__stdcall *FPTR_DCSPHASE_STATES)(int*, int*);    //July27
typedef int (__stdcall *FPTR_SCOPE)(bool, float, int*, int*, int*, int*, int*);
typedef int (__stdcall *FPTR_SCOPE_PARAMETERS)(FILENAME*, int, int, int, float, bool, bool, int*);    //July27//Aug21

//DCS functions
typedef int (__stdcall *FPTR_DCS)(float, int*, int*, int*, int*, int*, int*);
typedef int (__stdcall *FPTR_DCS_PARAMETERS)(FILENAME*, int, int, int, float, int, float, float);    //July28
typedef int (__stdcall *FPTR_DCS_MAP)(int*);

//DETECTOR_DATA functions
typedef int (__stdcall *FPTR_DETECTOR_DATA_PARAMETERS)(FILENAME*, bool, int);
typedef bool (__stdcall *FPTR_DETECTOR_STATUS)();

//etRunner own functions
typedef int (__stdcall *FPTR_PATH)(int, int*);
typedef int (__stdcall *FPTR_VADD)(float, int, int, int, int, int, int, int);

typedef int(__stdcall *FPTR_BOOL)(bool);         //Sept28
typedef int(__stdcall *FPTR_GETSIG)(int, int, int*, int*, int*, int*);         //Sept29

typedef int(__stdcall *FPTR_OutProcFunction)(char*, int, int[3], int, float*);
typedef int(__stdcall *FPTR_OutProcFunction2)(char*, int, int[2], int, float*);

// EVENT DATA
typedef int (__stdcall *FPTR_LKINCDATA)(LKINCDATA*);

#endif
