//   Copyright (c) 2014
//   New Global Systems for Intelligent Transportation Management Corp.
//
//   This file is part of ETFOMMAPI.
//
//   This program is free software: you can redistribute it and/or modify
//   it under the terms of the GNU Affero General Public License as
//   published by the Free Software Foundation, either version 3 of the
//   License, or (at your option) any later version.
//	
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU Affero General Public License for more details.
//
//   You should have received a copy of the GNU Affero General Public License
//   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#include "stdafx.h" //copy targetver.h to same folder as this header file
#include "etFommInterface.h"
#include <algorithm>
#include <iomanip>
#include <set>


//using namespace std; disabled by Li Zhang 09/26/2022


etFommInterface::etFommInterface()
	: use_dcs(0)
	, use_ntcip(0)
	,  external_detectors(0)
	, write_trf(0)
{
	TCHAR Path[MAX_PATH];
	int r = GetCurrentDirectory(MAX_PATH, Path);

	for (int i = 0; i < r; i++)
	{
		appPath += (char)(*(Path+i));
	}

	DefaultRnsFile = appPath + "\\rnsfile.dat"; //use current appPath
	DefaultTrfFile = appPath + "\\testInterface.trf"; //use current appPath
	m_ntimestep = 0;
	m_timestepunit = 1;

	//LZ 7/9/2024 turned to 1
	m_WriteFiles = 1;
	m_TSDFlag = 0;

	m_limit_to_maxgreen = 0;
	m_read_splits = false;
	m_use_controller_flag = false;

#if _UseController
#if _NamedPipe
	DataPipe = CreateNamedPipe(
		"\\\\.\\pipe\\DataPipe",
		PIPE_ACCESS_DUPLEX,
		PIPE_TYPE_BYTE,
		1,
		0,
		0,
		0,
		NULL
		);

	if (DataPipe == INVALID_HANDLE_VALUE)
	{
		std::cout << "Failed to create outbound pipe instance.";
		// look up error code here using GetLastError()
		system("pause");
	}
#endif
#endif
};


etFommInterface::~etFommInterface(void)
{
	if (hDLL != NULL) FreeLibrary(hDLL);
};


int etFommInterface::loadDll(std::string dllname)
{
	hDLL = LoadLibrary(dllname.c_str());

	if(hDLL != NULL) 
	{
		return 0;
	} else {
		std::string errmsg = "Can't load etFomm dll from " + dllname;
		throw(InterfaceException(errmsg));
		return -1; //fail
	}
};

int etFommInterface::InitDLLLoadings() {

	//hDLL = LoadLibrary("etfomm64.dll");
#ifndef _WIN64
	hDLL = LoadLibrary("etfomm32N.dll");
#else
	//hDLL = LoadLibrary("C:\\Users\\ngsim\\Desktop\\etfomm64.dll");
	hDLL = LoadLibrary("etfomm64.dll");
	//hDLL = LoadLibrary("C:\\Users\\NGSIM-1\\Desktop\\etFomm\\Development\\etFomm\\Main\\DLL\\etfomm64.dll");
#endif
	if (hDLL == NULL)
		throw(InterfaceException("Encountered problems loading etfomm dll."));

#if !UnifiedDLL

#endif




	return  0;
};


int etFommInterface::Init() {	
	

#if !UnifiedDLL

#endif

	//basic functions required in running a simulation
	GETSTREETLANEMOEDATA = (FPTR_OutProcFunction)GetProcAddress(hDLL, "GetStreetLaneMoeData");
	GETSTREETLINKMOEDATA = (FPTR_OutProcFunction2)GetProcAddress(hDLL, "GetStreetLinkMoeData");

	SETINPUTNAME = (FPTR_FN)GetProcAddress(hDLL, "setinputname");
	if (SETINPUTNAME == NULL) throw(InterfaceException("Encountered problems loading SETINPUTNAME."));
	SETMOEFOLDER = (FPTR_FN)GetProcAddress(hDLL, "setmoefolder");
	SETIOFLAGS = (FPTR_4INTS)GetProcAddress(hDLL, "setioflags");
	SET_RUN_INPUTS = (FPTR_10INTS)GetProcAddress(hDLL, "set_run_inputs");
	API_STARTUP = (FPTR_VOID)GetProcAddress(hDLL, "api_startup");
	if (API_STARTUP == NULL) throw(InterfaceException("Encountered problems loading API_STARTUP."));
	STARTUP = (FPTR_VOID)GetProcAddress(hDLL, "startup");
	if (STARTUP == NULL) throw(InterfaceException("Encountered problems loading STARTUP."));
	INITIALIZE = (FPTR_VOID)GetProcAddress(hDLL, "initialize");
	if (INITIALIZE == NULL) throw(InterfaceException("Encountered problems loading INITIALIZE."));
	RUNTOEQUILIBRIUM = (FPTR_VOID)GetProcAddress(hDLL, "runtoequilibrium");
	if (RUNTOEQUILIBRIUM == NULL) throw(InterfaceException("Encountered problems loading RUNTOEQUILIBRIUM."));
	SIMULATE = (FPTR_VOID)GetProcAddress(hDLL, "simulate");
	if (SIMULATE == NULL) throw(InterfaceException("Encountered problems loading SIMULATE."));
	SHUTDOWN = (FPTR_VOID)GetProcAddress(hDLL, "shutdown");
	if (SHUTDOWN == NULL) throw(InterfaceException("Encountered problems loading SHUTDOWN."));
	WRITE_ANIMATION_FILES = (FPTR_VOID)GetProcAddress(hDLL, "write_animation_files");
	SET_STOCHASTIC_FLAG = (FPTR_INT)GetProcAddress(hDLL, "set_stochastic_flag");
	WRITE_TRF_FILE = (FPTR_VOID)GetProcAddress(hDLL, "write_trf_file");
	PROCESS_FREEWAYINPUTS = (FPTR_VOID)GetProcAddress(hDLL, "process_freewayinputs");
	PROCESS_STREETINPUTS = (FPTR_VOID)GetProcAddress(hDLL, "process_streetinputs");
	GETCPUTIME = (FPTR_FLOAT)GetProcAddress(hDLL, "getcputime");
	GETELAPSEDTIME = (FPTR_FLOAT)GetProcAddress(hDLL, "getelapsedtime");
	CLOSE_LANE = (FPTR_3INTS)GetProcAddress(hDLL, "close_lane");
	REOPEN_LANE = (FPTR_3INTS)GetProcAddress(hDLL, "reopen_lane");

	//functions that set the number of various entities in the network
	SET_NETWORK_INPUTS = (FPTR_NETWORK_INPUTS)GetProcAddress(hDLL, "set_network_inputs");
	SET_FREEWAY_NETWORK_INPUTS = (FPTR_FREEWAY_NETWORK_INPUTS)GetProcAddress(hDLL, "set_freeway_network_inputs");
	SET_STREET_NETWORK_INPUTS = (FPTR_STREET_NETWORK_INPUTS)GetProcAddress(hDLL, "set_street_network_inputs");
	SET_NUMBER_OF_FREEWAYLINKS = (FPTR_INT)GetProcAddress(hDLL, "set_number_of_freewaylinks");
	SET_NUMBER_OF_STREETLINKS = (FPTR_INT)GetProcAddress(hDLL, "set_number_of_streetlinks");
	SET_NUMBER_OF_FREEWAY_DETECTORS = (FPTR_INT)GetProcAddress(hDLL, "set_number_of_freeway_detectors");
	SET_NUMBER_OF_STREET_DETECTORS = (FPTR_INT)GetProcAddress(hDLL, "set_number_of_street_detectors");
	SET_NUMBER_OF_ENTRYNODES = (FPTR_INT)GetProcAddress(hDLL, "set_number_of_entrynodes");
	SET_NUMBER_OF_RAMPMETERS = (FPTR_INT)GetProcAddress(hDLL, "set_number_of_rampmeters");
	SET_NUMBER_OF_FTC_SIGNALS = (FPTR_INT)GetProcAddress(hDLL, "set_number_of_ftc_signals");
	SET_NUMBER_OF_AC_SIGNALS = (FPTR_INT)GetProcAddress(hDLL, "set_number_of_ac_signals");
	SET_NUMBER_OF_BUSROUTES = (FPTR_INT)GetProcAddress(hDLL, "set_number_of_busroutes");
	SET_NUMBER_OF_DIVERSIONS = (FPTR_INT)GetProcAddress(hDLL, "set_number_of_diversions");
	SET_NUMBER_OF_EVENTS = (FPTR_INT)GetProcAddress(hDLL, "set_number_of_events");
	SET_NUMBER_OF_PARKING_ZONES = (FPTR_INT)GetProcAddress(hDLL, "set_number_of_parking_zones");
	SET_NUMBER_OF_INCIDENTS = (FPTR_INT)GetProcAddress(hDLL, "set_number_of_incidents");

	//functions that define the specific entities in the network
	DEFINE_FREEWAYLINKS = (FPTR_FLINK)GetProcAddress(hDLL, "define_freewaylinks");
	DEFINE_STREETLINKS = (FPTR_SLINK)GetProcAddress(hDLL, "define_streetlinks");
	DEFINE_ENTRYNODES = (FPTR_ENTRYNODES)GetProcAddress(hDLL, "define_entrynodes");
	DEFINE_RAMPMETERS = (FPTR_RM_DATA)GetProcAddress(hDLL, "define_rampmeters");
	DEFINE_FTC_SIGNALS = (FPTR_FTC)GetProcAddress(hDLL, "define_ftc_signals");
	DEFINE_AC_SIGNALS = (FPTR_AC_INPUTS)GetProcAddress(hDLL, "define_ac_signals");
	DEFINE_BUSROUTES = (FPTR_BUSROUTE)GetProcAddress(hDLL, "define_busroutes");
	DEFINE_BUSSTATIONS = (FPTR_BUSSTATION)GetProcAddress(hDLL, "define_busstations");
	DEFINE_FREEWAY_DETECTORS = (FPTR_DETECTOR_IN)GetProcAddress(hDLL, "define_freeway_detectors");
	DEFINE_STREET_DETECTORS = (FPTR_DETECTOR_IN)GetProcAddress(hDLL, "define_street_detectors");
	DEFINE_EVENTS = (FPTR_EVENT_DATA)GetProcAddress(hDLL, "define_events");
	DEFINE_PARKING_ZONES = (FPTR_PARKING_DATA)GetProcAddress(hDLL, "define_parking_zones");
	DEFINE_INCIDENTS = (FPTR_INCIDENT_DATA)GetProcAddress(hDLL, "define_incidents");
	DEFINE_NODE_COORDINATES = (FPTR_NODE_LOCATION_DATA)GetProcAddress(hDLL, "define_node_coordinates");
	DEFINE_VEHICLE_TYPES = (FPTR_VEHICLE_TYPE_DATA)GetProcAddress(hDLL, "define_vehicle_types");
	DEFINE_CONDITIONAL_TURNPCTS = (FPTR_COND_TURNPCTS)GetProcAddress(hDLL, "define_conditional_turnpcts");
	DEFINE_DIVERSIONS = (FPTR_DIVERSION)GetProcAddress(hDLL, "define_diversions");
	DEFINE_INTERSECTION_DATA = (FPTR_INTERSECTIONDIM)GetProcAddress(hDLL, "define_intersection_data");
	
	//functions that get the number of various entities in the network from etfomm
	GET_NETWORK_INPUTS = (FPTR_NETWORK_INPUTSPTR)GetProcAddress(hDLL, "get_network_inputs");
	GET_FREEWAY_NETWORK_INPUTS = (FPTR_FREEWAY_NETWORK_INPUTS)GetProcAddress(hDLL, "get_freeway_network_inputs");
	GET_STREET_NETWORK_INPUTS = (FPTR_STREET_NETWORK_INPUTS)GetProcAddress(hDLL, "get_street_network_inputs");
	GET_NUMBER_OF_ENTRYNODES = (FPTR_VOID)GetProcAddress(hDLL, "get_number_of_entrynodes");
	GET_NUMBER_OF_FREEWAYLINKS = (FPTR_VOID)GetProcAddress(hDLL, "get_number_of_freewaylinks");
	GET_NUMBER_OF_STREETLINKS = (FPTR_VOID)GetProcAddress(hDLL, "get_number_of_streetlinks");
	GET_NUMBER_OF_VEHICLE_TYPES = (FPTR_VOID)GetProcAddress(hDLL, "get_number_of_vehicle_types");
	GET_NUMBER_OF_FTC_SIGNALS = (FPTR_VOID)GetProcAddress(hDLL, "get_number_of_ftc_signals");
	GET_NUMBER_OF_AC_SIGNALS = (FPTR_VOID)GetProcAddress(hDLL, "get_number_of_ac_signals");
	GET_NUMBER_OF_RAMPMETERS = (FPTR_VOID)GetProcAddress(hDLL, "get_number_of_rampmeters");
	GET_NUMBER_OF_BUSROUTES = (FPTR_VOID)GetProcAddress(hDLL, "get_number_of_busroutes");
	GET_NUMBER_OF_DIVERSIONS = (FPTR_VOID)GetProcAddress(hDLL, "get_number_of_diversions");
	GET_NUMBER_OF_INCIDENTS = (FPTR_VOID)GetProcAddress(hDLL, "get_number_of_diversions");
	GET_NUMBER_OF_PARKING_ZONES = (FPTR_VOID)GetProcAddress(hDLL, "get_number_of_parking_zones");
	GET_NUMBER_OF_EVENTS = (FPTR_VOID)GetProcAddress(hDLL, "get_number_of_events");
	GET_NUMBER_OF_DIVERSIONS = (FPTR_VOID)GetProcAddress(hDLL, "get_number_of_diversions");
	GET_INCIDENTS = (FPTR_INCIDENT_DATA)GetProcAddress(hDLL, "get_incidents");
	GET_BUSSTATIONS = (FPTR_BUSSTATION)GetProcAddress(hDLL, "get_busstations");
	GET_NUMBER_OF_FREEWAY_DETECTORS = (FPTR_VOID)GetProcAddress(hDLL, "get_number_of_freeway_detectors");
	GET_NUMBER_OF_STREET_DETECTORS = (FPTR_VOID)GetProcAddress(hDLL, "get_number_of_street_detectors");
	GET_NODE_COORDINATES = (FPTR_NODE_LOCATION_DATA)GetProcAddress(hDLL, "get_node_coordinates");
	GET_VEHICLE_TYPES = (FPTR_VEHICLE_TYPE_DATA)GetProcAddress(hDLL, "get_vehicle_types");
	GET_CONDITIONAL_TURNPCTS = (FPTR_COND_TURNPCTS)GetProcAddress(hDLL, "get_conditional_turnpcts");
	GET_DIVERSIONS = (FPTR_DIVERSION)GetProcAddress(hDLL, "get_diversions");
	GET_FREEWAY_DETECTOR_DATA = (FPTR_DETECTOR_OUT)GetProcAddress(hDLL, "get_freeway_detector_data");
	GET_STREET_DETECTOR_OUTPUTS = (FPTR_DETECTOR_OUT)GetProcAddress(hDLL, "get_street_detector_outputs");
	GET_PARKING_ZONES = (FPTR_PARKING_DATA)GetProcAddress(hDLL, "get_parking_zones");
	GET_EVENTS = (FPTR_EVENT_DATA)GetProcAddress(hDLL, "get_events");

	//functions that get the specific entities in the network from etfomm
	GET_FREEWAYLINKS = (FPTR_FLINK)GetProcAddress(hDLL, "get_freewaylinks");
	GET_STREETLINKS = (FPTR_SLINK)GetProcAddress(hDLL, "get_streetlinks");
	GET_ENTRYNODES = (FPTR_ENTRYNODES2)GetProcAddress(hDLL, "get_entrynodes");
	GET_FTC_SIGNALS = (FPTR_FTC)GetProcAddress(hDLL, "get_ftc_signals");
	GET_AC_SIGNALS = (FPTR_AC_INPUTS)GetProcAddress(hDLL, "get_ac_signals");
	GET_RAMPMETERS = (FPTR_RM_DATA)GetProcAddress(hDLL, "get_rampmeters");
	GET_FREEWAY_DETECTOR_INPUTS = (FPTR_DETECTOR_IN)GetProcAddress(hDLL, "get_freeway_detector_inputs");
    GET_STREET_DETECTOR_INPUTS = (FPTR_DETECTOR_IN)GetProcAddress(hDLL, "get_street_detector_inputs");
    GET_BUSROUTES = (FPTR_BUSROUTE)GetProcAddress(hDLL, "get_busroutes");
	GET_BUSSTATIONS = (FPTR_BUSSTATION)GetProcAddress(hDLL, "get_busstations");
	GET_PHASES = (FPTR_INTINTP)GetProcAddress(hDLL, "get_phases");
	GET_INTERVAL = (FPTR_INT)GetProcAddress(hDLL, "get_interval");
	GET_DURATION = (FPTR_2INTS)GetProcAddress(hDLL, "get_duration");
	GET_SPLITS_FLAGS = (FPTR_INTPTR)GetProcAddress(hDLL, "get_splits_flags");
	GET_COORDINATION_NETWORK_INPUTS = (FPTR_COORDINATION)GetProcAddress(hDLL, "get_coordination_network_inputs");
	GET_INTERSECTION_DATA = (FPTR_INTERSECTIONDIM)GetProcAddress(hDLL, "get_intersection_data");

	//functions that change inputs for subsequent time periods
	CHANGE_ENTRYVOLUMES = (FPTR_NEW_ENTRYNODE_DATA)GetProcAddress(hDLL, "change_entryvolumes");
	CHANGE_TURNPERCENTAGES = (FPTR_6INTS)GetProcAddress(hDLL, "change_turnpercentages");
	CHANGE_DURATION = (FPTR_3INTS)GetProcAddress(hDLL, "change_duration");
	INCREMENT_INTERVAL = (FPTR_INT)GetProcAddress(hDLL, "increment_interval");

	GET_FVEHICLE_STRUCT_SIZE = (FPTR_VOID)GetProcAddress(hDLL, "get_fvehicle_struct_size");
	GET_FVEHICLE_STRUCT = (FPTR_VFD)GetProcAddress(hDLL, "get_fvehicle_struct");
	SET_FVEHICLE_STRUCT = (FPTR_VFD)GetProcAddress(hDLL, "set_fvehicle_struct");
	GET_SVEHICLE_STRUCT_SIZE = (FPTR_VOID)GetProcAddress(hDLL, "get_svehicle_struct_size");
	GET_SVEHICLE_STRUCT = (FPTR_VSD)GetProcAddress(hDLL, "get_svehicle_struct");
	SET_SVEHICLE_STRUCT = (FPTR_VSD)GetProcAddress(hDLL, "set_svehicle_struct");

	GET_STREET_ANIMATION_DATA = (FPTR_ANIMATION_DATA)GetProcAddress(hDLL, "get_street_animation_data");
	GET_FREEWAY_ANIMATION_DATA = (FPTR_ANIMATION_DATA)GetProcAddress(hDLL, "get_freeway_animation_data");

	GET_ETFOMM_MESSAGE_COUNT = (FPTR_VOID)GetProcAddress(hDLL, "get_etfomm_message_count");
    GET_ETFOMM_MESSAGE = (FPTR_CHAR)GetProcAddress(hDLL, "get_etfomm_message");
	GET_ETFOMM_PHASE_STATES = (FPTR_GET_PHASE_STATES)GetProcAddress(hDLL, "get_etfomm_phase_states");
	SET_ETFOMM_PHASE_STATES = (FPTR_PHASE_STATES)GetProcAddress(hDLL, "set_etfomm_phase_states");
	UPDATE_STREET_DETECTORS = (FPTR_DETECTOR_OUT)GetProcAddress(hDLL, "update_street_detectors");

	//GET_TIMINGPLAN = (FPTR_TIMING_PLAN)GetProcAddress(hDLL, "get_timingplan");
	SET_ETFOMM_MOE_DATA = (FPTR_MOES)GetProcAddress(hDLL, "set_etfomm_moe_data");
	GET_SIMTIME = (FPTR_REAL)GetProcAddress(hDLL, "get_simtime");         //July27
	GET_USER_DCS_ENTRIES = (FPTR_DCS_ENTRIES)GetProcAddress(hDLL, "get_user_dcs_entries");
	GET_STREET_TRAVEL_TIME = (FPTR_2INTSF)GetProcAddress(hDLL, "get_street_travel_time");
	GET_PHASE_CALLS = (FPTR_INTINTP)GetProcAddress(hDLL, "get_phase_calls"); //Aug23
	GET_SIGNAL = (FPTR_GETSIG)GetProcAddress(hDLL, "get_signal");
	SET_TSIS_FLAG = (FPTR_BOOL)GetProcAddress(hDLL, "set_tsis_flag"); //Sept28
	SET_EXTERNAL_ACTUATED_CONTROL = (FPTR_VOID)GetProcAddress(hDLL, "set_external_actuated_control"); //Oct20

#if !UnifiedDLL
//SCOPE
	GET_SCOPE_MESSAGE_COUNT = (FPTR_VOID)GetProcAddress(SCOPE_hDLL, "get_scope_message_count");
	GET_SCOPE_MESSAGE = (FPTR_CHAR)GetProcAddress(SCOPE_hDLL, "get_scope_message");
	DEFINE_SCOPE_DETECTORS = (FPTR_DEFINE_DETECTOR_INPUTS)GetProcAddress(SCOPE_hDLL, "define_scope_detectors");
	DEFINE_SCOPE_CONTROLLERS = (FPTR_ACS)GetProcAddress(SCOPE_hDLL, "define_scope_controllers");
	DEFINE_COORDINATION_INPUTS = (FPTR_FLOATS)GetProcAddress(SCOPE_hDLL, "define_coordination_inputs");
	UPDATE_SCOPE_DETECTORS = (FPTR_UPDATE_DETECTORS)GetProcAddress(SCOPE_hDLL, "update_scope_detectors");
	UPDATE_SCOPE_CONTROLLERS = (FPTR_SCOPE)GetProcAddress(SCOPE_hDLL, "update_scope_controllers");
	GET_SCOPE_PHASE_STATES = (FPTR_PHASE_STATES)GetProcAddress(SCOPE_hDLL, "get_scope_phase_states");
	INITIALIZE_SCOPE = (FPTR_SCOPE_PARAMETERS)GetProcAddress(SCOPE_hDLL, "initialize_scope");
	SCOPE_SHUTDOWN = (FPTR_VOID)GetProcAddress(SCOPE_hDLL, "scope_shutdown");
//Detector
	GET_DETECTOR_DATA_MESSAGE_COUNT = (FPTR_VOID)GetProcAddress(DETECTOR_hDLL, "get_detector_data_message_count");
	GET_DETECTOR_DATA_MESSAGE = (FPTR_CHAR)GetProcAddress(DETECTOR_hDLL, "get_detector_data_message");
	INITIALIZE_DETECTOR_DATA = (FPTR_DETECTOR_DATA_PARAMETERS)GetProcAddress(DETECTOR_hDLL, "initialize_detector_data");
	DEFINE_INTERNAL_DETECTORS = (FPTR_DEFINE_DETECTOR_INPUTS)GetProcAddress(DETECTOR_hDLL, "define_internal_detectors");
	DEFINE_EXTERNAL_DETECTORS = (FPTR_DEFINE_DETECTOR_INPUTS)GetProcAddress(DETECTOR_hDLL, "define_external_detectors");
	GET_DETECTOR_DATA = (FPTR_DETECTOR_OUT)GetProcAddress(DETECTOR_hDLL, "get_detector_data");
	UPDATE_DETECTOR_DATA = (FPTR_DETECTOR_OUT)GetProcAddress(DETECTOR_hDLL, "update_detector_data");
	DETECTOR_DATA_SHUTDOWN = (FPTR_VOID)GetProcAddress(DETECTOR_hDLL, "detector_data_shutdown");
	CHECK_DETECTOR_DATA_STATUS = (FPTR_DETECTOR_STATUS)GetProcAddress(DETECTOR_hDLL, "check_detector_data_status");
//SCOPE
	GET_CONTROLLER_ID = (FPTR_INTINTP)GetProcAddress(SCOPE_hDLL, "get_controller_id");//Added 4/29/15
	GET_LOCAL_CYCLE_TIMER = (FPTR_INTFLTP)GetProcAddress(SCOPE_hDLL, "get_local_cycle_timer");//Added 4/29/15
	GET_CYCLE_LENGTH = (FPTR_INTFLTP)GetProcAddress(SCOPE_hDLL, "get_cycle_length");//Added 4/29/15
	SET_NEW_CYCLE_LENGTH = (FPTR_INTFLT)GetProcAddress(SCOPE_hDLL, "set_new_cycle_length");//Added 4/29/15
	GET_NEW_CYCLE_LENGTH = (FPTR_INTFLTP)GetProcAddress(SCOPE_hDLL, "get_new_cycle_length");//Added 4/29/15
	GET_OFFSET = (FPTR_INTFLTP)GetProcAddress(SCOPE_hDLL, "get_offset");//Added 4/29/15
	SET_NEW_OFFSET = (FPTR_INTFLT)GetProcAddress(SCOPE_hDLL, "set_new_offset");//Added 4/29/15
	GET_NEW_OFFSET = (FPTR_INTFLTP)GetProcAddress(SCOPE_hDLL, "get_new_offset");//Added 4/29/15
	GET_SPLITS = (FPTR_INTFLTP)GetProcAddress(SCOPE_hDLL, "get_splits");//Added 4/29/15
	GET_MIN_SPLITS = (FPTR_INTFLTP)GetProcAddress(SCOPE_hDLL, "get_min_splits");//Added 4/29/15
	GET_NEW_SPLITS = (FPTR_INTFLTP)GetProcAddress(SCOPE_hDLL, "get_new_splits");//Added 4/29/15
	SET_NEW_SPLITS = (FPTR_INTFLTP)GetProcAddress(SCOPE_hDLL, "set_new_splits");//Added 4/29/15
	GET_TRANSITION_METHOD = (FPTR_INTINTP2FLTP)GetProcAddress(SCOPE_hDLL, "get_transition_method");//Added 4/29/15
	SET_TRANSITION_METHOD = (FPTR_2INT2FLT)GetProcAddress(SCOPE_hDLL, "set_transition_method");//Added 4/29/15
	GET_SCOPE_MOE_DATA = (FPTR_MOES)GetProcAddress(SCOPE_hDLL, "get_scope_moe_data");
	DEFINE_COORDINATION_NETWORK_INPUTS = (FPTR_COORDINATION)GetProcAddress(SCOPE_hDLL, "define_coordination_network_inputs");

#else

	GET_CONTROLLER_ID = (FPTR_INTINTP)GetProcAddress(hDLL, "get_controller_id");//Added 4/29/15
	GET_LOCAL_CYCLE_TIMER = (FPTR_INTFLTP)GetProcAddress(hDLL, "get_local_cycle_timer");//Added 4/29/15
	GET_CYCLE_LENGTH = (FPTR_INTFLTP)GetProcAddress(hDLL, "get_cycle_length");//Added 4/29/15
	SET_NEW_CYCLE_LENGTH = (FPTR_INTFLT)GetProcAddress(hDLL, "set_new_cycle_length");//Added 4/29/15
	GET_NEW_CYCLE_LENGTH = (FPTR_INTFLTP)GetProcAddress(hDLL, "get_new_cycle_length");//Added 4/29/15
	GET_OFFSET = (FPTR_INTFLTP)GetProcAddress(hDLL, "get_offset");//Added 4/29/15
	SET_NEW_OFFSET = (FPTR_INTFLT)GetProcAddress(hDLL, "set_new_offset");//Added 4/29/15
	GET_NEW_OFFSET = (FPTR_INTFLTP)GetProcAddress(hDLL, "get_new_offset");//Added 4/29/15
	GET_SPLITS = (FPTR_INTFLTP)GetProcAddress(hDLL, "get_splits");//Added 4/29/15
	GET_MIN_SPLITS = (FPTR_INTFLTP)GetProcAddress(hDLL, "get_min_splits");//Added 4/29/15
	GET_NEW_SPLITS = (FPTR_INTFLTP)GetProcAddress(hDLL, "get_new_splits");//Added 4/29/15
	SET_NEW_SPLITS = (FPTR_INTFLTP)GetProcAddress(hDLL, "set_new_splits");//Added 4/29/15
	GET_TRANSITION_METHOD = (FPTR_INTINTP2FLTP)GetProcAddress(hDLL, "get_transition_method");//Added 4/29/15
	SET_TRANSITION_METHOD = (FPTR_2INT2FLT)GetProcAddress(hDLL, "set_transition_method");//Added 4/29/15

#endif //end else #if !UnifiedDLL

	if (use_dcs )
	{
#if !UnifiedDLL
#if !WCF_SCOPE
		//DCS_hDLL = LoadLibrary("dcs64.dll");
#ifndef _WIN64
		DCS_hDLL = LoadLibrary("dcs32.dll");
#else
		DCS_hDLL = LoadLibrary("dcs64.dll");
		//DCS_hDLL = LoadLibrary("C:\\Users\\NGSIM-1\\Desktop\\etFomm\\Development\\etFomm\\Main\\DLL\\dcs64.dll");
#endif
		if (DCS_hDLL == NULL) 
			throw(InterfaceException("Encountered problems loading dcs dll."));
#endif	
//DCS
		GET_DCS_MESSAGE_COUNT = (FPTR_VOID)GetProcAddress(DCS_hDLL, "get_dcs_message_count");
		GET_DCS_MESSAGE = (FPTR_CHAR)GetProcAddress(DCS_hDLL, "get_dcs_message");
		DEFINE_DCS_LINKS = (FPTR_DEFINE_LINKS)GetProcAddress(DCS_hDLL, "define_dcs_links");
		DEFINE_DCS_DETECTORS = (FPTR_DEFINE_DETECTOR_INPUTS)GetProcAddress(DCS_hDLL, "define_dcs_detectors");
		DEFINE_DCS_DETECTOR_MAP = (FPTR_DCS_MAP)GetProcAddress(DCS_hDLL, "define_dcs_detector_map");
		DEFINE_DCS_CONTROLLERS = (FPTR_ACS)GetProcAddress(DCS_hDLL, "define_dcs_controllers");
		UPDATE_DCS_DETECTORS = (FPTR_UPDATE_DETECTORS)GetProcAddress(DCS_hDLL, "update_dcs_detectors");
		RUN_DCS = (FPTR_DCS)GetProcAddress(DCS_hDLL, "run_dcs");
		SET_DCS_PHASE_STATES = (FPTR_DCSPHASE_STATES)GetProcAddress(DCS_hDLL, "set_dcs_phase_states");         //July27
		INITIALIZE_DCS = (FPTR_DCS_PARAMETERS)GetProcAddress(DCS_hDLL, "initialize_dcs");
		DCS_SHUTDOWN = (FPTR_VOID)GetProcAddress(DCS_hDLL, "dcs_shutdown");

#endif
	}


	// etRunner own functions
	ADD_PATH = (FPTR_PATH)GetProcAddress(hDLL, "add_path");		
	SET_PHASES = (FPTR_PATH)GetProcAddress(hDLL, "set_phases");
	ADD_VEHICLE = (FPTR_VADD)GetProcAddress(hDLL, "add_vehicle");
	
	SET_NUMBER_OF_ROUNDABOUTS = (FPTR_INT)GetProcAddress(hDLL, "set_number_of_roundabouts");
	DEFINE_ROUNDABOUTS = (FPTR_RABT)GetProcAddress(hDLL, "define_roundabouts");
	GET_ROUNDABOUTS = (FPTR_RABT)GetProcAddress(hDLL, "get_roundabouts");
	//move the order to avoid crashes
	//Modified to seperate L/R turningways
	//SET_NUMBER_OF_TURNING_WAYS = (FPTR_INT)GetProcAddress(hDLL, "set_number_of_turning_ways");
	//DEFINE_TURNING_WAYS = (FPTR_TW)GetProcAddress(hDLL, "define_turning_ways");
	//GET_NUMBER_OF_TURNING_WAYS = (FPTR_VOID)GetProcAddress(hDLL, "get_number_of_turning_ways");

	//GET_TURNING_WAYS = (FPTR_TW)GetProcAddress(hDLL, "get_turning_ways");

	SET_NUMBER_OF_RTURNING_WAYS = (FPTR_INT)GetProcAddress(hDLL, "set_number_of_rturning_ways");
	GET_NUMBER_OF_RTURNING_WAYS = (FPTR_VOID)GetProcAddress(hDLL, "get_number_of_rturning_ways");
	DEFINE_RTURNING_WAYS = (FPTR_TW)GetProcAddress(hDLL, "define_rturning_ways");
	GET_RTURNING_WAYS = (FPTR_TW)GetProcAddress(hDLL, "get_rturning_ways");
	SET_NUMBER_OF_LTURNING_WAYS = (FPTR_INT)GetProcAddress(hDLL, "set_number_of_lturning_ways");
	GET_NUMBER_OF_LTURNING_WAYS = (FPTR_VOID)GetProcAddress(hDLL, "get_number_of_lturning_ways");
	DEFINE_LTURNING_WAYS = (FPTR_TW)GetProcAddress(hDLL, "define_lturning_ways");
	GET_LTURNING_WAYS = (FPTR_TW)GetProcAddress(hDLL, "get_lturning_ways");
	DEFINE_SUPER_CONTROLLER = (FPTR_INTPTR)GetProcAddress(hDLL, "define_super_controller");
	GET_CONTROLLER_ID = (FPTR_INTINTP)GetProcAddress(hDLL, "get_controller_id");//Added 4/29/15



	return  0;
};


int etFommInterface::SetInputs(std::string trfFILE, int TSDFlag, int TIDFlag, int OutFlag, int CSVFlag, int WriteTextFlag, int nrun)
{
//#if UnifiedDLL
//	SET_TSIS_FLAG(0);  //Sept28//removed Oct9
//#endif
	if (trfFILE.empty())
	{
		trfFILE = DefaultTrfFile;
	} else
	{
		DefaultTrfFile = trfFILE;
	}

	if (WriteTextFlag)
	{
		TSDFlag = 2;
		//LZ breing it back
		m_WriteFiles = WriteTextFlag;
	}
	m_TSDFlag = TSDFlag;

	FILENAME InputFile;
	strcpy_s(InputFile.str, trfFILE.c_str());
	InputFile.len = strlen(InputFile.str);
	m_status = SETINPUTNAME(&InputFile); 
	CheckStatus("SETINPUTNAME", m_status);
	std::cout << "\n" << "TRF file = " << trfFILE << "\n";
	std::cout << "\n" << "SETIOFLAGS TSDFlag =" << TSDFlag
		<< " TIDFlag ="<<TIDFlag 
		<< " OutFlag ="<<OutFlag 
		<< " CSVFlag ="<<CSVFlag 
		<< " WriteTextFlag="<<WriteTextFlag 
		<< "\n";
	m_status = SETIOFLAGS(TSDFlag, TIDFlag, OutFlag, CSVFlag);
	CheckStatus("SETIOFLAGS", m_status);
	m_nrun = nrun;

	char DataFileName[512];
	strncpy_s(DataFileName, InputFile.str, InputFile.len-3);
	strcat(DataFileName, "dat");

	strcpy_s(DATAFILENAME.str, DataFileName);
	DATAFILENAME.len = strlen(DATAFILENAME.str);
	return m_status;
}

int etFommInterface::StartUP(bool API)
{
	if (API == false)
	{
		m_status = STARTUP();
		CheckStatus("STARTUP", m_status);
	}
	else
	{
		m_status = API_STARTUP();
		CheckStatus("API_STARTUP", m_status);
	}
#if !UnifiedDLL
	SCOPEDataFile.open(DATAFILENAME.str);
	WriteSCOPEDataFile(SCOPEDataFile);
	SCOPEDataFile.close();
#endif
	return m_status;
}

void etFommInterface::RunInitialize()
{
#if !UnifiedDLL
#if !WCF_SCOPE
	InitSCOPEDETDCS(&DATAFILENAME, use_dcs, use_ntcip, external_detectors);
#endif
#endif
	//if no errors call INITIALIZE until done
	int nsdet = GetNumberOfStreetDetectors();;
	//float *simtime = (float*)GetProcAddress(hDLL, "SIMPARAMS_mp_SIMTIME");
	float simtime;         //July27
	int sba[2] = {0, 0};
	int sbb[2] = {0, 0};
	int sbc[2] = {0, 0};
	int extension = 0;
	int forceoff = 0;
	int dcs_error = 0;
	int status = 0;
	NETWORK_INPUTS Network_Inputs;
	GetNetworkInputs(Network_Inputs);

	//int n_acs = GetNumberOfACSignals();
	//DETECTOR_OUTPUTS *CurrentDetectorStates = (DETECTOR_OUTPUTS*)calloc(nsdet, sizeof(DETECTOR_OUTPUTS));

	if(Network_Inputs.run_init)
	{
		std::string msg = "";
		while(status == 0)
		{
			status = INITIALIZE();
			CheckStatus("INITIALIZE", status);
#if !UnifiedDLL
			UpdateDetectorData(external_detectors);
#endif
			//if (n_acs > 0)
			//{
			//	GET_STREET_DETECTOR_OUTPUTS(CurrentDetectorStates);
			//	UPDATE_DETECTOR_DATA(CurrentDetectorStates); //send to DETECTOR_DATA
			//	if (external_detectors)
			//	{
			//		//If using external detectors the detector data has to be passed into ETFOMM
			//		UPDATE_STREET_DETECTORS(CurrentDetectorStates);
			//	}
			//}
			
			simtime = GET_SIMTIME();         //July27
			GenerateMessage(msg);
			if (msg.length() > 0 )
			{
				std::cout << msg <<"\n";
			}
#if !UnifiedDLL
			UpdateDetectorOutput(nsdet, &simtime, sba, sbb, sbc, 
				extension, forceoff, dcs_error, use_dcs, false);
#endif		
			//UpdateDetectorData(external_detectors);
		}
	}

	if (m_WriteFiles > 0) 
	{
		InitAnimationDataSet();
		if (m_timestepunit != Network_Inputs.timestep)
			m_timestepunit = Network_Inputs.timestep;
	}
	
#if _UseController
#if _NamedPipe
	if (m_use_controller_flag)
	{
		std::cout << "Waiting for a client to connect to the pipe..." << "\n";
		std::cout << "please enter the command <2> in APIClient.exe and start up the ControllerInterface.exe program" << "\n" << "\n";
		//system("start ControllerInterface.exe");

		// This call blocks until a client process connects to the pipe
		BOOL fConnected = FALSE;
		fConnected = ConnectNamedPipe(DataPipe, NULL) ? TRUE : (GetLastError() == ERROR_PIPE_CONNECTED);
		// wait here
	}
#endif
#endif
}

int etFommInterface::RunToEquilibrium(void)
{
	m_status = RUNTOEQUILIBRIUM();
	CheckStatus("RUNTOEQUILIBRIUM", m_status);

	if (m_WriteFiles > 0) 
	{
		InitAnimationDataSet();
		NETWORK_INPUTS Network_Inputs;
		GetNetworkInputs(Network_Inputs);
		if (m_timestepunit != Network_Inputs.timestep)
			m_timestepunit = Network_Inputs.timestep;
	}
	
	return m_status;
}

//StepSimulate() is not called if using unified DLL
int etFommInterface::StepSimulate(void)
{
	int nsdet = GetNumberOfStreetDetectors();
	//float *simtime = (float*)GetProcAddress(hDLL, "SIMPARAMS_mp_SIMTIME");
	//float simtime = GET_SIMTIME();
	int sba[2] = {0, 0};
	int sbb[2] = {0, 0};
	int sbc[2] = {0, 0};
	int extension = 0;
	int forceoff = 0;
	int dcs_error = 0;
	
	//float curTimeStep = m_timestepunit * m_ntimestep++;
	float curTimeStep = GetSimtime();
	//float curTimeStep = GET_SIMTIME();
//#if !UnifiedDLL
	UpdateDetectorData(external_detectors);

	UpdateDetectorOutput(nsdet, &curTimeStep, sba, sbb, sbc, 
					extension, forceoff, dcs_error, use_dcs);
//#endif
	if (use_dcs == 1 && dcs_error > 0)
	{
		return dcs_error;
	} 

	int status = 0;
	//int status = SIMULATE();
	//CheckStatus("SIMULATE", m_status);

	//UpdateDetectorData(external_detectors);

//#if UnifiedDLL
#if _UseController
	int phase_calls = 0;
	int numBytes = sizeof(int) * 3;
	DWORD numBytesWritten = 0;
	DWORD numBytesRead;
	BOOL result;

	int n_acs = GetNumberOfACSignals();
	int i_act = 2; //node 375
	int green_phases = 0;
	int yellow_phases = 0;

#if _NamedPipe	
	for(int iact = 1; iact <= n_acs; iact++)
	{
		//GET_CONTROLLER_ID(m_controller_node_id, &i_act);
		//Use NamedPipe to use Controller
		//Named Pipe get green_phases and yellow_phases, set phase_calls
		if (m_use_controller_flag && iact == i_act)
		{

			GET_PHASE_CALLS(iact, &phase_calls);


			int ControllerData[3] = {0, 0, phase_calls};

			//pass only 3 integers
			BOOL bWritePipe = WriteFile(
				DataPipe, // handle to our outbound pipe
				&ControllerData, // data to send
				numBytes, // length of data to send (bytes)
				&numBytesWritten, // will store actual amount of data sent
				NULL
				);

			if (!bWritePipe || numBytes != numBytesWritten)
			{
				//CString errMsg;
				//errMsg.Format(L"Error in writing into pipe", GetLastError());
				//return false;
				std::cout << "Error in writing into pipe " << GetLastError() << "\n";
				std::cin.get();
			}


			numBytesRead = 0;
			result = ReadFile(
				DataPipe,
				ControllerData, // the data from the pipe will be put here
				3 * sizeof(int), // number of bytes allocated
				&numBytesRead, // this will store number of bytes actually read
				NULL // not using overlapped IO
				);

			if (result)
			{
				//std::cout << "Number of bytes read: " << numBytesRead << "\n";
				//std::cout << "Greens = " << ControllerData[0] << "\n";
				//std::cout << "Yellows = " << ControllerData[1] << "\n";
				//std::cout << "phase_calls = " << ControllerData[2] << "\n";
				green_phases = ControllerData[0];
				yellow_phases = ControllerData[1];
			}
			else
			{
				std::cout << "Failed to read data from the pipe." << "\n";
			}

			status = SET_ETFOMM_PHASE_STATES(iact, &green_phases, &yellow_phases);
			CheckStatus("SET_ETFOMM_PHASE_STATES", status);


			
		}
		
	}
#endif //_NamedPipe	
#endif //_UseController
//#endif //UnifiedDLL


	NETWORK_INPUTS Network_Inputs;
	GetNetworkInputs(Network_Inputs);

	if (Network_Inputs.type_of_run > -1 && m_WriteFiles > 0) 
	{
		//float curTimeStep = *simtime;
		if (GetNumberOfFreewayLinks() > 0)
			GenFreewayData(curTimeStep);
		if (GetNumberOfStreetLinks() > 0)
			GenStreetData(curTimeStep);
		//m_ntimestep++;
	}
	
	if(m_TSDFlag == 2 && (status== 0 || status == 4)) 
		WRITE_ANIMATION_FILES();

	return status;
};

int etFommInterface::ShutDown()
{
	int m_status = SHUTDOWN();
	CheckStatus("SHUTDOWN", m_status);
#if !UnifiedDLL
	if (m_number_of_ac_signals > 0)
	{
		SCOPE_SHUTDOWN();
		DETECTOR_DATA_SHUTDOWN();
	}
#endif
	//
	// LZ test only 7/9/2024
	// if (m_WriteFiles > 0) 
	{
	//	std::cout <<"Writing Text Files..." << "\n";
	//	WriteAnimatorFiles();
	//	std::cout <<"DONE - Writing Text Files" << "\n";
	}
	return m_status;
}

void etFommInterface::WriteTRFFile(void) 
{
	WRITE_TRF_FILE();
}

int etFommInterface::SetRunInputs(int nrun, int seed1, int seed2, int seed3)
{
	if(nrun == -1 && seed1 == -1 && seed2 == -1 && seed3 == -1)
	{
		if(!SET_RUN_INPUTS)
			
			return 0;
		else
			return 1;
	}
	else
	{
		int logging = 0;
		int stochastic_flag = 1;
		m_status = SET_RUN_INPUTS(nrun, seed1, seed2, seed3, use_ntcip, use_dcs, logging, external_detectors, write_trf, stochastic_flag);
		CheckStatus("SET_RUN_INPUTS", m_status);
		
#if !UnifiedDLL
		//float d1=0, d2=0;

		//LZ turn DCS off 07/09/2024

		//GET_USER_DCS_ENTRIES(&use_dcs, &stochastic_flag, &m_limit_to_maxgreen, &m_d1_for_DZ, &m_d2_for_DZ);
#endif
		//stochastic_flag = 0;  //0 turn off randomness
		//stochastic_flag = 1;  //turn on randomness
		//SET_STOCHASTIC_FLAG(stochastic_flag); 

		return 0;
	}
}

int etFommInterface::SetNetworkInputs(NETWORK_INPUTS Network_Inputs)
{
	m_status = SET_NETWORK_INPUTS(Network_Inputs);
	CheckStatus("SET_NETWORK_INPUTS", m_status);
	return m_status;
}
void etFommInterface::SetFreewayNetworkInputs(FREEWAY_NETWORK_INPUTS Freeway_Network_Inputs)
{
	m_status = SET_FREEWAY_NETWORK_INPUTS(&Freeway_Network_Inputs);
	CheckStatus("SET_FREEWAY_NETWORK_INPUTS", m_status);
}

void etFommInterface::SetNumberOfFreewayLinks(int n_freeway_links)
{
	m_status = SET_NUMBER_OF_FREEWAYLINKS(n_freeway_links);
	CheckStatus("SET_NUMBER_OF_FREEWAYLINKS", m_status);
}
int etFommInterface::SetFreewayLinks(FREEWAY_LINK *freeway_link_data)
{
	m_status = DEFINE_FREEWAYLINKS(freeway_link_data);
	CheckStatus("DEFINE_FREEWAYLINKS", m_status);
	return m_status;
}

void etFommInterface::SetNumberOfFreewayDetectors(int n_fdet)
{
	m_status = SET_NUMBER_OF_FREEWAY_DETECTORS(n_fdet);
	CheckStatus("SET_NUMBER_OF_FREEWAY_DETECTORS", m_status);
}
void etFommInterface::SetFDetectors(DETECTOR_INPUTS *fdet_inputs)
{
	m_status = DEFINE_FREEWAY_DETECTORS(fdet_inputs);
	CheckStatus("DEFINE_FREEWAY_DETECTORS", m_status);
}

void etFommInterface::SetNumberOfStreetLinks(int n_street_links)
{
	m_status = SET_NUMBER_OF_STREETLINKS(n_street_links);
	CheckStatus("SET_NUMBER_OF_STREETLINKS", m_status);
}
int etFommInterface::SetStreetLinks(STREET_LINK *street_link_data)
{
	m_status = DEFINE_STREETLINKS(street_link_data);
	CheckStatus("DEFINE_STREETLINKS", m_status);
	return m_status;
}

int etFommInterface::SetConditionalTurnpcts(COND_TURNPCTS *cond_turnpct_data)
{
	m_status = DEFINE_CONDITIONAL_TURNPCTS(cond_turnpct_data);
	CheckStatus("DEFINE_CONDITIONAL_TURNPCTS", m_status);
	return m_status;
}

void etFommInterface::SetNumberOfEntryNodes(int n_entrynodes)
{
	m_status = SET_NUMBER_OF_ENTRYNODES(n_entrynodes);
	CheckStatus("SET_NUMBER_OF_ENTRYNODES", m_status);
}
void etFommInterface::SetEntryNodes(int typedist, int erlanga, float minsep, ENTRYNODES_DATA *entrynode_inputs)
{
	m_status = DEFINE_ENTRYNODES(typedist, erlanga, minsep, entrynode_inputs);
	CheckStatus("DEFINE_ENTRYNODES", m_status);
}

void etFommInterface::SetNumberOfFTCSignals(int n_ftcs)
{
	m_status = SET_NUMBER_OF_FTC_SIGNALS(n_ftcs);
	CheckStatus("SET_NUMBER_OF_FTC_SIGNALS", m_status);
}
void etFommInterface::SetFTCSignals(FTC_DATA *ftc_data_inputs)
{
	m_status = DEFINE_FTC_SIGNALS(ftc_data_inputs);
	CheckStatus("DEFINE_FTC_SIGNALS", m_status);
}

void etFommInterface::SetNumberOfStreetDetectors(int n_sdet)
{
	m_status = SET_NUMBER_OF_STREET_DETECTORS(n_sdet);
	CheckStatus("SET_NUMBER_OF_STREET_DETECTORS", m_status);
}
void etFommInterface::SetSDetectors(DETECTOR_INPUTS *sdet_inputs)
{
	m_status = DEFINE_STREET_DETECTORS(sdet_inputs);
	CheckStatus("DEFINE_STREET_DETECTORS", m_status);
}

void etFommInterface::SetNumberOfACSignals(int n_acs)
{
	m_status = SET_NUMBER_OF_AC_SIGNALS(n_acs);
	CheckStatus("SET_NUMBER_OF_AC_SIGNALS", m_status);
}
void etFommInterface::SetACSignals(AC_INPUTS *ac_data_inputs)
{
	m_status = DEFINE_AC_SIGNALS(ac_data_inputs);
	CheckStatus("DEFINE_AC_SIGNALS", m_status);
}

void etFommInterface::SetNumberOfRampMeters(int n_rampmeters)
{
	m_status = SET_NUMBER_OF_RAMPMETERS(n_rampmeters);
	CheckStatus("SET_NUMBER_OF_RAMPMETERS", m_status);
}
void etFommInterface::SetRampMeters(RM_DATA *rampmeter_inputs)
{
	m_status = DEFINE_RAMPMETERS(rampmeter_inputs);
	CheckStatus("DEFINE_RAMPMETERS", m_status);
}

void etFommInterface::SetNumberOfBusRoutes(int n_busroutes)
{
	m_status = SET_NUMBER_OF_BUSROUTES(n_busroutes);
	CheckStatus("SET_NUMBER_OF_BUSROUTES", m_status);
}
void etFommInterface::SetBusRoutes(BUSROUTE_DATA *busroute_inputs)
{
	m_status = DEFINE_BUSROUTES(busroute_inputs);
	CheckStatus("DEFINE_BUSROUTES", m_status);
}

void etFommInterface::SetBusStations(BUSSTATION_DATA *busstation_inputs)
{
	m_status = DEFINE_BUSSTATIONS(busstation_inputs);
	CheckStatus("DEFINE_BUSSTATIONS", m_status);
}

void etFommInterface::SetNumberOfIncidents(int n_incidents)
{
	m_status = SET_NUMBER_OF_INCIDENTS(n_incidents);
	CheckStatus("SET_NUMBER_OF_INCIDENTS", m_status);
}
void etFommInterface::SetIncidents(INCIDENT_DATA *incident_data_inputs)
{
	m_status = DEFINE_INCIDENTS(incident_data_inputs);
	CheckStatus("DEFINE_INCIDENTS", m_status);
}

void etFommInterface::SetNodeCoordinates(NODE_LOCATION_DATA *xy_coord_inputs)
{
	m_status = DEFINE_NODE_COORDINATES(xy_coord_inputs);
	CheckStatus("DEFINE_NODE_COORDINATES", m_status);
}

void etFommInterface::SetIntersectionData(std::vector<IntersectionDimensions>& intersection_inputs)
{
	for (std::vector<IntersectionDimensions>::iterator it = IntersectionDimensions_Inputs.begin();
		it != IntersectionDimensions_Inputs.end(); ++it)
	{
		if (!IsInternalNode(it->usn) || !IsInternalNode(it->dsn))
			continue;
		INTERSECTION_DIMENSIONS* pIntDim = &(it->intDim);
		m_status = DEFINE_INTERSECTION_DATA(it->usn, it->dsn, &(it->intDim));
		// status == 0, link found; 
		// -1: link not found
		// CheckStatus("DEFINE_NODE_COORDINATES", status);
	}
}

void etFommInterface::SetNumberOfRoundabouts(int n_rabts)
{
	m_status = SET_NUMBER_OF_ROUNDABOUTS(n_rabts);
	CheckStatus("SET_NUMBER_OF_ROUNDABOUTS", m_status);
}

void etFommInterface::SetRoundabouts(RABT_API_DATA* roundabout_inputs)
{
	m_status = DEFINE_ROUNDABOUTS(roundabout_inputs);
	CheckStatus("DEFINE_ROUNDABOUTS", m_status);
}

void etFommInterface::SetNumberOfTurningWays(int n_rtws)
{
	m_status = SET_NUMBER_OF_RTURNING_WAYS(n_rtws);
	CheckStatus("SET_NUMBER_OF_RTURNING_WAYS", m_status);
}

void etFommInterface::SetTurningWays(TURNING_WAY* turningway_inputs)
{
	m_status = DEFINE_RTURNING_WAYS(turningway_inputs);
	CheckStatus("DEFINE_RTURNING_WAYS", m_status);
}

void etFommInterface::ProcessFreewayInputs(void)
{
	m_status = PROCESS_FREEWAYINPUTS();
	CheckStatus("PROCESS_FREEWAYINPUTS", m_status);
}

void etFommInterface::ProcessStreetInputs(void)
{
	m_status = PROCESS_STREETINPUTS();
	CheckStatus("PROCESS_STREETINPUTS", m_status);
}

void etFommInterface::SetPHASES(int Node, int current_phases[2])
{
	m_status = SET_PHASES(Node, current_phases);
	CheckStatus("SET_PHASES", m_status);
}

int etFommInterface::SetFVehicle(VFData *vfdata)
{
	m_status = SET_FVEHICLE_STRUCT(vfdata);
	CheckStatus("SET_FVEHICLE_STRUCT", m_status);
	return m_status;
}
int etFommInterface::SetSVehicle(VSData *vsdata)
{
	m_status = SET_SVEHICLE_STRUCT(vsdata);
	CheckStatus("SET_SVEHICLE_STRUCT", m_status);
	return m_status;
}

int etFommInterface::AddPath(int NofNodes, int *nodes)
{
	int PID = ADD_PATH(NofNodes, nodes);
	CheckStatus("ADD_PATH", PID); // Xiang: will PID carry error code?
	return PID;
}

int etFommInterface::AddVehicle(float timeStep, int srcNode, int pathID, int driverType, int fleet, int type, int overSpeed, int range)
{
	int VID = ADD_VEHICLE(timeStep, srcNode, pathID, driverType, fleet, type, overSpeed, range);
	CheckStatus("ADD_VEHICLE", VID); // Xiang: will PID carry error code?
	return VID;
}

int etFommInterface::GetNumberOfVehicleTypes(void)
{
	m_number_of_vehicle_types = GET_NUMBER_OF_VEHICLE_TYPES();
	return m_number_of_vehicle_types;
}

int etFommInterface::GetNumberOfFreewayLinks(void)
{
	m_number_of_freeway_links = GET_NUMBER_OF_FREEWAYLINKS();
	return m_number_of_freeway_links;
}

int etFommInterface::GetFreewayLinks(FREEWAY_LINK *freeway_link_data)
{
	m_status = GET_FREEWAYLINKS(freeway_link_data);
	CheckStatus("GET_FREEWAYLINKS", m_status);
	return m_status;
}

int etFommInterface::GetNumberOfStreetLinks(void)
{
	m_number_of_street_links = GET_NUMBER_OF_STREETLINKS();
	return m_number_of_street_links;
}

int etFommInterface::GetStreetLinks(STREET_LINK *street_link_data)
{
	m_status = GET_STREETLINKS(street_link_data);
	CheckStatus("GET_STREETLINKS", m_status);
	return m_status;
}

int etFommInterface::GetNumberOfEntrynodes(void)
{
	m_number_of_entrynodes = GET_NUMBER_OF_ENTRYNODES();
	return m_number_of_entrynodes;
}
int etFommInterface::GetEntrynodes(int *typedist, int *erlanga, float *minsep, ENTRYNODES_DATA *entrynode_data)
{
	m_status = GET_ENTRYNODES(typedist, erlanga, minsep, entrynode_data);
	CheckStatus("GET_ENTRYNODES", m_status);
	return m_status;
}

int etFommInterface::GetNumberOfACSignals(void)
{
	m_number_of_ac_signals = GET_NUMBER_OF_AC_SIGNALS();
	return m_number_of_ac_signals;
}

int etFommInterface::GetACSignals(AC_INPUTS *ac_signal_data)
{
	m_status = GET_AC_SIGNALS(ac_signal_data); //direct approaches are now USN
	CheckStatus("GET_AC_SIGNALS", m_status);
	return m_status;
}

int etFommInterface::GetNumberOfFTCSignals(void)
{
	m_number_of_ftc_signals = GET_NUMBER_OF_FTC_SIGNALS();
	return m_number_of_ftc_signals;
}
int etFommInterface::GetFTCSignals(FTC_DATA *ftc_signal_data)
{
	m_status = GET_FTC_SIGNALS(ftc_signal_data);
	CheckStatus("GET_FTC_SIGNALS", m_status);
	return m_status;
}

int etFommInterface::GetNumberOfRampmeters(void)
{
	m_number_of_rampmeters = GET_NUMBER_OF_RAMPMETERS();
	return m_number_of_rampmeters;
}
int etFommInterface::GetRampmeters(RM_DATA *rampmeter_data)
{
	m_status = GET_RAMPMETERS(rampmeter_data);
	CheckStatus("GET_RAMPMETERS", m_status);
	return m_status;
}

int etFommInterface::GetNumberOfBusroutes(void)
{
	m_number_of_busroutes = GET_NUMBER_OF_BUSROUTES();
	return m_number_of_busroutes;
}

int etFommInterface::GetBusroutes(BUSROUTE_DATA *busroute_data)
{
	m_status = GET_BUSROUTES(busroute_data);
	CheckStatus("GET_BUSROUTES", m_status);
	return m_status;
}
int etFommInterface::GetBusstations(BUSSTATION_DATA *busstation_data)
{
	m_status = GET_BUSSTATIONS(busstation_data);
	CheckStatus("GET_BUSSTATIONS", m_status);
	return m_status;
}

float etFommInterface::GetTimeStep(void)
{
	//float *temp;
	//temp = (float*)GetProcAddress(hDLL, "SIMPARAMS_mp_TIMESTEP");
	NETWORK_INPUTS Network_Inputs;
	Network_Inputs.timestep = 0.0; //satisfy the compiler
	GET_NETWORK_INPUTS(&Network_Inputs);
	m_timestep = Network_Inputs.timestep;
	return m_timestep;
}

int etFommInterface::GetNumberOfFVehicles(void)
{
	int n_freeway_links = GetNumberOfFreewayLinks();
	if (n_freeway_links == 0)
	{
		return 0;
	} else
	{
		int *temp = (int*)GetProcAddress(hDLL, "FREEWAY_VEHICLES_mp_HIGHEST_INDEX_F");
		int n = *temp;

		int nfvehicles = GET_FVEHICLE_STRUCT_SIZE();

		// Xiang: this test condition may not be true when initialization period is used
		if (n != nfvehicles) 
		{
			std::cout<<"TODO: Get Number of Vehicles functions need to be check"<<"\n";
		}//TODO
		return nfvehicles;
	}
	
}
int etFommInterface::GetFVehicle(VFData *vfdata)
{
	m_status = GET_FVEHICLE_STRUCT(vfdata);
	CheckStatus("GET_FVEHICLE_STRUCT", m_status);
	//m_fvehicle_data = vfdata; //TODO delete
	return m_status;
}

int etFommInterface::GetNumberOfSVehicles(void)
{
	int n_street_links = GetNumberOfStreetLinks();
	if (n_street_links == 0)
	{
		return 0;
	} else
	{
		int nsvehicles = GET_SVEHICLE_STRUCT_SIZE();
		return nsvehicles;
	}
}
int etFommInterface::GetSVehicle(VSData *vsdata)
{
	m_status = GET_SVEHICLE_STRUCT(vsdata);
	CheckStatus("GET_SVEHICLE_STRUCT", m_status);
	return m_status;
}


float etFommInterface::GetCPUTime(void)
{
	m_status = GETCPUTIME(&m_cpu_time);
	CheckStatus("GETCPUTIME", m_status);
	return m_cpu_time;
}

float etFommInterface::GetElapsedTime(void)
{
	m_status = GETELAPSEDTIME(&m_elapsed_time);
	CheckStatus("GETELAPSEDTIME", m_status);
	return m_elapsed_time;
}

float etFommInterface::GetCurrentSimTime(void)
{
	float *temp = (float*)GetProcAddress(hDLL, "SIMPARAMS_mp_SIMTIME");
	m_current_simtime = *temp;
	return m_current_simtime;
}

int etFommInterface::GetCurrentPeriod(void)
{
	int *CURRENTPERIOD = (int*)GetProcAddress(hDLL, "SIMPARAMS_mp_TIME_PERIOD");
	return *CURRENTPERIOD;

}

void etFommInterface::GetNetworkInputs(NETWORK_INPUTS &Network_Inputs)
{
	m_status = GET_NETWORK_INPUTS(&Network_Inputs);
	CheckStatus("GET_NETWORK_INPUTS", m_status);
}

void etFommInterface::GetFreewayNetworkInputs(FREEWAY_NETWORK_INPUTS &Freeway_Network_Inputs)
{
	m_status = GET_FREEWAY_NETWORK_INPUTS(&Freeway_Network_Inputs);
	CheckStatus("GET_FREEWAY_NETWORK_INPUTS", m_status);
}

void etFommInterface::SetStreetNetworkInputs(STREET_NETWORK_INPUTS Street_Network_Inputs)
{
	m_status = SET_STREET_NETWORK_INPUTS(&Street_Network_Inputs);
	CheckStatus("SET_STREET_NETWORK_INPUTS", m_status);
}

void etFommInterface::GetStreetNetworkInputs(STREET_NETWORK_INPUTS &Street_Network_Inputs)
{
	m_status = GET_STREET_NETWORK_INPUTS(&Street_Network_Inputs);
	CheckStatus("GET_STREET_NETWORK_INPUTS", m_status);
}

int etFommInterface::GetNumberOfFreewayDetectors(void)
{
	return GET_NUMBER_OF_FREEWAY_DETECTORS();
}

void etFommInterface::GetConditionalTurnpcts(COND_TURNPCTS *cond_turnpct_data)
{
	m_status = GET_CONDITIONAL_TURNPCTS(cond_turnpct_data);
	CheckStatus("GET_CONDITIONAL_TURNPCTS", m_status);
}

int etFommInterface::GetNumberOfStreetDetectors(void)
{
	return GET_NUMBER_OF_STREET_DETECTORS();
}

int etFommInterface::GetNumberOfIncidents(void)
{
	return GET_NUMBER_OF_INCIDENTS();
}

int etFommInterface::GetNumberOfDiversions(void)
{
	return GET_NUMBER_OF_DIVERSIONS();
}

void etFommInterface::SetVehicleTypes(VEHICLE_TYPE_DATA *Vehicle_Type_Inputs)
{
	m_status = DEFINE_VEHICLE_TYPES(Vehicle_Type_Inputs);
	CheckStatus("DEFINE_VEHICLE_TYPES", m_status);
}

void etFommInterface::SetNumberOfParkingZones(int n_parkingzones)
{
	m_status = SET_NUMBER_OF_PARKING_ZONES(n_parkingzones);
	CheckStatus("SET_NUMBER_OF_PARKING_ZONES", m_status);
}

void etFommInterface::SetParkingZones(PARKING_DATA *Parking_Zone_Inputs)
{
	m_status = DEFINE_PARKING_ZONES(Parking_Zone_Inputs);
	CheckStatus("DEFINE_PARKING_ZONES", m_status);
}

void etFommInterface::SetNumberOfEvents(int n_events)
{
	m_status = SET_NUMBER_OF_EVENTS(n_events);
	CheckStatus("SET_NUMBER_OF_EVENTS", m_status);
}

void etFommInterface::SetEvents(EVENT_DATA *Event_Inputs)
{
	m_status = DEFINE_EVENTS(Event_Inputs);
	CheckStatus("DEFINE_EVENTS", m_status);
}

void etFommInterface::GetVehicleTypes(VEHICLE_TYPE_DATA *Vehicle_Type_Inputs)
{
	m_status = GET_VEHICLE_TYPES(Vehicle_Type_Inputs);
	CheckStatus("GET_VEHICLE_TYPES", m_status);
}

void etFommInterface::SetNumberOfDiversions(int n_diversions)
{
	m_status = SET_NUMBER_OF_DIVERSIONS(n_diversions);
	CheckStatus("SET_NUMBER_OF_DIVERSIONS", m_status);
}

void etFommInterface::SetDiversions(DIVERSION_DATA *Diversion_Inputs)
{
	m_status = DEFINE_DIVERSIONS(Diversion_Inputs);
	CheckStatus("DEFINE_DIVERSIONS", m_status);
}

void etFommInterface::GetFreewayDetectorInputs(DETECTOR_INPUTS  *Freeway_Detector_Inputs)
{
	m_status = GET_FREEWAY_DETECTOR_INPUTS(Freeway_Detector_Inputs);
	CheckStatus("GET_FREEWAY_DETECTOR_INPUTS", m_status);
}

void etFommInterface::GetStreetDetectorInputs(DETECTOR_INPUTS *Street_Detector_Inputs)
{
	m_status = GET_STREET_DETECTOR_INPUTS(Street_Detector_Inputs);
	CheckStatus("GET_STREET_DETECTOR_INPUTS", m_status);
}

void etFommInterface::GetFreewayDetectorOutputs(DETECTOR_OUTPUTS  *Freeway_Detector_Outputs)
{
	m_status = GET_FREEWAY_DETECTOR_DATA(Freeway_Detector_Outputs);
	CheckStatus("GET_FREEWAY_DETECTOR_DATA", m_status);
}
	
void etFommInterface::GetStreetDetectorOutputs(DETECTOR_OUTPUTS *Street_Detector_Outputs)
{
	m_status = GET_STREET_DETECTOR_OUTPUTS(Street_Detector_Outputs);
	CheckStatus("GET_STREET_DETECTOR_OUTPUTS", m_status);
}
	

void etFommInterface::GetIncidents(INCIDENT_DATA *incident_data_inputs)
{
	m_status = GET_INCIDENTS(incident_data_inputs);
	CheckStatus("GET_INCIDENTS", m_status);
}

void etFommInterface::GetNodeCoordinates(NODE_LOCATION_DATA *xy_coord_inputs)
{
	m_status = GET_NODE_COORDINATES(xy_coord_inputs);
	CheckStatus("GET_NODE_COORDINATES", m_status);
}

void etFommInterface::GetDiversions(DIVERSION_DATA *Diversion_Inputs)
{
	m_status = GET_DIVERSIONS(Diversion_Inputs);
	CheckStatus("GET_DIVERSIONS", m_status);
}

int etFommInterface::GetNumberOfParkingZones()
{
	return GET_NUMBER_OF_PARKING_ZONES();
}

void etFommInterface::GetParkingZones(PARKING_DATA *Parking_Zone_Inputs)
{
	m_status = GET_PARKING_ZONES(Parking_Zone_Inputs);
	CheckStatus("GET_PARKING_ZONES", m_status);
}

int etFommInterface::GetNumberOfEvents()
{
	return GET_NUMBER_OF_EVENTS();
}

void etFommInterface::GetEvents(EVENT_DATA *Event_Inputs)
{
	m_status = GET_EVENTS(Event_Inputs);
	CheckStatus("GET_EVENTS", m_status);
}

void etFommInterface::CheckStatus(const std::string &fName, int status)
{
	std::string msg = "";
	GenerateMessage(msg);
	bool IsError = 
		( fName == "SIMULATE" || fName == "INITIALIZE") ? (status == 1) : (status > 0);
	
	if (fName == "ADD_VEHICLE")
		IsError = (status == 0);

	if (IsError) 
	{
		std::cout << " Error in " << fName << " (status=" << status << "): "<< msg <<"\n";
		throw(InterfaceException(" Error in " + fName + ": " + msg));
	} else 
	{
		if (msg.length() > 0 )
		{
			std::cout << msg <<"\n";
			//CString * s = new CString(msg);
		    //PostMessage(hWnd, WM_ETFOMMMSG, 0, (LPARAM)s);
		}
	}
	
}

void etFommInterface::GenerateMessage(std::string& msg)
{
	int nMsg = GET_ETFOMM_MESSAGE_COUNT();
	if (nMsg > 0)
	{
		char tempMsg[120] = "";
		for (int iMsg = 1; iMsg <= nMsg ; ++iMsg)
		{
			GET_ETFOMM_MESSAGE(tempMsg);
			msg += (std::string(tempMsg) + "\n");
		}
		//LogFile << std::string(msg) << "\n";
	}
}

void etFommInterface::InitAnimationDataSet()
{
	m_AniLinkDataSet.clear();
	m_AniVehicleDataSet.clear();
	m_AniVehicleList.clear();
	m_VehIDToVehMap.clear();
	m_AniRampMeterDataSet.clear();
	m_AniLocationSet.clear();
	GenFreewayLinks();
	GenSurfaceLinks();
	GenNodes();
	GenLocations();
}

void etFommInterface::GenFreewayLinks()
{
	m_AniFLinkSet.clear();
	int n_freeway_links = GET_NUMBER_OF_FREEWAYLINKS();
	if (n_freeway_links > 0)
	{
		FREEWAY_LINK* temp_data = (FREEWAY_LINK*)calloc(n_freeway_links, sizeof(FREEWAY_LINK));
		int status = GET_FREEWAYLINKS(temp_data);
		CheckStatus("GET_FREEWAYLINKS", status);

		for(int il = 0; il < n_freeway_links; ++il)
		{
			FREEWAY_LINK* cur_data = &(temp_data[il]);
			int usnID = cur_data->usn;
			//if (IsInternalNode(usnID)) 
			m_FNodeSet.insert(usnID);
			int dsnID = cur_data->dsn;
			//if (IsInternalNode(dsnID)) 
			m_FNodeSet.insert(dsnID);
			AniFreewayLink newFLink = {usnID, dsnID, cur_data->length, cur_data->thrunode, 
				cur_data->exitnode, cur_data->linktype, cur_data->fulllanes, 
				cur_data->mainline_receiving_lane, cur_data->offramp_sending_lane};

			for(int iaux = 0; iaux < N_AUXLANES; iaux++)
			{
				AniAuxLane newAuxLane = {cur_data->auxlaneid[iaux], cur_data->auxlanecode[iaux], 
					cur_data->auxlanelength[iaux]};
				newFLink.auxLanes[iaux] = newAuxLane;
			}
			
			for (int iadl = 0; iadl < 3; iadl++)
			{
				AniAddDropLane newADLane = {cur_data->adddrop_lane[iadl], cur_data->adddrop_code[iadl], 
					cur_data->adddrop_dist[iadl]};
				newFLink.addDropLanes[iadl] = newADLane;
			}
			m_AniFLinkSet.push_back(newFLink);
		}

		free(temp_data);
	}
}

void etFommInterface::GenSurfaceLinks()
{
	m_AniSLinkSet.clear();
	int n_street_links = GET_NUMBER_OF_STREETLINKS();
	if (n_street_links > 0)
	{
		STREET_LINK* temp_data = (STREET_LINK*)calloc(n_street_links, sizeof(STREET_LINK));
		int status = GET_STREETLINKS(temp_data);
		CheckStatus("GET_STREETLINKS", status);
		m_UsnDsnToSLinkIDMap.clear();
		for (int il = 0; il < n_street_links; ++il)
		{
			STREET_LINK* cur_data = &(temp_data[il]);
			int usnID = cur_data->usn;
			//if (IsInternalNode(usnID)) 
			m_SNodeSet.insert(usnID);
			int dsnID = cur_data->dsn;
			//if (IsInternalNode(dsnID)) 
			m_SNodeSet.insert(dsnID);
			m_UsnDsnToSLinkIDMap[std::pair<int, int>(usnID, dsnID)] = il+1;

			// TODO: which fields are for alignlane and thrualignlane? lane1 and lane2?
			AniSurfaceLink newSLink = {usnID, dsnID, cur_data->length, cur_data->fulllanes, 
				cur_data->lane1, cur_data->lane2, cur_data->leftturnbays, /*cur_data->lengthofleftbay,*/
				cur_data->rightturnbays, /*cur_data->lengthofrightbay,*/ cur_data->leftnode,
				cur_data->thrunode, cur_data->rightnode, cur_data->diagnode, cur_data->opposenode};
			m_AniSLinkSet.push_back(newSLink);
		}

		free(temp_data);
	}
}

bool etFommInterface::IsInternalNode(int nodeID)
{
	return ( nodeID < 7000 || nodeID > 8999);
}

void etFommInterface::GenNodes()
{
	
	m_AniNodeSet.clear();
	int n_nodelocations = 8999;
	if (n_nodelocations > 0)
	{
		NODE_LOCATION_DATA *temp_data = (NODE_LOCATION_DATA*)calloc(n_nodelocations, sizeof(NODE_LOCATION_DATA));
		int status = GET_NODE_COORDINATES(temp_data);
		CheckStatus("GET_NODE_COORDINATES", status);

		if (RefNode1.is_defined > 0 && RefNode2.is_defined > 0)
			GenGPSCoordinates(temp_data);

		for (int iNode = 0; iNode < n_nodelocations; ++iNode)
		{
			// should change the test condition to is_defined == 1 after the field is correctly set
			//if (temp_data[iNode].x != 0 || temp_data[iNode].y != 0)
			int nodeID = iNode + 1;
			if (m_FNodeSet.find(nodeID) != m_FNodeSet.end() || m_SNodeSet.find(nodeID) != m_SNodeSet.end())
			{
				// determine the networkcode
				// networkcode = 0 for interface node (7000-7999) and entry node (8000-8999)
				int networkCode = 0;
				// 1 for freeway
				if (m_FNodeSet.find(nodeID) != m_FNodeSet.end() && IsInternalNode(nodeID))
					networkCode = FREEWAYNWK;
				// 2 for surface street
				if (m_SNodeSet.find(nodeID) != m_SNodeSet.end() && IsInternalNode(nodeID))
					networkCode = SURFACENWK;
				
				AniNode NewNode = {nodeID, temp_data[iNode].x, temp_data[iNode].y, networkCode,
					temp_data[iNode].latitude, temp_data[iNode].longitude, temp_data[iNode].elevation};
				//AniNode NewNode = {nodeID, temp_data[iNode].x, temp_data[iNode].y, networkCode,
				//	0, 0, 0};
				m_AniNodeSet.push_back(NewNode);
			}
		}
		free(temp_data);
	}
}

//No longer used
void etFommInterface::GenLocations()
{
	int n_ftcs = GET_NUMBER_OF_FTC_SIGNALS();
	if(n_ftcs > 0) 
	{
		FTC_DATA *ftc_data = (FTC_DATA*)calloc(n_ftcs, sizeof(FTC_DATA));
		int status = GET_FTC_SIGNALS(ftc_data);
		CheckStatus("GET_FTC_SIGNALS", status);
		for(int isig = 0; isig < n_ftcs; isig++)
		{	
			FTC_DATA *cur_data = &(ftc_data[isig]);
			int dsnID = cur_data->node;
			for (int iapp = 0; iapp < cur_data->approaches; ++iapp)
			{
				int usnID = cur_data->approach[iapp];
				if (cur_data->active_intervals == 1)
				{
					int curSignalCode = cur_data->signal_code[0][iapp];
					if (curSignalCode == 30 || curSignalCode == 31)
					{
						int signType = (curSignalCode == 30) ? STOPSIGN : YIELDSIGN;
						AniLocation newLocData = {usnID, dsnID, 0, 16, 16, 5, signType};
						m_AniLocationSet.push_back(newLocData);
					} 
				} else
				{
					AniLocation newLocData = {usnID, dsnID, 0, 16, 16, 5, 0};
					m_AniLocationSet.push_back(newLocData);
				}
			}
		}
		free(ftc_data);
	}
	
	
	int n_acs = GET_NUMBER_OF_AC_SIGNALS();
	if (n_acs > 0) 
	{
		AC_INPUTS* ac_data = (AC_INPUTS*)calloc(n_acs, sizeof(AC_INPUTS));
		int status = GET_AC_SIGNALS(ac_data);
		CheckStatus("GET_AC_SIGNALS", status);

		for (int isig = 0; isig < n_acs; ++isig)
		{
			AC_INPUTS* cur_data = &(ac_data[isig]);
			for (int nodeid = 0; nodeid++; nodeid < 8)
			{
				int dsnID = cur_data->node[nodeid];
				for (int iapp = 0; iapp < cur_data->n_direct_approaches; ++iapp)
				{
					int usnID = cur_data->direct_approach_USN[iapp];
					AniLocation newLocData = { usnID, dsnID, 0, 16, 16, 5, 0 };
					m_AniLocationSet.push_back(newLocData);
				}
			}
		}
		free(ac_data);
	}

	int n_rampmeters = GET_NUMBER_OF_RAMPMETERS();
	if(n_rampmeters > 0)
	{
		RM_DATA *rm_data = (RM_DATA*)calloc(n_rampmeters, sizeof(RM_DATA));
		int status = GET_RAMPMETERS(rm_data);
		CheckStatus("GET_RAMPMETERS", status);

		for(int isig = 0; isig < n_rampmeters; isig++)
		{
			RM_DATA *cur_data = &(rm_data[isig]);
			int linkIdx = cur_data->link -1;
			AniLocation newLocData = {m_AniFLinkSet[linkIdx].usn, m_AniFLinkSet[linkIdx].dsn, 0, 0, 0, 5, 3};
			m_AniLocationSet.push_back(newLocData);
		}
		free(rm_data);
	}

}

void etFommInterface::GenFreewayData(float timeStep)
{
	std::vector<AniLinkData> subRampMeterDataSet;
	std::map<int, int> linkIDToRMMap;
	GenRampMeterDataSet(timeStep, subRampMeterDataSet, linkIDToRMMap);

	std::list<AniLinkData> subLinkDataSet;
	std::map<int, AniLinkData*> linkIDToLinkMap;
	
	int n_freeway_vehicles = GET_FVEHICLE_STRUCT_SIZE();
	if (n_freeway_vehicles > 0)
	{
		VFData* temp_data = (VFData*)calloc(n_freeway_vehicles, sizeof(VFData));
		int status = GET_FVEHICLE_STRUCT(temp_data);
		CheckStatus("GET_FVEHICLE_STRUCT", status);

		for (int i = 0; i < n_freeway_vehicles; ++i)
		{
			if (temp_data[i].id > 0)
			{
				VFData* cur_data = &(temp_data[i]);
				// cur_data->link should decrement by 1 to be used as link index
				int linkIdx = cur_data->link -1;
				int usnID = m_AniFLinkSet[linkIdx].usn, dsnID = m_AniFLinkSet[linkIdx].dsn;
				if (m_VehIDToVehMap.find(cur_data->id) == m_VehIDToVehMap.end())
				{
					int entryNode = (cur_data->entry_link > 0) ? m_AniFLinkSet[cur_data->entry_link - 1].usn : 0;
					AniVehicle newVehicle = {cur_data->id, entryNode, cur_data->fleet,
						cur_data->vtype, cur_data->vlength, cur_data->drivertype};
					m_AniVehicleList.push_back(newVehicle);
					m_VehIDToVehMap[cur_data->id] = &(m_AniVehicleList.back());
				}
				if (usnID >= 8000) continue;
				int prevUsn = (cur_data->prevlink > 0) ? m_AniFLinkSet[cur_data->prevlink - 1].usn : 0;
				int destNode = (cur_data->destination > 0) ? m_AniFLinkSet[cur_data->destination - 1].dsn : 0;
				int leaderID = (cur_data->leader > 0) ? temp_data[cur_data->leader - 1].id : 0;
				int followerID = (cur_data->follower > 0) ? temp_data[cur_data->follower - 1].id : 0;
				int prevLane = (usnID >= 7000 && cur_data->prevlane == 0) ? cur_data->lane : cur_data->prevlane;
				
				AniVehicleData newVehicleData = {timeStep, usnID, dsnID,
					cur_data->id, cur_data->lane, cur_data->location,  prevUsn, cur_data->turncode,
					0, cur_data->acceleration, cur_data->speed, 0, 0, destNode, leaderID, followerID,
					prevLane};
				m_AniVehicleDataSet.push_back(newVehicleData);

				if (linkIDToLinkMap.find(cur_data->link) == linkIDToLinkMap.end())
				{
					AniLinkData newLinkData = {timeStep, FREEWAYNWK, usnID, dsnID,
						1, S_NONE, S_NONE, S_NONE, S_NONE, S_NONE, 0, 0, S_GREEN, cur_data->link};
					if (linkIDToRMMap.find(cur_data->link) != linkIDToRMMap.end())
					{
						newLinkData.numberOfRampMeters = 1;
						//AniLinkData *RMData = &(subRampMeterDataSet[linkIDToRMMap[cur_data->link]]);
						newLinkData.rampMeterSignalState = subRampMeterDataSet[linkIDToRMMap[cur_data->link]].rampMeterSignalState;

					}
					subLinkDataSet.push_back(newLinkData);
					linkIDToLinkMap[cur_data->link] = &(subLinkDataSet.back());
					
				} else
				{
					linkIDToLinkMap[cur_data->link]->numberOfVehicles++;
				}
			}
		}
		free(temp_data);
		m_AniLinkDataSet.insert(m_AniLinkDataSet.end(), subLinkDataSet.begin(), subLinkDataSet.end());
	}

	// update number of vehicles for ramp meter data
	for (std::vector<AniLinkData>::iterator iRM = subRampMeterDataSet.begin(); 
		iRM != subRampMeterDataSet.end(); ++iRM)
	{
		if (linkIDToLinkMap.find(iRM->linkID) != linkIDToLinkMap.end())
		{
			iRM->numberOfVehicles = linkIDToLinkMap[iRM->linkID]->numberOfVehicles;
		}
	}
	m_AniRampMeterDataSet.insert(m_AniRampMeterDataSet.end(), subRampMeterDataSet.begin(), subRampMeterDataSet.end());
}

void etFommInterface::GenRampMeterDataSet(float timeStep, std::vector<AniLinkData>& RMSet, std::map<int, int>& LinkToRMMap)
{

	int n_rampmeters = GET_NUMBER_OF_RAMPMETERS();
	if(n_rampmeters > 0)
	{
		RM_DATA *temp_data = (RM_DATA*)calloc(n_rampmeters, sizeof(RM_DATA));
		int status = GET_RAMPMETERS(temp_data);
		CheckStatus("GET_RAMPMETERS", status);

		for(int isig = 0; isig < n_rampmeters; isig++)
		{
			RM_DATA *cur_data = &(temp_data[isig]);
			int sigState = S_RED;
			switch (cur_data->state)
			{
			case MS_INACTIVE: sigState = S_NONE; break;
			case MS_RED: sigState = S_RED; break;
			case MS_GREEN: sigState = S_GREEN; break;
			}
			int linkIdx = cur_data->link -1;
			AniLinkData newRM = {timeStep, FREEWAYNWK, m_AniFLinkSet[linkIdx].usn, m_AniFLinkSet[linkIdx].dsn,
				0, S_NONE, S_NONE, S_NONE, S_NONE, S_NONE, 0, 1, sigState, cur_data->link};
			RMSet.push_back(newRM);
			LinkToRMMap[cur_data->link] = int(RMSet.size()) - 1;
		}
		free(temp_data);

		
	}
}

//No longer used
void etFommInterface::GenStreetData(float timeStep)
{
	std::list<AniLinkData> subLinkDataSet;
	std::map<int, AniLinkData*> linkIDToLinkMap;

	// process vehicles on street links
	int n_street_vehicles = GET_SVEHICLE_STRUCT_SIZE();
	if (n_street_vehicles > 0)
	{
		VSData* temp_data = (VSData*)calloc(n_street_vehicles, sizeof(VSData));
		int status = GET_SVEHICLE_STRUCT(temp_data);
		CheckStatus("GET_SVEHICLE_STRUCT", status);

		for (int i = 0; i < n_street_vehicles; ++i)
		{
			if (temp_data[i].id > 0) 
			{
				VSData* cur_data = &(temp_data[i]);
				int linkIdx = cur_data->link -1;
				int usnID = m_AniSLinkSet[linkIdx].usn;
				if (m_VehIDToVehMap.find(cur_data->id) == m_VehIDToVehMap.end())
				{
					int entryNode = (cur_data->entry_link > 0) ? m_AniSLinkSet[cur_data->entry_link - 1].usn : 0;
					AniVehicle newVehicle = {cur_data->id, entryNode, cur_data->fleet,
						cur_data->vtype, cur_data->vlength, cur_data->drivertype};
					m_AniVehicleList.push_back(newVehicle);
					m_VehIDToVehMap[cur_data->id] = &(m_AniVehicleList.back());
				}

				if (usnID >= 8000) continue;
				int dsnID = m_AniSLinkSet[linkIdx].dsn;
				int prevUsn = (cur_data->prevlink > 0) ? m_AniSLinkSet[cur_data->prevlink - 1].usn : 0;
				int leaderID = (cur_data->leader > 0) ? temp_data[cur_data->leader - 1].id : 0;
				int followerID = (cur_data->follower > 0) ? temp_data[cur_data->follower - 1].id : 0;
				int prevLane = (usnID >= 7000 && cur_data->prevlane == 0) ? cur_data->lane : cur_data->prevlane;

				AniVehicleData newVehicleData = {timeStep, usnID, dsnID,
					cur_data->id, cur_data->lane, cur_data->location, prevUsn, cur_data->turncode,
					0, cur_data->acceleration, cur_data->speed, 0, 0, 0, leaderID, followerID,
					prevLane};
				m_AniVehicleDataSet.push_back(newVehicleData);

				if (linkIDToLinkMap.find(cur_data->link) == linkIDToLinkMap.end())
				{
					AniLinkData newLinkData = {timeStep, SURFACENWK,usnID, dsnID,
						1, S_NONE, S_NONE, S_NONE, S_NONE, S_NONE, 0, 0, S_GREEN, cur_data->link};
					
					subLinkDataSet.push_back(newLinkData);
					linkIDToLinkMap[cur_data->link] = &(subLinkDataSet.back());
				} else
				{
					linkIDToLinkMap[cur_data->link]->numberOfVehicles++;
				}
			}
		}
	}
	
	std::map<int, ANIMATION_DATA*> linkIDToAniDataMap;
	int n_street_links = GET_NUMBER_OF_STREETLINKS();
	ANIMATION_DATA* animation_data;
	if (n_street_links > 0)
	{
		animation_data = (ANIMATION_DATA*)calloc(n_street_links, sizeof(ANIMATION_DATA));
		int status = GET_STREET_ANIMATION_DATA(animation_data);
		CheckStatus("GET_STREET_ANIMATION_DATA", status);

		// add links without vehicles
		//if (linkIDToLinkMap.size() < n_street_links)
		//{
			for (int il = 0; il < n_street_links; ++il)
			{
				ANIMATION_DATA* cur_data = &(animation_data[il]);
				linkIDToAniDataMap[cur_data->ID] = cur_data;
				if (cur_data->usn < 8000 
					&& linkIDToLinkMap.find(cur_data->ID) == linkIDToLinkMap.end())
				{
					AniLinkData newLinkData = {timeStep, SURFACENWK,cur_data->usn, cur_data->dsn,
						0, S_NONE, S_NONE, S_NONE, S_NONE, S_NONE, 0, 0, S_GREEN, cur_data->ID};
					
					subLinkDataSet.push_back(newLinkData);
					linkIDToLinkMap[cur_data->ID] = &(subLinkDataSet.back());
				}
			}
		//}
	}

	// generate signal codes
	GenFTCSignalCodes(linkIDToAniDataMap, linkIDToLinkMap);
	GenACSignalCodes(linkIDToAniDataMap, linkIDToLinkMap);
	m_AniLinkDataSet.insert(m_AniLinkDataSet.end(), subLinkDataSet.begin(), subLinkDataSet.end());

	free(animation_data);
}

void etFommInterface::GenFTCSignalCodes(const std::map<int, ANIMATION_DATA*>& linkIDToAniDataMap, 
									 const std::map<int, AniLinkData*>& LinkIDToLinkData)
{
	int n_ftcs = GET_NUMBER_OF_FTC_SIGNALS();
	if(n_ftcs <= 0) return;
	
	FTC_DATA *temp_data = (FTC_DATA*)calloc(n_ftcs, sizeof(FTC_DATA));
	int status = GET_FTC_SIGNALS(temp_data);
	CheckStatus("GET_FTC_SIGNALS", status);
	for(int isig = 0; isig < n_ftcs; isig++)
	{
		if (temp_data[isig].active_intervals == 1) continue;
			
		FTC_DATA *cur_data = &(temp_data[isig]);
		int dsnID = cur_data->node;
		for (int iapp = 0; iapp < cur_data->approaches; ++iapp)
		{
			int usnID = cur_data->approach[iapp];
			
			std::pair<int, int> nodePair(usnID, dsnID);
			if (m_UsnDsnToSLinkIDMap.find(nodePair) != m_UsnDsnToSLinkIDMap.end())
			{
				int linkID = m_UsnDsnToSLinkIDMap[nodePair];
				int linkIdx = linkID - 1;
				if (LinkIDToLinkData.find(linkID) != LinkIDToLinkData.end())
				{
					ANIMATION_DATA* ani_data = linkIDToAniDataMap.find(linkID)->second;
					AniLinkData* pLink = LinkIDToLinkData.find(linkID)->second;
					pLink->signalizedFlag = 1;

					// NOTE: need to decrement curInterval to be used as array index
					// because array starts at 1 in FORTRAN, while at 0 in C++
					int curInterval = cur_data->current_interval - 1;
					//int curSigCode = cur_data->signal_code[curInterval][iapp];
					int curSigCode = ani_data->signal_code;

					int LeftTurn = 0, Thru = 0, RightTurn = 0, DiagTurn = 0;
					bool Amber = false;
					DecodeFTCSignalCode(curSigCode, LeftTurn, Thru, RightTurn, DiagTurn, Amber);

					if (Amber)
					{
						// find the previous interval?
						//curInterval -= 1;
						//if (curInterval < 0) curInterval = temp_data[isig].active_intervals - 1;
						//curSigCode = cur_data->signal_code[curInterval][iapp];
						int curSigCode = ani_data->prev_signal_code;
						DecodeFTCSignalCode(curSigCode, LeftTurn, Thru, RightTurn, DiagTurn, Amber);

						pLink->leftTurnCode = GenFTCCodeAtAmber(LeftTurn);
						pLink->thruCode = GenFTCCodeAtAmber(Thru);
						pLink->rightTurnCode = GenFTCCodeAtAmber(RightTurn);
						pLink->leftDiagCode = GenFTCCodeAtAmber(DiagTurn);
						pLink->rightDiagCode = GenFTCCodeAtAmber(DiagTurn);
					} else
					{
						if ((LeftTurn == S_GREEN) && m_AniSLinkSet[linkIdx].opposeNode != 0)
						{
						
							// method 1: find signal code of opposing link in current FTC signal record
							// because of shared dsn; 
							// but what if tmpAmber is true? should look at the previous interval too?
							/*
							int opposeApp = 0;
							for (; opposeApp < cur_data->approaches; ++opposeApp)
							{
								if (cur_data->approach[opposeApp] == m_AniSLinkSet[pLink->linkID - 1].opposeNode)
									break;
							}
							curSigCode = cur_data->signal_code[curInterval][opposeApp];
							int opposeThru = 0;
							int tmp1 = 0, tmp2 = 0, tmp3 = 0;
							bool tmpAmber = false;
							DecodeFTCSignalCode(curSigCode, tmp1, opposeThru, tmp2, tmp3, tmpAmber);
							if (opposeThru != S_RED) LeftTurn = S_PERGRN;
							*/

							// method 2: find signal code of opposig link in corresponding animation_data record
							// but need to confirm the accuracy
							// more accurate because there may be external control
							
							std::pair<int, int> opNodePair(m_AniSLinkSet[linkIdx].opposeNode, dsnID);
							if (m_UsnDsnToSLinkIDMap.find(opNodePair) != m_UsnDsnToSLinkIDMap.end()) 
							{
								int opLinkID = m_UsnDsnToSLinkIDMap[opNodePair];
								ANIMATION_DATA* op_ani_data = linkIDToAniDataMap.find(opLinkID)->second;
								curSigCode = op_ani_data->signal_code;
								int opposeThru = 0;
								int tmp1 = 0, tmp2 = 0, tmp3 = 0;
								DecodeFTCSignalCode(curSigCode, tmp1, opposeThru, tmp2, tmp3, Amber);
								if (Amber)
								{
									curSigCode = op_ani_data->prev_signal_code;
									DecodeFTCSignalCode(curSigCode, tmp1, opposeThru, tmp2, tmp3, Amber);
								}
								if (opposeThru != S_RED) LeftTurn = S_PERGRN;
							}
						}
						pLink->leftTurnCode = LeftTurn;
						pLink->thruCode = Thru;
						pLink->rightTurnCode = RightTurn;
						pLink->leftDiagCode = DiagTurn;
						pLink->rightDiagCode = DiagTurn;
					}
				}
			}
		}	
	}
	free(temp_data);
}

//No longer used
void etFommInterface::GenACSignalCodes(const std::map<int, ANIMATION_DATA*>& linkIDToAniDataMap, 
		const std::map<int, AniLinkData*>& LinkIDToLinkData)
{
	// should get AC_INPUTS array, and use it to determine whether there is an actuated control on the link, too
	int n_acs = GET_NUMBER_OF_AC_SIGNALS();
	if (n_acs <= 0) return;

	AC_INPUTS* temp_data = (AC_INPUTS*)calloc(n_acs, sizeof(AC_INPUTS));
	int status = GET_AC_SIGNALS(temp_data);
	CheckStatus("GET_AC_SIGNALS", status);

	for (int isig = 0; isig < n_acs; ++isig)
	{
		AC_INPUTS* cur_data = &(temp_data[isig]);
		for (int nodeid = 0; nodeid++; nodeid < 8)
		{

			int dsnID = cur_data->node[nodeid];
			for (int iapp = 0; iapp < cur_data->n_direct_approaches; ++iapp)
			{
				int usnID = cur_data->direct_approach_USN[iapp];
				std::pair<int, int> nodePair(usnID, dsnID);
				if (m_UsnDsnToSLinkIDMap.find(nodePair) == m_UsnDsnToSLinkIDMap.end()) continue;
				int linkID = m_UsnDsnToSLinkIDMap[nodePair];
				int linkIdx = linkID - 1;
				if (LinkIDToLinkData.find(linkID) != LinkIDToLinkData.end())
				{
					ANIMATION_DATA* ani_data = linkIDToAniDataMap.find(linkID)->second;
					AniLinkData* pLink = LinkIDToLinkData.find(linkID)->second;

					pLink->signalizedFlag = 1;
					pLink->thruCode = GenACCode(ani_data->amber_thru, ani_data->signal_thru);
					pLink->rightTurnCode = GenACCode(ani_data->amber_right, ani_data->signal_right);
					pLink->leftDiagCode = GenACCode(ani_data->amber_diag, ani_data->signal_diag);
					pLink->rightDiagCode = GenACCode(ani_data->amber_diag, ani_data->signal_diag);
					pLink->leftTurnCode = GenACCode(ani_data->amber_left, ani_data->signal_left);

					if ((pLink->leftTurnCode == S_GREEN) && (m_AniSLinkSet[linkIdx].opposeNode != 0))
					{
						for (int nodeid = 0; nodeid++; nodeid < 8)
						{

							std::pair<int, int> opNodePair(m_AniSLinkSet[linkIdx].opposeNode, cur_data->node[nodeid]);
							if (m_UsnDsnToSLinkIDMap.find(opNodePair) != m_UsnDsnToSLinkIDMap.end())
							{
								int opLinkID = m_UsnDsnToSLinkIDMap[opNodePair];
								if (linkIDToAniDataMap.find(opLinkID)->second->signal_thru)
									pLink->leftTurnCode = S_PERGRN;
							}
						}
					}
				}
			}
		}
	}
	free(temp_data);
}

void etFommInterface::DecodeFTCSignalCode(int CurCode, int& LeftTurn, int& Thru, int& RightTurn, int& DiagTurn, bool& Amber)
{
	if (CurCode == 0)
	{
		Amber = true;
	} else
	{
		Amber = false;
		
		switch (CurCode)
		{
		case 1: 
			LeftTurn = S_GREEN;
			Thru = S_GREEN;
			RightTurn = S_GREEN;
			DiagTurn = S_GREEN;
			break;
		case 2: 
			LeftTurn = S_RED;
			Thru = S_RED;
			RightTurn = S_RED;
			DiagTurn = S_RED;
			break;
		case 3: 
			LeftTurn = S_RED;
			Thru = S_RED;
			RightTurn = S_GREEN;
			DiagTurn = S_RED;
			break;
		case 4: 
			LeftTurn = S_GREEN;
			Thru = S_RED;
			RightTurn = S_RED;
			DiagTurn = S_RED;
			break;
		case 5: 
			LeftTurn = S_STOP;
			Thru = S_STOP;
			RightTurn = S_STOP;
			DiagTurn = S_STOP;
			break;
		case 6: 
			LeftTurn = S_RED;
			Thru = S_RED;
			RightTurn = S_RED;
			DiagTurn = S_GREEN;
			break;
		case 7: 
			LeftTurn = S_RED;
			Thru = S_GREEN;
			RightTurn = S_RED;
			DiagTurn = S_RED;
			break;
		case 8: 
			LeftTurn = S_GREEN;
			Thru = S_RED;
			RightTurn = S_GREEN;
			DiagTurn = S_RED;
			break;
		case 9: 
			LeftTurn = S_RED;
			Thru = S_GREEN;
			RightTurn = S_GREEN;
			DiagTurn = S_GREEN;
			break;
		}
	}
}

void etFommInterface::WriteAnimatorFiles()
{
	std::string setName;
	if (m_nrun == 1)	
		setName = DefaultTrfFile.substr(0, DefaultTrfFile.length() - 4);
	else
		setName = DefaultTrfFile.substr(0, DefaultTrfFile.length() - 4) + "_Run" + std::to_string(m_nrun);
	WriteLinkData(setName);
	WriteLocationFile(setName);
	WriteVehicleData(setName);
	WriteVehicleList(setName);
	WriteNodes(setName);
	WriteLinks(setName);
	WriteRampMeterData(setName);
}



void etFommInterface::WriteLocationFile(std::string setName)
{
	std::ofstream LocFile(setName + "_LOCATION.TXT");
	for (std::vector<AniLocation>::iterator iLoc = m_AniLocationSet.begin();
		iLoc != m_AniLocationSet.end(); ++iLoc)
	{
		LocFile << iLoc->usn << ",";
		LocFile << iLoc->dsn << ",";
		LocFile << iLoc->distUpStopBar2USN << ",";
		LocFile << iLoc->distDownStopBar2DSN << ",";
		LocFile << iLoc->distSign2DSN << ",";
		LocFile << iLoc->distCurb2Sign << ",";
		LocFile << iLoc->signType << "\n";
	}
	LocFile.close();
}

void etFommInterface::WriteLinkData(std::string setName)
{
	std::ofstream DataFile(setName + "_LINK_DATA.TXT");
	std::sort(m_AniLinkDataSet.begin(), m_AniLinkDataSet.end(), 
		&etFommInterface::compareLinkData);
	for (std::vector<AniLinkData>::iterator pDocLink = m_AniLinkDataSet.begin(); 
		pDocLink != m_AniLinkDataSet.end(); ++pDocLink)
	{
		DataFile 
			<< pDocLink->	timeStep 	 << ","
			<< pDocLink->	networkCode 	 << ","
			<< pDocLink->	usn 	 << ","
			<< pDocLink->	dsn 	 << ","
			<< pDocLink->	numberOfVehicles 	 << ","
			<< pDocLink->	leftTurnCode 	 << ","
			<< pDocLink->	leftDiagCode 	 << ","
			<< pDocLink->	thruCode 	 << ","
			<< pDocLink->	rightDiagCode 	 << ","
			<< pDocLink->	rightTurnCode 	 << ","
			<< pDocLink->	signalizedFlag 	 << ","
			<< pDocLink->	numberOfRampMeters 	 << ","	
			<< pDocLink->	rampMeterSignalState
			<< "\n";
	}

	DataFile.close();
}

void etFommInterface::WriteVehicleData(std::string setName)
{
	std::ofstream DataFile(setName + "_VEHICLE_DATA.TXT");
	std::ofstream DataOrigFile(setName + "_VEHICLE_DATA_Orig.TXT");
	std::sort(m_AniVehicleDataSet.begin(), m_AniVehicleDataSet.end(), 
		&etFommInterface::compareVehData);
	for (std::vector<AniVehicleData>::iterator pDocVehData = m_AniVehicleDataSet.begin(); 
		pDocVehData != m_AniVehicleDataSet.end(); ++pDocVehData)
	{
		if (pDocVehData->acceleration < -128 || pDocVehData->acceleration > 127)
		{
			DataOrigFile 
			<< pDocVehData->	timeStep 	<< ","
			<< pDocVehData->	usn 	<< ","
			<< pDocVehData->	dsn 	<< ","
			<< pDocVehData->	vehicleID 	<< ","
			<< pDocVehData->	laneID 	<< ","
			<< pDocVehData->	distFromUSN 	<< ","
			<< pDocVehData->	prevLinkUSN 	<< ","
			<< pDocVehData->	turnCode 	<< ","
			<< pDocVehData->	queueCode 	<< ","
			<< pDocVehData->	acceleration 	<< ","
			<< pDocVehData->	velocity 	<< ","
			<< pDocVehData->	laneChgStatus 	<< ","
			<< pDocVehData->	targetedLaneID 	<< ","
			<< pDocVehData->	destination 	<< ","
			<< pDocVehData->	leaderID 	<< ","
			<< pDocVehData->	followerID 	<< ","
			<< pDocVehData->	prevLinkLaneID 
			<< "\n";

			pDocVehData->acceleration = char(pDocVehData->acceleration);
		}

		DataFile 
			<< pDocVehData->	timeStep 	<< ","
			<< pDocVehData->	usn 	<< ","
			<< pDocVehData->	dsn 	<< ","
			<< pDocVehData->	vehicleID 	<< ","
			<< pDocVehData->	laneID 	<< ","
			<< pDocVehData->	distFromUSN 	<< ","
			<< pDocVehData->	prevLinkUSN 	<< ","
			<< pDocVehData->	turnCode 	<< ","
			<< pDocVehData->	queueCode 	<< ","
			<< pDocVehData->	acceleration 	<< ","
			<< pDocVehData->	velocity 	<< ","
			<< pDocVehData->	laneChgStatus 	<< ","
			<< pDocVehData->	targetedLaneID 	<< ","
			<< pDocVehData->	destination 	<< ","
			<< pDocVehData->	leaderID 	<< ","
			<< pDocVehData->	followerID 	<< ","
			<< pDocVehData->	prevLinkLaneID 
			<< "\n";

		
	}

	DataFile.close();
	DataOrigFile.close();
}

void etFommInterface::WriteVehicleList(std::string setName)
{
	std::ofstream DataFile(setName + "_VEHICLE_LIST.TXT");
	std::sort(m_AniVehicleList.begin(), m_AniVehicleList.end(), &etFommInterface::compareVehList);
	for (std::vector<AniVehicle>::iterator pDocVeh = m_AniVehicleList.begin(); 
		pDocVeh != m_AniVehicleList.end(); ++pDocVeh)
	{
		DataFile 
			<< pDocVeh->	vehicleID 	 << ","
			<< pDocVeh->	entryNodeID 	 << ","
			<< pDocVeh->	fleetID 	 << ","
			<< pDocVeh->	vehicleTypeCode 	 << ","
			<< pDocVeh->	vehicleLength 	 << ","
			<< pDocVeh->	driverTypeCode 
			<< "\n";
	}

	DataFile.close();
}

void etFommInterface::WriteNodes(std::string setName)
{
	std::ofstream DataFile(setName + "_NETWORK_NODES.TXT");
	//DataFile << std::setprecision(7) << std::fixed;
	for (std::vector<AniNode>::iterator pDocNode = m_AniNodeSet.begin(); 
		pDocNode != m_AniNodeSet.end(); ++pDocNode)
	{
		DataFile 
			<< pDocNode->	nodeID 	<< ","
			<< std::fixed << std::setprecision(0) << pDocNode->	x 	<< ","
			<< std::fixed << std::setprecision(0) << pDocNode->	y    	<< ","
			<< pDocNode->	networkCode 	<< ","
			<< "0,"
			<< std::fixed << std::setprecision(7) << pDocNode->	latitude 	<< ","
			<< std::fixed << std::setprecision(7) << pDocNode->	longitude 	<< ","
			<< pDocNode->	altitude 
			<< "\n";
	}

	DataFile.close();
}

void etFommInterface::WriteLinks(std::string setName)
{
	std::ofstream DataFile(setName + "_NETWORK_LINKS.TXT");
	for (std::vector<AniFreewayLink>::iterator pDocLink = m_AniFLinkSet.begin(); 
		pDocLink != m_AniFLinkSet.end(); ++pDocLink)
	{
		if (pDocLink->dsn >= 7000 && pDocLink->dsn <= 7999)
			pDocLink->linkType = -1;
		DataFile 
			<< pDocLink->	usn 	 << ","
			<< pDocLink->	dsn 	 << ","
			<< pDocLink->	length 	 << ","
			<< pDocLink->	numOfFullLanes 	 << ","
			<< pDocLink->	thruDSN	 << ","
			<< pDocLink->	offRampDSN	 << ","
			<< pDocLink->	linkType	 << ","
			<< pDocLink->	thruAlignLane 	 << ","
			<< pDocLink->	offrampAlignLane 	 << ",";

		for (int i = 0;  i< 6; ++i)
		{
			DataFile
				<< pDocLink->	auxLanes[i].laneID	 << ","
				<< pDocLink->	auxLanes[i].code	 << ","
				<< pDocLink->	auxLanes[i].length	 << ",";
		}

		for (int i = 0;  i< 3; ++i)
		{
			DataFile
				<< pDocLink->	addDropLanes[i].code	 << ","
				<< pDocLink->	addDropLanes[i].laneID 	 << ","
				<< pDocLink->	addDropLanes[i].dist 	;

			if (i < 2)
				DataFile << ",";
		}

		DataFile	<< "\n";
	}

	for (std::vector<AniSurfaceLink>::iterator pDocLink = m_AniSLinkSet.begin(); 
		pDocLink != m_AniSLinkSet.end(); ++pDocLink)
	{
		DataFile 
			<< pDocLink->	usn 	 << ","
			<< pDocLink->	dsn 	 << ","
			<< pDocLink->	length 	 << ","
			<< pDocLink->	numOfFullLanes 	 << ","
			<< pDocLink->	alignLane 	 << ","
			<< pDocLink->	thruAlignLane 	 << ","
			<< pDocLink->	numOfLeftPockets 	 << ","
			<< pDocLink->	lenOfLeftPockets 	 << ","
			<< pDocLink->	numOfRightPockets 	 << ","
			<< pDocLink->	lenOfRightPockets 	 << ","
			<< pDocLink->	leftDSN 	 << ","
			<< pDocLink->	thruDSN 	 << ","
			<< pDocLink->	rightDSN 	 << ","
			<< pDocLink->	diagDSN << ",";

		for (int i = 0; i < 4; ++i)
		{
			DataFile	<<  "0";
			if (i < 3)
				DataFile	<<  ",";
		}
		
		DataFile	<< "\n";
	}

	DataFile.close();
}

void etFommInterface::WriteRampMeterData(std::string setName)
{
	std::ofstream DataFile(setName + "_RAMPMETER_DATA.TXT");
	for (std::vector<AniLinkData>::iterator pDocRMData = m_AniRampMeterDataSet.begin(); 
		pDocRMData != m_AniRampMeterDataSet.end(); ++pDocRMData)
	{
		DataFile 
			<< pDocRMData->	timeStep 	 << ","
			<< pDocRMData->	networkCode 	 << ","
			<< pDocRMData->	usn 	 << ","
			<< pDocRMData->	dsn 	 << ","
			<< pDocRMData->	numberOfVehicles 	 << ","
			<< pDocRMData->	leftTurnCode 	 << ","
			<< pDocRMData->	leftDiagCode 	 << ","
			<< pDocRMData->	thruCode 	 << ","
			<< pDocRMData->	rightDiagCode 	 << ","
			<< pDocRMData->	rightTurnCode 	 << ","
			<< pDocRMData->	signalizedFlag 	 << ","
			<< pDocRMData->	numberOfRampMeters 	 << ","
			<< pDocRMData->	rampMeterSignalState 
			<< "\n";
	}

	DataFile.close();
}

bool etFommInterface::compareLinkData(const AniLinkData& LinkA, const AniLinkData& LinkB)
{
	if (LinkA.timeStep < LinkB.timeStep)
	{
		return true;
	} else if (LinkA.timeStep > LinkB.timeStep)
	{
		return false;
	} else
	{
		if (LinkA.usn < LinkB.usn)
		{
			return true;
		} else if (LinkA.usn > LinkB.usn)
		{
			return false;
		} else
		{
			if (LinkA.dsn < LinkB.dsn)
			{
				return true;
			} else //if (LinkA.dsn >= LinkB.dsn)
			{
				return false;
			} 
		}
	}
}

bool etFommInterface::compareVehData(const AniVehicleData& VehA, const AniVehicleData& VehB)
{
	if (VehA.timeStep < VehB.timeStep)
	{
		return true;
	} else if (VehA.timeStep > VehB.timeStep)
	{
		return false;
	} else
	{
		if (VehA.usn < VehB.usn)
		{
			return true;
		} else if (VehA.usn > VehB.usn)
		{
			return false;
		} else
		{
			if (VehA.dsn < VehB.dsn)
			{
				return true;
			} else if (VehA.dsn > VehB.dsn)
			{
				return false;
			} else
			{
				if (VehA.laneID < VehB.laneID)
				{
					return true;
				} else if (VehA.laneID > VehB.laneID)
				{
					return false;
				} else
				{
					if (VehA.distFromUSN < VehB.distFromUSN)
					{
						return true;
					} else 
					{
						return false;
					}
				}
			}
		}
	}
}

bool etFommInterface::compareVehList(const AniVehicle& VehA, const AniVehicle& VehB)
{
	if (VehA.vehicleID < VehB.vehicleID)
	{
		return true;
	} else //if (VehA.vehicleID >= VehB.vehicleID)
	{
		return false;
	}
}
void etFommInterface::GenGPSCoordinates(NODE_LOCATION_DATA *node_location_inputs)
{
	RefNode1.x = node_location_inputs[RefNode1.is_defined - 1].x;
	RefNode1.y = node_location_inputs[RefNode1.is_defined - 1].y;

	RefNode2.x = node_location_inputs[RefNode2.is_defined - 1].x;
	RefNode2.y = node_location_inputs[RefNode2.is_defined - 1].y;
	
	float m_XScale = 1;
	float m_YScale = 1;

	if(RefNode1.x - RefNode2.x)
	{
		m_XScale = (RefNode1.longitude - RefNode2.longitude)/(RefNode1.x - RefNode2.x);
	}

	if(RefNode1.y - RefNode2.y)
	{
		m_YScale = (RefNode1.latitude - RefNode2.latitude)/(RefNode1.y - RefNode2.y);
	}

	float m_XOrigin = RefNode1.x - RefNode1.longitude /m_XScale;

	float m_YOrigin = RefNode1.y - RefNode1.latitude /m_YScale;

	// adjust node coordinates
	int n_nodelocations = 8999;
	for (int iNode = 0; iNode < n_nodelocations; ++iNode)
	{
		NODE_LOCATION_DATA *cur_data = &(node_location_inputs[iNode]);
		cur_data->longitude = (cur_data->x - m_XOrigin) * m_XScale;
		cur_data->latitude = (cur_data->y - m_YOrigin) * m_YScale;
		cur_data->elevation = RefNode1.elevation;
	}
}

void etFommInterface::SetGPSRefNodes(const std::string& GPSRefFile)
{
	std::ifstream inputFile(GPSRefFile);
	if(inputFile.is_open() == false)
	{
		throw(InterfaceException("ERROR: Failed to open GPS reference file " + GPSRefFile));
	}

	int iRefNode = 1;
	while(!inputFile.eof() && iRefNode <= 2)
	{
		std::vector<std::string> lineData;
		char line[255];
		std::streamsize count = 255;
		inputFile.getline(line, count, '\n');
		char* pointer = strtok(line, ",");
		while(pointer)
		{
			lineData.push_back(std::string(pointer));
			pointer = strtok(NULL,",");
		}

		if (lineData.size() != 4)
		{
			throw(InterfaceException("ERROR: Wrong format of GPS reference file"));
		}

		NODE_LOCATION_DATA refNode;
		refNode.is_defined = std::stoi(lineData[0]);
		refNode.latitude = std::stof(lineData[1]);
		refNode.longitude = std::stof(lineData[2]);
		refNode.elevation = std::stoi(lineData[3]);

		if (iRefNode == 1)
			RefNode1 = refNode;
		else
			RefNode2 = refNode;
		
		iRefNode++;
	}
	inputFile.close();
}

void etFommInterface::SetGPSRefNodes(NODE_LOCATION_DATA *GPSRefNodes, int n_gps_ref_nodes)
{
	if (n_gps_ref_nodes ==  2)
	{
		RefNode1 = GPSRefNodes[0];
		RefNode2 = GPSRefNodes[1];
	}
}

//Not Reading _SCOPE_INPUTS.TXT any more
int etFommInterface::ReadSCOPEInputs(AC_INPUTS *ac_signal_data, int nac)
{
	std::string setName = DefaultTrfFile.substr(0, DefaultTrfFile.length() - 4) + "_SCOPE_Inputs.txt";
	std::ifstream inputFile(setName.c_str());
	if(inputFile.is_open() == false)
	{
		// No SCOPE_Input file
		return 1;
	}

	int lineNumber = 0;
	while(!inputFile.eof())
	{
		lineNumber++;
		std::vector<std::string> lineData;
		char line[255];
		std::streamsize count = 255;
		inputFile.getline(line, count, '\n');
		char* pointer = strtok(line, " ,");
		while(pointer)
		{
			lineData.push_back(std::string(pointer));
			pointer = strtok(NULL," ,");
		}

		if (lineNumber > 1)
		{
			if (lineData.size() == 0)//==0 for the last line without any char
			{
				continue;
			}
			if (lineData.size() != 12)
			{
				// Wrong Format
				return 2;
			}
			int iac = stoi(lineData[0]) - 1;
			int iPhase = stoi(lineData[1]) - 1;
			ac_signal_data[iac].actuated_mode[iPhase] = stoi(lineData[1]); //TODO double check
			ac_signal_data[iac].actuated_mode[iPhase] = stoi(lineData[2]); //TODO double check same variable?
			ac_signal_data[iac].min_green_time[iPhase] = stof(lineData[3]);
			ac_signal_data[iac].max_green_time[iPhase] = stof(lineData[4]);
			ac_signal_data[iac].default_extension_time[iPhase] = stof(lineData[5]);
			ac_signal_data[iac].gap_time[iPhase] = stof(lineData[6]);
			ac_signal_data[iac].times_before_reduction[iPhase] = stof(lineData[7]);
			ac_signal_data[iac].time_to_reduce[iPhase] = stof(lineData[8]);
			ac_signal_data[iac].min_gap_time[iPhase] = stof(lineData[9]);
			ac_signal_data[iac].yellow_change_int[iPhase] = stof(lineData[10]);
			ac_signal_data[iac].red_clear_int[iPhase] = stof(lineData[11]);
		} else
		{
			if (lineData.size() != 2)
			{
				// Wrong Format
				return 2;
			}
			int nac_read = stoi(lineData[0]);
			if (nac_read != nac)
			{
				// Wrong Number of AC Signals
				return 3;
			}
		}
	}
	inputFile.close();	
	return 0;
}

//Added 4/29/15
int etFommInterface::GetControllerID(int node, int *iact)
{
	//input node, return iact
	//determine the ID of the actuated controller at the node
	m_status = GET_CONTROLLER_ID(node, iact);
	CheckStatus("GET_CONTROLLER_ID", m_status);
	return m_status;
}
int etFommInterface::GetLocalCycleTimer(int node, float *local_cycle_timer)
{
	//input node, return local_cycle_timer
	//get the current value of the local cycle timer
	m_status = GET_LOCAL_CYCLE_TIMER(node, local_cycle_timer);
	CheckStatus("GET_LOCAL_CYCLE_TIMER", m_status);
	return m_status;
}
int etFommInterface::GetCycleLength(int node, float *cycle_length)
{
	//input node, return cycle_length
	//get the current cycle length
	m_status = GET_CYCLE_LENGTH(node, cycle_length);
	CheckStatus("GET_CYCLE_LENGTH", m_status);
	return m_status;
}
int etFommInterface::SetNewCycleLength(int node, float cycle_length)
{
	//input node, set cycle_length
	//set the cycle length that will be implemented at the next transition
	m_status = SET_NEW_CYCLE_LENGTH(node, cycle_length);
	CheckStatus("SET_NEW_CYCLE_LENGTH", m_status);
	return m_status;
}
int etFommInterface::GetNewCycleLength(int node, float *cycle_length)
{
	//input node, return cycle_length
	//get the cycle length that will be implemented at the next transition
	m_status = GET_NEW_CYCLE_LENGTH(node, cycle_length);
	CheckStatus("GET_NEW_CYCLE_LENGTH", m_status);
	return m_status;
}
int etFommInterface::GetOffset(int node, float *offset)
{
	//input node, return offset
	//get the current offset
	m_status = GET_OFFSET(node, offset);
	CheckStatus("GET_OFFSET", m_status);
	return m_status;
}
int etFommInterface::SetNewOffset(int node, float offset)
{
	//input node, set offset
	//set the offset that will be implemented at the next transition
	m_status = SET_NEW_OFFSET(node, offset);
	CheckStatus("SET_NEW_OFFSET", m_status);
	return m_status;
}
int etFommInterface::GetNewOffset(int node, float *offset)
{
	//input node, return offset
	//get the offset that will be implemented at the next transition
	m_status = GET_NEW_OFFSET(node, offset);
	CheckStatus("GET_NEW_OFFSET", m_status);
	return m_status;
}
int etFommInterface::GetTransitionMethod(int node, int *method, float *max_add, float *max_subt)
{
	//get the current transition method
	// Returns: int =  0 - short way transition method
	//              =  1 - dwell transition method
	//              =  2 - add transition method
	//              =  3 - subtract transition method
	//              = -1 - specified node is not under actuated control
	m_status = GET_TRANSITION_METHOD(node, method, max_add, max_subt);
	CheckStatus("GET_TRANSITION_METHOD", m_status);
	return m_status;
}
int etFommInterface::SetTransitionMethod(int node, int method, float max_add, float max_subt)
{
	//set the method for the next transition
	m_status = SET_TRANSITION_METHOD(node, method, max_add, max_subt);
	CheckStatus("SET_TRANSITION_METHOD", m_status);
	return m_status;
}
int etFommInterface::GetSplits(int node, float *splits)
{
	//get the current splits
	m_status = GET_SPLITS(node, splits);
	CheckStatus("GET_SPLITS", m_status);
	return m_status;
}
int etFommInterface::GetMinSplits(int node, float *splits)
{
	//get the minimum allowable splits
	m_status = GET_MIN_SPLITS(node, splits);
	CheckStatus("GET_MIN_SPLITS", m_status);
	return m_status;
}
int etFommInterface::SetNewSplits(int node, float *new_splits)
{
	//set the splits that will be implemented at the next transition
	m_status = SET_NEW_SPLITS(node, new_splits);
	CheckStatus("SET_NEW_SPLITS", m_status);
	return m_status;
}
int etFommInterface::GetNewSplits(int node, float *new_splits)
{
	//get the splits that will be implemented at the next transition
	m_status = GET_NEW_SPLITS(node, new_splits);
	CheckStatus("GET_NEW_SPLITS", m_status);
	return m_status;
}
float etFommInterface::GetStreetTravelTime(int usn_id, int dsn_id)
{
	float average_travel_time = GET_STREET_TRAVEL_TIME(usn_id, dsn_id);
	//CheckStatus("GET_STREET_TRAVEL_TIME", m_status);
	return average_travel_time;
}
//void etFommInterface::GetSplitsFlags(int *splits_flag)
//{
//	GET_SPLITS_FLAGS(splits_flag);
//}
int etFommInterface::GetETFOMMPhaseStates(int iact, int* greens, int* yellows)
{
#if UnifiedDLL
	m_status = GET_ETFOMM_PHASE_STATES(iact, greens, yellows);
#else
	m_status = GET_SCOPE_PHASE_STATES(iact, greens, yellows);
#endif
	return m_status;
}
int etFommInterface::SetETFOMMPhaseStates(int iact, int* greens, int* yellows)
{
	m_status = SET_ETFOMM_PHASE_STATES(iact, greens, yellows);
	CheckStatus("SET_ETFOMM_PHASE_STATES", m_status);
	return m_status;
}
int etFommInterface::GetPhaseCalls(int iact, int* phase_calls)
{
	m_status = GET_PHASE_CALLS(iact, phase_calls);
	CheckStatus("GET_PHASE_CALLS", m_status);
	return m_status;
}

void etFommInterface::GetRoundabouts(RABT_API_DATA* roundabout_inputs)
{
	m_status = GET_ROUNDABOUTS(roundabout_inputs);
	CheckStatus("GET_ROUNDABOUTS", m_status);
}

int etFommInterface::GetNumberOfTurningWays()
{
	//LZ for the tiime being return 0
	// 
	/*
	try {
		GET_NUMBER_OF_TURNING_WAYS();
	}
		catch (...)
		{
			throw;
		return 0;
	};
	return GET_NUMBER_OF_TURNING_WAYS();
	*/
	return 0;
	  
}

int  etFommInterface::GetTurningWays(TURNING_WAY* turningway_inputs)
{
	m_status = GET_RTURNING_WAYS(turningway_inputs);
	CheckStatus("GET_RTURNING_WAYS", m_status);
	return m_status;
}

void etFommInterface::PrintFVehicleData(std::ostream &outFile, int n_fvehicles, VFData *fvehicle_data)
{
	outFile << "FREEWAY_VEHICLE: " << n_fvehicles << "\n";
	
	for(int i = 0; i < n_fvehicles; i++)
	{
		outFile << "#" << i+1 << "\n";
		outFile << "acceleration = " << fvehicle_data[i].acceleration << "\n";
		outFile << "decel = " << fvehicle_data[i].decel << "\n";
		outFile << "desiredspeed = " << fvehicle_data[i].desiredspeed << "\n";
		outFile << "disch_timer = " << fvehicle_data[i].disch_timer << "\n";
		outFile << "drivertype = " << fvehicle_data[i].drivertype << "\n";
		outFile << "entry_link = " << fvehicle_data[i].entry_link << "\n";
		outFile << "entrytime = " << fvehicle_data[i].entrytime << "\n";
		outFile << "ev_dist = " << fvehicle_data[i].ev_dist << "\n";
		outFile << "ev_ovrspd = " << fvehicle_data[i].ev_ovrspd << "\n";
		outFile << "ev_range = " << fvehicle_data[i].ev_range << "\n";
		outFile << "ev_rand = " << fvehicle_data[i].ev_rand << "\n";

		outFile << "ev_wait_timer = " << fvehicle_data[i].ev_wait_timer << "\n";
		outFile << "ev_watch = " << fvehicle_data[i].ev_watch << "\n";
		outFile << "fleet = " << fvehicle_data[i].fleet << "\n";
		outFile << "go_thru_signal = " << fvehicle_data[i].go_thru_signal << "\n";
		outFile << "lag_timer = " << fvehicle_data[i].lag_timer << "\n";
		outFile << "lane = " << fvehicle_data[i].lane << "\n";
			
		outFile << "lanecodes:";
		for (int j = 0; j < MAX_FLANE_CODES; j++)
		{
			if (fvehicle_data[i].lanecodes[j] != 0)
			    outFile << "\t" << fvehicle_data[i].lanecodes[j];
		}
		outFile << "\n";

		outFile << "link = " << fvehicle_data[i].link << "\n";
		outFile << "follower = " << fvehicle_data[i].follower << "\n";
		outFile << "id = " << fvehicle_data[i].id << "\n";
		outFile << "last_detid = " << fvehicle_data[i].last_detid << "\n";
		outFile << "lc_timer = " << fvehicle_data[i].lc_timer << "\n";
		outFile << "leader = " << fvehicle_data[i].leader << "\n";
		outFile << "location = " << fvehicle_data[i].location << "\n";
		outFile << "pathid = " << fvehicle_data[i].pathid << "\n";
		outFile << "pathpoint = " << fvehicle_data[i].pathpoint << "\n";
		outFile << "saved_path = " << fvehicle_data[i].saved_path << "\n";
		outFile << "pseudo_leader = " << fvehicle_data[i].pseudo_leader << "\n";
		outFile << "prev_accel = " << fvehicle_data[i].prev_accel << "\n";
		outFile << "prevlink = " << fvehicle_data[i].prevlink << "\n";
		outFile << "prevlane = " << fvehicle_data[i].prevlane << "\n";
		outFile << "routeid = " << fvehicle_data[i].routeid << "\n";
		outFile << "speed = " << fvehicle_data[i].speed << "\n";

		outFile << "speed_adj = " << fvehicle_data[i].speed_adj << "\n";
		outFile << "turncode = " << fvehicle_data[i].turncode << "\n";
		outFile << "vlength = " << fvehicle_data[i].vlength << "\n";
		outFile << "vtype = " << fvehicle_data[i].vtype << "\n";
		outFile << "xcode = " << fvehicle_data[i].xcode << "\n";
		outFile << "will_coop_ev = " << fvehicle_data[i].will_coop_ev << "\n";
		outFile << "will_coop_lc = " << fvehicle_data[i].will_coop_lc << "\n";
		outFile << "will_move = " << fvehicle_data[i].will_move << "\n";

		outFile << "destination = " << fvehicle_data[i].destination << "\n";
		outFile << "distance_to_segment_end = " << fvehicle_data[i].distance_to_segment_end << "\n";
		outFile << "diverted = " << fvehicle_data[i].diverted << "\n";
		outFile << "hov_violator = " << fvehicle_data[i].hov_violator << "\n";
		outFile << "imeter = " << fvehicle_data[i].imeter << "\n";	
		outFile << "incident_num = " << fvehicle_data[i].incident_num << "\n";	
		outFile << "isegment = " << fvehicle_data[i].isegment << "\n";	
		outFile << "must_merge = " << fvehicle_data[i].must_merge << "\n";	
		outFile << "next_object = " << fvehicle_data[i].next_object << "\n";
		outFile << "remaining_dist = " << fvehicle_data[i].remaining_dist << "\n";
		outFile << "sorted_list = " << fvehicle_data[i].sorted_list << "\n";
		outFile << "sort_position = " << fvehicle_data[i].sort_position << "\n";
			
		outFile << "\n";
	}
	
	outFile << "\n";
}

void etFommInterface::PrintSVehicleData(std::ostream &outFile, int n_svehicles, VSData *svehicle_data)
{
	outFile << "STREET_VEHICLE: " << n_svehicles << "\n";
	
	for(int i = 0; i < n_svehicles; i++)
	{
		outFile << "#" << i+1 << "\n";
		outFile << "acceleration = " << svehicle_data[i].acceleration << "\n";
		outFile << "decel = " << svehicle_data[i].decel << "\n";
		outFile << "desiredspeed = " << svehicle_data[i].desiredspeed << "\n";
		outFile << "disch_timer = " << svehicle_data[i].disch_timer << "\n";
		outFile << "drivertype = " << svehicle_data[i].drivertype << "\n";
		outFile << "entry_link = " << svehicle_data[i].entry_link << "\n";
		outFile << "entrytime = " << svehicle_data[i].entrytime << "\n";
		outFile << "ev_dist = " << svehicle_data[i].ev_dist << "\n";
		outFile << "ev_ovrspd = " << svehicle_data[i].ev_ovrspd << "\n";
		outFile << "ev_range = " << svehicle_data[i].ev_range << "\n";
		outFile << "ev_rand = " << svehicle_data[i].ev_rand << "\n";

		outFile << "ev_wait_timer = " << svehicle_data[i].ev_wait_timer << "\n";
		outFile << "ev_watch = " << svehicle_data[i].ev_watch << "\n";
		outFile << "fleet = " << svehicle_data[i].fleet << "\n";
		outFile << "go_thru_signal = " << svehicle_data[i].go_thru_signal << "\n";
		outFile << "lag_timer = " << svehicle_data[i].lag_timer << "\n";
		outFile << "lane = " << svehicle_data[i].lane << "\n";
			
		outFile << "lanecodes:";
		for (int j = 0; j < MAX_SLANE_CODES; j++)
		{
			if (svehicle_data[i].lanecodes[j] != 0)
			    outFile << "\t" << svehicle_data[i].lanecodes[j];
		}
		outFile << "\n";

		outFile << "link = " << svehicle_data[i].link << "\n";
		outFile << "follower = " << svehicle_data[i].follower << "\n";
		outFile << "id = " << svehicle_data[i].id << "\n";
		outFile << "last_detid = " << svehicle_data[i].last_detid << "\n";
		outFile << "lc_timer = " << svehicle_data[i].lc_timer << "\n";
		outFile << "leader = " << svehicle_data[i].leader << "\n";
		outFile << "location = " << svehicle_data[i].location << "\n";
		outFile << "pathid = " << svehicle_data[i].pathid << "\n";
		outFile << "pathpoint = " << svehicle_data[i].pathpoint << "\n";
		outFile << "saved_path = " << svehicle_data[i].saved_path << "\n";
		outFile << "pseudo_leader = " << svehicle_data[i].pseudo_leader << "\n";
		outFile << "prev_accel = " << svehicle_data[i].prev_accel << "\n";
		outFile << "prevlink = " << svehicle_data[i].prevlink << "\n";
		outFile << "prevlane = " << svehicle_data[i].prevlane << "\n";
		outFile << "routeid = " << svehicle_data[i].routeid << "\n";
		outFile << "speed = " << svehicle_data[i].speed << "\n";

		outFile << "speed_adj = " << svehicle_data[i].speed_adj << "\n";
		outFile << "start_lag = " << svehicle_data[i].start_lag << "\n";
		outFile << "turncode = " << svehicle_data[i].turncode << "\n";
		outFile << "vlength = " << svehicle_data[i].vlength << "\n";
		outFile << "vtype = " << svehicle_data[i].vtype << "\n";
		outFile << "xcode = " << svehicle_data[i].xcode << "\n";
		outFile << "will_coop_ev = " << svehicle_data[i].will_coop_ev << "\n";
		outFile << "will_coop_lc = " << svehicle_data[i].will_coop_lc << "\n";
		outFile << "will_move = " << svehicle_data[i].will_move << "\n";
		outFile << "diverted = " << svehicle_data[i].diverted << "\n";

		outFile << "dwell_timer = " << svehicle_data[i].dwell_timer << "\n";
		outFile << "goal_lane = " << svehicle_data[i].goal_lane << "\n";
		outFile << "has_stopped = " << svehicle_data[i].has_stopped << "\n";
		outFile << "ispdicd = " << svehicle_data[i].ispdicd << "\n";
		outFile << "next_stop = " << svehicle_data[i].next_stop << "\n";
		outFile << "prvdist = " << svehicle_data[i].prvdist << "\n";

		outFile << "prvlink = " << svehicle_data[i].prvlink << "\n";
		outFile << "prvlnkicd = " << svehicle_data[i].prvlnkicd << "\n";
		outFile << "qstate = " << svehicle_data[i].qstate << "\n";
		outFile << "turn_code = " << svehicle_data[i].turn_code << "\n";
		outFile << "turn_code2 = " << svehicle_data[i].turn_code2 << "\n";
		outFile << "turn_link = " << svehicle_data[i].turn_link << "\n";
		outFile << "turn_link2 = " << svehicle_data[i].turn_link2 << "\n";

		outFile << "vehicd = " << svehicle_data[i].vehicd << "\n";
		outFile << "will_jump = " << svehicle_data[i].will_jump << "\n";
		outFile << "will_yield = " << svehicle_data[i].will_yield << "\n";
					
		outFile << "\n";
	}
	
	outFile << "\n";
}

void etFommInterface::PrintFreewayLinkData(std::ostream &outFile, int n_freeway_links, FREEWAY_LINK *freeway_link_data)
{
	outFile << "FREEWAY_LINK: " << n_freeway_links << "\n";
	
	for(int il = 0; il < n_freeway_links; il++)
	{
		outFile << "#" << il+1 << "\n";
		outFile << "id = " << freeway_link_data[il].ID << "\n";
		outFile << "usn = " << freeway_link_data[il].usn << "\n";
		outFile << "dsn = " << freeway_link_data[il].dsn << "\n";
		outFile << "linktype = " << freeway_link_data[il].linktype << "\n";
		outFile << "thrunode = " << freeway_link_data[il].thrunode << "\n";
		outFile << "mainline_sending_lane = " << freeway_link_data[il].mainline_sending_lane << "\n";
		outFile << "mainline_receiving_lane = " << freeway_link_data[il].mainline_receiving_lane << "\n";
		outFile << "offramp_sending_lane = " << freeway_link_data[il].offramp_sending_lane << "\n";
		outFile << "offramp_receiving_lane = " << freeway_link_data[il].offramp_receiving_lane << "\n";
		outFile << "exitnode = " << freeway_link_data[il].exitnode << "\n";
		outFile << "length = " << freeway_link_data[il].length << "\n";
		outFile << "fulllanes = " << freeway_link_data[il].fulllanes << "\n";
		
		for (int iadl = 0; iadl < MAX_ADDDROP_LANE; ++iadl)
		{
			if (freeway_link_data[il].adddrop_code[iadl] != 0)
			{
				outFile << "add drop lane #" << iadl + 1 << ":\t";
				outFile << "adddrop_code = " << freeway_link_data[il].adddrop_code[iadl] << "\t";
				outFile << "adddrop_lane = " << freeway_link_data[il].adddrop_lane[iadl] << "\t";
				outFile << "adddrop_dist = " << freeway_link_data[il].adddrop_dist[iadl] << "\t";
				outFile << "adddrop_warn = " << freeway_link_data[il].adddrop_warn[iadl] << "\n";
			}
		}

		for(int iaux = 0; iaux < N_AUXLANES; ++iaux)
		{
			if(freeway_link_data[il].auxlaneid[iaux] != 0)
			{
				outFile << "auxiliary lane #" << iaux + 1 << ":\t";
				outFile << "auxlaneid = " << freeway_link_data[il].auxlaneid[iaux] << "\t";
				outFile << "auxlanecode = " << freeway_link_data[il].auxlanecode[iaux] << "\t";
				outFile << "auxlanelength = " << freeway_link_data[il].auxlanelength[iaux] << "\n";
				
			}
		}

		outFile << "freeflowspeed = " << freeway_link_data[il].freeflowspeed << "\n";
		outFile << "thru_percent = " << freeway_link_data[il].thru_percent << "\n";
		outFile << "offramp_warn_distance = " << freeway_link_data[il].offramp_warn_distance << "\n";
		outFile << "anticip_warning_distance = " << freeway_link_data[il].anticip_warning_distance << "\n";
		outFile << "anticip_warning_speed = " << freeway_link_data[il].anticip_warning_speed << "\n";
		outFile << "grade = " << freeway_link_data[il].grade << "\n";
		outFile << "pavement = " << freeway_link_data[il].pavement << "\n";

		outFile << "nhov_lanes = " << freeway_link_data[il].nhov_lanes << "\n";
		outFile << "hov_begin = " << freeway_link_data[il].hov_begin << "\n";
		outFile << "hov_end = " << freeway_link_data[il].hov_end << "\n";
		outFile << "hov_code = " << freeway_link_data[il].hov_code << "\n";
		outFile << "hov_offramp_warn_distance = " << freeway_link_data[il].hov_offramp_warn_distance << "\n";
		outFile << "hov_side = " << freeway_link_data[il].hov_side << "\n";
		outFile << "hov_type = " << freeway_link_data[il].hov_type << "\n";
		outFile << "hov_warn = " << freeway_link_data[il].hov_warn << "\n";
		outFile << "hov_pct = " << freeway_link_data[il].hov_pct << "\n";
		outFile << "cfmult = " << freeway_link_data[il].cfmult << "\n";
		outFile << "first_detector = " << freeway_link_data[il].first_detector << "\n";
		outFile << "tilt = " << freeway_link_data[il].tilt << "\n";
		outFile << "curve = " << freeway_link_data[il].curve << "\n";
		outFile << "shoulder_width = " << freeway_link_data[il].shoulder_width << "\n";
		outFile << "lane_width:";
		for(int ilan = 0; ilan < MAX_FREEWAY_LANE_WIDTH; ++ilan)
		{
			if (freeway_link_data[il].lane_width[ilan] != 0)
			    outFile  << "\t" << freeway_link_data[il].lane_width[ilan];				
		}
		outFile << "\n";

		outFile << "barrier:";
		for(int ibar = 0; ibar < MAX_BARRIER; ++ibar)
		{
			if (freeway_link_data[il].barrier[ibar] != 0)
			    outFile << "\t" << freeway_link_data[il].barrier[ibar];				
		}
		outFile << "\n";
		outFile << "datastation_id = " << freeway_link_data[il].datastation_id << "\n";
		outFile << "datastation_location = " << freeway_link_data[il].datastation_location << "\n";
		outFile << "truck_code = " << freeway_link_data[il].truck_code << "\n";
		outFile << "truck_dir = " << freeway_link_data[il].truck_dir << "\n";
		outFile << "truck_lane = " << freeway_link_data[il].truck_lane << "\n";
		outFile << "etl_warn = " << freeway_link_data[il].etl_warn << "\n";

		outFile << "exclude_type:\n";
		
		for (int itype = 0; itype < NTYPES; ++itype)
		{
			bool printendofline= false;
			for (int idx = 0; idx < TO_BE_RENAMED1; ++idx)
			{
				if (freeway_link_data[il].exclude_type[itype][idx] != 0)
				{
					outFile  << "\t" << freeway_link_data[il].exclude_type[itype][idx];
					printendofline = true;
				}
			}
			if (printendofline) outFile << "\n";
		}
		outFile << "multiplier_exit:";
		for (int itype = 0; itype < NTYPES; ++itype)
		{
			if (freeway_link_data[il].multiplier_exit[itype] != 0)
				outFile  << "\t" << freeway_link_data[il].multiplier_exit[itype];
		}
		outFile << "\n";
		outFile << "startup_time = " << freeway_link_data[il].startup_time << "\n";
		outFile << "\n";
	}
	
	outFile << "\n";
}

void etFommInterface::PrintStreetLinkData(std::ostream &outFile, int n_street_links, STREET_LINK *street_link_data)
{
	outFile << "STREET_LINK: " << n_street_links << "\n";
	
	for(int i = 0; i < n_street_links; i++)
	{
		outFile << "#" << i+1 << "\n";
		outFile << "id = " << street_link_data[i].id << "\n";
		outFile << "usn = " << street_link_data[i].usn << "\n";
		outFile << "dsn = " << street_link_data[i].dsn << "\n";
		outFile << "thrunode = " << street_link_data[i].thrunode << "\n";
		outFile << "leftnode = " << street_link_data[i].leftnode << "\n";
		outFile << "rightnode = " << street_link_data[i].rightnode << "\n";
		outFile << "diagnode = " << street_link_data[i].diagnode << "\n";
		outFile << "opposenode = " << street_link_data[i].opposenode << "\n";
		outFile << "length = " << street_link_data[i].length << "\n";
		outFile << "fulllanes = " << street_link_data[i].fulllanes << "\n";
		outFile << "leftturnbays = " << street_link_data[i].leftturnbays << "\n";
		outFile << "rightturnbays = " << street_link_data[i].rightturnbays << "\n";
		//outFile << "lengthofleftbay = " << street_link_data[i].lengthofleftbay << "\n";
		//outFile << "lengthofrightbay = " << street_link_data[i].lengthofrightbay << "\n";
		outFile << "freeflowspeed = " << street_link_data[i].freeflowspeed << "\n";
				
		outFile << "channelization:";
		for (int j = 0; j < MAX_CHANNELIZATION; ++j)
		{
			outFile << "\t" << street_link_data[i].channelization[j];
		}
		outFile << "\n";

		outFile << "leftpct = " << street_link_data[i].leftpct << "\n";
		outFile << "thrupct = " << street_link_data[i].thrupct << "\n";
		outFile << "rightpct = " << street_link_data[i].rightpct << "\n";
		outFile << "diagpct = " << street_link_data[i].diagpct << "\n";
		outFile << "grade = " << street_link_data[i].grade << "\n";
		outFile << "distribution_code = " << street_link_data[i].distribution_code << "\n";
		outFile << "startup_delay = " << street_link_data[i].startup_delay << "\n";
		outFile << "discharge_hdwy = " << street_link_data[i].discharge_hdwy << "\n";
		outFile << "rtor = " << street_link_data[i].rtor << "\n";
		outFile << "ped_code = " << street_link_data[i].ped_code << "\n";
		outFile << "lane1 = " << street_link_data[i].lane1 << "\n";
		outFile << "lane2 = " << street_link_data[i].lane2 << "\n";
		outFile << "cfmult = " << street_link_data[i].cfmult << "\n";
		outFile << "sight_dist = " << street_link_data[i].sight_dist << "\n";
		outFile << "first_detector = " << street_link_data[i].first_detector << "\n";
		outFile << "shoulder_width = " << street_link_data[i].shoulder_width << "\n";

		outFile << "lane_width:";
		for (int j = 0; j < MAX_STREET_LANE_WIDTH; ++j)
		{
			if (street_link_data[i].lane_width[j] != 0)
			    outFile << "\t" << street_link_data[i].lane_width[j];
		}
		outFile << "\n";

		outFile << "ste_freq = " << street_link_data[i].ste_freq << "\n";
		outFile << "ste_duration = " << street_link_data[i].ste_duration << "\n";
		outFile << "signal_range = " << street_link_data[i].signal_range << "\n";
		outFile << "centroid = " << street_link_data[i].centroid << "\n";
		outFile << "centroid_label = " << street_link_data[i].centroid_label << "\n";
		
		outFile << "exclude_type:\n";
		for (int itype = 0; itype < NTYPES; ++itype)
		{
			bool printendofline = false;
			for (int idx = 0; idx < TO_BE_RENAMED2; ++idx)
			{
				if (street_link_data[i].exclude_type[itype][idx] != 0)
				{
					outFile  << "\t" << street_link_data[i].exclude_type[itype][idx];
					printendofline = true;
				}
			}
			if (printendofline) outFile << "\n";
		}
		outFile << "\n";
	}
	
	outFile << "\n";
}

void etFommInterface::PrintACSignals(std::ostream &outFile, int n_ac, AC_INPUTS *ac_signal_data)
{
	outFile << "AC SIGNAL: " << n_ac << "\n";

	for(int i = 0; i < n_ac; i++)
	{
		outFile << "#" << i+1 << "\n";
		AC_INPUTS* cur_data = &(ac_signal_data[i]);
		outFile << "	node 	" << cur_data->	node 	<< "\n";
		outFile << "	cfails 	" << cur_data->	cfails 	<< "\n";
		outFile << "	adj 	" << cur_data->adj 	<< "\n";
		outFile << "	cycle_length 	" << cur_data->cycle_length 	<< "\n";
		outFile << "	offset 	" << cur_data->offset 	<< "\n";
		outFile << "	n_direct_approaches 	" << cur_data->n_direct_approaches 	<< "\n";
		for (int iapp = 0; iapp < cur_data->n_direct_approaches; ++iapp)
		{
			outFile << "		upnode " << cur_data->direct_approach_USN[iapp] << "\n";
		}

		outFile << "	Ring Phases	" ;
		for (int i = 0; i < 2; ++i)
		{
			for (int j = 0; j < 4; ++j)
			{
				outFile << cur_data->ring_phase[j + i * 4] ;
			}
			outFile << "	";
		}
		outFile << "\n";

		for (int ip = 0; ip < 8; ++ip)
		{
			outFile << "	phase #" << ip+1 << "\n";
			outFile << "		actuated_mode  	" << cur_data->	actuated_mode  	[ip]	<< "\n";
			outFile << "		min_green_time  	" << cur_data->	min_green_time  	[ip]	<< "\n";
			outFile << "		max_green_time  	" << cur_data->	max_green_time  	[ip]	<< "\n";
			outFile << "		default_extension_time  	" << cur_data->	default_extension_time  	[ip]	<< "\n";
			outFile << "		gap_time  	" << cur_data->	gap_time  	[ip]	<< "\n";
			outFile << "		times_before_reduction  	" << cur_data->	times_before_reduction  	[ip]	<< "\n";
			outFile << "		time_to_reduce  	" << cur_data->	time_to_reduce  	[ip]	<< "\n";
			outFile << "		min_gap_time  	" << cur_data->	min_gap_time  	[ip]	<< "\n";
			outFile << "		yellow_change_int  	" << cur_data->	yellow_change_int  	[ip]	<< "\n";
			outFile << "		red_clear_int  	" << cur_data->	red_clear_int  	[ip]	<< "\n";
			//outFile << "		ped_allowed  	" << cur_data->	ped_allowed  	[ip]	<< "\n";  disabled changed by LZ 06/16/2024
			outFile << "		walk_time  	" << cur_data->	walk_time  	[ip]	<< "\n";
			outFile << "		walk_clearance_time	" << cur_data->	walk_clearance_time	[ip]	<< "\n";

			outFile << "		leftarrow   	";
			for (int iapp = 0; iapp <cur_data->n_direct_approaches; ++iapp)
			{
				outFile << cur_data->leftarrow[iapp][ip];
			}
			outFile << "\n";

			outFile << "		thruarrow   	";
			for (int iapp = 0; iapp <cur_data->n_direct_approaches; ++iapp)
			{
				outFile << cur_data->thruarrow[iapp][ip];

			}
			outFile << "\n";

			outFile << "		rightarrow   	";
			for (int iapp = 0; iapp <cur_data->n_direct_approaches; ++iapp)
			{
				outFile << cur_data->rightarrow[iapp][ip];

			}
			outFile << "\n";

			outFile << "		diagarrow   	";
			for (int iapp = 0; iapp <cur_data->n_direct_approaches; ++iapp)
			{
				outFile << cur_data->diagarrow[iapp][ip];

			}
			outFile << "\n";

		}
		outFile << "\n";
	}
	outFile << "\n";
}

void etFommInterface::PrintFTCSignals(std::ostream &outFile, int n_ftc, FTC_DATA *ftc_signal_data)
{
	outFile << "FTC SIGNAL: " << n_ftc << "\n";
	for(int i = 0; i < n_ftc; i++)
	{
		outFile << "#" << i+1 << "\n";
		outFile << "approaches = " << ftc_signal_data[i].approaches << "\n";
		outFile << "approach:";
		for (int j = 0; j < ftc_signal_data[i].approaches; ++j)
		{
			outFile << "\t" << ftc_signal_data[i].approach[j];
		}
		outFile << "\n";
		
		outFile << "active_intervals = " << ftc_signal_data[i].active_intervals << "\n";
		outFile << "duration:";
		for (int j = 0; j < ftc_signal_data[i].active_intervals; ++j)
		{
			outFile << "\t" << ftc_signal_data[i].duration[j];
		}
		outFile << "\n";

		outFile << "signal_code:\n";
		for (int j = 0; j < ftc_signal_data[i].active_intervals; ++j)
		{
			outFile << "\tduration" << j+1 << ":";
			for (int k = 0; k < ftc_signal_data[i].approaches; ++k)
			{
				outFile << "\t" << ftc_signal_data[i].signal_code[j][k];
			}
			outFile << "\n";
		}
		
		outFile << "current_interval = " << ftc_signal_data[i].current_interval << "\n";
		outFile << "cycle_length = " << ftc_signal_data[i].cycle_length << "\n";
		outFile << "external_control = " << ftc_signal_data[i].external_control << "\n";
		outFile << "node = " << ftc_signal_data[i].node << "\n";
		outFile << "offset = " << ftc_signal_data[i].offset << "\n";
		outFile << "range = " << ftc_signal_data[i].range << "\n";
		outFile << "time_in_interval = " << ftc_signal_data[i].time_in_interval << "\n";
		outFile << "\n";
	}
	outFile << "\n";
}

void etFommInterface::PrintEntryNodes(std::ostream &outFile, int n_entrynodes, ENTRYNODES_DATA *entrynode_data)
{
	outFile << "ENTRY NODES: " << n_entrynodes << "\n";
	for(int i = 0; i < n_entrynodes; i++)
	{
		outFile << "#" << i+1 << "\n";
		outFile << "Node_ID = " << entrynode_data[i].Node_ID << "\n";
		outFile << "truck_pct = " << entrynode_data[i].truck_pct << "\n";
		outFile << "carpool_pct = " << entrynode_data[i].carpool_pct << "\n";
		outFile << "flowrate = " << entrynode_data[i].flowrate << "\n";
		outFile << "hov_violators_per10000 = " << entrynode_data[i].hov_violators_per10000 << "\n";
		outFile << "lane_pct:";
		for (int j = 0; j < N_ENTRYLANES; ++j)
		{
			if (entrynode_data[i].lane_pct[j] != 0)
		        outFile << "\t" << entrynode_data[i].lane_pct[j];
		}
		outFile << "\n";
		outFile << "\n";
	}
	outFile << "\n";
}

void etFommInterface::PrintRampMeter(std::ostream &outFile, int n_rampmeters, RM_DATA *rampmeter_data)
{
	outFile << "RAMP METERS: " << n_rampmeters << "\n";
	for(int i = 0; i < n_rampmeters; i++)
	{
		outFile << "#" << i+1 << "\n";
		outFile << "dsn = " << rampmeter_data[i].dsn << "\n";
		outFile << "link = " << rampmeter_data[i].link << "\n";
		outFile << "control = " << rampmeter_data[i].control << "\n";
		outFile << "onset = " << rampmeter_data[i].onset << "\n";
		outFile << "state = " << rampmeter_data[i].state << "\n";

		outFile << "detector:";
		for (int j = 0; j < DETECTOR; ++j)
		{
			if (rampmeter_data[i].detector[j] != 0)
		        outFile << "\t" << rampmeter_data[i].detector[j];
		}
		outFile << "\n";

		outFile << "capacity = " << rampmeter_data[i].capacity << "\n";

		outFile << "speed:";
		for (int j = 0; j < SPEED; ++j)
		{
			if (rampmeter_data[i].speed[j] != 0)
		        outFile << "\t" << rampmeter_data[i].speed[j];
		}
		outFile << "\n";

		outFile << "headway:";
		for (int j = 0; j < HEADWAY; ++j)
		{
			if (rampmeter_data[i].headway[j] != 0)
		        outFile << "\t" << rampmeter_data[i].headway[j];
		}
		outFile << "\n";
		
		outFile << "timer = " << rampmeter_data[i].timer << "\n";
		outFile << "updint = " << rampmeter_data[i].updint << "\n";
		outFile << "twopergreen = " << rampmeter_data[i].twopergreen << "\n";

		outFile << "\n";
	}
	outFile << "\n";
}

void etFommInterface::PrintNetworkInputs(std::ostream &outFile, const NETWORK_INPUTS &Network_Inputs)
{
	outFile << "NETWORK INPUTS:" << "\n";

	//outFile << "run_init = " << ((Network_Inputs.run_init) ? "true" : "false") << "\n";
	outFile << "initialization_end = " << Network_Inputs.initialization_end << "\n";
	outFile << "time_interval = " << Network_Inputs.time_interval << "\n";
	outFile << "timestep = " << Network_Inputs.timestep << "\n";
	outFile << "type_of_run = " << Network_Inputs.type_of_run << "\n";
	outFile << "time_period_duration:" ;

	for (int i =0; i < MAX_TIME_PERIOD_DURATION; ++i)
	{
		if (Network_Inputs.time_period_duration[i] != 0 )
		    outFile << "\t" << Network_Inputs.time_period_duration[i];
	}
	outFile << "\n";
	outFile << "\n";
}

void etFommInterface::PrintFNetworkInputs(std::ostream &outFile, const FREEWAY_NETWORK_INPUTS &FNetwork_Inputs)
{
	outFile << "FREEWAY NETWORK INPUTS:" << "\n";

	outFile << "cfrict:" ;
	for (int i =0; i < CFRICT; ++i)
	{
		if (FNetwork_Inputs.cfrict[i] != 0 )
		    outFile << "\t" << FNetwork_Inputs.cfrict[i];
	}
	outFile << "\n";

	outFile << "default_hov_pct = " << FNetwork_Inputs.default_hov_pct << "\n";
	outFile << "lag_accel = " << FNetwork_Inputs.lag_accel << "\n";
	outFile << "lag_decel = " << FNetwork_Inputs.lag_decel << "\n";

	outFile << "ffspeed_adj:" ;
	for (int i =0; i < MAX_FFSPEED_ADJ; ++i)
	{
		if (FNetwork_Inputs.ffspeed_adj[i] != 0 )
		    outFile << "\t" << FNetwork_Inputs.ffspeed_adj[i];
	}
	outFile << "\n";

	outFile << "zfoll:" ;
	for (int i =0; i < ZFOLL; ++i)
	{
		//if (FNetwork_Inputs.zfoll[i] != 0 )
		//    outFile << "\t" << FNetwork_Inputs.zfoll[i];
	}
	outFile << "\n";

	outFile << "freeway_pct_coop = " << FNetwork_Inputs.freeway_pct_coop << "\n";
	outFile << "lc_time = " << FNetwork_Inputs.lc_time << "\n";
	
	outFile << "\n";
}

void etFommInterface::PrintSNetworkInputs(std::ostream &outFile, const STREET_NETWORK_INPUTS &SNetwork_Inputs)
{
	outFile << "STREET NETWORK INPUTS:" << "\n";

	outFile << "additional_gap:" ;
	for (int i =0; i < ADDITIONAL_GAP; ++i)
	{
		//if (SNetwork_Inputs.additional_gap[i] != 0 )
		    outFile << "\t" << SNetwork_Inputs.additional_gap[i];
	}
	outFile << "\n";

	outFile << "amber_decel:" ;
	for (int i =0; i < AMBER_DECEL; ++i)
	{
		//if (SNetwork_Inputs.amber_decel[i] != 0 )
		    outFile << "\t" << SNetwork_Inputs.amber_decel[i];
	}
	outFile << "\n";

	outFile << "pdelay_weak:" ;
	for (int i =0; i < PDELAY_WEAK; ++i)
	{
		//if (SNetwork_Inputs.pdelay_weak[i] != 0 )
		    outFile << "\t" << SNetwork_Inputs.pdelay_weak[i];
	}
	outFile << "\n";

	outFile << "pdelay_strong:" ;
	for (int i =0; i < PDELAY_STRONG; ++i)
	{
		//if (SNetwork_Inputs.pdelay_strong[i] != 0 )
		    outFile << "\t" << SNetwork_Inputs.pdelay_strong[i];
	}
	outFile << "\n";

	outFile << "ped_duration:" ;
	for (int i =0; i < PED_DURATION; ++i)
	{
		//if (SNetwork_Inputs.ped_duration[i] != 0 )
		    outFile << "\t" << SNetwork_Inputs.ped_duration[i];
	}
	outFile << "\n";

	outFile << "acceptable_gap:" ;
	for (int i =0; i < ACCEPTABLE_GAP; ++i)
	{
		//if (SNetwork_Inputs.acceptable_gap[i] != 0 )
		    outFile << "\t" << SNetwork_Inputs.acceptable_gap[i];
	}
	outFile << "\n";

	outFile << "acceptable_ltg:" ;
	for (int i =0; i < ACCEPTABLE_LTG; ++i)
	{
		//if (SNetwork_Inputs.acceptable_ltg[i] != 0 )
		    outFile << "\t" << SNetwork_Inputs.acceptable_ltg[i];
	}
	outFile << "\n";

	outFile << "acceptable_rtg:" ;
	for (int i =0; i < ACCEPTABLE_RTG; ++i)
	{
		//if (SNetwork_Inputs.acceptable_rtg[i] != 0 )
		    outFile << "\t" << SNetwork_Inputs.acceptable_rtg[i];
	}
	outFile << "\n";

	outFile << "dwell_multiplier:" << "\n";
	for (int i =0; i < DWELL_MULTIPLIER; ++i)
	{
		for (int j = 0; j < TO_BE_RENAMED3; ++j)
		{
		   // if (SNetwork_Inputs.dwell_multiplier[i][j] != 0 )
		        outFile << "\t" << SNetwork_Inputs.dwell_multiplier[i][j];
		}
		outFile << "\n";
	}
	
	outFile << "ffspeed_adj:" ;
	for (int i =0; i < MAX_FFSPEED_ADJ; ++i)
	{
		//if (SNetwork_Inputs.ffspeed_adj[i] != 0 )
		    outFile << "\t" << SNetwork_Inputs.ffspeed_adj[i];
	}
	outFile << "\n";

	outFile << "zfoll:" ;
	for (int i =0; i < ZFOLL; ++i)
	{
		//if (SNetwork_Inputs.zfoll[i] != 0 )
		    //outFile << "\t" << SNetwork_Inputs.zfoll[i];
	}
	outFile << "\n";

	outFile << "lc_time = " << SNetwork_Inputs.lc_time << "\n";

	outFile << "lt_jumper_prob:" ;
	for (int i =0; i < LT_JUMPER_PROB; ++i)
	{
		//if (SNetwork_Inputs.lt_jumper_prob[i] != 0 )
		    outFile << "\t" << SNetwork_Inputs.lt_jumper_prob[i];
	}
	outFile << "\n";

	outFile << "lt_lagger_prob:" ;
	for (int i =0; i < LT_LAGGER_PROB; ++i)
	{
		//if (SNetwork_Inputs.lt_lagger_prob[i] != 0 )
		    outFile << "\t" << SNetwork_Inputs.lt_lagger_prob[i];
	}
	outFile << "\n";

	outFile << "spillback_prob:" ;
	for (int i =0; i < SPILLBACK_PROB; ++i)
	{
		//if (SNetwork_Inputs.spillback_prob[i] != 0 )
		    outFile << "\t" << SNetwork_Inputs.spillback_prob[i];
	}
	outFile << "\n";

	outFile << "stop_spd = " << SNetwork_Inputs.stop_spd << "\n";
	outFile << "street_pct_coop = " << SNetwork_Inputs.street_pct_coop << "\n";
	outFile << "yield_spd = " << SNetwork_Inputs.yield_spd << "\n";
	outFile << "driver_fampct = " << SNetwork_Inputs.driver_fampct << "\n";

	outFile << "qfactor:" ;
	for (int i =0; i < QFACTOR; ++i)
	{
		//if (SNetwork_Inputs.qfactor[i] != 0 )
		    outFile << "\t" << SNetwork_Inputs.qfactor[i];
	}
	outFile << "\n";

	outFile << "ste_mult:" ;
	for (int i =0; i < STE_MULT; ++i)
	{
		//if (SNetwork_Inputs.ste_mult[i] != 0 )
		    outFile << "\t" << SNetwork_Inputs.ste_mult[i];
	}
	outFile << "\n";

	outFile << "\n";
}

void etFommInterface::PrintVTypeInputs(std::ostream &outFile, int n_vehicletype, VEHICLE_TYPE_DATA *Vehicle_Type_Inputs)
{
	outFile << "VEHICLE TYPE INPUTS: " << n_vehicletype << "\n";

	for (int i = 0; i < n_vehicletype; ++i)
	{
		outFile << "#" << i+1 << "\n";
		outFile << "length = " << Vehicle_Type_Inputs[i].length << "\n";
		outFile << "headway_factor = " << Vehicle_Type_Inputs[i].headway_factor << "\n";
		outFile << "average_occupancy = " << Vehicle_Type_Inputs[i].average_occupancy << "\n";
		outFile << "fleet_freeway_auto = " << Vehicle_Type_Inputs[i].fleet_freeway_auto << "\n";
		outFile << "fleet_freeway_truck = " << Vehicle_Type_Inputs[i].fleet_freeway_truck << "\n";
		outFile << "fleet_freeway_carpool = " << Vehicle_Type_Inputs[i].fleet_freeway_carpool << "\n";
		outFile << "fleet_freeway_bus = " << Vehicle_Type_Inputs[i].fleet_freeway_bus << "\n";
		outFile << "fleet_freeway_ev = " << Vehicle_Type_Inputs[i].fleet_freeway_ev << "\n";
		outFile << "fleet_street_auto = " << Vehicle_Type_Inputs[i].fleet_street_auto << "\n";
		outFile << "fleet_street_truck = " << Vehicle_Type_Inputs[i].fleet_street_truck << "\n";
		outFile << "fleet_street_carpool = " << Vehicle_Type_Inputs[i].fleet_street_carpool << "\n";
		outFile << "fleet_street_bus = " << Vehicle_Type_Inputs[i].fleet_street_bus << "\n";
		outFile << "fleet_street_ev = " << Vehicle_Type_Inputs[i].fleet_street_ev << "\n";
		outFile << "fleet_street_bike = " << Vehicle_Type_Inputs[i].fleet_street_bike << "\n";
		outFile << "\n";
	}

	outFile << "\n";
}

void etFommInterface::PrintDetectorInputs(std::ostream &outFile, const std::string &dtype,
						 int n_dets, DETECTOR_INPUTS *Detector_Inputs)
{
	outFile << dtype << " DETECTOR INPUTS: " << n_dets << "\n";

	for (int i = 0; i < n_dets; ++i)
	{
		outFile << "#" << i+1 << "\n";
		outFile << "usn = " << Detector_Inputs[i].usn << "\n";
		outFile << "dsn = " << Detector_Inputs[i].dsn << "\n";
		outFile << "associated_phase = " << Detector_Inputs[i].associated_phase << "\n";
		outFile << "station_id = " << Detector_Inputs[i].station_id << "\n";
		outFile << "location = " << Detector_Inputs[i].location << "\n";
		outFile << "link = " << Detector_Inputs[i].link << "\n";
		//output two lanes only! LZ 06/02/2024
		outFile << "lane1 = " << Detector_Inputs[i].detector_lanes[0] << "\n";
		outFile << "lane2 = " << Detector_Inputs[i].detector_lanes[1] << "\n";
		outFile << "zone_length = " << Detector_Inputs[i].zone_length << "\n";
		outFile << "delay_time = " << Detector_Inputs[i].delay_time << "\n";
		outFile << "carryover_time = " << Detector_Inputs[i].carryover_time << "\n";
		outFile << "type_code = " << Detector_Inputs[i].type_code << "\n";
		outFile << "operation_code = " << Detector_Inputs[i].operation_code << "\n";
		outFile << "detection_zone = " << Detector_Inputs[i].detection_zone << "\n";
		
		outFile << "\n";
	}

	outFile << "\n";
}

void etFommInterface::PrintCondTurnpctInputs(std::ostream &outFile, int n_turnpct, COND_TURNPCTS *cond_turnpct_data)
{
	outFile << "COND_TURNPCTS: " << n_turnpct << "\n";

	for (int i = 0; i < n_turnpct; ++i)
	{
		outFile << "#" << i+1 << "\n";
		outFile << "USN = " << cond_turnpct_data[i].USN << "\n";
		outFile << "DSN = " << cond_turnpct_data[i].DSN << "\n";
		
		outFile << "LEFTPCT:" ;
		for (int j = 0; j < MAX_LEFTPCT; ++j) 
		{
			outFile << "\t" << cond_turnpct_data[i].LEFTPCT[j];
		}
		outFile << "\n";

		outFile << "THRUPCT:" ;
		for (int j = 0; j < MAX_THRUPCT; ++j) 
		{
			outFile << "\t" << cond_turnpct_data[i].THRUPCT[j];
		}
		outFile << "\n";

		outFile << "RIGHTPCT:" ;
		for (int j = 0; j < MAX_RIGHTPCT; ++j) 
		{
			outFile << "\t" << cond_turnpct_data[i].RIGHTPCT[j];
		}
		outFile << "\n";

		outFile << "DIAGPCT:" ;
		for (int j = 0; j < MAX_DIAGPCT; ++j) 
		{
			outFile << "\t" << cond_turnpct_data[i].DIAGPCT[j];
		}
		outFile << "\n";

		outFile << "\n";
	}

	outFile << "\n";
}

void etFommInterface::PrintBusRouteInputs(std::ostream &outFile, int n_busroute, BUSROUTE_DATA *busroute_inputs)
{
	outFile << "BUSROUTE_DATA: " << n_busroute << "\n";

	for (int i = 0; i < n_busroute; ++i)
	{
		outFile << "#" << i+1 << "\n";
		outFile << "number = " << busroute_inputs[i].number << "\n";
		outFile << "hdwy = " << busroute_inputs[i].hdwy << "\n";
		outFile << "offset = " << busroute_inputs[i].offset << "\n";
		outFile << "nodes = " << busroute_inputs[i].nodes << "\n";
		
		outFile << "route_nodes:" ;
		for (int j = 0; j < ROUTE_NODES; ++j) 
		{
			if (busroute_inputs[i].route_nodes[j] != 0)
			    outFile << "\t" << busroute_inputs[i].route_nodes[j];
		}
		outFile << "\n";

		outFile << "stationlist:" ;
		for (int j = 0; j < STATIONLIST; ++j) 
		{
			if (busroute_inputs[i].stationlist[j] != 0)
			    outFile << "\t" << busroute_inputs[i].stationlist[j];
		}
		outFile << "\n";

		outFile << "persontrips = " << busroute_inputs[i].persontrips << "\n";
		outFile << "timer = " << busroute_inputs[i].timer << "\n";
		outFile << "traveltime = " << busroute_inputs[i].traveltime << "\n";
		outFile << "trips = " << busroute_inputs[i].trips << "\n";
		outFile << "\n";
	}

	outFile << "\n";
}

void etFommInterface::PrintBusStationInputs(std::ostream &outFile, int n_busstation, BUSSTATION_DATA *busstation_inputs)
{
	outFile << "BUSSTATION_DATA: " << n_busstation << "\n";

	for (int i = 0; i < n_busstation; ++i)
	{
		outFile << "#" << i+1 << "\n";
		outFile << "block_code = " << busstation_inputs[i].block_code << "\n";
		outFile << "usn = " << busstation_inputs[i].usn << "\n";
		outFile << "dsn = " << busstation_inputs[i].dsn << "\n";
		outFile << "capacity = " << busstation_inputs[i].capacity << "\n";
		outFile << "location = " << busstation_inputs[i].location << "\n";
		outFile << "dwell = " << busstation_inputs[i].dwell << "\n";
		outFile << "bypass_pct = " << busstation_inputs[i].bypass_pct << "\n";
		outFile << "next_station = " << busstation_inputs[i].next_station << "\n";
		outFile << "pocket_lane = " << busstation_inputs[i].pocket_lane << "\n";
		outFile << "front = " << busstation_inputs[i].front << "\n";
		outFile << "count = " << busstation_inputs[i].count << "\n";
		outFile << "dwell_time = " << busstation_inputs[i].dwell_time << "\n";
		outFile << "empty_time = " << busstation_inputs[i].empty_time << "\n";
		outFile << "overflow_time = " << busstation_inputs[i].overflow_time << "\n";
		
		outFile << "\n";
	}

	outFile << "\n";
}

void etFommInterface::PrintIncidentInputs(std::ostream &outFile, int n_incident, INCIDENT_DATA *incident_inputs)
{
	outFile << "INCIDENT_DATA: " << n_incident << "\n";

	for (int i = 0; i < n_incident; ++i)
	{
		outFile << "#" << i+1 << "\n";
		outFile << "usn = " << incident_inputs[i].usn << "\n";
		outFile << "dsn = " << incident_inputs[i].dsn << "\n";
		outFile << "begin_point = " << incident_inputs[i].begin_point << "\n";
		outFile << "begin_time = " << incident_inputs[i].begin_time << "\n";
		outFile << "end_point = " << incident_inputs[i].end_point << "\n";
		outFile << "end_time = " << incident_inputs[i].end_time << "\n";
		outFile << "rbnf = " << incident_inputs[i].rbnf << "\n";
		outFile << "warn_point = " << incident_inputs[i].warn_point << "\n";

		outFile << "code:" ;
		for (int j = 0; j < CODE; ++j) 
		{
			if (incident_inputs[i].code[j] != 0)
			    outFile << "\t" << incident_inputs[i].code[j];
		}
		outFile << "\n";
		
		outFile << "\n";
	}

	outFile << "\n";
}

void etFommInterface::PrintCoordInputs(std::ostream &outFile, int n_coords, NODE_LOCATION_DATA *xy_coord_inputs)
{
	outFile << "NODE_LOCATION_DATA: " << n_coords << "\n";

	for (int i = 0; i < n_coords; ++i)
	{
		if (xy_coord_inputs[i].x != 0 && xy_coord_inputs[i].y != 0)
		{
			outFile << "Node ID: " << i+1 << "\n";
			outFile << "x = " << xy_coord_inputs[i].x << "\n";
			outFile << "y = " << xy_coord_inputs[i].y << "\n";
			outFile << "latitude = " << xy_coord_inputs[i].latitude << "\n";
			outFile << "longitude = " << xy_coord_inputs[i].longitude << "\n";
			outFile << "elevation = " << xy_coord_inputs[i].elevation << "\n";
			outFile << "is_defined = " << xy_coord_inputs[i].is_defined << "\n";
			outFile << "\n";
		}
	}
	outFile << "\n";
}

void etFommInterface::PrintParkingZones(std::ostream &outFile, int n_parkingzones, PARKING_DATA *parkingzone_inputs)
{
	outFile << "PARKING_DATA: " << n_parkingzones << "\n";

	for (int i = 0; i < n_parkingzones; ++i)
	{
		outFile << "#" << i+1 << "\n";
		outFile << "usn = " << parkingzone_inputs[i].usn << "\n";
		outFile << "dsn = " << parkingzone_inputs[i].dsn << "\n";
		outFile << "duration = " << parkingzone_inputs[i].duration << "\n";
		outFile << "freq = " << parkingzone_inputs[i].freq << "\n";
		outFile << "left_start = " << parkingzone_inputs[i].left_start << "\n";
		outFile << "left_len = " << parkingzone_inputs[i].left_len << "\n";
		outFile << "right_start = " << parkingzone_inputs[i].right_start << "\n";
		outFile << "right_len = " << parkingzone_inputs[i].right_len << "\n";
		outFile << "\n";
	}
	outFile << "\n";
}

void etFommInterface::PrintEvents(std::ostream &outFile, int n_events, EVENT_DATA *event_inputs)
{
	outFile << "EVENT_DATA: " << n_events << "\n";

	for (int i = 0; i < n_events; ++i)
	{
		outFile << "#" << i+1 << "\n";
		outFile << "begin_time = " << event_inputs[i].begin_time << "\n";
		outFile << "end_time = " << event_inputs[i].end_time << "\n";
		outFile << "lane = " << event_inputs[i].lane << "\n";
		outFile << "usn = " << event_inputs[i].usn << "\n";
		outFile << "dsn = " << event_inputs[i].dsn << "\n";
		outFile << "location = " << event_inputs[i].location << "\n";
		//outFile << "type = " << event_inputs[i].type << "\n";
		
		outFile << "\n";
	}
	outFile << "\n";
}

void etFommInterface::PrintDiversions(std::ostream &outFile, int n_diversions, DIVERSION_DATA *diversion_inputs)
{
	outFile << "DIVERSION_DATA: " << n_diversions << "\n";

	for (int i = 0; i < n_diversions; ++i)
	{
		outFile << "#" << i+1 << "\n";
		outFile << "link = " << diversion_inputs[i].link << "\n";
		outFile << "begin_time = " << diversion_inputs[i].begin_time << "\n";
		outFile << "end_time = " << diversion_inputs[i].end_time << "\n";
		outFile << "location = " << diversion_inputs[i].location << "\n";
		outFile << "pathid = " << diversion_inputs[i].pathid << "\n";
		outFile << "percentage = " << diversion_inputs[i].percentage << "\n";
		outFile << "speed = " << diversion_inputs[i].speed << "\n";
		
		outFile << "\n";
	}
	outFile << "\n";
}

void etFommInterface::WriteSCOPEDataFile(std::ostream &SCOPEDataFile)
{
	int nac = GetNumberOfACSignals();
	if (nac > 0)
	{
		//Write links to the data file
		int nsl = GetNumberOfStreetLinks();
		STREET_LINK* Street_Link_Inputs = (STREET_LINK*)calloc(nsl, sizeof(STREET_LINK));
		GetStreetLinks(Street_Link_Inputs);
		SET_EXTERNAL_ACTUATED_CONTROL(); //Oct20
		SCOPEDataFile << "!LINKS" << "\n";
		for(int n = 0; n < nsl; n++)
		{
			STREET_LINK* pCurStreetLink = &(Street_Link_Inputs[n]); 
			SCOPEDataFile << pCurStreetLink->usn << ",";
			SCOPEDataFile << pCurStreetLink->dsn << ",";
			SCOPEDataFile << pCurStreetLink->fulllanes << ",";
			SCOPEDataFile << pCurStreetLink->length << ",";
			SCOPEDataFile << pCurStreetLink->channelization[0] << ",";
			SCOPEDataFile << pCurStreetLink->channelization[1] << ",";
			SCOPEDataFile << pCurStreetLink->channelization[2] << ",";
			SCOPEDataFile << pCurStreetLink->channelization[3] << ",";
			SCOPEDataFile << pCurStreetLink->channelization[4] << ",";
			SCOPEDataFile << pCurStreetLink->channelization[5] << ",";
			SCOPEDataFile << pCurStreetLink->channelization[6] << "\n";
		}
		free(Street_Link_Inputs);
 
		//Write detectors to the data file
		int nsdet = GetNumberOfStreetDetectors();
		DETECTOR_INPUTS* SDET_Inputs = (DETECTOR_INPUTS*)calloc(nsdet, sizeof(DETECTOR_INPUTS));
		GetStreetDetectorInputs(SDET_Inputs);
		SCOPEDataFile << "!DETECTORS" << "\n";
		for(int n = 0; n < nsdet; n++)
		{
			DETECTOR_INPUTS* pCurSDET = &(SDET_Inputs[n]);
			SCOPEDataFile << pCurSDET->usn << ",";
			SCOPEDataFile << pCurSDET->dsn << ",";
			//signal_node
			//LZ 06/02/2024
			for (int j=0;j<N_DETLANE;j++)
			SCOPEDataFile << pCurSDET->detector_lanes[j] << ",";
			//SCOPEDataFile << pCurSDET->lane2 << ",";
			SCOPEDataFile << pCurSDET->location << ",";
			SCOPEDataFile << pCurSDET->carryover_time << ",";
			SCOPEDataFile << pCurSDET->delay_time << ",";
			SCOPEDataFile << pCurSDET->type_code << ",";
			SCOPEDataFile << pCurSDET->station_id << ",";
			SCOPEDataFile << pCurSDET->zone_length << ",";
			SCOPEDataFile << pCurSDET->operation_code << ",";
			SCOPEDataFile << pCurSDET->associated_phase << "\n";
		}
		free(SDET_Inputs);

		//Write controllers to the data file
		SCOPEDataFile << "!CONTROLLERS" << "\n";
		AC_INPUTS* AC_Data_Inputs = (AC_INPUTS*)calloc(nac, sizeof(AC_INPUTS));
		GetACSignals(AC_Data_Inputs);
		int *splits_flag = (int*)calloc(nac, sizeof(int));
		GET_SPLITS_FLAGS(splits_flag);
		int coordination_network_inputs[3];
		GET_COORDINATION_NETWORK_INPUTS(coordination_network_inputs);

		for(int n = 0; n < nac; n++)
		{
			AC_INPUTS* pCurAC = &(AC_Data_Inputs[n]);
			SCOPEDataFile  << n+1 << "," << pCurAC->node << "," << pCurAC->n_direct_approaches << "\n";
			//n_indirect_approaches
			for(int p = 0; p < 8; p++)
			{
				SCOPEDataFile << pCurAC->actuated_mode[p] << ",";
				SCOPEDataFile << pCurAC->min_green_time[p] << ",";
				SCOPEDataFile << pCurAC->max_green_time[p] << ",";
				SCOPEDataFile << pCurAC->default_extension_time[p] << ","; 
				SCOPEDataFile << pCurAC->gap_time[p] << ","; 
				SCOPEDataFile << pCurAC->times_before_reduction[p] << ","; 
				SCOPEDataFile << pCurAC->time_to_reduce[p] << ","; 
				SCOPEDataFile << pCurAC->min_gap_time[p] << ","; 
				SCOPEDataFile << pCurAC->yellow_change_int[p] << ",";
				SCOPEDataFile << pCurAC->red_clear_int[p] << "\n";
				for(int k = 0; k < pCurAC->n_direct_approaches; k++)
				{
					SCOPEDataFile << pCurAC->direct_approach_USN[k] << ",";
					//indirect_approach_USN, indirect_approach_DSN
					SCOPEDataFile << pCurAC->leftarrow[k][p] << ",";
					SCOPEDataFile << pCurAC->thruarrow[k][p] << ",";
					SCOPEDataFile << pCurAC->rightarrow[k][p] << ",";
					SCOPEDataFile << pCurAC->diagarrow[k][p] << "\n";
				}
			}
			SCOPEDataFile << pCurAC->ring_phase[0] << ",";           //April29	//Sept7
			SCOPEDataFile << pCurAC->ring_phase[1] << ",";           //April29	//Sept7
			SCOPEDataFile << pCurAC->ring_phase[2] << ",";           //April29	//Sept7
			SCOPEDataFile << pCurAC->ring_phase[3] << ",";           //April29	//Sept7
			SCOPEDataFile << pCurAC->ring_phase[4] << ",";           //April29	//Sept7
			SCOPEDataFile << pCurAC->ring_phase[5] << ",";           //April29	//Sept7
			SCOPEDataFile << pCurAC->ring_phase[6] << ",";           //April29	//Sept7
			SCOPEDataFile << pCurAC->ring_phase[7] << "\n";     //April29	//Sept7

			SCOPEDataFile << pCurAC->transition_method << ",";          //April29
			SCOPEDataFile << pCurAC->cycle_length << ",";				//April29
			SCOPEDataFile << pCurAC->offset << ",";				//June15
			SCOPEDataFile << pCurAC->max_add << ",";                    //April29
			SCOPEDataFile << pCurAC->max_subtract << "\n";         //April29

			if( splits_flag[n] )
			{
				SCOPEDataFile << pCurAC->splits[0] << ",";         //July16//Aug10
				SCOPEDataFile << pCurAC->splits[1] << ",";         //July16//Aug10
				SCOPEDataFile << pCurAC->splits[2] << ",";         //July16//Aug10
				SCOPEDataFile << pCurAC->splits[3] << ",";         //July16//Aug10
				SCOPEDataFile << pCurAC->splits[4] << ",";         //July16//Aug10
				SCOPEDataFile << pCurAC->splits[5] << ",";         //July16//Aug10
				SCOPEDataFile << pCurAC->splits[6] << ",";         //July16//Aug10
				SCOPEDataFile << pCurAC->splits[7] << "\n";   //July16//Aug10
			} else {
				SCOPEDataFile << pCurAC->force_off_times[0] << ",";         //April29
				SCOPEDataFile << pCurAC->force_off_times[1] << ",";         //April29
				SCOPEDataFile << pCurAC->force_off_times[2] << ",";         //April29
				SCOPEDataFile << pCurAC->force_off_times[3] << ",";         //April29
				SCOPEDataFile << pCurAC->force_off_times[4] << ",";         //April29
				SCOPEDataFile << pCurAC->force_off_times[5] << ",";         //April29
				SCOPEDataFile << pCurAC->force_off_times[6] << ",";         //April29
				SCOPEDataFile << pCurAC->force_off_times[7] << "\n";   //April29
			}
			
		}
		free(AC_Data_Inputs);
	}
}

void etFommInterface::InitSCOPEDETDCS(FILENAME* DATAFILENAME, 
								   int use_dcs, int use_ntcip, int external_detectors)
{
	int n_acs = GetNumberOfACSignals();
	if (n_acs > 0)
	{
		int tmpnsl = GetNumberOfStreetLinks();
		STREET_LINK* Street_Link_Inputs = (STREET_LINK*)calloc(tmpnsl, sizeof(STREET_LINK));
		GetStreetLinks(Street_Link_Inputs);
		int n_street_links = 0;
		for (int isl = 0; isl < tmpnsl; ++isl)
		{
			STREET_LINK* pStreet_Link = &(Street_Link_Inputs[isl]);
			if (pStreet_Link->usn < 8000 && pStreet_Link->dsn < 8000)
				n_street_links++;
		}
		free(Street_Link_Inputs);
		n_street_links = GetNumberOfStreetLinks(); //need to use this, otherwise the output is different (from simplerunner) YZ

		int n_sdet = GetNumberOfStreetDetectors();

		NETWORK_INPUTS Network_Inputs;
		GetNetworkInputs(Network_Inputs);
		//Pass in parameters used in SCOPE
		int *splits_flag = (int*)calloc(n_acs, sizeof(int));
		GET_SPLITS_FLAGS(splits_flag);
		int coordination_network_inputs[3];
		GET_COORDINATION_NETWORK_INPUTS(coordination_network_inputs);
		int status = INITIALIZE_SCOPE(DATAFILENAME, n_street_links, n_sdet, n_acs, 
			Network_Inputs.timestep, use_ntcip, use_dcs, splits_flag);
		CheckOtherStatus(GET_SCOPE_MESSAGE_COUNT, GET_SCOPE_MESSAGE, 
			"INITIALIZE_SCOPE", status);
		DEFINE_COORDINATION_NETWORK_INPUTS(coordination_network_inputs);

		//Pass in parameters used in DETECTOR_DATA
		status = INITIALIZE_DETECTOR_DATA(DATAFILENAME, external_detectors, n_sdet);
		CheckOtherStatus(GET_DETECTOR_DATA_MESSAGE_COUNT, GET_DETECTOR_DATA_MESSAGE, 
			"INITIALIZE_DETECTOR_DATA", status);
		
		if(use_dcs) 
		{
			DETECTOR_INPUTS* SDET_Inputs = (DETECTOR_INPUTS*)calloc(n_sdet, sizeof(DETECTOR_INPUTS));
			GetStreetDetectorInputs(SDET_Inputs);
			if(external_detectors)
			{
				//Pass external detectors into DETECTOR_DATA
				if (n_sdet > 0)
				{
					
					status = DEFINE_EXTERNAL_DETECTORS(n_sdet, SDET_Inputs);
					CheckOtherStatus(GET_DETECTOR_DATA_MESSAGE_COUNT, GET_DETECTOR_DATA_MESSAGE, 
						"DEFINE_EXTERNAL_DETECTORS", status);
				}
			} else {
				//Pass in parameters used in DCS
				status = INITIALIZE_DCS(DATAFILENAME, n_street_links, 
					n_sdet, n_acs, Network_Inputs.timestep, m_limit_to_maxgreen, m_d1_for_DZ, m_d2_for_DZ);
				CheckOtherStatus(GET_DCS_MESSAGE_COUNT, GET_DCS_MESSAGE, 
					"INITIALIZE_DCS", status);

				//Pass detectors into DCS
				DEFINE_DCS_DETECTORS(n_sdet, SDET_Inputs);

				//Define detector mapping for DCS; could be other numbers?
				//int idet[16] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
				int idet[16];
				std::fstream mapfile;
				char mapFileName[512];
				strncpy_s(mapFileName, DATAFILENAME->str, DATAFILENAME->len-4);
				strcat(mapFileName, "_map.dat");
				mapfile.open(mapFileName);
				for(int id = 0; id < 16; id++)
				{
					mapfile >> idet[id];
				}
				status = DEFINE_DCS_DETECTOR_MAP(idet);
				CheckOtherStatus(GET_DCS_MESSAGE_COUNT, GET_DCS_MESSAGE, 
					"DEFINE_DCS_DETECTOR_MAP", status);
			}
			free(SDET_Inputs);
		}
	}
}

void etFommInterface::UpdateDetectorOutput(int n_sdet, float *simtime, int sba[], int sbb[], int sbc[],
		int &extension, int &forceoff, int &dcs_error, int use_dcs, bool insim)
{
	int n_acs = GetNumberOfACSignals();
	if (n_acs > 0)
	{
		DETECTOR_OUTPUTS *sdet_outputs = (DETECTOR_OUTPUTS*)calloc(n_sdet, sizeof(DETECTOR_OUTPUTS));
		//Get current detector outputs from DETECTOR_DATA
		int status = GET_DETECTOR_DATA(sdet_outputs);
		CheckOtherStatus(GET_DETECTOR_DATA_MESSAGE_COUNT, GET_DETECTOR_DATA_MESSAGE, 
			"GET_DETECTOR_DATA", status);

		//Pass the current detector outputs into SCOPE
		status = UPDATE_SCOPE_DETECTORS(n_sdet, sdet_outputs);
		CheckOtherStatus(GET_SCOPE_MESSAGE_COUNT, GET_SCOPE_MESSAGE, 
			"UPDATE_SCOPE_DETECTORS", status);

		//Get current detector status from DETECTOR_DATA
		if ( !CHECK_DETECTOR_DATA_STATUS() )
		{
			extension = 0;
			forceoff = 0;
			use_dcs = 0;
			std::cout << "Detector Status Failure" << "\n";
			std::cout << "DCS has been deactivated" << "\n";
		}

		//If using DCS, determine forceoffs
		
		if(insim && use_dcs)	
		{
			status = UPDATE_DCS_DETECTORS(n_sdet, sdet_outputs);
			CheckOtherStatus(GET_DCS_MESSAGE_COUNT, GET_DCS_MESSAGE, 
					"UPDATE_DCS_DETECTORS", status);
			RUN_DCS(*simtime, sba, sbb, sbc, &extension, &forceoff, &dcs_error);
		}

		//Update signal states
		status = UPDATE_SCOPE_CONTROLLERS(false, *simtime, sba, sbb, sbc, &extension, &forceoff);
		CheckOtherStatus(GET_SCOPE_MESSAGE_COUNT, GET_SCOPE_MESSAGE, 
			"UPDATE_SCOPE_CONTROLLERS", status);

		//Get greens and yellows from SCOPE and pass them into ETFOMM
		
		int green_phases = 0;
		int yellow_phases = 0;

#if _UseController
		int phase_calls = 0;
		int numBytes = sizeof(int) * 3;
		DWORD numBytesWritten = 0;
		DWORD numBytesRead;
		BOOL result;
#endif

		int times_started[8] = {0,0,0,0,0,0,0,0};
		int maxouts[8] = {0,0,0,0,0,0,0,0};
		int min_greens[8] = {0,0,0,0,0,0,0,0};

		for(int iact = 1; iact <= n_acs; iact++)
		{
			status = GetETFOMMPhaseStates(iact, &green_phases, &yellow_phases);
			//status = GET_SCOPE_PHASE_STATES(iact, &green_phases, &yellow_phases);
			//status = GET_ETFOMM_PHASE_STATES(iact, &green_phases, &yellow_phases);
			//CheckOtherStatus(GET_SCOPE_MESSAGE_COUNT, GET_SCOPE_MESSAGE, 
			//	"GET_ETFOMM_PHASE_STATES", status);

#if _UseController
#if _NamedPipe
			int i_act;
			GET_CONTROLLER_ID(m_controller_node_id, &i_act);
			//Use NamedPipe to use Controller
			//Named Pipe get green_phases and yellow_phases, set phase_calls
			if (m_use_controller_flag && iact == i_act)
			{

				GET_PHASE_CALLS(iact, &phase_calls);

				int ControllerData[3] = {0, 0, phase_calls};

				//pass only 3 integers
				BOOL bWritePipe = WriteFile(
					DataPipe, // handle to our outbound pipe
					&ControllerData, // data to send
					numBytes, // length of data to send (bytes)
					&numBytesWritten, // will store actual amount of data sent
					NULL
					);

				if (!bWritePipe || numBytes != numBytesWritten)
				{
					//CString errMsg;
					//errMsg.Format(L"Error in writing into pipe", GetLastError());
					//return false;
					std::cout << "Error in writing into pipe " << GetLastError() << "\n";
					std::cin.get();
				}


				numBytesRead = 0;
				result = ReadFile(
					DataPipe,
					ControllerData, // the data from the pipe will be put here
					3 * sizeof(int), // number of bytes allocated
					&numBytesRead, // this will store number of bytes actually read
					NULL // not using overlapped IO
					);

				if (result)
				{
					//std::cout << "Number of bytes read: " << numBytesRead << "\n";
					//std::cout << "Greens = " << ControllerData[0] << "\n";
					//std::cout << "Yellows = " << ControllerData[1] << "\n";
					//std::cout << "phase_calls = " << ControllerData[2] << "\n";
					green_phases = ControllerData[0];
					yellow_phases = ControllerData[1];
				}
				else
				{
					std::cout << "Failed to read data from the pipe." << "\n";
				}

			}
#endif
#endif

			status = SET_ETFOMM_PHASE_STATES(iact, &green_phases, &yellow_phases);
			CheckStatus("SET_ETFOMM_PHASE_STATES", status);

			if (insim)
			{
				GET_SCOPE_MOE_DATA(iact, maxouts, min_greens, times_started);
				SET_ETFOMM_MOE_DATA(iact, maxouts, min_greens, times_started);
			}
		}
		if(use_dcs) 
		{
			//need this for the first iteration in SIMULATE()
			status = SET_DCS_PHASE_STATES(&green_phases, &yellow_phases);
			CheckOtherStatus(GET_DCS_MESSAGE_COUNT, GET_DCS_MESSAGE, 
				"SET_DCS_PHASE_STATES", status);
		}
		free(sdet_outputs);
	}
}

void etFommInterface::UpdateDetectorData(int external_detectors)
{
	int n_acs = GetNumberOfACSignals();
	if (n_acs > 0)
	{
		int n_sdet = GetNumberOfStreetDetectors();
		DETECTOR_OUTPUTS *CurrentDetectorStates = (DETECTOR_OUTPUTS*)calloc(n_sdet, sizeof(DETECTOR_OUTPUTS));
		//Get detector data
		//currently getting detector data from ETFOMM for testing purposes
		int status = GET_STREET_DETECTOR_OUTPUTS(CurrentDetectorStates); //from ETFOMM
		CheckStatus("GET_STREET_DETECTOR_OUTPUTS", status);

		//save the current detector data to a file to test the process of
		//reading detector data from a file
		//DetectorDataFile.open(DetectorDataFileName);
		//for(int d=0; d<n_sdet; d++)
		//{
		//	DetectorDataFile << CurrentDetectorStates[d].current_count << "\n";
		//	DetectorDataFile << CurrentDetectorStates[d].current_state << "\n";
		//	DetectorDataFile << CurrentDetectorStates[d].hdwy_count << "\n";
		//	DetectorDataFile << CurrentDetectorStates[d].hdwy_total << "\n";
		//	DetectorDataFile << CurrentDetectorStates[d].length_total << "\n";
		//	DetectorDataFile << CurrentDetectorStates[d].on_time << "\n";
		//	DetectorDataFile << CurrentDetectorStates[d].previous_state << "\n";
		//	DetectorDataFile << CurrentDetectorStates[d].speed_total << "\n";
		//}
		//DetectorDataFile.close();

		////read the current detector data from the input file
		//DetectorDataFile.open(DetectorDataFileName);
		//for(int d=0; d<n_sdet; d++)
		//{
		//	DetectorDataFile >> CurrentDetectorStates[d].current_count;
		//	DetectorDataFile >> CurrentDetectorStates[d].current_state;
		//	DetectorDataFile >> CurrentDetectorStates[d].hdwy_count;
		//	DetectorDataFile >> CurrentDetectorStates[d].hdwy_total;
		//	DetectorDataFile >> CurrentDetectorStates[d].length_total;
		//	DetectorDataFile >> CurrentDetectorStates[d].on_time;
		//	DetectorDataFile >> CurrentDetectorStates[d].previous_state;
		//	DetectorDataFile >> CurrentDetectorStates[d].speed_total;
		//}
		//DetectorDataFile.close();

		//send current detector outputs to DETECTOR_DATA
		status = UPDATE_DETECTOR_DATA(CurrentDetectorStates); 
		CheckOtherStatus(GET_DETECTOR_DATA_MESSAGE_COUNT, GET_DETECTOR_DATA_MESSAGE, 
			"UPDATE_DETECTOR_DATA", status);
		
		if(external_detectors)
		{
			//If using external detectors the detector data has to be passed into ETFOMM
			status = UPDATE_STREET_DETECTORS(CurrentDetectorStates);
			CheckStatus("UPDATE_STREET_DETECTORS", status);
		}
		free(CurrentDetectorStates);
	}
}

void etFommInterface::CheckOtherStatus(FPTR_VOID GET_ETFOMM_MESSAGE_COUNT,
							   FPTR_CHAR GET_ETFOMM_MESSAGE,
							   const std::string &fName, int status)
{
	std::string msg = "";
	GenerateOtherMessage(GET_ETFOMM_MESSAGE_COUNT, GET_ETFOMM_MESSAGE, msg);
	if (status > 0)  
	{
		throw(InterfaceException(" Error in " + fName + ": " + msg));
	} else 
	{
		if (msg.length() > 0 )
		{
			std::cout << msg << "\n";
			//CString * s = new CString(msg);
		    //PostMessage(hWnd, WM_ETFOMMMSG, 0, (LPARAM)s);
		}
	}
	
}

void etFommInterface::GenerateOtherMessage(FPTR_VOID GET_ETFOMM_MESSAGE_COUNT,
							   FPTR_CHAR GET_ETFOMM_MESSAGE,
							   std::string& msg)
{
	int nMsg = GET_ETFOMM_MESSAGE_COUNT();
	if (nMsg > 0)
	{
		char tempMsg[120] = "";
		for (int iMsg = 1; iMsg <= nMsg ; ++iMsg)
		{
			GET_ETFOMM_MESSAGE(tempMsg);
			msg += (std::string(tempMsg) + "\n");
		}
		//LogFile << std::string(msg) << "\n";
	}
}

int etFommInterface::GetUseNtcipValue()
{
	return use_ntcip;
}
int etFommInterface::GetUseDcsValue()
{
	return use_dcs;
}
FILENAME etFommInterface::GetDataFileName()
{
	return DATAFILENAME;
}
int etFommInterface::ETFOMM_SIMULATE()
{
	m_status = SIMULATE();
	CheckStatus("SIMULATE", m_status);
	return m_status;
}


float etFommInterface::GetSimtime()
{
	return GET_SIMTIME(); 
}

void etFommInterface::SetUseControllerFlag(bool use_controller_flag)
{
	m_use_controller_flag = use_controller_flag;
}

void etFommInterface::SetControllerNodeID(int controller_node_id)
{
	m_controller_node_id = controller_node_id;
}

void etFommInterface::ETFOMM_WriteAnimationFiles()
{
	WRITE_ANIMATION_FILES();
}

void etFommInterface::ReloadDLL()
{
	FreeLibrary(hDLL);
	hDLL = LoadLibrary("etfomm64.dll");
}

float etFommInterface::GetStreetLaneMOEData(int usn_id, int dsn_id, int lane_id, int flag, std::string str_MOEString)
{
	//IARRAY  - [in] array of input parameters
	//!(up node, down node, lane number)
	//link (iarray[0], iarray[1]), lane iarray[2]
	//int iarray[3] = { 8, 10, 1 };  //link (8,10), lane 1
	int iarray[3] = {usn_id, dsn_id, lane_id};
	
	//FLAG    - [in] MOE granularity
	//0 = cumulative, 1 = time interval, 2 = time period
	//int flag = 0; //cumulative results

	//char *MOEString = "TravelTimePerVehicle";
	const char* const_MOEString = str_MOEString.c_str();
	int length = str_MOEString.length();
	char* MOEString = new char[length+1];
	strcpy(MOEString,str_MOEString.c_str());
	
	float MOEValue = -1;
	
	m_status = GETSTREETLANEMOEDATA(MOEString, length, iarray, flag, &MOEValue);
	//! Returns: int = 0 - success
	//! Returns: int = 1 - MOE name was not recognized or link was not found
	if (m_status == 1)
	{
		std::string msg = "";
		GenerateMessage(msg);
		if (msg.length() > 0 )
		{
			std::cout << msg <<"\n";
			std::cout << "Press enter to continue..." << "\n" << "\n";
			std::cin.get();
		}
	}

	return MOEValue;
}

float etFommInterface::GetStreetLinkMOEData(int usn_id, int dsn_id, /*int lane_id,*/ int flag, std::string str_MOEString)
{
	//IARRAY  - [in] array of input parameters
	//!(up node, down node)
	//link (iarray[0], iarray[1])
	//int iarray[3] = { 8, 10 };  //link (8,10),
	int iarray[2] = {usn_id, dsn_id};

	//FLAG    - [in] MOE granularity
	//0 = cumulative, 1 = time interval, 2 = time period
	//int flag = 0; //cumulative results

	//char *MOEString = "TravelTimePerVehicle";
	const char* const_MOEString = str_MOEString.c_str();
	int length = str_MOEString.length();
	char* MOEString = new char[length+1];
	strcpy(MOEString,str_MOEString.c_str());

	float MOEValue = -1;

	m_status = GETSTREETLINKMOEDATA(MOEString, length, iarray, flag, &MOEValue);
	//! Returns: int = 0 - success
	//! Returns: int = 1 - MOE name was not recognized or link was not found
	if (m_status == 1)
	{
		std::string msg = "";
		GenerateMessage(msg);
		if (msg.length() > 0 )
		{
			std::cout << msg <<"\n";
			std::cout << "Press enter to continue..." << "\n" << "\n";
			std::cin.get();
		}
	}

	return MOEValue;
}
//deteleted intersection model code

int etFommInterface::GetETFOMMDirection(STREET_LINK* pLink, int ReceivingDSN, int dir)
{
	if (dir != 8 && ReceivingDSN > 0)
	{
		if (ReceivingDSN == pLink->thrunode)
			return ET_THRU;
		else if (ReceivingDSN == pLink->leftnode)
			return ET_LEFTTURN;
		else if (ReceivingDSN == pLink->rightnode)
			return ET_RIGHTTURN;
		else if (ReceivingDSN == pLink->diagnode)
			return ET_LEFTDIAGTURN;
		else if (ReceivingDSN == pLink->rdiagnode)
			return ET_RIGHTDIAGTURN;
	}
	
	return ConvertIntersectionModelDirection(dir);
}

int etFommInterface::ConvertIntersectionModelDirection(int dir)
{
	/*�0�, straight
	�1�, U-turn
	�2�, left-hand U-turn, ignored
	�3�, left-turn
	�4�, right-turn
	�5�, partial left-turn, (used as left diagonal?)
	�6�, partial right-turn, (used as right diagonal?)
	�7�, no direction (dead end)*/
	switch (dir)
	{
	case 0: 
		return ET_THRU;
	case 3:
		return ET_LEFTTURN;
	case 4:
		return ET_RIGHTTURN;
	case 5:
		return ET_LEFTDIAGTURN;
	case 6:
		return ET_RIGHTDIAGTURN;
	case 1:
		return ET_UTURN;
	case 8:
		return ET_RIGHTTURNWAY;
	default:
		return ET_TURN_NOTDEFINED;
	}
}