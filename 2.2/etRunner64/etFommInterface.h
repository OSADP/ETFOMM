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
// header file for etFommInterface class
#pragma once

#include <Windows.h>
#include <string>
#include <fstream>
#include <iostream>
#include <map>
#include <unordered_set>

#include "TypeDef.h"
#include "etAnimatorStruct.h"

#include <msclr\marshal_cppstd.h> // for System::String^ to std::string
using namespace msclr::interop; // for System::String^ to std::string
using namespace System::Runtime::InteropServices;// for System::String^ to char*
//int NumberofETFOMM_runs = 0;

class etFommInterface {

public:
	struct IntersectionDimensions
	{
		int usn;
		int dsn;
		INTERSECTION_DIMENSIONS intDim;
	};

	enum NETWORK_CODE
	{
		FREEWAYNWK = 1, 
		SURFACENWK = 2
	};

	enum SIGNAL_CODE
	{
		S_RED    =  0,
		S_AMBER  =  1,
		S_GREEN  =  2,
		S_PERGRN =  3,
		S_NONE   =  4,
		S_PERAMB =  5,
		S_STOP   = 30,
		S_YIELD  = 31
	};

	enum RM_STATE_CODE
	{
		MS_INACTIVE = 0,
		MS_GREEN = -1,
		MS_RED = 1
	};

	enum SIGN_CODE
	{
		NOSIGN, 
		STOPSIGN, 
		YIELDSIGN
	};

	enum ET_TURN
	{
		ET_LEFTTURN, 
		ET_THRU, 
		ET_RIGHTTURN,
		ET_LEFTDIAGTURN, 
		ET_RIGHTDIAGTURN, 
		ET_OPLEFTTURN,
		ET_DIAG,
		ET_UTURN,
		ET_RIGHTTURNWAY,
		ET_LEFTTURNWAY,
		ET_CROSSING,
		ET_TRACK,
		ET_STATION,
		ET_MULTINODEINTERSECTION,
		ET_PAVEMENTMARKING,
		ET_TURN_NOTDEFINED,
	};
	const static int LENGTH_OF_MESSAGE = 1000;


public: //TODO should be private, will edit it later
	FPTR_VADD ADD_VEHICLE;
	int InitDLLLoadings(void);

private:
	FPTR_FN SETOUTPUTNAME;
	FPTR_PATH ADD_PATH, SET_PHASES;
	HMODULE hDLL, SCOPE_hDLL, DCS_hDLL, DETECTOR_hDLL;
	FPTR_FN SETINPUTNAME, SETMOEFOLDER;
	FPTR_VOID API_STARTUP, STARTUP, INITIALIZE, RUNTOEQUILIBRIUM, SIMULATE, SHUTDOWN, WRITE_TRF_FILE, WRITE_ANIMATION_FILES;
	FPTR_INT SET_STOCHASTIC_FLAG;
	FPTR_VOID SCOPE_SHUTDOWN, DCS_SHUTDOWN, DETECTOR_DATA_SHUTDOWN;
	FPTR_VOID GET_NUMBER_OF_VEHICLE_TYPES, GET_NUMBER_OF_FREEWAYLINKS, GET_NUMBER_OF_STREETLINKS, GET_NUMBER_OF_ENTRYNODES;
	FPTR_INT SET_NUMBER_OF_FREEWAYLINKS, SET_NUMBER_OF_ENTRYNODES, SET_NUMBER_OF_RAMPMETERS;
	FPTR_INT SET_NUMBER_OF_STREETLINKS, SET_NUMBER_OF_FTC_SIGNALS, SET_NUMBER_OF_AC_SIGNALS;
	FPTR_INT SET_NUMBER_OF_BUSROUTES;
	FPTR_VOID GET_NUMBER_OF_BUSROUTES, GET_NUMBER_OF_EVENTS;
	FPTR_INT SET_NUMBER_OF_FREEWAY_DETECTORS;
	FPTR_INT SET_NUMBER_OF_STREET_DETECTORS;
	FPTR_VOID GET_NUMBER_OF_FTC_SIGNALS, GET_NUMBER_OF_AC_SIGNALS, GET_NUMBER_OF_RAMPMETERS;
	FPTR_FLINK GET_FREEWAYLINKS, DEFINE_FREEWAYLINKS;
	FPTR_SLINK GET_STREETLINKS, DEFINE_STREETLINKS;
	FPTR_FTC GET_FTC_SIGNALS, DEFINE_FTC_SIGNALS;
	FPTR_AC_INPUTS DEFINE_AC_SIGNALS, GET_AC_SIGNALS;
	FPTR_ENTRYNODES DEFINE_ENTRYNODES;
	FPTR_ENTRYNODES2 GET_ENTRYNODES;
	FPTR_4INTS SETIOFLAGS;
	FPTR_10INTS SET_RUN_INPUTS;
	FPTR_NETWORK_INPUTS SET_NETWORK_INPUTS;
	FPTR_NETWORK_INPUTSPTR GET_NETWORK_INPUTS;
	FPTR_FREEWAY_NETWORK_INPUTS SET_FREEWAY_NETWORK_INPUTS, GET_FREEWAY_NETWORK_INPUTS;
	FPTR_STREET_NETWORK_INPUTS SET_STREET_NETWORK_INPUTS, GET_STREET_NETWORK_INPUTS;
	FPTR_RM_DATA GET_RAMPMETERS, DEFINE_RAMPMETERS;
	FPTR_BUSROUTE DEFINE_BUSROUTES;
	FPTR_BUSROUTE GET_BUSROUTES;
	FPTR_BUSSTATION DEFINE_BUSSTATIONS;
	FPTR_BUSSTATION GET_BUSSTATIONS;
	FPTR_DETECTOR_IN DEFINE_STREET_DETECTORS;
   	FPTR_DETECTOR_IN DEFINE_FREEWAY_DETECTORS;
   	FPTR_DETECTOR_IN GET_STREET_DETECTOR_INPUTS;
   	FPTR_DETECTOR_IN GET_FREEWAY_DETECTOR_INPUTS;
	FPTR_INT SET_NUMBER_OF_EVENTS;
	FPTR_EVENT_DATA DEFINE_EVENTS, GET_EVENTS;
	FPTR_INT SET_NUMBER_OF_PARKING_ZONES;
	FPTR_PARKING_DATA DEFINE_PARKING_ZONES, GET_PARKING_ZONES;
	FPTR_INT SET_NUMBER_OF_INCIDENTS;
	FPTR_NEW_ENTRYNODE_DATA CHANGE_ENTRYVOLUMES;
	FPTR_6INTS CHANGE_TURNPERCENTAGES;
	FPTR_VOID PROCESS_FREEWAYINPUTS, PROCESS_STREETINPUTS;
	FPTR_COND_TURNPCTS DEFINE_CONDITIONAL_TURNPCTS, GET_CONDITIONAL_TURNPCTS;
	FPTR_FLOAT GETCPUTIME, GETELAPSEDTIME;
	FPTR_VOID GET_FVEHICLE_STRUCT_SIZE;
	FPTR_VFD GET_FVEHICLE_STRUCT, SET_FVEHICLE_STRUCT;
	FPTR_VOID GET_SVEHICLE_STRUCT_SIZE;
	FPTR_VSD GET_SVEHICLE_STRUCT, SET_SVEHICLE_STRUCT;
	FPTR_3INTS CLOSE_LANE, REOPEN_LANE;
	FPTR_VOID GET_NUMBER_OF_FREEWAY_DETECTORS, GET_NUMBER_OF_STREET_DETECTORS;

	FPTR_INT SET_NUMBER_OF_DIVERSIONS;
	FPTR_VOID GET_NUMBER_OF_DIVERSIONS, GET_NUMBER_OF_INCIDENTS, GET_NUMBER_OF_PARKING_ZONES;
	FPTR_DIVERSION DEFINE_DIVERSIONS, GET_DIVERSIONS;
	FPTR_3INTS CHANGE_DURATION;
	FPTR_INT INCREMENT_INTERVAL;
	FPTR_VEHICLE_TYPE_DATA DEFINE_VEHICLE_TYPES, GET_VEHICLE_TYPES;
	FPTR_DETECTOR_OUT GET_FREEWAY_DETECTOR_DATA, GET_STREET_DETECTOR_OUTPUTS;
	FPTR_INTINTP GET_PHASES;
	FPTR_INT GET_INTERVAL;
	FPTR_INTPTR GET_SPLITS_FLAGS;
	FPTR_2INTS GET_DURATION;
	FPTR_INCIDENT_DATA DEFINE_INCIDENTS, GET_INCIDENTS;
	FPTR_NODE_LOCATION_DATA DEFINE_NODE_COORDINATES, GET_NODE_COORDINATES;

	FPTR_VOID GET_ETFOMM_MESSAGE_COUNT;
   	FPTR_CHAR GET_ETFOMM_MESSAGE;
	FPTR_DETECTOR_OUT UPDATE_STREET_DETECTORS;
	FPTR_MOES SET_ETFOMM_MOE_DATA;
	FPTR_COORDINATION GET_COORDINATION_NETWORK_INPUTS;

	FPTR_VOID GET_SCOPE_MESSAGE_COUNT;
   	FPTR_CHAR GET_SCOPE_MESSAGE;
	FPTR_DEFINE_DETECTOR_INPUTS DEFINE_SCOPE_DETECTORS;
	FPTR_ACS DEFINE_SCOPE_CONTROLLERS;
	FPTR_FLOATS DEFINE_COORDINATION_INPUTS;
	FPTR_UPDATE_DETECTORS UPDATE_SCOPE_DETECTORS;
	FPTR_SCOPE UPDATE_SCOPE_CONTROLLERS;
	FPTR_PHASE_STATES GET_SCOPE_PHASE_STATES;
	FPTR_PHASE_STATES SET_ETFOMM_PHASE_STATES;
	FPTR_GET_PHASE_STATES GET_ETFOMM_PHASE_STATES;
	FPTR_SCOPE_PARAMETERS INITIALIZE_SCOPE;
	FPTR_MOES GET_SCOPE_MOE_DATA;
	FPTR_COORDINATION DEFINE_COORDINATION_NETWORK_INPUTS;
	FPTR_INTERSECTIONDIM DEFINE_INTERSECTION_DATA, GET_INTERSECTION_DATA;

   	FPTR_VOID GET_DCS_MESSAGE_COUNT;
   	FPTR_CHAR GET_DCS_MESSAGE;
	FPTR_DEFINE_LINKS DEFINE_DCS_LINKS;
	FPTR_DEFINE_DETECTOR_INPUTS DEFINE_DCS_DETECTORS;
	FPTR_ACS DEFINE_DCS_CONTROLLERS;
	FPTR_UPDATE_DETECTORS UPDATE_DCS_DETECTORS;
	FPTR_DCS RUN_DCS;
	FPTR_DCSPHASE_STATES SET_DCS_PHASE_STATES;         //July27
	FPTR_DCS_PARAMETERS INITIALIZE_DCS;
	FPTR_DCS_MAP DEFINE_DCS_DETECTOR_MAP;

   	FPTR_VOID GET_DETECTOR_DATA_MESSAGE_COUNT;
   	FPTR_CHAR GET_DETECTOR_DATA_MESSAGE;
	FPTR_DETECTOR_DATA_PARAMETERS INITIALIZE_DETECTOR_DATA;
	FPTR_DETECTOR_OUT UPDATE_DETECTOR_DATA;
	FPTR_DETECTOR_OUT GET_DETECTOR_DATA;
	FPTR_DEFINE_DETECTOR_INPUTS DEFINE_INTERNAL_DETECTORS;
	FPTR_DEFINE_DETECTOR_INPUTS DEFINE_EXTERNAL_DETECTORS;
	FPTR_DETECTOR_STATUS CHECK_DETECTOR_DATA_STATUS;

	FPTR_ANIMATION_DATA GET_STREET_ANIMATION_DATA, GET_FREEWAY_ANIMATION_DATA;
	FPTR_INTINTP GET_CONTROLLER_ID;//Added 4/29/15
	FPTR_INTFLTP GET_LOCAL_CYCLE_TIMER;//Added 4/29/15
	FPTR_INTFLTP GET_CYCLE_LENGTH;//Added 4/29/15
	FPTR_INTFLTP GET_OFFSET;//Added 4/29/15
	FPTR_INTFLTP GET_NEW_OFFSET;//Added 4/29/15
	FPTR_INTFLTP GET_NEW_CYCLE_LENGTH;//Added 4/29/15
	FPTR_INTFLT SET_NEW_CYCLE_LENGTH;//Added 4/29/15
	FPTR_INTFLT SET_NEW_OFFSET;//Added 4/29/15
	FPTR_INTFLTP GET_SPLITS;//Added 4/29/15
	FPTR_INTFLTP GET_MIN_SPLITS;//Added 4/29/15
	FPTR_INTFLTP GET_NEW_SPLITS;//Added 4/29/15
	FPTR_INTFLTP SET_NEW_SPLITS;//Added 4/29/15
	FPTR_INTINTP2FLTP GET_TRANSITION_METHOD;//Added 4/29/15
	FPTR_2INT2FLT SET_TRANSITION_METHOD;//Added 4/29/15

	FPTR_REAL GET_SIMTIME;         //July27
	FPTR_DCS_ENTRIES GET_USER_DCS_ENTRIES; //July28
	FPTR_2INTSF GET_STREET_TRAVEL_TIME;//Aug11
	FPTR_INTINTP GET_PHASE_CALLS; //Aug23

	FPTR_BOOL SET_TSIS_FLAG;
	FPTR_GETSIG GET_SIGNAL;
	FPTR_VOID SET_EXTERNAL_ACTUATED_CONTROL; //Oct20

	FPTR_OutProcFunction GETSTREETLANEMOEDATA;
	FPTR_OutProcFunction2 GETSTREETLINKMOEDATA;

	FPTR_INT SET_NUMBER_OF_ROUNDABOUTS;
	FPTR_RABT DEFINE_ROUNDABOUTS, GET_ROUNDABOUTS;
	//LZ changed 06/23/2024
	//FPTR_VOID GET_NUMBER_OF_TURNING_WAYS;
	//FPTR_TW DEFINE_TURNING_WAYS, GET_TURNING_WAYS;
	//FPTR_INT SET_NUMBER_OF_TURNING_WAYS;
	FPTR_INT SET_NUMBER_OF_RTURNING_WAYS, SET_NUMBER_OF_LTURNING_WAYS;
	FPTR_VOID GET_NUMBER_OF_RTURNING_WAYS, GET_NUMBER_OF_LTURNING_WAYS;
	FPTR_TW DEFINE_RTURNING_WAYS, GET_RTURNING_WAYS, DEFINE_LTURNING_WAYS, GET_LTURNING_WAYS;
	FPTR_INTPTR DEFINE_SUPER_CONTROLLER;

	int m_status;
	std::string appPath, DefaultRnsFile, DefaultTrfFile, TRFFile;//TODO delete?
	int use_dcs, use_ntcip, external_detectors, write_trf;
	FILENAME DATAFILENAME;
	std::ofstream SCOPEDataFile;
	
	int m_WriteFiles,m_TSDFlag, m_nrun;
	int m_ntimestep;
	float m_timestep,  m_timestepunit;
	float m_cpu_time;
	float m_elapsed_time;
	float m_current_simtime;
	NETWORK_INPUTS Network_Inputs;
	FREEWAY_NETWORK_INPUTS Freeway_Network_Inputs;
	int m_number_of_vehicle_types;
	int m_number_of_freeway_links;
	int m_number_of_street_links;
	int m_number_of_entrynodes;
	int m_number_of_ac_signals;
	int m_number_of_ftc_signals;
	int m_number_of_rampmeters;
	int m_number_of_busroutes;
	int m_limit_to_maxgreen;
	float m_d1_for_DZ; //for Dilemma Zone calculation
	float m_d2_for_DZ; //for Dilemma Zone calculation
	bool m_read_splits;
	HANDLE DataPipe;
	bool m_use_controller_flag; //used for NamedPipe only
	int m_controller_node_id; //used for NamedPipe only

	// concatenate point string to intersection model input string
	void ConcatIMIntStr(std::string& Str, int x, char delimiter = ',')
	{
		Str += delimiter;
		Str += std::to_string(x);
	}

	void ConcatIMDoubleStr(std::string& Str, double x, char delimiter = ',')
	{
		Str += delimiter;
		Str += std::to_string(x);
	}

	int ConvertIMDoubleToInt(double a, double convertParam)
	{
		return int(a * convertParam + 0.5);
	}

	int GetETFOMMDirection(STREET_LINK* pLink, int ReceivingDSN, int dir);
	int ConvertIntersectionModelDirection(int dir);
	std::vector<IntersectionDimensions> IntersectionDimensions_Inputs;
public:

	

	char err_msg[1000];	// error message can be accessed by caller

	//class constructor
	etFommInterface();
	~etFommInterface();

	//loading etfomm.dll
	int loadDll(std::string dllname);

	//initialize etfomm.dll interface
	int Init();

	int SetInputs(std::string trfFILE = "", int TSDFlag = 0, int TIDFlag = 0, int OutFlag = 0, int CSVFlag = 0, 
		int WriteTextFlag = 0, int nrun = 1);
	int StartUP(bool API = false);
	int RunToEquilibrium(void);
	int StepSimulate(void);
	int ShutDown(void);
	void WriteTRFFile(void);

	int SetRunInputs(int nrun = -1, int seed1 = -1, int seed2 = -1, int seed3 = -1);
	int SetNetworkInputs(NETWORK_INPUTS Network_Inputs);
	void SetFreewayNetworkInputs(FREEWAY_NETWORK_INPUTS Freeway_Network_Inputs);
	void SetNumberOfFreewayLinks(int n_freeway_links);
	int SetFreewayLinks(FREEWAY_LINK *freeway_link_data);
	void SetNumberOfFreewayDetectors(int n_fdet);
	void SetFDetectors(DETECTOR_INPUTS *fdet_inputs);
	void SetNumberOfStreetLinks(int n_street_links);
	int SetStreetLinks(STREET_LINK *street_link_data);
	int SetConditionalTurnpcts(COND_TURNPCTS *cond_turnpct_data);
	void SetNumberOfEntryNodes(int n_entrynodes);
	void SetEntryNodes(int typedist, int erlanga, float minsep, ENTRYNODES_DATA *entrynode_inputs);
	void SetNumberOfFTCSignals(int n_ftcs);
	void SetFTCSignals(FTC_DATA *ftc_data_inputs);
	void SetNumberOfStreetDetectors(int n_sdet);
	void SetSDetectors(DETECTOR_INPUTS *sdet_inputs);
	void SetNumberOfACSignals(int n_acs);
	void SetACSignals(AC_INPUTS *ac_data_inputs);
	void SetNumberOfRampMeters(int n_rampmeters);
	void SetRampMeters(RM_DATA *rampmeter_inputs);
	void SetNumberOfBusRoutes(int n_busroutes);
	void SetBusRoutes(BUSROUTE_DATA *busroute_inputs);
	void SetBusStations(BUSSTATION_DATA *busstation_inputs);
	void SetNumberOfIncidents(int n_incidents);
	void SetIncidents(INCIDENT_DATA *incident_data_inputs);
	void SetNodeCoordinates(NODE_LOCATION_DATA *xy_coord_inputs);
	void SetIntersectionData(std::vector<IntersectionDimensions>& intersection_inputs);
	void SetNumberOfRoundabouts(int n_rabts);
	void SetRoundabouts(RABT_API_DATA* roundabout_inputs);
	void SetNumberOfTurningWays(int n_rtws);
	void SetTurningWays(TURNING_WAY* turningway_inputs);
	
	void ProcessFreewayInputs(void);
	void ProcessStreetInputs(void);

	void SetPHASES(int Node, int current_phases[2]);//TODO double check
	int SetFVehicle(VFData *vfdata);
	int SetSVehicle(VSData *vsdata);
	int AddPath(int NofNodes, int *nodes);
	int AddVehicle(float timeStep, int srcNode, int pathID, int driverType, int fleet, int type, int overSpeed, int range);


	int GetNumberOfVehicleTypes(void);
	int GetNumberOfFreewayLinks(void);
	int GetFreewayLinks(FREEWAY_LINK *freeway_link_data);
	int GetNumberOfStreetLinks(void);
	int GetStreetLinks(STREET_LINK *street_link_data);
	int GetNumberOfEntrynodes(void);
	int GetEntrynodes(int *typedist, int *erlanga, float *minsep, ENTRYNODES_DATA *entrynode_data);
	int GetNumberOfACSignals(void);
	int GetACSignals(AC_INPUTS *ac_signal_data);

	int GetNumberOfFTCSignals(void);
	int GetFTCSignals(FTC_DATA *ftc_signal_data);
	int GetNumberOfRampmeters(void);
	int GetRampmeters(RM_DATA *rampmeter_data);
	int GetNumberOfBusroutes(void);
	int GetBusroutes(BUSROUTE_DATA *busroute_data);
	int GetBusstations(BUSSTATION_DATA *busstation_data);



	float GetTimeStep(void);
	int GetNumberOfFVehicles(void);
	int GetFVehicle(VFData *vfdata);
	int GetNumberOfSVehicles(void);
	int GetSVehicle(VSData *vsdata);

	float GetCPUTime(void);
	float GetElapsedTime(void);
	float GetCurrentSimTime(void);

	int GetCurrentPeriod(void);

	// complementing
	void GetNetworkInputs(NETWORK_INPUTS &Network_Inputs);
	void GetFreewayNetworkInputs(FREEWAY_NETWORK_INPUTS &Freeway_Network_Inputs);
	void SetStreetNetworkInputs(STREET_NETWORK_INPUTS Street_Network_Inputs);
	void GetStreetNetworkInputs(STREET_NETWORK_INPUTS &Street_Network_Inputs);

	int GetNumberOfFreewayDetectors(void);
	void GetConditionalTurnpcts(COND_TURNPCTS *cond_turnpct_data);
	int GetNumberOfStreetDetectors(void);


	void SetVehicleTypes(VEHICLE_TYPE_DATA *Vehicle_Type_Inputs);
	void SetNumberOfParkingZones(int n_parkingzones);
	void SetParkingZones(PARKING_DATA *Parking_Zone_Inputs);
	void SetNumberOfEvents(int n_events);
	void SetEvents(EVENT_DATA *Event_Inputs);
	void GetVehicleTypes(VEHICLE_TYPE_DATA *Vehicle_Type_Inputs);
	void SetNumberOfDiversions(int n_diversions);
	void SetDiversions(DIVERSION_DATA *Diversion_Inputs);
	void GetFreewayDetectorInputs(DETECTOR_INPUTS  *Freeway_Detector_Inputs);
	void GetStreetDetectorInputs(DETECTOR_INPUTS *Street_Detector_Inputs);
	void GetFreewayDetectorOutputs(DETECTOR_OUTPUTS  *Freeway_Detector_Outputs);
	void GetStreetDetectorOutputs(DETECTOR_OUTPUTS *Street_Detector_Outputs);
	
	int GetNumberOfIncidents(void);
	void GetIncidents(INCIDENT_DATA *incident_data_inputs);
	void GetNodeCoordinates(NODE_LOCATION_DATA *xy_coord_inputs);
	int GetNumberOfDiversions(void);
	void GetDiversions(DIVERSION_DATA *Diversion_Inputs);

	int GetNumberOfParkingZones();
	void GetParkingZones(PARKING_DATA *Parking_Zone_Inputs);
	int GetNumberOfEvents();
	void GetEvents(EVENT_DATA *Event_Inputs);

	void SetGPSRefNodes(NODE_LOCATION_DATA *GPSRefNodes, int n_gps_ref_nodes = 2);
	void SetGPSRefNodes(const std::string& GPSRefFile);
	int ReadSCOPEInputs(AC_INPUTS *ac_signal_data, int nac);

//Added 4/29/15
	int GetControllerID(int node, int *iact);
	int GetLocalCycleTimer(int node, float *local_cycle_timer);	
	int GetCycleLength(int node, float *cycle_length);
	int SetNewCycleLength(int node, float cycle_length);
	int GetNewCycleLength(int node, float *cycle_length);
	int GetOffset(int node, float *offset);
	int SetNewOffset(int node, float offset);
	int GetNewOffset(int node, float *offset);
	int GetTransitionMethod(int node, int *method, float *max_add, float *max_subt);
	int SetTransitionMethod(int node, int method, float max_add, float max_subt);
	int GetSplits(int node, float *splits);
	int GetMinSplits(int node, float *splits);
	int SetNewSplits(int node, float *new_splits);
	int GetNewSplits(int node, float *new_splits);
	float GetStreetTravelTime(int usn_id, int dsn_id);
	//void GetSplitsFlags(int *splits_flag);
	int GetETFOMMPhaseStates(int iact, int* greens, int* yellows);
	int SetETFOMMPhaseStates(int iact, int* greens, int* yellows);
	int GetPhaseCalls(int iact, int* phase_calls);
	
	int GetNumberOfRoundabouts();
	void GetRoundabouts(RABT_API_DATA* roundabout_inputs);
	int GetNumberOfTurningWays();
	int GetTurningWays(TURNING_WAY* turningway_inputs);


	// write animation data to named pipe

	void etFommInterface::WriteAnimatortoPipe();
	//int WriteLinkDataPipe(HANDLE hPipe);
	//int wait_for_acknowledgment(HANDLE hPipe);
	
private:
	// check return status of ETFOMM function and throw error messagex
	void CheckStatus(const std::string &fName, int status);
	void GenerateMessage(std::string &msg);
	std::string retMsg;

	// generate animation data from simulation
	std::vector<AniFreewayLink> m_AniFLinkSet;
	std::vector<AniSurfaceLink> m_AniSLinkSet;
	std::map<std::pair<int,int>, int> m_UsnDsnToSLinkIDMap;
	std::vector<AniNode> m_AniNodeSet;
	std::vector<AniLocation> m_AniLocationSet;

	std::vector<AniVehicleData> m_AniVehicleDataSet;
	std::vector<AniLinkData> m_AniLinkDataSet, m_AniRampMeterDataSet;
	std::vector<AniVehicle> m_AniVehicleList;
	std::map<int, AniVehicle*> m_VehIDToVehMap;
	
	std::unordered_set<int> m_FNodeSet, m_SNodeSet;

	NODE_LOCATION_DATA RefNode1, RefNode2;

	void InitAnimationDataSet();
	void GenFreewayLinks();
	void GenSurfaceLinks();
	void GenNodes();
	void GenLocations();
	bool IsInternalNode(int nodeID);
	void GenGPSCoordinates(NODE_LOCATION_DATA *node_location_inputs);
	
	
	void GenRampMeterDataSet(float timeStep, 
		std::vector<AniLinkData>& RMSet, std::map<int, int>& LinkIDToRMMap);

	void GenFreewayData(float timeStep);
	void GenStreetData(float timeStep);
	
	void GenFTCSignalCodes(const std::map<int, ANIMATION_DATA*>& linkIDToAniDataMap, 
		const std::map<int, AniLinkData*>& LinkIDToLinkData);
	void DecodeFTCSignalCode(int CurCode, 
		int& LeftTurn, int& Thru, int& rightTurn, int& diagTurn, bool& Amber);
	
	int GenFTCCodeAtAmber(int DecodedCode)
	{
		return (DecodedCode == S_RED) ? S_RED : S_AMBER;
	}

	void GenACSignalCodes(const std::map<int, ANIMATION_DATA*>& linkIDToAniDataMap, 
		const std::map<int, AniLinkData*>& LinkIDToLinkData);

	int GenACCode(int Amber, int Signal)
	{
		return (Amber == 1) ? 
			((Signal == 1) ? S_AMBER : S_RED) 
			: ((Signal == 1) ? S_GREEN : S_RED);
	}

	// write animation data to text files


	// write animation data to text files
	void WriteAnimatorFiles();
	void WriteLocationFile(std::string setName);
	void WriteLinkData(std::string setName);
	void WriteVehicleData(std::string setName);
	void WriteVehicleList(std::string setName);
	void WriteNodes(std::string setName);
	void WriteLinks(std::string setName);
	void WriteRampMeterData(std::string setName);

	static bool compareLinkData(const AniLinkData& LinkA, const AniLinkData& LinkB);
	static bool compareVehData(const AniVehicleData& VehA, const AniVehicleData& VehB);
	static bool compareVehList(const AniVehicle& VehA, const AniVehicle& VehB);
	
	void WriteSCOPEDataFile(std::ostream &SCOPEDataFile);
	void InitSCOPEDETDCS(FILENAME* DATAFILENAME, int use_dcs = 0, int use_ntcip = 0, int external_detectors = 0);
	void UpdateDetectorOutput(int n_sdet, float *simtime, int sba[], int sbb[], int sbc[],
		int &extension, int &forceoff, int &dcs_error, int use_dcs = 0, bool insim = true);
	void UpdateDetectorData(int external_detectors = 0);

	void CheckOtherStatus(FPTR_VOID GET_ETFOMM_MESSAGE_COUNT,
					FPTR_CHAR GET_ETFOMM_MESSAGE,
							   const std::string &fName, int status);

	void GenerateOtherMessage(FPTR_VOID GET_ETFOMM_MESSAGE_COUNT,
						FPTR_CHAR GET_ETFOMM_MESSAGE,
						std::string& msg);
public:
	void PrintFVehicleData(std::ostream &outStream, int n_fvehicles, VFData *fvehicle_data);
	void PrintSVehicleData(std::ostream &outStream, int n_svehicles, VSData *svehicle_data);
	void PrintFreewayLinkData(std::ostream &outStream, int n_freeway_links, FREEWAY_LINK *freeway_link_data);
	void PrintStreetLinkData(std::ostream &outStream, int n_street_links, STREET_LINK *street_link_data);
	void PrintACSignals(std::ostream &outStream, int n_ac, AC_INPUTS *ac_signal_data);
	void PrintFTCSignals(std::ostream &outStream, int n_ftc, FTC_DATA *ftc_signal_data);
	void PrintEntryNodes(std::ostream &outStream, int n_entrynodes, ENTRYNODES_DATA *entrynode_data);
	void PrintRampMeter(std::ostream &outStream, int n_rampmeters, RM_DATA *rampmeter_data);

	void PrintNetworkInputs(std::ostream &outStream, const NETWORK_INPUTS &Network_Inputs);
	void PrintFNetworkInputs(std::ostream &outStream, const FREEWAY_NETWORK_INPUTS &FNetwork_Inputs);
	void PrintSNetworkInputs(std::ostream &outStream, const STREET_NETWORK_INPUTS &SNetwork_Inputs);
	void PrintVTypeInputs(std::ostream &outStream, int n_vehicletype, VEHICLE_TYPE_DATA *Vehicle_Type_Inputs);
	void PrintDetectorInputs(std::ostream &outStream, const std::string &dtype,
		int n_dets, DETECTOR_INPUTS *Detector_Inputs);
	void PrintCondTurnpctInputs(std::ostream &outStream, int n_turnpct, COND_TURNPCTS *cond_turnpct_data);
	void PrintBusRouteInputs(std::ostream &outStream, int n_busroute, BUSROUTE_DATA *busroute_inputs);
	void PrintBusStationInputs(std::ostream &outStream, int n_busstation, BUSSTATION_DATA *busstation_inputs);
	void PrintIncidentInputs(std::ostream &outStream, int n_incident, INCIDENT_DATA *incident_inputs);
	void PrintCoordInputs(std::ostream &outStream, int n_coords, NODE_LOCATION_DATA *node_location_inputs);
	void PrintParkingZones(std::ostream &outStream, int n_parkingzones, PARKING_DATA *parkingzone_inputs);
	void PrintEvents(std::ostream &outStream, int n_events, EVENT_DATA *event_inputs);
	void PrintDiversions(std::ostream &outStream, int n_diversions, DIVERSION_DATA *diversion_inputs);

	void RunInitialize();

	int GetUseNtcipValue();
	int GetUseDcsValue();
	FILENAME GetDataFileName();
	int ETFOMM_SIMULATE();
	float GetSimtime();
	void SetUseControllerFlag(bool use_controller_flag);
	void SetControllerNodeID(int controller_node_id);
	void ETFOMM_WriteAnimationFiles();
	void ReloadDLL();
	float GetStreetLaneMOEData(int usn_id, int dsn_id, int lane_id, int flag, std::string str_MOEString);
	float GetStreetLinkMOEData(int usn_id, int dsn_id, /*int lane_id,*/ int flag, std::string str_MOEString);

	//void ImportIntersectionModel();
	void GetShellExecuteError(const std::string& fileName, int ret);
	void FreeDLL();
	void etFommInterface::PrintSDETInputs(std::ofstream& outFile, DETECTOR_INPUTS* SDETData, int numberOfSDETs);

} ;

class InterfaceException : public std::runtime_error
{
public:
	explicit InterfaceException(const std::string &s)
		: std::runtime_error(s) {}
};
