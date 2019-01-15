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
#ifndef _etRunner
#define _etRunner

#include <iostream>
#include <fstream>
#include <string>
#include <wtypes.h>
#include <deque>
#include <map>
#include <sstream>
#include <malloc.h>
#include "DataStruct.h"
#include "../include/WCFClientState.h"
#include "etFommInterface.h"
#include "DTALiteDLL.h"

#using <System.ServiceModel.dll>

//#ifndef _WIN64
//#using "..\Release\WCFServer.dll"
//#else
//#using "..\x64\Release\WCFServer.dll"
//#endif
using namespace System;
using namespace System::ServiceModel;
using namespace WCFServer;

VFData *fvehicle_data;
VSData *svehicle_data;

#if WCF_SCOPE
enum RunnerState
{
	RunnerNotAvailable,
	WaitToStart,
	SCOPERunnerIsStarted,
	DCSRunnerIsStarted,
	DETECTORRunnerIsStarted,
	STEP_STARTED,
	GET_DETECTOR_DATA_CALLED,
	UPDATE_SCOPE_DETECTORS_CALLED,
	RUN_DCS_CALLED,
	ExtForceoffIsUpdatedIfPreprocessSDET,
	GET_SCOPE_PHASE_STATES_CALLED,
	SET_ETFOMM_PHASE_STATES_CALLED,
	SET_DCS_PHASE_STATES_CALLED,
	GET_STREET_DETECTOR_OUTPUTS_CALLED,
	UPDATE_DETECTOR_DATA_CALLED,
	RunnerFinished
};
#endif

struct AddedVehicle
{
	float timestep;
	int id;
	int srcNode;
	int dstNode;
	int type;
	int fleet;
	int pathID;
};

void DisplayArrayFVehicles(array<WCF_VFData> ^vehicle_data);
void DisplayArraySVehicles(array<WCF_VSData> ^vehicle_data);
void DisplayArrayPath(array<int> ^n);
void DisplayArrayNewVehicles(array<NewVehicle> ^new_veh);
void DisplaySignals(int n, int i, int d, int g);

void etFommUpdateVehicles(IService1^ proxy, etFommInterface *etFommIF);
void AddPath(IService1^ proxy, etFommInterface *etFommIF);
void AddNewVehicle(IService1^ proxy, etFommInterface *etFommIF);
void AddSignals(IService1^ proxy, etFommInterface *etFommIF);
void updateFTCSignals(IService1^ proxy, etFommInterface *etFommIF);

NETWORK_INPUTS WCF_to_HOST_network_input_data(array<WCF_NETWORK_INPUTS>^ wcf_network_inputs);
FREEWAY_NETWORK_INPUTS WCF_to_HOST_freeway_network_input_data(array<WCF_FREEWAY_NETWORK_INPUTS>^ wcf_freeway_network_inputs);
STREET_NETWORK_INPUTS WCF_to_HOST_street_network_input_data(array<WCF_STREET_NETWORK_INPUTS>^ wcf_street_network_inputs);
void WCF_to_HOST_Vehicle_Type_Inputs(VEHICLE_TYPE_DATA* Vehicle_Type_Inputs, array<WCF_VEHICLE_TYPE_DATA>^ wcf_vdi);

void WCF_to_HOST_freeway_link_data(FREEWAY_LINK* freeway_link_data, array<Wcf_freeway_link>^ wcf_fwl);
void WCF_to_HOST_street_link_data(STREET_LINK* street_link_data, array<Wcf_street_link>^ wcf_sl);
void WCF_to_HOST_cond_turnpct_data(COND_TURNPCTS* cond_turnpct_data, array<WCF_COND_TURNPCTS>^ wcf_cond_turnpct_data);
void WCF_to_HOST_entry_node_data(ENTRYNODES_DATA* entrynode_inputs, array <WCF_ENTRYNODES_DATA> ^ wcf_entry_node);

void WCF_to_HOST_ftc_data_inputs(FTC_DATA* ftc_data_inputs, array<WCF_FTC_DATA>^ wcf_ftcs);
void WCF_to_HOST_ac_data_inputs(AC_INPUTS* ac_data_inputs, array<WCF_AC>^ wcf_acl);

void WCF_to_HOST_det_inputs(DETECTOR_INPUTS* det_inputs, array<WCF_DETECTOR_INPUTS>^ wcf_det_inputs);
void WCF_to_HOST_busroute_inputs(BUSROUTE_DATA* busroute_inputs, array<WCF_BUSROUTE_DATA>^ wcf_busroute_inputs);
void WCF_to_HOST_busstation_inputs(BUSSTATION_DATA* busstation_inputs, array<WCF_BUSSTATION_DATA>^ wcf_busstation_inputs);
void WCF_to_HOST_xy_coord_inputs(NODE_LOCATION_DATA *xy_coord_inputs, array<WCF_NODE_LOCATION_DATA>^ wcf_xy_coord_inputs);
void WCF_to_HOST_rampmeter_inputs(RM_DATA* rampmeter_inputs, array<WCF_RM_DATA>^ wcf_rampmeter_inputs);
void WCF_to_Host_incident_data_inputs(INCIDENT_DATA* incident_data_inputs, array<WCF_INCIDENT_DATA>^ wcf_incident_data_inputs);

void WCF_to_Host_parking_data_inputs(PARKING_DATA* parking_data_inputs, array<WCF_PARKING_DATA>^ wcf_parking_data_inputs);
void WCF_to_Host_event_data_inputs(EVENT_DATA* event_data_inputs, array<WCF_EVENT_DATA>^ wcf_event_data_inputs);
void WCF_to_Host_diversion_data_inputs(DIVERSION_DATA* diversion_data_inputs, array<WCF_DIVERSION_DATA>^ wcf_diversion_data_inputs);

void WCF_to_Host_GPS_Ref_Nodes(NODE_LOCATION_DATA *GPS_Ref_Nodes, array<WCF_NODE_LOCATION_DATA>^ wcf_gps_ref_nodes);

void DETECTOR_OUTPUTS_to_WCF_DETECTOR_OUTPUTS(DETECTOR_OUTPUTS *sdet_outputs, array<WCF_DETECTOR_OUTPUTS>^ wcf_sdet_outputs);
void WCF_DETECTOR_OUTPUTS_to_DETECTOR_OUTPUTS(array<WCF_DETECTOR_OUTPUTS>^ wcf_sdet_outputs, DETECTOR_OUTPUTS *sdet_outputs);

void ProcessFVehicleData(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag = 0);
void ProcessSVehicleData(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag = 0);
void ProcessFreewayLinksData(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag = 0);
void ProcessStreetLinksData(std::ofstream &outputFile, IService1 ^proxy, bool *change_phase_flag, etFommInterface *etFommIF, int updatedFlag = 0);
void ProcessFTCSignals(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag = 0);
void ProcessEntryNodes(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag = 0);
void ProcessRampMeter(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag = 0);

void UpdateFVehicles(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF);
void UpdateSVehicles(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF);
void UpdateFreewayLinkData(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF);
void UpdateStreetLinkData(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF);
void UpdateACSignals(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF);
void UpdateFTCSignals(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF);
void UpdateEntryNodes(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF);
void UpdateRampMeters(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF);

void UpdateNetworkInputs(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF);
void UpdateFNetworkInputs(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF);
void UpdateSNetworkInputs(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF);
void UpdateVTypeInputs(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF);
void UpdateFDetectorInputs(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF);
void UpdateSDetectorInputs(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF);
void UpdateCondTurnpctInputs(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF);
void UpdateBusRouteInputs(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF);
void UpdateBusStationInputs(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF);
void UpdateIncidentInputs(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF);
void UpdateCoordInputs(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF);
void UpdateParkingZones(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF);
void UpdateEvents(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF);
void UpdateDiversionInputs(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF);

void ProcessNetworkInputs(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag = 0);
void ProcessFNetworkInputs(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag = 0);
void ProcessSNetworkInputs(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag = 0);
void ProcessVTypeInputs(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag = 0);
void ProcessDetectorOutputs(int n_det, DETECTOR_OUTPUTS* det_outputs, array<WCF_DETECTOR_OUTPUTS>^ wcf_det_outputs);
void ProcessFDetectorOutputs(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag = 0);
void ProcessSDetectorOutputs(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag = 0);
void ProcessCondTurnpctInputs(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag = 0);
void ProcessBusRouteInputs(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag = 0);
void ProcessBusStationInputs(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag = 0);
void ProcessIncidentInputs(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag = 0);
void ProcessCoordInputs(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag = 0);
void ProcessParkingZones(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag = 0);
void ProcessEvents(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag = 0);
void ProcessDiversionInputs(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag = 0);
//void ProcessCoordinationData(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag = 0);

int APIProcessClientRequest(IService1^ proxy, etFommInterface *etFommIF, 
	const std::string& dataPath, const std::string& fileName, 
	int &updatedFlag, std::string &prefix);

void ConvertString(String^ src, std::string& des);

// for SCOPE input from text file
void ProcessACData(etFommInterface *etFommIF);
void FillACData(int nacs, AC_INPUTS *signal_data, etFommInterface *etFommIF);
void SetNetworkInputs(etFommInterface *etFommIF);

//for Coordination data
void ProcessCoordinationData(IService1^ proxy, etFommInterface *etFommIF);
void UpdateCoordinationData(IService1^ proxy, etFommInterface *etFommIF);

void ProcessHITLSACData(IService1^ proxy, etFommInterface *etFommIF);

void ConvertPaths(etFommInterface *etFommIF, DTALiteNameSpace::CDTALiteDLL* pDTALite, 
				  float& minTimeStep, std::vector<std::vector<int> >& paths, std::vector<AddedVehicle>& vehicles);
int FindFreewayLinkIdx(int nLinks, FREEWAY_LINK* links, int usn, int dsn);
int FindStreetLinkIdx(int nLinks, STREET_LINK* links, int usn, int dsn);
void CreateLinkConnections(int nFLinks, FREEWAY_LINK* FLinks, int nSLinks, STREET_LINK* SLinks,
						   std::map<std::pair<int, int>, int >& connections);
bool InsertConnections(int usn, int dsn, int nusn, int ndsn, std::map<std::pair<int, int>, int >& connections);
bool IsEntryNode(int nFLinks, FREEWAY_LINK* FLinks, int nSLinks, STREET_LINK* SLinks, int node);
bool IsExitNode(int nFLinks, FREEWAY_LINK* FLinks, int nSLinks, STREET_LINK* SLinks, int node);
#endif
