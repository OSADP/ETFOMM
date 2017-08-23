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
//   You should have recetcadataByLinkived a copy of the GNU Affero General Public License
//   along with this program.  If not, see <http://www.gnu.org/licenses/>.
// APIClient.cpp : Defines the entry point for the console application.

#include "stdafx.h"
#include "../include/WCFClientState.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <list>
#include <msclr\marshal_cppstd.h>

#include <vector>
#include <queue>

#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#using <System.dll>
#using <System.ServiceModel.dll>

using namespace System;
using namespace System::ServiceModel;
using namespace System::IO;
using namespace WCFServer;
using namespace std;


int _tmain(int argc, _TCHAR* argv[])
{
	int vid, drivetype,link,lane;
	float location;
	int n, node, node_id, new_offset;
	array<int>^ nodesList;
	char t = NULL;
	float speed,ti;
	
	bool HostDefineNetworkFlag = false;
	bool ClientDefineNetworkFlag = false;
	bool TRFFileFlag = false;
	int currentPeriod = 1;
	int lastPeriod = 1;
	bool updateFlag = false;
	bool printFlag = false;
	
	//std::string str_command_to_start_etrunner = "start J:\\etRunner64Case\\x64\\Release_TCP_Controller\\etRunner64.exe trffile=\"C:\\Genetic.trf\"";
	//system(str_command_to_start_etrunner.c_str());
	
	/*The Client side to connect WCF Host. We want to Client side could modify data in the server side.
	  Client Configuration to connect the server host. Configuration of binding, address*/
	
	cout<<"connecting to WCF server... "<<endl;	
	    

#if NETTCPBINDING
	NetTcpBinding^ binding = gcnew NetTcpBinding();
	binding->MaxBufferPoolSize = 2147483647;
	binding->MaxReceivedMessageSize = 2147483647; 
	binding->Security->Mode = System::ServiceModel::SecurityMode::None;
	TimeSpan *TimeOutSetting = new TimeSpan(1, 0, 0);
	binding->OpenTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
	binding->CloseTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
	binding->SendTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
	binding->ReceiveTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));

	EndpointAddress^ address = gcnew EndpointAddress(String::Format(L"net.tcp://localhost:6000/service", Environment::MachineName));
#endif


	ChannelFactory<IService1^>^ factory = gcnew ChannelFactory<IService1^>(binding, address);
	IService1^ proxy = factory->CreateChannel();
	std::cout<<"Communication channel is completed."<<endl;

	float interval;
	proxy->SetNumberOfConnectedClients(1);
	proxy->SetClientState(RequestingUpdate);
	array <NewVehicle> ^ newVehicle = gcnew array<NewVehicle>(1);
	array<WCF_VFData> ^wcf_fvehicle_data = gcnew array<WCF_VFData>(1);
	array<WCF_VSData> ^wcf_svehicle_data = gcnew array<WCF_VSData>(1);
	//array<WCF_VSData> ^svehicle_BSMdata;
	array<WCF_VSData> ^svehicle_BSMdata = gcnew array<WCF_VSData>(1);
	array <WCF_NETWORK_INPUTS> ^ wcf_ni = gcnew array<WCF_NETWORK_INPUTS>(1);
	array <WCF_FREEWAY_NETWORK_INPUTS>^ wcf_fni = gcnew array<WCF_FREEWAY_NETWORK_INPUTS>(1);
	array <WCF_STREET_NETWORK_INPUTS>^ wcf_sni = gcnew array<WCF_STREET_NETWORK_INPUTS>(1);
	array <WCF_VEHICLE_TYPE_DATA>^ wcf_vti = gcnew array<WCF_VEHICLE_TYPE_DATA>(1);
	array <Wcf_freeway_link> ^ wcf_fwl = gcnew array<Wcf_freeway_link>(1);
	array <Wcf_street_link> ^ wcf_sl = gcnew array<Wcf_street_link>(1);
	array <WCF_COND_TURNPCTS>^ wcf_ct = gcnew array<WCF_COND_TURNPCTS>(1);
	array <WCF_ENTRYNODES_DATA> ^ wcf_entry_node = gcnew array<WCF_ENTRYNODES_DATA>(1);
	array<WCF_FTC_DATA>^ wcf_ftc =gcnew array<WCF_FTC_DATA>(1);
	array<WCF_AC> ^ wcf_acl = gcnew array<WCF_AC>(1);
	array<WCF_DETECTOR_INPUTS>^ wcf_fdet_inputs = gcnew array<WCF_DETECTOR_INPUTS>(1);
	array<WCF_DETECTOR_INPUTS>^ wcf_sdet_inputs = gcnew array<WCF_DETECTOR_INPUTS>(1);
	array<WCF_BUSROUTE_DATA>^ wcf_busroute_inputs = gcnew array<WCF_BUSROUTE_DATA>(1);
	array<WCF_BUSSTATION_DATA>^ wcf_busstation_inputs = gcnew array<WCF_BUSSTATION_DATA>(1);
	array<WCF_NODE_LOCATION_DATA>^ wcf_xy_coord_inputs = gcnew array<WCF_NODE_LOCATION_DATA>(1);
	array<WCF_RM_DATA>^ wcf_rampmeter_inputs = gcnew array<WCF_RM_DATA>(1);
	array<WCF_INCIDENT_DATA>^ wcf_incident_data_inputs = gcnew array<WCF_INCIDENT_DATA>(1);
	array<WCF_PARKING_DATA>^ wcf_parking_zone_inputs = gcnew array<WCF_PARKING_DATA>(1);
	array<WCF_EVENT_DATA>^ wcf_event_inputs = gcnew array<WCF_EVENT_DATA>(1);
	array<WCF_DIVERSION_DATA>^ wcf_diversion_inputs = gcnew array<WCF_DIVERSION_DATA>(1);

	bool exit = false;
	int command = 0;
	//enter timestep interval for etfomm to pause
	//If .trf file Column 1 RT4 specify "10", then 10 steps per second, that is 0.1 second per time step.
	//The interval value here specified here specifies the interval etRunner pauses
	//If specified "1" then you can only get the simulation data from WCF every 1 second, that is every 10 steps (because 10 steps per second)
	std::cout << "\n\n############################" << std::endl;
	std::cout<<"Please enter the timestep interval for etfomm to pause for API client input. "<<endl;
	std::cin >> interval;
	proxy->SetAPITimestepInterval(interval);

	//Generate 5 TXT files in the same folder with .trf file, these files can be converted in TSIS and animation can be seen in TRAFVU
	std::cout << "\n\n############################" << std::endl;
	std::cout << "Would you like to generate text files at the end of simulation? (Y/N) " << std::endl;
	int writeTextFlag = 0;
	char GPSFlag = 'N';
	std::cin >> GPSFlag;
	if (GPSFlag == 'Y' || GPSFlag == 'y')
	{
		writeTextFlag = 1;
	}
	proxy->SetServerWriteTextFlag(writeTextFlag);

	TRFFileFlag = true;
	
	float currentstep = 0.0;
	int initcount = 1;
	Boolean state=true;
	int laststep = -1;

	if (TRFFileFlag)
	{
		std::cout << "\n\n############################" << std::endl;
		std::cout << "Please enter the path and file name for TRF file: " << std::endl;
		std::string TRFFile = "";
		std::cin >> TRFFile;
		String ^TRFFileName = gcnew String(TRFFile.c_str());
		TRFFileFlag = false;
		proxy->SetServerTRFFile(TRFFileName);
		proxy->SetClientState(UseTRFFile);

	}
	//proxy->SetClientState(Continued);
	ofstream Debug;
	char DebugFileName[] = "_debug.csv";
	Debug.open(DebugFileName);
	Debug << "time, node, greens, yellows, local_cycle_timer, node, greens, yellows, local_cycle_timer, node, greens, yellows, local_cycle_timer, node, greens, yellows, local_cycle_timer, node, greens, yellows, local_cycle_timer, (375/450 6/8/10/14/16)" << std::endl;

	while (!exit)
	{
#pragma region TimePeriod//You can ignore
		currentPeriod = proxy->GetServerTimePeriod();
		if (currentPeriod > lastPeriod) 
		{
			updateFlag = true;
			lastPeriod = currentPeriod;
		}
#pragma endregion

#pragma region ForTerminationAfterSimulationIsDone
		if (proxy->GetClientState() == Finished)
		{
			std::cout << "\n\n############################" << std::endl;
			std::cout << "Simulation runs are finished, please shut down API Client..." << std::endl;
			exit = true;
			std::cout << "############################" << std::endl << std::endl;
		}
		if (exit == true)
		{
			break;
		}
#pragma endregion
#pragma region ForMultipleRuns
		if (proxy->GetClientState() == NextRun)
		{
			laststep = -1;
		}
#pragma endregion
		
		int hoststate = proxy->GetHostState();
		cout<< "hoststate = " << hoststate <<endl;
//Get current simulation time step
		currentstep = proxy->GetServerTimestep();
		
		if (currentstep > laststep)
		{
			state = true;
			laststep = currentstep;
		}

		if ((hoststate == 1) && (state))
		{
			cout << "run step in:" << currentstep << endl;
			Debug << currentstep;

			//wcf_fvehicle_data = proxy->GetServerFVehicleData();
			//wcf_svehicle_data = proxy->GetServerSVehicleData();

			if ( (int(currentstep) <= 1) && (initcount == 1) )
			{
				cout << "Waiting for simulation start" << endl;
				cin >> command;

				initcount = 0;

				proxy->SetClientState(Continued);	
#if _USE_TCA
				proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
				Debug << std::endl;
			}

			else if ((int)(currentstep*10) <= 1)
			{

				proxy->SetClientState(Continued);
#if _USE_TCA
				proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
				Debug << std::endl;
			}
			else if ((int)(currentstep*10) > 1)
			{

#if _USE_TCA
				while ((proxy->GetTCAState() != TCAProcessingDone) && (proxy->GetTCAState() != SimulationFinished))
				{
				}
#endif

#pragma region StreetVehicleData
//				//Get street vehicle data from WCF server		
//				wcf_svehicle_data = proxy->GetServerSVehicleData();
//
//				//Get number of street vehicles
//				int nsvehicles = wcf_svehicle_data->Length;
//				std::cout << nsvehicles << " vehicles found on the street..." << std::endl;
//
//				//Do something about the vehicle data, add your algorithm here to process the vehicle trajectory
//				//for (int i = 0; i < nsvehicles; i++)
//				//{
//				//	std::cout << "wcf_svehicle_data[" << i << "]:" << std::endl;
//				//	std::cout << "wcf_svehicle_data[" << i << "].id = " << wcf_svehicle_data[i].id<< std::endl;
//				//	std::cout << "wcf_svehicle_data[" << i << "].link = " << wcf_svehicle_data[i].link<< std::endl;
//				//	std::cout << "wcf_svehicle_data[" << i << "].lane = " << wcf_svehicle_data[i].lane<< std::endl;
//				//	std::cout << "wcf_svehicle_data[" << i << "].speed = " << wcf_svehicle_data[i].speed<< std::endl;
//				//	std::cout << "wcf_svehicle_data[" << i << "].location = " << wcf_svehicle_data[i].location<< std::endl;
//				//	std::cout << std::endl;
//				//}
//				if (nsvehicles > 0)
//				{
//					std::cout << wcf_svehicle_data[nsvehicles - 1].speed << std::endl << std::endl;
//				}
#pragma endregion

#pragma region StreetLinkData
//				//Get street link data from WCF Server
//				wcf_sl = proxy->GetServerStreetData();
//
//				//Get number of street links
//				int nslinks = wcf_sl->Length;
//				std::cout << nslinks << " street links found..." << std::endl;
//
//				//Do something about the street links data
//				if (nslinks > 0)
//				{
//					std::cout << wcf_sl[nslinks - 1].id << std::endl << std::endl;
//				}
#pragma endregion

#pragma region ActuatedControllerData
				//Get Actuated Controller data from WCF Server
				wcf_acl = proxy->GetServerACData();

				//Get number of actuated controllers
				int nac = wcf_acl->Length;
				std::cout << nac << " actuated controllers found..." << std::endl;

				//Do something about the actuated controllers
				for (int i = 1; i <= nac; i++)
				{
					//std::cout << "green_phases = " << wcf_acl[i - 1].green_phases << std::endl << std::endl;
					//std::cout << "yellow_phases = " << wcf_acl[i - 1].yellow_phases << std::endl << std::endl;
					Debug << "," << i << "," << wcf_acl[i - 1].green_phases << "," << wcf_acl[i - 1].yellow_phases;
					Debug << "," << wcf_acl[i - 1].local_cycle_timer;
				}
				Debug << std::endl;
#pragma endregion

#pragma region GetSplit
				////Get split
				//array<float>^ wcf_splits = gcnew array<float>(8);
				//int split[8];
				//node_id = 10;
				//proxy->SetServerNodeID(node_id);
				//proxy->SetClientState(GetSplits);
				//while (proxy->GetClientState() != GotCoordinationData)
				//{
				//}
				//wcf_splits = proxy->GetServerSplits();
				//std::cout << "splits:" << std::endl;
				//for (int i = 0; i < 8; i++)
				//{
				//	split[i] = wcf_splits[i];
				//	std::cout << split[i] << " ";
				//}
				//std::cout << std::endl;
#pragma endregion

#pragma region GetAverageTravelTime
				//int usn = 6;
				//int dsn = 8;
				//proxy->SetServerUSNID(usn);
				//proxy->SetServerDSNID(dsn);
				//proxy->SetClientState(GetAverageTravelTime);
				//while (proxy->GetClientState() != GotAverageTravelTime)
				//{
				//}
				//float average_travel_time = proxy->GetServerAverageTravelTime();
				//std::cout << "average_travel_time = " << average_travel_time << std::endl;
#pragma endregion

#pragma region GetCycleLength
				//node_id = 16;
				//proxy->SetServerNodeID(node_id);
				//proxy->SetClientState(GetCycleLength);
				//while (proxy->GetClientState() != GotCoordinationData)
				//{
				//}
				//float cycle_length = proxy->GetServerCycleLength();
				//std::cout << "cycle_length = " << cycle_length << std::endl;
#pragma endregion

#pragma region GetOffset
				//node_id = 6;
				//proxy->SetServerNodeID(node_id);
				//proxy->SetClientState(GetOffset);
				//while (proxy->GetClientState() != GotCoordinationData)
				//{
				//}
				//float offset = proxy->GetServerOffset();
				//std::cout << "offset = " << offset << std::endl;
#pragma endregion

#pragma region GetLocalCycleTimmer
//node_id = 8;
//proxy->SetServerNodeID(node_id);
//proxy->SetClientState(GetLocalCycleTimer);
//while (proxy->GetClientState() != GotCoordinationData)
//{
//}
//float local_cycle_timer = proxy->GetServerLocalCycleTimer();
//cout<<"local_cycle_timer = " << local_cycle_timer <<endl;
#pragma endregion

#pragma region SetNewCycleLength
				//if (currentstep == 30)
				//{
				//	float new_cycle_length = 120;
				//	node_id = 6;
				//	proxy->SetServerNewCycleLength(node_id, new_cycle_length);
				//	proxy->SetClientState(SetNewCycleLength);
				//	while (proxy->GetClientState() != SetCoordinationDataDone)
				//	{
				//	}
				//	node_id = 8;
				//	proxy->SetServerNewCycleLength(node_id, new_cycle_length);
				//	proxy->SetClientState(SetNewCycleLength);
				//	while (proxy->GetClientState() != SetCoordinationDataDone)
				//	{
				//	}
				//	node_id = 10;
				//	proxy->SetServerNewCycleLength(node_id, new_cycle_length);
				//	proxy->SetClientState(SetNewCycleLength);
				//	while (proxy->GetClientState() != SetCoordinationDataDone)
				//	{
				//	}
				//	node_id = 14;
				//	proxy->SetServerNewCycleLength(node_id, new_cycle_length);
				//	proxy->SetClientState(SetNewCycleLength);
				//	while (proxy->GetClientState() != SetCoordinationDataDone)
				//	{
				//	}
				//	node_id = 16;
				//	proxy->SetServerNewCycleLength(node_id, new_cycle_length);
				//	proxy->SetClientState(SetNewCycleLength);
				//	while (proxy->GetClientState() != SetCoordinationDataDone)
				//	{
				//	}
				//}
#pragma endregion

#pragma region SetNewSplits
				if (currentstep == 900)
				{
					node_id = 6;
					array<float>^ wcf_new_splits6 = gcnew array<float>(8){ 27, 39, 0, 32, 27, 39, 0, 32 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits6);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 8;
					array<float>^ wcf_new_splits8 = gcnew array<float>(8){ 0, 38, 0, 60, 0, 38, 0, 60 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits8);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 10;
					array<float>^ wcf_new_splits10 = gcnew array<float>(8){ 15, 33, 20, 30, 15, 33, 20, 30 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits10);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 14;
					array<float>^ wcf_new_splits14 = gcnew array<float>(8){ 13, 32, 24, 29, 13, 32, 24, 29 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits14);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 16;
					array<float>^ wcf_new_splits16 = gcnew array<float>(8){ 17, 32, 0, 49, 17, 32, 0, 49 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits16);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
				}
				if (currentstep == 1880)
				{
					node_id = 6;
					array<float>^ wcf_new_splits6 = gcnew array<float>(8){ 17, 60, 0, 21, 17, 60, 0, 21 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits6);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 8;
					array<float>^ wcf_new_splits8 = gcnew array<float>(8){ 0, 62, 0, 36, 0, 62, 0, 36 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits8);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 10;
					array<float>^ wcf_new_splits10 = gcnew array<float>(8){ 14, 50, 14, 20, 14, 50, 14, 20 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits10);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 14;
					array<float>^ wcf_new_splits14 = gcnew array<float>(8){ 10, 51, 13, 24, 10, 51, 13, 24 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits14);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 16;
					array<float>^ wcf_new_splits16 = gcnew array<float>(8){ 12, 69, 0, 17, 12, 69, 0, 17 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits16);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
				}
				if (currentstep == 2860)
				{
					node_id = 6;
					array<float>^ wcf_new_splits6 = gcnew array<float>(8){ 26, 56, 0, 16, 26, 56, 0, 16 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits6);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 8;
					array<float>^ wcf_new_splits8 = gcnew array<float>(8){ 0, 49, 0, 49, 0, 49, 0, 49 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits8);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 10;
					array<float>^ wcf_new_splits10 = gcnew array<float>(8){ 10, 30, 21, 37, 10, 30, 21, 37 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits10);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 14;
					array<float>^ wcf_new_splits14 = gcnew array<float>(8){ 16, 28, 27, 27, 16, 28, 27, 27 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits14);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 16;
					array<float>^ wcf_new_splits16 = gcnew array<float>(8){ 34, 54, 0, 10, 34, 54, 0, 10 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits16);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
				}
				if (currentstep == 3840)
				{
					node_id = 6;
					array<float>^ wcf_new_splits6 = gcnew array<float>(8){ 33, 48, 0, 17, 33, 48, 0, 17 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits6);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 8;
					array<float>^ wcf_new_splits8 = gcnew array<float>(8){ 0, 54, 0, 44, 0, 54, 0, 44 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits8);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 10;
					array<float>^ wcf_new_splits10 = gcnew array<float>(8){ 16, 21, 28, 33, 16, 21, 28, 33 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits10);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 14;
					array<float>^ wcf_new_splits14 = gcnew array<float>(8){ 17, 20, 26, 35, 17, 20, 26, 35 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits14);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 16;
					array<float>^ wcf_new_splits16 = gcnew array<float>(8){ 31, 51, 0, 16, 31, 51, 0, 16 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits16);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
				}
				if (currentstep == 4820)
				{
					node_id = 6;
					array<float>^ wcf_new_splits6 = gcnew array<float>(8){ 31, 51, 0, 16, 31, 51, 0, 16 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits6);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 8;
					array<float>^ wcf_new_splits8 = gcnew array<float>(8){ 0, 59, 0, 39, 0, 59, 0, 39 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits8);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 10;
					array<float>^ wcf_new_splits10 = gcnew array<float>(8){ 21, 25, 25, 27, 21, 25, 25, 27 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits10);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 14;
					array<float>^ wcf_new_splits14 = gcnew array<float>(8){ 17, 21, 28, 32, 17, 21, 28, 32 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits14);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 16;
					array<float>^ wcf_new_splits16 = gcnew array<float>(8){ 27, 54, 0, 17, 27, 54, 0, 17 };
					proxy->SetServerNewSplits(node_id, wcf_new_splits16);
					proxy->SetClientState(SetNewSplits);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
				}
#pragma endregion

#pragma region SetNewOffset
				if (currentstep == 900)
				{
					node_id = 6;
					new_offset = 43;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 8;
					new_offset = 72;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 10;
					new_offset = 45;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 14;
					new_offset = 26;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 16;
					new_offset = 46;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
				}
				if (currentstep == 1880)
				{
					node_id = 6;
					new_offset = 48;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 8;
					new_offset = 70;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 10;
					new_offset = 50;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 14;
					new_offset = 21;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 16;
					new_offset = 45;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
				}
				if (currentstep == 2860)
				{
					node_id = 6;
					new_offset = 5;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 8;
					new_offset = 93;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 10;
					new_offset = 93;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 14;
					new_offset = 93;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 16;
					new_offset = 5;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
				}
				if (currentstep == 3840)
				{
					node_id = 6;
					new_offset = 5;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 8;
					new_offset = 93;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 10;
					new_offset = 93;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 14;
					new_offset = 5;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 16;
					new_offset = 93;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
				}
				if (currentstep == 4820)
				{
					node_id = 6;
					new_offset = 3;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 8;
					new_offset = 95;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 10;
					new_offset = 93;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 14;
					new_offset = 5;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
					node_id = 16;
					new_offset = 93;
					proxy->SetServerNewOffset(node_id, new_offset);
					proxy->SetClientState(SetNewOffset);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
				}
#pragma endregion


//Continue to next simulation step
				std::cout << "=====================" << std::endl;
				proxy->SetClientState(Continued);
#if _USE_TCA
				proxy->SetTCAState(GoodForNextSimulationStep);//continue for TCA
#endif

			}
		
			state = false;
		}
		else
		{
			cout << "Waiting for host side!" << endl;
		}
	}
//Continue to next simulation step
		std::cout << "=====================" << std::endl;
		proxy->SetClientState(Continued);
#if _USE_TCA
		proxy->SetTCAState(TCAContinued);//continue for TCA
#endif


	char terminate;
	std::cout << "WCF service is stopped." << std::endl;
	std::cout << "############################" << std::endl << std::endl;
	std::cout << "hit any key to terminate" << std::endl;
	std::cin >> terminate;
	proxy->SetClientState(APIShutDown);
#if _USE_TCA
	proxy->SetTCAState(TCAContinued);//continue for TCA
#endif

	return 0;

}






