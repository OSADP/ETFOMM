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
// APIClient.cpp : Defines the entry point for the console application.

#include "stdafx.h"
#include "Client_Default_Network.h"
#include "../include/WCFClientState.h"
#include <msclr\marshal_cppstd.h> //System string to std string
using namespace msclr::interop; // for System::String^ to std::string

#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#using <System.dll>
#using <System.ServiceModel.dll>


#define TIMESTEP_INTERVAL 1
#define PRINTOUT 0
#define _TCP_DIST 1

using namespace System;
using namespace System::ServiceModel;
using namespace System::IO;
using namespace WCFServer;
using namespace std;

void enterSingalsData(int *i, int node, int intv);
void updateFTCSignals(IService1^ proxy);
void displayFTCSignals(array<WCF_FTC_DATA> ^ ftc_etfomm);

void displayFreeways(array<Wcf_freeway_link> ^wcf_fwl);
void displayStreets(array<Wcf_street_link> ^wcf_sl);
void displayACSignals(array<Wcf_street_link> ^wcf_sl, array <WCF_AC> ^wcf_acl);
void displayNode(array<WCF_NODE_LOCATION_DATA> ^wcf_xy_coord_inputs, int NodeID);
void displayFVehicles(array<WCF_VFData> ^fvehicle_data);
void displaySVehicles(array<WCF_VSData> ^svehicle_data);

int getCommand();

void printFVehicleData(StreamWriter^ outFile, array<WCF_VFData> ^fvehicle_data);
void printSVehicleData(StreamWriter^ outFile, array<WCF_VSData> ^svehicle_data);
void printFreewayLinkData(StreamWriter^ outFile, array <Wcf_freeway_link> ^ wcf_fwl);
void printStreetLinkData(StreamWriter^ outFile, array <Wcf_street_link> ^ wcf_sl);
void printACSignals(StreamWriter^ outFile, array<WCF_AC> ^ wcf_acl);
void printFTCSignals(StreamWriter^ outFile, array<WCF_FTC_DATA>^ wcf_ftc);
void printEntryNodes(StreamWriter^ outFile, array <WCF_ENTRYNODES_DATA> ^ wcf_entry_node);
void printRampMeter(StreamWriter^ outFile, array<WCF_RM_DATA>^ wcf_rampmeter_inputs);

void printNetworkInputs(StreamWriter^ outFile, array <WCF_NETWORK_INPUTS> ^ wcf_ni);
void printFNetworkInputs(StreamWriter^ outFile, array <WCF_FREEWAY_NETWORK_INPUTS> ^ wcf_fni);
void printSNetworkInputs(StreamWriter^ outFile, array <WCF_STREET_NETWORK_INPUTS> ^ wcf_sni);
void printVTypeInputs(StreamWriter^ outFile, array <WCF_VEHICLE_TYPE_DATA> ^ wcf_vti);
// for both freeway and street detectors
void printDetectorInputs(StreamWriter^ outFile, String^ type, array <WCF_DETECTOR_INPUTS> ^ wcf_di);
void printCondTurnpctInputs(StreamWriter^ outFile, array<WCF_COND_TURNPCTS>^ wcf_ct);
void printBusRouteInputs(StreamWriter^ outFile, array<WCF_BUSROUTE_DATA>^ wcf_busroute_inputs);
void printBusStationInputs(StreamWriter^ outFile, array<WCF_BUSSTATION_DATA>^ wcf_busstation_inputs);
void printBusStationInputs(StreamWriter^ outFile, array<WCF_BUSSTATION_DATA>^ wcf_busstation_inputs);
void printIncidentInputs(StreamWriter^ outFile, array<WCF_INCIDENT_DATA>^ wcf_incident_inputs);
void printCoordInputs(StreamWriter^ outFile, array<WCF_NODE_LOCATION_DATA>^ wcf_xy_coord_inputs);
void printParkingZones(StreamWriter^ outFile, array<WCF_PARKING_DATA>^ wcf_parking_zones);
void printEvents(StreamWriter^ outFile, array<WCF_EVENT_DATA>^ wcf_events);
void printDiversions(StreamWriter^ outFile, array<WCF_DIVERSION_DATA>^ wcf_diversions);

int _tmain(int argc, _TCHAR* argv[])
{

	int vid, drivetype,link,lane;
	float location;
	int n, node;
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
	
	//coordination data
	int node_id;
	float cycle_length;
	float new_cycle_length;
	float offset;
	float new_offset;
	float local_cycle_timer;
	float splits[8];
	float new_splits[8];
	float min_splits[8];
	array<float>^ wcf_splits = gcnew array<float>(8);
	array<float>^ wcf_new_splits = gcnew array<float>(8);
	array<float>^ wcf_min_splits = gcnew array<float>(8);
	array<int>^ wcf_greens_yellows = gcnew array<int>(2);
	int transition_method;
	float max_add, max_subt;
	int iact, method;
	//AverageTravelTime
	int usn_id, dsn_id;
	float AverageTravelTime;
	array<WCF_STREET_MOE_DATA>^ wcf_street_moe_data = gcnew array<WCF_STREET_MOE_DATA>(1);
	
	String^ dataPath = "APIOutput\\";
	String^ fileName1 = "Before_1";
	int fileCount1 = 0;
	String^ fileName2 = "After_6";
	int fileCount2 = 0;
	String^ tmpName = "";
	//ofstream TCA_TEST_OUTPUT_FILE;
	//TCA_TEST_OUTPUT_FILE.open("TCA_TEST_OUTPUT.dat");

	/*The Client side to connect WCF Host. We want to Client side could modify data in the server side.
	  Client Configuration to connect the server host. Configuration of binding, address*/
#ifdef _TCP_DIST
	String^ ServerIP;
	Console::Write(L"Please enter the IP address of the server (etRunner64): \n");
	Console::WriteLine(L"localhost\n192.168.0.100\n74.83.129.66\nwebapp.etfomm.com\n");
#ifndef _FASTTEST
    ServerIP = Console::ReadLine();
#else
	ServerIP = "localhost";
	Console::WriteLine(ServerIP);
#endif
#endif
	
	cout<<"connecting to WCF server... "<<endl;	
	    
#if WSHTTPBINDING
	WSHttpBinding^ binding = gcnew WSHttpBinding();
	binding->MaxBufferPoolSize = 2147483647;
	binding->MaxReceivedMessageSize = 2147483647; 
	binding->Security->Mode = System::ServiceModel::SecurityMode::None;
	EndpointAddress^ address = gcnew EndpointAddress(String::Format(L"http://localhost:8000/service", Environment::MachineName));
	//EndpointAddress^ address = gcnew EndpointAddress(String::Format(L"http://eteditor.cloudapp.net:80/service", Environment::MachineName)); //This one works with http://localhost:80/service in etRunner
#endif

#if NETTCPBINDING
	NetTcpBinding^ binding = gcnew NetTcpBinding();
	binding->MaxBufferPoolSize = 2147483647;
	binding->MaxReceivedMessageSize = 2147483647; 
#ifdef _TCP_DIST
	binding->Security->Mode = System::ServiceModel::SecurityMode::None;
	String^ serviceAddr = String::Concat(L"net.tcp://", ServerIP, L":6000/service");
	EndpointAddress^ address = gcnew EndpointAddress(String::Format(serviceAddr, Environment::MachineName));
#else
	binding->Security->Mode = System::ServiceModel::SecurityMode::None;
	//binding->TransferMode = System::ServiceModel::TransferMode::Streamed;
	EndpointAddress^ address = gcnew EndpointAddress(String::Format(L"net.tcp://localhost:6000/service", Environment::MachineName));
	//EndpointAddress^ address = gcnew EndpointAddress(String::Format(L"net.tcp://eteditor.cloudapp.net:6000/service", Environment::MachineName)); //This one works with net.tcp://localhost:6000/service in etRunner
#endif
	TimeSpan *TimeOutSetting = new TimeSpan(1, 0, 0);
	binding->OpenTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
    binding->CloseTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
    binding->SendTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
    binding->ReceiveTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
#endif

#if UDPBINDING
	UdpBinding^ binding = gcnew UdpBinding();
	binding->MaxBufferPoolSize = 2147483647;
	binding->MaxReceivedMessageSize = 2147483647;

	TimeSpan *TimeOutSetting = new TimeSpan(1, 0, 0);
	binding->OpenTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
	binding->CloseTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
	binding->SendTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
	binding->ReceiveTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
	binding->TimeToLive = 255;

	//String^ ServerIP;
	//Console::WriteLine(L"Please enter the IP address of the server: ");
	//Console::WriteLine(L"localhost\n192.168.0.100\n74.83.129.66\neteditor.cloudapp.net\n");
	//ServerIP = Console::ReadLine();
	String^ serviceAddr = String::Concat(L"soap.udp://", ServerIP, L":8080/service");
	EndpointAddress^ address = gcnew EndpointAddress(String::Format(serviceAddr, Environment::MachineName));
#endif
	ChannelFactory<IService1^>^ factory = gcnew ChannelFactory<IService1^>(binding, address);
	IService1^ proxy = factory->CreateChannel();
	std::cout<<"Communication channel is completed."<<endl;

	float interval;
	proxy->SetNumberOfConnectedClients(1);
	proxy->SetClientState(RequestingUpdate);
	array <NewVehicle> ^ newVehicle = gcnew array<NewVehicle>(1);
	array<WCF_VFData> ^fvehicle_data = gcnew array<WCF_VFData>(1);
	array<WCF_VSData> ^svehicle_data = gcnew array<WCF_VSData>(1);
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
	array<WCF_COORDINATION_DATA>^ wcf_coordination_data = gcnew array<WCF_COORDINATION_DATA>(1);

	bool exit = false;
	int command = 0;
	
	//std::cout << "\n\n############################" << std::endl;
	//std::cout << "Will you use a 2070 Signal Controller? (Y/N)" << std::endl;
	int useControllerFlag = 0;
	char InputCharacter = 'N';

	//if etRunner input <w> then use controller
	if (proxy->GetControllerDataState())
	{
		useControllerFlag = 1;
	}

	if (useControllerFlag)
	{
		//If use 2070 controller, ask if start multiple NTCIP Interface .exe
		std::cout << "\n\n############################" << std::endl;
		std::cout << "Using 2070 Controller..." << std::endl;
		std::cout << "############################" << std::endl;
		std::cout << "Would you want to start up multiple NTCIPInterface programs? (Y/N)" <<std::endl;
		std::cin >> InputCharacter;

		if (InputCharacter == 'Y' || InputCharacter == 'y')
		{
			std::string str_StartCommand = "start ";
			std::string str_EXEname = "..\\etRunnerControllerInterface\\NTCIPInterface.exe";
			std::string str_CtrlrIPListFileName;
			std::ifstream DataFile;
			std::string str_commandline;
			int n_controllers = 0;

			std::cout << "\n\n############################" << std::endl;
			//std::cout << "Please specify the NTCIPInterface.exe path, if want to use default path (./etRunnerControllerInterface/NTCIPInterface.exe), enter \"Y\" and press \"Enter\"" << std::endl;
			std::cout << "Please enter the path and .exe name of the NTCIPInterface.exe" << std::endl;
			std::cout << "Using default: \"\\etRunnerControllerInterface\\NTCIPInterface.exe\" enter \'Y\',\notherwise enter the full path" << std::endl;
			std::cin >> str_EXEname;
			if (str_EXEname == "Y" || str_EXEname == "y")
			{
				str_EXEname = "\\etRunnerControllerInterface\\NTCIPInterface.exe";
			}
			else if (str_EXEname == "t")
			{
				str_EXEname = "C:\\Users\\NGSIM-1\\Dropbox\\NGSIM\\code\\NTCIPInterface\\etRunnerControllerInterface\\NTCIPInterface.exe";
			}
			
			//Get the path of the ControllerInput.txt
//			std::cout << "\n\n############################" << std::endl;
//			std::cout << "Please enter the path and file name of the Controller IP list text file (ControllerInput.txt):" << std::endl;
			//std::string CtrlrIPListFileName = msclr::interop::marshal_as<std::string>(InputFileName);
			
			str_CtrlrIPListFileName = /*C:\\Users\\NGSIM-1\\Dropbox\\NGSIM\\code\\etRunner64Web\\x64\\Release_TCP_Controller\\*/"ControllerInput.txt";
			//std::cin >> CtrlrIPListFileName;
//			std::cout << "\n\n############################" << std::endl;
//			std::cout << "The file ControllerInput.txt path/name you entered is: " << std::endl << str_CtrlrIPListFileName << std::endl;
			
			DataFile.open(str_CtrlrIPListFileName.c_str());

			//Start multiple NTCIP Interface .exe
			
			DataFile >> n_controllers;				// first line, number of controllers
			std::getline(DataFile, str_commandline);

			//Start the NTCIP Interface .exe one by one
			for (int i = 0; i < n_controllers; i++)
			{
				
				std::getline(DataFile, str_commandline);
				str_StartCommand = "start " + str_EXEname + " " + str_commandline;
				std::cout << "\n\n############################" << std::endl;
				std::cout << "Starting controller number " << i + 1 <<  ", command: \n\n" << str_StartCommand << std::endl;
				system(str_StartCommand.c_str());
			}
		}
		else
		{
			std::cout << "\n\n############################" << std::endl;
			std::cout << "Please start all the NTCIP Interface .exe and press \"enter\" to continue" << std::endl;
			//std::cin >> InputCharacter;
			std::cin.get();
		}

		interval = -1;
	}
	else //If not using controller
	{
		//enter timestep interval for etfomm to pause
		std::cout << "\n\n############################" << std::endl;
		std::cout<<"Please enter the timestep interval for etfomm to pause for API client input. "<<endl;
#ifndef _FASTTEST
		std::cin >> interval;
#else
		interval = 40;
		std::cout << interval << std::endl;
#endif
	}
	
	proxy->SetAPITimestepInterval(interval);

	std::cout << "\n\n############################" << std::endl;
	std::cout << "Would you like to generate text files at the end of simulation? (Y/N) " << std::endl;
	int writeTextFlag = 0;
	char GPSFlag = 'N';
#ifndef _FASTTEST
	std::cin >> GPSFlag;
#else
	GPSFlag = 'Y';
	std::cout << GPSFlag << std::endl;
#endif
	
	if (GPSFlag == 'Y' || GPSFlag == 'y')
	{
		writeTextFlag = 1;
	}
	proxy->SetServerWriteTextFlag(writeTextFlag);

	//std::cout << "\n\n############################" << std::endl;
	//std::cout << "Please choose input source:" << std::endl;
	//std::cout << "	Press <C> to use client defined default network." << std::endl;
	//std::cout << "	Press <F> to UseTRFFile." << std::endl;
	//
	//std::cout<<'F'<<std::endl;
	t = 'F';
	//while (std::cin >> t)
	while (1)
	{
		if (t == 'H')
		{
			//proxy->SetClientState(UseHostDefineNetwork);
			HostDefineNetworkFlag = true;
			break;
		}
		else if (t == 'C')
		{
			//proxy->SetClientState(UseClientDefineNetwork);
			ClientDefineNetworkFlag = true;
			break;
		}
		else if (t == 'F')
		{
			TRFFileFlag = true;
			break;
		}
		else
		{
			std::cout<<"please enter again"<<std::endl;
		}
	}
	
	while (!exit)
	{
		
			
		if (ClientDefineNetworkFlag == true || proxy->GetClientState() == NextRun)
		{
			System::Array::Resize(wcf_xy_coord_inputs, 7002);//7002
			Client_Default_Network_define_xy_coord_inputs(wcf_xy_coord_inputs);
			proxy->SetServerXYCoordInputs(wcf_xy_coord_inputs);
			Client_Default_Network_define_Network_INPUTS(wcf_ni);
			proxy->SetServerNetworkInput(wcf_ni);
 			Client_Default_Network_define_FREEWAY_NETWORK_INPUTS(wcf_fni);
 			proxy->SetServerFreewayNetworkInput(wcf_fni);
 			Client_Default_Network_define_STREET_NETWORK_INPUTS(wcf_sni);
 			proxy->SetServerStreetNetworkInput(wcf_sni);
 			System::Array::Resize(wcf_vti, 9);
 			Client_Default_Network_define_Vehicle_Type_Inputs(wcf_vti);
 			proxy->SetServerVehicleTypeInputs(wcf_vti);
			System::Array::Resize(wcf_fwl, 10);
			Client_Default_Network_define_freeway_link_data(wcf_fwl);
			proxy->SetServerFreewayData(wcf_fwl);
			System::Array::Resize(wcf_sl,22);
			Client_Default_Network_define_street_link_data(wcf_sl);
			proxy->SetServerStreetData(wcf_sl);
			System::Array::Resize(wcf_ct, 1);
			Client_Default_Network_define_cond_turnpct_data(wcf_ct);
			proxy->SetServerCondTurnpctData(wcf_ct);
			System::Array::Resize(wcf_entry_node, 9);
			Client_Default_Network_define_entrynode_inputs(wcf_entry_node);
			proxy->SetServerEntryNodeData(wcf_entry_node);
			System::Array::Resize(wcf_ftc, 10);
			Client_Default_Network_define_ftc_data_inputs(wcf_ftc);
			proxy->SetServerFTCSignalData(wcf_ftc);
 			//System::Array::Resize(wcf_acl, 1);
 			//Client_Default_Network_define_ac_data_inputs(wcf_acl);
 			//proxy->SetServerACData(wcf_acl);
			System::Array::Resize(wcf_fdet_inputs, 1);
			Client_Default_Network_define_fdet_inputs(wcf_fdet_inputs);
			proxy->SetServerFreewayDetectorInputs(wcf_fdet_inputs);
 			System::Array::Resize(wcf_sdet_inputs, 5);
 			Client_Default_Network_define_sdet_inputs(wcf_sdet_inputs);
 			proxy->SetServerStreetDetectorInputs(wcf_sdet_inputs);
			System::Array::Resize(wcf_busroute_inputs, 1);
			Client_Default_Network_define_busroute_inputs(wcf_busroute_inputs);
			proxy->SetServerBusRouteInputs(wcf_busroute_inputs);
			System::Array::Resize(wcf_busstation_inputs, 1);
			Client_Default_Network_define_busstation_inputs(wcf_busstation_inputs);
			proxy->SetServerBusStationInputs(wcf_busstation_inputs);
			System::Array::Resize(wcf_xy_coord_inputs, 8999);//7002
			Client_Default_Network_define_xy_coord_inputs(wcf_xy_coord_inputs);
			proxy->SetServerXYCoordInputs(wcf_xy_coord_inputs);
			System::Array::Resize(wcf_rampmeter_inputs, 1);
			Client_Default_Network_define_rampmeter_inputs(wcf_rampmeter_inputs);
			proxy->SetServerRampmeterInputs(wcf_rampmeter_inputs);
			System::Array::Resize(wcf_incident_data_inputs, 1);
			Client_Default_Network_define_incident_data_inputs(wcf_incident_data_inputs);
			proxy->SetServerIncidentData_Inputs(wcf_incident_data_inputs);

			System::Array::Resize(wcf_parking_zone_inputs, 1);
			Client_Default_Network_define_parking_zone_inputs(wcf_parking_zone_inputs);
			proxy->SetServerParkingData(wcf_parking_zone_inputs);
			System::Array::Resize(wcf_event_inputs, 1);
			Client_Default_Network_define_event_inputs(wcf_event_inputs);
			proxy->SetServerEventData(wcf_event_inputs);
			System::Array::Resize(wcf_diversion_inputs, 1);
			Client_Default_Network_define_diversion_inputs(wcf_diversion_inputs);
			proxy->SetServerDiversionData(wcf_diversion_inputs);
			
			#if PRINTOUT
			tmpName = dataPath + fileName1 + ".dat";
			StreamWriter^ outputFile = gcnew StreamWriter(tmpName);
			printFreewayLinkData(outputFile, wcf_fwl);
			printStreetLinkData(outputFile, wcf_sl);
			printACSignals(outputFile, wcf_acl);
			printFTCSignals(outputFile, wcf_ftc);
			printEntryNodes(outputFile, wcf_entry_node);
			printRampMeter(outputFile, wcf_rampmeter_inputs);

			printNetworkInputs(outputFile, wcf_ni);
			printFNetworkInputs(outputFile, wcf_fni);
			printSNetworkInputs(outputFile, wcf_sni);
			printVTypeInputs(outputFile, wcf_vti);
			printDetectorInputs(outputFile, "FREEWAY", wcf_fdet_inputs);
			printDetectorInputs(outputFile, "STREET", wcf_sdet_inputs);
			printCondTurnpctInputs(outputFile, wcf_ct);
			printBusRouteInputs(outputFile, wcf_busroute_inputs);
			printBusStationInputs(outputFile, wcf_busstation_inputs);
			printIncidentInputs(outputFile, wcf_incident_data_inputs);
			printCoordInputs(outputFile, wcf_xy_coord_inputs);

			printParkingZones(outputFile, wcf_parking_zone_inputs);
			printEvents(outputFile, wcf_event_inputs);
			printDiversions(outputFile, wcf_diversion_inputs);

			outputFile->Close();
			#endif
			//client define network
			ClientDefineNetworkFlag = false;
			proxy->SetClientState(UseClientDefineNetwork);
		}
		else if (HostDefineNetworkFlag == true)
		{
			HostDefineNetworkFlag = false;
			proxy->SetClientState(UseHostDefineNetwork);
		}
		else if (TRFFileFlag)
		{
			std::cout << "\n\n############################" << std::endl;
			std::cout << "Please enter the path and file name for TRF file: " << std::endl;
			std::string TRFFile = "";
			//TRFFile = "C:\\coordination_test_new_format.trf";
#ifndef _FASTTEST
			std::cin >> TRFFile;
#else
			//TRFFile = "D:\\Genetic.trf";
			//TRFFile = "D:\\IBNN_WD_AM_5RT9-P01_etfomm.trf";
			TRFFile = "D:\\Starkville182DemoS3v2.TRF";
			//TRFFile = "D:\\us78_302s11_addedmedian_TSv3.TRF";
			//TRFFile = "D:\\freewayweavingtestV5.trf";
			std::cout << TRFFile << std::endl;
#endif
			
			
			String ^TRFFileName = gcnew String(TRFFile.c_str());
			TRFFileFlag = false;
			proxy->SetServerTRFFile(TRFFileName);
			proxy->SetClientState(UseTRFFile);
		}

		currentPeriod = proxy->GetServerTimePeriod();
		if (currentPeriod > lastPeriod) 
		{
			updateFlag = true;
			lastPeriod = currentPeriod;
		}


		StreamWriter ^outputFile3;
		switch(command) //use more meaningful char or use enum/string
		{			
		case 0:
			break;
		//cout<<"*** Basic Commands ***" <<endl;
		//cout<<"=\t enter <1> to continue;"<<endl;
		case 1:
			proxy->SetClientState(Continued);
			break;
		//cout<<"=\t enter <2> to finish current run w/o pause;"<<endl;
		case 2:
			proxy->SetClientState(NoRequest);
			break;
		// cout<<"=\t enter <3> to shut down;"<<endl;
		case 3:
			proxy = factory->CreateChannel();
			proxy->SetClientState(APIShutDown);
			exit = true;
			break;
		//cout<<"*** Auto-modify Commands ***" <<endl;
		//cout<<"=\t enter <100> to test the change of all data;" <<endl;
		case 100:
			if (!updateFlag) 
			{
				std::cout << "Data update is only allowed at the beginning of time period." << std::endl;
			} else {
				fvehicle_data = proxy->GetServerFVehicleData();
				fvehicle_data[0].desiredspeed += 0.1;//user can change everything in the array/struct
				proxy->SetServerFVehicleData(fvehicle_data);

				svehicle_data = proxy->GetServerSVehicleData();
				svehicle_data[0].desiredspeed -= 0.1;//user can change everything in the array/struct
				proxy->SetServerSVehicleData(svehicle_data);

				wcf_fwl = proxy->GetServerFreewayData();
				wcf_fwl[1].thru_percent = 40; //This relates to link (102,103) --- other inputs stay the same
				proxy->SetServerFreewayData(wcf_fwl);

				wcf_sl = proxy->GetServerStreetData();
				wcf_sl[0].leftpct = 10; //This relates to link (1,5) --- other inputs stay the same
				wcf_sl[0].thrupct = 80;
				wcf_sl[0].rightpct = 10;
				proxy->SetServerStreetData(wcf_sl);

				wcf_acl = proxy->GetServerACData();
				//wcf_acl = proxy->GetServerACData();
				wcf_acl[0].cycle_length = 60;
				proxy->SetServerACData(wcf_acl);

				wcf_ftc = proxy->GetServerFTCSignalData();
				wcf_ftc[4].active_intervals = 7;
				wcf_ftc[4].duration[3] = 25; //This relates to the signal at node 5
				wcf_ftc[4].duration[6] = 20;
				wcf_ftc[4].cycle_length = 0;
				for (int idua = 0; idua < wcf_ftc[4].active_intervals; ++idua)
				{
					wcf_ftc[4].cycle_length += wcf_ftc[4].duration[idua];
				};
				proxy->SetServerFTCSignalData(wcf_ftc);

				wcf_entry_node = proxy->GetServerEntryNodeData();
				wcf_entry_node[7].truck_pct = 0.15;
				wcf_entry_node[7].carpool_pct = 0.1;
				wcf_entry_node[7].hov_violators_per10000 = 200;
				wcf_entry_node[7].flowrate = 300; //This relates to entrynode 107 --- other values stay the same
				wcf_entry_node[7].lane_pct[0] = 30;
				wcf_entry_node[7].lane_pct[1] = 70;
				proxy->SetServerEntryNodeData(wcf_entry_node);

				wcf_rampmeter_inputs = proxy->GetServerRampmeterInputs();
				wcf_rampmeter_inputs[0].onset = 120;
				wcf_rampmeter_inputs[0].headway[0] = 11.0;
				wcf_rampmeter_inputs[0].twopergreen = true;
				proxy->SetServerRampmeterInputs(wcf_rampmeter_inputs);

#if PRINTOUT
				tmpName = dataPath + "MOD_" + fileName1 + ".dat";
				outputFile3 = gcnew StreamWriter(tmpName);
				printFreewayLinkData(outputFile3, wcf_fwl);
				printStreetLinkData(outputFile3, wcf_sl);
				printACSignals(outputFile3, wcf_acl);
				printFTCSignals(outputFile3, wcf_ftc);
				printEntryNodes(outputFile3, wcf_entry_node);
				printRampMeter(outputFile3, wcf_rampmeter_inputs);
				outputFile3->Close();
#endif

				proxy->SetClientState(UpdateAll);
			}
			break;
#if PRINTOUT
		//cout<<"*** Print Commands ***" <<endl;
		//cout<<"=\t enter <200> to print all data to file;"<<endl;
		case 200: {
			cout << "printing text file begins..." << endl;
			
			tmpName = dataPath + fileName2 + ".dat";
			fileCount2++;
			StreamWriter^ outputFile2 = gcnew StreamWriter(tmpName);
			
			fvehicle_data = proxy->GetServerFVehicleData();
			printFVehicleData(outputFile2, fvehicle_data);

			svehicle_data = proxy->GetServerSVehicleData();
			printSVehicleData(outputFile2, svehicle_data);

			wcf_fwl = proxy->GetServerFreewayData();
			printFreewayLinkData(outputFile2, wcf_fwl);

			wcf_sl = proxy->GetServerStreetData();
			printStreetLinkData(outputFile2, wcf_sl);

			wcf_acl = proxy->GetServerACData();
			printACSignals(outputFile2, wcf_acl);

			wcf_ftc = proxy->GetServerFTCSignalData();
			printFTCSignals(outputFile2, wcf_ftc);

			wcf_entry_node = proxy->GetServerEntryNodeData();
			printEntryNodes(outputFile2, wcf_entry_node);

			wcf_rampmeter_inputs = proxy->GetServerRampmeterInputs();
			printRampMeter(outputFile2, wcf_rampmeter_inputs);

			wcf_ni = proxy->GetServerNetworkInput();
			printNetworkInputs(outputFile2, wcf_ni);

			wcf_fni = proxy->GetServerFreewayNetworkInput();
			printFNetworkInputs(outputFile2, wcf_fni);

			wcf_sni = proxy->GetServerStreetNetworkInput();
			printSNetworkInputs(outputFile2, wcf_sni);

			wcf_vti = proxy->GetServerVehicleTypeInputs();
			printVTypeInputs(outputFile2, wcf_vti);

			wcf_fdet_inputs = proxy->GetServerFreewayDetectorInputs();
			printDetectorInputs(outputFile2, "FREEWAY", wcf_fdet_inputs);

			wcf_sdet_inputs = proxy->GetServerStreetDetectorInputs();
			printDetectorInputs(outputFile2, "STREET", wcf_sdet_inputs);

			wcf_ct = proxy->GetServerCondTurnpctData();
			printCondTurnpctInputs(outputFile2, wcf_ct);

			wcf_busroute_inputs = proxy->GetServerBusRouteInputs();
			printBusRouteInputs(outputFile2, wcf_busroute_inputs);

			wcf_busstation_inputs = proxy->GetServerBusStationInputs();
			printBusStationInputs(outputFile2, wcf_busstation_inputs);

			wcf_incident_data_inputs = proxy->GetServerIncidentData_Inputs();
			printIncidentInputs(outputFile2, wcf_incident_data_inputs);

			wcf_xy_coord_inputs = proxy->GetServerXYCoordInputs();
			printCoordInputs(outputFile2, wcf_xy_coord_inputs);

			wcf_parking_zone_inputs = proxy->GetServerParkingData();
			printParkingZones(outputFile2, wcf_parking_zone_inputs);

			wcf_event_inputs = proxy->GetServerEventData();
			printEvents(outputFile2, wcf_event_inputs);

			wcf_diversion_inputs = proxy->GetServerDiversionData();
			printDiversions(outputFile2, wcf_diversion_inputs);

			outputFile2->Close();
			cout << "printing text file finished." << endl;
			break;}
		//cout<<"=\t enter <201> to print freeway vehicle data;"<<endl;
		case 201:
			fvehicle_data = proxy->GetServerFVehicleData();
			tmpName = dataPath + "MOD_FV_" + fileName2 + ".dat";
			outputFile3 = gcnew StreamWriter(tmpName);
			printFVehicleData(outputFile3, fvehicle_data);
			outputFile3->Close();
			break;
		//cout<<"=\t enter <202> to print street vehicle data;"<<endl;
		case 202:
			svehicle_data = proxy->GetServerSVehicleData();
			tmpName = dataPath + "MOD_SV_" + fileName2 + ".dat";
			outputFile3 = gcnew StreamWriter(tmpName);
			printSVehicleData(outputFile3, svehicle_data);
			outputFile3->Close();
			break;
		//cout<<"=\t enter <203> to print freeway link data;"<<endl;
		case 203:
			wcf_fwl = proxy->GetServerFreewayData();
			tmpName = dataPath + "MOD_FL_" + fileName2 + ".dat";
			outputFile3 = gcnew StreamWriter(tmpName);
			printFreewayLinkData(outputFile3, wcf_fwl);
			outputFile3->Close();
			break;
		//cout<<"=\t enter <204> to print Street link data and AC signal data;"<<endl;
		case 204:
			wcf_sl = proxy->GetServerStreetData();
			wcf_acl = proxy->GetServerACData();
			tmpName = dataPath + "MOD_SL_" + fileName2 + ".dat";
			outputFile3 = gcnew StreamWriter(tmpName);
			printStreetLinkData(outputFile3, wcf_sl);
			printACSignals(outputFile3, wcf_acl);
			outputFile3->Close();
			break;
		//cout<<"=\t enter <205> to print FTC signal data;"<<endl;
		case 205:
			wcf_ftc = proxy->GetServerFTCSignalData();
			tmpName = dataPath + "MOD_FTC_" + fileName2 + ".dat";
			outputFile3 = gcnew StreamWriter(tmpName);
			printFTCSignals(outputFile3, wcf_ftc);
			outputFile3->Close();
			break;
		//cout<<"=\t enter <206> to print Entry Node data;"<<endl;
		case 206:
			wcf_entry_node = proxy->GetServerEntryNodeData();
			tmpName = dataPath + "MOD_EN_" + fileName2 + ".dat";
			outputFile3 = gcnew StreamWriter(tmpName);
			printEntryNodes(outputFile3, wcf_entry_node);
			outputFile3->Close();
			break;
		//cout<<"=\t enter <207> to print Ramp Meter data;"<<endl;
		case 207:
			wcf_rampmeter_inputs = proxy->GetServerRampmeterInputs();
			tmpName = dataPath + "MOD_RM_" + fileName2 + ".dat";
			outputFile3 = gcnew StreamWriter(tmpName);
			printRampMeter(outputFile3, wcf_rampmeter_inputs);
			outputFile3->Close();
			break;
#endif
		//cout<<"*** Display Commands ***" <<endl;
		//cout<<"=\t enter <301> to display freeway vehicle data;"<<endl;
		case 301:
			fvehicle_data = proxy->GetServerFVehicleData();
			displayFVehicles(fvehicle_data);
			break;
		//cout<<"=\t enter <302> to display street vehicle data;"<<endl;
		case 302:
			svehicle_data = proxy->GetServerSVehicleData();
			displaySVehicles(svehicle_data);
			break;
		//cout<<"=\t enter <303> to display freeway links data;"<<endl;
		case 303:
			wcf_fwl = proxy->GetServerFreewayData();
			displayFreeways(wcf_fwl);
			break;
		//cout<<"=\t enter <304> to display street links data and AC signals data;"<<endl; 
		case 304:
			wcf_sl = proxy->GetServerStreetData();
			displayStreets(wcf_sl);
			wcf_acl = proxy->GetServerACData();
			displayACSignals(wcf_sl, wcf_acl);
			break;
		//cout<<"=\t enter <305> to display FTC signals data;"<<endl; 
		case 305:
			wcf_ftc = proxy->GetServerFTCSignalData();
			displayFTCSignals(wcf_ftc);
			break;
		//cout<<"=\t enter <308> to display Node XY Coordinate;"<<endl;
		case 308:	
			int NodeId;
			std::cout<<"enter the node ID to display"<<std::endl;
			cin>>NodeId;
			wcf_xy_coord_inputs = proxy->GetServerXYCoordInputs();
			displayNode(wcf_xy_coord_inputs,NodeId);
			break;
		//cout<<"*** Manual-modify Commands ***" <<endl;
		//cout<<"=\t enter <401> to change freeway vehicle data;"<<endl;
		case 401: 
			fvehicle_data = proxy->GetServerFVehicleData();
			cout<<"enter vehicle id, driver type, speed, location, link, and lane "<<endl;
			cout<<"(one value per line) "<<endl;
			cin>>vid>>drivetype>>speed>>location>>link>>lane;
			int bVehicleFound;
			bVehicleFound = 0;
			for (int i = 0; i < fvehicle_data->Length; i++)
			{
				if (fvehicle_data[i].id == vid)
				{
					fvehicle_data[i].id = vid;
					fvehicle_data[i].drivertype=drivetype;
					fvehicle_data[i].speed=speed;
					fvehicle_data[i].location=location;
					fvehicle_data[i].link=link;
					fvehicle_data[i].lane=lane;
					bVehicleFound = 1;
				}
			}
			if (bVehicleFound) {
				proxy->SetServerFVehicleData(fvehicle_data);
				proxy->SetClientState(VehicleUpdateSubmitted);
			} else {
				std::cout << "Vehicle ID: " << vid	<< ", not found on freeway, information not changed..." << std::endl;
			}
			break;
		//cout<<"=\t enter <402> to add a new vehicle;"<<endl;
		case 402: 	
			//add a new vehicle
			int input;
			newVehicle[0].time = proxy->GetServerTimestep();
			cout<<"Current timestep is "<<newVehicle[0].time << " and the new vehicle time is "<< newVehicle[0].time + TIMESTEP_INTERVAL <<endl;
			newVehicle[0].time = newVehicle[0].time + TIMESTEP_INTERVAL;
			cout<<"enter entry node id for this vehicle (e.g. \"8001\")"<<endl;
			cin >> input;
			newVehicle[0].inode = input;
			cout<<"enter path id for this vehicle (e.g. \"0\" or use <403> to add more paths)"<<endl;
			cin >> input;	
			newVehicle[0].pathid = input;
			cout<<"enter driver for this vehicle (e.g. \"0\")"<<endl;
			cin >> input;	
			newVehicle[0].driver = input;
			cout<<"enter fleet for this vehicle (e.g. \"0\")"<<endl;
			cin >> input;	
			newVehicle[0].fleet = input;
			cout<<"enter vehicle type for this vehicle (e.g. \"1\")"<<endl;
			cin >> input;	
			newVehicle[0].vtype = input;
			cout<<"enter overspeed for this vehicle ((e.g. \"0\", only applies to emergency vehicle to exceed the free-flow speed)"<<endl;
			cin >> input;	
			newVehicle[0].overspeed = input;
			cout<<"enter range for this vehicle (e.g. \"0\")"<<endl;
			cin >> input;	
			newVehicle[0].range = input;		
			proxy->SetServerNewVehicleData(newVehicle);
			proxy->SetClientState(AddVehicleSubmitted);
			break;
		//cout<<"=\t enter <403> to add path;"<<endl;		
		case 403: 	
			// add path		
			cout<<"enter number of nodes on the path "<<endl;
			cin>>n;
			nodesList = gcnew array<int>(n);	
			std::cout << "starting from the entry node," << std::endl;
			for (int i = 0; i<n;i++) 
			{
				cout<<"enter node id "<<i+1<< " on the path. "<<endl;
				cin>>node;
				nodesList[i] = node;
			}
			proxy->SetServerPathData(nodesList);
			proxy->SetClientState(AddPathSubmitted);
			break;	
		//cout<<"=\t enter <404> to change timestep interval;"<<endl;	
		case 404: 
			cout<<"enter timestep interval "<<endl;
			cin>>interval; 
			proxy->SetAPITimestepInterval(interval);	
			break;
		//cout<<"=\t enter <405> to change FTC signal data;"<<endl;	
		//case 405:
		//	// update signal			
		//	updateFTCSignals(proxy);				
		//	break;
		//cout<<"*** Coordination Get/Set Commands ***" <<endl;
		//cout<<"=\t enter <501> to get coordination data;"<<endl;
		case 501:
			

			cout<<"enter the node ID to get coordination data"<<endl;
			cin>>node_id;
			wcf_coordination_data[0].node = node_id;
			proxy->SetServerCoordinationData(wcf_coordination_data);
			proxy->SetClientState(GetCoordinationData);
#if _USE_TCA
			proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
			while (proxy->GetClientState() != GotCoordinationData)
			{
			}
			wcf_coordination_data = proxy->GetServerCoordinationData();
			cout<<"local_cycle_timer = " << wcf_coordination_data[0].local_cycle_timer <<endl;
			cout<<"cycle_length = " << wcf_coordination_data[0].cycle_length <<endl;
			cout<<"offset = " << wcf_coordination_data[0].offset <<endl;
			cout<<"max_add = " << wcf_coordination_data[0].max_add <<endl;
			cout<<"max_subt = " << wcf_coordination_data[0].max_subt <<endl;
			cout<<"iact = " << wcf_coordination_data[0].iact <<endl;
			cout<<"method = " << wcf_coordination_data[0].method <<endl;
			for (int i = 0; i < wcf_coordination_data[0].splits->Length; ++i)
			{
				if (i == 4) {cout<<endl;}
				cout<<"splits[" << i << "] = " << wcf_coordination_data[0].splits[i] <<" ";

			}
			cout<<endl;
			for (int i = 0; i < wcf_coordination_data[0].min_splits->Length; ++i)
			{
				if (i == 4) {cout<<endl;}
				cout<<"min_splits[" << i << "] = " << wcf_coordination_data[0].min_splits[i] <<" ";
			}
			cout<<endl;
			break;
		//cout<<"=\t enter <502> to set coordination data;"<<endl;
		case 502:
			//int NodeID;
			//float cycle_length, offset, max_add, max_subt;
			//int iact, method;
			//float splits[8];

			cout<<"enter node id"<<endl;
			cin>>node_id;
			cout<<"enter new cycle_length"<<endl;
			cin>>cycle_length;
			cout<<"enter new offset"<<endl;
			cin>>offset;
			cout<<"enter new max_add"<<endl;
			cin>>max_add;
			cout<<"enter new max_subt"<<endl;
			cin>>max_subt;
			cout<<"enter new method"<<endl;
			cin>>method;
			cout<<"enter new splits[8]"<<endl;
			for (int i = 0; i < 8; ++i)
			{
				cin>>splits[i];
			}

			wcf_coordination_data[0].node = node_id;
			wcf_coordination_data[0].cycle_length = cycle_length;
			wcf_coordination_data[0].offset = offset;
			wcf_coordination_data[0].max_add = max_add;
			wcf_coordination_data[0].max_subt = max_subt;
			wcf_coordination_data[0].method = method;
			for (int i = 0; i < 8; ++i)
			{
				wcf_coordination_data[0].splits[i] = splits[i];
			}
			//wcf_coordination_data[0].coord_set = true;
			proxy->SetServerCoordinationData(wcf_coordination_data);
			proxy->SetClientState(SetCoordinationData);
#if _USE_TCA
			proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
			while (proxy->GetClientState() != SetCoordinationDataDone)
			{
			}
			break;

		//cout<<"=\t enter <503> to set new_cycle_length;"<<endl;
		case 503:
			cout<<"enter node id"<<endl;
			cin>>node_id;
			cout<<"enter new_cycle_length"<<endl;
			cin>>new_cycle_length;
			//proxy->SetServerNodeID(NodeID);
			proxy->SetServerNewCycleLength(node_id, new_cycle_length);
			proxy->SetClientState(SetNewCycleLength);
#if _USE_TCA
			proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
			while (proxy->GetClientState() != SetCoordinationDataDone)
			{
			}
			break;
		//cout<<"=\t enter <504> to get new_cycle_length;"<<endl;
		case 504:
			cout<<"enter node id"<<endl;
			cin>>node_id;
			proxy->SetServerNodeID(node_id);
			proxy->SetClientState(GetNewCycleLength);
#if _USE_TCA
			proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
			while (proxy->GetClientState() != GotCoordinationData)
			{
			}
			new_cycle_length = proxy->GetServerNewCycleLength();
			cout<<"new_cycle_length = " << new_cycle_length <<endl;
			break;
		//cout<<"=\t enter <505> to get cycle_length;"<<endl;
		case 505:
			cout<<"enter node id"<<endl;
			cin>>node_id;
			proxy->SetServerNodeID(node_id);
			proxy->SetClientState(GetCycleLength);
#if _USE_TCA
			proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
			while (proxy->GetClientState() != GotCoordinationData)
			{
			}
			cycle_length = proxy->GetServerCycleLength();
			cout<<"cycle_length = " << cycle_length <<endl;
			break;

		//cout<<"=\t enter <506> to set new_offset;"<<endl;
		case 506:
			cout<<"enter node id"<<endl;
			cin>>node_id;
			cout<<"enter new_offset"<<endl;
			cin>>new_offset;
			proxy->SetServerNewOffset(node_id, new_offset);
			proxy->SetClientState(SetNewOffset);
#if _USE_TCA
			proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
			while (proxy->GetClientState() != SetCoordinationDataDone)
			{
			}
			break;
		//cout<<"=\t enter <507> to get new_offset;"<<endl;
		case 507:
			cout<<"enter node id"<<endl;
			cin>>node_id;
			proxy->SetServerNodeID(node_id);
			proxy->SetClientState(GetNewOffset);
#if _USE_TCA
			proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
			while (proxy->GetClientState() != GotCoordinationData)
			{
			}
			new_offset = proxy->GetServerNewOffset();
			cout<<"new_offset = " << new_offset <<endl;
			break;
		//cout<<"=\t enter <508> to get offset;"<<endl;
		case 508:
			cout<<"enter node id"<<endl;
			cin>>node_id;
			proxy->SetServerNodeID(node_id);
			proxy->SetClientState(GetOffset);
#if _USE_TCA
			proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
			while (proxy->GetClientState() != GotCoordinationData)
			{
			}
			offset = proxy->GetServerOffset();
			cout<<"offset = " << offset <<endl;
			break;

		//cout<<"=\t enter <509> to get local_cycle_timer;"<<endl;
		case 509:
			cout<<"enter node id"<<endl;
			cin>>node_id;
			proxy->SetServerNodeID(node_id);
			proxy->SetClientState(GetLocalCycleTimer);
#if _USE_TCA
			proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
			while (proxy->GetClientState() != GotCoordinationData)
			{
			}
			local_cycle_timer = proxy->GetServerLocalCycleTimer();
			cout<<"local_cycle_timer = " << local_cycle_timer <<endl;
			break;

		//cout<<"=\t enter <510> to set new_splits;"<<endl;
		case 510:
			cout<<"enter node id"<<endl;
			cin>>node_id;
			cout<<"enter new_splits"<<endl;
			for (int i = 0; i < 8; ++i)
			{
				cin>>new_splits[i];
				wcf_new_splits[i] = new_splits[i];
			}
			proxy->SetServerNewSplits(node_id, wcf_new_splits);
			proxy->SetClientState(SetNewSplits);
#if _USE_TCA
			proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
			while (proxy->GetClientState() != SetCoordinationDataDone)
			{
			}
			break;
		//cout<<"=\t enter <511> to get new_splits;"<<endl;
		case 511:
			cout<<"enter node id"<<endl;
			cin>>node_id;
			proxy->SetServerNodeID(node_id);
			proxy->SetClientState(GetNewSplits);
#if _USE_TCA
			proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
			while (proxy->GetClientState() != GotCoordinationData)
			{
			}
			wcf_new_splits = proxy->GetServerNewSplits();
			std::cout<<"new_splits: " << std::endl;
			for (int i = 0; i < 8; i++)
			{
				new_splits[i] = wcf_new_splits[i];
				std::cout << new_splits[i] << " ";
			}
			std::cout<<std::endl;
			break;
		//cout<<"=\t enter <512> to get splits;"<<endl;
		case 512:
			cout<<"enter node id"<<endl;
			cin>>node_id;
			proxy->SetServerNodeID(node_id);
			proxy->SetClientState(GetSplits);
#if _USE_TCA
			proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
			while (proxy->GetClientState() != GotCoordinationData)
			{
			}
			wcf_splits = proxy->GetServerSplits();
			std::cout<<"splits: " << std::endl;
			for (int i = 0; i < 8; i++)
			{
				splits[i] = wcf_splits[i];
				std::cout << splits[i] << " ";
			}
			std::cout<<std::endl;
			break;

		//cout<<"=\t enter <513> to get min_splits;"<<endl;
		case 513:
			cout<<"enter node id"<<endl;
			cin>>node_id;
			proxy->SetServerNodeID(node_id);
			proxy->SetClientState(GetMinSplits);
#if _USE_TCA
			proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
			while (proxy->GetClientState() != GotCoordinationData)
			{
			}
			wcf_min_splits = proxy->GetServerMinSplits();
			std::cout<<"min_splits: " << std::endl;
			for (int i = 0; i < 8; i++)
			{
				min_splits[i] = wcf_min_splits[i];
				std::cout << min_splits[i] << " ";
			}
			std::cout<<std::endl;
			break;
		//cout<<"=\t enter <514> to get node GYR;"<<endl;
		case 514:
			cout<<"enter node id"<<endl;
			cin>>node_id;
			proxy->SetServerNodeID(node_id);
			proxy->SetClientState(GetNodeGYR);
#if _USE_TCA
			proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
			while (proxy->GetClientState() != GotCoordinationData)
			{
			}
			wcf_greens_yellows = proxy->GetServerNodeGYR();
			std::cout << "greens = " << wcf_greens_yellows[0] << std::endl;
			std::cout << "yellows = " << wcf_greens_yellows[1] << std::endl;
			std::cout << "a phase is neither green nor yellow is default as red.." << std::endl;
			std::cout<<std::endl;
			break;
		//cout<<"=\t enter <515> to set node GYR;"<<endl;
		case 515:
			cout<<"enter node id"<<endl;
			cin>>node_id;
			int greens, yellows;
			std::cout << "enter greens" << std::endl;
			cin>>greens;
			if (greens < 0 || greens > 255) {
				std::cout << "invalid green value.. setting failed.." << std::endl;
			} else {
				std::cout << "enter yellows" << std::endl;
				cin>>yellows;
				if (yellows < 0 || yellows > 255) {
					std::cout << "invalid green value.. setting failed.." << std::endl;
				} else {
					wcf_greens_yellows[0] = greens;
					wcf_greens_yellows[1] = yellows;
					proxy->SetServerNodeGYR(node_id, wcf_greens_yellows);
					proxy->SetClientState(SetNodeGYR);
#if _USE_TCA
					proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
					while (proxy->GetClientState() != SetCoordinationDataDone)
					{
					}
				}
			}
			break;
		//cout<<"=\t enter <901> to get AverageTravelTime;"<<endl;
		case 901:
			cout << "enter usn id" << endl;
			cin >> usn_id;
			cout << "enter dsn id" << endl;
			cin >> dsn_id;
			proxy->SetServerUSNID(usn_id);
			proxy->SetServerDSNID(dsn_id);
			proxy->SetClientState(GetAverageTravelTime);
#if _USE_TCA
			proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
			while (proxy->GetClientState() != GotAverageTravelTime)
			{
			}
			AverageTravelTime = proxy->GetServerAverageTravelTime();
			std::cout << "AverageTravelTime = " << AverageTravelTime << std::endl;
			break;
		//cout<<"=\t enter <902> to get GetStreetLaneMOEData;"<<endl;
		case 902:
			{
				int lane_id, flag;
				std::string str_MOEString;
				System::String^ sysstr_MOEString;
				cout<<"enter usn node id"<<endl; //8
				cin>>usn_id;
				cout<<"enter dsn node id"<<endl; //10
				cin>>dsn_id;
				cout<<"enter lane id"<<endl; //1
				cin>>lane_id;
				cout<<"enter flag (0 = cumulative, 1 = time interval, 2 = time period)"<<endl; //0
				cin>>flag;
				cout<<"enter MOE string (TravelTimePerVehicle)"<<endl; //"TravelTimePerVehicle";
				cin>>str_MOEString;
				wcf_street_moe_data[0].usn_id = usn_id;
				wcf_street_moe_data[0].dsn_id = dsn_id;
				wcf_street_moe_data[0].lane_id = lane_id;
				wcf_street_moe_data[0].flag = flag;
				wcf_street_moe_data[0].MOEString = gcnew String(str_MOEString.c_str());
				proxy->SetServerStreetMOEData(wcf_street_moe_data);
				proxy->SetClientState(GetStreetLaneMOEData);
#if _USE_TCA
				proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
				while (proxy->GetClientState() != GotStreetLaneMOEData)
				{
				}
				wcf_street_moe_data = proxy->GetServerStreetMOEData();
				sysstr_MOEString = wcf_street_moe_data[0].MOEString;
				str_MOEString = marshal_as<std::string>(sysstr_MOEString);
				std::cout << str_MOEString << " = " << wcf_street_moe_data[0].MOEValue << std::endl;
				break;
			}
		//cout<<"=\t enter <903> to get GetStreetLinkMOEData;"<<endl;
		case 903:
			{
				int flag;
				std::string str_MOEString;
				System::String^ sysstr_MOEString;
				cout<<"enter usn node id"<<endl; //8
				cin>>usn_id;
				cout<<"enter dsn node id"<<endl; //10
				cin>>dsn_id;
				cout<<"enter flag"<<endl; //0
				cin>>flag;
				cout<<"enter MOE string (DelayControlTotal)"<<endl; //"DelayControlTotal";
				cin>>str_MOEString;
				wcf_street_moe_data[0].usn_id = usn_id;
				wcf_street_moe_data[0].dsn_id = dsn_id;
				wcf_street_moe_data[0].flag = flag;
				wcf_street_moe_data[0].MOEString = gcnew String(str_MOEString.c_str());
				proxy->SetServerStreetMOEData(wcf_street_moe_data);
				proxy->SetClientState(GetStreetLinkMOEData);
#if _USE_TCA
				proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
				while (proxy->GetClientState() != GotStreetLinkMOEData)
				{
				}
				wcf_street_moe_data = proxy->GetServerStreetMOEData();
				sysstr_MOEString = wcf_street_moe_data[0].MOEString;
				str_MOEString = marshal_as<std::string>(sysstr_MOEString);
				std::cout << str_MOEString << " = " << wcf_street_moe_data[0].MOEValue << std::endl;
				break;
			}

		default:
			cout << "Invalid command." << endl;
		}

#if !_USE_TCA
		if (exit) break;
#endif

		cout<<"=================================================="<<endl;
		cout<<"*** Basic Commands ***" <<endl;
		cout<<"=\t enter <1> to proceed " << interval << " time steps."<<endl;
		cout<<"=\t enter <2> to finish current run w/o pause;"<<endl;
		cout<<"=\t enter <3> to shut down;"<<endl;
		cout<<"*** Program-modify Commands ***" <<endl;
		cout<<"=\t enter <100> to test the change of all data;" <<endl;
		#if PRINTOUT
		cout<<"*** Print Commands ***" <<endl;
		cout<<"=\t enter <200> to print all data to file;"<<endl;
		cout<<"=\t enter <201> to print freeway vehicle data;"<<endl;
		cout<<"=\t enter <202> to print street vehicle data;"<<endl;
		cout<<"=\t enter <203> to print freeway link data;"<<endl;
		cout<<"=\t enter <204> to print Street link data and AC signal data;"<<endl;
		cout<<"=\t enter <205> to print FTC signal data;"<<endl;
		cout<<"=\t enter <206> to print Entry Node data;"<<endl;
		cout<<"=\t enter <207> to print Ramp Meter data;"<<endl;
		#endif
		cout<<"*** Display Commands ***" <<endl;
		cout<<"=\t enter <301> to display freeway vehicle data;"<<endl;
		cout<<"=\t enter <302> to display street vehicle data;"<<endl;
		cout<<"=\t enter <303> to display freeway links data;"<<endl;
		cout<<"=\t enter <304> to display street links data and AC signals data;"<<endl; 
		cout<<"=\t enter <305> to display FTC signals data;"<<endl; 
		cout<<"=\t enter <308> to display Node XY Coordinate;"<<endl;
		cout<<"*** Manual-modify Commands ***" <<endl;
		cout<<"=\t enter <401> to change freeway vehicle information;"<<endl;
		cout<<"=\t enter <402> to add a new vehicle;"<<endl;
		cout<<"=\t enter <403> to add path;"<<endl;		
		cout<<"=\t enter <404> to change timestep interval;"<<endl;	
		//cout<<"=\t enter <405> to change FTC signal data;"<<endl;	
		cout<<"*** Coordination Get/Set Commands ***" <<endl;
		cout<<"=\t enter <501> to get coordination data;"<<endl;
		cout<<"=\t enter <502> to set coordination data;"<<endl;
		
		cout<<"=\t enter <503> to set new_cycle_length;"<<endl;
		cout<<"=\t enter <504> to get new_cycle_length;"<<endl;
		cout<<"=\t enter <505> to get cycle_length;"<<endl;
		
		cout<<"=\t enter <506> to set new_offset;"<<endl;
		cout<<"=\t enter <507> to get new_offset;"<<endl;
		cout<<"=\t enter <508> to get offset;"<<endl;
		
		cout<<"=\t enter <509> to get local_cycle_timer;"<<endl;

		cout<<"=\t enter <510> to set new_splits;"<<endl;
		cout<<"=\t enter <511> to get new_splits;"<<endl;
		cout<<"=\t enter <512> to get splits;"<<endl;
		
		cout<<"=\t enter <513> to get min_splits;"<<endl;

		cout<<"=\t enter <514> to get node GYR;"<<endl;
		cout<<"=\t enter <515> to set node GYR;"<<endl;
		cout<<"*** Other Commands ***" <<endl;
		cout<<"=\t enter <901> to get AverageTravelTime;"<<endl;
		cout<<"=\t enter <902> to get GetStreetLaneMOEData;"<<endl;
		cout<<"=\t enter <903> to get GetStreetLinkMOEData;"<<endl;
		cout<<"=================================================="<<endl;
#if _USE_TCA
		cout<<"=\t waiting TCA to update vehicle info..."<<endl;
		while ( (proxy->GetTCAState() != TCAProcessingDone) && (proxy->GetTCAState() != SimulationFinished) && (proxy->GetClientState() != GotCoordinationData) && (proxy->GetClientState() != GotAverageTravelTime) && (proxy->GetClientState() != GotStreetLaneMOEData) && (proxy->GetClientState() != GotStreetLinkMOEData) )
		{
			//cout<<proxy->GetTCAState()<<endl;
		}
		
		//Get new vehicle information including the TCA part from WCF server
		float BSMdataFromSimTime = proxy->GetServerTimestep();
		cout << "Getting BSMdata From SimTime: " << BSMdataFromSimTime << endl;
		svehicle_BSMdata = proxy->GetServerSVehicleData();
		cout << "svehicle_BSMdata[0].BSM_tmp_ID: " << svehicle_BSMdata[0].BSM_tmp_ID << endl;
		TCA_TEST_OUTPUT_FILE << "TimeStep = " << BSMdataFromSimTime << std::endl;
		TCA_TEST_OUTPUT_FILE << "svehicle_BSMdata[0].BSM_tmp_ID: " << svehicle_BSMdata[0].BSM_tmp_ID << std::endl;

		svehicle_data = proxy->GetServerSVehicleData();
		std::cout << "svehicle_data[0].BSM_tmp_ID = " << svehicle_data[0].BSM_tmp_ID <<std::endl;

		if (proxy->GetTCAState() != SimulationFinished)
		{
			proxy->SetTCAState(GoodForNextSimulationStep);
		}
#endif

		if (proxy->GetClientState() == Finished)
		{
			std::cout << "\n\n############################" << std::endl;
			std::cout << "Simulation runs are finished, please choose command 3 to shut down API Client..." << std::endl;
			std::cout << "############################" << std::endl << std::endl;
#if _USE_TCA
			command = 3;
#endif
		}
		cout<<"Please enter a command: "<<endl;
		cin >> command;
	}


	char terminate;
	std::cout << "WCF service is stopped." << std::endl;
	std::cout << "############################" << std::endl << std::endl;
	std::cout << "hit any key to terminate" << std::endl;
	std::cin >> terminate;
	return 0;

}



void displayFreeways(array<Wcf_freeway_link> ^ wcf_fwl)
{
	std::cout<<std::endl;
	std::cout << "#freeway links = " << wcf_fwl->Length << std::endl;

	for (int i = 0; i < wcf_fwl->Length; i++)
	{
		std::cout << "  link # " << i+1;
		std::cout << "  upstream node = " << wcf_fwl[i].usn;
		std::cout << "  downstream node = " << wcf_fwl[i].dsn << std::endl;
		std::cout << "  thru downstream node = " << wcf_fwl[i].thrunode;
		std::cout << "  offramp downstream node = " << wcf_fwl[i].exitnode;
		std::cout << "  length = " << wcf_fwl[i].length;
		std::cout << "  full lanes = " << wcf_fwl[i].fulllanes << std::endl;

		if (wcf_fwl[i].usn != 0 && wcf_fwl[i].dsn != 0)
		{
			for(int iaux = 0; iaux < 10; iaux++)
			{
				if( wcf_fwl[i].auxlaneid[iaux] != 0 )
				{
					std::cout << "  aux lane " << iaux+1;
					std::cout << ": ID = " << wcf_fwl[i].auxlaneid[iaux];
					std::cout << ", code = " << wcf_fwl[i].auxlanecode[iaux];
					std::cout << ", length = " << wcf_fwl[i].auxlanelength[iaux] << std::endl;
				}
			}
		}
		std::cout << "  freeflow speed = " << wcf_fwl[i].freeflowspeed << std::endl <<std::endl;
	}
}


void displayStreets(array<Wcf_street_link> ^ wcf_sl)
{
	std::cout<<std::endl;
	std::cout << "#stree links = " << wcf_sl->Length << std::endl;
	for(int i = 0; i < wcf_sl->Length; i++)
	{
		std::cout << "  link # " << i+1 << std::endl;
		std::cout << "  upstream node = " << wcf_sl[i].usn << std::endl;
		std::cout << "  downstream node = " << wcf_sl[i].dsn << std::endl;
		std::cout << "  left downstream node = " << wcf_sl[i].leftnode << std::endl;
		std::cout << "  thru downstream node = " << wcf_sl[i].thrunode << std::endl;
		std::cout << "  right downstream node = " << wcf_sl[i].rightnode << std::endl;
		std::cout << "  diagonal downstream node = " << wcf_sl[i].diagnode << std::endl;
		std::cout << "  length = " << wcf_sl[i].length << std::endl;
		std::cout << "  full lanes = " << wcf_sl[i].fulllanes << std::endl;
		std::cout << "  leftturnbays = " << wcf_sl[i].leftturnbays << std::endl;
		std::cout << "  rightturnbays = " << wcf_sl[i].rightturnbays << std::endl;
		//std::cout << "  lengthofleftbay = " << wcf_sl[i].lengthofleftbay << std::endl;
		//std::cout << "  lengthofrightbay = " << wcf_sl[i].lengthofrightbay << std::endl;
		std::cout << "  freeflow speed = " << wcf_sl[i].freeflowspeed << std::endl << std::endl;
	}
}


void displayACSignals(array<Wcf_street_link> ^ wcf_sl, array <WCF_AC> ^ wcf_acl)
{
	std::cout<<std::endl;
	if (wcf_acl->Length != 0)
	{
		for(int isig = 0; isig < wcf_acl->Length; isig++)
		{
			//print out approach info
			std::cout << "Actuated Control Signal at node " << wcf_acl[isig].node << std::endl;
			std::cout << "  Consecutive Fails = " << wcf_acl[isig].cfails << std::endl;
			std::cout << "  Adjustments = " << wcf_acl[isig].adj << std::endl;
			std::cout << "  cycle length = " << wcf_acl[isig].cycle_length << std::endl;
			std::cout << "  offset = " << wcf_acl[isig].offset << std::endl;
			std::cout << "  #Approachs = " << wcf_acl[isig].n_direct_approaches << std::endl;

			for(int iap = 0; iap < wcf_acl[isig].n_direct_approaches; iap++)
			{
				//std::cout << "   approach # " << iap+1 << " is link " << wcf_acl[isig].direct_approach_USN[iap];
				//int lindex = wcf_acl[isig].direct_approach_USN[iap] - 1;
				//std::cout << ", usn = " << wcf_sl[lindex].usn << std::endl;
				std::cout << "   approach # " << iap+1 << " is usn " << wcf_acl[isig].direct_approach_USN[iap];
				int lindex = wcf_acl[isig].direct_approach_USN[iap] - 1;
				//std::cout << ", usn = " << wcf_sl[lindex].usn << std::endl;
			}

			if (wcf_acl[isig].node != 0 && wcf_acl[isig].n_direct_approaches != 0)
			{
				for (int iphase = 0; iphase < 8; ++iphase)
				{
					std::cout << "Phase " << iphase + 1 << std::endl;
					std::cout << "  	actuated_mode	 =  " << wcf_acl[isig].	actuated_mode	[iphase] << std::endl;
					std::cout << "  	min_green_time	 =  " << wcf_acl[isig].	min_green_time	[iphase] << std::endl;
					std::cout << "  	max_green_time	 =  " << wcf_acl[isig].	max_green_time	[iphase] << std::endl;
					std::cout << "  	default_extension_time	 =  " << wcf_acl[isig].	default_extension_time	[iphase] << std::endl;
					std::cout << "  	gap_time	 =  " << wcf_acl[isig].	gap_time	[iphase] << std::endl;
					std::cout << "  	times_before_reduction	 =  " << wcf_acl[isig].	times_before_reduction	[iphase] << std::endl;
					std::cout << "  	time_to_reduce	 =  " << wcf_acl[isig].	time_to_reduce	[iphase] << std::endl;
					std::cout << "  	min_gap_time	 =  " << wcf_acl[isig].	min_gap_time	[iphase] << std::endl;
					std::cout << "  	yellow_change_int	 =  " << wcf_acl[isig].	yellow_change_int	[iphase] << std::endl;
					std::cout << "  	red_clear_int	 =  " << wcf_acl[isig].	red_clear_int	[iphase] << std::endl;
					std::cout << "  	ped_allowed	 =  " << wcf_acl[isig].	ped_allowed	[iphase] << std::endl;
					std::cout << "  	walk_time	 =  " << wcf_acl[isig].	walk_time	[iphase] << std::endl;
					std::cout << "  	walk_clearance_time	 =  " << wcf_acl[isig].	walk_clearance_time	[iphase] << std::endl;
					std::cout << "  	left arrows: ";
					for(int iap = 0; iap < wcf_acl[isig].n_direct_approaches; iap++)
					{
						std::cout << "\t" << wcf_acl[isig].leftarrow[iap][iphase];
					}
					std::cout << std::endl;

					std::cout << "  	thru arrows: ";
					for(int iap = 0; iap < wcf_acl[isig].n_direct_approaches; iap++)
					{
						std::cout << "\t" << wcf_acl[isig].thruarrow[iap][iphase];
					}
					std::cout << std::endl;

					std::cout << "  	right arrows: ";
					for(int iap = 0; iap < wcf_acl[isig].n_direct_approaches; iap++)
					{
						std::cout << "\t" << wcf_acl[isig].rightarrow[iap][iphase];
					}
					std::cout << std::endl;

					std::cout << "  	diag arrows: ";
					for(int iap = 0; iap < wcf_acl[isig].n_direct_approaches; iap++)
					{
						std::cout << "\t" << wcf_acl[isig].diagarrow[iap][iphase];
					}
					std::cout << std::endl;
			
				}
			}

			std::cout << "  Ring Phase: " << std::endl;
			for (int iRing = 0; iRing <2; ++iRing)
			{
				std::cout << "  	Ring " << iRing+1 << ":";
				for (int j = 0; j < 4; ++j)
				{
					//std::cout << wcf_acl[isig].ring_phase[j][iRing];
					std::cout << wcf_acl[isig].ring_phase[j + iRing * 4];
				}
				std::cout << endl;
			}

			std::cout << "  detector_count = " << wcf_acl[isig].detector_count << std::endl;
			std::cout << "  detector_list: ";
			for (int j = 0; j < wcf_acl[isig].detector_list->Length; ++j)
			{
				std::cout << wcf_acl[isig].detector_list[j] << "	";
			}
			std::cout << std::endl;
			std::cout << "  transition_method = " << wcf_acl[isig].transition_method << std::endl;
			std::cout << "  max_add = " << wcf_acl[isig].max_add << std::endl;
			std::cout << "  max_subtract = " << wcf_acl[isig].max_subtract << std::endl;
			std::cout << "  force_off_times: " << std::endl;
			for (int j = 0; j < wcf_acl[isig].force_off_times->Length; ++j)
			{
				std::cout << wcf_acl[isig].force_off_times[j] << "	";
			}
			std::cout << std::endl;
			///
			std::cout << " local_cycle_timer = " << wcf_acl[isig].local_cycle_timer << std::endl;
			std::cout << "  splits: " << std::endl;
			for (int j = 0; j < wcf_acl[isig].splits->Length; ++j)
			{
				std::cout << wcf_acl[isig].splits[j] << "	";
			}
			std::cout << std::endl;
			std::cout << "  min_splits: " << std::endl;
			for (int j = 0; j < wcf_acl[isig].min_splits->Length; ++j)
			{
				std::cout << wcf_acl[isig].min_splits[j] << "	";
			}
			std::cout << std::endl;
			std::cout<< " new_cycle_length = " << wcf_acl[isig].new_cycle_length << std::endl;
			std::cout<< " new_offset = " << wcf_acl[isig].new_offset << std::endl;
			std::cout << "  new_splits: " << std::endl;
			for (int j = 0; j < wcf_acl[isig].new_splits->Length; ++j)
			{
				std::cout << wcf_acl[isig].new_splits[j] << "	";
			}
			std::cout << std:: endl;
			std::cout << "green_phases = " << wcf_acl[isig].green_phases << std::endl;
			std::cout << "yellow_phases = " << wcf_acl[isig].yellow_phases << std::endl;
			std::cout << std::endl;
		}
	}
	else
	{
		std::cout<<"no AC signal found"<<std::endl;
	}

}


void displayNode(array<WCF_NODE_LOCATION_DATA>^ wcf_xy_coord_inputs, int NodeID)
{
	std::cout<<std::endl;
	if (NodeID > wcf_xy_coord_inputs->Length || NodeID < 1)
	{
		std::cout<< "Node "<< NodeID << " not found" << std::endl;
	}
	else
	{
		std::cout<<"Node " << NodeID << " [x,y] = ["<<wcf_xy_coord_inputs[NodeID-1].x<<','<<wcf_xy_coord_inputs[NodeID-1].y<<']'<<std::endl;
	}
}

void displayFVehicles(array<WCF_VFData> ^fvehicle_data)
{
	std::cout<<std::endl;
	std::cout<< fvehicle_data->Length << " freeway vehicles found in WCF server. "<<std::endl;
	for (int i = 0; i< fvehicle_data->Length; i++)
	{
		cout<<"  in API client Vehicle ID = " << fvehicle_data[i].id <<", driver type = " << fvehicle_data[i].drivertype;
		cout << ", speed = " << fvehicle_data[i].speed << ", location = " << fvehicle_data[i].location;
		cout << ", link = " << fvehicle_data[i].link << ", lane = " << fvehicle_data[i].lane << endl;
	} 
}

void displaySVehicles(array<WCF_VSData> ^svehicle_data)
{
	std::cout<<std::endl;
	std::cout<< svehicle_data->Length << " street vehicles found in WCF server. "<<std::endl;
	for (int i = 0; i< svehicle_data->Length; i++)
	{
		cout<<"  in API client Vehicle ID = " << svehicle_data[i].id <<", driver type = " << svehicle_data[i].drivertype;
		cout << ", speed = " << svehicle_data[i].speed << ", location = " << svehicle_data[i].location;
		cout << ", link = " << svehicle_data[i].link << ", lane = " << svehicle_data[i].lane << endl;
	}
}

void enterSingalsData(int *d, int node, int intv)
{
	int n;
	cout<<"enter duration for interval "<< intv <<endl;
	cin>>n;
	*d = n;
}


void updateFTCSignals(IService1^ proxy)
{
	//get available network signals from WCF server
	int di,node,duration;
	array<WCF_FTC_DATA> ^ wcf_ftc = proxy->GetServerFTCSignalData();

	displayFTCSignals(wcf_ftc);
	cout<<"enter node number for signal to change: "<<endl;
	cin>>node;
	cout<<" please enter which duration(i) you want to change i = "<<endl;
	cin>>di;
	cout<<" enter duration value = "<<endl;
	cin>>duration;

	int ftc_id = 0;
	bool found = false;
	for (int i = 0; i<wcf_ftc->Length; i++) 
	{
		if (node == wcf_ftc[i].node)
		{
			ftc_id = i;
			for (int j = 0; j<wcf_ftc[i].duration->Length;j++)
			{
				if ( j== di)
				{
					wcf_ftc[i].duration[j] = duration;
					found = true;
				}
			}	
		}
	}
	if (!found)
	{
		cout<< "The node "<< node<<" and duration "<< di << " are not found, please enter again."<<endl;
	}
	else
	{
		wcf_ftc[ftc_id].cycle_length = 0;
		for (int idua = 0; idua < wcf_ftc[ftc_id].active_intervals; ++idua)
		{
			wcf_ftc[ftc_id].cycle_length += wcf_ftc[ftc_id].duration[idua];
		};
		cout<< "Update etFomm signal network. "<< endl;
	}

	proxy->SetServerFTCSignalData(wcf_ftc); 
	proxy->SetClientState(FixTimeControlSignalsDataSubmitted);
}


void displayFTCSignals(array<WCF_FTC_DATA> ^ ftc_etfomm) 
{ 
	cout<<" Available network signals:"<<endl;
	for (int i = 0; i< ftc_etfomm->Length; i++)
	{
		if (ftc_etfomm[i].node != 0) {
			cout<<" @node ="<<ftc_etfomm[i].node <<endl;
			cout<<"  active_intervals = "<< ftc_etfomm[i].active_intervals << " approaches = " <<ftc_etfomm[i].approaches << endl;
			cout<<"  duration                            signal code                                         "<<endl;
			cout<<"             upnode("<< ftc_etfomm[i].approach[0]<<")  upnode("<< ftc_etfomm[i].approach[1]<<")   upnode("<< ftc_etfomm[i].approach[2]
			<<")  upnode("<< ftc_etfomm[i].approach[3]<<")   upnode("<<ftc_etfomm[i].approach[4]<<")"<<endl;
			for (int j = 0; j<ftc_etfomm[i].duration->Length; j++) 
			{
				cout<<" " <<ftc_etfomm[i].duration[j] << "    " <<ftc_etfomm[i].signal_code[j][0] << "    " <<ftc_etfomm[i].signal_code[j][1]
				<< "    " <<ftc_etfomm[i].signal_code[j][2]<< "    " <<ftc_etfomm[i].signal_code[j][3]<< "    " <<ftc_etfomm[i].signal_code[j][4]<<endl;			
			}
		}
	}
}

void printFVehicleData(StreamWriter^ outFile, array<WCF_VFData> ^fvehicle_data)
{
	outFile->WriteLine("FREEWAY_VEHICLE: {0}", fvehicle_data->Length);

	for(int i = 0; i < fvehicle_data->Length; i++)
	{
		outFile->WriteLine("#{0}", i+1);
		outFile->WriteLine("acceleration = {0:F6}", fvehicle_data[i].acceleration);
		outFile->WriteLine("decel = {0}", fvehicle_data[i].decel);
		outFile->WriteLine("desiredspeed = {0:F3}", fvehicle_data[i].desiredspeed);
		outFile->WriteLine("disch_timer = {0}", fvehicle_data[i].disch_timer);
		outFile->WriteLine("drivertype = {0}", fvehicle_data[i].drivertype);
		outFile->WriteLine("entry_link = {0}", fvehicle_data[i].entry_link);
		outFile->WriteLine("entrytime = {0}", fvehicle_data[i].entrytime);
		outFile->WriteLine("ev_dist = {0}", fvehicle_data[i].ev_dist);
		outFile->WriteLine("ev_ovrspd = {0}", fvehicle_data[i].ev_ovrspd);
		outFile->WriteLine("ev_range = {0}", fvehicle_data[i].ev_range);
		outFile->WriteLine("ev_rand = {0}", fvehicle_data[i].ev_rand);
		outFile->WriteLine("ev_wait_timer = {0}", fvehicle_data[i].ev_wait_timer);
		outFile->WriteLine("ev_watch = {0}", fvehicle_data[i].ev_watch);
		outFile->WriteLine("fleet = {0}", fvehicle_data[i].fleet);
		outFile->WriteLine("go_thru_signal = {0}", fvehicle_data[i].go_thru_signal);
		outFile->WriteLine("lag_timer = {0}", fvehicle_data[i].lag_timer);
		outFile->WriteLine("lane = {0}", fvehicle_data[i].lane);

		outFile->Write("lanecodes:");
		for (int j = 0; j < fvehicle_data[i].lanecodes->Length; j++)
		{
			if (fvehicle_data[i].lanecodes[j] != 0)
				outFile->Write("\t{0}", fvehicle_data[i].lanecodes[j]);
		}
		outFile->Write("\n");

		outFile->WriteLine("link = {0}", fvehicle_data[i].link);
		outFile->WriteLine("follower = {0}", fvehicle_data[i].follower);
		outFile->WriteLine("id = {0}", fvehicle_data[i].id);
		outFile->WriteLine("last_detid = {0}", fvehicle_data[i].last_detid);
		outFile->WriteLine("lc_timer = {0}", fvehicle_data[i].lc_timer);
		outFile->WriteLine("leader = {0}", fvehicle_data[i].leader);
		outFile->WriteLine("location = {0:F5}", fvehicle_data[i].location);
		outFile->WriteLine("pathid = {0}", fvehicle_data[i].pathid);
		outFile->WriteLine("pathpoint = {0}", fvehicle_data[i].pathpoint);
		outFile->WriteLine("saved_path = {0}", fvehicle_data[i].saved_path);
		outFile->WriteLine("pseudo_leader = {0}", fvehicle_data[i].pseudo_leader);
		outFile->WriteLine("prev_accel = {0:F6}", fvehicle_data[i].prev_accel);
		outFile->WriteLine("prevlink = {0}", fvehicle_data[i].prevlink);
		outFile->WriteLine("prevlane = {0}", fvehicle_data[i].prevlane);
		outFile->WriteLine("routeid = {0}", fvehicle_data[i].routeid);
		outFile->WriteLine("speed = {0:F3}", fvehicle_data[i].speed);
		outFile->WriteLine("speed_adj = {0}", fvehicle_data[i].speed_adj);
		outFile->WriteLine("turncode = {0}", fvehicle_data[i].turncode);
		outFile->WriteLine("vlength = {0}", fvehicle_data[i].vlength);
		outFile->WriteLine("vtype = {0}", fvehicle_data[i].vtype);
		outFile->WriteLine("xcode = {0}", fvehicle_data[i].xcode);
		outFile->WriteLine("will_coop_ev = {0}", fvehicle_data[i].will_coop_ev);
		outFile->WriteLine("will_coop_lc = {0}", fvehicle_data[i].will_coop_lc);
		outFile->WriteLine("will_move = {0}", fvehicle_data[i].will_move);
		outFile->WriteLine("destination = {0}", fvehicle_data[i].destination);
		outFile->WriteLine("distance_to_segment_end = {0:F2}", fvehicle_data[i].distance_to_segment_end);
		outFile->WriteLine("diverted = {0}", fvehicle_data[i].diverted);
		outFile->WriteLine("hov_violator = {0}", fvehicle_data[i].hov_violator);
		outFile->WriteLine("imeter = {0}", fvehicle_data[i].imeter);
		outFile->WriteLine("incident_num = {0}", fvehicle_data[i].incident_num);
		outFile->WriteLine("isegment = {0}", fvehicle_data[i].isegment);
		outFile->WriteLine("must_merge = {0}", fvehicle_data[i].must_merge);
		outFile->WriteLine("next_object = {0}", fvehicle_data[i].next_object);
		outFile->WriteLine("remaining_dist = {0:F3}", fvehicle_data[i].remaining_dist);
		outFile->WriteLine("sorted_list = {0}", fvehicle_data[i].sorted_list);
		outFile->WriteLine("sort_position = {0}", fvehicle_data[i].sort_position);
					
		outFile->Write("\n");
	}
	
	outFile->Write("\n");
}

void printSVehicleData(StreamWriter^ outFile, array<WCF_VSData> ^svehicle_data)
{
	outFile->WriteLine("STREET_VEHICLE: {0}", svehicle_data->Length);

	for(int i = 0; i < svehicle_data->Length; i++)
	{
		outFile->WriteLine("#{0}", i+1);
		outFile->WriteLine("acceleration = {0}", svehicle_data[i].acceleration);
		outFile->WriteLine("decel = {0}", svehicle_data[i].decel);
		outFile->WriteLine("desiredspeed = {0}", svehicle_data[i].desiredspeed);
		outFile->WriteLine("disch_timer = {0}", svehicle_data[i].disch_timer);
		outFile->WriteLine("drivertype = {0}", svehicle_data[i].drivertype);
		outFile->WriteLine("entry_link = {0}", svehicle_data[i].entry_link);
		outFile->WriteLine("entrytime = {0}", svehicle_data[i].entrytime);
		outFile->WriteLine("ev_dist = {0}", svehicle_data[i].ev_dist);
		outFile->WriteLine("ev_ovrspd = {0}", svehicle_data[i].ev_ovrspd);
		outFile->WriteLine("ev_range = {0}", svehicle_data[i].ev_range);
		outFile->WriteLine("ev_rand = {0}", svehicle_data[i].ev_rand);
		outFile->WriteLine("ev_wait_timer = {0}", svehicle_data[i].ev_wait_timer);
		outFile->WriteLine("ev_watch = {0}", svehicle_data[i].ev_watch);
		outFile->WriteLine("fleet = {0}", svehicle_data[i].fleet);
		outFile->WriteLine("go_thru_signal = {0}", svehicle_data[i].go_thru_signal);
		outFile->WriteLine("lag_timer = {0}", svehicle_data[i].lag_timer);
		outFile->WriteLine("lane = {0}", svehicle_data[i].lane);

		outFile->Write("lanecodes:");
		for (int j = 0; j < svehicle_data[i].lanecodes->Length; j++)
		{
			if (svehicle_data[i].lanecodes[j] != 0)
				outFile->Write("\t{0}", svehicle_data[i].lanecodes[j]);
		}
		outFile->Write("\n");

		outFile->WriteLine("link = {0}", svehicle_data[i].link);
		outFile->WriteLine("follower = {0}", svehicle_data[i].follower);
		outFile->WriteLine("id = {0}", svehicle_data[i].id);
		outFile->WriteLine("last_detid = {0}", svehicle_data[i].last_detid);
		outFile->WriteLine("lc_timer = {0}", svehicle_data[i].lc_timer);
		outFile->WriteLine("leader = {0}", svehicle_data[i].leader);
		outFile->WriteLine("location = {0}", svehicle_data[i].location);
		outFile->WriteLine("pathid = {0}", svehicle_data[i].pathid);
		outFile->WriteLine("pathpoint = {0}", svehicle_data[i].pathpoint);
		outFile->WriteLine("saved_path = {0}", svehicle_data[i].saved_path);
		outFile->WriteLine("pseudo_leader = {0}", svehicle_data[i].pseudo_leader);
		outFile->WriteLine("prev_accel = {0}", svehicle_data[i].prev_accel);
		outFile->WriteLine("prevlink = {0}", svehicle_data[i].prevlink);
		outFile->WriteLine("prevlane = {0}", svehicle_data[i].prevlane);
		outFile->WriteLine("routeid = {0}", svehicle_data[i].routeid);
		outFile->WriteLine("speed = {0}", svehicle_data[i].speed);
		outFile->WriteLine("speed_adj = {0}", svehicle_data[i].speed_adj);
		outFile->WriteLine("start_lag = {0}", svehicle_data[i].start_lag);
		outFile->WriteLine("turncode = {0}", svehicle_data[i].turncode);
		outFile->WriteLine("vlength = {0}", svehicle_data[i].vlength);
		outFile->WriteLine("vtype = {0}", svehicle_data[i].vtype);
		outFile->WriteLine("xcode = {0}", svehicle_data[i].xcode);
		outFile->WriteLine("will_coop_ev = {0}", svehicle_data[i].will_coop_ev);
		outFile->WriteLine("will_coop_lc = {0}", svehicle_data[i].will_coop_lc);
		outFile->WriteLine("will_move = {0}", svehicle_data[i].will_move);
		outFile->WriteLine("diverted = {0}", svehicle_data[i].diverted);
		outFile->WriteLine("dwell_timer = {0}", svehicle_data[i].dwell_timer);
		outFile->WriteLine("goal_lane = {0}", svehicle_data[i].goal_lane);
		outFile->WriteLine("has_stopped = {0}", svehicle_data[i].has_stopped);
		outFile->WriteLine("ispdicd = {0}", svehicle_data[i].ispdicd);
		outFile->WriteLine("next_stop = {0}", svehicle_data[i].next_stop);
		outFile->WriteLine("prvdist = {0}", svehicle_data[i].prvdist);
		outFile->WriteLine("prvlink = {0}", svehicle_data[i].prvlink);
		outFile->WriteLine("prvlnkicd = {0}", svehicle_data[i].prvlnkicd);
		outFile->WriteLine("qstate = {0}", svehicle_data[i].qstate);
		outFile->WriteLine("turn_code = {0}", svehicle_data[i].turn_code);
		outFile->WriteLine("turn_code2 = {0}", svehicle_data[i].turn_code2);
		outFile->WriteLine("turn_link = {0}", svehicle_data[i].turn_link);
		outFile->WriteLine("turn_link2 = {0}", svehicle_data[i].turn_link2);
		outFile->WriteLine("vehicd = {0}", svehicle_data[i].vehicd);
		outFile->WriteLine("will_jump = {0}", svehicle_data[i].will_jump);
		outFile->WriteLine("will_yield = {0}", svehicle_data[i].will_yield);
					
		outFile->Write("\n");
	}
	
	outFile->Write("\n");
}

void printFreewayLinkData(StreamWriter^ outFile, array <Wcf_freeway_link> ^ wcf_fwl)
{
	outFile->WriteLine("FREEWAY_LINK: {0}", wcf_fwl->Length);
	
	for(int il = 0; il < wcf_fwl->Length; il++)
	{
		outFile->WriteLine("#{0}", il+1);
		outFile->WriteLine("id = {0}", wcf_fwl[il].id);
		outFile->WriteLine("usn = {0}", wcf_fwl[il].usn);
		outFile->WriteLine("dsn = {0}", wcf_fwl[il].dsn);
		outFile->WriteLine("linktype = {0}", wcf_fwl[il].linktype);
		outFile->WriteLine("thrunode = {0}", wcf_fwl[il].thrunode);
		outFile->WriteLine("mainline_sending_lane = {0}", wcf_fwl[il].mainline_sending_lane);
		outFile->WriteLine("mainline_receiving_lane = {0}", wcf_fwl[il].mainline_receiving_lane);
		outFile->WriteLine("offramp_sending_lane = {0}", wcf_fwl[il].offramp_sending_lane);
		outFile->WriteLine("offramp_receiving_lane = {0}", wcf_fwl[il].offramp_receiving_lane);
		outFile->WriteLine("exitnode = {0}", wcf_fwl[il].exitnode);
		outFile->WriteLine("length = {0}", wcf_fwl[il].length);
		outFile->WriteLine("fulllanes = {0}", wcf_fwl[il].fulllanes);

		for (int iadl = 0; iadl < wcf_fwl[il].adddrop_code->Length; ++iadl)
		{
			if (wcf_fwl[il].adddrop_code[iadl] != 0)
			{
				outFile->Write("add drop lane #{0}:\t", iadl + 1);
				outFile->Write("adddrop_code = {0}\t", wcf_fwl[il].adddrop_code[iadl]);
				outFile->Write("adddrop_lane = {0}\t", wcf_fwl[il].adddrop_lane[iadl]);
				outFile->Write("adddrop_dist = {0}\t", wcf_fwl[il].adddrop_dist[iadl]);
				outFile->WriteLine("adddrop_warn = {0}", wcf_fwl[il].adddrop_warn[iadl]);
			}
		}

		for(int iaux = 0; iaux < wcf_fwl[il].auxlaneid->Length; ++iaux)
		{
			if(wcf_fwl[il].auxlaneid[iaux] != 0)
			{
				outFile->Write("auxiliary lane #{0}:\t", iaux + 1);
				outFile->Write("auxlaneid = {0}\t", wcf_fwl[il].auxlaneid[iaux]);
				outFile->Write("auxlanecode = {0}\t", wcf_fwl[il].auxlanecode[iaux]);
				outFile->WriteLine("auxlanelength = {0}", wcf_fwl[il].auxlanelength[iaux]);
			}
		}

		outFile->WriteLine("freeflowspeed = {0}", wcf_fwl[il].freeflowspeed);
		outFile->WriteLine("thru_percent = {0}", wcf_fwl[il].thru_percent);
		outFile->WriteLine("offramp_warn_distance = {0}", wcf_fwl[il].offramp_warn_distance);
		outFile->WriteLine("anticip_warning_distance = {0}", wcf_fwl[il].anticip_warning_distance);
		outFile->WriteLine("anticip_warning_speed = {0}", wcf_fwl[il].anticip_warning_speed);
		outFile->WriteLine("grade = {0}", wcf_fwl[il].grade);
		outFile->WriteLine("pavement = {0}", wcf_fwl[il].pavement);
		outFile->WriteLine("nhov_lanes = {0}", wcf_fwl[il].nhov_lanes);
		outFile->WriteLine("hov_begin = {0}", wcf_fwl[il].hov_begin);
		outFile->WriteLine("hov_end = {0}", wcf_fwl[il].hov_end);
		outFile->WriteLine("hov_code = {0}", wcf_fwl[il].hov_code);

		outFile->Write("hov_lanes:");
		for(int ihov = 0; ihov < wcf_fwl[il].hov_lanes->Length; ++ihov)
		{
			if(wcf_fwl[il].hov_lanes[ihov] != 0)
			{
				outFile->Write("\t{0}", wcf_fwl[il].hov_lanes[ihov]);
			}
		}
		outFile->Write("\n");

		outFile->WriteLine("hov_offramp_warn_distance = {0}", wcf_fwl[il].hov_offramp_warn_distance);
		outFile->WriteLine("hov_side = {0}", wcf_fwl[il].hov_side);
		outFile->WriteLine("hov_type = {0}", wcf_fwl[il].hov_type);
		outFile->WriteLine("hov_warn = {0}", wcf_fwl[il].hov_warn);
		outFile->WriteLine("hov_pct = {0}", wcf_fwl[il].hov_pct);
		outFile->WriteLine("cfmult = {0}", wcf_fwl[il].cfmult);
		outFile->WriteLine("first_detector = {0}", wcf_fwl[il].first_detector);
		outFile->WriteLine("tilt = {0}", wcf_fwl[il].tilt);
		outFile->WriteLine("curve = {0}", wcf_fwl[il].curve);
		outFile->WriteLine("shoulder_width = {0}", wcf_fwl[il].shoulder_width);
		
		outFile->Write("lane_width:");
		for(int ilan = 0; ilan < wcf_fwl[il].lane_width->Length; ++ilan)
		{
			if (wcf_fwl[il].lane_width[ilan] > 0)
			{
			    outFile->Write("\t{0}", wcf_fwl[il].lane_width[ilan]);	
			}
		}
		outFile->Write("\n");
		outFile->Write("barrier:");
		for(int ibar = 0; ibar < wcf_fwl[il].barrier->Length; ++ibar)
		{
			if (wcf_fwl[il].barrier[ibar] != 0)
			{
				outFile->Write("\t{0}", wcf_fwl[il].barrier[ibar]);		
			}
		}
		outFile->Write("\n");
		outFile->WriteLine("datastation_id = {0}", wcf_fwl[il].datastation_id);
		outFile->WriteLine("datastation_location = {0}", wcf_fwl[il].datastation_location);
		outFile->WriteLine("truck_code = {0}", wcf_fwl[il].truck_code);
		outFile->WriteLine("truck_dir = {0}", wcf_fwl[il].truck_dir);
		outFile->WriteLine("truck_lane = {0}", wcf_fwl[il].truck_lane);
		outFile->WriteLine("etl_warn = {0}", wcf_fwl[il].etl_warn);
		outFile->WriteLine("exclude_type:");
		
		for (int itype = 0; itype < wcf_fwl[il].exclude_type->Length; ++itype)
		{
			bool printEndl = false;
			for (int idx = 0; idx < wcf_fwl[il].exclude_type[itype]->Length; ++idx)
			{
				if (wcf_fwl[il].exclude_type[itype][idx] != 0)
				{
					outFile->Write("\t{0}", wcf_fwl[il].exclude_type[itype][idx]);
					printEndl = true;
				}
			}
			if (printEndl)
			{
			    outFile->Write("\n");
			}
		}
		outFile->Write("multiplier_exit:");
		for (int itype = 0; itype < wcf_fwl[il].multiplier_exit->Length; ++itype)
		{
			if (wcf_fwl[il].multiplier_exit[itype] != 0)
			{
				outFile->Write("\t{0}", wcf_fwl[il].multiplier_exit[itype]);
			}
		}
		outFile->Write("\n");
		outFile->WriteLine("startup_time = {0}", wcf_fwl[il].startup_time);
		outFile->Write("\n");
	}
	outFile->Write("\n");
}

void printStreetLinkData(StreamWriter^ outFile, array <Wcf_street_link> ^ wcf_sl)
{
	outFile->WriteLine("STREET_LINK: {0}", wcf_sl->Length);
	
	for(int il = 0; il < wcf_sl->Length; il++)
	{
		outFile->WriteLine("#{0}", il+1);
		outFile->WriteLine("id = {0}", wcf_sl[il].id);
		outFile->WriteLine("usn = {0}", wcf_sl[il].usn);
		outFile->WriteLine("dsn = {0}", wcf_sl[il].dsn);
		outFile->WriteLine("thrunode = {0}", wcf_sl[il].thrunode);
		outFile->WriteLine("leftnode = {0}", wcf_sl[il].leftnode);
		outFile->WriteLine("rightnode = {0}", wcf_sl[il].rightnode);
		outFile->WriteLine("diagnode = {0}", wcf_sl[il].diagnode);
		outFile->WriteLine("opposenode = {0}", wcf_sl[il].opposenode);
		outFile->WriteLine("length = {0}", wcf_sl[il].length);
		outFile->WriteLine("fulllanes = {0}", wcf_sl[il].fulllanes);
		outFile->WriteLine("leftturnbays = {0}", wcf_sl[il].leftturnbays);
		outFile->WriteLine("rightturnbays = {0}", wcf_sl[il].rightturnbays);
		//outFile->WriteLine("lengthofleftbay = {0}", wcf_sl[il].lengthofleftbay);
		//outFile->WriteLine("lengthofrightbay = {0}", wcf_sl[il].lengthofrightbay);
		outFile->WriteLine("freeflowspeed = {0}", wcf_sl[il].freeflowspeed);

		outFile->Write("channelization:");
		for (int j = 0; j < wcf_sl[il].channelization->Length; ++j)
		{
			outFile->Write("\t{0}", wcf_sl[il].channelization[j]);
		}
		outFile->Write("\n");

		outFile->WriteLine("leftpct = {0}", wcf_sl[il].leftpct);
		outFile->WriteLine("thrupct = {0}", wcf_sl[il].thrupct);
		outFile->WriteLine("rightpct = {0}", wcf_sl[il].rightpct);
		outFile->WriteLine("diagpct = {0}", wcf_sl[il].diagpct);
		outFile->WriteLine("grade = {0}", wcf_sl[il].grade);
		outFile->WriteLine("distribution_code = {0}", wcf_sl[il].distribution_code);
		outFile->WriteLine("startup_delay = {0}", wcf_sl[il].startup_delay);
		outFile->WriteLine("discharge_hdwy = {0}", wcf_sl[il].discharge_hdwy);
		outFile->WriteLine("rtor = {0}", wcf_sl[il].rtor);
		outFile->WriteLine("ped_code = {0}", wcf_sl[il].ped_code);
		outFile->WriteLine("lane1 = {0}", wcf_sl[il].lane1);
		outFile->WriteLine("lane2 = {0}", wcf_sl[il].lane2);
		outFile->WriteLine("cfmult = {0}", wcf_sl[il].cfmult);
		outFile->WriteLine("sight_dist = {0}", wcf_sl[il].sight_dist);
		outFile->WriteLine("first_detector = {0}", wcf_sl[il].first_detector);
		outFile->WriteLine("shoulder_width = {0}", wcf_sl[il].shoulder_width);
		
		outFile->Write("lane_width:");
		for(int ilan = 0; ilan < wcf_sl[il].lane_width->Length; ++ilan)
		{
			if (wcf_sl[il].lane_width[ilan] > 0)
			{
			    outFile->Write("\t{0}", wcf_sl[il].lane_width[ilan]);	
			}
		}
		outFile->Write("\n");

		outFile->WriteLine("ste_freq = {0}", wcf_sl[il].ste_freq);
		outFile->WriteLine("ste_duration = {0}", wcf_sl[il].ste_duration);
		outFile->WriteLine("signal_range = {0}", wcf_sl[il].signal_range);
		outFile->WriteLine("centroid = {0}", wcf_sl[il].centroid);
		outFile->WriteLine("centroid_label = {0}", wcf_sl[il].centroid_label);

		outFile->WriteLine("exclude_type:");
		for (int itype = 0; itype < wcf_sl[il].exclude_type->Length; ++itype)
		{
			bool printEndl = false;
			for (int idx = 0; idx < wcf_sl[il].exclude_type[itype]->Length; ++idx)
			{
				if (wcf_sl[il].exclude_type[itype][idx] != 0)
				{
					outFile->Write("\t{0}", wcf_sl[il].exclude_type[itype][idx]);
					printEndl = true;
				}
			}
			if (printEndl)
			{
			    outFile->Write("\n");
			}
		}
		
		outFile->Write("\n");
	}
	outFile->Write("\n");
}

void printACSignals(StreamWriter^ outFile, array<WCF_AC> ^ wcf_acl)
{
	outFile->WriteLine("AC SIGNAL: {0}", wcf_acl->Length);
	
	for(int i = 0; i < wcf_acl->Length; i++)
	{
		outFile->WriteLine("#{0}", i+1);
		outFile->WriteLine("node = {0}", wcf_acl[i].node);
		outFile->WriteLine("cfails = {0}", wcf_acl[i].cfails);
		outFile->WriteLine("adj = {0}", wcf_acl[i].adj);
		outFile->WriteLine("cycle_length = {0}", wcf_acl[i].cycle_length);
		outFile->WriteLine("offset = {0}", wcf_acl[i].offset);
	
		outFile->WriteLine("n_direct_approaches = {0}", wcf_acl[i].n_direct_approaches);
		outFile->Write("direct_approach_USN:");
		for(int j = 0; j < wcf_acl[i].n_direct_approaches; ++j)
		{
			outFile->Write("\t{0}", wcf_acl[i].direct_approach_USN[j]);	
		}
		outFile->Write("\n");

		outFile->WriteLine("phase:");
		for(int iphase = 0; iphase < 8; ++iphase)
		{
			outFile->WriteLine("\tphase {0}", iphase+1);
			outFile->WriteLine("	actuated_mode	 = {0} " , wcf_acl[i].	actuated_mode	[iphase]);
			outFile->WriteLine("	min_green_time	 = {0} " , wcf_acl[i].	min_green_time	[iphase]);
			outFile->WriteLine("	max_green_time	 = {0} " , wcf_acl[i].	max_green_time	[iphase]);
			outFile->WriteLine("	default_extension_time	 = {0} " , wcf_acl[i].	default_extension_time	[iphase]);
			outFile->WriteLine("	gap_time	 = {0} " , wcf_acl[i].	gap_time	[iphase]);
			outFile->WriteLine("	times_before_reduction	 = {0} " , wcf_acl[i].	times_before_reduction	[iphase]);
			outFile->WriteLine("	time_to_reduce	 = {0} " , wcf_acl[i].	time_to_reduce	[iphase]);
			outFile->WriteLine("	min_gap_time	 = {0} " , wcf_acl[i].	min_gap_time	[iphase]);
			outFile->WriteLine("	yellow_change_int	 = {0} " , wcf_acl[i].	yellow_change_int	[iphase]);
			outFile->WriteLine("	red_clear_int	 = {0} " , wcf_acl[i].	red_clear_int	[iphase]);
			outFile->WriteLine("	ped_allowed	 = {0} " , wcf_acl[i].	ped_allowed	[iphase]);
			outFile->WriteLine("	walk_time	 = {0} " , wcf_acl[i].	walk_time	[iphase]);
			outFile->WriteLine("	walk_clearance_time	 = {0} " , wcf_acl[i].	walk_clearance_time	[iphase]);
			outFile->Write("	left arrows: ");
			for(int j = 0; j < wcf_acl[i].n_direct_approaches; ++j)
			{
				outFile->Write("{0}", wcf_acl[i].leftarrow[j][iphase]);
			}
			outFile->Write("\n");

			outFile->Write("	thru arrows: ");
			for(int j = 0; j < wcf_acl[i].n_direct_approaches; ++j)
			{
				outFile->Write("{0}", wcf_acl[i].thruarrow[j][iphase]);
			}
			outFile->Write("\n");

			outFile->Write("	right arrows: ");
			for(int j = 0; j < wcf_acl[i].n_direct_approaches; ++j)
			{
				outFile->Write("{0}", wcf_acl[i].rightarrow[j][iphase]);
			}
			outFile->Write("\n");

			outFile->Write("	diag arrows: ");
			for(int j = 0; j < wcf_acl[i].n_direct_approaches; ++j)
			{
				outFile->Write("{0}", wcf_acl[i].diagarrow[j][iphase]);
			}
			outFile->Write("\n");
		}

		outFile->WriteLine("  Ring Phase: ");
		for (int iRing = 0; iRing <2; ++iRing)
		{
			outFile->Write( "  	Ring {0}: ",iRing+1);
			for (int j = 0; j < 4; ++j)
			{
				//outFile->Write("{0}", wcf_acl[i].ring_phase[j][iRing]);
				outFile->Write("{0}", wcf_acl[i].ring_phase[j + iRing * 4]);
			}
			outFile->Write("\n");
		}

		outFile->Write("\n");
	}
	outFile->Write("\n");
}

void printFTCSignals(StreamWriter^ outFile, array<WCF_FTC_DATA>^ wcf_ftc)
{
	outFile->WriteLine("FTC SIGNAL: {0}", wcf_ftc->Length);
	
	for(int i = 0; i < wcf_ftc->Length; i++)
	{
		outFile->WriteLine("#{0}", i+1);
		outFile->WriteLine("approaches = {0}", wcf_ftc[i].approaches);
		outFile->Write("approach:");
		for(int j = 0; j < wcf_ftc[i].approaches; ++j)
		{
			outFile->Write("\t{0}", wcf_ftc[i].approach[j]);	
		}
		outFile->Write("\n");

		outFile->WriteLine("active_intervals = {0}", wcf_ftc[i].active_intervals);
		outFile->Write("duration:");
		for(int j = 0; j < wcf_ftc[i].active_intervals; ++j)
		{
			outFile->Write("\t{0}", wcf_ftc[i].duration[j]);	
		}
		outFile->Write("\n");

		outFile->WriteLine("signal_code:");
		for(int j = 0; j < wcf_ftc[i].active_intervals; ++j)
		{
			outFile->Write("\tduration{0}:", j+1);
			for(int k = 0; k < wcf_ftc[i].approaches; ++k)
			{
				outFile->Write("\t{0}", wcf_ftc[i].signal_code[j][k]);	
			}
			outFile->Write("\n");
		}


		outFile->WriteLine("current_interval = {0}", wcf_ftc[i].current_interval);
		outFile->WriteLine("cycle_length = {0}", wcf_ftc[i].cycle_length);
		outFile->WriteLine("external_control = {0}", wcf_ftc[i].external_control);
		outFile->WriteLine("node = {0}", wcf_ftc[i].node);
		outFile->WriteLine("offset = {0}", wcf_ftc[i].offset);
		outFile->WriteLine("range = {0}", wcf_ftc[i].range);
		outFile->WriteLine("time_in_interval = {0}", wcf_ftc[i].time_in_interval);
		
		outFile->Write("\n");
	}
	outFile->Write("\n");
}

void printEntryNodes(StreamWriter^ outFile, array <WCF_ENTRYNODES_DATA> ^ wcf_entry_node)
{
	outFile->WriteLine("ENTRY NODES: {0}", wcf_entry_node->Length);
	
	for(int i = 0; i < wcf_entry_node->Length; i++)
	{
		outFile->WriteLine("#{0}", i+1);
		outFile->WriteLine("Node_ID = {0}", wcf_entry_node[i].Node_ID);
		outFile->WriteLine("truck_pct = {0}", wcf_entry_node[i].truck_pct);
		outFile->WriteLine("carpool_pct = {0}", wcf_entry_node[i].carpool_pct);
		outFile->WriteLine("flowrate = {0}", wcf_entry_node[i].flowrate);
		outFile->WriteLine("hov_violators_per10000 = {0}", wcf_entry_node[i].hov_violators_per10000);
		outFile->WriteLine("SS_USN = {0}", wcf_entry_node[i].SS_USN);
		outFile->WriteLine("SS_DSN = {0}", wcf_entry_node[i].SS_DSN);

		outFile->Write("lane_pct:");
		for(int j = 0; j < wcf_entry_node[i].lane_pct->Length; ++j)
		{
			if (wcf_entry_node[i].lane_pct[j] != 0) 
			{
			    outFile->Write("\t{0}", wcf_entry_node[i].lane_pct[j]);	
			}
		}
		outFile->Write("\n");
		outFile->Write("\n");
	}
	outFile->Write("\n");
}

void printRampMeter(StreamWriter^ outFile, array<WCF_RM_DATA>^ wcf_rampmeter_inputs)
{
	outFile->WriteLine("RAMP METERS: {0}", wcf_rampmeter_inputs->Length);
	
	for(int i = 0; i < wcf_rampmeter_inputs->Length; i++)
	{
		outFile->WriteLine("#{0}", i+1);
		outFile->WriteLine("dsn = {0}", wcf_rampmeter_inputs[i].dsn);
		outFile->WriteLine("link = {0}", wcf_rampmeter_inputs[i].link);
		outFile->WriteLine("control = {0}", wcf_rampmeter_inputs[i].control);
		outFile->WriteLine("onset = {0}", wcf_rampmeter_inputs[i].onset);
		outFile->WriteLine("state = {0}", wcf_rampmeter_inputs[i].state);
		
		outFile->Write("detector:");
		for(int j = 0; j < wcf_rampmeter_inputs[i].detector->Length; ++j)
		{
			if (wcf_rampmeter_inputs[i].detector[j] != 0) 
			{
			    outFile->Write("\t{0}", wcf_rampmeter_inputs[i].detector[j]);	
			}
		}
		outFile->Write("\n");

		outFile->WriteLine("capacity = {0}", wcf_rampmeter_inputs[i].capacity);

		outFile->Write("speed:");
		for(int j = 0; j < wcf_rampmeter_inputs[i].speed->Length; ++j)
		{
			if (wcf_rampmeter_inputs[i].speed[j] != 0) 
			{
			    outFile->Write("\t{0}", wcf_rampmeter_inputs[i].speed[j]);	
			}
		}
		outFile->Write("\n");

		outFile->Write("headway:");
		for(int j = 0; j < wcf_rampmeter_inputs[i].headway->Length; ++j)
		{
			if (wcf_rampmeter_inputs[i].headway[j] != 0) 
			{
			    outFile->Write("\t{0}", wcf_rampmeter_inputs[i].headway[j]);
			}
		}
		outFile->Write("\n");

		outFile->WriteLine("timer = {0}", wcf_rampmeter_inputs[i].timer);
		outFile->WriteLine("updint = {0}", wcf_rampmeter_inputs[i].updint);
		outFile->WriteLine("twopergreen = {0}", wcf_rampmeter_inputs[i].twopergreen);

		outFile->Write("\n");
	}
	outFile->Write("\n");
}

void printNetworkInputs(StreamWriter^ outFile, array <WCF_NETWORK_INPUTS> ^ wcf_ni)
{
	outFile->WriteLine("NETWORK INPUTS:");

	outFile->WriteLine("run_init = {0}", wcf_ni[0].run_init);
	outFile->WriteLine("initialization_end = {0}", wcf_ni[0].initialization_end);
	outFile->WriteLine("time_interval = {0}", wcf_ni[0].time_interval);
	outFile->WriteLine("timestep = {0}", wcf_ni[0].timestep);
	outFile->WriteLine("type_of_run = {0}", wcf_ni[0].type_of_run);
	outFile->WriteLine("sim_start_time = {0}", wcf_ni[0].sim_start_time);
	outFile->WriteLine("max_node_number = {0}", wcf_ni[0].max_node_number);
	outFile->Write("time_period_duration:");
	for (int i = 0; i < wcf_ni[0].time_period_duration->Length; ++i)
	{
		if (wcf_ni[0].time_period_duration[i] != 0)
		    outFile->Write("\t{0}", wcf_ni[0].time_period_duration[i]);
	}
	outFile->Write("\n");
	outFile->Write("\n");
}

void printFNetworkInputs(StreamWriter^ outFile, array <WCF_FREEWAY_NETWORK_INPUTS> ^ wcf_fni)
{
	outFile->WriteLine("FREEWAY NETWORK INPUTS:");

	outFile->Write("cfrict:");
	for (int i = 0; i < wcf_fni[0].cfrict->Length; ++i)
	{
		if (wcf_fni[0].cfrict[i] != 0)
		    outFile->Write("\t{0}", wcf_fni[0].cfrict[i]);
	}
	outFile->Write("\n");

	outFile->WriteLine("default_hov_pct = {0}", wcf_fni[0].default_hov_pct);
	outFile->WriteLine("lag_accel = {0}", wcf_fni[0].lag_accel);
	outFile->WriteLine("lag_decel = {0}", wcf_fni[0].lag_decel);

	outFile->Write("ffspeed_adj:");
	for (int i = 0; i < wcf_fni[0].ffspeed_adj->Length; ++i)
	{
		if (wcf_fni[0].ffspeed_adj[i] != 0)
		    outFile->Write("\t{0}", wcf_fni[0].ffspeed_adj[i]);
	}
	outFile->Write("\n");

	outFile->Write("zfoll:");
	//for (int i = 0; i < wcf_fni[0].zfoll->Length; ++i)
	//{
	//	if (wcf_fni[0].zfoll[i] != 0)
	//	    outFile->Write("\t{0}", wcf_fni[0].zfoll[i]);
	//}
	outFile->Write("\n");

	outFile->WriteLine("freeway_pct_coop = {0}", wcf_fni[0].freeway_pct_coop);
	outFile->WriteLine("lc_time = {0}", wcf_fni[0].lc_time);
	outFile->WriteLine("dlc_mult = {0}", wcf_fni[0].dlc_mult);

	outFile->Write("\n");
}

void printSNetworkInputs(StreamWriter^ outFile, array <WCF_STREET_NETWORK_INPUTS> ^ wcf_sni)
{
	outFile->WriteLine("STREET NETWORK INPUTS:");

	outFile->Write("additional_gap:");
	for (int i = 0; i < wcf_sni[0].additional_gap->Length; ++i)
	{
		//if (wcf_sni[0].additional_gap[i] != 0)
		    outFile->Write("\t{0}", wcf_sni[0].additional_gap[i]);
	}
	outFile->Write("\n");

	outFile->Write("amber_decel:");
	for (int i = 0; i < wcf_sni[0].amber_decel->Length; ++i)
	{
		//if (wcf_sni[0].amber_decel[i] != 0)
		    outFile->Write("\t{0}", wcf_sni[0].amber_decel[i]);
	}
	outFile->Write("\n");

	outFile->WriteLine("lt_speed = {0}", wcf_sni[0].lt_speed);
	outFile->WriteLine("rt_speed = {0}", wcf_sni[0].rt_speed);
	
	outFile->Write("pdelay_weak:");
	for (int i = 0; i < wcf_sni[0].pdelay_weak->Length; ++i)
	{
		//if (wcf_sni[0].pdelay_weak[i] != 0)
		    outFile->Write("\t{0}", wcf_sni[0].pdelay_weak[i]);
	}
	outFile->Write("\n");

	outFile->Write("pdelay_strong:");
	for (int i = 0; i < wcf_sni[0].pdelay_strong->Length; ++i)
	{
		//if (wcf_sni[0].pdelay_strong[i] != 0)
		    outFile->Write("\t{0}", wcf_sni[0].pdelay_strong[i]);
	}
	outFile->Write("\n");

	outFile->Write("ped_duration:");
	for (int i = 0; i < wcf_sni[0].ped_duration->Length; ++i)
	{
		//if (wcf_sni[0].ped_duration[i] != 0)
		    outFile->Write("\t{0}", wcf_sni[0].ped_duration[i]);
	}
	outFile->Write("\n");

	outFile->Write("acceptable_gap:");
	for (int i = 0; i < wcf_sni[0].acceptable_gap->Length; ++i)
	{
		//if (wcf_sni[0].acceptable_gap[i] != 0)
		    outFile->Write("\t{0}", wcf_sni[0].acceptable_gap[i]);
	}
	outFile->Write("\n");

	outFile->Write("acceptable_ltg:");
	for (int i = 0; i < wcf_sni[0].acceptable_ltg->Length; ++i)
	{
		//if (wcf_sni[0].acceptable_ltg[i] != 0)
		    outFile->Write("\t{0}", wcf_sni[0].acceptable_ltg[i]);
	}
	outFile->Write("\n");

	outFile->Write("acceptable_rtg:");
	for (int i = 0; i < wcf_sni[0].acceptable_rtg->Length; ++i)
	{
		//if (wcf_sni[0].acceptable_rtg[i] != 0)
		    outFile->Write("\t{0}", wcf_sni[0].acceptable_rtg[i]);
	}
	outFile->Write("\n");

	outFile->WriteLine("dwell_multiplier:");
	for (int i = 0; i < wcf_sni[0].dwell_multiplier->Length; ++i)
	{
		for (int j = 0; j < wcf_sni[0].dwell_multiplier[i]->Length; ++j)
		{
		    //if (wcf_sni[0].dwell_multiplier[i][j] != 0)
		        outFile->Write("\t{0}", wcf_sni[0].dwell_multiplier[i][j]);
		}
		outFile->Write("\n");
	}

	outFile->Write("ffspeed_adj:");
	for (int i = 0; i < wcf_sni[0].ffspeed_adj->Length; ++i)
	{
		//if (wcf_sni[0].ffspeed_adj[i] != 0)
		    outFile->Write("\t{0}", wcf_sni[0].ffspeed_adj[i]);
	}
	outFile->Write("\n");

	outFile->Write("zfoll:");
	//for (int i = 0; i < wcf_sni[0].zfoll->Length; ++i)
	//{
	//	//if (wcf_sni[0].zfoll[i] != 0)
	//	    outFile->Write("\t{0}", wcf_sni[0].zfoll[i]);
	//}
	outFile->Write("\n");

	outFile->WriteLine("lc_time = {0}", wcf_sni[0].lc_time);

	outFile->Write("lt_jumper_prob:");
	for (int i = 0; i < wcf_sni[0].lt_jumper_prob->Length; ++i)
	{
		//if (wcf_sni[0].lt_jumper_prob[i] != 0)
		    outFile->Write("\t{0}", wcf_sni[0].lt_jumper_prob[i]);
	}
	outFile->Write("\n");

	outFile->Write("lt_lagger_prob:");
	for (int i = 0; i < wcf_sni[0].lt_lagger_prob->Length; ++i)
	{
		//if (wcf_sni[0].lt_lagger_prob[i] != 0)
		    outFile->Write("\t{0}", wcf_sni[0].lt_lagger_prob[i]);
	}
	outFile->Write("\n");

	outFile->Write("spillback_prob:");
	for (int i = 0; i < wcf_sni[0].spillback_prob->Length; ++i)
	{
		//if (wcf_sni[0].spillback_prob[i] != 0)
		    outFile->Write("\t{0}", wcf_sni[0].spillback_prob[i]);
	}
	outFile->Write("\n");

	outFile->WriteLine("stop_spd = {0}", wcf_sni[0].stop_spd);
	outFile->WriteLine("street_pct_coop = {0}", wcf_sni[0].street_pct_coop);
	outFile->WriteLine("yield_spd = {0}", wcf_sni[0].yield_spd);
	outFile->WriteLine("driver_fampct = {0}", wcf_sni[0].driver_fampct);

	outFile->Write("qfactor:");
	for (int i = 0; i < wcf_sni[0].qfactor->Length; ++i)
	{
		//if (wcf_sni[0].qfactor[i] != 0)
		    outFile->Write("\t{0}", wcf_sni[0].qfactor[i]);
	}
	outFile->Write("\n");

	outFile->Write("ste_mult:");
	for (int i = 0; i < wcf_sni[0].ste_mult->Length; ++i)
	{
		//if (wcf_sni[0].ste_mult[i] != 0)
		    outFile->Write("\t{0}", wcf_sni[0].ste_mult[i]);
	}
	outFile->Write("\n");

	outFile->Write("\n");
}

void printVTypeInputs(StreamWriter^ outFile, array <WCF_VEHICLE_TYPE_DATA> ^ wcf_vti)
{
	outFile->WriteLine("VEHICLE TYPE INPUTS: {0}", wcf_vti->Length);

	for (int i = 0; i < wcf_vti->Length; ++i)
	{
		outFile->WriteLine("#{0}", i+1);
		outFile->WriteLine("length = {0}", wcf_vti[i].length);
		outFile->WriteLine("headway_factor = {0}", wcf_vti[i].headway_factor);
		outFile->WriteLine("average_occupancy = {0}", wcf_vti[i].average_occupancy);
		outFile->WriteLine("emergency_decel = {0}", wcf_vti[i].emergency_decel);
		outFile->WriteLine("fleet_freeway_auto = {0}", wcf_vti[i].fleet_freeway_auto);
		outFile->WriteLine("fleet_freeway_truck = {0}", wcf_vti[i].fleet_freeway_truck);
		outFile->WriteLine("fleet_freeway_carpool = {0}", wcf_vti[i].fleet_freeway_carpool);
		outFile->WriteLine("fleet_freeway_bus = {0}", wcf_vti[i].fleet_freeway_bus);
		outFile->WriteLine("fleet_freeway_ev = {0}", wcf_vti[i].fleet_freeway_ev);
		outFile->WriteLine("fleet_freeway_bike = {0}", wcf_vti[i].fleet_freeway_bike);
		outFile->WriteLine("fleet_street_auto = {0}", wcf_vti[i].fleet_street_auto);
		outFile->WriteLine("fleet_street_truck = {0}", wcf_vti[i].fleet_street_truck);
		outFile->WriteLine("fleet_street_carpool = {0}", wcf_vti[i].fleet_street_carpool);
		outFile->WriteLine("fleet_street_bus = {0}", wcf_vti[i].fleet_street_bus);
		outFile->WriteLine("fleet_street_ev = {0}", wcf_vti[i].fleet_street_ev);
		outFile->WriteLine("fleet_street_bike = {0}", wcf_vti[i].fleet_street_bike);
		outFile->Write("\n");
	}
	
	outFile->Write("\n");
}

void printDetectorInputs(StreamWriter^ outFile, String^ type, array <WCF_DETECTOR_INPUTS> ^ wcf_di)
{
	outFile->WriteLine("{0} DETECTOR INPUTS: {1}", type, wcf_di->Length);

	for (int i = 0; i < wcf_di->Length; ++i)
	{
		outFile->WriteLine("#{0}", i+1);
		outFile->WriteLine("usn = {0}", wcf_di[i].usn);
		outFile->WriteLine("dsn = {0}", wcf_di[i].dsn);
		outFile->WriteLine("associated_phase = {0}", wcf_di[i].associated_phase);
		outFile->WriteLine("station_id = {0}", wcf_di[i].station_id);
		outFile->WriteLine("location = {0}", wcf_di[i].location);
		outFile->WriteLine("link = {0}", wcf_di[i].link);
		outFile->WriteLine("lane1 = {0}", wcf_di[i].lane1);
		outFile->WriteLine("lane2 = {0}", wcf_di[i].lane2);
		outFile->WriteLine("zone_length = {0}", wcf_di[i].zone_length);
		outFile->WriteLine("delay_time = {0}", wcf_di[i].delay_time);
		outFile->WriteLine("carryover_time = {0}", wcf_di[i].carryover_time);
		outFile->WriteLine("type_code = {0}", wcf_di[i].type_code);
		outFile->WriteLine("operation_code = {0}", wcf_di[i].operation_code);
		outFile->WriteLine("detection_zone = {0}", wcf_di[i].detection_zone);
		outFile->Write("\n");
	}
	
	outFile->Write("\n");
}

void printCondTurnpctInputs(StreamWriter^ outFile, array<WCF_COND_TURNPCTS>^ wcf_ct)
{
	outFile->WriteLine("COND_TURNPCTS: {0}", wcf_ct->Length);

	for (int i = 0; i < wcf_ct->Length; ++i)
	{
		outFile->WriteLine("#{0}", i+1);
		outFile->WriteLine("USN = {0}", wcf_ct[i].USN);
		outFile->WriteLine("DSN = {0}", wcf_ct[i].DSN);

		outFile->Write("LEFTPCT:");
		for (int j = 0; j < wcf_ct[i].LEFTPCT->Length; ++j) 
		{
			outFile->Write("\t{0}", wcf_ct[i].LEFTPCT[j]);
		}
		outFile->Write("\n");

		outFile->Write("THRUPCT:");
		for (int j = 0; j < wcf_ct[i].THRUPCT->Length; ++j) 
		{
			outFile->Write("\t{0}", wcf_ct[i].THRUPCT[j]);
		}
		outFile->Write("\n");

		outFile->Write("RIGHTPCT:");
		for (int j = 0; j < wcf_ct[i].RIGHTPCT->Length; ++j) 
		{
			outFile->Write("\t{0}", wcf_ct[i].RIGHTPCT[j]);
		}
		outFile->Write("\n");

		outFile->Write("DIAGPCT:");
		for (int j = 0; j < wcf_ct[i].DIAGPCT->Length; ++j) 
		{
			outFile->Write("\t{0}", wcf_ct[i].DIAGPCT[j]);
		}
		outFile->Write("\n");

		outFile->Write("\n");
	}
	
	outFile->Write("\n");
}

void printBusRouteInputs(StreamWriter^ outFile, array<WCF_BUSROUTE_DATA>^ wcf_busroute_inputs)
{
	outFile->WriteLine("BUSROUTE_DATA: {0}", wcf_busroute_inputs->Length);

	for (int i = 0; i < wcf_busroute_inputs->Length; ++i)
	{
		outFile->WriteLine("#{0}", i+1);
		outFile->WriteLine("number = {0}", wcf_busroute_inputs[i].number);
		outFile->WriteLine("hdwy = {0}", wcf_busroute_inputs[i].hdwy);
		outFile->WriteLine("offset = {0}", wcf_busroute_inputs[i].offset);
		outFile->WriteLine("nodes = {0}", wcf_busroute_inputs[i].nodes);
		
		outFile->Write("route_nodes:");
		for (int j = 0; j < wcf_busroute_inputs[i].route_nodes->Length; ++j) 
		{
			if (wcf_busroute_inputs[i].route_nodes[j] != 0)
			    outFile->Write("\t{0}", wcf_busroute_inputs[i].route_nodes[j]);
		}
		outFile->Write("\n");

		outFile->Write("stationlist:");
		for (int j = 0; j < wcf_busroute_inputs[i].stationlist->Length; ++j) 
		{
			if (wcf_busroute_inputs[i].stationlist[j] != 0)
			    outFile->Write("\t{0}", wcf_busroute_inputs[i].stationlist[j]);
		}
		outFile->Write("\n");

		outFile->WriteLine("persontrips = {0}", wcf_busroute_inputs[i].persontrips);
		outFile->WriteLine("timer = {0}", wcf_busroute_inputs[i].timer);
		outFile->WriteLine("traveltime = {0}", wcf_busroute_inputs[i].traveltime);
		outFile->WriteLine("trips = {0}", wcf_busroute_inputs[i].trips);

		outFile->Write("\n");
	}
	
	outFile->Write("\n");
}

void printBusStationInputs(StreamWriter^ outFile, array<WCF_BUSSTATION_DATA>^ wcf_busstation_inputs)
{
	outFile->WriteLine("BUSSTATION_DATA: {0}", wcf_busstation_inputs->Length);

	for (int i = 0; i < wcf_busstation_inputs->Length; ++i)
	{
		outFile->WriteLine("#{0}", i+1);
		outFile->WriteLine("block_code = {0}", wcf_busstation_inputs[i].block_code);
		outFile->WriteLine("usn = {0}", wcf_busstation_inputs[i].usn);
		outFile->WriteLine("dsn = {0}", wcf_busstation_inputs[i].dsn);
		outFile->WriteLine("capacity = {0}", wcf_busstation_inputs[i].capacity);
		outFile->WriteLine("location = {0}", wcf_busstation_inputs[i].location);
		outFile->WriteLine("dwell = {0}", wcf_busstation_inputs[i].dwell);
		outFile->WriteLine("bypass_pct = {0}", wcf_busstation_inputs[i].bypass_pct);
		outFile->WriteLine("next_station = {0}", wcf_busstation_inputs[i].next_station);
		outFile->WriteLine("pocket_lane = {0}", wcf_busstation_inputs[i].pocket_lane);
		outFile->WriteLine("front = {0}", wcf_busstation_inputs[i].front);
		outFile->WriteLine("count = {0}", wcf_busstation_inputs[i].count);
		outFile->WriteLine("dwell_time = {0}", wcf_busstation_inputs[i].dwell_time);
		outFile->WriteLine("empty_time = {0}", wcf_busstation_inputs[i].empty_time);
		outFile->WriteLine("overflow_time = {0}", wcf_busstation_inputs[i].overflow_time);

		outFile->Write("\n");
	}
	
	outFile->Write("\n");
}

void printIncidentInputs(StreamWriter^ outFile, array<WCF_INCIDENT_DATA>^ wcf_incident_inputs)
{
	outFile->WriteLine("INCIDENT_DATA: {0}", wcf_incident_inputs->Length);

	for (int i = 0; i < wcf_incident_inputs->Length; ++i)
	{
		outFile->WriteLine("#{0}", i+1);
		outFile->WriteLine("usn = {0}", wcf_incident_inputs[i].usn);
		outFile->WriteLine("dsn = {0}", wcf_incident_inputs[i].dsn);
		outFile->WriteLine("begin_point = {0}", wcf_incident_inputs[i].begin_point);
		outFile->WriteLine("begin_time = {0}", wcf_incident_inputs[i].begin_time);
		outFile->WriteLine("end_point = {0}", wcf_incident_inputs[i].end_point);
		outFile->WriteLine("end_time = {0}", wcf_incident_inputs[i].end_time);
		outFile->WriteLine("rbnf = {0}", wcf_incident_inputs[i].rbnf);
		outFile->WriteLine("warn_point = {0}", wcf_incident_inputs[i].warn_point);

		outFile->Write("code:");
		for (int j = 0; j < wcf_incident_inputs[i].code->Length; ++j) 
		{
			if (wcf_incident_inputs[i].code[j] != 0)
			    outFile->Write("\t{0}", wcf_incident_inputs[i].code[j]);
		}
		outFile->Write("\n");

		outFile->Write("\n");
	}
	
	outFile->Write("\n");
}

void printCoordInputs(StreamWriter^ outFile, array<WCF_NODE_LOCATION_DATA>^ wcf_xy_coord_inputs)
{
	outFile->WriteLine("NODE_LOCATION_DATA: {0}", wcf_xy_coord_inputs->Length);

	for (int i = 0; i < wcf_xy_coord_inputs->Length; ++i)
	{
		if (wcf_xy_coord_inputs[i].x != 0 && wcf_xy_coord_inputs[i].y!= 0)
		{
			outFile->WriteLine("Node ID: {0}", i+1);
			outFile->WriteLine("x = {0}", wcf_xy_coord_inputs[i].x);
			outFile->WriteLine("y = {0}", wcf_xy_coord_inputs[i].y);
			outFile->WriteLine("latitude = {0}", wcf_xy_coord_inputs[i].latitude);
			outFile->WriteLine("longitude = {0}", wcf_xy_coord_inputs[i].longitude);
			outFile->WriteLine("elevation = {0}", wcf_xy_coord_inputs[i].elevation);
			outFile->WriteLine("is_defined = {0}", wcf_xy_coord_inputs[i].is_defined);
			outFile->Write("\n");
		}
	}

	outFile->Write("\n");
}

void printParkingZones(StreamWriter^ outFile, array<WCF_PARKING_DATA>^ wcf_parking_zones)
{
	outFile->WriteLine("PARKING_DATA: {0}", wcf_parking_zones->Length);

	for (int i = 0; i < wcf_parking_zones->Length; ++i)
	{
		outFile->WriteLine("#{0}", i+1);
		outFile->WriteLine("usn = {0}", wcf_parking_zones[i].usn);
		outFile->WriteLine("dsn = {0}", wcf_parking_zones[i].dsn);
		outFile->WriteLine("duration = {0}", wcf_parking_zones[i].duration);
		outFile->WriteLine("freq = {0}", wcf_parking_zones[i].freq);
		outFile->WriteLine("left_start = {0}", wcf_parking_zones[i].left_start);
		outFile->WriteLine("left_len = {0}", wcf_parking_zones[i].left_len);
		outFile->WriteLine("right_start = {0}", wcf_parking_zones[i].right_start);
		outFile->WriteLine("right_len = {0}", wcf_parking_zones[i].right_len);
		
		outFile->Write("\n");
	}

	outFile->Write("\n");
}

void printEvents(StreamWriter^ outFile, array<WCF_EVENT_DATA>^ wcf_events)
{
	outFile->WriteLine("EVENT_DATA: {0}", wcf_events->Length);

	for (int i = 0; i < wcf_events->Length; ++i)
	{
		outFile->WriteLine("#{0}", i+1);
		outFile->WriteLine("begin_time = {0}", wcf_events[i].begin_time);
		outFile->WriteLine("end_time = {0}", wcf_events[i].end_time);
		outFile->WriteLine("lane = {0}", wcf_events[i].lane);
		outFile->WriteLine("usn = {0}", wcf_events[i].usn);
		outFile->WriteLine("dsn = {0}", wcf_events[i].dsn);
		outFile->WriteLine("location = {0}", wcf_events[i].location);
		//outFile->WriteLine("type = {0}", wcf_events[i].type);
		
		outFile->Write("\n");
	}

	outFile->Write("\n");
}

void printDiversions(StreamWriter^ outFile, array<WCF_DIVERSION_DATA>^ wcf_diversions)
{
	outFile->WriteLine("DIVERSION_DATA: {0}", wcf_diversions->Length);

	for (int i = 0; i < wcf_diversions->Length; ++i)
	{
		outFile->WriteLine("#{0}", i+1);
		outFile->WriteLine("link = {0}", wcf_diversions[i].link);
		outFile->WriteLine("begin_time = {0}", wcf_diversions[i].begin_time);
		outFile->WriteLine("end_time = {0}", wcf_diversions[i].end_time);
		outFile->WriteLine("location = {0}", wcf_diversions[i].location);
		outFile->WriteLine("pathid = {0}", wcf_diversions[i].pathid);
		outFile->WriteLine("percentage = {0}", wcf_diversions[i].percentage);
		outFile->WriteLine("speed = {0}", wcf_diversions[i].speed);
		
		outFile->Write("\n");
	}

	outFile->Write("\n");
}

int getCommand()
{
	std::string input = "";
	int number = 0;
	while (true) {
		cout << "Please enter a command: " << endl;
		getline(cin, input);

		stringstream sstream(input);
		if (sstream >> number) {
			break;
		}
		cout << "Invalid command, please try again: " << endl;
	}
	return number;
}
