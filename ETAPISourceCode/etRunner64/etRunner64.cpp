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
//
// testInterface.cpp : Defines the entry point for the console application.
//
#include "stdafx.h"
#include <iostream>
#include "etRunner64.h"
#include "Display.h"
#include "Host_Default_Network.h"


#define PRINTOUT 0

using namespace std;

////////////////////////////////////////////////////////////////////////////////
// command-line argument names
////////////////////////////////////////////////////////////////////////////////
namespace argnames
{
	const std::string simdll  = "simdll";
	const std::string tsd     = "tsd"; //add when animation files are produced
	const std::string tid     = "tid"; //add when animation files are produced
	const std::string out     = "out";
	const std::string csv     = "csv";
	const std::string runs    = "runs";
	const std::string rnsfile = "rnsfile";
	const std::string trffile = "trffile";
	const std::string h		  = "-h";
	const std::string help	  = "-help";
	const std::string txt	  = "txt";
	const std::string writeTRF = "-o";
}

////////////////////////////////////////////////////////////////////////////////
// usage
//
// Provide a description of how to use the program.
////////////////////////////////////////////////////////////////////////////////
void usage()
{
	using namespace argnames;

	std::cout << std::endl << "usage:" << std::endl;
	std::cout << "\tetRunner trffile=\"c:\\full path to\\input.trf\" [tsd]" << std::endl << std::endl;
	std::cout << "Recognized Options:" << std::endl << std::endl;
	std::cout << tsd << "       - generate animation TXT file" << std::endl;
	std::cout << trffile << "=\"c:\\full path to\\trffile.trf\"" << std::endl;
	std::cout << std::endl << "options may be specified in any order." << std::endl;
	std::cout << std::endl;
}


int main(int argc, char* argv[]) 
{
	ServiceHost^ host = gcnew ServiceHost(Service1::typeid);
	IService1^ proxy;

	int status;

	int APIFlag = 0;
	int wcfServer = 0;
	int HostFlag = 0;
	int ClientFlag = 0;
	int PauseFlag = 1;
	int TRFInputFlag = 0;

	int TSDFlag = 0;
	int TIDFlag = 0;
	int CSVFlag = 0;
	int OutFlag = 0;
	int WriteTRFFlag = 0;
	int WriteTextFlag = 0;

	int NumberRuns = 1;

	char user_input;
	
	bool use_controller = false;

	int updatedFlag = 0;
	
	std::string dataPath = "APIOutput\\";
	std::string fileName1 = "After_2";
	int fileCount1 = 0;
	std::string fileName2 = "After_4";
	int fileCount2 = 0;
	std::string tmpName, prefix;

	std::string rnsFile = "rnsfile.dat";
	std::string trfFILE = "C:\\coordination_test_new_format.trf";
	
	for(int i = 1; i < argc; i++)
	{
		using namespace argnames;

		std::string argument = argv[i];

		if(argument == tsd)
		{
			TSDFlag = 2;
		}
		// write to the TSIS window
		else if(argument == out)
		{
			OutFlag = 1;
		}
		// write to the csv file
		else if(argument == csv)
		{
			CSVFlag = 1;
		}
		// number of runs
		else if(argument.substr(0, runs.size()) == runs)
		{
			NumberRuns = atoi(argument.substr(runs.size() + 1).c_str());
		}
		// rns file
		else if(argument.substr(0, rnsfile.size()) == rnsfile)
		{
			rnsFile = argument.substr(rnsfile.size() + 1).c_str();
		}
		else if(argument.substr(0, trffile.size()) == trffile)
		{
			trfFILE = argument.substr(trffile.size() + 1).c_str();
			TRFInputFlag = 1;
		}
		else if(argument == help || argument == h)
		{
			usage();
			return 1;
		}
		else if(argument == txt)
		{
			//TSDFlag = 2;
			WriteTextFlag = 1;
		}
		else if(argument == writeTRF)
		{
			WriteTRFFlag = 1;
		}
	}

	etFommInterface etFommIF = etFommInterface(); 

	std::cout << "Please enter <a> or <w> char to start API program with command line interface." << std::endl;
#if _UseController
	std::cout << "Using <a> to simulate with ONLY Enhanced SCOPE for actuated control..." << std::endl;
	std::cout << "Using <w> to simulate with Enhanced SCOPE and 2070 Advanced Traffic Controller for actuated control..." << std::endl;
#endif
#ifndef _FASTTEST
	std::cin >> user_input;
#else
	user_input = 'a';
	std::cout << user_input << std::endl;
#endif
	if (user_input == 'a' ||user_input == 'w')
	{
		APIFlag = 1;
#if _UseController
		if (user_input == 'w')
		{
			use_controller = true;
#if _NamedPipe
			std::cout << "please specify the node ID to use 2070 signal Controller." << std::endl;
			int ControllerNodeID;
			ControllerNodeID = 375;
			std::cout << "Controller node ID = " << ControllerNodeID << std::endl << std::endl;
			etFommIF.SetUseControllerFlag(true);
			etFommIF.SetControllerNodeID(ControllerNodeID);
#endif
		}
#endif
	} 

	if (APIFlag == 1)
	{
		/* start server host in WCF solution*/
		std::cout << "Starting WCF service ..." << std::endl << std::endl;;
#if WSHTTPBINDING
		WSHttpBinding^ sevBinding = gcnew WSHttpBinding();
		sevBinding->MaxBufferPoolSize = 2147483647;
		sevBinding->MaxReceivedMessageSize = 2147483647;
		sevBinding->Security->Mode = System::ServiceModel::SecurityMode::None;
		host->AddServiceEndpoint(IService1::typeid,sevBinding, L"http://localhost:8000/service");
#endif

#if NETTCPBINDING
		NetTcpBinding^ sevBinding = gcnew NetTcpBinding();
		host->AddServiceEndpoint(IService1::typeid, sevBinding, L"net.tcp://localhost:6000/service");
		sevBinding->MaxBufferPoolSize = 2147483647;
		sevBinding->MaxReceivedMessageSize = 2147483647;
		sevBinding->MaxBufferSize = 2147483647;
		sevBinding->Security->Mode = System::ServiceModel::SecurityMode::None;
		TimeSpan *TimeOutSetting = new TimeSpan(1, 0, 0);
		sevBinding->OpenTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
		sevBinding->CloseTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
		sevBinding->SendTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
		sevBinding->ReceiveTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
		
#ifdef _TCP_DIST
		sevBinding->Security->Mode = System::ServiceModel::SecurityMode::None;
#endif
#endif
		
#if UDPBINDING
		UdpBinding^ sevBinding = gcnew UdpBinding();
		host->AddServiceEndpoint(IService1::typeid, sevBinding, L"soap.udp://localhost:8080/service");
		sevBinding->MaxBufferPoolSize = 2147483647;
		sevBinding->MaxReceivedMessageSize = 2147483647;
		TimeSpan *TimeOutSetting = new TimeSpan(1, 0, 0);
		sevBinding->OpenTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
		sevBinding->CloseTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
		sevBinding->SendTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
		sevBinding->ReceiveTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
		sevBinding->TimeToLive = 255;
#endif

#if	0//NETPEERTCPBINDING
		NetPeerTcpBinding^ sevBinding = gcnew NetPeerTcpBinding();
		sevBinding->Security->Mode = System::ServiceModel::SecurityMode::None;
		host->AddServiceEndpoint(IService1::typeid, sevBinding, L"net.p2p://localhost:6000/service");
#endif


		host->Open();

		/* Connect server host*/
#if WSHTTPBINDING
		WSHttpBinding^ binding = gcnew WSHttpBinding();
		binding->MaxBufferPoolSize = 2147483647;
		binding->MaxReceivedMessageSize = 2147483647; 
		binding->Security->Mode = System::ServiceModel::SecurityMode::None;
		EndpointAddress^ address = gcnew EndpointAddress(String::Format(L"http://localhost:8000/service", Environment::MachineName));
#if WCF_SCOPE
		WSHttpBinding^ Runnerbinding = gcnew WSHttpBinding();
		Runnerbinding->MaxBufferPoolSize = 2147483647;
		Runnerbinding->MaxReceivedMessageSize = 2147483647; 
		Runnerbinding->Security->Mode = System::ServiceModel::SecurityMode::None;
		EndpointAddress^ Runneraddress = gcnew EndpointAddress(String::Format(L"http://localhost:8000/service", Environment::MachineName));
#endif
#endif

#if NETTCPBINDING
		NetTcpBinding^ binding = gcnew NetTcpBinding();
		binding->MaxBufferPoolSize = 2147483647;
		binding->MaxReceivedMessageSize = 2147483647; 
#ifdef _TCP_DIST
		binding->Security->Mode = System::ServiceModel::SecurityMode::None;
#endif
		binding->Security->Mode = System::ServiceModel::SecurityMode::None;
		EndpointAddress^ address = gcnew EndpointAddress(String::Format(L"net.tcp://localhost:6000/service", Environment::MachineName)); //This one works with net.tcp://eteditor.cloudapp.net:6000/service in APIClient
		binding->OpenTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
		binding->CloseTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
		binding->SendTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
		binding->ReceiveTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));

#if WCF_SCOPE
		NetTcpBinding^ Runnerbinding = gcnew NetTcpBinding();
		Runnerbinding->MaxBufferPoolSize = 2147483647;
		Runnerbinding->MaxReceivedMessageSize = 2147483647; 
#ifdef _TCP_DIST
		Runnerbinding->Security->Mode = System::ServiceModel::SecurityMode::None;
#endif
		EndpointAddress^ Runneraddress = gcnew EndpointAddress(String::Format(L"net.tcp://localhost:6000/service", Environment::MachineName));
#endif
#endif

#if UDPBINDING
		UdpBinding^ binding = gcnew UdpBinding();
		binding->MaxBufferPoolSize = 2147483647;
		binding->MaxReceivedMessageSize = 2147483647;
		EndpointAddress^ address = gcnew EndpointAddress(String::Format(L"soap.udp://localhost:8080/service", Environment::MachineName));
		binding->OpenTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
		binding->CloseTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
		binding->SendTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
		binding->ReceiveTimeout = *TimeOutSetting;//*(new TimeSpan(1, 0, 0));
		binding->TimeToLive = 255;
#endif

#if 0//NETPEERTCPBINDING
		NetPeerTcpBinding^ binding = gcnew NetPeerTcpBinding();
		binding->Security->Mode = System::ServiceModel::SecurityMode::None;
		EndpointAddress^ address = gcnew EndpointAddress(String::Format(L"net.p2p://localhost:6000/service", Environment::MachineName));
#endif

		ChannelFactory<IService1^>^ factory = gcnew ChannelFactory<IService1^>(binding, address);
		proxy = factory->CreateChannel();	

		std::cout << "WCF service is running..." << std::endl;
		wcfServer = 1;

		if (use_controller)
		{
			proxy->SetControllerDataState(1); //used only to tell APIClient to use Controller
		}
	}


	
	SimulateRun:
	try
	{
		//continue initialize connector etFOMM function interface
		if (etFommIF.Init() !=0) 
		{
			//fail, return to caller or stop here 				
			cout << "Error2: " << etFommIF.err_msg<< endl;
		};

#if 1//_EDITOR_INPUT
		std::ifstream seedfile(rnsFile.c_str());

		int nrand, seed1, seed2, seed3;
		seedfile >> nrand;
		
		if(NumberRuns > 1)
		{
			if(!etFommIF.SetRunInputs())
			{
				std::cout << std::endl << "The SET_RUN_INPUTS function was not found." << std::endl;
				return 1;
			}
			if(NumberRuns > nrand)
			{
				std::cout << std::endl << "The requested number of runs exceeds the number of random number seeds (" << nrand << ")." << std::endl;
				return 1;
			}
		}
#endif
		

		for(int nrun = 1; nrun <= NumberRuns; nrun++)
		{
			if (APIFlag == 1)
			{
				proxy->SetServerTimestep(0);
				PauseFlag = APIProcessClientRequest(proxy, &etFommIF, dataPath, fileName1, updatedFlag, prefix);
				if (proxy->GetClientState() == UseHostDefineNetwork)
				{
					HostFlag = 1;
					proxy->SetClientState(Started);
				}
				else if (proxy->GetClientState() == UseClientDefineNetwork)
				{
					ClientFlag = 1;
					proxy->SetClientState(Started);
				}
				else if (proxy->GetClientState() == UseTRFFile)
				{
					TRFInputFlag = 1;
					String ^ServerTRFFile = proxy->GetServerTRFFile();
					std::string tmps;
					ConvertString(ServerTRFFile, tmps);
					if (!tmps.empty()) 
						trfFILE = tmps;
					proxy->SetClientState(Started);
				}
				
				WriteTextFlag = proxy->GetServerWriteTextFlag();
			}

			status = etFommIF.SetInputs(trfFILE, TSDFlag, TIDFlag, OutFlag, CSVFlag, WriteTextFlag, nrun);

#if TRF_NETWORK_INPUT_FLAG
	
			status = etFommIF.StartUP();
			//put the startup() function in different place may affect the TXT result
			//the result of run1 and run2 may be the same, run1 and run2 may use the same random number seeds.
#endif
			
			std::cout << "Starting run " << nrun << std::endl;

#if 1//_EDITOR_INPUT
			seedfile >> seed1 >> seed2 >> seed3;
			std::cout << "Random number seeds used: " << seed1 << ", " << seed2 << ", " << seed3 << std::endl << std::endl;
#endif

#if HARD_CODED_NETWORK_INPUT_FLAG
			if (APIFlag != 1)
			{
				HostFlag = 1;
				TRFInputFlag = 1;
			}

#pragma region _EDITOR_INPUT
		if (TRFInputFlag == 0)
		{
//////////////////////////////////////////////////////////////////
//Network_Inputs start
			NETWORK_INPUTS Network_Inputs;
			if (HostFlag == 1)
			{
				Network_Inputs = Host_Default_Network_define_Network_INPUTS();
			}
			else if (ClientFlag == 1)
			{
				array <WCF_NETWORK_INPUTS> ^wcf_network_inputs = proxy->GetServerNetworkInput();
				Network_Inputs = WCF_to_HOST_network_input_data(wcf_network_inputs);
			}
			else
			{
				std::cout<<"set HostFlag/ClientFlag"<<std::endl;
			}
			status = etFommIF.SetNetworkInputs(Network_Inputs);
//Network_Inputs end
//////////////////////////////////////////////////////////////////
//Freeway_Network_Inputs start
			FREEWAY_NETWORK_INPUTS Freeway_Network_Inputs; 
			
			if (HostFlag == 1)
			{
				Freeway_Network_Inputs = Host_Default_Network_define_FREEWAY_NETWORK_INPUTS();
			}
			else if (ClientFlag == 1)
			{
				array <WCF_FREEWAY_NETWORK_INPUTS> ^ wcf_freeway_network_inputs = proxy->GetServerFreewayNetworkInput();
				Freeway_Network_Inputs = WCF_to_HOST_freeway_network_input_data(wcf_freeway_network_inputs);
			}
			else
			{
				std::cout<<"set HostFlag/ClientFlag"<<std::endl;
			}
			//Send the freeway network parameters to etfomm
			etFommIF.SetFreewayNetworkInputs(Freeway_Network_Inputs);
			
//Freeway_Network_Inputs end
//////////////////////////////////////////////////////////////////
//Street_Network_Inputs start
			STREET_NETWORK_INPUTS Street_Network_Inputs; 
			
			if (HostFlag == 1)
			{
				Street_Network_Inputs = Host_Default_Network_define_STREET_NETWORK_INPUTS();
			}
			else if (ClientFlag == 1)
			{
				array <WCF_STREET_NETWORK_INPUTS> ^ wcf_street_network_inputs = proxy->GetServerStreetNetworkInput();
				Street_Network_Inputs = WCF_to_HOST_street_network_input_data(wcf_street_network_inputs);
			}
			else
			{
				std::cout<<"set HostFlag/ClientFlag"<<std::endl;
			}
			//Send the street network parameters to etfomm
			try
			{
			    etFommIF.SetStreetNetworkInputs(Street_Network_Inputs);
			} catch (InterfaceException &e)
			{
				std::cout<< std::string(e.what()) << std::endl;
			}
			
//Street_Network_Inputs end
//////////////////////////////////////////////////////////////////
//Vehicle_Type_Inputs start
			
			int n_vehicle_types = 0;
			VEHICLE_TYPE_DATA* Vehicle_Type_Inputs;
			
			if (HostFlag == 1)
			{
				n_vehicle_types = 9;
				Vehicle_Type_Inputs = (VEHICLE_TYPE_DATA*)calloc(n_vehicle_types, sizeof(VEHICLE_TYPE_DATA));
				Host_Default_Network_define_Vehicle_Type_Inputs(Vehicle_Type_Inputs);
			}
			else if (ClientFlag == 1)
			{
				array <WCF_VEHICLE_TYPE_DATA>^ wcf_vdi = proxy->GetServerVehicleTypeInputs();
				n_vehicle_types = wcf_vdi->Length;
			    Vehicle_Type_Inputs = (VEHICLE_TYPE_DATA*)calloc(n_vehicle_types, sizeof(VEHICLE_TYPE_DATA));
				WCF_to_HOST_Vehicle_Type_Inputs(Vehicle_Type_Inputs, wcf_vdi);
			}
			try
			{
				etFommIF.SetVehicleTypes(Vehicle_Type_Inputs);
			}  catch (InterfaceException &e)
			{
				std::cout<< std::string(e.what()) << std::endl;
			}
			
//Vehicle_Type_Inputs end
//////////////////////////////////////////////////////////////////
//Freeway Links start
			int n_freeway_links = 0;
			FREEWAY_LINK* freeway_link_data;
			if (HostFlag == 1)
			{
				n_freeway_links = 10;
				freeway_link_data = (FREEWAY_LINK*)calloc(n_freeway_links, sizeof(FREEWAY_LINK));
				Host_Default_Network_define_freeway_link_data(freeway_link_data);
			}
			else if (ClientFlag == 1)
			{
				array <Wcf_freeway_link> ^ wcf_fwl = proxy->GetServerFreewayData();
				n_freeway_links = wcf_fwl->Length;
				//Define the links
				freeway_link_data = (FREEWAY_LINK*)calloc(n_freeway_links, sizeof(FREEWAY_LINK));
				WCF_to_HOST_freeway_link_data(freeway_link_data, wcf_fwl);
			}
			
			etFommIF.SetNumberOfFreewayLinks(n_freeway_links);
			status = etFommIF.SetFreewayLinks(freeway_link_data);
//Freeway Links end
//////////////////////////////////////////////////////////////////
//freeway detector_data start
			int n_fdet;
			DETECTOR_INPUTS *fdet_inputs;
			if (HostFlag == 1)
			{
				n_fdet = 1;
				fdet_inputs = (DETECTOR_INPUTS*)calloc(n_fdet, sizeof(DETECTOR_INPUTS));
				Host_Default_Network_define_fdet_inputs(fdet_inputs);
			}
			else if (ClientFlag == 1)
			{
				array<WCF_DETECTOR_INPUTS>^ wcf_det_inputs = proxy->GetServerFreewayDetectorInputs();
				n_fdet = wcf_det_inputs->Length;
				fdet_inputs = (DETECTOR_INPUTS*)calloc(n_fdet, sizeof(DETECTOR_INPUTS));
				WCF_to_HOST_det_inputs(fdet_inputs, wcf_det_inputs);
			}
			etFommIF.SetNumberOfFreewayDetectors(n_fdet);
			etFommIF.SetFDetectors(fdet_inputs);
//freeway detector_data end
//////////////////////////////////////////////////////////////////
//street links start				
			int n_street_links = 0;
			STREET_LINK* street_link_data;
			if (HostFlag == 1) 
			{
				n_street_links = 22;
				//Define the street links
				street_link_data = (STREET_LINK*)calloc(n_street_links, sizeof(STREET_LINK));
				Host_Default_Network_define_street_link_data(street_link_data);
			}
			else if (ClientFlag == 1)
			{
				//get initial street link data from client
				array <Wcf_street_link> ^ wcf_sl = proxy->GetServerStreetData();
				n_street_links = wcf_sl->Length;
				//Define the links
				street_link_data = (STREET_LINK*)calloc(n_street_links, sizeof(STREET_LINK));
				WCF_to_HOST_street_link_data(street_link_data, wcf_sl);
			}
			
			etFommIF.SetNumberOfStreetLinks(n_street_links);
			status = etFommIF.SetStreetLinks(street_link_data);
//street links start end	
//////////////////////////////////////////////////////////////////
//cond_turnpct_data start
			int n_turnpct;
			COND_TURNPCTS* cond_turnpct_data;
			if (HostFlag == 1)
			{
				n_turnpct = 1;
				cond_turnpct_data = (COND_TURNPCTS*)calloc(n_turnpct, sizeof(COND_TURNPCTS));
				Host_Default_Network_define_cond_turnpct_data(cond_turnpct_data);
			}
			else if (ClientFlag == 1)
			{
				array<WCF_COND_TURNPCTS>^ wcf_cond_turnpct_data = proxy->GetServerCondTurnpctData();
				n_turnpct = wcf_cond_turnpct_data->Length;
				cond_turnpct_data = (COND_TURNPCTS*)calloc(n_turnpct, sizeof(COND_TURNPCTS));
				WCF_to_HOST_cond_turnpct_data(cond_turnpct_data, wcf_cond_turnpct_data);
			}
			status = etFommIF.SetConditionalTurnpcts(cond_turnpct_data);
//cond_turnpct_data end
//////////////////////////////////////////////////////////////////
//entrynode_inputs start
			int n_entrynodes = 0;
			ENTRYNODES_DATA *entrynode_inputs;
			if (HostFlag == 1)
			{
				n_entrynodes = 9;
				//Define the entry nodes
				entrynode_inputs = (ENTRYNODES_DATA*)calloc(n_entrynodes, sizeof(ENTRYNODES_DATA));
				Host_Default_Network_define_entrynode_inputs(entrynode_inputs);
			}
			else if (ClientFlag == 1)
			{
				//get initial entry node data from client
				array <WCF_ENTRYNODES_DATA> ^ wcf_entry_node = proxy->GetServerEntryNodeData();
				n_entrynodes = wcf_entry_node->Length;
				//Define the entry nodes
				entrynode_inputs = (ENTRYNODES_DATA*)calloc(n_entrynodes, sizeof(ENTRYNODES_DATA));
				WCF_to_HOST_entry_node_data(entrynode_inputs, wcf_entry_node);
			}
			int typedist = 0;
			int erlanga = 0;
			float minsep = 1.2;
			etFommIF.SetNumberOfEntryNodes(n_entrynodes);
			etFommIF.SetEntryNodes(typedist, erlanga, minsep, entrynode_inputs);
//entrynode_inputs end
//////////////////////////////////////////////////////////////////
//ftc_data_inputs start
			int n_ftcs;
			FTC_DATA *ftc_data_inputs;
			if (HostFlag == 1)
			{
				n_ftcs = 10;
				ftc_data_inputs = (FTC_DATA*)calloc(n_ftcs, sizeof(FTC_DATA));
				Host_Default_Network_define_ftc_data_inputs(ftc_data_inputs);
			}
			else if (ClientFlag == 1)
			{
				array<WCF_FTC_DATA>^ wcf_ftcs = proxy->GetServerFTCSignalData();
				n_ftcs = wcf_ftcs->Length;
				ftc_data_inputs = (FTC_DATA*)calloc(n_ftcs, sizeof(FTC_DATA));
				WCF_to_HOST_ftc_data_inputs(ftc_data_inputs, wcf_ftcs);
			}
			etFommIF.SetNumberOfFTCSignals(n_ftcs);
			etFommIF.SetFTCSignals(ftc_data_inputs);
//ftc_data_inputs end	
//////////////////////////////////////////////////////////////////
//rampmeter_inputs start
			int n_rampmeters;
			RM_DATA *rampmeter_inputs;
			if (HostFlag == 1)
			{
				n_rampmeters = 1;
				rampmeter_inputs = (RM_DATA*)calloc(n_rampmeters, sizeof(RM_DATA));
				Host_Default_Network_define_rampmeter_inputs(rampmeter_inputs);
			}
			else if (ClientFlag == 1)
			{
				array<WCF_RM_DATA>^ wcf_rampmeter_inputs = proxy->GetServerRampmeterInputs();
				n_rampmeters = wcf_rampmeter_inputs->Length;
				rampmeter_inputs = (RM_DATA*)calloc(n_rampmeters, sizeof(RM_DATA));
				WCF_to_HOST_rampmeter_inputs(rampmeter_inputs, wcf_rampmeter_inputs);
			}
			etFommIF.SetNumberOfRampMeters(n_rampmeters);
			etFommIF.SetRampMeters(rampmeter_inputs);
//rampmeter_inputs end
//////////////////////////////////////////////////////////////////
//actuated control signal start
			
//actuated control signal end		
//////////////////////////////////////////////////////////////////
//street detector_data start
			int n_sdet;
			DETECTOR_INPUTS *sdet_inputs;
			if (HostFlag == 1)
			{
				n_sdet = 5;
				sdet_inputs = (DETECTOR_INPUTS*)calloc(n_sdet, sizeof(DETECTOR_INPUTS));
				Host_Default_Network_define_sdet_inputs(sdet_inputs);
			}
			else if (ClientFlag == 1)
			{
				array<WCF_DETECTOR_INPUTS>^ wcf_sdet_inputs = proxy->GetServerStreetDetectorInputs();
				n_sdet = wcf_sdet_inputs->Length;
				sdet_inputs = (DETECTOR_INPUTS*)calloc(n_sdet, sizeof(DETECTOR_INPUTS));
				WCF_to_HOST_det_inputs(sdet_inputs, wcf_sdet_inputs);
			}
			etFommIF.SetNumberOfStreetDetectors(n_sdet);
			etFommIF.SetSDetectors(sdet_inputs);
//street detector_data end
//////////////////////////////////////////////////////////////////
//Bus Routes start
			int n_busroutes;
			BUSROUTE_DATA *busroute_inputs;
			if (HostFlag == 1)
			{
				n_busroutes = 1;
				busroute_inputs = (BUSROUTE_DATA*)calloc(n_busroutes, sizeof(BUSROUTE_DATA));
				Host_Default_Network_define_busroute_inputs(busroute_inputs);
			}
			else if (ClientFlag == 1)
			{
				array<WCF_BUSROUTE_DATA>^ wcf_busroute_inputs = proxy->GetServerBusRouteInputs();
				n_busroutes = wcf_busroute_inputs->Length;
				busroute_inputs = (BUSROUTE_DATA*)calloc(n_busroutes, sizeof(BUSROUTE_DATA));
				WCF_to_HOST_busroute_inputs(busroute_inputs, wcf_busroute_inputs);
			}
			etFommIF.SetNumberOfBusRoutes(n_busroutes);
			etFommIF.SetBusRoutes(busroute_inputs);
//Bus Routes end
//////////////////////////////////////////////////////////////////
//Bus Stations start
			int n_busstations = 0;
			BUSSTATION_DATA *busstation_inputs;
			if (HostFlag == 1)
			{
				n_busstations = 99;
				busstation_inputs = (BUSSTATION_DATA*)calloc(n_busstations, sizeof(BUSSTATION_DATA));
				Host_Default_Network_define_busstation_inputs(busstation_inputs);
			}
			else if (ClientFlag == 1)
			{
				array<WCF_BUSSTATION_DATA>^ wcf_busstation_inputs = proxy->GetServerBusStationInputs();
				n_busstations = 99;
				busstation_inputs = (BUSSTATION_DATA*)calloc(n_busstations, sizeof(BUSSTATION_DATA));
				WCF_to_HOST_busstation_inputs(busstation_inputs, wcf_busstation_inputs);
			}
			etFommIF.SetBusStations(busstation_inputs);
//Bus Stations end		
//////////////////////////////////////////////////////////////////
//incident data start
			int n_incidents = 0;
			INCIDENT_DATA* incident_data_inputs;
			if (HostFlag == 1)
			{
				n_incidents = 1;
				incident_data_inputs = (INCIDENT_DATA*)calloc(n_incidents, sizeof(INCIDENT_DATA));
				Host_Default_Network_define_incident_data_inputs(incident_data_inputs);
			}
			else if (ClientFlag == 1)
			{
				array<WCF_INCIDENT_DATA>^ wcf_incident_data_inputs = proxy->GetServerIncidentData_Inputs();
				n_incidents = wcf_incident_data_inputs->Length;
				incident_data_inputs = (INCIDENT_DATA*)calloc(n_incidents, sizeof(INCIDENT_DATA));
				WCF_to_Host_incident_data_inputs(incident_data_inputs, wcf_incident_data_inputs);
			}
			etFommIF.SetNumberOfIncidents(n_incidents);
			etFommIF.SetIncidents(incident_data_inputs);
//incident data end
//////////////////////////////////////////////////////////////////
//Node Coordinates start
			NODE_LOCATION_DATA *xy_coord_inputs;
			if (HostFlag == 1 )
			{
				xy_coord_inputs = (NODE_LOCATION_DATA*)calloc(8999, sizeof(NODE_LOCATION_DATA));
				Host_Default_Network_define_xy_coord_inputs(xy_coord_inputs);
			}
			else if (ClientFlag == 1 )
			{
				array<WCF_NODE_LOCATION_DATA>^ wcf_xy_coord_inputs = proxy->GetServerXYCoordInputs();
				xy_coord_inputs = (NODE_LOCATION_DATA*)calloc(8999, sizeof(NODE_LOCATION_DATA));
				WCF_to_HOST_xy_coord_inputs(xy_coord_inputs, wcf_xy_coord_inputs);
			}
			etFommIF.SetNodeCoordinates(xy_coord_inputs);
//Node Coordinates end
//////////////////////////////////////////////////////////////////	
//////////////////////////////////////////////////////////////////
//Node Coordinates start
			int n_parking_zones = 0;
			PARKING_DATA *parking_zone_inputs;
			if (HostFlag == 1 )
			{
				n_parking_zones = 1;
				parking_zone_inputs = (PARKING_DATA*)calloc(n_parking_zones, sizeof(PARKING_DATA));
				Host_Default_Network_define_parking_zone_inputs(parking_zone_inputs);
			}
			else if (ClientFlag == 1 )
			{
				array<WCF_PARKING_DATA>^ wcf_parking_zone_inputs = proxy->GetServerParkingData();
				n_parking_zones = wcf_parking_zone_inputs->Length;
				parking_zone_inputs = (PARKING_DATA*)calloc(n_parking_zones, sizeof(PARKING_DATA));
				WCF_to_Host_parking_data_inputs(parking_zone_inputs, wcf_parking_zone_inputs);
			}
			etFommIF.SetNumberOfParkingZones(n_parking_zones);
			etFommIF.SetParkingZones(parking_zone_inputs);
//Node Coordinates end
//////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////
//Node Coordinates start
			int n_events = 0;
			EVENT_DATA *event_inputs;
			if (HostFlag == 1 )
			{
				n_events = 1;
				event_inputs = (EVENT_DATA*)calloc(n_events, sizeof(EVENT_DATA));
				Host_Default_Network_define_event_inputs(event_inputs);
			}
			else if (ClientFlag == 1 )
			{
				array<WCF_EVENT_DATA>^ wcf_event_inputs = proxy->GetServerEventData();
				n_events = wcf_event_inputs->Length;
				event_inputs = (EVENT_DATA*)calloc(n_events, sizeof(EVENT_DATA));
				WCF_to_Host_event_data_inputs(event_inputs, wcf_event_inputs);
			}
			etFommIF.SetNumberOfEvents(n_events);
			etFommIF.SetEvents(event_inputs);
//Node Coordinates end
//////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////
//Node Coordinates start
			int n_diversions = 0;
			DIVERSION_DATA *diversion_inputs;
			if (HostFlag == 1 )
			{
				n_diversions = 1;
				diversion_inputs = (DIVERSION_DATA*)calloc(n_diversions, sizeof(DIVERSION_DATA));
				Host_Default_Network_define_diversion_inputs(diversion_inputs);
			}
			else if (ClientFlag == 1 )
			{
				array<WCF_DIVERSION_DATA>^ wcf_diversion_inputs = proxy->GetServerDiversionData();
				n_diversions = wcf_diversion_inputs->Length;
				diversion_inputs = (DIVERSION_DATA*)calloc(n_diversions, sizeof(DIVERSION_DATA));
				WCF_to_Host_diversion_data_inputs(diversion_inputs, wcf_diversion_inputs);
			}
			etFommIF.SetNumberOfDiversions(n_diversions);
			etFommIF.SetDiversions(diversion_inputs);
//Node Coordinates end
//////////////////////////////////////////////////////////////////
#if PRINTOUT
			tmpName = dataPath + fileName1 + ".dat";
			std::ofstream outputFile(tmpName.c_str());
			
			etFommIF.PrintFreewayLinkData(outputFile, n_freeway_links, freeway_link_data);
			etFommIF.PrintStreetLinkData(outputFile, n_street_links, street_link_data);
			etFommIF.PrintFTCSignals(outputFile, n_ftcs, ftc_data_inputs);
			etFommIF.PrintEntryNodes(outputFile, n_entrynodes, entrynode_inputs);
			etFommIF.PrintRampMeter(outputFile, n_rampmeters, rampmeter_inputs);

			etFommIF.PrintNetworkInputs(outputFile, Network_Inputs);
			etFommIF.PrintFNetworkInputs(outputFile, Freeway_Network_Inputs);
			etFommIF.PrintSNetworkInputs(outputFile, Street_Network_Inputs);
			etFommIF.PrintVTypeInputs(outputFile, n_vehicle_types, Vehicle_Type_Inputs);
			etFommIF.PrintDetectorInputs(outputFile, "FREEWAY", n_fdet, fdet_inputs);
			etFommIF.PrintDetectorInputs(outputFile, "STREET", n_sdet, sdet_inputs);
			etFommIF.PrintCondTurnpctInputs(outputFile, n_turnpct, cond_turnpct_data);
			etFommIF.PrintBusRouteInputs(outputFile, n_busroutes, busroute_inputs);
			etFommIF.PrintBusStationInputs(outputFile, n_busstations, busstation_inputs);
			etFommIF.PrintIncidentInputs(outputFile, n_incidents, incident_data_inputs);
			etFommIF.PrintCoordInputs(outputFile, 8999, xy_coord_inputs);

			etFommIF.PrintParkingZones(outputFile, n_parking_zones, parking_zone_inputs);
			etFommIF.PrintEvents(outputFile, n_events, event_inputs);
			etFommIF.PrintDiversions(outputFile, n_diversions, diversion_inputs);

			outputFile.close();
#endif
		}
#pragma endregion
		    bool APIstartup = (TRFInputFlag == 0);
			status = etFommIF.StartUP(APIstartup);
			etFommIF.ImportIntersectionModel();
#if 0//_EDITOR_INPUT
			status = etFommIF.SetRunInputs(nrun, seed1, seed2, seed3);
#endif
			if (status !=0 )
			{
				std::cout << "Input error, please check input\nPress any key to exit..." << std::endl;
				std::cin.get();
				return 1;
			}
			
#if DISPLAY_DEFAULT_NETWORK_FLAG
			DisplayFreewayLinks(&etFommIF);
			DisplayStreetLinks(&etFommIF);
			DisplayEntryNodes(&etFommIF);
			DisplayFTCSignals(&etFommIF);
			DisplayRampMeters(&etFommIF);
			DisplayBusRoutes(&etFommIF);
			DisplayBusStations(&etFommIF);
#endif
			
#endif

#if _EDITOR_INPUT
			if (WriteTRFFlag == 1)
			{
				etFommIF.WriteTRFFile();
			}
			
#endif			
			if(status == 0) 
				etFommIF.RunInitialize();
		
			float timestep;
			bool change_phase_flag = false;
			float phase_timer[10] = {0};
			float simulation_count = 0, APIfrequency = 0;
			float SimulationTimeStep = etFommIF.GetTimeStep();
			
			float currentAPIfrequency = 0;
			if (APIFlag == 1)
			{
				currentAPIfrequency = proxy->GetAPITimestepInterval();
			}

			
			int LASTPERIOD = 0;
			float cpu_time = 0;
			float etime = 0;
			float CURRENT_SIMTIME = 0;
			
			int structsize;

			std::ofstream outputFile2;
			int CURRENTPERIOD = 0;


#if WCF_SCOPE 
			
			int n_street_links = etFommIF.GetNumberOfStreetLinks();
			int n_sdet = etFommIF.GetNumberOfStreetDetectors();
			int n_acs = etFommIF.GetNumberOfACSignals();
			array<WCF_DETECTOR_OUTPUTS>^ wcf_sdet_outputs = gcnew array<WCF_DETECTOR_OUTPUTS>(n_sdet);
			array<WCF_RUNNER_DATA>^ wcf_runner_data = gcnew array<WCF_RUNNER_DATA>(n_acs);
			std::cout << "Waiting Detector Runner... \nPlease start DETECTORRunner.exe\n" << std::endl;
			proxy->SetRunnerState(WaitToStart);

			int use_ntcip = etFommIF.GetUseNtcipValue();
			int use_dcs = etFommIF.GetUseDcsValue();
			int extension = 0;
			int forceoff = 0;
			int green_phases = 0;
			int yellow_phases = 0;
			double curTimeStep = 0.0;
			int dcs_error = 0;
			char dat_file[512];
			FILENAME DATAFILENAME = etFommIF.GetDataFileName();
			strcpy_s(dat_file, DATAFILENAME.str);
			System::String ^InputFileName = gcnew System::String(dat_file);
			for (int i = 0; i < n_acs; i++)
			{
				wcf_runner_data[i].initialization_dat_file_path = InputFileName;
				wcf_runner_data[i].n_street_links = n_street_links;
				wcf_runner_data[i].simulation_time_step = SimulationTimeStep;
				wcf_runner_data[i].use_ntcip = use_ntcip;
				wcf_runner_data[i].use_dcs = use_dcs;
				wcf_runner_data[i].n_sdet = n_sdet;
				wcf_runner_data[i].curTimeStep = curTimeStep;
				wcf_runner_data[i].extension = extension;
				wcf_runner_data[i].forceoff = forceoff;
				wcf_runner_data[i].dcs_error = dcs_error;
				wcf_runner_data[i].n_acs = n_acs;
				wcf_runner_data[i].green_phases = green_phases;
				wcf_runner_data[i].yellow_phases = yellow_phases;
			}
			proxy->SetServerRunnerData(wcf_runner_data);
			
			bool isSCOPEstarted = false;
			bool isDCSstarted = false;
			bool isDETECTORstarted = false;
			if (!use_dcs)
			{
				isDCSstarted = true;
			}
			while (1)
			{
				if (proxy->GetRunnerState() == SCOPERunnerIsStarted)
				{
					isSCOPEstarted = true;
				}
				if (proxy->GetRunnerState() == DCSRunnerIsStarted)
				{
					isDCSstarted = true;
				}
				if (proxy->GetRunnerState() == DETECTORRunnerIsStarted)
				{
					isDETECTORstarted = true;
				}
			
				if (isSCOPEstarted == true && isDCSstarted == true && isDETECTORstarted == true)
				{
					break;
				}
			}

			DETECTOR_OUTPUTS *sdet_outputs;
			sdet_outputs = (DETECTOR_OUTPUTS*)calloc(n_sdet, sizeof(DETECTOR_OUTPUTS));
			int n_timesteps = 0;
#endif
#if _UseController
			LARGE_INTEGER litmp,litmp2;
			LONGLONG QStart,QStop, QStart2, QStop2;
			double dfMinus, dfFreq, dfFreq2, dfTim;
			QueryPerformanceFrequency(&litmp);
			QueryPerformanceFrequency(&litmp2);
			// Counter Frequency
			dfFreq = (double)litmp.QuadPart;
			dfFreq2 = (double)litmp2.QuadPart;
			// Starting Time Stamp

#if 1//ContinueAfter100ms
			SYSTEMTIME st_start, st_end;
			double step_diff_accumulate = 0.0;
			double wait_until_N_sec = 0.0;
			GetSystemTime(&st_start);
			GetSystemTime(&st_end);

			//std::cout << st_end.wMilliseconds << " - " << st_start.wMilliseconds << std::endl;
#endif

			double MaxControllerResponseTime = 0.0;
			double MinControllerResponseTime = 999.9;

			char TimeLogFileName[] = "DebugTimeLog.txt";
			ofstream TimeLogFile;
			TimeLogFile.open(TimeLogFileName);
			char ACLogFileName[] = "DebugACLog.txt";
			ofstream ACLogFile;
			ACLogFile.open(ACLogFileName);
			ACLogFile << "time,node,greens,yellows,local_cycle_timer,node,greens,yellows,local_cycle_timer,node,greens,yellows,local_cycle_timer,node,greens,yellows,local_cycle_timer,node,greens,yellows,local_cycle_timer,(375/450 6/8/10/14/16)" << std::endl;
			SYSTEMTIME st;
			//GetSystemTime(&st);
			//TimeLogFile << "Simulation Step Time:" << CURRENT_SIMTIME << ", iteration executed at real-time : " 
			//	<< System::DateTime::Now.Hour.ToString() << ":"
			//	<< System::DateTime::Now.Minute.ToString() << ":"
			//	<< System::DateTime::Now.Second.ToString() << "."
			//	<< System::DateTime::Now.Millisecond.ToString() << "\n";

			int n_acs = etFommIF.GetNumberOfACSignals();
			AC_INPUTS* AC_Data_Inputs = (AC_INPUTS*)calloc(n_acs, sizeof(AC_INPUTS));
			etFommIF.GetACSignals(AC_Data_Inputs);
			int n_controllers = 0;
			int iact;
			bool is_wcf_controllers_all_ready = false;

			if (use_controller)
			{
				std::cout << "\n\n############################" << std::endl;
				std::cout << "Using 2070 Controller, AC list in the simulation:" << std::endl;
				for (int i = 1; i <= n_acs; i++)
				{
					etFommIF.GetControllerID(AC_Data_Inputs[i-1].node, &iact);
					std::cout << "AC Node " << AC_Data_Inputs[i-1].node << " has Controller ID number: " << iact <<std::endl;
				}

				std::cout << "\n\n############################" << std::endl;
				std::cout << "Please enter the total number of 2070 controllers that will be used for AC (this_controller_iact number in ETFOMMInterface.exe):" << std::endl;
				while (std::cin >> n_controllers)
				{
					if (n_controllers < 1)
					{
						std::cout << "Entered number: " << n_controllers << " is smaller than 1 \nPlease enter again..." << std::endl;
					}
					else if (n_controllers > n_acs)
					{
						std::cout << "Entered number: " << n_controllers << " is larger than the total number of AC node ( " << n_acs << " )\nPlease enter again..." << std::endl;
					}
					else
					{

						std::cout << "Number of ACs using 2070 controllers: " << n_controllers << std::endl;
						break;
					}
				}
			}
			
			int* controller_index_in_use = new int[n_controllers](); //contains the iact that replaced by controller
			int* phase_calls = new int[n_controllers]();
			int* green_phases = new int[n_controllers]();
			int* yellow_phases = new int[n_controllers]();
			int green_phases_temp = 0;
			int yellow_phases_temp = 0;


			array<WCF_CONTROLLER_DATA>^ wcf_controller_data = gcnew array<WCF_CONTROLLER_DATA>(n_controllers);
			int buff_check_controller_ready = 0;

			if (use_controller)
			{
				proxy->ResizeControllerData(n_controllers);

				std::cout << "\n\n############################" << std::endl;
				std::cout << "Please enter each Controller's ID number that will be used for AC:" << std::endl;
				for (int i = 0; i < n_controllers; i++)
				{
					cin>>iact;
					if ( (iact > n_acs) || (iact < 1) )
					{
						for (int j = 1; j <= n_acs; j++)
						{
							etFommIF.GetControllerID(AC_Data_Inputs[j-1].node, &iact);
							std::cout << "AC Node " << AC_Data_Inputs[j-1].node << " has Controller ID number: " << iact <<std::endl;
						}
						std::cout << "You entered an invalid Controller ID number, please enter again" << std::endl;
						i--;
						//std::cout<<"i1="<<i<<std::endl;
					}
					else
					{
						controller_index_in_use[i] = iact;
						wcf_controller_data[i].controller_iact = controller_index_in_use[i];
						//std::cout<<"i2="<<i<<std::endl;
					}
					//std::cout<<"i3="<<i<<std::endl;
				}
				proxy->SetControllerData(wcf_controller_data);
				ProcessHITLSACData(proxy, &etFommIF);

				PauseFlag = 0;
				std::cout << "\n\n############################" << std::endl;
				std::cout << "etRunner is ready, please start NTCIP Interface exe to continue..." <<std::endl;

				std::cout << "Please press the \"Etrunner Interface WCF\" button on all the opened Interface EXEs\nand enter any char to start" << std::endl;
				char start;
				std::cin >> start;
				//proxy->SetControllerDataState(1);
			}

			
			QueryPerformanceCounter(&litmp2);
			QStart2 = litmp2.QuadPart;
			QueryPerformanceCounter(&litmp);
			QStop = litmp.QuadPart;
#endif
			while(status == 0)
			{
#if _UseController
				QStart = QStop;
#endif
#if _UseController//ContinueAfter100ms
				GetSystemTime(&st_start);
#endif

#if !WCF_SCOPE
				status = etFommIF.ETFOMM_SIMULATE();

#endif
#if _UseController
				TimeLogFile << std::endl << "st_start call SIMULATE() @" << st_start.wMinute << "." << st_start.wSecond << ":" << st_start.wMilliseconds << std::endl;
#endif
				CURRENT_SIMTIME = etFommIF.GetSimtime();
#if _UseController
				//std::cout << "Current Simulation Time = " << CURRENT_SIMTIME << std::endl;
				ACLogFile << CURRENT_SIMTIME;
#endif
#if !UDPBINDING && !NETTCPBINDING
				if (APIFlag == 1) //Added to the end of the while according to Lei's comment
				{
					proxy->SetServerTimestep(CURRENT_SIMTIME);
				}
#endif

#if !WCF_SCOPE
				
#if ( !defined(UnifiedDLL) || defined(_NamedPipe) ) 
				/*status =*/ etFommIF.StepSimulate();
#else

#if _UseController
#pragma region _test_continue_simulation_use_phase_states_from_last_step
#if 0
				if (use_controller /*&& iact == i_act*/)
				{
					for (int i = 0; i < n_controllers; i++)
					{
						etFommIF.GetPhaseCalls(controller_index_in_use[i], &phase_calls[i]);
						wcf_controller_data[i].phase_calls = phase_calls[i];
						wcf_controller_data[i].is_etrunner_ready = 1;
					}

					//Set phase_calls to WCF
					proxy->SetControllerData(wcf_controller_data);

					wcf_controller_data = proxy->GetControllerData();
					for (int i = 0; i < n_controllers; i++)
					{
						green_phases_temp = wcf_controller_data[i].green_phases;
						yellow_phases_temp = wcf_controller_data[i].yellow_phases;
						etFommIF.SetETFOMMPhaseStates(controller_index_in_use[i], &green_phases_temp, &yellow_phases_temp);

						TimeLogFile << "Ctrlr#" << i <<", phase_call: " << wcf_controller_data[i].phase_calls << ", G: " << green_phases_temp << ", Y: " << yellow_phases_temp << " ";
					}
					TimeLogFile << std::endl;


				}
#endif
#pragma endregion
#pragma region working_code_before_test_continue_simulation_use_phase_states_from_last_step
#if 1
				int temp_greens = 0;
				int temp_yellows = 0;
				if (use_controller /*&& iact == i_act*/)
				{
					for (int i = 0; i < n_controllers; i++)
					{
						etFommIF.GetPhaseCalls(controller_index_in_use[i], &phase_calls[i]);
						wcf_controller_data[i].phase_calls = phase_calls[i];
						wcf_controller_data[i].is_etrunner_ready = 1;
						//wcf_controller_data[i].current_simtime = CURRENT_SIMTIME;
					}

					GetSystemTime(&st_end);
					TimeLogFile << "call SetControllerData() @" << st_end.wMinute << "." << st_end.wSecond << ":" << st_end.wMilliseconds << std::endl;

					TimeLogFile << "SetControllerData()\t G/Y: " << wcf_controller_data[0].green_phases << "/" << wcf_controller_data[0].yellow_phases << std::endl;
					if (temp_greens != wcf_controller_data[0].green_phases || temp_yellows != wcf_controller_data[0].yellow_phases)
					{
						TimeLogFile << "different phase states 1" << std::endl;
					}
					//Set phase_calls to WCF
					proxy->SetControllerDataNoPhaseStates(wcf_controller_data);
					//Tell controllers that the phase_calls data is ready (is_etrunner_ready)

					//proxy->SetControllerDataState(1);

					buff_check_controller_ready = 0;
					is_wcf_controllers_all_ready = false;

					GetSystemTime(&st_end);
					TimeLogFile << "start while() @" << st_end.wMinute << "." << st_end.wSecond << ":" << st_end.wMilliseconds << std::endl;

					TimeLogFile << "During while()\t G/Y: ";
					//Wait controller to return all the greens/yellows
					while (!is_wcf_controllers_all_ready)
					{
						wcf_controller_data = proxy->GetControllerData();
						TimeLogFile << "\n" << wcf_controller_data[0].green_phases << "/" << wcf_controller_data[0].yellow_phases << " ";
						temp_greens = wcf_controller_data[0].green_phases;
						temp_yellows = wcf_controller_data[0].yellow_phases;
						//check if all controllers are ready by checking the is_controller_ready variable
						for (int i = 0; i < n_controllers; i++)
						{
							if (wcf_controller_data[i].is_controller_ready == 1)
							{
								buff_check_controller_ready++;
								//wcf_controller_data[i].is_controller_ready = 0;
								proxy->SetControllerReadyByIndex(0,i);
							}
							if (buff_check_controller_ready == n_controllers)
							{
								is_wcf_controllers_all_ready = true;
							}
						}
#if 1//ContinueAfter100ms
						GetSystemTime(&st_end);
						TimeLogFile << "\nst_end during while() @" << st_end.wMinute << "." << st_end.wSecond << ":" << st_end.wMilliseconds;

						//if ( (st_end.wMilliseconds - st_start.wMilliseconds) > (SimulationTimeStep * 1000) )
						if ( ( (st_end.wMinute - st_start.wMinute) * 1000 * 1000 + (st_end.wSecond - st_start.wSecond) * 1000 + st_end.wMilliseconds - st_start.wMilliseconds ) > (SimulationTimeStep * 1000) )
						{
							is_wcf_controllers_all_ready = true;
							TimeLogFile << "\nstop waiting... ";
						}
#endif
					}

					//Get greens/yellows from WCF and Set back to ETFOMM
					//wcf_controller_data = proxy->GetControllerData();
					GetSystemTime(&st_end);
					TimeLogFile << std::endl << "after while() @" << st_end.wMinute << "." << st_end.wSecond << ":" << st_end.wMilliseconds << std::endl;

					TimeLogFile << "removed After while()\t G/Y: " << wcf_controller_data[0].green_phases << "/" << wcf_controller_data[0].yellow_phases << std::endl;
					if (temp_greens != wcf_controller_data[0].green_phases || temp_yellows != wcf_controller_data[0].yellow_phases)
					{
						TimeLogFile << "different phase states 2" << std::endl;
					}
					temp_greens = wcf_controller_data[0].green_phases;
					temp_yellows = wcf_controller_data[0].yellow_phases;

					for (int i = 0; i < n_controllers; i++)
					{
						green_phases_temp = wcf_controller_data[i].green_phases;
						yellow_phases_temp = wcf_controller_data[i].yellow_phases;
						//yellow_phases_temp = 255;
						//green_phases_temp = 0;
						etFommIF.SetETFOMMPhaseStates(controller_index_in_use[i], &green_phases_temp, &yellow_phases_temp);
						
						TimeLogFile << "Ctrlr#" << i <<", phase_call: " << wcf_controller_data[i].phase_calls << ", G: " << green_phases_temp << ", Y: " << yellow_phases_temp << " ";
					}
					TimeLogFile << std::endl;
	
				}
				for (int iact = 1; iact <= n_acs; iact++)
				{
					etFommIF.GetETFOMMPhaseStates(iact, &green_phases_temp, &yellow_phases_temp);
					ACLogFile << "," << iact << "," << green_phases_temp << "," << yellow_phases_temp;
					ACLogFile << "," << "local_cycle_timer";
				}
				ACLogFile << std::endl;
#endif
#pragma endregion

#endif //#if _UseController
				if( (TSDFlag == 2 || WriteTextFlag == 1) && (status== 0 || status == 4) ) etFommIF.ETFOMM_WriteAnimationFiles();

#endif		
#endif
#if WCF_SCOPE
				
				curTimeStep = SimulationTimeStep * n_timesteps++;
				for (int i = 0; i < n_acs; i++)
				{
					wcf_runner_data[i].n_sdet = n_sdet;
					wcf_runner_data[i].curTimeStep = curTimeStep;
					wcf_runner_data[i].extension = extension;
					wcf_runner_data[i].forceoff = forceoff;
					wcf_runner_data[i].dcs_error = dcs_error;
					wcf_runner_data[i].n_acs = n_acs;
					wcf_runner_data[i].green_phases = green_phases;
					wcf_runner_data[i].yellow_phases = yellow_phases;
				}
				proxy->SetServerRunnerData(wcf_runner_data);
				proxy->SetRunnerState(STEP_STARTED);
				if (use_dcs)
				{
					while (proxy->GetRunnerState() != RUN_DCS_CALLED)
					{
					}
				}
				else
				{
					while (proxy->GetRunnerState() != UPDATE_SCOPE_DETECTORS_CALLED)
					{
					}
				}

				//Get the extension/forceoff back from WCF
				wcf_runner_data = proxy->GetServerRunnerData();
				
				for (int i = 0; i < n_acs; i++)
				{
					extension = wcf_runner_data[0].extension;
					forceoff = wcf_runner_data[0].forceoff;
					
				} 

				wcf_sdet_outputs = proxy->GetServerStreetDetectorOutputs();
				WCF_DETECTOR_OUTPUTS_to_DETECTOR_OUTPUTS(wcf_sdet_outputs,sdet_outputs);


				/////////////////////////////////////////////////
				for(int iact = 1; iact <= n_acs; iact++)
				{
					wcf_runner_data[0].curTimeStep = curTimeStep;
					wcf_runner_data[0].extension = extension;
					wcf_runner_data[0].forceoff = forceoff;
					wcf_runner_data[0].n_acs = n_acs;
					
				}
				proxy->SetServerRunnerData(wcf_runner_data);
				proxy->SetRunnerState(ExtForceoffIsUpdatedIfPreprocessSDET);
				while (proxy->GetRunnerState() != GET_SCOPE_PHASE_STATES_CALLED)
				{
				}
				//Get updated greens/yellows from WCF
				wcf_runner_data = proxy->GetServerRunnerData();
				for (int i = 0; i < n_acs; i++)
				{
					green_phases = wcf_runner_data[0].green_phases;
					yellow_phases = wcf_runner_data[0].yellow_phases;
					
				} 
				

				///////////////////////////////
				for(int iact = 1; iact <= n_acs; iact++)
				{
					etFommIF.SetETFOMMPhaseStates(iact, &green_phases, &yellow_phases);
				}

				proxy->SetRunnerState(SET_ETFOMM_PHASE_STATES_CALLED);
				if (use_dcs)
				{
					while (proxy->GetRunnerState() != SET_DCS_PHASE_STATES_CALLED)
					{
					}
				}
				else
				{
				}

				/////////////////////////////
				if (dcs_error)
				{
					status = dcs_error;
				}
				else
				{
					status = etFommIF.ETFOMM_SIMULATE();
				}
				/////////////////////////////////////
				if(n_acs > 0)
				{
					//Get detector data
					//currently getting detector data from ETFOMM for testing purposes
					//etFommIF.GET_STREET_DETECTOR_OUTPUTS(sdet_outputs); //from ETFOMM
					etFommIF->GetStreetDetectorOutputs(sdet_outputs); //from ETFOMM


					DETECTOR_OUTPUTS_to_WCF_DETECTOR_OUTPUTS(sdet_outputs, wcf_sdet_outputs);
					proxy->SetServerStreetDetectorOutputs(wcf_sdet_outputs);
					proxy->SetRunnerState(GET_STREET_DETECTOR_OUTPUTS_CALLED);
					while (proxy->GetRunnerState() != UPDATE_DETECTOR_DATA_CALLED)
					{
					}
				}

				//////////////////////////////////////
				//etFommIF.GenFreewayData(curTimeStep);
				//etFommIF.GenStreetData(curTimeStep);
				//////////////////////////////////////
#endif

#if !_UseController
				CURRENTPERIOD = etFommIF.GetCurrentPeriod();
				if (CURRENTPERIOD > LASTPERIOD && status != 4)
				{
					if (APIFlag == 1) proxy->SetServerTimePeriod(CURRENTPERIOD);
					LASTPERIOD = CURRENTPERIOD;
					
				}
#endif
				simulation_count += SimulationTimeStep;
				
				if ((APIFlag == 1) && (PauseFlag == 1) && (simulation_count >= currentAPIfrequency))
				{
					#if PRINTOUT
					if (updatedFlag == 0)
					{
						tmpName = dataPath + fileName2 + ".dat";
					} else {
						tmpName = dataPath + prefix + fileName2 + ".dat";
					}
										
					outputFile2.open(tmpName.c_str());
					#endif

					ProcessFVehicleData(outputFile2, proxy,&etFommIF, updatedFlag);
					ProcessSVehicleData(outputFile2, proxy,&etFommIF, updatedFlag);
					ProcessFreewayLinksData(outputFile2, proxy, &etFommIF, updatedFlag);
					ProcessStreetLinksData(outputFile2, proxy, &change_phase_flag, &etFommIF, updatedFlag);
					ProcessFTCSignals(outputFile2, proxy, &etFommIF, updatedFlag);
					ProcessEntryNodes(outputFile2, proxy, &etFommIF, updatedFlag);
					ProcessRampMeter(outputFile2, proxy, &etFommIF, updatedFlag);
					
					ProcessNetworkInputs(outputFile2, proxy, &etFommIF, updatedFlag);
					ProcessFNetworkInputs(outputFile2, proxy, &etFommIF, updatedFlag);
					ProcessSNetworkInputs(outputFile2, proxy, &etFommIF, updatedFlag);
					ProcessVTypeInputs(outputFile2, proxy, &etFommIF, updatedFlag);
					ProcessFDetectorOutputs(outputFile2, proxy, &etFommIF, updatedFlag);
					ProcessSDetectorOutputs(outputFile2, proxy, &etFommIF, updatedFlag);
					ProcessBusRouteInputs(outputFile2, proxy, &etFommIF, updatedFlag);
					ProcessIncidentInputs(outputFile2, proxy, &etFommIF, updatedFlag);
					ProcessCoordInputs(outputFile2, proxy, &etFommIF, updatedFlag);

					ProcessParkingZones(outputFile2, proxy, &etFommIF, updatedFlag);
					ProcessEvents(outputFile2, proxy, &etFommIF, updatedFlag);
					ProcessDiversionInputs(outputFile2, proxy, &etFommIF, updatedFlag);

					#if PRINTOUT
					outputFile2.close();
					#endif
#if !_USE_TCA
					proxy->SetClientState(DataReady);
#endif
					CURRENT_SIMTIME = etFommIF.GetSimtime();
					std::cout << "Current Simulation Time = " << CURRENT_SIMTIME << std::endl;
					if (APIFlag == 1)
					{
						proxy->SetServerTimestep(CURRENT_SIMTIME);
					}
					PauseFlag = APIProcessClientRequest(proxy, &etFommIF, dataPath, fileName1, updatedFlag, prefix);
										
					APIfrequency = proxy->GetAPITimestepInterval();
										
					simulation_count = 0;
					currentAPIfrequency = APIfrequency;

				}
#if !UDPBINDING && !NETTCPBINDING
				if (APIFlag == 1)
				{
					currentAPIfrequency = proxy->GetAPITimestepInterval();
				}
#endif
#if _UseController

				GetSystemTime(&st_end);
				TimeLogFile << "calculate dfTim @" << st_end.wMinute << "." << st_end.wSecond << ":" << st_end.wMilliseconds << std::endl;

				GetSystemTime(&st);
				TimeLogFile << "Simulation Step Time: " <<
					CURRENT_SIMTIME <<", iteration executed at real-time: "
					<< st.wHour << ":"
					<< st.wMinute << ":"
					<< st.wSecond << "."
					<< st.wMilliseconds;// << std::endl;
				
				QueryPerformanceCounter(&litmp);
				QStop = litmp.QuadPart;
				dfMinus = (double)(QStop - QStart);
				dfTim = dfMinus / dfFreq;

				
#if 1//TIMELOG
				if (dfTim > MaxControllerResponseTime)
				{
					MaxControllerResponseTime = dfTim;
				}
				if (dfTim < MinControllerResponseTime)
				{
					MinControllerResponseTime = dfTim;
				}
				TimeLogFile << ", iteration execution time = " << dfTim << std::endl;
#endif
#if 1//WAIT01SEC

				
				//TimeLogFile << "dfTim = " << dfTim;

				if (use_controller)
				{
					wait_until_N_sec = SimulationTimeStep;

					if (dfTim > SimulationTimeStep)	//if the execution time of this step is larger than simulation time step. eg if execute 150ms for this 0.1 sec/step
					{
						wait_until_N_sec = 0;	//then do not wait any longer to execute the next step
						step_diff_accumulate += dfTim - SimulationTimeStep;	//add the extra 150-100 ms to step_diff_accumulate for future compensation
						TimeLogFile << "1.A dfTim > SimulationTimeStep " << ", step_diff_accumulate += " << step_diff_accumulate << ", wait_until_N_sec = 0;";
					}
					else if (dfTim <= SimulationTimeStep)
					{
						if ( step_diff_accumulate >= (SimulationTimeStep - dfTim) )
						{
							wait_until_N_sec = 0;	//then do not wait any longer to execute the next step
							step_diff_accumulate -= (SimulationTimeStep - dfTim);
							TimeLogFile << "2.B step_diff_accumulate >= (SimulationTimeStep - dfTim) " << ", step_diff_accumulate -= " << step_diff_accumulate << ", wait_until_N_sec = 0;";
						}
						else if ( step_diff_accumulate < (SimulationTimeStep - dfTim) )
						{
							wait_until_N_sec = SimulationTimeStep - step_diff_accumulate;
							step_diff_accumulate = 0;
							TimeLogFile << "3.C step_diff_accumulate < (SimulationTimeStep - dfTim) " << ", step_diff_accumulate = 0 " << ", wait_until_N_sec = " << wait_until_N_sec;
						}
					}
					
					TimeLogFile << std::endl;

					GetSystemTime(&st_end);
					TimeLogFile << "wait_until_N_sec?" << st_end.wMinute << "." << st_end.wSecond << ":" << st_end.wMilliseconds << std::endl;
					TimeLogFile << std::endl;
					while (dfTim < wait_until_N_sec/*SimulationTimeStep*/)/*;*/
					/*do */
					{
						QueryPerformanceCounter(&litmp);
						// Ending Time Stamp
						QStop = litmp.QuadPart;
						dfMinus = (double)(QStop - QStart);
						// Actual time in seconds
						dfTim = dfMinus / dfFreq;
					} 
				}
				
#endif
#endif

			} //while status ==0									
			
#if _UseController
			QueryPerformanceCounter(&litmp2);
			QStop2 = litmp2.QuadPart;
			dfMinus = (double)(QStop2 - QStart2);
			// Actual time in seconds
			dfTim = dfMinus / dfFreq2;
			double RunningTime = dfTim /*/ (double)(N2)*/ * 1000;

			TimeLogFile << "total execution time: " << RunningTime << " milliseconds." << std::endl;
			TimeLogFile << "MinControllerResponseTime = " << MinControllerResponseTime << std::endl;
			TimeLogFile << "MaxControllerResponseTime = " << MaxControllerResponseTime << std::endl;
			TimeLogFile.close();
			ACLogFile.close();

			if (use_controller)
			{
				for (int i = 0; i < n_controllers; i++)
				{
					wcf_controller_data[i].is_etrunner_ready = -1;
				}

				proxy->SetControllerData(wcf_controller_data);
				//proxy->SetControllerDataState(-1);
			}
			
			
			delete controller_index_in_use;
			delete phase_calls;
			delete green_phases;
			delete yellow_phases;
#endif
			//call SHUTDOWN once
			status = etFommIF.ShutDown();

			if(NumberRuns >= 1) std::cout << "Finished run " << nrun << std::endl << std::endl;
			if (APIFlag == 1)
			{
				proxy->SetClientState(NextRun);
#if WCF_SCOPE
				proxy->SetRunnerState(RunnerFinished);
#endif
			}

			if (NumberRuns > 1)
			{
				etFommIF.ReloadDLL();
			}
		}
	} catch (InterfaceException &e)
	{
		std::cout<< std::string(e.what()) << std::endl;
	}

	if (wcfServer == 1) 
	{
		bool APIShutFlag = false;
		while (!APIShutFlag)
		{
			std::cout<<"Simulation runs are finished. Please shut down API Client..." << endl;
			proxy->SetClientState(Finished);
#if _USE_TCA
			proxy->SetTCAState(SimulationFinished);
#endif
			PauseFlag = APIProcessClientRequest(proxy, &etFommIF, dataPath, fileName1, updatedFlag, prefix);
			if (proxy->GetClientState() == APIShutDown) APIShutFlag = true;
		}
		
		std::cout<<"Stopping WCF service, please wait ..."<<std::endl<<std::endl;
		host->Close();	
		std::cout << "WCF service is stopped." << std::endl << std::endl;
		std::cout << "############################" << std::endl << std::endl;
	}

	char terminate;
	std::cout << "hit any key to terminate" << std::endl;
	std::cin>>terminate;
	return 0;
}

void etFommUpdateVehicles(IService1^ proxy, etFommInterface *etFommIF)
{
	array<WCF_VFData> ^ vehicles = proxy->GetServerFVehicleData();	
	int nvehicles = etFommIF->GetNumberOfFVehicles();
	if (vehicles->Length > nvehicles) 
	{
		std::cout << "The client has submitted more vehicles than etFomm. Vehicles are not updated." << std::endl;
	} else if (vehicles->Length < nvehicles) 
	{
		std::cout << "The client has submitted less vehicles than etFomm. Vehicles are not updated." << std::endl;
	} else 
	{
		fvehicle_data = (VFData*)calloc(nvehicles, sizeof(VFData));
		int status = etFommIF->GetFVehicle(fvehicle_data);
		for(int i=0; i<vehicles->Length; i++)
		{
			fvehicle_data[i].id = vehicles[i].id;
			fvehicle_data[i].drivertype = vehicles[i].drivertype;
			fvehicle_data[i].speed = vehicles[i].speed;		
			fvehicle_data[i].location = vehicles[i].location;
			fvehicle_data[i].link = vehicles[i].link;
			fvehicle_data[i].lane = vehicles[i].lane;
		}
		//set etFomm 
		std::cout << std::endl << "client  vehicle information:" << std::endl;	
		DisplayArrayFVehicles(vehicles); 
		//update etFomm's vehicle data
		int status2 = etFommIF->SetFVehicle(fvehicle_data);
		proxy->SetClientState(RequestingUpdate);
		free(fvehicle_data);
	}
}


void AddPath(IService1^ proxy, etFommInterface *etFommIF)
{	
	array <int> ^ pathdata =  proxy->GetServerPathData();	
	int number_of_nodes = pathdata->Length;
	if (number_of_nodes >0) 
	{
		int* nodes = new int[number_of_nodes];

		for(int i=0; i<number_of_nodes; i++)
		{
			nodes[i]=pathdata[i];
		}	
		int PID = etFommIF->AddPath(number_of_nodes, nodes);
		std::cout << std::endl << "client path is added to etFomm. " << std::endl;	
		DisplayArrayPath( pathdata);	
		proxy->SetClientState(RequestingUpdate);
	}
	else 
	{
		std::cout << std::endl << "client path is empty. " << std::endl;	
	}
}


void AddNewVehicle(IService1^ proxy, etFommInterface *etFommIF)
{	
	array<NewVehicle> ^ newvehicle = proxy->GetServerNewVehicleData();
	int PID = etFommIF->ADD_VEHICLE(newvehicle[0].time, newvehicle[0].inode, newvehicle[0].pathid, newvehicle[0].driver, newvehicle[0].fleet, 
		newvehicle[0].vtype, newvehicle[0].overspeed, newvehicle[0].range);
	if (PID > 0) {
		std::cout << "client's new vehicle is added to etFomm, vehicle ID = " << PID << std::endl;
	} else {
		std::cout << "adding vehicle failed, please see message(s) below. " << std::endl;
	}
		
	proxy->SetClientState(RequestingUpdate);
}

void updateFTCSignals(IService1^ proxy, etFommInterface *etFommIF)
{
	//update FTC signals from client to etFomm
	int nftc = etFommIF->GetNumberOfFTCSignals();
	cout << "\n********** CHANGE FTC SIGNAL **********" << endl;
	cout<<" ****** Original FTC signals network:"<<endl;
	if (nftc > 0) 
	{
		FTC_DATA *ftc_signal_data;
		ftc_signal_data = (FTC_DATA*)calloc(nftc, sizeof(FTC_DATA));
		etFommIF->GetFTCSignals(ftc_signal_data);
		
		for (int isig = 0; isig <nftc; isig ++)
		{
			if (ftc_signal_data[isig].active_intervals > 1)
			{
				cout<<" @node="<<ftc_signal_data[isig].node 
					<<" active_intervals = "<<ftc_signal_data[isig].active_intervals
					<<" approaches  = "<<ftc_signal_data[isig].approaches
					<<" cycle_length = " << ftc_signal_data[isig].cycle_length
					<<endl;
				for(int nint = 0; nint < ftc_signal_data[isig].active_intervals; nint++)
				{
					cout<<"  duration("<<nint<<") = "<< ftc_signal_data[isig].duration[nint]<<" code: ";
					for (int nc = 0; nc < ftc_signal_data[isig].approaches; ++nc)
						cout << ftc_signal_data[isig].signal_code[nint][nc];
					cout << std::endl;
				}
			} else
			{
				cout<<" @node="<<ftc_signal_data[isig].node 
					<<" active_intervals = "<<ftc_signal_data[isig].active_intervals
					<<" approaches  = "<<ftc_signal_data[isig].approaches
					<<endl;
				for(int nint = 0; nint < ftc_signal_data[isig].active_intervals; nint++)
				{
					cout<<"  duration("<<nint<<") code: ";
					for (int nc = 0; nc < ftc_signal_data[isig].approaches; ++nc)
						cout << ftc_signal_data[isig].signal_code[nint][nc];
					cout << std::endl;
				}
			}
		} //for
		free(ftc_signal_data);
	}
	
	array<WCF_FTC_DATA>^ wcf_ftcs= proxy->GetServerFTCSignalData();

	FTC_DATA *ftc_signal_data_new;
	ftc_signal_data_new = (FTC_DATA*)calloc(wcf_ftcs->Length, sizeof(FTC_DATA));
	WCF_to_HOST_ftc_data_inputs(ftc_signal_data_new, wcf_ftcs);

	cout << endl;
	cout << endl;
	cout<<"****** New FTC signals network:"<<endl;
	for (int isig = 0; isig <wcf_ftcs->Length; isig ++)
	{
		if (ftc_signal_data_new[isig].active_intervals > 1)
		{
			cout<<" @node="<<ftc_signal_data_new[isig].node 
				<<" active_intervals = "<<ftc_signal_data_new[isig].active_intervals
				<<" approaches  = "<<ftc_signal_data_new[isig].approaches
				<<" cycle_length = " << ftc_signal_data_new[isig].cycle_length
				<<endl;
			for(int nint = 0; nint < ftc_signal_data_new[isig].active_intervals; nint++)
			{
				ftc_signal_data_new[isig].cycle_length += ftc_signal_data_new[isig].duration[nint];
				cout<<"  duration("<<nint<<") = "<< ftc_signal_data_new[isig].duration[nint]<<" code: ";
				for (int nc = 0; nc < ftc_signal_data_new[isig].approaches; ++nc)
						cout << ftc_signal_data_new[isig].signal_code[nint][nc];
					cout << std::endl;
			}
		} else
		{
			cout<<" @node="<<ftc_signal_data_new[isig].node 
				<<" active_intervals = "<<ftc_signal_data_new[isig].active_intervals
				<<" approaches  = "<<ftc_signal_data_new[isig].approaches
				<<endl;
			for(int nint = 0; nint < ftc_signal_data_new[isig].active_intervals; nint++)
			{
				ftc_signal_data_new[isig].cycle_length += ftc_signal_data_new[isig].duration[nint];
				cout<<"  duration("<<nint<<") code: ";
				for (int nc = 0; nc < ftc_signal_data_new[isig].approaches; ++nc)
						cout << ftc_signal_data_new[isig].signal_code[nint][nc];
					cout << std::endl;
			}
		}
	} //for
	cout << "********************" << endl << endl;
	//etFommIF->SetNumberOfFTCSignals(nftc);
	etFommIF->SetFTCSignals(ftc_signal_data_new);
	free(ftc_signal_data_new);
	proxy->SetClientState(RequestingUpdate);	
};



void DisplayArrayFVehicles(array<WCF_VFData> ^vehicle_data)
{
	std::cout << std::endl << "etFomm free way vehicle information:" << std::endl;
	for(int i=0; i<vehicle_data->Length; i++)
	{
		if(vehicle_data[i].id != 0)
		{
			std::cout << "Vehicle ID = " << vehicle_data[i].id ;
			std::cout << ", speed = " << vehicle_data[i].speed ;
			std::cout << ", location = " << vehicle_data[i].location << std::endl;
		}
	}
}
void DisplayArraySVehicles(array<WCF_VSData> ^vehicle_data)
{
	std::cout << std::endl << "etFomm street vehicle information:" << std::endl;
	for(int i=0; i<vehicle_data->Length; i++)
	{
		if(vehicle_data[i].id != 0)
		{
			std::cout << "Vehicle ID = " << vehicle_data[i].id ;
			std::cout << ", speed = " << vehicle_data[i].speed ;
			std::cout << ", location = " << vehicle_data[i].location << std::endl;
		}
	}
}

void DisplayArrayPath(array <int> ^nodelist)
{
	cout << "Number of nodes on path = " << nodelist->Length<< " nodes: "<<endl;
	for(int i=0; i<nodelist->Length; i++)
	{
		std::cout << nodelist[i] << " ";		
	}
	cout<<endl;
}

void DisplayArrayNewVehicles(array<NewVehicle> ^new_veh)
{
	for(int i=0; i<new_veh->Length; i++)
	{
		std::cout << "New Vehicle Data: time = " <<new_veh[i].time;
		std::cout << ", inode = " << new_veh[i].inode << ", pathid = "<< new_veh[i].pathid<<endl;
		std::cout << ", driver = " << new_veh[i].driver << ", fleet= "<< new_veh[i].fleet<<endl;
		std::cout << ", vtype = " << new_veh[i].vtype << ", overspeed= "<< new_veh[i].overspeed<<endl;
		std::cout << ", range = " << new_veh[i].range<< std::endl;		
	}
}

void DisplaySignals(int nodeid, int interval, int duration, int old_green_time)
{
	std::cout<<"node = "<<nodeid<<" interval = "<<interval<<" green time = "<<old_green_time<<endl;		
	std::cout<<"new green time  = "<<duration<<std::endl;
	std::cout<<std::endl;
}

NETWORK_INPUTS WCF_to_HOST_network_input_data(array<WCF_NETWORK_INPUTS>^ wcf_network_inputs)
{
	NETWORK_INPUTS Network_Inputs;
	if (wcf_network_inputs->Length > 0)
	{
	Network_Inputs.run_init = wcf_network_inputs[0].run_init;
	Network_Inputs.initialization_end = wcf_network_inputs[0].initialization_end;
	Network_Inputs.timestep = wcf_network_inputs[0].timestep;
	Network_Inputs.time_interval = wcf_network_inputs[0].time_interval;
	Network_Inputs.type_of_run = wcf_network_inputs[0].type_of_run;
	Network_Inputs.sim_start_time = wcf_network_inputs[0].sim_start_time;
	Network_Inputs.max_node_number = wcf_network_inputs[0].max_node_number;

	for (int i = 0; i < wcf_network_inputs[0].time_period_duration->Length; i++)
	{
		Network_Inputs.time_period_duration[i] = wcf_network_inputs[0].time_period_duration[i];
	}
	}
	return Network_Inputs;

}
FREEWAY_NETWORK_INPUTS WCF_to_HOST_freeway_network_input_data(array<WCF_FREEWAY_NETWORK_INPUTS>^ wcf_freeway_network_inputs)
{
	FREEWAY_NETWORK_INPUTS Freeway_Network_Inputs;
	if (wcf_freeway_network_inputs->Length > 0)
	{
	for (int i = 0; i < wcf_freeway_network_inputs[0].cfrict->Length; i++)
	{
		Freeway_Network_Inputs.cfrict[i] = wcf_freeway_network_inputs[0].cfrict[i];
	}
	Freeway_Network_Inputs.default_hov_pct = wcf_freeway_network_inputs[0].default_hov_pct;
	Freeway_Network_Inputs.lag_accel = wcf_freeway_network_inputs[0].lag_accel;
	Freeway_Network_Inputs.lag_decel = wcf_freeway_network_inputs[0].lag_decel;
	for (int i = 0; i < wcf_freeway_network_inputs[0].ffspeed_adj->Length; i++)
	{
		Freeway_Network_Inputs.ffspeed_adj[i] = wcf_freeway_network_inputs[0].ffspeed_adj[i];
	}
	
	Freeway_Network_Inputs.freeway_pct_coop = wcf_freeway_network_inputs[0].freeway_pct_coop;
	Freeway_Network_Inputs.lc_time = wcf_freeway_network_inputs[0].lc_time;
	Freeway_Network_Inputs.dlc_mult = wcf_freeway_network_inputs[0].dlc_mult;
	}
	return Freeway_Network_Inputs;
}
STREET_NETWORK_INPUTS WCF_to_HOST_street_network_input_data(array<WCF_STREET_NETWORK_INPUTS>^ wcf_street_network_inputs)
{
	STREET_NETWORK_INPUTS Street_Network_Inputs;
	if (wcf_street_network_inputs->Length > 0)
	{
	for (int i = 0; i < wcf_street_network_inputs[0].additional_gap->Length; i++)
	{
		Street_Network_Inputs.additional_gap[i] = wcf_street_network_inputs[0].additional_gap[i];
	}
	for (int i = 0; i < wcf_street_network_inputs[0].amber_decel->Length; i++)
	{
		Street_Network_Inputs.amber_decel[i] = wcf_street_network_inputs[0].amber_decel[i];
	}
	Street_Network_Inputs.lt_speed = wcf_street_network_inputs[0].lt_speed;
	Street_Network_Inputs.rt_speed = wcf_street_network_inputs[0].rt_speed;
	for (int i = 0; i < wcf_street_network_inputs[0].pdelay_weak->Length; i++)
	{
		Street_Network_Inputs.pdelay_weak[i] = wcf_street_network_inputs[0].pdelay_weak[i];
	}
	for (int i = 0; i < wcf_street_network_inputs[0].pdelay_strong->Length; i++)
	{
		Street_Network_Inputs.pdelay_strong[i] = wcf_street_network_inputs[0].pdelay_strong[i];
	}
	for (int i = 0; i < wcf_street_network_inputs[0].ped_duration->Length; i++)
	{
		Street_Network_Inputs.ped_duration[i] = wcf_street_network_inputs[0].ped_duration[i];
	}
	for (int i = 0; i < wcf_street_network_inputs[0].acceptable_gap->Length; i++)
	{
		Street_Network_Inputs.acceptable_gap[i] = wcf_street_network_inputs[0].acceptable_gap[i];
	}
	for (int i = 0; i < wcf_street_network_inputs[0].acceptable_ltg->Length; i++)
	{
		Street_Network_Inputs.acceptable_ltg[i] = wcf_street_network_inputs[0].acceptable_ltg[i];
	}
	for (int i = 0; i < wcf_street_network_inputs[0].acceptable_rtg->Length; i++)
	{
		Street_Network_Inputs.acceptable_rtg[i] = wcf_street_network_inputs[0].acceptable_rtg[i];
	}
	for (int i = 0; i < wcf_street_network_inputs[0].dwell_multiplier->Length; i++)
	{
		for (int j = 0; j < wcf_street_network_inputs[0].dwell_multiplier[i]->Length; j++)
		{
			Street_Network_Inputs.dwell_multiplier[i][j] = wcf_street_network_inputs[0].dwell_multiplier[i][j];
		}
	}
	for (int i = 0; i < wcf_street_network_inputs[0].ffspeed_adj->Length; i++)
	{
		Street_Network_Inputs.ffspeed_adj[i] = wcf_street_network_inputs[0].ffspeed_adj[i];
	}
	
	Street_Network_Inputs.lc_time = wcf_street_network_inputs[0].lc_time;
	for (int i = 0; i < wcf_street_network_inputs[0].lt_jumper_prob->Length; i++)
	{
		Street_Network_Inputs.lt_jumper_prob[i] = wcf_street_network_inputs[0].lt_jumper_prob[i];
	}
	for (int i = 0; i < wcf_street_network_inputs[0].lt_lagger_prob->Length; i++)
	{
		Street_Network_Inputs.lt_lagger_prob[i] = wcf_street_network_inputs[0].lt_lagger_prob[i];
	}
	for (int i = 0; i < wcf_street_network_inputs[0].spillback_prob->Length; i++)
	{
		Street_Network_Inputs.spillback_prob[i] = wcf_street_network_inputs[0].spillback_prob[i];
	}
	Street_Network_Inputs.stop_spd = wcf_street_network_inputs[0].stop_spd;
	Street_Network_Inputs.street_pct_coop = wcf_street_network_inputs[0].street_pct_coop;
	Street_Network_Inputs.yield_spd = wcf_street_network_inputs[0].yield_spd;
	Street_Network_Inputs.driver_fampct = wcf_street_network_inputs[0].driver_fampct;
	
	for (int i = 0; i < wcf_street_network_inputs[0].qfactor->Length; i++)
	{
		Street_Network_Inputs.qfactor[i] = wcf_street_network_inputs[0].qfactor[i];
	}
	for (int i = 0; i < wcf_street_network_inputs[0].ste_mult->Length; i++)
	{
		Street_Network_Inputs.ste_mult[i] = wcf_street_network_inputs[0].ste_mult[i];
	}
	}
	return Street_Network_Inputs;
}
void WCF_to_HOST_Vehicle_Type_Inputs(VEHICLE_TYPE_DATA* Vehicle_Type_Inputs, array<WCF_VEHICLE_TYPE_DATA>^ wcf_vdi)
{
	for (int i = 0; i < wcf_vdi->Length; i++)
	{
		Vehicle_Type_Inputs[i].length = wcf_vdi[i].length;
		Vehicle_Type_Inputs[i].headway_factor = wcf_vdi[i].headway_factor;
		Vehicle_Type_Inputs[i].average_occupancy = wcf_vdi[i].average_occupancy;
		Vehicle_Type_Inputs[i].emergency_decel = wcf_vdi[i].emergency_decel;
		Vehicle_Type_Inputs[i].fleet_freeway_auto = wcf_vdi[i].fleet_freeway_auto;
		Vehicle_Type_Inputs[i].fleet_freeway_truck = wcf_vdi[i].fleet_freeway_truck;
		Vehicle_Type_Inputs[i].fleet_freeway_carpool = wcf_vdi[i].fleet_freeway_carpool;
		Vehicle_Type_Inputs[i].fleet_freeway_bus = wcf_vdi[i].fleet_freeway_bus;
		Vehicle_Type_Inputs[i].fleet_freeway_ev = wcf_vdi[i].fleet_freeway_ev;
		Vehicle_Type_Inputs[i].fleet_freeway_bike = wcf_vdi[i].fleet_freeway_bike;
		Vehicle_Type_Inputs[i].fleet_street_auto = wcf_vdi[i].fleet_street_auto;
		Vehicle_Type_Inputs[i].fleet_street_truck = wcf_vdi[i].fleet_street_truck;
		Vehicle_Type_Inputs[i].fleet_street_carpool = wcf_vdi[i].fleet_street_carpool;
		Vehicle_Type_Inputs[i].fleet_street_bus = wcf_vdi[i].fleet_street_bus;
		Vehicle_Type_Inputs[i].fleet_street_ev = wcf_vdi[i].fleet_street_ev;
		Vehicle_Type_Inputs[i].fleet_street_bike = wcf_vdi[i].fleet_street_bike;
	}
}

void WCF_to_HOST_freeway_link_data(FREEWAY_LINK* freeway_link_data, array<Wcf_freeway_link>^ wcf_fwl)
{
	for (int il = 0; il < wcf_fwl->Length; il++)
	{
		freeway_link_data[il].ID = wcf_fwl[il].id;
		freeway_link_data[il].usn = wcf_fwl[il].usn;
		freeway_link_data[il].dsn = wcf_fwl[il].dsn;
		freeway_link_data[il].linktype = wcf_fwl[il].linktype;
		freeway_link_data[il].thrunode = wcf_fwl[il].thrunode;
		freeway_link_data[il].mainline_sending_lane = wcf_fwl[il].mainline_sending_lane;
		freeway_link_data[il].mainline_receiving_lane = wcf_fwl[il].mainline_receiving_lane;
		freeway_link_data[il].offramp_sending_lane = wcf_fwl[il].offramp_sending_lane;
		freeway_link_data[il].offramp_receiving_lane = wcf_fwl[il].offramp_receiving_lane;
		freeway_link_data[il].exitnode = wcf_fwl[il].exitnode;		
		freeway_link_data[il].length = wcf_fwl[il].length;
		freeway_link_data[il].fulllanes = wcf_fwl[il].fulllanes;

		freeway_link_data[il].freeflowspeed = wcf_fwl[il].freeflowspeed;
		freeway_link_data[il].thru_percent = wcf_fwl[il].thru_percent;
		freeway_link_data[il].offramp_warn_distance = wcf_fwl[il].offramp_warn_distance;
		freeway_link_data[il].anticip_warning_distance = wcf_fwl[il].anticip_warning_distance;
		freeway_link_data[il].anticip_warning_speed = wcf_fwl[il].anticip_warning_speed;
		freeway_link_data[il].nhov_lanes = wcf_fwl[il].nhov_lanes;
		freeway_link_data[il].hov_begin = wcf_fwl[il].hov_begin;
		freeway_link_data[il].hov_end = wcf_fwl[il].hov_end;
		freeway_link_data[il].hov_code = wcf_fwl[il].hov_code;

		freeway_link_data[il].hov_offramp_warn_distance = wcf_fwl[il].hov_offramp_warn_distance;
		freeway_link_data[il].hov_side = wcf_fwl[il].hov_side;
		freeway_link_data[il].hov_type = wcf_fwl[il].hov_type;
		freeway_link_data[il].hov_warn = wcf_fwl[il].hov_warn;
		freeway_link_data[il].hov_pct = wcf_fwl[il].hov_pct;
		freeway_link_data[il].cfmult = wcf_fwl[il].cfmult;
		freeway_link_data[il].first_detector = wcf_fwl[il].first_detector;
		freeway_link_data[il].grade = wcf_fwl[il].grade;
		freeway_link_data[il].tilt = wcf_fwl[il].tilt;
		freeway_link_data[il].curve = wcf_fwl[il].curve;
		freeway_link_data[il].pavement = wcf_fwl[il].pavement;
		freeway_link_data[il].shoulder_width = wcf_fwl[il].shoulder_width;

		freeway_link_data[il].datastation_id = wcf_fwl[il].datastation_id;
		freeway_link_data[il].datastation_location = wcf_fwl[il].datastation_location;
		freeway_link_data[il].truck_code = wcf_fwl[il].truck_code;
		freeway_link_data[il].truck_dir = wcf_fwl[il].truck_dir;
		freeway_link_data[il].truck_lane = wcf_fwl[il].truck_lane;
		freeway_link_data[il].etl_warn = wcf_fwl[il].etl_warn;

		for (int iadddrop = 0; iadddrop < wcf_fwl[il].adddrop_code->Length; iadddrop ++)
		{
			freeway_link_data[il].adddrop_code[iadddrop] = wcf_fwl[il].adddrop_code[iadddrop];
		}
		for (int iadddrop = 0; iadddrop < wcf_fwl[il].adddrop_lane->Length; iadddrop ++)
		{
			freeway_link_data[il].adddrop_lane[iadddrop] = wcf_fwl[il].adddrop_lane[iadddrop];
		}
		for (int iadddrop = 0; iadddrop < wcf_fwl[il].adddrop_dist->Length; iadddrop ++)
		{
			freeway_link_data[il].adddrop_dist[iadddrop] = wcf_fwl[il].adddrop_dist[iadddrop];
		}
		for (int iadddrop = 0; iadddrop < wcf_fwl[il].adddrop_warn->Length; iadddrop ++)
		{
			freeway_link_data[il].adddrop_warn[iadddrop] = wcf_fwl[il].adddrop_warn[iadddrop];
		}

		for (int iaux = 0; iaux < wcf_fwl[il].auxlaneid->Length; iaux ++)
		{
			freeway_link_data[il].auxlaneid[iaux] = wcf_fwl[il].auxlaneid[iaux];
		}
		for (int iaux = 0; iaux < wcf_fwl[il].auxlanecode->Length; iaux ++)
		{
			freeway_link_data[il].auxlanecode[iaux] = wcf_fwl[il].auxlanecode[iaux];
		}
		for (int iaux = 0; iaux < wcf_fwl[il].auxlanelength->Length; iaux ++)
		{
			freeway_link_data[il].auxlanelength[iaux] = wcf_fwl[il].auxlanelength[iaux];
		}

		for (int ihov = 0; ihov < wcf_fwl[il].hov_lanes->Length; ihov ++)
		{
			freeway_link_data[il].hov_lanes[ihov] = wcf_fwl[il].hov_lanes[ihov];
		}

		for (int i = 0; i < wcf_fwl[il].lane_width->Length; i++)
		{
			freeway_link_data[il].lane_width[i] = wcf_fwl[il].lane_width[i];
		}

		for (int ibarrier = 0; ibarrier < wcf_fwl[il].barrier->Length; ibarrier ++)
		{
			freeway_link_data[il].barrier[ibarrier] = wcf_fwl[il].barrier[ibarrier];
		}

		for (int i = 0; i < wcf_fwl[il].exclude_type->Length; i++)
		{
			for (int j = 0; j < wcf_fwl[il].exclude_type[i]->Length; j++)
			{
				freeway_link_data[il].exclude_type[i][j] = wcf_fwl[il].exclude_type[i][j];
			}
		}

		for (int imultiplier_exit = 0; imultiplier_exit < wcf_fwl[il].multiplier_exit->Length; imultiplier_exit ++)
		{
			freeway_link_data[il].multiplier_exit[imultiplier_exit] = wcf_fwl[il].multiplier_exit[imultiplier_exit];
		}
		freeway_link_data[il].startup_time = wcf_fwl[il].startup_time;
	}
}
void WCF_to_HOST_street_link_data(STREET_LINK* street_link_data, array<Wcf_street_link>^ wcf_sl)
{
	for (int il = 0; il < wcf_sl->Length; il++)
	{
		street_link_data[il].id = wcf_sl[il].id;
		street_link_data[il].usn = wcf_sl[il].usn;
		street_link_data[il].dsn = wcf_sl[il].dsn;
		street_link_data[il].thrunode = wcf_sl[il].thrunode;
		street_link_data[il].leftnode = wcf_sl[il].leftnode;
		street_link_data[il].rightnode = wcf_sl[il].rightnode;
		street_link_data[il].diagnode = wcf_sl[il].diagnode;
		street_link_data[il].opposenode = wcf_sl[il].opposenode;
		street_link_data[il].length = wcf_sl[il].length;
		street_link_data[il].fulllanes = wcf_sl[il].fulllanes;
		street_link_data[il].leftturnbays = wcf_sl[il].leftturnbays;
		street_link_data[il].rightturnbays = wcf_sl[il].rightturnbays;
		street_link_data[il].freeflowspeed = wcf_sl[il].freeflowspeed;

		street_link_data[il].leftpct = wcf_sl[il].leftpct;
		street_link_data[il].thrupct = wcf_sl[il].thrupct;
		street_link_data[il].rightpct = wcf_sl[il].rightpct;
		street_link_data[il].diagpct = wcf_sl[il].diagpct;
		street_link_data[il].distribution_code = wcf_sl[il].distribution_code;
		street_link_data[il].startup_delay = wcf_sl[il].startup_delay;
		street_link_data[il].discharge_hdwy = wcf_sl[il].discharge_hdwy;
		street_link_data[il].rtor = wcf_sl[il].rtor;
		street_link_data[il].ped_code = wcf_sl[il].ped_code;
		street_link_data[il].lane1 = wcf_sl[il].lane1;
		street_link_data[il].lane2 = wcf_sl[il].lane2;
		street_link_data[il].cfmult = wcf_sl[il].cfmult;
		street_link_data[il].sight_dist = wcf_sl[il].sight_dist;
		street_link_data[il].first_detector = wcf_sl[il].first_detector;
		street_link_data[il].grade = wcf_sl[il].grade;
		street_link_data[il].shoulder_width = wcf_sl[il].shoulder_width;
		street_link_data[il].ste_freq = wcf_sl[il].ste_freq;
		street_link_data[il].ste_duration = wcf_sl[il].ste_duration;
		street_link_data[il].signal_range = wcf_sl[il].signal_range;
		street_link_data[il].centroid = wcf_sl[il].centroid;
		street_link_data[il].centroid_label = wcf_sl[il].centroid_label;

		for (int ichannelization = 0; ichannelization < wcf_sl[il].channelization->Length; ichannelization++)
		{
			street_link_data[il].channelization[ichannelization] = wcf_sl[il].channelization[ichannelization];
		}

		for (int ilane = 0; ilane < wcf_sl[il].lane_width->Length; ++ilane)
		{
			street_link_data[il].lane_width[ilane] = wcf_sl[il].lane_width[ilane];
		}

		for (int i = 0; i < wcf_sl[il].exclude_type->Length; i++)
		{
			for (int j = 0; j < wcf_sl[il].exclude_type[i]->Length; j++)
			{
				street_link_data[il].exclude_type[i][j] = wcf_sl[il].exclude_type[i][j];
			}
		}

		for (int imultiplier_left = 0; imultiplier_left < wcf_sl[il].multiplier_left->Length; imultiplier_left ++)
		{
			street_link_data[il].multiplier_left[imultiplier_left] = wcf_sl[il].multiplier_left[imultiplier_left];
		}
		for (int imultiplier_thru = 0; imultiplier_thru < wcf_sl[il].multiplier_thru->Length; imultiplier_thru ++)
		{
			street_link_data[il].multiplier_thru[imultiplier_thru] = wcf_sl[il].multiplier_thru[imultiplier_thru];
		}
		for (int imultiplier_right = 0; imultiplier_right < wcf_sl[il].multiplier_right->Length; imultiplier_right ++)
		{
			street_link_data[il].multiplier_right[imultiplier_right] = wcf_sl[il].multiplier_right[imultiplier_right];
		}
		for (int imultiplier_diag = 0; imultiplier_diag < wcf_sl[il].multiplier_diag->Length; imultiplier_diag ++)
		{
			street_link_data[il].multiplier_diag[imultiplier_diag] = wcf_sl[il].multiplier_diag[imultiplier_diag];
		}
	}
}
void WCF_to_HOST_cond_turnpct_data(COND_TURNPCTS* cond_turnpct_data, array<WCF_COND_TURNPCTS>^ wcf_cond_turnpct_data)
{
	for (int i = 0; i < wcf_cond_turnpct_data->Length; i++)
	{
		cond_turnpct_data[i].USN = wcf_cond_turnpct_data[i].USN;
		cond_turnpct_data[i].DSN = wcf_cond_turnpct_data[i].DSN;
		for (int j = 0; j < wcf_cond_turnpct_data[i].LEFTPCT->Length; j++)
		{
			cond_turnpct_data[i].LEFTPCT[j] = wcf_cond_turnpct_data[i].LEFTPCT[j];
		}
		for (int j = 0; j < wcf_cond_turnpct_data[i].THRUPCT->Length; j++)
		{
			cond_turnpct_data[i].THRUPCT[j] = wcf_cond_turnpct_data[i].THRUPCT[j];
		}
		for (int j = 0; j < wcf_cond_turnpct_data[i].RIGHTPCT->Length; j++)
		{
			cond_turnpct_data[i].RIGHTPCT[j] = wcf_cond_turnpct_data[i].RIGHTPCT[j];
		}
		for (int j = 0; j < wcf_cond_turnpct_data[i].DIAGPCT->Length; j++)
		{
			cond_turnpct_data[i].DIAGPCT[j] = wcf_cond_turnpct_data[i].DIAGPCT[j];
		}
	}
}
void WCF_to_HOST_entry_node_data(ENTRYNODES_DATA* entrynode_inputs, array<WCF_ENTRYNODES_DATA>^ wcf_entry_node)
{
	for (int in = 0; in < wcf_entry_node->Length; in++)
	{
		entrynode_inputs[in].Node_ID = wcf_entry_node[in].Node_ID;
		entrynode_inputs[in].carpool_pct = wcf_entry_node[in].carpool_pct;
		entrynode_inputs[in].flowrate = wcf_entry_node[in].flowrate;
		entrynode_inputs[in].truck_pct = wcf_entry_node[in].truck_pct;
		entrynode_inputs[in].hov_violators_per10000 = wcf_entry_node[in].hov_violators_per10000;
		entrynode_inputs[in].SS_USN = wcf_entry_node[in].SS_USN;
		entrynode_inputs[in].SS_DSN = wcf_entry_node[in].SS_DSN;
		for (int ip = 0; ip < wcf_entry_node[in].lane_pct->Length; ip++)
		{
			entrynode_inputs[in].lane_pct[ip] = wcf_entry_node[in].lane_pct[ip];
		}
	}
}

void WCF_to_HOST_ftc_data_inputs(FTC_DATA* ftc_data_inputs, array<WCF_FTC_DATA>^ wcf_ftcs)
{
	for (int i = 0; i < wcf_ftcs->Length; i++)
	{
		ftc_data_inputs[i].active_intervals = wcf_ftcs[i].active_intervals;
		for (int j = 0; j < wcf_ftcs[i].approach->Length; j++)
		{
			ftc_data_inputs[i].approach[j] = wcf_ftcs[i].approach[j];
		}
		ftc_data_inputs[i].approaches = wcf_ftcs[i].approaches;
		ftc_data_inputs[i].current_interval = wcf_ftcs[i].current_interval;
		ftc_data_inputs[i].cycle_length = wcf_ftcs[i].cycle_length;
		for (int j = 0; j < wcf_ftcs[i].duration->Length; j++)
		{
			ftc_data_inputs[i].duration[j] = wcf_ftcs[i].duration[j];
		}
		ftc_data_inputs[i].external_control = wcf_ftcs[i].external_control;
		ftc_data_inputs[i].node = wcf_ftcs[i].node;
		ftc_data_inputs[i].offset = wcf_ftcs[i].offset;
		ftc_data_inputs[i].range = wcf_ftcs[i].range;
		ftc_data_inputs[i].time_in_interval = wcf_ftcs[i].time_in_interval;
		for (int j = 0; j < wcf_ftcs[i].signal_code->Length; j++)
		{
			for (int k = 0; k < wcf_ftcs[i].signal_code[j]->Length; k++)
			{
				ftc_data_inputs[i].signal_code[j][k] = wcf_ftcs[i].signal_code[j][k];
			}
		}
	}
}
void WCF_to_HOST_ac_data_inputs(AC_INPUTS* ac_data_inputs, array<WCF_AC>^ wcf_acl)
{
	for (int i = 0; i < wcf_acl->Length; i++)
	{
		ac_data_inputs[i].node = wcf_acl[i].node;
		ac_data_inputs[i].cfails = wcf_acl[i].cfails;
		ac_data_inputs[i].adj = wcf_acl[i].adj;
		ac_data_inputs[i].cycle_length = wcf_acl[i].cycle_length;
		ac_data_inputs[i].offset = wcf_acl[i].offset;
		ac_data_inputs[i].n_direct_approaches = wcf_acl[i].n_direct_approaches;
		for (int j = 0; j < wcf_acl[i].direct_approach_USN->Length; j++)
		{
			ac_data_inputs[i].direct_approach_USN[j] = wcf_acl[i].direct_approach_USN[j];
		}

		ac_data_inputs[i].n_indirect_approaches = wcf_acl[i].n_indirect_approaches;
		for (int j = 0; j < wcf_acl[i].indirect_approach_USN->Length; j++)
		{
			ac_data_inputs[i].indirect_approach_USN[j] = wcf_acl[i].indirect_approach_USN[j];
		}
		for (int j = 0; j < wcf_acl[i].indirect_approach_DSN->Length; j++)
		{
			ac_data_inputs[i].indirect_approach_DSN[j] = wcf_acl[i].indirect_approach_DSN[j];
		}

		for (int j = 0; j < wcf_acl[i].leftarrow->Length; j++)
		{
			for (int k = 0; k < wcf_acl[i].leftarrow[j]->Length; k++)
			{
				ac_data_inputs[i].leftarrow[j][k] = wcf_acl[i].leftarrow[j][k];
				ac_data_inputs[i].thruarrow[j][k] = wcf_acl[i].thruarrow[j][k];
				ac_data_inputs[i].rightarrow[j][k] = wcf_acl[i].rightarrow[j][k];
				ac_data_inputs[i].diagarrow[j][k] = wcf_acl[i].diagarrow[j][k];
			}
		}

		for (int j = 0; j < wcf_acl[i].actuated_mode->Length; j++)
		{
			ac_data_inputs[i].actuated_mode[j] = wcf_acl[i].actuated_mode[j];
			ac_data_inputs[i].	min_green_time	[j]	=	wcf_acl[i].	min_green_time	[j]	;
			ac_data_inputs[i].	max_green_time	[j]	=	wcf_acl[i].	max_green_time	[j]	;
			ac_data_inputs[i].	default_extension_time	[j]	=	wcf_acl[i].	default_extension_time	[j]	;
			ac_data_inputs[i].	gap_time	[j]	=	wcf_acl[i].	gap_time	[j]	;
			ac_data_inputs[i].	times_before_reduction	[j]	=	wcf_acl[i].	times_before_reduction	[j]	;
			ac_data_inputs[i].	time_to_reduce	[j]	=	wcf_acl[i].	time_to_reduce	[j]	;
			ac_data_inputs[i].	min_gap_time	[j]	=	wcf_acl[i].	min_gap_time	[j]	;
			ac_data_inputs[i].	yellow_change_int	[j]	=	wcf_acl[i].	yellow_change_int	[j]	;
			ac_data_inputs[i].	red_clear_int	[j]	=	wcf_acl[i].	red_clear_int	[j]	;
			ac_data_inputs[i].	ped_allowed	[j]	=	wcf_acl[i].	ped_allowed	[j]	;
			ac_data_inputs[i].	walk_time	[j]	=	wcf_acl[i].	walk_time	[j]	;
			ac_data_inputs[i].	walk_clearance_time	[j]	=	wcf_acl[i].	walk_clearance_time	[j]	;
		}

		for (int j = 0; j < wcf_acl[i].ring_phase->Length; j++)
		{
			ac_data_inputs[i].ring_phase[j] = wcf_acl[i].ring_phase[j];
		}

		ac_data_inputs[i].detector_count = wcf_acl[i].detector_count;
		for (int j = 0; j < wcf_acl[i].detector_list->Length; j++)
		{
			ac_data_inputs[i].detector_list[j] = wcf_acl[i].detector_list[j];
		}
		ac_data_inputs[i].transition_method = wcf_acl[i].transition_method;
		ac_data_inputs[i].max_add = wcf_acl[i].max_add;
		ac_data_inputs[i].max_subtract = wcf_acl[i].max_subtract;
		for (int j = 0; j < wcf_acl[i].force_off_times->Length; j++)
		{
			ac_data_inputs[i].force_off_times[j] = wcf_acl[i].force_off_times[j];
		}

	}
}

void WCF_to_HOST_det_inputs(DETECTOR_INPUTS* det_inputs, array<WCF_DETECTOR_INPUTS>^ wcf_det_inputs)
{
	for (int i = 0; i < wcf_det_inputs->Length; i++)
	{
		det_inputs[i].usn = wcf_det_inputs[i].usn;
		det_inputs[i].dsn = wcf_det_inputs[i].dsn;
		det_inputs[i].signal_node = wcf_det_inputs[i].signal_node;
		det_inputs[i].associated_phase = wcf_det_inputs[i].associated_phase;
		det_inputs[i].station_id = wcf_det_inputs[i].station_id;
		det_inputs[i].location = wcf_det_inputs[i].location;
		det_inputs[i].link = wcf_det_inputs[i].link;
		det_inputs[i].lane1 = wcf_det_inputs[i].lane1;
		det_inputs[i].lane2 = wcf_det_inputs[i].lane2;
		det_inputs[i].zone_length = wcf_det_inputs[i].zone_length;
		det_inputs[i].delay_time = wcf_det_inputs[i].delay_time;
		det_inputs[i].carryover_time = wcf_det_inputs[i].carryover_time;
		det_inputs[i].type_code = wcf_det_inputs[i].type_code;
		det_inputs[i].operation_code = wcf_det_inputs[i].operation_code;
		det_inputs[i].detection_zone = wcf_det_inputs[i].detection_zone;
		
	}

}
void WCF_to_HOST_busroute_inputs(BUSROUTE_DATA* busroute_inputs, array<WCF_BUSROUTE_DATA>^ wcf_busroute_inputs)
{
	for (int i = 0; i < wcf_busroute_inputs->Length; i++)
	{
		busroute_inputs[i].number = wcf_busroute_inputs[i].number;
		busroute_inputs[i].hdwy = wcf_busroute_inputs[i].hdwy;
		busroute_inputs[i].offset = wcf_busroute_inputs[i].offset;
		busroute_inputs[i].nodes = wcf_busroute_inputs[i].nodes;
		for (int j = 0; j < wcf_busroute_inputs[i].route_nodes->Length; j++)
		{
			busroute_inputs[i].route_nodes[j] = wcf_busroute_inputs[i].route_nodes[j];
		}
		for (int j = 0; j < wcf_busroute_inputs[i].stationlist->Length; j++)
		{
			busroute_inputs[i].stationlist[j] = wcf_busroute_inputs[i].stationlist[j];
		}
		busroute_inputs[i].persontrips = wcf_busroute_inputs[i].persontrips;
		busroute_inputs[i].timer = wcf_busroute_inputs[i].timer;
		busroute_inputs[i].traveltime = wcf_busroute_inputs[i].traveltime;
		busroute_inputs[i].trips = wcf_busroute_inputs[i].trips;
	}
}
void WCF_to_HOST_busstation_inputs(BUSSTATION_DATA* busstation_inputs, array<WCF_BUSSTATION_DATA>^ wcf_busstation_inputs)
{
	for (int i = 0; i < wcf_busstation_inputs->Length; i++)
	{
		busstation_inputs[i].block_code = wcf_busstation_inputs[i].block_code;
		busstation_inputs[i].usn = wcf_busstation_inputs[i].usn;
		busstation_inputs[i].dsn = wcf_busstation_inputs[i].dsn;
		busstation_inputs[i].capacity = wcf_busstation_inputs[i].capacity;
		busstation_inputs[i].type_code = wcf_busstation_inputs[i].type_code;
		busstation_inputs[i].location = wcf_busstation_inputs[i].location;
		busstation_inputs[i].dwell = wcf_busstation_inputs[i].dwell;
		busstation_inputs[i].bypass_pct = wcf_busstation_inputs[i].bypass_pct;
		busstation_inputs[i].next_station = wcf_busstation_inputs[i].next_station;
		busstation_inputs[i].pocket_lane = wcf_busstation_inputs[i].pocket_lane;
		busstation_inputs[i].front = wcf_busstation_inputs[i].front;
		busstation_inputs[i].count = wcf_busstation_inputs[i].count;
		busstation_inputs[i].dwell_time = wcf_busstation_inputs[i].dwell_time;
		busstation_inputs[i].empty_time = wcf_busstation_inputs[i].empty_time;
		busstation_inputs[i].overflow_time = wcf_busstation_inputs[i].overflow_time;
	}
}
void WCF_to_HOST_xy_coord_inputs(NODE_LOCATION_DATA* xy_coord_inputs, array<WCF_NODE_LOCATION_DATA>^ wcf_xy_coord_inputs)
{
	for (int i = 0; i < wcf_xy_coord_inputs->Length; i++)
	{
		xy_coord_inputs[i].x = wcf_xy_coord_inputs[i].x;
		xy_coord_inputs[i].y = wcf_xy_coord_inputs[i].y;
		xy_coord_inputs[i].	latitude	=	wcf_xy_coord_inputs[i].	latitude	;
		xy_coord_inputs[i].	longitude	=	wcf_xy_coord_inputs[i].	longitude	;
		xy_coord_inputs[i].	elevation	=	wcf_xy_coord_inputs[i].	elevation	;
		xy_coord_inputs[i].	is_defined	=	wcf_xy_coord_inputs[i].	is_defined	;
	}
}
void WCF_to_HOST_rampmeter_inputs(RM_DATA* rampmeter_inputs, array<WCF_RM_DATA>^ wcf_rampmeter_inputs)
{
	for (int i = 0; i < wcf_rampmeter_inputs->Length; i++)
	{
		rampmeter_inputs[i].dsn = wcf_rampmeter_inputs[i].dsn;
		rampmeter_inputs[i].link = wcf_rampmeter_inputs[i].link;
		rampmeter_inputs[i].control = wcf_rampmeter_inputs[i].control;
		rampmeter_inputs[i].onset = wcf_rampmeter_inputs[i].onset;
		rampmeter_inputs[i].state = wcf_rampmeter_inputs[i].state;
		for (int j = 0; j < wcf_rampmeter_inputs[i].detector->Length; j++)
		{
			rampmeter_inputs[i].detector[j] = wcf_rampmeter_inputs[i].detector[j];
		}
		rampmeter_inputs[i].capacity = wcf_rampmeter_inputs[i].capacity;
		for (int j = 0; j < wcf_rampmeter_inputs[i].speed->Length; j++)
		{
			rampmeter_inputs[i].speed[j] = wcf_rampmeter_inputs[i].speed[j];
		}
		for (int j = 0; j < wcf_rampmeter_inputs[i].headway->Length; j++)
		{
			rampmeter_inputs[i].headway[j] = wcf_rampmeter_inputs[i].headway[j];
		}
		rampmeter_inputs[i].timer = wcf_rampmeter_inputs[i].timer;
		rampmeter_inputs[i].updint = wcf_rampmeter_inputs[i].updint;
		rampmeter_inputs[i].twopergreen = wcf_rampmeter_inputs[i].twopergreen;
	}
}
void WCF_to_Host_incident_data_inputs(INCIDENT_DATA* incident_data_inputs, array<WCF_INCIDENT_DATA>^ wcf_incident_data_inputs)
{
	for (int i = 0; i < wcf_incident_data_inputs->Length; i++)
	{
		incident_data_inputs[i].link = wcf_incident_data_inputs[i].link;
		incident_data_inputs[i].begin_point = wcf_incident_data_inputs[i].begin_point;
		incident_data_inputs[i].begin_time = wcf_incident_data_inputs[i].begin_time;
		incident_data_inputs[i].end_point = wcf_incident_data_inputs[i].end_point;
		incident_data_inputs[i].end_time = wcf_incident_data_inputs[i].end_time;
		incident_data_inputs[i].rbnf = wcf_incident_data_inputs[i].rbnf;
		incident_data_inputs[i].warn_point = wcf_incident_data_inputs[i].warn_point;
		for (int j = 0; j < wcf_incident_data_inputs[i].code->Length; j++)
		{
			incident_data_inputs[i].code[j] = wcf_incident_data_inputs[i].code[j];
		}
	}
}

void WCF_to_Host_parking_data_inputs(PARKING_DATA* parking_data_inputs, array<WCF_PARKING_DATA>^ wcf_parking_data_inputs)
{
	for (int i = 0; i < wcf_parking_data_inputs->Length; ++i) 
	{
		parking_data_inputs[i].link = wcf_parking_data_inputs[i].link;
		parking_data_inputs[i].duration = wcf_parking_data_inputs[i].duration;
		parking_data_inputs[i].freq = wcf_parking_data_inputs[i].freq;
		parking_data_inputs[i].left_start = wcf_parking_data_inputs[i].left_start;
		parking_data_inputs[i].left_len = wcf_parking_data_inputs[i].left_len;
		parking_data_inputs[i].right_start = wcf_parking_data_inputs[i].right_start;
		parking_data_inputs[i].right_len = wcf_parking_data_inputs[i].right_len;
	}
}

void WCF_to_Host_event_data_inputs(EVENT_DATA* event_data_inputs, array<WCF_EVENT_DATA>^ wcf_event_data_inputs)
{
	for (int i = 0; i < wcf_event_data_inputs->Length; ++i) 
	{
		event_data_inputs[i].begin_time = wcf_event_data_inputs[i].begin_time;
		event_data_inputs[i].end_time = wcf_event_data_inputs[i].end_time;
		event_data_inputs[i].lane = wcf_event_data_inputs[i].lane;
		event_data_inputs[i].link = wcf_event_data_inputs[i].link;
		event_data_inputs[i].location = wcf_event_data_inputs[i].location;
		
	}
}

void WCF_to_Host_diversion_data_inputs(DIVERSION_DATA* diversion_data_inputs, array<WCF_DIVERSION_DATA>^ wcf_diversion_data_inputs)
{
	for (int i = 0; i < wcf_diversion_data_inputs->Length; ++i) 
	{
		diversion_data_inputs[i].link = wcf_diversion_data_inputs[i].link;
		diversion_data_inputs[i].begin_time = wcf_diversion_data_inputs[i].begin_time;
		diversion_data_inputs[i].end_time = wcf_diversion_data_inputs[i].end_time;
		diversion_data_inputs[i].location = wcf_diversion_data_inputs[i].location;
		diversion_data_inputs[i].pathid = wcf_diversion_data_inputs[i].pathid;
		diversion_data_inputs[i].percentage = wcf_diversion_data_inputs[i].percentage;
		diversion_data_inputs[i].speed = wcf_diversion_data_inputs[i].speed;
	}
}

void WCF_to_Host_GPS_Ref_Nodes(NODE_LOCATION_DATA *GPS_Ref_Nodes, array<WCF_NODE_LOCATION_DATA>^ wcf_gps_ref_nodes)
{
	for (int i = 0; i < wcf_gps_ref_nodes->Length; i++)
	{
		GPS_Ref_Nodes[i].x = wcf_gps_ref_nodes[i].x;
		GPS_Ref_Nodes[i].y = wcf_gps_ref_nodes[i].y;
		GPS_Ref_Nodes[i].	latitude	=	wcf_gps_ref_nodes[i].	latitude	;
		GPS_Ref_Nodes[i].	longitude	=	wcf_gps_ref_nodes[i].	longitude	;
		GPS_Ref_Nodes[i].	elevation	=	wcf_gps_ref_nodes[i].	elevation	;
		GPS_Ref_Nodes[i].	is_defined	=	wcf_gps_ref_nodes[i].	is_defined	;
	}
}

void DETECTOR_OUTPUTS_to_WCF_DETECTOR_OUTPUTS(DETECTOR_OUTPUTS *sdet_outputs, array<WCF_DETECTOR_OUTPUTS>^ wcf_sdet_outputs)
{
	for (int i = 0; i < wcf_sdet_outputs->Length; i++)
	{
		wcf_sdet_outputs[i].current_count = sdet_outputs[i].current_count;
		wcf_sdet_outputs[i].current_state = sdet_outputs[i].current_state;
		wcf_sdet_outputs[i].previous_state = sdet_outputs[i].previous_state;
		wcf_sdet_outputs[i].hdwy_count = sdet_outputs[i].hdwy_count;
		wcf_sdet_outputs[i].hdwy_total = sdet_outputs[i].hdwy_total;
		wcf_sdet_outputs[i].on_time = sdet_outputs[i].on_time;
		wcf_sdet_outputs[i].speed_total = sdet_outputs[i].speed_total;
		wcf_sdet_outputs[i].length_total = sdet_outputs[i].length_total;
	}
}
void WCF_DETECTOR_OUTPUTS_to_DETECTOR_OUTPUTS(array<WCF_DETECTOR_OUTPUTS>^ wcf_sdet_outputs, DETECTOR_OUTPUTS *sdet_outputs)
{
	for (int i = 0; i < wcf_sdet_outputs->Length; i++)
	{
		sdet_outputs[i].current_count = wcf_sdet_outputs[i].current_count;
		sdet_outputs[i].current_state = wcf_sdet_outputs[i].current_state;
		sdet_outputs[i].previous_state = wcf_sdet_outputs[i].previous_state;
		sdet_outputs[i].hdwy_count = wcf_sdet_outputs[i].hdwy_count;
		sdet_outputs[i].hdwy_total = wcf_sdet_outputs[i].hdwy_total;
		sdet_outputs[i].on_time = wcf_sdet_outputs[i].on_time;
		sdet_outputs[i].speed_total = wcf_sdet_outputs[i].speed_total;
		sdet_outputs[i].length_total = wcf_sdet_outputs[i].length_total;
	}
}



void ProcessFVehicleData(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag)
{
	int nfvehicles = etFommIF->GetNumberOfFVehicles();
	array<WCF_VFData> ^wcf_fvehicle_data = gcnew array<WCF_VFData>(0);
	if (nfvehicles != 0)
	{
		System::Array::Resize(wcf_fvehicle_data, nfvehicles);
		fvehicle_data = (VFData*)calloc(nfvehicles, sizeof(VFData));
		etFommIF->GetFVehicle(fvehicle_data);
#if PRINTOUT
		if (updatedFlag == 0 || updatedFlag == UpdateAll || updatedFlag == UpdateFVehicle)
		    etFommIF->PrintFVehicleData(outputFile, nfvehicles, fvehicle_data);
#endif
		for (int i = 0; i < nfvehicles; i++)
		{
			wcf_fvehicle_data[i].acceleration = fvehicle_data[i].acceleration;
			wcf_fvehicle_data[i].decel = fvehicle_data[i].decel;
			wcf_fvehicle_data[i].desiredspeed = fvehicle_data[i].desiredspeed;
			wcf_fvehicle_data[i].disch_timer = fvehicle_data[i].disch_timer;
			wcf_fvehicle_data[i].drivertype = fvehicle_data[i].drivertype;
			wcf_fvehicle_data[i].entry_link = fvehicle_data[i].entry_link;
			wcf_fvehicle_data[i].entrytime = fvehicle_data[i].entrytime;
			wcf_fvehicle_data[i].ev_dist = fvehicle_data[i].ev_dist;
			wcf_fvehicle_data[i].ev_ovrspd = fvehicle_data[i].ev_ovrspd;
			wcf_fvehicle_data[i].ev_range = fvehicle_data[i].ev_range;
			wcf_fvehicle_data[i].ev_rand = fvehicle_data[i].ev_rand;
			wcf_fvehicle_data[i].ev_wait_timer = fvehicle_data[i].ev_wait_timer;
			wcf_fvehicle_data[i].ev_watch = fvehicle_data[i].ev_watch;
			wcf_fvehicle_data[i].fleet = fvehicle_data[i].fleet;
			wcf_fvehicle_data[i].go_thru_signal = fvehicle_data[i].go_thru_signal;
			wcf_fvehicle_data[i].lag_timer = fvehicle_data[i].lag_timer;
			wcf_fvehicle_data[i].lane = fvehicle_data[i].lane;

			wcf_fvehicle_data[i].lanecodes = gcnew array<int>(N_FREEWAY_LANES);
			for (int j = 0; j < N_FREEWAY_LANES; j++)
			{
				wcf_fvehicle_data[i].lanecodes[j] = fvehicle_data[i].lanecodes[j];
			}
			wcf_fvehicle_data[i].link = fvehicle_data[i].link;
			wcf_fvehicle_data[i].follower = fvehicle_data[i].follower;
			wcf_fvehicle_data[i].id = fvehicle_data[i].id;
			wcf_fvehicle_data[i].last_detid = fvehicle_data[i].last_detid;
			wcf_fvehicle_data[i].lc_timer = fvehicle_data[i].lc_timer;
			wcf_fvehicle_data[i].leader = fvehicle_data[i].leader;
			wcf_fvehicle_data[i].location = fvehicle_data[i].location;
			wcf_fvehicle_data[i].pathid = fvehicle_data[i].pathid;
			wcf_fvehicle_data[i].pathpoint = fvehicle_data[i].pathpoint;
			wcf_fvehicle_data[i].saved_path = fvehicle_data[i].saved_path;
			wcf_fvehicle_data[i].pseudo_leader = fvehicle_data[i].pseudo_leader;
			wcf_fvehicle_data[i].prev_accel = fvehicle_data[i].prev_accel;
			wcf_fvehicle_data[i].prevlink = fvehicle_data[i].prevlink;
			wcf_fvehicle_data[i].prevlane = fvehicle_data[i].prevlane;
			wcf_fvehicle_data[i].routeid = fvehicle_data[i].routeid;
			wcf_fvehicle_data[i].speed = fvehicle_data[i].speed;
			wcf_fvehicle_data[i].speed_adj = fvehicle_data[i].speed_adj;
			wcf_fvehicle_data[i].turncode = fvehicle_data[i].turncode;
			wcf_fvehicle_data[i].vlength = fvehicle_data[i].vlength;
			wcf_fvehicle_data[i].vtype = fvehicle_data[i].vtype;
			wcf_fvehicle_data[i].xcode = fvehicle_data[i].xcode;
			wcf_fvehicle_data[i].will_coop_ev = fvehicle_data[i].will_coop_ev;
			wcf_fvehicle_data[i].will_coop_lc = fvehicle_data[i].will_coop_lc;
			wcf_fvehicle_data[i].will_move = fvehicle_data[i].will_move;
			wcf_fvehicle_data[i].destination = fvehicle_data[i].destination;
			wcf_fvehicle_data[i].distance_to_segment_end = fvehicle_data[i].distance_to_segment_end;
			wcf_fvehicle_data[i].diverted = fvehicle_data[i].diverted;
			wcf_fvehicle_data[i].hov_violator = fvehicle_data[i].hov_violator;
			wcf_fvehicle_data[i].imeter = fvehicle_data[i].imeter;
			wcf_fvehicle_data[i].incident_num = fvehicle_data[i].incident_num;
			wcf_fvehicle_data[i].isegment = fvehicle_data[i].isegment;
			wcf_fvehicle_data[i].must_merge = fvehicle_data[i].must_merge;
			wcf_fvehicle_data[i].next_object = fvehicle_data[i].next_object;
			wcf_fvehicle_data[i].remaining_dist = fvehicle_data[i].remaining_dist;
			wcf_fvehicle_data[i].sorted_list = fvehicle_data[i].sorted_list;
			wcf_fvehicle_data[i].sort_position = fvehicle_data[i].sort_position;
				wcf_fvehicle_data[i].car_following_model = fvehicle_data[i].car_following_model;
			
		}

		free(fvehicle_data);
	}

#if DISPLAY_VEHICLE_INFO_FLAG
	DisplayArrayFVehicles(wcf_fvehicle_data); 
#endif
	proxy->SetServerFVehicleDataSize(wcf_fvehicle_data->Length);
	proxy->SetServerFVehicleData(wcf_fvehicle_data);

	delete wcf_fvehicle_data;
}

void ProcessSVehicleData(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag)
{
	int nsvehicles = etFommIF->GetNumberOfSVehicles();
	array<WCF_VSData> ^wcf_svehicle_data = gcnew array<WCF_VSData>(0);
	if (nsvehicles != 0)
	{
		System::Array::Resize(wcf_svehicle_data, nsvehicles);
		svehicle_data = (VSData*)calloc(nsvehicles, sizeof(VSData));
		etFommIF->GetSVehicle(svehicle_data);
		#if PRINTOUT
		if (updatedFlag == 0   || updatedFlag == UpdateAll || updatedFlag == UpdateSVehicle) 
		    etFommIF->PrintSVehicleData(outputFile, nsvehicles, svehicle_data);
		#endif
		for (int i = 0; i < nsvehicles; i++)
		{
			wcf_svehicle_data[i].acceleration = svehicle_data[i].acceleration;
			wcf_svehicle_data[i].decel = svehicle_data[i].decel;
			wcf_svehicle_data[i].desiredspeed = svehicle_data[i].desiredspeed;
			wcf_svehicle_data[i].disch_timer = svehicle_data[i].disch_timer;
			wcf_svehicle_data[i].drivertype = svehicle_data[i].drivertype;
			wcf_svehicle_data[i].entry_link = svehicle_data[i].entry_link;
			wcf_svehicle_data[i].entrytime = svehicle_data[i].entrytime;
			wcf_svehicle_data[i].ev_dist = svehicle_data[i].ev_dist;
			wcf_svehicle_data[i].ev_ovrspd = svehicle_data[i].ev_ovrspd;
			wcf_svehicle_data[i].ev_range = svehicle_data[i].ev_range;
			wcf_svehicle_data[i].ev_rand = svehicle_data[i].ev_rand;
			wcf_svehicle_data[i].ev_wait_timer = svehicle_data[i].ev_wait_timer;
			wcf_svehicle_data[i].ev_watch = svehicle_data[i].ev_watch;
			wcf_svehicle_data[i].fleet = svehicle_data[i].fleet;
			wcf_svehicle_data[i].go_thru_signal = svehicle_data[i].go_thru_signal;
			wcf_svehicle_data[i].lag_timer = svehicle_data[i].lag_timer;
			wcf_svehicle_data[i].lane = svehicle_data[i].lane;

			wcf_svehicle_data[i].lanecodes = gcnew array<int>(N_STREET_LANES);
			for (int j = 0; j < N_STREET_LANES; j++)
			{
				wcf_svehicle_data[i].lanecodes[j] = svehicle_data[i].lanecodes[j];
			}
			wcf_svehicle_data[i].link = svehicle_data[i].link;
			wcf_svehicle_data[i].follower = svehicle_data[i].follower;
			wcf_svehicle_data[i].id = svehicle_data[i].id;
			wcf_svehicle_data[i].last_detid = svehicle_data[i].last_detid;
			wcf_svehicle_data[i].lc_timer = svehicle_data[i].lc_timer;
			wcf_svehicle_data[i].leader = svehicle_data[i].leader;
			wcf_svehicle_data[i].location = svehicle_data[i].location;
			wcf_svehicle_data[i].pathid = svehicle_data[i].pathid;
			wcf_svehicle_data[i].pathpoint = svehicle_data[i].pathpoint;
			wcf_svehicle_data[i].saved_path = svehicle_data[i].saved_path;
			wcf_svehicle_data[i].pseudo_leader = svehicle_data[i].pseudo_leader;
			wcf_svehicle_data[i].prev_accel = svehicle_data[i].prev_accel;
			wcf_svehicle_data[i].prevlink = svehicle_data[i].prevlink;
			wcf_svehicle_data[i].prevlane = svehicle_data[i].prevlane;
			wcf_svehicle_data[i].routeid = svehicle_data[i].routeid;
			wcf_svehicle_data[i].speed = svehicle_data[i].speed;
			wcf_svehicle_data[i].speed_adj = svehicle_data[i].speed_adj;
			wcf_svehicle_data[i].start_lag = svehicle_data[i].start_lag;
			wcf_svehicle_data[i].turncode = svehicle_data[i].turncode;
			wcf_svehicle_data[i].vlength = svehicle_data[i].vlength;
			wcf_svehicle_data[i].vtype = svehicle_data[i].vtype;
			wcf_svehicle_data[i].xcode = svehicle_data[i].xcode;
			wcf_svehicle_data[i].will_coop_ev = svehicle_data[i].will_coop_ev;
			wcf_svehicle_data[i].will_coop_lc = svehicle_data[i].will_coop_lc;
			wcf_svehicle_data[i].will_move = svehicle_data[i].will_move;
			wcf_svehicle_data[i].diverted = svehicle_data[i].diverted;
			wcf_svehicle_data[i].dwell_timer = svehicle_data[i].dwell_timer;
			wcf_svehicle_data[i].goal_lane = svehicle_data[i].goal_lane;
			wcf_svehicle_data[i].has_stopped = svehicle_data[i].has_stopped;
			wcf_svehicle_data[i].ispdicd = svehicle_data[i].ispdicd;
			wcf_svehicle_data[i].next_stop = svehicle_data[i].next_stop;
			wcf_svehicle_data[i].prvdist = svehicle_data[i].prvdist;
			wcf_svehicle_data[i].prvlink = svehicle_data[i].prvlink;
			wcf_svehicle_data[i].prvlnkicd = svehicle_data[i].prvlnkicd;
			wcf_svehicle_data[i].qstate = svehicle_data[i].qstate;
			wcf_svehicle_data[i].turn_code = svehicle_data[i].turn_code;
			wcf_svehicle_data[i].turn_code2 = svehicle_data[i].turn_code2;
			wcf_svehicle_data[i].turn_link = svehicle_data[i].turn_link;
			wcf_svehicle_data[i].turn_link2 = svehicle_data[i].turn_link2;
			wcf_svehicle_data[i].vehicd = svehicle_data[i].vehicd;
			wcf_svehicle_data[i].will_jump = svehicle_data[i].will_jump;
			wcf_svehicle_data[i].will_yield = svehicle_data[i].will_yield;
				wcf_svehicle_data[i].car_following_model = svehicle_data[i].car_following_model;
				wcf_svehicle_data[i].arc_location = svehicle_data[i].arc_location;
				wcf_svehicle_data[i].arc_entrylink = svehicle_data[i].arc_entrylink;
				wcf_svehicle_data[i].arc_entrylane = svehicle_data[i].arc_entrylane;
			
		}

		free(svehicle_data);
	}

#if DISPLAY_VEHICLE_INFO_FLAG
	DisplayArraySVehicles(wcf_svehicle_data); 
#endif

	proxy->SetServerSVehicleData(wcf_svehicle_data);
	//free(svehicle_data);
	delete wcf_svehicle_data;
}

void ProcessFreewayLinksData(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF, int updatedFlag)
{
	//freeway links
	int n_freeway_links = etFommIF->GetNumberOfFreewayLinks();
	array<Wcf_freeway_link>^ wcf_fwl= gcnew array<Wcf_freeway_link>(n_freeway_links);
	if( n_freeway_links > 0)
	{
		FREEWAY_LINK *freeway_link_data;
		freeway_link_data = (FREEWAY_LINK*)calloc(n_freeway_links, sizeof(FREEWAY_LINK));
		etFommIF->GetFreewayLinks(freeway_link_data);
		#if PRINTOUT
		if (updatedFlag == 0   || updatedFlag == UpdateAll || updatedFlag == UpdateFreewayLink) 
		    etFommIF->PrintFreewayLinkData(outputFile, n_freeway_links, freeway_link_data);
		#endif
		for(int il = 0; il < n_freeway_links; il++)
		{
			wcf_fwl[il].id = il+1;
			
			wcf_fwl[il].usn = freeway_link_data[il].usn;
			wcf_fwl[il].dsn =	freeway_link_data[il].dsn;
			wcf_fwl[il].usn_type =	freeway_link_data[il].usn_type;
			wcf_fwl[il].dsn_type =	freeway_link_data[il].dsn_type;
			wcf_fwl[il].linktype =	freeway_link_data[il].linktype;
			wcf_fwl[il].thrunode = freeway_link_data[il].thrunode;
			wcf_fwl[il].mainline_sending_lane = freeway_link_data[il].mainline_sending_lane;
			wcf_fwl[il].mainline_receiving_lane = freeway_link_data[il].mainline_receiving_lane;
			wcf_fwl[il].offramp_sending_lane = freeway_link_data[il].offramp_sending_lane;
			wcf_fwl[il].offramp_receiving_lane = freeway_link_data[il].offramp_receiving_lane;
			wcf_fwl[il].exitnode = freeway_link_data[il].exitnode;
			wcf_fwl[il].length = freeway_link_data[il].length;
			wcf_fwl[il].fulllanes = freeway_link_data[il].fulllanes;

			wcf_fwl[il].adddrop_code = gcnew array<int>(MAX_ADDDROP_LANE);
			wcf_fwl[il].adddrop_lane = gcnew array<int>(MAX_ADDDROP_LANE);
			wcf_fwl[il].adddrop_dist = gcnew array<int>(MAX_ADDDROP_LANE);
			wcf_fwl[il].adddrop_warn = gcnew array<int>(MAX_ADDDROP_LANE);
			for (int j = 0; j < MAX_ADDDROP_LANE; j++)
			{
				wcf_fwl[il].adddrop_code[j] = freeway_link_data[il].adddrop_code[j];
				wcf_fwl[il].adddrop_lane[j] = freeway_link_data[il].adddrop_lane[j];
				wcf_fwl[il].adddrop_dist[j] = freeway_link_data[il].adddrop_dist[j];
				wcf_fwl[il].adddrop_warn[j] = freeway_link_data[il].adddrop_warn[j];
			}

			wcf_fwl[il].auxlaneid = gcnew array<int>(N_AUXLANES);
			wcf_fwl[il].auxlanecode = gcnew array<int>(N_AUXLANES);
			wcf_fwl[il].auxlanelength = gcnew array<int>(N_AUXLANES);
			for(int iaux = 0; iaux < N_AUXLANES; iaux++)
			{
				wcf_fwl[il].auxlaneid[iaux] = freeway_link_data[il].auxlaneid[iaux];
				wcf_fwl[il].auxlanecode[iaux] = freeway_link_data[il].auxlanecode[iaux];
				wcf_fwl[il].auxlanelength[iaux] = freeway_link_data[il].auxlanelength[iaux];
			}

			wcf_fwl[il].freeflowspeed = freeway_link_data[il].freeflowspeed;
			wcf_fwl[il].thru_percent = freeway_link_data[il].thru_percent;
			wcf_fwl[il].offramp_warn_distance = freeway_link_data[il].offramp_warn_distance;
			wcf_fwl[il].anticip_warning_distance = freeway_link_data[il].anticip_warning_distance;
			wcf_fwl[il].anticip_warning_speed = freeway_link_data[il].anticip_warning_speed;
			wcf_fwl[il].nhov_lanes = freeway_link_data[il].nhov_lanes;
			wcf_fwl[il].hov_begin = freeway_link_data[il].hov_begin;
			wcf_fwl[il].hov_end = freeway_link_data[il].hov_end;
			wcf_fwl[il].hov_code = freeway_link_data[il].hov_code;

			wcf_fwl[il].hov_lanes = gcnew array<int>(MAX_HOV_LANE);
			for (int j = 0; j < MAX_HOV_LANE; j++)
			{
				wcf_fwl[il].hov_lanes[j] = freeway_link_data[il].hov_lanes[j];
			}
			wcf_fwl[il].hov_offramp_warn_distance = freeway_link_data[il].hov_offramp_warn_distance;
			wcf_fwl[il].hov_side = freeway_link_data[il].hov_side;
			wcf_fwl[il].hov_warn = freeway_link_data[il].hov_warn;
			wcf_fwl[il].hov_pct = freeway_link_data[il].hov_pct;
			wcf_fwl[il].cfmult = freeway_link_data[il].cfmult;
			wcf_fwl[il].first_detector = freeway_link_data[il].first_detector;
			wcf_fwl[il].grade = freeway_link_data[il].grade;
			wcf_fwl[il].tilt = freeway_link_data[il].tilt;
			wcf_fwl[il].curve = freeway_link_data[il].curve;
			wcf_fwl[il].pavement = freeway_link_data[il].pavement;
			wcf_fwl[il].shoulder_width = freeway_link_data[il].shoulder_width;
			
			wcf_fwl[il].lane_width = gcnew array<float>(N_FREEWAY_LANES);
			for (int j = 0; j < N_FREEWAY_LANES; j++)
			{
				wcf_fwl[il].lane_width[j] = freeway_link_data[il].lane_width[j];
			}
			wcf_fwl[il].barrier = gcnew array<int>(MAX_BARRIER);
			for (int j = 0; j < MAX_BARRIER; j++)
			{
				wcf_fwl[il].barrier[j] = freeway_link_data[il].barrier[j];
			}
			wcf_fwl[il].datastation_id = freeway_link_data[il].datastation_id;
			wcf_fwl[il].datastation_location = freeway_link_data[il].datastation_location;
			wcf_fwl[il].truck_code = freeway_link_data[il].truck_code;
			wcf_fwl[il].truck_dir = freeway_link_data[il].truck_dir;
			wcf_fwl[il].truck_lane = freeway_link_data[il].truck_lane;
			wcf_fwl[il].etl_warn = freeway_link_data[il].etl_warn;

			wcf_fwl[il].exclude_type = gcnew array<array<int>^>(NTYPES);
			for (int j = 0; j < NTYPES; j++)
			{
				wcf_fwl[il].exclude_type[j] = gcnew array<int>(N_FREEWAY_LANES);
				for (int k = 0; k < N_FREEWAY_LANES; k++)
				{
					wcf_fwl[il].exclude_type[j][k] = freeway_link_data[il].exclude_type[j][k];
				}
			}
			wcf_fwl[il].multiplier_exit = gcnew array<float>(NTYPES);
			for (int j = 0; j < NTYPES; j++)
			{
				wcf_fwl[il].multiplier_exit[j] = freeway_link_data[il].multiplier_exit[j];
			}
			wcf_fwl[il].startup_time = freeway_link_data[il].startup_time;
			wcf_fwl[il].merge_diverge_code = freeway_link_data[il].merge_diverge_code;
		}
		proxy->SetServerFreewayData(wcf_fwl);
		free(freeway_link_data);
	}
	delete wcf_fwl;
}


void ProcessStreetLinksData(std::ofstream &outputFile, IService1^ proxy, bool *change_phase_flag, etFommInterface *etFommIF, int updatedFlag)
{
	//street links
	int n_street_links = 0;
	int nac = 0;
	AC_INPUTS *signal_data;
	//import the delta t
	//float *TIMESTEP = (float*)GetProcAddress(hDLL, "SIMPARAMS_mp_TIMESTEP");
	//import the timestep
	//float CURRENT_SIMTIME = etFommIF->GetCurrentSimTime();

	array<Wcf_street_link>^ wcf_sl = gcnew array<Wcf_street_link>(0);
	array<WCF_AC>^ wcf_acl = gcnew array<WCF_AC>(0); 

	n_street_links = etFommIF->GetNumberOfStreetLinks();
	
	STREET_LINK *street_link_data;//
	if( n_street_links > 0)
	{
		street_link_data = (STREET_LINK*)calloc(n_street_links, sizeof(STREET_LINK));
		int tempstatus = etFommIF->GetStreetLinks(street_link_data);
		#if PRINTOUT
		if (updatedFlag == 0  || updatedFlag == UpdateAll || updatedFlag == UpdateStreetLink)
		    etFommIF->PrintStreetLinkData(outputFile, n_street_links, street_link_data);
		#endif
		System::Array::Resize(wcf_sl, n_street_links);

		for(int il = 0; il < n_street_links; il++)
		{
			wcf_sl[il].id = il+1;
			
			wcf_sl[il].usn = street_link_data[il].usn;
			wcf_sl[il].dsn =	street_link_data[il].dsn;
			wcf_sl[il].usn_type =	street_link_data[il].usn_type;
			wcf_sl[il].dsn_type =	street_link_data[il].dsn_type;
			wcf_sl[il].thrunode = street_link_data[il].thrunode;
			wcf_sl[il].leftnode = street_link_data[il].leftnode;
			wcf_sl[il].rightnode = street_link_data[il].rightnode;
			wcf_sl[il].diagnode = street_link_data[il].diagnode;
			wcf_sl[il].rdiagnode = street_link_data[il].rdiagnode;
			wcf_sl[il].opposenode = street_link_data[il].opposenode;
			wcf_sl[il].length = street_link_data[il].length;
			wcf_sl[il].fulllanes = street_link_data[il].fulllanes;
			wcf_sl[il].leftturnbays = street_link_data[il].leftturnbays;
			wcf_sl[il].rightturnbays = street_link_data[il].rightturnbays;
			wcf_sl[il].freeflowspeed = street_link_data[il].freeflowspeed;

			wcf_sl[il].channelization = gcnew array<int>(N_STREET_LANES);
			wcf_sl[il].laneLength = gcnew array<float>(N_STREET_LANES);
			for (int j = 0; j < N_STREET_LANES; j++)
			{
				wcf_sl[il].channelization[j] = street_link_data[il].channelization[j];
				wcf_sl[il].laneLength[j] = street_link_data[il].laneLength[j];
			}
			wcf_sl[il].leftpct = street_link_data[il].leftpct;
			wcf_sl[il].thrupct = street_link_data[il].thrupct;
			wcf_sl[il].rightpct = street_link_data[il].rightpct;
			wcf_sl[il].diagpct = street_link_data[il].diagpct;
			wcf_sl[il].rdiagpct = street_link_data[il].rdiagpct;
			wcf_sl[il].grade = street_link_data[il].grade;
			wcf_sl[il].distribution_code = street_link_data[il].distribution_code;
			wcf_sl[il].startup_delay = street_link_data[il].startup_delay;
			wcf_sl[il].discharge_hdwy = street_link_data[il].discharge_hdwy;
			wcf_sl[il].rtor = street_link_data[il].rtor;
			wcf_sl[il].ped_code = street_link_data[il].ped_code;
			wcf_sl[il].lane1 = street_link_data[il].lane1;
			wcf_sl[il].lane2 = street_link_data[il].lane2;
			wcf_sl[il].cfmult = street_link_data[il].cfmult;
			wcf_sl[il].sight_dist = street_link_data[il].sight_dist;
			wcf_sl[il].first_detector = street_link_data[il].first_detector;
			wcf_sl[il].shoulder_width = street_link_data[il].shoulder_width;

			wcf_sl[il].lane_width = gcnew array<float>(N_STREET_LANES);
			for (int j = 0; j < N_STREET_LANES; j++)
			{
				wcf_sl[il].lane_width[j] = street_link_data[il].lane_width[j];
			}
			wcf_sl[il].ste_freq = street_link_data[il].ste_freq;
			wcf_sl[il].ste_duration = street_link_data[il].ste_duration;
			wcf_sl[il].signal_range = street_link_data[il].signal_range;
			wcf_sl[il].centroid = street_link_data[il].centroid;
			wcf_sl[il].centroid_label = street_link_data[il].centroid_label;

			wcf_sl[il].exclude_type = gcnew array<array<int>^>(NTYPES);
			for (int j = 0; j < NTYPES; j++)
			{
				wcf_sl[il].exclude_type[j] = gcnew array<int>(N_STREET_LANES);
				for (int k = 0; k < N_STREET_LANES; k++)
				{
					wcf_sl[il].exclude_type[j][k] = street_link_data[il].exclude_type[j][k];
				}
			}
						
			wcf_sl[il].multiplier_left = gcnew array<float>(NTYPES);
			wcf_sl[il].multiplier_thru = gcnew array<float>(NTYPES);
			wcf_sl[il].multiplier_right = gcnew array<float>(NTYPES);
			wcf_sl[il].multiplier_diag = gcnew array<float>(NTYPES);
			wcf_sl[il].multiplier_rdiag = gcnew array<float>(NTYPES);
			for (int j = 0; j < NTYPES; j++)
			{
				wcf_sl[il].multiplier_left[j] = street_link_data[il].multiplier_left[j];
				wcf_sl[il].multiplier_thru[j] = street_link_data[il].multiplier_thru[j];
				wcf_sl[il].multiplier_right[j] = street_link_data[il].multiplier_right[j];
				wcf_sl[il].multiplier_diag[j] = street_link_data[il].multiplier_diag[j];
				wcf_sl[il].multiplier_rdiag[j] = street_link_data[il].multiplier_rdiag[j];
			}
		}
		free(street_link_data);

		//Process actuated control signals
		nac = etFommIF->GetNumberOfACSignals();
		if( nac > 0 )
		{
			signal_data = (AC_INPUTS*)calloc(nac, sizeof(AC_INPUTS));

			etFommIF->GetACSignals(signal_data);
			#if PRINTOUT
			if (updatedFlag == 0   || updatedFlag == UpdateAll || updatedFlag == UpdateStreetLink)
			    etFommIF->PrintACSignals(outputFile, nac, signal_data);
			#endif
			System::Array::Resize(wcf_acl, nac);

			int nMaxApproaches = 10, nPhases = 8, n_detector = 64;
			for(int isig = 0; isig < nac; isig++)
			{
				wcf_acl[isig].node = signal_data[isig].node;
				wcf_acl[isig].cfails = signal_data[isig].cfails;
				wcf_acl[isig].adj = signal_data[isig].adj;
				wcf_acl[isig].cycle_length = signal_data[isig].cycle_length;
				wcf_acl[isig].offset = signal_data[isig].offset;
				wcf_acl[isig].n_direct_approaches = signal_data[isig].n_direct_approaches;
				wcf_acl[isig].n_indirect_approaches = signal_data[isig].n_indirect_approaches;
				wcf_acl[isig].direct_approach_USN = gcnew array<int>(NUMBER_OF_AC_APPROACHES);
				wcf_acl[isig].indirect_approach_USN = gcnew array<int>(5);
				wcf_acl[isig].indirect_approach_DSN = gcnew array<int>(5);
				wcf_acl[isig].leftarrow = gcnew array<array<int>^>(nMaxApproaches);
				wcf_acl[isig].thruarrow = gcnew array<array<int>^>(nMaxApproaches);
				wcf_acl[isig].rightarrow = gcnew array<array<int>^>(nMaxApproaches);
				wcf_acl[isig].diagarrow = gcnew array<array<int>^>(nMaxApproaches);
				for(int iap = 0; iap < 5; iap++)
				{
					wcf_acl[isig].indirect_approach_USN[iap] = signal_data[isig].indirect_approach_USN[iap];
					wcf_acl[isig].indirect_approach_DSN[iap] = signal_data[isig].indirect_approach_DSN[iap];
				}
				for(int iap = 0; iap < NUMBER_OF_AC_APPROACHES; iap++) {
					wcf_acl[isig].direct_approach_USN[iap] = signal_data[isig].direct_approach_USN[iap];
				}
				for(int iap = 0; iap < nMaxApproaches; iap++)
				{					
#if DISPLAY_AC_FLAG
					std::cout << "   approach # " << iap+1 << " is link " << signal_data[isig].direct_approach_USN[iap];
					int lindex = signal_data[isig].direct_approach_USN[iap] - 1;
					std::cout << ", usn = " << street_link_data[lindex].usn << std::endl;
#endif
					wcf_acl[isig].leftarrow[iap] = gcnew array<int>(nPhases);
					wcf_acl[isig].thruarrow[iap] = gcnew array<int>(nPhases);
					wcf_acl[isig].rightarrow[iap] = gcnew array<int>(nPhases);
					wcf_acl[isig].diagarrow[iap] = gcnew array<int>(nPhases);
					for (int iphase = 0; iphase < nPhases; ++iphase)
					{
						wcf_acl[isig].leftarrow[iap][iphase] = signal_data[isig].leftarrow[iap][iphase];
						wcf_acl[isig].thruarrow[iap][iphase] = signal_data[isig].thruarrow[iap][iphase];
						wcf_acl[isig].rightarrow[iap][iphase] = signal_data[isig].rightarrow[iap][iphase];
						wcf_acl[isig].diagarrow[iap][iphase] = signal_data[isig].diagarrow[iap][iphase];
					}
				}

				wcf_acl[isig].	actuated_mode	=	gcnew array<int>(nPhases);
				wcf_acl[isig].	min_green_time	=	gcnew array<float>(nPhases);
				wcf_acl[isig].	max_green_time	=	gcnew array<float>(nPhases);
				wcf_acl[isig].	default_extension_time	=	gcnew array<float>(nPhases);
				wcf_acl[isig].	gap_time	=	gcnew array<float>(nPhases);
				wcf_acl[isig].	times_before_reduction	=	gcnew array<float>(nPhases);
				wcf_acl[isig].	time_to_reduce	=	gcnew array<float>(nPhases);
				wcf_acl[isig].	min_gap_time	=	gcnew array<float>(nPhases);
				wcf_acl[isig].	yellow_change_int	=	gcnew array<float>(nPhases);
				wcf_acl[isig].	red_clear_int	=	gcnew array<float>(nPhases);
				wcf_acl[isig].	ped_allowed	=	gcnew array<int>(nPhases);
				wcf_acl[isig].	walk_time	=	gcnew array<float>(nPhases);
				wcf_acl[isig].	walk_clearance_time	=	gcnew array<float>(nPhases);
				wcf_acl[isig].	force_off_times	=	gcnew array<float>(nPhases);
				wcf_acl[isig].	splits =	gcnew array<float>(nPhases);

				for (int iphase = 0; iphase < nPhases; ++iphase)
				{
					wcf_acl[isig].	actuated_mode	[iphase]	=	signal_data[isig].	actuated_mode	[iphase]	;
					wcf_acl[isig].	min_green_time	[iphase]	=	signal_data[isig].	min_green_time	[iphase]	;
					wcf_acl[isig].	max_green_time	[iphase]	=	signal_data[isig].	max_green_time	[iphase]	;
					wcf_acl[isig].	default_extension_time	[iphase]	=	signal_data[isig].	default_extension_time	[iphase]	;
					wcf_acl[isig].	gap_time	[iphase]	=	signal_data[isig].	gap_time	[iphase]	;
					wcf_acl[isig].	times_before_reduction	[iphase]	=	signal_data[isig].	times_before_reduction	[iphase]	;
					wcf_acl[isig].	time_to_reduce	[iphase]	=	signal_data[isig].	time_to_reduce	[iphase]	;
					wcf_acl[isig].	min_gap_time	[iphase]	=	signal_data[isig].	min_gap_time	[iphase]	;
					wcf_acl[isig].	yellow_change_int	[iphase]	=	signal_data[isig].	yellow_change_int	[iphase]	;
					wcf_acl[isig].	red_clear_int	[iphase]	=	signal_data[isig].	red_clear_int	[iphase]	;
					wcf_acl[isig].	ped_allowed	[iphase]	=	signal_data[isig].	ped_allowed	[iphase]	;
					wcf_acl[isig].	walk_time	[iphase]	=	signal_data[isig].	walk_time	[iphase]	;
					wcf_acl[isig].	walk_clearance_time	[iphase]	=	signal_data[isig].	walk_clearance_time	[iphase]	;
					wcf_acl[isig].	force_off_times[iphase]	=	signal_data[isig].	force_off_times[iphase]	;
					wcf_acl[isig].	splits[iphase]	=	signal_data[isig].	splits[iphase]	;
				}

				wcf_acl[isig].ring_phase = gcnew array<int>(8);
				
				for (int j = 0; j < 4; ++j)
				{
					//wcf_acl[isig].ring_phase[j] = gcnew array<int>(2);
					for (int k = 0; k < 2; ++k)
					{
						wcf_acl[isig].ring_phase[j + k * 4] = signal_data[isig].ring_phase[j + k * 4];
					}
				}

				
				wcf_acl[isig].detector_count = signal_data[isig].detector_count;
				wcf_acl[isig].	detector_list	=	gcnew array<int>(n_detector);

				for (int j = 0; j < n_detector; ++j)
				{
					wcf_acl[isig].detector_list[j] = signal_data[isig].detector_list[j];
				}

				wcf_acl[isig].transition_method = signal_data[isig].transition_method;
				wcf_acl[isig].max_add = signal_data[isig].max_add;
				wcf_acl[isig].max_subtract = signal_data[isig].max_subtract;

				//Extra coordination data apart from AC_INPUTS
#pragma region ExtraCoordinationData
				float local_cycle_timer;
				etFommIF->GetLocalCycleTimer(signal_data[isig].node, &local_cycle_timer);
				wcf_acl[isig].local_cycle_timer = local_cycle_timer;

				float new_cycle_length;
				etFommIF->GetNewCycleLength(signal_data[isig].node, &new_cycle_length);
				wcf_acl[isig].new_cycle_length = new_cycle_length;

				float new_offset;
				etFommIF->GetNewOffset(signal_data[isig].node, &new_offset);
				wcf_acl[isig].new_offset = new_offset;

				float min_splits[8];
				float new_splits[8];
				etFommIF->GetMinSplits(signal_data[isig].node, min_splits);
				etFommIF->GetNewSplits(signal_data[isig].node, new_splits);
				wcf_acl[isig].	min_splits = gcnew array<float>(nPhases);
				wcf_acl[isig].	new_splits = gcnew array<float>(nPhases);
				for (int iphase = 0; iphase < nPhases; ++iphase)
				{
					wcf_acl[isig].	min_splits[iphase]	=	min_splits[iphase];
					wcf_acl[isig].	new_splits[iphase]	=	new_splits[iphase];
				}

				int greens, yellows, iact;
				etFommIF->GetControllerID(signal_data[isig].node, &iact);
				etFommIF->GetETFOMMPhaseStates(iact, &greens, &yellows);
				wcf_acl[isig].	green_phases = greens;
				wcf_acl[isig].	yellow_phases = yellows;
				
#pragma endregion


			}
			free(signal_data);
		}
	}

	proxy->SetServerStreetData(wcf_sl);
	proxy->SetServerACData(wcf_acl);

	delete wcf_sl;
	delete wcf_acl;
}

void ProcessFTCSignals(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF, int updatedFlag)
{
	int Dimension_1 = 12;
	int Dimension_2 = 6;
	int nftc = etFommIF->GetNumberOfFTCSignals();
	if( nftc > 0 )
	{
		FTC_DATA *ftc_signal_data;

		ftc_signal_data = (FTC_DATA*)calloc(nftc, sizeof(FTC_DATA));
		etFommIF->GetFTCSignals(ftc_signal_data);
#if PRINTOUT
		if (updatedFlag == 0   || updatedFlag == UpdateAll 
			|| updatedFlag == UpdateFTCSignal || updatedFlag == FixTimeControlSignalsDataSubmitted)
			etFommIF->PrintFTCSignals(outputFile, nftc, ftc_signal_data);
#endif
		array <WCF_FTC_DATA> ^ wcf_ftcs = gcnew array<WCF_FTC_DATA>(nftc);	
		for(int isig = 0; isig < nftc; isig++)
		{
#if DISPLAY_FTC_FLAG
			cout << "etFomm Fixed Time Signal at node " << ftc_signal_data[isig].node ;
			cout << "  #intervals " << ftc_signal_data[isig].active_intervals ;
			cout << "  #approachs " << ftc_signal_data[isig].approaches <<endl;		
#endif
			wcf_ftcs[isig].active_intervals = ftc_signal_data[isig].active_intervals ;
			wcf_ftcs[isig].approaches = ftc_signal_data[isig].approaches ;

			wcf_ftcs[isig].approach = gcnew array <int>(Dimension_2);
			for (int i = 0; i < Dimension_2; ++i)
			{
				wcf_ftcs[isig].approach[i] = ftc_signal_data[isig].approach[i] ;
			}

			wcf_ftcs[isig].duration = gcnew array <float>(Dimension_1);
			for (int i = 0; i < Dimension_1; ++i)
			{
				wcf_ftcs[isig].duration[i] = ftc_signal_data[isig].duration[i] ;
			}

			
			wcf_ftcs[isig].signal_code = gcnew array<array<int>^>(Dimension_1);
			for (int i = 0; i < Dimension_1; ++i)
			{
				wcf_ftcs[isig].signal_code[i] = gcnew array<int>(Dimension_2);
				for (int j = 0; j < Dimension_2; ++j)
				{
					wcf_ftcs[isig].signal_code[i][j] = ftc_signal_data[isig].signal_code[i][j] ;
				}
			}

			wcf_ftcs[isig].current_interval = ftc_signal_data[isig].current_interval;
			wcf_ftcs[isig].cycle_length = ftc_signal_data[isig].cycle_length;
			wcf_ftcs[isig].external_control = ftc_signal_data[isig].external_control;
			wcf_ftcs[isig].node = ftc_signal_data[isig].node;
			wcf_ftcs[isig].offset = ftc_signal_data[isig].offset;
			wcf_ftcs[isig].range = ftc_signal_data[isig].range;
			wcf_ftcs[isig].time_in_interval = ftc_signal_data[isig].time_in_interval;
		} //for

		proxy->SetServerFTCSignalData(wcf_ftcs);
		free(ftc_signal_data);
		delete wcf_ftcs;
	}
}

void ProcessEntryNodes(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag)
{
	int typedist, erlanga;
	float minsep;
	int n_entrynodes = etFommIF->GetNumberOfEntrynodes();
	if (n_entrynodes > 0)
	{
		array<WCF_ENTRYNODES_DATA>^ wcf_entrynode_data = gcnew array<WCF_ENTRYNODES_DATA>(n_entrynodes);
		ENTRYNODES_DATA *entrynode_data = (ENTRYNODES_DATA*)calloc(n_entrynodes,sizeof(ENTRYNODES_DATA));
		etFommIF->GetEntrynodes(&typedist, &erlanga, &minsep, entrynode_data);
		#if PRINTOUT
		if (updatedFlag == 0 || updatedFlag == UpdateAll  || updatedFlag == UpdateEntryNode)
			etFommIF->PrintEntryNodes(outputFile, n_entrynodes, entrynode_data);
		#endif
		for (int i = 0; i < n_entrynodes; i++)
		{
			wcf_entrynode_data[i].carpool_pct = entrynode_data[i].carpool_pct;
			wcf_entrynode_data[i].flowrate = entrynode_data[i].flowrate;
			wcf_entrynode_data[i].hov_violators_per10000 = entrynode_data[i].hov_violators_per10000;
			wcf_entrynode_data[i].lane_pct = gcnew array<int>(N_ENTRYLANES);
			for (int j = 0; j < N_ENTRYLANES; j++)
			{
				wcf_entrynode_data[i].lane_pct[j] = entrynode_data[i].lane_pct[j];
			}
			wcf_entrynode_data[i].Node_ID = entrynode_data[i].Node_ID;
			wcf_entrynode_data[i].truck_pct = entrynode_data[i].truck_pct;
			wcf_entrynode_data[i].SS_USN = entrynode_data[i].SS_USN;
			wcf_entrynode_data[i].SS_DSN = entrynode_data[i].SS_DSN;
		}
		proxy->SetServerEntryNodeDataSize(wcf_entrynode_data->Length);
		proxy->SetServerEntryNodeData(wcf_entrynode_data);

		free(entrynode_data);
		delete wcf_entrynode_data;
	}
}
void ProcessRampMeter(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag)
{
	int n_rampmeters = etFommIF->GetNumberOfRampmeters();
	if (n_rampmeters > 0)
	{
			array<WCF_RM_DATA>^ wcf_rampmeter_data = gcnew array<WCF_RM_DATA>(n_rampmeters);
		RM_DATA *rampmeter_data = (RM_DATA*)calloc(n_rampmeters, sizeof(RM_DATA));
		etFommIF->GetRampmeters(rampmeter_data);
		#if PRINTOUT
		if (updatedFlag == 0 || updatedFlag == UpdateAll  || updatedFlag == UpdateRampMeter)
			etFommIF->PrintRampMeter(outputFile, n_rampmeters, rampmeter_data);
		#endif
		for (int i = 0; i < n_rampmeters; i++)
		{
			wcf_rampmeter_data[i].dsn = rampmeter_data[i].dsn;
			wcf_rampmeter_data[i].link = rampmeter_data[i].link;
			wcf_rampmeter_data[i].control = rampmeter_data[i].control;
			wcf_rampmeter_data[i].onset = rampmeter_data[i].onset;
			wcf_rampmeter_data[i].state = rampmeter_data[i].state;
		
			wcf_rampmeter_data[i].detector = gcnew array<int>(DETECTOR);
			for (int j = 0; j < DETECTOR; j++)
			{
				wcf_rampmeter_data[i].detector[j] = rampmeter_data[i].detector[j];
			}

			wcf_rampmeter_data[i].capacity = rampmeter_data[i].capacity;

			wcf_rampmeter_data[i].speed = gcnew array<int>(SPEED);
			for (int j = 0; j < SPEED; j++)
			{
				wcf_rampmeter_data[i].speed[j] = rampmeter_data[i].speed[j];
			}

			wcf_rampmeter_data[i].headway = gcnew array<float>(HEADWAY);
			for (int j = 0; j < HEADWAY; j++)
			{
				wcf_rampmeter_data[i].headway[j] = rampmeter_data[i].headway[j];
			}
		
			wcf_rampmeter_data[i].timer = rampmeter_data[i].timer;
			wcf_rampmeter_data[i].updint = rampmeter_data[i].updint;
			wcf_rampmeter_data[i].twopergreen = rampmeter_data[i].twopergreen;
		}
		proxy->SetServerRampmeterInputs(wcf_rampmeter_data);
		free(rampmeter_data);
		delete wcf_rampmeter_data;
	}
}

void UpdateFVehicles(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF)
{
	array<WCF_VFData>^ wcf_fvehicles = proxy->GetServerFVehicleData();
	int n_fvehicles = etFommIF->GetNumberOfFVehicles();
	fvehicle_data = (VFData*)calloc(n_fvehicles, sizeof(VFData));
	etFommIF->GetFVehicle(fvehicle_data);
	if (wcf_fvehicles->Length > n_fvehicles)
	{
		std::cout << "The client has submitted more fvehicles than etFomm." << std::endl;
	}
	else
	{
		for (int i = 0; i < wcf_fvehicles->Length; i++)
		{
			fvehicle_data[i].acceleration = wcf_fvehicles[i].acceleration;
			fvehicle_data[i].decel = wcf_fvehicles[i].decel;
			fvehicle_data[i].desiredspeed = wcf_fvehicles[i].desiredspeed;
			fvehicle_data[i].disch_timer = wcf_fvehicles[i].disch_timer;
			fvehicle_data[i].drivertype = wcf_fvehicles[i].drivertype;
			fvehicle_data[i].entry_link = wcf_fvehicles[i].entry_link;
			fvehicle_data[i].entrytime = wcf_fvehicles[i].entrytime;
			fvehicle_data[i].ev_dist = wcf_fvehicles[i].ev_dist;
			fvehicle_data[i].ev_ovrspd = wcf_fvehicles[i].ev_ovrspd;
			fvehicle_data[i].ev_range = wcf_fvehicles[i].ev_range;
			fvehicle_data[i].ev_rand = wcf_fvehicles[i].ev_rand;
			fvehicle_data[i].ev_wait_timer = wcf_fvehicles[i].ev_wait_timer;
			fvehicle_data[i].ev_watch = wcf_fvehicles[i].ev_watch;
			fvehicle_data[i].fleet = wcf_fvehicles[i].fleet;
			fvehicle_data[i].go_thru_signal = wcf_fvehicles[i].go_thru_signal;
			fvehicle_data[i].lag_timer = wcf_fvehicles[i].lag_timer;
			fvehicle_data[i].lane = wcf_fvehicles[i].lane;
			for (int j = 0; j < MAX_FLANE_CODES; j++)
			{
				fvehicle_data[i].lanecodes[j] = wcf_fvehicles[i].lanecodes[j];
			}
			fvehicle_data[i].link = wcf_fvehicles[i].link;
			fvehicle_data[i].follower = wcf_fvehicles[i].follower;
			fvehicle_data[i].id = wcf_fvehicles[i].id;
			fvehicle_data[i].last_detid = wcf_fvehicles[i].last_detid;
			fvehicle_data[i].lc_timer = wcf_fvehicles[i].lc_timer;
			fvehicle_data[i].leader = wcf_fvehicles[i].leader;
			fvehicle_data[i].location = wcf_fvehicles[i].location;
			fvehicle_data[i].pathid = wcf_fvehicles[i].pathid;
			fvehicle_data[i].pathpoint = wcf_fvehicles[i].pathpoint;
			fvehicle_data[i].saved_path = wcf_fvehicles[i].saved_path;
			fvehicle_data[i].pseudo_leader = wcf_fvehicles[i].pseudo_leader;
			fvehicle_data[i].prev_accel = wcf_fvehicles[i].prev_accel;
			fvehicle_data[i].prevlink = wcf_fvehicles[i].prevlink;
			fvehicle_data[i].prevlane = wcf_fvehicles[i].prevlane;
			fvehicle_data[i].routeid = wcf_fvehicles[i].routeid;
			fvehicle_data[i].speed = wcf_fvehicles[i].speed;
			fvehicle_data[i].speed_adj = wcf_fvehicles[i].speed_adj;
			fvehicle_data[i].turncode = wcf_fvehicles[i].turncode;
			fvehicle_data[i].vlength = wcf_fvehicles[i].vlength;
			fvehicle_data[i].vtype = wcf_fvehicles[i].vtype;
			fvehicle_data[i].xcode = wcf_fvehicles[i].xcode;
			fvehicle_data[i].will_coop_ev = wcf_fvehicles[i].will_coop_ev;
			fvehicle_data[i].will_coop_lc = wcf_fvehicles[i].will_coop_lc;
			fvehicle_data[i].will_move = wcf_fvehicles[i].will_move;
			fvehicle_data[i].destination = wcf_fvehicles[i].destination;
			fvehicle_data[i].distance_to_segment_end = wcf_fvehicles[i].distance_to_segment_end;
			fvehicle_data[i].diverted = wcf_fvehicles[i].diverted;
			fvehicle_data[i].hov_violator = wcf_fvehicles[i].hov_violator;
			fvehicle_data[i].imeter = wcf_fvehicles[i].imeter;
			fvehicle_data[i].incident_num = wcf_fvehicles[i].incident_num;
			fvehicle_data[i].isegment = wcf_fvehicles[i].isegment;
			fvehicle_data[i].must_merge = wcf_fvehicles[i].must_merge;
			fvehicle_data[i].next_object = wcf_fvehicles[i].next_object;
			fvehicle_data[i].remaining_dist = wcf_fvehicles[i].remaining_dist;
			fvehicle_data[i].sorted_list = wcf_fvehicles[i].sorted_list;
			fvehicle_data[i].sort_position = wcf_fvehicles[i].sort_position;

		}
	}
	#if PRINTOUT
	etFommIF->PrintFVehicleData(outputFile, n_fvehicles, fvehicle_data);
	#endif
	etFommIF->SetFVehicle(fvehicle_data);
	free(fvehicle_data);
	proxy->SetClientState(RequestingUpdate);
}

void UpdateSVehicles(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF)
{
	array<WCF_VSData>^ wcf_svehicles = proxy->GetServerSVehicleData();
	int n_svehicles = etFommIF->GetNumberOfSVehicles();
	if (n_svehicles <= 0) return;
	svehicle_data = (VSData*)calloc(n_svehicles, sizeof(VSData));
	etFommIF->GetSVehicle(svehicle_data);
	if (wcf_svehicles->Length > n_svehicles)
	{
		std::cout << "The client has submitted more svehicles than etFomm." << std::endl;
		
	}
	else
	{
		for (int i = 0; i < wcf_svehicles->Length; i++)
		{
			svehicle_data[i].acceleration = wcf_svehicles[i].acceleration;
			svehicle_data[i].decel = wcf_svehicles[i].decel;
			svehicle_data[i].desiredspeed = wcf_svehicles[i].desiredspeed;
			svehicle_data[i].disch_timer = wcf_svehicles[i].disch_timer;
			svehicle_data[i].drivertype = wcf_svehicles[i].drivertype;
			svehicle_data[i].entry_link = wcf_svehicles[i].entry_link;
			svehicle_data[i].entrytime = wcf_svehicles[i].entrytime;
			svehicle_data[i].ev_dist = wcf_svehicles[i].ev_dist;
			svehicle_data[i].ev_ovrspd = wcf_svehicles[i].ev_ovrspd;
			svehicle_data[i].ev_range = wcf_svehicles[i].ev_range;
			svehicle_data[i].ev_rand = wcf_svehicles[i].ev_rand;
			svehicle_data[i].ev_wait_timer = wcf_svehicles[i].ev_wait_timer;
			svehicle_data[i].ev_watch = wcf_svehicles[i].ev_watch;
			svehicle_data[i].fleet = wcf_svehicles[i].fleet;
			svehicle_data[i].go_thru_signal = wcf_svehicles[i].go_thru_signal;
			svehicle_data[i].lag_timer = wcf_svehicles[i].lag_timer;
			svehicle_data[i].lane = wcf_svehicles[i].lane;
			for (int j = 0; j < MAX_SLANE_CODES; j++)
			{
				svehicle_data[i].lanecodes[j] = wcf_svehicles[i].lanecodes[j];
			}
			svehicle_data[i].link = wcf_svehicles[i].link;
			svehicle_data[i].follower = wcf_svehicles[i].follower;
			svehicle_data[i].id = wcf_svehicles[i].id;
			svehicle_data[i].last_detid = wcf_svehicles[i].last_detid;
			svehicle_data[i].lc_timer = wcf_svehicles[i].lc_timer;
			svehicle_data[i].leader = wcf_svehicles[i].leader;
			svehicle_data[i].location = wcf_svehicles[i].location;
			svehicle_data[i].pathid = wcf_svehicles[i].pathid;
			svehicle_data[i].pathpoint = wcf_svehicles[i].pathpoint;
			svehicle_data[i].saved_path = wcf_svehicles[i].saved_path;
			svehicle_data[i].pseudo_leader = wcf_svehicles[i].pseudo_leader;
			svehicle_data[i].prev_accel = wcf_svehicles[i].prev_accel;
			svehicle_data[i].prevlink = wcf_svehicles[i].prevlink;
			svehicle_data[i].prevlane = wcf_svehicles[i].prevlane;
			svehicle_data[i].routeid = wcf_svehicles[i].routeid;
			svehicle_data[i].speed = wcf_svehicles[i].speed;
			svehicle_data[i].speed_adj = wcf_svehicles[i].speed_adj;
			svehicle_data[i].start_lag = wcf_svehicles[i].start_lag;
			svehicle_data[i].turncode = wcf_svehicles[i].turncode;
			svehicle_data[i].vlength = wcf_svehicles[i].vlength;
			svehicle_data[i].vtype = wcf_svehicles[i].vtype;
			svehicle_data[i].xcode = wcf_svehicles[i].xcode;
			svehicle_data[i].will_coop_ev = wcf_svehicles[i].will_coop_ev;
			svehicle_data[i].will_coop_lc = wcf_svehicles[i].will_coop_lc;
			svehicle_data[i].will_move = wcf_svehicles[i].will_move;
			svehicle_data[i].diverted = wcf_svehicles[i].diverted;
			svehicle_data[i].dwell_timer = wcf_svehicles[i].dwell_timer;
			svehicle_data[i].goal_lane = wcf_svehicles[i].goal_lane;
			svehicle_data[i].has_stopped = wcf_svehicles[i].has_stopped;
			svehicle_data[i].ispdicd = wcf_svehicles[i].ispdicd;
			svehicle_data[i].next_stop = wcf_svehicles[i].next_stop;
			svehicle_data[i].prvdist = wcf_svehicles[i].prvdist;
			svehicle_data[i].prvlink = wcf_svehicles[i].prvlink;
			svehicle_data[i].prvlnkicd = wcf_svehicles[i].prvlnkicd;
			svehicle_data[i].qstate = wcf_svehicles[i].qstate;
			svehicle_data[i].turn_code = wcf_svehicles[i].turn_code;
			svehicle_data[i].turn_code2 = wcf_svehicles[i].turn_code2;
			svehicle_data[i].turn_link = wcf_svehicles[i].turn_link;
			svehicle_data[i].turn_link2 = wcf_svehicles[i].turn_link2;
			svehicle_data[i].vehicd = wcf_svehicles[i].vehicd;
			svehicle_data[i].will_jump = wcf_svehicles[i].will_jump;
			svehicle_data[i].will_yield = wcf_svehicles[i].will_yield;
		}
	}
	#if PRINTOUT
	etFommIF->PrintSVehicleData(outputFile, n_svehicles, svehicle_data);
	#endif
	etFommIF->SetSVehicle(svehicle_data);
	free(svehicle_data);
	proxy->SetClientState(RequestingUpdate);
}

void UpdateFreewayLinkData(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF)
{
	array<Wcf_freeway_link>^ wcf_freeway_link_data = proxy->GetServerFreewayData();
	
	int n_freeway_links = wcf_freeway_link_data->Length;
	FREEWAY_LINK *freeway_link_data;
	freeway_link_data = (FREEWAY_LINK*)calloc(n_freeway_links, sizeof(FREEWAY_LINK));
	WCF_to_HOST_freeway_link_data(freeway_link_data, wcf_freeway_link_data);
	#if PRINTOUT
	etFommIF->PrintFreewayLinkData(outputFile, n_freeway_links, freeway_link_data);
	#endif
	etFommIF->SetFreewayLinks(freeway_link_data);
	free(freeway_link_data);
	proxy->SetClientState(RequestingUpdate);
}

void UpdateStreetLinkData(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF)
{
	array<Wcf_street_link>^ wcf_street_link_data = proxy->GetServerStreetData();
	
	int n_street_links = wcf_street_link_data->Length;
	STREET_LINK *street_link_data;
	street_link_data = (STREET_LINK*)calloc(n_street_links, sizeof(STREET_LINK));
	WCF_to_HOST_street_link_data(street_link_data, wcf_street_link_data);
	#if PRINTOUT
	etFommIF->PrintStreetLinkData(outputFile, n_street_links, street_link_data);
	#endif
	etFommIF->SetStreetLinks(street_link_data);
	free(street_link_data);
	proxy->SetClientState(RequestingUpdate);
}

void UpdateACSignals(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF)
{
	array<WCF_AC>^ wcf_ac = proxy->GetServerACData();
	int n_ac = wcf_ac->Length;
	AC_INPUTS* ac_data_inputs = (AC_INPUTS*)calloc(n_ac, sizeof(AC_INPUTS));
	WCF_to_HOST_ac_data_inputs(ac_data_inputs, wcf_ac);
	#if PRINTOUT
	etFommIF->PrintACSignals(outputFile, n_ac,ac_data_inputs);
	#endif
	etFommIF->SetACSignals(ac_data_inputs);
	free(ac_data_inputs);
	proxy->SetClientState(RequestingUpdate);
}

void UpdateFTCSignals(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF)
{
	array<WCF_FTC_DATA>^ wcf_ftc = proxy->GetServerFTCSignalData();
	int n_ftc = wcf_ftc->Length;
	FTC_DATA *ftc_data_inputs = (FTC_DATA*)calloc(n_ftc, sizeof(FTC_DATA));
	WCF_to_HOST_ftc_data_inputs(ftc_data_inputs, wcf_ftc);
	#if PRINTOUT
	etFommIF->PrintFTCSignals(outputFile, n_ftc, ftc_data_inputs);
	#endif
	etFommIF->SetFTCSignals(ftc_data_inputs);
	free(ftc_data_inputs);
	proxy->SetClientState(RequestingUpdate);
}

void UpdateEntryNodes(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF)
{
	int typedist, erlanga;
	float minsep;
	array<WCF_ENTRYNODES_DATA>^ wcf_entry_nodes = proxy->GetServerEntryNodeData();
	int n_entrynodes = wcf_entry_nodes->Length;
	ENTRYNODES_DATA *entrynode_data = (ENTRYNODES_DATA*)calloc(n_entrynodes,sizeof(ENTRYNODES_DATA));
	WCF_to_HOST_entry_node_data(entrynode_data, wcf_entry_nodes);
	#if PRINTOUT
	etFommIF->PrintEntryNodes(outputFile, n_entrynodes, entrynode_data);
	#endif
	etFommIF->SetEntryNodes(typedist, erlanga, minsep, entrynode_data);
	free(entrynode_data);
	proxy->SetClientState(RequestingUpdate);
}

void UpdateRampMeters(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF)
{
	array<WCF_RM_DATA>^ wcf_rampmeter_data = proxy->GetServerRampmeterInputs();
	int n_rampmeters = wcf_rampmeter_data->Length;
	RM_DATA *rampmeter_data = (RM_DATA*)calloc(n_rampmeters, sizeof(RM_DATA));
	WCF_to_HOST_rampmeter_inputs(rampmeter_data, wcf_rampmeter_data);
	#if PRINTOUT
	etFommIF->PrintRampMeter(outputFile, n_rampmeters, rampmeter_data);
	#endif
	etFommIF->SetRampMeters(rampmeter_data);
	free(rampmeter_data);
	proxy->SetClientState(RequestingUpdate);
}


void UpdateNetworkInputs(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF)
{
	array<WCF_NETWORK_INPUTS>^ wcf_network_inputs = proxy->GetServerNetworkInput();
	NETWORK_INPUTS Network_Inputs =	WCF_to_HOST_network_input_data(wcf_network_inputs);
	#if PRINTOUT
	etFommIF->PrintNetworkInputs(outputFile, Network_Inputs);
	#endif
	etFommIF->SetNetworkInputs(Network_Inputs);
	proxy->SetClientState(RequestingUpdate);
}

void UpdateFNetworkInputs(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF)
{
	array<WCF_FREEWAY_NETWORK_INPUTS>^ wcf_fnetwork_inputs = proxy->GetServerFreewayNetworkInput();
	FREEWAY_NETWORK_INPUTS FNetwork_Inputs = WCF_to_HOST_freeway_network_input_data(wcf_fnetwork_inputs);
	#if PRINTOUT
	etFommIF->PrintFNetworkInputs(outputFile, FNetwork_Inputs);
	#endif
	etFommIF->SetFreewayNetworkInputs(FNetwork_Inputs);
	proxy->SetClientState(RequestingUpdate);
}

void UpdateSNetworkInputs(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF)
{
	array<WCF_STREET_NETWORK_INPUTS>^ wcf_snetwork_inputs = proxy->GetServerStreetNetworkInput();
	STREET_NETWORK_INPUTS SNetwork_Inputs = WCF_to_HOST_street_network_input_data(wcf_snetwork_inputs);
	#if PRINTOUT
	etFommIF->PrintSNetworkInputs(outputFile, SNetwork_Inputs);
	#endif
	etFommIF->SetStreetNetworkInputs(SNetwork_Inputs);
	proxy->SetClientState(RequestingUpdate);
}

void UpdateVTypeInputs(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF)
{
	array<WCF_VEHICLE_TYPE_DATA>^ wcf_vtype_inputs = proxy->GetServerVehicleTypeInputs();
	int n_vehicletypes = wcf_vtype_inputs->Length;
	VEHICLE_TYPE_DATA* VType_Inputs = (VEHICLE_TYPE_DATA*)calloc(n_vehicletypes, sizeof(VEHICLE_TYPE_DATA));
	WCF_to_HOST_Vehicle_Type_Inputs(VType_Inputs, wcf_vtype_inputs);
	#if PRINTOUT
	etFommIF->PrintVTypeInputs(outputFile, n_vehicletypes, VType_Inputs);
	#endif
	etFommIF->SetVehicleTypes(VType_Inputs);
	free(VType_Inputs);
	proxy->SetClientState(RequestingUpdate);
}

void UpdateFDetectorInputs(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF)
{
	array<WCF_DETECTOR_INPUTS>^ wcf_det_inputs = proxy->GetServerFreewayDetectorInputs();
	int n_fdet = wcf_det_inputs->Length;
	DETECTOR_INPUTS* FDet_Inputs = (DETECTOR_INPUTS*)calloc(n_fdet, sizeof(DETECTOR_INPUTS));
	WCF_to_HOST_det_inputs(FDet_Inputs, wcf_det_inputs);
	#if PRINTOUT
	etFommIF->PrintDetectorInputs(outputFile, "FREEWAY", n_fdet, FDet_Inputs);
	#endif
	etFommIF->SetFDetectors(FDet_Inputs);
	free(FDet_Inputs);
	proxy->SetClientState(RequestingUpdate);
}

void UpdateSDetectorInputs(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF)
{
	array<WCF_DETECTOR_INPUTS>^ wcf_sdet_inputs = proxy->GetServerStreetDetectorInputs();
	int n_sdet = wcf_sdet_inputs->Length;
	DETECTOR_INPUTS* SDet_Inputs = (DETECTOR_INPUTS*)calloc(n_sdet, sizeof(DETECTOR_INPUTS));
	WCF_to_HOST_det_inputs(SDet_Inputs, wcf_sdet_inputs);
	#if PRINTOUT
	etFommIF->PrintDetectorInputs(outputFile, "STREET", n_sdet, SDet_Inputs);
	#endif
	etFommIF->SetSDetectors(SDet_Inputs);
	free(SDet_Inputs);
	proxy->SetClientState(RequestingUpdate);
}

void UpdateCondTurnpctInputs(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF)
{
	array<WCF_COND_TURNPCTS>^ wcf_ctp_inputs = proxy->GetServerCondTurnpctData();
	int n_turnpct = wcf_ctp_inputs->Length;
	COND_TURNPCTS* cont_turnpct_data = (COND_TURNPCTS*)calloc(n_turnpct, sizeof(COND_TURNPCTS));
	WCF_to_HOST_cond_turnpct_data(cont_turnpct_data, wcf_ctp_inputs);
	#if PRINTOUT
	etFommIF->PrintCondTurnpctInputs(outputFile, n_turnpct, cont_turnpct_data);
	#endif
	etFommIF->SetConditionalTurnpcts(cont_turnpct_data);
	free(cont_turnpct_data);
	proxy->SetClientState(RequestingUpdate);
}

void UpdateBusRouteInputs(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF)
{
	array<WCF_BUSROUTE_DATA>^ wcf_busroute = proxy->GetServerBusRouteInputs();
	int n_busroute = wcf_busroute->Length;
	BUSROUTE_DATA* busroute_inputs = (BUSROUTE_DATA*)calloc(n_busroute, sizeof(BUSROUTE_DATA));
	WCF_to_HOST_busroute_inputs(busroute_inputs, wcf_busroute);
	#if PRINTOUT
	etFommIF->PrintBusRouteInputs(outputFile, n_busroute, busroute_inputs);
	#endif
	etFommIF->SetBusRoutes(busroute_inputs);
	free(busroute_inputs);
	proxy->SetClientState(RequestingUpdate);
}

void UpdateBusStationInputs(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF)
{
	array<WCF_BUSSTATION_DATA>^ wcf_busstation = proxy->GetServerBusStationInputs();
	int n_busstation = wcf_busstation->Length;
	BUSSTATION_DATA* busstation_inputs = (BUSSTATION_DATA*)calloc(n_busstation, sizeof(BUSSTATION_DATA));
	WCF_to_HOST_busstation_inputs(busstation_inputs, wcf_busstation);
	#if PRINTOUT
	etFommIF->PrintBusStationInputs(outputFile, n_busstation, busstation_inputs);
	#endif
	etFommIF->SetBusStations(busstation_inputs);
	free(busstation_inputs);
	proxy->SetClientState(RequestingUpdate);
}

void UpdateIncidentInputs(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF)
{
	array<WCF_INCIDENT_DATA>^ wcf_incidents = proxy->GetServerIncidentData_Inputs();
	int n_incidents = wcf_incidents->Length;
	INCIDENT_DATA* incident_inputs = (INCIDENT_DATA*)calloc(n_incidents, sizeof(INCIDENT_DATA));
	WCF_to_Host_incident_data_inputs(incident_inputs, wcf_incidents);
	#if PRINTOUT
	etFommIF->PrintIncidentInputs(outputFile, n_incidents, incident_inputs);
	#endif
	etFommIF->SetIncidents(incident_inputs);
	free(incident_inputs);
	proxy->SetClientState(RequestingUpdate);
}

void UpdateCoordInputs(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF)
{
	array<WCF_NODE_LOCATION_DATA>^ wcf_xy_coords = proxy->GetServerXYCoordInputs();
	int n_xy_coords = wcf_xy_coords->Length;
	NODE_LOCATION_DATA* xy_coords = (NODE_LOCATION_DATA*)calloc(n_xy_coords, sizeof(NODE_LOCATION_DATA));
	WCF_to_HOST_xy_coord_inputs(xy_coords, wcf_xy_coords);
	#if PRINTOUT
	etFommIF->PrintCoordInputs(outputFile, n_xy_coords, xy_coords);
	#endif
	etFommIF->SetNodeCoordinates(xy_coords);
	free(xy_coords);
	proxy->SetClientState(RequestingUpdate);
}

void UpdateParkingZones(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF)
{
	array<WCF_PARKING_DATA>^ wcf_parking_data_input = proxy->GetServerParkingData();
	int n_parkingzones = wcf_parking_data_input->Length;
	PARKING_DATA *parkingzone_inputs = (PARKING_DATA*)calloc(n_parkingzones, sizeof(PARKING_DATA));
	WCF_to_Host_parking_data_inputs(parkingzone_inputs, wcf_parking_data_input);
	#if PRINTOUT
	etFommIF->PrintParkingZones(outputFile, n_parkingzones, parkingzone_inputs);
	#endif
	etFommIF->SetParkingZones(parkingzone_inputs);
	free(parkingzone_inputs);
	proxy->SetClientState(RequestingUpdate);
}

void UpdateEvents(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF)
{
	array<WCF_EVENT_DATA>^ wcf_event = proxy->GetServerEventData();
	int n_events = wcf_event->Length;
	EVENT_DATA *event_inputs = (EVENT_DATA*)calloc(n_events, sizeof(EVENT_DATA));
	WCF_to_Host_event_data_inputs(event_inputs, wcf_event);
	#if PRINTOUT
	etFommIF->PrintEvents(outputFile, n_events, event_inputs);
	#endif
	etFommIF->SetEvents(event_inputs);
	free(event_inputs);
	proxy->SetClientState(RequestingUpdate);
}

void UpdateDiversionInputs(std::ofstream &outputFile, IService1^ proxy, etFommInterface *etFommIF)
{
	array<WCF_DIVERSION_DATA>^ wcf_diversion = proxy->GetServerDiversionData();
	int n_diversions = wcf_diversion->Length;
	DIVERSION_DATA *diversion_inputs = (DIVERSION_DATA*)calloc(n_diversions, sizeof(DIVERSION_DATA));
	WCF_to_Host_diversion_data_inputs(diversion_inputs, wcf_diversion);
	#if PRINTOUT
	etFommIF->PrintDiversions(outputFile, n_diversions, diversion_inputs);
	#endif
	etFommIF->SetDiversions(diversion_inputs);
	free(diversion_inputs);
	proxy->SetClientState(RequestingUpdate);
}

void ProcessNetworkInputs(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag)
{
	array<WCF_NETWORK_INPUTS>^ wcf_network_inputs = gcnew array<WCF_NETWORK_INPUTS>(1);
	NETWORK_INPUTS Network_Inputs;
	etFommIF->GetNetworkInputs(Network_Inputs);
	#if PRINTOUT
	if (updatedFlag == 0  || updatedFlag == UpdateNInputs)
		etFommIF->PrintNetworkInputs(outputFile, Network_Inputs);
	#endif

	wcf_network_inputs[0].run_init = Network_Inputs.run_init;
	wcf_network_inputs[0].initialization_end = Network_Inputs.initialization_end;
	wcf_network_inputs[0].time_interval = Network_Inputs.time_interval;
	wcf_network_inputs[0].timestep = Network_Inputs.timestep;
	wcf_network_inputs[0].type_of_run = Network_Inputs.type_of_run;
	wcf_network_inputs[0].sim_start_time = Network_Inputs.sim_start_time;
	wcf_network_inputs[0].max_node_number = Network_Inputs.max_node_number;
	wcf_network_inputs[0].time_period_duration = gcnew array<int>(MAX_TIME_PERIOD_DURATION);
	for (int i = 0; i < MAX_TIME_PERIOD_DURATION; ++i)
	{
		wcf_network_inputs[0].time_period_duration[i] = Network_Inputs.time_period_duration[i];
	}

	proxy->SetServerNetworkInput(wcf_network_inputs);
}

void ProcessFNetworkInputs(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag )
{
	array<WCF_FREEWAY_NETWORK_INPUTS>^ wcf_fnetwork_inputs = gcnew array<WCF_FREEWAY_NETWORK_INPUTS>(1);
	FREEWAY_NETWORK_INPUTS FNetwork_Inputs;
	etFommIF->GetFreewayNetworkInputs(FNetwork_Inputs);
	#if PRINTOUT
	if (updatedFlag == 0  || updatedFlag == UpdateFNInputs)
		etFommIF->PrintFNetworkInputs(outputFile, FNetwork_Inputs);
	#endif
	wcf_fnetwork_inputs[0].cfrict = gcnew array<float>(CFRICT);
	for (int i = 0; i < CFRICT; ++i)
	{
		wcf_fnetwork_inputs[0].cfrict[i] = FNetwork_Inputs.cfrict[i];
	}

	wcf_fnetwork_inputs[0].default_hov_pct = FNetwork_Inputs.default_hov_pct;
	wcf_fnetwork_inputs[0].lag_accel = FNetwork_Inputs.lag_accel;
	wcf_fnetwork_inputs[0].lag_decel = FNetwork_Inputs.lag_decel;

	wcf_fnetwork_inputs[0].ffspeed_adj  = gcnew array<float>(MAX_FFSPEED_ADJ);
	for (int i = 0; i < MAX_FFSPEED_ADJ; ++i)
	{
		wcf_fnetwork_inputs[0].ffspeed_adj[i] = FNetwork_Inputs.ffspeed_adj[i];
	}

	wcf_fnetwork_inputs[0].zfoll_pitt  = gcnew array<float>(10);
	wcf_fnetwork_inputs[0].zfoll_idm  = gcnew array<float>(10);
	for (int i = 0; i < 10; ++i)
	{
		wcf_fnetwork_inputs[0].zfoll_pitt[i] = FNetwork_Inputs.zfoll_pitt[i];
		wcf_fnetwork_inputs[0].zfoll_idm[i] = FNetwork_Inputs.zfoll_idm[i];
	}

	wcf_fnetwork_inputs[0].pitt_sep = FNetwork_Inputs.pitt_sep;
	wcf_fnetwork_inputs[0].idm_sep = FNetwork_Inputs.idm_sep;
	wcf_fnetwork_inputs[0].freeway_pct_coop = FNetwork_Inputs.freeway_pct_coop;
	wcf_fnetwork_inputs[0].lc_time = FNetwork_Inputs.lc_time;
	wcf_fnetwork_inputs[0].dlc_mult = FNetwork_Inputs.dlc_mult;

	proxy->SetServerFreewayNetworkInput(wcf_fnetwork_inputs);
}

void ProcessSNetworkInputs(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag)
{
	STREET_NETWORK_INPUTS SNetwork_Inputs;
	etFommIF->GetStreetNetworkInputs(SNetwork_Inputs);
	#if PRINTOUT
	if (updatedFlag == 0  || updatedFlag == UpdateSNInputs)
		etFommIF->PrintSNetworkInputs(outputFile, SNetwork_Inputs);
	#endif
	array<WCF_STREET_NETWORK_INPUTS>^ wcf_snetwork_inputs = gcnew array<WCF_STREET_NETWORK_INPUTS>(1);
	wcf_snetwork_inputs[0].additional_gap = gcnew array<float>(ADDITIONAL_GAP);
	wcf_snetwork_inputs[0].amber_decel = gcnew array<int>(AMBER_DECEL);
	wcf_snetwork_inputs[0].pdelay_weak = gcnew array<int>(PDELAY_WEAK);
	wcf_snetwork_inputs[0].pdelay_strong = gcnew array<int>(PDELAY_STRONG);
	wcf_snetwork_inputs[0].ped_duration = gcnew array<int>(PED_DURATION);
	wcf_snetwork_inputs[0].acceptable_gap = gcnew array<float>(ACCEPTABLE_GAP);
	wcf_snetwork_inputs[0].acceptable_ltg = gcnew array<float>(ACCEPTABLE_LTG);
	wcf_snetwork_inputs[0].acceptable_rtg = gcnew array<float>(ACCEPTABLE_RTG);
	wcf_snetwork_inputs[0].ffspeed_adj = gcnew array<float>(MAX_FFSPEED_ADJ);
	wcf_snetwork_inputs[0].zfoll_pitt = gcnew array<float>(10);
	wcf_snetwork_inputs[0].zfoll_idm = gcnew array<float>(10);
	wcf_snetwork_inputs[0].lt_jumper_prob = gcnew array<float>(N_STREET_LANES);
	wcf_snetwork_inputs[0].lt_lagger_prob = gcnew array<float>(LT_LAGGER_PROB);
	wcf_snetwork_inputs[0].spillback_prob = gcnew array<float>(SPILLBACK_PROB);
	wcf_snetwork_inputs[0].qfactor = gcnew array<float>(QFACTOR);
	wcf_snetwork_inputs[0].ste_mult = gcnew array<float>(STE_MULT);
	wcf_snetwork_inputs[0].turnsignal_prob = gcnew array<float>(10);

	wcf_snetwork_inputs[0].dwell_multiplier = gcnew array<array<float>^>(DWELL_MULTIPLIER);
	for (int i = 0; i < DWELL_MULTIPLIER; ++i)
		 wcf_snetwork_inputs[0].dwell_multiplier[i] = gcnew array<float>(TO_BE_RENAMED3);

	for (int i = 0; i < ADDITIONAL_GAP; ++i)
	{
		wcf_snetwork_inputs[0].additional_gap[i] = SNetwork_Inputs.additional_gap[i];
	}
	for (int i = 0; i < AMBER_DECEL; ++i)
	{
		wcf_snetwork_inputs[0].amber_decel[i] = SNetwork_Inputs.amber_decel[i];
	}

	for (int i = 0; i < PDELAY_WEAK; ++i)
	{
		wcf_snetwork_inputs[0].pdelay_weak[i] = SNetwork_Inputs.pdelay_weak[i];
	}
	for (int i = 0; i < PDELAY_STRONG; ++i)
	{
		wcf_snetwork_inputs[0].pdelay_strong[i] = SNetwork_Inputs.pdelay_strong[i];
	}
	for (int i = 0; i < PED_DURATION; ++i)
	{
		wcf_snetwork_inputs[0].ped_duration[i] = SNetwork_Inputs.ped_duration[i];
	}
	for (int i = 0; i < ACCEPTABLE_GAP; ++i)
	{
		wcf_snetwork_inputs[0].acceptable_gap[i] = SNetwork_Inputs.acceptable_gap[i];
	}
	for (int i = 0; i < ACCEPTABLE_LTG; ++i)
	{
		wcf_snetwork_inputs[0].acceptable_ltg[i] = SNetwork_Inputs.acceptable_ltg[i];
	}
	for (int i = 0; i < ACCEPTABLE_RTG; ++i)
	{
		wcf_snetwork_inputs[0].acceptable_rtg[i] = SNetwork_Inputs.acceptable_rtg[i];
	}
	for (int i = 0; i < MAX_FFSPEED_ADJ; ++i)
	{
		wcf_snetwork_inputs[0].ffspeed_adj[i] = SNetwork_Inputs.ffspeed_adj[i];
	}
	for (int i = 0; i < 10; ++i)
	{
		wcf_snetwork_inputs[0].zfoll_pitt[i] = SNetwork_Inputs.zfoll_pitt[i];
		wcf_snetwork_inputs[0].zfoll_idm[i] = SNetwork_Inputs.zfoll_idm[i];
	}
	for (int i = 0; i < N_STREET_LANES; ++i)
	{
		wcf_snetwork_inputs[0].lt_jumper_prob[i] = SNetwork_Inputs.lt_jumper_prob[i];
	}
	for (int i = 0; i < LT_LAGGER_PROB; ++i)
	{
		wcf_snetwork_inputs[0].lt_lagger_prob[i] = SNetwork_Inputs.lt_lagger_prob[i];
	}
	for (int i = 0; i < SPILLBACK_PROB; ++i)
	{
		wcf_snetwork_inputs[0].spillback_prob[i] = SNetwork_Inputs.spillback_prob[i];
	}
	for (int i = 0; i < QFACTOR; ++i)
	{
		wcf_snetwork_inputs[0].qfactor[i] = SNetwork_Inputs.qfactor[i];
	}
	for (int i = 0; i < STE_MULT; ++i)
	{
		wcf_snetwork_inputs[0].ste_mult[i] = SNetwork_Inputs.ste_mult[i];
	}
	
	for (int i = 0; i < DWELL_MULTIPLIER; ++i)
		for (int j = 0; j < TO_BE_RENAMED3; ++j)
			wcf_snetwork_inputs[0].dwell_multiplier[i][j] = SNetwork_Inputs.dwell_multiplier[i][j];

	for (int i = 0; i < 10; ++i)
	{
		wcf_snetwork_inputs[0].turnsignal_prob[i] = SNetwork_Inputs.turnsignal_prob[i];
	}

	wcf_snetwork_inputs[0].lt_speed = SNetwork_Inputs.lt_speed;
	wcf_snetwork_inputs[0].rt_speed = SNetwork_Inputs.rt_speed;
	wcf_snetwork_inputs[0].pitt_sep = SNetwork_Inputs.pitt_sep;
	wcf_snetwork_inputs[0].idm_sep = SNetwork_Inputs.idm_sep;
	wcf_snetwork_inputs[0].lc_time = SNetwork_Inputs.lc_time;
	wcf_snetwork_inputs[0].stop_spd = SNetwork_Inputs.stop_spd;
	wcf_snetwork_inputs[0].street_pct_coop = SNetwork_Inputs.street_pct_coop;
	wcf_snetwork_inputs[0].yield_spd = SNetwork_Inputs.yield_spd;
	wcf_snetwork_inputs[0].driver_fampct = SNetwork_Inputs.driver_fampct;
	wcf_snetwork_inputs[0].turnsignal_dist = SNetwork_Inputs.turnsignal_dist;

	proxy->SetServerStreetNetworkInput(wcf_snetwork_inputs);
}

void ProcessVTypeInputs(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag)
{
	int n_vehicletypes = etFommIF->GetNumberOfVehicleTypes();
	if (n_vehicletypes > 0)
	{
			VEHICLE_TYPE_DATA *VType_Inputs = (VEHICLE_TYPE_DATA*)calloc(n_vehicletypes, sizeof(VEHICLE_TYPE_DATA));
		etFommIF->GetVehicleTypes(VType_Inputs);
		#if PRINTOUT
		if (updatedFlag == 0 || updatedFlag == UpdateVTypes)
			etFommIF->PrintVTypeInputs(outputFile, n_vehicletypes, VType_Inputs);
		#endif
		array<WCF_VEHICLE_TYPE_DATA>^ wcf_vtype_inputs = gcnew array<WCF_VEHICLE_TYPE_DATA>(n_vehicletypes);

		for (int i = 0; i < n_vehicletypes; ++i)
		{
			wcf_vtype_inputs[i].length = VType_Inputs[i].length;
			wcf_vtype_inputs[i].headway_factor = VType_Inputs[i].headway_factor;
			wcf_vtype_inputs[i].average_occupancy = VType_Inputs[i].average_occupancy;
			wcf_vtype_inputs[i].emergency_decel = VType_Inputs[i].emergency_decel;
			wcf_vtype_inputs[i].fleet_freeway_auto = VType_Inputs[i].fleet_freeway_auto;
			wcf_vtype_inputs[i].fleet_freeway_truck = VType_Inputs[i].fleet_freeway_truck;
			wcf_vtype_inputs[i].fleet_freeway_carpool = VType_Inputs[i].fleet_freeway_carpool;
			wcf_vtype_inputs[i].fleet_freeway_bus = VType_Inputs[i].fleet_freeway_bus;
			wcf_vtype_inputs[i].fleet_freeway_ev = VType_Inputs[i].fleet_freeway_ev;
			wcf_vtype_inputs[i].fleet_freeway_bike = VType_Inputs[i].fleet_freeway_bike;
			wcf_vtype_inputs[i].fleet_street_auto = VType_Inputs[i].fleet_street_auto;
			wcf_vtype_inputs[i].fleet_street_truck = VType_Inputs[i].fleet_street_truck;
			wcf_vtype_inputs[i].fleet_street_carpool = VType_Inputs[i].fleet_street_carpool;
			wcf_vtype_inputs[i].fleet_street_bus = VType_Inputs[i].fleet_street_bus;
			wcf_vtype_inputs[i].fleet_street_ev = VType_Inputs[i].fleet_street_ev;
			wcf_vtype_inputs[i].fleet_street_bike = VType_Inputs[i].fleet_street_bike;
			wcf_vtype_inputs[i].pct_pitt = VType_Inputs[i].pct_pitt;
			wcf_vtype_inputs[i].pct_idm = VType_Inputs[i].pct_idm;
			wcf_vtype_inputs[i].pct_acc = VType_Inputs[i].pct_acc;
			wcf_vtype_inputs[i].pct_cacc = VType_Inputs[i].pct_cacc;
		}

		proxy->SetServerVehicleTypeInputs(wcf_vtype_inputs);
		free(VType_Inputs);
		delete wcf_vtype_inputs;
	}
}

void ProcessDetectorOutputs(int n_det, DETECTOR_OUTPUTS* det_outputs, array<WCF_DETECTOR_OUTPUTS>^ wcf_det_outputs)
{
	for (int i = 0; i < n_det; ++i)
	{
		wcf_det_outputs[i].	current_count	= det_outputs[i].	current_count	;
		wcf_det_outputs[i].	current_state	= det_outputs[i].	current_state	;
		wcf_det_outputs[i].	previous_state	= det_outputs[i].	previous_state	;
		wcf_det_outputs[i].	hdwy_count	= det_outputs[i].	hdwy_count	;
		wcf_det_outputs[i].	hdwy_total	= det_outputs[i].	hdwy_total	;
		wcf_det_outputs[i].	on_time	= det_outputs[i].	on_time	;
		wcf_det_outputs[i].	speed_total	= det_outputs[i].	speed_total	;
		wcf_det_outputs[i].	length_total	= det_outputs[i].	length_total	;
	}
}

void ProcessFDetectorOutputs(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag)
{
	int n_fdet = etFommIF->GetNumberOfFreewayDetectors();
	if (n_fdet > 0)
	{
		DETECTOR_OUTPUTS *Det_Outputs = (DETECTOR_OUTPUTS*)calloc(n_fdet, sizeof(DETECTOR_OUTPUTS));
		etFommIF->GetFreewayDetectorOutputs(Det_Outputs);
		#if PRINTOUT
		//if (updatedFlag == 0 || updatedFlag == UpdateFDetInputs)
		//	etFommIF->PrintDetectorInputs(outputFile, "FREEWAY", n_fdet, FDet_Inputs);
		#endif
		array<WCF_DETECTOR_OUTPUTS>^ wcf_det_outputs = gcnew array<WCF_DETECTOR_OUTPUTS>(n_fdet);
		ProcessDetectorOutputs(n_fdet, Det_Outputs, wcf_det_outputs);
		proxy->SetServerFreewayDetectorOutputs(wcf_det_outputs);
		free(Det_Outputs);
		delete wcf_det_outputs;
	}
}

void ProcessSDetectorOutputs(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag)
{
	int n_sdet = etFommIF->GetNumberOfStreetDetectors();
	if (n_sdet > 0)
	{
		DETECTOR_OUTPUTS *Det_Outputs = (DETECTOR_OUTPUTS*)calloc(n_sdet, sizeof(DETECTOR_OUTPUTS));
		etFommIF->GetStreetDetectorOutputs(Det_Outputs);
		#if PRINTOUT
		//if (updatedFlag == 0 || updatedFlag == UpdateFDetInputs)
		//	etFommIF->PrintDetectorInputs(outputFile, "FREEWAY", n_fdet, FDet_Inputs);
		#endif
		array<WCF_DETECTOR_OUTPUTS>^ wcf_det_outputs = gcnew array<WCF_DETECTOR_OUTPUTS>(n_sdet);
		ProcessDetectorOutputs(n_sdet, Det_Outputs, wcf_det_outputs);
		proxy->SetServerStreetDetectorOutputs(wcf_det_outputs);
		free(Det_Outputs);
		delete wcf_det_outputs;
	}
}

void ProcessCondTurnpctInputs(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag)
{
	
}

void ProcessBusRouteInputs(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag)
{
	int n_busroute = etFommIF->GetNumberOfBusroutes();
	if (n_busroute > 0)
	{
			BUSROUTE_DATA *busroute_inputs = (BUSROUTE_DATA*)calloc(n_busroute, sizeof(BUSROUTE_DATA));
		etFommIF->GetBusroutes(busroute_inputs);
		#if PRINTOUT
		if (updatedFlag == 0 || updatedFlag == UpdateBRInputs)
			etFommIF->PrintBusRouteInputs(outputFile, n_busroute, busroute_inputs);
		#endif
		array<WCF_BUSROUTE_DATA>^ wcf_busroute = gcnew array<WCF_BUSROUTE_DATA>(n_busroute);
		for (int i = 0; i < n_busroute; ++i) 
		{
			wcf_busroute[i].number = busroute_inputs[i].number;
			wcf_busroute[i].hdwy = busroute_inputs[i].hdwy;
			wcf_busroute[i].offset = busroute_inputs[i].offset;
			wcf_busroute[i].nodes = busroute_inputs[i].nodes;

			wcf_busroute[i].route_nodes = gcnew array<int>(ROUTE_NODES);
			for (int j = 0; j < ROUTE_NODES; ++j)
			{
				wcf_busroute[i].route_nodes[j] = busroute_inputs[i].route_nodes[j];
			}

			wcf_busroute[i].stationlist = gcnew array<int>(STATIONLIST);
			for (int j = 0; j < STATIONLIST; ++j)
			{
				wcf_busroute[i].stationlist[j] = busroute_inputs[i].stationlist[j];
			}

			wcf_busroute[i].persontrips = busroute_inputs[i].persontrips;
			wcf_busroute[i].timer = busroute_inputs[i].timer;
			wcf_busroute[i].traveltime = busroute_inputs[i].traveltime;
			wcf_busroute[i].trips = busroute_inputs[i].trips;
		}
		
		proxy->SetServerBusRouteInputs(wcf_busroute);
		free(busroute_inputs);
		delete wcf_busroute;
	}
}

void ProcessBusStationInputs(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag)
{
	
}

void ProcessIncidentInputs(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag)
{
	int n_incidents = etFommIF->GetNumberOfIncidents();
	if (n_incidents > 0)
	{
		INCIDENT_DATA *incident_inputs = (INCIDENT_DATA*)calloc(n_incidents, sizeof(INCIDENT_DATA));
		etFommIF->GetIncidents(incident_inputs);
		#if PRINTOUT
		if (updatedFlag == 0 || updatedFlag == UpdateIncidents)
			etFommIF->PrintIncidentInputs(outputFile, n_incidents, incident_inputs);
		#endif
		array<WCF_INCIDENT_DATA>^ wcf_incident = gcnew array<WCF_INCIDENT_DATA>(n_incidents);

		for (int i = 0; i < n_incidents; ++i)
		{
			wcf_incident[i].link = incident_inputs[i].link;
			wcf_incident[i].begin_point = incident_inputs[i].begin_point;
			wcf_incident[i].begin_time = incident_inputs[i].begin_time;
			wcf_incident[i].end_point = incident_inputs[i].end_point;
			wcf_incident[i].end_time = incident_inputs[i].end_time;
			wcf_incident[i].rbnf = incident_inputs[i].rbnf;
			wcf_incident[i].warn_point = incident_inputs[i].warn_point;
		
			wcf_incident[i].code = gcnew array<int>(N_FREEWAY_LANES);
			for (int j = 0; j < N_FREEWAY_LANES; ++j)
			{
				wcf_incident[i].code[j] = incident_inputs[i].code[j];
			}
		}

		proxy->SetServerIncidentData_Inputs(wcf_incident);
		free(incident_inputs);
		delete wcf_incident;
	}
}

void ProcessCoordInputs(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag)
{
	int n_coords = 8999;
	NODE_LOCATION_DATA *xy_coords = (NODE_LOCATION_DATA*)calloc(n_coords, sizeof(NODE_LOCATION_DATA));
	etFommIF->GetNodeCoordinates(xy_coords);
	#if PRINTOUT
	if (updatedFlag == 0 || updatedFlag == UpdateXYCoords)
		etFommIF->PrintCoordInputs(outputFile, n_coords, xy_coords);
	#endif
	array<WCF_NODE_LOCATION_DATA>^ wcf_xy_coord = gcnew array<WCF_NODE_LOCATION_DATA>(n_coords);

	for (int i = 0; i < n_coords; ++i)
	{
		wcf_xy_coord[i].x = xy_coords[i].x;
		wcf_xy_coord[i].y = xy_coords[i].y;
		wcf_xy_coord[i].latitude = xy_coords[i].latitude;
		wcf_xy_coord[i].longitude = xy_coords[i].longitude;
		wcf_xy_coord[i].elevation = xy_coords[i].elevation;
		wcf_xy_coord[i].is_used = xy_coords[i].is_used;
		wcf_xy_coord[i].is_defined = xy_coords[i].is_defined;
	}
	proxy->SetServerXYCoordInputs(wcf_xy_coord);

	free(xy_coords);
	delete wcf_xy_coord;
}

void ProcessParkingZones(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag)
{
	int n_parkingzones = etFommIF->GetNumberOfParkingZones();
	if (n_parkingzones > 0)
	{
			PARKING_DATA *parkingzone_inputs = (PARKING_DATA*)calloc(n_parkingzones, sizeof(PARKING_DATA));
		etFommIF->GetParkingZones(parkingzone_inputs);
		#if PRINTOUT
		if (updatedFlag == 0 || updatedFlag == UpdateParkZInputs)
			etFommIF->PrintParkingZones(outputFile, n_parkingzones, parkingzone_inputs);
		#endif
		array<WCF_PARKING_DATA>^ wcf_parking_data_input = gcnew array<WCF_PARKING_DATA>(n_parkingzones);

		for (int i = 0; i < n_parkingzones; ++i)
		{
			wcf_parking_data_input[i].link = parkingzone_inputs[i].link;
			wcf_parking_data_input[i].duration = parkingzone_inputs[i].duration;
			wcf_parking_data_input[i].freq = parkingzone_inputs[i].freq;
			wcf_parking_data_input[i].left_start = parkingzone_inputs[i].left_start;
			wcf_parking_data_input[i].left_len = parkingzone_inputs[i].left_len;
			wcf_parking_data_input[i].right_start = parkingzone_inputs[i].right_start;
			wcf_parking_data_input[i].right_len = parkingzone_inputs[i].right_len;
		}

		proxy->SetServerParkingData(wcf_parking_data_input);
		free(parkingzone_inputs);
		delete wcf_parking_data_input;
	}
}

void ProcessEvents(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag)
{
	int n_events = etFommIF->GetNumberOfEvents();
	if (n_events > 0)
	{
		EVENT_DATA *event_inputs = (EVENT_DATA*)calloc(n_events, sizeof(EVENT_DATA));
		etFommIF->GetEvents(event_inputs);
		#if PRINTOUT
		if (updatedFlag == 0 || updatedFlag == UpdateEventInputs)
			etFommIF->PrintEvents(outputFile, n_events, event_inputs);
		#endif
		array<WCF_EVENT_DATA>^ wcf_event = gcnew array<WCF_EVENT_DATA>(n_events);

		for (int i = 0; i < n_events; ++i)
		{
			wcf_event[i].begin_time = event_inputs[i].begin_time;
			wcf_event[i].end_time = event_inputs[i].end_time;
			wcf_event[i].lane = event_inputs[i].lane;
			wcf_event[i].link = event_inputs[i].link;
			wcf_event[i].location = event_inputs[i].location;
			wcf_event[i].speed_reduction = event_inputs[i].speed_reduction;
			wcf_event[i].length = event_inputs[i].length;
			wcf_event[i].code = event_inputs[i].code;
		}

		proxy->SetServerEventData(wcf_event);
		free(event_inputs);
		delete wcf_event;
	}
}

void ProcessDiversionInputs(std::ofstream &outputFile, IService1 ^proxy, etFommInterface *etFommIF, int updatedFlag)
{
	int n_diversion = etFommIF->GetNumberOfDiversions();
	if (n_diversion > 0)
	{
			DIVERSION_DATA *diversion_data = (DIVERSION_DATA*)calloc(n_diversion, sizeof(DIVERSION_DATA));
		etFommIF->GetDiversions(diversion_data);
		#if PRINTOUT
		if (updatedFlag == 0 || updatedFlag == UpdateDiverInputs)
			etFommIF->PrintDiversions(outputFile, n_diversion, diversion_data);
		#endif
		array<WCF_DIVERSION_DATA>^ wcf_diversion_data = gcnew array<WCF_DIVERSION_DATA>(n_diversion);

		for (int i = 0; i < n_diversion; ++i)
		{
			wcf_diversion_data[i].link = diversion_data[i].link;
			wcf_diversion_data[i].begin_time = diversion_data[i].begin_time;
			wcf_diversion_data[i].end_time = diversion_data[i].end_time;
			wcf_diversion_data[i].location = diversion_data[i].location;
			wcf_diversion_data[i].pathid = diversion_data[i].pathid;
			wcf_diversion_data[i].percentage = diversion_data[i].percentage;
			wcf_diversion_data[i].speed = diversion_data[i].speed;
		}

		proxy->SetServerDiversionData(wcf_diversion_data);
		free(diversion_data);
		delete wcf_diversion_data;
	}
}

int APIProcessClientRequest(IService1^ proxy, etFommInterface *etFommIF, 
							const std::string& dataPath, const std::string& fileName, 
							int &updatedFlag, std::string &prefix)
{
	//no means no API request
	//int APIFlag = 1;
	int PauseFlag = 1;

#pragma region coordination_data
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
	array <int>^ wcf_greens_yellows = gcnew array<int>(2);
	int greens = 0;
	int yellows = 0;
	int iact = 0;
	int transition_method;
#pragma endregion
	//for average travel time
	int usn_id, dsn_id;
	float average_travel_time;

	proxy->SetHostState(1);
	bool exit = false;
	std::ofstream outputFile3;
	std::string tmpName;
	updatedFlag = 0;
	prefix = "";

#if _USE_TCA
	proxy->SetTCAState(StepSimulationDone);
	while ( (proxy->GetTCAState() != GoodForNextSimulationStep) && (proxy->GetTCAState() != TCAContinued) && (proxy->GetTCAState() != SimulationFinished) )
	{
	}
#endif

	std::cout << endl<< "Waiting for API inputs..." << std::endl;

	while(!exit)
	{
		int state  = proxy->GetClientState();
		switch (state) 
		{
		case UpdateAll:
			std::cout << endl<< "  API inputs UpdateAll received..." << std::endl;
			
			#if PRINTOUT
			prefix = "MOD_";
			tmpName = dataPath + prefix + fileName + ".dat";
			outputFile3.open(tmpName.c_str());
			#endif

			UpdateFVehicles(outputFile3, proxy, etFommIF);
			UpdateSVehicles(outputFile3, proxy, etFommIF);
			UpdateFreewayLinkData(outputFile3, proxy, etFommIF);
			UpdateStreetLinkData(outputFile3, proxy, etFommIF);
			UpdateACSignals(outputFile3, proxy, etFommIF);
			UpdateFTCSignals(outputFile3, proxy, etFommIF);
			UpdateRampMeters(outputFile3, proxy, etFommIF);
			#if PRINTOUT
			outputFile3.close();
			#endif
			updatedFlag = state;
			exit = true;
			break;

		case NoRequest:
			std::cout << endl<< "  API inputs received..." << std::endl;
			//APIFlag = 0;			
			PauseFlag = 0;
			exit = true;
			break;	
		case APIShutDown:
			std::cout << endl<< "  API is shut down." << std::endl << std::endl;
			//APIFlag = 0;			
			PauseFlag = 0;
			exit = true;
			break;
		case VehicleUpdateSubmitted:
			std::cout << endl<< "  API inputs received..." << std::endl;
			etFommUpdateVehicles(proxy, etFommIF);	
			exit = true;
			break;		
		case AddPathSubmitted:
			std::cout << endl<< "  API inputs received..." << std::endl;
			AddPath(proxy, etFommIF);	
			exit = true;
			break;		
		case AddVehicleSubmitted:
			std::cout << endl<< "  API inputs received..." << std::endl;
			AddNewVehicle(proxy, etFommIF);
			exit = true;
			break;	
		case SignalsDataSubmitted:
			std::cout << endl<< "  API inputs received..." << std::endl;	
			exit = true;
			break;	
		case FixTimeControlSignalsDataSubmitted:
			std::cout << endl<< "  API inputs received..." << std::endl;
			updateFTCSignals(proxy, etFommIF);
			exit = true;
			break;					
		case Continued:
			std::cout << endl<< "  Continued..." << std::endl;
			//after receiving the client update, reset state to RequestingUpdate
			proxy->SetClientState(RequestingUpdate);
			exit = true;
			break;
		case UseHostDefineNetwork:
			std::cout << endl<< "  API inputs received..." << std::endl;
			exit = true;
			break;
		case UseClientDefineNetwork:
			std::cout << endl<< "  API inputs received..." << std::endl;
			exit = true;
			break;
		case UseTRFFile:
			std::cout << endl<< "  API inputs received..." << std::endl;
			exit = true;
			break;
		case GetCoordinationData:
			std::cout << endl<< "  API inputs received..." << std::endl;
			std::cout << "  Getting coordination data..." << std::endl;
			ProcessCoordinationData(proxy, etFommIF);
			proxy->SetClientState(GotCoordinationData);
			std::cout << endl<< "Waiting for API inputs..." << std::endl;
			//exit = true;
			break;
		case SetCoordinationData:
			std::cout << endl<< "  API inputs received..." << std::endl;
			std::cout << "  Setting coordination data..." << std::endl;
			UpdateCoordinationData(proxy, etFommIF);
			proxy->SetClientState(SetCoordinationDataDone);
			std::cout << endl<< "Waiting for API inputs..." << std::endl;
			//exit = true;
			break;
		case SetNewCycleLength:
			std::cout << endl<< "  API inputs received..." << std::endl;
			node_id = proxy->GetServerNodeID();
			new_cycle_length = proxy->GetServerNewCycleLength();
			std::cout << "  Setting new_cycle_length for node " << node_id << " to " << new_cycle_length <<std::endl;
			etFommIF->SetNewCycleLength(node_id, new_cycle_length);
			proxy->SetClientState(SetCoordinationDataDone);
			std::cout << endl<< "Waiting for API inputs..." << std::endl;
			break;
		case GetNewCycleLength:
			std::cout << endl << "  API inputs received..." << std::endl;
			node_id = proxy->GetServerNodeID();
			std::cout << "  Getting new_cycle_length for node " << node_id << std::endl;
			etFommIF->GetNewCycleLength(node_id, &new_cycle_length);
			std::cout << "new_cycle_length = " << new_cycle_length << std::endl;
			proxy->SetServerNewCycleLength(node_id, new_cycle_length);
			proxy->SetClientState(GotCoordinationData);
			std::cout << endl<< "Waiting for API inputs..." << std::endl;
			break;
		case GetCycleLength:
			std::cout << endl << "  API inputs received..." << std::endl;
			node_id = proxy->GetServerNodeID();
			std::cout << "  Getting cycle_length for node " << node_id << std::endl;
			etFommIF->GetCycleLength(node_id, &cycle_length);
			std::cout << "cycle_length = " << cycle_length << std::endl;
			proxy->SetServerCycleLength(node_id, cycle_length);
			proxy->SetClientState(GotCoordinationData);
			std::cout << endl<< "Waiting for API inputs..." << std::endl;
			break;

		case SetNewOffset:
			std::cout << endl << "  API inputs received..." << std::endl;
			node_id = proxy->GetServerNodeID();
			new_offset = proxy->GetServerNewOffset(); 
			std::cout << "  Setting new_offset for node " << node_id << " to " << new_offset <<std::endl;
			etFommIF->SetNewOffset(node_id, new_offset);
			proxy->SetClientState(SetCoordinationDataDone);
			std::cout << endl<< "Waiting for API inputs..." << std::endl;
			break;
		case GetNewOffset:
			std::cout << endl << "  API inputs received..." << std::endl;
			node_id = proxy->GetServerNodeID();
			std::cout << "  Getting new_offset for node " << node_id << std::endl;
			etFommIF->GetNewOffset(node_id, &new_offset);
			std::cout << "new_offset = " << new_offset << std::endl;
			proxy->SetServerNewOffset(node_id, new_offset);
			proxy->SetClientState(GotCoordinationData);
			std::cout << endl<< "Waiting for API inputs..." << std::endl;
			break;
		case GetOffset:
			std::cout << endl << "  API inputs received..." << std::endl;
			node_id = proxy->GetServerNodeID();
			std::cout << "  Getting offset for node " << node_id << std::endl;
			etFommIF->GetOffset(node_id, &offset);
			std::cout << "offset = " << offset << std::endl;
			proxy->SetServerOffset(node_id, offset);
			proxy->SetClientState(GotCoordinationData);
			std::cout << endl<< "Waiting for API inputs..." << std::endl;
			break;

		case GetLocalCycleTimer:
			std::cout << endl << "  API inputs received..." << std::endl;
			node_id = proxy->GetServerNodeID();
			std::cout << "  Getting local_cycle_timer for node " << node_id << std::endl;
			etFommIF->GetLocalCycleTimer(node_id, &local_cycle_timer);
			std::cout << "local_cycle_timer = " << local_cycle_timer << std::endl;
			proxy->SetServerLocalCycleTimer(node_id, local_cycle_timer);
			proxy->SetClientState(GotCoordinationData);
			std::cout << endl<< "Waiting for API inputs..." << std::endl;
			break;

		case SetNewSplits:
			std::cout << endl<< "  API inputs received..." << std::endl;
			node_id = proxy->GetServerNodeID();
			wcf_new_splits = proxy->GetServerNewSplits();
			std::cout << "  Setting new_splits for node " << node_id << " to: " << std::endl;
			for (int i = 0; i < 8; ++i)
			{
				new_splits[i] = wcf_new_splits[i];
				std::cout << new_splits[i] << " ";
			}
			etFommIF->SetNewSplits(node_id, new_splits);
			proxy->SetClientState(SetCoordinationDataDone);
			std::cout << endl<< "Waiting for API inputs..." << std::endl;
			break;
		case GetNewSplits:
			std::cout << endl << "  API inputs received..." << std::endl;
			node_id = proxy->GetServerNodeID();
			std::cout << "  Getting new_splits for node " << node_id << std::endl;
			etFommIF->GetNewSplits(node_id, new_splits);
			std::cout << "new_splits: " << std::endl;
			for (int i = 0; i < 8; i++)
			{
				wcf_new_splits[i] = new_splits[i];
				std::cout << new_splits[i] << " ";
			}
			proxy->SetServerNewSplits(node_id, wcf_new_splits);
			proxy->SetClientState(GotCoordinationData);
			std::cout << endl<< "Waiting for API inputs..." << std::endl;
			break;
		case GetSplits:
			std::cout << endl << "  API inputs received..." << std::endl;
			node_id = proxy->GetServerNodeID();
			std::cout << "  Getting splits for node " << node_id << std::endl;
			etFommIF->GetSplits(node_id, splits);
			std::cout << "splits: " << std::endl;
			for (int i = 0; i < 8; i++)
			{
				wcf_splits[i] = splits[i];
				std::cout << splits[i] << " ";
			}
			proxy->SetServerSplits(node_id, wcf_splits);
			proxy->SetClientState(GotCoordinationData);
			std::cout << endl<< "Waiting for API inputs..." << std::endl;
			break;

		case GetMinSplits:
			std::cout << endl << "  API inputs received..." << std::endl;
			node_id = proxy->GetServerNodeID();
			std::cout << "  Getting min_splits for node " << node_id << std::endl;
			etFommIF->GetMinSplits(node_id, min_splits);
			std::cout << "min_splits: " << std::endl;
			for (int i = 0; i < 8; i++)
			{
				wcf_min_splits[i] = min_splits[i];
				std::cout << min_splits[i] << " ";
			}
			proxy->SetServerMinSplits(node_id, wcf_min_splits);
			proxy->SetClientState(GotCoordinationData);
			std::cout << endl<< "Waiting for API inputs..." << std::endl;
			break;

		case SetNodeGYR:
			std::cout << endl<< "  API inputs received..." << std::endl;
			node_id = proxy->GetServerNodeID();
			wcf_greens_yellows = proxy->GetServerNodeGYR();
			std::cout << "  Setting GYR for node " << node_id << " to greens:" << wcf_greens_yellows[0] << ", yellows: " << wcf_greens_yellows[1] << std::endl;
			etFommIF->GetControllerID(node_id, &iact);
			greens = wcf_greens_yellows[0];
			yellows = wcf_greens_yellows[1];
			std::cout << "etFommIF->SetETFOMMPhaseStates(" << node_id << ", " << greens << ", " << yellows << ");" << std::endl;
			etFommIF->SetETFOMMPhaseStates(node_id, &greens, &yellows);
			proxy->SetClientState(SetCoordinationDataDone);
			std::cout << endl<< "Waiting for API inputs..." << std::endl;
			break;
		case GetNodeGYR:
			std::cout << endl << "  API inputs received..." << std::endl;
			node_id = proxy->GetServerNodeID();
			std::cout << "  Getting GYR for node " << node_id << std::endl;
			etFommIF->GetControllerID(node_id, &iact);
			etFommIF->GetETFOMMPhaseStates(iact, &greens, &yellows);
			wcf_greens_yellows[0] = greens;
			wcf_greens_yellows[1] = yellows;
			std::cout << "  greens:" << wcf_greens_yellows[0] << ", yellows: " << wcf_greens_yellows[1] << std::endl;
			proxy->SetServerNodeGYR(node_id, wcf_greens_yellows);
			proxy->SetClientState(GotCoordinationData);
			std::cout << endl<< "Waiting for API inputs..." << std::endl;
			break;

		case GetAverageTravelTime:
			std::cout << endl << "  API inputs received..." << std::endl;
			usn_id = proxy->GetServerUSNID();
			dsn_id = proxy->GetServerDSNID();
			average_travel_time = etFommIF->GetStreetTravelTime(usn_id, dsn_id);
			std::cout << "average_travel_time = " << average_travel_time <<std::endl;
			proxy->SetServerAverageTravelTime(usn_id, dsn_id, average_travel_time);
			proxy->SetClientState(GotAverageTravelTime);
			std::cout << endl<< "Waiting for API inputs..." << std::endl;
			break;
		case GetStreetLaneMOEData:
			{
				std::cout << endl << "  API inputs received..." << std::endl;
				array<WCF_STREET_MOE_DATA>^ wcf_street_moe_data = gcnew array<WCF_STREET_MOE_DATA>(1);

				wcf_street_moe_data = proxy->GetServerStreetMOEData();
				usn_id = wcf_street_moe_data[0].usn_id;
				dsn_id = wcf_street_moe_data[0].dsn_id;
				int lane_id = wcf_street_moe_data[0].lane_id;
				int flag = wcf_street_moe_data[0].flag;
				std::string str_MOEString;
				ConvertString(wcf_street_moe_data[0].MOEString, str_MOEString);

				float MOEValue = etFommIF->GetStreetLaneMOEData(usn_id, dsn_id, lane_id, flag, str_MOEString);
				std::cout << "MOE: " << str_MOEString << " = " << MOEValue <<std::endl;

				wcf_street_moe_data[0].MOEValue = MOEValue;
				proxy->SetServerStreetMOEData(wcf_street_moe_data);
				proxy->SetClientState(GotStreetLaneMOEData);

				std::cout << endl<< "Waiting for API inputs..." << std::endl;
				break;
			}
		case GetStreetLinkMOEData:
			{
				std::cout << endl << "  API inputs received..." << std::endl;
				array<WCF_STREET_MOE_DATA>^ wcf_street_moe_data = gcnew array<WCF_STREET_MOE_DATA>(1);

				wcf_street_moe_data = proxy->GetServerStreetMOEData();
				usn_id = wcf_street_moe_data[0].usn_id;
				dsn_id = wcf_street_moe_data[0].dsn_id;
				//int lane_id = wcf_street_moe_data[0].lane_id;
				int flag = wcf_street_moe_data[0].flag;
				std::string str_MOEString;
				ConvertString(wcf_street_moe_data[0].MOEString, str_MOEString);

				float MOEValue = etFommIF->GetStreetLinkMOEData(usn_id, dsn_id, /*lane_id,*/ flag, str_MOEString);
				std::cout << "MOE: " << str_MOEString << " = " << MOEValue <<std::endl;

				wcf_street_moe_data[0].MOEValue = MOEValue;
				proxy->SetServerStreetMOEData(wcf_street_moe_data);
				proxy->SetClientState(GotStreetLinkMOEData);

				std::cout << endl<< "Waiting for API inputs..." << std::endl;
				break;
			}
		default: 
			//proxy->SetClientState(RequestingUpdate);
			exit = false;
		}
	} //while
	//return APIFlag;
	proxy->SetHostState(0);
	return PauseFlag;
}

void ConvertString(String^ src, std::string& des)
{
	using namespace Runtime::InteropServices;
	const char* chars = 
		(const char*)(Marshal::StringToHGlobalAnsi(src)).ToPointer();
	des = std::string(chars);
}

//No longer used
void ProcessACData(etFommInterface *etFommIF)
{
	int nac = etFommIF->GetNumberOfACSignals();
	if( nac > 0 )
	{
		AC_INPUTS *signal_data = (AC_INPUTS*)calloc(nac, sizeof(AC_INPUTS));

		etFommIF->GetACSignals(signal_data);
		int scopeStatus = etFommIF->ReadSCOPEInputs(signal_data, nac);
		if (scopeStatus > 0)
		{
			switch(scopeStatus)
			{
			case 1:
				std::cout << "No SCOPE_Input file." << std::endl;
				break;
			case 2:
				std::cout << "Wrong Format of SCOPE_Input file." << std::endl;
				break;
			case 3:
				std::cout << "Wrong Number of AC Signals in SCOPE_Input file." << std::endl;
				break;
			}
		} else
		{
			FillACData(nac, signal_data, etFommIF);

			etFommIF->SetNumberOfACSignals(nac);			
			etFommIF->SetACSignals(signal_data);
		}
		free(signal_data);
	}
}

void FillACData(int nacs, AC_INPUTS *signal_data, etFommInterface *etFommIF)
{
	int n_street_links = etFommIF->GetNumberOfStreetLinks();
	if( n_street_links > 0)
	{
		STREET_LINK *street_link_data = (STREET_LINK*)calloc(n_street_links, sizeof(STREET_LINK));
		etFommIF->GetStreetLinks(street_link_data);
		for (int iac = 0; iac < nacs; ++iac)	
		{
			AC_INPUTS *cur_data = &(signal_data[iac]);

			cur_data->cycle_length = 90;
			cur_data->offset = 0;
			cur_data->transition_method = 1;
			cur_data->max_add = 10.;
			cur_data->max_subtract = 11.;
			cur_data->force_off_times[0] = 20.;
			cur_data->force_off_times[1] = 0.;
			cur_data->force_off_times[2] = 20.;
			cur_data->force_off_times[3] = 20.;
			cur_data->force_off_times[4] = 20.;
			cur_data->force_off_times[5] = 0.;
			cur_data->force_off_times[6] = 20.;
			cur_data->force_off_times[7] = 20.;

			cur_data->ring_phase[0] = 1;
			cur_data->ring_phase[1] = 2;
			cur_data->ring_phase[2] = 3;
			cur_data->ring_phase[3] = 4;
			cur_data->ring_phase[4] = 5;
			cur_data->ring_phase[5] = 6;
			cur_data->ring_phase[6] = 7;
			cur_data->ring_phase[7] = 8;

			int dsnID = cur_data->node;
			for (int iapp = 0; iapp < cur_data->n_direct_approaches; ++iapp)
			{
				int usnID = cur_data->direct_approach_USN[iapp];
				for (int ilink = 0; ilink < n_street_links; ++ilink)
				{
					if (street_link_data[ilink].usn == usnID && street_link_data[ilink].dsn == dsnID)
					{
						cur_data->direct_approach_USN[iapp] = street_link_data[ilink].id;
						break;
					}
				}
			}
		}
		free(street_link_data);	
	}
}

void SetNetworkInputs(etFommInterface *etFommIF)
{
	NETWORK_INPUTS Network_Inputs;
    for(int i = 0; i<19; i++)
    {
        Network_Inputs.time_period_duration[i] = 0;
    }
    Network_Inputs.time_period_duration[0] = 1;
	Network_Inputs.type_of_run = 1; 
	etFommIF->SetNetworkInputs(Network_Inputs);
}

//for Coordination data
void ProcessCoordinationData(IService1^ proxy, etFommInterface *etFommIF)
{
	int error;
	array<WCF_COORDINATION_DATA>^ wcf_coordination_data = proxy->GetServerCoordinationData();
	wcf_coordination_data[0].splits = gcnew array<float>(8);
	wcf_coordination_data[0].min_splits = gcnew array<float>(8);
	int node = wcf_coordination_data[0].node;
	float local_cycle_timer, cycle_length, offset, max_add, max_subt;
	int iact, method;
	float splits[8];
	float min_splits[8];

	error = etFommIF->GetControllerID(node, &iact);
	
	if(error == 0 && iact > 0)
	{
		error = etFommIF->GetLocalCycleTimer(node, &local_cycle_timer);
		error = etFommIF->GetMinSplits(node, min_splits);

		error = etFommIF->GetTransitionMethod(node, &method, &max_add, &max_subt);
		// Returns: int =  0 - short way transition method
		//              =  1 - dwell transition method
		//              =  2 - add transition method
		//              =  3 - subtract transition method
		//              = -1 - specified node is not under actuated control

		//if (wcf_coordination_data[0].coord_set != true)
		{
			error = etFommIF->GetCycleLength(node, &cycle_length);
			//error = etFommIF->GetNewCycleLength(node, &cycle_length);
			error = etFommIF->GetOffset(node, &offset);
			//error = etFommIF->GetNewOffset(node, &offset);
			error = etFommIF->GetSplits(node, splits);
			//error = etFommIF->GetNewSplits(node, splits);
		}
		
		wcf_coordination_data[0].local_cycle_timer = local_cycle_timer;
		wcf_coordination_data[0].cycle_length = cycle_length;
		wcf_coordination_data[0].offset = offset;
		wcf_coordination_data[0].max_add = max_add;
		wcf_coordination_data[0].max_subt = max_subt;
		wcf_coordination_data[0].iact = iact;
		wcf_coordination_data[0].method = method;
		
		for (int i = 0; i < 8; ++i)
		{
			wcf_coordination_data[0].splits[i] = splits[i];
			wcf_coordination_data[0].min_splits[i] = min_splits[i];
		}
		
	}
	else
	{
		cout<<"\nnot getting correct coordination data in wcf_server_coordination_data,\nplease check the node ID..."<<endl;
	}

	proxy->SetServerCoordinationData(wcf_coordination_data);
	proxy->SetClientState(GotCoordinationData);
}
void UpdateCoordinationData(IService1^ proxy, etFommInterface *etFommIF)
{
	int error;
	array<WCF_COORDINATION_DATA>^ wcf_coordination_data = proxy->GetServerCoordinationData();
	int node = wcf_coordination_data[0].node;
	float local_cycle_timer;
	float cycle_length = wcf_coordination_data[0].cycle_length;
	float offset = wcf_coordination_data[0].offset;
	float max_add = wcf_coordination_data[0].max_add;
	float max_subt = wcf_coordination_data[0].max_subt;
	int iact;
	int method = wcf_coordination_data[0].method;
	float new_splits[8];
	float min_splits[8];

	for (int i = 0; i < 8; ++i)
	{
		new_splits[i] = wcf_coordination_data[0].splits[i];
	}

	error = etFommIF->GetControllerID(node, &iact);
	if(error == 0 && iact > 0)
	{
		error = etFommIF->SetNewCycleLength(node, cycle_length);
		error = etFommIF->SetNewOffset(node, offset);
		error = etFommIF->SetTransitionMethod(node, method, max_add, max_subt);
		error = etFommIF->SetNewSplits(node, new_splits);
	}
	proxy->SetClientState(GotCoordinationData);
}

void ProcessHITLSACData(IService1^ proxy, etFommInterface *etFommIF)
{
	int nac = etFommIF->GetNumberOfACSignals();
	array<WCF_AC>^ wcf_acl = gcnew array<WCF_AC>(0); 

	if( nac > 0 )
	{
		AC_INPUTS *signal_data = (AC_INPUTS*)calloc(nac, sizeof(AC_INPUTS));
		etFommIF->GetACSignals(signal_data);

		System::Array::Resize(wcf_acl, nac);

		int nPhases = 8;
		for(int isig = 0; isig < nac; isig++)
		{
			wcf_acl[isig].cycle_length = signal_data[isig].cycle_length;
			wcf_acl[isig].offset = signal_data[isig].offset;

			wcf_acl[isig].	min_green_time	=	gcnew array<float>(nPhases);
			wcf_acl[isig].	max_green_time	=	gcnew array<float>(nPhases);
			wcf_acl[isig].	default_extension_time	=	gcnew array<float>(nPhases);
			wcf_acl[isig].	yellow_change_int	=	gcnew array<float>(nPhases);
			wcf_acl[isig].	red_clear_int	=	gcnew array<float>(nPhases);
			wcf_acl[isig].	splits =	gcnew array<float>(nPhases);

			for (int iphase = 0; iphase < nPhases; ++iphase)
			{
				wcf_acl[isig].	min_green_time	[iphase]	=	signal_data[isig].	min_green_time	[iphase]	;
				wcf_acl[isig].	max_green_time	[iphase]	=	signal_data[isig].	max_green_time	[iphase]	;
				wcf_acl[isig].	default_extension_time	[iphase]	=	signal_data[isig].	default_extension_time	[iphase]	;
				wcf_acl[isig].	yellow_change_int	[iphase]	=	signal_data[isig].	yellow_change_int	[iphase]	;
				wcf_acl[isig].	red_clear_int	[iphase]	=	signal_data[isig].	red_clear_int	[iphase]	;
				wcf_acl[isig].	splits[iphase]	=	signal_data[isig].	splits[iphase]	;
			}
		}
	}
	proxy->SetServerACData(wcf_acl);
}
