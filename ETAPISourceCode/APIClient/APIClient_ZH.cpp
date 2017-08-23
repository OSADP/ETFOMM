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
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <vector>
#include <queue>


#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#using <System.dll>
#using <System.ServiceModel.dll>

#include "Client_Default_Network.h"



//#include "GlobalVariables.h"
//#include "CoodinatedNodes.h"
//#include "Freesim_link.h"
//#include "link.h"


#define TIMESTEP_INTERVAL 1
//#define WSHTTPBINDING 1
//#define NETTCPBINDING 0
#define PRINTOUT 0
#define STARTTIME 60
//#define _USE_TCA 0

using namespace System;
using namespace System::ServiceModel;
using namespace System::IO;
using namespace WCFServer;
using namespace std;



// for test by Xiang
int getCommand();


//Zhitong Huang
//Initial Function
void init(IService1^ proxy);
void ReadCoordinatedNodes(array<WCF_AC> ^ wcf_acl);
void GetLinks(IService1^ proxy);
void GetFreewayLinks(IService1^ proxy);
void ReadCoordinatedNodesCoordinatedData(); 
void ReadCoordinatedNodesFourApproach(); //read CoordinatedNodes_FourApproach.csv for four approach data of coordinated nodes
void ReadCoordinatedNodesPhaseData();
void GetInitialOffset(array<WCF_AC> ^ wcf_acl);
vector<string> SplitString(char* str,const char* delim); //divide a string to different elements according to a user defined demit symbol
void GetACPhaseData(array<WCF_AC>^ wcf_acl);//get phase signal timing data from etFOMM to save into CoodinatedNodes list.
void ReadCoordinatedPath();//Obtained Coordinated traffic path
void GetAllIntersectionCoordinatedPhaseGreen();
int GetIntialCoordinatedPhaseGreenP2(int NodeID);
void GetAllTravelTimeOfCoordinatedIntersections();//Get all free flow travel time between each pair of coordinated intersections.
int GetFFTBetweenTwoCoordinatedIntersections(int Up, int Down);//obtain free flow travel time between two nodes of street links
int FindElementInVector(vector<int> A, int node);// Find the index of an specific node in a path vector 
int RoundOffDouble (double num);//round-off a double value to a int value
void GetDistanceBetweenCoordinatedIntersections();//Get the distance between two neighbour coordinatd intersection
int GetDistanceBetweenTwoCoordinatedIntersections(int Up, int Down);//Get distance between two coordinated intersections
void ReadCoordinatedNodesUpcomingTrafficData();//Read data from CoordinatedNodes_InboundTrafficInfo.csv

//Real Time Function
void RecordFreSimArrivalProfile(array<WCF_VFData>^ vehicles);
void RecordNetSimArrivalProfile(array<WCF_VSData>^ vehicles);
void OnlineOffsetOptimization(IService1^ proxy);
void AdjustSplit(IService1^ proxy);
double EstimateGreenTime(int NodeID,int Direction, int TuringMovement);
vector<double> GetRealTimePastArrivalProfilesOfNetSIM(int UpNode, int DownNode);
//CLink FindLink(int USN, int DSN);//Find a street link according to given upstream and downstream nodes
void OptimizeOffsetofCoordinatedIntersections(IService1^ proxy);
int EstimateCoordinatedGreenTime(int NodeID);
//double LinkCoordinatedTurningPercetage(CLink pCurrentLink,int ID);//Calculate turning percentages of coordinated phases 
int EstimateNumofArrivalsonGreen (int NodeID, int Offset, int shifttime, int GreenTime);
int EstResidentQL_Beginning(int NodeID);
int RoundUp(double m);
vector<double> ShiftArrivalProfiles(vector<double> arrivals, int shifttime);
void RecordUsedYieldpoint(IService1^ proxy);//Record used yield point of each coordinated intersection
void RecordUsedSplits(IService1^ proxy);//record used splits for each phase of a intersection
//double EstimateDischargeHeadway(CLink pLink, double Percent);//Estimate discharge heaway of an approach for a predefined percentage
//vector<double> EstimateTurningPercentage(CLink pLink);//Estimate real-time turning percentage of an approach based on connected vehicle data
vector<double> PredictUpcomingArrivalProfiles(int node);//Predict upcoming arrival profiles for a coordinated intersection
int EstimateRealTimeApproachTravelTime(vector<int> Nodelist);//Estimate Real Time travel time of an approach based on TCA data
																
 //output function
void SimulationComplete();
void GenerateFreSIMArrivalProfile();
void GenerateNETSIMArrivalProfile();
void GenerateFreSIMDischargeProfile();
void GenerateNETSIMDischargeProfile();
void PrintOptimizedYieldPoints();//Print optimized yield points for each coordinated intersections for further analysis
void PrintUsedYieldPoints();//print used yield point for each coordinated intersection
void PrintAdjustedSplits();//Print adjusted splits for each coordinated intersections for further analysis
void PrintUsedSplits();//Print used splits of each coordinated intersection for further analysis
void PrintEstimatedCoordinatedGreen();//print predicted coordinated green time for each coordinated intersection

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
	
	
	String^ dataPath = "APIOutput\\";
	String^ fileName1 = "Before_1";
	int fileCount1 = 0;
	String^ fileName2 = "After_6";
	int fileCount2 = 0;
	String^ tmpName = "";
	ofstream TCA_TEST_OUTPUT_FILE;
	TCA_TEST_OUTPUT_FILE.open("TCA_TEST_OUTPUT.dat");


	/*The Client side to connect WCF Host. We want to Client side could modify data in the server side.
	  Client Configuration to connect the server host. Configuration of binding, address*/
#ifdef _TCP_DIST
	String^ ServerIP;
	Console::Write(L"Please enter the internal IP address of the server: ");
    ServerIP = Console::ReadLine();
#endif
	
	cout<<"connecting to WCF server... "<<endl;	
	    
#if WSHTTPBINDING
	WSHttpBinding^ binding = gcnew WSHttpBinding();
	binding->MaxBufferPoolSize = 2147483647;
	binding->MaxReceivedMessageSize = 2147483647; 
	binding->Security->Mode = System::ServiceModel::SecurityMode::None;
	EndpointAddress^ address = gcnew EndpointAddress(String::Format(L"http://localhost:8000/service", Environment::MachineName));
#endif

#if NETTCPBINDING
	NetTcpBinding^ binding = gcnew NetTcpBinding();
	binding->MaxBufferPoolSize = 2147483647;
	binding->MaxReceivedMessageSize = 2147483647; 
	binding->Security->Mode = System::ServiceModel::SecurityMode::None;
#ifdef _TCP_DIST
	binding->Security->Mode = System::ServiceModel::SecurityMode::None;
	String^ serviceAddr = String::Concat(L"net.tcp://", ServerIP, L":6000/service");
	EndpointAddress^ address = gcnew EndpointAddress(String::Format(serviceAddr, Environment::MachineName));
#else
	EndpointAddress^ address = gcnew EndpointAddress(String::Format(L"net.tcp://localhost:6000/service", Environment::MachineName));
#endif
#endif

	ChannelFactory<IService1^>^ factory = gcnew ChannelFactory<IService1^>(binding, address);
	IService1^ proxy = factory->CreateChannel();
	std::cout<<"Communication channel is completed."<<endl;

	float interval;
	proxy->SetNumberOfConnectedClients(1);
	proxy->SetClientState(RequestingUpdate);
	array <NewVehicle> ^ newVehicle = gcnew array<NewVehicle>(1);
	array<WCF_VFData> ^fvehicle_data;
	array<WCF_VSData> ^svehicle_data;
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
	std::cout<<"Please enter the timestep interval for etfomm to pause for API client input. "<<endl;
	std::cin >> interval;
	proxy->SetAPITimestepInterval(interval);

	std::cout << "\n\nWould you like to generate text files at the end of simulation? (Y/N) " << std::endl;
	int writeTextFlag = 0;
	char GPSFlag = 'N';
	std::cin >> GPSFlag;
	if (GPSFlag == 'Y' || GPSFlag == 'y')
	{
		writeTextFlag = 1;
	}
	proxy->SetServerWriteTextFlag(writeTextFlag);

	std::cout << "Please choose input source:" << std::endl;
	std::cout << "	Press <C> to use client defined default network." << std::endl;
	std::cout << "	Press <F> to UseTRFFile." << std::endl;
	while (std::cin >> t)
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

	float currentstep = 0.0;

	int initcount = 1;
	Boolean state=true;
	int laststep = -1;

	while (!exit)
	{
		
			
		if (ClientDefineNetworkFlag == true || proxy->GetClientState() == NextRun)
		{
			//dont need client define network;
		}
		else if (HostDefineNetworkFlag == true)
		{
			HostDefineNetworkFlag = false;
			proxy->SetClientState(UseHostDefineNetwork);
		}
		else if (TRFFileFlag)
		{
			std::cout << "\n\nPlease enter the path and file name for TRF file: " << std::endl;
			std::string TRFFile = "";
			std::cin >> TRFFile;
			String ^TRFFileName = gcnew String(TRFFile.c_str());
			TRFFileFlag = false;
			proxy->SetServerTRFFile(TRFFileName);

			proxy->SetClientState(UseTRFFile);
#if _USE_TCA
			proxy->SetTCAState(TCAContinued);
#endif
		}

		currentPeriod = proxy->GetServerTimePeriod();

		if (currentPeriod > lastPeriod) 
		{
			updateFlag = true;
			lastPeriod = currentPeriod;
		}

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

		StreamWriter ^outputFile3;

		int hoststate = proxy->GetHostState();
		cout<< "hoststate = " << hoststate <<endl;
		currentstep = proxy->GetServerTimestep();

		if (currentstep > laststep){
			state = true;
			laststep = currentstep;
		}

		

		if ((hoststate == 1) && (state)){
			cout << "run step in:" << currentstep << endl;
			fvehicle_data = proxy->GetServerFVehicleData();
			svehicle_data = proxy->GetServerSVehicleData();
			/*if (currentstep < 1){
				cout << "Waiting for simulation start" << endl;
				cin >> command;
				proxy->SetClientState(Continued);
			}*/


			if (currentstep <= 1 && initcount == 1){
				cout << "Waiting for simulation start" << endl;
				cin >> command;

			//	GetLinks(proxy);
				//GetFreewayLinks(proxy);
				//RecordFreSimArrivalProfile(fvehicle_data);
			//	RecordNetSimArrivalProfile(svehicle_data);
				
				initcount = 0;
			//	init(proxy);

				proxy->SetClientState(Continued);	
#if _USE_TCA
				proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
			}
			else if (currentstep <=1){
#if _USE_TCA
				while (proxy->GetTCAState() != TCAProcessingDone)
				{
				}
#endif
				proxy->SetClientState(Continued);
#if _USE_TCA
				proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
			}
			else if (currentstep > 1){
				//RecordFreSimArrivalProfile(fvehicle_data);
				//RecordNetSimArrivalProfile(svehicle_data);

#if _USE_TCA
				while ((proxy->GetTCAState() != TCAProcessingDone) && (proxy->GetTCAState() != SimulationFinished))
				{
				}
		
				//get vehicle BSM messages
				//Get new vehicle information including the TCA part from WCF server
				float BSMdataFromSimTime = proxy->GetServerTimestep();
				cout << "Getting BSMdata From SimTime: " << BSMdataFromSimTime << endl;
				svehicle_BSMdata = proxy->GetServerSVehicleData();
				cout << "svehicle_BSMdata[0].BSM_tmp_ID: " << svehicle_BSMdata[0].BSM_tmp_ID << endl;
				//proxy->SetTCAState(GoodForNextSimulationStep);//continue for TCA
				TCA_TEST_OUTPUT_FILE << "TimeStep = " << BSMdataFromSimTime << std::endl;
				
				//TCA_TEST_OUTPUT_FILE << "Getting BSMdata From SimTime: " << BSMdataFromSimTime << std::endl;
				//TCA_TEST_OUTPUT_FILE << "svehicle_BSMdata[0].BSM_tmp_ID: " << svehicle_BSMdata[0].BSM_tmp_ID << std::endl;
				////
				//Do something here about the street vehicle BSM data, svehicle_BSMdata
				////
#endif	
				if (currentstep >= STARTTIME){

					int node = 450;
					if (currentstep == 90)
					{
						proxy->SetServerNewOffset(node, 5);
						proxy->SetClientState(SetNewOffset);
						proxy->SetTCAState(TCAContinued);//continue for TCA
						while (proxy->GetClientState() != GotCoordinationData)
						{
						}

					}

				}
				//RecordUsedYieldpoint(proxy);
				//RecordUsedSplits(proxy);


				proxy->SetClientState(Continued);
#if _USE_TCA
				proxy->SetTCAState(GoodForNextSimulationStep);//continue for TCA
#endif
			}
			else if (currentstep == 60){

				proxy->SetClientState(NoRequest);
#if _USE_TCA
				proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
			}			

			state = false;
		}
		else {
			cout << "Waiting for host side!" << endl;
			//proxy->SetTCAState(GoodForNextSimulationStep);//continue for TCA
		}
				
	}

	//SimulationComplete();
	char terminate;
	std::cout << "WCF service is stopped." << std::endl;
	std::cout << "############################" << std::endl << std::endl;
	std::cout << "hit any key to terminate" << std::endl;
	std::cin >> terminate;
	//output add here

	proxy->SetClientState(APIShutDown);
#if _USE_TCA
	proxy->SetTCAState(TCAContinued);//continue for TCA
#endif
	return 0;

}
