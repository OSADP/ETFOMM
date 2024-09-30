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
//#include "Host_Default_Network.h"
#undef _DTA

#define PRINTOUT 0

// using namespace std; //disabled by li Zhang 09/26/2022

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
	const std::string dtafile = "dtafile"; // DTALite project file
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

	std::cout << "\n" << "usage:" << "\n";
	std::cout << "\tetRunner trffile=\"c:\\full path to\\input.trf\" [tsd]" << "\n" << "\n";
	std::cout << "Recognized Options:" << "\n" << "\n";
	//std::cout << simdll << "=\"c:\\full path to\\xxx.dll\" - specify an alternate simulation dll" << "\n" << "\n";
	//std::cout << tsd << "       - generate TSD file" << "\n"; //add when animation files are produced
	//std::cout << tid << "       - generate TID file" << "\n"; //add when animation files are produced
//	std::cout << txt << "       - generate TXT file" << "\n";
	std::cout << tsd << "       - generate animation TXT file" << "\n";
	//std::cout << out << "       - generate OUT file" << "\n";
	//std::cout << csv << "       - generate CSV file" << "\n";
//	std::cout << runs << "=n    - run n times" << "\n";
//	std::cout << rnsfile << "=\"c:\\full path to\\random.rns\"" << "\n";
	std::cout << trffile << "=\"c:\\full path to\\trffile.trf\"" << "\n";
	std::cout << dtafile << "=\"c:\\full path to\\dtafile.tnp\"" << "\n";
	std::cout << "\n" << "options may be specified in any order." << "\n";
	std::cout << "\n";
}


int main(int argc, char* argv[]) //TODO separate main() from etRunner64.cpp? file is too long
{


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

	// for test by Xiang
	int updatedFlag = 0;

	std::string dataPath = "APIOutput\\";
	std::string fileName1 = "After_2";
	int fileCount1 = 0;
	std::string fileName2 = "After_4";
	int fileCount2 = 0;
	std::string tmpName, prefix;

	std::string rnsFile = "rnsfile.dat";
//#ifdef _ETRUNNER_ONLY
	std::string trfFILE;
//#else
//TERST BY lz
//	std::string trfFILE = "C:\\Users\\LiZhang\\Documents\\NGSIM\\FRW1\\fwy1.trf";
	//std::string trfFILE = "Sample\\incd1.trf";
	//std::string trfFILE = "C:\\coordination_test_new_format.trf";
	//std::string trfFILE = "c:\\case2_text_files5.trf";
	//std::string rnsFile = appPath + "\\rnsfile.dat"; //use current appPath
	//std::string trfFILE = appPath + "\\testInterface.trf"; //use current appPath
//#endif
//DTA was removed by LZ
#ifdef _DTA //Added by LZ 4/13/2024

#endif // _DTA

	// parse the rest of the command-line arguments
	// Xiang: the command-line arguments should be passed from APIClient instead

	//test trf file directory:

	//trfFILE = "C:\\TSIS6 Projects\\Incd1 Example\\incd1.trf";
	for (int i = 1; i < argc; i++)
	{
		using namespace argnames;

		std::string argument = argv[i];
		if (argument == tsd)
		{
			TSDFlag = 2;
		}

		// tid
		else if(argument == tid)
		{
			TIDFlag = 1;
		}

		// write to the TSIS window
		else if (argument == out)
		{
			OutFlag = 1;
		}
		// write to the csv file
		else if (argument == csv)
		{
			CSVFlag = 1;
		}
		// number of runs
		else if (argument.substr(0, runs.size()) == runs)
		{
			NumberRuns = atoi(argument.substr(runs.size() + 1).c_str());
		}
		// rns file
		else if (argument.substr(0, rnsfile.size()) == rnsfile)
		{
			rnsFile = argument.substr(rnsfile.size() + 1).c_str();
		}
		else if (argument.substr(0, trffile.size()) == trffile)
		{
			trfFILE = argument.substr(trffile.size() + 1).c_str();
			TRFInputFlag = 1;
		}
#ifdef _DTA  //Added by LZ 4/13/2024
#endif // _DTA
		else if (argument == help || argument == h)
		{
			usage();
			return 1;
		}
		else if (argument == txt)
		{
			TSDFlag = 2;
			WriteTextFlag = 1;
		}
		else if (argument == writeTRF)
		{
			WriteTRFFlag = 1;
		}
		//TODO add WRITE_TRF_FILE //replaced by the 9th parameter of SET_RUN_INPUTS

		//LZ 07/09/2024 added for export to pipe
		//WriteTextFlag = 1;
		//m_WriteFile = 1;
	}
	if (trfFILE.empty()) //TODO need to rearrange the arguments 
	{
		//usage();
		//return 1; //TODO user can use WCF to input trf path
		trfFILE = "Sample\\incd1.trf";
	}

	etFommInterface* etFommIF = new etFommInterface(); // moved 9/24/15
	//I don't think we need this LZ
	//host->Open();

		/* Connect server host*/


SimulateRun:
	try {
		if (etFommIF->InitDLLLoadings() != 0)
		{
			//fail, return to caller or stop here 				
			std::cout << "Error2: " << etFommIF->err_msg << "\n";
		};


		etFommIF->Init();
#if 1//_EDITOR_INPUT
		std::ifstream seedfile(rnsFile.c_str());

		int nrand, seed1, seed2, seed3;
		seedfile >> nrand;

		if (NumberRuns > 1)//TODO etFommEditor only handle NumberRuns == 1, so we handle this outside etFommInterface
		{
			if (!etFommIF->SetRunInputs())
			{
				std::cout << "\n" << "The SET_RUN_INPUTS function was not found." << "\n";
				return 1;
			}
			if (NumberRuns > nrand)
			{
				std::cout << "\n" << "The requested number of runs exceeds the number of random number seeds (" << nrand << ")." << "\n";
				return 1;
			}
		}
#endif

		for (int nrun = 1; nrun <= NumberRuns; nrun++)
		{

			//LZ test named piple
			//etFommIF->WriteAnimatortoPipe();


			//array<WCF_NODE_LOCATION_DATA>^ wcf_gps_ref_nodes;
			//NODE_LOCATION_DATA* GPS_Ref_Nodes;
			//status = etFommIF->SetInputs();//use default trfFILE, TSDFlag, TIDFlag, OutFlag, CSVFlag
			status = etFommIF->SetInputs(trfFILE, TSDFlag, TIDFlag, OutFlag, CSVFlag, WriteTextFlag, nrun);

#if TRF_NETWORK_INPUT_FLAG

			status = etFommIF->StartUP();//TODO //TODO check email on 10/4/13 7:03PM from Tom
			//put the startup() function in different place may affect the TXT result
			//the result of run1 and run2 may be the same, run1 and run2 may use the same random number seeds.
#endif
			//TODO add sth to use not API, no pause
			std::cout << "Starting run " << nrun << "\n";



#if 1//_EDITOR_INPUT
			seedfile >> seed1 >> seed2 >> seed3;

			//int nrand = 1, seed1 = 97165909, seed2 = 67999630, seed3 = 41456717;
			//etFommIF->SetRunInputs(nrun, seed1, seed2, seed3);//TODO
			std::cout << "Random number seeds used: " << seed1 << ", " << seed2 << ", " << seed3 << "\n" << "\n";
#endif

#pragma region _EDITOR_INPUT
#ifndef _ETRUNNER_ONLY 
#endif//#ifndef _ETRUNNER_ONLY
#pragma endregion

			// try new SCOPE file format
			//if (TRFInputFlag == 1)
			//	SetNetworkInputs(etFommIF);

			bool APIstartup = (TRFInputFlag == 0);
			status = etFommIF->StartUP(APIstartup);

			//09/06/2025 LZ disabled intersection model
			//etFommIF->ImportIntersectionModel();
#if 0//_EDITOR_INPUT
			status = etFommIF->SetRunInputs(nrun, seed1, seed2, seed3);//TODO
#endif
			if (status != 0)
			{
				std::cout << "Input error, please check input\nPress any key to exit..." << "\n";
				std::cin.get();
				return 1;
			}

			NETWORK_INPUTS Network_Inputs;
			etFommIF->GetNetworkInputs(Network_Inputs);

			float minTimeStep = 0;
			std::vector<std::vector<int> > paths;
			std::vector<AddedVehicle> vehicles;
#ifdef _DTA


#endif // _DTA

			// try new SCOPE file format
			//if (TRFInputFlag == 1)
			//	ProcessACData(etFommIF);



#if _EDITOR_INPUT
			if (WriteTRFFlag == 1)
			{
				etFommIF->WriteTRFFile();
			}

#endif			
			if (status == 0)
				etFommIF->RunInitialize();

			float timestep;
			bool change_phase_flag = false;
			float phase_timer[10] = { 0 };
			float simulation_count = 0, APIfrequency = 0;
			float SimulationTimeStep = etFommIF->GetTimeStep();

			float currentAPIfrequency = 0;
#ifndef _ETRUNNER_ONLY

#endif //#ifndef _ETRUNNER_ONLY

			int LASTPERIOD = 0;
			float cpu_time = 0;
			float etime = 0;
			float CURRENT_SIMTIME = 0;

			int structsize;//TODO delete

			std::ofstream outputFile2;
			int CURRENTPERIOD = 0;
			/* commented 7/10/2024 LZ

			#if WCF_SCOPE //TODO if (APIFlag == 1){}?


			#endif
			#if _UseController
			#endif
			#ifdef _DTA  //added by LZ 04/13/2024


			#endif // _DTA
			*/
			while (status == 0)
			{
#if _UseController
				QStart = QStop;
#endif
#if _UseController//ContinueAfter100ms
				GetSystemTime(&st_start);
#endif

#if !WCF_SCOPE
				status = etFommIF->ETFOMM_SIMULATE();

#endif
				if ((TSDFlag == 2 || WriteTextFlag == 1) && (status == 0 || status == 4)) 
					etFommIF->ETFOMM_WriteAnimationFiles();

#if _UseController
				CURRENTPERIOD = etFommIF->GetCurrentPeriod();
				if (CURRENTPERIOD > LASTPERIOD && status != 4)
				{
					LASTPERIOD = CURRENTPERIOD;

				}
#endif


				simulation_count += SimulationTimeStep;

			} //while status ==0									

/*
				TimeLogFile << "\n" << "st_start call SIMULATE() @" << st_start.wMinute << "." << st_start.wSecond << ":" << st_start.wMilliseconds << "\n";
#endif
				CURRENT_SIMTIME = etFommIF->GetSimtime();
	*/
#if _UseController

	//std::cout << "Current Simulation Time = " << CURRENT_SIMTIME << "\n";
			ACLogFile << CURRENT_SIMTIME;
#endif
#if !UDPBINDING && !NETTCPBINDING
#ifndef _ETRUNNER_ONLY
#endif //#ifndef _ETRUNNER_ONLY
#endif


#if !WCF_SCOPE
#endif
#pragma endregion
#pragma region working_code_before_test_continue_simulation_use_phase_states_from_last_step
#if 1

#endif
#pragma endregion


#if WCF_SCOPE
#endif
#if !_UseController
#endif

#if _UseController

#endif
			//LZ added 7/9/2024 to pass anamation data to a 32 bit applications
			int iLZ = 9999;//set debug start

			//LZ test named piple
			//etFommIF->WriteAnimatortoPipe();

			//call SHUTDOWN once
			status = etFommIF->ShutDown();



			if (NumberRuns >= 1) std::cout << "Finished run " << nrun << "\n" << "\n";
		}
	}catch(InterfaceException & e)
			{
				std::cout << std::string(e.what()) << "\n";
			}
#ifndef _ETRUNNER_ONLY
	#endif //#ifndef _ETRUNNER_ONLY
#ifndef _ETRUNNER_ONLY
#endif //#ifndef _ETRUNNER_ONLY

	return 0;


}

//Detecletd WCF realted code


bool InsertConnections(int usn, int dsn, int nusn, int ndsn, std::map<std::pair<int, int>, int >& connections)
{
	if (nusn == dsn && ndsn != usn)
	{
		connections[std::pair<int, int>(usn, ndsn)] = dsn;
		return true;
	} else
	{
		return false;
	}
}

bool IsEntryNode(int nFLinks, FREEWAY_LINK* FLinks, int nSLinks, STREET_LINK* SLinks, int node)
{
	for (int i = 0; i < nFLinks; ++i)
	{
		FREEWAY_LINK* pLink = &(FLinks[i]); 
		if (pLink->dsn == node && pLink->usn >=8000)
			return true;
	}

	for (int i = 0; i < nSLinks; ++i)
	{
		STREET_LINK* pLink = &(SLinks[i]); 
		if (pLink->dsn == node && pLink->usn >=8000)
			return true;
	}

	return false;
}

bool IsExitNode(int nFLinks, FREEWAY_LINK* FLinks, int nSLinks, STREET_LINK* SLinks, int node)
{
	for (int i = 0; i < nFLinks; ++i)
	{
		FREEWAY_LINK* pLink = &(FLinks[i]); 
		if (pLink->usn == node && pLink->dsn >=8000)
			return true;
	}

	for (int i = 0; i < nSLinks; ++i)
	{
		STREET_LINK* pLink = &(SLinks[i]); 
		if (pLink->usn == node && pLink->dsn >=8000)
			return true;
	}

	return false;
}