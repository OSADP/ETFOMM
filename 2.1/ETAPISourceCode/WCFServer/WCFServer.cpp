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
// This is the main DLL file.

#include "stdafx.h"
#include "WCFServer.h"


namespace WCFServer
{

	[ServiceBehavior(
		ConcurrencyMode = ConcurrencyMode::Multiple,
		InstanceContextMode = InstanceContextMode::PerCall,
		//InstanceContextMode = InstanceContextMode::PerSession,
		)]
	public ref class Service1 : IService1
	{
		static array<WCF_VFData>^ server_fvehicles = gcnew array<WCF_VFData>(1);
		static array<WCF_VSData>^ server_svehicles = gcnew array<WCF_VSData>(1);

		static array<NewVehicle> ^ server_newVehicle = gcnew array<NewVehicle>(1);
		static array<Signal>^ server_signals = gcnew array<Signal>(1);

		static array<Wcf_freeway_link>^ server_freeway_links = gcnew array<Wcf_freeway_link>(1);
		static array<Wcf_street_link>^ server_street_links = gcnew array<Wcf_street_link>(1);
		static array<WCF_COND_TURNPCTS>^ server_cond_turnpct_data = gcnew array<WCF_COND_TURNPCTS>(1);		
		static array<WCF_ENTRYNODES_DATA>^ server_entry_nodes = gcnew array<WCF_ENTRYNODES_DATA>(1);
		static array<WCF_NETWORK_INPUTS>^ server_network_inputs = gcnew array<WCF_NETWORK_INPUTS>(1);
		static array<WCF_FREEWAY_NETWORK_INPUTS>^ server_freeway_network_inputs = gcnew array<WCF_FREEWAY_NETWORK_INPUTS>(1);
		static array<WCF_STREET_NETWORK_INPUTS>^ server_street_network_inputs = gcnew array<WCF_STREET_NETWORK_INPUTS>(1);
		static array<WCF_VEHICLE_TYPE_DATA>^ server_Vehicle_Type_Inputs = gcnew array<WCF_VEHICLE_TYPE_DATA>(1);
		static array<WCF_FTC_DATA>^ server_ftc_signals = gcnew array<WCF_FTC_DATA>(1);
		static array<WCF_AC>^ server_ac_signals = gcnew array<WCF_AC>(1);
		static array<WCF_DETECTOR_INPUTS>^ server_sdet_inputs = gcnew array<WCF_DETECTOR_INPUTS>(1);
		static array<WCF_DETECTOR_INPUTS>^ server_fdet_inputs = gcnew array<WCF_DETECTOR_INPUTS>(1);
		static array<WCF_DETECTOR_OUTPUTS>^ server_sdet_outputs = gcnew array<WCF_DETECTOR_OUTPUTS>(1);
		static array<WCF_DETECTOR_OUTPUTS>^ server_fdet_outputs = gcnew array<WCF_DETECTOR_OUTPUTS>(1);
		static array<WCF_BUSROUTE_DATA>^ server_busroute_inputs = gcnew array<WCF_BUSROUTE_DATA>(1);
		static array<WCF_BUSSTATION_DATA>^ server_busstation_inputs = gcnew array<WCF_BUSSTATION_DATA>(1);
		static array<WCF_INCIDENT_DATA>^ server_incident_inputs = gcnew array<WCF_INCIDENT_DATA>(1);
		static array<WCF_NODE_LOCATION_DATA>^ server_xy_coord_inputs = gcnew array<WCF_NODE_LOCATION_DATA>(1);
		static array<WCF_RM_DATA>^ server_rampmeter_inputs = gcnew array<WCF_RM_DATA>(1);
		static array<WCF_PARKING_DATA>^ server_parking_inputs = gcnew array<WCF_PARKING_DATA>(1);
		static array<WCF_EVENT_DATA>^ server_event_inputs = gcnew array<WCF_EVENT_DATA>(1);
		static array<WCF_DIVERSION_DATA>^ server_diversion_inputs = gcnew array<WCF_DIVERSION_DATA>(1);
		static array<WCF_NODE_LOCATION_DATA>^ server_gps_ref_nodes = gcnew array<WCF_NODE_LOCATION_DATA>(2);
		static array<int>^ server_nodesList = gcnew array<int>(1);
		static float server_timestep;
		static int server_timeperiod;
		static array<WCF_TIMING_PLAN>^ server_timing_plan_data = gcnew array<WCF_TIMING_PLAN>(1);
		static array<WCF_SIGNAL_CONTROLLER_DATA>^ server_signal_controller_data = gcnew array<WCF_SIGNAL_CONTROLLER_DATA>(1);
		static array<WCF_RUNNER_DATA>^ server_runner_data = gcnew array<WCF_RUNNER_DATA>(1);
		static int server_runner_state = 0;//For WCF-SCOPE/DETECTOR/DCS runner
		static int server_tca_state = 0;//For WCF-TCA
		static array<WCF_COORDINATION_DATA>^ server_coordination_data = gcnew array<WCF_COORDINATION_DATA>(1);
		static array<WCF_CONTROLLER_DATA>^ server_controller_data = gcnew array<WCF_CONTROLLER_DATA>(1);
		static int server_controller_data_state = 0;
		static array<WCF_STREET_MOE_DATA>^ server_street_moe_data = gcnew array<WCF_STREET_MOE_DATA>(1);

		static int numberOfConnectedClients =0;
		static int clientOperationStatus = 0;  //not available
		static int hostOperationStatus = 0;
		static float timestepInterval = 1.0; //default 1 second

		static String^ TRFFileName;
		static String^ GPSRefFileName;
		static int server_write_text_flag = 0;
		static int nFVehicle = 0;
		static int nEntryNodes = 0;
		static int nFTC = 0;
		
		static int server_node_id = 0;
		static int server_usn_id = 0;
		static int server_dsn_id = 0;
		static float server_average_travel_time = 0;
		static float server_max_add = 0;
		static float server_max_sub = 0;
		static float server_cycle_length = 0;
		static float server_new_cycle_length = 0;
		static float server_offset = 0;
		static float server_new_offset = 0;
		static float server_local_cycle_timer = 0;
		static array<float>^ server_splits = gcnew array<float>(8);
		static array<float>^ server_new_splits = gcnew array<float>(8);
		static array<float>^ server_min_splits = gcnew array<float>(8);
		static array<int>^ server_greens_yellows = gcnew array<int>(2);
		static int server_transition_method = 0;

	public:

		//getter setter for numberOfConnectedClients
		virtual int GetNumberOfConnectedClients()
		{
			return numberOfConnectedClients;
		}

		virtual void SetNumberOfConnectedClients(int count)
		{
			numberOfConnectedClients = count;
		}

		//getter setter for ClientState
		virtual int GetClientState()
		{
			return clientOperationStatus;
		}

		virtual void SetClientState(int s)
		{
			clientOperationStatus = s;
		}

		//getter setter for ClientState
		virtual int GetHostState()
		{
			return hostOperationStatus;
		}

		virtual void SetHostState(int hs)
		{
			hostOperationStatus = hs;
		}


		//getter setter for timestep
		virtual float GetServerTimestep()
		{
			return server_timestep;
		}

		virtual void SetServerTimestep(float t)
		{
			server_timestep = t;
		}

		//getter setter for timestepInterval
		virtual float GetAPITimestepInterval()
		{
			return timestepInterval;
		}

		virtual void SetAPITimestepInterval(float t)
		{
			timestepInterval = t;
		}

		//getter setter for time period
		virtual int GetServerTimePeriod()
		{
			return server_timeperiod;
		}

		virtual void SetServerTimePeriod(int tp)
		{
			server_timeperiod = tp;
		}

		//implement vehicle operation
		virtual void SetServerFVehicleData(array<WCF_VFData>^ wcf_fveh)
		{
			System::Array::Resize(server_fvehicles, wcf_fveh->Length);
			server_fvehicles = wcf_fveh;
			//for (int i = 0; i<wcf_fveh->Length;i++) 
			//{
			//	server_fvehicles[i].acceleration = wcf_fveh[i].acceleration;
			//	server_fvehicles[i].decel = wcf_fveh[i].decel;
			//	server_fvehicles[i].desiredspeed = wcf_fveh[i].desiredspeed;
			//	server_fvehicles[i].disch_timer = wcf_fveh[i].disch_timer;
			//	server_fvehicles[i].drivertype = wcf_fveh[i].drivertype;
			//	server_fvehicles[i].entry_link = wcf_fveh[i].entry_link;
			//	server_fvehicles[i].entrytime = wcf_fveh[i].entrytime;
			//	server_fvehicles[i].ev_dist = wcf_fveh[i].ev_dist;
			//	server_fvehicles[i].ev_ovrspd = wcf_fveh[i].ev_ovrspd;
			//	server_fvehicles[i].ev_range = wcf_fveh[i].ev_range;
			//	server_fvehicles[i].ev_rand = wcf_fveh[i].ev_rand;
			//	server_fvehicles[i].ev_wait_timer = wcf_fveh[i].ev_wait_timer;
			//	server_fvehicles[i].ev_watch = wcf_fveh[i].ev_watch;
			//	server_fvehicles[i].fleet = wcf_fveh[i].fleet;
			//	server_fvehicles[i].go_thru_signal = wcf_fveh[i].go_thru_signal;
			//	server_fvehicles[i].lag_timer = wcf_fveh[i].lag_timer;
			//	server_fvehicles[i].lane = wcf_fveh[i].lane;
			//	server_fvehicles[i].lanecodes = wcf_fveh[i].lanecodes;
			//	server_fvehicles[i].link = wcf_fveh[i].link;
			//	server_fvehicles[i].follower = wcf_fveh[i].follower;
			//	server_fvehicles[i].id = wcf_fveh[i].id;
			//	server_fvehicles[i].last_detid = wcf_fveh[i].last_detid;
			//	server_fvehicles[i].lc_timer = wcf_fveh[i].lc_timer;
			//	server_fvehicles[i].leader = wcf_fveh[i].leader;
			//	server_fvehicles[i].location = wcf_fveh[i].location;
			//	server_fvehicles[i].pathid = wcf_fveh[i].pathid;
			//	server_fvehicles[i].pathpoint = wcf_fveh[i].pathpoint;
			//	server_fvehicles[i].saved_path = wcf_fveh[i].saved_path;
			//	server_fvehicles[i].pseudo_leader = wcf_fveh[i].pseudo_leader;
			//	server_fvehicles[i].prev_accel = wcf_fveh[i].prev_accel;
			//	server_fvehicles[i].prevlink = wcf_fveh[i].prevlink;
			//	server_fvehicles[i].prevlane = wcf_fveh[i].prevlane;
			//	server_fvehicles[i].routeid = wcf_fveh[i].routeid;
			//	server_fvehicles[i].speed = wcf_fveh[i].speed;
			//	server_fvehicles[i].speed_adj = wcf_fveh[i].speed_adj;
			//	server_fvehicles[i].turncode = wcf_fveh[i].turncode;
			//	server_fvehicles[i].vlength = wcf_fveh[i].vlength;
			//	server_fvehicles[i].vtype = wcf_fveh[i].vtype;
			//	server_fvehicles[i].xcode = wcf_fveh[i].xcode;
			//	server_fvehicles[i].will_coop_ev = wcf_fveh[i].will_coop_ev;
			//	server_fvehicles[i].will_coop_lc = wcf_fveh[i].will_coop_lc;
			//	server_fvehicles[i].will_move = wcf_fveh[i].will_move;
			//	server_fvehicles[i].destination = wcf_fveh[i].destination;
			//	server_fvehicles[i].distance_to_segment_end = wcf_fveh[i].distance_to_segment_end;
			//	server_fvehicles[i].diverted = wcf_fveh[i].diverted;
			//	server_fvehicles[i].hov_violator = wcf_fveh[i].hov_violator;
			//	server_fvehicles[i].imeter = wcf_fveh[i].imeter;
			//	server_fvehicles[i].incident_num = wcf_fveh[i].incident_num;
			//	server_fvehicles[i].isegment = wcf_fveh[i].isegment;
			//	server_fvehicles[i].must_merge = wcf_fveh[i].must_merge;
			//	server_fvehicles[i].next_object = wcf_fveh[i].next_object;
			//	server_fvehicles[i].remaining_dist = wcf_fveh[i].remaining_dist;
			//	server_fvehicles[i].sorted_list = wcf_fveh[i].sorted_list;
			//	server_fvehicles[i].sort_position = wcf_fveh[i].sort_position;

			//	server_fvehicles[i].timestep = wcf_fveh[i].timestep;

			//	//TCA DATA
			//	server_fvehicles[i].DSRC_MessageID = wcf_fveh[i].DSRC_MessageID;
			//	server_fvehicles[i].Vehicle_ID = wcf_fveh[i].Vehicle_ID;
			//	server_fvehicles[i].BSM_tmp_ID = wcf_fveh[i].BSM_tmp_ID;
			//	server_fvehicles[i].transtime = wcf_fveh[i].transtime;
			//	server_fvehicles[i].Link = wcf_fveh[i].Link;
			//	server_fvehicles[i].Lane = wcf_fveh[i].Lane;
			//	server_fvehicles[i].Location = wcf_fveh[i].Location;
			//	server_fvehicles[i].Speed = wcf_fveh[i].Speed;
			//	server_fvehicles[i].Heading = wcf_fveh[i].Heading;
			//	server_fvehicles[i].avg_accel = wcf_fveh[i].avg_accel;
			//	server_fvehicles[i].brakeStatus = wcf_fveh[i].brakeStatus;
			//	server_fvehicles[i].brakePressure = wcf_fveh[i].brakePressure;
			//	server_fvehicles[i].hardBraking = wcf_fveh[i].hardBraking;
			//	server_fvehicles[i].transTo = wcf_fveh[i].transTo;
			//	server_fvehicles[i].transmission_received_time = wcf_fveh[i].transmission_received_time;
			//	server_fvehicles[i].TractionControl = wcf_fveh[i].TractionControl;
			//	server_fvehicles[i].AirTemp = wcf_fveh[i].AirTemp;
			//	server_fvehicles[i].Wipers = wcf_fveh[i].Wipers;
			//}			
		}
		virtual array<WCF_VFData>^ GetServerFVehicleData()
		{
			return server_fvehicles;
		}

		virtual void SetServerFVehicleDataSize(int nVehicle)
		{
			nFVehicle = nVehicle;
		}
		
		virtual int GetServerFVehicleDataSize()
		{
			return nFVehicle;
		}

		virtual void SetServerSVehicleData(array<WCF_VSData>^ wcf_sveh)
		{
			System::Array::Resize(server_svehicles, wcf_sveh->Length);
			server_svehicles = wcf_sveh;
			//for (int i = 0; i<wcf_sveh->Length;i++) 
			//{
			//	server_svehicles[i].acceleration = wcf_sveh[i].acceleration;
			//	server_svehicles[i].decel = wcf_sveh[i].decel;
			//	server_svehicles[i].desiredspeed = wcf_sveh[i].desiredspeed;
			//	server_svehicles[i].disch_timer = wcf_sveh[i].disch_timer;
			//	server_svehicles[i].drivertype = wcf_sveh[i].drivertype;
			//	server_svehicles[i].entry_link = wcf_sveh[i].entry_link;
			//	server_svehicles[i].entrytime = wcf_sveh[i].entrytime;
			//	server_svehicles[i].ev_dist = wcf_sveh[i].ev_dist;
			//	server_svehicles[i].ev_ovrspd = wcf_sveh[i].ev_ovrspd;
			//	server_svehicles[i].ev_range = wcf_sveh[i].ev_range;
			//	server_svehicles[i].ev_rand = wcf_sveh[i].ev_rand;
			//	server_svehicles[i].ev_wait_timer = wcf_sveh[i].ev_wait_timer;
			//	server_svehicles[i].ev_watch = wcf_sveh[i].ev_watch;
			//	server_svehicles[i].fleet = wcf_sveh[i].fleet;
			//	server_svehicles[i].go_thru_signal = wcf_sveh[i].go_thru_signal;
			//	server_svehicles[i].lag_timer = wcf_sveh[i].lag_timer;
			//	server_svehicles[i].lane = wcf_sveh[i].lane;
			//	server_svehicles[i].lanecodes = wcf_sveh[i].lanecodes;
			//	server_svehicles[i].link = wcf_sveh[i].link;
			//	server_svehicles[i].follower = wcf_sveh[i].follower;
			//	server_svehicles[i].id = wcf_sveh[i].id;
			//	server_svehicles[i].last_detid = wcf_sveh[i].last_detid;
			//	server_svehicles[i].lc_timer = wcf_sveh[i].lc_timer;
			//	server_svehicles[i].leader = wcf_sveh[i].leader;
			//	server_svehicles[i].location = wcf_sveh[i].location;
			//	server_svehicles[i].pathid = wcf_sveh[i].pathid;
			//	server_svehicles[i].pathpoint = wcf_sveh[i].pathpoint;
			//	server_svehicles[i].saved_path = wcf_sveh[i].saved_path;
			//	server_svehicles[i].pseudo_leader = wcf_sveh[i].pseudo_leader;
			//	server_svehicles[i].prev_accel = wcf_sveh[i].prev_accel;
			//	server_svehicles[i].prevlink = wcf_sveh[i].prevlink;
			//	server_svehicles[i].prevlane = wcf_sveh[i].prevlane;
			//	server_svehicles[i].routeid = wcf_sveh[i].routeid;
			//	server_svehicles[i].speed = wcf_sveh[i].speed;
			//	server_svehicles[i].speed_adj = wcf_sveh[i].speed_adj;
			//	server_svehicles[i].start_lag = wcf_sveh[i].start_lag;
			//	server_svehicles[i].turncode = wcf_sveh[i].turncode;
			//	server_svehicles[i].vlength = wcf_sveh[i].vlength;
			//	server_svehicles[i].vtype = wcf_sveh[i].vtype;
			//	server_svehicles[i].xcode = wcf_sveh[i].xcode;
			//	server_svehicles[i].will_coop_ev = wcf_sveh[i].will_coop_ev;
			//	server_svehicles[i].will_coop_lc = wcf_sveh[i].will_coop_lc;
			//	server_svehicles[i].will_move = wcf_sveh[i].will_move;
			//	server_svehicles[i].diverted = wcf_sveh[i].diverted;
			//	server_svehicles[i].dwell_timer = wcf_sveh[i].dwell_timer;
			//	server_svehicles[i].goal_lane = wcf_sveh[i].goal_lane;
			//	server_svehicles[i].has_stopped = wcf_sveh[i].has_stopped;
			//	server_svehicles[i].ispdicd = wcf_sveh[i].ispdicd;
			//	server_svehicles[i].next_stop = wcf_sveh[i].next_stop;
			//	server_svehicles[i].prvdist = wcf_sveh[i].prvdist;
			//	server_svehicles[i].prvlink = wcf_sveh[i].prvlink;
			//	server_svehicles[i].prvlnkicd = wcf_sveh[i].prvlnkicd;
			//	server_svehicles[i].qstate = wcf_sveh[i].qstate;
			//	server_svehicles[i].turn_code = wcf_sveh[i].turn_code;
			//	server_svehicles[i].turn_code2 = wcf_sveh[i].turn_code2;
			//	server_svehicles[i].turn_link = wcf_sveh[i].turn_link;
			//	server_svehicles[i].turn_link2 = wcf_sveh[i].turn_link2;
			//	server_svehicles[i].vehicd = wcf_sveh[i].vehicd;
			//	server_svehicles[i].will_jump = wcf_sveh[i].will_jump;
			//	server_svehicles[i].will_yield = wcf_sveh[i].will_yield;

			//	server_svehicles[i].timestep = wcf_sveh[i].timestep;

			//	//TCA DATA
			//	server_svehicles[i].DSRC_MessageID = wcf_sveh[i].DSRC_MessageID;
			//	server_svehicles[i].Vehicle_ID = wcf_sveh[i].Vehicle_ID;
			//	server_svehicles[i].BSM_tmp_ID = wcf_sveh[i].BSM_tmp_ID;
			//	server_svehicles[i].transtime = wcf_sveh[i].transtime;
			//	server_svehicles[i].Link = wcf_sveh[i].Link;
			//	server_svehicles[i].Lane = wcf_sveh[i].Lane;
			//	server_svehicles[i].Location = wcf_sveh[i].Location;
			//	server_svehicles[i].Speed = wcf_sveh[i].Speed;
			//	server_svehicles[i].Heading = wcf_sveh[i].Heading;
			//	server_svehicles[i].avg_accel = wcf_sveh[i].avg_accel;
			//	server_svehicles[i].brakeStatus = wcf_sveh[i].brakeStatus;
			//	server_svehicles[i].brakePressure = wcf_sveh[i].brakePressure;
			//	server_svehicles[i].hardBraking = wcf_sveh[i].hardBraking;
			//	server_svehicles[i].transTo = wcf_sveh[i].transTo;
			//	server_svehicles[i].transmission_received_time = wcf_sveh[i].transmission_received_time;
			//	server_svehicles[i].TractionControl = wcf_sveh[i].TractionControl;
			//	server_svehicles[i].AirTemp = wcf_sveh[i].AirTemp;
			//	server_svehicles[i].Wipers = wcf_sveh[i].Wipers;
			//}			
		}
		virtual array<WCF_VSData>^ GetServerSVehicleData()
		{
			return server_svehicles;
		}

		//path operation
		virtual void SetServerPathData(array<int>^ nl_client)
		{
			System::Array::Resize(server_nodesList, nl_client->Length);
			for (int i = 0; i<nl_client->Length;i++) 
			{
				server_nodesList[i] = nl_client[i];				
				//Console::WriteLine(L" in WCF server set VehicleData ID= {0} DriverType= {1} Speed = {2} ",server_vehicles[i].id, server_vehicles[i].drivertype,server_vehicles[i].speed );
			}				
		}

		virtual array<int>^ GetServerPathData()
		{
			array <int>^ nodes = gcnew array<int>(server_nodesList->Length);
			for (int i = 0; i<server_nodesList->Length;i++) 
			{
				nodes[i] = server_nodesList[i];				
				//Console::WriteLine(L" in WCF server set VehicleData ID= {0} DriverType= {1} Speed = {2} ",server_vehicles[i].id, server_vehicles[i].drivertype,server_vehicles[i].speed );
			}
			return nodes;
		}

		//new vehicle operation
		virtual void SetServerNewVehicleData( array<NewVehicle> ^ nv_client)
		{
			System::Array::Resize(server_newVehicle, nv_client->Length);
			for (int i = 0; i<server_newVehicle->Length;i++) 
			{
				server_newVehicle[i].time = nv_client[i].time;
				server_newVehicle[i].inode = nv_client[i].inode ;
				server_newVehicle[i].pathid = nv_client[i].pathid;
				server_newVehicle[i].driver = nv_client[i].driver;
				server_newVehicle[i].fleet = nv_client[i].fleet ;
				server_newVehicle[i].vtype = nv_client[i].vtype ;
				server_newVehicle[i].overspeed = nv_client[i].overspeed;
				server_newVehicle[i].range = nv_client[i].range;			
			}					
		}

		virtual array<NewVehicle> ^ GetServerNewVehicleData()
		{
			array <NewVehicle>^ newveh = gcnew array<NewVehicle>(server_newVehicle->Length);
			for (int i = 0; i<server_newVehicle->Length;i++) 				
			{
				Console::WriteLine(L" in WCF server get new VehicleData entry ID= {0} DriverType= {1} path = {2} ",
					server_newVehicle[i].inode, server_newVehicle[i].driver,server_newVehicle[i].pathid );		
				newveh[i].time = server_newVehicle[i].time;
				newveh[i].inode = server_newVehicle[i].inode ;
				newveh[i].pathid = server_newVehicle[i].pathid;
				newveh[i].driver = server_newVehicle[i].driver;
				newveh[i].fleet = server_newVehicle[i].fleet ;
				newveh[i].vtype = server_newVehicle[i].vtype ;
				newveh[i].overspeed = server_newVehicle[i].overspeed;
				newveh[i].range = server_newVehicle[i].range;
			}
			return newveh;
		}

		//signal operation
		virtual void SetServerSignalData(array<Signal> ^ sig)
		{
			System::Array::Resize(server_signals, sig->Length);
			for (int i = 0; i<server_signals->Length;i++) 
			{
				server_signals[i].nodeid = sig[i].nodeid;
				server_signals[i].interval = sig[i].interval ;
				server_signals[i].duration = sig[i].duration;							
			}					
		}

		virtual array <Signal> ^ GetServerSignalData()
		{
			array <Signal>^ signals = gcnew array<Signal>(server_signals->Length);
			for (int i = 0; i<server_signals->Length;i++) 				
			{
				signals[i].nodeid = server_signals[i].nodeid;
				signals[i].interval = server_signals[i].interval ;
				signals[i].duration = server_signals[i].duration;
			}
			return signals ;
		}



		virtual void SetServerNetworkInput(array <WCF_NETWORK_INPUTS> ^ wcf_network_inputs)
		{
			System::Array::Resize(server_network_inputs, wcf_network_inputs->Length);
			server_network_inputs = wcf_network_inputs;
			//for (int i = 0; i < wcf_network_inputs->Length; i++)
			//{
			//	//server_network_inputs[i].seed1 = wcf_network_inputs[i].seed1;
			//	//server_network_inputs[i].seed2 = wcf_network_inputs[i].seed2;
			//	//server_network_inputs[i].seed3 = wcf_network_inputs[i].seed3;
			//	server_network_inputs[i].run_init = wcf_network_inputs[i].run_init;
			//	server_network_inputs[i].initialization_end = wcf_network_inputs[i].initialization_end;
			//	server_network_inputs[i].timestep = wcf_network_inputs[i].timestep;
			//	server_network_inputs[i].time_interval = wcf_network_inputs[i].time_interval;
			//	server_network_inputs[i].time_period_duration = wcf_network_inputs[i].time_period_duration;
			//	server_network_inputs[i].type_of_run = wcf_network_inputs[i].type_of_run;
			//	server_network_inputs[i].sim_start_time = wcf_network_inputs[i].sim_start_time;
			//	server_network_inputs[i].max_node_number = wcf_network_inputs[i].max_node_number;
			//}
		}
		virtual array <WCF_NETWORK_INPUTS> ^ GetServerNetworkInput()
		{
			return server_network_inputs;
		}

		virtual void SetServerFreewayNetworkInput(array <WCF_FREEWAY_NETWORK_INPUTS> ^ wcf_freeway_network_inputs)
		{
			System::Array::Resize(server_freeway_network_inputs, wcf_freeway_network_inputs->Length);
			server_freeway_network_inputs = wcf_freeway_network_inputs;
			//for (int i = 0; i < wcf_freeway_network_inputs->Length; i++)
			//{
			//	server_freeway_network_inputs[i].cfrict = wcf_freeway_network_inputs[i].cfrict;
			//	server_freeway_network_inputs[i].default_hov_pct = wcf_freeway_network_inputs[i].default_hov_pct;
			//	server_freeway_network_inputs[i].lag_accel = wcf_freeway_network_inputs[i].lag_accel;
			//	server_freeway_network_inputs[i].lag_decel = wcf_freeway_network_inputs[i].lag_decel;
			//	server_freeway_network_inputs[i].ffspeed_adj = wcf_freeway_network_inputs[i].ffspeed_adj;
			//	server_freeway_network_inputs[i].zfoll = wcf_freeway_network_inputs[i].zfoll;
			//	server_freeway_network_inputs[i].freeway_pct_coop = wcf_freeway_network_inputs[i].freeway_pct_coop;
			//	server_freeway_network_inputs[i].lc_time = wcf_freeway_network_inputs[i].lc_time;
			//	server_freeway_network_inputs[i].dlc_mult = wcf_freeway_network_inputs[i].dlc_mult;
			//}
		}
		virtual array <WCF_FREEWAY_NETWORK_INPUTS> ^ GetServerFreewayNetworkInput()
		{
			return server_freeway_network_inputs;
		}

		virtual void SetServerStreetNetworkInput(array <WCF_STREET_NETWORK_INPUTS> ^ wcf_street_network_inputs)
		{
			System::Array::Resize(server_street_network_inputs, wcf_street_network_inputs->Length);
			server_street_network_inputs = wcf_street_network_inputs;
			//for (int i = 0; i < wcf_street_network_inputs->Length; i++)
			//{
			//	server_street_network_inputs[i].additional_gap = wcf_street_network_inputs[i].additional_gap;
			//	server_street_network_inputs[i].amber_decel = wcf_street_network_inputs[i].amber_decel;
			//	server_street_network_inputs[i].lt_speed = wcf_street_network_inputs[i].lt_speed;
			//	server_street_network_inputs[i].rt_speed = wcf_street_network_inputs[i].rt_speed;
			//	server_street_network_inputs[i].pdelay_weak = wcf_street_network_inputs[i].pdelay_weak;
			//	server_street_network_inputs[i].pdelay_strong = wcf_street_network_inputs[i].pdelay_strong;
			//	server_street_network_inputs[i].ped_duration = wcf_street_network_inputs[i].ped_duration;
			//	server_street_network_inputs[i].acceptable_gap = wcf_street_network_inputs[i].acceptable_gap;
			//	server_street_network_inputs[i].acceptable_ltg = wcf_street_network_inputs[i].acceptable_ltg;
			//	server_street_network_inputs[i].acceptable_rtg = wcf_street_network_inputs[i].acceptable_rtg;
			//	server_street_network_inputs[i].dwell_multiplier = wcf_street_network_inputs[i].dwell_multiplier;
			//	server_street_network_inputs[i].ffspeed_adj = wcf_street_network_inputs[i].ffspeed_adj;
			//	server_street_network_inputs[i].zfoll = wcf_street_network_inputs[i].zfoll;
			//	server_street_network_inputs[i].lc_time = wcf_street_network_inputs[i].lc_time;
			//	server_street_network_inputs[i].lt_jumper_prob = wcf_street_network_inputs[i].lt_jumper_prob;
			//	server_street_network_inputs[i].lt_lagger_prob = wcf_street_network_inputs[i].lt_lagger_prob;
			//	server_street_network_inputs[i].spillback_prob = wcf_street_network_inputs[i].spillback_prob;
			//	server_street_network_inputs[i].stop_spd = wcf_street_network_inputs[i].stop_spd;
			//	server_street_network_inputs[i].street_pct_coop = wcf_street_network_inputs[i].street_pct_coop;
			//	server_street_network_inputs[i].yield_spd = wcf_street_network_inputs[i].yield_spd;
			//	server_street_network_inputs[i].driver_fampct = wcf_street_network_inputs[i].driver_fampct;
			//	server_street_network_inputs[i].qfactor = wcf_street_network_inputs[i].qfactor;
			//	server_street_network_inputs[i].ste_mult = wcf_street_network_inputs[i].ste_mult;
			//}
		}
		virtual array <WCF_STREET_NETWORK_INPUTS>^ GetServerStreetNetworkInput()
		{
			return server_street_network_inputs;
		}

		virtual void SetServerVehicleTypeInputs(array <WCF_VEHICLE_TYPE_DATA>^ wcf_Vehicle_Type_Inputs)
		{
			System::Array::Resize(server_Vehicle_Type_Inputs, wcf_Vehicle_Type_Inputs->Length);
			server_Vehicle_Type_Inputs = wcf_Vehicle_Type_Inputs;

			//for (int i = 0; i < wcf_Vehicle_Type_Inputs->Length; i++)
			//{
			//	server_Vehicle_Type_Inputs[i].length = wcf_Vehicle_Type_Inputs[i].length;
			//	server_Vehicle_Type_Inputs[i].headway_factor = wcf_Vehicle_Type_Inputs[i].headway_factor;
			//	server_Vehicle_Type_Inputs[i].average_occupancy = wcf_Vehicle_Type_Inputs[i].average_occupancy;
			//	server_Vehicle_Type_Inputs[i].emergency_decel = wcf_Vehicle_Type_Inputs[i].emergency_decel;
			//	server_Vehicle_Type_Inputs[i].fleet_freeway_auto = wcf_Vehicle_Type_Inputs[i].fleet_freeway_auto;
			//	server_Vehicle_Type_Inputs[i].fleet_freeway_truck = wcf_Vehicle_Type_Inputs[i].fleet_freeway_truck;
			//	server_Vehicle_Type_Inputs[i].fleet_freeway_carpool = wcf_Vehicle_Type_Inputs[i].fleet_freeway_carpool;
			//	server_Vehicle_Type_Inputs[i].fleet_freeway_bus = wcf_Vehicle_Type_Inputs[i].fleet_freeway_bus;
			//	server_Vehicle_Type_Inputs[i].fleet_freeway_ev = wcf_Vehicle_Type_Inputs[i].fleet_freeway_ev;
			//	server_Vehicle_Type_Inputs[i].fleet_freeway_bike = wcf_Vehicle_Type_Inputs[i].fleet_freeway_bike;
			//	server_Vehicle_Type_Inputs[i].fleet_street_auto = wcf_Vehicle_Type_Inputs[i].fleet_street_auto;
			//	server_Vehicle_Type_Inputs[i].fleet_street_truck = wcf_Vehicle_Type_Inputs[i].fleet_street_truck;
			//	server_Vehicle_Type_Inputs[i].fleet_street_carpool = wcf_Vehicle_Type_Inputs[i].fleet_street_carpool;
			//	server_Vehicle_Type_Inputs[i].fleet_street_bus = wcf_Vehicle_Type_Inputs[i].fleet_street_bus;
			//	server_Vehicle_Type_Inputs[i].fleet_street_ev = wcf_Vehicle_Type_Inputs[i].fleet_street_ev;
			//	server_Vehicle_Type_Inputs[i].fleet_street_bike = wcf_Vehicle_Type_Inputs[i].fleet_street_bike;
			//}
		}
		virtual array <WCF_VEHICLE_TYPE_DATA>^ GetServerVehicleTypeInputs()
		{
			return server_Vehicle_Type_Inputs;
		}

		virtual void SetServerFreewayData(array <Wcf_freeway_link> ^ wcf_freeway_links)
		{
			System::Array::Resize(server_freeway_links, wcf_freeway_links->Length);
			server_freeway_links = wcf_freeway_links;
			//for (int i = 0; i < wcf_freeway_links->Length; i++) 
			//{
			//	server_freeway_links[i].id = wcf_freeway_links[i].id;
			//	server_freeway_links[i].usn = wcf_freeway_links[i].usn;
			//	server_freeway_links[i].dsn = wcf_freeway_links[i].dsn;
			//	server_freeway_links[i].linktype = wcf_freeway_links[i].linktype;
			//	server_freeway_links[i].thrunode = wcf_freeway_links[i].thrunode;
			//	server_freeway_links[i].thru_alignment_lane = wcf_freeway_links[i].thru_alignment_lane;
			//	server_freeway_links[i].offramp_sending_lane = wcf_freeway_links[i].offramp_sending_lane;
			//	server_freeway_links[i].exitnode = wcf_freeway_links[i].exitnode;		
			//	server_freeway_links[i].length = wcf_freeway_links[i].length;
			//	server_freeway_links[i].fulllanes = wcf_freeway_links[i].fulllanes;
			//	server_freeway_links[i].adddrop_code = wcf_freeway_links[i].adddrop_code;
			//	server_freeway_links[i].adddrop_lane = wcf_freeway_links[i].adddrop_lane;
			//	server_freeway_links[i].adddrop_dist = wcf_freeway_links[i].adddrop_dist;
			//	server_freeway_links[i].adddrop_warn = wcf_freeway_links[i].adddrop_warn;
			//	server_freeway_links[i].auxlaneid = wcf_freeway_links[i].auxlaneid;
			//	server_freeway_links[i].auxlanecode = wcf_freeway_links[i].auxlanecode;
			//	server_freeway_links[i].auxlanelength = wcf_freeway_links[i].auxlanelength;
			//	server_freeway_links[i].freeflowspeed = wcf_freeway_links[i].freeflowspeed;
			//	server_freeway_links[i].thru_percent = wcf_freeway_links[i].thru_percent;
			//	server_freeway_links[i].offramp_warn_distance = wcf_freeway_links[i].offramp_warn_distance;
			//	server_freeway_links[i].anticip_warning_distance = wcf_freeway_links[i].anticip_warning_distance;
			//	server_freeway_links[i].anticip_warning_speed = wcf_freeway_links[i].anticip_warning_speed;
			//	server_freeway_links[i].nhov_lanes = wcf_freeway_links[i].nhov_lanes;
			//	server_freeway_links[i].hov_begin = wcf_freeway_links[i].hov_begin;
			//	server_freeway_links[i].hov_end = wcf_freeway_links[i].hov_end;
			//	server_freeway_links[i].hov_code = wcf_freeway_links[i].hov_code;
			//	server_freeway_links[i].hov_lanes = wcf_freeway_links[i].hov_lanes;
			//	server_freeway_links[i].hov_offramp_warn_distance = wcf_freeway_links[i].hov_offramp_warn_distance;
			//	server_freeway_links[i].hov_side = wcf_freeway_links[i].hov_side;
			//	server_freeway_links[i].hov_type = wcf_freeway_links[i].hov_type;
			//	server_freeway_links[i].hov_warn = wcf_freeway_links[i].hov_warn;
			//	server_freeway_links[i].hov_pct = wcf_freeway_links[i].hov_pct;
			//	server_freeway_links[i].cfmult = wcf_freeway_links[i].cfmult;
			//	server_freeway_links[i].first_detector = wcf_freeway_links[i].first_detector;
			//	server_freeway_links[i].grade = wcf_freeway_links[i].grade;
			//	server_freeway_links[i].tilt = wcf_freeway_links[i].tilt;
			//	server_freeway_links[i].curve = wcf_freeway_links[i].curve;
			//	server_freeway_links[i].pavement = wcf_freeway_links[i].pavement;
			//	server_freeway_links[i].shoulder_width = wcf_freeway_links[i].shoulder_width;
			//	server_freeway_links[i].lane_width = wcf_freeway_links[i].lane_width;
			//	server_freeway_links[i].barrier = wcf_freeway_links[i].barrier;
			//	server_freeway_links[i].datastation_id = wcf_freeway_links[i].datastation_id;
			//	server_freeway_links[i].datastation_location = wcf_freeway_links[i].datastation_location;
			//	server_freeway_links[i].truck_code = wcf_freeway_links[i].truck_code;
			//	server_freeway_links[i].truck_dir = wcf_freeway_links[i].truck_dir;
			//	server_freeway_links[i].truck_lane = wcf_freeway_links[i].truck_lane;
			//	server_freeway_links[i].etl_warn = wcf_freeway_links[i].etl_warn;
			//	server_freeway_links[i].exclude_type = wcf_freeway_links[i].exclude_type;
			//	server_freeway_links[i].multiplier_exit = wcf_freeway_links[i].multiplier_exit;
			//	server_freeway_links[i].startup_time = wcf_freeway_links[i].startup_time;
			//}
		}
		virtual array <Wcf_freeway_link> ^ GetServerFreewayData()
		{
			return server_freeway_links;
		}

		virtual void SetServerStreetData(array <Wcf_street_link> ^ wcf_street_links)
		{
			System::Array::Resize(server_street_links, wcf_street_links->Length);
			server_street_links = wcf_street_links;
			//for (int i = 0; i<wcf_street_links->Length;i++) 
			//{
			//	server_street_links[i].id = wcf_street_links[i].id;
			//	server_street_links[i].usn = wcf_street_links[i].usn;
			//	server_street_links[i].dsn = wcf_street_links[i].dsn;	
			//	server_street_links[i].thrunode = wcf_street_links[i].thrunode;	
			//	server_street_links[i].leftnode = wcf_street_links[i].leftnode;	
			//	server_street_links[i].rightnode = wcf_street_links[i].rightnode;	
			//	server_street_links[i].diagnode = wcf_street_links[i].diagnode;
			//	server_street_links[i].opposenode = wcf_street_links[i].opposenode;	
			//	server_street_links[i].length = wcf_street_links[i].length;
			//	server_street_links[i].fulllanes = wcf_street_links[i].fulllanes;
			//	server_street_links[i].leftturnbays = wcf_street_links[i].leftturnbays;
			//	server_street_links[i].rightturnbays = wcf_street_links[i].rightturnbays;
			//	server_street_links[i].lengthofleftbay = wcf_street_links[i].lengthofleftbay;
			//	server_street_links[i].lengthofrightbay = wcf_street_links[i].lengthofrightbay;
			//	server_street_links[i].freeflowspeed = wcf_street_links[i].freeflowspeed;
			//	server_street_links[i].channelization = wcf_street_links[i].channelization;
			//	server_street_links[i].leftpct = wcf_street_links[i].leftpct;
			//	server_street_links[i].thrupct = wcf_street_links[i].thrupct;
			//	server_street_links[i].rightpct = wcf_street_links[i].rightpct;
			//	server_street_links[i].diagpct = wcf_street_links[i].diagpct;
			//	server_street_links[i].grade = wcf_street_links[i].grade;
			//	server_street_links[i].distribution_code = wcf_street_links[i].distribution_code;
			//	server_street_links[i].startup_delay = wcf_street_links[i].startup_delay;
			//	server_street_links[i].discharge_hdwy = wcf_street_links[i].discharge_hdwy;
			//	server_street_links[i].rtor = wcf_street_links[i].rtor;
			//	server_street_links[i].ped_code = wcf_street_links[i].ped_code;
			//	server_street_links[i].lane1 = wcf_street_links[i].lane1;
			//	server_street_links[i].lane2 = wcf_street_links[i].lane2;
			//	server_street_links[i].cfmult = wcf_street_links[i].cfmult;
			//	server_street_links[i].sight_dist = wcf_street_links[i].sight_dist;
			//	server_street_links[i].first_detector = wcf_street_links[i].first_detector;
			//	server_street_links[i].shoulder_width = wcf_street_links[i].shoulder_width;
			//	server_street_links[i].lane_width = wcf_street_links[i].lane_width;
			//	server_street_links[i].ste_freq = wcf_street_links[i].ste_freq;
			//	server_street_links[i].ste_duration = wcf_street_links[i].ste_duration;
			//	server_street_links[i].signal_range = wcf_street_links[i].signal_range;
			//	server_street_links[i].centroid = wcf_street_links[i].centroid;
			//	server_street_links[i].centroid_label = wcf_street_links[i].centroid_label;
			//	server_street_links[i].exclude_type = wcf_street_links[i].exclude_type;
			//	server_street_links[i].multiplier_left = wcf_street_links[i].multiplier_left;
			//	server_street_links[i].multiplier_thru = wcf_street_links[i].multiplier_thru;
			//	server_street_links[i].multiplier_right = wcf_street_links[i].multiplier_right;
			//	server_street_links[i].multiplier_diag = wcf_street_links[i].multiplier_diag;
			//}
		}
		virtual array <Wcf_street_link> ^ GetServerStreetData()
		{
			return server_street_links;
		}

		virtual void SetServerCondTurnpctData(array <WCF_COND_TURNPCTS> ^ wcf_cond_turnpct_data)
		{
			System::Array::Resize(server_cond_turnpct_data, wcf_cond_turnpct_data->Length);
			for (int i = 0; i < wcf_cond_turnpct_data->Length; i++)
			{
				server_cond_turnpct_data[i].USN = wcf_cond_turnpct_data[i].USN;
				server_cond_turnpct_data[i].DSN = wcf_cond_turnpct_data[i].DSN;
				server_cond_turnpct_data[i].LEFTPCT = wcf_cond_turnpct_data[i].LEFTPCT;
				server_cond_turnpct_data[i].THRUPCT = wcf_cond_turnpct_data[i].THRUPCT;
				server_cond_turnpct_data[i].RIGHTPCT = wcf_cond_turnpct_data[i].RIGHTPCT;
				server_cond_turnpct_data[i].DIAGPCT = wcf_cond_turnpct_data[i].DIAGPCT;
			}
		}
		virtual array <WCF_COND_TURNPCTS>^ GetServerCondTurnpctData()
		{
			return server_cond_turnpct_data;
		}

		virtual  void SetServerEntryNodeDataSize(int n_wcf_entry_nodes)
		{
			nEntryNodes = n_wcf_entry_nodes;
		}

		virtual int GetServerEntryNodeDataSize()
		{
			return nEntryNodes;
		}

		virtual void SetServerEntryNodeData(array <WCF_ENTRYNODES_DATA> ^ wcf_entry_nodes)
		{
			System::Array::Resize(server_entry_nodes, wcf_entry_nodes->Length);
			for (int i = 0; i < wcf_entry_nodes->Length; i++)
			{
				server_entry_nodes[i].carpool_pct = wcf_entry_nodes[i].carpool_pct;
				server_entry_nodes[i].flowrate = wcf_entry_nodes[i].flowrate;
				server_entry_nodes[i].hov_violators_per10000 = wcf_entry_nodes[i].hov_violators_per10000;
				server_entry_nodes[i].lane_pct = wcf_entry_nodes[i].lane_pct;
				server_entry_nodes[i].Node_ID = wcf_entry_nodes[i].Node_ID;
				server_entry_nodes[i].truck_pct = wcf_entry_nodes[i].truck_pct;
				server_entry_nodes[i].SS_USN = wcf_entry_nodes[i].SS_USN;
				server_entry_nodes[i].SS_DSN = wcf_entry_nodes[i].SS_DSN;
			}
		}
		virtual array <WCF_ENTRYNODES_DATA> ^ GetServerEntryNodeData()
		{
			return server_entry_nodes;
		}

		virtual void SetServerFTCSignalDataSize(int n_wcf_ftc)
		{
			nFTC = n_wcf_ftc;
		}
		virtual int GetServerFTCSignalDataSize()
		{
			return nFTC;
		}

		virtual void SetServerFTCSignalData(array<WCF_FTC_DATA> ^ ftcs)
		{
			//			Console::WriteLine(L" in WCF server setServerFTCSignalData size= {0} ",ftcs->Length );			
			System::Array::Resize(server_ftc_signals, ftcs->Length);
			server_ftc_signals = ftcs;
			//for (int i = 0; i<ftcs->Length;i++) 
			//{
			//	server_ftc_signals[i].node = ftcs[i].node;
			//	server_ftc_signals[i].active_intervals = ftcs[i].active_intervals;	
			//	server_ftc_signals[i].approaches = ftcs[i].approaches;	
			//	server_ftc_signals[i].duration =ftcs[i].duration;	
			//	server_ftc_signals[i].approach = ftcs[i].approach;
			//	server_ftc_signals[i].signal_code = ftcs[i].signal_code;
			//	server_ftc_signals[i].cycle_length = ftcs[i].cycle_length;
			//	server_ftc_signals[i].current_interval = ftcs[i].current_interval;
			//	server_ftc_signals[i].time_in_interval = ftcs[i].time_in_interval;
			//}		
		}
		virtual array <WCF_FTC_DATA> ^ GetServerFTCSignalData()
		{			
			return server_ftc_signals ;
		}

		virtual void SetServerACData(array <WCF_AC> ^ wcf_acl)
		{
			System::Array::Resize(server_ac_signals, wcf_acl->Length);
			server_ac_signals = wcf_acl;
			//for (int i = 0; i<wcf_acl->Length;i++) 
			//{
			//	server_ac_signals[i].	node	=	wcf_acl[i].	node	;
			//	server_ac_signals[i].	cfails	=	wcf_acl[i].	cfails	;
			//	server_ac_signals[i].	adj	=	wcf_acl[i].	adj	;
			//	server_ac_signals[i].	actuated_mode	=	wcf_acl[i].	actuated_mode	;
			//	server_ac_signals[i].	leftarrow	=	wcf_acl[i].	leftarrow	;
			//	server_ac_signals[i].	thruarrow	=	wcf_acl[i].	thruarrow	;
			//	server_ac_signals[i].	rightarrow	=	wcf_acl[i].	rightarrow	;
			//	server_ac_signals[i].	diagarrow	=	wcf_acl[i].	diagarrow	;
			//	server_ac_signals[i].	min_green_time	=	wcf_acl[i].	min_green_time	;
			//	server_ac_signals[i].	max_green_time	=	wcf_acl[i].	max_green_time	;
			//	server_ac_signals[i].	default_extension_time	=	wcf_acl[i].	default_extension_time	;
			//	server_ac_signals[i].	gap_time	=	wcf_acl[i].	gap_time	;
			//	server_ac_signals[i].	times_before_reduction	=	wcf_acl[i].	times_before_reduction	;
			//	server_ac_signals[i].	time_to_reduce	=	wcf_acl[i].	time_to_reduce	;
			//	server_ac_signals[i].	min_gap_time	=	wcf_acl[i].	min_gap_time	;
			//	server_ac_signals[i].	yellow_change_int	=	wcf_acl[i].	yellow_change_int	;
			//	server_ac_signals[i].	red_clear_int	=	wcf_acl[i].	red_clear_int	;
			//	server_ac_signals[i].	ped_allowed	=	wcf_acl[i].	ped_allowed	;
			//	server_ac_signals[i].	n_direct_approaches	=	wcf_acl[i].	n_direct_approaches	;
			//	server_ac_signals[i].	direct_approach_USN	=	wcf_acl[i].	direct_approach_USN	;
			//	server_ac_signals[i].	cycle_length	=	wcf_acl[i].	cycle_length	;
			//	server_ac_signals[i].	offset	=	wcf_acl[i].	offset	;
			//	server_ac_signals[i].	walk_time	=	wcf_acl[i].	walk_time	;
			//	server_ac_signals[i].	walk_clearance_time	=	wcf_acl[i].	walk_clearance_time	;
			//	server_ac_signals[i].	ring_phase	=	wcf_acl[i].	ring_phase	;
			//	server_ac_signals[i].	detector_count	=	wcf_acl[i].	detector_count	;
			//	server_ac_signals[i].	detector_list	=	wcf_acl[i].	detector_list	;
			//	server_ac_signals[i].	transition_method	=	wcf_acl[i].	transition_method	;
			//	server_ac_signals[i].	max_add	=	wcf_acl[i].	max_add	;
			//	server_ac_signals[i].	max_subtract	=	wcf_acl[i].	max_subtract	;
			//	server_ac_signals[i].	force_off_times	=	wcf_acl[i].	force_off_times	;

			//	server_ac_signals[i].	local_cycle_timer	=	wcf_acl[i].	local_cycle_timer	;
			//	server_ac_signals[i].	splits	=	wcf_acl[i].	splits	;
			//	server_ac_signals[i].	min_splits	=	wcf_acl[i].	min_splits	;
			//	server_ac_signals[i].	new_cycle_length	=	wcf_acl[i].	new_cycle_length	;
			//	server_ac_signals[i].	new_offset	=	wcf_acl[i].	new_offset	;
			//	server_ac_signals[i].	new_splits	=	wcf_acl[i].	new_splits	;

			//	server_ac_signals[i].	green_phases	=	wcf_acl[i].	green_phases	;
			//	server_ac_signals[i].	yellow_phases	=	wcf_acl[i].	yellow_phases	;
			//}
		}
		virtual array<WCF_AC>^ GetServerACData()
		{
			return server_ac_signals;
		}

		virtual void SetServerFreewayDetectorInputs(array <WCF_DETECTOR_INPUTS> ^ wcf_fdet_inputs)
		{
			System::Array::Resize(server_fdet_inputs, wcf_fdet_inputs->Length);
			server_fdet_inputs = wcf_fdet_inputs;
			//for (int i = 0; i < wcf_fdet_inputs->Length; i++)
			//{
			//	server_fdet_inputs[i].usn = wcf_fdet_inputs[i].usn;
			//	server_fdet_inputs[i].dsn = wcf_fdet_inputs[i].dsn;
			//	server_fdet_inputs[i].associated_phase = wcf_fdet_inputs[i].associated_phase;
			//	server_fdet_inputs[i].station_id = wcf_fdet_inputs[i].station_id;
			//	server_fdet_inputs[i].location = wcf_fdet_inputs[i].location;
			//	server_fdet_inputs[i].link = wcf_fdet_inputs[i].link;
			//	server_fdet_inputs[i].lane1 = wcf_fdet_inputs[i].lane1;
			//	server_fdet_inputs[i].lane2 = wcf_fdet_inputs[i].lane2;
			//	server_fdet_inputs[i].zone_length = wcf_fdet_inputs[i].zone_length;
			//	server_fdet_inputs[i].delay_time = wcf_fdet_inputs[i].delay_time;
			//	server_fdet_inputs[i].carryover_time = wcf_fdet_inputs[i].carryover_time;
			//	server_fdet_inputs[i].type_code = wcf_fdet_inputs[i].type_code;
			//	server_fdet_inputs[i].operation_code = wcf_fdet_inputs[i].operation_code;
			//	server_fdet_inputs[i].detection_zone = wcf_fdet_inputs[i].detection_zone;	
			//}
		}
		virtual array <WCF_DETECTOR_INPUTS>^ GetServerFreewayDetectorInputs()
		{
			return server_fdet_inputs;
		}

		virtual void SetServerStreetDetectorInputs(array <WCF_DETECTOR_INPUTS> ^ wcf_sdet_inputs)
		{
			System::Array::Resize(server_sdet_inputs, wcf_sdet_inputs->Length);
			server_sdet_inputs = wcf_sdet_inputs;
			//for (int i = 0; i < wcf_sdet_inputs->Length; i++)
			//{
			//	server_sdet_inputs[i].usn = wcf_sdet_inputs[i].usn;
			//	server_sdet_inputs[i].dsn = wcf_sdet_inputs[i].dsn;
			//	server_sdet_inputs[i].associated_phase = wcf_sdet_inputs[i].associated_phase;
			//	server_sdet_inputs[i].station_id = wcf_sdet_inputs[i].station_id;
			//	server_sdet_inputs[i].location = wcf_sdet_inputs[i].location;
			//	server_sdet_inputs[i].link = wcf_sdet_inputs[i].link;
			//	server_sdet_inputs[i].lane1 = wcf_sdet_inputs[i].lane1;
			//	server_sdet_inputs[i].lane2 = wcf_sdet_inputs[i].lane2;
			//	server_sdet_inputs[i].zone_length = wcf_sdet_inputs[i].zone_length;
			//	server_sdet_inputs[i].delay_time = wcf_sdet_inputs[i].delay_time;
			//	server_sdet_inputs[i].carryover_time = wcf_sdet_inputs[i].carryover_time;
			//	server_sdet_inputs[i].type_code = wcf_sdet_inputs[i].type_code;
			//	server_sdet_inputs[i].operation_code = wcf_sdet_inputs[i].operation_code;
			//	server_sdet_inputs[i].detection_zone = wcf_sdet_inputs[i].detection_zone;
			//	
			//}
		}
		virtual array <WCF_DETECTOR_INPUTS>^ GetServerStreetDetectorInputs()
		{
			return server_sdet_inputs;
		}

		virtual void SetServerFreewayDetectorOutputs(array <WCF_DETECTOR_OUTPUTS>^ wcf_fdet_outputs)
		{
			System::Array::Resize(server_fdet_outputs, wcf_fdet_outputs->Length);
			for (int i = 0; i < wcf_fdet_outputs->Length; i++)
			{
				server_fdet_outputs[i].	current_count	= wcf_fdet_outputs[i].	current_count	;
				server_fdet_outputs[i].	current_state	= wcf_fdet_outputs[i].	current_state	;
				server_fdet_outputs[i].	previous_state	= wcf_fdet_outputs[i].	previous_state	;
				server_fdet_outputs[i].	hdwy_count	= wcf_fdet_outputs[i].	hdwy_count	;
				server_fdet_outputs[i].	hdwy_total	= wcf_fdet_outputs[i].	hdwy_total	;
				server_fdet_outputs[i].	on_time	= wcf_fdet_outputs[i].	on_time	;
				server_fdet_outputs[i].	speed_total	= wcf_fdet_outputs[i].	speed_total	;
				server_fdet_outputs[i].	length_total	= wcf_fdet_outputs[i].	length_total	;
			}
		}

		virtual array <WCF_DETECTOR_OUTPUTS>^ GetServerFreewayDetectorOutputs()
		{
			return server_fdet_outputs;
		}

		virtual void SetServerStreetDetectorOutputs(array <WCF_DETECTOR_OUTPUTS>^ wcf_sdet_outputs)
		{
			System::Array::Resize(server_sdet_outputs, wcf_sdet_outputs->Length);
			for (int i = 0; i < wcf_sdet_outputs->Length; i++)
			{
				server_sdet_outputs[i].	current_count	= wcf_sdet_outputs[i].	current_count	;
				server_sdet_outputs[i].	current_state	= wcf_sdet_outputs[i].	current_state	;
				server_sdet_outputs[i].	previous_state	= wcf_sdet_outputs[i].	previous_state	;
				server_sdet_outputs[i].	hdwy_count	= wcf_sdet_outputs[i].	hdwy_count	;
				server_sdet_outputs[i].	hdwy_total	= wcf_sdet_outputs[i].	hdwy_total	;
				server_sdet_outputs[i].	on_time	= wcf_sdet_outputs[i].	on_time	;
				server_sdet_outputs[i].	speed_total	= wcf_sdet_outputs[i].	speed_total	;
				server_sdet_outputs[i].	length_total	= wcf_sdet_outputs[i].	length_total	;
			}
		}
		
		virtual array <WCF_DETECTOR_OUTPUTS>^ GetServerStreetDetectorOutputs()
		{
			return server_sdet_outputs;
		}

		virtual void SetServerBusRouteInputs(array <WCF_BUSROUTE_DATA>^ wcf_busroute_inputs)
		{
			System::Array::Resize(server_busroute_inputs, wcf_busroute_inputs->Length);
			for (int i = 0; i < wcf_busroute_inputs->Length; i++)
			{
				server_busroute_inputs[i].number = wcf_busroute_inputs[i].number;
				server_busroute_inputs[i].hdwy = wcf_busroute_inputs[i].hdwy;
				server_busroute_inputs[i].offset = wcf_busroute_inputs[i].offset;
				server_busroute_inputs[i].nodes = wcf_busroute_inputs[i].nodes;
				server_busroute_inputs[i].route_nodes = wcf_busroute_inputs[i].route_nodes;
				server_busroute_inputs[i].stationlist = wcf_busroute_inputs[i].stationlist;
				server_busroute_inputs[i].persontrips = wcf_busroute_inputs[i].persontrips;
				server_busroute_inputs[i].timer = wcf_busroute_inputs[i].timer;
				server_busroute_inputs[i].traveltime = wcf_busroute_inputs[i].traveltime;
				server_busroute_inputs[i].trips = wcf_busroute_inputs[i].trips;
			}
		}
		virtual array <WCF_BUSROUTE_DATA>^ GetServerBusRouteInputs()
		{
			return server_busroute_inputs;
		}

		virtual void SetServerBusStationInputs(array <WCF_BUSSTATION_DATA>^ wcf_busstation_inputs)
		{
			System::Array::Resize(server_busstation_inputs, wcf_busstation_inputs->Length);
			for (int i = 0; i < wcf_busstation_inputs->Length; i++)
			{
				server_busstation_inputs[i].block_code = wcf_busstation_inputs[i].block_code;
				server_busstation_inputs[i].usn = wcf_busstation_inputs[i].usn;
				server_busstation_inputs[i].dsn = wcf_busstation_inputs[i].dsn;
				server_busstation_inputs[i].capacity = wcf_busstation_inputs[i].capacity;
				server_busstation_inputs[i].type_code = wcf_busstation_inputs[i].type_code;
				server_busstation_inputs[i].location = wcf_busstation_inputs[i].location;
				server_busstation_inputs[i].dwell = wcf_busstation_inputs[i].dwell;
				server_busstation_inputs[i].bypass_pct = wcf_busstation_inputs[i].bypass_pct;
				server_busstation_inputs[i].next_station = wcf_busstation_inputs[i].next_station;
				server_busstation_inputs[i].pocket_lane = wcf_busstation_inputs[i].pocket_lane;
				server_busstation_inputs[i].front = wcf_busstation_inputs[i].front;
				server_busstation_inputs[i].count = wcf_busstation_inputs[i].count;
				server_busstation_inputs[i].dwell_time = wcf_busstation_inputs[i].dwell_time;
				server_busstation_inputs[i].empty_time = wcf_busstation_inputs[i].empty_time;
				server_busstation_inputs[i].overflow_time = wcf_busstation_inputs[i].overflow_time;
			}
		}
		virtual array <WCF_BUSSTATION_DATA>^ GetServerBusStationInputs()
		{
			return server_busstation_inputs;	
		}

		virtual void SetServerIncidentData_Inputs(array<WCF_INCIDENT_DATA>^ wcf_incident_data_inputs)
		{
			System::Array::Resize(server_incident_inputs, wcf_incident_data_inputs->Length);
			server_incident_inputs = wcf_incident_data_inputs;
			//for (int i = 0; i < wcf_incident_data_inputs->Length; i++)
			//{
			//	server_incident_inputs[i].link = wcf_incident_data_inputs[i].link;
			//	server_incident_inputs[i].begin_point = wcf_incident_data_inputs[i].begin_point;
			//	server_incident_inputs[i].begin_time = wcf_incident_data_inputs[i].begin_time;
			//	server_incident_inputs[i].end_point = wcf_incident_data_inputs[i].end_point;
			//	server_incident_inputs[i].end_time = wcf_incident_data_inputs[i].end_time;
			//	server_incident_inputs[i].rbnf = wcf_incident_data_inputs[i].rbnf;
			//	server_incident_inputs[i].warn_point = wcf_incident_data_inputs[i].warn_point;
			//	server_incident_inputs[i].code = wcf_incident_data_inputs[i].code;
			//}
		}
		virtual array<WCF_INCIDENT_DATA>^ GetServerIncidentData_Inputs()
		{
			return server_incident_inputs;
		}

		virtual void SetServerXYCoordInputs(array <WCF_NODE_LOCATION_DATA>^ wcf_xy_coord_inputs)
		{
			//Console::WriteLine(L" in WCF server server_xy_coord_inputs size= {0} ",server_xy_coord_inputs->Length );
			System::Array::Resize(server_xy_coord_inputs, wcf_xy_coord_inputs->Length);
			//Console::WriteLine(L" in WCF server server_xy_coord_inputs size= {0} ",server_xy_coord_inputs->Length );
			server_xy_coord_inputs = wcf_xy_coord_inputs;
			//for (int i = 0; i < wcf_xy_coord_inputs->Length; i++)
			//{
			//	server_xy_coord_inputs[i].x = wcf_xy_coord_inputs[i].x;
			//	server_xy_coord_inputs[i].y = wcf_xy_coord_inputs[i].y;
			//	server_xy_coord_inputs[i].latitude = wcf_xy_coord_inputs[i].latitude;
			//	server_xy_coord_inputs[i].longitude = wcf_xy_coord_inputs[i].longitude;
			//	server_xy_coord_inputs[i].elevation = wcf_xy_coord_inputs[i].elevation;
			//	server_xy_coord_inputs[i].is_defined = wcf_xy_coord_inputs[i].is_defined;
			//}
		}
		virtual array <WCF_NODE_LOCATION_DATA>^ GetServerXYCoordInputs()
		{
			return server_xy_coord_inputs;
		}

		virtual void SetServerRampmeterInputs(array <WCF_RM_DATA>^ wcf_rampmeter_inputs)
		{
			System::Array::Resize(server_rampmeter_inputs, wcf_rampmeter_inputs->Length);
			for (int i = 0; i < wcf_rampmeter_inputs->Length; i++)
			{
				server_rampmeter_inputs[i].dsn = wcf_rampmeter_inputs[i].dsn;
				server_rampmeter_inputs[i].link = wcf_rampmeter_inputs[i].link;
				server_rampmeter_inputs[i].control = wcf_rampmeter_inputs[i].control;
				server_rampmeter_inputs[i].onset = wcf_rampmeter_inputs[i].onset;
				server_rampmeter_inputs[i].state = wcf_rampmeter_inputs[i].state;
				server_rampmeter_inputs[i].detector = wcf_rampmeter_inputs[i].detector;
				server_rampmeter_inputs[i].capacity = wcf_rampmeter_inputs[i].capacity;
				server_rampmeter_inputs[i].speed = wcf_rampmeter_inputs[i].speed;
				server_rampmeter_inputs[i].headway = wcf_rampmeter_inputs[i].headway;
				server_rampmeter_inputs[i].timer = wcf_rampmeter_inputs[i].timer;
				server_rampmeter_inputs[i].updint = wcf_rampmeter_inputs[i].updint;
				server_rampmeter_inputs[i].twopergreen = wcf_rampmeter_inputs[i].twopergreen;

			}
		}
		virtual array <WCF_RM_DATA>^ GetServerRampmeterInputs()
		{
			return server_rampmeter_inputs;
		}

		virtual void SetServerParkingData(array<WCF_PARKING_DATA>^ wcf_parking_inputs)
		{
			System::Array::Resize(server_parking_inputs, wcf_parking_inputs->Length);
			for (int i = 0; i < wcf_parking_inputs->Length; ++i)
			{
				server_parking_inputs[i].usn = wcf_parking_inputs[i].usn;
				server_parking_inputs[i].dsn = wcf_parking_inputs[i].dsn;
				server_parking_inputs[i].duration = wcf_parking_inputs[i].duration;
				server_parking_inputs[i].freq = wcf_parking_inputs[i].freq;
				server_parking_inputs[i].left_start = wcf_parking_inputs[i].left_start;
				server_parking_inputs[i].left_len = wcf_parking_inputs[i].left_len;
				server_parking_inputs[i].right_start = wcf_parking_inputs[i].right_start;
				server_parking_inputs[i].right_len = wcf_parking_inputs[i].right_len;
			}
		}

		virtual array<WCF_PARKING_DATA>^ GetServerParkingData()
		{
			return server_parking_inputs;
		}

		virtual void SetServerEventData(array<WCF_EVENT_DATA>^ wcf_event_inputs)
		{
			System::Array::Resize(server_event_inputs, wcf_event_inputs->Length);
			server_event_inputs = wcf_event_inputs;
			//for (int i = 0; i < wcf_event_inputs->Length; ++i)
			//{
			//	server_event_inputs[i].begin_time = wcf_event_inputs[i].begin_time;
			//	server_event_inputs[i].end_time = wcf_event_inputs[i].end_time;
			//	server_event_inputs[i].lane = wcf_event_inputs[i].lane;
			//	server_event_inputs[i].link = wcf_event_inputs[i].link;
			//	server_event_inputs[i].location = wcf_event_inputs[i].location;
			//	server_event_inputs[i].type = wcf_event_inputs[i].type;
			//}
		}

		virtual array<WCF_EVENT_DATA>^ GetServerEventData()
		{
			return server_event_inputs;
		}

		virtual void SetServerDiversionData(array<WCF_DIVERSION_DATA>^ wcf_diversion_inputs)
		{
			System::Array::Resize(server_diversion_inputs, wcf_diversion_inputs->Length);
			for (int i = 0; i < wcf_diversion_inputs->Length; ++i)
			{
				server_diversion_inputs[i].link = wcf_diversion_inputs[i].link;
				server_diversion_inputs[i].begin_time = wcf_diversion_inputs[i].begin_time;
				server_diversion_inputs[i].end_time = wcf_diversion_inputs[i].end_time;
				server_diversion_inputs[i].location = wcf_diversion_inputs[i].location;
				server_diversion_inputs[i].pathid = wcf_diversion_inputs[i].pathid;
				server_diversion_inputs[i].percentage = wcf_diversion_inputs[i].percentage;
				server_diversion_inputs[i].speed = wcf_diversion_inputs[i].speed;
			}
		}

		virtual array<WCF_DIVERSION_DATA>^ GetServerDiversionData()
		{
			return server_diversion_inputs;
		}

		virtual void SetServerTRFFile(String ^TRFFile)
		{
			TRFFileName = TRFFile;
		}

		virtual String^ GetServerTRFFile()
		{
			return TRFFileName;
		}

		virtual void SetServerGPSRefFile(String ^GPSRefFile)
		{
			GPSRefFileName = GPSRefFile;
		}

		virtual String^ GetServerGPSRefFile()
		{
			return GPSRefFileName;
		}

		virtual void SetServerGPSRefNodes(array <WCF_NODE_LOCATION_DATA>^ wcf_gps_ref_nodes)
		{
			System::Array::Resize(server_gps_ref_nodes, wcf_gps_ref_nodes->Length);
			//Console::WriteLine(L" in WCF server server_gps_ref_nodes size= {0} ",server_gps_ref_nodes->Length );
			for (int i = 0; i < wcf_gps_ref_nodes->Length; i++)
			{
				server_gps_ref_nodes[i].x = wcf_gps_ref_nodes[i].x;
				server_gps_ref_nodes[i].y = wcf_gps_ref_nodes[i].y;
				server_gps_ref_nodes[i].latitude = wcf_gps_ref_nodes[i].latitude;
				server_gps_ref_nodes[i].longitude = wcf_gps_ref_nodes[i].longitude;
				server_gps_ref_nodes[i].elevation = wcf_gps_ref_nodes[i].elevation;
				server_gps_ref_nodes[i].is_defined = wcf_gps_ref_nodes[i].is_defined;
			}
		}
		
		virtual array <WCF_NODE_LOCATION_DATA>^ GetServerGPSRefNodes()
		{
			return server_gps_ref_nodes;
		}

		virtual void SetServerWriteTextFlag(int wcf_write_text_flag)
		{
			server_write_text_flag = wcf_write_text_flag;
		}
		
		virtual int GetServerWriteTextFlag()
		{
			return server_write_text_flag;
		}

		virtual void SetServerTimingPlanData(array <WCF_TIMING_PLAN>^ wcf_timing_plan_data)
		{
			System::Array::Resize(server_timing_plan_data, wcf_timing_plan_data->Length);
			for (int i = 0; i < wcf_timing_plan_data->Length; ++i)
			{
				server_timing_plan_data[i].mingreens = wcf_timing_plan_data[i].mingreens;
				server_timing_plan_data[i].passage = wcf_timing_plan_data[i].passage;
				server_timing_plan_data[i].maxgreens = wcf_timing_plan_data[i].maxgreens;
				server_timing_plan_data[i].yellowchange = wcf_timing_plan_data[i].yellowchange;
				server_timing_plan_data[i].redclear = wcf_timing_plan_data[i].redclear;
			}
		}
		virtual array <WCF_TIMING_PLAN>^ GetServerTimingPlanData()
		{
			return server_timing_plan_data;
		}
		
		virtual void SetServerSignalControllerData(array <WCF_SIGNAL_CONTROLLER_DATA>^ wcf_signal_controller_data)
		{
			System::Array::Resize(server_timing_plan_data, wcf_signal_controller_data->Length);
			for (int i = 0; i < wcf_signal_controller_data->Length; ++i)
			{
				server_signal_controller_data[i].holds = wcf_signal_controller_data[i].holds;
				server_signal_controller_data[i].force_offs = wcf_signal_controller_data[i].force_offs;
				server_signal_controller_data[i].phase_calls = wcf_signal_controller_data[i].phase_calls;
				server_signal_controller_data[i].greens = wcf_signal_controller_data[i].greens;
				server_signal_controller_data[i].yellows = wcf_signal_controller_data[i].yellows;
				server_signal_controller_data[i].ring_code1 = wcf_signal_controller_data[i].ring_code1;
				server_signal_controller_data[i].ring_code2 = wcf_signal_controller_data[i].ring_code2;
			}
		}
		virtual array <WCF_SIGNAL_CONTROLLER_DATA>^ GetServerSignalControllerData()
		{
			return server_signal_controller_data;
		}

		virtual void SetServerRunnerData(array <WCF_RUNNER_DATA>^ wcf_runner_data)
		{
			System::Array::Resize(server_runner_data, wcf_runner_data->Length);
			for (int i = 0; i < wcf_runner_data->Length; ++i)
			{
				server_runner_data[i].initialization_dat_file_path = wcf_runner_data[i].initialization_dat_file_path;
				server_runner_data[i].n_street_links = wcf_runner_data[i].n_street_links;
				server_runner_data[i].simulation_time_step = wcf_runner_data[i].simulation_time_step;
				server_runner_data[i].use_ntcip = wcf_runner_data[i].use_ntcip;
				server_runner_data[i].use_dcs = wcf_runner_data[i].use_dcs;
				server_runner_data[i].n_sdet = wcf_runner_data[i].n_sdet;
				server_runner_data[i].curTimeStep = wcf_runner_data[i].curTimeStep;
				server_runner_data[i].extension = wcf_runner_data[i].extension;
				server_runner_data[i].forceoff = wcf_runner_data[i].forceoff;
				server_runner_data[i].dcs_error = wcf_runner_data[i].dcs_error;
				server_runner_data[i].n_acs = wcf_runner_data[i].n_acs;
				server_runner_data[i].green_phases = wcf_runner_data[i].green_phases;
				server_runner_data[i].yellow_phases = wcf_runner_data[i].yellow_phases;
			}
		}
		virtual array <WCF_RUNNER_DATA>^ GetServerRunnerData()
		{
			return server_runner_data;
		}

		virtual int GetRunnerState()
		{
			return server_runner_state;
		}
		virtual void SetRunnerState(int s)
		{
			server_runner_state = s;
		}
		virtual int GetTCAState()
		{
			return server_tca_state;
		}

		virtual void SetTCAState(int s)
		{
			server_tca_state = s;
		}

		virtual void SetServerCoordinationData(array <WCF_COORDINATION_DATA>^ wcf_coordination_data)
		{
			System::Array::Resize(server_coordination_data, wcf_coordination_data->Length);
			for (int i = 0; i < wcf_coordination_data->Length; ++i)
			{
				server_coordination_data[i].error = wcf_coordination_data[i].error;
				server_coordination_data[i].node = wcf_coordination_data[i].node;
				server_coordination_data[i].local_cycle_timer = wcf_coordination_data[i].local_cycle_timer;
				server_coordination_data[i].cycle_length = wcf_coordination_data[i].cycle_length;
				server_coordination_data[i].offset = wcf_coordination_data[i].offset;
				server_coordination_data[i].max_add = wcf_coordination_data[i].max_add;
				server_coordination_data[i].max_subt = wcf_coordination_data[i].max_subt;
				server_coordination_data[i].iact = wcf_coordination_data[i].iact;
				server_coordination_data[i].method = wcf_coordination_data[i].method;
				server_coordination_data[i].splits = wcf_coordination_data[i].splits;
				server_coordination_data[i].min_splits = wcf_coordination_data[i].min_splits;

				//server_coordination_data[i].coord_set = wcf_coordination_data[i].coord_set;
			}
		}

		virtual array <WCF_COORDINATION_DATA>^ GetServerCoordinationData()
		{
			return server_coordination_data;
		}


		virtual void SetServerNodeID(int NodeID)
		{
			server_node_id = NodeID;
		}
		virtual int GetServerNodeID()
		{
			return server_node_id;
		}
		virtual void SetServerUSNID(int usn_id)
		{
			server_usn_id = usn_id;
		}
		virtual int GetServerUSNID()
		{
			return server_usn_id;
		}
		virtual void SetServerDSNID(int dsn_id)
		{
			server_dsn_id = dsn_id;
		}
		virtual int GetServerDSNID()
		{
			return server_dsn_id;
		}
		virtual void SetServerMaxAdd(float max_add)
		{
			server_max_add = max_add;
		}
		virtual float GetServerMaxAdd()
		{
			return server_max_add;
		}
		virtual void SetServerMaxSub(float max_subt)
		{
			server_max_sub = max_subt;
		}
		virtual float GetServerMaxSub()
		{
			return server_max_sub;
		}

		virtual void SetServerNewCycleLength(int wcf_node_id, float wcf_new_cycle_length)
		{
			server_node_id = wcf_node_id;
			server_new_cycle_length = wcf_new_cycle_length;
		}
		virtual float GetServerNewCycleLength()
		{
			return server_new_cycle_length;
		}
		virtual void SetServerCycleLength(int wcf_node_id, float wcf_cycle_length)
		{
			server_node_id = wcf_node_id;
			server_cycle_length = wcf_cycle_length;
		}
		virtual float GetServerCycleLength()
		{
			return server_cycle_length;
		}
		
		virtual void SetServerNewOffset(int wcf_node_id, float wcf_new_offset)
		{
			server_node_id = wcf_node_id;
			server_new_offset = wcf_new_offset;
		}
		virtual float GetServerNewOffset()
		{
			return server_new_offset;
		}
		virtual void SetServerOffset(int wcf_node_id, float wcf_offset)
		{
			server_node_id = wcf_node_id;
			server_offset = wcf_offset;
		}
		virtual float GetServerOffset()
		{
			return server_offset;
		}

		virtual void SetServerLocalCycleTimer(int wcf_node_id, float wcf_local_cycle_timer)
		{
			server_node_id = wcf_node_id;
			server_local_cycle_timer = wcf_local_cycle_timer;
		}
		virtual float GetServerLocalCycleTimer()
		{
			return server_local_cycle_timer;
		}

		virtual void SetServerNewSplits(int wcf_node_id, array<float>^ wcf_new_splits)
		{
			server_node_id = wcf_node_id;
			server_new_splits = wcf_new_splits;
			//System::Array::Resize(server_new_splits, wcf_new_splits->Length);
			//for (int i = 0; i < wcf_new_splits->Length; ++i)
			//{
			//	server_new_splits[i] = wcf_new_splits[i];
			//}
		}
		virtual array<float>^ GetServerNewSplits()
		{
			return server_new_splits;
		}
		virtual void SetServerSplits(int wcf_node_id, array<float>^ wcf_splits)
		{
			server_node_id = wcf_node_id;
			server_splits = wcf_splits;
		}
		virtual array<float>^ GetServerSplits()
		{
			return server_splits;
		}
		
		virtual void SetServerMinSplits(int wcf_node_id, array<float>^ wcf_min_splits)
		{
			server_node_id = wcf_node_id;
			server_min_splits = wcf_min_splits;
		}
		virtual array<float>^ GetServerMinSplits()
		{
			return server_min_splits;
		}

		virtual void SetServerNodeGYR(int wcf_node_id, array<int>^ wcf_greens_yellows)
		{
			server_node_id = wcf_node_id;
			server_greens_yellows = wcf_greens_yellows;
		}
		virtual array<int>^ GetServerNodeGYR()
		{
			return server_greens_yellows;
		}

		virtual void SetServerTransitionMethod(int wcf_node_id, int wcf_transition_method, float wcf_max_add, float wcf_max_sub)
		{
			server_node_id = wcf_node_id;
			server_transition_method = wcf_transition_method;
			server_max_add = wcf_max_add;
			server_max_sub = wcf_max_sub;
		}
		virtual int GetServerTransitionMethod()
		{
			return server_transition_method;
		}

		virtual void SetServerAverageTravelTime(int wcf_usn_id, int wcf_dsn_id, float wcf_average_travel_time)
		{
			server_usn_id = wcf_usn_id;
			server_dsn_id = wcf_dsn_id;
			server_average_travel_time = wcf_average_travel_time;
		}

		virtual float GetServerAverageTravelTime()
		{
			return server_average_travel_time;
		}

		virtual array <WCF_CONTROLLER_DATA>^ GetControllerData()
		{
			return server_controller_data;
		}
		virtual void SetControllerData(array <WCF_CONTROLLER_DATA>^ wcf_controller_data)
		{
			server_controller_data = wcf_controller_data;
		}
		virtual void SetControllerDataNoPhaseStates(array <WCF_CONTROLLER_DATA>^ wcf_controller_data)
		{
			for (int i = 0; i < wcf_controller_data->Length; ++i)
			{
				server_controller_data[i].phase_calls = wcf_controller_data[i].phase_calls;
				server_controller_data[i].is_etrunner_ready = wcf_controller_data[i].is_etrunner_ready;
			}	
		}
		virtual void ResizeControllerData(int number_of_controllers)
		{
			System::Array::Resize(server_controller_data, number_of_controllers);
		}
		virtual int GetControllerDataState()
		{
			return server_controller_data_state;
		}
		virtual void SetControllerDataState(int wcf_controller_data_state)
		{
			server_controller_data_state = wcf_controller_data_state;
		}
		virtual void IncreaseControllerDataState()
		{
			server_controller_data_state++;
		}
		virtual void SetControllerDataByIndex(array <WCF_CONTROLLER_DATA>^ wcf_controller_data, int index)
		{
			server_controller_data[index] = wcf_controller_data[index];
		}
		virtual void SetControllerPhasesByIndex(int green_phases, int yellow_phases, int index)
		{
			server_controller_data[index].green_phases = green_phases;
			server_controller_data[index].yellow_phases = yellow_phases;
		}
		virtual void SetControllerReadyByIndex(int is_controller_ready, int index)
		{
			server_controller_data[index].is_controller_ready = is_controller_ready;
		}
		virtual void SetControllerIsEtrunnerReadyByIndex(int is_etrunner_ready, int index)
		{
			server_controller_data[index].is_etrunner_ready = is_etrunner_ready;
		}
		virtual int GetControllerIsEtrunnerReadyByIndex(int index)
		{
			return server_controller_data[index].is_etrunner_ready;
		}

		virtual void SetServerStreetMOEData(array <WCF_STREET_MOE_DATA>^ wcf_street_moe_data)
		{
			server_street_moe_data = wcf_street_moe_data;
		}
		virtual array <WCF_STREET_MOE_DATA>^ GetServerStreetMOEData()
		{
			return server_street_moe_data;
		}
	};
}
