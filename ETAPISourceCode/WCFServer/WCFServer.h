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
// WCFServer.h

#pragma once
#include <string>
#include <array>
#include <exception>
#using <System.ServiceModel.dll>


using namespace System;
using namespace System::ServiceModel;
using namespace System::Runtime::Serialization;


namespace WCFServer
{

	public value struct NewVehicle
	{
		float time;
		int inode;
		int pathid;
		int driver;
		int fleet;
		int vtype;
		int overspeed;
		int range;
	};


	public value struct Signal
	{
		int nodeid;
		int interval;	
		int duration;
	};


	public value struct WCF_FTC_DATA
	{
		int active_intervals;
		array<int>^ approach; //[6]
		int approaches;
		int current_interval;
		float cycle_length;
		array<float>^ duration; //[12]
		int external_control;
		int node;
		int offset;
		int range;
		array<array<int>^>^ signal_code; //[12][6]
		float time_in_interval;
	};

	public value struct FixTimeControl
	{
		int active_intervals;
		array<int>^ approach; //[5]
		int approaches;
		array<int>^ duration; 
	};


	public value struct WCF_PHASE_DATA
	{
		int actuations;
		float additional_time;
		int arrival_time;
		array<int>^ const_demand_period_begin; //[5]
		array<int>^ const_demand_period_end; //[5]
		array<int>^ detector1_id; //[5]
		array<int>^ detector2_id; //[5]
		array<int>^ detector3_id; //[5]
		int dontwalk_duration;
		int dual_service_code;
		int doubleentry_code;
		int in_use;
		int initial_intcode;
		int forceoff_time;
		int gap_reduct_code;
		int lag_code;
		int lvpassage_code;
		float max_extension;
		float max_gap;
		float max_green;
		float max_init_interval;
		int max_recall_code;
		float min_cond_service_time;
		float min_gap;
		float min_green;
		int min_recall_code;
		array<int>^ movement_code; //[5]
		int overlap_code;
		float passage_time;
		int ped_headway;
		int ped_intensity;
		int ped_recall_code;
		int ped_rest_code;
		int permcodes;
		int pre_force_termination_code;
		float red_clear_int;
		int redlock_code;
		float redrevert_time;
		float reduce_by_time;
		float reduction_time;
		int restinred_code;
		int simult_gap_code;
		float yellow_change_int;
		int yellowlock_code;
		int walk_duration;
	};


	public value struct WCF_AC
	{
		int node;
		int cfails;
		int adj;
		array<int>^ actuated_mode; // [8]
		array<array<int>^>^ leftarrow; //[10][8]
		array<array<int>^>^ thruarrow; //[10][8];
		array<array<int>^>^ rightarrow; //[10][8];
		array<array<int>^>^ diagarrow; //[10][8];
		array<float>^ min_green_time; //[8];
		array<float>^ max_green_time; //[8];
		array<float>^ default_extension_time; //[8];
		array<float>^ gap_time; //[8];
		array<float>^ times_before_reduction; //[8];
		array<float>^ time_to_reduce; //[8];
		array<float>^ min_gap_time; //[8];
		array<float>^ yellow_change_int; //[8];
		array<float>^ red_clear_int; //[8];
		array<int>^ ped_allowed; //[8];
		int n_direct_approaches;
		array<int>^ direct_approach_USN; //[10];//->[20]
		int n_indirect_approaches;
		array<int>^ indirect_approach_USN; //[5];
		array<int>^ indirect_approach_DSN; //[5];
		float cycle_length;
		float offset;
		array<float>^ walk_time; //[8];
		array<float>^ walk_clearance_time; //[8];
		//array<array<int>^>^ ring_phase; //[4][2];
		array<int>^ ring_phase; //[8]
		int detector_count;
		array<int>^ detector_list; // [64]
		int transition_method;    //April29
		float max_add;            //April29 
		float max_subtract;       //April29
		array<float>^ force_off_times; //April29 //[8]

		//WCF_COORDINATION_DATA
		//int error;
		float local_cycle_timer;
		//float max_subt; //using max_subtract
		//int iact;
		//int method; //using transition_method
		array<float>^ splits;//8
		array<float>^ min_splits;//8

		//Extra coordination data apart from AC_INPUTS
		float new_cycle_length;
		float new_offset;
		array<float>^ new_splits;//8

		//bool coord_set;
		//Green/Yellow phase state info for current node
		int green_phases;
		int yellow_phases;
	};


	public value struct Wcf_freeway_link
	{
		int id;
		int usn;
		int dsn;
		int usn_type; //Internal = 0, Interface = 1, External = 2
		int dsn_type; //Internal = 0, Interface = 1, External = 2
		int linktype;
		int thrunode;
		int mainline_sending_lane;   //lane on the subject link that sends traffic to the mainline receiving link
		int mainline_receiving_lane; //was thru_alignment_lane //lane on the mainline receiving link that receives traffic from the subject link
		int offramp_sending_lane;    //was offramp_alignment_lane //lane on the subject link that sends traffic to the offramp or diverge link
		int offramp_receiving_lane;  //lane on the offramp or diverge link that receives traffic from the subject link
		//int thru_alignment_lane;//mainline_receiving_lane    //ID number of the lane on the downstream link that receives through traffic from lane 1 of this link
		//int offramp_alignment_lane;//offramp_sending_lane //ID number of the lane on this link that feeds lane 1 of the downstream off-ramp/diverging link
		int exitnode;
		int length;
		int fulllanes;
		array<int>^ adddrop_code; //[3]
		array<int>^ adddrop_lane; //[3]
		array<int>^ adddrop_dist; //[3]
		array<int>^ adddrop_warn; //[3]
		array<int>^ auxlaneid; //[6]->[10]
		array<int>^ auxlanecode; //[6]->[10]
		array<int>^ auxlanelength; //[6]->[10]
		int freeflowspeed;
		int thru_percent;
		int offramp_warn_distance;
		int anticip_warning_distance;
		int anticip_warning_speed;
		int nhov_lanes;
		int hov_begin;
		int hov_end;
		int hov_code;
		array<int>^ hov_lanes;
		int hov_offramp_warn_distance;
		int hov_side;
		int hov_type;
		int hov_warn;
		float hov_pct;
		float cfmult;
		int first_detector;
		float grade;
		float tilt;
		float curve;
		int pavement;
		float shoulder_width;
		array<float>^ lane_width; //11
		array<int>^ barrier;
		int datastation_id;
		int datastation_location;
		int truck_code;
		int truck_dir;
		int truck_lane;
		int etl_warn;
		array<array<int>^>^ exclude_type; //[16][11]
		array<float>^ multiplier_exit;
		float startup_time;
		int merge_diverge_code; //Merge = 1, Diverge = 2, most links are 0
	};


	public value struct Wcf_street_link
	{
		int id;
		int usn;
		int dsn;
		int usn_type; //Internal = 0, Interface = 1, External = 2
		int dsn_type; //Internal = 0, Interface = 1, External = 2
		int thrunode;
		int leftnode;
		int rightnode;
		int diagnode;
		int rdiagnode;
		int opposenode;
		int length;
		int fulllanes;
		int leftturnbays;
		int rightturnbays;
		//int lengthofleftbay;
		//int lengthofrightbay;
		int freeflowspeed;
		array<int>^ channelization;
		array<float>^ laneLength;
		int leftpct;
		int thrupct;
		int rightpct;
		int diagpct;
		int rdiagpct;
		float grade;
		int distribution_code;
		float startup_delay;
		float discharge_hdwy;
		int rtor;
		int ped_code;
		int lane1;
		int lane2;
		int cfmult;
		int sight_dist;
		int first_detector;
		float shoulder_width;
		array<float>^ lane_width;
		int ste_freq;
		int ste_duration;
		int signal_range;
		int centroid;
		int centroid_label;
		array<array<int>^>^ exclude_type;
		array<float>^ multiplier_left;
		array<float>^ multiplier_thru;
		array<float>^ multiplier_right;
		array<float>^ multiplier_diag;
		array<float>^ multiplier_rdiag;
	};


	public value struct WCF_ENTRYNODES_DATA //added 8/12
	{
		float carpool_pct;
		int flowrate;
		int hov_violators_per10000;
		array<int>^ lane_pct;
		int Node_ID;
		float truck_pct;
		int SS_USN;
		int SS_DSN;
	};


	public value struct WCF_NETWORK_INPUTS //added 8/12
	{
		int run_init;
		int initialization_end;
		array<int>^ time_period_duration;
		int time_interval;
		float timestep;
		int type_of_run;  
		int sim_start_time;
		int max_node_number;
	};

	public value struct WCF_FREEWAY_NETWORK_INPUTS
	{
		array<float>^ cfrict;
		float default_hov_pct;
		float lag_accel;
		float lag_decel;
		array<float>^ ffspeed_adj;
		array<float>^ zfoll_pitt;
		array<float>^ zfoll_idm;
		int pitt_sep;
		int idm_sep;
		float freeway_pct_coop;
		float lc_time;
		float dlc_mult;
	};

	public value struct WCF_STREET_NETWORK_INPUTS
	{
		array<float>^ additional_gap;
		array<int>^ amber_decel;
		int lt_speed;
		int rt_speed;
		array<int>^ pdelay_weak;
		array<int>^ pdelay_strong;
		array<int>^ ped_duration;
		array<float>^ acceptable_gap;
		array<float>^ acceptable_ltg;     
		array<float>^ acceptable_rtg;
		array<array<float>^>^ dwell_multiplier;
		array<float>^ ffspeed_adj;
		array<float>^ zfoll_pitt;
		array<float>^ zfoll_idm;
		int pitt_sep;
		int idm_sep;
		float lc_time;
		array<float>^ lt_jumper_prob;
		array<float>^ lt_lagger_prob;
		array<float>^ spillback_prob;
		float stop_spd;
		float street_pct_coop;
		float yield_spd;
		float driver_fampct;
		array<float>^ qfactor;
		array<float>^ ste_mult;
		array<float>^ turnsignal_prob;
		float turnsignal_dist;
	};

	public value struct WCF_RM_DATA
	{
		int dsn;
		int link;
		int control;
		int onset;
		int state;
		array<int>^ detector; //10
		int capacity;
		array<int>^ speed; //6
		array<float>^ headway; //6
		float timer;
		float updint;
		int twopergreen;
	};

	public value struct WCF_BUSROUTE_DATA
	{
		int number;
		int hdwy;
		int offset;
		int nodes;
		array<int>^ route_nodes;
		array<int>^ stationlist;
		int persontrips;
		int timer;
		int traveltime;
		int trips;
	};

	public value struct WCF_BUSSTATION_DATA
	{
		int block_code;
		int usn;
		int dsn;
		int capacity; 
		int type_code;
		int location;
		int dwell;
		float bypass_pct;
		int next_station;
		int pocket_lane;
		int front;
		int count;
		float dwell_time;
		float empty_time;
		float overflow_time;
	};


	public value struct WCF_DETECTOR_INPUTS
	{
		int usn;
		int dsn;
		int signal_node;
		float carryover_time;
		float delay_time;
		int link;
		int lane1;
		int lane2;
		float location;
		int operation_code;
		int type_code;
		int station_id;
		float zone_length;
		int associated_phase;
		int detection_zone;
	};

	public value struct WCF_DETECTOR_OUTPUTS
	{
		int current_count;
	   int current_state;
		int previous_state;
	   int hdwy_count;
	   float hdwy_total;
	   float on_time;
	   float speed_total;
	   int length_total;
	};

	public value struct WCF_EVENT_DATA
	{
		int begin_time;
		int end_time;
		int lane;
		int link;
		int location;
		int speed_reduction;
		int length;
		int code; // 0=blockage, 1=speed reduction
	};

	public value struct WCF_PARKING_DATA
	{
		int link;
		int duration;
		int freq;
		int left_start;
		int left_len;
		int right_start;
		int right_len;
	};

	public value struct WCF_INCIDENT_DATA
	{
		int link;
		int begin_point;
		int begin_time;
		int end_point;
		int end_time;
		int rbnf;
		int warn_point;
		array<int>^ code; //11
	};

	public value struct WCF_DIVERSION_DATA
	{
		int link;
		int begin_time;
		int end_time;
		int location;
		int pathid;
		int percentage;
		int speed;
	};

	public value struct WCF_NODE_LOCATION_DATA
	{
		int x;
		int y;
		float latitude;
		float longitude;
		int elevation;
		int is_used;
		int is_defined; 
	};

	public value struct WCF_VEHICLE_TYPE_DATA
	{
		int length;
		float headway_factor;
		float average_occupancy;
		float emergency_decel;
		int fleet_freeway_auto;
		int fleet_freeway_truck;
		int fleet_freeway_carpool;
		int fleet_freeway_bus;
		int fleet_freeway_ev;
		int fleet_freeway_bike;
		int fleet_street_auto;
		int fleet_street_truck;
		int fleet_street_carpool;
		int fleet_street_bus;
		int fleet_street_ev;
		int fleet_street_bike;
		float pct_pitt;
		float pct_idm;
		float pct_acc;
		float pct_cacc;
	};

	public value struct WCF_COND_TURNPCTS
	{
		int USN;
		int DSN;
		array<int>^ LEFTPCT; //4
		array<int>^ THRUPCT; //4
		array<int>^ RIGHTPCT; //4
		array<int>^ DIAGPCT; //4
	};

	public value struct WCF_VFData
	{
		float acceleration;
		float decel;
		float desiredspeed;
		float disch_timer;
		int drivertype;
		int entry_link;
		float entrytime;
		int ev_dist;
		int ev_ovrspd;
		int ev_range;
		int ev_rand;
		float ev_wait_timer;
		int ev_watch;
		int fleet;
		int go_thru_signal;
		float lag_timer;
		int lane;
		array<int>^ lanecodes; //11
		int link;
		int follower;
		int id;
		int last_detid;
		float lc_timer;
		int leader;
		float location;
		int pathid;
		int pathpoint;
		int saved_path;
		int pseudo_leader;
		float prev_accel;
		int prevlink;
		int prevlane;
		int routeid;
		float speed;
		float speed_adj;
		int turncode;
		int vlength;      
		int vtype;
		int xcode;
		int will_coop_ev;
		int will_coop_lc;
		int will_move;
		int destination;
		float distance_to_segment_end;
		int diverted;
		int hov_violator;
		int imeter;
		int incident_num;
		int isegment;
		int must_merge;
		int next_object;
		float remaining_dist;
		int sorted_list;
		int sort_position;
			int car_following_model; //1=PITT, 2=IDM, 3= ACC and 4=CACC. Use 1 for testing.

		float timestep;

		//TCA Data start
		int DSRC_MessageID;
		int Vehicle_ID;
		long long BSM_tmp_ID;
		float transtime;
		int Link;
		int Lane;
		int Location;
		int Speed;
		float Heading;
		float avg_accel;
		int brakeStatus;
		float brakePressure;
		int hardBraking;
		System::String^ transTo;
		int transmission_received_time;
		int TractionControl;
		int AirTemp;
		int Wipers;
	};

	public value struct WCF_VSData
	{
		float acceleration;		//Current acceleration.
		float decel;			//Deceleration computed for following a vehicle across an interface node.
		float desiredspeed;		//Desired speed.
		float disch_timer;		//Time remaining before vehicle IV can discharge from the ramp meter that it faces.
		int drivertype;			//Driver type of vehicle IV.
		int entry_link;			//ID of the entry link from which vehicle IV entered the network.
		float entrytime;		//Time when vehicle IV entered the network.
		int ev_dist;			//Distance to an upstream Emergency Vehicle.
		int ev_ovrspd;			//Increase in desired speed when vehicle IV is an Emergency Vehicle.
		int ev_range;			//Range of signal preemption transmitter when vehicle IV is an Emergency Vehicle.
		int ev_rand;			//Random number used for vehicle IV when making decisions about cooperating with an Emergency Vehicle.
		float ev_wait_timer;	//Time that vehicle IV must remain on the shoulder when cooperating with an Emergency Vehicle.  
		int ev_watch;			//ID of an Emergency Vehicle that vehicle IV has noticed.
		int fleet;				//Fleet that vehicle IV belongs to.
		int go_thru_signal;		//Flag indicating that vehicle IV may proceed through the ramp meter without stopping.
		float lag_timer;		//Lag timer. Vehicle IV will not respond to a change in acceleration or deceleration until the timer reaches 0.
		int lane;				//Lane in which vehicle IV is located.
		array<int>^ lanecodes;	//Current usability code for lane ILN for vehicle IV, //[7]
								////0=good, 1=must vacate, 2=doesn't exist, 3=anticipatory, 4=incident-good, 5=incident-vacate, 6=HOV-vacate, 7=excluded.
		int link;				//Link on which vehicle IV is located.
		int follower;			//Internal ID of the follower of vehicle IV.
		int id;					//Vehicle ID.
		int last_detid;			//ID of the last detector that vehicle IV passed.
		float lc_timer;			//Lane change timer. Vehicle IV may perform another lane change when the timer reaches 0.
		int leader;				//Internal ID of the leader of vehicle IV.
		float location;			//Current distance from upstream node of the current link to the front bumper.
		int pathid;				//ID of a path that vehicle IV is following.
		int pathpoint;			//Pointer to the step within a path or route that vehicle IV is following.
		int saved_path;			//
		int pseudo_leader;		//Internal ID of a vehicle, other than its leader, that vehicle IV must stay behind.
		float prev_accel;		//Acceleration on previous time step.
		int prevlink;			//Link on which vehicle IV was previously located.
		int prevlane;			//Lane in which vehicle IV was previously located.
		int routeid;			//ID of a bus route that vehicle IV is following.
		float speed;			//Current speed.
		float speed_adj;		//Temporary adjustment to desired speed.
		float start_lag;		//Time that the lag interval began.
		int turncode;			//Current turn code for vehicle IV.
		int vlength;			//Length of vehicle IV.
		int vtype;				//Vehicle type of vehicle IV.
		int xcode;				//Flag indicating that vehicle IV has been processed during the current time step, 0=no, 1=yes.
		int will_coop_ev;		//Flag indicating that vehicle IV will cooperate with an Emergency Vehicle.
		int will_coop_lc;		//Flag indicating that vehicle IV will cooperate with a lane changer.
		int will_move;			//Flag indicating that vehicle IV will move onto the shoulder
		int diverted;			//
		float dwell_timer;		//Bus dwell timer.
		int goal_lane;			//Lane that vehicle IV is trying to reach.
		int has_stopped;		//Flag indicating that vehicle IV has already stopped on the link.
		int ispdicd;			//Control delay speed, used in control delay calculation.
		int next_stop;			//Next station where bus IB will stop.
		int prvdist;			//Previous location, used in control delay calculation.
		int prvlink;			//Previous link, used in control delay calculation.
		int prvlnkicd;			//Previous link, used in control delay calculation.
		int qstate;				//Queue state. 0=>not in queue,
								//1=>stopped in queue,
								//2=>moving in queue,
								//3=>bus blocking entry to station,
								//4=>bus dwelling in station, 
								//5=>stopped in queue flagged for cycle failure
								//6=>moving in queue flagged for cycle failure
		int turn_code;			//Turn code for the movement from the current link.
		int turn_code2;			//Next downstream turn code.
		int turn_link;			//Link from which the vehicle's upcoming turn will be made.
		int turn_link2;			//Next downstream turn link.
		int vehicd;				//Vehicle status, used in control delay calculation,
		//int vehicd0;			//Added 4/21/15
		int will_jump;			//Flag indicating the vehicle IV will jump a left turn when the signal turns green.
		int will_yield;			//Flag indicating the vehicle IV will yield to opposing vehicles at an intersection.
			int car_following_model; // 1=PITT, 2=IDM, 3= ACC and 4=CACC. Use 1 for testing.
			float arc_location;
			int arc_entrylink;
			int arc_entrylane;
		float timestep;

		//TCA Data start
		int DSRC_MessageID;					//Data element used to tell the receiving application how to decode the message type
		int Vehicle_ID;						//ID of the vehicles as stated in the vehicle trajectory file
		long long BSM_tmp_ID;				//Temporary ID that changes every 5 minutes
		float transtime;					//Time that the snapshot was taken and transmitted
		int Link;							//Link that the vehicle is on
		int Lane;							//Lane that the vehicle is in
		float Location;						//The distance of vehicle in the link from the upstream node. (ft)
		float Speed;						//Speed in mph that the vehicle was going when the snapshot was taken (ft/s)
		float Heading;						//The heading of the vehicle (between 0 and 360 degrees)
		float avg_accel;					//Average acceleration that the vehicle was going between the previous snapshot and the current snapshot (ft/s/s)
		int brakeStatus;
		float brakePressure;				//The deceleration value or zero if the vehicle is accelerating (vehicle instantaneous acceleration value required for this output) (ft/s/s)
		int hardBraking;					
		System::String^ transTo;			//The name of the RSE or Cellular region the BSM was transmitted to
		float transmission_received_time;	//Time the BSM was received (sec)
		int TractionControl;
		int AirTemp;
		int Wipers;
	};

	public value struct WCF_TIMING_PLAN
	{
		array<float>^ mingreens; //8
		array<float>^ passage; //8
		array<float>^ maxgreens; //8
		array<float>^ yellowchange; //8
		array<float>^ redclear; //8
	};

	public value struct WCF_SIGNAL_CONTROLLER_DATA
	{
		int holds; //instruction[0]
		int force_offs; //instruction[1]
		int phase_calls;
		int greens;
		int yellows;
		int ring_code1;
		int ring_code2;
	};

	public value struct WCF_RUNNER_DATA
	{
		System::String ^initialization_dat_file_path;
		int n_street_links;
		float simulation_time_step;
		int use_ntcip;
		int use_dcs;
		int n_sdet;
		double curTimeStep;
		int extension;
		int forceoff;
		int dcs_error;
		int n_acs;
		int green_phases;
		int yellow_phases;
	};

	public value struct WCF_COORDINATION_DATA
	{
		int error;
		int node;
		float local_cycle_timer;
		float cycle_length;
		float offset;
		float max_add;
		float max_subt;
		int iact;
		int method;
		array<float>^ splits;//8
		array<float>^ min_splits;//8

		//bool coord_set;
	};
	
	public value struct WCF_CONTROLLER_DATA
	{
		int is_etrunner_ready;
		int phase_calls;
		int green_phases;
		int yellow_phases;
		int is_controller_ready;
		int controller_iact;
		//float current_simtime;
	};

	public value struct WCF_STREET_MOE_DATA
	{
		int usn_id;
		int dsn_id;
		int lane_id;
		int flag;
		System::String^ MOEString;
		float MOEValue;
	};


//todo, add_start
	public value struct WCF_LKINCDATA
	{
		int incidentid;
		int linkid;
		int incidenttype;
		float incidentposition;
		float incidentlength;
		int starttime;
		int duration;
		float reactionpoint;
		float rubberneckfactor;
		int modeltype;
		int mstate;
		int numaffectedlanes;
		array<int>^ affectedlaneidarray;
		array<int>^ laneincidentcodes;
	};

	public value struct WCF_INTERSECTION_DIMENSIONS
	{
		float up_int_width;
		array<float>^ lane_center;
		array<array<float>^>^ lt_arc_length;
		array<array<float>^>^ thru_arc_length;
		array<array<float>^>^ rt_arc_length;
		array<array<float>^>^ ld_arc_length;
		array<array<float>^>^ rd_arc_length;
	};

	public value struct WCF_API_DETECTOR_DATA
	{
		int usn;
		int dsn;
		int associated_phase;
		int station_id;
		float location;
		int lane1;
		int lane2;
		int zone_length;
		float delay_time;
		float carryover_time;
		int next_det;
		int type_code;
		int operation_code;
		int count;
		int current_state;
		float on_time;
		float speed_total;
		int hdwy_count;
		float hdwy_total;
	};

	public value struct WCF_COORDINATION_INPUTS
	{
		int node;
		float cycle_length;
		float offset;
	};

	public value struct WCF_API_CAR_FOLLOWING
	{
		float acc_tg;
		float acc_amax;
		float acc_dmax;
		float acc_k1;
		float acc_k2;
		float cacc_tg;
		float cacc_amax;
		float cacc_dmax;
		float cacc_k1;
		float cacc_k2;
	};
//todo, add_end

	[ServiceContract]
	public interface class IService1
	{
		[OperationContract]
		int GetNumberOfConnectedClients();
		[OperationContract]
		void SetNumberOfConnectedClients(int count);	

		[OperationContract]
		int GetClientState();
		[OperationContract]
		void SetClientState(int s);	

		[OperationContract]
		int GetHostState();
		[OperationContract]
		void SetHostState(int s);	

		//timestep
		[OperationContract]
		float GetServerTimestep();
		[OperationContract]
		void SetServerTimestep(float t);	

		//timestep interval
		[OperationContract]
		float GetAPITimestepInterval();
		[OperationContract]
		void SetAPITimestepInterval(float s);	

		//time period
		[OperationContract]
		int GetServerTimePeriod();
		[OperationContract]
		void SetServerTimePeriod(int tp);	

		[OperationContract]
		void SetServerFVehicleData(array<WCF_VFData>^ wcf_fveh);
		[OperationContract]
		array<WCF_VFData>^ GetServerFVehicleData();
		[OperationContract]
		void SetServerSVehicleData(array<WCF_VSData>^ wcf_sveh);
		[OperationContract]
		array<WCF_VSData>^ GetServerSVehicleData();

		// for website
		[OperationContract]
		void SetServerFVehicleDataSize(int nVehicle);
		[OperationContract]
		int GetServerFVehicleDataSize();

		//path
		[OperationContract]
		void SetServerPathData(array<int>^ nodes_list); 
		[OperationContract]
		array <int> ^ GetServerPathData(); 

		//new vehicle
		[OperationContract]
		void SetServerNewVehicleData(array<NewVehicle> ^ nv); 
		[OperationContract]
		array <NewVehicle> ^ GetServerNewVehicleData(); 

		//signal
		[OperationContract]
		void SetServerSignalData(array<Signal> ^ sig); 
		[OperationContract]
		array <Signal> ^ GetServerSignalData(); 



		[OperationContract]
		void SetServerNetworkInput(array <WCF_NETWORK_INPUTS> ^ wcf_network_inputs);
		[OperationContract]
		array <WCF_NETWORK_INPUTS>^ GetServerNetworkInput();

		[OperationContract]
		void SetServerFreewayNetworkInput(array <WCF_FREEWAY_NETWORK_INPUTS> ^ wcf_freeway_network_inputs);
		[OperationContract]
		array <WCF_FREEWAY_NETWORK_INPUTS>^ GetServerFreewayNetworkInput();

		[OperationContract]
		void SetServerStreetNetworkInput(array <WCF_STREET_NETWORK_INPUTS> ^ wcf_street_network_inputs);
		[OperationContract]
		array <WCF_STREET_NETWORK_INPUTS>^ GetServerStreetNetworkInput();

		[OperationContract]
		void SetServerVehicleTypeInputs(array <WCF_VEHICLE_TYPE_DATA> ^ wcf_Vehicle_Type_Inputs);
		[OperationContract]
		array <WCF_VEHICLE_TYPE_DATA>^ GetServerVehicleTypeInputs();

		//freeway links
		[OperationContract]
		void SetServerFreewayData(array <Wcf_freeway_link> ^ wcf_freeway_links);
		[OperationContract]
		array <Wcf_freeway_link>^ GetServerFreewayData();

		//surface street links
		[OperationContract]
		void SetServerStreetData(array <Wcf_street_link> ^ wcf_street_links);
		[OperationContract]
		array <Wcf_street_link>^ GetServerStreetData();

		//conditional turn percentages
		[OperationContract]
		void SetServerCondTurnpctData(array <WCF_COND_TURNPCTS> ^ wcf_cond_turnpct_data);
		[OperationContract]
		array <WCF_COND_TURNPCTS>^ GetServerCondTurnpctData();

		[OperationContract]
		void SetServerEntryNodeDataSize(int n_wcf_entry_nodes);
		[OperationContract]
		int GetServerEntryNodeDataSize();

		[OperationContract]
		void SetServerEntryNodeData(array <WCF_ENTRYNODES_DATA> ^ wcf_entry_nodes);
		[OperationContract]
		array <WCF_ENTRYNODES_DATA>^ GetServerEntryNodeData();

		//Fix Time control
		[OperationContract]
		void SetServerFTCSignalDataSize(int n_wcf_ftc); 
		[OperationContract]
		int GetServerFTCSignalDataSize();

		[OperationContract]
		void SetServerFTCSignalData(array<WCF_FTC_DATA> ^ wcf_ftc); 
		[OperationContract]
		array <WCF_FTC_DATA>^ GetServerFTCSignalData();

		//Actuated Control
		[OperationContract]
		void SetServerACData(array <WCF_AC>^ wcf_acl);
		[OperationContract]
		array <WCF_AC>^ GetServerACData();

		[OperationContract]
		void SetServerFreewayDetectorInputs(array <WCF_DETECTOR_INPUTS>^ wcf_fdet_inputs);
		[OperationContract]
		array <WCF_DETECTOR_INPUTS>^ GetServerFreewayDetectorInputs();

		[OperationContract]
		void SetServerStreetDetectorInputs(array <WCF_DETECTOR_INPUTS>^ wcf_sdet_inputs);
		[OperationContract]
		array <WCF_DETECTOR_INPUTS>^ GetServerStreetDetectorInputs();

		[OperationContract]
		void SetServerFreewayDetectorOutputs(array <WCF_DETECTOR_OUTPUTS>^ wcf_fdet_inputs);
		[OperationContract]
		array <WCF_DETECTOR_OUTPUTS>^ GetServerFreewayDetectorOutputs();

		[OperationContract]
		void SetServerStreetDetectorOutputs(array <WCF_DETECTOR_OUTPUTS>^ wcf_sdet_inputs);
		[OperationContract]
		array <WCF_DETECTOR_OUTPUTS>^ GetServerStreetDetectorOutputs();

		[OperationContract]
		void SetServerBusRouteInputs(array <WCF_BUSROUTE_DATA>^ wcf_busroute_inputs);
		[OperationContract]
		array <WCF_BUSROUTE_DATA>^ GetServerBusRouteInputs();

		[OperationContract]
		void SetServerBusStationInputs(array <WCF_BUSSTATION_DATA>^ wcf_busstation_inputs);
		[OperationContract]
		array <WCF_BUSSTATION_DATA>^ GetServerBusStationInputs();

		[OperationContract]
		void SetServerIncidentData_Inputs(array<WCF_INCIDENT_DATA>^ wcf_incident_data_inputs);
		[OperationContract]
		array<WCF_INCIDENT_DATA>^ GetServerIncidentData_Inputs();

		[OperationContract]
		void SetServerXYCoordInputs(array <WCF_NODE_LOCATION_DATA>^ wcf_xy_coord_inputs);
		[OperationContract]
		array <WCF_NODE_LOCATION_DATA>^ GetServerXYCoordInputs();

		[OperationContract]
		void SetServerRampmeterInputs(array <WCF_RM_DATA>^ wcf_rampmeter_inputs);
		[OperationContract]
		array <WCF_RM_DATA>^ GetServerRampmeterInputs();

		[OperationContract]
		void SetServerParkingData(array<WCF_PARKING_DATA>^ wcf_parking_inputs);
		[OperationContract]
		array<WCF_PARKING_DATA>^ GetServerParkingData();

		[OperationContract]
		void SetServerEventData(array<WCF_EVENT_DATA>^ wcf_event_inputs);
		[OperationContract]
		array<WCF_EVENT_DATA>^ GetServerEventData();

		[OperationContract]
		void SetServerDiversionData(array<WCF_DIVERSION_DATA>^ wcf_diversion_inputs);
		[OperationContract]
		array<WCF_DIVERSION_DATA>^ GetServerDiversionData();

		[OperationContract]
		void SetServerTRFFile(String ^TRFFile);
		[OperationContract]
		String^ GetServerTRFFile();

		[OperationContract]
		void SetServerGPSRefFile(String ^GPSRefFile);
		[OperationContract]
		String^ GetServerGPSRefFile();

		[OperationContract]
		void SetServerGPSRefNodes(array <WCF_NODE_LOCATION_DATA>^ wcf_gps_ref_nodes);
		[OperationContract]
		array <WCF_NODE_LOCATION_DATA>^ GetServerGPSRefNodes();

		[OperationContract]
		void SetServerWriteTextFlag(int wcf_write_text_flag);
		[OperationContract]
		int GetServerWriteTextFlag();

		[OperationContract]
		void SetServerTimingPlanData(array <WCF_TIMING_PLAN>^ wcf_timing_plan_data);
		[OperationContract]
		array <WCF_TIMING_PLAN>^ GetServerTimingPlanData();
		[OperationContract]
		virtual void SetServerSignalControllerData(array <WCF_SIGNAL_CONTROLLER_DATA>^ wcf_ac_controller_data);
		[OperationContract]
		virtual array <WCF_SIGNAL_CONTROLLER_DATA>^ GetServerSignalControllerData();

		[OperationContract]
		virtual void SetServerRunnerData(array <WCF_RUNNER_DATA>^ wcf_runner_data);
		[OperationContract]
		virtual array <WCF_RUNNER_DATA>^ GetServerRunnerData();

		[OperationContract]
		int GetRunnerState();
		[OperationContract]
		void SetRunnerState(int s);
		[OperationContract]
		int GetTCAState();
		[OperationContract]
		void SetTCAState(int s);

		[OperationContract]
		virtual void SetServerCoordinationData(array <WCF_COORDINATION_DATA>^ wcf_coordination_data);
		[OperationContract]
		virtual array <WCF_COORDINATION_DATA>^ GetServerCoordinationData();

		[OperationContract]
		virtual void SetServerNodeID(int NodeID);
		[OperationContract]
		virtual int GetServerNodeID();
		[OperationContract]
		virtual void SetServerUSNID(int usn_id);
		[OperationContract]
		virtual int GetServerUSNID();
		[OperationContract]
		virtual void SetServerDSNID(int dsn_id);
		[OperationContract]
		virtual int GetServerDSNID();
		[OperationContract]
		virtual void SetServerMaxAdd(float max_add);
		[OperationContract]
		virtual float GetServerMaxAdd();
		[OperationContract]
		virtual void SetServerMaxSub(float max_subt);
		[OperationContract]
		virtual float GetServerMaxSub();

		[OperationContract]
		virtual void SetServerNewCycleLength(int wcf_node_id, float wcf_new_cycle_length);
		[OperationContract]
		virtual float GetServerNewCycleLength();
		[OperationContract]
		virtual void SetServerCycleLength(int wcf_node_id, float wcf_cycle_length);
		[OperationContract]
		virtual float GetServerCycleLength();

		[OperationContract]
		virtual void SetServerNewOffset(int wcf_node_id, float wcf_new_offset);
		[OperationContract]
		virtual float GetServerNewOffset();
		[OperationContract]
		virtual void SetServerOffset(int wcf_node_id, float wcf_offset);
		[OperationContract]
		virtual float GetServerOffset();

		[OperationContract]
		virtual void SetServerLocalCycleTimer(int wcf_node_id, float wcf_local_cycle_timer);
		[OperationContract]
		virtual float GetServerLocalCycleTimer();

		[OperationContract]
		virtual void SetServerNewSplits(int wcf_node_id, array<float>^ wcf_new_splits);
		[OperationContract]
		virtual array<float>^ GetServerNewSplits();
		[OperationContract]
		virtual void SetServerSplits(int wcf_node_id, array<float>^ wcf_splits);
		[OperationContract]
		virtual array<float>^ GetServerSplits();


		[OperationContract]
		virtual void SetServerMinSplits(int wcf_node_id, array<float>^ wcf_min_splits);
		[OperationContract]
		virtual array<float>^ GetServerMinSplits();

		[OperationContract]
		virtual void SetServerNodeGYR(int wcf_node_id, array<int>^ wcf_greens_yellows);
		[OperationContract]
		virtual array<int>^ GetServerNodeGYR();

		[OperationContract]
		virtual void SetServerTransitionMethod(int wcf_node_id, int wcf_transition_method, float wcf_max_add, float wcf_max_sub);
		[OperationContract]
		virtual int GetServerTransitionMethod();

		[OperationContract]
		virtual void SetServerAverageTravelTime(int wcf_usn_id, int wcf_dsn_id, float wcf_average_travel_time);
		[OperationContract]
		virtual float GetServerAverageTravelTime();

		[OperationContract]
		virtual array <WCF_CONTROLLER_DATA>^ GetControllerData();
		[OperationContract]
		virtual void SetControllerData(array <WCF_CONTROLLER_DATA>^ wcf_controller_data);
		[OperationContract]
		virtual void SetControllerDataNoPhaseStates(array <WCF_CONTROLLER_DATA>^ wcf_controller_data);
		[OperationContract]
		virtual void ResizeControllerData(int number_of_controllers);
		[OperationContract]
		virtual int GetControllerDataState();
		[OperationContract]
		virtual void SetControllerDataState(int wcf_controller_data_state);
		[OperationContract]
		virtual void IncreaseControllerDataState();
		[OperationContract]
		virtual void SetControllerDataByIndex(array <WCF_CONTROLLER_DATA>^ wcf_controller_data, int index);
		[OperationContract]
		virtual void SetControllerPhasesByIndex(int green_phases, int yellow_phases, int index);
		[OperationContract]
		virtual void SetControllerReadyByIndex(int is_controller_ready, int index);
		[OperationContract]
		virtual void SetControllerIsEtrunnerReadyByIndex(int is_etrunner_ready, int index);
		[OperationContract]
		virtual int GetControllerIsEtrunnerReadyByIndex(int index);

		[OperationContract]
		virtual void SetServerStreetMOEData(array <WCF_STREET_MOE_DATA>^ wcf_street_moe_data);
		[OperationContract]
		virtual array <WCF_STREET_MOE_DATA>^ GetServerStreetMOEData();
	};

}
