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
#pragma once
#ifndef _DATASTRUCT
#define _DATASTRUCT

#include <iostream>
#include <fstream>
#include <string>
#include <wtypes.h>
#include <deque>
#include <map>
#include <sstream>
#include <malloc.h>
#include <vector>

//#define _UseController 1

#define DELTA_T 5 //AC Phase control

#define DISPLAY_DEFAULT_NETWORK_FLAG 0
#define DISPLAY_FTC_FLAG 0
#define DISPLAY_AC_FLAG 0
#define DISPLAY_VEHICLE_INFO_FLAG 0

#define TRF_NETWORK_INPUT_FLAG 0
#if TRF_NETWORK_INPUT_FLAG
#define HARD_CODED_NETWORK_INPUT_FLAG 0
#else
#define HARD_CODED_NETWORK_INPUT_FLAG 1
#endif


#define MAX_FILE_NAME_LEN 512
#define MAX_FLANE_CODES 11// same as N_FREEWAY_LANES
	#define N_FREEWAY_LANES 20//11->20
#define MAX_SLANE_CODES 20//changed from 7, same as N_STREET_LANES
	#define N_STREET_LANES 20
#define MAX_ADDDROP_LANE 3
#define MAX_AUXILIARY_LANE 6
#define MAX_HOV_LANE 3
#define MAX_FREEWAY_LANE_WIDTH 11
#define MAX_BARRIER 2
#define NTYPES 36 //NTYPES for FREEWAY_LINK struct
#define TO_BE_RENAMED1 11 //TODO
#define MAX_CHANNELIZATION 7
#define MAX_STREET_LANE_WIDTH 7
#define TO_BE_RENAMED2 7 //TODO
#define MAX_TIME_PERIOD_DURATION 19
#define CFRICT 4
#define MAX_FFSPEED_ADJ 10
#define ZFOLL 10
#define ADDITIONAL_GAP 10
#define AMBER_DECEL 10
#define PDELAY_WEAK 10
#define PDELAY_STRONG 10
#define PED_DURATION 3
#define ACCEPTABLE_GAP 10
#define ACCEPTABLE_LTG 10
#define ACCEPTABLE_RTG 10
#define DWELL_MULTIPLIER 10
#define TO_BE_RENAMED3 6 //TODO
#define LT_JUMPER_PROB 7
#define LT_LAGGER_PROB 3
#define SPILLBACK_PROB 4
#define STARTUP_MULT 40
#define GAPTIME_MULT 40
#define STE_MULT 10
#define DETECTOR 10
#define SPEED 6
#define HEADWAY 6
#define MAX_APPROACH 5
#define MAX_DURATION 12
#define CONST_DEMAND_PERIOD_BEGIN 5
#define CONST_DEMAND_PERIOD_END 5
#define DETECTOR1_ID 5
#define DETECTOR2_ID 5
#define DETECTOR3_ID 5
#define MOVEMENT_CODE 5
#define CURRENTPHASE 2
#define DIRECT_APPROACH 5
#define OTHER_APPROACH 5
#define PERMISSIVE_PERIOD_BEGIN 3
#define PERMISSIVE_PERIOD_END 3
#define MAX_PHASE 8
#define ROUTE_NODES 100
#define STATIONLIST 100
#define CODE 11
#define MAX_LEFTPCT 4
#define MAX_THRUPCT 4
#define MAX_RIGHTPCT 4
#define MAX_DIAGPCT 4
#define MAX_LANEC
#define QFACTOR 5
#define MAX_PERIOD 3
#define NUMBER_OF_AC_APPROACHES  20
#define N_AUXLANES 10
#define N_ADDDROPLANES 3
#define N_ENTRYLANES 10
#define N_TIME_PERIODS 19
#define NUMBER_OF_APPROACHES 6

struct TIMING_PLAN
{
	float mingreens[8];
	float passage[8];
	float maxgreens[8];
	float yellowchange[8];
	float redclear[8];
};

struct FILENAME
{
	char str[512];
	int  len;
};

struct AC_INPUTS
{
	int node;							//AC node ID
	int cfails;							// 	SCOPE variable ¨C not currently used
	int adj;							// 	SCOPE variable ¨C not currently used
	int actuated_mode[8];				//actuation mode type
	int leftarrow[10][8];				// 	left movement allowed, approach I, phase J, 0=no, 1=yes
	int thruarrow[10][8];				// 	thru movement allowed, approach I, phase J, 0=no, 1=yes
	int rightarrow[10][8];				// 	right movement allowed, approach I, phase J, 0=no, 1=yes
	int diagarrow[10][8];				// 	diagonal movement allowed, approach I, phase J, 0=no, 1=yes
	float min_green_time[8];			//minimum green time for 8 phases, part of timing plan
	float max_green_time[8];			//maximum green time for 8 phases, part of timing plan
	float default_extension_time[8];	////default extension time for 8 phases, part of timing plan
	float gap_time[8];					//gap time for 8 phases, part of timing plan
	float times_before_reduction[8];	//minimum green time for 8 phases, part of timing plan
	float time_to_reduce[8];			//time to reduce for 8 phases, part of timing plan
	float min_gap_time[8];				//minimum gap time for 8 phases, part of timing plan
	float yellow_change_int[8];			//yellow time for 8 phases, part of timing plan
	float red_clear_int[8];				//red time for 8 phases, part of timing plan
	int ped_allowed[8];					//allow pedestrian 8 phases
	int n_direct_approaches;			//the number of approaches this node has
	int direct_approach_USN[NUMBER_OF_AC_APPROACHES];			//the USN of each approach to this node, this node is the DSN
	int n_indirect_approaches;			//number of indirect approaches
	int indirect_approach_USN[5];		//USN of indirect approaches
	int indirect_approach_DSN[5];		//DSN of indirect approaches
	float cycle_length;					// for coordination
	float offset;						// for coordination
	int ring_phase[8];					//the phase ring setting //Sept7
	int detector_count;					// number of detectors, including scope and dcs, Number of detectors assigned to the signal
	int detector_list[64];				//the list of detectors, List of detectors assigned to the signal
										//SCOPE will use detectors that are assigned to a phase (actuated signal detectors)
										//DCS will use detectors that are not assigned to a phase (surveillance detectors)
										//ETFOMM will use all detectors for MOE calculations
	int transition_method;				//April29	// for coordination
	float max_add;						//April29	// for coordination, depending on transition method
	float max_subtract;					//April29 // for coordination, depending on transition method
	float force_off_times[8];			//April29 // for coordination
	float splits[8];					//July16
	   int ped_omit[8];
	   int ped_intensity[8];
	   int ped_headway[8];
	   int ped_start_time[8];
	   int ped_recall_code[8];
	float walk_time[8];					// for pedestrian
	float walk_clearance_time[8];		// for pedestrian
										//int ring_phase[4][2];	//the phase ring setting

};

struct ANIMATION_DATA
{
   int ID;
   int usn;
   int dsn;
   int signalized;
   int first_vehicle[N_STREET_LANES];
   int first_on_shoulder;
   int signal_code;
   int prev_signal_code;
   int signal_left;
   int signal_thru;
   int signal_right;
   int signal_diag;
   int amber_left;
   int amber_thru;
   int amber_right;
   int amber_diag;
};


struct FREEWAY_LINK
{
	int ID;
	int usn;
	int dsn;
	int usn_type; //Internal = 0, Interface = 1, External = 2
	int dsn_type; //Internal = 0, Interface = 1, External = 2
	int linktype;
	int thrunode;
	int exitnode;
	int length;
	int mainline_sending_lane;   //lane on the subject link that sends traffic to the mainline receiving link
	int mainline_receiving_lane; //was thru_alignment_lane //lane on the mainline receiving link that receives traffic from the subject link
	int offramp_sending_lane;    //was offramp_alignment_lane //lane on the subject link that sends traffic to the offramp or diverge link
	int offramp_receiving_lane;  //lane on the offramp or diverge link that receives traffic from the subject link
	//int thru_alignment_lane;//mainline_receiving_lane    //ID number of the lane on the downstream link that receives through traffic from lane 1 of this link
	//int offramp_alignment_lane;//offramp_sending_lane //ID number of the lane on this link that feeds lane 1 of the downstream off-ramp/diverging link
	
	int fulllanes;
	int adddrop_code[N_ADDDROPLANES];                  
	int adddrop_lane[N_ADDDROPLANES];                  
	int adddrop_dist[N_ADDDROPLANES];                  
	int adddrop_warn[N_ADDDROPLANES];                  
	int auxlaneid[N_AUXLANES];
	int auxlanecode[N_AUXLANES];
	int auxlanelength[N_AUXLANES];
	int freeflowspeed;
	int thru_percent;
	int offramp_warn_distance;
	int anticip_warning_distance;
	int anticip_warning_speed;
	int nhov_lanes;
	int hov_begin;
	int hov_end;
	int hov_code;
	//int hov_lanes[3];
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
	float lane_width[N_FREEWAY_LANES];
	int barrier[2];
	int datastation_id;
	int datastation_location;
	int truck_code;
	int truck_dir;
	int truck_lane;
	int etl_warn;
	int exclude_type[NTYPES][N_FREEWAY_LANES];
	float multiplier_exit[NTYPES];
	float startup_time;
		int merge_diverge_code; //Merge = 1, Diverge = 2, most links are 0
};

struct STREET_LINK
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
   int channelization[N_STREET_LANES];
	float laneLength[N_STREET_LANES];
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
   float cfmult;
   int sight_dist;
   int first_detector;
   float shoulder_width;
   float lane_width[N_STREET_LANES];
   int ste_freq;
   int ste_duration;
   int signal_range;
   int centroid;
   int centroid_label;
   int exclude_type[NTYPES][N_STREET_LANES];
   float multiplier_left[NTYPES];
   float multiplier_thru[NTYPES];
   float multiplier_right[NTYPES];
   float multiplier_diag[NTYPES];
	float multiplier_rdiag[NTYPES];
   int lt_speed;
   int rt_speed;
   float lt_limited_speed_dist;
   float rt_limited_speed_dist;
   int crosswalk_width;
};


struct ENTRYNODES_DATA
{
	int Node_ID;
	int flowrate;
	float carpool_pct;
	float truck_pct;
	int hov_violators_per10000;
	int lane_pct[N_ENTRYLANES];
};

struct SOURCE_SINK_DATA
{
	int ss_usn;
	int ss_dsn;
	int centroid_label;
	float carpool_pct;
	float truck_pct;
	int flowrate;
	int hov_violators_per10000;
};
struct NETWORK_INPUTS
{
	int run_init;
	int initialization_end;
	int time_period_duration[N_TIME_PERIODS];
	int time_interval;
	float timestep;
	int type_of_run;
	int sim_start_time;   
	int max_node_number;
	float dlc_mult;
	float dlc_threshold;
};

struct FREEWAY_NETWORK_INPUTS
{
	float cfrict[4];
	float default_hov_pct;
	float lag_accel;
	float lag_decel;
	float ffspeed_adj[10];
	float zfoll_pitt[10];
	float zfoll_idm[10];
	int pitt_sep;
	int idm_sep;
	float freeway_pct_coop;
	float lc_time;
};


struct STREET_NETWORK_INPUTS
{
       float additional_gap[10];
       int amber_decel[10];
       int pdelay_weak[10];
       int pdelay_strong[10];
       int ped_duration[3];
       float acceptable_gap[10];
       float acceptable_ltg[10];
       float acceptable_rtg[10];
       float dwell_multiplier[6][10];
       float ffspeed_adj[10];
       float zfoll_pitt[10];
	   float zfoll_idm[10];
	   int pitt_sep;
	   int idm_sep;
       float lc_time;
       float lt_jumper_prob[N_STREET_LANES];
       float lt_lagger_prob[3];
       float spillback_prob[4];
       float stop_spd;
       float street_pct_coop;
       float yield_spd;
       float driver_fampct;
       float qfactor[5];
       float ste_mult[10];
       float turnsignal_prob[10];
       float turnsignal_dist;
};


struct RM_DATA
{
	int dsn;
	int link;
	int control;
	int onset;
	int state;
	int detector[10];
	int capacity;
	int speed[6];
	float headway[6];
	float timer;
	int updint;
	bool twopergreen;
};

struct FTC_DATA
{
	int active_intervals;
	int approach[NUMBER_OF_APPROACHES];
	int approaches;
	int current_interval;
	float cycle_length;
	float duration[12];
	int external_control;
	int node;
	int offset;
	int range;
	int signal_code[12][NUMBER_OF_APPROACHES];
	float time_in_interval;
};


struct BUSROUTE_DATA
{
	int number;
        int number_of_tramcars;
	int hdwy;
	int offset;
	int nodes;
	int route_nodes[200];
	int stationlist[100]; 
	int persontrips;
	int timer;
	int traveltime;
	int trips;
}; 

struct BUSSTATION_DATA
{
	int station_number;
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

struct DETECTOR_INPUTS
{
	int usn;				//USN of the link where the detector is located
	int dsn;				//DSN of the link where the detector is located
	int signal_node;		//node that an indirect detector is associated with, not needed for detectors on direct approaches
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
	int next_det;
};

struct DETECTOR_OUTPUTS
{
   int current_count;
   int current_state;
	int previous_state;
   int hdwy_count;
   float hdwy_total;
   float on_time;
   float speed_total;
   int length_total;
   float last_actuation_time;  //July27
   float last_speed;           //July27
   int last_length;            //July27
   int current_state_temp[10]; //Sept22
   int previous_state_temp[10];//Sept22
};

struct EVENT_DATA
{
	int usn;
	int dsn;
	int lane;
	int begin_time;
	int end_time;
	int location;
	int speed_reduction;
	int length;
	int code; // 0=blockage, 1=speed reduction
	int group_id;
	int approach_usn;
	int approach_dsn;
};

struct PARKING_DATA
{
	int usn;
	int dsn;
	int duration;
	int freq;
	int left_start;
	int left_len;
	int right_start;
	int right_len;
};

struct INCIDENT_DATA
{
	int usn;
	int dsn;
	int begin_point;
	int begin_time;
	int end_point;
	int end_time;
	int rbnf;
	int warn_point;
	int code[N_FREEWAY_LANES];
};

struct NODE_LOCATION_DATA
{
	int x;
	int y;
	float latitude;
	float longitude;
	int elevation;
	int is_used;
	int is_defined;
};

struct VEHICLE_TYPE_DATA
{
	int length;
	float headway_factor;
	float average_occupancy;
	float non_emergency_decel;
	int fleet_freeway_auto;
	int fleet_freeway_truck;
	int fleet_freeway_carpool;
	int fleet_freeway_bus;
	int fleet_freeway_ev;
	//int fleet_freeway_bike; // removed 05/25/2018
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

struct COND_TURNPCTS
{
	int USN;
	int DSN;
	int LEFTPCT[4];
	int THRUPCT[4];
	int RIGHTPCT[4];
	int DIAGPCT[4];
};

struct VFData
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
	int lanecodes[N_FREEWAY_LANES];
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
};

//All of these variables are accessible through the API using GET_SVEHICLE_STRUCT() and SET_SVEHICLE_STRUCT().
struct VSData
{
	float acceleration;	//Current acceleration.
	float decel;		//Deceleration computed for following a vehicle across an interface node.
	float desiredspeed;	//Desired speed.
	float disch_timer;	//Time remaining before vehicle IV can discharge from the ramp meter that it faces.
	int drivertype;		//Driver type of vehicle IV.
	int entry_link;		//ID of the entry link from which vehicle IV entered the network.
	float entrytime;	//Time when vehicle IV entered the network.
	int ev_dist;		//Distance to an upstream Emergency Vehicle.
	int ev_ovrspd;		//Increase in desired speed when vehicle IV is an Emergency Vehicle.
	int ev_range;		//Range of signal preemption transmitter when vehicle IV is an Emergency Vehicle.
	int ev_rand;		//Random number used for vehicle IV when making decisions about cooperating with an Emergency Vehicle.
	float ev_wait_timer;//Time that vehicle IV must remain on the shoulder when cooperating with an Emergency Vehicle.  
	int ev_watch;		//ID of an Emergency Vehicle that vehicle IV has noticed.
	int fleet;			//Fleet that vehicle IV belongs to.
	int go_thru_signal;	//Flag indicating that vehicle IV may proceed through the ramp meter without stopping.
	float lag_timer;	//Lag timer. Vehicle IV will not respond to a change in acceleration or deceleration until the timer reaches 0.
	int lane;			//Lane in which vehicle IV is located.
	int lanecodes[N_STREET_LANES];	//Current usability code for lane ILN for vehicle IV,
						//0=good, 1=must vacate, 2=doesn't exist, 3=anticipatory, 4=incident-good, 5=incident-vacate, 6=HOV-vacate, 7=excluded.
	int link;			//Link on which vehicle IV is located.
	int follower;		//Internal ID of the follower of vehicle IV.
	int id;				//Vehicle ID.
	int last_detid;		//ID of the last detector that vehicle IV passed.
	float lc_timer;		//Lane change timer. Vehicle IV may perform another lane change when the timer reaches 0.
	int leader;			//Internal ID of the leader of vehicle IV.
	float location;		//Current distance from upstream node of the current link to the front bumper.
	int pathid;			//ID of a path that vehicle IV is following.
	int pathpoint;		//Pointer to the step within a path or route that vehicle IV is following.
	int saved_path;		//
	int pseudo_leader;	//Internal ID of a vehicle, other than its leader, that vehicle IV must stay behind.
	float prev_accel;	//Acceleration on previous time step.
	int prevlink;		//Link on which vehicle IV was previously located.
	int prevlane;		//Lane in which vehicle IV was previously located.
	int routeid;		//ID of a bus route that vehicle IV is following.
	float speed;		//Current speed.
	float speed_adj;	//Temporary adjustment to desired speed.
	float start_lag;	//Time that the lag interval began.
	int turncode;		//Current turn code for vehicle IV.
	int vlength;		//Length of vehicle IV.
	int vtype;			//Vehicle type of vehicle IV.
	int xcode;			//Flag indicating that vehicle IV has been processed during the current time step, 0=no, 1=yes.
	int will_coop_ev;	//Flag indicating that vehicle IV will cooperate with an Emergency Vehicle.
	int will_coop_lc;	//Flag indicating that vehicle IV will cooperate with a lane changer.
	int will_move;		//Flag indicating that vehicle IV will move onto the shoulder
	int diverted;		//
	float dwell_timer;	//Bus dwell timer.
	int goal_lane;		//Lane that vehicle IV is trying to reach.
	int has_stopped;	//Flag indicating that vehicle IV has already stopped on the link.
	int ispdicd;		//Control delay speed, used in control delay calculation.
	int next_stop;		//Next station where bus IB will stop.
	int prvdist;		//Previous location, used in control delay calculation.
	int prvlink;		//Previous link, used in control delay calculation.
	int prvlnkicd;		//Previous link, used in control delay calculation.
	int qstate;			//Queue state. 0=>not in queue,
									 //1=>stopped in queue,
									 //2=>moving in queue,
									 //3=>bus blocking entry to station,
									 //4=>bus dwelling in station, 
									 //5=>stopped in queue flagged for cycle failure
									 //6=>moving in queue flagged for cycle failure
	int turn_code;		//Turn code for the movement from the current link.
	int turn_code2;		//Next downstream turn code.
	int turn_link;		//Link from which the vehicle's upcoming turn will be made.
	int turn_link2;		//Next downstream turn link.
	int vehicd;			//Vehicle status, used in control delay calculation,
	int vehicd0;		//Added 4/21/15
	int will_jump;		//Flag indicating the vehicle IV will jump a left turn when the signal turns green.
	int will_yield;		//Flag indicating the vehicle IV will yield to opposing vehicles at an intersection.
		int car_following_model; // 1=PITT, 2=IDM, 3= ACC and 4=CACC. Use 1 for testing.
		float arc_location;
		int arc_entrylink;
		int arc_entrylane;
};

struct DIVERSION_DATA
{
	int link;
	int begin_time;
	int end_time;
	int location;
	int pathid;
	int percentage;
	int speed;
};

struct LKINCDATA
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
	int affectedlaneidarray[N_FREEWAY_LANES];
	int laneincidentcodes[N_FREEWAY_LANES];
};

struct INTERSECTION_DIMENSIONS
{
	float up_int_width;
	float lane_center[N_STREET_LANES];
	float lt_arc_length[N_STREET_LANES][N_STREET_LANES];
	float thru_arc_length[N_STREET_LANES][N_STREET_LANES];
	float rt_arc_length[N_STREET_LANES][N_STREET_LANES];
	float ld_arc_length[N_STREET_LANES][N_STREET_LANES];
	float rd_arc_length[N_STREET_LANES][N_STREET_LANES];
};

struct API_DETECTOR_DATA
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

struct COORDINATION_INPUTS
{
	int node;
	float cycle_length;
	float offset;
};

struct API_CAR_FOLLOWING
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

struct RABT_API_DATA
{
	int approaches;
	int approach_USN[5];
	int approach_DSN[5];
	int departing_USN[5];
	int departing_DSN[5];
	float exit_pcts[5][5];
	int radius;
};

struct TURNING_WAY
{
	int usn;
	int dsn;
	int usn2;
	int dsn2;
	float rtw_exit_point; // distance from usn at the exit link
	float rtw_entry_point; // distance from usn at the entry link
	float rtw_length;
	float rtw_ffspeed; // free flow speed
	int rtw_control_code; // stop sign (5) or yield sign (0)
	int rtw_lanes;
};

struct SUPER_CONTROLLER_IDS
{
	std::vector<int> super_controler_ids;
};
#endif
