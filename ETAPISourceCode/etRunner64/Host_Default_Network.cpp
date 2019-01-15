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
#include "stdafx.h"
#include "Host_Default_Network.h"

NETWORK_INPUTS Host_Default_Network_define_Network_INPUTS(void)
{
	NETWORK_INPUTS Network_Inputs;
	Network_Inputs.run_init = true;
	Network_Inputs.initialization_end = 180;
	Network_Inputs.timestep = 1.0;
	Network_Inputs.time_interval = 60.0;
	for(int i = 0; i<19; i++)
	{
		Network_Inputs.time_period_duration[i] = 0;
	}
	Network_Inputs.time_period_duration[0] = 120;
	Network_Inputs.time_period_duration[1] = 120;
	Network_Inputs.time_period_duration[2] = 120;
	return Network_Inputs;
}
FREEWAY_NETWORK_INPUTS Host_Default_Network_define_FREEWAY_NETWORK_INPUTS(void)
{
	FREEWAY_NETWORK_INPUTS Freeway_Network_Inputs;
	Freeway_Network_Inputs.cfrict[0] = 0.16;
	Freeway_Network_Inputs.cfrict[1] = 0.16;
	Freeway_Network_Inputs.cfrict[2] = 0.16;
	Freeway_Network_Inputs.cfrict[3] = 0.16;
	Freeway_Network_Inputs.default_hov_pct = 1.0;
	Freeway_Network_Inputs.lag_accel = 0.3;
	Freeway_Network_Inputs.lag_decel = 0.3;
	Freeway_Network_Inputs.ffspeed_adj[0] = .88;
	Freeway_Network_Inputs.ffspeed_adj[1] = .91;
	Freeway_Network_Inputs.ffspeed_adj[2] = .94;
	Freeway_Network_Inputs.ffspeed_adj[3] = .97;
	Freeway_Network_Inputs.ffspeed_adj[4] = .99;
	Freeway_Network_Inputs.ffspeed_adj[5] = 1.01;
	Freeway_Network_Inputs.ffspeed_adj[6] = 1.03;
	Freeway_Network_Inputs.ffspeed_adj[7] = 1.06;
	Freeway_Network_Inputs.ffspeed_adj[8] = 1.09;
	Freeway_Network_Inputs.ffspeed_adj[9] = 1.12;
	//Freeway_Network_Inputs.zfoll[0] = 1.25;
	//Freeway_Network_Inputs.zfoll[1] = 1.15;
	//Freeway_Network_Inputs.zfoll[2] = 1.05;
	//Freeway_Network_Inputs.zfoll[3] = 0.95;
	//Freeway_Network_Inputs.zfoll[4] = 0.85;
	//Freeway_Network_Inputs.zfoll[5] = 0.75;
	//Freeway_Network_Inputs.zfoll[6] = 0.65;
	//Freeway_Network_Inputs.zfoll[7] = 0.55;
	//Freeway_Network_Inputs.zfoll[8] = 0.45;
	//Freeway_Network_Inputs.zfoll[9] = 0.35;
	Freeway_Network_Inputs.freeway_pct_coop = 0.2;
	Freeway_Network_Inputs.lc_time = 2.0;
	return Freeway_Network_Inputs;
}
STREET_NETWORK_INPUTS Host_Default_Network_define_STREET_NETWORK_INPUTS(void)
{
	STREET_NETWORK_INPUTS Street_Network_Inputs;
	Street_Network_Inputs.additional_gap[0] = 1.2;
	Street_Network_Inputs.additional_gap[1] = 2.1;
	Street_Network_Inputs.additional_gap[2] = 2.6;
	Street_Network_Inputs.additional_gap[3] = 3.1;
	Street_Network_Inputs.additional_gap[4] = 3.5;
	Street_Network_Inputs.additional_gap[5] = 3.9;
	Street_Network_Inputs.additional_gap[6] = 4.2;
	Street_Network_Inputs.additional_gap[7] = 4.6;
	Street_Network_Inputs.additional_gap[8] = 4.9;
	Street_Network_Inputs.additional_gap[9] = 5.1;
	Street_Network_Inputs.amber_decel[0] = -21;
	Street_Network_Inputs.amber_decel[1] = -18;
	Street_Network_Inputs.amber_decel[2] = -15;
	Street_Network_Inputs.amber_decel[3] = -12;
	Street_Network_Inputs.amber_decel[4] = -9;
	Street_Network_Inputs.amber_decel[5] = -7;
	Street_Network_Inputs.amber_decel[6] = -6;
	Street_Network_Inputs.amber_decel[7] = -5;
	Street_Network_Inputs.amber_decel[8] = -4;
	Street_Network_Inputs.amber_decel[9] = -4;
	Street_Network_Inputs.pdelay_weak[0] = 0;
	Street_Network_Inputs.pdelay_weak[1] = 0;
	Street_Network_Inputs.pdelay_weak[2] = 0;
	Street_Network_Inputs.pdelay_weak[3] = 0;
	Street_Network_Inputs.pdelay_weak[4] = 0;
	Street_Network_Inputs.pdelay_weak[5] = 0;
	Street_Network_Inputs.pdelay_weak[6] = 0;
	Street_Network_Inputs.pdelay_weak[7] = 1;
	Street_Network_Inputs.pdelay_weak[8] = 2;
	Street_Network_Inputs.pdelay_weak[9] = 6;
	Street_Network_Inputs.pdelay_strong[0]= 0;
	Street_Network_Inputs.pdelay_strong[1]= 0;
	Street_Network_Inputs.pdelay_strong[2]= 0;
	Street_Network_Inputs.pdelay_strong[3]= 1;
	Street_Network_Inputs.pdelay_strong[4]= 2;
	Street_Network_Inputs.pdelay_strong[5]= 3;
	Street_Network_Inputs.pdelay_strong[6]= 4;
	Street_Network_Inputs.pdelay_strong[7]= 5;
	Street_Network_Inputs.pdelay_strong[8]= 8;
	Street_Network_Inputs.pdelay_strong[9]= 15;
	Street_Network_Inputs.ped_duration[0] = 0;
	Street_Network_Inputs.ped_duration[1] = 10;
	Street_Network_Inputs.ped_duration[2] = 25;
	Street_Network_Inputs.acceptable_gap[0] = 5.6;
	Street_Network_Inputs.acceptable_gap[1] = 5.0;
	Street_Network_Inputs.acceptable_gap[2] = 4.6;
	Street_Network_Inputs.acceptable_gap[3] = 4.2;
	Street_Network_Inputs.acceptable_gap[4] = 3.9;
	Street_Network_Inputs.acceptable_gap[5] = 3.7;
	Street_Network_Inputs.acceptable_gap[6] = 3.4;
	Street_Network_Inputs.acceptable_gap[7] = 3.0;
	Street_Network_Inputs.acceptable_gap[8] = 2.6;
	Street_Network_Inputs.acceptable_gap[9] = 2.0;
	Street_Network_Inputs.acceptable_ltg[0] = 7.8;  
	Street_Network_Inputs.acceptable_ltg[1] = 6.6;
	Street_Network_Inputs.acceptable_ltg[2] = 6.0;
	Street_Network_Inputs.acceptable_ltg[3] = 5.4;
	Street_Network_Inputs.acceptable_ltg[4] = 4.8;
	Street_Network_Inputs.acceptable_ltg[5] = 4.5;
	Street_Network_Inputs.acceptable_ltg[6] = 4.2;
	Street_Network_Inputs.acceptable_ltg[7] = 3.9;
	Street_Network_Inputs.acceptable_ltg[8] = 3.6;
	Street_Network_Inputs.acceptable_ltg[9] = 2.7;    
	Street_Network_Inputs.acceptable_rtg[0] = 10.0;
	Street_Network_Inputs.acceptable_rtg[1] = 8.8;
	Street_Network_Inputs.acceptable_rtg[2] = 8.0;
	Street_Network_Inputs.acceptable_rtg[3] = 7.2;
	Street_Network_Inputs.acceptable_rtg[4] = 6.4;
	Street_Network_Inputs.acceptable_rtg[5] = 6.0;
	Street_Network_Inputs.acceptable_rtg[6] = 5.6;
	Street_Network_Inputs.acceptable_rtg[7] = 5.2;
	Street_Network_Inputs.acceptable_rtg[8] = 4.8;
	Street_Network_Inputs.acceptable_rtg[9] = 3.6;
	Street_Network_Inputs.ffspeed_adj[0] = 0.75;
	Street_Network_Inputs.ffspeed_adj[1] = 0.81;
	Street_Network_Inputs.ffspeed_adj[2] = 0.91;
	Street_Network_Inputs.ffspeed_adj[3] = 0.94;
	Street_Network_Inputs.ffspeed_adj[4] = 0.97;
	Street_Network_Inputs.ffspeed_adj[5] = 1.00;
	Street_Network_Inputs.ffspeed_adj[6] = 1.07;
	Street_Network_Inputs.ffspeed_adj[7] = 1.11;
	Street_Network_Inputs.ffspeed_adj[8] = 1.17;
	Street_Network_Inputs.ffspeed_adj[9] = 1.27;
	//Street_Network_Inputs.zfoll[0] = 1.25;
	//Street_Network_Inputs.zfoll[1] = 1.15;
	//Street_Network_Inputs.zfoll[2] = 1.05;
	//Street_Network_Inputs.zfoll[3] = 0.95;
	//Street_Network_Inputs.zfoll[4] = 0.85;
	//Street_Network_Inputs.zfoll[5] = 0.75;
	//Street_Network_Inputs.zfoll[6] = 0.65;
	//Street_Network_Inputs.zfoll[7] = 0.55;
	//Street_Network_Inputs.zfoll[8] = 0.45;
	//Street_Network_Inputs.zfoll[9] = 0.35;
	Street_Network_Inputs.lc_time = 3.0;
	Street_Network_Inputs.lt_jumper_prob[0] = 0.38;
	Street_Network_Inputs.lt_jumper_prob[1] = 0.38;
	Street_Network_Inputs.lt_jumper_prob[2] = 0.38;
	Street_Network_Inputs.lt_jumper_prob[3] = 0.38;
	Street_Network_Inputs.lt_jumper_prob[4] = 0.38;
	Street_Network_Inputs.lt_jumper_prob[5] = 0.38;
	Street_Network_Inputs.lt_jumper_prob[6] = 0.38;
	Street_Network_Inputs.lt_lagger_prob[0] = 0.5;
	Street_Network_Inputs.lt_lagger_prob[1] = 0.15;
	Street_Network_Inputs.lt_lagger_prob[2] = 0.0;
	Street_Network_Inputs.spillback_prob[0] = 0.8;
	Street_Network_Inputs.spillback_prob[1] = 0.4;
	Street_Network_Inputs.spillback_prob[2] = 0.0;
	Street_Network_Inputs.spillback_prob[3] = 0.0;
	Street_Network_Inputs.stop_spd = 3.0;
	Street_Network_Inputs.street_pct_coop = 0.5;
	Street_Network_Inputs.yield_spd = 10.0;
	Street_Network_Inputs.driver_fampct = 0.1;
	Street_Network_Inputs.ste_mult[0] = 0.1;
	Street_Network_Inputs.ste_mult[1] = 0.2;
	Street_Network_Inputs.ste_mult[2] = 0.3;
	Street_Network_Inputs.ste_mult[3] = 0.4;
	Street_Network_Inputs.ste_mult[4] = 0.5;
	Street_Network_Inputs.ste_mult[5] = 0.7;
	Street_Network_Inputs.ste_mult[6] =  1.0;
	Street_Network_Inputs.ste_mult[7] =  1.3;
	Street_Network_Inputs.ste_mult[8] =  1.8;
	Street_Network_Inputs.ste_mult[9] =  3.7;

	Street_Network_Inputs.dwell_multiplier[0][0] = 0.40;
	Street_Network_Inputs.dwell_multiplier[1][0] = 0.60;
	Street_Network_Inputs.dwell_multiplier[2][0] = 0.70;
	Street_Network_Inputs.dwell_multiplier[3][0] = 0.80;
	Street_Network_Inputs.dwell_multiplier[4][0] = 0.90;
	Street_Network_Inputs.dwell_multiplier[5][0] = 1.00;
	Street_Network_Inputs.dwell_multiplier[6][0] = 1.20;
	Street_Network_Inputs.dwell_multiplier[7][0] = 1.30;
	Street_Network_Inputs.dwell_multiplier[8][0] = 1.40;
	Street_Network_Inputs.dwell_multiplier[9][0] = 1.70;

	Street_Network_Inputs.dwell_multiplier[0][1] = 0.24;
	Street_Network_Inputs.dwell_multiplier[1][1] = 0.48;
	Street_Network_Inputs.dwell_multiplier[2][1] = 0.59;
	Street_Network_Inputs.dwell_multiplier[3][1] = 0.75;
	Street_Network_Inputs.dwell_multiplier[4][1] = 0.85;
	Street_Network_Inputs.dwell_multiplier[5][1] = 0.94;
	Street_Network_Inputs.dwell_multiplier[6][1] = 1.11;
	Street_Network_Inputs.dwell_multiplier[7][1] = 1.26;
	Street_Network_Inputs.dwell_multiplier[8][1] = 1.55;
	Street_Network_Inputs.dwell_multiplier[9][1] = 2.23;

	Street_Network_Inputs.dwell_multiplier[0][2] = 0.30;
	Street_Network_Inputs.dwell_multiplier[1][2] = 0.47;
	Street_Network_Inputs.dwell_multiplier[2][2] = 0.65;
	Street_Network_Inputs.dwell_multiplier[3][2] = 0.77;
	Street_Network_Inputs.dwell_multiplier[4][2] = 0.90;
	Street_Network_Inputs.dwell_multiplier[5][2] = 1.03;
	Street_Network_Inputs.dwell_multiplier[6][2] = 1.16;
	Street_Network_Inputs.dwell_multiplier[7][2] = 1.37;
	Street_Network_Inputs.dwell_multiplier[8][2] = 1.57;
	Street_Network_Inputs.dwell_multiplier[9][2] = 1.78;

	Street_Network_Inputs.dwell_multiplier[0][3] = 0;
	Street_Network_Inputs.dwell_multiplier[1][3] = 0.29;
	Street_Network_Inputs.dwell_multiplier[2][3] = 0.59;
	Street_Network_Inputs.dwell_multiplier[3][3] = 0.75;
	Street_Network_Inputs.dwell_multiplier[4][3] = 0.92;
	Street_Network_Inputs.dwell_multiplier[5][3] = 1.08;
	Street_Network_Inputs.dwell_multiplier[6][3] = 1.25;
	Street_Network_Inputs.dwell_multiplier[7][3] = 1.48;
	Street_Network_Inputs.dwell_multiplier[8][3] = 1.70;
	Street_Network_Inputs.dwell_multiplier[9][3] = 1.94;

	Street_Network_Inputs.dwell_multiplier[0][4] = 0;
	Street_Network_Inputs.dwell_multiplier[1][4] = 0.18;
	Street_Network_Inputs.dwell_multiplier[2][4] = 0.36;
	Street_Network_Inputs.dwell_multiplier[3][4] = 0.70;
	Street_Network_Inputs.dwell_multiplier[4][4] = 1.04;
	Street_Network_Inputs.dwell_multiplier[5][4] = 1.25;
	Street_Network_Inputs.dwell_multiplier[6][4] = 1.44;
	Street_Network_Inputs.dwell_multiplier[7][4] = 1.56;
	Street_Network_Inputs.dwell_multiplier[8][4] = 1.67;
	Street_Network_Inputs.dwell_multiplier[9][4] = 1.80;

	Street_Network_Inputs.dwell_multiplier[0][5] = 0;
	Street_Network_Inputs.dwell_multiplier[1][5] = 0;
	Street_Network_Inputs.dwell_multiplier[2][5] = 0;
	Street_Network_Inputs.dwell_multiplier[3][5] = 0.48;
	Street_Network_Inputs.dwell_multiplier[4][5] = 0.96;
	Street_Network_Inputs.dwell_multiplier[5][5] = 1.20;
	Street_Network_Inputs.dwell_multiplier[6][5] = 1.44;
	Street_Network_Inputs.dwell_multiplier[7][5] = 1.71;
	Street_Network_Inputs.dwell_multiplier[8][5] = 1.98;
	Street_Network_Inputs.dwell_multiplier[9][5] = 2.23;
	return Street_Network_Inputs;
}
void Host_Default_Network_define_Vehicle_Type_Inputs(VEHICLE_TYPE_DATA* Vehicle_Type_Inputs)
{
	int i = 0;
	Vehicle_Type_Inputs[i].length = 14;
	Vehicle_Type_Inputs[i].headway_factor = 1.0;
	Vehicle_Type_Inputs[i].average_occupancy = 1.3;
	Vehicle_Type_Inputs[i].non_emergency_decel = -15.0;
	Vehicle_Type_Inputs[i].fleet_freeway_auto = 25;
	Vehicle_Type_Inputs[i].fleet_street_auto = 25;

	i++;
	Vehicle_Type_Inputs[i].length = 16;
	Vehicle_Type_Inputs[i].headway_factor = 1.0;
	Vehicle_Type_Inputs[i].average_occupancy = 1.3;
	Vehicle_Type_Inputs[i].non_emergency_decel = -15.0;
	Vehicle_Type_Inputs[i].fleet_freeway_auto = 75;
	Vehicle_Type_Inputs[i].fleet_street_auto = 75;

	i++;
	Vehicle_Type_Inputs[i].length = 35;
	Vehicle_Type_Inputs[i].headway_factor = 1.2;
	Vehicle_Type_Inputs[i].average_occupancy = 1.2;
	Vehicle_Type_Inputs[i].non_emergency_decel = -15.0;
	Vehicle_Type_Inputs[i].fleet_freeway_truck = 31;
	Vehicle_Type_Inputs[i].fleet_street_truck = 31;

	i++;
	Vehicle_Type_Inputs[i].length = 53;
	Vehicle_Type_Inputs[i].headway_factor = 1.2;
	Vehicle_Type_Inputs[i].average_occupancy = 1.2;
	Vehicle_Type_Inputs[i].non_emergency_decel = -15.0;
	Vehicle_Type_Inputs[i].fleet_freeway_truck = 36;
	Vehicle_Type_Inputs[i].fleet_street_truck = 36;

	i++;
	Vehicle_Type_Inputs[i].length = 53;
	Vehicle_Type_Inputs[i].headway_factor = 1.2;
	Vehicle_Type_Inputs[i].average_occupancy = 1.2;
	Vehicle_Type_Inputs[i].non_emergency_decel = -15.0;
	Vehicle_Type_Inputs[i].fleet_freeway_truck = 24;
	Vehicle_Type_Inputs[i].fleet_street_truck = 24;

	i++;
	Vehicle_Type_Inputs[i].length = 64;
	Vehicle_Type_Inputs[i].headway_factor = 1.2;
	Vehicle_Type_Inputs[i].average_occupancy = 1.2;
	Vehicle_Type_Inputs[i].non_emergency_decel = -15.0;
	Vehicle_Type_Inputs[i].fleet_freeway_truck = 9;
	Vehicle_Type_Inputs[i].fleet_street_truck = 9;

	i++;
	Vehicle_Type_Inputs[i].length = 40;
	Vehicle_Type_Inputs[i].headway_factor = 1.2;
	Vehicle_Type_Inputs[i].average_occupancy = 25.0;
	Vehicle_Type_Inputs[i].non_emergency_decel = -15.0;
	Vehicle_Type_Inputs[i].fleet_freeway_bus = 100;
	Vehicle_Type_Inputs[i].fleet_street_bus = 100;

	i++;
	Vehicle_Type_Inputs[i].length = 14;
	Vehicle_Type_Inputs[i].headway_factor = 1.0;
	Vehicle_Type_Inputs[i].average_occupancy = 2.5;
	Vehicle_Type_Inputs[i].non_emergency_decel = -15.0;
	Vehicle_Type_Inputs[i].fleet_freeway_carpool = 25;
	Vehicle_Type_Inputs[i].fleet_street_carpool = 25;

	i++;
	Vehicle_Type_Inputs[i].length = 16;
	Vehicle_Type_Inputs[i].headway_factor = 1.0;
	Vehicle_Type_Inputs[i].average_occupancy = 2.5;
	Vehicle_Type_Inputs[i].non_emergency_decel = -15.0;
	Vehicle_Type_Inputs[i].fleet_freeway_carpool = 75;
	Vehicle_Type_Inputs[i].fleet_street_carpool = 75;
}

void Host_Default_Network_define_freeway_link_data(FREEWAY_LINK* freeway_link_data)
{
	int i = 0;
	//std::cout << std::endl << "i = " << i << std::endl;
	freeway_link_data[i].usn = 101;
	freeway_link_data[i].dsn = 102;
	freeway_link_data[i].thrunode = 103;
	freeway_link_data[i].length = 439;
	freeway_link_data[i].fulllanes = 2;
	freeway_link_data[i].freeflowspeed = 65;
	freeway_link_data[i].linktype = 0;
	freeway_link_data[i].mainline_receiving_lane = 1;
	freeway_link_data[i].pavement = 1;
	freeway_link_data[i].thru_percent = 100;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	freeway_link_data[i].usn = 102;
	freeway_link_data[i].dsn = 103;
	freeway_link_data[i].thrunode = 104;
	freeway_link_data[i].exitnode = 105;
	freeway_link_data[i].length = 398;
	freeway_link_data[i].fulllanes = 2;
	freeway_link_data[i].auxlaneid[0] = 9;
	freeway_link_data[i].auxlanecode[0] = 3;
	freeway_link_data[i].auxlanelength[0] = 398;
	freeway_link_data[i].freeflowspeed = 65;
	freeway_link_data[i].linktype = 0;
	freeway_link_data[i].mainline_receiving_lane = 1;
	freeway_link_data[i].offramp_sending_lane = 9;
	freeway_link_data[i].thru_percent = 80;
	freeway_link_data[i].offramp_warn_distance = 2500;
	for(int nt = 0; nt < 16; nt++)
	{
		freeway_link_data[i].multiplier_exit[nt] = 1.0;
	}
	freeway_link_data[i].pavement = 1;
	freeway_link_data[i].anticip_warning_distance = 1500;
	freeway_link_data[i].anticip_warning_speed = 43;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	freeway_link_data[i].usn = 103;
	freeway_link_data[i].dsn = 104;
	freeway_link_data[i].thrunode = 7001;
	freeway_link_data[i].length = 856;
	freeway_link_data[i].fulllanes = 2;
	freeway_link_data[i].freeflowspeed = 65;
	freeway_link_data[i].linktype = 0;
	freeway_link_data[i].mainline_receiving_lane = 1;
	freeway_link_data[i].pavement = 1;
	freeway_link_data[i].thru_percent = 100;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	freeway_link_data[i].usn = 103;
	freeway_link_data[i].dsn = 105;
	freeway_link_data[i].thrunode = 106;
	freeway_link_data[i].length = 398;
	freeway_link_data[i].fulllanes = 1;
	freeway_link_data[i].freeflowspeed = 45;
	freeway_link_data[i].linktype = 1;
	freeway_link_data[i].mainline_receiving_lane = 1;
	freeway_link_data[i].pavement = 1;
	freeway_link_data[i].thru_percent = 100;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	freeway_link_data[i].usn = 104;
	freeway_link_data[i].dsn = 7001;
	freeway_link_data[i].thrunode = 0;
	freeway_link_data[i].length = 611;
	freeway_link_data[i].fulllanes = 2;
	freeway_link_data[i].freeflowspeed = 65;
	freeway_link_data[i].linktype = 0;
	freeway_link_data[i].mainline_receiving_lane = 1;
	freeway_link_data[i].pavement = 1;
	freeway_link_data[i].thru_percent = 100;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	freeway_link_data[i].usn = 105;
	freeway_link_data[i].dsn = 106;
	freeway_link_data[i].thrunode = 0;
	freeway_link_data[i].length = 357;
	freeway_link_data[i].fulllanes = 1;
	freeway_link_data[i].freeflowspeed = 45;
	freeway_link_data[i].linktype = 1;
	freeway_link_data[i].mainline_receiving_lane = 1;
	freeway_link_data[i].pavement = 1;
	freeway_link_data[i].thru_percent = 100;

	//i++;
	////std::cout << std::endl << "i = " << i << std::endl;
	//freeway_link_data[i].usn = 106;
	//freeway_link_data[i].dsn = 8009;
	//freeway_link_data[i].thrunode = 0;
	//freeway_link_data[i].length = 0;
	//freeway_link_data[i].fulllanes = 1;
	//freeway_link_data[i].linktype = 1;
	//freeway_link_data[i].pavement = 1;
	//freeway_link_data[i].thru_percent = 100;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	freeway_link_data[i].usn = 107;
	freeway_link_data[i].dsn = 102;
	freeway_link_data[i].thrunode = 103;
	freeway_link_data[i].length = 239;
	freeway_link_data[i].fulllanes = 1;
	freeway_link_data[i].freeflowspeed = 65;
	freeway_link_data[i].startup_time = 1.0;
	freeway_link_data[i].linktype = 1;
	freeway_link_data[i].mainline_receiving_lane = 9;
	freeway_link_data[i].pavement = 1;
	freeway_link_data[i].thru_percent = 100;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	freeway_link_data[i].usn = 200;
	freeway_link_data[i].dsn = 201;
	freeway_link_data[i].thrunode = 202;
	freeway_link_data[i].length = 835;
	freeway_link_data[i].fulllanes = 2;
	freeway_link_data[i].adddrop_code[0] = 1;
	freeway_link_data[i].adddrop_lane[0] = 3;
	freeway_link_data[i].adddrop_dist[0] = 0;
	freeway_link_data[i].freeflowspeed = 65;
	freeway_link_data[i].linktype = 0;
	freeway_link_data[i].mainline_receiving_lane = 1;
	freeway_link_data[i].nhov_lanes = 1;
	freeway_link_data[i].hov_code = 0;
	freeway_link_data[i].hov_begin = 0;
	freeway_link_data[i].hov_end = 0;
	freeway_link_data[i].hov_side = 0;
	freeway_link_data[i].hov_type = 0;
	freeway_link_data[i].hov_warn = 5280;
	freeway_link_data[i].pavement = 1;
	freeway_link_data[i].thru_percent = 100;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	freeway_link_data[i].usn = 201;
	freeway_link_data[i].dsn = 202;
	freeway_link_data[i].thrunode = 0;
	freeway_link_data[i].length = 1448;
	freeway_link_data[i].fulllanes = 3;
	freeway_link_data[i].freeflowspeed = 65;
	freeway_link_data[i].linktype = 0;
	freeway_link_data[i].mainline_receiving_lane = 1;
	freeway_link_data[i].nhov_lanes = 1;
	freeway_link_data[i].hov_code = 0;
	freeway_link_data[i].hov_begin = 0;
	freeway_link_data[i].hov_end = 0;
	freeway_link_data[i].hov_side = 0;
	freeway_link_data[i].hov_type = 0;
	freeway_link_data[i].hov_warn = 5280;
	freeway_link_data[i].pavement = 1;
	freeway_link_data[i].thru_percent = 100;

	//i++;
	////std::cout << std::endl << "i = " << i << std::endl;
	//freeway_link_data[i].usn = 202;
	//freeway_link_data[i].dsn = 8004;
	//freeway_link_data[i].thrunode = 0;
	//freeway_link_data[i].length = 0;
	//freeway_link_data[i].fulllanes = 3;
	//freeway_link_data[i].freeflowspeed = 65;
	//freeway_link_data[i].linktype = 0;
	//freeway_link_data[i].pavement = 1;
	//freeway_link_data[i].thru_percent = 100;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	freeway_link_data[i].usn = 7002;
	freeway_link_data[i].dsn = 200;
	freeway_link_data[i].thrunode = 201;
	freeway_link_data[i].length = 616;
	freeway_link_data[i].fulllanes = 2;
	freeway_link_data[i].freeflowspeed = 65;
	freeway_link_data[i].linktype = 0;
	freeway_link_data[i].mainline_receiving_lane = 1;
	freeway_link_data[i].pavement = 1;
	freeway_link_data[i].thru_percent = 100;

	//i++;
	////std::cout << std::endl << "i = " << i << std::endl;
	//freeway_link_data[i].usn = 8101;
	//freeway_link_data[i].dsn = 101;
	//freeway_link_data[i].thrunode = 102;
	//freeway_link_data[i].length = 0;
	//freeway_link_data[i].fulllanes = 2;
	//freeway_link_data[i].freeflowspeed = 65;
	//freeway_link_data[i].linktype = 0;
	//freeway_link_data[i].mainline_receiving_lane = 1;
	//freeway_link_data[i].pavement = 1;
	//freeway_link_data[i].thru_percent = 100;

	//i++;
	////std::cout << std::endl << "i = " << i << std::endl;
	//freeway_link_data[i].usn = 8102;
	//freeway_link_data[i].dsn = 107;
	//freeway_link_data[i].thrunode = 102;
	//freeway_link_data[i].length = 0;
	//freeway_link_data[i].fulllanes = 1;
	//freeway_link_data[i].freeflowspeed = 65;
	//freeway_link_data[i].linktype = 1;
	//freeway_link_data[i].mainline_receiving_lane = 1;
	//freeway_link_data[i].pavement = 1;
	//freeway_link_data[i].thru_percent = 100;

	for (int il = 0; il < i; ++il)
		freeway_link_data[il].ID = il+1;
}
void Host_Default_Network_define_street_link_data(STREET_LINK* street_link_data)
{
	//Set channelization defaults to -1 to indicate that the lane does not exist
	for(int i = 0; i < 22; i++) 
	{
		for(int j = 0; j < 7; j++)
		{
			street_link_data[i].channelization[j] = -1;
		}
	}

	int i = 0;
	//std::cout << std::endl << "i = " << i << std::endl;
	street_link_data[i].usn = 1;
	street_link_data[i].dsn = 5;
	street_link_data[i].leftnode = 2;
	street_link_data[i].thrunode = 3;
	street_link_data[i].rightnode = 9;
	street_link_data[i].opposenode = street_link_data[i].thrunode;
	street_link_data[i].length = 454;
	street_link_data[i].fulllanes = 1;
	street_link_data[i].leftturnbays = 1;
	street_link_data[i].rightturnbays = 1;
	//street_link_data[i].lengthofleftbay = 80;
	//street_link_data[i].lengthofrightbay = 50;
	street_link_data[i].freeflowspeed = 30;
	street_link_data[i].channelization[0] = 11;
	street_link_data[i].channelization[5] = 4;
	street_link_data[i].channelization[6] = 1;
	street_link_data[i].leftpct = 25;
	street_link_data[i].thrupct = 50;
	street_link_data[i].rightpct = 25;
	street_link_data[i].rtor = 1;
	for(int nt = 0; nt < 16; nt++)
	{
		street_link_data[i].multiplier_left[nt] = 1.0;
		street_link_data[i].multiplier_thru[nt] = 1.0;
		street_link_data[i].multiplier_right[nt] = 1.0;
		street_link_data[i].multiplier_diag[nt] = 1.0;
	}

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	street_link_data[i].usn = 2;
	street_link_data[i].dsn = 5;
	street_link_data[i].leftnode = 3;
	street_link_data[i].thrunode = 9;
	street_link_data[i].rightnode = 1;
	street_link_data[i].opposenode = street_link_data[i].thrunode;
	street_link_data[i].length = 766;
	street_link_data[i].fulllanes = 2;
	street_link_data[i].leftturnbays = 1;
	street_link_data[i].rightturnbays = 1;
	//street_link_data[i].lengthofleftbay = 100;
	//street_link_data[i].lengthofrightbay = 50;
	street_link_data[i].freeflowspeed = 30;
	street_link_data[i].channelization[0] = 11;
	street_link_data[i].channelization[1] = 11;
	street_link_data[i].channelization[5] = 4;
	street_link_data[i].channelization[6] = 1;
	street_link_data[i].leftpct = 25;
	street_link_data[i].thrupct = 50;
	street_link_data[i].rightpct = 25;
	street_link_data[i].rtor = 1;
	for(int nt = 0; nt < 16; nt++)
	{
		street_link_data[i].multiplier_left[nt] = 1.0;
		street_link_data[i].multiplier_thru[nt] = 1.0;
		street_link_data[i].multiplier_right[nt] = 1.0;
		street_link_data[i].multiplier_diag[nt] = 1.0;
	}

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	street_link_data[i].usn = 2;
	street_link_data[i].dsn = 6;
	street_link_data[i].thrunode = 0;
	street_link_data[i].length = 565;
	street_link_data[i].fulllanes = 2;
	street_link_data[i].freeflowspeed = 30;
	street_link_data[i].channelization[0] = 0;
	street_link_data[i].channelization[1] = 0;
	street_link_data[i].thrupct = 100;
	for(int nt = 0; nt < 16; nt++)
	{
		street_link_data[i].multiplier_left[nt] = 1.0;
		street_link_data[i].multiplier_thru[nt] = 1.0;
		street_link_data[i].multiplier_right[nt] = 1.0;
		street_link_data[i].multiplier_diag[nt] = 1.0;
	}

	//i++;
	////std::cout << std::endl << "i = " << i << std::endl;
	//street_link_data[i].usn = 6;
	//street_link_data[i].dsn = 8002;
	////street_link_data[i].lane1 = 1;
	////street_link_data[i].lane2 = 1;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	street_link_data[i].usn = 2;
	street_link_data[i].dsn = 7;
	street_link_data[i].thrunode = 0;
	street_link_data[i].length = 491;
	street_link_data[i].fulllanes = 2;
	street_link_data[i].freeflowspeed = 30;
	street_link_data[i].channelization[0] = 0;
	street_link_data[i].channelization[1] = 0;
	street_link_data[i].thrupct = 100;
	for(int nt = 0; nt < 16; nt++)
	{
		street_link_data[i].multiplier_left[nt] = 1.0;
		street_link_data[i].multiplier_thru[nt] = 1.0;
		street_link_data[i].multiplier_right[nt] = 1.0;
		street_link_data[i].multiplier_diag[nt] = 1.0;
	}
	//i++;
	////std::cout << std::endl << "i = " << i << std::endl;
	//street_link_data[i].usn = 7;
	//street_link_data[i].dsn = 8005;
	////street_link_data[i].lane1 = 1;
	////street_link_data[i].lane2 = 1;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	street_link_data[i].usn = 2;
	street_link_data[i].dsn = 8;
	street_link_data[i].thrunode = 0;
	street_link_data[i].length = 514;
	street_link_data[i].fulllanes = 2;
	street_link_data[i].freeflowspeed = 30;
	street_link_data[i].channelization[0] = 0;
	street_link_data[i].channelization[1] = 0;
	street_link_data[i].thrupct = 100;
	for(int nt = 0; nt < 16; nt++)
	{
		street_link_data[i].multiplier_left[nt] = 1.0;
		street_link_data[i].multiplier_thru[nt] = 1.0;
		street_link_data[i].multiplier_right[nt] = 1.0;
		street_link_data[i].multiplier_diag[nt] = 1.0;
	}
	//i++;
	////std::cout << std::endl << "i = " << i << std::endl;
	//street_link_data[i].usn = 8;
	//street_link_data[i].dsn = 8006;
	////street_link_data[i].lane1 = 1;
	////street_link_data[i].lane2 = 1;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	street_link_data[i].usn = 3;
	street_link_data[i].dsn = 5;
	street_link_data[i].leftnode = 9;
	street_link_data[i].thrunode = 1;
	street_link_data[i].rightnode = 2;
	street_link_data[i].opposenode = street_link_data[i].thrunode;
	street_link_data[i].length = 454;
	street_link_data[i].fulllanes = 1;
	street_link_data[i].leftturnbays = 1;
	street_link_data[i].rightturnbays = 1;
	//street_link_data[i].lengthofleftbay = 50;
	//street_link_data[i].lengthofrightbay = 50;
	street_link_data[i].freeflowspeed = 30;
	street_link_data[i].channelization[0] = 11;
	street_link_data[i].channelization[5] = 4;
	street_link_data[i].channelization[6] = 1;
	street_link_data[i].leftpct = 25;
	street_link_data[i].thrupct = 50;
	street_link_data[i].rightpct = 25;
	street_link_data[i].rtor = 1;
	for(int nt = 0; nt < 16; nt++)
	{
		street_link_data[i].multiplier_left[nt] = 1.0;
		street_link_data[i].multiplier_thru[nt] = 1.0;
		street_link_data[i].multiplier_right[nt] = 1.0;
		street_link_data[i].multiplier_diag[nt] = 1.0;
	}

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	street_link_data[i].usn = 4;
	street_link_data[i].dsn = 9;
	street_link_data[i].leftnode = 10;
	street_link_data[i].thrunode = 5;
	street_link_data[i].rightnode = 11;
	street_link_data[i].opposenode = street_link_data[i].thrunode;
	street_link_data[i].length = 442;
	street_link_data[i].fulllanes = 2;
	street_link_data[i].freeflowspeed = 30;
	street_link_data[i].channelization[0] = 0;
	street_link_data[i].channelization[1] = 0;
	street_link_data[i].leftpct = 33;
	street_link_data[i].thrupct = 34;
	street_link_data[i].rightpct = 33;
	for(int nt = 0; nt < 16; nt++)
	{
		street_link_data[i].multiplier_left[nt] = 1.0;
		street_link_data[i].multiplier_thru[nt] = 1.0;
		street_link_data[i].multiplier_right[nt] = 1.0;
		street_link_data[i].multiplier_diag[nt] = 1.0;
	}

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	street_link_data[i].usn = 4;
	street_link_data[i].dsn = 7002;
	street_link_data[i].thrunode = 0;
	street_link_data[i].length = 604;
	street_link_data[i].fulllanes = 2;
	street_link_data[i].freeflowspeed = 45;
	street_link_data[i].channelization[0] = 0;
	street_link_data[i].channelization[1] = 0;
	street_link_data[i].thrupct = 100;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	street_link_data[i].usn = 5;
	street_link_data[i].dsn = 1;
	street_link_data[i].thrunode = 0;
	street_link_data[i].length = 545;
	street_link_data[i].fulllanes = 2;
	street_link_data[i].freeflowspeed = 30;
	street_link_data[i].channelization[0] = 0;
	street_link_data[i].channelization[1] = 0;
	street_link_data[i].thrupct = 100;
	for(int nt = 0; nt < 16; nt++)
	{
		street_link_data[i].multiplier_left[nt] = 1.0;
		street_link_data[i].multiplier_thru[nt] = 1.0;
		street_link_data[i].multiplier_right[nt] = 1.0;
		street_link_data[i].multiplier_diag[nt] = 1.0;
	}
	//i++;
	////std::cout << std::endl << "i = " << i << std::endl;
	//street_link_data[i].usn = 1;
	//street_link_data[i].dsn = 8001;
	////street_link_data[i].lane1 = 1;
	////street_link_data[i].lane2 = 1;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	street_link_data[i].usn = 5;
	street_link_data[i].dsn = 2;
	street_link_data[i].leftnode = 7;
	street_link_data[i].thrunode = 6;
	street_link_data[i].rightnode = 8;
	street_link_data[i].length = 833;
	street_link_data[i].fulllanes = 2;
	street_link_data[i].leftturnbays = 1;
	street_link_data[i].rightturnbays = 1;
	//street_link_data[i].lengthofleftbay = 100;
	//street_link_data[i].lengthofrightbay = 50;
	street_link_data[i].freeflowspeed = 30;
	street_link_data[i].ped_code = 2;
	street_link_data[i].channelization[0] = 0;
	street_link_data[i].channelization[1] = 0;
	street_link_data[i].channelization[5] = 4;
	street_link_data[i].channelization[6] = 1;
	street_link_data[i].leftpct = 1;
	street_link_data[i].thrupct = 98;
	street_link_data[i].rightpct = 1;
	for(int nt = 0; nt < 16; nt++)
	{
		street_link_data[i].multiplier_left[nt] = 1.0;
		street_link_data[i].multiplier_thru[nt] = 1.0;
		street_link_data[i].multiplier_right[nt] = 1.0;
		street_link_data[i].multiplier_diag[nt] = 1.0;
	}

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	street_link_data[i].usn = 5;
	street_link_data[i].dsn = 3;
	street_link_data[i].thrunode = 0;
	street_link_data[i].length = 545;
	street_link_data[i].fulllanes = 2;
	street_link_data[i].freeflowspeed = 30;
	street_link_data[i].channelization[0] = 0;
	street_link_data[i].channelization[1] = 0;
	street_link_data[i].thrupct = 100;
	for(int nt = 0; nt < 16; nt++)
	{
		street_link_data[i].multiplier_left[nt] = 1.0;
		street_link_data[i].multiplier_thru[nt] = 1.0;
		street_link_data[i].multiplier_right[nt] = 1.0;
		street_link_data[i].multiplier_diag[nt] = 1.0;
	}

	//i++;
	////std::cout << std::endl << "i = " << i << std::endl;
	//street_link_data[i].usn = 3;
	//street_link_data[i].dsn = 8003;
	////street_link_data[i].lane1 = 1;
	////street_link_data[i].lane2 = 1;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	street_link_data[i].usn = 5;
	street_link_data[i].dsn = 9;
	street_link_data[i].leftnode = 11;
	street_link_data[i].thrunode = 4;
	street_link_data[i].rightnode = 10;
	street_link_data[i].opposenode = street_link_data[i].thrunode;
	street_link_data[i].length = 559;
	street_link_data[i].fulllanes = 2;
	street_link_data[i].freeflowspeed = 30;
	street_link_data[i].channelization[0] = 0;
	street_link_data[i].channelization[1] = 0;
	street_link_data[i].leftpct = 25;
	street_link_data[i].thrupct = 50;
	street_link_data[i].rightpct = 25;
	for(int nt = 0; nt < 16; nt++)
	{
		street_link_data[i].multiplier_left[nt] = 1.0;
		street_link_data[i].multiplier_thru[nt] = 1.0;
		street_link_data[i].multiplier_right[nt] = 1.0;
		street_link_data[i].multiplier_diag[nt] = 1.0;
	}

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	street_link_data[i].usn = 6;
	street_link_data[i].dsn = 2;
	street_link_data[i].leftnode = 8;
	street_link_data[i].thrunode = 5;
	street_link_data[i].rightnode = 7;
	//street_link_data[i].opposenode = street_link_data[i].thrunode;
	street_link_data[i].length = 565;
	street_link_data[i].fulllanes = 2;
	street_link_data[i].leftturnbays = 1;
	street_link_data[i].rightturnbays = 1;
	//street_link_data[i].lengthofleftbay = 100;
	//street_link_data[i].lengthofrightbay = 50;
	street_link_data[i].freeflowspeed = 30;
	street_link_data[i].ped_code = 2;
	street_link_data[i].channelization[0] = 0;
	street_link_data[i].channelization[1] = 0;
	street_link_data[i].channelization[5] = 4;
	street_link_data[i].channelization[6] = 1;
	street_link_data[i].leftpct = 33;
	street_link_data[i].thrupct = 34;
	street_link_data[i].rightpct = 33;
	for(int nt = 0; nt < 16; nt++)
	{
		street_link_data[i].multiplier_left[nt] = 1.0;
		street_link_data[i].multiplier_thru[nt] = 1.0;
		street_link_data[i].multiplier_right[nt] = 1.0;
		street_link_data[i].multiplier_diag[nt] = 1.0;
	}

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	street_link_data[i].usn = 7;
	street_link_data[i].dsn = 2;
	street_link_data[i].leftnode = 6;
	street_link_data[i].thrunode = 8;
	street_link_data[i].rightnode = 5;
	street_link_data[i].opposenode = street_link_data[i].thrunode;
	street_link_data[i].length = 491;
	street_link_data[i].fulllanes = 3;
	street_link_data[i].leftturnbays = 1;
	street_link_data[i].rightturnbays = 1;
	//street_link_data[i].lengthofleftbay = 100;
	//street_link_data[i].lengthofrightbay = 50;
	street_link_data[i].freeflowspeed = 30;
	street_link_data[i].ped_code = 2;
	street_link_data[i].channelization[0] = 0;
	street_link_data[i].channelization[1] = 0;
	street_link_data[i].channelization[2] = 0;
	street_link_data[i].channelization[5] = 4;
	street_link_data[i].channelization[6] = 1;
	street_link_data[i].leftpct = 33;
	street_link_data[i].thrupct = 34;
	street_link_data[i].rightpct = 33;
	for(int nt = 0; nt < 16; nt++)
	{
		street_link_data[i].multiplier_left[nt] = 1.0;
		street_link_data[i].multiplier_thru[nt] = 1.0;
		street_link_data[i].multiplier_right[nt] = 1.0;
		street_link_data[i].multiplier_diag[nt] = 1.0;
	}

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	street_link_data[i].usn = 8;
	street_link_data[i].dsn = 2;
	street_link_data[i].leftnode = 5;
	street_link_data[i].thrunode = 7;
	street_link_data[i].rightnode = 6;
	street_link_data[i].opposenode = street_link_data[i].thrunode;
	street_link_data[i].length = 514;
	street_link_data[i].fulllanes = 2;
	street_link_data[i].leftturnbays = 1;
	street_link_data[i].rightturnbays = 1;
	//street_link_data[i].lengthofleftbay = 100;
	//street_link_data[i].lengthofrightbay = 50;
	street_link_data[i].freeflowspeed = 30;
	street_link_data[i].ped_code = 2;
	street_link_data[i].channelization[0] = 0;
	street_link_data[i].channelization[1] = 0;
	street_link_data[i].channelization[5] = 4;
	street_link_data[i].channelization[6] = 1;
	street_link_data[i].leftpct = 33;
	street_link_data[i].thrupct = 34;
	street_link_data[i].rightpct = 33;
	for(int nt = 0; nt < 16; nt++)
	{
		street_link_data[i].multiplier_left[nt] = 1.0;
		street_link_data[i].multiplier_thru[nt] = 1.0;
		street_link_data[i].multiplier_right[nt] = 1.0;
		street_link_data[i].multiplier_diag[nt] = 1.0;
	}

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	street_link_data[i].usn = 9;
	street_link_data[i].dsn = 4;
	street_link_data[i].thrunode = 7002;
	street_link_data[i].length = 473;
	street_link_data[i].fulllanes = 2;
	street_link_data[i].freeflowspeed = 30;
	street_link_data[i].channelization[0] = 0;
	street_link_data[i].channelization[1] = 0;
	street_link_data[i].thrupct = 100;
	for(int nt = 0; nt < 16; nt++)
	{
		street_link_data[i].multiplier_left[nt] = 1.0;
		street_link_data[i].multiplier_thru[nt] = 1.0;
		street_link_data[i].multiplier_right[nt] = 1.0;
		street_link_data[i].multiplier_diag[nt] = 1.0;
	}

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	street_link_data[i].usn = 9;
	street_link_data[i].dsn = 5;
	street_link_data[i].leftnode = 1;
	street_link_data[i].thrunode = 2;
	street_link_data[i].rightnode = 3;
	street_link_data[i].opposenode = street_link_data[i].thrunode;
	street_link_data[i].length = 524;
	street_link_data[i].fulllanes = 2;
	street_link_data[i].leftturnbays = 1;
	street_link_data[i].rightturnbays = 1;
	//street_link_data[i].lengthofleftbay = 100;
	//street_link_data[i].lengthofrightbay = 50;
	street_link_data[i].freeflowspeed = 30;
	street_link_data[i].channelization[0] = 11;
	street_link_data[i].channelization[1] = 11;
	street_link_data[i].channelization[5] = 4;
	street_link_data[i].channelization[6] = 1;
	street_link_data[i].leftpct = 25;
	street_link_data[i].thrupct = 50;
	street_link_data[i].rightpct = 25;
	street_link_data[i].rtor = 1;
	for(int nt = 0; nt < 16; nt++)
	{
		street_link_data[i].multiplier_left[nt] = 1.0;
		street_link_data[i].multiplier_thru[nt] = 1.0;
		street_link_data[i].multiplier_right[nt] = 1.0;
		street_link_data[i].multiplier_diag[nt] = 1.0;
	}

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	street_link_data[i].usn = 9;
	street_link_data[i].dsn = 10;
	street_link_data[i].thrunode = 0;
	street_link_data[i].length = 471;
	street_link_data[i].fulllanes = 2;
	street_link_data[i].freeflowspeed = 30;
	street_link_data[i].channelization[0] = 0;
	street_link_data[i].channelization[1] = 0;
	street_link_data[i].thrupct = 100;
	for(int nt = 0; nt < 16; nt++)
	{
		street_link_data[i].multiplier_left[nt] = 1.0;
		street_link_data[i].multiplier_thru[nt] = 1.0;
		street_link_data[i].multiplier_right[nt] = 1.0;
		street_link_data[i].multiplier_diag[nt] = 1.0;
	}

	//i++;
	////std::cout << std::endl << "i = " << i << std::endl;
	//street_link_data[i].usn = 10;
	//street_link_data[i].dsn = 8007;
	////street_link_data[i].lane1 = 1;
	////street_link_data[i].lane2 = 1;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	street_link_data[i].usn = 9;
	street_link_data[i].dsn = 11;
	street_link_data[i].thrunode = 0;
	street_link_data[i].length = 515;
	street_link_data[i].fulllanes = 2;
	street_link_data[i].freeflowspeed = 30;
	street_link_data[i].channelization[0] = 0;
	street_link_data[i].channelization[1] = 0;
	street_link_data[i].thrupct = 100;
	for(int nt = 0; nt < 16; nt++)
	{
		street_link_data[i].multiplier_left[nt] = 1.0;
		street_link_data[i].multiplier_thru[nt] = 1.0;
		street_link_data[i].multiplier_right[nt] = 1.0;
		street_link_data[i].multiplier_diag[nt] = 1.0;
	}
	//i++;
	////std::cout << std::endl << "i = " << i << std::endl;
	//street_link_data[i].usn = 11;
	//street_link_data[i].dsn = 8008;
	////street_link_data[i].lane1 = 1;
	////street_link_data[i].lane2 = 1;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	street_link_data[i].usn = 10;
	street_link_data[i].dsn = 9;
	street_link_data[i].leftnode = 5;
	street_link_data[i].thrunode = 11;
	street_link_data[i].rightnode = 4;
	street_link_data[i].opposenode = street_link_data[i].thrunode;
	street_link_data[i].length = 471;
	street_link_data[i].fulllanes = 2;
	street_link_data[i].freeflowspeed = 30;
	street_link_data[i].channelization[0] = 0;
	street_link_data[i].channelization[1] = 0;
	street_link_data[i].leftpct = 33;
	street_link_data[i].thrupct = 34;
	street_link_data[i].rightpct = 33;
	for(int nt = 0; nt < 16; nt++)
	{
		street_link_data[i].multiplier_left[nt] = 1.0;
		street_link_data[i].multiplier_thru[nt] = 1.0;
		street_link_data[i].multiplier_right[nt] = 1.0;
		street_link_data[i].multiplier_diag[nt] = 1.0;
	}

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	street_link_data[i].usn = 11;
	street_link_data[i].dsn = 9;
	street_link_data[i].leftnode = 4;
	street_link_data[i].thrunode = 10;
	street_link_data[i].rightnode = 5;
	street_link_data[i].opposenode = street_link_data[i].thrunode;
	street_link_data[i].length = 515;
	street_link_data[i].fulllanes = 2;
	street_link_data[i].freeflowspeed = 30;
	street_link_data[i].channelization[0] = 0;
	street_link_data[i].channelization[1] = 0;
	street_link_data[i].leftpct = 33;
	street_link_data[i].thrupct = 34;
	street_link_data[i].rightpct = 33;
	for(int nt = 0; nt < 16; nt++)
	{
		street_link_data[i].multiplier_left[nt] = 1.0;
		street_link_data[i].multiplier_thru[nt] = 1.0;
		street_link_data[i].multiplier_right[nt] = 1.0;
		street_link_data[i].multiplier_diag[nt] = 1.0;
	}

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	street_link_data[i].usn = 7001;
	street_link_data[i].dsn = 4;
	street_link_data[i].thrunode = 9;
	street_link_data[i].length = 601;
	street_link_data[i].fulllanes = 2;
	street_link_data[i].freeflowspeed = 45;
	street_link_data[i].channelization[0] = 0;
	street_link_data[i].channelization[1] = 0;
	street_link_data[i].thrupct = 100;

	//i++;
	////std::cout << std::endl << "i = " << i << std::endl;
	//street_link_data[i].usn = 8001;
	//street_link_data[i].dsn = 1;
	//street_link_data[i].thrunode = 5;
	//street_link_data[i].length = 0;
	//street_link_data[i].fulllanes = 1;
	//street_link_data[i].freeflowspeed = 30;
	//street_link_data[i].channelization[0] = 0;
	//street_link_data[i].thrupct = 100;

	//i++;
	////std::cout << std::endl << "i = " << i << std::endl;
	//street_link_data[i].usn = 8002;
	//street_link_data[i].dsn = 6;
	//street_link_data[i].thrunode = 2;
	//street_link_data[i].length = 0;
	//street_link_data[i].fulllanes = 2;
	//street_link_data[i].freeflowspeed = 30;
	//street_link_data[i].channelization[0] = 0;
	//street_link_data[i].channelization[1] = 0;
	//street_link_data[i].thrupct = 100;

	//i++;
	////std::cout << std::endl << "i = " << i << std::endl;
	//street_link_data[i].usn = 8003;
	//street_link_data[i].dsn = 3;
	//street_link_data[i].thrunode = 5;
	//street_link_data[i].length = 0;
	//street_link_data[i].fulllanes = 1;
	//street_link_data[i].freeflowspeed = 30;
	//street_link_data[i].channelization[0] = 0;
	//street_link_data[i].channelization[1] = 0;
	//street_link_data[i].thrupct = 100;

	//i++;
	////std::cout << std::endl << "i = " << i << std::endl;
	//street_link_data[i].usn = 8005;
	//street_link_data[i].dsn = 7;
	//street_link_data[i].thrunode = 2;
	//street_link_data[i].length = 0;
	//street_link_data[i].fulllanes = 2;
	//street_link_data[i].freeflowspeed = 30;
	//street_link_data[i].channelization[0] = 0;
	//street_link_data[i].channelization[1] = 0;
	//street_link_data[i].thrupct = 100;

	//i++;
	////std::cout << std::endl << "i = " << i << std::endl;
	//street_link_data[i].usn = 8006;
	//street_link_data[i].dsn = 8;
	//street_link_data[i].thrunode = 2;
	//street_link_data[i].length = 0;
	//street_link_data[i].fulllanes = 2;
	//street_link_data[i].freeflowspeed = 30;
	//street_link_data[i].channelization[0] = 0;
	//street_link_data[i].channelization[1] = 0;
	//street_link_data[i].thrupct = 100;

	//i++;
	////std::cout << std::endl << "i = " << i << std::endl;
	//street_link_data[i].usn = 8007;
	//street_link_data[i].dsn = 10;
	//street_link_data[i].thrunode = 9;
	//street_link_data[i].length = 0;
	//street_link_data[i].fulllanes = 2;
	//street_link_data[i].freeflowspeed = 30;
	//street_link_data[i].channelization[0] = 0;
	//street_link_data[i].channelization[1] = 0;
	//street_link_data[i].thrupct = 100;

	//i++;
	////std::cout << std::endl << "i = " << i << std::endl;
	//street_link_data[i].usn = 8008;
	//street_link_data[i].dsn = 11;
	//street_link_data[i].thrunode = 9;
	//street_link_data[i].length = 0;
	//street_link_data[i].fulllanes = 2;
	//street_link_data[i].freeflowspeed = 30;
	//street_link_data[i].channelization[0] = 0;
	//street_link_data[i].channelization[1] = 0;
	//street_link_data[i].thrupct = 100;

	for (int il = 0; il < i; ++il)
		street_link_data[il].id = il+1;
}
void Host_Default_Network_define_cond_turnpct_data(COND_TURNPCTS* cond_turnpct_data)
{
	//Define conditional turn percentages
	cond_turnpct_data[0].USN = 5;
	cond_turnpct_data[0].DSN = 2;

	cond_turnpct_data[0].LEFTPCT[0] = 0;
	cond_turnpct_data[0].LEFTPCT[1] = 0;
	cond_turnpct_data[0].LEFTPCT[2] = 100;
	cond_turnpct_data[0].LEFTPCT[3] = 0;

	cond_turnpct_data[0].THRUPCT[0] = 0;
	cond_turnpct_data[0].THRUPCT[1] = 100;
	cond_turnpct_data[0].THRUPCT[2] = 0;
	cond_turnpct_data[0].THRUPCT[3] = 0;

	cond_turnpct_data[0].RIGHTPCT[0] = 100;
	cond_turnpct_data[0].RIGHTPCT[1] = 0;
	cond_turnpct_data[0].RIGHTPCT[2] = 0;
	cond_turnpct_data[0].RIGHTPCT[3] = 0;
}
void Host_Default_Network_define_entrynode_inputs(ENTRYNODES_DATA* entrynode_inputs)
{
	int i = 0;
	entrynode_inputs[i].Node_ID = 1;
	entrynode_inputs[i].carpool_pct = .05;
	entrynode_inputs[i].flowrate = 125;
	entrynode_inputs[i].truck_pct = .10;
	entrynode_inputs[i].hov_violators_per10000 = 100;

	i++;
	entrynode_inputs[i].Node_ID = 6;
	entrynode_inputs[i].carpool_pct = .05;
	entrynode_inputs[i].flowrate = 1000;
	entrynode_inputs[i].truck_pct = .10;
	entrynode_inputs[i].hov_violators_per10000 = 100;

	i++;
	entrynode_inputs[i].Node_ID = 3;
	entrynode_inputs[i].carpool_pct = .05;
	entrynode_inputs[i].flowrate = 162;
	entrynode_inputs[i].truck_pct = .10;
	entrynode_inputs[i].hov_violators_per10000 = 100;

	i++;
	entrynode_inputs[i].Node_ID = 7;
	entrynode_inputs[i].carpool_pct = .05;
	entrynode_inputs[i].flowrate = 100;
	entrynode_inputs[i].truck_pct = .10;
	entrynode_inputs[i].hov_violators_per10000 = 100;

	i++;
	entrynode_inputs[i].Node_ID = 8;
	entrynode_inputs[i].carpool_pct = .05;
	entrynode_inputs[i].flowrate = 125;
	entrynode_inputs[i].truck_pct = .10;
	entrynode_inputs[i].hov_violators_per10000 = 100;

	i++;
	entrynode_inputs[i].Node_ID = 10;
	entrynode_inputs[i].carpool_pct = .05;
	entrynode_inputs[i].flowrate = 100;
	entrynode_inputs[i].truck_pct = .10;
	entrynode_inputs[i].hov_violators_per10000 = 100;

	i++;
	entrynode_inputs[i].Node_ID = 11;
	entrynode_inputs[i].carpool_pct = .05;
	entrynode_inputs[i].flowrate = 100;
	entrynode_inputs[i].truck_pct = .10;
	entrynode_inputs[i].hov_violators_per10000 = 100;

	i++;
	entrynode_inputs[i].Node_ID = 101;
	entrynode_inputs[i].carpool_pct = .05;
	entrynode_inputs[i].flowrate = 1000;
	entrynode_inputs[i].truck_pct = .10;
	entrynode_inputs[i].hov_violators_per10000 = 100;

	i++;
	entrynode_inputs[i].Node_ID = 107;
	entrynode_inputs[i].carpool_pct = .0;
	entrynode_inputs[i].flowrate = 500;
	entrynode_inputs[i].truck_pct = .0;
	entrynode_inputs[i].hov_violators_per10000 = 100;
}

void Host_Default_Network_define_ftc_data_inputs(FTC_DATA* ftc_data_inputs)
{
	int i = 0;
	ftc_data_inputs[i].node = 1;
	ftc_data_inputs[i].active_intervals = 1;
	ftc_data_inputs[i].approaches = 2;
	ftc_data_inputs[i].approach[0] = 5;
	ftc_data_inputs[i].approach[1] = 0;
	ftc_data_inputs[i].signal_code[0][0] = 1;
	ftc_data_inputs[i].signal_code[0][1] = 1;

	//i++;
	//ftc_data_inputs[i].node = 2;
	//ftc_data_inputs[i].active_intervals = 1;
	//ftc_data_inputs[i].approaches = 4;
	//ftc_data_inputs[i].approach[0] = 5;
	//ftc_data_inputs[i].approach[1] = 6;
	//ftc_data_inputs[i].approach[2] = 7;
	//ftc_data_inputs[i].approach[3] = 8;
	//ftc_data_inputs[i].signal_code[0][0] = 1;
	//ftc_data_inputs[i].signal_code[0][1] = 1;
	//ftc_data_inputs[i].signal_code[0][2] = 5;
	//ftc_data_inputs[i].signal_code[0][3] = 5;

	i++;
	ftc_data_inputs[i].node = 3;
	ftc_data_inputs[i].active_intervals = 1;
	ftc_data_inputs[i].approaches = 2;
	ftc_data_inputs[i].approach[0] = 5;
	ftc_data_inputs[i].approach[1] = 0;
	ftc_data_inputs[i].signal_code[0][0] = 1;
	ftc_data_inputs[i].signal_code[0][1] = 1;

	i++;
	ftc_data_inputs[i].node = 4;
	ftc_data_inputs[i].active_intervals = 1;
	ftc_data_inputs[i].approaches = 2;
	ftc_data_inputs[i].approach[0] = 9;
	ftc_data_inputs[i].approach[1] = 7001;
	ftc_data_inputs[i].signal_code[0][0] = 1;
	ftc_data_inputs[i].signal_code[0][1] = 1;

	i++;
	ftc_data_inputs[i].node = 5;
	ftc_data_inputs[i].active_intervals = 12;
	ftc_data_inputs[i].approaches = 4;
	ftc_data_inputs[i].approach[0] = 9;
	ftc_data_inputs[i].approach[1] = 3;
	ftc_data_inputs[i].approach[2] = 2;
	ftc_data_inputs[i].approach[3] = 1;
	ftc_data_inputs[i].duration[0] = 15;
	ftc_data_inputs[i].duration[1] = 3;
	ftc_data_inputs[i].duration[2] = 2;
	ftc_data_inputs[i].duration[3] = 20;
	ftc_data_inputs[i].duration[4] = 3;
	ftc_data_inputs[i].duration[5] = 2;
	ftc_data_inputs[i].duration[6] = 25;
	ftc_data_inputs[i].duration[7] = 3;
	ftc_data_inputs[i].duration[8] = 2;
	ftc_data_inputs[i].duration[9] = 35;
	ftc_data_inputs[i].duration[10] = 3;
	ftc_data_inputs[i].duration[11] = 2;

	ftc_data_inputs[i].signal_code[0][0] = 2;
	ftc_data_inputs[i].signal_code[0][1] = 4;
	ftc_data_inputs[i].signal_code[0][2] = 2;
	ftc_data_inputs[i].signal_code[0][3] = 4;

	ftc_data_inputs[i].signal_code[1][0] = 2;
	ftc_data_inputs[i].signal_code[1][1] = 0;
	ftc_data_inputs[i].signal_code[1][2] = 2;
	ftc_data_inputs[i].signal_code[1][3] = 0;

	ftc_data_inputs[i].signal_code[2][0] = 2;
	ftc_data_inputs[i].signal_code[2][1] = 2;
	ftc_data_inputs[i].signal_code[2][2] = 2;
	ftc_data_inputs[i].signal_code[2][3] = 2;

	ftc_data_inputs[i].signal_code[3][0] = 2;
	ftc_data_inputs[i].signal_code[3][1] = 9;
	ftc_data_inputs[i].signal_code[3][2] = 2;
	ftc_data_inputs[i].signal_code[3][3] = 9;

	ftc_data_inputs[i].signal_code[4][0] = 2;
	ftc_data_inputs[i].signal_code[4][1] = 0;
	ftc_data_inputs[i].signal_code[4][2] = 2;
	ftc_data_inputs[i].signal_code[4][3] = 0;

	ftc_data_inputs[i].signal_code[5][0] = 2;
	ftc_data_inputs[i].signal_code[5][1] = 2;
	ftc_data_inputs[i].signal_code[5][2] = 2;
	ftc_data_inputs[i].signal_code[5][3] = 2;

	ftc_data_inputs[i].signal_code[6][0] = 4;
	ftc_data_inputs[i].signal_code[6][1] = 2;
	ftc_data_inputs[i].signal_code[6][2] = 4;
	ftc_data_inputs[i].signal_code[6][3] = 2;

	ftc_data_inputs[i].signal_code[7][0] = 0;
	ftc_data_inputs[i].signal_code[7][1] = 2;
	ftc_data_inputs[i].signal_code[7][2] = 0;
	ftc_data_inputs[i].signal_code[7][3] = 2;

	ftc_data_inputs[i].signal_code[8][0] = 2;
	ftc_data_inputs[i].signal_code[8][1] = 2;
	ftc_data_inputs[i].signal_code[8][2] = 2;
	ftc_data_inputs[i].signal_code[8][3] = 2;

	ftc_data_inputs[i].signal_code[9][0] = 9;
	ftc_data_inputs[i].signal_code[9][1] = 2;
	ftc_data_inputs[i].signal_code[9][2] = 9;
	ftc_data_inputs[i].signal_code[9][3] = 2;

	ftc_data_inputs[i].signal_code[10][0] = 0;
	ftc_data_inputs[i].signal_code[10][1] = 2;
	ftc_data_inputs[i].signal_code[10][2] = 0;
	ftc_data_inputs[i].signal_code[10][3] = 2;

	ftc_data_inputs[i].signal_code[11][0] = 2;
	ftc_data_inputs[i].signal_code[11][1] = 2;
	ftc_data_inputs[i].signal_code[11][2] = 2;
	ftc_data_inputs[i].signal_code[11][3] = 2;

	i++;
	ftc_data_inputs[i].node = 6;
	ftc_data_inputs[i].active_intervals = 1;
	ftc_data_inputs[i].approaches = 2;
	ftc_data_inputs[i].approach[0] = 2;
	ftc_data_inputs[i].approach[1] = 0;
	ftc_data_inputs[i].signal_code[0][0] = 1;
	ftc_data_inputs[i].signal_code[0][1] = 1;

	i++;
	ftc_data_inputs[i].node = 7;
	ftc_data_inputs[i].active_intervals = 1;
	ftc_data_inputs[i].approaches = 2;
	ftc_data_inputs[i].approach[0] = 2;
	ftc_data_inputs[i].approach[1] = 0;
	ftc_data_inputs[i].signal_code[0][0] = 1;
	ftc_data_inputs[i].signal_code[0][1] = 1;

	i++;
	ftc_data_inputs[i].node = 8;
	ftc_data_inputs[i].active_intervals = 1;
	ftc_data_inputs[i].approaches = 2;
	ftc_data_inputs[i].approach[0] = 2;
	ftc_data_inputs[i].approach[1] = 0;
	ftc_data_inputs[i].signal_code[0][0] = 1;
	ftc_data_inputs[i].signal_code[0][1] = 1;

	i++;
	ftc_data_inputs[i].node = 9;
	ftc_data_inputs[i].active_intervals = 8;
	ftc_data_inputs[i].approaches = 4;
	ftc_data_inputs[i].approach[0] = 4;
	ftc_data_inputs[i].approach[1] = 11;
	ftc_data_inputs[i].approach[2] = 5;
	ftc_data_inputs[i].approach[3] = 10;
	ftc_data_inputs[i].duration[0] = 20;
	ftc_data_inputs[i].duration[1] = 3;
	ftc_data_inputs[i].duration[2] = 2;
	ftc_data_inputs[i].duration[3] = 40;
	ftc_data_inputs[i].duration[4] = 2;
	ftc_data_inputs[i].duration[5] = 3;
	ftc_data_inputs[i].duration[6] = 3;
	ftc_data_inputs[i].duration[7] = 2;

	ftc_data_inputs[i].signal_code[0][0] = 2;
	ftc_data_inputs[i].signal_code[0][1] = 1;
	ftc_data_inputs[i].signal_code[0][2] = 2;
	ftc_data_inputs[i].signal_code[0][3] = 1;

	ftc_data_inputs[i].signal_code[1][0] = 2;
	ftc_data_inputs[i].signal_code[1][1] = 0;
	ftc_data_inputs[i].signal_code[1][2] = 2;
	ftc_data_inputs[i].signal_code[1][3] = 0;

	ftc_data_inputs[i].signal_code[2][0] = 2;
	ftc_data_inputs[i].signal_code[2][1] = 2;
	ftc_data_inputs[i].signal_code[2][2] = 2;
	ftc_data_inputs[i].signal_code[2][3] = 2;

	ftc_data_inputs[i].signal_code[3][0] = 1;
	ftc_data_inputs[i].signal_code[3][1] = 2;
	ftc_data_inputs[i].signal_code[3][2] = 1;
	ftc_data_inputs[i].signal_code[3][3] = 2;

	ftc_data_inputs[i].signal_code[4][0] = 0;
	ftc_data_inputs[i].signal_code[4][1] = 2;
	ftc_data_inputs[i].signal_code[4][2] = 0;
	ftc_data_inputs[i].signal_code[4][3] = 2;

	ftc_data_inputs[i].signal_code[5][0] = 2;
	ftc_data_inputs[i].signal_code[5][1] = 2;
	ftc_data_inputs[i].signal_code[5][2] = 2;
	ftc_data_inputs[i].signal_code[5][3] = 2;

	ftc_data_inputs[i].signal_code[6][0] = 2;
	ftc_data_inputs[i].signal_code[6][1] = 2;
	ftc_data_inputs[i].signal_code[6][2] = 2;
	ftc_data_inputs[i].signal_code[6][3] = 2;

	ftc_data_inputs[i].signal_code[7][0] = 2;
	ftc_data_inputs[i].signal_code[7][1] = 2;
	ftc_data_inputs[i].signal_code[7][2] = 2;
	ftc_data_inputs[i].signal_code[7][3] = 2;

	i++;
	ftc_data_inputs[i].node = 10;
	ftc_data_inputs[i].active_intervals = 1;
	ftc_data_inputs[i].approaches = 2;
	ftc_data_inputs[i].approach[0] = 9;
	ftc_data_inputs[i].approach[1] = 0;
	ftc_data_inputs[i].signal_code[0][0] = 1;
	ftc_data_inputs[i].signal_code[0][1] = 1;

	i++;
	ftc_data_inputs[i].node = 11;
	ftc_data_inputs[i].active_intervals = 1;
	ftc_data_inputs[i].approaches = 2;
	ftc_data_inputs[i].approach[0] = 9;
	ftc_data_inputs[i].approach[1] = 0;
	ftc_data_inputs[i].signal_code[0][0] = 1;
	ftc_data_inputs[i].signal_code[0][1] = 1;

	for (int iftc = 0; iftc < i; ++iftc)
		for (int j = 0; j < MAX_DURATION; ++j)
			ftc_data_inputs[iftc].cycle_length += ftc_data_inputs[iftc].duration[j];
}
void Host_Default_Network_define_ac_data_inputs(AC_INPUTS* ac_data_inputs)
{
	/*
	int i = 0;
	ac_data_inputs[i].node = 2;
	ac_data_inputs[i].direct_approaches = 4;
	ac_data_inputs[i].direct_approach[0] = 5;
	ac_data_inputs[i].direct_approach[1] = 6;
	ac_data_inputs[i].direct_approach[2] = 7;
	ac_data_inputs[i].direct_approach[3] = 8;

	ac_data_inputs[i].cycle_length = 91;
	ac_data_inputs[i].yield_point = 34;
	ac_data_inputs[i].permissive_period_begin[1] = 11;
	ac_data_inputs[i].permissive_period_end[1] = 24;
	ac_data_inputs[i].permissive_period_begin[2] = 41;
	ac_data_inputs[i].permissive_period_end[2] = 41;
	ac_data_inputs[i].inhibitmax = 0;
	ac_data_inputs[i].currentphase[0] = 1;

	ac_data_inputs[i].phase[0].in_use = true;
	ac_data_inputs[i].phase[0].max_green = 15;
	ac_data_inputs[i].phase[0].min_green = 4;
	ac_data_inputs[i].phase[0].passage_time = 3.0;
	ac_data_inputs[i].phase[0].yellow_change_int = 2.0;
	ac_data_inputs[i].phase[0].red_clear_int = 3.0;
	ac_data_inputs[i].phase[0].lag_code = 0;
	ac_data_inputs[i].phase[0].detector1_id[0] = 1;
	ac_data_inputs[i].phase[0].forceoff_time = 52;
	ac_data_inputs[i].phase[0].permcodes = 111;
	ac_data_inputs[i].phase[0].movement_code[0] = 22222;
	ac_data_inputs[i].phase[0].movement_code[1] = 12222;
	ac_data_inputs[i].phase[0].movement_code[2] = 22222;
	ac_data_inputs[i].phase[0].movement_code[3] = 22222;

	ac_data_inputs[i].phase[1].in_use = true;
	ac_data_inputs[i].phase[1].max_green = 50;
	ac_data_inputs[i].phase[1].min_green = 10;
	ac_data_inputs[i].phase[1].passage_time = 3.0;
	ac_data_inputs[i].phase[1].yellow_change_int = 4.0;
	ac_data_inputs[i].phase[1].red_clear_int = 3.0;
	ac_data_inputs[i].phase[1].lag_code = 1;
	ac_data_inputs[i].phase[1].detector1_id[0] = 2;
	ac_data_inputs[i].phase[1].movement_code[0] = 22222;
	ac_data_inputs[i].phase[1].movement_code[1] = 22222;
	ac_data_inputs[i].phase[1].movement_code[2] = 22222;
	ac_data_inputs[i].phase[1].movement_code[3] = 11122;

	ac_data_inputs[i].phase[2].in_use = true;
	ac_data_inputs[i].phase[2].max_green = 15;
	ac_data_inputs[i].phase[2].min_green = 4;
	ac_data_inputs[i].phase[2].passage_time = 3.0;
	ac_data_inputs[i].phase[2].yellow_change_int = 2.0;
	ac_data_inputs[i].phase[2].red_clear_int = 3.0;
	ac_data_inputs[i].phase[2].lag_code = 0;
	ac_data_inputs[i].phase[2].forceoff_time = 11;
	ac_data_inputs[i].phase[2].permcodes = 100;
	ac_data_inputs[i].phase[2].movement_code[0] = 22222;
	ac_data_inputs[i].phase[2].movement_code[1] = 22222;
	ac_data_inputs[i].phase[2].movement_code[2] = 12222;
	ac_data_inputs[i].phase[2].movement_code[3] = 22222;

	ac_data_inputs[i].phase[3].in_use = true;
	ac_data_inputs[i].phase[3].max_green = 30;
	ac_data_inputs[i].phase[3].min_green = 10;
	ac_data_inputs[i].phase[3].passage_time = 3.0;
	ac_data_inputs[i].phase[3].yellow_change_int = 4.0;
	ac_data_inputs[i].phase[3].red_clear_int = 2.0;
	ac_data_inputs[i].phase[3].lag_code = 1;
	ac_data_inputs[i].phase[3].detector1_id[0] = 3;
	ac_data_inputs[i].phase[3].forceoff_time = 41;
	ac_data_inputs[i].phase[3].permcodes = 110;
	ac_data_inputs[i].phase[3].movement_code[0] = 11122;
	ac_data_inputs[i].phase[3].movement_code[1] = 22222;
	ac_data_inputs[i].phase[3].movement_code[2] = 22222;
	ac_data_inputs[i].phase[3].movement_code[3] = 22222;

	ac_data_inputs[i].phase[4].in_use = true;
	ac_data_inputs[i].phase[4].max_green = 14;
	ac_data_inputs[i].phase[4].min_green = 4;
	ac_data_inputs[i].phase[4].passage_time = 3.0;
	ac_data_inputs[i].phase[4].yellow_change_int = 2.0;
	ac_data_inputs[i].phase[4].red_clear_int = 3.0;
	ac_data_inputs[i].phase[4].lag_code = 0;
	ac_data_inputs[i].phase[4].forceoff_time = 52;
	ac_data_inputs[i].phase[4].permcodes = 111;
	ac_data_inputs[i].phase[4].movement_code[0] = 22222;
	ac_data_inputs[i].phase[4].movement_code[1] = 22222;
	ac_data_inputs[i].phase[4].movement_code[2] = 22222;
	ac_data_inputs[i].phase[4].movement_code[3] = 12222;

	ac_data_inputs[i].phase[5].in_use = true;
	ac_data_inputs[i].phase[5].max_green = 50;
	ac_data_inputs[i].phase[5].min_green = 10;
	ac_data_inputs[i].phase[5].passage_time = 3.0;
	ac_data_inputs[i].phase[5].yellow_change_int = 4.0;
	ac_data_inputs[i].phase[5].red_clear_int = 2.0;
	ac_data_inputs[i].phase[5].lag_code = 1;
	ac_data_inputs[i].phase[5].detector1_id[0] = 4;
	ac_data_inputs[i].phase[5].permcodes = 111;
	ac_data_inputs[i].phase[5].movement_code[0] = 22222;
	ac_data_inputs[i].phase[5].movement_code[1] = 11122;
	ac_data_inputs[i].phase[5].movement_code[2] = 22222;
	ac_data_inputs[i].phase[5].movement_code[3] = 22222;

	ac_data_inputs[i].phase[6].in_use = true;
	ac_data_inputs[i].phase[6].max_green = 15;
	ac_data_inputs[i].phase[6].min_green = 4;
	ac_data_inputs[i].phase[6].passage_time = 3.0;
	ac_data_inputs[i].phase[6].yellow_change_int = 2.0;
	ac_data_inputs[i].phase[6].red_clear_int = 3.0;
	ac_data_inputs[i].phase[6].lag_code = 0;
	ac_data_inputs[i].phase[6].forceoff_time = 11;
	ac_data_inputs[i].phase[6].permcodes = 100;
	ac_data_inputs[i].phase[6].movement_code[0] = 12222;
	ac_data_inputs[i].phase[6].movement_code[1] = 22222;
	ac_data_inputs[i].phase[6].movement_code[2] = 22222;
	ac_data_inputs[i].phase[6].movement_code[3] = 22222;

	ac_data_inputs[i].phase[7].in_use = true;
	ac_data_inputs[i].phase[7].max_green = 30;
	ac_data_inputs[i].phase[7].min_green = 10;
	ac_data_inputs[i].phase[7].passage_time = 3.0;
	ac_data_inputs[i].phase[7].yellow_change_int = 4.0;
	ac_data_inputs[i].phase[7].red_clear_int = 3.0;
	ac_data_inputs[i].phase[7].lag_code = 1;
	ac_data_inputs[i].phase[7].detector1_id[0] = 5;
	ac_data_inputs[i].phase[7].forceoff_time = 41;
	ac_data_inputs[i].phase[7].permcodes = 110;
	ac_data_inputs[i].phase[7].movement_code[0] = 22222;
	ac_data_inputs[i].phase[7].movement_code[1] = 22222;
	ac_data_inputs[i].phase[7].movement_code[2] = 11122;
	ac_data_inputs[i].phase[7].movement_code[3] = 22222;
	*/
}

void Host_Default_Network_define_fdet_inputs(DETECTOR_INPUTS* fdet_inputs)
{
	int i = 0;
	fdet_inputs[i].usn = 101;
	fdet_inputs[i].dsn = 102;
	fdet_inputs[i].location = 500;
	fdet_inputs[i].lane1 = 1;
	fdet_inputs[i].zone_length = 20.0;
}
void Host_Default_Network_define_sdet_inputs(DETECTOR_INPUTS* sdet_inputs)
{
	int i = 0;
	sdet_inputs[i].usn = 5;
	sdet_inputs[i].dsn = 2;
	sdet_inputs[i].location = 0;
	sdet_inputs[i].lane1 = 7;
	sdet_inputs[i].zone_length = 20.0;

	i++;
	sdet_inputs[i].usn = 6;
	sdet_inputs[i].dsn = 2;
	sdet_inputs[i].location = 0;
	sdet_inputs[i].lane1 = 8;
	sdet_inputs[i].lane2 = 7;
	sdet_inputs[i].zone_length = 20.0;

	i++;
	sdet_inputs[i].usn = 8;
	sdet_inputs[i].dsn = 2;
	sdet_inputs[i].location = 0;
	sdet_inputs[i].lane1 = 8;
	sdet_inputs[i].lane2 = 7;
	sdet_inputs[i].zone_length = 20.0;

	i++;
	sdet_inputs[i].usn = 5;
	sdet_inputs[i].dsn = 2;
	sdet_inputs[i].location = 0;
	sdet_inputs[i].lane1 = 9;
	sdet_inputs[i].zone_length = 20.0;

	i++;
	sdet_inputs[i].usn = 7;
	sdet_inputs[i].dsn = 2;
	sdet_inputs[i].location = 0;
	sdet_inputs[i].lane1 = 8;
	sdet_inputs[i].lane2 = 7;
	sdet_inputs[i].zone_length = 20.0;
}
void Host_Default_Network_define_busroute_inputs(BUSROUTE_DATA* busroute_inputs)
{
	int i = 0;
	busroute_inputs[i].number = 1;
	busroute_inputs[i].hdwy = 300;
	busroute_inputs[i].offset = 0;
	busroute_inputs[i].nodes = 9;

	busroute_inputs[i].route_nodes[0] = 101;
	busroute_inputs[i].route_nodes[1] = 102;
	busroute_inputs[i].route_nodes[2] = 103;
	busroute_inputs[i].route_nodes[3] = 104;
	busroute_inputs[i].route_nodes[4] = 7001;
	busroute_inputs[i].route_nodes[5] = 4;
	busroute_inputs[i].route_nodes[6] = 9;
	busroute_inputs[i].route_nodes[7] = 5;
	busroute_inputs[i].route_nodes[8] = 3;

	busroute_inputs[i].stationlist[0] = 1;
}
void Host_Default_Network_define_busstation_inputs(BUSSTATION_DATA* busstation_inputs)
{
	int station_id = 1;
	busstation_inputs[station_id - 1].block_code = 0;
	busstation_inputs[station_id - 1].usn = 9;
	busstation_inputs[station_id - 1].dsn = 5;
	busstation_inputs[station_id - 1].location = 324;
	busstation_inputs[station_id - 1].capacity = 1;
	busstation_inputs[station_id - 1].type_code = 1;
	busstation_inputs[station_id - 1].dwell = 60;
	busstation_inputs[station_id - 1].bypass_pct = 0;
	busstation_inputs[station_id - 1].next_station = 0;
}
void Host_Default_Network_define_incident_data_inputs(INCIDENT_DATA* incident_data_inputs)
{
	incident_data_inputs[0].usn = 103; //(103,104)
	incident_data_inputs[0].dsn = 104;
	incident_data_inputs[0].begin_point = 50;
	incident_data_inputs[0].begin_time = 120;
	incident_data_inputs[0].code[0] = 2; //blockage in lane 1
	incident_data_inputs[0].end_point = 800;
	incident_data_inputs[0].end_time = 240;
	incident_data_inputs[0].warn_point = 200;
}
void Host_Default_Network_define_xy_coord_inputs(NODE_LOCATION_DATA* xy_coord_inputs)
{

	int inode = 1;
	xy_coord_inputs[inode-1].x = 4687;
	xy_coord_inputs[inode-1].y = 1521;

	inode = 2;
	xy_coord_inputs[inode-1].x = 5487;
	xy_coord_inputs[inode-1].y = 1021;

	inode = 3;
	xy_coord_inputs[inode-1].x = 4687;
	xy_coord_inputs[inode-1].y = 521;

	inode = 4;
	xy_coord_inputs[inode-1].x = 3687;
	xy_coord_inputs[inode-1].y = 1021;

	inode = 5;
	xy_coord_inputs[inode-1].x = 4687;
	xy_coord_inputs[inode-1].y = 1021;

	inode = 6;
	xy_coord_inputs[inode-1].x = 6052;
	xy_coord_inputs[inode-1].y = 1021;

	inode = 7;
	xy_coord_inputs[inode-1].x = 5487;
	xy_coord_inputs[inode-1].y = 1512;

	inode = 8;
	xy_coord_inputs[inode-1].x = 5487;
	xy_coord_inputs[inode-1].y = 507;

	inode = 9;
	xy_coord_inputs[inode-1].x = 4145;
	xy_coord_inputs[inode-1].y = 1019;

	inode = 10;
	xy_coord_inputs[inode-1].x = 4139;
	xy_coord_inputs[inode-1].y = 1490;

	inode = 11;
	xy_coord_inputs[inode-1].x = 4145;
	xy_coord_inputs[inode-1].y = 504;

	inode = 12;
	xy_coord_inputs[inode-1].x = 1619;
	xy_coord_inputs[inode-1].y = 1017;

	inode = 13;
	xy_coord_inputs[inode-1].x = 1924;
	xy_coord_inputs[inode-1].y = 762;

	inode = 14;
	xy_coord_inputs[inode-1].x = 2212;
	xy_coord_inputs[inode-1].y = 551;

	inode = 101;
	xy_coord_inputs[inode-1].x = 180;
	xy_coord_inputs[inode-1].y = 1008;

	inode = 102;
	xy_coord_inputs[inode-1].x = 619;
	xy_coord_inputs[inode-1].y = 997;

	inode = 103;
	xy_coord_inputs[inode-1].x = 1619;
	xy_coord_inputs[inode-1].y = 1017;

	inode = 104;
	xy_coord_inputs[inode-1].x = 2475;
	xy_coord_inputs[inode-1].y = 1008;

	inode = 105;
	xy_coord_inputs[inode-1].x = 1924;
	xy_coord_inputs[inode-1].y = 762;

	inode = 106;
	xy_coord_inputs[inode-1].x = 2212;
	xy_coord_inputs[inode-1].y = 551;

	inode = 107;
	xy_coord_inputs[inode-1].x = 440;
	xy_coord_inputs[inode-1].y = 838;


	inode = 200;
	xy_coord_inputs[inode-1].x = 2468;
	xy_coord_inputs[inode-1].y = 1046;

	inode = 201;
	xy_coord_inputs[inode-1].x = 1633;
	xy_coord_inputs[inode-1].y = 1053;

	inode = 202;
	xy_coord_inputs[inode-1].x = 185;
	xy_coord_inputs[inode-1].y = 1050;

	inode = 7001;
	xy_coord_inputs[inode-1].x = 3086;
	xy_coord_inputs[inode-1].y = 1005;

	inode = 7002;
	xy_coord_inputs[inode-1].x = 3084;
	xy_coord_inputs[inode-1].y = 1048;

	//inode = 8001;
	//xy_coord_inputs[inode-1].x = 4687;
	//xy_coord_inputs[inode-1].y = 1621;

	//inode = 8002;
	//xy_coord_inputs[inode-1].x = 6279;
	//xy_coord_inputs[inode-1].y = 1021;

	//inode = 8003;
	//xy_coord_inputs[inode-1].x = 4687;
	//xy_coord_inputs[inode-1].y = 421;

	//inode = 8004;
	//xy_coord_inputs[inode-1].x = 0;
	//xy_coord_inputs[inode-1].y = 1055;

	//inode = 8005;
	//xy_coord_inputs[inode-1].x = 5487;
	//xy_coord_inputs[inode-1].y = 1605;

	//inode = 8006;
	//xy_coord_inputs[inode-1].x = 5487;
	//xy_coord_inputs[inode-1].y = 420;

	//inode = 8007;
	//xy_coord_inputs[inode-1].x = 4139;
	//xy_coord_inputs[inode-1].y = 1602;

	//inode = 8008;
	//xy_coord_inputs[inode-1].x = 4145;
	//xy_coord_inputs[inode-1].y = 382;

	//inode = 8009;
	//xy_coord_inputs[inode-1].x = 2411;
	//xy_coord_inputs[inode-1].y = 436;

	//inode = 8101;
	//xy_coord_inputs[inode-1].x = 0;
	//xy_coord_inputs[inode-1].y = 1008;

	//inode = 8102;
	//xy_coord_inputs[inode-1].x = 288;
	//xy_coord_inputs[inode-1].y = 704;
}
void Host_Default_Network_define_rampmeter_inputs(RM_DATA* rampmeter_inputs)
{
	int i = 0;
	rampmeter_inputs[i].dsn = 102;
	rampmeter_inputs[i].link = 7;
	rampmeter_inputs[i].control = 1;
	rampmeter_inputs[i].onset = 30;
	rampmeter_inputs[i].headway[0] = 9.0;
	rampmeter_inputs[i].twopergreen = false;
}

void Host_Default_Network_define_parking_zone_inputs(PARKING_DATA* parking_zone_inputs)
{
	int i = 0;
	parking_zone_inputs[i].usn = 5;
	parking_zone_inputs[i].usn = 1;
	parking_zone_inputs[i].right_start = 10;
	parking_zone_inputs[i].right_len = 600;
	parking_zone_inputs[i].freq = 20;
	parking_zone_inputs[i].duration = 5;
}

void Host_Default_Network_define_event_inputs(EVENT_DATA* event_inputs)
{
	int i = 0;
	event_inputs[i].begin_time = 10;
	event_inputs[i].end_time= 110;
	event_inputs[i].lane = 1;
	event_inputs[i].usn = 5;
	event_inputs[i].usn = 1;
	event_inputs[i].location = 225;
	//event_inputs[i].type = 4;
}

void Host_Default_Network_define_diversion_inputs(DIVERSION_DATA* diversion_inputs)
{
	int i = 0;
	diversion_inputs[i].begin_time = 90;
	diversion_inputs[i].end_time = 390;
	diversion_inputs[i].link = 1;
	diversion_inputs[i].location = 100;
	diversion_inputs[i].pathid = 1;
	diversion_inputs[i].percentage = 75;
	diversion_inputs[i].speed = 40;
}
