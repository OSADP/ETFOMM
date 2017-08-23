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
#include "Client_Default_Network.h"

void Client_Default_Network_define_Network_INPUTS(array<WCF_NETWORK_INPUTS> ^ wcf_ni)
{
	for (int i = 0; i < wcf_ni->Length; i++)
	{
		wcf_ni[i].run_init = true;
		wcf_ni[i].initialization_end = 180;
		wcf_ni[i].timestep = 1.0;
		wcf_ni[i].time_interval = 60.0;
		wcf_ni[i].time_period_duration = gcnew array<int>(19);
		for(int j = 0; j < 19; j++)
		{
			wcf_ni[i].time_period_duration[j] = 0;
		}
		wcf_ni[i].time_period_duration[0] = 120;
		wcf_ni[i].time_period_duration[1] = 120;
		wcf_ni[i].time_period_duration[2] = 120;
	}
}
void Client_Default_Network_define_FREEWAY_NETWORK_INPUTS(array<WCF_FREEWAY_NETWORK_INPUTS>^ wcf_fni)
{
	wcf_fni[0].cfrict = gcnew array<float>(4);
	wcf_fni[0].cfrict[0] = 0.16;
	wcf_fni[0].cfrict[1] = 0.16;
	wcf_fni[0].cfrict[2] = 0.16;
	wcf_fni[0].cfrict[3] = 0.16;
	wcf_fni[0].default_hov_pct = 1.0;
	wcf_fni[0].lag_accel = 0.3;
	wcf_fni[0].lag_decel = 0.3;
	wcf_fni[0].ffspeed_adj = gcnew array<float>(10);
	wcf_fni[0].ffspeed_adj[0] = .88;
	wcf_fni[0].ffspeed_adj[1] = .91;
	wcf_fni[0].ffspeed_adj[2] = .94;
	wcf_fni[0].ffspeed_adj[3] = .97;
	wcf_fni[0].ffspeed_adj[4] = .99;
	wcf_fni[0].ffspeed_adj[5] = 1.01;
	wcf_fni[0].ffspeed_adj[6] = 1.03;
	wcf_fni[0].ffspeed_adj[7] = 1.06;
	wcf_fni[0].ffspeed_adj[8] = 1.09;
	wcf_fni[0].ffspeed_adj[9] = 1.12;
	//wcf_fni[0].zfoll = gcnew array<float>(10);
	//wcf_fni[0].zfoll[0] = 1.25;
	//wcf_fni[0].zfoll[1] = 1.15;
	//wcf_fni[0].zfoll[2] = 1.05;
	//wcf_fni[0].zfoll[3] = 0.95;
	//wcf_fni[0].zfoll[4] = 0.85;
	//wcf_fni[0].zfoll[5] = 0.75;
	//wcf_fni[0].zfoll[6] = 0.65;
	//wcf_fni[0].zfoll[7] = 0.55;
	//wcf_fni[0].zfoll[8] = 0.45;
	//wcf_fni[0].zfoll[9] = 0.35;
	wcf_fni[0].freeway_pct_coop = 0.2;
	wcf_fni[0].lc_time = 2.0;
	wcf_fni[0].dlc_mult = 0.5;
}
void Client_Default_Network_define_STREET_NETWORK_INPUTS(array<WCF_STREET_NETWORK_INPUTS>^ wcf_sni)
{
	wcf_sni[0].dwell_multiplier = gcnew array<array<float>^>(0);

	wcf_sni[0].additional_gap = gcnew array<float>(10);
	wcf_sni[0].additional_gap[0] = 1.2;
	wcf_sni[0].additional_gap[1] = 2.1;
	wcf_sni[0].additional_gap[2] = 2.6;
	wcf_sni[0].additional_gap[3] = 3.1;
	wcf_sni[0].additional_gap[4] = 3.5;
	wcf_sni[0].additional_gap[5] = 3.9;
	wcf_sni[0].additional_gap[6] = 4.2;
	wcf_sni[0].additional_gap[7] = 4.6;
	wcf_sni[0].additional_gap[8] = 4.9;
	wcf_sni[0].additional_gap[9] = 5.1;
	wcf_sni[0].amber_decel = gcnew array<int>(10);
	wcf_sni[0].amber_decel[0] = -21;
	wcf_sni[0].amber_decel[1] = -18;
	wcf_sni[0].amber_decel[2] = -15;
	wcf_sni[0].amber_decel[3] = -12;
	wcf_sni[0].amber_decel[4] = -9;
	wcf_sni[0].amber_decel[5] = -7;
	wcf_sni[0].amber_decel[6] = -6;
	wcf_sni[0].amber_decel[7] = -5;
	wcf_sni[0].amber_decel[8] = -4;
	wcf_sni[0].amber_decel[9] = -4;
	wcf_sni[0].lt_speed = 22;
	wcf_sni[0].rt_speed = 13;
	wcf_sni[0].pdelay_weak = gcnew array<int>(10);
	wcf_sni[0].pdelay_weak[0] = 0;
	wcf_sni[0].pdelay_weak[1] = 0;
	wcf_sni[0].pdelay_weak[2] = 0;
	wcf_sni[0].pdelay_weak[3] = 0;
	wcf_sni[0].pdelay_weak[4] = 0;
	wcf_sni[0].pdelay_weak[5] = 0;
	wcf_sni[0].pdelay_weak[6] = 0;
	wcf_sni[0].pdelay_weak[7] = 1;
	wcf_sni[0].pdelay_weak[8] = 2;
	wcf_sni[0].pdelay_weak[9] = 6;
	wcf_sni[0].pdelay_strong = gcnew array<int>(10);
	wcf_sni[0].pdelay_strong[0]= 0;
	wcf_sni[0].pdelay_strong[1]= 0;
	wcf_sni[0].pdelay_strong[2]= 0;
	wcf_sni[0].pdelay_strong[3]= 1;
	wcf_sni[0].pdelay_strong[4]= 2;
	wcf_sni[0].pdelay_strong[5]= 3;
	wcf_sni[0].pdelay_strong[6]= 4;
	wcf_sni[0].pdelay_strong[7]= 5;
	wcf_sni[0].pdelay_strong[8]= 8;
	wcf_sni[0].pdelay_strong[9]= 15;
	wcf_sni[0].ped_duration = gcnew array<int>(3);
	wcf_sni[0].ped_duration[0] = 0;
	wcf_sni[0].ped_duration[1] = 10;
	wcf_sni[0].ped_duration[2] = 25;
	wcf_sni[0].acceptable_gap = gcnew array<float>(10);
	wcf_sni[0].acceptable_gap[0] = 5.6;
	wcf_sni[0].acceptable_gap[1] = 5.0;
	wcf_sni[0].acceptable_gap[2] = 4.6;
	wcf_sni[0].acceptable_gap[3] = 4.2;
	wcf_sni[0].acceptable_gap[4] = 3.9;
	wcf_sni[0].acceptable_gap[5] = 3.7;
	wcf_sni[0].acceptable_gap[6] = 3.4;
	wcf_sni[0].acceptable_gap[7] = 3.0;
	wcf_sni[0].acceptable_gap[8] = 2.6;
	wcf_sni[0].acceptable_gap[9] = 2.0;
	wcf_sni[0].acceptable_ltg = gcnew array<float>(10);
	wcf_sni[0].acceptable_ltg[0] = 7.8;  
	wcf_sni[0].acceptable_ltg[1] = 6.6;
	wcf_sni[0].acceptable_ltg[2] = 6.0;
	wcf_sni[0].acceptable_ltg[3] = 5.4;
	wcf_sni[0].acceptable_ltg[4] = 4.8;
	wcf_sni[0].acceptable_ltg[5] = 4.5;
	wcf_sni[0].acceptable_ltg[6] = 4.2;
	wcf_sni[0].acceptable_ltg[7] = 3.9;
	wcf_sni[0].acceptable_ltg[8] = 3.6;
	wcf_sni[0].acceptable_ltg[9] = 2.7;
	wcf_sni[0].acceptable_rtg = gcnew array<float>(10);
	wcf_sni[0].acceptable_rtg[0] = 10.0;
	wcf_sni[0].acceptable_rtg[1] = 8.8;
	wcf_sni[0].acceptable_rtg[2] = 8.0;
	wcf_sni[0].acceptable_rtg[3] = 7.2;
	wcf_sni[0].acceptable_rtg[4] = 6.4;
	wcf_sni[0].acceptable_rtg[5] = 6.0;
	wcf_sni[0].acceptable_rtg[6] = 5.6;
	wcf_sni[0].acceptable_rtg[7] = 5.2;
	wcf_sni[0].acceptable_rtg[8] = 4.8;
	wcf_sni[0].acceptable_rtg[9] = 3.6;
	wcf_sni[0].ffspeed_adj = gcnew array<float>(10);
	wcf_sni[0].ffspeed_adj[0] = 0.75;
	wcf_sni[0].ffspeed_adj[1] = 0.81;
	wcf_sni[0].ffspeed_adj[2] = 0.91;
	wcf_sni[0].ffspeed_adj[3] = 0.94;
	wcf_sni[0].ffspeed_adj[4] = 0.97;
	wcf_sni[0].ffspeed_adj[5] = 1.00;
	wcf_sni[0].ffspeed_adj[6] = 1.07;
	wcf_sni[0].ffspeed_adj[7] = 1.11;
	wcf_sni[0].ffspeed_adj[8] = 1.17;
	wcf_sni[0].ffspeed_adj[9] = 1.27;
	//wcf_sni[0].zfoll = gcnew array<float>(10);
	//wcf_sni[0].zfoll[0] = 1.25;
	//wcf_sni[0].zfoll[1] = 1.15;
	//wcf_sni[0].zfoll[2] = 1.05;
	//wcf_sni[0].zfoll[3] = 0.95;
	//wcf_sni[0].zfoll[4] = 0.85;
	//wcf_sni[0].zfoll[5] = 0.75;
	//wcf_sni[0].zfoll[6] = 0.65;
	//wcf_sni[0].zfoll[7] = 0.55;
	//wcf_sni[0].zfoll[8] = 0.45;
	//wcf_sni[0].zfoll[9] = 0.35;
	wcf_sni[0].lc_time = 3.0;
	wcf_sni[0].lt_jumper_prob = gcnew array<float>(7);
	wcf_sni[0].lt_jumper_prob[0] = 0.38;
	wcf_sni[0].lt_jumper_prob[1] = 0.38;
	wcf_sni[0].lt_jumper_prob[2] = 0.38;
	wcf_sni[0].lt_jumper_prob[3] = 0.38;
	wcf_sni[0].lt_jumper_prob[4] = 0.38;
	wcf_sni[0].lt_jumper_prob[5] = 0.38;
	wcf_sni[0].lt_jumper_prob[6] = 0.38;
	wcf_sni[0].lt_lagger_prob = gcnew array<float>(3);
	wcf_sni[0].lt_lagger_prob[0] = 0.5;
	wcf_sni[0].lt_lagger_prob[1] = 0.15;
	wcf_sni[0].lt_lagger_prob[2] = 0.0;
	wcf_sni[0].spillback_prob = gcnew array<float>(4);
	wcf_sni[0].spillback_prob[0] = 0.8;
	wcf_sni[0].spillback_prob[1] = 0.4;
	wcf_sni[0].spillback_prob[2] = 0.0;
	wcf_sni[0].spillback_prob[3] = 0.0;
	wcf_sni[0].stop_spd = 3.0;
	wcf_sni[0].street_pct_coop = 0.5;
	wcf_sni[0].yield_spd = 10.0;
	wcf_sni[0].driver_fampct = 0.1;
	wcf_sni[0].ste_mult = gcnew array<float>(10);
	wcf_sni[0].ste_mult[0] = 0.1;
	wcf_sni[0].ste_mult[1] = 0.2;
	wcf_sni[0].ste_mult[2] = 0.3;
	wcf_sni[0].ste_mult[3] = 0.4;
	wcf_sni[0].ste_mult[4] = 0.5;
	wcf_sni[0].ste_mult[5] = 0.7;
	wcf_sni[0].ste_mult[6] =  1.0;
	wcf_sni[0].ste_mult[7] =  1.3;
	wcf_sni[0].ste_mult[8] =  1.8;
	wcf_sni[0].ste_mult[9] =  3.7;

	System::Array::Resize(wcf_sni[0].qfactor, 5);

	System::Array::Resize(wcf_sni[0].dwell_multiplier, 10);
	for (int j = 0; j < wcf_sni[0].dwell_multiplier->Length; j++)
	{
		wcf_sni[0].dwell_multiplier[j] = gcnew array<float>(6);
	}
	wcf_sni[0].dwell_multiplier[0][0] = 0.40;
	wcf_sni[0].dwell_multiplier[1][0] = 0.60;
	wcf_sni[0].dwell_multiplier[2][0] = 0.70;
	wcf_sni[0].dwell_multiplier[3][0] = 0.80;
	wcf_sni[0].dwell_multiplier[4][0] = 0.90;
	wcf_sni[0].dwell_multiplier[5][0] = 1.00;
	wcf_sni[0].dwell_multiplier[6][0] = 1.20;
	wcf_sni[0].dwell_multiplier[7][0] = 1.30;
	wcf_sni[0].dwell_multiplier[8][0] = 1.40;
	wcf_sni[0].dwell_multiplier[9][0] = 1.70;

	wcf_sni[0].dwell_multiplier[0][1] = 0.24;
	wcf_sni[0].dwell_multiplier[1][1] = 0.48;
	wcf_sni[0].dwell_multiplier[2][1] = 0.59;
	wcf_sni[0].dwell_multiplier[3][1] = 0.75;
	wcf_sni[0].dwell_multiplier[4][1] = 0.85;
	wcf_sni[0].dwell_multiplier[5][1] = 0.94;
	wcf_sni[0].dwell_multiplier[6][1] = 1.11;
	wcf_sni[0].dwell_multiplier[7][1] = 1.26;
	wcf_sni[0].dwell_multiplier[8][1] = 1.55;
	wcf_sni[0].dwell_multiplier[9][1] = 2.23;

	wcf_sni[0].dwell_multiplier[0][2] = 0.30;
	wcf_sni[0].dwell_multiplier[1][2] = 0.47;
	wcf_sni[0].dwell_multiplier[2][2] = 0.65;
	wcf_sni[0].dwell_multiplier[3][2] = 0.77;
	wcf_sni[0].dwell_multiplier[4][2] = 0.90;
	wcf_sni[0].dwell_multiplier[5][2] = 1.03;
	wcf_sni[0].dwell_multiplier[6][2] = 1.16;
	wcf_sni[0].dwell_multiplier[7][2] = 1.37;
	wcf_sni[0].dwell_multiplier[8][2] = 1.57;
	wcf_sni[0].dwell_multiplier[9][2] = 1.78;

	wcf_sni[0].dwell_multiplier[0][3] = 0;
	wcf_sni[0].dwell_multiplier[1][3] = 0.29;
	wcf_sni[0].dwell_multiplier[2][3] = 0.59;
	wcf_sni[0].dwell_multiplier[3][3] = 0.75;
	wcf_sni[0].dwell_multiplier[4][3] = 0.92;
	wcf_sni[0].dwell_multiplier[5][3] = 1.08;
	wcf_sni[0].dwell_multiplier[6][3] = 1.25;
	wcf_sni[0].dwell_multiplier[7][3] = 1.48;
	wcf_sni[0].dwell_multiplier[8][3] = 1.70;
	wcf_sni[0].dwell_multiplier[9][3] = 1.94;

	wcf_sni[0].dwell_multiplier[0][4] = 0;
	wcf_sni[0].dwell_multiplier[1][4] = 0.18;
	wcf_sni[0].dwell_multiplier[2][4] = 0.36;
	wcf_sni[0].dwell_multiplier[3][4] = 0.70;
	wcf_sni[0].dwell_multiplier[4][4] = 1.04;
	wcf_sni[0].dwell_multiplier[5][4] = 1.25;
	wcf_sni[0].dwell_multiplier[6][4] = 1.44;
	wcf_sni[0].dwell_multiplier[7][4] = 1.56;
	wcf_sni[0].dwell_multiplier[8][4] = 1.67;
	wcf_sni[0].dwell_multiplier[9][4] = 1.80;

	wcf_sni[0].dwell_multiplier[0][5] = 0;
	wcf_sni[0].dwell_multiplier[1][5] = 0;
	wcf_sni[0].dwell_multiplier[2][5] = 0;
	wcf_sni[0].dwell_multiplier[3][5] = 0.48;
	wcf_sni[0].dwell_multiplier[4][5] = 0.96;
	wcf_sni[0].dwell_multiplier[5][5] = 1.20;
	wcf_sni[0].dwell_multiplier[6][5] = 1.44;
	wcf_sni[0].dwell_multiplier[7][5] = 1.71;
	wcf_sni[0].dwell_multiplier[8][5] = 1.98;
	wcf_sni[0].dwell_multiplier[9][5] = 2.23;
}
void Client_Default_Network_define_Vehicle_Type_Inputs(array<WCF_VEHICLE_TYPE_DATA>^ wcf_vti)
{
	int i = 0;
	wcf_vti[i].length = 14;
	wcf_vti[i].headway_factor = 1.0;
	wcf_vti[i].average_occupancy = 1.3;
	wcf_vti[i].emergency_decel = -15.0;
	wcf_vti[i].fleet_freeway_auto = 25;
	wcf_vti[i].fleet_street_auto = 25;

	i++;
	wcf_vti[i].length = 16;
	wcf_vti[i].headway_factor = 1.0;
	wcf_vti[i].average_occupancy = 1.3;
	wcf_vti[i].emergency_decel = -15.0;
	wcf_vti[i].fleet_freeway_auto = 75;
	wcf_vti[i].fleet_street_auto = 75;

	i++;
	wcf_vti[i].length = 35;
	wcf_vti[i].headway_factor = 1.2;
	wcf_vti[i].average_occupancy = 1.2;
	wcf_vti[i].emergency_decel = -15.0;
	wcf_vti[i].fleet_freeway_truck = 31;
	wcf_vti[i].fleet_street_truck = 31;

	i++;
	wcf_vti[i].length = 53;
	wcf_vti[i].headway_factor = 1.2;
	wcf_vti[i].average_occupancy = 1.2;
	wcf_vti[i].emergency_decel = -15.0;
	wcf_vti[i].fleet_freeway_truck = 36;
	wcf_vti[i].fleet_street_truck = 36;

	i++;
	wcf_vti[i].length = 53;
	wcf_vti[i].headway_factor = 1.2;
	wcf_vti[i].average_occupancy = 1.2;
	wcf_vti[i].emergency_decel = -15.0;
	wcf_vti[i].fleet_freeway_truck = 24;
	wcf_vti[i].fleet_street_truck = 24;

	i++;
	wcf_vti[i].length = 64;
	wcf_vti[i].headway_factor = 1.2;
	wcf_vti[i].average_occupancy = 1.2;
	wcf_vti[i].emergency_decel = -15.0;
	wcf_vti[i].fleet_freeway_truck = 9;
	wcf_vti[i].fleet_street_truck = 9;

	i++;
	wcf_vti[i].length = 40;
	wcf_vti[i].headway_factor = 1.2;
	wcf_vti[i].average_occupancy = 25.0;
	wcf_vti[i].emergency_decel = -15.0;
	wcf_vti[i].fleet_freeway_bus = 100;
	wcf_vti[i].fleet_street_bus = 100;

	i++;
	wcf_vti[i].length = 14;
	wcf_vti[i].headway_factor = 1.0;
	wcf_vti[i].average_occupancy = 2.5;
	wcf_vti[i].emergency_decel = -15.0;
	wcf_vti[i].fleet_freeway_carpool = 25;
	wcf_vti[i].fleet_street_carpool = 25;

	i++;
	wcf_vti[i].length = 16;
	wcf_vti[i].headway_factor = 1.0;
	wcf_vti[i].average_occupancy = 2.5;
	wcf_vti[i].emergency_decel = -15.0;
	wcf_vti[i].fleet_freeway_carpool = 75;
	wcf_vti[i].fleet_street_carpool = 75;
}

void Client_Default_Network_define_freeway_link_data(array<Wcf_freeway_link> ^ wcf_fwl)
{
	for (int il = 0; il < wcf_fwl->Length; il++)
	{
		wcf_fwl[il].id = il+1;
		wcf_fwl[il].adddrop_code = gcnew array<int>(0);
		wcf_fwl[il].adddrop_lane = gcnew array<int>(0);
		wcf_fwl[il].adddrop_dist = gcnew array<int>(0);
		wcf_fwl[il].adddrop_warn = gcnew array<int>(0);
		wcf_fwl[il].auxlaneid = gcnew array<int>(0);
		wcf_fwl[il].auxlanecode = gcnew array<int>(0);
		wcf_fwl[il].auxlanelength = gcnew array<int>(0);
		wcf_fwl[il].hov_lanes = gcnew array<int>(0);
		wcf_fwl[il].lane_width = gcnew array<float>(0);
		wcf_fwl[il].barrier = gcnew array<int>(0);
		wcf_fwl[il].exclude_type = gcnew array<array<int>^>(0);
		wcf_fwl[il].multiplier_exit = gcnew array<float>(0);
	}

	int i = 0;
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_fwl[i].usn = 101;
	wcf_fwl[i].dsn = 102;
	wcf_fwl[i].thrunode = 103;
	wcf_fwl[i].length = 439;
	wcf_fwl[i].fulllanes = 2;
	wcf_fwl[i].freeflowspeed = 65;
	wcf_fwl[i].linktype = 0;
	wcf_fwl[i].mainline_receiving_lane = 1;
	wcf_fwl[i].pavement = 1;
	wcf_fwl[i].thru_percent = 100;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_fwl[i].usn = 102;
	wcf_fwl[i].dsn = 103;
	wcf_fwl[i].thrunode = 104;
	wcf_fwl[i].exitnode = 105;
	wcf_fwl[i].length = 398;
	wcf_fwl[i].fulllanes = 2;
	System::Array::Resize(wcf_fwl[i].auxlaneid, 1);
	wcf_fwl[i].auxlaneid[0] = 9;
	System::Array::Resize(wcf_fwl[i].auxlanecode, 1);
	wcf_fwl[i].auxlanecode[0] = 3;
	System::Array::Resize(wcf_fwl[i].auxlanelength, 1);
	wcf_fwl[i].auxlanelength[0] = 398;
	wcf_fwl[i].freeflowspeed = 65;
	wcf_fwl[i].linktype = 0;
	wcf_fwl[i].mainline_receiving_lane = 1;
	wcf_fwl[i].offramp_sending_lane = 9;
	wcf_fwl[i].thru_percent = 80;
	wcf_fwl[i].offramp_warn_distance = 2500;
	System::Array::Resize(wcf_fwl[i].multiplier_exit, 16);
	for(int nt = 0; nt < 16; nt++)
	{
		wcf_fwl[i].multiplier_exit[nt] = 1.0;
	}
	wcf_fwl[i].pavement = 1;
	wcf_fwl[i].anticip_warning_distance = 1500;
	wcf_fwl[i].anticip_warning_speed = 43;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_fwl[i].usn = 103;
	wcf_fwl[i].dsn = 104;
	wcf_fwl[i].thrunode = 7001;
	wcf_fwl[i].length = 856;
	wcf_fwl[i].fulllanes = 2;
	wcf_fwl[i].freeflowspeed = 65;
	wcf_fwl[i].linktype = 0;
	wcf_fwl[i].mainline_receiving_lane = 1;
	wcf_fwl[i].pavement = 1;
	wcf_fwl[i].thru_percent = 100;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_fwl[i].usn = 103;
	wcf_fwl[i].dsn = 105;
	wcf_fwl[i].thrunode = 106;
	wcf_fwl[i].length = 398;
	wcf_fwl[i].fulllanes = 1;
	wcf_fwl[i].freeflowspeed = 45;
	wcf_fwl[i].linktype = 1;
	wcf_fwl[i].mainline_receiving_lane = 1;
	wcf_fwl[i].pavement = 1;
	wcf_fwl[i].thru_percent = 100;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_fwl[i].usn = 104;
	wcf_fwl[i].dsn = 7001;
	wcf_fwl[i].thrunode = 0;
	wcf_fwl[i].length = 611;
	wcf_fwl[i].fulllanes = 2;
	wcf_fwl[i].freeflowspeed = 65;
	wcf_fwl[i].linktype = 0;
	wcf_fwl[i].mainline_receiving_lane = 1;
	wcf_fwl[i].pavement = 1;
	wcf_fwl[i].thru_percent = 100;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_fwl[i].usn = 105;
	wcf_fwl[i].dsn = 106;
	wcf_fwl[i].thrunode = 0;
	wcf_fwl[i].length = 357;
	wcf_fwl[i].fulllanes = 1;
	wcf_fwl[i].freeflowspeed = 45;
	wcf_fwl[i].linktype = 1;
	wcf_fwl[i].mainline_receiving_lane = 1;
	wcf_fwl[i].pavement = 1;
	wcf_fwl[i].thru_percent = 100;

	//i++;
	////std::cout << std::endl << "i = " << i << std::endl;
	//wcf_fwl[i].usn = 106;
	//wcf_fwl[i].dsn = 8009;
	//wcf_fwl[i].thrunode = 0;
	//wcf_fwl[i].length = 0;
	//wcf_fwl[i].fulllanes = 1;
	//wcf_fwl[i].linktype = 1;
	//wcf_fwl[i].pavement = 1;
	//wcf_fwl[i].thru_percent = 100;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_fwl[i].usn = 107;
	wcf_fwl[i].dsn = 102;
	wcf_fwl[i].thrunode = 103;
	wcf_fwl[i].length = 239;
	wcf_fwl[i].fulllanes = 1;
	wcf_fwl[i].freeflowspeed = 65;
	wcf_fwl[i].startup_time = 1.0;
	wcf_fwl[i].linktype = 1;
	wcf_fwl[i].mainline_receiving_lane = 9;
	wcf_fwl[i].pavement = 1;
	wcf_fwl[i].thru_percent = 100;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_fwl[i].usn = 200;
	wcf_fwl[i].dsn = 201;
	wcf_fwl[i].thrunode = 202;
	wcf_fwl[i].length = 835;
	wcf_fwl[i].fulllanes = 2;
	System::Array::Resize(wcf_fwl[i].adddrop_code, 1);
	wcf_fwl[i].adddrop_code[0] = 1;
	System::Array::Resize(wcf_fwl[i].adddrop_lane, 1);
	wcf_fwl[i].adddrop_lane[0] = 3;
	System::Array::Resize(wcf_fwl[i].adddrop_dist, 1);
	wcf_fwl[i].adddrop_dist[0] = 0;
	System::Array::Resize(wcf_fwl[i].adddrop_warn, 1);
	wcf_fwl[i].freeflowspeed = 65;
	wcf_fwl[i].linktype = 0;
	wcf_fwl[i].mainline_receiving_lane = 1;
	wcf_fwl[i].nhov_lanes = 1;
	wcf_fwl[i].hov_code = 0;
	wcf_fwl[i].hov_begin = 0;
	wcf_fwl[i].hov_end = 0;
	wcf_fwl[i].hov_side = 0;
	wcf_fwl[i].hov_type = 0;
	wcf_fwl[i].hov_warn = 5280;
	wcf_fwl[i].pavement = 1;
	wcf_fwl[i].thru_percent = 100;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_fwl[i].usn = 201;
	wcf_fwl[i].dsn = 202;
	wcf_fwl[i].thrunode = 0;
	wcf_fwl[i].length = 1448;
	wcf_fwl[i].fulllanes = 3;
	wcf_fwl[i].freeflowspeed = 65;
	wcf_fwl[i].linktype = 0;
	wcf_fwl[i].mainline_receiving_lane = 1;
	wcf_fwl[i].nhov_lanes = 1;
	wcf_fwl[i].hov_code = 0;
	wcf_fwl[i].hov_begin = 0;
	wcf_fwl[i].hov_end = 0;
	wcf_fwl[i].hov_side = 0;
	wcf_fwl[i].hov_type = 0;
	wcf_fwl[i].hov_warn = 5280;
	wcf_fwl[i].pavement = 1;
	wcf_fwl[i].thru_percent = 100;

	//i++;
	////std::cout << std::endl << "i = " << i << std::endl;
	//wcf_fwl[i].usn = 202;
	//wcf_fwl[i].dsn = 8004;
	//wcf_fwl[i].thrunode = 0;
	//wcf_fwl[i].length = 0;
	//wcf_fwl[i].fulllanes = 3;
	//wcf_fwl[i].freeflowspeed = 65;
	//wcf_fwl[i].linktype = 0;
	//wcf_fwl[i].pavement = 1;
	//wcf_fwl[i].thru_percent = 100;

	i++;
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_fwl[i].usn = 7002;
	wcf_fwl[i].dsn = 200;
	wcf_fwl[i].thrunode = 201;
	wcf_fwl[i].length = 616;
	wcf_fwl[i].fulllanes = 2;
	wcf_fwl[i].freeflowspeed = 65;
	wcf_fwl[i].linktype = 0;
	wcf_fwl[i].mainline_receiving_lane = 1;
	wcf_fwl[i].pavement = 1;
	wcf_fwl[i].thru_percent = 100;

	//i++;
	////std::cout << std::endl << "i = " << i << std::endl;
	//wcf_fwl[i].usn = 8101;
	//wcf_fwl[i].dsn = 101;
	//wcf_fwl[i].thrunode = 102;
	//wcf_fwl[i].length = 0;
	//wcf_fwl[i].fulllanes = 2;
	//wcf_fwl[i].freeflowspeed = 65;
	//wcf_fwl[i].linktype = 0;
	//wcf_fwl[i].mainline_receiving_lane = 1;
	//wcf_fwl[i].pavement = 1;
	//wcf_fwl[i].thru_percent = 100;

	//i++;
	////std::cout << std::endl << "i = " << i << std::endl;
	//wcf_fwl[i].usn = 8102;
	//wcf_fwl[i].dsn = 107;
	//wcf_fwl[i].thrunode = 102;
	//wcf_fwl[i].length = 0;
	//wcf_fwl[i].fulllanes = 1;
	//wcf_fwl[i].freeflowspeed = 65;
	//wcf_fwl[i].linktype = 1;
	//wcf_fwl[i].mainline_receiving_lane = 1;
	//wcf_fwl[i].pavement = 1;
	//wcf_fwl[i].thru_percent = 100;
}
void Client_Default_Network_define_street_link_data(array<Wcf_street_link>^ wcf_sl)
{
	for (int il = 0; il < wcf_sl->Length; il++)
	{
		wcf_sl[il].id = il+1;
		wcf_sl[il].channelization = gcnew array<int>(7);
		for (int j = 0; j < 7; j++)
		{
			wcf_sl[il].channelization[j] = -1;
		}
		wcf_sl[il].lane_width = gcnew array<float>(0);
		wcf_sl[il].multiplier_left = gcnew array<float>(0);
		wcf_sl[il].multiplier_thru = gcnew array<float>(0);
		wcf_sl[il].multiplier_right = gcnew array<float>(0);
		wcf_sl[il].multiplier_diag = gcnew array<float>(0);
		wcf_sl[il].exclude_type = gcnew array<array<int>^>(0);
	}

	int i = 0;
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_sl[i].usn = 1;
	wcf_sl[i].dsn = 5;
	wcf_sl[i].leftnode = 2;
	wcf_sl[i].thrunode = 3;
	wcf_sl[i].rightnode = 9;
	wcf_sl[i].opposenode = wcf_sl[i].thrunode;
	wcf_sl[i].length = 454;
	wcf_sl[i].fulllanes = 1;
	wcf_sl[i].leftturnbays = 1;
	wcf_sl[i].rightturnbays = 1;
	//wcf_sl[i].lengthofleftbay = 80;
	//wcf_sl[i].lengthofrightbay = 50;
	wcf_sl[i].freeflowspeed = 30;
	//System::Array::Resize(wcf_sl[i].channelization, 7);
	wcf_sl[i].channelization[0] = 11;
	wcf_sl[i].channelization[5] = 4;
	wcf_sl[i].channelization[6] = 1;
	wcf_sl[i].leftpct = 25;
	wcf_sl[i].thrupct = 50;
	wcf_sl[i].rightpct = 25;
	wcf_sl[i].rtor = 1;
	System::Array::Resize(wcf_sl[i].multiplier_left, 16);
	System::Array::Resize(wcf_sl[i].multiplier_left, 16);
	System::Array::Resize(wcf_sl[i].multiplier_thru, 16);
	System::Array::Resize(wcf_sl[i].multiplier_right, 16);
	System::Array::Resize(wcf_sl[i].multiplier_diag, 16);
	for(int nt = 0; nt < 16; nt++)
	{
		wcf_sl[i].multiplier_left[nt] = 1.0;
		wcf_sl[i].multiplier_thru[nt] = 1.0;
		wcf_sl[i].multiplier_right[nt] = 1.0;
		wcf_sl[i].multiplier_diag[nt] = 1.0;
	}

	i++; //1
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_sl[i].usn = 2;
	wcf_sl[i].dsn = 5;
	wcf_sl[i].leftnode = 3;
	wcf_sl[i].thrunode = 9;
	wcf_sl[i].rightnode = 1;
	wcf_sl[i].opposenode = wcf_sl[i].thrunode;
	wcf_sl[i].length = 766;
	wcf_sl[i].fulllanes = 2;
	wcf_sl[i].leftturnbays = 1;
	wcf_sl[i].rightturnbays = 1;
	//wcf_sl[i].lengthofleftbay = 100;
	//wcf_sl[i].lengthofrightbay = 50;
	wcf_sl[i].freeflowspeed = 30;
	//System::Array::Resize(wcf_sl[i].channelization, 7);
	wcf_sl[i].channelization[0] = 11;
	wcf_sl[i].channelization[1] = 11;
	wcf_sl[i].channelization[5] = 4;
	wcf_sl[i].channelization[6] = 1;
	wcf_sl[i].leftpct = 25;
	wcf_sl[i].thrupct = 50;
	wcf_sl[i].rightpct = 25;
	wcf_sl[i].rtor = 1;
	System::Array::Resize(wcf_sl[i].multiplier_left, 16);
	System::Array::Resize(wcf_sl[i].multiplier_thru, 16);
	System::Array::Resize(wcf_sl[i].multiplier_right, 16);
	System::Array::Resize(wcf_sl[i].multiplier_diag, 16);
	for(int nt = 0; nt < 16; nt++)
	{
		wcf_sl[i].multiplier_left[nt] = 1.0;
		wcf_sl[i].multiplier_thru[nt] = 1.0;
		wcf_sl[i].multiplier_right[nt] = 1.0;
		wcf_sl[i].multiplier_diag[nt] = 1.0;
	}

	i++; //2
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_sl[i].usn = 2;
	wcf_sl[i].dsn = 6;
	wcf_sl[i].thrunode = 0;
	wcf_sl[i].length = 565;
	wcf_sl[i].fulllanes = 2;
	wcf_sl[i].freeflowspeed = 30;
	//System::Array::Resize(wcf_sl[i].channelization, 2);
	wcf_sl[i].channelization[0] = 0;
	wcf_sl[i].channelization[1] = 0;
	wcf_sl[i].thrupct = 100;
	System::Array::Resize(wcf_sl[i].multiplier_left, 16);
	System::Array::Resize(wcf_sl[i].multiplier_thru, 16);
	System::Array::Resize(wcf_sl[i].multiplier_right, 16);
	System::Array::Resize(wcf_sl[i].multiplier_diag, 16);
	for(int nt = 0; nt < 16; nt++)
	{
		wcf_sl[i].multiplier_left[nt] = 1.0;
		wcf_sl[i].multiplier_thru[nt] = 1.0;
		wcf_sl[i].multiplier_right[nt] = 1.0;
		wcf_sl[i].multiplier_diag[nt] = 1.0;
	}

	//i++; //3
	////std::cout << std::endl << "i = " << i << std::endl;
	//wcf_sl[i].usn = 6;
	//wcf_sl[i].dsn = 8002;
	////wcf_sl[i].lane1 = 1;
	////wcf_sl[i].lane2 = 1;

	i++; //4
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_sl[i].usn = 2;
	wcf_sl[i].dsn = 7;
	wcf_sl[i].thrunode = 0;
	wcf_sl[i].length = 491;
	wcf_sl[i].fulllanes = 2;
	wcf_sl[i].freeflowspeed = 30;
	//System::Array::Resize(wcf_sl[i].channelization, 2);
	wcf_sl[i].channelization[0] = 0;
	wcf_sl[i].channelization[1] = 0;
	wcf_sl[i].thrupct = 100;
	System::Array::Resize(wcf_sl[i].multiplier_left, 16);
	System::Array::Resize(wcf_sl[i].multiplier_thru, 16);
	System::Array::Resize(wcf_sl[i].multiplier_right, 16);
	System::Array::Resize(wcf_sl[i].multiplier_diag, 16);
	for(int nt = 0; nt < 16; nt++)
	{
		wcf_sl[i].multiplier_left[nt] = 1.0;
		wcf_sl[i].multiplier_thru[nt] = 1.0;
		wcf_sl[i].multiplier_right[nt] = 1.0;
		wcf_sl[i].multiplier_diag[nt] = 1.0;
	}
	//i++; //5
	////std::cout << std::endl << "i = " << i << std::endl;
	//wcf_sl[i].usn = 7;
	//wcf_sl[i].dsn = 8005;
	////wcf_sl[i].lane1 = 1;
	////wcf_sl[i].lane2 = 1;

	i++; //6
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_sl[i].usn = 2;
	wcf_sl[i].dsn = 8;
	wcf_sl[i].thrunode = 0;
	wcf_sl[i].length = 514;
	wcf_sl[i].fulllanes = 2;
	wcf_sl[i].freeflowspeed = 30;
	//System::Array::Resize(wcf_sl[i].channelization, 2);
	wcf_sl[i].channelization[0] = 0;
	wcf_sl[i].channelization[1] = 0;
	wcf_sl[i].thrupct = 100;
	System::Array::Resize(wcf_sl[i].multiplier_left, 16);
	System::Array::Resize(wcf_sl[i].multiplier_thru, 16);
	System::Array::Resize(wcf_sl[i].multiplier_right, 16);
	System::Array::Resize(wcf_sl[i].multiplier_diag, 16);
	for(int nt = 0; nt < 16; nt++)
	{
		wcf_sl[i].multiplier_left[nt] = 1.0;
		wcf_sl[i].multiplier_thru[nt] = 1.0;
		wcf_sl[i].multiplier_right[nt] = 1.0;
		wcf_sl[i].multiplier_diag[nt] = 1.0;
	}
	//i++; //7
	////std::cout << std::endl << "i = " << i << std::endl;
	//wcf_sl[i].usn = 8;
	//wcf_sl[i].dsn = 8006;
	////wcf_sl[i].lane1 = 1;
	////wcf_sl[i].lane2 = 1;

	i++; //8
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_sl[i].usn = 3;
	wcf_sl[i].dsn = 5;
	wcf_sl[i].leftnode = 9;
	wcf_sl[i].thrunode = 1;
	wcf_sl[i].rightnode = 2;
	wcf_sl[i].opposenode = wcf_sl[i].thrunode;
	wcf_sl[i].length = 454;
	wcf_sl[i].fulllanes = 1;
	wcf_sl[i].leftturnbays = 1;
	wcf_sl[i].rightturnbays = 1;
	//wcf_sl[i].lengthofleftbay = 50;
	//wcf_sl[i].lengthofrightbay = 50;
	wcf_sl[i].freeflowspeed = 30;
	//System::Array::Resize(wcf_sl[i].channelization, 7);
	wcf_sl[i].channelization[0] = 11;
	wcf_sl[i].channelization[5] = 4;
	wcf_sl[i].channelization[6] = 1;
	wcf_sl[i].leftpct = 25;
	wcf_sl[i].thrupct = 50;
	wcf_sl[i].rightpct = 25;
	wcf_sl[i].rtor = 1;
	System::Array::Resize(wcf_sl[i].multiplier_left, 16);
	System::Array::Resize(wcf_sl[i].multiplier_thru, 16);
	System::Array::Resize(wcf_sl[i].multiplier_right, 16);
	System::Array::Resize(wcf_sl[i].multiplier_diag, 16);
	for(int nt = 0; nt < 16; nt++)
	{
		wcf_sl[i].multiplier_left[nt] = 1.0;
		wcf_sl[i].multiplier_thru[nt] = 1.0;
		wcf_sl[i].multiplier_right[nt] = 1.0;
		wcf_sl[i].multiplier_diag[nt] = 1.0;
	}

	i++; //9
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_sl[i].usn = 4;
	wcf_sl[i].dsn = 9;
	wcf_sl[i].leftnode = 10;
	wcf_sl[i].thrunode = 5;
	wcf_sl[i].rightnode = 11;
	wcf_sl[i].opposenode = wcf_sl[i].thrunode;
	wcf_sl[i].length = 442;
	wcf_sl[i].fulllanes = 2;
	wcf_sl[i].freeflowspeed = 30;
	//System::Array::Resize(wcf_sl[i].channelization, 2);
	wcf_sl[i].channelization[0] = 0;
	wcf_sl[i].channelization[1] = 0;
	wcf_sl[i].leftpct = 33;
	wcf_sl[i].thrupct = 34;
	wcf_sl[i].rightpct = 33;
	System::Array::Resize(wcf_sl[i].multiplier_left, 16);
	System::Array::Resize(wcf_sl[i].multiplier_thru, 16);
	System::Array::Resize(wcf_sl[i].multiplier_right, 16);
	System::Array::Resize(wcf_sl[i].multiplier_diag, 16);
	for(int nt = 0; nt < 16; nt++)
	{
		wcf_sl[i].multiplier_left[nt] = 1.0;
		wcf_sl[i].multiplier_thru[nt] = 1.0;
		wcf_sl[i].multiplier_right[nt] = 1.0;
		wcf_sl[i].multiplier_diag[nt] = 1.0;
	}

	i++; //10
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_sl[i].usn = 4;
	wcf_sl[i].dsn = 7002;
	wcf_sl[i].thrunode = 0;
	wcf_sl[i].length = 604;
	wcf_sl[i].fulllanes = 2;
	wcf_sl[i].freeflowspeed = 45;
	//System::Array::Resize(wcf_sl[i].channelization, 2);
	wcf_sl[i].channelization[0] = 0;
	wcf_sl[i].channelization[1] = 0;
	wcf_sl[i].thrupct = 100;

	i++; //11
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_sl[i].usn = 5;
	wcf_sl[i].dsn = 1;
	wcf_sl[i].thrunode = 0;
	wcf_sl[i].length = 545;
	wcf_sl[i].fulllanes = 2;
	wcf_sl[i].freeflowspeed = 30;
	//System::Array::Resize(wcf_sl[i].channelization, 2);
	wcf_sl[i].channelization[0] = 0;
	wcf_sl[i].channelization[1] = 0;
	wcf_sl[i].thrupct = 100;
	System::Array::Resize(wcf_sl[i].multiplier_left, 16);
	System::Array::Resize(wcf_sl[i].multiplier_thru, 16);
	System::Array::Resize(wcf_sl[i].multiplier_right, 16);
	System::Array::Resize(wcf_sl[i].multiplier_diag, 16);
	for(int nt = 0; nt < 16; nt++)
	{
		wcf_sl[i].multiplier_left[nt] = 1.0;
		wcf_sl[i].multiplier_thru[nt] = 1.0;
		wcf_sl[i].multiplier_right[nt] = 1.0;
		wcf_sl[i].multiplier_diag[nt] = 1.0;
	}
	//i++; //12
	////std::cout << std::endl << "i = " << i << std::endl;
	//wcf_sl[i].usn = 1;
	//wcf_sl[i].dsn = 8001;
	////wcf_sl[i].lane1 = 1;
	////wcf_sl[i].lane2 = 1;

	i++; //13
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_sl[i].usn = 5;
	wcf_sl[i].dsn = 2;
	wcf_sl[i].leftnode = 7;
	wcf_sl[i].thrunode = 6;
	wcf_sl[i].rightnode = 8;
	wcf_sl[i].length = 833;
	wcf_sl[i].fulllanes = 2;
	wcf_sl[i].leftturnbays = 1;
	wcf_sl[i].rightturnbays = 1;
	//wcf_sl[i].lengthofleftbay = 100;
	//wcf_sl[i].lengthofrightbay = 50;
	wcf_sl[i].freeflowspeed = 30;
	wcf_sl[i].ped_code = 2;
	//System::Array::Resize(wcf_sl[i].channelization, 7);
	wcf_sl[i].channelization[0] = 0;
	wcf_sl[i].channelization[1] = 0;
	wcf_sl[i].channelization[5] = 4;
	wcf_sl[i].channelization[6] = 1;
	wcf_sl[i].leftpct = 1;
	wcf_sl[i].thrupct = 98;
	wcf_sl[i].rightpct = 1;
	System::Array::Resize(wcf_sl[i].multiplier_left, 16);
	System::Array::Resize(wcf_sl[i].multiplier_thru, 16);
	System::Array::Resize(wcf_sl[i].multiplier_right, 16);
	System::Array::Resize(wcf_sl[i].multiplier_diag, 16);
	for(int nt = 0; nt < 16; nt++)
	{
		wcf_sl[i].multiplier_left[nt] = 1.0;
		wcf_sl[i].multiplier_thru[nt] = 1.0;
		wcf_sl[i].multiplier_right[nt] = 1.0;
		wcf_sl[i].multiplier_diag[nt] = 1.0;
	}

	i++; //14
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_sl[i].usn = 5;
	wcf_sl[i].dsn = 3;
	wcf_sl[i].thrunode = 0;
	wcf_sl[i].length = 545;
	wcf_sl[i].fulllanes = 2;
	wcf_sl[i].freeflowspeed = 30;
	//System::Array::Resize(wcf_sl[i].channelization, 2);
	wcf_sl[i].channelization[0] = 0;
	wcf_sl[i].channelization[1] = 0;
	wcf_sl[i].thrupct = 100;
	System::Array::Resize(wcf_sl[i].multiplier_left, 16);
	System::Array::Resize(wcf_sl[i].multiplier_thru, 16);
	System::Array::Resize(wcf_sl[i].multiplier_right, 16);
	System::Array::Resize(wcf_sl[i].multiplier_diag, 16);
	for(int nt = 0; nt < 16; nt++)
	{
		wcf_sl[i].multiplier_left[nt] = 1.0;
		wcf_sl[i].multiplier_thru[nt] = 1.0;
		wcf_sl[i].multiplier_right[nt] = 1.0;
		wcf_sl[i].multiplier_diag[nt] = 1.0;
	}

	//i++; //15
	////std::cout << std::endl << "i = " << i << std::endl;
	//wcf_sl[i].usn = 3;
	//wcf_sl[i].dsn = 8003;
	////wcf_sl[i].lane1 = 1;
	////wcf_sl[i].lane2 = 1;

	i++; //16
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_sl[i].usn = 5;
	wcf_sl[i].dsn = 9;
	wcf_sl[i].leftnode = 11;
	wcf_sl[i].thrunode = 4;
	wcf_sl[i].rightnode = 10;
	wcf_sl[i].opposenode = wcf_sl[i].thrunode;
	wcf_sl[i].length = 559;
	wcf_sl[i].fulllanes = 2;
	wcf_sl[i].freeflowspeed = 30;
	//System::Array::Resize(wcf_sl[i].channelization, 2);
	wcf_sl[i].channelization[0] = 0;
	wcf_sl[i].channelization[1] = 0;
	wcf_sl[i].leftpct = 25;
	wcf_sl[i].thrupct = 50;
	wcf_sl[i].rightpct = 25;
	System::Array::Resize(wcf_sl[i].multiplier_left, 16);
	System::Array::Resize(wcf_sl[i].multiplier_thru, 16);
	System::Array::Resize(wcf_sl[i].multiplier_right, 16);
	System::Array::Resize(wcf_sl[i].multiplier_diag, 16);
	for(int nt = 0; nt < 16; nt++)
	{
		wcf_sl[i].multiplier_left[nt] = 1.0;
		wcf_sl[i].multiplier_thru[nt] = 1.0;
		wcf_sl[i].multiplier_right[nt] = 1.0;
		wcf_sl[i].multiplier_diag[nt] = 1.0;
	}

	i++; //17
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_sl[i].usn = 6;
	wcf_sl[i].dsn = 2;
	wcf_sl[i].leftnode = 8;
	wcf_sl[i].thrunode = 5;
	wcf_sl[i].rightnode = 7;
	//wcf_sl[i].opposenode = wcf_sl[i].thrunode;
	wcf_sl[i].length = 565;
	wcf_sl[i].fulllanes = 2;
	wcf_sl[i].leftturnbays = 1;
	wcf_sl[i].rightturnbays = 1;
	//wcf_sl[i].lengthofleftbay = 100;
	//wcf_sl[i].lengthofrightbay = 50;
	wcf_sl[i].freeflowspeed = 30;
	wcf_sl[i].ped_code = 2;
	//System::Array::Resize(wcf_sl[i].channelization, 7);
	wcf_sl[i].channelization[0] = 0;
	wcf_sl[i].channelization[1] = 0;
	wcf_sl[i].channelization[5] = 4;
	wcf_sl[i].channelization[6] = 1;
	wcf_sl[i].leftpct = 33;
	wcf_sl[i].thrupct = 34;
	wcf_sl[i].rightpct = 33;
	System::Array::Resize(wcf_sl[i].multiplier_left, 16);
	System::Array::Resize(wcf_sl[i].multiplier_thru, 16);
	System::Array::Resize(wcf_sl[i].multiplier_right, 16);
	System::Array::Resize(wcf_sl[i].multiplier_diag, 16);
	for(int nt = 0; nt < 16; nt++)
	{
		wcf_sl[i].multiplier_left[nt] = 1.0;
		wcf_sl[i].multiplier_thru[nt] = 1.0;
		wcf_sl[i].multiplier_right[nt] = 1.0;
		wcf_sl[i].multiplier_diag[nt] = 1.0;
	}

	i++; //18
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_sl[i].usn = 7;
	wcf_sl[i].dsn = 2;
	wcf_sl[i].leftnode = 6;
	wcf_sl[i].thrunode = 8;
	wcf_sl[i].rightnode = 5;
	wcf_sl[i].opposenode = wcf_sl[i].thrunode;
	wcf_sl[i].length = 491;
	wcf_sl[i].fulllanes = 3;
	wcf_sl[i].leftturnbays = 1;
	wcf_sl[i].rightturnbays = 1;
	//wcf_sl[i].lengthofleftbay = 100;
	//wcf_sl[i].lengthofrightbay = 50;
	wcf_sl[i].freeflowspeed = 30;
	wcf_sl[i].ped_code = 2;
	//System::Array::Resize(wcf_sl[i].channelization, 7);
	wcf_sl[i].channelization[0] = 0;
	wcf_sl[i].channelization[1] = 0;
	wcf_sl[i].channelization[2] = 0;
	wcf_sl[i].channelization[5] = 4;
	wcf_sl[i].channelization[6] = 1;
	wcf_sl[i].leftpct = 33;
	wcf_sl[i].thrupct = 34;
	wcf_sl[i].rightpct = 33;
	System::Array::Resize(wcf_sl[i].multiplier_left, 16);
	System::Array::Resize(wcf_sl[i].multiplier_thru, 16);
	System::Array::Resize(wcf_sl[i].multiplier_right, 16);
	System::Array::Resize(wcf_sl[i].multiplier_diag, 16);
	for(int nt = 0; nt < 16; nt++)
	{
		wcf_sl[i].multiplier_left[nt] = 1.0;
		wcf_sl[i].multiplier_thru[nt] = 1.0;
		wcf_sl[i].multiplier_right[nt] = 1.0;
		wcf_sl[i].multiplier_diag[nt] = 1.0;
	}

	i++; //19
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_sl[i].usn = 8;
	wcf_sl[i].dsn = 2;
	wcf_sl[i].leftnode = 5;
	wcf_sl[i].thrunode = 7;
	wcf_sl[i].rightnode = 6;
	wcf_sl[i].opposenode = wcf_sl[i].thrunode;
	wcf_sl[i].length = 514;
	wcf_sl[i].fulllanes = 2;
	wcf_sl[i].leftturnbays = 1;
	wcf_sl[i].rightturnbays = 1;
	//wcf_sl[i].lengthofleftbay = 100;
	//wcf_sl[i].lengthofrightbay = 50;
	wcf_sl[i].freeflowspeed = 30;
	wcf_sl[i].ped_code = 2;
	//System::Array::Resize(wcf_sl[i].channelization, 7);
	wcf_sl[i].channelization[0] = 0;
	wcf_sl[i].channelization[1] = 0;
	wcf_sl[i].channelization[5] = 4;
	wcf_sl[i].channelization[6] = 1;
	wcf_sl[i].leftpct = 33;
	wcf_sl[i].thrupct = 34;
	wcf_sl[i].rightpct = 33;
	System::Array::Resize(wcf_sl[i].multiplier_left, 16);
	System::Array::Resize(wcf_sl[i].multiplier_thru, 16);
	System::Array::Resize(wcf_sl[i].multiplier_right, 16);
	System::Array::Resize(wcf_sl[i].multiplier_diag, 16);
	for(int nt = 0; nt < 16; nt++)
	{
		wcf_sl[i].multiplier_left[nt] = 1.0;
		wcf_sl[i].multiplier_thru[nt] = 1.0;
		wcf_sl[i].multiplier_right[nt] = 1.0;
		wcf_sl[i].multiplier_diag[nt] = 1.0;
	}

	i++; //20
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_sl[i].usn = 9;
	wcf_sl[i].dsn = 4;
	wcf_sl[i].thrunode = 7002;
	wcf_sl[i].length = 473;
	wcf_sl[i].fulllanes = 2;
	wcf_sl[i].freeflowspeed = 30;
	//System::Array::Resize(wcf_sl[i].channelization, 2);
	wcf_sl[i].channelization[0] = 0;
	wcf_sl[i].channelization[1] = 0;
	wcf_sl[i].thrupct = 100;
	System::Array::Resize(wcf_sl[i].multiplier_left, 16);
	System::Array::Resize(wcf_sl[i].multiplier_thru, 16);
	System::Array::Resize(wcf_sl[i].multiplier_right, 16);
	System::Array::Resize(wcf_sl[i].multiplier_diag, 16);
	for(int nt = 0; nt < 16; nt++)
	{
		wcf_sl[i].multiplier_left[nt] = 1.0;
		wcf_sl[i].multiplier_thru[nt] = 1.0;
		wcf_sl[i].multiplier_right[nt] = 1.0;
		wcf_sl[i].multiplier_diag[nt] = 1.0;
	}

	i++; //21
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_sl[i].usn = 9;
	wcf_sl[i].dsn = 5;
	wcf_sl[i].leftnode = 1;
	wcf_sl[i].thrunode = 2;
	wcf_sl[i].rightnode = 3;
	wcf_sl[i].opposenode = wcf_sl[i].thrunode;
	wcf_sl[i].length = 524;
	wcf_sl[i].fulllanes = 2;
	wcf_sl[i].leftturnbays = 1;
	wcf_sl[i].rightturnbays = 1;
	//wcf_sl[i].lengthofleftbay = 100;
	//wcf_sl[i].lengthofrightbay = 50;
	wcf_sl[i].freeflowspeed = 30;
	//System::Array::Resize(wcf_sl[i].channelization, 7);
	wcf_sl[i].channelization[0] = 11;
	wcf_sl[i].channelization[1] = 11;
	wcf_sl[i].channelization[5] = 4;
	wcf_sl[i].channelization[6] = 1;
	wcf_sl[i].leftpct = 25;
	wcf_sl[i].thrupct = 50;
	wcf_sl[i].rightpct = 25;
	wcf_sl[i].rtor = 1;
	System::Array::Resize(wcf_sl[i].multiplier_left, 16);
	System::Array::Resize(wcf_sl[i].multiplier_thru, 16);
	System::Array::Resize(wcf_sl[i].multiplier_right, 16);
	System::Array::Resize(wcf_sl[i].multiplier_diag, 16);
	for(int nt = 0; nt < 16; nt++)
	{
		wcf_sl[i].multiplier_left[nt] = 1.0;
		wcf_sl[i].multiplier_thru[nt] = 1.0;
		wcf_sl[i].multiplier_right[nt] = 1.0;
		wcf_sl[i].multiplier_diag[nt] = 1.0;
	}

	i++; //22
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_sl[i].usn = 9;
	wcf_sl[i].dsn = 10;
	wcf_sl[i].thrunode = 0;
	wcf_sl[i].length = 471;
	wcf_sl[i].fulllanes = 2;
	wcf_sl[i].freeflowspeed = 30;
	//System::Array::Resize(wcf_sl[i].channelization, 2);
	wcf_sl[i].channelization[0] = 0;
	wcf_sl[i].channelization[1] = 0;
	wcf_sl[i].thrupct = 100;
	System::Array::Resize(wcf_sl[i].multiplier_left, 16);
	System::Array::Resize(wcf_sl[i].multiplier_thru, 16);
	System::Array::Resize(wcf_sl[i].multiplier_right, 16);
	System::Array::Resize(wcf_sl[i].multiplier_diag, 16);
	for(int nt = 0; nt < 16; nt++)
	{
		wcf_sl[i].multiplier_left[nt] = 1.0;
		wcf_sl[i].multiplier_thru[nt] = 1.0;
		wcf_sl[i].multiplier_right[nt] = 1.0;
		wcf_sl[i].multiplier_diag[nt] = 1.0;
	}

	//i++; //23
	////std::cout << std::endl << "i = " << i << std::endl;
	//wcf_sl[i].usn = 10;
	//wcf_sl[i].dsn = 8007;
	////wcf_sl[i].lane1 = 1;
	////wcf_sl[i].lane2 = 1;

	i++; //24
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_sl[i].usn = 9;
	wcf_sl[i].dsn = 11;
	wcf_sl[i].thrunode = 0;
	wcf_sl[i].length = 515;
	wcf_sl[i].fulllanes = 2;
	wcf_sl[i].freeflowspeed = 30;
	//System::Array::Resize(wcf_sl[i].channelization, 2);
	wcf_sl[i].channelization[0] = 0;
	wcf_sl[i].channelization[1] = 0;
	wcf_sl[i].thrupct = 100;
	System::Array::Resize(wcf_sl[i].multiplier_left, 16);
	System::Array::Resize(wcf_sl[i].multiplier_thru, 16);
	System::Array::Resize(wcf_sl[i].multiplier_right, 16);
	System::Array::Resize(wcf_sl[i].multiplier_diag, 16);
	for(int nt = 0; nt < 16; nt++)
	{
		wcf_sl[i].multiplier_left[nt] = 1.0;
		wcf_sl[i].multiplier_thru[nt] = 1.0;
		wcf_sl[i].multiplier_right[nt] = 1.0;
		wcf_sl[i].multiplier_diag[nt] = 1.0;
	}

	//i++; //25
	////std::cout << std::endl << "i = " << i << std::endl;
	//wcf_sl[i].usn = 11;
	//wcf_sl[i].dsn = 8008;
	////wcf_sl[i].lane1 = 1;
	////wcf_sl[i].lane2 = 1;

	i++; //26
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_sl[i].usn = 10;
	wcf_sl[i].dsn = 9;
	wcf_sl[i].leftnode = 5;
	wcf_sl[i].thrunode = 11;
	wcf_sl[i].rightnode = 4;
	wcf_sl[i].opposenode = wcf_sl[i].thrunode;
	wcf_sl[i].length = 471;
	wcf_sl[i].fulllanes = 2;
	wcf_sl[i].freeflowspeed = 30;
	//System::Array::Resize(wcf_sl[i].channelization, 2);
	wcf_sl[i].channelization[0] = 0;
	wcf_sl[i].channelization[1] = 0;
	wcf_sl[i].leftpct = 33;
	wcf_sl[i].thrupct = 34;
	wcf_sl[i].rightpct = 33;
	System::Array::Resize(wcf_sl[i].multiplier_left, 16);
	System::Array::Resize(wcf_sl[i].multiplier_thru, 16);
	System::Array::Resize(wcf_sl[i].multiplier_right, 16);
	System::Array::Resize(wcf_sl[i].multiplier_diag, 16);
	for(int nt = 0; nt < 16; nt++)
	{
		wcf_sl[i].multiplier_left[nt] = 1.0;
		wcf_sl[i].multiplier_thru[nt] = 1.0;
		wcf_sl[i].multiplier_right[nt] = 1.0;
		wcf_sl[i].multiplier_diag[nt] = 1.0;
	}

	i++; //27
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_sl[i].usn = 11;
	wcf_sl[i].dsn = 9;
	wcf_sl[i].leftnode = 4;
	wcf_sl[i].thrunode = 10;
	wcf_sl[i].rightnode = 5;
	wcf_sl[i].opposenode = wcf_sl[i].thrunode;
	wcf_sl[i].length = 515;
	wcf_sl[i].fulllanes = 2;
	wcf_sl[i].freeflowspeed = 30;
	//System::Array::Resize(wcf_sl[i].channelization, 2);
	wcf_sl[i].channelization[0] = 0;
	wcf_sl[i].channelization[1] = 0;
	wcf_sl[i].leftpct = 33;
	wcf_sl[i].thrupct = 34;
	wcf_sl[i].rightpct = 33;
	System::Array::Resize(wcf_sl[i].multiplier_left, 16);
	System::Array::Resize(wcf_sl[i].multiplier_thru, 16);
	System::Array::Resize(wcf_sl[i].multiplier_right, 16);
	System::Array::Resize(wcf_sl[i].multiplier_diag, 16);
	for(int nt = 0; nt < 16; nt++)
	{
		wcf_sl[i].multiplier_left[nt] = 1.0;
		wcf_sl[i].multiplier_thru[nt] = 1.0;
		wcf_sl[i].multiplier_right[nt] = 1.0;
		wcf_sl[i].multiplier_diag[nt] = 1.0;
	}

	i++; //28
	//std::cout << std::endl << "i = " << i << std::endl;
	wcf_sl[i].usn = 7001;
	wcf_sl[i].dsn = 4;
	wcf_sl[i].thrunode = 9;
	wcf_sl[i].length = 601;
	wcf_sl[i].fulllanes = 2;
	wcf_sl[i].freeflowspeed = 45;
	//System::Array::Resize(wcf_sl[i].channelization, 2);
	wcf_sl[i].channelization[0] = 0;
	wcf_sl[i].channelization[1] = 0;
	wcf_sl[i].thrupct = 100;

	//i++; //29
	////std::cout << std::endl << "i = " << i << std::endl;
	//wcf_sl[i].usn = 8001;
	//wcf_sl[i].dsn = 1;
	//wcf_sl[i].thrunode = 5;
	//wcf_sl[i].length = 0;
	//wcf_sl[i].fulllanes = 1;
	//wcf_sl[i].freeflowspeed = 30;
	////System::Array::Resize(wcf_sl[i].channelization, 1);
	//wcf_sl[i].channelization[0] = 0;
	//wcf_sl[i].thrupct = 100;

	//i++; //30
	////std::cout << std::endl << "i = " << i << std::endl;
	//wcf_sl[i].usn = 8002;
	//wcf_sl[i].dsn = 6;
	//wcf_sl[i].thrunode = 2;
	//wcf_sl[i].length = 0;
	//wcf_sl[i].fulllanes = 2;
	//wcf_sl[i].freeflowspeed = 30;
	////System::Array::Resize(wcf_sl[i].channelization, 2);
	//wcf_sl[i].channelization[0] = 0;
	//wcf_sl[i].channelization[1] = 0;
	//wcf_sl[i].thrupct = 100;

	//i++; //31
	////std::cout << std::endl << "i = " << i << std::endl;
	//wcf_sl[i].usn = 8003;
	//wcf_sl[i].dsn = 3;
	//wcf_sl[i].thrunode = 5;
	//wcf_sl[i].length = 0;
	//wcf_sl[i].fulllanes = 1;
	//wcf_sl[i].freeflowspeed = 30;
	////System::Array::Resize(wcf_sl[i].channelization, 2);
	//wcf_sl[i].channelization[0] = 0;
	//wcf_sl[i].channelization[1] = 0;
	//wcf_sl[i].thrupct = 100;

	//i++; //32
	////std::cout << std::endl << "i = " << i << std::endl;
	//wcf_sl[i].usn = 8005;
	//wcf_sl[i].dsn = 7;
	//wcf_sl[i].thrunode = 2;
	//wcf_sl[i].length = 0;
	//wcf_sl[i].fulllanes = 2;
	//wcf_sl[i].freeflowspeed = 30;
	////System::Array::Resize(wcf_sl[i].channelization, 2);
	//wcf_sl[i].channelization[0] = 0;
	//wcf_sl[i].channelization[1] = 0;
	//wcf_sl[i].thrupct = 100;

	//i++; //33
	////std::cout << std::endl << "i = " << i << std::endl;
	//wcf_sl[i].usn = 8006;
	//wcf_sl[i].dsn = 8;
	//wcf_sl[i].thrunode = 2;
	//wcf_sl[i].length = 0;
	//wcf_sl[i].fulllanes = 2;
	//wcf_sl[i].freeflowspeed = 30;
	////System::Array::Resize(wcf_sl[i].channelization, 2);
	//wcf_sl[i].channelization[0] = 0;
	//wcf_sl[i].channelization[1] = 0;
	//wcf_sl[i].thrupct = 100;

	//i++; //34
	////std::cout << std::endl << "i = " << i << std::endl;
	//wcf_sl[i].usn = 8007;
	//wcf_sl[i].dsn = 10;
	//wcf_sl[i].thrunode = 9;
	//wcf_sl[i].length = 0;
	//wcf_sl[i].fulllanes = 2;
	//wcf_sl[i].freeflowspeed = 30;
	////System::Array::Resize(wcf_sl[i].channelization, 2);
	//wcf_sl[i].channelization[0] = 0;
	//wcf_sl[i].channelization[1] = 0;
	//wcf_sl[i].thrupct = 100;

	//i++; //35
	////std::cout << std::endl << "i = " << i << std::endl;
	//wcf_sl[i].usn = 8008;
	//wcf_sl[i].dsn = 11;
	//wcf_sl[i].thrunode = 9;
	//wcf_sl[i].length = 0;
	//wcf_sl[i].fulllanes = 2;
	//wcf_sl[i].freeflowspeed = 30;
	////System::Array::Resize(wcf_sl[i].channelization, 2);
	//wcf_sl[i].channelization[0] = 0;
	//wcf_sl[i].channelization[1] = 0;
	//wcf_sl[i].thrupct = 100;

}
void Client_Default_Network_define_cond_turnpct_data(array<WCF_COND_TURNPCTS>^ wcf_ct)
{
	for (int i = 0; i < wcf_ct->Length; i++)
	{
		wcf_ct[i].LEFTPCT = gcnew array<int>(0);
		wcf_ct[i].THRUPCT = gcnew array<int>(0);
		wcf_ct[i].RIGHTPCT = gcnew array<int>(0);
		wcf_ct[i].DIAGPCT = gcnew array<int>(0);
	}
	wcf_ct[0].USN = 5;
	wcf_ct[0].DSN = 2;

	System::Array::Resize(wcf_ct[0].LEFTPCT, 4);
	wcf_ct[0].LEFTPCT[0] = 0;
	wcf_ct[0].LEFTPCT[1] = 0;
	wcf_ct[0].LEFTPCT[2] = 100;
	wcf_ct[0].LEFTPCT[3] = 0;

	System::Array::Resize(wcf_ct[0].THRUPCT, 4);
	wcf_ct[0].THRUPCT[0] = 0;
	wcf_ct[0].THRUPCT[1] = 100;
	wcf_ct[0].THRUPCT[2] = 0;
	wcf_ct[0].THRUPCT[3] = 0;

	System::Array::Resize(wcf_ct[0].RIGHTPCT, 4);
	wcf_ct[0].RIGHTPCT[0] = 100;
	wcf_ct[0].RIGHTPCT[1] = 0;
	wcf_ct[0].RIGHTPCT[2] = 0;
	wcf_ct[0].RIGHTPCT[3] = 0;

	System::Array::Resize(wcf_ct[0].DIAGPCT, 4);
	wcf_ct[0].DIAGPCT[0] = 0;
	wcf_ct[0].DIAGPCT[1] = 0;
	wcf_ct[0].DIAGPCT[2] = 0;
	wcf_ct[0].DIAGPCT[3] = 0;
}
void Client_Default_Network_define_entrynode_inputs(array<WCF_ENTRYNODES_DATA> ^ wcf_entry_node)
{
	for (int i = 0; i < wcf_entry_node->Length; i++)
	{
		wcf_entry_node[i].lane_pct = gcnew array<int>(0);
	}
	int i = 0;
	wcf_entry_node[i].Node_ID = 1;
	wcf_entry_node[i].carpool_pct = .05;
	wcf_entry_node[i].flowrate = 125;
	wcf_entry_node[i].truck_pct = .10;
	wcf_entry_node[i].hov_violators_per10000 = 100;
	//System::Array::Resize(wcf_entry_node[i].lane_pct, 2);
	//wcf_entry_node[i].lane_pct[0] = 0.5;
	//wcf_entry_node[i].lane_pct[1] = 0.5;

	i++;
	wcf_entry_node[i].Node_ID = 6;
	wcf_entry_node[i].carpool_pct = .05;
	wcf_entry_node[i].flowrate = 1000;
	wcf_entry_node[i].truck_pct = .10;
	wcf_entry_node[i].hov_violators_per10000 = 100;

	i++;
	wcf_entry_node[i].Node_ID = 3;
	wcf_entry_node[i].carpool_pct = .05;
	wcf_entry_node[i].flowrate = 162;
	wcf_entry_node[i].truck_pct = .10;
	wcf_entry_node[i].hov_violators_per10000 = 100;

	i++;
	wcf_entry_node[i].Node_ID = 7;
	wcf_entry_node[i].carpool_pct = .05;
	wcf_entry_node[i].flowrate = 100;
	wcf_entry_node[i].truck_pct = .10;
	wcf_entry_node[i].hov_violators_per10000 = 100;

	i++;
	wcf_entry_node[i].Node_ID = 8;
	wcf_entry_node[i].carpool_pct = .05;
	wcf_entry_node[i].flowrate = 125;
	wcf_entry_node[i].truck_pct = .10;
	wcf_entry_node[i].hov_violators_per10000 = 100;

	i++;
	wcf_entry_node[i].Node_ID = 10;
	wcf_entry_node[i].carpool_pct = .05;
	wcf_entry_node[i].flowrate = 100;
	wcf_entry_node[i].truck_pct = .10;
	wcf_entry_node[i].hov_violators_per10000 = 100;

	i++;
	wcf_entry_node[i].Node_ID = 11;
	wcf_entry_node[i].carpool_pct = .05;
	wcf_entry_node[i].flowrate = 100;
	wcf_entry_node[i].truck_pct = .10;
	wcf_entry_node[i].hov_violators_per10000 = 100;

	i++;
	wcf_entry_node[i].Node_ID = 101;
	wcf_entry_node[i].carpool_pct = .05;
	wcf_entry_node[i].flowrate = 1000;
	wcf_entry_node[i].truck_pct = .10;
	wcf_entry_node[i].hov_violators_per10000 = 100;

	i++;
	wcf_entry_node[i].Node_ID = 107;
	wcf_entry_node[i].carpool_pct = .0;
	wcf_entry_node[i].flowrate = 500;
	wcf_entry_node[i].truck_pct = .0;
	wcf_entry_node[i].hov_violators_per10000 = 100;
}

void Client_Default_Network_define_ftc_data_inputs(array<WCF_FTC_DATA>^ wcf_ftc_data_inputs)
{
	for (int i = 0; i < wcf_ftc_data_inputs->Length; i++)
	{
		wcf_ftc_data_inputs[i].approach = gcnew array<int>(0);
		wcf_ftc_data_inputs[i].duration = gcnew array<float>(0);
		wcf_ftc_data_inputs[i].signal_code = gcnew array<array<int>^>(1);
	}

	int i = 0;
	wcf_ftc_data_inputs[i].node = 1;
	wcf_ftc_data_inputs[i].active_intervals = 1;
	wcf_ftc_data_inputs[i].approaches = 2;
	System::Array::Resize(wcf_ftc_data_inputs[i].approach, wcf_ftc_data_inputs[i].approaches);
	wcf_ftc_data_inputs[i].approach[0] = 5;
	wcf_ftc_data_inputs[i].approach[1] = 0;
	System::Array::Resize(wcf_ftc_data_inputs[i].duration, wcf_ftc_data_inputs[i].active_intervals);
	wcf_ftc_data_inputs[i].signal_code[0] = gcnew array<int>(2);
	wcf_ftc_data_inputs[i].signal_code[0][0] = 1;
	wcf_ftc_data_inputs[i].signal_code[0][1] = 1;

	//i++;
	//wcf_ftc_data_inputs[i].node = 2;
	//wcf_ftc_data_inputs[i].active_intervals = 1;
	//wcf_ftc_data_inputs[i].approaches = 4;
	//System::Array::Resize(wcf_ftc_data_inputs[i].approach, 4);
	//wcf_ftc_data_inputs[i].approach[0] = 5;
	//wcf_ftc_data_inputs[i].approach[1] = 6;
	//wcf_ftc_data_inputs[i].approach[2] = 7;
	//wcf_ftc_data_inputs[i].approach[3] = 8;
	//wcf_ftc_data_inputs[i].signal_code[0] = gcnew array<int>(4);
	//wcf_ftc_data_inputs[i].signal_code[0][0] = 1;
	//wcf_ftc_data_inputs[i].signal_code[0][1] = 1;
	//wcf_ftc_data_inputs[i].signal_code[0][2] = 5;
	//wcf_ftc_data_inputs[i].signal_code[0][3] = 5;

	i++;
	wcf_ftc_data_inputs[i].node = 3;
	wcf_ftc_data_inputs[i].active_intervals = 1;
	wcf_ftc_data_inputs[i].approaches = 2;
	System::Array::Resize(wcf_ftc_data_inputs[i].approach, 2);
	wcf_ftc_data_inputs[i].approach[0] = 5;
	wcf_ftc_data_inputs[i].approach[1] = 0;
	System::Array::Resize(wcf_ftc_data_inputs[i].duration, wcf_ftc_data_inputs[i].active_intervals);
	wcf_ftc_data_inputs[i].signal_code[0] = gcnew array<int>(2);
	wcf_ftc_data_inputs[i].signal_code[0][0] = 1;
	wcf_ftc_data_inputs[i].signal_code[0][1] = 1;

	i++;
	wcf_ftc_data_inputs[i].node = 4;
	wcf_ftc_data_inputs[i].active_intervals = 1;
	wcf_ftc_data_inputs[i].approaches = 2;
	System::Array::Resize(wcf_ftc_data_inputs[i].approach, 2);
	wcf_ftc_data_inputs[i].approach[0] = 9;
	wcf_ftc_data_inputs[i].approach[1] = 7001;
	System::Array::Resize(wcf_ftc_data_inputs[i].duration, wcf_ftc_data_inputs[i].active_intervals);
	wcf_ftc_data_inputs[i].signal_code[0] = gcnew array<int>(2);
	wcf_ftc_data_inputs[i].signal_code[0][0] = 1;
	wcf_ftc_data_inputs[i].signal_code[0][1] = 1;

	i++;
	wcf_ftc_data_inputs[i].node = 5;
	wcf_ftc_data_inputs[i].active_intervals = 12;
	wcf_ftc_data_inputs[i].approaches = 4;
	System::Array::Resize(wcf_ftc_data_inputs[i].approach, 4);
	wcf_ftc_data_inputs[i].approach[0] = 9;
	wcf_ftc_data_inputs[i].approach[1] = 3;
	wcf_ftc_data_inputs[i].approach[2] = 2;
	wcf_ftc_data_inputs[i].approach[3] = 1;
	System::Array::Resize(wcf_ftc_data_inputs[i].duration, wcf_ftc_data_inputs[i].active_intervals);
	wcf_ftc_data_inputs[i].duration[0] = 15;
	wcf_ftc_data_inputs[i].duration[1] = 3;
	wcf_ftc_data_inputs[i].duration[2] = 2;
	wcf_ftc_data_inputs[i].duration[3] = 20;
	wcf_ftc_data_inputs[i].duration[4] = 3;
	wcf_ftc_data_inputs[i].duration[5] = 2;
	wcf_ftc_data_inputs[i].duration[6] = 25;
	wcf_ftc_data_inputs[i].duration[7] = 3;
	wcf_ftc_data_inputs[i].duration[8] = 2;
	wcf_ftc_data_inputs[i].duration[9] = 35;
	wcf_ftc_data_inputs[i].duration[10] = 3;
	wcf_ftc_data_inputs[i].duration[11] = 2;

	System::Array::Resize(wcf_ftc_data_inputs[i].signal_code, wcf_ftc_data_inputs[i].active_intervals);
	for (int j = 0; j < wcf_ftc_data_inputs[i].signal_code->Length; j++)
	{
		wcf_ftc_data_inputs[i].signal_code[j] = gcnew array<int>(wcf_ftc_data_inputs[i].approaches);
	}
	wcf_ftc_data_inputs[i].signal_code[0][0] = 2;
	wcf_ftc_data_inputs[i].signal_code[0][1] = 4;
	wcf_ftc_data_inputs[i].signal_code[0][2] = 2;
	wcf_ftc_data_inputs[i].signal_code[0][3] = 4;

	wcf_ftc_data_inputs[i].signal_code[1][0] = 2;
	wcf_ftc_data_inputs[i].signal_code[1][1] = 0;
	wcf_ftc_data_inputs[i].signal_code[1][2] = 2;
	wcf_ftc_data_inputs[i].signal_code[1][3] = 0;

	wcf_ftc_data_inputs[i].signal_code[2][0] = 2;
	wcf_ftc_data_inputs[i].signal_code[2][1] = 2;
	wcf_ftc_data_inputs[i].signal_code[2][2] = 2;
	wcf_ftc_data_inputs[i].signal_code[2][3] = 2;

	wcf_ftc_data_inputs[i].signal_code[3][0] = 2;
	wcf_ftc_data_inputs[i].signal_code[3][1] = 9;
	wcf_ftc_data_inputs[i].signal_code[3][2] = 2;
	wcf_ftc_data_inputs[i].signal_code[3][3] = 9;

	wcf_ftc_data_inputs[i].signal_code[4][0] = 2;
	wcf_ftc_data_inputs[i].signal_code[4][1] = 0;
	wcf_ftc_data_inputs[i].signal_code[4][2] = 2;
	wcf_ftc_data_inputs[i].signal_code[4][3] = 0;

	wcf_ftc_data_inputs[i].signal_code[5][0] = 2;
	wcf_ftc_data_inputs[i].signal_code[5][1] = 2;
	wcf_ftc_data_inputs[i].signal_code[5][2] = 2;
	wcf_ftc_data_inputs[i].signal_code[5][3] = 2;

	wcf_ftc_data_inputs[i].signal_code[6][0] = 4;
	wcf_ftc_data_inputs[i].signal_code[6][1] = 2;
	wcf_ftc_data_inputs[i].signal_code[6][2] = 4;
	wcf_ftc_data_inputs[i].signal_code[6][3] = 2;

	wcf_ftc_data_inputs[i].signal_code[7][0] = 0;
	wcf_ftc_data_inputs[i].signal_code[7][1] = 2;
	wcf_ftc_data_inputs[i].signal_code[7][2] = 0;
	wcf_ftc_data_inputs[i].signal_code[7][3] = 2;

	wcf_ftc_data_inputs[i].signal_code[8][0] = 2;
	wcf_ftc_data_inputs[i].signal_code[8][1] = 2;
	wcf_ftc_data_inputs[i].signal_code[8][2] = 2;
	wcf_ftc_data_inputs[i].signal_code[8][3] = 2;

	wcf_ftc_data_inputs[i].signal_code[9][0] = 9;
	wcf_ftc_data_inputs[i].signal_code[9][1] = 2;
	wcf_ftc_data_inputs[i].signal_code[9][2] = 9;
	wcf_ftc_data_inputs[i].signal_code[9][3] = 2;

	wcf_ftc_data_inputs[i].signal_code[10][0] = 0;
	wcf_ftc_data_inputs[i].signal_code[10][1] = 2;
	wcf_ftc_data_inputs[i].signal_code[10][2] = 0;
	wcf_ftc_data_inputs[i].signal_code[10][3] = 2;

	wcf_ftc_data_inputs[i].signal_code[11][0] = 2;
	wcf_ftc_data_inputs[i].signal_code[11][1] = 2;
	wcf_ftc_data_inputs[i].signal_code[11][2] = 2;
	wcf_ftc_data_inputs[i].signal_code[11][3] = 2;

	i++;
	wcf_ftc_data_inputs[i].node = 6;
	wcf_ftc_data_inputs[i].active_intervals = 1;
	wcf_ftc_data_inputs[i].approaches = 2;
	System::Array::Resize(wcf_ftc_data_inputs[i].approach, 2);
	wcf_ftc_data_inputs[i].approach[0] = 2;
	wcf_ftc_data_inputs[i].approach[1] = 0;
	System::Array::Resize(wcf_ftc_data_inputs[i].duration, wcf_ftc_data_inputs[i].active_intervals);
	wcf_ftc_data_inputs[i].signal_code[0] = gcnew array<int>(2);
	wcf_ftc_data_inputs[i].signal_code[0][0] = 1;
	wcf_ftc_data_inputs[i].signal_code[0][1] = 1;

	i++;
	wcf_ftc_data_inputs[i].node = 7;
	wcf_ftc_data_inputs[i].active_intervals = 1;
	wcf_ftc_data_inputs[i].approaches = 2;
	System::Array::Resize(wcf_ftc_data_inputs[i].approach, 2);
	wcf_ftc_data_inputs[i].approach[0] = 2;
	wcf_ftc_data_inputs[i].approach[1] = 0;
	System::Array::Resize(wcf_ftc_data_inputs[i].duration, wcf_ftc_data_inputs[i].active_intervals);
	wcf_ftc_data_inputs[i].signal_code[0] = gcnew array<int>(2);
	wcf_ftc_data_inputs[i].signal_code[0][0] = 1;
	wcf_ftc_data_inputs[i].signal_code[0][1] = 1;

	i++;
	wcf_ftc_data_inputs[i].node = 8;
	wcf_ftc_data_inputs[i].active_intervals = 1;
	wcf_ftc_data_inputs[i].approaches = 2;
	System::Array::Resize(wcf_ftc_data_inputs[i].approach, 2);
	wcf_ftc_data_inputs[i].approach[0] = 2;
	wcf_ftc_data_inputs[i].approach[1] = 0;
	System::Array::Resize(wcf_ftc_data_inputs[i].duration, wcf_ftc_data_inputs[i].active_intervals);
	wcf_ftc_data_inputs[i].signal_code[0] = gcnew array<int>(2);
	wcf_ftc_data_inputs[i].signal_code[0][0] = 1;
	wcf_ftc_data_inputs[i].signal_code[0][1] = 1;

	i++;
	wcf_ftc_data_inputs[i].node = 9;
	wcf_ftc_data_inputs[i].active_intervals = 8;
	wcf_ftc_data_inputs[i].approaches = 4;
	System::Array::Resize(wcf_ftc_data_inputs[i].approach, 4);
	wcf_ftc_data_inputs[i].approach[0] = 4;
	wcf_ftc_data_inputs[i].approach[1] = 11;
	wcf_ftc_data_inputs[i].approach[2] = 5;
	wcf_ftc_data_inputs[i].approach[3] = 10;
	System::Array::Resize(wcf_ftc_data_inputs[i].duration, wcf_ftc_data_inputs[i].active_intervals);
	wcf_ftc_data_inputs[i].duration[0] = 20;
	wcf_ftc_data_inputs[i].duration[1] = 3;
	wcf_ftc_data_inputs[i].duration[2] = 2;
	wcf_ftc_data_inputs[i].duration[3] = 40;
	wcf_ftc_data_inputs[i].duration[4] = 2;
	wcf_ftc_data_inputs[i].duration[5] = 3;
	wcf_ftc_data_inputs[i].duration[6] = 3;
	wcf_ftc_data_inputs[i].duration[7] = 2;

	System::Array::Resize(wcf_ftc_data_inputs[i].signal_code, wcf_ftc_data_inputs[i].active_intervals);
	for (int j = 0; j < wcf_ftc_data_inputs[i].signal_code->Length; j++)
	{
		wcf_ftc_data_inputs[i].signal_code[j] = gcnew array<int>(4);
	}
	wcf_ftc_data_inputs[i].signal_code[0][0] = 2;
	wcf_ftc_data_inputs[i].signal_code[0][1] = 1;
	wcf_ftc_data_inputs[i].signal_code[0][2] = 2;
	wcf_ftc_data_inputs[i].signal_code[0][3] = 1;

	wcf_ftc_data_inputs[i].signal_code[1][0] = 2;
	wcf_ftc_data_inputs[i].signal_code[1][1] = 0;
	wcf_ftc_data_inputs[i].signal_code[1][2] = 2;
	wcf_ftc_data_inputs[i].signal_code[1][3] = 0;

	wcf_ftc_data_inputs[i].signal_code[2][0] = 2;
	wcf_ftc_data_inputs[i].signal_code[2][1] = 2;
	wcf_ftc_data_inputs[i].signal_code[2][2] = 2;
	wcf_ftc_data_inputs[i].signal_code[2][3] = 2;

	wcf_ftc_data_inputs[i].signal_code[3][0] = 1;
	wcf_ftc_data_inputs[i].signal_code[3][1] = 2;
	wcf_ftc_data_inputs[i].signal_code[3][2] = 1;
	wcf_ftc_data_inputs[i].signal_code[3][3] = 2;

	wcf_ftc_data_inputs[i].signal_code[4][0] = 0;
	wcf_ftc_data_inputs[i].signal_code[4][1] = 2;
	wcf_ftc_data_inputs[i].signal_code[4][2] = 0;
	wcf_ftc_data_inputs[i].signal_code[4][3] = 2;

	wcf_ftc_data_inputs[i].signal_code[5][0] = 2;
	wcf_ftc_data_inputs[i].signal_code[5][1] = 2;
	wcf_ftc_data_inputs[i].signal_code[5][2] = 2;
	wcf_ftc_data_inputs[i].signal_code[5][3] = 2;

	wcf_ftc_data_inputs[i].signal_code[6][0] = 2;
	wcf_ftc_data_inputs[i].signal_code[6][1] = 2;
	wcf_ftc_data_inputs[i].signal_code[6][2] = 2;
	wcf_ftc_data_inputs[i].signal_code[6][3] = 2;

	wcf_ftc_data_inputs[i].signal_code[7][0] = 2;
	wcf_ftc_data_inputs[i].signal_code[7][1] = 2;
	wcf_ftc_data_inputs[i].signal_code[7][2] = 2;
	wcf_ftc_data_inputs[i].signal_code[7][3] = 2;

	i++;
	wcf_ftc_data_inputs[i].node = 10;
	wcf_ftc_data_inputs[i].active_intervals = 1;
	wcf_ftc_data_inputs[i].approaches = 2;
	System::Array::Resize(wcf_ftc_data_inputs[i].approach, 2);
	wcf_ftc_data_inputs[i].approach[0] = 9;
	wcf_ftc_data_inputs[i].approach[1] = 0;
	System::Array::Resize(wcf_ftc_data_inputs[i].duration, wcf_ftc_data_inputs[i].active_intervals);
	wcf_ftc_data_inputs[i].signal_code[0] = gcnew array<int>(2);
	wcf_ftc_data_inputs[i].signal_code[0][0] = 1;
	wcf_ftc_data_inputs[i].signal_code[0][1] = 1;

	i++;
	wcf_ftc_data_inputs[i].node = 11;
	wcf_ftc_data_inputs[i].active_intervals = 1;
	wcf_ftc_data_inputs[i].approaches = 2;
	System::Array::Resize(wcf_ftc_data_inputs[i].approach, 2);
	wcf_ftc_data_inputs[i].approach[0] = 9;
	wcf_ftc_data_inputs[i].approach[1] = 0;
	System::Array::Resize(wcf_ftc_data_inputs[i].duration, wcf_ftc_data_inputs[i].active_intervals);
	wcf_ftc_data_inputs[i].signal_code[0] = gcnew array<int>(2);
	wcf_ftc_data_inputs[i].signal_code[0][0] = 1;
	wcf_ftc_data_inputs[i].signal_code[0][1] = 1;

	for (int iftc = 0; iftc < wcf_ftc_data_inputs->Length; iftc++)
	{
		for (int j = 0; j < wcf_ftc_data_inputs[iftc].duration->Length; ++j)
		{
			wcf_ftc_data_inputs[iftc].cycle_length += wcf_ftc_data_inputs[iftc].duration[j];
		}
	}
}
void Client_Default_Network_define_ac_data_inputs(array<WCF_AC>^ wcf_ac_data_inputs)
{
	/*
	for (int i = 0; i < wcf_ac_data_inputs->Length; i++)
	{
		wcf_ac_data_inputs[i].currentphase = gcnew array<int>(0);
		wcf_ac_data_inputs[i].direct_approach = gcnew array<int>(0);
		wcf_ac_data_inputs[i].other_approach = gcnew array<int>(0);
		wcf_ac_data_inputs[i].permissive_period_begin = gcnew array<int>(0);
		wcf_ac_data_inputs[i].permissive_period_end = gcnew array<int>(0);
		wcf_ac_data_inputs[i].phase = gcnew array<WCF_PHASE_DATA>(8);
		for (int j = 0; j < wcf_ac_data_inputs[i].phase->Length; j++)
		{
			wcf_ac_data_inputs[i].phase[j].const_demand_period_begin = gcnew array<int>(0);
			wcf_ac_data_inputs[i].phase[j].const_demand_period_end = gcnew array<int>(0);
			wcf_ac_data_inputs[i].phase[j].detector1_id = gcnew array<int>(0);
			wcf_ac_data_inputs[i].phase[j].detector2_id = gcnew array<int>(0);
			wcf_ac_data_inputs[i].phase[j].detector3_id = gcnew array<int>(0);
			wcf_ac_data_inputs[i].phase[j].movement_code = gcnew array<int>(0);
		}
	}

	int i = 0;
	wcf_ac_data_inputs[i].node = 2;
	wcf_ac_data_inputs[i].direct_approaches = 4;
	System::Array::Resize(wcf_ac_data_inputs[i].direct_approach, 4);
	wcf_ac_data_inputs[i].direct_approach[0] = 5;
	wcf_ac_data_inputs[i].direct_approach[1] = 6;
	wcf_ac_data_inputs[i].direct_approach[2] = 7;
	wcf_ac_data_inputs[i].direct_approach[3] = 8;

	wcf_ac_data_inputs[i].cycle_length = 91;
	wcf_ac_data_inputs[i].yield_point = 34;
	System::Array::Resize(wcf_ac_data_inputs[i].permissive_period_begin, 3);
	System::Array::Resize(wcf_ac_data_inputs[i].permissive_period_end, 3);
	wcf_ac_data_inputs[i].permissive_period_begin[1] = 11;
	wcf_ac_data_inputs[i].permissive_period_end[1] = 24;
	wcf_ac_data_inputs[i].permissive_period_begin[2] = 41;
	wcf_ac_data_inputs[i].permissive_period_end[2] = 41;
	wcf_ac_data_inputs[i].inhibitmax = 0;
	System::Array::Resize(wcf_ac_data_inputs[i].currentphase, 1);
	wcf_ac_data_inputs[i].currentphase[0] = 1;

	System::Array::Resize(wcf_ac_data_inputs[i].phase, 8);
	for (int ip = 0; ip < wcf_ac_data_inputs[i].phase->Length; ++ip)
	{
		System::Array::Resize(wcf_ac_data_inputs[i].phase[ip].movement_code, 
			wcf_ac_data_inputs[i].direct_approaches);
		System::Array::Resize(wcf_ac_data_inputs[i].phase[ip].const_demand_period_begin, 
			wcf_ac_data_inputs[i].direct_approaches);
		System::Array::Resize(wcf_ac_data_inputs[i].phase[ip].const_demand_period_end, 
			wcf_ac_data_inputs[i].direct_approaches);
		System::Array::Resize(wcf_ac_data_inputs[i].phase[ip].detector1_id, 
			wcf_ac_data_inputs[i].direct_approaches);
		System::Array::Resize(wcf_ac_data_inputs[i].phase[ip].detector2_id, 
			wcf_ac_data_inputs[i].direct_approaches);
		System::Array::Resize(wcf_ac_data_inputs[i].phase[ip].detector3_id, 
			wcf_ac_data_inputs[i].direct_approaches);
	}
	wcf_ac_data_inputs[i].phase[0].in_use = true;
	wcf_ac_data_inputs[i].phase[0].max_green = 15;
	wcf_ac_data_inputs[i].phase[0].min_green = 4;
	wcf_ac_data_inputs[i].phase[0].passage_time = 3.0;
	wcf_ac_data_inputs[i].phase[0].yellow_change_int = 2.0;
	wcf_ac_data_inputs[i].phase[0].red_clear_int = 3.0;
	wcf_ac_data_inputs[i].phase[0].lag_code = 0;
	//System::Array::Resize(wcf_ac_data_inputs[i].phase[0].detector1_id, 1);
	wcf_ac_data_inputs[i].phase[0].detector1_id[0] = 1;
	wcf_ac_data_inputs[i].phase[0].forceoff_time = 52;
	wcf_ac_data_inputs[i].phase[0].permcodes = 111;
	//System::Array::Resize(wcf_ac_data_inputs[i].phase[0].movement_code, 4);
	wcf_ac_data_inputs[i].phase[0].movement_code[0] = 22222;
	wcf_ac_data_inputs[i].phase[0].movement_code[1] = 12222;
	wcf_ac_data_inputs[i].phase[0].movement_code[2] = 22222;
	wcf_ac_data_inputs[i].phase[0].movement_code[3] = 22222;

	wcf_ac_data_inputs[i].phase[1].in_use = true;
	wcf_ac_data_inputs[i].phase[1].max_green = 50;
	wcf_ac_data_inputs[i].phase[1].min_green = 10;
	wcf_ac_data_inputs[i].phase[1].passage_time = 3.0;
	wcf_ac_data_inputs[i].phase[1].yellow_change_int = 4.0;
	wcf_ac_data_inputs[i].phase[1].red_clear_int = 3.0;
	wcf_ac_data_inputs[i].phase[1].lag_code = 1;
	//System::Array::Resize(wcf_ac_data_inputs[i].phase[1].detector1_id, 1);
	wcf_ac_data_inputs[i].phase[1].detector1_id[0] = 2;
	//System::Array::Resize(wcf_ac_data_inputs[i].phase[1].movement_code, 4);
	wcf_ac_data_inputs[i].phase[1].movement_code[0] = 22222;
	wcf_ac_data_inputs[i].phase[1].movement_code[1] = 22222;
	wcf_ac_data_inputs[i].phase[1].movement_code[2] = 22222;
	wcf_ac_data_inputs[i].phase[1].movement_code[3] = 11122;

	wcf_ac_data_inputs[i].phase[2].in_use = true;
	wcf_ac_data_inputs[i].phase[2].max_green = 15;
	wcf_ac_data_inputs[i].phase[2].min_green = 4;
	wcf_ac_data_inputs[i].phase[2].passage_time = 3.0;
	wcf_ac_data_inputs[i].phase[2].yellow_change_int = 2.0;
	wcf_ac_data_inputs[i].phase[2].red_clear_int = 3.0;
	wcf_ac_data_inputs[i].phase[2].lag_code = 0;
	wcf_ac_data_inputs[i].phase[2].forceoff_time = 11;
	wcf_ac_data_inputs[i].phase[2].permcodes = 100;
	//System::Array::Resize(wcf_ac_data_inputs[i].phase[2].movement_code, 4);
	wcf_ac_data_inputs[i].phase[2].movement_code[0] = 22222;
	wcf_ac_data_inputs[i].phase[2].movement_code[1] = 22222;
	wcf_ac_data_inputs[i].phase[2].movement_code[2] = 12222;
	wcf_ac_data_inputs[i].phase[2].movement_code[3] = 22222;

	wcf_ac_data_inputs[i].phase[3].in_use = true;
	wcf_ac_data_inputs[i].phase[3].max_green = 30;
	wcf_ac_data_inputs[i].phase[3].min_green = 10;
	wcf_ac_data_inputs[i].phase[3].passage_time = 3.0;
	wcf_ac_data_inputs[i].phase[3].yellow_change_int = 4.0;
	wcf_ac_data_inputs[i].phase[3].red_clear_int = 2.0;
	wcf_ac_data_inputs[i].phase[3].lag_code = 1;
	//System::Array::Resize(wcf_ac_data_inputs[i].phase[3].detector1_id, 1);
	wcf_ac_data_inputs[i].phase[3].detector1_id[0] = 3;
	wcf_ac_data_inputs[i].phase[3].forceoff_time = 41;
	wcf_ac_data_inputs[i].phase[3].permcodes = 110;
	//System::Array::Resize(wcf_ac_data_inputs[i].phase[3].movement_code, 4);
	wcf_ac_data_inputs[i].phase[3].movement_code[0] = 11122;
	wcf_ac_data_inputs[i].phase[3].movement_code[1] = 22222;
	wcf_ac_data_inputs[i].phase[3].movement_code[2] = 22222;
	wcf_ac_data_inputs[i].phase[3].movement_code[3] = 22222;

	wcf_ac_data_inputs[i].phase[4].in_use = true;
	wcf_ac_data_inputs[i].phase[4].max_green = 14;
	wcf_ac_data_inputs[i].phase[4].min_green = 4;
	wcf_ac_data_inputs[i].phase[4].passage_time = 3.0;
	wcf_ac_data_inputs[i].phase[4].yellow_change_int = 2.0;
	wcf_ac_data_inputs[i].phase[4].red_clear_int = 3.0;
	wcf_ac_data_inputs[i].phase[4].lag_code = 0;
	wcf_ac_data_inputs[i].phase[4].forceoff_time = 52;
	wcf_ac_data_inputs[i].phase[4].permcodes = 111;
	//System::Array::Resize(wcf_ac_data_inputs[i].phase[4].movement_code, 4);
	wcf_ac_data_inputs[i].phase[4].movement_code[0] = 22222;
	wcf_ac_data_inputs[i].phase[4].movement_code[1] = 22222;
	wcf_ac_data_inputs[i].phase[4].movement_code[2] = 22222;
	wcf_ac_data_inputs[i].phase[4].movement_code[3] = 12222;

	wcf_ac_data_inputs[i].phase[5].in_use = true;
	wcf_ac_data_inputs[i].phase[5].max_green = 50;
	wcf_ac_data_inputs[i].phase[5].min_green = 10;
	wcf_ac_data_inputs[i].phase[5].passage_time = 3.0;
	wcf_ac_data_inputs[i].phase[5].yellow_change_int = 4.0;
	wcf_ac_data_inputs[i].phase[5].red_clear_int = 2.0;
	wcf_ac_data_inputs[i].phase[5].lag_code = 1;
	//System::Array::Resize(wcf_ac_data_inputs[i].phase[5].detector1_id, 1);
	wcf_ac_data_inputs[i].phase[5].detector1_id[0] = 4;
	wcf_ac_data_inputs[i].phase[5].permcodes = 111;
	//System::Array::Resize(wcf_ac_data_inputs[i].phase[5].movement_code, 4);
	wcf_ac_data_inputs[i].phase[5].movement_code[0] = 22222;
	wcf_ac_data_inputs[i].phase[5].movement_code[1] = 11122;
	wcf_ac_data_inputs[i].phase[5].movement_code[2] = 22222;
	wcf_ac_data_inputs[i].phase[5].movement_code[3] = 22222;

	wcf_ac_data_inputs[i].phase[6].in_use = true;
	wcf_ac_data_inputs[i].phase[6].max_green = 15;
	wcf_ac_data_inputs[i].phase[6].min_green = 4;
	wcf_ac_data_inputs[i].phase[6].passage_time = 3.0;
	wcf_ac_data_inputs[i].phase[6].yellow_change_int = 2.0;
	wcf_ac_data_inputs[i].phase[6].red_clear_int = 3.0;
	wcf_ac_data_inputs[i].phase[6].lag_code = 0;
	wcf_ac_data_inputs[i].phase[6].forceoff_time = 11;
	wcf_ac_data_inputs[i].phase[6].permcodes = 100;
	//System::Array::Resize(wcf_ac_data_inputs[i].phase[6].movement_code, 4);
	wcf_ac_data_inputs[i].phase[6].movement_code[0] = 12222;
	wcf_ac_data_inputs[i].phase[6].movement_code[1] = 22222;
	wcf_ac_data_inputs[i].phase[6].movement_code[2] = 22222;
	wcf_ac_data_inputs[i].phase[6].movement_code[3] = 22222;

	wcf_ac_data_inputs[i].phase[7].in_use = true;
	wcf_ac_data_inputs[i].phase[7].max_green = 30;
	wcf_ac_data_inputs[i].phase[7].min_green = 10;
	wcf_ac_data_inputs[i].phase[7].passage_time = 3.0;
	wcf_ac_data_inputs[i].phase[7].yellow_change_int = 4.0;
	wcf_ac_data_inputs[i].phase[7].red_clear_int = 3.0;
	wcf_ac_data_inputs[i].phase[7].lag_code = 1;
	//System::Array::Resize(wcf_ac_data_inputs[i].phase[7].detector1_id, 1);
	wcf_ac_data_inputs[i].phase[7].detector1_id[0] = 5;
	wcf_ac_data_inputs[i].phase[7].forceoff_time = 41;
	wcf_ac_data_inputs[i].phase[7].permcodes = 110;
	//System::Array::Resize(wcf_ac_data_inputs[i].phase[7].movement_code, 4);
	wcf_ac_data_inputs[i].phase[7].movement_code[0] = 22222;
	wcf_ac_data_inputs[i].phase[7].movement_code[1] = 22222;
	wcf_ac_data_inputs[i].phase[7].movement_code[2] = 11122;
	wcf_ac_data_inputs[i].phase[7].movement_code[3] = 22222;
	*/

}

void Client_Default_Network_define_fdet_inputs(array<WCF_DETECTOR_INPUTS>^ wcf_fdet_inputs)
{
	int i = 0;
	wcf_fdet_inputs[i].usn = 101;
	wcf_fdet_inputs[i].dsn = 102;
	wcf_fdet_inputs[i].location = 500;
	wcf_fdet_inputs[i].lane1 = 1;
	wcf_fdet_inputs[i].zone_length = 20.0;
}
void Client_Default_Network_define_sdet_inputs(array<WCF_DETECTOR_INPUTS>^ wcf_sdet_inputs)
{
	int i = 0;
	wcf_sdet_inputs[i].usn = 5;
	wcf_sdet_inputs[i].dsn = 2;
	wcf_sdet_inputs[i].location = 0;
	wcf_sdet_inputs[i].lane1 = 7;
	wcf_sdet_inputs[i].zone_length = 20.0;

	i++;
	wcf_sdet_inputs[i].usn = 6;
	wcf_sdet_inputs[i].dsn = 2;
	wcf_sdet_inputs[i].location = 0;
	wcf_sdet_inputs[i].lane1 = 8;
	wcf_sdet_inputs[i].lane2 = 7;
	wcf_sdet_inputs[i].zone_length = 20.0;

	i++;
	wcf_sdet_inputs[i].usn = 8;
	wcf_sdet_inputs[i].dsn = 2;
	wcf_sdet_inputs[i].location = 0;
	wcf_sdet_inputs[i].lane1 = 8;
	wcf_sdet_inputs[i].lane2 = 7;
	wcf_sdet_inputs[i].zone_length = 20.0;

	i++;
	wcf_sdet_inputs[i].usn = 5;
	wcf_sdet_inputs[i].dsn = 2;
	wcf_sdet_inputs[i].location = 0;
	wcf_sdet_inputs[i].lane1 = 9;
	wcf_sdet_inputs[i].zone_length = 20.0;

	i++;
	wcf_sdet_inputs[i].usn = 7;
	wcf_sdet_inputs[i].dsn = 2;
	wcf_sdet_inputs[i].location = 0;
	wcf_sdet_inputs[i].lane1 = 8;
	wcf_sdet_inputs[i].lane2 = 7;
	wcf_sdet_inputs[i].zone_length = 20.0;
}
void Client_Default_Network_define_busroute_inputs(array<WCF_BUSROUTE_DATA>^ wcf_busroute_inputs)
{
	for (int i = 0; i < wcf_busroute_inputs->Length; i++)
	{
		wcf_busroute_inputs[i].route_nodes = gcnew array<int>(0);
		wcf_busroute_inputs[i].stationlist = gcnew array<int>(0);
	}
	int i = 0;
	wcf_busroute_inputs[i].number = 1;
	wcf_busroute_inputs[i].hdwy = 300;
	wcf_busroute_inputs[i].offset = 0;
	wcf_busroute_inputs[i].nodes = 9;

	System::Array::Resize(wcf_busroute_inputs[i].route_nodes, 11);
	wcf_busroute_inputs[i].route_nodes[0] = 101;
	wcf_busroute_inputs[i].route_nodes[1] = 102;
	wcf_busroute_inputs[i].route_nodes[2] = 103;
	wcf_busroute_inputs[i].route_nodes[3] = 104;
	wcf_busroute_inputs[i].route_nodes[4] = 7001;
	wcf_busroute_inputs[i].route_nodes[5] = 4;
	wcf_busroute_inputs[i].route_nodes[6] = 9;
	wcf_busroute_inputs[i].route_nodes[7] = 5;
	wcf_busroute_inputs[i].route_nodes[8] = 3;
	System::Array::Resize(wcf_busroute_inputs[i].stationlist, 1);
	wcf_busroute_inputs[i].stationlist[0] = 1;
}
void Client_Default_Network_define_busstation_inputs(array<WCF_BUSSTATION_DATA>^ wcf_busstation_inputs)
{
	int station_id = 1;
	wcf_busstation_inputs[station_id - 1].block_code = 0;
	wcf_busstation_inputs[station_id - 1].usn = 9;
	wcf_busstation_inputs[station_id - 1].dsn = 5;
	wcf_busstation_inputs[station_id - 1].location = 324;
	wcf_busstation_inputs[station_id - 1].capacity = 1;
	wcf_busstation_inputs[station_id - 1].type_code = 1;
	wcf_busstation_inputs[station_id - 1].dwell = 60;
	wcf_busstation_inputs[station_id - 1].bypass_pct = 10;
	wcf_busstation_inputs[station_id - 1].next_station = 0;
}
void Client_Default_Network_define_incident_data_inputs(array<WCF_INCIDENT_DATA>^ wcf_incident_data_inputs)
{
	for (int i = 0; i < wcf_incident_data_inputs->Length; i++)
	{
		wcf_incident_data_inputs[i].code = gcnew array<int>(0);
	}
	wcf_incident_data_inputs[0].link = 3;
	wcf_incident_data_inputs[0].begin_point = 50;
	wcf_incident_data_inputs[0].begin_time = 120;
	System::Array::Resize(wcf_incident_data_inputs[0].code, 1);
	wcf_incident_data_inputs[0].code[0] = 2;
	wcf_incident_data_inputs[0].end_point = 800;
	wcf_incident_data_inputs[0].end_time = 240;
	wcf_incident_data_inputs[0].warn_point = 200;
}
void Client_Default_Network_define_xy_coord_inputs(array<WCF_NODE_LOCATION_DATA>^ wcf_xy_coord_inputs)
{
	int inode = 1;
	wcf_xy_coord_inputs[inode-1].x = 4687;
	wcf_xy_coord_inputs[inode-1].y = 1521;

	inode = 2;
	wcf_xy_coord_inputs[inode-1].x = 5487;
	wcf_xy_coord_inputs[inode-1].y = 1021;

	inode = 3;
	wcf_xy_coord_inputs[inode-1].x = 4687;
	wcf_xy_coord_inputs[inode-1].y = 521;

	inode = 4;
	wcf_xy_coord_inputs[inode-1].x = 3687;
	wcf_xy_coord_inputs[inode-1].y = 1021;

	inode = 5;
	wcf_xy_coord_inputs[inode-1].x = 4687;
	wcf_xy_coord_inputs[inode-1].y = 1021;

	inode = 6;
	wcf_xy_coord_inputs[inode-1].x = 6052;
	wcf_xy_coord_inputs[inode-1].y = 1021;

	inode = 7;
	wcf_xy_coord_inputs[inode-1].x = 5487;
	wcf_xy_coord_inputs[inode-1].y = 1512;

	inode = 8;
	wcf_xy_coord_inputs[inode-1].x = 5487;
	wcf_xy_coord_inputs[inode-1].y = 507;

	inode = 9;
	wcf_xy_coord_inputs[inode-1].x = 4145;
	wcf_xy_coord_inputs[inode-1].y = 1019;

	inode = 10;
	wcf_xy_coord_inputs[inode-1].x = 4139;
	wcf_xy_coord_inputs[inode-1].y = 1490;

	inode = 11;
	wcf_xy_coord_inputs[inode-1].x = 4145;
	wcf_xy_coord_inputs[inode-1].y = 504;

	inode = 12;
	wcf_xy_coord_inputs[inode-1].x = 1619;
	wcf_xy_coord_inputs[inode-1].y = 1017;

	inode = 13;
	wcf_xy_coord_inputs[inode-1].x = 1924;
	wcf_xy_coord_inputs[inode-1].y = 762;

	inode = 14;
	wcf_xy_coord_inputs[inode-1].x = 2212;
	wcf_xy_coord_inputs[inode-1].y = 551;

	inode = 101;
	wcf_xy_coord_inputs[inode-1].x = 180;
	wcf_xy_coord_inputs[inode-1].y = 1008;

	inode = 102;
	wcf_xy_coord_inputs[inode-1].x = 619;
	wcf_xy_coord_inputs[inode-1].y = 997;

	inode = 103;
	wcf_xy_coord_inputs[inode-1].x = 1619;
	wcf_xy_coord_inputs[inode-1].y = 1017;

	inode = 104;
	wcf_xy_coord_inputs[inode-1].x = 2475;
	wcf_xy_coord_inputs[inode-1].y = 1008;

	inode = 105;
	wcf_xy_coord_inputs[inode-1].x = 1924;
	wcf_xy_coord_inputs[inode-1].y = 762;

	inode = 106;
	wcf_xy_coord_inputs[inode-1].x = 2212;
	wcf_xy_coord_inputs[inode-1].y = 551;

	inode = 107;
	wcf_xy_coord_inputs[inode-1].x = 440;
	wcf_xy_coord_inputs[inode-1].y = 838;

	inode = 200;
	wcf_xy_coord_inputs[inode-1].x = 2468;
	wcf_xy_coord_inputs[inode-1].y = 1046;

	inode = 201;
	wcf_xy_coord_inputs[inode-1].x = 1633;
	wcf_xy_coord_inputs[inode-1].y = 1053;

	inode = 202;
	wcf_xy_coord_inputs[inode-1].x = 185;
	wcf_xy_coord_inputs[inode-1].y = 1050;

	inode = 7001;
	wcf_xy_coord_inputs[inode-1].x = 3086;
	wcf_xy_coord_inputs[inode-1].y = 1005;

	inode = 7002;
	wcf_xy_coord_inputs[inode-1].x = 3084;
	wcf_xy_coord_inputs[inode-1].y = 1048;

}
void Client_Default_Network_define_rampmeter_inputs(array<WCF_RM_DATA>^ wcf_rampmeter_inputs)
{
	for (int i = 0; i < wcf_rampmeter_inputs->Length; i++)
	{
		wcf_rampmeter_inputs[i].detector = gcnew array<int>(0);
		wcf_rampmeter_inputs[i].speed = gcnew array<int>(0);
		wcf_rampmeter_inputs[i].headway = gcnew array<float>(0);
	}
	int i = 0;
	wcf_rampmeter_inputs[i].dsn = 102;
	wcf_rampmeter_inputs[i].link = 7;
	wcf_rampmeter_inputs[i].control = 1;
	wcf_rampmeter_inputs[i].onset = 30;
	System::Array::Resize(wcf_rampmeter_inputs[i].headway, 1);
	wcf_rampmeter_inputs[i].headway[0] = 9.0;
	wcf_rampmeter_inputs[i].twopergreen = false;
}

void Client_Default_Network_define_parking_zone_inputs(array<WCF_PARKING_DATA>^ wcf_parking_zone_inputs)
{
	int i = 0;
	wcf_parking_zone_inputs[i].link = 9;
	wcf_parking_zone_inputs[i].right_start = 10;
	wcf_parking_zone_inputs[i].right_len = 600;
	wcf_parking_zone_inputs[i].freq = 20;
	wcf_parking_zone_inputs[i].duration = 5;
}

void Client_Default_Network_define_event_inputs(array<WCF_EVENT_DATA>^ wcf_event_inputs)
{
	int i = 0;
	wcf_event_inputs[i].begin_time = 10;
	wcf_event_inputs[i].end_time= 110;
	wcf_event_inputs[i].lane = 1;
	wcf_event_inputs[i].link = 9;
	wcf_event_inputs[i].location = 225;
	//wcf_event_inputs[i].type = 4;
}

void Client_Default_Network_define_diversion_inputs(array<WCF_DIVERSION_DATA>^ wcf_diversion_inputs)
{
	int i = 0;
	wcf_diversion_inputs[i].begin_time = 90;
	wcf_diversion_inputs[i].end_time = 390;
	wcf_diversion_inputs[i].link = 1;
	wcf_diversion_inputs[i].location = 100;
	wcf_diversion_inputs[i].pathid = 1;
	wcf_diversion_inputs[i].percentage = 75;
	wcf_diversion_inputs[i].speed = 40;
}
