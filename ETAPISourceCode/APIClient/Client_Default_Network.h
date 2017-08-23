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

#ifndef _Client_Default_Network
#define _Client_Default_Network

//#ifndef _WIN64
//#using "..\Release\WCFServer.dll"
//#else
//#using "..\x64\Release\WCFServer.dll"
//#endif
using namespace WCFServer;

void Client_Default_Network_define_Network_INPUTS(array<WCF_NETWORK_INPUTS>^ wcf_ni);
void Client_Default_Network_define_FREEWAY_NETWORK_INPUTS(array<WCF_FREEWAY_NETWORK_INPUTS>^ wcf_fni);
void Client_Default_Network_define_STREET_NETWORK_INPUTS(array<WCF_STREET_NETWORK_INPUTS>^ wcf_sni);
void Client_Default_Network_define_Vehicle_Type_Inputs(array<WCF_VEHICLE_TYPE_DATA>^ wcf_vti);

void Client_Default_Network_define_freeway_link_data(array<Wcf_freeway_link>^ wcf_fwl);
void Client_Default_Network_define_street_link_data(array<Wcf_street_link>^ wcf_sl);
void Client_Default_Network_define_cond_turnpct_data(array<WCF_COND_TURNPCTS>^ wcf_ct);
void Client_Default_Network_define_entrynode_inputs(array<WCF_ENTRYNODES_DATA>^ wcf_entry_node);

void Client_Default_Network_define_ftc_data_inputs(array<WCF_FTC_DATA>^ ftc_data_inputs);
void Client_Default_Network_define_ac_data_inputs(array<WCF_AC>^ wcf_ac_data_inputs);

void Client_Default_Network_define_fdet_inputs(array<WCF_DETECTOR_INPUTS>^ wcf_fdet_inputs);
void Client_Default_Network_define_sdet_inputs(array<WCF_DETECTOR_INPUTS>^ wcf_sdet_inputs);
void Client_Default_Network_define_busroute_inputs(array<WCF_BUSROUTE_DATA>^ wcf_busroute_inputs);
void Client_Default_Network_define_busstation_inputs(array<WCF_BUSSTATION_DATA>^ wcf_busstation_inputs);
void Client_Default_Network_define_xy_coord_inputs(array<WCF_NODE_LOCATION_DATA>^ wcf_xy_coord_inputs);
void Client_Default_Network_define_rampmeter_inputs(array<WCF_RM_DATA>^ wcf_rampmeter_inputs);
void Client_Default_Network_define_incident_data_inputs(array<WCF_INCIDENT_DATA>^ wcf_incident_data_inputs);

void Client_Default_Network_define_parking_zone_inputs(array<WCF_PARKING_DATA>^ wcf_parking_zone_inputs);
void Client_Default_Network_define_event_inputs(array<WCF_EVENT_DATA>^ wcf_event_inputs);
void Client_Default_Network_define_diversion_inputs(array<WCF_DIVERSION_DATA>^ wcf_diversion_inputs);

#endif
