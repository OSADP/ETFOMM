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
#ifndef _Host_Default_Network
#define _Host_Default_Network

#include "DataStruct.h"


NETWORK_INPUTS Host_Default_Network_define_Network_INPUTS(void);
FREEWAY_NETWORK_INPUTS Host_Default_Network_define_FREEWAY_NETWORK_INPUTS(void);
STREET_NETWORK_INPUTS Host_Default_Network_define_STREET_NETWORK_INPUTS(void);
void Host_Default_Network_define_Vehicle_Type_Inputs(VEHICLE_TYPE_DATA* Vehicle_Type_Inputs);

void Host_Default_Network_define_freeway_link_data(FREEWAY_LINK* freeway_link_data);
void Host_Default_Network_define_street_link_data(STREET_LINK* street_link_data);
void Host_Default_Network_define_cond_turnpct_data(COND_TURNPCTS* cond_turnpct_data);
void Host_Default_Network_define_entrynode_inputs(ENTRYNODES_DATA* entrynode_inputs);

void Host_Default_Network_define_ftc_data_inputs(FTC_DATA* ftc_data_inputs);
void Host_Default_Network_define_ac_data_inputs(AC_INPUTS* ac_data_inputs);

void Host_Default_Network_define_fdet_inputs(DETECTOR_INPUTS* fdet_inputs);
void Host_Default_Network_define_sdet_inputs(DETECTOR_INPUTS* sdet_inputs);
void Host_Default_Network_define_busroute_inputs(BUSROUTE_DATA* busroute_inputs);
void Host_Default_Network_define_busstation_inputs(BUSSTATION_DATA* busstation_inputs);
void Host_Default_Network_define_incident_data_inputs(INCIDENT_DATA* incident_data_inputs);
void Host_Default_Network_define_xy_coord_inputs(NODE_LOCATION_DATA *xy_coord_inputs);
void Host_Default_Network_define_rampmeter_inputs(RM_DATA* rampmeter_inputs);

void Host_Default_Network_define_parking_zone_inputs(PARKING_DATA* parking_zone_inputs);
void Host_Default_Network_define_event_inputs(EVENT_DATA* event_inputs);
void Host_Default_Network_define_diversion_inputs(DIVERSION_DATA* diversion_inputs);

#endif
