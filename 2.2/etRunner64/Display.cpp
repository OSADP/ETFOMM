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
#include "Display.h"


void DisplayFreewayLinks(etFommInterface *etFommIF)
{
	int n_freeway_links = etFommIF->GetNumberOfFreewayLinks();
	std::cout << std::endl << "#freeway links = " << n_freeway_links << std::endl;
	if(n_freeway_links > 0)
	{
		FREEWAY_LINK* temp_data = (FREEWAY_LINK*)calloc(n_freeway_links, sizeof(FREEWAY_LINK));
		etFommIF->GetFreewayLinks(temp_data);
		for(int il = 0; il < n_freeway_links; il++)
		{
			std::cout << "  link # " << il+1 << std::endl;
			//TODO use temp_data[il].id?
			std::cout << "  upstream node = " << temp_data[il].usn << std::endl;
			std::cout << "  downstream node = " << temp_data[il].dsn << std::endl;
			std::cout << "  thru downstream node = " << temp_data[il].thrunode << std::endl;
			std::cout << "  offramp downstream node = " << temp_data[il].exitnode << std::endl;
			std::cout << "  length = " << temp_data[il].length << std::endl;
			std::cout << "  full lanes = " << temp_data[il].fulllanes << std::endl;
			for(int iaux = 0; iaux < N_AUXLANES; iaux++)
			{
				if(temp_data[il].auxlaneid[iaux] != 0)
				{
					std::cout << "  aux lane " << iaux+1;
					std::cout << ": lane ID = " << temp_data[il].auxlaneid[iaux];
					std::cout << ", code = " << temp_data[il].auxlanecode[iaux];
					std::cout << ", length = " << temp_data[il].auxlanelength[iaux] << std::endl;
				}
			}
			std::cout << "  freeflow speed = " << temp_data[il].freeflowspeed << std::endl << std::endl;
		}
	}
}
void DisplayStreetLinks(etFommInterface *etFommIF)
{
	int n_street_links = etFommIF->GetNumberOfStreetLinks();
	std::cout << std::endl << "#street links = " << n_street_links << std::endl;
	if(n_street_links > 0)
	{
		STREET_LINK* temp_data = (STREET_LINK*)calloc(n_street_links, sizeof(STREET_LINK));
		etFommIF->GetStreetLinks(temp_data);
		for(int il = 0; il < n_street_links; il++)
		{
			std::cout << "  link # " << il+1 << std::endl;
			std::cout << "  upstream node = " << temp_data[il].usn << std::endl;
			std::cout << "  downstream node = " << temp_data[il].dsn << std::endl;
			std::cout << "  thru downstream node = " << temp_data[il].thrunode << std::endl;				
			std::cout << "  length = " << temp_data[il].length << std::endl;
			std::cout << "  full lanes = " << temp_data[il].fulllanes << std::endl;
			for(int ilane = 0; ilane < temp_data[il].fulllanes; ilane++)
			{
				std::cout << "  lane " << ilane + 1 << " channelization = " << temp_data[il].channelization[ilane] << std::endl;
			}
			std::cout << "  freeflow speed = " << temp_data[il].freeflowspeed << std::endl << std::endl;
		}
	}
}
void DisplayEntryNodes(etFommInterface *etFommIF)
{
	int typedist, erlanga;
	float minsep;
	int n_entrynodes = etFommIF->GetNumberOfEntrynodes();
	if(n_entrynodes > 0)
	{
		ENTRYNODES_DATA *temp_data = (ENTRYNODES_DATA*)calloc(n_entrynodes, sizeof(ENTRYNODES_DATA));
		etFommIF->GetEntrynodes(&typedist, &erlanga, &minsep, temp_data);
		for(int inode = 0; inode < n_entrynodes; inode++)
		{
			std::cout << "  entry node  " << temp_data[inode].Node_ID << std::endl;
			std::cout << "     flowrate " << temp_data[inode].flowrate << std::endl;
			std::cout << "     carpool% " << temp_data[inode].carpool_pct << std::endl;
			std::cout << "     truck%   " << temp_data[inode].truck_pct << std::endl << std::endl;
		}
	}
}
void DisplayFTCSignals(etFommInterface *etFommIF)
{
	int n_ftcs = etFommIF->GetNumberOfFTCSignals();
	std::cout << std::endl << "#signs or signals = " << n_ftcs << std::endl;
	if(n_ftcs > 0)
	{
		FTC_DATA *temp_data = (FTC_DATA*)calloc(n_ftcs, sizeof(FTC_DATA));
		etFommIF->GetFTCSignals(temp_data);
		for(int isig = 0; isig < n_ftcs; isig++)
		{
			std::cout << "     node number      " << temp_data[isig].node << std::endl;
			std::cout << "     approaches       " << temp_data[isig].approaches << std::endl;
			for(int ix = 0; ix < temp_data[isig].approaches; ix++)
			{
				std::cout << "        upnode " << temp_data[isig].approach[ix] << std::endl;
			}
			std::cout << "     active_intervals " << temp_data[isig].active_intervals << std::endl;
			if(temp_data[isig].active_intervals > 1)
			{
				for(int ix = 0; ix < temp_data[isig].active_intervals; ix++)
				{
					std::cout << "         duration     " << temp_data[isig].duration[ix] << " codes ";
					for(int xx = 0; xx < temp_data[isig].approaches; xx++)
					{
						std::cout << temp_data[isig].signal_code[ix][xx];
					}
					std::cout << std::endl;
				}
				std::cout << "     cycle_length     " << temp_data[isig].cycle_length << std::endl;
			}
			std::cout << std::endl;
		}
	}
}
void DisplayRampMeters(etFommInterface *etFommIF)
{
	int n_rampmeters = etFommIF->GetNumberOfRampmeters();
	std::cout << std::endl << "#rampmeters = " << n_rampmeters << std::endl;
	if(n_rampmeters > 0)
	{
		RM_DATA *temp_data = (RM_DATA*)calloc(n_rampmeters, sizeof(RM_DATA));
		etFommIF->GetRampmeters(temp_data);

		for(int isig = 0; isig < n_rampmeters; isig++)
		{
			std::cout << "     node number      " << temp_data[isig].dsn << std::endl;
			std::cout << "     on set           " << temp_data[isig].onset << std::endl;
			std::cout << "     headway      " << temp_data[isig].headway[0] << std::endl;
		}
	}
}
void DisplayBusRoutes(etFommInterface *etFommIF)
{
	int n_busroutes = etFommIF->GetNumberOfBusroutes();
	std::cout << std::endl << "#bus routes = " << n_busroutes << std::endl;
	if(n_busroutes > 0)
	{
		BUSROUTE_DATA *temp_data = (BUSROUTE_DATA*)calloc(n_busroutes, sizeof(BUSROUTE_DATA));
		etFommIF->GetBusroutes(temp_data);
		for(int ibr = 0; ibr < n_busroutes; ibr++)
		{
			std::cout << "     bus route #  " << temp_data[ibr].number << std::endl;
			std::cout << "       headway    " << temp_data[ibr].hdwy << std::endl;
			std::cout << "       offset     " << temp_data[ibr].offset << std::endl;
			std::cout << "       #nodes     " << temp_data[ibr].nodes << " -> ";
			for(int in = 0; in < temp_data[ibr].nodes - 1; in++)
			{
				std::cout << temp_data[ibr].route_nodes[in] << ", ";
			}
			std::cout << temp_data[ibr].route_nodes[temp_data[ibr].nodes-1] << std::endl;
		}
	}
}
void DisplayBusStations(etFommInterface *etFommIF)
{
	int n_busroutes = 1;//TODO
	int n_busstations = 1;//TODO
	if(n_busstations > 0)
	{
		BUSSTATION_DATA *temp_data = (BUSSTATION_DATA*)calloc(n_busroutes, sizeof(BUSSTATION_DATA));
		etFommIF->GetBusstations(temp_data);
		for(int istat = 0; istat < n_busstations; istat++)
		{
			std::cout << "     bus station #  " << istat + 1 << std::endl;
			std::cout << "       usn         "  << temp_data[istat].usn << std::endl;
			std::cout << "       dsn         "  << temp_data[istat].dsn << std::endl;
			std::cout << "       block code  "  << temp_data[istat].block_code << std::endl;
			std::cout << "       location    "  << temp_data[istat].location << std::endl;
			std::cout << "       capacity    "  << temp_data[istat].capacity << std::endl;
			std::cout << "       type code   "  << temp_data[istat].type_code << std::endl;
			std::cout << "       dwell time  "  << temp_data[istat].dwell << std::endl;
			std::cout << "       bypass%     "  << temp_data[istat].bypass_pct << std::endl << std::endl;
			std::cout << "       next station"   << temp_data[istat].next_station << std::endl;
			std::cout << "       pocket_lane "   << temp_data[istat].pocket_lane;
			std::cout << "       front vehicle"  << temp_data[istat].front;
			std::cout << "       bus count"      << temp_data[istat].count;
			std::cout << "       dwell time"     << temp_data[istat].dwell_time;
			std::cout << "       empty time"     << temp_data[istat].empty_time;
			std::cout << "       overflow time"  << temp_data[istat].overflow_time << std::endl;
		}
	}
}
