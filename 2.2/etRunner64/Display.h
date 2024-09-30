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
#ifndef _DISPLAY
#define _DISPLAY
#include "etFommInterface.h"

void DisplayFreewayLinks(etFommInterface *etFommIF);
void DisplayStreetLinks(etFommInterface *etFommIF);
void DisplayEntryNodes(etFommInterface *etFommIF);
void DisplayFTCSignals(etFommInterface *etFommIF);
void DisplayRampMeters(etFommInterface *etFommIF);
void DisplayBusRoutes(etFommInterface *etFommIF);
void DisplayBusStations(etFommInterface *etFommIF);


#endif