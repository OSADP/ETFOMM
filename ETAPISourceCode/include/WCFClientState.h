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
#ifndef WCFCLIENTSTATE_H
#define WCFCLIENTSTATE_H

enum ClientState
{
	NotAvailable,
	Started,
	RequestingUpdate,
	NoRequest,
	APIShutDown,
	VehicleUpdateSubmitted,
	AddPathSubmitted,
	AddVehicleSubmitted,
	SignalsDataSubmitted,
	FixTimeControlSignalsDataSubmitted,
	Continued,
	Finished,
	UseHostDefineNetwork,
	UseClientDefineNetwork,
	UseTRFFile,
	NextRun,
	UpdateAll,
	UpdateFVehicle,
	UpdateSVehicle,
	UpdateFreewayLink,
	UpdateStreetLink,
	UpdateFTCSignal,
	UpdateACSignal,
	UpdateEntryNode,
	UpdateRampMeter,
	UpdateNInputs,
	UpdateFNInputs,
	UpdateSNInputs,
	UpdateVTypes,
	UpdateFDetInputs,
	UpdateSDetInputs,
	UpdateCPTInputs,
	UpdateBRInputs,
	UpdateBSInputs,
	UpdateIncidents,
	UpdateXYCoords,
	UpdateParkZInputs,
	UpdateEventInputs,
	UpdateDiverInputs,
	DataReady,
	GetCoordinationData,
	GotCoordinationData,
	SetCoordinationData,
	SetCoordinationDataDone,
	SetNewCycleLength,
	GetNewCycleLength,
	GetCycleLength,
	SetNewOffset,
	GetNewOffset,
	GetOffset,
	GetLocalCycleTimer,
	SetNewSplits,
	GetNewSplits,
	GetSplits,
	GetMinSplits,
	GetAverageTravelTime,
	GotAverageTravelTime,
	GetStreetLaneMOEData,
	GotStreetLaneMOEData,
	GetStreetLinkMOEData,
	GotStreetLinkMOEData,
	GetNodeGYR,
	SetNodeGYR
};

enum TCAState
{
	StartTCAProcess,
	StepSimulationDone,
	TCAProcessingDone,
	GoodForNextSimulationStep,
	SimulationFinished,
	TCAFinished,
	TCAContinued
};


#endif
