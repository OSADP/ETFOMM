// Define Animator input data structures

#pragma once
#ifndef ETANIMATORSTRUCT_H
#define ETANIMATORSTRUCT_H

#include <vector>
#include <memory>

struct AniLinkData
{
	float timeStep;
	int networkCode;
	int usn;
	int dsn;
	int numberOfVehicles;
	int leftTurnCode;
	int leftDiagCode;
	int thruCode;
	int rightDiagCode;
	int rightTurnCode;
	int signalizedFlag;
	int numberOfRampMeters;
	int rampMeterSignalState;
	int linkID;
};

struct AniVehicleData
{
	float timeStep;
	int usn;
	int dsn;
	int vehicleID;
	int laneID;
	int distFromUSN; // float or int?
	int prevLinkUSN;
	int turnCode;
	int queueCode;
	int acceleration; // float or int?
	int velocity; // float or int?
	int laneChgStatus;
	int targetedLaneID;
	int destination;
	int leaderID;
	int followerID;
	int prevLinkLaneID;
};

struct AniVehicle
{
	int vehicleID;
	int entryNodeID;
	int fleetID;
	int vehicleTypeCode;
	int vehicleLength;
	int driverTypeCode;
};

struct AniAuxLane
{
	int laneID;
	int code;
	int length;
};

struct AniAddDropLane
{
	int laneID;
	int code;
	int dist;
};

struct AniFreewayLink
{
	int usn;
	int dsn;
	int length;
	int thruDSN;
	int offRampDSN;
	int linkType;
	int numOfFullLanes;
	int thruAlignLane;
	int offrampAlignLane;
	AniAuxLane auxLanes[N_AUXLANES];
	AniAddDropLane addDropLanes[N_ADDDROPLANES];
};

struct AniSurfaceLink
{
	int usn;
	int dsn;
	int length;
	int numOfFullLanes;
	int alignLane;
	int thruAlignLane;
	int numOfLeftPockets;
	int lenOfLeftPockets;
	int numOfRightPockets;
	int lenOfRightPockets;
	int leftDSN;
	int thruDSN;
	int rightDSN;
	int diagDSN;
	int opposeNode;
};

struct AniNode
{
	int nodeID;
	float x;
	float y;   
	int networkCode;
	float latitude;
	float longitude;
	int altitude;
};

struct AniLocation
{
	int usn;
	int dsn;
	int distUpStopBar2USN;
	int distDownStopBar2DSN;
	int distSign2DSN;
	int distCurb2Sign;
	int signType;
};


#endif