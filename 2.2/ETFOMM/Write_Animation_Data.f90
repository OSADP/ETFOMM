! Copyright © 2014
! New Global Systems for Intelligent Transportation Management Corp.
  
! This file is part of ETFOMM.
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU Affero General Public License as
! published by the Free Software Foundation, either version 3 of the
! License, or (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Affero General Public License for more details.
!
! You should have received a copy of the GNU Affero General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.

! ==================================================================================================
  SUBROUTINE WRITE_ANIMATION_DATA
  USE SIMPARAMS
  USE FREEWAY_LINKS
  USE STREET_LINKS
  USE FREEWAY_VEHICLES
  USE STREET_VEHICLES
  USE RAMP_METERS
  USE TIMED_CONTROLLERS
  USE BUS_STATION_DATA
  USE SEEDS
  USE GLOBAL_DATA
  USE NODE_TABLE
  IMPLICIT NONE
#ifdef TSIS_COMPATIBLE
  INCLUDE 'CORWIN.FI'
#endif  
  INCLUDE 'TXDINTERFACE.FI'
  INCLUDE 'IOFILES.INC'
  COMMON /GRAPHIO/ TSTEPFILE, TINTERVALFILE
  INTEGER :: TSTEPFILE, TINTERVALFILE
  INTEGER :: IV, IVN, IL, NV, NMETER
  INTEGER :: ISIG, INTR
  INTEGER :: AL, ILANE, ISTAT, TMP, OPTHRU   
  LOGICAL :: AMBER
  INTEGER :: ICODE, NL, NT, NR, ND
  LOGICAL :: FIRST = .TRUE.
  INTEGER :: FLANE_NUMBER(0:20) = (/0,1,2,3,4,5,6,7,8,9,10,6,7,8,0,0,9,10,11,0,0/)
  !LZ
  LOGICAL :: VisitedSF(HIGHEST_INDEX_S)
  integer :: ivisited
! ----------------------------------------------------------------------
#ifndef TSIS_COMPATIBLE
  IF(FIRST .AND. TSTEPFILE .EQ. 2) THEN
    FIRST = .FALSE.
    CALL WRITE_NETWORK_TEXT
  ENDIF
#endif
 
! --- Write animation data if requested.
 
! --- Process streets.
 
  IF(N_STREET_LINKS .NE. 0) THEN
    THELINKDATA%MODELTYPE = 2
    THELINKDATA%NUMMETERS = 0
    THEVEHDATA%DESTINATION = 0
    DO IL = 1, N_STREET_LINKS
      IF(NODE_TYPE(SUSN(IL)) .EQ. NT_EXTERN) CYCLE
      IF(NODE_TYPE(SDSN(IL)) .EQ. NT_EXTERN) CYCLE
    
  ! --- Define the signal state.
 
      IF(.NOT. SIGNALIZED(IL)) THEN
        THELINKDATA%SIGNALIZED = 0
        ISIG = FTC_SIGNAL_ID(IL)
        IF(ISIG .EQ. 0) THEN
          THELINKDATA%LTURNMOVECODE = S_NONE
          THELINKDATA%THRUMOVECODE = S_NONE
          THELINKDATA%RTURNMOVECODE = S_NONE
          THELINKDATA%LDIAGMOVECODE = S_NONE
          THELINKDATA%RDIAGMOVECODE = S_NONE
        ELSE
          AL = APPROACH_NUM(IL)
          ICODE = FTC_SIGNALS(ISIG)%SIGNAL_CODE(1, AL)
          IF(ICODE .EQ. S_STOP .OR. ICODE .EQ. S_YIELD) THEN
            THELINKDATA%LTURNMOVECODE = ICODE
            THELINKDATA%THRUMOVECODE = ICODE
            THELINKDATA%RTURNMOVECODE = ICODE
            THELINKDATA%LDIAGMOVECODE = ICODE
            THELINKDATA%RDIAGMOVECODE = ICODE
          ELSE
            THELINKDATA%LTURNMOVECODE = S_NONE
            THELINKDATA%THRUMOVECODE = S_NONE
            THELINKDATA%RTURNMOVECODE = S_NONE
            THELINKDATA%LDIAGMOVECODE = S_NONE
            THELINKDATA%RDIAGMOVECODE = S_NONE
          ENDIF
        ENDIF
      ELSEIF(FTC_SIGNAL_ID(IL) .NE. 0) THEN
        ISIG = FTC_SIGNAL_ID(IL)
        INTR = FTC_SIGNALS(ISIG)%CURRENT_INTERVAL
        AL = APPROACH_NUM(IL)
        ICODE = FTC_SIGNALS(ISIG)%SIGNAL_CODE(AL, INTR)
        CALL DECODE_FTC(ICODE, NL, NT, NR, ND, AMBER)
        IF(FTC_SIGNALS(ISIG)%ACTIVE_INTERVALS .EQ. 1) THEN
          THELINKDATA%SIGNALIZED = 0
          THELINKDATA%LTURNMOVECODE = S_NONE
          THELINKDATA%THRUMOVECODE = S_NONE
          THELINKDATA%RTURNMOVECODE = S_NONE
          THELINKDATA%LDIAGMOVECODE = S_NONE
          THELINKDATA%RDIAGMOVECODE = S_NONE
        ELSE
          THELINKDATA%SIGNALIZED = 1
          IF(AMBER) THEN
            INTR = INTR - 1
            IF(INTR .EQ. 0) INTR = FTC_SIGNALS(ISIG)%ACTIVE_INTERVALS
            ICODE = FTC_SIGNALS(ISIG)%SIGNAL_CODE(AL, INTR)
            CALL DECODE_FTC(ICODE, NL, NT, NR, ND, AMBER)
            IF(NL .EQ. S_RED) THEN
              THELINKDATA%LTURNMOVECODE = S_RED
            ELSE
              THELINKDATA%LTURNMOVECODE = S_AMBER
            ENDIF
            IF(NT .EQ. S_RED) THEN
              THELINKDATA%THRUMOVECODE = S_RED
            ELSE
              THELINKDATA%THRUMOVECODE = S_AMBER
            ENDIF
            IF(NR .EQ. S_RED) THEN
              THELINKDATA%RTURNMOVECODE = S_RED
            ELSE
              THELINKDATA%RTURNMOVECODE = S_AMBER
            ENDIF
            IF(ND .EQ. S_RED) THEN
              THELINKDATA%LDIAGMOVECODE = S_RED
              THELINKDATA%RDIAGMOVECODE = S_RED
            ELSE
              THELINKDATA%LDIAGMOVECODE = S_AMBER
              THELINKDATA%RDIAGMOVECODE = S_AMBER
            ENDIF
          ELSE
            IF(NL .EQ. S_GREEN) THEN
              IF(OPPOSE_LINK(IL) .NE. 0) THEN
                ICODE = SIGNAL_CODE(OPPOSE_LINK(IL))
                CALL DECODE_FTC(ICODE, TMP, OPTHRU, TMP, TMP, AMBER)
                IF(AMBER) THEN
                  ICODE = PREV_SIGNAL_CODE(OPPOSE_LINK(IL))
                  CALL DECODE_FTC(ICODE, TMP, OPTHRU, TMP, TMP, AMBER)
                  IF(OPTHRU .NE. S_RED) NL = S_PERGRN
                ELSE
                  IF(OPTHRU .NE. S_RED) NL = S_PERGRN
                ENDIF
              ENDIF
            ENDIF
            THELINKDATA%LTURNMOVECODE = NL 
            THELINKDATA%THRUMOVECODE  = NT
            THELINKDATA%RTURNMOVECODE = NR
            THELINKDATA%LDIAGMOVECODE = ND
            THELINKDATA%RDIAGMOVECODE = ND
          ENDIF
        ENDIF
        
      ELSEIF(AC_SIGNAL_ID(IL) .NE. 0) THEN
        THELINKDATA%SIGNALIZED = 1
        IF(AMBER_LEFT(IL)) THEN
          IF(SIGNAL_LEFT(IL)) THEN
            THELINKDATA%LTURNMOVECODE = S_AMBER
          ELSE
            THELINKDATA%LTURNMOVECODE = S_RED
          ENDIF
        ELSE
          IF(SIGNAL_LEFT(IL)) THEN
            THELINKDATA%LTURNMOVECODE = S_GREEN
            IF(OPPOSE_LINK(IL) .NE. 0) THEN
              IF(SIGNAL_THRU(OPPOSE_LINK(IL))) THELINKDATA%LTURNMOVECODE = S_PERGRN
            ENDIF
          ELSE
            THELINKDATA%LTURNMOVECODE = S_RED
          ENDIF
        ENDIF
        
        IF(AMBER_THRU(IL)) THEN
          IF(SIGNAL_THRU(IL)) THEN
            THELINKDATA%THRUMOVECODE = S_AMBER
          ELSE
            THELINKDATA%THRUMOVECODE = S_RED
          ENDIF
        ELSE
          IF(SIGNAL_THRU(IL)) THEN
            THELINKDATA%THRUMOVECODE = S_GREEN
          ELSE
            THELINKDATA%THRUMOVECODE = S_RED
          ENDIF
        ENDIF
        
        IF(AMBER_RIGHT(IL)) THEN
          IF(SIGNAL_RIGHT(IL)) THEN
            THELINKDATA%RTURNMOVECODE = S_AMBER
          ELSE
            THELINKDATA%RTURNMOVECODE = S_RED
          ENDIF
        ELSE
          IF(SIGNAL_RIGHT(IL)) THEN
            THELINKDATA%RTURNMOVECODE = S_GREEN
          ELSE
            THELINKDATA%RTURNMOVECODE = S_RED
          ENDIF
        ENDIF
        
        IF(AMBER_DIAG(IL)) THEN
          IF(SIGNAL_DIAG(IL)) THEN
            THELINKDATA%LDIAGMOVECODE = S_AMBER
            THELINKDATA%RDIAGMOVECODE = S_AMBER
          ELSE
            THELINKDATA%LDIAGMOVECODE = S_RED
            THELINKDATA%RDIAGMOVECODE = S_RED
          ENDIF
        ELSE
          IF(SIGNAL_DIAG(IL)) THEN
            THELINKDATA%LDIAGMOVECODE = S_GREEN
            THELINKDATA%RDIAGMOVECODE = S_GREEN
          ELSE
            THELINKDATA%LDIAGMOVECODE = S_RED
            THELINKDATA%RDIAGMOVECODE = S_RED
          ENDIF
        ENDIF
      ENDIF
    
  ! Count the number of vehicles on each link. 
      
      NV = 0
      DO ILANE = 1, TOTAL_LANES(IL)
          VisitedSF = .false.
        IV = FIRST_VEHICLE(IL, ILANE)
        DO WHILE(IV .NE. 0)
          NV = NV + 1
          IV = SFOLLOWER(IV)
          if (IV .EQ. 0) THEN
            exit
          else
            if (VisitedSF(IV)) then
              exit
            else 
              VisitedSF(IV)=.true.  
            endif
          endif
        ENDDO
      ENDDO
      IF(FIRST_BUS_STATION(IL) .NE. 0) THEN
        ISTAT = FIRST_BUS_STATION(IL)
        DO WHILE(ISTAT .NE. 0)
          IV = BUS_STATION_LIST(ISTAT)%FRONT
          VisitedSF= .false.

          DO WHILE(IV .NE. 0)
            NV = NV + 1
            IV = SFOLLOWER(IV)
            if (IV .EQ. 0) THEN
              exit
            else
              if (VisitedSF(IV)) then
                exit
              else 
                VisitedSF(IV)=.true.  
              endif
            endif
          ENDDO
          ISTAT = BUS_STATION_LIST(ISTAT)%NEXT_STATION
        ENDDO
      ENDIF
 
  ! --- Process vehicles on the shoulder
          
      IF(SFIRST_ON_SHOULDER(IL) .NE. 0) THEN
        IV = SFIRST_ON_SHOULDER(IL)
        VisitedSF= .false.

        DO WHILE(IV .NE. 0)
            NV = NV + 1
            IV = SFOLLOWER(IV)
            if (IV .EQ. 0) THEN 
              exit
            else
              if (VisitedSF(IV)) then
                exit
              else 
                VisitedSF(IV)=.true.  
              endif
            endif       
        ENDDO
      ENDIF
 
  ! --- Define the elements of a link data record.
 
      THELINKDATA%NUMVEHICLES = NV
      THELINKDATA%USN = SUSN(IL)
      THELINKDATA%DSN = SDSN(IL)
#ifdef TSIS_COMPATIBLE
      CALL SENDMSG(SIM_LINKSTATEVALID, 0)
#else
      CALL WRITE_LINK_TEXT(THELINKDATA)
#endif
      IF(NV .GT. 0) THEN
        DO ILANE = 1, TOTAL_LANES(IL)
          IV = FIRST_VEHICLE(IL, ILANE)
          VisitedSF=.false.  

          DO WHILE(IV .NE. 0)
            IF(NODE_TYPE(SUSN(IL)) .EQ. NT_EXTERN) CYCLE
            IF(NODE_TYPE(SDSN(IL)) .EQ. NT_EXTERN) CYCLE
            THEVEHDATA%VEHICLEGLOBALID = SID(IV)
            THEVEHDATA%FLEET = SFLEET(IV)
            THEVEHDATA%TYPEVEH = SVTYPE(IV)
            THEVEHDATA%VEHICLELENGTH = SVLENGTH(IV)
            THEVEHDATA%DRIVERTYPE = SDRIVERTYPE(IV)
#ifdef TSIS_COMPATIBLE
            THEVEHDATA%LANENUMBER = 0
            IF(SLANE(IV) .NE. 0) THEVEHDATA%LANENUMBER = LANE_NUMBERS(IL, SLANE(IV))
            THEVEHDATA%PREVLANE = 0
            IF(SPREVLINK(IV) .NE. 0 .AND. SPREVLANE(IV) .NE. 0) THEVEHDATA%PREVLANE = LANE_NUMBERS(SPREVLINK(IV), SPREVLANE(IV))
#else         
            THEVEHDATA%LANENUMBER = SLANE(IV)
            THEVEHDATA%PREVLANE = SPREVLANE(IV)
#endif            
            IF(NODE_TYPE(THELINKDATA%USN) .NE. NT_INTERN .AND. THEVEHDATA%PREVLANE .EQ. 0) THEN
              THEVEHDATA%PREVLANE = THEVEHDATA%LANENUMBER
            ENDIF
            THEVEHDATA%DISTANCEFROMUSN = SLOCATION(IV)
            THEVEHDATA%USNPREVLINK = 0
            IF(SPREVLINK(IV) .NE. 0) THEVEHDATA%USNPREVLINK = SUSN(SPREVLINK(IV))
            THEVEHDATA%TURNCODE = STURNCODE(IV)
            THEVEHDATA%ACCELERATION = INT(SACCELERATION(IV))
            THEVEHDATA%VELOCITY = SSPEED(IV)
            THEVEHDATA%LEADVEHICLEID = 0
            IF(SLEADER(IV) .NE. 0) THEVEHDATA%LEADVEHICLEID = SID(SLEADER(IV))
            THEVEHDATA%FOLLOWVEHICLEID = 0
            IF(SLC_TIMER(IV) .LE. 0) THEN
              THEVEHDATA%LANECHANGESTATUS = 0
            ELSE
              THEVEHDATA%LANECHANGESTATUS = 1
            ENDIF
            IF(SFOLLOWER(IV) .NE. 0) THEVEHDATA%FOLLOWVEHICLEID = SID(SFOLLOWER(IV))
#ifdef TSIS_COMPATIBLE
            CALL SENDMSG(SIM_ADDVEHICLE, 0)
#else
            CALL WRITE_VEHICLE_TEXT(THELINKDATA%USN, THELINKDATA%DSN, THEVEHDATA, SPATHID(IV))
            IF(SSPEED(IV) .EQ. 0 .AND. ROUNDABOUT_ID(IL) .NE. 0 .AND. ROUNDABOUT_APPROACH_NUM(IL) .EQ. 0) THEN
              CALL WRITE_VEHICLE_TEXT_STOPPED(IV)
            ENDIF
#endif
    ! Add street vehilce data LZ 7/10/2024
     
            IV = SFOLLOWER(IV)
            if (IV .EQ. 0) THEN
              exit
            else
              if (VisitedSF(IV)) then
                exit
              else 
                VisitedSF(IV)=.true.  
              endif
            endif
          ENDDO
        ENDDO
        IF(FIRST_BUS_STATION(IL) .NE. 0) THEN
          ISTAT = FIRST_BUS_STATION(IL)
          VisitedSF=.false.  

          DO WHILE(ISTAT .NE. 0)
            IV = BUS_STATION_LIST(ISTAT)%FRONT
            DO WHILE(IV .NE. 0)
              THEVEHDATA%VEHICLEGLOBALID = SID(IV)
              THEVEHDATA%FLEET = SFLEET(IV)
              THEVEHDATA%TYPEVEH = SVTYPE(IV)
              THEVEHDATA%VEHICLELENGTH = SVLENGTH(IV)
              THEVEHDATA%DRIVERTYPE = SDRIVERTYPE(IV)
#ifdef TSIS_COMPATIBLE
              THEVEHDATA%LANENUMBER = 0
              THEVEHDATA%PREVLANE = 0
              IF(SPREVLINK(IV) .NE. 0 .AND. SPREVLANE(IV) .NE. 0) THEVEHDATA%PREVLANE = LANE_NUMBERS(SPREVLINK(IV), SPREVLANE(IV))
#else         
              THEVEHDATA%LANENUMBER = 0
              THEVEHDATA%PREVLANE = SPREVLANE(IV)
#endif            
              THEVEHDATA%DISTANCEFROMUSN = SLOCATION(IV)
              THEVEHDATA%USNPREVLINK = 0
              IF(SPREVLINK(IV) .NE. 0) THEVEHDATA%USNPREVLINK = SUSN(SPREVLINK(IV))
              THEVEHDATA%TURNCODE = STURNCODE(IV)
              THEVEHDATA%ACCELERATION = SACCELERATION(IV)
              THEVEHDATA%VELOCITY = SSPEED(IV)
              THEVEHDATA%LEADVEHICLEID = 0
              IF(SLEADER(IV) .NE. 0) THEVEHDATA%LEADVEHICLEID = SID(SLEADER(IV))
              THEVEHDATA%FOLLOWVEHICLEID = 0
              IF(SFOLLOWER(IV) .NE. 0) THEVEHDATA%FOLLOWVEHICLEID = SID(SFOLLOWER(IV))
#ifdef TSIS_COMPATIBLE
              CALL SENDMSG(SIM_ADDVEHICLE, 0)
#else
              CALL WRITE_VEHICLE_TEXT(THELINKDATA%USN, THELINKDATA%DSN, THEVEHDATA, SPATHID(IV))
#endif
            IV = SFOLLOWER(IV)
            if (IV .EQ. 0) THEN
              exit
            else
              if (VisitedSF(IV)) then
                exit
              else 
                VisitedSF(IV)=.true.  
              endif
            endif
            ENDDO
            ISTAT = BUS_STATION_LIST(ISTAT)%NEXT_STATION
          ENDDO
        ENDIF
        IF(SFIRST_ON_SHOULDER(IL) .NE. 0) THEN
          IV = SFIRST_ON_SHOULDER(IL)
          VisitedSF=.false.  

          DO WHILE(IV .NE. 0)
            THEVEHDATA%VEHICLEGLOBALID = SID(IV)
            THEVEHDATA%FLEET = SFLEET(IV)
            THEVEHDATA%TYPEVEH = SVTYPE(IV)
            THEVEHDATA%VEHICLELENGTH = SVLENGTH(IV)
            THEVEHDATA%DRIVERTYPE = SDRIVERTYPE(IV)
#ifdef TSIS_COMPATIBLE
            THEVEHDATA%LANENUMBER = 0
            THEVEHDATA%PREVLANE = 0
            IF(SPREVLINK(IV) .NE. 0 .AND. SPREVLANE(IV) .NE. 0) THEVEHDATA%PREVLANE = LANE_NUMBERS(SPREVLINK(IV), SPREVLANE(IV))
#else         
            THEVEHDATA%LANENUMBER = 0
            THEVEHDATA%PREVLANE = SPREVLANE(IV)
#endif            
            THEVEHDATA%DISTANCEFROMUSN = SLOCATION(IV)
            THEVEHDATA%USNPREVLINK = 0
            IF(SPREVLINK(IV) .NE. 0) THEVEHDATA%USNPREVLINK = SUSN(SPREVLINK(IV))
            THEVEHDATA%TURNCODE = STURNCODE(IV)
            THEVEHDATA%ACCELERATION = SACCELERATION(IV)
            THEVEHDATA%VELOCITY = SSPEED(IV)
            THEVEHDATA%LEADVEHICLEID = 0
            IF(SLEADER(IV) .NE. 0) THEVEHDATA%LEADVEHICLEID = SID(SLEADER(IV))
            THEVEHDATA%FOLLOWVEHICLEID = 0
            IF(SFOLLOWER(IV) .NE. 0) THEVEHDATA%FOLLOWVEHICLEID = SID(SFOLLOWER(IV))
#ifdef TSIS_COMPATIBLE
            CALL SENDMSG(SIM_ADDVEHICLE, 0)
#else
            CALL WRITE_VEHICLE_TEXT(THELINKDATA%USN, THELINKDATA%DSN, THEVEHDATA, SPATHID(IV))
#endif
            IV = SFOLLOWER(IV)
            if (IV .EQ. 0) THEN
              exit
            else
              if (VisitedSF(IV)) then
                exit
              else 
                VisitedSF(IV)=.true.  
              endif
            endif
          ENDDO
        ENDIF
      ENDIF
#ifdef TSIS_COMPATIBLE
      CALL SIMSTATUS(SIM_LINKCOMPLETE)
#endif
    ENDDO
  ENDIF
 
! --- Process freeway links. Count the number of vehicles on each link.
 
  IF(N_FREEWAY_LINKS .NE. 0) THEN
    NVEH = 0
    VFIRST = 0
    VLAST = 0
    DO CONCURRENT (IVN = 1: SORTED_LIST_LENGTH)
      IV = SORTED_LIST(IVN)
      IF(IV .EQ. 0) CYCLE
      IL = FLINK(IV)
      IF(IL .NE. 0) THEN
        NVEH(IL) = NVEH(IL) + 1
        IF(VFIRST(IL) .EQ. 0) VFIRST(IL) = IVN
        VLAST(IL) = IVN
      ENDIF
    ENDDO

    THELINKDATA%MODELTYPE = 1
    DO IL = 1, N_FREEWAY_LINKS
      IF(NVEH(IL) .EQ. 0) CYCLE
      IF(NODE_TYPE(FUSN(IL)) .EQ. NT_EXTERN) CYCLE
      IF(NODE_TYPE(FDSN(IL)) .EQ. NT_EXTERN) CYCLE
      THELINKDATA%NUMMETERS = 0
      IF(RAMPMETER(IL) .NE. 0) THELINKDATA%NUMMETERS = 1

    ! --- Define the elements of a link data record.
 
      THELINKDATA%NUMVEHICLES = NVEH(IL)
      THELINKDATA%USN = FUSN(IL)
      THELINKDATA%DSN = FDSN(IL)
#ifdef TSIS_COMPATIBLE
      CALL SENDMSG(SIM_LINKSTATEVALID, 0)
#else
      CALL WRITE_LINK_TEXT(THELINKDATA)      
#endif
      DO IVN = VFIRST(IL), VLAST(IL)
        IV = SORTED_LIST(IVN)
        IF(IV .EQ. 0) CYCLE
        IF(FLINK(IV) .EQ. IL) THEN
          THEVEHDATA%DESTINATION = 0
          IF(DESTINATION(IV) .NE. 0) THEVEHDATA%DESTINATION = FDSN(DESTINATION(IV))
          THEVEHDATA%VEHICLEGLOBALID = FID(IV)
          THEVEHDATA%FLEET = FFLEET(IV)
          THEVEHDATA%TYPEVEH = FVTYPE(IV)
          THEVEHDATA%VEHICLELENGTH = FVLENGTH(IV)
          THEVEHDATA%DRIVERTYPE = FDRIVERTYPE(IV)
          THEVEHDATA%LANENUMBER = FLANE_NUMBER(FLANE(IV))
          THEVEHDATA%PREVLANE = FLANE_NUMBER(FPREVLANE(IV))
          IF(NODE_TYPE(THELINKDATA%USN) .NE. NT_INTERN .AND. THEVEHDATA%PREVLANE .EQ. 0) THEN
            THEVEHDATA%PREVLANE = THEVEHDATA%LANENUMBER
          ENDIF
          THEVEHDATA%DISTANCEFROMUSN = FLOCATION(IV)
          THEVEHDATA%USNPREVLINK = 0
          IF(FPREVLINK(IV) .NE. 0) THEVEHDATA%USNPREVLINK = FUSN(FPREVLINK(IV))
          THEVEHDATA%TURNCODE = FTURNCODE(IV)
          THEVEHDATA%ACCELERATION = INT(FACCELERATION(IV))
          THEVEHDATA%VELOCITY = FSPEED(IV)
          THEVEHDATA%LEADVEHICLEID = 0
          IF(FLEADER(IV) .NE. 0) THEVEHDATA%LEADVEHICLEID = FID(FLEADER(IV))
          THEVEHDATA%FOLLOWVEHICLEID = 0
          IF(FFOLLOWER(IV) .NE. 0) THEVEHDATA%FOLLOWVEHICLEID = FID(FFOLLOWER(IV))
          IF(FLC_TIMER(IV) .LE. 0) THEN
            THEVEHDATA%LANECHANGESTATUS = 0
          ELSE
            THEVEHDATA%LANECHANGESTATUS = 1
          ENDIF
#ifdef TSIS_COMPATIBLE
          THEVEHDATA%LANENUMBER = FLANE_NUMBER(FLANE(IV))
          THEVEHDATA%PREVLANE = FLANE_NUMBER(FPREVLANE(IV))
          CALL SENDMSG(SIM_ADDVEHICLE, 0)
#else
          THEVEHDATA%LANENUMBER = FLANE(IV)
          THEVEHDATA%PREVLANE = FPREVLANE(IV)
          CALL WRITE_VEHICLE_TEXT(THELINKDATA%USN, THELINKDATA%DSN, THEVEHDATA, FPATHID(IV))
#endif
    ! Add freeway vehilce data LZ 7/10/2024


        ENDIF
      ENDDO
#ifdef TSIS_COMPATIBLE
      CALL SIMSTATUS(SIM_LINKCOMPLETE)
#endif
    ENDDO
 
  ! --- Add ramp meters.
 
    DO NMETER = 1, NUMBER_OF_RAMPMETERS
      THELINKDATA%SIGNALSTATE = RAMPMETERS(NMETER)%STATE
      IF(THELINKDATA%SIGNALSTATE .EQ. MS_INACTIVE) THEN
        THELINKDATA%SIGNALSTATE = 4
      ELSEIF(THELINKDATA%SIGNALSTATE .EQ. MS_RED) THEN
        THELINKDATA%SIGNALSTATE = 0
      ELSEIF(THELINKDATA%SIGNALSTATE .EQ. MS_GREEN) THEN
        THELINKDATA%SIGNALSTATE = 2
      ENDIF
      IL = RAMPMETERS(NMETER)%LINK
      THELINKDATA%USN = FUSN(IL)
      THELINKDATA%DSN = FDSN(IL)
#ifdef TSIS_COMPATIBLE
      CALL SENDMSG(SIM_ADDRAMPMETER, 0)
#else
      THELINKDATA%NUMMETERS = 1
      CALL WRITE_RAMPMETER_TEXT(THELINKDATA)
#endif
    ENDDO
  ENDIF
  RETURN
  END
  
!
!
!*******************************************************************************
!  Event Processing
!*******************************************************************************
!
!
!=======================================================================
!  SUBROUTINE SendEventData(Time, *)
!
!  Title: Send Event Data
!
!  Module ID: none
!
!  Description
!  -----------
!    Loads the time step data event structure for the TxD writer.
!
!  Arguments
!  ---------
!    Time - [in] simulation time step
!
!  Called By
!  ---------
!    SIMULATE
!
!  Calls To
!  --------
!    SetEventMsg
!    
! ==================================================================================================
!
  SUBROUTINE SENDEVENTDATA 
  USE SIMPARAMS
  USE EVENTS 
  IMPLICIT NONE
  INTEGER :: I
  DO I = 1, LASTEVENT
    IF(ALLEVENTS(I)%INCIDENTID .EQ. 0) CYCLE
    IF(SIMTIME .LT. ALLEVENTS(I)%STARTTIME) CYCLE
    IF(SIMTIME .LE. ALLEVENTS(I)%STARTTIME + ALLEVENTS(I)%DURATION) THEN
#ifdef TSIS_COMPATIBLE
      CALL SETEVENTMSG(ALLEVENTS(I))
#else
      CALL WRITE_INCIDENT_TEXT(ALLEVENTS(I))
#endif
    ELSE
      ALLEVENTS(I)%INCIDENTID = 0
    ENDIF
  ENDDO
  RETURN
  END
!
!
! ==================================================================================================
!  SUBROUTINE SetEventMsg(ThisEvent)
!
!  Title: Load Event Into the Time Step Data Event Structure
!
!  Module ID: none
!
!  Description
!  -----------
!    Loads the specified event into the time step data event structure
!    for the TxD writer.
!
!  Arguments
!  ---------
!    ThisEvent - [in] data structure that describes the event
!
!  Called By
!  ---------
!    SIMULATE
!
!  Calls To
!  --------
!    SENDMSG
!    
!    
!=======================================================================
!
#ifdef TSIS_COMPATIBLE
  SUBROUTINE SETEVENTMSG(THISEVENT)
  USE SIMPARAMS
  IMPLICIT NONE
  INCLUDE 'TXDINTERFACE.FI'
  INCLUDE 'CORWIN.FI'
  RECORD / LKINCDATA / THISEVENT
  INTEGER I
  COMMON /GRAPHIO/ TSTEPFILE, TINTERVALFILE
  INTEGER :: TSTEPFILE, TINTERVALFILE

  THELKINCDATA.INCIDENTID       = THISEVENT.INCIDENTID 
  THELKINCDATA.LINKID           = THISEVENT.LINKID
  THELKINCDATA.INCIDENTTYPE     = THISEVENT.INCIDENTTYPE
  THELKINCDATA.INCIDENTPOSITION = THISEVENT.INCIDENTPOSITION
  THELKINCDATA.INCIDENTLENGTH   = THISEVENT.INCIDENTLENGTH
  THELKINCDATA.STARTTIME        = THISEVENT.STARTTIME
  THELKINCDATA.DURATION         = THISEVENT.DURATION
  THELKINCDATA.REACTIONPOINT    = THISEVENT.REACTIONPOINT
  THELKINCDATA.RUBBERNECKFACTOR = THISEVENT.RUBBERNECKFACTOR
  THELKINCDATA.MODELTYPE        = THISEVENT.MODELTYPE
  THELKINCDATA.MSTATE           = THISEVENT.MSTATE
  THELKINCDATA.NUMAFFECTEDLANES = THISEVENT.NUMAFFECTEDLANES
  DO I = 1, THISEVENT.NUMAFFECTEDLANES
    THELKINCDATA.AFFECTEDLANEIDARRAY(I) = THISEVENT.AFFECTEDLANEIDARRAY(I)
    THELKINCDATA.LANEINCIDENTCODES(I) = THISEVENT.LANEINCIDENTCODES(I)
  ENDDO
  CALL SENDMSG(SIM_ADDINCIDENT, 0)
  RETURN
  END
#endif
!
!
! ==================================================================================================
!
!  Description
!  -----------
!    Adds an event with the specified parameters to the simulation's
!    array of events.
!
!  Arguments
!  ---------
!    start       - [in] start time of the event (time step)
!    dur         - [in] duration of the event in time steps
!    inctyp      - [in] incident (event) type
!    iup         - [in] upstream node number of the link
!                       on which event occurs
!    idn         - [in] downstream node number of the link
!                       on which event occurs
!    lanes       - [in] array of lanes affected by the incident
!    dist        - [in] location of the event measured from the
!                       link's upstream end in feet
!    ilength     - [in] length of the incident in feet
!    Reactpoint  - [in] distance upstream from the location of
!                       the event at which vehicles begin to
!                       react to the event
!    RubberneckF - [in] rubbernecking factor in percent
!   
!=======================================================================
!
  SUBROUTINE ADDFREEWAYEVENT(IL, START, DUR, INCTYP, LANECODES, DIST, ILENGTH, REACTPOINT, RUBBERNECKF)
  USE FREEWAY_LINKS
  USE GLOBAL_DATA
  USE EVENTS
  IMPLICIT NONE
  INTEGER :: IL, START, DUR, INCTYP, IUP, IDN, LANECODES(11)
  INTEGER :: DIST, ILENGTH, REACTPOINT, RUBBERNECKF, INDEX, I, J
  IUP = FUSN(IL)
  IDN = FDSN(IL)
  ICOUNT = ICOUNT + 1
  DO INDEX = 1, NUMBER_OF_EVENTS
 
! --- Find the first empty slot in the ALLEVENTS array.
 
    IF(ALLEVENTS(INDEX).INCIDENTID .EQ. 0) THEN
      ALLEVENTS(INDEX).INCIDENTID = ICOUNT
      ALLEVENTS(INDEX).LINKID = IUP * 10000 + IDN
      ALLEVENTS(INDEX).INCIDENTTYPE = INCTYP
      ALLEVENTS(INDEX).INCIDENTPOSITION = DIST
      ALLEVENTS(INDEX).STARTTIME = START
      ALLEVENTS(INDEX).DURATION = DUR
          
      ALLEVENTS(INDEX).INCIDENTLENGTH = ILENGTH
      ALLEVENTS(INDEX).REACTIONPOINT = REACTPOINT
      ALLEVENTS(INDEX).RUBBERNECKFACTOR = RUBBERNECKF
      ALLEVENTS(INDEX).MSTATE = 1
 
      ALLEVENTS(INDEX).MODELTYPE = 8
      J = 0
      DO I = 1, 11
        IF(LANECODES(I) .NE. 0) THEN
          J = J + 1
          ALLEVENTS(INDEX).AFFECTEDLANEIDARRAY(J) = I
          ALLEVENTS(INDEX).LANEINCIDENTCODES(J) = LANECODES(I)
        ENDIF
      ENDDO
      ALLEVENTS(INDEX).NUMAFFECTEDLANES = J
      IF(INDEX .GT. LASTEVENT) LASTEVENT = INDEX
      EXIT
    ENDIF
  ENDDO
  RETURN
  END
       
! ==================================================================================================
  SUBROUTINE ADDSTREETEVENT(IL, START, DUR, INCTYP, LANES, DIST, ILENGTH, REACTPOINT, RUBBERNECKF)
  USE STREET_LINKS
  USE GLOBAL_DATA
  USE EVENTS
  IMPLICIT NONE
  INTEGER :: IL, START, DUR, INCTYP, IUP, IDN, LANES(N_STREET_LANES)
  INTEGER :: DIST, ILENGTH, REACTPOINT, RUBBERNECKF, INDEX
  IUP = SUSN(IL)
  IDN = SDSN(IL)
  ICOUNT = ICOUNT + 1
  DO INDEX = 1, NUMBER_OF_EVENTS
 
! --- Find the first empty slot in the ALLEVENTS array.
 
    IF(ALLEVENTS(INDEX).INCIDENTID .EQ. 0) THEN
      ALLEVENTS(INDEX).INCIDENTID = ICOUNT
      ALLEVENTS(INDEX).LINKID = IUP * 10000 + IDN
      ALLEVENTS(INDEX).INCIDENTTYPE = INCTYP
      ALLEVENTS(INDEX).INCIDENTPOSITION = DIST
      ALLEVENTS(INDEX).STARTTIME = START
      ALLEVENTS(INDEX).DURATION = DUR
      ALLEVENTS(INDEX).INCIDENTLENGTH = ILENGTH
      ALLEVENTS(INDEX).REACTIONPOINT = REACTPOINT
      ALLEVENTS(INDEX).RUBBERNECKFACTOR = RUBBERNECKF
      ALLEVENTS(INDEX).MSTATE = 1
      ALLEVENTS(INDEX).NUMAFFECTEDLANES = 1
      ALLEVENTS(INDEX).AFFECTEDLANEIDARRAY(1) = LANES(1)
      ALLEVENTS(INDEX).LANEINCIDENTCODES(1) = 2
      ALLEVENTS(INDEX).MODELTYPE = 3
 
      IF(INDEX .GT. LASTEVENT) LASTEVENT = INDEX
      EXIT
    ENDIF
  ENDDO
  RETURN
  END

#ifdef TSIS_COMPATIBLE
!
!*******************************************************************************
!  Time Interval Data Processing
!
!  Based on code developed by ITT Industries
!  Original Author: Dan Tomich
!*******************************************************************************
!
! ==================================================================================================
  SUBROUTINE SENDTIMEINTERVALDATA
! ----------------------------------------------------------------------
! --- Loads the time interval data structure for the TxD writer.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  USE STREET_LINKS
  USE SIMPARAMS
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER :: TINT, IL, IUP, IDN
! ----------------------------------------------------------------------
  
! --- Set the interval index based on the current time and the length
! --- of the interval. Process all internal links.
 
  TINT = SIMTIME / TIME_INTERVAL
 
  DO IL = 1, N_STREET_LINKS
    IUP = SUSN(IL)
    IDN = SDSN(IL)
    IF(NODE_TYPE(IUP) .NE. NT_EXTERN .AND. NODE_TYPE(IDN) .NE. NT_EXTERN) CALL SETTIDDATA(3, IUP, IDN, TINT)
  ENDDO

  DO IL = 1, N_FREEWAY_LINKS
    IUP = FUSN(IL)
    IDN = FDSN(IL)
    IF(NODE_TYPE(IUP) .NE. NT_EXTERN .AND. NODE_TYPE(IDN) .NE. NT_EXTERN) CALL SETTIDDATA(8, IUP, IDN, TINT)
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE SETTIDDATA(MTYPE, USN, DSN, TINT)
! ----------------------------------------------------------------------
! --- Loads the time interval data structure for the TxD writer.
! ----------------------------------------------------------------------
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: MTYPE, USN, DSN, TINT
  INCLUDE 'CORWIN.FI'
  INCLUDE 'TXDINTERFACE.FI'
  INCLUDE 'MOEFUNCTIONS.FI'

  CHARACTER*80 CMOE
  INTEGER :: I, IRET, IARRAY(3), LINKID
  REAL :: RMOE
! ----------------------------------------------------------------------

! --- Set the node numbers for use in the MOE calculation functions.
 
  IARRAY(1) = USN
  IARRAY(2) = DSN
  IARRAY(3) = 0
 
! --- Initialize the TID data structure and load the link ID and
! --- interval ID into the structure.
 
  LINKID = 10000 * USN + DSN
  CALL INIT_TIDDAT(LINKID, TINT)
 
  IF(MTYPE .EQ. 3) THEN
!
!       ----------------------------------------------------------------
!       Link MOE for Buses
!
    CMOE = 'BusDelayTotal'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.BusDelayTotal = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.BusDelayTotal_Cum = RMOE

    CMOE = 'BusMoveTimePerTravelTimeRatio'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.BusMoveTimePerTravelTimeRatio = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.BusMoveTimePerTravelTimeRatio_Cum = RMOE

    CMOE = 'BusPersonTrips'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.BusPersonTrips = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.BusPersonTrips_Cum = RMOE

    CMOE = 'BusSpeedAverage'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.BusSpeedAverage = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.BusSpeedAverage_Cum = RMOE

    CMOE = 'BusTravelTimeTotal'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.BusTravelTimeTotal = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.BusTravelTimeTotal_Cum = RMOE

    CMOE = 'BusTrips'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.BusTrips = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.BusTrips_Cum = RMOE

    CMOE = 'BusesThatStopped'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.BusesThatStopped = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.BusesThatStopped_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'ContentAverage'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.ContentAverage = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.ContentAverage_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'ContentCurrent'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.ContentCurrent = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.ContentCurrent_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'DelayControlPerVehicle'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayControlPerVehicle = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayControlPerVehicle_Cum = RMOE

    CMOE = 'DelayControlPerVehicleLeft'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayControlPerVehicleLeft = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayControlPerVehicleLeft_Cum = RMOE

    CMOE = 'DelayControlPerVehicleRight'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayControlPerVehicleRight = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayControlPerVehicleRight_Cum = RMOE

    CMOE = 'DelayControlPerVehicleThrough'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayControlPerVehicleThrough = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayControlPerVehicleThrough_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'DelayControlTotal'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayControlTotal = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayControlTotal_Cum = RMOE

    CMOE = 'DelayControlTotalLeft'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayControlTotalLeft = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayControlTotalLeft_Cum = RMOE

    CMOE = 'DelayControlTotalRight'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayControlTotalRight = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayControlTotalRight_Cum = RMOE

    CMOE = 'DelayControlTotalThrough'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayControlTotalThrough = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayControlTotalThrough_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'DelayQueuePerVehicle'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayQueuePerVehicle = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayQueuePerVehicle_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'DelayQueueTotal'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayQueueTotal = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayQueueTotal_Cum = RMOE

    CMOE = 'DelayQueueTotalLeft'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayQueueTotalLeft = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayQueueTotalLeft_Cum = RMOE

    CMOE = 'DelayQueueTotalRight'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayQueueTotalRight = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayQueueTotalRight_Cum = RMOE

    CMOE = 'DelayQueueTotalThrough'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayQueueTotalThrough = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayQueueTotalThrough_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'DelayStopPerVehicle'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayStopPerVehicle = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayStopPerVehicle_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'DelayStopTotal'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayStopTotal = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayStopTotal_Cum = RMOE

    CMOE = 'DelayStopTotalLeft'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayStopTotalLeft = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayStopTotalLeft_Cum = RMOE

    CMOE = 'DelayStopTotalRight'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayStopTotalRight = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayStopTotalRight_Cum = RMOE

    CMOE = 'DelayStopTotalThrough'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayStopTotalThrough = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayStopTotalThrough_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'DelayTravelPerVehicle'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayTravelPerVehicle = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayTravelPerVehicle_Cum = RMOE

    CMOE = 'DelayTravelPerVehicleLeft'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayTravelPerVehicleLeft = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayTravelPerVehicleLeft_Cum = RMOE

    CMOE = 'DelayTravelPerVehicleRight'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayTravelPerVehicleRight = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayTravelPerVehicleRight_Cum = RMOE

    CMOE = 'DelayTravelPerVehicleThrough'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayTravelPerVehicleThrough = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayTravelPerVehicleThrough_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'DelayTravelTotal'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayTravelTotal = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayTravelTotal_Cum = RMOE

    CMOE = 'DelayTravelTotalLeft'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayTravelTotalLeft = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayTravelTotalLeft_Cum = RMOE

    CMOE = 'DelayTravelTotalRight'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayTravelTotalRight = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayTravelTotalRight_Cum = RMOE

    CMOE = 'DelayTravelTotalThrough'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayTravelTotalThrough = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayTravelTotalThrough_Cum = RMOE
 
!!       ----------------------------------------------------------------
!!       Emission Rates
! 
!    CMOE = 'EmissionsRateCO'
!    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
!    theTIDData.EmissionsRateCO = RMOE
!    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
!    theTIDData.EmissionsRateCO_Cum = RMOE
!
!    CMOE = 'EmissionsRateHC'
!    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
!    theTIDData.EmissionsRateHC = RMOE
!    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
!    theTIDData.EmissionsRateHC_Cum = RMOE
!
!    CMOE = 'EmissionsRateNOx'
!    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
!    theTIDData.EmissionsRateNOx = RMOE
!    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
!    theTIDData.EmissionsRateNOx_Cum = RMOE
! 
!       ----------------------------------------------------------------
!       Total Emissions

    CMOE = 'EmissionsTotalCO'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.EmissionsTotalCO = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.EmissionsTotalCO_Cum = RMOE
!
!    CMOE = 'EmissionsTotalHC'
!    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
!    theTIDData.EmissionsTotalHC = RMOE
!    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
!    theTIDData.EmissionsTotalHC_Cum = RMOE
!
!    CMOE = 'EmissionsTotalNOx'
!    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
!    theTIDData.EmissionsTotalNOx = RMOE
!    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
!    theTIDData.EmissionsTotalNOx_Cum = RMOE
! 
!!       ----------------------------------------------------------------
!!       Fuel Consumption By Fleet
! 
!    CMOE = 'FuelConsumptionTotal'
!    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
!    theTIDData.FuelConsumptionTotal = RMOE
!    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
!    theTIDData.FuelConsumptionTotal_Cum = RMOE
!
!    CMOE = 'FuelConsumption'
!    IARRAY(3) = 1
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
!    theTIDData.FuelConsumptionTotalAutos = RMOE
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
!    theTIDData.FuelConsumptionTotalAutos_Cum = RMOE
!
!    CMOE = 'FuelConsumption'
!    IARRAY(3) = 2
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
!    theTIDData.FuelConsumptionTotalTrucks = RMOE
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
!    theTIDData.FuelConsumptionTotalTrucks_Cum = RMOE
!
!    CMOE = 'FuelConsumption'
!    IARRAY(3) = 3
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
!    theTIDData.FuelConsumptionTotalBuses = RMOE
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
!    theTIDData.FuelConsumptionTotalBuses_Cum = RMOE
!
!    CMOE = 'FuelConsumption'
!    IARRAY(3) = 4
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
!    theTIDData.FuelConsumptionTotalCarpools = RMOE
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
!    theTIDData.FuelConsumptionTotalCarpools_Cum = RMOE
!    IARRAY(3) = 0
 
!       ----------------------------------------------------------------
    CMOE = 'LaneChangesTotal'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.LaneChangesTotal = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.LaneChangesTotal_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'MoveTimePerTravelTimeRatio'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.MoveTimePerTravelTimeRatio = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.MoveTimePerTravelTimeRatio_Cum = RMOE

    CMOE = 'MoveTimePerTravelTimeRatioLeft'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.MoveTimePerTravelTimeRatioLeft = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.MoveTimePerTravelTimeRatioLeft_Cum = RMOE

    CMOE = 'MoveTimePerTravelTimeRatioRight'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.MoveTimePerTravelTimeRatioRight = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.MoveTimePerTravelTimeRatioRight_Cum = RMOE

    CMOE = 'MoveTimePerTravelTimeRatioThrough'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.MoveTimePerTravelTimeRatioThrough = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.MoveTimePerTravelTimeRatioThrough_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'MoveTimeTotal'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.MoveTimeTotal = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.MoveTimeTotal_Cum = RMOE

    CMOE = 'MoveTimeTotalLeft'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.MoveTimeTotalLeft = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.MoveTimeTotalLeft_Cum = RMOE

    CMOE = 'MoveTimeTotalRight'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.MoveTimeTotalRight = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.MoveTimeTotalRight_Cum = RMOE

    CMOE = 'MoveTimeTotalThrough'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.MoveTimeTotalThrough = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.MoveTimeTotalThrough_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'PersonDelayTotal'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.PersonDelayTotal = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.PersonDelayTotal_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'PersonTripsTotal'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.PersonTripsTotal = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.PersonTripsTotal_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'PhaseFailuresTotal'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.PhaseFailuresTotal = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.PhaseFailuresTotal_Cum = RMOE
 
!       ----------------------------------------------------------------
!       Vehicles In Queue By Lane
 
    DO I = 1, 7
      IARRAY(3) = I

      CMOE = 'QueueAverageNumberVehicles'
      IRET = GETSTREETLANEMOEDATA(CMOE, IARRAY, 1, RMOE)
      theTIDData.QueueAverageNumberVehiclesSLT.Array(I) = RMOE
      IRET = GETSTREETLANEMOEDATA(CMOE, IARRAY, 0, RMOE)
      theTIDData.QueueAverageNumberVehiclesSLT_Cum.Array(I) = RMOE

      CMOE = 'QueueMaximumNumberVehicles'
      IRET = GETSTREETLANEMOEDATA(CMOE, IARRAY, 1, RMOE)
      theTIDData.QueueMaximumNumberVehiclesSLT.Array(I) = RMOE
      IRET = GETSTREETLANEMOEDATA(CMOE, IARRAY, 0, RMOE)
      theTIDData.QueueMaximumNumberVehiclesSLT_Cum.Array(I) = RMOE

    ENDDO
    theTIDData.QueueAverageNumberVehiclesSLT.Count = 7
    theTIDData.QueueAverageNumberVehiclesSLT_Cum.Count = 7
    theTIDData.QueueMaximumNumberVehiclesSLT.Count = 7
    theTIDData.QueueMaximumNumberVehiclesSLT_Cum.Count = 7
    IARRAY(3) = 0
 
!       ----------------------------------------------------------------
    CMOE = 'SpeedAverage'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.SpeedAverage = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.SpeedAverage_Cum = RMOE

    CMOE = 'SpeedAverageLeft'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.SpeedAverageLeft = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.SpeedAverageLeft_Cum = RMOE

    CMOE = 'SpeedAverageRight'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.SpeedAverageRight = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.SpeedAverageRight_Cum = RMOE

    CMOE = 'SpeedAverageThrough'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.SpeedAverageThrough = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.SpeedAverageThrough_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'StoppedVehicles'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.StoppedVehicles = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.StoppedVehicles_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'StoppedVehiclesPercent'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.StoppedVehiclesPercent = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.StoppedVehiclesPercent_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'StoragePercent'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.StoragePercent = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.StoragePercent_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'TravelDistanceTotal'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.TravelDistanceTotal = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.TravelDistanceTotal_Cum = RMOE

    CMOE = 'TravelDistanceTotalLeft'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.TravelDistanceTotalLeft = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.TravelDistanceTotalLeft_Cum = RMOE

    CMOE = 'TravelDistanceTotalRight'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.TravelDistanceTotalRight = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.TravelDistanceTotalRight_Cum = RMOE

    CMOE = 'TravelDistanceTotalThrough'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.TravelDistanceTotalThrough = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.TravelDistanceTotalThrough_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'TravelTimePerVehicle'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.TravelTimePerVehicle = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.TravelTimePerVehicle_Cum = RMOE

    CMOE = 'TravelTimePerVehicleLeft'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.TravelTimePerVehicleLeft = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.TravelTimePerVehicleLeft_Cum = RMOE

    CMOE = 'TravelTimePerVehicleRight'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.TravelTimePerVehicleRight = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.TravelTimePerVehicleRight_Cum = RMOE

    CMOE = 'TravelTimePerVehicleThrough'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.TravelTimePerVehicleThrough = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.TravelTimePerVehicleThrough_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'TravelTimeTotal'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.TravelTimeTotal = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.TravelTimeTotal_Cum = RMOE

    CMOE = 'TravelTimeTotalLeft'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.TravelTimeTotalLeft = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.TravelTimeTotalLeft_Cum = RMOE

    CMOE = 'TravelTimeTotalRight'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.TravelTimeTotalRight = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.TravelTimeTotalRight_Cum = RMOE

    CMOE = 'TravelTimeTotalThrough'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.TravelTimeTotalThrough = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.TravelTimeTotalThrough_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'Trips'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.Trips = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.Trips_Cum = RMOE

    CMOE = 'TripsLeft'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.TripsLeft = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.TripsLeft_Cum = RMOE

    CMOE = 'TripsRight'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.TripsRight = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.TripsRight_Cum = RMOE

    CMOE = 'TripsThrough'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.TripsThrough = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.TripsThrough_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'VehiclesDischarged'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.VehiclesDischarged = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.VehiclesDischarged_Cum = RMOE

    CMOE = 'VehiclesDischargedLeft'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.VehiclesDischargedLeft = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.VehiclesDischargedLeft_Cum = RMOE

    CMOE = 'VehiclesDischargedRight'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.VehiclesDischargedRight = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.VehiclesDischargedRight_Cum = RMOE

    CMOE = 'VehiclesDischargedThrough'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.VehiclesDischargedThrough = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.VehiclesDischargedThrough_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'Volume'
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.Volume = RMOE
    IRET = GETSTREETLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.Volume_Cum = RMOE
 
  ELSE
 
!       ----------------------------------------------------------------
!       Link MOE for Buses
 
    CMOE = 'BusDelayTotal'
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.BusDelayTotal = RMOE
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.BusDelayTotal_Cum = RMOE

    CMOE = 'BusMoveTimePerTravelTimeRatio'
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.BusMoveTimePerTravelTimeRatio = RMOE
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.BusMoveTimePerTravelTimeRatio_Cum = RMOE

    CMOE = 'BusPersonTrips'
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.BusPersonTrips = RMOE
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.BusPersonTrips_Cum = RMOE

    CMOE = 'BusSpeedAverage'
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.BusSpeedAverage = RMOE
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.BusSpeedAverage_Cum = RMOE

    CMOE = 'BusTravelTimeTotal'
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.BusTravelTimeTotal = RMOE
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.BusTravelTimeTotal_Cum = RMOE

    CMOE = 'BusTrips'
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.BusTrips = RMOE
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.BusTrips_Cum = RMOE

!    CMOE = 'BusesThatStopped'
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
!    theTIDData.BusesThatStopped = RMOE
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
!    theTIDData.BusesThatStopped_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'ContentAverage'
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.ContentAverage = RMOE
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.ContentAverage_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'ContentCurrent'
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.ContentCurrent = RMOE
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.ContentCurrent_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'DelayTravelPerVehicle'
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayTravelPerVehicle = RMOE
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayTravelPerVehicle_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'DelayTravelTotal'
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DelayTravelTotal = RMOE
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DelayTravelTotal_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'DensityPerLane'
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.DensityPerLane = RMOE
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.DensityPerLane_Cum = RMOE
 
!!       ----------------------------------------------------------------
!!       Emission Rates
! 
!    CMOE = 'EmissionsRateCO'
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
!    theTIDData.EmissionsRateCO = RMOE
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
!    theTIDData.EmissionsRateCO_Cum = RMOE
!
!    CMOE = 'EmissionsRateHC'
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
!    theTIDData.EmissionsRateHC = RMOE
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
!    theTIDData.EmissionsRateHC_Cum = RMOE
!
!    CMOE = 'EmissionsRateNOx'
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
!    theTIDData.EmissionsRateNOx = RMOE
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
!    theTIDData.EmissionsRateNOx_Cum = RMOE
! 
!       ----------------------------------------------------------------
!       Total Emissions
 
    CMOE = 'EmissionsTotalCO'
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.EmissionsTotalCO = RMOE
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.EmissionsTotalCO_Cum = RMOE
!
!    CMOE = 'EmissionsTotalHC'
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
!    theTIDData.EmissionsTotalHC = RMOE
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
!    theTIDData.EmissionsTotalHC_Cum = RMOE
!
!    CMOE = 'EmissionsTotalNOx'
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
!    theTIDData.EmissionsTotalNOx = RMOE
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
!    theTIDData.EmissionsTotalNOx_Cum = RMOE
! 
!!       ----------------------------------------------------------------
!!       Fuel Consumption By Fleet
! 
!    CMOE = 'FuelConsumptionTotal'
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
!    theTIDData.FuelConsumptionTotal = RMOE
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
!    theTIDData.FuelConsumptionTotal_Cum = RMOE
!
!    CMOE = 'FuelConsumption'
!    IARRAY(3) = 1
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
!    theTIDData.FuelConsumptionTotalAutos = RMOE
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
!    theTIDData.FuelConsumptionTotalAutos_Cum = RMOE
!
!    CMOE = 'FuelConsumption'
!    IARRAY(3) = 2
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
!    theTIDData.FuelConsumptionTotalTrucks = RMOE
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
!    theTIDData.FuelConsumptionTotalTrucks_Cum = RMOE
!
!    CMOE = 'FuelConsumption'
!    IARRAY(3) = 3
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
!    theTIDData.FuelConsumptionTotalBuses = RMOE
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
!    theTIDData.FuelConsumptionTotalBuses_Cum = RMOE
!
!    CMOE = 'FuelConsumption'
!    IARRAY(3) = 4
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
!    theTIDData.FuelConsumptionTotalCarpools = RMOE
!    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
!    theTIDData.FuelConsumptionTotalCarpools_Cum = RMOE
!    IARRAY(3) = 0
 
!       ----------------------------------------------------------------
    CMOE = 'LaneChangesTotal'
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.LaneChangesTotal = RMOE
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.LaneChangesTotal_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'MoveTimePerTravelTimeRatio'
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.MoveTimePerTravelTimeRatio = RMOE
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.MoveTimePerTravelTimeRatio_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'MoveTimeTotal'
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.MoveTimeTotal = RMOE
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.MoveTimeTotal_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'SpeedAverage'
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.SpeedAverage = RMOE
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.SpeedAverage_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'TravelDistanceTotal'
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.TravelDistanceTotal = RMOE
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.TravelDistanceTotal_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'TravelTimePerVehicle'
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.TravelTimePerVehicle = RMOE
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.TravelTimePerVehicle_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'TravelTimeTotal'
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.TravelTimeTotal = RMOE
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.TravelTimeTotal_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'VehiclesDischarged'
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.VehiclesDischarged = RMOE
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.VehiclesDischarged_Cum = RMOE
 
!       ----------------------------------------------------------------
    CMOE = 'VolumePerLane'
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 1, RMOE)
    theTIDData.VolumePerLane = RMOE
    IRET = GETFREEWAYLINKMOEDATA(CMOE, IARRAY, 0, RMOE)
    theTIDData.VolumePerLane_Cum = RMOE
 
  ENDIF
 
!     Inform the driver that the link MOE are available for
!     this interval.
 
  CALL SENDMSG(SIM_LINKTIDVALID, 0)
  RETURN
  END
#endif  
