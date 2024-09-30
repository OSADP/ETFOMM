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
  SUBROUTINE EMIT_FREEWAY_VEHICLE(THIS_VEHICLE, GOOD)
! ----------------------------------------------------------------------      
! --- Insert a vehicle into the network from an entry node.
! ---------------------------------------------------------------------- 
  USE GLOBAL_DATA  
  USE SIMPARAMS
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE ENTRYNODE_DATA
  USE VEHICLE_MOD
  USE VEHICLE_TYPES
  USE SEEDS
  USE CAR_FOLLOWING
  IMPLICIT NONE
  TYPE(VEHICLE), INTENT(INOUT) :: THIS_VEHICLE
  LOGICAL, INTENT(OUT) :: GOOD
  INTEGER :: IV, I, ILD, IFLT, ITYPE, CFM
  REAL :: RNDNUM, ACCEL
  LOGICAL :: WBAD, IW
  COMMON /GRAPHIO/ TSTEPFILE, TINTERVALFILE
  INTEGER :: TSTEPFILE, TINTERVALFILE
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
#endif
 
! --- Find the first available index into the vehicle arrays.
 
  CALL GET_NEXT_FINDEX(IV)
  CALL DELETE_FREEWAY_VEHICLE(IV)
  FLINK(IV) = THIS_VEHICLE%ENTRYLINKID
  FID(IV) = THIS_VEHICLE%GLOBALID
#ifdef DebugVersion
  temp = fid(iv)
#endif
  FEV_OVRSPD(IV) = THIS_VEHICLE%OVRSPD
  FEV_RANGE(IV) = THIS_VEHICLE%RANGE
  !FLC_TIMER(IV) = LC_TIME_FREEWAY
  DISTANCE_TO_SEGMENT_END(IV) = USN_TO_SEG_END(FTHRU_LINK(FLINK(IV)))
 
! --- Add the vehicle to the end of the sorted list.
      
  ISEGMENT(IV) = SEGMENT(FLINK(IV))
  CALL INSERT_VEHICLE(IV)
  CALL SET_NEXT_OBJECT(IV)
  IF(THIS_VEHICLE%ROUTEID .NE. 0) THEN
    CALL BUS_PROPERTIES_FREEWAY(IFLT, ITYPE)
    FFLEET(IV) = IFLT
    FVTYPE(IV) = ITYPE
    FVLENGTH(IV) = VTLENGTH(ITYPE)
    CALL FREEWAY_RANDOM(FSEED, RNDNUM)
    CALL GET_CAR_FOLLOWING_MODEL(ITYPE, RNDNUM, CFM)
    FCF_MODEL(IV) = CFM
  ELSEIF(THIS_VEHICLE%PATHID .NE. 0) THEN
    FFLEET(IV) = THIS_VEHICLE%FLEET
    FVTYPE(IV) = THIS_VEHICLE%VTYPE
    FVLENGTH(IV) = VTLENGTH(FVTYPE(IV))
    FCF_MODEL(IV) = THIS_VEHICLE%CFM
  ELSE
    CALL VEHICLE_PROPERTIES_FREEWAY(FLINK(IV), IFLT, ITYPE)
    FFLEET(IV) = IFLT
    FVTYPE(IV) = ITYPE
    FVLENGTH(IV) = VTLENGTH(ITYPE)
    CALL FREEWAY_RANDOM(FSEED, RNDNUM)
    IF(RNDNUM .LE. HOV_VIOLATOR_PCT(FUSN(FLINK(IV)))) HOV_VIOLATOR(IV) = .TRUE.
    CALL FREEWAY_RANDOM(FSEED, RNDNUM)
    CALL GET_CAR_FOLLOWING_MODEL(ITYPE, RNDNUM, CFM)
    FCF_MODEL(IV) = CFM
  ENDIF
  FLANECODES(IV, 1:N_FREEWAY_LANES) = LC_NULL
 
! --- Check for lane restrictions.
 
  DO I = 1, FNUMLANES(FLINK(IV))
    FLANECODES(IV, I) = LC_GOOD
    IF(TRUCK_CODE(FLINK(IV)) .GT. TRK_BIASED) THEN
      CALL CHECK_TRUCK_LANES(IV, FLINK(IV), I, WBAD)
      IF(WBAD) FLANECODES(IV, I) = LC_EXCLUDED
    ENDIF
    IF(FXCLUDE_TYPE(FLINK(IV), I, FVTYPE(IV))) FLANECODES(IV, I) = LC_EXCLUDED
  ENDDO
 
! --- Determine a lane to enter, if possible.
     
  FENTRYTIME(IV) = THIS_VEHICLE%DEPARTURE
  CALL FREEWAY_CHOOSE_ENTRY_LANE(IV, ILD)
  IF(FLANE(IV) .NE. 0) THEN
    FTURNCODE(IV) = TC_THRU
    FPATHID(IV) = THIS_VEHICLE%PATHID
    FROUTEID(IV) = THIS_VEHICLE%ROUTEID
    IF(FPATHID(IV) .NE. 0 .OR. FROUTEID(IV) .NE. 0) FPATHPOINT(IV) = 1
    CALL CHOOSE_DESTINATION(IV)
    IF(THIS_VEHICLE%DRIVER .EQ. 0) THEN
      CALL DETERMINE_PROPERTIES_FREEWAY(FFLEET(IV), FDRIVERTYPE(IV), IW)
    ELSE
      FDRIVERTYPE(IV) = THIS_VEHICLE%DRIVER
      FFLEET(IV) = THIS_VEHICLE%FLEET 
      FVTYPE(IV) = THIS_VEHICLE%VTYPE
      FPATHID(IV) = THIS_VEHICLE%PATHID
      FROUTEID(IV) = THIS_VEHICLE%ROUTEID
      CALL FREEWAY_RANDOM(SSEED, RNDNUM)
      IW = .FALSE.
      IF(RNDNUM .LE. FREEWAY_PCT_COOP) IW = .TRUE.
    ENDIF
    FWILL_COOP_LC(IV) = IW
    CALL SET_DESIREDSPEED_FREEWAY(IV)
    FSPEED(IV) = FDESIREDSPEED(IV)
    IF(ILD .NE. 0) THEN
      ACCEL = 0.
      CALL FREEWAY_CAR_FOLLOW(IV, ILD, ACCEL)
      IF(ACCEL .LT. 0.0) THEN
 
! --- The vehicle must enter at a lower speed.
      
        FSPEED(IV) = FSPEED(ILD)
      ENDIF
    ENDIF
    FLOCATION(IV) = -(TIMESTEP - MOD(FENTRYTIME(IV), TIMESTEP)) * FSPEED(IV)
    FLEADER(IV) = ILD
    IF(ILD .NE. 0) FFOLLOWER(ILD) = IV
    FENTRY_LINK(IV) = FLINK(IV)
    GOOD = .TRUE.
    THIS_VEHICLE%DRIVER = FDRIVERTYPE(IV)
    THIS_VEHICLE%FLEET = FFLEET(IV)
    THIS_VEHICLE%VTYPE = FVTYPE(IV)
    IF(TSTEPFILE .EQ. 2) CALL WRITE_VEHICLE_LIST(THIS_VEHICLE, FUSN(FLINK(IV)), FVLENGTH(IV)) 
    IF(FLAST_VEHICLE(FLINK(IV), FLANE(IV)) .NE. 0) THEN
      FLEADER(IV) = FLAST_VEHICLE(FLINK(IV), FLANE(IV)) 
    ENDIF
    FLAST_VEHICLE(FLINK(IV), FLANE(IV)) = IV
    IF(WRITE_SUPPLEMENTAL_FILES) CALL WRITE_VEHICLE_ENTRY_TEXT(FID(IV), FUSN(FLINK(IV)))

  ELSE
 
! --- The vehicle is unable to enter at this time.
! --- Remove it from the sorted list.
      
    GOOD = .FALSE.
    LAST_ID_USED = LAST_ID_USED - 1
    FLINK(IV) = 0
    FID(IV) = 0
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE EMIT_STREET_VEHICLE(THIS_VEHICLE, GOOD)
! ----------------------------------------------------------------------      
! --- Insert a vehicle into the network from an entry node.
! ----------------------------------------------------------------------      
  USE SIMPARAMS
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE ENTRYNODE_DATA
  USE VEHICLE_MOD
  USE VEHICLE_TYPES
  USE GLOBAL_DATA
  USE SEEDS
  USE CAR_FOLLOWING
  IMPLICIT NONE
  TYPE(VEHICLE), INTENT(INOUT) :: THIS_VEHICLE
  LOGICAL, INTENT(OUT) :: GOOD
  INTEGER :: IV, I, ILD, IFLT, ITYPE, CFM
  REAL :: ACCEL, RNDNUM
  LOGICAL :: IW
  COMMON /GRAPHIO/ TSTEPFILE, TINTERVALFILE
  INTEGER :: TSTEPFILE, TINTERVALFILE
  include 'iofiles.inc'
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
#endif

! --- Find the first available index into the vehicle arrays.
 
  CALL GET_NEXT_SINDEX(IV)
  CALL DELETE_STREET_VEHICLE(IV, .FALSE.)
  SLINK(IV) = THIS_VEHICLE%ENTRYLINKID
  SID(IV) = THIS_VEHICLE%GLOBALID
  
#ifdef DebugVersion
  temp = sid(iv)
#endif
  !SLC_TIMER(IV) = LC_TIME_STREET
  SEV_OVRSPD(IV) = THIS_VEHICLE%OVRSPD
  SEV_RANGE(IV) = THIS_VEHICLE%RANGE
  IF(THIS_VEHICLE%ROUTEID .NE. 0) THEN
    NEXT_STOP(IV) = 1 
    CALL BUS_PROPERTIES_STREET(IFLT, ITYPE)
    SFLEET(IV) = IFLT
    SVTYPE(IV) = ITYPE
    SVLENGTH(IV) = VTLENGTH(ITYPE)
    CALL STREET_RANDOM(SSEED, RNDNUM)
    CALL GET_CAR_FOLLOWING_MODEL(ITYPE, RNDNUM, CFM)
    SCF_MODEL(IV) = CFM
  ELSEIF(THIS_VEHICLE%PATHID .NE. 0) THEN
    SFLEET(IV) = THIS_VEHICLE%FLEET
    SVTYPE(IV) = THIS_VEHICLE%VTYPE
    SVLENGTH(IV) = VTLENGTH(SVTYPE(IV))
    SCF_MODEL(IV) = THIS_VEHICLE%CFM
  ELSE
    CALL VEHICLE_PROPERTIES_STREET(SLINK(IV), IFLT, ITYPE)
    SFLEET(IV) = IFLT
    SVTYPE(IV) = ITYPE
    SVLENGTH(IV) = VTLENGTH(ITYPE)
    CALL STREET_RANDOM(SSEED, RNDNUM)
    CALL GET_CAR_FOLLOWING_MODEL(ITYPE, RNDNUM, CFM)
    SCF_MODEL(IV) = CFM
  ENDIF
  SLANECODES(IV, 1:N_STREET_LANES) = LC_NULL
 
! --- Check for lane restrictions.
 
  DO I = FIRST_FULL_LANE(SLINK(IV)), LAST_FULL_LANE(SLINK(IV))
    SLANECODES(IV, I) = LC_GOOD
    IF(SXCLUDE_TYPE(SLINK(IV), I, SVTYPE(IV))) SLANECODES(IV, I) = LC_EXCLUDED
  ENDDO
 
! --- Determine a lane to enter, if possible.
      
  SENTRYTIME(IV) = THIS_VEHICLE%DEPARTURE
  CALL STREET_CHOOSE_ENTRY_LANE(IV, ILD)
  IF(SLANE(IV) .NE. 0) THEN
    STURNCODE(IV) = TC_THRU
    TURN_INDICATOR(IV) = TC_THRU
    IF(THIS_VEHICLE%DRIVER .EQ. 0) THEN
      CALL DETERMINE_PROPERTIES_STREET(SFLEET(IV), SDRIVERTYPE(IV), IW)
    ELSE
      SDRIVERTYPE(IV) = THIS_VEHICLE%DRIVER
      SFLEET(IV) = THIS_VEHICLE%FLEET 
      SVTYPE(IV) = THIS_VEHICLE%VTYPE
      SPATHID(IV) = THIS_VEHICLE%PATHID
      SROUTEID(IV) = THIS_VEHICLE%ROUTEID
      CALL STREET_RANDOM(SSEED, RNDNUM)
      IW = .FALSE.
      IF(RNDNUM .LE. STREET_PCT_COOP) IW = .TRUE.
    ENDIF
    DRIVER_TYPE(IV) = SDRIVERTYPE(IV)
    SWILL_COOP_LC(IV) = IW
    IF(SPATHID(IV) .NE. 0 .OR. SROUTEID(IV) .NE. 0) SPATHPOINT(IV) = 1
    CALL SET_DESIREDSPEED_STREET(IV)
    SSPEED(IV) = SDESIREDSPEED(IV)
    IF(ILD .NE. 0) THEN
      ACCEL = 0.
      CALL STREET_CAR_FOLLOW(IV, ILD, ACCEL)
      IF(ACCEL .LT. 0.0) THEN
 
! --- The vehicle must enter at a lower speed.
      
        SSPEED(IV) = SSPEED(ILD)
      ENDIF
    ENDIF
    SLOCATION(IV) = -(TIMESTEP - MOD(SENTRYTIME(IV), TIMESTEP)) * SSPEED(IV)
    TURN_CODE2(IV) = TC_NULL
    CALL FIND_NEXT_TURN(IV)
    IF(FIRST_VEHICLE(SLINK(IV), SLANE(IV)) .EQ. 0) FIRST_VEHICLE(SLINK(IV), SLANE(IV)) = IV
    SENTRY_LINK(IV) = SLINK(IV)
    GOOD = .TRUE.
    THIS_VEHICLE%DRIVER = SDRIVERTYPE(IV)
    THIS_VEHICLE%FLEET = SFLEET(IV)
    THIS_VEHICLE%VTYPE = SVTYPE(IV)
    IF(TSTEPFILE .EQ. 2) CALL WRITE_VEHICLE_LIST(THIS_VEHICLE, SUSN(SLINK(IV)), SVLENGTH(IV))      
    IF(SLAST_VEHICLE(SLINK(IV), SLANE(IV)) .NE. 0) THEN
      SLEADER(IV) = SLAST_VEHICLE(SLINK(IV), SLANE(IV)) 
    ENDIF
    SLAST_VEHICLE(SLINK(IV), SLANE(IV)) = IV
    IF(WRITE_SUPPLEMENTAL_FILES) CALL WRITE_VEHICLE_ENTRY_TEXT(SID(IV), SUSN(SLINK(IV)))
  ELSE
 
! --- The vehicle is unable to enter at this time.
! --- Remove it from the sorted list.
      
    GOOD = .FALSE.
    LAST_ID_USED = LAST_ID_USED - 1
    SLINK(IV) = 0
    SID(IV) = 0
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE SOURCE_VEHICLE(THIS_VEHICLE, GOOD)
! ----------------------------------------------------------------------      
! --- Insert a vehicle into the network from a source node
! --- or process an extraction at a sink node.
! ----------------------------------------------------------------------      
  USE SIMPARAMS
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE ENTRYNODE_DATA
  USE VEHICLE_MOD
  USE GLOBAL_DATA
  USE VEHICLE_TYPES
  USE SEEDS
  USE CAR_FOLLOWING
  IMPLICIT NONE
  TYPE(VEHICLE), INTENT(IN) :: THIS_VEHICLE
  LOGICAL, INTENT(OUT) :: GOOD
  INTEGER :: IV, I, IFLT, ITYPE, CFM
  LOGICAL :: IW
  COMMON /GRAPHIO/ TSTEPFILE, TINTERVALFILE
  INTEGER :: TSTEPFILE, TINTERVALFILE
  REAL :: RNDNUM
! ----------------------------------------------------------------------
 
! --- Find the first available index into the vehicle arrays.
 
  IF(THIS_VEHICLE%NETFLOW .EQ. 0) THEN
    CALL GET_NEXT_SINDEX(IV)
    CALL DELETE_STREET_VEHICLE(IV, .FALSE.)
    SLINK(IV) = THIS_VEHICLE%ENTRYLINKID
    SID(IV) = THIS_VEHICLE%GLOBALID
    IF(CENTROID_NV(SLINK(IV)) .LE. 0) THEN
      SDESIREDSPEED(IV) = SFREEFLOWSPEED(SLINK(IV)) 
      SSPEED(IV) = STOP_SPD
      SPATHID(IV) = THIS_VEHICLE%PATHID
      SROUTEID(IV) = THIS_VEHICLE%ROUTEID
      IF(SPATHID(IV) .NE. 0 .OR. SROUTEID(IV) .NE. 0) SPATHPOINT(IV) = 1
      IF(SROUTEID(IV) .NE. 0) THEN
        NEXT_STOP(IV) = 1 
        CALL BUS_PROPERTIES_STREET(IFLT, ITYPE)
        SFLEET(IV) = IFLT
        SVLENGTH(IV) = VTLENGTH(ITYPE)
        SVTYPE(IV) = ITYPE
      ELSE
        CALL VEHICLE_PROPERTIES_STREET(SLINK(IV), IFLT, ITYPE)
        SFLEET(IV) = IFLT
        SVTYPE(IV) = ITYPE
        SVLENGTH(IV) = VTLENGTH(ITYPE)
      ENDIF
      CALL DETERMINE_PROPERTIES_STREET(SFLEET(IV), SDRIVERTYPE(IV), IW)
      DRIVER_TYPE(IV) = SDRIVERTYPE(IV)
      SWILL_COOP_LC(IV) = IW
      CALL FIND_NEXT_TURN(IV)
      SLOCATION(IV) = SLENGTH(SLINK(IV)) / 2.
      CENTROID_NV(SLINK(IV)) = IV
      SENTRYTIME(IV) = SIMTIME
      SLANECODES(IV, 1:N_STREET_LANES) = LC_NULL
      DO I = FIRST_FULL_LANE(SLINK(IV)), LAST_FULL_LANE(SLINK(IV))
        SLANECODES(IV, I) = LC_GOOD
      ENDDO
      CALL STREET_RANDOM(SSEED, RNDNUM)
      CALL GET_CAR_FOLLOWING_MODEL(ITYPE, RNDNUM, CFM)
      SCF_MODEL(IV) = CFM
      CALL TRANSLATE_STREET_LANECODES(IV, SLINK(IV))
      CALL SET_GOAL_LANE(IV)
      GOOD = .TRUE.
      SOURCE_SINK(SLINK(IV)) = SOURCE_SINK(SLINK(IV)) + 1
      SENTRY_LINK(IV) = SLINK(IV)
      IF(TSTEPFILE .EQ. 2) CALL WRITE_VEHICLE_LIST(THIS_VEHICLE, SUSN(SLINK(IV)), SVLENGTH(IV)) 
    ELSE
 
! --- The vehicle is unable to enter at this time.
! --- Remove it from the sorted list.
       
      GOOD = .FALSE.
      LAST_ID_USED = LAST_ID_USED - 1
      SLINK(IV) = 0
      SID(IV) = 0
    ENDIF
  ELSE
 
! --- This vehicle represents a sink extraction. Increment the
! --- number of vehicles to be extracted.
 
    CENTROID_NV(THIS_VEHICLE%ENTRYLINKID) = CENTROID_NV(THIS_VEHICLE%ENTRYLINKID) - 1
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE ADD_TO_VEHICLE_LIST(IV)
! ----------------------------------------------------------------------
! --- Add a new vehicle to the list, or create the list with the first
! --- vehicle.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
! ----------------------------------------------------------------------
  SORTED_LIST_LENGTH = SORTED_LIST_LENGTH + 1
  SORTED_LIST(SORTED_LIST_LENGTH) = IV
  SORT_POSITION(IV) = SORTED_LIST_LENGTH
  RETURN
  END

! ==================================================================================================
  SUBROUTINE GET_NEXT_FINDEX(INDX)
! ----------------------------------------------------------------------
! --- Find the first available element of the vehicle arrays.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: INDX
  INTEGER :: IV
! ----------------------------------------------------------------------
  INDX = 0
  DO IV = 1, HIGHEST_INDEX_F
    IF(FLINK(IV) .EQ. 0) THEN
      INDX = IV
      EXIT
    ENDIF
  ENDDO
  IF(INDX .EQ. 0) THEN
    INDX = HIGHEST_INDEX_F + 1
    HIGHEST_INDEX_F = INDX
  ENDIF
  IF(INDX .GT. MAX_VEHICLES_F) CALL REALLOCATE_FREEWAY_VEHICLE_ARRAYS
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE GET_NEXT_SINDEX(INDX)
! ----------------------------------------------------------------------
! --- Find the first available element of the vehicle arrays.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: INDX
  INTEGER :: IV
! ----------------------------------------------------------------------
  INDX = 0
  DO IV = 1, HIGHEST_INDEX_S
    IF(SLINK(IV) .EQ. 0) THEN
      INDX = IV
      EXIT
    ENDIF
  ENDDO
  IF(INDX .EQ. 0) THEN
    INDX = HIGHEST_INDEX_S + 1
    HIGHEST_INDEX_S = INDX
  ENDIF
  IF(INDX .GT. MAX_VEHICLES_S) CALL REALLOCATE_STREET_VEHICLE_ARRAYS
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE CHOOSE_DESTINATION(IV)
! ----------------------------------------------------------------------
! --- Determine the vehicle's freeway destination.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  USE FREEWAY_VEHICLES
  USE VEHICLE_TYPES
  USE SIMPARAMS
  USE SEEDS
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER :: ILINK, TEMP
  REAL :: RNDNUM, XPCT
! ----------------------------------------------------------------------
  ILINK = 0
  DESTINATION(IV) = 0
 
! --- Check path following and bus routes.
 
  IF(FPATHID(IV) .NE. 0) THEN
    CALL GETDESTLINK(IV, ILINK)
  ELSEIF(FFLEET(IV) .EQ. FLEET_BUS) THEN
    CALL GETBUSDESTLINK(IV, ILINK)
  ENDIF
  IF(ILINK .GT. 0) THEN
    DESTINATION(IV) = ILINK
    RETURN
  ENDIF
 
! --- Get destinations available from this entry and randomly choose one
! --- based on turn percentages.
 
  ILINK = FLINK(IV)
  DO WHILE(ILINK .NE. 0)
    TEMP = ILINK
    IF(OFFRAMP_LINK(ILINK) .NE. 0) THEN
      CALL FREEWAY_RANDOM(FSEED, RNDNUM)
      XPCT = (100. - FTHRU_PERCENT(ILINK)) * MULTIPLIER_EXIT(ILINK, FVTYPE(IV))
      IF(100 * RNDNUM .LE. XPCT) THEN
        DESTINATION(IV) = OFFRAMP_LINK(ILINK)
        EXIT
      ENDIF
    ENDIF
    ILINK = FTHRU_LINK(ILINK)
    IF(ILINK .NE. 0) THEN
      IF(NODE_TYPE(FUSN(ILINK)) .NE. NT_INTERN) EXIT
    ENDIF
  ENDDO
  IF(DESTINATION(IV) .EQ. 0) DESTINATION(IV) = TEMP
  RETURN
  END

! ==================================================================================================
  SUBROUTINE GETDESTLINK(IV, ILINK)
! ----------------------------------------------------------------------
! --- Determine the next link for a path following vehicle.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  USE PATH_MOD
  USE FREEWAY_VEHICLES
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER, INTENT(OUT) :: ILINK
  INTEGER :: IN, N1, N2, NLINK
! ----------------------------------------------------------------------
  ILINK = 0
 
! --- Start with the current link and then look for the next off-ramp
! --- or the next interface node in the path.
 
! --- The exit may actually be an on-ramp going to another segment      
! --- so choose the first ramp after getting onto the mainline.          
                                                                        
  DO IN = FPATHPOINT(IV), NNODES(FPATHID(IV))-1
    N1 = PATH_NODES(FPATHID(IV), IN)
    N2 = PATH_NODES(FPATHID(IV), IN+1)
    CALL FIND_FREEWAY_LINK(N1, N2, NLINK)
    IF(NLINK .GT. 0) THEN                                           
      IF(LINKTYPE(NLINK) .LT. 0) THEN
        IF(ILINK .EQ. 0) ILINK = NLINK                              
        EXIT                                                      
      ENDIF                                                         
      IF(NODE_TYPE(N2) .NE. NT_INTERN) THEN                                         
        IF(ILINK .EQ. 0) ILINK = NLINK                              
        EXIT                                                      
      ENDIF                                                         
    ENDIF                                                           
  ENDDO
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE GETBUSDESTLINK(IV, ILINK)
! ----------------------------------------------------------------------
! --- Determine the next link for a bus.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  USE FREEWAY_VEHICLES
  USE BUS_ROUTE_DATA
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER, INTENT(OUT) :: ILINK
  INTEGER :: IN, N1, N2, NLINK
! ----------------------------------------------------------------------
  ILINK = 0
 
! --- Start with the current link and then look for the next off-ramp
! --- or the next interface node in the path.
 
! --- The exit may actually be an on-ramp going to another segment      
! --- so choose the first ramp after getting onto the mainline.          
                                                                      
  DO IN = FPATHPOINT(IV), BUSR_NNODES(FROUTEID(IV))-1
    N1 = BUSR_ROUTE_NODES(FROUTEID(IV), IN)
    N2 = BUSR_ROUTE_NODES(FROUTEID(IV), IN+1)
    CALL FIND_FREEWAY_LINK(N1, N2, NLINK)
    IF(NLINK .GT. 0) THEN                                           
      IF(LINKTYPE(NLINK) .LT. 0) THEN
        IF(ILINK .EQ. 0) ILINK = NLINK                              
      ENDIF                                                         
      IF(NODE_TYPE(N2) .NE. NT_INTERN) THEN                                         
        IF(ILINK .EQ. 0) ILINK = NLINK                              
        RETURN                                                      
      ENDIF                                                         
    ENDIF                                                           
  ENDDO
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE FREEWAY_CHOOSE_ENTRY_LANE(IV, ILD)
! ----------------------------------------------------------------------
! --- Determine which lane a vehicle should enter when coming from an
! --- entry node.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  USE FREEWAY_VEHICLES
  USE GLOBAL_DATA
  USE ENTRYNODE_DATA
  USE SIMPARAMS
  USE VEHICLE_TYPES
  USE SEEDS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER, INTENT(OUT) :: ILD
  INTEGER :: I, IL, N, LEAD(5), IPX, ILAST
  REAL :: RNDNUM, HEADWAY(5), HMAX, EPCT
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = fid(iv)
#endif

  FLANE(IV) = 0
  IL = FLINK(IV)
  HEADWAY = 0.
  LEAD = 0
 
! --- Loop over the lanes on the entry link.
      
  CALL FREEWAY_RANDOM(FSEED, RNDNUM)
  EPCT = 0.
  DO I = 1, FNUMLANES(IL)
    IF(LANE_PCT(FUSN(IL), I) .EQ. 0.0) CYCLE
    EPCT = EPCT + LANE_PCT(FUSN(IL), I)
 
! --- Do not consider unusable lanes.
 
    IF(FLANECODES(IV, I) .NE. LC_GOOD) CYCLE
    IF(NHOV_LANES(FTHRU_LINK(IL)) .GT. 0) THEN
      IF(HOV_SIDE(FTHRU_LINK(IL)) .EQ. 0) THEN
        IF(I .GT. FNUMLANES(FTHRU_LINK(IL)) - NHOV_LANES(FTHRU_LINK(IL))) CYCLE
      ELSE
        IF(I .LE. NHOV_LANES(FTHRU_LINK(IL))) CYCLE
      ENDIF
    ENDIF
    
! --- Do not consider a lane with a vehicle in it.

    IF(FLAST_VEHICLE(IL, I) .NE. 0 .AND. IV .NE. FLAST_VEHICLE(IL, I)) THEN
      IF(FLINK(FLAST_VEHICLE(IL, I)) .EQ. IL) CYCLE
    ENDIF
 
! --- If the vehicle is a bus consider lane 1. Otherwise
! --- use the random number to pick a lane.
 
    IF(EPCT .GE. RNDNUM .OR. (FFLEET(IV) .EQ. FLEET_BUS .AND. I .EQ. 1)) THEN
 
! --- Find the last vehicle in the lane.
 
      ILD = 0
      IF(FLAST_VEHICLE_OUT(IL, I) .NE. 0 .AND. IV .NE. FLAST_VEHICLE_OUT(IL, I)) THEN
        ILAST = FLAST_VEHICLE_OUT(IL, I)
        IF(FLINK(ILAST) .EQ. IL) THEN
          ILD = ILAST
        ELSEIF(FLINK(ILAST) .EQ. FTHRU_LINK(IL)) THEN
          IF(FLANE(ILAST) .EQ. I) THEN
            IF(FFOLLOWER(ILAST) .EQ. 0) THEN
              ILD = ILAST
            ELSE
              DO WHILE(ILAST .NE. 0)
                ILD = ILAST
                ILAST = FFOLLOWER(ILAST)
              ENDDO
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      IF(ILD .EQ. 0) THEN
        IPX = SORT_POSITION(IV)
        CALL FIND_FREEWAY_LEADER(IV, IPX, I, ILD)
      ENDIF
      IF(ILD .NE. 0) THEN
        IF(FLINK(ILD) .EQ. 0) THEN
          ILD = 0
        ENDIF
      ENDIF
 
! --- Calculate the headway behind the putative leader.
! --- If the headway is sufficient choose this lane.
 
      IF(ILD .EQ. 0) THEN
        FLANE(IV) = I
        RETURN
      ELSEIF(FLINK(ILD) .EQ. FTHRU_LINK(IL)) THEN
        HEADWAY(I) = SIMTIME - FENTRYTIME(ILD)
        IF(HEADWAY(I) .LT. 0.) HEADWAY(I) = HEADWAY(I) + INITIALIZATION_END
        LEAD(I) = ILD
        IF(HEADWAY(I) .GE. MINSEP .AND. FLOCATION(ILD) - FVLENGTH(ILD) .GT. 10) THEN
          FLANE(IV) = I
          RETURN
        ENDIF      
      ELSE
 
! --- If there is no putative leader choose this lane.
 
        FLANE(IV) = I
        FLEADER(IV) = ILD
        RETURN
      ENDIF
    ENDIF
    IF(FFLEET(IV) .EQ. FLEET_BUS .OR. FFLEET(IV) .EQ. FLEET_BIKE) RETURN
  ENDDO
 
! --- If no lane was chosen randomly, search for the lane with
! --- the largest headway.
       
! --- Store the headways.
 
  IF(FLANE(IV) .EQ. 0) THEN
    DO I = 1, FNUMLANES(IL)
 
! --- Do not consider unusable lanes.
 
      IF(LANE_PCT(FUSN(IL), I) .EQ. 0.0) CYCLE
      IF(FLANECODES(IV, I) .NE. LC_GOOD) CYCLE
      IF(NHOV_LANES(FTHRU_LINK(IL)) .GT. 0) THEN
        IF(HOV_SIDE(FTHRU_LINK(IL)) .EQ. 0) THEN
          IF(I .GT. FNUMLANES(FTHRU_LINK(IL)) - NHOV_LANES(FTHRU_LINK(IL))) CYCLE
        ELSE
          IF(I .LE. NHOV_LANES(FTHRU_LINK(IL))) CYCLE
        ENDIF
      ENDIF
      IF(FLAST_VEHICLE(IL, I) .NE. 0) THEN
        IF(FLINK(FLAST_VEHICLE(IL, I)) .EQ. IL) CYCLE
      ENDIF
      IF(LEAD(I) .EQ. 0) THEN
        !Look for the last vehicle out of the entry link.
        ILD = 0
        IF(FLAST_VEHICLE_OUT(IL, I) .NE. 0) THEN
          ILAST = FLAST_VEHICLE_OUT(IL, I)
          IF(FLINK(ILAST) .EQ. FTHRU_LINK(IL)) THEN
            IF(FLANE(ILAST) .EQ. I) THEN
              IF(FFOLLOWER(ILAST) .EQ. 0) THEN
                ILD = ILAST
              ELSE
                DO WHILE(ILAST .NE. 0)
                  ILD = ILAST
                  ILAST = FFOLLOWER(ILAST)
                ENDDO
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        IF(ILD .EQ. 0) THEN
          IPX = SORT_POSITION(IV)
          CALL FIND_FREEWAY_LEADER(IV, IPX, I, ILD)
        ENDIF
        IF(ILD .NE. 0) THEN
          IF(FLINK(ILD) .EQ. 0) THEN
            ILD = 0
          ENDIF
        ENDIF
        IF(ILD .EQ. 0) THEN
          HEADWAY(I) = 100.
        ELSEIF(FLINK(ILD) .EQ. FTHRU_LINK(IL)) THEN
          HEADWAY(I) = SIMTIME - FENTRYTIME(ILD)
          IF(HEADWAY(I) .LT. 0.) HEADWAY(I) = HEADWAY(I) + INITIALIZATION_END
          LEAD(I) = ILD
        ELSE
          HEADWAY(I) = 100.
          LEAD(I) = ILD
        ENDIF
      ENDIF
    ENDDO
 
! --- Select the lane with the best headway.
 
    HMAX = 0
    N = 0
    DO I = 1, FNUMLANES(IL)
      IF(LANE_PCT(FUSN(IL), I) .EQ. 0.0) CYCLE
      
! --- Do not consider a lane with a vehicle in it.
      IF(FLAST_VEHICLE(IL, I) .NE. 0) THEN
        IF(FLINK(FLAST_VEHICLE(IL, I)) .EQ. IL) CYCLE
      ENDIF
 
      IF(HEADWAY(I) .GT. HMAX) THEN
        HMAX = HEADWAY(I)
        N = I
      ENDIF
    ENDDO
    
    IF(HMAX .GE. MINSEP) THEN
      FLANE(IV) = N
      ILD = LEAD(N)
    ENDIF 
  ENDIF
  RETURN
  END

! ==========================================================================================================
  SUBROUTINE STREET_CHOOSE_ENTRY_LANE(IV, ILD)
! ----------------------------------------------------------------------
! --- Determine which lane a vehicle should enter when coming from an
! --- entry node.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE STREET_VEHICLES
  USE GLOBAL_DATA
  USE ENTRYNODE_DATA
  USE SIMPARAMS
  USE VEHICLE_TYPES
  USE SEEDS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER, INTENT(OUT) :: ILD
  INTEGER :: I, IL, N, LEAD(5), IUP, ILANE
  REAL :: RNDNUM, HEADWAY(5), HMAX, EPCT
! ----------------------------------------------------------------------
  SLANE(IV) = 0
  IL = SLINK(IV)
  HEADWAY = 0.
  LEAD = 0
 
! --- Loop over the lanes on the entry link.
      
  CALL STREET_RANDOM(SSEED, RNDNUM)
  EPCT = 0.
  DO I = FIRST_FULL_LANE(IL), LAST_FULL_LANE(IL)
    IUP = SUSN(IL)
    IF(LANE_PCT(IUP, I) .EQ. 0.0) CYCLE
 
! --- Do not consider unusable lanes.
 
    IF(SLANECODES(IV, I) .NE. LC_GOOD) CYCLE
 
! --- Do not choose the lane if there is already a vehicle in it.
 
    IF(FIRST_VEHICLE(IL, I) .NE. 0) CYCLE
 
! --- If the vehicle is a bus consider lane 1. Otherwise
! --- use the random number to pick a lane.
 
    EPCT = EPCT + LANE_PCT(IUP, I)
    IF(EPCT .GE. RNDNUM .OR. (SFLEET(IV) .EQ. FLEET_BUS .AND. I .EQ. 1)  &
       .OR. (SFLEET(IV) .EQ. FLEET_BIKE .AND. I .EQ. 1)) THEN
 
! --- Find the last vehicle in the lane.
 
      ILANE = I - FIRST_FULL_LANE(IL) + FIRST_FULL_LANE(STHRU_LINK(IL))
      CALL FIND_STREET_LEADER(IV, STHRU_LINK(IL), ILANE, ILD)
      DO WHILE(ILD .NE. 0)
        IF(SFOLLOWER(ILD) .EQ. 0) EXIT
        IF(SLINK(ILD) .NE. IL) EXIT
        ILD = SFOLLOWER(ILD)
      ENDDO
      IF(ILD .NE. 0) THEN
        IF(SLINK(ILD) .EQ. 0) THEN
          ILD = 0
        ENDIF
      ENDIF
 
! --- Calculate the headway behind the putative leader.
! --- If the headway is sufficient choose this lane.
 
      IF(ILD .EQ. 0) THEN
        SLANE(IV) = I
        RETURN
      ELSEIF(SLINK(ILD) .EQ. STHRU_LINK(IL)) THEN
        IF(SLOCATION(ILD) .LE. SVLENGTH(ILD)) THEN
          HEADWAY(I) = 0.
        ELSE
          HEADWAY(I) = SIMTIME - SENTRYTIME(ILD)
          IF(HEADWAY(I) .LT. 0.) HEADWAY(I) = HEADWAY(I) + INITIALIZATION_END
        ENDIF
        LEAD(I) = ILD
        IF(HEADWAY(I) .GE. MINSEP .AND. SLOCATION(ILD) - SVLENGTH(ILD) .GT. 10) THEN
          SLANE(IV) = I
          RETURN
        ENDIF      
      ELSE
 
! --- If there is no putative leader choose this lane.
 
        SLANE(IV) = I
        SLEADER(IV) = ILD
        EXIT
      ENDIF
    ENDIF
    IF(SFLEET(IV) .EQ. FLEET_BUS .OR. SFLEET(IV) .EQ. FLEET_BIKE) RETURN
  ENDDO
 
! --- If no lane was chosen randomly, search for the lane with
! --- the largest headway.
       
! --- Store the headways.
 
  IF(SLANE(IV) .EQ. 0) THEN
    DO I = FIRST_FULL_LANE(IL), LAST_FULL_LANE(IL)
      IUP = SUSN(IL)
      IF(LANE_PCT(IUP, I) .EQ. 0.0) CYCLE
 
! --- Do not consider unusable lanes.
 
      IF(SLANECODES(IV, I) .NE. LC_GOOD) CYCLE
 
! --- Do not choose the lane if there is already a vehicle in it.
 
      IF(FIRST_VEHICLE(IL, I) .NE. 0) CYCLE
      IF(LEAD(I) .EQ. 0) THEN
        ILANE = I - FIRST_FULL_LANE(IL) + FIRST_FULL_LANE(STHRU_LINK(IL))
        CALL FIND_STREET_LEADER(IV, STHRU_LINK(IL), ILANE, ILD)
        IF(ILD .NE. 0) THEN
          IF(SLINK(ILD) .EQ. 0) THEN
            ILD = 0
          ENDIF
        ENDIF
        IF(ILD .EQ. 0) THEN
          HEADWAY(I) = 100.
        ELSEIF(SLINK(ILD) .EQ. STHRU_LINK(IL)) THEN
          IF(SLOCATION(ILD) .LE. SVLENGTH(ILD)) THEN
            HEADWAY(I) = 0.
          ELSE
            HEADWAY(I) = SIMTIME - SENTRYTIME(ILD)
            IF(HEADWAY(I) .LT. 0.) HEADWAY(I) = HEADWAY(I) + INITIALIZATION_END
          ENDIF
          LEAD(I) = ILD
        ELSE
          HEADWAY(I) = 100.
          LEAD(I) = ILD
        ENDIF
      ENDIF
    ENDDO
 
! --- Select the lane with the best headway.
 
    HMAX = 0
    N = 0
    DO I = FIRST_FULL_LANE(IL), LAST_FULL_LANE(IL)
      IF(HEADWAY(I) .GT. HMAX) THEN
        HMAX = HEADWAY(I)
        N = I
      ENDIF
    ENDDO
    IF(HMAX .GE. MINSEP) THEN
      SLANE(IV) = N
      ILD = LEAD(N)
    ENDIF 
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE DETERMINE_PROPERTIES_FREEWAY(IFLT, IDT, IW)
! ----------------------------------------------------------------------
! --- Determine driver type.
! ----------------------------------------------------------------------
  USE GLOBAL_DATA
  USE VEHICLE_TYPES
  USE FREEWAY_VEHICLES
  USE SEEDS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IFLT
  INTEGER, INTENT(OUT) :: IDT
  LOGICAL, INTENT(OUT) :: IW
  REAL :: RNDNUM
! ----------------------------------------------------------------------
  CALL FREEWAY_RANDOM(FSEED, RNDNUM)
  IDT = 10 * RNDNUM + 1
  IF(IFLT .EQ. FLEET_EV) THEN
    IDT = 10
  ELSEIF(IFLT .EQ. FLEET_TRUCK .OR. IFLT .EQ. FLEET_BUS) THEN
    DO WHILE(IDT .LE. 5)
      CALL FREEWAY_RANDOM(FSEED, RNDNUM)
      IDT = 10 * RNDNUM + 1
    ENDDO
  ENDIF
 
! --- Determine if the driver will cooperate with a merging vehicle.
      
  CALL FREEWAY_RANDOM(FSEED, RNDNUM)
  IW = .FALSE.
  IF(RNDNUM .LE. FREEWAY_PCT_COOP) IW = .TRUE.
  RETURN
  END

! ==================================================================================================
  SUBROUTINE DETERMINE_PROPERTIES_STREET(IFLT, IDT, IW)
! ----------------------------------------------------------------------
! --- Determine driver type.
! ----------------------------------------------------------------------
  USE GLOBAL_DATA
  USE VEHICLE_TYPES
  USE STREET_VEHICLES
  USE SEEDS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IFLT
  INTEGER, INTENT(OUT) :: IDT
  LOGICAL, INTENT(OUT) :: IW
  REAL :: RNDNUM
! ----------------------------------------------------------------------
  CALL STREET_RANDOM(SSEED, RNDNUM)
  IDT = 10 * RNDNUM + 1
  IF(IFLT .EQ. FLEET_EV) THEN
    IDT = 10
  ELSEIF(IFLT .EQ. FLEET_TRUCK .OR. IFLT .EQ. FLEET_BUS) THEN
    DO WHILE(IDT .LE. 5)
      CALL STREET_RANDOM(SSEED, RNDNUM)
      IDT = 10 * RNDNUM + 1
    ENDDO
  ENDIF
 
! --- Determine if the driver will cooperate with a merging vehicle.
      
  CALL STREET_RANDOM(SSEED, RNDNUM)
  IW = .FALSE.
  IF(RNDNUM .LE. STREET_PCT_COOP) IW = .TRUE.

  RETURN
  END

! ==================================================================================================
  SUBROUTINE BUS_PROPERTIES_FREEWAY(IFLT, ITYPE)
! ----------------------------------------------------------------------
! --- Determine bus properties.
! ----------------------------------------------------------------------
  USE VEHICLE_TYPES
  USE FREEWAY_VEHICLES
  USE SIMPARAMS
  USE SEEDS
  IMPLICIT NONE
  INTEGER, INTENT(OUT) :: IFLT, ITYPE
  INTEGER :: I, SUM
  REAL :: RNDNUM
! ----------------------------------------------------------------------
  IFLT = FLEET_BUS
  CALL FREEWAY_RANDOM(FSEED, RNDNUM)
  RNDNUM = RNDNUM * 100
  SUM = 0
 
! --- Determine the type of bus.
     
  DO I = 1, 9
    SUM = SUM + FLT_FREEWAY_BUS(I)
    IF(SUM .GE. RNDNUM) THEN
      ITYPE = I
      EXIT
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE BUS_PROPERTIES_STREET(IFLT, ITYPE)
! ----------------------------------------------------------------------
! --- Determine bus properties.
! ----------------------------------------------------------------------
  USE VEHICLE_TYPES
  USE STREET_VEHICLES
  USE SIMPARAMS
  USE SEEDS
  IMPLICIT NONE
  INTEGER, INTENT(OUT) :: IFLT, ITYPE
  INTEGER :: I, SUM
  REAL :: RNDNUM
! ----------------------------------------------------------------------
  IFLT = FLEET_BUS
  CALL STREET_RANDOM(SSEED, RNDNUM)
  RNDNUM = RNDNUM * 100
  SUM = 0
 
! --- Determine the type of bus.
       
  DO I = 1, 9
    SUM = SUM + FLT_STREET_BUS(I)
    IF(SUM .GE. RNDNUM) THEN
      ITYPE = I
      EXIT
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE VEHICLE_PROPERTIES_FREEWAY(IL, IFLT, ITYPE)
! ----------------------------------------------------------------------
! --- Determine vehicle properties.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE VEHICLE_TYPES
  USE ENTRYNODE_DATA
  USE SIMPARAMS
  USE SEEDS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IL
  INTEGER, INTENT(OUT) :: IFLT, ITYPE
  INTEGER :: INODE, I, SUM
  REAL :: RNDNUM
! ----------------------------------------------------------------------
  INODE = FUSN(IL)
  CALL FREEWAY_RANDOM(FSEED, RNDNUM)
 
! --- Determine the fleet, then determine the type within the fleet.
      
  IF(RNDNUM .LT. 1.0 - TRUCK_PCT(INODE) - CARPOOL_PCT(INODE)) THEN
    IFLT = FLEET_AUTO
    CALL FREEWAY_RANDOM(FSEED, RNDNUM)
    RNDNUM = RNDNUM * 100
    SUM = 0
    DO I = 1, 9
      SUM = SUM + FLT_FREEWAY_AUTO(I)
      IF(SUM .GE. RNDNUM) THEN
        ITYPE = I
        EXIT
      ENDIF
    ENDDO
  ELSEIF(RNDNUM .LT. 1.0 - CARPOOL_PCT(INODE)) THEN
    IFLT = FLEET_TRUCK
    CALL FREEWAY_RANDOM(FSEED, RNDNUM)
    RNDNUM = RNDNUM * 100
    SUM = 0
    DO I = 1, 9
      SUM = SUM + FLT_FREEWAY_TRUCK(I)
      IF(SUM .GE. RNDNUM) THEN
        ITYPE = I
        EXIT
      ENDIF
    ENDDO
  ELSE
    IFLT = FLEET_CARPOOL
    CALL FREEWAY_RANDOM(FSEED, RNDNUM)
    RNDNUM = RNDNUM * 100
    SUM = 0
    DO I = 1, 9
      SUM = SUM + FLT_FREEWAY_CARPOOL(I)
      IF(SUM .GE. RNDNUM) THEN
        ITYPE = I
        EXIT
      ENDIF
    ENDDO
  ENDIF
  RETURN
  END


! ==================================================================================================
  SUBROUTINE VEHICLE_PROPERTIES_STREET(IL, IFLT, ITYPE)
! ----------------------------------------------------------------------
! --- Determine vehicle properties.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE VEHICLE_TYPES
  USE ENTRYNODE_DATA
  USE SIMPARAMS
  USE SEEDS
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IL
  INTEGER, INTENT(OUT) :: IFLT, ITYPE
  INTEGER :: INODE, I, SUM
  REAL :: RNDNUM
! ----------------------------------------------------------------------
  IF(NODE_TYPE(SUSN(IL)) .EQ. NT_EXTERN) THEN
    INODE = SUSN(IL)
  ELSE
    !This is a source node.
    !!!Need to update to use truck and carpool percentages.
    IFLT = FLEET_AUTO
    CALL STREET_RANDOM(SSEED, RNDNUM)
    RNDNUM = RNDNUM * 100
    SUM = 0
    DO I = 1, 9
      SUM = SUM + FLT_STREET_AUTO(I)
      IF(SUM .GE. RNDNUM) THEN
        ITYPE = I
        RETURN
      ENDIF
    ENDDO
  ENDIF
  CALL STREET_RANDOM(SSEED, RNDNUM)
 
! --- Determine the fleet, then determine the type within the fleet.
       
  IF(RNDNUM .LT. 1.0 - TRUCK_PCT(INODE) - CARPOOL_PCT(INODE)) THEN
    IFLT = FLEET_AUTO
    CALL STREET_RANDOM(SSEED, RNDNUM)
    RNDNUM = RNDNUM * 100
    SUM = 0
    DO I = 1, 9
      SUM = SUM + FLT_STREET_AUTO(I)
      IF(SUM .GE. RNDNUM) THEN
        ITYPE = I
        EXIT
      ENDIF
    ENDDO
  ELSEIF(RNDNUM .LT. 1.0 - CARPOOL_PCT(INODE)) THEN
    IFLT = FLEET_TRUCK
    CALL STREET_RANDOM(SSEED, RNDNUM)
    RNDNUM = RNDNUM * 100
    SUM = 0
    DO I = 1, 9
      SUM = SUM + FLT_STREET_TRUCK(I)
      IF(SUM .GE. RNDNUM) THEN
        ITYPE = I
        EXIT
      ENDIF
    ENDDO
  ELSE
    IFLT = FLEET_CARPOOL
    CALL STREET_RANDOM(SSEED, RNDNUM)
    RNDNUM = RNDNUM * 100
    SUM = 0
    DO I = 1, 9
      SUM = SUM + FLT_STREET_CARPOOL(I)
      IF(SUM .GE. RNDNUM) THEN
        ITYPE = I
        EXIT
      ENDIF
    ENDDO
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE GET_NEXT_ID(VID)
! ----------------------------------------------------------------------
! --- Get the next vehicle ID.
! ----------------------------------------------------------------------
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER, INTENT(OUT) :: VID
! ----------------------------------------------------------------------
  LAST_ID_USED = LAST_ID_USED + 1
  VID = LAST_ID_USED
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE INSERT_VEHICLE(IV)
! ----------------------------------------------------------------------
! --- Place a new vehicle into the correct position in the sorted list.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER :: I, J
  REAL :: VPOS
! ----------------------------------------------------------------------
  IF(SORTED_LIST_LENGTH .EQ. 0) THEN
    SORTED_LIST(1) = IV
    SORT_POSITION(IV) = 1
    SORTED_LIST_LENGTH = 1
  ELSE
    VPOS = DISTANCE_TO_SEGMENT_END(IV)
    IF(VPOS .GE. DISTANCE_TO_SEGMENT_END(SORTED_LIST_LENGTH)) THEN
      SORTED_LIST_LENGTH = SORTED_LIST_LENGTH + 1
      SORTED_LIST(SORTED_LIST_LENGTH) = IV
      SORT_POSITION(IV) = SORTED_LIST_LENGTH
    ELSE          
      DO I = SORTED_LIST_LENGTH, 1, -1
        IF(VPOS .LE. DISTANCE_TO_SEGMENT_END(I)) THEN
          IF(I .EQ. SORTED_LIST_LENGTH) THEN
            SORTED_LIST_LENGTH = SORTED_LIST_LENGTH + 1
            SORTED_LIST(SORTED_LIST_LENGTH) = IV
            SORT_POSITION(IV) = SORTED_LIST_LENGTH
          ELSE
            DO J = SORTED_LIST_LENGTH, I + 1, -1
              SORTED_LIST(J+1) = SORTED_LIST(J)
              SORT_POSITION(SORTED_LIST(J+1)) = J + 1
            ENDDO
            SORTED_LIST(I) = IV
            SORT_POSITION(IV) = I
          ENDIF
          EXIT
        ENDIF
      ENDDO
    ENDIF
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE REMOVE_VEHICLE(IV)
! ----------------------------------------------------------------------
! --- Remove a vehicle from the sorted list.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER :: I, NV
! ----------------------------------------------------------------------
  NV = SORT_POSITION(IV)
  SORTED_LIST_LENGTH = SORTED_LIST_LENGTH - 1
  DO I = NV, SORTED_LIST_LENGTH
    SORTED_LIST(I) = SORTED_LIST(I+1)
    SORT_POSITION(SORTED_LIST(I)) = I
  ENDDO
  SORTED_LIST(SORTED_LIST_LENGTH+1) = 0
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE GET_CAR_FOLLOWING_MODEL(ITYPE, RNDNUM, CFM)
! ----------------------------------------------------------------------
! --- Determine which car following model the vehicle will use.
! ----------------------------------------------------------------------
  USE CAR_FOLLOWING
  USE VEHICLE_TYPES
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ITYPE
  REAL, INTENT(IN) :: RNDNUM
  INTEGER, INTENT(OUT) :: CFM
! ----------------------------------------------------------------------
  IF(RNDNUM .LE. PCT_PITT(ITYPE)) THEN
    CFM = CFM_PITT
  ELSEIF(RNDNUM .LE. PCT_PITT(ITYPE) + PCT_IDM(ITYPE)) THEN
    CFM = CFM_IDM
  ELSEIF(RNDNUM .LE. PCT_PITT(ITYPE) + PCT_IDM(ITYPE) + PCT_ACC(ITYPE)) THEN
    CFM = CFM_ACC
  ELSEIF(RNDNUM .LE. PCT_PITT(ITYPE) + PCT_IDM(ITYPE) + PCT_ACC(ITYPE) + PCT_CACC(ITYPE)) THEN
    CFM = CFM_CACC
  ENDIF
  RETURN
  END
