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
  SUBROUTINE UPDATE_FREEWAY_VEHICLES
! ----------------------------------------------------------------------
! --- Loop through the freeway vehicles and update.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE TEXT
  USE SIMPARAMS
  USE SEGMENTS
  IMPLICIT NONE
#ifdef TSIS_COMPATIBLE
  INCLUDE 'CORWIN.FI'
#endif    
  INTEGER :: N, IV
  INTEGER :: ISEG
  
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
#endif
#ifdef TSIS_COMPATIBLE
  CALL SIMCALLRTE(RT_PRE_VEHICLEUPDATE)
#endif
  
! --- If there are Emergency Vehicles in the network process interactions.

  IF(EV_COUNT .GT. 0) CALL FREEWAY_EVSIM
  
! --- Loop over all segments and update vehicles on each segment.

  CURRENT_LIST = SORTED_LIST
  DO ISEG = 1, NUMBER_OF_SEGMENTS
    DO N = 1, HIGHEST_INDEX_F
      IV = CURRENT_LIST(N)
      IF(IV .EQ. 0) CYCLE
#ifdef DebugVersion
  temp = fid(iv)
#endif
      IF(ISEGMENT(IV) .EQ. ISEG) THEN
        IF(FXCODE(IV) .EQ. 0) THEN
          CALL UPDATE_FREEWAY_VEHICLE(IV)
        ENDIF
      ENDIF
    ENDDO
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE SORT_VEHICLE_LIST
! ----------------------------------------------------------------------
! --- Sort the freeway vehicle list.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER :: I, V1, V2, TEMP
  REAL :: D1, D2
! ----------------------------------------------------------------------
 
! --- Loop over the SORTED_LIST of vehicles and resort if necessary,
! --- based on back bumper location.

! --- Loop through the list and search for vehicle pairs in the wrong order.
! --- If a pair is found in the wrong order swap them.
! --- Repeat the process until no pair in the wrong order is found.

  DO 
    TEMP = 0
    DO I = 1, SORTED_LIST_LENGTH - 1
      V1 = SORTED_LIST(I)
      V2 = SORTED_LIST(I+1)
      D1 = DISTANCE_TO_SEGMENT_END(V1) + FVLENGTH(V1)
      D2 = DISTANCE_TO_SEGMENT_END(V2) + FVLENGTH(V2)
      IF(D1 .GT. D2) THEN   !This pair is in the wrong order so swap their positions.
        TEMP = SORTED_LIST(I)
        SORTED_LIST(I) = SORTED_LIST(I+1)
        SORTED_LIST(I+1) = TEMP
        SORT_POSITION(V1) = I + 1
        SORT_POSITION(V2) = I
      ENDIF
    ENDDO
    IF(TEMP .EQ. 0) EXIT
  ENDDO
  END
  
! ==================================================================================================
  SUBROUTINE UPDATE_FREEWAY_VEHICLE(IV)
! ----------------------------------------------------------------------
! --- Process a specific freeway vehicle.
! ----------------------------------------------------------------------
  USE SIMPARAMS
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE INCIDENTS
  USE TEXT
  USE VEHICLE_TYPES
  USE GLOBAL_DATA
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER :: INC, ILN
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = fid(iv)
#endif

  FXCODE(IV) = 1
  IF(FID(IV) .NE. 0) THEN
 
! --- If a freeway vehicle is responding to an incident
! --- determine if the incident has terminated.
 
    IF(INCIDENT_NUM(IV) .NE. 0) THEN
      INC = INCIDENT_NUM(IV)
      IF(SIMTIME .GT. INCIDENT_END_TIME(INC)) THEN
        INCIDENT_NUM(IV) = 0
        DO ILN = 1, N_FREEWAY_LANES
          IF(FLANECODES(IV, ILN) .EQ. LC_INC_GOOD .OR. FLANECODES(IV, ILN) .EQ. LC_INC_VACATE) THEN
            FLANECODES(IV, ILN) = FLANECODES(IV, ILN) - LC_INC_GOOD
          ENDIF
        ENDDO
      ENDIF
    ENDIF
 
! --- Determine if the vehicle should make a lane change.
 
    IF(FLC_TIMER(IV) .LE. 0 .OR. FFLEET(IV) .EQ. FLEET_EV .OR. FPSAVE(IV) .NE. 0) THEN
      IF(NODE_TYPE(FUSN(FLINK(IV))) .NE. NT_EXTERN) THEN
        IF(FLOCATION(IV) .GT. FVLENGTH(IV)) THEN
          CALL CHECK_FOR_FREEWAY_LANECHANGE(IV)
        ENDIF
      ENDIF
    ENDIF
 
! --- Move the vehicle for one time step.
 
    CALL MOVE_FREEWAY_VEHICLE(IV)
         
    IF(FLINK(IV) .EQ. 0) FID(IV) = 0
    IF(FID(IV) .NE. 0) THEN
      IF(FLANE(IV) .EQ. 0) THEN
        IF(FFLEET(IV) .NE. FLEET_BUS .AND. FENTRY_LINK(IV) .NE. FLINK(IV)) THEN
          WRITE(MSGTEXT, '(A,I5,A, 2I5, F10.1)') 'VEHICLE ', FID(IV), ' IS IN LANE 0 ON LINK ', FUSN(FLINK(IV)), FDSN(FLINK(IV)), SIMTIME
          CALL SENDTEXTMSG(M_ERROR)
        ENDIF
      ENDIF
    ENDIF
    IF(FLINK(IV) .NE. 0) THEN
 
! --- Compute emission data for the vehicle.
         
      CALL FREEWAY_EMISSIONS(IV)
 
! --- Store acceleration for use in the next time step.
         
      FPREV_ACCEL(IV) = FACCELERATION(IV)
    ENDIF
  ENDIF
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE UPDATE_STREET_VEHICLES
! ----------------------------------------------------------------------
! --- Loop through the surface street vehicles and update.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_DETECTORS
  USE SIMPARAMS
  USE STREET_LINKS
  USE BUS_STATION_DATA
  USE EVENTS
  USE VEHICLE_TYPES
  USE TEXT
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER :: IV, IL, ILANE, ITEMP, ISTAT, IDET
  !INTEGER :: LOOP_COUNTER
! ----------------------------------------------------------------------

! --- Surface street vehicles.

! --- Clear detector current state values
  DO IDET = 1, N_STREET_DETECTORS
    SDETECTOR(IDET)%PREVIOUS_STATE = SDETECTOR(IDET)%CURRENT_STATE
    SDETECTOR(IDET)%PREVIOUS_STATE_TEMP = SDETECTOR(IDET)%CURRENT_STATE_TEMP
    SDETECTOR(IDET)%CURRENT_STATE = 0
    SDETECTOR(IDET)%CURRENT_STATE_TEMP = 0
  ENDDO
  
! --- If there are Emergency Vehicles in the network process interactions.

  IF(EV_COUNT .GT. 0) CALL STREET_EVSIM

! --- Loop over all links and update vehicles on each link.

  DO IL = 1, N_STREET_LINKS
    LAGGER_TIMER(IL) = LAGGER_TIMER(IL) - TIMESTEP
    IF(.NOT. INITMODE) THEN
      CALL CANCEL_BLOCKAGE(IL)
      CALL APPLY_BLOCKAGE(IL)
    ENDIF
 
! --- Check for a vehicle coming from a source node.
! --- If it is still in lane 0 try to change to lane 1.
 
    IF(CENTROID(IL) .NE. 0) THEN
      IV = CENTROID_NV(IL)
      IF(IV .GT. 0) THEN
        IF(SXCODE(IV) .EQ. 0 .AND. SLANE(IV) .EQ. 0) THEN
          CALL TRY_STREET_LANECHANGE(IV, 1)
          IF(SLANE(IV) .EQ. 1) CENTROID_NV(IL) = 0
        ENDIF
      ENDIF
    ENDIF
          
! --- Loop over each lane and process each vehicle in the lane.
 
    DO ILANE = 1, N_STREET_LANES
      IV = FIRST_VEHICLE(IL, ILANE)
      !LOOP_COUNTER = 0
      DO WHILE(IV .NE. 0)
        !LOOP_COUNTER = LOOP_COUNTER + 1
        !IF(LOOP_COUNTER .GT. 1000) THEN
        !  WRITE(MSGTEXT, '(A, I5)') 'UPDATE_STREET_VEHICLES: INFINITE LOOP, VEHICLE #: ', SID(FIRST_VEHICLE(IL, ILANE))
        !  CALL SENDTEXTMSG(M_ERROR)
        !  EXITFLG = 1
        !  RETURN
        !ENDIF
        ITEMP = SFOLLOWER(IV)
        IF(SXCODE(IV) .EQ. 0) THEN
          CALL UPDATE_STREET_VEHICLE(IV)
          IF(SSPEED(IV) .LE. STOP_SPD .AND. SACCELERATION(IV) .LE. 0.) THEN
            HAS_STOPPED(IV) = .TRUE.
          ENDIF
          
! --- Accumulate queue delays
          IF(SLINK(IV) .NE. 0) CALL QUEUE_DELAYS(IV)
          
        ENDIF
        IV = ITEMP
      ENDDO
    ENDDO
 
! --- Check for a bus in a station.
 
    IF(FIRST_BUS_STATION(IL) .NE. 0) THEN
      ISTAT = FIRST_BUS_STATION(IL)
      DO WHILE(ISTAT .NE. 0)
        IV = BUS_STATION_LIST(ISTAT)%FRONT
        IF(IV .EQ. 0) THEN
          IF(.NOT. INITMODE) THEN
            BUS_STATION_LIST(ISTAT)%EMPTY_TIME = BUS_STATION_LIST(ISTAT)%EMPTY_TIME + TIMESTEP
          ENDIF
        ELSE
          DO WHILE(IV .NE. 0)
            IF(SLANE(IV) .NE. 0) EXIT
            IF(SFLEET(IV) .NE. FLEET_BUS) EXIT
            IF(SXCODE(IV) .EQ. 0) CALL UPDATE_STREET_VEHICLE(IV)
            IV = SFOLLOWER(IV)
          ENDDO
        ENDIF
        ISTAT = BUS_STATION_LIST(ISTAT)%NEXT_STATION
      ENDDO
    ENDIF
  ENDDO
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE UPDATE_STREET_VEHICLE(IV)
! ----------------------------------------------------------------------
! --- Process a specific street vehicle.
! ----------------------------------------------------------------------
  USE SIMPARAMS
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE INCIDENTS
  USE TEXT
  USE VEHICLE_TYPES
  USE GLOBAL_DATA
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

  SXCODE(IV) = 1
  RTOR_FLAG(IV) = .FALSE.
  IF(SID(IV) .NE. 0) THEN
 
! --- Clear the goal lane if the vehicle is already in it.
 
    IF(SLANE(IV) .EQ. GOAL_LANE(IV)) GOAL_LANE(IV) = 0
 
! --- Determine if the vehicle should make a lane change.
 
    IF(SLC_TIMER(IV) .LE. 0 .OR. SFLEET(IV) .EQ. FLEET_EV) THEN
      IF(NODE_TYPE(SUSN(SLINK(IV))) .NE. NT_EXTERN) THEN
        IF(SLOCATION(IV) .GT. SVLENGTH(IV)) THEN
          CALL CHECK_FOR_STREET_LANECHANGE(IV)
        ENDIF
      ENDIF
    ENDIF
 
! --- Move the vehicle for one time step.
 
    CALL MOVE_STREET_VEHICLE(IV)
         
    IF(SLINK(IV) .EQ. 0) SID(IV) = 0
    IF(SID(IV) .NE. 0) THEN
      IF(SLANE(IV) .EQ. 0) THEN
        IF(SFLEET(IV) .NE. FLEET_BUS .AND. SENTRY_LINK(IV) .NE. SLINK(IV)) THEN
          WRITE(MSGTEXT, '(A,I5,A, 2I5)') 'VEHICLE ', SID(IV), ' IS IN LANE 0 ON LINK ', SUSN(SLINK(IV)), SDSN(SLINK(IV))
          CALL SENDTEXTMSG(M_ERROR)
        ENDIF
      ENDIF
    ENDIF
    IF(SLINK(IV) .NE. 0) THEN
 
! --- Compute emission data for the vehicle.
         
      CALL STREET_EMISSIONS(IV)
 
! --- Store acceleration for use in the next time step.
         
      SPREV_ACCEL(IV) = SACCELERATION(IV)
    ENDIF
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE MOVE_FREEWAY_VEHICLE(IV)
! ----------------------------------------------------------------------
! --- Move a freeway vehicle.
! ----------------------------------------------------------------------
  USE SIMPARAMS
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE FREEWAY_NODES
  USE GLOBAL_DATA
  USE VDATA
  USE CAR_FOLLOWING
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  REAL :: ACCEL, TRAVEL, IDIST, T1, T2, Z1, Z2, RSPD, ASPD
  INTEGER :: IL, ILN, IL1, IL2, ILD, ILX, LANEX, IPX, IFL
  REAL :: DECEL, CF_DATA(7)
  LOGICAL :: ISGREEN, RECALL
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = fid(iv)
#endif

  IF(FLINK(IV) .NE. 0) THEN
    IF(FWILL_COOP_EV(IV) .AND. FLANE(IV) .EQ. 0) RETURN
    IDIST = DISTANCE_TO_SEGMENT_END(IV) + FVLENGTH(IV)
    RSPD = FSPEED(IV)
    IL1 = FLINK(IV)
    ISGREEN = .TRUE.
    IF(IMETER(IV) .EQ. 1) ISGREEN = .FALSE.
 
! --- Compute the acceleration needed to reach the desired speed.
! --- Unless the vehicle is on the shoulder.
         
    IF(WEVRUN .AND. FLANE(IV) .EQ. 0) THEN
      ACCEL = 0.
    ELSE
      CALL ACCELERATE_FREEWAY(IV, ACCEL)
    ENDIF
    ACCEL = MIN(ACCEL, FDECEL(IV))

! --- Check for illogical leader or follower situations. Indicates a defect in the logic.

    IF(FLEADER(IV) .EQ. IV) THEN
      IPX = SORT_POSITION(IV)
      CALL FIND_FREEWAY_LEADER(IV, IPX, FLANE(IV), ILD)
      FLEADER(IV) = ILD
    ENDIF
    IF(FFOLLOWER(IV) .EQ. IV) THEN
      CALL FIND_FREEWAY_FOLLOWER(IV, FLANE(IV), IFL)
      FFOLLOWER(IV) = IFL
    ENDIF
 
! --- Limit the acceleration to achieve a safe following distance.
 
    IF(FLEADER(IV) .NE. 0) THEN
      CALL FREEWAY_CAR_FOLLOW(IV, FLEADER(IV), ACCEL)
    ENDIF
 
! --- If the vehicle is cooperating with an EV make it stay behind the EV.
         
    IF(FPSEUDO_LEADER(IV) .NE. 0) THEN
      IF(DISTANCE_TO_SEGMENT_END(FPSEUDO_LEADER(IV)) .LT. DISTANCE_TO_SEGMENT_END(IV)) THEN
        CALL FREEWAY_CAR_FOLLOW(IV, FPSEUDO_LEADER(IV), ACCEL)
      ENDIF
    ENDIF
 
! --- Look ahead for objects that may affect the vehicle's speed.
! --- Unless the vehicle is on the shoulder.
         
    IF(FLANE(IV) .NE. 0) THEN
      IF(FLANE_CLOSED(FLINK(IV), FLANE(IV))) THEN
        FLANECODES(IV, FLANE(IV)) = LC_VACATE
        ACCEL = NEMDEC(FVTYPE(IV))
      ELSEIF(FTHRU_LINK(FLINK(IV)) .NE. 0) THEN
        ILX = FTHRU_LINK(FLINK(IV))
        LANEX = RECEIVING_LANE(FLINK(IV), FLANE(IV))
        IF(LANEX .NE. 0) THEN
          IF(FLANE_CLOSED(ILX, LANEX)) THEN
            CF_DATA(1) = FZFOLL(10)               
            CF_DATA(2) = 20.    !Arbitrary constant
            CF_DATA(3) = FSPEED(IV)
            CF_DATA(4) = FLENGTH(FLINK(IV)) - FLOCATION(IV)
            CF_DATA(5) = MAX_DECEL(FVTYPE(IV), FSPEED(IV), FGRADE(ILX))
            CF_DATA(6) = 0.1
            CF_DATA(7) = 0.0
            CALL INTERFACE_CAR_FOLLOW(CF_DATA, FDECEL(IV))
          ENDIF
        ENDIF
      ENDIF
      CALL PROCESS_OBJECTS_AHEAD(IV, ACCEL)
      IF(RAMP_MERGE_LINK(IL1) .AND. FLANE(IV) .EQ. MERGING_LANE(IL1)) THEN
        CALL MERGE_TRAFFIC(IV, ACCEL)
      ELSEIF(MAIN_MERGE_LINK(IL1) .AND. FLANE(IV) .EQ. MERGING_LANE(IL1)) THEN
        CALL MAIN_MERGE_TRAFFIC(IV, ACCEL)
      ENDIF
    ENDIF
 
! --- Compare the computed acceleration to the previous acceleration
! --- to determine if there should be a lag period before accelerating
! --- or decelerating. 
 
    IF(FLAG_TIMER(IV) .LE. 0.0) THEN
      IF(ACCEL .GT. 0 .AND. FACCELERATION(IV) .LE. 0) THEN
        FLAG_TIMER(IV) = LAG_ACCEL
      ELSEIF(ACCEL .LT. 0 .AND. FACCELERATION(IV) .GE. 0) THEN
        FLAG_TIMER(IV) = LAG_DECEL
      ENDIF
    ENDIF
 
! --- Do not apply any change in speed during a lag period.
 
    IF(FLAG_TIMER(IV) .GT. 0) THEN
      IF(FLAG_TIMER(IV) .GE. TIMESTEP) THEN
 
! --- Apply the previous acceleration during the entire TIMESTEP.
  
        FSPEED(IV) = FSPEED(IV) + TIMESTEP * FACCELERATION(IV)
        FSPEED(IV) = MAX(FSPEED(IV), 0.)
      ELSE
 
! --- Keep the speed constant during the lag time, and then
! --- apply the new computed acceleration during the remainder of
! --- the timestep.
  
        FSPEED(IV) = FSPEED(IV) + (TIMESTEP - FLAG_TIMER(IV)) * ACCEL       
        FSPEED(IV) = MAX(FSPEED(IV), 0.)
        FACCELERATION(IV) = (FSPEED(IV) - RSPD) / TIMESTEP
      ENDIF
      FLAG_TIMER(IV) = MAX(FLAG_TIMER(IV) - TIMESTEP, 0.0)
    ELSE
      FACCELERATION(IV) = ACCEL
      FSPEED(IV) = FSPEED(IV) + TIMESTEP * FACCELERATION(IV)
      FSPEED(IV) = MAX(FSPEED(IV), 0.)
    ENDIF
 
! --- Apply the resulting travel distance.
         
    TRAVEL = FSPEED(IV) * TIMESTEP + 0.5 * FACCELERATION(IV) * (TIMESTEP**2)
    TRAVEL = MAX(TRAVEL, 0.)
    FLOCATION(IV) = FLOCATION(IV) + TRAVEL
    DISTANCE_TO_SEGMENT_END(IV) = DISTANCE_TO_SEGMENT_END(IV) - TRAVEL
    IF(REMAINING_DIST(IV) .NE. 0) REMAINING_DIST(IV) = REMAINING_DIST(IV) - TRAVEL
 
! --- Process the effects of the objects that the vehicle has passed.
       
    CALL PROCESS_OBJECTS_PASSED(IV, RECALL)
    IF(RECALL) CALL PROCESS_OBJECTS_PASSED(IV, RECALL)
    IF(.NOT. INITMODE) THEN
      IF(FLINK(IV) .NE. 0 .AND. FLANE(IV) .NE. 0) THEN
        IL = FLINK(IV)
        ILN = FLANE(IV)
        IL2 = IL
        IF(IL1 .EQ. IL2) THEN
          CALL UPDATE_FREEWAY_STATS(IV, IL, 0, TRAVEL, 0., TIMESTEP, 0.)
 
! --- The vehicle has been on one link the entire time step.
! --- Increment the distance and time traveled on the link.
      
        ELSE
 
! --- The vehicle was on two links. Determine the fraction of time spent
! --- and distance traveled on each link.
! --- Z1 = distance traveled on link 1
! --- Z2 = distance traveled on link 2
! --- T1 = time on link 1
! --- T2 = time on link 2
 
          Z1 = IDIST - USN_TO_SEG_END(IL1) + FLENGTH(IL1)
          Z2 = TRAVEL - Z1
          IF(NODE_TYPE(FUSN(IL1)) .EQ. NT_EXTERN) THEN
            ASPD = (RSPD + FSPEED(IV)) / 2.0
            T2 = Z2 / ASPD
            T1 = TIMESTEP - T2
          ELSE
            T1 = TIMESTEP * Z1 / TRAVEL
            T2 = TIMESTEP - T1
          ENDIF
          CALL UPDATE_FREEWAY_STATS(IV, IL1, IL2, Z1, Z2, T1, T2)
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END
        

! ==================================================================================================
  SUBROUTINE MOVE_STREET_VEHICLE(IV)
! ----------------------------------------------------------------------
! --- Move a vehicle on a surface street.
! ----------------------------------------------------------------------
  USE SIMPARAMS
  USE GLOBAL_DATA
  USE SEEDS
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE BUS_STATION_DATA
  USE BUS_ROUTE_DATA
  USE EVENTS
  USE VEHICLE_TYPES
  USE VDATA
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  REAL :: ACCEL, TRAVEL, T1, T2, Z1, Z2, LOC1, LOC2, ILOC
  INTEGER :: ILN, ILD, IL1, IL2, ISPD, ISTAT, IEVENT, IFRSPD, ILANE
  LOGICAL :: WSTOP
  REAL :: RNDNUM, DIST, DECEL, RMAX, EFFECTIVE_TIME, DISCHARGE_TRAVEL, DISCHARGE_SPEED
  REAL :: ISTOPL
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

  IF(SLINK(IV) .NE. 0) THEN
    ILANE = SLANE(IV)
    LOC1 = SLOCATION(IV)
    IL1 = SLINK(IV)
    ACCEL = 0.
    DISCHARGE_TRAVEL = 0.
    DISCHARGE_SPEED = 0.
    
    !Determine if the vehicle will use a turn signal.
    IF(STURNCODE(IV) .EQ. TC_LEFT .OR. STURNCODE(IV) .EQ. TC_RIGHT) THEN
      IF(SLENGTH(IL1) - SLOCATION(IV) .LE. TURNSIGNAL_DIST .AND. TURN_INDICATOR(IV) .NE. TC_NULL) THEN
        CALL SIGNAL_FOR_TURN(IV)
      ENDIF
    ENDIF
 
! --- Determine the acceleration needed to achieve the desired speed.
 
    CALL ACCELERATE_STREET(IV, ACCEL)
    ACCEL = MIN(ACCEL, SDECEL(IV))
 
! --- See if the vehicle should exit at a sink.
 
    CALL CHECK_CENTROID(IV, IL1, ACCEL)
    IF(SLINK(IV) .EQ. 0) RETURN
 
! --- Check for a blockage ahead of the vehicle on the current link.
 
    IF(ILANE .NE. 0) THEN
      IF(BLOCKAGE(IL1, ILANE) .NE. 0) THEN
        IEVENT = BLOCKAGE(IL1, ILANE)
        IF(EVENT_LANE(IEVENT) .EQ. ILANE) THEN
          DIST = EVENT_LOCATION(IEVENT) - SLOCATION(IV)
          CALL CHECK_BLOCKAGE(IV, INT(DIST), ACCEL, WSTOP)
        ENDIF
      ENDIF
    ENDIF
 
! --- If the vehicle is a bus that stops at a station on the link,
! --- try to enter the station.
 
    IF(SFLEET(IV) .EQ. FLEET_BUS) THEN
      ISTAT = BUSR_STATIONLIST(SROUTEID(IV), NEXT_STOP(IV))
      IF(QSTATE(IV) .EQ. QS_DWELL) THEN
 
! --- If the bus is stopped at a station decrement dwell timer.
! --- When the timer expires try to move from the station.
 
        DWELL_TIMER(IV) = DWELL_TIMER(IV) - TIMESTEP
        IF(DWELL_TIMER(IV) .LE. 0.0) THEN
          IF(SLEADER(IV) .EQ. 0) CALL DISCHARGE_FROM_STATION(IV, ACCEL, ISTAT)
        ELSE
          IF(LOC1 .LT. BUS_STATION_LIST(ISTAT)%LOCATION) THEN
            IF(SLEADER(IV) .NE. 0) CALL STREET_CAR_FOLLOW(IV, SLEADER(IV), ACCEL)
            TRAVEL = SSPEED(IV) * TIMESTEP + 0.5 * ACCEL * TIMESTEP**2
            SLOCATION(IV) = LOC1 + TRAVEL
            SLOCATION(IV) = MIN(SLOCATION(IV), FLOAT(BUS_STATION_LIST(ISTAT)%LOCATION))
          ELSE
            ACCEL = 0.
          ENDIF
          IF(.NOT. INITMODE) CALL UPDATE_STREET_STATS(IV, IL1, 0, TRAVEL, 0., TIMESTEP, 0.)
        ENDIF
      ELSE
        IF(ISTAT .NE. 0) THEN
          IF(IL1 .EQ. BUS_STATION_LIST(ISTAT)%LINK) THEN
            CALL STREET_RANDOM(SSEED, RNDNUM)
            IF(RNDNUM .GT. BUS_STATION_LIST(ISTAT)%BYPASS_PCT) CALL APPROACH_BUS_STATION(IV, ACCEL)
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    
! --- Determine if the vehicle can discharge from the current link.
 
    IF(ILANE .NE. 0 .AND. QSTATE(IV) .NE. QS_DWELL) THEN
      ILD = SLEADER(IV)
      IF(ILD .NE. 0) THEN
        IF(SLINK(ILD) .NE. IL1) ILD = 0
      ENDIF
      WSTOP = .FALSE.
        
! --- If the vehicle needs to complete a mandatory lane change
! --- determine if it needs to slow down.
 
      IF(ILANE .NE. 0) THEN
        IF(SLANECODES(IV, ILANE) .NE. LC_GOOD) THEN
          DIST = SLENGTH(IL1) - SLOCATION(IV)
          IF((SFLEET(IV) .EQ. FLEET_BUS .OR. DIST .LT. 0.5 * SLENGTH(IL1)) .AND. SSPEED(IV) .GT. 20) THEN        !!!!Arbitrary constants
            RMAX = MAX_DECEL(SVTYPE(IV), SSPEED(IV), SGRADE(SLINK(IV)))
            IF(DIST .GT. 0) THEN
              ISPD = SSPEED(IV) - STOP_SPD
              DECEL = -(SSPEED(IV) ** 2) / (2 * MAX(DIST - 50, 1.))            !!!!Arbitrary constant
            ELSE
              DECEL = -SSPEED(IV) / TIMESTEP
            ENDIF
            DECEL = MAX(DECEL, RMAX)
            ACCEL = MIN(ACCEL, DECEL)
          ENDIF
        ENDIF
      ENDIF
      
      VEHICD0(IV) = 0
      IF(ILD .EQ. 0) THEN
        IFRSPD = SFREEFLOWSPEED(IL1)
        ISTOPL = SLENGTH(IL1) - SLOCATION(IV)
        IF(ISTOPL .LE. IFRSPD ** 2 / 7 .OR. ISTOPL .LE. SSPEED(IV) + 5) THEN
          IF(NODE_TYPE(SUSN(IL1)) .NE. NT_EXTERN) VEHICD0(IV) = 1
          CALL CHECK_DISCHARGE(IV, ACCEL, WSTOP, DISCHARGE_TRAVEL, DISCHARGE_SPEED)
        ELSE
          !If the vehicle is not close enough to consider discharging, determine
          !if it must car follow the last vehicle discharged.
          IF(SLAST_VEHICLE_OUT(IL1, ILANE) .NE. 0) THEN
            ILD = SLAST_VEHICLE_OUT(IL1, ILANE)
            IF(SLINK(ILD) .NE. LEFT_LINK(IL1) .AND.          &
                SLINK(ILD) .NE. STHRU_LINK(IL1) .AND.        &
                SLINK(ILD) .NE. RIGHT_LINK(IL1) .AND.        &
                SLINK(ILD) .NE. LEFT_DIAG_LINK(IL1) .AND.    &
                SLINK(ILD) .NE. RIGHT_DIAG_LINK(IL1)) THEN
              ILD = 0
              SLAST_VEHICLE_OUT(IL1, ILANE) = 0
            ENDIF
            IF(ILD .NE. 0) THEN
              IF(SLOCATION(ILD) .LT. SVLENGTH(ILD)) THEN
                CALL STREET_CAR_FOLLOW(IV, ILD, ACCEL)
                ACCEL = MIN(ACCEL, 0.0)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ELSE
 
! --- Limit the acceleration to achieve a safe following distance.
 
        CALL STREET_CAR_FOLLOW(IV, ILD, ACCEL)
        IF(ACCEL .LT. 0 .AND. QSTATE(ILD) .NE. QS_NOTINQ) THEN
          IF(SLOCATION(ILD) - SVLENGTH(ILD) - SLOCATION(IV) .LT. 20) THEN
            IF(QSTATE(IV) .EQ. QS_NOTINQ) THEN
              QSTATE(IV) = QS_MOVING
              IF(SSPEED(IV) .LE. STOP_SPD) QSTATE(IV) = QS_STOPPED
            ELSE
              IF(QSTATE(IV) .EQ. QS_CFSTOPPED .AND. SSPEED(IV) .GT. STOP_SPD) QSTATE(IV) = QS_CFMOVING
            ENDIF
          ENDIF
        ENDIF
      ENDIF
 
! --- If the vehicle needs to enter a turn pocket, determine if it
! --- needs to slow down to avoid driving past the end of the queue
! --- in the pocket lane.
 
      IF(SPSEUDO_LEADER(IV) .NE. 0) THEN
        DISCHARGE_TRAVEL = 0.
        DISCHARGE_SPEED = 0.
        ILD = SPSEUDO_LEADER(IV)
        IF(SLOCATION(IV) .LT. SLOCATION(ILD)) THEN
          CALL STREET_CAR_FOLLOW(IV, ILD, ACCEL)
        ELSE
        
! --- The vehicle has already passed its pseudo leader. Look at the next vehicle downstream.   

          DO WHILE(SPSEUDO_LEADER(IV) .NE. 0)
            SPSEUDO_LEADER(IV) = SLEADER(SPSEUDO_LEADER(IV))
            IF(SPSEUDO_LEADER(IV) .GT. LOC1) THEN
              ILD = SPSEUDO_LEADER(IV)
              CALL STREET_CAR_FOLLOW(IV, ILD, ACCEL)
              EXIT
            ENDIF
          ENDDO
        ENDIF
      ENDIF
           
! --- Compare the computed acceleration to the previous acceleration
! --- to determine if there should be a lag period before accelerating
! --- or decelerating. 
 
      IF(SLAG_TIMER(IV) .LE. 0.0) THEN
        IF(ACCEL .GT. 1 .AND. SACCELERATION(IV) .LE. -1) THEN
          SLAG_TIMER(IV) = LAG_ACCEL
        ELSEIF(ACCEL .LT. -1 .AND. SACCELERATION(IV) .GE. 1) THEN
          SLAG_TIMER(IV) = LAG_DECEL
        ENDIF
      ENDIF
 
! --- Apply the resulting acceleration.
        
      IF(SLINK(IV) .NE. 0) THEN
        TRAVEL = 0.
        IF(WSTOP) THEN
          SACCELERATION(IV) = -SSPEED(IV)
          SSPEED(IV) = 0.
          SLOCATION(IV) = SLENGTH(SLINK(IV))
          IF(IN_TURNING_WAY(IV)) ARC_LOCATION(IV) = ARC_LENGTH(IV)
          QSTATE(IV) = QS_STOPPED
        ELSE
          SACCELERATION(IV) = ACCEL
          IF(DISCHARGE_TRAVEL .GT. 0.) THEN
            TRAVEL = DISCHARGE_TRAVEL
            SSPEED(IV) = DISCHARGE_SPEED
          ELSE
            EFFECTIVE_TIME = TIMESTEP
            IF(SLAG_TIMER(IV) .GT. 0.0) THEN
              IF(SLAG_TIMER(IV) .GT. EFFECTIVE_TIME) THEN
                TRAVEL = EFFECTIVE_TIME * SSPEED(IV)
              ELSE
                EFFECTIVE_TIME = EFFECTIVE_TIME - SLAG_TIMER(IV)
                TRAVEL = SSPEED(IV) * SLAG_TIMER(IV) + SSPEED(IV) * EFFECTIVE_TIME + 0.5 * SACCELERATION(IV) * (EFFECTIVE_TIME**2)
                SSPEED(IV) = SSPEED(IV) + EFFECTIVE_TIME * SACCELERATION(IV)
              ENDIF
            ELSE
              TRAVEL = SSPEED(IV) * TIMESTEP + 0.5 * SACCELERATION(IV) * (TIMESTEP**2)
              SSPEED(IV) = SSPEED(IV) + TIMESTEP * SACCELERATION(IV)
            ENDIF
            SSPEED(IV) = MAX(SSPEED(IV), 0.)
            TRAVEL = MAX(TRAVEL, 0.)
          ENDIF
        ENDIF
        IF(ARC_ENTRYLINK(IV) .NE. 0) THEN
          CALL MOVE_ALONG_ARC(IV, TRAVEL)
        ELSEIF(IN_TURNING_WAY(IV)) THEN
          CALL MOVE_ALONG_TURNING_WAY(IV, TRAVEL)
        ELSE
          SLOCATION(IV) = SLOCATION(IV) + TRAVEL
        ENDIF
       
        SLAG_TIMER(IV) = MAX(SLAG_TIMER(IV) - TIMESTEP, 0.0)
 
        LOC2 = SLOCATION(IV)
        IF(SLINK(IV) .NE. 0 .AND. SLANE(IV) .NE. 0) THEN
          ILN = SLANE(IV)
          IL2 = SLINK(IV)
          IF(IL1 .EQ. IL2) THEN
            IF(.NOT. INITMODE) CALL UPDATE_STREET_STATS(IV, IL1, 0, TRAVEL, 0., TIMESTEP, 0.)
 
! --- Determine if the vehicle has passed a detector.
 
            IF(SFIRST_DETECTOR(IL1) .NE. 0) CALL CHECK_DETECTOR_STREET(IV, IL1, ILN, LOC1, LOC2)
 
! --- The vehicle has been on one link the entire time step.
! --- Increment the distance and time traveled on the link.
       
          ELSE
 
! --- The vehicle was on two links. Determine the fraction of
! --- time spent and distance traveled on each link.
! --- Z1 = distance traveled on link 1
! --- Z2 = distance traveled on link 2
! --- T1 = time on link 1
! --- T2 = time on link 2
 
            IF(.NOT. INITMODE) THEN
              IF(TRAVEL .EQ. 0.) THEN
                Z1 = 0.
                Z2 = 0.
                T1 = 0.
                T2 = TIMESTEP
              ELSE
                Z1 = SLENGTH(IL1) - LOC1
                Z2 = TRAVEL - Z1 
                T1 = TIMESTEP * Z1 / TRAVEL
                T2 = TIMESTEP - T1  
              ENDIF
              CALL UPDATE_STREET_STATS(IV, IL1, IL2, Z1, Z2, T1, T2)
            ENDIF
 
! --- Determine if the vehicle has passed a detector.
 
            IF(SFIRST_DETECTOR(IL1) .NE. 0) THEN
              ILOC = FLOAT(SLENGTH(IL1))
              CALL CHECK_DETECTOR_STREET(IV, IL1, ILN, LOC1, ILOC)
            ENDIF
            IF(SFIRST_DETECTOR(IL2) .NE. 0) THEN
              ILOC = 0.
              CALL CHECK_DETECTOR_STREET(IV, IL2, ILN, ILOC, LOC2)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE CHECK_DETECTOR_FREEWAY(IV, IL, ILN, LOC1, LOC2)
! ----------------------------------------------------------------------
! --- Determine if the vehicle crossed a detector in the current 
! --- timestep.
! ----------------------------------------------------------------------
  USE FREEWAY_DETECTORS
  USE FREEWAY_LINKS
  USE FREEWAY_VEHICLES
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, IL, ILN
  REAL, INTENT(IN) :: LOC1, LOC2
  INTEGER :: IDET, ILANE1, ILANE2, VLEN
  REAL :: D1, D2
! ----------------------------------------------------------------------
  VLEN = FVLENGTH(IV)
  IDET = FFIRST_DETECTOR(IL)
  D1 = FDETECTOR(IDET)%LOCATION
  D2 = FDETECTOR(IDET)%LOCATION + FDETECTOR(IDET)%ZONE_LENGTH
10 ILANE1 = FDETECTOR(IDET)%LANE1
  ILANE2 = FDETECTOR(IDET)%LANE2
  IF(ILANE1 .EQ. ILN .OR. ILANE2 .EQ. ILN .OR. ILANE1 .EQ. 9 .OR. (ILANE1 .EQ. 8 .AND. ILN .LE. FNUMLANES(IL))) THEN
    
!Freeway detectors are treated as passage detectors.
    
    IF(LOC1 .LT. D1 .AND. LOC2 .GE. D1) CALL STORE_DETECTOR_FREEWAY(IDET, IV)
  ENDIF
  IDET = FDETECTOR(IDET)%NEXT_DET
  IF(IDET .NE. 0) GOTO 10
  RETURN
  END
      

! ==================================================================================================
  SUBROUTINE CHECK_DETECTOR_STREET(IV, IL, ILN, LOC1, LOC2)
! ----------------------------------------------------------------------
! --- Determine if the vehicle crossed a detector in the current 
! --- timestep.
! ----------------------------------------------------------------------
  USE STREET_DETECTORS
  USE STREET_LINKS
  USE STREET_VEHICLES
  USE GLOBAL_DATA
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, IL, ILN
  REAL, INTENT(IN) :: LOC1, LOC2
  INTEGER :: IDET, ILANE1, ILANE2, VLEN
  REAL :: D1, D2, RTIME, SPEED
  LOGICAL :: COVERED
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

  VLEN = SVLENGTH(IV)
  IDET = SFIRST_DETECTOR(IL)
10 CONTINUE
  D2 = SDETECTOR(IDET)%LOCATION
  D1 = D2 - SDETECTOR(IDET)%ZONE_LENGTH
  ILANE1 = SDETECTOR(IDET)%LANE1
  ILANE2 = SDETECTOR(IDET)%LANE2
  COVERED = .FALSE.
  IF(ILANE1 .EQ. ILN .OR. ILANE2 .EQ. ILN) THEN
    COVERED = .TRUE.
  ELSEIF(ILANE1 .EQ. 200 .OR. ILANE2 .EQ. 200) THEN
    COVERED = .TRUE.
  ELSEIF(ILN .GE. FIRST_FULL_LANE(IL) .AND. ILN .LE. LAST_FULL_LANE(IL)) THEN
    IF(ILANE1 .EQ. 100 .OR. ILANE2 .EQ. 100) THEN 
      COVERED = .TRUE.
    ENDIF
  ENDIF
  IF(COVERED) THEN
    IF(SDETECTOR(IDET)%OPERATION_CODE .EQ. 0) THEN  !presence detector
      IF(LOC1 .LT. D1 .AND. LOC2 - VLEN .GT. D2) THEN
        !the vehicle jumped over the detector in the time step
        SPEED = SSPEED(IV) - 0.5 * SACCELERATION(IV) !average speed during the time step
        RTIME = (D1 - LOC1) / SPEED !time for front bumper to reach detector
        RTIME = MAX(RTIME, 0.)
        RTIME = MIN(RTIME, TIMESTEP)
        !in this case we should also determine the time within the time step when the back bumper crossed the detector
        CALL STORE_DETECTOR_STREET(IDET, IV, RTIME)
      ELSEIF(LOC2 .GE. D1 .AND. LOC2 .LE. D2) THEN
        !front bumper is over detector
        IF(LOC1 .GE. D1) THEN !front bumper was over the detector on the previous time step
          RTIME = 0
        ELSE
          SPEED = SSPEED(IV) - 0.5 * SACCELERATION(IV) !average speed during the time step
          RTIME = (D1 - LOC1) / SPEED !time for front bumper to reach detector
          RTIME = MAX(RTIME, 0.)
          RTIME = MIN(RTIME, TIMESTEP)
        ENDIF
        CALL STORE_DETECTOR_STREET(IDET, IV, RTIME)
      ELSEIF(LOC2 - VLEN .GE. D1 .AND. LOC2 - VLEN .LE. D2) THEN
        !back bumper is over detector
        IF(LOC1 .GE. D1) THEN !front bumper was over the detector on the previous time step
          RTIME = 0
        ELSE
          SPEED = SSPEED(IV) - 0.5 * SACCELERATION(IV) !average speed during the time step
          RTIME = (D1 - LOC1) / SPEED !time for front bumper to reach detector
          RTIME = MAX(RTIME, 0.)
          RTIME = MIN(RTIME, TIMESTEP)
        ENDIF
        CALL STORE_DETECTOR_STREET(IDET, IV, RTIME)
      ELSEIF(LOC2 .GT. D2 .AND. LOC2 - VLEN .LE. D1) THEN
        !vehicle is stradling the detector
        IF(LOC1 .GE. D1) THEN !front bumper was over the detector on the previous time step
          RTIME = 0
        ELSE
          SPEED = SSPEED(IV) - 0.5 * SACCELERATION(IV) !average speed during the time step
          RTIME = (D1 - LOC1) / SPEED !time for front bumper to reach detector
          RTIME = MAX(RTIME, 0.)
          RTIME = MIN(RTIME, TIMESTEP)
        ENDIF
        CALL STORE_DETECTOR_STREET(IDET, IV, RTIME)      
      ENDIF
    ELSE  !passage detector
      IF(LOC1 - VLEN .LT. D2 .AND. LOC2 - VLEN .GE. D2) THEN
        !store actuation when back bumper crosses the detector
        IF(LOC1 .GE. D1) THEN !front bumper was over the detector on the previous time step
          RTIME = 0
        ELSE
          SPEED = SSPEED(IV) - 0.5 * SACCELERATION(IV) !average speed during the time step
          RTIME = (D1 - LOC1) / SPEED !time for front bumper to reach detector
          RTIME = MAX(RTIME, 0.)
          RTIME = MIN(RTIME, TIMESTEP)
        ENDIF
        CALL STORE_DETECTOR_STREET(IDET, IV, RTIME)   
      ENDIF
    ENDIF
  ENDIF
  IDET = SDETECTOR(IDET)%NEXT_DET
  IF(IDET .NE. 0) GOTO 10
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE ACCELERATE_FREEWAY(IV, A)
! ----------------------------------------------------------------------
! --- Calculated the acceleration required to reach the vehicle's
! --- desired speed.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE VDATA
  USE SIMPARAMS
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  REAL, INTENT(OUT) :: A
  INTEGER :: IFLV, N
  REAL :: RSPD, RMAX
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = fid(iv)
#endif

  A = 0.
  RSPD = FDESIREDSPEED(IV)
  IF(FWILL_COOP_EV(IV) .AND. FFOLLOWER(IV) .NE. 0) THEN
 
! --- If the EV is stopped behind the cooperating vehicle,
! --- let the cooperating vehicle move forward.
       
    IFLV = FFOLLOWER(IV)
    N = 0
    DO WHILE(IFLV .NE. 0)
      N = N + 1
      IF(N .EQ. 5) EXIT
      IF(FFLEET(IFLV) .EQ. FLEET_EV) THEN
        IF(FSPEED(FFOLLOWER(IV)) .LE. 20.) THEN
          RSPD = MAX(RSPD, 20.)
          EXIT
        ENDIF
      ENDIF
      IFLV = FFOLLOWER(IFLV)
    ENDDO
  ENDIF
  
  IF(FSPEED(IV) .LE. 110) THEN
    A = (RSPD + FSPEED_ADJ(IV) - FSPEED(IV))
 
! --- Limit acceleration to the maximum possible acceleration for the
! --- vehicle type at the current speed.
 
    A = MIN(A, MAX_ACCEL(FVTYPE(IV), FSPEED(IV), FGRADE(FLINK(IV))))
 
! --- Limit deceleration to the maximum possible deceleration for the
! --- vehicle type at the current speed and grade.
 
    A = MAX(A, MAX_DECEL(FVTYPE(IV), FSPEED(IV), FGRADE(FLINK(IV))))
  ENDIF
 
! --- If the vehicle is cooperating with an EV allow it to slow down
! --- at the maximum rate.
 
  IF(FWILL_COOP_EV(IV)) THEN
    RMAX = MAX_DECEL(FVTYPE(IV), FSPEED(IV), FGRADE(FLINK(IV)))
    A = MAX(A, RMAX)
  ELSE
    A = MAX(A, -8.0)
  ENDIF
  FSPEED_ADJ(IV) = 0.
  RETURN
  END 

! ==================================================================================================
  SUBROUTINE ACCELERATE_STREET(IV, A)
! ----------------------------------------------------------------------
! --- Calculated the acceleration required to reach the vehicle's
! --- desired speed.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE VDATA
  USE STREET_LINKS
  USE SIMPARAMS
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  REAL, INTENT(OUT) :: A
  REAL :: RSPD, DIST
  INTEGER :: IFLV, N, IL
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

  A = 0.
  RSPD = SDESIREDSPEED(IV)
  IL = SLINK(IV)
 
! --- If the vehicle is close to initiating a turn
! --- limit the target speed to the turning speed for this link.
  
  IF(STURNCODE(IV) .EQ. TC_LEFT .OR. STURNCODE(IV) .EQ. TC_RIGHT) THEN
    IF(SLENGTH(IL) - SLOCATION(IV) .LE. RSPD ** 2 / 7) THEN
      DIST = SLENGTH(IL) - SLOCATION(IV)
      IF(DIST .GT. 0.0) THEN
        IF(STURNCODE(IV) .EQ. TC_LEFT) THEN
          RSPD = SQRT(8 * DIST + LT_SPEED(IL) ** 2)
        ELSEIF(STURNCODE(IV) .EQ. TC_RIGHT) THEN
          IF(.NOT. IN_TURNING_WAY(IV) .AND. ROUNDABOUT_ID(IL) .EQ. 0) THEN
            RSPD = SQRT(8 * DIST + RT_SPEED(IL) ** 2)
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  
! --- If the vehicle is in the process of completing a turn 
! --- limit the target speed to the turning speed of the previous link.
 
  IF(ARC_LENGTH(IV) .NE. 0 .AND. ARC_LOCATION(IV) .LT. LIMITED_SPEED_DIST(IV)) THEN
    IF(PREV_TURNCODE(IV) .EQ. TC_LEFT) THEN
      RSPD = LT_SPEED(SPREVLINK(IV))
    ELSEIF(PREV_TURNCODE(IV) .EQ. TC_RIGHT) THEN
      IF(.NOT. IN_TURNING_WAY(IV) .AND. ROUNDABOUT_ID(SPREVLINK(IV)) .EQ. 0) THEN
        RSPD = RT_SPEED(SPREVLINK(IV))
      ENDIF
    ENDIF
  ENDIF
  
  IF(SWILL_COOP_EV(IV) .AND. SFOLLOWER(IV) .NE. 0) THEN
 
! --- If the EV is stopped behind the cooperating vehicle,
! --- let the cooperating vehicle move forward.
       
    IFLV = SFOLLOWER(IV)
    N = 0
    DO WHILE(IFLV .NE. 0)
      N = N + 1
      IF(N .EQ. 5) EXIT
      IF(SFLEET(IFLV) .EQ. FLEET_EV) THEN
        IF(SSPEED(SFOLLOWER(IV)) .LE. 20.) THEN
          RSPD = MAX(RSPD, 20.)
          EXIT
        ENDIF
      ENDIF
      IFLV = SFOLLOWER(IFLV)
    ENDDO
  ENDIF
  
  IF(SSPEED(IV) .LE. 110) THEN
    A = (RSPD + SSPEED_ADJ(IV) - SSPEED(IV))
 
! --- Limit acceleration to the maximum possible acceleration for the
! --- vehicle type at the current speed and grade.
 
    A = MIN(A, MAX_ACCEL(SVTYPE(IV), SSPEED(IV), SGRADE(IL)))
 
! --- Limit deceleration to the maximum possible deceleration for the
! --- vehicle type at the current speed and grade.
 
    A = MAX(A, MAX_DECEL(SVTYPE(IV), SSPEED(IV), SGRADE(IL)))
    
  ENDIF
  SSPEED_ADJ(IV) = 0.
  RETURN
  END 

! ==================================================================================================
  SUBROUTINE PROCESS_OBJECTS_PASSED(IV, RECALL)
! ----------------------------------------------------------------------
! --- Process the objects that the vehicle has passed in the current
! --- timestep.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE FREEWAY_NODES
  USE OBJECTS
  USE VEHICLE_TYPES
  USE ADD_DROP_ALIGNMENTS
  USE INCIDENTS
  USE DIVERSIONS
  USE PATH_MOD
  USE SIMPARAMS
  USE FREEWAY_DETECTORS
  USE DATASTATIONS
  USE GLOBAL_DATA
  USE SEEDS
  USE TEXT
  USE SEGMENTS
  USE DRIVERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  LOGICAL, INTENT(OUT) :: RECALL
  INTEGER :: IL, ILANE, IOBJ, I, CURRENT(N_FREEWAY_LANES)
  INTEGER :: ILON, INC, ISEG, ILH, HLANE, IAUX, NLANES, NDROP
  INTEGER :: ILNK, ILT, ILN, INODE, NDIV
  REAL :: VLOC, RNDNUM, TRAVEL
  LOGICAL :: WHOV, WTEST, WBAD
  INTEGER :: LTYPE, ILD, NODE, NOBJ
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = fid(iv)
#endif

  RECALL = .FALSE.
  IL = FLINK(IV)
  LTYPE = LINKTYPE(FLINK(IV))
  VLOC = DISTANCE_TO_SEGMENT_END(IV) + FVLENGTH(IV)
 
! --- Start with the first object ahead of the vehicle.
       
  IOBJ = NEXT_OBJECT(IV)
  DO WHILE(IOBJ .GT. 0)
    IF(VLOC .LT. OBJECT_LIST(IOBJ)%LOCATION) THEN
 
! --- Store the next object in case the vehicle has changed to a
! --- new roadway or segment.
 
      NEXT_OBJECT(IV) = IOBJ - 1
           
      IF(OBJECT_LIST(IOBJ)%SEGMENT .EQ. ISEGMENT(IV)) THEN
        IF(LINKTYPE(OBJECT_LIST(IOBJ)%LINK) .EQ. LTYPE) THEN
          IF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_AUX_BEGIN) THEN
 
! --- Beginning of an auxiliary lane.
               
            CALL PROCESS_BEGIN_AUX(IV, IOBJ)
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_ALIGN_ON) THEN
            LTYPE = 0
            !IF(RAMP_MERGE_LINK(IL)) THEN
            !  temp=0
            !ENDIF
            
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_ALIGN_ADP) THEN
 
! --- Alignment at an add or drop.
              
            ILANE = FLANE(IV)
            IF(ILANE .EQ. 0) EXIT
            FLANE(IV) = ADDRP_ALIGNMENT(OBJECT_LIST(IOBJ)%VALUE, ILANE)
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_LANE_ADD) THEN
 
! --- Lane add.
               
            ILANE = OBJECT_LIST(IOBJ)%LANE
 
! --- Shift the lanecodes if the add is on the right.
                 
            IF(ILANE .EQ. 1) THEN
              DO I = 5, 2, -1
                FLANECODES(IV, I) = FLANECODES(IV, I - 1)
              ENDDO
            ENDIF
 
! --- Set the lanecode to make the lane available,
! --- but if the lane leads to the wrong exit make it unavailable.
                 
            FLANECODES(IV, ILANE) = LC_GOOD
            IF(FTURNCODE(IV) .EQ. TC_THRU) THEN
              IF(OFFRAMP_LINK(IL) .NE. 0) THEN 
                IF(OFFRAMP_LINK(IL) .NE. DESTINATION(IV)) THEN
                  IF(RECEIVING_LANE(IL, ILANE) .EQ. 0) FLANECODES(IV, ILANE) = LC_VACATE
                ENDIF
              ENDIF
            ENDIF
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_LANE_DROP) THEN
 
! --- Lane drop. Shift codes from the lanes that still exist.
               
            IF(OBJECT_LIST(IOBJ)%LANE .EQ. 1) THEN
              DO I = 2, FNUMLANES(IL)
                FLANECODES(IV, I-1) = FLANECODES(IV, I)
              ENDDO
              FLANECODES(IV, FNUMLANES(IL)) = LC_NULL
            ELSEIF(OBJECT_LIST(IOBJ)%LANE .LE. FNUMLANES(IL)) THEN
              FLANECODES(IV, OBJECT_LIST(IOBJ)%LANE) = LC_NULL
            ENDIF
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_NODE_INT) THEN
 
! --- Internal node.

            CALL PROCESS_FREEWAY_NODE(IV, FTHRU_LINK(IL))
            IF(FLINK(IV) .NE. 0) LTYPE = LINKTYPE(FLINK(IV))
 
! --- Reset IOBJ and VLOC in case the vehicle has entered a new segment.
                 
            IOBJ = NEXT_OBJECT(IV) + 1
            VLOC = DISTANCE_TO_SEGMENT_END(IV) + FVLENGTH(IV)
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_NODE_ENTRY) THEN
 
! --- Entry node.
               
            CALL PROCESS_FREEWAY_NODE(IV, FTHRU_LINK(IL))
            IF(FLINK(IV) .NE. 0) LTYPE = LINKTYPE(FLINK(IV))
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_DETECTOR) THEN
 
! --- Detector.
 
            IF(.NOT. INITMODE .AND. FLANE(IV) .EQ. OBJECT_LIST(IOBJ)%LANE) THEN
              CALL STORE_DETECTOR_FREEWAY(OBJECT_LIST(IOBJ)%VALUE, IV)
            ENDIF
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_DATASTAT) THEN
 
! --- Datastation.
 
            IF(.NOT. INITMODE) CALL STORE_DATASTAT(OBJECT_LIST(IOBJ)%VALUE, IV)
            
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_HOV_BEGIN) THEN

! --- Beginning of an HOV lane. Reset the flag that indicates that the vehicle must enter 
! --- the exclusive HOV lane at the beginning of the facility, and reset the lanecodes.
            
            HOV_URGENT(IV) = .FALSE.
            DO I = 1, N_FREEWAY_LANES
              IF(FLANECODES(IV, I) .EQ. LC_AVOIDHOV) FLANECODES(IV, I) = LC_GOOD
            ENDDO

          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_HOV_WARN_EXIT) THEN
 
! --- HOV exit warning sign. Set turn code and define lane codes for carpool vehicles and buses.
               
             IF(FFLEET(IV) .EQ. FLEET_CARPOOL .OR. FFLEET(IV) .EQ. FLEET_BUS) THEN
              IL = FLINK(IV)
              IF(OFFRAMP_LINK(OBJECT_LIST(IOBJ)%LINK) .EQ. DESTINATION(IV)) THEN
                FTURNCODE(IV) = TC_LEFT
                IF(OFFRAMP_SENDING_LANE(OBJECT_LIST(IOBJ)%LINK) .EQ. 1 .OR.  &
                   OFFRAMP_SENDING_LANE(OBJECT_LIST(IOBJ)%LINK) .GE. 16) FTURNCODE(IV) = TC_RIGHT
                IF(DESTINATION(IV) .NE. OFFRAMP_LINK(IL)) THEN

! --- If the vehicle will exit farther downstream use a diagonal turn code.

                  IF(FTURNCODE(IV) .EQ. TC_LEFT) THEN
                    FTURNCODE(IV) = TC_LDIAG
                  ELSEIF(FTURNCODE(IV) .EQ. TC_RIGHT) THEN
                    FTURNCODE(IV) = TC_RDIAG
                  ENDIF
                ENDIF
                CALL FIND_EXIT_LANES(IV, IL, OBJECT_LIST(IOBJ)%LINK)
                NLANES = 0
                DO I = 1, N_FREEWAY_LANES
                  IF(FLANECODES(IV, I) .EQ. LC_GOOD) THEN
                    NLANES = NLANES + 1
                    EXIT
                  ENDIF
                ENDDO
                IF(NLANES .EQ. 0) THEN
                  FLANECODES(IV, FLANE(IV)) = LC_GOOD
                ENDIF
              ELSE
                CALL AVOID_EXIT_LANES(IV, OBJECT_LIST(IOBJ)%LINK)
              ENDIF 
            ENDIF
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_WARN_EXIT) THEN
 
! --- Exit warning sign. Set turn code and define lane codes for vehicles that are not carpools or buses.
               
            IF(FFLEET(IV) .NE. FLEET_CARPOOL .AND. FFLEET(IV) .NE. FLEET_BUS) THEN
              IL = FLINK(IV)
              IF(OFFRAMP_LINK(OBJECT_LIST(IOBJ)%LINK) .EQ. DESTINATION(IV)) THEN
                FTURNCODE(IV) = TC_LEFT
                IF(OFFRAMP_SENDING_LANE(OBJECT_LIST(IOBJ)%LINK) .EQ. 1 .OR.  &
                   OFFRAMP_SENDING_LANE(OBJECT_LIST(IOBJ)%LINK) .GE. 16) FTURNCODE(IV) = TC_RIGHT
                IF(DESTINATION(IV) .NE. OFFRAMP_LINK(IL)) THEN

! --- If the vehicle will exit farther downstream use a diagonal turn code.

                  IF(FTURNCODE(IV) .EQ. TC_LEFT) THEN
                    FTURNCODE(IV) = TC_LDIAG
                  ELSEIF(FTURNCODE(IV) .EQ. TC_RIGHT) THEN
                    FTURNCODE(IV) = TC_RDIAG
                  ENDIF
                ENDIF
                WARNED_EXIT(IV) = .TRUE.
                CALL FIND_EXIT_LANES(IV, IL, OBJECT_LIST(IOBJ)%LINK)  
                NLANES = 0
                DO I = 1, N_FREEWAY_LANES
                  IF(FLANECODES(IV, I) .EQ. LC_GOOD) THEN
                    NLANES = NLANES + 1
                    EXIT
                  ENDIF
                ENDDO
                IF(NLANES .EQ. 0) THEN
                  FLANECODES(IV, FLANE(IV)) = LC_GOOD
                ENDIF
              ELSE
                IF(.NOT. WARNED_EXIT(IV)) CALL AVOID_EXIT_LANES(IV, OBJECT_LIST(IOBJ)%LINK)
              ENDIF   
            ENDIF
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_WARN_DROP) THEN
 
! --- Lane drop warning sign. Set lane codes to avoid the lane.
               
            NDROP = 1
            DO I = IOBJ, 1, -1
              IF(OBJECT_LIST(I)%ITYPE .EQ. M_LANE_DROP) THEN
                IF(OBJECT_LIST(I)%LOCATION  .EQ. OBJECT_LIST(IOBJ)%LOCATION - OBJECT_LIST(IOBJ)%VALUE) THEN
                  NDROP = I
                ENDIF
              ENDIF
            ENDDO               
            DO I = 1, N_FREEWAY_LANES
              IL = FLINK(IV)
              ILANE = I
              CALL TRACE_LANE_TO_DROP(IV, ILANE, NDROP, OBJECT_LIST(IOBJ)%LOCATION - OBJECT_LIST(IOBJ)%VALUE)
              IF(ILANE .EQ. OBJECT_LIST(IOBJ)%LANE) THEN
                IF(FLANECODES(IV, I) .NE. LC_NULL) THEN
                  FLANECODES(IV, I) = LC_VACATE
                ENDIF
              ENDIF
            ENDDO
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_WARN_ANTICIP) THEN
 
! --- Beginning of anticipatory lane change area.
               
            IF(LINKTYPE(IL) .EQ. 0) THEN
              ILON = OBJECT_LIST(IOBJ)%VALUE
 
! --- Determine if congestion exists.
               
              IF(AVERAGE_SPEED(ILON) .LE. ANTICIP_WARNING_SPEED(ILON)) THEN
                CURRENT = FLANECODES(IV, 1:N_FREEWAY_LANES)
                IF(OBJECT_LIST(IOBJ)%LANE .EQ. 0) THEN
 
! --- Set lane codes to avoid leftmost lanes.
               
                  DO I = FNUMLANES(IL), 15
                    IF(FLANECODES(IV, I) .EQ. LC_GOOD) FLANECODES(IV, I) = LC_ANTICIP
                  ENDDO
                ELSE
 
! --- Set lane codes to avoid rightmost lanes.
              
                  IF(FLANECODES(IV, 1) .EQ. LC_GOOD) FLANECODES(IV, 1) = LC_ANTICIP
                  DO I = 9, 11
                    IF(FLANECODES(IV, I) .EQ. LC_GOOD) FLANECODES(IV, I) = LC_ANTICIP
                  ENDDO
                ENDIF
 
! --- Make sure that at least one lane remains usable.
               
                NLANES = 0
                DO I = 1, N_FREEWAY_LANES
                  IF(FLANECODES(IV, I) .EQ. LC_GOOD) NLANES = NLANES + 1
                ENDDO
                IF(NLANES .EQ. 0) FLANECODES(IV, 1:N_FREEWAY_LANES) = CURRENT
              ENDIF
            ENDIF
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_END_ANTICIP) THEN
 
! --- End of anticipatory lane change area.
               
            DO I = 1, N_FREEWAY_LANES
              IF(FLANECODES(IV, I) .EQ. LC_ANTICIP) FLANECODES(IV, I) = LC_GOOD
            ENDDO
                                 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_NODE_ENTRY) THEN
 
! --- Entry node.
               
            CALL PROCESS_FREEWAY_NODE(IV, FTHRU_LINK(IL))
            IF(FLINK(IV) .NE. 0) LTYPE = LINKTYPE(FLINK(IV))
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_NODE_EXIT) THEN
 
! --- Exit node.
               
            CALL PROCESS_FREEWAY_NODE(IV, FTHRU_LINK(IL))
            IF(FLINK(IV) .NE. 0) LTYPE = LINKTYPE(FLINK(IV))

! --- Reset IOBJ and VLOC in case the vehicle has entered a new segment.
                 
            IOBJ = NEXT_OBJECT(IV) + 1
            VLOC = DISTANCE_TO_SEGMENT_END(IV) + FVLENGTH(IV)
            
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_EXIT_INTERFACE) THEN
 
! --- Interface node.
               
            CALL PROCESS_FREEWAY_NODE(IV, FTHRU_LINK(IL))
            IF(FLINK(IV) .NE. 0) LTYPE = LINKTYPE(FLINK(IV))
            
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_INC_BEGIN) THEN
 
! --- Start of an incident.
               
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_INC_END) THEN
 
! --- End of an incident.
               
            CALL SET_DESIREDSPEED_FREEWAY(IV)
            IF(OBJECT_LIST(IOBJ)%VALUE .EQ. INCIDENT_NUM(IV)) THEN
              INCIDENT_NUM(IV) = 0
              DO ILANE = 1, N_FREEWAY_LANES
                IF(FLANECODES(IV, ILANE) .EQ. LC_INC_GOOD) THEN
                  FLANECODES(IV, ILANE) = LC_GOOD
                ELSEIF(FLANECODES(IV, ILANE) .EQ. LC_INC_VACATE)THEN
                  FLANECODES(IV, ILANE) = LC_VACATE
                ENDIF
              ENDDO
            ENDIF
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_INC_WARN) THEN
 
! --- Warning sign for an incident.
        
            IF(.NOT. INITMODE) THEN
              INC = OBJECT_LIST(IOBJ)%VALUE
              IF(SIMTIME .GE. INCIDENT_BEGIN_TIME(INC) .AND. &
                 SIMTIME .LE. INCIDENT_END_TIME(INC)) THEN
                INCIDENT_NUM(IV) = INC
                ISEG = OBJECT_LIST(IOBJ)%SEGMENT
 
! --- If there is a blockage set lanecodes.
 
                IL = FLINK(IV)
                DO ILANE = 1, N_FREEWAY_LANES
                  IF(INCIDENT_CODE(INC, ILANE) .EQ. INC_BLOCK) THEN
                    I = ILANE
 
! --- Find the lane that leads to the incident.
 
                    CALL TRACE_LANE_BACK(IL, I, ISEG, IOBJ, VLOC)
                    IF(I .NE. 0) THEN
                      IF(FLANECODES(IV, I) .EQ. LC_GOOD .OR. FLANECODES(IV, I) .EQ. LC_ANTICIP) THEN
                        FLANECODES(IV, I) = LC_INC_GOOD
                      ELSEIF(FLANECODES(IV, I) .EQ. LC_VACATE) THEN
                        FLANECODES(IV, I) = LC_INC_VACATE
                      ENDIF
                    ENDIF
                  ENDIF
                ENDDO
              ENDIF
            ENDIF   
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_DIVERGE) THEN
 
! --- Warning sign for a diverge.
        
            IF(.NOT. INITMODE) THEN
              NDIV = OBJECT_LIST(IOBJ)%VALUE
              IF(SIMTIME .GE. DIVERSION_BEGIN_TIME(NDIV) .AND. SIMTIME .LT. DIVERSION_END_TIME(NDIV)) THEN
                IF(DESTINATION(IV) .NE. OFFRAMP_LINK(FLINK(IV))) THEN
                  CALL FREEWAY_RANDOM(FSEED, RNDNUM)
                  IF(RNDNUM * 100 .LE. DIVERSION_PERCENTAGE(NDIV)) THEN
 
! --- Save the current path ID.
                   
                    VEHICLES_DIVERTED(IL) = VEHICLES_DIVERTED(IL) + 1
                    FPSAVE(IV) = FPATHID(IV)
                    FPATHID(IV) = DIVERSION_PATHID(NDIV)
                    IF(FPATHID(IV) .GT. 0) THEN
                      IF(.NOT. ALLOCATED(NNODES)) THEN
                        WRITE(MSGTEXT, '(A)') 'NO PATHS WERE DEFINED FOR A DIVERSION'
                        CALL SENDTEXTMSG(M_ERROR)
                        EXITFLG = 1
                        RETURN
                      ENDIF
                      IL = FLINK(IV)
                      DO I = 1, NNODES(FPATHID(IV))-1
                        INODE = PATH_NODES(FPATHID(IV), I)
                        IF(INODE .EQ. FDSN(IL)) THEN
                          FPATHPOINT(IV) = I
                          DESTINATION(IV) = OFFRAMP_LINK(IL)
                          EXIT
                        ENDIF
                      ENDDO
                    ENDIF
 
! --- Vehicle will follow the diversion path.
                    
                    FTURNCODE(IV) = TC_LEFT
                    I = OFFRAMP_SENDING_LANE(OBJECT_LIST(IOBJ)%LINK)
                    IF(I .EQ. 1 .OR. I .GE. 16) FTURNCODE(IV) = TC_RIGHT
                    DO ILANE = 1, N_FREEWAY_LANES
                      IF(FLANECODES(IV, ILANE) .NE. LC_NULL) FLANECODES(IV, ILANE) = LC_GOOD
                    ENDDO
                    CALL FIND_EXIT_LANES(IV, IL, 0)
                    FDESIREDSPEED(IV) = DIVERSION_SPEED(NDIV)
                    FDIVERTED(IV) = .TRUE.
                  ENDIF
                ENDIF
              ENDIF   
            ENDIF
            
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_ETL_WARN) THEN
 
! --- Warning for a link with truck lane restriction. Identify
! --- the lane with restrictions and identify the lanes that the
! --- vehicle cannot use.
 
            ILT = OBJECT_LIST(IOBJ)%VALUE
            DO ILANE = 1, FNUMLANES(ILT)
              CALL CHECK_TRUCK_LANES(IV, ILT, ILANE, WBAD)
              IF(WBAD) THEN
 
! --- Find the lane that leads to the excluded lane.
 
                ILN = ILANE
                CALL TRACE_LANE_BACK(IL, IL, ISEG, IOBJ, VLOC)
                IF(ILN .NE. 0) THEN
                  IF(FLANECODES(IV, ILN) .EQ. LC_GOOD) FLANECODES(IV, ILN) = LC_EXCLUDED
                ENDIF
              ENDIF
            ENDDO
 
! --- Exclusive truck lane.
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_HOV_END) THEN
 
! --- End of an HOV lane.
        
            DO ILANE = 1, N_FREEWAY_LANES
              IF(FLANECODES(IV, ILANE) .EQ. LC_AVOIDHOV) FLANECODES(IV, ILANE) = LC_GOOD
            ENDDO
            
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_HOV_WARN) THEN
 
! --- Warning sign for the beginning of an HOV lane.
        
            WHOV = .FALSE.
            WTEST = .TRUE.
            ILH = OBJECT_LIST(IOBJ)%LINK
 
! --- If the vehicle has already processed a warning for an HOV lane
! --- ignore this warning.
 
            DO ILANE = 1, N_FREEWAY_LANES
              IF(FLANECODES(IV, ILANE) .EQ. LC_AVOIDHOV) THEN
                WTEST = .FALSE.
                EXIT
              ENDIF
            ENDDO
            
            IF(WTEST) THEN
              IF(FTURNCODE(IV) .EQ. TC_THRU) THEN
                IF(HOV_VIOLATOR(IV)) THEN
                  WHOV = .TRUE.
                ELSE
                  I = FFLEET(IV)
                  IF(HOV_CODE(ILH) .EQ. 0) THEN
                    WHOV = I .EQ. FLEET_CARPOOL .OR. I .EQ. FLEET_BUS
                  ELSEIF(HOV_CODE(ILH) .EQ. 1) THEN
                    WHOV = I .EQ. FLEET_BUS
                  ELSEIF(HOV_CODE(ILH) .EQ. 2) THEN
                    WHOV = I .EQ. FLEET_CARPOOL
                  ELSEIF(HOV_CODE(ILH) .EQ. 3) THEN
                    WHOV = .TRUE.
                  ELSEIF(HOV_CODE(ILH) .EQ. 4) THEN
                    WHOV = .FALSE.
                  ENDIF
                ENDIF
              ENDIF
              IF(WHOV) THEN
 
! --- The vehicle is allowed to use the HOV lane.
 
                IF(HOV_TYPE(ILH) .EQ. 1) THEN
 
! --- If the HOV lane is exclusive the vehicle should not use it
! --- unless it ends before the vehicle must exit.
 
                  ILNK = ILH
                  DO WHILE(ILNK .NE. 0)
 
! --- if the destination is reached first do not enter the hov lane
 
                    IF(DESTINATION(IV) .EQ. OFFRAMP_LINK(ILNK)) THEN
                      WHOV = .FALSE.
                      EXIT
                    ENDIF
 
! --- if an egress point is reached before reaching the destination
! --- link enter the hov lane.
 
                    IF(BARRIER(ILNK, 1) .EQ. 0) THEN            
                      IF(NHOV_LANES(ILNK) .EQ. 0) THEN
                        EXIT
                      ELSE
                        IF(HOV_END(ILNK) .LT. FLENGTH(ILNK) - 100.) THEN
                          EXIT
                        ELSE
                          IF(HOV_TYPE(ILNK) .EQ. 0) THEN
                            EXIT
                          ENDIF
                        ENDIF
                      ENDIF
                    ENDIF                                          
                    ILNK = FTHRU_LINK(ILNK)
                    IF(NHOV_LANES(ILNK) .GT. 0) THEN
                      IF(HOV_BEGIN(ILNK) .GT. 100.) THEN
                        EXIT
                      ENDIF
                    ENDIF
                  ENDDO
                ENDIF
              ENDIF
              IF(WHOV) THEN
 
! --- Determine if the driver chooses to use it.
 
                CALL FREEWAY_RANDOM(FSEED, RNDNUM)
                IF(RNDNUM .LE. HOV_PCT(ILH)) THEN
 
! --- Set lanecodes to use the lane.
                  
                  IF(HOV_TYPE(ILH) .EQ. 1) HOV_URGENT(IV) = .TRUE.
                  IF(HOV_SIDE(ILH) .EQ. 0) THEN
                    DO ILANE = 1, N_FREEWAY_LANES
                      IF(FLANECODES(IV, ILANE) .EQ. LC_GOOD .OR. &
                         FLANECODES(IV, ILANE) .EQ. LC_ANTICIP) THEN
 
! --- Find the leftmost lane.
 
                        HLANE = FNUMLANES(ILH)
                        DO IAUX = 1, N_AUXLANES
                          IF(AUX_LANE_ID(ILH, IAUX) .EQ. 0) EXIT !no more auxiliary lanes on this link
                          IF(AUX_LANE_ID(ILH, IAUX) .LT. 16) THEN
                            IF(AUX_LANE_CODE(ILH, IAUX) .EQ. AUX_FULL) THEN
                              HLANE = AUX_LANE_ID(ILH, IAUX)
                            ENDIF
                          ENDIF
                        ENDDO
 
! --- Set code to leave the lane unless it leads to the HOV lane.
 
                        FLANECODES(IV, ILANE) = LC_AVOIDHOV
                      ENDIF
                    ENDDO
                    IF(FLANECODES(IV, HLANE) .EQ. LC_AVOIDHOV) FLANECODES(IV, HLANE) = LC_GOOD
                    IF(NHOV_LANES(ILH) .GT. 1) THEN
                      HLANE = FREEWAY_LANE_TO_RIGHT(HLANE)
                      IF(FLANECODES(IV, HLANE) .EQ. LC_AVOIDHOV) FLANECODES(IV, HLANE) = LC_GOOD
                    ELSEIF(NHOV_LANES(ILH) .GT. 2) THEN
                      HLANE = FREEWAY_LANE_TO_RIGHT(HLANE)
                      IF(FLANECODES(IV, HLANE) .EQ. LC_AVOIDHOV) FLANECODES(IV, HLANE) = LC_GOOD
                    ENDIF
                  ELSE
                    DO ILANE = 1, N_FREEWAY_LANES
                      IF(FLANECODES(IV, ILANE) .EQ. LC_GOOD) THEN
 
! --- Find the rightmost lane.
 
                        HLANE = 1
                        DO IAUX = 1, N_AUXLANES
                          IF(AUX_LANE_ID(ILH, IAUX) .EQ. 0) EXIT !no more auxiliary lanes on this link
                          IF(AUX_LANE_ID(ILH, IAUX) .GE. 16) THEN
                            IF(AUX_LANE_CODE(ILH, IAUX) .EQ. AUX_FULL) HLANE = AUX_LANE_ID(ILH, IAUX)
                          ENDIF
                        ENDDO
 
! --- Set code to leave the lane unless it leads to the HOV lane.
 
                        FLANECODES(IV, ILANE) = LC_AVOIDHOV
                      ENDIF
                    ENDDO
                    IF(FLANECODES(IV, HLANE) .EQ. LC_AVOIDHOV) FLANECODES(IV, HLANE) = LC_GOOD
                    IF(NHOV_LANES(ILH) .GT. 1) THEN
                      HLANE = FREEWAY_LANE_TO_LEFT(HLANE)
                      IF(FLANECODES(IV, HLANE) .EQ. LC_AVOIDHOV) FLANECODES(IV, HLANE) = LC_GOOD
                    ELSEIF(NHOV_LANES(ILH) .GT. 2) THEN
                      HLANE = FREEWAY_LANE_TO_LEFT(HLANE)
                      IF(FLANECODES(IV, HLANE) .EQ. LC_AVOIDHOV) FLANECODES(IV, HLANE) = LC_GOOD
                    ENDIF
                  ENDIF
                ENDIF
              ELSE
 
! --- The vehicle is not allowed to use the HOV lane.
! --- Set lanecodes to avoid the lane.
 
                IF(HOV_TYPE(ILH) .EQ. 1) THEN
                  REMAINING_DIST(IV) = HOV_WARN(ILH) - USN_TO_SEG_END(ILH) + HOV_BEGIN(ILH)
                  HOV_URGENT(IV) = .TRUE.
                ENDIF
                IF(HOV_SIDE(ILH) .EQ. 0) THEN
                  HLANE = FNUMLANES(ILH)
                  DO IAUX = 1, N_AUXLANES
                    IF(AUX_LANE_ID(ILH, IAUX) .EQ. 0) EXIT !no more auxiliary lanes on this link
                    IF(AUX_LANE_ID(ILH, IAUX) .GE. 11 .AND. AUX_LANE_ID(ILH, IAUX) .LE. 15) THEN
                      IF(AUX_LANE_CODE(ILH, IAUX) .EQ. AUX_FULL) HLANE = AUX_LANE_ID(ILH, IAUX)
                    ENDIF
                  ENDDO
                  IF(FLANECODES(IV, HLANE) .NE. LC_NULL) FLANECODES(IV, HLANE) = LC_AVOIDHOV
                  IF(NHOV_LANES(ILH) .GT. 1) THEN
                    HLANE = FREEWAY_LANE_TO_RIGHT(HLANE)
                    IF(FLANECODES(IV, HLANE) .NE. LC_NULL) FLANECODES(IV, HLANE) = LC_AVOIDHOV
                  ENDIF
                  IF(NHOV_LANES(ILH) .GT. 2) THEN
                    HLANE = FREEWAY_LANE_TO_RIGHT(HLANE)
                    IF(FLANECODES(IV, HLANE) .NE. LC_NULL) FLANECODES(IV, HLANE) = LC_AVOIDHOV
                  ENDIF
                ELSE
                  HLANE = 1
                  DO IAUX = 1, N_AUXLANES
                    IF(AUX_LANE_ID(ILH, IAUX) .EQ. 0) EXIT !no more auxiliary lanes on this link
                    IF(AUX_LANE_ID(ILH, IAUX) .GE. 16) THEN
                      IF(AUX_LANE_CODE(ILH, IAUX) .EQ. AUX_FULL) HLANE = AUX_LANE_ID(ILH, IAUX)
                    ENDIF
                  ENDDO
                  IF(FLANECODES(IV, HLANE) .NE. LC_NULL) FLANECODES(IV, HLANE) = LC_AVOIDHOV
                  IF(NHOV_LANES(ILH) .GT. 1) THEN
                    HLANE = FREEWAY_LANE_TO_LEFT(HLANE)
                    IF(FLANECODES(IV, HLANE) .NE. LC_NULL) FLANECODES(IV, HLANE) = LC_AVOIDHOV
                  ENDIF
                  IF(NHOV_LANES(ILH) .GT. 2) THEN
                    HLANE = FREEWAY_LANE_TO_LEFT(HLANE)
                    IF(FLANECODES(IV, HLANE) .NE. LC_NULL) FLANECODES(IV, HLANE) = LC_AVOIDHOV
                  ENDIF
                ENDIF
              ENDIF
            ENDIF

          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_MERGE_POINT) THEN
            
            !The vehicle is leaving the merge link and joining the mainline freeway.
            CALL PROCESS_FREEWAY_NODE(IV, FTHRU_LINK(IL))
            
            !Find a new leader on the new segment.
            CALL FIND_FREEWAY_LEADER(IV, HIGHEST_INDEX_F, FLANE(IV), ILD)
            
            !Clear the leader follower relationship on the merge link.
            IF(FFOLLOWER(IV) .NE. 0) THEN
              FLEADER(FFOLLOWER(IV)) = 0
            ENDIF
            FFOLLOWER(IV) = 0
            
            !Determine if the new leader already has a follower.
            FLEADER(IV) = ILD
            IF(ILD .NE. 0) THEN
              IF(FFOLLOWER(ILD) .NE. 0) THEN
                TRAVEL = 0.
                IF(FXCODE(FFOLLOWER(ILD)) .EQ. 0) TRAVEL = FSPEED(FFOLLOWER(ILD)) * TIMESTEP
                IF(DISTANCE_TO_SEGMENT_END(IV) .LT. DISTANCE_TO_SEGMENT_END(FFOLLOWER(ILD)) - TRAVEL) THEN
                  !If the existing follower is farther upstream insert this vehicle between the leader and the existing follower.
                  FFOLLOWER(IV) = FFOLLOWER(ILD)
                  FLEADER(FFOLLOWER(ILD)) = IV
                  FFOLLOWER(ILD) = IV
                ELSE
                  !Use the leader's follower instead of the leader.
                  ILD = FFOLLOWER(ILD)
                  FLEADER(IV) = ILD
                  FFOLLOWER(IV) = FFOLLOWER(ILD)
                  IF(FFOLLOWER(ILD) .NE. 0) FLEADER(FFOLLOWER(ILD)) = IV
                  FFOLLOWER(ILD) = IV
                ENDIF
              ELSE
                !Store the vehicle as the new follower of the leader.
                FFOLLOWER(ILD) = IV
              ENDIF
            ENDIF
            LTYPE = 0
            
            !Identify the object representing the node at the merge point.
            NODE = FUSN(FTHRU_LINK(IL))
            DO NOBJ = NEXT_OBJECT(IV) + 1, NUMBER_OF_OBJECTS
              IF(OBJECT_LIST(NOBJ)%ITYPE .EQ. M_NODE_INT .AND. OBJECT_LIST(NOBJ)%VALUE .EQ. NODE) THEN
                NEXT_OBJECT(IV) = NOBJ - 1
                !Set the recall flag to process the objects passed on the new segment.
                RECALL = .TRUE.
                RETURN
              ENDIF
            ENDDO
            
            
          ENDIF
        ENDIF
      ENDIF
    ELSE
      EXIT
    ENDIF
 
! --- Get the next object.
              
    IOBJ = IOBJ - 1
  ENDDO
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE PROCESS_OBJECTS_AHEAD(IV, ACCEL)
! ----------------------------------------------------------------------
! --- Process the objects ahead of the vehicle to determine if the 
! --- vehicle must decelerate.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE STREET_VEHICLES
  USE GLOBAL_DATA
  USE FREEWAY_LINKS
  USE FREEWAY_NODES
  USE STREET_LINKS
  USE OBJECTS
  USE VEHICLE_TYPES
  USE SIMPARAMS
  USE DRIVERS
  USE VDATA
  USE SEGMENTS
  USE CAR_FOLLOWING
  USE NODE_TABLE
  USE INCIDENTS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  REAL, INTENT(INOUT) :: ACCEL
  INTEGER :: IL, ILANE, VLOC, IOBJ, I, IRL, ILD, ISEG
  INTEGER :: INODE, ILN, IVN, ILNE
  REAL :: DECEL, CF_DATA(7), RSTOP, RSPEED, RMAX, DIST, OFFSET
  INTEGER :: LTYPE, IVX, NODE, NOBJ, INC
  LOGICAL :: WATCH
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = fid(iv)
#endif
            
  IL = FLINK(IV)
  LTYPE = LINKTYPE(FLINK(IV))
  ILANE = FLANE(IV)
  VLOC = DISTANCE_TO_SEGMENT_END(IV) - 750
  OFFSET = 0.
 
! --- Start with the first object ahead of the vehicle.
       
  IOBJ = NEXT_OBJECT(IV)
  ISEG = ISEGMENT(IV)
  DO WHILE(IOBJ .GT. 0)
    IF(VLOC .LT. OBJECT_LIST(IOBJ)%LOCATION) THEN
      IF(OBJECT_LIST(IOBJ)%SEGMENT .EQ. ISEG) THEN
        IF(LINKTYPE(OBJECT_LIST(IOBJ)%LINK) .EQ. LTYPE) THEN
          IF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_INC_BEGIN) THEN
 
! --- Beginning of an incident.
                  
            IF(INCIDENT_NUM(IV) .EQ. OBJECT_LIST(IOBJ)%VALUE) THEN
              INC = INCIDENT_NUM(IV)
              IF(SIMTIME .GE. INCIDENT_BEGIN_TIME(INC) .AND. &
                 SIMTIME .LE. INCIDENT_END_TIME(INC)) THEN
                REMAINING_DIST(IV) = DISTANCE_TO_SEGMENT_END(IV) - OBJECT_LIST(IOBJ)%LOCATION
                IF(FLANECODES(IV, FLANE(IV)) .EQ. LC_INC_GOOD .OR. FLANECODES(IV, FLANE(IV)) .EQ. LC_INC_VACATE) THEN
                  DECEL = - (FSPEED(IV) ** 2) / (2 * REMAINING_DIST(IV))
                  ACCEL = MIN(ACCEL, DECEL)
                ENDIF
              ENDIF
                 
            ELSE
 
! --- If there is a blockage set lanecodes.
 
              ISEG = OBJECT_LIST(IOBJ)%SEGMENT
              INC = OBJECT_LIST(IOBJ)%VALUE
              DO ILANE = 1, N_FREEWAY_LANES
                IF(INCIDENT_CODE(INC, ILANE) .EQ. INC_BLOCK) THEN
                  REMAINING_DIST(IV) = DISTANCE_TO_SEGMENT_END(IV) - OBJECT_LIST(IOBJ)%LOCATION
                  IF(FLANECODES(IV, FLANE(IV)) .EQ. LC_INC_GOOD .OR. FLANECODES(IV, FLANE(IV)) .EQ. LC_INC_VACATE) THEN
                    DECEL = - (FSPEED(IV) ** 2) / (2 * REMAINING_DIST(IV))
                    ACCEL = MIN(ACCEL, DECEL)
                  ENDIF
                  INCIDENT_NUM(IV) = INC
                  I = ILANE
 
! --- Find the lane that leads to the incident.
 
                  CALL TRACE_LANE_BACK(IL, I, ISEG, IOBJ, VLOC)
                  IF(I .NE. 0) THEN
                    IF(FLANECODES(IV, I) .EQ. LC_GOOD .OR. FLANECODES(IV, I) .EQ. LC_ANTICIP) THEN
                      FLANECODES(IV, I) = LC_INC_GOOD
                    ELSEIF(FLANECODES(IV, I) .EQ. LC_VACATE) THEN
                      FLANECODES(IV, I) = LC_INC_VACATE
                    ENDIF
                  ENDIF
                ENDIF
              ENDDO
              
            ENDIF
              
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_AUX_END) THEN
 
! --- End of auxiliary lane.
                  
            IF(OBJECT_LIST(IOBJ)%VALUE .EQ. 1) THEN
              CALL TRACE_LANE_TO_AUX_END(IV, ILANE, IOBJ, OBJECT_LIST(IOBJ)%LOCATION)                            
              IF(ILANE .EQ. OBJECT_LIST(IOBJ)%LANE) THEN
                IF(FLANECODES(IV, FLANE(IV)) .EQ. LC_GOOD) THEN
                  DO I = 1, N_FREEWAY_LANES
                    IF(I .EQ. FLANE(IV)) CYCLE
                    IF(FLANECODES(IV, I) .EQ. LC_GOOD) THEN
                      FLANECODES(IV, FLANE(IV)) = LC_VACATE
                      EXIT
                    ENDIF
                  ENDDO
                ENDIF
                MUST_MERGE(IV) = .TRUE.
                RMAX = MAX_DECEL(FVTYPE(IV), FSPEED(IV), FGRADE(FLINK(IV)))
                RSTOP = -(FSPEED(IV) ** 2) / (2 * RMAX)
                REMAINING_DIST(IV) = DISTANCE_TO_SEGMENT_END(IV) - OBJECT_LIST(IOBJ)%LOCATION - 5.0
                REMAINING_DIST(IV) = REMAINING_DIST(IV) + OFFSET
                IF(REMAINING_DIST(IV) .LT. FSPEED(IV) * TIMESTEP) THEN
                  ACCEL = -FSPEED(IV) / TIMESTEP
                ELSEIF(REMAINING_DIST(IV) .GT. 0. .AND. REMAINING_DIST(IV) .LE. RSTOP) THEN
                  DECEL = -(FSPEED(IV) ** 2) /(2 * REMAINING_DIST(IV))
                  DECEL = MAX(DECEL, RMAX)
                  ACCEL = MIN(ACCEL, DECEL)
                ELSEIF(REMAINING_DIST(IV) .LE. RSTOP + FSPEED(IV)) THEN
                  ACCEL = MIN(ACCEL, 0.)
                ENDIF
              ENDIF
            ENDIF
            
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_LANE_DROP) THEN
 
! --- Lane drop.
                   
              CALL TRACE_LANE_TO_DROP(IV, ILANE, IOBJ, OBJECT_LIST(IOBJ)%LOCATION)                            
              IF(ILANE .EQ. OBJECT_LIST(IOBJ)%LANE) THEN
                RMAX = MAX_DECEL(FVTYPE(IV), FSPEED(IV), FGRADE(FLINK(IV)))
                RSTOP = -(FSPEED(IV) ** 2) / (2 * RMAX) 
                IF(OBJECT_LIST(IOBJ)%SEGMENT .EQ. ISEGMENT(IV)) THEN
                  REMAINING_DIST(IV) = DISTANCE_TO_SEGMENT_END(IV) - OBJECT_LIST(IOBJ)%LOCATION
                  IF(REMAINING_DIST(IV) .LE. FSPEED(IV) * TIMESTEP) THEN
                    ACCEL = -FSPEED(IV)
                  ELSEIF(REMAINING_DIST(IV) .LE. RSTOP + 100) THEN
                    FLANECODES(IV, ILANE) = LC_VACATE
                    DECEL = -(FSPEED(IV) ** 2) / (2 * REMAINING_DIST(IV))
                    DECEL = MAX(DECEL, RMAX)
                    ACCEL = MIN(ACCEL, DECEL)
                  ENDIF
                ENDIF
              ENDIF
            
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_METER) THEN
 
! --- Ramp meter.
 
! --- Determine if the vehicle must react to the signal. If it has
! --- a leader on the same link it will let the leader react to the
! --- signal.
       
            IF(FFLEET(IV) .NE. FLEET_EV) THEN
              IF(LINKTYPE(FLINK(IV)) .GT. 0) THEN
                IF(RAMPMETER(FLINK(IV)) .NE. 0) THEN
                  IF(FLEADER(IV) .EQ. 0) THEN
                    CALL CHECK_RAMPMETER(IV, ACCEL)
                  ELSEIF(FLINK(FLEADER(IV)) .NE. FLINK(IV)) THEN
                    CALL CHECK_RAMPMETER(IV, ACCEL)
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
                
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_ALIGN_ON) THEN
            ISEG = SEGMENT(FTHRU_LINK(IL))
            LTYPE = 0
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_ALIGN_OFF) THEN
            IF(.NOT. DUMMY_EXIT(FDSN(IL))) THEN
 
! --- Alignment at an off-ramp.

              IF(OFFRAMP_LINK(OBJECT_LIST(IOBJ)%LINK) .EQ. DESTINATION(IV)) THEN
              
! --- Determine if the vehicle must slow to get into another lane to reach the exit.
         
                REMAINING_DIST(IV) = DISTANCE_TO_SEGMENT_END(IV) - OBJECT_LIST(IOBJ)%LOCATION
                IRL = DESTINATION(IV)
                ISEG = SEGMENT(IRL)
                LTYPE = LINKTYPE(IRL)
 
! --- Determine if the vehicle is approaching a link with a lower freeflow speed.
 
                RSPEED = FFREEFLOWSPEED(IRL) * FFSPEED_ADJ(FDRIVERTYPE(IV), I_FREEWAY)
                FDESIREDSPEED(IV) = MIN(FDESIREDSPEED(IV), RSPEED)
                IF(RSPEED .LT. FSPEED(IV)) THEN
                  RMAX = MAX_DECEL(FVTYPE(IV), FSPEED(IV), FGRADE(FLINK(IV)))
                  DECEL = (RSPEED - FSPEED(IV)) / TIMESTEP
                  DECEL = MAX(ACCEL, RMAX)
                  ACCEL = MIN(ACCEL, DECEL)
                ENDIF
 
! --- If the vehicle needs to complete a mandatory lane change determine if it needs to slow down.
 
                !!!Arbitrary constants
                IF(FLANECODES(IV, FLANE(IV)) .EQ. LC_VACATE .AND. FSPEED(IV) .GT. 20) THEN
                  REMAINING_DIST(IV) = DISTANCE_TO_SEGMENT_END(IV) - OBJECT_LIST(IOBJ)%LOCATION
                  IF(REMAINING_DIST(IV) .GT. 0.) THEN
                    RMAX = MAX_DECEL(FVTYPE(IV), FSPEED(IV), FGRADE(FLINK(IV)))            
                    DECEL = -(FSPEED(IV) ** 2) / (2 * REMAINING_DIST(IV))
                    DECEL = MAX(DECEL, RMAX)
                    ACCEL = MIN(ACCEL, DECEL)
                  ENDIF
                ENDIF

! --- If the vehicle is approaching a new segment it might need to car follow a vehicle
! --- on the new segment. It might also need to search for a leader on the off-ramp if it
! --- currently does not have a leader.
 
                IF(SEGMENT(IRL) .NE. ISEGMENT(IV)) THEN
                  IVX = FLAST_VEHICLE_OUT(FLINK(IV), FLANE(IV))
                  ILD = 0
                  IF(FTURNCODE(IV) .EQ. TC_THRU .AND. RECEIVING_LANE(IL, FLANE(IV)) .NE. 0) THEN
                    IF(IVX .NE. 0) THEN
                      IF(FLINK(IVX) .EQ. FTHRU_LINK(IL) .AND. FLANE(IVX) .EQ. RECEIVING_LANE(IL, FLANE(IV))) THEN
                        ILD = IVX
                      ENDIF
                    ENDIF
                    IF(ILD .EQ. 0) CALL FIND_SEGMENT_LEADER(IV, FTHRU_LINK(IL), RECEIVING_LANE(IL, FLANE(IV)), ILD)
                  ELSEIF(OFFRAMP_LINK(IL) .NE. 0 .AND. EXIT_LANE(IL, FLANE(IV)) .NE. 0) THEN
                    IF(IVX .NE. 0) THEN
                      IF(FLINK(IVX) .EQ. OFFRAMP_LINK(IL) .AND. FLANE(IVX) .EQ. EXIT_LANE(IL, FLANE(IV))) THEN
                        ILD = IVX
                      ENDIF
                    ENDIF
                    IF(ILD .EQ. 0) CALL FIND_SEGMENT_LEADER(IV, FTHRU_LINK(IL), EXIT_LANE(IL, FLANE(IV)), ILD)
                  ENDIF
                  IF(ILD .NE. 0) CALL FREEWAY_CAR_FOLLOW(IV, ILD, ACCEL)
                ELSEIF(FLEADER(IV) .EQ. 0 .AND. ILANE .NE. 0) THEN
                  ILD = 0
                  ILN = EXIT_LANE(IL, ILANE)
                  DO I = HIGHEST_INDEX_F, 1, -1
                    IVN = SORTED_LIST(I)
                    IF(IVN .EQ. 0 .OR. IVN .EQ. IV) CYCLE
                    IF(FLINK(IVN) .EQ. IRL .AND. FLANE(IVN) .EQ. ILN) THEN
                      ILD = IVN
                      EXIT
                    ENDIF
                  ENDDO
                  IF(ILD .NE. 0) THEN
                    FLEADER(IV) = ILD
                    FFOLLOWER(ILD) = IV
                  ENDIF
                ENDIF
              ELSE
              
! --- Determine if the vehicle must slow to get into another lane to avoid the exit.
! --- If the vehicle needs to complete a mandatory lane change determine if it needs to slow down.
 
                !!!Arbitrary constants
                IF(FLANECODES(IV, FLANE(IV)) .EQ. LC_VACATE .AND. FSPEED(IV) .GT. 20) THEN
                  REMAINING_DIST(IV) = DISTANCE_TO_SEGMENT_END(IV) - OBJECT_LIST(IOBJ)%LOCATION
                  RMAX = MAX_DECEL(FVTYPE(IV), FSPEED(IV), FGRADE(FLINK(IV)))            
                  DECEL = -(FSPEED(IV) ** 2) / (2 * MAX(REMAINING_DIST(IV), 0.01))
                  DECEL = MAX(DECEL, RMAX)
                  ACCEL = MIN(ACCEL, DECEL)
                ENDIF
         
              ENDIF
            ENDIF
            
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_EXIT_INTERFACE) THEN
 
! --- The vehicle is approaching an interface exit. Determine if it needs
! --- to car-follow a vehicle on the surface street.
            
            INODE = 0
            IRL = 0
            IL = FLINK(IV)
            IF(NODE_TYPE(FDSN(IL)) .NE. NT_INTERN) THEN
              INODE = FDSN(IL)
            ELSEIF(FTHRU_LINK(IL) .NE. 0) THEN
              IF(NODE_TYPE(FDSN(FTHRU_LINK(IL))) .NE. NT_INTERN) INODE = FDSN(FTHRU_LINK(IL))
            ENDIF
            IF(INODE .NE. 0) THEN
              IF(FCROSS1(INODE, FLANE(IV)) .NE. 0) THEN
                CALL FREEWAY_CAR_FOLLOW(IV, FCROSS1(INODE, FLANE(IV)), ACCEL)
              ELSE
                DO IRL = 1, N_STREET_LINKS
                  IF(SUSN(IRL) .EQ. INODE) THEN
 
! --- Determine if the vehicle is approaching a link with a lower
! --- freeflow speed.
 
                    RSPEED = SFREEFLOWSPEED(IRL) * FFSPEED_ADJ(FDRIVERTYPE(IV), I_STREET)
                    FDESIREDSPEED(IV) = MIN(FDESIREDSPEED(IV), RSPEED)
                    IF(RSPEED .LT. FSPEED(IV)) THEN
                      ACCEL = MIN(ACCEL, (RSPEED - FSPEED(IV)) / TIMESTEP)
                      RMAX = MAX_DECEL(FVTYPE(IV), FSPEED(IV), FGRADE(FLINK(IV)))
                      ACCEL = MAX(ACCEL, RMAX)
                      ACCEL = MIN(ACCEL, 0.)
                    ENDIF
                    IF(FLEADER(IV) .EQ. 0 .AND. FCROSS1(INODE, FLANE(IV)) .EQ. 0) THEN
                      ILD = SLAST_VEHICLE(IRL, FLANE(IV))
                      IF(ILD .NE. 0) THEN
                        CF_DATA(1) = FZFOLL(FDRIVERTYPE(IV))
                        CF_DATA(2) = SSPEED(ILD)
                        CF_DATA(3) = FSPEED(IV)
                        CF_DATA(4) = FLENGTH(FLINK(IV)) - FLOCATION(IV) + SLOCATION(ILD) - SVLENGTH(ILD)
                        CF_DATA(5) = MAX_DECEL(FVTYPE(IV), FSPEED(IV), FGRADE(FLINK(IV)))
                        CF_DATA(6) = FCFMULT(FLINK(IV))
                        CF_DATA(7) = FPCFSEP
                        CALL INTERFACE_CAR_FOLLOW(CF_DATA, ACCEL)
                      ENDIF
                    ENDIF
                    EXIT
                  ENDIF
                ENDDO
              ENDIF

! --- If the vehicle is a bus make it use the rightmost usable lane

              IF(FFLEET(IV) .EQ. FLEET_BUS) THEN
                IF(FLANECODES(IV, 11) .EQ. LC_GOOD) THEN
                  ILN = 11
                ELSEIF(FLANECODES(IV, 10) .EQ. LC_GOOD) THEN
                  ILN = 10
                ELSEIF(FLANECODES(IV, 9) .EQ. LC_GOOD) THEN
                  ILN = 9
                ELSE
                  DO ILNE = 1, 5
                    IF(FLANECODES(IV, ILNE) .EQ. LC_GOOD) THEN
                      ILN = ILNE
                      EXIT
                    ENDIF
                  ENDDO
                ENDIF
                DO ILNE = 1, N_FREEWAY_LANES
                  IF(FLANECODES(IV, ILNE) .EQ. LC_GOOD .AND. ILNE .NE. ILN) THEN
                    FLANECODES(IV, ILNE) = LC_VACATE
                  ENDIF
                ENDDO
              ENDIF
            ENDIF  

          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_HOV_BEGIN) THEN

! --- Beginning of an HOV lane. If the vehicle must vacate the current lane
! --- to avoid entering the HOV lane make it slow down to allow a lane change.

            IF(FLANECODES(IV, FLANE(IV)) .EQ. LC_AVOIDHOV) THEN
              RMAX = MAX_DECEL(FVTYPE(IV), FSPEED(IV), FGRADE(FLINK(IV)))
              RSTOP = -(FSPEED(IV) ** 2) / (2 * RMAX)  
              REMAINING_DIST(IV) = DISTANCE_TO_SEGMENT_END(IV) - OBJECT_LIST(IOBJ)%LOCATION
              REMAINING_DIST(IV) = MAX(REMAINING_DIST(IV), 0.001)
              IF(REMAINING_DIST(IV) .LE. RSTOP + 100) THEN
                IF(FSPEED(IV) .GT. 20.) THEN          !!!ARBITRARY CONSTANT
                  DECEL = -(FSPEED(IV) ** 2) /(2 * REMAINING_DIST(IV))
                  DECEL = MAX(DECEL, RMAX)
                  IF(.NOT. HOV_URGENT(IV)) DECEL = MAX(DECEL, -4.0)
                  ACCEL = MIN(ACCEL, DECEL)
                ENDIF
              ENDIF
            ENDIF

          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_MERGE_POINT) THEN
            ISEG = SEGMENT(FTHRU_LINK(IL))
            ILANE = RECEIVING_LANE(IL, ILANE)
            VLOC = USN_TO_SEG_END(FTHRU_LINK(IL)) + DISTANCE_TO_SEGMENT_END(IV) - 1000
            OFFSET = USN_TO_SEG_END(FTHRU_LINK(IL)) - DISTANCE_TO_SEGMENT_END(IV)
            !Identify the object representing the node at the merge point.
            NODE = FUSN(FTHRU_LINK(IL))
            DO NOBJ = 1, NUMBER_OF_OBJECTS
              IF(OBJECT_LIST(NOBJ)%ITYPE .EQ. M_NODE_INT .AND. OBJECT_LIST(NOBJ)%VALUE .EQ. NODE) THEN
                IOBJ = NOBJ - 1
                EXIT
              ENDIF
            ENDDO
            
          ENDIF
        ENDIF
      ENDIF
    ELSE
      EXIT
    ENDIF
 
! --- Get the next object.
              
    IOBJ = IOBJ - 1
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE PROCESS_BEGIN_AUX(IV, IOBJ)
! ----------------------------------------------------------------------
! --- Change lane codes when a vehicle passes the beginning of an
! --- auxiliary lane.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE GLOBAL_DATA
  USE FREEWAY_LINKS
  USE OBJECTS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER, INTENT(IN) :: IOBJ
  INTEGER :: IL, I, IAUX
! ----------------------------------------------------------------------
  IL = OBJECT_LIST(IOBJ)%LINK
 
! --- If this is an acceleration lane set the lane code to vacate.
      
  IF(OBJECT_LIST(IOBJ)%VALUE .EQ. 1) THEN
    FLANECODES(IV, OBJECT_LIST(IOBJ)%LANE) = LC_VACATE
  ELSE
 
! --- If this is a deceleration lane or a full lane the vehicle may use it
! --- if it will exit here.
      
    IF(DESTINATION(IV) .EQ. OFFRAMP_LINK(IL)) THEN
      IF(EXIT_LANE(IL, OBJECT_LIST(IOBJ)%LANE) .NE. 0) THEN
        DO I = 1, N_FREEWAY_LANES
          IF(FLANECODES(IV, I) .EQ. LC_GOOD) THEN
            IF(EXIT_LANE(IL, I) .EQ. 0) FLANECODES(IV, I) = LC_VACATE
          ENDIF
        ENDDO
      ENDIF
      IF(EXIT_LANE(IL, OBJECT_LIST(IOBJ)%LANE) .NE. 0) FLANECODES(IV, OBJECT_LIST(IOBJ)%LANE) = LC_GOOD
    ELSE
 
! --- If the vehicle will not exit here it may only use the lane
! --- if it aligns with a mainline lane, or is a full auxiliary lane
! --- that leads to a mainline link.
        
      IF(RECEIVING_LANE(IL, OBJECT_LIST(IOBJ)%LANE) .NE. 0) THEN
        FLANECODES(IV, OBJECT_LIST(IOBJ)%LANE) = LC_GOOD
      ELSE
        FLANECODES(IV, OBJECT_LIST(IOBJ)%LANE) = LC_VACATE
        DO IAUX = 1, N_AUXLANES
          IF(AUX_LANE_ID(IL, IAUX) .EQ. OBJECT_LIST(IOBJ)%LANE) THEN
            IF(AUX_LANE_CODE(IL, IAUX) .EQ. AUX_FULL .AND. RECEIVING_LANE(IL, AUX_LANE_ID(IL, IAUX)) .NE. 0) THEN
              FLANECODES(IV, OBJECT_LIST(IOBJ)%LANE) = LC_GOOD
              EXIT
            ENDIF
          ENDIF
        ENDDO
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END
   
! ==================================================================================================
  SUBROUTINE PROCESS_FREEWAY_NODE(IV, ILINK)                     
! ----------------------------------------------------------------------
! --- Change lane codes when a vehicle passes a node.
! ----------------------------------------------------------------------
  USE SIMPARAMS
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE FREEWAY_NODES
  USE VEHICLE_TYPES
  USE BUS_STATION_DATA
  USE BUS_ROUTE_DATA
  USE GLOBAL_DATA
  USE SEEDS
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, ILINK
  INTEGER :: IL, ILANE, I, ILD, ILAST, ILNEXT
  INTEGER :: ITYPE, RID, IPX, IVN, IRL, ILN, IVX
  REAL :: RNDNUM, Z1, T1, ACCEL
  LOGICAL :: WILL_EXIT, WBAD
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = fid(iv)
#endif

  FPREVLINK(IV) = FLINK(IV)
  FPREVLANE(IV) = FLANE(IV)
  IF(FROUTEID(IV) .NE. 0 .OR. FPATHID(IV) .NE. 0) FPATHPOINT(IV) = FPATHPOINT(IV) + 1
  IL = FLINK(IV)
  ILANE = FLANE(IV)
  ILNEXT = ILINK
 
! --- If the vehicle is on the shoulder consider it in the rightmost lane.
      
  IF(ILANE .EQ. 0) THEN
    IF(FLANECODES(IV, 20) .NE. LC_NULL) THEN
      ILANE = 20
    ELSEIF(FLANECODES(IV, 19) .NE. LC_NULL) THEN
      ILANE = 19
    ELSEIF(FLANECODES(IV, 18) .NE. LC_NULL) THEN
      ILANE = 18
    ELSEIF(FLANECODES(IV, 17) .NE. LC_NULL) THEN
      ILANE = 17
    ELSEIF(FLANECODES(IV, 16) .NE. LC_NULL) THEN
      ILANE = 16
    ELSE
      ILANE = 1
    ENDIF
  ENDIF
 
! --- Store the index of the last vehicle to exit from the link.
 
  FLAST_VEHICLE_OUT(IL, ILANE) = IV
  FLAST_VEHICLE_OUT_ID(IL, ILANE) = FID(IV)
 
! --- Increment the number of vehicles discharged from the prior link.
 
  IF(.NOT. INITMODE) THEN
    FDISCHARGED(IL) = FDISCHARGED(IL) + 1
    IF(FFLEET(IV) .EQ. FLEET_BUS) THEN
      FDISCHARGED_BUSES(IL) = FDISCHARGED_BUSES(IL) + 1
    ENDIF
    ITYPE = FVTYPE(IV)
    FDISCHARGED_TYPE(IL, ITYPE) = FDISCHARGED_TYPE(IL, ITYPE) + 1
    FDISCHARGED_LANE(IL, ILANE) = FDISCHARGED_LANE(IL, ILANE) + 1
    IF(FTURNCODE(IV) .NE. TC_THRU) THEN
      DISCHARGED_RAMP(IL) = DISCHARGED_RAMP(IL) + 1
    ENDIF
    FPERSON_TRIPS(IL) = FPERSON_TRIPS(IL) + AVG_OCCS(ITYPE)
  ENDIF
       
! --- Store the average speed of the link for use by the anticipatory lane
! --- change logic.
! --- (Temporarily just use the speed of the last vehicle to represent the average.)
       
  IF(IL .NE. 0) THEN
    AVERAGE_SPEED(IL) = FSPEED(IV)
    IF(NODE_TYPE(FUSN(IL)) .EQ. NT_EXTERN) THEN
 
! --- The vehicle is coming from an entry link.
 
      FLAST_VEHICLE(IL, FLANE(IV)) = 0
      FXCODE(IV) = 1
      FLINK(IV) = FTHRU_LINK(IL)
      IF(FLEADER(IV) .NE. 0) THEN
        ILD = FLEADER(IV)
        FLOCATION(IV) = MIN(FLOCATION(IV), FLOCATION(ILD) - FVLENGTH(ILD) - 3.0)
      ENDIF
 
! --- Define lane codes for the new link.
 
      DO I = 1, N_FREEWAY_LANES
        FLANECODES(IV, I) = LC_NULL
      ENDDO
 
! --- Apply lane restrictions.
 
      DO I = 1, FNUMLANES(IL)
        FLANECODES(IV, I) = LC_GOOD
        IF(TRUCK_CODE(IL) .GT. TRK_BIASED) THEN
          CALL CHECK_TRUCK_LANES(IV, FLINK(IV), I, WBAD)
          IF(WBAD) FLANECODES(IV, I) = LC_EXCLUDED 
        ENDIF
        IF(FXCLUDE_TYPE(FLINK(IV), I, FVTYPE(IV))) FLANECODES(IV, I) = LC_EXCLUDED
      ENDDO
 
    ELSE
 
! --- This is a vehicle on the freeway.
! --- Save the travel time and distance traveled on the previous link
! --- in case it is needed when the vehicle exits the network.
 
      T1 = 0.
      Z1 = 0.
      IF(FSPEED(IV) .NE. 0) THEN
        T1 = ((FLOCATION(IV) - FLENGTH(IL)) / FSPEED(IV)) * TIMESTEP
        Z1 = T1 * FSPEED(IV)
      ENDIF
 
! --- Determine if the vehicle is exiting or remaining on the mainline.
 
      FLOCATION(IV) = FLOCATION(IV) - FLENGTH(IL)
      WILL_EXIT = .FALSE.
      IF(DUMMY_EXIT(FDSN(IL))) THEN
        WILL_EXIT = .TRUE.
      ELSEIF(FTURNCODE(IV) .NE. TC_LEFT .AND. FTURNCODE(IV) .NE. TC_RIGHT) THEN
 
!! --- Determine if the vehicle is on a mainline link that merges with another mainline link.
!        
!        IF(RAMP_MERGE_LINK(IL)) THEN
!          FLINK(IV) = FTHRU_LINK(IL)
!          FLANE(IV) = RECEIVING_LANE(IL, FLANE(IV))
!          ISEGMENT(IV) = SEGMENT(FTHRU_LINK(IL))
!          DISTANCE_TO_SEGMENT_END(IV) = USN_TO_SEG_END(FTHRU_LINK(IL))
!          IF(FLANE(IV) .EQ. MERGING_LANE(IL)) THEN
!            IPX = HIGHEST_INDEX_F
!            CALL FIND_FREEWAY_LEADER(IV, IPX, FLANE(IV), ILD)
!            IF(ILD .NE. 0) CALL FREEWAY_CAR_FOLLOW(IV, ILD, ACCEL)
!            FLEADER(IV) = ILD
!          ENDIF
!        ELSEIF(RECEIVING_LANE(IL, ILANE) .EQ. 0 .AND. EXIT_LANE(IL, ILANE) .NE. 0) THEN
 
! --- Determine if the vehicle is in a lane that does not go thru.
! --- If so, the vehicle will have to exit.
        
        IF(RECEIVING_LANE(IL, ILANE) .EQ. 0 .AND. EXIT_LANE(IL, ILANE) .NE. 0) THEN          
          WILL_EXIT = .TRUE.
          DESTINATION(IV) = OFFRAMP_LINK(IL)
          WARNED_EXIT(IV) = .FALSE.
          CALL FIND_EXIT_LANES(IV, IL, 0)
          IF(FLANE(IV) .EQ. 1 .OR. FLANE(IV) .GE. 16) THEN
            FTURNCODE(IV) = TC_RIGHT
          ELSE
            FTURNCODE(IV) = TC_LEFT
          ENDIF
          
! --- Search for a leader on the off-ramp.
 
          IF(FLEADER(IV) .EQ. 0) THEN
            ILD = 0
            IRL = OFFRAMP_LINK(IL)
            ILN = EXIT_LANE(IL, ILANE)
            DO I = HIGHEST_INDEX_F, 1, -1
              IVN = SORTED_LIST(I)
              IF(IVN .EQ. 0 .OR. IVN .EQ. IV) CYCLE
              IF(FLINK(IVN) .EQ. IRL .AND. FLANE(IVN) .EQ. ILN) THEN
                ILD = IVN
                EXIT
              ENDIF
            ENDDO
            IF(ILD .NE. 0) THEN
              FLEADER(IV) = ILD
              FFOLLOWER(ILD) = IV
            ENDIF
          ENDIF
        ENDIF
        
      ELSE
        IF(FTHRU_LINK(IL) .EQ. DESTINATION(IV)) THEN
          WILL_EXIT = .TRUE.
        ELSEIF(OFFRAMP_LINK(IL) .EQ. DESTINATION(IV)) THEN
          IF(EXIT_LANE(IL, ILANE) .NE. 0) THEN
            WILL_EXIT = .TRUE.
          ELSE
 
! --- Vehicle will miss the exit. Find the next exit.
               
            FTURNCODE(IV) = TC_THRU
            DESTINATION(IV) = 0
            ILNEXT = FTHRU_LINK(IL)
            DO WHILE(ILNEXT .NE. 0)
              IF(OFFRAMP_LINK(ILNEXT) .NE. 0) THEN
                DESTINATION(IV) = OFFRAMP_LINK(ILNEXT)
                EXIT
              ENDIF
              ILAST = ILNEXT
              ILNEXT = FTHRU_LINK(ILNEXT)
            ENDDO
            IF(DESTINATION(IV) .EQ. 0) THEN
              DESTINATION(IV) = ILAST
              DO I = 1, N_FREEWAY_LANES
                IF(RECEIVING_LANE(IL, I) .NE. 0) THEN
                  IF(FLANECODES(IV, I) .NE. LC_NULL) THEN
                    IF(FLANECODES(IV, I) .EQ. LC_VACATE) THEN
                      FLANECODES(IV, I) = LC_GOOD
                    ELSEIF(FLANECODES(IV, I) .EQ. LC_INC_VACATE) THEN
                      FLANECODES(IV, I) = LC_INC_GOOD
                    ENDIF
                  ENDIF
                ELSE
                  FLANECODES(IV, I) = LC_NULL
                ENDIF
              ENDDO
            ELSE
              CALL FIND_EXIT_LANES(IV, FTHRU_LINK(IL), ILNEXT)
              IF(DISTANCE_TO_SEGMENT_END(IV) .LT. OFFRAMP_WARN_DISTANCE(ILNEXT)) THEN
                IF(OFFRAMP_SENDING_LANE(ILNEXT) .EQ. 1 .OR. &
                   OFFRAMP_SENDING_LANE(ILNEXT) .GE. 16) THEN
                  FTURNCODE(IV) = TC_RIGHT
                ELSE
                  FTURNCODE(IV) = TC_LEFT
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      IF(.NOT. WILL_EXIT) THEN
        IF(FTHRU_LINK(IL) .NE. 0) THEN
          IF(NODE_TYPE(FDSN(FTHRU_LINK(IL))) .EQ. NT_EXTERN) WILL_EXIT = .TRUE.
        ENDIF
      ENDIF
      IF(WILL_EXIT) THEN
 
! --- The vehicle will exit onto an off-ramp or an exit link.
 
        WARNED_EXIT(IV) = .FALSE.
        IF(NODE_TYPE(FDSN(FTHRU_LINK(IL))) .NE. NT_EXTERN .OR. DUMMY_EXIT(FDSN(IL))) THEN
          IF(OFFRAMP_LINK(IL) .EQ. DESTINATION(IV) .OR. DUMMY_EXIT(FDSN(IL))) THEN

! --- The vehicle will exit onto an off-ramp.
 
            FDIVERTED(IV) = .FALSE.
            FLINK(IV) = OFFRAMP_LINK(IL)
            FLANE(IV) = EXIT_LANE(IL, ILANE)
!c
!! --- Update the leader and follower in the lane being vacated.
!c
!                IF(FLEADER(IV) .NE. 0) THEN
!                  IF(FFOLLOWER(FLEADER(IV)) .EQ. IV) THEN
!                    FFOLLOWER(FLEADER(IV)) = 0
!                  ENDIF
!                ENDIF
!                IF(FFOLLOWER(IV) .NE. 0) THEN
!                  IF(FLEADER(FFOLLOWER(IV)) .EQ. IV) THEN
!                    FLEADER(FFOLLOWER(IV)) = 0
!                  ENDIF
!                ENDIF
            IF(SEGMENT(FLINK(IV)) .NE. ISEGMENT(IV)) THEN
              ISEGMENT(IV) = SEGMENT(FLINK(IV))
              DISTANCE_TO_SEGMENT_END(IV) = USN_TO_SEG_END(FLINK(IV)) - FLOCATION(IV)            
              CALL SET_NEXT_OBJECT(IV)
              CALL CHOOSE_DESTINATION(IV)
 
! --- If the vehicle does not have a leader search for a leader on the off-ramp.
! --- If the vehicle has a leader but the leader is not on the off-ramp ignore that leader.
 
              ILD = FLEADER(IV)
              IF(ILD .NE. 0) THEN
                IF(FLINK(ILD) .NE. FLINK(IV)) ILD = 0
              ENDIF
              IF(ILD .EQ. 0) THEN
                DO I = HIGHEST_INDEX_F, 1, -1
                  IVN = SORTED_LIST(I)
                  IF(IVN .EQ. 0 .OR. IVN .EQ. IV) CYCLE
                  IF(FLINK(IVN) .EQ. FLINK(IV) .AND. FLANE(IVN) .EQ. FLANE(IV)) THEN
                    ILD = IVN
                    EXIT
                  ENDIF
                ENDDO
                IF(ILD .EQ. 0) THEN
                  ILANE = EXIT_LANE(IL, ILANE)
                  IF(ILANE .NE. 0) THEN
                    IPX = HIGHEST_INDEX_F
                    CALL FIND_FREEWAY_LEADER(IV, IPX, ILANE, ILD)
                  ENDIF
                ENDIF
                FLEADER(IV) = ILD
                IF(ILD .NE. 0) FFOLLOWER(ILD) = IV
              ENDIF
            ELSE
              IF(FLEADER(IV) .NE. 0) THEN
                IF(FLINK(FLEADER(IV)) .EQ. FLINK(IV)) THEN
                  IF(FLANE(FLEADER(IV)) .EQ. FLANE(IV)) FFOLLOWER(FLEADER(IV)) = IV
                ENDIF
              ELSE
                IPX = SORT_POSITION(IV)
                CALL FIND_FREEWAY_LEADER(IV, IPX, ILANE, ILD)
                FLEADER(IV) = ILD
                IF(ILD .NE. 0) FFOLLOWER(ILD) = IV
              ENDIF
            ENDIF
 
! --- Update lane codes for the new link.
 
            CALL TRANSLATE_FREEWAY_LANECODES(IV, IL, FLINK(IV))
            FTURNCODE(IV) = TC_THRU
          ELSE
            IF(NODE_TYPE(FDSN(FTHRU_LINK(IL))) .EQ. NT_EXTERN) THEN
              IF(FFLEET(IV) .EQ. FLEET_BUS .AND. .NOT. INITMODE) THEN
                RID = FROUTEID(IV)
                BUSR_TRIPS(RID) = BUSR_TRIPS(RID) + 1
                BUSR_PERSONTRIPS(RID) = BUSR_PERSONTRIPS(RID) + AVG_OCCS(FVTYPE(IV))
              ENDIF
              IF(FFLEET(IV) .EQ. FLEET_EV) EV_COUNT = EV_COUNT - 1
              IF(WRITE_SUPPLEMENTAL_FILES) CALL WRITE_VEHICLE_EXIT_TEXT(FID(IV), FDSN(FTHRU_LINK(IL)))
              CALL DELETE_FREEWAY_VEHICLE(IV)
            ELSE
              FLINK(IV) = FTHRU_LINK(IL)
              FLANE(IV) = RECEIVING_LANE(IL, ILANE)
            ENDIF
          ENDIF
        ELSE
 
! --- The vehicle is leaving the network.
 
          IF(FFLEET(IV) .EQ. FLEET_BUS) THEN
            RID = FROUTEID(IV)
            BUSR_TRIPS(RID) = BUSR_TRIPS(RID) + 1
            BUSR_PERSONTRIPS(RID) = BUSR_PERSONTRIPS(RID) + AVG_OCCS(FVTYPE(IV))
          ENDIF
          IF(FFLEET(IV) .EQ. FLEET_EV) EV_COUNT = EV_COUNT - 1
          IF(WRITE_SUPPLEMENTAL_FILES) CALL WRITE_VEHICLE_EXIT_TEXT(FID(IV), FDSN(FTHRU_LINK(IL)))
          CALL DELETE_FREEWAY_VEHICLE(IV)
        ENDIF
      ELSE
        IF(NODE_TYPE(FUSN(IL)) .NE. NT_INTERN) THEN
 
! --- The vehicle has entered from an interface. Move it to the next link.
              
          FLINK(IV) = FTHRU_LINK(IL)
          FLANE(IV) = RECEIVING_LANE(IL, ILANE)
        ELSEIF(NODE_TYPE(FDSN(IL)) .NE. NT_INTERN) THEN
 
! --- The vehicle will exit onto a street link. Store it in a holding array to be
! --- processed. Determine deceleration to be applied to the follower.
 
          IF(FCROSS1(FDSN(IL), FLANE(IV)) .EQ. 0) THEN
            FCROSS1(FDSN(IL), FLANE(IV)) = IV
          ELSE
            FCROSS2(FDSN(IL), FLANE(IV)) = IV
          ENDIF
          IF(FFOLLOWER(IV) .NE. 0) THEN
            IVN = FFOLLOWER(IV)
            IF(FLINK(IVN) .EQ. FLINK(IV)) THEN

! --- The location of the subject vehicle is now on the receiving link. Add the length of the previous link back in.

              FLOCATION(IV) = FLOCATION(IV) + FLENGTH(FLINK(IV))
              ACCEL = 0.
              CALL FREEWAY_CAR_FOLLOW(IVN, IV, ACCEL)
              FDECEL(IVN) = MAX(FDECEL(IVN), ACCEL)

! --- Restore the location to the original value.

              FLOCATION(IV) = FLOCATION(IV) - FLENGTH(FLINK(IV))
            ENDIF
 
! --- Notify the follower vehicle that it has no leader.
 
            IF(FLEADER(FFOLLOWER(IV)) .EQ. IV) THEN
              FLEADER(FFOLLOWER(IV)) = 0
            ENDIF
            FFOLLOWER(IV) = 0
          ENDIF
        
        ELSEIF(DUMMY_ENTRY(FDSN(IL))) THEN
          FLINK(IV) = FTHRU_LINK(IL)
          IF(SEGMENT(FLINK(IV)) .NE. ISEGMENT(IV)) THEN
            ISEGMENT(IV) = SEGMENT(FLINK(IV))
            DISTANCE_TO_SEGMENT_END(IV) = USN_TO_SEG_END(FLINK(IV)) - FLOCATION(IV)
            CALL SET_NEXT_OBJECT(IV)
          ENDIF
          
        ELSEIF(NODE_TYPE(FDSN(FTHRU_LINK(IL))) .NE. NT_EXTERN) THEN
 
! --- The vehicle will proceed to the next link.
 
          FLINK(IV) = FTHRU_LINK(IL)
          FLANE(IV) = RECEIVING_LANE(IL, ILANE)
          IF(RAMP_MERGE_LINK(IL)) THEN
            ISEGMENT(IV) = SEGMENT(FLINK(IV))
            DISTANCE_TO_SEGMENT_END(IV) = USN_TO_SEG_END(FLINK(IV)) - FLOCATION(IV)
            CALL SET_NEXT_OBJECT(IV)
          ENDIF
        ENDIF
 
! --- Define lane codes for the new link.
 
        IF(FLINK(IV) .NE. 0) THEN
          IF(NODE_TYPE(FUSN(FLINK(IV))) .EQ. NT_INTERN) THEN
            CALL TRANSLATE_FREEWAY_LANECODES(IV, IL, FLINK(IV))
          ENDIF
        ENDIF
      ENDIF
    ENDIF      
 
    IL = FLINK(IV)
    IF(IL .NE. 0) THEN
 
! --- Increment the number of vehicles entering the new link.
       
      FENTERING(IL) = FENTERING(IL) + 1
 
! --- Define desired speed for the new link and update the distance
! --- to the end of the segment.
 
      CALL SET_DESIREDSPEED_FREEWAY(IV)
      DISTANCE_TO_SEGMENT_END(IV) = USN_TO_SEG_END(IL) - FLOCATION(IV)
    ELSE
 
! --- If the vehicle had a follower notify that vehicle that it has no leader.
 
      IF(FFOLLOWER(IV) .NE. 0) THEN
        IF(FLEADER(FFOLLOWER(IV)) .EQ. IV) FLEADER(FFOLLOWER(IV)) = 0
      ENDIF
 
! --- Remove the vehicle from the vehicle arrays.
               
      IF(FFLEET(IV) .EQ. FLEET_EV) EV_COUNT = EV_COUNT - 1
      CALL DELETE_FREEWAY_VEHICLE(IV)
    ENDIF
  ENDIF
    
  IF(FTURNCODE(IV) .GT. TC_RIGHT) THEN
    IF(DESTINATION(IV) .EQ. OFFRAMP_LINK(ILINK)) THEN
 
! --- If the vehicle was assigned a diagonal turn code change it
! --- to either left or right.
 
      IF(FTURNCODE(IV) .EQ. TC_LDIAG) THEN
        FTURNCODE(IV) = TC_LEFT
      ELSEIF(FTURNCODE(IV) .EQ. TC_RDIAG) THEN
        FTURNCODE(IV) = TC_RIGHT
      ENDIF
    ENDIF
  ENDIF
  
  RETURN
  END

! ==================================================================================================
  SUBROUTINE PROCESS_STREET_NODE(IV, ILINK)                     
! ----------------------------------------------------------------------
! --- Change lane codes when a vehicle passes a node.
! ----------------------------------------------------------------------
  USE SIMPARAMS
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE VEHICLE_TYPES
  USE BUS_STATION_DATA
  USE BUS_ROUTE_DATA
  USE GLOBAL_DATA
  USE ACTUATED_CONTROLLERS
  USE STREET_NODES
  USE SEEDS
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, ILINK
  INTEGER :: IL, ILANE, ILD, ILNEXT
  INTEGER :: ITYPE, RID, ITURN, IREC, IACT, PHASE, INTCH
  REAL :: RNDNUM
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

  TURN_INDICATOR(IV) = TC_NULL
  SGO_THRU_SIGNAL(IV) = .FALSE.
  SPREVLANE(IV) = SLANE(IV)
  SPREVLINK(IV) = SLINK(IV)
  PREV_TURNCODE(IV) = STURNCODE(IV)
  IF(SROUTEID(IV) .NE. 0 .OR. SPATHID(IV) .NE. 0) SPATHPOINT(IV) = SPATHPOINT(IV) + 1
  IL = SLINK(IV)
  ILANE = SLANE(IV)
  ILNEXT = ILINK
  IF(ILINK .EQ. ROUNDABOUT_EXIT(IV)) THEN
    ROUNDABOUT_EXIT(IV) = 0
  ENDIF
 
! --- Store the ID of the last vehicle to exit from the link.
 
  SLAST_VEHICLE_OUT(IL, ILANE) = IV
  SLAST_VEHICLE_OUT_ID(IL, ILANE) = SID(IV)
 
! --- Increment the number of vehicles discharged from the prior link.
 
  IF(.NOT. INITMODE) THEN
    SDISCHARGED(IL) = SDISCHARGED(IL) + 1
    IF(SFLEET(IV) .EQ. FLEET_BUS) SDISCHARGED_BUSES(IL) = SDISCHARGED_BUSES(IL) + 1
    ITYPE = SVTYPE(IV)
    SDISCHARGED_TYPE(IL, ITYPE) = SDISCHARGED_TYPE(IL, ITYPE) + 1
    SDISCHARGED_LANE(IL, ILANE) = SDISCHARGED_LANE(IL, ILANE) + 1
    IF(STURNCODE(IV) .EQ. TC_LEFT) THEN
      DISCHARGED_LEFT(IL) = DISCHARGED_LEFT(IL) + 1
    ELSEIF(STURNCODE(IV) .EQ. TC_RIGHT) THEN
      DISCHARGED_RIGHT(IL) = DISCHARGED_RIGHT(IL) + 1
    ELSEIF(STURNCODE(IV) .GT. TC_RIGHT) THEN
      DISCHARGED_DIAG(IL) = DISCHARGED_DIAG(IL) + 1
    ENDIF
    SPERSON_TRIPS(IL) = SPERSON_TRIPS(IL) + AVG_OCCS(ITYPE)
    IACT = NACT(SDSN(IL))
    IF(IACT .NE. 0) THEN
      IF(STURNCODE(IV) .EQ. TC_LEFT) THEN
        PHASE = PHASE_LEFT(IL)
      ELSEIF(STURNCODE(IV) .EQ. TC_THRU) THEN
        PHASE = PHASE_THRU(IL)
      ELSEIF(STURNCODE(IV) .EQ. TC_RIGHT) THEN
        PHASE = PHASE_RIGHT(IL)
      ELSE
        PHASE = PHASE_DIAG(IL)
      ENDIF
      IF(PHASE .NE. 0) THEN
        AC_SIGNALS(IACT)%VEHICLES_DISCHARGED(PHASE) = AC_SIGNALS(IACT)%VEHICLES_DISCHARGED(PHASE) + 1
      ENDIF                
    ENDIF
  ENDIF
       
! --- Store the average speed of the link for use by the anticipatory lane
! --- change logic.
! --- (Temporarily just use the speed of the last vehicle to represent the average.)
       
  IF(IL .NE. 0) THEN
    IF(NODE_TYPE(SUSN(IL)) .EQ. NT_EXTERN) THEN
 
! --- The vehicle is coming from an entry link.
 
      SLAST_VEHICLE(IL, SLANE(IV)) = 0
      SXCODE(IV) = 1
      FIRST_VEHICLE(SLINK(IV), SLANE(IV)) = 0
      SLINK(IV) = STHRU_LINK(IL)
      IF(SLEADER(IV) .NE. 0) THEN
        ILD = SLEADER(IV)
        SLOCATION(IV) = MIN(SLOCATION(IV), SLOCATION(ILD) - SVLENGTH(ILD) - 3.0)
      ENDIF
 
! --- Determine the vehicle's turn code.
           
      IF(STURNCODE(IV) .NE. TC_THRU) THEN
        CALL FIND_NEXT_TURN(IV)
      ELSEIF(ILINK .EQ. TURN_LINK(IV)) THEN
        STURNCODE(IV) = TURN_CODE(IV)
      ENDIF
      IF(ROUNDABOUT_APPROACH_NUM(ILINK) .NE. 0) THEN
        !The vehicle is entering a roundabout.
        !Determine which exit the vehicle will use.
        CALL FIND_NEXT_TURN(IV)
        CALL CHOOSE_ROUNDABOUT_EXIT(ILINK, IV)
      ENDIF
      
! --- Define lane codes for the new link.
           
      IF(SLINK(IV) .NE. 0) THEN
        CALL TRANSLATE_STREET_LANECODES(IV, SLINK(IV))
        CALL SET_GOAL_LANE(IV)
      ENDIF
    ELSE
 
! --- This is a vehicle on a street.
 
      SLINK(IV) = ILINK
      !ITURN = STURNCODE(IV)
      !IF(ITURN .EQ. TC_LEFT) THEN
      !  IREC = LEFT_LINK(IL)
      !ELSEIF(ITURN .EQ. TC_THRU) THEN
      !  IREC = STHRU_LINK(IL)
      !ELSEIF(ITURN .EQ. TC_RIGHT) THEN
      !  IREC = RIGHT_LINK(IL)
      !ELSEIF(ITURN .EQ. TC_LDIAG) THEN
      !  IREC = LEFT_DIAG_LINK(IL)
      !ELSEIF(ITURN .EQ. TC_RDIAG) THEN
      !  IREC = RIGHT_DIAG_LINK(IL)
      !ENDIF 
      IF(IREC .NE. 0) THEN
        IF(NODE_TYPE(SDSN(IREC)) .EQ. NT_EXTERN) THEN
          IF(SFLEET(IV) .EQ. FLEET_BUS .AND. .NOT. INITMODE) THEN
            RID = SROUTEID(IV)
            BUSR_TRIPS(RID) = BUSR_TRIPS(RID) + 1
            BUSR_PERSONTRIPS(RID) = BUSR_PERSONTRIPS(RID) + AVG_OCCS(SVTYPE(IV))
          ENDIF
        ENDIF
      ENDIF
      !Determine if the vehicle is entering a roundabout
      IF(ROUNDABOUT_EXIT(IV) .NE. 0) THEN 
        !The vehicle is currently traveling in a roundabout.
        IF(RIGHT_LINK(ILINK) .EQ. ROUNDABOUT_EXIT(IV)) THEN
          STURNCODE(IV) = TC_RIGHT
          ROUNDABOUT_EXIT(IV) = 0
        ELSEIF(RIGHT_DIAG_LINK(ILINK) .EQ. ROUNDABOUT_EXIT(IV)) THEN
          STURNCODE(IV) = TC_RDIAG
          ROUNDABOUT_EXIT(IV) = 0
        ELSEIF(STHRU_LINK(ILINK) .EQ. ROUNDABOUT_EXIT(IV)) THEN
          STURNCODE(IV) = TC_THRU
          ROUNDABOUT_EXIT(IV) = 0
        ELSEIF(LEFT_DIAG_LINK(ILINK) .EQ. ROUNDABOUT_EXIT(IV)) THEN
          STURNCODE(IV) = TC_LDIAG
          ROUNDABOUT_EXIT(IV) = 0
        ELSEIF(LEFT_LINK(ILINK) .EQ. ROUNDABOUT_EXIT(IV)) THEN
          STURNCODE(IV) = TC_LEFT
          ROUNDABOUT_EXIT(IV) = 0
        ELSE
          IF(LEFT_LINK(ILINK) .NE. 0) THEN
            STURNCODE(IV) = TC_LEFT
          ELSEIF(LEFT_DIAG_LINK(ILINK) .NE. 0) THEN
            STURNCODE(IV) = TC_LDIAG
          ELSEIF(STHRU_LINK(ILINK) .NE. 0) THEN
            STURNCODE(IV) = TC_THRU
          ELSEIF(RIGHT_DIAG_LINK(ILINK) .NE. 0) THEN
            STURNCODE(IV) = TC_RDIAG
          ELSEIF(RIGHT_LINK(ILINK) .NE. 0) THEN
            STURNCODE(IV) = TC_RIGHT
          ENDIF
        ENDIF
      ELSEIF(ROUNDABOUT_APPROACH_NUM(ILINK) .NE. 0) THEN
        !The vehicle is entering a roundabout.
        !Determine which exit the vehicle will use.
        CALL FIND_NEXT_TURN(IV)
        CALL CHOOSE_ROUNDABOUT_EXIT(ILINK, IV)
      ELSEIF(STURNCODE(IV) .NE. TC_THRU) THEN
        CALL FIND_NEXT_TURN(IV)
      ELSEIF(ILINK .EQ. TURN_LINK(IV)) THEN
        STURNCODE(IV) = TURN_CODE(IV)
      ENDIF

! --- Define lane codes for the new link.
 
      IF(SLINK(IV) .NE. 0) THEN
        CALL TRANSLATE_STREET_LANECODES(IV, SLINK(IV))
        CALL SET_GOAL_LANE(IV)
      ENDIF
    ENDIF      
 
    IL = SLINK(IV)
    IF(IL .NE. 0) THEN
 
! --- Increment the number of vehicles entering the new link.
      
      SENTERING(IL) = SENTERING(IL) + 1
 
! --- Define desired speed for the new link and update the distance
! --- to the end of the segment.
 
      CALL SET_DESIREDSPEED_STREET(IV)
    ELSE
 
! --- If the vehicle had a follower notify that vehicle that
! --- it has no leader.
 
      IF(SFOLLOWER(IV) .NE. 0) THEN
        IF(SLEADER(SFOLLOWER(IV)) .EQ. IV) SLEADER(SFOLLOWER(IV)) = 0
      ENDIF
 
! --- Remove the vehicle from the vehicle arrays.
               
      IF(SFLEET(IV) .EQ. FLEET_EV) EV_COUNT = EV_COUNT - 1
      IF(WRITE_SUPPLEMENTAL_FILES) CALL WRITE_VEHICLE_EXIT_TEXT(SID(IV), SDSN(STHRU_LINK(IL)))
      CALL DELETE_STREET_VEHICLE(IV, .FALSE.)
    ENDIF
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE NEW_STREET_VEHICLE(FV)
! ----------------------------------------------------------------------
! --- Generate a new street vehicle on an interface link.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE STREET_VEHICLES
  USE FREEWAY_LINKS
  USE STREET_LINKS
  USE BUS_ROUTE_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: FV
  INTEGER :: ILANE, ILD, INODE, SV, IL
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = fid(fv)
#endif
  CALL GET_NEXT_SINDEX(SV)
  CALL DELETE_STREET_VEHICLE(SV, .FALSE.)
  SID(SV) = FID(FV)
  SCF_MODEL(SV) = FCF_MODEL(FV)
  SEV_OVRSPD(SV) = FEV_OVRSPD(FV)
  SEV_RANGE(SV) = FEV_RANGE(FV)
  SPREVLINK(SV) = 0
  INODE = FDSN(FLINK(FV))
  DO IL = 1, N_STREET_LINKS
    IF(SUSN(IL) .EQ. INODE) EXIT
  ENDDO
  SLINK(SV) = IL
  SLANE(SV) = FLANE(FV)
  SFLEET(SV) = FFLEET(FV)
  SVTYPE(SV) = FVTYPE(FV)
  SDRIVERTYPE(SV) = FDRIVERTYPE(FV)
  DRIVER_TYPE(SV) = FDRIVERTYPE(FV)
  SVLENGTH(SV) = FVLENGTH(FV)
  SSPEED(SV) = FSPEED(FV)
  CALL SET_DESIREDSPEED_STREET(SV)
  SROUTEID(SV) = FROUTEID(FV)
  SPATHID(SV) = FPATHID(FV)
  SPSAVE(SV) = FPSAVE(FV)
  IF(SPATHID(SV) .NE. 0) THEN
    SPATHPOINT(SV) = FPATHPOINT(FV) - 1
  ELSEIF(SROUTEID(SV) .NE. 0) THEN
    IF(SAVE_NEXT_STOP(FV) .NE. 0) THEN
      NEXT_STOP(SV) = SAVE_NEXT_STOP(FV)
    ELSE
      NEXT_STOP(SV) = 1
    ENDIF
    SPATHPOINT(SV) = FPATHPOINT(FV)
  ENDIF
  SXCODE(SV) = 1
  ILANE = SLANE(SV)
  SLOCATION(SV) = 0.
  CALL FIND_STREET_LEADER(SV, IL, ILANE, ILD)
  SLOCATION(SV) = FLOCATION(FV)
  SLEADER(SV) = ILD
  SFOLLOWER(SV) = 0
  IF(ILD .NE. 0) THEN
    SFOLLOWER(ILD) = SV
  ENDIF
  SDIVERTED(SV) = FDIVERTED(FV)
  CALL FIND_NEXT_TURN(SV)
  IF(SLINK(SV) .NE. 0) THEN
 
! --- Update first and last vehicle for the new link.
 
    SLAST_VEHICLE(IL, ILANE) = SV
    IF(FIRST_VEHICLE(IL, ILANE) .EQ. 0) FIRST_VEHICLE(IL, ILANE) = SV
 
! --- Define lane codes for the new link.
 
    SLANECODES(SV, 1:N_STREET_LANES) = LC_NULL
    DO ILANE = FIRST_FULL_LANE(IL), LAST_FULL_LANE(IL)
      SLANECODES(SV, ILANE) = LC_GOOD
    ENDDO
 
! --- Determine which lanes to use on the interface link.
           
    CALL TRANSLATE_STREET_LANECODES(SV, SLINK(SV))
    CALL SET_GOAL_LANE(SV)
  ENDIF
  SXCODE(SV) = 1
  CALL DELETE_FREEWAY_VEHICLE(FV)
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE NEW_FREEWAY_VEHICLE(SV)
! ----------------------------------------------------------------------
! --- Generate a new freeway vehicle on an interface link.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE STREET_VEHICLES
  USE FREEWAY_LINKS
  USE STREET_LINKS
  USE OBJECTS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: SV
  INTEGER :: IPOS, ILD, IOBJ, ILANE, INODE, FV, IL, IV, I
  REAL :: DMAX
  LOGICAL RECALL
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
#endif
  CALL GET_NEXT_FINDEX(FV)
  CALL DELETE_FREEWAY_VEHICLE(FV)
  FID(FV) = SID(SV)
  FCF_MODEL(FV) = SCF_MODEL(SV)
#ifdef DebugVersion
  temp = fid(fv)
#endif
  FEV_OVRSPD(FV) = SEV_OVRSPD(SV)
  FEV_RANGE(FV) = SEV_RANGE(SV)
  FPREVLINK(FV) = 0
  INODE = SDSN(SLINK(SV))
  DO IL = 1, N_FREEWAY_LINKS
    IF(FUSN(IL) .EQ. INODE) EXIT
  ENDDO      
  FLINK(FV) = IL
  FLANE(FV) = SLANE(SV)
  FFLEET(FV) = SFLEET(SV)
  FVTYPE(FV) = SVTYPE(SV)
  FDRIVERTYPE(FV) = SDRIVERTYPE(SV)
  FVLENGTH(FV) = SVLENGTH(SV)
  FSPEED(FV) = SSPEED(SV)
  CALL SET_DESIREDSPEED_FREEWAY(FV)
  FTURNCODE(FV) = TC_THRU
  FROUTEID(FV) = SROUTEID(SV)
 
! --- If the vehicle was previously diverted from the freeway
! --- restore the path ID.
       
  IF(SPSAVE(SV) .NE. 0 .AND. SPATHID(SV) .EQ. 0) THEN
    FPATHID(FV) = SPSAVE(SV)
  ELSE
    FPATHID(FV) = SPATHID(SV)
  ENDIF
  IF(SROUTEID(SV) .NE. 0 .OR. SPATHID(SV) .NE. 0) THEN
    FPATHPOINT(FV) = SPATHPOINT(SV) + 1
    SAVE_NEXT_STOP(FV) = NEXT_STOP(SV)
  ENDIF
 
! --- Define the distance to the end of the segment and identify
! --- the first object that would affect the vehicle.
 
  DISTANCE_TO_SEGMENT_END(FV) = USN_TO_SEG_END(IL) - SLOCATION(SV)
  FLOCATION(FV) = SLOCATION(SV)
  ISEGMENT(FV) = SEGMENT(IL)
  NEXT_OBJECT(FV) = 0
  DO IOBJ = NUMBER_OF_OBJECTS, 1, -1
    IF(OBJECT_LIST(IOBJ)%LINK .EQ. IL) THEN
      NEXT_OBJECT(FV) = IOBJ
      EXIT
    ENDIF
  ENDDO
  
! --- Add the vehicle to the end of the freeway vehicle list.
 
  CALL CHOOSE_DESTINATION(FV)
  CALL ADD_TO_VEHICLE_LIST(FV)
  
! --- Try to find a leader

  ILD = 0
  DMAX = DISTANCE_TO_SEGMENT_END(FV) + FVLENGTH(FV)
  IPOS = SORTED_LIST_LENGTH
  DO I = IPOS, 1, -1
    IV = SORTED_LIST(I)
    IF(IV .EQ. 0 .OR. IV .EQ. FV) CYCLE
    IF(DISTANCE_TO_SEGMENT_END(IV) + FVLENGTH(IV) .GE. DMAX) IPOS = I
    IF(FLINK(IV) .EQ. IL .AND. FLANE(IV) .EQ. FLANE(FV)) THEN
      ILD = IV
      EXIT
    ENDIF
  ENDDO
  IF(ILD .EQ. 0) CALL FIND_FREEWAY_LEADER(FV, IPOS, FLANE(FV), ILD)
  IF(ILD .NE. 0) THEN
    FLEADER(FV) = ILD
    FFOLLOWER(ILD) = FV
  ELSE
    FLEADER(FV) = 0
  ENDIF
 
! --- Define lane codes for the new link.
 
  FLANECODES(FV, 1:N_FREEWAY_LANES) = LC_NULL
  DO ILANE = 1, FNUMLANES(IL)
    FLANECODES(FV, ILANE) = LC_GOOD
  ENDDO
  FLAST_VEHICLE(IL, FLANE(FV)) = FV
  CALL PROCESS_OBJECTS_PASSED(FV, RECALL)
  FXCODE(FV) = 1
  CALL DELETE_STREET_VEHICLE(SV, .FALSE.)
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE CHECK_RAMPMETER(IV, ACCEL)
! ----------------------------------------------------------------------
! --- Determine if the vehicle must stop for a ramp meter.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE RAMP_METERS
  USE SIMPARAMS
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  REAL, INTENT(INOUT) :: ACCEL
  INTEGER :: IL, METER, IFL
  REAL :: A, D
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = fid(iv)
#endif
  A = ACCEL
  IL = FLINK(IV)
  METER = RAMPMETER(IL)
  IF(WEVRUN .AND. FFOLLOWER(IV) .NE. 0) THEN
    IF(FFLEET(FFOLLOWER(IV)) .EQ. FLEET_EV) THEN
      IF(FSPEED(FFOLLOWER(IV)) .LE. STOP_SPD) THEN
        FGO_THRU_SIGNAL(IV) = .TRUE.
      ENDIF
    ENDIF
  ENDIF
  IF(RAMPMETERS(METER)%STATE .NE. 0) THEN
 
! --- The meter is active.
      
    FDISCH_TIMER(IV) = FDISCH_TIMER(IV) - 1
 
! --- If the vehicle has been allowed to proceed on a previous time step
! --- decrement the discharge timer. When the timer reaches zero 
! --- discharge the vehicle; until then skip processing the signal.
 
    IF(FGO_THRU_SIGNAL(IV)) THEN
      IF(FDISCH_TIMER(IV) .LE. 0) THEN
        FGO_THRU_SIGNAL(IV) = .FALSE.
        RETURN
      ELSE
        ACCEL = -FSPEED(IV)
        RETURN
      ENDIF
    ENDIF
         
    IF(IMETER(IV) .GE. 0) THEN
 
! --- The vehicle has not yet stopped for the signal.
       
      D = FLENGTH(IL) - FLOCATION(IV)
      IF(D .GT. 10 .AND. FSPEED(IV) .GT. 0) THEN
 
! --- The vehicle is still approaching the stopbar.
       
        IMETER(IV) = 1
        
        !Slow the vehicle down to 5 mph.
        IF(FSPEED(IV) .GT. 7.3) THEN
          ACCEL = -(FSPEED(IV) ** 2) / (2 * D)
          ACCEL = MIN(A, ACCEL)
        ENDIF
      ELSE
 
! --- The vehicle is close to the stopbar.
       
        IF(RAMPMETERS(METER)%STATE .EQ. MS_RED) THEN
 
! --- The signal is red. The vehicle must stop.
       

          ACCEL = -FSPEED(IV) / 2.
          IF(D .LT. 3) ACCEL = -FSPEED(IV)
        ELSE
 
! --- The signal is green. If the vehicle has stopped it may
! --- accelerate. If it has not stopped it must continue
! --- decelerating and will discharge on the next green.
       
          IF(FSPEED(IV) .LE. STOP_SPD .OR. D .LT. 3) THEN
            ACCEL = -FSPEED(IV)
            FDISCH_TIMER(IV) = FSTARTUP_TIME(IL)
            FGO_THRU_SIGNAL(IV) = .TRUE.
            IMETER(IV) = -1
            IF(RAMPMETERS(METER)%TWO_PERGREEN) THEN
              IF(FFOLLOWER(IV) .NE. 0) THEN
                IFL = FFOLLOWER(IV)
                IF(FLINK(IV) .EQ. FLINK(IFL)) THEN
                  IF(FLOCATION(IV) - FVLENGTH(IV) - FLOCATION(IFL) .LE. 35) THEN
                    FGO_THRU_SIGNAL(IFL) = .TRUE.
                    IMETER(IFL) = -1
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ELSE
            ACCEL = -(FSPEED(IV) ** 2) / (2 * D)
            ACCEL = MIN(A, ACCEL)
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE FIND_NEXT_TURN(IV)
! ----------------------------------------------------------------------
! --- Determine the next turn that the vehicle will make. Store the
! --- link ID and the turn code. 
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE BUS_ROUTE_DATA
  USE PATH_MOD
  USE GLOBAL_DATA
  USE SEEDS
  USE NODE_TABLE
  USE TIMED_CONTROLLERS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER :: IL, TCODE, IN, N1, N2, NL
  REAL :: RNDNUM
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif
  TURN_LINK(IV) = 0
  IL = SLINK(IV)
  
  IF(SFLEET(IV) .EQ. FLEET_BIKE) THEN
    TURN_CODE(IV) = TC_THRU

  ELSEIF(SROUTEID(IV) .NE. 0) THEN
    DO IN = SPATHPOINT(IV) + 1, BUSR_NNODES(SROUTEID(IV)) - 1
      N1 = BUSR_ROUTE_NODES(SROUTEID(IV), IN)
      N2 = BUSR_ROUTE_NODES(SROUTEID(IV), IN+1)
      CALL FIND_STREET_LINK(N1, N2, NL)
      IF(NL .NE. 0 .AND. NL .EQ. STHRU_LINK(IL)) THEN
        IL = NL
        CYCLE
      ELSE
        TURN_LINK(IV) = IL
        IF(NL .EQ. 0) THEN
          TURN_CODE(IV) = TC_THRU
        ELSEIF(NL .EQ. LEFT_LINK(IL)) THEN
          TURN_CODE(IV) = TC_LEFT
        ELSEIF(NL .EQ. STHRU_LINK(IL)) THEN
          TURN_CODE(IV) = TC_THRU
        ELSEIF(NL .EQ. RIGHT_LINK(IL)) THEN
          TURN_CODE(IV) = TC_RIGHT
        ELSEIF(NL .EQ. LEFT_DIAG_LINK(IL)) THEN
          TURN_CODE(IV) = TC_LDIAG
        ELSEIF(NL .EQ. RIGHT_DIAG_LINK(IL)) THEN
          TURN_CODE(IV) = TC_RDIAG
        ENDIF
        EXIT
      ENDIF
    ENDDO
    
  ELSEIF(SPATHID(IV) .NE. 0) THEN
    DO IN = SPATHPOINT(IV) + 1, NNODES(SPATHID(IV))-1
      N1 = PATH_NODES(SPATHID(IV), IN)
      N2 = PATH_NODES(SPATHID(IV), IN+1)
      CALL FIND_STREET_LINK(N1, N2, NL)
      IF(NL .EQ. STHRU_LINK(IL)) THEN
        IL = NL
        CYCLE
      ELSE
        TURN_LINK(IV) = IL
        IF(NL .EQ. 0) THEN
          TURN_CODE(IV) = TC_THRU
        ELSEIF(NL .EQ. LEFT_LINK(IL)) THEN
          TURN_CODE(IV) = TC_LEFT
        ELSEIF(NL .EQ. STHRU_LINK(IL)) THEN
          TURN_CODE(IV) = TC_THRU
        ELSEIF(NL .EQ. RIGHT_LINK(IL)) THEN
          TURN_CODE(IV) = TC_RIGHT
        ELSEIF(NL .EQ. LEFT_DIAG_LINK(IL)) THEN
          TURN_CODE(IV) =  TC_LDIAG
        ELSEIF(NL .EQ. RIGHT_DIAG_LINK(IL)) THEN
          TURN_CODE(IV) =  TC_RDIAG
        ENDIF
        EXIT
      ENDIF
    ENDDO
    
  ELSE
 
! --- If the next turn was determined previously, use it now.
 
    IF(TURN_CODE2(IV) .NE. TC_NULL) THEN
      TURN_LINK(IV) = TURN_LINK2(IV)
      TURN_CODE(IV) = TURN_CODE2(IV)
    ELSE

! --- Randomly determine the first turn.

      TCODE = TURN_CODE(IV)
      DO WHILE(IL .NE. 0)
        CALL STREET_RANDOM(SSEED, RNDNUM)
        RNDNUM = 100 * RNDNUM
        CALL GET_TURNCODE(IV, IL, RNDNUM, TCODE)
        IF(ROUNDABOUT_ID(IL) .NE. 0) THEN
          TURN_LINK(IV) = IL
          TURN_CODE(IV) = TCODE
        ENDIF
        IF(TCODE .EQ. 1) THEN
          IL = STHRU_LINK(IL)
          IF(IL .NE. 0) THEN
            IF(NODE_TYPE(SDSN(IL)) .NE. NT_INTERN) EXIT
          ENDIF
        ELSE
          TURN_LINK(IV) = IL
          TURN_CODE(IV) = TCODE
          EXIT
        ENDIF
      ENDDO
    ENDIF

! --- Determine if the driver knows the second downstream turn.

    TURN_CODE2(IV) = TC_NULL
    IF(TURN_LINK(IV) .NE. 0) THEN
      IF(ROUNDABOUT_ID(TURN_LINK(IV)) .EQ. 0 .OR. ROUNDABOUT_EXIT_NUM(TURN_LINK(IV)) .NE. 0) THEN
        CALL STREET_RANDOM(SSEED, RNDNUM)
        IF(RNDNUM .LE. DRIVER_FAMPCT .AND. IL .NE. 0) THEN
          IF(TURN_CODE(IV) .EQ. TC_LEFT) THEN
            IL = LEFT_LINK(TURN_LINK(IV))
          ELSEIF(TURN_CODE(IV) .EQ. TC_THRU) THEN
            IL = STHRU_LINK(TURN_LINK(IV))
          ELSEIF(TURN_CODE(IV) .EQ. TC_RIGHT) THEN
            IL = RIGHT_LINK(TURN_LINK(IV))
          ELSEIF(TURN_CODE(IV) .EQ. TC_LDIAG) THEN
            IL = LEFT_DIAG_LINK(TURN_LINK(IV))
          ELSEIF(TURN_CODE(IV) .EQ. TC_RDIAG) THEN
            IL = RIGHT_DIAG_LINK(TURN_LINK(IV))
          ENDIF
          DO WHILE(IL .NE. 0)
            CALL STREET_RANDOM(SSEED, RNDNUM)
            RNDNUM = 100 * RNDNUM
            CALL GET_TURNCODE(IV, IL, RNDNUM, TCODE)
            IF(TCODE .EQ. 1) THEN
              IL = STHRU_LINK(IL)
              IF(IL .NE. 0) THEN
                IF(NODE_TYPE(SDSN(IL)) .NE. NT_INTERN) EXIT
              ENDIF
            ELSE
              TURN_LINK2(IV) = IL
              TURN_CODE2(IV) = TCODE
              EXIT
            ENDIF
          ENDDO
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  IF(TURN_LINK(IV) .EQ. SLINK(IV)) THEN
    STURNCODE(IV) = TURN_CODE(IV)
  ELSE
    STURNCODE(IV) = TC_THRU
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE GET_TURNCODE(IV, IL, RNDNUM, TCODE)
! ----------------------------------------------------------------------
! --- Randomly choose the direction that the vehicle will turn when
! --- discharging from the link.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE STREET_VEHICLES
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, IL
  REAL, INTENT(IN) :: RNDNUM
  INTEGER, INTENT(INOUT) :: TCODE
  REAL :: LPCT, TPCT, RPCT, LDPCT, RDPCT, ITYP
! ----------------------------------------------------------------------
 
! --- This implementation fails to maintain RT 21 turn percentages
! --- when partial conditional turn percentages are applied.
 
! --- Get base turn percentages from RT 21 inputs.
 
  LPCT = LEFT_PERCENT(IL)
  TPCT = STHRU_PERCENT(IL)
  RPCT = RIGHT_PERCENT(IL)
  LDPCT = LDIAG_PERCENT(IL)
  RDPCT = RDIAG_PERCENT(IL)
 
! --- Get conditional turn percentages from RT 22/27 inputs when they
! --- exist for the current link and direction of entry.
! --- TCODE is the turn code used to enter the link on input
! --- and is the new turn code on output.
 
  IF(TCODE .EQ. TC_LEFT .AND. COND_LEFT(IL)) THEN
    LPCT = COND_LEFTPCT(IL, 1)
    TPCT = COND_LEFTPCT(IL, 2)
    RPCT = COND_LEFTPCT(IL, 3)
    LDPCT = COND_LEFTPCT(IL, 4)
    RDPCT = COND_LEFTPCT(IL, 5)
  ELSEIF(TCODE .EQ. TC_THRU .AND. COND_THRU(IL)) THEN
    LPCT = COND_THRUPCT(IL, 1)
    TPCT = COND_THRUPCT(IL, 2)
    RPCT = COND_THRUPCT(IL, 3)
    LDPCT = COND_THRUPCT(IL, 4)
    RDPCT = COND_THRUPCT(IL, 5)
  ELSEIF(TCODE .EQ. TC_RIGHT .AND. COND_RIGHT(IL)) THEN
    LPCT = COND_RIGHTPCT(IL, 1)
    TPCT = COND_RIGHTPCT(IL, 2)
    RPCT = COND_RIGHTPCT(IL, 3)
    LDPCT = COND_RIGHTPCT(IL, 4)
    RDPCT = COND_RIGHTPCT(IL, 5)
  ELSEIF(TCODE .EQ. TC_LDIAG .AND. COND_LDIAG(IL)) THEN
    LPCT = COND_LDIAGPCT(IL, 1)
    TPCT = COND_LDIAGPCT(IL, 2)
    RPCT = COND_LDIAGPCT(IL, 3)
    LDPCT = COND_LDIAGPCT(IL, 4)
    RDPCT = COND_LDIAGPCT(IL, 5)
  ELSEIF(TCODE .EQ. TC_RDIAG .AND. COND_RDIAG(IL)) THEN
    LPCT = COND_RDIAGPCT(IL, 1)
    TPCT = COND_RDIAGPCT(IL, 2)
    RPCT = COND_RDIAGPCT(IL, 3)
    LDPCT = COND_RDIAGPCT(IL, 4)
    RDPCT = COND_RDIAGPCT(IL, 5)
  ENDIF
 
! --- Apply multipliers if they exist.
 
  ITYP = SVTYPE(IV)
  LPCT = LPCT * MULTIPLIER_LEFT(IL, ITYP)
  TPCT = TPCT * MULTIPLIER_THRU(IL, ITYP)
  RPCT = RPCT * MULTIPLIER_RIGHT(IL, ITYP)
  LDPCT = LDPCT * MULTIPLIER_LDIAG(IL, ITYP)
  RDPCT = RDPCT * MULTIPLIER_RDIAG(IL, ITYP)
       
  IF(RNDNUM .LT. LPCT) THEN
    TCODE = TC_LEFT
  ELSEIF(RNDNUM .LT. LPCT + TPCT) THEN
    TCODE = TC_THRU
  ELSEIF(RNDNUM .LT. LPCT + TPCT + RPCT) THEN
    TCODE = TC_RIGHT
  ELSEIF(RNDNUM .LT. LPCT + TPCT + RPCT + LDPCT) THEN
    TCODE = TC_LDIAG
  ELSEIF(RNDNUM .LT. LPCT + TPCT + RPCT + LDPCT + RDPCT) THEN
    TCODE = TC_RDIAG
  ELSE
    TCODE = TC_THRU
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE SET_GOAL_LANE(IV)
! ----------------------------------------------------------------------
! --- Determine which lane the vehicle should turn from.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE GLOBAL_DATA
  USE SIMPARAMS
  USE GLOBAL_DATA
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER :: I, J, IL, IRL, IRLANE
! ----------------------------------------------------------------------
#ifdef DebugVersion  
  integer :: temp
  temp = sid(iv)
#endif  
  GOAL_LANE(IV) = 0
  IF(STHRU_LINK(SLINK(IV)) .EQ. TURN_LINK(IV)) THEN
    IL = TURN_LINK(IV)
  ELSE
    IL = SLINK(IV)
  ENDIF
  IF(IL .NE. 0) THEN
    IF(TURN_CODE(IV) .EQ. TC_THRU) THEN
      IRL = STHRU_LINK(IL)
      IF(IRL .NE. 0) THEN
        IF(NODE_TYPE(SDSN(IRL)) .NE. NT_EXTERN) THEN
          DO I = FIRST_FULL_LANE(IL), LAST_FULL_LANE(IL)
            IF(SLANECODES(IV, I) .EQ. LC_GOOD) THEN
              IRLANE = I + STHRU_ALIGNMENT_LANE(IL) - SALIGNMENT_LANE(IL)
              IF(IRLANE .LT. FIRST_FULL_LANE(IRL) .OR. IRLANE .GT. LAST_FULL_LANE(IRL)) THEN
                SLANECODES(IV, I) = LC_VACATE
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDIF
    ENDIF
 
! --- If the driver is aware of the next downstream turn movement
! --- determine the goal lane based on that.
 
    IF(TURN_CODE2(IV) .EQ. TC_LEFT) THEN
      DO I = LAST_FULL_LANE(IL), FIRST_FULL_LANE(IL), -1
        IF(SLANECODES(IV, I) .EQ. LC_GOOD) THEN
          GOAL_LANE(IV) = I
          EXIT
        ENDIF
      ENDDO
    ELSEIF(TURN_CODE2(IV) .EQ. TC_RIGHT) THEN
      DO I = FIRST_FULL_LANE(IL), LAST_FULL_LANE(IL)
        IF(SLANECODES(IV, I) .EQ. LC_GOOD) THEN
          GOAL_LANE(IV) = I
          EXIT
        ENDIF
      ENDDO
    ENDIF
    
    !Set goal lanes for vehicles entering a roundabout.
    IF(ROUNDABOUT_ID(IL) .NE. 0) THEN
      IF(FIRST_RIGHT(IV)) THEN
        DO I = 1, TOTAL_LANES(IL)
          IF(SLANECODES(IV, I) .EQ. LC_GOOD) THEN
            GOAL_LANE(IV) = I
            EXIT
          ENDIF
        ENDDO
      ELSE
        DO I = 1, TOTAL_LANES(IL)
          IF(SLANECODES(IV, I) .EQ. LC_GOOD) THEN
            DO J = I + 1, TOTAL_LANES(IL)
              IF(SLANECODES(IV, J) .EQ. LC_GOOD) THEN
                GOAL_LANE(IV) = J
                EXIT
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDIF
    ENDIF    
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE CHECK_SIGNAL(IV, IRL, WPERM, WPROT, REDLIGHT)
! ----------------------------------------------------------------------
! --- Determine if the signal will allow the vehicle to discharge.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE TIMED_CONTROLLERS
  USE ACTUATED_CONTROLLERS
  USE GLOBAL_DATA
  USE SEEDS
  USE SIMPARAMS
  USE VDATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, IRL
  LOGICAL, INTENT(OUT) :: WPERM, WPROT, REDLIGHT
  INTEGER :: IL, ITURN, ISIG, ILAG, IACT, ILANE, IVX
  INTEGER :: ICODE, NL, NT, NR, ND, PHASE
  LOGICAL :: AMBER
  REAL :: RNDNUM, DIST
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif
 
! --- Set the default values for no control.
 
  WPERM = .TRUE.
  WPROT = .FALSE.
  REDLIGHT = .FALSE.
  ICODE = S_NONE
 
! --- Check the current signal state.
 
  IL = SLINK(IV)
  ITURN = STURNCODE(IV)
  ISIG = FTC_SIGNAL_ID(IL)
  IACT = AC_SIGNAL_ID(IL)
  DIST = SLENGTH(IL) - SLOCATION(IV)
  IF(IN_TURNING_WAY(IV)) THEN
    ICODE = RTW_CONTROL_CODE(IL)
    IF(ICODE .EQ. S_YIELD) THEN
      WPERM = .TRUE.
    ELSEIF(ICODE .EQ. S_STOP) THEN
      IF(SDRIVERTYPE(IV) .LE. 5 .AND. SSPEED(IV) .GT. 0.0) THEN
        WPERM = .FALSE.
      ELSEIF(SSPEED(IV) .GT. STOP_SPD) THEN
        WPERM = .FALSE.
      ELSEIF(SLOCATION(IV) .LT. SLENGTH(IL)) THEN
        WPERM = .FALSE.
      ELSE
        WPERM = .TRUE.
      ENDIF
    ENDIF
  ELSEIF(ISIG .NE. 0) THEN
    IF(FTC_SIGNALS(ISIG)%ACTIVE_INTERVALS .EQ. 1) THEN
      ICODE = SIGNAL_CODE(IL)
      IF(ICODE .EQ. S_GREEN) THEN
        WPERM = .TRUE.
      ELSEIF(ICODE .EQ. S_YIELD) THEN
        IF(ROUNDABOUT_ID(IL) .EQ. 0) WILL_YIELD(IV) = .TRUE.
        WPERM = .TRUE.
        IF(ROUNDABOUT_APPROACH_NUM(IL) .NE. 0) THEN
          DO ILANE = 1, SNUMLANES(IRL)
            IVX = SLAST_VEHICLE(IRL, ILANE)
            IF(IVX .NE. 0) THEN
              IF(SPREVLINK(IVX) .NE. IL) THEN
                IF(SLOCATION(IVX) - SVLENGTH(IVX) .LT. UP_INT_WIDTH(IRL)) THEN
                  WPERM = .FALSE.
                  EXIT
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ELSEIF(ICODE .EQ. S_STOP) THEN
        IF(SDRIVERTYPE(IV) .LE. 5 .AND. SSPEED(IV) .GT. 0.0) THEN
          WPERM = .FALSE.
        ELSEIF(SSPEED(IV) .GT. STOP_SPD) THEN
          WPERM = .FALSE.
        ELSEIF(SLOCATION(IV) .LT. SLENGTH(IL)) THEN
          WPERM = .FALSE.
        ELSE
          WPERM = .TRUE.
        ENDIF
      ENDIF
    ELSE
      WPERM = .TRUE.
      ICODE = SIGNAL_CODE(IL)
      CALL DECODE_FTC(ICODE, NL, NT, NR, ND, AMBER)
      IF(ITURN .EQ. TC_LEFT) THEN
 
! --- Determine if current signal state allows left turn.
       
        IF(AMBER .AND. FTC_SIGNALS(ISIG)%TIME_IN_INTERVAL .LE. 1.0) THEN
          IF(DIST .LT. 0. .OR. SSPEED(IV) * TIMESTEP .GT. DIST) THEN
            SGO_THRU_SIGNAL(IV) = .TRUE.
          ELSEIF(SSPEED(IV) .GT. 0.) THEN
            CALL AMBER_DECISION(IV, DIST, WPERM)
          ENDIF
        ELSEIF(AMBER) THEN
          WPERM = .FALSE.
        ELSE
          IF(NL .EQ. S_RED) THEN
            IF(FTC_SIGNALS(ISIG)%TIME_IN_INTERVAL .LE. 1.0) REDLIGHT = .TRUE.
            WPERM = .FALSE.
            
! --- Determine if the previous interval allowed left turns and see if the vehicle is
! --- a left turn lagger.

            ICODE = PREV_SIGNAL_CODE(IL)
            CALL DECODE_FTC(ICODE, NL, NT, NR, ND, AMBER)
            IF(NL .NE. S_RED) THEN
              IF(LAGGER_TIMER(IL) .GT. -5.0 .AND. QSTATE(IV) .NE. QS_NOTINQ) THEN
                CALL STREET_RANDOM(SSEED, RNDNUM)
                IF(LAGGER_TIMER(IL) .GT. -2.0) THEN
                  ILAG = 1
                ELSEIF(LAGGER_TIMER(IL) .GT. -4.0) THEN
                  ILAG = 2
                ELSE
                  ILAG = 3
                ENDIF
                IF(RNDNUM .LE. LT_LAGGER_PROB(ILAG)) THEN
                  WPERM = .TRUE.
                  SGO_THRU_SIGNAL(IV) = .TRUE.
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        IF(WPERM) THEN
          IF(OPPOSE_LINK(IL) .EQ. 0) THEN
            WPROT = .TRUE.
          ELSE
            ICODE = SIGNAL_CODE(OPPOSE_LINK(IL))
            CALL DECODE_FTC(ICODE, NL, NT, NR, ND, AMBER)
            IF(NT .EQ. S_RED) WPROT = .TRUE.
          ENDIF
        ENDIF
 
      ELSEIF(ITURN .EQ. TC_THRU) THEN
 
! --- Determine if current signal state allows thru movement.
       
        IF(AMBER .AND. FTC_SIGNALS(ISIG)%TIME_IN_INTERVAL .LE. 1.0) THEN
          IF(DIST .LT. 0. .OR. SSPEED(IV) * TIMESTEP .GT. DIST) THEN
            SGO_THRU_SIGNAL(IV) = .TRUE.
          ELSEIF(SSPEED(IV) .GT. 0.) THEN
            CALL AMBER_DECISION(IV, DIST, WPERM)
          ENDIF
        ELSEIF(AMBER) THEN
          WPERM = .FALSE.
        ELSE
          IF(NT .EQ. S_RED) THEN
            IF(FTC_SIGNALS(ISIG)%TIME_IN_INTERVAL .LE. 1.0) REDLIGHT = .TRUE.
            WPERM = .FALSE.
          ENDIF
        ENDIF
 
      ELSEIF(ITURN .EQ. TC_RIGHT) THEN
 
! --- Determine if current signal state allows right turn.
      
        IF(AMBER .AND. FTC_SIGNALS(ISIG)%TIME_IN_INTERVAL .LE. 1.0) THEN
          IF(DIST .LT. 0. .OR. SSPEED(IV) * TIMESTEP .GT. DIST) THEN
            SGO_THRU_SIGNAL(IV) = .TRUE.
          ELSEIF(SSPEED(IV) .GT. 0.) THEN
            CALL AMBER_DECISION(IV, DIST, WPERM)
          ENDIF
        ELSEIF(AMBER) THEN
          WPERM = .FALSE.
        ELSE
          IF(NR .EQ. S_RED) THEN
            IF(FTC_SIGNALS(ISIG)%TIME_IN_INTERVAL .LE. 1.0) REDLIGHT = .TRUE.
            WPERM = .FALSE.
          ENDIF
        ENDIF
        IF(.NOT. WPERM .AND. RTOR(IL) .AND. SSPEED(IV) .EQ. 0 .AND. &
           SLOCATION(IV) .GE. SLENGTH(IL) - 3) THEN
          WPERM = .TRUE.
          RTOR_FLAG(IV) = .TRUE.
        ENDIF
 
      ELSEIF(ITURN .GT. TC_RIGHT) THEN
 
! --- Determine if current signal state allows diagonal movement.
       
        IF(AMBER .AND. FTC_SIGNALS(ISIG)%TIME_IN_INTERVAL .LE. 1.0) THEN
          IF(DIST .LT. 0. .OR. SSPEED(IV) * TIMESTEP .GT. DIST) THEN
            SGO_THRU_SIGNAL(IV) = .TRUE.
          ELSEIF(SSPEED(IV) .GT. 0.) THEN
            CALL AMBER_DECISION(IV, DIST, WPERM)
          ENDIF
        ELSEIF(AMBER) THEN
          WPERM = .FALSE.
        ELSE
          IF(ND .EQ. S_RED) THEN
            IF(FTC_SIGNALS(ISIG)%TIME_IN_INTERVAL .LE. 1.0) REDLIGHT = .TRUE.
            WPERM = .FALSE.
          ENDIF
        ENDIF
        IF(WPERM) THEN
          IF(OPPOSE_LINK(IL) .EQ. 0) THEN
            WPROT = .TRUE.
          ELSE
            ICODE = SIGNAL_CODE(OPPOSE_LINK(IL))
            CALL DECODE_FTC(ICODE, NL, NT, NR, ND, AMBER)
            IF(ND .EQ. S_RED) WPROT = .TRUE.
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ELSEIF(IACT .NE. 0) THEN
    IF(ITURN .EQ. TC_LEFT) THEN
 
! --- Determine if current signal state allows left turn.
       
      IF(AMBER_LEFT(IL) .AND. TIME_IN_AMBER_LEFT(IL) .LE. 1.0) THEN
        IF(DIST .LT. 0. .OR. SSPEED(IV) * TIMESTEP .GT. DIST) THEN
          SGO_THRU_SIGNAL(IV) = .TRUE.
        ELSEIF(SSPEED(IV) .GT. 0.) THEN
          CALL AMBER_DECISION(IV, DIST, WPERM)
        ENDIF
        !Count the number of vehicles in the dilemma zone
        IF(SSPEED(IV) .GT. 0.0 .AND. TIME_IN_AMBER_LEFT(IL) .LE. 1.0) THEN
          IF(.NOT. INDZ(IV)) THEN
            IF(DIST / SSPEED(IV) .LE. DZ_ENTRY_TIME .AND. DIST / SSPEED(IV) .GE. DZ_EXIT_TIME) THEN
              VEHICLES_INDZ(IL) = VEHICLES_INDZ(IL) + 1
              INDZ(IV) = .TRUE.
              PHASE = PHASE_LEFT(IL)
              AC_SIGNALS(IACT)%VEHICLES_INDZ(PHASE) = AC_SIGNALS(IACT)%VEHICLES_INDZ(PHASE) + 1
            ENDIF
          ENDIF
        ENDIF
      ELSEIF(AMBER_LEFT(IL)) THEN
        WPERM = .FALSE.
      ELSE
        IF(.NOT. SIGNAL_LEFT(IL)) THEN
          IF(TIME_IN_RED_LEFT(IL) .LE. 1.0) REDLIGHT = .TRUE.
          WPERM = .FALSE.

! --- Determine if the previous phase allowed left turns and see if the vehicle is
! --- a left turn lagger.

          IF(PREV_SIGNAL_LEFT(IL)) THEN
            IF(LAGGER_TIMER(IL) .GT. -5.0 .AND. QSTATE(IV) .NE. QS_NOTINQ) THEN
              CALL STREET_RANDOM(SSEED, RNDNUM)
              IF(LAGGER_TIMER(IL) .GT. -2.0) THEN
                ILAG = 1
              ELSEIF(LAGGER_TIMER(IL) .GT. -4.0) THEN
                ILAG = 2
              ELSE
                ILAG = 3
              ENDIF
              IF(RNDNUM .LE. LT_LAGGER_PROB(ILAG)) THEN
                WPERM = .TRUE.
                SGO_THRU_SIGNAL(IV) = .TRUE.
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      IF(WPERM) THEN
        IF(OPPOSE_LINK(IL) .EQ. 0) THEN
          WPROT = .TRUE.
        ELSE
          IF(.NOT. SIGNAL_THRU(OPPOSE_LINK(IL))) WPROT = .TRUE.
        ENDIF
      ENDIF
 
    ELSEIF(ITURN .EQ. TC_THRU) THEN
 
! --- Determine if current signal state allows thru movement.
       
      IF(AMBER_THRU(IL) .AND. TIME_IN_AMBER_THRU(IL) .LE. 1.0) THEN
        IF(DIST .LT. 0. .OR. SSPEED(IV) * TIMESTEP .GT. DIST) THEN
          SGO_THRU_SIGNAL(IV) = .TRUE.
        ELSEIF(SSPEED(IV) .GT. 0.) THEN
          CALL AMBER_DECISION(IV, DIST, WPERM)
        ENDIF
        !Count the number of vehicles in the dilemma zone
        IF(SSPEED(IV) .GT. 0.0 .AND. TIME_IN_AMBER_THRU(IL) .LE. 1.0) THEN
          IF(.NOT. INDZ(IV)) THEN
            IF(DIST / SSPEED(IV) .LE. DZ_ENTRY_TIME .AND. DIST / SSPEED(IV) .GE. DZ_EXIT_TIME) THEN
              VEHICLES_INDZ(IL) = VEHICLES_INDZ(IL) + 1
              INDZ(IV) = .TRUE.
              PHASE = PHASE_THRU(IL)
              AC_SIGNALS(IACT)%VEHICLES_INDZ(PHASE) = AC_SIGNALS(IACT)%VEHICLES_INDZ(PHASE) + 1
            ENDIF
          ENDIF
        ENDIF
      ELSEIF(AMBER_THRU(IL)) THEN
        WPERM = .FALSE.
      ELSE
        IF(.NOT. SIGNAL_THRU(IL)) THEN
          IF(TIME_IN_RED_THRU(IL) .LE. 1.0) REDLIGHT = .TRUE.
          WPERM = .FALSE.
        ENDIF
      ENDIF
 
    ELSEIF(ITURN .EQ. TC_RIGHT) THEN
 
! --- Determine if current signal state allows right turn.
      
      IF(AMBER_RIGHT(IL) .AND. TIME_IN_AMBER_RIGHT(IL) .LE. 1.0) THEN
        IF(DIST .LT. 0. .OR. SSPEED(IV) * TIMESTEP .GT. DIST) THEN
          SGO_THRU_SIGNAL(IV) = .TRUE.
        ELSEIF(SSPEED(IV) .GT. 0.) THEN
          CALL AMBER_DECISION(IV, DIST, WPERM)
        ENDIF
        !Count the number of vehicles in the dilemma zone
        IF(SSPEED(IV) .GT. 0.0 .AND. TIME_IN_AMBER_RIGHT(IL) .LE. 1.0) THEN
          IF(.NOT. INDZ(IV)) THEN
            IF(DIST / SSPEED(IV) .LE. DZ_ENTRY_TIME .AND. DIST / SSPEED(IV) .GE. DZ_EXIT_TIME) THEN
              VEHICLES_INDZ(IL) = VEHICLES_INDZ(IL) + 1
              INDZ(IV) = .TRUE.
              PHASE = PHASE_RIGHT(IL)
              AC_SIGNALS(IACT)%VEHICLES_INDZ(PHASE) = AC_SIGNALS(IACT)%VEHICLES_INDZ(PHASE) + 1
            ENDIF
          ENDIF
        ENDIF
      ELSEIF(AMBER_RIGHT(IL)) THEN
        WPERM = .FALSE.
      ELSE
        IF(.NOT. SIGNAL_RIGHT(IL)) THEN
          IF(TIME_IN_RED_RIGHT(IL) .LE. 1.0) REDLIGHT = .TRUE.
          WPERM = .FALSE.
        ENDIF
      ENDIF
      IF(.NOT. WPERM .AND. RTOR(IL) .AND. SSPEED(IV) .EQ. 0 .AND. &
          SLOCATION(IV) .GE. SLENGTH(IL) - 3) THEN
        WPERM = .TRUE.
        RTOR_FLAG(IV) = .TRUE.
      ENDIF
 
    ELSEIF(ITURN .GT. TC_RIGHT) THEN
 
! --- Determine if current signal state allows diagonal movement.
       
      IF(AMBER_DIAG(IL) .AND. TIME_IN_AMBER_DIAG(IL) .LE. 1.0) THEN
        IF(DIST .LT. 0. .OR. SSPEED(IV) * TIMESTEP .GT. DIST) THEN
          SGO_THRU_SIGNAL(IV) = .TRUE.
        ELSEIF(SSPEED(IV) .GT. 0.) THEN
          CALL AMBER_DECISION(IV, DIST, WPERM)
        ENDIF
        !Count the number of vehicles in the dilemma zone
        IF(SSPEED(IV) .GT. 0.0 .AND. TIME_IN_AMBER_DIAG(IL) .LE. 1.0) THEN
          IF(.NOT. INDZ(IV)) THEN
            IF(DIST / SSPEED(IV) .LE. DZ_ENTRY_TIME .AND. DIST / SSPEED(IV) .GE. DZ_EXIT_TIME) THEN
              VEHICLES_INDZ(IL) = VEHICLES_INDZ(IL) + 1
              INDZ(IV) = .TRUE.
              PHASE = PHASE_DIAG(IL)
              AC_SIGNALS(IACT)%VEHICLES_INDZ(PHASE) = AC_SIGNALS(IACT)%VEHICLES_INDZ(PHASE) + 1
            ENDIF
          ENDIF
        ENDIF
      ELSEIF(AMBER_DIAG(IL)) THEN
        WPERM = .FALSE.
      ELSE
        IF(.NOT. SIGNAL_DIAG(IL)) THEN
          IF(TIME_IN_RED_DIAG(IL) .LE. 1.0) REDLIGHT = .TRUE.
          WPERM = .FALSE.
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END      

! ==================================================================================================
  SUBROUTINE CHECK_NEXT_SIGNAL(IV, IRL, DECEL)
! ----------------------------------------------------------------------
! --- Determine if the signal on the thru link will allow the vehicle to discharge.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE TIMED_CONTROLLERS
  USE ACTUATED_CONTROLLERS
  USE VDATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, IRL
  REAL, INTENT(OUT) :: DECEL
  INTEGER :: ISIG, IACT
  INTEGER :: ICODE, NL, NT, NR, ND
  LOGICAL :: AMBER, WPERM
  REAL :: DIST, RMIN, STOPDIST
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif
 
  WPERM = .TRUE.
 
! --- Check the current signal state on the thru link for thru traffic.
 
  ISIG = FTC_SIGNAL_ID(IRL)
  IACT = AC_SIGNAL_ID(IRL)
  DIST = SLENGTH(IRL)
  WPERM = .TRUE.
  IF(ISIG .NE. 0) THEN
    IF(FTC_SIGNALS(ISIG)%ACTIVE_INTERVALS .EQ. 1) THEN
      IF(SIGNAL_CODE(IRL) .EQ. S_STOP) THEN
        WPERM = .FALSE.
      ENDIF
    ELSE
 
! --- Determine if current signal state allows thru movement.
       
      ICODE = SIGNAL_CODE(IRL)
      CALL DECODE_FTC(ICODE, NL, NT, NR, ND, AMBER)
      IF(NT .EQ. S_RED) THEN
        WPERM = .FALSE.
      ENDIF
    ENDIF
    
  ELSEIF(IACT .NE. 0) THEN

! --- Determine if current signal state allows thru movement.
       
    IF(.NOT. SIGNAL_THRU(IRL)) THEN
      WPERM = .FALSE.
    ENDIF
  ENDIF
  IF(WPERM) THEN
    DECEL = 100.
  ELSE
  
! --- Compute the required stopping deceleration if the vehicle must stop at the end of the link.

    IF(SFLEET(IV) .EQ. FLEET_BIKE) THEN
      RMIN = -8.0
    ELSE
      RMIN = MAX_DECEL(SVTYPE(IV), SSPEED(IV), SGRADE(IRL))
    ENDIF
    STOPDIST = -(SSPEED(IV) ** 2) / (2 * RMIN)
    IF(DIST .LT. STOPDIST) THEN
      DECEL = -SSPEED(IV) ** 2 / (2 * DIST)
    ENDIF
  ENDIF
  RETURN
  END      

! ==================================================================================================
  SUBROUTINE CHECK_PED_DELAY(IV, DELAY)
! ----------------------------------------------------------------------
! --- Determine if the vehicle must wait for pedestrians.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE TIMED_CONTROLLERS
  USE PEDS
  USE SIMPARAMS
  USE SEEDS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  REAL, INTENT(OUT) :: DELAY
  INTEGER :: IL, ISIG, IMAX, IRN
  REAL :: RNDNUM
  LOGICAL :: STRONG
! ----------------------------------------------------------------------
  DELAY = 0.
  IL = SLINK(IV)
  IF(PED_CODE(IL) .NE. 0) THEN 
 
! --- Determine if the signal is still within the strong interaction
! --- period.
 
    STRONG = .FALSE.
    IMAX = PED_DURATION(PED_CODE(IL))
    ISIG = FTC_SIGNAL_ID(IL)
    IF(ISIG .NE. 0) THEN
      IF(FTC_SIGNALS(ISIG)%TIME_IN_INTERVAL .LT. IMAX) THEN
        STRONG = .TRUE.
      ENDIF
    ENDIF
 
! --- Randomly choose the delay from the appropriate distribution.
 
    CALL STREET_RANDOM(SSEED, RNDNUM)  
    IRN = 1 + 10 * RNDNUM
    IF(STRONG) THEN
      DELAY = PDELAY_STRONG(IRN)
    ELSE
      DELAY = PDELAY_WEAK(IRN)
    ENDIF
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE CHECK_CROSS_STREETS(IV, IRL, IRLANE, WGO)
! ----------------------------------------------------------------------
! --- Look for vehicles on near and far cross links that might cause
! --- conflicts.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE DRIVERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, IRL, IRLANE
  LOGICAL, INTENT(OUT) :: WGO
  INTEGER :: I, IL, ILINK, IVX, ILANE, NLANES, RLANE
  INTEGER :: J, XWIDTH, YWIDTH, I1, I2, I3
  REAL :: RDIST, TTC, TTCMIN, XDIST, YDIST
  LOGICAL :: WPERM, WPROT, REDLIGHT
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

  TTC = 1000.
  IL = SLINK(IV)
  XWIDTH = 0
  NLANES = 0
  DO I = 1, 2
    IF(I .EQ. 1) THEN
      ILINK = NEAR_CROSSLINK(IL)
      !If the vehicle is in a roundabout and the cross street is an approach to the roundabout
      !ignore the traffic on the cross link
      IF(ILINK .NE. 0) THEN
        IF(ROUNDABOUT_ID(IL) .NE. 0 .AND. ROUNDABOUT_APPROACH_NUM(ILINK) .NE. 0) ILINK = 0
      ENDIF
      IF(ILINK .NE. 0) THEN
        I1 = FIRST_FULL_LANE(ILINK)
        IF(STURNCODE(IV) .EQ. TC_RIGHT) THEN
          IF(STHRU_LINK(ILINK) .NE. 0) THEN
            I2 = IRLANE - FIRST_FULL_LANE(ILINK) + FIRST_FULL_LANE(STHRU_LINK(ILINK)) &
                        + SALIGNMENT_LANE(ILINK) - STHRU_ALIGNMENT_LANE(ILINK)
          ELSE
            I2 = TOTAL_LANES(ILINK)
          ENDIF
        ELSE
          I2 = TOTAL_LANES(ILINK)
        ENDIF
        IF(I2 .LT. 1) THEN !No lane feeds the receiving lane.
          ILINK = 0 
        ENDIF
        I3 = 1
      ENDIF
    ELSE
      ILINK = FAR_CROSSLINK(IL)
      IF(ILINK .NE. 0) THEN
        IF(STURNCODE(IV) .EQ. TC_RIGHT .OR. ROUNDABOUT_APPROACH_NUM(IL) .NE. 0) THEN
          ILINK = 0
        ELSE
          I1 = TOTAL_LANES(ILINK)
          I2 = FIRST_FULL_LANE(ILINK)
          I3 = -1
          IF(STURNCODE(IV) .EQ. TC_THRU) NLANES = I2
        ENDIF
      ENDIF
    ENDIF
    IF(ILINK .NE. 0) THEN
      YWIDTH = 0
      DO ILANE = I1, I2, I3
        IVX = FIRST_VEHICLE(ILINK, ILANE)
        IF(IVX .NE. 0) THEN
          IF(SLINK(IVX) .EQ. 0) CYCLE
          !Special cases for roundabouts
          IF(STURNCODE(IVX) .EQ. TC_RIGHT .AND. ROUNDABOUT_ID(ILINK) .EQ. 0) CYCLE
          IF(STURNCODE(IVX) .EQ. TC_THRU .AND. ROUNDABOUT_ID(ILINK) .NE. 0 .AND. ILANE .GT. SLANE(IV)) THEN
            IF(TURN_LINK(IVX) .NE. STHRU_LINK(ILINK)) CYCLE
          ENDIF
          IF(STURNCODE(IVX) .EQ. TC_THRU .AND. ROUNDABOUT_ID(ILINK) .NE. 0) THEN
            IF(ILANE .EQ. SLANE(IV)) THEN
              CALL FIND_RECEIVING_LANE(IVX, IRL, ILANE, RLANE)
              IF(RLANE .GT. SLANE(IV)) THEN
                IF(TURN_LINK(IVX) .NE. STHRU_LINK(ILINK)) CYCLE
              ENDIF
            ENDIF
          ENDIF
 
! --- If the subject vehicle is an EV and the conflicting vehicle is
! --- cooperating, ignore the conflict.
 
          IF(SWILL_COOP_EV(IVX) .AND. SEV_WATCH(IVX) .EQ. IV) CYCLE
          
          IF(SIGNAL_CODE(ILINK) .EQ. S_STOP .AND. WAIT_TIME(IV) .GT. WAIT_TIME(IVX)) THEN
            WPERM = .FALSE.
          ELSE
            CALL CHECK_SIGNAL(IVX, IRL, WPERM, WPROT, REDLIGHT)
          ENDIF
 
! --- If the conflicting vehicle is permitted to enter the intersection
! --- compute the time to the collision point.
          
          IF(WPERM) THEN
            XDIST = SLENGTH(ILINK) - SLOCATION(IVX) + 0.5 * UP_INT_WIDTH(IRL)  !Assuming the collision takes place in the center of the intersection
            IF(SXCODE(IVX) .EQ. 0) XDIST = MAX(XDIST - SSPEED(IVX), 0.)
            YDIST = YWIDTH + XWIDTH
            RDIST = SQRT(XDIST**2 + YDIST**2)
            IF(RDIST .LE. SIGHT_DIST(IL)) THEN
              IF(SSPEED(IVX) .NE. 0) THEN
                TTC = MIN(TTC, RDIST / SSPEED(IVX))
              ELSE
                IF(.NOT. WILL_YIELD(IVX)) TTC = 0
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        !Consider the last vehicle out if it is still in the intersection
        IVX = SLAST_VEHICLE_OUT(ILINK, ILANE)
        IF(IVX .NE. 0) THEN
          IF(SLINK(IVX) .NE. 0) THEN
            IF(SUSN(SLINK(IVX)) .EQ. SDSN(IL)) THEN
              IF(SLOCATION(IVX) - SVLENGTH(IVX) .LT. UP_INT_WIDTH(SLINK(IVX))) THEN
                TTC = 0.
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        YWIDTH = YWIDTH + SLANE_WIDTH(ILINK, ILANE)
      ENDDO
      XWIDTH = 0
      DO J = FIRST_FULL_LANE(I), LAST_FULL_LANE(I)
        XWIDTH = XWIDTH + SLANE_WIDTH(ILINK, J)
      ENDDO
    ENDIF
  ENDDO
       
  WGO = .TRUE.
  IF(ROUNDABOUT_APPROACH_NUM(IL) .NE. 0) THEN
    TTCMIN = ACCEPTABLE_RABT_GAP(SDRIVERTYPE(IV))
  ELSEIF(STURNCODE(IV) .EQ. TC_RIGHT) THEN
    TTCMIN = ACCEPTABLE_RTG(SDRIVERTYPE(IV))
  ELSE
    TTCMIN = ACCEPTABLE_GAP(SDRIVERTYPE(IV))
    IF(SIGNAL_CODE(IL) .EQ. S_YIELD) TTCMIN = MAX(TTCMIN - 1.5, 0.)
    IF(NLANES .GT. 0) TTCMIN = TTCMIN + ADDITIONAL_GAP(NLANES)
  ENDIF
  IF(TTC .LT. TTCMIN) WGO = .FALSE.
  RETURN
  END      

! ==================================================================================================
  SUBROUTINE CHECK_UPSTREAM_LINK(IV, IRL, IRLANE, WGO)
! ----------------------------------------------------------------------
! --- Look for vehicles on the upstream link from the near cross links that might cause
! --- conflicts for a vehicle entering a roundabout.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE DRIVERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, IRL, IRLANE
  LOGICAL, INTENT(OUT) :: WGO
  INTEGER :: IL, ILC, ILINK, IVX, N, ILANE
  INTEGER :: XWIDTH, YWIDTH, I1, I2, I3
  REAL :: RDIST, TTC, XDIST, YDIST
  LOGICAL :: WPERM, WPROT, REDLIGHT
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

  TTC = 1000.
  IL = SLINK(IV)
  ILC = NEAR_CROSSLINK(IL)
  N = 1
  IF(ILC .NE. 0) ILINK = NEAR_UPPER_CROSSLINK(ILC, N)
  DO WHILE(ILINK .NE. 0)
    ILANE = FIRST_FULL_LANE(ILINK)
    IVX = FIRST_VEHICLE(ILINK, ILANE)
    IF(SIGNAL_CODE(ILINK) .NE. S_YIELD) THEN
      IF(IVX .NE. 0) THEN
        IF(STURNCODE(IVX) .NE. TC_RIGHT) THEN
 
    ! --- If the subject vehicle is an EV and the conflicting vehicle is
    ! --- cooperating, ignore the conflict.
 
          IF(SWILL_COOP_EV(IVX) .AND. SEV_WATCH(IVX) .EQ. IV) CYCLE
          CALL CHECK_SIGNAL(IVX, IRL, WPERM, WPROT, REDLIGHT)
 
    ! --- If the conflicting vehicle is permitted to enter the intersection
    ! --- compute the time to the collision point.
 
          IF(WPERM) THEN
            XDIST = SLENGTH(ILC) + SLENGTH(ILINK) - SLOCATION(IVX) + 0.5 * UP_INT_WIDTH(IRL)  !Assuming the collision takes place in the center of the intersection
            IF(SXCODE(IVX) .EQ. 0) XDIST = MAX(XDIST - SSPEED(IVX), 0.)
            YDIST = YWIDTH + XWIDTH
            RDIST = SQRT(XDIST**2 + YDIST**2)
            IF(RDIST .LE. SIGHT_DIST(IL)) THEN
              IF(SSPEED(IVX) .NE. 0) THEN
                TTC = MIN(TTC, RDIST / SSPEED(IVX))
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    N = N + 1
    ILINK = NEAR_UPPER_CROSSLINK(ILC, N)
  ENDDO
       
  WGO = .TRUE.
  IF(TTC .LT. ACCEPTABLE_RABT_GAP(SDRIVERTYPE(IV))) WGO = .FALSE.
  
  RETURN
  END      
      
! ==================================================================================================
  SUBROUTINE CHECK_RTOR(IV, IRLANE, WGO)
! ----------------------------------------------------------------------
! --- Look for vehicles on near cross link and right receiving link 
! --- that might cause conflicts.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE DRIVERS
  USE GLOBAL_DATA
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, IRLANE
  LOGICAL, INTENT(OUT) :: WGO
  INTEGER :: IL, ILINK, IVX, ILANE, XDIST
  INTEGER :: I1, I2, IRL
  REAL :: TTC, TTCMIN
  LOGICAL :: WPERM, WPROT, REDLIGHT
  !INTEGER :: LOOP_COUNTER
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

  WGO = .TRUE.

  !Look at the last vehicle in the first lane on the receiving link.
  IL = SLINK(IV)
  ILINK = RIGHT_LINK(IL)
  ILANE = FIRST_FULL_LANE(ILINK)
  I1 = FIRST_FULL_LANE(ILINK)
  I2 = IRLANE + STHRU_ALIGNMENT_LANE(ILINK) - SALIGNMENT_LANE(ILINK) 
  IF(I2 .LT. 1) THEN !No lane feeds the receiving lane.
    ILINK = 0 
  ENDIF
  DO ILANE = I1, I2
    IF(FIRST_VEHICLE(ILINK, ILANE) .NE. 0) THEN
      IVX = SLAST_VEHICLE(ILINK, ILANE)
      IF(IVX .NE. 0) THEN
        IF(SLOCATION(IVX) - SVLENGTH(IVX) .LT. UP_INT_WIDTH(ILINK)) THEN
          WGO = .FALSE.
          EXIT
        ENDIF
      ENDIF
    ENDIF 
  ENDDO
  
  IF(WGO) THEN
    !Look for vehicles on the near crosslink.
    IRL = 0
    TTC = 1000.
    ILINK = NEAR_CROSSLINK(IL)
    IF(ILINK .NE. 0) THEN
      I1 = FIRST_FULL_LANE(ILINK)
      I2 = IRLANE + STHRU_ALIGNMENT_LANE(ILINK) - SALIGNMENT_LANE(ILINK) 
      IF(I2 .LT. 1) THEN !No lane feeds the receiving lane.
        ILINK = 0 
      ENDIF
      IF(ILINK .NE. 0) THEN
        DO ILANE = I1, I2
          IF(FIRST_VEHICLE(ILINK, ILANE) .NE. 0) THEN
            IVX = FIRST_VEHICLE(ILINK, ILANE)
            IF(IVX .NE. 0) THEN
              IF(STURNCODE(IVX) .EQ. TC_RIGHT) CYCLE
            
              IF(SIGNAL_CODE(ILINK) .EQ. S_STOP .AND. WAIT_TIME(IV) .GT. WAIT_TIME(IVX)) THEN
                WPERM = .FALSE.
              ELSE
                CALL CHECK_SIGNAL(IVX, IRL, WPERM, WPROT, REDLIGHT)
              ENDIF
 
  ! --- If the conflicting vehicle is permitted to enter the intersection
  ! --- compute the time to collision point.
 
              IF(WPERM) THEN
                XDIST = SLENGTH(ILINK) - SLOCATION(IVX) + UP_INT_WIDTH(RIGHT_LINK(IL)) 
                IF(SXCODE(IVX) .EQ. 0) XDIST = MAX(XDIST - SSPEED(IVX), 0.)
                IF(SSPEED(IVX) .NE. 0) THEN
                  TTC = MIN(TTC, XDIST / SSPEED(IVX))
                ELSE
                  IF(.NOT. WILL_YIELD(IVX)) TTC = 0
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
    ENDIF
   
    TTCMIN = ACCEPTABLE_RTG(SDRIVERTYPE(IV))
    IF(TTC .LT. TTCMIN) WGO = .FALSE.
  ENDIF
  RETURN
  END   
  
! ==================================================================================================
  SUBROUTINE CHECK_INTERSECTION(IV, WGO)
! ----------------------------------------------------------------------
! --- Examine left and right receiving links to see if there are
! --- vehicles in the intersection. Also check the receiving link of the
! --- link opposing left turners if the vehicle is turning left.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE DRIVERS
  USE GLOBAL_DATA
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  LOGICAL, INTENT(OUT) :: WGO
  INTEGER :: I, IL, IRL, IVX
  REAL :: ZBACK, LANWID, DRIGHT
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

  WGO = .TRUE.
  IL = SLINK(IV)
  IF(STURNCODE(IV) .EQ. TC_RIGHT .OR. ROUNDABOUT_APPROACH_NUM(IL) .NE. 0) RETURN

  !DRIGHT is the istance to the right curb
  LANWID = SLANE_WIDTH(IL, 1) !Assume all lanes have same width
  DRIGHT = (SLANE(IV) - 1) * LANWID
 
! --- Check for spillback on the left receiving link.
 
  IF(LEFT_LINK(IL) .NE. 0) THEN
    IRL = LEFT_LINK(IL)
    IF(NODE_TYPE(SDSN(IRL)) .NE. NT_EXTERN) THEN
      DO I = FIRST_FULL_LANE(IRL), LAST_FULL_LANE(IRL)
        IVX = SLAST_VEHICLE(IRL, I)
        IF(IVX .NE. 0) THEN
          ZBACK = SLOCATION(IVX) - SVLENGTH(IVX)
          IF(ZBACK .LT. 0.5 * UP_INT_WIDTH(IRL)) THEN
            IF(SPREVLINK(IVX) .NE. IL) THEN
              WGO = .FALSE.
              EXIT
            ENDIF
          ENDIF
        ENDIF
      ENDDO
    ENDIF
  ENDIF
 
! --- Check for spillback on the right receiving link.
 
  IF(WGO .AND. RIGHT_LINK(IL) .NE. 0) THEN
    IRL = RIGHT_LINK(IL)
    IF(NODE_TYPE(SDSN(IRL)) .NE. NT_EXTERN) THEN
      DO I = FIRST_FULL_LANE(IRL), LAST_FULL_LANE(IRL)
        IVX = SLAST_VEHICLE(IRL, I)
        IF(IVX .NE. 0) THEN
 
  ! --- If the subject vehicle is turning left and the other vehicle 
  ! --- made an opposing left turn it is not a spillback situation.
 
          IF(STURNCODE(IV) .EQ. TC_LEFT .AND. SPREVLINK(IVX) .NE. 0) THEN
            IF(IRL .EQ. LEFT_LINK(SPREVLINK(IVX))) EXIT
          ENDIF
          ZBACK = SLOCATION(IVX) - SVLENGTH(IVX)
          IF(ZBACK .LT. UP_INT_WIDTH(IRL) - DRIGHT) THEN
            IF(SPREVLINK(IVX) .NE. IL) THEN
              WGO = .FALSE.
              EXIT
            ENDIF
          ENDIF
        ENDIF
      ENDDO
    ENDIF
  ENDIF
 
! --- If the vehicle is turning left, check for spillback on the
! --- link that receives traffic from the link that opposes the
! --- left turn vehicles.
 
  IF(WGO .AND. STURNCODE(IV) .EQ. TC_LEFT) THEN
    IF(OPPOSE_LINK(IL) .NE. 0) THEN
      IRL = STHRU_LINK(OPPOSE_LINK(IL))
      IF(IRL .NE. 0) THEN
        DO I = FIRST_FULL_LANE(IRL), LAST_FULL_LANE(IRL)
          IVX = SLAST_VEHICLE(IRL, I)
          IF(IVX .NE. 0) THEN
            IF(PREV_TURNCODE(IVX) .NE. TC_RIGHT) THEN
              ZBACK = SLOCATION(IVX) - SVLENGTH(IVX)
              IF(ZBACK .LT. 0.5 * UP_INT_WIDTH(IRL)) THEN
                WGO = .FALSE.
                EXIT
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE CHECK_OPPOSING_STREAM(IV, WGO)
! ----------------------------------------------------------------------
! --- Consider opposing link traffic for left turners.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE DRIVERS
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  LOGICAL, INTENT(OUT) :: WGO
  INTEGER :: IL, ILINK, IVX, ILANE, IRL
  REAL :: RDIST, TTC, TTCMIN, XDIST, YDIST
  LOGICAL :: WPERM, WPROT, REDLIGHT, WCHECK, IGNORE
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

  TTC = 1000.
  IL = SLINK(IV)
  ILINK = OPPOSE_LINK(IL)
  YDIST = 0
  DO ILANE = 1, N_STREET_LANES
    WCHECK = .FALSE.
    IF(ILANE .LE. LAST_FULL_LANE(ILINK)) THEN
      WCHECK = .TRUE.
    ENDIF
    IF(WCHECK) THEN
      IVX = FIRST_VEHICLE(ILINK, ILANE)
      IF(IVX .NE. 0) THEN
        IF(SLINK(IVX) .NE. 0 .AND. .NOT. WILL_YIELD(IVX)) THEN
 
! --- If the subject vehicle is an EV and the conflicting vehicle is
! --- cooperating, ignore the conflict.
 
          IF(SWILL_COOP_EV(IVX) .AND. SEV_WATCH(IVX) .EQ. IV) CYCLE
          
          IF(SIGNAL_CODE(ILINK) .EQ. S_STOP .AND. WAIT_TIME(IV) .GT. WAIT_TIME(IVX)) THEN
            WPERM = .FALSE.
          ELSE
            CALL CHECK_SIGNAL(IVX, IRL, WPERM, WPROT, REDLIGHT)
          ENDIF
 
! --- If the conflicting vehicle is permitted to enter the intersection
! --- compute the time to the collision point.
 
          IF(WPERM .AND. STURNCODE(IVX) .NE. TC_LEFT) THEN
            IGNORE = .FALSE.
            !If the conflicting vehicle is making a right turn and there is more than one lane
            !on the receiving link, and the conflicting vehicle is in a right turn pocket
            !or a lane that is channelized for right turns only, ignore the conflict.
            IF(STURNCODE(IVX) .EQ. TC_RIGHT) THEN
              IRL = RIGHT_LINK(SLINK(IVX))
              IF(IRL .NE. 0) THEN
                IF(SNUMLANES(SLINK(IVX)) .GT. 1) THEN
                  IF(SLANE(IVX) .LE. NUMBER_RIGHTPOCKETS(SLINK(IVX))) THEN
                    IGNORE = .FALSE.
                  ELSEIF(CHANNELIZATION(SLINK(IVX), SLANE(IVX)) .EQ. 4) THEN
                    IGNORE = .FALSE.
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
            IF(.NOT. IGNORE) THEN
              IF(STHRU_LINK(ILINK) .NE. 0) THEN
                XDIST = SLENGTH(ILINK) - SLOCATION(IVX) + UP_INT_WIDTH(STHRU_LINK(ILINK)) / 2       
                RDIST = SQRT(XDIST**2 + YDIST**2)
                TTC = MIN(TTC, RDIST / MAX(SSPEED(IVX), 10.0))
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      YDIST = YDIST + SLANE_WIDTH(ILINK, ILANE)
      
! --- Make sure the last vehicle that departed from the opposing link is not blocking the left turner.
      
      IVX = SLAST_VEHICLE_OUT(ILINK, ILANE)
      IF(IVX .NE. 0) THEN
        IF(SLINK(IVX) .NE. 0 .AND. SLINK(IVX) .EQ. STHRU_LINK(ILINK)) THEN
          IF(SLOCATION(IVX) - SVLENGTH(IVX) .LT. UP_INT_WIDTH(STHRU_LINK(ILINK)) / 2) THEN
            TTC = 0.
            EXIT
          ENDIF
        ENDIF
      ENDIF
      
    ENDIF
  ENDDO
  TTCMIN = ACCEPTABLE_LTG(SDRIVERTYPE(IV))
  WGO = .TRUE.
  IF(TTC .LT. TTCMIN) WGO = .FALSE.
  RETURN
  END

! ==================================================================================================
  SUBROUTINE STOP_AT_INTERSECTION(IV, ACCEL, REDLIGHT, WSTOP)
! ----------------------------------------------------------------------
! --- Compute the deceleration required to stop at the end of the link.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE SIMPARAMS
  USE VEHICLE_TYPES
  USE VDATA
  USE GLOBAL_DATA
  USE ACTUATED_CONTROLLERS
  USE STREET_NODES
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  REAL, INTENT(INOUT) :: ACCEL
  LOGICAL, INTENT(IN) :: REDLIGHT
  LOGICAL, INTENT(OUT) :: WSTOP
  INTEGER :: IL, IACT, PHASE
  REAL :: DIST, RMIN, DECEL, STOPDIST, SAVE_ACCEL
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

  WSTOP = .FALSE.
  SAVE_ACCEL = ACCEL
  IL = SLINK(IV)
  IF(SFLEET(IV) .EQ. FLEET_BIKE) THEN
    RMIN = -8.0
  ELSE
    RMIN = MAX_DECEL(SVTYPE(IV), SSPEED(IV), SGRADE(IL))
  ENDIF
  
! --- Compute the required stopping deceleration if the vehicle must stop at the end of the link.

  IF(IN_TURNING_WAY(IV)) THEN
    DIST = ARC_LENGTH(IV) - ARC_LOCATION(IV)
  ELSE
    DIST = SLENGTH(IL) - SLOCATION(IV)
  ENDIF
  STOPDIST = -(SSPEED(IV) ** 2) / (2 * RMIN)
  IF(DIST .LE. 0 .OR. STOPDIST .GT. DIST) THEN
    IF(DIST .LE. SSPEED(IV) * TIMESTEP) THEN
      WSTOP = .TRUE.
      ACCEL = -SSPEED(IV) / TIMESTEP
    ELSE
      ACCEL = RMIN
    ENDIF
  ELSEIF(DIST .LE. SSPEED(IV) * TIMESTEP .AND. SSPEED(IV) .LE. STOP_SPD) THEN
    WSTOP = .TRUE.
    DECEL = -SSPEED(IV) ** 2 / (2 * DIST)
    !Account for lag time if the vehicle is close to the end of the link
    IF(SSPEED(IV) .GT. DIST .AND. SACCELERATION(IV) .GT. 0) THEN
      DECEL = 1.5 * DECEL
    ENDIF
    ACCEL = MIN(ACCEL, DECEL)
    ACCEL = MAX(ACCEL, RMIN)
  ELSEIF(DIST .LE. 5.0 .AND. SSPEED(IV) .LE. STOP_SPD) THEN
    WSTOP = .TRUE.
    DECEL = -SSPEED(IV) ** 2 / (2 * DIST)
    !Account for lag time if the vehicle is close to the end of the link
    IF(SSPEED(IV) .GT. DIST .AND. SACCELERATION(IV) .GT. 0) THEN
      DECEL = 1.5 * DECEL
    ENDIF
    ACCEL = MIN(ACCEL, DECEL)
    ACCEL = MAX(ACCEL, RMIN)
  ELSEIF(DIST .LT. (SSPEED(IV) ** 2) / 7 .AND. SSPEED(IV) .GT. STOP_SPD) THEN
    IF(DIST .LT. SSPEED(IV) * TIMESTEP) WSTOP = .TRUE.
    DECEL = -SSPEED(IV) ** 2 / (2 * DIST)
    !Account for lag time
    IF(WSTOP .AND. SSPEED(IV) .GT. DIST .AND. SACCELERATION(IV) .GT. 0) THEN
      DECEL = 1.5 * DECEL
    ENDIF
    ACCEL = MIN(ACCEL, DECEL)
    ACCEL = MAX(ACCEL, RMIN)
  ELSE
    
! --- Let the vehicle accelerate to move up to the stopbar.

    ACCEL = MIN(ACCEL, (DIST - SSPEED(IV)) / TIMESTEP)
  ENDIF
  IF(REDLIGHT .AND. ACCEL .LT. 0.75 * RMIN .AND. .NOT. HAS_STOPPED(IV)) THEN
    IF(DIST .LT. STOPDIST) THEN
      !Vehicle faces a red light and must make an extreme stop
      IF(.NOT. INITMODE) THEN
        IF(.NOT. REDRUNNER(IV)) THEN
          IACT = NACT(SDSN(IL))
          IF(IACT .NE. 0) THEN
            RED_LIGHT_RUNNERS(IL) = RED_LIGHT_RUNNERS(IL) + 1
            IF(STURNCODE(IV) .EQ. TC_LEFT) THEN
              PHASE = PHASE_LEFT(IL)
            ELSEIF(STURNCODE(IV) .EQ. TC_THRU) THEN
              PHASE = PHASE_THRU(IL)
            ELSEIF(STURNCODE(IV) .EQ. TC_RIGHT) THEN
              PHASE = PHASE_RIGHT(IL)
            ELSE
              PHASE = PHASE_DIAG(IL)
            ENDIF
            IF(PHASE .NE. 0) THEN
              AC_SIGNALS(IACT)%RED_LIGHT_RUNNERS(PHASE) = AC_SIGNALS(IACT)%RED_LIGHT_RUNNERS(PHASE) + 1
            ENDIF
            REDRUNNER(IV) = .TRUE.
            SGO_THRU_SIGNAL(IV) = .TRUE.
            ACCEL = SAVE_ACCEL
            WSTOP = .FALSE.
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END          

! ==================================================================================================
  SUBROUTINE CHECK_DISCHARGE(IV, ACCEL, WSTOP, DISCHARGE_TRAVEL, DISCHARGE_SPEED)
! ----------------------------------------------------------------------
! --- Determine if the vehicle can discharge from the link.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE FREEWAY_VEHICLES
  USE STREET_LINKS
  USE SIMPARAMS
  USE GLOBAL_DATA
  USE VEHICLE_TYPES
  USE CAR_FOLLOWING
  USE VDATA
  USE EVENTS
  USE NODE_TABLE
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  REAL, INTENT(INOUT) :: ACCEL, DISCHARGE_TRAVEL, DISCHARGE_SPEED
  LOGICAL, INTENT(OUT) :: WSTOP
  LOGICAL :: WPERM, WEV, WPROT, REDLIGHT, MUST_STOP, VINRABT, CAN_EXIT_RTW
  INTEGER :: ITURN, IL, IRLANE, IRL, ILD, ILANE, ILN, IVN, SFL, IFL
  INTEGER :: IVX, VID, IEVENT, DIST, NLINK, NRLANE
  REAL :: Z1, RTIME, DECEL, CF_DATA(7), NEW_ACCEL, EFFECTIVE_TIME
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

  WPERM = .FALSE.
  WSTOP = .FALSE.
  WPROT = .FALSE.
  REDLIGHT = .FALSE.
  ILANE = SLANE(IV)
  IL = SLINK(IV)
  DISCHARGE_TRAVEL = 0.
  DISCHARGE_SPEED = 0.
  MUST_STOP = .FALSE.
  VINRABT = ROUNDABOUT_ID(IL) .GT. 0 .AND. ROUNDABOUT_APPROACH_NUM(IL) .EQ. 0
  
! --- If the vehicle is in a station or on the shoulder do not discharge.
 
  IF(SFLEET(IV) .EQ. FLEET_BUS .OR. SFLEET(IV) .EQ. FLEET_EV) THEN
    IF(ILANE .EQ. 0) RETURN
  ENDIF

! --- Determine receiving link and lane.

  ITURN = STURNCODE(IV)
  IF(ITURN .EQ. TC_LEFT) THEN
    IRL = LEFT_LINK(IL)
  ELSEIF(ITURN .EQ. TC_THRU) THEN
    IRL = STHRU_LINK(IL)
  ELSEIF(ITURN .EQ. TC_RIGHT) THEN
    IRL = RIGHT_LINK(IL)
  ELSEIF(ITURN .EQ. TC_LDIAG) THEN
    IRL = LEFT_DIAG_LINK(IL)
  ELSEIF(ITURN .EQ. TC_RDIAG) THEN
    IRL = RIGHT_DIAG_LINK(IL)
  ENDIF
 
! --- If the vehicle is cooperating with an EV do not discharge,
! --- unless the EV is stopped behind the vehicle.
 
  WEV = SWILL_COOP_EV(IV)
  IF(WEV .AND. IRL .NE. 0) THEN
    IF(NODE_TYPE(SDSN(IRL)) .EQ. NT_EXTERN) THEN
      WEV = .FALSE.
    ENDIF
  ENDIF
  IF(WEV .AND. SEV_WATCH(IV) .NE. 0) THEN
    SFL = SFOLLOWER(IV)
    DO WHILE(SFL .NE. 0)
      IF(SFL .EQ. SEV_WATCH(IV)) THEN
        IF(SSPEED(SFL) .LE. STOP_SPD) THEN
          WEV = .FALSE. 
        ENDIF
        EXIT                          
      ELSE
        IF(SSPEED(SFL) .GT. STOP_SPD) EXIT
        SFL = SFOLLOWER(SFL)
      ENDIF
    ENDDO
  ENDIF
  IF(WEV) THEN
    WSTOP = .TRUE.
  ELSE
    IRLANE = 0
    ILD = 0
    IF(NODE_TYPE(SDSN(IL)) .NE. NT_INTERN) THEN
      ILD = SLAST_VEHICLE_OUT(IL, ILANE)
 
! --- In some cases the last vehicle out has left the network
! --- and the ID may represent a new vehicle.
! --- If so ignore it.
 
      IF(ILD .NE. 0) THEN
        IF(SID(ILD) .NE. SLAST_VEHICLE_OUT_ID(IL, ILANE)) THEN
          SLAST_VEHICLE_OUT(IL, ILANE) = 0
          SLAST_VEHICLE_OUT_ID(IL, ILANE) = 0
        ENDIF
      ENDIF
      IRLANE = ILANE
    ELSEIF(NODE_TYPE(SDSN(IRL)) .EQ. NT_EXTERN) THEN
      IRLANE = ILANE
    ELSEIF(IRL .NE. 0) THEN
      CALL FIND_RECEIVING_LANE(IV, IRL, ILANE, IRLANE)
      !If there is no receiving lane see if there is a lane to the left or right
      if(irlane .eq. 0) then
        if(slength(il) - slocation(iv) .le. 50) then
          ilane = slane(iv) - 1
          if(ilane .ge. 1) call find_receiving_lane(iv, irl, ilane, irlane)
          ilane = slane(iv)
          if(irlane .eq. 0) then
            ilane = slane(iv) + 1
            if(ilane .le. total_laneS(il)) call find_receiving_lane(iv, irl, ilane, irlane)
            ilane = slane(iv)
          endif
        endif
      endif
    ELSE
      WRITE(MSGTEXT, '(A,2I5)') 'No receiving link was found for link ', SUSN(IL), SDSN(IL)
      CALL SENDTEXTMSG(M_ERROR)
      IF(ITURN .EQ. TC_LEFT) THEN
        WRITE(MSGTEXT, '(A)') '  Turn movement is Left'
      ELSEIF(ITURN .EQ. TC_THRU) THEN
        WRITE(MSGTEXT, '(A)') '  Turn movement is Thru'
      ELSEIF(ITURN .EQ. TC_RIGHT) THEN
        WRITE(MSGTEXT, '(A)') '  Turn movement is Right'
      ELSEIF(ITURN .EQ. TC_LDIAG) THEN
        WRITE(MSGTEXT, '(A)') '  Turn movement is Left Diagonal'
      ELSEIF(ITURN .EQ. TC_RDIAG) THEN
        WRITE(MSGTEXT, '(A)') '  Turn movement is Right Diagonal'
      ENDIF
      CALL SENDTEXTMSG(M_ERROR)
      WRITE(MSGTEXT, '(A,I8, A, I8)') '  Vehicle # ', SID(IV), '; Path # ', SPATHID(IV)
      CALL SENDTEXTMSG(M_ERROR)
      EXITFLG = 1
      RETURN
    ENDIF
    IF(IRLANE .NE. 0) THEN
 
! --- Check the signal state unless the vehicle must be forced to stop.
       
      IF(SLANECODES(IV, ILANE) .EQ. LC_GOOD .AND. .NOT. FORCE_STOP(IV)) THEN
 
! --- If the vehicle is an EV allow it to proceed even if the signal
! --- is red, after slowing to a stop.
 
        IF(SFLEET(IV) .EQ. FLEET_EV) THEN
          IF(SGO_THRU_SIGNAL(IV)) THEN
            WPERM = .TRUE.
          ELSE
            CALL CHECK_SIGNAL(IV, IRL, WPERM, WPROT, REDLIGHT)
            IF(SSPEED(IV) .LE. STOP_SPD) WPERM = .TRUE.    !EV can proceed
            IF(SGO_THRU_SIGNAL(IV)) WPERM = .TRUE.
          ENDIF
        ELSE
 
          !Check for a vehicle approaching an interface node
          IF(NODE_TYPE(SDSN(IL)) .NE. NT_INTERN) THEN
            WPERM = .TRUE.
            IF(ILD .NE. 0) THEN
              !Find the lead vehicle on the freeway interface link
              VID = SID(ILD)
              ILD = 0
              DO IVX = 1, HIGHEST_INDEX_F
                IF(FID(IVX) .EQ. VID) THEN
                  ILD = IVX
                  EXIT
                ENDIF
              ENDDO
              IF(ILD .NE. 0 .AND. SCROSS1(SDSN(IL), SLANE(IV)) .EQ. 0) THEN
                CF_DATA(1) = SZFOLL(SDRIVERTYPE(IV))
                CF_DATA(3) = SSPEED(IV)
                CF_DATA(2) = FSPEED(ILD)
                CF_DATA(4) = SLENGTH(SLINK(IV)) - SLOCATION(IV) + FLOCATION(ILD) - FVLENGTH(ILD)
                CF_DATA(5) = MAX_DECEL(SVTYPE(IV), SSPEED(IV), SGRADE(IL))
                CF_DATA(6) = SCFMULT(SLINK(IV))
                CF_DATA(7) = SPCFSEP
                CALL INTERFACE_CAR_FOLLOW(CF_DATA, SDECEL(IV))
              ENDIF
            ENDIF
          ELSEIF(IN_TURNING_WAY(IV)) THEN
            CALL CHECK_SIGNAL(IV, IRL, WPERM, WPROT, REDLIGHT)
          ELSEIF(WILL_JUMP(IV)) THEN

! --- If the vehicle is a left turn jumper allow it to discharge immediately.
 
            WILL_JUMP(IV) = .FALSE.
            WPERM = .TRUE.
            SDISCH_TIMER(IV) = 0.0
          ELSE
            IF(REDRUNNER(IV)) THEN
              WPERM = .TRUE.
            ELSEIF(SGO_THRU_SIGNAL(IV)) THEN
              WPERM = .TRUE.
            ELSE
              CALL CHECK_SIGNAL(IV, IRL, WPERM, WPROT, REDLIGHT)
              IF(SGO_THRU_SIGNAL(IV)) WPERM = .TRUE.
              IF(SIGNAL_CODE(IL) .EQ. S_YIELD) ACCEL = MIN(ACCEL, 5.0)
            ENDIF
            IF(WEVRUN) THEN
              IF(.NOT. WPERM .AND. SFOLLOWER(IV) .NE. 0) THEN
                IF(SFLEET(SFOLLOWER(IV)) .EQ. FLEET_EV) THEN
                  IF(SSPEED(SFOLLOWER(IV)) .LE. STOP_SPD) THEN
                    WPERM = .TRUE.
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      
      !If the signal prevented the vehicle from discharging, the vehicle's desired speed
      !may have been reduced. Recalculate the desired acceleration.
      IF(.NOT. WPERM) THEN
        CALL ACCELERATE_STREET(IV, NEW_ACCEL)
        ACCEL = MIN(ACCEL, NEW_ACCEL)
      ENDIF
 
! --- Decrement the vehicle's discharge timer.
 
      SDISCH_TIMER(IV) = SDISCH_TIMER(IV) - TIMESTEP
      IF(SDISCH_TIMER(IV) .GT. 0.5 * TIMESTEP) WPERM = .FALSE.
 
! --- If the signal allows the vehicle to proceed, check the intersection for conflicting traffic.
 
      IF(WPERM .AND. IN_TURNING_WAY(IV)) THEN
        CAN_EXIT_RTW = .FALSE.
        IF(ARC_LOCATION(IV) + SSPEED(IV) * TIMESTEP .GE. ARC_LENGTH(IV)) THEN
          CALL CHECK_TURNING_WAY_EXIT(IV, ILD, IFL, CAN_EXIT_RTW)
        ELSE
          IF(SLENGTH(IL) - SLOCATION(IV) .LT. (SDESIREDSPEED(IV) ** 2) / 7) WPERM = .FALSE.
        ENDIF
        IF(CAN_EXIT_RTW) THEN
          IN_TURNING_WAY(IV) = .FALSE.
          ARC_LOCATION(IV) = 0
          ARC_LENGTH(IV) = 0
          FIRST_VEHICLE(IL, 1) = SFOLLOWER(IV)
          IF(IV .EQ. SLAST_VEHICLE(IL, 1)) SLAST_VEHICLE(IL, 1) = SFOLLOWER(IV)
          IF(SFOLLOWER(IV) .NE. 0) THEN
            SLEADER(SFOLLOWER(IV)) = 0
          ENDIF
          IF(IFL .NE. 0) SLEADER(IFL) = IV
          SDISCH_TIMER(IV) = 0.
          QSTATE(IV) = QS_NOTINQ
          IF(HAS_STOPPED(IV)) THEN
            HAS_STOPPED(IV) = .FALSE.
            IF(.NOT. INITMODE) THEN
              STOPPED_VEHICLES(IL) = STOPPED_VEHICLES(IL) + 1
            ENDIF
          ENDIF
          CALL PROCESS_STREET_NODE(IV, IRL)
          SLOCATION(IV) = RTW_ENTRY_POINT(RTW_RECEIVING_LINK(IL))
          SLEADER(IV) = ILD
          SFOLLOWER(IV) = IFL
          IF(ILD .NE. 0) THEN
            SFOLLOWER(ILD) = IV
          ELSE
            FIRST_VEHICLE(RTW_RECEIVING_LINK(IL), FIRST_FULL_LANE(RTW_RECEIVING_LINK(IL))) = IV
          ENDIF
          IF(IFL .EQ. 0) THEN
            SLAST_VEHICLE(RTW_RECEIVING_LINK(IL), FIRST_FULL_LANE(RTW_RECEIVING_LINK(IL))) = IV
          ENDIF
          RETURN
        ELSE
          WPERM = .FALSE.
        ENDIF
        
      ELSE
        IF(ITURN .EQ. TC_LEFT .AND. WPERM .AND. .NOT. WPROT .AND. OPPOSE_LINK(IL) .NE. 0) THEN
          IF(.NOT. WILL_JUMP(IV)) THEN
            CALL CHECK_OPPOSING_STREAM(IV, WPERM)
          ENDIF
        ENDIF

        IF(WPERM) THEN
          IF(SIGNAL_CODE(IL) .EQ. S_STOP .OR. SIGNAL_CODE(IL) .EQ. S_YIELD) THEN
            CALL CHECK_CROSS_STREETS(IV, IRL, IRLANE, WPERM)
            IF(WPERM .AND. ROUNDABOUT_APPROACH_NUM(IL) .NE. 0) CALL CHECK_UPSTREAM_LINK(IV, IRL, IRLANE, WPERM)
          ELSEIF(RTOR_FLAG(IV)) THEN
            CALL CHECK_RTOR(IV, IRLANE, WPERM)
          ENDIF
      
      ! --- Determine if there is spillback on the left link that prevents
      ! --- the vehicle from discharging.
      
          IF(WPERM .AND. STURNCODE(IV) .NE. TC_RIGHT) THEN
            IF(LEFT_LINK(IL) .NE. 0 .AND. STURNCODE(IV) .NE. TC_LEFT) THEN
              IF(UP_INT_WIDTH(LEFT_LINK(IL)) .NE. 0) THEN
                CALL CHECK_LEFTBLOCKAGE(IV, WPERM, LEFT_LINK(IL), ACCEL)
                IF(.NOT. WPERM .AND. ROUNDABOUT_ID(IL) .EQ. 0) WILL_YIELD(IV) = .TRUE.
              ENDIF
            ENDIF
            IF(WPERM .AND. LEFT_DIAG_LINK(IL) .NE. 0 .AND. STURNCODE(IV) .NE. TC_LDIAG) THEN
              IF(UP_INT_WIDTH(LEFT_DIAG_LINK(IL)) .NE. 0) THEN
                CALL CHECK_LEFTBLOCKAGE(IV, WPERM, LEFT_DIAG_LINK(IL), ACCEL)
                IF(.NOT. WPERM) WILL_YIELD(IV) = .TRUE.
              ENDIF
            ENDIF
          ENDIF
      
      ! --- Determine if there is spillback on the right link that prevents
      ! --- the vehicle from discharging.
      
          IF(WPERM .AND. STURNCODE(IV) .NE. TC_LEFT) THEN
            IF(RIGHT_LINK(IL) .NE. 0 .AND. STURNCODE(IV) .NE. TC_RIGHT) THEN
              IF(UP_INT_WIDTH(RIGHT_LINK(IL)) .NE. 0) THEN
                CALL CHECK_RIGHTBLOCKAGE(IV, WPERM, RIGHT_LINK(IL), ACCEL)
                IF(.NOT. WPERM) WILL_YIELD(IV) = .TRUE.
              ENDIF
            ENDIF
            IF(WPERM .AND. RIGHT_DIAG_LINK(IL) .NE. 0 .AND. STURNCODE(IV) .NE. TC_RDIAG) THEN
              IF(UP_INT_WIDTH(RIGHT_DIAG_LINK(IL)) .NE. 0) THEN
                CALL CHECK_RIGHTBLOCKAGE(IV, WPERM, RIGHT_DIAG_LINK(IL), ACCEL)
                IF(.NOT. WPERM) WILL_YIELD(IV) = .TRUE.
              ENDIF
            ENDIF
          ENDIF          
        ENDIF
           
  ! --- Determine if there is spillback on the receiving link that prevents
  ! --- the vehicle from discharging.
      
        IF(WPERM .AND. .NOT. VINRABT) THEN
          IF(IRL .NE. 0) THEN
            IF(UP_INT_WIDTH(IRL) .NE. 0) THEN
              CALL CHECK_SPILLBACK(IV, WPERM, IRL, IRLANE)
              IF(.NOT. WPERM .AND. ROUNDABOUT_ID(IL) .EQ. 0) WILL_YIELD(IV) = .TRUE.
            ENDIF
          ENDIF
        ENDIF
      
  ! --- Determine if there is an event on the receiving link that prevents
  ! --- the vehicle from discharging.
      
        IF(WPERM) THEN
          IF(IRL .NE. 0) THEN
            IF(BLOCKAGE(IRL, IRLANE) .NE. 0) THEN
              IEVENT = BLOCKAGE(IRL, IRLANE)
              IF(SIMTIME .GE. EVENT_BEGIN_TIME(IEVENT) .AND. SIMTIME .LE. EVENT_END_TIME(IEVENT)) THEN
                DIST = SLENGTH(IL) - SLOCATION(IV) + EVENT_LOCATION(IEVENT)
                CALL CHECK_BLOCKAGE(IV, DIST, ACCEL, WSTOP)
                IF(WSTOP) WPERM = .FALSE.
              ENDIF
            ENDIF
          ENDIF
        ENDIF

        IF(WPERM .AND. CHECK_MERGE(IL) .NE. 0) THEN
          CALL CHECK_MERGING_VEHICLE(IV, IRL, IRLANE, ACCEL)
        ENDIF

      ENDIF
              
      IF(IRL .NE. 0) THEN
        IF(NODE_TYPE(SDSN(IRL)) .NE. NT_EXTERN) THEN
          ILD = SLAST_VEHICLE_OUT(IL, ILANE)
 
! --- In some cases the last vehicle out has left the network
! --- and the ID may or may not represent a new vehicle.
! --- If so ignore it.
 
          IF(ILD .NE. 0) THEN
            IF(SLINK(ILD) .EQ. 0) THEN
              ILD = 0
              SLAST_VEHICLE_OUT(IL, ILANE) = 0
            ELSEIF(SLINK(ILD) .EQ. IL) THEN
              ILD = 0
              SLAST_VEHICLE_OUT(IL, ILANE) = 0
            ELSE                
              IF(SLINK(ILD) .NE. LEFT_LINK(IL) .AND.          &
                  SLINK(ILD) .NE. STHRU_LINK(IL) .AND.        &
                  SLINK(ILD) .NE. RIGHT_LINK(IL) .AND.        &
                  SLINK(ILD) .NE. LEFT_DIAG_LINK(IL) .AND.    &
                  SLINK(ILD) .NE. RIGHT_DIAG_LINK(IL)) THEN
                ILD = 0
                SLAST_VEHICLE_OUT(IL, ILANE) = 0
              ENDIF
            ENDIF
          ENDIF
          
          ! --- Car follow the last vehicle to exit from the current link and lane.              
          IF(ILD .NE. 0) THEN
            IF(SID(ILD) .EQ. SLAST_VEHICLE_OUT_ID(IL, ILANE)) THEN
              IF(SLANE(ILD) .EQ. IRLANE) THEN
                CALL STREET_CAR_FOLLOW(IV, ILD, ACCEL)
              ENDIF
            ENDIF
          ENDIF
          ! --- or car follow the last vehicle in the receiving lane
          IF(ILD .NE. SLAST_VEHICLE(IRL, IRLANE)) THEN
            ILD = SLAST_VEHICLE(IRL, IRLANE)
            IF(ILD .NE. 0) THEN
              CALL STREET_CAR_FOLLOW(IV, ILD, ACCEL)
            ENDIF
          ENDIF
          ! --- or if the vehicle is going thru to a short link car follow the last vehicle in the receiving lane on the next link
          IF(ILD .EQ. 0 .AND. SLENGTH(IRL) .LT. 200) THEN
            ITURN = STURNCODE(IV)
            IF(IRL .EQ. TURN_LINK(IV)) THEN
              STURNCODE(IV) = TURN_CODE(IV) 
            ENDIF
            IF(STURNCODE(IV) .EQ. TC_LEFT) THEN
              NLINK = LEFT_LINK(IRL)
            ELSEIF(STURNCODE(IV) .EQ. TC_THRU) THEN
              NLINK = STHRU_LINK(IRL)
            ELSEIF(STURNCODE(IV) .EQ. TC_RIGHT) THEN
              NLINK = RIGHT_LINK(IRL)
            ELSEIF(STURNCODE(IV) .EQ. TC_LDIAG) THEN
              NLINK = LEFT_DIAG_LINK(IRL)
            ELSEIF(STURNCODE(IV) .EQ. TC_RDIAG) THEN
              NLINK = RIGHT_DIAG_LINK(IRL)
            ENDIF
            IF(NLINK .NE. 0) THEN
              IL = SLINK(IV)
              SLINK(IV) = IRL
              CALL FIND_RECEIVING_LANE(IV, NLINK, ILANE, NRLANE)
              SLINK(IV) = IL
              STURNCODE(IV) = ITURN
              IF(NRLANE .NE. 0) THEN
                ILD = SLAST_VEHICLE(NLINK, NRLANE)
                IF(ILD .NE. 0) THEN
                  CALL STREET_CAR_FOLLOW(IV, ILD, ACCEL)
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      
      IF(.NOT. IN_TURNING_WAY(IV)) THEN
        IF(WPERM) THEN
 
  ! --- Determine if there are vehicles in the intersection that prevent
  ! --- the vehicle from discharging.
      
          CALL CHECK_INTERSECTION(IV, WPERM)
        ENDIF
      
        IF(.NOT. WPERM .AND. VINRABT) THEN
          DIST = SLENGTH(IL) - SLOCATION(IV)
          IF(DIST .LE. 0) THEN
            ACCEL = MIN(ACCEL, -SSPEED(IV))
          ELSE
            DECEL = (10. - SSPEED(IV)) ** 2 / (2 * DIST)
            ACCEL = MIN(ACCEL, DECEL)
          ENDIF
        ENDIF
      ENDIF
      
      MUST_STOP = .NOT. WPERM
      
! --- Determine if the vehicle can reach the end of the link during the current time step. 
!     (Adjusted for remainder of discharge timer.)

      EFFECTIVE_TIME = TIMESTEP - MAX(SDISCH_TIMER(IV), 0.)
      IF(SLAG_TIMER(IV) .GT. 0.0) THEN
        IF(SLAG_TIMER(IV) .GT. EFFECTIVE_TIME) THEN
          DISCHARGE_TRAVEL = EFFECTIVE_TIME * SSPEED(IV)
        ELSE
          EFFECTIVE_TIME = EFFECTIVE_TIME - SLAG_TIMER(IV)
          DISCHARGE_TRAVEL = SSPEED(IV) * SLAG_TIMER(IV) + SSPEED(IV) * EFFECTIVE_TIME + 0.5 * ACCEL * (EFFECTIVE_TIME**2)
          DISCHARGE_SPEED = SSPEED(IV) + EFFECTIVE_TIME * ACCEL
        ENDIF
      ELSE
        DISCHARGE_TRAVEL = SSPEED(IV) * EFFECTIVE_TIME + 0.5 * ACCEL * (EFFECTIVE_TIME**2)
        DISCHARGE_SPEED = SSPEED(IV) + EFFECTIVE_TIME * ACCEL
      ENDIF
      DISCHARGE_SPEED = MAX(DISCHARGE_SPEED, 0.)
      DISCHARGE_TRAVEL = MAX(DISCHARGE_TRAVEL, 0.)
      IF(SLANECODES(IV, ILANE) .NE. LC_GOOD) THEN
        DISCHARGE_SPEED = 0.
        DISCHARGE_TRAVEL = 0.
      ENDIF
      IF(SLOCATION(IV) + DISCHARGE_TRAVEL .LT. SLENGTH(IL)) THEN
        WPERM = .FALSE.
      ENDIF
      
    ENDIF
    
    IF(WPERM .OR. VINRABT) THEN
      WILL_YIELD(IV) = .FALSE.
      REDRUNNER(IV) = .FALSE.
      INDZ(IV) = .FALSE.
      TURN_INDICATOR(IV) = TC_THRU
      IF(.NOT. IN_TURNING_WAY(IV) .AND. SLOCATION(IV) + DISCHARGE_TRAVEL .GE. SLENGTH(IL)) THEN
        IF(ITURN .EQ. TC_LEFT) THEN
          IF(LT_ARC_LENGTH(IL, ILANE, IRLANE) .NE. 0.) THEN
            ARC_ENTRYLINK(IV) = IL
            ARC_ENTRYLANE(IV) = ILANE
            ARC_DIRECTION(IV) = TC_LEFT
            ARC_LOCATION(IV) = SLOCATION(IV) - SLENGTH(IL)
            ARC_LENGTH(IV) = LT_ARC_LENGTH(IL, ILANE, IRLANE)
            IF(LT_LIMITED_SPEED_DIST(IL) .NE. 0.) THEN
              LIMITED_SPEED_DIST(IV) = LT_LIMITED_SPEED_DIST(IL)
            ELSE
              !LIMITED_SPEED_DIST(IV) = LT_ARC_LENGTH(IL, SNUMLANES(IL), LAST_FULL_LANE(IRL))
              LIMITED_SPEED_DIST(IV) = ARC_LENGTH(IV)
            ENDIF
          ENDIF
        ELSEIF(ITURN .EQ. TC_THRU) THEN
          IF(THRU_ARC_LENGTH(IL, ILANE, IRLANE) .NE. 0.) THEN
            ARC_ENTRYLINK(IV) = IL
            ARC_ENTRYLANE(IV) = ILANE
            ARC_DIRECTION(IV) = ITURN
            ARC_LOCATION(IV) = SLOCATION(IV) - SLENGTH(IL)
            ARC_LENGTH(IV) = THRU_ARC_LENGTH(IL, ILANE, IRLANE)
          ENDIF
        ELSEIF(ITURN .EQ. TC_RIGHT) THEN
          IF(RT_ARC_LENGTH(IL, ILANE, IRLANE) .NE. 0.) THEN
            ARC_ENTRYLINK(IV) = IL
            ARC_ENTRYLANE(IV) = ILANE
            ARC_DIRECTION(IV) = ITURN
            ARC_LOCATION(IV) = SLOCATION(IV) - SLENGTH(IL)
            ARC_LENGTH(IV) = RT_ARC_LENGTH(IL, ILANE, IRLANE)
            IF(RT_LIMITED_SPEED_DIST(IL) .NE. 0.) THEN
              LIMITED_SPEED_DIST(IV) = RT_LIMITED_SPEED_DIST(IL)
            ELSE
              !LIMITED_SPEED_DIST(IV) = RT_ARC_LENGTH(IL, 1, FIRST_FULL_LANE(IRL))
              LIMITED_SPEED_DIST(IV) = ARC_LENGTH(IV)
            ENDIF
          ENDIF
        ELSEIF(ITURN .EQ. TC_LDIAG) THEN
          IF(LD_ARC_LENGTH(IL, ILANE, IRLANE) .NE. 0.) THEN
            ARC_ENTRYLINK(IV) = IL
            ARC_ENTRYLANE(IV) = ILANE
            ARC_DIRECTION(IV) = ITURN
            ARC_LOCATION(IV) = SLOCATION(IV) - SLENGTH(IL)
            ARC_LENGTH(IV) = LD_ARC_LENGTH(IL, ILANE, IRLANE)
          ENDIF
        ELSEIF(ITURN .EQ. TC_RDIAG) THEN
          IF(RD_ARC_LENGTH(IL, ILANE, IRLANE) .NE. 0.) THEN
            ARC_ENTRYLINK(IV) = IL
            ARC_ENTRYLANE(IV) = ILANE
            ARC_DIRECTION(IV) = ITURN
            ARC_LOCATION(IV) = SLOCATION(IV) - SLENGTH(IL)
            ARC_LENGTH(IV) = RD_ARC_LENGTH(IL, ILANE, IRLANE)
          ENDIF
        ENDIF
      ENDIF
    ELSEIF(MUST_STOP) THEN
      CALL STOP_AT_INTERSECTION(IV, ACCEL, REDLIGHT, WSTOP)
      
      !Recalculate discharge speed and travel distance
      EFFECTIVE_TIME = TIMESTEP - MAX(SDISCH_TIMER(IV), 0.)
      EFFECTIVE_TIME = MAX(EFFECTIVE_TIME, 0.)
      IF(SLAG_TIMER(IV) .GT. 0.0) THEN
        IF(SLAG_TIMER(IV) .GT. EFFECTIVE_TIME) THEN
          DISCHARGE_TRAVEL = EFFECTIVE_TIME * SSPEED(IV)
        ELSE
          EFFECTIVE_TIME = EFFECTIVE_TIME - SLAG_TIMER(IV)
          DISCHARGE_TRAVEL = SSPEED(IV) * SLAG_TIMER(IV) + SSPEED(IV) * EFFECTIVE_TIME + 0.5 * ACCEL * (EFFECTIVE_TIME**2)
          DISCHARGE_SPEED = SSPEED(IV) + EFFECTIVE_TIME * ACCEL
        ENDIF
      ELSE
        DISCHARGE_TRAVEL = SSPEED(IV) * EFFECTIVE_TIME + 0.5 * ACCEL * (EFFECTIVE_TIME**2)
        DISCHARGE_SPEED = SSPEED(IV) + EFFECTIVE_TIME * ACCEL
      ENDIF
      DISCHARGE_SPEED = MAX(DISCHARGE_SPEED, 0.)
      DISCHARGE_TRAVEL = MAX(DISCHARGE_TRAVEL, 0.)
      
      IF(SIGNAL_CODE(IL) .EQ. S_STOP .OR. SIGNAL_CODE(IL) .EQ. S_YIELD) THEN 
        WAIT_TIME(IV) = WAIT_TIME(IV) + TIMESTEP
        IF(SLOCATION(IV) .EQ. SLENGTH(IL) .AND. SDRIVERTYPE(IV) .LT. 10) THEN
          IF(WAIT_TIME(IV) .GE. 5.) THEN
            WAIT_TIME(IV) = 0.
            SDRIVERTYPE(IV) = SDRIVERTYPE(IV) + 1
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  
    IF(WPERM) THEN
 
! --- Subtract the length of the previous link to prepare for
! --- placement on the new link.
 
      Z1 = SLENGTH(IL) - SLOCATION(IV)
      SLOCATION(IV) = SLOCATION(IV) - SLENGTH(IL)
      SDRIVERTYPE(IV) = DRIVER_TYPE(IV)
 
! --- If the vehicle is performing a turn adjust its location to
! --- account for the width of the intersection, if not using an arc length.
      
      IF(ARC_LENGTH(IV) .EQ. 0) THEN
        IF(STURNCODE(IV) .EQ. TC_LEFT) THEN
          SLOCATION(IV) = SLOCATION(IV) + 0.5 * UP_INT_WIDTH(IRL)
        ELSEIF(STURNCODE(IV) .EQ. TC_RIGHT) THEN
          IF(IN_TURNING_WAY(IV)) THEN
            IN_TURNING_WAY(IV) = .FALSE.
            SLOCATION(IV) = RTW_ENTRY_POINT(RTW_RECEIVING_LINK(IL)) 
            IRL = RTW_RECEIVING_LINK(IL)
          ELSE
            SLOCATION(IV) = SLOCATION(IV) + UP_INT_WIDTH(IRL)
          ENDIF
        ENDIF
      ENDIF
      
! --- Reset the discharge timer and the queue status.
 
      SDISCH_TIMER(IV) = 0.
      QSTATE(IV) = QS_NOTINQ
      IF(HAS_STOPPED(IV)) THEN
        HAS_STOPPED(IV) = .FALSE.
        IF(.NOT. INITMODE) THEN
          STOPPED_VEHICLES(IL) = STOPPED_VEHICLES(IL) + 1
        ENDIF
      ENDIF
 
! --- If the receiving link was not identified it may be an exit link.
 
      IF(IRL .EQ. 0) THEN
        IF(NODE_TYPE(SDSN(IL)) .NE. NT_INTERN) THEN
          IF(.NOT. INITMODE) THEN
            EXITING_VEHICLES = EXITING_VEHICLES + 1
          ENDIF
 
! --- The vehicle is leaving the network.
! --- Update first and last vehicle on previous link if necessary.
 
          FIRST_VEHICLE(IL, ILANE) = SFOLLOWER(IV)
          IF(IV .EQ. SLAST_VEHICLE(IL, ILANE)) THEN
            SLAST_VEHICLE(IL, ILANE) = 0
          ENDIF
          IF(NODE_TYPE(SDSN(IL)) .NE. NT_EXTERN) THEN
            SLAST_VEHICLE_OUT(IL, ILANE) = IV
            SLAST_VEHICLE_OUT_ID(IL, ILANE) = SID(IV)
            IF(SCROSS1(SDSN(IL), SLANE(IV)) .EQ. 0) THEN
              SCROSS1(SDSN(IL), SLANE(IV)) = IV
            ELSE
              SCROSS2(SDSN(IL), SLANE(IV)) = IV
            ENDIF
            IF(SFOLLOWER(IV) .NE. 0) THEN
              IF(SLEADER(SFOLLOWER(IV)) .EQ. IV) THEN
                SLEADER(SFOLLOWER(IV)) = 0
              ENDIF
              SFOLLOWER(IV) = 0
            ENDIF
          ELSE
            IF(SFLEET(IV) .EQ. FLEET_EV) EV_COUNT = EV_COUNT - 1
            IF(WRITE_SUPPLEMENTAL_FILES) CALL WRITE_VEHICLE_EXIT_TEXT(SID(IV), SDSN(STHRU_LINK(IL)))
            CALL DELETE_STREET_VEHICLE(IV, .FALSE.)
          ENDIF
        ENDIF
      ELSE
 
! --- Update first and last vehicle on previous link if necessary.
 
        IF(IV .EQ. FIRST_VEHICLE(IL, ILANE)) THEN
          FIRST_VEHICLE(IL, ILANE) = SFOLLOWER(IV)
        ENDIF
        IF(IV .EQ. SLAST_VEHICLE(IL, ILANE)) THEN
          SLAST_VEHICLE(IL, ILANE) = 0
        ENDIF
 
! --- If the vehicle had a follower on the previous link, clear it.
 
        IF(SFOLLOWER(IV) .NE. 0) SLEADER(SFOLLOWER(IV)) = 0
        SFOLLOWER(IV) = 0
        IF(NODE_TYPE(SDSN(IRL)) .EQ. NT_EXTERN) THEN
          IF(.NOT. INITMODE) THEN
            CALL UPDATE_STREET_STATS(IV, IL, 0, Z1, 0., TIMESTEP, 0.)
            EXITING_VEHICLES = EXITING_VEHICLES + 1
          ENDIF
          CALL PROCESS_STREET_NODE(IV, IRL)
          IF(SFLEET(IV) .EQ. FLEET_EV) EV_COUNT = EV_COUNT - 1
          IF(WRITE_SUPPLEMENTAL_FILES) CALL WRITE_VEHICLE_EXIT_TEXT(SID(IV), SDSN(STHRU_LINK(IL)))
          CALL DELETE_STREET_VEHICLE(IV, .FALSE.)
        ELSE
 
! --- Find the last vehicle in the new lane.
 
          ILD = SLAST_VEHICLE(IRL, IRLANE)
          IF(ILD .NE. 0) THEN
            IF(SLINK(ILD) .NE. IRL) ILD = 0
          ENDIF
          SLEADER(IV) = ILD
          IF(ILD .NE. 0) SFOLLOWER(ILD) = IV
 
! --- Update first and last vehicle on the receiving link.
 
          SLAST_VEHICLE(IRL, IRLANE) = IV
          IF(FIRST_VEHICLE(IRL, IRLANE) .EQ. 0) FIRST_VEHICLE(IRL, IRLANE) = IV
          CALL PROCESS_STREET_NODE(IV, IRL)
          SLANE(IV) = IRLANE
          SGO_THRU_SIGNAL(IV) = .FALSE.
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE FIND_RECEIVING_LANE(IV, IRL, ILANE, RLANE)
! ----------------------------------------------------------------------
! --- Find the lane on the receiving link that aligns with the lane
! --- that the vehicle is discharging from.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE GLOBAL_DATA
  USE SEEDS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, IRL, ILANE
  INTEGER, INTENT(OUT) :: RLANE
  INTEGER :: IL, ILN, NTHRU
  REAL :: RNDNUM
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

  IL = SLINK(IV)
  IF(STURNCODE(IV) .EQ. TC_LEFT .OR. STURNCODE(IV) .EQ. TC_LDIAG) THEN
 
! --- Turning left. Account for any full lanes to the left plus any
! --- left turn pocket lanes.
 
    CALL GETLANESLEFT(IL, ILANE, ILN)
    RLANE = MAX(LAST_FULL_LANE(IRL) - ILN, FIRST_FULL_LANE(IRL))
    IF(RLANE .NE. 0) THEN
      IF(SLANE_CLOSED(IRL, RLANE)) THEN
        DO
          RLANE = RLANE - 1
          IF(RLANE .EQ. 0) EXIT
          IF(.NOT. SLANE_CLOSED(IRL, RLANE)) EXIT
        ENDDO
      ENDIF
    ENDIF

  ELSEIF(STURNCODE(IV) .EQ. TC_THRU) THEN
 
! --- Going through. Account for lane alignment. 
 
    RLANE = ILANE + STHRU_ALIGNMENT_LANE(IL) - SALIGNMENT_LANE(IL)
    IF(RLANE .GT. 0) THEN
      IF(RLANE .LT. FIRST_FULL_LANE(IRL)) THEN
        RLANE = FIRST_FULL_LANE(IRL)
      ELSEIF(RLANE .GT. LAST_FULL_LANE(IRL)) THEN
        RLANE = LAST_FULL_LANE(IRL)
      ELSEIF(SLANE_CLOSED(IRL, RLANE)) THEN
        CALL STREET_RANDOM(SSEED, RNDNUM)
        IF(RNDNUM .LT. 0.5) THEN
          DO
            RLANE = RLANE - 1
            IF(RLANE .LT. FIRST_FULL_LANE(IRL)) EXIT
            IF(.NOT. SLANE_CLOSED(IRL, RLANE)) EXIT
          ENDDO
        ELSE
          DO
            RLANE = RLANE + 1
            IF(RLANE .GT. LAST_FULL_LANE(IRL)) EXIT
            IF(.NOT. SLANE_CLOSED(IRL, RLANE)) EXIT
          ENDDO
        ENDIF
      ENDIF
    ENDIF

  ELSEIF(STURNCODE(IV) .EQ. TC_RIGHT .OR. STURNCODE(IV) .EQ. TC_RDIAG) THEN
 
! --- Turning right. Account for any full lanes to the right plus any
! --- right turn pocket lanes.
 
    CALL GETLANESRIGHT(IL, ILANE, ILN)
    IF(RTW_EXIT_POINT(IL) .NE. 0 .AND. .NOT. IN_TURNING_WAY(IV)) ILN = ILN - 1
    RLANE = ILN + FIRST_FULL_LANE(IRL)
    IF(RLANE .NE. 0) THEN
      IF(SLANE_CLOSED(IRL, RLANE)) THEN
        DO
          RLANE = RLANE + 1
          IF(RLANE .GT. LAST_FULL_LANE(IRL)) EXIT
          IF(.NOT. SLANE_CLOSED(IRL, RLANE)) EXIT
        ENDDO
        IF(RLANE .GT. LAST_FULL_LANE(IRL)) RLANE = 0
      ENDIF
    ENDIF
    
  ENDIF
  
  IF(RLANE .LT. 0) RLANE = 0
  RETURN
  END

! ==================================================================================================
  SUBROUTINE GETLANESLEFT(IL, ILANE, ILN)
! ----------------------------------------------------------------------
! --- Count the number of lanes to the left.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IL, ILANE
  INTEGER, INTENT(OUT) :: ILN
! ----------------------------------------------------------------------
  ILN = TOTAL_LANES(IL) - ILANE
  RETURN
  END

! ==================================================================================================
  SUBROUTINE GETLANESRIGHT(IL, ILANE, ILN)
! ----------------------------------------------------------------------
! --- Count the number of lanes to the right.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IL, ILANE
  INTEGER, INTENT(OUT) :: ILN
! ----------------------------------------------------------------------
  ILN = ILANE - 1
  RETURN
  END

! ==================================================================================================
  SUBROUTINE CHECK_SPILLBACK(IV, WPERM, IRL, IRLANE)
! ----------------------------------------------------------------------
! --- Check for spillback on the receiving link.
! ----------------------------------------------------------------------
  USE SIMPARAMS
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE GLOBAL_DATA
  USE TEXT
  USE SEEDS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, IRL, IRLANE
  LOGICAL, INTENT(INOUT) :: WPERM
  INTEGER :: IL, IVX, NV, IVXL, VLEN, LASTV, SPACE_AHEAD
  REAL :: BACK_BUMPER, RNDNUM, ACCEL, LOCATION
  LOGICAL :: WSPILL
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

  IF(SFLEET(IV) .NE. FLEET_BIKE) THEN

    WSPILL = .FALSE.
    BACK_BUMPER = 0
    VLEN = 0
    LASTV = 0
    IVX = SLAST_VEHICLE(IRL, IRLANE)
    
   !Find the last vehicle that is stopped on the receiving link
    DO WHILE(IVX .NE. 0)
      VLEN = VLEN + SVLENGTH(IVX)
      IF(SLOCATION(IVX) - VLEN .GT. UP_INT_WIDTH(IRL) + SVLENGTH(IV)) EXIT
      IF(SSPEED(IVX) .LE. STOP_SPD .OR. (SSPEED(IVX) .LE. 10 .AND. SACCELERATION(IVX) .LT. 0.0)) THEN
        LASTV = IVX
        EXIT
      ENDIF
      IVX = SLEADER(IVX)
    ENDDO
    
    IF(LASTV .NE. 0) THEN
      !Determine how much open space is ahead of the vehicle.
      IF(SLEADER(LASTV) .NE. 0) THEN
        SPACE_AHEAD = SLOCATION(SLEADER(LASTV)) - SVLENGTH(SLEADER(LASTV))
      ELSE
        SPACE_AHEAD = SLENGTH(IRL) - SLOCATION(LASTV)
      ENDIF
      WSPILL = SLOCATION(LASTV) + SPACE_AHEAD - VLEN .LT. SVLENGTH(IV)
    ENDIF
    
    IF(WSPILL) THEN
      BACK_BUMPER = LOCATION - SVLENGTH(IVX) - UP_INT_WIDTH(IRL)
      IF(LOCATION - SVLENGTH(IVX) .LT. 0.) THEN
        WPERM = .FALSE.
      ELSEIF(BACK_BUMPER .LT. 0.0) THEN
        IF(STURNCODE(IV) .EQ. TC_RIGHT) THEN
          WPERM = .FALSE.
        ELSE
          CALL STREET_RANDOM(SSEED, RNDNUM)
          NV = 1 - BACK_BUMPER / 20
          NV = MAX(NV, 1)
          NV = MIN(NV, 4)
          IF(RNDNUM .LT. 1.0 - SPILLBACK_PROB(NV)) WPERM = .FALSE.
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE CHECK_UTURNBLOCKAGE(IV, WPERM, IRL, IRLANE, ACCEL)
! ----------------------------------------------------------------------
! --- Check for spillback on the u-turn link that blocks a thru vehicle.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE GLOBAL_DATA
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, IRL, IRLANE
  LOGICAL, INTENT(INOUT) :: WPERM
  REAL, INTENT(INOUT) :: ACCEL
  INTEGER :: IL, IVX, NV, IVXL
  REAL :: VPOS
  LOGICAL :: WSPILL
  !INTEGER :: LOOP_COUNTER
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

  IL = SLINK(IV)
  IVX = SLAST_VEHICLE(IRL, IRLANE)
  IF(IVX .NE. 0) THEN
    if(prev_turncode(ivx) .eq. tc_left) then
 
! --- Determine if the last vehicle is in spillback.
 
      WSPILL = .FALSE.
      IF(SFLEET(IV) .NE. FLEET_BIKE) THEN
        IF(QSTATE(IVX) .NE. QS_NOTINQ .OR. SSPEED(IVX) .LE. STOP_SPD) THEN
          WSPILL = .TRUE.
        ELSEIF(UP_INT_WIDTH(IRL) .GT. 0 .AND. SLOCATION(IVX) - SVLENGTH(IVX) .LT. UP_INT_WIDTH(IRL)) THEN
          IVXL = SLEADER(IVX)
          NV = 0
          !LOOP_COUNTER = 0
          DO WHILE(IVXL .NE. 0) 
            !LOOP_COUNTER = LOOP_COUNTER + 1
            !IF(LOOP_COUNTER .GT. 1000) THEN
            !  WRITE(MSGTEXT, '(A)') 'CHECK_UTURNBLOCKAGE: INFINITE LOOP'
            !  CALL SENDTEXTMSG(M_ERROR)
            !  EXIT
            !ENDIF            
            NV = NV + 1
            IF(NV .EQ. 4) EXIT
            IF(SSPEED(IVXL) .LE. STOP_SPD .OR. SACCELERATION(IVXL) .LT. -1.0) THEN
              CALL STREET_CAR_FOLLOW(IVX, IVXL, ACCEL)
              IF(ACCEL .LT. 0.) THEN
                WSPILL = .TRUE.
                EXIT
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDIF
      IF(WSPILL) THEN
        VPOS = SLANE_WIDTH(IL, 1) * (SLANE(IV) + NUMBER_RIGHTPOCKETS(IL))
        IF(SLOCATION(IVX) - SVLENGTH(IVX) .LT. VPOS) THEN
          WPERM = .FALSE.
        ENDIF
      ENDIF
    ENDIF
  endif
  RETURN
  END      
! ==================================================================================================
  SUBROUTINE CHECK_LEFTBLOCKAGE(IV, WPERM, IRL, ACCEL)
! ----------------------------------------------------------------------
! --- Check for spillback on the left link that blocks a thru vehicle.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE GLOBAL_DATA
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, IRL
  LOGICAL, INTENT(INOUT) :: WPERM
  REAL, INTENT(INOUT) :: ACCEL
  INTEGER :: IL, ILN, IVX, BLOCKDIST
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

  IL = SLINK(IV)
  !If the left receiving link is the same as the thru link disregard any blockage
  IF(IRL .NE. STHRU_LINK(IL)) THEN
    BLOCKDIST = SLANE(IV) * SLANE_WIDTH(IL, 1) + 3
    DO ILN = FIRST_FULL_LANE(IRL), LAST_FULL_LANE(IRL)
      IVX = SLAST_VEHICLE(IRL, ILN)
      IF(IVX .NE. 0) THEN
      
    ! --- Determine if the last vehicle in each lane will block the discharging vehicle.
        IF(SLOCATION(IVX) - SVLENGTH(IVX) .LT. BLOCKDIST) THEN
          IF(SPREVLINK(IVX) .EQ. SLINK(IV) .AND. SPREVLANE(IVX) .GT. SLANE(IV)) THEN
            CYCLE
          ELSE
            WPERM = .FALSE.
            EXIT
          ENDIF
        ENDIF
      ENDIF
    ENDDO
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE CHECK_RIGHTBLOCKAGE(IV, WPERM, IRL, ACCEL)
! ----------------------------------------------------------------------
! --- Check for spillback on the right link that blocks a thru vehicle.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE GLOBAL_DATA
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, IRL
  LOGICAL, INTENT(INOUT) :: WPERM
  REAL, INTENT(INOUT) :: ACCEL
  INTEGER :: IL, ILN, IVX, BLOCKDIST
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

  IL = SLINK(IV)
  !If the right receiving link is the same as the thru link disregard any blockage
  IF(IRL .NE. STHRU_LINK(IL)) THEN
    BLOCKDIST = UP_INT_WIDTH(IRL) - (SLANE(IV) - 1) * SLANE_WIDTH(IL, 1) + 3
    IF(ROUNDABOUT_ID(IL) .NE. 0) BLOCKDIST = MIN(BLOCKDIST, TOTAL_LANES(IL) * 12 - 6)
    DO ILN = FIRST_FULL_LANE(IRL), LAST_FULL_LANE(IRL)
      IVX = SLAST_VEHICLE(IRL, ILN)
      IF(IVX .NE. 0) THEN
 
    ! --- Determine if the last vehicle in each lane will block the discharging vehicle.
        IF(SLOCATION(IVX) - SVLENGTH(IVX) .LT. BLOCKDIST) THEN
          IF(SPREVLINK(IVX) .EQ. SLINK(IV) .AND. SPREVLANE(IVX) .LE. SLANE(IV)) THEN
            CYCLE
          ELSEIF(ROUNDABOUT_EXIT_NUM(IRL) .NE. 0 .AND. SSPEED(IVX) .GT. STOP_SPD) THEN
            IF(SLOCATION(IVX) .LT. UP_INT_WIDTH(IRL)) THEN
              WPERM = .FALSE.
              EXIT
            ENDIF
          ELSEIF(SSPEED(IVX) .LE. STOP_SPD) THEN
            WPERM = .FALSE.
            EXIT
          ENDIF
        ENDIF
      ENDIF
    ENDDO
  ENDIF
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE PROCESS_QUEUED_VEHICLES(IL, ICODE, JCODE)
! ----------------------------------------------------------------------
! --- Loop through vehicles in queue and set each vehicle's queue
! --- state and discharge timer.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE STREET_VEHICLES
  USE GLOBAL_DATA
  USE DISTRIBUTIONS
  USE SIMPARAMS
  USE TEXT
  USE SEEDS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IL, ICODE, JCODE
  INTEGER :: ILANE, IV, QPOS, ITYPE, DTYPE
  INTEGER :: ILC, ITC, IRC, IDC
  INTEGER :: JLC, JTC, JRC, JDC, IVX
  LOGICAL :: AMBER
  REAL :: PDELAY, DELAY, RNDNUM
  !INTEGER :: LOOP_COUNTER
! ----------------------------------------------------------------------
 
! --- Previous signal code... ICODE
! --- Current signal code... JCODE
 
! --- Previous signal codes for left, thru, right diagonal...
! --- ILC, ITC, IRC, IDC
 
! --- Current signal codes for left, thru, right diagonal...
! --- JLC, JTC, JRC, JDC
 
  CALL DECODE_FTC(ICODE, ILC, ITC, IRC, IDC, AMBER)
  CALL DECODE_FTC(JCODE, JLC, JTC, JRC, JDC, AMBER)
  IF(ILC .EQ. S_GREEN .AND. JLC .EQ. S_RED) LAGGER_TIMER(IL) = 0.0
 
! --- Loop over lanes and apply change in status to queued vehicles
! --- when the signal has changed from red to green.
 
! --- Set QSTATE to indicate the vehicles that are tagged for
! --- cycle failure.
 
  IF(.NOT. AMBER) THEN
    DO ILANE = 1, N_STREET_LANES
      IV = FIRST_VEHICLE(IL, ILANE)
      QPOS = 0
      DO WHILE(IV .NE. 0)
        IF(SID(IV) .EQ. 0) EXIT
        FORCE_STOP(IV) = .FALSE.
        SDISCH_TIMER(IV) = 0.
        IF(QSTATE(IV) .EQ. QS_NOTINQ) EXIT
        IF(QSTATE(IV) .NE. QS_DWELL) THEN
          ITYPE = SVTYPE(IV)
          DTYPE = SDRIVERTYPE(IV)
          
          !Determine the vehicle's queue position.
          QPOS = QPOS + 1
          
          !If the vehicle is not in a lane that it can discharge from
          !set it to not in queue, and do the same for all the vehicles behind it.
          IF(SLANECODES(IV, SLANE(IV)) .NE. LC_GOOD) THEN
            QSTATE(IV) = QS_NOTINQ
            IVX = IV
            IVX = SFOLLOWER(IVX)
            !LOOP_COUNTER = 0
            DO WHILE(IVX .NE. 0)
              !LOOP_COUNTER = LOOP_COUNTER + 1
              !IF(LOOP_COUNTER .GT. 1000) THEN
              !  WRITE(MSGTEXT, '(A)') 'PROCESS_QUEUED_VEHICLES: INFINITE LOOP'
              !  CALL SENDTEXTMSG(M_ERROR)
              !  EXIT
              !ENDIF            
              QSTATE(IVX) = QS_NOTINQ
              IVX = SFOLLOWER(IVX)
            ENDDO
          ENDIF
          DELAY = 0.
          
! --- Left turners

          IF(STURNCODE(IV) .EQ. TC_LEFT) THEN
            IF(ILC .EQ. S_RED .AND. JLC .EQ. S_GREEN) THEN !changed from red to green on this timestep
              IF(QPOS .EQ. 1 .AND. OPPOSE_LINK(IL) .NE. 0) THEN
 
  ! --- Determine if the vehicle will be a jumper.
 
                WILL_JUMP(IV) = .FALSE.
                CALL STREET_RANDOM(SSEED, RNDNUM)
                IF(RNDNUM .LE. LT_JUMPER_PROB(SNUMLANES(IL))) WILL_JUMP(IV) = .TRUE.
              ENDIF
              QSTATE(IV) = QS_CFMOVING
              IF(SSPEED(IV) .LE. STOP_SPD) QSTATE(IV) = QS_CFSTOPPED
              IF(QPOS .LE. 5) THEN
                DELAY = STARTUP_MULT(DTYPE) * QFACTOR(QPOS) * SSTARTUP_TIME(IL) + HDWY_FACTOR(ITYPE) * HDWY_MULTIPLIER(DTYPE) * QDISCHARGE_HDWY(IL)
              ENDIF 
              
  ! --- Check for pedestrian delay.
 
              CALL CHECK_PED_DELAY(IV, PDELAY)
              SDISCH_TIMER(IV) = MAX(DELAY, PDELAY)
            ELSEIF(ILC .EQ. S_GREEN .AND. JLC .EQ. S_RED) THEN !changed to red on this timestep
              IF(.NOT. INITMODE) THEN
                IF(QSTATE(IV) .EQ. QS_CFSTOPPED .OR. QSTATE(IV) .EQ. QS_CFMOVING) THEN
                  CYCLE_FAILURES(IL) = CYCLE_FAILURES(IL) + 1
                ENDIF
              ENDIF
              QSTATE(IV) = QS_MOVING
              IF(SSPEED(IV) .LE. STOP_SPD) QSTATE(IV) = QS_STOPPED 
            ENDIF
            
! --- Thru vehicles

          ELSEIF(STURNCODE(IV) .EQ. TC_THRU) THEN
            IF(ITC .EQ. S_RED .AND. JTC .EQ. S_GREEN) THEN !changed from red to green on this timestep
              QSTATE(IV) = QS_CFMOVING
              IF(SSPEED(IV) .LE. STOP_SPD) QSTATE(IV) = QS_CFSTOPPED
              IF(QPOS .LE. 5) THEN
                DELAY = STARTUP_MULT(DTYPE) * QFACTOR(QPOS) * SSTARTUP_TIME(IL) + HDWY_FACTOR(ITYPE) * HDWY_MULTIPLIER(DTYPE) * QDISCHARGE_HDWY(IL)
              ENDIF              
              SDISCH_TIMER(IV) = DELAY
            ELSEIF(ITC .EQ. S_GREEN .AND. JTC .EQ. S_RED) THEN !changed to red on this timestep
              IF(.NOT. INITMODE) THEN
                IF(QSTATE(IV) .EQ. QS_CFSTOPPED .OR. QSTATE(IV) .EQ. QS_CFMOVING) THEN
                  CYCLE_FAILURES(IL) = CYCLE_FAILURES(IL) + 1
                ENDIF
              ENDIF
              QSTATE(IV) = QS_MOVING
              IF(SSPEED(IV) .LE. STOP_SPD) QSTATE(IV) = QS_STOPPED 
            ENDIF
            
! --- Right turners

          ELSEIF(STURNCODE(IV) .EQ. TC_RIGHT) THEN
            IF(IRC .EQ. S_RED .AND. JRC .EQ. S_GREEN) THEN !changed from red to green on this timestep
              QSTATE(IV) = QS_CFMOVING
              IF(SSPEED(IV) .LE. STOP_SPD) QSTATE(IV) = QS_CFSTOPPED
              IF(QPOS .LE. 5) THEN
                DELAY = STARTUP_MULT(DTYPE) * QFACTOR(QPOS) * SSTARTUP_TIME(IL) + HDWY_FACTOR(ITYPE) * HDWY_MULTIPLIER(DTYPE) * QDISCHARGE_HDWY(IL)
              ENDIF 
              
  ! --- Check for pedestrian delay.
 
              CALL CHECK_PED_DELAY(IV, PDELAY)
              SDISCH_TIMER(IV) = MAX(DELAY, PDELAY)
            ELSEIF(IRC .EQ. S_GREEN .AND. JRC .EQ. S_RED) THEN !changed to red on this timestep
              IF(.NOT. INITMODE) THEN
                IF(QSTATE(IV) .EQ. QS_CFSTOPPED .OR. QSTATE(IV) .EQ. QS_CFMOVING) THEN
                  CYCLE_FAILURES(IL) = CYCLE_FAILURES(IL) + 1
                ENDIF
              ENDIF
              QSTATE(IV) = QS_MOVING
              IF(SSPEED(IV) .LE. STOP_SPD) QSTATE(IV) = QS_STOPPED 
            ENDIF
            
! --- Diagonals

          ELSEIF(STURNCODE(IV) .GT. TC_RIGHT) THEN
            IF(IDC .EQ. S_RED .AND. JDC .EQ. S_GREEN) THEN !changed from red to green on this timestep
              QSTATE(IV) = QS_CFMOVING
              IF(SSPEED(IV) .LE. STOP_SPD) QSTATE(IV) = QS_CFSTOPPED
              IF(QPOS .LE. 5) THEN
                DELAY = STARTUP_MULT(DTYPE) * QFACTOR(QPOS) * SSTARTUP_TIME(IL) + HDWY_FACTOR(ITYPE) * HDWY_MULTIPLIER(DTYPE) * QDISCHARGE_HDWY(IL)
              ENDIF
              SDISCH_TIMER(IV) = DELAY
            ELSEIF(IDC .EQ. S_GREEN .AND. JDC .EQ. S_RED) THEN !changed to red on this timestep
              IF(.NOT. INITMODE) THEN
                IF(QSTATE(IV) .EQ. QS_CFSTOPPED .OR. QSTATE(IV) .EQ. QS_CFMOVING) THEN
                  CYCLE_FAILURES(IL) = CYCLE_FAILURES(IL) + 1
                ENDIF
              ENDIF
              QSTATE(IV) = QS_MOVING
              IF(SSPEED(IV) .LE. STOP_SPD) QSTATE(IV) = QS_STOPPED 
            ENDIF
          ENDIF
        ENDIF
        IV = SFOLLOWER(IV)
      ENDDO
    ENDDO
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE PROCESS_QUEUED_VEHICLES_AC(IL, ILC, ITC, IRC, IDC, JLC, JTC, JRC, JDC)
! ----------------------------------------------------------------------
! --- Loop through vehicles in queue and set each vehicle's queue
! --- state and discharge timer.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE STREET_VEHICLES
  USE GLOBAL_DATA
  USE DISTRIBUTIONS
  USE SIMPARAMS
  USE TEXT
  USE SEEDS
  USE ACTUATED_CONTROLLERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IL, ILC, ITC, IRC, IDC, JLC, JTC, JRC, JDC
  INTEGER :: ILANE, IV, QPOS, IVX, ITYPE, DTYPE
  REAL :: PDELAY, DELAY, RNDNUM
  !INTEGER :: LOOP_COUNTER
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
#endif  
 
! --- Previous signal code... ICODE
! --- Current signal code... JCODE
 
! --- Previous signal codes for left, thru, right diagonal...
! --- ILC, ITC, IRC, IDC
 
! --- Current signal codes for left, thru, right diagonal...
! --- JLC, JTC, JRC, JDC
 
  IF(ILC .EQ. S_GREEN .AND. JLC .EQ. S_RED) LAGGER_TIMER(IL) = 0.0
 
! --- Loop over lanes and apply change in status to queued vehicles
! --- when the signal has changed from red to green.
 
! --- Set QSTATE to indicate the vehicles that are tagged for
! --- cycle failure.
 
  DO ILANE = 1, N_STREET_LANES
    IV = FIRST_VEHICLE(IL, ILANE)
    QPOS = 0
    DO WHILE(IV .NE. 0)
#ifdef DebugVersion
      temp = sid(iv)
#endif      
      IF(SID(IV) .EQ. 0) EXIT
      SDISCH_TIMER(IV) = 0.
      FORCE_STOP(IV) = .FALSE.
      IF(QSTATE(IV) .EQ. QS_NOTINQ) EXIT
      IF(QSTATE(IV) .NE. QS_DWELL) THEN
        ITYPE = SVTYPE(IV)
        DTYPE = (LINKTYPE_CODE(IL) - 1) * 10 + SDRIVERTYPE(IV)
          
        !Determine the vehicle's queue position.
        QPOS = QPOS + 1
          
        !If the vehicle is not in a lane that it can discharge from
        !set it to not in queue, and do the same for all the vehicles behind it.
        IF(SLANECODES(IV, SLANE(IV)) .NE. LC_GOOD) THEN
          QSTATE(IV) = QS_NOTINQ
          IVX = IV
          IVX = SFOLLOWER(IVX)
          !LOOP_COUNTER = 0
          DO WHILE(IVX .NE. 0)
            !LOOP_COUNTER = LOOP_COUNTER + 1
            !IF(LOOP_COUNTER .GT. 1000) THEN
            !  WRITE(MSGTEXT, '(A)') 'PROCESS_QUEUED_VEHICLES: INFINITE LOOP'
            !  CALL SENDTEXTMSG(M_ERROR)
            !  EXIT
            !ENDIF            
            QSTATE(IVX) = QS_NOTINQ
            IVX = SFOLLOWER(IVX)
          ENDDO
        ENDIF
        DELAY = 0.
          
! --- Left turners

        IF(STURNCODE(IV) .EQ. TC_LEFT) THEN
          IF(ILC .EQ. S_RED .AND. JLC .EQ. S_GREEN) THEN !changed from red to green on this timestep
            IF(QPOS .EQ. 1 .AND. OPPOSE_LINK(IL) .NE. 0) THEN
 
! --- Determine if the vehicle will be a jumper.
 
              WILL_JUMP(IV) = .FALSE.
              CALL STREET_RANDOM(SSEED, RNDNUM)
              IF(RNDNUM .LE. LT_JUMPER_PROB(SNUMLANES(IL))) WILL_JUMP(IV) = .TRUE.
            ENDIF
            QSTATE(IV) = QS_CFMOVING
            IF(SSPEED(IV) .LE. STOP_SPD) QSTATE(IV) = QS_CFSTOPPED
            IF(QPOS .LE. 5) THEN
              DELAY = STARTUP_MULT(DTYPE) * QFACTOR(QPOS) * SSTARTUP_TIME(IL) + HDWY_FACTOR(ITYPE) * HDWY_MULTIPLIER(DTYPE) * QDISCHARGE_HDWY(IL)
            ENDIF 
              
! --- Check for pedestrian delay.
 
            CALL CHECK_PED_DELAY(IV, PDELAY)
            SDISCH_TIMER(IV) = MAX(DELAY, PDELAY)
          ELSEIF(ILC .EQ. S_GREEN .AND. JLC .EQ. S_RED) THEN !changed to red on this timestep
            IF(.NOT. INITMODE) THEN
              IF(QSTATE(IV) .EQ. QS_CFSTOPPED .OR. QSTATE(IV) .EQ. QS_CFMOVING) THEN
                CYCLE_FAILURES(IL) = CYCLE_FAILURES(IL) + 1
              ENDIF
            ENDIF
            QSTATE(IV) = QS_MOVING
            IF(SSPEED(IV) .LE. STOP_SPD) QSTATE(IV) = QS_STOPPED 
          ENDIF
            
! --- Thru vehicles

        ELSEIF(STURNCODE(IV) .EQ. TC_THRU) THEN
          IF(ITC .EQ. S_RED .AND. JTC .EQ. S_GREEN) THEN !changed from red to green on this timestep
            QSTATE(IV) = QS_CFMOVING
            IF(SSPEED(IV) .LE. STOP_SPD) QSTATE(IV) = QS_CFSTOPPED
            IF(QPOS .LE. 5) THEN
              DELAY = STARTUP_MULT(DTYPE) * QFACTOR(QPOS) * SSTARTUP_TIME(IL) + HDWY_FACTOR(ITYPE) * HDWY_MULTIPLIER(DTYPE) * QDISCHARGE_HDWY(IL)
            ENDIF              
            SDISCH_TIMER(IV) = DELAY
          ELSEIF(ITC .EQ. S_GREEN .AND. JTC .EQ. S_RED) THEN !changed to red on this timestep
            IF(.NOT. INITMODE) THEN
              IF(QSTATE(IV) .EQ. QS_CFSTOPPED .OR. QSTATE(IV) .EQ. QS_CFMOVING) THEN
                CYCLE_FAILURES(IL) = CYCLE_FAILURES(IL) + 1
              ENDIF
            ENDIF
            QSTATE(IV) = QS_MOVING
            IF(SSPEED(IV) .LE. STOP_SPD) QSTATE(IV) = QS_STOPPED 
          ENDIF
            
! --- Right turners

        ELSEIF(STURNCODE(IV) .EQ. TC_RIGHT) THEN
          IF(IRC .EQ. S_RED .AND. JRC .EQ. S_GREEN) THEN !changed from red to green on this timestep
            QSTATE(IV) = QS_CFMOVING
            IF(SSPEED(IV) .LE. STOP_SPD) QSTATE(IV) = QS_CFSTOPPED
            IF(QPOS .LE. 5) THEN
              DELAY = STARTUP_MULT(DTYPE) * QFACTOR(QPOS) * SSTARTUP_TIME(IL) + HDWY_FACTOR(ITYPE) * HDWY_MULTIPLIER(DTYPE) * QDISCHARGE_HDWY(IL)
            ENDIF 
              
! --- Check for pedestrian delay.
 
            CALL CHECK_PED_DELAY(IV, PDELAY)
            SDISCH_TIMER(IV) = MAX(DELAY, PDELAY)
          ELSEIF(IRC .EQ. S_GREEN .AND. JRC .EQ. S_RED) THEN !changed to red on this timestep
            IF(.NOT. INITMODE) THEN
              IF(QSTATE(IV) .EQ. QS_CFSTOPPED .OR. QSTATE(IV) .EQ. QS_CFMOVING) THEN
                CYCLE_FAILURES(IL) = CYCLE_FAILURES(IL) + 1
              ENDIF
            ENDIF
            QSTATE(IV) = QS_MOVING
            IF(SSPEED(IV) .LE. STOP_SPD) QSTATE(IV) = QS_STOPPED 
          ENDIF
            
! --- Diagonals

        ELSEIF(STURNCODE(IV) .GT. TC_RIGHT) THEN
          IF(IDC .EQ. S_RED .AND. JDC .EQ. S_GREEN) THEN !changed from red to green on this timestep
            QSTATE(IV) = QS_CFMOVING
            IF(SSPEED(IV) .LE. STOP_SPD) QSTATE(IV) = QS_CFSTOPPED
            IF(QPOS .LE. 5) THEN
              DELAY = STARTUP_MULT(DTYPE) * QFACTOR(QPOS) * SSTARTUP_TIME(IL) + HDWY_FACTOR(ITYPE) * HDWY_MULTIPLIER(DTYPE) * QDISCHARGE_HDWY(IL)
            ENDIF
            SDISCH_TIMER(IV) = DELAY
          ELSEIF(IDC .EQ. S_GREEN .AND. JDC .EQ. S_RED) THEN !changed to red on this timestep
            IF(.NOT. INITMODE) THEN
              IF(QSTATE(IV) .EQ. QS_CFSTOPPED .OR. QSTATE(IV) .EQ. QS_CFMOVING) THEN
                CYCLE_FAILURES(IL) = CYCLE_FAILURES(IL) + 1
              ENDIF
            ENDIF
            QSTATE(IV) = QS_MOVING
            IF(SSPEED(IV) .LE. STOP_SPD) QSTATE(IV) = QS_STOPPED 
          ENDIF
        ENDIF
      ENDIF
      IV = SFOLLOWER(IV)
    ENDDO
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE UPDATE_FREEWAY_STATS(IV, L1, L2, D1, D2, T1, T2)
! ----------------------------------------------------------------------
! --- Update statistics at the end of the time step.
! ----------------------------------------------------------------------
  USE SIMPARAMS
  USE FREEWAY_LINKS
  USE VEHICLE_TYPES
  USE BUS_ROUTE_DATA
  USE FREEWAY_VEHICLES
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, L1, L2
  REAL, INTENT(IN) :: D1, D2, T1, T2
  INTEGER :: I, IL, ILN, ITYP, IBR, ITURN
  REAL :: XDELAY, TRAVEL_TIME, TRAVEL_DIST, MOVETIME
! ----------------------------------------------------------------------
 
! --- L1 = first link
! --- L2 = second link
! --- D1 = distance traveled on first link
! --- D2 = distance traveled on second link
! --- T1 = time on first link
! --- T2 = time on second link
 
  DO I = 1, 2
    IF(I .EQ. 1) THEN
      IL = L1
      TRAVEL_TIME = T1
      TRAVEL_DIST = D1
    ELSEIF(I .EQ. 2) THEN
      IL = L2
      TRAVEL_TIME = T2
      TRAVEL_DIST = D2
    ENDIF
    IF(IL .NE. 0) THEN
      ILN = FLANE(IV)
      IBR = FROUTEID(IV)
      ITYP = FVTYPE(IV)
      ITURN = FTURNCODE(IV)
      IF(IBR .NE. 0) THEN
        FZTIME_BUS(IL) = FZTIME_BUS(IL) + TRAVEL_TIME
        BUSR_TRAVELTIME(IBR) = BUSR_TRAVELTIME(IBR) + TRAVEL_TIME
      ENDIF
      FZDIST(IL) = FZDIST(IL) + TRAVEL_DIST
      FZTIME(IL) = FZTIME(IL) + TRAVEL_TIME
      IF(ILN .NE. 0) THEN
        FZDIST_LANE(IL, ILN) = FZDIST_LANE(IL, ILN) + TRAVEL_DIST
        FZTIME_LANE(IL, ILN) = FZTIME_LANE(IL, ILN) + TRAVEL_TIME
          
        FZOCCS(IL, ILN) = FZOCCS(IL, ILN) + TRAVEL_TIME * AVG_OCCS(ITYP)
        FZDIST_TYPE(IL, ITYP) = FZDIST_TYPE(IL, ITYP) + TRAVEL_DIST
        FZTIME_TYPE(IL, ITYP) = FZTIME_TYPE(IL, ITYP) + TRAVEL_TIME

        IF(FFREEFLOWSPEED(IL) .NE. 0) THEN
          MOVETIME = TRAVEL_DIST / FFREEFLOWSPEED(IL) 
          XDELAY = TRAVEL_TIME - MOVETIME
        ELSE
          XDELAY = 0.
        ENDIF
        FZDELAY_LANE(IL, ILN) = FZDELAY_LANE(IL, ILN) + XDELAY
      ENDIF
    ENDIF
  ENDDO
  RETURN
  END  
      
! ==================================================================================================
  SUBROUTINE UPDATE_STREET_STATS(IV, L1, L2, D1, D2, T1, T2)
! ----------------------------------------------------------------------
! --- Update statistics at the end of the time step.
! ----------------------------------------------------------------------
  USE SIMPARAMS
  USE STREET_LINKS
  USE VEHICLE_TYPES
  USE BUS_ROUTE_DATA
  USE STREET_VEHICLES
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, L1, L2
  REAL, INTENT(IN) :: D1, D2, T1, T2
  INTEGER :: I, IL, ILN, ITYP, IBR, ITURN
  REAL :: XDELAY, TRAVEL_TIME, TRAVEL_DIST, MOVETIME
! ----------------------------------------------------------------------
 
! --- L1 = first link
! --- L2 = second link
! --- D1 = distance traveled on first link
! --- D2 = distance traveled on second link
! --- T1 = time on first link
! --- T2 = time on second link
 
  DO I = 1, 2
    IF(I .EQ. 1) THEN
      IL = L1
      TRAVEL_TIME = T1
      TRAVEL_DIST = D1
    ELSEIF(I .EQ. 2) THEN
      IL = L2
      TRAVEL_TIME = T2
      TRAVEL_DIST = D2
    ENDIF
    IF(IL .NE. 0) THEN
      ILN = SLANE(IV)
      IBR = SROUTEID(IV)
      ITYP = SVTYPE(IV)
      ITURN = STURNCODE(IV)
      IF(IBR .NE. 0) THEN
        SZTIME_BUS(IL) = SZTIME_BUS(IL) + TRAVEL_TIME
        BUSR_TRAVELTIME(IBR) = BUSR_TRAVELTIME(IBR) + TRAVEL_TIME
      ENDIF
      SZDIST(IL) = SZDIST(IL) + TRAVEL_DIST
      SZTIME(IL) = SZTIME(IL) + TRAVEL_TIME
      IF(ITURN .EQ. 0) THEN
        ZDIST_LEFT(IL) = ZDIST_LEFT(IL) + TRAVEL_DIST
        ZTIME_LEFT(IL) = ZTIME_LEFT(IL) + TRAVEL_TIME
      ELSEIF(ITURN .EQ. 2) THEN
        ZDIST_RIGHT(IL) = ZDIST_RIGHT(IL) + TRAVEL_DIST
        ZTIME_RIGHT(IL) = ZTIME_RIGHT(IL) + TRAVEL_TIME
      ELSEIF(ITURN .GT. 2) THEN
        ZDIST_DIAG(IL) = ZDIST_DIAG(IL) + TRAVEL_DIST
        ZTIME_DIAG(IL) = ZTIME_DIAG(IL) + TRAVEL_TIME
      ENDIF
      IF(ILN .NE. 0) THEN
        SZDIST_LANE(IL, ILN) = SZDIST_LANE(IL, ILN) + TRAVEL_DIST
        SZTIME_LANE(IL, ILN) = SZTIME_LANE(IL, ILN) + TRAVEL_TIME
           
        SZOCCS(IL, ILN) = SZOCCS(IL, ILN) + TRAVEL_TIME * AVG_OCCS(ITYP)
        SZDIST_TYPE(IL, ITYP) = SZDIST_TYPE(IL, ITYP) + TRAVEL_DIST
        SZTIME_TYPE(IL, ITYP) = SZTIME_TYPE(IL, ITYP) + TRAVEL_TIME
 
        IF(SFREEFLOWSPEED(IL) .NE. 0) THEN
          MOVETIME = TRAVEL_DIST / SFREEFLOWSPEED(IL) 
          XDELAY = TRAVEL_TIME - MOVETIME
        ELSE
          XDELAY = 0.
        ENDIF
        SZDELAY_LANE(IL, ILN) = SZDELAY_LANE(IL, ILN) + XDELAY
      ENDIF
    ENDIF
  ENDDO
  RETURN
  END  
      
! ==================================================================================================
  SUBROUTINE APPROACH_BUS_STATION(IV, ACCEL)
! ----------------------------------------------------------------------
! --- Process a bus approaching a bus station.
! ----------------------------------------------------------------------
  USE GLOBAL_DATA
  USE STREET_VEHICLES
  USE BUS_ROUTE_DATA
  USE BUS_STATION_DATA
  USE VEHICLE_TYPES
  USE SIMPARAMS
  USE VDATA
  USE STREET_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  REAL, INTENT(INOUT) :: ACCEL
  INTEGER :: I, ISTAT, IB, NTOT = 0
  REAL :: ISTOP, DIST, AVGLEN = -1, RMAX
  LOGICAL :: FULL
! ----------------------------------------------------------------------
  IF(AVGLEN .LT. 0) THEN
    AVGLEN = 0.
    DO I = 1, 9
      IF(FLT_STREET_BUS(I) .NE. 0) THEN
        AVGLEN = AVGLEN + VTLENGTH(I)
        NTOT = NTOT + 1
      ENDIF
    ENDDO
    IF(NTOT .NE. 0) AVGLEN = AVGLEN / NTOT
  ENDIF
  ISTAT = BUSR_STATIONLIST(SROUTEID(IV), NEXT_STOP(IV))
  IB = BUS_STATION_LIST(ISTAT)%FRONT
  IF(BUS_STATION_LIST(ISTAT)%BLOCK_CODE .EQ. 0) THEN
 
! --- The station is protected, so move the bus into lane 0
! --- and stop when it is far enough downstream.
 
    ISTOP = BUS_STATION_LIST(ISTAT)%LOCATION
    DO WHILE(IB .NE. 0)
      ISTOP = ISTOP - SVLENGTH(IB) - 5
      IF(SFOLLOWER(IB) .NE. 0) THEN
        IB = SFOLLOWER(IB)
      ELSE
        EXIT
      ENDIF
    ENDDO
    DIST = ISTOP - SLOCATION(IV)
    IF(DIST .LT. 500. .AND. SSPEED(IV) .GT. 20) THEN    !!!ARBITRARY CONSTANTS
      ACCEL = -SSPEED(IV)**2 / DIST
      RMAX = MAX_DECEL(SVTYPE(IV), SSPEED(IV), SGRADE(SLINK(IV)))
      ACCEL = MAX(ACCEL, RMAX)
    ENDIF
    IF(SSPEED(IV) + ACCEL * TIMESTEP .LE. STOP_SPD .OR. DIST .LT. 100) THEN
      IF(DIST .LE. 10) THEN
        CALL CHECK_CAPACITY(ISTAT, FULL)
        IF(.NOT. FULL) THEN
          CALL ENTER_BUS_STATION(IV, ISTAT, IB, ISTOP)
        ELSE
          IF(.NOT. INITMODE) THEN
            BUS_STATION_LIST(ISTAT)%OVERFLOW_TIME = BUS_STATION_LIST(ISTAT)%OVERFLOW_TIME + TIMESTEP
          ENDIF
          QSTATE(IV) = QS_BLOCKER
          ACCEL = -SSPEED(IV) / TIMESTEP
          RMAX = MAX_DECEL(SVTYPE(IV), SSPEED(IV), SGRADE(SLINK(IV)))
          ACCEL = MAX(ACCEL, RMAX)
        ENDIF
      ELSE
        IF(SSPEED(IV) .LE. STOP_SPD) ACCEL = MAX(ACCEL, 1.0)          !!!ARBITRARY CONSTANT
        IF(IB .NE. 0) CALL STREET_CAR_FOLLOW(IV, IB, ACCEL)
      ENDIF
    ENDIF
  ELSE
    ISTOP = BUS_STATION_LIST(ISTAT)%LOCATION
    DIST = ISTOP - SLOCATION(IV)
    IF(DIST .LT. 250.) ACCEL = -SSPEED(IV)**2 / DIST    !!!ARBITRARY CONSTANT
    IF(DIST .LT. 5. .OR. (DIST .LT. 50. .AND. SSPEED(IV) .LE. STOP_SPD)) THEN
      SSPEED(IV) = 0.
      SLOCATION(IV) = ISTOP
      CALL SET_DWELL(IV, ISTAT)
      QSTATE(IV) = QS_DWELL
    ENDIF
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE CHECK_CAPACITY(ISTAT, FULL)
! ----------------------------------------------------------------------
! --- Move a bus into a bus station.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE BUS_STATION_DATA
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ISTAT
  LOGICAL, INTENT(OUT) :: FULL
  INTEGER :: IV, NV
! ----------------------------------------------------------------------
  FULL = .FALSE.
  NV = 0
  IV = BUS_STATION_LIST(ISTAT)%FRONT
  DO WHILE(IV .NE. 0)
    NV = NV + 1
    IF(NV .EQ. BUS_STATION_LIST(ISTAT)%CAPACITY) THEN
      FULL = .TRUE.
      EXIT
    ENDIF
    IV = SFOLLOWER(IV)
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE ENTER_BUS_STATION(IV, ISTAT, IB, ISTOP)
! ----------------------------------------------------------------------
! --- Move a bus into a bus station.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE BUS_STATION_DATA
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, ISTAT, IB
  REAL, INTENT(IN) :: ISTOP
  INTEGER :: IL, GOAL
! ----------------------------------------------------------------------
  IL = SLINK(IV)
  SSPEED(IV) = 0.
  SLOCATION(IV) = ISTOP
  GOAL = BUS_STATION_LIST(ISTAT)%POCKET_LANE
  CALL STREET_CHANGE_LANE(IV, GOAL, IB, 0)
  IF(BUS_STATION_LIST(ISTAT)%FRONT .EQ. 0) BUS_STATION_LIST(ISTAT)%FRONT = IV
  SLEADER(IV) = IB
  CALL SET_DWELL(IV, ISTAT)
  QSTATE(IV) = QS_DWELL
  GOAL_LANE(IV) = 0
  IF(.NOT. INITMODE) THEN
    BUS_STATION_LIST(ISTAT)%COUNT = BUS_STATION_LIST(ISTAT)%COUNT + 1
    BUS_STATION_LIST(ISTAT)%DWELL_TIME = BUS_STATION_LIST(ISTAT)%DWELL_TIME + DWELL_TIMER(IV)
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE SET_DWELL(IV, ISTAT)      
! ----------------------------------------------------------------------
! --- Set the dwell timer for a bus in station.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE BUS_STATION_DATA
  USE SEEDS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, ISTAT
  INTEGER :: ITYPE, IRAND
  REAL :: RNDNUM
! ----------------------------------------------------------------------
 
! --- Determine the specific dwell time for a bus entering a station.
 
  CALL STREET_RANDOM(SSEED, RNDNUM)
  IRAND = RNDNUM * 10 + 1
  ITYPE = BUS_STATION_LIST(ISTAT)%TYPE_CODE
  DWELL_TIMER(IV) = BUS_STATION_LIST(ISTAT)%DWELL * DWELL_MULTIPLIER(ITYPE, IRAND)     
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE DISCHARGE_FROM_STATION(IV, ACCEL, ISTAT) 
! ----------------------------------------------------------------------
! --- Determine if a bus can discharge from a station.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE BUS_ROUTE_DATA
  USE BUS_STATION_DATA
  USE VDATA
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, ISTAT
  REAL, INTENT(INOUT) :: ACCEL
  INTEGER :: GOAL, IL, I, IFB
  LOGICAL :: WSTOP
! ----------------------------------------------------------------------
 
! --- Attempt to discharge from a protected or unprotected station.
 
  IFB = SFOLLOWER(IV)
  IL = SLINK(IV)
  IF(STURNCODE(IV) .EQ. TC_RIGHT .AND. SLANE(IV) .LE. NUMBER_RIGHTPOCKETS(IL) .AND. SLANE(IV) .NE. 0) THEN
    GOAL = SLANE(IV)
  ELSE
    GOAL = FIRST_FULL_LANE(IL)
 
! --- If the bus will be turning left allow it to use the far left lane
! --- if there are no left turn pockets, and the bus will not be stopping
! --- at another station on the same link.
 
    IF(STURNCODE(IV) .EQ. TC_LEFT) THEN
 
! --- Determine if there is another stop on the same link. If not, set
! --- the lane codes.
 
      WSTOP = .FALSE.
      IF(ISTAT .NE. 0) THEN
        IF(IL .NE. BUS_STATION_LIST(ISTAT)%LINK) WSTOP = .TRUE.
      ENDIF
      IF(.NOT. WSTOP) THEN
        DO I = 1, LAST_FULL_LANE(IL)
          IF(SLANECODES(IV, I) .NE. LC_NULL) THEN
            SLANECODES(IV, I) = LC_VACATE
          ENDIF
        ENDDO
        IF(NUMBER_LEFTPOCKETS(IL) .EQ. 0) SLANECODES(IV, LAST_FULL_LANE(IL)) = LC_GOOD
        DO I = LAST_FULL_LANE(IL) + 1, TOTAL_LANES(IL)
          SLANECODES(IV, I) = LC_GOOD
        ENDDO
      ENDIF
    ELSEIF(STURNCODE(IV) .EQ. TC_RIGHT) THEN
 
! --- Determine if there is another stop on the same link. If not, set
! --- the lane codes.
 
      WSTOP = .FALSE.
      IF(ISTAT .NE. 0) THEN
        IF(IL .NE. BUS_STATION_LIST(ISTAT)%LINK) WSTOP = .TRUE.
      ENDIF
      IF(.NOT. WSTOP) THEN
        DO I = FIRST_FULL_LANE(IL), TOTAL_LANES(IL)
          IF(SLANECODES(IV, I) .NE. LC_NULL) THEN
            SLANECODES(IV, I) = LC_VACATE
          ENDIF
        ENDDO
        IF(NUMBER_RIGHTPOCKETS(IL) .EQ. 0) SLANECODES(IV, FIRST_FULL_LANE(IL)) = LC_GOOD
        DO I = 1, NUMBER_RIGHTPOCKETS(IL)
          SLANECODES(IV, I) = LC_GOOD
        ENDDO
      ENDIF
    ENDIF
    IF(SLANE(IV) .NE. GOAL) CALL TRY_STREET_LANECHANGE(IV, GOAL)
  ENDIF
  IF(SLANE(IV) .EQ. GOAL) THEN
 
! --- The vehicle may accelerate. Find its current maximum acceleration.
                                         
    IF(SFLEET(IV) .EQ. FLEET_BIKE) THEN
      ACCEL = MIN(SDESIREDSPEED(IV) - SSPEED(IV), 2.0)
    ELSE
      ACCEL = MAX_ACCEL(SVTYPE(IV), SSPEED(IV), SGRADE(SLINK(IV)))
    ENDIF
    QSTATE(IV) = QS_NOTINQ
 
! --- If the bus is in a protected station update the list of buses
! --- in the station.
 
    IF(BUS_STATION_LIST(ISTAT)%BLOCK_CODE .EQ. 0) THEN
      BUS_STATION_LIST(ISTAT)%FRONT = IFB
      IF(IFB .NE. 0) SLEADER(IFB) = 0
    ENDIF
    NEXT_STOP(IV) = NEXT_STOP(IV) + 1
    CALL FIND_TARGET_LANE_STREET(IV, GOAL)
  ELSE
    ACCEL = 0.
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE FIND_EXIT_LANES(IV, ILINK, XLINK)
! ----------------------------------------------------------------------
! --- Set lane codes for a vehicle that wants to exit.     
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE FREEWAY_NODES
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, ILINK, XLINK
  INTEGER :: NLANES, I, ILANE, CURRENT(N_FREEWAY_LANES), DOWNLINK, DOWNLANE
  LOGICAL ALLOW_LANE
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = fid(iv)
#endif
 
! --- Save the current lane codes.
 
  CURRENT(1:N_FREEWAY_LANES) = FLANECODES(IV, 1:N_FREEWAY_LANES)
 
! --- The vehicle has passed the warning sign for its exit.
         
  FLANECODES(IV, 1:N_FREEWAY_LANES) = LC_NULL
  NLANES = 0
  IF(OFFRAMP_LINK(ILINK) .EQ. DESTINATION(IV)) THEN
 
! --- The vehicle will exit here.
           
    DO I = 1, N_FREEWAY_LANES
      IF(CURRENT(I) .NE. LC_NULL) THEN
        FLANECODES(IV, I) = LC_VACATE
        IF(EXIT_LANE(ILINK, I) .NE. 0) THEN
          FLANECODES(IV, I) = CURRENT(I)
          NLANES = NLANES + 1
        ELSEIF(I .EQ. 1) THEN
          IF(EXIT_LANE(ILINK, 16) .NE. 0) THEN
            FLANECODES(IV, 1) = CURRENT(1)
            NLANES = NLANES + 1
          ENDIF
        ELSEIF(I .EQ. FNUMLANES(ILINK)) THEN
          IF(EXIT_LANE(ILINK, 11) .NE. 0) THEN
            FLANECODES(IV, 11) = CURRENT(11)
            NLANES = NLANES + 1
          ENDIF
        ENDIF
      ENDIF
    ENDDO

  ELSEIF(XLINK .NE. 0) THEN
 
! --- The vehicle is exiting at a downstream off-ramp. 
 
    IF(DUMMY_EXIT(FUSN(DESTINATION(IV)))) THEN
      DO I = 1, FNUMLANES(ILINK)
        FLANECODES(IV, I) = LC_GOOD
      ENDDO
    ELSE
      !Loop over all lanes and determine if the lanes go to the downstream exit
      DO I = 1, N_FREEWAY_LANES
        FLANECODES(IV, I) = CURRENT(I)
      ENDDO
      DO I = 1, N_FREEWAY_LANES
        ALLOW_LANE = .FALSE.
        DOWNLINK = ILINK
        DOWNLANE = I
        DO WHILE(DOWNLINK .NE. 0)
          IF(OFFRAMP_LINK(DOWNLINK) .EQ. DESTINATION(IV)) THEN
            DOWNLANE = RECEIVING_LANE(ILINK, DOWNLANE)
            IF(DOWNLANE .EQ. 0) THEN
              !The lane ends prior to reaching the exit
              IF(FLANECODES(IV, I) .NE. LC_NULL) THEN
                FLANECODES(IV, I) = LC_VACATE
              ENDIF
            ELSEIF(EXIT_LANE(DOWNLINK, DOWNLANE) .EQ. 0) THEN
              !The lane does not feed the exit link
              IF(FLANECODES(IV, I) .NE. LC_NULL) THEN
                IF((I .EQ. 1 .AND. EXIT_LANE(DOWNLINK, 16) .NE. 0) &
                  .OR. (I .EQ. FNUMLANES(DOWNLINK) .AND. EXIT_LANE(DOWNLINK, 11)) .NE. 0) THEN
                  ALLOW_LANE = .TRUE.
                ENDIF
                IF(.NOT. ALLOW_LANE) THEN
                  IF(FLANECODES(IV, I) .EQ. LC_GOOD) FLANECODES(IV, I) = LC_VACATE
                  EXIT
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          DOWNLINK = FTHRU_LINK(DOWNLINK)
        ENDDO
      ENDDO  
    ENDIF
  ENDIF
  RETURN
  END
                  
! ==================================================================================================
  SUBROUTINE AVOID_EXIT_LANES(IV, ILINK)
! ----------------------------------------------------------------------
! --- Set lane codes for a vehicle that does not want to exit.     
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE GLOBAL_DATA
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, ILINK
  INTEGER :: I, CURRENT(N_FREEWAY_LANES)
  INTEGER :: UPLINK, DOWNLINK, DOWNLANE
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = fid(iv)
#endif

! --- The vehicle must avoid any lane that does not go through.
! --- Examine each usable lane on the current link and determine if it
! --- leads only to the off-ramp, if so the vehicle should not use it.
        
  CURRENT = FLANECODES(IV, 1:N_FREEWAY_LANES)
  DO I = 1, N_FREEWAY_LANES
    IF(RECEIVING_LANE(FLINK(IV), I) .NE. 0) THEN
      DOWNLINK = FTHRU_LINK(FLINK(IV))
      UPLINK = FLINK(IV)
      DOWNLANE = I
      DO WHILE(DOWNLINK .NE. 0)
        DOWNLANE = RECEIVING_LANE(UPLINK, DOWNLANE)
        IF(DOWNLANE .NE. 0) THEN
          IF(DOWNLINK .EQ. DESTINATION(IV)) THEN
            DOWNLANE = RECEIVING_LANE(UPLINK, DOWNLANE)
            IF(DOWNLANE .NE. 0) FLANECODES(IV, I) = CURRENT(I)
            EXIT
          ENDIF
          IF(UPLINK .EQ. ILINK) EXIT
          UPLINK = DOWNLINK
          DOWNLINK = FTHRU_LINK(DOWNLINK)
        ELSE
          FLANECODES(IV, I) = LC_VACATE
          EXIT
        ENDIF
      ENDDO
    ELSE
      IF(FLANECODES(IV, I) .EQ. LC_GOOD) FLANECODES(IV, I) = LC_VACATE
    ENDIF
  ENDDO
  IF(INCIDENT_NUM(IV) .NE. 0) THEN
    DO I = 1, N_FREEWAY_LANES
      IF(CURRENT(I) .GE. 4) FLANECODES(IV, RECEIVING_LANE(ILINK, I)) = CURRENT(I)
    ENDDO
  ENDIF
  RETURN
  END


! ==================================================================================================
  SUBROUTINE CHECK_BLOCKAGE(IV, DIST, ACCEL, WSTOP)
! ----------------------------------------------------------------------
! --- Determine if there is a blockage ahead of the vehicle
! --- on the current link.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE VEHICLE_TYPES
  USE EVENTS
  USE VDATA
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, DIST
  REAL, INTENT(OUT) :: ACCEL
  LOGICAL, INTENT(OUT) :: WSTOP
! ----------------------------------------------------------------------
  WSTOP = .FALSE.
  IF(DIST .GT. -1 .AND. DIST .LT. 5) THEN
    WSTOP = .TRUE.
    ACCEL = -SSPEED(IV) / TIMESTEP
  ELSEIF(DIST .GT. 0) THEN
    ACCEL = -SSPEED(IV)**2 / (2 * DIST)
  ENDIF
  RETURN
  END
                 
! ==================================================================================================
  SUBROUTINE FIND_SEGMENT_LEADER(IV, IRL, ILN, ILD)
! ----------------------------------------------------------------------
! --- Search for a leader in a target lane on a different segment.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE FREEWAY_NODES
  USE OBJECTS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, IRL, ILN
  INTEGER, INTENT(OUT) :: ILD
  INTEGER :: ILANE, I, IVX, ISEG, IL, IOBJECT, ITYPE
  REAL :: VLOC
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = fid(iv)
#endif

  ILD = 0
  IOBJECT = NODE_OBJECT(FUSN(IRL))
 
! --- Loop through the sorted vehicle list, starting with the next
! --- vehicle downstream.
 
  DO I = SORTED_LIST_LENGTH, 1, -1
    IVX = SORTED_LIST(I)
    IF(IVX .EQ. 0 .OR. IVX .EQ. IV) CYCLE
    IF(FLINK(IVX) .EQ. 0) CYCLE
 
! --- Determine if the downstream vehicle is the putative leader.
 
    VLOC = DISTANCE_TO_SEGMENT_END(IVX) + FVLENGTH(IVX)
    IF(VLOC .GT. USN_TO_SEG_END(IRL)) CYCLE
    ILANE = ILN
    IL = IRL
    ISEG = SEGMENT(IRL)
    CALL TRACE_LANE_FWD(IV, IL, ILANE, ISEG, ITYPE, IOBJECT, VLOC)
    IF(ILANE .EQ. 0) EXIT
    IF(IL .EQ. 0) EXIT
    IF(IOBJECT .EQ. 0) EXIT
    IF(ISEGMENT(IVX) .EQ. ISEG) THEN
      IF(LINKTYPE(FLINK(IVX)) .EQ. ITYPE) THEN
        IF(FLANE(IVX) .EQ. ILANE) THEN
 
! --- If the vehicle is in the correct lane, roadway and segment, 
! --- select it as the putative leader.
 
          ILD = IVX
          EXIT
        ENDIF
      ENDIF
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE STORE_DATASTAT(IS, IV)
! ----------------------------------------------------------------------        
! --- Process vehicle crossing a datastation.
! ----------------------------------------------------------------------        
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE GLOBAL_DATA
  USE DATASTATIONS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IS, IV
  REAL :: HEADWAY
  INTEGER :: ILN, ILD
! ----------------------------------------------------------------------        

! --- Increment the count of vehicles and the total speed.

  ILN = FLANE(IV)
  DATASTATION(IS)%COUNT(ILN) = DATASTATION(IS)%COUNT(ILN) + 1
  DATASTATION(IS)%SPEED_TOTAL(ILN) = DATASTATION(IS)%SPEED_TOTAL(ILN) + FSPEED(IV)

! --- If the vehicle has a leader compute headway.

  IF(FSPEED(IV) .GT. STOP_SPD .AND. FLEADER(IV) .NE. 0) THEN
    ILD = FLEADER(IV)
    HEADWAY = (DISTANCE_TO_SEGMENT_END(IV) - DISTANCE_TO_SEGMENT_END(ILD)) / FSPEED(IV)

! --- Increment the count of vehicles used for the headway calculation
! --- and the total headway.

    DATASTATION(IS)%HDWY_COUNT(ILN) = DATASTATION(IS)%HDWY_COUNT(ILN) + 1
    DATASTATION(IS)%HDWY_TOTAL(ILN) = DATASTATION(IS)%HDWY_TOTAL(ILN) + HEADWAY
  ENDIF
  END

! ==================================================================================================
  SUBROUTINE FREEWAY_EMISSIONS(IV)
! ----------------------------------------------------------------------
! --- Compute emissions produced by a vehicle during a single time step
! --- using MOVES.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER :: IL, IOP
  REAL :: VSP, SLOPE, A1, A0, V, RCO, CONV = 0.6818181818181818
! ----------------------------------------------------------------------
  IL = FLINK(IV)
  SLOPE = FGRADE(IL)
  V = INT(FSPEED(IV)) * CONV
  A0 = FPREV_ACCEL(IV) * CONV
  A1 = FACCELERATION(IV) * CONV
 
! --- Compute Vehicle Specific Power.
       
  VSP = 4.39 * SIN(SLOPE) * V + 0.22 * V * A1 + 0.0954 * V + 0.0000272 * V ** 3
 
! --- Determine operating mode.
    
  CALL GET_OPMODE(A0, A1, V, VSP, IOP)
 
! --- Get the emission rates based on the operating mode.
   
  CALL GET_CO_EMISSION(IOP, RCO)
  FCO_TOTAL(IL) = FCO_TOTAL(IL) + RCO * TIMESTEP
  RETURN
  END

! ==================================================================================================
  SUBROUTINE STREET_EMISSIONS(IV)
! ----------------------------------------------------------------------
! --- Compute emissions produced by a vehicle during a single time step
! --- using MOVES.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER :: IL, IOP
  REAL :: VSP, SLOPE, A1, A0, V, RCO, CONV = 0.6818181818181818
! ----------------------------------------------------------------------
  IL = SLINK(IV)
  SLOPE = SGRADE(IL)
  V = INT(SSPEED(IV)) * CONV
  A0 = SPREV_ACCEL(IV) * CONV
  A1 = SACCELERATION(IV) * CONV
 
! --- Compute Vehicle Specific Power.
       
  VSP = 4.39 * SIN(SLOPE) * V + 0.22 * V * A1 + 0.0954 * V + 0.0000272 * V ** 3

! --- Determine operating mode.
   
  CALL GET_OPMODE(A0, A1, V, VSP, IOP)

! --- Get the emission rates based on the operating mode.
  
  CALL GET_CO_EMISSION(IOP, RCO)
  SCO_TOTAL(IL) = SCO_TOTAL(IL) + RCO * TIMESTEP
  RETURN
  END

! ==================================================================================================
  SUBROUTINE GET_OPMODE(A0, A1, V, VSP, IOP)
! ----------------------------------------------------------------------
! --- Determine the operating mode for use in MOVES.
! ----------------------------------------------------------------------
  IMPLICIT NONE
  REAL, INTENT(IN) :: A0, A1, V, VSP
  INTEGER, INTENT(OUT) :: IOP
! ----------------------------------------------------------------------
  IOP = -1
  IF(A1 .LE. -2.0 .OR. (A1 .LT. -1.0 .AND. A0 .LT. -1.0)) THEN
    IOP = 0
  ELSEIF(V .GT. -1.0 .AND. V .LT. 1.0) THEN
    IOP = 1
  ELSEIF(V .GE. 0.0 .AND. V .LT. 25.0) THEN
    IF(VSP .LT. 0.0) THEN
      IOP = 11
    ELSEIF(VSP .LT. 3.0) THEN
      IOP = 12
    ELSEIF(VSP .LT. 6.0) THEN
      IOP = 13
    ELSEIF(VSP .LT. 9.0) THEN
      IOP = 14
    ELSEIF(VSP .LT. 12.0) THEN
      IOP = 15
    ELSE
      IOP = 16
    ENDIF
  ELSEIF(V .GE. 25.0 .AND. V .LT. 50.0) THEN
    IF(VSP .LT. 0.0) THEN
      IOP = 21
    ELSEIF(VSP .LT. 3.0) THEN
      IOP = 22
    ELSEIF(VSP .LT. 6.0) THEN
      IOP = 23
    ELSEIF(VSP .LT. 9.0) THEN
      IOP = 24
    ELSEIF(VSP .LT. 12.0) THEN
      IOP = 25
    ELSEIF(VSP .LT. 18.0) THEN
      IOP = 27
    ELSEIF(VSP .LT. 24.0) THEN
      IOP = 28
    ELSEIF(VSP .LT. 30.0) THEN
      IOP = 29
    ELSE
      IOP = 30
    ENDIF
  ELSE
    IF(VSP .LT. 6.0) THEN
      IOP = 33
    ELSEIF(VSP .LT. 12.0) THEN
      IOP = 35
    ELSEIF(VSP .LT. 18.0) THEN
      IOP = 37
    ELSEIF(VSP .LT. 24.0) THEN
      IOP = 38
    ELSEIF(VSP .LT. 30.0) THEN
      IOP = 39
    ELSE
      IOP = 40
    ENDIF
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE GET_CO_EMISSION(IOP, RCO)
! ----------------------------------------------------------------------
! --- Use the operating mode to determine CO.
! ----------------------------------------------------------------------
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IOP
  REAL, INTENT(OUT) :: RCO
  REAL :: CO_DATA(0:40) = (/           &
   0.277778,                           &!0
   0.019444,                           &!1
   0., 0., 0., 0., 0., 0., 0., 0., 0., &
   0.083333,                           &!11
   0.277778,                           &!12
   0.555556,                           &!13
   1.666667,                           &!14
   1.111111,                           &!15
   1.666667,                           &!16
    0., 0., 0., 0.,                    &
   0.166667,                           &!21
   0.833333,                           &!22
   1.666667,                           &!23
   1.388889,                           &!24
   2.777778,                           &!25
   0.,                                 &
   8.333333,                           &!27
   83.33333,                           &!28
   222.2222,                           &!29
   472.2222,                           &!30
   0., 0.,                             &
   0.111111,                           &!33
   0.,                                 &
   0.277778,                           &!35
   0.,                                 &
   0.833333,                           &!37
   5.555556,                           &!38
   8.333333,                           &!39
   33.33333 /)                          !40
! ----------------------------------------------------------------------
  RCO = 0
  IF(IOP .GT. 0 .AND. IOP .LE. 40) THEN
    RCO = CO_DATA(IOP)
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE STORE_DETECTOR_FREEWAY(IDET, IV)
! ----------------------------------------------------------------------        
! --- Register a detector actuation.
! ----------------------------------------------------------------------        
  USE FREEWAY_DETECTORS
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE GLOBAL_DATA
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IDET, IV
  REAL :: HEADWAY, ISPEED
  INTEGER :: ILD, VDET
! ----------------------------------------------------------------------
  IF(WRITE_SUPPLEMENTAL_FILES) CALL WRITE_DETECTOR_TEXT(IDET, FID(IV))
  
  ISPEED = FSPEED(IV)
  ILD = FLEADER(IV)
  VDET = FLAST_DETID(IV)
  IF(VDET .NE. IDET) THEN

! --- Store the ID of the detector and only register once per vehicle.

    FLAST_DETID(IV) = IDET

! --- Increment the count of vehicles and the total speed.

    FDETECTOR(IDET)%COUNT = FDETECTOR(IDET)%COUNT + 1
    FDETECTOR(IDET)%SPEED_TOTAL = FDETECTOR(IDET)%SPEED_TOTAL + ISPEED

! --- Compute the time that the vehicle is over the detector.

    IF(ISPEED .NE. 0) THEN
      FDETECTOR(IDET)%ON_TIME = FDETECTOR(IDET)%ON_TIME + &
        (MAX(TIMESTEP, FDETECTOR(IDET)%ZONE_LENGTH / ISPEED))
    ELSE
      FDETECTOR(IDET)%ON_TIME = FDETECTOR(IDET)%ON_TIME + TIMESTEP
    ENDIF

! --- If the vehicle has a leader compute headway.

    IF(ISPEED .GT. STOP_SPD .AND. ILD .NE. 0) THEN
      HEADWAY = (DISTANCE_TO_SEGMENT_END(IV) - DISTANCE_TO_SEGMENT_END(ILD)) / ISPEED

! --- Increment the count of vehicles used for the headway calculation
! --- and the total headway.

      FDETECTOR(IDET)%HDWY_COUNT = FDETECTOR(IDET)%HDWY_COUNT + 1
      FDETECTOR(IDET)%HDWY_TOTAL = FDETECTOR(IDET)%HDWY_TOTAL + HEADWAY
    ENDIF
  ENDIF
  END
  
! ==================================================================================================
  SUBROUTINE STORE_DETECTOR_STREET(IDET, IV, RTIME)
! ----------------------------------------------------------------------        
! --- Register a detector actuation.
! ----------------------------------------------------------------------        
  USE STREET_DETECTORS
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE GLOBAL_DATA
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IDET, IV
  REAL, INTENT(IN) :: RTIME
  REAL :: ISPEED, HEADWAY
  INTEGER :: ILD, I, NSTEPS
  LOGICAL :: FIRST = .TRUE.
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif
  IF(WRITE_SUPPLEMENTAL_FILES) CALL WRITE_DETECTOR_TEXT(IDET, SID(IV))
  
  IF(FIRST) THEN
    FIRST = .FALSE.
    IF(TIMESTEP .GT. SCOPE_TIMESTEP) THEN
      NSTEPS = TIMESTEP / SCOPE_TIMESTEP
    ELSE
      NSTEPS = 1
    ENDIF
  ENDIF
  
  ISPEED = SSPEED(IV)
  ILD = SLEADER(IV)
  SDETECTOR(IDET)%CURRENT_STATE = 1
  
  !!Method 1
  !Estimate when the vehicle crossed the detector
  IF(NSTEPS .NE. 1) THEN
    DO I = 1, NSTEPS
      IF(FLOAT(I) / NSTEPS .GE. RTIME) THEN
        SDETECTOR(IDET)%CURRENT_STATE_TEMP(I) = 1
      ENDIF
    ENDDO
  ENDIF
          !write(msgtext, '(f5.1,a,i)') simtime, ' det: ', idet
          !call sendtextmsg(m_error)
  
  IF(SLAST_DETID(IV) .NE. IDET .AND. SDETECTOR(IDET)%LAST_VEHICLE_ID .NE. IV) THEN
! --- Store the ID of the detector and only register once per vehicle.
    SLAST_DETID(IV) = IDET
    SDETECTOR(IDET)%LAST_VEHICLE_ID = IV

! --- Increment the count of vehicles and the total speed.
    SDETECTOR(IDET)%COUNT = SDETECTOR(IDET)%COUNT + 1
    SDETECTOR(IDET)%SPEED_TOTAL = SDETECTOR(IDET)%SPEED_TOTAL + ISPEED  
    SDETECTOR(IDET)%LENGTH_TOTAL = SDETECTOR(IDET)%LENGTH_TOTAL + SVLENGTH(IV)
    
    !Save data for DCS
    SDETECTOR(IDET)%LAST_ACTUATION_TIME = SIMTIME - RTIME
    SDETECTOR(IDET)%LAST_SPEED = ISPEED
    SDETECTOR(IDET)%LAST_LENGTH = SVLENGTH(IV)
  ENDIF    
! --- Passage detectors will provide vehicle counts and average speeds.
! --- Presence detectors will provide vehicle counts and average speeds, plus cumulative activation time (on time), and occupancy.      
    
  IF(SDETECTOR(IDET)%OPERATION_CODE .EQ. 0) THEN
! --- Detector is operating in presence mode.
! --- Compute the time that the vehicle is over the detector.
    SDETECTOR(IDET)%ON_TIME = SDETECTOR(IDET)%ON_TIME + TIMESTEP   
  ENDIF
! --- If the vehicle has a leader compute headway.

  IF(ISPEED .GT. STOP_SPD .AND. ILD .NE. 0) THEN
    IF(SLINK(ILD) .EQ. SDETECTOR(IDET)%LINK) THEN
      HEADWAY = (SLOCATION(ILD) - SLOCATION(IV)) / ISPEED

! --- Increment the count of vehicles used for the headway calculation
! --- and the total headway.

      SDETECTOR(IDET)%HDWY_COUNT = SDETECTOR(IDET)%HDWY_COUNT + 1
      SDETECTOR(IDET)%HDWY_TOTAL = SDETECTOR(IDET)%HDWY_TOTAL + HEADWAY
    ENDIF
  ENDIF
  RETURN
  END 

! ==================================================================================================
  SUBROUTINE FIND_FREEWAY_LEADER(IV, IPX, ILN, ILD)
! ----------------------------------------------------------------------
! --- Search for a leader in a target lane.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, IPX, ILN
  INTEGER, INTENT(OUT) :: ILD
  INTEGER :: ILANE, IOBJECT, I, IVX, ISEG, IL, ITYPE
  REAL :: VLOC
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = fid(iv)
#endif

  ILD = 0
 
! --- Loop through the sorted vehicle list, starting with the next
! --- vehicle downstream.
 
  DO I = IPX, 1, -1
    IVX = SORTED_LIST(I)
    IF(IVX .EQ. 0 .OR. IVX .EQ. IV) CYCLE
    IF(FLINK(IVX) .EQ. 0) CYCLE
    IF(ISEGMENT(IVX) .NE. ISEGMENT(IV)) CYCLE
 
! --- Determine if the downstream vehicle is the putative leader.
 
    IF(DISTANCE_TO_SEGMENT_END(IVX) .GT. DISTANCE_TO_SEGMENT_END(IV)) CYCLE
    ILANE = ILN
    IL = FLINK(IV)
    ISEG = ISEGMENT(IV)
    IOBJECT = NEXT_OBJECT(IV)
    VLOC = DISTANCE_TO_SEGMENT_END(IVX) + FVLENGTH(IVX)
    CALL TRACE_LANE_FWD(IV, IL, ILANE, ISEG, ITYPE, IOBJECT, VLOC)
    IF(ILANE .EQ. 0) EXIT
    IF(IL .EQ. 0) EXIT
    IF(ISEGMENT(IVX) .EQ. ISEG) THEN
      IF(LINKTYPE(FLINK(IVX)) .EQ. ITYPE) THEN
        IF(FLANE(IVX) .EQ. ILANE) THEN
 
! --- If the vehicle is in the correct lane, roadway and
! --- segment, select it as the putative leader.
 
          ILD = IVX
          EXIT
        ENDIF
      ENDIF
    ENDIF
    IF(IOBJECT .EQ. 0) EXIT
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE FIND_FREEWAY_FOLLOWER(IV, ILN, IFL)
! ----------------------------------------------------------------------
! --- Search for a follower in a target lane.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, ILN
  INTEGER, INTENT(OUT) :: IFL
  INTEGER :: ILANE, IOBJ, I, IVX, ISEG, IL
  REAL :: VLOC
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = fid(iv)
#endif

  IOBJ = NEXT_OBJECT(IV)
  IFL = 0
 
! --- Loop through the sorted vehicle list, starting with the next vehicle upstream.
 
  DO I = SORT_POSITION(IV) + 1, SORTED_LIST_LENGTH
    IVX = SORTED_LIST(I)
    IF(IVX .EQ. 0) CYCLE
    IF(FLINK(IVX) .EQ. 0) CYCLE
 
! --- Determine if the upstream vehicle is the putative follower.
 
    ILANE = ILN
    IL = FLINK(IV)
    ISEG = ISEGMENT(IV)
    VLOC = DISTANCE_TO_SEGMENT_END(IVX) + FVLENGTH(IVX)
    CALL TRACE_LANE_BACK(IL, ILANE, ISEG, IOBJ, VLOC)
    IF(ILANE .EQ. 0) EXIT
    IF(IL .EQ. 0) EXIT
    IF(ISEGMENT(IVX) .EQ. ISEG) THEN
      IF(LINKTYPE(FLINK(IVX)) .EQ. LINKTYPE(IL)) THEN
        IF(FLANE(IVX) .EQ. ILANE) THEN
 
! --- If the vehicle is in the correct lane, roadway and
! --- segment, select it as the putative follower.
 
          IFL = IVX
          EXIT
        ENDIF
      ENDIF
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE FIND_STREET_FOLLOWER(IV, ILN, IFL)
! ----------------------------------------------------------------------
! --- Search for a follower in a target lane.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, ILN
  INTEGER, INTENT(OUT) :: IFL
  INTEGER :: IVX
  !INTEGER :: LOOP_COUNTER
! ----------------------------------------------------------------------
 
! --- Loop through the vehicles in the target lane.
 
  IFL = 0    
  IVX = FIRST_VEHICLE(SLINK(IV), ILN)
  !LOOP_COUNTER = 0
  DO WHILE(IVX .NE. 0)

    !LOOP_COUNTER = LOOP_COUNTER + 1
    !IF(LOOP_COUNTER .GT. 1000) THEN
    !  WRITE(MSGTEXT, '(A)') 'FIND_STREET_FOLLOWER: INFINITE LOOP'
    !  CALL SENDTEXTMSG(M_ERROR)
    !  EXIT
    !ENDIF            
!!!!! this indicates problems in the leader/follower chain          
    if(slink(ivx) .ne. slink(iv)) then
      ifl=0
      exit
    endif
           
    IF(SLOCATION(IVX) .LT. SLOCATION(IV)) THEN
      IFL = IVX
      RETURN
    ENDIF
    IVX = SFOLLOWER(IVX)
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE FIND_STREET_LEADER(IV, IL, ILN, ILD)
! ----------------------------------------------------------------------
! --- Locate the lead vehicle in the target lane.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, IL, ILN
  INTEGER, INTENT(OUT) :: ILD
  INTEGER :: IVX !, LOOP_COUNTER
! ----------------------------------------------------------------------
  ILD = 0 
  IVX = SLAST_VEHICLE(IL, ILN)
  !LOOP_COUNTER = 0
  DO WHILE(IVX .NE. 0)
    !LOOP_COUNTER = LOOP_COUNTER + 1
    !IF(LOOP_COUNTER .GT. 1000) THEN
    !  WRITE(MSGTEXT, '(A)') 'FIND_STREET_LEADER: INFINITE LOOP'
    !  CALL SENDTEXTMSG(M_ERROR)
    !  EXIT
    !ENDIF
    IF(SLOCATION(IVX) .GE. SLOCATION(IV)) THEN
      ILD = IVX
      RETURN
    ENDIF
    IVX = SLEADER(IVX)
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE TRANSLATE_STREET_LANECODES(IV, NLINK)
! ----------------------------------------------------------------------
! --- Translate lane codes on the current link to lane codes on the
! --- receiving link.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE VEHICLE_TYPES
  USE SIMPARAMS
  USE BUS_ROUTE_DATA
  USE BUS_STATION_DATA
  USE GLOBAL_DATA
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, NLINK
  INTEGER :: I, ICODE, ITURN
  INTEGER :: CURRENT(N_STREET_LANES)
  INTEGER :: IRLANE, ISTAT
  LOGICAL :: WSTOP, WDONE
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif
 
! --- The subject vehicle is discharging from link ILINK to link NLINK.
! --- Define the lanecodes for the vehicle as it enters the new link.
 
! --- Save the current lane codes.
 
  CURRENT(1:N_STREET_LANES) = SLANECODES(IV, 1:N_STREET_LANES)
 
! --- The vehicle is on a street. Let the vehicle use any lane that
! --- services its turn movement.
          
  SLANECODES(IV, 1:N_STREET_LANES) = LC_NULL
  IF(SFLEET(IV) .EQ. FLEET_BIKE) THEN
    SLANECODES(IV, 1) = LC_GOOD
    RETURN
  ENDIF
  ITURN = STURNCODE(IV)
  WDONE = .FALSE.
  DO I = 1, N_STREET_LANES
    IF(CHANNELIZATION(NLINK, I) .LT. 0) CYCLE
    IF(RTW_EXIT_POINT(NLINK) .NE. 0 .AND. I .EQ. 1) THEN
      SLANECODES(IV, I) = LC_VACATE
      CYCLE
    ENDIF
    SLANECODES(IV, I) = LC_GOOD
    ICODE = CHANNELIZATION(NLINK, I)
 
! --- Apply lane restrictions.
 
    IF(SXCLUDE_TYPE(SLINK(IV), I, SVTYPE(IV))) THEN
      SLANECODES(IV, I) = LC_EXCLUDED
      CYCLE
    ENDIF
    IF(ICODE .EQ. 0 .OR. ICODE .EQ. 9) THEN

      IF(ITURN .EQ. TC_THRU) THEN
        IF(STHRU_LINK(NLINK) .GT. 0) THEN
          IF(NODE_TYPE(SDSN(STHRU_LINK(NLINK))) .NE. NT_EXTERN) THEN
            IRLANE = I + STHRU_ALIGNMENT_LANE(NLINK) - SALIGNMENT_LANE(NLINK)
            IF(IRLANE .EQ. 0 .OR. IRLANE .GT. LAST_FULL_LANE(STHRU_LINK(NLINK))) SLANECODES(IV, I) = LC_VACATE 
          ENDIF
        ENDIF
      ELSE
 
        IF(ICODE .EQ. 0) THEN
          
! --- 0 => Unchannelized.
! --- Only the leftmost full lane may turn left and lane 1 may turn right,
! --- and only if there is no turn pocket for that direction.
 
          IF(ITURN .EQ. TC_LEFT) THEN
            IF(I .EQ. LAST_FULL_LANE(NLINK)) THEN
              IF(NUMBER_LEFTPOCKETS(NLINK) .GT. 0) SLANECODES(IV, I) = LC_VACATE
            ELSE                
              SLANECODES(IV, I) = LC_VACATE
            ENDIF
          ELSEIF(ITURN .EQ. TC_RIGHT) THEN
            IF(I .EQ. FIRST_FULL_LANE(NLINK)) THEN
              IF(NUMBER_RIGHTPOCKETS(NLINK) .GT. 0) SLANECODES(IV, I) = LC_VACATE
            ELSE
              SLANECODES(IV, I) = LC_VACATE
            ENDIF
          ENDIF
          
        ELSEIF(ICODE .EQ. 9) THEN
          
! --- 9 => All movements permitted by geometry and adjacent lane channelization.              

          IF(ITURN .EQ. TC_LEFT) THEN
 
! --- Do not allow the vehicle to use the lane unless it is the leftmost through lane.
                 
            IF(I .LT. LAST_FULL_LANE(SLINK(IV))) SLANECODES(IV, I) = LC_VACATE
          ELSEIF(ITURN .EQ. TC_RIGHT) THEN
 
! --- Do not allow the vehicle to use the lane unless it is the rightmost through lane.
                 
            IF(I .GT. FIRST_FULL_LANE(SLINK(IV))) SLANECODES(IV, I) = LC_VACATE
          ENDIF
        ENDIF
 
        IF(ITURN .EQ. TC_LDIAG) THEN
 
! --- Left diagonal movements can be performed from the leftmost unchannelized lane.
! --- The current lane has been set to 0. Set the lane to the right to 1.
 
          IF(I .GT. 1) THEN
            IF(CHANNELIZATION(NLINK, I - 1) .EQ. 0) SLANECODES(IV, I - 1) = LC_VACATE
          ENDIF
 
        ELSEIF(ITURN .EQ. TC_RDIAG) THEN
 
! --- Right diagonal movements can be performed from the rightmost unchannelized lane.
! --- If a lane has been used for right diagonal set other lanes to 1.
 
          IF(WDONE) THEN
            IF(CHANNELIZATION(NLINK, I) .EQ. 0) SLANECODES(IV, I) = LC_VACATE
          ELSE
            WDONE = .TRUE.
          ENDIF
        ENDIF
      ENDIF
 
    ELSEIF(ICODE .EQ. 1) THEN

! --- Left only.              

      IF(ITURN .NE. TC_LEFT) SLANECODES(IV, I) = LC_VACATE
 
    ELSEIF(ICODE .EQ. 2) THEN

! --- Buses only.              

      IF(SFLEET(IV) .NE. FLEET_BUS) SLANECODES(IV, I) = LC_VACATE
 
    ELSEIF(ICODE .EQ. 3) THEN

! --- Closed.              

      SLANECODES(IV, I) = LC_VACATE
 
    ELSEIF(ICODE .EQ. 4) THEN

! --- Right only.              

      IF(ITURN .NE. TC_RIGHT) SLANECODES(IV, I) = LC_VACATE
 
    ELSEIF(ICODE .EQ. 5) THEN

! --- Carpool only.              

      IF(SFLEET(IV) .NE. FLEET_CARPOOL) SLANECODES(IV, I) = LC_VACATE
 
    ELSEIF(ICODE .EQ. 6) THEN
! --- Carpool and buses only.              
      IF(SFLEET(IV) .NE. FLEET_CARPOOL .AND. SFLEET(IV) .NE. FLEET_BUS) SLANECODES(IV, I) = LC_VACATE
 
    ELSEIF(ICODE .EQ. 7) THEN

! --- No left or left diagonal.              

      IF(ITURN .EQ. TC_LEFT .OR. ITURN .EQ. TC_LDIAG) SLANECODES(IV, I) = LC_VACATE
 
    ELSEIF(ICODE .EQ. 8) THEN

! --- No right or right diagonal.              

      IF(ITURN .EQ. TC_RIGHT .OR. ITURN .EQ. TC_RDIAG) SLANECODES(IV, I) = LC_VACATE
 
    ELSEIF(ICODE .EQ. 10) THEN

! --- Diagonal only.              

      IF(ITURN .LE. TC_RIGHT) SLANECODES(IV, I) = LC_VACATE
 
    ELSEIF(ICODE .EQ. 11) THEN

! --- Through only.              

      IF(ITURN .NE. TC_THRU) SLANECODES(IV, I) = LC_VACATE
    ENDIF
  ENDDO
         
  IF(SFLEET(IV) .EQ. FLEET_BUS) THEN
 
! --- The vehicle is a bus. If it needs to stop at a station on
! --- the current link keep it in lane 1.
          
    WSTOP = .FALSE.
    ISTAT = BUSR_STATIONLIST(SROUTEID(IV), NEXT_STOP(IV))
    IF(ISTAT .NE. 0) THEN
      IF(BUS_STATION_LIST(ISTAT)%LINK .EQ. NLINK) WSTOP = .TRUE.
    ENDIF
    IF(WSTOP) THEN
      DO I = 1, TOTAL_LANES(NLINK)
        IF(SLANECODES(IV, I) .NE. LC_NULL) SLANECODES(IV, I) = LC_VACATE
      ENDDO
      SLANECODES(IV, FIRST_FULL_LANE(NLINK)) = LC_GOOD
    ENDIF
  ENDIF
  
! --- Apply lane closures.
 
  DO I = 1, TOTAL_LANES(SLINK(IV))
    IF(SLANECODES(IV, I) .EQ. LC_GOOD) THEN
      IF(SLANE_CLOSED(SLINK(IV), I)) THEN
        SLANECODES(IV, I) = LC_NULL
      ENDIF
    ENDIF
  ENDDO 
  
  !!!This needs work to determine if the other lane that leads to the turn is better than the current lane.
  !!Determine if the vehicle should use a different lane that leads to the turn lane.
  !DESIRED_LANE(IV) = 0
  !IF(STHRU_LINK(NLINK) .EQ. TURN_LINK(IV) .AND. SNUMLANES(NLINK) .GT. 1) THEN
  !  IRL = STHRU_LINK(NLINK)
  !  IF(TURN_CODE(IV) .EQ. TC_RIGHT) THEN
  !    DO I = SLANE(IV), 1, -1
  !      IF(LANE_GOES_RIGHT(NLINK, I)) THEN
  !        DESIRED_LANE(IV) = I
  !        EXIT
  !      ENDIF
  !    ENDDO
  !  ELSEIF(TURN_CODE(IV)  .EQ. TC_LEFT) THEN
  !    DO I = SLANE(IV), SNUMLANES(NLINK)
  !      IF(LANE_GOES_LEFT(NLINK, I)) THEN
  !        DESIRED_LANE(IV) = I
  !        EXIT
  !      ENDIF
  !    ENDDO
  !  ENDIF
  !ENDIF
  
  RETURN
  END

! ==================================================================================================
  SUBROUTINE TRANSLATE_FREEWAY_LANECODES(IV, ILINK, NLINK)
! ----------------------------------------------------------------------
! --- Translate lane codes on the current link to lane codes on the
! --- receiving link.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE VEHICLE_TYPES
  USE SIMPARAMS
  USE BUS_ROUTE_DATA
  USE BUS_STATION_DATA
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER, INTENT(IN) :: ILINK, NLINK
  INTEGER :: I, NLANES, J
  INTEGER :: CURRENT(N_FREEWAY_LANES)
  INTEGER :: IRLANE
  LOGICAL :: WBAD
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = fid(iv)
#endif
 
! --- The subject vehicle is discharging from link ILINK to link NLINK.
! --- Define the lanecodes for the vehicle as it enters the new link.
 
! --- Save the current lane codes.
 
  CURRENT(1:N_FREEWAY_LANES) = FLANECODES(IV, 1:N_FREEWAY_LANES)
 
! --- The vehicle is on a freeway link.
         
  FLANECODES(IV, 1:N_FREEWAY_LANES) = LC_NULL
  IF(FTURNCODE(IV) .NE. TC_LEFT .AND. FTURNCODE(IV) .NE. TC_RIGHT) THEN
 
! --- The lanes that the vehicle was allowed to use can still be used
! --- if they have a receiving lane on the mainline receiving link.
        
    DO I = 1, N_FREEWAY_LANES
      IF(RECEIVING_LANE(ILINK, I) .NE. 0) THEN
        FLANECODES(IV, RECEIVING_LANE(ILINK, I)) = CURRENT(I) 
      ENDIF
    ENDDO
    IF(MAIN_MERGE_LINK(ILINK)) THEN
      DO I = 1, N_FREEWAY_LANES
        IF(FLANECODES(IV, I) .NE. LC_GOOD) THEN
          IF(RECEIVING_LANE(NLINK, I) .NE. 0) THEN
            FLANECODES(IV, I) = LC_GOOD
          ENDIF
        ENDIF
      ENDDO
    ENDIF

! --- If the vehicle is coming from an on-ramp onto a mainline link
! --- allow it to use all full lanes that have a downstream receiver.
 
    IF((LINKTYPE(ILINK) .GT. 0 .OR. RAMP_MERGE_LINK(ILINK)) .AND. LINKTYPE(NLINK) .EQ. 0) THEN
      DO I = 1, FNUMLANES(NLINK)
        FLANECODES(IV, I) = LC_VACATE
        IF(RECEIVING_LANE(NLINK, I) .NE. 0) THEN
          FLANECODES(IV, I) = LC_GOOD
        ENDIF
        IF(OFFRAMP_LINK(NLINK) .NE. 0) THEN
          IF(EXIT_LANE(NLINK, I) .NE. 0) THEN
            FLANECODES(IV, I) = LC_GOOD
          ENDIF
        ENDIF
        IF(OFFRAMP_LINK(NLINK) .NE. 0) THEN
          IF(DIVERGE_LINK(OFFRAMP_LINK(NLINK))) THEN
            IF(EXIT_LANE(NLINK, I) .NE. 0) THEN
              FLANECODES(IV, I) = LC_GOOD
            ENDIF
          ENDIF
        ENDIF
      ENDDO
 
! --- If the lane is an auxiliary lane, use it if it leads to the mainline link.
 
      DO J = 1, N_AUXLANES
        IF(AUX_LANE_ID(NLINK, J) .EQ. 0) EXIT !no more auxiliary lanes on this link
        IF(AUX_LANE_CODE(NLINK, J) .NE. AUX_DECEL) THEN
          IF(RECEIVING_LANE(NLINK, I) .NE. 0) THEN
            FLANECODES(IV, AUX_LANE_ID(NLINK, J)) = LC_GOOD
          ENDIF
        ENDIF
      ENDDO
    ENDIF
  ELSE
 
! --- The lanes that the vehicle was allowed to use can still be used
! --- if they have a receiving lane on the off-ramp receiving link.
        
    IF(OFFRAMP_LINK(NLINK) .EQ. DESTINATION(IV)) THEN
      NLANES = 0
      DO I = 1, N_FREEWAY_LANES
        IF(CURRENT(I) .NE. LC_NULL) THEN
          IF(RECEIVING_LANE(ILINK, I) .NE. 0) THEN
            FLANECODES(IV, RECEIVING_LANE(ILINK, I)) = LC_VACATE
            IF(EXIT_LANE(NLINK, RECEIVING_LANE(ILINK, I)) .NE. 0) THEN
              FLANECODES(IV, RECEIVING_LANE(ILINK, I)) = LC_GOOD
              NLANES = NLANES + 1
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      
! --- or the leftmost lane for a left side exit and the rightmost lane for a right side exit.
      IF(NLANES .EQ. 0) THEN
        IF(OFFRAMP_SENDING_LANE(NLINK) .GE. 16) THEN
          DO I = 1, FNUMLANES(NLINK)
            IF(CURRENT(I) .EQ. LC_GOOD) THEN
              FLANECODES(IV, I) = LC_GOOD
              EXIT
            ENDIF
          ENDDO
        ELSE
          DO I = FNUMLANES(NLINK), 1, -1
            IF(CURRENT(I) .EQ. LC_GOOD) THEN
              FLANECODES(IV, I) = LC_GOOD
              EXIT
            ENDIF
          ENDDO
        ENDIF
      ENDIF
      
    ELSE
 
! --- The destination is farther downstream.
 
      DO I = 1, N_FREEWAY_LANES
        IF(CURRENT(I) .NE. 2) THEN
          IF(FTURNCODE(IV) .EQ. TC_THRU) THEN
            IRLANE = RECEIVING_LANE(ILINK, I)
          ELSE
            IRLANE = EXIT_LANE(ILINK, I)
          ENDIF
          IF(IRLANE .NE. 0) THEN
            FLANECODES(IV, IRLANE) = LC_VACATE
            IF(RECEIVING_LANE(NLINK, IRLANE) .NE. 0) THEN
              FLANECODES(IV, IRLANE) = LC_GOOD
            ENDIF
          ENDIF
        ENDIF
      ENDDO
    ENDIF
  ENDIF
 
! --- Apply lane restrictions.
 
  DO I = 1, N_FREEWAY_LANES
    IF(FLANECODES(IV, I) .EQ. LC_GOOD) THEN
      IF(TRUCK_CODE(FLINK(IV)) .GT. TRK_BIASED) THEN
        CALL CHECK_TRUCK_LANES(IV, FLINK(IV), I, WBAD)
        IF(WBAD) FLANECODES(IV, I) = LC_EXCLUDED
      ENDIF
      IF(FXCLUDE_TYPE(FLINK(IV), I, FVTYPE(IV))) FLANECODES(IV, I) = LC_EXCLUDED
    ENDIF
  ENDDO
  
! --- If the vehicle is aware of an incident transfer the incident
! --- code to the receiving lane.
 
  IF(INCIDENT_NUM(IV) .NE. 0 .AND. FTURNCODE(IV) .EQ. TC_THRU) THEN
    DO I = 1, N_FREEWAY_LANES
      IF(RECEIVING_LANE(NLINK, I) .NE. 0) THEN
        IF(CURRENT(I) .GE. 4) FLANECODES(IV, RECEIVING_LANE(NLINK, I)) = CURRENT(I)
      ENDIF
    ENDDO
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE SET_DESIREDSPEED_FREEWAY(IV)
! ----------------------------------------------------------------------
! --- Set the vehicle's desired speed.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE GLOBAL_DATA
  USE FREEWAY_LINKS
  USE DRIVERS
  USE VDATA
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER :: IL, ILANE, RIGHTLANE, I, NL
  REAL :: FFSPD
! ----------------------------------------------------------------------
 
! --- Multiply the link mean freeflow speed by the factor for the
! --- driver type.
 
  IL = FLINK(IV)
  ILANE = FLANE(IV)
  IF(NODE_TYPE(FUSN(IL)) .EQ. NT_EXTERN) THEN
    FFSPD = FFREEFLOWSPEED(FTHRU_LINK(IL))
  ELSE
    FFSPD = FFREEFLOWSPEED(IL)
  ENDIF
  FDESIREDSPEED(IV) = FFSPD * FFSPEED_ADJ(FDRIVERTYPE(IV), I_FREEWAY)
  FDESIREDSPEED(IV) = FDESIREDSPEED(IV) + FEV_OVRSPD(IV)
  
  FDESIREDSPEED(IV) = MIN(FDESIREDSPEED(IV), MAX_SPEED(FVTYPE(IV)) * MILES2FEET)
 
  IF(NODE_TYPE(FUSN(IL)) .NE. NT_EXTERN) THEN
 
! ---Determine if shoulder width affects desired speed.
 
    IF(FSHOULDER_WIDTH(IL) .LT. 6) THEN
      IF(ILANE .EQ. 1 .OR. ILANE .GT. 15) THEN
        RIGHTLANE = FREEWAY_LANE_TO_RIGHT(ILANE)
        IF(FLANECODES(IV, RIGHTLANE) .EQ. LC_NULL) THEN
 
! --- The vehicle is in the far right lane.
 
          NL = 0
          DO I = 1, N_FREEWAY_LANES
            IF(FLANECODES(IV, I) .NE. LC_NULL) NL = NL + 1
          ENDDO
          IF(NL .LT. 6) THEN
            FDESIREDSPEED(IV) = FDESIREDSPEED(IV) - (6 - FSHOULDER_WIDTH(IL)) * (0.2 * (6 - NL) -0.2)
          ENDIF
        ENDIF
      ENDIF
    ENDIF
 
! ---Determine if lane width affects desired speed.
 
    IF(ILANE .NE. 0) THEN
      IF(FLANE_WIDTH(IL, ILANE) .LT. 11) THEN
        FDESIREDSPEED(IV) = FDESIREDSPEED(IV) - 6.6
      ELSEIF(FLANE_WIDTH(IL, ILANE) .LT. 12) THEN
        FDESIREDSPEED(IV) = FDESIREDSPEED(IV) - 1.9
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END
      

! ==================================================================================================
  SUBROUTINE SET_DESIREDSPEED_STREET(IV)
! ----------------------------------------------------------------------
! --- Set the vehicle's desired speed.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE GLOBAL_DATA
  USE STREET_LINKS
  USE DRIVERS
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER :: IL, ILANE
  REAL :: FFSPD
! ----------------------------------------------------------------------
 
! --- Multiply the link mean freeflow speed by the factor for the
! --- driver type.
 
  IL = SLINK(IV)
  ILANE = SLANE(IV)
  IF(NODE_TYPE(SUSN(IL)) .EQ. NT_EXTERN) THEN
    FFSPD = SFREEFLOWSPEED(STHRU_LINK(IL))
  ELSE
    FFSPD = SFREEFLOWSPEED(IL)
  ENDIF
  SDESIREDSPEED(IV) = FFSPD * FFSPEED_ADJ(SDRIVERTYPE(IV), I_STREET)
  SDESIREDSPEED(IV) = SDESIREDSPEED(IV) + SEV_OVRSPD(IV)
  SDESIREDSPEED(IV) = MIN(SDESIREDSPEED(IV), 110.)
  IF(SFLEET(IV) .EQ. FLEET_BIKE) SDESIREDSPEED(IV) = 15.
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE SET_NEXT_OBJECT(IV)
! ----------------------------------------------------------------------
! --- Find the object immediately ahead of the vehicle.
! ----------------------------------------------------------------------
  USE OBJECTS
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE SEGMENTS
  USE TEXT
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER :: IOBJ, IL, VLOC, ISEG
! ----------------------------------------------------------------------
 
! --- Find the first object the vehicle will encounter on the current
! --- segment and roadway.
 
  IL = FLINK(IV)
  VLOC = DISTANCE_TO_SEGMENT_END(IV) + FVLENGTH(IV)
  NEXT_OBJECT(IV) = 0
  ISEG = ISEGMENT(IV)
  IF(ISEG .EQ. 0) THEN
    WRITE(MSGTEXT, '(A)') 'INTERNAL ERROR - VEHICLE NOT ASSIGNED TO ANY SEGMENT'
    CALL SENDTEXTMSG(M_ERROR)
    EXITFLG = 1
    RETURN
  ENDIF
  DO IOBJ = LAST_OBJECT(ISEG), FIRST_OBJECT(ISEG), -1
    IF(LINKTYPE(OBJECT_LIST(IOBJ)%LINK) .EQ. LINKTYPE(IL)) THEN
      IF(VLOC .GE. OBJECT_LIST(IOBJ)%LOCATION) THEN
        NEXT_OBJECT(IV) = IOBJ
        EXIT
      ENDIF
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE TRACE_LANE_TO_DROP(IV, ILANE, IOBJECT, ZLOC)
! ----------------------------------------------------------------------
! --- Loop through the downstream objects and identify the lane number
! --- as the effect of the object is applied.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE OBJECTS
  USE ADD_DROP_ALIGNMENTS
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, IOBJECT, ZLOC
  INTEGER, INTENT(OUT) :: ILANE
  INTEGER :: IL, IOBJ, ILN, ISEG, LTYPE
! ----------------------------------------------------------------------
  ISEG = ISEGMENT(IV)
  IL = FLINK(IV)
  LTYPE = LINKTYPE(IL)
  DO IOBJ = NEXT_OBJECT(IV), IOBJECT, -1
    IF(OBJECT_LIST(IOBJ)%LOCATION .GE. ZLOC) THEN
      IF(OBJECT_LIST(IOBJ)%SEGMENT .EQ. ISEG) THEN
        IF(LINKTYPE(OBJECT_LIST(IOBJ)%LINK) .EQ. LTYPE) THEN
 
! --- Alignment at an internal node.
 
          IF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_ALIGN_INTNODE) THEN
            ILANE = RECEIVING_LANE(IL, ILANE)
            IF(ILANE .EQ. 0) EXIT
            IL = FTHRU_LINK(IL)
            IF(IL .EQ. 0) EXIT
            LTYPE = LINKTYPE(IL)
 
! --- Alignment at an on-ramp.
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_ALIGN_ON) THEN
            ILANE = RECEIVING_LANE(IL, ILANE)
            IF(ILANE .EQ. 0) EXIT
            IL = FTHRU_LINK(IL)
            IF(IL .EQ. 0) EXIT
            LTYPE = LINKTYPE(IL)
 
! --- Alignment at an off-ramp.
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_ALIGN_OFF) THEN
            ILN = ILANE
            ILANE = RECEIVING_LANE(IL, ILANE)
            IF(ILANE .EQ. 0) THEN
              ILANE = EXIT_LANE(IL, ILN)
              IF(ILANE .EQ. 0) EXIT
              IL = OFFRAMP_LINK(IL)
              ISEG = SEGMENT(IL)
              LTYPE = LINKTYPE(IL)
            ENDIF
 
! --- Alignment at an add or drop.
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_ALIGN_ADP) THEN
            ILANE = ADDRP_ALIGNMENT(OBJECT_LIST(IOBJ)%VALUE, ILANE)
            IF(ILANE .EQ. 0) EXIT
 
! --- Lane drop.
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_LANE_DROP) THEN
            IF(OBJECT_LIST(IOBJ)%LANE .EQ. ILANE) THEN
              EXIT
            ENDIF
 
! --- Alignment at the end of an auxiliary lane.
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_AUX_END) THEN
            IF(OBJECT_LIST(IOBJ)%LANE .EQ. ILANE) THEN
              IF(OBJECT_LIST(IOBJ)%VALUE .EQ. 1) THEN
                ILANE = 0
                EXIT
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ELSE
      EXIT
    ENDIF
  ENDDO
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE TRACE_LANE_TO_AUX_END(IV, ILANE, IOBJECT, ZLOC)
! ----------------------------------------------------------------------
! --- Loop through the downstream objects and identify the lane number
! --- as the effect of the object is applied.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE OBJECTS
  USE ADD_DROP_ALIGNMENTS
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, IOBJECT, ZLOC
  INTEGER, INTENT(OUT) :: ILANE
  INTEGER :: IL, IOBJ, ILN, ISEG, LTYPE
! ----------------------------------------------------------------------
  ISEG = ISEGMENT(IV)
  IL = FLINK(IV)
  LTYPE = LINKTYPE(IL)
  DO IOBJ = NEXT_OBJECT(IV), IOBJECT, -1
    IF(ILANE .EQ. 0) EXIT
    IF(OBJECT_LIST(IOBJ)%LOCATION .GE. ZLOC) THEN
      IF(OBJECT_LIST(IOBJ)%SEGMENT .EQ. ISEG) THEN
        IF(LINKTYPE(OBJECT_LIST(IOBJ)%LINK) .EQ. LTYPE) THEN
 
! --- Alignment at an internal node.
 
          IF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_ALIGN_INTNODE) THEN
            ILANE = RECEIVING_LANE(IL, ILANE)
            IF(ILANE .EQ. 0) EXIT
            IL = FTHRU_LINK(IL)
            IF(IL .EQ. 0) EXIT
            LTYPE = LINKTYPE(IL)
 
! --- Alignment at an on-ramp.
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_ALIGN_ON) THEN
            ILANE = RECEIVING_LANE(IL, ILANE)
            IF(ILANE .EQ. 0) EXIT
            IL = FTHRU_LINK(IL)
            IF(IL .EQ. 0) EXIT
            LTYPE = LINKTYPE(IL)
 
! --- Alignment at an off-ramp.
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_ALIGN_OFF) THEN
            ILN = ILANE
            ILANE = RECEIVING_LANE(IL, ILANE)
            IF(ILANE .EQ. 0) THEN
              ILANE = EXIT_LANE(IL, ILN)
              IF(ILANE .EQ. 0) EXIT
              IL = OFFRAMP_LINK(IL)
              ISEG = SEGMENT(IL)
              LTYPE = LINKTYPE(IL)
            ENDIF
 
! --- Alignment at an add or drop.
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_ALIGN_ADP) THEN
            ILANE = ADDRP_ALIGNMENT(OBJECT_LIST(IOBJ)%VALUE, ILANE)
            IF(ILANE .EQ. 0) EXIT
 
! --- Lane drop.
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_LANE_DROP) THEN
            IF(OBJECT_LIST(IOBJ)%LANE .EQ. ILANE) THEN
              ILANE = 0
              EXIT
            ENDIF
 
! --- Alignment at the end of an auxiliary lane.
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_AUX_END) THEN
            IF(OBJECT_LIST(IOBJ)%LANE .EQ. ILANE) THEN
              EXIT
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ELSE
      EXIT
    ENDIF
  ENDDO
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE TRACE_LANE_FWD(IV, IL, ILANE, ISEG, ITYPE, IOBJ, VLOC)
! ----------------------------------------------------------------------
! --- Loop through the downstream objects and identify the lane number
! --- as the effect of the object is applied.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE OBJECTS
  USE ADD_DROP_ALIGNMENTS
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER, INTENT(INOUT) :: IL, ILANE, ISEG, ITYPE, IOBJ
  REAL, INTENT(IN) :: VLOC
  LOGICAL :: FROM_ONRAMP
! ----------------------------------------------------------------------
  FROM_ONRAMP = .FALSE.
  ITYPE = LINKTYPE(IL)
  DO WHILE(IOBJ .GT. 0)
    IF(VLOC .LT. OBJECT_LIST(IOBJ)%LOCATION) THEN
      IF(OBJECT_LIST(IOBJ)%SEGMENT .EQ. ISEG) THEN
        IF(LINKTYPE(OBJECT_LIST(IOBJ)%LINK) .EQ. ITYPE) THEN
 
! --- End of an auxiliary lane.
 
          IF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_AUX_END) THEN
            IF(OBJECT_LIST(IOBJ)%LANE .EQ. ILANE) THEN
              IF(OBJECT_LIST(IOBJ)%VALUE .EQ. 1) THEN
                ILANE = 0
                EXIT
              ENDIF
            ENDIF
 
! --- Alignment at an add or drop.
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_ALIGN_ADP) THEN
            ILANE = ADDRP_ALIGNMENT(OBJECT_LIST(IOBJ)%VALUE, ILANE)
            IF(ILANE .EQ. 0) EXIT
 
! --- Alignment at an internal node.
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_ALIGN_INTNODE) THEN
            IF(.NOT. FROM_ONRAMP) THEN
              ILANE = RECEIVING_LANE(OBJECT_LIST(IOBJ)%LINK, ILANE)
              IF(ILANE .EQ. 0) EXIT
            ENDIF
 
! --- Alignment at an on-ramp.
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_ALIGN_ON) THEN
            ILANE = RECEIVING_LANE(OBJECT_LIST(IOBJ)%LINK, ILANE)
            IF(ILANE .EQ. 0) EXIT
            ITYPE = 0
            FROM_ONRAMP = .TRUE.
 
! --- Alignment at an off-ramp.
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_ALIGN_OFF) THEN
            IF(RECEIVING_LANE(OBJECT_LIST(IOBJ)%LINK, ILANE) .EQ. 0) THEN
              ILANE = EXIT_LANE(OBJECT_LIST(IOBJ)%LINK, ILANE)
              ISEG = SEGMENT(OFFRAMP_LINK(OBJECT_LIST(IOBJ)%LINK))
              ITYPE = LINKTYPE(OFFRAMP_LINK(OBJECT_LIST(IOBJ)%LINK))
            ENDIF
            IF(ILANE .EQ. 0) EXIT
 
! --- Exit interface node.
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_EXIT_INTERFACE) THEN
            ILANE = 0
            EXIT
          ENDIF
        ENDIF
      ENDIF
    ELSE
      EXIT
    ENDIF
 
! --- Get the next object.
  
    IOBJ = IOBJ - 1
  ENDDO
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE TRACE_LANE_BACK(IL, ILANE, ISEG, IOBJECT, VLOC)
! ----------------------------------------------------------------------
! --- Trace a lane upstream to determine the lane ID.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE OBJECTS
  USE ADD_DROP_ALIGNMENTS
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: IL, ILANE, ISEG
  INTEGER, INTENT(IN) :: IOBJECT
  REAL, INTENT(IN) :: VLOC
  INTEGER :: IOBJ, I, ILN, N, ILON, ILNX, LTYPE
! ----------------------------------------------------------------------
 
! --- Loop through the upstream objects and identify the lane 
! --- number before the effect of the object is applied.
 
  IOBJ = IOBJECT
  LTYPE = LINKTYPE(IL)
  DO WHILE(IOBJ .GT. 0 .AND. IOBJ .LE. NUMBER_OF_OBJECTS)
    IF(OBJECT_LIST(IOBJ)%SEGMENT .EQ. ISEG) THEN
      IF(LINKTYPE(OBJECT_LIST(IOBJ)%LINK) .EQ. LTYPE) THEN
        IF(VLOC .GE. OBJECT_LIST(IOBJ)%LOCATION) THEN
 
! --- Internal node.
 
          IF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_NODE_INT) THEN
            DO I = 1, N_FREEWAY_LINKS
              IF(FDSN(I) .EQ. FUSN(IL)) THEN
                IF(LINKTYPE(I) .EQ. LTYPE) THEN
                  IL = I
                  EXIT
                ENDIF
              ENDIF
            ENDDO
            IF(IL .EQ. 0) EXIT
 
! --- Alignment at an internal node.
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_ALIGN_INTNODE) THEN
            ILN = ILANE
            ILANE = 0
            DO I = 1, N_FREEWAY_LANES
              IF(RECEIVING_LANE(OBJECT_LIST(IOBJ)%LINK, I) .EQ. ILN) THEN
                ILANE = I
                EXIT
              ENDIF
            ENDDO
            IF(ILANE .EQ. 0) EXIT
 
! --- Alignment at an on-ramp.
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_ALIGN_ON) THEN
            ILN = ILANE
            ILANE = 0
            DO I = 1, N_FREEWAY_LANES
              IF(RECEIVING_LANE(OBJECT_LIST(IOBJ)%LINK, I) .EQ. ILN) THEN
                ILANE = I
                EXIT
              ENDIF
            ENDDO
            IF(ILANE .EQ. 0) EXIT
            LTYPE = LINKTYPE(OBJECT_LIST(IOBJ)%LINK)
 
! --- Alignment at an off-ramp.
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_ALIGN_OFF) THEN
            ILN = ILANE
            ILANE = 0
            IF(LTYPE .EQ. 0) THEN
              DO I = 1, N_FREEWAY_LANES
                IF(RECEIVING_LANE(OBJECT_LIST(IOBJ)%LINK, I) .EQ. ILN)THEN
                  ILANE = I
                  EXIT
                ENDIF
              ENDDO
            ELSE
              DO I = 1, N_FREEWAY_LANES
                IF(EXIT_LANE(OBJECT_LIST(IOBJ)%LINK, I) .EQ. ILN) THEN
                  ILANE = I
                  EXIT
                ENDIF
              ENDDO
            ENDIF
            IF(ILANE .EQ. 0) EXIT
            LTYPE = LINKTYPE(OBJECT_LIST(IOBJ)%LINK)
 
! --- Alignment at an add or drop.
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_ALIGN_ADP) THEN
            ILN = ILANE
            ILANE = 0
            DO I = 1, N_FREEWAY_LANES
              IF(ADDRP_ALIGNMENT(OBJECT_LIST(IOBJ)%VALUE, I) .EQ. ILN) THEN
                ILANE = I
                EXIT
              ENDIF
            ENDDO
            IF(ILANE .EQ. 0) EXIT
 
! --- Beginning of an auxiliary lane.
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_AUX_BEGIN) THEN
            IF(OBJECT_LIST(IOBJ)%LANE .EQ. ILANE) THEN
              IF(OBJECT_LIST(IOBJ)%VALUE .EQ. 2) THEN
                ILANE = 0
                EXIT
              ELSE
                N = IOBJ + 1
                DO WHILE(N .LE. NUMBER_OF_OBJECTS)
                  IF(OBJECT_LIST(N)%LOCATION .NE. OBJECT_LIST(IOBJ)%LOCATION) EXIT
                  IF(OBJECT_LIST(N)%ITYPE .EQ. M_ALIGN_ON) THEN
                    ILON = OBJECT_LIST(N)%LINK
                    IF(FTHRU_LINK(ILON) .EQ. OBJECT_LIST(IOBJ)%LINK) THEN
                      DO ILNX = 1, 3
                        IF(RECEIVING_LANE(ILON, ILNX) .EQ. ILANE) THEN
                          LTYPE = LINKTYPE(ILON)
                          EXIT
                        ENDIF
                      ENDDO
                      EXIT
                    ENDIF
                  ENDIF
                  N = N + 1
                ENDDO
              ENDIF
            ENDIF
 
! --- Interface entry.
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_ENTRY_INTERFACE) THEN
            ILANE = 0
            EXIT
 
! --- Entry node.
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_NODE_ENTRY) THEN
            ILANE = 0
            EXIT
 
! --- Lane add.
 
          ELSEIF(OBJECT_LIST(IOBJ)%ITYPE .EQ. M_LANE_ADD) THEN
            ILANE = 0
            EXIT
          ENDIF
        ELSE
          EXIT
        ENDIF
      ENDIF
    ENDIF
 
! --- Get the next object.
 
    IOBJ = IOBJ + 1
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE CHECK_TRUCK_LANES(IV, IL, ILANE, WBAD)
! ----------------------------------------------------------------------
! --- Determine if the subject vehicle can use lane ILANE.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE VEHICLE_TYPES
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, IL, ILANE
  LOGICAL, INTENT(OUT) :: WBAD
! ----------------------------------------------------------------------
  WBAD = .FALSE.
  IF(FFLEET(IV) .EQ. FLEET_TRUCK) THEN
    IF(TRUCK_DIR(IL) .EQ. DIR_RIGHT) THEN
      IF(ILANE .GT. TRUCK_LANE(IL)) WBAD = .TRUE.
    ELSE
      IF(ILANE .LT. TRUCK_LANE(IL)) WBAD = .TRUE.
    ENDIF
  ELSEIF(TRUCK_CODE(IL) .GT. TRK_RESTRICTED) THEN
 
! --- The link has lanes that are exclusive to trucks. Other
! --- vehicles may not use them.
 
    IF(TRUCK_DIR(IL) .EQ. DIR_RIGHT) THEN
      IF(ILANE .LE. TRUCK_LANE(IL)) WBAD = .TRUE.
    ELSE
      IF(ILANE .GE. TRUCK_LANE(IL)) WBAD = .TRUE.
    ENDIF
  ENDIF
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE QUEUE_DELAYS(IV)
! ----------------------------------------------------------------------
! --- Accumulate queue delays for the subject vehicle.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_DETECTORS
  USE SIMPARAMS
  USE STREET_LINKS
  USE BUS_STATION_DATA
  USE EVENTS
  USE VEHICLE_TYPES
  USE TEXT
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER :: IL
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif
  IF(SFLEET(IV) .NE. FLEET_BIKE) THEN
    IL = SLINK(IV)
    IF(QSTATE(IV) .EQ. QS_DWELL .OR. SSPEED(IV) .GT. 3 * STOP_SPD) RETURN
    IF(SACCELERATION(IV) .GE. 2 .AND. QSTATE(IV) .EQ. QS_NOTINQ) RETURN
    IF(SSPEED(IV) .GT. STOP_SPD) THEN
      QUEUE_DELAY(IL) = QUEUE_DELAY(IL) + 0.5 * TIMESTEP
      IF(STURNCODE(IV) .EQ. TC_LEFT) THEN
        QUEUE_DELAY_LEFT(IL) = QUEUE_DELAY_LEFT(IL)  + 0.5 * TIMESTEP
      ELSEIF(STURNCODE(IV) .EQ. TC_RIGHT) THEN
        QUEUE_DELAY_RIGHT(IL) = QUEUE_DELAY_RIGHT(IL) + 0.5 * TIMESTEP
      ELSEIF(STURNCODE(IV) .GT. TC_RIGHT) THEN
        QUEUE_DELAY_DIAG(IL) = QUEUE_DELAY_DIAG(IL) + 0.5 * TIMESTEP
      ENDIF
    ELSE
      QUEUE_DELAY(IL) = QUEUE_DELAY(IL) + TIMESTEP
      IF(STURNCODE(IV) .EQ. TC_LEFT) THEN
        QUEUE_DELAY_LEFT(IL) = QUEUE_DELAY_LEFT(IL) + TIMESTEP
      ELSEIF(STURNCODE(IV) .EQ. TC_RIGHT) THEN
        QUEUE_DELAY_RIGHT(IL) = QUEUE_DELAY_RIGHT(IL) + TIMESTEP
      ELSEIF(STURNCODE(IV) .GT. TC_RIGHT) THEN
        QUEUE_DELAY_DIAG(IL) = QUEUE_DELAY_DIAG(IL) + TIMESTEP
      ENDIF
      IF(QSTATE(IV) .EQ. QS_NOTINQ .AND. SSPEED(IV) .GT. 0) RETURN
      STOP_DELAY(IL) = STOP_DELAY(IL) + TIMESTEP
      IF(STURNCODE(IV) .EQ. TC_LEFT) THEN
        STOP_DELAY_LEFT(IL) = STOP_DELAY_LEFT(IL) + TIMESTEP
      ELSEIF(STURNCODE(IV) .EQ. TC_RIGHT) THEN
        STOP_DELAY_RIGHT(IL) = STOP_DELAY_RIGHT(IL) + TIMESTEP
      ELSEIF(STURNCODE(IV) .GT. TC_RIGHT) THEN
        STOP_DELAY_DIAG(IL) = STOP_DELAY_DIAG(IL) + TIMESTEP
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE CHECK_CENTROID(IV, IL, ACCEL)
! ----------------------------------------------------------------------
! --- See if the vehicle should exit at a sink.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE SIMPARAMS
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, IL
  REAL, INTENT(INOUT) :: ACCEL
  INTEGER :: ILANE, ILN
  REAL :: T1, Z1,LOC1, LOC2, DECEL
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif
  LOC1 = SLOCATION(IV)
  ILANE = SLANE(IV)
  IF(CENTROID_NV(IL) .LT. 0 .AND. ILANE .EQ. 1) THEN
    IF(SFLEET(IV) .NE. FLEET_BUS .AND. SFLEET(IV) .NE. FLEET_EV) THEN
      IF(LOC1 .LE. SLENGTH(IL) / 2) THEN
        IF(SLENGTH(IL)/2 - LOC1 .LE. SSPEED(IV)) THEN
          IF(.NOT. INITMODE) EXITING_VEHICLES = EXITING_VEHICLES + 1
 
! --- Delete the vehicle, patch the leader and follower, and
! --- update first and last vehicle on the link, if necessary.
 
          IF(SFOLLOWER(IV) .NE. 0) SLEADER(SFOLLOWER(IV)) = SLEADER(IV)
          IF(SLEADER(IV) .NE. 0) SFOLLOWER(SLEADER(IV)) = SFOLLOWER(IV)
          IF(FIRST_VEHICLE(IL, ILANE) .EQ. IV) FIRST_VEHICLE(IL, ILANE) = SFOLLOWER(IV)
          IF(SLAST_VEHICLE(IL, ILANE) .EQ. IV) SLAST_VEHICLE(IL, ILANE) = SLEADER(IV)
 
! --- Decrement the number of vehicles to be extracted.
 
          CENTROID_NV(IL) = CENTROID_NV(IL) + 1

! --- Determine if the vehicle has passed a detector.

          LOC2 = SLENGTH(IL) / 2
          IF(SFIRST_DETECTOR(IL) .NE. 0) CALL CHECK_DETECTOR_STREET(IV, IL, ILN, LOC1, LOC2)

          IF(.NOT. INITMODE) THEN
            Z1 = SLOCATION(IV) + SSPEED(IV) - SLENGTH(IL) / 2
            T1 = Z1 / SSPEED(IV)
            CALL UPDATE_STREET_STATS(IV, IL, 0, Z1, 0., T1, 0.)
 
! --- Count the vehicle as a discharge from the link.
 
            SOURCE_SINK(IL) = SOURCE_SINK(IL) - 1
            SDISCHARGED(IL) = SDISCHARGED(IL) + 1
            ILN = SLANE(IV)
            SDISCHARGED_LANE(IL, ILN) = SDISCHARGED_LANE(IL, ILN) + 1
            IF(STURNCODE(IV) .EQ. TC_LEFT) THEN
              DISCHARGED_LEFT(IL) = DISCHARGED_LEFT(IL) + 1
            ELSEIF(STURNCODE(IV) .EQ. TC_RIGHT) THEN
              DISCHARGED_RIGHT(IL) = DISCHARGED_RIGHT(IL) + 1
            ELSEIF(STURNCODE(IV) .GT. TC_RIGHT) THEN
              DISCHARGED_DIAG(IL) = DISCHARGED_DIAG(IL) + 1
            ENDIF
            SPERSON_TRIPS(IL) = SPERSON_TRIPS(IL) + AVG_OCCS(SVTYPE(IV)) / 2
            IF(WRITE_SUPPLEMENTAL_FILES) CALL WRITE_VEHICLE_EXIT_TEXT(SID(IV), CENTROID_LABEL(IL))
            CALL DELETE_STREET_VEHICLE(IV, .TRUE.)
            RETURN
          ENDIF
        ELSE
          IF(SSPEED(IV) .GT. RT_SPEED(IL)) THEN
            IF(SLEADER(IV) .EQ. 0) THEN
              DECEL = -(SSPEED(IV) - RT_SPEED(IL)) ** 2 / (2 * (SLENGTH(IL)/2 - LOC1))
            ELSEIF(SLOCATION(SLEADER(IV)) .GT. SLENGTH(IL)/2) THEN
              DECEL = -(SSPEED(IV) - RT_SPEED(IL)) ** 2 / (2 * (SLENGTH(IL)/2 - LOC1))
            ELSE
              DECEL = 100
            ENDIF
            ACCEL = MIN(ACCEL, DECEL)
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE AMBER_DECISION(IV, DIST, WPERM)
! ----------------------------------------------------------------------
! --- Determine if the vehicle will drive through on amber.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE DRIVERS
  USE VDATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  REAL, INTENT(IN) :: DIST
  LOGICAL, INTENT(INOUT) :: WPERM
  INTEGER :: IL
  REAL :: DECEL, RMIN, STOPDIST
  REAL, PARAMETER :: DECISION_TIME = 2.0
! ----------------------------------------------------------------------
  WPERM = .FALSE.
  DECEL = -SSPEED(IV)**2 / (2 * DIST)
  IL = SLINK(IV)
  IF(SFLEET(IV) .EQ. FLEET_BIKE) THEN
    RMIN = -8.0
  ELSE
    RMIN = MAX_DECEL(SVTYPE(IV), SSPEED(IV), SGRADE(IL))
  ENDIF
  STOPDIST = -(SSPEED(IV) ** 2) / (2 * RMIN)
  IF(DECEL .LT. AMBER_DECEL(SDRIVERTYPE(IV))) THEN
    IF(DIST / SSPEED(IV) .LE. DECISION_TIME .OR. DIST .LT. STOPDIST) THEN
      WPERM = .TRUE.
      SGO_THRU_SIGNAL(IV) = .TRUE.
    ELSE
      FORCE_STOP(IV) = .TRUE.
    ENDIF
  ENDIF
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE SIGNAL_FOR_TURN(IV)
! ----------------------------------------------------------------------
! --- Determine if the vehicle will use a turn indicator.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE SEEDS
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  REAL :: RNDNUM
! ----------------------------------------------------------------------
  !Use a probability distribution to decide if the driver uses the turn signal.
  CALL STREET_RANDOM(SSEED, RNDNUM)
  IF(RNDNUM .GE. TURNSIGNAL_PROB(SDRIVERTYPE(IV))) THEN
    TURN_INDICATOR(IV) = STURNCODE(IV)
  ELSE
    !Set the indicator to show that the driver decided to not use the indicator
    !for the upcoming turn to prevent calling the subroutine again.
    TURN_INDICATOR(IV) = TC_THRU
  ENDIF
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE MOVE_ALONG_ARC(IV, TRAVEL)
  USE STREET_LINKS
  USE STREET_VEHICLES
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  REAL, INTENT(IN) :: TRAVEL
  REAL :: FRAC, DIST
  COMMON /GRAPHIO/ TSTEPFILE, TINTERVALFILE
  INTEGER :: TSTEPFILE, TINTERVALFILE
  INTEGER :: IL
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

  IL = SLINK(IV)
  IF(ARC_DIRECTION(IV) .EQ. TC_THRU) THEN
    !Update the location of the vehicle
    SLOCATION(IV) = SLOCATION(IV) + TRAVEL
    !Map the location of the vehicle onto the arc
    IF(UP_INT_WIDTH(IL) .EQ. 0) THEN
      SLOCATION(IV) = SLOCATION(IV) - ARC_LENGTH(IV)
      ARC_ENTRYLINK(IV) = 0
      ARC_LOCATION(IV) = 0.
      ARC_LENGTH(IV) = 0.
    ELSE
      ARC_LOCATION(IV) = ARC_LENGTH(IV) * SLOCATION(IV) / UP_INT_WIDTH(IL)
    ENDIF
  ELSE
    !Compute the location along the arc
    ARC_LOCATION(IV) = ARC_LOCATION(IV) + TRAVEL
    FRAC = ARC_LOCATION(IV) / ARC_LENGTH(IV)
    DIST = UP_INT_WIDTH(IL) - LANE_CENTER(ARC_ENTRYLINK(IV), ARC_ENTRYLANE(IV))
    IF(DIST .GT. 0.) THEN
      !Map the location along the arc into SLOCATION
      IF(TRAVEL .GT. 0.) THEN
        IF(FRAC .GE. 1.0) THEN
          SLOCATION(IV) = SLOCATION(IV) + TRAVEL
        ELSE
          SLOCATION(IV) = MAX(SLOCATION(IV), LANE_CENTER(ARC_ENTRYLINK(IV), ARC_ENTRYLANE(IV)) + FRAC * DIST)
        ENDIF
      ENDIF
    ELSE
      SLOCATION(IV) = SLOCATION(IV) + TRAVEL
    ENDIF
  ENDIF

  !If the vehicle is outside of the intersection clear the arc settings
  IF(ARC_LOCATION(IV) .GT. ARC_LENGTH(IV)) THEN
    ARC_ENTRYLINK(IV) = 0
    ARC_LOCATION(IV) = 0.
    ARC_LENGTH(IV) = 0.
  ENDIF
  
  !Write the position on the arc to be used by the 3D animator. 
  IF(.NOT. INITMODE .AND. TSTEPFILE .EQ. 2) THEN
    IF(ARC_LOCATION(IV) .GT. 0) THEN
      CALL WRITE_VEHICLE_TEXT_ARC(IV)
    ENDIF
  ENDIF
  
  RETURN
  END

! ==================================================================================================
  SUBROUTINE MOVE_ALONG_TURNING_WAY(IV, TRAVEL)
  USE STREET_LINKS
  USE STREET_VEHICLES
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  REAL, INTENT(IN) :: TRAVEL
  INTEGER:: IL
  REAL :: FRAC, DIST
  COMMON /GRAPHIO/ TSTEPFILE, TINTERVALFILE
  INTEGER :: TSTEPFILE, TINTERVALFILE
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

  ARC_LOCATION(IV) = ARC_LOCATION(IV) + TRAVEL
  
  !Map the distance along the arc into SLOCATION
  IL = SLINK(IV)
  FRAC = TRAVEL / ARC_LENGTH(IV)
  DIST = SLENGTH(IL) - RTW_EXIT_POINT(IL)
  SLOCATION(IV) = SLOCATION(IV) + FRAC * DIST
  !Write the position on the turning way to be used by the 3D animator. 
  IF(.NOT. INITMODE .AND. TSTEPFILE .EQ. 2) THEN
    CALL WRITE_VEHICLE_TEXT_RTW(IV)
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE CHOOSE_ROUNDABOUT_EXIT(IL, IV)
  USE STREET_LINKS
  USE STREET_VEHICLES
  USE ROUNDABOUT_DATA
  USE SEEDS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IL, IV
  INTEGER :: IAP, IRND, I, IEXIT(5)
  REAL :: RNDNUM, PCT(5)
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

  CALL STREET_RANDOM(SSEED, RNDNUM)
  IRND = ROUNDABOUT_ID(IL)
  IAP = ROUNDABOUT_APPROACH_NUM(IL)
  IEXIT = 0
  IEXIT(1) = IAP
  PCT(1) = ROUNDABOUT(IRND)%EXIT_PCTS(IAP, IAP)
  DO I = 2, ROUNDABOUT(IRND)%APPROACHES
    IEXIT(I) = IEXIT(I - 1) + 1
    IF(IEXIT(I) .GT. ROUNDABOUT(IRND)%APPROACHES) IEXIT(I) = 1
    PCT(I) = PCT(I - 1) + ROUNDABOUT(IRND)%EXIT_PCTS(IAP, IEXIT(I))
  ENDDO
  FIRST_RIGHT(IV) = .FALSE.
  IF(RNDNUM .LE. PCT(1)) THEN
    ROUNDABOUT_EXIT(IV) = ROUNDABOUT(IRND)%DEPARTING_LINKS(IEXIT(1))
    IF(IAP .EQ. IEXIT(1)) FIRST_RIGHT(IV) = .TRUE.
  ELSEIF(RNDNUM .LE. PCT(2)) THEN
    ROUNDABOUT_EXIT(IV) = ROUNDABOUT(IRND)%DEPARTING_LINKS(IEXIT(2))
    IF(IAP .EQ. IEXIT(2)) FIRST_RIGHT(IV) = .TRUE.
  ELSEIF(RNDNUM .LE. PCT(3)) THEN
    ROUNDABOUT_EXIT(IV) = ROUNDABOUT(IRND)%DEPARTING_LINKS(IEXIT(3))
    IF(IAP .EQ. IEXIT(3)) FIRST_RIGHT(IV) = .TRUE.
  ELSEIF(RNDNUM .LE. PCT(4)) THEN
    ROUNDABOUT_EXIT(IV) = ROUNDABOUT(IRND)%DEPARTING_LINKS(IEXIT(4))
    IF(IAP .EQ. IEXIT(4)) FIRST_RIGHT(IV) = .TRUE.
  ELSE
    ROUNDABOUT_EXIT(IV) = ROUNDABOUT(IRND)%DEPARTING_LINKS(IEXIT(5))
    IF(IAP .EQ. 5) FIRST_RIGHT(IV) = .TRUE.
  ENDIF
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE MERGE_TRAFFIC(IV, ACCEL)
  USE FREEWAY_LINKS
  USE FREEWAY_VEHICLES
  USE FREEWAY_NODES
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  REAL, INTENT(INOUT) :: ACCEL
  INTEGER :: MAIN, RAMP, ILD, JLINK
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = fid(iv)
#endif

  RAMP = FLINK(IV)
  MAIN = MAINLINE_APPROACH(FDSN(RAMP))
  JLINK = FTHRU_LINK(RAMP)
  IF(FLANE(IV) .EQ. MERGING_LANE(RAMP)) THEN
    ILD = FLAST_VEHICLE_OUT(MAIN, MERGING_LANE(MAIN))
    IF(ILD .NE. 0) THEN
      IF(FLINK(ILD) .EQ. JLINK) THEN
        CALL FREEWAY_CAR_FOLLOW(IV, ILD, ACCEL)
      ENDIF
    ENDIF
  ENDIF
  
  RETURN
  END  
  
  SUBROUTINE MAIN_MERGE_TRAFFIC(IV, ACCEL)
  USE FREEWAY_LINKS
  USE FREEWAY_VEHICLES
  USE FREEWAY_NODES
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  REAL, INTENT(INOUT) :: ACCEL
  INTEGER :: MAIN, RAMP, ILD, JLINK
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = fid(iv)
#endif

  MAIN = FLINK(IV)
  RAMP = MERGE_APPROACH(FDSN(MAIN))
  JLINK = FTHRU_LINK(MAIN)
  IF(FLANE(IV) .EQ. MERGING_LANE(MAIN)) THEN
    ILD = FLAST_VEHICLE_OUT(RAMP, MERGING_LANE(RAMP))
    IF(ILD .NE. 0) THEN
      IF(FLINK(ILD) .EQ. JLINK) THEN
        CALL FREEWAY_CAR_FOLLOW(IV, ILD, ACCEL)
      ENDIF
    ENDIF
  ENDIF
  
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE CHECK_TURNING_WAY_EXIT(IV, ILD, IFL, WPERM)
  !Determine if the vehicle can leave the turning way and enter the receiving link
  USE STREET_LINKS
  USE STREET_VEHICLES
  USE DRIVERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER, INTENT(OUT) :: ILD, IFL
  LOGICAL, INTENT(INOUT) :: WPERM
  INTEGER :: IL, ENTRY_POINT, ENTRY_LINK
  REAL :: TTC
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

  WPERM = .FALSE.
  IL = SLINK(IV)
  ILD = 0
  IFL = 0
  ENTRY_LINK = RTW_RECEIVING_LINK(IL)
  ENTRY_POINT = RTW_ENTRY_POINT(ENTRY_LINK)
  IF(ENTRY_LINK .EQ. 0) THEN
    WPERM = .TRUE.
  ELSE
    ILD = FIRST_VEHICLE(ENTRY_LINK, FIRST_FULL_LANE(ENTRY_LINK))
    IF(ILD .EQ. 0) THEN
      WPERM = .TRUE.
    ELSE
      IFL = SFOLLOWER(ILD)
      DO 
        IF(IFL .EQ. 0) EXIT
        IF(SLOCATION(IFL) .GT. ENTRY_POINT) THEN
          ILD = IFL
          IFL = SFOLLOWER(ILD)
        ELSE
          EXIT
        ENDIF
      ENDDO
    ENDIF
  ENDIF
  IF(ILD .NE. 0) THEN
    IF(SLOCATION(ILD) .GT. ENTRY_POINT .AND. SLOCATION(ILD) - SVLENGTH(ILD) .LT. ENTRY_POINT) THEN
      WPERM = .FALSE.
    ELSEIF(SLOCATION(ILD) .LE. ENTRY_POINT) THEN
      TTC = (ENTRY_POINT - SLOCATION(ILD)) / SSPEED(ILD)
      IF(TTC .GT. ACCEPTABLE_RTG(SDRIVERTYPE(IV))) THEN
        WPERM = .TRUE.
      ELSE
        WPERM = .FALSE.
      ENDIF
    ELSEIF(IFL .EQ. 0) THEN
      WPERM = .TRUE.
    ELSE
      IF(SSPEED(IFL) .NE. 0) THEN
        TTC = (ENTRY_POINT - SLOCATION(IFL)) / SSPEED(IFL)
        IF(TTC .GT. ACCEPTABLE_RTG(SDRIVERTYPE(IV))) THEN
          WPERM = .TRUE.
        ELSE
          WPERM = .FALSE.
        ENDIF
      ELSEIF(SLOCATION(IFL) .LT. ENTRY_POINT - 20) THEN
        WPERM = .TRUE.
      ENDIF
    ENDIF
  ENDIF
  IF(WPERM .AND. ILD .NE. 0) THEN
    IF(SLOCATION(ILD) .LT. ENTRY_POINT) THEN
      IFL = ILD
      ILD = 0
    ENDIF
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE CHECK_MERGING_VEHICLE(IV, IRL, IRLANE, ACCEL)
  USE STREET_LINKS
  USE STREET_VEHICLES
  USE VDATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, IRL, IRLANE
  REAL, INTENT(INOUT) :: ACCEL
  INTEGER :: IL, ILINK, ILD, ILANE, NRLANE
  REAL :: TTC, TTCX, A, RMAX
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

  IL = SLINK(IV)
  IF(SLENGTH(IL) - SLOCATION(IV) .LT. (SSPEED(IV) ** 2) / 7) THEN
    ILINK = CHECK_MERGE(IL)
    TTC = 1000.
    TTCX = 1000.
    IF(SSPEED(IV) .NE. 0.) THEN
      TTC = (SLENGTH(IL) - SLOCATION(IV)) / SSPEED(IV)
    ENDIF
    DO ILANE = 1, TOTAL_LANES(ILINK)
      ILD = FIRST_VEHICLE(ILINK, ILANE)
      IF(ILD .NE. 0) THEN
        CALL FIND_RECEIVING_LANE(ILD, IRL, SLANE(ILD), NRLANE)
        IF(IRLANE .EQ. NRLANE) THEN
          IF(SSPEED(ILD) .NE. 0.) THEN
            TTCX = (SLENGTH(ILINK) - SLOCATION(ILD)) / SSPEED(ILD)
            IF(TTCX .LT. TTC) THEN
              CALL STREET_CAR_FOLLOW(IV, ILD, A)
              ACCEL = MIN(ACCEL, A)
              RMAX = MAX_DECEL(SVTYPE(IV), SSPEED(IV), SGRADE(SLINK(IV)))
              ACCEL = MAX(ACCEL, RMAX)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDDO
  ENDIF
  RETURN
  END
  
