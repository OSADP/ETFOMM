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
  SUBROUTINE CHECK_FOR_FREEWAY_LANECHANGE(IV)
! ----------------------------------------------------------------------
! --- Determine if the vehicle will attempt a lane change this time step.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE GLOBAL_DATA
  USE FREEWAY_LINKS
  USE VEHICLE_TYPES
  USE BUS_ROUTE_DATA
  USE BUS_STATION_DATA
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER :: IL, ILANE, IGOAL, ITURN, ICODE
  INTEGER :: RLANE
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = fid(iv)
#endif

  IL = FLINK(IV)  
  ILANE = FLANE(IV)
  ITURN = FTURNCODE(IV)
  RLANE = 0
  IF(ILANE .NE. 0) THEN
    ICODE = FLANECODES(IV, ILANE)
 
! --- If the vehicle is cooperating with an EV determine the rightmost lane.
        
    IF(FWILL_COOP_EV(IV) .AND. FEV_WATCH(IV) .NE. 0) THEN
      IF(DISTANCE_TO_SEGMENT_END(IV) .LT. DISTANCE_TO_SEGMENT_END(FEV_WATCH(IV))) THEN
        IF(ILANE .GT. 1 .AND. ILANE .LT. 9) THEN
          RLANE = ILANE - 1
        ELSEIF(ILANE .EQ. 1) THEN
          IF(FLANECODES(IV, 9) .NE. LC_NULL) RLANE = 9
        ELSEIF(ILANE .EQ. 9) THEN
          IF(FLANECODES(IV, 10) .NE. LC_NULL) RLANE = 10
        ELSEIF(ILANE .EQ. 10) THEN
          IF(FLANECODES(IV, 11) .NE. LC_NULL) RLANE = 11
        ENDIF
      ENDIF
    ENDIF
 
! --- Determine if the vehicle needs to perform a mandatory lane change.
 
    IF(ICODE .NE. 0 .AND. FLANE(IV) .NE. 0) THEN
      IF(RLANE .NE. 0) THEN
 
! --- If the vehicle is cooperating with an EV set the rightmost lane
! --- as the goal lane.
         
        IGOAL = RLANE
      ELSE
        CALL FIND_TARGET_LANE_FREEWAY(IV, IGOAL)
      ENDIF
      IF(IGOAL .NE. 0) CALL TRY_FREEWAY_LANECHANGE(IV, IGOAL)
 
    ELSEIF(FSPEED(IV) .LT. FDESIREDSPEED(IV)) THEN
 
! --- The vehicle may wish to perform a discretionary lane change.
 
      IF(FLEADER(IV) .NE. 0) THEN
 
! --- If the vehicle is cooperating with an EV do not allow
! --- a discretionary lane change.
         
        IF(FFLEET(FLEADER(IV)) .NE. FLEET_EV) CALL FREEWAY_DISC_LANECHANGE(IV)
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE CHECK_FOR_STREET_LANECHANGE(IV)
! ----------------------------------------------------------------------
! --- Determine if the vehicle will attempt a lane change this time step.
! ----------------------------------------------------------------------
  USE GLOBAL_DATA
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE VEHICLE_TYPES
  USE BUS_ROUTE_DATA
  USE BUS_STATION_DATA
  USE SIMPARAMS
  USE SEEDS
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER :: IL, ILANE, IGOAL, ISTAT, ITURN, ICODE
  INTEGER :: CURRENT(N_STREET_LANES)
  REAL :: RNDNUM, MINDIST
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

  IL = SLINK(IV)
  ILANE = SLANE(IV)
  ITURN = STURNCODE(IV)
  IF(ILANE .EQ. 0) THEN
    ICODE = LC_VACATE
  ELSE
    ICODE = SLANECODES(IV, ILANE) 
  ENDIF
  
  !Consider goal lane for a vehicle entering a roundabout.
  IF(ROUNDABOUT_APPROACH_NUM(IL) .NE. 0 .AND. GOAL_LANE(IV) .NE. 0 .AND. ILANE .NE. GOAL_LANE(IV)) THEN
    IGOAL = GOAL_LANE(IV)
    CALL TRY_STREET_LANECHANGE(IV, IGOAL)
    
  ELSEIF(SLENGTH(IL) - SLOCATION(IV) .LT. 10 .AND. ICODE .NE. LC_GOOD) THEN
 
! --- The vehicle has reached the end of the link in a lane that does not
! --- allow its assigned turn movement, and it is not a bus or an EV,
! --- reassign a different movement that is allowed from the current lane.
 
    IF(SFLEET(IV) .NE. FLEET_BUS .AND. SFLEET(IV) .NE. FLEET_EV) THEN
      CURRENT = SLANECODES(IV, 1:N_STREET_LANES)
      CALL RESET_TURNCODE(IV)
      ICODE = SLANECODES(IV, SLANE(IV))
      IF(ITURN .NE. STURNCODE(IV)) THEN
        SPSEUDO_LEADER(IV) = 0
      ELSE
        SLANECODES(IV, 1:N_STREET_LANES) = CURRENT
        CALL FIND_TARGET_LANE_STREET(IV, IGOAL)
        IF(IGOAL .NE. 0) SLANECODES(IV, SLANE(IV)) = LC_GOOD
      ENDIF
    ENDIF
  ENDIF

  IF(SFLEET(IV) .NE. FLEET_EV .AND. SFLEET(IV) .NE. FLEET_BUS) THEN
! --- Do not allow a second lane change until the timer reaches zero, other than EVs and Buses.
    IF(SLC_TIMER(IV) .GT. 0) RETURN
  ENDIF
 
! --- Do not allow a lane change within 500 feet of an entry node or
! --- within an intersection.
 
  MINDIST = UP_INT_WIDTH(IL)
  
  !Allow lane changes within intersections for a roundabout
  IF(ROUNDABOUT_ID(IL) .NE. 0) MINDIST = 0.
  
  IF(SPREVLINK(IV) .EQ. 0) THEN
    IF(CENTROID(IL) .EQ. 0) MINDIST = 100.
  ELSEIF(NODE_TYPE(SDSN(IL)) .NE. NT_INTERN) THEN
    MINDIST = 100.
  ENDIF
  IF(SLOCATION(IV) - SVLENGTH(IV) .GE. MINDIST) THEN
    IF(ILANE .GT. 0) THEN
      IGOAL = 0
      IF(STURNCODE(IV) .EQ. TC_RIGHT .AND. RTW_EXIT_POINT(IL) .GT. 0 .AND. &
        ILANE .EQ. NUMBER_TURNINGWAYS(IL) + 1 .AND. ILANE .LT. FIRST_FULL_LANE(IL)) THEN
        
! --- Try to enter the turning way from the turn pocket.
 
        CALL ENTER_TURNING_WAY(IV)
        
      ELSEIF(STURNCODE(IV) .EQ. TC_LEFT .AND. ILANE .EQ. LAST_FULL_LANE(IL) .AND. NUMBER_LEFTPOCKETS(IL) .GT. 0) THEN

! --- Try to enter the first left turn pocket lane.

        CALL ENTER_POCKET(IV, IGOAL)
        
      ELSEIF(STURNCODE(IV) .EQ. TC_RIGHT .AND. ILANE .EQ. FIRST_FULL_LANE(IL) .AND. NUMBER_RIGHTPOCKETS(IL) .GT. 0) THEN

! --- Try to enter the first right turn pocket lane.
 
        CALL ENTER_POCKET(IV, IGOAL)
        
      ELSEIF(STURNCODE(IV) .EQ. TC_RIGHT .AND. ILANE .EQ. FIRST_FULL_LANE(IL) .AND. RTW_EXIT_POINT(IL) .GT. 0) THEN

! --- Try to enter the turning way.
 
        CALL ENTER_TURNING_WAY(IV)
        RETURN
        
      ENDIF 
 
! --- Determine if the vehicle needs to perform a mandatory lane change.
 
      IF(IGOAL .EQ. 0) IGOAL = DESIRED_LANE(IV)
      IF(ICODE .NE. LC_GOOD .OR. (IGOAL .NE. 0 .AND. IGOAL .NE. SLANE(IV))) THEN
 
! --- The vehicle is in an unacceptable lane or has a goal lane.
 
! --- If the vehicle did not try to enter a turn pocket check to see
! --- if it has a previously determined goal lane to position it for
! --- a downstream turn movement.
 
        IF(IGOAL .EQ. 0) CALL FIND_TARGET_LANE_STREET(IV, IGOAL)
 
        IF(SFLEET(IV) .NE. FLEET_BUS) THEN
 
! --- The vehicle is not a bus.
 
          IF(SLENGTH(IL) - SLOCATION(IV) .LT. 25) THEN
 
! --- The vehicle is in an acceptable lane and is within 25 ft
! --- of the intersection, so it should remain in the current lane. It
! --- will try to get into the goal lane on the next link. Set the
! --- current goal lane to 0 to prevent the lane change.
 
            IF(ICODE .EQ. LC_GOOD) IGOAL = 0
          ENDIF
 
        ELSE
 
! --- The vehicle is a bus. Check for a required stop at a station.
 
          ISTAT = BUSR_STATIONLIST(SROUTEID(IV), NEXT_STOP(IV))
          IF(ISTAT .NE. 0) THEN
            IF(IL .EQ. BUS_STATION_LIST(ISTAT)%LINK) IGOAL = FIRST_FULL_LANE(IL)
          ENDIF
        ENDIF
 
! --- The vehicle should attempt to make a lane change into lane IGOAL.
 
        IF(IGOAL .NE. 0) THEN
 
! --- If the vehicle is in a pocket lane and the driver knows the next
! --- downstream turncode use that to determine which pocket lane to use,
! --- if there is more than one.
 
          IF(ILANE .GT. LAST_FULL_LANE(IL) .OR. ILANE .LT. FIRST_FULL_LANE(IL)) THEN
            CALL STREET_RANDOM(SSEED, RNDNUM)
            IF(RNDNUM .GT. DRIVER_FAMPCT) THEN
              IF(STURNCODE(IV) .EQ. TC_LEFT .AND. ILANE .LT. TOTAL_LANES(IL)) THEN
                IF(TURN_CODE2(IV) .EQ. TC_LEFT) THEN
                  IGOAL = ILANE + 1
                ENDIF             
              ELSEIF(STURNCODE(IV) .EQ. TC_RIGHT .AND. ILANE .LT. NUMBER_RIGHTPOCKETS(IL)) THEN
                IF(TURN_CODE2(IV) .EQ. TC_RIGHT) THEN
                  IGOAL = ILANE - 1
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          CALL TRY_STREET_LANECHANGE(IV, IGOAL)
        ENDIF
        
      ELSEIF(SSPEED(IV) .LT. SDESIREDSPEED(IV)) THEN
 
! --- A thru vehicle may wish to perform a discretionary lane change.
 
        IF(SLEADER(IV) .NE. 0) THEN
          IF(ILANE .GT. LAST_FULL_LANE(IL) .OR. ILANE .LT. FIRST_FULL_LANE(IL)) THEN
 
! --- If the vehicle is in a pocket lane check for a better pocket lane.
 
            IF(STURNCODE(IV) .EQ. TC_LEFT .AND. NUMBER_LEFTPOCKETS(IL) .GT. 1) THEN
              IF(ILANE .LT. TOTAL_LANES(IL)) THEN
                IGOAL = ILANE - 1
                CALL STREET_POCKET_LANECHANGE(IV, IGOAL)
              ENDIF             
            ELSEIF(STURNCODE(IV) .EQ. TC_RIGHT .AND. NUMBER_RIGHTPOCKETS(IL) .GT. 1) THEN
              IF(ILANE .GT. 1) THEN
                IGOAL = ILANE - 1
                CALL STREET_POCKET_LANECHANGE(IV, IGOAL)
              ENDIF
            ENDIF
          ELSE
 
! --- If the vehicle is cooperating with an EV do not allow
! --- a discretionary lane change.
        
            IF(SFLEET(SLEADER(IV)) .NE. FLEET_EV) CALL STREET_DISC_LANECHANGE(IV)
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE ENTER_POCKET(IV, GOAL)
! ----------------------------------------------------------------------
! --- Determine if a vehicle can enter a turn pocket.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER, INTENT(OUT) :: GOAL 
  INTEGER :: IL, IPL, IVX
! ----------------------------------------------------------------------
  GOAL = 0
  IL = SLINK(IV)
  IF(STURNCODE(IV) .EQ. TC_LEFT) THEN
    IPL = LAST_FULL_LANE(IL) + 1
 
! --- Determine if the subject vehicle has reached the beginning
! --- of the pocket lane.
 
    IF(SLOCATION(IV) .GT. SLENGTH(IL) - LANE_LENGTH(IL, IPL)) GOAL = IPL
 
! --- Identify the last vehicle in the turn pocket lane. Car follow
! --- that vehicle to prevent driving past the back of the queue.
 
    IF(SLANECODES(IV, SLANE(IV)) .EQ. LC_VACATE) THEN
      IVX = SLAST_VEHICLE(IL, IPL)
      IF(IVX .NE. 0) THEN
        IF(SLOCATION(IV) .LT. SLOCATION(IVX) + SVLENGTH(IVX)) THEN
          SPSEUDO_LEADER(IV) = IVX
        ENDIF
      ENDIF
    ENDIF
  ELSEIF(STURNCODE(IV) .EQ. TC_RIGHT) THEN
    IPL = NUMBER_RIGHTPOCKETS(IL) + NUMBER_TURNINGWAYS(IL)
 
! --- Determine if the subject vehicle has reached the beginning
! --- of the pocket lane.
 
    IF(SLOCATION(IV) .GT. SLENGTH(IL) - LANE_LENGTH(IL, IPL)) GOAL = IPL
 
! --- Identify the last vehicle in the turn pocket lane. Car follow
! --- that vehicle to prevent driving past the back of the queue.
 
    IF(SLANECODES(IV, SLANE(IV)) .EQ. LC_VACATE) THEN
      IVX = SLAST_VEHICLE(IL, IPL)
      IF(IVX .NE. 0) THEN
        IF(SLOCATION(IV) .LT. SLOCATION(IVX) + SVLENGTH(IVX)) THEN
          SPSEUDO_LEADER(IV) = IVX
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE FIND_TARGET_LANE_FREEWAY(IV, GOAL)
! ----------------------------------------------------------------------
! --- Determine if the vehicle has a preferred lane.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE SEEDS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER, INTENT(OUT) :: GOAL
  INTEGER :: RIGHT, NRIGHT, LEFT, NLEFT
  INTEGER :: FIRSTLEFT, FIRSTRIGHT, LASTLEFT, LASTRIGHT, IL
  REAL :: RNDNUM
! ----------------------------------------------------------------------
  IL = FLINK(IV)
  FIRSTLEFT = 0
  FIRSTRIGHT = 0
  LASTLEFT = 0
  LASTRIGHT = 0
  GOAL = 0
 
! --- Find the first acceptable lane to the right and the number of
! --- lane changes required to reach that lane.
      
  RIGHT = FLANE(IV)
  NRIGHT = 0
  DO WHILE(FLANECODES(IV, RIGHT) .NE. LC_GOOD)
    RIGHT = FREEWAY_LANE_TO_RIGHT(RIGHT)
    IF(RIGHT .EQ. 0) EXIT
 
! --- Do not allow a lane change across a barrier.
 
    IF(BARRIER(IL, 1) .EQ. RIGHT) EXIT
    IF(BARRIER(IL, 2) .EQ. RIGHT) EXIT
           
    IF(FLANECODES(IV, RIGHT) .NE. LC_NULL) THEN
      NRIGHT = NRIGHT + 1
      IF(FIRSTRIGHT .EQ. 0) FIRSTRIGHT = RIGHT
      IF(FLANECODES(IV, RIGHT) .EQ. LC_GOOD) LASTRIGHT = RIGHT
    ENDIF
  ENDDO
 
! --- Find the first acceptable lane to the left and the number of
! --- lane changes required to reach that lane.
      
  LEFT = FLANE(IV)
  NLEFT = 0
  DO WHILE(FLANECODES(IV, LEFT) .NE. LC_GOOD)       
    LEFT = FREEWAY_LANE_TO_LEFT(LEFT)
    IF(LEFT .EQ. 0) EXIT
 
! --- Do not allow a lane change across a barrier.
 
    IF(BARRIER(IL, 1) .EQ. LEFT) EXIT
    IF(BARRIER(IL, 2) .EQ. LEFT) EXIT
    
    IF(FLANECODES(IV, LEFT) .NE. LC_NULL) THEN
      NLEFT = NLEFT + 1
      IF(FIRSTLEFT .EQ. 0) FIRSTLEFT = LEFT
      IF(FLANECODES(IV, LEFT) .EQ. LC_GOOD) LASTLEFT = LEFT
    ENDIF
  ENDDO
 
! --- Choose a goal lane and direction.
 
  IF(LASTLEFT .NE. 0 .AND. LASTRIGHT .EQ. 0) THEN
    GOAL = FIRSTLEFT
  ELSEIF(LASTLEFT .EQ. 0 .AND. LASTRIGHT .NE. 0) THEN
    GOAL = FIRSTRIGHT
  ELSEIF(LASTLEFT .NE. 0 .AND. LASTRIGHT .NE. 0) THEN
    IF(NLEFT .LT. NRIGHT) THEN
      GOAL = FIRSTLEFT
    ELSEIF(NLEFT .GT. NRIGHT) THEN
      GOAL = FIRSTRIGHT
    ELSE
 
! --- Choose randomly between equal options.
        
      CALL FREEWAY_RANDOM(FSEED, RNDNUM)
      IF(RNDNUM .LT. 0.5) THEN
        GOAL = FIRSTLEFT
      ELSE
        GOAL = FIRSTRIGHT
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE FIND_TARGET_LANE_STREET(IV, GOAL)
! ----------------------------------------------------------------------
! --- Determine if the vehicle has a preferred lane.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE SEEDS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER, INTENT(OUT) :: GOAL
  INTEGER :: RIGHT, NRIGHT, LEFT, NLEFT
  INTEGER :: FIRSTLEFT, FIRSTRIGHT, IL, ILANE
  REAL :: RNDNUM
! ----------------------------------------------------------------------
  IL = SLINK(IV)
  GOAL = 0
 
! --- Search through lanecodes to find the nearest acceptable lane.
       
  IF(STURNCODE(IV) .EQ. TC_LEFT .OR. STURNCODE(IV) .EQ. TC_LDIAG) THEN
 
! --- Find the first acceptable lane to the left.
      
    DO ILANE = SLANE(IV) + 1, LAST_FULL_LANE(IL)
      IF(SLANECODES(IV, ILANE) .NE. LC_NULL) THEN
        GOAL = ILANE
        EXIT
      ENDIF
    ENDDO
  ELSEIF(STURNCODE(IV) .EQ. TC_THRU) THEN
 
! --- Find the first acceptable lane to the right and the number of
! --- lane changes required to reach that lane.
       
    FIRSTRIGHT = 0
    NRIGHT = 0
    RIGHT = SLANE(IV) - 1
    IF(RIGHT .GT. 0) THEN
      DO ILANE = RIGHT, FIRST_FULL_LANE(IL), -1
        IF(SLANECODES(IV, ILANE) .EQ. LC_GOOD) THEN
          NRIGHT = NRIGHT + 1
          IF(FIRSTRIGHT .EQ. 0) FIRSTRIGHT = ILANE
        ENDIF
      ENDDO
    ENDIF
 
! --- Find the first acceptable lane to the left and the number of
! --- lane changes required to reach that lane.
       
    FIRSTLEFT = 0
    NLEFT = 0
    LEFT = SLANE(IV) + 1
    IF(LEFT .GT. LAST_FULL_LANE(IL)) THEN
      LEFT = 0
    ELSE
      DO ILANE = LEFT, LAST_FULL_LANE(IL)
        IF(SLANECODES(IV, ILANE) .EQ. LC_GOOD) THEN
          NLEFT = NLEFT + 1
          IF(FIRSTLEFT .EQ. 0) FIRSTLEFT = ILANE
        ENDIF
      ENDDO
    ENDIF
 
! --- Choose a goal lane and direction.
   
    IF(FIRSTLEFT .NE. 0 .AND. FIRSTRIGHT .EQ. 0) THEN
      GOAL = FIRSTLEFT
    ELSEIF(FIRSTLEFT .EQ. 0 .AND. FIRSTRIGHT .NE. 0) THEN
      GOAL = FIRSTRIGHT
    ELSEIF(FIRSTLEFT .NE. 0 .AND. FIRSTRIGHT .NE. 0) THEN
      IF(NLEFT .LT. NRIGHT) THEN
        GOAL = FIRSTLEFT
      ELSEIF(NLEFT .GT. NRIGHT) THEN
        GOAL = FIRSTRIGHT
      ELSE
 
! --- Choose randomly between equal options.
        
        CALL STREET_RANDOM(SSEED, RNDNUM)
        IF(RNDNUM .LT. 0.5) THEN
          GOAL = FIRSTLEFT
        ELSE
          GOAL = FIRSTRIGHT
        ENDIF
      ENDIF
    ENDIF
  ELSEIF(STURNCODE(IV) .EQ. TC_RIGHT .OR. STURNCODE(IV) .EQ. TC_RDIAG) THEN
 
! --- Find the first acceptable lane to the right.
      
    DO ILANE = SLANE(IV) - 1, FIRST_FULL_LANE(IL), -1
      IF(SLANECODES(IV, ILANE) .NE. LC_NULL) THEN
        GOAL = ILANE
        EXIT
      ENDIF
    ENDDO
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE TRY_FREEWAY_LANECHANGE(IV, GOAL)
! ----------------------------------------------------------------------
! --- Try to perform the lane change.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE FREEWAY_NODES
  USE GLOBAL_DATA
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER, INTENT(IN) :: GOAL
  REAL PD, AD
  INTEGER :: IL, ILD, IFL, MANDATORY_FLAG, I, IVX, ILANE, ILINK, IPX
  LOGICAL :: TRY
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = fid(iv)
#endif

  IL = FLINK(IV)
 
! --- Do not allow a lane change from an exclusive HOV lane into
! --- a lane that is not within the HOV facility.
 
  IF(NHOV_LANES(IL) .NE. 0) THEN
    IF(HOV_TYPE(IL) .EQ. 1) THEN
      IF(FLANE(IV) .EQ. HOV_LANES(IL, 1) .OR. FLANE(IV) .EQ. HOV_LANES(IL, 2) .OR. &
         FLANE(IV) .EQ. HOV_LANES(IL, 3)) THEN
        IF(GOAL .NE. HOV_LANES(IL, 1) .AND. GOAL .NE. HOV_LANES(IL, 2) .AND. &
           GOAL .NE. HOV_LANES(IL, 3)) THEN
          RETURN
        ENDIF
      ENDIF
    ENDIF
  ENDIF
 
! --- If the vehicle is on a freeway and is merging from an acceleration
! --- lane and its leader also needs to merge, it must wait behind the leader.
 
  IF(MUST_MERGE(IV)) THEN
    IF(FLEADER(IV) .NE. 0) THEN
      ILD = FLEADER(IV)
      IF(MUST_MERGE(ILD)) THEN
        IF(FLINK(ILD) .EQ. IL .AND. FLANE(ILD) .EQ. FLANE(IV)) THEN
          IF(FLANECODES(ILD, FLANE(ILD)) .NE. LC_GOOD) RETURN
        ENDIF
      ENDIF
    ENDIF
  ENDIF
 
! --- Search for the putative leader in the target lane.
      
  IPX = SORT_POSITION(IV)
  CALL FIND_FREEWAY_LEADER(IV, IPX, GOAL, ILD)
 
! --- If no leader was found and the vehicle is approaching a new segment
! --- look for a leader on the first link inside the new segment.
 
  IF(ILD .NE. 0) THEN
    IF(FID(ILD) .EQ. 0) ILD = 0.
  ENDIF
  IF(ILD .EQ. 0) THEN
    IF(FTURNCODE(IV) .EQ. TC_THRU .AND. FTHRU_LINK(IL) .NE. 0) THEN
      IF(SEGMENT(FTHRU_LINK(IL)) .NE. SEGMENT(IL)) THEN
        ILANE = RECEIVING_LANE(IL, GOAL)
        DO I = HIGHEST_INDEX_F, 1, -1
          IVX = SORTED_LIST(I)
          IF(IVX .EQ. 0) CYCLE
          IF(FLINK(IVX) .EQ. FTHRU_LINK(IL)) THEN
            IF(FLANE(IVX) .EQ. ILANE) THEN
              ILD = IVX
              EXIT
            ENDIF
          ENDIF
        ENDDO
      ENDIF
    ELSEIF(FTURNCODE(IV) .NE. TC_THRU .AND. OFFRAMP_LINK(IL) .NE. 0) THEN
      IF(SEGMENT(OFFRAMP_LINK(IL)) .NE. SEGMENT(IL)) THEN
        ILANE = EXIT_LANE(IL, GOAL)
        DO I = HIGHEST_INDEX_F, 1, -1
          IVX = SORTED_LIST(I)
          IF(IVX .EQ. 0) CYCLE
          IF(FLINK(IVX) .EQ. OFFRAMP_LINK(IL)) THEN
            IF(FLANE(IVX) .EQ. ILANE) THEN
              ILD = IVX
              EXIT
            ENDIF
          ENDIF
        ENDDO
      ENDIF
    ENDIF
  ENDIF
 
! --- Search for the putative follower. If a putative leader was found
! --- determine if that vehicle has a follower.
      
  IFL = 0
  IF(ILD .NE. 0) THEN
    IF(FFOLLOWER(ILD) .NE. 0) THEN
      IF(ISEGMENT(FFOLLOWER(ILD)) .EQ. ISEGMENT(IV)) THEN
        IFL = FFOLLOWER(ILD)
      ENDIF
    ENDIF
  ENDIF
  IF(IFL .EQ. 0) CALL FIND_FREEWAY_FOLLOWER(IV, GOAL, IFL)
  
! --- If no follower was found and the vehicle is on the first link of a
! --- new segment look for a follower on the last link of the previous segment.
 
  IF(IFL .NE. 0) THEN
    IF(FID(IFL) .EQ. 0) IFL = 0.
  ENDIF
  IF(IFL .EQ. 0) THEN
    ILINK = MAINLINE_APPROACH(FUSN(IL))
    IF(ILINK .NE. 0) THEN
      IF(NODE_TYPE(FUSN(ILINK)) .NE. NT_EXTERN) THEN
        IF(SEGMENT(ILINK) .NE. SEGMENT(IL)) THEN
          ILANE = 0
          DO I = 1, N_FREEWAY_LANES
            IF(RECEIVING_LANE(ILINK, I) .EQ. GOAL) THEN
              ILANE = I
              EXIT
            ENDIF
          ENDDO
          IF(ILANE .NE. 0) THEN
            DO I = 1, HIGHEST_INDEX_F
              IVX = SORTED_LIST(I)
              IF(IVX .EQ. 0) CYCLE
              IF(FLINK(IVX) .EQ. ILINK) THEN
                IF(FLANE(IVX) .EQ. ILANE) THEN
                  IFL = IVX
                  EXIT
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    ILINK = RAMP_APPROACH(FUSN(IL))
    IF(ILINK .NE. 0) THEN
      IF(NODE_TYPE(FUSN(ILINK)) .NE. NT_EXTERN) THEN
        IF(SEGMENT(ILINK) .NE. SEGMENT(IL)) THEN
          ILANE = 0
          DO I = 1, N_FREEWAY_LANES
            IF(RECEIVING_LANE(ILINK, I) .EQ. GOAL) THEN
              ILANE = I
              EXIT
            ENDIF
          ENDDO
          IF(ILANE .NE. 0) THEN
            DO I = 1, HIGHEST_INDEX_F
              IVX = SORTED_LIST(I)
              IF(IVX .EQ. 0) CYCLE
              IF(FLINK(IVX) .EQ. ILINK) THEN
                IF(FLANE(IVX) .EQ. ILANE) THEN
                  IF(ILD .EQ. 0) THEN
                    ILD = IVX           
                  ELSE
                    IF(DISTANCE_TO_SEGMENT_END(IVX) .LT. DISTANCE_TO_SEGMENT_END(ILD)) ILD = IVX
                  ENDIF
                  EXIT
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF
 
  TRY = .TRUE.
  IF(ILD .NE. 0) THEN
    IF(DISTANCE_TO_SEGMENT_END(ILD) + FVLENGTH(ILD) .GT. DISTANCE_TO_SEGMENT_END(IV)) TRY = .FALSE.
  ENDIF
  IF(TRY .AND. IFL .NE. 0) THEN
    IF(DISTANCE_TO_SEGMENT_END(IV) + FVLENGTH(IV) .GT. DISTANCE_TO_SEGMENT_END(IFL)) TRY = .FALSE.
  ENDIF
  AD = 1000.
  PD = 0.
  IF(TRY) THEN
    
! --- Determine the potential deceleration (PD) required as a result of
! --- performing the lane change.
 
    CALL FREEWAY_LANECHANGE_RISK(IV, ILD, IFL, PD)     
 
! --- Determine the acceptable deceleration (AD) for the driver of the
! --- subject vehicle.
       
    MANDATORY_FLAG = 1
    CALL FREEWAY_ACCEPTABLE_RISK(IV, MANDATORY_FLAG, AD)
  ENDIF
 
! --- If the risk is acceptable perform the lane change.
 
  IF(IFL .NE. 0) THEN
    IF(FLANE(IFL) .EQ. 0) IFL = 0
  ENDIF
  IF(AD .LE. PD) THEN
    CALL FREEWAY_CHANGE_LANE(IV, GOAL, ILD, IFL)
  ELSEIF(IFL .NE. 0) THEN
 
! --- Determine if the follower will cooperate by slowing down.
       
    IF(FWILL_COOP_LC(IFL) .OR. INCIDENT_NUM(IFL) .NE. 0 .OR. FLANE(IV) .EQ. 0 .OR. &
        FDIVERTED(IV) .OR. FLANECODES(IFL, FLANE(IFL)) .NE. LC_GOOD .OR. FFLEET(IV) .EQ. FLEET_BUS) THEN
      CALL FREEWAY_CAR_FOLLOW(IFL, IV, PD)

! --- If the potential deceleration is small tell the follower to slow down.

      IF(PD .GT. -8 .AND. FLOCATION(IFL) .LT. FLOCATION(IV) - FVLENGTH(IV)) THEN          !!!!arbitrary constants
        FSPEED_ADJ(IFL) = -15.     !!!!arbitrary constants
      ENDIF
      IF(FDIVERTED(IV)) THEN
        IF(PD .GT. -8 .AND. FLOCATION(IFL) .LT. FLOCATION(IV) - FVLENGTH(IV)) THEN
          FPSEUDO_LEADER(IV) = ILD
          FPSEUDO_LEADER(IFL) = IV
        ELSE
          FPSEUDO_LEADER(IV) = IFL
          IF(FFOLLOWER(IFL) .NE. 0) THEN
            FPSEUDO_LEADER(FFOLLOWER(IFL)) = IV
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ELSEIF(ILD .NE. 0) THEN
    IF(FDIVERTED(IV)) THEN
      CALL FREEWAY_CAR_FOLLOW(IV, ILD, PD)
      IF(PD .GT. -8 .AND. FLOCATION(IFL) .LT. FLOCATION(IV) - FVLENGTH(IV)) FPSEUDO_LEADER(IV) = ILD
    ENDIF
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE TRY_STREET_LANECHANGE(IV, GOAL)
! ----------------------------------------------------------------------
! --- Try to perform the lane change.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE VEHICLE_TYPES
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER, INTENT(IN) :: GOAL
  REAL PD, AD, D1, D2
  INTEGER :: IL, ILD, IFL, MANDATORY_FLAG, ILINK
  LOGICAL :: WBLOCK
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

  IL = SLINK(IV)
  IF(GOAL .LT. FIRST_FULL_LANE(IL) .OR. GOAL .GT. LAST_FULL_LANE(IL)) THEN
    IF(SLOCATION(IV) .LT. SLENGTH(IL) - LANE_LENGTH(IL, GOAL)) THEN
      RETURN
    ENDIF
  ENDIF
  
! --- Search for the putative leader in the target lane.
      
  CALL FIND_STREET_LEADER(IV, IL, GOAL, ILD)
 
! --- Search for the putative follower. If a putative leader was found
! --- determine if that vehicle has a follower.
      
  IFL = 0
  IF(ILD .NE. 0) THEN
    IF(SFOLLOWER(ILD) .NE. 0) IFL = SFOLLOWER(ILD)
  ENDIF
  IF(IFL .EQ. 0) CALL FIND_STREET_FOLLOWER(IV, GOAL, IFL)
 
! --- Determine the potential deceleration (PD) required as a result of
! --- performing the lane change.
  
  WBLOCK = .FALSE.
  IF(ILD .NE. 0) THEN
    IF(SLOCATION(ILD) - SVLENGTH(ILD) .LT. SLOCATION(IV) + 5) WBLOCK = .TRUE.
  ENDIF
  IF(IFL .NE. 0) THEN
    IF(SLOCATION(IV) - SVLENGTH(IV) .LT. SLOCATION(IFL) - 5) WBLOCK = .TRUE.
  ENDIF

  IF(IFL .EQ. 0 .AND. ROUNDABOUT_ID(IL) .GT. 0 .AND. ROUNDABOUT_APPROACH_NUM(IL) .EQ. 0) THEN
    DO ILINK = 1, N_STREET_LINKS
      IF(STHRU_LINK(ILINK) .EQ. IL) THEN
        IF(FIRST_VEHICLE(ILINK, GOAL) .NE. 0) THEN
          IF(SLENGTH(ILINK) - SLOCATION(FIRST_VEHICLE(ILINK, GOAL)) .LT. 50) WBLOCK = .TRUE.
        ENDIF
        EXIT
      ENDIF
    ENDDO
  ENDIF
  D1 = 0.
  D2 = 0.
  IF(ILD .NE. 0) THEN
    CALL STREET_CAR_FOLLOW(IV, ILD, D1)
  ELSEIF(SLANE(IV) .NE. 0) THEN
    ILD = SLAST_VEHICLE_OUT(IL, GOAL)
    IF(ILD .NE. 0) THEN
      IF(SLINK(ILD) .NE. 0) THEN
        IF(SUSN(SLINK(ILD)) .EQ. SDSN(IL)) THEN
          IF(SLOCATION(ILD) - SVLENGTH(ILD) .LT. SLOCATION(IV) - SLENGTH(IL)) THEN
            WBLOCK = .TRUE.
          ELSE
            CALL STREET_CAR_FOLLOW(IV, ILD, D1)
          ENDIF
        ENDIF
      ENDIF
      ILD = 0
    ENDIF
  ENDIF
  IF(IFL .NE. 0) CALL STREET_CAR_FOLLOW(IFL, IV, D2)
  PD = MIN(D1, D2)
 
! --- Determine the acceptable deceleration (AD) for the driver of the
! --- subject vehicle.
       
  MANDATORY_FLAG = 1
  CALL STREET_ACCEPTABLE_RISK(IV, MANDATORY_FLAG, AD)
 
! --- If the risk is acceptable perform the lane change.
 
  IF(AD .LT. PD .AND. .NOT. WBLOCK) THEN
    CALL STREET_CHANGE_LANE(IV, GOAL, ILD, IFL)
  ELSEIF(IFL .NE. 0) THEN
         
    IF(SFLEET(IV) .EQ. FLEET_BUS .OR. SWILL_COOP_LC(IFL) .OR. SLANE(IV) .EQ. 0 .OR. SSPEED(IV) .LE. STOP_SPD) THEN
 
! --- Determine if the follower will cooperate by slowing down.
! --- If the subject vehicle is on the shoulder make the follower cooperate.
! --- If the subject vehicle is a bus make the follower cooperate.
 
      IF(SLOCATION(IFL) .LT. SLOCATION(IV) - SVLENGTH(IV)) THEN
        IF(D1 .GT. D2 .AND. D2  .GE. -8.0) THEN
          SSPEED_ADJ(IFL) = -15.     !!!!arbitrary constant
        ELSEIF(SFLEET(IV) .EQ. FLEET_BUS) THEN
          IFL = SFOLLOWER(IFL)
          IF(IFL .NE. 0) THEN
            IF(SLOCATION(IV) - SVLENGTH(IV) - SLOCATION(IFL) .LT. 100) THEN
              SSPEED_ADJ(IFL) = -15.     !!!!arbitrary constant
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ELSEIF(D1 .GE. 0.) THEN
      SSPEED_ADJ(IV) = 15.     !!!!arbitrary constant
    ELSEIF(D2 .GE. 0.) THEN
      SSPEED_ADJ(IV) = -15.     !!!!arbitrary constant
    ELSE
    ENDIF
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE FREEWAY_LANECHANGE_RISK(IV, ILD, IFL, D)
! ----------------------------------------------------------------------
! --- Determine the risk of performing the lane change.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, ILD, IFL
  REAL, INTENT(OUT) :: D
  REAL D1, D2
! ----------------------------------------------------------------------
  D1 = 0.
  D2 = 0.
 
! --- Determine the car following deceleration required by the subject
! --- vehicle and the putative follower if the lane change is attempted.
      
  IF(ILD .NE. 0) CALL FREEWAY_CAR_FOLLOW(IV, ILD, D1)
  IF(IFL .NE. 0) CALL FREEWAY_CAR_FOLLOW(IFL, IV, D2)

! --- Return the worst-case deceleration.
      
  D = MIN(D1, D2)
  RETURN
  END

! ==================================================================================================
  SUBROUTINE FREEWAY_ACCEPTABLE_RISK(IV, FLAG, D)
! ----------------------------------------------------------------------
! --- Determine how much risk the driver will accept to perform the
! --- lane change.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE VEHICLE_TYPES
  USE VDATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, FLAG
  REAL, INTENT(OUT) :: D
  REAL :: FRACD
  REAL, PARAMETER :: MAX_DIST = 1000.
  REAL :: MAX_DEC
! ----------------------------------------------------------------------
!!!!arbitrary constants      
 
  D = NEMDEC(FVTYPE(IV))
  IF(FLAG .EQ. 1) THEN
 
! --- This is a mandatory lane change. Increase the risk to the maximum possible deceleration.
 
    MAX_DEC = MAX_DECEL(FVTYPE(IV), FSPEED(IV), FGRADE(FLINK(IV)))
    
    IF(FFLEET(IV) .EQ. FLEET_BUS) THEN
 
! --- Vehicle is a bus.
      D = MAX_DEC
      
    ELSEIF(FDIVERTED(IV)) THEN
 
! --- Vehicle is being diverted from the freeway.      
      D = MAX_DEC
 
    ELSEIF(HOV_URGENT(IV)) THEN
 
! --- Vehicle is trying to avoid an exclusive HOV lane or trying to get into avoid an exclusive HOV lane.      
      D = MAX_DEC
 
    ELSEIF(REMAINING_DIST(IV) .LE. MAX_DIST) THEN
 
! --- Vehicle is approaching a lane end, incident or an off-ramp
! --- that requires a lane change. Compute the acceptable risk based on
! --- the distance to the object requiring the lane change.
        
      IF(MUST_MERGE(IV)) THEN
        D = MAX_DEC
      ELSE
        FRACD = 1.0 - (REMAINING_DIST(IV) / MAX_DIST) 
        D = MIN(D, FRACD * MAX_DEC)
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE STREET_ACCEPTABLE_RISK(IV, FLAG, D)
! ----------------------------------------------------------------------
! --- Determine how much risk the driver will accept to perform the
! --- lane change.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE VEHICLE_TYPES
  USE VDATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, FLAG
  REAL, INTENT(OUT) :: D
  INTEGER :: IL
  REAL :: MAX_DEC, FRACD
! ----------------------------------------------------------------------
!!!!arbitrary constants      
 
! --- Set the default to 25% of the maximum acceptable risk.
 
  D = NEMDEC(SVTYPE(IV))
  IF(FLAG .EQ. 1) THEN
! --- This is a mandatory lane change. Increase the risk to the maximum possible deceleration.
 
    MAX_DEC = MAX_DECEL(SVTYPE(IV), SSPEED(IV), SGRADE(SLINK(IV)))
    
    IF(SFLEET(IV) .EQ. FLEET_BUS) THEN
 
! --- Vehicle is a bus.
         
      D = MAX_DEC
 
    ELSEIF(SPSAVE(IV) .NE. 0) THEN
 
! --- Vehicle is being diverted from the freeway.
         
      D = MAX_DEC
      
    ELSEIF(IN_TURNING_WAY(IV)) THEN
 
! --- Vehicle is being diverted from the freeway.
         
      D = MAX_DEC
      
    ELSE
 
! --- Compute the acceptable risk based on the distance to the end of the link.
        
      IL = SLINK(IV)
      FRACD = SLOCATION(IV) / SLENGTH(IL)
      D = MIN(D, FRACD * MAX_DEC)
      
    ENDIF
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE FREEWAY_CHANGE_LANE(IV, GOAL, ILD, IFL)
! ----------------------------------------------------------------------
! --- Perform the lane change.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE GLOBAL_DATA
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, GOAL, ILD, IFL
  INTEGER :: CL, CF, IL
! ----------------------------------------------------------------------
 
! --- The vehicle may have a different desired speed in the new lane.
 
  IF(.NOT. FDIVERTED(IV)) CALL SET_DESIREDSPEED_FREEWAY(IV)
 
! --- Initialize the timer for the next lane change.
 
  MUST_MERGE(IV) = .FALSE.
  FLC_TIMER(IV) = LC_TIME_FREEWAY
 
! --- Clear any pseudo leader the vehicle may have had.
 
  FPSEUDO_LEADER(IV) = 0
 
! --- Increment the number of lane changes on the link.
 
  IL = FLINK(IV)
  IF(.NOT. INITMODE) THEN
    IF(GOAL .NE. 0 .AND. FLANE(IV) .NE. 0) FLANE_CHANGES(IL) = FLANE_CHANGES(IL) + 1
  ENDIF
 
! --- If the vehicle is on the shoulder update the shoulder list.
     
  IF(FLANE(IV) .EQ. 0) THEN
    IF(IV .EQ. FFIRST_ON_SHOULDER(IL)) FFIRST_ON_SHOULDER(IL) = FFOLLOWER(IV)
  ENDIF
 
! --- Identify current leader and follower.
 
  CL = FLEADER(IV)
  CF = FFOLLOWER(IV)
 
! --- Update the chain in the lane being vacated.
 
  IF(CF .NE. 0) THEN
    IF(CL .NE. 0) THEN
      FLEADER(CF) = CL
    ELSE
      FLEADER(CF) = 0
    ENDIF
  ENDIF
 
  IF(CL .NE. 0) THEN
    IF(CF .NE. 0) THEN
      FFOLLOWER(CL) = CF
    ELSE
      FFOLLOWER(CL) = 0
    ENDIF
  ENDIF
 
! --- Update the chain in the new lane.
 
  IF(IFL .NE. 0) THEN           
    FLEADER(IFL) = IV
    FFOLLOWER(IV) = IFL
  ELSE
    FFOLLOWER(IV) = 0
  ENDIF
 
  IF(ILD .NE. 0) THEN
    FLEADER(IV) = ILD
    FFOLLOWER(ILD) = IV
  ELSE
    FLEADER(IV) = 0
  ENDIF
 
! --- Move the vehicle into the goal lane.
 
  FLANE(IV) = GOAL
  REMAINING_DIST(IV) = 0
  RETURN
  END

! ==================================================================================================
  SUBROUTINE STREET_CHANGE_LANE(IV, GOAL, ILD, IFL)
! ----------------------------------------------------------------------
! --- Perform the lane change.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE GLOBAL_DATA
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, GOAL, ILD, IFL
  INTEGER :: CL, CF, IL
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif
 
! --- Initialize the timer for the next lane change.
 
  SLC_TIMER(IV) = LC_TIME_STREET
 
! --- Clear any pseudo leader the vehicle may have had.
 
  SPSEUDO_LEADER(IV) = 0
 
! --- Increment the number of lane changes on the link.
 
  IL = SLINK(IV)
  IF(.NOT. INITMODE) THEN
    IF(GOAL .NE. 0 .AND. SLANE(IV) .NE. 0) THEN
 
! --- Do not count a move into a turn pocket as a lane change.
 
      IF(GOAL .GE. FIRST_FULL_LANE(IL) .AND. GOAL .LE. LAST_FULL_LANE(IL)) SLANE_CHANGES(IL) = SLANE_CHANGES(IL) + 1
    ENDIF
  ENDIF
 
! --- Update first and last vehicles in each lane, if necessary.
 
  IF(SLANE(IV) .NE. 0) THEN
    IF(SLAST_VEHICLE(IL, SLANE(IV)) .EQ. IV) SLAST_VEHICLE(IL, SLANE(IV)) = SLEADER(IV)
    IF(FIRST_VEHICLE(IL, SLANE(IV)) .EQ. IV) FIRST_VEHICLE(IL, SLANE(IV)) = SFOLLOWER(IV)
  ENDIF
 
! --- If the vehicle is on the shoulder update the shoulder list.
     
  IF(SLANE(IV) .EQ. 0) THEN
    IF(IV .EQ. SFIRST_ON_SHOULDER(IL)) SFIRST_ON_SHOULDER(IL) = SFOLLOWER(IV)
  ENDIF
 
! --- Identify current leader and follower.
 
  CL = SLEADER(IV)
  CF = SFOLLOWER(IV)
 
! --- Update the chain in the lane being vacated.
 
  IF(CF .NE. 0) THEN
    IF(CL .NE. 0) THEN
      SLEADER(CF) = CL
    ELSE
      SLEADER(CF) = 0
    ENDIF
  ENDIF
 
  IF(CL .NE. 0) THEN
    IF(CF .NE. 0) THEN
      SFOLLOWER(CL) = CF
    ELSE
      SFOLLOWER(CL) = 0
    ENDIF
  ENDIF
 
! --- Update the chain in the new lane.
 
  IF(IFL .NE. 0) THEN           
    SLEADER(IFL) = IV
    SFOLLOWER(IV) = IFL
  ELSE
    SFOLLOWER(IV) = 0
  ENDIF
 
  IF(ILD .NE. 0) THEN
    SLEADER(IV) = ILD
    SFOLLOWER(ILD) = IV
  ELSE
    SLEADER(IV) = 0
  ENDIF
 
! --- Move the vehicle into the goal lane.
 
  SLANE(IV) = GOAL
 
! --- Update first and last vehicle arrays if necessary.
 
  IF(GOAL .NE. 0) THEN
    IF(SLEADER(IV) .EQ. 0) FIRST_VEHICLE(IL, GOAL) = IV
    IF(SFOLLOWER(IV) .EQ. 0) SLAST_VEHICLE(IL, GOAL) = IV
  ENDIF
  
  RETURN
  END

! ==================================================================================================
  SUBROUTINE FREEWAY_DISC_LANECHANGE(IV)
! ----------------------------------------------------------------------
! --- Determine if the vehicle wants to perform a discretionary lane
! --- change.
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE GLOBAL_DATA
  USE SEEDS
  USE VEHICLE_TYPES
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER :: LEFT, RIGHT, GOAL, IL
  REAL :: RNDNUM, TLRSP, DESIRE, HDWY, HDWY_LEFT, HDWY_RIGHT, ACCEL
  LOGICAL :: WBAD
  !!!!! HMIN does not relate to any CORSIM input
  !!!!! It is the minimum headway difference to trigger a lane change
  !!!!! CORSIM uses an advantage factor
  !!!!! This is an arbitrary number used for testing.
  !!!!! The advantage factor in CORSIM is calculated as 
  !!!!! (HDWY - 2) / 3, and must be > 0.4 to perform a discretionary
  !!!!! lane change.
! ----------------------------------------------------------------------
  IF(FLC_TIMER(FLEADER(IV)) .GT. 0) RETURN
  IF(NODE_TYPE(FUSN(FLINK(IV))) .EQ. NT_EXTERN) RETURN
      
  IL = FLINK(IV)
 
! --- Determine the vehicle's minimum tolerable speed based on driver type.
 
  TLRSP = FDESIREDSPEED(IV) * (0.5 + 0.02 * FDRIVERTYPE(IV))
 
! --- Calculate a desire factor.
      
  IF(FSPEED(IV) .LT. TLRSP) THEN
    DESIRE = 1.0
  ELSE
    DESIRE = 1.0 - ((FSPEED(IV) - TLRSP)/(FDESIREDSPEED(IV) - TLRSP))
    DESIRE = DLC_MULT * DESIRE / SQRT(10. / FDRIVERTYPE(IV))
  ENDIF
 
! --- If the vehicle is a truck and the link has a truck bias, determine
! --- if the truck is out of the bias lanes. If it is set the desire to
! --- the maximum value.
 
  IF(FFLEET(IV) .EQ. FLEET_TRUCK) THEN
    IF(TRUCK_CODE(IL) .EQ. TRK_BIASED .OR. TRUCK_CODE(IL) .EQ. TRK_BIASED_EXCL) THEN
      CALL CHECK_TRUCK_LANES(IV, IL, FLANE(IV), WBAD)
      IF(WBAD) DESIRE = 1.0
    ENDIF
  ENDIF
 
! --- Always try to move out of an exclusion lane.
      
  IF(FXCLUDE_TYPE(FLINK(IV), FLANE(IV), FVTYPE(IV))) DESIRE = 1.0
 
! --- Do not allow a lane change close to a rampmeter.
      
  IF(RAMPMETER(FLINK(IV)) .NE. 0) THEN
    IF(FLENGTH(IL) - FLOCATION(IV) .LT. 100) DESIRE = 0.0
  ENDIF
 
! --- Compare the desire to a random number.
 
  CALL FREEWAY_RANDOM(FSEED, RNDNUM)
  IF(DESIRE .GT. RNDNUM) THEN
 
! --- If the vehicle is an EV, do not allow a discretionary lane change
! --- unless the leader is close enough to affect the speed of the EV.
        
    IF(FFLEET(IV) .EQ. FLEET_EV) THEN
      ACCEL = 0.
      CALL FREEWAY_CAR_FOLLOW(IV, FLEADER(IV), ACCEL)
      IF(ACCEL .GE. 0.) RETURN
    ENDIF
 
! --- Determine the headway in the current lane.
      
    IF(FSPEED(IV) .GT. 0) THEN
      CALL FREEWAY_CALCULATE_HEADWAY(IV, FLANE(IV), HDWY)
    ELSE
      HDWY = 0.
    ENDIF 
    
! --- Determine the headway in the lane to the left.
       
    HDWY_LEFT = 0.
    LEFT = FREEWAY_LANE_TO_LEFT(FLANE(IV))
    IF(LEFT .NE. 0) THEN
      IF(FFLEET(IV) .EQ. FLEET_EV) THEN
        IF(FLANECODES(IV, LEFT) .EQ. LC_NULL) LEFT = 0
      ELSE
        IF(FLANECODES(IV, LEFT) .NE. LC_GOOD) LEFT = 0
      ENDIF
    ENDIF
 
! --- Do not allow a lane change from an exclusive HOV lane into
! --- a lane that is not within the HOV facility.
 
    IF(NHOV_LANES(IL) .NE. 0) THEN
      IF(HOV_TYPE(IL) .EQ. 1) THEN
        IF(FLANE(IV) .EQ. HOV_LANES(IL, 1) .OR. &
           FLANE(IV) .EQ. HOV_LANES(IL, 2) .OR. &
           FLANE(IV) .EQ. HOV_LANES(IL, 3)) THEN
          IF(LEFT .NE. HOV_LANES(IL, 1) .AND. &
             LEFT .NE. HOV_LANES(IL, 2) .AND. &
             LEFT .NE. HOV_LANES(IL, 3)) THEN
            LEFT = 0
          ENDIF
        ENDIF
      ENDIF
    ENDIF
 
! --- Do not allow a lane change into an exclusive HOV lane from
! --- a lane that is not within the HOV facility.
 
    IF(NHOV_LANES(IL) .NE. 0) THEN
      IF(HOV_TYPE(IL) .EQ. 1) THEN
        IF(FLANE(IV) .NE. HOV_LANES(IL, 1) .AND. &
           FLANE(IV) .NE. HOV_LANES(IL, 2) .AND. &
           FLANE(IV) .NE. HOV_LANES(IL, 3)) THEN
          IF(LEFT .EQ. HOV_LANES(IL, 1) .OR. &
             LEFT .EQ. HOV_LANES(IL, 2) .OR. &
             LEFT .EQ. HOV_LANES(IL, 3)) THEN
            LEFT = 0
          ENDIF
        ENDIF
      ENDIF
    ENDIF
 
! --- Do not allow a lane change across a barrier.
 
    IF(BARRIER(IL, 1) .EQ. FLANE(IV)) LEFT = 0
    IF(BARRIER(IL, 2) .EQ. FLANE(IV)) LEFT = 0
        
    IF(LEFT .NE. 0) CALL FREEWAY_CALCULATE_HEADWAY(IV, LEFT, HDWY_LEFT)
 
! --- Determine the headway in the lane to the right.
      
    HDWY_RIGHT = 0.
    RIGHT = FREEWAY_LANE_TO_RIGHT(FLANE(IV))
    IF(RIGHT .NE. 0) THEN
      IF(FFLEET(IV) .EQ. FLEET_EV) THEN
        IF(FLANECODES(IV, RIGHT) .EQ. LC_NULL) RIGHT = 0
      ELSE
        IF(FLANECODES(IV, RIGHT) .NE. LC_GOOD) RIGHT = 0
      ENDIF
    ENDIF
 
! --- Do not allow a lane change from an exclusive HOV lane into
! --- a lane that is not within the HOV facility.
 
    IF(NHOV_LANES(IL) .NE. 0) THEN
      IF(HOV_TYPE(IL) .EQ. 1) THEN
        IF(FLANE(IV) .EQ. HOV_LANES(IL, 1) .OR. &
           FLANE(IV) .EQ. HOV_LANES(IL, 2) .OR. &
           FLANE(IV) .EQ. HOV_LANES(IL, 3)) THEN
         IF(RIGHT .NE. HOV_LANES(IL, 1) .AND. &
            RIGHT .NE. HOV_LANES(IL, 2) .AND. &
            RIGHT .NE. HOV_LANES(IL, 3)) THEN
            RIGHT = 0
          ENDIF
        ENDIF
      ENDIF
    ENDIF
 
! --- Do not allow a lane change into an exclusive HOV lane from
! --- a lane that is not within the HOV facility.
 
    IF(NHOV_LANES(IL) .NE. 0) THEN
      IF(HOV_TYPE(IL) .EQ. 1) THEN
        IF(FLANE(IV) .NE. HOV_LANES(IL, 1) .AND. &
           FLANE(IV) .NE. HOV_LANES(IL, 2) .AND. &
           FLANE(IV) .NE. HOV_LANES(IL, 3)) THEN
          IF(RIGHT .EQ. HOV_LANES(IL, 1) .OR. &
             RIGHT .EQ. HOV_LANES(IL, 2) .OR. &
             RIGHT .EQ. HOV_LANES(IL, 3)) THEN
            RIGHT = 0
          ENDIF
        ENDIF
      ENDIF
    ENDIF

! --- Do not allow a lane change across a barrier.
 
    IF(BARRIER(IL, 1) .EQ. RIGHT) RIGHT = 0
    IF(BARRIER(IL, 2) .EQ. RIGHT) RIGHT = 0
 
    IF(RIGHT .NE. 0) CALL FREEWAY_CALCULATE_HEADWAY(IV, RIGHT, HDWY_RIGHT)
 
! --- If the vehicle is a truck and the link has a truck bias, adjust
! --- the headways to reflect the bias.
 
    IF(FFLEET(IV) .EQ. FLEET_TRUCK) THEN
      IF(TRUCK_CODE(IL) .EQ. TRK_BIASED .OR. TRUCK_CODE(IL) .EQ. TRK_BIASED_EXCL) THEN
        CALL CHECK_TRUCK_LANES(IV, IL, LEFT, WBAD)
        IF(WBAD) HDWY_LEFT = HDWY_LEFT - 0.3
        CALL CHECK_TRUCK_LANES(IV, IL, RIGHT, WBAD)
        IF(WBAD) HDWY_RIGHT = HDWY_RIGHT - 0.3
      ENDIF
    ENDIF
 
! --- If the vehicle is a bus, bias it toward the right.
        
    IF(FFLEET(IV) .EQ. FLEET_BUS .AND. RIGHT .NE. 0) THEN
      IF(FLANECODES(IV, RIGHT) .EQ. LC_GOOD) HDWY_RIGHT = HDWY_RIGHT + 0.3
    ENDIF
 
! --- Check for lane restriction.
 
    IF(LEFT .NE. 0) THEN
      IF(FXCLUDE_TYPE(FLINK(IV), LEFT, FVTYPE(IV))) HDWY_LEFT = HDWY_LEFT - 0.3
    ENDIF
    IF(RIGHT .NE. 0) THEN
      IF(FXCLUDE_TYPE(FLINK(IV), RIGHT, FVTYPE(IV))) HDWY_RIGHT = HDWY_RIGHT - 0.3
    ENDIF
 
! --- Determine if there is an advantage in moving into the left or right lane.
       
    IF(MAX(HDWY_LEFT, HDWY_RIGHT) - HDWY .GT. HMIN) THEN
      IF(HDWY_LEFT .GT. HDWY_RIGHT) THEN
        GOAL = LEFT
      ELSEIF(HDWY_LEFT .LT. HDWY_RIGHT) THEN
        GOAL = RIGHT
      ELSE
        CALL FREEWAY_RANDOM(FSEED, RNDNUM)
        IF(RNDNUM .LT. 0.5) THEN
          GOAL = LEFT
        ELSE
          GOAL = RIGHT
        ENDIF
      ENDIF
 
! --- Attempt to perform a lane change into the goal lane.
       
      CALL TRY_FREEWAY_LANECHANGE(IV, GOAL)
    ENDIF
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE STREET_DISC_LANECHANGE(IV)
! ----------------------------------------------------------------------
! --- Determine if the vehicle wants to perform a discretionary lane
! --- change.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE GLOBAL_DATA
  USE SEEDS
  USE VEHICLE_TYPES
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER :: LEFT, RIGHT, GOAL, IL
  REAL :: RNDNUM, TLRSP, DESIRE, HDWY, HDWY_LEFT, HDWY_RIGHT
  !!!!! HMIN does not relate to any CORSIM input
  !!!!! It is the minimum headway difference to trigger a lane change
  !!!!! CORSIM uses an advantage factor
  !!!!! This is an arbitrary number used for testing.
  !!!!! The advantage factor in CORSIM is calculated as 
  !!!!! (HDWY - 2) / 3, and must be > 0.4 to perform a discretionary
  !!!!! lane change.
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif
  if(sfleet(iv) .eq. fleet_bike) return
  IF(NODE_TYPE(SUSN(SLINK(IV))) .EQ. NT_EXTERN) RETURN
 
! --- Do not consider a discretionary lane change when the subject vehicle
! --- has a goal lane, or if the vehicle or it's leader has just performed
! --- a lane change.
 
  IF(SLC_TIMER(SLEADER(IV)) .GT. 0) RETURN
  IF(SFLEET(IV) .EQ. FLEET_BUS) RETURN
      
  IL = SLINK(IV)
  IF(UP_INT_WIDTH(IL) .NE. 0) THEN
    IF(SLOCATION(IV) .LT. UP_INT_WIDTH(IL)) RETURN
  ENDIF
 
! --- Determine the vehicle's minimum tolerable speed based on driver type.
 
  TLRSP = SDESIREDSPEED(IV) * (0.5 + 0.02 * SDRIVERTYPE(IV))
 
! --- Calculate a desire factor.
       
  IF(SSPEED(IV) .LT. TLRSP) THEN
    DESIRE = 1.0
  ELSE
    DESIRE = 1.0 -((SSPEED(IV) - TLRSP)/(SDESIREDSPEED(IV) - TLRSP))
    DESIRE = DLC_MULT * DESIRE / SQRT(10. / SDRIVERTYPE(IV))
  ENDIF
 
  IF(SXCLUDE_TYPE(SLINK(IV), SLANE(IV), SVTYPE(IV))) DESIRE = 1.0
 
! --- Compare the desire to a random number.
 
  CALL STREET_RANDOM(SSEED, RNDNUM)
  IF(DESIRE .GT. RNDNUM) THEN
 
! --- Determine the headway in the current lane.
      
    IF(SSPEED(IV) .GT. 0) THEN
      CALL STREET_CALCULATE_HEADWAY(IV, SLANE(IV), HDWY)
    ELSE
      HDWY = 0.
    ENDIF
    
! --- Determine the headway in the lane to the left.
      
    HDWY_LEFT = 0.
    LEFT = 0
    IF(SLANE(IV) .LT. LAST_FULL_LANE(IL)) LEFT = SLANE(IV) + 1
    IF(LEFT .NE. 0) THEN
      IF(SLANECODES(IV, LEFT) .NE. LC_GOOD) LEFT = 0
    ENDIF  
    IF(LEFT .NE. 0) CALL STREET_CALCULATE_HEADWAY(IV, LEFT, HDWY_LEFT)
 
! --- Determine the headway in the lane to the right.
      
    HDWY_RIGHT = 0.
    RIGHT = 0
    IF(SLANE(IV) .GT. FIRST_FULL_LANE(IL)) RIGHT = SLANE(IV) - 1
    IF(RIGHT .NE. 0) THEN
      IF(SLANECODES(IV, RIGHT) .NE. LC_GOOD) RIGHT = 0
    ENDIF
    IF(RIGHT .NE. 0) CALL STREET_CALCULATE_HEADWAY(IV, RIGHT, HDWY_RIGHT)
 
! --- Check for lane restriction.
 
    IF(LEFT .NE. 0) THEN
      IF(SXCLUDE_TYPE(SLINK(IV), LEFT, SVTYPE(IV))) HDWY_LEFT = HDWY_LEFT - 0.3
    ENDIF
    IF(RIGHT .NE. 0) THEN
      IF(SXCLUDE_TYPE(SLINK(IV), RIGHT, SVTYPE(IV))) HDWY_RIGHT = HDWY_RIGHT - 0.3
    ENDIF
 
! --- Determine if there is an advantage in moving into the left or right lane.
       
    IF(MAX(HDWY_LEFT, HDWY_RIGHT) - HDWY .GT. HMIN) THEN
      IF(HDWY_LEFT .GT. HDWY_RIGHT) THEN
        GOAL = LEFT
      ELSEIF(HDWY_LEFT .LT. HDWY_RIGHT) THEN
        GOAL = RIGHT
      ELSE
        CALL STREET_RANDOM(SSEED, RNDNUM)
        IF(RNDNUM .LT. 0.5) THEN
          GOAL = LEFT
        ELSE
          GOAL = RIGHT
        ENDIF
      ENDIF
 
! --- Attempt to perform a lane change into the goal lane.
      
      CALL TRY_STREET_LANECHANGE(IV, GOAL)

    ENDIF
  ENDIF
  RETURN
  END

  SUBROUTINE STREET_POCKET_LANECHANGE(IV, GOAL)
! ----------------------------------------------------------------------
! --- Determine if the vehicle wants to perform a discretionary lane
! --- change into a different pocket lane
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE GLOBAL_DATA
  USE VEHICLE_TYPES
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, GOAL
  INTEGER :: IL
  REAL :: HDWY, HDWY_GOAL
  !!!!! HMIN does not relate to any CORSIM input
  !!!!! It is the minimum headway difference to trigger a lane change
  !!!!! CORSIM uses an advantage factor
  !!!!! This is an arbitrary number used for testing.
  !!!!! The advantage factor in CORSIM is calculated as 
  !!!!! (HDWY - 2) / 3, and must be > 0.4 to perform a discretionary
  !!!!! lane change.
! ----------------------------------------------------------------------
 
! --- Do not consider a discretionary lane change when the subject vehicle
! --- has a goal lane, or if the vehicle or it's leader has just performed
! --- a lane change.
 
  IF(SLC_TIMER(SLEADER(IV)) .GT. 0) RETURN
  IF(SFLEET(IV) .EQ. FLEET_BUS) RETURN
       
  IL = SLINK(IV)
 
! --- Determine the headway in the current lane.
       
  CALL STREET_CALCULATE_HEADWAY(IV, SLANE(IV), HDWY)
 
! --- Determine the headway in the goal lane.
      
  HDWY_GOAL = 0.
  CALL STREET_CALCULATE_HEADWAY(IV, GOAL, HDWY_GOAL)
 
! --- Check for lane restriction.
 
  IF(SXCLUDE_TYPE(SLINK(IV), GOAL, SVTYPE(IV))) HDWY_GOAL = HDWY_GOAL - 0.3
 
! --- Determine if there is an advantage in moving into the goal lane.
       
  IF(HDWY_GOAL - HDWY .GT. HMIN) THEN
  
! --- Attempt to perform a lane change into the goal lane.
       
    CALL TRY_STREET_LANECHANGE(IV, GOAL)
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE FREEWAY_CALCULATE_HEADWAY(IV, ILANE, HDWY)
! ----------------------------------------------------------------------
! --- Calculate the headway between a vehicle and the back bumper of its
! --- leader. 
! ----------------------------------------------------------------------
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, ILANE
  REAL, INTENT(OUT) :: HDWY
  INTEGER :: ILD, IPX, IFL
! ----------------------------------------------------------------------
 
! --- The leader may be in the same lane or in an adjacent lane.
! --- Set the headway to a large number in case there is no leader.
 
  HDWY = 100.
  IF(FSPEED(IV) .GT. 0) THEN
 
! --- Find the leader in the specified lane.
 
    IPX = SORT_POSITION(IV)
    CALL FIND_FREEWAY_LEADER(IV, IPX, ILANE, ILD)
    IF(ILD .NE. 0) THEN
      IFL = FFOLLOWER(ILD)
    ELSE
      CALL FIND_FREEWAY_FOLLOWER(IV, ILANE, IFL)
    ENDIF
 
! --- Divide the distance to the leader's back bumper by the speed
! --- of the follower.
 
    IF(ILD .NE. 0) THEN
      HDWY = (DISTANCE_TO_SEGMENT_END(IV) - DISTANCE_TO_SEGMENT_END(ILD) - FVLENGTH(ILD)) / FSPEED(IV)
      HDWY = MAX(HDWY, 0.)
    ENDIF
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE STREET_CALCULATE_HEADWAY(IV, ILANE, HDWY)
! ----------------------------------------------------------------------
! --- Calculate the headway between a vehicle and the back bumper of its
! --- leader. 
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, ILANE
  REAL, INTENT(OUT) :: HDWY
  INTEGER :: ILD
! ----------------------------------------------------------------------
 
! --- The leader may be in the same lane or in an adjacent lane.
! --- Set the headway to a large number in case there is no leader.
 
  HDWY = 100.
 
! --- Find the leader in the specified lane.
 
  IF(ILANE .EQ. SLANE(IV)) THEN
    ILD = SLEADER(IV)
  ELSE
    CALL FIND_STREET_LEADER(IV, SLINK(IV), ILANE, ILD)
  ENDIF
 
! --- Divide the distance to the leader's back bumper by the speed
! --- of the follower.
 
  IF(ILD .NE. 0) THEN
    IF(SLOCATION(ILD) - SVLENGTH(ILD) - SLOCATION(IV) .LT. 20) THEN
      HDWY = 0.
    ELSEIF(SSPEED(IV) .GT. STOP_SPD) THEN
      HDWY = (SLOCATION(ILD) - SVLENGTH(ILD) - SLOCATION(IV)) / SSPEED(IV)
      HDWY = MAX(HDWY, 0.)
    ELSE
      HDWY = (SLOCATION(ILD) - SVLENGTH(ILD) - SLOCATION(IV)) / STOP_SPD
      HDWY = MAX(HDWY, 0.)
    ENDIF
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE RESET_TURNCODE(IV)
! ----------------------------------------------------------------------
! --- Choose a new turn code.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER :: ICODE, IL, ILN, I, ITURN, ILINK
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif
 
! --- When a vehicle reaches the end of its link and has failed to get
! --- into a correct lane for its assigned turn movement, choose
! --- a different turn movement that the lane allows.
 
  SPATHID(IV) = 0
  TURN_CODE2(IV) = TC_NULL
  IL = SLINK(IV)
  ILN = SLANE(IV)
  ICODE = CHANNELIZATION(IL, ILN)
  IF(ICODE .EQ. 0 .OR. ICODE .EQ. 9) THEN
 
! --- Unchannelized or channelized for all allowable movements.
 
    DO ITURN = TC_LEFT, TC_RDIAG
      IF(ITURN .EQ. TC_LEFT .AND. STURNCODE(IV) .NE. ITURN) THEN
        IF(ILN .EQ. LAST_FULL_LANE(IL) .AND. LEFT_LINK(IL) .NE. 0 .AND. &
           NUMBER_LEFTPOCKETS(IL) .EQ. 0 .AND. LEFT_PERCENT(IL) .NE. 0) THEN
          STURNCODE(IV) = TC_LEFT
          EXIT
        ENDIF
      ELSEIF(ITURN .EQ. TC_THRU .AND. STURNCODE(IV) .NE. ITURN) THEN
        IF(STHRU_LINK(IL) .NE. 0 .AND. STHRU_PERCENT(IL) .NE. 0) THEN
          STURNCODE(IV) = TC_THRU
          EXIT
        ENDIF
      ELSEIF(ITURN .EQ. TC_RIGHT .AND. STURNCODE(IV) .NE. ITURN) THEN            
        IF(ILN .EQ. 1 .AND. RIGHT_LINK(IL) .NE. 0 .AND. NUMBER_RIGHTPOCKETS(IL) .EQ. 0 .AND. RIGHT_PERCENT(IL) .NE. 0) THEN
          STURNCODE(IV) = TC_RIGHT
          EXIT
        ENDIF
      ELSEIF(ITURN .EQ. TC_LDIAG .AND. STURNCODE(IV) .NE. ITURN .AND. LDIAG_PERCENT(IL) .NE. 0) THEN
        IF(ILN .EQ. LAST_FULL_LANE(IL) .AND. LEFT_DIAG_LINK(IL) .NE. 0) THEN
          STURNCODE(IV) = TC_LDIAG
          EXIT
        ENDIF
      ELSEIF(ITURN .EQ. TC_RDIAG .AND. STURNCODE(IV) .LE. ITURN .AND. RDIAG_PERCENT(IL) .NE. 0) THEN
        IF(ILN .EQ. FIRST_FULL_LANE(IL) .AND. RIGHT_DIAG_LINK(IL) .NE. 0) THEN
          STURNCODE(IV) = TC_RDIAG
          EXIT
        ENDIF
      ENDIF
    ENDDO
  ELSEIF(ICODE .EQ. 1) THEN

! --- Channelized for left turn only.

    STURNCODE(IV) = TC_LEFT
  ELSEIF(ICODE .EQ. 4) THEN

! --- Channelized for right turn only.
 
    STURNCODE(IV) = TC_RIGHT
  ELSEIF(ICODE .EQ. 10) THEN
 
! --- Channelized for diagonal only.
 
    STURNCODE(IV) = TC_RDIAG
    IF(LEFT_DIAG_LINK(IL) .NE. 0) STURNCODE(IV) = TC_LDIAG
    IF(RIGHT_DIAG_LINK(IL) .NE. 0) STURNCODE(IV) = TC_RDIAG
  ELSEIF(ICODE .EQ. 11) THEN
 
! --- Channelized for thru only.
 
    STURNCODE(IV) = TC_THRU
    
  ELSEIF(ICODE .EQ. 7) THEN
 
! --- The lane allows right, right diagonal and thru movement.
       
    IF(STURNCODE(IV) .NE. TC_THRU .AND. STHRU_PERCENT(IL) .NE. 0) THEN
      STURNCODE(IV) = TC_THRU
    ELSEIF(STURNCODE(IV) .NE. TC_RIGHT .AND. RIGHT_PERCENT(IL) .NE. 0) THEN
      STURNCODE(IV) = TC_RIGHT
    ELSEIF(STURNCODE(IV) .NE. TC_RDIAG .AND. RDIAG_PERCENT(IL) .NE. 0) THEN
      STURNCODE(IV) = TC_RDIAG
    ENDIF
    
  ELSEIF(ICODE .EQ. 8) THEN
 
! --- The lane allows left, left diagonal and thru movement.
       
    IF(STURNCODE(IV) .NE. TC_THRU .AND. STHRU_PERCENT(IL) .NE. 0) THEN
      STURNCODE(IV) = TC_THRU
    ELSEIF(STURNCODE(IV) .NE. TC_LEFT .AND. LEFT_PERCENT(IL) .NE. 0) THEN
      STURNCODE(IV) = TC_LEFT
    ELSEIF(STURNCODE(IV) .NE. TC_LDIAG .AND. LDIAG_PERCENT(IL) .NE. 0) THEN
      STURNCODE(IV) = TC_LDIAG
    ENDIF
    
  ENDIF
  
! --- Reset codes to force the vehicle to use the current lane.
 
  DO I = 1, N_STREET_LANES
    IF(SLANECODES(IV, I) .EQ. LC_GOOD) SLANECODES(IV, I) = LC_VACATE
  ENDDO
  SLANECODES(IV, ILN) = LC_GOOD
  GOAL_LANE(IV) = 0
  
  IF(STURNCODE(IV) .EQ. TC_THRU) THEN
    IF(ROUNDABOUT_ID(IL) .NE. 0 .AND. ROUNDABOUT_APPROACH_NUM(IL) .EQ. 0) THEN
      ILINK = STHRU_LINK(IL)
      DO WHILE(ILINK .NE. 0 .AND. ILINK .NE. IL)
        IF(RIGHT_LINK(ILINK) .NE. 0) THEN
          TURN_LINK(IV) = ILINK
          TURN_CODE(IV) = TC_RIGHT
          EXIT
        ELSEIF(RIGHT_DIAG_LINK(ILINK) .NE. 0) THEN
          TURN_LINK(IV) = ILINK
          TURN_CODE(IV) = TC_RDIAG
          EXIT
        ELSE
          ILINK = STHRU_LINK(ILINK)
        ENDIF
      ENDDO
    ELSE
 
! --- Temporarily set the link to the thru receiving link to find
! --- the next turn link. After finding the next turn link and turn code
! --- set link and turn code back.
 
      SLINK(IV) = STHRU_LINK(IL)
      CALL FIND_NEXT_TURN(IV)
      SLINK(IV) = IL
      STURNCODE(IV) = TC_THRU
    ENDIF
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE ENTER_TURNING_WAY(IV)
! ----------------------------------------------------------------------
! --- Determine if the vehicle can enter the turning way.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE GLOBAL_DATA
  USE DRIVERS
  USE STREET_VEHICLES
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER :: IL, ILD
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

! --- Determine if the subject vehicle can enter the turning way.
 
  IL = SLINK(IV)
  IF(SLOCATION(IV) .GE. RTW_EXIT_POINT(IL) + 12 * RTW_LANES(IL)) THEN
    IN_TURNING_WAY(IV) = .FALSE.
  ELSEIF(SLOCATION(IV) + SSPEED(IV) * TIMESTEP .GE. RTW_EXIT_POINT(IL)) THEN
    IN_TURNING_WAY(IV) = .TRUE.
    CALL TRY_STREET_LANECHANGE(IV, 1)
    IF(SLANE(IV) .EQ. 1) THEN
      ARC_LENGTH(IV) = RTW_LENGTH(IL)
      ARC_LOCATION(IV) = SLOCATION(IV) - RTW_EXIT_POINT(IL)
      SDESIREDSPEED(IV) = RTW_FFSPEED(IL) * FFSPEED_ADJ(SDRIVERTYPE(IV), I_STREET)
      SDESIREDSPEED(IV) = MIN(SDESIREDSPEED(IV), 110.)
      SLANECODES(IV, 1) = LC_GOOD
    ELSE
      IN_TURNING_WAY(IV) = .FALSE.
    ENDIF
  ELSE
    !The vehicle is approaching the turning way.
    !Look for the last vehicle in the turning way and car follow it.
    CALL FIND_STREET_LEADER(IV, IL, 1, ILD)
    SPSEUDO_LEADER(IV) = ILD
  ENDIF
  RETURN
  END

  
  

