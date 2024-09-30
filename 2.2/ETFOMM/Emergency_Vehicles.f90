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

! ==========================================================================================================
  SUBROUTINE FREEWAY_EVSIM
!
!
! --- TITLE - PROCESS EMERGENCY VEHICLE INTERACTIONS ON FREEWAYS
!
! --- FUNCTION - THIS MODULE PROCESSES ALL INTERACTIONS BETWEEN EMERGENCY
! ---            VEHICLES AND OTHER VEHICLES
! ----------------   GLOSSARY OF VARIABLE NAMES   --------------------
!                    --------------------------
  USE FREEWAY_LINKS
  USE FREEWAY_VEHICLES
  USE SIMPARAMS
  USE SEGMENTS
  USE EV_DATA
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER :: IL, IV, IEV, DIST, NFULL, JTLEAD, JTFLW, JV, VLEN, GAP
  INTEGER :: IRIGHT, IEVN, ISEG
  REAL :: EVBACK
  LOGICAL :: WMOVE
 
  FEV_ONLINK = .FALSE.
 
! --- Reset all other vehicles every time step.
! --- Since they continually check for the presence of an EV they
! --- don't need to remember awareness and cooperation.
 
  DO IV = 1, MAX_VEHICLES_F
    IF(FLINK(IV) .EQ. 0) CYCLE
    IL = FLINK(IV)
    IF(FWILL_COOP_EV(IV)) CALL FREEWAY_RESET_NEV(IV)
    IF(NODE_TYPE(FUSN(FLINK(IV))) .EQ. NT_EXTERN) CYCLE
 
! --- If there is an EV on the link set a flag.
         
    IF(FFLEET(IV) .EQ. FLEET_EV) FEV_ONLINK(IL) = .TRUE.
  ENDDO
 
! --- Loop over all vehicles and check for EVs.
 
  DO IEVN = 1, MAX_VEHICLES_F
    IEV = SORTED_LIST(IEVN)
    IF(IEV .EQ. 0) EXIT
    IF(FLINK(IEV) .EQ. 0) CYCLE
    IL = FLINK(IEV)
 
    NFULL = FNUMLANES(IL)                                         
    IF(FFLEET(IEV) .EQ. FLEET_EV) THEN
      EVBACK = DISTANCE_TO_SEGMENT_END(IEV) + FVLENGTH(IEV)
      EVBACK = MAX(EVBACK, 0.)
 
! --- Locate vehicles on the same segment as the EV.
! --- Notify them that an EV is present.
! --- If the non-EV is downstream from the EV check for awareness
! --- and cooperation.
! --- If the non-EV is upstream slow it down to give separation
! --- between it and the EV.
  
      IL = FLINK(IEV)
      ISEG = ISEGMENT(IEV)
      DO IV = 1, HIGHEST_INDEX_F
        IF(FID(IV) .EQ. 0) CYCLE
        IF(FFLEET(IV) .EQ. FLEET_EV) CYCLE
        IF(ISEGMENT(IV) .NE. ISEG) CYCLE
        IF(FNUMLANES(FLINK(IV)) .GT. 1 .OR. FLANE(IV) .NE. 1 .OR. FSHOULDER_WIDTH(IL) .GE. 10) THEN
          IF(DISTANCE_TO_SEGMENT_END(IV) .LT. EVBACK) THEN
 
! --- The EV is upstream. Determine if the vehicle will cooperate with it.
                 
            DIST = DISTANCE_TO_SEGMENT_END(IV) - EVBACK
            IF(DIST .LE. 1000) THEN
              WMOVE = .TRUE.
              CALL FREEWAY_DETECT_EV(IV, IEV, DIST, WMOVE)
            ENDIF
 
! --- The EV is downstream. Determine if the vehicle will cooperate with it.
                 
          ELSE
            DIST = EVBACK - DISTANCE_TO_SEGMENT_END(IV)
            IF(DIST .LE. 1000) THEN
              WMOVE = .FALSE.
              CALL FREEWAY_DETECT_EV(IV, IEV, DIST, WMOVE)
            ENDIF
          ENDIF
        ENDIF
      ENDDO
 
! --- Process the vehicles that are already on the shoulder.
           
      WMOVE = .FALSE.
      IV = FFIRST_ON_SHOULDER(IL)
      DO WHILE(IV .NE. 0)
        IF(DISTANCE_TO_SEGMENT_END(IV) .LT. EVBACK) THEN
          IF(EVBACK - DISTANCE_TO_SEGMENT_END(IV) .LE. 1000) THEN
            DIST = EVBACK - DISTANCE_TO_SEGMENT_END(IV)
            WMOVE = .FALSE.
            CALL FREEWAY_DETECT_EV(IV, IEV, DIST, WMOVE)
          ENDIF
        ELSEIF(DISTANCE_TO_SEGMENT_END(IV) - DISTANCE_TO_SEGMENT_END(IEV) .LE. 200) THEN
          FEV_WAIT_TIMER(IV) = MAX(FEV_WAIT_TIMER(IV), 3.)
        ENDIF
        IV = FFOLLOWER(IV)
      ENDDO
    ENDIF
  ENDDO
 
! --- Process lane changes between the rightmost lane and the shoulder
! --- for vehicles that are cooperating with an EV.
 
  DO IV = 1, MAX_VEHICLES_F
    IL = FLINK(IV)
    IF(IL .EQ. 0) CYCLE
    IF(FFLEET(IV) .EQ. FLEET_EV) CYCLE
    NFULL = FNUMLANES(IL)
    IF(FLANE(IV) .EQ. 0) FLC_TIMER(IV) = FLC_TIMER(IV) - TIMESTEP
    IF(FLANECODES(IV, 11) .NE. LC_NULL) THEN
      IRIGHT = 11
    ELSEIF(FLANECODES(IV, 10) .NE. LC_NULL) THEN
      IRIGHT = 10
    ELSEIF(FLANECODES(IV, 9) .NE. LC_NULL) THEN
      IRIGHT = 9
    ELSE
      IRIGHT = 1
    ENDIF
    IF(FWILL_COOP_EV(IV) .AND. FSHOULDER_WIDTH(IL) .GE. 10 .AND. FLANE(IV) .EQ. IRIGHT) THEN
 
! --- The vehicle is cooperating and there is a shoulder.
! --- The vehicle is in the rightmost lane.
      
      IF(FSPEED(IV) .LE. 12) THEN
 
! --- When the vehicle's speed has slowed enough, try to perform
! --- a lane change onto the shoulder.
           
        JTLEAD = 0
        JTFLW = 0
        IF(FFIRST_ON_SHOULDER(IL) .NE. 0) THEN
          JV = FFIRST_ON_SHOULDER(IL)
          DO WHILE(JV .NE. 0)
            IF(DISTANCE_TO_SEGMENT_END(JV) .LT. DISTANCE_TO_SEGMENT_END(IV)) THEN
              VLEN = FVLENGTH(JV)
              JTLEAD = JV
              JTFLW = FFOLLOWER(JV)
            ELSE
              JTFLW = JV
              EXIT
            ENDIF
            JV = FFOLLOWER(JV)
          ENDDO
          IF(JTLEAD .EQ. 0) JTFLW = FFIRST_ON_SHOULDER(IL)
        ENDIF
 
! --- Locate a gap between vehicles on the shoulder.
 
        IF(JTLEAD .EQ. 0 .AND. JTFLW .EQ. 0) THEN
          GAP = 1000
        ELSEIF(JTLEAD .NE. 0) THEN
          GAP = 1000
          IF(DISTANCE_TO_SEGMENT_END(JTLEAD) + FVLENGTH(JTLEAD) .GT. DISTANCE_TO_SEGMENT_END(IV)) GAP = 0
        ELSEIF(JTFLW .NE. 0) THEN
          GAP = 1000
          IF(DISTANCE_TO_SEGMENT_END(IV) + FVLENGTH(IV) .GT. DISTANCE_TO_SEGMENT_END(JTFLW)) GAP = 0
        ENDIF
 
! --- If the gap is big enough move into it.
             
        IF(GAP .GT. 1.5 * VLEN) THEN  
          FSPEED(IV) = 0
          FLANE(IV) = 0
          IF(JTLEAD .EQ. 0) FFIRST_ON_SHOULDER(IL) = IV
          IF(FLEADER(IV) .NE. 0) FFOLLOWER(FLEADER(IV)) = FFOLLOWER(IV)
          IF(FFOLLOWER(IV) .NE. 0) FLEADER(FFOLLOWER(IV)) = FLEADER(IV)
          FLEADER(IV) = JTLEAD
          FFOLLOWER(IV) = JTFLW
          IF(JTLEAD .NE. 0) FFOLLOWER(JTLEAD) = IV
          IF(JTFLW .NE. 0) FLEADER(JTFLW) = IV
          FEV_WAIT_TIMER(IV) = 5
          FLOCATION(IV) = MIN(FLOCATION(IV) + 3, FLOAT(FLENGTH(IL)))
          IF(JTLEAD .NE. 0) THEN
            DISTANCE_TO_SEGMENT_END(IV) = MIN(DISTANCE_TO_SEGMENT_END(IV), &
                                          DISTANCE_TO_SEGMENT_END(JTLEAD) - FVLENGTH(JTLEAD))
          ENDIF
          FLC_TIMER(IV) = MAX(FLC_TIMER(IV), 3.)
        ENDIF
      ENDIF
    ELSEIF(.NOT. FWILL_COOP_EV(IV) .AND. FLANE(IV) .EQ. 0 .AND. FEV_WAIT_TIMER(IV) .LE. 0) THEN

! --- The vehicle is on the shoulder, the EV has passed and the vehicle
! --- has waited long enough. Try to perform a lane change from the shoulder.

      CALL TRY_FREEWAY_LANECHANGE(IV, IRIGHT)
    ENDIF

! --- Decrement the wait timer.
        
    FEV_WAIT_TIMER(IV) = FEV_WAIT_TIMER(IV) - TIMESTEP
  ENDDO
  RETURN
  END

! ==========================================================================================================
  SUBROUTINE STREET_EVSIM
!
! --- TITLE - PROCESS EMERGENCY VEHICLE INTERACTIONS
!
! --- FUNCTION - THIS MODULE PROCESSES ALL INTERACTIONS BETWEEN EMERGENCY
! ---            VEHICLES AND OTHER VEHICLES
!
  USE STREET_LINKS
  USE STREET_VEHICLES
  USE TIMED_CONTROLLERS
  USE SIMPARAMS
  USE EV_DATA
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER :: IL, IV, IEV, DNODE, DIST, IRL, NFULL, ILINK, ILANE, IAP
  INTEGER :: IREC, IUP, IDN, ILOP, LNKLEN, ILN, INODE, ISIG 
  INTEGER :: JTLEAD, JTFLW, JV, VLEN, GAP
  INTEGER :: IRANGE, ICODE, NL, NT, NR, ND, IRIGHT
  REAL :: EVBACK, DIST1, INTLEN
  LOGICAL :: WMOVE, AMBER, WCLEAR
 
  SEV_ONLINK = .FALSE.
  PREEMPT_FLAG = .FALSE.
 
! --- Reset all other vehicles every time step.
! --- Since they continually check for the presence of an EV they
! --- don't need to remember awareness and cooperation.
 
  DO IV = 1, MAX_VEHICLES_S
    IF(SLINK(IV) .EQ. 0) CYCLE
    IL = SLINK(IV)
    IF(SWILL_COOP_EV(IV)) THEN
      SEV_WAIT_TIMER(IV) = MAX(SEV_WAIT_TIMER(IV), 5.)
      CALL STREET_RESET_NEV(IV)
    ENDIF
    IF(NODE_TYPE(SUSN(SLINK(IV))) .EQ. NT_EXTERN) CYCLE
 
! --- Set speed and target lanes for EVs.
 
    IF(SFLEET(IV) .EQ. FLEET_EV) THEN
      SEV_ONLINK(IL) = .TRUE.
      DIST = SLENGTH(IL) - SLOCATION(IV)
      GOAL_LANE(IV) = LAST_FULL_LANE(IL)
 
! --- Look for signals within the preemption range.
           
      IF(SEV_RANGE(IV) .GT. 0) THEN
        IRL = IL
        DO WHILE(IRL .GT. 0)
          IRANGE = MIN(SEV_RANGE(IV), SIGNAL_RANGE(IRL))
          IF(IRANGE .GT. 0 .AND. DIST .LE. IRANGE) THEN
            ICODE = SIGNAL_CODE(IRL)
            CALL DECODE_FTC(ICODE, NL, NT, NR, ND, AMBER)
            IF(NT .NE. S_NONE) THEN
              INODE = SDSN(IRL)
              IF(PREEMPT_EV(INODE) .EQ. IV .OR. PREEMPT_EV(INODE) .EQ. 0) THEN
                PREEMPT_FLAG(INODE) = .TRUE.
                PREEMPT_EV(INODE) = IV
                PREEMPT_LINK(INODE) = IRL
              ENDIF
            ENDIF
          ENDIF
          IRL = STHRU_LINK(IRL)
          IF(IRL .EQ. 0) EXIT
          DIST = DIST + SLENGTH(IRL)
        ENDDO
      ENDIF
    ENDIF
  ENDDO
 
! --- Loop over all vehicles and check for approaching EVs.
! --- Find vehicles in the vicinity of the EV.
 
  DO IEV = 1, MAX_VEHICLES_S
    IF(SLINK(IEV) .EQ. 0) CYCLE
    IF(SID(IEV) .EQ. 0) THEN
      IL=0
      CYCLE
    ENDIF
    IL = SLINK(IEV)
    DNODE = SDSN(IL)
 
    IF(SFLEET(IEV) .EQ. FLEET_EV .AND. SLINK(IEV) .NE. 0) THEN
      EVBACK = SLOCATION(IEV) - VTLENGTH(SVTYPE(IEV))
      EVBACK = MAX(EVBACK, 0.)
 
! --- Locate vehicles on the same link as the EV.
! --- Notify them that an EV is present.
! --- If the non-EV is downstream from the EV check for awareness
! --- and cooperation.
! --- If the non-EV is upstream slow it down to give separation
! --- between it and the EV.
  
      IL = SLINK(IEV)
      DO ILANE = 1, N_STREET_LANES
        IV = FIRST_VEHICLE(IL, ILANE)
        DO WHILE(IV .NE. 0)
          IF(SFLEET(IV) .NE. FLEET_EV) THEN
            IF(SLOCATION(IV) .LE. EVBACK) THEN
              IF(SLOCATION(IV) - SLOCATION(IEV) .LE. SIGHT_DIST(IL)) THEN
                WMOVE = .FALSE.
                DIST = -1
                CALL STREET_DETECT_EV(IV, IEV, DIST, WMOVE)
              ENDIF
            ELSE
              IF(SLOCATION(IV) .LT. SLOCATION(IEV)) THEN
                DIST = EVBACK - SLOCATION(IV)
                WMOVE = .TRUE.
                CALL STREET_DETECT_EV(IV, IEV, DIST, WMOVE)
              ELSEIF(SLOCATION(IV) - SLOCATION(IEV) .LE. SIGHT_DIST(IL)) THEN
                DIST = SLOCATION(IV) - SLOCATION(IEV)
                WMOVE = .TRUE.
                CALL STREET_DETECT_EV(IV, IEV, DIST, WMOVE)
              ENDIF
            ENDIF
          ENDIF
          IV = SFOLLOWER(IV)
        ENDDO
      ENDDO
 
! --- Process the vehicles that are already on the shoulder.
          
      WMOVE = .FALSE.
      IV = SFIRST_ON_SHOULDER(IL)
      DO WHILE(IV .NE. 0)
        IF(SLOCATION(IV) .GT. EVBACK) THEN
          IF(SLOCATION(IV) - EVBACK .LE. SIGHT_DIST(IL)) THEN
            DIST = SLOCATION(IV) - EVBACK
            CALL STREET_DETECT_EV(IV, IEV, DIST, WMOVE)
          ENDIF
        ELSEIF(SLOCATION(IEV) - SLOCATION(IV) .LE. 200) THEN
          SEV_WAIT_TIMER(IV) = MAX(SEV_WAIT_TIMER(IV), 3.)
        ENDIF
        IV = SFOLLOWER(IV)
      ENDDO
 
! --- Locate vehicles on the approach links to the node the EV is
! --- approaching.
 
! --- If the node is an intersection.                                    
 
      DIST1 = SLENGTH(IL) - SLOCATION(IEV)
      IF(DIST1 .LE. LATDIST .AND. NODE_TYPE(DNODE) .EQ. NT_INTERN) THEN
        IF(STHRU_LINK(IL) .NE. 0) THEN
          IF(UP_INT_WIDTH(STHRU_LINK(IL)) .GT. 0) THEN
            DO IAP = 1, 5
              ILINK = FTC_SIGNALS(FTC_SIGNAL_ID(IL))%APPROACH(IAP)
              IF(ILINK .EQ. 0 .OR. ILINK .EQ. IL) CYCLE
              DO ILANE = 1, N_STREET_LANES
                IV = FIRST_VEHICLE(ILINK, ILANE)
                DO WHILE(IV .NE. 0)
                  IF(SFLEET(IV) .NE. FLEET_EV) THEN
                    IF(SLENGTH(ILINK) - SLOCATION(IV) .LE. SIGHT_DIST(IL)) THEN
                      DIST = SLENGTH(ILINK) - SLOCATION(IV)
                      CALL STREET_DETECT_EV(IV, IEV, DIST, WMOVE)
                    ENDIF
                  ENDIF
                  IV = SFOLLOWER(IV)
                ENDDO
              ENDDO
              IV = SFIRST_ON_SHOULDER(ILINK)
              DO WHILE(IV .NE. 0)
                IF(SFLEET(IV) .NE. FLEET_EV) THEN
                  DIST = SLENGTH(ILINK) - SLOCATION(IV)
                  CALL STREET_DETECT_EV(IV, IEV, DIST, WMOVE)
                ENDIF
                IV = SFOLLOWER(IV)
              ENDDO
            ENDDO
          ENDIF                                                   
        ENDIF                                                     
      ENDIF
 
! --- Locate vehicles on any of the links ahead of the EV.
! --- If the non-EV is within the sight distance check for awareness
! --- and cooperation.
  
      WMOVE = .TRUE.
      IF(DIST1 .LE. SIGHT_DIST(IL)) THEN
        DO IREC = 1, 5
          IF(IREC .NE. 2 .AND. DIST1 .GT. LATDIST) CYCLE
          IF(IREC .EQ. 1) ILINK = LEFT_LINK(IL)
          IF(IREC .EQ. 2) ILINK = STHRU_LINK(IL)
          IF(IREC .EQ. 3) ILINK = RIGHT_LINK(IL)
          IF(IREC .EQ. 4) ILINK = LEFT_DIAG_LINK(IL)
          IF(IREC .EQ. 5) ILINK = RIGHT_DIAG_LINK(IL)
          IF(ILINK .EQ. 0) CYCLE
          DO ILANE = 1, N_STREET_LANES
            IV = FIRST_VEHICLE(ILINK, ILANE)
            DO WHILE(IV .NE. 0)
              IF(SFLEET(IV) .NE. FLEET_EV) THEN
                IF(DIST1 + SLOCATION(IV) .LE. SIGHT_DIST(IL)) THEN
                  DIST = DIST1 + SLOCATION(IV)
                  CALL STREET_DETECT_EV(IV, IEV, DIST, WMOVE)
                ENDIF
              ENDIF
              IV = SFOLLOWER(IV)
            ENDDO
          ENDDO
          IV = SFIRST_ON_SHOULDER(ILINK)
          DO WHILE(IV .NE. 0)
            IF(DIST1 + SLOCATION(IV) .LE. SIGHT_DIST(IL)) THEN
              DIST = DIST1 + SLOCATION(IV)
              CALL STREET_DETECT_EV(IV, IEV, DIST, WMOVE)
            ENDIF
            IV = SFOLLOWER(IV)
          ENDDO
        ENDDO
      ENDIF
 
! --- Locate vehicles on the link opposite to the link the EV is on.
  
      IUP = SUSN(IL)
      IDN = SDSN(IL)
      WMOVE = .TRUE.
      DO ILOP = 1, N_STREET_LINKS
        IF(SUSN(ILOP) .EQ. IDN .AND. SDSN(ILOP) .EQ. IUP) THEN
          DO ILANE = 1, N_STREET_LANES
            IV = FIRST_VEHICLE(ILOP, ILANE)
            DO WHILE(IV .NE. 0)
              IF(SFLEET(IV) .NE. FLEET_EV) THEN
                IF(SLOCATION(IV) .LT. DIST1) THEN
                  IF(DIST1 - SLOCATION(IV) .LE. SIGHT_DIST(ILOP)) THEN
                    DIST = DIST1 - SLOCATION(IV)
                    CALL STREET_DETECT_EV(IV, IEV, DIST, WMOVE)
                  ENDIF
                ENDIF
              ENDIF
              IV = SFOLLOWER(IV)
            ENDDO
          ENDDO
          IV = SFIRST_ON_SHOULDER(ILOP)
          DO WHILE(IV .NE. 0)
            IF(SLOCATION(IV) .LT. DIST1) THEN
              IF(DIST1 - SLOCATION(IV) .LE. SIGHT_DIST(ILOP)) THEN
                DIST = DIST1 - SLOCATION(IV)
                CALL STREET_DETECT_EV(IV, IEV, DIST, WMOVE)
              ENDIF
            ENDIF
            IV = SFOLLOWER(IV)
          ENDDO
        ENDIF
      ENDDO
 
! --- Locate vehicles on the link opposite to the link directly ahead
! --- of the EV.
  
      ILINK = STHRU_LINK(IL)
      WMOVE = .TRUE.
      IF(ILINK .NE. 0) THEN
        IUP = SUSN(ILINK)
        IDN = SDSN(ILINK)
        LNKLEN = SLENGTH(ILINK)
        DO ILOP = 1, N_STREET_LINKS
          IF(SUSN(ILOP) .EQ. IDN .AND. SDSN(ILOP) .EQ. IUP) THEN
            DO ILANE = 1, N_STREET_LANES
              IV = FIRST_VEHICLE(ILOP, ILANE)
              DO WHILE(IV .NE. 0)
                IF(SFLEET(IV) .NE. FLEET_EV) THEN
                  IF(DIST1 + LNKLEN - SLOCATION(IV) .LE. SIGHT_DIST(IL)) THEN
                    DIST = DIST1 + (LNKLEN - SLOCATION(IV))
                    CALL STREET_DETECT_EV(IV, IEV, DIST, WMOVE)
                  ENDIF
                ENDIF
                IV = SFOLLOWER(IV)
              ENDDO
            ENDDO
            IV = SFIRST_ON_SHOULDER(ILOP)
            DO WHILE(IV .NE. 0)
              IF(DIST1 + LNKLEN - SLOCATION(IV) .LE. SIGHT_DIST(IL))THEN
                DIST = DIST1 + (LNKLEN - SLOCATION(IV))
                CALL STREET_DETECT_EV(IV, IEV, DIST, WMOVE)
              ENDIF
              IV = SFOLLOWER(IV)
            ENDDO
          ENDIF
        ENDDO
      ENDIF
    ENDIF
  ENDDO
 
! --- Process lane changes between lane 1 and lane 0 for vehicles
! --- that are cooperating with an EV.
 
  DO IV = 1, MAX_VEHICLES_S
    IL = SLINK(IV)
    IF(IL .EQ. 0) CYCLE
    IF(SFLEET(IV) .EQ. FLEET_BUS .AND. QSTATE(IV) .EQ. QS_DWELL) CYCLE
    IF(SFLEET(IV) .EQ. FLEET_EV) CYCLE
    IF(SLANE(IV) .GT. 1) CYCLE
    IF(SEV_WATCH(IV) .NE. 0) THEN
      IF(SLOCATION(SEV_WATCH(IV)) .GT. SLOCATION(IV)) CYCLE
    ENDIF
    INTLEN = UP_INT_WIDTH(IL)
    IF(SLOCATION(IV) - VTLENGTH(SVTYPE(IV)) .LT. INTLEN) CYCLE
    IF(SLANE(IV) .EQ. 0) SLC_TIMER(IV) = SLC_TIMER(IV) - 1
    IF(SWILL_COOP_EV(IV) .AND. SSHOULDER_WIDTH(IL) .GE. 10 .AND. SLANE(IV) .EQ. 1) THEN
 
! --- The vehicle is cooperating and there is a shoulder.
! --- The vehicle is in lane 1.
      
      IF(SSPEED(IV) .LE. 12) THEN
 
! --- When the vehicle's speed has slowed enough, try to perform
! --- a lane change onto the shoulder.
  
        ILN = 0
        JTLEAD = 0
        JTFLW = 0
        IF(SFIRST_ON_SHOULDER(IL) .NE. 0) THEN
          JV = SFIRST_ON_SHOULDER(IL)
          DO WHILE(JV .NE. 0)
            IF(SLOCATION(JV) .GT. SLOCATION(IV)) THEN
              VLEN = VTLENGTH(SVTYPE(JV))
              JTLEAD = JV
              JTFLW = SFOLLOWER(JV)
            ENDIF
            JV = SFOLLOWER(JV)
          ENDDO
          IF(JTLEAD .EQ. 0) JTFLW = SFIRST_ON_SHOULDER(IL)
        ENDIF
        IF(JTLEAD .EQ. 0) THEN
          GAP = SLENGTH(IL)
        ELSE
          GAP = SLOCATION(JTLEAD) - VLEN
        ENDIF
        IF(JTFLW .NE. 0) GAP = GAP - SLOCATION(JTFLW)
        IF(JTLEAD .NE. 0) THEN
          IF(SLOCATION(IV) .GT. SLOCATION(JTLEAD) - VLEN) GAP = 0
        ENDIF
        VLEN = VTLENGTH(SVTYPE(IV))
        IF(JTFLW .NE. 0) THEN
          IF(SLOCATION(IV) - VLEN .LT. SLOCATION(JTFLW)) GAP = 0
        ENDIF
        IF(GAP .GT. 1.5 * VLEN) THEN  
          SSPEED(IV) = 0
          SLANE(IV) = 0
          QSTATE(IV) = QS_NOTINQ
          IF(JTLEAD .EQ. 0) SFIRST_ON_SHOULDER(IL) = IV
          IF(IV .EQ. FIRST_VEHICLE(IL, 1)) FIRST_VEHICLE(IL, 1) = SFOLLOWER(IV)
          IF(IV .EQ. SLAST_VEHICLE(IL, 1)) SLAST_VEHICLE(IL, 1) = SLEADER(IV)
          IF(SLEADER(IV) .NE. 0) SFOLLOWER(SLEADER(IV)) = SFOLLOWER(IV)
          IF(SFOLLOWER(IV) .NE. 0) THEN
            SLEADER(SFOLLOWER(IV)) = SLEADER(IV)
            QSTATE(SFOLLOWER(IV)) = QS_NOTINQ
          ENDIF
          SLEADER(IV) = JTLEAD
          SFOLLOWER(IV) = JTFLW
          IF(JTLEAD .NE. 0) SFOLLOWER(JTLEAD) = IV
          IF(JTFLW .NE. 0) SLEADER(JTFLW) = IV
          SEV_WAIT_TIMER(IV) = 5
          SLOCATION(IV) = MIN(SLOCATION(IV) + 3, FLOAT(SLENGTH(IL)))
          IF(JTLEAD .NE. 0) SLOCATION(IV) = MIN(SLOCATION(IV), SLOCATION(JTLEAD) - VTLENGTH(SVTYPE(JTLEAD)))
          SLC_TIMER(IV) = MAX(SLC_TIMER(IV), 3.)
        ELSE
          IF(SFOLLOWER(IV) .NE. 0) THEN
            IF(SFLEET(SFOLLOWER(IV)) .EQ. FLEET_EV) SDESIREDSPEED(IV) = MAX(SDESIREDSPEED(IV), 12.)
          ENDIF
        ENDIF
      ENDIF
    ELSEIF(.NOT. SWILL_COOP_EV(IV) .AND. SLANE(IV) .EQ. 0 .AND. SEV_WAIT_TIMER(IV) .LE. 0) THEN
 
! --- The vehicle is on the shoulder, the EV has passed and the vehicle
! --- has waited long enough. Try to perform a lane change from the shoulder.
 
      IRIGHT = 1
      CALL TRY_STREET_LANECHANGE(IV, IRIGHT)
    ENDIF
 
! --- Decrement the wait timer.
         
    SEV_WAIT_TIMER(IV) = SEV_WAIT_TIMER(IV) - 1
  ENDDO
 
! --- CLEAR NODE PREEMPT FLAGS
  
  DO ISIG = 1, NUMBER_OF_FTCS
    INODE = FTC_SIGNALS(ISIG)%NODE
    IF(PREEMPT_EV(INODE) .EQ. 0) CYCLE
    WCLEAR = .TRUE.
    DO IAP = 1, 5
      ILINK = FTC_SIGNALS(ISIG)%APPROACH(IAP)
      IF(ILINK .EQ. 0) EXIT
      IF(PREEMPT_FLAG(INODE)) THEN
        WCLEAR = .FALSE.
        EXIT
      ENDIF
    ENDDO
    IF(WCLEAR) PREEMPT_EV(INODE) = 0
  ENDDO
  RETURN
  END
 
! ==========================================================================================================
  SUBROUTINE FREEWAY_DETECT_EV(IV, IEV, DIST, WMOVE)
! ----------------------------------------------------------------------      
! --- Determine if the subject vehicle is aware of the EV.
! --- If it is aware determine if it will cooperate.
! --- If aware and cooperating set desired speed to make it slow down.
! ----------------------------------------------------------------------      
  USE FREEWAY_LINKS
  USE FREEWAY_VEHICLES
  USE SIMPARAMS
  USE GLOBAL_DATA
  USE DRIVERS
  USE TEXT
  USE EV_DATA
  USE SEEDS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, IEV, DIST
  LOGICAL, INTENT(INOUT) :: WMOVE
  INCLUDE 'IOFILES.INC'
  INTEGER I, NFULL
  INTEGER :: IRN, KDEDUC, IL, ILEV, RLANE
  LOGICAL :: WA, WC
  REAL :: RNDNUM, IFRSPD
 
  IF(.NOT. FWILL_COOP_EV(IV) .OR. FEV_WATCH(IV) .NE. IEV) THEN
    WA = .FALSE.
    WC = .FALSE.
    IF(FEV_RAND(IV) .EQ. -1) THEN
      CALL FREEWAY_RANDOM(FSEED, RNDNUM)  
      IRN = 100 * RNDNUM
      FEV_RAND(IV) = IRN
    ELSE
      IRN = FEV_RAND(IV)
    ENDIF
    IF(FEV_WAIT_TIMER(IV) .GT. 0) THEN
      WA = .TRUE.
    ELSE
      DO I = 1, 10
        IF(FSPEED(IEV) .LE. KSPEED(I)) THEN
          KDEDUC = KRED(I)
          EXIT
        ENDIF
      ENDDO
      DO I = 1, 10
        IF(ABS(DIST) .LE. KDIST1(I)) THEN
          IF(IRN .LE. KRN1(I) - KDEDUC) WA = .TRUE.
          EXIT
        ENDIF
      ENDDO
    ENDIF
    IF(WA) THEN
      DO I = 1, 10
        IF(ABS(DIST) .LE. KDIST2(I)) THEN
          IF(IRN .LE. KRN2(I)) WC = .TRUE.
          EXIT
        ENDIF
      ENDDO
    ENDIF
    IF(WA) FEV_WATCH(IV) = IEV
    IF(.NOT. WA) THEN
      IF(IV .EQ. FLEADER(IEV)) THEN
        IF(DISTANCE_TO_SEGMENT_END(IV) - FVLENGTH(IV) - 10 .LE. DISTANCE_TO_SEGMENT_END(IEV)) WA = .TRUE.
      ENDIF
    ENDIF
  ENDIF
  IF(WC .OR. FWILL_COOP_EV(IV)) THEN
    IF(FEV_WATCH(IV) .EQ. 0 .OR. IEV .EQ. FEV_WATCH(IV) .OR. ABS(DIST) .LT. ABS(FEV_DIST(IV)) .OR. &
       (DIST .LT. 0 .AND. FEV_DIST(IV) .GT. 0)) THEN
      FEV_WATCH(IV) = IEV
      FEV_DIST(IV) = DIST
      FPSEUDO_LEADER(IV) = IEV
    ENDIF
  ENDIF
 
! --- If the vehicle is cooperating with this vehicle set a flag to make it move to the shoulder.
       
  IF((WC .OR. FWILL_COOP_EV(IV)) .AND. IEV .EQ. FEV_WATCH(IV)) THEN
    FWILL_COOP_EV(IV) = .TRUE.
    FWILL_MOVE(IV) = WMOVE
    IL = FLINK(IV)
    ILEV = FLINK(IEV)
    IF(FLANECODES(IV, 11) .NE. LC_NULL) THEN
      RLANE = 11
    ELSEIF(FLANECODES(IV, 10) .NE. LC_NULL) THEN
      RLANE = 10
    ELSEIF(FLANECODES(IV, 9) .NE. LC_NULL) THEN
      RLANE = 9
    ELSE
      RLANE = 1
    ENDIF
    IF(FLANE(IV) .EQ. RLANE .OR. FLANE(IV) .EQ. 0) THEN
      FDESIREDSPEED(IV) = 0
    ELSE
      IF(IL .NE. ILEV) THEN
        IF(FSHOULDER_WIDTH(IL) .GE. 10 .OR. NFULL .GT. 1) FDESIREDSPEED(IV) = STOP_SPD * FLANE(IV)
      ELSEIF(IL .EQ. ILEV .AND. DIST .LT. 100) THEN
        IF(FLANE(IV) .NE. FLANE(IEV)) THEN
          FWILL_MOVE(IV) = .FALSE.
          FDESIREDSPEED(IV) = 0
        ELSEIF(FLOCATION(IV) .LT. FLOCATION(IEV)) THEN
          FWILL_MOVE(IV) = .FALSE.
          FDESIREDSPEED(IV) = 0
        ELSE
          FWILL_MOVE(IV) = .TRUE.
          FDESIREDSPEED(IV) = FDESIREDSPEED(IEV) / 2
        ENDIF              
      ELSEIF(IL .EQ. ILEV .AND. FLANE(IV) .NE. FLANE(IEV)) THEN
        IF(FLANE(IV) .LT. NFULL) THEN
          FDESIREDSPEED(IV) = STOP_SPD * FLANE(IV)
        ELSEIF(FLOCATION(IV) .LT. FLOCATION(IEV)) THEN
          FDESIREDSPEED(IV) = STOP_SPD
        ENDIF
      ELSE
        IFRSPD = (FFSPEED_ADJ(FDRIVERTYPE(IV), I_FREEWAY) * FFREEFLOWSPEED(IL) + 50) / 100
        FDESIREDSPEED(IV) = MAX(IFRSPD, FDESIREDSPEED(IV))
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END
 
! ==================================================================================================
  SUBROUTINE STREET_DETECT_EV(IV, IEV, DIST, WMOVE)
  USE GLOBAL_DATA
  USE STREET_LINKS
  USE STREET_VEHICLES
  USE SIMPARAMS
  USE SEEDS
  USE DRIVERS
  USE TEXT
  USE TIMED_CONTROLLERS
  USE EV_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, IEV, DIST
  LOGICAL, INTENT(INOUT) :: WMOVE
  INTEGER I, NFULL
  INTEGER :: IRN, KDEDUC, IL, ILEV
  LOGICAL :: WA, WC, W1, W2
  REAL :: RNDNUM, IFRSPD, INTLEN
 
  NFULL = SNUMLANES(SLINK(IV))
  INTLEN = UP_INT_WIDTH(SLINK(IV))
  IF(SLOCATION(IV) - VTLENGTH(SVTYPE(IV)) .LT. INTLEN .AND. INTLEN .GT. 0 .AND. DIST .GE. 0) RETURN
 
  IF(.NOT. SWILL_COOP_EV(IV) .OR. SEV_WATCH(IV) .NE. IEV) THEN
    WA = .FALSE.
    WC = .FALSE.
    IF(SEV_RAND(IV) .EQ. 0) THEN
      CALL STREET_RANDOM(SSEED, RNDNUM)  
      IRN = 100 * RNDNUM
      SEV_RAND(IV) = MAX(IRN, 1)
    ELSE
      IRN = SEV_RAND(IV)
    ENDIF
    IF(SEV_WAIT_TIMER(IV) .GT. 0) THEN
      WA = .TRUE.
    ELSE
      DO I = 1, 10
        IF(SSPEED(IEV) .LE. KSPEED(I)) THEN
          KDEDUC = KRED(I)
          EXIT
        ENDIF
      ENDDO
      DO I = 1, 10
        IF(DIST .LE. KDIST1(I)) THEN
          IF(IRN .LE. KRN1(I) - KDEDUC) WA = .TRUE.
          EXIT
        ENDIF
      ENDDO
    ENDIF
    IF(WA) THEN
      DO I = 1, 10
        IF(DIST .LE. KDIST2(I)) THEN
          IF(IRN .LE. KRN2(I)) WC = .TRUE.
          EXIT
        ENDIF
      ENDDO
    ENDIF
    IF(WA) SEV_WATCH(IV) = IEV
    IF(.NOT. WA) THEN
      IF(IV .EQ. SLEADER(IEV)) THEN
        IF(SLOCATION(IV) - SVLENGTH(IV) - 10 .LE. SLOCATION(IEV)) WA = .TRUE.
      ENDIF
    ENDIF
  ENDIF
  IF(WC .OR. SWILL_COOP_EV(IV)) THEN
    IF(SEV_WATCH(IV) .EQ. 0 .OR. IEV .EQ. SEV_WATCH(IV) .OR. ABS(DIST) .LT. ABS(SEV_DIST(IV)) .OR. &
       (DIST .LT. 0 .AND. SEV_DIST(IV) .GT. 0)) THEN
      SEV_WATCH(IV) = IEV
      SEV_DIST(IV) = DIST
    ENDIF
  ENDIF
  IF((WC .OR. SWILL_COOP_EV(IV)) .AND. IEV .EQ. SEV_WATCH(IV)) THEN
    SWILL_COOP_EV(IV) = .TRUE.
    SWILL_MOVE(IV) = WMOVE
    IL = SLINK(IV)
    ILEV = SLINK(IEV)
    IF(SLANE(IV) .EQ. 1) THEN
      IF((IL .EQ. ILEV .AND. SLANE(IEV) .NE. 1) .OR. &
         (IL .NE. ILEV .AND. (NFULL .GT. 1 .OR. SSHOULDER_WIDTH(IL) .GE. 10))) THEN
        SDESIREDSPEED(IV) = 0
      ELSEIF(SSHOULDER_WIDTH(IL) .GE. 10) THEN
        SDESIREDSPEED(IV) = 2 * STOP_SPD
      ENDIF
    ELSE
      IF(WMOVE) THEN
        W1 = STURNCODE(IV) .EQ. TC_LEFT .AND. &
             ((SLANE(IV) .EQ. NFULL .AND. QSTATE(IV) .GT. QS_NOTINQ) .OR. SLANE(IV) .GT. NFULL)
        W2 = STURNCODE(IV) .EQ. TC_RIGHT .AND. &
             SLANE(IV) .GT. NFULL .AND. QSTATE(IV) .GT. QS_NOTINQ
        IF(W1 .OR. W2) THEN
          SWILL_MOVE(IV) = .FALSE.
        ELSE
 
! --- Set lane 1 as the only goal lane.
 
          GOAL_LANE(IV) = 1                                      
        ENDIF
      ENDIF
      IF(IL .NE. ILEV .AND. IL .NE. LEFT_LINK(ILEV) .AND. IL .NE. STHRU_LINK(ILEV) .AND. &
         IL .NE. RIGHT_LINK(ILEV) .AND. IL .NE. LEFT_DIAG_LINK(ILEV) .AND. IL .NE. RIGHT_DIAG_LINK(ILEV)) THEN
        IF(SSHOULDER_WIDTH(IL) .GE. 10 .OR. NFULL .GT. 1) SDESIREDSPEED(IV) = STOP_SPD * SLANE(IV)
      ELSEIF(IL .EQ. ILEV .AND. DIST .LT. 50 .AND. SLANE(IV) .LE. NFULL) THEN
        IF(SLANE(IV) .NE. SLANE(IEV)) THEN
          SWILL_MOVE(IV) = .FALSE.
          SDESIREDSPEED(IV) = 0
        ELSEIF(SLOCATION(IV) .LT. SLOCATION(IEV)) THEN
          SWILL_MOVE(IV) = .FALSE.
          SDESIREDSPEED(IV) = 0
        ELSE
          SWILL_MOVE(IV) = .TRUE.
          SDESIREDSPEED(IV) = SDESIREDSPEED(IEV) / 2
        ENDIF              
      ELSEIF(IL .EQ. ILEV .AND. SLANE(IV) .NE. SLANE(IEV)) THEN
        IF(SLANE(IV) .LT. NFULL) THEN
          SDESIREDSPEED(IV) = STOP_SPD * SLANE(IV)
        ELSEIF(SLOCATION(IV) .LT. SLOCATION(IEV)) THEN
          SDESIREDSPEED(IV) = STOP_SPD
        ENDIF
      ELSE
        IFRSPD = (FFSPEED_ADJ(SDRIVERTYPE(IV), I_STREET) * SFREEFLOWSPEED(IL) + 50) / 100
        SDESIREDSPEED(IV) = MAX(IFRSPD, SDESIREDSPEED(IV))
      ENDIF
    ENDIF
    IF(SWILL_MOVE(IV)) SDESIREDSPEED(IV) = MAX(SDESIREDSPEED(IV), STOP_SPD)
  ENDIF
  RETURN
  END
 
! ==========================================================================================================
  SUBROUTINE FREEWAY_RESET_NEV(IV)
  USE FREEWAY_LINKS
  USE FREEWAY_VEHICLES
  USE DRIVERS
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER :: IL, IDRVR
  REAL :: IFRSPD
  FEV_WATCH(IV) = 0
  FWILL_COOP_EV(IV) = .FALSE.
  FEV_DIST(IV) = 0
  IL = FLINK(IV)
  IDRVR = FDRIVERTYPE(IV)
  IFRSPD = FFREEFLOWSPEED(IL)
  IFRSPD = (2 * FFSPEED_ADJ(IDRVR, I_FREEWAY) * IFRSPD + 1) / 2
  FDESIREDSPEED(IV) = IFRSPD
  RETURN
  END

  SUBROUTINE STREET_RESET_NEV(IV)
  USE STREET_LINKS
  USE STREET_VEHICLES
  USE DRIVERS
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV
  INTEGER :: IL, IDRVR
  REAL :: IFRSPD
  SEV_WATCH(IV) = 0
  SWILL_COOP_EV(IV) = .FALSE.
  SEV_DIST(IV) = 0
  IL = SLINK(IV)
  IDRVR = SDRIVERTYPE(IV)
  IFRSPD = SFREEFLOWSPEED(IL)
  IFRSPD = (2 * FFSPEED_ADJ(IDRVR, I_STREET) * IFRSPD + 1) / 2
  SDESIREDSPEED(IV) = IFRSPD
  CALL SET_GOAL_LANE(IV)
  RETURN
  END
