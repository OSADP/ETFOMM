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
  SUBROUTINE UPDATE_SIGNALS
! ----------------------------------------------------------------------
! --- Executive subroutine to update all types of controllers.
! ----------------------------------------------------------------------
  USE SIMPARAMS
  IMPLICIT NONE
#ifdef TSIS_COMPATIBLE
  INCLUDE 'CORWIN.FI'
! ----------------------------------------------------------------------
  CALL SIMCALLRTE(RT_PRE_SIGNALUPDATE)
#endif    
  CALL UPDATE_INTERSECTION_CONTROLLERS
  IF(.NOT. INITMODE) CALL UPDATE_RAMPMETERS
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE UPDATE_RAMPMETERS
! ----------------------------------------------------------------------
! --- Update freeway ramp meters.
! ----------------------------------------------------------------------
  USE RAMP_METERS
  USE FREEWAY_DETECTORS
  USE FREEWAY_LINKS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  INTEGER :: IM, ICODE, NDET, IARRAY(3), IL, FLAG, IDET, TEMP, N
  REAL :: R1, TOTAL, AVG, DIFF, SPD_HDWY, DC_HDWY
  INCLUDE 'MOEFUNCTIONS.FI'
! ----------------------------------------------------------------------
  DO IM = 1, NUMBER_OF_RAMPMETERS
    IF(RAMPMETERS(IM)%STATE .EQ. 0 .AND. SIMTIME .GE. RAMPMETERS(IM)%ONSET) THEN
 
! --- Time of onset has been reached. Set state.
 
      RAMPMETERS(IM)%STATE = MS_GREEN 
      RAMPMETERS(IM)%TIMER = 1.0
    ENDIF
  ENDDO
  DO IM = 1, NUMBER_OF_RAMPMETERS
    ICODE = RAMPMETERS(IM)%CONTROL
    SELECT CASE(ICODE)
    CASE(0) !Externally controlled
    CASE(1) !Clock-time metering
 
! --- Increment timer.
 
      IF(RAMPMETERS(IM)%STATE .NE. MS_INACTIVE) THEN
 
! --- If meter is active deduct a time step from the timer.
 
        RAMPMETERS(IM)%TIMER = RAMPMETERS(IM)%TIMER - TIMESTEP
        IF(RAMPMETERS(IM)%TIMER .LE. 0.) THEN
 
! --- Timer has reached zero. Reset state and timer.
! --- Signal stays green for 1 second.
 
          RAMPMETERS(IM)%STATE = -RAMPMETERS(IM)%STATE
          IF(RAMPMETERS(IM)%STATE .EQ. MS_RED) THEN
            RAMPMETERS(IM)%TIMER = RAMPMETERS(IM)%HEADWAY(1)
          ELSE
            RAMPMETERS(IM)%TIMER = 1.0
          ENDIF
        ENDIF
      ENDIF
    CASE(2) !Demand/capacity metering
      IF(MOD(SIMTIME, 60.) .LT. TIMESTEP/2) THEN
 
! --- Determine volume from all detectors associated with the meter.
 
        TOTAL = 0.
        FLAG = 1
        DO NDET = 1, 10
          IDET = RAMPMETERS(IM)%DETECTOR(NDET)
          IF(IDET .EQ. 0) THEN
            IF(NDET .EQ. 1) THEN
              WRITE(MSGTEXT, '(A)') '  FATAL ERROR'
              CALL SENDTEXTMSG(M_ERROR)
              WRITE(MSGTEXT, '(A, I4)') '  NO DETECTOR SPECIFIED FOR DEMAND/CAPACITY METERING AT NODE', &
                                         RAMPMETERS(IM)%DSN
              CALL SENDTEXTMSG(M_ERROR)
              ERROR_FLAG = 1
              RETURN
            ELSE
              EXIT
            ENDIF
          ENDIF
          IL = FDETECTOR(IDET)%LINK
          IARRAY(1) = FUSN(IL)
          IARRAY(2) = FDSN(IL)
          IARRAY(3) = IDET
          TEMP = GETFREEWAYDETECTORMOEDATA('Volume', IARRAY, FLAG, R1)
          TOTAL = TOTAL + R1
        ENDDO
        DIFF = RAMPMETERS(IM)%CAPACITY * FNUMLANES(IL) - TOTAL
        IF(DIFF .LE. 0.0) THEN
          DC_HDWY = 20.0
        ELSE
          DC_HDWY = 3600.0 / DIFF * FNUMLANES(RAMPMETERS(IM)%LINK)
          IF(DC_HDWY .GT. 20.0) THEN
            DC_HDWY = 20.0
          ELSEIF(DC_HDWY .LT. 2.0) THEN
            DC_HDWY = 2.0
          ENDIF
        ENDIF
      ENDIF
      IF(RAMPMETERS(IM)%STATE .NE. MS_INACTIVE) THEN
 
! --- If meter is active deduct a time step from the timer.
 
        RAMPMETERS(IM)%TIMER = RAMPMETERS(IM)%TIMER - TIMESTEP
        IF(RAMPMETERS(IM)%TIMER .LE. 0.) THEN
 
! --- Timer has reached zero. Reset state and timer.
! --- Signal stays green for 1 second.
 
          RAMPMETERS(IM)%STATE = -RAMPMETERS(IM)%STATE
          IF(RAMPMETERS(IM)%STATE .EQ. MS_RED) THEN
            IF(DC_HDWY .NE. 0) THEN
              RAMPMETERS(IM)%TIMER = DC_HDWY
            ELSE
              RAMPMETERS(IM)%TIMER = 30 / FNUMLANES(RAMPMETERS(IM)%LINK)
            ENDIF
          ELSE
            RAMPMETERS(IM)%TIMER = 1.0
          ENDIF
        ENDIF
      ENDIF
    CASE(3) !Speed control metering
      IF(MOD(SIMTIME, 60.) .LT. TIMESTEP/2) THEN
        TOTAL = 0.
        DO NDET = 1, 10
          IDET = RAMPMETERS(IM)%DETECTOR(NDET)
          IF(IDET .EQ. 0) EXIT
          IL = FDETECTOR(IDET)%LINK
          IARRAY(1) = FUSN(IL)
          IARRAY(2) = FDSN(IL)
          IARRAY(3) = IDET
          FLAG = 0
          TEMP = GETFREEWAYDETECTORMOEDATA('SpeedAverage', IARRAY, FLAG, R1)
          TOTAL = TOTAL + R1
        ENDDO
        AVG = TOTAL / (NDET - 1)
        IF(AVG .GT. RAMPMETERS(IM)%SPEED(1)) THEN
          SPD_HDWY = 0.0
        ELSE
          SPD_HDWY = RAMPMETERS(IM)%HEADWAY(6)
          DO N = 2, 6
            IF(AVG .GT. RAMPMETERS(IM)%SPEED(N)) THEN
              SPD_HDWY = RAMPMETERS(IM)%HEADWAY(N-1)
              EXIT
            ENDIF
          ENDDO 
        ENDIF
      ENDIF
      IF(RAMPMETERS(IM)%STATE .NE. MS_INACTIVE) THEN
 
! --- If meter is active deduct a time step from the timer.
 
        RAMPMETERS(IM)%TIMER = RAMPMETERS(IM)%TIMER - TIMESTEP
        IF(RAMPMETERS(IM)%TIMER .LE. 0.) THEN
 
! --- Timer has reached zero. Reset state and timer.
! --- Signal stays green for 1 second.
 
          RAMPMETERS(IM)%STATE = -RAMPMETERS(IM)%STATE
          IF(RAMPMETERS(IM)%STATE .EQ. MS_RED) THEN
            IF(SPD_HDWY .NE. 0) THEN
              RAMPMETERS(IM)%TIMER = SPD_HDWY
            ELSE
              RAMPMETERS(IM)%TIMER = 30.0 / FNUMLANES(IL)
            ENDIF
          ELSE
            RAMPMETERS(IM)%TIMER = 1.0
          ENDIF
        ENDIF
      ENDIF
!        CASE(5) !Multiple-Threshold Occupancy metering
!          IF(MOD(SIMTIME, RAMPMETERS(IM)%UPDINT) .LT. TIMESTEP/2) THEN
!            TOTAL = 0.
!            DO NDET = 1, 10
!              IDET = RAMPMETERS(IM)%DETECTOR(NDET)
!              IF(IDET .EQ. 0) EXIT
!              IL = FDETECTOR(IDET)%LINK
!              IARRAY(1) = FUSN(IL)
!              IARRAY(2) = FDSN(IL)
!              IARRAY(3) = IDET
!              FLAG = 1
!              TEMP = GETFREEWAYDETECTORMOEDATA('OccupancyPercent', IARRAY, FLAG, R1)
!              TOTAL = TOTAL + R1
!            ENDDO
!            AVG = TOTAL / (NDET - 1)
!            IF(AVG .GT. SPEED(IM, 1)) THEN
!              SPD_HDWY = 0.0
!            ELSE
!              SPD_HDWY = HEADWAY(IM, 6)
!              DO N = 2, 6
!                IF(AVG .GT. SPEED(IM, N)) THEN
!                  SPD_HDWY = HEADWAY(IM, N-1)
!                  EXIT
!                ENDIF
!              ENDDO 
!            ENDIF
!          ENDIF
!          IF(RAMPMETERS(IM)%STATE .NE. MS_INACTIVE) THEN
! 
!! --- If meter is active deduct a time step from the timer.
! 
!            RAMPMETERS(IM)%TIMER = RAMPMETERS(IM)%TIMER - TIMESTEP
!            IF(RAMPMETERS(IM)%TIMER .LE. 0.) THEN
! 
!! --- Timer has reached zero. Reset state and timer.
!! --- Signal stays green for 1 second.
! 
!              RAMPMETERS(IM)%STATE = -RAMPMETERS(IM)%STATE
!              IF(RAMPMETERS(IM)%STATE .EQ. MS_RED) THEN
!                IF(SPD_HDWY .NE. 0) THEN
!                  RAMPMETERS(IM)%TIMER = SPD_HDWY
!                ELSE
!                  RAMPMETERS(IM)%TIMER = 30.0 / FNUMLANES(IL)
!                ENDIF
!              ELSE
!                RAMPMETERS(IM)%TIMER = 1.0
!              ENDIF
!            ENDIF
!          ENDIF
!        CASE(6) !ALINEA metering
    END SELECT
  ENDDO
  RETURN
  END      

! ==================================================================================================
  SUBROUTINE UPDATE_INTERSECTION_CONTROLLERS
! ----------------------------------------------------------------------
! --- Executive subroutine to update all intersection controllers.
! ----------------------------------------------------------------------
  USE TIMED_CONTROLLERS
  USE ACTUATED_CONTROLLERS
  IMPLICIT NONE
!----------------------------------------------------------------------
  IF(NUMBER_OF_FTCS .GT. 0) CALL UPDATE_FIXEDTIME_CONTROLLERS
  IF(NUMBER_OF_ACS .GT. 0) CALL UPDATE_ACTUATED_CONTROLLERS

  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE ACCUMULATE_GREEN_TIME
! ----------------------------------------------------------------------
! --- Subroutine to accumulate green time for all actuated controllers.
! ----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER :: IACT, RING, PHASE
! ----------------------------------------------------------------------
  DO IACT = 1, NUMBER_OF_ACS + NUMBER_OF_SUPER_ACS
    IF(AC_SIGNALS(IACT)%NODE(1) .LT. 0) CYCLE
    DO RING = 1, 2
      PHASE = AC_SIGNALS(IACT)%CURRENT_PHASES(RING)
      IF(PHASE .NE. 0) THEN
        AC_SIGNALS(IACT)%TOTAL_GREEN_TIME(PHASE) = AC_SIGNALS(IACT)%TOTAL_GREEN_TIME(PHASE) + TIMESTEP
      ENDIF
    ENDDO
  ENDDO
  RETURN
  END      
  
! ==================================================================================================
  SUBROUTINE UPDATE_FIXEDTIME_CONTROLLERS
! ----------------------------------------------------------------------
! --- Update all fixed time controllers.
! ----------------------------------------------------------------------
  USE TIMED_CONTROLLERS
  USE SIMPARAMS
  USE STREET_LINKS
  USE STREET_VEHICLES
  USE EV_DATA
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER :: ISIG, INTRVL, IAP, ICODE
  LOGICAL :: WLOCK(MAX_NODE_NUMBER), WSHORT
  REAL    :: ADJUST(MAX_NODE_NUMBER), EXTEND(MAX_NODE_NUMBER), PEDTIMER(MAX_NODE_NUMBER), ICYCLE
  INTEGER :: EXTENDED_INTRVL(MAX_NODE_NUMBER)
  INTEGER :: NGO, NEV, PEDCLEAR
  INTEGER :: IL, NL, NT, NR, ND, INODE
  LOGICAL :: AMBER
  REAL :: RTIME, ENDTIME
! ----------------------------------------------------------------------
      
! --- Increment timer and determine current interval for each signal.
 
  DO ISIG = 1, NUMBER_OF_FTCS
    IF(FTC_SIGNALS(ISIG)%EXTERNAL_CONTROL) CYCLE
    INODE = FTC_SIGNALS(ISIG)%NODE
    IF(FTC_SIGNALS(ISIG)%ACTIVE_INTERVALS .GT. 1) THEN
      INTRVL = FTC_SIGNALS(ISIG)%CURRENT_INTERVAL
      RTIME = FTC_SIGNALS(ISIG)%TIME_IN_INTERVAL + TIMESTEP
      ENDTIME = FTC_SIGNALS(ISIG)%DURATION(INTRVL)
 
! --- Preempt pre-timed signal for an approaching EV.
 
      IF(PREEMPT_FLAG(INODE)) THEN
        NGO = 0
        NEV = 0
        WSHORT = .FALSE.
        PEDCLEAR = 0
 
! --- Locate the approaching EV.
 
        DO IAP = 1, FTC_SIGNALS(ISIG)%APPROACHES
          IL = FTC_SIGNALS(ISIG)%APPROACH(IAP)
          IF(IL .EQ. 0) EXIT
          ICODE = SIGNAL_CODE(IL)
          CALL DECODE_FTC(ICODE, NL, NT, NR, ND, AMBER)
          IF(PREEMPT_LINK(INODE) .EQ. IL) THEN
 
! --- The EV will approach the signal on this link.
               
            WSHORT = .TRUE.
 
! --- Determine the number of seconds required for pedestrians
! --- to cross the intersection.
                 
            PEDCLEAR = 0
            IF(NEAR_CROSSLINK(IL) .NE. 0) THEN
              PEDCLEAR = TOTAL_LANES(NEAR_CROSSLINK(IL)) * 5
            ENDIF
            IF(FAR_CROSSLINK(IL) .NE. 0) THEN
              PEDCLEAR = PEDCLEAR + TOTAL_LANES(FAR_CROSSLINK(IL)) * 5
            ENDIF
 
! --- If the signal is currently allowing the movement of the EV
! --- in any direction keep it in the current interval.
      
            IF(NL .EQ. S_GREEN .OR. NT .EQ. S_GREEN .OR. NR .EQ. S_GREEN .OR. ND .EQ. S_GREEN) THEN
 
! --- Set a flag indicating that the signal is locked in the current interval.
      
              WLOCK(INODE) = .TRUE.
 
! --- Extend the end time for the current interval.
                   
              ENDTIME = RTIME + 1
            ENDIF
 
! --- Increment the number of approaches with an EV.
 
            NEV = NEV + 1
 
          ELSEIF(NL .NE. S_RED .OR. NT .NE. S_RED .OR. NR .NE. S_RED .OR. ND .NE. S_RED) THEN

! --- If any movement on another approach is not red
! --- increment the number of approaches with green.
 
            NGO = NGO + 1
          ENDIF
        ENDDO
 
        IF(NEV .GT. 0 .AND. NGO .GT. 0) PEDTIMER(INODE) = PEDTIMER(INODE) + TIMESTEP
        IF(NGO .EQ. 0 .AND. NEV .GE. 1 .AND. RTIME .EQ. 0) THEN
          WLOCK(INODE) = .TRUE.
          EXTENDED_INTRVL(INODE) = INTRVL
          IF(INTRVL .GT. 1) EXTENDED_INTRVL(INODE) = 0
        ENDIF
 
        IF(WSHORT .AND. PEDTIMER(INODE) .GT. PEDCLEAR) THEN
          ENDTIME = 0
          PEDCLEAR = 0
        ENDIF
      ENDIF
          
      IF(RTIME .GE. ENDTIME) THEN
        IF(WEVRUN) THEN
          EXTEND(INODE) = RTIME - FTC_SIGNALS(ISIG)%DURATION(INTRVL)
          IF(EXTEND(INODE) .NE. 0) THEN
            ICYCLE = FTC_SIGNALS(ISIG)%CYCLE_LENGTH
            EXTEND(INODE) = MOD(EXTEND(INODE), ICYCLE)
            IF(EXTEND(INODE) / ICYCLE .GT. 0.5) THEN
              EXTEND(INODE) = EXTEND(INODE) - ICYCLE 
            ENDIF
          ENDIF
          IF(EXTEND(INODE) .GT. 0) THEN
            ADJUST(INODE) = MIN(EXTEND(INODE), 10.)
          ELSE
            ADJUST(INODE) = MAX(EXTEND(INODE), -10.)
          ENDIF
          PEDTIMER(INODE) = 0
          IF(FTC_SIGNALS(ISIG)%TIME_IN_INTERVAL .NE. FTC_SIGNALS(ISIG)%DURATION(INTRVL)) THEN
            EXTEND(INODE) = EXTEND(INODE) + FTC_SIGNALS(ISIG)%TIME_IN_INTERVAL &
                          - FTC_SIGNALS(ISIG)%DURATION(INTRVL) - 1
            EXTENDED_INTRVL(INODE) = 0
          ENDIF
        ENDIF
        RTIME = 0.
        INTRVL = INTRVL + 1
        IF(INTRVL .GT. FTC_SIGNALS(ISIG)%ACTIVE_INTERVALS) INTRVL = 1
        FTC_SIGNALS(ISIG)%CURRENT_INTERVAL = INTRVL
        FTC_SIGNALS(ISIG)%TIME_IN_INTERVAL = 0
        DO IAP = 1, FTC_SIGNALS(ISIG)%APPROACHES
          IL = FTC_SIGNALS(ISIG)%APPROACH(IAP)
          ICODE = SIGNAL_CODE(IL)
          PREV_SIGNAL_CODE(IL) = SIGNAL_CODE(IL)
          PREV_SIGNAL_LEFT(IL) = SIGNAL_LEFT(IL)
          SIGNAL_CODE(IL) = FTC_SIGNALS(ISIG)%SIGNAL_CODE(IAP, INTRVL)
          IF(ICODE .NE. SIGNAL_CODE(IL)) CALL PROCESS_QUEUED_VEHICLES(IL, ICODE, SIGNAL_CODE(IL))
        ENDDO
      ENDIF
      FTC_SIGNALS(ISIG)%TIME_IN_INTERVAL = RTIME
    ENDIF
  ENDDO
  RETURN
  END  

! ==================================================================================================
  SUBROUTINE DECODE_FTC(ICODE, NL, NT, NR, ND, AMBER)
! ----------------------------------------------------------------------
! --- Convert signal code into left, thru, right, diagonal and amber.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ICODE
  INTEGER, INTENT(OUT) :: NL, NT, NR, ND
  LOGICAL, INTENT(OUT) :: AMBER
! ----------------------------------------------------------------------
 
! --- These codes are defined by TSIS/CORSIM RT 36.
 
!    0       Yield Sign or Amber (see Notes)         
!    1       Green Ball                              
!    2       Red Ball                                
!    3       Red with Green Right Arrow              
!    4       Red with Green Left Arrow               
!    5       Stop Sign (see Notes)                   
!    6       Red with Green Diagonal Arrow           
!    7       No Turns - Green Through Arrow          
!    8       Red with Left and Right Green Arrrows   
!    9       No Left Turn - Green Thru and Right
 
  IF(ICODE .EQ. 0) THEN
    AMBER = .TRUE.
  ELSEIF(ICODE .EQ. 1) THEN
    NL = S_GREEN
    NT = S_GREEN
    NR = S_GREEN
    ND = S_GREEN
    AMBER = .FALSE.
  ELSEIF(ICODE .EQ. 2) THEN
    NL = S_RED
    NT = S_RED
    NR = S_RED
    ND = S_RED
    AMBER = .FALSE.
  ELSEIF(ICODE .EQ. 3) THEN
    NL = S_RED
    NT = S_RED
    NR = S_GREEN
    ND = S_RED
    AMBER = .FALSE.
  ELSEIF(ICODE .EQ. 4) THEN
    NL = S_GREEN
    NT = S_RED
    NR = S_RED
    ND = S_RED
    AMBER = .FALSE.
  ELSEIF(ICODE .EQ. 5) THEN
    NL = S_STOP
    NT = S_STOP
    NR = S_STOP
    ND = S_STOP
    AMBER = .FALSE.
  ELSEIF(ICODE .EQ. 6) THEN
    NL = S_RED
    NT = S_RED
    NR = S_RED
    ND = S_GREEN
    AMBER = .FALSE.
  ELSEIF(ICODE .EQ. 7) THEN
    NL = S_RED
    NT = S_GREEN
    NR = S_RED
    ND = S_RED
    AMBER = .FALSE.
  ELSEIF(ICODE .EQ. 8) THEN
    NL = S_GREEN
    NT = S_RED
    NR = S_GREEN
    ND = S_RED
    AMBER = .FALSE.
  ELSEIF(ICODE .EQ. 9) THEN
    NL = S_RED
    NT = S_GREEN
    NR = S_GREEN
    ND = S_GREEN
    AMBER = .FALSE.
  ENDIF
  RETURN
  END    
  
! ==================================================================================================
  SUBROUTINE UPDATE_ACTUATED_CONTROLLERS
! ----------------------------------------------------------------------
  USE TIMED_CONTROLLERS
  USE ACTUATED_CONTROLLERS
  USE STREET_LINKS
  USE NTCIP_DATA
  USE GLOBAL_DATA
  USE SIMPARAMS
  USE STREET_VEHICLES
  USE DCS_DATA
  IMPLICIT NONE
  !LOGICAL :: FIRST = .TRUE.
  INCLUDE 'IOFILES.INC'
  LOGICAL :: W, FIRST = .TRUE., FIRST_DCS = .TRUE.
  INTEGER :: NQ, ILN, IV, PREVIOUS_COLOR, CURRENT_COLOR
  INTEGER :: ILC, ITC, IRC, IDC
  INTEGER :: JLC, JTC, JRC, JDC
  INTEGER :: PHASE, ILINK, IL, IACT, RING, I, FORCEOFF, EXTENSION
! ----------------------------------------------------------------------
  !Use DCS_DATA if requested
  FORCEOFF = 0
  EXTENSION = 0
  IF(USE_DCS .AND. FIRST_DCS) THEN
    FIRST_DCS = .FALSE.
    !Detectors for the DCS zones
    ZONE_DETECTORS(2, 1) = 1
    ZONE_DETECTORS(2, 2) = 2
    ZONE_DETECTORS(2, 3) = 3
    ZONE_DETECTORS(2, 4) = 4
    ZONE_DETECTORS(6, 1) = 5
    ZONE_DETECTORS(6, 2) = 6
    ZONE_DETECTORS(6, 3) = 7
    ZONE_DETECTORS(6, 4) = 8

    !Stopbar detectors, 1 for each phase, spanning all lanes
    STOPBAR_DETECTORS(1) = 9
    STOPBAR_DETECTORS(2) = 10
    STOPBAR_DETECTORS(3) = 11
    STOPBAR_DETECTORS(4) = 12
    STOPBAR_DETECTORS(5) = 13
    STOPBAR_DETECTORS(6) = 14
    STOPBAR_DETECTORS(7) = 15
    STOPBAR_DETECTORS(8) = 16
  ENDIF
    
  IF(USE_NTCIP) THEN
    IF(FIRST) THEN
      FIRST = .FALSE.
      OPEN(1, FILE = LINFNAME(1:IROOT-1)//'.LOG', STATUS='REPLACE')
      FIRST = .FALSE.
      WRITE(1, '(A)') 'Timing Plan'
      DO I = 1, 8
        WRITE(1, '(A,I1,A,F5.1,A,F5.1)') 'Phase ', I, ' Min Green = ', AC_SIGNALS(1)%GUI_MIN_GREEN_TIMES(I), &
                        ' Max Green = ', AC_SIGNALS(1)%GUI_MAX_GREEN_TIMES(I)
      ENDDO
      WRITE(1, '(A,F8.1)') 'Timestep: ', TIMESTEP
      CLOSE(1)
    ENDIF
    !The NTCIP controller will update the signal states.
    IF(USE_DCS .AND. .NOT. INITMODE) CALL EXECUTE_DCS
  ELSE   
    IF(USE_DCS .AND. .NOT. INITMODE) CALL EXECUTE_DCS
    CALL UPDATE_SCOPE_ACTUATED_CONTROLLERS
  ENDIF

  !!We don't currently have any way to accumulate number of times started when using the NTCIP interface.
  ! --- Transfer controller states to approach links
  DO IACT = 1, NUMBER_OF_ACS + NUMBER_OF_SUPER_ACS
    IF(AC_SIGNALS(IACT)%NODE(1) .LT. 0)CYCLE
    IF(AC_SIGNALS(IACT)%EXTERNAL_CONTROL ) CYCLE
      
    DO ILINK = 1, AC_SIGNALS(IACT)%N_DIRECT_APPROACHES
      IL = AC_SIGNALS(IACT)%DIRECT_APPROACH(ILINK)
    
      !Save previous signal states
      ILC = S_RED
      IF(SIGNAL_LEFT(IL)) ILC = S_GREEN
      ITC = S_RED
      IF(SIGNAL_THRU(IL)) ITC = S_GREEN
      IRC = S_RED
      IF(SIGNAL_RIGHT(IL)) IRC = S_GREEN
      IDC = S_RED
      IF(SIGNAL_DIAG(IL)) IDC = S_GREEN        
      
      SIGNAL_LEFT(IL) = .FALSE.
      SIGNAL_THRU(IL) = .FALSE.
      SIGNAL_RIGHT(IL) = .FALSE.
      SIGNAL_DIAG(IL) = .FALSE.
      AMBER_LEFT(IL) = .FALSE.
      AMBER_THRU(IL) = .FALSE.
      AMBER_RIGHT(IL) = .FALSE.
      AMBER_DIAG(IL) = .FALSE.
      
      DO PHASE = 1, 8
        
        IF(.NOT. AC_SIGNALS(IACT)%PHASE(PHASE)%IN_USE) CYCLE

        !If this is a tram link, only update the signal state if it is a tram-only phase
        !or if a tram has checked in, or if priority is not allowed
        !if(tram_link(il)) then
        !  if(.not. ac_signals(iact)%tram_only_phase(phase)) then    !shared phase
        !    if(sum(ac_signals(iact)%checkin_distance) .ne. 0) then  !priority is allowed
        !      if(.not. ac_signals(iact)%serving_tram(phase)) then   !no tram has checked in
        !        if(ac_signals(iact)%policy(phase, ilink) .ne. 0) then
        !          cycle
        !        endif
        !      endif
        !    endif
        !  endif
        !endif
      
        PREVIOUS_COLOR = RED
        CURRENT_COLOR = RED
        IF(USE_NTCIP) THEN
          !How to define PREVIOUS_COLOR using NTCIP?
          IF(BTEST(GREEN_PHASES(IACT), PHASE-1)) THEN
            CURRENT_COLOR = GREEN
          ELSEIF(BTEST(YELLOW_PHASES(IACT), PHASE-1)) THEN
            CURRENT_COLOR = YELLOW
          ENDIF
        ELSE
          DO RING = 1, 2
            IF(AC_SIGNALS(IACT)%CURRENT_PHASES(RING) .EQ. PHASE) THEN
              IF(AC_SIGNALS(IACT)%SDP%PREVIOUS_COLORS(RING) .EQ. GREEN) THEN
                PREVIOUS_COLOR = GREEN
              ELSEIF(AC_SIGNALS(IACT)%SDP%PREVIOUS_COLORS(RING) .EQ. YELLOW) THEN
                PREVIOUS_COLOR = YELLOW
              ENDIF
              IF(AC_SIGNALS(IACT)%SDP%CURRENT_COLORS(RING) .EQ. GREEN) THEN
                CURRENT_COLOR = GREEN
              ELSEIF(AC_SIGNALS(IACT)%SDP%CURRENT_COLORS(RING) .EQ. YELLOW) THEN
                CURRENT_COLOR = YELLOW
              ENDIF
              EXIT
            ENDIF
          ENDDO
        ENDIF
          
        IF(CURRENT_COLOR .EQ. GREEN) THEN
          IF(AC_SIGNALS(IACT)%PHASE(PHASE)%LEFTARROW(ILINK)) THEN
            SIGNAL_LEFT(IL) = .TRUE.
            TIME_IN_AMBER_LEFT(IL) = 0.
            TIME_IN_RED_LEFT(IL) = 0.
          ENDIF
          IF(AC_SIGNALS(IACT)%PHASE(PHASE)%THRUARROW(ILINK)) THEN
            SIGNAL_THRU(IL) = .TRUE.
            TIME_IN_AMBER_THRU(IL) = 0.
            TIME_IN_RED_THRU(IL) = 0.
          ENDIF
          IF(AC_SIGNALS(IACT)%PHASE(PHASE)%RIGHTARROW(ILINK)) THEN
            SIGNAL_RIGHT(IL) = .TRUE.
            TIME_IN_AMBER_RIGHT(IL) = 0.
            TIME_IN_RED_RIGHT(IL) = 0.
          ENDIF
          IF(AC_SIGNALS(IACT)%PHASE(PHASE)%LDIAGARROW(ILINK) .OR. AC_SIGNALS(IACT)%PHASE(PHASE)%RDIAGARROW(ILINK)) THEN
            SIGNAL_DIAG(IL) = .TRUE.
            TIME_IN_AMBER_DIAG(IL) = 0.
            TIME_IN_RED_DIAG(IL) = 0.
          ENDIF
        ELSEIF(CURRENT_COLOR .EQ. YELLOW) THEN
          IF(AC_SIGNALS(IACT)%PHASE(PHASE)%LEFTARROW(ILINK)) THEN
            SIGNAL_LEFT(IL) = .TRUE.
            AMBER_LEFT(IL) = .TRUE.
            TIME_IN_AMBER_LEFT(IL) = TIME_IN_AMBER_LEFT(IL) + TIMESTEP
          ENDIF
          IF(AC_SIGNALS(IACT)%PHASE(PHASE)%THRUARROW(ILINK)) THEN
            SIGNAL_THRU(IL) = .TRUE.
            AMBER_THRU(IL) = .TRUE.
            TIME_IN_AMBER_THRU(IL) = TIME_IN_AMBER_THRU(IL) + TIMESTEP
          ENDIF
          IF(AC_SIGNALS(IACT)%PHASE(PHASE)%RIGHTARROW(ILINK)) THEN
            SIGNAL_RIGHT(IL) = .TRUE.
            AMBER_RIGHT(IL) = .TRUE.
            TIME_IN_AMBER_RIGHT(IL) = TIME_IN_AMBER_RIGHT(IL) + TIMESTEP
          ENDIF
          IF(AC_SIGNALS(IACT)%PHASE(PHASE)%LDIAGARROW(ILINK) .OR. AC_SIGNALS(IACT)%PHASE(PHASE)%RDIAGARROW(ILINK)) THEN
            SIGNAL_DIAG(IL) = .TRUE.
            AMBER_DIAG(IL) = .TRUE.
            TIME_IN_AMBER_DIAG(IL) = TIME_IN_AMBER_DIAG(IL) + TIMESTEP
          ENDIF
        ELSE
          IF(ALWAYS_GREEN(IL, 1)) THEN
            SIGNAL_LEFT(IL) = .TRUE.
            TIME_IN_AMBER_LEFT(IL) = 0.
            TIME_IN_RED_LEFT(IL) = 0.
          ELSE
            IF(AC_SIGNALS(IACT)%PHASE(PHASE)%LEFTARROW(ILINK)) THEN
              TIME_IN_RED_LEFT(IL) = TIME_IN_RED_LEFT(IL) + TIMESTEP
            ENDIF
          ENDIF
          IF(ALWAYS_GREEN(IL, 2)) THEN
            SIGNAL_THRU(IL) = .TRUE.
            TIME_IN_AMBER_THRU(IL) = 0.
            TIME_IN_RED_THRU(IL) = 0.
          ELSE
            IF(AC_SIGNALS(IACT)%PHASE(PHASE)%THRUARROW(ILINK)) THEN
              TIME_IN_RED_THRU(IL) = TIME_IN_RED_THRU(IL) + TIMESTEP
            ENDIF
          ENDIF
          IF(ALWAYS_GREEN(IL, 3)) THEN
            SIGNAL_RIGHT(IL) = .TRUE.
            TIME_IN_AMBER_RIGHT(IL) = 0.
            TIME_IN_RED_RIGHT(IL) = 0.
          ELSE
            IF(AC_SIGNALS(IACT)%PHASE(PHASE)%RIGHTARROW(ILINK)) THEN
              TIME_IN_RED_RIGHT(IL) = TIME_IN_RED_RIGHT(IL) + TIMESTEP
            ENDIF
          ENDIF
          IF(ALWAYS_GREEN(IL, 4) .OR. ALWAYS_GREEN(IL, 5)) THEN
            SIGNAL_DIAG(IL) = .TRUE.
            TIME_IN_AMBER_DIAG(IL) = 0.
            TIME_IN_RED_DIAG(IL) = 0.
          ELSE
            IF(AC_SIGNALS(IACT)%PHASE(PHASE)%LDIAGARROW(ILINK) .OR. AC_SIGNALS(IACT)%PHASE(PHASE)%RDIAGARROW(ILINK)) THEN
              TIME_IN_RED_DIAG(IL) = TIME_IN_RED_DIAG(IL) + TIMESTEP
            ENDIF
          ENDIF
        ENDIF
      ENDDO
        
      !Save current signal states
      JLC = S_RED
      IF(SIGNAL_LEFT(IL)) JLC = S_GREEN
      JTC = S_RED
      IF(SIGNAL_THRU(IL)) JTC = S_GREEN
      JRC = S_RED
      IF(SIGNAL_RIGHT(IL)) JRC = S_GREEN
      JDC = S_RED
      IF(SIGNAL_DIAG(IL)) JDC = S_GREEN

      !Process queued vehicles.
      CALL PROCESS_QUEUED_VEHICLES_AC(IL, ILC, ITC, IRC, IDC, JLC, JTC, JRC, JDC)

    ENDDO
  ENDDO
  IF(.NOT. INITMODE) THEN
    CALL ACCUMULATE_GREEN_TIME
 
! --- Update statistics for queued vehicles in each lane.
 
    DO IL = 1, N_STREET_LINKS
      DO ILN = 1, N_STREET_LANES
        NQ = 0
        IV = FIRST_VEHICLE(IL, ILN)
        DO WHILE(IV .NE. 0)
          W = QSTATE(IV) .NE. QS_NOTINQ .AND. QSTATE(IV) .NE. QS_DWELL
          W = W .OR. HAS_STOPPED(IV)
          IF(W) THEN
            NQ = NQ + 1
          ENDIF
          IV = SFOLLOWER(IV)
        ENDDO
        QUEUE_TOTAL(IL, ILN) = QUEUE_TOTAL(IL, ILN) + NQ
        QUEUE_MAX(IL, ILN) = MAX(QUEUE_MAX(IL, ILN), NQ)
      ENDDO
    ENDDO
  ENDIF
  RETURN
  END
