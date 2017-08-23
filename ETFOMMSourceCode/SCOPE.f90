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
  
!----------------------------------------------------------------------------
! This part of ETFOMM was derived from the open source project named SCOPE.
! SCOPE copyright information is copied below:  
!----------------------------------------------------------------------------
!--    Copyright (C) 2012  Advanced Technologies, Inc
!--
!--    SCOPE is free software: you can redistribute it and/or modify
!--    it under the terms of the GNU General Public License as published by
!--    the Free Software Foundation, version 2 of the License.
!--
!--    SCOPE is distributed in the hope that it will be useful,
!--    but WITHOUT ANY WARRANTY; without even the implied warranty of
!--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!--    GNU General Public License for more details.
!--
!--
!--
!----------------------------------------------------------------------------
  
!----------------------------------------------------------------------------
!--
!-- Some of the original SCOPE code has been modified or replaced.
!-- Tom Simmerman, Connected, Inc.  2014
!--
!----------------------------------------------------------------------------

  
  module debug
    !Set verbose true to write information messages in debug mode
    !logical :: verbose = .true.
    logical :: verbose = .false.
  end module

! ==================================================================================================
  SUBROUTINE UPDATE_SCOPE_ACTUATED_CONTROLLERS
!----------------------------------------------------------------------
!--- Update all actuated controllers for the current time step.
!--- Duplicates functionality in SCOPE_MAIN
!----------------------------------------------------------------------
#ifdef DebugVersion
  use debug
#endif  
  USE ACTUATED_CONTROLLERS
  USE SCOPE_PARAMETERS
  USE SIMPARAMS
  USE STREET_LINKS
  USE STREET_DETECTORS
  USE TEXT
  USE NTCIP_DATA
  IMPLICIT NONE
  INTEGER :: IACT
!----------------------------------------------------------------------
  IF(.NOT. SIGNALS_INITIALIZED) THEN
    SIGNALS_INITIALIZED = .TRUE.
    CALL SET_SDP_DATA 
    CALL ALLOCATE_NTCIP_ARRAYS(NUMBER_OF_ACS)
    DO IACT = 1, NUMBER_OF_ACS + NUMBER_OF_SUPER_ACS
      IF(AC_SIGNALS(IACT)%NODE(1) .LT. 0) CYCLE
      CALL INITIALIZE(IACT)
    ENDDO  
    IF(TIMESTEP .GT. SCOPE_TIMESTEP) THEN
      ISTEPS = TIMESTEP / SCOPE_TIMESTEP
    ELSE
      ISTEPS = 1
    ENDIF
  ENDIF

  !Loop over ISTEPS to account for difference in SCOPE timestep and ETFOMM timestep.
  DO ISTEP = 1, ISTEPS
    IF(ISTEPS .GT. 1) CALL UPDATE_SIMTIME
    !Loop over all actuated signals and use SCOPE code to operate phasing.
    DO IACT = 1, NUMBER_OF_ACS + NUMBER_OF_SUPER_ACS
      IF(AC_SIGNALS(IACT)%NODE(1) .LT. 0) CYCLE
      CALL UPDATE_PEDESTRIAN_DETECTORS(IACT) !Not fully implemented yet.
      CALL CONTROL_ACTUATED_INTERSECTION(IACT)
      CALL UPDATE_TIMERS(IACT)
    ENDDO
  ENDDO
  RETURN
  END 

! ==================================================================================================
  SUBROUTINE UPDATE_SIMTIME
!----------------------------------------------------------------------
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  INTEGER :: COUNT
!----------------------------------------------------------------------
  IF(SIMTIME .EQ. 0) COUNT = 0
  COUNT = COUNT + 1
  SIMTIME = COUNT * SCOPE_TIMESTEP
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE INITIALIZE(IACT)
!----------------------------------------------------------------------
!--- SCOPE
!--  Method : Initialize
!--
!--  Purpose
!--
!--  Initialize data in this package
!--
!--
!--  Change History
!--
!--  DATE                 Description
!--
!--  Aug 17, 2009         Initial Version
!--  Mar 21, 2011         Fix name and add actuated
! -- August, 2014         Ported to ETFOMM
!----------------------------------------------------------------------
#ifdef DebugVersion
  use debug
#endif  
  USE SIMPARAMS
  USE ACTUATED_CONTROLLERS
  USE GLOBAL_DATA
  USE SCOPE_PARAMETERS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT
  INTEGER :: INDEX, PRIORITY_INDEX, PRIORITY_PAIR, IMODE, PHASE
  LOGICAL :: PING_PONG_FLAG, W1, W2
!----------------------------------------------------------------------
  !--  initialize phase and color
  AC_SIGNALS(IACT)%SDP%CURRENT_COLORS(1) = RED
  AC_SIGNALS(IACT)%SDP%CURRENT_COLORS(2) = RED

  !--  Initialize control
  AC_SIGNALS(IACT)%SDP%CONTROL_MODE = ACTUATED   !!!Can't find any explanation in SCOPE about what this is.
                                                 !!!Seems like it should be "ACTUATED" but it is treated as an integer
                                                 !!!and is compared to PRESENCE_MODE, RECALL_MIN and RECALL_MAX in
                                                 !!!CONTROL_ACTUATED_INTERSECTION.
                                                 !!!In another place it is compared to ACTUATED or ADAPTIVE.
  
  
  AC_SIGNALS(IACT)%SDP%PEDESTRIAN_ACTIVE = .FALSE.

  !--  Initialize Red Clear, Yellow Clear
  AC_SIGNALS(IACT)%SDP%YC = AC_SIGNALS(IACT)%GUI_YC
  AC_SIGNALS(IACT)%SDP%RC = AC_SIGNALS(IACT)%GUI_RC

  !--  actuated information
  AC_SIGNALS(IACT)%SDP%ACTUATED = .FALSE.
  AC_SIGNALS(IACT)%SDP%ADJUSTMENT = 0 
  AC_SIGNALS(IACT)%SDP%ACTUATED_MODE = 0 
  AC_SIGNALS(IACT)%SDP%MINIMUM_GREEN_TIME = 0.0
  AC_SIGNALS(IACT)%SDP%MAXIMUM_GREEN_ORIGINAL_VALUE = 0.0
  AC_SIGNALS(IACT)%SDP%MAXIMUM_GREEN_TIME = 0.0
  AC_SIGNALS(IACT)%SDP%EXTENSION_TIME = 0.0
  AC_SIGNALS(IACT)%SDP%GAP_TIMES = 0.0
  AC_SIGNALS(IACT)%SDP%TIME_BEFORE_REDUCTION = 0.0
  AC_SIGNALS(IACT)%SDP%TIME_TO_REDUCE = 0.0
  AC_SIGNALS(IACT)%SDP%MIN_GAP_TIMES = 0.0
  AC_SIGNALS(IACT)%SDP%QUEUED = .FALSE.
  AC_SIGNALS(IACT)%QUEUED_PHASES = 0
  AC_SIGNALS(IACT)%SDP%GREEN_DONE_ONCE = .FALSE.
  AC_SIGNALS(IACT)%SDP%GREEN_TIME = 0.0
  AC_SIGNALS(IACT)%SDP%TIME_LIMIT = 0.0

  !--  Actuated Data

  !--  phase controller splits
  !--  splits represent phase transition time
  AC_SIGNALS(IACT)%CURRENT_PHASES = 0
  DO INDEX = 1, 8
    IF(.NOT. AC_SIGNALS(IACT)%PHASE(INDEX)%IN_USE) CYCLE
    IMODE = AC_SIGNALS(IACT)%GUI_ACTUATED_MODE(INDEX)
    IF(IMODE .EQ. PRESENCE_MODE .OR. IMODE .EQ. RECALL_MIN) THEN
      AC_SIGNALS(IACT)%SDP%GREEN_TIME(INDEX) = AC_SIGNALS(IACT)%GUI_MIN_GREEN_TIMES(INDEX)
    ELSEIF(IMODE .EQ. RECALL_MAX) THEN
      AC_SIGNALS(IACT)%SDP%GREEN_TIME(INDEX) = AC_SIGNALS(IACT)%GUI_MAX_GREEN_TIMES(INDEX)
    ENDIF
    AC_SIGNALS(IACT)%SDP%TIME_LIMIT(INDEX) = AC_SIGNALS(IACT)%SDP%GREEN_TIME(INDEX) + AC_SIGNALS(IACT)%GUI_RC(INDEX) + AC_SIGNALS(IACT)%GUI_YC(INDEX)
  ENDDO

  AC_SIGNALS(IACT)%SDP%MINIMUM_GREEN_TIME = AC_SIGNALS(IACT)%GUI_MIN_GREEN_TIMES
  AC_SIGNALS(IACT)%SDP%MAXIMUM_GREEN_ORIGINAL_VALUE = AC_SIGNALS(IACT)%GUI_MAX_GREEN_TIMES
  AC_SIGNALS(IACT)%SDP%MAXIMUM_GREEN_TIME = AC_SIGNALS(IACT)%GUI_MAX_GREEN_TIMES
  AC_SIGNALS(IACT)%SDP%EXTENSION_TIME = AC_SIGNALS(IACT)%GUI_DEFAULT_EXTENSION_TIMES
  AC_SIGNALS(IACT)%SDP%ACTUATED_MODE = AC_SIGNALS(IACT)%GUI_ACTUATED_MODE
  
  AC_SIGNALS(IACT)%SDP%GAP_TIMES = AC_SIGNALS(IACT)%GUI_GAP_TIMES
  AC_SIGNALS(IACT)%SDP%COMPUTED_GAP_TIME = AC_SIGNALS(IACT)%GUI_GAP_TIMES
  AC_SIGNALS(IACT)%SDP%TIME_BEFORE_REDUCTION = AC_SIGNALS(IACT)%GUI_TIME_BEFORE_REDUCTION
  AC_SIGNALS(IACT)%SDP%TIME_TO_REDUCE = AC_SIGNALS(IACT)%GUI_TIME_TO_REDUCE
  AC_SIGNALS(IACT)%SDP%MIN_GAP_TIMES = AC_SIGNALS(IACT)%GUI_MIN_GAP_TIMES

  DO PHASE = 1, 8
    CALL RESET_PASSAGE_TIMER(IACT, PHASE) 
    CALL INITIALIZE_REDUCTION_TIMER(IACT, PHASE) 
    CALL INITIALIZE_GAP_DOWN_TIMER(IACT, PHASE) 
  ENDDO
  AC_SIGNALS(IACT)%SDP%TIME_PLUS_EXTENSION = 0.0

  AC_SIGNALS(IACT)%SDP%WAIT_FOR_GAP = .FALSE.
  AC_SIGNALS(IACT)%SDP%GAPOUT = .FALSE.
  !AC_SIGNALS(IACT)%EXTEND_GREEN = .FALSE.

  AC_SIGNALS(IACT)%SDP%PREVIOUS_PHASES(RING_1) = 0
  AC_SIGNALS(IACT)%SDP%PREVIOUS_PHASES(RING_2) = 0
  AC_SIGNALS(IACT)%SDP%PREVIOUS_COLORS(RING_1) = RED
  AC_SIGNALS(IACT)%SDP%PREVIOUS_COLORS(RING_2) = RED

  !--  initialize pedestrian data
  
  AC_SIGNALS(IACT)%SDP%PED_WALK_TIME = AC_SIGNALS(IACT)%GUI_NEW_PED_WALK_TIMES
  AC_SIGNALS(IACT)%SDP%PED_CLEARANCE_TIME = AC_SIGNALS(IACT)%GUI_NEW_PED_CLEARANCE_TIMES
  AC_SIGNALS(IACT)%SDP%PED_OMIT = AC_SIGNALS(IACT)%GUI_NEW_PED_OMIT
  AC_SIGNALS(IACT)%SDP%PED_DETECT_ASSIGN = AC_SIGNALS(IACT)%GUI_NEW_PED_DETECTOR_ASSIGNMENTS

  AC_SIGNALS(IACT)%SDP%ADJUSTMENT = AC_SIGNALS(IACT)%GUI_EXTENSION_TIME_INCREMENT
        
  !--  for testing max min pairs
  PING_PONG_FLAG = .TRUE.

  !--  if a phase has a minimum or maximum recall value then add it to
  !--  the queue.  Start with the main phase
  DO INDEX = 1, 8
    PRIORITY_INDEX = ACTUATED_PRIORITIES(INDEX)
    IF(.NOT. AC_SIGNALS(IACT)%PHASE(PRIORITY_INDEX)%IN_USE) CYCLE
    IF(INDEX .LT. ACTUATED_PRIORITIES(INDEX)) THEN
      PRIORITY_PAIR = ACTUATED_PRIORITIES(INDEX + 1)
    ENDIF

    IMODE = AC_SIGNALS(IACT)%GUI_ACTUATED_MODE(PRIORITY_INDEX)
    IF(IMODE .EQ. RECALL_MIN .OR. IMODE .EQ. RECALL_MAX) THEN
      IF(.NOT. AC_SIGNALS(IACT)%SDP%QUEUED(PRIORITY_INDEX)) THEN
        CALL QUEUE_PHASE(IACT, PRIORITY_INDEX)
        AC_SIGNALS(IACT)%QUEUED_PHASES = AC_SIGNALS(IACT)%QUEUED_PHASES + 1
      ENDIF

      !--  see if we're running in a concurrent state
      IF(PING_PONG_FLAG) THEN
        W1 = AC_SIGNALS(IACT)%SDP%ACTUATED_MODE(PRIORITY_INDEX) .NE.  AC_SIGNALS(IACT)%SDP%ACTUATED_MODE(PRIORITY_PAIR)
        W2 = AC_SIGNALS(IACT)%SDP%GREEN_TIME(PRIORITY_INDEX) .NE. AC_SIGNALS(IACT)%SDP%GREEN_TIME(PRIORITY_PAIR)
        IF(W1 .OR. W2) THEN
          AC_SIGNALS(IACT)%SDP%CONCURRENT_RECALL = .FALSE.          
        ENDIF !-- index odd
      ENDIF
            
    ELSE !--  mode is not max or min recall

      AC_SIGNALS(IACT)%SDP%CONCURRENT_RECALL = .FALSE.

#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'initialize message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a)') '  mode not max or min, concurrent recall of phases not set'
call sendtextmsg(m_info)
endif
#endif

    ENDIF !--  recall mode  

    !--  for phase pairs
    PING_PONG_FLAG = .NOT. PING_PONG_FLAG

  ENDDO
  
  !!--  set some default values based on type of intersection
  !IF(AC_SIGNALS(IACT)%SDP%INTERSECTION_TYPE .EQ. FOUR_LEG) THEN       
  !  AC_SIGNALS(IACT)%NUMBER_OF_RINGS = 2
  !ELSEIF(AC_SIGNALS(IACT)%SDP%INTERSECTION_TYPE .EQ. THREE_LEG) THEN
  !  AC_SIGNALS(IACT)%NUMBER_OF_RINGS = 1
  !ELSE
  !  WRITE(MSGTEXT, '(A)') '  Type of intersection not recognized'
  !  CALL SENDTEXTMSG(M_INFO)
  !  !ERROR_FLAG = 1
  !ENDIF  

#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'initialize message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a)') '  completed'
call sendtextmsg(m_info)
endif
#endif

  !Determine the phasing by ring.
  !!!This needs to be modified to consider lagging phases.
  AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE = 0
  DO PHASE = 1, 4
    IF(AC_SIGNALS(IACT)%PHASE(PHASE)%IN_USE) THEN
      AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(1, PHASE) = PHASE
      IF(PHASE .LT. 3) THEN
        AC_SIGNALS(IACT)%MAX_INDEX(1, 1) = PHASE
      ELSE
        AC_SIGNALS(IACT)%MAX_INDEX(1, 2) = PHASE
      ENDIF
    ENDIF
  ENDDO
  DO PHASE = 5, 8
    IF(AC_SIGNALS(IACT)%PHASE(PHASE)%IN_USE) THEN
      AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(2, PHASE - 4) = PHASE
      IF(PHASE .LT. 7) THEN
        AC_SIGNALS(IACT)%MAX_INDEX(2, 1) = PHASE - 4
      ELSE
        AC_SIGNALS(IACT)%MAX_INDEX(2, 2) = PHASE - 4
      ENDIF
    ENDIF
  ENDDO

  CALL START_INITIAL_PHASES(IACT)
  AC_SIGNALS(IACT)%INITIAL_PHASES_STARTED = .TRUE.
    
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE CONTROL_ACTUATED_INTERSECTION(IACT)
!----------------------------------------------------------------------
!--- SCOPE
!--  Method : Control Actuated Intersection
!--  Date   : Feb 23, 2011
!--
!--  Purpose
!--
!--  Control an intersection that has actuators or min/max recalls.
!--
!--
!--  Change History
!--
!--  DATE                 Description
!--
!--  Feb 23, 2011         Initial Version
! -- August, 2014         Ported to ETFOMM
! -- August, 2014         Added logic to start initial phases
!----------------------------------------------------------------------
#ifdef DebugVersion
  use debug
#endif 
  USE TEXT
  USE SIMPARAMS
  USE GLOBAL_DATA
  USE SCOPE_PARAMETERS
  USE STREET_DETECTORS
  USE ACTUATED_CONTROLLERS
  USE NTCIP_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT
  INTEGER :: CALLING_PHASE, DETECT_INDEX, RING, PHASE, IDET, IMODE
  LOGICAL :: MAX_OR_GAP_MODE, GAP_OUT_MODE, WAITFORGAP
  LOGICAL :: CHANNEL_HAD_CALL, CHANNEL_HAS_CALL, CHANNEL_HAS_NEW_CALL
  REAL :: TIME_IN_PHASE, TIME_LIMIT
  INTEGER :: INDEX, NEXT_PHASE, IPHASE, NPHASE, IRING, NEW_PHASE
  INTEGER :: FIRST_RING, SECOND_RING
  LOGICAL :: CALLS_ON_NEXT_BARRIER
  REAL :: REMAINING_GREEN_TIME
  INTEGER :: OTHER_PHASE, MASTER_PHASE
  LOGICAL :: MUST_TERMINATE, FORCE_YELLOW, START_FLAG
! ---------------------------------------------------------------------- 
  !!Determine if it is the beginning of a coordination cycle
  IF(AC_SIGNALS(IACT)%CYCLE_LENGTH .NE. 0) THEN
    CALL COORDINATION(IACT)
  ENDIF
  
  AC_SIGNALS(IACT)%HAS_DEMAND = .FALSE.
  !Get detector actuations from ETFOMM detectors.
  DO DETECT_INDEX = 1, AC_SIGNALS(IACT)%SDP%DETECTOR_COUNT
    IDET = AC_SIGNALS(IACT)%SDP%DETECTOR_LIST(DETECT_INDEX)
    IF(ISTEPS .EQ. 1) THEN 
      CHANNEL_HAS_CALL = SDETECTOR(IDET)%CURRENT_STATE .EQ. 1
      CHANNEL_HAD_CALL = SDETECTOR(IDET)%CURRENT_STATE .EQ. 0 .AND. SDETECTOR(IDET)%PREVIOUS_STATE .EQ. 1
      IF(SDETECTOR(IDET)%OPERATION_CODE .EQ. 0) THEN
        !!!Presence Mode Detector
        !!!Send a call as long as the vehicle is over the detector.
        CHANNEL_HAS_NEW_CALL = SDETECTOR(IDET)%CURRENT_STATE .EQ. 1      
      ELSE
        !!!Passage Mode Detector
        !!!Send a call when the vehicle has passed the detector.
        CHANNEL_HAS_NEW_CALL = SDETECTOR(IDET)%CURRENT_STATE .EQ. 0 .AND. SDETECTOR(IDET)%PREVIOUS_STATE .EQ. 1
      ENDIF
      
    ELSE
      CHANNEL_HAS_CALL = SDETECTOR(IDET)%CURRENT_STATE_TEMP(ISTEP) .EQ. 1
      IF(ISTEP .EQ. 1) THEN
        CHANNEL_HAD_CALL = SDETECTOR(IDET)%CURRENT_STATE_TEMP(ISTEP) .EQ. 0 .AND. SDETECTOR(IDET)%PREVIOUS_STATE_TEMP(ISTEP) .EQ. 1
      ELSE
        CHANNEL_HAD_CALL = SDETECTOR(IDET)%CURRENT_STATE_TEMP(ISTEP) .EQ. 0 .AND. SDETECTOR(IDET)%CURRENT_STATE_TEMP(ISTEP-1) .EQ. 1
      ENDIF
      IF(SDETECTOR(IDET)%OPERATION_CODE .EQ. 0) THEN
        !!!Presence Mode Detector
        !!!Send a call as long as the vehicle is over the detector.
        CHANNEL_HAS_NEW_CALL = SDETECTOR(IDET)%CURRENT_STATE_TEMP(ISTEP) .EQ. 1      
      ELSE
        !!!Passage Mode Detector
        !!!Send a call when the vehicle has passed the detector.
        CHANNEL_HAS_NEW_CALL = SDETECTOR(IDET)%CURRENT_STATE_TEMP(ISTEP) .EQ. 0 .AND. SDETECTOR(IDET)%PREVIOUS_STATE_TEMP(ISTEP) .EQ. 1
      ENDIF
    ENDIF
      
    !--  check to see if a new call happened
    IF(CHANNEL_HAS_NEW_CALL) THEN 
      CALLING_PHASE = SDETECTOR(IDET)%ASSOCIATED_PHASE
      AC_SIGNALS(IACT)%HAS_DEMAND(CALLING_PHASE) = .TRUE.
      
#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'control_actuated_intersection message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i2, a, i2)') '  sensor id ', detect_index, ' has a new call for phase ', calling_phase
call sendtextmsg(m_info)
endif
#endif 
  
      IF(.NOT. PHASE_IS_GREEN(IACT, CALLING_PHASE)) THEN
        
#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'control_actuated_intersection message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i2, a)') '  adding phase ', calling_phase, ' to queue'
call sendtextmsg(m_info)
endif
#endif
        !!!IF(AC_SIGNALS(IACT)%YELLOW_LOCK_CODE(CALLING_PHASE) .EQ. 1 .OR. &
        !!!  (AC_SIGNALS(IACT)%RED_LOCK_CODE(CALLING_PHASE) .EQ. 1 .AND. PHASE_IS_RED(IACT, CALLING_PHASE))) THEN
          IF(.NOT. AC_SIGNALS(IACT)%SDP%QUEUED(CALLING_PHASE)) THEN
            CALL QUEUE_PHASE(IACT, CALLING_PHASE)
            AC_SIGNALS(IACT)%QUEUED_PHASES = AC_SIGNALS(IACT)%QUEUED_PHASES + 1
          ENDIF
      !!!  ENDIF
      ELSE

        !--  if the mode is recall or presence and the phase was green, 
        !--  skip adding to the queue
        
        !IMODE = AC_SIGNALS(IACT)%SDP%CONTROL_MODE                     !!!What?
        IMODE = AC_SIGNALS(IACT)%SDP%ACTUATED_MODE(CALLING_PHASE)      !!!Should it be this?
        IF(IMODE .EQ. PRESENCE_MODE .OR. IMODE .EQ. RECALL_MIN .OR. IMODE .EQ. RECALL_MAX) THEN
          
#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'control_actuated_intersection message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i2, a)') '  skipping phase ', calling_phase, ' already in green'
call sendtextmsg(m_info)
endif
#endif     

          !!!Extend the green time if the phase is in presence or min recall mode and it hasn't reached maximum green yet.
          IF(IMODE .EQ. PRESENCE_MODE .OR. IMODE .EQ. RECALL_MIN) THEN
            IF(AC_SIGNALS(IACT)%SDP%PHASE_TIMER(CALLING_PHASE) .LT. AC_SIGNALS(IACT)%SDP%MAXIMUM_GREEN_TIME(CALLING_PHASE)) THEN
              AC_SIGNALS(IACT)%SDP%GREEN_TIME(CALLING_PHASE) = MAX(AC_SIGNALS(IACT)%SDP%GREEN_TIME(CALLING_PHASE), &
                AC_SIGNALS(IACT)%SDP%PHASE_TIMER(CALLING_PHASE) + AC_SIGNALS(IACT)%SDP%EXTENSION_TIME(CALLING_PHASE))
            ENDIF
          ENDIF
          
        ELSE  !--  the mode is pulse
                         
          !--  new call resets the timer value to zero
          CALL RESET_PASSAGE_TIMER(IACT, CALLING_PHASE)
          
        ENDIF
      ENDIF !--  phase is green

    ELSEIF(CHANNEL_HAD_CALL) THEN
      
#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'control_actuated_intersection message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i2, a)') '  sensor id ', detect_index, ' had a call'
call sendtextmsg(m_info)
endif
#endif

      CALLING_PHASE = SDETECTOR(IDET)%ASSOCIATED_PHASE

      !--  the channel had a call and doesn't any longer.  Reset the
      !--  phases passage timer to zero if its running.  We could test
      !--  for max out or gap out mode here but its just as fast not to
      !--  and less code
      IF(AC_SIGNALS(IACT)%SDP%PASSAGE_TIMER(CALLING_PHASE) .NE. TIMER_STOPPED) THEN
        
#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'control_actuated_intersection message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i2)') '  restarting passage timer because of call on phase', calling_phase
call sendtextmsg(m_info)
endif
#endif

        CALL RESET_PASSAGE_TIMER(IACT, CALLING_PHASE)
        
      ENDIF !-- timer stopped
          
      !!!If the detector is operating in presence mode and has no calls, remove the calling phase from the queue.
      IF(SDETECTOR(IDET)%OPERATION_CODE .EQ. 0) THEN
        CALL DEQUEUE_PHASE(IACT, CALLING_PHASE)
      ENDIF
      
    ELSEIF(CHANNEL_HAS_CALL) THEN
      !!!Nothing done here???
      
    ENDIF !--  new call
  ENDDO !--  number of detectors.

  !Save phase data in case there is a master phase situation.
  !If there is a master phase situation make sure the master phase is processed first.
  FIRST_RING = 1
  SECOND_RING = 2
  DO RING = RING_1, RING_2  
    PHASE = AC_SIGNALS(IACT)%CURRENT_PHASES(RING)
    IF(PHASE .NE. 0) THEN
      
      !If the phase has just ended wait for gap, it should terminate with a yellow/red phase,
      !so modify the phase timer so the yellow phase starts
      IF(AC_SIGNALS(IACT)%SDP%PREVIOUS_WAIT_FOR_GAP(PHASE) .AND. .NOT. AC_SIGNALS(IACT)%SDP%WAIT_FOR_GAP(PHASE)) THEN
        AC_SIGNALS(IACT)%SDP%PHASE_TIMER(PHASE) = AC_SIGNALS(IACT)%SDP%TIME_LIMIT(PHASE) - AC_SIGNALS(IACT)%SDP%RC(PHASE) - AC_SIGNALS(IACT)%SDP%YC(PHASE)
      ENDIF

      !Save a flag to indicate the previous wait for gap status.
      AC_SIGNALS(IACT)%SDP%PREVIOUS_WAIT_FOR_GAP(PHASE) = AC_SIGNALS(IACT)%SDP%WAIT_FOR_GAP(PHASE)

      IF(AC_SIGNALS(IACT)%SDP%MASTER_PHASE(PHASE) .NE. 0) THEN
        SECOND_RING = RING
        FIRST_RING = THE_OTHER_RING(RING)
      ENDIF
    ENDIF
  ENDDO
  
  !--
  !--  at this point, actuators have been processed 
  !--  Now, execute phases in each ring.
  !-- 

  DO IRING = RING_1, RING_2
    IF(IRING .EQ. 1) THEN
      RING = FIRST_RING
    ELSE
      RING = SECOND_RING
    ENDIF
    PHASE = AC_SIGNALS(IACT)%CURRENT_PHASES(RING)
    
    IF(PHASE .NE. 0) THEN

      IF(AC_SIGNALS(IACT)%SDP%MASTER_PHASE(PHASE) .NE. 0) THEN
        
        MASTER_PHASE = AC_SIGNALS(IACT)%SDP%MASTER_PHASE(PHASE)
        TIME_IN_PHASE = AC_SIGNALS(IACT)%SDP%PHASE_TIMER(MASTER_PHASE)
        TIME_LIMIT = AC_SIGNALS(IACT)%SDP%TIME_LIMIT(MASTER_PHASE)
        WAITFORGAP = AC_SIGNALS(IACT)%SDP%WAIT_FOR_GAP(MASTER_PHASE) &
                .OR. AC_SIGNALS(IACT)%SDP%CURRENT_COLORS(THE_OTHER_RING(RING)) .EQ. GREEN
        
      ELSE
      
        !--  this reflects whether we're using NCHRP 3-66 concepts
        IMODE = AC_SIGNALS(IACT)%SDP%ACTUATED_MODE(PHASE)
        GAP_OUT_MODE = IMODE .EQ. GAP_OUT
        MAX_OR_GAP_MODE = GAP_OUT_MODE .OR. IMODE .EQ. MAX_OUT

        !--  green time may have changed
        IF(.NOT. AC_SIGNALS(IACT)%LOCKED(RING) .AND. PHASE_IS_GREEN(IACT, PHASE)) THEN
          AC_SIGNALS(IACT)%SDP%TIME_LIMIT(PHASE) = AC_SIGNALS(IACT)%SDP%GREEN_TIME(PHASE) + &
            AC_SIGNALS(IACT)%SDP%YC(PHASE) + AC_SIGNALS(IACT)%SDP%RC(PHASE)
        ENDIF 

        TIME_IN_PHASE = AC_SIGNALS(IACT)%SDP%PHASE_TIMER(PHASE)
        TIME_LIMIT = AC_SIGNALS(IACT)%SDP%TIME_LIMIT(PHASE)
        WAITFORGAP = AC_SIGNALS(IACT)%SDP%WAIT_FOR_GAP(PHASE)
      
        !If the signal is coordinated use the force off times to limit non-coordinated phases.
        FORCE_YELLOW = .FALSE.
        IF(AC_SIGNALS(IACT)%CYCLE_LENGTH .NE. 0. .AND. PHASE_IS_GREEN(IACT, PHASE)) THEN
          IF(PHASE .NE. 2 .AND. PHASE .NE. 6) THEN
            IF(AC_SIGNALS(IACT)%LOCAL_CYCLE_TIMER .GE. AC_SIGNALS(IACT)%FORCE_OFF_TIME(PHASE)) THEN
              !Set parameters to force the phase to go into yellow immediately.
              AC_SIGNALS(IACT)%SDP%WAIT_FOR_GAP(PHASE) = .FALSE.
              WAITFORGAP = .FALSE.
              TIME_LIMIT = TIME_IN_PHASE + AC_SIGNALS(IACT)%SDP%YC(PHASE) + AC_SIGNALS(IACT)%SDP%RC(PHASE)      
              AC_SIGNALS(IACT)%SDP%TIME_LIMIT(PHASE) = TIME_LIMIT
              AC_SIGNALS(IACT)%LOCKED(RING) = .TRUE.
              FORCE_YELLOW = .TRUE.
            ENDIF
          ENDIF
          
        ELSEIF(AC_SIGNALS(IACT)%TERMINATE_FLAG(RING)) THEN
          !If the phase must terminate set parameters to force the phase to go into yellow immediately.
          IF(AC_SIGNALS(IACT)%SDP%PHASE_TIMER(PHASE) .GE. AC_SIGNALS(IACT)%SDP%MINIMUM_GREEN_TIME(PHASE)) THEN
            AC_SIGNALS(IACT)%SDP%WAIT_FOR_GAP(PHASE) = .FALSE.
            AC_SIGNALS(IACT)%SDP%PHASE_TIMER(PHASE) = TIME_LIMIT - AC_SIGNALS(IACT)%SDP%RC(PHASE) - AC_SIGNALS(IACT)%SDP%YC(PHASE)      
            TIME_IN_PHASE = AC_SIGNALS(IACT)%SDP%PHASE_TIMER(PHASE)
            WAITFORGAP = .FALSE.
            AC_SIGNALS(IACT)%TERMINATE_FLAG(RING) = .FALSE.
          ENDIF
        
        ELSEIF(USE_DCS .AND. .NOT. INITMODE) THEN
          IF(BTEST(EXTENSION_GROUP, PHASE-1)) THEN
            WAITFORGAP = .TRUE.
          ELSEIF(BTEST(FORCEOFF_GROUP, PHASE-1)) THEN
            FORCEOFF_GROUP = IBCLR(FORCEOFF_GROUP, PHASE-1)
            IF(PHASE_IS_GREEN(IACT, PHASE)) THEN
              !Set parameters to force the phase to go into yellow
              AC_SIGNALS(IACT)%SDP%WAIT_FOR_GAP(PHASE) = .FALSE.
              AC_SIGNALS(IACT)%SDP%PHASE_TIMER(PHASE) = TIME_LIMIT - AC_SIGNALS(IACT)%SDP%RC(PHASE) - AC_SIGNALS(IACT)%SDP%YC(PHASE)      
              TIME_IN_PHASE = AC_SIGNALS(IACT)%SDP%PHASE_TIMER(PHASE)
              WAITFORGAP = .FALSE.
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      
#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'control_actuated_intersection message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i2)') '  phase ', phase
call sendtextmsg(m_info)
write(msgtext, '(a, f5.1)') '  green time ', ac_signals(iact)%sdp%green_time(phase)
call sendtextmsg(m_info)
write(msgtext, '(a, f5.1)') '  split ', ac_signals(iact)%sdp%time_limit(phase)
call sendtextmsg(m_info)
write(msgtext, '(a, f6.1)') '  phase timer ', ac_signals(iact)%sdp%PHASE_TIMER(phase)
call sendtextmsg(m_info)
endif
#endif

      !--
      !--  phase completed
      !--

      !!!Compensate for floating point precision errors. Eliminate data less than 0.01 second.
      TIME_LIMIT = NINT(TIME_LIMIT * 100) / 100.
      TIME_IN_PHASE = NINT(TIME_IN_PHASE * 100) / 100.
      
      IF(TIME_IN_PHASE .GE. TIME_LIMIT .AND. .NOT. WAITFORGAP) THEN
        
        !If the phase does not have an all red period it may not have turned red yet.
        IF(.NOT. PHASE_IS_RED(IACT, PHASE)) THEN
          CALL ACTUATED_RED_PROCESSING(IACT, RING, PHASE, MAX_OR_GAP_MODE)
        ENDIF
        
        !If the phase is ending because the master phase ended just terminate the phase
        !and set the ready to cross flag if the other ring is ready to cross
        IF(AC_SIGNALS(IACT)%SDP%MASTER_PHASE(PHASE) .NE. 0) THEN
          AC_SIGNALS(IACT)%CURRENT_PHASES(RING) = 0
          IF(.NOT. AC_SIGNALS(IACT)%READY_TO_CROSS(RING)) THEN
            AC_SIGNALS(IACT)%READY_TO_CROSS(RING) = AC_SIGNALS(IACT)%READY_TO_CROSS(THE_OTHER_RING(RING))
          ENDIF
          CALL ACTUATED_PHASE_COMPLETED(IACT, RING, PHASE)
          AC_SIGNALS(IACT)%RING_INDEX(RING) = AC_SIGNALS(IACT)%RING_INDEX(THE_OTHER_RING(RING))
        ELSE
          CALL ACTUATED_PHASE_COMPLETED(IACT, RING, PHASE)
        ENDIF
        
        !Reset the red clearance time in case it was set to red revert time
        !!!AC_SIGNALS(IACT)%SDP%RC(PHASE) = AC_SIGNALS(IACT)%GUI_RC(PHASE)
        
      !--
      !--  phase red
      !--
      ELSEIF(TIME_IN_PHASE .GE. TIME_LIMIT - AC_SIGNALS(IACT)%SDP%RC(PHASE) .AND. .NOT. WAITFORGAP) THEN

        CALL ACTUATED_RED_PROCESSING(IACT, RING, PHASE, MAX_OR_GAP_MODE)

      !--
      !--  phase yellow
      !--
      ELSEIF(FORCE_YELLOW .OR. (TIME_IN_PHASE .GE. TIME_LIMIT - AC_SIGNALS(IACT)%SDP%YC(PHASE) - AC_SIGNALS(IACT)%SDP%RC(PHASE) .AND. .NOT. WAITFORGAP)) THEN

        IF(FORCE_YELLOW .OR. PHASE_IS_YELLOW(IACT, PHASE)) THEN
          !If the phase has already gone into yellow continue with yellow.
          CALL ACTUATED_YELLOW_PROCESSING(IACT, RING, PHASE)
        ELSE
          !if(ac_signals(iact)%cycle_length .ne. 0) then
          !  if(phase_is_green(iact, phase)) then
          !    if(phase .eq. 2) then
          !      write(msgtext, '(a,i4,a,f10.1)') 'node : ',  ac_signals(iact)%node(1), ' ---ending coordinated green at ', simtime
          !      call sendtextmsg(m_info)
          !    endif
          !  endif
          !endif
          !If the yellow is going to start, determine if the phase can be continued.
          NEW_PHASE = 0
          !If the terminating phase is a coordinated phase it must terminate now.
          MUST_TERMINATE = .FALSE.
          IF(AC_SIGNALS(IACT)%CYCLE_LENGTH .NE. 0) THEN
            IF(PHASE .EQ. 2 .OR. PHASE .EQ. 6) THEN
              MUST_TERMINATE = .TRUE.
            ENDIF
          ENDIF
          IF(.NOT. MUST_TERMINATE) THEN
            IF(AC_SIGNALS(IACT)%SDP%SLAVE_PHASE(PHASE) .NE. 0) THEN 
              MUST_TERMINATE = .TRUE.
            ENDIF
          ENDIF
          OTHER_PHASE = AC_SIGNALS(IACT)%CURRENT_PHASES(THE_OTHER_RING(RING))
          IF(.NOT. MUST_TERMINATE) THEN
            IF(OTHER_PHASE .NE. 0) THEN
              IF(PHASE_IS_GREEN(IACT, OTHER_PHASE)) THEN
                !The other phase is still green, so see which phase will be started next on this ring.
                CALL CHECK_SINGLE_PHASE_RESTART(IACT, PHASE, THE_OTHER_RING(RING), OTHER_PHASE, .FALSE., NEW_PHASE)
              ENDIF
            ENDIF
          ENDIF
          !experimental code to prevent unnecessary yellow and red when signal should stay green
          !if(new_phase .eq. 0) then
          !  if(ac_signals(iact)%ring_index(the_other_ring(ring)) .lt. ac_signals(iact)%ring_index(ring)) then
          !    new_phase = phase
          !  endif
          !endif
          IF(NEW_PHASE .EQ. PHASE) THEN
            !!!IF(AC_SIGNALS(IACT)%RED_REVERT_TIME(PHASE) .NE. 0) THEN
            !!!  AC_SIGNALS(IACT)%NEXT_PHASE(RING) = NEW_PHASE
            !!!  AC_SIGNALS(IACT)%SDP%RC(PHASE) = MAX(AC_SIGNALS(IACT)%SDP%RC(PHASE), AC_SIGNALS(IACT)%RED_REVERT_TIME(PHASE))
            !!!  CALL ACTUATED_YELLOW_PROCESSING(IACT, RING, PHASE)              
            !!!ELSE
              !The next phase will be the same as the current phase, so
              !extend the current phase instead of starting yellow.
              !Define the running phase to be the master of the new phase, so that
              !the new phase will not run longer than the master phase.
              IF(OTHER_PHASE .NE. 0) THEN
                !Clear any current master/slave situation.
                AC_SIGNALS(IACT)%SDP%MASTER_PHASE = 0
                AC_SIGNALS(IACT)%SDP%SLAVE_PHASE = 0
                AC_SIGNALS(IACT)%SDP%MASTER_PHASE(PHASE) = OTHER_PHASE
                AC_SIGNALS(IACT)%SDP%SLAVE_PHASE(OTHER_PHASE) = PHASE
              ENDIF
              IMODE = AC_SIGNALS(IACT)%SDP%ACTUATED_MODE(PHASE)
              AC_SIGNALS(IACT)%SDP%WAIT_FOR_GAP(PHASE) = IMODE .EQ. MAX_OUT .OR. IMODE .EQ. GAP_OUT
              IF(.NOT. AC_SIGNALS(IACT)%READY_TO_CROSS(RING)) THEN
                AC_SIGNALS(IACT)%READY_TO_CROSS(RING) = AC_SIGNALS(IACT)%READY_TO_CROSS(THE_OTHER_RING(RING))
              ENDIF
              AC_SIGNALS(IACT)%RING_INDEX(RING) = &
                MAX(AC_SIGNALS(IACT)%RING_INDEX(RING), AC_SIGNALS(IACT)%RING_INDEX(THE_OTHER_RING(RING)))
            !!!ENDIF
          ELSE
            OTHER_PHASE = AC_SIGNALS(IACT)%CURRENT_PHASES(THE_OTHER_RING(RING))
            IF(OTHER_PHASE .NE. 0) THEN
              IF(AC_SIGNALS(IACT)%SDP%MASTER_PHASE(OTHER_PHASE) .EQ. PHASE) THEN
                !The phase going into yellow is the master of the other phase.
                !Force the other phase into yellow.
                IF(PHASE_IS_GREEN(IACT, OTHER_PHASE)) THEN
                  CALL ACTUATED_YELLOW_PROCESSING(IACT, THE_OTHER_RING(RING), OTHER_PHASE)
                ENDIF
              ENDIF
            ENDIF
            AC_SIGNALS(IACT)%NEXT_PHASE(RING) = NEW_PHASE
            CALL ACTUATED_YELLOW_PROCESSING(IACT, RING, PHASE)
          ENDIF
        ENDIF
          
      !--
      !--  phase green
      !--
      ELSEIF((TIME_IN_PHASE .GE. 0.0 .AND. TIME_IN_PHASE .LT. TIME_LIMIT) .OR. WAITFORGAP) THEN 

        CALL PEDESTRIAN_ADJUST(IACT, RING, PHASE)
        CALL ACTUATED_GREEN_PROCESSING(IACT, RING, PHASE, MAX_OR_GAP_MODE, GAP_OUT_MODE)

      ENDIF !-- phase completed
    ENDIF !-- phase running
  ENDDO !--  rings

  IF(SUM(AC_SIGNALS(IACT)%CURRENT_PHASES) .EQ. 0) THEN
    !Determine if both rings are ready to cross the barrier.
    DO RING = 1, 2
      IF(AC_SIGNALS(IACT)%READY_TO_CROSS(RING)) CYCLE
      IF(SUM(AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(RING,1:4)) .EQ. 0) THEN
        AC_SIGNALS(IACT)%READY_TO_CROSS(RING) = .TRUE.
        AC_SIGNALS(IACT)%RING_INDEX(RING) = AC_SIGNALS(IACT)%RING_INDEX(THE_OTHER_RING(RING))
      ENDIF
    ENDDO
    IF(AC_SIGNALS(IACT)%READY_TO_CROSS(1) .AND. AC_SIGNALS(IACT)%READY_TO_CROSS(2)) THEN
      AC_SIGNALS(IACT)%READY_TO_CROSS = .FALSE.
    
      !Determine if there are calls or recalls on the next barrier.
      CALLS_ON_NEXT_BARRIER = .FALSE.
      INDEX = AC_SIGNALS(IACT)%RING_INDEX(1)
      IF(INDEX .LE. AC_SIGNALS(IACT)%MAX_INDEX(1, 1)) THEN
        INDEX = 1
      ELSE
        INDEX = AC_SIGNALS(IACT)%MAX_INDEX(1, 1) + 1
      ENDIF
      DO IPHASE = INDEX, INDEX + 1
        PHASE = AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(1, IPHASE)
        IF(PHASE .NE. 0) THEN
          IMODE = AC_SIGNALS(IACT)%SDP%ACTUATED_MODE(PHASE)
          IF(AC_SIGNALS(IACT)%SDP%QUEUED(PHASE) .OR. IMODE .EQ. RECALL_MIN .OR. IMODE .EQ. RECALL_MAX) THEN
            CALLS_ON_NEXT_BARRIER = .TRUE.
            EXIT
          ENDIF
        ENDIF
        PHASE = AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(2, IPHASE)
        IF(PHASE .NE. 0) THEN
          IMODE = AC_SIGNALS(IACT)%SDP%ACTUATED_MODE(PHASE)
          IF(AC_SIGNALS(IACT)%SDP%QUEUED(PHASE) .OR. IMODE .EQ. RECALL_MIN .OR. IMODE .EQ. RECALL_MAX) THEN
            CALLS_ON_NEXT_BARRIER = .TRUE.
            EXIT
          ENDIF
        ENDIF
      ENDDO

      !There are no calls or recalls on the other barrier.
      !Reset the phase for this ring to the first phase of the current barrier.
      IF(.NOT. CALLS_ON_NEXT_BARRIER) THEN      
        IF(INDEX .EQ. AC_SIGNALS(IACT)%MAX_INDEX(1, 1) + 1) THEN
          INDEX = 1
        ELSE
          INDEX = AC_SIGNALS(IACT)%MAX_INDEX(1, 1) + 1
        ENDIF
        AC_SIGNALS(IACT)%RING_INDEX = INDEX
      ENDIF
    
      !Determine if this is the start of a new cycle.
      IF((AC_SIGNALS(IACT)%CYCLE_LENGTH .EQ. 0. .AND. INDEX .EQ. 1) .OR. &
         (AC_SIGNALS(IACT)%CYCLE_LENGTH .NE. 0. .AND. INDEX .EQ. 3)) THEN
        AC_SIGNALS(IACT)%SERVED_THIS_CYCLE = .FALSE.
      ENDIF
    
      !Try to start the first phase of the barrier in each ring.
      DO RING = 1, 2
        NEXT_PHASE = AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(RING, INDEX)
        IF(NEXT_PHASE .NE. 0) THEN
          IMODE = AC_SIGNALS(IACT)%SDP%ACTUATED_MODE(NEXT_PHASE)
          IF(AC_SIGNALS(IACT)%SDP%QUEUED(NEXT_PHASE) .OR. IMODE .EQ. RECALL_MIN .OR. IMODE .EQ. RECALL_MAX) THEN
            IF(PHASE_IS_ALLOWED(IACT, NEXT_PHASE)) CALL START_ACTUATED_PHASE(IACT, RING, NEXT_PHASE)
          ENDIF
        ENDIF
      ENDDO
    
      !If no phases were started increment the ring index and try to start the next phase in each ring.
      IF(SUM(AC_SIGNALS(IACT)%CURRENT_PHASES) .EQ. 0) THEN
        AC_SIGNALS(IACT)%RING_INDEX = INDEX + 1
        DO RING = 1, 2
          NEXT_PHASE = AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(RING, INDEX+1)
          IF(NEXT_PHASE .EQ. 0) THEN
            AC_SIGNALS(IACT)%RING_INDEX(RING) = AC_SIGNALS(IACT)%RING_INDEX(RING) + 1
            IF(AC_SIGNALS(IACT)%RING_INDEX(RING) .GT. 4) AC_SIGNALS(IACT)%RING_INDEX(RING) = 1
          ELSE
            IMODE = AC_SIGNALS(IACT)%SDP%ACTUATED_MODE(NEXT_PHASE)
            IF(AC_SIGNALS(IACT)%SDP%QUEUED(NEXT_PHASE) .OR. IMODE .EQ. RECALL_MIN .OR. IMODE .EQ. RECALL_MAX) THEN
              IF(PHASE_IS_ALLOWED(IACT, NEXT_PHASE)) CALL START_ACTUATED_PHASE(IACT, RING, NEXT_PHASE)
            ENDIF
          ENDIF
        ENDDO
      ENDIF    
    ENDIF
    
    !If no phase was started reset the flags to cross the barrier.
    IF(SUM(AC_SIGNALS(IACT)%CURRENT_PHASES) .EQ. 0) THEN
      AC_SIGNALS(IACT)%READY_TO_CROSS = .TRUE.
      INDEX = MAXVAL(AC_SIGNALS(IACT)%RING_INDEX)
      IF(INDEX .LT. 3) THEN
        AC_SIGNALS(IACT)%RING_INDEX = 1
      ELSE
        AC_SIGNALS(IACT)%RING_INDEX = 3
      ENDIF
    ENDIF
  ENDIF
  
  !If there is only one phase active determine if another phase should be started.
  NPHASE = 0
  OTHER_PHASE = 0
  IRING = 0
  DO RING = 1, 2
    IF(AC_SIGNALS(IACT)%CURRENT_PHASES(RING) .NE. 0) THEN
      NPHASE = NPHASE + 1
      OTHER_PHASE = AC_SIGNALS(IACT)%CURRENT_PHASES(RING)
      IRING = RING
    ENDIF
  ENDDO
  IF(NPHASE .EQ. 1) THEN
    RING = THE_OTHER_RING(IRING)
    IF(AC_SIGNALS(IACT)%NEXT_PHASE(RING) .NE. 0) THEN
      NEXT_PHASE = AC_SIGNALS(IACT)%NEXT_PHASE(RING)
      IF(.NOT. CONFLICTS(PHASE, NEXT_PHASE) .AND. PHASE_IS_ALLOWED(IACT, NEXT_PHASE)) THEN
        IF(AC_SIGNALS(IACT)%SDP%WAIT_FOR_GAP(OTHER_PHASE) .AND. .NOT. AC_SIGNALS(IACT)%SDP%GAPOUT(OTHER_PHASE)) THEN
          !If the phase is being extended and has not gapped out calculate the maximum time remaining.
          REMAINING_GREEN_TIME = AC_SIGNALS(IACT)%SDP%MAXIMUM_GREEN_TIME(OTHER_PHASE) - AC_SIGNALS(IACT)%SDP%PHASE_TIMER(OTHER_PHASE)
        ELSEIF(AC_SIGNALS(IACT)%SDP%WAIT_FOR_GAP(OTHER_PHASE)) THEN
          !If the phase is being extended but has gapped out calculate the time remaining in the extension.
          REMAINING_GREEN_TIME = AC_SIGNALS(IACT)%SDP%TIME_PLUS_EXTENSION(OTHER_PHASE) - AC_SIGNALS(IACT)%SDP%PHASE_TIMER(OTHER_PHASE)
        ELSE
          !Calculate the time remaining in the phase.
          REMAINING_GREEN_TIME = AC_SIGNALS(IACT)%SDP%GREEN_TIME(OTHER_PHASE) - AC_SIGNALS(IACT)%SDP%PHASE_TIMER(OTHER_PHASE)
        ENDIF
        IF(REMAINING_GREEN_TIME .GE. 3.0) THEN
          CALL START_ACTUATED_PHASE(IACT, RING, NEXT_PHASE)
          IF(AC_SIGNALS(IACT)%SERVED_THIS_CYCLE(NEXT_PHASE)) THEN
            !Do not allow the new phase to run longer than the current phase.
            !Define the running phase to be the master of the new phase.
            AC_SIGNALS(IACT)%SDP%MASTER_PHASE(NEXT_PHASE) = OTHER_PHASE
            AC_SIGNALS(IACT)%SDP%SLAVE_PHASE(OTHER_PHASE) = NEXT_PHASE
            IF(.NOT. AC_SIGNALS(IACT)%READY_TO_CROSS(RING)) THEN
              AC_SIGNALS(IACT)%READY_TO_CROSS(RING) = AC_SIGNALS(IACT)%READY_TO_CROSS(IRING)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      AC_SIGNALS(IACT)%NEXT_PHASE(RING) = 0
    ELSE
      !!!START_FLAG = AC_SIGNALS(IACT)%DUAL_ENTRY_CODE(OTHER_PHASE) .EQ. 1
      START_FLAG = .TRUE.
      CALL CHECK_SINGLE_PHASE_RESTART(IACT, 0, IRING, OTHER_PHASE, START_FLAG, NEW_PHASE)
    ENDIF
  ENDIF
  
  IF(USE_DCS) THEN
    GREEN_PHASES(IACT) = 0
    YELLOW_PHASES(IACT) = 0
    DO RING = RING_1, RING_2  
      PHASE = AC_SIGNALS(IACT)%CURRENT_PHASES(RING)
      IF(PHASE .NE. 0) THEN
        IF(USE_DCS .AND. .NOT. INITMODE) THEN
          IF(AC_SIGNALS(IACT)%SDP%CURRENT_COLORS(RING) .EQ. GREEN) THEN      
            GREEN_PHASES(IACT) = IBSET(GREEN_PHASES(IACT), PHASE-1)
          ELSEIF(AC_SIGNALS(IACT)%SDP%CURRENT_COLORS(RING) .EQ. YELLOW) THEN      
            YELLOW_PHASES(IACT) = IBSET(YELLOW_PHASES(IACT), PHASE-1)
          ENDIF
        ENDIF
      ENDIF
    ENDDO
  ENDIF

  IF(AC_SIGNALS(IACT)%CYCLE_LENGTH .NE. 0.) THEN
    IF(SIMTIME .NE. 0) THEN
      CALL MASTER(IACT)
      CALL LOCAL(IACT)
    ENDIF
  ENDIF
  
!Write log file for NTCIP comparison
  !IF(FIRST) THEN
  !  OPEN(1, FILE = LINFNAME(1:IROOT-1)//'.LOGX', STATUS='REPLACE')
  !  FIRST = .FALSE.
  !  WRITE(1, '(A)') 'Timing Plan'
  !  DO I = 1, 8
  !    WRITE(1, '(A,I1,A,F5.1,A,F5.1)') 'Phase ', I, ' Min Green = ', AC_SIGNALS(IACT)%GUI_MIN_GREEN_TIMES(I), &
  !                   ' Max Green = ', AC_SIGNALS(IACT)%GUI_MAX_GREEN_TIMES(I)
  !  ENDDO
  !  WRITE(1, '(A,F8.1)') 'Timestep: ', SCOPE_TIMESTEP
  !ELSE
  !  OPEN(1, FILE = LINFNAME(1:IROOT-1)//'.LOGX', ACCESS='APPEND')  
  !ENDIF
  !WRITE(1, '(A,F8.1,A,I8)') 'Time: ', SIMTIME, ' GREEN PHASES: ', GREENS
  !WRITE(1, '(A,F8.1,A,I8)') 'Time: ', SIMTIME, ' YELLOW PHASES: ', YELLOWS
  !CLOSE(1)
  RETURN
  END
    
! ==================================================================================================
  SUBROUTINE START_INITIAL_PHASES(IACT)
!----------------------------------------------------------------------
!--- SCOPE
!--  Method : Start Initial Phases
!--  Date   : Apr 4, 2011
!--
!--  Purpose
!--
!--  Start the call queue processing
!--
!--
!--  Change History
!--
!--  DATE                 Description
!--
!--  Apr 04, 2011         Initial Version
! -- August, 2014         Ported to ETFOMM
! -- October, 2014        Revised for ETFOMM
!----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE SCOPE_PARAMETERS
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT
  INTEGER :: RING, PHASE
!----------------------------------------------------------------------
  !--  start the 1st two phases
  IF(AC_SIGNALS(IACT)%OFFSET .NE. 0) THEN
    RING = 1
    PHASE = 2
    IF(AC_SIGNALS(IACT)%PHASE(PHASE)%IN_USE) THEN
      AC_SIGNALS(IACT)%RING_INDEX(RING) = PHASE
      CALL START_ACTUATED_PHASE(IACT, RING, PHASE)
    ENDIF
    RING = 2
    PHASE = 6
    IF(AC_SIGNALS(IACT)%PHASE(PHASE)%IN_USE) THEN
      AC_SIGNALS(IACT)%RING_INDEX(RING) = PHASE - 4
      CALL START_ACTUATED_PHASE(IACT, RING, PHASE)
    ENDIF
  ELSE
    PHASE = AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(1, 1)
    AC_SIGNALS(IACT)%RING_INDEX(1) = PHASE
    IF(PHASE .NE. 0) THEN
      CALL START_ACTUATED_PHASE(IACT, 1, PHASE)
      AC_SIGNALS(IACT)%SDP%TIME_LIMIT(PHASE) = AC_SIGNALS(IACT)%SDP%TIME_LIMIT(PHASE) + AC_SIGNALS(IACT)%OFFSET
    ENDIF
    PHASE = AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(2, 1)
    AC_SIGNALS(IACT)%RING_INDEX(2) = PHASE - 4
    IF(PHASE .NE. 0) THEN
      CALL START_ACTUATED_PHASE(IACT, 2, PHASE)
    ENDIF
  ENDIF
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE START_ACTUATED_PHASE(IACT, RING, PHASE)
!----------------------------------------------------------------------
!--- SCOPE
!--  Method : Start_Actuated_Phase
!--  Date   : Apr 2, 2011
!--
!--  Purpose
!--
!--  This starts a new phase cycle
!--
!--
!--  Change History
!--
!--  DATE                 Description
!--
!--  Apr 2, 2010          Initial Version
! -- August, 2014         Ported to ETFOMM
!----------------------------------------------------------------------
#ifdef DebugVersion
  use debug
#endif  
  USE GLOBAL_DATA
  USE ACTUATED_CONTROLLERS
  USE SCOPE_PARAMETERS
  USE SIMPARAMS
  USE NTCIP_DATA
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, RING, PHASE
  INTEGER :: IMODE
  LOGICAL :: XEQ
  !INCLUDE 'IOFILES.INC'
!----------------------------------------------------------------------
  !OPEN(1, FILE = LINFNAME(1:IROOT-1)//'.LOGX')
  !WRITE(1, '(A,F8.1,A,I1)') 'Time = ', SIMTIME, ' Starting phase ', PHASE
  !CLOSE(1)
  SBB(RING) = 0
  
  AC_SIGNALS(IACT)%TIMES_STARTED(PHASE) = AC_SIGNALS(IACT)%TIMES_STARTED(PHASE) + 1
  AC_SIGNALS(IACT)%START_TIME(PHASE) = SIMTIME

  !Indicate that the phase has been served during the current cycle.
  AC_SIGNALS(IACT)%SERVED_THIS_CYCLE(PHASE) = .TRUE.
  
  !--  this phase is no longer on the queue
  IF(AC_SIGNALS(IACT)%SDP%QUEUED(PHASE)) THEN
    !If the phase is coordinated, keep it queued.
    IF(XEQ(AC_SIGNALS(IACT)%CYCLE_LENGTH, 0.) .OR. (PHASE  .NE. 2 .AND. PHASE .NE. 6)) THEN
      CALL DEQUEUE_PHASE(IACT, PHASE)
      AC_SIGNALS(IACT)%QUEUED_PHASES = AC_SIGNALS(IACT)%QUEUED_PHASES - 1
    ENDIF
  ENDIF
 
  !--  clear the pedestrian active flag
  AC_SIGNALS(IACT)%SDP%PEDESTRIAN_ACTIVE(PHASE) = .FALSE.
 
  !--  start the new phase
  AC_SIGNALS(IACT)%CURRENT_PHASES(RING) = PHASE
 
  !change the light to green.
  AC_SIGNALS(IACT)%SDP%CURRENT_COLORS(RING) = GREEN
  
  !--  time to extend 
  AC_SIGNALS(IACT)%SDP%TIME_PLUS_EXTENSION(PHASE) =  0.0
 
  AC_SIGNALS(IACT)%SDP%GREEN_DONE_ONCE(PHASE) = .FALSE. 
  AC_SIGNALS(IACT)%SDP%GREEN_DONE_FRAME(PHASE) = .FALSE.
 
  AC_SIGNALS(IACT)%SDP%COMPUTED_GAP_TIME(PHASE) = AC_SIGNALS(IACT)%SDP%GAP_TIMES(PHASE)
 
  AC_SIGNALS(IACT)%SDP%GAPOUT(PHASE) = .FALSE.
  !AC_SIGNALS(IACT)%READY_TO_TERMINATE(PHASE) = .FALSE.
 
  !--  make sure the new phase uses its correct green time
  IMODE = AC_SIGNALS(IACT)%SDP%ACTUATED_MODE(PHASE)
  IF(IMODE .EQ. RECALL_MAX) THEN  
    AC_SIGNALS(IACT)%SDP%GREEN_TIME(PHASE) = AC_SIGNALS(IACT)%SDP%MAXIMUM_GREEN_TIME(PHASE)  
  ELSE
    AC_SIGNALS(IACT)%SDP%GREEN_TIME(PHASE) = AC_SIGNALS(IACT)%SDP%MINIMUM_GREEN_TIME(PHASE)
  ENDIF
  
  !--  new phase's split time
  AC_SIGNALS(IACT)%SDP%TIME_LIMIT(PHASE) = AC_SIGNALS(IACT)%SDP%GREEN_TIME(PHASE) + AC_SIGNALS(IACT)%SDP%YC(PHASE) + AC_SIGNALS(IACT)%SDP%RC(PHASE)
 
  AC_SIGNALS(IACT)%LOCKED(RING) = .FALSE.

  !If the signal is coordinated...
  IF(AC_SIGNALS(IACT)%CYCLE_LENGTH .NE. 0.) THEN
    !Determine splits for coordinated phases and lock the phase so it will stay green until the end of the coordination cycle.
    IF(PHASE .EQ. 2 .OR. PHASE .EQ. 6) THEN
      AC_SIGNALS(IACT)%SDP%TIME_LIMIT(PHASE) = AC_SIGNALS(IACT)%COORDINATION_TIMER &
        + AC_SIGNALS(IACT)%GUI_YC(PHASE) + AC_SIGNALS(IACT)%GUI_RC(PHASE) + SCOPE_TIMESTEP
      AC_SIGNALS(IACT)%LOCKED(RING) = .TRUE.
    ENDIF
  ENDIF
 
  !--  only send 1 debug message for reduction if debug is turned on
  AC_SIGNALS(IACT)%SDP%REDUCTION_MESSAGE_SENT(PHASE) = .FALSE.
 
  !--  reset this phase timer
  CALL SET_PHASE_TIMER(IACT, PHASE, 0.)
 
  !--  if the mode is max out or gap out, we won't transition to yellow until that occurs.
  AC_SIGNALS(IACT)%SDP%WAIT_FOR_GAP(PHASE) = IMODE .EQ. MAX_OUT .OR. IMODE .EQ. GAP_OUT

#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'start_actuated_phase message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i1)') '  new phase started => ', phase
call sendtextmsg(m_info)
write(msgtext, '(a, i1)') '  on ring => ', ring
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') '  split time => ', ac_signals(iact)%sdp%time_limit(phase)
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') '  green time => ', ac_signals(iact)%sdp%green_time(phase)
call sendtextmsg(m_info)
if(ac_signals(iact)%sdp%wait_for_gap(phase)) then
  write(msgtext, '(a)') '  wait for gap => true'
else
  write(msgtext, '(a)') '  wait for gap => false'
endif
call sendtextmsg(m_info)
endif
#endif

  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE ACTUATED_PHASE_COMPLETED(IACT, RING, PHASE)
!----------------------------------------------------------------------
!--- SCOPE
!--  Method : ACTUATED_PHASE_COMPLETED
!--
!--  Purpose
!--
!--  At the end of an actuated phases's green time, perform this
!--  processing
!--
!--
!--  Change History
!--
!--  DATE                 Description
!--
!--  March 04, 2011       Initial Version
! -- August, 2014         Ported to ETFOMM
! -- October, 2014        Rewritten
!----------------------------------------------------------------------
#ifdef DebugVersion
  use debug
#endif  
  USE TEXT
  USE ACTUATED_CONTROLLERS
  USE SCOPE_PARAMETERS
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, RING, PHASE
  INTEGER :: NEXT_PHASE, IMODE, INDX
  REAL :: MIN_TOTAL_PHASE_TIME, PHASE_TIME
  !INCLUDE 'IOFILES.INC'
!----------------------------------------------------------------------
  !AC_SIGNALS(IACT)%LOCKED(RING) = .FALSE.
  
  !OPEN(1, FILE = LINFNAME(1:IROOT-1)//'.LOGX', ACCESS='APPEND')
  !WRITE(1, '(A,F8.1,A,I1)') '  Time = ', SIMTIME, ' Ending phase ', PHASE
  !CLOSE(1)
  
#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'actuated_phase_completed message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i1, a)') '  Phase ', phase, ' complete'
call sendtextmsg(m_info)
endif
#endif

  MIN_TOTAL_PHASE_TIME = AC_SIGNALS(IACT)%SDP%MINIMUM_GREEN_TIME(PHASE) + AC_SIGNALS(IACT)%SDP%YC(PHASE) + AC_SIGNALS(IACT)%SDP%RC(PHASE)
  PHASE_TIME = SIMTIME + ISTEP * SCOPE_TIMESTEP - AC_SIGNALS(IACT)%START_TIME(PHASE)
  IF(ABS(MIN_TOTAL_PHASE_TIME - PHASE_TIME) .LT. 1.0) THEN
    AC_SIGNALS(IACT)%MIN_GREENS(PHASE) = AC_SIGNALS(IACT)%MIN_GREENS(PHASE) + 1
  ENDIF
  
  !--  reset this phase timer
  CALL INCREMENT_PHASE_TIMER(IACT, PHASE)

  !--  and keep track of the phase just run
  AC_SIGNALS(IACT)%SDP%PREVIOUS_PHASES(RING) = PHASE
  AC_SIGNALS(IACT)%CURRENT_PHASES(RING) = 0

  !Determine if the ring is ready to cross the barrier.
  INDX = AC_SIGNALS(IACT)%RING_INDEX(RING)
  IF(INDX .EQ. AC_SIGNALS(IACT)%MAX_INDEX(RING, 1) .OR. INDX .EQ. AC_SIGNALS(IACT)%MAX_INDEX(RING, 2)) THEN
    !Set the flag to cross the barrier, set the index to the first phase of the next barrier.
    AC_SIGNALS(IACT)%READY_TO_CROSS(RING) = .TRUE.
    IF(AC_SIGNALS(IACT)%SDP%SLAVE_PHASE(PHASE) .NE. 0) THEN
      AC_SIGNALS(IACT)%READY_TO_CROSS(THE_OTHER_RING(RING)) = .TRUE.
    ENDIF
    IF(INDX .EQ. 2) THEN
      INDX = 3
    ELSE
      INDX = 1
    ENDIF
  ELSEIF(.NOT. AC_SIGNALS(IACT)%READY_TO_CROSS(RING)) THEN
    !Try to start the next phase on the ring in the same barrier.
    INDX = INDX + 1
    NEXT_PHASE = 0
    IF(INDX .LE. 4) NEXT_PHASE = AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(RING, INDX)
    IF(NEXT_PHASE .NE. 0) THEN
      IMODE = AC_SIGNALS(IACT)%SDP%ACTUATED_MODE(NEXT_PHASE)
      IF(AC_SIGNALS(IACT)%SDP%QUEUED(NEXT_PHASE) .OR. IMODE .EQ. RECALL_MIN .OR. IMODE .EQ. RECALL_MAX) THEN
        !If the signal is coordinated and a non-coordinated phase has not been served
        !set the flag to indicate that it has been served, because it was skipped.
        IF(.NOT. AC_SIGNALS(IACT)%SERVED_THIS_CYCLE(NEXT_PHASE) .AND. AC_SIGNALS(IACT)%CYCLE_LENGTH .NE. 0.) THEN
          IF(NEXT_PHASE .NE. 2 .AND. NEXT_PHASE .NE. 6) THEN
            IF(AC_SIGNALS(IACT)%LOCAL_CYCLE_TIMER .GE. AC_SIGNALS(IACT)%FORCE_OFF_TIME(NEXT_PHASE) &
              - AC_SIGNALS(IACT)%GUI_MIN_GREEN_TIMES(NEXT_PHASE)) THEN
              AC_SIGNALS(IACT)%SERVED_THIS_CYCLE(NEXT_PHASE) = .TRUE.
            ENDIF
          ENDIF
        ENDIF
        IF(PHASE_IS_ALLOWED(IACT, NEXT_PHASE) .AND. .NOT. AC_SIGNALS(IACT)%SERVED_THIS_CYCLE(NEXT_PHASE)) THEN
          CALL START_ACTUATED_PHASE(IACT, RING, NEXT_PHASE)
        ELSE
          AC_SIGNALS(IACT)%READY_TO_CROSS(RING) = .TRUE.
        ENDIF
      ELSE
        !The phase wasn't started, so the ring is ready to cross the barrier.
        AC_SIGNALS(IACT)%READY_TO_CROSS(RING) = .TRUE.
        IF(INDX .EQ. 2) THEN
          INDX = 3
        ELSE
          INDX = 1
        ENDIF
      ENDIF
    ELSE
      AC_SIGNALS(IACT)%READY_TO_CROSS(RING) = .TRUE.
    ENDIF
  ELSE
    !Set INDX to the last phase in the barrier.
    IF(INDX .LE. AC_SIGNALS(IACT)%MAX_INDEX(1, 1)) THEN
      INDX = AC_SIGNALS(IACT)%MAX_INDEX(1, 1)
    ELSE
      INDX = AC_SIGNALS(IACT)%MAX_INDEX(1, 2)
    ENDIF
  ENDIF
  AC_SIGNALS(IACT)%RING_INDEX(RING) = INDX

  RETURN
  END

! ==================================================================================================
  SUBROUTINE ACTUATED_RED_PROCESSING(IACT, RING, PHASE, MAX_OR_GAP_MODE)
!----------------------------------------------------------------------
!--- SCOPE
!--  Method : Actuated_Red_Processing
!--
!--  Purpose
!--
!--  Red processing in actuated modes
!--
!--
!--  Change History
!--
!--  DATE                 Description
!--
!--  March 04, 2011       Initial Version
! -- August, 2014         Ported to ETFOMM
!----------------------------------------------------------------------
#ifdef DebugVersion
  use debug
#endif  
  USE TEXT
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE GLOBAL_DATA
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, RING, PHASE
  LOGICAL, INTENT(IN) :: MAX_OR_GAP_MODE
!----------------------------------------------------------------------
  CALL PROCESS_COLOR(IACT, RING, RED)
  
  !--  stop any max out or gap out timers
  IF(MAX_OR_GAP_MODE) THEN
    CALL RESET_PASSAGE_TIMER(IACT, PHASE) 
    CALL INITIALIZE_REDUCTION_TIMER(IACT, PHASE) 
    CALL INITIALIZE_GAP_DOWN_TIMER(IACT, PHASE) 
    
#ifdef DebugVersion
if(verbose) then
if(ac_signals(iact)%sdp%passage_timer(phase) .ne. timer_stopped) then
  write(msgtext, '(a, f5.1, a)') 'actuated_red_processing message: ', simtime, ' seconds'
  call sendtextmsg(m_info)
  write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
  call sendtextmsg(m_info)
  write(msgtext, '(a, i1)') 'in red, stopping timers for phase ', phase
  call sendtextmsg(m_info)
endif
endif
#endif

  ENDIF
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE PROCESS_COLOR(IACT, RING, COLOR)
!----------------------------------------------------------------------
!--- SCOPE
!--  Method : Process_Color
!--
!--  Purpose
!--
!--  This sets a ring's phase to a new color. 
!--
!--
!--  Change History
!--
!--  DATE                 Description
!--
!--  Apr 7, 2011          Initial Version
! -- August, 2014         Ported to ETFOMM
!----------------------------------------------------------------------
#ifdef DebugVersion
  use debug
#endif  
  USE TEXT
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE GLOBAL_DATA
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, RING, COLOR
  INTEGER :: PHASE
#ifdef DebugVersion
  logical :: in_color(8, 3)
#endif  
!----------------------------------------------------------------------
  PHASE = AC_SIGNALS(IACT)%CURRENT_PHASES(RING)
  AC_SIGNALS(IACT)%SDP%PREVIOUS_COLORS(RING) = AC_SIGNALS(IACT)%SDP%CURRENT_COLORS(RING)
  AC_SIGNALS(IACT)%SDP%CURRENT_COLORS(RING) = COLOR

#ifdef DebugVersion
if(verbose) then
  if(.not. in_color(phase, color)) then 
    in_color(phase, red) = .false.
    in_color(phase, yellow) = .false.
    in_color(phase, green) = .false.
    in_color(phase, color) = .true.
    write(msgtext, '(a, f5.1, a)') 'process_color message: ', simtime, ' seconds'
    call sendtextmsg(m_info)
    write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
    call sendtextmsg(m_info)
    write(msgtext, '(a, i1)') '  phase is -> ', ac_signals(iact)%current_phases(ring)
    call sendtextmsg(m_info)
    write(msgtext, '(a, i1)') '  new color is -> ', Color
    call sendtextmsg(m_info)
  endif
endif
#endif

  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE ACTUATED_YELLOW_PROCESSING(IACT, RING, PHASE)
!----------------------------------------------------------------------
!--- SCOPE
!--  Method : Actuated_Yellow_Processing
!--
!--  Purpose
!--
!--  Yellow processing in actuated modes
!--
!--
!--  Change History
!--
!--  DATE                 Description
!--
!--  March 04, 2011       Initial Version
! -- August, 2014         Ported to ETFOMM
!----------------------------------------------------------------------
#ifdef DebugVersion
  use debug
#endif  
  USE TEXT
  USE ACTUATED_CONTROLLERS
  USE GLOBAL_DATA
  USE SCOPE_PARAMETERS
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, RING, PHASE
  INTEGER :: IMODE, IP
  LOGICAL :: PAIRED, CONFLICTING_PHASE
  LOGICAL :: XEQ
!----------------------------------------------------------------------
  !If the phase is a slave to another phase clear the master/slave arrays.
  IF(AC_SIGNALS(IACT)%SDP%MASTER_PHASE(PHASE) .NE. 0) THEN
    AC_SIGNALS(IACT)%SDP%SLAVE_PHASE(AC_SIGNALS(IACT)%SDP%MASTER_PHASE(PHASE)) = 0
    AC_SIGNALS(IACT)%SDP%MASTER_PHASE(PHASE) = 0
    AC_SIGNALS(IACT)%SDP%TIME_LIMIT(PHASE) = AC_SIGNALS(IACT)%SDP%PHASE_TIMER(PHASE) &
      + AC_SIGNALS(IACT)%SDP%YC(PHASE) + AC_SIGNALS(IACT)%SDP%RC(PHASE)
  ENDIF
  
  !--  indicate that this phases green has been done at least once
  IF(.NOT. AC_SIGNALS(IACT)%SDP%GREEN_DONE_ONCE(PHASE)) THEN

    AC_SIGNALS(IACT)%SDP%GREEN_DONE_ONCE(PHASE) = .TRUE.
    AC_SIGNALS(IACT)%SDP%GREEN_DONE_FRAME(PHASE) = .TRUE.

#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'actuated_yellow_processing message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i1)') '  green done frame for phase ', phase
call sendtextmsg(m_info)
endif
#endif

  ENDIF !--  green done once

  !--  gap out and max out are handled in green processing

  !--  depending on the control mode, when green has completed
  !--  the processing is very different.  If the mode is 
  !--  recall min or recall max and nothing is on the call queue
  !--  then continue running the phases.  Otherwise, requeue
  !--  the phase (if its not already queued) and go into yellow. 

  !--  check mode min or max recall
  IMODE = AC_SIGNALS(IACT)%SDP%ACTUATED_MODE(PHASE)
  IF(IMODE .EQ. RECALL_MIN .OR. IMODE .EQ. RECALL_MAX) THEN

    !--  if something is on the call queue and its not the current
    !--  phase then requeue the current phase                       
    IF(AC_SIGNALS(IACT)%QUEUED_PHASES .GT. 0) THEN

      !--  something is waiting to run, requeue the phase if it conflicts
      !--  with phases waiting to run and it is not already queued. 
      !Look at all phases that are queued and see if there are any conflicts.
      CONFLICTING_PHASE = .FALSE.
      DO IP = 1, 8
        IF(AC_SIGNALS(IACT)%SDP%QUEUED(IP)) THEN
          IF(CONFLICTS(PHASE, IP)) THEN
            CONFLICTING_PHASE = .TRUE.
            EXIT
          ENDIF
        ENDIF
      ENDDO
      IF(CONFLICTING_PHASE) THEN

        !--  there is a conflict, requeue if its not on the queue
        IF(.NOT. AC_SIGNALS(IACT)%SDP%QUEUED(PHASE)) THEN

#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'actuated_yellow_processing message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i1)') 'in yellow, queue not empty, conflict, recall min or max re-queuing phase ', phase
call sendtextmsg(m_info)
endif
#endif

          !--  requeue
          CALL QUEUE_PHASE(IACT, PHASE)
          AC_SIGNALS(IACT)%QUEUED_PHASES = AC_SIGNALS(IACT)%QUEUED_PHASES + 1

        ENDIF  !-- phase not queued.

        IF(AC_SIGNALS(IACT)%SDP%PEDESTRIAN_ACTIVE(PHASE)) THEN
          AC_SIGNALS(IACT)%SDP%PEDESTRIAN_ACTIVE(PHASE) = .FALSE.
        ENDIF

        !--  change light yellow
        CALL PROCESS_COLOR(IACT, RING, YELLOW)

      ELSE !-- no conflict

        !--  if we're in concurrent recall mode and the phase's paired phase
        !--  is in yellow then turn this phase to yellow even though there is
        !--  no conflict.
        PAIRED = PHASE_PAIRS(AC_SIGNALS(IACT)%CURRENT_PHASES(1), AC_SIGNALS(IACT)%CURRENT_PHASES(2))
        IF(AC_SIGNALS(IACT)%SDP%CONCURRENT_RECALL .AND. PAIRED) THEN

          IF(.NOT. AC_SIGNALS(IACT)%SDP%QUEUED(PHASE)) THEN

#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'actuated_yellow_processing message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i1)') 'in yellow, queue not empty, conflict, recall min or max concurrent recall true ', phase
call sendtextmsg(m_info)
endif
#endif

            !--  requeue
            CALL QUEUE_PHASE(IACT, PHASE)
            AC_SIGNALS(IACT)%QUEUED_PHASES = AC_SIGNALS(IACT)%QUEUED_PHASES + 1

          ENDIF  !-- phase not queued.

          IF(AC_SIGNALS(IACT)%SDP%PEDESTRIAN_ACTIVE(PHASE)) AC_SIGNALS(IACT)%SDP%PEDESTRIAN_ACTIVE(PHASE) = .FALSE.

          !--  change light yellow
          CALL PROCESS_COLOR(IACT, RING, YELLOW)

        ELSE 

          !-- keep this phase running
          !Unless it is a coordinated phase
          IF(XEQ(AC_SIGNALS(IACT)%CYCLE_LENGTH, 0.) .OR. (PHASE .NE. 2 .AND. PHASE .NE. 6)) THEN
            CALL INCREMENT_PHASE_TIMER(IACT, PHASE)
          ENDIF

        ENDIF  !--  concurrent recall.

      ENDIF !-- conflicts

    ELSE !--  nothing on the queue
        
      !--  nothing is ready to run, keep this phase running
      CALL INCREMENT_PHASE_TIMER(IACT, PHASE)

    ENDIF !--  call queue empty
         
  ELSE

    IF(AC_SIGNALS(IACT)%SDP%PEDESTRIAN_ACTIVE(PHASE)) THEN
      AC_SIGNALS(IACT)%SDP%PEDESTRIAN_ACTIVE(PHASE) = .FALSE.
    ENDIF

    !--  process yellow with no requeue
    CALL PROCESS_COLOR(IACT, RING, YELLOW)

  ENDIF  !-- mode recall min or max
  RETURN
  END

! ==================================================================================================
  SUBROUTINE PEDESTRIAN_ADJUST(IACT, RING, PHASE)
!----------------------------------------------------------------------
!--- SCOPE
!--  Method : 
!--
!--  Purpose
!--
!--  
!--
!--
!--  Change History
!--
!--  DATE                 Description
!--
!--  Feb 23, 2011         Initial Version
! -- August, 2014         Ported to ETFOMM
!----------------------------------------------------------------------
#ifdef DebugVersion
  use debug
#endif  
  USE TEXT
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, RING, PHASE
  INTEGER :: THE_OTHER_PHASE
  REAL :: TOTAL_PED_TIME
!----------------------------------------------------------------------
  THE_OTHER_PHASE = AC_SIGNALS(IACT)%CURRENT_PHASES(THE_OTHER_RING(RING))
  TOTAL_PED_TIME = AC_SIGNALS(IACT)%SDP%PED_CLEARANCE_TIME(PHASE) + AC_SIGNALS(IACT)%SDP%PED_WALK_TIME(PHASE)
  IF(AC_SIGNALS(IACT)%PEDESTRIAN_DETECTOR_STATE(PHASE) .EQ. 1) THEN
    
    !--  adjust green time based on pedestrian call

    !--  if  green time .LT. Pedestrian Walk Time + Pedestrian Clearance Time)
    !--  green time = Pedestrian Walk Time + Pedestrian Clearance Time

    !--  first, if Ped Omit is active, ignore the adjustment
    IF(.NOT. AC_SIGNALS(IACT)%SDP%PED_OMIT(PHASE)) THEN

      !--  Pedestrian is active, change green time for this phase
      IF(AC_SIGNALS(IACT)%SDP%GREEN_TIME(PHASE) .LT. TOTAL_PED_TIME .AND. .NOT. AC_SIGNALS(IACT)%SDP%PEDESTRIAN_ACTIVE(PHASE)) THEN
                        
        AC_SIGNALS(IACT)%SDP%GREEN_TIME(PHASE) = TOTAL_PED_TIME
        AC_SIGNALS(IACT)%SDP%PEDESTRIAN_ACTIVE(PHASE) = .TRUE.
        AC_SIGNALS(IACT)%PEDESTRIAN_DETECTOR_STATE = 0

#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'pedestrian_adjust message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i5, a, i5)') ' pedestrian adjusting green to ', total_ped_time, ' for phase ', phase
call sendtextmsg(m_info)
endif
#endif

      ENDIF !--  Green Time .LT. Total Ped Time
    ENDIF !--  Ped Omit, Phase .NE. 0 
  ENDIF !--  has call

  !--  if the phase's pedestrian is active, then check the other phase
  IF(THE_OTHER_PHASE .NE. 0) THEN
    IF(AC_SIGNALS(IACT)%SDP%PEDESTRIAN_ACTIVE(PHASE) .AND. PHASE_PAIRS(PHASE, THE_OTHER_PHASE) .AND. .NOT. AC_SIGNALS(IACT)%SDP%PEDESTRIAN_ACTIVE(THE_OTHER_PHASE)) THEN

#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'pedestrian_adjust message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i5, a, i5)') ' pedestrian adjusting green to ', total_ped_time, ' for phase ', the_other_phase
call sendtextmsg(m_info)
endif
#endif

      AC_SIGNALS(IACT)%SDP%PEDESTRIAN_ACTIVE(THE_OTHER_PHASE) = .TRUE.         
      AC_SIGNALS(IACT)%SDP%GREEN_TIME(THE_OTHER_PHASE) = TOTAL_PED_TIME      

    ENDIF !--  phase's pedestrian is active
  ENDIF
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE ACTUATED_GREEN_PROCESSING(IACT, RING, PHASE, MAX_OR_GAP_MODE, GAP_OUT_MODE)
!----------------------------------------------------------------------
!--- SCOPE
!--  Method : Actuated_Green_Processing
!--
!--  Purpose
!--
!--  Green processing in actuated modes
!--
!--
!--  Change History
!--
!--  DATE                 Description
!--
!--  March 04, 2011       Initial Version
! -- August, 2014         Ported to ETFOMM
!----------------------------------------------------------------------
#ifdef DebugVersion
  use debug
#endif  
  USE TEXT
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE GLOBAL_DATA
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, RING, PHASE
  LOGICAL, INTENT(INOUT) :: MAX_OR_GAP_MODE, GAP_OUT_MODE
  INTEGER :: IPHASE
  LOGICAL :: NEXT_CONFLICT
  LOGICAL :: XEQ
!----------------------------------------------------------------------
  !--  indicate that this phases green has been done at least once
  IF(.NOT. AC_SIGNALS(IACT)%SDP%GREEN_DONE_ONCE(PHASE)) THEN
    AC_SIGNALS(IACT)%SDP%GREEN_DONE_ONCE(PHASE) = AC_SIGNALS(IACT)%SDP%PHASE_TIMER(PHASE) .GE. AC_SIGNALS(IACT)%SDP%GREEN_TIME(PHASE)
    IF(AC_SIGNALS(IACT)%SDP%GREEN_DONE_ONCE(PHASE)) THEN
      AC_SIGNALS(IACT)%SDP%GREEN_DONE_FRAME(PHASE) = .TRUE.

#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'actuated_green_processing message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i5)') ' green done frame for phase ', phase
call sendtextmsg(m_info)
endif
#endif
 
    ENDIF !--  green done once, set frame
  ENDIF !--  green done once

  CALL PROCESS_COLOR(IACT, RING, GREEN)

  !--  if the mode is max out or gap out, 
  IF(MAX_OR_GAP_MODE) THEN
     
    !--  start a passage timer for the phase
    IF(XEQ(AC_SIGNALS(IACT)%SDP%PASSAGE_TIMER(PHASE), TIMER_STOPPED)) THEN

#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'actuated_green_processing message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i5)') ' starting passage timer at start of green ', phase
call sendtextmsg(m_info)
endif
#endif

      CALL RESET_PASSAGE_TIMER(IACT, PHASE)

    ELSE 

      !--  check if passage timer > min time 
      IF(.NOT. AC_SIGNALS(IACT)%SDP%GAPOUT(PHASE)) THEN
        !IF(.NOT. AC_SIGNALS(IACT)%EXTEND_GREEN(PHASE)) THEN
          CALL GAP_CHECK(IACT, RING, PHASE)
        !ENDIF
      ENDIF

    ENDIF !--  passage timer = timer stopped
  ENDIF !--  max or gap out mode

  !--  see if something is waiting to run
  IF(AC_SIGNALS(IACT)%QUEUED_PHASES .GT. 0) THEN
 
    !See if any queued phase conflicts with the current phase.
    NEXT_CONFLICT = .FALSE.
    DO IPHASE = 1, 8
      IF(AC_SIGNALS(IACT)%SDP%QUEUED(IPHASE)) THEN
        NEXT_CONFLICT = CONFLICTS(IPHASE, PHASE)
        IF(NEXT_CONFLICT) EXIT
      ENDIF
    ENDDO
      
    !--  phase has been in green at least its min/max time
    !--  and something on the queue OR Phase is in Max Out/
    !--  Gap Out mode

    !--  see if a conflict exists
    IF(NEXT_CONFLICT) THEN

      !--  yes start the reduction timer if it hasn't been started and
      !--  we are in gap out mode
      IF(GAP_OUT_MODE .AND. XEQ(AC_SIGNALS(IACT)%SDP%REDUCTION_TIMER(PHASE), TIMER_STOPPED)) THEN

        CALL RESET_REDUCTION_TIMER(IACT, PHASE)

#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'actuated_green_processing message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i5, a)') ' phase ', phase, ' reduction timer started'
call sendtextmsg(m_info)
endif
#endif
 
      ENDIF

      !--   the green has been done one time 
      IF(AC_SIGNALS(IACT)%SDP%GREEN_DONE_ONCE(PHASE)) THEN         

        !--  see if the controller mode is in max or gap out  
        IF(MAX_OR_GAP_MODE) THEN

          !--  Gap Out Mode only processing
          IF(GAP_OUT_MODE) THEN

            !--  if the time before reduction is reached, start the gap down timer
            IF(.NOT. XEQ(AC_SIGNALS(IACT)%SDP%REDUCTION_TIMER(PHASE), TIMER_STOPPED) .AND. AC_SIGNALS(IACT)%SDP%REDUCTION_TIMER(PHASE) .NE. AC_SIGNALS(IACT)%SDP%TIME_BEFORE_REDUCTION(PHASE)) THEN

              !--  start the gap down timer if it hasn't been started
              IF(XEQ(AC_SIGNALS(IACT)%SDP%GAP_DOWN_TIMER(PHASE), TIMER_STOPPED)) THEN

                CALL RESET_GAP_DOWN_TIMER(IACT, PHASE)

#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'actuated_green_processing message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i5, a)') ' phase ', phase, ' gap down timer started'
call sendtextmsg(m_info)
endif
#endif

              ENDIF

              !--  compute the gap time by this equation:
              !--  gap time = gap time - (gap_time - min gap time)/time to reduce) * gap down time
              !--  until the computed gap time = minimum gap time.
              IF(AC_SIGNALS(IACT)%SDP%GAP_DOWN_TIMER(PHASE) .GT. 0.0) THEN
                   
                IF(AC_SIGNALS(IACT)%SDP%COMPUTED_GAP_TIME(PHASE) .GE. AC_SIGNALS(IACT)%SDP%MIN_GAP_TIMES(PHASE)) THEN

                  AC_SIGNALS(IACT)%SDP%COMPUTED_GAP_TIME(PHASE) = AC_SIGNALS(IACT)%SDP%GAP_TIMES(PHASE) - (AC_SIGNALS(IACT)%SDP%GAP_TIMES(PHASE) -  AC_SIGNALS(IACT)%SDP%MIN_GAP_TIMES(PHASE)) &
                         / AC_SIGNALS(IACT)%SDP%TIME_TO_REDUCE(PHASE) * AC_SIGNALS(IACT)%SDP%GAP_DOWN_TIMER(PHASE)
                      
#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'actuated_green_processing message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i5, a, i5)') ' phase ', phase, ' gap down timer =>', ac_signals(iact)%sdp%gap_down_timer(phase)
call sendtextmsg(m_info)
write(msgtext, '(a, i5, a, i5)') ' phase ', phase, ' computed gap time =>', ac_signals(iact)%sdp%computed_gap_time(phase)
call sendtextmsg(m_info)
endif
#endif

                ELSE!--  computed gap time .LT. Min Gap Time 

#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'actuated_green_processing message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i5, a, i5)') ' phase ', phase, ' computed gap time .lt. or =  min gap time, reduction stopped'
call sendtextmsg(m_info)
endif
#endif
                  
                ENDIF!--  computed gap time .LT. min gap time  

              ENDIF!--  gap down timer > 0

            ENDIF!-- reduction timer running

          ENDIF!--  gap out mode
        
          !--  for max out mode gap out conditions

          !--  1st, the green period has just expired
          !--  and the gap time is exceeded
          IF(AC_SIGNALS(IACT)%SDP%GREEN_DONE_FRAME(PHASE)) THEN

            !--  trigger for exact frame green done
            AC_SIGNALS(IACT)%SDP%GREEN_DONE_FRAME(PHASE) = .FALSE.

#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'actuated_green_processing message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i5, a, i5)') ' phase ', phase, ' green done frame gap out check'
call sendtextmsg(m_info)
endif
#endif

            IF(AC_SIGNALS(IACT)%SDP%GAPOUT(PHASE)) THEN
      
              CALL PREEMPT(IACT, RING, PHASE)

            ELSE     
              !--  set time to check extension
              AC_SIGNALS(IACT)%SDP%TIME_PLUS_EXTENSION(PHASE) = AC_SIGNALS(IACT)%SDP%PHASE_TIMER(PHASE) + AC_SIGNALS(IACT)%SDP%EXTENSION_TIME(PHASE)

            ENDIF

          !--  second, the maximum green time has passed
          ELSEIF(AC_SIGNALS(IACT)%SDP%PHASE_TIMER(PHASE) .GE. AC_SIGNALS(IACT)%SDP%MAXIMUM_GREEN_TIME(PHASE)) THEN
 
            !--  or max green reached
            CALL PREEMPT(IACT, RING, PHASE)

#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'actuated_green_processing message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i5, a, i5)') ' phase ', phase, ' preempting because max green reached time'
call sendtextmsg(m_info)
endif
#endif

          !--  last, check for the extension time is reached 
          ELSE

            !--  if the end of the extension time
            !--  has been reached, check for gap out.
            !--  then add more time and reset passage
            CALL EXTENSION_TIME_PROCESSING(IACT, RING, PHASE)

          ENDIF !--  green just done once frame
                  
        ELSE !--  running presence or min recall or max recall,  preempt

#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'actuated_green_processing message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i5, a, i5)') ' min or max recall mode, phase ', phase, ' is being preempted'
call sendtextmsg(m_info)
endif
#endif

          CALL PREEMPT(IACT, RING, PHASE)

        ENDIF !--  actuated mode max or gap out
      ENDIF !--  green done once
    ENDIF !-- conflicts    
  ENDIF !--  call queue empty
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE EXTENSION_TIME_PROCESSING(IACT, RING, PHASE)
!----------------------------------------------------------------------
!--- SCOPE
!--  Method : Extension_Time_Processing
!--
!--  Purpose
!--
!--  Extend Green Times
!--
!--
!--  Change History
!--
!--  DATE                 Description
!--
!--  Apr 20, 2011         Initial Version
! -- August, 2014         Ported to ETFOMM
!----------------------------------------------------------------------
#ifdef DebugVersion
  use debug
#endif  
  USE TEXT
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, RING, PHASE
!----------------------------------------------------------------------
  !--  see if extension time reached
  IF(AC_SIGNALS(IACT)%SDP%PHASE_TIMER(PHASE) .GE. AC_SIGNALS(IACT)%SDP%TIME_PLUS_EXTENSION(PHASE)) THEN

#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'extension_time_processing message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i5, a, f5.1)') ' phase ', phase, ' extension time reached, extension timer =>', ac_signals(iact)%sdp%time_plus_extension(phase)
call sendtextmsg(m_info)
endif
#endif

    !--  if min gap is reached then preempt
    IF(AC_SIGNALS(IACT)%SDP%GAPOUT(PHASE)) THEN
      CALL PREEMPT(IACT, RING, PHASE)
    ELSE

      !--  increase the green by the extension time
      AC_SIGNALS(IACT)%SDP%GREEN_TIME(PHASE) = AC_SIGNALS(IACT)%SDP%GREEN_TIME(PHASE) + AC_SIGNALS(IACT)%SDP%EXTENSION_TIME(PHASE)

#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'extension_time_processing message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i5, a, f5.1)') ' phase ', phase, ' extended green time to =>', ac_signals(iact)%sdp%green_time(phase)
call sendtextmsg(m_info)
endif
#endif

      !--  set the next extension time 
      AC_SIGNALS(IACT)%SDP%TIME_PLUS_EXTENSION(PHASE) = AC_SIGNALS(IACT)%SDP%PHASE_TIMER(PHASE) + AC_SIGNALS(IACT)%SDP%EXTENSION_TIME(PHASE)

#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'extension_time_processing message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i5, a, f5.1)') ' phase ', phase, ' next extension time =>', ac_signals(iact)%sdp%time_plus_extension(phase)
call sendtextmsg(m_info)
endif
#endif

    ENDIF !-- no preempted
  ENDIF !--  extension time reached
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE PREEMPT(IACT, RING, PHASE)
!----------------------------------------------------------------------
!--- SCOPE
!--  Method : Preempt
!--
!--  Purpose
!--
!--  Preempt a Phase
!--
!--  Change History
!--
!--  DATE                 Description
!--
!--  Mar 27, 2011         Initial Version
! -- August, 2014         Ported to ETFOMM
!----------------------------------------------------------------------
#ifdef DebugVersion
  use debug
#endif  
  USE TEXT
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, RING, PHASE
!----------------------------------------------------------------------
  IF(.NOT. AC_SIGNALS(IACT)%LOCKED(RING)) THEN
    !--  modify the phase timer so the yellow phase starts
    AC_SIGNALS(IACT)%SDP%PHASE_TIMER(PHASE) = AC_SIGNALS(IACT)%SDP%TIME_LIMIT(PHASE) -  AC_SIGNALS(IACT)%SDP%RC(PHASE) - AC_SIGNALS(IACT)%SDP%YC(PHASE)

    !--  if we were waiting for a gap or max out, 
    !--  it has arrived. 
    AC_SIGNALS(IACT)%SDP%WAIT_FOR_GAP(PHASE) = .FALSE.

#ifdef DebugVersion
  if(verbose) then
  write(msgtext, '(a, f5.1, a)') 'preempt message: ', simtime, ' seconds'
  call sendtextmsg(m_info)
  write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
  call sendtextmsg(m_info)
  write(msgtext, '(a, i5)') ' preempting ', phase
  call sendtextmsg(m_info)
  endif
#endif
  
  ENDIF

  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE GAP_CHECK(IACT, RING, PHASE)
!----------------------------------------------------------------------
!--- SCOPE
!--  Method : Gap Check
!--
!--  Purpose
!--
!--    Check Phase Timer > Gap Time.  If so, set indicator
!--
!--
!--  Change History
!--
!--  DATE                 Description
!--
!--  Apr 22, 2011         Initial Version
! -- August, 2014         Ported to ETFOMM
!----------------------------------------------------------------------
#ifdef DebugVersion
  use debug
#endif  
  USE TEXT
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE NTCIP_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, RING, PHASE
  INTEGER :: OTHER_PHASE
  !LOGICAL :: OK_TO_TERMINATE
!----------------------------------------------------------------------
  !--  if min gap is reached then set indicator
  IF(AC_SIGNALS(IACT)%SDP%PASSAGE_TIMER(PHASE) .GE. AC_SIGNALS(IACT)%SDP%COMPUTED_GAP_TIME(PHASE)) THEN
    !AC_SIGNALS(IACT)%READY_TO_TERMINATE(PHASE) = .TRUE.
    !!Check simultaneous gap setting to determine if this phase can gapout at this time
    !IF(AC_SIGNALS(IACT)%SIMULTANEOUS_GAP_CODE(PHASE) .EQ. 0) THEN
    !  OK_TO_TERMINATE = .TRUE.
    !ELSE
    !  OK_TO_TERMINATE = .FALSE.
    !  OTHER_PHASE = AC_SIGNALS(IACT)%CURRENT_PHASES(THE_OTHER_RING(RING))
    !  IF(OTHER_PHASE .EQ. 0) THEN
    !    OK_TO_TERMINATE = .TRUE.
    !  ELSE
    !    IF(AC_SIGNALS(IACT)%READY_TO_TERMINATE(OTHER_PHASE)) THEN
    !      OK_TO_TERMINATE = .TRUE.
    !    ENDIF
    !  ENDIF
    !ENDIF
    !Do not apply gapout until min green has been reached.
    !IF(OK_TO_TERMINATE .AND. AC_SIGNALS(IACT)%SDP%PHASE_TIMER(PHASE) .GE. AC_SIGNALS(IACT)%SDP%MINIMUM_GREEN_TIME(PHASE)) THEN
    IF(AC_SIGNALS(IACT)%SDP%PHASE_TIMER(PHASE) .GE. AC_SIGNALS(IACT)%SDP%MINIMUM_GREEN_TIME(PHASE)) THEN
      AC_SIGNALS(IACT)%SDP%GAPOUT(PHASE) = .TRUE.
      AC_SIGNALS(IACT)%MAXOUTS(PHASE) = AC_SIGNALS(IACT)%MAXOUTS(PHASE) + 1
      IF(PHASE .EQ. 2) THEN
        SBB(1) = 1
      ELSEIF(PHASE .EQ. 6) THEN
        SBB(2) = 1
      ENDIF
    ENDIF
    
#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'gap_check message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i5, a)') ' phase ', phase, ' passage timer > computed gap time, set indicator'
call sendtextmsg(m_info)
endif
#endif

  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE QUEUE_PHASE(IACT, PHASE)
!-------------------------------------------------------------------------
! Set the phase queued flag.
! Setting it in a single location allows easier debugging.  
!-------------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, PHASE
!-------------------------------------------------------------------------
  AC_SIGNALS(IACT)%SDP%QUEUED(PHASE) = .TRUE.
  RETURN
  END

! ==================================================================================================
  SUBROUTINE DEQUEUE_PHASE(IACT, PHASE)
!-------------------------------------------------------------------------
! Set the phase queued flag.
! Setting it in a single location allows easier debugging.  
!-------------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, PHASE
!-------------------------------------------------------------------------
  AC_SIGNALS(IACT)%SDP%QUEUED(PHASE) = .FALSE.
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE CHECK_SINGLE_PHASE_RESTART(IACT, ENDING_PHASE, ACTIVE_RING, ACTIVE_PHASE, START_FLAG, NEW_PHASE)
!-------------------------------------------------------------------------
! Select a phase to activate when there is only one other phase active.
!-------------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE NTCIP_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, ENDING_PHASE, ACTIVE_RING, ACTIVE_PHASE
  LOGICAL, INTENT(IN) :: START_FLAG
  INTEGER, INTENT(OUT) :: NEW_PHASE
  INTEGER :: I, IP, PHASE1, PHASE2, INDEX, THIS_RING
  REAL :: REMAINING_GREEN_TIME
  LOGICAL :: RESTART, ALL_SERVED, GAPOUT1, GAPOUT2, QUEUED1, QUEUED2, HAS_DEMAND
!-------------------------------------------------------------------------
  !Determine if all phases in the barrier have been served.
  ALL_SERVED = .TRUE.
  INDEX = MAXVAL(AC_SIGNALS(IACT)%RING_INDEX)
  IF(INDEX .LT. 3) THEN
    INDEX = 1
  ELSE
    INDEX = 3
  ENDIF
  DO I = INDEX, INDEX + 1
    IP = AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(1, I)
    IF(IP .NE. 0) THEN
      IF(.NOT. AC_SIGNALS(IACT)%SERVED_THIS_CYCLE(IP)) THEN
        ALL_SERVED = .FALSE.
        EXIT
      ENDIF
    ENDIF
    IP = AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(2, I)
    IF(IP .NE. 0) THEN
      IF(.NOT. AC_SIGNALS(IACT)%SERVED_THIS_CYCLE(IP)) THEN
        ALL_SERVED = .FALSE.
        EXIT
      ENDIF
    ENDIF
  ENDDO
  IF(ALL_SERVED) THEN
    AC_SIGNALS(IACT)%READY_TO_CROSS = .TRUE.
  ENDIF
  GAPOUT1 = .FALSE.
  GAPOUT2 = .FALSE.
  IF(AC_SIGNALS(IACT)%SDP%WAIT_FOR_GAP(ACTIVE_PHASE) .AND. .NOT. AC_SIGNALS(IACT)%SDP%GAPOUT(ACTIVE_PHASE)) THEN
    !If the phase is being extended and has not gapped out calculate the maximum time remaining.
    REMAINING_GREEN_TIME = AC_SIGNALS(IACT)%SDP%MAXIMUM_GREEN_TIME(ACTIVE_PHASE) - AC_SIGNALS(IACT)%SDP%PHASE_TIMER(ACTIVE_PHASE)
  ELSEIF(AC_SIGNALS(IACT)%SDP%WAIT_FOR_GAP(ACTIVE_PHASE)) THEN
    !If the phase is being extended but has gapped out calculate the time remaining in the extension.
    !The other phase will continue green until there is conflicting demand, or until reaching maximum green.
    IF(AC_SIGNALS(IACT)%SDP%TIME_PLUS_EXTENSION(ACTIVE_PHASE) .NE. 0) THEN
      !There is conflicting demand.
      REMAINING_GREEN_TIME = AC_SIGNALS(IACT)%SDP%TIME_PLUS_EXTENSION(ACTIVE_PHASE) - AC_SIGNALS(IACT)%SDP%PHASE_TIMER(ACTIVE_PHASE)
    ELSE
      !There is no conflicting demand.
      REMAINING_GREEN_TIME = AC_SIGNALS(IACT)%SDP%MAXIMUM_GREEN_TIME(ACTIVE_PHASE) - AC_SIGNALS(IACT)%SDP%PHASE_TIMER(ACTIVE_PHASE)
    ENDIF
    GAPOUT1 = .TRUE.
    IF(ENDING_PHASE .NE. 0) GAPOUT2 = AC_SIGNALS(IACT)%SDP%GAPOUT(ENDING_PHASE)
  ELSE
    IF(USE_DCS .AND. .NOT. INITMODE) THEN
      IF(BTEST(EXTENSION_GROUP, ACTIVE_PHASE-1)) THEN
        REMAINING_GREEN_TIME = AC_SIGNALS(IACT)%SDP%MAXIMUM_GREEN_TIME(ACTIVE_PHASE) - AC_SIGNALS(IACT)%SDP%PHASE_TIMER(ACTIVE_PHASE)
      ELSEIF(BTEST(FORCEOFF_GROUP, ACTIVE_PHASE-1)) THEN
        REMAINING_GREEN_TIME = 0.0
      ELSE
        !Calculate the time remaining in the phase.
        REMAINING_GREEN_TIME = AC_SIGNALS(IACT)%SDP%TIME_LIMIT(ACTIVE_PHASE) - AC_SIGNALS(IACT)%SDP%PHASE_TIMER(ACTIVE_PHASE)
      ENDIF
    ELSE   
      !Calculate the time remaining in the phase.
      REMAINING_GREEN_TIME = AC_SIGNALS(IACT)%SDP%TIME_LIMIT(ACTIVE_PHASE) - AC_SIGNALS(IACT)%SDP%PHASE_TIMER(ACTIVE_PHASE)
    ENDIF
  ENDIF
  REMAINING_GREEN_TIME = REMAINING_GREEN_TIME - AC_SIGNALS(IACT)%GUI_YC(ACTIVE_PHASE) - AC_SIGNALS(IACT)%GUI_RC(ACTIVE_PHASE)
  NEW_PHASE = 0
  HAS_DEMAND = .FALSE.
  IF(ENDING_PHASE .NE. 0) THEN
    HAS_DEMAND = AC_SIGNALS(IACT)%HAS_DEMAND(ENDING_PHASE)
  ENDIF
  IF(GAPOUT1 .AND. GAPOUT2) THEN
    !Both phases are gapping out. Let them expire at the same time.
    IF(REMAINING_GREEN_TIME .GE. 1.0) NEW_PHASE = ENDING_PHASE
  ELSE
    !If there is demand on a non-conflicting phase start it.
    PHASE1 = PAIRED_PHASE(ACTIVE_PHASE)
    IF(.NOT. AC_SIGNALS(IACT)%PHASE(PHASE1)%IN_USE) THEN
      PHASE1 = 0
    ELSEIF(AC_SIGNALS(IACT)%SDP%MASTER_PHASE(PHASE1) .NE. 0) THEN
      PHASE1 = 0
    ELSEIF(AC_SIGNALS(IACT)%SDP%SLAVE_PHASE(PHASE1) .NE. 0) THEN
      PHASE1 = 0
    ELSEIF(.NOT. PHASE_IS_ALLOWED(IACT, PHASE1)) THEN
      PHASE1 = 0
    ENDIF
    PHASE2 = COMPLEMENTARY_PHASE(ACTIVE_PHASE)
    IF(.NOT. AC_SIGNALS(IACT)%PHASE(PHASE2)%IN_USE) THEN
      PHASE2 = 0
    ELSEIF(AC_SIGNALS(IACT)%SDP%MASTER_PHASE(PHASE2) .NE. 0) THEN
      PHASE2 = 0
    ELSEIF(AC_SIGNALS(IACT)%SDP%SLAVE_PHASE(PHASE2) .NE. 0) THEN
      PHASE2 = 0
    ELSEIF(.NOT. PHASE_IS_ALLOWED(IACT, PHASE2)) THEN
      PHASE2 = 0
    ENDIF
    QUEUED1 = .FALSE.
    IF(PHASE1 .NE. 0) THEN
      QUEUED1 = AC_SIGNALS(IACT)%SDP%QUEUED(PHASE1)
      IF(PHASE1 .EQ. ENDING_PHASE) QUEUED1 = QUEUED1 .OR. AC_SIGNALS(IACT)%HAS_DEMAND(PHASE1)
      IF(AC_SIGNALS(IACT)%CYCLE_LENGTH .NE. 0 .AND. PHASE1 .EQ. 2) QUEUED1 = .TRUE.
    ENDIF
    QUEUED2 = .FALSE.
    IF(PHASE2 .NE. 0) THEN
      QUEUED2 = AC_SIGNALS(IACT)%SDP%QUEUED(PHASE2)
      IF(PHASE2 .EQ. ENDING_PHASE) QUEUED2 = QUEUED2 .OR. AC_SIGNALS(IACT)%HAS_DEMAND(PHASE2)
      IF(AC_SIGNALS(IACT)%CYCLE_LENGTH .NE. 0 .AND. PHASE2 .EQ. 6) QUEUED2 = .TRUE.
    ENDIF
    IF(QUEUED1 .AND. .NOT. QUEUED2) THEN
      NEW_PHASE = PHASE1
    ELSEIF(QUEUED2 .AND. .NOT. QUEUED1) THEN
      NEW_PHASE = PHASE2
    ELSEIF(QUEUED1 .AND. QUEUED2) THEN
      NEW_PHASE = PHASE1
    ELSE
      !If there is no demand on an allowable phase continue the current phase.
      IF(ENDING_PHASE .NE. 0) THEN
        NEW_PHASE = ENDING_PHASE
      ELSE
        !If there is no current phase start a compatible phase.
        NEW_PHASE = PHASE1
      ENDIF
    ENDIF
    !If the new phase is not the same as the ending phase
    !do not start it if the remaining time of the other phase is too small.
    !If the new phase is the ending phase let it continue for the remaining time.
    IF(REMAINING_GREEN_TIME .LT. 3.0 .AND. AC_SIGNALS(IACT)%READY_TO_CROSS(1) .AND. AC_SIGNALS(IACT)%READY_TO_CROSS(2)) THEN
      NEW_PHASE = 0
    ELSEIF(REMAINING_GREEN_TIME .LT. 3.0 .AND. NEW_PHASE .NE. ENDING_PHASE) THEN
      NEW_PHASE = 0
      IF(ENDING_PHASE .NE. 0 .AND. (ENDING_PHASE .EQ. PHASE1 .OR. ENDING_PHASE .EQ. PHASE2)) THEN
        IF(AC_SIGNALS(IACT)%HAS_DEMAND(ENDING_PHASE)) NEW_PHASE = ENDING_PHASE
      ENDIF
    !If the remaining time of the other phase is too small terminate the ending phase.
    ELSEIF(REMAINING_GREEN_TIME .LE. 0.5 * TIMESTEP) THEN
      NEW_PHASE = 0
    ENDIF
  ENDIF
  IF(NEW_PHASE .NE. 0) THEN
    IF(START_FLAG .OR. AC_SIGNALS(IACT)%HAS_DEMAND(NEW_PHASE)) THEN
      RESTART = AC_SIGNALS(IACT)%SERVED_THIS_CYCLE(NEW_PHASE)
      THIS_RING = THE_OTHER_RING(ACTIVE_RING)
      CALL START_ACTUATED_PHASE(IACT, THIS_RING, NEW_PHASE)
      IF(AC_SIGNALS(IACT)%SDP%PHASE_TIMER(ACTIVE_PHASE) .NE. 0.0 .OR. RESTART) THEN
        !Do not allow the new phase to run longer than the current phase.
        !Define the running phase to be the master of the new phase.
        AC_SIGNALS(IACT)%SDP%MASTER_PHASE(NEW_PHASE) = ACTIVE_PHASE
        AC_SIGNALS(IACT)%SDP%SLAVE_PHASE(ACTIVE_PHASE) = NEW_PHASE
        IF(.NOT. AC_SIGNALS(IACT)%READY_TO_CROSS(THIS_RING)) THEN
          AC_SIGNALS(IACT)%READY_TO_CROSS(THIS_RING) = AC_SIGNALS(IACT)%READY_TO_CROSS(ACTIVE_RING)
        ENDIF
      ENDIF
      IF(NEW_PHASE .EQ. AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(THIS_RING, 1)) THEN
        AC_SIGNALS(IACT)%RING_INDEX(THIS_RING) = 1
      ELSEIF(NEW_PHASE .EQ. AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(THIS_RING, 2)) THEN
        AC_SIGNALS(IACT)%RING_INDEX(THIS_RING) = 2
      ELSEIF(NEW_PHASE .EQ. AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(THIS_RING, 3)) THEN
        AC_SIGNALS(IACT)%RING_INDEX(THIS_RING) = 3
      ELSEIF(NEW_PHASE .EQ. AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(THIS_RING, 4)) THEN
        AC_SIGNALS(IACT)%RING_INDEX(THIS_RING) = 4
      ENDIF
      AC_SIGNALS(IACT)%RING_INDEX(THIS_RING) = &
        MAX(AC_SIGNALS(IACT)%RING_INDEX(THIS_RING), AC_SIGNALS(IACT)%RING_INDEX(ACTIVE_RING))
    ENDIF
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE UPDATE_PEDESTRIAN_DETECTORS(IACT)
!-------------------------------------------------------------------------
! Determine the state of pedestrian detectors.
!-------------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT
  INTEGER :: PHASE
!-------------------------------------------------------------------------
  DO PHASE = 1, 8
    IF(AC_SIGNALS(IACT)%GUI_PED_PHASE(PHASE)) THEN
    !Get the type code and time of arrival
    ENDIF
  ENDDO
  !for testing
  !if(simtime .eq. 75) ac_signals(iact)%pedestrian_detector_state(2) = 1
  RETURN
  END

    
! ==================================================================================================
  SUBROUTINE UPDATE_TIMERS(IACT)
!-------------------------------------------------------------------------
! Update actuated control timers.
!-------------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE SCOPE_PARAMETERS
  USE TEXT
#ifdef DebugVersion
  USE DEBUG
  USE SIMPARAMS
#endif
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT
  LOGICAL :: PHASE_ACTIVE(2)
  INTEGER :: RING, CURRENT_PHASE, IMODE
!-------------------------------------------------------------------------
  PHASE_ACTIVE(RING_1) = .FALSE.
  IF(AC_SIGNALS(IACT)%CURRENT_PHASES(RING_1) .NE. 0) PHASE_ACTIVE(RING_1) = AC_SIGNALS(IACT)%SDP%TIME_LIMIT(AC_SIGNALS(IACT)%CURRENT_PHASES(RING_1)) .NE. 0.0
    
  PHASE_ACTIVE(RING_2) = .FALSE.
  IF(AC_SIGNALS(IACT)%CURRENT_PHASES(RING_2) .NE. 0) PHASE_ACTIVE(RING_2) = AC_SIGNALS(IACT)%SDP%TIME_LIMIT(AC_SIGNALS(IACT)%CURRENT_PHASES(RING_2)) .NE. 0.0

  !--  actuated timers and passage timers
  DO RING = RING_1, RING_2
    IF(PHASE_ACTIVE(RING)) THEN
      CURRENT_PHASE = AC_SIGNALS(IACT)%CURRENT_PHASES(RING)
      CALL INCREMENT_PHASE_TIMER(IACT, CURRENT_PHASE)

#ifdef DebugVersion
if(verbose) then
write(msgtext, '(a, f5.1, a)') 'update_actuated_controllers message: ', simtime, ' seconds'
call sendtextmsg(m_info)
write(msgtext, '(a, i4)') ' node ', ac_signals(iact)%node(1)
call sendtextmsg(m_info)
write(msgtext, '(a, i1, a, f5.2)') '  phase timer ', current_phase, ' => ', ac_signals(iact)%sdp%PHASE_TIMER(current_phase)
call sendtextmsg(m_info)
endif
#endif

      IMODE = AC_SIGNALS(IACT)%SDP%ACTUATED_MODE(CURRENT_PHASE)
      IF(IMODE .EQ. MAX_OUT .OR. IMODE .EQ. GAP_OUT) THEN
        IF(AC_SIGNALS(IACT)%SDP%PASSAGE_TIMER(CURRENT_PHASE) .NE. TIMER_STOPPED) THEN
          CALL INCREMENT_PASSAGE_TIMER(IACT, CURRENT_PHASE)
        ENDIF
        IF(AC_SIGNALS(IACT)%SDP%REDUCTION_TIMER(CURRENT_PHASE) .NE. TIMER_STOPPED) THEN
          CALL INCREMENT_REDUCTION_TIMER(IACT, CURRENT_PHASE)
        ENDIF
        IF(AC_SIGNALS(IACT)%SDP%GAP_DOWN_TIMER(CURRENT_PHASE) .NE. TIMER_STOPPED) THEN
          CALL INCREMENT_GAP_DOWN_TIMER(IACT, CURRENT_PHASE)
        ENDIF 
      ENDIF !--  max out or gap out              
    ENDIF !--  ring active
  ENDDO !-- loop
  RETURN
  END

! ==================================================================================================
  SUBROUTINE SET_PHASE_TIMER(IACT, PHASE, TIME)  
!----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, PHASE 
  REAL, INTENT(IN) :: TIME
!----------------------------------------------------------------------
  AC_SIGNALS(IACT)%SDP%PHASE_TIMER(PHASE) = TIME
  AC_SIGNALS(IACT)%SDP%PHASE_COUNTER(PHASE) = AC_SIGNALS(IACT)%SDP%PHASE_TIMER(PHASE) / SCOPE_TIMESTEP
  RETURN
  END

! ==================================================================================================
  SUBROUTINE RESET_PASSAGE_TIMER(IACT, PHASE)  
!----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, PHASE          
!----------------------------------------------------------------------
  AC_SIGNALS(IACT)%SDP%PASSAGE_COUNTER(PHASE) = 0
  AC_SIGNALS(IACT)%SDP%PASSAGE_TIMER(PHASE) = 0.
  RETURN
  END

! ==================================================================================================
  SUBROUTINE RESET_REDUCTION_TIMER(IACT, PHASE)  
!----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, PHASE          
!----------------------------------------------------------------------
  AC_SIGNALS(IACT)%SDP%REDUCTION_COUNTER(PHASE) = 0
  AC_SIGNALS(IACT)%SDP%REDUCTION_TIMER(PHASE) = 0.
  RETURN
  END

! ==================================================================================================
  SUBROUTINE RESET_GAP_DOWN_TIMER(IACT, PHASE)  
!----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, PHASE          
!----------------------------------------------------------------------
  AC_SIGNALS(IACT)%SDP%GAP_DOWN_COUNTER(PHASE) = 0
  AC_SIGNALS(IACT)%SDP%GAP_DOWN_TIMER(PHASE) = 0.
  RETURN
  END

! ==================================================================================================
  SUBROUTINE INITIALIZE_REDUCTION_TIMER(IACT, PHASE)  
!----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, PHASE          
!----------------------------------------------------------------------
  AC_SIGNALS(IACT)%SDP%REDUCTION_COUNTER(PHASE) = -1
  AC_SIGNALS(IACT)%SDP%REDUCTION_TIMER(PHASE) = -1.
  RETURN
  END

! ==================================================================================================
  SUBROUTINE INITIALIZE_GAP_DOWN_TIMER(IACT, PHASE)  
!----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, PHASE          
!----------------------------------------------------------------------
  AC_SIGNALS(IACT)%SDP%GAP_DOWN_COUNTER(PHASE) = -1
  AC_SIGNALS(IACT)%SDP%GAP_DOWN_TIMER(PHASE) = -1.
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE INCREMENT_PHASE_TIMER(IACT, PHASE)  
!----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, PHASE          
!----------------------------------------------------------------------
  AC_SIGNALS(IACT)%SDP%PHASE_COUNTER(PHASE) = AC_SIGNALS(IACT)%SDP%PHASE_COUNTER(PHASE) + 1
  AC_SIGNALS(IACT)%SDP%PHASE_TIMER(PHASE) = AC_SIGNALS(IACT)%SDP%PHASE_COUNTER(PHASE) * SCOPE_TIMESTEP
  RETURN
  END

! ==================================================================================================
  SUBROUTINE INCREMENT_PASSAGE_TIMER(IACT, PHASE)  
!----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, PHASE          
!----------------------------------------------------------------------
  AC_SIGNALS(IACT)%SDP%PASSAGE_COUNTER(PHASE) = AC_SIGNALS(IACT)%SDP%PASSAGE_COUNTER(PHASE) + 1
  AC_SIGNALS(IACT)%SDP%PASSAGE_TIMER(PHASE) = AC_SIGNALS(IACT)%SDP%PASSAGE_COUNTER(PHASE) * SCOPE_TIMESTEP
  RETURN
  END

! ==================================================================================================
  SUBROUTINE INCREMENT_REDUCTION_TIMER(IACT, PHASE)  
!----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, PHASE          
!----------------------------------------------------------------------
  AC_SIGNALS(IACT)%SDP%REDUCTION_COUNTER(PHASE) = AC_SIGNALS(IACT)%SDP%REDUCTION_COUNTER(PHASE) + 1
  AC_SIGNALS(IACT)%SDP%REDUCTION_TIMER(PHASE) = AC_SIGNALS(IACT)%SDP%REDUCTION_COUNTER(PHASE) * SCOPE_TIMESTEP
  RETURN
  END

! ==================================================================================================
  SUBROUTINE INCREMENT_GAP_DOWN_TIMER(IACT, PHASE)  
!----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, PHASE          
!----------------------------------------------------------------------
  AC_SIGNALS(IACT)%SDP%GAP_DOWN_COUNTER(PHASE) = AC_SIGNALS(IACT)%SDP%GAP_DOWN_COUNTER(PHASE) + 1
  AC_SIGNALS(IACT)%SDP%GAP_DOWN_TIMER(PHASE) = AC_SIGNALS(IACT)%SDP%GAP_DOWN_COUNTER(PHASE) * SCOPE_TIMESTEP
  RETURN
  END

