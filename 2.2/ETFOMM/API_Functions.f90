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
  INTEGER FUNCTION ADD_PATH[DLLEXPORT, STDCALL](N, NODES)
! ----------------------------------------------------------------------
!     This function allows an external entity to insert a path into
!     the simulation.
! ----------------------------------------------------------------------
  USE PATH_MOD
  USE FREEWAY_LINKS
  USE STREET_LINKS
  USE NODE_TABLE
  USE TEXT
  INTEGER, INTENT(IN) :: N, NODES(100)
  INTEGER :: IL
  LOGICAL :: FOUND
! ----------------------------------------------------------------------
  IF(.NOT. ALLOCATED(PATH_NODES)) THEN
    CALL ALLOCATE_PATH_ARRAYS
  ENDIF
  NPATHS = NPATHS + 1
  IF(NPATHS .GT. NUMBER_OF_PATHS) THEN
    CALL REALLOCATE_PATH_ARRAYS
  ENDIF
  NNODES(NPATHS) = N + 2
  FOUND = .FALSE.
  DO IL = 1, N_FREEWAY_LINKS
    IF(FDSN(IL) .EQ. NODES(1) .AND. NODE_TYPE(FUSN(IL)) .EQ. NT_EXTERN) THEN
      PATH_NODES(NPATHS, 1) = FUSN(IL)
      FOUND = .TRUE.
      EXIT
    ENDIF
  ENDDO
  IF(.NOT. FOUND) THEN
    DO IL = 1, N_STREET_LINKS
      IF(SDSN(IL) .EQ. NODES(1) .AND. NODE_TYPE(SUSN(IL)) .EQ. NT_EXTERN) THEN
        PATH_NODES(NPATHS, 1) = SUSN(IL)
        FOUND = .TRUE.
        EXIT
      ENDIF
    ENDDO
  ENDIF
  IF(.NOT. FOUND) THEN
    CALL SENDTEXTMSG(M_INFO)
    WRITE(MSGTEXT,'(A,I5)') 'ADD_PATH : ENTRY NODE NOT FOUND : ', NODES(1)
    ADD_PATH = -1
  ELSE
    FOUND = .FALSE.
    DO IL = 1, N_FREEWAY_LINKS
      IF(FUSN(IL) .EQ. NODES(N) .AND. NODE_TYPE(FDSN(IL)) .EQ. NT_EXTERN) THEN
        PATH_NODES(NPATHS, N+2) = FDSN(IL)
        FOUND = .TRUE.
        EXIT
      ENDIF
    ENDDO
    IF(.NOT. FOUND) THEN
      DO IL = 1, N_STREET_LINKS
        IF(SUSN(IL) .EQ. NODES(N) .AND. NODE_TYPE(SDSN(IL)) .EQ. NT_EXTERN) THEN
          PATH_NODES(NPATHS, N+2) = SDSN(IL)
          FOUND = .TRUE.
          EXIT
        ENDIF
      ENDDO
    ENDIF
    IF(.NOT. FOUND) THEN
      CALL SENDTEXTMSG(M_INFO)
      WRITE(MSGTEXT,'(A,I5)') 'ADD_PATH : EXIT NODE NOT FOUND : ', NODES(N)
      ADD_PATH = -1
    ELSE
      PATH_NODES(NPATHS, 2:N+1) = NODES(1:N)
      ADD_PATH = 0
    ENDIF
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION ADD_VEHICLE[DLLEXPORT, STDCALL](TIME, INODE, IPATHID, DRIVER, IFLT, ITYPE, OVRSPD, IRANGE)
! ----------------------------------------------------------------------
!     This function allows an external entity to insert a vehicle into
!     the simulation.
! ----------------------------------------------------------------------
  USE SIMPARAMS
  USE VEHICLE_MOD
  USE VEHICLE_PARAMETERS
  USE PATH_MOD
  USE QUEUE_MOD
  USE FREEWAY_LINKS
  USE STREET_LINKS
  USE TEXT
  USE VEHICLE_TYPES
  USE GLOBAL_DATA
  USE FREEWAY_VEHICLES
  USE STREET_VEHICLES
  USE SEEDS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: INODE, IPATHID, DRIVER, IFLT, ITYPE, OVRSPD, IRANGE
  REAL, INTENT(IN) :: TIME
  INTEGER :: ENODE, NETWORK, IL, RDIST, STATUS, IV, CFM, NIV
  REAL :: RNDNUM
  TYPE(VEHICLE) :: THIS_VEHICLE
 
! --- Set return value to indicate failure.
 
  ADD_VEHICLE = 0
 
! --- Find which network the vehicle will enter.
 
  NETWORK = 0
  IF(IPATHID .EQ. 0 .AND. INODE .NE. 0) THEN
    ENODE = INODE
  ELSE
 
! --- Validate path id.
 
    IF(IPATHID .NE. 0) THEN
      IF(ALLOCATED(NNODES)) THEN
        IF(NNODES(IPATHID) .EQ. 0) THEN
          MSGTEXT = ''
          CALL SENDTEXTMSG(M_INFO)
          WRITE(MSGTEXT,'(A,I5)') 'ADD_VEHICLE : INVALID PATH ID : ', IPATHID
          CALL SENDTEXTMSG(M_INFO)
          RETURN
        ENDIF              
      ELSE
        MSGTEXT = ''
        CALL SENDTEXTMSG(M_INFO)
        WRITE(MSGTEXT,'(A,I5)') 'ADD_VEHICLE : INVALID PATH ID : ', IPATHID
        CALL SENDTEXTMSG(M_INFO)
        WRITE(MSGTEXT,'(A)') '  NO PATHS HAVE BEEN SPECIFIED'
        CALL SENDTEXTMSG(M_INFO)
        MSGTEXT = ''
        CALL SENDTEXTMSG(M_INFO)
        RETURN
      ENDIF
    ENDIF
    ENODE = PATH_NODES(IPATHID, 1)
  ENDIF
  DO IL = 1, N_FREEWAY_LINKS
    IF(ENODE .EQ. FUSN(IL)) THEN
      NETWORK = 1
      EXIT
    ENDIF
  ENDDO
  IF(NETWORK .EQ. 0) THEN
    DO IL = 1, N_STREET_LINKS
      IF(ENODE .EQ. SUSN(IL)) THEN
        NETWORK = 2
        EXIT
      ENDIF
    ENDDO
  ENDIF
  IF(NETWORK .EQ. 0) THEN
    WRITE(MSGTEXT,'(A,I5)') 'INVALID ENTRY NODE : ', ENODE
    CALL SENDTEXTMSG(M_WARNING)
    RETURN
  ENDIF
 
! --- Reject if fleet is bus.
 
  IF(IFLT .EQ. FLEET_BUS) THEN
    WRITE(MSGTEXT,'(A)') 'FAILURE IN PUT_VEHICLE'
    CALL SENDTEXTMSG(M_WARNING)
    WRITE(MSGTEXT,'(A)') 'PUT_VEHICLE CANNOT ACCEPT BUSES'
    CALL SENDTEXTMSG(M_WARNING)
    RETURN
  ENDIF
 
! --- Reject if type is not represented in the fleet.
 
  RDIST = 0.
  IF(NETWORK .EQ. I_FREEWAY) THEN
    IF(IFLT .EQ. FLEET_AUTO) THEN
      RDIST = FLT_FREEWAY_AUTO(ITYPE)
    ELSEIF(IFLT .EQ. FLEET_TRUCK) THEN
      RDIST = FLT_FREEWAY_TRUCK(ITYPE)
    ELSEIF(IFLT .EQ. FLEET_CARPOOL) THEN
      RDIST = FLT_FREEWAY_CARPOOL(ITYPE)
    ELSEIF(IFLT .EQ. FLEET_EV) THEN
      RDIST = FLT_FREEWAY_EV(ITYPE)
    ENDIF
  ELSE
    IF(IFLT .EQ. FLEET_AUTO) THEN
      RDIST = FLT_STREET_AUTO(ITYPE)
    ELSEIF(IFLT .EQ. FLEET_TRUCK) THEN
      RDIST = FLT_STREET_TRUCK(ITYPE)
    ELSEIF(IFLT .EQ. FLEET_CARPOOL) THEN
      RDIST = FLT_STREET_CARPOOL(ITYPE)
    ELSEIF(IFLT .EQ. FLEET_EV) THEN
      RDIST = FLT_STREET_EV(ITYPE)
    ELSEIF(IFLT .EQ. FLEET_BIKE) THEN
      RDIST = FLT_STREET_BIKE(ITYPE)
    ENDIF
  ENDIF
  IF(RDIST .EQ. 0) THEN
    WRITE(MSGTEXT,'(A)') 'FAILURE IN EMITTING A PATH FOLLOWING VEHICLE.'
    CALL SENDTEXTMSG(M_WARNING)
    IF(IFLT .EQ. FLEET_AUTO) THEN
      WRITE(MSGTEXT,'(A,I2,A)') 'TYPE ', ITYPE, ' IS NOT A MEMBER OF THE AUTO FLEET.'
    ELSEIF(IFLT .EQ. FLEET_TRUCK) THEN
      WRITE(MSGTEXT,'(A,I2,A)') 'TYPE ', ITYPE, ' IS NOT A MEMBER OF THE TRUCK FLEET.'
    ELSEIF(IFLT .EQ. FLEET_CARPOOL) THEN
      WRITE(MSGTEXT,'(A,I2,A)') 'TYPE ', ITYPE, ' IS NOT A MEMBER OF THE CARPOOL FLEET.'
    ELSEIF(IFLT .EQ. FLEET_EV) THEN
      WRITE(MSGTEXT,'(A,I2,A)') 'TYPE ', ITYPE, ' IS NOT A MEMBER OF THE EMERGENCY VEHICLE FLEET.'
    ENDIF
    CALL SENDTEXTMSG(M_WARNING)
    WRITE(MSGTEXT,'(A)') 'USE RECORD TYPE 71 TO DEFINE THIS TYPE'
    CALL SENDTEXTMSG(M_WARNING)
    RETURN
  ENDIF
 
  THIS_VEHICLE%NETWORK = NETWORK
  THIS_VEHICLE%ENTRYLINKID = IL
  THIS_VEHICLE%DEPARTURE = TIME
  THIS_VEHICLE%PATHID = IPATHID
  THIS_VEHICLE%ROUTEID = 0
  THIS_VEHICLE%DRIVER = DRIVER
  THIS_VEHICLE%FLEET = IFLT
  THIS_VEHICLE%VTYPE = ITYPE
  THIS_VEHICLE%TURNCODE = TC_NULL   
  THIS_VEHICLE%OVRSPD = OVRSPD
  THIS_VEHICLE%RANGE = IRANGE
  IF(NETWORK .EQ. I_FREEWAY) THEN
    CALL FREEWAY_RANDOM(SSEED, RNDNUM)
    CALL GET_CAR_FOLLOWING_MODEL(ITYPE, RNDNUM, CFM)
  ELSE
    CALL STREET_RANDOM(SSEED, RNDNUM)
    CALL GET_CAR_FOLLOWING_MODEL(ITYPE, RNDNUM, CFM)
  ENDIF
  THIS_VEHICLE%GLOBALID = 0
  THIS_VEHICLE%CFM = CFM
 
!Put the vehicle into the queue for path following vehicles, even if it is not following a path.
 
  IF(.NOT. ASSOCIATED(PBV_FRONT%PTR)) THEN
    ALLOCATE(PBV_FRONT%PTR, STAT=STATUS)
    IF(STATUS .NE. 0) THEN
      WRITE(MSGTEXT,'(A)') 'FAILED TO ALLOCATE MEMORY'
      CALL SENDTEXTMSG(M_WARNING)
      RETURN
    ENDIF
    PBV_REAR%PTR => PBV_FRONT%PTR
  ELSE
    ALLOCATE(PBV_REAR%PTR%NEXT, STAT=STATUS)
    IF(STATUS .NE. 0) THEN
      WRITE(MSGTEXT,'(A)') 'FAILED TO ALLOCATE MEMORY'
      CALL SENDTEXTMSG(M_WARNING)
      RETURN
    ENDIF
    PBV_REAR%PTR => PBV_REAR%PTR%NEXT
  ENDIF
  PBV_REAR%PTR%VEHICLE = THIS_VEHICLE
  NULLIFY(PBV_REAR%PTR%NEXT)
!
! --- Set the return value to indicate success.
!
  NIV = NIV + 1
  ADD_VEHICLE = NIV
  RETURN
  END

! ==================================================================================================
  INTEGER FUNCTION GET_CURRENT_EVENTS[DLLEXPORT, STDCALL](EVENTDATA)
  USE ANIMATION_DATA
  USE SIMPARAMS
  USE EVENTS
  IMPLICIT NONE
  TYPE(API_LKINCDATA) :: EVENTDATA(NUMBER_OF_EVENTS)
  INTEGER :: I, N
! ----------------------------------------------------------------------
  GET_CURRENT_EVENTS = 0
  DO I = 1, NUMBER_OF_EVENTS
    IF(ALLEVENTS(I)%INCIDENTID .EQ. 0) CYCLE
    IF(SIMTIME .LT. ALLEVENTS(I)%STARTTIME) CYCLE
    IF(SIMTIME .LE. ALLEVENTS(I)%STARTTIME + ALLEVENTS(I)%DURATION) THEN
      GET_CURRENT_EVENTS = GET_CURRENT_EVENTS + 1
      EVENTDATA(GET_CURRENT_EVENTS)%INCIDENTID       = ALLEVENTS(I)%INCIDENTID 
      EVENTDATA(GET_CURRENT_EVENTS)%LINKID           = ALLEVENTS(I)%LINKID
      EVENTDATA(GET_CURRENT_EVENTS)%INCIDENTTYPE     = ALLEVENTS(I)%INCIDENTTYPE
      EVENTDATA(GET_CURRENT_EVENTS)%INCIDENTPOSITION = ALLEVENTS(I)%INCIDENTPOSITION
      EVENTDATA(GET_CURRENT_EVENTS)%INCIDENTLENGTH   = ALLEVENTS(I)%INCIDENTLENGTH
      EVENTDATA(GET_CURRENT_EVENTS)%STARTTIME        = ALLEVENTS(I)%STARTTIME
      EVENTDATA(GET_CURRENT_EVENTS)%DURATION         = ALLEVENTS(I)%DURATION
      EVENTDATA(GET_CURRENT_EVENTS)%REACTIONPOINT    = ALLEVENTS(I)%REACTIONPOINT
      EVENTDATA(GET_CURRENT_EVENTS)%RUBBERNECKFACTOR = ALLEVENTS(I)%RUBBERNECKFACTOR
      EVENTDATA(GET_CURRENT_EVENTS)%MODELTYPE        = ALLEVENTS(I)%MODELTYPE
      EVENTDATA(GET_CURRENT_EVENTS)%MSTATE           = ALLEVENTS(I)%MSTATE
      EVENTDATA(GET_CURRENT_EVENTS)%NUMAFFECTEDLANES = ALLEVENTS(I)%NUMAFFECTEDLANES
 
      DO N = 1, ALLEVENTS(I)%NUMAFFECTEDLANES
        EVENTDATA(GET_CURRENT_EVENTS)%AFFECTEDLANEIDARRAY(N) = ALLEVENTS(I)%AFFECTEDLANEIDARRAY(N)
        EVENTDATA(GET_CURRENT_EVENTS)%LANEINCIDENTCODES(N) = ALLEVENTS(I)%LANEINCIDENTCODES(N)
      ENDDO
    ENDIF
  ENDDO
  RETURN
  END  

! ==================================================================================================
! These functions are provided to allow our NTCIP interface program to get data from ETFOMM and
! to send data to ETFOMM.
! ==================================================================================================
  REAL FUNCTION GET_SIMTIME[DLLEXPORT, STDCALL] ()
  USE SIMPARAMS
  IMPLICIT NONE
! ----------------------------------------------------------------------
  GET_SIMTIME = SIMTIME
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_USER_DCS_ENTRIES[DLLEXPORT, STDCALL] (I1, I2, D1, D2)
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER :: I1[REFERENCE], I2[REFERENCE]
  REAL :: D1[REFERENCE], D2[REFERENCE]
! ----------------------------------------------------------------------
  I1 = USE_DCS
  I2 = LIMIT_TO_MAXGREEN
  D1 = DZ_ENTRY_TIME
  D2 = DZ_EXIT_TIME
  GET_USER_DCS_ENTRIES = 0
  END

! ==================================================================================================
  INTEGER FUNCTION SET_USER_DCS_ENTRIES[DLLEXPORT, STDCALL] (I1, D1, D2)
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER :: I1
  REAL :: D1, D2
! ----------------------------------------------------------------------
  LIMIT_TO_MAXGREEN = I1
  DZ_ENTRY_TIME = D1
  DZ_EXIT_TIME = D2
  SET_USER_DCS_ENTRIES = 0
  END

! ==================================================================================================
  INTEGER FUNCTION GET_TIMINGPLAN[DLLEXPORT, STDCALL] (OUTPUT_STRUCT)
  USE NTCIP_DATA
  USE ACTUATED_CONTROLLERS
  IMPLICIT NONE
  TYPE ARG    
    REAL :: MINGREEN(8)
    REAL :: PASSAGE(8)
    REAL :: MAXGREEN(8)
    REAL :: YELLOWCHANGE(8)
    REAL :: REDCLEAR(8)
  END TYPE
  TYPE(ARG) :: OUTPUT_STRUCT   [REFERENCE]
! ----------------------------------------------------------------------
  OUTPUT_STRUCT%MINGREEN = AC_SIGNALS(1)%GUI_MIN_GREEN_TIMES(NPHASES)
  OUTPUT_STRUCT%PASSAGE = AC_SIGNALS(1)%GUI_DEFAULT_EXTENSION_TIMES
  OUTPUT_STRUCT%MAXGREEN = AC_SIGNALS(1)%GUI_MAX_GREEN_TIMES
  OUTPUT_STRUCT%YELLOWCHANGE = AC_SIGNALS(1)%GUI_YC
  OUTPUT_STRUCT%REDCLEAR = AC_SIGNALS(1)%GUI_RC
  GET_TIMINGPLAN = 0
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_PHASE_OPTIONS[DLLEXPORT, STDCALL] (PHASE_OPTIONS)
  USE NTCIP_DATA
  USE ACTUATED_CONTROLLERS
  IMPLICIT NONE
  INTEGER :: PHASE_OPTIONS(8)
  INTEGER :: PHASE
  INTEGER, PARAMETER :: RECALL_MIN = 1
  INTEGER, PARAMETER :: RECALL_MAX = 2
! ----------------------------------------------------------------------
  PHASE_OPTIONS = 0
  DO PHASE = 1, 8
    IF(AC_SIGNALS(1)%PHASE(PHASE)%IN_USE) PHASE_OPTIONS(PHASE) = IBSET(PHASE_OPTIONS(PHASE), 0)
    IF(AC_SIGNALS(1)%GUI_ACTUATED_MODE(PHASE) .EQ. RECALL_MIN) THEN
      PHASE_OPTIONS(PHASE) = IBSET(PHASE_OPTIONS(PHASE), 6)
    ELSEIF(AC_SIGNALS(1)%GUI_ACTUATED_MODE(PHASE) .EQ. RECALL_MAX) THEN
      PHASE_OPTIONS(PHASE) = IBSET(PHASE_OPTIONS(PHASE), 7)
    ENDIF
  ENDDO
  GET_PHASE_OPTIONS = 0
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_PHASE_INSTRUCTIONS[DLLEXPORT, STDCALL] (PHASE_INSTRUCTIONS)
  USE NTCIP_DATA
  IMPLICIT NONE
  INTEGER :: PHASE_INSTRUCTIONS(2)
! ----------------------------------------------------------------------
  PHASE_INSTRUCTIONS(1) = EXTENSION_GROUP
  PHASE_INSTRUCTIONS(2) = FORCEOFF_GROUP
  GET_PHASE_INSTRUCTIONS = 0
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_PHASE_CALLS[DLLEXPORT, STDCALL] (IACT, PHASE_CALLS)
  USE NTCIP_DATA
  IMPLICIT NONE
  INTEGER :: IACT
  INTEGER :: PHASE_CALLS   [REFERENCE]
! ----------------------------------------------------------------------
  !CALL READ_DETECTOR_FILE
  CALL PACKAGE_DETECTOR_STATES(IACT)
  PHASE_CALLS = PHASE_CALLS_GROUP
  GET_PHASE_CALLS = 0
  END
  
! ==================================================================================================
  SUBROUTINE PACKAGE_DETECTOR_STATES(IACT)
  USE NTCIP_DATA
  USE ACTUATED_CONTROLLERS
  USE STREET_DETECTORS
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER :: IACT
  INTEGER :: I, IDET, PHASE, ICOLOR(8)
! ----------------------------------------------------------------------
  !Set all phase calls to 0
  PHASE_CALLS_GROUP = 0
  
  !!Get current phase colors
  !DO I = 1, 8
  !  IF(BTEST(GREEN_PHASES, PHASE-1)) THEN
  !    ICOLOR(I) = GREEN
  !  ELSEIF(BTEST(YELLOW_PHASES, PHASE-1)) THEN
  !    ICOLOR(I) = YELLOW
  !  ELSE
  !    ICOLOR(I) = RED
  !  ENDIF
  !ENDDO
  
  !This will send current detector states
  DO I = 1, AC_SIGNALS(IACT)%SDP%DETECTOR_COUNT
    IDET = AC_SIGNALS(IACT)%SDP%DETECTOR_LIST(I)
    IF(SDETECTOR(IDET)%CURRENT_STATE .NE. 0) THEN
      PHASE = SDETECTOR(IDET)%ASSOCIATED_PHASE
      !IF(ICOLOR(PHASE) .EQ. RED) PHASE_CALLS_GROUP = IBSET(PHASE_CALLS_GROUP, PHASE-1) !If reading from file don't consider color, comment this out
      PHASE_CALLS_GROUP = IBSET(PHASE_CALLS_GROUP, PHASE-1)
    ENDIF
  ENDDO
  !    
  !!This will convert detector states to phase calls based on detector operation mode
  !DO IDET = 1, N_STREET_DETECTORS
  !  IF(SDETECTOR(IDET)%OPERATION_CODE .EQ. 0) THEN
  !    !!!Presence Mode Detector
  !    IF(SDETECTOR(IDET)%CURRENT_STATE .NE. 0) THEN    
  !      PHASE = SDETECTOR(IDET)%ASSOCIATED_PHASE
  !      IF(ICOLOR(PHASE) .EQ. RED) PHASE_CALLS_GROUP = IBSET(PHASE_CALLS_GROUP, PHASE-1)
  !    ENDIF
  !  ELSE
  !    !!!Passage Mode Detector
  !    IF(SDETECTOR(IDET)%CURRENT_STATE .EQ. 0 .AND. SDETECTOR(IDET)%PREVIOUS_STATE .EQ. 1) THEN
  !      PHASE = SDETECTOR(IDET)%ASSOCIATED_PHASE
  !      IF(ICOLOR(PHASE) .EQ. RED) PHASE_CALLS_GROUP = IBSET(PHASE_CALLS_GROUP, PHASE-1)
  !    ENDIF
  !  ENDIF
  !ENDDO
  RETURN
  END

! ==================================================================================================
  INTEGER FUNCTION PUT_GREEN_PHASES[DLLEXPORT, STDCALL] (NODE, GREENS)
  USE NTCIP_DATA
  USE SIMPARAMS
  USE TEXT
  USE ACTUATED_CONTROLLERS
  use STREET_NODES
  IMPLICIT NONE
  INTEGER :: NODE, IACT, GREENS
  INCLUDE 'IOFILES.INC'
!----------------------------------------------------------------------
! Returns: int =  0 - success
!              = -1 - controller was not found
! ----------------------------------------------------------------------
  IACT = NACT(NODE)
  IF(IACT .EQ. 0) THEN
    PUT_GREEN_PHASES = -1
    WRITE(MSGTEXT,'(A,I4)') 'PUT_GREEN_PHASES: ACTUATED CONTROLLER NOT FOUND AT NODE ', NODE
    CALL SENDTEXTMSG(M_INFO)
  ELSE
    OPEN(1, FILE = LINFNAME(1:IROOT-1)//'.LOG', ACCESS='APPEND', ERR=10, IOMSG=ETEXT)
    WRITE(1, '(A,F8.1,A,I4,A,I8)') 'Time: ', SIMTIME, 'NODE: ', ABS(AC_SIGNALS(IACT)%NODE(1)), ' GREEN PHASES: ', GREENS
    CLOSE(1)
    GREEN_PHASES(IACT) = GREENS
    PUT_GREEN_PHASES = 0
  10 CONTINUE 
    WRITE(MSGTEXT,'(A)') 'FILE OPEN ERROR: PUT_GREEN_PHASES'
    CALL SENDTEXTMSG(M_INFO)
    WRITE(MSGTEXT, '(A)') ETEXT
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION GET_ETFOMM_PHASE_STATES[DLLEXPORT, STDCALL](IACT, GP, YP)
  USE ACTUATED_CONTROLLERS
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER :: IACT, GP [REFERENCE], YP [REFERENCE]
  INTEGER :: RING, PHASE
!----------------------------------------------------------------------
  GP = 0
  YP = 0
  DO RING = 1, 2
    PHASE = AC_SIGNALS(IACT)%CURRENT_PHASES(RING)
    IF(PHASE .NE. 0) THEN
      IF(AC_SIGNALS(IACT)%SDP%CURRENT_COLORS(RING) .EQ. GREEN) THEN
        GP = IBSET(GP, PHASE - 1)
      ELSEIF(AC_SIGNALS(IACT)%SDP%CURRENT_COLORS(RING) .EQ. YELLOW) THEN
        YP = IBSET(YP, PHASE - 1)
      ENDIF
    ENDIF
  ENDDO 
  GET_ETFOMM_PHASE_STATES = 0
  RETURN
  END 
  
! ==================================================================================================
  INTEGER FUNCTION SET_ETFOMM_PHASE_STATES[DLLEXPORT, STDCALL] (NODE, GP, YP)
  USE SIMPARAMS
  USE ACTUATED_CONTROLLERS
  USE STREET_LINKS
  USE STREET_NODES
  USE GLOBAL_DATA
  USE NTCIP_DATA
  USE TEXT
  IMPLICIT NONE
  INTEGER :: NODE, IACT, GP[REFERENCE], YP[REFERENCE]
  INTEGER :: RING, PHASE, ICOLOR, ILINK, IL
! ----------------------------------------------------------------------
! Returns: int =  0 - success
!              = -1 - controller was not found
! ----------------------------------------------------------------------
  IACT = NACT(NODE)
  IF(IACT .EQ. 0) THEN
    SET_ETFOMM_PHASE_STATES = -1
    WRITE(MSGTEXT,'(A,I4)') 'SET_ETFOMM_PHASE_STATES: ACTUATED CONTROLLER NOT FOUND AT NODE ', NODE
    CALL SENDTEXTMSG(M_INFO)
  ELSE    
    GREEN_PHASES(IACT) = GP
    YELLOW_PHASES(IACT) = YP
  
    AC_SIGNALS(IACT)%CURRENT_PHASES = 0
    AC_SIGNALS(IACT)%SDP%CURRENT_COLORS = RED
  
    DO ILINK = 1, AC_SIGNALS(IACT)%N_DIRECT_APPROACHES
      IL = AC_SIGNALS(IACT)%DIRECT_APPROACH(ILINK)
      SIGNAL_LEFT(IL) = .FALSE.
      SIGNAL_THRU(IL) = .FALSE.
      SIGNAL_RIGHT(IL) = .FALSE.
      SIGNAL_DIAG(IL) = .FALSE.
      AMBER_LEFT(IL) = .FALSE.
      AMBER_THRU(IL) = .FALSE.
      AMBER_RIGHT(IL) = .FALSE.
      AMBER_DIAG(IL) = .FALSE.
    ENDDO
  
    DO ILINK = 1, AC_SIGNALS(IACT)%N_DIRECT_APPROACHES
      IL = AC_SIGNALS(IACT)%DIRECT_APPROACH(ILINK)
      DO PHASE = 1, 8
        IF(PHASE .LE. 4) THEN
          RING = 1
        ELSE
          RING = 2
        ENDIF
        IF(BTEST(GP, PHASE-1)) THEN
          ICOLOR = GREEN
        ELSEIF(BTEST(YP, PHASE-1)) THEN
          ICOLOR = YELLOW
        ELSE
          ICOLOR = RED
        ENDIF
        IF(ICOLOR .NE. RED) THEN
          AC_SIGNALS(IACT)%CURRENT_PHASES(RING) = PHASE
        ENDIF
        IF(ICOLOR .EQ. GREEN) THEN
          AC_SIGNALS(IACT)%SDP%CURRENT_COLORS(RING) = GREEN
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
        ELSEIF(ICOLOR .EQ. YELLOW) THEN
          AC_SIGNALS(IACT)%SDP%CURRENT_COLORS(RING) = YELLOW
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
          IF(AC_SIGNALS(IACT)%PHASE(PHASE)%LEFTARROW(ILINK)) THEN
            TIME_IN_RED_LEFT(IL) = TIME_IN_RED_LEFT(IL) + TIMESTEP
          ENDIF
          IF(AC_SIGNALS(IACT)%PHASE(PHASE)%THRUARROW(ILINK)) THEN
            TIME_IN_RED_THRU(IL) = TIME_IN_RED_THRU(IL) + TIMESTEP
          ENDIF
          IF(AC_SIGNALS(IACT)%PHASE(PHASE)%RIGHTARROW(ILINK)) THEN
            TIME_IN_RED_RIGHT(IL) = TIME_IN_RED_RIGHT(IL) + TIMESTEP
          ENDIF
          IF(AC_SIGNALS(IACT)%PHASE(PHASE)%LDIAGARROW(ILINK) .OR. AC_SIGNALS(IACT)%PHASE(PHASE)%RDIAGARROW(ILINK)) THEN
            TIME_IN_RED_DIAG(IL) = TIME_IN_RED_DIAG(IL) + TIMESTEP
          ENDIF
        ENDIF
      ENDDO
    ENDDO
    SET_ETFOMM_PHASE_STATES = 0
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION SET_ETFOMM_MOE_DATA[DLLEXPORT, STDCALL](NODE, MAXOUTS, MIN_GREENS, TIMES_STARTED)
  USE ACTUATED_CONTROLLERS
  USE STREET_NODES
  USE GLOBAL_DATA
  USE TEXT
  IMPLICIT NONE
  INTEGER :: NODE, IACT, MAXOUTS(8), MIN_GREENS(8), TIMES_STARTED(8)
!----------------------------------------------------------------------
! Returns: int =  0 - success
!              = -1 - controller was not found
!----------------------------------------------------------------------
  IACT = NACT(NODE)
  IF(IACT .EQ. 0) THEN
    SET_ETFOMM_MOE_DATA = -1
    WRITE(MSGTEXT,'(A,I4)') 'SET_ETFOMM_MOE_DATA: ACTUATED CONTROLLER NOT FOUND AT NODE ', NODE
    CALL SENDTEXTMSG(M_INFO)
  ELSE
    AC_SIGNALS(IACT)%MAXOUTS = MAXOUTS
    AC_SIGNALS(IACT)%MIN_GREENS = MIN_GREENS
    AC_SIGNALS(IACT)%TIMES_STARTED = TIMES_STARTED
    SET_ETFOMM_MOE_DATA = 0
  ENDIF
  RETURN
  END 
  
! ==================================================================================================
  INTEGER FUNCTION PUT_YELLOW_PHASES[DLLEXPORT, STDCALL] (IACT, YELLOWS)
  USE NTCIP_DATA
  USE SIMPARAMS
  USE TEXT
  USE ACTUATED_CONTROLLERS
  IMPLICIT NONE
  INTEGER :: IACT, YELLOWS
  INCLUDE 'IOFILES.INC'
! ----------------------------------------------------------------------
  OPEN(1, FILE = LINFNAME(1:IROOT-1)//'.LOG', ACCESS='APPEND', ERR=10, IOMSG=ETEXT)
  WRITE(1, '(A,F8.1,A,I4,A,I8)') 'Time: ', SIMTIME, 'NODE: ', ABS(AC_SIGNALS(IACT)%NODE(1)), ' YELLOW PHASES: ', YELLOWS
  CLOSE(1)
  YELLOW_PHASES(IACT) = YELLOWS
  PUT_YELLOW_PHASES = 0
10 CONTINUE 
  WRITE(MSGTEXT,'(A)') 'FILE OPEN ERROR - PUT_YELLOW_PHASES'
  CALL SENDTEXTMSG(M_INFO)
  WRITE(MSGTEXT, '(A)') ETEXT
  CALL SENDTEXTMSG(M_ERROR)
  END
  
! ==================================================================================================
  INTEGER FUNCTION PUT_RING_STATUS_BITS[DLLEXPORT, STDCALL] (CODE1, CODE2)
  USE NTCIP_DATA
  USE ACTUATED_CONTROLLERS
  IMPLICIT NONE
  INTEGER :: CODE1, CODE2
  INTEGER :: RING, I, BIT(0:4), PHASE, IC(2)
! ----------------------------------------------------------------------
!+======+=====+=====+=====+===============+
!| Code | Bit States | State |
!| ## | A | B | C | Names |
!+======+=====+=====+=====+===============+
!|  0 | 0 | 0 | 0 | Min Green |
!|  1 | 1 | 0 | 0 | Extension |
!|  2 | 0 | 1 | 0 | Maximum |
!|  3 | 1 | 1 | 0 | Green Rest |
!|  4 | 0 | 0 | 1 | Yellow Change |
!|  5 | 1 | 0 | 1 | Red Clearance |
!|  6 | 0 | 1 | 1 | Red Rest |
!|  7 | 1 | 1 | 1 | Undefined |
!+======+=====+=====+=====+===============+
  IC(1) = CODE1
  IC(2) = CODE2
  DO RING = 1, 2
    DO I = 0, 4
      BIT(I) = BTEST(IC(RING), I)
    ENDDO
    SBA(RING) = BIT(0)
    SBB(RING) = BIT(1)
    SBC(RING) = BIT(2)
    PHASE = AC_SIGNALS(1)%CURRENT_PHASES(RING)
    IF(PHASE .NE. 0) THEN
      IF(BIT(4) .EQ. 1) THEN
        AC_SIGNALS(1)%MAXOUTS(PHASE) = AC_SIGNALS(1)%MAXOUTS(PHASE) + 1
      ELSEIF(IC(RING) .EQ. 0) THEN
        AC_SIGNALS(1)%MIN_GREENS(PHASE) = AC_SIGNALS(1)%MIN_GREENS(PHASE) + 1
      ENDIF
    ENDIF
  ENDDO
  PUT_RING_STATUS_BITS = 0
  END

! ==================================================================================================
! End of NTCIP related functions.  
! ==================================================================================================

! ==================================================================================================
! These functions are provided to allow ETEditor or any similar application to control a simulation.
! ==================================================================================================
  INTEGER FUNCTION GET_ETFOMM_MESSAGE_COUNT[DLLEXPORT, STDCALL]
  USE TEXT
  IMPLICIT NONE
! ----------------------------------------------------------------------
  GET_ETFOMM_MESSAGE_COUNT = NTEXT
  END

! ==================================================================================================
  INTEGER FUNCTION SAVE_SIMULATION[DLLEXPORT, STDCALL](FILE1, FILE2)
  USE TEXT
  IMPLICIT NONE
  STRUCTURE /FILE/
    CHARACTER*512 NAME
    INTEGER LEN
  END STRUCTURE
  RECORD /FILE/ FILE1[REFERENCE]
  RECORD /FILE/ FILE2[REFERENCE]
! ----------------------------------------------------------------------
  WRITE(MSGTEXT, '(A)') '  SIMULATION DATA WILL BE SAVED TO FILES'
  CALL SENDTEXTMSG(M_INFO)
  CALL SAVE_SIMULATION_DATA(FILE1, FILE2)
  SAVE_SIMULATION = 0
  END

! ==================================================================================================
  INTEGER FUNCTION RESTORE_SIMULATION[DLLEXPORT, STDCALL](FILE1, FILE2)
  USE TEXT
  IMPLICIT NONE
  STRUCTURE /FILE/
    CHARACTER*512 NAME
    INTEGER LEN
  END STRUCTURE
  RECORD /FILE/ FILE1[REFERENCE]
  RECORD /FILE/ FILE2[REFERENCE]
! ----------------------------------------------------------------------
  WRITE(MSGTEXT, '(A)') '  SIMULATION DATA WILL BE RESTORED FROM FILES'
  CALL SENDTEXTMSG(M_INFO)
  CALL RESTORE_SIMULATION_DATA(FILE1, FILE2)
  RESTORE_SIMULATION = 0
  END

! ==================================================================================================
  INTEGER FUNCTION GET_ETFOMM_MESSAGE[DLLEXPORT, STDCALL](OUT_BUFFER)
  USE TEXT
  IMPLICIT NONE
  CHARACTER*120 :: OUT_BUFFER      [REFERENCE]
! ----------------------------------------------------------------------
  MSG_POINTER = MSG_POINTER + 1
  OUT_BUFFER = TEXT_BUFFER(MSG_POINTER)
  OUT_BUFFER(120:120) = CHAR(0)
  GET_ETFOMM_MESSAGE = TEXT_TYPE(MSG_POINTER)
  IF(MSG_POINTER .EQ. NTEXT) THEN
    NTEXT = 0
    MSG_POINTER = 0
    TEXT_BUFFER = ''
    TEXT_TYPE = 0
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION API_STARTUP[DLLEXPORT, STDCALL]
!
!     return values
!         0 - no errors
!         1 - failure opening files
!         3 - errors in input
!

  USE SIMPARAMS
  USE TEXT
  USE FREEWAY_LINKS
  USE STREET_LINKS
  USE INCIDENTS
  USE GLOBAL_DATA
  USE FREEWAY_VEHICLES
  USE STREET_VEHICLES
  USE API_DATA
  USE NODE_TABLE
  IMPLICIT NONE
  INCLUDE 'VERSION.FI'
  INTEGER :: ERRORS
! ----------------------------------------------------------------------
  WRITE_MOES = .TRUE.   
  READING_TRF = .FALSE.
  
! --- START THE CPU TIMER.

  CALL CPU_TIME(START_CPU)
  PRIOR_CPU = START_CPU
  START_TIMER = SECNDS(0.0)
  TIME_PERIOD = 1
  LAST_ID_USED = 0
  HIGHEST_INDEX_F = 0
  HIGHEST_INDEX_S = 0
  SORTED_LIST_LENGTH = 0
  CALL INIT_QUEUES
  
  IF(TYPE_OF_RUN .EQ. 1 .OR. TYPE_OF_RUN .EQ. -1) THEN
    WRITE(MSGTEXT, '(A)') 'CHECKING FOR INPUT ERRORS FOR TIME PERIOD 1'
    CALL SENDTEXTMSG(M_INFO)
    CALL CHECK_INPUTS
    MSGTEXT = ''
    CALL SENDTEXTMSG(M_INFO)
    IF(ERROR_COUNT .EQ. 0) THEN
      WRITE(MSGTEXT, '(A)') '  NO ERRORS WERE FOUND'
      CALL SENDTEXTMSG(M_INFO)
    ELSEIF(ERROR_COUNT .EQ. 1) THEN
      WRITE(MSGTEXT, '(A, I4, A)') '**** FOUND 1 ERROR ****'
      CALL SENDTEXTMSG(M_INFO)
    ELSE
      WRITE(MSGTEXT, '(A, I4, A)') '**** FOUND ', ERROR_COUNT, ' ERROR(S) ****'
      CALL SENDTEXTMSG(M_INFO)
    ENDIF
  ENDIF
 
  IF(ERROR_FLAG .EQ. 0 .AND. ERROR_COUNT .EQ. 0) THEN
    API_STARTUP = 0
#ifdef WIN64
    WRITE(MSGTEXT, '(A, A, A)') '------------- ETFOMM 64 BIT VERSION ', KVER, ' ----------------'
#else
    WRITE(MSGTEXT, '(A, A, A)') '------------- ETFOMM 32 BIT VERSION ', KVER, ' ----------------'
#endif    
    CALL SENDTEXTMSG(M_INFO)
    IF(TYPE_OF_RUN .NE. -1) THEN
      WRITE(MSGTEXT, '(A)') ''
      CALL SENDTEXTMSG(M_INFO)
      CALL ALLOCATE_NODE_ARRAYS(MAX_NODE_NUMBER + 1000)
      IF(N_FREEWAY_LINKS .GT. 0) THEN
        CALL ALLOCATE_FREEWAY_VEHICLE_ARRAYS
        CALL PROCESS_FREEWAYLINKS
        CALL ORGANIZE_FREEWAY_LINKS
      ENDIF
      IF(N_STREET_LINKS .GT. 0) THEN
        CALL ALLOCATE_STREET_VEHICLE_ARRAYS
        CALL PROCESS_STREETLINKS  
        CALL PROCESS_ACTUATED_CONTROL
      ENDIF
      IF(N_FREEWAY_LINKS .GT. 0 .AND. N_STREET_LINKS .GT. 0) THEN
        CALL ALLOCATE_FREEWAY_INTERFACE_ARRAYS
        CALL ALLOCATE_STREET_INTERFACE_ARRAYS
      ENDIF
      IF(ERROR_FLAG .NE. 0 .OR. ERROR_COUNT .NE. 0) THEN
        API_STARTUP = 3
        CALL CPU_TIME(PRIOR_CPU)
      ENDIF
    ENDIF
  ELSE
    API_STARTUP = 3
    CALL CPU_TIME(PRIOR_CPU)
  ENDIF
  
#ifndef TSIS_COMPATIBLE
  IF(WTRF) THEN
    CALL WRITE_TRF_FILE(ERRORS)
  ENDIF
#endif

  END

! ==================================================================================================
  INTEGER FUNCTION GET_RUN_INPUTS[DLLEXPORT, STDCALL](R, S1, S2, S3, L1, L2, L3, L4, L5, L6)
  USE SIMPARAMS
  USE SEEDS
  USE FREEWAY_VEHICLES
  USE STREET_VEHICLES
  IMPLICIT NONE
  INTEGER :: R, S1, S2, S3, L1, L2, L3, L4, L5, L6
! ----------------------------------------------------------------------
  R = RUN_NUMBER
  S1 = ISEED1
  S2 = ISEED2
  S3 = ISEED3
  L1 = USE_NTCIP
  L2 = USE_DCS
  L3 = LOGGING
  L4 = USE_EXTERNAL_DETECTORS
  L5 = WTRF
  L6 = STOCHASTIC
  GET_RUN_INPUTS = 0
  END

! ==================================================================================================
  INTEGER FUNCTION GET_NETWORK_INPUTS[DLLEXPORT, STDCALL](OUTPUTS)
  USE API_DATA
  USE SIMPARAMS
  IMPLICIT NONE
  TYPE(NETWORK_INPUTS) :: OUTPUTS  [REFERENCE]
! ----------------------------------------------------------------------
  OUTPUTS%TIMESTEP = TIMESTEP
  OUTPUTS%TIME_PERIOD_DURATION = TPSECONDS
  OUTPUTS%TIME_INTERVAL = TIME_INTERVAL
  OUTPUTS%RUN_INIT = .NOT. SKIP_INIT
  OUTPUTS%INITIALIZATION_END = INITIALIZATION_END
  OUTPUTS%TYPE_OF_RUN = TYPE_OF_RUN
  OUTPUTS%SIM_START_TIME = SIM_START_TIME
  OUTPUTS%MAX_NODE_NUMBER = MAX_NODE_NUMBER
  OUTPUTS%DLC_MULT = DLC_MULT
  OUTPUTS%DLC_THRESHOLD = HMIN
  GET_NETWORK_INPUTS = 0
  END

! ==================================================================================================
  INTEGER FUNCTION GET_FREEWAY_NETWORK_INPUTS[DLLEXPORT, STDCALL](OUTPUTS)
  USE API_DATA
  USE CAR_FOLLOWING
  USE DRIVERS
  USE GLOBAL_DATA
  IMPLICIT NONE
  TYPE(FREEWAY_NETWORK_INPUTS) :: OUTPUTS
! ----------------------------------------------------------------------
  OUTPUTS%LAG_ACCEL = LAG_ACCEL
  OUTPUTS%LAG_DECEL = LAG_DECEL
  OUTPUTS%CFRICT = CFRICT
  OUTPUTS%DEFAULT_HOV_PCT = DEFAULT_HOV_PCT
  OUTPUTS%ZFOLL_PITT = FZFOLL
  OUTPUTS%ZFOLL_IDM = FZFOLL_IDM
  OUTPUTS%PITT_SEP = FPCFSEP
  OUTPUTS%IDM_SEP = FIDMSEP
  OUTPUTS%FFSPEED_ADJ = FFSPEED_ADJ(1:10, 1)
  OUTPUTS%FREEWAY_PCT_COOP = FREEWAY_PCT_COOP
  OUTPUTS%LC_TIME = LC_TIME_FREEWAY
  GET_FREEWAY_NETWORK_INPUTS = 0
  END

! ==================================================================================================
  INTEGER FUNCTION GET_STREET_NETWORK_INPUTS[DLLEXPORT, STDCALL](OUTPUTS)
  USE API_DATA
  USE CAR_FOLLOWING
  USE DRIVERS
  USE BUS_STATION_DATA
  USE GLOBAL_DATA
  USE DISTRIBUTIONS
  USE PEDS
  USE EVENTS
  IMPLICIT NONE
  TYPE(STREET_NETWORK_INPUTS) :: OUTPUTS
  INTEGER :: ITYPE, IRAND
! ----------------------------------------------------------------------
  OUTPUTS%ACCEPTABLE_GAP = ACCEPTABLE_GAP
  OUTPUTS%ACCEPTABLE_LTG = ACCEPTABLE_LTG
  OUTPUTS%ACCEPTABLE_RTG = ACCEPTABLE_RTG
  OUTPUTS%ADDITIONAL_GAP = ADDITIONAL_GAP
  OUTPUTS%AMBER_DECEL = AMBER_DECEL
  OUTPUTS%FFSPEED_ADJ = FFSPEED_ADJ(1:10, 2)
  DO ITYPE = 1, 6
    DO IRAND = 1, 10
      OUTPUTS%DWELL_MULTIPLIER(IRAND, ITYPE) = DWELL_MULTIPLIER(ITYPE, IRAND)
    ENDDO
  ENDDO
  OUTPUTS%ZFOLL_PITT = SZFOLL
  OUTPUTS%ZFOLL_IDM = SZFOLL_IDM
  OUTPUTS%PITT_SEP = SPCFSEP
  OUTPUTS%IDM_SEP = SIDMSEP
  OUTPUTS%LC_TIME = LC_TIME_STREET
  OUTPUTS%LT_JUMPER_PROB = LT_JUMPER_PROB
  OUTPUTS%LT_LAGGER_PROB = LT_LAGGER_PROB
  OUTPUTS%SPILLBACK_PROB = SPILLBACK_PROB
  OUTPUTS%STOP_SPD = STOP_SPD
  OUTPUTS%STREET_PCT_COOP = STREET_PCT_COOP
  OUTPUTS%YIELD_SPD = YIELD_SPD
  OUTPUTS%DRIVER_FAMPCT = DRIVER_FAMPCT
  OUTPUTS%PDELAY_WEAK = PDELAY_WEAK 
  OUTPUTS%PDELAY_STRONG = PDELAY_STRONG
  OUTPUTS%PED_DURATION = PED_DURATION
  OUTPUTS%QFACTOR = QFACTOR
  OUTPUTS%STE_MULT = STE_MULT
  OUTPUTS%TURNSIGNAL_PROB = TURNSIGNAL_PROB
  OUTPUTS%TURNSIGNAL_DIST = TURNSIGNAL_DIST
  GET_STREET_NETWORK_INPUTS = 0
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_VEHICLE_TYPES[DLLEXPORT, STDCALL](OUTPUTS)
  USE API_DATA
  USE VEHICLE_TYPES
  IMPLICIT NONE
  TYPE(VEHICLE_TYPE_DATA) :: OUTPUTS(NTYPES)
  INTEGER :: I
! ----------------------------------------------------------------------
  DO I = 1, NTYPES
    OUTPUTS(I)%LENGTH                = VTLENGTH(I)            
    OUTPUTS(I)%HEADWAY_FACTOR        = HDWY_FACTOR(I) * 100        
    OUTPUTS(I)%AVERAGE_OCCUPANCY     = AVG_OCCS(I)          
    OUTPUTS(I)%NON_EMERGENCY_DECEL   = NEMDEC(I)               
    OUTPUTS(I)%FLEET_FREEWAY_AUTO    = FLT_FREEWAY_AUTO(I)    
    OUTPUTS(I)%FLEET_FREEWAY_TRUCK   = FLT_FREEWAY_TRUCK(I)   
    OUTPUTS(I)%FLEET_FREEWAY_CARPOOL = FLT_FREEWAY_CARPOOL(I) 
    OUTPUTS(I)%FLEET_FREEWAY_BUS     = FLT_FREEWAY_BUS(I)     
    OUTPUTS(I)%FLEET_FREEWAY_EV      = FLT_FREEWAY_EV(I)      
    OUTPUTS(I)%FLEET_STREET_AUTO     = FLT_STREET_AUTO(I)     
    OUTPUTS(I)%FLEET_STREET_TRUCK    = FLT_STREET_TRUCK(I)    
    OUTPUTS(I)%FLEET_STREET_CARPOOL  = FLT_STREET_CARPOOL(I)  
    OUTPUTS(I)%FLEET_STREET_BUS      = FLT_STREET_BUS(I)      
    OUTPUTS(I)%FLEET_STREET_EV       = FLT_STREET_EV(I)       
    OUTPUTS(I)%FLEET_STREET_BIKE     = FLT_STREET_BIKE(I)     
    OUTPUTS(I)%PCT_PITT              = PCT_PITT(I) 
    OUTPUTS(I)%PCT_IDM               = PCT_IDM(I)  
    OUTPUTS(I)%PCT_ACC               = PCT_ACC(I)  
    OUTPUTS(I)%PCT_CACC              = PCT_CACC(I)  
  ENDDO
  GET_VEHICLE_TYPES = 0
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_NUMBER_OF_FREEWAY_DETECTORS[DLLEXPORT, STDCALL]()
  USE FREEWAY_DETECTORS
  IMPLICIT NONE
! ----------------------------------------------------------------------
  GET_NUMBER_OF_FREEWAY_DETECTORS = N_FREEWAY_DETECTORS
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_NUMBER_OF_STREET_DETECTORS[DLLEXPORT, STDCALL]()
  USE STREET_DETECTORS
  IMPLICIT NONE
! ----------------------------------------------------------------------
  GET_NUMBER_OF_STREET_DETECTORS = N_STREET_DETECTORS
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_CONDITIONAL_TURNPCTS[DLLEXPORT, STDCALL](OUTPUTS)
  USE API_DATA
  USE STREET_LINKS
  IMPLICIT NONE
  TYPE(COND_TURNPCTS) :: OUTPUTS
  INTEGER :: IL
!----------------------------------------------------------------------
! Returns: int =  0 - success
!              = -1 - link was not found
! ----------------------------------------------------------------------
  GET_CONDITIONAL_TURNPCTS = -1
  CALL FIND_STREET_LINK(OUTPUTS%USN, OUTPUTS%DSN, IL)
  IF(IL .NE. 0) THEN
    GET_CONDITIONAL_TURNPCTS = 0
    OUTPUTS%LEFTPCT = COND_LEFTPCT(IL, 1:5)
    OUTPUTS%THRUPCT = COND_THRUPCT(IL, 1:5)
    OUTPUTS%RIGHTPCT = COND_RIGHTPCT(IL, 1:5)
    OUTPUTS%LDIAGPCT = COND_LDIAGPCT(IL, 1:5)
    OUTPUTS%RDIAGPCT = COND_RDIAGPCT(IL, 1:5)
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_NUMBER_OF_BUSROUTES[DLLEXPORT, STDCALL]
  USE BUS_ROUTE_DATA
  IMPLICIT NONE
! ----------------------------------------------------------------------
  GET_NUMBER_OF_BUSROUTES = NUMBER_OF_ROUTES
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_BUSROUTES[DLLEXPORT, STDCALL](OUTPUTS)
  USE API_DATA
  USE BUS_ROUTE_DATA
  IMPLICIT NONE
  TYPE(BUSR) :: OUTPUTS(NUMBER_OF_ROUTES)
  INTEGER :: I, NR
! ----------------------------------------------------------------------
  NR = 0
  DO I = 1, NUMBER_OF_ROUTES
    IF(BUSR_HDWY(I) .EQ. 0) CYCLE
    NR = NR + 1
    OUTPUTS(I)%NUMBER = NR
    OUTPUTS(I)%HDWY = BUSR_HDWY(NR)
    OUTPUTS(I)%OFFSET = BUSR_OFFSET(NR)
    OUTPUTS(I)%NNODES = BUSR_NNODES(NR)
    OUTPUTS(I)%ROUTE_NODES = BUSR_ROUTE_NODES(NR, 1:200)
    OUTPUTS(I)%STATIONLIST = BUSR_STATIONLIST(NR, 1:100)
    OUTPUTS(I)%PERSONTRIPS = BUSR_PERSONTRIPS(NR)
    OUTPUTS(I)%TIMER = BUSR_TIMER(NR)
    OUTPUTS(I)%TRAVELTIME = BUSR_TRAVELTIME(NR)
    OUTPUTS(I)%TRIPS = BUSR_TRIPS(NR)
  ENDDO
  GET_BUSROUTES = 0
  END

! ==================================================================================================
  INTEGER FUNCTION GET_BUSSTATIONS[DLLEXPORT, STDCALL](OUTPUTS)
  USE API_DATA
  USE BUS_STATION_DATA
  USE STREET_LINKS
  IMPLICIT NONE
  TYPE(BUSSTATION_DATA) :: OUTPUTS(NUMBER_OF_BUSSTATIONS)
  INTEGER :: NS, IL
! ----------------------------------------------------------------------
  DO NS = 1, NUMBER_OF_BUSSTATIONS
    IL = BUS_STATION_LIST(NS)%LINK
    IF(IL .EQ. 0) EXIT
    OUTPUTS(NS)%STATION_NUMBER = BUS_STATION_LIST(NS)%STATION_NUMBER
    OUTPUTS(NS)%BLOCK_CODE = BUS_STATION_LIST(NS)%BLOCK_CODE
    OUTPUTS(NS)%USN = SUSN(IL)
    OUTPUTS(NS)%DSN = SDSN(IL)
    OUTPUTS(NS)%LOCATION = BUS_STATION_LIST(NS)%LOCATION
    OUTPUTS(NS)%CAPACITY = BUS_STATION_LIST(NS)%CAPACITY
    OUTPUTS(NS)%TYPE_CODE = BUS_STATION_LIST(NS)%TYPE_CODE
    OUTPUTS(NS)%DWELL = BUS_STATION_LIST(NS)%DWELL
    OUTPUTS(NS)%BYPASS_PCT = BUS_STATION_LIST(NS)%BYPASS_PCT * 100
    OUTPUTS(NS)%NEXT_STATION = BUS_STATION_LIST(NS)%NEXT_STATION
    OUTPUTS(NS)%POCKET_LANE = BUS_STATION_LIST(NS)%POCKET_LANE
    OUTPUTS(NS)%FRONT = BUS_STATION_LIST(NS)%FRONT
    OUTPUTS(NS)%COUNT = BUS_STATION_LIST(NS)%COUNT
    OUTPUTS(NS)%DWELL_TIME = BUS_STATION_LIST(NS)%DWELL_TIME
    OUTPUTS(NS)%EMPTY_TIME = BUS_STATION_LIST(NS)%EMPTY_TIME
    OUTPUTS(NS)%OVERFLOW_TIME = BUS_STATION_LIST(NS)%OVERFLOW_TIME
  ENDDO
  GET_BUSSTATIONS = 0
  END

 !==================================================================================================
  INTEGER FUNCTION GET_FREEWAY_DETECTOR_DATA[DLLEXPORT, STDCALL](OUTPUTS)
  USE API_DATA
  USE FREEWAY_DETECTORS
  USE FREEWAY_LINKS
  IMPLICIT NONE
  TYPE(API_DETECTOR_DATA) :: OUTPUTS(N_FREEWAY_DETECTORS)
  INTEGER :: IDET, IL
 !----------------------------------------------------------------------
  DO IDET = 1, N_FREEWAY_DETECTORS
    IL = FDETECTOR(IDET)%LINK
    OUTPUTS(IDET)%USN = FUSN(IL)
    OUTPUTS(IDET)%DSN = FDSN(IL)
    OUTPUTS(IDET)%STATION_ID = FDETECTOR(IDET)%STATION_ID
    OUTPUTS(IDET)%LOCATION = FDETECTOR(IDET)%LOCATION
    OUTPUTS(IDET)%DETECTOR_LANES(1) = FDETECTOR(IDET)%LANE1
    OUTPUTS(IDET)%ZONE_LENGTH = FDETECTOR(IDET)%ZONE_LENGTH
    OUTPUTS(IDET)%DELAY_TIME = FDETECTOR(IDET)%DELAY_TIME
    OUTPUTS(IDET)%CARRYOVER_TIME = FDETECTOR(IDET)%CARRYOVER_TIME
    OUTPUTS(IDET)%NEXT_DET = FDETECTOR(IDET)%NEXT_DET
    OUTPUTS(IDET)%TYPE_CODE = FDETECTOR(IDET)%TYPE_CODE
    OUTPUTS(IDET)%OPERATION_CODE = FDETECTOR(IDET)%OPERATION_CODE
    OUTPUTS(IDET)%COUNT = FDETECTOR(IDET)%COUNT 
    OUTPUTS(IDET)%CURRENT_STATE = FDETECTOR(IDET)%CURRENT_STATE
    OUTPUTS(IDET)%ON_TIME = FDETECTOR(IDET)%ON_TIME
    OUTPUTS(IDET)%SPEED_TOTAL = FDETECTOR(IDET)%SPEED_TOTAL
    OUTPUTS(IDET)%HDWY_COUNT = FDETECTOR(IDET)%HDWY_COUNT
    OUTPUTS(IDET)%HDWY_TOTAL = FDETECTOR(IDET)%HDWY_TOTAL
  ENDDO
  GET_FREEWAY_DETECTOR_DATA = 0
  END

 !==================================================================================================
  INTEGER FUNCTION GET_STREET_DETECTOR_OUTPUTS[DLLEXPORT, STDCALL](OUTPUTS)
  USE API_DATA
  USE STREET_DETECTORS
  IMPLICIT NONE
  TYPE(DETECTOR_OUTPUTS) :: OUTPUTS(N_STREET_DETECTORS)
  INTEGER :: IDET
 !----------------------------------------------------------------------
  DO IDET = 1, N_STREET_DETECTORS
    OUTPUTS(IDET)%COUNT = SDETECTOR(IDET)%COUNT 
    OUTPUTS(IDET)%CURRENT_STATE = SDETECTOR(IDET)%CURRENT_STATE
    OUTPUTS(IDET)%PREVIOUS_STATE = SDETECTOR(IDET)%PREVIOUS_STATE
    OUTPUTS(IDET)%ON_TIME = SDETECTOR(IDET)%ON_TIME
    OUTPUTS(IDET)%SPEED_TOTAL = SDETECTOR(IDET)%SPEED_TOTAL
    OUTPUTS(IDET)%LENGTH_TOTAL = SDETECTOR(IDET)%LENGTH_TOTAL
    OUTPUTS(IDET)%HDWY_COUNT = SDETECTOR(IDET)%HDWY_COUNT
    OUTPUTS(IDET)%HDWY_TOTAL = SDETECTOR(IDET)%HDWY_TOTAL
    OUTPUTS(IDET)%LAST_ACTUATION_TIME = SDETECTOR(IDET)%LAST_ACTUATION_TIME
    OUTPUTS(IDET)%LAST_SPEED = SDETECTOR(IDET)%LAST_SPEED
    OUTPUTS(IDET)%LAST_LENGTH = SDETECTOR(IDET)%LAST_LENGTH
    OUTPUTS(IDET)%CURRENT_STATE_TEMP = SDETECTOR(IDET)%CURRENT_STATE_TEMP
    OUTPUTS(IDET)%PREVIOUS_STATE_TEMP = SDETECTOR(IDET)%PREVIOUS_STATE_TEMP
  ENDDO
  GET_STREET_DETECTOR_OUTPUTS = 0
  END
  
 !==================================================================================================
  REAL FUNCTION GET_STREET_TRAVEL_TIME[DLLEXPORT, STDCALL](USN, DSN)
  USE STREET_LINKS
  IMPLICIT NONE
  INTEGER :: USN, DSN
  INTEGER :: TEMP, IARRAY(2), FLAG
  REAL :: R1
 !----------------------------------------------------------------------
 !This function gets average travel time by calling a Street Link MOE function 
 !----------------------------------------------------------------------
  INTERFACE
  INTEGER RECURSIVE FUNCTION GETSTREETLINKMOEDATA &
  [DLLEXPORT, ALIAS:'GetStreetLinkMoeData'] (MOETYPE, IARRAY, FLAG, RMOE)
    CHARACTER*(*) MOETYPE
    INTEGER     IARRAY  [REFERENCE]
    INTEGER     FLAG    [VALUE]
    REAL        RMOE    [REFERENCE]
    DIMENSION IARRAY(*)
  END FUNCTION
  END INTERFACE

  IARRAY(1) = USN
  IARRAY(2) = DSN
  FLAG = 1
  TEMP = GETSTREETLINKMOEDATA('TravelTimePerVehicleX', IARRAY, FLAG, R1)
  GET_STREET_TRAVEL_TIME = R1
  END
  
 !==================================================================================================
  INTEGER FUNCTION UPDATE_STREET_DETECTORS[DLLEXPORT, STDCALL](OUTPUTS)
  USE API_DATA
  USE STREET_DETECTORS
  IMPLICIT NONE
  TYPE(DETECTOR_OUTPUTS) :: OUTPUTS(N_STREET_DETECTORS)
  INTEGER :: IDET
 !----------------------------------------------------------------------
  DO IDET = 1, N_STREET_DETECTORS
    SDETECTOR(IDET)%COUNT = OUTPUTS(IDET)%COUNT
    SDETECTOR(IDET)%CURRENT_STATE = OUTPUTS(IDET)%CURRENT_STATE
    SDETECTOR(IDET)%PREVIOUS_STATE = OUTPUTS(IDET)%PREVIOUS_STATE
    SDETECTOR(IDET)%ON_TIME = OUTPUTS(IDET)%ON_TIME
    SDETECTOR(IDET)%SPEED_TOTAL = OUTPUTS(IDET)%SPEED_TOTAL
    SDETECTOR(IDET)%LENGTH_TOTAL = OUTPUTS(IDET)%LENGTH_TOTAL
    SDETECTOR(IDET)%HDWY_COUNT = OUTPUTS(IDET)%HDWY_COUNT
    SDETECTOR(IDET)%HDWY_TOTAL = OUTPUTS(IDET)%HDWY_TOTAL
    SDETECTOR(IDET)%LAST_ACTUATION_TIME = OUTPUTS(IDET)%LAST_ACTUATION_TIME
    SDETECTOR(IDET)%LAST_SPEED = OUTPUTS(IDET)%LAST_SPEED
    SDETECTOR(IDET)%LAST_LENGTH = OUTPUTS(IDET)%LAST_LENGTH
  ENDDO
  UPDATE_STREET_DETECTORS = 0
  END

! ==================================================================================================
  INTEGER FUNCTION GET_PHASES[DLLEXPORT, STDCALL](NODE, PHASES)
! ----------------------------------------------------------------------
!     This function allows an external entity to determine the
!     currently active phases of an actuated control signal.
! ----------------------------------------------------------------------
  USE STREET_NODES
  USE ACTUATED_CONTROLLERS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NODE
  INTEGER, INTENT(OUT) :: PHASES(2)
  INTEGER :: IACT
!----------------------------------------------------------------------
! Returns: int =  0 - success
!              = -1 - controller was not found
! ----------------------------------------------------------------------
  PHASES = 0
  IACT = NACT(NODE)
  IF(IACT .EQ. 0) THEN
    GET_PHASES = -1
    WRITE(MSGTEXT,'(A,I4)') 'GET_PHASES: ACTUATED CONTROLLER NOT FOUND AT NODE ', NODE
    CALL SENDTEXTMSG(M_INFO)
  ELSE
    GET_PHASES = 0
    PHASES = AC_SIGNALS(IACT)%CURRENT_PHASES
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION SET_PHASES[DLLEXPORT, STDCALL](NODE, PHASES)
! ----------------------------------------------------------------------
!     This function allows an external entity to set the
!     currently active phases of an actuated control signal.
! ----------------------------------------------------------------------
  USE STREET_NODES
  USE ACTUATED_CONTROLLERS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NODE, PHASES(2)
  INTEGER :: IACT
!----------------------------------------------------------------------
! Returns: int =  0 - success
!              = -1 - controller was not found
! ----------------------------------------------------------------------
  IACT = NACT(NODE)
  IF(IACT .EQ. 0) THEN
    SET_PHASES = -1
    WRITE(MSGTEXT,'(A,I4)') 'SET_PHASES: ACTUATED CONTROLLER NOT FOUND AT NODE ', NODE
    CALL SENDTEXTMSG(M_INFO)
  ELSE
    SET_PHASES = 0
    AC_SIGNALS(IACT)%CURRENT_PHASES = PHASES
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION GET_NUMBER_OF_VEHICLE_TYPES[DLLEXPORT, STDCALL]()
  USE VEHICLE_TYPES
  IMPLICIT NONE
! ----------------------------------------------------------------------
  GET_NUMBER_OF_VEHICLE_TYPES = NTYPES
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_NUMBER_OF_FREEWAYLINKS[DLLEXPORT, STDCALL]()
  USE FREEWAY_LINKS
  IMPLICIT NONE
! ----------------------------------------------------------------------
  GET_NUMBER_OF_FREEWAYLINKS = N_FREEWAY_LINKS
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_FREEWAYLINKS[DLLEXPORT, STDCALL](LINK_DATA)
  USE FREEWAY_LINKS
  USE API_DATA
  USE NODE_TABLE
  IMPLICIT NONE
  TYPE(FREEWAY_LINK) :: LINK_DATA(N_FREEWAY_LINKS)
  INTEGER :: IL
  REAL :: DSN_TO_SEG_END
! ----------------------------------------------------------------------
  DO IL = 1, N_FREEWAY_LINKS
    IF(FTHRU_LINK(IL) .NE. 0) THEN
      LINK_DATA(IL)%THRUNODE = FDSN(FTHRU_LINK(IL))
    ELSE
      LINK_DATA(IL)%THRUNODE = 0
    ENDIF
    IF(OFFRAMP_LINK(IL) .NE. 0) THEN
      LINK_DATA(IL)%EXITNODE = FDSN(OFFRAMP_LINK(IL))
    ELSE
      LINK_DATA(IL)%EXITNODE = 0
    ENDIF
    DSN_TO_SEG_END = USN_TO_SEG_END(IL) - FLENGTH(IL)
    LINK_DATA(IL)%ID                        = IL
    LINK_DATA(IL)%USN                       = FUSN(IL)
    LINK_DATA(IL)%DSN                       = FDSN(IL)
    LINK_DATA(IL)%USN_TYPE                  = NODE_TYPE(FUSN(IL)) 
    LINK_DATA(IL)%DSN_TYPE                  = NODE_TYPE(FDSN(IL))
    LINK_DATA(IL)%LENGTH                    = FLENGTH(IL)
    LINK_DATA(IL)%FULLLANES                 = FNUMLANES(IL)
    LINK_DATA(IL)%AUXLANEID                 = AUX_LANE_ID(IL, 1:N_AUXLANES)
    LINK_DATA(IL)%AUXLANECODE               = AUX_LANE_CODE(IL, 1:N_AUXLANES)
    LINK_DATA(IL)%AUXLANELENGTH             = AUX_LANE_LENGTH(IL, 1:N_AUXLANES)
    LINK_DATA(IL)%FREEFLOWSPEED             = NINT(FFREEFLOWSPEED(IL) * FEET2MILES)
    LINK_DATA(IL)%PAVEMENT                  = PAVEMENT(IL)
    LINK_DATA(IL)%THRUPCT                   = FTHRU_PERCENT(IL)
    LINK_DATA(IL)%MAINLINE_SENDING_LANE     = MAINLINE_SENDING_LANE(IL)
    LINK_DATA(IL)%MAINLINE_RECEIVING_LANE   = MAINLINE_RECEIVING_LANE(IL)
    LINK_DATA(IL)%OFFRAMP_SENDING_LANE      = OFFRAMP_SENDING_LANE(IL)
    LINK_DATA(IL)%OFFRAMP_RECEIVING_LANE    = OFFRAMP_RECEIVING_LANE(IL)
    LINK_DATA(IL)%LINKTYPE                  = ABS(LINKTYPE(IL))
    LINK_DATA(IL)%ADDDROP_CODE              = ADDDROP_CODE(IL, 1:3)
    LINK_DATA(IL)%ADDDROP_LANE              = ADDDROP_LANE(IL, 1:3)       
    LINK_DATA(IL)%ADDDROP_DIST              = ADDDROP_DIST(IL, 1:3)       
    LINK_DATA(IL)%ADDDROP_WARN              = ADDDROP_WARN(IL, 1:3)
    IF(OFFRAMP_WARN_DISTANCE(IL) .NE. 0.) THEN
      LINK_DATA(IL)%OFFRAMP_WARN_DISTANCE     = OFFRAMP_WARN_DISTANCE(IL) - DSN_TO_SEG_END
    ELSE
      LINK_DATA(IL)%OFFRAMP_WARN_DISTANCE     = 0.
    ENDIF
    IF(HOV_OFFRAMP_WARN_DISTANCE(IL) .NE. 0.) THEN
      LINK_DATA(IL)%HOV_OFFRAMP_WARN_DISTANCE = HOV_OFFRAMP_WARN_DISTANCE(IL) - DSN_TO_SEG_END
    ELSE
      LINK_DATA(IL)%HOV_OFFRAMP_WARN_DISTANCE = 0.
    ENDIF
    LINK_DATA(IL)%ANTICIP_WARNING_DISTANCE  = ANTICIP_WARNING_DISTANCE(IL)
    LINK_DATA(IL)%ANTICIP_WARNING_SPEED     = NINT(ANTICIP_WARNING_SPEED(IL) * FEET2MILES)
    LINK_DATA(IL)%NHOV_LANES                = NHOV_LANES(IL)
    LINK_DATA(IL)%HOV_BEGIN                 = HOV_BEGIN(IL)
    LINK_DATA(IL)%HOV_END                   = HOV_END(IL)
    LINK_DATA(IL)%HOV_CODE                  = HOV_CODE(IL)
    LINK_DATA(IL)%HOV_SIDE                  = HOV_SIDE(IL)
    LINK_DATA(IL)%HOV_TYPE                  = HOV_TYPE(IL)
    LINK_DATA(IL)%HOV_WARN                  = HOV_WARN(IL)
    LINK_DATA(IL)%HOV_PCT                   = INT(HOV_PCT(IL) * 100)
    LINK_DATA(IL)%CFMULT                    = FCFMULT(IL)
    LINK_DATA(IL)%FIRST_DETECTOR            = FFIRST_DETECTOR(IL)
    LINK_DATA(IL)%GRADE                     = FGRADE(IL)
    LINK_DATA(IL)%TILT                      = TILT(IL)
    LINK_DATA(IL)%CURVE                     = CURVE(IL)
    LINK_DATA(IL)%SHOULDER_WIDTH            = FSHOULDER_WIDTH(IL)
    LINK_DATA(IL)%LANE_WIDTH                = FLANE_WIDTH(IL, 1:N_FREEWAY_LANES)
    LINK_DATA(IL)%BARRIER                   = BARRIER(IL, 1:2)
    LINK_DATA(IL)%DATASTATION_ID            = DATASTATION_ID(IL)
    LINK_DATA(IL)%DATASTATION_LOCATION      = DATASTATION_LOCATION(IL)
    LINK_DATA(IL)%TRUCK_CODE                = TRUCK_CODE(IL)
    LINK_DATA(IL)%TRUCK_DIR                 = TRUCK_DIR(IL)
    LINK_DATA(IL)%TRUCK_LANE                = TRUCK_LANE(IL)
    LINK_DATA(IL)%ETL_WARN                  = ETL_WARN(IL)
    LINK_DATA(IL)%EXCLUDE_TYPE              = FXCLUDE_TYPE(IL, 1:N_FREEWAY_LANES, 1:NVTYPES)
    LINK_DATA(IL)%MULTIPLIER_EXIT           = MULTIPLIER_EXIT(IL, 1:NVTYPES)
    LINK_DATA(IL)%STARTUP_TIME              = FSTARTUP_TIME(IL)
    LINK_DATA(IL)%MERGE_DIVERGE_CODE        = 0
    IF(RAMP_MERGE_LINK(IL)) LINK_DATA(IL)%MERGE_DIVERGE_CODE = 1
    IF(DIVERGE_LINK(IL)) LINK_DATA(IL)%MERGE_DIVERGE_CODE = 2
  ENDDO
  GET_FREEWAYLINKS = 0
  END

! ==================================================================================================
  INTEGER FUNCTION GET_NUMBER_OF_STREETLINKS[DLLEXPORT, STDCALL]()
  USE STREET_LINKS
  IMPLICIT NONE
! ----------------------------------------------------------------------
  GET_NUMBER_OF_STREETLINKS = N_STREET_LINKS
  END 

!! ==================================================================================================
!  INTEGER FUNCTION GET_RAMPMETER_SIGNAL_STATES[DLLEXPORT, STDCALL](SIGNAL_DATA)
!  USE FREEWAY_LINKS
!  USE RAMP_METERS
!  USE API_DATA
!  IMPLICIT NONE
!  TYPE(SIGNAL_STATE) :: SIGNAL_DATA(NUMBER_OF_RAMPMETERS)
!  INTEGER :: IL, IM
!! ----------------------------------------------------------------------
!  DO IM = 1, NUMBER_OF_RAMPMETERS
!    IL = RAMPMETERS(IM)%LINK
!    SIGNAL_DATA(IL)%ID               = IL
!    SIGNAL_DATA(IL)%USN              = FUSN(IL)
!    SIGNAL_DATA(IL)%DSN              = FDSN(IL)
!    IF(RAMPMETERS(IM)%STATE .EQ. MS_INACTIVE) THEN
!      SIGNAL_DATA(IL)%SIGNAL_CODE = 4
!    ELSEIF(RAMPMETERS(IM)%STATE .EQ. MS_RED) THEN
!      SIGNAL_DATA(IL)%SIGNAL_CODE = 0
!    ELSEIF(RAMPMETERS(IM)%STATE .EQ. MS_GREEN) THEN
!      SIGNAL_DATA(IL)%SIGNAL_CODE = 2
!    ENDIF
!  ENDDO
!  GET_RAMPMETER_SIGNAL_STATES = 0
!  END
  
! ==================================================================================================
  INTEGER FUNCTION DEFINE_INTERSECTION_DATA[DLLEXPORT, STDCALL](USN, DSN, INTERSECTION_DATA)
  USE STREET_LINKS
  USE API_DATA
  IMPLICIT NONE
  INTEGER :: USN, DSN
  TYPE(INTERSECTION_DIMENSIONS) :: INTERSECTION_DATA
  INTEGER :: IL, I, J
  REAL :: TEMP1(N_STREET_LANES, N_STREET_LANES)
  REAL :: TEMP2(N_STREET_LANES, N_STREET_LANES)
  REAL :: TEMP3(N_STREET_LANES, N_STREET_LANES)
  REAL :: TEMP4(N_STREET_LANES, N_STREET_LANES)
  REAL :: TEMP5(N_STREET_LANES, N_STREET_LANES)
!----------------------------------------------------------------------
! Returns: int =  0 - success
!              = -1 - link was not found
! ----------------------------------------------------------------------
  CALL FIND_STREET_LINK(USN, DSN, IL)
  IF(IL .NE. 0) THEN
    WRITE82(IL) = .TRUE.
    WRITE83(IL) = .TRUE.
    UP_INT_WIDTH(IL) = INTERSECTION_DATA%UP_INT_WIDTH
    LANE_CENTER(IL, 1:N_STREET_LANES) = INTERSECTION_DATA%LANE_CENTER
    !Swap indices for C++ to Fortran exchange
    DO I = 1, N_STREET_LANES
      DO J = 1, N_STREET_LANES
        TEMP1(I, J) = INTERSECTION_DATA%LT_ARC_LENGTH(J, I)
        TEMP2(I, J) = INTERSECTION_DATA%THRU_ARC_LENGTH(J, I)
        TEMP3(I, J) = INTERSECTION_DATA%RT_ARC_LENGTH(J, I)
        TEMP4(I, J) = INTERSECTION_DATA%LD_ARC_LENGTH(J, I)
        TEMP5(I, J) = INTERSECTION_DATA%RD_ARC_LENGTH(J, I)
      ENDDO
    ENDDO
    LT_ARC_LENGTH(IL, 1:N_STREET_LANES, 1:N_STREET_LANES) = TEMP1
    THRU_ARC_LENGTH(IL, 1:N_STREET_LANES, 1:N_STREET_LANES) = TEMP2
    RT_ARC_LENGTH(IL, 1:N_STREET_LANES, 1:N_STREET_LANES) = TEMP3
    LD_ARC_LENGTH(IL, 1:N_STREET_LANES, 1:N_STREET_LANES) = TEMP4
    RD_ARC_LENGTH(IL, 1:N_STREET_LANES, 1:N_STREET_LANES) = TEMP5
    DEFINE_INTERSECTION_DATA = 0
  ELSE
    DEFINE_INTERSECTION_DATA = -1
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_INTERSECTION_DATA[DLLEXPORT, STDCALL](USN, DSN, INTERSECTION_DATA)
  USE STREET_LINKS
  USE API_DATA
  IMPLICIT NONE
  INTEGER :: USN, DSN
  TYPE(INTERSECTION_DIMENSIONS) :: INTERSECTION_DATA
  INTEGER :: IL, I, J
  REAL :: TEMP1(N_STREET_LANES, N_STREET_LANES)
  REAL :: TEMP2(N_STREET_LANES, N_STREET_LANES)
  REAL :: TEMP3(N_STREET_LANES, N_STREET_LANES)
  REAL :: TEMP4(N_STREET_LANES, N_STREET_LANES)
  REAL :: TEMP5(N_STREET_LANES, N_STREET_LANES)
!----------------------------------------------------------------------
! Returns: int =  0 - success
!              = -1 - link was not found
! ----------------------------------------------------------------------
  CALL FIND_STREET_LINK(USN, DSN, IL)
  IF(IL .NE. 0) THEN
    INTERSECTION_DATA%UP_INT_WIDTH = UP_INT_WIDTH(IL)
    INTERSECTION_DATA%LANE_CENTER = LANE_CENTER(IL, 1:N_STREET_LANES)
    !Swap indices for C++ to Fortran exchange
    DO I = 1, N_STREET_LANES
      DO J = 1, N_STREET_LANES
        TEMP1(I, J) = LT_ARC_LENGTH(IL, J, I)
        TEMP2(I, J) = THRU_ARC_LENGTH(IL, J, I)
        TEMP3(I, J) = RT_ARC_LENGTH(IL, J, I)
        TEMP4(I, J) = LD_ARC_LENGTH(IL, J, I)
        TEMP5(I, J) = RD_ARC_LENGTH(IL, J, I)
      ENDDO
    ENDDO
    INTERSECTION_DATA%LT_ARC_LENGTH = TEMP1
    INTERSECTION_DATA%THRU_ARC_LENGTH = TEMP2
    INTERSECTION_DATA%RT_ARC_LENGTH = TEMP3
    INTERSECTION_DATA%LD_ARC_LENGTH = TEMP4
    INTERSECTION_DATA%RD_ARC_LENGTH = TEMP5
    GET_INTERSECTION_DATA = 0
  ELSE
    GET_INTERSECTION_DATA = -1
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_STREETLINKS[DLLEXPORT, STDCALL](LINK_DATA)
  USE STREET_LINKS
  USE API_DATA
  USE GLOBAL_DATA
  USE NODE_TABLE
  IMPLICIT NONE
  TYPE(STREET_LINK) :: LINK_DATA(N_STREET_LINKS)
  INTEGER :: IL
! ----------------------------------------------------------------------
  DO IL = 1, N_STREET_LINKS
    LINK_DATA(IL)%ID       = IL
    LINK_DATA(IL)%USN      = SUSN(IL)
    LINK_DATA(IL)%DSN      = SDSN(IL)
    LINK_DATA(IL)%USN_TYPE = NODE_TYPE(SUSN(IL)) 
    LINK_DATA(IL)%DSN_TYPE = NODE_TYPE(SDSN(IL))
    IF(STHRU_LINK(IL) .NE. 0) THEN
      LINK_DATA(IL)%THRUNODE = SDSN(STHRU_LINK(IL))
    ELSE
      LINK_DATA(IL)%THRUNODE = 0
    ENDIF
    IF(LEFT_LINK(IL) .NE. 0) THEN
      LINK_DATA(IL)%LEFTNODE = SDSN(LEFT_LINK(IL))
    ELSE
      LINK_DATA(IL)%LEFTNODE = 0
    ENDIF
    IF(RIGHT_LINK(IL) .NE. 0) THEN
      LINK_DATA(IL)%RIGHTNODE = SDSN(RIGHT_LINK(IL))
    ELSE
      LINK_DATA(IL)%RIGHTNODE = 0
    ENDIF
    IF(LEFT_DIAG_LINK(IL) .NE. 0) THEN
      LINK_DATA(IL)%LDIAGNODE = SDSN(LEFT_DIAG_LINK(IL))
    ELSE
      LINK_DATA(IL)%LDIAGNODE = 0
    ENDIF
    IF(RIGHT_DIAG_LINK(IL) .NE. 0) THEN
      LINK_DATA(IL)%RDIAGNODE = SDSN(RIGHT_DIAG_LINK(IL))
    ELSE
      LINK_DATA(IL)%RDIAGNODE = 0
    ENDIF
    IF(OPPOSE_LINK(IL) .NE. 0) THEN
      LINK_DATA(IL)%OPPOSENODE = SUSN(OPPOSE_LINK(IL))
    ELSE
      LINK_DATA(IL)%OPPOSENODE = 0
    ENDIF
    LINK_DATA(IL)%LENGTH            = SLENGTH(IL)
    LINK_DATA(IL)%FULLLANES         = SNUMLANES(IL)
    LINK_DATA(IL)%LEFTTURNBAYS      = NUMBER_LEFTPOCKETS(IL)
    LINK_DATA(IL)%RIGHTTURNBAYS     = NUMBER_RIGHTPOCKETS(IL)
    LINK_DATA(IL)%FREEFLOWSPEED     = NINT(SFREEFLOWSPEED(IL) * FEET2MILES)
    LINK_DATA(IL)%CHANNELIZATION    = CHANNELIZATION(IL, 1:N_STREET_LANES)
    LINK_DATA(IL)%LANE_LENGTH       = LANE_LENGTH(IL, 1:N_STREET_LANES)
    LINK_DATA(IL)%LEFTPCT           = LEFT_PERCENT(IL)
    LINK_DATA(IL)%THRUPCT           = STHRU_PERCENT(IL)
    LINK_DATA(IL)%RIGHTPCT          = RIGHT_PERCENT(IL)
    LINK_DATA(IL)%LDIAGPCT          = LDIAG_PERCENT(IL)
    LINK_DATA(IL)%RDIAGPCT          = RDIAG_PERCENT(IL)
    LINK_DATA(IL)%GRADE             = SGRADE(IL)
    LINK_DATA(IL)%DISTRIBUTION_CODE = LINKTYPE_CODE(IL)
    LINK_DATA(IL)%STARTUP_DELAY     = SSTARTUP_TIME(IL)
    LINK_DATA(IL)%DISCHARGE_HDWY    = QDISCHARGE_HDWY(IL)
    IF(RTOR(IL)) THEN
      LINK_DATA(IL)%RTOR            = 0
    ELSE                            
      LINK_DATA(IL)%RTOR            = 1
    ENDIF                           
    LINK_DATA(IL)%PED_CODE          = PED_CODE(IL)
    LINK_DATA(IL)%LANE1             = SALIGNMENT_LANE(IL)
    LINK_DATA(IL)%LANE2             = STHRU_ALIGNMENT_LANE(IL)
    LINK_DATA(IL)%CFMULT            = SCFMULT(IL)
    LINK_DATA(IL)%SIGHT_DIST        = SIGHT_DIST(IL)
    LINK_DATA(IL)%FIRST_DETECTOR    = SFIRST_DETECTOR(IL)
    LINK_DATA(IL)%SHOULDER_WIDTH    = SSHOULDER_WIDTH(IL)
    LINK_DATA(IL)%LANE_WIDTH        = SLANE_WIDTH(IL, 1:N_STREET_LANES)
    LINK_DATA(IL)%STE_FREQ          = STE_FREQ(IL)
    LINK_DATA(IL)%STE_DURATION      = STE_DURATION(IL)
    LINK_DATA(IL)%SIGNAL_RANGE      = SIGNAL_RANGE(IL)
    LINK_DATA(IL)%CENTROID          = CENTROID(IL)
    LINK_DATA(IL)%CENTROID_LABEL    = CENTROID_LABEL(IL)
    LINK_DATA(IL)%EXCLUDE_TYPE      = SXCLUDE_TYPE(IL, 1:N_STREET_LANES, 1:NVTYPES)
    LINK_DATA(IL)%MULTIPLIER_LEFT   = MULTIPLIER_LEFT(IL, 1:NVTYPES)
    LINK_DATA(IL)%MULTIPLIER_THRU   = MULTIPLIER_THRU(IL, 1:NVTYPES)
    LINK_DATA(IL)%MULTIPLIER_RIGHT  = MULTIPLIER_RIGHT(IL, 1:NVTYPES)
    LINK_DATA(IL)%MULTIPLIER_LDIAG  = MULTIPLIER_LDIAG(IL, 1:NVTYPES)
    LINK_DATA(IL)%MULTIPLIER_RDIAG  = MULTIPLIER_RDIAG(IL, 1:NVTYPES)
    LINK_DATA(IL)%LT_SPEED          = LT_SPEED(IL) * FEET2MILES
    LINK_DATA(IL)%RT_SPEED          = RT_SPEED(IL) * FEET2MILES
    LINK_DATA(IL)%LT_LIMITED_SPEED_DIST          = LT_LIMITED_SPEED_DIST(IL)
    LINK_DATA(IL)%RT_LIMITED_SPEED_DIST          = RT_LIMITED_SPEED_DIST(IL)
  ENDDO
  GET_STREETLINKS = 0
  END

! ==================================================================================================
  INTEGER FUNCTION GET_FREEWAY_ANIMATION_DATA[DLLEXPORT, STDCALL](OUT_DATA)
  USE FREEWAY_LINKS
  USE RAMP_METERS
  USE API_DATA
  IMPLICIT NONE
  TYPE(ANIMATION_DATA) :: OUT_DATA(N_FREEWAY_LINKS)
  INTEGER :: IL, NMETER
! ----------------------------------------------------------------------
  DO IL = 1, N_FREEWAY_LINKS
    OUT_DATA(IL)%ID                = IL
    OUT_DATA(IL)%USN               = FUSN(IL)
    OUT_DATA(IL)%DSN               = FDSN(IL)
    OUT_DATA(IL)%FIRST_ON_SHOULDER = FFIRST_ON_SHOULDER(IL)
    IF(RAMPMETER(IL) .NE. 0) THEN
      NMETER = RAMPMETER(IL)
      IF(RAMPMETERS(NMETER)%STATE .EQ. MS_INACTIVE) THEN
        OUT_DATA(IL)%SIGNAL_CODE = 4
      ELSEIF(RAMPMETERS(NMETER)%STATE .EQ. MS_RED) THEN
        OUT_DATA(IL)%SIGNAL_CODE = 0
      ELSEIF(RAMPMETERS(NMETER)%STATE .EQ. MS_GREEN) THEN
        OUT_DATA(IL)%SIGNAL_CODE = 2
      ENDIF
    ENDIF
  ENDDO
  GET_FREEWAY_ANIMATION_DATA = 0
  END

! ==================================================================================================
  INTEGER FUNCTION GET_STREET_ANIMATION_DATA[DLLEXPORT, STDCALL](OUT_DATA)
  USE STREET_LINKS
  USE API_DATA
  IMPLICIT NONE
  TYPE(ANIMATION_DATA) :: OUT_DATA(N_STREET_LINKS)
  INTEGER :: IL, ILANE
! ----------------------------------------------------------------------
  DO IL = 1, N_STREET_LINKS
    OUT_DATA(IL)%ID                = IL
    OUT_DATA(IL)%USN               = SUSN(IL)
    OUT_DATA(IL)%DSN               = SDSN(IL)
    OUT_DATA(IL)%SIGNALIZED         = 0
    IF(SIGNALIZED(IL)) OUT_DATA(IL)%SIGNALIZED         = 1
    DO ILANE = 1, N_STREET_LANES
      OUT_DATA(IL)%FIRST_VEHICLE(ILANE) = FIRST_VEHICLE(IL, ILANE)
    ENDDO
    OUT_DATA(IL)%FIRST_ON_SHOULDER = SFIRST_ON_SHOULDER(IL)
    OUT_DATA(IL)%SIGNAL_CODE        = SIGNAL_CODE(IL)
    OUT_DATA(IL)%PREV_SIGNAL_CODE   = PREV_SIGNAL_CODE(IL)
    OUT_DATA(IL)%SIGNAL_LEFT        = 0
    OUT_DATA(IL)%SIGNAL_THRU        = 0
    OUT_DATA(IL)%SIGNAL_RIGHT       = 0
    OUT_DATA(IL)%SIGNAL_DIAG        = 0
    OUT_DATA(IL)%AMBER_LEFT         = 0
    OUT_DATA(IL)%AMBER_THRU         = 0
    OUT_DATA(IL)%AMBER_RIGHT        = 0
    OUT_DATA(IL)%AMBER_DIAG         = 0
    IF(SIGNAL_LEFT(IL))  OUT_DATA(IL)%SIGNAL_LEFT  = 1
    IF(SIGNAL_THRU(IL))  OUT_DATA(IL)%SIGNAL_THRU  = 1
    IF(SIGNAL_RIGHT(IL)) OUT_DATA(IL)%SIGNAL_RIGHT = 1
    IF(SIGNAL_DIAG(IL))  OUT_DATA(IL)%SIGNAL_DIAG  = 1
    IF(AMBER_LEFT(IL))   OUT_DATA(IL)%AMBER_LEFT   = 1
    IF(AMBER_THRU(IL))   OUT_DATA(IL)%AMBER_THRU   = 1
    IF(AMBER_RIGHT(IL))  OUT_DATA(IL)%AMBER_RIGHT  = 1
    IF(AMBER_DIAG(IL))   OUT_DATA(IL)%AMBER_DIAG   = 1
  ENDDO
  GET_STREET_ANIMATION_DATA = 0
  END

! ==================================================================================================
  INTEGER FUNCTION GET_NUMBER_OF_ENTRYNODES[DLLEXPORT, STDCALL]
  USE API_DATA
  USE FLOWDATA_MOD
! ----------------------------------------------------------------------
  GET_NUMBER_OF_ENTRYNODES = NUMBER_OF_ENTRYNODES
  END

! ==================================================================================================
  INTEGER FUNCTION GET_NODE_COORDINATES[DLLEXPORT, STDCALL](OUTPUTS)
  USE API_DATA
  USE NODE_TABLE
  USE GLOBAL_DATA
  IMPLICIT NONE
  TYPE(NODE_LOCATION_DATA) :: OUTPUTS(MAX_NODE_NUMBER)
  INTEGER :: I
! ----------------------------------------------------------------------
  DO I = 1, MAX_NODE_NUMBER
    OUTPUTS(I)%X = X195(I) 
    OUTPUTS(I)%Y = Y195(I)
    OUTPUTS(I)%LATITUDE = NODE_LAT(I)
    OUTPUTS(I)%LONGITUDE = NODE_LON(I)
    OUTPUTS(I)%ELEVATION = NODE_ELEV(I) 
    OUTPUTS(I)%IS_USED = IS_USED(I)
    OUTPUTS(I)%IS_DEFINED = IS_DEFINED(I)
  ENDDO
  GET_NODE_COORDINATES = 0
  END

! ==================================================================================================
  INTEGER FUNCTION GET_ENTRYNODES[DLLEXPORT, STDCALL](TYPED, ERLA, MNSEP, ENTRY_DATA)
  USE ENTRYNODE_DATA
  USE API_DATA
  USE FLOWDATA_MOD
  IMPLICIT NONE
  INTEGER, INTENT(OUT) :: TYPED       [REFERENCE]
  INTEGER, INTENT(OUT) ::  ERLA       [REFERENCE]
  REAL, INTENT(OUT) :: MNSEP          [REFERENCE]
  TYPE(ENTRY_NODE_DATA) :: ENTRY_DATA(NUMBER_OF_ENTRYNODES)
  INTEGER :: I, INODE
! ----------------------------------------------------------------------
  INODE = 0
  TYPED = TYPEDIST
  ERLA = ERLANGA
  MNSEP = MINSEP
  DO I = MAX_NODE_NUMBER + 1, MAX_NODE_NUMBER + 1000
    IF(ENTRYNODE_IS_USED(I)) THEN
      INODE = INODE + 1
      ENTRY_DATA(INODE)%NODE_ID = I
      ENTRY_DATA(INODE)%FLOWRATE = FLOWRATE(I) 
      ENTRY_DATA(INODE)%CARPOOL_PCT = CARPOOL_PCT(I)
      ENTRY_DATA(INODE)%HOV_VIOLATORS_PER10000 = INT(HOV_VIOLATOR_PCT(I) * 10000)
      ENTRY_DATA(INODE)%LANE_PCT = INT(100 * LANE_PCT(I, 1:10))
      ENTRY_DATA(INODE)%TRUCK_PCT = TRUCK_PCT(I)
    ENDIF
  ENDDO
  GET_ENTRYNODES = 0
  END

! ==================================================================================================
  INTEGER FUNCTION GET_NUMBER_OF_RAMPMETERS[DLLEXPORT, STDCALL]
  USE RAMP_METERS
  IMPLICIT NONE
! ----------------------------------------------------------------------
  GET_NUMBER_OF_RAMPMETERS = NUMBER_OF_RAMPMETERS
  END

! ==================================================================================================
  INTEGER FUNCTION GET_RAMPMETERS[DLLEXPORT, STDCALL](RAMPMETERDATA)
  USE RAMP_METERS
  USE FREEWAY_LINKS
  IMPLICIT NONE
  TYPE(RM_DATA) :: RAMPMETERDATA(NUMBER_OF_RAMPMETERS)
  INTEGER :: I
! ----------------------------------------------------------------------
  DO I = 1, NUMBER_OF_RAMPMETERS
    RAMPMETERDATA(I)%DSN = RAMPMETERS(I)%DSN
    RAMPMETERDATA(I)%LINK = RAMPMETERS(I)%LINK
    RAMPMETERDATA(I)%CONTROL = RAMPMETERS(I)%CONTROL
    RAMPMETERDATA(I)%ONSET =  RAMPMETERS(I)%ONSET
    RAMPMETERDATA(I)%CAPACITY = RAMPMETERS(I)%CAPACITY
    RAMPMETERDATA(I)%SPEED = RAMPMETERS(I)%SPEED
    RAMPMETERDATA(I)%HEADWAY = RAMPMETERS(I)%HEADWAY
    !RAMPMETERDATA(I)%UPDINT = RAMPMETERS(I)%UPDINT
    RAMPMETERDATA(I)%STATE = RAMPMETERS(I)%STATE
    RAMPMETERDATA(I)%TWO_PERGREEN = RAMPMETERS(I)%TWO_PERGREEN
    GET_RAMPMETERS = 0
  ENDDO
  END

! ==================================================================================================
  INTEGER FUNCTION GET_NUMBER_OF_FTC_SIGNALS[DLLEXPORT, STDCALL]
  USE TIMED_CONTROLLERS
  IMPLICIT NONE
! ----------------------------------------------------------------------
  GET_NUMBER_OF_FTC_SIGNALS = NUMBER_OF_FTCS
  END

! ==================================================================================================
  INTEGER FUNCTION GET_FTC_SIGNALS[DLLEXPORT, STDCALL](SIGNALDATA)
  USE TIMED_CONTROLLERS
  USE STREET_LINKS
  USE SIMPARAMS
  USE NODE_TABLE
  IMPLICIT NONE
  TYPE(FTC_DATA) :: SIGNALDATA(NUMBER_OF_FTCS)
  INTEGER :: I, J
! ----------------------------------------------------------------------
  DO I = 1, NUMBER_OF_FTCS
    SIGNALDATA(I) = FTC_SIGNALS(I)
    DO J = 1, SIGNALDATA(I)%APPROACHES
      IF(NODE_TYPE(SIGNALDATA(I)%APPROACH(J)) .EQ. NT_INTERN) THEN
        SIGNALDATA(I)%APPROACH(J) = SUSN(SIGNALDATA(I)%APPROACH(J))
      ELSE
        SIGNALDATA(I)%APPROACH(J) = SIGNALDATA(I)%APPROACH(J)
      ENDIF
    ENDDO
  ENDDO
  GET_FTC_SIGNALS = 0
  END

! ==================================================================================================
  INTEGER FUNCTION GET_NUMBER_OF_AC_SIGNALS[DLLEXPORT, STDCALL]
  USE ACTUATED_CONTROLLERS
  IMPLICIT NONE
! ----------------------------------------------------------------------
  GET_NUMBER_OF_AC_SIGNALS = NUMBER_OF_ACS
  END

! ==================================================================================================
  INTEGER FUNCTION GET_AC_SIGNALS[DLLEXPORT, STDCALL](SIGNALDATA)
  USE API_DATA
  USE ACTUATED_CONTROLLERS
  USE STREET_LINKS
  USE GLOBAL_DATA
  IMPLICIT NONE
  TYPE(API_ACTUATED_DATA) :: SIGNALDATA(NUMBER_OF_ACS)
  INTEGER :: I, IACT, PHASE, IAP
! ----------------------------------------------------------------------
  DO IACT = 1, NUMBER_OF_ACS
    SIGNALDATA(IACT)%NODE = ABS(AC_SIGNALS(IACT)%NODE(1))
    SIGNALDATA(IACT)%CFAILS = 0
    SIGNALDATA(IACT)%ADJ = 0
    SIGNALDATA(IACT)%ACTUATED_MODE = AC_SIGNALS(IACT)%GUI_ACTUATED_MODE(1:8)
    DO PHASE = 1, 8
      SIGNALDATA(IACT)%LEFTARROW(PHASE, 1:10) = AC_SIGNALS(IACT)%PHASE(PHASE)%LEFTARROW(1:10)
      SIGNALDATA(IACT)%THRUARROW(PHASE, 1:10) = AC_SIGNALS(IACT)%PHASE(PHASE)%THRUARROW(1:10)
      SIGNALDATA(IACT)%RIGHTARROW(PHASE, 1:10) = AC_SIGNALS(IACT)%PHASE(PHASE)%RIGHTARROW(1:10)
      DO IAP = 1, 10
        SIGNALDATA(IACT)%LDIAGARROW(PHASE, IAP) = AC_SIGNALS(IACT)%PHASE(PHASE)%LDIAGARROW(IAP)
        SIGNALDATA(IACT)%RDIAGARROW(PHASE, IAP) = AC_SIGNALS(IACT)%PHASE(PHASE)%RDIAGARROW(IAP)
      ENDDO
    ENDDO
    SIGNALDATA(IACT)%MIN_GREEN_TIME = AC_SIGNALS(IACT)%GUI_MIN_GREEN_TIMES
    SIGNALDATA(IACT)%MAX_GREEN_TIME = AC_SIGNALS(IACT)%GUI_MAX_GREEN_TIMES
    SIGNALDATA(IACT)%DEFAULT_EXTENSION_TIME = AC_SIGNALS(IACT)%GUI_DEFAULT_EXTENSION_TIMES
    SIGNALDATA(IACT)%GAP_TIME = AC_SIGNALS(IACT)%GUI_GAP_TIMES
    SIGNALDATA(IACT)%TIME_BEFORE_REDUCTION = AC_SIGNALS(IACT)%GUI_TIME_BEFORE_REDUCTION
    SIGNALDATA(IACT)%TIME_TO_REDUCE = AC_SIGNALS(IACT)%GUI_TIME_TO_REDUCE
    SIGNALDATA(IACT)%MIN_GAP_TIME = AC_SIGNALS(IACT)%GUI_MIN_GAP_TIMES
    SIGNALDATA(IACT)%YC = AC_SIGNALS(IACT)%GUI_YC
    SIGNALDATA(IACT)%RC = AC_SIGNALS(IACT)%GUI_RC
    SIGNALDATA(IACT)%WALK_TIME = AC_SIGNALS(IACT)%NEW_PED_WALK_TIMES
    SIGNALDATA(IACT)%WALK_CLEARANCE_TIME = AC_SIGNALS(IACT)%NEW_PED_CLEARANCE_TIMES
    SIGNALDATA(IACT)%PED_OMIT = AC_SIGNALS(IACT)%NEW_PED_OMIT

    SIGNALDATA(IACT)%N_DIRECT_APPROACHES = AC_SIGNALS(IACT)%N_DIRECT_APPROACHES
    DO I = 1, SIGNALDATA(IACT)%N_DIRECT_APPROACHES
      SIGNALDATA(IACT)%DIRECT_APPROACH_USN(I) = SUSN(AC_SIGNALS(IACT)%DIRECT_APPROACH(I))
      SIGNALDATA(IACT)%DIRECT_APPROACH_DSN(I) = SDSN(AC_SIGNALS(IACT)%DIRECT_APPROACH(I))
    ENDDO
    SIGNALDATA(IACT)%N_INDIRECT_APPROACHES = AC_SIGNALS(IACT)%N_INDIRECT_APPROACHES
    DO I = 1, SIGNALDATA(IACT)%N_INDIRECT_APPROACHES
      SIGNALDATA(IACT)%INDIRECT_APPROACH_USN(I) = SUSN(AC_SIGNALS(IACT)%INDIRECT_APPROACH(I))
      SIGNALDATA(IACT)%INDIRECT_APPROACH_DSN(I) = SDSN(AC_SIGNALS(IACT)%INDIRECT_APPROACH(I))
    ENDDO
    SIGNALDATA(IACT)%DETECTOR_COUNT = AC_SIGNALS(IACT)%SDP%DETECTOR_COUNT
    SIGNALDATA(IACT)%DETECTOR_LIST = AC_SIGNALS(IACT)%SDP%DETECTOR_LIST
    
    SIGNALDATA(IACT)%CYCLE_LENGTH = AC_SIGNALS(IACT)%CYCLE_LENGTH
    SIGNALDATA(IACT)%OFFSET = AC_SIGNALS(IACT)%OFFSET
    SIGNALDATA(IACT)%TRANSITION_METHOD = AC_SIGNALS(IACT)%TRANSITION_METHOD
    SIGNALDATA(IACT)%MAX_ADD = AC_SIGNALS(IACT)%MAXPCT_ADD
    SIGNALDATA(IACT)%MAX_SUBTRACT = AC_SIGNALS(IACT)%MAXPCT_SUBTRACT
    SIGNALDATA(IACT)%FORCE_OFF_TIMES = AC_SIGNALS(IACT)%FORCE_OFF_TIME
    SIGNALDATA(IACT)%SPLITS = AC_SIGNALS(IACT)%PHASE_SPLITS

    SIGNALDATA(IACT)%RING_PHASE = AC_SIGNALS(IACT)%PHASE_SEQUENCE
    
    SIGNALDATA(IACT)%CHECKIN_DISTANCE = AC_SIGNALS(IACT)%CHECKIN_DISTANCE
    SIGNALDATA(IACT)%CHECKOUT_DISTANCE = AC_SIGNALS(IACT)%CHECKOUT_DISTANCE 
    SIGNALDATA(IACT)%POLICY = AC_SIGNALS(IACT)%POLICY 

  ENDDO
  GET_AC_SIGNALS = 0
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_FREEWAY_DETECTOR_INPUTS[DLLEXPORT, STDCALL](OUTPUTS)
  USE FREEWAY_DETECTORS
  USE API_DATA
  USE FREEWAY_LINKS
  TYPE(DETECTOR_INPUTS) :: OUTPUTS(N_FREEWAY_DETECTORS)
  INTEGER :: I, USN, DSN
! ----------------------------------------------------------------------
  DO I = 1, N_FREEWAY_DETECTORS
    USN = FUSN(FDETECTOR(I)%LINK)
    DSN = FDSN(FDETECTOR(I)%LINK)
    OUTPUTS(I)%USN = USN
    OUTPUTS(I)%DSN = DSN
    OUTPUTS(I)%DETECTOR_LANES(1) = FDETECTOR(I)%LANE1 
    OUTPUTS(I)%LOCATION = FDETECTOR(I)%LOCATION
    OUTPUTS(I)%CARRYOVER_TIME = FDETECTOR(I)%CARRYOVER_TIME
    OUTPUTS(I)%DELAY_TIME = FDETECTOR(I)%DELAY_TIME
    OUTPUTS(I)%TYPE_CODE = FDETECTOR(I)%TYPE_CODE 
    OUTPUTS(I)%STATION_ID = FDETECTOR(I)%STATION_ID
    OUTPUTS(I)%ZONE_LENGTH = FDETECTOR(I)%ZONE_LENGTH
    OUTPUTS(I)%OPERATION_CODE = FDETECTOR(I)%OPERATION_CODE
  ENDDO
  GET_FREEWAY_DETECTOR_INPUTS = 0
  END  
  
! ==================================================================================================
  INTEGER FUNCTION GET_STREET_DETECTOR_INPUTS[DLLEXPORT, STDCALL](OUTPUTS)
  USE STREET_DETECTORS
  USE API_DATA
  USE STREET_LINKS
  USE STREET_NODES
  TYPE(DETECTOR_INPUTS) :: OUTPUTS(N_STREET_DETECTORS)
  INTEGER :: I, USN, DSN
! ----------------------------------------------------------------------
  DO I = 1, N_STREET_DETECTORS
    USN = SUSN(SDETECTOR(I)%LINK)
    DSN = SDSN(SDETECTOR(I)%LINK)
    OUTPUTS(I)%USN = USN
    OUTPUTS(I)%DSN = DSN
! Allocated check is added to avoid crashes    LZ 05/02/2024
!    If ( Allocated(DETECTOR_LANES)) then
    OUTPUTS(I)%DETECTOR_LANES = SDETECTOR(I)%DETECTOR_LANES 
    OUTPUTS(I)%LOCATION = SLENGTH(SDETECTOR(I)%LINK) - SDETECTOR(I)%LOCATION
    OUTPUTS(I)%CARRYOVER_TIME = SDETECTOR(I)%CARRYOVER_TIME
    OUTPUTS(I)%DELAY_TIME = SDETECTOR(I)%DELAY_TIME
    OUTPUTS(I)%OPERATION_CODE = SDETECTOR(I)%OPERATION_CODE 
    OUTPUTS(I)%STATION_ID = SDETECTOR(I)%STATION_ID
    OUTPUTS(I)%ZONE_LENGTH = SDETECTOR(I)%ZONE_LENGTH
    OUTPUTS(I)%OPERATION_CODE = SDETECTOR(I)%OPERATION_CODE
    OUTPUTS(I)%ASSOCIATED_PHASE = SDETECTOR(I)%ASSOCIATED_PHASE
    OUTPUTS(I)%NEXT_DET = SDETECTOR(I)%NEXT_DET
 !   endif 
  ENDDO
  GET_STREET_DETECTOR_INPUTS = 0
  END

! ==================================================================================================
  INTEGER FUNCTION GET_NUMBER_OF_PARKING_ZONES[DLLEXPORT, STDCALL]
  USE PARKING
  IMPLICIT NONE
! ----------------------------------------------------------------------
  GET_NUMBER_OF_PARKING_ZONES = NUMBER_OF_PARKING_ZONES
  END

! ==================================================================================================
  INTEGER FUNCTION GET_PARKING_ZONES[DLLEXPORT, STDCALL](OUTPUTS)
  USE API_DATA
  USE PARKING
  USE STREET_LINKS
  IMPLICIT NONE
  TYPE(PARKING_DATA) :: OUTPUTS(NUMBER_OF_PARKING_ZONES)
  INTEGER :: I
! ----------------------------------------------------------------------
  DO I = 1, NUMBER_OF_PARKING_ZONES
    OUTPUTS(I)%USN = SUSN(PARKING_ZONE_LINK(I))
    OUTPUTS(I)%DSN = SDSN(PARKING_ZONE_LINK(I))
    OUTPUTS(I)%DURATION = PARK_DURATION(I)    
    OUTPUTS(I)%FREQ = PARK_FREQ(I)      
    OUTPUTS(I)%LEFT_START = PARK_LEFT_START(I)
    OUTPUTS(I)%LEFT_LEN = PARK_LEFT_LEN(I)   
    OUTPUTS(I)%RIGHT_START = PARK_RIGHT_START(I)
    OUTPUTS(I)%RIGHT_LEN = PARK_RIGHT_LEN(I)  
  ENDDO
  GET_PARKING_ZONES = 0
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_NUMBER_OF_EVENTS[DLLEXPORT, STDCALL]
  USE EVENTS
  IMPLICIT NONE
! ----------------------------------------------------------------------
  GET_NUMBER_OF_EVENTS = NUMBER_OF_EVENTS
  END

! ==================================================================================================
  INTEGER FUNCTION GET_NUMBER_OF_LONGTERM_EVENTS[DLLEXPORT, STDCALL]
  USE EVENTS
  IMPLICIT NONE
! ----------------------------------------------------------------------
  GET_NUMBER_OF_LONGTERM_EVENTS = NUMBER_OF_LONGTERM_EVENTS
  END

! ==================================================================================================
  INTEGER FUNCTION GET_FVEHICLE_STRUCT_SIZE[DLLEXPORT, STDCALL]
  USE FREEWAY_VEHICLES
  IMPLICIT NONE
  GET_FVEHICLE_STRUCT_SIZE = HIGHEST_INDEX_F
  END
 
! ==================================================================================================
  INTEGER FUNCTION GET_LONGTERM_EVENTS[DLLEXPORT, STDCALL](OUTPUTS)
  USE API_DATA
  USE EVENTS
  USE STREET_LINKS
  IMPLICIT NONE
  TYPE(EVENT_DATA) :: OUTPUTS(NUMBER_OF_EVENTS)
  INTEGER :: I, IL
! ----------------------------------------------------------------------
  GET_LONGTERM_EVENTS = 0
  DO I = 1, NUMBER_OF_EVENTS
    IL = EVENT_LINK(I)
    IF(IL .EQ. 0) THEN
      GET_LONGTERM_EVENTS = 1
    ELSE
      OUTPUTS(I)%USN = SUSN(IL)
      OUTPUTS(I)%DSN = SDSN(IL)
      OUTPUTS(I)%LANE = EVENT_LANE(I) 
      OUTPUTS(I)%BEGIN_TIME = EVENT_BEGIN_TIME(I)
      OUTPUTS(I)%END_TIME = EVENT_END_TIME(I)  
      OUTPUTS(I)%LOCATION = EVENT_LOCATION(I)
      OUTPUTS(I)%SPEED_REDUCTION = EVENT_SPEED_REDUCTION(I)
      OUTPUTS(I)%LENGTH = EVENT_LENGTH(I)
      OUTPUTS(I)%CODE = EVENT_CODE(I)
      OUTPUTS(I)%GROUP_ID = EVENT_GROUP_ID(I)
      IF(PED_APPROACH_LINK(I) .NE. 0) THEN
        OUTPUTS(I)%APPROACH_USN = SUSN(PED_APPROACH_LINK(I))
        OUTPUTS(I)%APPROACH_DSN = SDSN(PED_APPROACH_LINK(I))
      ENDIF
    ENDIF
  ENDDO
    END
 
! ==================================================================================================
!Added by LZ 7/14/2024 to save anamation data 
! ==================================================================================================
    
!  INTEGER FUNCTION GET_ALLFVEHICLE_STRUCT[DLLEXPORT, STDCALL](VDATAALL) !BIND(C, NAME="GET_ALLFVEHICLE_STRUCT")
! USE API_DATA
!  USE FREEWAY_VEHICLES
! IMPLICIT NONE
! !TYPE(FV_STRUCT), ALLOCATABLE, INTENT(OUT) :: VDATAALL(:,:)

  ! Simply return VDATAALL
!  GET_ALLFVEHICLE_STRUCT = 0
!  END FUNCTION GET_ALLFVEHICLE_STRUCT
! ==================================================================================================
  INTEGER FUNCTION GET_FVEHICLE_STRUCT[DLLEXPORT, STDCALL](VDATA)
  USE API_DATA
  USE FREEWAY_VEHICLES
  IMPLICIT NONE
  TYPE(FV_STRUCT) :: VDATA(HIGHEST_INDEX_F)
  TYPE(SV_STRUCT) :: SVDATA(1)

  INTEGER :: I
  
  ! Print the size of each element in bytes
  !INTEGER ISIZEZ, ISIZEX
  !ISIZEZ=SIZEOF(VDATA(1))
  !PRINT *, 'Size of SV_STRUCT: ', SIZEOF(SVDATA(1)), ' bytes. Size of FV_STRUCT: ', ISIZEZ, ' bytes'

  ! Print the total size of the array in bytes
  !ISIZEX=ISIZEZ*SIZE(VDATA)
  !PRINT *, 'Total size of VDATA: ', ISIZEX, ' bytes'

  ! Print the number of elements
  !PRINT *, 'Number of elements in VDATA: ', SIZE(VDATA)
  
  

  IF(ALLOCATED(FACCELERATION)) THEN
    DO I = 1, HIGHEST_INDEX_F
!      PRINT *, 'ACCELERATION = ', FACCELERATION(I)
      VDATA(I)%ACCELERATION = FACCELERATION(I)
!      PRINT *, 'VDATA(', I-1, ')%ACCELERATION = ', VDATA(I)%ACCELERATION
!
!     PRINT *, 'DECEL = ', FDECEL(I)
      VDATA(I)%DECEL = FDECEL(I)
!     PRINT *, 'VDATA(', I-1, ')%DECEL = ', VDATA(I)%DECEL

!     PRINT *, 'DESIREDSPEED = ', FDESIREDSPEED(I)
      VDATA(I)%DESIREDSPEED = FDESIREDSPEED(I)
!     PRINT *, 'VDATA(', I-1, ')%DESIREDSPEED = ', VDATA(I)%DESIREDSPEED

 !    PRINT *, 'DISCH_TIMER = ', FDISCH_TIMER(I)
      VDATA(I)%DISCH_TIMER = FDISCH_TIMER(I)
 !    PRINT *, 'VDATA(', I-1, ')%DISCH_TIMER = ', VDATA(I)%DISCH_TIMER

 !    PRINT *, 'DRIVERTYPE = ', FDRIVERTYPE(I)
      VDATA(I)%DRIVERTYPE = FDRIVERTYPE(I)
  !   PRINT *, 'VDATA(', I-1, ')%DRIVERTYPE = ', VDATA(I)%DRIVERTYPE

  !   PRINT *, 'ENTRY_LINK = ', FENTRY_LINK(I)
      VDATA(I)%ENTRY_LINK = FENTRY_LINK(I)
  !   PRINT *, 'VDATA(', I-1, ')%ENTRY_LINK = ', VDATA(I)%ENTRY_LINK

  !   PRINT *, 'ENTRYTIME = ', FENTRYTIME(I)
      VDATA(I)%ENTRYTIME = FENTRYTIME(I)
 !    PRINT *, 'VDATA(', I-1, ')%ENTRYTIME = ', VDATA(I)%ENTRYTIME

  !   PRINT *, 'EV_DIST = ', FEV_DIST(I)
      VDATA(I)%EV_DIST = FEV_DIST(I)
  !   PRINT *, 'VDATA(', I-1, ')%EV_DIST = ', VDATA(I)%EV_DIST

  !   PRINT *, 'EV_OVRSPD = ', FEV_OVRSPD(I)
      VDATA(I)%EV_OVRSPD = FEV_OVRSPD(I)
  !   PRINT *, 'VDATA(', I-1, ')%EV_OVRSPD = ', VDATA(I)%EV_OVRSPD

   !  PRINT *, 'EV_RANGE = ', FEV_RANGE(I)
      VDATA(I)%EV_RANGE = FEV_RANGE(I)
   !  PRINT *, 'VDATA(', I-1, ')%EV_RANGE = ', VDATA(I)%EV_RANGE

   !  PRINT *, 'EV_RAND = ', FEV_RAND(I)
      VDATA(I)%EV_RAND = FEV_RAND(I)
   !  PRINT *, 'VDATA(', I-1, ')%EV_RAND = ', VDATA(I)%EV_RAND

   !  PRINT *, 'EV_WAIT_TIMER = ', FEV_WAIT_TIMER(I)
      VDATA(I)%EV_WAIT_TIMER = FEV_WAIT_TIMER(I)
   !  PRINT *, 'VDATA(', I-1, ')%EV_WAIT_TIMER = ', VDATA(I)%EV_WAIT_TIMER

    ! PRINT *, 'EV_WATCH = ', FEV_WATCH(I)
      VDATA(I)%EV_WATCH = FEV_WATCH(I)
    ! PRINT *, 'VDATA(', I-1, ')%EV_WATCH = ', VDATA(I)%EV_WATCH

    ! PRINT *, 'FLEET = ', FFLEET(I)
      VDATA(I)%FLEET = FFLEET(I)
    ! PRINT *, 'VDATA(', I-1, ')%FLEET = ', VDATA(I)%FLEET

    ! PRINT *, 'GO_THRU_SIGNAL = ', FGO_THRU_SIGNAL(I)
      VDATA(I)%GO_THRU_SIGNAL = FGO_THRU_SIGNAL(I)
    ! PRINT *, 'VDATA(', I-1, ')%GO_THRU_SIGNAL = ', VDATA(I)%GO_THRU_SIGNAL

   !  PRINT *, 'LAG_TIMER = ', FLAG_TIMER(I)! 
      VDATA(I)%LAG_TIMER = FLAG_TIMER(I)
   !  PRINT *, 'VDATA(', I-1, ')%LAG_TIMER = ', VDATA(I)%LAG_TIMER

   !  PRINT *, 'LANE = ', FLANE(I)
      VDATA(I)%LANE = FLANE(I)
   !  PRINT *, 'VDATA(', I-1, ')%LANE = ', VDATA(I)%LANE

   !  PRINT *, 'LANECODES = ', FLANECODES(I, 1:N_FREEWAY_LANES)
      VDATA(I)%LANECODES = FLANECODES(I, 1:N_FREEWAY_LANES)
   !  PRINT *, 'VDATA(', I-1, ')%LANECODES = ', VDATA(I)%LANECODES

   !  PRINT *, 'LINK = ', FLINK(I)
      VDATA(I)%LINK = FLINK(I)
   !  PRINT *, 'VDATA(', I-1, ')%LINK = ', VDATA(I)%LINK

   !  PRINT *, 'FOLLOWER = ', FFOLLOWER(I)
      VDATA(I)%FOLLOWER = FFOLLOWER(I)
   !  PRINT *, 'VDATA(', I-1, ')%FOLLOWER = ', VDATA(I)%FOLLOWER

   !  PRINT *, 'ID = ', FID(I)
      VDATA(I)%ID = FID(I)
   !  PRINT *, 'VDATA(', I-1, ')%ID = ', VDATA(I)%ID

   !  PRINT *, 'LAST_DETID = ', FLAST_DETID(I)
      VDATA(I)%LAST_DETID = FLAST_DETID(I)
   !  PRINT *, 'VDATA(', I-1, ')%LAST_DETID = ', VDATA(I)%LAST_DETID

   !  PRINT *, 'LC_TIMER = ', FLC_TIMER(I)
      VDATA(I)%LC_TIMER = FLC_TIMER(I)
   !  PRINT *, 'VDATA(', I-1, ')%LC_TIMER = ', VDATA(I)%LC_TIMER

   !  PRINT *, 'LEADER = ', FLEADER(I)
      VDATA(I)%LEADER = FLEADER(I)
   !  PRINT *, 'VDATA(', I-1, ')%LEADER = ', VDATA(I)%LEADER

   !  PRINT *, 'LOCATION = ', FLOCATION(I)
      VDATA(I)%LOCATION = FLOCATION(I)
   !  PRINT *, 'VDATA(', I-1, ')%LOCATION = ', VDATA(I)%LOCATION

   !  PRINT *, 'PATHID = ', FPATHID(I)
      VDATA(I)%PATHID = FPATHID(I)
   !  PRINT *, 'VDATA(', I-1, ')%PATHID = ', VDATA(I)%PATHID

    
    ENDDO
    GET_FVEHICLE_STRUCT = 0
  ELSE
    GET_FVEHICLE_STRUCT = 1
  ENDIF
  END  

! ==================================================================================================
  INTEGER FUNCTION GET_SVEHICLE_STRUCT_SIZE[DLLEXPORT, STDCALL]
  USE STREET_VEHICLES
  IMPLICIT NONE
  GET_SVEHICLE_STRUCT_SIZE = HIGHEST_INDEX_S
  END

! ==================================================================================================
  INTEGER FUNCTION GET_SVEHICLE_STRUCT[DLLEXPORT, STDCALL](VDATA)
  USE API_DATA
  USE STREET_VEHICLES
  IMPLICIT NONE
  TYPE(SV_STRUCT) :: VDATA(HIGHEST_INDEX_S)
  INTEGER :: I
  IF(ALLOCATED(SACCELERATION)) THEN
    DO I = 1, HIGHEST_INDEX_S
      VDATA(I)%ACCELERATION   = SACCELERATION(I)
      VDATA(I)%DECEL          = SDECEL(I)
      VDATA(I)%DESIREDSPEED   = SDESIREDSPEED(I)
      VDATA(I)%DISCH_TIMER    = SDISCH_TIMER(I)
      VDATA(I)%DRIVERTYPE     = SDRIVERTYPE(I)
      VDATA(I)%ENTRY_LINK     = SENTRY_LINK(I)
      VDATA(I)%ENTRYTIME      = SENTRYTIME(I)
      VDATA(I)%EV_DIST        = SEV_DIST(I)
      VDATA(I)%EV_OVRSPD      = SEV_OVRSPD(I)
      VDATA(I)%EV_RANGE       = SEV_RANGE(I)
      VDATA(I)%EV_RAND        = SEV_RAND(I)
      VDATA(I)%EV_WAIT_TIMER  = SEV_WAIT_TIMER(I)
      VDATA(I)%EV_WATCH       = SEV_WATCH(I)
      VDATA(I)%FLEET          = SFLEET(I)
      VDATA(I)%GO_THRU_SIGNAL = SGO_THRU_SIGNAL(I)
      VDATA(I)%LAG_TIMER      = SLAG_TIMER(I)
      VDATA(I)%LANE           = SLANE(I)
      VDATA(I)%LANECODES      = SLANECODES(I, 1:N_STREET_LANES)
      VDATA(I)%LINK           = SLINK(I)
      VDATA(I)%FOLLOWER       = SFOLLOWER(I)
      VDATA(I)%ID             = SID(I)
      VDATA(I)%LAST_DETID     = SLAST_DETID(I)
      VDATA(I)%LC_TIMER       = SLC_TIMER(I)
      VDATA(I)%LEADER         = SLEADER(I)
      VDATA(I)%LOCATION       = SLOCATION(I)
      VDATA(I)%PATHID         = SPATHID(I)
      VDATA(I)%PATHPOINT      = SPATHPOINT(I)
      VDATA(I)%PSAVE          = SPSAVE(I)
      VDATA(I)%PSEUDO_LEADER  = SPSEUDO_LEADER(I)
      VDATA(I)%PREV_ACCEL     = SPREV_ACCEL(I)
      VDATA(I)%PREVLINK       = SPREVLINK(I)
      VDATA(I)%PREVLANE       = SPREVLANE(I)
      VDATA(I)%ROUTEID        = SROUTEID(I)
      VDATA(I)%SPEED          = SSPEED(I)
      VDATA(I)%SPEED_ADJ      = SSPEED_ADJ(I)
      VDATA(I)%START_LAG      = SSTART_LAG(I)
      VDATA(I)%TURNCODE       = STURNCODE(I)
      VDATA(I)%VLENGTH        = SVLENGTH(I)
      VDATA(I)%VTYPE          = SVTYPE(I)
      VDATA(I)%XCODE          = SXCODE(I)
      VDATA(I)%WILL_COOP_EV   = SWILL_COOP_EV(I)
      VDATA(I)%WILL_COOP_LC   = SWILL_COOP_LC(I)
      VDATA(I)%WILL_MOVE      = SWILL_MOVE(I)
      VDATA(I)%DIVERTED       = SDIVERTED(I)
      VDATA(I)%DWELL_TIMER    = DWELL_TIMER(I)
      VDATA(I)%GOAL_LANE      = GOAL_LANE(I)
      VDATA(I)%HAS_STOPPED    = HAS_STOPPED(I)
      VDATA(I)%SPDICD         = SPDICD(I)
      VDATA(I)%NEXT_STOP      = NEXT_STOP(I)
      VDATA(I)%PRVDIST        = PRVDIST(I)
      VDATA(I)%ICD_LINK        = ICD_LINK(I)
      VDATA(I)%PRVLNKICD      = PRVLNKICD(I)
      VDATA(I)%QSTATE         = QSTATE(I)
      VDATA(I)%TURN_CODE      = TURN_CODE(I)
      VDATA(I)%TURN_CODE2     = TURN_CODE2(I)
      VDATA(I)%TURN_LINK      = TURN_LINK(I)
      VDATA(I)%TURN_LINK2     = TURN_LINK2(I)
      VDATA(I)%VEHICD         = VEHICD(I)
      VDATA(I)%VEHICD0        = VEHICD0(I)
      VDATA(I)%WILL_JUMP      = WILL_JUMP(I)
      VDATA(I)%WILL_YIELD     = WILL_YIELD(I)
      VDATA(I)%ARC_LOCATION   = ARC_LOCATION(I)
      VDATA(I)%ARC_ENTRYLINK  = ARC_ENTRYLINK(I)
      VDATA(I)%ARC_ENTRYLANE  = ARC_ENTRYLANE(I)
      VDATA(I)%CF_MODEL       = SCF_MODEL(I)
    ENDDO
    GET_SVEHICLE_STRUCT = 0
  ELSE
    GET_SVEHICLE_STRUCT = 1
  ENDIF
  END  

! ==================================================================================================
  INTEGER FUNCTION SET_FVEHICLE_STRUCT[DLLEXPORT, STDCALL](VDATA)
  USE API_DATA
  USE FREEWAY_VEHICLES
  IMPLICIT NONE
  TYPE(FV_STRUCT) :: VDATA(HIGHEST_INDEX_F)
  INTEGER :: I
  DO I = 1, HIGHEST_INDEX_F
    FACCELERATION(I)           = VDATA(I)%ACCELERATION           
    FDECEL(I)                  = VDATA(I)%DECEL                  
    FDESIREDSPEED(I)           = VDATA(I)%DESIREDSPEED           
    FDISCH_TIMER(I)            = VDATA(I)%DISCH_TIMER            
    FDRIVERTYPE(I)             = VDATA(I)%DRIVERTYPE             
    FENTRY_LINK(I)             = VDATA(I)%ENTRY_LINK             
    FENTRYTIME(I)              = VDATA(I)%ENTRYTIME              
    FEV_DIST(I)                = VDATA(I)%EV_DIST                
    FEV_OVRSPD(I)              = VDATA(I)%EV_OVRSPD              
    FEV_RANGE(I)               = VDATA(I)%EV_RANGE               
    FEV_RAND(I)                = VDATA(I)%EV_RAND                
    FEV_WAIT_TIMER(I)          = VDATA(I)%EV_WAIT_TIMER          
    FEV_WATCH(I)               = VDATA(I)%EV_WATCH               
    FFLEET(I)                  = VDATA(I)%FLEET                  
    FGO_THRU_SIGNAL(I)         = VDATA(I)%GO_THRU_SIGNAL         
    FLAG_TIMER(I)              = VDATA(I)%LAG_TIMER              
    FLANE(I)                   = VDATA(I)%LANE                   
    FLANECODES(I, 1:N_FREEWAY_LANES)        = VDATA(I)%LANECODES         
    FLINK(I)                   = VDATA(I)%LINK                   
    FFOLLOWER(I)               = VDATA(I)%FOLLOWER               
    FID(I)                     = VDATA(I)%ID                     
    FLAST_DETID(I)             = VDATA(I)%LAST_DETID             
    FLC_TIMER(I)               = VDATA(I)%LC_TIMER               
    FLEADER(I)                 = VDATA(I)%LEADER                 
    FLOCATION(I)               = VDATA(I)%LOCATION               
    FPATHID(I)                 = VDATA(I)%PATHID                 
    FPATHPOINT(I)              = VDATA(I)%PATHPOINT              
    FPSAVE(I)                  = VDATA(I)%PSAVE                  
    FPSEUDO_LEADER(I)          = VDATA(I)%PSEUDO_LEADER          
    FPREV_ACCEL(I)             = VDATA(I)%PREV_ACCEL             
    FPREVLINK(I)               = VDATA(I)%PREVLINK               
    FPREVLANE(I)               = VDATA(I)%PREVLANE               
    FROUTEID(I)                = VDATA(I)%ROUTEID                
    FSPEED(I)                  = VDATA(I)%SPEED                  
    FSPEED_ADJ(I)              = VDATA(I)%SPEED_ADJ              
    FTURNCODE(I)               = VDATA(I)%TURNCODE               
    FVLENGTH(I)                = VDATA(I)%VLENGTH                
    FVTYPE(I)                  = VDATA(I)%VTYPE                  
    FXCODE(I)                  = VDATA(I)%XCODE                  
    FWILL_COOP_EV(I)           = VDATA(I)%WILL_COOP_EV           
    FWILL_COOP_LC(I)           = VDATA(I)%WILL_COOP_LC           
    FWILL_MOVE(I)              = VDATA(I)%WILL_MOVE              
    DESTINATION(I)             = VDATA(I)%DESTINATION            
    DISTANCE_TO_SEGMENT_END(I) = VDATA(I)%DISTANCE_TO_SEGMENT_END
    FDIVERTED(I)               = VDATA(I)%DIVERTED               
    HOV_VIOLATOR(I)            = VDATA(I)%HOV_VIOLATOR           
    IMETER(I)                  = VDATA(I)%IMETER                 
    INCIDENT_NUM(I)            = VDATA(I)%INCIDENT_NUM           
    ISEGMENT(I)                = VDATA(I)%ISEGMENT               
    MUST_MERGE(I)              = VDATA(I)%MUST_MERGE             
    NEXT_OBJECT(I)             = VDATA(I)%NEXT_OBJECT            
    REMAINING_DIST(I)          = VDATA(I)%REMAINING_DIST         
    SORTED_LIST(I)             = VDATA(I)%SORTED_LIST            
    SORT_POSITION(I)           = VDATA(I)%SORT_POSITION  
    FCF_MODEL(I)               = VDATA(I)%CF_MODEL
  ENDDO
  SET_FVEHICLE_STRUCT = 0
  END  

! ==================================================================================================
  INTEGER FUNCTION SET_SVEHICLE_STRUCT[DLLEXPORT, STDCALL](VDATA)
  USE API_DATA
  USE STREET_VEHICLES
  IMPLICIT NONE
  TYPE(SV_STRUCT) :: VDATA(HIGHEST_INDEX_S)
  INTEGER :: I
  DO I = 1, HIGHEST_INDEX_S
    SACCELERATION(I)         = VDATA(I)%ACCELERATION  
    SDECEL(I)                = VDATA(I)%DECEL         
    SDESIREDSPEED(I)         = VDATA(I)%DESIREDSPEED  
    SDISCH_TIMER(I)          = VDATA(I)%DISCH_TIMER   
    SDRIVERTYPE(I)           = VDATA(I)%DRIVERTYPE    
    SENTRY_LINK(I)           = VDATA(I)%ENTRY_LINK    
    SENTRYTIME(I)            = VDATA(I)%ENTRYTIME     
    SEV_DIST(I)              = VDATA(I)%EV_DIST       
    SEV_OVRSPD(I)            = VDATA(I)%EV_OVRSPD     
    SEV_RANGE(I)             = VDATA(I)%EV_RANGE      
    SEV_RAND(I)              = VDATA(I)%EV_RAND       
    SEV_WAIT_TIMER(I)        = VDATA(I)%EV_WAIT_TIMER 
    SEV_WATCH(I)             = VDATA(I)%EV_WATCH      
    SFLEET(I)                = VDATA(I)%FLEET         
    SGO_THRU_SIGNAL(I)       = VDATA(I)%GO_THRU_SIGNAL
    SLAG_TIMER(I)            = VDATA(I)%LAG_TIMER     
    SLANE(I)                 = VDATA(I)%LANE          
    SLANECODES(I, 1:N_STREET_LANES)       = VDATA(I)%LANECODES     
    SLINK(I)                 = VDATA(I)%LINK          
    SFOLLOWER(I)             = VDATA(I)%FOLLOWER      
    SID(I)                   = VDATA(I)%ID            
    SLAST_DETID(I)           = VDATA(I)%LAST_DETID    
    SLC_TIMER(I)             = VDATA(I)%LC_TIMER      
    SLEADER(I)               = VDATA(I)%LEADER        
    SLOCATION(I)             = VDATA(I)%LOCATION      
    SPATHID(I)               = VDATA(I)%PATHID        
    SPATHPOINT(I)            = VDATA(I)%PATHPOINT     
    SPSAVE(I)                = VDATA(I)%PSAVE         
    SPSEUDO_LEADER(I)        = VDATA(I)%PSEUDO_LEADER 
    SPREV_ACCEL(I)           = VDATA(I)%PREV_ACCEL    
    SPREVLINK(I)             = VDATA(I)%PREVLINK      
    SPREVLANE(I)             = VDATA(I)%PREVLANE      
    SROUTEID(I)              = VDATA(I)%ROUTEID       
    SSPEED(I)                = VDATA(I)%SPEED         
    SSPEED_ADJ(I)            = VDATA(I)%SPEED_ADJ     
    SSTART_LAG(I)            = VDATA(I)%START_LAG     
    STURNCODE(I)             = VDATA(I)%TURNCODE                        
    SVLENGTH(I)              = VDATA(I)%VLENGTH       
    SVTYPE(I)                = VDATA(I)%VTYPE         
    SXCODE(I)                = VDATA(I)%XCODE         
    SWILL_COOP_EV(I)         = VDATA(I)%WILL_COOP_EV  
    SWILL_COOP_LC(I)         = VDATA(I)%WILL_COOP_LC  
    SWILL_MOVE(I)            = VDATA(I)%WILL_MOVE     
    SDIVERTED(I)             = VDATA(I)%DIVERTED      
    DWELL_TIMER(I)           = VDATA(I)%DWELL_TIMER   
    GOAL_LANE(I)             = VDATA(I)%GOAL_LANE     
    HAS_STOPPED(I)           = VDATA(I)%HAS_STOPPED   
    SPDICD(I)                = VDATA(I)%SPDICD       
    NEXT_STOP(I)             = VDATA(I)%NEXT_STOP     
    PRVDIST(I)               = VDATA(I)%PRVDIST       
    ICD_LINK(I)               = VDATA(I)%ICD_LINK       
    PRVLNKICD(I)             = VDATA(I)%PRVLNKICD     
    QSTATE(I)                = VDATA(I)%QSTATE        
    TURN_CODE(I)             = VDATA(I)%TURN_CODE     
    TURN_CODE2(I)            = VDATA(I)%TURN_CODE2    
    TURN_LINK(I)             = VDATA(I)%TURN_LINK     
    TURN_LINK2(I)            = VDATA(I)%TURN_LINK2    
    VEHICD(I)                = VDATA(I)%VEHICD
    VEHICD0(I)               = VDATA(I)%VEHICD0        
    WILL_JUMP(I)             = VDATA(I)%WILL_JUMP     
    WILL_YIELD(I)            = VDATA(I)%WILL_YIELD    
    SCF_MODEL(I)             = VDATA(I)%CF_MODEL
    ARC_LOCATION(I)          = VDATA(I)%ARC_LOCATION
    ARC_ENTRYLINK(I)         = VDATA(I)%ARC_ENTRYLINK
    ARC_ENTRYLANE(I)         = VDATA(I)%ARC_ENTRYLANE
  ENDDO
  SET_SVEHICLE_STRUCT = 0
  END  

! ==================================================================================================
  INTEGER FUNCTION GET_INTERVAL[DLLEXPORT, STDCALL](NODE)
! ----------------------------------------------------------------------
!     This function allows an external entity to determine the
!     current interval of a fixed time signal.
! ----------------------------------------------------------------------
  USE STREET_NODES
  USE TIMED_CONTROLLERS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NODE
  INTEGER :: IACT
!----------------------------------------------------------------------
! Returns: int =  0 - success
!              = -1 - controller was not found
! ----------------------------------------------------------------------
  IACT = NFTC(NODE)
  IF(IACT .EQ. 0) THEN
    GET_INTERVAL = -1
    WRITE(MSGTEXT,'(A,I4)') 'FIXED TIME CONTROLLER NOT FOUND AT NODE ', NODE
    CALL SENDTEXTMSG(M_INFO)
  ELSE
    GET_INTERVAL = FTC_SIGNALS(IACT)%CURRENT_INTERVAL
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION GET_DURATION[DLLEXPORT, STDCALL](NODE, N)
  USE STREET_NODES
  USE TIMED_CONTROLLERS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NODE, N
  INTEGER :: ISIG
!----------------------------------------------------------------------
! Returns: int =  0 - success
!              = -1 - controller was not found
! ----------------------------------------------------------------------
  ISIG = NFTC(NODE)
  IF(ISIG .NE. 0) THEN
    GET_DURATION = FTC_SIGNALS(ISIG)%DURATION(N)
  ELSE
    GET_DURATION = -1
    WRITE(MSGTEXT,'(A,I4)') 'FIXED TIME CONTROLLER NOT FOUND AT NODE ', NODE
    CALL SENDTEXTMSG(M_INFO)
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION GET_NUMBER_OF_INCIDENTS[DLLEXPORT, STDCALL]
  USE INCIDENTS
  IMPLICIT NONE
! ----------------------------------------------------------------------
  GET_NUMBER_OF_INCIDENTS = NUMBER_OF_INCIDENTS
  END

! ==================================================================================================
  INTEGER FUNCTION GET_INCIDENTS[DLLEXPORT, STDCALL](OUTPUTS)
  USE API_DATA
  USE INCIDENTS
  USE FREEWAY_LINKS
  IMPLICIT NONE
  TYPE(INCIDENT_DATA) :: OUTPUTS(NUMBER_OF_INCIDENTS)
  INTEGER :: I
! ----------------------------------------------------------------------
  DO I = 1, NUMBER_OF_INCIDENTS
    OUTPUTS(I)%USN = FUSN(INCIDENT_LINK(I)) 
    OUTPUTS(I)%DSN = FDSN(INCIDENT_LINK(I)) 
    OUTPUTS(I)%BEGIN_POINT = INCIDENT_BEGIN_POINT(I)
    OUTPUTS(I)%BEGIN_TIME = INCIDENT_BEGIN_TIME(I)
    OUTPUTS(I)%END_POINT = INCIDENT_END_POINT(I)
    OUTPUTS(I)%END_TIME = INCIDENT_END_TIME(I)
    OUTPUTS(I)%RBNF = INCIDENT_RBNF(I)
    OUTPUTS(I)%WARN_POINT = INCIDENT_WARN_POINT(I)
    OUTPUTS(I)%CODE = INCIDENT_CODE(I, 1:N_FREEWAY_LANES)
  ENDDO
  GET_INCIDENTS = 0
  END

! ==================================================================================================
  INTEGER FUNCTION GET_NUMBER_OF_DIVERSIONS[DLLEXPORT, STDCALL]
  USE DIVERSIONS
  IMPLICIT NONE
! ----------------------------------------------------------------------
  GET_NUMBER_OF_DIVERSIONS = NUMBER_OF_DIVERSIONS
  END

! ==================================================================================================
  INTEGER FUNCTION GET_DIVERSIONS[DLLEXPORT, STDCALL](OUTPUTS)
  USE API_DATA
  USE DIVERSIONS
  USE FREEWAY_LINKS
  IMPLICIT NONE
  TYPE(DIVERSION_DATA) :: OUTPUTS(NUMBER_OF_DIVERSIONS)
  INTEGER :: I, IL
! ----------------------------------------------------------------------
  DO I = 1, NUMBER_OF_DIVERSIONS
    IL = DIVERSION_LINK(I)
    OUTPUTS(I)%USN = FUSN(IL)
    OUTPUTS(I)%DSN = FDSN(IL)
    OUTPUTS(I)%BEGIN_TIME = DIVERSION_BEGIN_TIME(I)
    OUTPUTS(I)%END_TIME = DIVERSION_END_TIME(I)
    OUTPUTS(I)%LOCATION = DIVERSION_LOCATION(I)
    OUTPUTS(I)%PATHID = DIVERSION_PATHID(I)
    OUTPUTS(I)%PERCENTAGE = DIVERSION_PERCENTAGE(I)
    OUTPUTS(I)%SPEED = DIVERSION_SPEED(I)
  ENDDO
  GET_DIVERSIONS = 0
  END

! ==================================================================================================
  INTEGER FUNCTION CHANGE_ENTRYVOLUMES[DLLEXPORT, STDCALL](INPUTS)
  USE API_DATA
  USE ENTRYNODE_DATA
  USE FLOWDATA_MOD
  USE SIMPARAMS
  IMPLICIT NONE
  TYPE(ENTRY_NODE_DATA) :: INPUTS
  INTEGER :: I, INODE
!----------------------------------------------------------------------
! Returns: int =  0 - success
!              = -1 - link was not found
! ----------------------------------------------------------------------
  CHANGE_ENTRYVOLUMES = -1
  DO I = 1, NUMBER_OF_ENTRYNODES
    IF(ENTRYLINK(I)%UP .EQ. INPUTS%NODE_ID) THEN
      ENTRYLINK(I)%INDEX = ENTRYLINK(I)%INDEX + 1
      ENTRYLINK(I)%TIME(ENTRYLINK(I)%INDEX) = (SIMTIME - 1) / 60.
      ENTRYLINK(I)%FLOW(ENTRYLINK(I)%INDEX) = INPUTS%FLOWRATE
      ENTRYLINK(I)%INDEX = ENTRYLINK(I)%INDEX + 1
      ENTRYLINK(I)%TIME(ENTRYLINK(I)%INDEX) = (SIMTIME - 1 + TPSECONDS(TIME_PERIOD)) / 60.
      ENTRYLINK(I)%FLOW(ENTRYLINK(I)%INDEX) = INPUTS%FLOWRATE
      INODE = ENTRYLINK(I)%UP
      TRUCK_PCT(INODE) = INPUTS%TRUCK_PCT
      CARPOOL_PCT(INODE) = INPUTS%CARPOOL_PCT
      HOV_VIOLATOR_PCT(INODE) = INPUTS%HOV_VIOLATORS_PER10000 / 10000.
      CHANGE_ENTRYVOLUMES = 0
      EXIT
    ENDIF
  ENDDO 
  END

! ==================================================================================================
  INTEGER FUNCTION CHANGE_TURNPERCENTAGES[DLLEXPORT, STDCALL](USN, DSN, LPCT, TPCT, RPCT, LDPCT, RDPCT)
  USE STREET_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: USN, DSN, LPCT, TPCT, RPCT, LDPCT, RDPCT
  INTEGER IL
!----------------------------------------------------------------------
! Returns: int =  0 - success
!              = -1 - link was not found
! ----------------------------------------------------------------------
  CALL FIND_STREET_LINK(USN, DSN, IL)
  IF(IL .NE. 0) THEN
    LEFT_PERCENT(IL) = LPCT
    STHRU_PERCENT(IL) = TPCT
    RIGHT_PERCENT(IL) = RPCT
    LDIAG_PERCENT(IL) = LDPCT
    RDIAG_PERCENT(IL) = RDPCT
    CHANGE_TURNPERCENTAGES = 0
  ELSE
    CHANGE_TURNPERCENTAGES = -1
  ENDIF
  END
  
!=======================================================================
  INTEGER*4 FUNCTION INCREMENT_INTERVAL[DLLEXPORT, STDCALL] (NODE)
  USE TIMED_CONTROLLERS
  USE STREET_LINKS
  USE STREET_VEHICLES
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NODE
  INTEGER :: ISIG, IAP, IL, ICODE, NQ, IV, ILN, INTRVL
!----------------------------------------------------------------------
! Returns: int =  0 - success
!              = -1 - controller was not found
! ----------------------------------------------------------------------
  INCREMENT_INTERVAL = -1
  DO ISIG = 1, NUMBER_OF_FTCS
    IF(FTC_SIGNALS(ISIG)%NODE .EQ. NODE) THEN
      INTRVL = FTC_SIGNALS(ISIG)%CURRENT_INTERVAL + 1
      FTC_SIGNALS(ISIG)%CURRENT_INTERVAL = INTRVL
      DO IAP = 1, FTC_SIGNALS(ISIG)%APPROACHES
        IL = FTC_SIGNALS(ISIG)%APPROACH(IAP)
        ICODE = SIGNAL_CODE(IL)
        SIGNAL_CODE(IL) = FTC_SIGNALS(ISIG)%SIGNAL_CODE(IAP, INTRVL)
        IF(ICODE .NE. SIGNAL_CODE(IL)) CALL PROCESS_QUEUED_VEHICLES(IL, ICODE, SIGNAL_CODE(IL))
 
! --- Update statistics for queued vehicles in each lane.
 
        DO ILN = 1, N_STREET_LANES
          NQ = 0
          IV = FIRST_VEHICLE(IL, ILN)
          DO WHILE(IV .NE. 0)
            IF(QSTATE(IV) .NE. QS_NOTINQ .AND. QSTATE(IV) .NE. QS_DWELL) THEN
              NQ = NQ + 1
            ENDIF
            IV = SFOLLOWER(IV)
          ENDDO
          QUEUE_TOTAL(IL, ILN) = QUEUE_TOTAL(IL, ILN) + NQ
          QUEUE_MAX(IL, ILN) = MAX(QUEUE_MAX(IL, ILN), NQ)
        ENDDO
      ENDDO
      INCREMENT_INTERVAL = 0
      EXIT
    ENDIF
  ENDDO
  END
               
! ==================================================================================================
  INTEGER FUNCTION CHANGE_DURATION[DLLEXPORT, STDCALL](NODE, N, T)
  USE STREET_NODES
  USE TIMED_CONTROLLERS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NODE, N, T
  INTEGER :: ISIG
!----------------------------------------------------------------------
! Returns: int =  0 - success
!              = -1 - controller was not found
! ----------------------------------------------------------------------
  ISIG = NFTC(NODE)
  IF(ISIG .EQ. 0) THEN
    CHANGE_DURATION = -1
    WRITE(MSGTEXT,'(A,I4)') 'CHANGE_DURATION: FIXED TIME CONTROLLER NOT FOUND AT NODE ', NODE
    CALL SENDTEXTMSG(M_INFO)
  ELSE
    FTC_SIGNALS(ISIG)%DURATION(N) = T
    CHANGE_DURATION = 0
    FTC_SIGNALS(ISIG)%CYCLE_LENGTH = SUM(FTC_SIGNALS(ISIG)%DURATION)
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION GETCPUTIME[DLLEXPORT, STDCALL](CPUSEC)
  USE SIMPARAMS
  IMPLICIT NONE
  REAL :: CPUSEC       [REFERENCE]
! ----------------------------------------------------------------------
  CALL CPU_TIME(CPUSEC)
  CPUSEC = CPUSEC - START_CPU
  GETCPUTIME = 0
  END
  
! ==================================================================================================
  INTEGER FUNCTION GETELAPSEDTIME[DLLEXPORT, STDCALL](ETIME)
  USE SIMPARAMS
  IMPLICIT NONE
  REAL :: ETIME       [REFERENCE]
! ----------------------------------------------------------------------
  ETIME = SECNDS(START_TIMER)
  GETELAPSEDTIME = 0
  END

! ==================================================================================================
  INTEGER FUNCTION CLOSE_LANE[DLLEXPORT, STDCALL](USN, DSN, LANE)
  USE FREEWAY_LINKS
  USE STREET_LINKS
  IMPLICIT NONE
  INTEGER :: USN, DSN, LANE
  INTEGER :: IL
!----------------------------------------------------------------------
! Returns: int =  0 - success
!              = -1 - link was not found
! ----------------------------------------------------------------------
  CLOSE_LANE = -1
  CALL FIND_FREEWAY_LINK(USN, DSN, IL)
  IF(IL .NE. 0) THEN
    FLANE_CLOSED(IL, LANE) = .TRUE.
    CLOSE_LANE = 0
    FREEWAY_LANE_CLOSED = .TRUE.
  ELSE
    CALL FIND_STREET_LINK(USN, DSN, IL)
    IF(IL .NE. 0) THEN
      SLANE_CLOSED(IL, LANE) = .TRUE.
      CLOSE_LANE = 0
      STREET_LANE_CLOSED = .TRUE.
    ENDIF
  ENDIF
  END
    
! ==================================================================================================
  INTEGER FUNCTION REOPEN_LANE[DLLEXPORT, STDCALL](USN, DSN, LANE)
  USE FREEWAY_LINKS
  USE STREET_LINKS
  IMPLICIT NONE
  INTEGER :: USN, DSN, LANE
  INTEGER :: IL
!----------------------------------------------------------------------
! Returns: int =  0 - success
!              = -1 - link was not found
! ----------------------------------------------------------------------
  REOPEN_LANE = -1
  CALL FIND_FREEWAY_LINK(USN, DSN, IL)
  IF(IL .NE. 0) THEN
    FLANE_CLOSED(IL, LANE) = .FALSE.
    REOPEN_LANE = 0
  ELSE
    CALL FIND_STREET_LINK(USN, DSN, IL)
    IF(IL .NE. 0) THEN
      SLANE_CLOSED(IL, LANE) = .FALSE.
      REOPEN_LANE = 0
    ENDIF
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION SET_RUN_INPUTS[DLLEXPORT, STDCALL](R, S1, S2, S3, L1, L2, L3, L4, L5, L6) RESULT(ERRORS)
  USE SIMPARAMS
  USE SEEDS
  USE FREEWAY_VEHICLES
  USE STREET_VEHICLES
  USE TEXT
  IMPLICIT NONE                          
  INTEGER, INTENT(IN) :: R, S1, S2, S3, L1, L2, L3, L4, L5, L6
  INTEGER :: ISEED(2)
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    ERRORS = 0
    RUN_NUMBER = R
    ISEED1 = S1
    ISEED2 = S2
    ISEED3 = S3
    FSEED = ISEED3
    SSEED = ISEED3
    ISEED(1) = ISEED1
    CALL RANDOM_SEED(PUT = ISEED)
    USE_NTCIP = L1 .EQ. 1
    USE_DCS = L2 .EQ. 1
    LOGGING = L3 .EQ. 1
    USE_EXTERNAL_DETECTORS = L4 .EQ. 1
    WTRF = L5 .EQ. 1
    STOCHASTIC = L6 .NE. 0
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'SET_RUN_INPUTS CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION SET_NETWORK_INPUTS[DLLEXPORT, STDCALL](INPUTS) RESULT(ERRORS)
  USE API_DATA
  USE SIMPARAMS
  USE FREEWAY_LINKS
  USE STREET_LINKS
  USE STREET_DETECTORS
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS  
  USE TEXT
  USE NODE_TABLE
  IMPLICIT NONE
  TYPE(NETWORK_INPUTS) :: INPUTS
  INTEGER :: I
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    TYPE_OF_RUN = INPUTS%TYPE_OF_RUN
    IF(TYPE_OF_RUN .LT. -3 .OR. TYPE_OF_RUN .GT. 4) THEN
      WRITE(MSGTEXT, '(A, I4)') 'INVALID TYPE OF RUN = ', TYPE_OF_RUN
      CALL SENDTEXTMSG(M_ERROR)
      ERRORS = 1
    ELSE
      ERRORS = 0
      N_FREEWAY_LINKS = 0
      N_STREET_LINKS = 0
      NUMBER_OF_ACS = 0
      N_STREET_DETECTORS = 0
      TIME_PERIOD = 1
      TIMESTEP = INPUTS%TIMESTEP
      DO I = 1, 19
        IF(INPUTS%TIME_PERIOD_DURATION(I) .EQ. 0) EXIT
        N_PERIODS = I
        TPSECONDS(I) = INPUTS%TIME_PERIOD_DURATION(I)
      ENDDO
      SIMULATION_END = SUM(TPSECONDS)
      TIME_INTERVAL = INPUTS%TIME_INTERVAL
      SKIP_INIT = INPUTS%RUN_INIT .EQ. 0
      INITIALIZATION_END = INPUTS%INITIALIZATION_END
      SIM_START_TIME =  INPUTS%SIM_START_TIME
      MAX_NODE_NUMBER = INPUTS%MAX_NODE_NUMBER
      CALL ALLOCATE_NODE_ARRAYS(MAX_NODE_NUMBER + 1000)
      DLC_MULT = INPUTS%DLC_MULT
      HMIN = INPUTS%DLC_THRESHOLD
    ENDIF
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'SET_NETWORK_INPUTS CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION SET_FREEWAY_NETWORK_INPUTS[DLLEXPORT, STDCALL](INPUTS) RESULT(ERRORS)
  USE API_DATA
  USE CAR_FOLLOWING
  USE DRIVERS
  USE GLOBAL_DATA
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  TYPE(FREEWAY_NETWORK_INPUTS) :: INPUTS
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    ERRORS = 0
    LAG_ACCEL = INPUTS%LAG_ACCEL
    LAG_DECEL = INPUTS%LAG_DECEL
    CFRICT = INPUTS%CFRICT
    DEFAULT_HOV_PCT = INPUTS%DEFAULT_HOV_PCT
    FZFOLL = INPUTS%ZFOLL_PITT
    FZFOLL_IDM = INPUTS%ZFOLL_IDM
    FPCFSEP = INPUTS%PITT_SEP
    FIDMSEP = INPUTS%IDM_SEP
    FFSPEED_ADJ(1:10, 1) = INPUTS%FFSPEED_ADJ
    FREEWAY_PCT_COOP = INPUTS%FREEWAY_PCT_COOP
    LC_TIME_FREEWAY = INPUTS%LC_TIME
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'SET_FREEWAY_NETWORK_INPUTS CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION SET_CAR_FOLLOWING_INPUTS[DLLEXPORT, STDCALL](INPUTS) RESULT(ERRORS)
  USE API_DATA
  USE CAR_FOLLOWING
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  TYPE(API_CAR_FOLLOWING) :: INPUTS
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    ERRORS = 0
    ACC_TG = INPUTS%ACC_TG
    ACC_AMAX = INPUTS%ACC_AMAX
    ACC_DMAX = INPUTS%ACC_DMAX
    ACC_K1 = INPUTS%ACC_K1
    ACC_K2 = INPUTS%ACC_K2
    CACC_TG = INPUTS%CACC_TG
    CACC_AMAX = INPUTS%CACC_AMAX
    CACC_DMAX = INPUTS%CACC_DMAX
    CACC_K1 = INPUTS%CACC_K1
    CACC_K2 = INPUTS%CACC_K2
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'SET_CAR_FOLLOWING_INPUTS CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION SET_STREET_NETWORK_INPUTS[DLLEXPORT, STDCALL](INPUTS) RESULT(ERRORS)
  USE API_DATA
  USE CAR_FOLLOWING
  USE DRIVERS
  USE BUS_STATION_DATA
  USE GLOBAL_DATA
  USE DISTRIBUTIONS
  USE PEDS
  USE EVENTS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  TYPE(STREET_NETWORK_INPUTS) :: INPUTS
  INTEGER :: ITYPE, IRAND
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    ERRORS = 0
    ACCEPTABLE_GAP = INPUTS%ACCEPTABLE_GAP
    ACCEPTABLE_LTG = INPUTS%ACCEPTABLE_LTG    
    ACCEPTABLE_RTG = INPUTS%ACCEPTABLE_RTG
    ADDITIONAL_GAP = INPUTS%ADDITIONAL_GAP
    AMBER_DECEL = INPUTS%AMBER_DECEL
    DO ITYPE = 1, 6
      DO IRAND = 1, 10
        DWELL_MULTIPLIER(ITYPE, IRAND) = INPUTS%DWELL_MULTIPLIER(IRAND, ITYPE)
      ENDDO
    ENDDO
    SZFOLL = INPUTS%ZFOLL_PITT
    SZFOLL_IDM = INPUTS%ZFOLL_IDM
    SPCFSEP = INPUTS%PITT_SEP
    SIDMSEP = INPUTS%IDM_SEP
    FFSPEED_ADJ(1:10, 2) = INPUTS%FFSPEED_ADJ
    LC_TIME_STREET = INPUTS%LC_TIME
    LT_JUMPER_PROB = INPUTS%LT_JUMPER_PROB
    LT_LAGGER_PROB = INPUTS%LT_LAGGER_PROB
    SPILLBACK_PROB = INPUTS%SPILLBACK_PROB
    STOP_SPD = INPUTS%STOP_SPD
    STREET_PCT_COOP = INPUTS%STREET_PCT_COOP
    YIELD_SPD = INPUTS%YIELD_SPD
    DRIVER_FAMPCT = INPUTS%DRIVER_FAMPCT
    PDELAY_WEAK = INPUTS%PDELAY_WEAK 
    PDELAY_STRONG = INPUTS%PDELAY_STRONG
    PED_DURATION = INPUTS%PED_DURATION
    QFACTOR = INPUTS%QFACTOR
    STE_MULT = INPUTS%STE_MULT
    TURNSIGNAL_PROB = INPUTS%TURNSIGNAL_PROB
    TURNSIGNAL_DIST = INPUTS%TURNSIGNAL_DIST
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'SET_STREET_NETWORK_INPUTS CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION SET_NUMBER_OF_FREEWAYLINKS[DLLEXPORT, STDCALL](N) RESULT(ERRORS)
  USE FREEWAY_LINKS
  USE FREEWAY_NODES
  USE SIMPARAMS
  USE API_DATA
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    ERRORS = 0
    N_FREEWAY_LINKS = N
    CALL ALLOCATE_FREEWAY_LINK_ARRAYS
    CALL ALLOCATE_FREEWAY_NODE_ARRAYS
    ALLOCATE(FTHRU_PCT(N_FREEWAY_LINKS, N_PERIODS))
    FTHRU_PCT = 0
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'SET_NUMBER_OF_FREEWAYLINKS CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION DEFINE_FREEWAYLINKS[DLLEXPORT, STDCALL](LINK_DATA) RESULT(ERRORS)
  USE API_DATA
  USE FREEWAY_LINKS
  USE FREEWAY_NODES
  USE GLOBAL_DATA
  USE NODE_TABLE
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  TYPE(FREEWAY_LINK) :: LINK_DATA(N_FREEWAY_LINKS)
  INTEGER :: IL, IADD, IAUX, IL2, ILN, NLINKS, INODE
! ----------------------------------------------------------------------
  ERRORS = 0
  IF(TIME_PERIOD .LE. 1) THEN
    DO IL = 1, N_FREEWAY_LINKS
      FUSN(IL) = LINK_DATA(IL)%USN
      FDSN(IL) = LINK_DATA(IL)%DSN
#ifdef DebugVersion
      inode=fdsn(il)
#endif
      NODE_TYPE(FUSN(IL)) = LINK_DATA(IL)%USN_TYPE
      NODE_TYPE(FDSN(IL)) = LINK_DATA(IL)%DSN_TYPE
      IS_USED(FUSN(IL)) = .TRUE.
      IS_USED(FDSN(IL)) = .TRUE.
      IF(NODE_TYPE(FUSN(IL)) .EQ. NT_INTERN) NETCODE(FUSN(IL)) = I_FREEWAY
      IF(NODE_TYPE(FDSN(IL)) .EQ. NT_INTERN) NETCODE(FDSN(IL)) = I_FREEWAY
      FLENGTH(IL) = LINK_DATA(IL)%LENGTH
      FNUMLANES(IL) = LINK_DATA(IL)%FULLLANES
      IF(FNUMLANES(IL) .LT. 0 .OR. FNUMLANES(IL) .GT. 11) THEN
        ERRORS = ERRORS + 1
        WRITE(MSGTEXT, '(A, 2I5)') 'INVALID NUMBER OF FULL LANES ON LINK ', FUSN(IL), FDSN(IL)
        CALL SENDTEXTMSG(M_ERROR)
        WRITE(MSGTEXT, '(A, I2)') '  NUMBER OF FULL LANES = ', FNUMLANES(IL)
        CALL SENDTEXTMSG(M_ERROR)
      ENDIF
      LINKTYPE(IL) = LINK_DATA(IL)%LINKTYPE
      IF(LINKTYPE(IL) .LT. 0 .OR. LINKTYPE(IL) .GT. 2) THEN
        ERRORS = ERRORS + 1
        WRITE(MSGTEXT, '(A, 2I5)') 'INVALID FREEWAY LINK TYPE CODE ON LINK ', FUSN(IL), FDSN(IL)
        CALL SENDTEXTMSG(M_ERROR)
        WRITE(MSGTEXT, '(A, I2)') '  LINK TYPE CODE = ', LINKTYPE(IL)
        CALL SENDTEXTMSG(M_ERROR)
      ENDIF
      MAINLINE_SENDING_LANE(IL) = LINK_DATA(IL)%MAINLINE_SENDING_LANE
      MAINLINE_RECEIVING_LANE(IL) = LINK_DATA(IL)%MAINLINE_RECEIVING_LANE
      OFFRAMP_SENDING_LANE(IL) = LINK_DATA(IL)%OFFRAMP_SENDING_LANE
      OFFRAMP_RECEIVING_LANE(IL) = LINK_DATA(IL)%OFFRAMP_RECEIVING_LANE
      RAMP_MERGE_LINK(IL) = LINK_DATA(IL)%MERGE_DIVERGE_CODE .EQ. 1
      IF(RAMP_MERGE_LINK(IL)) MERGE_APPROACH(FDSN(IL)) = IL
      DIVERGE_LINK(IL) = LINK_DATA(IL)%MERGE_DIVERGE_CODE .EQ. 2
      DO IADD = 1, 3
        IF(LINK_DATA(IL)%ADDDROP_CODE(IADD) .EQ. 0) EXIT
        ADDDROP_CODE(IL, IADD) = LINK_DATA(IL)%ADDDROP_CODE(IADD)
        ADDDROP_LANE(IL, IADD) = LINK_DATA(IL)%ADDDROP_LANE(IADD) 
        ADDDROP_DIST(IL, IADD) = LINK_DATA(IL)%ADDDROP_DIST(IADD)
        IF(LINK_DATA(IL)%ADDDROP_WARN(IADD) .NE. 0) ADDDROP_WARN(IL, IADD) = LINK_DATA(IL)%ADDDROP_WARN(IADD)
      ENDDO
      IF(LINKTYPE(IL) .EQ. 0 .AND. .NOT. RAMP_MERGE_LINK(IL)) THEN
        MAINLINE_APPROACH(FDSN(IL)) = IL
        DO IAUX = 1, N_AUXLANES
          IF(LINK_DATA(IL)%AUXLANEID(IAUX) .EQ. 0) EXIT
          AUX_LANE_ID(IL, IAUX) = LINK_DATA(IL)%AUXLANEID(IAUX)
          AUX_LANE_CODE(IL, IAUX) = LINK_DATA(IL)%AUXLANECODE(IAUX)
          AUX_LANE_LENGTH(IL, IAUX) = LINK_DATA(IL)%AUXLANELENGTH(IAUX)
        ENDDO
      ELSEIF(LINKTYPE(IL) .EQ. 1) THEN
        RAMP_APPROACH(FDSN(IL)) = IL
      ENDIF
      FFREEFLOWSPEED(IL) = LINK_DATA(IL)%FREEFLOWSPEED * MILES2FEET
      FTHRU_PERCENT(IL) = 100
      IF(LINK_DATA(IL)%CFMULT .NE. 0) FCFMULT(IL) = LINK_DATA(IL)%CFMULT
      FFIRST_DETECTOR(IL) = LINK_DATA(IL)%FIRST_DETECTOR
      FGRADE(IL) = LINK_DATA(IL)%GRADE
      TILT(IL) = LINK_DATA(IL)%TILT
      CURVE(IL) = LINK_DATA(IL)%CURVE
      PAVEMENT(IL) = LINK_DATA(IL)%PAVEMENT
      DO ILN = 1, N_FREEWAY_LANES
        IF(LINK_DATA(IL)%LANE_WIDTH(ILN) .NE. 0) FLANE_WIDTH(IL, ILN) = LINK_DATA(IL)%LANE_WIDTH(ILN)
      ENDDO
      IF(LINK_DATA(IL)%SHOULDER_WIDTH .NE. 0) FSHOULDER_WIDTH(IL) = LINK_DATA(IL)%SHOULDER_WIDTH
      BARRIER(IL, 1:2) = LINK_DATA(IL)%BARRIER
      DATASTATION_ID (IL) = LINK_DATA(IL)%DATASTATION_ID
      DATASTATION_LOCATION(IL) = LINK_DATA(IL)%DATASTATION_LOCATION
      TRUCK_CODE(IL) = LINK_DATA(IL)%TRUCK_CODE
      TRUCK_DIR(IL) = LINK_DATA(IL)%TRUCK_DIR
      TRUCK_LANE(IL) = LINK_DATA(IL)%TRUCK_LANE
      ETL_WARN(IL) = LINK_DATA(IL)%ETL_WARN
      FXCLUDE_TYPE(IL, 1:N_FREEWAY_LANES, 1:NTYPES) = LINK_DATA(IL)%EXCLUDE_TYPE(1:N_FREEWAY_LANES, 1:NTYPES)
      MULTIPLIER_EXIT(IL, 1:NTYPES) = LINK_DATA(IL)%MULTIPLIER_EXIT
      ANTICIP_WARNING_DISTANCE(IL) = LINK_DATA(IL)%ANTICIP_WARNING_DISTANCE
      IF(LINK_DATA(IL)%ANTICIP_WARNING_SPEED .EQ. 0) THEN
        ANTICIP_WARNING_SPEED(IL) = NINT(2. / 3. * FFREEFLOWSPEED(IL))
      ELSE
        ANTICIP_WARNING_SPEED(IL) = NINT(LINK_DATA(IL)%ANTICIP_WARNING_SPEED * MILES2FEET)
      ENDIF  
      IF(LINK_DATA(IL)%STARTUP_TIME .NE. 0) FSTARTUP_TIME(IL) = LINK_DATA(IL)%STARTUP_TIME
      IF(LINK_DATA(IL)%NHOV_LANES .NE. 0) THEN
        NHOV_LANES(IL) = LINK_DATA(IL)%NHOV_LANES
        IF(NHOV_LANES(IL) .GT. 3) THEN
          NHOV_LANES(IL) = 3
          WRITE(MSGTEXT, '(A, 2I5)') 'INVALID NUMBER OF HOV LANES SPECIFIED ON LINK', FUSN(IL), FDSN(IL)
          CALL SENDTEXTMSG(M_ERROR)
          WRITE(MSGTEXT, '(A, 2I5)') '  NUMBER OF HOV LANES = ', LINK_DATA(IL)%NHOV_LANES
          CALL SENDTEXTMSG(M_ERROR)
        ENDIF
        HOV_SIDE(IL) = LINK_DATA(IL)%HOV_SIDE
        HOV_TYPE(IL) = LINK_DATA(IL)%HOV_TYPE
        HOV_CODE(IL) = LINK_DATA(IL)%HOV_CODE
        HOV_BEGIN(IL) = LINK_DATA(IL)%HOV_BEGIN
        IF(LINK_DATA(IL)%HOV_END .NE. 0) THEN
          HOV_END(IL) = LINK_DATA(IL)%HOV_END
        ELSE
          HOV_END(IL) = FLENGTH(IL)
        ENDIF
        IF(LINK_DATA(IL)%HOV_WARN .NE. 0) THEN
          HOV_WARN(IL) = LINK_DATA(IL)%HOV_WARN
        ELSE
          HOV_WARN(IL) = 5280
        ENDIF
        IF(LINK_DATA(IL)%HOV_PCT .NE. 0) THEN
          HOV_PCT(IL) = LINK_DATA(IL)%HOV_PCT / 100.
        ELSE
          HOV_PCT(IL) = DEFAULT_HOV_PCT
        ENDIF
      ENDIF
    ENDDO
    DO IL = 1, N_FREEWAY_LINKS
      IF(LINK_DATA(IL)%THRUNODE .NE. 0) THEN
        CALL FIND_FREEWAY_LINK(FDSN(IL), LINK_DATA(IL)%THRUNODE, IL2)
        FTHRU_LINK(IL) = IL2
        IF(IL2 .EQ. 0) THEN
          ERRORS = ERRORS + 1
          WRITE(MSGTEXT, '(A, 2I5)') 'THRU RECEIVING LINK DOES NOT EXIST ', FUSN(IL), FDSN(IL)
          CALL SENDTEXTMSG(M_ERROR)
        ENDIF
      ENDIF
      FTHRU_PERCENT(IL) = 100
      IF(LINK_DATA(IL)%EXITNODE .NE. 0) THEN
        FTHRU_PERCENT(IL) = LINK_DATA(IL)%THRUPCT
        CALL FIND_FREEWAY_LINK(FDSN(IL), LINK_DATA(IL)%EXITNODE, IL2)
        OFFRAMP_LINK(IL) = IL2
        IF(IL2 .EQ. 0) THEN
          ERRORS = ERRORS + 1
          WRITE(MSGTEXT, '(A, 2I5)') 'OFF-RAMP LINK DOES NOT EXIST ', FUSN(IL), FDSN(IL)
          CALL SENDTEXTMSG(M_ERROR)
        ENDIF
        OFFRAMP_WARN_DISTANCE(IL) = LINK_DATA(IL)%OFFRAMP_WARN_DISTANCE
        SAVE_WARN1(IL) = OFFRAMP_WARN_DISTANCE(IL)
        IF(LINK_DATA(IL)%HOV_OFFRAMP_WARN_DISTANCE .GT. 0) THEN
          HOV_OFFRAMP_WARN_DISTANCE(IL) = LINK_DATA(IL)%HOV_OFFRAMP_WARN_DISTANCE
        ELSE
          HOV_OFFRAMP_WARN_DISTANCE(IL) = OFFRAMP_WARN_DISTANCE(IL)
        ENDIF
        SAVE_WARN2(IL) = HOV_OFFRAMP_WARN_DISTANCE(IL)
      ENDIF
      IF(NODE_TYPE(FUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(FDSN(IL)) .NE. NT_EXTERN) THEN
        IF(FTHRU_PERCENT(IL) .LT. 0) THEN
          ERRORS = ERRORS + 1
          WRITE(MSGTEXT, '(A, 2I5)') 'NEGATIVE ENTRY FOR THRU PERCENTAGE ON LINK ', FUSN(IL), FDSN(IL)
          CALL SENDTEXTMSG(M_ERROR)
          !WRITE(MSGTEXT, '(A, I6)') '  THRU PERCENTAGE = ', FTHRU_PERCENT(IL)
          !CALL SENDTEXTMSG(M_ERROR)
        ELSE
          FTHRU_PCT(IL, 1) = FTHRU_PERCENT(IL)
        ENDIF
      ENDIF
    ENDDO
    NLINKS = N_FREEWAY_LINKS
    DO IL = 1, NLINKS
      IF(NODE_TYPE(FDSN(IL)) .EQ. NT_INTERN .AND. FTHRU_LINK(IL) .EQ. 0) THEN
        CALL GET_NEXT_EXTERNAL_NODE_ID(INODE)
        CALL REALLOCATE_FREEWAY_LINK_ARRAYS(1)
        FUSN(N_FREEWAY_LINKS) = FDSN(IL)
        FDSN(N_FREEWAY_LINKS) = INODE
        LINKTYPE(N_FREEWAY_LINKS) = LINKTYPE(IL)
        FTHRU_LINK(IL) = N_FREEWAY_LINKS
        FNUMLANES(N_FREEWAY_LINKS) = FNUMLANES(IL)
        IF(LINKTYPE(N_FREEWAY_LINKS) .EQ. 0) THEN
          MAINLINE_APPROACH(FDSN(N_FREEWAY_LINKS)) = IL
        ELSE
          RAMP_APPROACH(FDSN(N_FREEWAY_LINKS)) = IL
        ENDIF
        MAINLINE_SENDING_LANE(N_FREEWAY_LINKS) = 1
        MAINLINE_RECEIVING_LANE(N_FREEWAY_LINKS) = 1
      ENDIF
    ENDDO  
  ELSE
    DO IL = 1, N_FREEWAY_LINKS
      IF(NODE_TYPE(FUSN(IL)) .EQ. NT_EXTERN .OR. NODE_TYPE(FDSN(IL)) .EQ. NT_EXTERN) EXIT
      IF(LINK_DATA(IL)%EXITNODE .NE. 0) THEN
        FTHRU_PCT(IL, TIME_PERIOD) = LINK_DATA(IL)%THRUPCT
        IF(FTHRU_PERCENT(IL) .NE. LINK_DATA(IL)%THRUPCT) THEN
          WRITE25(IL, TIME_PERIOD) = .TRUE.
          FTHRU_PERCENT(IL) = LINK_DATA(IL)%THRUPCT
        ENDIF
        IF(NHOV_LANES(IL) .GT. 0) THEN
          HOV_CODE(IL) = LINK_DATA(IL)%HOV_CODE
          IF(LINK_DATA(IL)%HOV_PCT .NE. HOV_PCT(IL)) THEN
            WRITE33(IL, TIME_PERIOD) = .TRUE.
            HOV_PCT(IL) = LINK_DATA(IL)%HOV_PCT / 100.
          ENDIF
        ENDIF
      ENDIF
    ENDDO
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION SET_NUMBER_OF_STREETLINKS[DLLEXPORT, STDCALL](N) RESULT(ERRORS)
  USE STREET_LINKS
  USE STREET_NODES
  USE SIMPARAMS
  USE API_DATA
  USE GLOBAL_DATA
  USE EV_DATA
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    ERRORS = 0
    N_STREET_LINKS = N
    CALL ALLOCATE_STREET_LINK_ARRAYS
    CALL ALLOCATE_STREET_NODE_ARRAYS
    CALL ALLOCATE_EV_DATA_ARRAYS
    CALL ALLOCATE_API_DATA(N_STREET_LINKS, N_PERIODS)
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'SET_NUMBER_OF_STREETLINKS CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END 
  
! ==================================================================================================
  INTEGER FUNCTION DEFINE_STREETLINKS[DLLEXPORT, STDCALL](LINK_DATA) RESULT(ERRORS)
  USE API_DATA
  USE STREET_LINKS
  USE NODE_TABLE
  USE GLOBAL_DATA
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  TYPE(STREET_LINK) :: LINK_DATA(N_STREET_LINKS)
  INTEGER :: IL, IL2, ILN, NLINKS, INODE, ITW, NLANE, ILANE, I
  INTEGER :: LPCT, TPCT, RPCT, LDPCT, RDPCT, SUM, ERROR, NODE
! ----------------------------------------------------------------------
  ERRORS = 0 
  IF(TIME_PERIOD .LE. 1) THEN
    DO IL = 1, N_STREET_LINKS
      SUSN(IL) = LINK_DATA(IL)%USN 
      SDSN(IL) = LINK_DATA(IL)%DSN
      NODE_TYPE(SUSN(IL)) = LINK_DATA(IL)%USN_TYPE
      NODE_TYPE(SDSN(IL)) = LINK_DATA(IL)%DSN_TYPE
      IF(NODE_TYPE(SDSN(IL)) .EQ. NT_EXTERN) CYCLE
      LT_SPEED(IL) = LINK_DATA(IL)%LT_SPEED * MILES2FEET
      RT_SPEED(IL) = LINK_DATA(IL)%RT_SPEED * MILES2FEET
      LT_LIMITED_SPEED_DIST(IL) = LINK_DATA(IL)%LT_LIMITED_SPEED_DIST 
      RT_LIMITED_SPEED_DIST(IL) = LINK_DATA(IL)%RT_LIMITED_SPEED_DIST
      LPCT = LINK_DATA(IL)%LEFTPCT
      TPCT = LINK_DATA(IL)%THRUPCT
      RPCT = LINK_DATA(IL)%RIGHTPCT
      LDPCT = LINK_DATA(IL)%LDIAGPCT
      RDPCT = LINK_DATA(IL)%RDIAGPCT
      SUM = LPCT + TPCT + RPCT + LDPCT + RDPCT
      IF(SUM .EQ. 0) THEN
        ERRORS = ERRORS + 1
        WRITE(MSGTEXT, '(A, 2I5)') 'NO TURN PERCENTAGES ENTERED FOR LINK ', SUSN(IL), SDSN(IL)
        CALL SENDTEXTMSG(M_ERROR)
      ELSEIF(SUM .NE. 100) THEN
        LPCT = 100 * FLOAT(LPCT) / SUM
        TPCT = 100 * FLOAT(TPCT) / SUM
        RPCT = 100 * FLOAT(RPCT) / SUM
        LDPCT = 100 * FLOAT(LDPCT) / SUM
        RDPCT = 100 * FLOAT(RDPCT) / SUM
        SUM = LPCT + TPCT + RPCT + LDPCT + RDPCT
        ERROR = 100 - SUM
        IF(ERROR .NE. 0) THEN
          IF(LPCT .NE. 0) THEN
            LPCT = LPCT + ERROR
          ELSEIF(TPCT .NE. 0) THEN
            TPCT = TPCT + ERROR
          ELSEIF(RPCT .NE. 0) THEN
            RPCT = RPCT + ERROR
          ELSEIF(LDPCT .NE. 0) THEN
            LDPCT = LDPCT + ERROR
          ELSEIF(RDPCT .NE. 0) THEN
            RDPCT = RDPCT + ERROR
          ENDIF
        ENDIF
      ENDIF

      IF(NODE_TYPE(SDSN(IL)) .NE. NT_INTERN) THEN
        SUM = LINK_DATA(IL)%LEFTNODE + LINK_DATA(IL)%THRUNODE + LINK_DATA(IL)%RIGHTNODE + LINK_DATA(IL)%LDIAGNODE + LINK_DATA(IL)%RDIAGNODE
        IF(SUM .NE. 0) THEN
          ERRORS = ERRORS + 1
          WRITE(MSGTEXT, '(A, 2I8)') 'RECEIVING LINK SPECIFIED FOR INTERFACE OR EXIT LINK ', SUSN(IL), SDSN(IL)
          CALL SENDTEXTMSG(M_ERROR)
        ENDIF
      ENDIF
      IF(NODE_TYPE(SUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(SDSN(IL)) .NE. NT_EXTERN) THEN
        IF(.NOT. ALLOCATED(LEFT_PCT)) CALL ALLOCATE_API_DATA(N_STREET_LINKS, N_PERIODS)
        LEFT_PCT(IL, 1) = LPCT
        STHRU_PCT(IL, 1) = TPCT
        RIGHT_PCT(IL, 1) = RPCT
        LDIAG_PCT(IL, 1) = LDPCT
        RDIAG_PCT(IL, 1) = RDPCT
      ENDIF
      IS_USED(SUSN(IL)) = .TRUE.
      IS_USED(SDSN(IL)) = .TRUE.
      IF(NODE_TYPE(SUSN(IL)) .EQ. NT_INTERN) NETCODE(SUSN(IL)) = I_STREET
      IF(NODE_TYPE(SDSN(IL)) .EQ. NT_INTERN) NETCODE(SDSN(IL)) = I_STREET
      SLENGTH(IL) = LINK_DATA(IL)%LENGTH
      SNUMLANES(IL) = LINK_DATA(IL)%FULLLANES
      IF(SNUMLANES(IL) .LT. 0 .OR. SNUMLANES(IL) .GT. 20) THEN
        ERRORS = ERRORS + 1
        WRITE(MSGTEXT, '(A, 2I5)') 'INVALID NUMBER OF FULL LANES ON LINK ', SUSN(IL), SDSN(IL)
        CALL SENDTEXTMSG(M_ERROR)
        WRITE(MSGTEXT, '(A, I2)') '  NUMBER OF FULL LANES = ', SNUMLANES(IL)
        CALL SENDTEXTMSG(M_ERROR)
      ENDIF
      NUMBER_LEFTPOCKETS(IL) = LINK_DATA(IL)%LEFTTURNBAYS
      NUMBER_RIGHTPOCKETS(IL) = LINK_DATA(IL)%RIGHTTURNBAYS
      FIRST_FULL_LANE(IL) = NUMBER_RIGHTPOCKETS(IL) + 1
      LAST_FULL_LANE(IL) = FIRST_FULL_LANE(IL) + SNUMLANES(IL) - 1
      TOTAL_LANES(IL) = SNUMLANES(IL) + NUMBER_LEFTPOCKETS(IL) + NUMBER_RIGHTPOCKETS(IL)
      IF(LINK_DATA(IL)%DISTRIBUTION_CODE .NE. 0) THEN
        LINKTYPE_CODE(IL) = LINK_DATA(IL)%DISTRIBUTION_CODE
      ELSE
        LINKTYPE_CODE(IL) = 1
      ENDIF
      SFREEFLOWSPEED(IL) = LINK_DATA(IL)%FREEFLOWSPEED * MILES2FEET
      CHANNELIZATION(IL, 1:N_STREET_LANES) = LINK_DATA(IL)%CHANNELIZATION
      LANE_LENGTH(IL, 1:N_STREET_LANES) = LINK_DATA(IL)%LANE_LENGTH
      LANE_LENGTH(IL, FIRST_FULL_LANE(IL):LAST_FULL_LANE(IL)) = SLENGTH(IL) 
      PED_CODE(IL) = LINK_DATA(IL)%PED_CODE
      SGRADE(IL) = LINK_DATA(IL)%GRADE
      IF(LINK_DATA(IL)%STARTUP_DELAY .NE. 0) SSTARTUP_TIME(IL) = LINK_DATA(IL)%STARTUP_DELAY
      IF(SSTARTUP_TIME(IL) .LT. 0 .OR. SSTARTUP_TIME(IL) .GT. 9.9) THEN
        WRITE(MSGTEXT, '(A, 2I5)') 'INVALID MEAN STARTUP LOST TIME ON LINK ', SUSN(IL), SDSN(IL)
        CALL SENDTEXTMSG(M_ERROR)
        WRITE(MSGTEXT, '(A, F5.1)') '  VALUE = ', SSTARTUP_TIME(IL)
        CALL SENDTEXTMSG(M_ERROR)
      ENDIF
      IF(LINK_DATA(IL)%DISCHARGE_HDWY .NE. 0) QDISCHARGE_HDWY(IL) = LINK_DATA(IL)%DISCHARGE_HDWY
      SALIGNMENT_LANE(IL) = LINK_DATA(IL)%LANE1
      STHRU_ALIGNMENT_LANE(IL) = LINK_DATA(IL)%LANE2
      IF(LINK_DATA(IL)%CFMULT .NE. 0) SCFMULT(IL) = LINK_DATA(IL)%CFMULT
      IF(LINK_DATA(IL)%SIGHT_DIST .NE. 0) SIGHT_DIST(IL) = LINK_DATA(IL)%SIGHT_DIST
      RTOR(IL) = LINK_DATA(IL)%RTOR .EQ. 0
      SFIRST_DETECTOR(IL) = LINK_DATA(IL)%FIRST_DETECTOR
      SGRADE(IL) = LINK_DATA(IL)%GRADE
      CROSS_WALK(IL) = LINK_DATA(IL)%CROSS_WALK
      DO ILN = 1, TOTAL_LANES(IL)
        IF(LINK_DATA(IL)%LANE_WIDTH(ILN) .NE. 0) THEN
          SLANE_WIDTH(IL, ILN) = LINK_DATA(IL)%LANE_WIDTH(ILN)
        ELSE
          SLANE_WIDTH(IL, ILN) = STD_WIDTH
        ENDIF
      ENDDO
      IF(LINK_DATA(IL)%SHOULDER_WIDTH .NE. 0) SSHOULDER_WIDTH(IL) = LINK_DATA(IL)%SHOULDER_WIDTH
      STE_FREQ(IL) = LINK_DATA(IL)%STE_FREQ
      STE_DURATION(IL) = LINK_DATA(IL)%STE_DURATION
      CENTROID(IL) = LINK_DATA(IL)%CENTROID
      CENTROID_LABEL(IL) = LINK_DATA(IL)%CENTROID_LABEL
      SXCLUDE_TYPE(IL, 1:N_STREET_LANES, 1:NTYPES) = LINK_DATA(IL)%EXCLUDE_TYPE(1:N_STREET_LANES, 1:NTYPES)
      MULTIPLIER_LEFT(IL, 1:NTYPES) = LINK_DATA(IL)%MULTIPLIER_LEFT
      MULTIPLIER_THRU(IL, 1:NTYPES) = LINK_DATA(IL)%MULTIPLIER_THRU
      MULTIPLIER_RIGHT(IL, 1:NTYPES) = LINK_DATA(IL)%MULTIPLIER_RIGHT
      MULTIPLIER_LDIAG(IL, 1:NTYPES) = LINK_DATA(IL)%MULTIPLIER_LDIAG
      MULTIPLIER_RDIAG(IL, 1:NTYPES) = LINK_DATA(IL)%MULTIPLIER_RDIAG
      LEFT_PERCENT(IL) = LPCT
      STHRU_PERCENT(IL) = TPCT
      RIGHT_PERCENT(IL) = RPCT
      LDIAG_PERCENT(IL) = LDPCT
      RDIAG_PERCENT(IL) = RDPCT
    ENDDO
    DO IL = 1, N_STREET_LINKS
      IF(LINK_DATA(IL)%THRUNODE .NE. 0) THEN
        NODE = LINK_DATA(IL)%THRUNODE
        IF(NODE_TYPE(NODE) .NE. NT_EXTERN) THEN
          CALL FIND_STREET_LINK(SDSN(IL), LINK_DATA(IL)%THRUNODE, IL2)
          STHRU_LINK(IL) = IL2
          IF(IL2 .EQ. 0) THEN
            ERRORS = ERRORS + 1
            WRITE(MSGTEXT, '(A, 2I4)') 'THRU RECEIVING LINK DOES NOT EXIST FOR LINK ', SUSN(IL), SDSN(IL)
            CALL SENDTEXTMSG(M_ERROR)
            WRITE(MSGTEXT, '(A, 2I5)') '  THRU RECEIVING LINK ', SDSN(IL), NODE
            CALL SENDTEXTMSG(M_ERROR)
          ENDIF
        ENDIF
      ENDIF
      IF(LINK_DATA(IL)%LEFTNODE .NE. 0) THEN
        NODE = LINK_DATA(IL)%LEFTNODE
        IF(NODE_TYPE(NODE) .NE. NT_EXTERN) THEN
          CALL FIND_STREET_LINK(SDSN(IL), LINK_DATA(IL)%LEFTNODE, IL2)
          LEFT_LINK(IL) = IL2
          IF(IL2 .EQ. 0) THEN
            ERRORS = ERRORS + 1
            WRITE(MSGTEXT, '(A, 2I4)') 'LEFT RECEIVING LINK DOES NOT EXIST FOR LINK ', SUSN(IL), SDSN(IL)
            CALL SENDTEXTMSG(M_ERROR)
            WRITE(MSGTEXT, '(A, 2I5)') '  LEFT RECEIVING LINK ', SDSN(IL), NODE
            CALL SENDTEXTMSG(M_ERROR)
          ENDIF
        ENDIF
      ENDIF
      IF(LINK_DATA(IL)%RIGHTNODE .NE. 0) THEN
        NODE = LINK_DATA(IL)%RIGHTNODE
        IF(NODE_TYPE(NODE) .NE. NT_EXTERN) THEN
          CALL FIND_STREET_LINK(SDSN(IL), LINK_DATA(IL)%RIGHTNODE, IL2)
          RIGHT_LINK(IL) = IL2
          IF(IL2 .EQ. 0) THEN
            ERRORS = ERRORS + 1
            WRITE(MSGTEXT, '(A, 2I4)') 'RIGHT RECEIVING LINK DOES NOT EXIST FOR LINK ', SUSN(IL), SDSN(IL)
            CALL SENDTEXTMSG(M_ERROR)
            WRITE(MSGTEXT, '(A, 2I5)') '  RIGHT RECEIVING LINK ', SDSN(IL), NODE
            CALL SENDTEXTMSG(M_ERROR)
          ENDIF
        ENDIF
      ENDIF
      IF(LINK_DATA(IL)%LDIAGNODE .NE. 0) THEN
        NODE = ABS(LINK_DATA(IL)%LDIAGNODE)
        IF(NODE_TYPE(NODE) .NE. NT_EXTERN) THEN
          CALL FIND_STREET_LINK(SDSN(IL), ABS(LINK_DATA(IL)%LDIAGNODE), IL2)
          LEFT_DIAG_LINK(IL) = IL2
          IF(IL2 .EQ. 0) THEN
            ERRORS = ERRORS + 1
            WRITE(MSGTEXT, '(A, 2I4)') 'LEFT DIAGONAL RECEIVING LINK DOES NOT EXIST FOR LINK ', SUSN(IL), SDSN(IL)
            CALL SENDTEXTMSG(M_ERROR)
            WRITE(MSGTEXT, '(A, 2I5)') '  LEFT DIAGONAL RECEIVING LINK ', SDSN(IL), NODE
            CALL SENDTEXTMSG(M_ERROR)
          ENDIF
        ENDIF  
      ENDIF
      IF(LINK_DATA(IL)%RDIAGNODE .NE. 0) THEN
        NODE = LINK_DATA(IL)%RDIAGNODE
        IF(NODE_TYPE(NODE) .NE. NT_EXTERN) THEN
          CALL FIND_STREET_LINK(SDSN(IL), ABS(LINK_DATA(IL)%RDIAGNODE), IL2)
          RIGHT_DIAG_LINK(IL) = IL2
          IF(IL2 .EQ. 0) THEN
            ERRORS = ERRORS + 1
            WRITE(MSGTEXT, '(A, 2I4)') 'RIGHT DIAGONAL RECEIVING LINK DOES NOT EXIST FOR LINK ', SUSN(IL), SDSN(IL)
            CALL SENDTEXTMSG(M_ERROR)
            WRITE(MSGTEXT, '(A, 2I5)') '  RIGHT DIAGONAL RECEIVING LINK ', SDSN(IL), NODE
            CALL SENDTEXTMSG(M_ERROR)
          ENDIF
        ENDIF
      ENDIF
      IF(LINK_DATA(IL)%OPPOSENODE .NE. 0) THEN
        CALL FIND_STREET_LINK(LINK_DATA(IL)%OPPOSENODE, SDSN(IL), IL2)
        OPPOSE_LINK(IL) = IL2
        IF(IL2 .EQ. 0) THEN
          ERRORS = ERRORS + 1
          WRITE(MSGTEXT, '(A, 2I4)') 'LINK OPPOSING LEFT TURNS DOES NOT EXIST FOR LINK ', SUSN(IL), SDSN(IL)
          CALL SENDTEXTMSG(M_ERROR)
          WRITE(MSGTEXT, '(A, 2I5)') '  OPPOSING LINK ', LINK_DATA(IL)%OPPOSENODE, SDSN(IL)
          CALL SENDTEXTMSG(M_ERROR)
        ENDIF
      ENDIF
      
      !Define the traditional lane numbers.
      NLANE = 0
      DO ILANE = NUMBER_RIGHTPOCKETS(IL), 1, -1
        NLANE = NLANE + 1
        LANE_NUMBERS(IL, NLANE) = 8 - NUMBER_LEFTPOCKETS(IL) - ILANE
      ENDDO
      IF(RTW_EXIT_POINT(IL) .GT. 0) THEN
        DO I = FIRST_FULL_LANE(IL), LAST_FULL_LANE(IL)
          LANE_NUMBERS(IL, I) = I - 1
        ENDDO
        LANE_NUMBERS(IL, 1) = 7 - NUMBER_LEFTPOCKETS(IL)
      ELSE
        DO ILANE = FIRST_FULL_LANE(IL), LAST_FULL_LANE(IL)
          NLANE = NLANE + 1
          LANE_NUMBERS(IL, NLANE) = ILANE - NUMBER_RIGHTPOCKETS(IL)
        ENDDO
      ENDIF
      DO ILANE = 1, NUMBER_LEFTPOCKETS(IL)
        NLANE = NLANE + 1
        LANE_NUMBERS(IL, NLANE) = 8 - ILANE
      ENDDO
    
    ENDDO
    NLINKS = N_STREET_LINKS
    DO IL = 1, NLINKS
      IF(NODE_TYPE(SDSN(IL)) .EQ. NT_INTERN) THEN
        SUM = LEFT_LINK(IL) + STHRU_LINK(IL) + RIGHT_LINK(IL) + LEFT_DIAG_LINK(IL) + RIGHT_DIAG_LINK(IL)
        IF(SUM .EQ. 0) THEN
          CALL GET_NEXT_EXTERNAL_NODE_ID(INODE)
          CALL REALLOCATE_STREET_LINK_ARRAYS(1)
          SUSN(N_STREET_LINKS) = SDSN(IL)
          SDSN(N_STREET_LINKS) = INODE
          SNUMLANES(N_STREET_LINKS) = SNUMLANES(IL)
          FIRST_FULL_LANE(N_STREET_LINKS) = 1
          LAST_FULL_LANE(N_STREET_LINKS) = SNUMLANES(IL)
          TOTAL_LANES(N_STREET_LINKS) = SNUMLANES(IL) 
          SFREEFLOWSPEED(N_STREET_LINKS) = SFREEFLOWSPEED(IL)
          LINKTYPE_CODE(N_STREET_LINKS) = 1
          STHRU_LINK(IL) = N_STREET_LINKS
          LEFT_PERCENT(IL) = 0
          STHRU_PERCENT(IL) = 100
          RIGHT_PERCENT(IL) = 0
          LDIAG_PERCENT(IL) = 0
          RDIAG_PERCENT(IL) = 0
          LEFT_PCT(IL, 1) = 0
          STHRU_PCT(IL, 1) = 100
          RIGHT_PCT(IL, 1) = 0
          LDIAG_PCT(IL, 1) = 0
          RDIAG_PCT(IL, 1) = 0
          MULTIPLIER_LEFT(IL, 1:NTYPES) = 1.0
          MULTIPLIER_THRU(IL, 1:NTYPES) = 1.0
          MULTIPLIER_RIGHT(IL, 1:NTYPES) = 1.0
          MULTIPLIER_LDIAG(IL, 1:NTYPES) = 1.0
          MULTIPLIER_RDIAG(IL, 1:NTYPES) = 1.0
        ENDIF
      ENDIF
    ENDDO 
  ELSE
    DO IL = 1, N_STREET_LINKS
      IF(NODE_TYPE(IL) .EQ. NT_EXTERN .OR. SDSN(IL) .GE. 8000) EXIT
      LPCT = LINK_DATA(IL)%LEFTPCT
      TPCT = LINK_DATA(IL)%THRUPCT
      RPCT = LINK_DATA(IL)%RIGHTPCT
      LDPCT = LINK_DATA(IL)%LDIAGPCT
      RDPCT = LINK_DATA(IL)%RDIAGPCT
      SUM = LPCT + TPCT + RPCT + LDPCT + RDPCT
      IF(SUM .EQ. 0) CYCLE
      IF(SUM .NE. 100) THEN
        LPCT = 100 * FLOAT(LPCT) / SUM
        TPCT = 100 * FLOAT(TPCT) / SUM
        RPCT = 100 * FLOAT(RPCT) / SUM
        LDPCT = 100 * FLOAT(LDPCT) / SUM
        RDPCT = 100 * FLOAT(RDPCT) / SUM
        SUM = LPCT + TPCT + RPCT + LDPCT + RDPCT
        ERROR = 100 - SUM
        IF(ERROR .NE. 0) THEN
          IF(LPCT .NE. 0) THEN
            LPCT = LPCT + ERROR
          ELSEIF(TPCT .NE. 0) THEN
            TPCT = TPCT + ERROR
          ELSEIF(RPCT .NE. 0) THEN
            RPCT = RPCT + ERROR
          ELSEIF(LDPCT .NE. 0) THEN
            LDPCT = LDPCT + ERROR
          ELSEIF(RDPCT .NE. 0) THEN
            RDPCT = RDPCT + ERROR
          ENDIF
        ENDIF
      ENDIF
      IF(LEFT_PERCENT(IL) .NE. LPCT .OR. STHRU_PERCENT(IL) .NE. TPCT .OR. &
         RIGHT_PERCENT(IL) .NE. RPCT .OR. LDIAG_PERCENT(IL) .NE. LDPCT .OR. RDIAG_PERCENT(IL) .NE. RDPCT) THEN
        WRITE21(IL, TIME_PERIOD) = .TRUE.
        LEFT_PERCENT(IL) = LPCT
        STHRU_PERCENT(IL) = TPCT
        RIGHT_PERCENT(IL) = RPCT
        LDIAG_PERCENT(IL) = LDPCT
        RDIAG_PERCENT(IL) = RDPCT
        LEFT_PCT(IL, TIME_PERIOD) = LPCT
        STHRU_PCT(IL, TIME_PERIOD) = TPCT
        RIGHT_PCT(IL, TIME_PERIOD) = RPCT
        LDIAG_PCT(IL, TIME_PERIOD) = LDPCT
        RDIAG_PCT(IL, TIME_PERIOD) = RDPCT
      ENDIF
    ENDDO
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION SET_NUMBER_OF_ENTRYNODES[DLLEXPORT, STDCALL](NS, NF) RESULT(ERRORS)
  USE API_DATA
  USE FLOWDATA_MOD
  USE SIMPARAMS
  USE TEXT
  !USE STREET_LINKS
  !USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NS, NF
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    ERRORS = 0
    NUMBER_OF_ENTRYNODES = NS + NF
    !IF(NS .GT. 0) CALL REALLOCATE_STREET_LINK_ARRAYS(NS)
    !IF(NF .GT. 0) CALL REALLOCATE_FREEWAY_LINK_ARRAYS(NF)
    ALLOCATE(FLOW_RATE(MAX_NODE_NUMBER+1:MAX_NODE_NUMBER+1000, N_PERIODS))
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'SET_NUMBER_OF_ENTRYNODES CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION DEFINE_ENTRYNODES[DLLEXPORT, STDCALL](TYPED, ERLA, MNSEP, ENTRY_DATA) RESULT(ERRORS)
  USE API_DATA
  USE ENTRYNODE_DATA
  USE FLOWDATA_MOD
  USE SIMPARAMS
  USE FREEWAY_LINKS
  USE STREET_LINKS
  USE GLOBAL_DATA
  USE FREEWAY_NODES
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: TYPED, ERLA
  REAL, INTENT(IN) :: MNSEP
  TYPE(ENTRY_NODE_DATA) :: ENTRY_DATA(NUMBER_OF_ENTRYNODES)
  INTEGER :: I, INODE, IL, START, END, INDEX, NLINKS, NLANES, ITYPE
  LOGICAL :: FOUND, FIRSTCALL = .TRUE.
! ----------------------------------------------------------------------
  IF(N_FREEWAY_LINKS + N_STREET_LINKS .EQ. 0) THEN
    ERRORS = 1
  ELSE
    ERRORS = 0
    TYPEDIST = TYPED
    ERLANGA = ERLA
    MINSEP = MNSEP
    IF(TIME_PERIOD .LE. 1 .AND. FIRSTCALL) THEN
      FIRSTCALL = .FALSE.
      CALL ALLOCATE_ENTRYNODE_DATA
      DO I = 1, NUMBER_OF_ENTRYNODES
        CALL GET_NEXT_EXTERNAL_NODE_ID(INODE)
        FLOW_RATE(INODE, 1) = ENTRY_DATA(I)%FLOWRATE
        ENTRYLINK(I)%TIME(1) = 0
        ENTRYLINK(I)%FLOW(1) = ENTRY_DATA(I)%FLOWRATE
        ENTRYLINK(I)%TIME(2) = TPSECONDS(1) / 60.
        ENTRYLINK(I)%FLOW(2) = ENTRY_DATA(I)%FLOWRATE
        ENTRYLINK(I)%INDEX = 2
        FOUND = .FALSE.
        NLINKS = N_FREEWAY_LINKS
        DO IL = 1, NLINKS
          IF(FUSN(IL) .EQ. ENTRY_DATA(I)%NODE_ID) THEN
            CALL REALLOCATE_FREEWAY_LINK_ARRAYS(1)
            FUSN(N_FREEWAY_LINKS) = INODE
            FDSN(N_FREEWAY_LINKS) = FUSN(IL)
            LINKTYPE(N_FREEWAY_LINKS) = LINKTYPE(IL)
            FTHRU_LINK(N_FREEWAY_LINKS) = IL
            FNUMLANES(N_FREEWAY_LINKS) = FNUMLANES(IL)
            FFREEFLOWSPEED(N_FREEWAY_LINKS) = FFREEFLOWSPEED(IL)
            MAINLINE_RECEIVING_LANE(N_FREEWAY_LINKS) = 1
            FTHRU_PERCENT(N_FREEWAY_LINKS) = 100
            MULTIPLIER_EXIT(N_FREEWAY_LINKS, 1:NTYPES) = 1.0
            IF(LINKTYPE(N_FREEWAY_LINKS) .EQ. 0) THEN
              MAINLINE_APPROACH(FUSN(IL)) = N_FREEWAY_LINKS
            ELSE
              RAMP_APPROACH(FUSN(IL)) = N_FREEWAY_LINKS
            ENDIF
            ENTRYLINK(I)%LINKID = N_FREEWAY_LINKS
            ENTRYLINK(I)%UP = INODE
            ENTRYLINK(I)%DOWN = FDSN(N_FREEWAY_LINKS)
            ENTRYLINK(I)%NETWORK = I_FREEWAY
            NLANES = FNUMLANES(IL)
            NLANES = MAX(NLANES, 1)
            NLANES = MIN(NLANES, 5)
            IF(SUM(ENTRY_DATA(I)%LANE_PCT) .EQ. 100) THEN
              LANE_PCT(INODE, 1:NLANES) = ENTRY_DATA(I)%LANE_PCT(1:NLANES) / 100.
            ELSE
              LANE_PCT(INODE, 1:NLANES) = 1.0 / NLANES
              LANE_PCT(INODE, 1) = LANE_PCT(INODE, 1) + 1.0 - SUM(LANE_PCT(INODE, 1:NLANES))
            ENDIF
            MAINLINE_SENDING_LANE(N_FREEWAY_LINKS) = 1
            MAINLINE_RECEIVING_LANE(N_FREEWAY_LINKS) = 1
            FOUND = .TRUE.
            EXIT
          ENDIF
        ENDDO
        IF(.NOT. FOUND) THEN
          NLINKS = N_STREET_LINKS
          DO IL = 1, NLINKS
            IF(SUSN(IL) .EQ. ENTRY_DATA(I)%NODE_ID) THEN
              CALL REALLOCATE_STREET_LINK_ARRAYS(1)
              SUSN(N_STREET_LINKS) = INODE
              SDSN(N_STREET_LINKS) = SUSN(IL)
              STHRU_LINK(N_STREET_LINKS) = IL
              STHRU_PERCENT(N_STREET_LINKS) = 100
              MULTIPLIER_LEFT(N_STREET_LINKS, 1:NTYPES) = 1.0
              MULTIPLIER_THRU(N_STREET_LINKS, 1:NTYPES) = 1.0
              MULTIPLIER_RIGHT(N_STREET_LINKS, 1:NTYPES) = 1.0
              MULTIPLIER_LDIAG(N_STREET_LINKS, 1:NTYPES) = 1.0
              MULTIPLIER_RDIAG(N_STREET_LINKS, 1:NTYPES) = 1.0
              SNUMLANES(N_STREET_LINKS) = SNUMLANES(IL)
              FIRST_FULL_LANE(N_STREET_LINKS) = 1
              LAST_FULL_LANE(N_STREET_LINKS) = SNUMLANES(IL)
              SALIGNMENT_LANE(N_STREET_LINKS) = 1
              STHRU_ALIGNMENT_LANE(N_STREET_LINKS) = FIRST_FULL_LANE(IL)
              TOTAL_LANES(N_STREET_LINKS) = SNUMLANES(IL) 
              SFREEFLOWSPEED(N_STREET_LINKS) = SFREEFLOWSPEED(IL)
              LINKTYPE_CODE(N_STREET_LINKS) = 1
              ENTRYLINK(I)%LINKID = N_STREET_LINKS
              ENTRYLINK(I)%UP = INODE
              ENTRYLINK(I)%DOWN = SDSN(N_STREET_LINKS)
              ENTRYLINK(I)%NETWORK = I_STREET
              NLANES = MAX(SNUMLANES(IL), 1)
              NLANES = MIN(SNUMLANES(IL), 5)
              IF(SUM(ENTRY_DATA(I)%LANE_PCT) .NE. 0) THEN
                LANE_PCT(INODE, 1:NLANES) = ENTRY_DATA(I)%LANE_PCT(1:NLANES) / 100.
              ELSE
                LANE_PCT(INODE, 1:NLANES) = 1.0 / NLANES
                LANE_PCT(INODE, 1) = LANE_PCT(INODE, 1) + 1.0 - SUM(LANE_PCT(INODE, 1:NLANES))
              ENDIF
              FOUND = .TRUE.
              EXIT
            ENDIF
          ENDDO
        ENDIF
        IF(FOUND) THEN
          ENTRYNODE_IS_USED(INODE) = .TRUE.
          FLOWRATE(INODE) = ENTRY_DATA(I)%FLOWRATE
          TRUCK_PCT(INODE) = ENTRY_DATA(I)%TRUCK_PCT
          CARPOOL_PCT(INODE) = ENTRY_DATA(I)%CARPOOL_PCT
          HOV_VIOLATOR_PCT(INODE) = ENTRY_DATA(I)%HOV_VIOLATORS_PER10000 / 10000.
        ENDIF
      ENDDO
    ELSE
      START = SIMTIME
      END = SUM(TPSECONDS(1:TIME_PERIOD))
      DO I = 1, NUMBER_OF_ENTRYNODES - NUMBER_OF_SSNODES
        INODE = ENTRYLINK(I)%UP
        INDEX = ENTRYLINK(I)%INDEX
        IF(SIMTIME .EQ. SUM(TPSECONDS(1:TIME_PERIOD-1)) + TIMESTEP) THEN
          IF(FLOWRATE(INODE) .NE. ENTRY_DATA(I)%FLOWRATE) THEN
            WRITE50(INODE, TIME_PERIOD) = .TRUE.
          ELSEIF(TRUCK_PCT(INODE) .NE. ENTRY_DATA(I)%TRUCK_PCT) THEN
            WRITE50(INODE, TIME_PERIOD) = .TRUE.
          ELSEIF(CARPOOL_PCT(INODE) .NE. ENTRY_DATA(I)%CARPOOL_PCT) THEN
            WRITE50(INODE, TIME_PERIOD) = .TRUE.
          ELSEIF(HOV_VIOLATOR_PCT(INODE) .NE. ENTRY_DATA(I)%HOV_VIOLATORS_PER10000 / 10000.) THEN
            WRITE50(INODE, TIME_PERIOD) = .TRUE.
          ENDIF
          IF(WRITE50(INODE, TIME_PERIOD)) THEN
            FLOW_RATE(INODE, TIME_PERIOD) = ENTRY_DATA(I)%FLOWRATE
            FLOWRATE(INODE) = ENTRY_DATA(I)%FLOWRATE
            ENTRYLINK(I)%TIME(INDEX + 1) = START / 60.
            ENTRYLINK(I)%FLOW(INDEX + 1) = ENTRY_DATA(I)%FLOWRATE
            ENTRYLINK(I)%TIME(INDEX + 2) = END / 60.
            ENTRYLINK(I)%FLOW(INDEX + 2) = ENTRY_DATA(I)%FLOWRATE
            ENTRYLINK(I)%INDEX = INDEX + 2
            TRUCK_PCT(INODE) = ENTRY_DATA(I)%TRUCK_PCT
            CARPOOL_PCT(INODE) = ENTRY_DATA(I)%CARPOOL_PCT
            HOV_VIOLATOR_PCT(INODE) = ENTRY_DATA(I)%HOV_VIOLATORS_PER10000 / 10000.
            IF(SUM(ENTRY_DATA(I)%LANE_PCT) .NE. 0) THEN
              LANE_PCT(INODE, 1:NLANES) = ENTRY_DATA(I)%LANE_PCT(1:NLANES) / 100.
            ELSE
              LANE_PCT(INODE, 1:NLANES) = 1.0 / NLANES
            ENDIF
          ENDIF
        ELSE
          IF(FLOWRATE(INODE) .NE. ENTRY_DATA(I)%FLOWRATE) THEN
            WRITE53(INODE, TIME_PERIOD) = .TRUE.
            FLOW_RATE(INODE, TIME_PERIOD) = ENTRY_DATA(I)%FLOWRATE
            FLOWRATE(INODE) = ENTRY_DATA(I)%FLOWRATE
            ENTRYLINK(I)%TIME(INDEX) = START / 60.
            ENTRYLINK(I)%TIME(INDEX + 1) = START / 60. + 1
            ENTRYLINK(I)%FLOW(INDEX + 1) = ENTRY_DATA(I)%FLOWRATE
            ENTRYLINK(I)%TIME(INDEX + 2) = END / 60.
            ENTRYLINK(I)%FLOW(INDEX + 2) = ENTRY_DATA(I)%FLOWRATE
            ENTRYLINK(I)%INDEX = INDEX + 2
          ENDIF
        ENDIF
      ENDDO
    ENDIF
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION SET_NUMBER_OF_SSNODES[DLLEXPORT, STDCALL](N) RESULT(ERRORS)
  USE FLOWDATA_MOD
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    ERRORS = 0
    NUMBER_OF_SSNODES = N
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'SET_NUMBER_OF_SSNODES CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_NUMBER_OF_SSNODES[DLLEXPORT, STDCALL](N) RESULT(ERRORS)
  USE FLOWDATA_MOD
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  INTEGER :: N
! ----------------------------------------------------------------------
  ERRORS = 0
  N = NUMBER_OF_SSNODES
  END
  
! ==================================================================================================
  INTEGER FUNCTION DEFINE_SSNODES[DLLEXPORT, STDCALL](SS_DATA) RESULT(ERRORS)
  USE FLOWDATA_MOD
  USE STREET_LINKS
  USE API_DATA
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  TYPE(SOURCE_SINK_DATA) :: SS_DATA(NUMBER_OF_SSNODES)
  INTEGER :: I, INODE, IL, INDEX, INS, T1, T2
  LOGICAL :: FOUND, FIRSTCALL = .TRUE.
! ----------------------------------------------------------------------
  !!!Need to add truck and carpool percentages.
  ERRORS = 0
  IF(TIME_PERIOD .LE. 1) THEN
    INS = NUMBER_OF_ENTRYNODES
    DO I = 1, NUMBER_OF_SSNODES
      FOUND = .FALSE.
      INDEX = 0
      DO IL = 1, N_STREET_LINKS
        IF(SUSN(IL) .EQ. SS_DATA(I)%SS_USN .AND. SDSN(IL) .EQ. SS_DATA(I)%SS_DSN) THEN
          INS = INS + 1
          CENTROID(IL) = I
          CENTROID_LABEL(IL) = SS_DATA(I)%CENTROID_LABEL
          ENTRYLINK(INS)%CARDTYPE = 51
          ENTRYLINK(INS)%LINKID = IL
          ENTRYLINK(INS)%UP = SUSN(IL)
          ENTRYLINK(INS)%DOWN = SDSN(IL)
          ENTRYLINK(INS)%NETWORK = I_STREET
          INDEX = INDEX + 1
          ENTRYLINK(INS)%INDEX = INDEX
          ENTRYLINK(INS)%TIME(INDEX) = 0
          ENTRYLINK(INS)%FLOW(INDEX) = SS_DATA(I)%FLOWRATE
          INDEX = INDEX + 1
          ENTRYLINK(INS)%INDEX = INDEX
          ENTRYLINK(INS)%TIME(INDEX) = TPSECONDS(1) / 60.
          ENTRYLINK(INS)%FLOW(INDEX) = SS_DATA(I)%FLOWRATE
          FOUND = .TRUE.
          EXIT
        ENDIF
      ENDDO
      IF(.NOT. FOUND) THEN
        ERRORS = ERRORS + 1
        WRITE(MSGTEXT, '(A,2I6)') 'LINK NOT FOUND IN DEFINE_SSNODES ', SS_DATA(I)%SS_USN, SS_DATA(I)%SS_DSN
        CALL SENDTEXTMSG(M_ERROR)
        RETURN
      ENDIF
    ENDDO
    NUMBER_OF_ENTRYNODES = NUMBER_OF_ENTRYNODES + NUMBER_OF_SSNODES
  ELSE
    T1 = SUM(TPSECONDS(1:TIME_PERIOD-1)) / 60
    T2 = SUM(TPSECONDS(1:TIME_PERIOD)) / 60
    DO INS = 1, NUMBER_OF_SSNODES
      DO I = 1, NUMBER_OF_ENTRYNODES
        FOUND = .FALSE.
        IF(ENTRYLINK(I)%UP .EQ. SS_DATA(INS)%SS_USN .AND. ENTRYLINK(I)%DOWN .EQ. SS_DATA(INS)%SS_DSN) THEN
          INDEX = ENTRYLINK(I)%INDEX
          INDEX = INDEX + 1
          ENTRYLINK(I)%TIME(INDEX) = T1
          ENTRYLINK(I)%FLOW(INDEX) = SS_DATA(INS)%FLOWRATE
          INDEX = INDEX + 1
          ENTRYLINK(I)%INDEX = INDEX
          ENTRYLINK(I)%TIME(INDEX) = T2
          ENTRYLINK(I)%FLOW(INDEX) = SS_DATA(INS)%FLOWRATE
          FOUND = .TRUE.
          EXIT
        ENDIF
      ENDDO
      IF(.NOT. FOUND) THEN
        ERRORS = ERRORS + 1
        WRITE(MSGTEXT, '(A,2I6)') 'LINK NOT FOUND IN DEFINE_SSNODES ', SS_DATA(I)%SS_USN, SS_DATA(I)%SS_DSN
        CALL SENDTEXTMSG(M_ERROR)
        RETURN
      ENDIF
    ENDDO
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION SET_NUMBER_OF_FTC_SIGNALS[DLLEXPORT, STDCALL](N) RESULT(ERRORS)
  USE TIMED_CONTROLLERS
  USE SIMPARAMS
  USE API_DATA
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    ERRORS = 0
    NUMBER_OF_FTCS = N
    CALL ALLOCATE_FTC_ARRAYS
    ALLOCATE(DURATIONS(N, 12, N_PERIODS))
    DURATIONS = 0
    ALLOCATE(SIGCODES(N, 6, 12, N_PERIODS))
    SIGCODES = 0
    ALLOCATE(WRITE3536(N, N_PERIODS))
    WRITE3536 = .FALSE.
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'SET_NUMBER_OF_FTC_SIGNALS CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION DEFINE_FTC_SIGNALS[DLLEXPORT, STDCALL](SIGNALDATA) RESULT(ERRORS)
  USE TIMED_CONTROLLERS
  USE STREET_LINKS
  USE STREET_NODES
  USE SIMPARAMS
  USE API_DATA 
  USE TEXT
  USE NODE_TABLE
  IMPLICIT NONE
  TYPE(FTC_DATA) :: SIGNALDATA(NUMBER_OF_FTCS)
  INTEGER :: I, INTERVAL, APPROACH, NODE, IL, OFFSET, INT, ILX
! ----------------------------------------------------------------------
  ERRORS = 0
  IF(TIME_PERIOD .LE. 1) THEN
    DO I = 1, NUMBER_OF_FTCS
      NODE = SIGNALDATA(I)%NODE
      IF(NODE_TYPE(NODE) .NE. NT_INTERN) THEN
        ERRORS = ERRORS + 1
        WRITE(MSGTEXT, '(A, I8)') 'INVALID NODE SPECIFIED FOR FIXED-TIME CONTROL ', NODE
        CALL SENDTEXTMSG(M_ERROR)
        EXIT
      ELSE
        NFTC(NODE) = I
        FTC_SIGNALS(I) = SIGNALDATA(I)
        DURATIONS(I, 1:12, 1) = SIGNALDATA(I)%DURATION(1:12)
        DO APPROACH = 1, FTC_SIGNALS(I)%APPROACHES
          IF(FTC_SIGNALS(I)%APPROACH(APPROACH) .EQ. 0) THEN
            IL = 0
            DO ILX = 1, N_STREET_LINKS
              IF(SDSN(ILX) .NE. NODE) CYCLE
              IF(NODE_TYPE(SUSN(ILX)) .EQ. NT_EXTERN) THEN
                IL = ILX
                EXIT
              ENDIF
            ENDDO
          ELSE
            CALL FIND_STREET_LINK(FTC_SIGNALS(I)%APPROACH(APPROACH), NODE, IL)
          ENDIF
          IF(IL .EQ. 0) THEN
            ERRORS = ERRORS + 1
            WRITE(MSGTEXT, '(A, I5)') 'APPROACH NODE NOT FOUND FOR FTC SIGNAL AT NODE ', NODE
            CALL SENDTEXTMSG(M_ERROR)
            WRITE(MSGTEXT, '(A, I4)') '  APPROACH NODE = ', FTC_SIGNALS(I)%APPROACH(APPROACH)
            CALL SENDTEXTMSG(M_ERROR)
          ELSE
            FTC_SIGNALS(I)%APPROACH(APPROACH) = IL
            FTC_SIGNAL_ID(IL) = I
            APPROACH_NUM(IL) = APPROACH
            IF(FTC_SIGNALS(I)%ACTIVE_INTERVALS .EQ. 1) THEN
              IF(FTC_SIGNALS(I)%SIGNAL_CODE(APPROACH, 1) .EQ. 0) THEN
                FTC_SIGNALS(I)%SIGNAL_CODE(APPROACH, 1) = S_YIELD
              ELSEIF(FTC_SIGNALS(I)%SIGNAL_CODE(APPROACH, 1) .EQ. 1) THEN
                FTC_SIGNALS(I)%SIGNAL_CODE(APPROACH, 1) = S_GREEN
              ELSEIF(FTC_SIGNALS(I)%SIGNAL_CODE(APPROACH, 1) .EQ. 5) THEN
                FTC_SIGNALS(I)%SIGNAL_CODE(APPROACH, 1) = S_STOP
              ENDIF
            ELSE
              SIGNALIZED(IL) = .TRUE.
            ENDIF
            SIGNAL_CODE(IL) = FTC_SIGNALS(I)%SIGNAL_CODE(APPROACH, 1)
          ENDIF
        ENDDO
        IF(FTC_SIGNALS(I)%ACTIVE_INTERVALS .GT. 1) NUMBER_OF_FTSIGNALS = NUMBER_OF_FTSIGNALS + 1
        FTC_SIGNALS(I)%CURRENT_INTERVAL = 1
        OFFSET = FTC_SIGNALS(I)%OFFSET
        IF(OFFSET .NE. 0) THEN
          INT = FTC_SIGNALS(I)%ACTIVE_INTERVALS
          DO WHILE(OFFSET .GT. 0)
            OFFSET = OFFSET - FTC_SIGNALS(I)%DURATION(INT)
            INT = INT - 1
          ENDDO
          INT = INT + 1
          FTC_SIGNALS(I)%CURRENT_INTERVAL = INT
          FTC_SIGNALS(I)%TIME_IN_INTERVAL = -OFFSET     
        ENDIF
        FTC_SIGNALS(I)%CYCLE_LENGTH = SUM(FTC_SIGNALS(I)%DURATION)
      ENDIF
      SIGCODES(I, 1:6, 1:12, 1) = SIGNALDATA(I)%SIGNAL_CODE
    ENDDO
  ELSE
    IF(.NOT. ALLOCATED(WRITE3536)) THEN
      ALLOCATE(DURATIONS(NUMBER_OF_FTCS, 12, N_PERIODS))
      DURATIONS = 0
      ALLOCATE(SIGCODES(NUMBER_OF_FTCS, 6, 12, N_PERIODS))
      SIGCODES = 0
      ALLOCATE(WRITE3536(NUMBER_OF_FTCS, N_PERIODS))
      WRITE3536 = .FALSE.
    ENDIF

    DO I = 1, NUMBER_OF_FTCS
      !Determine if any inputs are changed from previous time period
      IF(FTC_SIGNALS(I)%ACTIVE_INTERVALS .GT. 1) THEN
        DO INTERVAL = 1, 12
          IF(FTC_SIGNALS(I)%DURATION(INTERVAL) .NE. SIGNALDATA(I)%DURATION(INTERVAL)) THEN
            WRITE3536(I, TIME_PERIOD) = .TRUE.
            GOTO 100
          ENDIF
          DO APPROACH = 1, 5
            IF(FTC_SIGNALS(I)%SIGNAL_CODE(APPROACH, INTERVAL) .NE. SIGNALDATA(I)%SIGNAL_CODE(APPROACH, INTERVAL)) THEN
              WRITE3536(I, TIME_PERIOD) = .TRUE.
              GOTO 100
            ENDIF
          ENDDO
        ENDDO
      ENDIF
100   CONTINUE      
      IF(WRITE3536(I, TIME_PERIOD)) THEN
        DO APPROACH = 1, 5
          DO INTERVAL = 1, 12
            WRITE3536(I, TIME_PERIOD) = .TRUE.
            FTC_SIGNALS(I)%DURATION = SIGNALDATA(I)%DURATION
            FTC_SIGNALS(I)%SIGNAL_CODE = SIGNALDATA(I)%SIGNAL_CODE
            DURATIONS(I, 1:12, TIME_PERIOD) = SIGNALDATA(I)%DURATION(1:12)
            SIGCODES(I, 1:6, 1:12, TIME_PERIOD) = SIGNALDATA(I)%SIGNAL_CODE
          ENDDO
        ENDDO
      ENDIF
    ENDDO
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION SET_NUMBER_OF_AC_SIGNALS[DLLEXPORT, STDCALL](N1) RESULT(ERRORS)
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N1
! ----------------------------------------------------------------------
  ERRORS = 0
  IF(TIME_PERIOD .LE. 1) THEN
    NUMBER_OF_ACS = N1
    CALL ALLOCATE_AC_ARRAYS(N1)
    ALLOCATE(ACSIGNAL_DATA(N1, N_PERIODS))
    ALLOCATE(WRITE4X(N1, N_PERIODS))
    WRITE4X = .FALSE.
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'SET_NUMBER_OF_AC_SIGNALS CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION DEFINE_COORDINATION[DLLEXPORT, STDCALL](INPUTS) RESULT(ERRORS)
  USE API_DATA
  USE ACTUATED_CONTROLLERS
  USE STREET_NODES
  IMPLICIT NONE
  TYPE(COORDINATION_INPUTS) :: INPUTS
  INTEGER :: IACT
! ----------------------------------------------------------------------
  ERRORS = 0
  IACT = NACT(INPUTS%NODE)
  IF(IACT .EQ. 0) THEN
    ERRORS = 1
  ELSE
    IF(INPUTS%CYCLE_LENGTH .NE. AC_SIGNALS(IACT)%CYCLE_LENGTH) AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH = INPUTS%CYCLE_LENGTH
    IF(INPUTS%OFFSET .NE. AC_SIGNALS(IACT)%OFFSET) AC_SIGNALS(IACT)%NEW_OFFSET = INPUTS%OFFSET
    AC_SIGNALS(IACT)%CYCLE_LENGTH = INPUTS%CYCLE_LENGTH
    AC_SIGNALS(IACT)%OFFSET = INPUTS%OFFSET
    CALL SET_COORDINATION_TIMER(IACT, AC_SIGNALS(IACT)%CYCLE_LENGTH + AC_SIGNALS(IACT)%OFFSET)
  ENDIF
  END
    
! ==================================================================================================
  INTEGER FUNCTION GET_COORDINATION_NETWORK_INPUTS[DLLEXPORT, STDCALL](OUTPUTS) RESULT(ERRORS)
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER :: OUTPUTS(3)
! ----------------------------------------------------------------------
  ERRORS = 0
  OUTPUTS(1) = SYNC_REF_TIME
  OUTPUTS(2) = INITIALIZATION_END
  OUTPUTS(3) = SYNCOF
  END

! ==================================================================================================
  INTEGER FUNCTION DEFINE_AC_SIGNALS[DLLEXPORT, STDCALL](AC_DATA_INPUTS) RESULT(ERRORS)
  USE ACTUATED_CONTROLLERS
  USE STREET_NODES
  USE API_DATA
  USE STREET_DETECTORS
  USE STREET_LINKS
  USE SIMPARAMS
  USE TEXT
  USE NODE_TABLE
  USE STREET_VEHICLES
  USE SEEDS
  IMPLICIT NONE
  TYPE(API_ACTUATED_DATA) :: AC_DATA_INPUTS(NUMBER_OF_ACS)
  INCLUDE 'IOFILES.INC'
  INTEGER :: IACT, NODE, PHASE, IAP, IL, DCS_FLAG, RING, NP, USN, DSN, ILINK
  INTEGER :: APLINKS(10), NSC, INODE, NNODES
  REAL :: SPLITS(8), FORCEOFFS(8), RNDNUM
  LOGICAL :: IN_USE(8), FOUND, SERVED_LEFT, SERVED_RIGHT, SERVED_THRU, SERVED_LD, SERVED_RD, FIRST = .TRUE.
! ----------------------------------------------------------------------
  ERRORS = 0    
  DO IACT = 1, NUMBER_OF_ACS
    IN_USE = .FALSE.
    NNODES = 0
    DO INODE = 1, 8
      NODE = AC_DATA_INPUTS(IACT)%NODE(INODE)
      IF(NODE .EQ. 0) EXIT
      NNODES = NNODES + 1
      IF(NODE_TYPE(NODE) .NE. NT_INTERN) THEN
        ERRORS = ERRORS + 1
        EXIT
      ELSE
        NACT(NODE) = IACT
        AC_SIGNALS(IACT)%NODE(INODE) = NODE
      ENDIF
    ENDDO
    
    !Define the direct approach links
    AC_SIGNALS(IACT)%N_DIRECT_APPROACHES = AC_DATA_INPUTS(IACT)%N_DIRECT_APPROACHES
    APLINKS = 0
    DO IAP = 1, AC_SIGNALS(IACT)%N_DIRECT_APPROACHES
      USN = AC_DATA_INPUTS(IACT)%DIRECT_APPROACH_USN(IAP)
      DSN = AC_DATA_INPUTS(IACT)%DIRECT_APPROACH_DSN(IAP)
      CALL FIND_STREET_LINK(USN, DSN, IL)
      IF(IL .EQ. 0) THEN
        ERRORS = ERRORS + 1
        IF(NNODES .EQ. 1) THEN
          WRITE(MSGTEXT, '(A, I4)') 'APPROACH LINK NOT FOUND FOR ACTUATED SIGNAL AT NODE ', AC_DATA_INPUTS(IACT)%NODE(1)
        ELSE
          WRITE(MSGTEXT, '(A)') 'APPROACH LINK NOT FOUND FOR MULTI-NODE ACTUATED SIGNAL'
        ENDIF
        CALL SENDTEXTMSG(M_ERROR)
        WRITE(MSGTEXT, '(A, I1, A, I4, A, I4)') '  APPROACH NUMBER = ', IAP, ' USN = ', USN, ' DSN = ', DSN
        CALL SENDTEXTMSG(M_ERROR)
        EXITFLG = 1
        RETURN
      ELSE
        APLINKS(IAP) = IL
        AC_SIGNALS(IACT)%DIRECT_APPROACH(IAP) = IL
        SIGNALIZED(IL) = .TRUE.
        AC_SIGNAL_ID(IL) = IACT
      ENDIF
    ENDDO
    
    !Define the indirect approach links
    AC_SIGNALS(IACT)%N_INDIRECT_APPROACHES = AC_DATA_INPUTS(IACT)%N_INDIRECT_APPROACHES
    DO IAP = 1, AC_SIGNALS(IACT)%N_INDIRECT_APPROACHES
      USN = AC_DATA_INPUTS(IACT)%INDIRECT_APPROACH_USN(IAP)
      DSN = AC_DATA_INPUTS(IACT)%INDIRECT_APPROACH_DSN(IAP)
      CALL FIND_STREET_LINK(USN, DSN, IL)
      IF(IL .EQ. 0) THEN
        ERRORS = ERRORS + 1
        IF(NNODES .EQ. 1) THEN
          WRITE(MSGTEXT, '(A, I4)') 'INDIRECT APPROACH LINK NOT FOUND FOR ACTUATED SIGNAL AT NODE ', AC_DATA_INPUTS(IACT)%NODE(1)
        ELSE
          WRITE(MSGTEXT, '(A)') 'INDIRECT APPROACH LINK NOT FOUND FOR MULTI-NODE ACTUATED SIGNAL'
        ENDIF
        CALL SENDTEXTMSG(M_ERROR)
        WRITE(MSGTEXT, '(A, I1, A, I4, A, I4)') '  APPROACH NUMBER = ', IAP, ' USN = ', USN, ' DSN = ', DSN
        CALL SENDTEXTMSG(M_ERROR)
        EXIT
      ELSE
        AC_SIGNALS(IACT)%INDIRECT_APPROACH(IAP) = IL
      ENDIF
    ENDDO
    
    !Define the timing plan
    AC_SIGNALS(IACT)%SDP%CONSECUTIVE_FAILURES = AC_DATA_INPUTS(IACT)%CFAILS
    AC_SIGNALS(IACT)%SDP%ADJUSTMENT = AC_DATA_INPUTS(IACT)%ADJ
    AC_SIGNALS(IACT)%GUI_ACTUATED_MODE(1:8) = AC_DATA_INPUTS(IACT)%ACTUATED_MODE(1:8)
    AC_SIGNALS(IACT)%GUI_MIN_GREEN_TIMES = AC_DATA_INPUTS(IACT)%MIN_GREEN_TIME
    AC_SIGNALS(IACT)%GUI_MAX_GREEN_TIMES = AC_DATA_INPUTS(IACT)%MAX_GREEN_TIME
    AC_SIGNALS(IACT)%GUI_DEFAULT_EXTENSION_TIMES = AC_DATA_INPUTS(IACT)%DEFAULT_EXTENSION_TIME
    AC_SIGNALS(IACT)%GUI_GAP_TIMES = AC_DATA_INPUTS(IACT)%GAP_TIME
    AC_SIGNALS(IACT)%GUI_TIME_BEFORE_REDUCTION = AC_DATA_INPUTS(IACT)%TIME_BEFORE_REDUCTION
    AC_SIGNALS(IACT)%GUI_TIME_TO_REDUCE = AC_DATA_INPUTS(IACT)%TIME_TO_REDUCE
    AC_SIGNALS(IACT)%GUI_MIN_GAP_TIMES = AC_DATA_INPUTS(IACT)%MIN_GAP_TIME
    AC_SIGNALS(IACT)%GUI_YC = AC_DATA_INPUTS(IACT)%YC
    AC_SIGNALS(IACT)%GUI_RC = AC_DATA_INPUTS(IACT)%RC
      
    !Define pedestrian inputs
    DO PHASE = 1, 8
      AC_SIGNALS(IACT)%NEW_PED_OMIT(PHASE) = .TRUE.
      IF(.NOT. AC_DATA_INPUTS(IACT)%PED_OMIT(PHASE)) THEN
        AC_SIGNALS(IACT)%PED_PHASE(PHASE) = .TRUE.
        AC_SIGNALS(IACT)%NEW_PED_OMIT(PHASE) = .FALSE.
        AC_SIGNALS(IACT)%NEW_PED_WALK_TIMES(PHASE) = AC_DATA_INPUTS(IACT)%WALK_TIME(PHASE)
        AC_SIGNALS(IACT)%NEW_PED_CLEARANCE_TIMES(PHASE) = AC_DATA_INPUTS(IACT)%WALK_CLEARANCE_TIME(PHASE)
        AC_SIGNALS(IACT)%PED_INTENSITY(PHASE) = AC_DATA_INPUTS(IACT)%PED_INTENSITY(PHASE)
        AC_SIGNALS(IACT)%PED_HEADWAY(PHASE) = AC_DATA_INPUTS(IACT)%PED_HEADWAY(PHASE)
        AC_SIGNALS(IACT)%PED_RECALL_CODE(PHASE) = AC_DATA_INPUTS(IACT)%PED_RECALL_CODE(PHASE)
        AC_SIGNALS(IACT)%PED_TYPE(PHASE) = 0
        IF(AC_SIGNALS(IACT)%PED_INTENSITY(PHASE) .GT. 0) THEN
          AC_SIGNALS(IACT)%PED_TYPE(PHASE) = 1
          CALL STREET_RANDOM(SSEED, RNDNUM)  
          AC_SIGNALS(IACT)%NEXT_PED_ARRIVAL(PHASE) = (3600.0 / AC_SIGNALS(IACT)%PED_INTENSITY(PHASE)) * (-LOG(RNDNUM))
        ELSEIF(AC_SIGNALS(IACT)%PED_HEADWAY(PHASE) .GT. 0) THEN
          AC_SIGNALS(IACT)%PED_TYPE(PHASE) = 2
          AC_SIGNALS(IACT)%NEXT_PED_ARRIVAL(PHASE) = AC_DATA_INPUTS(IACT)%PED_HEADWAY(PHASE) + AC_DATA_INPUTS(IACT)%PED_START_TIME(PHASE)
        ENDIF
      ENDIF
    ENDDO
      
    !Define the turn movements for each phase
    DO PHASE = 1, 8
      DO IAP = 1, AC_SIGNALS(IACT)%N_DIRECT_APPROACHES
        USN = AC_DATA_INPUTS(IACT)%DIRECT_APPROACH_USN(IAP)
        DSN = AC_DATA_INPUTS(IACT)%DIRECT_APPROACH_DSN(IAP)
        AC_SIGNALS(IACT)%PHASE(PHASE)%LEFTARROW(IAP) = AC_DATA_INPUTS(IACT)%LEFTARROW(PHASE, IAP)
        AC_SIGNALS(IACT)%PHASE(PHASE)%THRUARROW(IAP) = AC_DATA_INPUTS(IACT)%THRUARROW(PHASE, IAP)
        AC_SIGNALS(IACT)%PHASE(PHASE)%RIGHTARROW(IAP) = AC_DATA_INPUTS(IACT)%RIGHTARROW(PHASE, IAP)
        AC_SIGNALS(IACT)%PHASE(PHASE)%LDIAGARROW(IAP) = AC_DATA_INPUTS(IACT)%LDIAGARROW(PHASE, IAP)
        AC_SIGNALS(IACT)%PHASE(PHASE)%RDIAGARROW(IAP) = AC_DATA_INPUTS(IACT)%RDIAGARROW(PHASE, IAP)
        
        !Convert the signal turn movements to link turn movements
        IF(AC_SIGNALS(IACT)%PHASE(PHASE)%LEFTARROW(IAP)) THEN
          PHASE_LEFT(APLINKS(IAP)) = PHASE
          AC_SIGNALS(IACT)%PHASE(PHASE)%IN_USE = .TRUE.
        ENDIF
        IF(AC_SIGNALS(IACT)%PHASE(PHASE)%THRUARROW(IAP)) THEN
          PHASE_THRU(APLINKS(IAP)) = PHASE
          AC_SIGNALS(IACT)%PHASE(PHASE)%IN_USE = .TRUE.
        ENDIF
        IF(AC_SIGNALS(IACT)%PHASE(PHASE)%RIGHTARROW(IAP)) THEN
          PHASE_RIGHT(APLINKS(IAP)) = PHASE
          AC_SIGNALS(IACT)%PHASE(PHASE)%IN_USE = .TRUE.
        ENDIF
        IF(AC_SIGNALS(IACT)%PHASE(PHASE)%LDIAGARROW(IAP) .OR. AC_SIGNALS(IACT)%PHASE(PHASE)%RDIAGARROW(IAP)) THEN
          PHASE_DIAG(APLINKS(IAP)) = PHASE
          AC_SIGNALS(IACT)%PHASE(PHASE)%IN_USE = .TRUE.
        ENDIF
        
        !IF(AC_DATA_INPUTS(IACT)%CHECKIN_DISTANCE(PHASE, IAP) .GT. 0) THEN
        !  IF(FIRST) THEN
        !    FIRST = .FALSE.
        !    OPEN(9975, FILE = LINFNAME(1:IROOT-1)//'_PRIORITY.TXT', ERR=10, IOMSG=ETEXT)
        !  ELSE
        !    OPEN(9975, FILE = LINFNAME(1:IROOT-1)//'_PRIORITY.TXT', ACCESS='APPEND', ERR=10, IOMSG=ETEXT)
        !  ENDIF
        !  WRITE(9975, '(A, I4)') '  APPROACH NUMBER = ', IAP
        !  WRITE(9975, '(A, I4)') '  UPNODE = ', USN
        !  WRITE(9975, '(A, I4)') '  SIGNAL NODE = ', DSN
        !  WRITE(9975, '(A, I4)') '  PHASE = ', PHASE
        !  WRITE(9975, '(A, I5)') '  CHECKIN DISTANCE = ', AC_DATA_INPUTS(IACT)%CHECKIN_DISTANCE(PHASE, IAP
        !  WRITE(9975, '(A, I5)') '  CHECKOUT DISTANCE = ', AC_DATA_INPUTS(IACT)%CHECKOUT_DISTANCE(PHASE, IAP)
        !  WRITE(9975, '(A, I4)') '  POLICY = ', AC_DATA_INPUTS(IACT)%POLICY(IAP, PHASE)
        !  CLOSE(9975)
        !ENDIF

      ENDDO
    ENDDO
        
    !Store the phasing and lead/lag code by ring
    DO PHASE = 1, 8
      AC_SIGNALS(IACT)%PHASE_SEQUENCE(PHASE) = AC_DATA_INPUTS(IACT)%RING_PHASE(PHASE)
      IF(MOD(PHASE, 2) .EQ. 1 .AND. MOD(AC_SIGNALS(IACT)%PHASE_SEQUENCE(PHASE), 2) .EQ. 0) THEN
        AC_SIGNALS(IACT)%LEAD(PHASE) = .FALSE.
      ELSEIF(MOD(PHASE, 2) .EQ. 0 .AND. MOD(AC_SIGNALS(IACT)%PHASE_SEQUENCE(PHASE), 2) .EQ. 0) THEN
        AC_SIGNALS(IACT)%LEAD(PHASE) = .TRUE.
      ENDIF
    ENDDO 
        
    !Store tram priority inputs
    AC_SIGNALS(IACT)%CHECKIN_DISTANCE = AC_DATA_INPUTS(IACT)%CHECKIN_DISTANCE
    AC_SIGNALS(IACT)%CHECKOUT_DISTANCE = AC_DATA_INPUTS(IACT)%CHECKOUT_DISTANCE
    AC_SIGNALS(IACT)%POLICY = AC_DATA_INPUTS(IACT)%POLICY
    !AC_SIGNALS(IACT)%rtmode = 1 !Replace with data from the editor 

    !Store options for all phases
    !AC_SIGNALS(IACT)%RED_LOCK_CODE = AC_DATA_INPUTS(IACT)%RED_LOCK_CODE
    !AC_SIGNALS(IACT)%YELLOW_LOCK_CODE = AC_DATA_INPUTS(IACT)%YELLOW_LOCK_CODE
    AC_SIGNALS(IACT)%DUAL_ENTRY_CODE = AC_DATA_INPUTS(IACT)%DUAL_ENTRY_CODE
    !AC_SIGNALS(IACT)%RED_REVERT_TIME = AC_DATA_INPUTS(IACT)%RED_REVERT_TIME
    !AC_SIGNALS(IACT)%SIMULTANEOUS_GAP_CODE = AC_DATA_INPUTS(IACT)%SIMULTANEOUS_GAP_CODE
    !AC_SIGNALS(IACT)%CONDITIONAL_SERVICE_CODE = AC_DATA_INPUTS(IACT)%CONDITIONAL_SERVICE_CODE 
    !AC_SIGNALS(IACT)%MIN_CONDITIONAL_SERVICE_TIME = AC_DATA_INPUTS(IACT)%MIN_CONDITIONAL_SERVICE_TIME 

    !Define the coordination inputs
    AC_SIGNALS(IACT)%OFFSET = AC_DATA_INPUTS(IACT)%OFFSET
    AC_SIGNALS(IACT)%NEW_OFFSET = AC_DATA_INPUTS(IACT)%OFFSET
    AC_SIGNALS(IACT)%NEW_OFFSET = AC_SIGNALS(IACT)%OFFSET
    AC_SIGNALS(IACT)%CYCLE_LENGTH = AC_DATA_INPUTS(IACT)%CYCLE_LENGTH
    AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH = AC_DATA_INPUTS(IACT)%CYCLE_LENGTH
    AC_SIGNALS(IACT)%TRANSITION_METHOD = AC_DATA_INPUTS(IACT)%TRANSITION_METHOD
    AC_SIGNALS(IACT)%MAXPCT_ADD = AC_DATA_INPUTS(IACT)%MAX_ADD
    AC_SIGNALS(IACT)%MAXPCT_SUBTRACT = AC_DATA_INPUTS(IACT)%MAX_SUBTRACT
    AC_SIGNALS(IACT)%FORCE_OFF_TIME = AC_DATA_INPUTS(IACT)%FORCE_OFF_TIMES
    CALL SET_COORDINATION_TIMER(IACT, AC_SIGNALS(IACT)%CYCLE_LENGTH + AC_SIGNALS(IACT)%OFFSET)
      
    IF(AC_SIGNALS(IACT)%CYCLE_LENGTH .NE. 0.) THEN
      IF(SUM(AC_DATA_INPUTS(IACT)%SPLITS) .EQ. 0 .AND. SUM(AC_DATA_INPUTS(IACT)%FORCE_OFF_TIMES) .NE. 0) THEN
        !Convert force-off times to splits
        CALL CALCSPLITS(IACT, AC_SIGNALS(IACT)%CYCLE_LENGTH, AC_SIGNALS(IACT)%FORCE_OFF_TIME, AC_SIGNALS(IACT)%PHASE_SEQUENCE, SPLITS)                         
        AC_SIGNALS(IACT)%PHASE_SPLITS = SPLITS
      ELSEIF(SUM(AC_DATA_INPUTS(IACT)%SPLITS) .NE. 0 .AND. SUM(AC_DATA_INPUTS(IACT)%FORCE_OFF_TIMES) .EQ. 0) THEN
        !Convert splits to force-off times
        AC_SIGNALS(IACT)%PHASE_SPLITS = AC_DATA_INPUTS(IACT)%SPLITS
        CALL CALCFORCEOFFS(IACT, AC_DATA_INPUTS(IACT)%SPLITS, AC_SIGNALS(IACT)%PHASE_SEQUENCE, FORCEOFFS) 
        AC_SIGNALS(IACT)%FORCE_OFF_TIME = FORCEOFFS
      ELSEIF(SUM(AC_DATA_INPUTS(IACT)%SPLITS) .EQ. 0 .AND. SUM(AC_DATA_INPUTS(IACT)%FORCE_OFF_TIMES) .EQ. 0) THEN
        ERRORS = ERRORS + 1
        WRITE(MSGTEXT, '(A, I4)') 'SPLITS AND FORCE_OFF_TIMES ARE ALL ZERO FOR NODE ', NODE
        CALL SENDTEXTMSG(M_ERROR)
      ENDIF
    ENDIF
      
    DO PHASE = 1, 8
      IF(AC_SIGNALS(IACT)%GUI_MAX_GREEN_TIMES(PHASE) .LT. AC_SIGNALS(IACT)%GUI_MIN_GREEN_TIMES(PHASE)) THEN
        IF(AC_SIGNALS(IACT)%CYCLE_LENGTH .EQ. 0. .OR. (AC_SIGNALS(IACT)%CYCLE_LENGTH .NE. 0. .AND. PHASE .NE. 2 .AND. PHASE .NE. 6)) THEN
          WRITE(MSGTEXT, '(A,I1,A,I4)') 'RT 47: MAX GREEN IS LESS THAN MIN GREEN FOR PHASE ', PHASE, ' AT NODE ', NODE
          CALL SENDTEXTMSG(M_ERROR)
          WRITE(MSGTEXT, '(A)') ' MAX GREEN WILL BE SET EQUAL TO MIN GREEN'
          CALL SENDTEXTMSG(M_ERROR)
          AC_SIGNALS(IACT)%GUI_MAX_GREEN_TIMES(PHASE) = AC_SIGNALS(IACT)%GUI_MIN_GREEN_TIMES(PHASE)
        ENDIF
      ENDIF 
    ENDDO
    
    IF(NNODES .GT. 1) THEN
      !Determine if any movement on any approach to any of the nodes defined by this controller does not have a phase that serves it
      DO INODE = 1, 8
        NODE = AC_SIGNALS(IACT)%NODE(INODE)
        IF(NODE .EQ. 0) EXIT
        DO IL = 1, N_STREET_LINKS
          IF(SDSN(IL) .EQ. NODE) THEN
            DO IAP = 1, AC_SIGNALS(IACT)%N_DIRECT_APPROACHES
              IF(IL .EQ. AC_SIGNALS(IACT)%DIRECT_APPROACH(IAP)) THEN
                IF(FTC_SIGNAL_ID(IL) .NE. 0) CYCLE
                SERVED_LEFT = .FALSE.
                SERVED_THRU = .FALSE.
                SERVED_RIGHT = .FALSE.
                SERVED_LD = .FALSE.
                SERVED_RD = .FALSE.
                IF(LEFT_LINK(IL) .NE. 0) THEN
                  DO PHASE = 1, 8
                    IF(AC_SIGNALS(IACT)%PHASE(PHASE)%LEFTARROW(IAP)) THEN
                      SERVED_LEFT = .TRUE.
                      EXIT
                    ENDIF
                  ENDDO
                ENDIF
                IF(STHRU_LINK(IL) .NE. 0) THEN
                  DO PHASE = 1, 8
                    IF(AC_SIGNALS(IACT)%PHASE(PHASE)%THRUARROW(IAP)) THEN
                      SERVED_THRU = .TRUE.
                      EXIT
                    ENDIF
                  ENDDO
                ENDIF
                IF(RIGHT_LINK(IL) .NE. 0) THEN
                  DO PHASE = 1, 8
                    IF(AC_SIGNALS(IACT)%PHASE(PHASE)%RIGHTARROW(IAP)) THEN
                      SERVED_RIGHT = .TRUE.
                      EXIT
                    ENDIF
                  ENDDO
                ENDIF
                IF(LEFT_DIAG_LINK(IL) .NE. 0) THEN
                  DO PHASE = 1, 8
                    IF(AC_SIGNALS(IACT)%PHASE(PHASE)%LDIAGARROW(IAP)) THEN
                      SERVED_LD = .TRUE.
                      EXIT
                    ENDIF
                  ENDDO
                ENDIF
                IF(RIGHT_DIAG_LINK(IL) .NE. 0) THEN
                  DO PHASE = 1, 8
                    IF(AC_SIGNALS(IACT)%PHASE(PHASE)%RDIAGARROW(IAP)) THEN
                      SERVED_RD = .TRUE.
                      EXIT
                    ENDIF
                  ENDDO
                ENDIF
              ENDIF
            ENDDO
            IF(LEFT_LINK(IL) .NE. 0 .AND. .NOT. SERVED_LEFT) THEN
              ALWAYS_GREEN(IL, TC_LEFT + 1) = .TRUE.
              WRITE(MSGTEXT, '(A,2I4,A)') '  Left movement on link ', SUSN(IL), SDSN(IL), ' is flagged for perpetual green'
              CALL SENDTEXTMSG(M_ERROR)
            ENDIF
            IF(STHRU_LINK(IL) .NE. 0 .AND. .NOT. SERVED_THRU) THEN
              ALWAYS_GREEN(IL, TC_THRU + 1) = .TRUE.
              WRITE(MSGTEXT, '(A,2I4,A)') '  Thru movement on link ', SUSN(IL), SDSN(IL), ' is flagged for perpetual green'
              CALL SENDTEXTMSG(M_ERROR)
            ENDIF
            IF(RIGHT_LINK(IL) .NE. 0 .AND. .NOT. SERVED_RIGHT) THEN
              ALWAYS_GREEN(IL, TC_RIGHT + 1) = .TRUE.
              WRITE(MSGTEXT, '(A,2I4,A)') '  Right movement on link ', SUSN(IL), SDSN(IL), ' is flagged for perpetual green'
              CALL SENDTEXTMSG(M_ERROR)
            ENDIF
            IF(LEFT_DIAG_LINK(IL) .NE. 0 .AND. .NOT. SERVED_LD) THEN
              ALWAYS_GREEN(IL, TC_LDIAG + 1) = .TRUE.
              WRITE(MSGTEXT, '(A,2I4,A)') '  Left Diagonal movement on link ', SUSN(IL), SDSN(IL), ' is flagged for perpetual green'
              CALL SENDTEXTMSG(M_ERROR)
            ENDIF
            IF(RIGHT_DIAG_LINK(IL) .NE. 0 .AND. .NOT. SERVED_RD) THEN
              ALWAYS_GREEN(IL, TC_RDIAG + 1) = .TRUE.
              WRITE(MSGTEXT, '(A,2I4,A)') '  Right Diagonal movement on link ', SUSN(IL), SDSN(IL), ' is flagged for perpetual green'
              CALL SENDTEXTMSG(M_ERROR)
            ENDIF            
          ENDIF
        ENDDO
      ENDDO  
    ENDIF
    
    !Save the data passed in for the time period
    ACSIGNAL_DATA(IACT, TIME_PERIOD) = AC_SIGNALS(IACT)
  ENDDO
  RETURN
10 WRITE(MSGTEXT, '(A)') 'FILE OPEN ERROR : DEFINE_AC_SIGNALS'
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A)') ETEXT
  CALL SENDTEXTMSG(M_ERROR)
  RETURN
  END
  
! ==================================================================================================
  INTEGER FUNCTION REDEFINE_AC_SIGNALS[DLLEXPORT, STDCALL](AC_DATA_INPUTS) RESULT(ERRORS)
  USE ACTUATED_CONTROLLERS
  USE STREET_NODES
  USE API_DATA
  USE STREET_DETECTORS
  USE STREET_LINKS
  USE SIMPARAMS
  IMPLICIT NONE
  TYPE(API_ACTUATED_DATA) :: AC_DATA_INPUTS(NUMBER_OF_ACS)
  INTEGER :: IACT, NODE, PHASE, IAP, IL
! ----------------------------------------------------------------------
  ERRORS = 0    
  DO IACT = 1, NUMBER_OF_ACS
    !Define the timing plan
    AC_SIGNALS(IACT)%SDP%CONSECUTIVE_FAILURES = AC_DATA_INPUTS(IACT)%CFAILS
    AC_SIGNALS(IACT)%SDP%ADJUSTMENT = AC_DATA_INPUTS(IACT)%ADJ
    AC_SIGNALS(IACT)%GUI_ACTUATED_MODE(1:8) = AC_DATA_INPUTS(IACT)%ACTUATED_MODE(1:8)
    AC_SIGNALS(IACT)%GUI_MIN_GREEN_TIMES = AC_DATA_INPUTS(IACT)%MIN_GREEN_TIME
    AC_SIGNALS(IACT)%GUI_MAX_GREEN_TIMES = AC_DATA_INPUTS(IACT)%MAX_GREEN_TIME
    AC_SIGNALS(IACT)%GUI_DEFAULT_EXTENSION_TIMES = AC_DATA_INPUTS(IACT)%DEFAULT_EXTENSION_TIME
    AC_SIGNALS(IACT)%GUI_GAP_TIMES = AC_DATA_INPUTS(IACT)%GAP_TIME
    AC_SIGNALS(IACT)%GUI_TIME_BEFORE_REDUCTION = AC_DATA_INPUTS(IACT)%TIME_BEFORE_REDUCTION
    AC_SIGNALS(IACT)%GUI_TIME_TO_REDUCE = AC_DATA_INPUTS(IACT)%TIME_TO_REDUCE
    AC_SIGNALS(IACT)%GUI_MIN_GAP_TIMES = AC_DATA_INPUTS(IACT)%MIN_GAP_TIME
    AC_SIGNALS(IACT)%GUI_YC = AC_DATA_INPUTS(IACT)%YC
    AC_SIGNALS(IACT)%GUI_RC = AC_DATA_INPUTS(IACT)%RC
    !Define pedestrian inputs
    DO PHASE = 1, 8
      AC_SIGNALS(IACT)%PED_PHASE(PHASE) = .TRUE.
      AC_SIGNALS(IACT)%NEW_PED_WALK_TIMES(PHASE) = AC_DATA_INPUTS(IACT)%WALK_TIME(PHASE)
      AC_SIGNALS(IACT)%NEW_PED_CLEARANCE_TIMES(PHASE) = AC_DATA_INPUTS(IACT)%WALK_CLEARANCE_TIME(PHASE)
      AC_SIGNALS(IACT)%NEW_PED_OMIT(PHASE) = AC_DATA_INPUTS(IACT)%PED_OMIT(PHASE)
      AC_SIGNALS(IACT)%PED_INTENSITY(PHASE) = AC_DATA_INPUTS(IACT)%PED_INTENSITY(PHASE)
      AC_SIGNALS(IACT)%PED_HEADWAY(PHASE) = AC_DATA_INPUTS(IACT)%PED_HEADWAY(PHASE)
      AC_SIGNALS(IACT)%PED_RECALL_CODE(PHASE) = AC_DATA_INPUTS(IACT)%PED_RECALL_CODE(PHASE)
    ENDDO
    DO PHASE = 1, 8
      AC_SIGNALS(IACT)%PHASE(PHASE)%IN_USE = .TRUE.
      DO IAP = 1, AC_SIGNALS(IACT)%N_DIRECT_APPROACHES
        AC_SIGNALS(IACT)%PHASE(PHASE)%LEFTARROW(IAP) = AC_DATA_INPUTS(IACT)%LEFTARROW(PHASE, IAP)
        AC_SIGNALS(IACT)%PHASE(PHASE)%THRUARROW(IAP) = AC_DATA_INPUTS(IACT)%THRUARROW(PHASE, IAP)
        AC_SIGNALS(IACT)%PHASE(PHASE)%RIGHTARROW(IAP) = AC_DATA_INPUTS(IACT)%RIGHTARROW(PHASE, IAP)
        AC_SIGNALS(IACT)%PHASE(PHASE)%LDIAGARROW(IAP) = AC_DATA_INPUTS(IACT)%LDIAGARROW(PHASE, IAP)
        AC_SIGNALS(IACT)%PHASE(PHASE)%RDIAGARROW(IAP) = AC_DATA_INPUTS(IACT)%RDIAGARROW(PHASE, IAP)
      ENDDO
    ENDDO
      
    !Store the phasing and lead/lag code by ring
    DO PHASE = 1, 8
      AC_SIGNALS(IACT)%PHASE_SEQUENCE(PHASE) = AC_DATA_INPUTS(IACT)%RING_PHASE(PHASE)
      IF(MOD(PHASE, 2) .EQ. 1 .AND. MOD(AC_SIGNALS(IACT)%PHASE_SEQUENCE(PHASE), 2) .EQ. 0) THEN
        AC_SIGNALS(IACT)%LEAD(PHASE) = .FALSE.
      ELSEIF(MOD(PHASE, 2) .EQ. 0 .AND. MOD(AC_SIGNALS(IACT)%PHASE_SEQUENCE(PHASE), 2) .EQ. 0) THEN
        AC_SIGNALS(IACT)%LEAD(PHASE) = .TRUE.
      ENDIF
    ENDDO

    !Define the coordination inputs
    AC_SIGNALS(IACT)%NEW_OFFSET = AC_DATA_INPUTS(IACT)%OFFSET
    AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH = AC_DATA_INPUTS(IACT)%CYCLE_LENGTH
    AC_SIGNALS(IACT)%TRANSITION_METHOD = AC_DATA_INPUTS(IACT)%TRANSITION_METHOD
    AC_SIGNALS(IACT)%MAXPCT_ADD = AC_DATA_INPUTS(IACT)%MAX_ADD
    AC_SIGNALS(IACT)%MAXPCT_SUBTRACT = AC_DATA_INPUTS(IACT)%MAX_SUBTRACT
    AC_SIGNALS(IACT)%FORCE_OFF_TIME = AC_DATA_INPUTS(IACT)%FORCE_OFF_TIMES

    !Save the data passed in for subsequent time period
    !New inputs that are passed in during a time period cannot be represented by the TRF file.
    ACSIGNAL_DATA(IACT, TIME_PERIOD) = AC_SIGNALS(IACT)
  ENDDO
  END
  
! ==================================================================================================
  INTEGER FUNCTION SET_NUMBER_OF_RAMPMETERS[DLLEXPORT, STDCALL](N) RESULT(ERRORS)
  USE RAMP_METERS
  USE SIMPARAMS
  USE API_DATA
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    ERRORS = 0
    NUMBER_OF_RAMPMETERS = N
    CALL ALLOCATE_RAMPMETER_ARRAYS
    ALLOCATE(METER_HEADWAY1(N, N_PERIODS))
    ALLOCATE(METER_ONSET(N, N_PERIODS))
    ALLOCATE(METER_TWOPERGREEN(N, N_PERIODS))
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'SET_NUMBER_OF_RAMPMETERS CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION DEFINE_RAMPMETERS[DLLEXPORT, STDCALL](RAMPMETERDATA) RESULT(ERRORS)
  USE API_DATA
  USE RAMP_METERS
  USE FREEWAY_LINKS
  USE FREEWAY_DETECTORS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  TYPE(RAMPMETER_DATA) :: RAMPMETERDATA(NUMBER_OF_RAMPMETERS)
  INTEGER :: I, IL, N, IDET
  LOGICAL :: FOUND
! ----------------------------------------------------------------------
  ERRORS = 0
  IF(TIME_PERIOD .LE. 1) THEN
    DO I = 1, NUMBER_OF_RAMPMETERS
      METER_ONSET(I, 1) = RAMPMETERDATA(I)%ONSET
      METER_HEADWAY1(I, 1) = RAMPMETERDATA(I)%HEADWAY(1)
      RAMPMETERS(I)%DSN = RAMPMETERDATA(I)%DSN
      FOUND = .FALSE.
      DO IL = 1, N_FREEWAY_LINKS
        IF(FDSN(IL) .EQ. RAMPMETERDATA(I)%DSN .AND. LINKTYPE(IL) .GT. 0) THEN
          RAMPMETER(IL) = I
          RAMPMETERS(I)%LINK = IL
          FOUND = .TRUE.
          EXIT
        ENDIF
      ENDDO
      IF(.NOT. FOUND) THEN
        ERRORS = 1
        EXIT
      ELSE
        RAMPMETERS(I)%CONTROL = RAMPMETERDATA(I)%CONTROL
        IF(RAMPMETERDATA(I)%CONTROL .EQ. 1) RAMPMETERS(I)%TWO_PERGREEN = RAMPMETERDATA(I)%TWOPERGREEN
        RAMPMETERS(I)%ONSET = RAMPMETERDATA(I)%ONSET
        RAMPMETERS(I)%CAPACITY = RAMPMETERDATA(I)%CAPACITY
        RAMPMETERS(I)%SPEED = RAMPMETERDATA(I)%SPEED
        RAMPMETERS(I)%STATE = MS_INACTIVE
        DO N = 1, 6
          RAMPMETERS(I)%HEADWAY(N) = RAMPMETERDATA(I)%HEADWAY(N)
        ENDDO
        DO N = 1, 10
          IF(RAMPMETERDATA(I)%DETECTOR(N) .NE. 0) THEN
            FOUND = .FALSE.
            DO IDET = 1, N_FREEWAY_DETECTORS
              IF(FDETECTOR(IDET)%LINK .NE. 0) THEN
                FOUND = .TRUE.
                EXIT
              ENDIF
            ENDDO
          ENDIF
          IF(.NOT. FOUND) THEN
            ERRORS = ERRORS + 1
            WRITE(MSGTEXT, '(A, I5)') 'DETECTOR NOT FOUND FOR RAMPMETER AT NODE ', RAMPMETERDATA(I)%DSN
            CALL SENDTEXTMSG(M_ERROR)
            WRITE(MSGTEXT, '(A, I5)') '  DETECTOR NUMBER = ', RAMPMETERDATA(I)%DETECTOR(N)
            CALL SENDTEXTMSG(M_ERROR)
          ENDIF
        ENDDO
        RAMPMETERS(I)%DETECTOR = RAMPMETERDATA(I)%DETECTOR
        METER_HEADWAY1(I, 1) = RAMPMETERDATA(I)%HEADWAY(1)
        METER_ONSET(I, 1) = RAMPMETERDATA(I)%ONSET
        METER_TWOPERGREEN(I, 1) = RAMPMETERS(I)%TWO_PERGREEN
      ENDIF
    ENDDO
  ELSE
    DO I = 1, NUMBER_OF_RAMPMETERS
      DO N = 1, 6
        IF(RAMPMETERS(I)%HEADWAY(N) .NE. RAMPMETERDATA(I)%HEADWAY(N)) THEN
          WRITE37(I, TIME_PERIOD) = .TRUE.
        ELSEIF(RAMPMETERDATA(I)%ONSET .NE. 0) THEN
          WRITE37(I, TIME_PERIOD) = .TRUE.
        ELSEIF(RAMPMETERS(I)%TWO_PERGREEN .NE. RAMPMETERDATA(I)%TWOPERGREEN) THEN
          WRITE37(I, TIME_PERIOD) = .TRUE.
        ENDIF
        IF(WRITE37(I, TIME_PERIOD)) THEN
          METER_HEADWAY1(I, TIME_PERIOD) = RAMPMETERDATA(I)%HEADWAY(1)
          METER_ONSET(I, TIME_PERIOD) = RAMPMETERDATA(I)%ONSET
          METER_TWOPERGREEN(I, TIME_PERIOD) = RAMPMETERS(I)%TWO_PERGREEN
          RAMPMETERS(I)%HEADWAY(N) = RAMPMETERDATA(I)%HEADWAY(N)
          RAMPMETERS(I)%ONSET = RAMPMETERDATA(I)%ONSET
          RAMPMETERS(I)%TWO_PERGREEN = RAMPMETERDATA(I)%TWOPERGREEN
        ENDIF
        IF(RAMPMETERS(I)%CONTROL .EQ. 1) EXIT
      ENDDO
    ENDDO
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION SET_NUMBER_OF_FREEWAY_DETECTORS[DLLEXPORT, STDCALL](N) RESULT(ERRORS)
  USE FREEWAY_DETECTORS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    ERRORS = 0
    N_FREEWAY_DETECTORS = N
    CALL ALLOCATE_FREEWAY_DETECTOR_ARRAYS
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'SET_NUMBER_OF_FREEWAY_DETECTORS CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION DEFINE_FREEWAY_DETECTORS[DLLEXPORT, STDCALL](INPUTS) RESULT(ERRORS)
  USE API_DATA
  USE FREEWAY_DETECTORS
  USE FREEWAY_LINKS
  IMPLICIT NONE
  TYPE(DETECTOR_INPUTS) :: INPUTS(N_FREEWAY_DETECTORS)
  INTEGER :: I, USN, DSN, IL, IDET, NDET
! ----------------------------------------------------------------------
  ERRORS = 0
  DO I = 1, N_FREEWAY_DETECTORS
    USN = INPUTS(I)%USN
    DSN = INPUTS(I)%DSN
    CALL FIND_FREEWAY_LINK(USN, DSN, IL)
    IF(IL .NE. 0) THEN
      IF(FFIRST_DETECTOR(IL) .EQ. 0) THEN
        FFIRST_DETECTOR(IL) = I
      ELSE
        IDET = FFIRST_DETECTOR(IL)
        DO WHILE(IDET .NE. 0)
          NDET = IDET            
          IDET = FDETECTOR(NDET)%NEXT_DET
        ENDDO
        FDETECTOR(NDET)%NEXT_DET = I
      ENDIF
      FDETECTOR(I)%LINK = IL
      FDETECTOR(I)%DETECTOR_LANES = INPUTS(I)%DETECTOR_LANES
      FDETECTOR(I)%LOCATION = INPUTS(I)%LOCATION
      FDETECTOR(I)%CARRYOVER_TIME = INPUTS(I)%CARRYOVER_TIME
      FDETECTOR(I)%DELAY_TIME = INPUTS(I)%DELAY_TIME
      FDETECTOR(I)%TYPE_CODE = 0
      FDETECTOR(I)%STATION_ID = INPUTS(I)%STATION_ID
      FDETECTOR(I)%ZONE_LENGTH = INPUTS(I)%ZONE_LENGTH
      FDETECTOR(I)%OPERATION_CODE = INPUTS(I)%OPERATION_CODE
    ENDIF
  ENDDO
  END
  
! ==================================================================================================
  INTEGER FUNCTION SET_NUMBER_OF_BUSROUTES[DLLEXPORT, STDCALL](N) RESULT(ERRORS)
  USE BUS_ROUTE_DATA
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    ERRORS = 0
    NUMBER_OF_ROUTES = N
    CALL ALLOCATE_BUS_ROUTE_DATA_ARRAYS
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'SET_NUMBER_OF_BUSROUTES CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION GET_NUMBER_OF_BUSSTATIONS[DLLEXPORT, STDCALL]()
  USE BUS_STATION_DATA
  IMPLICIT NONE
! ----------------------------------------------------------------------
  GET_NUMBER_OF_BUSSTATIONS = NUMBER_OF_BUSSTATIONS
  END
  
! ==================================================================================================
  INTEGER FUNCTION SET_NUMBER_OF_BUSSTATIONS[DLLEXPORT, STDCALL](N) RESULT(ERRORS)
  USE BUS_STATION_DATA
  USE BUS_ROUTE_DATA
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    ERRORS = 0
    NUMBER_OF_BUSSTATIONS = N
    CALL ALLOCATE_BUS_STATION_ARRAYS(NUMBER_OF_BUSSTATIONS + NUMBER_OF_ROUTES)
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'SET_NUMBER_OF_BUSSTATIONS CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION DEFINE_BUSROUTES[DLLEXPORT, STDCALL](INPUTS) RESULT(ERRORS)
  USE API_DATA
  USE BUS_ROUTE_DATA
  USE BUS_STATION_DATA
  USE NODE_TABLE
  USE FREEWAY_LINKS
  USE STREET_LINKS
  USE GLOBAL_DATA
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  TYPE(BUSR) :: INPUTS(NUMBER_OF_ROUTES)
  INTEGER :: I, NR, IL, INODE, LAST, USN, DSN, IS, ISTAT, NS, LASTLOC
  INTEGER :: MSTAT, ILX
  LOGICAL :: FOUND
! ----------------------------------------------------------------------
  ERRORS = 0
  IF(TIME_PERIOD .LE. 1) THEN
    MSTAT = 0
    DO I = 1, NUMBER_OF_ROUTES
      DO IS = 1, 100
        ISTAT = INPUTS(I)%STATIONLIST(IS)
        MSTAT = MAX(MSTAT, ISTAT)
      ENDDO
    ENDDO
    DO I = 1, NUMBER_OF_ROUTES
      NR = INPUTS(I)%NUMBER
      IF(NR .EQ. 0) CYCLE
      BUSR_TRAMCARS(NR) = INPUTS(I)%NUMBER_OF_TRAMCARS 
      ROUTE_HEADWAY(NR, TIME_PERIOD) = INPUTS(I)%HDWY
      ROUTE_OFFSET(NR, TIME_PERIOD) = INPUTS(I)%OFFSET
      BUSR_HDWY(NR) = INPUTS(I)%HDWY
      BUSR_OFFSET(NR) = INPUTS(I)%OFFSET
      BUSR_NNODES(NR) = INPUTS(I)%NNODES + 2
      FOUND = .FALSE.
      
      !Locate the entry node
      DO IL = 1, N_FREEWAY_LINKS
        IF(NODE_TYPE(FUSN(IL)) .EQ. NT_EXTERN .AND. FDSN(IL) .EQ. INPUTS(I)%ROUTE_NODES(1)) THEN
          FOUND = .TRUE.
          BUSR_ROUTE_NODES(NR, 1) = FUSN(IL)
          EXIT
        ENDIF
      ENDDO
      IF(.NOT. FOUND) THEN
        DO IL = 1, N_STREET_LINKS
          IF(NODE_TYPE(SUSN(IL)) .EQ. NT_EXTERN .AND. SDSN(IL) .EQ. INPUTS(I)%ROUTE_NODES(1)) THEN
            FOUND = .TRUE.
            BUSR_ROUTE_NODES(NR, 1) = SUSN(IL)
            EXIT
          ENDIF
        ENDDO
      ENDIF
      
      IF(.NOT. FOUND) THEN
        ERRORS = 1
        WRITE(MSGTEXT, '(A,I4)') 'DEFINE_BUSROUTES CANNOT LOCATE THE ENTRY NODE FOR BUS ROUTE ', NR
        CALL SENDTEXTMSG(M_ERROR)
        WRITE(MSGTEXT, '(I4)') INPUTS(I)%ROUTE_NODES(1)
        CALL SENDTEXTMSG(M_ERROR)
      ELSE      
        !Store the internal nodes
        DO INODE = 1, INPUTS(I)%NNODES
          BUSR_ROUTE_NODES(NR, INODE+1) = INPUTS(I)%ROUTE_NODES(INODE)
        ENDDO
      
        !Locate the exit node
        LAST = INPUTS(I)%ROUTE_NODES(INPUTS(I)%NNODES)
        ILX = 0
        IF(NETCODE(LAST) .EQ. I_FREEWAY) THEN
          DO IL = 1, N_FREEWAY_LINKS
            IF(NODE_TYPE(FDSN(IL)) .EQ. NT_EXTERN .AND. FUSN(IL) .EQ. LAST) THEN
              BUSR_ROUTE_NODES(NR, BUSR_NNODES(NR)) = FDSN(IL)
              EXIT
            ENDIF
          ENDDO
        ELSE
          DO IL = 1, N_STREET_LINKS
            IF(NODE_TYPE(SDSN(IL)) .EQ. NT_EXTERN .AND. SUSN(IL) .EQ. LAST) THEN
              BUSR_ROUTE_NODES(NR, BUSR_NNODES(NR)) = SDSN(IL)
              EXIT
            ENDIF
          ENDDO
        ENDIF
        BUSR_STATIONLIST(NR, 1:100) = INPUTS(I)%STATIONLIST
        DO IS = 1, 100
          ISTAT = INPUTS(I)%STATIONLIST(IS)
          IF(ISTAT .EQ. 0) EXIT
          FOUND = .FALSE.
          DO NS = 1, NUMBER_OF_BUSSTATIONS
            IF(BUS_STATION_LIST(NS)%STATION_NUMBER .EQ. ISTAT) THEN
              BUSR_STATIONLIST(NR, IS) = NS
              FOUND = .TRUE.
              EXIT
            ENDIF
          ENDDO
          IF(.NOT. FOUND) THEN
            ERRORS = 1
            WRITE(MSGTEXT, '(A,I4)') 'DEFINE_BUSROUTES CANNOT LOCATE BUS STATION # ', ISTAT
            CALL SENDTEXTMSG(M_ERROR)
            WRITE(MSGTEXT, '(I4)') INPUTS(I)%ROUTE_NODES(1)
            CALL SENDTEXTMSG(M_ERROR)
          ENDIF
        ENDDO
      ENDIF
      
      !Add exit stations 
      IF(NUMBER_OF_BUSSTATIONS .NE. 0) THEN
        USN = BUSR_ROUTE_NODES(NR, BUSR_NNODES(NR) - 2)
        DSN = BUSR_ROUTE_NODES(NR, BUSR_NNODES(NR) - 1)
        CALL FIND_STREET_LINK(USN, DSN, ILX)
        MSTAT = MSTAT + 1
        BUSR_STATIONLIST(NR, IS) = MSTAT
        BUS_STATION_LIST(MSTAT)%LINK = ILX
        BUS_STATION_LIST(MSTAT)%POCKET_LANE = 0
        BUS_STATION_LIST(MSTAT)%LOCATION = SLENGTH(ILX)
        BUS_STATION_LIST(MSTAT)%BLOCK_CODE = 0
        BUS_STATION_LIST(MSTAT)%CAPACITY = 1
        BUS_STATION_LIST(MSTAT)%TYPE_CODE = 1
        IF(FIRST_BUS_STATION(ILX) .EQ. 0) THEN
          FIRST_BUS_STATION(ILX) = MSTAT
        ELSE
          ISTAT = FIRST_BUS_STATION(ILX)
          DO WHILE(ISTAT .NE. 0)
            IS = ISTAT
            ISTAT = BUS_STATION_LIST(ISTAT)%NEXT_STATION
          ENDDO
          BUS_STATION_LIST(IS)%NEXT_STATION = MSTAT
        ENDIF
        BUS_STATION_LIST(MSTAT)%DWELL = 1
        BUS_STATION_LIST(MSTAT)%BYPASS_PCT = 1.0
        BUS_STATION_LIST(MSTAT)%STATION_NUMBER = MSTAT
        STATION_DWELL(MSTAT, TIME_PERIOD) = 0 
        STATION_BYPASS_PCT(MSTAT, TIME_PERIOD) = 1.0
      ENDIF
      
      !Determine station to station distances
      LASTLOC = 0
      DO INODE = 1, BUSR_NNODES(NR) - 1
        USN = BUSR_ROUTE_NODES(NR, INODE)
        DSN = BUSR_ROUTE_NODES(NR, INODE + 1)
        CALL FIND_STREET_LINK(USN, DSN, IL)
        IF(IL .NE. 0) THEN
          !Identify tram links
          IF(INPUTS(I)%TYPE_OF_ROUTE .EQ. 1) THEN
            TRAM_SIMULATION = .TRUE.
            TRAM_LINK(IL) = .TRUE.
          ENDIF
          IF(FIRST_BUS_STATION(IL) .NE. 0) THEN
            ISTAT = FIRST_BUS_STATION(IL)
            DO WHILE(ISTAT .NE. 0)
              BUS_STATION_LIST(ISTAT)%ROUTE_LOCATION = BUSR_DIST(NR) + BUS_STATION_LIST(ISTAT)%LOCATION
              BUS_STATION_LIST(ISTAT)%APPROACH_DISTANCE = BUS_STATION_LIST(ISTAT)%ROUTE_LOCATION - LASTLOC
              LASTLOC = BUS_STATION_LIST(ISTAT)%ROUTE_LOCATION
              ISTAT = BUS_STATION_LIST(ISTAT)%NEXT_STATION
            ENDDO
          ENDIF
          BUSR_DIST(NR) = BUSR_DIST(NR) + SLENGTH(IL)
        ENDIF
      ENDDO
      
    ENDDO
  ELSE
    DO I = 1, NUMBER_OF_ROUTES
      NR = INPUTS(I)%NUMBER
      IF(INPUTS(I)%HDWY .NE. BUSR_HDWY(NR) .OR. INPUTS(I)%OFFSET .NE. BUSR_OFFSET(NR)) THEN 
        BUSR_HDWY(NR) = INPUTS(I)%HDWY
        BUSR_OFFSET(NR) = INPUTS(I)%OFFSET
        WRITE189(NR, TIME_PERIOD) = .TRUE.
        ROUTE_HEADWAY(NR, TIME_PERIOD) = INPUTS(I)%HDWY
        ROUTE_OFFSET(NR, TIME_PERIOD) = INPUTS(I)%OFFSET
        IF(BUSR_HDWY(NR) .EQ. 0) THEN
          ERRORS = ERRORS + 1
          WRITE(MSGTEXT, '(A)') 'DEFINE_BUSROUTES: BUS ROUTE HEADWAY CANNOT BE 0 SECONDS'
          CALL SENDTEXTMSG(M_ERROR)
          WRITE(MSGTEXT, '(A, I4)') '  ROUTE #', NR
          CALL SENDTEXTMSG(M_ERROR)
        ENDIF
      ENDIF
    ENDDO
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION DEFINE_BUSSTATIONS[DLLEXPORT, STDCALL](INPUTS) RESULT(ERRORS)
  USE API_DATA
  USE BUS_STATION_DATA
  USE STREET_LINKS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  TYPE(BUSSTATION_DATA) :: INPUTS(NUMBER_OF_BUSSTATIONS)
  INTEGER :: NS, IL, ISTAT, I
! ----------------------------------------------------------------------
  ERRORS = 0
  IF(TIME_PERIOD .LE. 1) THEN
    IF(SUM(DWELL_MULTIPLIER) .EQ. 0.0) CALL DEFINE_DWELL_MULTIPLIERS
    DO NS = 1, NUMBER_OF_BUSSTATIONS
      IF(INPUTS(NS)%USN .EQ. 0 .AND. INPUTS(NS)%DSN .EQ. 0) CYCLE
      CALL FIND_STREET_LINK(INPUTS(NS)%USN, INPUTS(NS)%DSN, IL)
      IF(IL .EQ. 0) THEN
        ERRORS = ERRORS + 1
        WRITE(MSGTEXT, '(A, I4)') 'LINK NOT FOUND FOR BUS STATION ', NS
        CALL SENDTEXTMSG(M_ERROR)
        WRITE(MSGTEXT, '(A, 2I5)') '  LINK = ', INPUTS(NS)%USN, INPUTS(NS)%DSN
        CALL SENDTEXTMSG(M_ERROR)
      ELSE
        BUS_STATION_LIST(NS)%LINK = IL
        BUS_STATION_LIST(NS)%POCKET_LANE = 0
        BUS_STATION_LIST(NS)%LOCATION = INPUTS(NS)%LOCATION
        BUS_STATION_LIST(NS)%BLOCK_CODE = INPUTS(NS)%BLOCK_CODE
        IF(SLENGTH(IL) - BUS_STATION_LIST(NS)%LOCATION .LE. 50 .AND. NS .LT. 64) THEN
          NUMBER_RIGHTPOCKETS(IL) = 1
          BUS_STATION_LIST(NS)%POCKET_LANE = 7 - NUMBER_LEFTPOCKETS(IL)
        ENDIF
        BUS_STATION_LIST(NS)%CAPACITY = INPUTS(NS)%CAPACITY
        BUS_STATION_LIST(NS)%TYPE_CODE = INPUTS(NS)%TYPE_CODE
        IF(FIRST_BUS_STATION(IL) .EQ. 0) THEN
          FIRST_BUS_STATION(IL) = INPUTS(NS)%STATION_NUMBER
        ELSE
          ISTAT = FIRST_BUS_STATION(IL)
          DO WHILE(ISTAT .NE. 0)
            I = ISTAT
            ISTAT = BUS_STATION_LIST(ISTAT)%NEXT_STATION
          ENDDO
          BUS_STATION_LIST(I)%NEXT_STATION = INPUTS(NS)%STATION_NUMBER
        ENDIF
      ENDIF
      BUS_STATION_LIST(NS)%DWELL = INPUTS(NS)%DWELL
      BUS_STATION_LIST(NS)%BYPASS_PCT = INPUTS(NS)%BYPASS_PCT / 100.
      BUS_STATION_LIST(NS)%STATION_NUMBER = INPUTS(NS)%STATION_NUMBER
      STATION_DWELL(NS, TIME_PERIOD) = BUS_STATION_LIST(NS)%DWELL 
      STATION_BYPASS_PCT(NS, TIME_PERIOD) = BUS_STATION_LIST(NS)%BYPASS_PCT 
    ENDDO
  ELSE
    DO NS = 1, NUMBER_OF_BUSSTATIONS
      IF(INPUTS(NS)%DWELL .NE. BUS_STATION_LIST(NS)%DWELL .OR. &
         INPUTS(NS)%BYPASS_PCT / 100. .NE. BUS_STATION_LIST(NS)%BYPASS_PCT) THEN
        BUS_STATION_LIST(NS)%DWELL = INPUTS(NS)%DWELL
        BUS_STATION_LIST(NS)%BYPASS_PCT = INPUTS(NS)%BYPASS_PCT / 100.
        WRITE186(NS, TIME_PERIOD) = .TRUE.
        STATION_DWELL(NS, TIME_PERIOD) = BUS_STATION_LIST(NS)%DWELL 
        STATION_BYPASS_PCT(NS, TIME_PERIOD) = BUS_STATION_LIST(NS)%BYPASS_PCT 
      ENDIF
    ENDDO
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION SET_NUMBER_OF_STREET_DETECTORS[DLLEXPORT, STDCALL](N) RESULT(ERRORS)
  USE STREET_DETECTORS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    ERRORS = 0
    N_STREET_DETECTORS = N
    CALL ALLOCATE_STREET_DETECTOR_ARRAYS
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'SET_NUMBER_OF_STREET_DETECTORS CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION DEFINE_STREET_DETECTORS[DLLEXPORT, STDCALL](INPUTS) RESULT(ERRORS)
  USE API_DATA
  USE ACTUATED_CONTROLLERS
  USE STREET_DETECTORS
  USE STREET_LINKS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  TYPE(DETECTOR_INPUTS) :: INPUTS(N_STREET_DETECTORS)
  INTEGER :: I, USN, DSN, IL, NDET, IDET, NCOUNT, ISIG, SIGNAL_NODE, INODE
! ----------------------------------------------------------------------
  ERRORS = 0
  IF(TIME_PERIOD .LE. 1) THEN
    DO I = 1, N_STREET_DETECTORS
      SDETECTOR(I)%NEXT_DET = 0
    ENDDO
    DO I = 1, N_STREET_DETECTORS
      USN = INPUTS(I)%USN
      DSN = INPUTS(I)%DSN
      CALL FIND_STREET_LINK(USN, DSN, IL)
      IF(IL .NE. 0) THEN
        IF(SFIRST_DETECTOR(IL) .EQ. 0) THEN
          SFIRST_DETECTOR(IL) = I
        ELSE
          IDET = SFIRST_DETECTOR(IL)
          DO WHILE(IDET .NE. 0)
            NDET = IDET            
            IDET = SDETECTOR(NDET)%NEXT_DET
          ENDDO
          IF(NDET .NE. I) SDETECTOR(NDET)%NEXT_DET = I
        ENDIF
        SDETECTOR(I)%LINK = IL
        SDETECTOR(I)%SIGNAL_NODE = DSN
        SDETECTOR(I)%DETECTOR_LANES = INPUTS(I)%DETECTOR_LANES
        SDETECTOR(I)%LOCATION = SLENGTH(IL) - INPUTS(I)%LOCATION
        SDETECTOR(I)%CARRYOVER_TIME = INPUTS(I)%CARRYOVER_TIME
        SDETECTOR(I)%DELAY_TIME = INPUTS(I)%DELAY_TIME
        SDETECTOR(I)%OPERATION_CODE = INPUTS(I)%OPERATION_CODE
        SDETECTOR(I)%STATION_ID = INPUTS(I)%STATION_ID
        SDETECTOR(I)%ZONE_LENGTH = INPUTS(I)%ZONE_LENGTH
        SDETECTOR(I)%ASSOCIATED_PHASE = INPUTS(I)%ASSOCIATED_PHASE
      
        !Determine the number of lanes covered by the detector
        IF(SDETECTOR(I)%LANE1 .GE. 1 .AND. SDETECTOR(I)%LANE1 .LE. N_STREET_LANES) THEN
          SDETECTOR(I)%LANES_COVERED = SDETECTOR(I)%LANES_COVERED + 1
        ELSEIF(SDETECTOR(I)%LANE1 .EQ. 100) THEN
          SDETECTOR(I)%LANES_COVERED = SDETECTOR(I)%LANES_COVERED + SNUMLANES(IL)
        ELSEIF(SDETECTOR(I)%LANE1 .EQ. 200) THEN
          SDETECTOR(I)%LANES_COVERED = SDETECTOR(I)%LANES_COVERED + TOTAL_LANES(IL)
        ENDIF
        IF(SDETECTOR(I)%LANE2 .GE. 1 .AND. SDETECTOR(I)%LANE2 .LE. N_STREET_LANES) THEN
          SDETECTOR(I)%LANES_COVERED = SDETECTOR(I)%LANES_COVERED + 1
        ELSEIF(SDETECTOR(I)%LANE2 .EQ. 100) THEN
          SDETECTOR(I)%LANES_COVERED = SDETECTOR(I)%LANES_COVERED + SNUMLANES(IL)
        ELSEIF(SDETECTOR(I)%LANE2 .EQ. 200) THEN
          SDETECTOR(I)%LANES_COVERED = SDETECTOR(I)%LANES_COVERED + TOTAL_LANES(IL)
        ENDIF

        IF(SDETECTOR(I)%ASSOCIATED_PHASE .NE. 0) THEN
          DO ISIG = 1, NUMBER_OF_ACS
            DO INODE = 1, 8
              IF(AC_SIGNALS(ISIG)%NODE(INODE) .EQ. 0) EXIT
              IF(AC_SIGNALS(ISIG)%NODE(INODE) .EQ. SDETECTOR(I)%SIGNAL_NODE) THEN
                NCOUNT = AC_SIGNALS(ISIG)%SDP%DETECTOR_COUNT + 1
                AC_SIGNALS(ISIG)%SDP%DETECTOR_COUNT = NCOUNT
                AC_SIGNALS(ISIG)%SDP%DETECTOR_LIST(NCOUNT) = I
              ENDIF
            ENDDO
          ENDDO
        ENDIF
   
      ENDIF
    ENDDO
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'DEFINE_STREET_DETECTORS CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION SET_NUMBER_OF_LONGTERM_EVENTS[DLLEXPORT, STDCALL](N) RESULT(ERRORS)
  USE EVENTS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    ERRORS = 0
    NUMBER_OF_LONGTERM_EVENTS = N
    NUMBER_OF_EVENTS = NUMBER_OF_EVENTS + N
    CALL ALLOCATE_EVENT_ARRAYS(NUMBER_OF_EVENTS)
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'SET_NUMBER_OF_LONGTERM_EVENTS CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION DEFINE_LONGTERM_EVENTS[DLLEXPORT, STDCALL](INPUTS) RESULT(ERRORS)
  USE API_DATA
  USE EVENTS
  USE STREET_LINKS
  USE TEXT
  IMPLICIT NONE
  TYPE(EVENT_DATA) :: INPUTS(NUMBER_OF_LONGTERM_EVENTS)
  INTEGER :: I, IDUR, INCTYP, LANES_AFFECTED(N_STREET_LANES), IL, GROUP
! ----------------------------------------------------------------------
  ERRORS = 0
  DO I = 1, NUMBER_OF_LONGTERM_EVENTS
    CALL FIND_STREET_LINK(INPUTS(I)%USN, INPUTS(I)%DSN, IL)
    IF(IL .EQ. 0) THEN
      ERRORS = ERRORS + 1
      WRITE(MSGTEXT, '(A, 2I4)') 'LINK NOT FOUND FOR LONGTERM EVENT ', INPUTS(I)%USN, INPUTS(I)%DSN
      CALL SENDTEXTMSG(M_ERROR)
    ELSE
      EVENT_LINK(I) = IL
      EVENT_LANE(I) = INPUTS(I)%LANE
      EVENT_BEGIN_TIME(I) = INPUTS(I)%BEGIN_TIME
      EVENT_END_TIME(I) = INPUTS(I)%END_TIME
      EVENT_LOCATION(I) = INPUTS(I)%LOCATION
      EVENT_TYPE(I) = 2
      EVENT_SPEED_REDUCTION(I) = INPUTS(I)%SPEED_REDUCTION
      EVENT_LENGTH(I) = INPUTS(I)%LENGTH
      EVENT_CODE(I) = INPUTS(I)%CODE
      EVENT_GROUP_ID(I) = INPUTS(I)%GROUP_ID
      EVENT_PRIMARY(I) = .FALSE.
      IF(INPUTS(I)%GROUP_ID .NE. GROUP) THEN
        GROUP = INPUTS(I)%GROUP_ID
        EVENT_PRIMARY(I) = .TRUE.
      ENDIF
      CALL FIND_STREET_LINK(INPUTS(I)%APPROACH_USN, INPUTS(I)%APPROACH_DSN, IL)
      PED_APPROACH_LINK(I) = IL
    ENDIF
  ENDDO
  END
  
! ==================================================================================================
  INTEGER FUNCTION ADD_LONGTERM_EVENT[DLLEXPORT, STDCALL](EVENT) RESULT(ERRORS)
  USE API_DATA
  USE EVENTS
  USE STREET_LINKS
  USE TEXT
  IMPLICIT NONE
  TYPE(EVENT_DATA) :: EVENT
  INTEGER :: IL
! ----------------------------------------------------------------------
  ERRORS = 0
  CALL FIND_STREET_LINK(EVENT%USN, EVENT%DSN, IL)
  IF(IL .EQ. 0) THEN
    ERRORS = ERRORS + 1
    WRITE(MSGTEXT, '(A, 2I4)') 'LINK NOT FOUND FOR LONGTERM EVENT ', EVENT%USN, EVENT%DSN
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
    CALL NEW_EVENT
    EVENT_LINK(NUMBER_OF_EVENTS) = IL
    EVENT_LANE(NUMBER_OF_EVENTS) = EVENT%LANE
    EVENT_BEGIN_TIME(NUMBER_OF_EVENTS) = EVENT%BEGIN_TIME
    EVENT_END_TIME(NUMBER_OF_EVENTS) = EVENT%END_TIME
    EVENT_LOCATION(NUMBER_OF_EVENTS) = EVENT%LOCATION
    EVENT_TYPE(NUMBER_OF_EVENTS) = 2
    EVENT_SPEED_REDUCTION(NUMBER_OF_EVENTS) = EVENT%SPEED_REDUCTION
    EVENT_LENGTH(NUMBER_OF_EVENTS) = EVENT%LENGTH
    EVENT_CODE(NUMBER_OF_EVENTS) = EVENT%CODE
    EVENT_GROUP_ID(NUMBER_OF_EVENTS) = EVENT%GROUP_ID
    CALL FIND_STREET_LINK(EVENT%APPROACH_USN, EVENT%APPROACH_DSN, IL)
    PED_APPROACH_LINK(NUMBER_OF_EVENTS) = IL
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION SET_NUMBER_OF_PARKING_ZONES[DLLEXPORT, STDCALL](N) RESULT(ERRORS)
  USE PARKING
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    ERRORS = 0
    NUMBER_OF_PARKING_ZONES = N
    CALL ALLOCATE_PARKING_ARRAYS
    IF(N .GT. 1) CALL REALLOCATE_PARKING_ARRAYS
    ALLOCATE(WRITE56(N, N_PERIODS))
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'SET_NUMBER_OF_PARKING_ZONES CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION DEFINE_PARKING_ZONES[DLLEXPORT, STDCALL](INPUTS) RESULT(ERRORS)
  USE API_DATA
  USE PARKING
  USE SIMPARAMS
  USE STREET_LINKS
  USE TEXT
  IMPLICIT NONE
  TYPE(PARKING_DATA) :: INPUTS(NUMBER_OF_PARKING_ZONES)
  INTEGER :: I, IL
! ----------------------------------------------------------------------
  ERRORS = 0
  DO I = 1, NUMBER_OF_PARKING_ZONES
    IF(TIME_PERIOD .EQ. 1) WRITE56(I, 1) = .TRUE.
    CALL FIND_STREET_LINK(INPUTS(I)%USN, INPUTS(I)%DSN, IL)
    IF(IL .EQ. 0) THEN
      ERRORS = ERRORS + 1
      WRITE(MSGTEXT, '(A, 2I4)') 'LINK NOT FOUND FOR PARKING ZONE ', INPUTS(I)%USN, INPUTS(I)%DSN
      CALL SENDTEXTMSG(M_ERROR)
    ELSE
      IF(TIME_PERIOD .EQ. 1) THEN
        PARK_LEFT_START(I) = INPUTS(I)%LEFT_START
        PARK_LEFT_LEN(I) = INPUTS(I)%LEFT_LEN
        PARK_RIGHT_START(I) = INPUTS(I)%RIGHT_START
        PARK_RIGHT_LEN(I) = INPUTS(I)%RIGHT_LEN
        PARKING_ZONE_LINK(I) = IL
      ENDIF
      IF(INPUTS(I)%DURATION .NE. PARK_DURATION(I) .OR. INPUTS(I)%FREQ .NE. PARK_FREQ(I)) THEN
        WRITE56(I, TIME_PERIOD) = .TRUE.
        PARK_DURATION(I) = INPUTS(I)%DURATION
        PARK_FREQ(I) = INPUTS(I)%FREQ
      ENDIF
    ENDIF
  ENDDO
  END
  
! ==================================================================================================
  INTEGER FUNCTION SET_NUMBER_OF_INCIDENTS[DLLEXPORT, STDCALL](N) RESULT(ERRORS)
  USE INCIDENTS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    ERRORS = 0
    NUMBER_OF_INCIDENTS = N
    CALL ALLOCATE_INCIDENT_ARRAYS
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'SET_NUMBER_OF_INCIDENTS CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION DEFINE_INCIDENTS[DLLEXPORT, STDCALL](INPUTS) RESULT(ERRORS)
  USE API_DATA
  USE INCIDENTS
  USE SIMPARAMS
  USE TEXT
  USE FREEWAY_LINKS
  USE EVENTS
  IMPLICIT NONE
  TYPE(INCIDENT_DATA) :: INPUTS(NUMBER_OF_INCIDENTS)
  INTEGER :: I, IL, INCTYP, DURATION, ILENGTH
! ----------------------------------------------------------------------
  ERRORS = 0
  DO I = 1, NUMBER_OF_INCIDENTS
    CALL FIND_FREEWAY_LINK(INPUTS(I)%USN, INPUTS(I)%DSN, IL)
    IF(IL .EQ. 0) THEN
      ERRORS = ERRORS + 1
      WRITE(MSGTEXT, '(A, I4)') 'LINK NOT SPECIFIED FOR INCIDENT NUMBER ', I
      CALL SENDTEXTMSG(M_ERROR)
    ELSE
      INCIDENT_LINK(I) = IL
      INCIDENT_BEGIN_POINT(I) = INPUTS(I)%BEGIN_POINT
      INCIDENT_BEGIN_TIME(I) = INPUTS(I)%BEGIN_TIME
      INCIDENT_END_POINT(I) = INPUTS(I)%END_POINT
      INCIDENT_END_TIME(I) = INPUTS(I)%END_TIME
      INCIDENT_RBNF(I) = INPUTS(I)%RBNF
      INCIDENT_WARN_POINT(I) = INPUTS(I)%WARN_POINT
      INCIDENT_CODE(I, 1:N_FREEWAY_LANES) = INPUTS(I)%CODE
      INCTYP = 1                                  
      DURATION = INPUTS(I)%END_TIME - INPUTS(I)%BEGIN_TIME
      ILENGTH = INPUTS(I)%END_POINT - INPUTS(I)%BEGIN_POINT
      CALL ADDFREEWAYEVENT(IL, INPUTS(I)%BEGIN_TIME, DURATION, INCTYP,  &
          INPUTS(I)%CODE, INPUTS(I)%BEGIN_POINT, ILENGTH, INPUTS(I)%WARN_POINT, INPUTS(I)%RBNF)
    ENDIF
  ENDDO
  END
  
! ==================================================================================================
  INTEGER FUNCTION DEFINE_NODE_COORDINATES[DLLEXPORT, STDCALL](INPUTS) RESULT(ERRORS)
  USE API_DATA
  USE NODE_TABLE
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  TYPE(NODE_LOCATION_DATA) :: INPUTS(MAX_NODE_NUMBER)
  INTEGER :: I
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    ERRORS = 0
    DO I = 1, MAX_NODE_NUMBER
      X195(I) = INPUTS(I)%X
      Y195(I) = INPUTS(I)%Y
      NODE_LAT(I) = INPUTS(I)%LATITUDE
      NODE_LON(I) = INPUTS(I)%LONGITUDE
      NODE_ELEV(I) = INPUTS(I)%ELEVATION
      IS_DEFINED(I) = INPUTS(I)%IS_DEFINED
    ENDDO
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'DEFINE_NODE_COORDINATES CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION DEFINE_VEHICLE_TYPES[DLLEXPORT, STDCALL](INPUTS) RESULT(ERRORS)
  USE API_DATA
  USE VEHICLE_TYPES
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  TYPE(VEHICLE_TYPE_DATA) :: INPUTS(NTYPES)
  INTEGER :: I
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    ERRORS = 0
    WRITE58 = .TRUE.
    WRITE71 = .TRUE.
    DO I = 1, NTYPES
      VTLENGTH(I) = INPUTS(I)%LENGTH
      HDWY_FACTOR(I) = INPUTS(I)%HEADWAY_FACTOR / 100.
      AVG_OCCS(I) = INPUTS(I)%AVERAGE_OCCUPANCY
      NEMDEC(I) = INPUTS(I)%NON_EMERGENCY_DECEL
      FLT_FREEWAY_AUTO(I) = INPUTS(I)%FLEET_FREEWAY_AUTO
      FLT_FREEWAY_TRUCK(I) = INPUTS(I)%FLEET_FREEWAY_TRUCK
      FLT_FREEWAY_CARPOOL(I) = INPUTS(I)%FLEET_FREEWAY_CARPOOL
      FLT_FREEWAY_BUS(I) = INPUTS(I)%FLEET_FREEWAY_BUS
      FLT_FREEWAY_EV(I) = INPUTS(I)%FLEET_FREEWAY_EV
      FLT_STREET_AUTO(I) = INPUTS(I)%FLEET_STREET_AUTO
      FLT_STREET_TRUCK(I) = INPUTS(I)%FLEET_STREET_TRUCK
      FLT_STREET_CARPOOL(I) = INPUTS(I)%FLEET_STREET_CARPOOL
      FLT_STREET_BUS(I) = INPUTS(I)%FLEET_STREET_BUS
      FLT_STREET_EV(I) = INPUTS(I)%FLEET_STREET_EV
      FLT_STREET_BIKE(I) = INPUTS(I)%FLEET_STREET_BIKE
      PCT_PITT(I) = INPUTS(I)%PCT_PITT
      PCT_IDM(I) = INPUTS(I)%PCT_IDM
      PCT_ACC(I) = INPUTS(I)%PCT_ACC
      PCT_CACC(I) = INPUTS(I)%PCT_CACC
    ENDDO
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'DEFINE_VEHICLE_TYPES CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION SET_NUMBER_OF_DIVERSIONS[DLLEXPORT, STDCALL](N) RESULT(ERRORS)
  USE DIVERSIONS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    ERRORS = 0
    NUMBER_OF_DIVERSIONS = N
    CALL ALLOCATE_DIVERSION_ARRAYS
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'SET_NUMBER_OF_DIVERSIONS CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION DEFINE_DIVERSIONS[DLLEXPORT, STDCALL](INPUTS) RESULT(ERRORS)
  USE API_DATA
  USE DIVERSIONS
  USE FREEWAY_LINKS
  USE TEXT
  IMPLICIT NONE
  TYPE(DIVERSION_DATA) :: INPUTS(NUMBER_OF_DIVERSIONS)
  INTEGER :: I, IL
! ----------------------------------------------------------------------
  ERRORS = 0
  DO I = 1, NUMBER_OF_DIVERSIONS
    CALL FIND_FREEWAY_LINK(INPUTS(I)%USN, INPUTS(I)%DSN, IL)
    IF(IL .EQ. 0) THEN
      ERRORS = ERRORS + 1
      WRITE(MSGTEXT, '(A, 2I4)') 'LINK NOT FOUND FOR FREEWAY DIVERSION ', INPUTS(I)%USN, INPUTS(I)%DSN
      CALL SENDTEXTMSG(M_ERROR)
    ELSE
      DIVERSION_LINK(I) = IL
      DIVERSION_BEGIN_TIME(I) = INPUTS(I)%BEGIN_TIME
      DIVERSION_END_TIME(I) = INPUTS(I)%END_TIME
      DIVERSION_LOCATION(I) = INPUTS(I)%LOCATION
      DIVERSION_PATHID(I) = INPUTS(I)%PATHID
      DIVERSION_PERCENTAGE(I) = INPUTS(I)%PERCENTAGE
      DIVERSION_SPEED(I) = INPUTS(I)%SPEED
    ENDIF
  ENDDO
  END  

! ==================================================================================================
  INTEGER FUNCTION DEFINE_CONDITIONAL_TURNPCTS[DLLEXPORT, STDCALL](INPUTS) RESULT(ERRORS)
  USE API_DATA
  USE STREET_LINKS
  USE SIMPARAMS
  USE TEXT
  USE NODE_TABLE
  IMPLICIT NONE
  TYPE(COND_TURNPCTS) :: INPUTS
  INTEGER :: IL
  REAL :: INPUT(5)
! ----------------------------------------------------------------------
  ERRORS = 0
  IF(NODE_TYPE(INPUTS%DSN) .EQ. NT_EXTERN) THEN
    ERRORS = ERRORS + 1
    WRITE(MSGTEXT, '(A, 2I4)') 'CONDITIONAL TURN PERCENTAGES ENTERED FOR AN EXIT LINK ', INPUTS%USN, INPUTS%DSN
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
    CALL FIND_STREET_LINK(INPUTS%USN, INPUTS%DSN, IL)
    IF(IL .NE. 0) THEN
      IF(SUM(INPUTS%LEFTPCT) .NE. 0) THEN
        COND_LEFT(IL) = .TRUE.
        CALL NORMALIZE5(INPUTS%LEFTPCT, INPUT)
        COND_LEFTPCT(IL, 1) = INPUT(1)
        COND_LEFTPCT(IL, 2) = INPUT(2)
        COND_LEFTPCT(IL, 3) = INPUT(3)
        COND_LEFTPCT(IL, 4) = INPUT(4)
        COND_LEFTPCT(IL, 5) = INPUT(5)
      ENDIF
      IF(SUM(INPUTS%THRUPCT) .NE. 0) THEN
        COND_THRU(IL) = .TRUE.
        CALL NORMALIZE5(INPUTS%THRUPCT, INPUT)
        COND_THRUPCT(IL, 1) = INPUT(1)
        COND_THRUPCT(IL, 5) = INPUT(5)
        COND_THRUPCT(IL, 2) = INPUT(2)
        COND_THRUPCT(IL, 3) = INPUT(3)
        COND_THRUPCT(IL, 4) = INPUT(4)
      ENDIF
      IF(SUM(INPUTS%RIGHTPCT) .NE. 0) THEN
        COND_RIGHT(IL) = .TRUE.
        CALL NORMALIZE5(INPUTS%RIGHTPCT, INPUT)
        COND_RIGHTPCT(IL, 1) = INPUT(1)
        COND_RIGHTPCT(IL, 2) = INPUT(2)
        COND_RIGHTPCT(IL, 3) = INPUT(3)
        COND_RIGHTPCT(IL, 4) = INPUT(4)
        COND_RIGHTPCT(IL, 5) = INPUT(5)
      ENDIF
      IF(SUM(INPUTS%LDIAGPCT) .NE. 0) THEN
        COND_LDIAG(IL) = .TRUE.
        CALL NORMALIZE5(INPUTS%LDIAGPCT, INPUT)
        COND_LDIAGPCT(IL, 1) = INPUT(1)
        COND_LDIAGPCT(IL, 2) = INPUT(2)
        COND_LDIAGPCT(IL, 3) = INPUT(3)
        COND_LDIAGPCT(IL, 4) = INPUT(4)
        COND_LDIAGPCT(IL, 5) = INPUT(5)
      ENDIF
      IF(SUM(INPUTS%RDIAGPCT) .NE. 0) THEN
        COND_RDIAG(IL) = .TRUE.
        CALL NORMALIZE5(INPUTS%RDIAGPCT, INPUT)
        COND_RDIAGPCT(IL, 1) = INPUT(1)
        COND_RDIAGPCT(IL, 2) = INPUT(2)
        COND_RDIAGPCT(IL, 3) = INPUT(3)
        COND_RDIAGPCT(IL, 4) = INPUT(4)
        COND_RDIAGPCT(IL, 5) = INPUT(5)
      ENDIF
    ENDIF
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION WRITE_ANIMATION_FILES[DLLEXPORT, STDCALL] RESULT(RETVAL)
  USE SIMPARAMS
  IMPLICIT NONE
!----------------------------------------------------------------------
! Returns: int =  0 - success
!              = -1 - currently in the initialization period
! ----------------------------------------------------------------------
  IF(INITMODE) THEN
    RETVAL = -1
  ELSE
    RETVAL = 0
    !Temporarily set SIMTIME back one time step to be in sync with TSIS animations.
    SIMTIME = SIMTIME - TIMESTEP
    CALL WRITE_ANIMATION_DATA
    CALL SENDEVENTDATA
    SIMTIME = SIMTIME + TIMESTEP    
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_SIGNAL[DLLEXPORT, STDCALL] (USN, DSN, NL, NT, NR, ND)
! ----------------------------------------------------------------------
! Description: Returns the link signal codes for the specified link.
!
!
! Returns: int =  0 - success
!              = -1 - specified link was not found
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE STREET_NODES
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: USN, DSN
  INTEGER :: NL [REFERENCE]
  INTEGER :: NT [REFERENCE]
  INTEGER :: NR [REFERENCE]
  INTEGER :: ND [REFERENCE]
  INTEGER :: IL, ICODE, NLX, NTX, NRX, NDX
  LOGICAL :: AMBER, AMBERX
! ----------------------------------------------------------------------
  IL = 0
  CALL FIND_STREET_LINK(USN, DSN, IL)
  IF(IL .EQ. 0) THEN
    GET_SIGNAL = -1
  ELSEIF(FTC_SIGNAL_ID(IL) .NE. 0) THEN
    GET_SIGNAL = 0
    ICODE = SIGNAL_CODE(IL)
    CALL DECODE_FTC(ICODE, NL, NT, NR, ND, AMBER)
    IF(NL .EQ. S_GREEN .AND. OPPOSE_LINK(IL) .NE. 0) THEN
      ICODE = SIGNAL_CODE(OPPOSE_LINK(IL))
      CALL DECODE_FTC(ICODE, NLX, NTX, NRX, NDX, AMBERX)
      IF(NTX .NE. S_RED) NL = S_PERGRN
    ENDIF
    IF(AMBER) THEN
      IF(NL .NE. S_RED) NL = S_AMBER
      IF(NT .NE. S_RED) NT = S_AMBER
      IF(NR .NE. S_RED) NR = S_AMBER
      IF(ND .NE. S_RED) ND = S_AMBER
    ENDIF
  ELSEIF(AC_SIGNAL_ID(IL) .NE. 0) THEN
    GET_SIGNAL = 0
    NL = S_RED
    NT = S_RED
    NR = S_RED
    ND = S_RED
    IF(SIGNAL_LEFT(IL)) THEN
      NL = S_GREEN
      IF(OPPOSE_LINK(IL) .NE. 0) THEN
        IF(SIGNAL_THRU(OPPOSE_LINK(IL))) NL = S_PERGRN
      ENDIF
    ENDIF
    IF(SIGNAL_THRU(IL)) NT = S_GREEN
    IF(SIGNAL_RIGHT(IL)) NR = S_GREEN
    IF(SIGNAL_DIAG(IL)) ND = S_GREEN
    IF(AMBER_LEFT(IL)) NL = S_AMBER
    IF(AMBER_THRU(IL)) NT = S_AMBER
    IF(AMBER_RIGHT(IL)) NR = S_AMBER
    IF(AMBER_DIAG(IL)) ND = S_AMBER
  ENDIF
  END  

! ==================================================================================================
  INTEGER FUNCTION SET_EXTERNAL_ACTUATED_CONTROL[DLLEXPORT, STDCALL] (NODE, SETTING) RESULT(ERRORS)
  USE ACTUATED_CONTROLLERS
  USE STREET_NODES
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NODE
  LOGICAL, INTENT(IN) :: SETTING
  INTEGER :: IACT
! ----------------------------------------------------------------------
  IACT = NACT(NODE)
  IF(IACT .NE. 0) THEN
    AC_SIGNALS(IACT)%EXTERNAL_CONTROL = SETTING
    ERRORS = 0
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'SET_EXTERNAL_ACTUATED_CONTROL CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION SET_NUMBER_OF_ROUNDABOUTS[DLLEXPORT, STDCALL](N) RESULT(ERRORS)
  USE ROUNDABOUT_DATA
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    ERRORS = 0
    NUMBER_OF_ROUNDABOUTS = N
    CALL ALLOCATE_ROUNDABOUT_ARRAYS
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'SET_NUMBER_OF_ROUNDABOUTS CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION DEFINE_ROUNDABOUTS[DLLEXPORT, STDCALL](ROUNDABOUTDATA) RESULT(ERRORS)
  USE API_DATA
  USE ROUNDABOUT_DATA
  USE SIMPARAMS
  USE STREET_LINKS
  USE TEXT
  IMPLICIT NONE
  TYPE(RABT_API_DATA), INTENT(IN) :: ROUNDABOUTDATA(NUMBER_OF_ROUNDABOUTS)
  INTEGER :: IRND, IAP, IRL, IL, ERROR, N, USN, DSN
  REAL :: TOTAL, PCT(5)
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    ERRORS = 0
    DO IRND = 1, NUMBER_OF_ROUNDABOUTS
      ROUNDABOUT(IRND)%APPROACHES = ROUNDABOUTDATA(IRND)%APPROACHES
      DO IAP = 1, ROUNDABOUTDATA(IRND)%APPROACHES
        USN = ROUNDABOUTDATA(IRND)%APPROACH_USN(IAP)
        DSN = ROUNDABOUTDATA(IRND)%APPROACH_DSN(IAP)
        CALL FIND_STREET_LINK(USN, DSN, IL)
        IF(IL .NE. 0) THEN
          ROUNDABOUT_ID(IL) = IRND
          ROUNDABOUT_APPROACH_NUM(IL) = IAP
          ROUNDABOUT(IRND)%APPROACH_LINKS(IAP) = IL
          PCT = ROUNDABOUTDATA(IRND)%EXIT_PCTS(1:5, IAP)
          TOTAL = SUM(PCT)
          ERROR = 100 - TOTAL
          IF(ERROR .NE. 0) THEN
            DO N = 1, 5
              IF(PCT(N) .NE. 0) THEN
                PCT(N) = PCT(N) + ERROR
                EXIT
              ENDIF
            ENDDO
          ENDIF
          DO N = 1, 5 
            ROUNDABOUT(IRND)%EXIT_PCTS(IAP, N) = PCT(N) / 100.
          ENDDO
          USN = ROUNDABOUTDATA(IRND)%DEPARTING_USN(IAP)
          DSN = ROUNDABOUTDATA(IRND)%DEPARTING_DSN(IAP)
          CALL FIND_STREET_LINK(USN, DSN, IL)
          IF(IL .NE. 0) THEN
            ROUNDABOUT(IRND)%DEPARTING_LINKS(IAP) = IL
            ROUNDABOUT_EXIT_NUM(IL) = IAP
          ENDIF
        ENDIF
      ENDDO
      ROUNDABOUT(IRND)%RADIUS = ROUNDABOUTDATA(IRND)%RADIUS
      !Identify links within the roundabout.
      DO N = 1, 2 
        DO IL = 1, N_STREET_LINKS
          IF(ROUNDABOUT_ID(IL) .EQ. IRND .AND. ROUNDABOUT_EXIT_NUM(IL) .EQ. 0) THEN
            IF(LEFT_LINK(IL) .NE. 0) THEN
              ROUNDABOUT_ID(LEFT_LINK(IL)) = IRND
            ENDIF
            IF(STHRU_LINK(IL) .NE. 0) THEN
              ROUNDABOUT_ID(STHRU_LINK(IL)) = IRND
            ENDIF
            IF(RIGHT_LINK(IL) .NE. 0) THEN
              ROUNDABOUT_ID(RIGHT_LINK(IL)) = IRND
            ENDIF
            IF(LEFT_DIAG_LINK(IL) .NE. 0) THEN
              ROUNDABOUT_ID(LEFT_DIAG_LINK(IL)) = IRND
            ENDIF
            IF(RIGHT_DIAG_LINK(IL) .NE. 0) THEN
              ROUNDABOUT_ID(RIGHT_DIAG_LINK(IL)) = IRND
            ENDIF
          ENDIF
        ENDDO
      ENDDO
    ENDDO
  ELSE
    ERRORS = 0
    DO IRND = 1, NUMBER_OF_ROUNDABOUTS
      DO IAP = 1, ROUNDABOUTDATA(IRND)%APPROACHES
        PCT = ROUNDABOUTDATA(IRND)%EXIT_PCTS(1:5, IAP)
        TOTAL = SUM(PCT)
        ERROR = 100 - TOTAL
        IF(ERROR .NE. 0) THEN
          DO N = 1, 5
            IF(PCT(N) .NE. 0) THEN
              PCT(N) = PCT(N) + ERROR
              EXIT
            ENDIF
          ENDDO
        ENDIF
        DO N = 1, 5 
          ROUNDABOUT(IRND)%EXIT_PCTS(IAP, N) = PCT(N) / 100.
        ENDDO
      ENDDO
    ENDDO
  ENDIF
  END
    
! ==================================================================================================
  INTEGER FUNCTION GET_ROUNDABOUTS[DLLEXPORT, STDCALL](OUTPUTS) RESULT(ERRORS)
  USE API_DATA
  USE ROUNDABOUT_DATA
  USE STREET_LINKS
  IMPLICIT NONE
  TYPE(RABT_API_DATA) :: OUTPUTS(NUMBER_OF_ROUNDABOUTS)
  INTEGER :: IRND, I
! ----------------------------------------------------------------------
  DO IRND = 1, NUMBER_OF_ROUNDABOUTS
    OUTPUTS(IRND)%APPROACHES     = ROUNDABOUT(IRND)%APPROACHES
    DO I = 1, ROUNDABOUT(IRND)%APPROACHES
      OUTPUTS(IRND)%APPROACH_USN(I)   = SUSN(ROUNDABOUT(IRND)%APPROACH_LINKS(I))
      OUTPUTS(IRND)%APPROACH_DSN(I)   = SDSN(ROUNDABOUT(IRND)%APPROACH_LINKS(I))
      OUTPUTS(IRND)%DEPARTING_USN(I)  = SUSN(ROUNDABOUT(IRND)%DEPARTING_LINKS(I))
      OUTPUTS(IRND)%DEPARTING_DSN(I)  = SDSN(ROUNDABOUT(IRND)%DEPARTING_LINKS(I))
    ENDDO
    OUTPUTS(IRND)%EXIT_PCTS      = ROUNDABOUT(IRND)%EXIT_PCTS
    OUTPUTS(IRND)%RADIUS         = ROUNDABOUT(IRND)%RADIUS
  ENDDO
  ERRORS = 0
  END

! ==================================================================================================
  INTEGER FUNCTION SET_NUMBER_OF_RTURNING_WAYS[DLLEXPORT, STDCALL](N) RESULT(ERRORS)
  USE SIMPARAMS
  USE API_DATA
  USE TURNING_WAYS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    ERRORS = 0
    NUMBER_OF_TURNING_WAYS = N
    CALL ALLOCATE_TURNING_WAY_ARRAYS
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'SET_NUMBER_OF_RTURNING_WAYS CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_NUMBER_OF_ROUNDABOUTS[DLLEXPORT, STDCALL]()
  USE ROUNDABOUT_DATA
  IMPLICIT NONE
! ----------------------------------------------------------------------
  GET_NUMBER_OF_ROUNDABOUTS = NUMBER_OF_ROUNDABOUTS
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_NUMBER_OF_RTURNING_WAYS[DLLEXPORT, STDCALL]()
  USE TURNING_WAYS
  IMPLICIT NONE
! ----------------------------------------------------------------------
  GET_NUMBER_OF_RTURNING_WAYS = NUMBER_OF_TURNING_WAYS
  END
  
! ==================================================================================================
  INTEGER FUNCTION DEFINE_RTURNING_WAYS[DLLEXPORT, STDCALL](TURNING_WAY_DATA) RESULT(ERRORS)
  USE TURNING_WAYS
  USE STREET_LINKS
  USE NODE_TABLE
  USE GLOBAL_DATA
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  TYPE(RTW_DATA) :: TURNING_WAY_DATA(NUMBER_OF_TURNING_WAYS)
  INTEGER :: IRTW, IL, ILN, KLN, ILINK
! ----------------------------------------------------------------------
  IF(TIME_PERIOD .LE. 1) THEN
    ERRORS = 0
    DO IRTW = 1, NUMBER_OF_TURNING_WAYS
      CALL FIND_STREET_LINK(TURNING_WAY_DATA(IRTW)%USN, TURNING_WAY_DATA(IRTW)%DSN, IL)
      IF(IL .EQ. 0) THEN
        ERRORS = ERRORS + 1
      ELSE
        WRITE12(IL) = .TRUE.
        IF(TURNING_WAY_DATA(IRTW)%USN2 + TURNING_WAY_DATA(IRTW)%DSN2 .NE. 0) THEN
          CALL FIND_STREET_LINK(TURNING_WAY_DATA(IRTW)%USN2, TURNING_WAY_DATA(IRTW)%DSN2, ILINK)
          IF(ILINK .EQ. 0) THEN
            ERRORS = ERRORS + 1
            WRITE(MSGTEXT, '(A, 2I5)') 'DEFINE_TURNING_WAYS: TURNING WAY RECEIVING LINK NOT FOUND ', TURNING_WAY_DATA(IRTW)%USN2, TURNING_WAY_DATA(IRTW)%DSN2
            CALL SENDTEXTMSG(M_ERROR)
          ENDIF
        ELSE
          ILINK = RIGHT_LINK(IL)
          IF(ILINK .EQ. 0) THEN
            ERRORS = ERRORS + 1
            WRITE(MSGTEXT, '(A)') 'DEFINE_TURNING_WAYS: TURNING WAY RIGHT RECEIVING LINK NOT FOUND '
            CALL SENDTEXTMSG(M_ERROR)
          ENDIF
        ENDIF
        IF(ILINK .NE. 0) THEN
          NUMBER_TURNINGWAYS(IL) = NUMBER_TURNINGWAYS(IL) + 1
          RTW_RECEIVING_LINK(IL) = ILINK
          RTW_EXIT_POINT(IL) = TURNING_WAY_DATA(IRTW)%RTW_EXIT_POINT
          RTW_ENTRY_POINT(RTW_RECEIVING_LINK(IL)) = TURNING_WAY_DATA(IRTW)%RTW_ENTRY_POINT
          RTW_LENGTH(IL) = TURNING_WAY_DATA(IRTW)%RTW_LENGTH
          RTW_FFSPEED(IL) = TURNING_WAY_DATA(IRTW)%RTW_FFSPEED
          IF(TURNING_WAY_DATA(IRTW)%RTW_CONTROL_CODE .EQ. 0) THEN
            RTW_CONTROL_CODE(IL) = S_YIELD
          ELSEIF(TURNING_WAY_DATA(IRTW)%RTW_CONTROL_CODE .EQ. 1) THEN
            RTW_CONTROL_CODE(IL) = S_PERGRN
          ELSEIF(TURNING_WAY_DATA(IRTW)%RTW_CONTROL_CODE .EQ. 5) THEN
            RTW_CONTROL_CODE(IL) = S_STOP
          ENDIF
          RTW_LANES(IL) = TURNING_WAY_DATA(IRTW)%RTW_LANES
          CALL SHIFT_LANES_RTW(IL)
        ENDIF
      ENDIF
    ENDDO
  ELSE
    ERRORS = 1
    WRITE(MSGTEXT, '(A)') 'DEFINE_RTURNING_WAYS CANNOT BE CALLED DURING SUBSEQUENT TIME PERIODS'
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION GET_RTURNING_WAYS[DLLEXPORT, STDCALL](OUTPUTS) RESULT(ERRORS)
  USE TURNING_WAYS
  USE STREET_LINKS
  USE TEXT
  IMPLICIT NONE
  TYPE(RTW_DATA) :: OUTPUTS(NUMBER_OF_TURNING_WAYS)
  INTEGER :: IRTW, IL
! ----------------------------------------------------------------------
  IRTW = 0
  DO IL = 1, N_STREET_LINKS
    IF(RTW_EXIT_POINT(IL) .NE. 0) THEN
      IRTW = IRTW + 1
      OUTPUTS(IRTW)%USN = SUSN(IL)
      OUTPUTS(IRTW)%DSN = SDSN(IL)
      OUTPUTS(IRTW)%RTW_EXIT_POINT = RTW_EXIT_POINT(IL)
      OUTPUTS(IRTW)%RTW_LENGTH = RTW_LENGTH(IL)
      OUTPUTS(IRTW)%RTW_FFSPEED = RTW_FFSPEED(IL)
      OUTPUTS(IRTW)%RTW_CONTROL_CODE = RTW_CONTROL_CODE(IL)
      IF(RTW_CONTROL_CODE(IL) .EQ. S_YIELD) THEN
        OUTPUTS(IRTW)%RTW_CONTROL_CODE = 0
      ELSEIF(RTW_CONTROL_CODE(IL) .EQ. S_PERGRN) THEN
        OUTPUTS(IRTW)%RTW_CONTROL_CODE = 1
      ELSEIF(RTW_CONTROL_CODE(IL) .EQ. S_STOP) THEN
        OUTPUTS(IRTW)%RTW_CONTROL_CODE = 5
      ENDIF
      OUTPUTS(IRTW)%RTW_LANES = RTW_LANES(IL)
      IF(RTW_RECEIVING_LINK(IL) .NE. 0) THEN
        OUTPUTS(IRTW)%USN2 = SUSN(RTW_RECEIVING_LINK(IL))
        OUTPUTS(IRTW)%DSN2 = SDSN(RTW_RECEIVING_LINK(IL))
        OUTPUTS(IRTW)%RTW_ENTRY_POINT = RTW_ENTRY_POINT(RTW_RECEIVING_LINK(IL))
      ENDIF
    ENDIF
  ENDDO
  ERRORS = 0
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_TRF_FILE(ERRORS)
  USE FREEWAY_LINKS
  USE STREET_LINKS
  USE BUS_ROUTE_DATA
  USE GLOBAL_DATA
  USE SIMPARAMS
  USE ACTUATED_CONTROLLERS
  USE CAR_FOLLOWING
  USE ROUNDABOUT_DATA
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(OUT) :: ERRORS
  INCLUDE 'IOFILES.INC'
  INTEGER :: I, NTPS, I1, I2, TIMEPERIOD, IACT, PHASE, DCS_FLAG
  LOGICAL :: IN_USE(8)
! ----------------------------------------------------------------------
  ERRORS = 0
  IF(MAX_NODE_NUMBER .GT. 7999) GOTO 30
  NTPS = 1
  DO I = 2, 19
    IF(TPSECONDS(I) .EQ. 0) EXIT
    NTPS = I
  ENDDO
  OPEN(100, FILE=LINFNAME(1:IROOT)//'TRF', FORM='FORMATTED', ACTION='WRITE', ERR=10, IOMSG=ETEXT)
  CALL WRITE_RT01  
  CALL WRITE_RT02  
  CALL WRITE_RT03  
  CALL WRITE_RT04  
  CALL WRITE_RT05  
  TIMEPERIOD = 1
  IF(N_STREET_LINKS .GT. 0) THEN
    !Some RTs are not supported now
    CALL WRITE_RT11
    CALL WRITE_RT12
    CALL WRITE_RT13
    CALL WRITE_RT21(TIMEPERIOD)
    CALL WRITE_RT22
    !CALL WRITE_RT23
    CALL WRITE_RT35(TIMEPERIOD)
    CALL WRITE_RT36(TIMEPERIOD)
    CALL WRITE_RT42
    CALL WRITE_RT43
    CALL WRITE_RT44(TIMEPERIOD)
    CALL WRITE_RT45(TIMEPERIOD)
    CALL WRITE_RT46(TIMEPERIOD)
    CALL WRITE_RT47(TIMEPERIOD)
    !CALL WRITE_RT48(TIMEPERIOD)
    CALL WRITE_RT49(TIMEPERIOD)
    CALL WRITE_RT50(I_STREET, TIMEPERIOD)
    CALL WRITE_RT51
    !CALL WRITE_RT53(TIMEPERIOD)
    CALL WRITE_RT54
    CALL WRITE_RT55
    CALL WRITE_RT56(TIMEPERIOD)
    IF(WRITE58) CALL WRITE_RT58
    CALL WRITE_RT80
    CALL WRITE_RT82
    CALL WRITE_RT83
    CALL WRITE_RT97
    CALL WRITE_RT98
    IF(WRITE154) CALL WRITE_RT154
    CALL WRITE_RT170(I_STREET)
  ENDIF
  IF(N_FREEWAY_LINKS .GT. 0) THEN
    CALL WRITE_RT19
    CALL WRITE_RT20
    CALL WRITE_RT25(TIMEPERIOD)
    CALL WRITE_RT28
    CALL WRITE_RT29
    CALL WRITE_RT32
    CALL WRITE_RT33(TIMEPERIOD)
    CALL WRITE_RT37(TIMEPERIOD)
    CALL WRITE_RT38
    CALL WRITE_RT50(I_FREEWAY, TIMEPERIOD)
    !IF(WRITE70) CALL WRITE_RT70
    IF(WRITE71) CALL WRITE_RT71
    CALL WRITE_RT170(I_FREEWAY)
  ENDIF
  IF(NUMBER_OF_ROUTES .GT. 0) THEN
    CALL WRITE_RT185
    CALL WRITE_RT186(TIMEPERIOD)
    CALL WRITE_RT187
    CALL WRITE_RT188
    CALL WRITE_RT189(TIMEPERIOD)
  ENDIF
  CALL WRITE_RT195
  !CALL WRITE_RT196
  CALL WRITE_RT197
  IF(WRITE201) CALL WRITE_RT201
  IF(WRITE202) CALL WRITE_RT202
  IF(NTPS .EQ. 1) THEN
    I1 = 1
    I2 = 0
    CALL WRITE_RT210(I1, I2)
  ELSE
    I1 = 0
    IF(N_STREET_LINKS .GT. 0) THEN
      I2 = 3
    ELSE
      I2 = 8
    ENDIF
    CALL WRITE_RT210(I1, I2)
    DO I = 2, NTPS
      TIMEPERIOD = I
      IF(N_STREET_LINKS .GT. 0) THEN
        !Some RTs are not supported now
        CALL WRITE_RT21(TIMEPERIOD)
        CALL WRITE_RT22
        !CALL WRITE_RT23(TIMEPERIOD)
        CALL WRITE_RT35(TIMEPERIOD)
        CALL WRITE_RT36(TIMEPERIOD)
        CALL WRITE_RT44(TIMEPERIOD)
        CALL WRITE_RT45(TIMEPERIOD)
        CALL WRITE_RT46(TIMEPERIOD)
        CALL WRITE_RT47(TIMEPERIOD)
        !CALL WRITE_RT48(TIMEPERIOD)
        CALL WRITE_RT49(TIMEPERIOD)
        CALL WRITE_RT50(I_STREET, TIMEPERIOD)
        !CALL WRITE_RT53(TIMEPERIOD)
        CALL WRITE_RT56(TIMEPERIOD)
        CALL WRITE_RT170(I_STREET)
      ENDIF
      IF(N_FREEWAY_LINKS .GT. 0) THEN
        CALL WRITE_RT25(TIMEPERIOD)
        CALL WRITE_RT37(TIMEPERIOD)
        CALL WRITE_RT50(I_FREEWAY, TIMEPERIOD)
        CALL WRITE_RT170(I_FREEWAY)
      ENDIF
      IF(NUMBER_OF_ROUTES .GT. 0) THEN
        CALL WRITE_RT186(TIMEPERIOD)
        CALL WRITE_RT189(TIMEPERIOD)
      ENDIF
      IF(I .LT. NTPS) THEN
        IF(N_STREET_LINKS .GT. 0) THEN
          I2 = 3
        ELSE
          I2 = 8
        ENDIF
      ELSE
        I1 = 1
        I2 = 0
      ENDIF
      CALL WRITE_RT210(I1, I2)
    ENDDO        
  ENDIF
  CLOSE(100)
  RETURN
10 CONTINUE
  ERRORS = ERRORS + 1
  WRITE(MSGTEXT,'(A)') 'WRITE_TRF_FILE: FILE OPEN ERROR'
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A)') ETEXT
  CALL SENDTEXTMSG(M_ERROR)
  RETURN
20 CONTINUE 
  ERRORS = ERRORS + 1
  WRITE(MSGTEXT,'(A)') 'WRITE_TRF_FILE: FILE WRITE ERROR'
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A)') ETEXT
  CALL SENDTEXTMSG(M_ERROR)
  CLOSE(100)
  RETURN
30 CONTINUE
  WRITE(MSGTEXT,'(A)') 'WRITE_TRF_FILE: NETWORK DOES NOT CONFORM TO TRF STANDARDS'
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A)') ETEXT
  CALL SENDTEXTMSG(M_ERROR)
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT01 
  IMPLICIT NONE
  CHARACTER*80 STRING
! ----------------------------------------------------------------------
  STRING(1:78) = ''
  STRING(79:80) = ' 1'
  WRITE(100, '(A)') STRING
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT02 
  USE SIMPARAMS
  USE SEEDS
  USE STREET_LINKS
  USE ENTRYNODE_DATA
  IMPLICIT NONE
  CHARACTER*80 STRING
  COMMON /GRAPHIO/ TSTEPFILE, TINTERVALFILE
  INTEGER :: TSTEPFILE, TINTERVALFILE
! ----------------------------------------------------------------------
  STRING(1:78) = ''
  STRING(79:80) = ' 2'
  IF(TSTEPFILE .EQ. 2) THEN
    STRING(8:8) = '4'
  ELSE
    STRING(8:8) = '1'
  ENDIF
  STRING(12:12) = '0'
  IF(SKIP_INIT) THEN
    STRING(16:16) = '2'
  ELSE
    STRING(16:16) = '0'
  ENDIF
  WRITE(STRING(17:20), '(I4)') INITIALIZATION_END / 60
  WRITE(STRING(22:29), '(I8)') ISEED1
  WRITE(STRING(37:37), '(I1)') TYPEDIST
  IF(N_STREET_LINKS .GT. 0) THEN
    STRING(52:52) = '3'
  ELSE
    STRING(52:52) = '8'
  ENDIF
  WRITE(STRING(61:68), '(I8)') ISEED2
  WRITE(STRING(22:29), '(I8)') ISEED1
  WRITE(STRING(69:76), '(I8)') ISEED3
  WRITE(100, '(A)') STRING
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT03  
  USE SIMPARAMS
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: I, N
! ----------------------------------------------------------------------
  STRING(1:78) = ''
  N = 1
  DO I = 1, 19
    IF(TPSECONDS(I) .EQ. 0) EXIT
    WRITE(STRING(N:N+3), '(I4)') TPSECONDS(I)
    N = N + 4
  ENDDO
  STRING(79:80) = ' 3'
  WRITE(100, '(A)') STRING
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT04  
  USE SIMPARAMS
  IMPLICIT NONE
  CHARACTER*80 STRING
! ----------------------------------------------------------------------
  STRING(1:78) = ''
  WRITE(STRING(9:12), '(I4)') INT(1.0 / TIMESTEP)
  WRITE(STRING(17:20), '(I4)') TIME_INTERVAL
  STRING(79:80) = ' 4'
  WRITE(100, '(A)') STRING
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT05  
  IMPLICIT NONE
  CHARACTER*80 STRING
! ----------------------------------------------------------------------
  STRING(1:78) = ''
  STRING(79:80) = ' 5'
  WRITE(100, '(A)') STRING
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT11 
  USE STREET_LINKS
  USE GLOBAL_DATA
  USE TEXT
  USE NODE_TABLE
  USE SIMPARAMS
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: IL, ILANE, NLANE
! ----------------------------------------------------------------------
  STRING(79:80) = '11'
  DO IL = 1, N_STREET_LINKS
    STRING(1:78) = ''
    IF(NODE_TYPE(SDSN(IL)) .NE. NT_EXTERN) THEN
      IF(SLENGTH(IL) .GT. 9999) THEN
        WRITE(MSGTEXT, '(A)') 'LINK LENGTH EXCEEDS 9999 FEET, CANNOT CREATE TRF FILE'
        CALL SENDTEXTMSG(M_ERROR)
        EXITFLG = 1
        RETURN
      ENDIF
      WRITE(STRING(1:4), '(I4)') SUSN(IL)
      WRITE(STRING(5:8), '(I4)') SDSN(IL)
      WRITE(STRING(9:12), '(I4)') SLENGTH(IL)
      IF(NUMBER_LEFTPOCKETS(IL) .GT. 0) THEN
        WRITE(STRING(13:16), '(I4)') INT(LANE_LENGTH(IL, LAST_FULL_LANE(IL) + 1))
      ENDIF
      IF(NUMBER_RIGHTPOCKETS(IL) .GT. 0) THEN
        WRITE(STRING(17:20), '(I4)') INT(LANE_LENGTH(IL, NUMBER_RIGHTPOCKETS(IL)))
      ENDIF
      WRITE(STRING(22:22), '(I1)') SNUMLANES(IL)
      WRITE(STRING(24:24), '(I1)') NUMBER_LEFTPOCKETS(IL)
      WRITE(STRING(26:26), '(I1)') NUMBER_RIGHTPOCKETS(IL)
      WRITE(STRING(27:28), '(I2)') INT(SGRADE(IL) * 100)
      WRITE(STRING(29:29), '(I1)') LINKTYPE_CODE(IL)
      NLANE = 0
      DO ILANE = FIRST_FULL_LANE(IL), LAST_FULL_LANE(IL)
        NLANE = NLANE + 1
        IF(CHANNELIZATION(IL, ILANE) .EQ. 11) THEN
          WRITE(STRING(NLANE+29:NLANE+29), '(A1)') 'T'
        ELSEIF(CHANNELIZATION(IL, ILANE) .EQ. 10) THEN
          WRITE(STRING(NLANE+29:NLANE+29), '(A1)') 'D'
        ELSEIF(CHANNELIZATION(IL, ILANE) .GE. 0) THEN
          WRITE(STRING(NLANE+29:NLANE+29), '(I1)') CHANNELIZATION(IL, ILANE)
        ENDIF
      ENDDO
      IF(LEFT_LINK(IL) .NE. 0) WRITE(STRING(37:40), '(I4)') SDSN(LEFT_LINK(IL))
      IF(STHRU_LINK(IL) .NE. 0) WRITE(STRING(41:44), '(I4)') SDSN(STHRU_LINK(IL))
      IF(RIGHT_LINK(IL) .NE. 0) WRITE(STRING(45:48), '(I4)') SDSN(RIGHT_LINK(IL))
      IF(RIGHT_DIAG_LINK(IL) .NE. 0) THEN
        WRITE(STRING(49:52), '(I4)') SDSN(RIGHT_DIAG_LINK(IL))
      ELSEIF(LEFT_DIAG_LINK(IL) .NE. 0) THEN
        WRITE(STRING(49:52), '(I4)') -SDSN(ABS(LEFT_DIAG_LINK(IL)))
      ENDIF
      IF(OPPOSE_LINK(IL) .NE. 0) WRITE(STRING(53:56), '(I4)') SUSN(OPPOSE_LINK(IL))
      WRITE(STRING(57:60), '(I4)') NINT(SSTARTUP_TIME(IL) * 10)
      WRITE(STRING(61:64), '(I4)') NINT(QDISCHARGE_HDWY(IL) * 10)
      IF(NODE_TYPE(SUSN(IL)) .NE. NT_EXTERN) WRITE(STRING(65:68), '(I4)') NINT(SFREEFLOWSPEED(IL) * FEET2MILES)
      IF(.NOT. RTOR(IL)) STRING(70:70) = '1'
      WRITE(STRING(71:71), '(I1)') PED_CODE(IL)
      IF(NODE_TYPE(SUSN(IL)) .EQ. NT_INTERN .AND. NODE_TYPE(SDSN(IL)) .EQ. NT_INTERN .AND. STHRU_LINK(IL) .NE. 0) THEN
        WRITE(STRING(72:73), '(I1)') SALIGNMENT_LANE(IL) - NUMBER_RIGHTPOCKETS(IL)
        WRITE(STRING(73:74), '(I1)') STHRU_ALIGNMENT_LANE(IL) - NUMBER_RIGHTPOCKETS(STHRU_LINK(IL))
      ENDIF
      WRITE(100, '(A)') STRING
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE WRITE_RT12 
  USE STREET_LINKS
  USE NODE_TABLE
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: IL, ILANE, NLANE
! ----------------------------------------------------------------------
  STRING(79:80) = '12'
  DO IL = 1, N_STREET_LINKS
    IF(WRITE12(IL)) THEN
      IF(NODE_TYPE(SDSN(IL)) .NE. NT_EXTERN) THEN
        STRING(1:78) = ''
        WRITE(STRING(1:4), '(I4)') SUSN(IL)
        WRITE(STRING(5:8), '(I4)') SDSN(IL)
        WRITE(STRING(9:12), '(I4)') SNUMLANES(IL)
        WRITE(STRING(13:16), '(I4)') NUMBER_LEFTPOCKETS(IL)
        WRITE(STRING(17:20), '(I4)') NUMBER_RIGHTPOCKETS(IL)
        NLANE = 0
        DO ILANE = 1, 20
          IF(RTW_EXIT_POINT(IL) .NE. 0 .AND. ILANE .EQ. 1) CYCLE
          NLANE = NLANE + 1
          IF(CHANNELIZATION(IL, ILANE) .EQ. 11) THEN
            WRITE(STRING(NLANE+20:NLANE+20), '(A1)') 'T'
          ELSEIF(CHANNELIZATION(IL, ILANE) .EQ. 10) THEN
            WRITE(STRING(NLANE+20:NLANE+20), '(A1)') 'D'
          ELSEIF(CHANNELIZATION(IL, ILANE) .GE. 0) THEN
            WRITE(STRING(NLANE+20:NLANE+20), '(I1)') CHANNELIZATION(IL, ILANE)
          ENDIF
        ENDDO
        WRITE(STRING(41:44), '(I1)') LEFT_DIAG_LINK(IL)
        WRITE(STRING(45:48), '(I1)') RIGHT_DIAG_LINK(IL)
        WRITE(STRING(49:52), '(I4)') INT(RTW_EXIT_POINT(IL))
        WRITE(STRING(53:56), '(I4)') INT(RTW_ENTRY_POINT(RIGHT_LINK(IL)))
        WRITE(STRING(57:60), '(I4)') INT(RTW_LENGTH(IL))
        WRITE(100, '(A)') STRING
      ENDIF
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE WRITE_RT13 
  USE STREET_LINKS
  USE NODE_TABLE
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: IL, ILANE, N
! ----------------------------------------------------------------------
  STRING(79:80) = '13'
  DO IL = 1, N_STREET_LINKS
    IF(WRITE13(IL)) THEN
      IF(NODE_TYPE(SDSN(IL)) .NE. NT_EXTERN) THEN
        STRING(1:78) = ''
        WRITE(STRING(1:4), '(I4)') SUSN(IL)
        WRITE(STRING(5:8), '(I4)') SDSN(IL)
        N = 4
        DO ILANE = NUMBER_RIGHTPOCKETS(IL), 1, -1
          N = N + 4
          WRITE(STRING(N:N+3), '(I4)') LANE_LENGTH(IL, ILANE)
        ENDDO
        DO ILANE = LAST_FULL_LANE(IL) + 1, TOTAL_LANES(IL)
          N = N + 4
          WRITE(STRING(N:N+3), '(I4)') LANE_LENGTH(IL, ILANE)
        ENDDO
      ENDIF
    ENDIF
  ENDDO
  RETURN
  END
    
! ==================================================================================================
  SUBROUTINE WRITE_RT19  
  USE FREEWAY_LINKS
  USE NODE_TABLE
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: IL, I, J, ILANE
! ----------------------------------------------------------------------
  STRING(79:80) = '19'
  DO IL = 1, N_FREEWAY_LINKS
    IF(NODE_TYPE(FDSN(IL)) .EQ. NT_EXTERN) CYCLE
    STRING(1:78) = ''
    WRITE(STRING(1:4), '(I4)') FUSN(IL)
    WRITE(STRING(5:8), '(I4)') FDSN(IL)
    IF(FTHRU_LINK(IL) .NE. 0) WRITE(STRING(9:12), '(I4)') FDSN(FTHRU_LINK(IL))
    WRITE(STRING(13:17), '(I5)') FLENGTH(IL)
    IF(LINKTYPE(IL) .EQ. 0) THEN
      STRING(18:18) = '0'
    ELSE
      STRING(18:18) = '1'
    ENDIF
    WRITE(STRING(20:20), '(I1)') FNUMLANES(IL)
    J = 21
    DO I = 1, 3     
      IF(AUX_LANE_ID(IL, I) .EQ. 0) EXIT
      IF(AUX_LANE_ID(IL, I) .GE. 16) THEN
        WRITE(STRING(J:J+1), '(I2)') AUX_LANE_ID(IL, I) - 7
      ELSE
        WRITE(STRING(J:J+1), '(I2)') AUX_LANE_ID(IL, I) - 5
      ENDIF
      WRITE(STRING(J+2:J+2), '(I1)') AUX_LANE_CODE(IL, I)
      WRITE(STRING(J+3:J+7), '(I5)') AUX_LANE_LENGTH(IL, I)
      J = J + 8
    ENDDO
    
    IF(RECEIVING_LANE(IL, 1) .GE. 16) THEN
      WRITE(STRING(45:46), '(I2)') RECEIVING_LANE(IL, 1) - 7
    ELSEIF(RECEIVING_LANE(IL, 1) .GE. 10) THEN
      WRITE(STRING(45:46), '(I2)') RECEIVING_LANE(IL, 1) - 5
    ELSEIF(FDSN(IL) .GE. 7000) THEN
      WRITE(STRING(45:46), '(I2)') 1
    ELSE
      WRITE(STRING(45:46), '(I2)') RECEIVING_LANE(IL, 1)
    ENDIF
    
    ILANE = 0
    DO I = 1, N_FREEWAY_LANES
      IF(EXIT_LANE(IL, I) .EQ. 1) THEN
        ILANE = I
        EXIT
      ENDIF
    ENDDO
    IF(ILANE .GE. 16) THEN
      WRITE(STRING(47:48), '(I2)') ILANE - 7
    ELSEIF(ILANE .GE. 10) THEN
      WRITE(STRING(47:48), '(I2)') ILANE - 5
    ELSE
      WRITE(STRING(47:48), '(I2)') ILANE
    ENDIF
    
    IF(BARRIER(IL, 1) .GE. 16) THEN
      WRITE(STRING(49:50), '(I2)') BARRIER(IL, 1) - 7
    ELSEIF(BARRIER(IL, 1) .GE. 10) THEN
      WRITE(STRING(49:50), '(I2)') BARRIER(IL, 1) - 5
    ELSE
      WRITE(STRING(49:50), '(I2)') BARRIER(IL, 1)
    ENDIF
    IF(BARRIER(IL, 2) .GE. 16) THEN
      WRITE(STRING(51:52), '(I2)') BARRIER(IL, 2) - 7
    ELSEIF(BARRIER(IL, 2) .GE. 10) THEN
      WRITE(STRING(51:52), '(I2)') BARRIER(IL, 2) - 5
    ELSE
      WRITE(STRING(51:52), '(I2)') BARRIER(IL, 2)
    ENDIF
    J = 53
    DO I = 4, 6  
      IF(AUX_LANE_ID(IL, I) .EQ. 0) EXIT
      IF(AUX_LANE_ID(IL, I) .GE. 16) THEN
        WRITE(STRING(J:J+1), '(I2)') AUX_LANE_ID(IL, I) - 7
      ELSE
        WRITE(STRING(J:J+1), '(I2)') AUX_LANE_ID(IL, I) - 5
      ENDIF
      WRITE(STRING(J+2:J+2), '(I1)') AUX_LANE_CODE(IL, I)
      WRITE(STRING(J+3:J+7), '(I5)') AUX_LANE_LENGTH(IL, I)
      J = J + 8
    ENDDO
    IF(RAMP_MERGE_LINK(IL)) THEN
      STRING(77:77) = '1'
    ELSEIF(DIVERGE_LINK(IL)) THEN
      STRING(77:77) = '2'
    ENDIF
    WRITE(100, '(A)') STRING
  ENDDO
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT20  
  USE FREEWAY_LINKS
  USE FREEWAY_NODES
  USE DATASTATIONS
  USE GLOBAL_DATA
  USE NODE_TABLE
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: IL
! ----------------------------------------------------------------------
  STRING(79:80) = '20'
  DO IL = 1, N_FREEWAY_LINKS
    IF(NODE_TYPE(FDSN(IL)) .EQ. NT_EXTERN) CYCLE
    STRING(1:78) = ''
    WRITE(STRING(1:4), '(I4)') FUSN(IL)
    WRITE(STRING(5:8), '(I4)') FDSN(IL)
    WRITE(STRING(9:10), '(I2)') INT(FGRADE(IL) * 100)
    WRITE(STRING(11:12), '(I2)') INT(TILT(IL) * 100)
    WRITE(STRING(13:16), '(I4)') INT(CURVE(IL))
    WRITE(STRING(18:18), '(I1)') PAVEMENT(IL)
    WRITE(STRING(19:20), '(I2)') INT(FSTARTUP_TIME(IL) * 10)
    IF(NODE_TYPE(FUSN(IL)) .NE. NT_EXTERN) WRITE(STRING(21:22), '(I2)') INT(FFREEFLOWSPEED(IL) * FEET2MILES)
    WRITE(STRING(24:24), '(I1)') TRUCK_CODE(IL)
    IF(TRUCK_LANE(IL) .LE. 10) THEN
      WRITE(STRING(25:26), '(I2)') TRUCK_LANE(IL)
    ELSEIF(TRUCK_LANE(IL) .EQ. 16) THEN
      WRITE(STRING(25:26), '(I2)') 9
    ELSEIF(TRUCK_LANE(IL) .EQ. 17) THEN
      WRITE(STRING(25:26), '(I2)') 10
    ELSEIF(TRUCK_LANE(IL) .EQ. 18) THEN
      WRITE(STRING(25:26), '(I2)') 11
    ELSEIF(TRUCK_LANE(IL) .EQ. 11) THEN
      WRITE(STRING(25:26), '(I2)') 6
    ELSEIF(TRUCK_LANE(IL) .EQ. 12) THEN
      WRITE(STRING(25:26), '(I2)') 7
    ELSEIF(TRUCK_LANE(IL) .EQ. 13) THEN
      WRITE(STRING(25:26), '(I2)') 8
    ENDIF
    WRITE(STRING(28:28), '(I1)') TRUCK_DIR(IL)
    IF(OFFRAMP_LINK(IL) .NE. 0) WRITE(STRING(29:33), '(I5)') SAVE_WARN1(IL)
    IF(DATASTATION_ID(IL) .NE. 0)  WRITE(STRING(34:38), '(I5)') DATASTATION(DATASTATION_ID(IL))%LOCATION
    IF(OFFRAMP_LINK(IL) .NE. 0) WRITE(STRING(39:43), '(I5)') SAVE_WARN2(IL)
    WRITE(STRING(44:48), '(I5)') ETL_WARN(IL)
    IF(RAMP_APPROACH(FUSN(IL)) .NE. 0) THEN
      WRITE(STRING(61:64), '(I4)') INT(ANTICIP_WARNING_SPEED(IL) * FEET2MILES)
      WRITE(STRING(65:68), '(I4)') ANTICIP_WARNING_DISTANCE(IL)
    ENDIF
    WRITE(STRING(69:72), '(I4)') INT(FCFMULT(IL) * 100)
    WRITE(100, '(A)') STRING
  ENDDO
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT21(TIMEPERIOD) 
  USE API_DATA
  USE STREET_LINKS
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: TIMEPERIOD
  CHARACTER*80 STRING
  INTEGER :: IL
  LOGICAL :: WRITE_IT
! ----------------------------------------------------------------------
  STRING(79:80) = '21'
  DO IL = 1, N_STREET_LINKS
    STRING(1:78) = ''
    IF(NODE_TYPE(SDSN(IL)) .NE. NT_EXTERN) THEN
      WRITE_IT = .FALSE.
      IF(TIMEPERIOD .EQ. 1) THEN
        WRITE_IT = .TRUE.
      ELSEIF(NODE_TYPE(SUSN(IL)) .NE. NT_EXTERN) THEN
        IF(WRITE21(IL, TIMEPERIOD)) WRITE_IT = .TRUE.
      ENDIF
      IF(WRITE_IT) THEN
        WRITE(STRING(1:4), '(I4)') SUSN(IL)
        WRITE(STRING(5:8), '(I4)') SDSN(IL)
        IF(NODE_TYPE(SUSN(IL)) .NE. NT_EXTERN) THEN
          WRITE(STRING(9:12), '(I4)') LEFT_PCT(IL, TIMEPERIOD)
          WRITE(STRING(13:16), '(I4)') STHRU_PCT(IL, TIMEPERIOD)
          WRITE(STRING(17:20), '(I4)') RIGHT_PCT(IL, TIMEPERIOD)
          WRITE(STRING(21:24), '(I4)') LDIAG_PCT(IL, TIMEPERIOD) + RDIAG_PCT(IL, TIMEPERIOD)
        ELSE
          WRITE(STRING(9:12), '(I4)') 0
          WRITE(STRING(13:16), '(I4)') 100
          WRITE(STRING(17:20), '(I4)') 0
          WRITE(STRING(21:24), '(I4)') 0
          WRITE(STRING(25:28), '(I4)') 0
        ENDIF
        WRITE(100, '(A)') STRING
      ENDIF
    ENDIF
  ENDDO
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT22
  USE STREET_LINKS
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: IL
! ----------------------------------------------------------------------
  STRING(79:80) = '22'
  DO IL = 1, N_STREET_LINKS
    STRING(1:78) = ''
    IF(COND_LEFT(IL) .OR. COND_THRU(IL) .OR. COND_RIGHT(IL) .OR. COND_LDIAG(IL) .OR. COND_RDIAG(IL)) THEN
      WRITE(STRING(1:4), '(I4)') SUSN(IL)
      WRITE(STRING(5:8), '(I4)') SDSN(IL)
      WRITE(STRING(9:12), '(I4)') INT(COND_LEFTPCT(IL, 1))
      WRITE(STRING(13:16), '(I4)') INT(COND_LEFTPCT(IL, 2))
      WRITE(STRING(17:20), '(I4)') INT(COND_LEFTPCT(IL, 3))
      WRITE(STRING(21:24), '(I4)') INT(COND_LEFTPCT(IL, 4) + COND_LEFTPCT(IL, 5))
      WRITE(STRING(25:28), '(I4)') INT(COND_THRUPCT(IL, 1))
      WRITE(STRING(29:32), '(I4)') INT(COND_THRUPCT(IL, 2))
      WRITE(STRING(33:36), '(I4)') INT(COND_THRUPCT(IL, 3))
      WRITE(STRING(37:40), '(I4)') INT(COND_THRUPCT(IL, 4) + COND_THRUPCT(IL, 5))
      WRITE(STRING(41:44), '(I4)') INT(COND_RIGHTPCT(IL, 1))
      WRITE(STRING(45:48), '(I4)') INT(COND_RIGHTPCT(IL, 2))
      WRITE(STRING(49:52), '(I4)') INT(COND_RIGHTPCT(IL, 3))
      WRITE(STRING(53:56), '(I4)') INT(COND_RIGHTPCT(IL, 4) + COND_RIGHTPCT(IL, 5))
      WRITE(STRING(57:60), '(I4)') INT(COND_LDIAGPCT(IL, 1) + COND_RDIAGPCT(IL, 1))
      WRITE(STRING(61:64), '(I4)') INT(COND_LDIAGPCT(IL, 2) + COND_RDIAGPCT(IL, 2))
      WRITE(STRING(65:68), '(I4)') INT(COND_LDIAGPCT(IL, 3) + COND_RDIAGPCT(IL, 3))
      WRITE(STRING(69:72), '(I4)') INT(COND_LDIAGPCT(IL, 4) + COND_LDIAGPCT(IL, 5) + COND_RDIAGPCT(IL, 4) + COND_RDIAGPCT(IL, 5))
      WRITE(100, '(A)') STRING
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE WRITE_RT23
  USE STREET_LINKS
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: IL
! ----------------------------------------------------------------------
!ETFOMM does not currently support turn percentages that vary within a time period  
  STRING(79:80) = '23'
  DO IL = 1, N_STREET_LINKS
    STRING(1:78) = ''
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE WRITE_RT25(TIMEPERIOD)  
  USE FREEWAY_LINKS
  USE DATASTATIONS
  USE API_DATA
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: TIMEPERIOD
  CHARACTER*80 STRING
  INTEGER :: IL
  LOGICAL :: WRITE_IT
! ----------------------------------------------------------------------
  STRING(79:80) = '25'
  DO IL = 1, N_FREEWAY_LINKS
    IF(NODE_TYPE(FDSN(IL)) .NE. NT_INTERN) CYCLE
    STRING(1:78) = ''
    WRITE_IT = .FALSE.
    IF(TIMEPERIOD .EQ. 1) THEN
      WRITE_IT = .TRUE.
    ELSEIF(NODE_TYPE(FUSN(IL)) .NE. NT_EXTERN) THEN
      IF(WRITE25(IL, TIMEPERIOD)) WRITE_IT = .TRUE.
    ENDIF
    IF(WRITE_IT) THEN
      WRITE(STRING(1:4), '(I4)') FUSN(IL)
      WRITE(STRING(5:8), '(I4)') FDSN(IL)
      IF(FTHRU_LINK(IL) .NE. 0) WRITE(STRING(9:12), '(I4)') FDSN(FTHRU_LINK(IL))
      IF(NODE_TYPE(FUSN(IL)) .NE. NT_EXTERN) THEN
        WRITE(STRING(13:16), '(I4)') FTHRU_PCT(IL, TIMEPERIOD)
        IF(OFFRAMP_LINK(IL) .NE. 0) THEN
          WRITE(STRING(17:20), '(I4)') FDSN(OFFRAMP_LINK(IL))
          WRITE(STRING(21:24), '(I4)') 100 - FTHRU_PCT(IL, TIMEPERIOD)
        ENDIF
      ELSE
        WRITE(STRING(13:16), '(I4)') 100
        IF(OFFRAMP_LINK(IL) .NE. 0) THEN
          WRITE(STRING(17:20), '(I4)') FDSN(OFFRAMP_LINK(IL))
          WRITE(STRING(21:24), '(I4)') 0
        ENDIF
      ENDIF
      WRITE(100, '(A)') STRING
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE WRITE_RT28
  USE FREEWAY_LINKS
  USE FREEWAY_DETECTORS
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: IDET, IL
! ----------------------------------------------------------------------
  STRING(79:80) = '28'
  DO IDET = 1, N_FREEWAY_DETECTORS
    STRING(1:78) = ''
    IL = FDETECTOR(IDET)%LINK
    WRITE(STRING(1:4), '(I4)') FUSN(IL)
    WRITE(STRING(5:8), '(I4)') FDSN(IL)
    WRITE(STRING(9:12), '(I4)') FDETECTOR(IDET)%LANE1
    WRITE(STRING(13:16), '(I4)') INT(FDETECTOR(IDET)%LOCATION)
    WRITE(STRING(17:20), '(I4)') INT(FDETECTOR(IDET)%ZONE_LENGTH)
    WRITE(STRING(28:28), '(I1)') FDETECTOR(IDET)%TYPE_CODE
    WRITE(STRING(30:32), '(I3)') FDETECTOR(IDET)%STATION_ID
    WRITE(100, '(A)') STRING
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE WRITE_RT29
  USE FREEWAY_LINKS
  USE INCIDENTS
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: INC, IL, ILANE, N, LOC
! ----------------------------------------------------------------------
  STRING(79:80) = '29'
  DO INC = 1, NUMBER_OF_INCIDENTS
    STRING(1:78) = ''
    IL = INCIDENT_LINK(INC)
    WRITE(STRING(1:4), '(I4)') FUSN(IL)
    WRITE(STRING(5:8), '(I4)') FDSN(IL)
    N = 10
    DO ILANE = 1, 5
      WRITE(STRING(N:N), '(I1)') INCIDENT_CODE(INC, ILANE)
      N = N + 2
    ENDDO
    DO ILANE = 11, 13
      WRITE(STRING(N:N), '(I1)') INCIDENT_CODE(INC, ILANE)
      N = N + 2
    ENDDO
    DO ILANE = 18, 16, -1
      WRITE(STRING(N:N), '(I1)') INCIDENT_CODE(INC, ILANE)
      N = N + 2
    ENDDO
    LOC = USN_TO_SEG_END(IL) - INCIDENT_BEGIN_POINT(INC)
    WRITE(STRING(33:37), '(I5)') LOC
    WRITE(STRING(40:44), '(I5)') INCIDENT_BEGIN_POINT(INC) - INCIDENT_END_POINT(INC) - LOC
    WRITE(STRING(45:51), '(I7)') INCIDENT_BEGIN_TIME(INC)
    WRITE(STRING(52:56), '(I5)') INCIDENT_END_TIME(INC) - INCIDENT_BEGIN_TIME(INC)
    WRITE(STRING(61:64), '(I4)') INCIDENT_RBNF(INC)
    WRITE(STRING(65:69), '(I5)') INT(INCIDENT_WARN_POINT(INC) - INCIDENT_BEGIN_POINT(INC))
    WRITE(100, '(A)') STRING
  ENDDO
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT32
  USE FREEWAY_LINKS
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: IL, IADD, N
! ----------------------------------------------------------------------
  STRING(79:80) = '32'
  DO IL = 1, N_FREEWAY_LINKS
    IF(ADDDROP_CODE(IL, 1) .NE. 0) THEN
      STRING(1:78) = ''
      WRITE(STRING(1:4), '(I4)') FUSN(IL)
      WRITE(STRING(5:8), '(I4)') FDSN(IL)
      N = 12
      DO IADD = 1, 3
        IF(ADDDROP_CODE(IL, IADD) .EQ. 0) EXIT
        IF(ADDDROP_CODE(IL, IADD) .EQ. I_ADD) THEN
          STRING(N:N) = '1'
        ELSEIF(ADDDROP_CODE(IL, IADD) .EQ. I_DROP) THEN
          STRING(N:N) = '2'
        ENDIF
        WRITE(STRING(N+2:N+2), '(I1)') ADDDROP_LANE(IL, IADD)
        WRITE(STRING(N+5:N+9), '(I5)') ADDDROP_DIST(IL, IADD)
        WRITE(STRING(N+10:N+14), '(I5)') ADDDROP_WARN(IL, IADD)
        N = N + 20
      ENDDO
      WRITE(100, '(A)') STRING
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE WRITE_RT33(TIMEPERIOD)
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: TIMEPERIOD
  CHARACTER*80 STRING
  INTEGER :: IL
  LOGICAL :: WRITE_IT
! ----------------------------------------------------------------------
  STRING(79:80) = '33'
  DO IL = 1, N_FREEWAY_LINKS
    IF(NHOV_LANES(IL) .NE. 0) THEN
      IF(TIMEPERIOD .EQ. 1) THEN
        WRITE_IT = .TRUE.
      ELSEIF(WRITE33(IL, TIMEPERIOD)) THEN
        WRITE_IT = .TRUE.
      ENDIF
      IF(WRITE_IT) THEN
        STRING(1:78) = ''
        WRITE(STRING(1:4), '(I4)') FUSN(IL)
        WRITE(STRING(5:8), '(I4)') FDSN(IL)
        WRITE(STRING(9:12), '(I4)') NHOV_LANES(IL)
        WRITE(STRING(16:16), '(I1)') HOV_SIDE(IL)
        WRITE(STRING(20:20), '(I1)') HOV_TYPE(IL)
        WRITE(STRING(24:24), '(I1)') HOV_CODE(IL)
        WRITE(STRING(26:30), '(I5)') HOV_BEGIN(IL)
        WRITE(STRING(31:35), '(I5)') HOV_END(IL)
        WRITE(STRING(36:40), '(I5)') HOV_WARN(IL)
        WRITE(STRING(47:50), '(I4)') INT(HOV_PCT(IL) * 100)
        WRITE(100, '(A)') STRING
      ENDIF
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE WRITE_RT35(TIMEPERIOD)  
  USE STREET_LINKS
  USE TIMED_CONTROLLERS
  USE API_DATA
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: TIMEPERIOD
  CHARACTER*80 STRING
  INTEGER :: I, J, N
  LOGICAL :: WRITE_IT
! ----------------------------------------------------------------------
  STRING(79:80) = '35'
  DO I = 1, NUMBER_OF_FTCS
    STRING(1:78) = ''
    WRITE_IT = .FALSE.
    IF(TIMEPERIOD .EQ. 1) THEN
      WRITE_IT = .TRUE.
    ELSEIF(WRITE3536(I, TIMEPERIOD)) THEN
      WRITE_IT = .TRUE.
    ENDIF
    IF(WRITE_IT) THEN
      WRITE(STRING(1:4), '(I4)') FTC_SIGNALS(I)%NODE
      WRITE(STRING(5:8), '(I4)') FTC_SIGNALS(I)%OFFSET
      N = 9
      DO J = 1, FTC_SIGNALS(I)%APPROACHES
        IF(NODE_TYPE(FTC_SIGNALS(I)%APPROACH(J)) .NE. NT_EXTERN) THEN
          WRITE(STRING(N:N+3), '(I4)') SUSN(FTC_SIGNALS(I)%APPROACH(J))
        ELSE
          WRITE(STRING(N:N+3), '(I4)') FTC_SIGNALS(I)%APPROACH(J)
        ENDIF
        N = N + 4
      ENDDO
      IF(FTC_SIGNALS(I)%ACTIVE_INTERVALS .GT. 1) THEN
        N = 30
        DO J = 1, FTC_SIGNALS(I)%ACTIVE_INTERVALS
          WRITE(STRING(N:N+2), '(I3)') DURATIONS(I, J, TIMEPERIOD)
          N = N + 4
        ENDDO
      ENDIF
      IF(TIMEPERIOD .GT. 1) STRING(77:78) = '10'
      WRITE(100, '(A)') STRING
    ENDIF
  ENDDO
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT36(TIMEPERIOD)
  USE TIMED_CONTROLLERS
  USE STREET_LINKS
  USE API_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: TIMEPERIOD
  CHARACTER*80 STRING
  INTEGER :: I, INT, IAP, N
  LOGICAL :: WRITE_IT
! ----------------------------------------------------------------------
  STRING(79:80) = '36'
  DO I = 1, NUMBER_OF_FTCS
    STRING(1:78) = ''
    WRITE_IT = .FALSE.
    IF(TIMEPERIOD .EQ. 1) THEN
      WRITE_IT = .TRUE.
    ELSEIF(WRITE3536(I, TIMEPERIOD)) THEN
      WRITE_IT = .TRUE.
    ENDIF
    IF(WRITE_IT) THEN
      WRITE(STRING(1:4), '(I4)') FTC_SIGNALS(I)%NODE
      N = 5
      DO INT = 1, FTC_SIGNALS(I)%ACTIVE_INTERVALS
        DO IAP = 1, FTC_SIGNALS(I)%APPROACHES
          N = N + 1
          IF(IAP .LE. FTC_SIGNALS(I)%APPROACHES) THEN
            IF(FTC_SIGNALS(I)%ACTIVE_INTERVALS .EQ. 1) THEN
              IF(FTC_SIGNALS(I)%SIGNAL_CODE(IAP, INT) .EQ. S_YIELD) THEN
                WRITE(STRING(N:N), '(I1)') 0
              ELSEIF(FTC_SIGNALS(I)%SIGNAL_CODE(IAP, INT) .EQ. S_PERGRN) THEN
                WRITE(STRING(N:N), '(I1)') 1
              ELSEIF(FTC_SIGNALS(I)%SIGNAL_CODE(IAP, INT) .EQ. S_STOP) THEN
                WRITE(STRING(N:N), '(I1)') 5
              ELSE
                WRITE(STRING(N:N), '(I1)') SIGCODES(I, IAP, INT, TIMEPERIOD)
              ENDIF
            ELSE
              WRITE(STRING(N:N), '(I1)') SIGCODES(I, IAP, INT, TIMEPERIOD)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      WRITE(100, '(A)') STRING
    ENDIF
  ENDDO
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT37(TIMEPERIOD)
  USE API_DATA
  USE RAMP_METERS
  USE FREEWAY_LINKS
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: TIMEPERIOD
  CHARACTER*80 STRING
  INTEGER :: N, IL
  LOGICAL :: WRITE_IT
! ----------------------------------------------------------------------
  STRING(79:80) = '37'
  DO N = 1, NUMBER_OF_RAMPMETERS
    STRING(1:78) = ''
    WRITE_IT = .FALSE.
    IF(TIMEPERIOD .EQ. 1) THEN
      WRITE_IT = .TRUE.
    ELSEIF(WRITE37(N, TIMEPERIOD)) THEN
      WRITE_IT = .TRUE.
    ENDIF
    IF(WRITE_IT) THEN
      IL = RAMPMETERS(N)%LINK
      WRITE(STRING(1:4), '(I4)') FDSN(IL)
      WRITE(STRING(8:8), '(I1)') RAMPMETERS(N)%CONTROL
      IF(TIMEPERIOD .GT. 1 .AND. METER_ONSET(N, TIMEPERIOD) .EQ. 0) THEN
        WRITE(STRING(9:12), '(I4)') SUM(TPSECONDS(1:TIMEPERIOD-1))
      ELSE
        WRITE(STRING(9:12), '(I4)') METER_ONSET(N, TIMEPERIOD)
      ENDIF
      IF(RAMPMETERS(N)%CONTROL .EQ. 1) THEN
        WRITE(STRING(13:16), '(I4)') INT(METER_HEADWAY1(N, TIMEPERIOD) * 10)
      ELSEIF(RAMPMETERS(N)%CONTROL .EQ. 2) THEN
        WRITE(STRING(17:20), '(I4)') RAMPMETERS(N)%CAPACITY
      ENDIF
      IF(METER_TWOPERGREEN(N, TIMEPERIOD)) THEN
        STRING(76:76) = '2'
      ELSE
        STRING(76:76) = '1'
      ENDIF
      WRITE(100, '(A)') STRING
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE WRITE_RT38
  USE RAMP_METERS
  USE FREEWAY_LINKS
  USE FREEWAY_DETECTORS
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: N, D, IDET, IL, K
! ----------------------------------------------------------------------
  STRING(1:78) = ''
  STRING(79:80) = '38'
  DO N = 1, NUMBER_OF_RAMPMETERS
    IDET = RAMPMETERS(N)%DETECTOR(1)
    IF(IDET .EQ. 0) EXIT
    IL = FDETECTOR(IDET)%LINK
    WRITE(STRING(1:4), '(I4)') RAMPMETERS(N)%DSN
    WRITE(STRING(5:8), '(I4)') FUSN(IL)
    WRITE(STRING(9:12), '(I4)') FDSN(IL)
    K = 12
    DO D = 1, 7
      IDET = RAMPMETERS(N)%DETECTOR(D)
      IF(IDET .EQ. 0) EXIT
      WRITE(STRING(K+3:K+4), '(I2)') FDETECTOR(IDET)%LANE1
      WRITE(STRING(K+5:K+8), '(I4)') INT(FDETECTOR(IDET)%LOCATION)
      K = K + 8
    ENDDO
    WRITE(100, '(A)') STRING
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE WRITE_RT42
  USE STREET_LINKS
  USE STREET_DETECTORS
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: IDET, IL
! ----------------------------------------------------------------------
  STRING(79:80) = '42'
  STRING(1:78) = ''
  DO IDET = 1, N_STREET_DETECTORS
    IF(SDETECTOR(IDET)%ASSOCIATED_PHASE .EQ. 0) THEN
      IL = SDETECTOR(IDET)%LINK
      WRITE(STRING(1:4), '(I4)') SUSN(IL)
      WRITE(STRING(5:8), '(I4)') SDSN(IL)
      IF(SDETECTOR(IDET)%LANE1 .EQ. 100) THEN
        WRITE(STRING(12:12), '(I1)') 8
      ELSEIF(SDETECTOR(IDET)%LANE1 .EQ. 200) THEN
        WRITE(STRING(12:12), '(I1)') 9
      ELSE
        WRITE(STRING(12:12), '(I1)') SDETECTOR(IDET)%LANE1
      ENDIF
      IF(SDETECTOR(IDET)%LANE2 .EQ. 100) THEN
        WRITE(STRING(13:13), '(I1)') 8
      ELSEIF(SDETECTOR(IDET)%LANE2 .EQ. 200) THEN
        WRITE(STRING(13:13), '(I1)') 9
      ELSE
        WRITE(STRING(13:13), '(I1)') SDETECTOR(IDET)%LANE2
      ENDIF
      WRITE(STRING(16:20), '(I5)') SLENGTH(IL) * 10 - INT(SDETECTOR(IDET)%LOCATION * 10) - INT(SDETECTOR(IDET)%ZONE_LENGTH * 10)
      WRITE(STRING(23:26), '(I4)') SDETECTOR(IDET)%STATION_ID
      WRITE(STRING(29:32), '(I4)') INT(SDETECTOR(IDET)%ZONE_LENGTH * 10)
      WRITE(STRING(35:35), '(I1)') SDETECTOR(IDET)%OPERATION_CODE
      WRITE(100, '(A)') STRING
    ENDIF
  ENDDO
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT43
  USE ACTUATED_CONTROLLERS
  USE STREET_LINKS
  USE API_DATA
  USE SIMPARAMS
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: IACT, IAP, N
! ----------------------------------------------------------------------
  STRING(79:80) = '43'
  DO IACT = 1, NUMBER_OF_ACS
    STRING(1:78) = ''
    WRITE(STRING(1:4), '(I4)') ABS(AC_SIGNALS(IACT)%NODE(1))
    N = 5
    DO IAP = 1, MIN(AC_SIGNALS(IACT)%N_DIRECT_APPROACHES, 5)
      WRITE(STRING(N:N+3), '(I4)') SUSN(AC_SIGNALS(IACT)%DIRECT_APPROACH(IAP))
      WRITE(STRING(N+4:N+7), '(I4)') SDSN(AC_SIGNALS(IACT)%DIRECT_APPROACH(IAP))
      N = N + 8
    ENDDO
    IF(USE_DCS) THEN
      STRING(54:54) = '1'
      IF(LIMIT_TO_MAXGREEN) STRING(54:54) = '1'
      WRITE(STRING(56:57), '(I2)') DZ_ENTRY_TIME
      WRITE(STRING(59:60), '(I2)') DZ_EXIT_TIME
    ENDIF
    IF(AC_SIGNALS(IACT)%EXTERNAL_CONTROL) STRING(77:77) = '1'
    WRITE(100, '(A)') STRING
  ENDDO
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT44(TIMEPERIOD)
  USE ACTUATED_CONTROLLERS
  USE STREET_LINKS
  USE API_DATA
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: TIMEPERIOD
  CHARACTER*80 STRING
  INTEGER :: IACT
  LOGICAL :: WRITE_IT
! ----------------------------------------------------------------------
  STRING(79:80) = '44'
  DO IACT = 1, NUMBER_OF_ACS
    STRING(1:78) = ''
    WRITE_IT = .FALSE.
    IF(TIMEPERIOD .EQ. 1) THEN
      WRITE_IT = .TRUE.
    ELSEIF(WRITE4X(IACT, TIMEPERIOD)) THEN
      WRITE_IT = .TRUE.
    ENDIF
    IF(WRITE_IT) THEN
      WRITE(STRING(1:4), '(I4)') ABS(ACSIGNAL_DATA(IACT, TIMEPERIOD)%NODE(1))
      WRITE(STRING(5:7), '(I3)') INT(ACSIGNAL_DATA(IACT, TIMEPERIOD)%CYCLE_LENGTH)
      WRITE(STRING(8:10), '(I3)') INT(ACSIGNAL_DATA(IACT, TIMEPERIOD)%OFFSET)
      WRITE(STRING(29:31), '(I3)') INT(ACSIGNAL_DATA(IACT, TIMEPERIOD)%FORCE_OFF_TIME(1))
      WRITE(STRING(35:37), '(I3)') INT(ACSIGNAL_DATA(IACT, TIMEPERIOD)%FORCE_OFF_TIME(3))
      WRITE(STRING(38:40), '(I3)') INT(ACSIGNAL_DATA(IACT, TIMEPERIOD)%FORCE_OFF_TIME(4))
      WRITE(STRING(41:43), '(I3)') INT(ACSIGNAL_DATA(IACT, TIMEPERIOD)%FORCE_OFF_TIME(5))
      WRITE(STRING(47:49), '(I3)') INT(ACSIGNAL_DATA(IACT, TIMEPERIOD)%FORCE_OFF_TIME(7))
      WRITE(STRING(50:52), '(I3)') INT(ACSIGNAL_DATA(IACT, TIMEPERIOD)%FORCE_OFF_TIME(8))
      IF(READ_SPLITS) STRING(68:68) = '1'
      WRITE(100, '(A)') STRING
    ENDIF
  ENDDO
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT45(TIMEPERIOD)
  USE ACTUATED_CONTROLLERS
  USE STREET_LINKS
  USE API_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: TIMEPERIOD
  CHARACTER*80 STRING
  INTEGER :: I, IACT, PHASE, IAP, CODE(5), KK
  LOGICAL :: WRITE_IT
! ----------------------------------------------------------------------
  STRING(79:80) = '45'
  DO IACT = 1, NUMBER_OF_ACS
    STRING(1:78) = ''
    WRITE_IT = .FALSE.
    IF(TIMEPERIOD .EQ. 1) THEN
      WRITE_IT = .TRUE.
    ELSEIF(WRITE4X(IACT, TIMEPERIOD)) THEN
      WRITE_IT = .TRUE.
    ENDIF
    IF(WRITE_IT) THEN
      WRITE(STRING(1:4), '(I4)') ABS(ACSIGNAL_DATA(IACT, TIMEPERIOD)%NODE(1))
      DO PHASE = 1, 8
        IF(ACSIGNAL_DATA(IACT, TIMEPERIOD)%PHASE(PHASE)%IN_USE) THEN
          WRITE(STRING(8:8), '(I1)') PHASE
          STRING(10:54) = ''
          KK = 10
          DO IAP = 1, MIN(AC_SIGNALS(IACT)%N_DIRECT_APPROACHES, 5)
            CODE = 0
            IF(ACSIGNAL_DATA(IACT, TIMEPERIOD)%PHASE(PHASE)%LEFTARROW(IAP)) THEN
              CODE(1) = 1
            ELSE
              CODE(1) = 2
            ENDIF
            IF(ACSIGNAL_DATA(IACT, TIMEPERIOD)%PHASE(PHASE)%THRUARROW(IAP)) THEN
              CODE(2) = 1
            ELSE
              CODE(2) = 2
            ENDIF
            IF(ACSIGNAL_DATA(IACT, TIMEPERIOD)%PHASE(PHASE)%RIGHTARROW(IAP)) THEN
              CODE(3) = 1
            ELSE
              CODE(3) = 2
            ENDIF
            IF(ACSIGNAL_DATA(IACT, TIMEPERIOD)%PHASE(PHASE)%LDIAGARROW(IAP)) THEN
              CODE(4) = 1
            ELSE
              CODE(4) = 2
            ENDIF
            IF(ACSIGNAL_DATA(IACT, TIMEPERIOD)%PHASE(PHASE)%RDIAGARROW(IAP)) THEN
              CODE(5) = 1
            ELSE
              CODE(5) = 2
            ENDIF
            WRITE(STRING(KK:KK+4), '(5I1)') (CODE(I), I = 1, 5)
            KK = KK + 10
          ENDDO
          WRITE(100, '(A)') STRING
        ENDIF
      ENDDO
    ENDIF
  ENDDO
  RETURN
  END
   
! ==================================================================================================
  SUBROUTINE WRITE_RT46(TIMEPERIOD)
  USE ACTUATED_CONTROLLERS
  USE STREET_LINKS
  USE API_DATA
  USE STREET_DETECTORS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: TIMEPERIOD
  CHARACTER*80 STRING
  INTEGER :: IACT, PHASE, IDET, IAP, IL
  LOGICAL :: WRITE_IT 
! ----------------------------------------------------------------------
  STRING(79:80) = '46'
  IF(N_STREET_DETECTORS .GT. 0) THEN
    DO IACT = 1, NUMBER_OF_ACS
      WRITE_IT = .FALSE.
      IF(TIMEPERIOD .EQ. 1) THEN
        WRITE_IT = .TRUE.
      ELSEIF(WRITE4X(IACT, TIMEPERIOD)) THEN
        WRITE_IT = .TRUE.
      ENDIF
      IF(WRITE_IT) THEN
        STRING(1:78) = ''
        STRING(8:8) = '1'
        WRITE(STRING(1:4), '(I4)') ABS(AC_SIGNALS(IACT)%NODE(1))
        DO PHASE = 1, 8
          DO IDET = 1, N_STREET_DETECTORS
            IF(SDETECTOR(IDET)%ASSOCIATED_PHASE .EQ. PHASE) THEN
              DO IAP = 1, MIN(AC_SIGNALS(IACT)%N_DIRECT_APPROACHES, 5)
                IF(AC_SIGNALS(IACT)%DIRECT_APPROACH(IAP) .EQ. SDETECTOR(IDET)%LINK) THEN
                  WRITE(STRING(6:6), '(I1)') SDETECTOR(IDET)%ASSOCIATED_PHASE
                  WRITE(STRING(10:11), '(I2)') IAP
                  IF(SDETECTOR(IDET)%LANE1 .EQ. 100) THEN
                    WRITE(STRING(12:12), '(I1)') 8
                  ELSEIF(SDETECTOR(IDET)%LANE1 .EQ. 200) THEN
                    WRITE(STRING(12:12), '(I1)') 9
                  ELSE
                    IF(SDETECTOR(IDET)%LANE1 .LE. LAST_FULL_LANE(SDETECTOR(IDET)%LINK)) THEN
                      WRITE(STRING(12:12), '(I1)') SDETECTOR(IDET)%LANE1
                    ELSEIF(SDETECTOR(IDET)%LANE1 .GT. LAST_FULL_LANE(SDETECTOR(IDET)%LINK)) THEN
                      WRITE(STRING(12:12), '(I1)') LANE_NUMBERS(SDETECTOR(IDET)%LINK, SDETECTOR(IDET)%LANE1)
                    ENDIF
                  ENDIF
                  IF(SDETECTOR(IDET)%LANE2 .EQ. 100) THEN
                    WRITE(STRING(13:13), '(I1)') 8
                  ELSEIF(SDETECTOR(IDET)%LANE2 .EQ. 200) THEN
                    WRITE(STRING(13:13), '(I1)') 9
                  ELSE
                    IF(SDETECTOR(IDET)%LANE2 .LE. LAST_FULL_LANE(SDETECTOR(IDET)%LINK)) THEN
                      WRITE(STRING(13:13), '(I1)') SDETECTOR(IDET)%LANE2
                    ELSEIF(SDETECTOR(IDET)%LANE1 .GT. LAST_FULL_LANE(SDETECTOR(IDET)%LINK)) THEN
                      WRITE(STRING(13:13), '(I1)') LANE_NUMBERS(SDETECTOR(IDET)%LINK, SDETECTOR(IDET)%LANE2)
                    ENDIF
                  ENDIF
                  IL = SDETECTOR(IDET)%LINK
                  WRITE(STRING(14:17), '(I4)') (SLENGTH(IL) * 10) - INT(SDETECTOR(IDET)%LOCATION * 10)
                  WRITE(STRING(18:20), '(I3)') INT(SDETECTOR(IDET)%DELAY_TIME * 10)
                  WRITE(STRING(21:23), '(I3)') INT(SDETECTOR(IDET)%CARRYOVER_TIME * 10)
                  WRITE(STRING(24:26), '(I3)') INT(SDETECTOR(IDET)%ZONE_LENGTH * 10)
                  WRITE(STRING(29:29), '(I1)') SDETECTOR(IDET)%OPERATION_CODE
                  WRITE(100, '(A)') STRING
                  EXIT
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ENDDO
      ENDIF
    ENDDO
  ENDIF
  RETURN
  END
   
! ==================================================================================================
  SUBROUTINE WRITE_RT47(TIMEPERIOD)
  USE ACTUATED_CONTROLLERS
  USE STREET_LINKS
  USE API_DATA
  USE STREET_DETECTORS
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: TIMEPERIOD
  CHARACTER*80 STRING
  INTEGER :: IACT, PHASE
  LOGICAL :: WRITE_IT
  REAL :: TEMP
! ----------------------------------------------------------------------
  STRING(79:80) = '47'
  DO IACT = 1, NUMBER_OF_ACS
    WRITE_IT = .FALSE.
    IF(TIMEPERIOD .EQ. 1) THEN
      WRITE_IT = .TRUE.
    ELSEIF(WRITE4X(IACT, TIMEPERIOD)) THEN
      WRITE_IT = .TRUE.
    ENDIF
    IF(WRITE_IT) THEN
      WRITE(STRING(1:4), '(I4)') ABS(ACSIGNAL_DATA(IACT, TIMEPERIOD)%NODE(1))
      DO PHASE = 1, 8
        WRITE_IT = .FALSE.
        IF(ACSIGNAL_DATA(IACT, TIMEPERIOD)%PHASE(PHASE)%IN_USE) THEN
          STRING(5:78) = ''
          WRITE(STRING(6:6), '(I1)') PHASE
          WRITE(STRING(8:10), '(I3)') INT(ACSIGNAL_DATA(IACT, TIMEPERIOD)%GUI_MAX_GREEN_TIMES(PHASE))
          WRITE(STRING(11:13), '(I3)') INT(ACSIGNAL_DATA(IACT, TIMEPERIOD)%GUI_MIN_GREEN_TIMES(PHASE))
          IF(AC_SIGNALS(IACT)%GUI_DEFAULT_EXTENSION_TIMES(PHASE) .NE. 0) THEN
            WRITE(STRING(14:15), '(I2)') INT(ACSIGNAL_DATA(IACT, TIMEPERIOD)%GUI_DEFAULT_EXTENSION_TIMES(PHASE) * 10)
            WRITE(STRING(29:29), '(I1)') 2
            TEMP = AC_SIGNALS(IACT)%GUI_TIME_BEFORE_REDUCTION(PHASE) - AC_SIGNALS(IACT)%GUI_TIME_TO_REDUCE(PHASE)
            WRITE(STRING(30:33), '(I4)') INT(TEMP) * 100
            WRITE(STRING(34:36), '(I3)') INT(ACSIGNAL_DATA(IACT, TIMEPERIOD)%GUI_TIME_TO_REDUCE(PHASE))
            WRITE(STRING(37:40), '(I4)') INT(ACSIGNAL_DATA(IACT, TIMEPERIOD)%GUI_MIN_GAP_TIMES(PHASE) * 100)
            WRITE(STRING(41:44), '(I4)') INT(ACSIGNAL_DATA(IACT, TIMEPERIOD)%GUI_GAP_TIMES(PHASE) * 100)
          ENDIF
          WRITE(STRING(45:47), '(I3)') INT(ACSIGNAL_DATA(IACT, TIMEPERIOD)%GUI_YC(PHASE) * 10)
          WRITE(STRING(48:50), '(I3)') INT(ACSIGNAL_DATA(IACT, TIMEPERIOD)%GUI_RC(PHASE) * 10)
          IF(ACSIGNAL_DATA(IACT, TIMEPERIOD)%GUI_ACTUATED_MODE(PHASE) .EQ. RECALL_MIN) THEN
            WRITE(STRING(55:55), '(I1)') 1
          ELSEIF(ACSIGNAL_DATA(IACT, TIMEPERIOD)%GUI_ACTUATED_MODE(PHASE) .EQ. RECALL_MAX) THEN
            WRITE(STRING(56:56), '(I1)') 1
          ENDIF
          IF(ACSIGNAL_DATA(IACT, TIMEPERIOD)%LEAD(PHASE)) THEN
            WRITE(STRING(58:58), '(I1)') 0
          ELSE
            WRITE(STRING(58:58), '(I1)') 1
          ENDIF
          
          !WRITE(STRING(51:51), '(I1)') AC_SIGNALS(IACT)%RED_LOCK_CODE(PHASE) 
          !WRITE(STRING(52:52), '(I1)') AC_SIGNALS(IACT)%YELLOW_LOCK_CODE(PHASE)
          WRITE(STRING(53:53), '(I1)') ACSIGNAL_DATA(IACT, TIMEPERIOD)%DUAL_ENTRY_CODE(PHASE)
          !WRITE(STRING(63:66), '(I4)') INT(ACSIGNAL_DATA(IACT, TIMEPERIOD)%RED_REVERT_TIME(PHASE) * 10)
          !WRITE(STRING(72:72), '(I1)') ACSIGNAL_DATA(IACT, TIMEPERIOD)%CONDITIONAL_SERVICE_CODE(PHASE)
          !WRITE(STRING(75:76), '(I2)') INT(ACSIGNAL_DATA(IACT, TIMEPERIOD)%MIN_CONDITIONAL_SERVICE_TIME(PHASE))

          WRITE(100, '(A)') STRING
        ENDIF  
      ENDDO
    ENDIF
  ENDDO
  RETURN
  END
  
!! ==================================================================================================
!  SUBROUTINE WRITE_RT48
!  USE ACTUATED_CONTROLLERS
!  USE STREET_LINKS
!  USE API_DATA
!  USE STREET_DETECTORS
!  IMPLICIT NONE
!  CHARACTER*80 STRING
!! ----------------------------------------------------------------------
!  STRING(79:80) = '48'
!  RETURN
!  END
!
! ==================================================================================================
  SUBROUTINE WRITE_RT49(TIMEPERIOD)
  USE ACTUATED_CONTROLLERS
  USE STREET_LINKS
  USE API_DATA
  USE STREET_DETECTORS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: TIMEPERIOD
  CHARACTER*80 STRING
  LOGICAL :: WRITE_IT
  INTEGER :: IACT
! ----------------------------------------------------------------------
  STRING(79:80) = '49'
  DO IACT = 1, NUMBER_OF_ACS
    IF(ACSIGNAL_DATA(IACT, TIMEPERIOD)%TRANSITION_METHOD .GT. 3) CYCLE
    STRING(1:78) = ''
    WRITE_IT = .FALSE.
    IF(TIMEPERIOD .EQ. 1) THEN
      WRITE_IT = .TRUE.
    ELSEIF(WRITE4X(IACT, TIMEPERIOD)) THEN
      WRITE_IT = .TRUE.
    ENDIF
    IF(WRITE_IT) THEN
      WRITE(STRING(1:4), '(I4)') ABS(ACSIGNAL_DATA(IACT, TIMEPERIOD)%NODE(1))
      WRITE(STRING(5:8), '(I4)') ACSIGNAL_DATA(IACT, TIMEPERIOD)%TRANSITION_METHOD
      WRITE(STRING(9:12), '(I4)') ACSIGNAL_DATA(IACT, TIMEPERIOD)%MAXPCT_ADD
      WRITE(STRING(13:16), '(I4)') ACSIGNAL_DATA(IACT, TIMEPERIOD)%MAXPCT_SUBTRACT
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE WRITE_RT50(INET, TIMEPERIOD)
  USE ENTRYNODE_DATA
  USE FREEWAY_LINKS
  USE STREET_LINKS
  USE GLOBAL_DATA
  USE API_DATA
  USE FLOWDATA_MOD
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: INET, TIMEPERIOD
  CHARACTER*80 STRING
  INTEGER :: I, IL, DSN, TOTAL, PCT(5), NODE
  LOGICAL :: WRITE_IT
! ----------------------------------------------------------------------
  STRING(79:80) = '50'
  DO I = MAX_NODE_NUMBER + 1, MAX_NODE_NUMBER + 1000
    STRING(1:78) = ''
    IF(ENTRYNODE_IS_USED(I)) THEN
      DSN = 0
      IF(INET .EQ. I_FREEWAY) THEN
        DO IL = 1, N_FREEWAY_LINKS
          IF(FUSN(IL) .EQ. I) THEN
            DSN = FDSN(IL)
            EXIT
          ENDIF
        ENDDO
      ELSE
        DO IL = 1, N_STREET_LINKS
          IF(SUSN(IL) .EQ. I) THEN
            DSN = SDSN(IL)
            EXIT
          ENDIF
        ENDDO
      ENDIF
      IF(DSN .NE. 0) THEN
        WRITE_IT = .FALSE.
        IF(TIMEPERIOD .EQ. 1) THEN
          WRITE_IT = .TRUE.
        ELSEIF(WRITE50(I, TIMEPERIOD)) THEN
          WRITE_IT = .TRUE.
        ENDIF
        IF(WRITE_IT) THEN
          PCT = 100 * LANE_PCT(I, 1:5)
          TOTAL = SUM(PCT)
          IF(TOTAL .NE. 100) THEN
            PCT(1) = PCT(1) + 100 - TOTAL
          ENDIF
          WRITE(STRING(1:4), '(I4)') I
          WRITE(STRING(5:8), '(I4)') DSN
          WRITE(STRING(9:12), '(I4)') MIN(9999, FLOW_RATE(I, TIMEPERIOD))
          WRITE(STRING(13:16), '(I4)') INT(TRUCK_PCT(I) * 100)
          WRITE(STRING(17:20), '(I4)') INT(CARPOOL_PCT(I) * 100)
          WRITE(STRING(21:25), '(I4)') INT(HOV_VIOLATOR_PCT(I) * 10000)
          WRITE(STRING(61:63), '(I3)') PCT(1)
          WRITE(STRING(64:66), '(I3)') PCT(2)
          WRITE(STRING(67:69), '(I3)') PCT(3)
          WRITE(STRING(70:72), '(I3)') PCT(4)
          WRITE(STRING(73:75), '(I3)') PCT(5)
          WRITE(100, '(A)') STRING
        ENDIF
      ENDIF
    ENDIF
  ENDDO
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT51
  USE STREET_LINKS
  USE FLOWDATA_MOD
  USE GLOBAL_DATA
  USE SIMPARAMS
  USE NODE_TABLE
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: IN, IL, I
! ----------------------------------------------------------------------
  STRING(79:80) = '51'
  DO IN = 1, NUMBER_OF_ENTRYNODES
    IF(ENTRYLINK(IN)%NETWORK .EQ. I_STREET) THEN
      IL = ENTRYLINK(IN)%LINKID
      IF(NODE_TYPE(SUSN(IL)) .NE. NT_EXTERN) THEN
        STRING(1:78) = ''
        WRITE(STRING(1:76), '(19I4)') CENTROID_LABEL(IL), SUSN(IL), SDSN(IL),&
            (ENTRYLINK(IN)%FLOW(I), ENTRYLINK(IN)%TIME(I), I = 1, ENTRYLINK(IN)%INDEX) 
        WRITE(100, '(A)') STRING
      ENDIF
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE WRITE_RT53(TIMEPERIOD)
  USE STREET_LINKS
  USE ENTRYNODE_DATA
  USE FLOWDATA_MOD
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: TIMEPERIOD
  CHARACTER*80 STRING
  INTEGER :: I, J, DSN, IND, T(16), V(16), K, T1, T2
! ----------------------------------------------------------------------
  STRING(79:80) = '53'
  STRING(76:78) = '1  '
  T1 = SUM(TPSECONDS(1:TIMEPERIOD-1)) / 60
  T2 = SUM(TPSECONDS(1:TIMEPERIOD)) / 60
  DO I = 8000, 8999
    STRING(1:75) = ''
    IF(WRITE53(I, TIMEPERIOD)) THEN
      DSN = 0
      DO J = 1, NUMBER_OF_ENTRYNODES
        IF(ENTRYLINK(J)%UP .EQ. I) THEN
          DSN = ENTRYLINK(J)%DOWN
          K = 0
          V = 0
          T = 0
          DO IND = 1, ENTRYLINK(J)%INDEX
            IF(ENTRYLINK(J)%TIME(IND) .GE. T1 .AND. ENTRYLINK(J)%TIME(IND) .LT. T2) THEN
              K = K + 1
              IF(K .GT. 16) EXIT
              V(K) = ENTRYLINK(J)%FLOW(IND)
              T(K) = ENTRYLINK(J)%TIME(IND)
            ENDIF
          ENDDO
          IF(K .GT. 0) THEN
            WRITE(STRING(1:4), '(I4)') I
            WRITE(STRING(5:8), '(I4)') DSN
            WRITE(STRING(9:72), '(16I4)') (V(K), T(K), K = 1, 8)
            WRITE(100, '(A)') STRING
            IF(K .GT. 8) THEN
              WRITE(STRING(9:72), '(16I4)') (V(K), T(K), K = 9, 16)
              WRITE(100, '(A)') STRING
            ENDIF
          ENDIF
          EXIT
        ENDIF
      ENDDO
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE WRITE_RT54
  USE STREET_LINKS
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: IL
! ----------------------------------------------------------------------
  STRING(1:78) = ''
  STRING(79:80) = '54'
  DO IL = 1, N_STREET_LINKS
    IF(STE_FREQ(IL) .NE. 0) THEN
      WRITE(STRING(1:16), '(4I4)') SUSN(IL), SDSN(IL), STE_FREQ(IL), STE_DURATION(IL)
      WRITE(100, '(A)') STRING
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE WRITE_RT55
  USE STREET_LINKS
  USE EVENTS
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: I, IL, DURATION
! ----------------------------------------------------------------------
  STRING(1:78) = ''
  STRING(79:80) = '55'
  DO I = 1, NUMBER_OF_EVENTS
    IF(EVENT_TYPE(I) .EQ. 3) CYCLE
    IL = EVENT_LINK(I)
    DURATION = EVENT_END_TIME(I) - EVENT_BEGIN_TIME(I) 
    WRITE(STRING(1:40), '(5I4,I8,3I4)') SUSN(IL), SDSN(IL), EVENT_BEGIN_TIME(I), DURATION, EVENT_LANE(I), &
      EVENT_LOCATION(I), EVENT_LENGTH(I), EVENT_CODE(I), EVENT_SPEED_REDUCTION(I)
    WRITE(100, '(A)') STRING
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE WRITE_RT56(TIMEPERIOD)
  USE PARKING
  USE STREET_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: TIMEPERIOD
  CHARACTER*80 STRING
  INTEGER :: I, IL, R1, R2, L1, L2
  LOGICAL :: WRITE_IT 
! ----------------------------------------------------------------------
  STRING(1:78) = ''
  STRING(79:80) = '56'
  WRITE_IT = .FALSE.
  IF(TIMEPERIOD .EQ. 1) WRITE_IT = .TRUE.
  DO I = 1, NUMBER_OF_PARKING_ZONES
    IF(WRITE56(I, TIMEPERIOD)) WRITE_IT = .TRUE.
    IF(WRITE_IT) THEN
      IL = PARKING_ZONE_LINK(I)
      L1 = 0
      L2 = 0
      R1 = 0
      R2 = 0
      IF(PARK_LEFT_LEN(I) .GT. 0) THEN
        L1 = SLENGTH(IL) - PARK_LEFT_START(I) - PARK_LEFT_LEN(I)
        L2 = PARK_LEFT_LEN(I)
      ENDIF
      IF(PARK_RIGHT_LEN(I) .GT. 0) THEN
        R1 = SLENGTH(IL) - PARK_RIGHT_START(I) - PARK_RIGHT_LEN(I)
        R2 = PARK_RIGHT_LEN(I)
      ENDIF
      WRITE(STRING(1:32), '(8I4)') SUSN(IL), SDSN(IL), R1, R2, L1, L2, PARK_DURATION(I) * 10, PARK_FREQ(I)
      WRITE(100, '(A)') STRING
    ENDIF
  ENDDO
  RETURN
  END  

! ==================================================================================================
  SUBROUTINE WRITE_RT58
  USE VEHICLE_TYPES
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: ITYPE
! ----------------------------------------------------------------------
  IF(WRITE58) THEN
    STRING(79:80) = '58'
    DO ITYPE = 1, 9
      STRING(1:78) = ''
      WRITE(STRING(1:4), '(I4)') ITYPE
      WRITE(STRING(5:8), '(I4)') VTLENGTH(ITYPE)
      WRITE(STRING(17:20), '(I4)') INT(HDWY_FACTOR(ITYPE) * 100)
      WRITE(STRING(41:44), '(I4)') FLT_STREET_AUTO(ITYPE)
      WRITE(STRING(45:48), '(I4)') FLT_STREET_TRUCK(ITYPE)
      WRITE(STRING(49:52), '(I4)') FLT_STREET_BUS(ITYPE)
      WRITE(STRING(53:56), '(I4)') FLT_STREET_CARPOOL(ITYPE)
      WRITE(STRING(73:76), '(I4)') INT(AVG_OCCS(ITYPE) * 100)
      WRITE(100, '(A)') STRING
    ENDDO
  ENDIF
  RETURN
  END

!! ==================================================================================================
!  SUBROUTINE WRITE_RT70
!  USE VEHICLE_TYPES
!  IMPLICIT NONE
!  CHARACTER*80 STRING
!! ----------------------------------------------------------------------
!  STRING(1:80) = '70'
!  WRITE(STRING(1:4), '(I4)')
!  WRITE(STRING(5:8), '(I4)')
!  WRITE(STRING(9:12), '(I4)')
!  WRITE(STRING(13:16), '(I4)') 
!  WRITE(STRING(17:20), '(I4)') 
!  WRITE(STRING(21:24), '(I4)') 
!  WRITE(STRING(25:28), '(I4)') 
!  WRITE(STRING(29:32), '(I4)') 
!  WRITE(STRING(33:36), '(I4)') 
!  WRITE(STRING(37:40), '(I4)') 
!  WRITE(STRING(41:44), '(I4)') 
!  WRITE(STRING(45:48), '(I4)') 
!  WRITE(STRING(49:52), '(I4)') 
!  WRITE(STRING(53:56), '(I4)') 
!  WRITE(STRING(57:60), '(I4)') 
!  WRITE(STRING(61:64), '(I4)') 
!  WRITE(STRING(64:68), '(I4)') 
!  WRITE(100, '(A)') STRING
!  RETURN
!  END

! ==================================================================================================
  SUBROUTINE WRITE_RT71
  USE VEHICLE_TYPES
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: ITYPE
! ----------------------------------------------------------------------
  STRING(79:80) = '71'
  DO ITYPE = 1, 9
    STRING(1:78) = ''
    WRITE(STRING(1:4), '(I4)') ITYPE
    WRITE(STRING(5:8), '(I4)') VTLENGTH(ITYPE)
    !WRITE(STRING(9:12), '(I4)') JERK(ITYPE)
    WRITE(STRING(13:16), '(I4)') INT(10*NEMDEC(ITYPE))
    WRITE(STRING(17:20), '(I4)') FLT_FREEWAY_AUTO(ITYPE)
    WRITE(STRING(21:24), '(I4)') FLT_FREEWAY_TRUCK(ITYPE)
    WRITE(STRING(25:28), '(I4)') FLT_FREEWAY_BUS(ITYPE)
    WRITE(STRING(29:32), '(I4)') FLT_FREEWAY_CARPOOL(ITYPE)
    !WRITE(STRING(33:36), '(I4)') PERF_INDEX(ITYPE)
    WRITE(STRING(37:40), '(I4)') INT(AVG_OCCS(ITYPE) * 100)
    WRITE(100, '(A)') STRING
  ENDDO
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT80
  USE STREET_LINKS
  USE GLOBAL_DATA
  USE NODE_TABLE
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: IL, I, N
  LOGICAL :: WRITE_IT
! ----------------------------------------------------------------------
  STRING(79:80) = '80'
  DO IL = 1, N_STREET_LINKS
    IF(NODE_TYPE(SUSN(IL)) .NE. NT_INTERN .OR. NODE_TYPE(SDSN(IL)) .NE. NT_INTERN) CYCLE
    WRITE_IT = .FALSE.
    DO I = 1, 7
      IF(SLANE_WIDTH(IL, I) .NE. STD_WIDTH) WRITE_IT = .TRUE.
      EXIT
    ENDDO
    IF(WRITE_IT) THEN
      STRING(1:78) = ''
      WRITE(STRING(1:4), '(I4)') SUSN(IL)
      WRITE(STRING(5:8), '(I4)') SDSN(IL)
      N = 13
      DO I = 1, 7
        WRITE(STRING(N:N+3), '(I4)') INT(SLANE_WIDTH(IL, I))
        N = N + 4
      ENDDO
      WRITE(STRING(41:44), '(I4)') 4
      WRITE(STRING(45:48), '(I4)') 1000
      WRITE(STRING(49:52), '(I4)') 0
      WRITE(100, '(A)') STRING
    ENDIF
  ENDDO
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT82
  USE STREET_LINKS
  USE NODE_TABLE
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: IL, I, N
! ----------------------------------------------------------------------
  STRING(79:80) = '82'
  DO IL = 1, N_STREET_LINKS
    IF(WRITE82(IL)) THEN
      STRING(1:78) = ''
      WRITE(STRING(1:4), '(I4)') SUSN(IL)
      WRITE(STRING(5:8), '(I4)') SDSN(IL)
      WRITE(STRING(9:12), '(I4)') INT(UP_INT_WIDTH(IL))
      N = 13
      DO I = 1, 7
        WRITE(STRING(N:N+3), '(I4)') (INT(LANE_CENTER(IL, I)))
        N = N + 4
      ENDDO
      WRITE(100, '(A)') STRING
    ENDIF
  ENDDO
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT83
  USE STREET_LINKS
  USE NODE_TABLE
  USE VEHICLE_PARAMETERS
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: IL, ENTRY_LANE, EXIT_LANE, IT
! ----------------------------------------------------------------------
  STRING(79:80) = '83'
  DO IL = 1, N_STREET_LINKS
    IF(WRITE83(IL)) THEN
      STRING(1:78) = ''
      WRITE(STRING(1:4), '(I4)') SUSN(IL)
      WRITE(STRING(5:8), '(I4)') SDSN(IL)
      DO ENTRY_LANE = 1, 7
        DO EXIT_LANE = 1, 7
          DO IT = 0, 4
            IF(IT .EQ. TC_LEFT .AND. LT_ARC_LENGTH(IL, ENTRY_LANE, EXIT_LANE) .NE. 0) THEN
              WRITE(STRING(9:12), '(I4)') IT
              WRITE(STRING(13:16), '(I4)') ENTRY_LANE
              WRITE(STRING(17:20), '(I4)') EXIT_LANE
              WRITE(STRING(21:24), '(I4)') INT(LT_ARC_LENGTH(IL, ENTRY_LANE, EXIT_LANE))
              WRITE(100, '(A)') STRING
            ELSEIF(IT .EQ. TC_THRU .AND. THRU_ARC_LENGTH(IL, ENTRY_LANE, EXIT_LANE) .NE. 0) THEN
              WRITE(STRING(9:12), '(I4)') IT
              WRITE(STRING(13:16), '(I4)') ENTRY_LANE
              WRITE(STRING(17:20), '(I4)') EXIT_LANE
              WRITE(STRING(21:24), '(I4)') INT(THRU_ARC_LENGTH(IL, ENTRY_LANE, EXIT_LANE))
              WRITE(100, '(A)') STRING
            ELSEIF(IT .EQ. TC_RIGHT .AND. RT_ARC_LENGTH(IL, ENTRY_LANE, EXIT_LANE) .NE. 0) THEN
              WRITE(STRING(9:12), '(I4)') IT
              WRITE(STRING(13:16), '(I4)') ENTRY_LANE
              WRITE(STRING(17:20), '(I4)') EXIT_LANE
              WRITE(STRING(21:24), '(I4)') INT(RT_ARC_LENGTH(IL, ENTRY_LANE, EXIT_LANE))
              WRITE(100, '(A)') STRING
            ELSEIF(IT .EQ. TC_LDIAG .AND. LD_ARC_LENGTH(IL, ENTRY_LANE, EXIT_LANE) .NE. 0) THEN
              WRITE(STRING(9:12), '(I4)') IT
              WRITE(STRING(13:16), '(I4)') ENTRY_LANE
              WRITE(STRING(17:20), '(I4)') EXIT_LANE
              WRITE(STRING(21:24), '(I4)') INT(LD_ARC_LENGTH(IL, ENTRY_LANE, EXIT_LANE))
              WRITE(100, '(A)') STRING
            ELSEIF(IT .EQ. TC_RDIAG .AND. RD_ARC_LENGTH(IL, ENTRY_LANE, EXIT_LANE) .NE. 0) THEN
              WRITE(STRING(9:12), '(I4)') IT
              WRITE(STRING(13:16), '(I4)') ENTRY_LANE
              WRITE(STRING(17:20), '(I4)') EXIT_LANE
              WRITE(STRING(21:24), '(I4)') INT(RD_ARC_LENGTH(IL, ENTRY_LANE, EXIT_LANE))
              WRITE(100, '(A)') STRING
            ENDIF
          ENDDO
        ENDDO
      ENDDO
    ENDIF
  ENDDO
  RETURN
  END

  
! ==================================================================================================
  SUBROUTINE WRITE_RT97
  USE ROUNDABOUT_DATA
  USE STREET_LINKS
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: IRND, IAP, N
! ----------------------------------------------------------------------
  STRING(1:78) = ''
  STRING(79:80) = '97'
  DO IRND = 1, NUMBER_OF_ROUNDABOUTS
    WRITE(STRING(1:4), '(I4)') IRND
    N = 5
    DO IAP = 1, 4
      IF(IAP .GT. ROUNDABOUT(IRND)%APPROACHES) EXIT
      WRITE(STRING(N:N+3), '(I4)') SUSN(ROUNDABOUT(IRND)%APPROACH_LINKS(IAP))
      WRITE(STRING(N+4:N+7), '(I4)') SDSN(ROUNDABOUT(IRND)%APPROACH_LINKS(IAP))
      N = N + 8
    ENDDO
    N = 37
    DO IAP = 1, 4
      IF(IAP .GT. ROUNDABOUT(IRND)%APPROACHES) EXIT
      WRITE(STRING(N:N+3), '(I4)') SUSN(ROUNDABOUT(IRND)%DEPARTING_LINKS(IAP))
      WRITE(STRING(N+4:N+7), '(I4)') SDSN(ROUNDABOUT(IRND)%DEPARTING_LINKS(IAP))
      N = N + 8
    ENDDO
    WRITE(100, '(A)') STRING
  ENDDO
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT98
  USE ROUNDABOUT_DATA
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: IRND, IAP, NAP
! ----------------------------------------------------------------------
  STRING(79:80) = '98'
  DO IRND = 1, NUMBER_OF_ROUNDABOUTS
    NAP = MIN(ROUNDABOUT(IRND)%APPROACHES, 4)
    STRING(1:78) = ''
    WRITE(STRING(1:4), '(I4)') IRND
    DO IAP = 1, NAP
      WRITE(STRING(5:8), '(I4)') IAP
      WRITE(STRING(9:12), '(I4)') INT(ROUNDABOUT(IRND)%EXIT_PCTS(IAP, 1) * 100)
      WRITE(STRING(13:16), '(I4)') INT(ROUNDABOUT(IRND)%EXIT_PCTS(IAP, 2) * 100)
      WRITE(STRING(17:20), '(I4)') INT(ROUNDABOUT(IRND)%EXIT_PCTS(IAP, 3) * 100)
      WRITE(STRING(21:24), '(I4)') INT(ROUNDABOUT(IRND)%EXIT_PCTS(IAP, 4) * 100)
      WRITE(100, '(A)') STRING
    ENDDO
  ENDDO
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT154
  USE ACTUATED_CONTROLLERS
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: IACT, J
! ----------------------------------------------------------------------
  STRING(78:80) = '154'
  DO IACT = 1, NUMBER_OF_ACS + NUMBER_OF_SUPER_ACS
    IF(AC_SIGNALS(IACT)%NODE(2) .NE. 0) THEN
      WRITE(STRING(1:32), '(8I4)') (AC_SIGNALS(IACT)%NODE(J), J = 1, 8)
      WRITE(100, '(A)') STRING
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE WRITE_RT170(INET) 
  USE FREEWAY_LINKS
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: INET
  CHARACTER*80 STRING
! ----------------------------------------------------------------------
  STRING(1:77) = ''
  STRING(78:80) = '170'
  IF(INET .EQ. I_STREET) THEN
    IF(N_FREEWAY_LINKS .GT. 0) THEN
      STRING(4:4) = '8'
    ELSE
      STRING(4:4) = '0'
    ENDIF
  ELSE
    STRING(4:4) = '0'
  ENDIF
  WRITE(100, '(A)') STRING
  RETURN
  END
  
  SUBROUTINE WRITE_RT185
  USE BUS_STATION_DATA
  USE STREET_LINKS
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: NS
! ----------------------------------------------------------------------
  STRING(78:80) = '185'
  IF(ALLOCATED(BUS_STATION_LIST)) THEN
    DO NS = 1, NUMBER_OF_BUSSTATIONS
      IF(NS .GT. 99) EXIT
      IF(BUS_STATION_LIST(NS)%LINK .NE. 0) THEN
        STRING(1:77) = ''
        WRITE(STRING(1:2), '(I2)') NS
        WRITE(STRING(4:4), '(I1)') BUS_STATION_LIST(NS)%BLOCK_CODE
        WRITE(STRING(5:8), '(I4)') SUSN(BUS_STATION_LIST(NS)%LINK)
        WRITE(STRING(9:12), '(I4)') SDSN(BUS_STATION_LIST(NS)%LINK)
        WRITE(STRING(13:16), '(I4)') SLENGTH(BUS_STATION_LIST(NS)%LINK) - BUS_STATION_LIST(NS)%LOCATION
        WRITE(STRING(18:18), '(I1)') BUS_STATION_LIST(NS)%CAPACITY
        WRITE(STRING(20:20), '(I1)') BUS_STATION_LIST(NS)%TYPE_CODE
        WRITE(100, '(A)') STRING
      ENDIF
    ENDDO
  ENDIF
  RETURN
  END
  
  SUBROUTINE WRITE_RT186(TIMEPERIOD)
  USE BUS_STATION_DATA
  USE STREET_LINKS
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: TIMEPERIOD
  CHARACTER*80 STRING
  INTEGER :: NS
  LOGICAL :: WRITE_IT
! ----------------------------------------------------------------------
  STRING(78:80) = '186'
  IF(ALLOCATED(BUS_STATION_LIST)) THEN
    DO NS = 1, NUMBER_OF_BUSSTATIONS
      IF(NS .GT. 99) EXIT
      IF(BUS_STATION_LIST(NS)%LINK .NE. 0) THEN
        WRITE_IT = .FALSE.
        IF(TIMEPERIOD .EQ. 1) THEN
          WRITE_IT = .TRUE.
        ELSE
          IF(WRITE186(NS, TIMEPERIOD)) WRITE_IT = .TRUE.
        ENDIF
        IF(WRITE_IT) THEN
          STRING(1:77) = ''
          WRITE(STRING(3:4), '(I2)') NS
          WRITE(STRING(6:8), '(I3)') INT(STATION_DWELL(NS, TIMEPERIOD))
          WRITE(STRING(9:12), '(I4)') INT(STATION_BYPASS_PCT(NS, TIMEPERIOD) * 100)
          WRITE(100, '(A)') STRING
        ENDIF
      ENDIF
    ENDDO
  ENDIF
  RETURN
  END
  
  SUBROUTINE WRITE_RT187
  USE BUS_ROUTE_DATA
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: NR, INODE, N
! ----------------------------------------------------------------------
  STRING(78:80) = '187'
  DO NR = 1, NUMBER_OF_ROUTES
    IF(BUSR_NNODES(NR) .GT. 0) THEN
      STRING(1:77) = ''
      WRITE(STRING(2:4), '(I3)') NR
      N = 5
      DO INODE = 1, BUSR_NNODES(NR)
        WRITE(STRING(N:N+3), '(I4)') BUSR_ROUTE_NODES(NR, INODE)
        N = N + 4
      ENDDO
      WRITE(100, '(A)') STRING
    ENDIF
  ENDDO
  RETURN
  END

  SUBROUTINE WRITE_RT188
  USE BUS_ROUTE_DATA
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: NR, ISTAT, N
! ----------------------------------------------------------------------
  STRING(78:80) = '188'
  DO NR = 1, NUMBER_OF_ROUTES
    IF(BUSR_NNODES(NR) .GT. 0) THEN
      STRING(1:77) = ''
      WRITE(STRING(2:4), '(I3)') NR
      N = 9
      DO ISTAT = 1, 34
        IF(BUSR_STATIONLIST(NR, ISTAT) .EQ. 0) EXIT
        WRITE(STRING(N:N+1), '(I2)') BUSR_STATIONLIST(NR, ISTAT)
        N = N + 2
      ENDDO
      IF(N .GT. 9) WRITE(100, '(A)') STRING
    ENDIF
  ENDDO
  RETURN
  END
  
  SUBROUTINE WRITE_RT189(TIMEPERIOD)
  USE BUS_ROUTE_DATA
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: TIMEPERIOD
  CHARACTER*80 STRING
  INTEGER :: NR
  LOGICAL :: WRITE_IT
! ----------------------------------------------------------------------
  STRING(78:80) = '189'
  DO NR = 1, NUMBER_OF_ROUTES
    IF(BUSR_NNODES(NR) .GT. 0) THEN
      WRITE_IT = .FALSE.
      IF(TIMEPERIOD .EQ. 1) THEN
        WRITE_IT = .TRUE.
      ELSE
        IF(WRITE189(NR, TIMEPERIOD)) WRITE_IT = .TRUE.
      ENDIF
      IF(WRITE_IT) THEN
        STRING(1:77) = ''
        WRITE(STRING(2:4), '(I3)') NR
        WRITE(STRING(5:8), '(I4)') ROUTE_HEADWAY(NR, TIMEPERIOD)
        WRITE(STRING(9:12), '(I4)') ROUTE_OFFSET(NR, TIMEPERIOD)
        WRITE(100, '(A)') STRING
      ENDIF
    ENDIF
  ENDDO
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT195
  USE NODE_TABLE
  USE GLOBAL_DATA
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: I
! ----------------------------------------------------------------------
  STRING(78:80) = '195'
  DO I = 1, MAX_NODE_NUMBER
    IF(IS_USED(I)) THEN
      STRING(1:77) = ''
      WRITE(STRING(1:4), '(I4)') I
      WRITE(STRING(6:12), '(I7)') X195(I)
      WRITE(STRING(15:20), '(I6)') Y195(I)
      WRITE(100, '(A)') STRING
    ENDIF
  ENDDO
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT196
  USE FREEWAY_LINKS
  USE STREET_LINKS
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: IL
! ----------------------------------------------------------------------
  STRING(78:80) = '196'
  DO IL = 1, N_FREEWAY_LINKS
    WRITE(STRING(1:4), '(I4)') FUSN(IL)
    WRITE(STRING(5:8), '(I4)') FDSN(IL)
    WRITE(100, '(A)') STRING
  ENDDO
  DO IL = 1, N_STREET_LINKS
    WRITE(STRING(1:4), '(I4)') SUSN(IL)
    WRITE(STRING(5:8), '(I4)') SDSN(IL)
    WRITE(100, '(A)') STRING
  ENDDO
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT197
  USE ROUNDABOUT_DATA
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: IRND
! ----------------------------------------------------------------------
  STRING(78:80) = '197'
  DO IRND = 1, NUMBER_OF_ROUNDABOUTS
    WRITE(STRING(1:4), '(I4)') IRND
    WRITE(STRING(5:8), '(I4)') ROUNDABOUT(IRND)%RADIUS
    WRITE(100, '(A)') STRING
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE WRITE_RT201
  USE VEHICLE_TYPES
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: I
! ----------------------------------------------------------------------
  STRING(78:80) = '201'
  DO I = 1, NTYPES
    STRING(1:77) = ''
    WRITE(STRING(1:4), '(I4)') I
    WRITE(STRING(5:8), '(I4)') INT(PCT_PITT(I) * 100)
    WRITE(STRING(9:12), '(I4)') INT(PCT_IDM(I) * 100)
    WRITE(STRING(13:16), '(I4)') INT(PCT_ACC(I) * 100)
    WRITE(STRING(17:20), '(I4)') INT(PCT_CACC(I) * 100)
    WRITE(100, '(A)') STRING
  ENDDO
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT202
  USE VEHICLE_TYPES
  USE CAR_FOLLOWING
  IMPLICIT NONE
  CHARACTER*80 STRING
  INTEGER :: I
! ----------------------------------------------------------------------
  STRING(1:77) = ''
  STRING(78:80) = '202'
  WRITE(STRING(1:4), '(I4)') 1
  DO I = 1, NTYPES
    WRITE(STRING(5:8), '(I4)') I
    WRITE(STRING(9:12), '(I4)') INT(ACC_TG * 100)
    WRITE(STRING(13:16), '(I4)') INT(ACC_AMAX * 100)
    WRITE(STRING(17:20), '(I4)') INT(ACC_DMAX * 100)
    WRITE(STRING(21:24), '(I4)') INT(ACC_K1 * 100)
    WRITE(STRING(25:28), '(I4)') INT(ACC_K2 * 100)
    WRITE(100, '(A)') STRING
  ENDDO
  WRITE(STRING(1:4), '(I4)') 2
  DO I = 1, NTYPES
    WRITE(STRING(5:8), '(I4)') I
    WRITE(STRING(9:12), '(I4)') INT(CACC_TG * 100)
    WRITE(STRING(13:16), '(I4)') INT(CACC_AMAX * 100)
    WRITE(STRING(17:20), '(I4)') INT(CACC_DMAX * 100)
    WRITE(STRING(21:24), '(I4)') INT(CACC_K1 * 100)
    WRITE(STRING(25:28), '(I4)') INT(CACC_K2 * 100)
    WRITE(100, '(A)') STRING
  ENDDO
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE WRITE_RT210(I1, I2) 
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: I1, I2
  CHARACTER*80 STRING
! ----------------------------------------------------------------------
  STRING(1:77) = ''
  STRING(78:80) = '210'
  WRITE(STRING(4:4), '(I1)') I1
  WRITE(STRING(8:8), '(I1)') I2
  WRITE(100, '(A)') STRING
  RETURN
  END
