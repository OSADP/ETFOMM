  MODULE DCS_DATA
    !INTEGER :: LAG
    INTEGER :: PHASELANES(8)
    INTEGER :: STOPBAR_DETECTORS(8)
    INTEGER :: ZONE_DETECTORS(8, 4)
    INTEGER :: DETECTOR_STATES
    INTEGER :: TRAPMAP(17:32)
    REAL    :: MINHDWY
    REAL    :: LOOPDIST(8, 4)
    REAL    :: MAXSPEED
    REAL    :: MINSPEED
    REAL    :: FIRSTSTARTOFGREEN
    REAL    :: MAXTRUCKLENGTH
    REAL    :: CARLENGTH
    REAL    :: QUESPEED
    REAL    :: WEIGHTDELAY
    REAL    :: WEIGHTLANE
    REAL    :: MAXPCINDZ
    REAL    :: LOOKAHEADTIME(8, 4)
    LOGICAL :: PHASEENDFLAG(8)
    REAL    :: VSLLAST(8, 4)
    REAL    :: TSLLAST(8, 4)
    REAL    :: STARTOFGREEN(8)
    REAL    :: FGRNATMAX(8)
    !REAL    :: ALG_CORE_VER
    INTEGER :: ICYCLE
    INTEGER :: NUMBERCALLS(8)     !Number of conflicting calls
    REAL    :: TIMETOMAXOUT(8)
    REAL    :: LOOPSIZE
    REAL    :: LOOPSPACE(32)
    REAL    :: MEGW(8)
    REAL    :: EGWPLUSDELAY(8)
    INTEGER :: LVDZ(8, 4, 0:600)    !(phase, lane, 600 time intervals = 5min*60s/min/(deltat s/int))
    REAL    :: EGW(8, 0:600)        !(phase, deltat-s intervals)same dimension as lvdz
    REAL    :: MAXPCEINDZ(8, 0:600) !(time intervals)
    REAL    :: FORCEFLAG(8)
    LOGICAL :: PHASEGRN(8)
    LOGICAL :: LAGGING1
    LOGICAL :: LAGGING5
    LOGICAL :: MAXOUT(8)
    REAL    :: MINGREEN(8)
    REAL    :: MAXGREEN(8)
    REAL    :: DELTAT = 0.5
    
  END MODULE
  
! ==================================================================================================
  SUBROUTINE EXECUTE_DCS
!----------------------------------------------------------------------
!-- Implementation of D-CS using NTCIP functions 
!----------------------------------------------------------------------
  USE NTCIP_DATA
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE STREET_DETECTORS
  USE STREET_LINKS
  USE TEXT
  USE DCS_DATA
  IMPLICIT NONE
  LOGICAL :: FIRST = .TRUE.
  INTEGER :: IACT, MAINLINKS(2)
  INTEGER :: IL, ILINK, ILANE, ICODE
  INTEGER :: J, K, MAXLOOPDIST
  INTEGER :: IDET, DETMAP(16), STEP_COUNTER = 0
  INCLUDE 'IOFILES.INC'
!----------------------------------------------------------------------
  IACT = 1
  STEP_COUNTER = STEP_COUNTER + 1
  IF(FIRST) THEN
    FGRNATMAX = 0.1  !!!Not defined in original DCS code
#ifdef _TSIS_COMPATIBLE
    OPEN(1, FILE = LINFNAME(1:IROOT-1)//'_MAP.DAT', STATUS='OLD', ERR=10, IOMSG=ETEXT)
    READ(1,*, END=20, ERR=20) DETMAP
    !Detectors for the DCS zones
    ZONE_DETECTORS(2, 1) = DETMAP(1)
    ZONE_DETECTORS(2, 2) = DETMAP(2)
    ZONE_DETECTORS(2, 3) = DETMAP(3)
    ZONE_DETECTORS(2, 4) = DETMAP(4)
    ZONE_DETECTORS(6, 1) = DETMAP(5)
    ZONE_DETECTORS(6, 2) = DETMAP(6)
    ZONE_DETECTORS(6, 3) = DETMAP(7)
    ZONE_DETECTORS(6, 4) = DETMAP(8)

    !Stopbar detectors, 1 for each phase, spanning all lanes
    STOPBAR_DETECTORS(1) = DETMAP(9)
    STOPBAR_DETECTORS(2) = DETMAP(10)
    STOPBAR_DETECTORS(3) = DETMAP(11)
    STOPBAR_DETECTORS(4) = DETMAP(12)
    STOPBAR_DETECTORS(5) = DETMAP(13)
    STOPBAR_DETECTORS(6) = DETMAP(14)
    STOPBAR_DETECTORS(7) = DETMAP(15)
    STOPBAR_DETECTORS(8) = DETMAP(16)
#endif

    !Define approach links for main street green phases
    PHASELANES = 0
    DO ILINK = 1, AC_SIGNALS(IACT)%N_DIRECT_APPROACHES
      IL = AC_SIGNALS(IACT)%DIRECT_APPROACH(ILINK)
      IF(AC_SIGNALS(IACT)%PHASE(2)%THRUARROW(ILINK)) THEN
        MAINLINKS(1) = IL
        DO ILANE = FIRST_FULL_LANE(IL), LAST_FULL_LANE(IL)
          ICODE = CHANNELIZATION(IL, ILANE)
          IF(ICODE .EQ. 0 .OR. ICODE .GE. 7) THEN
            PHASELANES(2) = PHASELANES(2) + 1
          ENDIF
        ENDDO
      ENDIF
      IF(AC_SIGNALS(IACT)%PHASE(6)%THRUARROW(ILINK)) THEN
        MAINLINKS(2) = IL
        DO ILANE = FIRST_FULL_LANE(IL), LAST_FULL_LANE(IL)
          ICODE = CHANNELIZATION(IL, ILANE)
          IF(ICODE .EQ. 0 .OR. ICODE .GE. 7) THEN
            PHASELANES(6) = PHASELANES(6) + 1
          ENDIF
        ENDDO
      ENDIF
    ENDDO
    
    FIRST = .FALSE.
    MINGREEN = AC_SIGNALS(IACT)%GUI_MIN_GREEN_TIMES
    MAXGREEN = AC_SIGNALS(IACT)%GUI_MAX_GREEN_TIMES
    
    !!!Constants from DCS default settings
    MAXTRUCKLENGTH = 28
    MAXSPEED = 70
    MINSPEED = 20
    CARLENGTH = 18
    WEIGHTLANE = 1.2
    WEIGHTDELAY = 0.1
    MAXPCINDZ = 1.4

    DO J = 2, 6, 4
      DO K = 1, 4
        IDET = ZONE_DETECTORS(J, K)
        IF(IDET .EQ. 0) CYCLE
        IL = SDETECTOR(IDET)%LINK
        LOOPDIST(J, K) = SLENGTH(IL) - SDETECTOR(IDET)%LOCATION
      ENDDO
    ENDDO

    CALL SETCYCLE       !compute lookaheadtime and cycle
    
    DO J = 2, 6, 4     !TIME INTO FUTURE FOR WHICH EGW IS COMPUTED, S
      DO K = 1, PHASELANES(J)
        LOOKAHEADTIME(J, K) = (LOOPDIST(J, K) - MAXTRUCKLENGTH) / (MAXSPEED * 1.47) - 500. / 1000. - DZ_ENTRY_TIME
        IF(LOOKAHEADTIME(J, K) .LT. 0) THEN
          WRITE(MSGTEXT, '(A,I1,A,I1,A)') 'lookahead for phase ', j, ' lane ', k, ' is less than 0'
          CALL SENDTEXTMSG(M_ERROR)
          ERROR_FLAG = 1
          RETURN
        ENDIF
      ENDDO
      PHASEGRN(J) = .FALSE.
      FORCEFLAG(J) = 0
    ENDDO

    MAXLOOPDIST = 0
    DO J = 2, 6, 4
      DO K = 1, PHASELANES(J)
        IF(LOOPDIST(J, K) .GT. 0 .AND. LOOPDIST(J, K) .GT. MAXLOOPDIST) THEN
         MAXLOOPDIST = LOOPDIST(J, K)
        ENDIF
      ENDDO
    ENDDO
    ICYCLE = INT(MAXLOOPDIST / (MINSPEED * 1.47) / TIMESTEP + 0.499) + 1
    IF(ICYCLE .GT. 600) ICYCLE = 600
  
  ENDIF
  !
  !write(msgtext, '(a, i)') '  green phases = ', green_phases(IACT)
  !call sendtextmsg(m_info)
  !write(msgtext, '(a, i)') '  yellow phases = ', yellow_phases(IACT)
  !call sendtextmsg(m_info)
  !
  !if(btest(green_phases(iact), 0)) then
  !  write(msgtext, '(a)') '  Phase 1 is green'
  !  call sendtextmsg(m_info)
  !  ac_signals(1)%current_phases(1) = 1
  !endif
  !if(btest(green_phases(iact), 1)) then
  !  write(msgtext, '(a)') '  Phase 2 is green'
  !  call sendtextmsg(m_info)
  !  ac_signals(1)%current_phases(1) = 2
  !endif
  !if(btest(green_phases(iact), 2)) then
  !  write(msgtext, '(a)') '  Phase 3 is green'
  !  call sendtextmsg(m_info)
  !  ac_signals(1)%current_phases(1) = 3
  !endif
  !if(btest(green_phases(iact), 3)) then
  !  write(msgtext, '(a)') '  Phase 4 is green'
  !  call sendtextmsg(m_info)
  !  ac_signals(1)%current_phases(1) = 4
  !endif
  !if(btest(green_phases(iact), 4)) then
  !  write(msgtext, '(a)') '  Phase 5 is green'
  !  call sendtextmsg(m_info)
  !  ac_signals(1)%current_phases(2) = 5
  !endif
  !if(btest(green_phases(iact), 5)) then
  !  write(msgtext, '(a)') '  Phase 6 is green'
  !  call sendtextmsg(m_info)
  !  ac_signals(1)%current_phases(2) = 6
  !endif
  !if(btest(green_phases(iact), 6)) then
  !  write(msgtext, '(a)') '  Phase 7 is green'
  !  call sendtextmsg(m_info)
  !  ac_signals(1)%current_phases(2) = 7
  !endif
  !if(btest(green_phases(iact), 7)) then
  !  write(msgtext, '(a)') '  Phase 8 is green'
  !  call sendtextmsg(m_info)
  !  ac_signals(1)%current_phases(2) = 8
  !endif  
  !
  !if(btest(yellow_phases(iact), 0)) then
  !  write(msgtext, '(a)') '  Phase 1 is yellow'
  !  call sendtextmsg(m_info)
  !  ac_signals(1)%current_phases(1) = 1
  !endif
  !if(btest(yellow_phases(iact), 1)) then
  !  write(msgtext, '(a)') '  Phase 2 is yellow'
  !  call sendtextmsg(m_info)
  !  ac_signals(1)%current_phases(1) = 2
  !endif
  !if(btest(yellow_phases(iact), 2)) then
  !  write(msgtext, '(a)') '  Phase 3 is yellow'
  !  call sendtextmsg(m_info)
  !  ac_signals(1)%current_phases(1) = 3
  !endif
  !if(btest(yellow_phases(iact), 3)) then
  !  write(msgtext, '(a)') '  Phase 4 is yellow'
  !  call sendtextmsg(m_info)
  !  ac_signals(1)%current_phases(1) = 4
  !endif
  !if(btest(yellow_phases(iact), 4)) then
  !  write(msgtext, '(a)') '  Phase 5 is yellow'
  !  call sendtextmsg(m_info)
  !  ac_signals(1)%current_phases(2) = 5
  !endif
  !if(btest(yellow_phases(iact), 5)) then
  !  write(msgtext, '(a)') '  Phase 6 is yellow'
  !  call sendtextmsg(m_info)
  !  ac_signals(1)%current_phases(2) = 6
  !endif
  !if(btest(yellow_phases(iact), 6)) then
  !  write(msgtext, '(a)') '  Phase 7 is yellow'
  !  call sendtextmsg(m_info)
  !  ac_signals(1)%current_phases(2) = 7
  !endif
  !if(btest(yellow_phases(iact), 7)) then
  !  write(msgtext, '(a)') '  Phase 8 is yellow'
  !  call sendtextmsg(m_info)
  !  ac_signals(1)%current_phases(2) = 8
  !endif
   
  !Call the Vehicle Status Module Every 100 milliseconds.
  CALL VEHICLE_STATUS
  IF(MOD(STEP_COUNTER, 5) .EQ. 0) THEN
    !Call the Phase Status Module Every 500 milliseconds.
    CALL PHASE_STATUS
  ENDIF
  RETURN
10 CONTINUE  
  WRITE(MSGTEXT,'(A)') 'FILE OPEN ERROR: EXECUTE_DCS'
  CALL SENDTEXTMSG(M_INFO)
  WRITE(MSGTEXT, '(A)') ETEXT
  CALL SENDTEXTMSG(M_ERROR)
  ERROR_FLAG = 1
  RETURN
20 CONTINUE  
  WRITE(MSGTEXT,'(A)') 'FILE READ ERROR: EXECUTE_DCS'
  CALL SENDTEXTMSG(M_INFO)
  WRITE(MSGTEXT, '(A)') ETEXT
  CALL SENDTEXTMSG(M_ERROR)
  ERROR_FLAG = 1
  RETURN
  END

  SUBROUTINE VEHICLE_STATUS()         ! called every 100 ms
  USE DCS_DATA
  USE NTCIP_DATA
  USE SIMPARAMS
  USE TEXT
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER :: I, K, RING, PHASE, ICOLOR
!----------------------------------------------------------------------
  !ALG_CORE_VER = 5.1

  !write(msgtext, '(a, f8.1)') ' VEHICLE_STATUS @TIME = ', simtime
  !call sendtextmsg(m_info)
  
  DO PHASE = 2, 6, 4
    IF(PHASE .EQ. 2) THEN
      RING = 1
    ELSE
      RING = 2
    ENDIF
  
    !things to do when a phase is green
    IF(BTEST(GREEN_PHASES(1), PHASE-1)) THEN
      ICOLOR = GREEN
    ELSEIF(BTEST(YELLOW_PHASES(1), PHASE-1)) THEN
      ICOLOR = YELLOW
    ELSE
      ICOLOR = RED
    ENDIF
    
    IF(ICOLOR .EQ. GREEN) THEN 
      PHASEGRN(PHASE) = .TRUE.                !phase is green
    
      IF(STARTOFGREEN(PHASE) .EQ. 0) THEN   !things to do at start of green
        STARTOFGREEN(PHASE) = SIMTIME       !true time at start of green in seconds        
        CALL SET_EXTENSION(PHASE)           !put hold on phase green 
        CALL CLEAR_FORCEOFF(PHASE)          !Reset Force off for each ring
        TIMETOMAXOUT(PHASE) = 99999         !reset maxtimer at start of green (see phaseStatus)
        !If limiting the phase to max green determine if the phase needs to be forced off
        IF(LIMIT_TO_MAXGREEN) THEN
          TIMETOMAXOUT(PHASE) = MAXGREEN(PHASE)
        ENDIF
        
        IF(PHASE .EQ. 2) THEN
          K = 5
        ELSE
          K = 1
        ENDIF
        TIMETOMAXOUT(K) = 99999         !reset maxtimer for left-turn phase in opposite ring
        PHASEENDFLAG(PHASE) = .FALSE.

      ENDIF
      
      IF(.NOT. PHASEENDFLAG(PHASE) .AND. SBB(RING) .GT. 0) THEN
        PHASEENDFLAG(PHASE) = .TRUE. !gap-out
      ENDIF
    
    ELSE                                !phase is red
      PHASEGRN(PHASE) = .FALSE.
      IF(STARTOFGREEN(PHASE) .GT. 0) THEN   !things to do at the start of red
        CALL CLEAR_FORCEOFF(PHASE)          !Reset Force off for each ring
        STARTOFGREEN(PHASE) = 0
        DO K = 1, 4
          DO I = 0, 600
             LVDZ(PHASE, K, I) = 0          !set lvdz matrix to zero
          ENDDO
          VSLLAST(PHASE, K) = 0             !set stopline time and speed variables to zero
          TSLLAST(PHASE, K) = 0
        ENDDO
      ENDIF
    ENDIF                              !end of green check
  ENDDO
  RETURN              
  END

  SUBROUTINE PHASE_STATUS()
  USE SIMPARAMS
  USE DCS_DATA
  USE NTCIP_DATA
  USE TEXT
  USE STREET_DETECTORS
  IMPLICIT NONE
  INTEGER :: I, K, IMOD, STARTLOOP, ENDLOOP
  REAL :: RELTIMENOW
  INTEGER :: TIMETOEND
  LOGICAL :: FORCE(8), FORCEBOTH(8), FORCEEND
  CHARACTER :: DECISION*30
  INTEGER :: PHASE, RING
  REAL :: MEGWPLUSDELAY, PCE, EGWDELAY(8)
  INTEGER :: GETENDLOOP, GET_DETECTOR_STATUS
!----------------------------------------------------------------------
  !write(msgtext, '(a, f8.1)') ' PHASE_STATUS @TIME = ', simtime
  !call sendtextmsg(m_info)

  CALL CLASSIFIERSTATUS
  
  !clear equivalent green weight matrix each call
  EGW = 0.0                      
  NUMBERCALLS = 0
  MAXPCEINDZ = 0.0
  MEGW(2) = 0.1
  MEGW(6) = 0.1

  !Get Main Street Phase Status MSPS and, if green, calculate End Green Weight

  RELTIMENOW = SIMTIME - FIRSTSTARTOFGREEN
  STARTLOOP = RELTIMENOW / DELTAT + 0.499
  STARTLOOP = MAX(STARTLOOP, 1)

  !Evaluate each ring and reset any force that remains on after 2 seconds
  DO PHASE = 2, 6, 4             
    IF(PHASE .EQ. 2) THEN
      RING = 1
    ELSE
      RING = 2
    ENDIF
    !???INTERVAL = FRMMAIN.TIMER2.INTERVAL / 1000
    IF(FORCEFLAG(PHASE) .GT. 0 .AND. SIMTIME .GT. FORCEFLAG(PHASE) + 2.0) THEN 
      IF(PHASEGRN(PHASE)) CALL SET_EXTENSION(PHASE)                !put hold on phase green
      CALL CLEAR_FORCEOFF(PHASE)                                   !Reset Force off
      FORCEFLAG(PHASE) = 0
    ENDIF
  ENDDO
  
  IF(STARTLOOP .LT. 0 .OR. FIRSTSTARTOFGREEN .EQ. 0) THEN !things to do when 2 and 6 are not green
    RETURN
  ENDIF

  IF(PHASEGRN(2) .AND. .NOT. PHASEGRN(6)) THEN
    PHASEGRN(5) = .TRUE.
  ELSE
    PHASEGRN(5) = .FALSE.
  ENDIF
  IF(PHASEGRN(6) .AND. .NOT. PHASEGRN(2)) THEN
    PHASEGRN(1) = .TRUE.
  ELSE
    PHASEGRN(1) = .FALSE.
  ENDIF

  DO PHASE = 2, 6, 4
    IF(PHASE .EQ. 2) THEN
      K = 5
    ELSE
      K = 1
    ENDIF !phase of left-turn on opposite ring as PHASE
    IF(PHASEGRN(PHASE) .AND. PHASEGRN(K)) THEN  !2+5 grn or 1+6 grn
      DO I = 1, 8        !check status of other phases via their stopline detectors
        IF(I .NE. 1 .AND. I .NE. 5 .AND. STOPBAR_DETECTORS(I) .GT. 0) THEN  ! i = _,2,3,4,  _,6,7,8
          IF((K .EQ. 1 .AND. I .NE. 5 .AND. I .NE. 6) .OR. (K .EQ. 5 .AND. I .NE. 1 .AND. I .NE. 2)) THEN
            NUMBERCALLS(K) = NUMBERCALLS(K) + GET_DETECTOR_STATUS(STOPBAR_DETECTORS(I))
          ENDIF
        ENDIF
      ENDDO

      IF(NUMBERCALLS(K) .GT. 0 .AND. TIMETOMAXOUT(K) .EQ. 99999) THEN
        TIMETOMAXOUT(K) = RELTIMENOW + 40
      ENDIF

      IF(NUMBERCALLS(K) .EQ. 0) TIMETOMAXOUT(K) = 99999 !reset max timer

    ENDIF
  ENDDO

  DO PHASE = 2, 6, 4             !Evaluate each main street phase separately
    ENDLOOP = GETENDLOOP(PHASE, PHASE, STARTLOOP)

    IF(PHASEGRN(PHASE)) THEN     !phase is green
      IF(LOGGING) CALL DPRINT_JAB2(RELTIMENOW, PHASE, STARTLOOP, ENDLOOP)
 
      DO I = 1, 8        !check status of other phases via their stopline detectors
        IF(I .NE. 2 .AND. I .NE. 6 .AND. STOPBAR_DETECTORS(I) .GT. 0) THEN !sum number of phases with waiting vehicles
          IF((PHASE .EQ. 2 .AND. I .NE. 5 .AND. I .NE. 6) .OR. (PHASE .EQ. 6 .AND. I .NE. 1 .AND. I .NE. 2)) THEN
            NUMBERCALLS(PHASE) = NUMBERCALLS(PHASE) + GET_DETECTOR_STATUS(STOPBAR_DETECTORS(I))
          ENDIF
        ENDIF
      ENDDO
      
      IF(NUMBERCALLS(PHASE) .EQ. 0) THEN        !put hold back on and drop force off if no conflicting calls due to rtor, failsafe
        CALL CLEAR_FORCEOFF(PHASE)              !Reset (drop) Force off for each ring, failsafe
        CALL SET_EXTENSION(PHASE)               !put hold on phase green, failsafe 
      ENDIF
    
      IF(NUMBERCALLS(PHASE) .GT. 0 .AND. TIMETOMAXOUT(PHASE) .EQ. 99999) THEN
        TIMETOMAXOUT(PHASE) = RELTIMENOW + MAXGREEN(PHASE)
      ENDIF
      
      IF(.NOT. LIMIT_TO_MAXGREEN) THEN
        IF(NUMBERCALLS(PHASE) .EQ. 0) TIMETOMAXOUT(PHASE) = 99999 !reset max timer
      ENDIF
      
      IF(RELTIMENOW .GT. TIMETOMAXOUT(PHASE) - FGRNATMAX(PHASE) * MAXGREEN(PHASE)) THEN
        MEGW(PHASE) = MAXPCINDZ           !allow some veh to be in dz late in phase
      ENDIF
      
      IF(RELTIMENOW .LT. TIMETOMAXOUT(PHASE)) THEN
      
        DO I = STARTLOOP, ENDLOOP
          IMOD = MOD(I, ICYCLE)
          DO K = 1, PHASELANES(PHASE)                 !sum over all lanes
            PCE = LVDZ(PHASE, K, IMOD) / CARLENGTH
            EGW(PHASE, IMOD) = EGW(PHASE, IMOD) + PCE ** WEIGHTLANE
            IF(PCE .GT. MAXPCEINDZ(PHASE, IMOD)) THEN
              MAXPCEINDZ(PHASE, IMOD) = PCE !max PCE's in any one dz at time t
            ENDIF
          ENDDO
        ENDDO
        IF(LOGGING) CALL DPRINT_JAB3(0, STARTLOOP, ENDLOOP, PHASELANES(PHASE), PHASE)
       
      ELSE                    !phase has reached its maximum limit
        IMOD = MOD(STARTLOOP, ICYCLE)
        EGW(PHASE, IMOD) = -999 !a number sufficiently small as to ensure the phase will end immediately
      ENDIF                 !end of maxout check
    ENDIF                   !end of green check
  ENDDO                     !end of phase 2 & 6 loop

  !  Determine the best time to end phases 2 and 6

  IF(PHASEGRN(2) .OR. PHASEGRN(6)) THEN           !one or both phases are green
    FORCEBOTH(2) = .FALSE.                          !reset force-off flags
    FORCEBOTH(6) = .FALSE.
    FORCE(2) = .FALSE.
    FORCE(6) = .FALSE.
  
   ! whole approach
  
    IF(PHASEGRN(2) .AND. PHASEGRN(6)) THEN     !both phases are green
      ENDLOOP = GETENDLOOP(2, 6, STARTLOOP)
  
      MEGWPLUSDELAY = 999999                  !minimum end green weight plus delay
      TIMETOEND = ENDLOOP + 1                 !wait if no time is acceptable
      DO I = STARTLOOP, ENDLOOP   !find lowest megw from now to 10s in future
        IMOD = MOD(I, ICYCLE)
        EGWDELAY(2) = EGW(2, IMOD) + ((I - STARTLOOP) * NUMBERCALLS(2) * DELTAT) * WEIGHTDELAY
        EGWDELAY(6) = EGW(6, IMOD) + ((I - STARTLOOP) * NUMBERCALLS(6) * DELTAT) * WEIGHTDELAY
        IF(MAXPCEINDZ(2, IMOD) .LT. MEGW(2) .AND. MAXPCEINDZ(6, IMOD) .LT. MEGW(6)) THEN
          IF(EGWDELAY(2) + EGWDELAY(6) .LT. MEGWPLUSDELAY) THEN
            MEGWPLUSDELAY = EGWDELAY(2) + EGWDELAY(6)
            TIMETOEND = I
          ENDIF
        ENDIF
      ENDDO
  
      IF(LOGGING) CALL DPRINT_JAB4(0, STARTLOOP, ENDLOOP, 0., MEGWPLUSDELAY, TIMETOEND)

      IMOD = MOD(STARTLOOP, ICYCLE)

      DO PHASE = 2, 6, 4            !deal with each phase separately
        FORCEEND = .FALSE.
        MAXOUT(PHASE) = .FALSE.
        IF(TIMETOEND .EQ. STARTLOOP .AND. NUMBERCALLS(PHASE) .GT. 0) THEN
          FORCEEND = .TRUE.
        ENDIF
        IF(EGW(PHASE, IMOD) .EQ. -999) THEN
          MAXOUT(PHASE) = .TRUE.
        ENDIF
        IF((PHASEENDFLAG(PHASE) .AND. FORCEEND) .OR. MAXOUT(PHASE)) THEN
          FORCEBOTH(PHASE) = .TRUE.
        ENDIF
      ENDDO
    ENDIF                            ! both phases are green
  
  !  single phase code

    DO PHASE = 2, 6, 4
      IF(PHASEGRN(PHASE)) THEN     !phase PHASE is green
        ENDLOOP = GETENDLOOP(PHASE, PHASE, STARTLOOP)
      
        MEGWPLUSDELAY = 999999                  !minimum end green weight plus delay
        TIMETOEND = ENDLOOP + 1                 !wait if no time is acceptable
        DO I = STARTLOOP, ENDLOOP   !find lowest megw from now to 10s in future
          IMOD = MOD(I, ICYCLE)
          EGWDELAY(PHASE) = EGW(PHASE, IMOD) + ((I - STARTLOOP) * NUMBERCALLS(PHASE) * DELTAT) * WEIGHTDELAY
          IF(I .EQ. STARTLOOP) EGWPLUSDELAY(PHASE) = EGWDELAY(PHASE)  ! output to Panel for logging
          IF(MAXPCEINDZ(PHASE, IMOD) .LT. MEGW(PHASE)) THEN
            IF(EGWDELAY(PHASE) .LT. MEGWPLUSDELAY) THEN
              MEGWPLUSDELAY = EGWDELAY(PHASE)
              TIMETOEND = I
            ENDIF
          ENDIF
        ENDDO
    
        IF(LOGGING) CALL DPRINT_JAB4(PHASE, STARTLOOP, ENDLOOP, MEGW(PHASE), MEGWPLUSDELAY, TIMETOEND)
    
        IMOD = MOD(STARTLOOP, ICYCLE)
  
        FORCEEND = .FALSE.
        MAXOUT(PHASE) = .FALSE.
        IF(TIMETOEND .EQ. STARTLOOP .AND. NUMBERCALLS(PHASE) .GT. 0) THEN
          FORCEEND = .TRUE.
        ENDIF
        IF(EGW(PHASE, IMOD) .EQ. -999) THEN
          MAXOUT(PHASE) = .TRUE.
        ENDIF
        IF((PHASEENDFLAG(PHASE) .AND. FORCEEND) .OR. MAXOUT(PHASE)) THEN
          FORCE(PHASE) = .TRUE.
        ENDIF
      ENDIF
    ENDDO
  
    DECISION = ''
    IF(GET_DETECTOR_STATUS(STOPBAR_DETECTORS(3)) + GET_DETECTOR_STATUS(STOPBAR_DETECTORS(4)) + GET_DETECTOR_STATUS(STOPBAR_DETECTORS(7)) + GET_DETECTOR_STATUS(STOPBAR_DETECTORS(8)) .EQ. 0) THEN
                           !no calls on side street
      IF(FORCE(2) .AND. GET_DETECTOR_STATUS(STOPBAR_DETECTORS(1)) .EQ. 1) THEN !revert to phase 1
        CALL SET_FORCEOFF(2, RELTIMENOW)           !force off ring 1
        CALL CLEAR_EXTENSION(2)        !drop hold on phase 2
        FORCEFLAG(2) = SIMTIME         
        DECISION = "End 2, go to 1 "
      ENDIF
    
      IF(FORCE(6) .AND. GET_DETECTOR_STATUS(STOPBAR_DETECTORS(5)) .EQ. 1) THEN !revert to phase 5
        CALL SET_FORCEOFF(6, RELTIMENOW)           !force off ring 2
        CALL CLEAR_EXTENSION(6)        !drop hold on phase 6
        FORCEFLAG(6) = SIMTIME         
        DECISION = DECISION // "End 6, go to 5"
      ENDIF
    ELSE
                        !one or more calls on the side street
                        !if 2 or 6 is not at barrier and is lagged by left, allow move to lagging left phase
      IF(LAGGING1) THEN !lagging lefts ring 1 (2 + 1)
        IF(FORCE(2) .AND. GET_DETECTOR_STATUS(STOPBAR_DETECTORS(1)) .EQ. 1) THEN !serve phase 1 before going to minor road
          CALL SET_FORCEOFF(2, RELTIMENOW)         !force off ring 1
          CALL CLEAR_EXTENSION(2)      !drop hold on phase 2
          FORCEFLAG(2) = SIMTIME         
          DECISION = "End 2, go to 1 "
        ENDIF
      ELSE             !leading lefts ring 2 (1 + 2)
        IF(.NOT. PHASEGRN(2) .AND. SBB(1) .GT. 0 .AND. GET_DETECTOR_STATUS(STOPBAR_DETECTORS(2)) .EQ. 1) THEN
          ! in 6, 1 gapped out, call on 2 and 4+8
          DECISION = "End 1, go to 2 "
        ENDIF
      ENDIF
    
      IF(LAGGING5) THEN !lagging lefts ring 2 (6 + 5)
        IF(FORCE(6) .AND. GET_DETECTOR_STATUS(STOPBAR_DETECTORS(5)) .EQ. 1) THEN !serve phase 5 before going to minor road
          CALL SET_FORCEOFF(6, RELTIMENOW)         !force off ring 2
          CALL CLEAR_EXTENSION(6)      !drop hold on phase 6
          FORCEFLAG(6) = SIMTIME       !
          DECISION = DECISION // "End 6, go to 5 "
        ENDIF
      ELSE             !leading lefts ring 2 (5 + 6)
        IF(.NOT. PHASEGRN(6) .AND. SBB(2) .GT. 0 .AND. GET_DETECTOR_STATUS(STOPBAR_DETECTORS(6)) .EQ. 1) THEN
          !in 2, 5 gapped out, call on 6 and 4+8
          DECISION = DECISION // "End 5, go to 6 "
        ENDIF
      ENDIF
      
      DO PHASE = 2, 6, 4
        IF(PHASE .EQ. 2) THEN
          K = 5
        ELSE
          K = 1
        ENDIF
        IF(K .EQ. 1) THEN
          RING = 1
        ELSE
          RING = 2
        ENDIF
        IF(PHASEGRN(PHASE) .AND. PHASEGRN(K) .AND. RELTIMENOW .GT. TIMETOMAXOUT(K)) SBB(RING) = 1    
        
      ENDDO
  
      IF(DECISION .EQ. '') THEN     !at the barrier with a call on the side street
        IF((FORCEBOTH(2) .AND. FORCEBOTH(6)) .OR. (FORCE(2) .AND. FORCE(6))) THEN
          CALL SET_FORCEOFF(2, RELTIMENOW)         !force off ring 1
          CALL SET_FORCEOFF(6, RELTIMENOW)         !force off ring 2
          CALL CLEAR_EXTENSION(2)      !drop hold on phase 2
          CALL CLEAR_EXTENSION(6)      !drop hold on phase 6
          FORCEFLAG(2) = SIMTIME      
          FORCEFLAG(6) = SIMTIME      
          DECISION = "End 2 + 6, go to 4 and/or 8"
        ENDIF
  
        IF(FORCE(2) .AND. .NOT. PHASEGRN(6) .AND. SBB(2) .GT. 0) THEN  !phase 5 green but gapped out OR maxout on 2
          CALL SET_FORCEOFF(2, RELTIMENOW)         !force off ring 1
          CALL SET_FORCEOFF(6, RELTIMENOW)         !force off ring 2
          CALL CLEAR_EXTENSION(2)      !drop hold on phase 2
          FORCEFLAG(2) = SIMTIME        
          FORCEFLAG(6) = SIMTIME       !skipped phase 6, so 6 stays red, must explicitly drop force in 2.0 second
          DECISION = "End 2 + 5, go to 4 and/or 8"
        ENDIF
  
        IF(FORCE(6) .AND. .NOT. PHASEGRN(2) .AND. SBB(1) .GT. 0) THEN !phase 1 green but gapped out OR maxout on 6
          CALL SET_FORCEOFF(2, RELTIMENOW)         !force off ring 1
          CALL SET_FORCEOFF(6, RELTIMENOW)         !force off ring 2
          CALL CLEAR_EXTENSION(6)      !drop hold on phase 6
          FORCEFLAG(2) = SIMTIME       !skipped phase 2, so 2 stays red, must explicitly drop force in 2.0 second
          FORCEFLAG(6) = SIMTIME       
          DECISION = "End 1 + 6, go to 4 and/or 8"
        ENDIF
      ENDIF
    ENDIF
  
    IF(DECISION .NE. '' .AND. LOGGING) CALL DPRINT_JAB5(DECISION, FORCEEND, MAXOUT(2), MAXOUT(6))
  ENDIF                          !end of EGW check
  RETURN
  END

  SUBROUTINE CLASSIFIERSTATUS             !called every 10 ms for TS-1 and 100 ms for TS-2
  USE DCS_DATA
  USE SIMPARAMS
  USE STREET_LINKS
  USE STREET_DETECTORS
  IMPLICIT NONE
  INTEGER :: PHASE, K
  INTEGER :: LENGTH, LAG !soft classifier
  REAL :: AVSPEED !soft classifier
  INTEGER :: IDET
  
  !Call SoftClassifier(SVehSpeed(), SVehLength(), lag)  !soft classifier
  !!!Average speed and average length are calculated below
  LAG = 0
  
  IF(PHASEGRN(2) .OR. PHASEGRN(6)) THEN   !either phase 2 or 6 is green
    IF(FIRSTSTARTOFGREEN .EQ. 0) THEN     !things to do at start of first green
      FIRSTSTARTOFGREEN = SIMTIME         !record start of green for first phase to get green
    ENDIF
  
    DO PHASE = 2, 6, 4
      IF(PHASEGRN(PHASE)) THEN      !things to do when phase PHASE is green
   
        DO K = 1, PHASELANES(PHASE)   !step through each speed trap
          CALL REFRESHLVDZ(PHASE, K)  !reach ahead and zero out cells
          AVSPEED = 0
          LENGTH = 0
          IDET = ZONE_DETECTORS(PHASE, K)

          IF(SDETECTOR(IDET)%LAST_ACTUATION_TIME .GE. SIMTIME - 0.5) THEN
            AVSPEED = SDETECTOR(IDET)%LAST_SPEED
            LENGTH = SDETECTOR(IDET)%LAST_LENGTH
          ENDIF
          
          IF(AVSPEED .GT. 0 .AND. LENGTH .GT. 0) THEN !valid speed and length estimates
            IF(AVSPEED .GT. MAXSPEED * 1.47) AVSPEED = MAXSPEED * 1.47
            IF(AVSPEED .LT. MINSPEED * 1.47) AVSPEED = MINSPEED * 1.47
            IF(LENGTH .GT. MAXTRUCKLENGTH) LENGTH = MAXTRUCKLENGTH
            CALL UPDATELVDZ(PHASE, K, AVSPEED, LENGTH, LAG)
          ENDIF
        ENDDO
      ENDIF
    ENDDO
  ELSE                            !both phases are red
    FIRSTSTARTOFGREEN = 0
  ENDIF                         !end of either-phase-green actions
  RETURN
  END

  SUBROUTINE UPDATELVDZ(PHASE, K, AVSPEED, LENGTH, LAG) !(lanes, phase)
  USE DCS_DATA
  USE NTCIP_DATA
  USE SIMPARAMS
  USE ACTUATED_CONTROLLERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: PHASE, K, LENGTH, LAG
  REAL, INTENT(IN) :: AVSPEED
  REAL :: DIST, TRAVELTIME
  REAL :: VSL, TTRAP, ALTTRAVELTIME, VFINAL, ACCEL
  INTEGER :: IMOD, RING, P, ARIVALTIME, DEPARTTIME

  TTRAP = SIMTIME - FIRSTSTARTOFGREEN
  DIST = LOOPDIST(PHASE, K) - (LENGTH + LAG * AVSPEED)
  TRAVELTIME = DIST / AVSPEED + TTRAP !travel time to stop line
  VSL = AVSPEED
 
  !the following code adjusts the arival and depart times when platoons form in the d.z.
  IF(TRAVELTIME .LT. TSLLAST(PHASE, K) + MINHDWY) THEN
    TRAVELTIME = TSLLAST(PHASE, K) + MINHDWY
    VSL = VSLLAST(PHASE, K)
  ENDIF
 
  !the following code adjusts only the depart time when there is a queue in service at
  !the stop line but the min. green has timed out.
  IF(PHASE .EQ. 2) THEN
    RING = 1
  ELSE
    RING = 2
   ENDIF 
  ACCEL = 0
  ALTTRAVELTIME = TRAVELTIME
  
  !!!IF(SBA(RING) .EQ. 1 .AND. SBB(RING) .EQ. 0 .AND. SBC(RING) .EQ. 0 .AND. PHASEENDFLAG(PHASE) .EQ. .FALSE.) THEN
  !!!status bits A, B and C = 1,0,0 indicates "extension" --- 2.8.6.1 Ring Status
  !!!we use signal data instead of status bits, so we replace SBA(RING) .EQ. 1 .AND. SBB(RING) .EQ. 0 .AND. SBC(RING) .EQ. 0
  !! with BTEST(EXTENSION_GROUP, PHASE-1)
  IF(BTEST(EXTENSION_GROUP, PHASE-1) .AND. PHASEENDFLAG(PHASE) .EQ. .FALSE.) THEN  
    VFINAL = AVSPEED
    IF(AVSPEED .GT. QUESPEED) VFINAL = QUESPEED
    IF(AVSPEED .GT. VFINAL) THEN
      IF(DIST .GT. 0) THEN
        ACCEL = (AVSPEED * AVSPEED - VFINAL * VFINAL) / 2 / DIST 
      ELSE
        ACCEL = 99
      ENDIF
      ALTTRAVELTIME = (AVSPEED - VFINAL) / ACCEL + TTRAP
      IF(ALTTRAVELTIME .LT. TRAVELTIME) ALTTRAVELTIME = TRAVELTIME
    ENDIF
  ENDIF
 
  ARIVALTIME = INT((TRAVELTIME - DZ_ENTRY_TIME) / DELTAT + 0.499) !intervals to arrival
  IF(ARIVALTIME .LT. 0) ARIVALTIME = 0
  DEPARTTIME = INT((ALTTRAVELTIME - DZ_EXIT_TIME) / DELTAT + 0.499)   !intervals to departure

  VSLLAST(PHASE, K) = VSL
  TSLLAST(PHASE, K) = TRAVELTIME
 
  !add the vehicle's length to each time interval in zone
  DO P = ARIVALTIME, DEPARTTIME
    IMOD = MOD(P, ICYCLE)
    LVDZ(PHASE, K, IMOD) = LVDZ(PHASE, K, IMOD) + LENGTH
  ENDDO
  
  IF(LOGGING) CALL DPRINT_JAB1(PHASE, K, TRAVELTIME, ARIVALTIME, DEPARTTIME, VSL, AVSPEED, ACCEL, LENGTH, LAG)
  RETURN
  END

  SUBROUTINE REFRESHLVDZ(PHASE, K)
  USE DCS_DATA
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: PHASE, K
  INTEGER :: LMOD, ARIVALTIME, DEPARTTIME, P
  REAL :: DIST, TRAVELTIME

  DIST = LOOPDIST(PHASE, K)
             
  !arrival time to stop line of slowest vehicle.
  TRAVELTIME = DIST / (MINSPEED * 1.47) + SIMTIME - FIRSTSTARTOFGREEN !travel time to stop line
  ARIVALTIME = INT((TRAVELTIME - DZ_EXIT_TIME) / DELTAT)  !intervals to arrival
  IF(ARIVALTIME .LT. 0) ARIVALTIME = 0
  DEPARTTIME = INT((TRAVELTIME - 0) / DELTAT)    !intervals to departure

  !every Timer1 interval wipe LVDZ cells just beyond D.Z. of slowest veh.
  DO P = ARIVALTIME, DEPARTTIME
    LMOD = MOD(P, ICYCLE)
    LVDZ(PHASE, K, LMOD) = 0         !set LVDZ array values to 0
  ENDDO
  RETURN
  END

  SUBROUTINE DPRINT_JAB1(PHASE, K, TRAVELTIME, ARIVALTIME, DEPARTTIME, VSL, AVSPEED, ACCEL, LENGTH, LAG)
  USE TEXT
  USE SIMPARAMS
  USE DCS_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: PHASE, K, ARIVALTIME, DEPARTTIME, LENGTH, LAG
  REAL, INTENT(IN) :: TRAVELTIME, VSL, AVSPEED, ACCEL
  INTEGER :: P, LMOD
  WRITE(MSGTEXT, '(A)') "DPRINT_JAB1:"
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,F8.1,A,I1)') " Time: ", SIMTIME, " Phase: ", PHASE
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,I3)') " Cycle:", ICYCLE
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,I)') " Lane: ", K
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,F8.1)') " TravelTime: ", TRAVELTIME
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,I)') " ArrTime: ", ARIVALTIME
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,I)')  " DepTime: ", DEPARTTIME
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,F8.1)')  " FirstGreen: ", FIRSTSTARTOFGREEN
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,F8.1)')  " avSpeed: ", AVSPEED
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,F8.1)')  " vsl: ", VSL
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,F8.1)')  " accel: ", ACCEL
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,F8.1)')  " length: ", Length
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,F8.1)')  " lag: ", lag
  CALL SENDTEXTMSG(M_ERROR)
  DO P = ARIVALTIME, DEPARTTIME
    LMOD = MOD(P, ICYCLE)
    WRITE(MSGTEXT, '(3I)') LVDZ(PHASE, K, LMOD)
    CALL SENDTEXTMSG(M_ERROR)
  ENDDO
  RETURN
  END
  
  
  SUBROUTINE DPRINT_JAB2(RELTIMENOW, PHASE, STARTLOOP, ENDLOOP)
  USE TEXT
  USE SIMPARAMS
  USE DCS_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: PHASE, STARTLOOP, ENDLOOP
  REAL, INTENT(IN) :: RELTIMENOW
  WRITE(MSGTEXT, '(A)') "DPRINT_JAB2:"
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,F5.1,A,I1)') " Time: ", SIMTIME, " Phase: ", PHASE
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,I3)')  " Cycle: ", ICYCLE
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,F8.1)')  " FirstGreen: ", FIRSTSTARTOFGREEN
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,I3)')  " startloop: ", STARTLOOP
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,I3)')  " endloop: ", ENDLOOP
  CALL SENDTEXTMSG(M_ERROR)
  RETURN
  END

  SUBROUTINE DPRINT_JAB3(P, STARTLOOP, ENDLOOP, MAXLANES, PHASE)
  USE DCS_DATA
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: P, STARTLOOP, ENDLOOP, MAXLANES, PHASE
  INTEGER :: I, K, IMOD
  WRITE(MSGTEXT, '(A)') "DPRINT_JAB3:"
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,I1)') " nCalls: ", NUMBERCALLS(PHASE)
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,F8.1)') " TimeToMaxOut: ", TIMETOMAXOUT(PHASE)
  CALL SENDTEXTMSG(M_ERROR)
  IF(P .EQ. 1) THEN
    DO I = STARTLOOP, ENDLOOP
      IMOD = MOD(I, ICYCLE)
      DO K = 1, MAXLANES
        WRITE(MSGTEXT, '(A,I,A,I,A,I)') " Phase: ", PHASE, " i: ", IMOD, " Lane: ", K, "lvdz: ", LVDZ(PHASE, K, IMOD)
        CALL SENDTEXTMSG(M_ERROR)
      ENDDO
      WRITE(MSGTEXT, '(A,F8.1)') " futdly: ", (I - STARTLOOP) * NUMBERCALLS(PHASE) * DELTAT
      CALL SENDTEXTMSG(M_ERROR)
      WRITE(MSGTEXT, '(A,F8.1,A, F8.1)') " egw: ", EGW(PHASE, IMOD), " maxpcindz: ", MAXPCEINDZ(PHASE, IMOD)
      CALL SENDTEXTMSG(M_ERROR)
    ENDDO
  ENDIF
  RETURN
  END
  
  SUBROUTINE DPRINT_JAB4(PHASE, STARTLOOP, ENDLOOP, MEG1, MEG2, TIMETOEND)
  USE SIMPARAMS
  USE DCS_DATA
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: PHASE, STARTLOOP, ENDLOOP, TIMETOEND
  REAL, INTENT(IN) :: MEG1, MEG2
  INTEGER :: I, IMOD
  WRITE(MSGTEXT, '(A)') "DPRINT_JAB4:"
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,F8.1,A,I1)') " Time: ", SIMTIME, " Phase: ", PHASE
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,F5.1)') " megw: ", MEG1
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,F8.1)') " megw+d: ", MEG2
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,I)') " timetoend: ", TIMETOEND
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,I)') " ncalls2: ", NUMBERCALLS(2)
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,I)') " ncalls6: ", NUMBERCALLS(6)
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,I)') " PhaseEndFlag2: ", PHASEENDFLAG(2)
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,I)') " PhaseEndFlag6: ", PHASEENDFLAG(6)
  CALL SENDTEXTMSG(M_ERROR)
  RETURN
  END

  SUBROUTINE DPRINT_JAB5(DECISION, FORCEEND, MAXOUT2, MAXOUT6)
  USE TEXT
  USE SIMPARAMS
  USE DCS_DATA
  IMPLICIT NONE
  CHARACTER, INTENT(IN) :: DECISION*30
  LOGICAL, INTENT(IN) :: FORCEEND, MAXOUT2, MAXOUT6
  WRITE(MSGTEXT, '(A)') "DPRINT_JAB5:"
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,F5.1,A)') " Time: ", SIMTIME, " ", DECISION
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,L)') " PhaseEndFlag2: ", PHASEENDFLAG(2)
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,L)') " PhaseEndFlag6: ", PHASEENDFLAG(6)
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,L)') " ForceEnd: ", FORCEEND
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,L)') " MaxOut2: ", MAXOUT2
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A,L)') " MaxOut6: ", MAXOUT6
  CALL SENDTEXTMSG(M_ERROR)
  RETURN
  END
  
  INTEGER FUNCTION GETENDLOOP(A, B, STARTLOOP)
  USE DCS_DATA
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: A, B, STARTLOOP
  INTEGER :: PHASE, K
  REAL :: MINLOOKAHEAD
  MINLOOKAHEAD = 999999
  DO PHASE = A, B, 4
    DO K = 1, PHASELANES(PHASE)
      IF(LOOKAHEADTIME(PHASE, K) .LT. MINLOOKAHEAD) MINLOOKAHEAD = LOOKAHEADTIME(PHASE, K)
    ENDDO
  ENDDO
  GETENDLOOP = STARTLOOP + INT(MINLOOKAHEAD / DELTAT + 0.499)
  END

  SUBROUTINE SETCYCLE
  USE DCS_DATA
  USE TEXT
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER :: PHASE, K, MAXLOOPDIST

  DO PHASE = 2, 6, 4     !time into future for which EGW is computed, s
    DO K = 1, PHASELANES(PHASE)
      LOOKAHEADTIME(PHASE, K) = (LOOPDIST(PHASE, K) - MAXTRUCKLENGTH) / (MAXSPEED * 1.47) - 500 / 1000 - DZ_ENTRY_TIME
      IF(LOOKAHEADTIME(PHASE, K) .LT. 0) THEN
        WRITE(MSGTEXT, '(A,I1,A,I1,A)') 'lookahead for phase ', PHASE, ' lane ', k, ' is less than 0'
        CALL SENDTEXTMSG(M_ERROR)
        ERROR_FLAG = 1
        RETURN
      ENDIF
    ENDDO
    PHASEGRN(PHASE) = .FALSE.
    FORCEFLAG(PHASE) = 0
  ENDDO

  MAXLOOPDIST = 0
  DO PHASE = 2, 6, 4
    DO K = 1, PHASELANES(PHASE)
      IF(LOOPDIST(PHASE, K) .GT. 0 .AND. LOOPDIST(PHASE, K) .GT. MAXLOOPDIST) THEN
         MAXLOOPDIST = LOOPDIST(PHASE, K)
      ENDIF
    ENDDO
  ENDDO
  ICYCLE = INT(MAXLOOPDIST / (MINSPEED * 1.47) / DELTAT + 0.499) + 1
  IF(ICYCLE .GT. 600) ICYCLE = 600
  
  RETURN
  END

! ==================================================================================================
  INTEGER FUNCTION GET_DETECTOR_STATUS(IDET)
!--------------------------------------------------------------------
!This function will return the current state of a specific detector.
!--------------------------------------------------------------------
  USE STREET_DETECTORS
  USE SIMPARAMS
  USE DCS_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IDET
  IF(USE_EXTERNAL_DETECTORS) THEN
    GET_DETECTOR_STATUS = BTEST(DETECTOR_STATES, IDET-1)
  ELSE
    GET_DETECTOR_STATUS = SDETECTOR(IDET)%CURRENT_STATE
  ENDIF
  END   
