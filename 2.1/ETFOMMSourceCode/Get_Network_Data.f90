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
  SUBROUTINE GET_NETWORK_DATA
! ----------------------------------------------------------------------
! --- Read inputs.
! ----------------------------------------------------------------------
  USE SIMPARAMS
  USE TEXT
  USE FREEWAY_LINKS
  USE STREET_LINKS
  USE ENTRYNODE_DATA
  USE SEGMENTS
  USE FREEWAY_NODES
  USE STREET_NODES
  USE FLOWDATA_MOD
  USE BUS_ROUTE_DATA
  USE BUS_STATION_DATA
  USE CORSIM_NODES
  USE INCIDENTS
  USE DIVERSIONS
  USE FREEWAY_DETECTORS
  USE STREET_DETECTORS
  USE TIMED_CONTROLLERS
  USE ACTUATED_CONTROLLERS
  USE ROUNDABOUT_DATA
  USE DATASTATIONS
  USE RAMP_METERS
  USE GLOBAL_DATA
  USE EV_DATA
  IMPLICIT NONE
  INCLUDE 'IOFILES.INC'
  CHARACTER*80 :: STRING
  INTEGER :: I, RT, ITP, THISROUTE
  INTEGER, SAVE :: LASTROUTE
  INTEGER :: TTLFLK, NFMAP, TTLNK, NMAP, INET, IACT, PHASE, INTCH
  INTEGER :: LIST(8), NSC, NP, ERRORS
  REAL :: SPLITS(8), MINSPLITS(8)
  COMMON /PRI215[DLLEXPORT]/ TTLFLK
  COMMON /PRI075[DLLEXPORT]/ NFMAP(8999)
  COMMON /SIN116[DLLEXPORT]/ TTLNK
  COMMON /SIN075[DLLEXPORT]/ NMAP(8999) 
! ----------------------------------------------------------------------

! --- These arrays are only used by TRAFVU.

  DO CONCURRENT (I = 1:8999)
    NMAP(I) = I
    NFMAP(I) = I
  ENDDO

! --- Open the input file.

  OPEN(65, FILE = LINFNAME(1:IROOT)//'TRF', STATUS='OLD', ERR=10, IOMSG=ETEXT)
  IF(TIME_PERIOD .EQ. 1) THEN
    N_PERIODS = 0
    DO
      READ(65, '(A)', END=1, ERR=20, IOMSG=ETEXT) STRING
      IF(STRING(79:80) .EQ. '00') CYCLE
      IF(STRING(1:1) .NE. '<') THEN
        READ(STRING(78:80), '(I3)') RT
        IF(RT .EQ. 210) N_PERIODS = N_PERIODS + 1
      ENDIF
    ENDDO
1   CONTINUE
    
    REWIND(65)
    DO
      READ(65, '(A)', END=20, ERR=20, IOMSG=ETEXT) STRING
      IF(STRING(79:80) .EQ. '00') CYCLE
      READ(STRING(78:80), '(I3)') RT
      SELECT CASE(RT)
        CASE(11)
  ! --- Count a street link for each RT11.
          N_STREET_LINKS = N_STREET_LINKS + 1
  ! --- Add exit links if necessary.
          IF(STRING(41:41) .EQ. '8') N_STREET_LINKS = N_STREET_LINKS + 1
          IF(STRING(45:45) .EQ. '8') N_STREET_LINKS = N_STREET_LINKS + 1
          IF(STRING(49:49) .EQ. '8') N_STREET_LINKS = N_STREET_LINKS + 1
          IF(STRING(53:53) .EQ. '8') N_STREET_LINKS = N_STREET_LINKS + 1
        
        CASE(19)
  ! --- Add a freeway link for each RT19.
          N_FREEWAY_LINKS = N_FREEWAY_LINKS + 1
  ! --- Add an exit link if necessary.
          IF(STRING(9:9) .EQ. '8') N_FREEWAY_LINKS = N_FREEWAY_LINKS + 1       
        
        CASE(20)
  ! --- Count Datastations.
          IF(STRING(34:38) .NE. '     ') NUMBER_OF_DATASTATIONS = NUMBER_OF_DATASTATIONS + 1
        
        CASE(28)        
  ! --- Count the number of freeway detectors.
          N_FREEWAY_DETECTORS = N_FREEWAY_DETECTORS + 1        
        
        CASE(29)
  ! --- Count the number of incidents.
          NUMBER_OF_INCIDENTS = NUMBER_OF_INCIDENTS + 1

        CASE(30)
  ! --- Count the number of diversions.
          NUMBER_OF_DIVERSIONS = NUMBER_OF_DIVERSIONS + 1
      
        CASE(35)
  ! --- Count the number of fixed time control signals.
  ! --- This count includes signs and uncontrolled intersections.
          NUMBER_OF_FTCS = NUMBER_OF_FTCS + 1

        CASE(37)
  ! --- Count the number of rampmeters.
          NUMBER_OF_RAMPMETERS = NUMBER_OF_RAMPMETERS + 1
        
        CASE(42)       
  ! --- Count the number of street detectors.
          N_STREET_DETECTORS = N_STREET_DETECTORS + 1
        
        CASE(43)
  ! --- Count the number of actuated control signals.
          NUMBER_OF_ACS = NUMBER_OF_ACS + 1
      
        CASE(46)       
  ! --- Count the number of street detectors.
          N_STREET_DETECTORS = N_STREET_DETECTORS + 1
          IF(STRING(32:32) .NE. ' ') N_STREET_DETECTORS = N_STREET_DETECTORS + 1
          IF(STRING(52:52) .NE. ' ') N_STREET_DETECTORS = N_STREET_DETECTORS + 1
      
        CASE(97)
  ! --- Count the number of roundabouts.
          NUMBER_OF_ROUNDABOUTS = NUMBER_OF_ROUNDABOUTS + 1

        CASE(154)
  ! --- Count the number of super controllers.
          NUMBER_OF_SUPER_ACS = NUMBER_OF_SUPER_ACS + 1
      
        CASE(187)
  ! --- Count the number of bus routes.     
          READ(STRING(1:4), '(I4)') THISROUTE
          IF(THISROUTE .NE. LASTROUTE) THEN
            LASTROUTE = THISROUTE
            NUMBER_OF_ROUTES = MAX(NUMBER_OF_ROUTES, THISROUTE)
          ENDIF
        
        CASE(210)
          EXIT
      END SELECT
      
    ENDDO

! --- These arrays are only used by TRAFVU.

    TTLFLK = N_FREEWAY_LINKS
    ALLOCATE(UPNODE(TTLFLK))
    ALLOCATE(DWNODE(TTLFLK))
    TTLNK = N_STREET_LINKS
    ALLOCATE(UPNOD(TTLNK))
    ALLOCATE(DWNOD(TTLNK))

! --- Allocate link, entrynode and node arrays.

    CALL ALLOCATE_FREEWAY_LINK_ARRAYS
    CALL ALLOCATE_STREET_LINK_ARRAYS
    CALL ALLOCATE_FREEWAY_NODE_ARRAYS
    CALL ALLOCATE_STREET_NODE_ARRAYS
    CALL ALLOCATE_EV_DATA_ARRAYS

! --- Allocate FTC arrays if necessary.

    IF(NUMBER_OF_FTCS .GT. 0) THEN
      CALL ALLOCATE_FTC_ARRAYS
      NUMBER_OF_FTCS = 0
    ENDIF

! --- Allocate ROUNDABOUT arrays if necessary.

    IF(NUMBER_OF_ROUNDABOUTS .GT. 0) THEN
      CALL ALLOCATE_ROUNDABOUT_ARRAYS
    ENDIF

! --- Allocate AC arrays if necessary.

    IF(NUMBER_OF_ACS .GT. 0) THEN
      CALL ALLOCATE_AC_ARRAYS(NUMBER_OF_ACS + NUMBER_OF_SUPER_ACS)
      NUMBER_OF_ACS = 0
    ENDIF
             
! --- Allocate RAMPMETER arrays if necessary

    IF(NUMBER_OF_RAMPMETERS .GT. 0) THEN
      CALL ALLOCATE_RAMPMETER_ARRAYS
      NUMBER_OF_RAMPMETERS = 0
    ENDIF

! --- Allocate detector arrays if necessary.

    IF(N_FREEWAY_DETECTORS .GT. 0) THEN
      CALL ALLOCATE_FREEWAY_DETECTOR_ARRAYS
      N_FREEWAY_DETECTORS = 0
    ENDIF

! --- Allocate detector arrays if necessary.

    IF(N_STREET_DETECTORS .GT. 0) THEN
      CALL ALLOCATE_STREET_DETECTOR_ARRAYS
      N_STREET_DETECTORS = 0
    ENDIF

! --- Allocate datastation arrays if necessary.

    IF(NUMBER_OF_DATASTATIONS .GT. 0) THEN
      CALL ALLOCATE_DATASTATION_ARRAYS
      NUMBER_OF_DATASTATIONS = 0
    ENDIF

! --- Allocate incident arrays if necessary.

    IF(NUMBER_OF_INCIDENTS .GT. 0) CALL ALLOCATE_INCIDENT_ARRAYS

! --- Allocate diversion arrays if necessary.

    IF(NUMBER_OF_DIVERSIONS .GT. 0) CALL ALLOCATE_DIVERSION_ARRAYS

! --- Allocate bus arrays if necessary.

    IF(NUMBER_OF_ROUTES .GT. 0) THEN
      CALL ALLOCATE_BUS_ROUTE_DATA_ARRAYS
      CALL DEFINE_DWELL_MULTIPLIERS
    ENDIF

! --- Rewind the input file for further processing.

    REWIND(65)
    N_FREEWAY_LINKS = 0
    N_STREET_LINKS = 0
    NUMBER_OF_ACS = 0
    N_STREET_DETECTORS = 0
    DO

! --- Read each line and process the inputs for the first time period.

      READ(65, '(A)', ERR=20, IOMSG=ETEXT) STRING
      READ(STRING(78:80), '(I3)', ERR=21, IOMSG=ETEXT) RT
      SELECT CASE(RT)
        CASE(0)
          CYCLE
        CASE(1)
          CYCLE
        CASE(2)
          CALL READ_RT02(STRING, INET)
        CASE(3)
          CALL READ_RT03(STRING)
        CASE(4)
          CALL READ_RT04(STRING)
        CASE(5)
          CALL READ_RT05(STRING)
        CASE(10)
          CYCLE
        CASE(11)
          CALL READ_RT11(STRING)
        CASE(12)
          !Turning Ways cannot be processed until all links have been processed.
        CASE(13)
          CALL READ_RT13(STRING)
        CASE(19) 
          CALL READ_RT19(STRING)
        CASE(20) 
          CALL READ_RT20(STRING)
        CASE(21) 
          CALL READ_RT21(STRING)
        CASE(22) 
          CALL READ_RT22(STRING)
        CASE(24) 
          CALL READ_RT24(STRING)
        CASE(25) 
          CALL READ_RT25(STRING)
        CASE(27) 
          CALL READ_RT27(STRING)
        CASE(28) 
          CALL READ_RT28(STRING)
        CASE(29) 
          CALL READ_RT29(STRING)
        CASE(30) 
          CALL READ_RT30(STRING)
        CASE(32) 
          CALL READ_RT32(STRING)
        CASE(33) 
          CALL READ_RT33(STRING)
        CASE(35) 
          CALL READ_RT35(STRING)
        CASE(36) 
          CALL READ_RT36(STRING)
        CASE(37) 
          CALL READ_RT37(STRING)
        CASE(38) 
          CALL READ_RT38(STRING)
        CASE(42) 
          CALL READ_RT42(STRING)
        CASE(43) 
          CALL READ_RT43(STRING)
        CASE(44) 
          CALL READ_RT44(STRING)
        CASE(45) 
          CALL READ_RT45(STRING)
        CASE(46) 
          CALL READ_RT46(STRING)
        CASE(47) 
          CALL READ_RT47(STRING)
        CASE(48) 
          CALL READ_RT48(STRING)
        CASE(49) 
          CALL READ_RT49(STRING)
        CASE(50) 
          CALL READ_RT50(STRING)
        CASE(51) 
          CALL READ_RT51(STRING)
        CASE(54) 
          CALL READ_RT54(STRING)
        CASE(55) 
          CALL READ_RT55(STRING)
        CASE(56) 
          CALL READ_RT56(STRING)
        CASE(58) 
          CALL READ_RT58(STRING)
        CASE(68) 
          CALL READ_RT68(STRING, INET)
        CASE(69) 
          CALL READ_RT69(STRING)
        CASE(70) 
          CALL READ_RT70(STRING)
        CASE(71) 
          CALL READ_RT71(STRING)
        CASE(80) 
          CALL READ_RT80(STRING)
        CASE(81) 
          CALL READ_RT81(STRING)
        CASE(82) 
          CALL READ_RT82(STRING)
        CASE(83) 
          CALL READ_RT83(STRING)
        CASE(84) 
          CALL READ_RT84(STRING)
        CASE(97) 
          !Roundabouts cannot be processed until all links have been processed.
        CASE(98)   
          !Roundabouts cannot be processed until all links have been processed.
        CASE(119) 
          CALL READ_RT119(STRING)
        CASE(136) 
          CALL READ_RT136(STRING)
        CASE(140) 
          CALL READ_RT140(STRING)
        CASE(141) 
          CALL READ_RT141(STRING)
        CASE(142) 
          CALL READ_RT142(STRING)
        CASE(143) 
          CALL READ_RT143(STRING)
        CASE(144) 
          CALL READ_RT144(STRING)
        CASE(145) 
          CALL READ_RT145(STRING)
        CASE(146) 
          CALL READ_RT146(STRING)
        CASE(147) 
          CALL READ_RT147(STRING, INET)
        CASE(148) 
          CALL READ_RT148(STRING)
        CASE(149) 
          CALL READ_RT149(STRING)
        CASE(150) 
          CALL READ_RT150(STRING)
        CASE(153) 
          CALL READ_RT153(STRING)
        CASE(154) 
          !Supercontrollers cannot be processed until all links have been processed.
        CASE(170) 
          CALL READ_RT170(STRING, INET)
        CASE(171) 
          CALL READ_RT171(STRING)
        CASE(173) 
          CALL READ_RT173(STRING)
        CASE(174) 
          CALL READ_RT174(STRING)
        CASE(185) 
          CALL READ_RT185(STRING)
        CASE(186) 
          CALL READ_RT186(STRING)
        CASE(187) 
          CALL READ_RT187(STRING)
        CASE(188) 
          CALL READ_RT188(STRING)
        CASE(189) 
          CALL READ_RT189(STRING)
        CASE(195) 
          CALL READ_RT195(STRING)
        CASE(196) 
          CALL READ_RT196(STRING)
        CASE(197) 
          CALL READ_RT197(STRING)
        CASE(201) 
          CALL READ_RT201(STRING)
        CASE(202) 
          CALL READ_RT202(STRING)
        CASE(210)
          EXIT
        CASE DEFAULT
          WRITE(MSGTEXT, '(A)') 'Record Type not implemented in ETFOMM.'
          CALL SENDTEXTMSG(M_WARNING)
          WRITE(MSGTEXT, '(A)') STRING
          CALL SENDTEXTMSG(M_WARNING)
          WRITE(MSGTEXT, '(A)') ''
          CALL SENDTEXTMSG(M_WARNING)
          IF(RT .EQ. 95 .OR. RT .EQ. 96) ERROR_COUNT = ERROR_COUNT + 1
      END SELECT    
    ENDDO

! --- These subroutines process link inputs that were read during the first pass through the RTs.

    REWIND(65)
    DO
      READ(65, '(A)', ERR=20, IOMSG=ETEXT) STRING
      IF(STRING(79:80) .EQ. '00') CYCLE
      READ(STRING(78:80), '(I3)') RT
      SELECT CASE(RT)
        CASE(11) 
          CALL PROCESS_RT11(STRING)
        CASE(12)
          CALL READ_RT12(STRING)
        CASE(19) 
          CALL PROCESS_RT19(STRING)
        CASE(97) 
          CALL READ_RT97(STRING)
        CASE(98) 
          CALL READ_RT98(STRING)
        CASE(154) 
          CALL READ_RT154(STRING)
        CASE(210)
          EXIT
      END SELECT
    ENDDO
    CALL PROCESS_FREEWAYLINKS
    CALL PROCESS_STREETLINKS
    CALL PROCESS_ACTUATED_CONTROL
  
  ELSE  
                                  
! --- Process the inputs for a subsequent time period.

    ITP = 1
    DO
      READ(65, '(A)', END=5, ERR=20, IOMSG=ETEXT) STRING
      READ(STRING(78:80), '(I3)', ERR=21, IOMSG=ETEXT) RT
      IF(STRING(79:80) .EQ. '35') RT = 35
      IF(RT .EQ. 210) ITP = ITP + 1
      IF(ITP .GT. TIME_PERIOD) THEN
        EXIT
      ELSEIF(ITP .EQ. TIME_PERIOD) THEN
        SELECT CASE(RT)
          CASE(0)
            CYCLE
          CASE(11) 
            CALL READ_RT11(STRING)
          CASE(21) 
            CALL READ_RT21(STRING)
          CASE(22) 
            CALL READ_RT22(STRING)
          CASE(24) 
            CALL READ_RT24(STRING)
          CASE(25) 
            CALL READ_RT25(STRING)
          CASE(27) 
            CALL READ_RT27(STRING)
          CASE(33)
            CALL READ_RT33(STRING)
          CASE(35)
            CALL READ_RT35(STRING)
          CASE(36) 
            CALL READ_RT36(STRING)
          CASE(37) 
            CALL READ_RT37(STRING)
          CASE(44) 
            CALL READ_RT44(STRING)
          CASE(45) 
            CALL READ_RT45(STRING)
          CASE(47) 
            CALL READ_RT47(STRING)
          CASE(48) 
            CALL READ_RT48(STRING)
          CASE(49) 
            CALL READ_RT49(STRING)
          CASE(50) 
            CYCLE
          CASE(56) 
            CALL READ_RT56(STRING)
          CASE(98) 
            CALL READ_RT98(STRING)
          CASE(136) 
            CALL READ_RT136(STRING)
          CASE(170)
            CALL READ_RT170(STRING, INET)
          CASE(189) 
            CALL READ_RT189(STRING)
          CASE(210)
            CYCLE
          CASE DEFAULT
            WRITE(MSGTEXT, '(A)') 'Record Type not allowed in subsequent time periods.'
            CALL SENDTEXTMSG(M_ERROR)
            WRITE(MSGTEXT, '(A)') STRING
            CALL SENDTEXTMSG(M_ERROR)
            WRITE(MSGTEXT, '(A)') ''
            CALL SENDTEXTMSG(M_ERROR)
        END SELECT
      ENDIF
    ENDDO
    !Process new inputs for actuated signals
    DO IACT = 1, NUMBER_OF_ACS                                                
!
!       If the controller is coordinated, calculate the phase splits    
!       and minimum splits using the user-specified input.  Check the   
!       phase splits and issue a warning if any split is less than its  
!       minimum split.                                                  
!                                                                       
      IF(AC_SIGNALS(IACT)%NEWPLAN) THEN                                    
        CALL CALCSPLITS(IACT, AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH, AC_SIGNALS(IACT)%NEW_FORCE_OFF_TIME, AC_SIGNALS(IACT)%PHASE_SEQUENCE, SPLITS)
        CALL CALCMINSPLITS(IACT, MINSPLITS)                            
        DO PHASE = 1, 8                                         
          IF(SPLITS(PHASE) .LT. MINSPLITS(PHASE)) THEN 
            WRITE(MSGTEXT, '(A,I4,A,F4.1,A,I4,A,F4.1)') 'For the controller at node ', AC_SIGNALS(IACT)%NODE(1), ' a split of ', SPLITS(PHASE), &
                ' for phase ', PHASE, ' is less than its minimum split of ', MINSPLITS(PHASE)
            CALL SENDTEXTMSG(M_WARNING)
          ENDIF                                                       
        ENDDO                                                         
      ENDIF                                                           
    ENDDO                                                             
    ALWAYS_GREEN = .FALSE.
    LIST = 0
    DO NSC = 1, NUMBER_OF_SUPER_ACS
      IACT = NUMBER_OF_ACS + NSC
      LIST(1) = AC_SIGNALS(IACT)%NODE(1)
      IF(AC_SIGNALS(IACT)%NODE(2) .NE. 0) THEN
        DO NP = 2, 8
          IF(AC_SIGNALS(IACT)%NODE(NP) .NE. 0) THEN
            LIST(NP) = AC_SIGNALS(IACT)%NODE(NP)
          ENDIF
        ENDDO
        CALL DELETE_SUPER_CONTROLLER(NSC)
        CALL STORE_SUPER_CONTROLLER(NSC, LIST, ERRORS)
      ENDIF
    ENDDO
  ENDIF
5 CONTINUE
  CLOSE(65)
  RETURN
10 CONTINUE 
  WRITE(MSGTEXT,'(A)') 'FILE OPEN ERROR - GET_NETWORK_DATA'
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A)') ETEXT
  CALL SENDTEXTMSG(M_ERROR)
  ERROR_FLAG = 1
  RETURN
20 CONTINUE 
  WRITE(MSGTEXT,'(A)') 'FILE READ ERROR - GET_NETWORK_DATA'
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A)') ETEXT
  CALL SENDTEXTMSG(M_ERROR)
  ERROR_FLAG = 1
  RETURN
21 CONTINUE 
  WRITE(MSGTEXT,'(A)') 'FILE READ ERROR - GET_NETWORK_DATA'
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A)') ETEXT
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A)') 'CHECK THE FOLLOWING RECORD'
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A)') STRING
  CALL SENDTEXTMSG(M_ERROR)
  ERROR_FLAG = 1
  RETURN
30 CONTINUE 
  WRITE(MSGTEXT,'(A)') 'FILE READ ERROR - GET_NETWORK_DATA'
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT,'(A)') '  COULD NOT READ ACTUATED CONTROL TIMING PLAN'
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A)') ETEXT
  CALL SENDTEXTMSG(M_ERROR)
  ERROR_FLAG = 1
  RETURN
  END

! ==================================================================================================
  SUBROUTINE PROCESS_FREEWAYLINKS
! ----------------------------------------------------------------------
! --- Define link areas, etc.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  USE FREEWAY_NODES
  USE GLOBAL_DATA
  USE TEXT
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER :: I, IL2, IL, IFLOW
  REAL :: RFLOW
! ----------------------------------------------------------------------
  
! --- Define the area of all freeway internal links.

  DO IL = 1, N_FREEWAY_LINKS

! --- Store interface links.

    IF(NODE_TYPE(FDSN(IL)) .EQ. NT_INTERFACE) THEN
      DO IL2 = 1, N_FREEWAY_LINKS
        IF(FUSN(IL2) .EQ. FDSN(IL)) THEN
          FTHRU_LINK(IL) = IL2
          EXIT
        ENDIF
      ENDDO
    ENDIF
    IF(NODE_TYPE(FUSN(IL)) .EQ. NT_INTERN .AND. NODE_TYPE(FDSN(IL)) .EQ. NT_INTERN) THEN

! --- Get the number of through lanes on the link and set the area.

      FAREA(IL) = FNUMLANES(IL) * FLENGTH(IL)

! --- If a lane is added or dropped, add or subtract the appropriate area.

      DO I = 1, 3
        IF(ADDDROP_LANE(IL, I) .GT. 0) THEN
          FAREA(IL) = FAREA(IL) + (FLENGTH(IL) - ADDDROP_DIST(IL, I)) * ADDDROP_CODE(IL, I)
        ENDIF
      ENDDO
 
! --- Add the area of all auxiliaries.                       
 
      DO I = 1, N_AUXLANES
        IF(AUX_LANE_CODE(IL, I) .NE. 0) FAREA(IL) = FAREA(IL) + AUX_LANE_LENGTH(IL, I)
      ENDDO
 
! --- Consider maximum safe freeflow speed based on link curvature,
! --- super-elevation and coefficient of friction.
 
      IF(PAVEMENT(IL) .GE. 1 .AND. PAVEMENT(IL) .LE. 4) THEN
        RFLOW = 15 * CURVE(IL) * (TILT(IL) + CFRICT(PAVEMENT(IL)))
        IF(RFLOW .GT. 0.) THEN
          RFLOW = SQRT(RFLOW)
          IFLOW = NINT(FFREEFLOWSPEED(IL) * FEET2MILES)
 
  ! --- If the freeflow speed exceeds the maximum safe speed reset the freeflow speed.
 
          IF(RFLOW .LT. IFLOW) THEN
            WRITE(MSGTEXT, 1) FUSN(IL), FDSN(IL), IFLOW, INT(RFLOW)
            CALL SENDTEXTMSG(M_ERROR)                  
            WRITE(MSGTEXT, *)                                   
            CALL SENDTEXTMSG(M_ERROR)                  
            RFLOW = NINT(MILES2FEET * RFLOW)
            FFREEFLOWSPEED(IL) = MIN(RFLOW, FFREEFLOWSPEED(IL))
          ENDIF                                                 
        ENDIF 
      ENDIF
    ENDIF 
    FAREA(IL) = FAREA(IL) / 5280.
    
  ENDDO       
1 FORMAT('The freeflow speed for link (', I4, ',', I4, ') entered on &
  &Record Type 20 was ', I2, ' MPH. The limiting speed based on radius &
  &of curvature, superelevation and coefficient of friction is ', I2, ' MPH.')   
  RETURN
  END 
  
! ==================================================================================================
  SUBROUTINE PROCESS_MERGE_LINKS(IL1, IL2)
! ----------------------------------------------------------------------
! --- 
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  USE FREEWAY_NODES
  USE GLOBAL_DATA
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IL1, IL2
  INTEGER :: ILN1, IRLANE1, ILN2, IRLANE2
! ----------------------------------------------------------------------
  !Determine the merge lanes.
  DO ILN1 = 1, 20
    IRLANE1 = RECEIVING_LANE(IL1, ILN1)
    DO ILN2 = 1, 20
      IRLANE2 = RECEIVING_LANE(IL2, ILN2)
      IF(IRLANE2 .EQ. IRLANE1) THEN
        MERGING_LANE(IL1) = ILN1
        MERGING_LANE(IL2) = ILN2
        RETURN
      ENDIF
    ENDDO
  ENDDO
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE PROCESS_STREETLINKS
! ----------------------------------------------------------------------
! --- Define link areas, etc.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE NODE_TABLE
  USE TIMED_CONTROLLERS
  IMPLICIT NONE
  REAL, ALLOCATABLE :: LFEED(:), RFEED(:)
  LOGICAL, ALLOCATABLE :: DONE(:)
  INTEGER :: I, J, IL, IL2, IREC, WIDTH, ILANE, ICODE, IRLANE, ILX, NX
  INTEGER :: IL1, NTHRU, KREC
  REAL :: AVE_WIDTH
  LOGICAL :: FEEDER
! ----------------------------------------------------------------------
 
! --- Define the area of all street internal links.
 
  DO CONCURRENT (IL = 1: N_STREET_LINKS)
 
! --- Store interface links.
 
    IF(NODE_TYPE(SDSN(IL)) .EQ. NT_INTERFACE) THEN
      DO IL2 = 1, N_STREET_LINKS
        IF(SUSN(IL2) .EQ. SDSN(IL)) THEN
          STHRU_LINK(IL) = IL2
          EXIT
        ENDIF
      ENDDO
    ENDIF
    
! --- Determine which lanes go to turn lanes on the receiving link
    
    IF(NODE_TYPE(SUSN(IL)) .EQ. NT_INTERN .AND. NODE_TYPE(SDSN(IL)) .EQ. NT_INTERN) THEN
      IL2 = STHRU_LINK(IL)
      IF(IL2 .NE. 0) THEN
        DO ILANE = FIRST_FULL_LANE(IL), LAST_FULL_LANE(IL)
          IRLANE = ILANE + STHRU_ALIGNMENT_LANE(IL) - SALIGNMENT_LANE(IL)
          IF(IRLANE .GT. 0) THEN
            ICODE = CHANNELIZATION(IL2, IRLANE)
            IF(ICODE .EQ. 0 .OR. ICODE .EQ. 9 .OR. ICODE .EQ. 1 .OR. ICODE .EQ. 8) THEN
              LANE_GOES_LEFT(IL, ILANE) = .TRUE.
            ELSEIF(ICODE .EQ. 0 .OR. ICODE .EQ. 9 .OR. ICODE .EQ. 4 .OR. ICODE .EQ. 7) THEN
              LANE_GOES_RIGHT(IL, ILANE) = .TRUE.
            ENDIF
          ENDIF
        ENDDO
      ENDIF
    ENDIF
    
    
    IF(NODE_TYPE(SUSN(IL)) .EQ. NT_INTERN .AND. NODE_TYPE(SDSN(IL)) .EQ. NT_INTERN) THEN
 
! --- Get the number of through lanes on the link and set the area.
 
      SAREA(IL) = SNUMLANES(IL) * SLENGTH(IL)
 
! --- Define near and far cross links.
 
      IF(NODE_TYPE(SDSN(IL)) .EQ. NT_INTERN) THEN
        DO I = 1, N_STREET_LINKS
          IF(I .EQ. IL) CYCLE
          IF(SDSN(I) .EQ. SDSN(IL)) THEN

! --- Look for the near cross street. 
! --- A link with a thru movement onto the right receiving link is the near cross street.
            
            IREC = RIGHT_LINK(IL)
            IF(IREC .NE. 0) THEN
              IF(STHRU_LINK(I) .EQ. IREC) THEN
                NEAR_CROSSLINK(IL) = I
              ENDIF
            ENDIF

! --- Look for the far cross street.
! --- A link with a thru movement onto the left receiving link is the far cross street.
            
            IREC = LEFT_LINK(IL)
            IF(IREC .NE. 0) THEN
              IF(STHRU_LINK(I) .EQ. IREC) THEN
                FAR_CROSSLINK(IL) = I
              ENDIF
            ENDIF
          ENDIF
        ENDDO

! If cross streets were not identified look for links that have a movement onto the thru receiving link.
        
        DO I = 1, N_STREET_LINKS
          IF(I .EQ. IL) CYCLE
          IF(SDSN(I) .EQ. SDSN(IL)) THEN
            IF(NEAR_CROSSLINK(IL) .EQ. 0) THEN
              IREC = STHRU_LINK(IL)
              IF(IREC .NE. 0) THEN

! --- A link with a left movement onto the thru receiving link is the near cross street
                 
                IF(LEFT_LINK(I) .EQ. IREC .OR. STHRU_LINK(I) .EQ. IREC .OR. LEFT_DIAG_LINK(I) .EQ. IREC) THEN
                  IF(STHRU_LINK(I) .NE. 0) NEAR_CROSSLINK(IL) = I
                ENDIF
              ENDIF
            ENDIF
            IF(FAR_CROSSLINK(IL) .EQ. 0) THEN
              IREC = STHRU_LINK(IL)
              IF(IREC .NE. 0) THEN
                IF(STHRU_LINK(I) .EQ. IREC .OR. RIGHT_LINK(IL) .EQ. IREC .OR. RIGHT_DIAG_LINK(I) .EQ. IREC) THEN

! --- A link with a right movement onto the thru receiving link is the far cross street
                
                  IF(STHRU_LINK(I) .NE. 0) FAR_CROSSLINK(IL) = I
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
    ENDIF
 
! --- Convert to miles and store the result.
 
    SAREA(IL) = SAREA(IL) / 5280.
  ENDDO
  
  DO IL = 1, N_STREET_LINKS
    IF(FAR_CROSSLINK(IL) .NE. 0 .AND. FAR_CROSSLINK(IL) .EQ. NEAR_CROSSLINK(IL)) THEN
      FAR_CROSSLINK(IL) = 0
    ENDIF
  ENDDO
  
  !Identify upstream approaches to the cross links
  DO IL = 1, N_STREET_LINKS
    NX = 0
    IL2 = NEAR_CROSSLINK(IL)
    IF(IL2 .NE. 0) THEN
      DO ILX = 1, N_STREET_LINKS
        IF(NODE_TYPE(SUSN(ILX)) .EQ. NT_EXTERN) CYCLE
        FEEDER = LEFT_LINK(ILX) .EQ. IL2 .OR. STHRU_LINK(ILX) .EQ. IL2 .OR. RIGHT_LINK(ILX) .EQ. IL2 .OR. &
                 LEFT_DIAG_LINK(ILX) .EQ. IL2 .OR. RIGHT_DIAG_LINK(ILX) .EQ. IL2
        IF(FEEDER) THEN
          NX = NX + 1
          NEAR_UPPER_CROSSLINK(IL2, NX) = ILX
          IF(NX .EQ. 5) EXIT
        ENDIF
      ENDDO
    ENDIF
  ENDDO

  DO IL = 1, N_STREET_LINKS
    IF(AC_SIGNAL_ID(IL) .NE. 0) CYCLE
    IF(SIGNAL_CODE(IL) .EQ. 30 .OR. SIGNAL_CODE(IL) .EQ. 31) CYCLE
    NTHRU = 0
    IF(LEFT_LINK(IL) .NE. 0) THEN
      IREC = LEFT_LINK(IL)
      NTHRU = NTHRU + 1
    ENDIF
    IF(STHRU_LINK(IL) .NE. 0) THEN
      IREC = STHRU_LINK(IL)
      NTHRU = NTHRU + 1
    ENDIF
    IF(RIGHT_LINK(IL) .NE. 0) THEN
      IREC = RIGHT_LINK(IL)
      NTHRU = NTHRU + 1
    ENDIF
    IF(LEFT_DIAG_LINK(IL) .NE. 0) THEN
      IREC = LEFT_DIAG_LINK(IL)
      NTHRU = NTHRU + 1
    ENDIF
    IF(RIGHT_DIAG_LINK(IL) .NE. 0) THEN
      IREC = RIGHT_DIAG_LINK(IL)
      NTHRU = NTHRU + 1
    ENDIF
    IF(NTHRU .EQ. 1) THEN
      DO IL2 = 1, N_STREET_LINKS
        IF(IL2 .EQ. IL) CYCLE
        IF(SDSN(IL2) .NE. SDSN(IL)) CYCLE
        IF(AC_SIGNAL_ID(IL2) .NE. 0) CYCLE
        IF(SIGNAL_CODE(IL2) .EQ. 30 .OR. SIGNAL_CODE(IL2) .EQ. 31) CYCLE
        NTHRU = 0
        IF(LEFT_LINK(IL2) .NE. 0) THEN
          KREC = LEFT_LINK(IL2)
          NTHRU = NTHRU + 1
        ENDIF
        IF(STHRU_LINK(IL2) .NE. 0) THEN
          KREC = STHRU_LINK(IL2)
          NTHRU = NTHRU + 1
        ENDIF
        IF(RIGHT_LINK(IL2) .NE. 0) THEN
          KREC = RIGHT_LINK(IL2)
          NTHRU = NTHRU + 1
        ENDIF
        IF(LEFT_DIAG_LINK(IL2) .NE. 0) THEN
          KREC = LEFT_DIAG_LINK(IL2)
          NTHRU = NTHRU + 1
        ENDIF
        IF(RIGHT_DIAG_LINK(IL2) .NE. 0) THEN
          KREC = RIGHT_DIAG_LINK(IL2)
          NTHRU = NTHRU + 1
        ENDIF
        IF(NTHRU .EQ. 1 .AND. IREC .EQ. KREC) THEN
          CHECK_MERGE(IL) = IL2
          CHECK_MERGE(IL2) = IL
        ENDIF
      ENDDO
    ENDIF
  ENDDO
 
! --- Define the width of the upstream intersection.
 
  ALLOCATE(LFEED(N_STREET_LINKS))
  ALLOCATE(RFEED(N_STREET_LINKS))
  LFEED = 0
  RFEED = 0
  
  ALLOCATE(DONE(N_STREET_LINKS))
  DONE = .FALSE.
  DO CONCURRENT (I = 1: N_STREET_LINKS)
    IF(UP_INT_WIDTH(I) .NE. 0) DONE(I) = .TRUE.
  ENDDO
  
  DO CONCURRENT (I = 1: N_STREET_LINKS)
    IREC = RIGHT_LINK(I)
    IF(IREC .NE. 0) THEN
      IF(.NOT. DONE(IREC)) THEN
        WIDTH = 0
        DO J = FIRST_FULL_LANE(I), LAST_FULL_LANE(I)
          WIDTH = WIDTH + SLANE_WIDTH(IREC, J)
        ENDDO
        AVE_WIDTH = WIDTH / SNUMLANES(I)
 
! --- Save the width of the turn pockets on the link that feeds the right receiving link.
 
        RFEED(IREC) = NUMBER_LEFTPOCKETS(I) * AVE_WIDTH
        UP_INT_WIDTH(IREC) = UP_INT_WIDTH(IREC) + WIDTH &
                            + (NUMBER_LEFTPOCKETS(I) + NUMBER_RIGHTPOCKETS(I)) * AVE_WIDTH
      ENDIF
    ENDIF
    IREC = LEFT_LINK(I)
    IF(IREC .NE. 0) THEN
      IF(.NOT. DONE(IREC)) THEN
        WIDTH = 0
        DO J = FIRST_FULL_LANE(I), LAST_FULL_LANE(I)
          WIDTH = WIDTH + SLANE_WIDTH(IREC, J)
        ENDDO
        AVE_WIDTH = WIDTH / SNUMLANES(I)

! --- Save the width of the turn pockets on the link that feeds the left receiving link.

        LFEED(IREC) = NUMBER_LEFTPOCKETS(I) * AVE_WIDTH
        UP_INT_WIDTH(IREC) = UP_INT_WIDTH(IREC) + WIDTH &
                            + (NUMBER_LEFTPOCKETS(I) + NUMBER_RIGHTPOCKETS(I)) * AVE_WIDTH
      ENDIF
    ENDIF
  ENDDO
 
! --- Correct for offsetting, opposing left turn pocket lanes.
 
  DO CONCURRENT (I = 1: N_STREET_LINKS)
    IF(DONE(I)) CYCLE
    IF(UP_INT_WIDTH(I) .EQ. 0) CYCLE
    UP_INT_WIDTH(I) = UP_INT_WIDTH(I) - MIN(LFEED(I), RFEED(I))
  ENDDO
  
  DO CONCURRENT (I = 1: N_STREET_LINKS)
    IF(SIGNAL_CODE(I) .EQ. S_PERGRN) SSTARTUP_TIME(I) = 0
  ENDDO
  
  DEALLOCATE(LFEED)
  DEALLOCATE(RFEED)
  DEALLOCATE(DONE)
  RETURN
  END
 
! ==================================================================================================
  SUBROUTINE READ_RT02(STRING, INET)
! ----------------------------------------------------------------------
!     Run Control
! ----------------------------------------------------------------------
  USE SIMPARAMS
  USE ENTRYNODE_DATA
  USE SEEDS
  USE GLOBAL_DATA
  USE FREEWAY_VEHICLES
  USE STREET_VEHICLES
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER, INTENT(OUT) :: INET
  INTEGER :: IBUF(20), ISEED(2)
! ----------------------------------------------------------------------
  READ(STRING, '(I8,3I4,1X,I8,1X,2I2,2X,2I1,1X,3I1,2I2,5X,I1,I4,3X,I1,2I8,I1)') IBUF
  TYPE_OF_RUN = IBUF(1)
  !  0 = SIMULATION ONLY
  ! -1 = DIAGNOSTIC ONLY
  !  1 = SIMULATION PLUS DIAGNOSTIC
  !  4 = CONVERT TEXT ANIMATION FILES TO TRAFVU FORMAT
  IF(TYPE_OF_RUN .LT. -1 .OR. TYPE_OF_RUN .GT. 4) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A, I4)') 'RT 02: INVALID TYPE OF RUN = ', TYPE_OF_RUN
    CALL SENDTEXTMSG(M_ERROR)
  ELSEIF(TYPE_OF_RUN .EQ. 1 .OR. TYPE_OF_RUN .EQ. -1) THEN
    WRITE(MSGTEXT, '(A)') 'CHECKING FOR INPUT ERRORS FOR TIME PERIOD 1'
    CALL SENDTEXTMSG(M_INFO)
  ENDIF
  
  TRANSITION_CODE = IBUF(12)
    
  READ_ANIMATION_RUN = IBUF(1) .EQ. 4
  SKIP_INIT = IBUF(3) .EQ. 2
  IF(IBUF(4) .GT. 0) THEN
    INITIALIZATION_END = IBUF(4) * 60
  ELSEIF(IBUF(4) .LT. 0) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A, I4)') 'RT 02: Negative Time Specified for Initialization = ', IBUF(4)
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
    INITIALIZATION_END = 900
  ENDIF
  IF(SKIP_INIT) INITIALIZATION_END = 0
  TYPEDIST = IBUF(8)
  ERLANGA = IBUF(9)
  READ_SPLITS = IBUF(10) .EQ. 1
  USE_DCS = IBUF(11) .EQ. 1
  LIMIT_TO_MAXGREEN = IBUF(12) .EQ. 1
  DZ_ENTRY_TIME = IBUF(13) / 10.
  DZ_EXIT_TIME = IBUF(14) / 10.
  SIM_START_TIME = IBUF(16)
  IF(IBUF(15) .NE. 3 .AND. IBUF(15) .NE. 8) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A, I4)') 'RT 02: Invalid Next Model Type = ', IBUF(15)
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
    IF(IBUF(10) .EQ. 3) THEN
      INET = 2
    ELSE
      INET = 1
    ENDIF
  ENDIF
 
! --- ISEED1 is used for vehicle entry headways
! --- ISEED2 is not currently used
! --- ISEED3 is used for traffic choices
 
  ISEED1 = IBUF(5)
  ISEED2 = IBUF(18)
  ISEED3 = IBUF(19)
  ISEED(1) = ISEED1
  CALL RANDOM_SEED(PUT = ISEED)
  FSEED = ISEED3
  SSEED = ISEED3
  STOCHASTIC = IBUF(20) .EQ. 0
  RETURN
  END

! ==================================================================================================
  SUBROUTINE READ_RT03(STRING)
! ----------------------------------------------------------------------
! --- Time Period Specification.
! ----------------------------------------------------------------------
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(19), I, NP
! ----------------------------------------------------------------------
  READ(STRING, '(19I4)') IBUF
  SIMULATION_END = 0
  NP = 0
  DO I = 1, 19
    IF(IBUF(I) .EQ. 0) EXIT
    NP = NP + 1
    TPSECONDS(I) = IBUF(I)
  ENDDO
  IF(NP .NE. N_PERIODS) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A)') 'RT 03: Number of Time Period Durations Not Equal To Number of Time Periods'
    CALL SENDTEXTMSG(M_ERROR)
    WRITE(MSGTEXT, '(A, I2)') '  Number of time periods = ', N_PERIODS
    CALL SENDTEXTMSG(M_ERROR)
    WRITE(MSGTEXT, '(A, I2)') '  Number of Durations = ', NP
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  SIMULATION_END = SUM(IBUF)
  RETURN
  END
      

! ==================================================================================================
  SUBROUTINE READ_RT04(STRING)
! ----------------------------------------------------------------------
! --- Time Intervals.
! ----------------------------------------------------------------------
  USE SIMPARAMS
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(3)
  INTEGER :: IBHRS, IBMNS, SYNCTM
! ----------------------------------------------------------------------
  READ(STRING, '(8X,I4,I8,I8,I8)') IBUF
  IF(IBUF(1) .EQ. 0 .OR. IBUF(1) .EQ. 1) THEN
    TIMESTEP = 1.0
  ELSE
    TIMESTEP = 1.0 / IBUF(1)
  ENDIF
  TIME_INTERVAL = IBUF(2)
  IF(STRING(25:28) .NE. '    ') THEN
    SYNC_REF_TIME = IBUF(3)
    SKIP_INIT = .FALSE.                             
    IBHRS = IBUF(3) / 100                       
    IBMNS = IBUF(3) - IBHRS * 100               
    SYNCTM = IBHRS*3600 + IBMNS*60              
    IBHRS = SIM_START_TIME / 100                        
    IBMNS = SIM_START_TIME - IBHRS * 100                
    STARTM = IBHRS*3600 + IBMNS*60              
    SYNCOF = STARTM - SYNCTM                    
    IF(SYNCOF .LT. 0) SYNCOF = SYNCOF + 86400   
  ELSE
    SYNC_REF_TIME = -1
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE READ_RT05(STRING)
! ----------------------------------------------------------------------
! --- Reports.
! ----------------------------------------------------------------------
  USE SIMPARAMS
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(1)
! ----------------------------------------------------------------------
  READ(STRING, '(58X,I4)') IBUF
  WRITE_SUPPLEMENTAL_FILES = IBUF(1) .NE. 0
  RETURN
  END

! ==================================================================================================
  SUBROUTINE READ_RT11(STRING)
! ----------------------------------------------------------------------
! --- Surface Link Specification.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE CORSIM_NODES
  USE GLOBAL_DATA
  USE NODE_TABLE
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: I, IL, IBUF(30), ILANE, N, NODE, NLANE
! ----------------------------------------------------------------------
  READ(STRING(1:29), '(BZ, 5I4, 4I2, I1)') (IBUF(I), I = 1, 10)
  DO I = 30, 36
    IF(STRING(I:I) .EQ. 'D' .OR. STRING(I:I) .EQ. 'd') THEN
      IBUF(I-19) = 10
    ELSEIF(STRING(I:I) .EQ. 'T' .OR. STRING(I:I) .EQ. 't') THEN
      IBUF(I-19) = 11
    ELSE
      READ(STRING(I:I), '(BZ,I1)') IBUF(I-19)  
    ENDIF
  ENDDO
  READ(STRING(37:77), '(BZ, 8I4, I2, 3I1, I4)') (IBUF(I), I = 18, 30)
  IF(TIME_PERIOD .EQ. 1) THEN
    N_STREET_LINKS = N_STREET_LINKS + 1
    IL = N_STREET_LINKS
    SUSN(IL) = IBUF(1)
    SDSN(IL) = IBUF(2)

    DO N = 1, 2
      NODE = IBUF(N)
      IS_USED(NODE) = .TRUE.
      IF(NODE .LT. 7000) THEN
        NODE_TYPE(NODE) = NT_INTERN
        NETCODE(NODE) = I_STREET
      ELSEIF(NODE .LT. 8000) THEN
        NODE_TYPE(NODE) = NT_INTERFACE
      ELSEIF(NODE .LT. 9000) THEN
        NODE_TYPE(NODE) = NT_EXTERN
        NETCODE(NODE) = I_STREET
      ENDIF
    ENDDO
 
! --- Arrays used for TRAFVU.
    UPNOD(IL) = SUSN(IL)
    DWNOD(IL) = SDSN(IL)
       
    SLENGTH(IL) = IBUF(3)
    SNUMLANES(IL) = IBUF(6)
    NUMBER_LEFTPOCKETS(IL) = IBUF(7)
    NUMBER_RIGHTPOCKETS(IL) = IBUF(8)
    FIRST_FULL_LANE(IL) = NUMBER_RIGHTPOCKETS(IL) + 1
    LAST_FULL_LANE(IL) = NUMBER_RIGHTPOCKETS(IL) + SNUMLANES(IL)
    TOTAL_LANES(IL) = SNUMLANES(IL) + NUMBER_LEFTPOCKETS(IL) + NUMBER_RIGHTPOCKETS(IL)
    IF(TOTAL_LANES(IL) .GT. N_STREET_LANES) THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, 2I5)') 'TOO MANY LANES SPECIFIED ON LINK ', IBUF(1), IBUF(2)
      CALL SENDTEXTMSG(M_ERROR)
      WRITE(MSGTEXT, '(A, I2)') '  TOTAL NUMBER OF LANES = ', TOTAL_LANES(IL)
      CALL SENDTEXTMSG(M_ERROR)
    ENDIF
    IF(SNUMLANES(IL) .LT. 1 .OR. SNUMLANES(IL) .GT. N_STREET_LANES) THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, 2I5)') 'INVALID NUMBER OF FULL LANES ON LINK ', IBUF(1), IBUF(2)
      CALL SENDTEXTMSG(M_ERROR)
      WRITE(MSGTEXT, '(A, I2)') '  NUMBER OF FULL = ', SNUMLANES(IL)
      CALL SENDTEXTMSG(M_ERROR)
      SNUMLANES(IL) = 1
    ENDIF
    DIAG_NODE(IL) = IBUF(21)
    SALIGNMENT_LANE(IL) = IBUF(28)
    STHRU_ALIGNMENT_LANE(IL) = IBUF(29)
    NUMBER_LEFTPOCKETS(IL) = IBUF(7)
    NUMBER_RIGHTPOCKETS(IL) = IBUF(8)
    SGRADE(IL) = IBUF(9) / 100.
  ENDIF
  
  IF(IBUF(10) .NE. 0) THEN
    LINKTYPE_CODE(IL) = IBUF(10)
  ELSE
    LINKTYPE_CODE(IL) = 1
  ENDIF
  ILANE = 0
  DO I = 1, NUMBER_RIGHTPOCKETS(IL) 
    ILANE = ILANE + 1
    CHANNELIZATION(IL, ILANE) = 4
    LANE_LENGTH(IL, ILANE) = IBUF(5)
  ENDDO
  DO I = 1, SNUMLANES(IL)
    ILANE = ILANE + 1
    CHANNELIZATION(IL, ILANE) = IBUF(10 + I)
    LANE_LENGTH(IL, ILANE) = IBUF(3)
  ENDDO
  DO I = 1, NUMBER_LEFTPOCKETS(IL) 
    ILANE = ILANE + 1
    CHANNELIZATION(IL, ILANE) = 1
    LANE_LENGTH(IL, ILANE) = IBUF(4)
  ENDDO

  IF(IBUF(23) .NE. 0) SSTARTUP_TIME(IL) = IBUF(23) / 10.
  IF(SSTARTUP_TIME(IL) .LT. 0 .OR. SSTARTUP_TIME(IL) .GT. 9.9) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A, 2I5)') 'INVALID MEAN STARTUP LOST TIME ON LINK ', SUSN(IL), SDSN(IL)
    CALL SENDTEXTMSG(M_ERROR)
    WRITE(MSGTEXT, '(A, F5.1)') '  VALUE = ', SSTARTUP_TIME(IL)
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
      
  IF(IBUF(24) .NE. 0) QDISCHARGE_HDWY(IL) = IBUF(24) / 10.
  IF(IBUF(25) .NE. 0) THEN
    SFREEFLOWSPEED(IL) = IBUF(25) * MILES2FEET
  ELSE
    SFREEFLOWSPEED(IL) = 30 * MILES2FEET
  ENDIF
  IF(IBUF(26) .EQ. 1) RTOR(IL) = .FALSE.
  PED_CODE(IL) = IBUF(27)
  IF(IBUF(30) .NE. 0) SCFMULT(IL) = IBUF(30) / 100.
  
  !Define the traditional lane numbers.
  DO IL = 1, N_STREET_LINKS
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
    
! --- Create exit links if necessary.
 
  IF(TIME_PERIOD .EQ. 1) THEN
    DO I = 1, 4
      IF(IBUF(18 + I) .GE. 8000) THEN
        NODE = IBUF(18 + I)
        N_STREET_LINKS = N_STREET_LINKS + 1
        IL = N_STREET_LINKS
        SUSN(IL) = IBUF(2)
        SDSN(IL) = NODE
        FIRST_FULL_LANE(IL) = 1
        LAST_FULL_LANE(IL) = SNUMLANES(IL)
        IS_USED(NODE) = .TRUE.
        NODE_TYPE(NODE) = NT_EXTERN
        NETCODE(NODE) = I_STREET
        
! --- Arrays used for TRAFVU.
        UPNOD(IL) = SUSN(IL)
        DWNOD(IL) = SDSN(IL)
      ENDIF
    ENDDO
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE READ_RT12(STRING)
! ----------------------------------------------------------------------
! --- Extended Surface Link Specification.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE CORSIM_NODES
  USE GLOBAL_DATA
  USE NODE_TABLE
  USE SIMPARAMS
  USE TEXT
  USE TURNING_WAYS
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: I, IL, IBUF(33), ILINK
! ----------------------------------------------------------------------
  READ(STRING, '(5I4)') (IBUF(I), I = 1, 5)
  DO I = 21, 40
    IF(STRING(I:I) .EQ. 'D' .OR. STRING(I:I) .EQ. 'd') THEN
      IBUF(I-15) = 10
    ELSEIF(STRING(I:I) .EQ. 'T' .OR. STRING(I:I) .EQ. 't') THEN
      IBUF(I-15) = 11
    ELSE
      READ(STRING(I:I), '(BZ,I1)') IBUF(I-15)  
    ENDIF
  ENDDO
  CALL FIND_STREET_LINK(IBUF(1), IBUF(2), IL)
  IF(IL .EQ. 0) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A, 2I5)') 'RT 12: SPECIFIED LINK NOT FOUND ', IBUF(1), IBUF(2)
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
    WRITE12(IL) = .TRUE.
    SNUMLANES(IL) = IBUF(3)
    NUMBER_LEFTPOCKETS(IL) = IBUF(4)
    NUMBER_RIGHTPOCKETS(IL) = IBUF(5)
    TOTAL_LANES(IL) = SNUMLANES(IL) + NUMBER_LEFTPOCKETS(IL) + NUMBER_RIGHTPOCKETS(IL)
    DO I = 1, N_STREET_LANES
      CHANNELIZATION(IL, I) = IBUF(I+5)
    ENDDO
    FIRST_FULL_LANE(IL) = NUMBER_RIGHTPOCKETS(IL) + 1
    LAST_FULL_LANE(IL) = NUMBER_RIGHTPOCKETS(IL) + SNUMLANES(IL)
    READ(STRING, '(40X,2I4)') (IBUF(I), I = 26, 27)
    IF(IBUF(26) .NE. 0) THEN
      CALL FIND_STREET_LINK(IBUF(2), IBUF(26), ILINK)
      IF(ILINK .EQ. 0) THEN
        CALL NEW_ERROR
        WRITE(MSGTEXT, '(A, 2I5)') 'RT 12: SPECIFIED LEFT DIAGONAL RECEIVING LINK NOT FOUND ', IBUF(2), IBUF(26)
        CALL SENDTEXTMSG(M_ERROR)
      ELSE
        LEFT_DIAG_LINK(IL) = ILINK
      ENDIF
    ENDIF
    IF(IBUF(27) .NE. 0) THEN
      CALL FIND_STREET_LINK(IBUF(2), IBUF(27), ILINK)
      IF(ILINK .EQ. 0) THEN
        CALL NEW_ERROR
        WRITE(MSGTEXT, '(A, 2I5)') 'RT 12: SPECIFIED RIGHT DIAGONAL RECEIVING LINK NOT FOUND ', IBUF(2), IBUF(27)
        CALL SENDTEXTMSG(M_ERROR)
      ELSE
        RIGHT_DIAG_LINK(IL) = ILINK
      ENDIF
    ENDIF
    READ(STRING, '(48X,5I4)') (IBUF(I), I = 28, 32)
    IF(IBUF(28) .NE. 0 .AND. IBUF(29) .NE. 0) THEN
      IF(RIGHT_LINK(IL) .EQ. 0) THEN
        CALL NEW_ERROR
        WRITE(MSGTEXT, '(A, 2I5)') 'RT 12: ISOLATED RIGHT TURN LANE SPECIFIED WITH NO RIGHT RECEIVING LINK ', IBUF(1), IBUF(2)
        CALL SENDTEXTMSG(M_ERROR)
      ELSE
        IF(IBUF(31) + IBUF(32) .NE. 0) THEN
          CALL FIND_STREET_LINK(IBUF(31), IBUF(32), ILINK)
        ELSE
          ILINK = RIGHT_LINK(IL)
        ENDIF
        IF(ILINK .EQ. 0) THEN
          CALL NEW_ERROR
          WRITE(MSGTEXT, '(A, 2I5)') 'RT 12: TURNING WAY RECEIVING LINK NOT FOUND ', IBUF(1), IBUF(2)
          CALL SENDTEXTMSG(M_ERROR)
        ELSE
          NUMBER_OF_TURNING_WAYS = NUMBER_OF_TURNING_WAYS + 1
          NUMBER_TURNINGWAYS(IL) = NUMBER_TURNINGWAYS(IL) + 1
          RTW_RECEIVING_LINK(IL) = ILINK
          RTW_EXIT_POINT(IL) = IBUF(28)
          RTW_ENTRY_POINT(RTW_RECEIVING_LINK(IL)) = IBUF(29)
          RTW_LENGTH(IL) = IBUF(30)
          IF(IBUF(31) .NE. 0) THEN
            RTW_CONTROL_CODE(IL) = IBUF(31)
          ELSE
            RTW_CONTROL_CODE(IL) = 31
          ENDIF
          IF(IBUF(32) .NE. 0) THEN
            RTW_FFSPEED(IL) = IBUF(32)
          ELSE
            RTW_FFSPEED(IL) = SFREEFLOWSPEED(IL)
          ENDIF
          IF(IBUF(33) .NE. 0) THEN
            RTW_LANES(IL) = IBUF(33)
          ELSE
            RTW_LANES(IL) = 1
          ENDIF
          CALL SHIFT_LANES_RTW(IL)
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END 
  
! ==================================================================================================
  SUBROUTINE READ_RT13(STRING)
! ----------------------------------------------------------------------
! --- Turn Pocket Lengths.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE NODE_TABLE
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: I, IL, IBUF(12), N
! ----------------------------------------------------------------------
  READ(STRING, '(12I4)') IBUF
  CALL FIND_STREET_LINK(IBUF(1), IBUF(2), IL)
  IF(IL .EQ. 0) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A, 2I5)') 'RT 13: SPECIFIED LINK NOT FOUND ', IBUF(1), IBUF(2)
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
    WRITE13(IL) = .TRUE.
    N = 0
    DO I = NUMBER_RIGHTPOCKETS(IL), 1, -1
      N = N + 1
      LANE_LENGTH(IL, I) = IBUF(2 + N)
    ENDDO
    N = 0
    DO I = LAST_FULL_LANE(IL) + 1, TOTAL_LANES(IL)
      N = N + 1
      LANE_LENGTH(IL, I) = IBUF(7 + N)
    ENDDO
  ENDIF
  RETURN
  END
   
! ==================================================================================================
  SUBROUTINE PROCESS_RT11(STRING)
! ----------------------------------------------------------------------
! --- Surface Street Links.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(29), I, IL, ILINK, SUM
  CHARACTER AI(7)
! ----------------------------------------------------------------------
  READ(STRING, '(5I4,4I2,I1,7A1,8I4,I2,3I1)') IBUF(1:10), AI(1:7), IBUF(18:29)
  DO I = 1, 7
    IF(AI(I) .EQ. 'D' .OR. AI(I) .EQ. 'd') THEN
      IBUF(10+I) = 10
    ELSEIF(AI(I) .EQ. 'T' .OR. AI(I) .EQ. 't') THEN
      IBUF(10+I) = 11
    ELSE
      READ(AI(I), '(I1)') IBUF(10+I)
    ENDIF
  ENDDO
  IF(IBUF(2) .GE. 7000) THEN
    SUM = IBUF(18) + IBUF(19) + IBUF(20) + ABS(IBUF(21))
    IF(SUM .NE. 0) THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, 2I5)') 'RT 11: RECEIVING LINK SPECIFIED FOR INTERFACE OR EXIT LINK ', IBUF(1), IBUF(2)
      CALL SENDTEXTMSG(M_ERROR)
    ENDIF
  ENDIF
  CALL FIND_STREET_LINK(IBUF(1), IBUF(2), IL)
 
! --- Store the receiving links.
       
  IF(IBUF(18) .NE. 0) THEN
    CALL FIND_STREET_LINK(IBUF(2), IBUF(18), ILINK)
    LEFT_LINK(IL) = ILINK
    IF(ILINK .EQ. 0) THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, 2I5)') 'RT 11: LEFT RECEIVING LINK DOES NOT EXIST FOR LINK ', IBUF(1), IBUF(2)
      CALL SENDTEXTMSG(M_ERROR)
      WRITE(MSGTEXT, '(A, 2I5)') '  LEFT RECEIVING LINK ', IBUF(2), IBUF(18)
      CALL SENDTEXTMSG(M_ERROR)
    ENDIF
  ENDIF
 
  IF(IBUF(19) .NE. 0) THEN
    CALL FIND_STREET_LINK(IBUF(2), IBUF(19), ILINK)
    STHRU_LINK(IL) = ILINK
    SALIGNMENT_LANE(IL) = SALIGNMENT_LANE(IL) + FIRST_FULL_LANE(IL) - 1
    STHRU_ALIGNMENT_LANE(IL) = STHRU_ALIGNMENT_LANE(IL) + FIRST_FULL_LANE(ILINK) - 1
    IF(SALIGNMENT_LANE(IL) .EQ. 0) THEN
      SALIGNMENT_LANE(IL) = SALIGNMENT_LANE(IL) + 1
      STHRU_ALIGNMENT_LANE(IL) = STHRU_ALIGNMENT_LANE(IL) + 1
    ENDIF
    IF(ILINK .EQ. 0) THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, 2I5)') 'RT 11: THRU RECEIVING LINK DOES NOT EXIST FOR LINK ', IBUF(1), IBUF(2)
      CALL SENDTEXTMSG(M_ERROR)
      WRITE(MSGTEXT, '(A, 2I5)') '  THRU RECEIVING LINK ', IBUF(2), IBUF(19)
      CALL SENDTEXTMSG(M_ERROR)
    ENDIF
  ENDIF
       
  IF(IBUF(20) .NE. 0) THEN
    CALL FIND_STREET_LINK(IBUF(2), IBUF(20), ILINK)
    RIGHT_LINK(IL) = ILINK
    IF(ILINK .EQ. 0) THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, 2I5)') 'RT 11: RIGHT RECEIVING LINK DOES NOT EXIST FOR LINK ', IBUF(1), IBUF(2)
      CALL SENDTEXTMSG(M_ERROR)
      WRITE(MSGTEXT, '(A, 2I5)') '  RIGHT RECEIVING LINK ', IBUF(2), IBUF(20)
      CALL SENDTEXTMSG(M_ERROR)
    ENDIF
  ENDIF
      
  IF(IBUF(21) .LT. 0) THEN
    CALL FIND_STREET_LINK(IBUF(2), ABS(IBUF(21)), ILINK)
    IF(ILINK .EQ. 0)  THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, 2I5)') 'RT 11: LEFT DIAGONAL RECEIVING LINK DOES NOT EXIST FOR LINK ', IBUF(1), IBUF(2)
      CALL SENDTEXTMSG(M_ERROR)
      WRITE(MSGTEXT, '(A, 2I5)') '  LEFT DIAGONAL RECEIVING LINK ', IBUF(2), IBUF(21)
      CALL SENDTEXTMSG(M_ERROR)
    ELSE
      LEFT_DIAG_LINK(IL) = ILINK
    ENDIF
      
  ELSEIF(IBUF(21) .GT. 0) THEN
    CALL FIND_STREET_LINK(IBUF(2), IBUF(21), ILINK)
    IF(ILINK .EQ. 0)  THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, 2I5)') 'RT 11: RIGHT DIAGONAL RECEIVING LINK DOES NOT EXIST FOR LINK ', IBUF(1), IBUF(2)
      CALL SENDTEXTMSG(M_ERROR)
      WRITE(MSGTEXT, '(A, 2I5)') '  RIGHT DIAGONAL RECEIVING LINK ', IBUF(2), IBUF(21)
      CALL SENDTEXTMSG(M_ERROR)
    ELSE
      RIGHT_DIAG_LINK(IL) = ILINK
    ENDIF
  ENDIF
      
  IF(IBUF(22) .NE. 0) THEN
    CALL FIND_STREET_LINK(IBUF(22), IBUF(2), ILINK)
    OPPOSE_LINK(IL) = ILINK
    IF(ILINK .EQ. 0) THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, 2I5)') 'RT 11: LINK OPPOSING LEFT TURNS DOES NOT EXIST FOR LINK ', IBUF(1), IBUF(2)
      CALL SENDTEXTMSG(M_ERROR)
      WRITE(MSGTEXT, '(A, 2I5)') '  OPPOSING LINK ', IBUF(2), IBUF(22)
      CALL SENDTEXTMSG(M_ERROR)
    ENDIF
  ENDIF
  
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE READ_RT19(STRING)
! ----------------------------------------------------------------------
! --- Freeway Link Geometry.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  USE FREEWAY_NODES
  USE CORSIM_NODES
  USE GLOBAL_DATA
  USE NODE_TABLE
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(29), IAUX, IL, ILX, N, NODE
! ----------------------------------------------------------------------
  READ(STRING, '(3I4,I5,I1,I2,3(I2,I1,I5),4I2,3(I2,I1,I5),I1)') IBUF
  N_FREEWAY_LINKS = N_FREEWAY_LINKS + 1
  IL = N_FREEWAY_LINKS
  FUSN(IL) = IBUF(1)
  FDSN(IL) = IBUF(2)
  
  DO N = 1, 2
    NODE = IBUF(N)
    IS_USED(NODE) = .TRUE.
    IF(NODE .LT. 7000) THEN
      NODE_TYPE(NODE) = NT_INTERN
      NETCODE(NODE) = I_FREEWAY
    ELSEIF(NODE .LT. 8000) THEN
      NODE_TYPE(NODE) = NT_INTERFACE
    ELSEIF(NODE .LT. 9000) THEN
      NODE_TYPE(NODE) = NT_EXTERN
      NETCODE(NODE) = I_FREEWAY
    ENDIF
  ENDDO

  ! --- Arrays used for TRAFVU.
  UPNODE(IL) = FUSN(IL)
  DWNODE(IL) = FDSN(IL)
 
  FLENGTH(IL) = IBUF(4)
  LINKTYPE(IL) = IBUF(5)
  FNUMLANES(IL) = IBUF(6)
  IF(IBUF(16) .GT. 5 .AND. IBUF(16) .LT. 9) THEN
    IBUF(16) = IBUF(16) + 5
  ELSEIF(IBUF(16) .GT. 5 .AND. IBUF(16) .GE. 9) THEN
    IBUF(16) = IBUF(16) + 7
  ENDIF
  IF(IBUF(17) .GT. 5 .AND. IBUF(17) .LT. 9) THEN
    IBUF(17) = IBUF(17) + 5
  ELSEIF(IBUF(17) .GT. 5 .AND. IBUF(17) .GE. 9) THEN
    IBUF(17) = IBUF(17) + 7
  ENDIF
  MAINLINE_SENDING_LANE(IL) = 1
  MAINLINE_RECEIVING_LANE(IL) = IBUF(16)
  OFFRAMP_SENDING_LANE(IL) = IBUF(17)
  OFFRAMP_RECEIVING_LANE(IL) = 1
 
! --- Process auxiliary lane inputs.
 
  IAUX = 0
  IF(IBUF(7) .NE. 0) THEN
    IAUX = IAUX + 1
    IF(IBUF(7) .LT. 9) THEN
      IBUF(7) = IBUF(7) + 5
    ELSE
      IBUF(7) = IBUF(7) + 7
    ENDIF
    AUX_LANE_ID(IL, IAUX) = IBUF(7)
    AUX_LANE_CODE(IL, IAUX) = IBUF(8)
    AUX_LANE_LENGTH(IL, IAUX) = IBUF(9)
  ENDIF
  IF(IBUF(10) .NE. 0) THEN
    IF(IBUF(10) .LT. 9) THEN
      IBUF(10) = IBUF(10) + 5
    ELSE
      IBUF(10) = IBUF(10) + 7
    ENDIF
    IAUX = IAUX + 1
    AUX_LANE_ID(IL, IAUX) = IBUF(10)
    AUX_LANE_CODE(IL, IAUX) = IBUF(11)
    AUX_LANE_LENGTH(IL, IAUX) = IBUF(12)
  ENDIF
  IF(IBUF(13) .NE. 0) THEN
    IAUX = IAUX + 1
    IF(IBUF(13) .LT. 9) THEN
      IBUF(13) = IBUF(13) + 5
    ELSE
      IBUF(13) = IBUF(13) + 7
    ENDIF
    AUX_LANE_ID(IL, IAUX) = IBUF(13)
    AUX_LANE_CODE(IL, IAUX) = IBUF(14)
    AUX_LANE_LENGTH(IL, IAUX) = IBUF(15)
  ENDIF
  IF(IBUF(20) .NE. 0) THEN
    IAUX = IAUX + 1
    IF(IBUF(20) .LT. 9) THEN
      IBUF(20) = IBUF(20) + 5
    ELSE
      IBUF(20) = IBUF(20) + 7
    ENDIF
    AUX_LANE_ID(IL, IAUX) = IBUF(20)
    AUX_LANE_CODE(IL, IAUX) = IBUF(21)
    AUX_LANE_LENGTH(IL, IAUX) = IBUF(22)
  ENDIF
  IF(IBUF(23) .NE. 0) THEN
    IF(IBUF(23) .LT. 9) THEN
      IBUF(23) = IBUF(23) + 5
    ELSE
      IBUF(23) = IBUF(23) + 7
    ENDIF
    AUX_LANE_ID(IL, IAUX) = IBUF(23)
    AUX_LANE_CODE(IL, IAUX) = IBUF(24)
    AUX_LANE_LENGTH(IL, IAUX) = IBUF(25)
  ENDIF
  IF(IBUF(26) .NE. 0) THEN
    IAUX = IAUX + 1
    IF(IBUF(26) .LT. 9) THEN
      IBUF(26) = IBUF(26) + 5
    ELSE
      IBUF(26) = IBUF(26) + 7
    ENDIF
    AUX_LANE_ID(IL, IAUX) = IBUF(26)
    AUX_LANE_CODE(IL, IAUX) = IBUF(27)
    AUX_LANE_LENGTH(IL, IAUX) = IBUF(28)
  ENDIF

  RAMP_MERGE_LINK(IL) = IBUF(29) .EQ. 1
  IF(RAMP_MERGE_LINK(IL)) MERGE_APPROACH(FDSN(IL)) = IL
  DIVERGE_LINK(IL) = IBUF(29) .EQ. 2
  
  IF(IBUF(18) .NE. 0) THEN
    IF(IBUF(18) .GE. 6 .AND. IBUF(18) .LE. 8) THEN
      IBUF(18) = IBUF(18) + 5
    ELSEIF(IBUF(18) .GE. 9 .AND. IBUF(18) .LE. 11) THEN
      IBUF(18) = IBUF(18) + 7
    ENDIF
    BARRIER(IL, 1) = IBUF(18)
  ENDIF
  IF(IBUF(19) .NE. 0) THEN
    IF(IBUF(19) .GE. 6 .AND. IBUF(19) .LE. 8) THEN
      IBUF(19) = IBUF(18) + 5
    ELSEIF(IBUF(19) .GE. 9 .AND. IBUF(19) .LE. 11) THEN
      IBUF(19) = IBUF(19) + 7
    ENDIF
    BARRIER(IL, 2) = IBUF(19)
  ENDIF
 
! --- Create an exit link if necessary.
 
  IF(IBUF(3) .GE. 8000) THEN
    NODE = IBUF(3)
    N_FREEWAY_LINKS = N_FREEWAY_LINKS + 1
    ILX = N_FREEWAY_LINKS
    FUSN(ILX) = IBUF(2)
    FDSN(ILX) = NODE
    IS_USED(NODE) = .TRUE.
    NODE_TYPE(NODE) = NT_EXTERN
    NETCODE(NODE) = I_FREEWAY
    MAINLINE_SENDING_LANE(IL) = 1
    OFFRAMP_RECEIVING_LANE(IL) = 1
    
! --- Arrays used for TRAFVU.
    UPNODE(ILX) = FUSN(ILX)
    DWNODE(ILX) = FDSN(ILX)
 
    LINKTYPE(ILX) = IBUF(5)
    FNUMLANES(ILX) = IBUF(6)
    DO IAUX = 1, N_AUXLANES                           
      IF(AUX_LANE_ID(IL, IAUX) .EQ. 0) EXIT
      IF(AUX_LANE_CODE(IL, IAUX) .NE. AUX_DECEL) THEN
        FNUMLANES(ILX) = FNUMLANES(ILX) + 1
      ENDIF
    ENDDO
  ENDIF
  RETURN
  END 
         
! ==================================================================================================
  SUBROUTINE PROCESS_RT19(STRING)
! ----------------------------------------------------------------------
! --- Freeway Link Geometry.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  USE FREEWAY_NODES
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(28), IL, ITHRU
! ----------------------------------------------------------------------
  READ(STRING, '(3I4,I5,I1,I2,3(I2,I1,I5),4I2,3(I2,I1,I5))') IBUF
  CALL FIND_FREEWAY_LINK(IBUF(1), IBUF(2), IL)
  CALL FIND_FREEWAY_LINK(IBUF(2), IBUF(3), ITHRU)
  IF(IBUF(3) .NE. 0 .AND. ITHRU .EQ. 0) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A, 2I5)') 'RT 19: THRU RECEIVING LINK DOES NOT EXIST FOR LINK ', IBUF(1), IBUF(2)
    CALL SENDTEXTMSG(M_ERROR)
    WRITE(MSGTEXT, '(A, I5)') '  DOWNSTREAM NODE = ', IBUF(3)
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  IF(ITHRU .NE. 0) THEN
    FTHRU_LINK(IL) = ITHRU
    IF(LINKTYPE(IL) .EQ. 0 .AND. .NOT. RAMP_MERGE_LINK(IL)) THEN
      MAINLINE_APPROACH(IBUF(2)) = IL
    ELSEIF(LINKTYPE(IL) .EQ. 1) THEN
      RAMP_APPROACH(IBUF(2)) = IL
    ENDIF
  ENDIF
  RETURN
  END 

! ==================================================================================================
  SUBROUTINE READ_RT20(STRING)
! ----------------------------------------------------------------------
! --- Freeway Link Operation.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  USE DATASTATIONS
  USE SIMPARAMS
  USE TEXT
  USE GLOBAL_DATA
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(18), IL 
! ----------------------------------------------------------------------
  READ(STRING, '(2I4,2I2,I4,6I2,4I5,12X,2I4,I4)') IBUF
  CALL FIND_FREEWAY_LINK(IBUF(1), IBUF(2), IL)
  IF(IL .EQ. 0) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A, 2I5)') 'RT 20: OPERATIONAL DATA ENTERED FOR A LINK THAT DOES NOT EXIST ', IBUF(1), IBUF(2)
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
 
! --- store the link grade, superelevation and radius of curvature
 
    FGRADE(IL) = IBUF(3) / 100.
    IF(FGRADE(IL) .LT. -9) THEN
      FGRADE(IL) = -9
    ELSEIF(FGRADE(IL) .GT. 9) THEN
      FGRADE(IL) = 9
    ENDIF
    TILT(IL) = IBUF(4) / 100.
    CURVE(IL) = IBUF(5)
 
! --- store the link pavement code, mean queue headway and desired free-flow speed
 
    IF(IBUF(6) .NE. 0) PAVEMENT(IL) = IBUF(6)
 
    IF(IBUF(7) .NE. 0) THEN
      FSTARTUP_TIME(IL) = IBUF(7) / 10.
    ENDIF
    IF(IBUF(8) .GT. 0) THEN
      FFREEFLOWSPEED(IL) = IBUF(8) * MILES2FEET
    ELSE
      FFREEFLOWSPEED(IL) = 65 * MILES2FEET
    ENDIF
 
! --- Store the truck movement code and number of through lanes to
! --- which the trucks are restricted.
 
    TRUCK_CODE(IL) = IBUF(9)
    TRUCK_LANE(IL) = IBUF(10)
    TRUCK_DIR(IL) = IBUF(11)
       
    IF(OFFRAMP_SENDING_LANE(IL) .NE. 0 .AND. IBUF(12) .EQ. 0) THEN
      OFFRAMP_WARN_DISTANCE(IL) = 2500
    ELSE
      OFFRAMP_WARN_DISTANCE(IL) = IBUF(12)
    ENDIF
    
    IF(IBUF(13) .NE. 0) THEN
      NUMBER_OF_DATASTATIONS = NUMBER_OF_DATASTATIONS + 1
      DATASTATION_ID(IL) = NUMBER_OF_DATASTATIONS
      DATASTATION(NUMBER_OF_DATASTATIONS)%LOCATION = IBUF(13)
    ENDIF
    IF(IBUF(14) .NE. 0) THEN
      HOV_OFFRAMP_WARN_DISTANCE(IL) = IBUF(14) 
    ELSE
      HOV_OFFRAMP_WARN_DISTANCE(IL) = OFFRAMP_WARN_DISTANCE(IL)
    ENDIF

    IF(IBUF(15) .EQ. 0) THEN
      IF(IBUF(9) .NE. 0) ETL_WARN(IL) = 5280
    ELSE
      ETL_WARN(IL) = IBUF(15)
    ENDIF
    IF(IBUF(16) .EQ. 0) THEN
      ANTICIP_WARNING_SPEED(IL) = 2. / 3. * FFREEFLOWSPEED(IL)
    ELSE
      ANTICIP_WARNING_SPEED(IL) = IBUF(16) * MILES2FEET
    ENDIF
    IF(IBUF(17) .NE. 0) ANTICIP_WARNING_DISTANCE(IL) = IBUF(17)
    IF(IBUF(18) .NE. 0) FCFMULT(IL) = IBUF(18) / 100.
  ENDIF
  RETURN
  END 
           
! ==================================================================================================
  SUBROUTINE READ_RT21(STRING)
! ----------------------------------------------------------------------
! --- Surface Street Turn Movements.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(7), IL, TOTAL, ERROR
! ----------------------------------------------------------------------
  READ(STRING, '(7I4)') IBUF
  IF(IBUF(2) .GE. 8000) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A, 2I5)') 'RT 21: TURN PERCENTAGES ENTERED FOR AN EXIT LINK ', IBUF(1), IBUF(2)
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
    CALL FIND_STREET_LINK(IBUF(1), IBUF(2), IL)
    IF(IL .EQ. 0) THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, 2I5)') 'RT 21: TURN PERCENTAGES ENTERED FOR A LINK THAT DOES NOT EXIST ', IBUF(1), IBUF(2)
      CALL SENDTEXTMSG(M_ERROR)
    ELSE
      LEFT_PERCENT(IL) = IBUF(3)
      STHRU_PERCENT(IL) = IBUF(4)
      RIGHT_PERCENT(IL) = IBUF(5)
      
      IF(DIAG_NODE(IL) .EQ. 0) THEN
        LDIAG_PERCENT(IL) = IBUF(6)
        RDIAG_PERCENT(IL) = IBUF(7)
      ELSEIF(DIAG_NODE(IL) .LT. 0) THEN
        LDIAG_PERCENT(IL) = IBUF(6)
      ELSEIF(DIAG_NODE(IL) .GT. 0) THEN
        RDIAG_PERCENT(IL) = IBUF(6)
      ENDIF
      
      TOTAL = IBUF(3) + IBUF(4) + IBUF(5) + IBUF(6) + IBUF(7)
 
  ! --- Convert to percentages if necessary.
 
      IF(TOTAL .EQ. 0.) THEN
        CALL NEW_ERROR
        WRITE(MSGTEXT, '(A, 2I5)') 'RT 21: INVALID TURN PERCENTAGES ENTERED FOR LINK ', IBUF(1), IBUF(2)
        CALL SENDTEXTMSG(M_ERROR)
      ELSEIF(TOTAL .NE. 100) THEN
        LEFT_PERCENT(IL) = 100 * LEFT_PERCENT(IL) / TOTAL
        STHRU_PERCENT(IL) = 100 * STHRU_PERCENT(IL) / TOTAL
        RIGHT_PERCENT(IL) = 100 * RIGHT_PERCENT(IL) / TOTAL
        LDIAG_PERCENT(IL) = 100 * LDIAG_PERCENT(IL) / TOTAL
        RDIAG_PERCENT(IL) = 100 * RDIAG_PERCENT(IL) / TOTAL
        ERROR = 100 - LEFT_PERCENT(IL) - STHRU_PERCENT(IL) - RIGHT_PERCENT(IL) - LDIAG_PERCENT(IL) - RDIAG_PERCENT(IL)
        IF(ERROR .NE. 0) THEN
          IF(IBUF(3) .NE. 0) THEN
            LEFT_PERCENT(IL) = LEFT_PERCENT(IL) + ERROR
          ELSEIF(IBUF(4) .NE. 0) THEN
            STHRU_PERCENT(IL) = STHRU_PERCENT(IL) + ERROR
          ELSEIF(IBUF(5) .NE. 0) THEN
            RIGHT_PERCENT(IL) = RIGHT_PERCENT(IL) + ERROR
          ELSEIF(IBUF(6) .NE. 0) THEN
            LDIAG_PERCENT(IL) = LDIAG_PERCENT(IL) + ERROR
          ELSEIF(IBUF(7) .NE. 0) THEN
            RDIAG_PERCENT(IL) = RDIAG_PERCENT(IL) + ERROR
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE READ_RT22(STRING)
! ----------------------------------------------------------------------
! --- Conditional Surface Street Turn Movements.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE GLOBAL_DATA
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(18), IL
! ----------------------------------------------------------------------
  READ(STRING, '(18I4)') IBUF
  CALL FIND_STREET_LINK(IBUF(1), IBUF(2), IL)
  IF(IBUF(2) .GE. 8000) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A, 2I5)') 'RT 22: CONDITIONAL TURN PERCENTAGES ENTERED FOR AN EXIT LINK ', IBUF(1), IBUF(2)
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
    IF(IL .EQ. 0) THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, 2I5)') 'RT 22: CONDITIONAL TURN PERCENTAGES ENTERED FOR A LINK THAT DOES NOT EXIST ', IBUF(1), IBUF(2)
      CALL SENDTEXTMSG(M_ERROR)
    ELSE
      IF(IBUF(3) + IBUF(4) + IBUF(5) + IBUF(6) .GT. 0) THEN
        COND_LEFT(IL) = .TRUE.
        COND_LEFTPCT(IL, 1) = IBUF(3)
        COND_LEFTPCT(IL, 2) = IBUF(4)
        COND_LEFTPCT(IL, 3) = IBUF(5)
        COND_LEFTPCT(IL, 4) = IBUF(6)
      ENDIF
      IF(IBUF(7) + IBUF(8) + IBUF(9) + IBUF(10) .GT. 0) THEN
        COND_THRU(IL) = .TRUE.
        COND_THRUPCT(IL, 1) = IBUF(7)
        COND_THRUPCT(IL, 2) = IBUF(8)
        COND_THRUPCT(IL, 3) = IBUF(9)
        COND_THRUPCT(IL, 4) = IBUF(10)
      ENDIF
      IF(IBUF(11) + IBUF(12) + IBUF(13) + IBUF(14) .GT. 0) THEN
        COND_RIGHT(IL) = .TRUE.
        COND_RIGHTPCT(IL, 1) = IBUF(11)
        COND_RIGHTPCT(IL, 2) = IBUF(12)
        COND_RIGHTPCT(IL, 3) = IBUF(13)
        COND_RIGHTPCT(IL, 4) = IBUF(14)
      ENDIF
      IF(IBUF(15) + IBUF(16) + IBUF(17) + IBUF(18) .GT. 0) THEN
        IF(DIAG_NODE(IL) .LT. 0) THEN
          COND_LDIAG(IL) = .TRUE.
          COND_LDIAGPCT(IL, 1) = IBUF(15)
          COND_LDIAGPCT(IL, 2) = IBUF(16)
          COND_LDIAGPCT(IL, 3) = IBUF(17)
          COND_LDIAGPCT(IL, 4) = IBUF(18)
        ELSEIF(DIAG_NODE(IL) .GT. 0) THEN
          COND_RDIAG(IL) = .TRUE.
          COND_RDIAGPCT(IL, 1) = IBUF(15)
          COND_RDIAGPCT(IL, 2) = IBUF(16)
          COND_RDIAGPCT(IL, 3) = IBUF(17)
          COND_RDIAGPCT(IL, 5) = IBUF(18)
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END 
      
! ==================================================================================================
  SUBROUTINE READ_RT24(STRING)
! ----------------------------------------------------------------------
! --- Surface Street Turn Movement Multipliers.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  USE STREET_LINKS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(8), IL, ITYP
! ----------------------------------------------------------------------
  READ(STRING, '(8I4)') IBUF
  ITYP = IBUF(3)
  CALL FIND_FREEWAY_LINK(IBUF(1), IBUF(2), IL)
  IF(IL .NE. 0) THEN
    MULTIPLIER_EXIT(IL, ITYP) = IBUF(4) / 100.
  ELSE
    CALL FIND_STREET_LINK(IBUF(1), IBUF(2), IL)
    IF(IL .EQ. 0) THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, 2I5)') 'RT 24: TURN MULTIPLIERS ENTERED FOR A LINK THAT DOES NOT EXIST ', IBUF(1), IBUF(2)
      CALL SENDTEXTMSG(M_ERROR)
    ELSE
      MULTIPLIER_LEFT(IL, ITYP) = IBUF(4) / 100.
      MULTIPLIER_THRU(IL, ITYP) = IBUF(5) / 100.
      MULTIPLIER_RIGHT(IL, ITYP) = IBUF(6) / 100.
      MULTIPLIER_LDIAG(IL, ITYP) = IBUF(7) / 100.
      MULTIPLIER_RDIAG(IL, ITYP) = IBUF(8) / 100.
    ENDIF
  ENDIF
  RETURN
  END 

! ==================================================================================================
  SUBROUTINE READ_RT25(STRING)
! ----------------------------------------------------------------------
! --- Freeway Turn Movements.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(6), IL, IRAMP
  REAL :: PCT
! ----------------------------------------------------------------------
  READ(STRING, '(6I4)') IBUF
  IF(IBUF(5) .NE. 0) THEN
    CALL FIND_FREEWAY_LINK(IBUF(1), IBUF(2), IL)
    CALL FIND_FREEWAY_LINK(IBUF(2), IBUF(5), IRAMP)
    IF(IRAMP .EQ. 0) THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, 2I5)') 'RT 25: OFF-RAMP LINK DOES NOT EXIST FOR LINK ', IBUF(1), IBUF(2)
      CALL SENDTEXTMSG(M_ERROR)
      WRITE(MSGTEXT, '(A, 2I5)') '  OFF-RAMP LINK ', IBUF(2), IBUF(3)
      CALL SENDTEXTMSG(M_ERROR)
    ELSEIF(IRAMP .NE. 0) THEN
      OFFRAMP_LINK(IL) = IRAMP
      IF(IBUF(4) .LT. 0 .OR. IBUF(6) .LT. 0) THEN
        CALL NEW_ERROR
        WRITE(MSGTEXT, '(A, 2I5)') 'RT 25: NEGATIVE ENTRY FOR EXIT OR THRU PERCENTAGE ON LINK ', IBUF(1), IBUF(2)
        CALL SENDTEXTMSG(M_ERROR)
        WRITE(MSGTEXT, '(A, 2I5)') '  PERCENTAGES = ', IBUF(4), IBUF(6)
        CALL SENDTEXTMSG(M_ERROR)
      ELSE
        PCT = FLOAT(IBUF(6)) / (IBUF(4) + IBUF(6))
        FTHRU_PERCENT(IL) = 100 - INT(100 * PCT)
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END 

! ==================================================================================================
  SUBROUTINE READ_RT27(STRING)
! ----------------------------------------------------------------------
! --- Conditional Surface Street Turn Movements for Enhanced Diagonals.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE GLOBAL_DATA
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(8), IL, I, ERROR
  REAL :: TOTAL
! ----------------------------------------------------------------------
  READ(STRING, '(8I4)') IBUF
  CALL FIND_STREET_LINK(IBUF(1), IBUF(2), IL)
  IF(IBUF(2) .GE. 8000) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A, 2I5)') 'RT 27: CONDITIONAL TURN PERCENTAGES ENTERED FOR AN EXIT LINK ', IBUF(1), IBUF(2)
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
    IF(IL .EQ. 0) THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, 2I5)') 'RT 27: CONDITIONAL TURN PERCENTAGES ENTERED FOR A LINK THAT DOES NOT EXIST ', IBUF(1), IBUF(2)
      CALL SENDTEXTMSG(M_ERROR)
    ELSE
      TOTAL = SUM(IBUF(4:8))
      IF(TOTAL .EQ. 0.) THEN
        CALL NEW_ERROR
        WRITE(MSGTEXT, '(A, 2I5)') 'RT 27: INVALID TURN PERCENTAGES ENTERED FOR LINK ', IBUF(1), IBUF(2)
        CALL SENDTEXTMSG(M_ERROR)
      ELSE
        ! Convert to percentages if necessary.
        IF(TOTAL .NE. 100.) THEN
          DO I = 4, 8
            IF(IBUF(I) .EQ. 0) CYCLE
            IBUF(I) = MAX(INT(100 * IBUF(I) / TOTAL), 1)
          ENDDO
          TOTAL = SUM(IBUF(4:8))
          IF(TOTAL .NE. 100.) THEN
            IF(TOTAL .LT. 100) THEN
              ERROR = 1
            ELSE
              ERROR = -1
            ENDIF
            DO WHILE(SUM(IBUF(4:8)) .NE. 100)
              DO I = 4, 8
                IF(IBUF(I) .GT. 1) THEN
                  IBUF(I) = IBUF(I) + ERROR
                  IF(SUM(IBUF(4:8)) .EQ. 100) EXIT
                ENDIF
              ENDDO
            ENDDO
          ENDIF
        ENDIF
        IF(IBUF(3) .EQ. 1) THEN
          COND_LEFT(IL) = .TRUE.
          COND_LEFTPCT(IL, 1) = IBUF(4)
          COND_LEFTPCT(IL, 2) = IBUF(5)
          COND_LEFTPCT(IL, 3) = IBUF(6)
          COND_LEFTPCT(IL, 4) = IBUF(7)
          COND_LEFTPCT(IL, 5) = IBUF(8)
        ELSEIF(IBUF(3) .EQ. 2) THEN
          COND_THRU(IL) = .TRUE.
          COND_THRUPCT(IL, 1) = IBUF(4)
          COND_THRUPCT(IL, 2) = IBUF(5)
          COND_THRUPCT(IL, 3) = IBUF(6)
          COND_THRUPCT(IL, 4) = IBUF(7)
          COND_THRUPCT(IL, 5) = IBUF(8)
        ELSEIF(IBUF(3) .EQ. 3) THEN
          COND_RIGHT(IL) = .TRUE.
          COND_RIGHTPCT(IL, 1) = IBUF(4)
          COND_RIGHTPCT(IL, 2) = IBUF(5)
          COND_RIGHTPCT(IL, 3) = IBUF(6)
          COND_RIGHTPCT(IL, 4) = IBUF(7)
          COND_RIGHTPCT(IL, 5) = IBUF(8)
        ELSEIF(IBUF(3) .EQ. 4) THEN
          COND_LDIAG(IL) = .TRUE.
          COND_LDIAGPCT(IL, 1) = IBUF(4)
          COND_LDIAGPCT(IL, 2) = IBUF(5)
          COND_LDIAGPCT(IL, 3) = IBUF(6)
          COND_LDIAGPCT(IL, 4) = IBUF(7)
          COND_LDIAGPCT(IL, 5) = IBUF(8)
        ELSEIF(IBUF(3) .EQ. 5) THEN
          COND_RDIAG(IL) = .TRUE.
          COND_RDIAGPCT(IL, 1) = IBUF(4)
          COND_RDIAGPCT(IL, 2) = IBUF(5)
          COND_RDIAGPCT(IL, 3) = IBUF(6)
          COND_RDIAGPCT(IL, 4) = IBUF(7)
          COND_RDIAGPCT(IL, 5) = IBUF(8)
        ELSE
          CALL NEW_ERROR
          WRITE(MSGTEXT, '(A, I4)') 'RT 27: UNACCEPTABLE ENTERING DIRECTION CODE ', IBUF(3)
          CALL SENDTEXTMSG(M_ERROR)
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END 
      
! ==================================================================================================
  SUBROUTINE READ_RT28(STRING)
! ----------------------------------------------------------------------
! --- Freeway Detector Specification.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  USE FREEWAY_DETECTORS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(8), I, IL, IDET
! ----------------------------------------------------------------------
  READ(STRING, '(8I4)') IBUF
  CALL FIND_FREEWAY_LINK(IBUF(1), IBUF(2), IL)
  IF(IL .EQ. 0) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A, 2I5)') 'RT 28: DETECTOR SPECIFIED ON A LINK THAT DOES NOT EXIST ', IBUF(1), IBUF(2)
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
    N_FREEWAY_DETECTORS = N_FREEWAY_DETECTORS + 1
    FDETECTOR(N_FREEWAY_DETECTORS)%LINK = IL
    FDETECTOR(N_FREEWAY_DETECTORS)%LANE1 = IBUF(3)
    FDETECTOR(N_FREEWAY_DETECTORS)%LOCATION = IBUF(4)
    FDETECTOR(N_FREEWAY_DETECTORS)%ZONE_LENGTH = MAX(1, IBUF(5))
    FDETECTOR(N_FREEWAY_DETECTORS)%TYPE_CODE = 0
    FDETECTOR(N_FREEWAY_DETECTORS)%STATION_ID = IBUF(8)
    IF(FFIRST_DETECTOR(IL) .EQ. 0) THEN
      FFIRST_DETECTOR(IL) = N_FREEWAY_DETECTORS
    ELSE
      IDET = FFIRST_DETECTOR(IL)
      DO WHILE(IDET .NE. 0)
        I = IDET            
        IDET = FDETECTOR(I)%NEXT_DET
      ENDDO
      FDETECTOR(I)%NEXT_DET = N_FREEWAY_DETECTORS
    ENDIF
  ENDIF
  RETURN
  END 
      
! ==================================================================================================
  SUBROUTINE READ_RT29(STRING)
! ----------------------------------------------------------------------
! --- Freeway Incident Specification.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  USE INCIDENTS
  USE SIMPARAMS
  USE TEXT
  USE EVENTS
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(19), IL, NINC = 0
  INTEGER :: INCTYP, I, LANEDATA(11)
! ----------------------------------------------------------------------
  READ(STRING, '(2I4,11(1X,I1),2X,I5,2X,I5,I7,I5,4X,I4,I5)') IBUF
  CALL FIND_FREEWAY_LINK(IBUF(1), IBUF(2), IL)
  IF(IL .EQ. 0) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A, 2I5)') 'RT 29: INCIDENT SPECIFIED ON A LINK THAT DOES NOT EXIST ', IBUF(1), IBUF(2)
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
    CALL NEW_EVENT
    NINC = NINC + 1
    INCIDENT_LINK(NINC) = IL
    INCIDENT_CODE(NINC, 1) = IBUF(3)
    INCIDENT_CODE(NINC, 2) = IBUF(4)
    INCIDENT_CODE(NINC, 3) = IBUF(5)
    INCIDENT_CODE(NINC, 4) = IBUF(6)
    INCIDENT_CODE(NINC, 5) = IBUF(7)
    INCIDENT_CODE(NINC, 13) = IBUF(8)
    INCIDENT_CODE(NINC, 12) = IBUF(9)
    INCIDENT_CODE(NINC, 11) = IBUF(10)
    INCIDENT_CODE(NINC, 18) = IBUF(11)
    INCIDENT_CODE(NINC, 17) = IBUF(12)
    INCIDENT_CODE(NINC, 16) = IBUF(13)
    INCIDENT_BEGIN_POINT(NINC) = IBUF(14)
    INCIDENT_END_POINT(NINC) = IBUF(14) + IBUF(15)
    INCIDENT_BEGIN_TIME(NINC) = IBUF(16)
    INCIDENT_END_TIME(NINC) = IBUF(16) + IBUF(17)
    INCIDENT_RBNF(NINC) = IBUF(18)
    IF(IBUF(19) .EQ. 0) IBUF(19) = 1500
    INCIDENT_WARN_POINT(NINC) = IBUF(19)
    INCTYP = 1                                  
    DO I = 1, 11
      LANEDATA(I) = IBUF(I + 2)
    ENDDO
    CALL ADDFREEWAYEVENT(IL, IBUF(16), IBUF(17), INCTYP, LANEDATA, IBUF(14), IBUF(15), IBUF(19), IBUF(18))
  ENDIF
  RETURN
  END 

! ==================================================================================================
  SUBROUTINE READ_RT30(STRING)
! ----------------------------------------------------------------------
! --- Freeway Diversion Specification.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  USE DIVERSIONS
  USE GLOBAL_DATA
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(8), IL, ND
! ----------------------------------------------------------------------
  READ(STRING, '(2I4, 3I7, 3I3)') IBUF
  CALL FIND_FREEWAY_LINK(IBUF(1), IBUF(2), IL)
  IF(IL .NE. 0) THEN
    ND = ND + 1
    DIVERSION_LINK(ND) = IL
    DIVERSION_LOCATION(ND) = IBUF(3)   
    DIVERSION_BEGIN_TIME(ND) = IBUF(4)
    DIVERSION_END_TIME(ND) = IBUF(4) + IBUF(5)
    DIVERSION_PERCENTAGE(ND) = IBUF(6)
    DIVERSION_PATHID(ND) = IBUF(7)
    DIVERSION_SPEED(ND) = IBUF(8) * MILES2FEET
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE READ_RT32(STRING)
! ----------------------------------------------------------------------
! --- Freeway Lane Add or Drop.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(14), IL
! ----------------------------------------------------------------------
  READ(STRING, '(2I4,3X,3(I1,1X,I1,2X,I5,I5,5X))') IBUF
  CALL FIND_FREEWAY_LINK(IBUF(1), IBUF(2), IL)
  IF(IL .NE. 0) THEN
    IF(IBUF(3) .NE. 0) THEN
      IF(IBUF(3) .EQ. 1) THEN
        ADDDROP_CODE(IL, 1) = I_ADD
      ELSEIF(IBUF(3) .EQ. 2) THEN
        ADDDROP_CODE(IL, 1) = I_DROP
      ENDIF
      ADDDROP_LANE(IL, 1) = IBUF(4)
      ADDDROP_DIST(IL, 1) = IBUF(5)
      IF(IBUF(6) .NE. 0) ADDDROP_WARN(IL, 1) = IBUF(6)
    ENDIF
    IF(IBUF(7) .NE. 0) THEN
      IF(IBUF(7) .EQ. 1) THEN
        ADDDROP_CODE(IL, 2) = I_ADD
      ELSEIF(IBUF(7) .EQ. 2) THEN
        ADDDROP_CODE(IL, 2) = I_DROP
      ENDIF
      ADDDROP_LANE(IL, 2) = IBUF(8)
      ADDDROP_DIST(IL, 2) = IBUF(9)
      IF(IBUF(10) .NE. 0) ADDDROP_WARN(IL, 2) = IBUF(10)
    ENDIF
    IF(IBUF(11) .NE. 0) THEN
      IF(IBUF(11) .EQ. 1) THEN
        ADDDROP_CODE(IL, 3) = I_ADD
      ELSEIF(IBUF(11) .EQ. 2) THEN
        ADDDROP_CODE(IL, 3) = I_DROP
      ENDIF
      ADDDROP_LANE(IL, 3) = IBUF(12)
      ADDDROP_DIST(IL, 3) = IBUF(13)
      IF(IBUF(14) .NE. 0) ADDDROP_WARN(IL, 3) = IBUF(14)
    ENDIF
  ENDIF
  RETURN
  END 

! ==================================================================================================
  SUBROUTINE READ_RT33(STRING)
! ----------------------------------------------------------------------
! --- Freeway HOV Lane Specification.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  USE GLOBAL_DATA
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(10), IL, I, LANE_IDS(N_FREEWAY_LANES), IAUX, ILANE, TOTLAN
! ----------------------------------------------------------------------
  READ(STRING, '(6I4,1X,3I5,6X,I4)') IBUF
  CALL FIND_FREEWAY_LINK(IBUF(1), IBUF(2), IL)
  IF(IL .NE. 0) THEN
    IF(TIME_PERIOD .EQ. 1) THEN
      IF(.NOT. ALLOCATED(HOV_SIDE)) THEN
        ALLOCATE(HOV_SIDE(N_FREEWAY_LINKS))
        ALLOCATE(HOV_TYPE(N_FREEWAY_LINKS))
        ALLOCATE(HOV_CODE(N_FREEWAY_LINKS))
        ALLOCATE(HOV_BEGIN(N_FREEWAY_LINKS))
        ALLOCATE(HOV_END(N_FREEWAY_LINKS))
        ALLOCATE(HOV_WARN(N_FREEWAY_LINKS))
        ALLOCATE(HOV_PCT(N_FREEWAY_LINKS))
        ALLOCATE(HOV_LANES(N_FREEWAY_LINKS, 3))
      ENDIF
      IF(IL .NE. 0) THEN
        NHOV_LANES(IL) = IBUF(3)
        IF(NHOV_LANES(IL) .GT. 3) THEN
          NHOV_LANES(IL) = 3
          CALL NEW_ERROR
          WRITE(MSGTEXT, '(A, 2I5)') 'RT 33: INVALID NUMBER OF HOV LANES SPECIFIED ON LINK', IBUF(1), IBUF(2)
          CALL SENDTEXTMSG(M_ERROR)
          WRITE(MSGTEXT, '(A, 2I5)') '  NUMBER OF HOV LANES = ', IBUF(3)
          CALL SENDTEXTMSG(M_ERROR)
        ENDIF
        IF(IBUF(4) .NE. 0) THEN
          HOV_SIDE(IL) = IBUF(4)
        ELSE
          HOV_SIDE(IL) = 0
        ENDIF
        IF(IBUF(5) .NE. 0) THEN
          HOV_TYPE(IL) = IBUF(5)
        ELSE
          HOV_TYPE(IL) = 0
        ENDIF
        IF(IBUF(6) .NE. 0) THEN
          HOV_CODE(IL) = IBUF(6)
        ELSE
          HOV_CODE(IL) = 0
        ENDIF
        IF(IBUF(7) .NE. 0) THEN
          HOV_BEGIN(IL) = IBUF(7)
        ELSE
          HOV_BEGIN(IL) = 0
        ENDIF
        IF(IBUF(8) .NE. 0) THEN
          HOV_END(IL) = IBUF(8)
        ELSE
          HOV_END(IL) = FLENGTH(IL)
        ENDIF
        IF(IBUF(9) .NE. 0) THEN
          HOV_WARN(IL) = IBUF(9)
        ELSE
          HOV_WARN(IL) = 5280
        ENDIF
        IF(STRING(47:50) .EQ. '') THEN
          HOV_PCT(IL) = DEFAULT_HOV_PCT
        ELSE
          HOV_PCT(IL) = IBUF(10) / 100.
        ENDIF
      ENDIF
 
    ! --- Identify the lanes in the HOV facility.
 
      LANE_IDS = 0
      ILANE = 1                                                   
      DO IAUX = N_AUXLANES, 1, -1         
        IF(AUX_LANE_CODE(IL, IAUX) .EQ. AUX_FULL) THEN
          IF(AUX_LANE_ID(IL, IAUX) .GT. 15) THEN
            LANE_IDS(ILANE) = AUX_LANE_ID(IL, IAUX)
            ILANE = ILANE + 1
          ENDIF
        ENDIF
      ENDDO
      DO I = 1, FNUMLANES(IL)
        LANE_IDS(ILANE) = I
        ILANE = ILANE + 1
      ENDDO
      DO IAUX = N_AUXLANES, 1, -1
        IF(AUX_LANE_CODE(IL, IAUX)  .EQ. AUX_FULL) THEN
          IF(AUX_LANE_ID(IL, IAUX) .LE. 15) THEN
            LANE_IDS(ILANE) = AUX_LANE_ID(IL, IAUX)
            ILANE = ILANE + 1
          ENDIF
        ENDIF
      ENDDO
      TOTLAN = ILANE - 1
      DO I = 1, NHOV_LANES(IL)
        IF(HOV_SIDE(IL) .EQ. 1) THEN
          HOV_LANES(IL, I) = LANE_IDS(I)
        ELSE
          HOV_LANES(IL, I) = LANE_IDS(TOTLAN + 1 - I)
        ENDIF
      ENDDO
    ELSE
      HOV_CODE(IL) = IBUF(6)
    ENDIF
  ENDIF
  RETURN
  END 

! ==================================================================================================
  SUBROUTINE READ_RT35(STRING)
! ----------------------------------------------------------------------
! --- Sign or Signal Approaches and Intervals.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE STREET_NODES
  USE TIMED_CONTROLLERS
  USE SIMPARAMS
  USE TEXT
  USE GLOBAL_DATA
  USE NODE_TABLE
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(20), IL, NODE, ISIG = 0, I, INT, OFFSET, N, IAP
! ----------------------------------------------------------------------
  READ(STRING, '(19I4, I2)') IBUF
  IF(IBUF(1) .GE. 7000) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A, I4)') 'RT 35: INVALID NODE SPECIFIED ', IBUF(1)
    CALL SENDTEXTMSG(M_ERROR)
  ELSEIF(NETCODE(IBUF(1)) .NE. I_STREET) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A, I4)') 'RT 35: INVALID NODE SPECIFIED ', IBUF(1)
    CALL SENDTEXTMSG(M_ERROR)
  ELSEIF(TIME_PERIOD .EQ. 1) THEN
    NODE = IBUF(1)
    IF(NODE .NE. 0) THEN
      ISIG = 0
      DO I = 1, NUMBER_OF_FTCS
        IF(FTC_SIGNALS(I)%NODE .EQ. NODE) THEN
          ISIG = I
          EXIT
        ENDIF
      ENDDO
      IF(ISIG .EQ. 0) THEN
        NUMBER_OF_FTCS = NUMBER_OF_FTCS + 1
        ISIG = NUMBER_OF_FTCS
      ENDIF
      IF(FTC_SIGNALS(ISIG)%APPROACHES .EQ. 0) THEN
        NFTC(NODE) = NUMBER_OF_FTCS
        FTC_SIGNALS(ISIG)%NODE = NODE
 
    ! --- Store the approach links.
 
        IAP = 0
        DO N = 3, 7
          IF(IBUF(N) .NE. 0) THEN
            CALL FIND_STREET_LINK(IBUF(N), NODE, IL)
            IF(IL .EQ. 0) THEN
              CALL NEW_ERROR
              WRITE(MSGTEXT, '(A, I5)') 'RT 35: APPROACH LINK NOT FOUND FOR FIXED TIME CONTROL SIGNAL AT NODE ', NODE
              CALL SENDTEXTMSG(M_ERROR)
              WRITE(MSGTEXT, '(A, I4)') '  APPROACH NODE = ', IBUF(N)
              CALL SENDTEXTMSG(M_ERROR)
            ELSE
              IAP = IAP + 1
              FTC_SIGNALS(ISIG)%APPROACH(IAP) = IL
              FTC_SIGNALS(ISIG)%APPROACHES = IAP
              FTC_SIGNAL_ID(IL) = ISIG
              APPROACH_NUM(IL) = IAP
            ENDIF
          ENDIF
        ENDDO
         
        FTC_SIGNALS(ISIG)%ACTIVE_INTERVALS = 1
        DO I = 1, 12
          FTC_SIGNALS(ISIG)%DURATION(I) = IBUF(I + 7)
          FTC_SIGNALS(ISIG)%CYCLE_LENGTH = FTC_SIGNALS(ISIG)%CYCLE_LENGTH + IBUF(I + 7)
          IF(IBUF(I + 7) .NE. 0) FTC_SIGNALS(ISIG)%ACTIVE_INTERVALS = I
        ENDDO
        IF(FTC_SIGNALS(ISIG)%ACTIVE_INTERVALS .GT. 1) NUMBER_OF_FTSIGNALS = NUMBER_OF_FTSIGNALS + 1
 
    ! --- If there is an offset determine the starting interval and
    ! --- the time in that interval so that the signal will reach interval 1
    ! --- at the offset time.
        
        FTC_SIGNALS(ISIG)%CURRENT_INTERVAL = 1
    
        IF(IBUF(2) .NE. 0) THEN
          IF(IBUF(2) .LT. 0) THEN
            CALL NEW_ERROR
            WRITE(MSGTEXT, '(A, I5)') 'RT 35: NEGATIVE OFFSET FOR FIXED TIME CONTROL SIGNAL AT NODE ', NODE
            CALL SENDTEXTMSG(M_ERROR)
            WRITE(MSGTEXT, '(A, I4)') '  OFFSET = ', IBUF(2)
            CALL SENDTEXTMSG(M_ERROR)
          ELSE
            OFFSET = IBUF(2)
            INT = FTC_SIGNALS(ISIG)%ACTIVE_INTERVALS
            DO WHILE(OFFSET .GT. 0)
              OFFSET = OFFSET - FTC_SIGNALS(ISIG)%DURATION(INT)
              INT = INT - 1
            ENDDO
            INT = INT + 1
            FTC_SIGNALS(ISIG)%CURRENT_INTERVAL = INT
            FTC_SIGNALS(ISIG)%TIME_IN_INTERVAL = -OFFSET     
          ENDIF
        ENDIF
      ELSE
 
    ! --- Store the additional approach link.
 
        IAP = FTC_SIGNALS(ISIG)%APPROACHES
        N = 3
        IF(IBUF(N) .NE. 0) THEN
          CALL FIND_STREET_LINK(IBUF(N), NODE, IL)
          IF(IL .EQ. 0) THEN
            CALL NEW_ERROR
            WRITE(MSGTEXT, '(A, I5)') 'RT 35: APPROACH LINK NOT FOUND FOR FIXED TIME CONTROL SIGNAL AT NODE ', NODE
            CALL SENDTEXTMSG(M_ERROR)
            WRITE(MSGTEXT, '(A, I4)') '  APPROACH NODE = ', IBUF(N)
            CALL SENDTEXTMSG(M_ERROR)
          ELSE
            IAP = IAP + 1
            FTC_SIGNALS(ISIG)%APPROACH(IAP) = IL
            FTC_SIGNALS(ISIG)%APPROACHES = IAP
            FTC_SIGNAL_ID(IL) = ISIG
            APPROACH_NUM(IL) = IAP
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ELSE

    !Store interval data for subsequent time periods
    NODE = IBUF(1)
    ISIG = 0
    DO I = 1, NUMBER_OF_FTCS
      IF(FTC_SIGNALS(I)%NODE .EQ. NODE) THEN
        ISIG = I
        EXIT
      ENDIF
    ENDDO
    FTC_SIGNALS(ISIG)%ACTIVE_INTERVALS = 1
    DO I = 1, 12
      FTC_SIGNALS(ISIG)%DURATION(I) = IBUF(I + 7)
      FTC_SIGNALS(ISIG)%CYCLE_LENGTH = FTC_SIGNALS(ISIG)%CYCLE_LENGTH + IBUF(I + 7)
      IF(IBUF(I + 7) .NE. 0) FTC_SIGNALS(ISIG)%ACTIVE_INTERVALS = I
    ENDDO
    IF(FTC_SIGNALS(ISIG)%ACTIVE_INTERVALS .GT. 1) NUMBER_OF_FTSIGNALS = NUMBER_OF_FTSIGNALS + 1
 
! --- If there is an offset determine the starting interval and
! --- the time in that interval so that the signal will reach interval 1
! --- at the offset time.
        
    !!!This requires signal transition, which has not been implemented yet.
    !IF(IBUF(2) .NE. 0) THEN
    !  IF(IBUF(2) .LT. 0) THEN
    !    CALL NEW_ERROR
    !    WRITE(MSGTEXT, '(A, I5)') 'RT 35: NEGATIVE OFFSET FOR FIXED TIME CONTROL SIGNAL AT NODE ', NODE
    !    CALL SENDTEXTMSG(M_ERROR)
    !    WRITE(MSGTEXT, '(A, I4)') '  OFFSET = ', IBUF(2)
    !    CALL SENDTEXTMSG(M_ERROR)
    !  ELSE
    !    OFFSET = IBUF(2)
    !    INT = FTC_SIGNALS(ISIG)%ACTIVE_INTERVALS
    !    DO WHILE(OFFSET .GT. 0)
    !      OFFSET = OFFSET - FTC_SIGNALS(ISIG)%DURATION(INT)
    !      INT = INT - 1
    !    ENDDO
    !    INT = INT + 1
    !    FTC_SIGNALS(ISIG)%CURRENT_INTERVAL = INT
    !    FTC_SIGNALS(ISIG)%TIME_IN_INTERVAL = -OFFSET     
    !  ENDIF
    !ENDIF
  ENDIF
  RETURN
  END 
      
! ==================================================================================================
  SUBROUTINE READ_RT36(STRING)
! ----------------------------------------------------------------------
! --- Sign or Signal Control Codes.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE TIMED_CONTROLLERS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(62), IL, AL, NODE, INTERVAL, I, ISIG
! ----------------------------------------------------------------------
  READ(STRING, '(I4, 1X, 60I1, 11x, I1)') IBUF
  IF(IBUF(1) .GE. 7000) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A, I4)') 'RT 36: INVALID NODE SPECIFIED ', IBUF(1)
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
    NODE = IBUF(1)
    IF(NODE .NE. 0) THEN
      ISIG = 0
      DO I = 1, NUMBER_OF_FTCS
        IF(FTC_SIGNALS(I)%NODE .EQ. NODE) THEN
          ISIG = I
          EXIT
        ENDIF
      ENDDO
      IF(ISIG .NE. 0) THEN
 
  ! --- Store up to 12 intervals for up to 5 approach links.
 
        I = 1
        DO INTERVAL = 1, FTC_SIGNALS(ISIG)%ACTIVE_INTERVALS
          DO AL = 1, 5
            I = I + 1
            IF(AL .GT. FTC_SIGNALS(ISIG)%APPROACHES) CYCLE
            IL = FTC_SIGNALS(ISIG)%APPROACH(AL)
            IF(IL .NE. 0) THEN
              SIGNAL_CODE(IL) = FTC_SIGNALS(ISIG)%SIGNAL_CODE(AL, 1)
              IF(FTC_SIGNALS(ISIG)%ACTIVE_INTERVALS .EQ. 1) THEN
                IF(IBUF(I) .EQ. 0) IBUF(I) = S_YIELD
                IF(IBUF(I) .EQ. 1) IBUF(I) = S_GREEN
                IF(IBUF(I) .EQ. 5) IBUF(I) = S_STOP
                SIGNAL_CODE(IL) = IBUF(I)
              ELSE
                IF(INTERVAL .EQ. 1) SIGNALIZED(IL) = .TRUE.
              ENDIF
            ENDIF
            FTC_SIGNALS(ISIG)%SIGNAL_CODE(AL, INTERVAL) = IBUF(I)
          ENDDO
        ENDDO
        FTC_SIGNALS(ISIG)%EXTERNAL_CONTROL = IBUF(62) .EQ. 2
        IF(IBUF(62) .EQ. 1) THEN
          CALL NEW_ERROR
          WRITE(MSGTEXT, '(A, I4)') 'RT 36: MICRONODE OPERATION SPECIFIED FOR NODE ', IBUF(1)
          CALL SENDTEXTMSG(M_ERROR)
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END  
      
! ==================================================================================================
  SUBROUTINE READ_RT37(STRING)
! ----------------------------------------------------------------------
! --- Freeway Metering.
! ----------------------------------------------------------------------
  USE FREEWAY_NODES
  USE FREEWAY_LINKS
  USE RAMP_METERS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(19), IL, I
! ----------------------------------------------------------------------
  READ(STRING, '(19I4)') IBUF
  IF(IBUF(1) .NE. 0) THEN
    IL = 0
    DO I = 1, N_FREEWAY_LINKS
      IF(FDSN(I) .EQ. IBUF(1) .AND. LINKTYPE(I) .GT. 0) THEN
        IL = I
        EXIT
      ENDIF
    ENDDO
    IF(IL .EQ. 0) THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, I4)') 'LINK NOT FOUND FOR RAMPMETER AT NODE ', IBUF(1)
      CALL SENDTEXTMSG(M_ERROR)
    ELSE
      IF(TIME_PERIOD .EQ. 1) THEN
        NUMBER_OF_RAMPMETERS = NUMBER_OF_RAMPMETERS + 1
        RAMPMETER(IL) = NUMBER_OF_RAMPMETERS
        RAMPMETERS(NUMBER_OF_RAMPMETERS)%DSN = IBUF(1)
        RAMPMETERS(NUMBER_OF_RAMPMETERS)%LINK = IL
        RAMPMETERS(NUMBER_OF_RAMPMETERS)%STATE = MS_INACTIVE
      ENDIF
      RAMPMETERS(NUMBER_OF_RAMPMETERS)%CONTROL = IBUF(2)
      RAMPMETERS(NUMBER_OF_RAMPMETERS)%ONSET = IBUF(3)
      !RAMPMETERS(NUMBER_OF_RAMPMETERS)%UPDINT = 60
      IF(IBUF(4) .NE. 0) THEN
        IF(IBUF(2) .EQ. 1) THEN
          RAMPMETERS(NUMBER_OF_RAMPMETERS)%HEADWAY(1) = FLOAT(IBUF(4)) / 10.
        !ELSE
        !  RAMPMETERS(NUMBER_OF_RAMPMETERS)%UPDINT = FLOAT(IBUF(4)) / 10.
        ENDIF
      ENDIF
      IF(IBUF(2) .EQ. 2) THEN
        RAMPMETERS(NUMBER_OF_RAMPMETERS)%CAPACITY = IBUF(5)
      ELSEIF(IBUF(2) .EQ. 3) THEN
        DO I = 1, 6
          RAMPMETERS(NUMBER_OF_RAMPMETERS)%SPEED(I) = IBUF(2*(I-1)+6)
          RAMPMETERS(NUMBER_OF_RAMPMETERS)%HEADWAY(I) = FLOAT(IBUF(2*(I-1)+7)) / 10.
        ENDDO
      ENDIF
      IF(IBUF(2) .EQ. 1) RAMPMETERS(NUMBER_OF_RAMPMETERS)%TWO_PERGREEN = IBUF(19) .EQ. 2
      IF(IBUF(19) .LT. 0 .OR. IBUF(19) .GT. 2) THEN
        WRITE(MSGTEXT, '(A, I4)') 'INVALID NUMBER OF VEHICLES PER GREEN PER LANE FOR RAMPMETER AT NODE ', IBUF(1)
        CALL SENDTEXTMSG(M_WARNING)
        WRITE(MSGTEXT, '(A, I4)') '  NUMBER OF VEHICLES PER GREEN PER LANE = ', IBUF(19)
        CALL SENDTEXTMSG(M_WARNING)
        WRITE(MSGTEXT, '(A)') '  ONLY ONE VEHICLE WILL BE ALLOWED PER GREEN PER LANE'
        CALL SENDTEXTMSG(M_WARNING)
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END 

! ==================================================================================================
  SUBROUTINE READ_RT38(STRING)
! ----------------------------------------------------------------------
! --- Freeway Metering Detectors.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  USE FREEWAY_DETECTORS
  USE RAMP_METERS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(17), I, IL, IDET, NDET, ILANE, ILOC, IM
  LOGICAL :: FOUND
! ----------------------------------------------------------------------
  READ(STRING, '(17I4)') IBUF
  DO IM = 1, NUMBER_OF_RAMPMETERS
    IF(FDSN(RAMPMETERS(IM)%LINK) .EQ. IBUF(1)) EXIT
  ENDDO
  CALL FIND_FREEWAY_LINK(IBUF(2), IBUF(3), IL)
  I = 2
  NDET = 1
1 I = I + 2
  IF(IL .NE. 0) THEN
    IDET = FFIRST_DETECTOR(IL)
    FOUND = .FALSE.
    ILANE = IBUF(I)
    ILOC = IBUF(I + 1)
    DO WHILE(IDET .NE. 0)
      IF(FDETECTOR(IDET)%LANE1 .EQ. ILANE) THEN
        IF(FDETECTOR(IDET)%LOCATION .EQ. ILOC) THEN
          RAMPMETERS(IM)%DETECTOR(NDET) = IDET
          FOUND = .TRUE.
          EXIT
        ENDIF
      ENDIF
      IDET = FDETECTOR(IDET)%NEXT_DET
    ENDDO
    IF(.NOT. FOUND) THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, I5)') 'RT 38: DETECTOR NOT FOUND FOR RAMPMETER AT NODE ', IBUF(1)
      CALL SENDTEXTMSG(M_ERROR)
      WRITE(MSGTEXT, '(A, 2I5)') '  LINK = ', IBUF(2), IBUF(3)
      CALL SENDTEXTMSG(M_ERROR)
      WRITE(MSGTEXT, '(A, I4, A, I4)') '  LANE = ', ILANE, 'LOCATION = ', ILOC
      CALL SENDTEXTMSG(M_ERROR)
    ENDIF
    IF(IBUF(I + 2) .NE. 0) THEN
      NDET = NDET + 1
      GOTO 1
    ENDIF
  ENDIF
  RETURN
  END 
      
! ==================================================================================================
  SUBROUTINE READ_RT42(STRING)
! ----------------------------------------------------------------------
! --- Surface Street Detector Specification.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE STREET_DETECTORS
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(8), I, IL, IDET, ILN
! ----------------------------------------------------------------------
  READ(STRING, '(3I4,I1,2X,I5,2X,I4,2X,I4,2X,I1)') IBUF
  CALL FIND_STREET_LINK(IBUF(1), IBUF(2), IL)
  IF(IL .NE. 0) THEN
    N_STREET_DETECTORS = N_STREET_DETECTORS + 1
    SDETECTOR(N_STREET_DETECTORS)%LINK = IL
    IF(IBUF(3) .EQ. 8) THEN
      IBUF(3) = 100
    ELSEIF(IBUF(3) .EQ. 9) THEN
      IBUF(3) = 200
    ENDIF
    IF(IBUF(4) .EQ. 8) THEN
      IBUF(4) = 100
    ELSEIF(IBUF(4) .EQ. 9) THEN
      IBUF(4) = 200
    ENDIF
    SDETECTOR(N_STREET_DETECTORS)%LANE1 = IBUF(3)
    SDETECTOR(N_STREET_DETECTORS)%LANE2 = IBUF(4)
    SDETECTOR(N_STREET_DETECTORS)%LOCATION = SLENGTH(IL) - (IBUF(5) + IBUF(7)) / 10.
    SDETECTOR(N_STREET_DETECTORS)%ZONE_LENGTH = MAX(1., IBUF(7)/10.)
    SDETECTOR(N_STREET_DETECTORS)%STATION_ID = IBUF(6)
    SDETECTOR(N_STREET_DETECTORS)%OPERATION_CODE = IBUF(8)
    
    !Determine the number of lanes covered by the detector
    IF(SDETECTOR(N_STREET_DETECTORS)%LANE1 .GE. 1 .AND. SDETECTOR(N_STREET_DETECTORS)%LANE1 .LE. 7) THEN
      SDETECTOR(N_STREET_DETECTORS)%LANES_COVERED = SDETECTOR(N_STREET_DETECTORS)%LANES_COVERED + 1
    ELSEIF(SDETECTOR(N_STREET_DETECTORS)%LANE1 .EQ. 8) THEN
      SDETECTOR(N_STREET_DETECTORS)%LANES_COVERED = SDETECTOR(N_STREET_DETECTORS)%LANES_COVERED &
        + SNUMLANES(IL)
    ELSEIF(SDETECTOR(N_STREET_DETECTORS)%LANE1 .EQ. 9) THEN
      SDETECTOR(N_STREET_DETECTORS)%LANES_COVERED = SDETECTOR(N_STREET_DETECTORS)%LANES_COVERED + TOTAL_LANES(IL)
    ENDIF
    IF(SDETECTOR(N_STREET_DETECTORS)%LANE2 .GE. 1 .AND. SDETECTOR(N_STREET_DETECTORS)%LANE2 .LE. 7) THEN
      SDETECTOR(N_STREET_DETECTORS)%LANES_COVERED = SDETECTOR(N_STREET_DETECTORS)%LANES_COVERED + 1
    ELSEIF(SDETECTOR(N_STREET_DETECTORS)%LANE2 .EQ. 8) THEN
      SDETECTOR(N_STREET_DETECTORS)%LANES_COVERED = SDETECTOR(N_STREET_DETECTORS)%LANES_COVERED &
        + SNUMLANES(IL)
    ELSEIF(SDETECTOR(N_STREET_DETECTORS)%LANE2 .EQ. 9) THEN
      SDETECTOR(N_STREET_DETECTORS)%LANES_COVERED = SDETECTOR(N_STREET_DETECTORS)%LANES_COVERED + TOTAL_LANES(IL)
    ENDIF
          
    IF(SDETECTOR(N_STREET_DETECTORS)%LANE1 .EQ. 8) THEN
      SDETECTOR(N_STREET_DETECTORS)%LANE1 = 100
    ELSEIF(SDETECTOR(N_STREET_DETECTORS)%LANE1 .EQ. 9) THEN
      SDETECTOR(N_STREET_DETECTORS)%LANE1 = 200
    ELSE
      DO ILN = 1, 7
        IF(SDETECTOR(N_STREET_DETECTORS)%LANE1 .EQ. LANE_NUMBERS(IL, ILN)) THEN
          SDETECTOR(N_STREET_DETECTORS)%LANE1 = ILN
          EXIT
        ENDIF
      ENDDO
    ENDIF
          
    IF(SDETECTOR(N_STREET_DETECTORS)%LANE2 .EQ. 8) THEN
      SDETECTOR(N_STREET_DETECTORS)%LANE2 = 100
    ELSEIF(SDETECTOR(N_STREET_DETECTORS)%LANE2 .EQ. 9) THEN
      SDETECTOR(N_STREET_DETECTORS)%LANE2 = 200
    ELSE
      DO ILN = 1, 7
        IF(SDETECTOR(N_STREET_DETECTORS)%LANE2 .EQ. LANE_NUMBERS(IL, ILN)) THEN
          SDETECTOR(N_STREET_DETECTORS)%LANE2 = ILN
          EXIT
        ENDIF
      ENDDO
    ENDIF
         
    IF(SFIRST_DETECTOR(IL) .EQ. 0) THEN
      SFIRST_DETECTOR(IL) = N_STREET_DETECTORS
    ELSE
      IDET = SFIRST_DETECTOR(IL)
      DO WHILE(IDET .NE. 0)
        I = IDET            
        IDET = SDETECTOR(I)%NEXT_DET
      ENDDO
      SDETECTOR(I)%NEXT_DET = N_STREET_DETECTORS
    ENDIF
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE READ_RT43(STRING)
! ----------------------------------------------------------------------
! --- Approaches for actuated controllers.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE STREET_NODES
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(13), I, IL, N1, N2
! ----------------------------------------------------------------------
  READ(STRING, '(12I4,28X,I1)') IBUF
  IF(IBUF(12) .EQ. 0) THEN
    NUMBER_OF_ACS = NUMBER_OF_ACS + 1
    NACT(IBUF(1)) = NUMBER_OF_ACS
    AC_SIGNALS(NUMBER_OF_ACS)%NODE(1) = IBUF(1)
    IF(IBUF(13) .EQ. 1) THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, I4)') 'RT 43: MICRONODE OPERATION SPECIFIED FOR NODE ', IBUF(1)
      CALL SENDTEXTMSG(M_ERROR)
    ENDIF
    N1 = 0
    N2 = 0
    DO I = 2, 10, 2
      CALL FIND_STREET_LINK(IBUF(I), IBUF(I+1), IL)
      IF(IL .NE. 0) THEN
        IF(IBUF(I+1) .EQ. IBUF(1)) THEN
          AC_SIGNAL_ID(IL) = NUMBER_OF_ACS
          SIGNALIZED(IL) = .TRUE.
          SIGNAL_CODE(IL) = S_RED
          N1 = N1 + 1
          AC_SIGNALS(NUMBER_OF_ACS)%DIRECT_APPROACH(N1) = IL
          AC_SIGNALS(NUMBER_OF_ACS)%N_DIRECT_APPROACHES = N1
        ELSE
          N2 = N2 + 1
          AC_SIGNALS(NUMBER_OF_ACS)%INDIRECT_APPROACH(N2) = IL
          AC_SIGNALS(NUMBER_OF_ACS)%N_INDIRECT_APPROACHES = N2
        ENDIF
        AC_SIGNALS(NUMBER_OF_ACS)%APPROACH_LINK(N1+N2) = IL
      ENDIF
    ENDDO
    AC_SIGNALS(NUMBER_OF_ACS)%EXTERNAL_CONTROL = IBUF(13) .EQ. 2
  ELSE
    N1 = AC_SIGNALS(NUMBER_OF_ACS)%N_DIRECT_APPROACHES
    N2 = AC_SIGNALS(NUMBER_OF_ACS)%N_INDIRECT_APPROACHES
    DO I = 2, 10, 2
      CALL FIND_STREET_LINK(IBUF(I), IBUF(I+1), IL)
      IF(IL .NE. 0) THEN
        IF(IBUF(I+1) .EQ. IBUF(1)) THEN
          N1 = N1 + 1
          AC_SIGNAL_ID(IL) = NUMBER_OF_ACS
          AC_SIGNALS(NUMBER_OF_ACS)%DIRECT_APPROACH(N1) = IL
          AC_SIGNALS(NUMBER_OF_ACS)%N_DIRECT_APPROACHES = N1
        ELSE
          N2 = N2 + 1
          AC_SIGNALS(NUMBER_OF_ACS)%INDIRECT_APPROACH(N2) = IL
        ENDIF
      ENDIF
    ENDDO
  ENDIF
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE READ_RT44(STRING)
! ----------------------------------------------------------------------
! --- Coordination for actuated controllers.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(10), NODE, IACT, SPLITS(8)
! ----------------------------------------------------------------------
  READ(STRING, '(I4,2I3,18X,I3,3X,3I3,3X,2I3,15X,I1)') IBUF
  NODE = IBUF(1)
  IF(TIME_PERIOD .EQ. 1) THEN
    DO IACT = 1, NUMBER_OF_ACS
      IF(AC_SIGNALS(IACT)%NODE(1) .EQ. NODE) THEN
        CALL SET_CYCLE_LENGTH(IACT, FLOAT(IBUF(2)))
        AC_SIGNALS(IACT)%OFFSET = IBUF(3)
        IF(SYNC_REF_TIME .EQ. -1) THEN
          CALL SET_COORDINATION_TIMER(IACT, AC_SIGNALS(IACT)%CYCLE_LENGTH + AC_SIGNALS(IACT)%OFFSET)
        ELSE
          CALL SET_COORDINATION_TIMER(IACT, AC_SIGNALS(IACT)%CYCLE_LENGTH - SYNC_REF_TIME - TIMESTEP)
          CALL SET_MASTER_CYCLE_TIMER(IACT, SYNC_REF_TIME)
          IF(SYNC_REF_TIME - AC_SIGNALS(IACT)%OFFSET .LT. 0.) THEN
            CALL SET_LOCAL_CYCLE_TIMER(IACT, SYNC_REF_TIME - AC_SIGNALS(IACT)%OFFSET + AC_SIGNALS(IACT)%CYCLE_LENGTH)
          ELSE
            CALL SET_LOCAL_CYCLE_TIMER(IACT, SYNC_REF_TIME - AC_SIGNALS(IACT)%OFFSET)
          ENDIF
        ENDIF
        IF(READ_SPLITS) THEN
          AC_SIGNALS(IACT)%PHASE_SPLITS(1) = IBUF(4)
          AC_SIGNALS(IACT)%PHASE_SPLITS(3) = IBUF(5)
          AC_SIGNALS(IACT)%PHASE_SPLITS(4) = IBUF(6)
          AC_SIGNALS(IACT)%PHASE_SPLITS(5) = IBUF(7)
          AC_SIGNALS(IACT)%PHASE_SPLITS(7) = IBUF(8)
          AC_SIGNALS(IACT)%PHASE_SPLITS(8) = IBUF(9)
        ELSE
          !Convert force-off times to splits
          !The conversion is done in PROCESS_ACTUATED_CONTROL after sequence information is obtained from RT47.
          AC_SIGNALS(IACT)%FORCE_OFF_TIME(1) = IBUF(4)
          AC_SIGNALS(IACT)%FORCE_OFF_TIME(3) = IBUF(5)
          AC_SIGNALS(IACT)%FORCE_OFF_TIME(4) = IBUF(6)
          AC_SIGNALS(IACT)%FORCE_OFF_TIME(5) = IBUF(7)
          AC_SIGNALS(IACT)%FORCE_OFF_TIME(7) = IBUF(8)
          AC_SIGNALS(IACT)%FORCE_OFF_TIME(8) = IBUF(9)
          
          !CONSISTENT WITH SUBROUTINE DUPACTIN IN CORSIM
          AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH = AC_SIGNALS(IACT)%CYCLE_LENGTH 
          AC_SIGNALS(IACT)%NEW_OFFSET = AC_SIGNALS(IACT)%OFFSET 
          AC_SIGNALS(IACT)%NEW_FORCE_OFF_TIME = AC_SIGNALS(IACT)%FORCE_OFF_TIME
        ENDIF 
      ENDIF
    ENDDO
  ELSE
    DO IACT = 1, NUMBER_OF_ACS
      IF(AC_SIGNALS(IACT)%NODE(1) .EQ. NODE) THEN
        AC_SIGNALS(IACT)%NEWPLAN = .TRUE.
        AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH = IBUF(2)
        AC_SIGNALS(IACT)%NEW_OFFSET = IBUF(3)
        IF(READ_SPLITS) THEN
          AC_SIGNALS(IACT)%PHASE_SPLITS(1) = IBUF(4)
          AC_SIGNALS(IACT)%PHASE_SPLITS(3) = IBUF(5)
          AC_SIGNALS(IACT)%PHASE_SPLITS(4) = IBUF(6)
          AC_SIGNALS(IACT)%PHASE_SPLITS(5) = IBUF(7)
          AC_SIGNALS(IACT)%PHASE_SPLITS(7) = IBUF(8)
          AC_SIGNALS(IACT)%PHASE_SPLITS(8) = IBUF(9)
        ELSE      
          !Convert force-off times to splits
          AC_SIGNALS(IACT)%FORCE_OFF_TIME(1) = IBUF(4)
          AC_SIGNALS(IACT)%FORCE_OFF_TIME(3) = IBUF(5)
          AC_SIGNALS(IACT)%FORCE_OFF_TIME(4) = IBUF(6)
          AC_SIGNALS(IACT)%FORCE_OFF_TIME(5) = IBUF(7)
          AC_SIGNALS(IACT)%FORCE_OFF_TIME(7) = IBUF(8)
          AC_SIGNALS(IACT)%FORCE_OFF_TIME(8) = IBUF(9)
          AC_SIGNALS(IACT)%NEW_FORCE_OFF_TIME = AC_SIGNALS(IACT)%FORCE_OFF_TIME
          
          CALL CALCSPLITS(IACT, AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH, AC_SIGNALS(IACT)%NEW_FORCE_OFF_TIME, AC_SIGNALS(IACT)%PHASE_SEQUENCE, SPLITS)                         

        ENDIF 
      ENDIF
    ENDDO
  ENDIF
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE READ_RT45(STRING)
! ----------------------------------------------------------------------
! --- Traffic movements for actuated controllers.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  IMPLICIT NONE
  CHARACTER*80, INTENT(INOUT) :: STRING
  INTEGER :: IBUF(2), NODE, PHASE, IACT, N, KK, IL
! ----------------------------------------------------------------------
  READ(STRING, '(I4,3X,I1)') IBUF
  NODE = IBUF(1)
  PHASE = IBUF(2)
  IF(TIME_PERIOD .EQ. 1) THEN
    DO IACT = 1, NUMBER_OF_ACS
      IF(AC_SIGNALS(IACT)%NODE(1) .EQ. NODE) THEN      
        KK = 10
        DO N = 1, 6
          AC_SIGNALS(IACT)%PHASE(PHASE)%LEFTARROW(N) = STRING(KK:KK) .EQ. '1'
          KK = KK + 1
          AC_SIGNALS(IACT)%PHASE(PHASE)%THRUARROW(N) = STRING(KK:KK) .EQ. '1'
          KK = KK + 1
          AC_SIGNALS(IACT)%PHASE(PHASE)%RIGHTARROW(N) = STRING(KK:KK) .EQ. '1'
          KK = KK + 1
          AC_SIGNALS(IACT)%PHASE(PHASE)%LDIAGARROW(N) = STRING(KK:KK) .EQ. '1' .OR. STRING(KK+1:KK+1) .EQ. '1'
          KK = KK + 1
          AC_SIGNALS(IACT)%PHASE(PHASE)%RDIAGARROW(N) = STRING(KK:KK) .EQ. '1' .OR. STRING(KK+1:KK+1) .EQ. '1'
          KK = KK + 6
          IL = AC_SIGNALS(IACT)%DIRECT_APPROACH(N)
          IF(AC_SIGNALS(IACT)%PHASE(PHASE)%LEFTARROW(N)) PHASE_LEFT(IL) = PHASE
          IF(AC_SIGNALS(IACT)%PHASE(PHASE)%THRUARROW(N)) PHASE_THRU(IL) = PHASE
          IF(AC_SIGNALS(IACT)%PHASE(PHASE)%RIGHTARROW(N)) PHASE_RIGHT(IL) = PHASE
          IF(AC_SIGNALS(IACT)%PHASE(PHASE)%LDIAGARROW(N) .OR. AC_SIGNALS(IACT)%PHASE(PHASE)%RDIAGARROW(N)) PHASE_DIAG(IL) = PHASE
        ENDDO
      ENDIF
    ENDDO
  ELSE
    DO IACT = 1, NUMBER_OF_ACS
      IF(AC_SIGNALS(IACT)%NODE(1) .EQ. NODE) THEN      
        KK = 10
        DO N = 1, 6
          AC_SIGNALS(IACT)%PHASE(PHASE)%LEFTARROW(N) = STRING(KK:KK) .EQ. '1'
          KK = KK + 1
          AC_SIGNALS(IACT)%PHASE(PHASE)%THRUARROW(N) = STRING(KK:KK) .EQ. '1'
          KK = KK + 1
          AC_SIGNALS(IACT)%PHASE(PHASE)%RIGHTARROW(N) = STRING(KK:KK) .EQ. '1'
          KK = KK + 1
          AC_SIGNALS(IACT)%PHASE(PHASE)%LDIAGARROW(N) = STRING(KK:KK) .EQ. '1' .OR. STRING(KK+1:KK+1) .EQ. '1'
          KK = KK + 1
          AC_SIGNALS(IACT)%PHASE(PHASE)%RDIAGARROW(N) = STRING(KK:KK) .EQ. '1' .OR. STRING(KK+1:KK+1) .EQ. '1'
          KK = KK + 6
          IL = AC_SIGNALS(IACT)%DIRECT_APPROACH(N)
          IF(AC_SIGNALS(IACT)%PHASE(PHASE)%LEFTARROW(N)) PHASE_LEFT(IL) = PHASE
          IF(AC_SIGNALS(IACT)%PHASE(PHASE)%THRUARROW(N)) PHASE_THRU(IL) = PHASE
          IF(AC_SIGNALS(IACT)%PHASE(PHASE)%RIGHTARROW(N)) PHASE_RIGHT(IL) = PHASE
          IF(AC_SIGNALS(IACT)%PHASE(PHASE)%LDIAGARROW(N) .OR. AC_SIGNALS(IACT)%PHASE(PHASE)%RDIAGARROW(N)) PHASE_DIAG(IL) = PHASE
        ENDDO
      ENDIF
    ENDDO
  ENDIF
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE READ_RT46(STRING)
! ----------------------------------------------------------------------
! --- Detectors for actuated controllers.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE ACTUATED_CONTROLLERS
  USE STREET_DETECTORS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(28), NDET, IACT, I, N, IDET, IL, II, ILN
  INTEGER :: NCOUNT
! ----------------------------------------------------------------------
  READ(STRING, '(I4,1X,I1,1X,I1,1X,I2,3(2I1,I4,3I3,I2,I1,2X))') IBUF
  NDET = N_STREET_DETECTORS
  DO IACT = 1, NUMBER_OF_ACS
    IF(AC_SIGNALS(IACT)%NODE(1) .EQ. IBUF(1)) THEN
      IF(IBUF(4) .EQ. 0) THEN
        CALL NEW_ERROR
        WRITE(MSGTEXT, '(A,I2,A,I4,A)') 'RT 46: A DETECTOR FOR PHASE NUMBER ', IBUF(2), &
          ' OF THE ACTUATED CONTROLLER AT NODE ', IBUF(1), ' WAS ENTERED BUT THE APPROACH LINK WAS NOT SPECIFIED'
        CALL SENDTEXTMSG(M_ERROR)
      ELSE
        IL = AC_SIGNALS(IACT)%APPROACH_LINK(IBUF(4))
      ENDIF
      IF(IL .EQ. 0) THEN
        CALL NEW_ERROR
        WRITE(MSGTEXT, '(A,I2,A,I4,A)') 'RT 46: A DETECTOR FOR PHASE NUMBER ', IBUF(2), &
          ' OF THE ACTUATED CONTROLLER AT NODE ', IBUF(1), ' WAS ENTERED BUT THE APPROACH LINK COULD NOT BE FOUND'
        CALL SENDTEXTMSG(M_ERROR)
      ELSE
        I = 5
        N = 0
        DO
          IF(N .EQ. 0) THEN
            N = 1
            NDET = NDET + 1
            IF(SFIRST_DETECTOR(IL) .EQ. 0) THEN
              SFIRST_DETECTOR(IL) = NDET
            ELSE
              IDET = SFIRST_DETECTOR(IL)
              DO WHILE(IDET .NE. 0)
                II = IDET            
                IDET = SDETECTOR(II)%NEXT_DET
              ENDDO
              SDETECTOR(II)%NEXT_DET = NDET
            ENDIF
          ELSE
            NDET = NDET + 1
            IDET = SFIRST_DETECTOR(IL)
            DO WHILE(IDET .NE. 0)
              II = IDET            
              IDET = SDETECTOR(II)%NEXT_DET
            ENDDO
            SDETECTOR(II)%NEXT_DET = NDET
          ENDIF
          N_STREET_DETECTORS = N_STREET_DETECTORS + 1
          SDETECTOR(N_STREET_DETECTORS)%LINK = IL
          SDETECTOR(N_STREET_DETECTORS)%SIGNAL_NODE = IBUF(1)
          SDETECTOR(N_STREET_DETECTORS)%LANE1 = IBUF(I)
          SDETECTOR(N_STREET_DETECTORS)%LANE2 = IBUF(I+1)
          SDETECTOR(N_STREET_DETECTORS)%LOCATION = SLENGTH(IL) - IBUF(I+2) / 10.
          SDETECTOR(N_STREET_DETECTORS)%DELAY_TIME = IBUF(I+3) / 10.
          SDETECTOR(N_STREET_DETECTORS)%CARRYOVER_TIME = IBUF(I+4) / 10.
          SDETECTOR(N_STREET_DETECTORS)%ZONE_LENGTH = IBUF(I+5) / 10.
          SDETECTOR(N_STREET_DETECTORS)%STATION_ID = 0
          SDETECTOR(N_STREET_DETECTORS)%OPERATION_CODE = IBUF(I+7)
          SDETECTOR(N_STREET_DETECTORS)%ASSOCIATED_PHASE = IBUF(2)
          
          !Determine the number of lanes covered by the detector
          IF(SDETECTOR(N_STREET_DETECTORS)%LANE1 .GE. 1 .AND. SDETECTOR(N_STREET_DETECTORS)%LANE1 .LE. 7) THEN
            SDETECTOR(N_STREET_DETECTORS)%LANES_COVERED = SDETECTOR(N_STREET_DETECTORS)%LANES_COVERED + 1
          ELSEIF(SDETECTOR(N_STREET_DETECTORS)%LANE1 .EQ. 8) THEN
            SDETECTOR(N_STREET_DETECTORS)%LANES_COVERED = SDETECTOR(N_STREET_DETECTORS)%LANES_COVERED &
              + SNUMLANES(IL)
          ELSEIF(SDETECTOR(N_STREET_DETECTORS)%LANE1 .EQ. 9) THEN
            SDETECTOR(N_STREET_DETECTORS)%LANES_COVERED = SDETECTOR(N_STREET_DETECTORS)%LANES_COVERED + TOTAL_LANES(IL)
          ENDIF
          IF(SDETECTOR(N_STREET_DETECTORS)%LANE2 .GE. 1 .AND. SDETECTOR(N_STREET_DETECTORS)%LANE2 .LE. 7) THEN
            SDETECTOR(N_STREET_DETECTORS)%LANES_COVERED = SDETECTOR(N_STREET_DETECTORS)%LANES_COVERED + 1
          ELSEIF(SDETECTOR(N_STREET_DETECTORS)%LANE2 .EQ. 8) THEN
            SDETECTOR(N_STREET_DETECTORS)%LANES_COVERED = SDETECTOR(N_STREET_DETECTORS)%LANES_COVERED &
              + SNUMLANES(IL)
          ELSEIF(SDETECTOR(N_STREET_DETECTORS)%LANE2 .EQ. 9) THEN
            SDETECTOR(N_STREET_DETECTORS)%LANES_COVERED = SDETECTOR(N_STREET_DETECTORS)%LANES_COVERED + TOTAL_LANES(IL)
          ENDIF
          
          IF(SDETECTOR(N_STREET_DETECTORS)%LANE1 .EQ. 8) THEN
            SDETECTOR(N_STREET_DETECTORS)%LANE1 = 100
          ELSEIF(SDETECTOR(N_STREET_DETECTORS)%LANE1 .EQ. 9) THEN
            SDETECTOR(N_STREET_DETECTORS)%LANE1 = 200
          ELSE
            DO ILN = 1, 7
              IF(SDETECTOR(N_STREET_DETECTORS)%LANE1 .EQ. LANE_NUMBERS(IL, ILN)) THEN
                SDETECTOR(N_STREET_DETECTORS)%LANE1 = ILN
                EXIT
              ENDIF
            ENDDO
          ENDIF
          
          IF(SDETECTOR(N_STREET_DETECTORS)%LANE2 .EQ. 8) THEN
            SDETECTOR(N_STREET_DETECTORS)%LANE2 = 100
          ELSEIF(SDETECTOR(N_STREET_DETECTORS)%LANE2 .EQ. 9) THEN
            SDETECTOR(N_STREET_DETECTORS)%LANE2 = 200
          ELSE
            DO ILN = 1, 7
              IF(SDETECTOR(N_STREET_DETECTORS)%LANE2 .EQ. LANE_NUMBERS(IL, ILN)) THEN
                SDETECTOR(N_STREET_DETECTORS)%LANE2 = ILN
                EXIT
              ENDIF
            ENDDO
          ENDIF
          
          NCOUNT = AC_SIGNALS(IACT)%SDP%DETECTOR_COUNT + 1
          AC_SIGNALS(IACT)%SDP%DETECTOR_COUNT = NCOUNT
          AC_SIGNALS(IACT)%SDP%DETECTOR_LIST(NCOUNT) = N_STREET_DETECTORS
          AC_SIGNALS(IACT)%DETECTOR_OPERATING_CODE(IBUF(2)) = IBUF(I+7)

          I = I + 8
          IF(I .GT. 28) EXIT
          IF(IBUF(I) .EQ. 0) EXIT
        ENDDO
        EXIT
      ENDIF
    ENDIF
  ENDDO
  RETURN
  END

  
! ==================================================================================================
  SUBROUTINE READ_RT47(STRING)
! ----------------------------------------------------------------------
! --- Phase operations for actuated controllers.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE ACTUATED_CONTROLLERS
  USE SCOPE_PARAMETERS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(32), NODE, PHASE, IACT, MODE, ISAVE, FLAGA, FLAGB, FLAGC, FLAGD, IAP, IL
! ----------------------------------------------------------------------
  READ(STRING, '(I4,1X,I1,1X,I3,I3,I2,I3,I1,I3,I2,I4,I1,I4,I3,2I4,2I3,8I1,2I4,3I2,2X,I2,I1)') IBUF
  NODE = IBUF(1)
  PHASE = IBUF(2)
  DO IACT = 1, NUMBER_OF_ACS
    IF(AC_SIGNALS(IACT)%NODE(1) .EQ. NODE) THEN
      AC_SIGNALS(IACT)%GUI_MAX_GREEN_TIMES(PHASE) = IBUF(3)
      AC_SIGNALS(IACT)%GUI_MIN_GREEN_TIMES(PHASE) = IBUF(4)
      AC_SIGNALS(IACT)%GUI_YC(PHASE) = IBUF(16) / 10.
      AC_SIGNALS(IACT)%GUI_RC(PHASE) = IBUF(17) / 10.
      AC_SIGNALS(IACT)%SDP%YC(PHASE) = AC_SIGNALS(IACT)%GUI_YC(PHASE)
      AC_SIGNALS(IACT)%SDP%RC(PHASE) = AC_SIGNALS(IACT)%GUI_RC(PHASE)

      !Determine mode
      !Allow presence, min_recall and max_recall modes with presence detectors or no detectors
      MODE = PRESENCE_MODE
      IF(AC_SIGNALS(IACT)%DETECTOR_OPERATING_CODE(PHASE) .EQ. 0) THEN !Presence mode detector or no detector
        AC_SIGNALS(IACT)%GUI_DEFAULT_EXTENSION_TIMES(PHASE) = IBUF(5) / 10.
        IF(IBUF(22) .GT. 0) THEN
          MODE = RECALL_MIN
        ELSEIF(IBUF(23) .GT. 0) THEN
          MODE = RECALL_MAX
        ENDIF
      ELSEIF(AC_SIGNALS(IACT)%DETECTOR_OPERATING_CODE(PHASE) .EQ. 1) THEN !Pulse mode detector
        !Allow max_out and gap_out modes with pulse detectors
        IF(IBUF(5) .GT. 0) THEN
          AC_SIGNALS(IACT)%GUI_DEFAULT_EXTENSION_TIMES(PHASE) = IBUF(5) / 10.
          IF((IBUF(13) .EQ. 0 .AND. IBUF(14) .EQ. 0) .OR. IBUF(14) .EQ. IBUF(15)) THEN
            MODE = MAX_OUT
          ELSE
            MODE = GAP_OUT
            IF(IBUF(11) .NE. 2) THEN
              CALL NEW_ERROR
              WRITE(MSGTEXT, '(A, 2I5)') 'RT 47: UNSUPPORTED GAP REDUCTION CODE', IBUF(1), IBUF(2)
              CALL SENDTEXTMSG(M_ERROR)
            ELSE
              AC_SIGNALS(IACT)%GUI_TIME_TO_REDUCE(PHASE) = IBUF(13)
              AC_SIGNALS(IACT)%GUI_TIME_BEFORE_REDUCTION(PHASE) = IBUF(13) + (IBUF(12) / 100.)
            ENDIF
          ENDIF
          AC_SIGNALS(IACT)%GUI_MIN_GAP_TIMES(PHASE) = IBUF(14) / 100.
          AC_SIGNALS(IACT)%GUI_GAP_TIMES(PHASE) = IBUF(15) / 100.
        ENDIF
      ENDIF
      AC_SIGNALS(IACT)%GUI_ACTUATED_MODE(PHASE) = MODE
      IF(AC_SIGNALS(IACT)%GUI_MIN_GREEN_TIMES(PHASE) .NE. 0) AC_SIGNALS(IACT)%PHASE(PHASE)%IN_USE = .TRUE.   
!
!     SWAP THE PHASES IN THE PHASE SEQUENCE ARRAY BASED ON WHETHER
!     THE ODD PHASE OF A PHASE PAIR IS LAGGING OR THE EVEN PHASE OF
!     A PHASE PAIR IS LEADING.
!
      !Entry 25 was misinterpreted, %LEAD should be changed to %LAG
      AC_SIGNALS(IACT)%LEAD(PHASE) = IBUF(25) .EQ. 1
      IF(MOD(PHASE, 2) .EQ. 1) THEN                                
        IF(AC_SIGNALS(IACT)%LEAD(PHASE)) THEN          !Odd phase is lagging, so swap                              
          ISAVE = AC_SIGNALS(IACT)%PHASE_SEQUENCE(PHASE+1) 
          AC_SIGNALS(IACT)%PHASE_SEQUENCE(PHASE+1) = PHASE                           
          IF(ISAVE .NE. 0) AC_SIGNALS(IACT)%PHASE_SEQUENCE(PHASE) = ISAVE               
        ELSE                                                        
          AC_SIGNALS(IACT)%PHASE_SEQUENCE(PHASE) = PHASE                               
        ENDIF                                                       
      ELSE                                                          
        IF(AC_SIGNALS(IACT)%LEAD(PHASE)) THEN          !Even phase is lagging, so do not swap                              
          AC_SIGNALS(IACT)%PHASE_SEQUENCE(PHASE) = PHASE                               
        ELSE                                                        
          ISAVE = AC_SIGNALS(IACT)%PHASE_SEQUENCE(PHASE-1) 
          AC_SIGNALS(IACT)%PHASE_SEQUENCE(PHASE-1) = PHASE                           
          IF(ISAVE .NE. 0) AC_SIGNALS(IACT)%PHASE_SEQUENCE(PHASE) = ISAVE               
        ENDIF                                                       
      ENDIF        
      
      !Process overlapping phases
      IF(IBUF(26) .NE. 0) THEN
        FLAGA = IBUF(26) / 1000
        FLAGB = (IBUF(26) - FLAGA * 1000) / 100
        FLAGC = ((IBUF(26) - FLAGA * 1000) - (FLAGB * 100)) / 10
        FLAGD = (IBUF(26) - FLAGA * 1000) - (FLAGB * 100) - FLAGC *10
        IF(FLAGA .NE. 0) THEN
          IF(AC_SIGNALS(IACT)%OVERLAP_A(1) .EQ. 0) THEN
            AC_SIGNALS(IACT)%OVERLAP_A(1) = PHASE
          ELSEIF(AC_SIGNALS(IACT)%OVERLAP_A(2) .EQ. 0) THEN
            AC_SIGNALS(IACT)%OVERLAP_A(2) = PHASE
          ELSE
            CALL NEW_ERROR
            WRITE(MSGTEXT, '(A, I5)') 'RT 47: THERE WAS AN ERROR DETECTED IN PROCESSING OVERLAPPING PHASE A FOR NODE ', NODE
            CALL SENDTEXTMSG(M_ERROR)
          ENDIF
        ENDIF
        IF(FLAGB .NE. 0) THEN
          IF(AC_SIGNALS(IACT)%OVERLAP_B(1) .EQ. 0) THEN
            AC_SIGNALS(IACT)%OVERLAP_B(1) = PHASE
          ELSEIF(AC_SIGNALS(IACT)%OVERLAP_B(2) .EQ. 0) THEN
            AC_SIGNALS(IACT)%OVERLAP_B(2) = PHASE
          ELSE
            CALL NEW_ERROR
            WRITE(MSGTEXT, '(A, I5)') 'RT 47: THERE WAS AN ERROR DETECTED IN PROCESSING OVERLAPPING PHASE B FOR NODE ', NODE
            CALL SENDTEXTMSG(M_ERROR)
          ENDIF
        ENDIF
        IF(FLAGC .NE. 0) THEN
          IF(AC_SIGNALS(IACT)%OVERLAP_C(1) .EQ. 0) THEN
            AC_SIGNALS(IACT)%OVERLAP_C(1) = PHASE
          ELSEIF(AC_SIGNALS(IACT)%OVERLAP_C(2) .EQ. 0) THEN
            AC_SIGNALS(IACT)%OVERLAP_C(2) = PHASE
          ELSE
            CALL NEW_ERROR
            WRITE(MSGTEXT, '(A, I5)') 'RT 47: THERE WAS AN ERROR DETECTED IN PROCESSING OVERLAPPING PHASE C FOR NODE ', NODE
            CALL SENDTEXTMSG(M_ERROR)
          ENDIF
        ENDIF
        IF(FLAGD .NE. 0) THEN
          IF(AC_SIGNALS(IACT)%OVERLAP_D(1) .EQ. 0) THEN
            AC_SIGNALS(IACT)%OVERLAP_D(1) = PHASE
          ELSEIF(AC_SIGNALS(IACT)%OVERLAP_D(2) .EQ. 0) THEN
            AC_SIGNALS(IACT)%OVERLAP_D(2) = PHASE
          ELSE
            CALL NEW_ERROR
            WRITE(MSGTEXT, '(A, I5)') 'RT 47: THERE WAS AN ERROR DETECTED IN PROCESSING OVERLAPPING PHASE D FOR NODE ', NODE
            CALL SENDTEXTMSG(M_ERROR)
          ENDIF
        ENDIF
      ENDIF  
      
      !AC_SIGNALS(IACT)%RED_LOCK_CODE(PHASE) = IBUF(18) 
      !AC_SIGNALS(IACT)%YELLOW_LOCK_CODE(PHASE) = IBUF(19) 
      !AC_SIGNALS(IACT)%DUAL_ENTRY_CODE(PHASE) = IBUF(20)
      !AC_SIGNALS(IACT)%RED_REVERT_TIME(PHASE) = IBUF(27) / 10. 
      !AC_SIGNALS(IACT)%SIMULTANEOUS_GAP_CODE(PHASE) = IBUF(29) 
      !AC_SIGNALS(IACT)%CONDITIONAL_SERVICE_CODE(PHASE) = IBUF(30) 
      !AC_SIGNALS(IACT)%MIN_CONDITIONAL_SERVICE_TIME(PHASE) = IBUF(31) 
      
      EXIT
    ENDIF
  ENDDO
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE READ_RT48(STRING)
! ----------------------------------------------------------------------
! --- Pedestrian operations for actuated controllers.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE STREET_VEHICLES
  USE ACTUATED_CONTROLLERS
  USE SEEDS
  USE SIMPARAMS
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(19), NODE, PHASE, IACT
  REAL :: RNDNUM
! ----------------------------------------------------------------------
  READ(STRING, '(19I4)') IBUF
  NODE = IBUF(1)
  PHASE = IBUF(2)
  DO IACT = 1, NUMBER_OF_ACS
    IF(AC_SIGNALS(IACT)%NODE(1) .EQ. NODE) THEN
      AC_SIGNALS(IACT)%PED_PHASE(PHASE) = .TRUE.
      AC_SIGNALS(IACT)%NEW_PED_WALK_TIMES(PHASE) = IBUF(3)
      AC_SIGNALS(IACT)%NEW_PED_CLEARANCE_TIMES(PHASE) = IBUF(4)
      AC_SIGNALS(IACT)%PED_INTENSITY(PHASE) = IBUF(5)
      AC_SIGNALS(IACT)%PED_HEADWAY(PHASE) = IBUF(6)
      AC_SIGNALS(IACT)%PED_RECALL_CODE(PHASE) = IBUF(8)
      !AC_SIGNALS(IACT)%PED_REST_CODE(PHASE) = IBUF(9)
      !AC_SIGNALS(IACT)%CONST_DEMAND_PERIOD_BEGIN(1) = IBUF(10)
      !AC_SIGNALS(IACT)%CONST_DEMAND_PERIOD_END(1) = IBUF(11)
      !AC_SIGNALS(IACT)%CONST_DEMAND_PERIOD_BEGIN(2) = IBUF(12)
      !AC_SIGNALS(IACT)%CONST_DEMAND_PERIOD_END(2) = IBUF(13)
      !AC_SIGNALS(IACT)%CONST_DEMAND_PERIOD_BEGIN(3) = IBUF(14)
      !AC_SIGNALS(IACT)%CONST_DEMAND_PERIOD_END(3) = IBUF(15)
      !AC_SIGNALS(IACT)%CONST_DEMAND_PERIOD_BEGIN(4) = IBUF(16)
      !AC_SIGNALS(IACT)%CONST_DEMAND_PERIOD_END(4) = IBUF(17)
      !AC_SIGNALS(IACT)%CONST_DEMAND_PERIOD_BEGIN(5) = IBUF(18)
      !AC_SIGNALS(IACT)%CONST_DEMAND_PERIOD_END(5) = IBUF(19)
      IF(TIME_PERIOD .EQ. 1) THEN
        IF(IBUF(5) .GT. 0) THEN
          AC_SIGNALS(IACT)%PED_TYPE(PHASE) = 1
          CALL STREET_RANDOM(SSEED, RNDNUM)  
          AC_SIGNALS(IACT)%NEXT_PED_ARRIVAL(PHASE) = (3600.0 / IBUF(5)) * (-LOG(RNDNUM))
        ELSEIF(IBUF(6) .GT. 0) THEN
          AC_SIGNALS(IACT)%PED_TYPE(PHASE) = 2
          AC_SIGNALS(IACT)%NEXT_PED_ARRIVAL(PHASE) = IBUF(6) + IBUF(7)
        ENDIF
      ENDIF
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE READ_RT49(STRING)
! ----------------------------------------------------------------------
! --- Transition parameters for actuated controllers.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE ACTUATED_CONTROLLERS
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(4), NODE, IACT
! ----------------------------------------------------------------------
  READ(STRING, '(4I4)') IBUF
  NODE = IBUF(1)
  DO IACT = 1, NUMBER_OF_ACS
    IF(AC_SIGNALS(IACT)%NODE(1) .EQ. NODE) THEN
      AC_SIGNALS(IACT)%TRANSITION_METHOD = IBUF(2)
      AC_SIGNALS(IACT)%MAXPCT_ADD = IBUF(3)
      AC_SIGNALS(IACT)%MAXPCT_SUBTRACT = IBUF(4)
    ENDIF
  ENDDO
  RETURN
  END
  
  ! ==================================================================================================
  SUBROUTINE READ_RT50(STRING)
! ----------------------------------------------------------------------
! --- Entry Volume Specification.
! --- Volumes are defined in READ_TIME_VARYING_INPUT.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  USE STREET_LINKS
  USE ENTRYNODE_DATA
  USE SIMPARAMS
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(11), I, IL, ISUM, NODE = 0
  LOGICAL :: FOUND
! ----------------------------------------------------------------------
  READ(STRING, '(5I4,I5,35X,5I3)') IBUF
  NODE = IBUF(1)
  ENTRYNODE_IS_USED(NODE) = .TRUE.
  FLOWRATE(NODE) = FLOAT(IBUF(3))
  TRUCK_PCT(NODE) = FLOAT(IBUF(4)) / 100
  CARPOOL_PCT(NODE) = FLOAT(IBUF(5)) / 100
  HOV_VIOLATOR_PCT(NODE) = FLOAT(IBUF(6)) / 10000.
  FOUND = .FALSE.
  DO IL = 1, N_FREEWAY_LINKS
    IF(FUSN(IL) .EQ. NODE) THEN
      ISUM = 0
      DO I = 7, 11
        ISUM = ISUM + IBUF(I)
      ENDDO
      IF(ISUM .EQ. 100) THEN
        LANE_PCT(NODE, 1:5) = IBUF(7:11) / 100.
      ELSE
        LANE_PCT(NODE, 1:FNUMLANES(IL)) = 1.0 / FNUMLANES(IL)
      ENDIF
      FOUND = .TRUE.
      EXIT
    ENDIF
  ENDDO
  IF(.NOT. FOUND) THEN
    DO IL = 1, N_STREET_LINKS
      IF(SUSN(IL) .EQ. NODE) THEN
        ISUM = 0
        DO I = 7, 11
          ISUM = ISUM + IBUF(I)
        ENDDO
        IF(ISUM .EQ. 100) THEN
          LANE_PCT(NODE, 1:5) = IBUF(7:11) / 100.
        ELSE
          LANE_PCT(NODE, 1:SNUMLANES(IL)) = 1.0 / SNUMLANES(IL)
        ENDIF
        EXIT
      ENDIF
    ENDDO
  ENDIF
  RETURN
  END 

! ==================================================================================================
  SUBROUTINE READ_RT51(STRING)
! ----------------------------------------------------------------------
! --- Entry Volume Specification.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE ENTRYNODE_DATA
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(19), IL
  INTEGER, SAVE :: ICENT
! ----------------------------------------------------------------------
  READ(STRING, '(19I4)') IBUF
  CALL FIND_STREET_LINK(IBUF(2), IBUF(3), IL)
  IF(IL .EQ. 0) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A, 2I5)') 'RT 51: SOURCE VOLUME ENTERED FOR A LINK THAT DOES NOT EXIST ', IBUF(2), IBUF(3)
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
    ICENT = ICENT + 1
    CENTROID(IL) = ICENT
    IF(IBUF(1) .NE. 0) THEN
      CENTROID_LABEL(IL) = IBUF(1)
    ELSE
      CENTROID_LABEL(IL) = ICENT
    ENDIF
  ENDIF
  RETURN
  END 
      
! ==================================================================================================
  SUBROUTINE READ_RT54(STRING)
! ----------------------------------------------------------------------
! --- Short-Term Event Specification.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(11), IL
! ----------------------------------------------------------------------
  READ(STRING, '(4I4, 7I1)') IBUF
  CALL FIND_STREET_LINK(IBUF(1), IBUF(2), IL)
  IF(IL .EQ. 0) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A, 2I5)') 'RT 54: SHORT TERM EVENTS ENTERED FOR A LINK THAT DOES NOT EXIST ', IBUF(1), IBUF(2)
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
    STE_FREQ(IL) = IBUF(3)
    STE_DURATION(IL) = IBUF(4)
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE READ_RT55(STRING)
! ----------------------------------------------------------------------
! --- Long-Term Event Specification. Create a blockage for each event. 
! --- Short-term events and parking events will be generated
! --- stochastically, but long-term events are deterministic.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE STREET_VEHICLES
  USE EVENTS
  USE SIMPARAMS
  USE TEXT
  USE SEEDS
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(11), IL
! ----------------------------------------------------------------------
  READ(STRING, '(4I4,1X,2I1,I1,I8,3I4)') IBUF
  CALL FIND_STREET_LINK(IBUF(1), IBUF(2), IL)
  IF(IL .EQ. 0) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A, 2I5)') 'RT 55: LONG TERM EVENT ENTERED FOR A LINK THAT DOES NOT EXIST ', IBUF(1), IBUF(2)
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
    CALL NEW_EVENT
    EVENT_LINK(NUMBER_OF_EVENTS) = IL
    EVENT_TYPE(NUMBER_OF_EVENTS) = 2
    EVENT_BEGIN_TIME(NUMBER_OF_EVENTS) = IBUF(3)
    EVENT_END_TIME(NUMBER_OF_EVENTS) = IBUF(3) + IBUF(4)
    EVENT_LANE(NUMBER_OF_EVENTS) = IBUF(7)
    EVENT_LOCATION(NUMBER_OF_EVENTS) = IBUF(8)
    EVENT_LENGTH(NUMBER_OF_EVENTS) = IBUF(9)
    EVENT_CODE(NUMBER_OF_EVENTS) = IBUF(10)
    EVENT_SPEED_REDUCTION(NUMBER_OF_EVENTS) = IBUF(11)
    IF(EVENT_BEGIN_TIME(NUMBER_OF_EVENTS) .LT. 0) THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, I5)') 'NEGATIVE START TIME FOR LONG TERM EVENT ', NUMBER_OF_EVENTS
      CALL SENDTEXTMSG(M_ERROR)
      WRITE(MSGTEXT, '(A, I4)') '  START TIME = ', EVENT_BEGIN_TIME(NUMBER_OF_EVENTS)
      CALL SENDTEXTMSG(M_ERROR)
    ELSEIF(EVENT_END_TIME(NUMBER_OF_EVENTS) .LE. EVENT_BEGIN_TIME(NUMBER_OF_EVENTS)) THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, I5)') 'INVALID DURATION FOR LONG TERM EVENT ', NUMBER_OF_EVENTS
      CALL SENDTEXTMSG(M_ERROR)
      WRITE(MSGTEXT, '(A, I4)') '  DURATION = ', EVENT_END_TIME(NUMBER_OF_EVENTS) - EVENT_BEGIN_TIME(NUMBER_OF_EVENTS)
      CALL SENDTEXTMSG(M_ERROR)
    ENDIF
    IF(EVENT_LANE(NUMBER_OF_EVENTS) .GT. 7) THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, I5)') 'INVALID LANE SPECIFIED FOR LONG TERM EVENT ', NUMBER_OF_EVENTS
      CALL SENDTEXTMSG(M_ERROR)
      WRITE(MSGTEXT, '(A, I4)') '  LANE = ', EVENT_LANE(NUMBER_OF_EVENTS)
      CALL SENDTEXTMSG(M_ERROR)
    ENDIF
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE READ_RT56(STRING)
! ----------------------------------------------------------------------
! --- Parking Event Specification.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE PARKING
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(8), IL, IZ
! ----------------------------------------------------------------------
  READ(STRING, '(8I4)') IBUF
  CALL FIND_STREET_LINK(IBUF(1), IBUF(2), IL)
  IF(IL .EQ. 0) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A, 2I5)') 'RT 56: PARKING EVENTS ENTERED FOR A LINK THAT DOES NOT EXIST ', IBUF(1), IBUF(2)
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
    IF(NUMBER_OF_PARKING_ZONES .EQ. 0) THEN
      CALL ALLOCATE_PARKING_ARRAYS
    ELSE
      CALL REALLOCATE_PARKING_ARRAYS
    ENDIF
    IZ = NUMBER_OF_PARKING_ZONES
    PARKING_ZONE_LINK(IZ) = IL
    PARK_RIGHT_START(IZ) = 0
    PARK_RIGHT_LEN(IZ) = 0
    PARK_LEFT_START(IZ) = 0
    PARK_LEFT_LEN(IZ) = 0
    IF(IBUF(4) .NE. 0) THEN
      PARK_RIGHT_START(IZ) = SLENGTH(IL) - IBUF(3) - IBUF(4)
      PARK_RIGHT_LEN(IZ) = IBUF(4)
    ENDIF
    IF(IBUF(6) .NE. 0) THEN
      PARK_LEFT_START(IZ) = SLENGTH(IL) - IBUF(5) - IBUF(6)
      PARK_LEFT_LEN(IZ) = IBUF(6)
    ENDIF
    PARK_DURATION(IZ) = IBUF(7) / 10.
    PARK_FREQ(IZ) = IBUF(8)
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE READ_RT58(STRING)
! ----------------------------------------------------------------------
! --- Vehicle Type Specifications. 
! ----------------------------------------------------------------------
  USE VEHICLE_TYPES
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(8), ITYPE
! ----------------------------------------------------------------------
! --- Length and average occupancy values should agree with values entered on RT71.
! --- Entries on RT71 will override values entered on RT58.
  WRITE58 = .TRUE.
  READ(STRING, '(2I4,8X,I4,20X,4I4,16X,I4)') IBUF
  ITYPE = IBUF(1)
  IF(IBUF(2) .NE. 0) VTLENGTH(ITYPE) = IBUF(2)
  IF(IBUF(3) .NE. 0) HDWY_FACTOR(ITYPE) = IBUF(3) / 100.

! --- Entries for fleet distribution will be used for street network entry nodes only. 
  FLT_STREET_AUTO(ITYPE) = IBUF(4)
  FLT_STREET_TRUCK(ITYPE) = IBUF(5)
  FLT_STREET_BUS(ITYPE) = IBUF(6)
  FLT_STREET_CARPOOL(ITYPE) = IBUF(7)
  IF(IBUF(8) .NE. 0) AVG_OCCS(ITYPE) = IBUF(8) / 100.
  RETURN
  END

! ==================================================================================================
  SUBROUTINE READ_RT68(STRING, INET)
! ----------------------------------------------------------------------
! --- Car Following Sensitivity Factor.
! ----------------------------------------------------------------------
  USE CAR_FOLLOWING
  USE GLOBAL_DATA
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER, INTENT(IN) :: INET
  INTEGER :: IBUF(12), I
! ----------------------------------------------------------------------
  READ(STRING, '(12I4)') IBUF
  IF(IBUF(12) .EQ. 0) THEN    !PITT values
    IF(INET .EQ. I_FREEWAY) THEN
      DO I = 1, 10
        FZFOLL(I) = IBUF(I) / 100.
      ENDDO
      IF(IBUF(11) .NE. 0) FPCFSEP = IBUF(11)
    ELSE
      DO I = 1, 10
        SZFOLL(I) = IBUF(I) / 100.
      ENDDO
      IF(IBUF(11) .NE. 0) SPCFSEP = IBUF(11)
    ENDIF
  ELSEIF(IBUF(12) .EQ. 1) THEN    !IDM values
    IF(INET .EQ. I_FREEWAY) THEN
      DO I = 1, 10
        FZFOLL_IDM(I) = IBUF(I) / 100.
      ENDDO
      IF(IBUF(11) .NE. 0) FIDMSEP = IBUF(11)
    ELSE
      DO I = 1, 10
        SZFOLL_IDM(I) = IBUF(I) / 100.
      ENDDO
      IF(IBUF(11) .NE. 0) SIDMSEP = IBUF(11)
    ENDIF
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE READ_RT69(STRING)
! ----------------------------------------------------------------------
! --- Pavement Friction Coefficients, Lag to Accelerate and Decelerate.
! ----------------------------------------------------------------------
  USE GLOBAL_DATA
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(6), I
! ----------------------------------------------------------------------
  READ(STRING, '(6I4)') IBUF
  DO I = 1, 4
    IF(IBUF(I) .NE. 0) CFRICT(I) = IBUF(I) / 100.
  ENDDO
  IF(IBUF(5) .NE. 0) LAG_ACCEL = IBUF(5) / 10.
  IF(IBUF(6) .NE. 0) LAG_DECEL = IBUF(6) / 10.
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE READ_RT70(STRING)
! ----------------------------------------------------------------------
! --- Freeway Lane Change Parameters,
! --- Minimum Separation for Vehicle Generation,
! --- HOV Lane Entry Percentage.
! ----------------------------------------------------------------------
  USE ENTRYNODE_DATA
  USE VEHICLE_TYPES
  USE GLOBAL_DATA
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(17), I
! ----------------------------------------------------------------------
  READ(STRING, '(17I4)') IBUF
  IF(IBUF(1) .NE. 0) LC_TIME_FREEWAY = IBUF(1) / 10.
  IF(IBUF(2) .NE. 0) MINSEP = IBUF(2) / 10.
  IF(IBUF(4) .NE. 0) FREEWAY_PCT_COOP = IBUF(4) / 100.
  IF(IBUF(5) .NE. 0) DLC_MULT = IBUF(5) / 10.
 
! --- This is an attempt to relate the CORSIM advantage factor to a
! --- minimum headway advantage to use as a threshold for determining
! --- if a vehicle will initiate a discretionary lane change.
 
  IF(IBUF(6) .NE. 0) HMIN = 3.0 * IBUF(6) + 2.0
 
  DO I = 1, 9
    NEMDEC(I) = -IBUF(I+6) / 10.
  ENDDO
 
  IF(IBUF(16) .NE. 0) DEFAULT_HOV_PCT = IBUF(16) / 100.
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE READ_RT71(STRING)
! ----------------------------------------------------------------------
! --- Vehicle Type Specification.
! ----------------------------------------------------------------------
  USE ENTRYNODE_DATA
  USE VEHICLE_TYPES
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(11), ITYPE
! ----------------------------------------------------------------------
  WRITE71 = .TRUE.
  READ(STRING, '(10I4,I6)') IBUF
  ITYPE = IBUF(1)
  IF(IBUF(2) .NE. 0) VTLENGTH(ITYPE) = IBUF(2)
!      JERK(ITYPE) = IBUF(3)
  IF(IBUF(4) .NE. 0) NEMDEC(ITYPE) = IBUF(4)
  
! --- Entries for fleet distribution will be used for freeway network entry nodes only.
  FLT_FREEWAY_AUTO(ITYPE) = IBUF(5)
  FLT_FREEWAY_TRUCK(ITYPE) = IBUF(6)
  FLT_FREEWAY_BUS(ITYPE) = IBUF(7)
  FLT_FREEWAY_CARPOOL(ITYPE) = IBUF(8)
!     PERF_INDEX(ITYPE) = IBUF(9)
  
  IF(IBUF(11) .NE. 0) AVG_OCCS(ITYPE) = IBUF(11) / 100.
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE READ_RT80(STRING)
! ----------------------------------------------------------------------
! --- Optional Link Geometric Data.
! --- If RT 171 is used to specify lane widths those inputs will
! --- over-ride these inputs.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(12), I, IL
! ----------------------------------------------------------------------
  READ(STRING, '(2I4,4X,10I4)') IBUF
  CALL FIND_STREET_LINK(IBUF(1), IBUF(2), IL)
  IF(IL .NE. 0) THEN
    DO I = 1, 7
      IF(IBUF(I+2) .NE. 0) SLANE_WIDTH(IL, I) = IBUF(I+2)
    ENDDO
    !IF(IBUF(10) .NE. 0) 
    IF(IBUF(11) .NE. 0) SIGHT_DIST(IL) = IBUF(11)
    !IF(IBUF(12) .NE. 0) 
  ENDIF
  RETURN
  END      

! ==================================================================================================
  SUBROUTINE READ_RT81(STRING)
! ----------------------------------------------------------------------
! --- Surface Street Lane Change Parameters.
! ----------------------------------------------------------------------
  USE GLOBAL_DATA
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(15)
! ----------------------------------------------------------------------
  READ(STRING, '(2I4,4X,13I4)') IBUF
  IF(IBUF(1) .NE. 0) LC_TIME_STREET = IBUF(1)
  IF(IBUF(11) .NE. 0) STREET_PCT_COOP = IBUF(11) / 100.
  RETURN
  END
      
! ==========================================================================================================
  SUBROUTINE READ_RT82(STRING)
! ----------------------------------------------------------------------
! --- Get intersection width and turning points to define the intersection.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(10), IL, I, N
! ----------------------------------------------------------------------
  READ(STRING, '(10I4)') IBUF
  CALL FIND_STREET_LINK(IBUF(1), IBUF(2), IL)
  IF(IL .NE. 0) THEN
    WRITE82(IL) = .TRUE.
    UP_INT_WIDTH(IL) = IBUF(3)
    N = 3
    DO I = 1, 7
      N = N + 1
      LANE_CENTER(IL, I) = IBUF(N)
    ENDDO
  ENDIF
  RETURN
  END
    
! ==========================================================================================================
  SUBROUTINE READ_RT83(STRING)
! ----------------------------------------------------------------------
! --- Get turning path arc lengths to define the intersection.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(6), IL, ENTRY_LANE, EXIT_LANE
! ----------------------------------------------------------------------
  READ(STRING, '(6I4)') IBUF
  CALL FIND_STREET_LINK(IBUF(1), IBUF(2), IL)
  IF(IL .NE. 0) THEN
    WRITE83(IL) = .TRUE.
    ENTRY_LANE = IBUF(4)
    EXIT_LANE = IBUF(5)
    IF(IBUF(3) .EQ. 0) THEN
      LT_ARC_LENGTH(IL, ENTRY_LANE, EXIT_LANE) = IBUF(6)
    ELSEIF(IBUF(3) .EQ. 1) THEN
      THRU_ARC_LENGTH(IL, ENTRY_LANE, EXIT_LANE) = IBUF(6)
    ELSEIF(IBUF(3) .EQ. 2) THEN
      RT_ARC_LENGTH(IL, ENTRY_LANE, EXIT_LANE) = IBUF(6)
    ELSEIF(IBUF(3) .EQ. 3) THEN
      LD_ARC_LENGTH(IL, ENTRY_LANE, EXIT_LANE) = IBUF(6)
    ELSEIF(IBUF(3) .EQ. 4) THEN
      RD_ARC_LENGTH(IL, ENTRY_LANE, EXIT_LANE) = IBUF(6)
    ELSE
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A,I4)') 'RT 83: UNRECOGNIZED TURN MOVEMENT CODE ', IBUF(3)
      CALL SENDTEXTMSG(M_ERROR)
    ENDIF
  ENDIF
  RETURN
  END
       
! ==================================================================================================
  SUBROUTINE READ_RT84(STRING)
! ----------------------------------------------------------------------
! --- Turn Signal Parameters.
! ----------------------------------------------------------------------
  USE GLOBAL_DATA
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(11)
! ----------------------------------------------------------------------
  READ(STRING, '(11I4)') IBUF
  TURNSIGNAL_DIST = IBUF(1)
  TURNSIGNAL_PROB = IBUF(2:11) / 100.
  RETURN
  END
         
! ==================================================================================================
  SUBROUTINE READ_RT97(STRING)
! ----------------------------------------------------------------------
! --- Roundabout Link Inputs.
! ----------------------------------------------------------------------
  USE ROUNDABOUT_DATA
  USE STREET_LINKS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(17), IRND, IAP, N, IL, IRL
! ----------------------------------------------------------------------
  READ(STRING, '(17I4)') IBUF
  IRND = IBUF(1)
 
! --- Store the approach links.
 
  IAP = 0
  DO N = 2, 9, 2
    IF(IBUF(N) .EQ. 0) EXIT
    CALL FIND_STREET_LINK(IBUF(N), IBUF(N + 1), IL)
    IF(IL .EQ. 0) THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, I5)') 'RT 97: APPROACH LINK NOT FOUND FOR ROUNDABOUT NUMBER ', IRND
      CALL SENDTEXTMSG(M_ERROR)
      WRITE(MSGTEXT, '(A, 2I5)') '  APPROACH LINK = ', IBUF(N), IBUF(N + 1)
      CALL SENDTEXTMSG(M_ERROR)
    ELSE
      IAP = IAP + 1
      ROUNDABOUT(IRND)%APPROACH_LINKS(IAP) = IL
      ROUNDABOUT(IRND)%APPROACHES = IAP
      ROUNDABOUT_ID(IL) = IRND
      ROUNDABOUT_APPROACH_NUM(IL) = IAP
    ENDIF
  ENDDO
 
! --- Store the exit links.
 
  IAP = 0
  DO N = 10, 17, 2
    IF(IBUF(N) .EQ. 0) EXIT
    CALL FIND_STREET_LINK(IBUF(N), IBUF(N + 1), IL)
    IF(IL .EQ. 0) THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, I5)') 'RT 97: DEPARTING LINK NOT FOUND FOR ROUNDABOUT NUMBER ', IRND
      CALL SENDTEXTMSG(M_ERROR)
      WRITE(MSGTEXT, '(A, 2I5)') '  DEPARTING LINK = ', IBUF(N), IBUF(N + 1)
      CALL SENDTEXTMSG(M_ERROR)
    ELSE
      IAP = IAP + 1
      ROUNDABOUT(IRND)%DEPARTING_LINKS(IAP) = IL
      ROUNDABOUT_ID(IL) = IRND
    ENDIF
  ENDDO  
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

  RETURN
  END
            
! ==================================================================================================
  SUBROUTINE READ_RT98(STRING)
! ----------------------------------------------------------------------
! --- Roundabout Origin-Destination Inputs.
! ----------------------------------------------------------------------
  USE ROUNDABOUT_DATA
  USE STREET_LINKS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(7), IRND, IAP, IL, N, ERROR
  REAL :: TOTAL
! ----------------------------------------------------------------------
  READ(STRING, '(7I4)') IBUF
  IRND = IBUF(1)
  IF(IRND .EQ. 0) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A)') 'RT 98: ROUNDABOUT WAS NOT SPECIFIED: '
    CALL SENDTEXTMSG(M_ERROR)
    WRITE(MSGTEXT, '(A)') STRING
    CALL SENDTEXTMSG(M_ERROR)
  ELSEIF(IRND .GT. NUMBER_OF_ROUNDABOUTS) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A, I5)') 'RT 98: ROUNDABOUT WAS NOT SPECIFIED FOR NUMBER ', IRND
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
    IAP = IBUF(2)
    IF(IAP .LT. 1 .OR. IAP .GT. ROUNDABOUT(IRND)%APPROACHES) THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, I5)') 'RT 98: ERROR IN SPECIFYING APPROACH NUMBER FOR ROUNDABOUT ', IRND
      CALL SENDTEXTMSG(M_ERROR)
      WRITE(MSGTEXT, '(A, I5)') '  APPROACH NUMBER ', IAP
      CALL SENDTEXTMSG(M_ERROR)
    ELSE
      IL = ROUNDABOUT(IRND)%APPROACH_LINKS(IAP)
      TOTAL = SUM(IBUF(3:7))
      ERROR = 100 - TOTAL
      IF(ERROR .NE. 0) THEN
        DO N = 3, 7
          IF(IBUF(N) .NE. 0) THEN
            IBUF(N) = IBUF(N) + ERROR
            EXIT
          ENDIF
        ENDDO
      ENDIF
      DO N = 1, 5 
        ROUNDABOUT(IRND)%EXIT_PCTS(IAP, N) = FLOAT(IBUF(N + 2)) / 100.
      ENDDO
    ENDIF
  ENDIF
  RETURN
  END
            
! ==================================================================================================
  SUBROUTINE READ_RT119(STRING)
! ----------------------------------------------------------------------
! --- Extended Freeway Lane Inputs.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  USE CORSIM_NODES
  USE GLOBAL_DATA
  USE NODE_TABLE
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(19), IAUX, IL, I, N
! ----------------------------------------------------------------------
  READ(STRING, '(2I4,2X,2I2,5(1X,I2,I1,I6))') IBUF
  CALL FIND_FREEWAY_LINK(IBUF(1), IBUF(2), IL)
  IF(IL .EQ. 0) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A, 2I5)') 'RT 119: SPECIFIED LINK NOT FOUND ', IBUF(1), IBUF(2)
    CALL SENDTEXTMSG(M_ERROR)
    RETURN
  ELSE
    FNUMLANES(IL) = IBUF(3)
    IF(IBUF(4) .EQ. 1) THEN
      IAUX = 0
    ELSEIF(IBUF(4) .EQ. 2) THEN
      IAUX = 5
    ELSE
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, I2)') 'RT 119: INVALID SEQUENCE IDENTIFIER ', IBUF(4)
      CALL SENDTEXTMSG(M_ERROR)
      RETURN
    ENDIF
 
! --- Process auxiliary lane inputs.
 
    N = 5
    DO I = 1, 5
      IF(IBUF(N) .EQ. 0) EXIT
      IAUX = IAUX + 1
      AUX_LANE_ID(IL, IAUX) = IBUF(N)
      AUX_LANE_CODE(IL, IAUX) = IBUF(N + 1)
      AUX_LANE_LENGTH(IL, IAUX) = IBUF(N + 2)
      N = N + 3
    ENDDO
  ENDIF
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE READ_RT136(STRING)
! ----------------------------------------------------------------------
! --- Extended Sign or Signal Control Codes.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE TIMED_CONTROLLERS
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(73), IL, AL, NODE, INTERVAL, I, ISIG
! ----------------------------------------------------------------------
  READ(STRING, '(I4, 1X, 72I1)') IBUF
  IF(IBUF(1) .GE. 7000) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A, I4)') 'RT 136: INVALID NODE SPECIFIED ', IBUF(1)
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
    NODE = IBUF(1)
    IF(NODE .NE. 0) THEN
      ISIG = 0
      DO I = 1, NUMBER_OF_FTCS
        IF(FTC_SIGNALS(I)%NODE .EQ. NODE) THEN
          ISIG = I
          EXIT
        ENDIF
      ENDDO
      IF(ISIG .NE. 0) THEN
 
  ! --- Store up to 12 intervals for up to 6 approach links.
 
        I = 1
        DO INTERVAL = 1, FTC_SIGNALS(ISIG)%ACTIVE_INTERVALS
          DO AL = 1, 6
            I = I + 1
            IF(AL .GT. FTC_SIGNALS(ISIG)%APPROACHES) CYCLE
            IL = FTC_SIGNALS(ISIG)%APPROACH(AL)
            IF(IL .NE. 0) THEN
              SIGNAL_CODE(IL) = FTC_SIGNALS(ISIG)%SIGNAL_CODE(AL, 1)
              IF(FTC_SIGNALS(ISIG)%ACTIVE_INTERVALS .EQ. 1) THEN
                IF(IBUF(I) .EQ. 0) IBUF(I) = S_YIELD
                IF(IBUF(I) .EQ. 1) IBUF(I) = S_GREEN
                IF(IBUF(I) .EQ. 5) IBUF(I) = S_STOP
                SIGNAL_CODE(IL) = IBUF(I)
              ELSE
                IF(INTERVAL .EQ. 1) SIGNALIZED(IL) = .TRUE.
              ENDIF
            ENDIF
            FTC_SIGNALS(ISIG)%SIGNAL_CODE(AL, INTERVAL) = IBUF(I)
          ENDDO
        ENDDO
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END 
  
! ==================================================================================================
  SUBROUTINE READ_RT140(STRING)
! ----------------------------------------------------------------------
! --- Left and Right Turning Speeds.
! ----------------------------------------------------------------------
  USE GLOBAL_DATA
  USE STREET_LINKS
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(16), INDEX
! ----------------------------------------------------------------------
  READ(STRING, '(16I4)') IBUF
  IF(IBUF(1) .NE. 0) THEN
    INDEX = IBUF(1)
    LT_JUMPER_PROB(INDEX) = IBUF(2) / 100.
  ENDIF
  IF(IBUF(3) .NE. 0) THEN
    INDEX = IBUF(3)
    LT_JUMPER_PROB(INDEX) = IBUF(4) / 100.
  ENDIF
  IF(IBUF(5) .NE. 0) THEN
    INDEX = IBUF(5)
    LT_JUMPER_PROB(INDEX) = IBUF(6) / 100.
  ENDIF
  IF(IBUF(7) .NE. 0) THEN
    INDEX = IBUF(7)
    LT_JUMPER_PROB(INDEX) = IBUF(8) / 100.
  ENDIF
  IF(IBUF(9) .NE. 0) THEN
    INDEX = IBUF(9)
    LT_JUMPER_PROB(INDEX) = IBUF(10) / 100.
  ENDIF
  IF(IBUF(11) .NE. 0) THEN
    INDEX = IBUF(11)
    LT_JUMPER_PROB(INDEX) = IBUF(12) / 100.
  ENDIF
  IF(IBUF(13) .NE. 0) THEN
    INDEX = IBUF(13)
    LT_JUMPER_PROB(INDEX) = IBUF(14) / 100.
  ENDIF
  
  !Apply the turning speed limitations to all links
  IF(IBUF(15) .NE. 0) LT_SPEED = IBUF(15)
  IF(IBUF(16) .NE. 0) RT_SPEED = IBUF(16)
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE READ_RT141(STRING)
! ----------------------------------------------------------------------
! --- Spillback Probability.
! ----------------------------------------------------------------------
  USE GLOBAL_DATA
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(7)
! ----------------------------------------------------------------------
  READ(STRING, '(7I4)') IBUF
  SPILLBACK_PROB = IBUF(1:4) / 100.
  LT_LAGGER_PROB = IBUF(5:7) / 100.
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE READ_RT142(STRING)
! ----------------------------------------------------------------------
! --- Acceptable Gap in Near-side Cross Traffic for Vehicles at a Sign.
! ----------------------------------------------------------------------
  USE DRIVERS
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(10)
! ----------------------------------------------------------------------
  READ(STRING, '(10I4)') IBUF
  ACCEPTABLE_GAP = IBUF / 10.
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE READ_RT143(STRING)
! ----------------------------------------------------------------------
! --- Additional Time for Far-side Cross Traffic for Vehicles at a Sign.
! ----------------------------------------------------------------------
  USE DRIVERS
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(10)
! ----------------------------------------------------------------------
  READ(STRING, '(10I4)') IBUF
  ADDITIONAL_GAP = IBUF / 10.
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE READ_RT144(STRING)
! ----------------------------------------------------------------------
! --- Amber Response Interval.
! ----------------------------------------------------------------------
  USE DRIVERS
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(10)
! ----------------------------------------------------------------------
  READ(STRING, '(10I4)') IBUF
  AMBER_DECEL = -IBUF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE READ_RT145(STRING)
! ----------------------------------------------------------------------
! --- Gaps for Permitted Left Turns and for RTOR or at signs.
! ----------------------------------------------------------------------
  USE DRIVERS
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(11)
! ----------------------------------------------------------------------
  READ(STRING, '(11I4)') IBUF
  IF(IBUF(1) .EQ. 0) THEN
    ACCEPTABLE_LTG = IBUF(2:11) / 10.
  ELSEIF(IBUF(1) .EQ. 1) THEN
    ACCEPTABLE_RTG = IBUF(2:11) / 10.
  ELSEIF(IBUF(1) .EQ. 2) THEN
    ACCEPTABLE_RABT_GAP = IBUF(2:11) / 10.
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE READ_RT146(STRING)
! ----------------------------------------------------------------------
! --- Pedestrian delay.
! ----------------------------------------------------------------------
  USE DRIVERS
  USE PEDS
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(17), I
! ----------------------------------------------------------------------
  READ(STRING, '(17I4)') IBUF
  IF(IBUF(1) .EQ. 0) THEN
    PDELAY_WEAK(1:10) = IBUF(2:11)
  ELSE
    PDELAY_STRONG(1:10) = IBUF(2:11)
  ENDIF
  DO I = 12, 16, 2
    IF(IBUF(I) .NE. 0) PED_DURATION(IBUF(I)) = IBUF(I+1)
  ENDDO
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE READ_RT147(STRING, INET)
! ----------------------------------------------------------------------
! --- Freeflow Speed Percentages.
! ----------------------------------------------------------------------
  USE DRIVERS
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER, INTENT(IN) :: INET
  INTEGER :: IBUF(10), I
! ----------------------------------------------------------------------
  IF(INET .EQ. 1 .OR. INET .EQ. 2) THEN
    READ(STRING, '(10I4)') IBUF
    DO I = 1, 10
      FFSPEED_ADJ(I, INET) = IBUF(I) / 100.
    ENDDO
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE READ_RT148(STRING)
! ----------------------------------------------------------------------
! --- Short-Term-Event Durations.
! ----------------------------------------------------------------------
  USE EVENTS
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(10), I
! ----------------------------------------------------------------------
  READ(STRING, '(10I4)') IBUF
  DO I = 1, 10
    STE_MULT(I) = IBUF(I) / 100.
  ENDDO
  RETURN
  END
      

! ==================================================================================================
  SUBROUTINE READ_RT149(STRING)
! ----------------------------------------------------------------------
! --- Surface Street Link Distributions.
! ----------------------------------------------------------------------
  USE DRIVERS
  USE DISTRIBUTIONS
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(12), I, N
! ----------------------------------------------------------------------
  READ(STRING, '(12I4)') IBUF
  N = (IBUF(1) - 1) * 10
  IF(IBUF(2) .EQ. 0) THEN
    DO I = 1, 10
      N = N + 1
      STARTUP_MULT(N) = IBUF(I + 2)
    ENDDO
  ELSEIF(IBUF(2) .EQ. 1) THEN
    DO I = 1, 10
      N = N + 1
      HDWY_MULTIPLIER(N) = IBUF(I + 2) / 100.
    ENDDO
  ELSEIF(IBUF(2) .EQ. 2) THEN
    !Multiplier based on position in queue
    DO I = 1, 5
      QFACTOR(I) = IBUF(I + 2) / 100.
    ENDDO
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE READ_RT150(STRING)
! ----------------------------------------------------------------------
! --- Dwell Time Distributions.
! ----------------------------------------------------------------------
  USE BUS_STATION_DATA
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(11)
! ----------------------------------------------------------------------
  READ(STRING, '(11I4)') IBUF
  DWELL_MULTIPLIER(IBUF(1), 1:10) = IBUF(2:11) / 100.
  RETURN
  END
            
! ==================================================================================================
  SUBROUTINE READ_RT153(STRING)
! ----------------------------------------------------------------------
! --- Driver's Familiarity With Path.
! ----------------------------------------------------------------------
  USE GLOBAL_DATA
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF
! ----------------------------------------------------------------------
  READ(STRING, '(I4)') IBUF
  DRIVER_FAMPCT = IBUF / 100.
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE READ_RT154(STRING)
! ----------------------------------------------------------------------
! --- Controllers that operate multiple intersections.
! ----------------------------------------------------------------------
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(8), ERRORS, NSC
! ----------------------------------------------------------------------
  NSC = NSC + 1
  READ(STRING, '(8I4)') IBUF
  CALL STORE_SUPER_CONTROLLER(NSC, IBUF, ERRORS)
  RETURN
  END

! ==================================================================================================
  SUBROUTINE STORE_SUPER_CONTROLLER(NSC, IBUF, ERRORS)
  USE ACTUATED_CONTROLLERS
  USE STREET_NODES
  USE STREET_LINKS
  USE VEHICLE_PARAMETERS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NSC, IBUF(8)
  INTEGER, INTENT(OUT) :: ERRORS
  INTEGER :: IACT, IAP, NAP, ISIG, PHASE, I, IDET, N, IL, NODE
  LOGICAL :: FOUND(8), SERVED
! ----------------------------------------------------------------------
  WRITE154 = .TRUE.
  ERRORS = 0
  IACT = NSC + NUMBER_OF_ACS
  FOUND = .TRUE.
  WRITE(MSGTEXT, '(A)') 'THE FOLLOWING NODES WILL BE OPERATED AS ONE COMBINED ACTUATED CONTROL SYSTEM:'
  CALL SENDTEXTMSG(M_ERROR)
  DO N = 1, 8
    IF(IBUF(N) .LE. 0) EXIT
    FOUND(N) = .FALSE.
    !Identify the nodes to be combined.
    DO ISIG = 1, NUMBER_OF_ACS
      IF(ABS(AC_SIGNALS(ISIG)%NODE(1)) .EQ. IBUF(N)) THEN
        FOUND(N) = .TRUE.
        NACT(IBUF(N)) = IACT
        AC_SIGNALS(IACT)%NODE(N) = IBUF(N)
        AC_SIGNALS(ISIG)%NODE(1) = -IBUF(N)
        WRITE(MSGTEXT, '(2X,I4)') IBUF(N)
        CALL SENDTEXTMSG(M_ERROR)
        !Copy direct approaches to the super controller
        NAP = AC_SIGNALS(IACT)%N_DIRECT_APPROACHES
        DO IAP = 1, AC_SIGNALS(ISIG)%N_DIRECT_APPROACHES
          NAP = NAP + 1
          AC_SIGNALS(IACT)%DIRECT_APPROACH(NAP) = AC_SIGNALS(ISIG)%DIRECT_APPROACH(IAP)
          DO PHASE = 1, 8
            IF(AC_SIGNALS(ISIG)%PHASE(PHASE)%IN_USE) THEN
              AC_SIGNALS(IACT)%PHASE(PHASE)%LEFTARROW(NAP) = AC_SIGNALS(ISIG)%PHASE(PHASE)%LEFTARROW(IAP)
              AC_SIGNALS(IACT)%PHASE(PHASE)%THRUARROW(NAP) = AC_SIGNALS(ISIG)%PHASE(PHASE)%THRUARROW(IAP)
              AC_SIGNALS(IACT)%PHASE(PHASE)%RIGHTARROW(NAP) = AC_SIGNALS(ISIG)%PHASE(PHASE)%RIGHTARROW(IAP)
              AC_SIGNALS(IACT)%PHASE(PHASE)%LDIAGARROW(NAP) = AC_SIGNALS(ISIG)%PHASE(PHASE)%LDIAGARROW(IAP)
              AC_SIGNALS(IACT)%PHASE(PHASE)%RDIAGARROW(NAP) = AC_SIGNALS(ISIG)%PHASE(PHASE)%RDIAGARROW(IAP)
              AC_SIGNALS(IACT)%PHASE(PHASE)%IN_USE = .TRUE. 
            ENDIF
          ENDDO
        ENDDO
        AC_SIGNALS(IACT)%N_DIRECT_APPROACHES = NAP
          
        !Copy detectors to the super controller.
        IDET = AC_SIGNALS(IACT)%SDP%DETECTOR_COUNT
        DO I = 1, AC_SIGNALS(ISIG)%SDP%DETECTOR_COUNT
          IDET = IDET + 1
          AC_SIGNALS(IACT)%SDP%DETECTOR_LIST(IDET) = AC_SIGNALS(ISIG)%SDP%DETECTOR_LIST(I)
        ENDDO
        AC_SIGNALS(IACT)%SDP%DETECTOR_COUNT = AC_SIGNALS(IACT)%SDP%DETECTOR_COUNT + AC_SIGNALS(ISIG)%SDP%DETECTOR_COUNT
          
        !Copy the phase data to the super controller.
        DO PHASE = 1, 8
          IF(AC_SIGNALS(ISIG)%PHASE(PHASE)%IN_USE) THEN
            AC_SIGNALS(IACT)%PHASE(PHASE)%IN_USE = .TRUE.        
            AC_SIGNALS(IACT)%PHASE_SEQUENCE(PHASE) = AC_SIGNALS(ISIG)%PHASE_SEQUENCE(PHASE)
            AC_SIGNALS(IACT)%GUI_MIN_GREEN_TIMES(PHASE) = AC_SIGNALS(ISIG)%GUI_MIN_GREEN_TIMES(PHASE)
            AC_SIGNALS(IACT)%GUI_MAX_GREEN_TIMES(PHASE) = AC_SIGNALS(ISIG)%GUI_MAX_GREEN_TIMES(PHASE)
            AC_SIGNALS(IACT)%GUI_YC(PHASE) = AC_SIGNALS(ISIG)%GUI_YC(PHASE)
            AC_SIGNALS(IACT)%GUI_RC(PHASE) = AC_SIGNALS(ISIG)%GUI_RC(PHASE)
            AC_SIGNALS(IACT)%GUI_ACTUATED_MODE(PHASE) = AC_SIGNALS(ISIG)%GUI_ACTUATED_MODE(PHASE)
            AC_SIGNALS(IACT)%GUI_DEFAULT_EXTENSION_TIMES(PHASE) = AC_SIGNALS(ISIG)%GUI_DEFAULT_EXTENSION_TIMES(PHASE)
            AC_SIGNALS(IACT)%GUI_TIME_TO_REDUCE(PHASE) = AC_SIGNALS(ISIG)%GUI_TIME_TO_REDUCE(PHASE)
            AC_SIGNALS(IACT)%GUI_TIME_BEFORE_REDUCTION(PHASE) = AC_SIGNALS(ISIG)%GUI_TIME_BEFORE_REDUCTION(PHASE)
            AC_SIGNALS(IACT)%GUI_MIN_GAP_TIMES(PHASE) = AC_SIGNALS(ISIG)%GUI_MIN_GAP_TIMES(PHASE)
            AC_SIGNALS(IACT)%GUI_GAP_TIMES(PHASE) = AC_SIGNALS(ISIG)%GUI_GAP_TIMES(PHASE)
          ENDIF
        ENDDO          
      ENDIF
    ENDDO
  ENDDO
    
  !Determine if any movement on any approach to any of the nodes defined by this controller does not have a phase that serves it
  DO I = 1, 8
    NODE = AC_SIGNALS(IACT)%NODE(I)
    IF(NODE .EQ. 0) EXIT
    DO IL = 1, N_STREET_LINKS
      IF(SDSN(IL) .EQ. NODE) THEN
        DO IAP = 1, AC_SIGNALS(IACT)%N_DIRECT_APPROACHES
          IF(IL .EQ. AC_SIGNALS(IACT)%DIRECT_APPROACH(IAP)) THEN
            IF(FTC_SIGNAL_ID(IL) .NE. 0) CYCLE
            IF(LEFT_LINK(IL) .NE. 0) THEN
              SERVED = .FALSE.
              DO PHASE = 1, 8
                IF(AC_SIGNALS(IACT)%PHASE(PHASE)%LEFTARROW(IAP)) THEN
                  SERVED = .TRUE.
                  EXIT
                ENDIF
              ENDDO
              IF(.NOT. SERVED) THEN
                ALWAYS_GREEN(IL, TC_LEFT + 1) = .TRUE.
                WRITE(MSGTEXT, '(A,2I4,A)') '  Left movement on link ', SUSN(IL), SDSN(IL), ' is flagged for perpetual green'
                CALL SENDTEXTMSG(M_ERROR)
              ENDIF
            ENDIF
            IF(STHRU_LINK(IL) .NE. 0) THEN
              SERVED = .FALSE.
              DO PHASE = 1, 8
                IF(AC_SIGNALS(IACT)%PHASE(PHASE)%THRUARROW(IAP)) THEN
                  SERVED = .TRUE.
                  EXIT
                ENDIF
              ENDDO
              IF(.NOT. SERVED) THEN
                ALWAYS_GREEN(IL, TC_THRU + 1) = .TRUE.
                WRITE(MSGTEXT, '(A,2I4,A)') '  Thru movement on link ', SUSN(IL), SDSN(IL), ' is flagged for perpetual green'
                CALL SENDTEXTMSG(M_ERROR)
              ENDIF
            ENDIF
            IF(RIGHT_LINK(IL) .NE. 0) THEN
              SERVED = .FALSE.
              DO PHASE = 1, 8
                IF(AC_SIGNALS(IACT)%PHASE(PHASE)%RIGHTARROW(IAP)) THEN
                  SERVED = .TRUE.
                  EXIT
                ENDIF
              ENDDO
              IF(.NOT. SERVED) THEN
                ALWAYS_GREEN(IL, TC_RIGHT + 1) = .TRUE.
                WRITE(MSGTEXT, '(A,2I4,A)') '  Right movement on link ', SUSN(IL), SDSN(IL), ' is flagged for perpetual green'
                CALL SENDTEXTMSG(M_ERROR)
              ENDIF
            ENDIF
            IF(LEFT_DIAG_LINK(IL) .NE. 0) THEN
              SERVED = .FALSE.
              DO PHASE = 1, 8
                IF(AC_SIGNALS(IACT)%PHASE(PHASE)%LDIAGARROW(IAP)) THEN
                  SERVED = .TRUE.
                  EXIT
                ENDIF
              ENDDO
              IF(.NOT. SERVED) THEN
                ALWAYS_GREEN(IL, TC_LDIAG + 1) = .TRUE.
                WRITE(MSGTEXT, '(A,2I4,A)') '  Left Diagonal movement on link ', SUSN(IL), SDSN(IL), ' is flagged for perpetual green'
                CALL SENDTEXTMSG(M_ERROR)
              ENDIF
            ENDIF
            IF(RIGHT_DIAG_LINK(IL) .NE. 0) THEN
              SERVED = .FALSE.
              DO PHASE = 1, 8
                IF(AC_SIGNALS(IACT)%PHASE(PHASE)%RDIAGARROW(IAP)) THEN
                  SERVED = .TRUE.
                  EXIT
                ENDIF
              ENDDO
              IF(.NOT. SERVED) THEN
                ALWAYS_GREEN(IL, TC_RDIAG + 1) = .TRUE.
                WRITE(MSGTEXT, '(A,2I4,A)') '  Right Diagonal movement on link ', SUSN(IL), SDSN(IL), ' is flagged for perpetual green'
                CALL SENDTEXTMSG(M_ERROR)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
    ENDDO
  ENDDO
  DO N = 1, 8
    IF(.NOT. FOUND(N)) THEN
      ERRORS = ERRORS + 1
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A)') 'RT 154: NODE NOT FOUND'
      CALL SENDTEXTMSG(M_ERROR)
      WRITE(MSGTEXT, '(A, I4)') '  NODE = ', IBUF(N)
      CALL SENDTEXTMSG(M_ERROR)
    ENDIF
  ENDDO
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE READ_RT170(STRING, INET)
! ----------------------------------------------------------------------
! --- Sub-network Delimiter.
! ----------------------------------------------------------------------
  USE GLOBAL_DATA
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER, INTENT(OUT) :: INET
  INTEGER :: IBUF
! ----------------------------------------------------------------------
  READ(STRING, '(I4)') IBUF
  IF(IBUF .EQ. 0) THEN
    INET = 0
  ELSEIF(IBUF .EQ. 3) THEN
    INET = I_STREET
  ELSEIF(IBUF .EQ. 8) THEN
    INET = I_FREEWAY
  ELSE
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A)') 'RT 170: INVALID CODE FOR NEXT SECTION'
    CALL SENDTEXTMSG(M_ERROR)
    WRITE(MSGTEXT, '(A, I5)') '  CODE = ', IBUF
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE READ_RT171(STRING)
! ----------------------------------------------------------------------
! --- Additional link inputs.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  USE STREET_LINKS
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(15), I, IL
! ----------------------------------------------------------------------
  READ(STRING, '(2I4, 12I2, I4)') IBUF
  CALL FIND_FREEWAY_LINK(IBUF(1), IBUF(2), IL)
  IF(IL .NE. 0) THEN
 
! --- Store shoulder width and lane widths for freeway links.
 
    IF(IBUF(3) .NE. 0) FSHOULDER_WIDTH(IL) = IBUF(3)
    DO I = 1, 11
      IF(IBUF(I+3) .NE. 0) FLANE_WIDTH(IL, I) = IBUF(I+3)
    ENDDO
 
  ELSE
    CALL FIND_STREET_LINK(IBUF(1), IBUF(2), IL)
    IF(IL .NE. 0) THEN
 
! --- Store shoulder width and lane widths for street links.
 
      IF(IBUF(3) .NE. 0) SSHOULDER_WIDTH(IL) = IBUF(3)
      DO I = 1, 7
        IF(IBUF(I+3) .NE. 0) SLANE_WIDTH(IL, I) = IBUF(I+3)
      ENDDO
      
! --- Store the width of the upstream intersection for a street link.

      IF(IBUF(15) .NE. 0) UP_INT_WIDTH(IL) = IBUF(15)
 
    ENDIF
  ENDIF      
  RETURN
  END

! ==================================================================================================
  SUBROUTINE READ_RT173(STRING)
! ----------------------------------------------------------------------
! --- Maximum Acceleration Tables.
! ----------------------------------------------------------------------
  USE VDATA
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(15), I, J, N
  REAL :: A(0:11)
! ----------------------------------------------------------------------
  READ(STRING, '(15I4)') IBUF
  IF(IBUF(1) .EQ. 0) THEN
 
! --- Get 11 input values.
       
    DO I = 0, 11
      A(I) = IBUF(I + 4) / 100.
    ENDDO
 
! --- Interpolate between input values to fill the table.
         
    N = 0
    DO I = 0, 10
      DO J = 0, 9
        ACCEL_TABLE(N, IBUF(2)) = A(I) + (A(I+1) - A(I)) * J / 10.
        N = N + 1
      ENDDO
    ENDDO
    ACCEL_TABLE(110, IBUF(2)) = A(11)
  ELSEIF(IBUF(1) .EQ. 1) THEN
 
! --- Get 11 input values.
       
    DO I = 0, 11
      A(I) = IBUF(I + 4) / 100.
    ENDDO
 
! --- Interpolate between input values to fill the table.
         
    N = 0
    DO I = 0, 10
      DO J = 0, 9
        GRADE_TABLE(N, IBUF(2)) = A(I) + (A(I+1) - A(I)) * J / 10.
        N = N + 1
      ENDDO
    ENDDO
    GRADE_TABLE(110, IBUF(2)) = A(11)
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE READ_RT174(STRING)
! ----------------------------------------------------------------------
! --- Vehicle type exclusions by lane.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  USE STREET_LINKS
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(3), I, IL, CODE(36)
! ----------------------------------------------------------------------
  READ(STRING, '(2I4, I2, 1X, 36I1)') IBUF, CODE
 
! --- Store vehicle types excluded by lane.
 
  IF(IBUF(3) .GE. 1 .AND. IBUF(3) .LE. NTYPES) THEN
    CALL FIND_FREEWAY_LINK(IBUF(1), IBUF(2), IL)
    IF(IL .NE. 0) THEN
      DO I = 1, N_FREEWAY_LANES
        IF(CODE(I) .EQ. 1) THEN
          FXCLUDE_TYPE(IL, I, IBUF(3)) = .TRUE.
        ENDIF
      ENDDO
    ELSE
      CALL FIND_STREET_LINK(IBUF(1), IBUF(2), IL)
      IF(IL .NE. 0) THEN
        DO I = 1, N_STREET_LANES
          IF(CODE(I) .EQ. 1) THEN
            SXCLUDE_TYPE(IL, I, IBUF(3)) = .TRUE.
          ENDIF
        ENDDO
      ENDIF
    ENDIF
  ENDIF      
  RETURN
  END

! ==================================================================================================
  SUBROUTINE READ_RT185(STRING)
! ----------------------------------------------------------------------
! --- Bus Stations.
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE BUS_STATION_DATA
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(7), IL, ISTAT, NS, I
! ----------------------------------------------------------------------
  READ(STRING, '(I2,1X,I1,3I4,2(1X,I1))') IBUF
  IF(.NOT. ALLOCATED(BUS_STATION_LIST)) THEN
! --- The station number is limited to 99 because of the CORSIM input record structure.
    NUMBER_OF_BUSSTATIONS = 99
    CALL ALLOCATE_BUS_STATION_ARRAYS
  ENDIF
  NS = IBUF(1)
  IF(NS .LT. 1 .OR. NS .GT. 99) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A)') 'RT 185: ILLEGAL BUS STATION NUMBER'
    CALL SENDTEXTMSG(M_ERROR)
    WRITE(MSGTEXT, '(A, 2I5)') '  BUS STATION NUMBER = ', NS
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
    BUS_STATION_LIST(NS)%BLOCK_CODE = IBUF(2)
    CALL FIND_STREET_LINK(IBUF(3), IBUF(4), IL)
    IF(IL .EQ. 0) THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, I4)') 'RT 185: LINK NOT FOUND FOR BUS STATION ', NS
      CALL SENDTEXTMSG(M_ERROR)
      WRITE(MSGTEXT, '(A, 2I5)') '  LINK = ', IBUF(3), IBUF(4)
      CALL SENDTEXTMSG(M_ERROR)
    ELSE
      BUS_STATION_LIST(NS)%LINK = IL
      BUS_STATION_LIST(NS)%POCKET_LANE = 0
      IF(IBUF(5) .LE. 50 .AND. NS .LT. 64) THEN
        IF(RIGHT_LINK(IL) .NE. 0) THEN
          NUMBER_RIGHTPOCKETS(IL) = 1
          BUS_STATION_LIST(NS)%POCKET_LANE = 7 - NUMBER_LEFTPOCKETS(IL)
        ENDIF
      ENDIF
      BUS_STATION_LIST(NS)%LOCATION = SLENGTH(IL) - IBUF(5)
      BUS_STATION_LIST(NS)%CAPACITY = IBUF(6)
      BUS_STATION_LIST(NS)%TYPE_CODE = 1
      IF(IBUF(7) .NE. 0) BUS_STATION_LIST(NS)%TYPE_CODE = IBUF(7)
      IF(FIRST_BUS_STATION(IL) .EQ. 0) THEN
        FIRST_BUS_STATION(IL) = NS
      ELSE
        ISTAT = FIRST_BUS_STATION(IL)
        DO WHILE(ISTAT .NE. 0)
          I = ISTAT
          ISTAT = BUS_STATION_LIST(NS)%NEXT_STATION
        ENDDO
        BUS_STATION_LIST(I)%NEXT_STATION = NS
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END
            
! ==================================================================================================
  SUBROUTINE READ_RT186(STRING)
! ----------------------------------------------------------------------
! --- Bus Dwell Times.
! ----------------------------------------------------------------------
  USE BUS_STATION_DATA
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(3), NS
! ----------------------------------------------------------------------
  READ(STRING, '(3I4)') IBUF
  NS = IBUF(1)
  IF(NS .LT. 1 .OR. NS .GT. 99) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A)') 'RT 186: INVALID BUS STATION NUMBER'
    CALL SENDTEXTMSG(M_ERROR)
    WRITE(MSGTEXT, '(A, 2I5)') '  BUS STATION NUMBER = ', NS
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
    BUS_STATION_LIST(NS)%DWELL = IBUF(2)
    BUS_STATION_LIST(NS)%BYPASS_PCT = IBUF(3) / 100.
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE READ_RT187(STRING)
! ----------------------------------------------------------------------
! --- Bus Routes.
! ----------------------------------------------------------------------
  USE BUS_ROUTE_DATA
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(19), I, THISROUTE, N
  INTEGER, SAVE :: LASTROUTE
! ----------------------------------------------------------------------
  READ(STRING, '(19I4)') IBUF
  THISROUTE = IBUF(1)
  I = 2
  IF(THISROUTE .EQ. LASTROUTE) THEN
    N = BUSR_NNODES(THISROUTE)
  ELSE
    N = 0
  ENDIF
  DO WHILE(IBUF(I) .NE. 0)
    BUSR_NNODES(THISROUTE) = BUSR_NNODES(THISROUTE) + 1
    N = N + 1
    BUSR_ROUTE_NODES(THISROUTE, N) = IBUF(I)
    I = I + 1
    IF(I .GT. 19) EXIT
  ENDDO
  LASTROUTE = THISROUTE
  RETURN
  END

! ==================================================================================================
  SUBROUTINE READ_RT188(STRING)
! ----------------------------------------------------------------------
! --- Bus Stations on Specific Routes.
! ----------------------------------------------------------------------
  USE BUS_ROUTE_DATA
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(35), IROUTE, I
! ----------------------------------------------------------------------
  READ(STRING, '(1X,I3,4X,34I2)') IBUF
  IROUTE = IBUF(1)
  IF(IROUTE .LT. 1 .OR. IROUTE .GT. NUMBER_OF_ROUTES) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A)') 'RT 188: INVALID BUS ROUTE NUMBER'
    CALL SENDTEXTMSG(M_ERROR)
    WRITE(MSGTEXT, '(A, 2I5)') '  BUS ROUTE NUMBER = ', IROUTE
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
    DO I = 1, 34
      BUSR_STATIONLIST(IROUTE, I) = IBUF(I + 1)
    ENDDO
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE READ_RT189(STRING)
! ----------------------------------------------------------------------
! --- Bus Headways.
! ----------------------------------------------------------------------
  USE BUS_ROUTE_DATA
  USE SIMPARAMS
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(3), IROUTE
! ----------------------------------------------------------------------
  READ(STRING, '(3I4)') IBUF
  IROUTE = IBUF(1)
  IF(IROUTE .LT. 1 .OR. IROUTE .GT. NUMBER_OF_ROUTES) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A)') 'RT 189: INVALID BUS ROUTE NUMBER'
    CALL SENDTEXTMSG(M_ERROR)
    WRITE(MSGTEXT, '(A, 2I5)') '  BUS ROUTE NUMBER = ', IROUTE
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
    BUSR_HDWY(IROUTE) = IBUF(2)
    BUSR_OFFSET(IROUTE) = IBUF(3)
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE READ_RT195(STRING)
! ----------------------------------------------------------------------
! --- Node coordinates.
! ----------------------------------------------------------------------
  USE NODE_TABLE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(3)
! ----------------------------------------------------------------------
  READ(STRING, '(I4,2I8)') IBUF
  X195(IBUF(1)) = IBUF(2)
  Y195(IBUF(1)) = IBUF(3)
  IS_DEFINED(IBUF(1)) = .TRUE.
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE READ_RT196(STRING)
! ----------------------------------------------------------------------
! --- Optional Link Geometric Data.
! ----------------------------------------------------------------------
  USE NODE_TABLE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(20)
! ----------------------------------------------------------------------
  !READ(STRING, '(2I4,I3,I1,16I4)') IBUF
  !Store inputs to write out later if needed.
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE READ_RT197(STRING)
! ----------------------------------------------------------------------
! --- Roundabout Specification.
! ----------------------------------------------------------------------
  USE ROUNDABOUT_DATA
  USE NODE_TABLE
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(2), IRND
! ----------------------------------------------------------------------
  READ(STRING, '(2I4)') IBUF
  IRND = IBUF(1)
  IF(IRND .LT. 1 .OR. IRND .GT. NUMBER_OF_ROUNDABOUTS) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A,I4)') 'RT 197: INVALID ROUNDABOUT ID', IRND
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
    !Read dimensions of roundabout and store to write out later if needed.
    ROUNDABOUT(IRND)%RADIUS = IBUF(2)
  ENDIF
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE READ_RT201(STRING)
! ----------------------------------------------------------------------
! --- Vehicle Type Specification for Car Following Models.
! ----------------------------------------------------------------------
  USE VEHICLE_TYPES
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(5), ITYPE
! ----------------------------------------------------------------------
  WRITE201 = .TRUE.
  READ(STRING, '(5I4)') IBUF
  ITYPE = IBUF(1)
  IF(SUM(IBUF(2:5)) .NE. 100) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A,I2)') 'RT 201: CAR FOLLOWING PERCENTAGES DO NOT TOTAL 100 FOR VEHICLE TYPE ', ITYPE
    CALL SENDTEXTMSG(M_ERROR)
  ELSE    
    PCT_PITT(ITYPE) = IBUF(2) / 100.
    PCT_IDM(ITYPE) = IBUF(3) / 100.
    PCT_ACC(ITYPE) = IBUF(4) / 100.
    PCT_CACC(ITYPE) = IBUF(5) / 100.
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE READ_RT202(STRING)
! ----------------------------------------------------------------------
! --- Inputs for Advanced Technology Algorithms.
! ----------------------------------------------------------------------
  USE CAR_FOLLOWING
  USE TEXT
  IMPLICIT NONE
  CHARACTER*80, INTENT(IN) :: STRING
  INTEGER :: IBUF(6)
! ----------------------------------------------------------------------
  WRITE202 = .TRUE.
  READ(STRING, '(6I4)') IBUF
  IF(IBUF(1) .EQ. 1) THEN
    ACC_TG = IBUF(2) / 100.
    ACC_AMAX = IBUF(3) / 100.
    ACC_DMAX = IBUF(4) / 100.
    ACC_K1 = IBUF(5) / 100.
    ACC_K2 = IBUF(6) / 100.
  ELSEIF(IBUF(1) .EQ. 2) THEN
    CACC_TG = IBUF(2) / 100.
    CACC_AMAX = IBUF(3) / 100.
    CACC_DMAX = IBUF(4) / 100.
    CACC_K1 = IBUF(5) / 100.
    CACC_K2 = IBUF(6) / 100.
  ELSE    
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A,I2)') 'RT 202: UNRECOGNIZED CAR FOLLOWING MODEL TYPE ', IBUF(1)
    CALL SENDTEXTMSG(M_ERROR)
  ENDIF
  RETURN
  END
      
! ==========================================================================================================
  SUBROUTINE READ_TIME_VARYING_INPUT
! ----------------------------------------------------------------------
! --- This routine reads card types 21, 23, 26, 50, 51 and 53. It stores
! --- the data in the modules that are used for emitting vehicles and
! --- determining turn percentages.
! ----------------------------------------------------------------------
  USE TURNDATA_MOD
  USE FLOWDATA_MOD
  USE TEXT
  USE SIMPARAMS
  USE FREEWAY_LINKS
  USE STREET_LINKS
  IMPLICIT NONE
  INCLUDE 'IOFILES.INC'
  CHARACTER*2047 :: LINE
  INTEGER :: PCT(16), NPCT(4)
  INTEGER :: ENDOFPERIOD(0:19), TPLENGTH(19), T(8), V(8), SSNOD
  INTEGER :: I, IL, CARDTYPE, TPCOUNT, TOTAL, UP, DOWN, N, FLOW
  INTEGER :: ANINDEX, START, ITIME, J, IND
  INTEGER :: NETWORK, LINKID, LANES, CODE, LASTTIME, NCOUNTS, SUBTIM
  REAL :: RPCT(2)
  LOGICAL :: WREAD3, W53, W51, WPRINT
  DATA WREAD3 /.FALSE./
! ----------------------------------------------------------------------
  OPEN(65, FILE = LINFNAME(1:IROOT)//'TRF', STATUS='OLD', ERR=10, IOMSG=ETEXT)
  REWIND(65)
  TPCOUNT = 1
  NUMBER_OF_ENTRYNODES = 0
  W53 = .FALSE.
  W51 = .FALSE.
1 READ(65,'(A80)',END=2, ERR=20, IOMSG=ETEXT) LINE
  DO I = 78, 80
    IF(ICHAR(LINE(I:I)) .LT. 48 .OR. ICHAR(LINE(I:I)) .GT. 57) THEN
      IF(LINE(I:I) .NE. ' ') GOTO 1
    ENDIF
  ENDDO
  IF(WREAD3) THEN
    READ(LINE(78:80),'(BZ,I3)') CARDTYPE
  ELSE
    READ(LINE(79:80),'(BZ,I2)') CARDTYPE
  ENDIF
  IF(CARDTYPE .EQ. 3) THEN
    WREAD3 = .TRUE.
    READ(LINE,'(BZ,19I4)') (TPLENGTH(I),I=1,19)
    I = 1
    ENDOFPERIOD(0) = 0
    TOTAL = 0                                                      
    DO WHILE(I .LE. 19)
      IF(TPLENGTH(I) .EQ. 0) EXIT
      TOTAL = TOTAL + TPLENGTH(I)                                  
      ENDOFPERIOD(I) = TOTAL / 60                                  
      I = I + 1
    ENDDO

  ELSEIF(CARDTYPE .EQ. 23) THEN
    IF(.NOT. ALLOCATED(TURNDATA)) ALLOCATE(TURNDATA(N_FREEWAY_LINKS + N_STREET_LINKS))
    W23 = .TRUE.
    READ(LINE,'(BZ,19I4)') UP, DOWN, T(1), (PCT(I), I=1,4),   &
                                     T(2), (PCT(I), I=5,8),   &
                                     T(3), (PCT(I), I=9,12)
    CALL FIND_STREET_LINK(UP, DOWN, IL)

! Check input for times that are not in the current period

    DO I = 1, 3
      N = 4 * (I - 1)
      TOTAL = PCT(N+1) + PCT(N+2) + PCT(N+3) + PCT(N+4)
      IF(TOTAL .NE. 0) THEN
        IF(T(I) .LT. ENDOFPERIOD(TPCOUNT-1)) THEN
          MSGTEXT = 'TIME BEFORE START OF PERIOD ON CARD TYPE 23'
          CALL SENDTEXTMSG(M_ERROR)
          MSGTEXT = LINE
          CALL SENDTEXTMSG(M_ERROR)
          CALL NEW_ERROR
          RETURN
        ENDIF
        IF(T(I) .GT. ENDOFPERIOD(TPCOUNT)) THEN
          MSGTEXT = 'TIME BEYOND END OF PERIOD ON CARD TYPE 23'
          CALL SENDTEXTMSG(M_ERROR)
          MSGTEXT = LINE
          CALL SENDTEXTMSG(M_ERROR)
          CALL NEW_ERROR
          RETURN
        ENDIF
      ENDIF
    ENDDO
 
! Check for a previous entry for this link
 
    DO I = 1, NTURNDATA
      IF(UP .EQ. TURNDATA(I)%UP .AND. DOWN .EQ. TURNDATA(I)%DOWN) THEN
        ITIME = 1
        J = 0
 
!  If the previous entry was from ct21 and still in the same time period,
!  use data from this card to overwrite the previous data
 
        IF(TURNDATA(I)%CARDTYPE .EQ. 21) THEN
          IND = TURNDATA(I)%INDEX
          IF(TURNDATA(I)%TIME(IND) .EQ. ENDOFPERIOD(TPCOUNT)) TURNDATA(I)%INDEX = IND - 2
        ENDIF
        TURNDATA(I)%CARDTYPE = 23
        TOTAL = PCT(1) + PCT(2) + PCT(3) + PCT(4)
        DO WHILE(ITIME .LE. 3 .AND. TOTAL .GT. 0)
          DO N = 1, 4                                               
            NPCT(N) = PCT(J+N)                                      
          ENDDO                                                     
          CALL ADJPCT(NPCT)                                 
          TURNDATA(I)%INDEX = TURNDATA(I)%INDEX + 1
          ANINDEX = TURNDATA(I)%INDEX
          TURNDATA(I)%TIME(ANINDEX) = T(ITIME)
          TURNDATA(I)%RPCT(1,ANINDEX) = FLOAT(NPCT(1)) / TOTAL
          TURNDATA(I)%RPCT(2,ANINDEX) = FLOAT(NPCT(2)) / TOTAL
          TURNDATA(I)%RPCT(3,ANINDEX) = FLOAT(NPCT(3)) / TOTAL
          TURNDATA(I)%RPCT(4,ANINDEX) = FLOAT(NPCT(4)) / TOTAL
          ITIME = ITIME + 1
          J = J + 4
          TOTAL = PCT(J+1) + PCT(J+2) + PCT(J+3) + PCT(J+4)
        ENDDO
        GOTO 1
      ENDIF
    ENDDO
 
! Increment the number of NTURNDATA if this is a new link
 
    NTURNDATA = NTURNDATA + 1
    TURNDATA(NTURNDATA)%CARDTYPE = 23
    TURNDATA(NTURNDATA)%UP = UP
    TURNDATA(NTURNDATA)%DOWN = DOWN
    ITIME = 1
    J = 0
    TURNDATA(I)%INDEX = 0
    TOTAL = PCT(1) + PCT(2) + PCT(3) + PCT(4)
    DO WHILE(ITIME .LE. 3 .AND. TOTAL .GT. 0)
      DO N = 1, 4                                                  
        NPCT(N) = PCT(J+N)                                         
      ENDDO                                                        
      CALL ADJPCT(NPCT)                                    
      TURNDATA(I)%INDEX = TURNDATA(I)%INDEX + 1
      ANINDEX = TURNDATA(I)%INDEX
      TURNDATA(I)%TIME(ANINDEX) = T(ITIME)
      TURNDATA(I)%RPCT(1,ANINDEX) = FLOAT(NPCT(1)) / TOTAL
      TURNDATA(I)%RPCT(2,ANINDEX) = FLOAT(NPCT(2)) / TOTAL
      TURNDATA(I)%RPCT(3,ANINDEX) = FLOAT(NPCT(3)) / TOTAL
      TURNDATA(I)%RPCT(4,ANINDEX) = FLOAT(NPCT(4)) / TOTAL
      ITIME = ITIME + 1
      J = J + 4
      TOTAL = PCT(J+1) + PCT(J+2) + PCT(J+3) + PCT(J+4)
    ENDDO

  ELSEIF(CARDTYPE .EQ. 26) THEN
    IF(.NOT. ALLOCATED(TURNDATA)) CALL ALLOCATE_TURNDATA(N_FREEWAY_LINKS + N_STREET_LINKS)
    W26 = .TRUE.
    READ(LINE,'(BZ,20I4)') UP, DOWN, T(1), (PCT(I), I=1,2),   &
                           T(2), (PCT(I), I=3,4),             &
                           T(3), (PCT(I), I=5,6),             &
                           T(4), (PCT(I), I=7,8),             &
                           T(5), (PCT(I), I=9,10)
    CALL FIND_FREEWAY_LINK(UP, DOWN, IL)
 
! --- Check input for times that are not in the current period
! --- and check for negative turn percentages.
 
    DO I = 1, 5
      N = 2 * (I - 1)
      TOTAL = ABS(PCT(N+1)) + ABS(PCT(N+2))
      IF(TOTAL .NE. 0) THEN
        IF(T(I) .LT. ENDOFPERIOD(TPCOUNT-1)) THEN
          MSGTEXT = 'TIME BEFORE START OF PERIOD ON CARD TYPE 26'
          CALL SENDTEXTMSG(M_ERROR)
          MSGTEXT = LINE
          CALL SENDTEXTMSG(M_ERROR)
          CALL NEW_ERROR
          GOTO 2
        ENDIF
        IF(T(I) .GT. ENDOFPERIOD(TPCOUNT)) THEN
          MSGTEXT = 'TIME BEYOND END OF PERIOD ON CARD TYPE 26'
          CALL SENDTEXTMSG(M_ERROR)
          MSGTEXT = LINE
          CALL SENDTEXTMSG(M_ERROR)
          CALL NEW_ERROR
          GOTO 2
        ENDIF
        IF(PCT(I) .LT. 0) THEN
          MSGTEXT = 'NEGATIVE TURN PERCENTAGE ON CARD TYPE 26'
          CALL SENDTEXTMSG(M_ERROR)
          MSGTEXT = LINE
          CALL SENDTEXTMSG(M_ERROR)
          CALL NEW_ERROR
          GOTO 2
        ENDIF
      ENDIF
    ENDDO
 
! --- Check for a previous entry for this link.
 
    DO I = 1, NTURNDATA
      IF(UP .EQ. TURNDATA(I)%UP .AND. DOWN .EQ. TURNDATA(I)%DOWN) THEN
        ITIME = 1
        J = 0
 
! --- If the previous entry was from ct25 and still in the same time period,
! --- use data from this card to overwrite the previous data.
 
        IF(TURNDATA(I)%CARDTYPE .EQ. 25) THEN
          IND = TURNDATA(I)%INDEX
          IF(TURNDATA(I)%TIME(IND) .EQ. ENDOFPERIOD(TPCOUNT)) TURNDATA(I)%INDEX = IND - 2
        ENDIF
        TURNDATA(I)%CARDTYPE = 26
        TOTAL = PCT(1) + PCT(2)
        DO WHILE(ITIME .LE. 5 .AND. TOTAL .GT. 0)                  
          DO N = 1, 2
            RPCT(N) = FLOAT(PCT(J+N)) / TOTAL
          ENDDO
          TURNDATA(I)%INDEX = TURNDATA(I)%INDEX + 1
          ANINDEX = TURNDATA(I)%INDEX
          TURNDATA(I)%TIME(ANINDEX) = T(ITIME)
          TURNDATA(I)%RPCT(1,ANINDEX) = RPCT(1)
          TURNDATA(I)%RPCT(2,ANINDEX) = RPCT(2)
          ITIME = ITIME + 1
          J = J + 2
          TOTAL = PCT(J+1) + PCT(J+2)
        ENDDO
        GOTO 1
      ENDIF
    ENDDO
 
! --- Increment the number of NTURNDATA if this is a new link.
 
    NTURNDATA = NTURNDATA + 1
    TURNDATA(NTURNDATA)%CARDTYPE = 26
    TURNDATA(NTURNDATA)%UP = UP
    TURNDATA(NTURNDATA)%DOWN = DOWN
    ITIME = 1
    J = 0
    TURNDATA(I)%INDEX = 0
    TOTAL = PCT(1) + PCT(2)
    DO WHILE(ITIME .LE. 5 .AND. TOTAL .GT. 0)                     
      DO N = 1, 2
        RPCT(N) = FLOAT(PCT(J+N)) / TOTAL
      ENDDO
      TURNDATA(I)%INDEX = TURNDATA(I)%INDEX + 1
      ANINDEX = TURNDATA(I)%INDEX
      TURNDATA(I)%TIME(ANINDEX) = T(ITIME)
      TURNDATA(I)%RPCT(1,ANINDEX) = RPCT(1)
      TURNDATA(I)%RPCT(2,ANINDEX) = RPCT(2)
      ITIME = ITIME + 1
      J = J + 2
      TOTAL = PCT(J+1) + PCT(J+2)
    ENDDO

  ELSEIF(CARDTYPE .EQ. 50) THEN
    READ(LINE,'(BZ,3I4)') UP, DOWN, FLOW
    CALL GET_LINKDATA(UP, DOWN, NETWORK, LINKID, LANES)
 
! --- Check for a previous entry for this link.
 
    DO I = 1, NUMBER_OF_ENTRYNODES
      IF(UP .EQ. ENTRYLINK(I)%UP .AND. DOWN .EQ. ENTRYLINK(I)%DOWN) THEN
        ENTRYLINK(I)%CARDTYPE = CARDTYPE
        ANINDEX = ENTRYLINK(I)%INDEX
        ANINDEX = ANINDEX + 1
        ENTRYLINK(I)%TIME(ANINDEX) = ENDOFPERIOD(TPCOUNT-1)
        ENTRYLINK(I)%FLOW(ANINDEX) = FLOW
        ANINDEX = ANINDEX + 1
        ENTRYLINK(I)%TIME(ANINDEX) = ENDOFPERIOD(TPCOUNT)
        ENTRYLINK(I)%FLOW(ANINDEX) = FLOW
        ENTRYLINK(I)%INDEX = ANINDEX
        GOTO 1
      ENDIF
    ENDDO
 
! --- Increment the number of entries if this is a new link.
 
    NUMBER_OF_ENTRYNODES = NUMBER_OF_ENTRYNODES + 1
    ENTRYLINK(I)%INTERPOLATE = .TRUE.                               
    ENTRYLINK(NUMBER_OF_ENTRYNODES)%CARDTYPE = CARDTYPE
    ENTRYLINK(NUMBER_OF_ENTRYNODES)%UP = UP
    ENTRYLINK(NUMBER_OF_ENTRYNODES)%DOWN = DOWN
    IF(TPCOUNT .EQ. 1) THEN
      START = 0
    ELSE
      START = ENDOFPERIOD(TPCOUNT-1)
    ENDIF
 
! --- Define time and flow rate at beginning and end of period TPCOUNT.
 
    ENTRYLINK(NUMBER_OF_ENTRYNODES)%TIME(1) = START
    ENTRYLINK(NUMBER_OF_ENTRYNODES)%FLOW(1) = FLOW
    ENTRYLINK(NUMBER_OF_ENTRYNODES)%TIME(2) = ENDOFPERIOD(TPCOUNT)
    ENTRYLINK(NUMBER_OF_ENTRYNODES)%FLOW(2) = FLOW
    ENTRYLINK(NUMBER_OF_ENTRYNODES)%INDEX = 2
    ENTRYLINK(NUMBER_OF_ENTRYNODES)%LINKID = LINKID
    ENTRYLINK(NUMBER_OF_ENTRYNODES)%LANES = LANES
    ENTRYLINK(NUMBER_OF_ENTRYNODES)%NETWORK = NETWORK

  ELSEIF(CARDTYPE .EQ. 51 .OR. CARDTYPE .EQ. 53) THEN
    IF(CARDTYPE .EQ. 51) THEN
      W51 = .TRUE.
      READ(LINE,'(BZ,19I4)') SSNOD, UP, DOWN, (V(I), T(I), I=1,8)
      IF(T(1) .EQ. 0 .AND. LINE(20:20) .EQ. ' ') THEN               
        T(1) = ENDOFPERIOD(TPCOUNT-1)
        IF(T(2) .EQ. 0) THEN               
          V(2) = V(1)
          T(2) = ENDOFPERIOD(TPCOUNT)
        ENDIF
      ENDIF                                                         
    ELSE
      W53 = .TRUE.
      READ(LINE,'(BZ,19I4)') UP, DOWN, (V(I), T(I), I=1,8), CODE
    ENDIF
    CALL GET_LINKDATA(UP, DOWN, NETWORK, LINKID, LANES)
                                      
! --- Check input for times that are not in the current period.
 
    !IF(T(1) .EQ. 0) THEN
    !  IF(TPCOUNT .NE. 1) THEN
    !    WRITE(MSGTEXT, '(A, I4)') 'TIME BEFORE START OF PERIOD ON CARD TYPE', CARDTYPE
    !    CALL SENDTEXTMSG(M_ERROR)
    !    MSGTEXT = LINE
    !    CALL SENDTEXTMSG(M_ERROR)
    !    CALL NEW_ERROR
    !    GOTO 2
    !  ENDIF
    !ENDIF
    !DO I = 2, 8
    !  IF(T(I) .EQ. 0) EXIT
    !  IF(T(I) .LT. ENDOFPERIOD(TPCOUNT-1)) THEN
    !    WRITE(MSGTEXT, '(A, I4)') 'TIME BEFORE START OF PERIOD ON CARD TYPE', CARDTYPE
    !    CALL SENDTEXTMSG(M_ERROR)
    !    MSGTEXT = LINE
    !    CALL SENDTEXTMSG(M_ERROR)
    !    CALL NEW_ERROR
    !    GOTO 2
    !  ENDIF
    !  IF(T(I) .GT. ENDOFPERIOD(TPCOUNT)) THEN
    !    WRITE(MSGTEXT, '(A, I4)') 'TIME BEYOND END OF PERIOD ON CARD TYPE', CARDTYPE
    !    CALL SENDTEXTMSG(M_ERROR)
    !    MSGTEXT = LINE
    !    CALL SENDTEXTMSG(M_ERROR)
    !    CALL NEW_ERROR
    !    GOTO 2
    !  ENDIF
    !ENDDO
 
! --- Check input for vehicle count or vehicles/hour.
 
    IF(CARDTYPE .EQ. 53 .AND. CODE .EQ. 0) THEN
 
! --- Check for a previous entry for this link.
                                                                      
      LASTTIME = 0                                                  
      DO I = 1, NUMBER_OF_ENTRYNODES                                             
        IF(UP .EQ. ENTRYLINK(I)%UP .AND. DOWN .EQ. ENTRYLINK(I)%DOWN) THEN                        
          IF(ENTRYLINK(I)%CARDTYPE .EQ. 53) THEN                    
            IND = ENTRYLINK(I)%INDEX                                
            LASTTIME = ENTRYLINK(I)%TIME(IND)                       
          ENDIF                                                     
          EXIT                                                      
        ENDIF                                                       
      ENDDO                                                         
      IF(T(1) .GT. ENDOFPERIOD(TPCOUNT - 1)) THEN                   
        IF(LASTTIME .EQ. 0) THEN                                    
          V(1) = V(1) * 60 / (T(1) - ENDOFPERIOD(TPCOUNT - 1))      
        ELSEIF(T(1) .GT. LASTTIME) THEN                             
          V(1) = V(1) * 60 / (T(1) - LASTTIME)                      
        ELSE
          WRITE(MSGTEXT, 1001)
          CALL SENDTEXTMSG(M_ERROR)
          MSGTEXT = ' '
          CALL SENDTEXTMSG(M_ERROR)
          CALL NEW_ERROR
          GOTO 2
        ENDIF                                                      
      ELSE
        IF(TPCOUNT .EQ. 1) THEN                                    
          WRITE(MSGTEXT, 1000) UP, DOWN, T(1)                      
        ELSE                                                       
          WRITE(MSGTEXT, 2000) TPCOUNT, UP, DOWN, T(1), ENDOFPERIOD(TPCOUNT - 1)            
        ENDIF                                                      
        CALL SENDTEXTMSG(M_ERROR)
        MSGTEXT = ' '
        CALL SENDTEXTMSG(M_ERROR)
        CALL NEW_ERROR
        GOTO 2
      ENDIF
      NCOUNTS = 1                                                  
      DO I = 2, 8            
        IF(T(I) .EQ. 0) EXIT
        NCOUNTS = NCOUNTS + 1                                      
        SUBTIM = T(I) - T(I-1)
        IF(SUBTIM .GT. 0) THEN
          V(I) = V(I) * 60 / SUBTIM
        ELSEIF(T(I) .NE. ENDOFPERIOD(TPCOUNT)) THEN
          WRITE(MSGTEXT, '(A, I4, A)') 'ERROR IN CARDTYPE', CARDTYPE, ' :INTERVAL = 0 MINUTES'
          CALL SENDTEXTMSG(M_ERROR)
          MSGTEXT = LINE
          CALL SENDTEXTMSG(M_ERROR)
          CALL NEW_ERROR
        ENDIF
        IF(T(I) .EQ. ENDOFPERIOD(TPCOUNT)) T(I) = 0
      ENDDO
      DO I = 8, 2, -1
        T(I) = T(I-1)
      ENDDO
      IF(LASTTIME .EQ. 0) THEN                                      
        T(1) = ENDOFPERIOD(TPCOUNT-1)
      ELSE
        T(1) = LASTTIME
      ENDIF                                                         
      IF(NCOUNTS .LT. 8) V(NCOUNTS+1) = V(NCOUNTS)                  
    ENDIF
 
! --- Check for a previous entry for this link.
 
    DO I = 1, NUMBER_OF_ENTRYNODES
      IF(UP .EQ. ENTRYLINK(I)%UP .AND. DOWN .EQ. ENTRYLINK(I)%DOWN) THEN
        J = 1
 
! --- If the previous entry was from ct50 and still in the same time period,
! --- use data from this card to overwrite the previous data.
 
        IF(CARDTYPE .EQ. 53) THEN
          IF(ENTRYLINK(I)%CARDTYPE .EQ. 50) THEN
            IND = ENTRYLINK(I)%INDEX
            IF(ENTRYLINK(I)%TIME(IND) .EQ. ENDOFPERIOD(TPCOUNT)) ENTRYLINK(I)%INDEX = IND - 2
          ELSEIF(LASTTIME .NE. 0) THEN                             
            ENTRYLINK(I)%INDEX = ENTRYLINK(I)%INDEX - 1            
          ENDIF              
        ENDIF
        ENTRYLINK(I)%CARDTYPE = CARDTYPE
        DO J = 1, 8
          IF(J .GT. 1 .AND. T(J) .EQ. 0) EXIT
          ENTRYLINK(I)%INDEX = ENTRYLINK(I)%INDEX + 1
          ANINDEX = ENTRYLINK(I)%INDEX
          ENTRYLINK(I)%TIME(ANINDEX) = T(J)
          ENTRYLINK(I)%FLOW(ANINDEX) = V(J)
        ENDDO
        IF(CARDTYPE .EQ. 53 .AND. CODE .EQ. 0) THEN
          ENTRYLINK(I)%INTERPOLATE = .FALSE.
        ELSE
          ENTRYLINK(I)%INTERPOLATE = .TRUE.
        ENDIF
        GOTO 1
      ENDIF
    ENDDO
 
! --- Increment the number of entries if this is a new link.
 
    NUMBER_OF_ENTRYNODES = NUMBER_OF_ENTRYNODES + 1
    ENTRYLINK(NUMBER_OF_ENTRYNODES)%CARDTYPE = CARDTYPE
    ENTRYLINK(NUMBER_OF_ENTRYNODES)%UP = UP
    ENTRYLINK(NUMBER_OF_ENTRYNODES)%DOWN = DOWN
    IF(CARDTYPE .EQ. 51) THEN
      ENTRYLINK(NUMBER_OF_ENTRYNODES)%SSNODE = SSNOD
    ELSE
      ENTRYLINK(NUMBER_OF_ENTRYNODES)%SSNODE = 0
    ENDIF
    ENTRYLINK(NUMBER_OF_ENTRYNODES)%LINKID = LINKID
    ENTRYLINK(NUMBER_OF_ENTRYNODES)%LANES = LANES
    ENTRYLINK(NUMBER_OF_ENTRYNODES)%NETWORK = NETWORK
    ENTRYLINK(NUMBER_OF_ENTRYNODES)%INDEX = 0
    DO J = 1, 8
      IF(J .GT. 1 .AND. T(J) .EQ. 0) EXIT
      ENTRYLINK(NUMBER_OF_ENTRYNODES)%INDEX = ENTRYLINK(NUMBER_OF_ENTRYNODES)%INDEX + 1
      ANINDEX = ENTRYLINK(NUMBER_OF_ENTRYNODES)%INDEX
      ENTRYLINK(NUMBER_OF_ENTRYNODES)%TIME(ANINDEX) = T(J)
      ENTRYLINK(NUMBER_OF_ENTRYNODES)%FLOW(ANINDEX) = V(J)
    ENDDO
    IF(CARDTYPE .EQ. 53 .AND. CODE .EQ. 0) THEN                     
      ENTRYLINK(I)%INTERPOLATE = .FALSE.                            
    ELSE                                                            
      ENTRYLINK(I)%INTERPOLATE = .TRUE.                             
    ENDIF                                                           

!      ELSEIF(CARDTYPE .EQ. 74) THEN
!        IF(W26) THEN
!          MSGTEXT = 'RECORD TYPES 26 AND 74 WERE BOTH DETECTED'
!          CALL SENDTEXTMSG(M_ERROR)
!          MSGTEXT = 'RECORD TYPE 74 INPUT WILL BE IGNORED IN THIS CASE'
!          CALL SENDTEXTMSG(M_ERROR)
!        ENDIF

  ELSEIF(CARDTYPE .EQ. 210) THEN
    TPCOUNT = TPCOUNT + 1
    WPRINT = LINE(12:12) .EQ. '0' .OR. LINE(12:12) .EQ. ''
    IF(LINE(4:4) .EQ. '1') GOTO 2

  ENDIF
  GOTO 1
2 CONTINUE
    
!      IF(W23 .AND. WPRINT) CALL PRINT23(*99999)
!      IF(W26 .AND. WPRINT) CALL PRINT26(*99999)
!      IF(W51 .AND. WPRINT) CALL PRINT51(*99999)
!      IF(W53 .AND. WPRINT) CALL PRINT53(*99999)
  CLOSE(65)
  RETURN
10 WRITE(MSGTEXT, '(A)') 'FILE OPEN ERROR : READ_TIME_VARYING_INPUT'
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A)') ETEXT
  CALL SENDTEXTMSG(M_ERROR)
  ERROR_FLAG = 1
  RETURN
20 WRITE(MSGTEXT, '(A)') 'FILE READ ERROR : READ_TIME_VARYING_INPUT'
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A)') ETEXT
  CALL SENDTEXTMSG(M_ERROR)
  ERROR_FLAG = 1
  RETURN
1000 FORMAT('***** FATAL ERROR ***** The time of the first vehicle count&
  & specified on Record Type 53 for Time Period one for link ('&
  &, I4, ',', I4, ') is ', I3, ' minutes, which is not within the &
  &acceptable range. It must be at least one minute after the start of &
  &the time period.') 
1001 FORMAT('***** FATAL ERROR ***** When specifying entry volumes on &
  &Record Type 53 using vehicle counts the first time entry on the second&
  & record for an entry node cannot be the same as the last time on the&
  & first record for the same entry node.')
2000 FORMAT('***** FATAL ERROR ***** The time of the first vehicle count&
  & specified on Record Type 53 for Time Period ', I2, ' for link ('&
  &, I4, ',', I4, ') is ', I3, ' minutes, which is not within the &
  &acceptable range. It must be at least one minute after the start of the&
  & time period. This Time Period starts at ', I3, ' minutes.') 
  END

! ==========================================================================================================
  SUBROUTINE ADJPCT(PCT)
! ----------------------------------------------------------------------
! --- Make sure that percentages add to 100.
! ----------------------------------------------------------------------
  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: PCT(4)
  INTEGER :: TOTAL, IMAX, N
! ==========================================================================================================
  TOTAL = SUM(PCT)
  IF(TOTAL .EQ. 100) RETURN
  IF(TOTAL .NE. 0) THEN
    DO N = 1, 4
      PCT(N) = (200 * PCT(N) + TOTAL) / (2 * TOTAL)
    ENDDO
  ENDIF
  TOTAL = SUM(PCT)
  IF(TOTAL .NE. 100) THEN
    IMAX = MAX(PCT(1), PCT(2), PCT(3), PCT(4))
    DO N = 1, 4
      IF(IMAX .GT. PCT(N)) CYCLE
      PCT(N) = PCT(N) + 100 - TOTAL
      EXIT
    ENDDO
  ENDIF
  RETURN
  END

! ==========================================================================================================
  SUBROUTINE GET_INTERSECTION_PATHS
! ----------------------------------------------------------------------
! --- Get optional inputs to define where turning vehicles enter the
! --- link. This will improve the accuracy of the intersection logic.
! ----------------------------------------------------------------------
  USE STREET_NODES
  USE STREET_LINKS
  USE TIMED_CONTROLLERS
  USE ACTUATED_CONTROLLERS
  IMPLICIT NONE
  INTEGER :: ISIG, IAP, ILENTRY, ILEXIT
! ----------------------------------------------------------------------
  DO ISIG = 1, MAX_NODE_NUMBER
    IF(NFTC(ISIG) .GT. 0) THEN
      DO IAP = 1, FTC_SIGNALS(ISIG)%APPROACHES
        ILENTRY = FTC_SIGNALS(ISIG)%APPROACH(IAP)
        IF(LEFT_LINK(ILENTRY) .NE. 0) THEN
          ILEXIT = LEFT_LINK(ILENTRY)
        ENDIF
      ENDDO
    ELSEIF(NACT(ISIG) .GT. 0) THEN
      DO IAP = 1, AC_SIGNALS(ISIG)%N_DIRECT_APPROACHES
        ILENTRY = AC_SIGNALS(ISIG)%DIRECT_APPROACH(IAP)
        IF(LEFT_LINK(ILENTRY) .NE. 0) THEN
          ILEXIT = LEFT_LINK(ILENTRY)
        ENDIF
      ENDDO
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE GET_EVENT_INPUTS
! ----------------------------------------------------------------------
! --- This routine reads inputs for events from a text file, if it exists.
! ----------------------------------------------------------------------
  USE SIMPARAMS
  USE TEXT
  USE EVENTS
  USE STREET_LINKS
  IMPLICIT NONE
  INCLUDE 'IOFILES.INC'
  CHARACTER*1000 :: EVENT_FILE
  LOGICAL :: DOES_EXIST
  INTEGER :: IBUF(11), IL
! ----------------------------------------------------------------------
  EVENT_FILE = LINFNAME(1:IROOT)//'EVT'
  INQUIRE(FILE = EVENT_FILE, EXIST=DOES_EXIST)
  IF(DOES_EXIST) THEN
    OPEN(1, FILE = EVENT_FILE, ERR=10, IOMSG=ETEXT)
    READ(1, *, ERR=20, IOMSG=ETEXT) IBUF(1), IBUF(2), IBUF(3), IBUF(4), IBUF(7), IBUF(8), IBUF(9), IBUF(10), IBUF(11)
    CALL FIND_STREET_LINK(IBUF(1), IBUF(2), IL)
    IF(IL .EQ. 0) THEN
      CALL NEW_ERROR
      WRITE(MSGTEXT, '(A, 2I5)') 'LONG TERM EVENT SPECIFIED FOR A LINK THAT DOES NOT EXIST ', IBUF(1), IBUF(2)
      CALL SENDTEXTMSG(M_ERROR)
    ELSE
      CALL NEW_EVENT
      EVENT_LINK(NUMBER_OF_EVENTS) = IL
      EVENT_TYPE(NUMBER_OF_EVENTS) = 2
      EVENT_BEGIN_TIME(NUMBER_OF_EVENTS) = IBUF(3)
      EVENT_END_TIME(NUMBER_OF_EVENTS) = IBUF(3) + IBUF(4)
      EVENT_LANE(NUMBER_OF_EVENTS) = IBUF(7)
      EVENT_LOCATION(NUMBER_OF_EVENTS) = IBUF(8)
      EVENT_LENGTH(NUMBER_OF_EVENTS) = IBUF(9)
      EVENT_CODE(NUMBER_OF_EVENTS) = IBUF(10)
      EVENT_SPEED_REDUCTION(NUMBER_OF_EVENTS) = IBUF(11)
      IF(EVENT_BEGIN_TIME(NUMBER_OF_EVENTS) .LT. 0) THEN
        CALL NEW_ERROR
        WRITE(MSGTEXT, '(A, I5)') 'NEGATIVE START TIME FOR LONG TERM EVENT ', NUMBER_OF_EVENTS
        CALL SENDTEXTMSG(M_ERROR)
        WRITE(MSGTEXT, '(A, I4)') '  START TIME = ', EVENT_BEGIN_TIME(NUMBER_OF_EVENTS)
        CALL SENDTEXTMSG(M_ERROR)
      ELSEIF(EVENT_END_TIME(NUMBER_OF_EVENTS) .LE. EVENT_BEGIN_TIME(NUMBER_OF_EVENTS)) THEN
        CALL NEW_ERROR
        WRITE(MSGTEXT, '(A, I5)') 'INVALID DURATION FOR LONG TERM EVENT ', NUMBER_OF_EVENTS
        CALL SENDTEXTMSG(M_ERROR)
        WRITE(MSGTEXT, '(A, I4)') '  DURATION = ', EVENT_END_TIME(NUMBER_OF_EVENTS) - EVENT_BEGIN_TIME(NUMBER_OF_EVENTS)
        CALL SENDTEXTMSG(M_ERROR)
      ENDIF
      IF(EVENT_LANE(NUMBER_OF_EVENTS) .GT. 7) THEN
        CALL NEW_ERROR
        WRITE(MSGTEXT, '(A, I5)') 'INVALID LANE SPECIFIED FOR LONG TERM EVENT ', NUMBER_OF_EVENTS
        CALL SENDTEXTMSG(M_ERROR)
        WRITE(MSGTEXT, '(A, I4)') '  LANE = ', EVENT_LANE(NUMBER_OF_EVENTS)
        CALL SENDTEXTMSG(M_ERROR)
      ENDIF
    ENDIF
  ENDIF
  RETURN
10 WRITE(MSGTEXT, '(A)') 'FILE OPEN ERROR : GET_EVENT_INPUTS'
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A)') ETEXT
  CALL SENDTEXTMSG(M_ERROR)
  ERROR_FLAG = 1
  RETURN
20 WRITE(MSGTEXT, '(A)') 'FILE READ ERROR : GET_EVENT_INPUTS'
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A)') ETEXT
  CALL SENDTEXTMSG(M_ERROR)
  ERROR_FLAG = 1
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE PROCESS_ACTUATED_CONTROL
  USE ACTUATED_CONTROLLERS
  USE TEXT
  IMPLICIT NONE
  REAL :: SPLITS(8)
  INTEGER :: IACT, PHASE  
! ----------------------------------------------------------------------
  DO IACT = 1, NUMBER_OF_ACS
    AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(1, 1) = AC_SIGNALS(IACT)%PHASE_SEQUENCE(1)
    AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(1, 2) = AC_SIGNALS(IACT)%PHASE_SEQUENCE(2)
    AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(1, 3) = AC_SIGNALS(IACT)%PHASE_SEQUENCE(3)
    AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(1, 4) = AC_SIGNALS(IACT)%PHASE_SEQUENCE(4)
    AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(2, 1) = AC_SIGNALS(IACT)%PHASE_SEQUENCE(5)
    AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(2, 2) = AC_SIGNALS(IACT)%PHASE_SEQUENCE(6)
    AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(2, 3) = AC_SIGNALS(IACT)%PHASE_SEQUENCE(7)
    AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(2, 4) = AC_SIGNALS(IACT)%PHASE_SEQUENCE(8)
      
    !!!Convert splits from force-off times 
    IF(AC_SIGNALS(IACT)%CYCLE_LENGTH .NE. 0.) THEN
      CALL CALCSPLITS(IACT, AC_SIGNALS(IACT)%CYCLE_LENGTH, AC_SIGNALS(IACT)%FORCE_OFF_TIME, AC_SIGNALS(IACT)%PHASE_SEQUENCE, SPLITS)                         
      AC_SIGNALS(IACT)%PHASE_SPLITS = SPLITS
    ENDIF
      
    !If the user did not specify max green and the signal is coordinated determine max green from the splits
    DO PHASE = 1, 8
      IF(AC_SIGNALS(IACT)%GUI_MAX_GREEN_TIMES(PHASE) .EQ. 0.) THEN
        IF(AC_SIGNALS(IACT)%PHASE_SPLITS(PHASE) .GT. 0.) THEN
          AC_SIGNALS(IACT)%GUI_MAX_GREEN_TIMES(PHASE) = &
            AC_SIGNALS(IACT)%PHASE_SPLITS(PHASE) - AC_SIGNALS(IACT)%GUI_YC(PHASE) - AC_SIGNALS(IACT)%GUI_RC(PHASE)
        ELSEIF(AC_SIGNALS(IACT)%GUI_MAX_GREEN_TIMES(PHASE) .LT. AC_SIGNALS(IACT)%GUI_MIN_GREEN_TIMES(PHASE)) THEN
          IF(AC_SIGNALS(IACT)%CYCLE_LENGTH .EQ. 0 .OR. (AC_SIGNALS(IACT)%CYCLE_LENGTH .NE. 0 .AND. PHASE .NE. 2 .AND. PHASE .NE. 6)) THEN
            WRITE(MSGTEXT, '(A,I1,A,I4)') 'RT 47: MAX GREEN IS LESS THAN MIN GREEN FOR PHASE ', PHASE, ' AT NODE ', AC_SIGNALS(IACT)%NODE(1)
            CALL SENDTEXTMSG(M_ERROR)
            WRITE(MSGTEXT, '(A)') ' MAX GREEN WILL BE SET EQUAL TO MIN GREEN'
            CALL SENDTEXTMSG(M_ERROR)
            AC_SIGNALS(IACT)%GUI_MAX_GREEN_TIMES(PHASE) = AC_SIGNALS(IACT)%GUI_MIN_GREEN_TIMES(PHASE)
          ENDIF
        ENDIF
      ENDIF
    ENDDO
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE SHIFT_LANES_RTW(IL)
  USE STREET_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IL
  INTEGER :: ILN, KLN, ILUP
! ----------------------------------------------------------------------
  FIRST_FULL_LANE(IL) = FIRST_FULL_LANE(IL) + 1
  LAST_FULL_LANE(IL) = LAST_FULL_LANE(IL) + 1
  TOTAL_LANES(IL) = TOTAL_LANES(IL) + 1
  DO ILN = TOTAL_LANES(IL), 2, -1
    CHANNELIZATION(IL, ILN) = CHANNELIZATION(IL, ILN - 1)
    LANE_LENGTH(IL, ILN) = LANE_LENGTH(IL, ILN - 1)
    LANE_NUMBERS(IL, ILN) = LANE_NUMBERS(IL, ILN - 1)
    DO KLN = 1, N_STREET_LANES
      LT_ARC_LENGTH(IL, ILN, KLN) = LT_ARC_LENGTH(IL, ILN - 1, KLN)
      THRU_ARC_LENGTH(IL, ILN, KLN) = THRU_ARC_LENGTH(IL, ILN - 1, KLN)
      RT_ARC_LENGTH(IL, ILN, KLN) = RT_ARC_LENGTH(IL, ILN - 1, KLN)
      LD_ARC_LENGTH(IL, ILN, KLN) = LD_ARC_LENGTH(IL, ILN - 1, KLN)
      RD_ARC_LENGTH(IL, ILN, KLN) = RD_ARC_LENGTH(IL, ILN - 1, KLN)
    ENDDO
  ENDDO
  CHANNELIZATION(IL, 1) = 4
  LANE_LENGTH(IL, 1) = SLENGTH(IL) - RTW_EXIT_POINT(IL)
  LANE_NUMBERS(IL, 1) = 7 - NUMBER_LEFTPOCKETS(IL)
  DO KLN = 1, N_STREET_LANES
    LT_ARC_LENGTH(IL, 1, KLN) = 0.
    THRU_ARC_LENGTH(IL, 1, KLN) = 0.
    RT_ARC_LENGTH(IL, 1, KLN) = 0.
    LD_ARC_LENGTH(IL, 1, KLN) = 0.
    RD_ARC_LENGTH(IL, 1, KLN) = 0.
  ENDDO
  SALIGNMENT_LANE(IL) = SALIGNMENT_LANE(IL) + 1
  DO ILUP = 1, N_STREET_LINKS
    IF(STHRU_LINK(ILUP) .EQ. IL) THEN
      STHRU_ALIGNMENT_LANE(ILUP) = STHRU_ALIGNMENT_LANE(ILUP) + 1
      EXIT
    ENDIF
  ENDDO
  RETURN
  END
  
 ! ==================================================================================================
  SUBROUTINE DELETE_SUPER_CONTROLLER(NSC)
  USE ACTUATED_CONTROLLERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NSC
  INTEGER :: IACT, PHASE
! ----------------------------------------------------------------------
  IACT = NSC + NUMBER_OF_ACS
  AC_SIGNALS(IACT)%N_DIRECT_APPROACHES = 0        
  DO PHASE = 1, 8
    AC_SIGNALS(IACT)%PHASE(PHASE)%IN_USE = .FALSE. 
    AC_SIGNALS(IACT)%PHASE(PHASE)%LEFTARROW = .FALSE.
    AC_SIGNALS(IACT)%PHASE(PHASE)%THRUARROW = .FALSE.
    AC_SIGNALS(IACT)%PHASE(PHASE)%RIGHTARROW = .FALSE.
    AC_SIGNALS(IACT)%PHASE(PHASE)%LDIAGARROW = .FALSE.
    AC_SIGNALS(IACT)%PHASE(PHASE)%RDIAGARROW = .FALSE.
    AC_SIGNALS(IACT)%PHASE_SEQUENCE(PHASE) = 0
    AC_SIGNALS(IACT)%GUI_MIN_GREEN_TIMES(PHASE) = 0.
    AC_SIGNALS(IACT)%GUI_MAX_GREEN_TIMES(PHASE) = 0.
    AC_SIGNALS(IACT)%GUI_YC(PHASE) = 0.
    AC_SIGNALS(IACT)%GUI_RC(PHASE) = 0.
    AC_SIGNALS(IACT)%GUI_ACTUATED_MODE(PHASE) = 0.
    AC_SIGNALS(IACT)%GUI_DEFAULT_EXTENSION_TIMES(PHASE) = 0.
    AC_SIGNALS(IACT)%GUI_TIME_TO_REDUCE(PHASE) = 0.
    AC_SIGNALS(IACT)%GUI_TIME_BEFORE_REDUCTION(PHASE) = 0.
    AC_SIGNALS(IACT)%GUI_MIN_GAP_TIMES(PHASE) = 0.
    AC_SIGNALS(IACT)%GUI_GAP_TIMES(PHASE) = 0.
    AC_SIGNALS(IACT)%SDP%DETECTOR_COUNT = 0
    AC_SIGNALS(IACT)%SDP%DETECTOR_LIST = 0
  ENDDO
  RETURN
  END
