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
! =================================================================================================
  MODULE ZTABLE

! --- Normal distribution values.
  REAL :: Z(0:299) = (/&
              0.50000, &
              0.50399, &
              0.50798, &
              0.51197, &
              0.51595, &
              0.51994, &
              0.52392, &
              0.52790, &
              0.53188, &
              0.53586, &
              0.53983, &
              0.54380, &
              0.54776, &
              0.55172, &
              0.55567, &
              0.55962, &
              0.56356, &
              0.56749, &
              0.57142, &
              0.57535, &
              0.57926, &
              0.58317, &
              0.58706, &
              0.59095, &
              0.59483, &
              0.59871, &
              0.60257, &
              0.60642, &
              0.61026, &
              0.61409, &
              0.61791, &
              0.62172, &
              0.62552, &
              0.62930, &
              0.63307, &
              0.63683, &
              0.64058, &
              0.64431, &
              0.64803, &
              0.65173, &
              0.65542, &
              0.65910, &
              0.66276, &
              0.66640, &
              0.67003, &
              0.67364, &
              0.67724, &
              0.68082, &
              0.68439, &
              0.68793, &
              0.69146, &
              0.69497, &
              0.69847, &
              0.70194, &
              0.70540, &
              0.70884, &
              0.71226, &
              0.71566, &
              0.71904, &
              0.72240, &
              0.72575, &
              0.72907, &
              0.73237, &
              0.73565, &
              0.73891, &
              0.74215, &
              0.74537, &
              0.74857, &
              0.75175, &
              0.75490, &
              0.75804, &
              0.76115, &
              0.76424, &
              0.76730, &
              0.77035, &
              0.77337, &
              0.77637, &
              0.77935, &
              0.78230, &
              0.78524, &
              0.78814, &
              0.79103, &
              0.79389, &
              0.79673, &
              0.79955, &
              0.80234, &
              0.80511, &
              0.80785, &
              0.81057, &
              0.81327, &
              0.81594, &
              0.81859, &
              0.82121, &
              0.82381, &
              0.82639, &
              0.82894, &
              0.83147, &
              0.83398, &
              0.83646, &
              0.83891, &
              0.84134, &
              0.84375, &
              0.84614, &
              0.84849, &
              0.85083, &
              0.85314, &
              0.85543, &
              0.85769, &
              0.85993, &
              0.86214, &
              0.86433, &
              0.86650, &
              0.86864, &
              0.87076, &
              0.87286, &
              0.87493, &
              0.87698, &
              0.87900, &
              0.88100, &
              0.88298, &
              0.88493, &
              0.88686, &
              0.88877, &
              0.89065, &
              0.89251, &
              0.89435, &
              0.89617, &
              0.89796, &
              0.89973, &
              0.90147, &
              0.90320, &
              0.90490, &
              0.90658, &
              0.90824, &
              0.90988, &
              0.91149, &
              0.91309, &
              0.91466, &
              0.91621, &
              0.91774, &
              0.91924, &
              0.92073, &
              0.92220, &
              0.92364, &
              0.92507, &
              0.92647, &
              0.92785, &
              0.92922, &
              0.93056, &
              0.93189, &
              0.93319, &
              0.93448, &
              0.93574, &
              0.93699, &
              0.93822, &
              0.93943, &
              0.94062, &
              0.94179, &
              0.94295, &
              0.94408, &
              0.94520, &
              0.94630, &
              0.94738, &
              0.94845, &
              0.94950, &
              0.95053, &
              0.95154, &
              0.95254, &
              0.95352, &
              0.95449, &
              0.95543, &
              0.95637, &
              0.95728, &
              0.95818, &
              0.95907, &
              0.95994, &
              0.96080, &
              0.96164, &
              0.96246, &
              0.96327, &
              0.96407, &
              0.96485, &
              0.96562, &
              0.96638, &
              0.96712, &
              0.96784, &
              0.96856, &
              0.96926, &
              0.96995, &
              0.97062, &
              0.97128, &
              0.97193, &
              0.97257, &
              0.97320, &
              0.97381, &
              0.97441, &
              0.97500, &
              0.97558, &
              0.97615, &
              0.97670, &
              0.97725, &
              0.97778, &
              0.97831, &
              0.97882, &
              0.97932, &
              0.97982, &
              0.98030, &
              0.98077, &
              0.98124, &
              0.98169, &
              0.98214, &
              0.98257, &
              0.98300, &
              0.98341, &
              0.98382, &
              0.98422, &
              0.98461, &
              0.98500, &
              0.98537, &
              0.98574, &
              0.98610, &
              0.98645, &
              0.98679, &
              0.98713, &
              0.98745, &
              0.98778, &
              0.98809, &
              0.98840, &
              0.98870, &
              0.98899, &
              0.98928, &
              0.98956, &
              0.98983, &
              0.99010, &
              0.99036, &
              0.99061, &
              0.99086, &
              0.99111, &
              0.99134, &
              0.99158, &
              0.99180, &
              0.99202, &
              0.99224, &
              0.99245, &
              0.99266, &
              0.99286, &
              0.99305, &
              0.99324, &
              0.99343, &
              0.99361, &
              0.99379, &
              0.99396, &
              0.99413, &
              0.99430, &
              0.99446, &
              0.99461, &
              0.99477, &
              0.99492, &
              0.99506, &
              0.99520, &
              0.99534, &
              0.99547, &
              0.99560, &
              0.99573, &
              0.99585, &
              0.99598, &
              0.99609, &
              0.99621, &
              0.99632, &
              0.99643, &
              0.99653, &
              0.99664, &
              0.99674, &
              0.99683, &
              0.99693, &
              0.99702, &
              0.99711, &
              0.99720, &
              0.99728, &
              0.99736, &
              0.99744, &
              0.99752, &
              0.99760, &
              0.99767, &
              0.99774, &
              0.99781, &
              0.99788, &
              0.99795, &
              0.99801, &
              0.99807, &
              0.99813, &
              0.99819, &
              0.99825, &
              0.99831, &
              0.99836, &
              0.99841, &
              0.99846, &
              0.99851, &
              0.99856, &
              0.99861 /)
  END MODULE
      
! =================================================================================================
  SUBROUTINE TRIPGENERATOR
! ----------------------------------------------------------------------
!     This routine determines how many vehicles to emit during the next
!     minute of simulation, calls a subroutine to distribute the headways,
!     and then calls a subroutine that inserts the vehicles into entry queues.
! ----------------------------------------------------------------------
  USE FLOWDATA_MOD
  USE VEHICLE_MOD
  USE VEHICLE_PARAMETERS
  USE TEXT
  USE QUEUE_MOD
  USE SIMPARAMS
  USE GLOBAL_DATA
  IMPLICIT NONE
  REAL RHEAD(1500), RAVGVL, RVLBEG, RVLEND
  INTEGER :: IN, IL, INDEX, NEMIT, IND, IUP
  INTEGER :: T1, T2, F1, F2
  REAL :: TIME
  TYPE(VEHICLE) :: THIS_VEHICLE
! ----------------------------------------------------------------------
  TIME = SIMTIME /  60.
  DO IN = 1, NUMBER_OF_ENTRYNODES
    IL = ENTRYLINK(IN)%LINKID
    IF(INITMODE) THEN                                               
      RAVGVL = ENTRYLINK(IN)%FLOW(1) / 60.                          
    ELSE                                                            
      INDEX = ENTRYLINK(IN)%INDEX
      IF(TIME .GE. ENTRYLINK(IN)%TIME(INDEX)) THEN
        RVLBEG = ENTRYLINK(IN)%FLOW(INDEX)
        RVLEND = RVLBEG
      ELSE 
        DO IND = 1, INDEX - 1
          IF(TIME .GE. ENTRYLINK(IN)%TIME(IND) .AND. TIME .LT. ENTRYLINK(IN)%TIME(IND+1)) THEN
            IF(ENTRYLINK(IN)%INTERPOLATE) THEN                      
              T1 = ENTRYLINK(IN)%TIME(IND)
              T2 = ENTRYLINK(IN)%TIME(IND+1)
              F1 = ENTRYLINK(IN)%FLOW(IND)
              F2 = ENTRYLINK(IN)%FLOW(IND+1)
              RVLBEG = F1 + (F2-F1)*(TIME-T1)/(T2-T1)
              RVLEND = F1 + (F2-F1)*(TIME+1-T1)/(T2-T1)
            ELSE                                                    
              RVLBEG = ENTRYLINK(IN)%FLOW(IND)                      
              RVLEND = RVLBEG                                       
            ENDIF                                                   
            EXIT
          ENDIF
        ENDDO
      ENDIF
      RVLBEG = RVLBEG / 60.
      RVLEND = RVLEND / 60.
      RAVGVL = (RVLBEG + RVLEND) / 2.
    ENDIF                                                      
    RSPEC(IN) = RSPEC(IN) + ABS(RAVGVL)
    NEMIT = RSPEC(IN) - NEMITD(IN)
    IUP = ENTRYLINK(IN)%UP
 
! --- GET_HEADWAYS is limited to 1500 headways
 
    IF(NEMIT .GT. 1500 .OR. NEMIT .LT. -1500) THEN
      IF(NEMIT .GT. 1500) NEMIT = 1500
      IF(NEMIT .LT. -1500) NEMIT = -1500
      WRITE(MSGTEXT, 1000) IUP
      CALL SENDTEXTMSG(M_ERROR)
      MSGTEXT = ' '
      CALL SENDTEXTMSG(M_ERROR)
    ENDIF
 
    IF(NEMIT .NE. 0) THEN
 
      THIS_VEHICLE%ENTRYLINKID = ENTRYLINK(IN)%LINKID
      THIS_VEHICLE%NETWORK = ENTRYLINK(IN)%NETWORK
      THIS_VEHICLE%VTYPE = 0
      THIS_VEHICLE%PATHID = 0
      THIS_VEHICLE%ROUTEID = 0
      THIS_VEHICLE%DRIVER = 0
      THIS_VEHICLE%FLEET = 0
      THIS_VEHICLE%GLOBALID = 0
      THIS_VEHICLE%NETFLOW = 0
      THIS_VEHICLE%TURNCODE = TC_THRU
      IF(NEMIT .GT. 0) THEN
        CALL GET_HEADWAYS(NEMIT, RHEAD)
        NEMITD(IN) = NEMITD(IN) + NEMIT
        IF(RAVGVL .LT. 0) THIS_VEHICLE%NETFLOW = -1
        CALL QUEUE_VEHICLES(SIMTIME, RHEAD, NEMIT, IN, THIS_VEHICLE)
        IF(ERROR_COUNT .NE. 0) RETURN
      ENDIF
    ENDIF
  ENDDO
  RETURN
1000 FORMAT('The number of vehicles requested for entry node ', I4,&
  &' will exceed a program limitation of 1500 vehicles per minute. ',&
  &'The number of vehicles entering the network at that node will be& 
  &less than the number requested.')
  END
 
! ==================================================================================================
  SUBROUTINE QUEUE_VEHICLES(SECONDS, RHEAD, INUM, IN, THIS_VEHICLE)
! ----------------------------------------------------------------------
!     this routine determines the entry time for each vehicle and 
!     inserts the vehicles into entry queues.
! ----------------------------------------------------------------------
  USE QUEUE_MOD
  USE TEXT
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: INUM, IN
  REAL, INTENT(IN) :: SECONDS, RHEAD(1500)
  TYPE(VEHICLE), INTENT(INOUT) :: THIS_VEHICLE
  REAL :: RDEPTIME
  INTEGER :: IV, STATUS
! ----------------------------------------------------------------------
  RDEPTIME = SECONDS
  DO IV = 1, INUM
    RDEPTIME = RDEPTIME + RHEAD(IV)
    THIS_VEHICLE%DEPARTURE = RDEPTIME
    IF(.NOT. ASSOCIATED(FRONT(IN)%PTR)) THEN
      ALLOCATE(FRONT(IN)%PTR, STAT=STATUS)
      IF(STATUS .NE. 0) THEN
        MSGTEXT = 'INSUFFICIENT MEMORY FOR VEHICLE GENERATION'
        CALL SENDTEXTMSG(M_ERROR)
        CALL NEW_ERROR
        RETURN
      ENDIF
      REAR(IN)%PTR => FRONT(IN)%PTR
    ELSE
      ALLOCATE(REAR(IN)%PTR%NEXT, STAT=STATUS)
      IF(STATUS .NE. 0) THEN
        MSGTEXT = 'INSUFFICIENT MEMORY FOR VEHICLE GENERATION'
        CALL SENDTEXTMSG(M_ERROR)
        CALL NEW_ERROR
        RETURN
      ENDIF
      REAR(IN)%PTR => REAR(IN)%PTR%NEXT
    ENDIF
    REAR(IN)%PTR%VEHICLE = THIS_VEHICLE
  ENDDO
  RETURN
  END
 
! ==================================================================================================
  SUBROUTINE QUEUE_BUS(THIS_VEHICLE)
! ----------------------------------------------------------------------
!     this routine inserts a bus into an entry queue.
! ----------------------------------------------------------------------
  USE QUEUE_MOD
  USE TEXT
  USE SIMPARAMS
  IMPLICIT NONE
  TYPE(VEHICLE), INTENT(INOUT) ::THIS_VEHICLE
  INTEGER :: STATUS
! ----------------------------------------------------------------------
  IF(.NOT. ASSOCIATED(BUS_FRONT%PTR)) THEN
    ALLOCATE(BUS_FRONT%PTR, STAT=STATUS)
    IF(STATUS .NE. 0) THEN
      MSGTEXT = 'INSUFFICIENT MEMORY FOR VEHICLE GENERATION'
      CALL SENDTEXTMSG(M_ERROR)
      CALL NEW_ERROR
      RETURN
    ENDIF
    BUS_REAR%PTR => BUS_FRONT%PTR
  ELSE
    ALLOCATE(BUS_REAR%PTR%NEXT, STAT=STATUS)
    IF(STATUS .NE. 0) THEN
      MSGTEXT = 'INSUFFICIENT MEMORY FOR VEHICLE GENERATION'
      CALL SENDTEXTMSG(M_ERROR)
      CALL NEW_ERROR
      RETURN
    ENDIF
    BUS_REAR%PTR => BUS_REAR%PTR%NEXT
  ENDIF
  BUS_REAR%PTR%VEHICLE = THIS_VEHICLE
  RETURN
  END

! ==================================================================================================
  SUBROUTINE CHECK_ENTRY_QUEUES
! ----------------------------------------------------------------------
!     This routine determines if it is time to emit the vehicle at the front
!     of each entry queue.
! ----------------------------------------------------------------------
  USE FLOWDATA_MOD
  USE QUEUE_MOD
  USE STREET_LINKS
  USE SIMPARAMS
  USE GLOBAL_DATA
  USE ENTRYNODE_DATA
  USE NODE_TABLE
  IMPLICIT NONE
  TYPE(VEHICLE) :: FRONT_VEHICLE
  TYPE(QNODE), POINTER :: TEMP
#ifdef TSIS_COMPATIBLE
  INCLUDE 'CORWIN.FI'
#endif    
  INTEGER :: IN, IV, IL, INODE
  LOGICAL :: WEMIT
! ----------------------------------------------------------------------
 
! --- loop over all entry links and movements
! ---  if queue exists for this movement
! ---    get the first vehicle in the queue
! ---      compare current time to vehicle's departure time
! ---      if time to emit, try to emit into correct network
! ---      if successfully emitted
! ---        remove vehicle from queue and repeat for next vehicle in queue
 
  DO IN = 1, NUMBER_OF_ENTRYNODES
    DO
      IF(ASSOCIATED(FRONT(IN)%PTR)) THEN
        FRONT_VEHICLE = FRONT(IN)%PTR%VEHICLE
        IF(SIMTIME .GE. FRONT_VEHICLE%DEPARTURE) THEN
          IF(FRONT_VEHICLE%GLOBALID .EQ. 0) THEN
            CALL GET_NEXT_ID(IV)
            FRONT_VEHICLE%GLOBALID = IV
          ENDIF
          IL = FRONT_VEHICLE%ENTRYLINKID
          IF(FRONT_VEHICLE%NETWORK .EQ. I_FREEWAY) THEN
            CALL EMIT_FREEWAY_VEHICLE(FRONT_VEHICLE, WEMIT)
          ELSE
            IF(NODE_TYPE(SUSN(IL)) .EQ. NT_EXTERN) THEN
              CALL EMIT_STREET_VEHICLE(FRONT_VEHICLE, WEMIT)
            ELSE
              CALL SOURCE_VEHICLE(FRONT_VEHICLE, WEMIT)
            ENDIF
          ENDIF
          IF(WEMIT) THEN
            TEMP => FRONT(IN)%PTR%NEXT
            DEALLOCATE(FRONT(IN)%PTR)
            FRONT(IN)%PTR => TEMP
          ELSE
            EXIT
          ENDIF
        ELSE
          EXIT
        ENDIF
      ELSE
        EXIT
      ENDIF
    ENDDO
  ENDDO
 
! --- Determine the total delay for vehicles that were not able to
! --- enter the network when they were scheduled.
 
  IF(.NOT. INITMODE) THEN
    DO IN = 1, NUMBER_OF_ENTRYNODES
      IF(NODE_TYPE(ENTRYLINK(IN)%UP) .EQ. NT_EXTERN) THEN
        IF(ASSOCIATED(FRONT(IN)%PTR)) THEN
          TEMP => FRONT(IN)%PTR%NEXT
          DO WHILE(ASSOCIATED(TEMP))
            FRONT_VEHICLE = TEMP%VEHICLE
            IF(SIMTIME .GT. FRONT_VEHICLE%DEPARTURE) THEN
              INODE = ENTRYLINK(IN)%UP
              IF(SIMTIME - FRONT_VEHICLE%DEPARTURE .LE. 1) THEN
                DELAYED_COUNT(INODE) = DELAYED_COUNT(INODE) + 1
                DELAYED_TIME(INODE) = DELAYED_TIME(INODE) + SIMTIME - FRONT_VEHICLE%DEPARTURE
              ELSE
                DELAYED_TIME(INODE) = DELAYED_TIME(INODE) + 1
              ENDIF
            ELSE
              EXIT
            ENDIF
            TEMP => TEMP%NEXT
          ENDDO
        ENDIF
      ENDIF
    ENDDO
  ENDIF
#ifdef TSIS_COMPATIBLE
    CALL SIMCALLRTE(RT_POST_VEHICLE_EMIT)
#endif
  RETURN
  END

! ==================================================================================================
  SUBROUTINE CHECK_PBV_QUEUE
! ----------------------------------------------------------------------
!     this routine determines if it is time to emit the vehicle at the front
!     of each entry queue of path-based vehicles.
! ----------------------------------------------------------------------
  USE QUEUE_MOD
  USE SIMPARAMS
  USE GLOBAL_DATA
  IMPLICIT NONE
  TYPE(VEHICLE) :: FRONT_VEHICLE
  TYPE(QNODE), POINTER :: FRONT_PTR, TAIL_PTR, TEMP
  LOGICAL :: WEMIT
  INTEGER :: IV
! ----------------------------------------------------------------------
  DO
    IF(.NOT. ASSOCIATED(PBV_FRONT%PTR)) RETURN
    FRONT_VEHICLE = PBV_FRONT%PTR%VEHICLE
    IF(SIMTIME .GE. FRONT_VEHICLE%DEPARTURE) THEN
      IF(FRONT_VEHICLE%GLOBALID .EQ. 0) THEN
        CALL GET_NEXT_ID(IV)
        FRONT_VEHICLE%GLOBALID = IV
      ENDIF
      IF(FRONT_VEHICLE%NETWORK .EQ. I_FREEWAY) THEN
        CALL EMIT_FREEWAY_VEHICLE(FRONT_VEHICLE, WEMIT)
      ELSE
        CALL EMIT_STREET_VEHICLE(FRONT_VEHICLE, WEMIT)
      ENDIF
      IF(WEMIT) THEN
        TEMP => PBV_FRONT%PTR%NEXT
        DEALLOCATE(PBV_FRONT%PTR)
        PBV_FRONT%PTR => TEMP
      ELSE
        EXIT
      ENDIF
    ELSE
      EXIT
    ENDIF
  ENDDO     
  TAIL_PTR => PBV_FRONT%PTR    !where we've been
  FRONT_PTR => TAIL_PTR%NEXT   !next to try
  DO
    IF(.NOT. ASSOCIATED(FRONT_PTR)) RETURN
    FRONT_VEHICLE = FRONT_PTR%VEHICLE
    IF(SIMTIME .GE. FRONT_VEHICLE%DEPARTURE) THEN
      IF(FRONT_VEHICLE%GLOBALID .EQ. 0) THEN
        CALL GET_NEXT_ID(IV)
        FRONT_VEHICLE%GLOBALID = IV
      ENDIF
      IF(FRONT_VEHICLE%NETWORK .EQ. I_FREEWAY) THEN
        CALL EMIT_FREEWAY_VEHICLE(FRONT_VEHICLE, WEMIT)
      ELSE
        CALL EMIT_STREET_VEHICLE(FRONT_VEHICLE, WEMIT)
      ENDIF
      IF(WEMIT) THEN  !Remove vehicle from queue
        TEMP => FRONT_PTR%NEXT
        DEALLOCATE(FRONT_PTR)
        FRONT_PTR => TEMP
        TAIL_PTR%NEXT => FRONT_PTR
      ELSE            !Try next vehicle in queue
        TAIL_PTR => FRONT_PTR
        FRONT_PTR => FRONT_PTR%NEXT
      ENDIF
    ELSE          !Try next vehicle in queue
      TAIL_PTR => FRONT_PTR
      FRONT_PTR => FRONT_PTR%NEXT
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE CHECK_BUS_ROUTES
! ----------------------------------------------------------------------
!     This routine determines if it is time to place a bus at the front
!     of the bus queue.
! ----------------------------------------------------------------------
  USE BUS_ROUTE_DATA
  USE SIMPARAMS
  USE VEHICLE_MOD
  USE VEHICLE_PARAMETERS
  USE GLOBAL_DATA
  USE FREEWAY_LINKS
  USE STREET_LINKS
  USE TEXT
  IMPLICIT NONE
  INTEGER :: NBR, ITIME
  TYPE(VEHICLE) :: THIS_VEHICLE
  INTEGER :: IL
! ----------------------------------------------------------------------
  DO NBR = 1, NUMBER_OF_ROUTES
    IF(BUSR_HDWY(NBR) .EQ. 0) CYCLE
    ITIME = INT(SIMTIME)
    IF(INITMODE .AND. ITIME .EQ. 0) THEN
      BUSR_TIMER(NBR) = BUSR_TIMER(NBR) + BUSR_OFFSET(NBR)
    ENDIF
    IF(BUSR_TIMER(NBR) .LE. 0) THEN
      CALL FIND_FREEWAY_LINK(BUSR_ROUTE_NODES(NBR, 1), BUSR_ROUTE_NODES(NBR, 2), IL)
      IF(IL .NE. 0) THEN
        THIS_VEHICLE%NETWORK = 1
      ELSE
        CALL FIND_STREET_LINK(BUSR_ROUTE_NODES(NBR, 1), BUSR_ROUTE_NODES(NBR, 2), IL)
        THIS_VEHICLE%NETWORK = 2
      ENDIF
      IF(IL .EQ. 0) THEN
        WRITE(MSGTEXT, '(A, I4)') 'ENTRY LINK NOT FOUND FOR BUS ROUTE ', NBR
        CALL SENDTEXTMSG(M_ERROR)
        ERROR_FLAG = 1
        RETURN
      ELSE
        THIS_VEHICLE%ENTRYLINKID = IL
        THIS_VEHICLE%DEPARTURE = SIMTIME
        THIS_VEHICLE%PATHID = 0
        THIS_VEHICLE%ROUTEID = NBR
        THIS_VEHICLE%DRIVER = 10
        THIS_VEHICLE%FLEET = FLEET_BUS
        THIS_VEHICLE%VTYPE = 7
        THIS_VEHICLE%TURNCODE = TC_NULL
        CALL QUEUE_BUS(THIS_VEHICLE)
        BUSR_TIMER(NBR) = BUSR_HDWY(NBR) - TIMESTEP
      ENDIF
    ELSE
      BUSR_TIMER(NBR) = BUSR_TIMER(NBR) - TIMESTEP
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE CHECK_BUS_QUEUES
! ----------------------------------------------------------------------
!     this routine examines the bus queue and determines if it is time for
!     a bus to enter the network.
! ----------------------------------------------------------------------
  USE FLOWDATA_MOD
  USE QUEUE_MOD
  USE SIMPARAMS
  USE GLOBAL_DATA
  IMPLICIT NONE
  TYPE(VEHICLE) :: FRONT_VEHICLE
  TYPE(QNODE), POINTER :: TEMP
  INTEGER :: VID
  LOGICAL :: WEMIT
! ----------------------------------------------------------------------
1 IF(ASSOCIATED(BUS_FRONT%PTR)) THEN
    FRONT_VEHICLE = BUS_FRONT%PTR%VEHICLE
    IF(SIMTIME .GE. FRONT_VEHICLE%DEPARTURE) THEN
      CALL GET_NEXT_ID(VID)
      FRONT_VEHICLE%GLOBALID = VID
      IF(FRONT_VEHICLE%NETWORK .EQ. I_FREEWAY) THEN
        CALL EMIT_FREEWAY_VEHICLE(FRONT_VEHICLE, WEMIT)
      ELSE
        CALL EMIT_STREET_VEHICLE(FRONT_VEHICLE, WEMIT)
      ENDIF
      IF(WEMIT) THEN
        TEMP => BUS_FRONT%PTR%NEXT
        DEALLOCATE(BUS_FRONT%PTR)
        BUS_FRONT%PTR => TEMP
        GOTO 1
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE GET_HEADWAYS(NUMVEHS, RHEAD)
! ----------------------------------------------------------------------
!     Assign entry headways to the vehicles being generated for the next
!     60 seconds.
! ----------------------------------------------------------------------
  USE ENTRYNODE_DATA
  USE SEEDS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NUMVEHS
  REAL, INTENT(INOUT) :: RHEAD(1500)
  INTEGER :: I
  REAL :: MU
! ----------------------------------------------------------------------
  IF(TYPEDIST .EQ. 0) THEN        !UNIFORM DISTRIBUTION
    DO I = 1, NUMVEHS
      RHEAD(I) = 60. / NUMVEHS
    ENDDO
    
  ELSEIF(TYPEDIST .EQ. 1) THEN    !NORMAL DISTRIBUTION
    MU = 60. / NUMVEHS
    DO I = 1, NUMVEHS
      CALL NORM_DISTR(MU, RHEAD(I))
    ENDDO

  ELSEIF(TYPEDIST .EQ. 2) THEN    !ERLANG DISTRIBUTION
    MU = 60. / NUMVEHS
    DO I = 1, NUMVEHS
      CALL ERL_DISTR(MU, ERLANGA, RHEAD(I))
    ENDDO
 
  ENDIF      
  RETURN
  END      

! ==================================================================================================
  SUBROUTINE NORM_DISTR(MU, X)
! ----------------------------------------------------------------------
!     Derive headways from the normal distribution.
! ----------------------------------------------------------------------
  USE ENTRYNODE_DATA
  USE SIMPARAMS
  USE ZTABLE
  IMPLICIT NONE
  REAL, INTENT(IN) :: MU
  REAL, INTENT(OUT) :: X
  INTEGER :: I
  REAL :: Y, SIGMA
! ----------------------------------------------------------------------
  Y = 0
  DO WHILE(Y .LT. 0.005)
    CALL RANDOM_NUMBER(Y)
  ENDDO
  SIGMA = (MU - MINSEP) / 2.575
  I = 299
  IF(Y .GE. 0.5) THEN
    DO WHILE(Y .LT. Z(I))
      I = I - 1
      X = ((MU + SIGMA * I / 100) / TIMESTEP) * TIMESTEP
    ENDDO
  ELSE
    DO WHILE(1 - Y .LT. Z(I))
      I = I - 1
      X = ((MU - SIGMA * I / 100) / TIMESTEP) * TIMESTEP
    ENDDO
  ENDIF
  RETURN
  END      

! ==================================================================================================
  SUBROUTINE ERL_DISTR(MU, K, X)
! ----------------------------------------------------------------------
!     Derive headways from the Erlang distribution.
! ----------------------------------------------------------------------
  USE ENTRYNODE_DATA
  USE SIMPARAMS
  IMPLICIT NONE
  REAL, INTENT(IN) :: MU
  INTEGER, INTENT(IN) :: K
  REAL, INTENT(OUT) :: X
  INTEGER :: I
  REAL, ALLOCATABLE :: Y(:)
! ----------------------------------------------------------------------
  ALLOCATE(Y(K))
  CALL RANDOM_NUMBER(Y)
  X = 0
  DO I = 1, K - 1
    X = X - MU / K * LOG(1-Y(I))
  ENDDO
  X = INT(X / TIMESTEP + 0.5) * TIMESTEP
  X = MAX(X, MINSEP)
  DEALLOCATE(Y)
  RETURN
  END      

! ==================================================================================================
  SUBROUTINE INIT_QUEUES
! ----------------------------------------------------------------------
!     this routine nullifies the queue pointers and zeros out arrays used
!     to emit vehicles.
! ----------------------------------------------------------------------
  USE FLOWDATA_MOD
  USE QUEUE_MOD
  IMPLICIT NONE
  INTEGER :: IN
! ----------------------------------------------------------------------
  DO IN = 1, NUMBER_OF_ENTRYNODES
    RSPEC(IN) = 0
    NEMITD(IN) = 0
    NULLIFY(FRONT(IN)%PTR,REAR(IN)%PTR)
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE EMPTY_QUEUES
! ----------------------------------------------------------------------
!     This routine walks through the vehicle emitter queues and removes
!     all the vehicles that were waiting to be emitted. Used when the
!     initialization period ends and again when the simulation ends.
! ----------------------------------------------------------------------
  USE FLOWDATA_MOD
  USE QUEUE_MOD
  IMPLICIT NONE
  TYPE(QNODE),POINTER::TEMP
  INTEGER :: IN
! ----------------------------------------------------------------------
  DO IN = 1, NUMBER_OF_ENTRYNODES
    RSPEC(IN) = 0
    NEMITD(IN) = 0
    DO WHILE(ASSOCIATED(FRONT(IN)%PTR))
      TEMP => FRONT(IN)%PTR%NEXT
      DEALLOCATE(FRONT(IN)%PTR)
      FRONT(IN)%PTR => TEMP
    ENDDO
  ENDDO
  DO WHILE(ASSOCIATED(PBV_FRONT%PTR))
    TEMP => PBV_FRONT%PTR%NEXT
    DEALLOCATE(PBV_FRONT%PTR)
    PBV_FRONT%PTR => TEMP
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE GET_LINKDATA(IUP, IDOWN, NETWORK, ILN, LANES)
! ----------------------------------------------------------------------
!     This routine gets information about a link
! ----------------------------------------------------------------------
  USE SIMPARAMS
  USE FREEWAY_LINKS
  USE STREET_LINKS
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IUP, IDOWN
  INTEGER, INTENT(OUT) :: NETWORK, ILN, LANES
  INTEGER :: IL, ILUP, ILDN
! ----------------------------------------------------------------------
  NETWORK = 0
  ILN = 0
  LANES = 0
  DO IL = 1, N_FREEWAY_LINKS
    ILUP = FUSN(IL)
    ILDN = FDSN(IL)
    IF(ILUP .EQ. IUP .AND. ILDN .EQ. IDOWN) THEN
      NETWORK = 1
      ILN = IL
      LANES = FNUMLANES(IL)
      RETURN
    ENDIF
  ENDDO
  DO IL = 1, N_STREET_LINKS
    ILUP = SUSN(IL)
    ILDN = SDSN(IL)
    IF(ILUP .EQ. IUP .AND. ILDN .EQ. IDOWN) THEN
      NETWORK = 2
      ILN = IL
      LANES = SNUMLANES(IL)
      RETURN
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE GET_PBV_INPUTS
! ----------------------------------------------------------------------
! --- This routine reads inputs for path following operation.
! ----------------------------------------------------------------------
  USE PATH_MOD
  USE VEHICLE_TYPES
  USE SIMPARAMS
  USE TEXT
  USE SEEDS
  USE FREEWAY_LINKS
  USE STREET_LINKS
  USE FREEWAY_VEHICLES
  USE STREET_VEHICLES
  USE CAR_FOLLOWING
  IMPLICIT NONE
  INCLUDE 'IOFILES.INC'
  INTEGER :: NNODE, I, N, ID, IL, NETWORK
  INTEGER :: ENODE, PATHID, DRIVER, FLEET, VTYPE, EVDATA = 0, OVRSPD, IRANGE
  REAL :: TIME, RNDNUM
  CHARACTER*1000 :: LINE, PNAME, VNAME
  LOGICAL :: WPEXIST, WVEXIST
! ----------------------------------------------------------------------
  INTERFACE TO INTEGER FUNCTION ADD_VEHICLE[DLLEXPORT, STDCALL] &
  (TIME, INODE, PATHID, DRIVER, IFLT, IVTYP, OVRSPD, IRANGE)
  INTEGER, INTENT(IN) :: INODE, PATHID, DRIVER, IFLT, IVTYP, OVRSPD, IRANGE
  REAL, INTENT(IN) :: TIME
  END
! ----------------------------------------------------------------------
  PNAME = LINFNAME(1:IROOT)//'PAT'
  VNAME = LINFNAME(1:IROOT)//'VEH'
  INQUIRE(FILE = PNAME, EXIST=WPEXIST)
  INQUIRE(FILE = VNAME, EXIST=WVEXIST)
  IF(WPEXIST) THEN
    IF(.NOT. ALLOCATED(NNODES)) THEN
      OPEN(999, FILE = PNAME, ERR=10, IOMSG=ETEXT)
      ALLOCATE(NNODES(100))
      ALLOCATE(PATH_NODES(100, 100))
      I = 0
 1    CONTINUE
      READ(999, '(A)', END=2, ERR=20, IOMSG=ETEXT) LINE
      IF(LINE(1:1) .EQ. '/' .OR. LINE(1:1000) .EQ. '') GOTO 1
      NNODE = 0
      IF(LINE(1:1) .NE. ' ') NNODE = 1
      DO N = 1, 999
        IF(LINE(N:N) .EQ. ' ' .AND. LINE(N+1:N+1) .NE. ' ') THEN
          NNODE = NNODE + 1
        ENDIF
      ENDDO
      I = I + 1
      NNODES(I) = NNODE
      READ(LINE, *, END = 20, ERR=20, IOMSG=ETEXT) (PATH_NODES(I, N), N=1,NNODE)
      GOTO 1
2     CONTINUE
      CLOSE(999)
    ENDIF
  ENDIF
  IF(WVEXIST) THEN
    OPEN(999, FILE = VNAME, ERR=10, IOMSG=ETEXT)
11   CONTINUE
    READ(999, '(A)', END=3, ERR=20, IOMSG=ETEXT) LINE
    IF(LINE(1:1) .EQ. '/' .OR. LINE(1:1000) .EQ. '') GOTO 11
    IF(LINE(1:1) .EQ. 'N' .OR. LINE(1:1) .EQ. 'n') THEN
      EVDATA = 0
      GOTO 11
    ENDIF
    IF(LINE(1:1) .EQ. 'E' .OR. LINE(1:1) .EQ. 'e') THEN
      EVDATA = 1
      GOTO 11
    ENDIF
    IF(EVDATA .EQ. 0) THEN
      READ(LINE, *, END=20, ERR=20, IOMSG=ETEXT) TIME, ENODE, PATHID, DRIVER, FLEET, VTYPE
      OVRSPD = 0
      IRANGE = 0
    ELSE
      READ(LINE, *, END=20, ERR=20, IOMSG=ETEXT) TIME, ENODE, PATHID, DRIVER, FLEET, VTYPE, OVRSPD, IRANGE
    ENDIF
    IF(FLEET .EQ. FLEET_EV) THEN
      IF(INITMODE) GOTO 11
      EV_COUNT = EV_COUNT + 1
      WEVRUN = .TRUE.
    ENDIF
    ID = ADD_VEHICLE(TIME, ENODE, PATHID, DRIVER, FLEET, VTYPE, OVRSPD, IRANGE)
    GOTO 11
3   CONTINUE
    CLOSE(999)
  ENDIF
  RETURN
10 WRITE(MSGTEXT, '(A)') 'FILE OPEN ERROR : GET_PBV_INPUTS'
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A)') ETEXT
  CALL SENDTEXTMSG(M_ERROR)
  ERROR_FLAG = 1
  RETURN
20 WRITE(MSGTEXT, '(A)') 'FILE READ ERROR : GET_PBV_INPUTS'
  CALL SENDTEXTMSG(M_ERROR)
  WRITE(MSGTEXT, '(A)') ETEXT
  CALL SENDTEXTMSG(M_ERROR)
  ERROR_FLAG = 1
  RETURN
  END  

    
    


      
