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

  MODULE NUMMOEOBJECTS
!
    IMPLICIT NONE
!
    INTEGER, ALLOCATABLE :: AC_PHASE_OBJS(:,:)
    INTEGER, ALLOCATABLE :: ROUTE_OBJS(:)
    INTEGER, ALLOCATABLE :: STATION_OBJS(:)
    INTEGER, ALLOCATABLE :: DATASTATION_OBJS(:,:)
    INTEGER, ALLOCATABLE :: LANE_FREEWAY_OBJS(:,:)
    INTEGER, ALLOCATABLE :: LANE_STREET_OBJS(:,:)
    INTEGER, ALLOCATABLE :: LINK_FREEWAY_OBJS(:,:)
    INTEGER, ALLOCATABLE :: LINK_STREET_OBJS(:,:)
    INTEGER, ALLOCATABLE :: ENTRYLINK_OBJS(:,:)
    INTEGER, ALLOCATABLE :: VTYPELINK_FREEWAY_OBJS(:,:)
    INTEGER, ALLOCATABLE :: VTYPELINK_STREET_OBJS(:,:)
      
    CONTAINS
      
    SUBROUTINE DEALLOCATE_OBJECT_ARRAYS
    IF(ALLOCATED(AC_PHASE_OBJS)) DEALLOCATE(AC_PHASE_OBJS)
    IF(ALLOCATED(ROUTE_OBJS)) DEALLOCATE(ROUTE_OBJS)
    IF(ALLOCATED(STATION_OBJS)) DEALLOCATE(STATION_OBJS)
    IF(ALLOCATED(DATASTATION_OBJS)) DEALLOCATE(DATASTATION_OBJS)
    IF(ALLOCATED(LANE_FREEWAY_OBJS)) DEALLOCATE(LANE_FREEWAY_OBJS)
    IF(ALLOCATED(LANE_STREET_OBJS)) DEALLOCATE(LANE_STREET_OBJS)
    IF(ALLOCATED(LINK_FREEWAY_OBJS)) DEALLOCATE(LINK_FREEWAY_OBJS)
    IF(ALLOCATED(LINK_STREET_OBJS)) DEALLOCATE(LINK_STREET_OBJS)
    IF(ALLOCATED(ENTRYLINK_OBJS)) DEALLOCATE(ENTRYLINK_OBJS)
    IF(ALLOCATED(VTYPELINK_FREEWAY_OBJS)) DEALLOCATE(VTYPELINK_FREEWAY_OBJS)
    IF(ALLOCATED(VTYPELINK_STREET_OBJS)) DEALLOCATE(VTYPELINK_STREET_OBJS)
    RETURN
    END SUBROUTINE
 
  END MODULE
 
! ==========================================================================================================
  INTEGER FUNCTION GETVERSIONNUMBER &
  [DLLEXPORT, STDCALL, ALIAS:'GetVersionNumber'] (VERSION)
  
  IMPLICIT NONE
  CHARACTER*18 VERSION [REFERENCE]
  INCLUDE 'VERSION.FI'
 
  VERSION = KVER
  GETVERSIONNUMBER = 0
  END
 
! ==========================================================================================================
  INTEGER FUNCTION GETRANDOMNUMBERSEEDS &
  [DLLEXPORT, STDCALL, ALIAS:'GetRandomNumberSeeds'] (HDWYSEED, VEHSEED, TRAFSEED)
  USE SEEDS
  IMPLICIT NONE
  INTEGER HDWYSEED [REFERENCE]
  INTEGER VEHSEED  [REFERENCE]
  INTEGER TRAFSEED [REFERENCE]
 
  HDWYSEED = ISEED1
  VEHSEED = ISEED2
  TRAFSEED = ISEED3
  GETRANDOMNUMBERSEEDS = 0
  END
!
! ==========================================================================================================
! Function: GETMOELISTLENGTH( int ISIZE )
!
! --- CODED   11-25-03 BY T. SIMMERMAN
!
! Description: Returns the size of the MOE list
!
!
! Arguments: ISIZE - [out] size of the MOE list
!
! Returns: int = 0 - success
! ======================================================================
  INTEGER FUNCTION GETMOELISTLENGTH &
  [DLLEXPORT, STDCALL, ALIAS:'GetMoeListLength'] (ISIZE)
  IMPLICIT NONE
  INTEGER ISIZE [REFERENCE]
  INTEGER KSIZE
  INCLUDE 'MOEBUFFERSIZE.FI'
  ISIZE = KSIZE
  GETMOELISTLENGTH = 0
 
  END FUNCTION

! ==========================================================================================================
! Function: GETMOELIST( char CBUF, int ISIZE )
!
! --- CODED   11-25-03 BY T. SIMMERMAN
!
! Description: Returns the MOE list
!
!
! Arguments: CBUF - [out] character string to hold the MOE list
!
!            ISIZE - [in] size of the MOE list
!
! Returns: int = 0 - success
! ======================================================================
  INTEGER FUNCTION GETMOELIST &
  [DLLEXPORT, STDCALL, ALIAS:'GetMoeList'] (CBUF, ISIZE)
  IMPLICIT NONE
  INTEGER :: I, IEND, ISIZE, KSIZE, KEND
  CHARACTER*(ISIZE) CBUF [REFERENCE]
 
  INCLUDE 'MOEBUFFERSIZE.FI'
 
  CHARACTER*1500 STRING
  CHARACTER*1 C1, C2
  DATA C1 /1H'/
  DATA C2 /1H#/
 
  INCLUDE 'MOELIST.FI'
 
  DO I = 1, ISIZE
    IF(CBUF(I:I) .EQ. C2) CBUF(I:I) = C1
  ENDDO
 
  GETMOELIST = 0
  END
 
! ==========================================================================================================
! Subroutine: DEALLOCATE_MOE_MEMORY
!
! Description: Deallocate memory that was dynamically allocated for
!              use in function GETNUMMOEOBJECTS
!
!
! Arguments: none
!
!
! Returns: none
! ======================================================================
  SUBROUTINE DEALLOCATE_MOE_MEMORY
  USE NUMMOEOBJECTS
  IMPLICIT NONE
  INCLUDE 'MOEFUNCTIONS.FI'
  INTEGER :: IARRAY(1), FLAG, TEMP
  REAL :: RX
! ======================================================================
 
! --- Deallocate arrays used to calculate MOEs. 
 
  FLAG = 0
  TEMP = GETBASICMOEDATA('DeallocateMemory', FLAG, RX)      
  TEMP = GETSIGNALPHASESMOEDATA('DeallocateMemory', IARRAY, FLAG, RX)
  TEMP = GETBUSROUTEMOEDATA('DeallocateMemory', IARRAY, FLAG, RX)
  TEMP = GETBUSSTATIONMOEDATA('DeallocateMemory', IARRAY, FLAG, RX)
  TEMP = GETDATASTATIONMOEDATA('DeallocateMemory', IARRAY, FLAG, RX)
  TEMP = GETFREEWAYDETECTORMOEDATA('DeallocateMemory', IARRAY, FLAG, RX)
  TEMP = GETSTREETDETECTORMOEDATA('DeallocateMemory', IARRAY, FLAG, RX)
  TEMP = GETFREEWAYLANEMOEDATA('DeallocateMemory', IARRAY, FLAG, RX)
  TEMP = GETSTREETLANEMOEDATA('DeallocateMemory', IARRAY, FLAG, RX)
  TEMP = GETENTRYLINKMOEDATA('DeallocateMemory', IARRAY, FLAG, RX)
  TEMP = GETFREEWAYLINKMOEDATA('DeallocateMemory', IARRAY, FLAG, RX)
  TEMP = GETSTREETLINKMOEDATA('DeallocateMemory', IARRAY, FLAG, RX)
  TEMP = GETNETWORKFREEWAYMOEDATA('DeallocateMemory', IARRAY, FLAG, RX)
  TEMP = GETNETWORKSTREETMOEDATA('DeallocateMemory', IARRAY, FLAG, RX)
  TEMP = GETVEHICLETYPESTREETLINKMOEDATA('DeallocateMemory', IARRAY, FLAG, RX)
  TEMP = GETVEHICLETYPEFREEWAYLINKMOEDATA('DeallocateMemory', IARRAY, FLAG, RX)
  RETURN
  END
!
! ==========================================================================================================
! Function: GETNUMMOEOBJECTS( char CLASSNAME, int ARGS, int OBJNUM )
!
! --- CODED   11-25-03 BY T. SIMMERMAN
!
! Description: When called with OBJNUM = 0 
!              Returns the number of objects that exist in the network
!              for the specified MOE CLASS, NOBJ
!
!              When called with OBJNUM = 1, 2, ..., NOBJ
!              Returns the input parameters for that object for the
!              specified MOE CLASS
!
!
! Arguments: CLASSNAME - [in] name of the MOE CLASS
!
!            ARGS  -     [out] array of input parameters
!                        to be used to call the MOE function
!
!            OBJNUM    - [in, out] number of objects
!
! Returns: int = 0 - success
! Returns: int = 1 - CLASS name was not recognized or object was not found
! Returns: int = 2 - memory could not be allocated
! ======================================================================
  INTEGER FUNCTION GETNUMMOEOBJECTS & 
  [DLLEXPORT, ALIAS:'GetNumMoeObjects'](CLASSNAME, ARGS, OBJNUM)
 
  USE FREEWAY_LINKS
  USE STREET_LINKS
  USE FREEWAY_DETECTORS
  USE STREET_DETECTORS
  USE NUMMOEOBJECTS
  USE TEXT
  USE ENTRYNODE_DATA
  USE BUS_ROUTE_DATA
  USE BUS_STATION_DATA
  USE DATASTATIONS
  USE GLOBAL_DATA
  USE VEHICLE_TYPES
  USE SIMPARAMS
  USE ACTUATED_CONTROLLERS
  USE NODE_TABLE
  IMPLICIT NONE
  CHARACTER*(*) CLASSNAME  
  INTEGER     ARGS       [REFERENCE]
  INTEGER     OBJNUM     [REFERENCE]
  DIMENSION ARGS(*)
       
  INTEGER :: I, IWORK(N_FREEWAY_LANES), STATUS, NOBJ
  INTEGER :: IL, ILN, ILINK
  INTEGER :: IUP, IDW, NLANES, IADP, IAUX
  INTEGER :: PKLANE, IPKT, ITYPE, IROUTE, NROUTES = 0, NSTATIONS = 0
  INTEGER :: DSTATIONS, IACT, PHASE
  REAL :: DSPOS
 
  GETNUMMOEOBJECTS = 0
  STATUS = 0
! ----------------------------------------------------------------------
  IF(CLASSNAME .EQ. 'SIGNALPHASES') THEN
    IF(OBJNUM .EQ. 0) THEN
      OBJNUM = NUMBER_OF_ACS * 8
      IF(OBJNUM .GT. 0) THEN
        IF(.NOT. ALLOCATED(AC_PHASE_OBJS)) ALLOCATE(AC_PHASE_OBJS(OBJNUM, 2), STAT=STATUS)
        IF(STATUS .NE. 0) GOTO 10
        NOBJ = 0
        DO IACT = 1, NUMBER_OF_ACS
          DO PHASE = 1, 8
            NOBJ = NOBJ + 1
            AC_PHASE_OBJS(NOBJ, 1) = AC_SIGNALS(IACT)%NODE(1)
            AC_PHASE_OBJS(NOBJ, 2) = PHASE
          ENDDO
        ENDDO
      ENDIF
    ELSEIF(OBJNUM .LE. NUMBER_OF_ACS * 8) THEN
      ARGS(1) = AC_PHASE_OBJS(OBJNUM, 1)
      ARGS(2) = AC_PHASE_OBJS(OBJNUM, 2)
    ELSE
      GETNUMMOEOBJECTS = 1
    ENDIF
  ELSEIF(CLASSNAME .EQ. 'BUSROUTE') THEN
    IF(OBJNUM .EQ. 0) THEN
      DO IROUTE = 1, NUMBER_OF_ROUTES
        IF(BUSR_NNODES(IROUTE) .GT. 0) OBJNUM = OBJNUM + 1
      ENDDO
      IF(OBJNUM .GT. 0) THEN
        IF(.NOT. ALLOCATED(ROUTE_OBJS)) ALLOCATE(ROUTE_OBJS(OBJNUM), STAT=STATUS)
        IF(STATUS .NE. 0) GOTO 10
        NOBJ = 0
        DO IROUTE = 1, NUMBER_OF_ROUTES
          IF(BUSR_NNODES(IROUTE) .GT. 0) THEN
            NOBJ = NOBJ + 1
            ROUTE_OBJS(NOBJ) = IROUTE
          ENDIF
        ENDDO
        NROUTES = OBJNUM
      ENDIF
    ELSEIF(OBJNUM .LE. NROUTES) THEN
      ARGS(1) = ROUTE_OBJS(OBJNUM)
    ELSE
      GETNUMMOEOBJECTS = 1
    ENDIF
! ----------------------------------------------------------------------
  ELSEIF(CLASSNAME .EQ. 'BUSSTATION') THEN
    IF(OBJNUM .EQ. 0) THEN
      DO IROUTE = 1, NUMBER_OF_ROUTES
        IF(BUSR_NNODES(IROUTE) .GT. 0) THEN
          I = 1
          DO WHILE(BUSR_STATIONLIST(IROUTE, I) .NE. 0)
            OBJNUM = OBJNUM + 1
            I = I + 1
          ENDDO
        ENDIF
      ENDDO
      IF(OBJNUM .GT. 0) THEN
        IF(.NOT. ALLOCATED(STATION_OBJS)) ALLOCATE(STATION_OBJS(OBJNUM), STAT=STATUS)
        IF(STATUS .NE. 0) GOTO 10
        NOBJ = 0
        DO IROUTE = 1, NUMBER_OF_ROUTES
          IF(BUSR_NNODES(IROUTE) .GT. 0) THEN
            I = 1
            DO WHILE(BUSR_STATIONLIST(IROUTE, I) .NE. 0)
              NOBJ = NOBJ + 1
              STATION_OBJS(NOBJ) = BUSR_STATIONLIST(IROUTE, I)
              I = I + 1
            ENDDO
          ENDIF
        ENDDO
        NSTATIONS = OBJNUM
        IF(.NOT. ALLOCATED(STATION_OBJS)) ALLOCATE(STATION_OBJS(OBJNUM), STAT=STATUS)
        IF(STATUS .NE. 0) GOTO 10
      ENDIF
    ELSEIF(OBJNUM .LE. NSTATIONS) THEN
      ARGS(1) = STATION_OBJS(OBJNUM)
    ELSE
      GETNUMMOEOBJECTS = 1
    ENDIF
! ----------------------------------------------------------------------
  ELSEIF(CLASSNAME .EQ. 'DATASTATION') THEN
    IF(OBJNUM .EQ. 0) THEN
      OBJNUM = NUMBER_OF_DATASTATIONS * N_FREEWAY_LANES
      IF(ALLOCATED(DATASTATION_OBJS)) DEALLOCATE(DATASTATION_OBJS)
      ALLOCATE(DATASTATION_OBJS(OBJNUM, 3), STAT=STATUS)
      IF(STATUS .NE. 0) GOTO 10
      NOBJ = 0
      DO IL = 1, N_FREEWAY_LINKS
        IF(DATASTATION_ID(IL) .GT. 0) THEN
          IUP = FUSN(IL)
          IDW = FDSN(IL)
          DSPOS = DATASTATION_LOCATION(IL)
          DO ILN = 1, N_FREEWAY_LANES
            IWORK(ILN) = 0
          ENDDO
          NLANES = FNUMLANES(IL)
          DO IADP = 1, 3
            IF(ADDDROP_DIST(IL, IADP) .LT. DSPOS) NLANES = NLANES + ADDDROP_CODE(IL, IADP)
          ENDDO
          DO ILN = 1, NLANES
            IWORK(ILN) = 1
          ENDDO
          DO IAUX = 1, N_AUXLANES
            IF(AUX_LANE_ID(IL, IAUX) .EQ. 0) EXIT !no more auxiliary lanes on this link
            IF(AUX_LANE_CODE(IL, IAUX) .EQ. AUX_FULL .OR. &
               AUX_LANE_LENGTH(IL, IAUX) .GE. DSPOS) THEN
              IWORK(AUX_LANE_ID(IL, IAUX)) = 1
            ENDIF
          ENDDO
          DO ILN = 1, N_FREEWAY_LANES
            IF(IWORK(ILN) .EQ. 0) CYCLE
            NOBJ = NOBJ + 1
            DATASTATION_OBJS(NOBJ, 1) = IUP
            DATASTATION_OBJS(NOBJ, 2) = IDW
            DATASTATION_OBJS(NOBJ, 3) = ILN
          ENDDO
        ENDIF
      ENDDO
      OBJNUM = NOBJ
      DSTATIONS = OBJNUM
    ELSEIF(OBJNUM .LE. DSTATIONS) THEN
      ARGS(1) = DATASTATION_OBJS(OBJNUM, 1)
      ARGS(2) = DATASTATION_OBJS(OBJNUM, 2)
      ARGS(3) = DATASTATION_OBJS(OBJNUM, 3)
    ELSE
      GETNUMMOEOBJECTS = 1
    ENDIF
! ----------------------------------------------------------------------
  ELSEIF(CLASSNAME .EQ. 'FREEWAY_DETECTOR') THEN
    IF(OBJNUM .EQ. 0) THEN
      OBJNUM = N_FREEWAY_DETECTORS
    ELSEIF(OBJNUM .LE. N_FREEWAY_DETECTORS) THEN
      IL = FDETECTOR(OBJNUM)%LINK
      ARGS(1) = FUSN(IL)
      ARGS(2) = FDSN(IL)
      ARGS(3) = OBJNUM
    ELSE
      GETNUMMOEOBJECTS = 1
    ENDIF
! ----------------------------------------------------------------------
  ELSEIF(CLASSNAME .EQ. 'STREET_DETECTOR') THEN
    IF(OBJNUM .EQ. 0) THEN
      OBJNUM = N_STREET_DETECTORS
    ELSEIF(OBJNUM .LE. N_STREET_DETECTORS) THEN
      IL = SDETECTOR(OBJNUM)%LINK
      ARGS(1) = SUSN(IL)
      ARGS(2) = SDSN(IL)
      ARGS(3) = OBJNUM
    ELSE
      GETNUMMOEOBJECTS = 1
    ENDIF
! ----------------------------------------------------------------------
  ELSEIF(CLASSNAME .EQ. 'ENTRYLINK') THEN
    IF(OBJNUM .EQ. 0) THEN
      OBJNUM = 1000
      IF(ALLOCATED(ENTRYLINK_OBJS)) DEALLOCATE(ENTRYLINK_OBJS)
      ALLOCATE(ENTRYLINK_OBJS(OBJNUM, 2), STAT=STATUS)
      IF(STATUS .NE. 0) GOTO 10
      NOBJ = 0
      DO IL = 1, N_FREEWAY_LINKS
        IF(NODE_TYPE(FUSN(IL)) .EQ. NT_EXTERN) THEN
          NOBJ = NOBJ + 1
          ENTRYLINK_OBJS(NOBJ, 1) = FUSN(IL)
          ENTRYLINK_OBJS(NOBJ, 2) = FDSN(IL)
        ENDIF
      ENDDO
      DO IL = 1, N_STREET_LINKS
        IF(NODE_TYPE(SUSN(IL)) .EQ. NT_EXTERN) THEN
          NOBJ = NOBJ + 1
          ENTRYLINK_OBJS(NOBJ, 1) = SUSN(IL)
          ENTRYLINK_OBJS(NOBJ, 2) = SDSN(IL)
        ENDIF
      ENDDO
      OBJNUM = NOBJ
    ELSEIF(OBJNUM .LE. 2000) THEN
      ARGS(1) = ENTRYLINK_OBJS(OBJNUM, 1)
      ARGS(2) = ENTRYLINK_OBJS(OBJNUM, 2)
    ELSE
      GETNUMMOEOBJECTS = 1
    ENDIF
! ----------------------------------------------------------------------
  ELSEIF(CLASSNAME .EQ. 'LANE_FREEWAY') THEN
    IF(OBJNUM .EQ. 0) THEN
      OBJNUM = N_FREEWAY_LINKS * N_FREEWAY_LANES
      IF(.NOT. ALLOCATED(LANE_FREEWAY_OBJS)) ALLOCATE(LANE_FREEWAY_OBJS(OBJNUM, 3), STAT=STATUS)
      IF(STATUS .NE. 0) GOTO 10
      NOBJ = 0
      DO IL = 1, N_FREEWAY_LINKS
        IF(NODE_TYPE(FUSN(IL)) .EQ. NT_EXTERN .OR. NODE_TYPE(FDSN(IL)) .EQ. NT_EXTERN) CYCLE
        IUP = FUSN(IL)
        IDW = FDSN(IL)
        DO ILN = 1, N_FREEWAY_LANES
          IWORK(ILN) = 0
        ENDDO
        NLANES = FNUMLANES(IL)
        DO ILN = 1, NLANES
          IWORK(ILN) = 1
        ENDDO
        DO IADP = 1, 3
          IF(ADDDROP_LANE(IL, IADP) .EQ. 0) EXIT !no more add/drops on this link
          IF(ADDDROP_CODE(IL, IADP) .GT. 0) THEN
            NLANES = NLANES + 1
            IWORK(NLANES) = 1
          ENDIF
        ENDDO
        DO IAUX = 1, N_AUXLANES
          IF(AUX_LANE_ID(IL, IAUX) .GT. 0) IWORK(AUX_LANE_ID(IL, IAUX)) = 1
        ENDDO
        DO ILN = 1, N_FREEWAY_LANES
          IF(IWORK(ILN) .EQ. 0) CYCLE
          NOBJ = NOBJ + 1
          LANE_FREEWAY_OBJS(NOBJ, 1) = IUP
          LANE_FREEWAY_OBJS(NOBJ, 2) = IDW
          LANE_FREEWAY_OBJS(NOBJ, 3) = ILN
        ENDDO
      ENDDO
      OBJNUM = NOBJ
    ELSE
      ARGS(1) = LANE_FREEWAY_OBJS(OBJNUM, 1)
      ARGS(2) = LANE_FREEWAY_OBJS(OBJNUM, 2)
      ARGS(3) = LANE_FREEWAY_OBJS(OBJNUM, 3)
    ENDIF
! ----------------------------------------------------------------------
  ELSEIF(CLASSNAME .EQ. 'LANE_STREET') THEN
    IF(OBJNUM .EQ. 0) THEN
      OBJNUM = N_STREET_LINKS * 7
      IF(.NOT. ALLOCATED(LANE_STREET_OBJS)) ALLOCATE(LANE_STREET_OBJS(OBJNUM, 3), STAT=STATUS)
      IF(STATUS .NE. 0) GOTO 10
      NOBJ = 0
      DO IL = 1, N_STREET_LINKS
        IF(NODE_TYPE(SUSN(IL)) .EQ. NT_EXTERN .OR. NODE_TYPE(SDSN(IL)) .EQ. NT_EXTERN) CYCLE
        IUP = SUSN(IL)
        IDW = SDSN(IL)
        NLANES = TOTAL_LANES(IL)
        DO ILN = 1, NLANES
          NOBJ = NOBJ + 1
          LANE_STREET_OBJS(NOBJ, 1) = IUP
          LANE_STREET_OBJS(NOBJ, 2) = IDW
          LANE_STREET_OBJS(NOBJ, 3) = ILN
        ENDDO
      ENDDO
      OBJNUM = NOBJ
    ELSE
      ARGS(1) = LANE_STREET_OBJS(OBJNUM, 1)
      ARGS(2) = LANE_STREET_OBJS(OBJNUM, 2)
      ARGS(3) = LANE_STREET_OBJS(OBJNUM, 3)
    ENDIF
! ----------------------------------------------------------------------
  ELSEIF(CLASSNAME .EQ. 'LINK_FREEWAY') THEN
    IF(OBJNUM .EQ. 0) THEN
      IF(.NOT. ALLOCATED(LINK_FREEWAY_OBJS)) ALLOCATE(LINK_FREEWAY_OBJS(N_FREEWAY_LINKS, 2), STAT=STATUS)
      ILINK = 0 
      DO IL = 1, N_FREEWAY_LINKS
        IF(NODE_TYPE(FUSN(IL)) .EQ. NT_EXTERN .OR. NODE_TYPE(FDSN(IL)) .EQ. NT_EXTERN) CYCLE
        ILINK = ILINK + 1
        IUP = FUSN(IL)
        IDW = FDSN(IL)
        LINK_FREEWAY_OBJS(ILINK, 1) = IUP
        LINK_FREEWAY_OBJS(ILINK, 2) = IDW
      ENDDO
      OBJNUM = ILINK
    ELSEIF(OBJNUM .LE. N_FREEWAY_LINKS) THEN  
      ARGS(1) = LINK_FREEWAY_OBJS(OBJNUM, 1)
      ARGS(2) = LINK_FREEWAY_OBJS(OBJNUM, 2)
    ENDIF
! ----------------------------------------------------------------------
  ELSEIF(CLASSNAME .EQ. 'LINK_STREET') THEN
    IF(OBJNUM .EQ. 0) THEN
      IF(.NOT. ALLOCATED(LINK_STREET_OBJS)) ALLOCATE(LINK_STREET_OBJS(N_STREET_LINKS, 2), STAT=STATUS)
      ILINK = 0 
      DO IL = 1, N_STREET_LINKS
        IF(NODE_TYPE(SUSN(IL)) .EQ. NT_EXTERN .OR. NODE_TYPE(SDSN(IL)) .EQ. NT_EXTERN) CYCLE
        ILINK = ILINK + 1
        IUP = SUSN(IL)
        IDW = SDSN(IL)
        LINK_STREET_OBJS(ILINK, 1) = IUP
        LINK_STREET_OBJS(ILINK, 2) = IDW
      ENDDO
      OBJNUM = ILINK
    ELSEIF(OBJNUM .LE. N_STREET_LINKS) THEN  
      ARGS(1) = LINK_STREET_OBJS(OBJNUM, 1)
      ARGS(2) = LINK_STREET_OBJS(OBJNUM, 2)
    ENDIF        
! ----------------------------------------------------------------------
  ELSEIF(CLASSNAME .EQ. 'NETWORK_FREEWAY') THEN
    IF(OBJNUM .LE. 1) THEN
      OBJNUM = 1
    ELSE
      GETNUMMOEOBJECTS = 1
    ENDIF
! ----------------------------------------------------------------------
  ELSEIF(CLASSNAME .EQ. 'NETWORK_STREET') THEN
    IF(OBJNUM .LE. 1) THEN
      OBJNUM = 1
    ELSE
      GETNUMMOEOBJECTS = 1
    ENDIF
! ----------------------------------------------------------------------
  ELSEIF(CLASSNAME .EQ. 'VEHICLETYPE_LINK_FREEWAY') THEN
    IF(OBJNUM .EQ. 0) THEN
      IF(.NOT. ALLOCATED(VTYPELINK_FREEWAY_OBJS)) THEN
        ALLOCATE(VTYPELINK_FREEWAY_OBJS(N_FREEWAY_LINKS*NTYPES, 3), STAT=STATUS)
        !CALL DETERMINE_ACTIVE_VEHICLE_TYPES
      ENDIF
      DO IL = 1, N_FREEWAY_LINKS
        IF(NODE_TYPE(FUSN(IL)) .EQ. NT_EXTERN .OR. NODE_TYPE(FDSN(IL)) .EQ. NT_EXTERN) CYCLE
        DO ITYPE = 1, NTYPES
          OBJNUM = OBJNUM + 1
          VTYPELINK_FREEWAY_OBJS(OBJNUM, 1) = FUSN(IL)
          VTYPELINK_FREEWAY_OBJS(OBJNUM, 2) = FDSN(IL)
          VTYPELINK_FREEWAY_OBJS(OBJNUM, 3) = ITYPE
        ENDDO
      ENDDO
    ELSEIF(OBJNUM .LE. N_FREEWAY_LINKS*NTYPES) THEN  
      ARGS(1) = VTYPELINK_FREEWAY_OBJS(OBJNUM, 1)
      ARGS(2) = VTYPELINK_FREEWAY_OBJS(OBJNUM, 2)
      ARGS(3) = VTYPELINK_FREEWAY_OBJS(OBJNUM, 3)
    ENDIF
! ----------------------------------------------------------------------
  ELSEIF(CLASSNAME .EQ. 'VEHICLETYPE_LINK_STREET') THEN
    IF(OBJNUM .EQ. 0) THEN
      IF(.NOT. ALLOCATED(VTYPELINK_STREET_OBJS)) THEN
        ALLOCATE(VTYPELINK_STREET_OBJS(N_STREET_LINKS*NTYPES, 3), STAT=STATUS)
        !CALL DETERMINE_ACTIVE_VEHICLE_TYPES
      ENDIF
      DO IL = 1, N_STREET_LINKS
        IF(NODE_TYPE(SUSN(IL)) .EQ. NT_EXTERN .OR. NODE_TYPE(SDSN(IL)) .EQ. NT_EXTERN) CYCLE
        DO ITYPE = 1, NTYPES
          OBJNUM = OBJNUM + 1
          VTYPELINK_STREET_OBJS(OBJNUM, 1) = SUSN(IL)
          VTYPELINK_STREET_OBJS(OBJNUM, 2) = SDSN(IL)
          VTYPELINK_STREET_OBJS(OBJNUM, 3) = ITYPE
        ENDDO
      ENDDO
    ELSEIF(OBJNUM .LE. N_STREET_LINKS*NTYPES) THEN  
      ARGS(1) = VTYPELINK_STREET_OBJS(OBJNUM, 1)
      ARGS(2) = VTYPELINK_STREET_OBJS(OBJNUM, 2)
      ARGS(3) = VTYPELINK_STREET_OBJS(OBJNUM, 3)
    ENDIF
! ----------------------------------------------------------------------
  ELSE
    GETNUMMOEOBJECTS = 1
    WRITE(MSGTEXT, '(A)') 'GETNUMMOEOBJECTS'
    CALL SENDTEXTMSG(3)
    WRITE(MSGTEXT, '(A,A)') 'FAILED TO RECOGNIZE CLASSNAME ', CLASSNAME
    CALL SENDTEXTMSG(3)
  ENDIF
10 IF(STATUS .NE. 0) THEN
    CALL SENDTEXTMSG2('MEMORY ALLOCATION FAILED IN FUNCTION: ', 'GETNUMMOEOBJECTS', 3)
    GETNUMMOEOBJECTS = 2
  ENDIF
  END
!
! ==========================================================================================================
! Function: GETSIGNALPHASESMOEDATA( char MOETYPE, int IARRAY, int FLAG, float RMOE )
!
! Description: Returns the value of the specified MOE for the bus route
!
!
! Arguments: MOETYPE - [in] name of the MOE
!
!            IARRAY  - [in] array of input parameters
!                           (route number)
!
!            FLAG    - [in] MOE granularity
!                           0 = cumulative
!                           1 = time interval
!                           2 = time period
!
!            RMOE    - [out] value of the MOE
!
! Returns: int = 0 - success
! Returns: int = 1 - MOE name was not recognized
! ======================================================================
  INTEGER RECURSIVE FUNCTION GETSIGNALPHASESMOEDATA &
  [DLLEXPORT, ALIAS:'GetSignalPhasesMoeData'](MOETYPE, IARRAY, FLAG, RMOE)
 
  USE SIMPARAMS
  USE TEXT
  USE ACTUATED_CONTROLLERS
  USE STREET_NODES
  IMPLICIT NONE
  CHARACTER*(*) MOETYPE
  INTEGER     IARRAY  [REFERENCE]
  INTEGER     FLAG    [VALUE]
  REAL        RMOE    [REFERENCE]
  DIMENSION IARRAY(*)
 
  REAL SAVED(:,:,:,:)
  ALLOCATABLE SAVED
  SAVE SAVED
  INTEGER :: STATUS, TEMP, RSAVE, MTIME, MOENUM, MDAT, NODE, IACT, PHASE
  REAL :: R1, R2
 
  STATUS = 0
  GETSIGNALPHASESMOEDATA = 0
        
  IF(MOETYPE .EQ. 'DeallocateMemory') THEN
    IF(ALLOCATED(SAVED)) DEALLOCATE(SAVED)
  ELSE
    IF(.NOT. ALLOCATED(SAVED)) THEN
      ALLOCATE(SAVED(9, MAX_NODE_NUMBER, 8, 8), STAT = STATUS)
      IF(STATUS .NE. 0) GOTO 10
      SAVED = 0.0
    ENDIF
 
    IF(FLAG .EQ. 0) THEN
      MTIME = 1
      MDAT = 2
    ELSEIF(FLAG .EQ. 1) THEN
      MTIME = 3
      MDAT = 4
    ELSEIF(FLAG .EQ. 2) THEN
      MTIME = 6
      MDAT = 7
    ELSE
      GETSIGNALPHASESMOEDATA = 2
      GOTO 10
    ENDIF
        
    NODE = IARRAY(1)
    PHASE = IARRAY(2)
    RMOE = 0.0
    IF(NODE .LE. 0) RETURN
    IACT = NACT(NODE)       
! ----------------------------------------------------------------------
    IF(MOETYPE .EQ. 'GreenTimeTotal') THEN
      MOENUM = 1
      IF(SIMTIME .EQ. SAVED(MOENUM, IACT, PHASE, MTIME)) THEN
        RMOE = SAVED(MOENUM, IACT, PHASE, MDAT)
      ELSE
        RMOE = AC_SIGNALS(IACT)%TOTAL_GREEN_TIME(PHASE) / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IACT, PHASE, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IACT, PHASE, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IACT, PHASE, MDAT) = RMOE
          SAVED(MOENUM, IACT, PHASE, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IACT, PHASE, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MaxoutsTotal') THEN
      MOENUM = 2
      IF(SIMTIME .EQ. SAVED(MOENUM, IACT, PHASE, MTIME)) THEN
        RMOE = SAVED(MOENUM, IACT, PHASE, MDAT)
      ELSE
        RMOE = AC_SIGNALS(IACT)%MAXOUTS(PHASE) 
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IACT, PHASE, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IACT, PHASE, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IACT, PHASE, MDAT) = RMOE
          SAVED(MOENUM, IACT, PHASE, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IACT, PHASE, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MinimumGreensTotal') THEN
      MOENUM = 3
      IF(SIMTIME .EQ. SAVED(MOENUM, IACT, PHASE, MTIME)) THEN
        RMOE = SAVED(MOENUM, IACT, PHASE, MDAT)
      ELSE
        RMOE = AC_SIGNALS(IACT)%MIN_GREENS(PHASE)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IACT, PHASE, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IACT, PHASE, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IACT, PHASE, MDAT) = RMOE
          SAVED(MOENUM, IACT, PHASE, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IACT, PHASE, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'NumberOfCyclesTotal') THEN
      MOENUM = 4
      IF(SIMTIME .EQ. SAVED(MOENUM, IACT, PHASE, MTIME)) THEN
        RMOE = SAVED(MOENUM, IACT, PHASE, MDAT)
      ELSE
        RMOE = AC_SIGNALS(IACT)%TIMES_STARTED(PHASE)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IACT, PHASE, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IACT, PHASE, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IACT, PHASE, MDAT) = RMOE
          SAVED(MOENUM, IACT, PHASE, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IACT, PHASE, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'PercentageOfMaxouts') THEN
        
      MOENUM = 5
      IF(SIMTIME .EQ. SAVED(MOENUM, IACT, PHASE, MTIME)) THEN
        RMOE = SAVED(MOENUM, IACT, PHASE, MDAT)
      ELSE
        TEMP = GETSIGNALPHASESMOEDATA('MaxoutsTotal', IARRAY, FLAG, R1)
        TEMP = GETSIGNALPHASESMOEDATA('NumberOfCyclesTotal', IARRAY, FLAG, R2)
        IF(R2 .NE. 0) RMOE = 100 * R1 / R2
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IACT, PHASE, MDAT) = RMOE
        SAVED(MOENUM, IACT, PHASE, MDAT+1) = RMOE
        SAVED(MOENUM, IACT, PHASE, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'PercentageOfMinimumGreens') THEN
        
      MOENUM = 6
      IF(SIMTIME .EQ. SAVED(MOENUM, IACT, PHASE, MTIME)) THEN
        RMOE = SAVED(MOENUM, IACT, PHASE, MDAT)
      ELSE
        TEMP = GETSIGNALPHASESMOEDATA('MinimumGreensTotal', IARRAY, FLAG, R1)
        TEMP = GETSIGNALPHASESMOEDATA('NumberOfCyclesTotal', IARRAY, FLAG, R2)
        IF(R2 .NE. 0) RMOE = 100 * R1 / R2
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IACT, PHASE, MDAT) = RMOE
        SAVED(MOENUM, IACT, PHASE, MDAT+1) = RMOE
        SAVED(MOENUM, IACT, PHASE, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'RedLightRunnersTotal') THEN
      MOENUM = 7
      IF(SIMTIME .EQ. SAVED(MOENUM, IACT, PHASE, MTIME)) THEN
        RMOE = SAVED(MOENUM, IACT, PHASE, MDAT)
      ELSE
        RMOE = AC_SIGNALS(IACT)%RED_LIGHT_RUNNERS(PHASE)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IACT, PHASE, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IACT, PHASE, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IACT, PHASE, MDAT) = RMOE
          SAVED(MOENUM, IACT, PHASE, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IACT, PHASE, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'VehiclesDischargedTotal') THEN
      MOENUM = 8
      IF(SIMTIME .EQ. SAVED(MOENUM, IACT, PHASE, MTIME)) THEN
        RMOE = SAVED(MOENUM, IACT, PHASE, MDAT)
      ELSE
        RMOE = AC_SIGNALS(IACT)%VEHICLES_DISCHARGED(PHASE)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IACT, PHASE, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IACT, PHASE, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IACT, PHASE, MDAT) = RMOE
          SAVED(MOENUM, IACT, PHASE, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IACT, PHASE, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'VehiclesInDilemmaZoneTotal') THEN
      MOENUM = 9
      IF(SIMTIME .EQ. SAVED(MOENUM, IACT, PHASE, MTIME)) THEN
        RMOE = SAVED(MOENUM, IACT, PHASE, MDAT)
      ELSE
        RMOE = AC_SIGNALS(IACT)%VEHICLES_INDZ(PHASE)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IACT, PHASE, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IACT, PHASE, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IACT, PHASE, MDAT) = RMOE
          SAVED(MOENUM, IACT, PHASE, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IACT, PHASE, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSE
      GETSIGNALPHASESMOEDATA = 1
      WRITE(MSGTEXT, '(A)') 'GETSIGNALPHASESMOEDATA'
      CALL SENDTEXTMSG(3)
      WRITE(MSGTEXT, '(A,A)') 'FAILED TO RECOGNIZE CLASSNAME ', MOETYPE
      CALL SENDTEXTMSG(3)
    ENDIF
  ENDIF
10 IF(STATUS .NE. 0) THEN
    CALL SENDTEXTMSG2('MEMORY ALLOCATION FAILED IN FUNCTION: ', 'GETSIGNALPHASESMOEDATA', 3)
    GETSIGNALPHASESMOEDATA = 2
  ENDIF
  END                                          
!
! ==========================================================================================================
! Function: GETBUSROUTEMOEDATA( char MOETYPE, int IARRAY, int FLAG, float RMOE )
!
! Description: Returns the value of the specified MOE for the bus route
!
!Modified by LZ 06/24/2024 to remove bug for BUS route on FREeWAY
! Arguments: MOETYPE - [in] name of the MOE
!
!            IARRAY  - [in] array of input parameters
!                           (route number)
!
!            FLAG    - [in] MOE granularity
!                           0 = cumulative
!                           1 = time interval
!                           2 = time period
!
!            RMOE    - [out] value of the MOE
!
! Returns: int = 0 - success
! Returns: int = 1 - MOE name was not recognized
! ======================================================================
  INTEGER RECURSIVE FUNCTION GETBUSROUTEMOEDATA &
  [DLLEXPORT, ALIAS:'GetBusRouteMoeData'](MOETYPE, IARRAY, FLAG, RMOE)
 
  USE SIMPARAMS
  USE TEXT
  USE BUS_ROUTE_DATA
  USE STREET_LINKS
 ! Add by LZ
  USE FREEWAY_LINKS
  IMPLICIT NONE
  CHARACTER*(*) MOETYPE
  INTEGER     IARRAY  [REFERENCE]
  INTEGER     FLAG    [VALUE]
  REAL        RMOE    [REFERENCE]
  DIMENSION IARRAY(*)
 
  REAL SAVED(:,:,:)
  ALLOCATABLE SAVED
  SAVE SAVED
  INTEGER :: STATUS, TEMP, RSAVE, MTIME, MOENUM, MDAT, IBR, INODE, N1, N2, IL
  REAL :: R1, R2, R3
 
  STATUS = 0
  GETBUSROUTEMOEDATA = 0
        
  IF(MOETYPE .EQ. 'DeallocateMemory') THEN
    IF(ALLOCATED(SAVED)) DEALLOCATE(SAVED)
  ELSE
    IF(.NOT. ALLOCATED(SAVED)) THEN
      ALLOCATE(SAVED(15, 500, 8), STAT = STATUS)
      IF(STATUS .NE. 0) GOTO 10
      SAVED = 0.0
    ENDIF
 
    IF(FLAG .EQ. 0) THEN
      MTIME = 1
      MDAT = 2
    ELSEIF(FLAG .EQ. 1) THEN
      MTIME = 3
      MDAT = 4
    ELSEIF(FLAG .EQ. 2) THEN
      MTIME = 6
      MDAT = 7
    ELSE
      GETBUSROUTEMOEDATA = 2
      GOTO 10
    ENDIF
        
    IBR = IARRAY(1)
        
    RMOE = 0.0
! ----------------------------------------------------------------------
    IF(MOETYPE .EQ. 'PersonTravelTime') THEN
        
      MOENUM = 1
      IF(SIMTIME .EQ. SAVED(MOENUM, IBR, MTIME)) THEN
        RMOE = SAVED(MOENUM, IBR, MDAT)
      ELSE
        TEMP = GETBUSROUTEMOEDATA('TravelTimePerBus', IARRAY, FLAG, R1)
        TEMP = GETBUSROUTEMOEDATA('PersonTrips', IARRAY, FLAG, R2)
        RMOE = R1 * R2 / 3600.
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IBR, MDAT) = RMOE
        SAVED(MOENUM, IBR, MDAT+1) = RMOE
        SAVED(MOENUM, IBR, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'PersonTrips') THEN      
      MOENUM = 2
      IF(SIMTIME .EQ. SAVED(MOENUM, IBR, MTIME)) THEN
        RMOE = SAVED(MOENUM, IBR, MDAT)
      ELSE
        RMOE = BUSR_PERSONTRIPS(IBR)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IBR, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IBR, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IBR, MDAT) = RMOE
          SAVED(MOENUM, IBR, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IBR, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimePerBus') THEN      
      MOENUM = 3
      IF(SIMTIME .EQ. SAVED(MOENUM, IBR, MTIME)) THEN
        RMOE = SAVED(MOENUM, IBR, MDAT)
      ELSE
        TEMP = GETBUSROUTEMOEDATA('TravelTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETBUSROUTEMOEDATA('Trips', IARRAY, FLAG, R2)
        IF(R2 .NE. 0) RMOE = R1 / R2 * 60.
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IBR, MDAT) = RMOE
        SAVED(MOENUM, IBR, MDAT+1) = RMOE
        SAVED(MOENUM, IBR, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimeTotal') THEN      
      MOENUM = 4
      IF(SIMTIME .EQ. SAVED(MOENUM, IBR, MTIME)) THEN
        RMOE = SAVED(MOENUM, IBR, MDAT)
      ELSE
        RMOE = BUSR_TRAVELTIME(IBR) / 60.0
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IBR, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IBR, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IBR, MDAT) = RMOE
          SAVED(MOENUM, IBR, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IBR, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'Trips') THEN     
      MOENUM = 5
      IF(SIMTIME .EQ. SAVED(MOENUM, IBR, MTIME)) THEN
        RMOE = SAVED(MOENUM, IBR, MDAT)
      ELSE
        RMOE = BUSR_TRIPS(IBR)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IBR, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IBR, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IBR, MDAT) = RMOE
          SAVED(MOENUM, IBR, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IBR, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DwellTimeTotal') THEN     
      MOENUM = 6
      IF(SIMTIME .EQ. SAVED(MOENUM, IBR, MTIME)) THEN
        RMOE = SAVED(MOENUM, IBR, MDAT)
      ELSE
        RMOE = BUSR_DWELLTIME(IBR) / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IBR, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IBR, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IBR, MDAT) = RMOE
          SAVED(MOENUM, IBR, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IBR, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DwellTimePerBus') THEN      
      MOENUM = 7
      IF(SIMTIME .EQ. SAVED(MOENUM, IBR, MTIME)) THEN
        RMOE = SAVED(MOENUM, IBR, MDAT)
      ELSE
        TEMP = GETBUSROUTEMOEDATA('DwellTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETBUSROUTEMOEDATA('Trips', IARRAY, FLAG, R2)
        IF(R2 .NE. 0) RMOE = R1 / R2
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IBR, MDAT) = RMOE
        SAVED(MOENUM, IBR, MDAT+1) = RMOE
        SAVED(MOENUM, IBR, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'ControlDelayTotal') THEN     
      MOENUM = 8
      IF(SIMTIME .EQ. SAVED(MOENUM, IBR, MTIME)) THEN
        RMOE = SAVED(MOENUM, IBR, MDAT)
      ELSE
        RMOE = BUSR_CONTROLDELAY(IBR) / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IBR, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IBR, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IBR, MDAT) = RMOE
          SAVED(MOENUM, IBR, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IBR, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'ControlDelayPerBus') THEN      
      MOENUM = 9
      IF(SIMTIME .EQ. SAVED(MOENUM, IBR, MTIME)) THEN
        RMOE = SAVED(MOENUM, IBR, MDAT)
      ELSE
        TEMP = GETBUSROUTEMOEDATA('ControlDelayTotal', IARRAY, FLAG, R1)
        TEMP = GETBUSROUTEMOEDATA('Trips', IARRAY, FLAG, R2)
        IF(R2 .NE. 0) RMOE = R1 / R2
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IBR, MDAT) = RMOE
        SAVED(MOENUM, IBR, MDAT+1) = RMOE
        SAVED(MOENUM, IBR, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelDistanceTotal') THEN      
      MOENUM = 10
      IF(SIMTIME .EQ. SAVED(MOENUM, IBR, MTIME)) THEN
        RMOE = SAVED(MOENUM, IBR, MDAT)
      ELSE
        RMOE = BUSR_TRAVELDIST(IBR) / 5280.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IBR, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IBR, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IBR, MDAT) = RMOE
          SAVED(MOENUM, IBR, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IBR, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'AverageSpeed') THEN      
      MOENUM = 11
      IF(SIMTIME .EQ. SAVED(MOENUM, IBR, MTIME)) THEN
        RMOE = SAVED(MOENUM, IBR, MDAT)
      ELSE
        TEMP = GETBUSROUTEMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R1)
        TEMP = GETBUSROUTEMOEDATA('TravelTimeTotal', IARRAY, FLAG, R2)
        IF(R2 .NE. 0) RMOE = R1 / R2 * 60.
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IBR, MDAT) = RMOE
        SAVED(MOENUM, IBR, MDAT+1) = RMOE
        SAVED(MOENUM, IBR, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'AverageTravelSpeed') THEN      
      MOENUM = 12
      IF(SIMTIME .EQ. SAVED(MOENUM, IBR, MTIME)) THEN
        RMOE = SAVED(MOENUM, IBR, MDAT)
      ELSE
        TEMP = GETBUSROUTEMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R1)
        TEMP = GETBUSROUTEMOEDATA('TravelTimeTotal', IARRAY, FLAG, R2)
        TEMP = GETBUSROUTEMOEDATA('DwellTimeTotal', IARRAY, FLAG, R3)
        IF(R2 - R3 .NE. 0) RMOE = R1 / (R2 - R3) * 60.
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IBR, MDAT) = RMOE
        SAVED(MOENUM, IBR, MDAT+1) = RMOE
        SAVED(MOENUM, IBR, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'AverageFreeflowSpeed') THEN      
      MOENUM = 13
      IF(SIMTIME .EQ. SAVED(MOENUM, IBR, MTIME)) THEN
        RMOE = SAVED(MOENUM, IBR, MDAT)
      ELSE
        R1 = 0.
        R2 = 0.
        DO INODE = 1, BUSR_NNODES(IBR) - 1
          N1 = BUSR_ROUTE_NODES(IBR, INODE)
          N2 = BUSR_ROUTE_NODES(IBR, INODE+1)
          CALL FIND_STREET_LINK(N1, N2, IL)
    ! Added by LZ to including freeway
          IF (IL .GT. 0) THEN 
            R1 = R1 + (SFREEFLOWSPEED(IL) * SLENGTH(IL))
            R2 = R2 + SLENGTH(IL)
          ELSE
            IF (IL .EQ. 0) THEN
                CALL FIND_FREEWAY_LINK(N1, N2, IL)
                IF (IL .GT. 0) THEN

                        R1 = R1 + (FFREEFLOWSPEED(IL) * FLENGTH(IL))
                       R2 = R2 + FLENGTH(IL)
                ENDIF
             ENDIF
          ENDIF
                                  
        ENDDO
        IF(R2 .NE. 0) RMOE = (R1 / R2) * (3600. / 5280.)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IBR, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IBR, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IBR, MDAT) = RMOE
          SAVED(MOENUM, IBR, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IBR, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'FreeflowSpeedTravelTime') THEN      
      MOENUM = 14
      IF(SIMTIME .EQ. SAVED(MOENUM, IBR, MTIME)) THEN
        RMOE = SAVED(MOENUM, IBR, MDAT)
      ELSE
        TEMP = GETBUSROUTEMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R1)
        TEMP = GETBUSROUTEMOEDATA('AverageFreeflowSpeed', IARRAY, FLAG, R2)
        IF(R2 .NE. 0) RMOE = R1 / R2 * 60.  
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IBR, MDAT) = RMOE
        SAVED(MOENUM, IBR, MDAT+1) = RMOE
        SAVED(MOENUM, IBR, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayTime') THEN      
      MOENUM = 15
      IF(SIMTIME .EQ. SAVED(MOENUM, IBR, MTIME)) THEN
        RMOE = SAVED(MOENUM, IBR, MDAT)
      ELSE
        TEMP = GETBUSROUTEMOEDATA('TravelTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETBUSROUTEMOEDATA('FreeflowSpeedTravelTime', IARRAY, FLAG, R2)
        IF(R1 .GT. R2) RMOE = R1 - R2
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IBR, MDAT) = RMOE
        SAVED(MOENUM, IBR, MDAT+1) = RMOE
        SAVED(MOENUM, IBR, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSE
      GETBUSROUTEMOEDATA = 1
      WRITE(MSGTEXT, '(A)') 'GETBUSROUTEMOEDATA'
      CALL SENDTEXTMSG(3)
      WRITE(MSGTEXT, '(A,A)') 'FAILED TO RECOGNIZE CLASSNAME ', MOETYPE
      CALL SENDTEXTMSG(3)
    ENDIF
  ENDIF
10 IF(STATUS .NE. 0) THEN
    CALL SENDTEXTMSG2('MEMORY ALLOCATION FAILED IN FUNCTION: ', 'GETBUSROUTEMOEDATA', 3)
    GETBUSROUTEMOEDATA = 2
  ENDIF
  END                                          
!
! ==========================================================================================================
! Function: GETBUSSTATIONMOEDATA( char MOETYPE, int IARRAY, int FLAG, float RMOE )
!
! Description: Returns the value of the specified MOE for the bus station
!
!
! Arguments: MOETYPE - [in] name of the MOE
!
!            IARRAY  - [in] array of input parameters
!                           (station number)
!
!            FLAG    - [in] MOE granularity
!                           0 = cumulative
!                           1 = time interval
!                           2 = time period
!
!            RMOE    - [out] value of the MOE
!
! Returns: int = 0 - success
! Returns: int = 1 - MOE name was not recognized
! ======================================================================
  INTEGER RECURSIVE FUNCTION GETBUSSTATIONMOEDATA &
  [DLLEXPORT,ALIAS:'GetBusStationMoeData'](MOETYPE, IARRAY, FLAG, RMOE)
 
  USE SIMPARAMS
  USE TEXT
  USE BUS_STATION_DATA
  IMPLICIT NONE
  CHARACTER*(*) MOETYPE
  INTEGER     IARRAY  [REFERENCE]
  INTEGER     FLAG    [VALUE]
  REAL        RMOE    [REFERENCE]
  DIMENSION IARRAY(*)
 
  REAL SAVED(:,:,:)
  ALLOCATABLE SAVED
  SAVE SAVED
  INTEGER :: STATUS, TEMP, RSAVE, MTIME, MOENUM, MDAT, ISTAT
  REAL :: R1, R2, R3
 
  STATUS = 0
  GETBUSSTATIONMOEDATA = 0
  IF(MOETYPE .EQ. 'DeallocateMemory') THEN
    IF(ALLOCATED(SAVED)) DEALLOCATE(SAVED)
  ELSE
    IF(.NOT. ALLOCATED(SAVED)) THEN
      ALLOCATE(SAVED(8, 99, 8), STAT = STATUS)
      IF(STATUS .NE. 0) GOTO 10
      SAVED = 0.0
    ENDIF
 
    IF(FLAG .EQ. 0) THEN
      MTIME = 1
      MDAT = 2
    ELSEIF(FLAG .EQ. 1) THEN
      MTIME = 3
      MDAT = 4
    ELSEIF(FLAG .EQ. 2) THEN
      MTIME = 6
      MDAT = 7
    ELSE
      GETBUSSTATIONMOEDATA = 2
      GOTO 10
    ENDIF
        
    ISTAT = IARRAY(1)
        
    GETBUSSTATIONMOEDATA = 0
        
    RMOE = 0.0
! ----------------------------------------------------------------------
    IF(MOETYPE .EQ. 'BusesServicedTotal') THEN     
      MOENUM = 1
      IF(SIMTIME .EQ. SAVED(MOENUM, ISTAT, MTIME)) THEN
        RMOE = SAVED(MOENUM, ISTAT, MDAT)
      ELSE
        RMOE = BUS_STATION_LIST(ISTAT)%COUNT
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, ISTAT, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, ISTAT, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, ISTAT, MDAT) = RMOE
          SAVED(MOENUM, ISTAT, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, ISTAT, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'CapacityExceededTimeTotal') THEN       
      MOENUM = 2
      IF(SIMTIME .EQ. SAVED(MOENUM, ISTAT, MTIME)) THEN
        RMOE = SAVED(MOENUM, ISTAT, MDAT)
      ELSE
        RMOE = BUS_STATION_LIST(ISTAT)%OVERFLOW_TIME / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, ISTAT, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, ISTAT, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, ISTAT, MDAT) = RMOE
          SAVED(MOENUM, ISTAT, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, ISTAT, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DwellTimeTotal') THEN       
      MOENUM = 3
      IF(SIMTIME .EQ. SAVED(MOENUM, ISTAT, MTIME)) THEN
        RMOE = SAVED(MOENUM, ISTAT, MDAT)
      ELSE
        RMOE = BUS_STATION_LIST(ISTAT)%DWELL_TIME / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, ISTAT, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, ISTAT, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, ISTAT, MDAT) = RMOE
          SAVED(MOENUM, ISTAT, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, ISTAT, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'EmptyTimeTotal') THEN
      MOENUM = 4
      IF(SIMTIME .EQ. SAVED(MOENUM, ISTAT, MTIME)) THEN
        RMOE = SAVED(MOENUM, ISTAT, MDAT)
      ELSE
        RMOE = BUS_STATION_LIST(ISTAT)%EMPTY_TIME / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, ISTAT, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, ISTAT, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, ISTAT, MDAT) = RMOE
          SAVED(MOENUM, ISTAT, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, ISTAT, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TotalTravelTime') THEN
      MOENUM = 5
      IF(SIMTIME .EQ. SAVED(MOENUM, ISTAT, MTIME)) THEN
        RMOE = SAVED(MOENUM, ISTAT, MDAT)
      ELSE
        RMOE = BUS_STATION_LIST(ISTAT)%TIME_FROM_LAST / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, ISTAT, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, ISTAT, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, ISTAT, MDAT) = RMOE
          SAVED(MOENUM, ISTAT, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, ISTAT, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'AverageTravelTime') THEN      
      MOENUM = 6
      IF(SIMTIME .EQ. SAVED(MOENUM, ISTAT, MTIME)) THEN
        RMOE = SAVED(MOENUM, ISTAT, MDAT)
      ELSE
        TEMP = GETBUSSTATIONMOEDATA('TotalTravelTime', IARRAY, FLAG, R1)
        TEMP = GETBUSSTATIONMOEDATA('BusesServicedTotal', IARRAY, FLAG, R2)
        IF(R2 .NE. 0) RMOE = R1 / R2
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, ISTAT, MDAT) = RMOE
        SAVED(MOENUM, ISTAT, MDAT+1) = RMOE
        SAVED(MOENUM, ISTAT, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'Distance') THEN
      MOENUM = 7
      IF(SIMTIME .EQ. SAVED(MOENUM, ISTAT, MTIME)) THEN
        RMOE = SAVED(MOENUM, ISTAT, MDAT)
      ELSE
        RMOE = BUS_STATION_LIST(ISTAT)%APPROACH_DISTANCE
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, ISTAT, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, ISTAT, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, ISTAT, MDAT) = RMOE
          SAVED(MOENUM, ISTAT, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, ISTAT, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'Speed') THEN      
      MOENUM = 8
      IF(SIMTIME .EQ. SAVED(MOENUM, ISTAT, MTIME)) THEN
        RMOE = SAVED(MOENUM, ISTAT, MDAT)
      ELSE
        TEMP = GETBUSSTATIONMOEDATA('BusesServicedTotal', IARRAY, FLAG, R1)
        TEMP = GETBUSSTATIONMOEDATA('Distance', IARRAY, FLAG, R2)
        TEMP = GETBUSSTATIONMOEDATA('TotalTravelTime', IARRAY, FLAG, R3)
        IF(R3 .NE. 0) RMOE = R1 * R2 / R3 / 60.
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, ISTAT, MDAT) = RMOE
        SAVED(MOENUM, ISTAT, MDAT+1) = RMOE
        SAVED(MOENUM, ISTAT, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSE
      GETBUSSTATIONMOEDATA = 1
      WRITE(MSGTEXT, '(A)') 'GETBUSSTATIONMOEDATA'
      CALL SENDTEXTMSG(3)
      WRITE(MSGTEXT, '(A,A)') 'FAILED TO RECOGNIZE CLASSNAME ', MOETYPE
      CALL SENDTEXTMSG(3)
    ENDIF
  ENDIF
10 IF(STATUS .NE. 0) THEN
    CALL SENDTEXTMSG2('MEMORY ALLOCATION FAILED IN FUNCTION: ', 'GETBUSSTATIONMOEDATA', 3)
    GETBUSSTATIONMOEDATA = 2
  ENDIF
  END
!
! ==========================================================================================================
! Function: GETDATASTATIONMOEDATA( char MOETYPE, int IARRAY, int FLAG, float RMOE )
!
! Description: Returns the value of the specified MOE for the detector
!              on the link specified by up node and down node
!
!
! Arguments: MOETYPE - [in] name of the MOE
!
!            IARRAY  - [in] array of input parameters
!                           (up node, down node, detector number)
!
!            FLAG    - [in] MOE granularity
!                           0 = cumulative
!                           1 = time interval
!                           2 = time period
!
!            RMOE    - [out] value of the MOE
!
! Returns: int = 0 - success
! Returns: int = 1 - MOE name was not recognized or link was not found
! ======================================================================
  INTEGER RECURSIVE FUNCTION GETDATASTATIONMOEDATA &
  [DLLEXPORT, ALIAS:'GetDatastationMoeData'](MOETYPE, IARRAY, FLAG, RMOE)
 
  USE FREEWAY_LINKS
  USE DATASTATIONS
  USE TEXT
  USE SIMPARAMS
  IMPLICIT NONE
  CHARACTER*(*) MOETYPE
  INTEGER     IARRAY  [REFERENCE]
  INTEGER     FLAG    [VALUE]
  REAL        RMOE    [REFERENCE]
  DIMENSION IARRAY(*)
 
  REAL SAVED(:,:,:,:)
  ALLOCATABLE SAVED
  SAVE SAVED
  INTEGER :: STATUS, MTIME, MDAT, MOENUM, TEMP
  INTEGER :: IDW, IUP, IS, IL, ILN
  REAL :: R1, R2, RSAVE
 
  STATUS = 0
  GETDATASTATIONMOEDATA = 0
  IF(MOETYPE .EQ. 'DeallocateMemory') THEN
    IF(ALLOCATED(SAVED)) DEALLOCATE(SAVED)
  ELSE
    IF(.NOT. ALLOCATED(SAVED)) THEN
      ALLOCATE(SAVED(6, NUMBER_OF_DATASTATIONS, N_FREEWAY_LANES, 8), STAT = STATUS)
      IF(STATUS .NE. 0) GOTO 10
      SAVED = 0.0
    ENDIF
   
    IF(FLAG .EQ. 0) THEN
      MTIME = 1
      MDAT = 2
    ELSEIF(FLAG .EQ. 1) THEN
      MTIME = 3
      MDAT = 4
    ELSEIF(FLAG .EQ. 2) THEN
      MTIME = 6
      MDAT = 7
    ELSE
      GETDATASTATIONMOEDATA = 2
      GOTO 10
    ENDIF
   
    IUP = IARRAY(1)
    IDW = IARRAY(2)
    ILN = IARRAY(3)
   
    RMOE = 0.0
   
    CALL FIND_FREEWAY_LINK(IUP, IDW, IL)
   
    GETDATASTATIONMOEDATA = 0
   
    IF(IL .EQ. 0) THEN
      GETDATASTATIONMOEDATA = 1
    ELSE
! ----------------------------------------------------------------------
      IS = DATASTATION_ID(IL)
      IF(MOETYPE .EQ. 'HeadwayAverage') THEN
   
        MOENUM = 1
        IF(SIMTIME .EQ. SAVED(MOENUM, IS, ILN, MTIME)) THEN
          RMOE = SAVED(MOENUM, IS, ILN, MDAT)
        ELSE
          TEMP = GETDATASTATIONMOEDATA('HeadwaySum', IARRAY, FLAG, R1)
          TEMP = GETDATASTATIONMOEDATA('HeadwayCount', IARRAY, FLAG, R2)
          IF(R2 .NE. 0) RMOE = R1 / R2
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IS, ILN, MDAT) = RMOE
          SAVED(MOENUM, IS, ILN, MDAT+1) = RMOE
          SAVED(MOENUM, IS, ILN, MTIME) = SIMTIME
        ENDIF
! ----------------------------------------------------------------------
      ELSEIF(MOETYPE .EQ. 'HeadwayCount') THEN
   
        MOENUM = 2
        IF(SIMTIME .EQ. SAVED(MOENUM, IS, ILN, MTIME)) THEN
          RMOE = SAVED(MOENUM, IS, ILN, MDAT)
        ELSE
          RMOE = DATASTATION(IS)%HDWY_COUNT(ILN)
          IF(FLAG .EQ. 0) THEN
            SAVED(MOENUM, IS, ILN, MDAT) = RMOE
          ELSE
            RSAVE = RMOE
            RMOE = RMOE - SAVED(MOENUM, IS, ILN, MDAT+1)
            IF((RMOE) .LT. 1E-5) RMOE = 0.0
            SAVED(MOENUM, IS, ILN, MDAT) = RMOE
            SAVED(MOENUM, IS, ILN, MDAT+1) = RSAVE
          ENDIF
          SAVED(MOENUM, IS, ILN, MTIME) = SIMTIME
        ENDIF
! ----------------------------------------------------------------------
      ELSEIF(MOETYPE .EQ. 'HeadwaySum') THEN
   
        MOENUM = 3
        IF(SIMTIME .EQ. SAVED(MOENUM, IS, ILN, MTIME)) THEN
          RMOE = SAVED(MOENUM, IS, ILN, MDAT)
        ELSE
          RMOE = DATASTATION(IS)%HDWY_TOTAL(ILN)
          IF(FLAG .EQ. 0) THEN
            SAVED(MOENUM, IS, ILN, MDAT) = RMOE
          ELSE
            RSAVE = RMOE
            RMOE = RMOE - SAVED(MOENUM, IS, ILN, MDAT+1)
            IF((RMOE) .LT. 1E-5) RMOE = 0.0
            SAVED(MOENUM, IS, ILN, MDAT) = RMOE
            SAVED(MOENUM, IS, ILN, MDAT+1) = RSAVE
          ENDIF
          SAVED(MOENUM, IS, ILN, MTIME) = SIMTIME
        ENDIF
! ----------------------------------------------------------------------
      ELSEIF(MOETYPE .EQ. 'SpeedAverage') THEN
   
        MOENUM = 4
        IF(SIMTIME .EQ. SAVED(MOENUM, IS, ILN, MTIME)) THEN
          RMOE = SAVED(MOENUM, IS, ILN, MDAT)
        ELSE
          TEMP = GETDATASTATIONMOEDATA('SpeedSum', IARRAY, FLAG, R1)
          TEMP = GETDATASTATIONMOEDATA('SpeedCount', IARRAY, FLAG, R2)
          IF(R2 .NE. 0) RMOE = R1 / R2
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IS, ILN, MDAT) = RMOE
          SAVED(MOENUM, IS, ILN, MDAT+1) = RMOE
          SAVED(MOENUM, IS, ILN, MTIME) = SIMTIME
        ENDIF
! ----------------------------------------------------------------------
      ELSEIF(MOETYPE .EQ. 'SpeedCount') THEN
    
        MOENUM = 5
        IF(SIMTIME .EQ. SAVED(MOENUM, IS, ILN, MTIME)) THEN
          RMOE = SAVED(MOENUM, IS, ILN, MDAT)
        ELSE
          RMOE = DATASTATION(IS)%COUNT(ILN)
          IF(FLAG .EQ. 0) THEN
            SAVED(MOENUM, IS, ILN, MDAT) = RMOE
          ELSE
            RSAVE = RMOE
            RMOE = RMOE - SAVED(MOENUM, IS, ILN, MDAT+1)
            IF((RMOE) .LT. 1E-5) RMOE = 0.0
            SAVED(MOENUM, IS, ILN, MDAT) = RMOE
            SAVED(MOENUM, IS, ILN, MDAT+1) = RSAVE
          ENDIF
          SAVED(MOENUM, IS, ILN, MTIME) = SIMTIME
        ENDIF
! ----------------------------------------------------------------------
      ELSEIF(MOETYPE .EQ. 'SpeedSum') THEN
   
        MOENUM = 6
        IF(SIMTIME .EQ. SAVED(MOENUM, IS, ILN, MTIME)) THEN
          RMOE = SAVED(MOENUM, IS, ILN, MDAT)
        ELSE
          RMOE = DATASTATION(IS)%SPEED_TOTAL(ILN)
          IF(FLAG .EQ. 0) THEN
            SAVED(MOENUM, IS, ILN, MDAT) = RMOE
          ELSE
            RSAVE = RMOE
            RMOE = RMOE - SAVED(MOENUM, IS, ILN, MDAT+1)
            IF((RMOE) .LT. 1E-5) RMOE = 0.0
            SAVED(MOENUM, IS, ILN, MDAT) = RMOE
            SAVED(MOENUM, IS, ILN, MDAT+1) = RSAVE
          ENDIF
          SAVED(MOENUM, IS, ILN, MTIME) = SIMTIME
        ENDIF
! ----------------------------------------------------------------------
      ELSE
        GETDATASTATIONMOEDATA = 1
        msgtext = moetype            
        call sendtextmsg(3)
      ENDIF
    ENDIF
  ENDIF
10 IF(STATUS .NE. 0) THEN
    CALL SENDTEXTMSG2('MEMORY ALLOCATION FAILED IN FUNCTION: ', 'GETDATASTATIONMOEDATA', 3)
    GETDATASTATIONMOEDATA = 2
  ENDIF
  END
!
! ==========================================================================================================
! Function: GETFREEWAYDETECTORMOEDATA( char MOETYPE, int IARRAY, int FLAG, float RMOE )
!
! Description: Returns the value of the specified MOE for the detector
!              on the link specified by up node and down node
!
!
! Arguments: MOETYPE - [in] name of the MOE
!
!            IARRAY  - [in] array of input parameters
!                           (up node, down node, detector number)
!
!            FLAG    - [in] MOE granularity
!                           0 = cumulative
!                           1 = time interval
!                           2 = time period
!
!            RMOE    - [out] value of the MOE
!
! Returns: int = 0 - success
! Returns: int = 1 - MOE name was not recognized or link was not found
! ======================================================================
  INTEGER RECURSIVE FUNCTION GETFREEWAYDETECTORMOEDATA &
  [DLLEXPORT,ALIAS:'GetFreewayDetectorMoeData'](MOETYPE, IARRAY, FLAG, RMOE)
 
  USE FREEWAY_LINKS
  USE FREEWAY_DETECTORS
  USE TEXT
  USE SIMPARAMS
  USE GLOBAL_DATA
  IMPLICIT NONE
  CHARACTER*(*) MOETYPE
  INTEGER     IARRAY  [REFERENCE]
  INTEGER     FLAG    [VALUE]
  REAL        RMOE    [REFERENCE]
  DIMENSION IARRAY(*)
 
  REAL SAVED(:,:,:)
  ALLOCATABLE SAVED
  SAVE SAVED
  INTEGER :: STATUS, MTIME, MDAT, MOENUM, TEMP, GETBASICMOEDATA
  INTEGER :: IDET, IDW, IUP, IL
  REAL :: R1, R2, RSAVE
 
  STATUS = 0
  GETFREEWAYDETECTORMOEDATA = 0
  IF(MOETYPE .EQ. 'DeallocateMemory') THEN
    IF(ALLOCATED(SAVED)) DEALLOCATE(SAVED)
  ELSE
    IF(.NOT. ALLOCATED(SAVED)) THEN
      ALLOCATE(SAVED(9, N_FREEWAY_DETECTORS, 8), STAT = STATUS)
      IF(STATUS .NE. 0) GOTO 10
      SAVED = 0.0
    ENDIF
   
    IF(FLAG .EQ. 0) THEN
      MTIME = 1
      MDAT = 2
    ELSEIF(FLAG .EQ. 1) THEN
      MTIME = 3
      MDAT = 4
    ELSEIF(FLAG .EQ. 2) THEN
      MTIME = 6
      MDAT = 7
    ELSE
      GETFREEWAYDETECTORMOEDATA = 2
      GOTO 10
    ENDIF
   
    IUP = IARRAY(1)
    IDW = IARRAY(2)
    IDET = IARRAY(3)
   
    RMOE = 0.0
   
    CALL FIND_FREEWAY_LINK(IUP, IDW, IL)
   
    GETFREEWAYDETECTORMOEDATA = 0
   
    IF(IL .EQ. 0) THEN
      GETFREEWAYDETECTORMOEDATA = 1
    ELSE
! ----------------------------------------------------------------------
      IF(MOETYPE .EQ. 'Count') THEN
       
        MOENUM = 1
        IF(SIMTIME .EQ. SAVED(MOENUM, IDET, MTIME)) THEN
          RMOE = SAVED(MOENUM, IDET, MDAT)
        ELSE
          RMOE = FDETECTOR(IDET)%COUNT
          IF(FLAG .EQ. 0) THEN
            SAVED(MOENUM, IDET, MDAT) = RMOE
          ELSE
            RSAVE = RMOE
            RMOE = RMOE - SAVED(MOENUM, IDET, MDAT+1)
            IF((RMOE) .LT. 1E-5) RMOE = 0.0
            SAVED(MOENUM, IDET, MDAT) = RMOE
            SAVED(MOENUM, IDET, MDAT+1) = RSAVE
          ENDIF
          SAVED(MOENUM, IDET, MTIME) = SIMTIME              
        ENDIF
! ----------------------------------------------------------------------
      ELSEIF(MOETYPE .EQ. 'HeadwayAverage') THEN
        
        MOENUM = 2
        IF(SIMTIME .EQ. SAVED(MOENUM, IDET, MTIME)) THEN
          RMOE = SAVED(MOENUM, IDET, MDAT)
        ELSE
          TEMP = GETFREEWAYDETECTORMOEDATA('HeadwayTotal', IARRAY, FLAG, R1)
          TEMP = GETFREEWAYDETECTORMOEDATA('HeadwayCount', IARRAY, FLAG, R2)
          IF(R2 .NE. 0) RMOE = R1 / R2
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IDET, MDAT) = RMOE
          SAVED(MOENUM, IDET, MDAT+1) = RMOE
          SAVED(MOENUM, IDET, MTIME) = SIMTIME
        ENDIF
! ----------------------------------------------------------------------
      ELSEIF(MOETYPE .EQ. 'HeadwayCount') THEN
        
        MOENUM = 3
        IF(SIMTIME .EQ. SAVED(MOENUM, IDET, MTIME)) THEN
          RMOE = SAVED(MOENUM, IDET, MDAT)
        ELSE
          RMOE = FDETECTOR(IDET)%HDWY_COUNT
          IF(FLAG .EQ. 0) THEN
            SAVED(MOENUM, IDET, MDAT) = RMOE
          ELSE
            RSAVE = RMOE
            RMOE = RMOE - SAVED(MOENUM, IDET, MDAT+1)
            IF((RMOE) .LT. 1E-5) RMOE = 0.0
            SAVED(MOENUM, IDET, MDAT) = RMOE
            SAVED(MOENUM, IDET, MDAT+1) = RSAVE
          ENDIF
          SAVED(MOENUM, IDET, MTIME) = SIMTIME              
        ENDIF
! ----------------------------------------------------------------------
      ELSEIF(MOETYPE .EQ. 'HeadwayTotal') THEN
        
        MOENUM = 4
        IF(SIMTIME .EQ. SAVED(MOENUM, IDET, MTIME)) THEN
          RMOE = SAVED(MOENUM, IDET, MDAT)
        ELSE
          RMOE = FDETECTOR(IDET)%HDWY_TOTAL
          IF(FLAG .EQ. 0) THEN
            SAVED(MOENUM, IDET, MDAT) = RMOE
          ELSE
            RSAVE = RMOE
            RMOE = RMOE - SAVED(MOENUM, IDET, MDAT+1)
            IF((RMOE) .LT. 1E-5) RMOE = 0.0
            SAVED(MOENUM, IDET, MDAT) = RMOE
            SAVED(MOENUM, IDET, MDAT+1) = RSAVE
          ENDIF
          SAVED(MOENUM, IDET, MTIME) = SIMTIME              
        ENDIF
! ----------------------------------------------------------------------
      ELSEIF(MOETYPE .EQ. 'OccupancyPercent') THEN
        
        MOENUM = 5
        IF(SIMTIME .EQ. SAVED(MOENUM, IDET, MTIME)) THEN
          RMOE = SAVED(MOENUM, IDET, MDAT)
        ELSE
          TEMP = GETFREEWAYDETECTORMOEDATA('OnTime', IARRAY, FLAG, R1)
          TEMP = GETBASICMOEDATA('SimulationTime', FLAG, R2)
          RMOE = 100 * R1 / R2 
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IDET, MDAT) = RMOE
          SAVED(MOENUM, IDET, MDAT+1) = RMOE
          SAVED(MOENUM, IDET, MTIME) = SIMTIME
        ENDIF
! ----------------------------------------------------------------------
      ELSEIF(MOETYPE .EQ. 'OnTime') THEN
        
        MOENUM = 6
        IF(SIMTIME .EQ. SAVED(MOENUM, IDET, MTIME)) THEN
          RMOE = SAVED(MOENUM, IDET, MDAT)
        ELSE
          RMOE = FDETECTOR(IDET)%ON_TIME
          IF(FLAG .EQ. 0) THEN
            SAVED(MOENUM, IDET, MDAT) = RMOE
          ELSE
            RSAVE = RMOE
            RMOE = RMOE - SAVED(MOENUM, IDET, MDAT+1)
            IF((RMOE) .LT. 1E-5) RMOE = 0.0
            SAVED(MOENUM, IDET, MDAT) = RMOE
            SAVED(MOENUM, IDET, MDAT+1) = RSAVE
          ENDIF
          SAVED(MOENUM, IDET, MTIME) = SIMTIME              
        ENDIF
! ----------------------------------------------------------------------
      ELSEIF(MOETYPE .EQ. 'SpeedAverage') THEN
        
        MOENUM = 7
        IF(SIMTIME .EQ. SAVED(MOENUM, IDET, MTIME)) THEN
          RMOE = SAVED(MOENUM, IDET, MDAT)
        ELSE
          TEMP = GETFREEWAYDETECTORMOEDATA('SpeedTotal', IARRAY, FLAG, R1)
          TEMP = GETFREEWAYDETECTORMOEDATA('Count', IARRAY, FLAG, R2)
          IF(R2 .NE. 0) RMOE = (R1 / R2) * FEET2MILES
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IDET, MDAT) = RMOE
          SAVED(MOENUM, IDET, MDAT+1) = RMOE
          SAVED(MOENUM, IDET, MTIME) = SIMTIME
        ENDIF
! ----------------------------------------------------------------------
      ELSEIF(MOETYPE .EQ. 'SpeedTotal') THEN
       
        MOENUM = 8
        IF(SIMTIME .EQ. SAVED(MOENUM, IDET, MTIME)) THEN
          RMOE = SAVED(MOENUM, IDET, MDAT)
        ELSE
          RMOE = FDETECTOR(IDET)%SPEED_TOTAL
          IF(FLAG .EQ. 0) THEN
            SAVED(MOENUM, IDET, MDAT) = RMOE
          ELSE
            RSAVE = RMOE
            RMOE = RMOE - SAVED(MOENUM, IDET, MDAT+1)
            IF((RMOE) .LT. 1E-5) RMOE = 0.0
            SAVED(MOENUM, IDET, MDAT) = RMOE
            SAVED(MOENUM, IDET, MDAT+1) = RSAVE
          ENDIF
          SAVED(MOENUM, IDET, MTIME) = SIMTIME              
        ENDIF
! ----------------------------------------------------------------------
      ELSEIF(MOETYPE .EQ. 'Volume') THEN
 
        MOENUM = 9
        IF(SIMTIME .EQ. SAVED(MOENUM, IDET, MTIME)) THEN
          RMOE = SAVED(MOENUM, IDET, MDAT)
        ELSE
          TEMP = GETFREEWAYDETECTORMOEDATA('Count', IARRAY, FLAG, R1)
          TEMP = GETBASICMOEDATA('SimulationTime', FLAG, R2)
          RMOE = 3600. * R1 / R2
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IDET, MDAT) = RMOE
          SAVED(MOENUM, IDET, MDAT+1) = RMOE
          SAVED(MOENUM, IDET, MTIME) = SIMTIME
        ENDIF
! ----------------------------------------------------------------------
      ELSE
        GETFREEWAYDETECTORMOEDATA = 1
        msgtext = moetype            
        call sendtextmsg(3)
      ENDIF
    ENDIF
  ENDIF
10 IF(STATUS .NE. 0) THEN
    CALL SENDTEXTMSG2('MEMORY ALLOCATION FAILED IN FUNCTION: ', 'GETFREEWAYDETECTORMOEDATA', 3)
    GETFREEWAYDETECTORMOEDATA = 2
  ENDIF
  END
!
! ==========================================================================================================
! Function: GETSTREETDETECTORMOEDATA( char MOETYPE, int IARRAY, int FLAG, float RMOE )
!
! Description: Returns the value of the specified MOE for the detector
!              on the link specified by up node and down node
!
!
! Arguments: MOETYPE - [in] name of the MOE
!
!            IARRAY  - [in] array of input parameters
!                           (up node, down node, detector number)
!
!            FLAG    - [in] MOE granularity
!                           0 = cumulative
!                           1 = time interval
!                           2 = time period
!
!            RMOE    - [out] value of the MOE
!
! Returns: int = 0 - success
! Returns: int = 1 - MOE name was not recognized or link was not found
! ======================================================================
  INTEGER RECURSIVE FUNCTION GETSTREETDETECTORMOEDATA &
  [DLLEXPORT,ALIAS:'GetStreetDetectorMoeData'](MOETYPE, IARRAY, FLAG, RMOE)
 
  USE STREET_LINKS
  USE STREET_DETECTORS
  USE TEXT
  USE SIMPARAMS
  IMPLICIT NONE
  CHARACTER*(*) MOETYPE
  INTEGER     IARRAY  [REFERENCE]
  INTEGER     FLAG    [VALUE]
  REAL        RMOE    [REFERENCE]
  DIMENSION IARRAY(*)
 
  REAL SAVED(:,:,:)
  ALLOCATABLE SAVED
  SAVE SAVED
  INTEGER :: STATUS, MTIME, MDAT, MOENUM, TEMP, GETBASICMOEDATA
  INTEGER :: IDET, IDW, IUP, IL
  REAL :: R1, R2, RSAVE
 
  STATUS = 0
  GETSTREETDETECTORMOEDATA = 0
  IF(MOETYPE .EQ. 'DeallocateMemory') THEN
    IF(ALLOCATED(SAVED)) DEALLOCATE(SAVED)
  ELSE
    IF(.NOT. ALLOCATED(SAVED)) THEN
      ALLOCATE(SAVED(11, N_STREET_DETECTORS, 8), STAT = STATUS)
      IF(STATUS .NE. 0) GOTO 10
      SAVED = 0.0
    ENDIF
   
    IF(FLAG .EQ. 0) THEN
      MTIME = 1
      MDAT = 2
    ELSEIF(FLAG .EQ. 1) THEN
      MTIME = 3
      MDAT = 4
    ELSEIF(FLAG .EQ. 2) THEN
      MTIME = 6
      MDAT = 7
    ELSE
      GETSTREETDETECTORMOEDATA = 2
      GOTO 10
    ENDIF
   
    IUP = IARRAY(1)
    IDW = IARRAY(2)
    IDET = IARRAY(3)
   
    RMOE = 0.0
   
    CALL FIND_STREET_LINK(IUP, IDW, IL)
   
    GETSTREETDETECTORMOEDATA = 0
   
    IF(IL .EQ. 0) THEN
      GETSTREETDETECTORMOEDATA = 1
    ELSE
! ----------------------------------------------------------------------
      IF(MOETYPE .EQ. 'Count') THEN
       
        MOENUM = 1
        IF(SIMTIME .EQ. SAVED(MOENUM, IDET, MTIME)) THEN
          RMOE = SAVED(MOENUM, IDET, MDAT)
        ELSE
          RMOE = SDETECTOR(IDET)%COUNT
          IF(FLAG .EQ. 0) THEN
            SAVED(MOENUM, IDET, MDAT) = RMOE
          ELSE
            RSAVE = RMOE
            RMOE = RMOE - SAVED(MOENUM, IDET, MDAT+1)
            IF((RMOE) .LT. 1E-5) RMOE = 0.0
            SAVED(MOENUM, IDET, MDAT) = RMOE
            SAVED(MOENUM, IDET, MDAT+1) = RSAVE
          ENDIF
          SAVED(MOENUM, IDET, MTIME) = SIMTIME              
        ENDIF
! ----------------------------------------------------------------------
      ELSEIF(MOETYPE .EQ. 'HeadwayAverage') THEN
        
        MOENUM = 2
        IF(SIMTIME .EQ. SAVED(MOENUM, IDET, MTIME)) THEN
          RMOE = SAVED(MOENUM, IDET, MDAT)
        ELSE
          TEMP = GETSTREETDETECTORMOEDATA('HeadwayTotal', IARRAY, FLAG, R1)
          TEMP = GETSTREETDETECTORMOEDATA('HeadwayCount', IARRAY, FLAG, R2)
          IF(R2 .NE. 0) RMOE = R1 / R2
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IDET, MDAT) = RMOE
          SAVED(MOENUM, IDET, MDAT+1) = RMOE
          SAVED(MOENUM, IDET, MTIME) = SIMTIME
        ENDIF
! ----------------------------------------------------------------------
      ELSEIF(MOETYPE .EQ. 'HeadwayCount') THEN
        
        MOENUM = 3
        IF(SIMTIME .EQ. SAVED(MOENUM, IDET, MTIME)) THEN
          RMOE = SAVED(MOENUM, IDET, MDAT)
        ELSE
          RMOE = SDETECTOR(IDET)%HDWY_COUNT
          IF(FLAG .EQ. 0) THEN
            SAVED(MOENUM, IDET, MDAT) = RMOE
          ELSE
            RSAVE = RMOE
            RMOE = RMOE - SAVED(MOENUM, IDET, MDAT+1)
            IF((RMOE) .LT. 1E-5) RMOE = 0.0
            SAVED(MOENUM, IDET, MDAT) = RMOE
            SAVED(MOENUM, IDET, MDAT+1) = RSAVE
          ENDIF
          SAVED(MOENUM, IDET, MTIME) = SIMTIME              
        ENDIF
! ----------------------------------------------------------------------
      ELSEIF(MOETYPE .EQ. 'HeadwayTotal') THEN
        
        MOENUM = 4
        IF(SIMTIME .EQ. SAVED(MOENUM, IDET, MTIME)) THEN
          RMOE = SAVED(MOENUM, IDET, MDAT)
        ELSE
          RMOE = SDETECTOR(IDET)%HDWY_TOTAL
          IF(FLAG .EQ. 0) THEN
            SAVED(MOENUM, IDET, MDAT) = RMOE
          ELSE
            RSAVE = RMOE
            RMOE = RMOE - SAVED(MOENUM, IDET, MDAT+1)
            IF((RMOE) .LT. 1E-5) RMOE = 0.0
            SAVED(MOENUM, IDET, MDAT) = RMOE
            SAVED(MOENUM, IDET, MDAT+1) = RSAVE
          ENDIF
          SAVED(MOENUM, IDET, MTIME) = SIMTIME              
        ENDIF
! ----------------------------------------------------------------------
      ELSEIF(MOETYPE .EQ. 'OccupancyPercent') THEN
        
        MOENUM = 5
        IF(SIMTIME .EQ. SAVED(MOENUM, IDET, MTIME)) THEN
          RMOE = SAVED(MOENUM, IDET, MDAT)
        ELSEIF(SDETECTOR(IDET)%LANES_COVERED .NE. 0) THEN
          TEMP = GETSTREETDETECTORMOEDATA('OnTime', IARRAY, FLAG, R1)
          TEMP = GETBASICMOEDATA('SimulationTime', FLAG, R2)
          RMOE = 100 * R1 / R2 / SDETECTOR(IDET)%LANES_COVERED
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IDET, MDAT) = RMOE
          SAVED(MOENUM, IDET, MDAT+1) = RMOE
          SAVED(MOENUM, IDET, MTIME) = SIMTIME
        ENDIF
! ----------------------------------------------------------------------
      ELSEIF(MOETYPE .EQ. 'OnTime') THEN
        
        MOENUM = 6
        IF(SIMTIME .EQ. SAVED(MOENUM, IDET, MTIME)) THEN
          RMOE = SAVED(MOENUM, IDET, MDAT)
        ELSE
          RMOE = SDETECTOR(IDET)%ON_TIME
          IF(FLAG .EQ. 0) THEN
            SAVED(MOENUM, IDET, MDAT) = RMOE
          ELSE
            RSAVE = RMOE
            RMOE = RMOE - SAVED(MOENUM, IDET, MDAT+1)
            IF((RMOE) .LT. 1E-5) RMOE = 0.0
            SAVED(MOENUM, IDET, MDAT) = RMOE
            SAVED(MOENUM, IDET, MDAT+1) = RSAVE
          ENDIF
          SAVED(MOENUM, IDET, MTIME) = SIMTIME              
        ENDIF
! ----------------------------------------------------------------------
      ELSEIF(MOETYPE .EQ. 'SpeedAverage') THEN
        
        MOENUM = 7
        IF(SIMTIME .EQ. SAVED(MOENUM, IDET, MTIME)) THEN
          RMOE = SAVED(MOENUM, IDET, MDAT)
        ELSE
          TEMP = GETSTREETDETECTORMOEDATA('SpeedTotal', IARRAY, FLAG, R1)
          TEMP = GETSTREETDETECTORMOEDATA('Count', IARRAY, FLAG, R2)
          IF(R2 .NE. 0) RMOE = (R1 / R2) * (3600. / 5280.)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IDET, MDAT) = RMOE
          SAVED(MOENUM, IDET, MDAT+1) = RMOE
          SAVED(MOENUM, IDET, MTIME) = SIMTIME
        ENDIF
! ----------------------------------------------------------------------
      ELSEIF(MOETYPE .EQ. 'SpeedTotal') THEN
       
        MOENUM = 8
        IF(SIMTIME .EQ. SAVED(MOENUM, IDET, MTIME)) THEN
          RMOE = SAVED(MOENUM, IDET, MDAT)
        ELSE
          RMOE = SDETECTOR(IDET)%SPEED_TOTAL
          IF(FLAG .EQ. 0) THEN
            SAVED(MOENUM, IDET, MDAT) = RMOE
          ELSE
            RSAVE = RMOE
            RMOE = RMOE - SAVED(MOENUM, IDET, MDAT+1)
            IF((RMOE) .LT. 1E-5) RMOE = 0.0
            SAVED(MOENUM, IDET, MDAT) = RMOE
            SAVED(MOENUM, IDET, MDAT+1) = RSAVE
          ENDIF
          SAVED(MOENUM, IDET, MTIME) = SIMTIME              
        ENDIF
! ----------------------------------------------------------------------
      ELSEIF(MOETYPE .EQ. 'Volume') THEN
 
        MOENUM = 9
        IF(SIMTIME .EQ. SAVED(MOENUM, IDET, MTIME)) THEN
          RMOE = SAVED(MOENUM, IDET, MDAT)
        ELSE
          TEMP = GETSTREETDETECTORMOEDATA('Count', IARRAY, FLAG,R1)
          TEMP = GETBASICMOEDATA('SimulationTime', FLAG, R2)
          RMOE = 3600. * R1 / R2 / TIMESTEP
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IDET, MDAT) = RMOE
          SAVED(MOENUM, IDET, MDAT+1) = RMOE
          SAVED(MOENUM, IDET, MTIME) = SIMTIME
        ENDIF
! ----------------------------------------------------------------------
      ELSEIF(MOETYPE .EQ. 'LengthTotal') THEN
       
        MOENUM = 10
        IF(SIMTIME .EQ. SAVED(MOENUM, IDET, MTIME)) THEN
          RMOE = SAVED(MOENUM, IDET, MDAT)
        ELSE
          RMOE = SDETECTOR(IDET)%LENGTH_TOTAL
          IF(FLAG .EQ. 0) THEN
            SAVED(MOENUM, IDET, MDAT) = RMOE
          ELSE
            RSAVE = RMOE
            RMOE = RMOE - SAVED(MOENUM, IDET, MDAT+1)
            IF((RMOE) .LT. 1E-5) RMOE = 0.0
            SAVED(MOENUM, IDET, MDAT) = RMOE
            SAVED(MOENUM, IDET, MDAT+1) = RSAVE
          ENDIF
          SAVED(MOENUM, IDET, MTIME) = SIMTIME              
        ENDIF
! ----------------------------------------------------------------------
      ELSEIF(MOETYPE .EQ. 'VehicleLengthAverage') THEN
        
        MOENUM = 11
        IF(SIMTIME .EQ. SAVED(MOENUM, IDET, MTIME)) THEN
          RMOE = SAVED(MOENUM, IDET, MDAT)
        ELSE
          TEMP = GETSTREETDETECTORMOEDATA('LengthTotal', IARRAY, FLAG, R1)
          TEMP = GETSTREETDETECTORMOEDATA('Count', IARRAY, FLAG, R2)
          IF(R2 .NE. 0) RMOE = R1 / R2 
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IDET, MDAT) = RMOE
          SAVED(MOENUM, IDET, MDAT+1) = RMOE
          SAVED(MOENUM, IDET, MTIME) = SIMTIME
        ENDIF
! ----------------------------------------------------------------------
      ELSE
        GETSTREETDETECTORMOEDATA = 1
        msgtext = moetype            
        call sendtextmsg(3)
      ENDIF
    ENDIF
  ENDIF
10 IF(STATUS .NE. 0) THEN
    CALL SENDTEXTMSG2('MEMORY ALLOCATION FAILED IN FUNCTION: ', 'GETSTREETDETECTORMOEDATA', 3)
    GETSTREETDETECTORMOEDATA = 2
  ENDIF
  END
!
! ==========================================================================================================
! Function: GETENTRYLINKMOEDATA( char MOETYPE, int IARRAY, int FLAG, float RMOE )
!
! --- CODED   11-25-03 BY T. SIMMERMAN
!
! Description: Returns the value of the specified MOE for the link
!              specified by up node and down node
!
!
! Arguments: MOETYPE - [in] name of the MOE
!
!            IARRAY  - [in] array of input parameters
!                           (up node, down node)
!
!            FLAG    - [in] MOE granularity
!                           0 = cumulative
!                           1 = time interval
!                           2 = time period
!
!            RMOE    - [out] value of the MOE
!
! Returns: int = 0 - success
! Returns: int = 1 - MOE name was not recognized or link was not found
!
! ======================================================================
  INTEGER RECURSIVE FUNCTION GETENTRYLINKMOEDATA &
  [DLLEXPORT,ALIAS:'GetEntryLinkMoeData'](MOETYPE, IARRAY, FLAG, RMOE)
 
  USE FREEWAY_LINKS
  USE STREET_LINKS
  USE TEXT
  USE SIMPARAMS
  USE VEHICLE_TYPES
  USE FLOWDATA_MOD
  USE QUEUE_MOD
  USE ENTRYNODE_DATA
  IMPLICIT NONE
 
  CHARACTER*(*) MOETYPE
  INTEGER     IARRAY  [REFERENCE]
  INTEGER     FLAG    [VALUE]
  REAL        RMOE    [REFERENCE]
  DIMENSION IARRAY(*)
  INTEGER NARRAY(2)
 
  REAL SAVED(:,:,:)
  ALLOCATABLE SAVED
  SAVE SAVED
  INTEGER :: STATUS, MTIME, MDAT, MOENUM, TEMP
  INTEGER :: INODE, IUP, IDW, IL, NWAIT, IN, INET
  REAL :: R1, R2, RSAVE
  TYPE(VEHICLE) :: FRONT_VEHICLE
  TYPE(QNODE), POINTER :: VTEMP
 
  IUP = IARRAY(1)
  IDW = IARRAY(2)
  NARRAY(1) = IUP
  NARRAY(2) = IDW
 
  RMOE = 0.
  GETENTRYLINKMOEDATA = 0
 
  STATUS = 0
  IF(MOETYPE .EQ. 'DeallocateMemory') THEN
    IF(ALLOCATED(SAVED)) DEALLOCATE(SAVED)
  ELSE
    IF(.NOT. ALLOCATED(SAVED)) THEN
      ALLOCATE(SAVED(5, MAX_NODE_NUMBER+1:MAX_NODE_NUMBER+1000, 8), STAT = STATUS)
      IF(STATUS .NE. 0) GOTO 10
      SAVED = 0.0
    ENDIF
   
    IF(FLAG .EQ. 0) THEN
      MTIME = 1
      MDAT = 2
    ELSEIF(FLAG .EQ. 1) THEN
      MTIME = 3
      MDAT = 4
    ELSEIF(FLAG .EQ. 2) THEN
      MTIME = 6
      MDAT = 7
    ELSE
      GETENTRYLINKMOEDATA = 2
      GOTO 10
    ENDIF
        
    CALL FIND_FREEWAY_LINK(IUP, IDW, IL)
    IF(IL .NE. 0) THEN
      INODE = FUSN(IL)
      INET = 1
    ELSE
      CALL FIND_STREET_LINK(IUP, IDW, IL)
      IF(IL .NE. 0) THEN
        INODE = SUSN(IL)
        INET = 2
      ENDIF
    ENDIF
        
    IF(IL .EQ. 0) THEN
      GETENTRYLINKMOEDATA = 1
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayedVehicles') THEN     
      MOENUM = 1
      IF(SIMTIME .EQ. SAVED(MOENUM, INODE, MTIME)) THEN
        RMOE = SAVED(MOENUM, INODE, MDAT)
      ELSE
        RMOE = DELAYED_COUNT(INODE)
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, INODE, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, INODE, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, INODE, MDAT) = RMOE
          SAVED(MOENUM, INODE, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, INODE, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayPerVehicle') THEN
      MOENUM = 2
      IF(SIMTIME .EQ. SAVED(MOENUM, INODE, MTIME)) THEN
        RMOE = SAVED(MOENUM, INODE, MDAT)
      ELSE
        TEMP = GETENTRYLINKMOEDATA('DelayTotal', IARRAY, FLAG, R1)
        TEMP = GETENTRYLINKMOEDATA('DelayedVehicles', IARRAY, FLAG, R2)
        IF(R2 .NE. 0) RMOE = R1 / R2
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, INODE, MDAT) = RMOE
        SAVED(MOENUM, INODE, MDAT+1) = RMOE
        SAVED(MOENUM, INODE, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayTotal') THEN
      MOENUM = 3
      IF(SIMTIME .EQ. SAVED(MOENUM, INODE, MTIME)) THEN
        RMOE = SAVED(MOENUM, INODE, MDAT)
      ELSE
        RMOE = DELAYED_TIME(INODE)
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, INODE, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, INODE, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, INODE, MDAT) = RMOE
          SAVED(MOENUM, INODE, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, INODE, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'VehiclesDischargedTotal') THEN
       
      MOENUM = 4
      IF(SIMTIME .EQ. SAVED(MOENUM, INODE, MTIME)) THEN
        RMOE = SAVED(MOENUM, INODE, MDAT)
      ELSE
        IF(INET .EQ. 1) THEN
          RMOE = FDISCHARGED(IL)
        ELSE
          RMOE = SDISCHARGED(IL)
        ENDIF
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, INODE, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, INODE, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, INODE, MDAT) = RMOE
          SAVED(MOENUM, INODE, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, INODE, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'VehiclesInQueueCurrent') THEN
       
      MOENUM = 5
      IF(SIMTIME .EQ. SAVED(MOENUM, INODE, MTIME)) THEN
        RMOE = SAVED(MOENUM, INODE, MDAT)
      ELSE
        NWAIT = 0
        DO IN = 1, NUMBER_OF_ENTRYNODES
          IF(ENTRYLINK(IN)%UP .EQ. IUP .AND. ENTRYLINK(IN)%DOWN .EQ. IDW) THEN
            IF(ASSOCIATED(FRONT(IN)%PTR)) THEN
              FRONT_VEHICLE = FRONT(IN)%PTR%VEHICLE
              VTEMP => FRONT(IN)%PTR
              DO WHILE(ASSOCIATED(VTEMP))
                IF(SIMTIME .GT. VTEMP%VEHICLE%DEPARTURE) THEN
                  NWAIT = NWAIT + 1
                ENDIF
                VTEMP => VTEMP%NEXT
              ENDDO
              EXIT
            ENDIF
          ENDIF
        ENDDO
        RMOE = NWAIT
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, INODE, MDAT) = RMOE
        SAVED(MOENUM, INODE, MDAT+1) = RMOE
        SAVED(MOENUM, INODE, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSE
      GETENTRYLINKMOEDATA = 1
      msgtext = moetype            
      call sendtextmsg(3)
    ENDIF
  ENDIF
10 IF(STATUS .NE. 0) THEN
    CALL SENDTEXTMSG2('MEMORY ALLOCATION FAILED IN FUNCTION: ', 'GETENTRYLINKMOEDATA', 3)
    GETENTRYLINKMOEDATA = 2
  ENDIF
  END
!
! ==========================================================================================================
! Function: GETFREEWAYLANEMOEDATA( char MOETYPE, int IARRAY, int FLAG, float RMOE )
!
! --- CODED   11-25-03 BY T. SIMMERMAN
!
! Description: Returns the value of the specified MOE for the specified
!              lane on the link specified by up node and down node
!
!
! Arguments: MOETYPE - [in] name of the MOE
!
!            IARRAY  - [in] array of input parameters
!                           (up node, down node, lane number)
!
!            FLAG    - [in] MOE granularity
!                           0 = cumulative
!                           1 = time interval
!                           2 = time period
!
!            RMOE    - [out] value of the MOE
!
! Returns: int = 0 - success
! Returns: int = 1 - MOE name was not recognized or link was not found
! ======================================================================
  INTEGER RECURSIVE FUNCTION GETFREEWAYLANEMOEDATA &
  [DLLEXPORT,ALIAS:'GetFreewayLaneMoeData'](MOETYPE, IARRAY, FLAG, RMOE)
 
  USE FREEWAY_LINKS
  USE FREEWAY_VEHICLES
  USE TEXT
  USE SIMPARAMS
  USE GLOBAL_DATA
  USE NODE_TABLE
  IMPLICIT NONE
  CHARACTER*(*) MOETYPE
  INTEGER     IARRAY  [REFERENCE]
  INTEGER     FLAG    [VALUE]
  REAL        RMOE    [REFERENCE]
  DIMENSION IARRAY(*)
 
  REAL SAVED(:,:,:,:)
  ALLOCATABLE SAVED
  SAVE SAVED
  INTEGER :: STATUS, MTIME, MDAT, MOENUM, TEMP, GETBASICMOEDATA
  INTEGER :: IUP, IDW, IL, ILN, ICNT, IV
  INTEGER :: IADP, IAUX
  REAL :: RL, R1, R2, R3, R4, RSAVE, RDEN, RSPEED
 
  IUP = IARRAY(1)
  IDW = IARRAY(2)
  ILN = IARRAY(3)
  GETFREEWAYLANEMOEDATA = 0
 
  RMOE = 0.0
  STATUS = 0
  IF(MOETYPE .EQ. 'DeallocateMemory') THEN
    IF(ALLOCATED(SAVED)) DEALLOCATE(SAVED)
  ELSE
    IF(NODE_TYPE(IUP) .EQ. NT_EXTERN .OR. NODE_TYPE(IDW) .EQ. NT_EXTERN) THEN
      GETFREEWAYLANEMOEDATA = 1
      RETURN
    ENDIF
    
    IF(.NOT. ALLOCATED(SAVED)) THEN
      ALLOCATE(SAVED(22, N_FREEWAY_LINKS, N_FREEWAY_LANES, 8),STAT = STATUS)
      IF(STATUS .NE. 0) GOTO 10                                     
      SAVED = 0.0                                                 
    ENDIF
   
    IF(FLAG .EQ. 0) THEN
      MTIME = 1
      MDAT = 2
    ELSEIF(FLAG .EQ. 1) THEN
      MTIME = 3
      MDAT = 4
    ELSEIF(FLAG .EQ. 2) THEN
      MTIME = 6
      MDAT = 7
    ELSE
      GETFREEWAYLANEMOEDATA = 2
      GOTO 10
    ENDIF
        
    CALL FIND_FREEWAY_LINK(IUP, IDW, IL)
        
    IF(IL .EQ. 0) THEN
      GETFREEWAYLANEMOEDATA = 1
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'ContentAverage') THEN
 
! ------- VEHICLES
 
      MOENUM = 1
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        TEMP = GETFREEWAYLANEMOEDATA('TravelTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETBASICMOEDATA('SimulationTime', FLAG, R2)
        RMOE = R1 * 60. / R2
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        SAVED(MOENUM, IL, ILN, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'ContentCurrent') THEN
 
! ------- VEHICLES
 
      MOENUM = 2
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        ICNT = 0
        DO IV = 1, MAX_VEHICLES_F
          IF(FLINK(IV) .EQ. IL .AND. FLANE(IV) .EQ. ILN) THEN
            ICNT = ICNT + 1
          ENDIF
        ENDDO
        RMOE = ICNT
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        SAVED(MOENUM, IL, ILN, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayTravelPerVehicle') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 3
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        TEMP = GETFREEWAYLANEMOEDATA('TravelTimePerVehicle', IARRAY, FLAG, R1)
        TEMP = GETFREEWAYLANEMOEDATA('MoveTimePerVehicle', IARRAY, FLAG, R2)
        RMOE = R1 - R2
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        SAVED(MOENUM, IL, ILN, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayTravelTotal') THEN
 
! ------- MINUTES
 
      MOENUM = 4
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        RMOE = FZDELAY_LANE(IL, ILN) / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, ILN, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, ILN, MDAT) = RMOE
          SAVED(MOENUM, IL, ILN, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'Length') THEN                              
                                                                     
! ------- FEET                                                          
                                                                        
      MOENUM = 5          
!This MOE does not vary by time. If it has been computed use the previously computed value.                                          
      IF(SAVED(MOENUM, IL, ILN, MDAT) .NE. 0) THEN  
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)                         
      ELSE                                                          
                                                                        
! --- DETERMINE THE INDIVIDUAL LANE LENGTHS                             
                                                                        
        RL = 0.0                                                    
        IF(ILN .LE. FNUMLANES(IL)) THEN
          RL = FLENGTH(IL)
                                                                       
! --- ACCOUNT FOR LANE ADDS AND DROPS RELATED TO LANE ILN               
                                                                        
          DO IADP = 1, 3                            
            IF(ADDDROP_LANE(IL, IADP) .EQ. 0) EXIT
            IF(ADDDROP_LANE(IL, IADP) .EQ. ILN) THEN
              RL = RL + (RL - ADDDROP_DIST(IL, IADP)) * ADDDROP_CODE(IL, IADP)
            ENDIF                                                   
          ENDDO                                                     
        ELSE                                                        
                                                                       
! --- GET THE LENGTH OF AUXILIARY LANE ILN                              
                                                                      
          DO IAUX = 1, N_AUXLANES                            
            IF(AUX_LANE_CODE(IL, IAUX) .EQ. 0) EXIT !no more auxiliary lanes on this link 
            IF(AUX_LANE_ID(IL, IAUX) .EQ. ILN) THEN
              RL = RL + AUX_LANE_LENGTH(IL, IAUX)
            ENDIF
          ENDDO                                                     
        ENDIF                                                       
        RMOE = RL                                                   
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE                       
      ENDIF                                                         
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MoveTimePerVehicle') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 6
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        TEMP = GETFREEWAYLANEMOEDATA('TravelTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETFREEWAYLANEMOEDATA('DelayTravelTotal', IARRAY, FLAG, R2)
        TEMP = GETFREEWAYLANEMOEDATA('TravelTimePerVehicle', IARRAY, FLAG, R3)
        IF(R1 .NE. 0.) RMOE = (R1 - R2) * R3 / R1 
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        SAVED(MOENUM, IL, ILN, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'PersonDelayPerPerson') THEN
 
! ------- SECONDS/PERSON
 
      MOENUM = 7
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        TEMP = GETFREEWAYLANEMOEDATA('DelayTravelPerVehicle', IARRAY, FLAG, R1)
        TEMP = GETFREEWAYLANEMOEDATA('ContentAverage', IARRAY, FLAG, R2)
        TEMP = GETFREEWAYLANEMOEDATA('PersonTravelTimeTotal', IARRAY, FLAG, R3)
        TEMP = GETBASICMOEDATA('SimulationTime', FLAG, R4)
        IF(R3 .NE. 0.) RMOE = R1 * R2 / (R3 / R4) / 60.    
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        SAVED(MOENUM, IL, ILN, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'PersonMoveTimePerPerson') THEN
 
! ------- SECONDS/PERSON
 
      MOENUM = 8 
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        TEMP = GETFREEWAYLANEMOEDATA('MoveTimePerVehicle', IARRAY, FLAG, R1)
        TEMP = GETFREEWAYLANEMOEDATA('ContentAverage', IARRAY, FLAG, R2)
        TEMP = GETFREEWAYLANEMOEDATA('PersonTravelTimeTotal', IARRAY, FLAG, R3)
        TEMP = GETBASICMOEDATA('SimulationTime', FLAG, R4)
        IF(R3 .NE. 0.) RMOE = (R1 * R2 / (R3 / R4)) / 60.
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        SAVED(MOENUM, IL, ILN, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'PersonTravelTimePerPerson') THEN
 
! ------- SECONDS/PERSON
 
      MOENUM = 9
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        TEMP = GETFREEWAYLANEMOEDATA('PersonDelayPerPerson', IARRAY, FLAG, R1)
        TEMP = GETFREEWAYLANEMOEDATA('PersonMoveTimePerPerson', IARRAY, FLAG, R2)
        RMOE = R2 + R1
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        SAVED(MOENUM, IL, ILN, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'PersonTravelTimeTotal') THEN
 
! ------- SECONDS                                                       
 
      MOENUM = 10
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        RMOE = FZOCCS(IL, ILN) / 60.
        RMOE = RMOE                                           
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, ILN, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, ILN, MDAT) = RMOE
          SAVED(MOENUM, IL, ILN, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
!        ELSEIF(MOETYPE .EQ. 'QTotal') THEN
!          MOENUM = 11
!          IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
!            RMOE = SAVED(MOENUM, IL, ILN, MDAT)
!          ELSE
!            RMOE = QUEUE_TOTAL(IL, ILN)
!            IF(FLAG .EQ. 0) THEN
!              SAVED(MOENUM, IL, ILN, MDAT) = RMOE
!            ELSE
!              RSAVE = RMOE
!              RMOE = RMOE - SAVED(MOENUM, IL, ILN, MDAT+1)
!              IF((RMOE) .LT. 1E-5) RMOE = 0.0
!              SAVED(MOENUM, IL, ILN, MDAT) = RMOE
!              SAVED(MOENUM, IL, ILN, MDAT+1) = RSAVE
!            ENDIF
!            SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
!          ENDIF
!! ----------------------------------------------------------------------
!        ELSEIF(MOETYPE .EQ. 'QueueAverageNumberVehicles') THEN
!c       
!          MOENUM = 12
!          IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
!            RMOE = SAVED(MOENUM, IL, ILN, MDAT)
!          ELSE
!            TEMP = GETFREEWAYLANEMOEDATA('QTotal', IARRAY, FLAG, R1)
!            TEMP = GETBASICMOEDATA('SimulationTime', FLAG, R2)
!            RMOE = R1 / (R2 / TIMESTEP)
!            SAVED(MOENUM, IL, ILN, MDAT) = RMOE
!            SAVED(MOENUM, IL, ILN, MDAT+1) = RMOE
!            SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
!          ENDIF
!! ----------------------------------------------------------------------
!        ELSEIF(MOETYPE .EQ. 'QueueCurrentLength') THEN
!c
!! --- FEET
!c
!          MOENUM = 13
!          IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
!            RMOE = SAVED(MOENUM, IL, ILN, MDAT)
!          ELSE
!            IV = FIRST_VEHICLE(IL, ILN)
!            DO WHILE(IV .NE. 0)
!              IF(QSTATE(IV) .EQ. QS_NOTINQ) EXIT
!              RMOE = LENGTH(IL) - SLOCATION(IV) + SVLENGTH(IV)
!              IV = SFOLLOWER(IV)
!            ENDDO
!            IF(FLAG .EQ. 0) THEN
!              SAVED(MOENUM, IL, ILN, MDAT) = RMOE
!            ELSE
!              RSAVE = RMOE
!              RMOE = RMOE - SAVED(MOENUM, IL, ILN, MDAT+1)
!              IF((RMOE) .LT. 1E-5) RMOE = 0.0
!              SAVED(MOENUM, IL, ILN, MDAT) = RMOE
!              SAVED(MOENUM, IL, ILN, MDAT+1) = RSAVE
!            ENDIF
!            SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
!          ENDIF
!! ----------------------------------------------------------------------
!        ELSEIF(MOETYPE .EQ. 'QueueCurrentNumberVehicles') THEN
!c
!! --- NUMBER OF VEHICLES
!c
!          MOENUM = 14
!          IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
!            RMOE = SAVED(MOENUM, IL, ILN, MDAT)
!          ELSE
!            IV = FIRST_VEHICLE(IL, ILN)
!            DO WHILE(IV .NE. 0)
!              IF(QSTATE(IV) .EQ. QS_NOTINQ) EXIT
!              RMOE = RMOE + 1
!              IV = SFOLLOWER(IV)
!            ENDDO
!            IF(FLAG .EQ. 0) THEN
!              SAVED(MOENUM, IL, ILN, MDAT) = RMOE
!            ELSE
!              RSAVE = RMOE
!              RMOE = RMOE - SAVED(MOENUM, IL, ILN, MDAT+1)
!              IF((RMOE) .LT. 1E-5) RMOE = 0.0
!              SAVED(MOENUM, IL, ILN, MDAT) = RMOE
!              SAVED(MOENUM, IL, ILN, MDAT+1) = RSAVE
!            ENDIF
!            SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
!          ENDIF
!! ----------------------------------------------------------------------
!        ELSEIF(MOETYPE .EQ. 'QueueMaximumNumberVehicles') THEN
!          MOENUM = 15
!          IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
!            RMOE = SAVED(MOENUM, IL, ILN, MDAT)
!          ELSE
!            RMOE = QUEUE_MAX(IL, ILN)
!            IF(FLAG .EQ. 0) THEN
!              SAVED(MOENUM, IL, ILN, MDAT) = RMOE
!            ELSE
!              RSAVE = RMOE
!              RMOE = RMOE - SAVED(MOENUM, IL, ILN, MDAT+1)
!              IF((RMOE) .LT. 1E-5) RMOE = 0.0
!              SAVED(MOENUM, IL, ILN, MDAT) = RMOE
!              SAVED(MOENUM, IL, ILN, MDAT+1) = RSAVE
!            ENDIF
!            SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
!          ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'SpeedAverage') THEN
 
! ------- MILES/HOUR
 
      MOENUM = 16
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        TEMP = GETFREEWAYLANEMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R1)
        TEMP = GETFREEWAYLANEMOEDATA('TravelTimeTotal', IARRAY, FLAG, R2)
        R2 = R2 / 60.
        IF(R2 .NE. 0.0) RMOE = R1 / R2
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        SAVED(MOENUM, IL, ILN, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelDistanceTotal') THEN
 
! ------- MILES
 
      MOENUM = 17
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        RMOE = FZDIST_LANE(IL, ILN) / 5280.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, ILN, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, ILN, MDAT) = RMOE
          SAVED(MOENUM, IL, ILN, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimeTotal') THEN
 
! ------- MINUTES
 
      MOENUM = 18
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        RMOE = FZTIME_LANE(IL, ILN) / 60. 
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, ILN, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, ILN, MDAT) = RMOE
          SAVED(MOENUM, IL, ILN, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimePerVehicle') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 19
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        TEMP = GETFREEWAYLANEMOEDATA('SpeedAverage', IARRAY, FLAG, R1)
        IF(R1 .NE. 0.0) RMOE = FLENGTH(IL) / R1 * 0.68
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        SAVED(MOENUM, IL, ILN, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'VehiclesDischargedPerHour') THEN
       
      MOENUM = 20
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        TEMP = GETFREEWAYLANEMOEDATA('VehiclesDischargedTotal', IARRAY, FLAG, R1)
        TEMP = GETBASICMOEDATA('SimulationTime', FLAG, R2)
        RMOE = 3600. * R1 / R2
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        SAVED(MOENUM, IL, ILN, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'VehiclesDischargedTotal') THEN
       
      MOENUM = 21
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        RMOE = FDISCHARGED_LANE(IL, ILN)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, ILN, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, ILN, MDAT) = RMOE
          SAVED(MOENUM, IL, ILN, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'Volume') THEN
 
! ------- VEHICLES/HOUR
 
      MOENUM = 22
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        TEMP = GETFREEWAYLANEMOEDATA('ContentAverage', IARRAY, FLAG, R1)
        TEMP = GETFREEWAYLANEMOEDATA('TravelTimeTotal', IARRAY, FLAG, R2)
        TEMP = GETFREEWAYLANEMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R3)
        TEMP = GETFREEWAYLANEMOEDATA('Length', IARRAY, FLAG, R4)
        R2 = R2 / 60.
        R4 = R4 / 5280.
        IF(R2 .NE. 0.0) THEN                           
          RSPEED = R3 / R2                 
        ELSE                                              
          RSPEED = 0.0                                    
        ENDIF                                             
        IF(R4 .NE. 0) THEN   
          RDEN = R1 / R4
        ELSE                     
          RDEN = 0               
        ENDIF                    
        RMOE = RDEN * RSPEED
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        SAVED(MOENUM, IL, ILN, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSE
      GETFREEWAYLANEMOEDATA = 1
      WRITE(MSGTEXT, '(A)') 'GETFREEWAYLANEMOEDATA'
      CALL SENDTEXTMSG(3)
      WRITE(MSGTEXT, '(A,A)') 'FAILED TO RECOGNIZE MOE ', MOETYPE
      CALL SENDTEXTMSG(3)
    ENDIF
  ENDIF
10 IF(STATUS .NE. 0) THEN
    CALL SENDTEXTMSG2('MEMORY ALLOCATION FAILED IN FUNCTION: ',  &
                      'GETFREEWAYLANEMOEDATA', 3)
    GETFREEWAYLANEMOEDATA = 2
  ENDIF
  END
!
! ==========================================================================================================
! Function: GETSTREETLANEMOEDATA( char MOETYPE, int IARRAY, int FLAG, float RMOE )
!
! --- CODED   11-25-03 BY T. SIMMERMAN
!
! Description: Returns the value of the specified MOE for the specified
!              lane on the link specified by up node and down node
!
!
! Arguments: MOETYPE - [in] name of the MOE
!
!            IARRAY  - [in] array of input parameters
!                           (up node, down node, lane number)
!
!            FLAG    - [in] MOE granularity
!                           0 = cumulative
!                           1 = time interval
!                           2 = time period
!
!            RMOE    - [out] value of the MOE
!
! Returns: int = 0 - success
! Returns: int = 1 - MOE name was not recognized or link was not found
! ======================================================================
  INTEGER RECURSIVE FUNCTION GETSTREETLANEMOEDATA &
  [DLLEXPORT, ALIAS:'GetStreetLaneMoeData'](MOETYPE, IARRAY, FLAG, RMOE)
 
  USE STREET_LINKS
  USE STREET_VEHICLES
  USE TEXT
  USE SIMPARAMS
  USE GLOBAL_DATA
  USE NODE_TABLE
  IMPLICIT NONE
  CHARACTER*(*) MOETYPE
  INTEGER     IARRAY  [REFERENCE]
  INTEGER     FLAG    [VALUE]
  REAL        RMOE    [REFERENCE]
  DIMENSION IARRAY(*)
 
  REAL SAVED(:,:,:,:)
  ALLOCATABLE SAVED
  SAVE SAVED
  INTEGER :: STATUS, MTIME, MDAT, MOENUM, TEMP, GETBASICMOEDATA
  INTEGER :: IUP, IDW, IL, ILN, ICNT, IV, max_size_A
  REAL :: RL, R1, R2, R3, R4, RSAVE, RDEN, RSPEED
 
  IUP = IARRAY(1)
  IDW = IARRAY(2)
  ILN = IARRAY(3)
  !LZ added 
 max_size_A = SIZE(SFOLLOWER)
  RMOE = 0.0
  GETSTREETLANEMOEDATA = 0
 
  STATUS = 0
  IF(MOETYPE .EQ. 'DeallocateMemory') THEN
    IF(ALLOCATED(SAVED)) DEALLOCATE(SAVED)
  ELSE
    IF(NODE_TYPE(IUP) .EQ. NT_EXTERN .OR. NODE_TYPE(IDW) .EQ. NT_EXTERN) THEN
      GETSTREETLANEMOEDATA = 1
      RETURN
    ENDIF
    
    IF(.NOT. ALLOCATED(SAVED)) THEN
      ALLOCATE(SAVED(22, N_STREET_LINKS, N_FREEWAY_LANES, 8),STAT = STATUS)
      IF(STATUS .NE. 0) GOTO 10                                     
      SAVED = 0.0                                                 
    ENDIF
   
    IF(FLAG .EQ. 0) THEN
      MTIME = 1
      MDAT = 2
    ELSEIF(FLAG .EQ. 1) THEN
      MTIME = 3
      MDAT = 4
    ELSEIF(FLAG .EQ. 2) THEN
      MTIME = 6
      MDAT = 7
    ELSE
      GETSTREETLANEMOEDATA = 2
      GOTO 10
    ENDIF
        
    CALL FIND_STREET_LINK(IUP, IDW, IL)
        
    IF(IL .EQ. 0) THEN
      GETSTREETLANEMOEDATA = 1
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'ContentAverage') THEN
 
! ------- VEHICLES
 
      MOENUM = 1
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        TEMP = GETSTREETLANEMOEDATA('TravelTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETBASICMOEDATA('SimulationTime', FLAG, R2)
        RMOE = R1 * 60. / R2
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        SAVED(MOENUM, IL, ILN, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'ContentCurrent') THEN
 
! ------- VEHICLES
 
      MOENUM = 2
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        ICNT = 0
        DO IV = 1, MAX_VEHICLES_S
          IF(SLINK(IV) .EQ. IL .AND. SLANE(IV) .EQ. ILN) THEN
            ICNT = ICNT + 1
          ENDIF
        ENDDO
        RMOE = ICNT
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        SAVED(MOENUM, IL, ILN, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayTravelPerVehicle') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 3
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        TEMP = GETSTREETLANEMOEDATA('TravelTimePerVehicle', IARRAY, FLAG, R1)
        TEMP = GETSTREETLANEMOEDATA('MoveTimePerVehicle', IARRAY, FLAG, R2)
        RMOE = R1 - R2
        IF (RMOE<0) THEN
            RMOE=0
        ENDIF
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        SAVED(MOENUM, IL, ILN, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayTravelTotal') THEN
 
! ------- MINUTES
 
      MOENUM = 4
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        RMOE = SZDELAY_LANE(IL, ILN) / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, ILN, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, ILN, MDAT) = RMOE
          SAVED(MOENUM, IL, ILN, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'Length') THEN                              
                                                                       
! ------- FEET                                                          
                                                                       
      MOENUM = 5                                                    
!This MOE does not vary by time. If it has been computed use the previously computed value.                                          
      IF(SAVED(MOENUM, IL, ILN, MDAT) .NE. 0) THEN  
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)                         
      ELSE                                                          
                                                                       
! --- DETERMINE THE INDIVIDUAL LANE LENGTHS                             
                                                                       
        RL = LANE_LENGTH(IL, ILN)
        RMOE = RL                                                   
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE                       
      ENDIF                                                         
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MoveTimePerVehicle') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 6
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        TEMP = GETSTREETLANEMOEDATA('TravelTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETSTREETLANEMOEDATA('DelayTravelTotal', IARRAY, FLAG, R2)
        TEMP = GETSTREETLANEMOEDATA('TravelTimePerVehicle', IARRAY, FLAG, R3)
        IF(R1 .NE. 0.) RMOE = (R1 - R2) * R3 / R1 
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        SAVED(MOENUM, IL, ILN, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'PersonDelayPerPerson') THEN
 
! ------- SECONDS/PERSON
 
      MOENUM = 7
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        TEMP = GETSTREETLANEMOEDATA('DelayTravelPerVehicle', IARRAY, FLAG, R1)
        TEMP = GETSTREETLANEMOEDATA('ContentAverage', IARRAY, FLAG, R2)
        TEMP = GETSTREETLANEMOEDATA('PersonTravelTimeTotal', IARRAY, FLAG, R3)
        TEMP = GETBASICMOEDATA('SimulationTime', FLAG, R4)
        IF(R3 .NE. 0.) RMOE = R1 * R2 / (R3 / R4) / 60.    
        IF(RMOE .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        SAVED(MOENUM, IL, ILN, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'PersonMoveTimePerPerson') THEN
 
! ------- SECONDS/PERSON
 
      MOENUM = 8 
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        TEMP = GETSTREETLANEMOEDATA('MoveTimePerVehicle', IARRAY, FLAG, R1)
        TEMP = GETSTREETLANEMOEDATA('ContentAverage', IARRAY, FLAG, R2)
        TEMP = GETSTREETLANEMOEDATA('PersonTravelTimeTotal', IARRAY, FLAG, R3)
        TEMP = GETBASICMOEDATA('SimulationTime', FLAG, R4)
        IF(R3 .NE. 0.) RMOE = (R1 * R2 / (R3 / R4)) / 60.
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        SAVED(MOENUM, IL, ILN, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'PersonTravelTimePerPerson') THEN
 
! ------- SECONDS/PERSON
 
      MOENUM = 9
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        TEMP = GETSTREETLANEMOEDATA('PersonDelayPerPerson', IARRAY, FLAG, R1)
        TEMP = GETSTREETLANEMOEDATA('PersonMoveTimePerPerson', IARRAY, FLAG, R2)
        RMOE = R2 + R1
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        SAVED(MOENUM, IL, ILN, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'PersonTravelTimeTotal') THEN
 
! ------- SECONDS                                                       
 
      MOENUM = 10
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        RMOE = SZOCCS(IL, ILN) / 60.
        RMOE = RMOE                                           
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, ILN, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, ILN, MDAT) = RMOE
          SAVED(MOENUM, IL, ILN, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'QTotal') THEN
      MOENUM = 11
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        RMOE = QUEUE_TOTAL(IL, ILN)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, ILN, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, ILN, MDAT) = RMOE
          SAVED(MOENUM, IL, ILN, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'QueueAverageNumberVehicles') THEN
        
      MOENUM = 12
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        TEMP = GETSTREETLANEMOEDATA('QTotal', IARRAY, FLAG, R1)
        TEMP = GETBASICMOEDATA('SimulationTime', FLAG, R2)
        IF(R2 .NE. 0) RMOE = R1 / R2
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        SAVED(MOENUM, IL, ILN, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'QueueCurrentLength') THEN
 
! --- FEET
 
      MOENUM = 13
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        IV = FIRST_VEHICLE(IL, ILN)
        DO WHILE(IV .NE. 0 .AND. IV .LE. HIGHEST_INDEX_S)
          IF(QSTATE(IV) .EQ. QS_NOTINQ) EXIT
          RMOE = SLENGTH(IL) - SLOCATION(IV) + SVLENGTH(IV)
          IV = SFOLLOWER(IV)
        ENDDO
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'QueueCurrentNumberVehicles') THEN
 
! --- NUMBER OF VEHICLES
 
      MOENUM = 14
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        IV = FIRST_VEHICLE(IL, ILN)
        DO WHILE(IV .NE. 0.AND. IV .LE. HIGHEST_INDEX_S)
          IF(QSTATE(IV) .EQ. QS_NOTINQ) EXIT
          RMOE = RMOE + 1
          IV = SFOLLOWER(IV)
        ENDDO
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'QueueMaximumNumberVehicles') THEN
      MOENUM = 15
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        RMOE = QUEUE_MAX(IL, ILN)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, ILN, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, ILN, MDAT) = RMOE
          SAVED(MOENUM, IL, ILN, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'SpeedAverage') THEN
 
! ------- MILES/HOUR
 
      MOENUM = 16
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        TEMP = GETSTREETLANEMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R1)
        TEMP = GETSTREETLANEMOEDATA('TravelTimeTotal', IARRAY, FLAG, R2)
        R2 = R2 / 60.
        IF(R2 .NE. 0.0) RMOE = R1 / R2
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        SAVED(MOENUM, IL, ILN, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelDistanceTotal') THEN
 
! ------- MILES
 
      MOENUM = 17
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        RMOE = SZDIST_LANE(IL, ILN) / 5280.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, ILN, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, ILN, MDAT) = RMOE
          SAVED(MOENUM, IL, ILN, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimeTotal') THEN
 
! ------- MINUTES
 
      MOENUM = 18
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        RMOE = SZTIME_LANE(IL, ILN) / 60. 
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, ILN, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, ILN, MDAT) = RMOE
          SAVED(MOENUM, IL, ILN, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimePerVehicle') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 19
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        TEMP = GETSTREETLANEMOEDATA('SpeedAverage', IARRAY, FLAG, R1)
        IF(R1 .NE. 0.0) RMOE = SLENGTH(IL) / R1 * 0.68
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        SAVED(MOENUM, IL, ILN, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'VehiclesDischargedPerHour') THEN
        
      MOENUM = 20
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        TEMP = GETSTREETLANEMOEDATA('VehiclesDischargedTotal', IARRAY, FLAG, R1)
        TEMP = GETBASICMOEDATA('SimulationTime', FLAG, R2)
        RMOE = 3600. * R1 / R2
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        SAVED(MOENUM, IL, ILN, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'VehiclesDischargedTotal') THEN
        
      MOENUM = 21
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        RMOE = SDISCHARGED_LANE(IL, ILN)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, ILN, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, ILN, MDAT) = RMOE
          SAVED(MOENUM, IL, ILN, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'Volume') THEN
 
! ------- VEHICLES/HOUR
 
      MOENUM = 22
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ILN, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ILN, MDAT)
      ELSE
        TEMP = GETSTREETLANEMOEDATA('ContentAverage', IARRAY, FLAG, R1)
        TEMP = GETSTREETLANEMOEDATA('TravelTimeTotal', IARRAY, FLAG, R2)
        TEMP = GETSTREETLANEMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R3)
        TEMP = GETSTREETLANEMOEDATA('Length', IARRAY, FLAG, R4)
        R2 = R2 / 60.
        R4 = R4 / 5280.
        IF(R2 .NE. 0.0) THEN                           
          RSPEED = R3 / R2                 
        ELSE                                              
          RSPEED = 0.0                                    
        ENDIF                                             
        IF(R4 .NE. 0) THEN   
          RDEN = R1 / R4
        ELSE                     
          RDEN = 0               
        ENDIF                    
        RMOE = RDEN * RSPEED
        SAVED(MOENUM, IL, ILN, MDAT) = RMOE
        SAVED(MOENUM, IL, ILN, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ILN, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSE
      GETSTREETLANEMOEDATA = 1
      WRITE(MSGTEXT, '(A)') 'GETSTREETLANEMOEDATA'
      CALL SENDTEXTMSG(3)
      WRITE(MSGTEXT, '(A,A)') 'FAILED TO RECOGNIZE MOE ', MOETYPE
      CALL SENDTEXTMSG(3)
    ENDIF
  ENDIF
10 IF(STATUS .NE. 0) THEN
    CALL SENDTEXTMSG2('MEMORY ALLOCATION FAILED IN FUNCTION: ', &
                      'GETSTREETLANEMOEDATA', 3)
    GETSTREETLANEMOEDATA = 2
  ENDIF
  END
!
! ==========================================================================================================
! Function: GETFREEWAYLINKMOEDATA( char MOETYPE, int IARRAY, int FLAG, float RMOE )
!
! --- CODED   11-25-03 BY T. SIMMERMAN
!
! Description: Returns the value of the specified MOE for the link
!              specified by up node and down node
!
!
! Arguments: MOETYPE - [in] name of the MOE
!
!            IARRAY  - [in] array of input parameters
!                           (up node, down node)
!
!            FLAG    - [in] MOE granularity
!                           0 = cumulative
!                           1 = time interval
!                           2 = time period
!
!            RMOE    - [out] value of the MOE
!
! Returns: int = 0 - success
! Returns: int = 1 - MOE name was not recognized or link was not found
!
! ======================================================================
  INTEGER RECURSIVE FUNCTION GETFREEWAYLINKMOEDATA &
  [DLLEXPORT, ALIAS:'GetFreewayLinkMoeData'](MOETYPE, IARRAY, FLAG, RMOE)
 
  USE FREEWAY_LINKS
  USE FREEWAY_VEHICLES
  USE TEXT
  USE SIMPARAMS
  USE VEHICLE_TYPES
  USE GLOBAL_DATA
  USE NODE_TABLE
  IMPLICIT NONE
 
  CHARACTER*(*) MOETYPE
  INTEGER     IARRAY  [REFERENCE]
  INTEGER     FLAG    [VALUE]
  REAL        RMOE    [REFERENCE]
  DIMENSION IARRAY(*)
  INTEGER NARRAY(3)
 
  REAL SAVED(:,:,:)
  ALLOCATABLE SAVED
  SAVE SAVED
  INTEGER :: STATUS, MTIME, MDAT, MOENUM, TEMP, GETBASICMOEDATA
  INTEGER :: IV, ICNT, IUP, IDW, IL
  REAL :: R1, R2, R3, RSAVE
 
  INTERFACE
  INTEGER RECURSIVE FUNCTION GETVEHICLETYPEFREEWAYLINKMOEDATA &
  [DLLEXPORT,ALIAS:'GetVehicleTypeFreewayLinkMoeData'](MOETYPE, IARRAY, FLAG, RMOE)
    CHARACTER*(*) MOETYPE
    INTEGER     IARRAY  [REFERENCE]
    INTEGER     FLAG    [VALUE]
    REAL        RMOE    [REFERENCE]
    DIMENSION IARRAY(*)
  END FUNCTION
  END INTERFACE
 
  IUP = IARRAY(1)
  IDW = IARRAY(2)
  NARRAY(1) = IUP
  NARRAY(2) = IDW
 
  RMOE = 0.
  GETFREEWAYLINKMOEDATA = 0
 
  STATUS = 0
  IF(MOETYPE .EQ. 'DeallocateMemory') THEN
    IF(ALLOCATED(SAVED)) DEALLOCATE(SAVED)
  ELSE
    IF(NODE_TYPE(IUP) .EQ. NT_EXTERN .OR. NODE_TYPE(IDW) .EQ. NT_EXTERN) THEN
      GETFREEWAYLINKMOEDATA = 1
      RETURN
    ENDIF
    
    IF(.NOT. ALLOCATED(SAVED)) THEN
      ALLOCATE(SAVED(40, N_FREEWAY_LINKS, 8), STAT = STATUS)
      IF(STATUS .NE. 0) GOTO 10
      SAVED = 0.0
    ENDIF
   
    IF(FLAG .EQ. 0) THEN
      MTIME = 1
      MDAT = 2
    ELSEIF(FLAG .EQ. 1) THEN
      MTIME = 3
      MDAT = 4
    ELSEIF(FLAG .EQ. 2) THEN
      MTIME = 6
      MDAT = 7
    ELSE
      GETFREEWAYLINKMOEDATA = 2
      GOTO 10
    ENDIF
        
    CALL FIND_FREEWAY_LINK(IUP, IDW, IL)
        
    IF(IL .EQ. 0) THEN
      GETFREEWAYLINKMOEDATA = 1
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'BusDelayPerVehicle') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 1
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETFREEWAYLINKMOEDATA('BusDelayTotal', IARRAY, FLAG, R1)
        IF(R1 .GT. 0) THEN
          TEMP = GETFREEWAYLINKMOEDATA('BusTrips', IARRAY, FLAG, R2)
          IF(R2 .GT. 0) RMOE = R1 / R2
        ENDIF
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'BusDelayTotal') THEN
 
! ------- MINUTES
 
      MOENUM = 2
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETFREEWAYLINKMOEDATA('BusTravelTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETFREEWAYLINKMOEDATA('BusMoveTimeTotal', IARRAY, FLAG, R2)
        RMOE = R1 - R2
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'BusMoveTimePerTravelTimeRatio') THEN
      MOENUM = 3
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = 0.
        TEMP = GETFREEWAYLINKMOEDATA('BusTravelTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETFREEWAYLINKMOEDATA('BusMoveTimeTotal', IARRAY, FLAG, R2)
        IF(R1 .GT. 0) RMOE = R2 / R1
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'BusMoveTimeTotal') THEN
 
! ------- MINUTES
 
      MOENUM = 4
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETFREEWAYLINKMOEDATA('BusTravelTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETFREEWAYLINKMOEDATA('BusTrips', IARRAY, FLAG, R2)
        RMOE = MIN((R2 * FLENGTH(IL) / FFREEFLOWSPEED(IL)) / 60., R1)
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'BusPersonTrips') THEN
      MOENUM = 5 
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETFREEWAYLINKMOEDATA('BusTrips', IARRAY, FLAG, R1)
        IF(R1 .GT. 0) THEN
          RMOE = R1 * MAX(AVG_OCCS(7), 1.0)
        ENDIF
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'BusSpeedAverage') THEN
 
! ------- MILES/HOUR
 
      MOENUM = 6
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETFREEWAYLINKMOEDATA('BusTravelTimeTotal', IARRAY, FLAG, R1)
        IF(R1 .GT. 0) THEN
          TEMP = GETFREEWAYLINKMOEDATA('BusTrips', IARRAY, FLAG, R2)
          R1 = R1 / 60.
          R2 = R2 * FLENGTH(IL) / 5280.
          IF(R1 .GT. 0) RMOE = R2 / R1
        ENDIF
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'BusTravelTimePerVehicle') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 7 
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETFREEWAYLINKMOEDATA('BusTravelTimeTotal', IARRAY, FLAG, R1)
        R1 = R1 * 60.
        IF(R1 .GT. 0) THEN
          TEMP = GETFREEWAYLINKMOEDATA('BusTrips', IARRAY, FLAG, R2)
          IF(R2 .GT. 0) RMOE = R1 / R2
        ENDIF
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'BusTravelTimeTotal') THEN
 
! ------- MINUTES
 
      MOENUM = 8 
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = FZTIME_BUS(IL) / 60.0
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'BusTrips') THEN
      MOENUM = 9
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = FDISCHARGED_BUSES(IL)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'ContentAverage') THEN 
 
! ------- VEHICLES
 
      MOENUM = 10
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETFREEWAYLINKMOEDATA('TravelTimeTotal', IARRAY, FLAG, R1)
        R1 = R1 * 60.
        TEMP = GETBASICMOEDATA('SimulationTime', FLAG, R2)
        IF(R2 .NE. 0.0) RMOE = R1 / R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'ContentCurrent') THEN
 
! ------- VEHICLES
 
      MOENUM = 11
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        ICNT = 0
        DO IV = 1, MAX_VEHICLES_F
          IF(FLINK(IV) .EQ. IL) THEN
            ICNT = ICNT + 1
          ENDIF
        ENDDO
        RMOE = ICNT
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayTravelPerDistanceTraveled') THEN
 
! ------- MINUTES/MILE
 
      MOENUM = 12
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETFREEWAYLINKMOEDATA('TravelTimePerVehicle', IARRAY, FLAG, R1)
        TEMP = GETFREEWAYLINKMOEDATA('DelayTravelPerVehicle', IARRAY, FLAG, R2)
        TEMP = GETFREEWAYLINKMOEDATA('TravelTimePerDistanceTraveled', IARRAY, FLAG, R3)
        IF(R1 .NE. 0.0) RMOE = R3 * R2 / R1
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayTravelPerVehicle') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 13
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETFREEWAYLINKMOEDATA('TravelTimePerVehicle', IARRAY, FLAG, R1)
        TEMP = GETFREEWAYLINKMOEDATA('MoveTimePerVehicle', IARRAY, FLAG, R2)   
        RMOE = R1 - R2
        IF (RMOE<0) Then
            RMOE=0
        ENDIF
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayTravelTotal') THEN
 
! ------- MINUTES
 
      MOENUM = 14
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETFREEWAYLINKMOEDATA('MoveTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETFREEWAYLINKMOEDATA('TravelTimeTotal', IARRAY, FLAG, R2)
        RMOE = R2 - R1    
        IF (RMOE<0) THEN 
            RMOE=0
        ENDIF
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DensityPerLane') THEN
 
! ------- VEHICLES/MILE/LANE
 
      MOENUM = 15
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETFREEWAYLINKMOEDATA('ContentAverage', IARRAY, FLAG, R1)
        IF(FAREA(IL) .NE. 0) RMOE = R1 / FAREA(IL)
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'EmissionsTotalCO') THEN
        
      MOENUM = 19
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = FCO_TOTAL(IL) / 1000.
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'EmissionsTotalHC') THEN
        
      MOENUM = 20
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = FHC_TOTAL(IL) / 1000.
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'EmissionsTotalNOx') THEN
        
      MOENUM = 21
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = FNOX_TOTAL(IL) / 1000.
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'FuelConsumptionTotal') THEN
       
      MOENUM = 22
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = FFUEL_TOTAL(IL)
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'LaneChangesTotal') THEN
      MOENUM = 23
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = FLANE_CHANGES(IL)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MoveTimePerTravelTimeRatio') THEN
      MOENUM = 24
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETFREEWAYLINKMOEDATA('MoveTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETFREEWAYLINKMOEDATA('TravelTimeTotal', IARRAY, FLAG, R2)
        IF(R2 .NE. 0.0) RMOE = R1 / R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MoveTimePerVehicle') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 25
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETFREEWAYLINKMOEDATA('TravelTimePerVehicle', IARRAY, FLAG, R1)
        TEMP = GETFREEWAYLINKMOEDATA('MoveTimePerTravelTimeRatio', IARRAY, FLAG, R2)
        RMOE = R1 * R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MoveTimeTotal') THEN
 
! ------- MINUTES
 
      MOENUM = 26
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETFREEWAYLINKMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R1) 
        RMOE = R1 / FFREEFLOWSPEED(IL) * 5280. / 60.
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'PersonDelayTotal') THEN
      MOENUM = 27
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETFREEWAYLINKMOEDATA('PersonsPerVehicle', IARRAY, FLAG, R1)                                        
        TEMP = GETFREEWAYLINKMOEDATA('TravelTimeTotal', IARRAY, FLAG, R2)                                         
        TEMP = GETFREEWAYLINKMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R3)
        R3 = R3 * 5280.
        RMOE = MAX(R2 * R1 - R3 * R1 / (FFREEFLOWSPEED(IL) * 60.0), 0.0)           
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'PersonTravelDistanceTotal') THEN
      MOENUM = 28
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETFREEWAYLINKMOEDATA('PersonsPerVehicle', IARRAY, FLAG, R1)                                        
        TEMP = GETFREEWAYLINKMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R2)                                        
        RMOE = R2 * R1                                      
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'PersonTravelTimeTotal') THEN
      MOENUM = 29
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETFREEWAYLINKMOEDATA('PersonsPerVehicle', IARRAY, FLAG, R1)                                      
        TEMP = GETFREEWAYLINKMOEDATA('TravelTimeTotal', IARRAY, FLAG, R2)     
        RMOE = R1 * R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'PersonTripsTotal') THEN
      MOENUM = 30
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = FPERSON_TRIPS(IL)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'SpeedAverage') THEN
 
! ------- MILES/HOUR
 
      MOENUM = 31
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETFREEWAYLINKMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R1)
        TEMP = GETFREEWAYLINKMOEDATA('TravelTimeTotal', IARRAY, FLAG, R2)
        R2 = R2 / 60.
        IF(R2 .NE. 0.0) RMOE = R1 / R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelDistanceTotal') THEN
 
! ------- MILES
 
      MOENUM = 32
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = FZDIST(IL) / 5280.                                   
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimePerDistanceTraveled') THEN
 
! ------- MINUTES/MILE
 
      MOENUM = 33
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETFREEWAYLINKMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R1)
        TEMP = GETFREEWAYLINKMOEDATA('TravelTimeTotal', IARRAY, FLAG, R2)       
        IF(R1 .NE. 0.0) RMOE = R2 / R1   
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimePerVehicle') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 34
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETFREEWAYLINKMOEDATA('SpeedAverage', IARRAY, FLAG, R1)
        IF(R1 .NE. 0.0) RMOE = FLENGTH(IL) / R1 * 3600. / 5280.
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimeTotal') THEN
 
! ------- MINUTES
 
      MOENUM = 35
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = FZTIME(IL) / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'VehiclesDischarged') THEN
 
! ------- VEHICLES
 
      MOENUM = 36
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = FDISCHARGED(IL) 
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'VehiclesEnteringLink') THEN
 
! ------- VEHICLES
 
      MOENUM = 37
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = FENTERING(IL)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'VolumePerLane') THEN
 
! ------- VEHICLES/HOUR/LANE
 
      MOENUM = 38
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETFREEWAYLINKMOEDATA('DensityPerLane', IARRAY, FLAG, R1)
        TEMP = GETFREEWAYLINKMOEDATA('SpeedAverage', IARRAY, FLAG, R2)
        RMOE = R1 * R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'PersonsPerVehicle') THEN
 
! --- Used for calculating other MOEs.
         
      MOENUM = 39
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETFREEWAYLINKMOEDATA('PersonTripsTotal', IARRAY, FLAG, R1)                                      
        TEMP = GETFREEWAYLINKMOEDATA('VehiclesDischarged', IARRAY, FLAG, R2)
        IF(R2 .NE. 0) RMOE = R1 / R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'VehiclesDiverted') THEN
 
! --- VEHICLES
         
      MOENUM = 40
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = VEHICLES_DIVERTED(IL)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSE
      GETFREEWAYLINKMOEDATA = 1
      WRITE(MSGTEXT, '(A)') 'GETFREEWAYLINKMOEDATA'
      CALL SENDTEXTMSG(3)
      WRITE(MSGTEXT, '(A,A)') 'FAILED TO RECOGNIZE MOE ', MOETYPE
      CALL SENDTEXTMSG(3)
    ENDIF
  ENDIF
10 IF(STATUS .NE. 0) THEN
    CALL SENDTEXTMSG2('MEMORY ALLOCATION FAILED IN FUNCTION: ', &
                      'GETFREEWAYLINKMOEDATA', 3)
    GETFREEWAYLINKMOEDATA = 2
  ENDIF
  END
!
! ==========================================================================================================
! Function: GETSTREETLINKMOEDATA( char MOETYPE, int IARRAY, int FLAG, float RMOE )
!
! --- CODED   11-25-03 BY T. SIMMERMAN
!
! Description: Returns the value of the specified MOE for the link
!              specified by up node and down node
!
!
! Arguments: MOETYPE - [in] name of the MOE
!
!            IARRAY  - [in] array of input parameters
!                           (up node, down node)
!
!            FLAG    - [in] MOE granularity
!                           0 = cumulative
!                           1 = time interval
!                           2 = time period
!
!            RMOE    - [out] value of the MOE
!
! Returns: int = 0 - success
! Returns: int = 1 - MOE name was not recognized or link was not found
!
! ======================================================================
  INTEGER RECURSIVE FUNCTION GETSTREETLINKMOEDATA &
  [DLLEXPORT, ALIAS:'GetStreetLinkMoeData'](MOETYPE, IARRAY, FLAG, RMOE)
 
  USE STREET_LINKS
  USE STREET_VEHICLES
  USE TEXT
  USE SIMPARAMS
  USE VEHICLE_TYPES
  USE GLOBAL_DATA
  USE NODE_TABLE
  IMPLICIT NONE
 
  CHARACTER*(*) MOETYPE
  INTEGER     IARRAY  [REFERENCE]
  INTEGER     FLAG    [VALUE]
  REAL        RMOE    [REFERENCE]
  DIMENSION IARRAY(*)
  INTEGER NARRAY(3)
 
  REAL SAVED(:,:,:)
  ALLOCATABLE SAVED
  SAVE SAVED
  INTEGER :: STATUS, MTIME, MDAT, MOENUM, TEMP, GETBASICMOEDATA
  INTEGER :: IUP, IDW, IL, IV, ICNT, ITYP
  REAL :: R1, R2, R3, R4, RSAVE
  REAL :: RNLEN, RVLEN, RTRIPS, RLEN, RMVLEN
 
  INTERFACE
  INTEGER RECURSIVE FUNCTION GETVEHICLETYPESTREETLINKMOEDATA &
  [DLLEXPORT,ALIAS:'GetVehicleTypeStreetLinkMoeData'](MOETYPE, IARRAY, FLAG, RMOE)
    CHARACTER*(*) MOETYPE
    INTEGER     IARRAY  [REFERENCE]
    INTEGER     FLAG    [VALUE]
    REAL        RMOE    [REFERENCE]
    DIMENSION IARRAY(*)
  END FUNCTION
  END INTERFACE
 
  IUP = IARRAY(1)
  IDW = IARRAY(2)
  NARRAY(1) = IUP
  NARRAY(2) = IDW
 
  RMOE = 0.
  GETSTREETLINKMOEDATA = 0
 
  STATUS = 0
  IF(MOETYPE .EQ. 'DeallocateMemory') THEN
    IF(ALLOCATED(SAVED)) DEALLOCATE(SAVED)
  ELSE
    IF(NODE_TYPE(IUP) .EQ. NT_EXTERN .OR. NODE_TYPE(IDW) .EQ. NT_EXTERN) THEN
      GETSTREETLINKMOEDATA = 1
      RETURN
    ENDIF
    
    IF(.NOT. ALLOCATED(SAVED)) THEN
      ALLOCATE(SAVED(118, N_STREET_LINKS, 8), STAT = STATUS)
      IF(STATUS .NE. 0) GOTO 10
      SAVED = 0.0
    ENDIF
   
    IF(FLAG .EQ. 0) THEN
      MTIME = 1
      MDAT = 2
    ELSEIF(FLAG .EQ. 1) THEN
      MTIME = 3
      MDAT = 4
    ELSEIF(FLAG .EQ. 2) THEN
      MTIME = 6
      MDAT = 7
    ELSE
      GETSTREETLINKMOEDATA = 2
      GOTO 10
    ENDIF
        
    CALL FIND_STREET_LINK(IUP, IDW, IL)
        
    IF(IL .EQ. 0) THEN
      GETSTREETLINKMOEDATA = 1
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'BusDelayPerVehicle') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 1
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('BusDelayTotal', IARRAY, FLAG, R1)
        IF(R1 .GT. 0) THEN
          TEMP = GETSTREETLINKMOEDATA('BusTrips', IARRAY, FLAG, R2)
          IF(R2 .GT. 0) RMOE = R1 / R2
        ENDIF
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'BusDelayTotal') THEN
 
! ------- MINUTES
 
      MOENUM = 2
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('BusTravelTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('BusMoveTimeTotal', IARRAY, FLAG, R2)
        RMOE = R1 - R2
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'BusesThatStopped') THEN
      MOENUM = 3
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = 0.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'BusMoveTimePerTravelTimeRatio') THEN
      MOENUM = 4
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = 0.
        TEMP = GETSTREETLINKMOEDATA('BusTravelTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('BusMoveTimeTotal', IARRAY, FLAG, R2)
        IF(R1 .GT. 0) RMOE = R2 / R1
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'BusMoveTimeTotal') THEN
 
! ------- MINUTES
 
      MOENUM = 5
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('BusTravelTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('BusTrips', IARRAY, FLAG, R2)
        RMOE = MIN((R2 * SLENGTH(IL) / SFREEFLOWSPEED(IL)) / 60., R1)
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'BusPersonTrips') THEN
      MOENUM = 6 
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('BusTrips', IARRAY, FLAG, R1)
        IF(R1 .GT. 0) THEN
          RMOE = R1 * MAX(AVG_OCCS(7), 1.0)
        ENDIF
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'BusSpeedAverage') THEN
 
! ------- MILES/HOUR
 
      MOENUM = 7
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('BusTravelTimeTotal', IARRAY, FLAG, R1)
        IF(R1 .GT. 0) THEN
          TEMP = GETSTREETLINKMOEDATA('BusTrips', IARRAY, FLAG, R2)
          R1 = R1 / 60.
          R2 = R2 * SLENGTH(IL) / 5280.
          RMOE = R2 / R1
        ENDIF
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'BusTravelTimePerVehicle') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 8 
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('BusTravelTimeTotal', IARRAY, FLAG, R1)
        R1 = R1 * 60.
        IF(R1 .GT. 0) THEN
          TEMP = GETSTREETLINKMOEDATA('BusTrips', IARRAY, FLAG, R2)
          IF(R2 .GT. 0) RMOE = R1 / R2
        ENDIF
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'BusTravelTimeTotal') THEN
 
! ------- MINUTES
 
      MOENUM = 9 
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = SZTIME_BUS(IL) / 60.0
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'BusTrips') THEN
      MOENUM = 10
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = SDISCHARGED_BUSES(IL)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'ContentAverage') THEN 
 
! ------- VEHICLES
 
      MOENUM = 11
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelTimeTotal', IARRAY, FLAG, R1)
        R1 = R1 * 60.
        TEMP = GETBASICMOEDATA('SimulationTime', FLAG, R2)
        IF(R2 .NE. 0.0) RMOE = R1 / R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'ContentCurrent') THEN
 
! ------- VEHICLES
 
      MOENUM = 12
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        ICNT = 0
        DO IV = 1, MAX_VEHICLES_S
          IF(SLINK(IV) .EQ. IL) THEN
            ICNT = ICNT + 1
          ENDIF
        ENDDO
        RMOE = ICNT
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'ContentCurrentDiag') THEN
 
! ------- VEHICLES
 
      MOENUM = 13
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        ICNT = 0
        DO IV = 1, MAX_VEHICLES_S
          IF(SLINK(IV) .EQ. IL) THEN
            IF(STURNCODE(IV) .GT. TC_RIGHT) THEN
              ICNT = ICNT + 1
            ENDIF
          ENDIF
        ENDDO
        RMOE = ICNT
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'ContentCurrentLeft') THEN
 
! ------- VEHICLES
 
      MOENUM = 14
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        ICNT = 0
        DO IV = 1, MAX_VEHICLES_S
          IF(SLINK(IV) .EQ. IL) THEN
            IF(STURNCODE(IV) .EQ. TC_LEFT) THEN
              ICNT = ICNT + 1
            ENDIF
          ENDIF
        ENDDO
        RMOE = ICNT
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'ContentCurrentRight') THEN
 
! ------- VEHICLES
 
      MOENUM = 15
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        ICNT = 0
        DO IV = 1, MAX_VEHICLES_S
          IF(SLINK(IV) .EQ. IL) THEN
            IF(STURNCODE(IV) .EQ. TC_RIGHT) THEN
              ICNT = ICNT + 1
            ENDIF
          ENDIF
        ENDDO
        RMOE = ICNT
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'ContentCurrentThrough') THEN
 
! ------- VEHICLES
 
      MOENUM = 16
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        ICNT = 0
        DO IV = 1, MAX_VEHICLES_S
          IF(SLINK(IV) .EQ. IL) THEN
            IF(STURNCODE(IV) .EQ. TC_THRU) THEN
              ICNT = ICNT + 1
            ENDIF
          ENDIF
        ENDDO
        RMOE = ICNT
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayControlPerVehicle') THEN
      MOENUM = 17
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('DelayControlTotal', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('Trips', IARRAY, FLAG, R2)
        TEMP = GETSTREETLINKMOEDATA('ContentCurrent', IARRAY, FLAG, R3)
        R1 = R1 * 60.
        IF(R2 + R3 .GT. 0) RMOE = R1 / (R2 + R3) 
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayControlPerVehicleDiag') THEN
      MOENUM = 18
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('DelayControlTotalDiag', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TripsDiag', IARRAY, FLAG, R2)
        TEMP = GETSTREETLINKMOEDATA('ContentCurrentDiag', IARRAY, FLAG, R3)
        R1 = R1 * 60.
        IF(R2 + R3 .GT. 0) RMOE = R1 / (R2 + R3) 
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayControlPerVehicleLeft') THEN
      MOENUM = 19
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('DelayControlTotalLeft', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TripsLeft', IARRAY, FLAG, R2)
        TEMP = GETSTREETLINKMOEDATA('ContentCurrentLeft', IARRAY, FLAG, R3)
        R1 = R1 * 60.
        IF(R2 + R3 .GT. 0) RMOE = R1 / (R2 + R3) 
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayControlPerVehicleThrough') THEN
      MOENUM = 20
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('DelayControlTotalThrough', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TripsThrough', IARRAY, FLAG, R2)
        TEMP = GETSTREETLINKMOEDATA('ContentCurrentThrough', IARRAY, FLAG, R3)
        R1 = R1 * 60.
        IF(R2 + R3 .GT. 0) RMOE = R1 / (R2 + R3) 
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayControlPerVehicleRight') THEN
      MOENUM = 21
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('DelayControlTotalRight', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TripsRight', IARRAY, FLAG, R2)
        TEMP = GETSTREETLINKMOEDATA('ContentCurrentRight', IARRAY, FLAG, R3)
        R1 = R1 * 60.
        IF(R2 + R3 .GT. 0) RMOE = R1 / (R2 + R3) 
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayControlTotal') THEN
      MOENUM = 22
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = RICDELAY(IL) / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayControlTotalDiag') THEN
      MOENUM = 23
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = RICDELD(IL) / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayControlTotalLeft') THEN
      MOENUM = 24
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = RICDELL(IL) / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayControlTotalThrough') THEN
      MOENUM = 25
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = (RICDELAY(IL) - RICDELL(IL) - RICDELR(IL) - RICDELD(IL)) / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayControlTotalRight') THEN
      MOENUM = 26
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = RICDELR(IL) / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayQueuePerVehicle') THEN      
      MOENUM = 27
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('Trips', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('DelayQueueTotal', IARRAY, FLAG, R2)
        TEMP = GETSTREETLINKMOEDATA('ContentCurrent',IARRAY, FLAG, R3)
        R2 = R2 * 60.
        IF(R1 + R3 .GT. 0.) RMOE = R2 / (R1 + R3)
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayQueueTotal') THEN
      MOENUM = 28
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = QUEUE_DELAY(IL) / 60. 
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayQueueTotalDiag') THEN     
      MOENUM = 29
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = QUEUE_DELAY_DIAG(IL) / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayQueueTotalLeft') THEN     
      MOENUM = 30
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = QUEUE_DELAY_LEFT(IL) / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayQueueTotalRight') THEN      
      MOENUM = 31
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = STOP_DELAY_RIGHT(IL) / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayQueueTotalThrough') THEN      
      MOENUM = 32
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('DelayQueueTotal', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('DelayQueueTotalLeft', IARRAY, FLAG, R2)
        TEMP = GETSTREETLINKMOEDATA('DelayQueueTotalRight', IARRAY, FLAG, R3)
        TEMP = GETSTREETLINKMOEDATA('DelayQueueTotalDiag', IARRAY, FLAG, R4)
        RMOE = MAX(R1 - R2 - R3 - R4, 0.)
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME

      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayStopPerVehicle') THEN
      MOENUM = 33
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('DelayStopTotal', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('Trips', IARRAY, FLAG, R2)
        TEMP = GETSTREETLINKMOEDATA('ContentCurrent', IARRAY, FLAG, R3)
        R1 = R1 * 60.
        IF(R2 + R3 .GT. 0.) RMOE = R1 / (R2 + R3)
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayStopTotal') THEN
      MOENUM = 34
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = STOP_DELAY(IL) / 60. 
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayStopTotalDiag') THEN      
      MOENUM = 35
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = STOP_DELAY_DIAG(IL) / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayStopTotalLeft') THEN      
      MOENUM = 36
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = STOP_DELAY_LEFT(IL) / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayStopTotalRight') THEN     
      MOENUM = 37
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = STOP_DELAY_RIGHT(IL) / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayStopTotalThrough') THEN    
      MOENUM = 38
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('DelayStopTotal', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('DelayStopTotalLeft', IARRAY, FLAG, R2)
        TEMP = GETSTREETLINKMOEDATA('DelayStopTotalRight', IARRAY, FLAG, R3)
        TEMP = GETSTREETLINKMOEDATA('DelayStopTotalDiag', IARRAY, FLAG, R4)
        RMOE = MAX(R1 - R2 - R3 - R4, 0.)
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayTravelPerDistanceTraveled') THEN
 
! ------- MINUTES/MILE
 
      MOENUM = 39
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelTimePerVehicle', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('DelayTravelPerVehicle', IARRAY, FLAG, R2)
        TEMP = GETSTREETLINKMOEDATA('TravelTimePerDistanceTraveled', IARRAY, FLAG, R3)
        IF(R1 .NE. 0.0) RMOE = R3 * R2 / R1
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayTravelPerVehicle') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 40
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelTimePerVehicle', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('MoveTimePerVehicle', IARRAY, FLAG, R2)   
        RMOE = R1 - R2
        IF (RMOE<0) THEN 
            RMOE=0
        ENDIF
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayTravelPerVehicleDiag') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 41
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelTimePerVehicleDiag', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('MoveTimePerVehicleDiag', IARRAY, FLAG, R2)   
        RMOE = R1 - R2
        IF (RMOE<0) THEN 
            RMOE=0
        ENDIF
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayTravelPerVehicleLeft') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 42
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelTimePerVehicleLeft', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('MoveTimePerVehicleLeft', IARRAY, FLAG, R2)   
        RMOE = R1 - R2
        IF (RMOE<0) THEN 
            RMOE=0
        ENDIF
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayTravelPerVehicleRight') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 43
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelTimePerVehicleRight', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('MoveTimePerVehicleRight', IARRAY, FLAG, R2)   
        RMOE = R1 - R2
        IF (RMOE<0) THEN 
            RMOE=0
        ENDIF
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayTravelPerVehicleThrough') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 44
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelTimePerVehicleThrough', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('MoveTimePerVehicleThrough', IARRAY, FLAG, R2)   
        RMOE = R1 - R2
        IF (RMOE<0) THEN 
            RMOE=0
        ENDIF
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayTravelTotal') THEN
 
! ------- MINUTES
 
      MOENUM = 45
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('MoveTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TravelTimeTotal', IARRAY, FLAG, R2)
        RMOE = R2 - R1       
        IF (RMOE<0) THEN 
            RMOE=0
        ENDIF
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayTravelTotalDiag') THEN
 
! ------- MINUTES
 
      MOENUM = 46
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('MoveTimeTotalDiag', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TravelTimeTotalDiag', IARRAY, FLAG, R2)
        RMOE = R2 - R1      
        IF (RMOE<0) THEN 
            RMOE=0
        ENDIF
        
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayTravelTotalLeft') THEN
 
! ------- MINUTES
 
      MOENUM = 47
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('MoveTimeTotalLeft', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TravelTimeTotalLeft', IARRAY, FLAG, R2)
        RMOE = R2 - R1   
        IF (RMOE<0) THEN 
            RMOE=0
        ENDIF
        
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayTravelTotalRight') THEN
 
! ------- MINUTES
 
      MOENUM = 48
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('MoveTimeTotalRight', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TravelTimeTotalRight', IARRAY, FLAG, R2)
        RMOE = R2 - R1      
        IF (RMOE<0) THEN 
            RMOE=0
        ENDIF
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayTravelTotalThrough') THEN
 
! ------- MINUTES
 
      MOENUM = 49
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('MoveTimeTotalThrough', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TravelTimeTotalThrough', IARRAY, FLAG, R2)
        RMOE = R2 - R1        
        
        IF (RMOE<0) THEN 
            RMOE=0
        ENDIF
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DensityPerLane') THEN
 
! ------- VEHICLES/MILE/LANE
 
      MOENUM = 50
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('ContentAverage', IARRAY, FLAG, R1)
        IF(SAREA(IL) .NE. 0) RMOE = R1 / SAREA(IL)
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'EmissionsTotalCO') THEN
        
      MOENUM = 51
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = SCO_TOTAL(IL) / 1000.
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'EmissionsTotalHC') THEN
        
      MOENUM = 52
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = SHC_TOTAL(IL) / 1000.
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'EmissionsTotalNOx') THEN
       
      MOENUM = 53
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = SNOX_TOTAL(IL) / 1000.
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'FuelConsumptionTotal') THEN
       
      MOENUM = 54
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = SFUEL_TOTAL(IL)
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'LaneChangesTotal') THEN
      MOENUM = 55
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = SLANE_CHANGES(IL)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MoveTimePerTravelTimeRatio') THEN
      MOENUM = 56
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('MoveTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TravelTimeTotal', IARRAY, FLAG, R2)
        IF(R2 .NE. 0.0) RMOE = R1 / R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MoveTimePerTravelTimeRatioDiag') THEN
      MOENUM = 57
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('MoveTimeTotalDiag', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TravelTimeTotalDiag', IARRAY, FLAG, R2)
        IF(R2 .NE. 0.0) RMOE = R1 / R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MoveTimePerTravelTimeRatioLeft') THEN     
      MOENUM = 58
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('MoveTimeTotalLeft', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TravelTimeTotalLeft', IARRAY, FLAG, R2)
        IF(R2 .NE. 0.0) RMOE = R1 / R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! --------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MoveTimePerTravelTimeRatioRight') THEN
       
      MOENUM = 59
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('MoveTimeTotalRight', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TravelTimeTotalRight', IARRAY, FLAG, R2)
        IF(R2 .NE. 0.0) RMOE = R1 / R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MoveTimePerTravelTimeRatioThrough') THEN
        
      MOENUM = 60
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('MoveTimeTotalThrough', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TravelTimeTotalThrough', IARRAY, FLAG, R2)
        IF(R2 .NE. 0.0) RMOE = R1 / R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MoveTimePerVehicle') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 61
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelTimePerVehicle', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('MoveTimePerTravelTimeRatio', IARRAY, FLAG, R2)
        RMOE = R1 * R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MoveTimePerVehicleDiag') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 62
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelTimePerVehicleDiag', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('MoveTimePerTravelTimeRatioDiag', IARRAY, FLAG, R2)
        RMOE = R1 * R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MoveTimePerVehicleLeft') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 63
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelTimePerVehicleLeft', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('MoveTimePerTravelTimeRatioLeft', IARRAY, FLAG, R2)
        RMOE = R1 * R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MoveTimePerVehicleRight') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 64
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelTimePerVehicleRight', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('MoveTimePerTravelTimeRatioRight', IARRAY, FLAG, R2)
        RMOE = R1 * R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF          
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MoveTimePerVehicleThrough') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 65
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelTimePerVehicleThrough', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('MoveTimePerTravelTimeRatioThrough', IARRAY, FLAG, R2)
        RMOE = R1 * R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MoveTimeTotal') THEN
 
! ------- MINUTES
 
      MOENUM = 66
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R1) 
        RMOE = R1 / SFREEFLOWSPEED(IL) * 5280. / 60.
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MoveTimeTotalDiag') THEN
 
! ------- MINUTES
 
      MOENUM = 67
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelDistanceTotalDiag', IARRAY, FLAG, R1)
        R1 = R1 * 5280.
        RMOE = R1 / SFREEFLOWSPEED(IL) / 60.
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MoveTimeTotalLeft') THEN
 
! ------- MINUTES
 
      MOENUM = 68
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelDistanceTotalLeft', IARRAY, FLAG, R1)
        R1 = R1 * 5280.
        RMOE = R1 / SFREEFLOWSPEED(IL) / 60.
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MoveTimeTotalRight') THEN
 
! ------- MINUTES
 
      MOENUM = 69
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelDistanceTotalRight', IARRAY, FLAG, R1)
        R1 = R1 * 5280.
        RMOE = R1 / SFREEFLOWSPEED(IL) / 60.
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MoveTimeTotalThrough') THEN
 
! ------- MINUTES
 
      MOENUM = 70
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelDistanceTotalThrough', IARRAY, FLAG, R1)
        R1 = R1 * 5280.
        RMOE = R1 / SFREEFLOWSPEED(IL) / 60.
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF          
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'PersonDelayTotal') THEN
      MOENUM = 71
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('PersonsPerVehicle', IARRAY, FLAG, R1)                                        
        TEMP = GETSTREETLINKMOEDATA('TravelTimeTotal', IARRAY, FLAG, R2)                                         
        TEMP = GETSTREETLINKMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R3)
        R3 = R3 * 5280.
        RMOE = MAX(R2 * R1 - R3 * R1 / (SFREEFLOWSPEED(IL) * 60.0), 0.0)           
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'PersonsPerVehicle') THEN
      MOENUM = 72
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('PersonTripsTotal', IARRAY, FLAG, R1)                                      
        TEMP = GETSTREETLINKMOEDATA('Trips', IARRAY, FLAG, R2)
        IF(R2 .NE. 0) RMOE = R1 / R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'PersonTravelDistanceTotal') THEN
      MOENUM = 73
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('PersonsPerVehicle', IARRAY, FLAG, R1)                                        
        TEMP = GETSTREETLINKMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R2)                                        
        RMOE = R2 * R1                                      
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'PersonTravelTimeTotal') THEN
      MOENUM = 74
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('PersonsPerVehicle', IARRAY, FLAG, R1)                                      
        TEMP = GETSTREETLINKMOEDATA('TravelTimeTotal', IARRAY, FLAG, R2)     
        RMOE = R1 * R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'PersonTripsTotal') THEN
      MOENUM = 75
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = SPERSON_TRIPS(IL)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'PhaseFailuresTotal') THEN
      MOENUM = 76
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = CYCLE_FAILURES(IL)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'SpeedAverage') THEN
 
! ------- MILES/HOUR
 
      MOENUM = 77
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TravelTimeTotal', IARRAY, FLAG, R2)
        R2 = R2 / 60.
        IF(R2 .NE. 0.0) RMOE = R1 / R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'SpeedAverageDiag') THEN
 
! ------- MILES/HOUR
 
      MOENUM = 78
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelDistanceTotalDiag', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TravelTimeTotalDiag', IARRAY, FLAG, R2)
        R2 = R2 / 60.
        IF(R2 .NE. 0.0) RMOE = R1 / R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'SpeedAverageLeft') THEN
 
! ------- MILES/HOUR
 
      MOENUM = 79
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelDistanceTotalLeft', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TravelTimeTotalLeft', IARRAY, FLAG, R2)
        R2 = R2 / 60.
        IF(R2 .NE. 0.0) RMOE = R1 / R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'SpeedAverageRight') THEN
 
! ------- MILES/HOUR
 
      MOENUM = 80
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelDistanceTotalRight', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TravelTimeTotalRight', IARRAY, FLAG, R2)
        R2 = R2 / 60.
        IF(R2 .NE. 0.0) RMOE = R1 / R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'SpeedAverageThrough') THEN
 
! ------- MILES/HOUR
 
      MOENUM = 81
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelDistanceTotalThrough', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TravelTimeTotalThrough', IARRAY, FLAG, R2)
        R2 = R2 / 60.
        IF(R2 .NE. 0.0) RMOE = R1 / R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'StoppedVehicles') THEN
 
! ------- MILES/HOUR
 
      MOENUM = 82
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = STOPPED_VEHICLES(IL)
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'StoppedVehiclesPercent') THEN
 
! ------- MILES/HOUR
 
      MOENUM = 83
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('StoppedVehicles', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('VehiclesDischarged', IARRAY, FLAG, R2)
        IF(R2 .NE. 0.0) RMOE = 100 * R1 / R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'StoragePercent') THEN
      MOENUM = 84
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RNLEN = 0.0
        RVLEN = 0.0
        RTRIPS = 0.0
        IUP = SUSN(IL)
        IDW = SDSN(IL)
        NARRAY(1) = IUP
        NARRAY(2) = IDW
        DO ITYP = 1, NTYPES
          NARRAY(3) = ITYP          
          TEMP = GETVEHICLETYPESTREETLINKMOEDATA('VehiclesDischarged', NARRAY, FLAG, R1)
          RVLEN = RVLEN + R1 * VTLENGTH(ITYP)
          RTRIPS = RTRIPS + R1
        ENDDO
        RLEN = SUM(LANE_LENGTH(IL, 1:TOTAL_LANES(IL)))
        IF(RTRIPS .GT. 0) THEN
          RMVLEN = RVLEN / RTRIPS
          TEMP = GETSTREETLINKMOEDATA('ContentAverage', NARRAY, FLAG, R2)
          RMOE = R2 * RMVLEN * 100.0 / RLEN
        ENDIF
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelDistanceTotal') THEN
 
! ------- MILES
 
      MOENUM = 85
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = SZDIST(IL) / 5280.                                   
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelDistanceTotalDiag') THEN
 
! ------- MILES
 
      MOENUM = 86
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = ZDIST_DIAG(IL) / 5280.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelDistanceTotalLeft') THEN
 
! ------- MILES
 
      MOENUM = 87
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = ZDIST_LEFT(IL) / 5280.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelDistanceTotalRight') THEN
 
! ------- MILES
 
      MOENUM = 88
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = ZDIST_RIGHT(IL) / 5280.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelDistanceTotalThrough') THEN
 
! ------- MILES
 
      MOENUM = 89
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = (SZDIST(IL) - ZDIST_LEFT(IL) - ZDIST_RIGHT(IL) - ZDIST_DIAG(IL)) / 5280.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimePerDistanceTraveled') THEN
 
! ------- MINUTES/MILE
 
      MOENUM = 90
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TravelTimeTotal', IARRAY, FLAG, R2)       
        IF(R1 .NE. 0.0) RMOE = R2 / R1   
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimePerVehicle') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 91
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('SpeedAverage', IARRAY, FLAG, R1)
        IF(R1 .NE. 0.0) RMOE = SLENGTH(IL) / R1 * 3600. / 5280.
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimePerVehicleDiag') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 92
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelDistanceTotalDiag', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TravelTimeTotalDiag', IARRAY, FLAG, R2)
        R2 = R2 / 60.
        IF(R2 .NE. 0.) R1 = R1 / R2
        IF(R1 .NE. 0.) RMOE = SLENGTH(IL) / R1 * 3600. / 5280.
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimePerVehicleLeft') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 93
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelDistanceTotalLeft', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TravelTimeTotalLeft', IARRAY, FLAG, R2)
        R2 = R2 / 60.
        IF(R2 .NE. 0.0) R1 = R1 / R2
        IF(R1 .NE. 0.0) RMOE = SLENGTH(IL) / R1 * 3600. / 5280.
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! --------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimePerVehicleRight') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 94
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelDistanceTotalRight', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TravelTimeTotalRight', IARRAY, FLAG, R2)
        R2 = R2 / 60.
        IF(R2 .NE. 0.0) R1 = R1 / R2
        IF(R1 .NE. 0.0) RMOE = SLENGTH(IL) / R1 * 3600. / 5280.
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimePerVehicleThrough') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 95
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelDistanceTotalThrough', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TravelTimeTotalThrough', IARRAY, FLAG, R2)
        R2 = R2 / 60.
        IF(R2 .NE. 0.0) R1 = R1 / R2
        IF(R1 .NE. 0.0) RMOE = SLENGTH(IL) / R1 * 3600. / 5280.
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimeTotal') THEN
 
! ------- MINUTES
 
      MOENUM = 96
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = SZTIME(IL) / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimeTotalDiag') THEN
 
! ------- MINUTES
 
      MOENUM = 97
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = ZTIME_DIAG(IL) / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimeTotalLeft') THEN
 
! ------- MINUTES
 
      MOENUM = 98
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = ZTIME_LEFT(IL) / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimeTotalRight') THEN
 
! ------- MINUTES
 
      MOENUM = 99
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = ZTIME_RIGHT(IL) / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF          
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimeTotalThrough') THEN
 
! ------- MINUTES
 
      MOENUM = 100
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = (SZTIME(IL) - ZTIME_LEFT(IL) - ZTIME_RIGHT(IL) - ZTIME_DIAG(IL)) / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'Trips') THEN
      MOENUM = 101
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('VehiclesDischarged', IARRAY, FLAG, R1)
        RMOE = R1
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TripsDiag') THEN
      MOENUM = 102
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('VehiclesDischargedDiag', IARRAY, FLAG, R1)
        RMOE = R1
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TripsLeft') THEN
      MOENUM = 103
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('VehiclesDischargedLeft', IARRAY, FLAG, R1)
        RMOE = R1
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TripsRight') THEN
      MOENUM = 104
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('VehiclesDischargedRight', IARRAY, FLAG, R1)
        RMOE = R1
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TripsThrough') THEN
      MOENUM = 105
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('VehiclesDischargedThrough', IARRAY, FLAG, R1)
        RMOE = R1
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'VehiclesDischarged') THEN
 
! ------- VEHICLES
 
      MOENUM = 106
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = SDISCHARGED(IL) 
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'VehiclesDischargedDiag') THEN
 
! ------- VEHICLES
 
      MOENUM = 107
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = DISCHARGED_DIAG(IL)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'VehiclesDischargedLeft') THEN
 
! ------- VEHICLES
 
      MOENUM = 108
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = DISCHARGED_LEFT(IL)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'VehiclesDischargedRight') THEN
 
! ------- VEHICLES
 
      MOENUM = 109
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = DISCHARGED_RIGHT(IL)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'VehiclesDischargedThrough')THEN
 
! ------- VEHICLES
 
      MOENUM = 110
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = SDISCHARGED(IL) - DISCHARGED_LEFT(IL) - DISCHARGED_RIGHT(IL) &
             - DISCHARGED_DIAG(IL)  
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'VehiclesEnteringLink') THEN
 
! ------- VEHICLES
 
      MOENUM = 111
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = SENTERING(IL)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'VolumePerLane') THEN
 
! ------- VEHICLES/HOUR/LANE
 
      MOENUM = 112
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('DensityPerLane', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('SpeedAverage', IARRAY, FLAG, R2)
        RMOE = R1 * R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'Volume') THEN
        
      MOENUM = 113
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('Trips', IARRAY, FLAG, RTRIPS)
        TEMP = GETBASICMOEDATA('SimulationTime', FLAG, R1)
        RMOE = RTRIPS * 3600. / R1
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'RedLightRunnersTotal') THEN
        
      MOENUM = 114
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = RED_LIGHT_RUNNERS(IL)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'VehiclesInDilemmaZoneTotal') THEN
        
      MOENUM = 115
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = VEHICLES_INDZ(IL)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    !These MOEs are provided to calculate existing MOEs without interfering
    !with the operation of the Output Processor
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimePerVehicleX') THEN
 
! ------- SECONDS/VEHICLE
 
      MOENUM = 116
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('SpeedAverageX', IARRAY, FLAG, R1)
        IF(R1 .NE. 0.0) RMOE = SLENGTH(IL) / R1 * 3600. / 5280.
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'SpeedAverageX') THEN
 
! ------- MILES/HOUR
 
      MOENUM = 116
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        TEMP = GETSTREETLINKMOEDATA('TravelDistanceTotalX', IARRAY, FLAG, R1)
        TEMP = GETSTREETLINKMOEDATA('TravelTimeTotalX', IARRAY, FLAG, R2)
        R2 = R2 / 60.
        IF(R2 .NE. 0.0) RMOE = R1 / R2
        SAVED(MOENUM, IL, MDAT) = RMOE
        SAVED(MOENUM, IL, MDAT+1) = RMOE
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelDistanceTotalX') THEN
 
! ------- MILES
 
      MOENUM = 117
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = SZDIST(IL) / 5280.                                   
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimeTotalX') THEN
 
! ------- MINUTES
 
      MOENUM = 118
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, MDAT)
      ELSE
        RMOE = SZTIME(IL) / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, MDAT) = RMOE
          SAVED(MOENUM, IL, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, MTIME) = SIMTIME
      ENDIF 
! ----------------------------------------------------------------------
    ELSE
      GETSTREETLINKMOEDATA = 1
      WRITE(MSGTEXT, '(A)') 'GETSTREETLINKMOEDATA'
      CALL SENDTEXTMSG(3)
      WRITE(MSGTEXT, '(A,A)') 'FAILED TO RECOGNIZE MOE ', MOETYPE
      CALL SENDTEXTMSG(3)
    ENDIF
  ENDIF
10 IF(STATUS .NE. 0) THEN
    CALL SENDTEXTMSG2('MEMORY ALLOCATION FAILED IN FUNCTION: ',  &
                      'GETSTREETLINKMOEDATA', 3)
    GETSTREETLINKMOEDATA = 2
  ENDIF
  END
!
! ==========================================================================================================
! Function: GETNETWORKFREEWAYMOEDATA( char MOETYPE, int IARRAY, int FLAG, float RMOE )
!
! Description: Returns the value of the specified MOE for the subnetwork
!
!
! Arguments: MOETYPE - [in] name of the MOE
!
!            IARRAY  - [in] array of input parameters
!                           (none)
!
!            FLAG    - [in] MOE granularity
!                           0 = cumulative
!                           1 = time interval
!                           2 = time period
!
!            RMOE    - [out] value of the MOE
!
! Returns: int = 0 - success
! Returns: int = 1 - MOE name was not recognized
! ======================================================================
  INTEGER RECURSIVE FUNCTION GETNETWORKFREEWAYMOEDATA &
  [DLLEXPORT, ALIAS:'GetNetworkFreewayMoeData'](MOETYPE, IARRAY, FLAG, RMOE)
 
  USE FREEWAY_LINKS
  USE TEXT
  USE SIMPARAMS
  USE VEHICLE_TYPES
  USE NODE_TABLE
  IMPLICIT NONE
 
  CHARACTER*(*) MOETYPE
  INTEGER     IARRAY  [REFERENCE]
  INTEGER     FLAG    [VALUE]
  REAL        RMOE    [REFERENCE]
  DIMENSION IARRAY(*)
  INTEGER NARRAY(2)
 
  REAL SAVED(:,:)
  ALLOCATABLE SAVED
  SAVE SAVED
  INTEGER :: STATUS, MTIME, MDAT, MOENUM, TEMP
  INTEGER :: IL, IUP, IDW
  INTEGER :: GETBASICMOEDATA
  REAL :: R1, R2, R3, RSAVE
 
  INTERFACE
  INTEGER RECURSIVE FUNCTION GETFREEWAYLINKMOEDATA &
  [DLLEXPORT, ALIAS:'GetFreewayLinkMoeData'] (MOETYPE, IARRAY, FLAG, RMOE)
    CHARACTER*(*) MOETYPE
    INTEGER     IARRAY  [REFERENCE]
    INTEGER     FLAG    [VALUE]
    REAL        RMOE    [REFERENCE]
    DIMENSION IARRAY(*)
  END FUNCTION
  END INTERFACE
 
  STATUS = 0
  GETNETWORKFREEWAYMOEDATA = 0
  IF(MOETYPE .EQ. 'DeallocateMemory') THEN
    IF(ALLOCATED(SAVED)) DEALLOCATE(SAVED)
  ELSE
    IF(.NOT. ALLOCATED(SAVED)) THEN
      ALLOCATE(SAVED(15, 8), STAT = STATUS)
      IF(STATUS .NE. 0) GOTO 10
      SAVED = 0.0
    ENDIF
 
    IF(FLAG .EQ. 0) THEN
      MTIME = 1
      MDAT = 2
    ELSEIF(FLAG .EQ. 1) THEN
      MTIME = 3
      MDAT = 4
    ELSEIF(FLAG .EQ. 2) THEN
      MTIME = 6
      MDAT = 7
    ELSE
      GETNETWORKFREEWAYMOEDATA = 2
      GOTO 10
    ENDIF
 
    RMOE = 0.0
        
    GETNETWORKFREEWAYMOEDATA = 0
! ----------------------------------------------------------------------
    IF(MOETYPE .EQ. 'ContentAverage') THEN
       
      MOENUM = 1
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        R1 = 0.0
        DO IL = 1, N_FREEWAY_LINKS
          IF(NODE_TYPE(FUSN(IL)) .EQ. NT_EXTERN .OR. NODE_TYPE(FDSN(IL)) .EQ. NT_EXTERN) CYCLE
          IUP = FUSN(IL)
          IDW = FDSN(IL)
          NARRAY(1) = IUP
          NARRAY(2) = IDW
          TEMP = GETFREEWAYLINKMOEDATA('TravelTimeTotal', NARRAY, FLAG, R2)
          R1 = R1 + R2
        ENDDO
        TEMP = GETBASICMOEDATA('SimulationTime', FLAG, R3)
        RMOE = 60. * R1 / R3
        SAVED(MOENUM, MDAT) = RMOE
        SAVED(MOENUM, MDAT+1) = RMOE
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'ContentCurrent') THEN
       
      MOENUM = 2
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        R1 = 0.0
        DO IL = 1, N_FREEWAY_LINKS
          IF(NODE_TYPE(FUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(FDSN(IL)) .NE. NT_EXTERN) THEN
            IUP = FUSN(IL)
            IDW = FDSN(IL)
            NARRAY(1) = IUP
            NARRAY(2) = IDW
            TEMP = GETFREEWAYLINKMOEDATA('ContentCurrent', NARRAY, FLAG, R2)
            R1 = R1 + R2
          ENDIF
        ENDDO
        RMOE = R1
        SAVED(MOENUM, MDAT) = RMOE
        SAVED(MOENUM, MDAT+1) = RMOE
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayTravelPerDistanceTraveled') THEN
        
      MOENUM = 3
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        TEMP = GETNETWORKFREEWAYMOEDATA('DelayTravelTotal', IARRAY, FLAG, R1)
        TEMP = GETNETWORKFREEWAYMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R2)
        IF(R2 .GT. 0) RMOE = 60. * R1 / R2
        SAVED(MOENUM, MDAT) = RMOE
        SAVED(MOENUM, MDAT+1) = RMOE
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayTravelTotal') THEN
       
      MOENUM = 4
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        R1 = 0.0
        DO IL = 1, N_FREEWAY_LINKS
          IF(NODE_TYPE(FUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(FDSN(IL)) .NE. NT_EXTERN) THEN
            IUP = FUSN(IL)
            IDW = FDSN(IL)
            NARRAY(1) = IUP
            NARRAY(2) = IDW
            TEMP = GETFREEWAYLINKMOEDATA('DelayTravelTotal', NARRAY, FLAG, R2)
            R1 = R1 + R2
          ENDIF
        ENDDO
        RMOE = R1 / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, MDAT) = RMOE
          SAVED(MOENUM, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'EmissionsTotalCO') THEN
       
      MOENUM = 5
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        R1 = 0.0
        DO IL = 1, N_FREEWAY_LINKS
          IF(NODE_TYPE(FUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(FDSN(IL)) .NE. NT_EXTERN) THEN
            IUP = FUSN(IL)
            IDW = FDSN(IL)
            NARRAY(1) = IUP
            NARRAY(2) = IDW
            TEMP = GETFREEWAYLINKMOEDATA('EmissionsTotalCO', NARRAY, FLAG, R2)
            R1 = R1 + R2
          ENDIF
        ENDDO
        RMOE = R1
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, MDAT) = RMOE
          SAVED(MOENUM, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'EmissionsTotalHC') THEN
        
      MOENUM = 6
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        R1 = 0.0
        DO IL = 1, N_FREEWAY_LINKS
          IF(NODE_TYPE(FUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(FDSN(IL)) .NE. NT_EXTERN) THEN
            IUP = FUSN(IL)
            IDW = FDSN(IL)
            NARRAY(1) = IUP
            NARRAY(2) = IDW
            TEMP = GETFREEWAYLINKMOEDATA('EmissionsTotalHC', NARRAY, FLAG, R2)
            R1 = R1 + R2
          ENDIF
        ENDDO
        RMOE = R1
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, MDAT) = RMOE
          SAVED(MOENUM, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'EmissionsTotalNOx') THEN
       
      MOENUM = 7
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        R1 = 0.0
        DO IL = 1, N_FREEWAY_LINKS
          IF(NODE_TYPE(FUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(FDSN(IL)) .NE. NT_EXTERN) THEN
            IUP = FUSN(IL)
            IDW = FDSN(IL)
            NARRAY(1) = IUP
            NARRAY(2) = IDW
            TEMP = GETFREEWAYLINKMOEDATA('EmissionsTotalNOx', NARRAY, FLAG, R2)
            R1 = R1 + R2
          ENDIF
        ENDDO
        RMOE = R1
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, MDAT) = RMOE
          SAVED(MOENUM, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'FuelConsumptionTotal') THEN
       
      MOENUM = 8
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        R1 = 0.0
        DO IL = 1, N_FREEWAY_LINKS
          IF(NODE_TYPE(FUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(FDSN(IL)) .NE. NT_EXTERN) THEN
            IUP = FUSN(IL)
            IDW = FDSN(IL)
            NARRAY(1) = IUP
            NARRAY(2) = IDW
            TEMP = GETFREEWAYLINKMOEDATA('FuelConsumptionTotal', NARRAY, FLAG, R2)
            R1 = R1 + R2
          ENDIF
        ENDDO
        RMOE = R1
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, MDAT) = RMOE
          SAVED(MOENUM, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'LaneChangesTotal') THEN
        
      MOENUM = 9
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        R2 = 0
        DO IL = 1, N_FREEWAY_LINKS
          IF(NODE_TYPE(FUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(FDSN(IL)) .NE. NT_EXTERN) THEN
            IUP = FUSN(IL)
            IDW = FDSN(IL)
            NARRAY(1) = IUP
            NARRAY(2) = IDW
            TEMP = GETFREEWAYLINKMOEDATA('LaneChangesTotal', NARRAY, FLAG, R1)
            R2 = R2 + R1
          ENDIF
        ENDDO
        RMOE = R2
        SAVED(MOENUM, MDAT) = RMOE
        SAVED(MOENUM, MDAT+1) = RMOE
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MoveTimePerTravelTimeRatio') THEN
        
      MOENUM = 10
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        TEMP = GETNETWORKFREEWAYMOEDATA('TravelTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETNETWORKFREEWAYMOEDATA('DelayTravelTotal', IARRAY, FLAG, R2)
        IF(R1 .GT. 0) RMOE = (R1 - R2) / R1
        SAVED(MOENUM, MDAT) = RMOE
        SAVED(MOENUM, MDAT+1) = RMOE
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MoveTimeTotal') THEN
        
      MOENUM = 11
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        TEMP = GETNETWORKFREEWAYMOEDATA('TravelTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETNETWORKFREEWAYMOEDATA('DelayTravelTotal', IARRAY, FLAG, R2)
        IF(R1 .GT. 0) RMOE = R1 - R2
        SAVED(MOENUM, MDAT) = RMOE
        SAVED(MOENUM, MDAT+1) = RMOE
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
          
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'SpeedAverage') THEN
        
      MOENUM = 12
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        TEMP = GETNETWORKFREEWAYMOEDATA('TravelTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETNETWORKFREEWAYMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R2)
        IF(R1 .GT. 0) RMOE = R2 / R1
        SAVED(MOENUM, MDAT) = RMOE
        SAVED(MOENUM, MDAT+1) = RMOE
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelDistanceTotal') THEN
       
      MOENUM = 13
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        R2 = 0.0
        DO IL = 1, N_FREEWAY_LINKS
          IF(NODE_TYPE(FUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(FDSN(IL)) .NE. NT_EXTERN) THEN
            R1 = FZDIST(IL) / 5280.
            R2 = R2 + R1
          ENDIF
        ENDDO
        RMOE = R2
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, MDAT) = RMOE
          SAVED(MOENUM, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimePerDistanceTraveled') THEN
       
      MOENUM = 14
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        TEMP = GETNETWORKFREEWAYMOEDATA('TravelTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETNETWORKFREEWAYMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R2)
        IF(R2 .GT. 0) RMOE = 60. * R1 / R2
        SAVED(MOENUM, MDAT) = RMOE
        SAVED(MOENUM, MDAT+1) = RMOE
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimeTotal') THEN
 
! ------- HOURS
 
      MOENUM = 15
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        R2 = 0.
        DO IL = 1, N_FREEWAY_LINKS
          IF(NODE_TYPE(FUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(FDSN(IL)) .NE. NT_EXTERN) THEN
            IUP = FUSN(IL)
            IDW = FDSN(IL)
            NARRAY(1) = IUP
            NARRAY(2) = IDW
            TEMP = GETFREEWAYLINKMOEDATA('TravelTimeTotal', NARRAY, FLAG, R1)
            R2 = R2 + R1
          ENDIF
        ENDDO
        RMOE = R2 / 60.
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, MDAT) = RMOE
        SAVED(MOENUM, MDAT+1) = RMOE
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSE
      GETNETWORKFREEWAYMOEDATA = 1
      msgtext = moetype            
      call sendtextmsg(3)
    ENDIF
  ENDIF
10 IF(STATUS .NE. 0) THEN
    CALL SENDTEXTMSG2('MEMORY ALLOCATION FAILED IN FUNCTION: ',  &
                      'GETNETWORKFREEWAYMOEDATA', 3)
    GETNETWORKFREEWAYMOEDATA = 2
  ENDIF
  END
!
! ==========================================================================================================
! Function: GETNETWORKSTREETMOEDATA( char MOETYPE, int IARRAY, int FLAG, float RMOE )
!
! Description: Returns the value of the specified MOE for the subnetwork
!
!
! Arguments: MOETYPE - [in] name of the MOE
!
!            IARRAY  - [in] array of input parameters
!                           (none)
!
!            FLAG    - [in] MOE granularity
!                           0 = cumulative
!                           1 = time interval
!                           2 = time period
!
!            RMOE    - [out] value of the MOE
!
! Returns: int = 0 - success
! Returns: int = 1 - MOE name was not recognized
! ======================================================================
  INTEGER RECURSIVE FUNCTION GETNETWORKSTREETMOEDATA &
  [DLLEXPORT, ALIAS:'GetNetworkStreetMoeData'](MOETYPE, IARRAY, FLAG, RMOE)
 
  USE STREET_LINKS
  USE STREET_VEHICLES
  USE TEXT
  USE SIMPARAMS
  USE VEHICLE_TYPES
  USE NODE_TABLE
  IMPLICIT NONE
 
  CHARACTER*(*) MOETYPE
  INTEGER     IARRAY  [REFERENCE]
  INTEGER     FLAG    [VALUE]
  REAL        RMOE    [REFERENCE]
  DIMENSION IARRAY(*)
  INTEGER NARRAY(2), KARRAY(3)
 
  REAL SAVED(:,:)
  ALLOCATABLE SAVED
  SAVE SAVED
  INTEGER :: STATUS, MTIME, MDAT, MOENUM, TEMP
  INTEGER :: IL, IUP, IDW, ITYP
  INTEGER :: GETBASICMOEDATA
  REAL :: R1, R2, R3, RSAVE
  REAL :: RNLEN, RVLEN, RNTRIPS, RLEN, RMVLEN
 
  INTERFACE
  INTEGER RECURSIVE FUNCTION GETSTREETLINKMOEDATA &
  [DLLEXPORT,ALIAS:'GetStreetLinkMoeData'] (MOETYPE, IARRAY, FLAG, RMOE)
    CHARACTER*(*) MOETYPE
    INTEGER     IARRAY  [REFERENCE]
    INTEGER     FLAG    [VALUE]
    REAL        RMOE    [REFERENCE]
    DIMENSION IARRAY(*)
  END FUNCTION
  INTEGER RECURSIVE FUNCTION GETVEHICLETYPESTREETLINKMOEDATA &
  [DLLEXPORT,ALIAS:'GetVehicleTypeStreetLinkMoeData'] (MOETYPE, IARRAY, FLAG, RMOE)
    CHARACTER*(*) MOETYPE
    INTEGER     IARRAY  [REFERENCE]
    INTEGER     FLAG    [VALUE]
    REAL        RMOE    [REFERENCE]
    DIMENSION IARRAY(*)
  END FUNCTION
  END INTERFACE
 
  STATUS = 0
  GETNETWORKSTREETMOEDATA = 0
  IF(MOETYPE .EQ. 'DeallocateMemory') THEN
    IF(ALLOCATED(SAVED)) DEALLOCATE(SAVED)
  ELSE
    IF(.NOT. ALLOCATED(SAVED)) THEN
      ALLOCATE(SAVED(21, 8), STAT = STATUS)
      IF(STATUS .NE. 0) GOTO 10
      SAVED = 0.0
    ENDIF
 
    IF(FLAG .EQ. 0) THEN
      MTIME = 1
      MDAT = 2
    ELSEIF(FLAG .EQ. 1) THEN
      MTIME = 3
      MDAT = 4
    ELSEIF(FLAG .EQ. 2) THEN
      MTIME = 6
      MDAT = 7
    ELSE
      GETNETWORKSTREETMOEDATA = 2
      GOTO 10
    ENDIF
 
    RMOE = 0.0
        
! ----------------------------------------------------------------------
    IF(MOETYPE .EQ. 'ContentAverage') THEN
        
      MOENUM = 1
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        R1 = 0.0
        DO IL = 1, N_STREET_LINKS
          IF(NODE_TYPE(SUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(SDSN(IL)) .NE. NT_EXTERN) THEN
            IUP = SUSN(IL)
            IDW = SDSN(IL)
            NARRAY(1) = IUP
            NARRAY(2) = IDW
            TEMP = GETSTREETLINKMOEDATA('TravelTimeTotal', NARRAY, FLAG, R2)
            R1 = R1 + R2
          ENDIF
        ENDDO
        TEMP = GETBASICMOEDATA('SimulationTime', FLAG, R3)
        RMOE = 60. * R1 / R3
        SAVED(MOENUM, MDAT) = RMOE
        SAVED(MOENUM, MDAT+1) = RMOE
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'ContentCurrent') THEN
        
      MOENUM = 2
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        R1 = 0.0
        DO IL = 1, N_STREET_LINKS
          IF(NODE_TYPE(SUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(SDSN(IL)) .NE. NT_EXTERN) THEN
            IUP = SUSN(IL)
            IDW = SDSN(IL)
            NARRAY(1) = IUP
            NARRAY(2) = IDW
            TEMP = GETSTREETLINKMOEDATA('ContentCurrent', NARRAY, FLAG, R2)
            R1 = R1 + R2
          ENDIF
        ENDDO
        RMOE = R1
        SAVED(MOENUM, MDAT) = RMOE
        SAVED(MOENUM, MDAT+1) = RMOE
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayQueueTotal') THEN
      MOENUM = 3
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        R1 = 0.0
        DO IL = 1, N_STREET_LINKS
          IF(NODE_TYPE(SUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(SDSN(IL)) .NE. NT_EXTERN) THEN
            IUP = SUSN(IL)
            IDW = SDSN(IL)
            NARRAY(1) = IUP
            NARRAY(2) = IDW
            TEMP = GETSTREETLINKMOEDATA('DelayQueueTotal', NARRAY, FLAG, R2)
            R1 = R1 + R2
          ENDIF
        ENDDO
        RMOE = R1 / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, MDAT) = RMOE
          SAVED(MOENUM, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayStopTotal') THEN
      MOENUM = 4
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        R1 = 0.0
        DO IL = 1, N_STREET_LINKS
          IF(NODE_TYPE(SUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(SDSN(IL)) .NE. NT_EXTERN) THEN
            IUP = SUSN(IL)
            IDW = SDSN(IL)
            NARRAY(1) = IUP
            NARRAY(2) = IDW
            TEMP = GETSTREETLINKMOEDATA('DelayStopTotal', NARRAY, FLAG, R2)
            R1 = R1 + R2
          ENDIF
        ENDDO
        RMOE = R1 / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, MDAT) = RMOE
          SAVED(MOENUM, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayTravelPerDistanceTraveled') THEN
        
      MOENUM = 5
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        TEMP = GETNETWORKSTREETMOEDATA('DelayTravelTotal', IARRAY, FLAG, R1)
        TEMP = GETNETWORKSTREETMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R2)
        IF(R2 .GT. 0) RMOE = 60. * R1 / R2
        SAVED(MOENUM, MDAT) = RMOE
        SAVED(MOENUM, MDAT+1) = RMOE
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'DelayTravelTotal') THEN
        
      MOENUM = 6
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        R1 = 0.0
        DO IL = 1, N_STREET_LINKS
          IF(NODE_TYPE(SUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(SDSN(IL)) .NE. NT_EXTERN) THEN
            IUP = SUSN(IL)
            IDW = SDSN(IL)
            NARRAY(1) = IUP
            NARRAY(2) = IDW
            TEMP = GETSTREETLINKMOEDATA('DelayTravelTotal', NARRAY, FLAG, R2)
            R1 = R1 + R2
          ENDIF
        ENDDO
        RMOE = R1 / 60.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, MDAT) = RMOE
          SAVED(MOENUM, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'EmissionsTotalCO') THEN
        
      MOENUM = 7
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        R1 = 0.0
        DO IL = 1, N_STREET_LINKS
          IF(NODE_TYPE(SUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(SDSN(IL)) .NE. NT_EXTERN) THEN
            IUP = SUSN(IL)
            IDW = SDSN(IL)
            NARRAY(1) = IUP
            NARRAY(2) = IDW
            TEMP = GETSTREETLINKMOEDATA('EmissionsTotalCO', NARRAY, FLAG, R2)
            R1 = R1 + R2
          ENDIF
        ENDDO
        RMOE = R1
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, MDAT) = RMOE
          SAVED(MOENUM, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'EmissionsTotalHC') THEN
        
      MOENUM = 8
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        R1 = 0.0
        DO IL = 1, N_STREET_LINKS
          IF(NODE_TYPE(SUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(SDSN(IL)) .NE. NT_EXTERN) THEN
            IUP = SUSN(IL)
            IDW = SDSN(IL)
            NARRAY(1) = IUP
            NARRAY(2) = IDW
            TEMP = GETSTREETLINKMOEDATA('EmissionsTotalHC', NARRAY, FLAG, R2)
            R1 = R1 + R2
          ENDIF
        ENDDO
        RMOE = R1
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, MDAT) = RMOE
          SAVED(MOENUM, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'EmissionsTotalNOx') THEN
        
      MOENUM = 9
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        R1 = 0.0
        DO IL = 1, N_STREET_LINKS
          IF(NODE_TYPE(SUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(SDSN(IL)) .NE. NT_EXTERN) THEN
            IUP = SUSN(IL)
            IDW = SDSN(IL)
            NARRAY(1) = IUP
            NARRAY(2) = IDW
            TEMP = GETSTREETLINKMOEDATA('EmissionsTotalNOx', NARRAY, FLAG, R2)
            R1 = R1 + R2
          ENDIF
        ENDDO
        RMOE = R1
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, MDAT) = RMOE
          SAVED(MOENUM, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'FuelConsumptionTotal') THEN
        
      MOENUM = 10
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        R1 = 0.0
        DO IL = 1, N_STREET_LINKS
          IF(NODE_TYPE(SUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(SDSN(IL)) .NE. NT_EXTERN) THEN
            IUP = SUSN(IL)
            IDW = SDSN(IL)
            NARRAY(1) = IUP
            NARRAY(2) = IDW
            TEMP = GETSTREETLINKMOEDATA('FuelConsumptionTotal', NARRAY, FLAG, R2)
            R1 = R1 + R2
          ENDIF
        ENDDO
        RMOE = R1
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, MDAT) = RMOE
          SAVED(MOENUM, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'LaneChangesTotal') THEN
        
      MOENUM = 11
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        R2 = 0
        DO IL = 1, N_STREET_LINKS
          IF(NODE_TYPE(SUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(SDSN(IL)) .NE. NT_EXTERN) THEN
            IUP = SUSN(IL)
            IDW = SDSN(IL)
            NARRAY(1) = IUP
            NARRAY(2) = IDW
            TEMP = GETSTREETLINKMOEDATA('LaneChangesTotal', NARRAY, FLAG, R1)
            R2 = R2 + R1
          ENDIF
        ENDDO
        RMOE = R2
        SAVED(MOENUM, MDAT) = RMOE
        SAVED(MOENUM, MDAT+1) = RMOE
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MoveTimePerTravelTimeRatio') THEN
       
      MOENUM = 12
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        TEMP = GETNETWORKSTREETMOEDATA('TravelTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETNETWORKSTREETMOEDATA('DelayTravelTotal', IARRAY, FLAG, R2)
        IF(R1 .GT. 0) RMOE = (R1 - R2) / R1
        SAVED(MOENUM, MDAT) = RMOE
        SAVED(MOENUM, MDAT+1) = RMOE
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'MoveTimeTotal') THEN
        
      MOENUM = 13
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        TEMP = GETNETWORKSTREETMOEDATA('TravelTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETNETWORKSTREETMOEDATA('DelayTravelTotal', IARRAY, FLAG, R2)
        IF(R1 .GT. 0) RMOE = R1 - R2
        SAVED(MOENUM, MDAT) = RMOE
        SAVED(MOENUM, MDAT+1) = RMOE
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'PhaseFailuresTotal') THEN
        
      MOENUM = 14
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        R2 = 0
        DO IL = 1, N_STREET_LINKS
          IF(NODE_TYPE(SUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(SDSN(IL)) .NE. NT_EXTERN) THEN
            IUP = SUSN(IL)
            IDW = SDSN(IL)
            NARRAY(1) = IUP
            NARRAY(2) = IDW
            TEMP = GETSTREETLINKMOEDATA('PhaseFailuresTotal', NARRAY, FLAG, R1)
            R2 = R2 + R1
          ENDIF
        ENDDO
        RMOE = R2
        SAVED(MOENUM, MDAT) = RMOE
        SAVED(MOENUM, MDAT+1) = RMOE
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'SpeedAverage') THEN
       
      MOENUM = 15
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        TEMP = GETNETWORKSTREETMOEDATA('TravelTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETNETWORKSTREETMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R2)
        IF(R1 .GT. 0) RMOE = R2 / R1
        SAVED(MOENUM, MDAT) = RMOE
        SAVED(MOENUM, MDAT+1) = RMOE
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'StoppedVehiclesPercent') THEN
        
      MOENUM = 16
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        R3 = 0.
        TEMP = GETNETWORKSTREETMOEDATA('Trips', IARRAY, FLAG, R1)
        IF(R1 .GT. 0) THEN
          DO IL = 1, N_STREET_LINKS
            IF(NODE_TYPE(SUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(SDSN(IL)) .NE. NT_EXTERN) THEN
              IUP = SUSN(IL)
              IDW = SDSN(IL)
              NARRAY(1) = IUP
              NARRAY(2) = IDW
              TEMP = GETSTREETLINKMOEDATA('StoppedVehicles', NARRAY, FLAG, R2)
              R3 = R3 + R2
            ENDIF
          ENDDO
          RMOE = 100 * R3 / R1
        ENDIF
        SAVED(MOENUM, MDAT) = RMOE
        SAVED(MOENUM, MDAT+1) = RMOE
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'StoragePercent') THEN
        
      MOENUM = 17
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        RNLEN = 0.0
        RVLEN = 0.0
        RNTRIPS = 0.0
        DO IL = 1, N_STREET_LINKS
          IF(NODE_TYPE(SUSN(IL)) .EQ. NT_EXTERN) CYCLE
          IUP = SUSN(IL)
          IDW = SDSN(IL)
          KARRAY(1) = IUP
          KARRAY(2) = IDW
          DO ITYP = 1, NTYPES
            KARRAY(3) = ITYP          
            TEMP = GETVEHICLETYPESTREETLINKMOEDATA('VehiclesDischarged', KARRAY, FLAG, R1)
            RVLEN = RVLEN + R1 * VTLENGTH(ITYP)
            RNTRIPS = RNTRIPS + R1
          ENDDO
          RLEN = SUM(LANE_LENGTH(IL, 1:TOTAL_LANES(IL)))
          RNLEN = RNLEN + RLEN
        ENDDO
        IF(RNTRIPS .GT. 0) THEN
          RMVLEN = RVLEN / RNTRIPS
          TEMP = GETNETWORKSTREETMOEDATA('ContentAverage', NARRAY, FLAG, R2)
          RMOE = R2 * RMVLEN * 100.0 / RNLEN
        ENDIF
        SAVED(MOENUM, MDAT) = RMOE
        SAVED(MOENUM, MDAT+1) = RMOE
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelDistanceTotal') THEN
       
      MOENUM = 18
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        R2 = 0.0
        DO IL = 1, N_STREET_LINKS
          IF(NODE_TYPE(SUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(SDSN(IL)) .NE. NT_EXTERN) THEN
            R1 = SZDIST(IL) / 5280.
            R2 = R2 + R1
          ENDIF
        ENDDO
        RMOE = R2
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, MDAT) = RMOE
          SAVED(MOENUM, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimePerDistanceTraveled') THEN
        
      MOENUM = 19
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        TEMP = GETNETWORKSTREETMOEDATA('TravelTimeTotal', IARRAY, FLAG, R1)
        TEMP = GETNETWORKSTREETMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R2)
        IF(R2 .GT. 0) RMOE = 60. * R1 / R2
        SAVED(MOENUM, MDAT) = RMOE
        SAVED(MOENUM, MDAT+1) = RMOE
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimeTotal') THEN
 
! ------- HOURS
 
      MOENUM = 20
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        R2 = 0.
        DO IL = 1, N_STREET_LINKS
          IF(NODE_TYPE(SUSN(IL)) .NE. NT_EXTERN .AND. NODE_TYPE(SDSN(IL)) .NE. NT_EXTERN) CYCLE
          IUP = SUSN(IL)
          IDW = SDSN(IL)
          NARRAY(1) = IUP
          NARRAY(2) = IDW
          TEMP = GETSTREETLINKMOEDATA('TravelTimeTotal', NARRAY, FLAG, R1)
          R2 = R2 + R1
        ENDDO
        RMOE = R2 / 60.
        IF((RMOE) .LT. 1E-5) RMOE = 0.0
        SAVED(MOENUM, MDAT) = RMOE
        SAVED(MOENUM, MDAT+1) = RMOE
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'Trips') THEN
        
      MOENUM = 21
      IF(SIMTIME .EQ. SAVED(MOENUM, MTIME)) THEN
        RMOE = SAVED(MOENUM, MDAT)
      ELSE
        RMOE = EXITING_VEHICLES
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, MDAT) = RMOE
          SAVED(MOENUM, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSE
      GETNETWORKSTREETMOEDATA = 1
      msgtext = moetype            
      call sendtextmsg(3)
    ENDIF
  ENDIF
10 IF(STATUS .NE. 0) THEN
    CALL SENDTEXTMSG2('MEMORY ALLOCATION FAILED IN FUNCTION: ',  &
                      'GETNETWORKSTREETMOEDATA', 3)
    GETNETWORKSTREETMOEDATA = 2
  ENDIF
  END
!
! ==========================================================================================================
! Function: GETVEHICLETYPEFREEWAYLINKMOEDATA( char MOETYPE, int IARRAY, int FLAG, float RMOE )
!
! Description: Returns the value of the specified MOE for the vehicle type
!              on the link specified by up node and down node
!
!
! Arguments: MOETYPE - [in] name of the MOE
!
!            IARRAY  - [in] array of input parameters
!                           (up node, down node, vehicle type)
!
!            FLAG    - [in] MOE granularity
!                           0 = cumulative
!                           1 = time interval
!                           2 = time period
!
!            RMOE    - [out] value of the MOE
!
! Returns: int = 0 - success
! Returns: int = 1 - MOE name was not recognized or link was not found
! ======================================================================
  INTEGER RECURSIVE FUNCTION GETVEHICLETYPEFREEWAYLINKMOEDATA &
  [DLLEXPORT, ALIAS:'GetVehicleTypeFreewayLinkMoeData'](MOETYPE, IARRAY, FLAG, RMOE)
 
  USE FREEWAY_LINKS
  USE TEXT
  USE SIMPARAMS
  USE VEHICLE_TYPES
  IMPLICIT NONE
 
  CHARACTER*(*) MOETYPE
  INTEGER     IARRAY  [REFERENCE]
  INTEGER     FLAG    [VALUE]
  REAL        RMOE    [REFERENCE]
  DIMENSION IARRAY(*)
 
  INTEGER :: STATUS, MTIME, MDAT, MOENUM, TEMP
  INTEGER :: IL, IUP, IDW, ITYP
  REAL :: R1, R2, RSAVE
 
  REAL SAVED(:,:,:,:)
  ALLOCATABLE SAVED
  SAVE SAVED
 
  STATUS = 0
  GETVEHICLETYPEFREEWAYLINKMOEDATA = 0
  IF(MOETYPE .EQ. 'DeallocateMemory') THEN
    IF(ALLOCATED(SAVED)) DEALLOCATE(SAVED)
  ELSE
    IF(.NOT. ALLOCATED(SAVED)) THEN
      ALLOCATE(SAVED(5, N_FREEWAY_LINKS, NTYPES, 8), STAT = STATUS)
      IF(STATUS .NE. 0) GOTO 10
      SAVED = 0.0
    ENDIF
 
    IF(FLAG .EQ. 0) THEN
      MTIME = 1
      MDAT = 2
    ELSEIF(FLAG .EQ. 1) THEN
      MTIME = 3
      MDAT = 4
    ELSEIF(FLAG .EQ. 2) THEN
      MTIME = 6
      MDAT = 7
    ELSE
      GETVEHICLETYPEFREEWAYLINKMOEDATA = 2
      GOTO 10
    ENDIF
        
    IUP = IARRAY(1)
    IDW = IARRAY(2)
    ITYP = IARRAY(3)
   
    CALL FIND_FREEWAY_LINK(IUP, IDW, IL)
   
    RMOE = 0.0
    GETVEHICLETYPEFREEWAYLINKMOEDATA = 0
        
    IF(IL .EQ. 0) THEN
      GETVEHICLETYPEFREEWAYLINKMOEDATA = 1
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'Distance') THEN
              
      MOENUM = 1
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ITYP, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ITYP, MDAT)
      ELSE
        RMOE = FZDIST_TYPE(IL, ITYP)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, ITYP, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, ITYP, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, ITYP, MDAT) = RMOE
          SAVED(MOENUM, IL, ITYP, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, ITYP, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'SpeedAverage') THEN
        
      MOENUM = 2
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ITYP, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ITYP, MDAT)
      ELSE
        TEMP = GETVEHICLETYPEFREEWAYLINKMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R1)
        TEMP = GETVEHICLETYPEFREEWAYLINKMOEDATA('TravelTimeTotal', IARRAY, FLAG, R2)
        IF(R2 .GT. 0) RMOE = R1 / R2
        SAVED(MOENUM, IL, ITYP, MDAT) = RMOE
        SAVED(MOENUM, IL, ITYP, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ITYP, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelDistanceTotal') THEN
      MOENUM = 3
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ITYP, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ITYP, MDAT)
      ELSE
        RMOE = FZDIST_TYPE(IL, ITYP) / 5280.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, ITYP, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, ITYP, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, ITYP, MDAT) = RMOE
          SAVED(MOENUM, IL, ITYP, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, ITYP, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimeTotal') THEN
      MOENUM = 4
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ITYP, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ITYP, MDAT)
      ELSE
        RMOE = FZTIME_TYPE(IL, ITYP) / 3600.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, ITYP, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, ITYP, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, ITYP, MDAT) = RMOE
          SAVED(MOENUM, IL, ITYP, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, ITYP, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'VehiclesDischarged') THEN
      MOENUM = 5
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ITYP, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ITYP, MDAT)
      ELSE
        RMOE = FDISCHARGED_TYPE(IL, ITYP)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, ITYP, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, ITYP, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, ITYP, MDAT) = RMOE
          SAVED(MOENUM, IL, ITYP, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, ITYP, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSE
      GETVEHICLETYPEFREEWAYLINKMOEDATA = 1
      msgtext = moetype            
      call sendtextmsg(3)
    ENDIF
  ENDIF
10 IF(STATUS .NE. 0) THEN
    CALL SENDTEXTMSG2('MEMORY ALLOCATION FAILED IN FUNCTION: ', &
                      'GETVEHICLETYPEFREEWAYLINKMOEDATA', 3)
    GETVEHICLETYPEFREEWAYLINKMOEDATA = 2
  ENDIF
  END
!
! ==========================================================================================================
! Function: GETVEHICLETYPESTREETLINKMOEDATA( char MOETYPE, int IARRAY, int FLAG, float RMOE )
!
! Description: Returns the value of the specified MOE for the vehicle type
!              on the link specified by up node and down node
!
!
! Arguments: MOETYPE - [in] name of the MOE
!
!            IARRAY  - [in] array of input parameters
!                           (up node, down node, vehicle type)
!
!            FLAG    - [in] MOE granularity
!                           0 = cumulative
!                           1 = time interval
!                           2 = time period
!
!            RMOE    - [out] value of the MOE
!
! Returns: int = 0 - success
! Returns: int = 1 - MOE name was not recognized or link was not found
! ======================================================================
  INTEGER RECURSIVE FUNCTION GETVEHICLETYPESTREETLINKMOEDATA &
  [DLLEXPORT, ALIAS:'GetVehicleTypeStreetLinkMoeData'](MOETYPE, IARRAY, FLAG, RMOE)
 
  USE STREET_LINKS
  USE TEXT
  USE SIMPARAMS
  USE VEHICLE_TYPES
  IMPLICIT NONE
 
  CHARACTER*(*) MOETYPE
  INTEGER     IARRAY  [REFERENCE]
  INTEGER     FLAG    [VALUE]
  REAL        RMOE    [REFERENCE]
  DIMENSION IARRAY(*)
 
  INTEGER :: STATUS, MTIME, MDAT, MOENUM, TEMP
  INTEGER :: IL, IUP, IDW, ITYP
  REAL :: R1, R2, RSAVE
 
  REAL SAVED(:,:,:,:)
  ALLOCATABLE SAVED
  SAVE SAVED
 
  STATUS = 0
  GETVEHICLETYPESTREETLINKMOEDATA = 0
  IF(MOETYPE .EQ. 'DeallocateMemory') THEN
    IF(ALLOCATED(SAVED)) DEALLOCATE(SAVED)
  ELSE
    IF(.NOT. ALLOCATED(SAVED)) THEN
      ALLOCATE(SAVED(5, N_STREET_LINKS, NTYPES, 8), STAT = STATUS)
      IF(STATUS .NE. 0) GOTO 10
      SAVED = 0.0
    ENDIF
 
    IF(FLAG .EQ. 0) THEN
      MTIME = 1
      MDAT = 2
    ELSEIF(FLAG .EQ. 1) THEN
      MTIME = 3
      MDAT = 4
    ELSEIF(FLAG .EQ. 2) THEN
      MTIME = 6
      MDAT = 7
    ELSE
      GETVEHICLETYPESTREETLINKMOEDATA = 2
      GOTO 10
    ENDIF
        
    IUP = IARRAY(1)
    IDW = IARRAY(2)
    ITYP = IARRAY(3)
   
    CALL FIND_STREET_LINK(IUP, IDW, IL)
   
    RMOE = 0.0
    GETVEHICLETYPESTREETLINKMOEDATA = 0
        
    IF(IL .EQ. 0) THEN
      GETVEHICLETYPESTREETLINKMOEDATA = 1
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'Distance') THEN
             
      MOENUM = 1
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ITYP, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ITYP, MDAT)
      ELSE
        RMOE = SZDIST_TYPE(IL, ITYP)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, ITYP, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, ITYP, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, ITYP, MDAT) = RMOE
          SAVED(MOENUM, IL, ITYP, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, ITYP, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'SpeedAverage') THEN
        
      MOENUM = 2
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ITYP, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ITYP, MDAT)
      ELSE
        TEMP = GETVEHICLETYPESTREETLINKMOEDATA('TravelDistanceTotal', IARRAY, FLAG, R1)
        TEMP = GETVEHICLETYPESTREETLINKMOEDATA('TravelTimeTotal', IARRAY, FLAG, R2)
        IF(R2 .GT. 0) RMOE = R1 / R2
        SAVED(MOENUM, IL, ITYP, MDAT) = RMOE
        SAVED(MOENUM, IL, ITYP, MDAT+1) = RMOE
        SAVED(MOENUM, IL, ITYP, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelDistanceTotal') THEN
      MOENUM = 3
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ITYP, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ITYP, MDAT)
      ELSE
        RMOE = SZDIST_TYPE(IL, ITYP) / 5280.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, ITYP, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, ITYP, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, ITYP, MDAT) = RMOE
          SAVED(MOENUM, IL, ITYP, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, ITYP, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'TravelTimeTotal') THEN
      MOENUM = 4
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ITYP, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ITYP, MDAT)
      ELSE
        RMOE = SZTIME_TYPE(IL, ITYP) / 3600.
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, ITYP, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, ITYP, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, ITYP, MDAT) = RMOE
          SAVED(MOENUM, IL, ITYP, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, ITYP, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'VehiclesDischarged') THEN
      MOENUM = 5
      IF(SIMTIME .EQ. SAVED(MOENUM, IL, ITYP, MTIME)) THEN
        RMOE = SAVED(MOENUM, IL, ITYP, MDAT)
      ELSE
        RMOE = SDISCHARGED_TYPE(IL, ITYP)
        IF(FLAG .EQ. 0) THEN
          SAVED(MOENUM, IL, ITYP, MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MOENUM, IL, ITYP, MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MOENUM, IL, ITYP, MDAT) = RMOE
          SAVED(MOENUM, IL, ITYP, MDAT+1) = RSAVE
        ENDIF
        SAVED(MOENUM, IL, ITYP, MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSE
      GETVEHICLETYPESTREETLINKMOEDATA = 1
      msgtext = moetype            
      call sendtextmsg(3)
    ENDIF
  ENDIF
10 IF(STATUS .NE. 0) THEN
    CALL SENDTEXTMSG2('MEMORY ALLOCATION FAILED IN FUNCTION: ',  &
                      'GETVEHICLETYPESTREETLINKMOEDATA', 3)
    GETVEHICLETYPESTREETLINKMOEDATA = 2
  ENDIF
  END
!
! ==========================================================================================================
! Function: GETBASICMOEDATA( char MOETYPE, int FLAG, float RMOE )
!
! --- CODED   11-25-03 BY T. SIMMERMAN
!
! Description: Returns the value of the specified MOE for the link
!              specified by up node and down node
!
!
! Arguments: MOETYPE - [in] name of the MOE
!
!            IARRAY  - [in] array of input parameters
!                           (up node, down node)
!
!            FLAG    - [in] MOE granularity
!                           0 = cumulative
!                           1 = time interval
!                           2 = time period
!
!            RMOE    - [out] value of the MOE
!
! Returns: int = 0 - success
! Returns: int = 1 - MOE name was not recognized or link was not found
! ======================================================================
  INTEGER FUNCTION GETBASICMOEDATA(MOETYPE, FLAG, RMOE)
 
  USE SIMPARAMS
  IMPLICIT NONE
  CHARACTER*(*) MOETYPE
  INTEGER     FLAG   
  REAL        RMOE   
 
  REAL SAVED(:)
  ALLOCATABLE SAVED
  SAVE SAVED
  INTEGER :: STATUS, MTIME, MDAT
  REAL :: RSAVE
 
  GETBASICMOEDATA = 0
 
  RMOE = 0.
 
  STATUS = 0
  IF(MOETYPE .EQ. 'DeallocateMemory') THEN
    IF(ALLOCATED(SAVED)) DEALLOCATE(SAVED)
  ELSE
    IF(.NOT. ALLOCATED(SAVED)) THEN
      ALLOCATE(SAVED(8), STAT = STATUS)
      SAVED = 0.0
    ENDIF
 
    IF(FLAG .EQ. 0) THEN
      MTIME = 1
      MDAT = 2
    ELSEIF(FLAG .EQ. 1) THEN
      MTIME = 3
      MDAT = 4
    ELSEIF(FLAG .EQ. 2) THEN
      MTIME = 6
      MDAT = 7
    ELSE
      GETBASICMOEDATA = 2
      GOTO 10
    ENDIF
! ------------------------------------------------------------------
    IF(MOETYPE .EQ. 'SimulationTime') THEN
 
! --- ADDED FOR USE BY OTHER MOE CALCULATIONS
 
      IF(SIMTIME .EQ. SAVED(MTIME)) THEN
        RMOE = SAVED(MDAT)
      ELSE
        RMOE = SIMTIME
        IF(FLAG .EQ. 0) THEN
          SAVED(MDAT) = RMOE
        ELSE
          RSAVE = RMOE
          RMOE = RMOE - SAVED(MDAT+1)
          IF((RMOE) .LT. 1E-5) RMOE = 0.0
          SAVED(MDAT) = RMOE
          SAVED(MDAT+1) = RSAVE
        ENDIF
        SAVED(MTIME) = SIMTIME
      ENDIF
! ----------------------------------------------------------------------
    ELSEIF(MOETYPE .EQ. 'ResetSimulationTimeAfterInit') THEN
      SAVED = 0.
! ----------------------------------------------------------------------
    ELSE
      GETBASICMOEDATA = 1
    ENDIF
  ENDIF
10 IF(STATUS .NE. 0) THEN
    CALL SENDTEXTMSG2('MEMORY ALLOCATION FAILED IN FUNCTION: ', &
                      'GETBASICMOEDATA', 3)
    GETBASICMOEDATA = 2
  ENDIF
  END
!
! ==========================================================================================================
! Subroutine: SENDTEXTMSG2( char STRING1, char STRING2, int CODE)
!
! Description: Calls SENDTEXTMSG with 2 input strings
!
!
! Arguments: STRING1 - [in] first string
!
!            STRING2 - [in] second string
!
!            CODE    - [in] message color code
!
!
! ======================================================================
  SUBROUTINE SENDTEXTMSG2(STRING1, STRING2, CODE)
  USE TEXT
  IMPLICIT NONE
  CHARACTER*(*), INTENT(IN) :: STRING1, STRING2
  INTEGER, INTENT(IN) :: CODE
  MSGTEXT = STRING1//STRING2
  CALL SENDTEXTMSG(CODE)
  RETURN
  END
