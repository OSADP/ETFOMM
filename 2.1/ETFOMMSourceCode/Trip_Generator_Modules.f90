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

  MODULE DATA_MOD
! --- CODED   12-10-02 BY T. SIMMERMAN
  INTEGER, PARAMETER::MAXENTRYNODES = 1000
  INTEGER, PARAMETER::MAXTIMEPOINTS = 304
  END MODULE


  MODULE TURNDATA_MOD
! --- CODED   12-10-02 BY T. SIMMERMAN
  USE DATA_MOD
  TYPE TURN_DATA
    INTEGER :: UP = 0
    INTEGER :: DOWN = 0
    INTEGER :: INDEX = 0
    INTEGER :: CARDTYPE = 0
    INTEGER :: LINKID = 0
    INTEGER, DIMENSION(MAXTIMEPOINTS) :: TIME = 0
    REAL, DIMENSION(5, MAXTIMEPOINTS) :: RPCT = 0
  END TYPE
  TYPE(TURN_DATA), ALLOCATABLE :: TURNDATA(:)
  INTEGER :: NTURNDATA = 0
  LOGICAL :: W23 = .FALSE.
  LOGICAL :: W26 = .FALSE.
      
  CONTAINS
      
! ==================================================================================================
  SUBROUTINE ALLOCATE_TURNDATA(N)
  INTEGER, INTENT(IN) :: N
  ALLOCATE(TURNDATA(N))
  RETURN
  END SUBROUTINE
  
! ==================================================================================================
  SUBROUTINE DEALLOCATE_TURNDATA
  IF(ALLOCATED(TURNDATA)) DEALLOCATE(TURNDATA)
  RETURN
  END SUBROUTINE
  
! ==================================================================================================
  SUBROUTINE SAVE_TURNDATA_MOD(FILE)
  INTEGER, INTENT(IN) :: FILE
  INTEGER :: I, J
! ----------------------------------------------------------------------
  WRITE(FILE) NTURNDATA
  DO I = 1, NTURNDATA
    WRITE(FILE) TURNDATA(I)%UP
    WRITE(FILE) TURNDATA(I)%DOWN
    WRITE(FILE) TURNDATA(I)%INDEX
    WRITE(FILE) TURNDATA(I)%CARDTYPE
    DO J = 1, TURNDATA(I)%INDEX
      WRITE(FILE) TURNDATA(I)%TIME(J)
      WRITE(FILE) TURNDATA(I)%RPCT(1, J)
      WRITE(FILE) TURNDATA(I)%RPCT(2, J)
      WRITE(FILE) TURNDATA(I)%RPCT(3, J)
      WRITE(FILE) TURNDATA(I)%RPCT(4, J)
    ENDDO
  ENDDO
  END SUBROUTINE
      
! ==================================================================================================
  SUBROUTINE RESTORE_TURNDATA_MOD(FILE)
  INTEGER, INTENT(IN) :: FILE
  INTEGER :: I, J
! ----------------------------------------------------------------------
  READ(FILE) NTURNDATA
  IF(NTURNDATA .NE. 0) THEN
    IF(.NOT. ALLOCATED(TURNDATA)) ALLOCATE(TURNDATA(NTURNDATA))
    DO I = 1, NTURNDATA
      READ(FILE) TURNDATA(I)%UP
      READ(FILE) TURNDATA(I)%DOWN
      READ(FILE) TURNDATA(I)%INDEX
      READ(FILE) TURNDATA(I)%CARDTYPE
      DO J = 1, TURNDATA(I)%INDEX
        READ(FILE) TURNDATA(I)%TIME(J)
        READ(FILE) TURNDATA(I)%RPCT(1, J)
        READ(FILE) TURNDATA(I)%RPCT(2, J)
        READ(FILE) TURNDATA(I)%RPCT(3, J)
        READ(FILE) TURNDATA(I)%RPCT(4, J)
      ENDDO
    ENDDO
  ENDIF
  END SUBROUTINE
      
  END MODULE
      
  MODULE FLOWDATA_MOD
! --- CODED   12-10-02 BY T. SIMMERMAN
    USE DATA_MOD
    TYPE FLOWDATA
      INTEGER :: UP = 0
      INTEGER :: DOWN = 0
      INTEGER :: SSNODE = 0
      INTEGER :: INDEX = 0
      INTEGER :: CARDTYPE = 0
      INTEGER :: LINKID = 0
      INTEGER :: NETWORK = 0
      INTEGER :: LANES = 0
      LOGICAL :: INTERPOLATE = .FALSE.          
      INTEGER, DIMENSION(MAXTIMEPOINTS) :: TIME = 0
      INTEGER, DIMENSION(MAXTIMEPOINTS) :: FLOW = 0
    END TYPE
    REAL, DIMENSION(MAXENTRYNODES) :: RSPEC
    INTEGER, DIMENSION(MAXENTRYNODES) :: NEMITD
    INTEGER :: NUMBER_OF_ENTRYNODES = 0
    INTEGER :: NUMBER_OF_SSNODES = 0
    TYPE(FLOWDATA), DIMENSION(MAXENTRYNODES) :: ENTRYLINK
    LOGICAL :: WRITE50(8000:8999, 19) = .FALSE.
    LOGICAL :: WRITE53(8000:8999, 19) = .FALSE.
        
    CONTAINS
      
! ==================================================================================================
    SUBROUTINE SAVE_FLOWDATA_MOD(FILE)
    INTEGER, INTENT(IN) :: FILE
    INTEGER :: I, J
! ----------------------------------------------------------------------
    WRITE(FILE) NUMBER_OF_ENTRYNODES
    DO I = 1, NUMBER_OF_ENTRYNODES
      WRITE(FILE) ENTRYLINK(I)%UP
      WRITE(FILE) ENTRYLINK(I)%DOWN
      WRITE(FILE) ENTRYLINK(I)%SSNODE
      WRITE(FILE) ENTRYLINK(I)%INDEX
      WRITE(FILE) ENTRYLINK(I)%CARDTYPE
      WRITE(FILE) ENTRYLINK(I)%LINKID
      WRITE(FILE) ENTRYLINK(I)%NETWORK
      WRITE(FILE) ENTRYLINK(I)%LANES
      WRITE(FILE) ENTRYLINK(I)%INTERPOLATE
      DO J = 1, ENTRYLINK(I)%INDEX
        WRITE(FILE) ENTRYLINK(I)%TIME(J)
        WRITE(FILE) ENTRYLINK(I)%FLOW(J)
      ENDDO
    ENDDO
    END SUBROUTINE
      
! ==================================================================================================
    SUBROUTINE RESTORE_FLOWDATA_MOD(FILE)
    INTEGER, INTENT(IN) :: FILE
    INTEGER :: I, J
! ----------------------------------------------------------------------
    READ(FILE) NUMBER_OF_ENTRYNODES
    IF(NUMBER_OF_ENTRYNODES .NE. 0) THEN
      DO I = 1, NUMBER_OF_ENTRYNODES
        READ(FILE) ENTRYLINK(I)%UP
        READ(FILE) ENTRYLINK(I)%DOWN
        READ(FILE) ENTRYLINK(I)%SSNODE
        READ(FILE) ENTRYLINK(I)%INDEX
        READ(FILE) ENTRYLINK(I)%CARDTYPE
        READ(FILE) ENTRYLINK(I)%LINKID
        READ(FILE) ENTRYLINK(I)%NETWORK
        READ(FILE) ENTRYLINK(I)%LANES
        READ(FILE) ENTRYLINK(I)%INTERPOLATE
        DO J = 1, ENTRYLINK(I)%INDEX
          READ(FILE) ENTRYLINK(I)%TIME(J)
          READ(FILE) ENTRYLINK(I)%FLOW(J)
        ENDDO
      ENDDO
    ENDIF
    END SUBROUTINE

  END MODULE 
     
  MODULE VEHICLE_MOD
! --- CODED   12-10-02 BY T. SIMMERMAN
  USE DATA_MOD
  TYPE VEHICLE
      INTEGER :: ENTRYLINKID = 0
      REAL    :: DEPARTURE = 0
      INTEGER :: NETWORK = 0
      INTEGER :: PATHID = 0
      INTEGER :: ROUTEID = 0
      INTEGER :: DRIVER = 0
      INTEGER :: FLEET = 0
      INTEGER :: VTYPE = 0
      INTEGER :: GLOBALID = 0
      INTEGER :: TURNCODE = 0
      INTEGER :: NETFLOW = 0
      INTEGER :: OVRSPD = 0
      INTEGER :: RANGE = 0
      INTEGER :: CFM = 0
  END TYPE VEHICLE
  END MODULE 

  MODULE QUEUE_MOD
! --- CODED   12-10-02 BY T. SIMMERMAN
    USE DATA_MOD
    USE VEHICLE_MOD
    TYPE QNODE
      TYPE(VEHICLE) :: VEHICLE
      TYPE(QNODE),POINTER :: NEXT=>NULL()
    END TYPE QNODE
      
    TYPE QPOINTER
      TYPE(QNODE),POINTER :: PTR=>NULL()
    END TYPE QPOINTER
        
    TYPE(QPOINTER),DIMENSION(MAXENTRYNODES) :: FRONT
    TYPE(QPOINTER),DIMENSION(MAXENTRYNODES) :: REAR
    TYPE(QPOINTER) :: PBV_FRONT, PBV_REAR
    TYPE(QPOINTER) :: BUS_FRONT, BUS_REAR
        
    CONTAINS
      
! ==================================================================================================
    SUBROUTINE SAVE_QUEUE_MOD(FILE)
    USE FLOWDATA_MOD
    USE NODE_TABLE
    INTEGER, INTENT(IN) :: FILE
    TYPE(VEHICLE) :: FRONT_VEHICLE
    TYPE(QNODE), POINTER :: TEMP
    INTEGER :: IN
! ----------------------------------------------------------------------
    DO IN = 1, NUMBER_OF_ENTRYNODES
      IF(NODE_TYPE(ENTRYLINK(IN)%UP) .EQ. NT_EXTERN) THEN
        IF(ASSOCIATED(FRONT(IN)%PTR)) THEN
          TEMP => FRONT(IN)%PTR%NEXT
          DO WHILE(ASSOCIATED(TEMP))
            FRONT_VEHICLE = TEMP%VEHICLE
            WRITE(FILE) FRONT_VEHICLE
            TEMP => TEMP%NEXT
          ENDDO
        ENDIF
      ENDIF
    ENDDO
    RETURN
    END SUBROUTINE
      
! ==================================================================================================
    SUBROUTINE RESTORE_QUEUE_MOD(FILE)
    USE FLOWDATA_MOD
    USE TEXT
    USE SIMPARAMS
    USE NODE_TABLE
    INTEGER, INTENT(IN) :: FILE
    TYPE(VEHICLE) :: THIS_VEHICLE
    TYPE(QNODE), POINTER :: TEMP
    INTEGER :: IN, STATUS
! ----------------------------------------------------------------------
    DO IN = 1, NUMBER_OF_ENTRYNODES
      IF(NODE_TYPE(ENTRYLINK(IN)%UP) .EQ. NT_EXTERN) THEN
        IF(ASSOCIATED(FRONT(IN)%PTR)) THEN
          TEMP => FRONT(IN)%PTR%NEXT
          DO WHILE(ASSOCIATED(TEMP))
            THIS_VEHICLE = TEMP%VEHICLE
            READ(FILE) THIS_VEHICLE
            TEMP => TEMP%NEXT
            IF(.NOT. ASSOCIATED(FRONT(IN)%PTR)) THEN
              ALLOCATE(FRONT(IN)%PTR, STAT=STATUS)
              IF(STATUS .NE. 0) THEN
                CALL NEW_ERROR
                MSGTEXT = 'INSUFFICIENT MEMORY'
                CALL SENDTEXTMSG(M_ERROR)
                RETURN
              ENDIF
              REAR(IN)%PTR => FRONT(IN)%PTR
            ELSE
              ALLOCATE(REAR(IN)%PTR%NEXT, STAT=STATUS)
              IF(STATUS .NE. 0) THEN
                CALL NEW_ERROR
                MSGTEXT = 'INSUFFICIENT MEMORY'
                CALL SENDTEXTMSG(M_ERROR)
                RETURN
              ENDIF
              REAR(IN)%PTR => REAR(IN)%PTR%NEXT
            ENDIF
            REAR(IN)%PTR%VEHICLE = THIS_VEHICLE
          ENDDO
        ENDIF
      ENDIF
    ENDDO
    RETURN
    END SUBROUTINE
        
  END MODULE 

  MODULE PATH_MOD
! --- CODED   12-10-02 BY T. SIMMERMAN
    USE DATA_MOD
    INTEGER, ALLOCATABLE :: NNODES(:)
    INTEGER, ALLOCATABLE :: PATH_NODES(:,:)
    INTEGER :: MXNODESINPATH = 100
    INTEGER :: NPATHS = 0
    INTEGER :: NUMBER_OF_PATHS = 100
        
    CONTAINS
      
! ==================================================================================================
    SUBROUTINE ALLOCATE_PATH_ARRAYS
    ALLOCATE(PATH_NODES(NUMBER_OF_PATHS, MXNODESINPATH))
    ALLOCATE(NNODES(NUMBER_OF_PATHS))
    PATH_NODES = 0
    NNODES = 0
    END SUBROUTINE
    
! ==================================================================================================
    SUBROUTINE REALLOCATE_PATH_ARRAYS
    USE ARRAY_FUNCTIONS
    INTEGER :: I1, I2
    I1 = NUMBER_OF_PATHS
    I2 = NUMBER_OF_PATHS + 100
    NUMBER_OF_PATHS = I2
    CALL REALLOCATE_INTEGER_X(PATH_NODES, I1, I2, MXNODESINPATH)
    CALL REALLOCATE_INTEGER(NNODES, I1, I2)
    END SUBROUTINE
        
! ==================================================================================================
    SUBROUTINE SAVE_PATH_MOD(FILE)
    INTEGER, INTENT(IN) :: FILE
    INTEGER :: I, J
! ----------------------------------------------------------------------
    WRITE(FILE) NPATHS
    DO I = 1, NPATHS
      WRITE(FILE) NNODES(I)
      WRITE(FILE) (PATH_NODES(I, J), J = 1, NNODES(I))
    ENDDO
    END SUBROUTINE
      
! ==================================================================================================
    SUBROUTINE RESTORE_PATH_MOD(FILE)
    INTEGER, INTENT(IN) :: FILE
    INTEGER :: I, J
! ----------------------------------------------------------------------
    READ(FILE) NPATHS
    IF(NPATHS .NE. 0) THEN
      IF(.NOT. ALLOCATED(NNODES)) THEN
        ALLOCATE(NNODES(NPATHS))
        ALLOCATE(PATH_NODES(NPATHS, 100))
      ENDIF
      DO I = 1, NPATHS
        READ(FILE) NNODES(I)
        READ(FILE) (PATH_NODES(I, J), J = 1, NNODES(I))
      ENDDO
    ENDIF
    END SUBROUTINE
        
  END MODULE 
 
