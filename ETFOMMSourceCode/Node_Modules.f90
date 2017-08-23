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

  MODULE ENTRYNODE_DATA
  USE GLOBAL_DATA
    IMPLICIT NONE
    INTEGER, ALLOCATABLE :: FLOWRATE(:)
    REAL, ALLOCATABLE    :: CARPOOL_PCT(:)
    REAL, ALLOCATABLE    :: TRUCK_PCT(:)
    INTEGER, ALLOCATABLE :: DELAYED_COUNT(:)
    INTEGER, ALLOCATABLE :: DELAYED_TIME(:)
    LOGICAL, ALLOCATABLE :: ENTRYNODE_IS_USED(:)
    REAL, ALLOCATABLE    :: HOV_VIOLATOR_PCT(:)
    REAL, ALLOCATABLE    :: LANE_PCT(:, :)
    INTEGER :: ERLANGA
    REAL    :: MINSEP = 1.2
    INTEGER :: TYPEDIST = 0
      
    CONTAINS
     
! ==================================================================================================
    SUBROUTINE ALLOCATE_ENTRYNODE_DATA
    IF(ALLOCATED(FLOWRATE)) RETURN
    ALLOCATE(FLOWRATE(MAX_NODE_NUMBER+1:MAX_NODE_NUMBER+1000))
    ALLOCATE(CARPOOL_PCT(MAX_NODE_NUMBER+1:MAX_NODE_NUMBER+1000))
    ALLOCATE(TRUCK_PCT(MAX_NODE_NUMBER+1:MAX_NODE_NUMBER+1000))
    ALLOCATE(DELAYED_COUNT(MAX_NODE_NUMBER+1:MAX_NODE_NUMBER+1000))
    ALLOCATE(DELAYED_TIME(MAX_NODE_NUMBER+1:MAX_NODE_NUMBER+1000))
    ALLOCATE(ENTRYNODE_IS_USED(MAX_NODE_NUMBER+1:MAX_NODE_NUMBER+1000))
    ALLOCATE(HOV_VIOLATOR_PCT(MAX_NODE_NUMBER+1:MAX_NODE_NUMBER+1000))
    ALLOCATE(LANE_PCT(MAX_NODE_NUMBER+1:MAX_NODE_NUMBER+1000, 10))
    FLOWRATE = 0
    CARPOOL_PCT = 0
    TRUCK_PCT = 0
    DELAYED_COUNT = 0
    DELAYED_TIME = 0
    ENTRYNODE_IS_USED = 0
    HOV_VIOLATOR_PCT = 0
    LANE_PCT = 0
    RETURN
    END SUBROUTINE    
    
! ==================================================================================================
    SUBROUTINE SAVE_ENTRYNODE_DATA(FILE)
    INTEGER, INTENT(IN) :: FILE
! ----------------------------------------------------------------------

! --- Save static data.

    WRITE(FILE) MINSEP
    WRITE(FILE) TYPEDIST
    WRITE(FILE) ERLANGA
    WRITE(FILE) DELAYED_COUNT
    WRITE(FILE) DELAYED_TIME
    WRITE(FILE) TRUCK_PCT
    WRITE(FILE) CARPOOL_PCT
    WRITE(FILE) HOV_VIOLATOR_PCT
    WRITE(FILE) LANE_PCT
    RETURN
    END SUBROUTINE
     
! ==================================================================================================
    SUBROUTINE RESTORE_ENTRYNODE_DATA(FILE)
    INTEGER, INTENT(IN) :: FILE
! ----------------------------------------------------------------------

! --- Restore data.

    READ(FILE) MINSEP
    READ(FILE) TYPEDIST
    READ(FILE) ERLANGA
    READ(FILE) DELAYED_COUNT
    READ(FILE) DELAYED_TIME
    READ(FILE) TRUCK_PCT
    READ(FILE) CARPOOL_PCT
    READ(FILE) HOV_VIOLATOR_PCT
    READ(FILE) LANE_PCT
    RETURN
    END SUBROUTINE
        
  END MODULE

  MODULE FREEWAY_NODES
    IMPLICIT NONE
    INTEGER, ALLOCATABLE       :: MAINLINE_APPROACH(:)
    INTEGER, ALLOCATABLE       :: RAMP_APPROACH(:)
    INTEGER, ALLOCATABLE       :: MERGE_APPROACH(:)
    INTEGER, ALLOCATABLE       :: WARN_DISTANCE(:)
    INTEGER, ALLOCATABLE       :: HOV_WARN_DISTANCE(:)
    LOGICAL, ALLOCATABLE       :: DUMMY_ENTRY(:)
    LOGICAL, ALLOCATABLE       :: DUMMY_EXIT(:)
    INTEGER, ALLOCATABLE       :: NODE_OBJECT(:)


    CONTAINS

! ==================================================================================================
    SUBROUTINE ALLOCATE_FREEWAY_NODE_ARRAYS
    IF(ALLOCATED(MAINLINE_APPROACH)) RETURN
    ALLOCATE(MAINLINE_APPROACH(8999))
    ALLOCATE(RAMP_APPROACH(8999))
    ALLOCATE(MERGE_APPROACH(8999))
    ALLOCATE(WARN_DISTANCE(8999))
    ALLOCATE(HOV_WARN_DISTANCE(8999))
    ALLOCATE(DUMMY_ENTRY(8999))
    ALLOCATE(DUMMY_EXIT(8999))
    ALLOCATE(NODE_OBJECT(8999))
    MAINLINE_APPROACH = 0
    RAMP_APPROACH = 0
    MERGE_APPROACH = 0
    WARN_DISTANCE = 0
    HOV_WARN_DISTANCE = 0
    DUMMY_ENTRY = .FALSE.
    DUMMY_EXIT = .FALSE.
    NODE_OBJECT = 0
    RETURN
    END SUBROUTINE

    SUBROUTINE DEALLOCATE_FREEWAY_NODE_ARRAYS
    IF(ALLOCATED(MAINLINE_APPROACH)) DEALLOCATE(MAINLINE_APPROACH)
    IF(ALLOCATED(RAMP_APPROACH)) DEALLOCATE(RAMP_APPROACH)
    IF(ALLOCATED(MERGE_APPROACH)) DEALLOCATE(MERGE_APPROACH)
    IF(ALLOCATED(WARN_DISTANCE)) DEALLOCATE(WARN_DISTANCE)
    IF(ALLOCATED(HOV_WARN_DISTANCE)) DEALLOCATE(HOV_WARN_DISTANCE)
    IF(ALLOCATED(DUMMY_ENTRY)) DEALLOCATE(DUMMY_ENTRY)
    IF(ALLOCATED(DUMMY_EXIT)) DEALLOCATE(DUMMY_EXIT)
    IF(ALLOCATED(NODE_OBJECT)) DEALLOCATE(NODE_OBJECT)
    RETURN
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE SAVE_FREEWAY_NODES(FILE)
    INTEGER, INTENT(IN) :: FILE
    INTEGER :: I
! ----------------------------------------------------------------------

! --- Save static data.

    DO I = 1, 8999 
      WRITE(FILE) MAINLINE_APPROACH(I)
      WRITE(FILE) RAMP_APPROACH(I)
      WRITE(FILE) MERGE_APPROACH(I)
      WRITE(FILE) WARN_DISTANCE(I)
      WRITE(FILE) HOV_WARN_DISTANCE(I)
      WRITE(FILE) DUMMY_ENTRY(I)
      WRITE(FILE) DUMMY_EXIT(I)
      WRITE(FILE) NODE_OBJECT(I)
    ENDDO
    RETURN
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE RESTORE_FREEWAY_NODES(FILE)
    INTEGER, INTENT(IN) :: FILE
    INTEGER :: I
! ----------------------------------------------------------------------

! --- Restore data.

    DO I = 1, 8999 
      READ(FILE) MAINLINE_APPROACH(I)
      READ(FILE) RAMP_APPROACH(I)
      READ(FILE) MERGE_APPROACH(I)
      READ(FILE) WARN_DISTANCE(I)
      READ(FILE) HOV_WARN_DISTANCE(I)
      READ(FILE) DUMMY_ENTRY(I)
      READ(FILE) DUMMY_EXIT(I)
      READ(FILE) NODE_OBJECT(I)
    ENDDO
    RETURN
    END SUBROUTINE
                
  END MODULE

  MODULE STREET_NODES
    USE GLOBAL_DATA
    IMPLICIT NONE
    INTEGER, ALLOCATABLE :: NACT(:)
    INTEGER, ALLOCATABLE :: NFTC(:)

    CONTAINS

! ==================================================================================================
    SUBROUTINE ALLOCATE_STREET_NODE_ARRAYS
    IF(ALLOCATED(NACT)) RETURN
    ALLOCATE(NACT(MAX_NODE_NUMBER))
    ALLOCATE(NFTC(MAX_NODE_NUMBER))
    NACT = 0
    NFTC = 0
    RETURN
    END SUBROUTINE

    SUBROUTINE DEALLOCATE_STREET_NODE_ARRAYS
    IF(ALLOCATED(NACT)) DEALLOCATE(NACT)
    IF(ALLOCATED(NFTC)) DEALLOCATE(NFTC)
    RETURN
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE SAVE_STREET_NODES(FILE)
    INTEGER, INTENT(IN) :: FILE
    INTEGER :: I
! ----------------------------------------------------------------------

! --- Save static data.

    DO I = 1, MAX_NODE_NUMBER 
      WRITE(FILE) NACT(I)
      WRITE(FILE) NFTC(I)
    ENDDO
    RETURN
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE RESTORE_STREET_NODES(FILE)
    INTEGER, INTENT(IN) :: FILE
    INTEGER :: I
! ----------------------------------------------------------------------

! --- Restore data.

    CALL ALLOCATE_STREET_NODE_ARRAYS
    DO I = 1, MAX_NODE_NUMBER
      READ(FILE) NACT(I)
      READ(FILE) NFTC(I)
    ENDDO
    RETURN
    END SUBROUTINE
                
  END MODULE
  
  MODULE NODE_TABLE
    INTEGER, PARAMETER :: NT_INTERN = 0
    INTEGER, PARAMETER :: NT_INTERFACE = 1
    INTEGER, PARAMETER :: NT_EXTERN = 2
    INTEGER, ALLOCATABLE :: X195(:)
    INTEGER, ALLOCATABLE :: Y195(:)
    REAL, ALLOCATABLE    :: NODE_LAT(:)
    REAL, ALLOCATABLE    :: NODE_LON(:)
    INTEGER, ALLOCATABLE :: NODE_ELEV(:)
    LOGICAL, ALLOCATABLE :: IS_USED(:)
    LOGICAL, ALLOCATABLE :: IS_DEFINED(:)
    INTEGER, ALLOCATABLE :: NETCODE(:)
    INTEGER, ALLOCATABLE :: NODE_TYPE(:)
    
    CONTAINS
  
    SUBROUTINE ALLOCATE_NODE_ARRAYS(N)
    INTEGER :: N
    IF(ALLOCATED(X195)) RETURN
    ALLOCATE(X195(N))
    ALLOCATE(Y195(N))
    ALLOCATE(NODE_LAT(N))
    ALLOCATE(NODE_LON(N))
    ALLOCATE(NODE_ELEV(N))
    ALLOCATE(IS_USED(N))
    ALLOCATE(IS_DEFINED(N))
    ALLOCATE(NETCODE(N))
    ALLOCATE(NODE_TYPE(N))
    X195 = 0
    Y195 = 0
    NODE_LAT = 0.
    NODE_LON = 0.
    NODE_ELEV = 0
    IS_USED = .FALSE.
    IS_DEFINED = .FALSE.
    NETCODE = 0
    NODE_TYPE = 0
    RETURN
    END SUBROUTINE

    SUBROUTINE DEALLOCATE_NODE_ARRAYS
    IF(ALLOCATED(X195)) THEN
      DEALLOCATE(X195)
      DEALLOCATE(Y195)
      DEALLOCATE(NODE_LAT)
      DEALLOCATE(NODE_LON)
      DEALLOCATE(NODE_ELEV)
      DEALLOCATE(IS_USED)
      DEALLOCATE(IS_DEFINED)
      DEALLOCATE(NETCODE)
      DEALLOCATE(NODE_TYPE)
    ENDIF
    RETURN
    END SUBROUTINE

  END MODULE

