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

  MODULE OBJECTS

    IMPLICIT NONE
        
    INTEGER, PARAMETER :: M_AUX_BEGIN = 1
    INTEGER, PARAMETER :: M_AUX_END = 2
    INTEGER, PARAMETER :: M_ALIGN_INTNODE = 3
    INTEGER, PARAMETER :: M_ALIGN_ON = 4
    INTEGER, PARAMETER :: M_ALIGN_OFF = 5
    INTEGER, PARAMETER :: M_ALIGN_ADP = 6
    INTEGER, PARAMETER :: M_LANE_ADD = 7
    INTEGER, PARAMETER :: M_LANE_DROP = 8
    INTEGER, PARAMETER :: M_NODE_INT = 9
    INTEGER, PARAMETER :: M_METER = 10
    INTEGER, PARAMETER :: M_DETECTOR = 11
    INTEGER, PARAMETER :: M_DATASTAT = 12
    INTEGER, PARAMETER :: M_HOV_WARN_EXIT = 13
    INTEGER, PARAMETER :: M_WARN_EXIT = 14
    INTEGER, PARAMETER :: M_WARN_DROP = 15
    INTEGER, PARAMETER :: M_WARN_BLK = 16
    INTEGER, PARAMETER :: M_WARN_ANTICIP = 17
    INTEGER, PARAMETER :: M_END_ANTICIP = 18
    INTEGER, PARAMETER :: M_WARN_HOV = 19
    INTEGER, PARAMETER :: M_WARN_ETL = 20
    INTEGER, PARAMETER :: M_NODE_ENTRY = 23
    INTEGER, PARAMETER :: M_NODE_EXIT = 24
    INTEGER, PARAMETER :: M_EXIT_INTERFACE = 25
    INTEGER, PARAMETER :: M_ENTRY_INTERFACE = 26
    INTEGER, PARAMETER :: M_INC_BEGIN = 27
    INTEGER, PARAMETER :: M_INC_END = 28
    INTEGER, PARAMETER :: M_INC_WARN = 29
    INTEGER, PARAMETER :: M_DIVERGE = 30
    INTEGER, PARAMETER :: M_HOV_BEGIN = 31
    INTEGER, PARAMETER :: M_HOV_END = 32
    INTEGER, PARAMETER :: M_HOV_WARN = 33
    INTEGER, PARAMETER :: M_ETL_WARN = 34
    INTEGER, PARAMETER :: M_MERGE_POINT = 35
    INTEGER, PARAMETER :: M_DIVERGE_POINT = 36

    TYPE OBJECT
      INTEGER :: ITYPE = 0
      INTEGER :: SEGMENT = 0
      INTEGER :: LOCATION = 0
      INTEGER :: LINK = 0
      INTEGER :: LANE = 0
      INTEGER :: VALUE = 0
    END TYPE
    
    INTEGER :: NUMBER_OF_OBJECTS
    TYPE(OBJECT), ALLOCATABLE :: OBJECT_LIST(:)

    CONTAINS

! ==================================================================================================
    SUBROUTINE ALLOCATE_OBJECT_LIST
    IF(ALLOCATED(OBJECT_LIST)) RETURN
    ALLOCATE(OBJECT_LIST(NUMBER_OF_OBJECTS))
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE DEALLOCATE_OBJECT_LIST
    IF(ALLOCATED(OBJECT_LIST)) DEALLOCATE(OBJECT_LIST)
    END SUBROUTINE
        
! ==================================================================================================
    SUBROUTINE REALLOCATE_OBJECT_LIST
    INTEGER OLD
    TYPE(OBJECT) , ALLOCATABLE :: T(:) 
! ----------------------------------------------------------------------
    OLD = NUMBER_OF_OBJECTS
    NUMBER_OF_OBJECTS = OLD + 1
    ALLOCATE(T(OLD))
    T(1:OLD) = OBJECT_LIST(1:OLD)
    DEALLOCATE(OBJECT_LIST)
    ALLOCATE(OBJECT_LIST(NUMBER_OF_OBJECTS))
    OBJECT_LIST(1:OLD) = T(1:OLD)
    DEALLOCATE(T)
    RETURN
    END SUBROUTINE
        
! ==================================================================================================
    SUBROUTINE SAVE_OBJECT_LIST(FILE)
    INTEGER, INTENT(IN) :: FILE
    INTEGER :: I
! ----------------------------------------------------------------------

! --- Save data.

    WRITE(FILE) NUMBER_OF_OBJECTS
    DO I = 1, NUMBER_OF_OBJECTS 
      WRITE(FILE) OBJECT_LIST(I)%ITYPE
      WRITE(FILE) OBJECT_LIST(I)%SEGMENT
      WRITE(FILE) OBJECT_LIST(I)%LOCATION
      WRITE(FILE) OBJECT_LIST(I)%LINK
      WRITE(FILE) OBJECT_LIST(I)%LANE
      WRITE(FILE) OBJECT_LIST(I)%VALUE
    ENDDO
    RETURN
    END SUBROUTINE
        
! ==================================================================================================
    SUBROUTINE RESTORE_OBJECT_LIST(FILE)
    INTEGER, INTENT(IN) :: FILE
    INTEGER :: I
! ----------------------------------------------------------------------

! --- Restore data.

    READ(FILE) NUMBER_OF_OBJECTS
    IF(NUMBER_OF_OBJECTS .NE. 0) THEN
      CALL ALLOCATE_OBJECT_LIST
      DO I = 1, NUMBER_OF_OBJECTS 
        READ(FILE) OBJECT_LIST(I)%ITYPE
        READ(FILE) OBJECT_LIST(I)%SEGMENT
        READ(FILE) OBJECT_LIST(I)%LOCATION
        READ(FILE) OBJECT_LIST(I)%LINK
        READ(FILE) OBJECT_LIST(I)%LANE
        READ(FILE) OBJECT_LIST(I)%VALUE
      ENDDO
    ENDIF
    RETURN
    END SUBROUTINE
        
  END MODULE
