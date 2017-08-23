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

  MODULE PARKING
    IMPLICIT NONE
    INTEGER              :: NUMBER_OF_PARKING_ZONES

    INTEGER, ALLOCATABLE :: PARK_DURATION(:)
    INTEGER, ALLOCATABLE :: PARK_FREQ(:)
    INTEGER, ALLOCATABLE :: PARK_LEFT_START(:)
    INTEGER, ALLOCATABLE :: PARK_LEFT_LEN(:)
    INTEGER, ALLOCATABLE :: PARK_RIGHT_START(:)
    INTEGER, ALLOCATABLE :: PARK_RIGHT_LEN(:)
    INTEGER, ALLOCATABLE :: PARKING_ZONE_LINK(:)
    LOGICAL, ALLOCATABLE :: WRITE56(:,:)

    CONTAINS

! ==================================================================================================
    SUBROUTINE ALLOCATE_PARKING_ARRAYS
    IF(ALLOCATED(PARKING_ZONE_LINK)) RETURN
    ALLOCATE(PARKING_ZONE_LINK(1))
    ALLOCATE(PARK_RIGHT_START(1))
    ALLOCATE(PARK_RIGHT_LEN(1))
    ALLOCATE(PARK_LEFT_START(1))
    ALLOCATE(PARK_LEFT_LEN(1))
    ALLOCATE(PARK_DURATION(1))
    ALLOCATE(PARK_FREQ(1))
    NUMBER_OF_PARKING_ZONES = 1
    RETURN
    END SUBROUTINE
      
! ==================================================================================================
    SUBROUTINE REALLOCATE_PARKING_ARRAYS
    USE ARRAY_FUNCTIONS
    INTEGER :: I1, I2
! ----------------------------------------------------------------------
    I1 = NUMBER_OF_PARKING_ZONES
    I2 = NUMBER_OF_PARKING_ZONES + 1
    NUMBER_OF_PARKING_ZONES = I2
    CALL REALLOCATE_INTEGER(PARKING_ZONE_LINK, I1, I2)
    CALL REALLOCATE_INTEGER(PARK_RIGHT_START, I1, I2)
    CALL REALLOCATE_INTEGER(PARK_RIGHT_LEN, I1, I2)
    CALL REALLOCATE_INTEGER(PARK_LEFT_START, I1, I2)
    CALL REALLOCATE_INTEGER(PARK_LEFT_LEN, I1, I2)
    CALL REALLOCATE_INTEGER(PARK_DURATION, I1, I2)
    CALL REALLOCATE_INTEGER(PARK_FREQ, I1, I2)
    END SUBROUTINE
      
! ==================================================================================================
    SUBROUTINE DEALLOCATE_PARKING_ARRAYS
    IF(ALLOCATED(PARKING_ZONE_LINK)) DEALLOCATE(PARKING_ZONE_LINK)
    IF(ALLOCATED(PARK_RIGHT_START)) DEALLOCATE(PARK_RIGHT_START)
    IF(ALLOCATED(PARK_RIGHT_LEN)) DEALLOCATE(PARK_RIGHT_LEN)
    IF(ALLOCATED(PARK_LEFT_START)) DEALLOCATE(PARK_LEFT_START)
    IF(ALLOCATED(PARK_LEFT_LEN)) DEALLOCATE(PARK_LEFT_LEN)
    IF(ALLOCATED(PARK_DURATION)) DEALLOCATE(PARK_DURATION)
    IF(ALLOCATED(PARK_FREQ)) DEALLOCATE(PARK_FREQ)
    RETURN
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE SAVE_PARKING(FILE)
    INTEGER, INTENT(IN) :: FILE
    INTEGER :: I
! ----------------------------------------------------------------------

! --- Save data.

    WRITE(FILE) NUMBER_OF_PARKING_ZONES
    DO I = 1, NUMBER_OF_PARKING_ZONES 
      WRITE(FILE) PARKING_ZONE_LINK(I)
      WRITE(FILE) PARK_RIGHT_START(I)
      WRITE(FILE) PARK_RIGHT_LEN(I)
      WRITE(FILE) PARK_LEFT_START(I)
      WRITE(FILE) PARK_LEFT_LEN(I)
      WRITE(FILE) PARK_DURATION(I)
      WRITE(FILE) PARK_FREQ(I)
    ENDDO
    RETURN
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE RESTORE_PARKING(FILE)
    INTEGER, INTENT(IN) :: FILE
    INTEGER :: I
! ----------------------------------------------------------------------

! --- Restore data.

    READ(FILE) NUMBER_OF_PARKING_ZONES
    IF(NUMBER_OF_PARKING_ZONES .NE. 0) THEN
      CALL ALLOCATE_PARKING_ARRAYS
      DO I = 1, NUMBER_OF_PARKING_ZONES 
        READ(FILE) PARKING_ZONE_LINK(I)
        READ(FILE) PARK_RIGHT_START(I)
        READ(FILE) PARK_RIGHT_LEN(I)
        READ(FILE) PARK_LEFT_START(I)
        READ(FILE) PARK_LEFT_LEN(I)
        READ(FILE) PARK_DURATION(I)
        READ(FILE) PARK_FREQ(I)
      ENDDO
    ENDIF
    RETURN
    END SUBROUTINE
                                
  END MODULE