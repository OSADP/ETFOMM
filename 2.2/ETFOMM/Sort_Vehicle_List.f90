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

!QSORT doesn't work with the CVF calling convention, so SORT_VEHICLE_LIST
!is in a separate file with separate compiler settings.

MODULE SORT_MODULE
  TYPE DATA_PAIR
    INTEGER :: INDEX
    REAL    :: LOCATION
  END TYPE 
END MODULE

SUBROUTINE QSORT_VEHICLE_LIST
USE IFPORT
USE IFPORT_TYPES
USE SORT_MODULE
USE FREEWAY_VEHICLES
IMPLICIT NONE

INTERFACE 
  SUBROUTINE QSORT_VEHICLES(ARRAY, A, B, C)
!DEC$ ATTRIBUTES DEFAULT, DECORATE, ALIAS:'QSORT' ::  QSORT_VEHICLES
  USE SORT_MODULE
  USE IFPORT_TYPES
  TYPE(DATA_PAIR) :: ARRAY(*)
  INTEGER(SIZEOF_SIZE_T) ::  A, B
  INTEGER(2), EXTERNAL :: C
  END SUBROUTINE
END INTERFACE

INTEGER :: I
INTEGER(SIZEOF_SIZE_T) :: A, B
INTEGER(2), EXTERNAL :: COMPARE
TYPE(DATA_PAIR), ALLOCATABLE :: SORT_ARRAY(:)
INTEGER :: ARRAY_DIM

IF(.NOT. ALLOCATED(SORT_ARRAY)) THEN
  ALLOCATE(SORT_ARRAY(MAX_VEHICLES_F))
  ARRAY_DIM = MAX_VEHICLES_F
ELSEIF(MAX_VEHICLES_F .GT. ARRAY_DIM) THEN
  DEALLOCATE(SORT_ARRAY)
  ALLOCATE(SORT_ARRAY(MAX_VEHICLES_F))
  ARRAY_DIM = MAX_VEHICLES_F
ENDIF
IF(SORTED_LIST_LENGTH .GT. 1) THEN
  A = SORTED_LIST_LENGTH
  B = SIZEOF(SORT_ARRAY(1))
  DO I = 1, SORTED_LIST_LENGTH
    SORT_ARRAY(I)%LOCATION = DISTANCE_TO_SEGMENT_END(SORTED_LIST(I)) + FVLENGTH(SORTED_LIST(I))
    SORT_ARRAY(I)%INDEX = SORTED_LIST(I)
  ENDDO
  CALL QSORT_VEHICLES(SORT_ARRAY, A, B, COMPARE)
  DO I = 1, SORTED_LIST_LENGTH
    SORTED_LIST(I) = SORT_ARRAY(I)%INDEX
    SORT_POSITION(SORT_ARRAY(I)%INDEX) = I
  ENDDO
ENDIF
RETURN
END
  
INTEGER(2) FUNCTION COMPARE(A1, A2)
USE SORT_MODULE
TYPE(DATA_PAIR) :: A1, A2
IF(A1%LOCATION - A2%LOCATION .LT. 0.) THEN
  COMPARE = -1
ELSEIF(A1%LOCATION - A2%LOCATION .GT. 0.) THEN
  COMPARE = 1
ELSE
  COMPARE = 0
ENDIF
END