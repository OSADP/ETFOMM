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

  MODULE SEGMENTS
    IMPLICIT NONE
    INTEGER              :: MAXLINKSPERSEGMENT = 100
    INTEGER              :: NUMBER_OF_SEGMENTS
    INTEGER, ALLOCATABLE :: NUMBER_OF_LINKS_IN_SEGMENT(:)                !SEGMENT NUMBER
    INTEGER, ALLOCATABLE :: FIRST_OBJECT(:)                              !SEGMENT NUMBER
    INTEGER, ALLOCATABLE :: LAST_OBJECT(:)                               !SEGMENT NUMBER
    INTEGER, ALLOCATABLE :: LINKS_IN_SEGMENT(:,:)                        !SEGMENT NUMBER, #LINKS
        
    CONTAINS
        
! ==================================================================================================
    SUBROUTINE ALLOCATE_SEGMENT_ARRAYS
    IF(ALLOCATED(NUMBER_OF_LINKS_IN_SEGMENT)) RETURN
    ALLOCATE(NUMBER_OF_LINKS_IN_SEGMENT(NUMBER_OF_SEGMENTS))
    ALLOCATE(FIRST_OBJECT(NUMBER_OF_SEGMENTS))
    ALLOCATE(LAST_OBJECT(NUMBER_OF_SEGMENTS))
    ALLOCATE(LINKS_IN_SEGMENT(NUMBER_OF_SEGMENTS, MAXLINKSPERSEGMENT))
    NUMBER_OF_LINKS_IN_SEGMENT = 0
    FIRST_OBJECT = 0
    LAST_OBJECT = 0
    LINKS_IN_SEGMENT = 0
    RETURN
    END SUBROUTINE
        
! ==================================================================================================
    SUBROUTINE REALLOCATE_SEGLINKS
    INTEGER, ALLOCATABLE :: TEMP(:,:)
    INTEGER :: OLD, NEW
! ----------------------------------------------------------------------
    OLD = MAXLINKSPERSEGMENT
    NEW = MAXLINKSPERSEGMENT + 1
    MAXLINKSPERSEGMENT = NEW
    ALLOCATE(TEMP(NUMBER_OF_SEGMENTS, MAXLINKSPERSEGMENT))
    TEMP = LINKS_IN_SEGMENT
    DEALLOCATE(LINKS_IN_SEGMENT)
    ALLOCATE(LINKS_IN_SEGMENT(NUMBER_OF_SEGMENTS, NEW))
    LINKS_IN_SEGMENT = 0
    LINKS_IN_SEGMENT(1:NUMBER_OF_SEGMENTS, 1:OLD) = TEMP(1:NUMBER_OF_SEGMENTS, 1:OLD)
    DEALLOCATE(TEMP)
    END SUBROUTINE

        
! ==================================================================================================
    SUBROUTINE DEALLOCATE_SEGMENT_ARRAYS
    IF(ALLOCATED(NUMBER_OF_LINKS_IN_SEGMENT)) DEALLOCATE(NUMBER_OF_LINKS_IN_SEGMENT)
    IF(ALLOCATED(FIRST_OBJECT)) DEALLOCATE(FIRST_OBJECT)
    IF(ALLOCATED(LAST_OBJECT)) DEALLOCATE(LAST_OBJECT)
    IF(ALLOCATED(LINKS_IN_SEGMENT)) DEALLOCATE(LINKS_IN_SEGMENT)
    RETURN
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE SAVE_SEGMENTS(FILE)
    INTEGER, INTENT(IN) :: FILE
    INTEGER :: I, N
! ----------------------------------------------------------------------

! --- Save static data.

    WRITE(FILE) NUMBER_OF_SEGMENTS
    DO I = 1, NUMBER_OF_SEGMENTS 
      WRITE(FILE) NUMBER_OF_LINKS_IN_SEGMENT(I)
      IF(NUMBER_OF_LINKS_IN_SEGMENT(I) .EQ. 0) CYCLE
      WRITE(FILE) FIRST_OBJECT(I)
      WRITE(FILE) LAST_OBJECT(I)
      WRITE(FILE) (LINKS_IN_SEGMENT(I, N), N = 1, NUMBER_OF_LINKS_IN_SEGMENT(I))
    ENDDO
    RETURN
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE RESTORE_SEGMENTS(FILE)
    INTEGER, INTENT(IN) :: FILE
    INTEGER :: I, N
! ----------------------------------------------------------------------

! --- Restore data.

    READ(FILE) NUMBER_OF_SEGMENTS
    IF(NUMBER_OF_SEGMENTS .NE. 0) THEN
      CALL ALLOCATE_SEGMENT_ARRAYS
      DO I = 1, NUMBER_OF_SEGMENTS 
        READ(FILE) NUMBER_OF_LINKS_IN_SEGMENT(I)
        IF(NUMBER_OF_LINKS_IN_SEGMENT(I) .EQ. 0) CYCLE
        READ(FILE) FIRST_OBJECT(I)
        READ(FILE) LAST_OBJECT(I)
        READ(FILE) (LINKS_IN_SEGMENT(I, N), N = 1, NUMBER_OF_LINKS_IN_SEGMENT(I))
      ENDDO
    ENDIF
    RETURN
    END SUBROUTINE
                
  END MODULE
