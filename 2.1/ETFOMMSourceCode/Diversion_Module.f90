! Copyright � 2014
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

  MODULE DIVERSIONS
    IMPLICIT NONE

    INTEGER              :: NUMBER_OF_DIVERSIONS = 0
    INTEGER, ALLOCATABLE :: DIVERSION_BEGIN_TIME(:)
    INTEGER, ALLOCATABLE :: DIVERSION_END_TIME(:)
    INTEGER, ALLOCATABLE :: DIVERSION_LINK(:)
    INTEGER, ALLOCATABLE :: DIVERSION_LOCATION(:)
    INTEGER, ALLOCATABLE :: DIVERSION_PATHID(:)
    INTEGER, ALLOCATABLE :: DIVERSION_PERCENTAGE(:)
    INTEGER, ALLOCATABLE :: DIVERSION_SPEED(:)

    CONTAINS

! ==================================================================================================
    SUBROUTINE ALLOCATE_DIVERSION_ARRAYS
    IF(ALLOCATED(DIVERSION_LINK)) RETURN
    ALLOCATE(DIVERSION_LINK(NUMBER_OF_DIVERSIONS))
    ALLOCATE(DIVERSION_PATHID(NUMBER_OF_DIVERSIONS))
    ALLOCATE(DIVERSION_BEGIN_TIME(NUMBER_OF_DIVERSIONS))
    ALLOCATE(DIVERSION_END_TIME(NUMBER_OF_DIVERSIONS))
    ALLOCATE(DIVERSION_LOCATION(NUMBER_OF_DIVERSIONS))
    ALLOCATE(DIVERSION_PERCENTAGE(NUMBER_OF_DIVERSIONS))
    ALLOCATE(DIVERSION_SPEED(NUMBER_OF_DIVERSIONS))
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE DEALLOCATE_INCIDENT_ARRAYS
    IF(ALLOCATED(DIVERSION_LINK))       DEALLOCATE(DIVERSION_LINK)
    IF(ALLOCATED(DIVERSION_PATHID))     DEALLOCATE(DIVERSION_PATHID)
    IF(ALLOCATED(DIVERSION_BEGIN_TIME)) DEALLOCATE(DIVERSION_BEGIN_TIME)
    IF(ALLOCATED(DIVERSION_END_TIME))   DEALLOCATE(DIVERSION_END_TIME)
    IF(ALLOCATED(DIVERSION_LOCATION))     DEALLOCATE(DIVERSION_LOCATION)
    IF(ALLOCATED(DIVERSION_PERCENTAGE)) DEALLOCATE(DIVERSION_PERCENTAGE)
    IF(ALLOCATED(DIVERSION_SPEED))      DEALLOCATE(DIVERSION_SPEED)
    END SUBROUTINE
      
! ==================================================================================================
    SUBROUTINE SAVE_DIVERSIONS(FILE1)
    USE SIMPARAMS
    INTEGER, INTENT(IN) :: FILE1
! ----------------------------------------------------------------------

! --- Save static data.

    WRITE(FILE1) NUMBER_OF_DIVERSIONS
    WRITE(FILE1) DIVERSION_LINK
    WRITE(FILE1) DIVERSION_PATHID
    WRITE(FILE1) DIVERSION_BEGIN_TIME
    WRITE(FILE1) DIVERSION_END_TIME
    WRITE(FILE1) DIVERSION_LOCATION
    WRITE(FILE1) DIVERSION_PERCENTAGE
    WRITE(FILE1) DIVERSION_SPEED
    RETURN
    END SUBROUTINE
            
! ==================================================================================================
    SUBROUTINE RESTORE_INCIDENTS(FILE1)
    USE SIMPARAMS
    INTEGER, INTENT(IN) :: FILE1
! ----------------------------------------------------------------------

! --- Restore data.

    READ(FILE1) NUMBER_OF_DIVERSIONS
    IF(NUMBER_OF_DIVERSIONS .NE. 0) THEN
      IF(.NOT. ALLOCATED(DIVERSION_LINK)) THEN
        ALLOCATE(DIVERSION_LINK(NUMBER_OF_DIVERSIONS))
        ALLOCATE(DIVERSION_PATHID(NUMBER_OF_DIVERSIONS))
        ALLOCATE(DIVERSION_BEGIN_TIME(NUMBER_OF_DIVERSIONS))
        ALLOCATE(DIVERSION_END_TIME(NUMBER_OF_DIVERSIONS))
        ALLOCATE(DIVERSION_LOCATION(NUMBER_OF_DIVERSIONS))
        ALLOCATE(DIVERSION_PERCENTAGE(NUMBER_OF_DIVERSIONS))
        ALLOCATE(DIVERSION_SPEED(NUMBER_OF_DIVERSIONS))
      ENDIF
      READ(FILE1) NUMBER_OF_DIVERSIONS
      READ(FILE1) DIVERSION_LINK
      READ(FILE1) DIVERSION_PATHID
      READ(FILE1) DIVERSION_BEGIN_TIME
      READ(FILE1) DIVERSION_END_TIME
      READ(FILE1) DIVERSION_LOCATION
      READ(FILE1) DIVERSION_PERCENTAGE
      READ(FILE1) DIVERSION_SPEED
    ENDIF
    RETURN
    END SUBROUTINE

  END MODULE

     