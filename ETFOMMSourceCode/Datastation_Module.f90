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

  MODULE DATASTATIONS
    USE GLOBAL_DATA
    IMPLICIT NONE
    TYPE DATASTATION_DATA
      INTEGER :: HDWY_COUNT(N_FREEWAY_LANES) = 0
      REAL    :: HDWY_TOTAL(N_FREEWAY_LANES) = 0
      INTEGER :: LINK = 0
      INTEGER :: LOCATION = 0
      INTEGER :: STATION_ID = 0
      INTEGER :: COUNT(N_FREEWAY_LANES) = 0
      REAL    :: SPEED_TOTAL(N_FREEWAY_LANES) = 0
    END TYPE

    INTEGER :: NUMBER_OF_DATASTATIONS
    TYPE(DATASTATION_DATA), ALLOCATABLE :: DATASTATION(:)
        
    CONTAINS
        
! ==================================================================================================
    SUBROUTINE ALLOCATE_DATASTATION_ARRAYS
    IF(ALLOCATED(DATASTATION)) RETURN
    ALLOCATE(DATASTATION(NUMBER_OF_DATASTATIONS))
    RETURN
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE DEALLOCATE_DATASTATION_ARRAYS
    IF(ALLOCATED(DATASTATION)) DEALLOCATE(DATASTATION)
    RETURN
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE SAVE_DATASTATIONS(FILE1, FILE2, FIRST)
    USE SIMPARAMS
    INTEGER, INTENT(IN) :: FILE1, FILE2
    LOGICAL, INTENT(IN) :: FIRST
    INTEGER :: I, N
! ----------------------------------------------------------------------

! --- Save static data.

    IF(FIRST) THEN
    WRITE(FILE1) NUMBER_OF_DATASTATIONS
      IF(NUMBER_OF_DATASTATIONS .NE. 0) THEN
        DO I = 1, NUMBER_OF_DATASTATIONS
          IF(DATASTATION(I)%LINK .NE. 0) THEN
            WRITE(FILE1) DATASTATION(I)%LINK
            WRITE(FILE1) DATASTATION(I)%LOCATION
            WRITE(FILE1) DATASTATION(I)%STATION_ID
          ENDIF
        ENDDO
      ENDIF

! --- Save dynamic data.

      DO I = 1, NUMBER_OF_DATASTATIONS
        IF(DATASTATION(I)%LINK .NE. 0) THEN
          WRITE(FILE2) (DATASTATION(I)%HDWY_COUNT(N), N = 1, N_FREEWAY_LANES)
          WRITE(FILE2) (DATASTATION(I)%COUNT(N), N = 1, N_FREEWAY_LANES)
          WRITE(FILE2) (DATASTATION(I)%HDWY_TOTAL(N), N = 1, N_FREEWAY_LANES)
          WRITE(FILE2) (DATASTATION(I)%SPEED_TOTAL(N), N = 1, N_FREEWAY_LANES)
        ENDIF
      ENDDO
    ENDIF
    RETURN
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE RESTORE_DATASTATIONS(FILE1, FILE2)
    USE SIMPARAMS
    INTEGER, INTENT(IN) :: FILE1, FILE2
    INTEGER :: I, N
! ----------------------------------------------------------------------

! --- Restore data.

    READ(FILE1) NUMBER_OF_DATASTATIONS
    IF(NUMBER_OF_DATASTATIONS .NE. 0) THEN
      CALL ALLOCATE_DATASTATION_ARRAYS
      DO I = 1, NUMBER_OF_DATASTATIONS
        IF(DATASTATION(I)%LINK .NE. 0) THEN
          READ(FILE1) DATASTATION(I)%LINK
          READ(FILE1) DATASTATION(I)%LOCATION
          READ(FILE1) DATASTATION(I)%STATION_ID
        ENDIF
      ENDDO
      DO I = 1, NUMBER_OF_DATASTATIONS
        IF(DATASTATION(I)%LINK .NE. 0) THEN
          READ(FILE2) (DATASTATION(I)%HDWY_COUNT(N), N = 1, N_FREEWAY_LANES)
          READ(FILE2) (DATASTATION(I)%COUNT(N), N = 1, N_FREEWAY_LANES)
          READ(FILE2) (DATASTATION(I)%HDWY_TOTAL(N), N = 1, N_FREEWAY_LANES)
          READ(FILE2) (DATASTATION(I)%SPEED_TOTAL(N), N = 1, N_FREEWAY_LANES)
        ENDIF
      ENDDO
    ENDIF
    RETURN
    END SUBROUTINE

  END MODULE
