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

  MODULE DETECTORS
    USE GLOBAL_DATA
    IMPLICIT NONE
    TYPE DETECTOR_DATA
      REAL    :: CARRYOVER_TIME = 0
      INTEGER :: CURRENT_COUNT = 0
      INTEGER :: CURRENT_STATE = 0
      INTEGER :: CURRENT_STATE_TEMP(10)
      INTEGER :: PREVIOUS_STATE
      INTEGER :: PREVIOUS_STATE_TEMP(10)
      REAL    :: DELAY_TIME = 0
      INTEGER :: HDWY_COUNT = 0
      REAL    :: HDWY_TOTAL = 0
      INTEGER :: LINK = 0
      INTEGER :: SIGNAL_NODE = 0
      INTEGER :: LANE1 = 0
      INTEGER :: LANE2 = 0
      INTEGER :: DETECTOR_LANES(NDET_LANES)
      REAL    :: LOCATION = 0
      INTEGER :: NEXT_DET = 0
      REAL    :: ON_TIME = 0
      INTEGER :: OPERATION_CODE = 0
      INTEGER :: TYPE_CODE = 0
      INTEGER :: COUNT = 0
      REAL    :: SPEED_TOTAL = 0
      INTEGER :: LENGTH_TOTAL = 0
      INTEGER :: STATION_ID = 0
      REAL    :: ZONE_LENGTH = 0.
      INTEGER :: ASSOCIATED_PHASE
      INTEGER :: DETECTION_ZONE
      REAL    :: LAST_ACTUATION_TIME
      INTEGER :: LAST_VEHICLE_ID
      REAL    :: LAST_SPEED
      INTEGER :: LAST_LENGTH
      INTEGER :: LANES_COVERED
    END TYPE
  END MODULE

  MODULE FREEWAY_DETECTORS
    USE DETECTORS
    IMPLICIT NONE
    INTEGER :: N_FREEWAY_DETECTORS
    TYPE(DETECTOR_DATA), ALLOCATABLE :: FDETECTOR(:)
        
    CONTAINS
        
! ==================================================================================================
    SUBROUTINE ALLOCATE_FREEWAY_DETECTOR_ARRAYS
    IF(ALLOCATED(FDETECTOR)) RETURN
    ALLOCATE(FDETECTOR(N_FREEWAY_DETECTORS))
    RETURN
    END SUBROUTINE
        
! ==================================================================================================
    SUBROUTINE DEALLOCATE_FREEWAY_DETECTOR_ARRAYS
    IF(ALLOCATED(FDETECTOR)) DEALLOCATE(FDETECTOR)
    RETURN
    END SUBROUTINE
        
! ==================================================================================================
    SUBROUTINE SAVE_FREEWAY_DETECTOR(FILE1, FILE2, FIRST)
    USE SIMPARAMS
    INTEGER, INTENT(IN) :: FILE1, FILE2
    LOGICAL, INTENT(IN) :: FIRST
    INTEGER :: I
! ----------------------------------------------------------------------

! --- Save static data.

    IF(FIRST) THEN
      WRITE(FILE1) N_FREEWAY_DETECTORS
      DO I = 1, N_FREEWAY_DETECTORS
        WRITE(FILE1) FDETECTOR(I)%LINK
        WRITE(FILE1) FDETECTOR(I)%LANE1
        WRITE(FILE1) FDETECTOR(I)%LOCATION
        WRITE(FILE1) FDETECTOR(I)%ZONE_LENGTH
        WRITE(FILE1) FDETECTOR(I)%TYPE_CODE
        WRITE(FILE1) FDETECTOR(I)%STATION_ID
        WRITE(FILE1) FDETECTOR(I)%NEXT_DET
      ENDDO
    ENDIF

! --- Save dynamic data.

    DO I = 1, N_FREEWAY_DETECTORS
      WRITE(FILE2) FDETECTOR(I)%HDWY_COUNT
      WRITE(FILE2) FDETECTOR(I)%COUNT
      WRITE(FILE2) FDETECTOR(I)%HDWY_TOTAL
      WRITE(FILE2) FDETECTOR(I)%SPEED_TOTAL
      WRITE(FILE2) FDETECTOR(I)%ON_TIME
    ENDDO
    RETURN
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE RESTORE_FREEWAY_DETECTOR(FILE1, FILE2)
    USE SIMPARAMS
    INTEGER, INTENT(IN) :: FILE1, FILE2
    INTEGER :: I
! ----------------------------------------------------------------------

! --- Restore data.

    READ(FILE1) N_FREEWAY_DETECTORS
    IF(N_FREEWAY_DETECTORS .NE. 0) THEN
      CALL ALLOCATE_FREEWAY_DETECTOR_ARRAYS
      DO I = 1, N_FREEWAY_DETECTORS
        READ(FILE1) FDETECTOR(I)%LINK
        READ(FILE1) FDETECTOR(I)%LANE1
        READ(FILE1) FDETECTOR(I)%LOCATION
        READ(FILE1) FDETECTOR(I)%ZONE_LENGTH
        READ(FILE1) FDETECTOR(I)%TYPE_CODE
        READ(FILE1) FDETECTOR(I)%STATION_ID
        READ(FILE1) FDETECTOR(I)%NEXT_DET
      ENDDO
      DO I = 1, N_FREEWAY_DETECTORS
        READ(FILE2) FDETECTOR(I)%HDWY_COUNT
        READ(FILE2) FDETECTOR(I)%COUNT
        READ(FILE2) FDETECTOR(I)%HDWY_TOTAL
        READ(FILE2) FDETECTOR(I)%SPEED_TOTAL
        READ(FILE2) FDETECTOR(I)%ON_TIME
      ENDDO
    ENDIF
    RETURN
    END SUBROUTINE   
  END MODULE
           
  MODULE STREET_DETECTORS
    USE DETECTORS
    IMPLICIT NONE
    INTEGER :: N_STREET_DETECTORS
    TYPE(DETECTOR_DATA), ALLOCATABLE :: SDETECTOR(:)
    TYPE(DETECTOR_DATA), ALLOCATABLE :: PED_DETECTOR(:)
    TYPE(DETECTOR_DATA), ALLOCATABLE :: transit_detector(:)

    CONTAINS
        
! ==================================================================================================
    SUBROUTINE ALLOCATE_STREET_DETECTOR_ARRAYS
    IF(ALLOCATED(SDETECTOR)) RETURN
    ALLOCATE(SDETECTOR(N_STREET_DETECTORS))
    ALLOCATE(PED_DETECTOR(N_STREET_DETECTORS))
    RETURN
    END SUBROUTINE
        
! ==================================================================================================
    SUBROUTINE DEALLOCATE_STREET_DETECTOR_ARRAYS
    IF(ALLOCATED(SDETECTOR)) DEALLOCATE(SDETECTOR)
    IF(ALLOCATED(PED_DETECTOR)) DEALLOCATE(PED_DETECTOR)
    RETURN
    END SUBROUTINE
        
! ==================================================================================================
    SUBROUTINE SAVE_STREET_DETECTOR(FILE1, FILE2, FIRST)
    USE SIMPARAMS
    INTEGER, INTENT(IN) :: FILE1, FILE2
    LOGICAL, INTENT(IN) :: FIRST
    INTEGER :: I
! ----------------------------------------------------------------------

! --- Save static data.

    IF(FIRST) THEN
      WRITE(FILE1) N_STREET_DETECTORS
      DO I = 1, N_STREET_DETECTORS
        WRITE(FILE1) SDETECTOR(I)%LINK
        WRITE(FILE1) SDETECTOR(I)%LANE1
        WRITE(FILE1) SDETECTOR(I)%LANE2
        WRITE(FILE1) SDETECTOR(I)%LOCATION
        WRITE(FILE1) SDETECTOR(I)%ZONE_LENGTH
        WRITE(FILE1) SDETECTOR(I)%OPERATION_CODE
        WRITE(FILE1) SDETECTOR(I)%STATION_ID
        WRITE(FILE1) SDETECTOR(I)%NEXT_DET
      ENDDO
    ENDIF

! --- Save dynamic data.

    DO I = 1, N_STREET_DETECTORS
      WRITE(FILE2) SDETECTOR(I)%HDWY_COUNT
      WRITE(FILE2) SDETECTOR(I)%COUNT
      WRITE(FILE2) SDETECTOR(I)%HDWY_TOTAL
      WRITE(FILE2) SDETECTOR(I)%SPEED_TOTAL
      WRITE(FILE2) SDETECTOR(I)%ON_TIME
    ENDDO
    RETURN
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE RESTORE_STREET_DETECTOR(FILE1, FILE2)
    USE SIMPARAMS
    INTEGER, INTENT(IN) :: FILE1, FILE2
    INTEGER :: I
! ----------------------------------------------------------------------

! --- Restore data.

    READ(FILE1) N_STREET_DETECTORS
    IF(N_STREET_DETECTORS .NE. 0) THEN
      CALL ALLOCATE_STREET_DETECTOR_ARRAYS
      DO I = 1, N_STREET_DETECTORS
        READ(FILE1) SDETECTOR(I)%LINK
        READ(FILE1) SDETECTOR(I)%LANE1
        READ(FILE1) SDETECTOR(I)%LANE2
        READ(FILE1) SDETECTOR(I)%LOCATION
        READ(FILE1) SDETECTOR(I)%ZONE_LENGTH
        READ(FILE1) SDETECTOR(I)%OPERATION_CODE
        READ(FILE1) SDETECTOR(I)%STATION_ID
        READ(FILE1) SDETECTOR(I)%NEXT_DET
      ENDDO
      DO I = 1, N_STREET_DETECTORS
        READ(FILE2) SDETECTOR(I)%HDWY_COUNT
        READ(FILE2) SDETECTOR(I)%COUNT
        READ(FILE2) SDETECTOR(I)%HDWY_TOTAL
        READ(FILE2) SDETECTOR(I)%SPEED_TOTAL
        READ(FILE2) SDETECTOR(I)%ON_TIME
      ENDDO
    ENDIF
    RETURN
    END SUBROUTINE
           
  END MODULE
           
