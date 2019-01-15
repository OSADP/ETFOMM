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

  MODULE BUS_ROUTE_DATA    
    IMPLICIT NONE
    INTEGER :: NUMBER_OF_ROUTES
    INTEGER, ALLOCATABLE :: BUSR_HDWY(:)
    INTEGER, ALLOCATABLE :: BUSR_NNODES(:)
    INTEGER, ALLOCATABLE :: BUSR_OFFSET(:)
    INTEGER, ALLOCATABLE :: BUSR_PERSONTRIPS(:)
    INTEGER, ALLOCATABLE :: BUSR_ROUTE_NODES(:,:)
    INTEGER, ALLOCATABLE :: BUSR_STATIONLIST(:,:)
    REAL,    ALLOCATABLE :: BUSR_TIMER(:)
    REAL,    ALLOCATABLE :: BUSR_TRAVELTIME(:)
    INTEGER, ALLOCATABLE :: BUSR_TRIPS(:)
    LOGICAL, ALLOCATABLE :: WRITE189(:,:)
    INTEGER, ALLOCATABLE :: ROUTE_HEADWAY(:,:)
    INTEGER, ALLOCATABLE :: ROUTE_OFFSET(:,:)
        
    CONTAINS
      
! ==================================================================================================
    SUBROUTINE ALLOCATE_BUS_ROUTE_DATA_ARRAYS
    IF(.NOT. ALLOCATED(BUSR_ROUTE_NODES)) THEN
      ALLOCATE(BUSR_HDWY(NUMBER_OF_ROUTES))
      ALLOCATE(BUSR_NNODES(NUMBER_OF_ROUTES))
      ALLOCATE(BUSR_OFFSET(NUMBER_OF_ROUTES))
      ALLOCATE(BUSR_PERSONTRIPS(NUMBER_OF_ROUTES))
      ALLOCATE(BUSR_ROUTE_NODES(NUMBER_OF_ROUTES, 200))
      ALLOCATE(BUSR_TIMER(NUMBER_OF_ROUTES))
      ALLOCATE(BUSR_TRAVELTIME(NUMBER_OF_ROUTES))
      ALLOCATE(BUSR_TRIPS(NUMBER_OF_ROUTES))
      ALLOCATE(BUSR_STATIONLIST(NUMBER_OF_ROUTES, 100))
      ALLOCATE(WRITE189(NUMBER_OF_ROUTES, 19))
      ALLOCATE(ROUTE_HEADWAY(NUMBER_OF_ROUTES, 19))
      ALLOCATE(ROUTE_OFFSET(NUMBER_OF_ROUTES, 19))
      BUSR_HDWY = 0
      BUSR_NNODES = 0
      BUSR_OFFSET = 0
      BUSR_PERSONTRIPS = 0
      BUSR_ROUTE_NODES = 0
      BUSR_TRAVELTIME = 0
      BUSR_TRIPS = 0
      BUSR_TIMER = 0
      BUSR_STATIONLIST = 0
      WRITE189 = .FALSE.
      ROUTE_HEADWAY = 0
      ROUTE_OFFSET = 0
    ENDIF
    END SUBROUTINE
      
! ==================================================================================================
    SUBROUTINE DEALLOCATE_BUS_ROUTE_DATA_ARRAYS
    IF(ALLOCATED(BUSR_NNODES)) DEALLOCATE(BUSR_NNODES)
    IF(ALLOCATED(BUSR_ROUTE_NODES)) DEALLOCATE(BUSR_ROUTE_NODES)
    IF(ALLOCATED(BUSR_HDWY)) DEALLOCATE(BUSR_HDWY)
    IF(ALLOCATED(BUSR_TIMER)) DEALLOCATE(BUSR_TIMER)
    IF(ALLOCATED(BUSR_OFFSET)) DEALLOCATE(BUSR_OFFSET)
    IF(ALLOCATED(BUSR_STATIONLIST)) DEALLOCATE(BUSR_STATIONLIST)
    IF(ALLOCATED(BUSR_TRAVELTIME)) DEALLOCATE(BUSR_TRAVELTIME)
    IF(ALLOCATED(BUSR_TRIPS)) DEALLOCATE(BUSR_TRIPS)
    IF(ALLOCATED(BUSR_PERSONTRIPS)) DEALLOCATE(BUSR_PERSONTRIPS)
    IF(ALLOCATED(WRITE189)) DEALLOCATE(WRITE189)
    IF(ALLOCATED(ROUTE_HEADWAY)) DEALLOCATE(ROUTE_HEADWAY)
    IF(ALLOCATED(ROUTE_OFFSET)) DEALLOCATE(ROUTE_OFFSET)
    END SUBROUTINE
        
! ==================================================================================================
    SUBROUTINE SAVE_BUS_ROUTE_DATA(FILE)
    INTEGER, INTENT(IN) :: FILE
    INTEGER :: I, N
! ----------------------------------------------------------------------

! --- Save static data.

    WRITE(FILE) NUMBER_OF_ROUTES
    DO I = 1, NUMBER_OF_ROUTES 
      WRITE(FILE) BUSR_NNODES(I)
      WRITE(FILE) (BUSR_ROUTE_NODES(I, N), N = 1, 200)
      WRITE(FILE) BUSR_HDWY(I)
      WRITE(FILE) BUSR_TIMER(I)
      WRITE(FILE) BUSR_OFFSET(I)
      WRITE(FILE) (BUSR_STATIONLIST(I, N), N = 1, 100)
      WRITE(FILE) BUSR_TRAVELTIME(I)
      WRITE(FILE) BUSR_TRIPS(I)
      WRITE(FILE) BUSR_PERSONTRIPS(I)
    ENDDO
    RETURN
    END SUBROUTINE
        
! ==================================================================================================
    SUBROUTINE RESTORE_BUS_ROUTE_DATA(FILE)
    INTEGER, INTENT(IN) :: FILE
    INTEGER :: I, N
! ----------------------------------------------------------------------

! --- Restore data.

    READ(FILE) NUMBER_OF_ROUTES
    IF(NUMBER_OF_ROUTES .NE. 0) THEN
      CALL ALLOCATE_BUS_ROUTE_DATA_ARRAYS
      DO I = 1, NUMBER_OF_ROUTES 
        READ(FILE) BUSR_NNODES(I)
        READ(FILE) (BUSR_ROUTE_NODES(I, N), N = 1, 200)
        READ(FILE) BUSR_HDWY(I)
        READ(FILE) BUSR_TIMER(I)
        READ(FILE) BUSR_OFFSET(I)
        READ(FILE) (BUSR_STATIONLIST(I, N), N = 1, 100)
        READ(FILE) BUSR_TRAVELTIME(I)
        READ(FILE) BUSR_TRIPS(I)
        READ(FILE) BUSR_PERSONTRIPS(I)
      ENDDO
    ENDIF
    RETURN
    END SUBROUTINE        
  END MODULE
      
  MODULE BUS_STATION_DATA
    IMPLICIT NONE
    INTEGER :: NUMBER_OF_BUSSTATIONS = 0
    TYPE BUS_STATION
      INTEGER :: STATION_NUMBER = 0
      INTEGER :: BLOCK_CODE = 0
      REAL    :: BYPASS_PCT = 0
      INTEGER :: CAPACITY = 0
      INTEGER :: COUNT = 0
      INTEGER :: DWELL = 0
      REAL    :: DWELL_TIME = 0
      REAL    :: EMPTY_TIME = 0
      INTEGER :: FRONT = 0
      INTEGER :: LINK = 0
      INTEGER :: LOCATION = 0
      INTEGER :: NEXT_STATION = 0
      REAL    :: OVERFLOW_TIME = 0
      INTEGER :: POCKET_LANE = 0
      INTEGER :: TYPE_CODE = 0
    END TYPE
      
    TYPE(BUS_STATION), ALLOCATABLE :: BUS_STATION_LIST(:)
    REAL :: DWELL_MULTIPLIER(6, 10)
    LOGICAL, ALLOCATABLE :: WRITE186(:,:)
    REAL, ALLOCATABLE :: STATION_DWELL(:,:)
    REAL, ALLOCATABLE :: STATION_BYPASS_PCT(:,:)
    
    CONTAINS
      
! ==================================================================================================
    SUBROUTINE ALLOCATE_BUS_STATION_ARRAYS
    IF(.NOT. ALLOCATED(BUS_STATION_LIST)) THEN
      ALLOCATE(BUS_STATION_LIST(NUMBER_OF_BUSSTATIONS))
      ALLOCATE(WRITE186(NUMBER_OF_BUSSTATIONS, 19))
      ALLOCATE(STATION_DWELL(NUMBER_OF_BUSSTATIONS, 19))
      ALLOCATE(STATION_BYPASS_PCT(NUMBER_OF_BUSSTATIONS, 19))
      WRITE186 = .FALSE.
      STATION_DWELL = 0.
      STATION_BYPASS_PCT = 0.
    ENDIF
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE DEFINE_DWELL_MULTIPLIERS
    INTEGER :: DM1(10) = (/40, 60, 70, 80, 90, 100, 120, 130, 140, 170/)
    INTEGER :: DM2(10) = (/24, 48, 59, 75, 85, 94, 111, 126, 155, 223/) 
    INTEGER :: DM3(10) = (/30, 47, 65, 77, 90, 103, 116, 137, 157, 178/)
    INTEGER :: DM4(10) = (/0, 29, 59, 75, 92, 108, 125, 148, 170, 194/) 
    INTEGER :: DM5(10) = (/0, 18, 36, 70, 104, 125, 144, 156, 167, 180/)
    INTEGER :: DM6(10) = (/0, 0, 0, 48, 96, 120, 144, 171, 198, 223/) 
! ----------------------------------------------------------------------
    DWELL_MULTIPLIER(1, 1:10) = DM1(1:10) / 100.
    DWELL_MULTIPLIER(2, 1:10) = DM2(1:10) / 100.
    DWELL_MULTIPLIER(3, 1:10) = DM3(1:10) / 100.
    DWELL_MULTIPLIER(4, 1:10) = DM4(1:10) / 100.
    DWELL_MULTIPLIER(5, 1:10) = DM5(1:10) / 100.
    DWELL_MULTIPLIER(6, 1:10) = DM6(1:10) / 100.
    END SUBROUTINE
        
! ==================================================================================================
    SUBROUTINE DEALLOCATE_BUS_STATION_DATA
    IF(ALLOCATED(BUS_STATION_LIST)) DEALLOCATE(BUS_STATION_LIST)
    IF(ALLOCATED(WRITE186)) DEALLOCATE(WRITE186)
    IF(ALLOCATED(STATION_DWELL)) DEALLOCATE(STATION_DWELL)
    IF(ALLOCATED(STATION_BYPASS_PCT)) DEALLOCATE(STATION_BYPASS_PCT)
    RETURN
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE SAVE_BUS_STATION_DATA(FILE1, FILE2, FIRST)
    USE SIMPARAMS
    INTEGER, INTENT(IN) :: FILE1, FILE2
    LOGICAL, INTENT(IN) :: FIRST
    INTEGER :: I
! ----------------------------------------------------------------------

! --- Save static data.

    IF(.NOT. ALLOCATED(BUS_STATION_LIST)) THEN
      WRITE(FILE1) 0
    ELSE
      WRITE(FILE1) NUMBER_OF_BUSSTATIONS
      IF(FIRST) THEN
        DO I = 1, NUMBER_OF_BUSSTATIONS
          IF(BUS_STATION_LIST(I)%LINK .NE. 0) THEN
            WRITE(FILE1) BUS_STATION_LIST(I)%BLOCK_CODE
            WRITE(FILE1) BUS_STATION_LIST(I)%LINK
            WRITE(FILE1) BUS_STATION_LIST(I)%POCKET_LANE
            WRITE(FILE1) BUS_STATION_LIST(I)%LOCATION
            WRITE(FILE1) BUS_STATION_LIST(I)%CAPACITY
            WRITE(FILE1) BUS_STATION_LIST(I)%TYPE_CODE
            WRITE(FILE1) BUS_STATION_LIST(I)%DWELL
            WRITE(FILE1) BUS_STATION_LIST(I)%NEXT_STATION
            WRITE(FILE1) BUS_STATION_LIST(I)%BYPASS_PCT
          ENDIF
        ENDDO
      ENDIF
      DO I = 1, NUMBER_OF_BUSSTATIONS
        IF(BUS_STATION_LIST(I)%LINK .NE. 0) THEN
          WRITE(FILE2) BUS_STATION_LIST(I)%FRONT
          WRITE(FILE2) BUS_STATION_LIST(I)%COUNT
          WRITE(FILE2) BUS_STATION_LIST(I)%DWELL_TIME
          WRITE(FILE2) BUS_STATION_LIST(I)%EMPTY_TIME
          WRITE(FILE2) BUS_STATION_LIST(I)%OVERFLOW_TIME
        ENDIF
      ENDDO
    ENDIF
    RETURN
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE RESTORE_BUS_STATION_DATA(FILE1, FILE2)
    USE SIMPARAMS
    INTEGER, INTENT(IN) :: FILE1, FILE2
    INTEGER :: I, NSTAT
! ----------------------------------------------------------------------

! --- Restore data.

    READ(FILE1) NUMBER_OF_BUSSTATIONS
    IF(NUMBER_OF_BUSSTATIONS .NE. 0) THEN
      IF(.NOT. ALLOCATED(BUS_STATION_LIST)) ALLOCATE(BUS_STATION_LIST(NUMBER_OF_BUSSTATIONS))
      DO I = 1, NUMBER_OF_BUSSTATIONS
        READ(FILE1) BUS_STATION_LIST(I)%BLOCK_CODE
        READ(FILE1) BUS_STATION_LIST(I)%LINK
        READ(FILE1) BUS_STATION_LIST(I)%POCKET_LANE
        READ(FILE1) BUS_STATION_LIST(I)%LOCATION
        READ(FILE1) BUS_STATION_LIST(I)%CAPACITY
        READ(FILE1) BUS_STATION_LIST(I)%TYPE_CODE
        READ(FILE1) BUS_STATION_LIST(I)%DWELL
        READ(FILE1) BUS_STATION_LIST(I)%NEXT_STATION
        READ(FILE1) BUS_STATION_LIST(I)%BYPASS_PCT
      ENDDO
      DO I = 1, NUMBER_OF_BUSSTATIONS
        READ(FILE2) BUS_STATION_LIST(I)%FRONT
        READ(FILE2) BUS_STATION_LIST(I)%COUNT
        READ(FILE2) BUS_STATION_LIST(I)%DWELL_TIME
        READ(FILE2) BUS_STATION_LIST(I)%EMPTY_TIME
        READ(FILE2) BUS_STATION_LIST(I)%OVERFLOW_TIME
      ENDDO
    ENDIF
    RETURN
    END SUBROUTINE

  END MODULE
