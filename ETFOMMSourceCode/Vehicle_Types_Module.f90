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

  MODULE VEHICLE_TYPES
    LOGICAL :: WRITE58 = .FALSE.
    LOGICAL :: WRITE71 = .FALSE.
    LOGICAL :: WRITE201 = .FALSE.
    INTEGER, PARAMETER :: NTYPES = 36
    INTEGER, PARAMETER :: FLEET_AUTO = 0
    INTEGER, PARAMETER :: FLEET_TRUCK = 1
    INTEGER, PARAMETER :: FLEET_CARPOOL = 2
    INTEGER, PARAMETER :: FLEET_BUS = 3
    INTEGER, PARAMETER :: FLEET_EV = 4
    INTEGER, PARAMETER :: FLEET_BIKE = 5
    INTEGER, PARAMETER :: FLEET_PED = 6

    !There is a problem using this flag when more than one time period is used.
    !A vehicle type may be used in a subsequent time period but not in the previous period(s).
    !Arrays are dimensioned based on the vehicle types used in the first time period only.
    LOGICAL :: TYPE_IS_USED(NTYPES) = .FALSE.
    
    REAL :: MAX_SPEED(NTYPES) = (/ 100.0, 100.0, 80.0, 70.0, 70.0, 70.0, 70.0, 100.0, 100.0 &
                                 , 100., 100., 10., 0., 0., 0., 0., 0., 0.                          &
                                 , 0., 0., 0., 0., 0., 0., 0., 0., 0.                               &
                                 , 0., 0., 0., 0., 0., 0., 0., 0., 0.                               &
                                 /)
    
    INTEGER :: VTLENGTH(NTYPES)                               &
                = (/17, 19, 38, 56, 56, 67, 43, 17, 19        &
                  , 30, 33, 6, 0, 0, 0, 0, 0, 0               &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                    /)
    REAL :: HDWY_FACTOR(NTYPES)                                           &
             = (/1.0, 1.0, 1.2, 1.2, 1.2, 1.2, 1.2, 1.0, 1.0              &
               , 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0              &
               ,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0             &
               ,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0             &
               /)
    REAL :: AVG_OCCS(NTYPES)                                              &
             = (/1.3, 1.3, 1.2, 1.2, 1.2, 1.2, 25.0, 2.5, 2.5             &
               , 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0              &
               ,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0             &
               ,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0             &
               /)
    REAL :: PCT_PITT(NTYPES)                                              &
             = (/1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0              &
               , 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0              &
               ,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0             &
               ,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0             &
               /)
    REAL :: PCT_IDM(NTYPES)                                               &
             = (/0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0              &
               , 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0              &
               ,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0             &
               ,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0             &
               /)
    REAL :: PCT_ACC(NTYPES)                                               &
             = (/0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0              &
               , 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0              &
               ,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0             &
               ,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0             &
               /)
    REAL :: PCT_CACC(NTYPES)                                               &
             = (/0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0              &
               , 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0              &
               ,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0             &
               ,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0             &
               /)
    REAL :: NEMDEC(NTYPES)                                                &
             = (/-8.0, -8.0, -8.0, -8.0, -8.0, -8.0, -8.0, -8.0, -8.0     &
               , -8.0, -8.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0            &
               ,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0             &
               ,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0             &
               /)
    INTEGER :: FLT_FREEWAY_AUTO(NTYPES)                       &
                = (/25, 75, 0, 0, 0, 0, 0, 0, 0               &
                  , 0, 0, 0, 0, 0, 0, 0, 0, 0                 &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                    /)
    INTEGER :: FLT_FREEWAY_TRUCK(NTYPES)                      &
                = (/0, 0, 31, 36, 24, 9, 0, 0, 0              &
                  , 0, 0, 0, 0, 0, 0, 0, 0, 0                 &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                    /)
    INTEGER :: FLT_FREEWAY_CARPOOL(NTYPES)                    &
                = (/0, 0, 0, 0, 0, 0, 0, 25, 75               &
                  , 0, 0, 0, 0, 0, 0, 0, 0, 0                 &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                    /)
    INTEGER :: FLT_FREEWAY_BUS(NTYPES)                        &
                = (/0, 0, 0, 0, 0, 0, 100, 0, 0               &
                  , 0, 0, 0, 0, 0, 0, 0, 0, 0                 &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                    /)
    INTEGER :: FLT_FREEWAY_EV(NTYPES)                         &
                = (/0, 0, 0, 0, 0, 0, 0, 0, 0                 &
                  , 50, 50, 0, 0, 0, 0, 0, 0, 0               &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                    /)
    INTEGER :: FLT_STREET_AUTO(NTYPES)                        &
                = (/25, 75, 0, 0, 0, 0, 0, 0, 0               &
                  , 0, 0, 0, 0, 0, 0, 0, 0, 0                 &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                    /)
    INTEGER :: FLT_STREET_TRUCK(NTYPES)                       &
                = (/0, 0, 100, 0, 0, 0, 0, 0, 0               &
                  , 0, 0, 0, 0, 0, 0, 0, 0, 0                 &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                    /)
    INTEGER :: FLT_STREET_CARPOOL(NTYPES)                     &
                = (/0, 0, 0, 0, 0, 0, 0, 0, 100               &
                  , 0, 0, 0, 0, 0, 0, 0, 0, 0                 &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                    /)
    INTEGER :: FLT_STREET_BUS(NTYPES)                         &
                = (/0, 0, 0, 0, 0, 0, 100, 0, 0               &
                  , 0, 0, 0, 0, 0, 0, 0, 0, 0                 &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                    /)
    INTEGER :: FLT_STREET_EV(NTYPES)                          &
                = (/0, 0, 0, 0, 0, 0, 0, 0, 0                 &
                  , 50, 50, 0, 0, 0, 0, 0, 0, 0               &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                    /)
    INTEGER :: FLT_STREET_BIKE(NTYPES)                        &
                = (/0, 0, 0, 0, 0, 0, 0, 0, 0                 &
                  , 0, 0, 100, 0, 0, 0, 0, 0, 0               &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                    /)
    INTEGER :: FLT_STREET_PED(NTYPES)                         &
                = (/0, 0, 0, 0, 0, 0, 0, 0, 0                 &
                  , 0, 0, 0, 0, 0, 0, 0, 0, 0                 &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                     ,  0, 0, 0, 0, 0, 0, 0, 0, 0             &
                    /)
        
    CONTAINS    

! ==================================================================================================
    SUBROUTINE DETERMINE_ACTIVE_VEHICLE_TYPES      
    INTEGER :: I
    LOGICAL :: DONE = .FALSE.
! ----------------------------------------------------------------------
    IF(.NOT. DONE) THEN
      DONE = .TRUE.
      DO I = 1, NTYPES
        IF(VTLENGTH(I) .NE. 0) TYPE_IS_USED(I) = .TRUE.
      ENDDO
    ENDIF
    END SUBROUTINE
    
! ==================================================================================================
    SUBROUTINE SAVE_VEHICLE_TYPES(FILE)
    INTEGER, INTENT(IN) :: FILE
! ----------------------------------------------------------------------

! --- Save static data.

    WRITE(FILE) VTLENGTH
    WRITE(FILE) HDWY_FACTOR
    WRITE(FILE) AVG_OCCS
    WRITE(FILE) NEMDEC
    WRITE(FILE) FLT_FREEWAY_AUTO
    WRITE(FILE) FLT_FREEWAY_TRUCK
    WRITE(FILE) FLT_FREEWAY_CARPOOL
    WRITE(FILE) FLT_FREEWAY_BUS
    WRITE(FILE) FLT_FREEWAY_EV
    WRITE(FILE) FLT_STREET_AUTO
    WRITE(FILE) FLT_STREET_TRUCK
    WRITE(FILE) FLT_STREET_CARPOOL
    WRITE(FILE) FLT_STREET_BUS
    WRITE(FILE) FLT_STREET_EV
    RETURN
    END SUBROUTINE
      
! ==================================================================================================
    SUBROUTINE RESTORE_VEHICLE_TYPES(FILE)
    INTEGER, INTENT(IN) :: FILE
! ----------------------------------------------------------------------

! --- Restore data.

    READ(FILE) VTLENGTH
    READ(FILE) HDWY_FACTOR
    READ(FILE) AVG_OCCS
    READ(FILE) NEMDEC
    READ(FILE) FLT_FREEWAY_AUTO
    READ(FILE) FLT_FREEWAY_TRUCK
    READ(FILE) FLT_FREEWAY_CARPOOL
    READ(FILE) FLT_FREEWAY_BUS
    READ(FILE) FLT_FREEWAY_EV
    READ(FILE) FLT_STREET_AUTO
    READ(FILE) FLT_STREET_TRUCK
    READ(FILE) FLT_STREET_CARPOOL
    READ(FILE) FLT_STREET_BUS
    READ(FILE) FLT_STREET_EV
    RETURN
    END SUBROUTINE
      
  END MODULE
