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

  MODULE GLOBAL_DATA
    INTEGER, PARAMETER :: I_FREEWAY = 1
    INTEGER, PARAMETER :: I_STREET = 2
    INTEGER, PARAMETER :: RED = 1
    INTEGER, PARAMETER :: YELLOW = 2
    INTEGER, PARAMETER :: GREEN = 3
    INTEGER, PARAMETER :: N_STREET_APPROACHES = 6
    INTEGER, PARAMETER :: N_STREET_LANES = 20
    INTEGER, PARAMETER :: N_FREEWAY_LANES = 20
    INTEGER, PARAMETER :: N_INTERFACE_LANES = 20
    INTEGER, PARAMETER :: N_AUXLANES = 10
    INTEGER, PARAMETER :: NDET_LANES = 20
    INTEGER :: MAX_NODE_NUMBER = 7999
    REAL    :: CFRICT(4) = 0.16
    REAL    :: DEFAULT_HOV_PCT = 1.0
    REAL    :: DLC_MULT = 0.5
    REAL    :: DRIVER_FAMPCT = .1
    REAL    :: FREEWAY_PCT_COOP = 0.2     
    REAL    :: HMIN = 3.0
    REAL    :: LAG_ACCEL = .3
    REAL    :: LAG_DECEL = .3
    INTEGER :: LAST_ID_USED
    REAL    :: LC_TIME_FREEWAY = 2.0
    REAL    :: LC_TIME_STREET = 3.0
    REAL    :: LT_JUMPER_PROB(N_STREET_LANES) = 0.38
    REAL    :: LT_LAGGER_PROB(3) = (/.5, .15, 0./)
    REAL    :: SPILLBACK_PROB(4) = (/.8, .4, 0., 0./)
    REAL    :: STOP_SPD = 3.0
    REAL    :: STREET_PCT_COOP = 0.5
    REAL    :: TURNSIGNAL_PROB(10) = 0.5
    REAL    :: TURNSIGNAL_DIST = 50.
    REAL    :: YIELD_SPD = 5.0
    INTEGER :: STD_WIDTH = 12
    REAL    :: MILES2FEET = 5280. / 3600.
    REAL    :: FEET2MILES = 3600. / 5280.
        
    CONTAINS
      
! ==================================================================================================
    SUBROUTINE SAVE_GLOBAL_DATA(FILE1)
    INTEGER, INTENT(IN) :: FILE1
! ----------------------------------------------------------------------

! --- Save static data.

    WRITE(FILE1) STOP_SPD
    WRITE(FILE1) YIELD_SPD
    WRITE(FILE1) FREEWAY_PCT_COOP     
    WRITE(FILE1) HMIN
    WRITE(FILE1) LC_TIME_FREEWAY
    WRITE(FILE1) LC_TIME_STREET
    WRITE(FILE1) DLC_MULT
    WRITE(FILE1) DEFAULT_HOV_PCT
    WRITE(FILE1) LAG_ACCEL
    WRITE(FILE1) LAG_DECEL
    WRITE(FILE1) DRIVER_FAMPCT
    WRITE(FILE1) SPILLBACK_PROB
    WRITE(FILE1) LT_JUMPER_PROB
    WRITE(FILE1) LT_LAGGER_PROB
    WRITE(FILE1) CFRICT
    RETURN
    END SUBROUTINE
      
! ==================================================================================================
    SUBROUTINE RESTORE_GLOBAL_DATA(FILE1)
    INTEGER, INTENT(IN) :: FILE1
! ----------------------------------------------------------------------

! --- Restore data.

    READ(FILE1) STOP_SPD
    READ(FILE1) YIELD_SPD
    READ(FILE1) FREEWAY_PCT_COOP     
    READ(FILE1) HMIN
    READ(FILE1) LC_TIME_FREEWAY
    READ(FILE1) LC_TIME_STREET
    READ(FILE1) DLC_MULT
    READ(FILE1) DEFAULT_HOV_PCT
    READ(FILE1) LAG_ACCEL
    READ(FILE1) LAG_DECEL
    READ(FILE1) DRIVER_FAMPCT
    READ(FILE1) SPILLBACK_PROB
    READ(FILE1) LT_JUMPER_PROB
    READ(FILE1) LT_LAGGER_PROB
    READ(FILE1) CFRICT
    RETURN
    END SUBROUTINE
        
  END MODULE
      
  MODULE SEEDS
    INTEGER :: ISEED1
    INTEGER :: ISEED2
    INTEGER :: ISEED3
    INTEGER :: FSEED
    INTEGER :: SSEED
        
    CONTAINS
      
! ==================================================================================================
    SUBROUTINE SAVE_SEEDS(FILE1)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: FILE1
! ----------------------------------------------------------------------
    WRITE(FILE1) ISEED1
    WRITE(FILE1) ISEED2
    WRITE(FILE1) ISEED3
    WRITE(FILE1) FSEED
    WRITE(FILE1) SSEED
    RETURN
    END SUBROUTINE
      
! ==================================================================================================
    SUBROUTINE RESTORE_SEEDS(FILE1)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: FILE1
! ----------------------------------------------------------------------
    READ(FILE1) ISEED1
    READ(FILE1) ISEED2
    READ(FILE1) ISEED3
    READ(FILE1) FSEED
    READ(FILE1) SSEED
    RETURN
    END SUBROUTINE
        
  END MODULE

