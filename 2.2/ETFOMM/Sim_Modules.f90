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

  MODULE SIMPARAMS
    IMPLICIT NONE
    REAL    :: SIMTIME  
    INTEGER :: EV_COUNT
    INTEGER :: EXITFLG
    INTEGER :: ERROR_FLAG
    INTEGER :: INITIALIZATION_END
    LOGICAL :: INITMODE
    REAL    :: PRIOR_CPU
    INTEGER :: RUN_NUMBER
    INTEGER :: SIM_START_TIME
    INTEGER :: SIMULATION_END
    LOGICAL :: SKIP_INIT
    INTEGER :: TYPE_OF_RUN
    LOGICAL :: READ_ANIMATION_RUN
    REAL    :: START_CPU
    REAL    :: START_TIMER
    INTEGER :: ERROR_COUNT
    INTEGER :: TIME_INTERVAL
    INTEGER :: TIME_PERIOD
    REAL    :: TIMESTEP
    INTEGER :: SYNC_REF_TIME, SYNCOF, STARTM
    INTEGER :: TPSECONDS(19)
    LOGICAL :: WRITE_MOES
    LOGICAL :: WEVRUN
    INTEGER :: N_PERIODS
    INTEGER :: TRANSITION_CODE
    LOGICAL :: WTRF
    LOGICAL :: USE_NTCIP = .FALSE.
    LOGICAL :: USE_DCS = .FALSE.
    logical :: use_tram_dcs = .false.
    LOGICAL :: LOGGING = .FALSE.
    LOGICAL :: USE_EXTERNAL_DETECTORS = .FALSE.
    INTEGER :: IROOT
    LOGICAL :: STOCHASTIC
    LOGICAL :: LIMIT_TO_MAXGREEN
    REAL    :: DZ_ENTRY_TIME
    REAL    :: DZ_EXIT_TIME
    LOGICAL :: READING_TRF
    LOGICAL :: READ_SPLITS = .FALSE.
    LOGICAL :: WRITE_SUPPLEMENTAL_FILES = .FALSE. 
    logical :: using_dual_entry = .false.
    !logical :: using_dual_entry = .true.
    
!DEC$ATTRIBUTES DLLEXPORT::SIMTIME
!DEC$ATTRIBUTES DLLEXPORT::TIMESTEP
!DEC$ATTRIBUTES DLLEXPORT::TIME_PERIOD
    
    CONTAINS
        
! ==================================================================================================
    SUBROUTINE SAVE_SIMPARAMS(FILE)
    INTEGER, INTENT(IN) :: FILE
    INTEGER :: I
! ----------------------------------------------------------------------
    WRITE(FILE) TYPE_OF_RUN
    WRITE(FILE) SIMTIME  
    WRITE(FILE) TIMESTEP
    WRITE(FILE) TIME_INTERVAL
    WRITE(FILE) INITIALIZATION_END
    WRITE(FILE) SIMULATION_END
    WRITE(FILE) SIM_START_TIME
    WRITE(FILE) (TPSECONDS(I), I = 1, 19)
    WRITE(FILE) INITMODE
    WRITE(FILE) ERROR_FLAG
    WRITE(FILE) START_CPU
    WRITE(FILE) PRIOR_CPU
    WRITE(FILE) START_TIMER
    WRITE(FILE) TIME_PERIOD
    WRITE(FILE) SKIP_INIT
    WRITE(FILE) ERROR_COUNT
    WRITE(FILE) WRITE_MOES
    WRITE(FILE) WEVRUN
    RETURN
    END SUBROUTINE
        
! ==================================================================================================
    SUBROUTINE RESTORE_SIMPARAMS(FILE)
    INTEGER, INTENT(IN) :: FILE
    INTEGER :: I
! ----------------------------------------------------------------------

! --- Restore data.

    READ(FILE) TYPE_OF_RUN
    READ(FILE) SIMTIME  
    READ(FILE) TIMESTEP
    READ(FILE) TIME_INTERVAL
    READ(FILE) INITIALIZATION_END
    READ(FILE) SIMULATION_END
    READ(FILE) SIM_START_TIME
    READ(FILE) (TPSECONDS(I), I = 1, 19)
    READ(FILE) INITMODE
    READ(FILE) ERROR_FLAG
    READ(FILE) START_CPU
    READ(FILE) PRIOR_CPU
    READ(FILE) START_TIMER
    READ(FILE) TIME_PERIOD
    READ(FILE) SKIP_INIT
    READ(FILE) ERROR_COUNT
    READ(FILE) WRITE_MOES
    READ(FILE) WEVRUN
    RETURN
    END SUBROUTINE
        
  END MODULE

