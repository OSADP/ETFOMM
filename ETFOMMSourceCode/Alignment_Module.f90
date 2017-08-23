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

  MODULE ADD_DROP_ALIGNMENTS
    USE GLOBAL_DATA
    IMPLICIT NONE
    INTEGER, ALLOCATABLE :: ADDRP_ALIGNMENT(:,:)
    INTEGER              :: NADDRP
    INTEGER              :: TOTAL_NADDRP
        
    CONTAINS
        
  ! ==================================================================================================
    SUBROUTINE SET_ADDRP_ALIGNMENT(IRALGN, I, N)
    INTEGER, INTENT(IN) :: IRALGN, I, N
  ! ----------------------------------------------------------------------
    IF(IRALGN .GT. TOTAL_NADDRP) THEN
      IF(TOTAL_NADDRP .EQ. 0) THEN
        CALL ALLOCATE_ALIGNMENT_ARRAYS(IRALGN)
      ELSE
        CALL REALLOCATE_ALIGNMENT_ARRAYS(IRALGN)
      ENDIF
    ENDIF
    ADDRP_ALIGNMENT(IRALGN, I) = N
    RETURN
        
    END SUBROUTINE
        
  ! ==================================================================================================
    SUBROUTINE ALLOCATE_ALIGNMENT_ARRAYS(N)
    INTEGER, INTENT(IN) :: N
  ! ----------------------------------------------------------------------
    IF(ALLOCATED(ADDRP_ALIGNMENT)) RETURN
    TOTAL_NADDRP = N
    ALLOCATE(ADDRP_ALIGNMENT(N, N_FREEWAY_LANES))
    ADDRP_ALIGNMENT = 0
    RETURN
    END SUBROUTINE
      
  ! ==================================================================================================
    SUBROUTINE DEALLOCATE_ALIGNMENT_ARRAYS
  ! ----------------------------------------------------------------------
    IF(ALLOCATED(ADDRP_ALIGNMENT)) DEALLOCATE(ADDRP_ALIGNMENT)
    RETURN
    END SUBROUTINE
      
  ! ==================================================================================================
    SUBROUTINE REALLOCATE_ALIGNMENT_ARRAYS(N)
    INTEGER, INTENT(IN) :: N
    INTEGER, ALLOCATABLE :: TEMP(:,:)
  ! ----------------------------------------------------------------------
    ALLOCATE(TEMP(NADDRP, N_FREEWAY_LANES))
    TEMP = 0
    TEMP(1:TOTAL_NADDRP, 1:N_FREEWAY_LANES) =  ADDRP_ALIGNMENT(1:TOTAL_NADDRP, 1:N_FREEWAY_LANES)
    DEALLOCATE(ADDRP_ALIGNMENT)
    ALLOCATE(ADDRP_ALIGNMENT(N, N_FREEWAY_LANES))
    TOTAL_NADDRP = N
    ADDRP_ALIGNMENT(1:NADDRP, 1:N_FREEWAY_LANES) = TEMP
    DEALLOCATE(TEMP)
    RETURN
    END SUBROUTINE
        
  ! ==================================================================================================
    SUBROUTINE SAVE_ALIGNMENTS(FILE)
    INTEGER, INTENT(IN) :: FILE
    INTEGER :: I, N
  ! ----------------------------------------------------------------------
  
  ! --- Save static data.
  
    WRITE(FILE) TOTAL_NADDRP
    DO I = 1, TOTAL_NADDRP
      WRITE(FILE) (ADDRP_ALIGNMENT(I, N), N = 1, N_FREEWAY_LANES)
    ENDDO
    RETURN
    END SUBROUTINE
        
  ! ==================================================================================================
    SUBROUTINE RESTORE_ALIGNMENTS(FILE)
    INTEGER, INTENT(IN) :: FILE
    INTEGER :: I, N
  ! ----------------------------------------------------------------------

  ! --- Restore data.
 
    READ(FILE) TOTAL_NADDRP
    IF(TOTAL_NADDRP .NE. 0) THEN
      IF(.NOT. ALLOCATED(ADDRP_ALIGNMENT)) ALLOCATE(ADDRP_ALIGNMENT(TOTAL_NADDRP, N_FREEWAY_LANES))
      DO I = 1, TOTAL_NADDRP
        READ(FILE) (ADDRP_ALIGNMENT(I, N), N = 1, N_FREEWAY_LANES)
      ENDDO
    ENDIF
    RETURN
    END SUBROUTINE
        
  END MODULE
