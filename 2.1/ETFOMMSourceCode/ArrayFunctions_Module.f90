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

  MODULE ARRAY_FUNCTIONS

    CONTAINS
      
! ==================================================================================================
    SUBROUTINE REALLOCATE_INTEGER(A, OLD, NEW)
    INTEGER, ALLOCATABLE, INTENT(INOUT) :: A(:)
    INTEGER, INTENT(IN) :: OLD, NEW
    INTEGER, ALLOCATABLE :: TEMP(:)
! ----------------------------------------------------------------------
    ALLOCATE(TEMP(OLD))
    TEMP(1:OLD) = A(1:OLD)
    DEALLOCATE(A)
    ALLOCATE(A(NEW))
    A(1:OLD) = TEMP(1:OLD)
    A(OLD+1:NEW) = 0
    DEALLOCATE(TEMP)
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE REALLOCATE_INTEGER_X(A, OLD, NEW, X)
    INTEGER, ALLOCATABLE, INTENT(INOUT) :: A(:,:)
    INTEGER, INTENT(IN) :: OLD, NEW, X
    INTEGER, ALLOCATABLE :: TEMP(:,:)
! ----------------------------------------------------------------------
    ALLOCATE(TEMP(OLD, X))
    TEMP(1:OLD, 1:X) = A(1:OLD, 1:X)
    DEALLOCATE(A)
    ALLOCATE(A(NEW, X))
    A(1:OLD, 1:X) = TEMP(1:OLD, 1:X)
    A(OLD+1:NEW, 1:X) = 0
    DEALLOCATE(TEMP)
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE REALLOCATE_INTEGER_XY(A, OLD, NEW, X, Y)
    INTEGER, ALLOCATABLE, INTENT(INOUT) :: A(:,:,:)
    INTEGER, INTENT(IN) :: OLD, NEW, X, Y
    INTEGER, ALLOCATABLE :: TEMP(:,:,:)
! ----------------------------------------------------------------------
    ALLOCATE(TEMP(OLD, X, Y))
    TEMP(1:OLD, 1:X, 1:Y) = A(1:OLD, 1:X, 1:Y)
    DEALLOCATE(A)
    ALLOCATE(A(NEW, X, Y))
    A(1:OLD, 1:X, 1:Y) = TEMP(1:OLD, 1:X, 1:Y)
    A(OLD+1:NEW, 1:X, 1:Y) = 0
    DEALLOCATE(TEMP)
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE REALLOCATE_REAL_XY(A, OLD, NEW, X, Y)
    REAL, ALLOCATABLE, INTENT(INOUT) :: A(:,:,:)
    INTEGER, INTENT(IN) :: OLD, NEW, X, Y
    REAL, ALLOCATABLE :: TEMP(:,:,:)
! ----------------------------------------------------------------------
    ALLOCATE(TEMP(OLD, X, Y))
    TEMP(1:OLD, 1:X, 1:Y) = A(1:OLD, 1:X, 1:Y)
    DEALLOCATE(A)
    ALLOCATE(A(NEW, X, Y))
    A(1:OLD, 1:X, 1:Y) = TEMP(1:OLD, 1:X, 1:Y)
    A(OLD+1:NEW, 1:X, 1:Y) = 0
    DEALLOCATE(TEMP)
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE REALLOCATE_LOGICAL_X(A, OLD, NEW, X)
    LOGICAL, ALLOCATABLE, INTENT(INOUT) :: A(:,:)
    INTEGER, INTENT(IN) :: OLD, NEW, X
    LOGICAL, ALLOCATABLE :: TEMP(:,:)
! ----------------------------------------------------------------------
    ALLOCATE(TEMP(OLD, X))
    TEMP(1:OLD, 1:X) = A(1:OLD, 1:X)
    DEALLOCATE(A)
    ALLOCATE(A(NEW, X))
    A(1:OLD, 1:X) = TEMP(1:OLD, 1:X)
    A(OLD+1:NEW, 1:X) = .FALSE.
    DEALLOCATE(TEMP)
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE REALLOCATE_LOGICAL_XY(A, OLD, NEW, X, Y)
    LOGICAL, ALLOCATABLE, INTENT(INOUT) :: A(:,:,:)
    INTEGER, INTENT(IN) :: OLD, NEW, X, Y
    LOGICAL, ALLOCATABLE :: TEMP(:,:,:)
! ----------------------------------------------------------------------
    ALLOCATE(TEMP(OLD, X, Y))
    TEMP(1:OLD, 1:X, 1:Y) = A(1:OLD, 1:X, 1:Y)
    DEALLOCATE(A)
    ALLOCATE(A(NEW, X, Y))
    A(1:OLD, 1:X, 1:Y) = TEMP(1:OLD, 1:X, 1:Y)
    A(OLD+1:NEW, 1:X, 1:Y) = .FALSE.
    DEALLOCATE(TEMP)
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE REALLOCATE_REAL_X(A, OLD, NEW, X)
    REAL, ALLOCATABLE, INTENT(INOUT) :: A(:,:)
    INTEGER, INTENT(IN) :: OLD, NEW, X
    REAL, ALLOCATABLE :: TEMP(:,:)
! ----------------------------------------------------------------------
    ALLOCATE(TEMP(OLD, X))
    TEMP(1:OLD, 1:X) = A(1:OLD, 1:)
    DEALLOCATE(A)
    ALLOCATE(A(NEW, X))
    A(1:OLD, 1:X) = TEMP(1:OLD, 1:X)
    A(OLD+1:NEW, 1:X) = 0
    DEALLOCATE(TEMP)
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE REALLOCATE_REAL(A, OLD, NEW)
    REAL, ALLOCATABLE, INTENT(INOUT) :: A(:)
    INTEGER, INTENT(IN) :: OLD, NEW
    REAL, ALLOCATABLE :: TEMP(:)
! ----------------------------------------------------------------------
    ALLOCATE(TEMP(OLD))
    TEMP(1:OLD) = A(1:OLD)
    DEALLOCATE(A)
    ALLOCATE(A(NEW))
    A(1:OLD) = TEMP(1:OLD)
    A(OLD+1:NEW) = 0.
    DEALLOCATE(TEMP)
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE REALLOCATE_LOGICAL(A, OLD, NEW)
    LOGICAL, ALLOCATABLE, INTENT(INOUT) :: A(:)
    INTEGER, INTENT(IN) :: OLD, NEW
    LOGICAL, ALLOCATABLE :: TEMP(:)
! ----------------------------------------------------------------------
    ALLOCATE(TEMP(OLD))
    TEMP(1:OLD) = A(1:OLD)
    DEALLOCATE(A)
    ALLOCATE(A(NEW))
    A(1:OLD) = TEMP(1:OLD)
    A(OLD+1:NEW) = .FALSE.
    DEALLOCATE(TEMP)
    END SUBROUTINE
      
  END MODULE
       
