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

  MODULE TEXT
    IMPLICIT NONE
    CHARACTER*2047 :: MSGTEXT
    INTEGER, PARAMETER :: M_INFO=1,  M_WARNING=2,  M_ERROR=3
    CHARACTER*120, ALLOCATABLE :: TEXT_BUFFER(:)
    INTEGER, ALLOCATABLE :: TEXT_TYPE(:)
    INTEGER :: NTEXT
    INTEGER :: MSG_POINTER
    CHARACTER*2047 :: ETEXT

    CONTAINS

    SUBROUTINE SENDTEXTMSG(MSGTYPE)
    USE SIMPARAMS
    INTEGER, INTENT(IN) :: MSGTYPE
    INTEGER LU6FLG
    COMMON /LLU6IO/ LU6FLG
    CHARACTER*120, ALLOCATABLE :: B(:)
    INTEGER, ALLOCATABLE :: A(:)
#ifdef TSIS_COMPATIBLE
    INCLUDE 'CORWIN.FI'
    !Send text to TSIS
    CALL OUTPUTSTRING(MSGTEXT, MSGTYPE, 0)
#else
    !Send text to the screen
    !WRITE(*,*) MSGTEXT(1:80)
    !Add text to the buffer
    IF(.NOT. ALLOCATED(TEXT_BUFFER)) THEN
      ALLOCATE(TEXT_BUFFER(1000))
      ALLOCATE(TEXT_TYPE(1000))
    ELSEIF(NTEXT .EQ. SIZE(TEXT_BUFFER)) THEN
      ALLOCATE(A(NTEXT+100))
      A(1:NTEXT) = TEXT_TYPE(1:NTEXT)
      DEALLOCATE(TEXT_TYPE)
      ALLOCATE(TEXT_TYPE(NTEXT+100))
      TEXT_TYPE(1:NTEXT) = A(1:NTEXT)
      ALLOCATE(B(NTEXT+100))
      B(1:NTEXT) = TEXT_BUFFER(1:NTEXT)
      DEALLOCATE(TEXT_BUFFER)
      ALLOCATE(TEXT_BUFFER(NTEXT+100))
      TEXT_BUFFER(1:NTEXT) = B(1:NTEXT)
      DEALLOCATE(A)
      DEALLOCATE(B)
    ENDIF
    NTEXT = NTEXT + 1
    TEXT_BUFFER(NTEXT) = MSGTEXT(1:120)
    TEXT_TYPE(NTEXT) = MSGTYPE
#endif        
    RETURN
    END SUBROUTINE
         
  END MODULE
