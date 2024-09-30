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

  MODULE EV_DATA
    USE GLOBAL_DATA
    IMPLICIT NONE
    INTEGER :: LATDIST
    INTEGER, ALLOCATABLE :: PREEMPT_EV(:)
    INTEGER, ALLOCATABLE :: PREEMPT_LINK(:)
    LOGICAL, ALLOCATABLE :: PREEMPT_FLAG(:)
    INTEGER :: KDIST1(10) = (/50, 100, 150, 200, 300, 400, 500, 700, 1000, 1500/)
    INTEGER :: KDIST2(10) = (/50, 100, 150, 200, 300, 400, 500, 700, 1000, 1500/)
    INTEGER :: KDIST3(10) = (/25, 50, 75, 100, 150, 200, 250, 300, 400, 500/)
    INTEGER :: KRN1(10) = (/99, 90, 70, 50, 40, 30, 20, 10, 5, 2/)
    INTEGER :: KRN2(10) = (/99, 90, 70, 50, 40, 30, 20, 10, 5, 2/)
    INTEGER :: KRN3(10) = (/99, 90, 70, 50, 40, 30, 20, 10, 5, 2/)
    INTEGER :: KSPEED(10) = (/10, 20, 30, 40, 50, 60, 70, 80, 90, 100/)
    INTEGER :: KRED(10) =  (/10, 20, 30, 40, 50, 50, 50, 50, 50, 50/)
     
    CONTAINS

! ==========================================================================================================
    SUBROUTINE ALLOCATE_EV_DATA_ARRAYS
    IF(ALLOCATED(PREEMPT_EV)) RETURN
    ALLOCATE(PREEMPT_EV(MAX_NODE_NUMBER))
    ALLOCATE(PREEMPT_LINK(MAX_NODE_NUMBER))
    ALLOCATE(PREEMPT_FLAG(MAX_NODE_NUMBER))
    PREEMPT_EV = 0
    PREEMPT_LINK = 0
    PREEMPT_FLAG = .FALSE.
    RETURN
    END SUBROUTINE
        
! ==========================================================================================================
    SUBROUTINE DEALLOCATE_EV_DATA_ARRAYS
    IF(ALLOCATED(PREEMPT_EV)) THEN
      DEALLOCATE(PREEMPT_EV)
      DEALLOCATE(PREEMPT_LINK)
      DEALLOCATE(PREEMPT_FLAG)
    ENDIF
    RETURN
    END SUBROUTINE
        
! ==========================================================================================================
    SUBROUTINE READ_EV_DATA
    USE TIMED_CONTROLLERS
    USE STREET_LINKS
    USE TEXT
    USE SIMPARAMS
    INCLUDE 'IOFILES.INC'
    CHARACTER*1000 FNAME
    CHARACTER*1 TAG
    LOGICAL :: WEXIST
    INTEGER :: I, ISIG, IAP, ILINK, IN, IRANGE

! --- Try to open the EV Data file.
        
    FNAME = LINFNAME(1:IROOT)//'EVD'
    INQUIRE(FILE=FNAME, EXIST=WEXIST)
    IF(WEXIST) THEN

! --- Store the inputs.
        
      OPEN(999, FILE=FNAME, ERR=10, IOMSG=ETEXT)
      DO
        READ(999, '(A)', ERR=20, IOMSG=ETEXT) TAG
        IF(TAG .EQ. '/' .OR. TAG .EQ. '') THEN
          CYCLE

! --- Awareness distribution.
              
        ELSEIF(TAG .EQ. 'A' .OR. TAG .EQ. 'a') THEN
          READ(999, *, ERR=20, IOMSG=ETEXT) (KDIST1(I), I = 1, 10)
          READ(999, *, ERR=20, IOMSG=ETEXT) (KRN1(I), I = 1, 10)

! --- Cooperation distribution.
              
        ELSEIF(TAG .EQ. 'C' .OR. TAG .EQ. 'c') THEN
          READ(999, *, ERR=20, IOMSG=ETEXT) (KDIST2(I), I = 1, 10)
          READ(999, *, ERR=20, IOMSG=ETEXT) (KRN2(I), I = 1, 10)

! --- Awareness reduction due to speed.
              
        ELSEIF(TAG .EQ. 'S' .OR. TAG .EQ. 's') THEN
          READ(999, *, ERR=20, IOMSG=ETEXT) (KSPEED(I), I = 1, 10)
          READ(999, *, ERR=20, IOMSG=ETEXT) (KRED(I), I = 1, 10)

! --- Trailing distance distribution.
              
        ELSEIF(TAG .EQ. 'T' .OR. TAG .EQ. 't') THEN
          READ(999, *, ERR=20, IOMSG=ETEXT) (KDIST3(I), I = 1, 10)
          READ(999, *, ERR=20, IOMSG=ETEXT) (KRN3(I), I = 1, 10)

! --- Lateral sight distance distribution.
              
        ELSEIF(TAG .EQ. 'L' .OR. TAG .EQ. 'l') THEN
          READ(999, *, ERR=20, IOMSG=ETEXT) LATDIST

! --- Signal preemption entries.
              
        ELSEIF(TAG .EQ. 'P' .OR. TAG .EQ. 'p') THEN
          DO
            READ(999, *, END=1, ERR=20) IN, IRANGE
            DO ISIG = 1, NUMBER_OF_FTCS
              IF(FTC_SIGNALS(ISIG)%NODE .EQ. IN) THEN
                DO IAP = 1, FTC_SIGNALS(ISIG)%APPROACHES
                  ILINK = FTC_SIGNALS(ISIG)%APPROACH(IAP)
                  IF(ILINK .EQ. 0) EXIT
                  SIGNAL_RANGE(ILINK) = IRANGE
                ENDDO
                EXIT
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ENDDO
1     CONTINUE
      CLOSE(999)
    ENDIF
    RETURN
10  WRITE(MSGTEXT, '(A)') 'FILE OPEN ERROR: READ_EV_DATA'
    CALL SENDTEXTMSG(M_ERROR)
    WRITE(MSGTEXT, '(A)') ETEXT
    CALL SENDTEXTMSG(M_ERROR)
    ERROR_FLAG = 1
    RETURN
20  WRITE(MSGTEXT, '(A)') 'FILE READ ERROR: READ_EV_DATA'
    CALL SENDTEXTMSG(M_ERROR)
    WRITE(MSGTEXT, '(A)') ETEXT
    CALL SENDTEXTMSG(M_ERROR)
    ERROR_FLAG = 1
    RETURN
    END SUBROUTINE

  END MODULE
