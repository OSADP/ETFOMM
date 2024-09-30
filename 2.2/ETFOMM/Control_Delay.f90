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

! ==========================================================================================================
  SUBROUTINE CONTROL_DELAY
! ----------------------------------------------------------------------
! --- Scan all vehicles to update control delay.
! ----------------------------------------------------------------------
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE TIMED_CONTROLLERS
  USE GLOBAL_DATA
  USE SIMPARAMS
  USE NODE_TABLE
  USE BUS_ROUTE_DATA
  IMPLICIT NONE
  INTEGER :: IL, IV
  INTEGER :: ITURN, IUP, IDN, ILENTH, II
  INTEGER :: ILEADER, INODE, NBR
  REAL :: ISPEED, ACCEL, DELAY
  LOGICAL :: WA, WCHECKEXITNODE
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
#endif
  DO IV = 1, HIGHEST_INDEX_S
#ifdef DebugVersion
  temp = sid(iv)
#endif
    IF(SLINK(IV) .EQ. 0) CYCLE
    IF(TRAM_FOLLOWER(IV)) CYCLE
    IL = SLINK(IV)
    ILENTH = SLENGTH(IL)
    DELAY = 0.

! --- Calculate control delay.

! --- If the vehicle is a bus in dwell skip delay accumulation

    IF(QSTATE(IV) .NE. QS_DWELL) THEN

      ISPEED = SSPEED(IV)
      ACCEL = SACCELERATION(IV)

! --- Check if the vehicle is in network or not        

      IF(NODE_TYPE(SUSN(IL)) .NE. NT_INTERN) GOTO 20
      IF(NODE_TYPE(SDSN(IL)) .NE. NT_INTERN) GOTO 20
!
      IDN = SDSN(IL)
      IUP = SUSN(IL)
      ITURN = STURNCODE(IV)

! --- Check VEHICD to determine how to calculate control delay

      II = VEHICD(IV)

      ICDCASE: SELECT CASE (II) 
          
      CASE (0)

! --- Check if vehicle will be affected by intersection control or
! --- if any of its leaders is affected by intersection control

        WA = .FALSE.
        ILEADER = IV
        DO WHILE(.NOT. WA .AND. ILEADER .NE. 0)
          WA = VEHICD0(ILEADER)
          ILEADER = SLEADER(ILEADER)
        ENDDO

! --- If vehicle's speed is less than 10 ft/s and it has started deceleration and 
! --- its leader is affected by intersection control (WA = .TRUE.)

        IF((ACCEL .LT. 0 .OR. ISPEED .LE. 10) .AND. WA) THEN

! --- Check downstream node      

          IF(.NOT. WCHECKEXITNODE(IL)) GOTO 20

! --- Compare speed with free flow speed

          ISPEED = MIN(ISPEED, .8 * SDESIREDSPEED(IV))

! --- Record control delay speed and transfer to next status 0->2

          SPDICD(IV) = ISPEED
          VEHICD(IV) = 2
          INODE = IDN

! --- If vehicle is behind a stopped bus, skip delay accumulation

          IF(ACCEL .GE. 2 .AND. QSTATE(IV) .EQ. QS_NOTINQ) GOTO 20

! --- Calculate the control delay

          IF(ISPEED .GT. 0.0) THEN
            DELAY = TIMESTEP - (SLOCATION(IV) - PRVDIST(IV)) / ISPEED
            DELAY = MIN( MAX(DELAY, 0.), TIMESTEP)
          ENDIF

          RICDELAY(IL) = RICDELAY(IL) + DELAY
          IF(ITURN .EQ. 0) THEN
            RICDELL(IL) = RICDELL(IL) + DELAY
          ELSEIF(ITURN .EQ. 2) THEN
            RICDELR(IL) = RICDELR(IL) + DELAY
          ELSEIF(ITURN .GT. 2) THEN
            RICDELD(IL) = RICDELD(IL) + DELAY
          ENDIF
          
          !Save the turn code for use in subsequent processing
          ICD_TURNCODE(IV) = ITURN
                      
        ENDIF
          
      CASE (2)

! --- Transfer to next status 2->4

        IF(ICD_LINK(IV) .NE. 0 .AND. ICD_LINK(IV) .NE. SLINK(IV)) THEN  
          VEHICD(IV) = 4
          PRVLNKICD(IV) = ICD_LINK(IV)
          GOTO 20
        ENDIF

        ISPEED = SPDICD(IV)
        INODE = IDN

        IF(.NOT. WCHECKEXITNODE(IL)) GOTO 20

! --- If vehicle is behind a stopped bus, skip delay accumulation

        IF(ACCEL .GE. 2 .AND. QSTATE(IV) .EQ. 0) GOTO 20

        IF(ISPEED .GT. 0.0) THEN
          DELAY = TIMESTEP - (SLOCATION(IV) - PRVDIST(IV)) / ISPEED
          DELAY = MIN( MAX(DELAY, 0.), TIMESTEP)
        ENDIF

        RICDELAY(IL) = RICDELAY(IL) + DELAY
        IF(ITURN .EQ. 0) THEN
          RICDELL(IL) = RICDELL(IL) + DELAY
        ELSEIF(ITURN .EQ. 2) THEN
          RICDELR(IL) = RICDELR(IL) + DELAY
        ELSEIF(ITURN .GT. 2) THEN
          RICDELD(IL) = RICDELD(IL) + DELAY
        ENDIF

! --- Transfer to next status 3->4

      CASE (3)
        VEHICD(IV) = 4
          
      CASE (4)

! --- Transfer to next status 3->4 (passed the stop bar)

        IL = PRVLNKICD(IV)
        ITURN = ICD_TURNCODE(IV)
        VEHICD(IV) = 5

! --- If start acceleration, transfer back to status 0

        IF(ACCEL .LE. 1) THEN
          VEHICD(IV) = 0
        ELSE

! --- Check if the vehicle is affected by downstream node intersection control
! --- although the vehicle has not been accelerated.

          WA = .FALSE.
          ILEADER = IV
   
          DO WHILE(.NOT. WA .AND. ILEADER .NE. 0)
            WA = VEHICD0(ILEADER)   
            ILEADER = SLEADER(ILEADER)
          ENDDO

! --- If the vehicle is affected by downstream intersection control
! --- check if the vehicle has passed half the distance of that length
 
          IF((ACCEL .LT. 0 .OR. ISPEED .LE. 10) .AND. WA)  THEN
            IF(SLOCATION(IV) .GT. 0.5 * ILENTH) THEN

! --- Transfer to status 2 

            VEHICD(IV) = 2
            ISPEED = MIN(ISPEED, .8 * SDESIREDSPEED(IV))
            SPDICD(IV) = ISPEED
          ELSE

! --- Record the senario 

              VEHICD(IV) = 6
            ENDIF
          ENDIF
        ENDIF

! --- Calculate the control delay

        IF(IL .EQ. 0) GOTO 20
        IF(NODE_TYPE(SUSN(IL)) .EQ. NT_EXTERN) GOTO 20
        IF(NODE_TYPE(SDSN(IL)) .EQ. NT_EXTERN) GOTO 20

! --- If vehicle is behind the stopped bus, skip delay accumulation

        IF(ACCEL .GE. 2 .AND. QSTATE(IV) .EQ. QS_NOTINQ) GOTO 20

        INODE = IUP
        ISPEED = SSPEED(IV)
        IF(ISPEED .LE. 10) ISPEED = 10
        IF(ISPEED .GT. 0.0) THEN
          DELAY = TIMESTEP - (SLOCATION(IV) - PRVDIST(IV)) / ISPEED
          DELAY = MIN( MAX(DELAY, 0.), TIMESTEP)
        ENDIF
        DELAY = 2.0 * DELAY
        RICDELAY(IL) = RICDELAY(IL) + DELAY
        IF(ITURN .EQ. 0) THEN
          RICDELL(IL) = RICDELL(IL) + DELAY
        ELSEIF(ITURN .EQ. 2) THEN
          RICDELR(IL) = RICDELR(IL) + DELAY
        ELSEIF(ITURN .GT. 2) THEN
          RICDELD(IL) = RICDELD(IL) + DELAY
        ENDIF

      CASE (5)

! --- Same as in case 4

        IF(ACCEL .GE. 1) THEN
          VEHICD(IV) = 0
        ELSE

          WA = .FALSE.
          ILEADER = IV
   
          DO WHILE(.NOT. WA .AND. ILEADER .NE. 0)
            WA = VEHICD0(ILEADER)   
            ILEADER = SLEADER(ILEADER)
          ENDDO

          IF((ACCEL .LT. 0 .OR. ISPEED .LE. 10) .AND. WA) THEN
            IF(SLOCATION(IV) .GT. 0.5 * ILENTH) THEN
              VEHICD(IV) = 2
              ISPEED = MIN(ISPEED, .8 * SDESIREDSPEED(IV))
              SPDICD(IV) = ISPEED
            ELSE
              VEHICD(IV) = 6
            ENDIF
          ENDIF

        ENDIF

        IL = PRVLNKICD(IV)
        ITURN = ICD_TURNCODE(IV)
        IF(IL .EQ. 0) GOTO 20
        IF(NODE_TYPE(SUSN(IL)) .EQ. NT_EXTERN) GOTO 20
        IF(NODE_TYPE(SDSN(IL)) .EQ. NT_EXTERN) GOTO 20

! --- If vehicle is behind the stopped bus, skip delay accumulation

        IF(ACCEL .GE. 2 .AND. QSTATE(IV) .EQ. QS_NOTINQ) GOTO 20

        INODE = IUP
        ISPEED = SSPEED(IV)
        IF(ISPEED .LE. 10) ISPEED = 10
        IF(ISPEED .GT. 0.0) THEN
          DELAY = TIMESTEP - (SLOCATION(IV) - PRVDIST(IV)) / ISPEED
          DELAY = MIN( MAX(DELAY, 0.), TIMESTEP)
        ENDIF
        RICDELAY(IL) = RICDELAY(IL) + DELAY
        IF(ITURN .EQ. 0) THEN
          RICDELL(IL) = RICDELL(IL) + DELAY
        ELSEIF(ITURN .EQ. 2) THEN
          RICDELR(IL) = RICDELR(IL) + DELAY
        ELSEIF(ITURN .GT. 2) THEN
          RICDELD(IL) = RICDELD(IL) + DELAY
        ENDIF

! --- Status 6 is transfer from 4 and 5, after the vehicle passsed half
! --- the distance, its status should have changed
  
      CASE (6)

        ISPEED = SSPEED(IV)
        IF(ISPEED .LE. 10) ISPEED = 10

        IF(ACCEL .GE. 1) VEHICD(IV) = 0

        WA = .FALSE.
        ILEADER = IV
        
        DO WHILE(.NOT. WA .AND. ILEADER .NE. 0)
          WA = VEHICD0(ILEADER)   
          ILEADER = SLEADER(ILEADER)
        ENDDO

        IF((ACCEL .LT. 0 .OR. ISPEED .LE. 10) .AND. WA) THEN
          VEHICD(IV) = 2
          ISPEED = MIN(ISPEED, .8 * SDESIREDSPEED(IV))
          SPDICD(IV) = ISPEED
        ENDIF
        IF(SLOCATION(IV) .GT. 0.5*ILENTH) THEN
          VEHICD(IV) = 2
          ISPEED = MIN(ISPEED, .8 * SDESIREDSPEED(IV))
          SPDICD(IV) = ISPEED
        ENDIF

        IL = PRVLNKICD(IV)
        ITURN = ICD_TURNCODE(IV)
        IF(IL .EQ. 0) GOTO 20
        IF(NODE_TYPE(SUSN(IL)) .EQ. NT_EXTERN) GOTO 20
        IF(NODE_TYPE(SDSN(IL)) .EQ. NT_EXTERN) GOTO 20

! --- If vehicle is behind the stopped bus, skip delay accumulation

        IF(ACCEL .GE. 2 .AND. QSTATE(IV) .EQ. QS_NOTINQ) GOTO 20

        INODE = IUP
        IF(ISPEED .GT. 0.0) THEN
          DELAY = TIMESTEP - (SLOCATION(IV) - PRVDIST(IV)) / ISPEED
          DELAY = MIN( MAX(DELAY, 0.), TIMESTEP)
        ENDIF
        RICDELAY(IL) = RICDELAY(IL) + DELAY
        IF(ITURN .EQ. 0) THEN
          RICDELL(IL) = RICDELL(IL) + DELAY
        ELSEIF(ITURN .EQ. 2) THEN
          RICDELR(IL) = RICDELR(IL) + DELAY
        ELSEIF(ITURN .GT. 2) THEN
          RICDELD(IL) = RICDELD(IL) + DELAY
        ENDIF
                  
      CASE DEFAULT

      END SELECT ICDCASE
    ENDIF   !I2 .NE. 0 .AND.QSTATE(IV).NE.4

20  CONTINUE
    PRVDIST(IV) = SLOCATION(IV)
    ICD_LINK(IV) = SLINK(IV)
    IF(SFLEET(IV) .EQ. FLEET_BUS) THEN
      NBR = SROUTEID(IV)
      BUSR_CONTROLDELAY(NBR) = BUSR_CONTROLDELAY(NBR) + DELAY
    ENDIF
      
  ENDDO
  RETURN
  END
      
  LOGICAL FUNCTION WCHECKEXITNODE(IL)
!
! --- CODED    1-12-00 BY L. ZHANG
!
! --- TITLE - CHECK DOWNSTREAM NODES
!
! --- FUNCTION - THIS MODULE CHECK IF THERE IS ANY DOWNSTREAM NODES
!
! --- ARGUMENTS - IL:    LINK IDENTIFICATION NUMBER (INPUT)
!
! -----------------------   DESCRIPTION   ------------------------------
!                           -----------
!
!     THIS MODULE CHECK FIVE POSSIBLE LINKS TOWARDS DOWNSTREAM NODES.
!     RETURNS TRUE IF THERE IS ANY NON-INTERFACE NODES
!
! --------------------   THIS ROUTINE CALLED BY   ----------------------
!                        ----------------------
!
!                      CNTRLDLY
!
! ---------------------   THIS ROUTINE CALLS   -------------------------
!                         ------------------
!
!                      NONE
!
! ---------------   GLOSSARY OF COMMON VARIABLE NAMES   ----------------
!                   ---------------------------------
!
!     ARIGHT  LINK SPECIFIC ARRAY - RIGHT TURN RECEIVING LINK
!     DIAGNL  LINK SPECIFIC ARRAY - DIAGONAL RECEIVING LINK
!     LEFT    LINK SPECIFIC ARRAY - LEFT TURN RECEIVING LINK
!     OPPOSE  LINK SPECIFIC ARRAY - OPPOSING TRAFFIC LINK
!     THRU    LINK SPECIFIC ARRAY - THRU RECEIVING LINK 
!
! ---------------   GLOSSARY OF LOCAL VARIABLE NAMES   -----------------
!                   --------------------------------
!     WCHECKEXITNODE     DOWNSTREAM NODE STATUS 
!
! ----------------------------------------------------------------------
  USE STREET_LINKS
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IL
  
  WCHECKEXITNODE = .FALSE.
  IF(LEFT_LINK(IL) .GT. 0) THEN
    IF(NODE_TYPE(SDSN(LEFT_LINK(IL))) .EQ. NT_INTERN) WCHECKEXITNODE = .TRUE.
  ENDIF
  IF(STHRU_LINK(IL) .GT. 0) THEN
    IF(NODE_TYPE(SDSN(STHRU_LINK(IL))) .EQ. NT_INTERN) WCHECKEXITNODE = .TRUE.
  ENDIF
  IF(RIGHT_LINK(IL) .GT. 0) THEN
    IF(NODE_TYPE(SDSN(RIGHT_LINK(IL))) .EQ. NT_INTERN) WCHECKEXITNODE = .TRUE.
  ENDIF
  IF(LEFT_DIAG_LINK(IL) .GT. 0) THEN
    IF(NODE_TYPE(SDSN(LEFT_DIAG_LINK(IL))) .EQ. NT_INTERN) WCHECKEXITNODE = .TRUE.
  ENDIF
  IF(RIGHT_DIAG_LINK(IL) .GT. 0) THEN
    IF(NODE_TYPE(SDSN(RIGHT_DIAG_LINK(IL))) .EQ. NT_INTERN) WCHECKEXITNODE = .TRUE.
  ENDIF
  RETURN
  END
