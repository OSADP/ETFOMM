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

  MODULE EVENTS
    USE GLOBAL_DATA
    IMPLICIT NONE
    INTEGER :: NUMBER_OF_EVENTS
    INTEGER :: NUMBER_OF_LONGTERM_EVENTS
    INTEGER, ALLOCATABLE :: EVENT_BEGIN_TIME(:)
    INTEGER, ALLOCATABLE :: EVENT_END_TIME(:)
    INTEGER, ALLOCATABLE :: EVENT_LANE(:)
    INTEGER, ALLOCATABLE :: EVENT_LINK(:)
    INTEGER, ALLOCATABLE :: EVENT_LOCATION(:)
    INTEGER, ALLOCATABLE :: EVENT_TYPE(:)
    INTEGER, ALLOCATABLE :: EVENT_SPEED_REDUCTION(:)
    INTEGER, ALLOCATABLE :: EVENT_LENGTH(:)
    INTEGER, ALLOCATABLE :: EVENT_CODE(:)
    INTEGER, ALLOCATABLE :: EVENT_GROUP_ID(:)
    LOGICAL, ALLOCATABLE :: EVENT_PRIMARY(:)
    INTEGER, ALLOCATABLE :: PED_APPROACH_LINK(:)
    INTEGER :: LASTEVENT
    INTEGER :: ICOUNT
    INTEGER :: NEVENTS = 100
    INTEGER, PARAMETER :: MORE_EVENTS = 100
    REAL :: PKDURATION_MULT(10) = (/ 0., 0., .14, .35, .5, .71, 1.07, 1.46, 2.24, 3.53/)
    REAL :: PKHEADWAY_MULT(10) = (/.12, .16, .29, .41, .55, .69, .76, 1.14, 1.93, 3.95/)
    REAL :: STE_MULT(10) = (/.1, .2, .3, .4, .5, .7, 1., 1.3, 1.8, 3.7/)
#ifdef TSIS_COMPATIBLE
    STRUCTURE /LKINCDATA/
      INTEGER*4 INCIDENTID
      INTEGER*4 LINKID
      INTEGER*2 INCIDENTTYPE
      REAL*4    INCIDENTPOSITION
      REAL*4    INCIDENTLENGTH
      INTEGER*4 STARTTIME
      INTEGER*4 DURATION
      REAL*4    REACTIONPOINT
      REAL*4    RUBBERNECKFACTOR
      INTEGER*2 MODELTYPE
      INTEGER*2 MSTATE
      INTEGER*2 NUMAFFECTEDLANES
      INTEGER*4 AFFECTEDLANEIDARRAY(11)
      INTEGER*2 LANEINCIDENTCODES(11)
    END STRUCTURE
#else
    STRUCTURE /LKINCDATA/
      INTEGER*4 INCIDENTID
      INTEGER*4 LINKID
      INTEGER*2 INCIDENTTYPE
      REAL*4    INCIDENTPOSITION
      REAL*4    INCIDENTLENGTH
      INTEGER*4 STARTTIME
      INTEGER*4 DURATION
      REAL*4    REACTIONPOINT
      REAL*4    RUBBERNECKFACTOR
      INTEGER*2 MODELTYPE
      INTEGER*2 MSTATE
      INTEGER*2 NUMAFFECTEDLANES
      INTEGER*4 AFFECTEDLANEIDARRAY(20)
      INTEGER*2 LANEINCIDENTCODES(20)
    END STRUCTURE
#endif    
    TYPE(LKINCDATA), ALLOCATABLE :: ALLEVENTS(:)
    TYPE(LKINCDATA), ALLOCATABLE :: THELKINCDATA(:)
        
    CONTAINS

! ==================================================================================================
    SUBROUTINE ALLOCATE_EVENT_ARRAYS(N)
    INTEGER :: N, I, J
    IF(ALLOCATED(EVENT_BEGIN_TIME)) RETURN
    ALLOCATE(EVENT_BEGIN_TIME(N))
    ALLOCATE(EVENT_END_TIME(N))
    ALLOCATE(EVENT_LOCATION(N))
    ALLOCATE(EVENT_LINK(N))
    EVENT_LINK = 0
    ALLOCATE(EVENT_LANE(N))
    ALLOCATE(EVENT_TYPE(N))
    ALLOCATE(EVENT_SPEED_REDUCTION(N))
    ALLOCATE(EVENT_LENGTH(N))
    ALLOCATE(EVENT_CODE(N))
    ALLOCATE(EVENT_GROUP_ID(N))
    ALLOCATE(EVENT_PRIMARY(N))
    ALLOCATE(PED_APPROACH_LINK(N))
    ALLOCATE(ALLEVENTS(N))
    ALLOCATE(THELKINCDATA(N))
    DO I = 1, N
      ALLEVENTS(I).INCIDENTID = 0
      ALLEVENTS(I).LINKID = 0
      ALLEVENTS(I).INCIDENTTYPE = 0
      ALLEVENTS(I).INCIDENTPOSITION = 0
      ALLEVENTS(I).STARTTIME = 0
      ALLEVENTS(I).DURATION = 0
      ALLEVENTS(I).INCIDENTLENGTH = 0
      ALLEVENTS(I).REACTIONPOINT = 0
      ALLEVENTS(I).RUBBERNECKFACTOR = 0
      ALLEVENTS(I).MSTATE = 0
      ALLEVENTS(I).MODELTYPE = 0
#ifdef TSIS_COMPATIBLE
      DO J = 1, 11
#else        
      DO J = 1, 20
#endif        
        ALLEVENTS(I).AFFECTEDLANEIDARRAY(J) = 0
        ALLEVENTS(I).LANEINCIDENTCODES(J) = 0
      ENDDO
      ALLEVENTS(I).NUMAFFECTEDLANES = 0
    ENDDO
    RETURN
    END SUBROUTINE
      
! ==================================================================================================
    SUBROUTINE REALLOCATE_EVENT_ARRAYS
    USE ARRAY_FUNCTIONS
    INTEGER :: I1, I2
! ----------------------------------------------------------------------
    I1 = NEVENTS
    I2 = NEVENTS + MORE_EVENTS
    NEVENTS = I2
    CALL REALLOCATE_INTEGER(EVENT_BEGIN_TIME, I1, I2)
    CALL REALLOCATE_INTEGER(EVENT_END_TIME, I1, I2)
    CALL REALLOCATE_INTEGER(EVENT_LOCATION, I1, I2)
    CALL REALLOCATE_INTEGER(EVENT_LINK, I1, I2)
    CALL REALLOCATE_INTEGER(EVENT_LANE, I1, I2)
    CALL REALLOCATE_INTEGER(EVENT_TYPE, I1, I2)
    CALL REALLOCATE_INTEGER(EVENT_SPEED_REDUCTION, I1, I2)
    CALL REALLOCATE_INTEGER(EVENT_LENGTH, I1, I2)
    CALL REALLOCATE_INTEGER(EVENT_CODE, I1, I2)
    CALL REALLOCATE_INTEGER(EVENT_GROUP_ID, I1, I2)
    CALL REALLOCATE_LOGICAL(EVENT_PRIMARY, I1, I2)
    CALL REALLOCATE_EVENTS(ALLEVENTS, I1, I2)
    CALL REALLOCATE_EVENTS(THELKINCDATA, I1, I2)
    END SUBROUTINE
      
! ==================================================================================================
    SUBROUTINE DEALLOCATE_EVENT_ARRAYS
    IF(ALLOCATED(EVENT_BEGIN_TIME)) DEALLOCATE(EVENT_BEGIN_TIME)
    IF(ALLOCATED(EVENT_END_TIME)) DEALLOCATE(EVENT_END_TIME)
    IF(ALLOCATED(EVENT_LOCATION)) DEALLOCATE(EVENT_LOCATION)
    IF(ALLOCATED(EVENT_LINK)) DEALLOCATE(EVENT_LINK)
    IF(ALLOCATED(EVENT_LANE)) DEALLOCATE(EVENT_LANE)
    IF(ALLOCATED(EVENT_TYPE)) DEALLOCATE(EVENT_TYPE)
    IF(ALLOCATED(EVENT_SPEED_REDUCTION)) DEALLOCATE(EVENT_SPEED_REDUCTION)
    IF(ALLOCATED(EVENT_LENGTH)) DEALLOCATE(EVENT_LENGTH)
    IF(ALLOCATED(EVENT_CODE)) DEALLOCATE(EVENT_CODE)
    IF(ALLOCATED(EVENT_GROUP_ID)) DEALLOCATE(EVENT_GROUP_ID)
    IF(ALLOCATED(EVENT_PRIMARY)) DEALLOCATE(EVENT_PRIMARY)
    IF(ALLOCATED(PED_APPROACH_LINK)) DEALLOCATE(PED_APPROACH_LINK)
    IF(ALLOCATED(ALLEVENTS)) DEALLOCATE(ALLEVENTS)
    IF(ALLOCATED(THELKINCDATA)) DEALLOCATE(THELKINCDATA)
    RETURN
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE REALLOCATE_EVENTS(A, OLD, NEW)
    TYPE(LKINCDATA), ALLOCATABLE, INTENT(INOUT) :: A(:)
    INTEGER, INTENT(IN) :: OLD, NEW
    TYPE(LKINCDATA), ALLOCATABLE :: TEMP(:)
    INTEGER :: I, J
! ----------------------------------------------------------------------
    ALLOCATE(TEMP(OLD))
    TEMP(1:OLD) = A(1:OLD)
    DEALLOCATE(A)
    ALLOCATE(A(NEW))
    A(1:OLD) = TEMP(1:OLD)
    DO I = OLD + 1, NEW
      ALLEVENTS(I).INCIDENTID = 0
      ALLEVENTS(I).LINKID = 0
      ALLEVENTS(I).INCIDENTTYPE = 0
      ALLEVENTS(I).INCIDENTPOSITION = 0
      ALLEVENTS(I).STARTTIME = 0
      ALLEVENTS(I).DURATION = 0
      ALLEVENTS(I).INCIDENTLENGTH = 0
      ALLEVENTS(I).REACTIONPOINT = 0
      ALLEVENTS(I).RUBBERNECKFACTOR = 0
      ALLEVENTS(I).MSTATE = 0
      ALLEVENTS(I).MODELTYPE = 0
      DO J = 1, N_FREEWAY_LANES
        ALLEVENTS(I).AFFECTEDLANEIDARRAY(J) = 0
        ALLEVENTS(I).LANEINCIDENTCODES(J) = 0
      ENDDO
      ALLEVENTS(I).NUMAFFECTEDLANES = 0
    ENDDO
    DEALLOCATE(TEMP)
    END SUBROUTINE

! ==================================================================================================
    SUBROUTINE NEW_EVENT
    IMPLICIT NONE
! ----------------------------------------------------------------------
    IF(NUMBER_OF_EVENTS .EQ. 0) THEN
      CALL ALLOCATE_EVENT_ARRAYS(NEVENTS)
      NUMBER_OF_EVENTS = 1
    ELSE
      NUMBER_OF_EVENTS = NUMBER_OF_EVENTS + 1
      IF(NUMBER_OF_EVENTS .GT. NEVENTS) THEN
        CALL REALLOCATE_EVENT_ARRAYS
      ENDIF
    ENDIF 
    RETURN
    END SUBROUTINE
    
! ==========================================================================================================
    SUBROUTINE CANCEL_BLOCKAGE(IL)
! ----------------------------------------------------------------------
! --- If there is a blockage on the link, determine if the event
! --- has ended
! ----------------------------------------------------------------------
    USE STREET_LINKS
    USE SIMPARAMS
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: IL
    INTEGER :: I, ILANE
! ----------------------------------------------------------------------
    DO I = 1, NUMBER_OF_EVENTS
      IF(EVENT_LINK(I) .EQ. IL) THEN
        ILANE = EVENT_LANE(I)
        IF(SIMTIME .GE. EVENT_END_TIME(I)) THEN
          BLOCKAGE(IL, ILANE) = 0
        ENDIF
      ENDIF
    ENDDO
    RETURN
    END SUBROUTINE
        
! ==========================================================================================================
    SUBROUTINE APPLY_BLOCKAGE(IL)
! ----------------------------------------------------------------------
! --- Determine if there is a blockage on the link. A blockage may be
! --- caused by short-term and long-term events and parking activities.
! ----------------------------------------------------------------------
    USE STREET_LINKS
    USE SIMPARAMS
    USE STREET_VEHICLES
    USE ACTUATED_CONTROLLERS
    USE SEEDS
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: IL
    INTEGER :: ILANE, IV, BACK
    INTEGER :: IDUR, INCTYP, LANES_AFFECTED(N_FREEWAY_LANES)
    INTEGER :: IACT, ISIG, APL, PHASE, IEVENT, IEV
    INTEGER :: ICODE, NL, NT, NR, ND
    LOGICAL :: AMBER
    LOGICAL :: DELAY_EVENT
    REAL :: DELAY_TIME
! ----------------------------------------------------------------------
    LANES_AFFECTED = 0
    DO IEVENT = 1, NUMBER_OF_EVENTS
      IF(EVENT_LINK(IEVENT) .EQ. IL) THEN
        IF(SIMTIME .GE. EVENT_BEGIN_TIME(IEVENT) .AND. SIMTIME .LT. EVENT_END_TIME(IEVENT)) THEN
          DELAY_EVENT = .FALSE.
          ILANE = EVENT_LANE(IEVENT)
          IF(EVENT_PRIMARY(IEVENT) .AND. EVENT_LOCATION(IEVENT) .GE. SLENGTH(IL) - CROSS_WALK(IL)) THEN
            IF(SIMTIME .EQ. EVENT_BEGIN_TIME(IEVENT)) THEN
            
              !Check the status of the traffic signal
              APL = PED_APPROACH_LINK(IEVENT)
              IACT = AC_SIGNAL_ID(APL)
              ISIG = FTC_SIGNAL_ID(APL)
              IF(ISIG .NE. 0) THEN
                ICODE = SIGNAL_CODE(APL)
                CALL DECODE_FTC(ICODE, NL, NT, NR, ND, AMBER)
                IF(NT .EQ. S_RED .OR. AMBER) THEN
                  DELAY_EVENT = .TRUE.
                ENDIF
              ELSEIF(IACT .NE. 0) THEN
                PHASE = PHASE_THRU(APL)
                IF(PHASE .NE. AC_SIGNALS(IACT)%CURRENT_PHASES(1) .AND. PHASE .NE. AC_SIGNALS(IACT)%CURRENT_PHASES(2)) THEN
                  DELAY_EVENT = .TRUE.
                ELSEIF(.NOT. SIGNAL_THRU(APL)) THEN
                  DELAY_EVENT = .TRUE.
                ELSEIF(AC_SIGNALS(IACT)%SDP%PED_OMIT(PHASE)) THEN
                  DELAY_EVENT = .TRUE.
                ELSEIF(AC_SIGNALS(IACT)%SDP%PHASE_TIMER(PHASE) .GT. AC_SIGNALS(IACT)%SDP%PED_WALK_TIME(PHASE)) THEN
                  DELAY_EVENT = .TRUE.
                ENDIF
                IF(DELAY_EVENT) THEN
                  DELAY_TIME = TIMESTEP
                  DO IEV = 1, NUMBER_OF_EVENTS
                    IF(EVENT_GROUP_ID(IEV) .EQ. EVENT_GROUP_ID(IEVENT)) THEN
                      EVENT_BEGIN_TIME(IEV) = EVENT_BEGIN_TIME(IEV) + DELAY_TIME
                      EVENT_END_TIME(IEV) = EVENT_END_TIME(IEV) + DELAY_TIME
                    ENDIF
                  ENDDO
                  IF(IACT .NE. 0) THEN
                    IF(PHASE .EQ. AC_SIGNALS(IACT)%CURRENT_PHASES(1) .OR. PHASE .EQ. AC_SIGNALS(IACT)%CURRENT_PHASES(2)) THEN
                      IF(.NOT. AC_SIGNALS(IACT)%SDP%PED_OMIT(PHASE)) THEN
                        AC_SIGNALS(IACT)%PEDESTRIAN_DETECTOR_STATE(PHASE) = 1
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          IF(DELAY_EVENT) THEN
            IF(ILANE .EQ. 1) THEN
              RTOR_BLOCKAGE(IL) = .TRUE.
            ENDIF
          ELSE
            
            !BACK = SLENGTH(IL)
            !IV = FIRST_VEHICLE(IL, ILANE)
            !DO WHILE(IV .NE. 0)
            !  IF(QSTATE(IV) .NE. QS_NOTINQ) THEN
            !    BACK = SLOCATION(IV) - SVLENGTH(IV) - 1
            !  ELSE
            !    EXIT
            !  ENDIF
            !  IV = SFOLLOWER(IV)
            !ENDDO
            !EVENT_LOCATION(IEVENT) = MIN(EVENT_LOCATION(IEVENT), BACK)
            
            IF(ILANE .EQ. 1) THEN
              RTOR_BLOCKAGE(IL) = .FALSE.
            ENDIF
            BLOCKAGE(IL, ILANE) = IEVENT
            INCTYP = EVENT_TYPE(IEVENT)
            LANES_AFFECTED(1) = ILANE
            IDUR = EVENT_END_TIME(IEVENT) - EVENT_BEGIN_TIME(IEVENT)
            CALL ADDSTREETEVENT(IL, EVENT_BEGIN_TIME(IEVENT), IDUR, INCTYP, LANES_AFFECTED, EVENT_LOCATION(IEVENT), EVENT_LENGTH(IEVENT), 0, 0)
          ENDIF
        ENDIF
      ENDIF
    ENDDO
    RETURN
    END SUBROUTINE
        
! ==================================================================================================
    SUBROUTINE SAVE_EVENT_DATA(FILE1)
    USE SIMPARAMS
    INTEGER, INTENT(IN) :: FILE1
    INTEGER :: I
! ----------------------------------------------------------------------

! --- Save static data.

    WRITE(FILE1) (STE_MULT(I), I = 1, 10)
    WRITE(FILE1) (PKDURATION_MULT(I), I = 1, 10)
    WRITE(FILE1) (PKHEADWAY_MULT(I), I = 1, 10)
    WRITE(FILE1) NUMBER_OF_EVENTS
    DO I = 1, NUMBER_OF_EVENTS
      WRITE(FILE1) EVENT_BEGIN_TIME(I)
      WRITE(FILE1) EVENT_END_TIME(I)
      WRITE(FILE1) EVENT_LOCATION(I)
      WRITE(FILE1) EVENT_LINK(I)
      WRITE(FILE1) EVENT_LANE(I)
      WRITE(FILE1) EVENT_TYPE(I)
      WRITE(FILE1) EVENT_SPEED_REDUCTION(I)
      WRITE(FILE1) EVENT_LENGTH(I)
      WRITE(FILE1) EVENT_CODE(I)
    ENDDO
    RETURN
    END SUBROUTINE
        
! ==================================================================================================
    SUBROUTINE RESTORE_EVENT_DATA(FILE1)
    USE SIMPARAMS
    INTEGER, INTENT(IN) :: FILE1
    INTEGER :: I
! ----------------------------------------------------------------------

! --- Restore data.

    READ(FILE1) (STE_MULT(I), I = 1, 10)
    READ(FILE1) (PKDURATION_MULT(I), I = 1, 10)
    READ(FILE1) (PKHEADWAY_MULT(I), I = 1, 10)
    READ(FILE1) NUMBER_OF_EVENTS
    IF(NUMBER_OF_EVENTS .NE. 0) THEN
      CALL ALLOCATE_EVENT_ARRAYS(NUMBER_OF_EVENTS)
      DO I = 1, NUMBER_OF_EVENTS
        READ(FILE1) EVENT_BEGIN_TIME(I)
        READ(FILE1) EVENT_END_TIME(I)
        READ(FILE1) EVENT_LOCATION(I)
        READ(FILE1) EVENT_LINK(I)
        READ(FILE1) EVENT_LANE(I)
        READ(FILE1) EVENT_TYPE(I)
        READ(FILE1) EVENT_SPEED_REDUCTION(I)
        READ(FILE1) EVENT_LENGTH(I)
        READ(FILE1) EVENT_CODE(I)
      ENDDO
    ENDIF
    RETURN
    END SUBROUTINE

  END MODULE
     
