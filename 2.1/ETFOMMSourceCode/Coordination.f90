  SUBROUTINE COORDINATION(IACT)
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT
  LOGICAL :: XEQ
! ----------------------------------------------------------------------
!
!     Determine if a new plan needs to be loaded and/or the controller
!     needs to transition to a new plan.  When the transition flag is
!     set, the controller is in the process of transitioning from the
!     previous plan to a new plan.  Subroutine TRANSITION will determine
!     if the transition process is complete or if it is time to adjust
!     the controller parameters as part of the transition process.
!
  IF(AC_SIGNALS(IACT)%NEWPLAN .OR. AC_SIGNALS(IACT)%INTRAN .GT. 0) THEN
    IF(.NOT. INITMODE) CALL TRANSITION(IACT)
  ENDIF
!
!     DETERMINE IF THE CONTROLLER IS COORDINATED.  IF COORDINATED,
!     THE CONTROLLER WILL HAVE A SPECIFIED CYCLE LENGTH.
!
  IF(AC_SIGNALS(IACT)%CYCLE_LENGTH .GT. 0) THEN
!
!       CONTROLLER IS COORDINATED.
!
    AC_SIGNALS(IACT)%COORDINATION_PLAN = 1
!
!       CHECK IF IT IS TIME FOR COORDINATION.  IF IT IS, SET THE FLAG.
!       IF NOT, DECREMENT THE TIMER.  NOTE: THE TIMER SHOULD BE 1 SEC
!       LESS THAN THE CYCLE LENGTH TO PROVIDE PROPER COORDINATION.
!
    IF(XEQ(AC_SIGNALS(IACT)%COORDINATION_TIMER, 0.)) THEN
      CALL SET_COORDINATION_TIMER(IACT, AC_SIGNALS(IACT)%CYCLE_LENGTH - SCOPE_TIMESTEP)
      AC_SIGNALS(IACT)%COORDINATION_FLAG = 1
      CALL SET_LOCAL_CYCLE_TIMER(IACT, 0.)
    ELSE
      CALL DECREMENT_COORDINATION_TIMER(IACT)
      AC_SIGNALS(IACT)%COORDINATION_FLAG = 0
    ENDIF
  ELSE
!
!       CONTROLLER IS NOT COORDINATED.
!
    AC_SIGNALS(IACT)%COORDINATION_PLAN = 14
  ENDIF
!
  RETURN
  END
  
  SUBROUTINE TRANSITION(IACT)
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT
  REAL :: TIME, NEWLOC0,  CURLOC0, LZDIFF, TIMEFROMSYNC, LOCALZERO
  LOGICAL :: XEQ
! ----------------------------------------------------------------------
  IF(AC_SIGNALS(IACT)%NEWPLAN) THEN
!
!       If the controller is currently coordinated, wait until its
!       yield point to load the new plan.  If the controller is
!       currently operating in free mode, wait until phase 2 is
!       amber to load the new plan.
!
    IF((XEQ(AC_SIGNALS(IACT)%CYCLE_LENGTH, 0.) .AND. PHASE_IS_YELLOW(IACT, 2)) .OR. &
      (AC_SIGNALS(IACT)%CYCLE_LENGTH .GT. 0.0001 .AND. XEQ(AC_SIGNALS(IACT)%LOCAL_CYCLE_TIMER, 0.))) THEN
!
      AC_SIGNALS(IACT)%NEWPLAN = .FALSE.
!
!          If the new plan is NOT coordinated, reset the cycle length and offset.
!
      IF(XEQ(AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH, 0.)) THEN
        CALL SET_CYCLE_LENGTH(IACT, 0.)
        AC_SIGNALS(IACT)%OFFSET = 0.
      ENDIF
!
!          If the controller is currently operating in free mode,
!          reset its local cycle timer.  This will enable the
!          controller to transition if it is coordinated in the
!          new plan.
!
      IF(XEQ(AC_SIGNALS(IACT)%CYCLE_LENGTH, 0.)) CALL SET_LOCAL_CYCLE_TIMER(IACT, 0.)
    ENDIF
  ENDIF
!
!     Perform transition, if required.
!
  IF(AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH .GT. 0.) THEN
!
!     Compute the current simulation time relative to the
!     sync reference time.
!
    TIME = TIMEFROMSYNC()
!
!     Compute the time of the next local zero for the new plan.
!     LOCALZERO returns a time that is relative to the sync reference.
!
    NEWLOC0 = LOCALZERO(AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH, AC_SIGNALS(IACT)%NEW_OFFSET)
!
!     Check the local cycle timer to determine where the controller
!     is relative to its current plan yield point.
!
    IF(XEQ(AC_SIGNALS(IACT)%LOCAL_CYCLE_TIMER, AC_SIGNALS(IACT)%CYCLE_LENGTH - SCOPE_TIMESTEP)) THEN
!
!       This controller is ready to process its yield point.
!
!       If the controller is using the dwell transition method and
!       it is not already in transition, the logic will extend the
!       current green cycle before the controller processes its
!       yield point.  If the controller is already in transition,
!       this logic is skipped and the controller's next cycle will
!       be modified after it processes the yield point.
!
      IF(AC_SIGNALS(IACT)%TRANSITION_METHOD .EQ. 1 .AND. AC_SIGNALS(IACT)%INTRAN .EQ. 0) THEN
!
!         The next local zero for the current plan is one time step
!         into the future.  Compute the difference between the next
!         local zeros for the new and current plans.
!
        CURLOC0 = TIME + TIMESTEP
        LZDIFF = NEWLOC0 - CURLOC0
!
        CALL PTDWELL(IACT, LZDIFF)
      ELSEIF(AC_SIGNALS(IACT)%TRANSITION_METHOD .EQ. 4 .AND. AC_SIGNALS(IACT)%INTRAN .EQ. 0) THEN
!
!         The next local zero for the current plan is one time step
!         into the future.  Compute the difference between the next
!         local zeros for the new and current plans.
!
        CURLOC0 = TIME + TIMESTEP
        LZDIFF = NEWLOC0 - CURLOC0
!
        CALL IMMEDIATE_TRANSITION(IACT, LZDIFF)
      ENDIF
    ELSEIF(XEQ(AC_SIGNALS(IACT)%LOCAL_CYCLE_TIMER, 0.)) THEN
!
!       The controller has just processed its yield point.
!
!       The current time is at the local zero of the current plan.
!       Compute the difference between the next local zeros for the
!       new and current plans.
!
      CURLOC0 = TIME
      LZDIFF = NEWLOC0 - CURLOC0
!
!       Perform plan transition based on the currently specified
!       transition method.
!
      IF(AC_SIGNALS(IACT)%TRANSITION_METHOD .EQ. 0) THEN
        CALL PTSHORTWAY(IACT, LZDIFF)
      ELSEIF(AC_SIGNALS(IACT)%TRANSITION_METHOD .EQ. 1) THEN
        CALL PTDWELL(IACT, LZDIFF)
      ELSEIF(AC_SIGNALS(IACT)%TRANSITION_METHOD .EQ. 2) THEN
        CALL PTADD(IACT, LZDIFF)
      ELSEIF(AC_SIGNALS(IACT)%TRANSITION_METHOD .EQ. 3) THEN
        CALL PTSUBTRACT(IACT, LZDIFF)
      ELSEIF(AC_SIGNALS(IACT)%TRANSITION_METHOD .EQ. 4) THEN
        CALL IMMEDIATE_TRANSITION(IACT, LZDIFF)
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END  
  
  SUBROUTINE PTSHORTWAY(IACT, LZDIFF)
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT
  REAL, INTENT(INOUT) :: LZDIFF
  REAL :: LZDIFA, DELTA, ADDTIM, SUBTIM, RPERDIF
! ----------------------------------------------------------------------
!
!     If there is a difference in the local zeros, a transition is
!     needed.  If there is no difference, no transition is needed or
!     an active transition is complete.
!
  IF(LZDIFF .NE. 0) THEN
!
!       The new plan requires a transition.  Increment the cycle count
!       that indicates the controller is in the process of transition.
!
    AC_SIGNALS(IACT)%INTRAN = AC_SIGNALS(IACT)%INTRAN + 1
!
!       Determine which method will be fastest in transitioning to the
!       new plan.  This is done by comparing time required to complete
!       transition for both methods, ASSUMING THE SUBTRACT METHOD WILL
!       BE ABLE TO REDUCE THE SPLITS SUCCESSFULLY.  Take the absolute
!       value of the difference value passed in and store it in a local
!       variable.
!
    LZDIFA = ABS(LZDIFF)
!
!       Compute the time required to transition when using the ADD
!       method.  First, determine if the amount of change per cycle
!       is limited (user-specified input)  If limited, the transition
!       will require more than one cycle.
!
    DELTA = LZDIFA
    RPERDIF = (100.0 * DELTA) / AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH
    IF(RPERDIF .GT. AC_SIGNALS(IACT)%MAXPCT_ADD) THEN
      RPERDIF = AC_SIGNALS(IACT)%MAXPCT_ADD
      DELTA = AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH * (RPERDIF / 100.0)
    ENDIF
!
!       Compute the amount of time required to complete the transition
!       by multiplying the number of full cycles required by the
!       computed cycle length, and adding in any remainder for the
!       last cycle of the transition.
!
    ADDTIM = 9999
    IF(DELTA .NE. 0) THEN
      ADDTIM = (AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH + DELTA) * (LZDIFA / DELTA)
      IF(MOD(LZDIFA, DELTA) .NE. 0) THEN
        ADDTIM = ADDTIM + AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH + MOD(LZDIFA, DELTA)
      ENDIF
    ENDIF
!
!       Compute the time required to transition when using the SUBTRACT
!       method.  First, determine if the amount of change per cycle
!       is limited (user-specified input)  If limited, the transition
!       will require more than one cycle.
!
    DELTA = LZDIFF
    CALL PTSUBTRACT(IACT, DELTA)
    AC_SIGNALS(IACT)%INTRAN  = AC_SIGNALS(IACT)%INTRAN - 1 ! PTSUBTRACT increments it again
!
!       Because we are subtracting (going backward), subtract the
!       difference from the new cycle length.  Also, compute the
!       actual difference implemented by the transition method (it
!       may have been reduced because of a user-specified limit or
!       to prevent violation of minimum splits).
!
    LZDIFA = AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH - LZDIFA
    DELTA = AC_SIGNALS(IACT)%CYCLE_LENGTH - AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH
!
!       Compute the amount of time required to complete the transition
!       by multiplying the number of full cycles required by the
!       computed cycle length, and adding in any remainder for the
!       last cycle of the transition.
!
    SUBTIM = 9999
    IF(DELTA .NE. 0) THEN
      SUBTIM = (AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH + DELTA) * INT((LZDIFA / ABS(DELTA)))
      IF(DELTA .GT. 0) THEN
        SUBTIM = SUBTIM + AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH + MOD(LZDIFA, DELTA)
      ELSE
        SUBTIM = SUBTIM + AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH - MOD(LZDIFA, DELTA)
      ENDIF
    ENDIF
!
!       If the time to complete the transition for the ADD method is
!       less than (or equal to) the time required for the SUBTRACT
!       method, call the PTADD subroutine to perform the transition.
!       If the time for the SUBTRACT method is smaller, call the
!       PTSUBTRACT subroutine to perform the transition.
!
    IF(ADDTIM .LE. SUBTIM) THEN
      CALL PTADD(IACT, LZDIFF)
      AC_SIGNALS(IACT)%INTRAN  = AC_SIGNALS(IACT)%INTRAN  - 1 ! PTADD increments it again
      WRITE(MSGTEXT, '(F8.1,A,I3,A)') SIMTIME, ' sec : Controller @ node ', ABS(AC_SIGNALS(IACT)%NODE(1)), ', Short Way Transition using Add Method'
      CALL SENDTEXTMSG(M_WARNING)
    ELSE
      WRITE(MSGTEXT, '(F8.1,A,I3,A)') SIMTIME, ' sec : Controller @ node ', ABS(AC_SIGNALS(IACT)%NODE(1)), ', Short Way Transition using Subtract Method'
      CALL SENDTEXTMSG(M_WARNING)
    ENDIF
  ELSE
!
!       Done transitioning.  Output a status message and zero the
!       transition count.
!
    WRITE(MSGTEXT, '(F8.1,A,I3,A,I3,A)') SIMTIME, ' sec : Controller @ node ', ABS(AC_SIGNALS(IACT)%NODE(1)), ', Transition Complete in ', AC_SIGNALS(IACT)%INTRAN, ' cycle(s).'
    CALL SENDTEXTMSG(M_WARNING)
!
    AC_SIGNALS(IACT)%INTRAN = 0
!
!       Store the new plan cycle length and offset.
!
    CALL SET_CYCLE_LENGTH(IACT, AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH)
    AC_SIGNALS(IACT)%OFFSET = AC_SIGNALS(IACT)%NEW_OFFSET
!
!       Store the new plan force-off times.
!
    AC_SIGNALS(IACT)%FORCE_OFF_TIME = AC_SIGNALS(IACT)%NEW_FORCE_OFF_TIME
!
!       Reset the local and master cycle timers based on the
!       adjusted cycle length and offset.
!
    CALL SET_LOCAL_CYCLE_TIMER(IACT, 0.)
    CALL SET_MASTER_CYCLE_TIMER(IACT, AC_SIGNALS(IACT)%OFFSET)
    CALL SET_COORDINATION_TIMER(IACT, AC_SIGNALS(IACT)%CYCLE_LENGTH - SCOPE_TIMESTEP - AC_SIGNALS(IACT)%OFFSET)
  ENDIF
  RETURN
  END
  
  SUBROUTINE PTDWELL(IACT, LZDIFF)
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT
  REAL, INTENT(INOUT) :: LZDIFF
  REAL :: TIME, TIMEFROMSYNC, LOCALZERO, NEXTSYS0, LZPRNT, LOFFSET, RPERDIF
! ----------------------------------------------------------------------
!
!     If there is a difference in the local zeros, a transition is
!     needed.  If there is no difference, no transition is needed or
!     an active transition is complete.
!
  IF(LZDIFF .NE. 0) THEN
!
!       The new plan requires a transition.  Save the local zero
!       difference for the status message.
!
    LZPRNT = LZDIFF
!
!       Limit the percent change to the user-specified maximum.  Based
!       on the percent change, calculate the amount to adjust the
!       settings.
!
    RPERDIF = (100.0 * ABS(LZDIFF)) / AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH
    IF(RPERDIF .GT. AC_SIGNALS(IACT)%MAXPCT_ADD) THEN
      RPERDIF = AC_SIGNALS(IACT)%MAXPCT_ADD
      LZDIFF = AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH * (RPERDIF / 100.0)
    ENDIF
!
    IF(AC_SIGNALS(IACT)%INTRAN .GT. 0) THEN
!
!         This is not the first time through for this transition.
!
!         Adjust the cycle length of the new plan and save it as the
!         current cycle length.
!
      CALL SET_CYCLE_LENGTH(IACT, AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH + LZDIFF)
!
!         Adjust the yield point.  First, determine the next system
!         zero for the adjusted cycle length.  Because the offset is
!         passed in as zero, LOCALZERO returns the next system zero.
!
      NEXTSYS0 = LOCALZERO(AC_SIGNALS(IACT)%CYCLE_LENGTH, 0.)
!
!         Because the current time is at the local zero for the current
!         plan, the next local zero, by definition, is a new cycle
!         length in the future.  The offset is then the difference
!         between the local zero and the system zero.
!
      TIME = TIMEFROMSYNC()
      LOFFSET = TIME + AC_SIGNALS(IACT)%CYCLE_LENGTH - NEXTSYS0
!
!         Ensure the offset is in the proper range and save it as the
!         current offset.
!
      IF(LOFFSET .LT. 0) LOFFSET = LOFFSET + AC_SIGNALS(IACT)%CYCLE_LENGTH
      IF(LOFFSET .GE. AC_SIGNALS(IACT)%CYCLE_LENGTH) LOFFSET = LOFFSET - AC_SIGNALS(IACT)%CYCLE_LENGTH
      AC_SIGNALS(IACT)%OFFSET = LOFFSET
!
!         Reset the local and master cycle timers based on the adjusted
!         cycle length and offset.
!
      CALL SET_LOCAL_CYCLE_TIMER(IACT, 0.)
      CALL SET_MASTER_CYCLE_TIMER(IACT, AC_SIGNALS(IACT)%OFFSET)
      CALL SET_COORDINATION_TIMER(IACT, AC_SIGNALS(IACT)%CYCLE_LENGTH - SCOPE_TIMESTEP)

    ELSE
!
!         This is the first time through for this transition.  The
!         controller is at its yield point but its yield point logic
!         has not yet been executed.
!
!         Adjust the current cycle length and offset to extend the
!         current phase (i.e., move the yield point).  DO NOT reset
!         the controller's timers.
!
      CALL SET_CYCLE_LENGTH(IACT, AC_SIGNALS(IACT)%CYCLE_LENGTH + LZDIFF)
      AC_SIGNALS(IACT)%OFFSET = AC_SIGNALS(IACT)%OFFSET + LZDIFF
      
      !Adjust the time limit for the coordinated phases to keep them running
      AC_SIGNALS(IACT)%SDP%TIME_LIMIT(2) = AC_SIGNALS(IACT)%SDP%PHASE_TIMER(2) + AC_SIGNALS(IACT)%GUI_YC(2) + AC_SIGNALS(IACT)%GUI_RC(2) + LZDIFF
      AC_SIGNALS(IACT)%SDP%TIME_LIMIT(6) = AC_SIGNALS(IACT)%SDP%PHASE_TIMER(6) - AC_SIGNALS(IACT)%SDP%PHASE_TIMER(2) + AC_SIGNALS(IACT)%SDP%TIME_LIMIT(2)
    ENDIF
!
!       Increment the cycle count that indicates the controller is
!       in the process of transition.
!
    AC_SIGNALS(IACT)%INTRAN = AC_SIGNALS(IACT)%INTRAN + 1
!
!       Output a status message for the transition.
!
    WRITE(MSGTEXT, '(F8.1,A,I3,A,F5.1,A,F5.1,A,F5.1)') SIMTIME, ' sec : Controller @ node ', ABS(AC_SIGNALS(IACT)%NODE(1)), ', Dwell Transition,    Delta = ', &
      LZPRNT, ', Cycle = ', AC_SIGNALS(IACT)%CYCLE_LENGTH, ', Target Cycle = ', AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH
    CALL SENDTEXTMSG(M_WARNING)
  ELSE
!
!       Done transitioning.  Output a status message and zero the
!       transition count.
!
    WRITE(MSGTEXT, '(F8.1,A,I3,A,I3,A)') SIMTIME, ' sec : Controller @ node ', ABS(AC_SIGNALS(IACT)%NODE(1)), ', Transition Complete in ', AC_SIGNALS(IACT)%INTRAN, ' cycle(s).'
    CALL SENDTEXTMSG(M_WARNING)
!
    AC_SIGNALS(IACT)%INTRAN = 0
!
!       Store the new plan cycle length and offset.
!
    CALL SET_CYCLE_LENGTH(IACT, AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH)
    AC_SIGNALS(IACT)%OFFSET = AC_SIGNALS(IACT)%NEW_OFFSET
!
!       Reset the local and master cycle timers based on the new plan's
!       cycle length and offset.
!
    CALL SET_LOCAL_CYCLE_TIMER(IACT, 0.)
    CALL SET_MASTER_CYCLE_TIMER(IACT, AC_SIGNALS(IACT)%OFFSET)
    CALL SET_COORDINATION_TIMER(IACT, AC_SIGNALS(IACT)%CYCLE_LENGTH - SCOPE_TIMESTEP - AC_SIGNALS(IACT)%OFFSET)
    !AC_SIGNALS(IACT)%SDP%TIME_LIMIT(2) = AC_SIGNALS(IACT)%SDP%PHASE_TIMER(2) + AC_SIGNALS(IACT)%GUI_YC(2) + AC_SIGNALS(IACT)%GUI_RC(2)
    !AC_SIGNALS(IACT)%SDP%TIME_LIMIT(6) = AC_SIGNALS(IACT)%SDP%PHASE_TIMER(6) - AC_SIGNALS(IACT)%SDP%PHASE_TIMER(2) + AC_SIGNALS(IACT)%SDP%TIME_LIMIT(2)
  ENDIF
  RETURN
  END  
  
  SUBROUTINE PTADD(IACT, LZDIFF)
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT
  REAL, INTENT(INOUT) :: LZDIFF
  REAL :: SPLITS(8), FORCEOFF(8)
  REAL :: LZPRNT, TIME, TIMEFROMSYNC, LOCALZERO, SIDETM, NEXTSYS0, LOFFSET
  INTEGER :: IPHASE
  REAL :: RPERDIF, RSFRACT
! ----------------------------------------------------------------------
!
!     If there is a difference in the local zeros, a transition is
!     needed.  If there is no difference, no transition is needed or
!     an active transition is complete.
!
  IF(LZDIFF .NE. 0) THEN
!
!       The new plan requires a transition.  Increment the cycle count
!       that indicates the controller is in the process of transition.
!       Save the local zero difference for the status message.
!
    AC_SIGNALS(IACT)%INTRAN = AC_SIGNALS(IACT)%INTRAN + 1
    LZPRNT = LZDIFF
!
!       Limit the percent change to the user-specified maximum.  Based
!       on the percent change, calculate the amount to adjust the
!       settings.
!
    RPERDIF = (100.0 * ABS(LZDIFF)) / AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH
    IF(RPERDIF .GT. AC_SIGNALS(IACT)%MAXPCT_ADD) THEN
      RPERDIF = AC_SIGNALS(IACT)%MAXPCT_ADD
      LZDIFF = AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH * (RPERDIF / 100.0)
    ENDIF
!
!       Adjust the cycle length of the new plan and save it as the
!       current cycle length.
!
    CALL SET_CYCLE_LENGTH(IACT, AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH + LZDIFF)
    CALL SET_COORDINATION_TIMER(IACT, AC_SIGNALS(IACT)%CYCLE_LENGTH - SCOPE_TIMESTEP)
!
!       Output a status message for the transition.
!
    WRITE(MSGTEXT, '(F8.1,A,I3,A,F5.1,A,F5.1,A,F5.1)') SIMTIME, ' sec : Controller @ node ', ABS(AC_SIGNALS(IACT)%NODE(1)), &
      ', Add Transition,      Delta = ', LZPRNT, ', Cycle = ', AC_SIGNALS(IACT)%CYCLE_LENGTH,&
      ', Target Cycle = ', AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH
    CALL SENDTEXTMSG(M_WARNING)

!       Adjust the yield point.  First, determine the next system
!       zero for the adjusted cycle length.  Because the offset is
!       passed in as zero, LOCALZERO returns the next system zero.
!
    NEXTSYS0 = LOCALZERO(AC_SIGNALS(IACT)%CYCLE_LENGTH, 0.)
!
!       Because the current time is at the local zero for the current
!       plan, the next local zero, by definition, is a new cycle
!       length in the future.  The offset is then the difference
!       between the local zero and the system zero.
!
    TIME = TIMEFROMSYNC()
    LOFFSET = TIME + AC_SIGNALS(IACT)%CYCLE_LENGTH - NEXTSYS0
!
!       Ensure the offset is in the proper range and save it as the
!       current offset.
!
    IF(LOFFSET .LT. 0) LOFFSET = LOFFSET + AC_SIGNALS(IACT)%CYCLE_LENGTH
    IF(LOFFSET .GE. AC_SIGNALS(IACT)%CYCLE_LENGTH) LOFFSET = LOFFSET - AC_SIGNALS(IACT)%CYCLE_LENGTH
    AC_SIGNALS(IACT)%OFFSET = LOFFSET
!
!       Calculate the splits for the new plan and update them
!       using the adjusted cycle length.
!
    CALL CALCSPLITS(IACT, AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH, AC_SIGNALS(IACT)%NEW_FORCE_OFF_TIME, AC_SIGNALS(IACT)%PHASE_SEQUENCE, SPLITS)                         
!
!       Adjust the splits, except for phases 2 and 6.                   
!                                                                       
    DO IPHASE = 1, 8                                           
      IF(IPHASE .NE. 2 .AND. IPHASE .NE. 6) THEN                    
        RSFRACT = SPLITS(IPHASE) / AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH
        SPLITS(IPHASE) = INT(RSFRACT * AC_SIGNALS(IACT)%CYCLE_LENGTH)
      ENDIF
    ENDDO
!
!       Compute the splits for phases 2 and 6.  The following logic     
!       compensates for roundoff error and ensures both rings cross     
!       the barriers at the same time.  In the case where one ring      
!       has side street phases and the other does not, the sum of       
!       the splits for the ring without side street will not equal      
!       the cycle length.                                               
!                                                                       
    SIDETM = MAX((SPLITS(3)+SPLITS(4)), (SPLITS(7)+SPLITS(8)))      
    SPLITS(2) = AC_SIGNALS(IACT)%CYCLE_LENGTH - (SPLITS(1) + SIDETM)                  
    IF(SPLITS(6) .GT. 0) THEN                                       
      SPLITS(6) = AC_SIGNALS(IACT)%CYCLE_LENGTH - (SPLITS(5) + SIDETM)                
    ENDIF  
    
    !Save splits to the controller.
    AC_SIGNALS(IACT)%PHASE_SPLITS = SPLITS
    
!
!       Calculate the force-off times using the adjusted splits.
!

    CALL CALCFORCEOFFS(IACT, SPLITS, AC_SIGNALS(IACT)%PHASE_SEQUENCE, FORCEOFF)       
!
!       Store the new force-off times.
!
    AC_SIGNALS(IACT)%FORCE_OFF_TIME = FORCEOFF
!!
!!       Calculate new permissive period times based on the new
!!       force-off times, and load them into the current permissive
!!       period arrays.
!!
!        CALL CALCPERMISSIVE(IAC, FORCEOFF, .TRUE., ACBPRM(1,IAC), ACEPRM(1,IAC))               
  ELSE
!
!       Done transitioning.  Output a status message and zero the
!       transition count.
!
    WRITE(MSGTEXT, '(F8.1,A,I3,A,I3,A)') SIMTIME, ' sec : Controller @ node ', ABS(AC_SIGNALS(IACT)%NODE(1)), &
      ', Transition Complete in ', AC_SIGNALS(IACT)%INTRAN, ' cycle(s).'
    CALL SENDTEXTMSG(M_WARNING)
!
    AC_SIGNALS(IACT)%INTRAN = 0
!
!       Store the new plan cycle length and offset.
!
    CALL SET_CYCLE_LENGTH(IACT, AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH)
    AC_SIGNALS(IACT)%OFFSET = AC_SIGNALS(IACT)%NEW_OFFSET
!
!       Store the new plan force-off times.
!
    AC_SIGNALS(IACT)%FORCE_OFF_TIME = AC_SIGNALS(IACT)%NEW_FORCE_OFF_TIME                      
  ENDIF
!
!       Reset the local and master cycle timers based on the new plan's
!       cycle length and offset.
!
  CALL SET_LOCAL_CYCLE_TIMER(IACT, 0.)
  CALL SET_MASTER_CYCLE_TIMER(IACT, AC_SIGNALS(IACT)%OFFSET)
  CALL SET_COORDINATION_TIMER(IACT, AC_SIGNALS(IACT)%CYCLE_LENGTH - SCOPE_TIMESTEP)
  RETURN
  END
    
  SUBROUTINE IMMEDIATE_TRANSITION(IACT, LZDIFF)
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT
  REAL, INTENT(INOUT) :: LZDIFF
  REAL :: SPLITS(8), FORCEOFF(8)
  REAL :: LOCALZERO, SIDETM, NEXTSYS0, RSFRACT, TIME, MAX_FORCE_OFF
  INTEGER :: PHASE
  LOGICAL :: HAS_CHANGED
! ----------------------------------------------------------------------
!
!     If there is a difference in the local zeros, a transition is
!     needed.  If there is no difference, no transition is needed or
!     an active transition is complete.
!
  IF(LZDIFF .NE. 0) THEN
!
!       Output a status message for the transition.
!
    WRITE(MSGTEXT, '(F8.1,A,I4,A,F5.1,A,F5.1,A,F5.1)') SIMTIME, ' sec : Controller @ node ', ABS(AC_SIGNALS(IACT)%NODE(1)), &
      ', Immediate Transition,      New Offset = ', AC_SIGNALS(IACT)%NEW_OFFSET, ', New Cycle Length = ', AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH
    CALL SENDTEXTMSG(M_WARNING)
!    
!       Adjust the yield point.  First, determine the next system
!       zero for the new cycle length and offset.
!
    NEXTSYS0 = LOCALZERO(AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH, AC_SIGNALS(IACT)%NEW_OFFSET)
!
!       Calculate the splits for the new plan and update them
!       using the adjusted cycle length.
!
    CALL CALCSPLITS(IACT, AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH, AC_SIGNALS(IACT)%NEW_FORCE_OFF_TIME, AC_SIGNALS(IACT)%PHASE_SEQUENCE, SPLITS)                         
!
!       Adjust the splits, except for phases 2 and 6.                   
!                                                                       
    DO PHASE = 1, 8                                           
      IF(PHASE .NE. 2 .AND. PHASE .NE. 6) THEN                    
        RSFRACT = SPLITS(PHASE) / AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH
        SPLITS(PHASE) = INT(RSFRACT * AC_SIGNALS(IACT)%CYCLE_LENGTH)
      ENDIF
    ENDDO
!
!       Compute the splits for phases 2 and 6.  The following logic     
!       compensates for roundoff error and ensures both rings cross     
!       the barriers at the same time.  In the case where one ring      
!       has side street phases and the other does not, the sum of       
!       the splits for the ring without side street will not equal      
!       the cycle length.                                               
!                                                                       
    SIDETM = MAX((SPLITS(3)+SPLITS(4)), (SPLITS(7)+SPLITS(8)))      
    SPLITS(2) = AC_SIGNALS(IACT)%CYCLE_LENGTH - (SPLITS(1) + SIDETM)                  
    IF(SPLITS(6) .GT. 0) THEN                                       
      SPLITS(6) = AC_SIGNALS(IACT)%CYCLE_LENGTH - (SPLITS(5) + SIDETM)                
    ENDIF                                                           
!
!       Calculate the force-off times using the adjusted splits.
!
    CALL CALCFORCEOFFS(IACT, SPLITS, AC_SIGNALS(IACT)%PHASE_SEQUENCE, FORCEOFF)       
!
!       Store the new force-off times.
!
    AC_SIGNALS(IACT)%FORCE_OFF_TIME = FORCEOFF
      
    !Save splits to the controller.
    AC_SIGNALS(IACT)%PHASE_SPLITS = SPLITS
!
!       Store the new plan force-off times.
!
    AC_SIGNALS(IACT)%FORCE_OFF_TIME = AC_SIGNALS(IACT)%NEW_FORCE_OFF_TIME                      
!
!       Store the new plan cycle length and offset.
!
    CALL SET_CYCLE_LENGTH(IACT, AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH)
    AC_SIGNALS(IACT)%OFFSET = AC_SIGNALS(IACT)%NEW_OFFSET
!
!       Reset the local and master cycle timers based on the new plan's
!       cycle length and offset.
!
    TIME = NEXTSYS0 - SIMTIME
    IF(TIME .LT. 0.) TIME = TIME + AC_SIGNALS(IACT)%CYCLE_LENGTH

    !If the coordinated green time will be sufficient implement the transition as calculated,
    !otherwise add another cycle length.
    MAX_FORCE_OFF = MAXVAL(AC_SIGNALS(IACT)%FORCE_OFF_TIME(1:4))
    IF(TIME .LT. MAX_FORCE_OFF + AC_SIGNALS(IACT)%GUI_MIN_GREEN_TIMES(2)) THEN
      TIME = TIME + AC_SIGNALS(IACT)%CYCLE_LENGTH
    ENDIF
    
    CALL SET_COORDINATION_TIMER(IACT, TIME)
    CALL SET_LOCAL_CYCLE_TIMER(IACT, -SCOPE_TIMESTEP)
    AC_SIGNALS(IACT)%NEWPLAN = .FALSE.
    
  ELSE
    !Splits may need to be recalculated even though no transition is required
    HAS_CHANGED = .FALSE.
    DO PHASE = 1, 8
      IF(AC_SIGNALS(IACT)%NEW_FORCE_OFF_TIME(PHASE) .NE. AC_SIGNALS(IACT)%FORCE_OFF_TIME(PHASE)) THEN
        HAS_CHANGED = .TRUE.
        EXIT
      ENDIF
    ENDDO
    IF(HAS_CHANGED) THEN
!
!       Calculate the splits for the new plan and update them
!       using the adjusted cycle length.
!
      CALL CALCSPLITS(IACT, AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH, AC_SIGNALS(IACT)%NEW_FORCE_OFF_TIME, AC_SIGNALS(IACT)%PHASE_SEQUENCE, SPLITS)                         
!
!       Adjust the splits, except for phases 2 and 6.                   
!                                                                       
      DO PHASE = 1, 8                                           
        IF(PHASE .NE. 2 .AND. PHASE .NE. 6) THEN                    
          RSFRACT = SPLITS(PHASE) / AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH
          SPLITS(PHASE) = INT(RSFRACT * AC_SIGNALS(IACT)%CYCLE_LENGTH)
        ENDIF
      ENDDO
!
!       Compute the splits for phases 2 and 6.  The following logic     
!       compensates for roundoff error and ensures both rings cross     
!       the barriers at the same time.  In the case where one ring      
!       has side street phases and the other does not, the sum of       
!       the splits for the ring without side street will not equal      
!       the cycle length.                                               
!                                                                       
      SIDETM = MAX((SPLITS(3)+SPLITS(4)), (SPLITS(7)+SPLITS(8)))      
      SPLITS(2) = AC_SIGNALS(IACT)%CYCLE_LENGTH - (SPLITS(1) + SIDETM)                  
      IF(SPLITS(6) .GT. 0) THEN                                       
        SPLITS(6) = AC_SIGNALS(IACT)%CYCLE_LENGTH - (SPLITS(5) + SIDETM)                
      ENDIF                                                           
      !Save splits to the controller.
      AC_SIGNALS(IACT)%PHASE_SPLITS = SPLITS
      AC_SIGNALS(IACT)%FORCE_OFF_TIME = AC_SIGNALS(IACT)%NEW_FORCE_OFF_TIME
      AC_SIGNALS(IACT)%NEWPLAN = .FALSE.
    ENDIF
  ENDIF
  RETURN
  END  
  
  SUBROUTINE PTSUBTRACT(IACT, LZDIFF)
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT
  REAL, INTENT(INOUT) :: LZDIFF
  REAL :: RPERDIF, RSPLIT, RFRACT, FORCEOFF(8), SPLITS(8)
  LOGICAL :: WLIMIT
  REAL :: LZPRNT, TIMEFROMSYNC, LOCALZERO, TIME, NEXTSYS0, LOFFSET
  REAL :: MINSPLITS(8), ISPLIT
  INTEGER :: DELTA(8)
  INTEGER :: IRING, RING1, RING2, INDEX, CYCLEN, IPHASE
! ----------------------------------------------------------------------
!
!     The following arrays are dimensioned by the number of rings
!     and are used in the split adjustment calculations.
!
  INTEGER :: DEFICIT(2), SURPLUS(2)
! ----------------------------------------------------------------------
!
!     If there is a difference in the local zeros, a transition is
!     needed.  If there is no difference, no transition is needed or
!     an active transition is complete.
!
  IF(LZDIFF .NE. 0) THEN
!
!       The new plan requires a transition.  Increment the cycle count
!       that indicates the controller is in the process of transition.
!
    AC_SIGNALS(IACT)%INTRAN = AC_SIGNALS(IACT)%INTRAN + 1
!
!       Compute the percent change needed to transition.  Because we
!       are subtracting (going backward), subtract the difference from
!       the new cycle length.  Save the local zero difference for the
!       status message.
!
    LZDIFF = AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH - ABS(LZDIFF)
    RPERDIF = (100.0 * LZDIFF) / AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH
    LZPRNT = LZDIFF
!
!       Limit the percent change to the user-specified maximum.
!
    WLIMIT = .FALSE.
    IF(RPERDIF .GT. AC_SIGNALS(IACT)%MAXPCT_SUBTRACT) THEN
      WLIMIT = .TRUE.
      RPERDIF = AC_SIGNALS(IACT)%MAXPCT_SUBTRACT
    ENDIF
!
!       Compute the target cycle length for this transition.
!
    CYCLEN = NINT(AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH * (1.0 - (RPERDIF / 100.0)))
!
!       Calculate the absolute minimum split time for each phase.
!
    CALL CALCMINSPLITS(IACT, MINSPLITS)
!
!       Calculate the splits for the new plan.
!
    CALL CALCSPLITS(IACT, AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH, AC_SIGNALS(IACT)%NEW_FORCE_OFF_TIME, AC_SIGNALS(IACT)%PHASE_SEQUENCE, SPLITS)                         
!
!       Calclulate the effective minimum split for each phase.  This
!       value is either the absolute minimum split or the new split
!       reduced by the user-specified maximum (rounded up), whichever
!       is greater.
!
    DO IPHASE = 1, 8
      RSPLIT = SPLITS(IPHASE) * (1.0 - AC_SIGNALS(IACT)%MAXPCT_SUBTRACT/100.0)
      ISPLIT = INT(RSPLIT + 0.9999)
      MINSPLITS(IPHASE) = MAX(MINSPLITS(IPHASE), ISPLIT)
    ENDDO
!
    IF(WLIMIT) THEN
!
!         The transition is limited by the user-specified maximum.
!
!         Set the split for each phase to the effective minimum for
!         the phase.
!
      SPLITS = MINSPLITS
    ELSE
!
!         The transition is NOT limited by the user-specified maximum.
!
!         Compute the amount by which the split for each phase is
!         either greater than (+) or less than (-) its effective
!         minimum.  Also compute the total surplus and deficit values
!         for each ring.
!
      DO IRING = 1, 2
        SURPLUS(IRING) = 0.0
        DEFICIT(IRING) = 0.0
        DO INDEX = 1, 4
          IPHASE = (IRING - 1) * 4 + INDEX
          RSPLIT = SPLITS(IPHASE) * (1.0 - RPERDIF/100.0)
          SPLITS(IPHASE) = INT(RSPLIT)
          DELTA(IPHASE) = SPLITS(IPHASE) - MINSPLITS(IPHASE)
          IF(DELTA(IPHASE) .GT. 0.0) THEN
            SURPLUS(IRING) = SURPLUS(IRING) + DELTA(IPHASE)
          ELSE
            DEFICIT(IRING) = DEFICIT(IRING) - DELTA(IPHASE)
          ENDIF
        ENDDO
!
!           Adjust the splits in each ring.
!
        IF(DEFICIT(IRING) .GT. 0) THEN
          IF(DEFICIT(IRING) .GE. SURPLUS(IRING)) THEN
!
!               The deficit exceeds the surplus.  Therefore, all
!               of the surplus must be used -- set all splits
!               to their minimum value.  NOTE: this will increase
!               the cycle length to account for the deficit not
!               covered by the surplus.
!
            DO INDEX = 1, 4
              IPHASE = (IRING - 1) * 4 + INDEX
              SPLITS(IPHASE) = MINSPLITS(IPHASE)
            ENDDO
          ELSE
!
!               The surplus exceeds the deficit.  Increase all
!               deficit phases to their minimum values and decrease
!               surplus phases as necessary to cover the deficit.
!
            RFRACT = FLOAT(DEFICIT(IRING)) / FLOAT(SURPLUS(IRING))
            DO INDEX = 1, 4
              IPHASE = (IRING - 1) * 4 + INDEX
              IF(DELTA(IPHASE) .LT. 0) THEN
                SPLITS(IPHASE) = SPLITS(IPHASE) - DELTA(IPHASE)
              ELSEIF(DELTA(IPHASE) .GT. 0) THEN
                SPLITS(IPHASE) = SPLITS(IPHASE) - (INT(RFRACT * DELTA(IPHASE)) + 1)
              ENDIF
            ENDDO
          ENDIF
        ENDIF
      ENDDO
    ENDIF
!
!       Ensure both rings cross the main street to side street
!       barrier at the same time.
!
    RING1 = SPLITS(1) + SPLITS(2)
    RING2 = SPLITS(5) + SPLITS(6)
    IF(RING2 .GT. 0.0) THEN                                         
      IF(RING1 .LT. RING2) THEN
        SPLITS(2) = SPLITS(2) + RING2 - RING1
      ELSEIF(RING1 .GT. RING2) THEN
        SPLITS(6) = SPLITS(6) + RING1 - RING2
      ENDIF
    ENDIF                                                           
!
!       Ensure both rings cross the side street to main street
!       barrier at the same time.
!
    RING1 = SPLITS(3) + SPLITS(4)
    RING2 = SPLITS(7) + SPLITS(8)
    IF(RING1 .GT. 0.0 .AND. RING2 .GT. 0.0) THEN
      IF(RING1 .LT. RING2) THEN
        SPLITS(4) = SPLITS(4) + RING2 - RING1
      ELSEIF(RING1 .GT. RING2) THEN
        SPLITS(8) = SPLITS(8) + RING1 - RING2
      ENDIF
    ENDIF
!
!       If ring 1 has side street splits OR if ring 2 does not have     
!       side street splits, sum the splits for ring 1 to get the cycle  
!       length for this transition.  Otherwise, sum the splits for      
!       ring 2 to get the cycle length for this transition.             
!                                                                       
    CALL SET_CYCLE_LENGTH(IACT, 0.)
    IF(RING1 .GT. 0.0 .OR. RING2 .LE. 0.0) THEN                     
      DO IPHASE = 1, 4                                              
        CALL SET_CYCLE_LENGTH(IACT, AC_SIGNALS(IACT)%CYCLE_LENGTH + SPLITS(IPHASE))
      ENDDO                                                         
    ELSE                                                            
      DO IPHASE = 5, 8                                              
        CALL SET_CYCLE_LENGTH(IACT, AC_SIGNALS(IACT)%CYCLE_LENGTH + SPLITS(IPHASE))
      ENDDO                                                         
    ENDIF                                                           
!
!       If the cycle length was reduced more than necessary (compared
!       to the target cycle length), increase cycle length to the
!       target value by adding the difference to the synch phases.
!
    IF(CYCLEN - AC_SIGNALS(IACT)%CYCLE_LENGTH .GT. 0) THEN
      SPLITS(2) = SPLITS(2) + CYCLEN - AC_SIGNALS(IACT)%CYCLE_LENGTH
      IF(SPLITS(6) .GT. 0) THEN                                     
        SPLITS(6) = SPLITS(6) + CYCLEN - AC_SIGNALS(IACT)%CYCLE_LENGTH                
      ENDIF                                                         
      CALL SET_CYCLE_LENGTH(IACT, FLOAT(CYCLEN))
    ENDIF
    
    !Save splits to the controller.
    AC_SIGNALS(IACT)%PHASE_SPLITS = SPLITS
!
!       Output a status message for the transition.
!
    WRITE(MSGTEXT, '(F8.1,A,I3,A,F5.1,A,F5.1,A,F5.1)') SIMTIME, ' sec : Controller @ node ', ABS(AC_SIGNALS(IACT)%NODE(1)), &
      ', Subtract Transition, Delta = ', LZPRNT, ', Cycle = ', AC_SIGNALS(IACT)%CYCLE_LENGTH, &
      ', Target Cycle = ', AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH
    CALL SENDTEXTMSG(M_WARNING)
!
    IF(AC_SIGNALS(IACT)%CYCLE_LENGTH .GT. AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH) THEN
      WRITE(MSGTEXT, '(A)') '***** WARNING: Subtract Transition Method Failed'
      CALL SENDTEXTMSG(M_ERROR)
    ENDIF
!
!       Calculate the force-off times using the adjusted splits.
!
    CALL CALCFORCEOFFS(IACT, SPLITS, AC_SIGNALS(IACT)%PHASE_SEQUENCE, FORCEOFF)       
!
!       Store the new force-off times.
!
    AC_SIGNALS(IACT)%FORCE_OFF_TIME = FORCEOFF
!!
!!       Calculate new permissive period times based on the new
!!       force-off times, and load them into the current permissive
!!       period arrays.
!!
!        CALL CALCPERMISSIVE(IAC, FORCEOFF, .TRUE., ACBPRM(1,IAC), ACEPRM(1,IAC))               
!
!       Adjust the yield point.  First, determine the next system
!       zero for the adjusted cycle length.  Because the offset is
!       passed in as zero, LOCALZERO returns the next system zero.
!
    NEXTSYS0 = LOCALZERO(AC_SIGNALS(IACT)%CYCLE_LENGTH, 0.)
!
!       Because the current time is at the local zero for the current
!       plan, the next local zero, by definition, is a new cycle
!       length in the future.  The offset is then the difference
!       between the local zero and the system zero.
!
    TIME = TIMEFROMSYNC()
    LOFFSET = TIME + AC_SIGNALS(IACT)%CYCLE_LENGTH - NEXTSYS0
!
!       Ensure the offset is in the proper range and save it as the
!       current offset.
!
    IF(LOFFSET .LT. 0) LOFFSET = LOFFSET + AC_SIGNALS(IACT)%CYCLE_LENGTH
    IF(LOFFSET .GE. AC_SIGNALS(IACT)%CYCLE_LENGTH) LOFFSET = LOFFSET - AC_SIGNALS(IACT)%CYCLE_LENGTH
    AC_SIGNALS(IACT)%OFFSET = LOFFSET
  ELSE
!
!       Done transitioning.  Output a status message and zero the
!       transition count.
!
    WRITE(MSGTEXT, '(F8.1,A,I3,A,I3,A)') SIMTIME, ' sec : Controller @ node ', ABS(AC_SIGNALS(IACT)%NODE(1)), &
      ', Transition Complete in ', AC_SIGNALS(IACT)%INTRAN, ' cycle(s).'
    CALL SENDTEXTMSG(M_WARNING)
!
    AC_SIGNALS(IACT)%INTRAN = 0
!!
!!       Store the new plan begin and end permissive times.
!!
!        DO IPP = 1, INPRMS                                              
!          ACBPRM(IPP,IAC) = ACBPRMS(IPP,IAC)                            
!          ACEPRM(IPP,IAC) = ACEPRMS(IPP,IAC)                            
!        ENDDO
!
!       Store the new plan force-off times.
!
    AC_SIGNALS(IACT)%FORCE_OFF_TIME = AC_SIGNALS(IACT)%NEW_FORCE_OFF_TIME                      
  ENDIF
!
!       Reset the local and master cycle timers based on the new plan's
!       cycle length and offset.
!
  CALL SET_LOCAL_CYCLE_TIMER(IACT, 0.)
  CALL SET_MASTER_CYCLE_TIMER(IACT, AC_SIGNALS(IACT)%OFFSET)
  CALL SET_COORDINATION_TIMER(IACT, AC_SIGNALS(IACT)%CYCLE_LENGTH - SCOPE_TIMESTEP)
  RETURN
  END
  
  REAL FUNCTION TIMEFROMSYNC
  USE SIMPARAMS
  IMPLICIT NONE
!-----------------------------------------------------------------------
!
!     Compute the current simulation time relative to the
!     system's sync reference time.
!
  IF(INITMODE) THEN
!
!       Time is during initialization.
!
    TIMEFROMSYNC = SIMTIME + SYNCOF - INITIALIZATION_END
  ELSE
!
!       Time is during simulation.
!
    TIMEFROMSYNC = SIMTIME + SYNCOF
  ENDIF
!
!     Make sure the time is in the proper range.
!
  IF(TIMEFROMSYNC .LT. 0) THEN
    TIMEFROMSYNC = TIMEFROMSYNC + 86400
  ELSEIF(TIMEFROMSYNC .GT. 86400) THEN
    TIMEFROMSYNC = TIMEFROMSYNC - 86400
  ENDIF
!
  RETURN
  END
  
  REAL FUNCTION LOCALZERO(CYCLE_LENGTH, OFFSET)
  IMPLICIT NONE
  REAL, INTENT(IN) :: CYCLE_LENGTH, OFFSET
  INTEGER :: NCYCLES
  REAL :: TIME, TIMEFROMSYNC
!-----------------------------------------------------------------------
!
!     Compute the current simulation time relative to the
!     system's sync reference time.
!
  TIME = TIMEFROMSYNC()
!
!     Compute the number of complete cycles since sync reference.
!
  NCYCLES = TIME / CYCLE_LENGTH
!
!     Compute the time of the next local zero.  Because of the integer
!     math, this value may be less than the current time.  If so,
!     add a cycle length to the value to make it the "next" local zero.
!     However, if the local zero is AT the current time, just return
!     the value.
!
  LOCALZERO = NCYCLES * CYCLE_LENGTH + OFFSET
  IF(LOCALZERO .LT. TIME) THEN
    LOCALZERO = LOCALZERO + CYCLE_LENGTH
  ENDIF
  END
  
  SUBROUTINE CALCSPLITS(IACT, CYCLEN, FORCEOFF, SEQ, SPLIT)          
  USE ACTUATED_CONTROLLERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, SEQ(8)
  REAL, INTENT(IN) :: CYCLEN, FORCEOFF(8)
  REAL, INTENT(OUT) :: SPLIT(8)
  INTEGER :: IPHASE, INDEXP, INDEX, IDXP2, IDXP6
  REAL :: FOPREV, FOCURR, CLRTMP, CLRTMC
  LOGICAL :: YPH6, Y3OR4, Y7OR8
!----------------------------------------------------------------------
!
!     Initialize the array of splits.
!
  SPLIT = 0
!
! Compute the split values for ring 1 phases.
! 
! First, find the index for phase 2 in the phase sequence array.
! A coordinated controller must always have a phase 2.
!
  DO IDXP2 = 1, 4
    IF(SEQ(IDXP2) .EQ. 2) EXIT                                      
  ENDDO
!
! Initialize the previous values to the phase 2 values.
! NOTE: the force off for phase 2 is, by definition, zero.
!
  FOPREV = 0
  IPHASE = 2
  CLRTMP = AC_SIGNALS(IACT)%SDP%YC(IPHASE) + AC_SIGNALS(IACT)%SDP%RC(IPHASE)
!
! Loop over the ring 1 phases starting with the phase that
! follows phase 2.  The phase 2 split will be calculated
! later.
!
  DO INDEX = 1, 3
    INDEXP = IDXP2 + INDEX
    IF(INDEXP .GT. 4) INDEXP = 1
    IPHASE = SEQ(INDEXP)                                            
!
! If a phase is missing, it will have a zero value in the phase
! sequence array, indicating the phase should not be used.
!
    IF(IPHASE .GT. 0) THEN
!
! Compute the clearance time for the current phase.
!
      CLRTMC = AC_SIGNALS(IACT)%SDP%YC(IPHASE) + AC_SIGNALS(IACT)%SDP%RC(IPHASE)    
!
! Look up the force off for the current phase and compute
! the split.  Add the split to the sum of splits for ring 1.
!
      FOCURR = FORCEOFF(IPHASE)
      SPLIT(IPHASE) = FOCURR + CLRTMC - FOPREV - CLRTMP
!
! Save the current values as the previous values.
!
      FOPREV = FOCURR
      CLRTMP = CLRTMC
    ENDIF
  ENDDO
!
! ------------------------------------------------------------------
! Compute the split values for ring 2 phases.
! 
! First, find the index for phase 6 in the phase sequence array.
! If there are any ring 2 phases, phase 6 must exist.
!
  YPH6 = .FALSE.
  DO IDXP6 = 5, 8
    IF(SEQ(IDXP6) .EQ. 6) THEN                                      
      YPH6 = .TRUE.
      EXIT
    ENDIF
  ENDDO
!
! If phase 6 does not exist, skip the processing of ring 2 splits.
!
  IF(YPH6) THEN
!
! Initialize the previous values to the phase 6 values.
! NOTE: the force off for phase 6 is, by definition, zero.
!
    FOPREV = 0
    IPHASE = 6
    CLRTMP = AC_SIGNALS(IACT)%SDP%YC(IPHASE) + AC_SIGNALS(IACT)%SDP%RC(IPHASE)      
!
! Loop over the ring 2 phases starting with the phase that
! follows phase 6.  The phase 6 split will be calculated
! later.
!
    DO INDEX = 1, 3
      INDEXP = IDXP6 + INDEX
      IF(INDEXP .GT. 8) INDEXP = 5
      IPHASE = SEQ(INDEXP)                                          
!
! If a phase is missing, it will have a zero value in the phase
! sequence array, indicating the phase should not be used.
!
      IF(IPHASE .GT. 0) THEN
!
! Compute the clearance time for the current phase.
!
        CLRTMC = AC_SIGNALS(IACT)%SDP%YC(IPHASE) + AC_SIGNALS(IACT)%SDP%RC(IPHASE)  
!
! Look up the force off for the current phase and compute
! the split.
!
        FOCURR = FORCEOFF(IPHASE)
        SPLIT(IPHASE) = FOCURR + CLRTMC - FOPREV - CLRTMP
!
! Save the current values as the previous values.
!
        FOPREV = FOCURR
        CLRTMP = CLRTMC
      ENDIF
    ENDDO
  ENDIF
!
! Compute the splits for phases 2 and 6 based on which rings        
! have side street phases.                                          
!                                                                       
  Y3OR4 = SPLIT(3) .GT. 0 .OR. SPLIT(4) .GT. 0                      
  Y7OR8 = SPLIT(7) .GT. 0 .OR. SPLIT(8) .GT. 0                      
!                                                                       
  IF(.NOT. Y3OR4) THEN                                              
!                                                                       
!  Ring 1 has no side street phase specifications.  Therefore,     
!  the split settings for the ring 2 side street specifications    
!  will be used.                                                   
!                                                                  
!  First, the split for phase 6 (if it exists) is the remainder of 
!  the cycle length not used by the splits for the other ring 2    
!  phases.                                                         
!                                                                       
    IF(YPH6) THEN                                                   
      SPLIT(6) = CYCLEN - (SPLIT(5) + SPLIT(7) + SPLIT(8))          
    ENDIF                                                           
!                                                                       
! Second, if phase 1 exists, adjust its split by subtracting the  
! time used by the ring 2 side street phases.  This is necessary  
! to keep the barrier crossings aligned between the rings.        
!                                                                       
    IF(SPLIT(1) .GT. 0) THEN                                        
      SPLIT(1) = SPLIT(1) - (SPLIT(7) + SPLIT(8))                   
    ENDIF                                                           
!                                                                       
! Finally, the split for phase 2 is the remainder of the cycle    
! length not used by split 1 and the ring 2 side street phases.   
!                                                                       
    SPLIT(2) = CYCLEN - (SPLIT(1) + SPLIT(7) + SPLIT(8))            
!                                                                       
  ELSEIF(.NOT. Y7OR8) THEN                                          
!                                                                       
! Ring 2 has no side street phase specifications.  Therefore,     
! the split settings for the ring 1 side street specifications    
! will be used.                                                   
!                                                                 
! First, the split for phase 2 is the remainder of the cycle      
! length not used by the splits for the other ring 1 phases.      
!                                                                       
    SPLIT(2) = CYCLEN - (SPLIT(1) + SPLIT(3) + SPLIT(4))            
!                                                                       
    IF(YPH6) THEN                                                   
!                                                                       
! Second, if phase 5 exists, adjust its split by subtracting    
! the time used by the ring 1 side street phases.  This is      
! necessary to keep the barrier crossings aligned between the   
! rings.                                                        
!                                                                       
      IF(SPLIT(5) .GT. 0) THEN                                      
        SPLIT(5) = SPLIT(5) - (SPLIT(3) + SPLIT(4))                 
      ENDIF                                                         
!                                                                       
! Finally, the split for phase 6 is the remainder of the cycle  
! length not used by split 5 and the ring 1 side street phases. 
!                                                                       
      SPLIT(6) = CYCLEN - (SPLIT(5) + SPLIT(3) + SPLIT(4))          
    ENDIF                                                           
!                                                                       
  ELSE                                                              
!                                                                       
! The split for phase 2 is the remainder of the cycle length      
! not used by the splits for the other ring 1 phases.             
!                                                                       
    SPLIT(2) = CYCLEN - (SPLIT(1) + SPLIT(3) + SPLIT(4))            
!                                                                       
! The split for phase 6 (if it exists) is the remainder of the    
! cycle length not used by the splits for the other ring 2        
! phases.                                                         
!                                                                       
    IF(YPH6) THEN                                                   
      SPLIT(6) = CYCLEN - (SPLIT(5) + SPLIT(7) + SPLIT(8))          
    ENDIF                                                           
  ENDIF                                                             
  RETURN
  END
  
  SUBROUTINE CALCMINSPLITS(IACT, SPLIT)
  USE ACTUATED_CONTROLLERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT
  REAL, INTENT(OUT) :: SPLIT(8)
  INTEGER :: PHASE, IMAX
!
! For each phase, calculate the minimum allowable split value.
!
  DO PHASE = 1, 8
!
! Determine the maximum of the min green time and the sum of the pedestrian walk and don't walk times.
!
    IMAX = MAX(AC_SIGNALS(IACT)%SDP%MINIMUM_GREEN_TIME(PHASE), (AC_SIGNALS(IACT)%SDP%PED_WALK_TIME(PHASE) + AC_SIGNALS(IACT)%SDP%PED_CLEARANCE_TIME(PHASE)))      
!
!       Account for the Variable Initial Interval, if enabled.
!       CORSIM supports 3 variable initial interval options:
!       Extensible (code 0), Added (code 1), and Computed (code 3).
!
!       NOTES: 1) If the user specifies a zero seconds/actuation
!                 parameter when using the extensible option, the
!                 variable initial interval operation is disabled.
!                 In that event, the CORSIM input logic sets the
!                 maximum initial interval to the minimum green time.
!
!              2) The added initial option does not use a maximum
!                 initial interval.  CORSIM sets the maximum initial
!                 interval to zero for this option.
!
    IMAX = MAX(IMAX, INT(AC_SIGNALS(IACT)%PHASE(PHASE)%MAX_INIT_INTERVAL))
!
!
! Add the yellow change and red clearance times.
!
    SPLIT(PHASE) = IMAX + AC_SIGNALS(IACT)%SDP%YC(PHASE) + AC_SIGNALS(IACT)%SDP%RC(PHASE)  
  ENDDO
  RETURN
  END
  
  SUBROUTINE CALCFORCEOFFS(IACT, SPLIT, SEQ, FORCEOFF)  
  USE ACTUATED_CONTROLLERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT, SEQ(8)            
  REAL, INTENT(IN) :: SPLIT(8)
  REAL, INTENT(OUT) :: FORCEOFF(8)
  INTEGER :: IPHASE, IDXP2, IDXP6, FOPREV, INDEX, INDEXP
  REAL :: SPTCUR, CLRTMP, CLRTMC
  LOGICAL :: YPH6, Y3OR4, Y7OR8
!
! Initialize the array of force-off times.
!
  FORCEOFF = 0
!
!     ------------------------------------------------------------------
! Compute the force-off values for ring 1 phases.
!
! First, find the index for phase 2 in the phase sequence array.
! A coordinated controller must always have a phase 2.
!
  DO IDXP2 = 1, 4
    IF(SEQ(IDXP2) .EQ. 2) EXIT                                      
  ENDDO
!
! Initialize the previous values to the phase 2 values.
! NOTE: the force off for phase 2 is, by definition, zero.
!
  FOPREV = 0
  IPHASE = 2
  CLRTMP = AC_SIGNALS(IACT)%SDP%YC(IPHASE) + AC_SIGNALS(IACT)%SDP%RC(IPHASE)
!
! Loop over the ring 1 phases starting with the phase that follows phase 2.
!
  Y3OR4 = .FALSE.                                                   
  FORCEOFF(2) = 0
  DO INDEX = 1, 3
    INDEXP = IDXP2 + INDEX
    IF(INDEXP .GT. 4) INDEXP = 1
    IPHASE = SEQ(INDEXP)                                            
!
! If a phase is missing, it will have a zero value in the phase sequence array,
! indicating the phase should not be used.
!
    IF(IPHASE .GT. 0) THEN
!
! Compute the clearance time for the current phase.
!
      CLRTMC = AC_SIGNALS(IACT)%SDP%YC(IPHASE) + AC_SIGNALS(IACT)%SDP%RC(IPHASE)
!
! Look up the split for the current phase and compute the force off.
!
      SPTCUR = SPLIT(IPHASE)
      FORCEOFF(IPHASE) = FOPREV + CLRTMP + SPTCUR - CLRTMC
!
! Save the current values as the previous values.
!
      FOPREV = FORCEOFF(IPHASE)
      CLRTMP = CLRTMC
!
! Keep track of whether there is a ring 1 side street phase.
!
      IF(IPHASE .EQ. 3 .OR. IPHASE .EQ. 4) Y3OR4 = .TRUE.           
    ENDIF
  ENDDO
!
! Compute the force-off values for ring 2 phases.
!
! First, find the index for phase 6 in the phase sequence array.
! If there are any ring 2 phases, phase 6 must exist.
!
  YPH6 = .FALSE.
  DO IDXP6 = 5, 8
    IF(SEQ(IDXP6) .EQ. 6) THEN                                      
      YPH6 = .TRUE.
      EXIT
    ENDIF
  ENDDO
!
! If phase 6 does not exist, skip the processing of ring 2 force offs.
!
  Y7OR8 = .FALSE.
  IF(YPH6) THEN
!
! Initialize the previous values to the phase 6 values.
! NOTE: the force off for phase 6 is, by definition, zero.
!
    FOPREV = 0
    IPHASE = 6
    CLRTMP = AC_SIGNALS(IACT)%SDP%YC(IPHASE) + AC_SIGNALS(IACT)%SDP%RC(IPHASE)
!
! Loop over the ring 2 phases starting with the phase that follows phase 6.
!
    FORCEOFF(2) = 0
    DO INDEX = 1, 3
      INDEXP = IDXP6 + INDEX
      IF(INDEXP .GT. 8) INDEXP = 5
      IPHASE = SEQ(INDEXP)                                          
!
! If a phase is missing, it will have a zero value in the phase sequence array,
! indicating the phase should not be used.
!
      IF(IPHASE .GT. 0) THEN
!
! Compute the clearance time for the current phase.
!
        CLRTMC = AC_SIGNALS(IACT)%SDP%YC(IPHASE) + AC_SIGNALS(IACT)%SDP%RC(IPHASE)  
!
! Look up the split for the current phase and compute the force off.
!
        SPTCUR = SPLIT(IPHASE)
        FORCEOFF(IPHASE) =  FOPREV + CLRTMP + SPTCUR - CLRTMC
!
! Save the current values as the previous values.
!
        FOPREV = FORCEOFF(IPHASE)
        CLRTMP = CLRTMC
!
! Keep track of whether there is a ring 2 side street phase.
!
        IF(IPHASE .EQ. 7 .OR. IPHASE .EQ. 8) Y7OR8 = .TRUE.         
      ENDIF
    ENDDO
!
    IF(.NOT. Y3OR4 .AND. FORCEOFF(1) .GT. 0) THEN                   
!                                                                       
! Ring 1 has no side street phase specifications.  Therefore,
! adjust the value for phase 1 by adding the time used by the   
! ring 2 side street phases.  This is necessary to keep the
! barrier crossings aligned between the rings.                  
!                                                                       
      FORCEOFF(1) = FORCEOFF(1) + SPLIT(7) + SPLIT(8)               
    ELSEIF(.NOT. Y7OR8 .AND. FORCEOFF(5) .GT. 0) THEN               
!                                                                       
! Ring 2 has no side street phase specifications.  Therefore,
! adjust the value for phase 5 by adding the time used by the   
! ring 1 side street phases.  This is necessary to keep the
! barrier crossings aligned between the rings.                  
!                                                                       
      FORCEOFF(5) = FORCEOFF(5) + SPLIT(3) + SPLIT(4)               
    ENDIF 
  ENDIF
  RETURN
  END

    
! ==================================================================================================
  SUBROUTINE LOCAL(IACT)
!----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT
  LOGICAL :: XEQ
!----------------------------------------------------------------------
  !Update the local cycle timer
  CALL INCREMENT_LOCAL_CYCLE_TIMER(IACT)
  IF(XEQ(AC_SIGNALS(IACT)%OFFSET - AC_SIGNALS(IACT)%MASTER_CYCLE_TIMER, 0.)) THEN
    CALL SET_LOCAL_CYCLE_TIMER(IACT, 0.)
  ENDIF
  RETURN
  END
    
! ==================================================================================================
  SUBROUTINE MASTER(IACT)
!----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT
!----------------------------------------------------------------------
  !Update the master cycle timer
  CALL INCREMENT_MASTER_CYCLE_TIMER(IACT)
  IF(MOD(SIMTIME, AC_SIGNALS(IACT)%CYCLE_LENGTH) .LT. 0.0001) THEN
    CALL SET_MASTER_CYCLE_TIMER(IACT, 0.)
  ENDIF
  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE SET_LOCAL_CYCLE_TIMER(IACT, TIME)  
!----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT          
  REAL, INTENT(IN) :: TIME
!----------------------------------------------------------------------
  AC_SIGNALS(IACT)%LOCAL_CYCLE_TIMER = TIME
  AC_SIGNALS(IACT)%LOCAL_CYCLE_COUNTER = TIME / SCOPE_TIMESTEP
  RETURN
  END

! ==================================================================================================
  SUBROUTINE INCREMENT_LOCAL_CYCLE_TIMER(IACT)  
!----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT          
!----------------------------------------------------------------------
  AC_SIGNALS(IACT)%LOCAL_CYCLE_COUNTER = AC_SIGNALS(IACT)%LOCAL_CYCLE_COUNTER + 1
  AC_SIGNALS(IACT)%LOCAL_CYCLE_TIMER = AC_SIGNALS(IACT)%LOCAL_CYCLE_COUNTER * SCOPE_TIMESTEP
  RETURN
  END

! ==================================================================================================
  SUBROUTINE SET_MASTER_CYCLE_TIMER(IACT, TIME)  
!----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT          
  REAL, INTENT(IN) :: TIME
!----------------------------------------------------------------------
  AC_SIGNALS(IACT)%MASTER_CYCLE_TIMER = TIME
  AC_SIGNALS(IACT)%MASTER_CYCLE_COUNTER = TIME / SCOPE_TIMESTEP
  RETURN
  END

! ==================================================================================================
  SUBROUTINE INCREMENT_MASTER_CYCLE_TIMER(IACT)  
!----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT          
!----------------------------------------------------------------------
  AC_SIGNALS(IACT)%MASTER_CYCLE_COUNTER = AC_SIGNALS(IACT)%MASTER_CYCLE_COUNTER + 1
  AC_SIGNALS(IACT)%MASTER_CYCLE_TIMER = AC_SIGNALS(IACT)%MASTER_CYCLE_COUNTER * SCOPE_TIMESTEP
  RETURN
  END

! ==================================================================================================
  SUBROUTINE SET_COORDINATION_TIMER(IACT, TIME)  
!----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT          
  REAL, INTENT(IN) :: TIME
!----------------------------------------------------------------------
  AC_SIGNALS(IACT)%COORDINATION_TIMER = TIME
  AC_SIGNALS(IACT)%COORDINATION_COUNTER = TIME / SCOPE_TIMESTEP
  RETURN
  END

! ==================================================================================================
  SUBROUTINE DECREMENT_COORDINATION_TIMER(IACT)  
!----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT          
!----------------------------------------------------------------------
  AC_SIGNALS(IACT)%COORDINATION_COUNTER = AC_SIGNALS(IACT)%COORDINATION_COUNTER - 1
  AC_SIGNALS(IACT)%COORDINATION_TIMER = AC_SIGNALS(IACT)%COORDINATION_COUNTER * SCOPE_TIMESTEP
  RETURN
  END

! ==================================================================================================
  SUBROUTINE SET_CYCLE_LENGTH(IACT, TIME)  
!----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE SIMPARAMS
  USE SCOPE_PARAMETERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT          
  REAL, INTENT(IN) :: TIME
!----------------------------------------------------------------------
  AC_SIGNALS(IACT)%CYCLE_LENGTH = TIME
  RETURN
  END  

! ==================================================================================================
  SUBROUTINE RESET_SPLIT(IACT, DIFF)  
!----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IACT
  REAL, INTENT(IN) :: DIFF
  INTEGER :: RING, PHASE
!----------------------------------------------------------------------
  DO RING = 1, 2
    PHASE = AC_SIGNALS(IACT)%CURRENT_PHASES(RING)
    IF(PHASE .EQ. 2 .OR. PHASE .EQ. 6) THEN
      AC_SIGNALS(IACT)%SDP%TIME_LIMIT(PHASE) = AC_SIGNALS(IACT)%SDP%TIME_LIMIT(PHASE) + DIFF
    ENDIF
  ENDDO
  RETURN
  END    
  
! ==================================================================================================
  LOGICAL FUNCTION XEQ(A, B)
!----------------------------------------------------------------------
! Replaces .EQ. for floating point numbers.  
!----------------------------------------------------------------------
  IMPLICIT NONE
  REAL :: A, B
!----------------------------------------------------------------------
  IF(ABS(A - B) .LT. 0.0001) THEN
    XEQ = .TRUE.
  ELSE
    XEQ = .FALSE.
  ENDIF
  END
