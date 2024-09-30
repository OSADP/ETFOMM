! ==================================================================================================
  INTEGER FUNCTION GET_CONTROLLER_ID[DLLEXPORT, STDCALL] (NODE, CONTROLLER_ID)
! ----------------------------------------------------------------------
! Description: Indicates if the specified node is under actuated
!              control.
!
! Arguments: NODE - [in] ID of the node at which the controller is
!                      located.
!
!            CONTROLLER_ID - [out]
!
! Returns: int =  0 - success
!              = -1 - specified node is not an allowable node number
! ----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE STREET_NODES
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NODE
  INTEGER, INTENT(OUT) :: CONTROLLER_ID [REFERENCE]
! ----------------------------------------------------------------------
  IF(NODE_TYPE(NODE) .EQ. NT_INTERN) THEN
    CONTROLLER_ID = NACT(NODE)
    GET_CONTROLLER_ID = 0
  ELSE
    CONTROLLER_ID = 0
    GET_CONTROLLER_ID = -1
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_LOCAL_CYCLE_TIMER[DLLEXPORT, STDCALL] (NODE, LOCAL_CYCLE_TIMER)
! ----------------------------------------------------------------------
! Description: Returns the value of the controller's local cycle
!              timer in seconds past its yield point.
!
! Arguments: NODE - [in] ID of the node at which the controller is
!                      located.
!            LOCAL_CYCLE_TIMER - [out]
!  
! Returns: int =  0 - success
!              = -1 - specified node is not under actuated control
! ----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE STREET_NODES
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NODE
  REAL, INTENT(OUT) :: LOCAL_CYCLE_TIMER [REFERENCE]
  INTEGER :: IACT
! ----------------------------------------------------------------------
! Look up the controller ID and determine if the node is under actuated control.
  IACT = NACT(NODE)
  IF(IACT .GT. 0) THEN

! Return the value of the local cycle timer for the specified controller.
    LOCAL_CYCLE_TIMER = AC_SIGNALS(IACT)%LOCAL_CYCLE_TIMER
    GET_LOCAL_CYCLE_TIMER = 0
  ELSE

! Return an error code indicating the node is not under actuated control.
    GET_LOCAL_CYCLE_TIMER = -1
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_CYCLE_LENGTH[DLLEXPORT, STDCALL] (NODE, CYCLE_LENGTH)
! ----------------------------------------------------------------------
! Description: Returns the cycle length currently in use by the
!              controller at the specified node.
!
! Arguments: NODE - [in] ID of the node at which the controller is
!                      located.
!            CYCLE_LENGTH - [out]
!
! Returns: int =  0 - success
!              = -1 - specified node is not under actuated control
! ----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE STREET_NODES
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NODE
  REAL, INTENT(OUT) :: CYCLE_LENGTH [REFERENCE]
  INTEGER :: IACT
! ----------------------------------------------------------------------
! Look up the controller ID and determine if the node is under actuated control.
  IACT = NACT(NODE)
  IF(IACT .GT. 0) THEN

! Return the value of the cycle length for the specified controller.
    CYCLE_LENGTH = AC_SIGNALS(IACT)%CYCLE_LENGTH
    GET_CYCLE_LENGTH = 0
  ELSE

! Return an error code indicating the node is not under actuated control.
    GET_CYCLE_LENGTH = -1
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_OFFSET[DLLEXPORT, STDCALL] (NODE, OFFSET)
! ----------------------------------------------------------------------
! Description: Returns the offset currently in use by the
!              controller at the specified node.
!
! Arguments: NODE - [in] ID of the node at which the controller is
!                      located.
!            OFFSET - [out]
!
! Returns: int =  0 - success
!              = -1 - specified node is not under actuated control
! ----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE STREET_NODES
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NODE
  REAL, INTENT(OUT) :: OFFSET [REFERENCE]
  INTEGER :: IACT
! ----------------------------------------------------------------------
! Look up the controller ID and determine if the node is under actuated control.
  IACT = NACT(NODE)
  IF(IACT .GT. 0) THEN

! Return the value of the cycle length for the specified controller.
    OFFSET = AC_SIGNALS(IACT)%OFFSET
    GET_OFFSET = 0
  ELSE

! Return an error code indicating the node is not under actuated control.
    GET_OFFSET = -1
  ENDIF
  END 
  
! ==================================================================================================
  INTEGER FUNCTION GET_NEW_OFFSET[DLLEXPORT, STDCALL] (NODE, NEW_OFFSET)
! ----------------------------------------------------------------------
! Description: Returns the offset that will be implemented at the
!              next plan update by the controller at the specified node.
!
!              NOTE: if the plan update has already occurred, this
!                    function will return the same value as
!                    GetOffset.
!
! Arguments: NODE - [in] ID of the node at which the controller is
!                      located.
!            NEW_OFFSET - [out]
!
! Returns: int =  0 - success
!              = -1 - specified node is not under actuated control
! ----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE STREET_NODES
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NODE
  REAL, INTENT(OUT) :: NEW_OFFSET [REFERENCE]
  INTEGER :: IACT
! ----------------------------------------------------------------------
! Look up the controller ID and determine if the node is under actuated control.
  IACT = NACT(NODE)
  IF(IACT .GT. 0) THEN

! Return the value of the offset for the specified controller.
    NEW_OFFSET = AC_SIGNALS(IACT)%NEW_OFFSET
    GET_NEW_OFFSET = 0
  ELSE

! Return an error code indicating the node is not under actuated control.
    GET_NEW_OFFSET = -1
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_NEW_CYCLE_LENGTH[DLLEXPORT, STDCALL] (NODE, NEW_CYCLE_LENGTH)
! ----------------------------------------------------------------------
! Description: Returns the cycle length that will be implemented at the
!              next plan update by the controller at the specified node.
!
!              NOTE: if the plan update has already occurred, this
!                    function will return the same value as
!                    GetOffset.
!
! Arguments: NODE - [in] ID of the node at which the controller is
!                      located.
!            NEW_CYCLE_LENGTH - [out]
!
! Returns: int =  0 - success
!              = -1 - specified node is not under actuated control
! ----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE STREET_NODES
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NODE
  REAL, INTENT(OUT) :: NEW_CYCLE_LENGTH [REFERENCE]
  INTEGER :: IACT
! ----------------------------------------------------------------------
! Look up the controller ID and determine if the node is under actuated control.
  IACT = NACT(NODE)
  IF(IACT .GT. 0) THEN

! Return the value of the cycle length for the specified controller.
    NEW_CYCLE_LENGTH = AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH
    GET_NEW_CYCLE_LENGTH = 0
  ELSE

! Return an error code indicating the node is not under actuated control.
    GET_NEW_CYCLE_LENGTH = -1
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION SET_NEW_CYCLE_LENGTH[DLLEXPORT, STDCALL] (NODE, CYCLELENGTH)
! ----------------------------------------------------------------------
! Description: Sets the cycle length that will be implemented at the
!              next plan update by the controller at the specified node.
!
! Arguments: NODE - [in] ID of the node at which the controller is
!                      located.
!
!            CYCLELENGTH - [in] the cycle length to be used.
!
! Returns: int =  0 - success
!              = -1 - specified node is not under actuated control
!              = -2 - a plan transition is already in progress
!              = -3 - specified cycle length is invalid  
! ----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE STREET_NODES
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NODE
  REAL, INTENT(IN) :: CYCLELENGTH
  INTEGER :: IACT
! ----------------------------------------------------------------------
! Look up the controller ID and determine if the node is under actuated control.
  IACT = NACT(NODE)
  IF(IACT .GT. 0) THEN
    IF(AC_SIGNALS(IACT)%INTRAN .GT. 0) THEN

! A plan transition is already in progress.
      SET_NEW_CYCLE_LENGTH = -2
    ELSE
      IF(CYCLELENGTH .GE. 1) THEN

! Set the cycle length that will be implemented by the controller at its next plan update. 
! Also, set the flag that indicates a new plan has been specified.
        AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH = CYCLELENGTH
        AC_SIGNALS(IACT)%NEWPLAN = .TRUE.
        SET_NEW_CYCLE_LENGTH = 0
        WRITE(MSGTEXT, '(A,I4,A,F10.1,A,F10.1)') 'node: ', NODE, ' ---changing cycle length from ', AC_SIGNALS(IACT)%CYCLE_LENGTH, ' to ', CYCLELENGTH
        CALL SENDTEXTMSG(M_INFO)
      ELSE
 
! Specified cycle length is invalid.
        SET_NEW_CYCLE_LENGTH = -3
      ENDIF
    ENDIF
  ELSE
 
! Return an error code indicating the node is not under actuated control.
    SET_NEW_CYCLE_LENGTH = -1
  ENDIF
  END  
  
! ==================================================================================================
  INTEGER FUNCTION SET_NEW_OFFSET[DLLEXPORT, STDCALL] (NODE, OFFSET)
! ----------------------------------------------------------------------
! Description: Sets the cycle length that will be implemented at the
!              next plan update by the controller at the specified node.
!
! Arguments: NODE - [in] ID of the node at which the controller is
!                      located.
!
!            OFFSET - [in] the offset to be used.
!
! Returns: int =  0 - success
!              = -1 - specified node is not under actuated control
!              = -2 - a plan transition is already in progress
!              = -3 - specified cycle length is invalid  
! ----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE STREET_NODES
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NODE
  REAL, INTENT(IN) :: OFFSET
  INTEGER :: IACT
! ----------------------------------------------------------------------
! Look up the controller ID and determine if the node is under actuated control.
  IACT = NACT(NODE)
  IF(IACT .GT. 0) THEN
    IF(AC_SIGNALS(IACT)%INTRAN .GT. 0) THEN

! A plan transition is already in progress.
      SET_NEW_OFFSET = -2
    ELSE
      IF(OFFSET .GE. 0) THEN

! Set the offset that will be implemented by the controller at its next plan update. 
! Also, set the flag that indicates a new plan has been specified.
        AC_SIGNALS(IACT)%NEW_OFFSET = OFFSET
        AC_SIGNALS(IACT)%NEWPLAN = .TRUE.
        SET_NEW_OFFSET = 0
        WRITE(MSGTEXT, '(A,I4,A,F10.1,A,F10.1)') 'node: ', NODE, ' ---changing offset from ', AC_SIGNALS(IACT)%OFFSET, ' to ', OFFSET
        CALL SENDTEXTMSG(M_INFO)
      ELSE
 
! Specified cycle offset is invalid.
        SET_NEW_OFFSET = -3
      ENDIF
    ENDIF
  ELSE
 
! Return an error code indicating the node is not under actuated control.
    SET_NEW_OFFSET = -1
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_SPLITS[DLLEXPORT, STDCALL] (NODE, SPLITS)
! ----------------------------------------------------------------------
! Description: Returns the phase splits currently in use by the
!              controller at the specified node.
!
! Arguments: nNodeID - [in] ID of the node at which the controller is
!                      located.
!
!            nSplits - [out] address of an array to receive the
!                      split values.
!
! Returns: int =  0 - success
!              = -1 - specified node is not under actuated control
! ----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE STREET_NODES
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NODE
  REAL, INTENT(OUT) :: SPLITS(NPHASES)
  INTEGER :: IACT
! ----------------------------------------------------------------------
! Look up the controller ID and determine if the node is under actuated control.
  IACT = NACT(NODE)
  IF(IACT .GT. 0) THEN
!
! Calculate the splits from the current force off values and current cycle length.
    CALL CALCSPLITS(IACT, AC_SIGNALS(IACT)%CYCLE_LENGTH, AC_SIGNALS(IACT)%FORCE_OFF_TIME, AC_SIGNALS(IACT)%PHASE_SEQUENCE, SPLITS)
    GET_SPLITS = 0
  ELSE
!
! Return an error code indicating the node is not under actuated control.
    GET_SPLITS = -1
  ENDIF
  END
    
! ==================================================================================================
  INTEGER FUNCTION GET_MIN_SPLITS[DLLEXPORT, STDCALL] (NODE, MIN_SPLITS)
! ----------------------------------------------------------------------
! Description: Returns the minimum allowable phase splits based on the
!              current min. green, pedestrian, and clearance times for
!              the controller at the specified node.
!
! Arguments: NODE - [in] ID of the node at which the controller is
!                      located.
!
!            MIN_SPLITS - [out] address of an array to receive the
!                         minimum split values.
!
! Returns: int =  0 - success
!              = -1 - specified node is not under actuated control
! ----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE STREET_NODES
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NODE
  REAL, INTENT(OUT) :: MIN_SPLITS(NPHASES)
  INTEGER :: IACT
! ----------------------------------------------------------------------
! Look up the controller ID and determine if the node is under actuated control.
  IACT = NACT(NODE)
  IF(IACT .GT. 0) THEN
!
! Calculate the minimum splits for the specified controller.
    CALL CALCMINSPLITS(IACT, MIN_SPLITS)
    GET_MIN_SPLITS = 0
  ELSE
!
! Return an error code indicating the node is not under actuated control.
    GET_MIN_SPLITS = -1
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION GET_NEW_SPLITS[DLLEXPORT, STDCALL] (NODE, SPLITS)
! ----------------------------------------------------------------------
! Description: Returns the phase splits that will be implemented at the
!              next plan update by the controller at the specified node.
!
!              NOTE: if the plan update has already occurred, this
!                    function will return the same value as
!                    GetSplits.
!
! Arguments: NODE - [in] ID of the node at which the controller is
!                      located.
!
!            SPLITS - [out] address of an array to receive the
!                      split values.
!
! Returns: int =  0 - success
!              = -1 - specified node is not under actuated control
! ----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE STREET_NODES
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NODE
  REAL, INTENT(OUT) :: SPLITS(NPHASES)
  INTEGER :: IACT
! ----------------------------------------------------------------------
! Look up the controller ID and determine if the node is under actuated control.
  IACT = NACT(NODE)
  IF(IACT .GT. 0) THEN
!
! Calculate the splits from the pending force off values and pending cycle length.
    CALL CALCSPLITS(IACT, AC_SIGNALS(IACT)%NEW_CYCLE_LENGTH, AC_SIGNALS(IACT)%NEW_FORCE_OFF_TIME, AC_SIGNALS(IACT)%PHASE_SEQUENCE, SPLITS)
    GET_NEW_SPLITS = 0
  ELSE
!
! Return an error code indicating the node is not under actuated control.
    GET_NEW_SPLITS = -1
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION SET_NEW_SPLITS[DLLEXPORT, STDCALL] (NODE, SPLITS)
! ----------------------------------------------------------------------
! Description: Sets the phase splits that will be implemented at the
!              next plan update by the controller at the specified node.
!
! Arguments: NODE - [in] ID of the node at which the controller is
!                      located.
!
!            SPLITS - [in] address of an array that contains the new
!                      split values.
!
! Returns: int =  0 - success
!              = -1 - specified node is not under actuated control
!              = -2 - a plan transition is already in progress
!              = -3 - at least one specified split is invalid
! ----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE STREET_NODES
  USE TEXT
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NODE
  REAL, INTENT(IN) :: SPLITS(NPHASES)
  INTEGER :: IACT, IPHASE
  LOGICAL :: WVALID
  INTEGER :: SEQ(8)
! ----------------------------------------------------------------------
! Look up the controller ID and determine if the node is under actuated control.
  IACT = NACT(NODE)
  IF(IACT .EQ. 0) THEN
!    
! Return an error code indicating the node is not under actuated control.
    SET_NEW_SPLITS = -1
    
  ELSEIF(AC_SIGNALS(IACT)%INTRAN .GT. 0) THEN

! A plan transition is already in progress.
    SET_NEW_SPLITS = -2
    
  ELSEIF(IACT .GT. 0) THEN
!
! Validate the split values.
    WVALID = .TRUE.
    DO IPHASE = 1, NPHASES
      IF(SPLITS(IPHASE) .LT. 0) WVALID = .FALSE.
    ENDDO
!
    IF(WVALID) THEN
!
! Calculate and set the new force offs from the specified split values.
! Also, set the flag that indicates a new plan has been specified.
      WRITE(MSGTEXT, '(A,I4,A,8F6.1)') 'node: ', NODE, ' ---changing splits from ', AC_SIGNALS(IACT)%PHASE_SPLITS
      CALL SENDTEXTMSG(M_INFO)
      WRITE(MSGTEXT, '(A,8F6.1)') '                                to ', SPLITS
      CALL SENDTEXTMSG(M_INFO)
      WRITE(MSGTEXT, '(A,8F6.1)') '              existing force off times ', AC_SIGNALS(IACT)%FORCE_OFF_TIME
      CALL SENDTEXTMSG(M_INFO)
      SEQ(1:4) = AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(1, 1:4)
      SEQ(5:8) = AC_SIGNALS(IACT)%RING_PHASE_SEQUENCE(2, 1:4)
      CALL CALCFORCEOFFS(IACT, SPLITS, SEQ, AC_SIGNALS(IACT)%NEW_FORCE_OFF_TIME) 
      AC_SIGNALS(IACT)%NEWPLAN = .TRUE.
      SET_NEW_SPLITS = 0
      WRITE(MSGTEXT, '(A,8F6.1)') '                   new force off times ', AC_SIGNALS(IACT)%NEW_FORCE_OFF_TIME
      CALL SENDTEXTMSG(M_INFO)
    ELSE
!
! At least one of the new splits is invalid.
      SET_NEW_SPLITS = -3
    ENDIF
  ENDIF
  END

! ==================================================================================================
  INTEGER FUNCTION GET_TRANSITION_METHOD[DLLEXPORT, STDCALL] (NODE, TRANSITION_METHOD, MAXPCTADD, MAXPCTSUB)
! ----------------------------------------------------------------------
! Description: Returns the plan transition method and parameters for
!              the controller at the specified node.
!
! Arguments: NODE - [in] ID of the node at which the controller is
!                      located.
!
!            MAXPCTADD - [out] address of integer to receive the
!                         maximum percent add value
!
!            MAXPCTSUB - [out] address of integer to receive the
!                         maximum percent subtract value
!  
!            TRANSITION_METHOD - [out]  
!
! Returns: int =  0 - short way transition method
!              =  1 - dwell transition method
!              =  2 - add transition method
!              =  3 - subtract transition method
!              = -1 - specified node is not under actuated control
! ----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE STREET_NODES
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NODE
  INTEGER, INTENT(OUT) :: TRANSITION_METHOD [REFERENCE]
  REAL :: MAXPCTADD [REFERENCE], MAXPCTSUB [REFERENCE]
  INTEGER :: IACT
! ----------------------------------------------------------------------
! Look up the controller ID and determine if the node is under actuated control.
  IACT = NACT(NODE)
  IF(IACT .GT. 0) THEN
!
! Load the transition method parameters into the return arguments.
    MAXPCTADD = AC_SIGNALS(IACT)%MAXPCT_ADD
    MAXPCTSUB = AC_SIGNALS(IACT)%MAXPCT_SUBTRACT
!
! Return the plan transition method.
    TRANSITION_METHOD = AC_SIGNALS(IACT)%TRANSITION_METHOD
    GET_TRANSITION_METHOD = 0
  ELSE
!
! Return an error code indicating the node is not under actuated control.
    GET_TRANSITION_METHOD = -1
  ENDIF
  END
  
! ==================================================================================================
  INTEGER FUNCTION SET_TRANSITION_METHOD[DLLEXPORT, STDCALL] (NODE, METHOD, MAXPCTADD, MAXPCTSUB)
! ----------------------------------------------------------------------
! Description: Returns the plan transition method and parameters for
!              the controller at the specified node.
!
! Arguments: NODE - [in] ID of the node at which the controller is
!                      located.
!
!            METHOD - [in] plan transition method:
!                           0 = short way transition method
!                           1 = dwell transition method
!                           2 = add transition method
!                           3 = subtract transition method
!
!            MAXPCTADD - [in] maximum percent add value
!
!            MAXPCTSUB - [in] maximum percent subtract value
!
! Returns: int =  0 - success
!              = -1 - specified node is not under actuated control
!              = -2 - invalid method specified
!              = -3 - maximum percent add out of range [1, 100]
!              = -4 - maximum percent subtract out of range [1, 100]
! ----------------------------------------------------------------------
  USE ACTUATED_CONTROLLERS
  USE STREET_NODES
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NODE, METHOD
  REAL :: MAXPCTADD, MAXPCTSUB
  INTEGER :: IACT
  LOGICAL :: WVALID
! ----------------------------------------------------------------------
! Look up the controller ID and determine if the node is under actuated control.
  IACT = NACT(NODE)
  IF(IACT .GT. 0) THEN
!
! Validate the specified method.
    WVALID = .TRUE.
    IF(METHOD .LT. 0 .OR. METHOD .GT. 4) THEN
      SET_TRANSITION_METHOD = -2
      WVALID = .FALSE.
    ELSE
!
! Validate the maximum percent add value.
      IF(METHOD .LT. 2) THEN
        IF(MAXPCTADD .LT. 1 .OR. MAXPCTADD .GT. 100) THEN
          SET_TRANSITION_METHOD = -3
          WVALID = .FALSE.
        ENDIF
      ENDIF
!
! Validate the maximum percent subtract value.
      IF(METHOD .EQ. 0 .OR. METHOD .EQ. 3) THEN
        IF(MAXPCTSUB .LT. 1 .OR. MAXPCTSUB .GT. 100) THEN
          SET_TRANSITION_METHOD = -4
          WVALID = .FALSE.
        ENDIF
      ENDIF
    ENDIF
!
! If the validation tests passed, set the values.
    IF(WVALID) THEN
!
! Store the specified method.
      AC_SIGNALS(IACT)%TRANSITION_METHOD = METHOD
!
! Set the maximum percent adjustment values based on the transition method.
      IF(METHOD .EQ. 0) THEN
        AC_SIGNALS(IACT)%MAXPCT_ADD = MAXPCTADD
        AC_SIGNALS(IACT)%MAXPCT_SUBTRACT = MAXPCTSUB
      ELSEIF(METHOD .EQ. 1 .OR. METHOD .EQ. 2) THEN
        AC_SIGNALS(IACT)%MAXPCT_ADD = MAXPCTADD
      ELSEIF(METHOD .EQ. 3) THEN
        AC_SIGNALS(IACT)%MAXPCT_SUBTRACT = MAXPCTSUB
      ENDIF
!
      SET_TRANSITION_METHOD = 0
    ENDIF
  ELSE
!
! Return an error code indicating the node is not under actuated control.
    SET_TRANSITION_METHOD = -1
  ENDIF
  END
  
!! ==================================================================================================
!  INTEGER FUNCTION EXTEND_GREEN[DLLEXPORT, STDCALL](IACT, PHASE)
!! ----------------------------------------------------------------------
!! Keep the signal in the current phase, until reaching max green.  
!! ----------------------------------------------------------------------
!  USE ACTUATED_CONTROLLERS
!  IMPLICIT NONE
!  INTEGER, INTENT(IN) :: IACT, PHASE
!! ----------------------------------------------------------------------
!  AC_SIGNALS(IACT)%EXTEND_GREEN(PHASE) = .TRUE.
!  EXTEND_GREEN = 0
!  END
