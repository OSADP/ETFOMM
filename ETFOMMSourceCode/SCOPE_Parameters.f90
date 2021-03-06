  MODULE SCOPE_PARAMETERS
    INTEGER, PARAMETER :: PRESENCE_MODE = 0
    INTEGER, PARAMETER :: RECALL_MIN = 1
    INTEGER, PARAMETER :: RECALL_MAX = 2
    INTEGER, PARAMETER :: MAX_OUT = 3
    INTEGER, PARAMETER :: GAP_OUT = 4
    INTEGER, PARAMETER :: PEDESTRIAN_RECALL = 5
    INTEGER, PARAMETER :: RING_1 = 1
    INTEGER, PARAMETER :: RING_2 = 2
    INTEGER, PARAMETER :: FOUR_LEG = 0
    INTEGER, PARAMETER :: THREE_LEG = 1
    INTEGER, PARAMETER :: ACTUATED = 3
    INTEGER :: ACTUATED_PRIORITIES(8) = (/2,6,1,5,3,7,4,8/)
    REAL, PARAMETER :: SCOPE_TIMESTEP = 0.1
    REAL, PARAMETER :: TIMER_STOPPED = -1.0
    INTEGER :: ISTEP, ISTEPS
  END MODULE
