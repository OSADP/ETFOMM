! ==================================================================================================
  INTEGER FUNCTION PUT_DETECTOR_STATES[DLLEXPORT, STDCALL] (STATES)
!--------------------------------------------------------------------
! This function will set the current state of all detectors.
!  0 indicates that the detector is not currently detecting a vehicle
!  1 indicates that the detector is currently detecting a vehicle
!--------------------------------------------------------------------
  USE NTCIP_DATA
  IMPLICIT NONE
  INTEGER :: STATES
! ----------------------------------------------------------------------
  DETECTOR_STATES = STATES
  PUT_DETECTOR_STATES = 0
  END

