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

! ==================================================================================================
  SUBROUTINE STEP_SIMULATION(NSTEPS)
! ----------------------------------------------------------------------
! --- Run the simulation for one time step.
! ----------------------------------------------------------------------
  USE SIMPARAMS
  USE TEXT
  USE FREEWAY_VEHICLES
  USE STREET_VEHICLES
  USE FREEWAY_LINKS
  USE STREET_LINKS
  USE SEEDS
  USE STREET_DETECTORS
  USE GLOBAL_DATA
  USE CAR_FOLLOWING
  USE VDATA
  USE OMP_LIB 
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: NSTEPS
#ifdef TSIS_COMPATIBLE
  INCLUDE 'CORWIN.FI'
#endif    
  COMMON /GRAPHIO/ TSTEPFILE, TINTERVALFILE
  INTEGER :: TSTEPFILE, TINTERVALFILE
  INTEGER :: IV, INODE, ILANE, ILX, ILD, NTHREADS
  REAL :: CPUSEC, TSTART = 0, ACCEL, CF_DATA(7)
  LOGICAL :: FIRST = .TRUE.
! ----------------------------------------------------------------------
!Need an interface because SORT_VEHICLE_LIST doesn't work with the CVF calling convention  
  INTERFACE
  SUBROUTINE QSORT_VEHICLE_LIST
!DEC$ ATTRIBUTES DEFAULT, DECORATE, ALIAS:'SORT_VEHICLE_LIST' ::  QSORT_VEHICLE_LIST
  END
  END INTERFACE

! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
#endif

#ifdef TSIS_COMPATIBLE
    
  ! --- If not in the initialization phase...  
  ! --- Tell the Output Processor when an interval begins.
 
  IF(.NOT. INITMODE) THEN 
    IF(MOD(SIMTIME, FLOAT(TIME_INTERVAL)) .LT. TIMESTEP/2) CALL SIMSTATUS(SIM_BEGININTERVAL)
 
! --- Tell the Output Processor that a time step is beginning.
 
    IF(MOD(SIMTIME, 1.0) .LT. TIMESTEP/2) CALL SIMSTATUS(SIM_BEGINTIMESTEP)
  ENDIF
#endif

! --- Update traffic flow and turn percentages.
 
  CALL UPDATE_TURNPCTS
  
! --- Process vehicles on interface links. Use car following
! --- to determine if the vehicle needs to slow down going
! --- across the interface.
       
  IF(N_FREEWAY_LINKS .GT. 0 .AND. N_STREET_LINKS .GT. 0) THEN
    DO IV = 1, HIGHEST_INDEX_F
#ifdef DebugVersion
      temp = fid(iv)
#endif
      FDECEL(IV) = 20.
      IF(FLINK(IV) .EQ. 0) CYCLE
      IF(FLEADER(IV) .NE. 0) CYCLE
      IF(NODE_TYPE(FDSN(FLINK(IV))) .NE. NT_INTERN) THEN
        DO ILX = 1, N_STREET_LINKS
          IF(SUSN(ILX) .EQ. FDSN(FLINK(IV))) THEN
            ILD = SLAST_VEHICLE(ILX, FLANE(IV))
            IF(ILD .NE. 0) THEN
              IF(SLINK(ILD) .EQ. ILX .AND. SLANE(ILD) .EQ. FLANE(IV)) THEN
                CF_DATA(1) = FZFOLL(FDRIVERTYPE(IV))
                CF_DATA(2) = SSPEED(ILD)
                CF_DATA(3) = FSPEED(IV)
                CF_DATA(4) = FLENGTH(FLINK(IV)) - FLOCATION(IV) + SLOCATION(ILD) - SVLENGTH(ILD)
                CF_DATA(5) = MAX_DECEL(FVTYPE(IV), FSPEED(IV), FGRADE(FLINK(IV)))
                CF_DATA(6) = FCFMULT(FLINK(IV))
                CF_DATA(7) = FPCFSEP
                CALL INTERFACE_CAR_FOLLOW(CF_DATA, FDECEL(IV))
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
    ENDDO
    DO IV = 1, HIGHEST_INDEX_S
      SDECEL(IV) = 20.
      IF(SLINK(IV) .EQ. 0) CYCLE
      IF(SLEADER(IV) .NE. 0) CYCLE
      IF(NODE_TYPE(SDSN(SLINK(IV))) .NE. NT_INTERN) THEN
        DO ILX = 1, N_FREEWAY_LINKS
          IF(FUSN(ILX) .EQ. SDSN(SLINK(IV))) THEN
            ILD = FLAST_VEHICLE(ILX, SLANE(IV))
            IF(ILD .NE. 0) THEN
              IF(FLINK(ILD) .EQ. ILX .AND. FLANE(ILD) .EQ. SLANE(IV)) THEN           
                CF_DATA(1) = SZFOLL(SDRIVERTYPE(IV))
                CF_DATA(3) = SSPEED(IV)
                CF_DATA(2) = FSPEED(ILD)
                CF_DATA(4) = SLENGTH(SLINK(IV)) - SLOCATION(IV) + FLOCATION(ILD) - FVLENGTH(ILD)
                CF_DATA(5) = MAX_DECEL(SVTYPE(IV), SSPEED(IV), SGRADE(SLINK(IV)))
                CF_DATA(6) = SCFMULT(SLINK(IV))
                CF_DATA(7) = SPCFSEP
                CALL INTERFACE_CAR_FOLLOW(CF_DATA, SDECEL(IV))
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
    ENDDO
  ENDIF
  
#ifndef DebugVersion
  IF(FIRST) THEN
    FIRST = .FALSE.
    
    !By default, threads will wait 200 ms before sleeping after they have completed their processing.
    !Setting KMP_BLOCKTIME to 0 will force the threads to sleep immediately after completing their processing.
    !Leaving the setting at 200 ms will greatly increase total CPU time and will only slightly increase elapsed time,
    !which will make parallel processing seem to reduce elapsed time by almost 50%. Reducing the wait time to 0
    !will reduce total elapsed time of the simulation, but will make parallel processing seem less effective.
    CALL KMP_SET_BLOCKTIME(0)
    
    NTHREADS = MIN(2, OMP_GET_MAX_THREADS())
    CALL OMP_SET_NUM_THREADS(NTHREADS)
    !WRITE(MSGTEXT, '(A,I2)') 'Number of threads = ', OMP_GET_MAX_THREADS()
    !CALL SENDTEXTMSG(M_INFO)
  ENDIF                                                   

! --- Use compiler directives to perform parallel processing.
!$OMP PARALLEL
!$OMP SECTIONS   !Define the sections for parallel processing.
!$OMP SECTION    !Start the first section and process freeway vehicles.
#endif 

! --- Reset processing codes for freeway vehicles.
 
  IF(N_FREEWAY_LINKS .GT. 0) THEN
    !WRITE(MSGTEXT, '(A,I1)') 'Freeway from thread ', OMP_GET_THREAD_NUM()
    !CALL SENDTEXTMSG(M_INFO)
    DO IV = 1, HIGHEST_INDEX_F
      FXCODE(IV) = MAX(FXCODE(IV) - 1, 0)
    ENDDO
 
! --- Update freeway vehicles.
 
    CALL QSORT_VEHICLE_LIST
    CALL UPDATE_FREEWAY_VEHICLES
 
! --- Decrement freeway lane change timers.
               
    DO IV = 1, HIGHEST_INDEX_F
      FLC_TIMER(IV) = FLC_TIMER(IV) - TIMESTEP
    ENDDO
  ENDIF
  
#ifndef DebugVersion
!$OMP SECTION    !Start the second section and process street vehicles.
#endif 
 
! --- Reset processing codes for street vehicles.
 
  IF(N_STREET_LINKS .GT. 0) THEN
    !WRITE(MSGTEXT, '(A,I1)') '                 Street from thread ', OMP_GET_THREAD_NUM()
    !CALL SENDTEXTMSG(M_INFO)
    DO IV = 1, HIGHEST_INDEX_S
      SXCODE(IV) = MAX(SXCODE(IV) - 1, 0)
    ENDDO
 
! --- Update street vehicles.
 
    CALL UPDATE_STREET_VEHICLES
 
! --- Decrement lane change timers.
               
    DO IV = 1, HIGHEST_INDEX_S
      SLC_TIMER(IV) = SLC_TIMER(IV) - TIMESTEP
    ENDDO
  ENDIF
!  
#ifndef DebugVersion
!$OMP END SECTIONS
!$OMP END PARALLEL  
#endif 
 
! --- Move vehicles from storage.
 
  IF(N_FREEWAY_LINKS .GT. 0 .AND. N_STREET_LINKS .GT. 0) THEN
    CALL QSORT_VEHICLE_LIST
    DO INODE = 7000, 7999
      DO ILANE = 1, N_INTERFACE_LANES
        IF(FCROSS1(INODE, ILANE) .NE. 0) THEN
          IV = FCROSS1(INODE, ILANE)
          CALL NEW_STREET_VEHICLE(IV)
          FCROSS1(INODE, ILANE) = 0
          IF(FCROSS2(INODE, ILANE) .NE. 0) THEN
            IV = FCROSS2(INODE, ILANE)
            CALL NEW_STREET_VEHICLE(IV)
            FCROSS2(INODE, ILANE) = 0
          ENDIF
        ENDIF
      ENDDO
      DO ILANE = 1, N_INTERFACE_LANES
        IF(SCROSS1(INODE, ILANE) .NE. 0) THEN
          IV = SCROSS1(INODE, ILANE)
          CALL NEW_FREEWAY_VEHICLE(IV)
          SCROSS1(INODE, ILANE) = 0
          IF(SCROSS2(INODE, ILANE) .NE. 0) THEN
            IV = SCROSS2(INODE, ILANE)
            CALL NEW_FREEWAY_VEHICLE(IV)
            SCROSS2(INODE, ILANE) = 0
          ENDIF
        ENDIF
      ENDDO
    ENDDO
  ENDIF

! --- Accumulate control delay.
 
  IF(.NOT. INITMODE .AND. N_STREET_LINKS .GT. 0) CALL CONTROL_DELAY
 
! --- Update signal states.
 
  CALL UPDATE_SIGNALS
    
! --- Write animation data if requested.
 
#ifdef TSIS_COMPATIBLE
  IF(.NOT. INITMODE .AND. TSTEPFILE .EQ. 1 .AND. MOD(SIMTIME, 1.0) .LT. TIMESTEP/2) THEN
    IF(TSTEPFILE .GT. 0) THEN
      CALL WRITE_ANIMATION_DATA
      CALL SENDEVENTDATA
    ENDIF
    CALL SIMSTATUS(SIM_TIMESTEPCOMPLETE)
    CALL SIMCALLRTE(RT_TIMESTEPCOMPLETE)
  ENDIF
#endif
    
  NSTEPS = NSTEPS + 1
  SIMTIME = NSTEPS * TIMESTEP
  IF(MOD(SIMTIME, FLOAT(TIME_INTERVAL)) .LT. TIMESTEP/2) THEN
 
! --- Send progress reports to the screen.
 
    CALL CPU_TIME(CPUSEC)
    IF(INITMODE) THEN
      WRITE(MSGTEXT, '(A,I8,A,F10.3,A)') '...COMPLETED ', INT(SIMTIME), ' SECONDS OF INITIALIZATION ', &
      CPUSEC - PRIOR_CPU, ' CPU SECONDS'
    ELSE
      WRITE(MSGTEXT, '(A,I8,A,F10.3,A)') '...COMPLETED ', INT(SIMTIME), ' SECONDS OF SIMULATION     ', &
      CPUSEC - PRIOR_CPU, ' CPU SECONDS' 
#ifdef TSIS_COMPATIBLE
      IF(TINTERVALFILE .NE. 0) CALL SENDTIMEINTERVALDATA
      CALL SIMSTATUS(SIM_INTERVALCOMPLETE)
      CALL SIMCALLRTE(RT_INTERVALCOMPLETE)
#endif
    ENDIF
    CALL SENDTEXTMSG(M_INFO)
    PRIOR_CPU = CPUSEC
  ENDIF
  IF(.NOT. INITMODE) THEN
#ifdef TSIS_COMPATIBLE
    CALL SIMSTATUS(SIM_BEGINTIMESTEP)
#endif
    IF(SIMTIME - TSTART .GE. TPSECONDS(TIME_PERIOD)) THEN
      TSTART = SIMTIME

! --- The current time period is finished.

#ifdef TSIS_COMPATIBLE
      CALL SIMSTATUS(SIM_TIMEPERIODCOMPLETE)
      CALL SIMCALLRTE(RT_TIMEPERIODCOMPLETE)
#endif
      WRITE(MSGTEXT, '(A,I2)') 'COMPLETED TIME PERIOD:', TIME_PERIOD
      CALL SENDTEXTMSG(M_INFO)
      IF(WRITE_MOES) CALL STORE_ALL_MOES
      TIME_PERIOD = TIME_PERIOD + 1
      IF(SIMTIME .LT. SIMULATION_END) THEN

! --- Get inputs for a subsequent time period.

        IF(TYPE_OF_RUN .EQ. 1 .OR. TYPE_OF_RUN .EQ. -1) THEN
          MSGTEXT = ''
          CALL SENDTEXTMSG(M_INFO)
          WRITE(MSGTEXT, '(A, I2)') 'CHECKING FOR INPUT ERRORS FOR TIME PERIOD ', TIME_PERIOD
          CALL SENDTEXTMSG(M_INFO)
          CALL CHECK_INPUTS
          IF(ERROR_COUNT .EQ. 0) THEN
            WRITE(MSGTEXT, '(A)') '  NO ERRORS WERE FOUND'
            CALL SENDTEXTMSG(M_INFO)
          ELSEIF(ERROR_COUNT .EQ. 1) THEN
            MSGTEXT = ''
            CALL SENDTEXTMSG(M_INFO)
            WRITE(MSGTEXT, '(A, I4, A)') '**** FOUND 1 ERROR ****'
            CALL SENDTEXTMSG(M_ERROR)
          ELSE
            MSGTEXT = ''
            CALL SENDTEXTMSG(M_INFO)
            WRITE(MSGTEXT, '(A, I4, A)') '**** FOUND ', ERROR_COUNT, ' ERROR(S) ****'
            CALL SENDTEXTMSG(M_ERROR)
          ENDIF
          MSGTEXT = ''
          CALL SENDTEXTMSG(M_INFO)
        ENDIF
        IF(READING_TRF) CALL GET_NETWORK_DATA
      ENDIF
    ENDIF
  ENDIF
  END
