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
  SUBROUTINE FREEWAY_CAR_FOLLOW(IV, ILD, ACCEL)
! ----------------------------------------------------------------------      
! --- Determine which car following model to use.
! ----------------------------------------------------------------------    
  USE SIMPARAMS
  USE FREEWAY_VEHICLES
  USE VEHICLE_TYPES
  USE FREEWAY_LINKS
  USE VDATA
  USE INCIDENTS
  USE TEXT
  USE CAR_FOLLOWING
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER :: IV, ILD, IL
  REAL :: CF_INPUTS(9), ACCEL, RB, LCTIME, DIST
! ----------------------------------------------------------------------    
#ifdef DebugVersion
  integer :: temp
  temp = fid(iv)
#endif
    
! --- If the vehicle or its leader have left the network
! --- do not car follow. This may indicate an error in
! --- removing vehicles from the list.
    
  IF(FLINK(IV) .EQ. 0 .OR. FLINK(ILD) .EQ. 0) RETURN
  IL = FLINK(IV)
     
! --- Determine the distance from the front bumper of the follower
! --- to the front bumper of the leader.

  IL = FLINK(IV)
  IF(IL .EQ. FLINK(ILD)) THEN
    DIST = FLOCATION(ILD) - FLOCATION(IV)
  ELSEIF(NODE_TYPE(FUSN(IL)) .EQ. NT_EXTERN .AND. FTHRU_LINK(IL) .EQ. FLINK(ILD)) THEN
    DIST = FLOCATION(ILD)
  ELSEIF(NODE_TYPE(FUSN(IL)) .EQ. NT_EXTERN) THEN
    RETURN
  ELSEIF(FTHRU_LINK(IL) .EQ. FLINK(ILD) .OR. OFFRAMP_LINK(IL) .EQ. FLINK(ILD)) THEN
    DIST = FLENGTH(IL) - FLOCATION(IV) + FLOCATION(ILD)
  ELSE

! --- If the leader is more than two links ahead it should not affect
! --- the follower.

    RETURN
  ENDIF
  IF(FXCODE(ILD) .EQ. 0) DIST = DIST + FSPEED(ILD)

  IF(FCF_MODEL(IV) .EQ. CFM_PITT) THEN
    CF_INPUTS(1) = FZFOLL(FDRIVERTYPE(IV)) 

  ! --- Adjust car following sensitivity by the multiplier from RT 20
  ! --- at this location in the code, it will also apply to rubbernecking.
    
    IF(NODE_TYPE(FUSN(IL)) .NE. NT_EXTERN) CF_INPUTS(1) = CF_INPUTS(1) * FCFMULT(IL)
    
  ! --- If the vehicle is affected by an incident apply the rubbernecking factor.
    
    IF(INCIDENT_NUM(IV) .GT. 0) THEN
      IF(INCIDENT_CODE(INCIDENT_NUM(IV), FLANE(IV)) .EQ. INC_SLOW) THEN
        RB = INCIDENT_RBNF(INCIDENT_NUM(IV)) / 100.
        CF_INPUTS(1) = CF_INPUTS(1) / (1.0 - RB)
      ENDIF
    ENDIF
    
  ! --- Allow a relaxation period immediately after a lane change         
  ! --- unless the leader is slowing down. This logic will reduce the
  ! --- car following sensitivity factor to 10% of its orignal value
  ! --- during the time when either the follower or the leader are
  ! --- performing a lane change, then gradually increase it back to 
  ! --- the original value over a span of 5 seconds after the lane change
  ! --- is completed.

    LCTIME = -5.0
    IF(IV .NE. 0 .AND. ILD .NE. 0) THEN                        
      LCTIME = MAX(FLC_TIMER(IV), FLC_TIMER(ILD))                    
      IF(LCTIME .GT. -5.0 .AND. FACCELERATION(ILD) .GE. 0.0) THEN     
        IF(LCTIME .GE. 0.) THEN                                 
          CF_INPUTS(1) = CF_INPUTS(1) * 0.1                           
        ELSE                                                    
          CF_INPUTS(1) = CF_INPUTS(1) * ABS(LCTIME) / 5.
        ENDIF                                                   
      ENDIF                                                     
    ENDIF
    
    CF_INPUTS(2) = FSPEED(ILD)
    CF_INPUTS(3) = FSPEED(IV)
    IF(CF_INPUTS(2) .LT. 0.01) CF_INPUTS(2) = 0.0
    IF(CF_INPUTS(3) .LT. 0.01) CF_INPUTS(3) = 0.0
    CF_INPUTS(4) = FPCFSEP
    CF_INPUTS(5) = FVLENGTH(ILD)
    CF_INPUTS(6) = DIST
    CF_INPUTS(7) = MAX_DECEL(FVTYPE(IV), FSPEED(IV), FGRADE(FLINK(IV)))
    CALL CAR_FOLLOW_PITT(CF_INPUTS, ACCEL)
    
  ELSEIF(FCF_MODEL(IV) .EQ. CFM_IDM) THEN
    CF_INPUTS(1) = FZFOLL_IDM(FDRIVERTYPE(IV))    

  ! --- Adjust car following sensitivity by the multiplier from RT 20
  ! --- at this location in the code, it will also apply to rubbernecking.
    
    IF(NODE_TYPE(FUSN(IL)) .NE. NT_EXTERN) CF_INPUTS(1) = CF_INPUTS(1) * FCFMULT(IL)
    
  ! --- If the vehicle is affected by an incident apply the rubbernecking factor.
    
    IF(INCIDENT_NUM(IV) .GT. 0) THEN
      IF(INCIDENT_CODE(INCIDENT_NUM(IV), FLANE(IV)) .EQ. INC_SLOW) THEN
        RB = INCIDENT_RBNF(INCIDENT_NUM(IV)) / 100.
        CF_INPUTS(1) = CF_INPUTS(1) / (1.0 - RB)
      ENDIF
    ENDIF
    
  ! --- Allow a relaxation period immediately after a lane change         
  ! --- unless the leader is slowing down. This logic will reduce the
  ! --- car following sensitivity factor to 10% of its orignal value
  ! --- during the time when either the follower or the leader are
  ! --- performing a lane change, then gradually increase it back to 
  ! --- the original value over a span of 5 seconds after the lane change
  ! --- is completed.

    LCTIME = -5.0
    IF(IV .NE. 0 .AND. ILD .NE. 0) THEN                        
      LCTIME = MAX(FLC_TIMER(IV), FLC_TIMER(ILD))                    
      IF(LCTIME .GT. -5.0 .AND. FACCELERATION(ILD) .GE. 0.0) THEN     
        IF(LCTIME .GE. 0.) THEN                                 
          CF_INPUTS(1) = CF_INPUTS(1) * 0.1                           
        ELSE                                                    
          CF_INPUTS(1) = CF_INPUTS(1) * ABS(LCTIME) / 5.
        ENDIF                                                   
      ENDIF                                                     
    ENDIF
    
    CF_INPUTS(2) = FSPEED(IV)
    CF_INPUTS(3) = FSPEED(ILD)
    CF_INPUTS(4) = DIST
    CF_INPUTS(5) = FVLENGTH(ILD)
    CF_INPUTS(6) = MAX(FFREEFLOWSPEED(IL), FDESIREDSPEED(IV))
    CF_INPUTS(7) = FFLEET(IV)
    CF_INPUTS(8) = MAX_ACCEL(FVTYPE(IV), FSPEED(IV), FGRADE(FLINK(IV)))
    CF_INPUTS(9) = FIDMSEP
    CALL CAR_FOLLOW_IDM(CF_INPUTS, ACCEL)
    
  ELSEIF(FCF_MODEL(IV) .EQ. CFM_ACC) THEN
    CF_INPUTS(1) = DIST - FVLENGTH(ILD) 
    CF_INPUTS(2) = ACC_TG
    CF_INPUTS(3) = FSPEED(ILD)
    CF_INPUTS(4) = FSPEED(IV)
    CF_INPUTS(5) = ACC_AMAX
    CF_INPUTS(6) = ACC_DMAX
    CF_INPUTS(7) = ACC_K1
    CF_INPUTS(8) = ACC_K2
    CALL CAR_FOLLOW_ACC(CF_INPUTS, ACCEL)
    
  ELSEIF(FCF_MODEL(IV) .EQ. CFM_CACC) THEN
    CF_INPUTS(1) = DIST - FVLENGTH(ILD) 
    CF_INPUTS(2) = CACC_TG
    CF_INPUTS(3) = FSPEED(ILD)
    CF_INPUTS(4) = FSPEED(IV)
    CF_INPUTS(5) = CACC_AMAX
    CF_INPUTS(6) = CACC_DMAX
    CF_INPUTS(7) = CACC_K1
    CF_INPUTS(8) = CACC_K2
    CF_INPUTS(9) = FACCELERATION(ILD)
    CALL CAR_FOLLOW_CACC(CF_INPUTS, ACCEL)
    
  ELSE
    WRITE(MSGTEXT, '(A,I6)') 'NO CAR FOLLOWING MODEL WAS DEFINED FOR VEHICLE ', FID(IV)
    CALL SENDTEXTMSG(M_ERROR)
    EXITFLG = 1
  ENDIF
  
! --- Prevent the follower from hitting the leader.
  CALL FREEWAY_AVOID_COLLISION(IV, ILD, ACCEL)
  RETURN
  END
  
! ==========================================================================================================
  SUBROUTINE CAR_FOLLOW_PITT(CF_INPUTS, ACCEL)
! ----------------------------------------------------------------------      
! --- Compute car-following deceleration using the PITT model.
! ----------------------------------------------------------------------    
  USE GLOBAL_DATA
  USE CAR_FOLLOWING
  USE SIMPARAMS
  IMPLICIT NONE
  REAL, INTENT(IN) :: CF_INPUTS(N_STREET_LANES)
  REAL, INTENT(INOUT) :: ACCEL
  INTEGER :: IL
  REAL :: ZCO, RDEN, RNUM, RDGAP
  REAL :: Z1, Z2, Z3, DIST, Z6, Z7, Z8
  REAL :: RPGAP, RATIO, ZACC, RMXAC, PITT_SEP
! ----------------------------------------------------------------------
!     iv   follower vehicle ID
!     ild  leader vehicle ID
!     z1   follower vehicle driver characteristic.
!     z2   lead vehicle speed in fps. 
!     z3   follower vehicle speed in fps. 
!     z6   processing time duration in sec. 
!     z7   lead vehicle car length in ft. 
!     z8   follower vehicle acceleration in fpss. (output)
!
  Z1 = CF_INPUTS(1)                
  Z2 = CF_INPUTS(2)                
  Z3 = CF_INPUTS(3)                
  PITT_SEP = CF_INPUTS(4)
  Z7 = CF_INPUTS(5)                
  DIST = CF_INPUTS(6)
  RMXAC = CF_INPUTS(7)
  Z6 = TIMESTEP                
                                                                                                                                          
  IF(Z2 .LE. Z3) THEN

! --- The follower is faster.

    ZCO = 0.3                                                       
  ELSE

! --- The leader is faster.

    ZCO = -0.3                                                      
  ENDIF
                                                         
  RNUM = DIST - Z7 - PITT_SEP - Z3 * (Z1 + Z6) - ZCO * (Z2 - Z3)**2
  RDEN = Z6**2 + 2 * Z1 * Z6
  Z8 = 2 * RNUM / RDEN
  IF(Z8 .LT. 0) THEN
                                                                       
! --- Calculate the desired gap, RDGAP.                                       
                                                                       
    RDGAP = PITT_SEP + Z3 * Z1 + ABS(ZCO) * (Z2 - Z3)**2
                                                                       
! --- Calculate the projected gap, RPGAP.                                     
                                                                       
    RPGAP = DIST - Z7 - Z3 * Z6
                                                                       
! --- Calculate the ratio of the projected gap to the desired gap.            
                                                                       
    RATIO = RPGAP / RDGAP
    RATIO = MAX(RATIO, 0.0)
    IF(RATIO .GE. 1.0 .AND. RPGAP .GT. 0) THEN 

! --- The vehicle may accelerate. Find its current maximum acceleration.
                                        
      ZACC = (1.0 - (1.0 / RATIO**2)) * RMXAC
      IF(Z8 .GE. 0) THEN                                     
        Z8 = MIN(Z8, ZACC)                            
      ELSE                                                          
        Z8 = ZACC                                            
      ENDIF                                                         
    ELSE

! --- The vehicle must decelerate.
                                     
      ZACC = (1. - RATIO) * MAX(Z8, RMXAC)
      Z8 = MAX(Z8, ZACC)
    ENDIF                                                           
  ENDIF

  ACCEL = MIN(ACCEL, Z8)

  RETURN
  END

! ==========================================================================================================
  SUBROUTINE FREEWAY_AVOID_COLLISION(IV, ILD, ACCEL)
! ----------------------------------------------------------------------      
! --- Compute emergency deceleration.
! ----------------------------------------------------------------------      
  USE FREEWAY_VEHICLES
  USE SIMPARAMS
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, ILD
  REAL, INTENT(INOUT) :: ACCEL
  REAL :: DIST, ISPEED, DECEL
! ----------------------------------------------------------------------

! --- Calculate the distance from the follower's front bumper to the leader's
! --- back bumper after applying the computed acceleration for the follower.
! --- Do not allow the follower to drive through the leader.

  DIST = 100.
  ISPEED = FSPEED(IV) + ACCEL * TIMESTEP
  IF(FLINK(IV) .EQ. FLINK(ILD)) THEN
    DIST = FLOCATION(ILD) - FVLENGTH(ILD) - FLOCATION(IV) - TIMESTEP * ISPEED 
  ELSEIF(FTHRU_LINK(FLINK(IV)) .EQ. FLINK(ILD)) THEN
    DIST = FLENGTH(FLINK(IV)) - FLOCATION(IV) + FLOCATION(ILD) - FVLENGTH(ILD) - TIMESTEP * ISPEED 
  ELSEIF(OFFRAMP_LINK(FLINK(IV)) .EQ. FLINK(ILD)) THEN
    DIST = FLENGTH(FLINK(IV)) - FLOCATION(IV) + FLOCATION(ILD) - FVLENGTH(ILD) - TIMESTEP * ISPEED 
  ENDIF
  IF(FXCODE(ILD) .EQ. 0) DIST = DIST + FSPEED(ILD)
  IF(DIST .LE. 0) THEN
    ISPEED = FSPEED(IV) - FSPEED(ILD)
    IF(ISPEED .GT. 0.) THEN
      DIST = DIST + TIMESTEP * ISPEED
      IF(DIST .GT. 0.) THEN
        DECEL = -ISPEED ** 2 / (2 * DIST)
        ACCEL = ACCEL + DECEL
      ELSE
        ACCEL = -FSPEED(IV) / TIMESTEP
      ENDIF
    ENDIF
  ENDIF
  RETURN
  END

! ==========================================================================================================
  SUBROUTINE STREET_CAR_FOLLOW(IV, ILD, ACCEL)
! ----------------------------------------------------------------------      
! --- Determine which car following model to use.
! ----------------------------------------------------------------------    
  USE STREET_VEHICLES
  USE STREET_LINKS
  USE CAR_FOLLOWING
  USE VDATA
  USE SIMPARAMS
  USE TEXT
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER :: IV, ILD, IL
  REAL :: ACCEL, CF_INPUTS(9), DIST
! ----------------------------------------------------------------------    
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif
! --- If the vehicle or its leader have left the network
! --- do not car follow. This may indicate an error in
! --- removing vehicles from the list.
    
  IF(SLINK(IV) .EQ. 0 .OR. SLINK(ILD) .EQ. 0) RETURN

! --- Determine the distance from the front bumper of the follower
! --- to the front bumper of the leader.

  IL = SLINK(IV)
  IF(IL .EQ. SLINK(ILD)) THEN
    IF(ARC_LOCATION(IV) .NE. 0) THEN
      IF(ARC_LOCATION(ILD) .NE. 0) THEN
        !Both vehicles are following a turn arc
        DIST = ARC_LOCATION(ILD) - ARC_LOCATION(IV) 
      ELSE
        !The follower is on an arc and the leader is not
        DIST = (SLOCATION(ILD) - UP_INT_WIDTH(IL)) + (ARC_LENGTH(IV) - ARC_LOCATION(IV)) 
      ENDIF
    ELSE
      DIST = SLOCATION(ILD) - SLOCATION(IV)
    ENDIF
  ELSEIF(NODE_TYPE(SUSN(IL)) .EQ. NT_EXTERN .AND. STHRU_LINK(IL) .EQ. SLINK(ILD)) THEN
    DIST = SLOCATION(ILD)
  ELSEIF(NODE_TYPE(SUSN(IL)) .EQ. NT_EXTERN) THEN
    RETURN
  ELSEIF(STHRU_LINK(IL) .EQ. SLINK(ILD)) THEN
    DIST = SLENGTH(IL) - SLOCATION(IV) + SLOCATION(ILD)

! --- These distance calculations need to be corrected
! --- to consider the geometry of the intersection.

  ELSEIF(LEFT_LINK(IL) .EQ. SLINK(ILD)) THEN
    IF(ARC_LOCATION(ILD) .NE. 0) THEN
      DIST = SLENGTH(IL) - SLOCATION(IV) + ARC_LOCATION(ILD)
    ELSEIF(LANE_CENTER(IL, SLANE(IV)) .GT. 0) THEN
      DIST = SLENGTH(IL) - SLOCATION(IV) + SLOCATION(ILD) - LANE_CENTER(IL, SLANE(IV))
    ELSE
      DIST = SLENGTH(IL) - SLOCATION(IV) + SLOCATION(ILD) - SLANE(IV) * 12
    ENDIF
  ELSEIF(RIGHT_LINK(IL) .EQ. SLINK(ILD)) THEN
    IF(ARC_LOCATION(ILD) .NE. 0) THEN
      DIST = SLENGTH(IL) - SLOCATION(IV) + ARC_LOCATION(ILD)
    ELSEIF(LANE_CENTER(IL, SLANE(IV)) .GT. 0) THEN
      DIST = SLENGTH(IL) - SLOCATION(IV) + SLOCATION(ILD) - LANE_CENTER(IL, SLANE(IV))
    ELSE
      DIST = SLENGTH(IL) - SLOCATION(IV) + SLOCATION(ILD) - UP_INT_WIDTH(SLINK(ILD)) + SLANE(IV) * 12
    ENDIF
  ELSEIF(LEFT_DIAG_LINK(IL) .EQ. SLINK(ILD)) THEN
    DIST = SLENGTH(IL) - SLOCATION(IV) + SLOCATION(ILD) 
  ELSEIF(RIGHT_DIAG_LINK(IL) .EQ. SLINK(ILD)) THEN
    DIST = SLENGTH(IL) - SLOCATION(IV) + SLOCATION(ILD) 
  ELSEIF(SLINK(ILD) .EQ. CHECK_MERGE(IL)) THEN
    DIST = SLENGTH(IL) - SLOCATION(IV) - (SLENGTH(SLINK(ILD)) - SLOCATION(ILD))
    
! --- Look two links downstream.
    
  ELSEIF(STHRU_LINK(IL) .NE. 0) THEN
    IF(STHRU_LINK(STHRU_LINK(IL)) .EQ. SLINK(ILD)) THEN
      DIST = SLENGTH(IL) - SLOCATION(IV) + SLOCATION(ILD) + SLENGTH(STHRU_LINK(IL))
    ELSEIF(LEFT_LINK(STHRU_LINK(IL)) .EQ. SLINK(ILD)) THEN
      DIST = SLENGTH(IL) - SLOCATION(IV) + SLOCATION(ILD) + SLENGTH(STHRU_LINK(IL))
    ELSEIF(RIGHT_LINK(STHRU_LINK(IL)) .EQ. SLINK(ILD)) THEN
      DIST = SLENGTH(IL) - SLOCATION(IV) + SLOCATION(ILD) + SLENGTH(STHRU_LINK(IL))
    ELSEIF(LEFT_DIAG_LINK(STHRU_LINK(IL)) .EQ. SLINK(ILD)) THEN
      DIST = SLENGTH(IL) - SLOCATION(IV) + SLOCATION(ILD) + SLENGTH(STHRU_LINK(IL))
    ELSEIF(RIGHT_DIAG_LINK(STHRU_LINK(IL)) .EQ. SLINK(ILD)) THEN
      DIST = SLENGTH(IL) - SLOCATION(IV) + SLOCATION(ILD) + SLENGTH(STHRU_LINK(IL))
    ENDIF
  ELSE

! --- If the leader is more than two links ahead it should not affect
! --- the follower. The leader may also be on the right or left 
! --- receiving link.

    RETURN
  ENDIF
  IF(SXCODE(ILD) .EQ. 0) DIST = DIST + SSPEED(ILD)

  CF_INPUTS(1) = SZFOLL(SDRIVERTYPE(IV))
  IF(NODE_TYPE(SUSN(IL)) .NE. NT_EXTERN) CF_INPUTS(1) = CF_INPUTS(1) * SCFMULT(IL)
     
  IF(SCF_MODEL(IV) .EQ. CFM_PITT) THEN
    CF_INPUTS(1) = SZFOLL(SDRIVERTYPE(IV))    
    CF_INPUTS(2) = SSPEED(ILD)
    CF_INPUTS(3) = SSPEED(IV)
    IF(CF_INPUTS(2) .LT. 0.01) CF_INPUTS(2) = 0.0
    IF(CF_INPUTS(3) .LT. 0.01) CF_INPUTS(3) = 0.0
    CF_INPUTS(4) = SPCFSEP
    CF_INPUTS(5) = SVLENGTH(ILD)
    CF_INPUTS(6) = DIST
    CF_INPUTS(7) = MAX_DECEL(SVTYPE(IV), SSPEED(IV), SGRADE(SLINK(IV)))
    CALL CAR_FOLLOW_PITT(CF_INPUTS, ACCEL)
  
  ELSEIF(SCF_MODEL(IV) .EQ. CFM_IDM) THEN
    CF_INPUTS(1) = SZFOLL_IDM(SDRIVERTYPE(IV))    
    CF_INPUTS(2) = SSPEED(IV)
    CF_INPUTS(3) = SSPEED(ILD)
    CF_INPUTS(4) = DIST
    CF_INPUTS(5) = SVLENGTH(ILD)
    CF_INPUTS(6) = MAX(SFREEFLOWSPEED(IL), SDESIREDSPEED(IV))
    CF_INPUTS(7) = SFLEET(IV)
    CF_INPUTS(8) = MAX_ACCEL(SVTYPE(IV), SSPEED(IV), SGRADE(SLINK(IV)))
    CF_INPUTS(9) = SIDMSEP
    CALL CAR_FOLLOW_IDM(CF_INPUTS, ACCEL)
    
    
  ELSEIF(SCF_MODEL(IV) .EQ. CFM_ACC) THEN
    CF_INPUTS(1) = DIST - SVLENGTH(ILD) 
    CF_INPUTS(2) = ACC_TG
    CF_INPUTS(3) = SSPEED(ILD)
    CF_INPUTS(4) = SSPEED(IV)
    CF_INPUTS(5) = ACC_AMAX
    CF_INPUTS(6) = ACC_DMAX
    CF_INPUTS(7) = ACC_K1
    CF_INPUTS(8) = ACC_K2
    CALL CAR_FOLLOW_ACC(CF_INPUTS, ACCEL)
  
  ELSEIF(SCF_MODEL(IV) .EQ. CFM_CACC) THEN
    CF_INPUTS(1) = DIST - SVLENGTH(ILD) 
    CF_INPUTS(2) = CACC_TG
    CF_INPUTS(3) = SSPEED(ILD)
    CF_INPUTS(4) = SSPEED(IV)
    CF_INPUTS(5) = CACC_AMAX
    CF_INPUTS(6) = CACC_DMAX
    CF_INPUTS(7) = CACC_K1
    CF_INPUTS(8) = CACC_K2
    CF_INPUTS(9) = SACCELERATION(ILD)
    CALL CAR_FOLLOW_CACC(CF_INPUTS, ACCEL)
  
  ELSE
    WRITE(MSGTEXT, '(A,I6)') 'NO CAR FOLLOWING MODEL WAS DEFINED FOR VEHICLE ', SID(IV)
    CALL SENDTEXTMSG(M_ERROR)
    EXITFLG = 1
  ENDIF
! --- Prevent the follower from hitting the leader.
  CALL STREET_AVOID_COLLISION(IV, ILD, ACCEL)
  RETURN
  END

! ==========================================================================================================
  SUBROUTINE STREET_AVOID_COLLISION(IV, ILD, ACCEL)
! ----------------------------------------------------------------------      
! --- Compute emergency deceleration.
! ----------------------------------------------------------------------      
  USE STREET_VEHICLES
  USE SIMPARAMS
  USE STREET_LINKS
  USE CAR_FOLLOWING
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IV, ILD
  REAL, INTENT(INOUT) :: ACCEL
  REAL :: DIST, ISPEED, DECEL
  integer :: v1, v2
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = sid(iv)
#endif

! --- Calculate the distance from the follower's front bumper to the leader's
! --- back bumper after applying the computed acceleration for the follower.
! --- Do not allow the follower to drive through the leader.

  DIST = 100.
  ISPEED = SSPEED(IV) + ACCEL * TIMESTEP
  IF(SLINK(IV) .EQ. SLINK(ILD)) THEN
    IF(ARC_LOCATION(IV) .NE. 0) THEN
      IF(ARC_LOCATION(ILD) .NE. 0) THEN
        !Both vehicles are following a turn arc
        DIST = ARC_LOCATION(ILD) - ARC_LOCATION(IV) 
      ELSE
        !The follower is on an arc and the leader is not
        DIST = (SLOCATION(ILD) - UP_INT_WIDTH(SLINK(IV))) + (ARC_LENGTH(IV) - ARC_LOCATION(IV)) 
      ENDIF
      DIST = DIST - TIMESTEP * ISPEED 
    ELSE
      DIST = SLOCATION(ILD) - SVLENGTH(ILD) - SLOCATION(IV) - TIMESTEP * ISPEED 
    ENDIF
  ELSEIF(STHRU_LINK(SLINK(IV)) .EQ. SLINK(ILD)) THEN
    DIST = SLENGTH(SLINK(IV)) - SLOCATION(IV) + SLOCATION(ILD) - SVLENGTH(ILD) - TIMESTEP * ISPEED 
    IF(SXCODE(ILD) .EQ. 0) DIST = DIST + SSPEED(ILD)
  ENDIF
  IF(SFLEET(ILD) .EQ. FLEET_BIKE) THEN
    IF(SLINK(IV) .EQ. SLINK(ILD) .AND. SLENGTH(SLINK(ILD)) - SLOCATION(ILD) .GT. 50 &
      .AND. SSPEED(ILD) .GT. 0) THEN
      IF(DIST .LT. 10 .AND. SSPEED(IV) .LT. SSPEED(ILD) + 5) THEN
! --- Switch the positions of the follower and the bike 
        v1 = sleader(ild)
        v2 = sfollower(iv)
        SLEADER(IV) = v1
        SFOLLOWER(ILD) = v2
        SFOLLOWER(IV) = ILD
        SLEADER(ILD) = IV
        if(v1 .ne. 0) sfollower(v1) = iv
        if(v2 .ne. 0) sleader(v2) = ild
        IF(SLEADER(IV) .EQ. 0) FIRST_VEHICLE(SLINK(IV), SLANE(IV)) = IV
        IF(SFOLLOWER(ILD) .EQ. 0) SLAST_VEHICLE(SLINK(IV), SLANE(IV)) = ILD
      ENDIF
    ENDIF
    
  ELSEIF(DIST .LE. 0) THEN
    ISPEED = SSPEED(IV) - SSPEED(ILD)
    IF(ISPEED .GT. 0.) THEN
      DIST = DIST + TIMESTEP * ISPEED
      IF(DIST .GT. 0.) THEN
        DECEL = -ISPEED ** 2 / (2 * DIST)
        ACCEL = ACCEL + DECEL
      ELSE
        ACCEL = -SSPEED(IV) / TIMESTEP
      ENDIF
    ELSE
      ACCEL = MIN(ACCEL, 0.)
    ENDIF
    
  ENDIF
  RETURN
  END

! ==========================================================================================================
  SUBROUTINE INTERFACE_CAR_FOLLOW(CF_DATA, ACCEL)
! ----------------------------------------------------------------------      
! --- Compute car-following deceleration for a vehicle crossing
! --- an interface node.
! ----------------------------------------------------------------------      
  USE GLOBAL_DATA
  USE SIMPARAMS
  USE CAR_FOLLOWING
  USE VDATA
  USE VEHICLE_TYPES
  IMPLICIT NONE
  REAL, INTENT(IN) :: CF_DATA(N_STREET_LANES)
  REAL, INTENT(INOUT) :: ACCEL
  REAL :: ZCO, RDEN, RNUM
  REAL :: Z1, Z2, Z3, DIST, Z6, Z8, RDGAP, RPGAP, RATIO, ZACC
! ----------------------------------------------------------------------
!     z1   follower vehicle driver characteristic.
!     z2   lead vehicle speed in fps. 
!     z3   follower vehicle speed in fps. 
!     z6   processing time duration in sec. 
 
  Z1 = CF_DATA(1)
  Z2 = CF_DATA(2)
  Z3 = CF_DATA(3)
  Z6 = TIMESTEP
  DIST = CF_DATA(4)
 
! --- Adjust car following sensitivity by the multiplier from RT 20
! --- at this location in the code, it will also apply to rubbernecking.
 
  Z1 = Z1 * CF_DATA(6)
                                                                       
! --- allow a relaxation period immediately after a lane change         
! --- unless the leader is slowing down                                 
!        (do this after lane changing module has been developed)
                                                                       
  IF(Z2 .LE. Z3) THEN

! --- The follower is faster.

    ZCO = 0.3                                                       
  ELSE

! --- The leader is faster.

    ZCO = -0.3                                                      
  ENDIF
                                                          
  RNUM = DIST - CF_DATA(7) - Z3 * (Z1 + Z6) - ZCO * (Z2 - Z3)**2
  RDEN = Z6**2 + 2 * Z1 * Z6
  Z8 = 2 * RNUM / RDEN
  IF(Z8 .LT. 0) THEN
                                                                       
! --- Calculate the desired gap, RDGAP.                                       
                                                                       
    RDGAP = CF_DATA(7) + Z3 * Z1 + ABS(ZCO) * (Z2 - Z3)**2
                                                                       
! --- Calculate the projected gap, RPGAP.                                     
                                                                       
    RPGAP = DIST - Z3 * Z6
                                                                       
! --- Calculate the ratio of the projected gap to the desired gap.            
                                                                       
    RATIO = RPGAP / RDGAP
    RATIO = MAX(RATIO, 0.0)
    IF(RATIO .GE. 1.0 .AND. RPGAP .GT. 0) THEN 

! --- The vehicle may accelerate. Find its current maximum acceleration.
                                        
      ZACC = (1.0 - (1.0 / RATIO**2)) * CF_DATA(5)
      IF(Z8 .GE. 0) THEN                                     
        Z8 = MIN(Z8, ZACC)                            
      ELSE                                                          
        Z8 = ZACC                                            
      ENDIF                                                         
    ELSE

! --- The vehicle must decelerate.
                                     
      ZACC = (1. - RATIO) * MAX(Z8, CF_DATA(5))
      Z8 = MAX(Z8, ZACC)
    ENDIF 
  ENDIF
  ACCEL = MIN(ACCEL, Z8)
  RETURN
  END

! ==========================================================================================================
  SUBROUTINE CAR_FOLLOW_IDM(CF_INPUTS, ACCEL)
! ----------------------------------------------------------------------      
! --- Compute car-following deceleration using the IDM model.
! ---------------------------------------------------------------------- 
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE SIMPARAMS
  IMPLICIT NONE
  REAL, INTENT(IN) :: CF_INPUTS(9)
  REAL, INTENT(INOUT) :: ACCEL
  REAL :: DIST, IDM_V, NEWSPEED, HEADWAYTIME, VSPEED, LSPEED, DESSPEED, MYACCEL, MINGAP
  INTEGER :: FLEET
  LOGICAL :: RESPECTMINGAP
! ---------------------------------------------------------------------- 
  HEADWAYTIME = CF_INPUTS(1)
  VSPEED = CF_INPUTS(2)
  LSPEED = CF_INPUTS(3)
  DIST = CF_INPUTS(4) - CF_INPUTS(5)
  DESSPEED = CF_INPUTS(6)
  FLEET = CF_INPUTS(7)
  MYACCEL = CF_INPUTS(8)
  MINGAP = CF_INPUTS(9)
  RESPECTMINGAP = .TRUE.
  NEWSPEED = IDM_V(FLEET, HEADWAYTIME, DIST, VSPEED, LSPEED, DESSPEED, RESPECTMINGAP, MYACCEL, MINGAP)
  ACCEL = MIN(ACCEL, (NEWSPEED - VSPEED) / TIMESTEP)
  RETURN
  END
  
  REAL FUNCTION IDM_V(FLEET, HEADWAYTIME, DIST, VSPEED, LSPEED, DESSPEED, RESPECTMINGAP, MYACCEL, MINGAP)
  USE CAR_FOLLOWING
  USE VDATA
  USE SIMPARAMS
  IMPLICIT NONE
  REAL :: DIST, VSPEED, LSPEED, DESSPEED
  LOGICAL :: RESPECTMINGAP
  REAL :: HEADWAYTIME, NEWSPEED, GAP, DELTA_V, S, ACC, MYACCEL, MINGAP
  REAL :: DEFAULTACCEL, DEFAULTDECEL, GETDEFAULTACCEL, GETDEFAULTDECEL
  INTEGER :: FLEET, I, MYITERATIONS = 1
  REAL :: MYDELTA = 4.0
! ---------------------------------------------------------------------- 
  NEWSPEED = VSPEED
  GAP = DIST
  DEFAULTACCEL = GETDEFAULTACCEL(FLEET)
  DEFAULTDECEL = GETDEFAULTDECEL(FLEET)
  DO I = 1, MYITERATIONS
    DELTA_V = NEWSPEED - LSPEED
    S = MAX(0., NEWSPEED * HEADWAYTIME + NEWSPEED * DELTA_V / (2*SQRT(DEFAULTACCEL*DEFAULTDECEL)))
    IF(RESPECTMINGAP) THEN
      S = S + MINGAP
    ENDIF
    ACC = MYACCEL * (1. - ((NEWSPEED / DESSPEED) ** MYDELTA) - (S * S) / (GAP * GAP))
    NEWSPEED = NEWSPEED + (TIMESTEP * ACC / MYITERATIONS)
    GAP = GAP - MAX(0., TIMESTEP * (NEWSPEED - LSPEED) / MYITERATIONS)
  ENDDO
  IDM_V = MAX(0., NEWSPEED)
  END
  
  REAL FUNCTION GETDEFAULTACCEL(FLEET)
  USE VEHICLE_TYPES
  INTEGER :: FLEET
  SELECT CASE(FLEET)
  CASE(FLEET_TRUCK)
    GETDEFAULTACCEL = 1.3
  CASE(FLEET_PED)
    GETDEFAULTACCEL = 1.5
  CASE(FLEET_BUS)
    GETDEFAULTACCEL = 1.2
  CASE DEFAULT
    GETDEFAULTACCEL = 2.6
  END SELECT
  END  
  
  REAL FUNCTION GETDEFAULTDECEL(FLEET)
  USE VEHICLE_TYPES
  INTEGER :: FLEET
  SELECT CASE(FLEET)
  CASE(FLEET_TRUCK)
    GETDEFAULTDECEL = 4.0
  CASE(FLEET_PED)
    GETDEFAULTDECEL = 2.0
  CASE(FLEET_BUS)
    GETDEFAULTDECEL = 4.0
  CASE DEFAULT
    GETDEFAULTDECEL = 4.5
  END SELECT
  END

! ==========================================================================================================
  SUBROUTINE CAR_FOLLOW_ACC(CF_INPUTS, ACCEL)
! ----------------------------------------------------------------------      
! --- Compute car-following deceleration using the ACC model.
! --- Based on the VanderWerf model.
! ---------------------------------------------------------------------- 
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE SIMPARAMS
  IMPLICIT NONE
  REAL, INTENT(IN) :: CF_INPUTS(9)
  REAL, INTENT(INOUT) :: ACCEL
  REAL :: RD, R, TG, V1, V2, AMAX, DMAX, K1, K2, A
! ---------------------------------------------------------------------- 
  R = CF_INPUTS(1)
  TG = CF_INPUTS(2)
  V1 = CF_INPUTS(3)
  V2 = CF_INPUTS(4)
  AMAX = CF_INPUTS(5)
  DMAX = CF_INPUTS(6)
  K1 = CF_INPUTS(7)
  K2 = CF_INPUTS(8)
  RD = TG * V2
  A = K1 * (V1 - V2) + K2 * (R - RD)
  A = MIN(A, AMAX)
  A = MAX(A, DMAX)
  ACCEL = MIN(ACCEL, A)
  RETURN
  END

! ==========================================================================================================
  SUBROUTINE CAR_FOLLOW_CACC(CF_INPUTS, ACCEL)
! ----------------------------------------------------------------------      
! --- Compute car-following deceleration using the CACC model.
! --- Based on the VanderWerf model.
! ---------------------------------------------------------------------- 
  USE FREEWAY_VEHICLES
  USE FREEWAY_LINKS
  USE SIMPARAMS
  IMPLICIT NONE
  REAL, INTENT(IN) :: CF_INPUTS(9)
  REAL, INTENT(INOUT) :: ACCEL
  REAL :: RD, R, TG, A1, V1, V2, AMAX, D2, K1, K2, A
! ---------------------------------------------------------------------- 
  R = CF_INPUTS(1)
  TG = CF_INPUTS(2)
  V1 = CF_INPUTS(3)
  V2 = CF_INPUTS(4)
  AMAX = CF_INPUTS(5)
  D2 = CF_INPUTS(6)
  K1 = CF_INPUTS(7)
  K2 = CF_INPUTS(8)
  A1 = CF_INPUTS(9)
  RD = TG * V2
  A = A1 + K1 * (V1 - V2) + K2 * (R - RD)
  A = MIN(A, AMAX)
  A = MAX(A, D2)
  ACCEL = MIN(ACCEL, A)
  RETURN
  END
  
