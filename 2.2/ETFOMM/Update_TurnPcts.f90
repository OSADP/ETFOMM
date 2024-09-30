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
  SUBROUTINE UPDATE_TURNPCTS
  USE FREEWAY_LINKS
  USE STREET_LINKS
  USE TURNDATA_MOD
  USE SIMPARAMS
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER :: IL, TIME, IFT, T1, T2, INDEX, IND, I, NPCT(5)
  INTEGER :: IUP, IDN
  REAL :: RPCT(5), RF1, RF2  
! ----------------------------------------------------------------------
 
! --- Get the current turn percentages and exit percentages.
 
  TIME = SIMTIME / 60.
  IF(W26) THEN
    DO IL = 1, N_FREEWAY_LINKS
      IUP = FUSN(IL)
      IDN = FDSN(IL)
      DO IFT = 1, NTURNDATA
        IF(TURNDATA(IFT)%UP .EQ. IUP .AND. TURNDATA(IFT)%DOWN .EQ. IDN) THEN
          IF(INITMODE) THEN                   
            RPCT(1) = TURNDATA(IFT)%RPCT(1,1) 
          ELSE                             
            INDEX = TURNDATA(IFT)%INDEX
            IF(TIME .GE. TURNDATA(IFT)%TIME(INDEX)) THEN
              RPCT(1) = TURNDATA(IFT)%RPCT(1,INDEX)
            ELSE
              DO IND = 1, INDEX - 1
                IF(TIME .GE. TURNDATA(IFT)%TIME(IND) .AND. TIME .LT. TURNDATA(IFT)%TIME(IND+1)) THEN
                  T1 = TURNDATA(IFT)%TIME(IND)
                  T2 = TURNDATA(IFT)%TIME(IND+1)
                  RF1 = TURNDATA(IFT)%RPCT(1,IND)
                  RF2 = TURNDATA(IFT)%RPCT(1,IND+1)
                  RPCT(1) = RF1 + (RF2 - RF1) * FLOAT((TIME - T1)/(T2 - T1))
                  EXIT
                ENDIF
              ENDDO
            ENDIF
          ENDIF
        ENDIF
 
! --- Store the percentage of traffic which proceeds along the thru
! --- movement.
 
        FTHRU_PERCENT(IL) = 100 * RPCT(1)
      ENDDO
    ENDDO
  ENDIF
  IF(W23) THEN
    DO IL = 1, N_STREET_LINKS
      IUP = SUSN(IL)
      IDN = SDSN(IL)
      DO IFT = 1, NTURNDATA
        IF(TURNDATA(IFT)%UP .EQ. IUP .AND. TURNDATA(IFT)%DOWN .EQ. IDN) THEN
          IF(INITMODE) THEN                   
            RPCT(1) = TURNDATA(IFT)%RPCT(1,1) 
            RPCT(2) = TURNDATA(IFT)%RPCT(2,1) 
            RPCT(3) = TURNDATA(IFT)%RPCT(3,1) 
            RPCT(4) = TURNDATA(IFT)%RPCT(4,1) 
            RPCT(5) = TURNDATA(IFT)%RPCT(5,1) 
          ELSE                             
            INDEX = TURNDATA(IFT)%INDEX
            IF(TIME .GE. TURNDATA(IFT)%TIME(INDEX)) THEN
              RPCT(1) = TURNDATA(IFT)%RPCT(1,INDEX)
              RPCT(2) = TURNDATA(IFT)%RPCT(2,INDEX)
              RPCT(3) = TURNDATA(IFT)%RPCT(3,INDEX)
              RPCT(4) = TURNDATA(IFT)%RPCT(4,INDEX)
              RPCT(5) = TURNDATA(IFT)%RPCT(5,INDEX)
            ELSE
              DO IND = 1, INDEX - 1
                IF(TIME .GE. TURNDATA(IFT)%TIME(IND) .AND. TIME .LT. TURNDATA(IFT)%TIME(IND+1)) THEN
                  T1 = TURNDATA(IFT)%TIME(IND)
                  T2 = TURNDATA(IFT)%TIME(IND+1)
                  DO I = 1, 4
                    RF1 = TURNDATA(IFT)%RPCT(I,IND)
                    RF2 = TURNDATA(IFT)%RPCT(I,IND+1)
                    RPCT(I) = RF1 + (RF2 - RF1) * FLOAT((TIME - T1)/(T2 - T1))
                  ENDDO
                  EXIT
                ELSE
                ENDIF
              ENDDO
            ENDIF
          ENDIF
          DO I = 1, 5
            NPCT(I) = 100 * RPCT(I)
          ENDDO
          CALL ADJPCT(NPCT)                                 
 
! --- Store the percentage of traffic for each turn movement.
 
          LEFT_PERCENT(IL) = NPCT(1)
          STHRU_PERCENT(IL) = NPCT(2)
          RIGHT_PERCENT(IL) = NPCT(3)
          LDIAG_PERCENT(IL) = NPCT(4)
          RDIAG_PERCENT(IL) = NPCT(5)
        ENDIF
      ENDDO
    ENDDO
  ENDIF
  RETURN
  END
