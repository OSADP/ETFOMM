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
  SUBROUTINE ORGANIZE_FREEWAY_LINKS
! ----------------------------------------------------------------------
! --- Organize freeway links into segments.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  USE ENTRYNODE_DATA
  USE SEGMENTS
  USE TEXT
  USE SIMPARAMS
  USE FREEWAY_NODES
  USE NODE_TABLE
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER :: IL, NLINKS, ISEG, OFFLINK, MAINLINK, ILX
  INTEGER :: ONLINK, ILNK, ILEN, IRAMP, UPNODE, INODE
  LOGICAL :: STORE_RAMP
  REAL :: TEMP
  INTEGER :: ONRAMP_COUNTER, OFFRAMP_COUNTER
! ----------------------------------------------------------------------
 
! --- Loop over all freeway links and determine the number of segments
! --- in the network. Each segment ends at an exit node or an exit interface node.

  NUMBER_OF_SEGMENTS = 0
  DO ILNK = 1, N_FREEWAY_LINKS
    IF(LINKTYPE(ILNK) .EQ. 0) THEN
      IF(NODE_TYPE(FDSN(ILNK)) .NE. NT_INTERN) THEN
        NUMBER_OF_SEGMENTS = NUMBER_OF_SEGMENTS + 1
      ELSEIF(RAMP_MERGE_LINK(ILNK)) THEN
        NUMBER_OF_SEGMENTS = NUMBER_OF_SEGMENTS + 1
      ELSE
        
        ! --- Create a dummy exit if needed.

        IF(FTHRU_LINK(ILNK) .NE. 0) THEN
          IF(LINKTYPE(FTHRU_LINK(ILNK)) .EQ. 1) THEN
              
            !There is a mainline link that feeds an onramp. 
            !Increment the number of segments and create a dummy exit to prevent
            !an illegal freeway segment.
            
            NUMBER_OF_SEGMENTS = NUMBER_OF_SEGMENTS + 1
            DUMMY_EXIT(FDSN(ILNK)) = .TRUE.
            CALL GET_NEXT_EXTERNAL_NODE_ID(INODE)
            CALL REALLOCATE_FREEWAY_LINK_ARRAYS
            FUSN(N_FREEWAY_LINKS) = FDSN(ILNK)
            FDSN(N_FREEWAY_LINKS) = INODE
            LINKTYPE(N_FREEWAY_LINKS) = 0
            OFFRAMP_LINK(ILNK) = FTHRU_LINK(ILNK)
            FTHRU_LINK(ILNK) = N_FREEWAY_LINKS
            FTHRU_LINK(N_FREEWAY_LINKS) = 0
            FNUMLANES(N_FREEWAY_LINKS) = FNUMLANES(ILNK)
            FFREEFLOWSPEED(N_FREEWAY_LINKS) = FFREEFLOWSPEED(ILNK)
            NETCODE(INODE) = I_FREEWAY
            IS_USED(INODE) = .TRUE.
            SEGMENT(N_FREEWAY_LINKS) = NUMBER_OF_SEGMENTS
            MAINLINE_RECEIVING_LANE(N_FREEWAY_LINKS) = 1
            OFFRAMP_SENDING_LANE(ILNK) = 1
            MAINLINE_SENDING_LANE(N_FREEWAY_LINKS) = 1
            OFFRAMP_RECEIVING_LANE(N_FREEWAY_LINKS) = 1
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDDO
  
! --- Allocate segment arrays.
 
  IF(NUMBER_OF_SEGMENTS .EQ. 0) THEN
    CALL NEW_ERROR
    WRITE(MSGTEXT, '(A)') 'NO VALID SEGMENTS WERE DETECTED IN THE FREEWAY NETWORK'
    CALL SENDTEXTMSG(M_ERROR)
    WRITE(MSGTEXT, '(A,A)') 'A valid segment has continuous mainline links from an entry or entry interface node',&
       ' to an exit or exit interface node.'
    CALL SENDTEXTMSG(M_ERROR)
    WRITE(MSGTEXT, '(A)') ''
    CALL SENDTEXTMSG(M_ERROR)
  ELSE
    CALL ALLOCATE_SEGMENT_ARRAYS
    NUMBER_OF_LINKS_IN_SEGMENT = 0
    LINKS_IN_SEGMENT = 0
    ISEG = 0
 
  ! --- Assign links to segments.
 
    DO ILNK = 1, N_FREEWAY_LINKS
      IL = ILNK
      IF((NODE_TYPE(FDSN(IL)) .NE. NT_INTERN .AND. LINKTYPE(IL) .EQ. 0) .OR. RAMP_MERGE_LINK(IL)) THEN
        ISEG = ISEG + 1
        ONRAMP_COUNTER = 0
        OFFRAMP_COUNTER = 0
        SEGMENT(IL) = ISEG
        NLINKS = 1
        LINKS_IN_SEGMENT(ISEG, NLINKS) = IL
        NUMBER_OF_LINKS_IN_SEGMENT(ISEG) = NLINKS
        DO

  ! --- Continue the search until reaching an entry or entry interface node.
 
          IF(NODE_TYPE(FUSN(IL)) .NE. NT_INTERN) EXIT
          IF(DUMMY_ENTRY(FUSN(IL))) EXIT
          IF(DIVERGE_LINK(IL)) EXIT
          
  ! --- Determine if there is an off-ramp connected to the upstream node.
 
          MAINLINK = MAINLINE_APPROACH(FUSN(IL))
          IF(MAINLINK .NE. 0) THEN
            OFFLINK = OFFRAMP_LINK(MAINLINK)
          ELSE
            OFFLINK = 0
          ENDIF
          ILX = OFFLINK

  ! --- If the ramp goes to an exit or exit interface node it is part of the current segment.
  ! --- If it connects to a different segment it does not belong to this segment.

          STORE_RAMP = .FALSE.
          DO WHILE(ILX .NE. 0)
            IF(NODE_TYPE(FDSN(ILX)) .NE. NT_INTERN .OR. FTHRU_LINK(ILX) .EQ. 0) THEN
              STORE_RAMP = .TRUE.
              EXIT
            ELSE
              ILX = FTHRU_LINK(ILX)
              IF(LINKTYPE(ILX) .EQ. 0) EXIT 
            ENDIF
          ENDDO

          IF(STORE_RAMP) THEN

  ! --- Add the ramp link to the current segment.

            SEGMENT(OFFLINK) = ISEG
            NLINKS = NLINKS + 1
            IF(NLINKS .GT. MAXLINKSPERSEGMENT) CALL REALLOCATE_SEGLINKS
            LINKS_IN_SEGMENT(ISEG, NLINKS) = OFFLINK
            NUMBER_OF_LINKS_IN_SEGMENT(ISEG) = NLINKS
            OFFRAMP_COUNTER = OFFRAMP_COUNTER - 1
            LINKTYPE(OFFLINK) = OFFRAMP_COUNTER

  ! --- Add the downstream ramp links, if there are any.

            DO WHILE(NODE_TYPE(FDSN(OFFLINK)) .EQ. NT_INTERN) 
              OFFLINK = FTHRU_LINK(OFFLINK)
              SEGMENT(OFFLINK) = ISEG
              NLINKS = NLINKS + 1
              IF(NLINKS .GT. MAXLINKSPERSEGMENT) CALL REALLOCATE_SEGLINKS
              LINKS_IN_SEGMENT(ISEG, NLINKS) = OFFLINK
              NUMBER_OF_LINKS_IN_SEGMENT(ISEG) = NLINKS
              LINKTYPE(OFFLINK) = OFFRAMP_COUNTER
            ENDDO
          ELSE
 
  ! --- If there is an on-ramp approach to the node add the link to the segment.
 
            ONLINK = RAMP_APPROACH(FUSN(IL))
            IF(ONLINK .GT. 0) THEN

  ! --- Add the upstream ramp links, if there are any.

              ONRAMP_COUNTER = ONRAMP_COUNTER + 1
              DO WHILE(ONLINK .NE. 0)
                SEGMENT(ONLINK) = ISEG
                NLINKS = NLINKS + 1
                IF(NLINKS .GT. MAXLINKSPERSEGMENT) CALL REALLOCATE_SEGLINKS
                LINKS_IN_SEGMENT(ISEG, NLINKS) = ONLINK
                NUMBER_OF_LINKS_IN_SEGMENT(ISEG) = NLINKS
                LINKTYPE(ONLINK) = ONRAMP_COUNTER
                UPNODE = FUSN(ONLINK)
                IF(NODE_TYPE(UPNODE) .NE. NT_INTERN) EXIT
                IF(MAINLINE_APPROACH(UPNODE) .GT. 0) THEN
                  IF(OFFRAMP_LINK(MAINLINE_APPROACH(UPNODE)) .EQ. IL) EXIT
                  IF(DUMMY_EXIT(UPNODE)) EXIT
                ENDIF
                ONLINK = RAMP_APPROACH(UPNODE)
              ENDDO
            ENDIF
          ENDIF
 
  ! --- Load the mainline approach to the node.
 
          IF(MAINLINK .NE. 0) THEN
            IL = MAINLINK
            SEGMENT(IL) = ISEG
            NLINKS = NLINKS + 1
            IF(NLINKS .GT. MAXLINKSPERSEGMENT) CALL REALLOCATE_SEGLINKS
            LINKS_IN_SEGMENT(ISEG, NLINKS) = IL
            NUMBER_OF_LINKS_IN_SEGMENT(ISEG) = NLINKS
          ELSE
          
            !If there is no mainline approach to the node create a dummy entry link
            !to avoid an illegal freeway segment.
          
            CALL GET_NEXT_EXTERNAL_NODE_ID(INODE)
            CALL REALLOCATE_FREEWAY_LINK_ARRAYS
            FUSN(N_FREEWAY_LINKS) = INODE
            FDSN(N_FREEWAY_LINKS) = FUSN(IL)
            LINKTYPE(N_FREEWAY_LINKS) = 0
            FTHRU_LINK(N_FREEWAY_LINKS) = IL
            FNUMLANES(N_FREEWAY_LINKS) = FNUMLANES(IL)
            MAINLINE_RECEIVING_LANE(N_FREEWAY_LINKS) = 1
            MAINLINE_SENDING_LANE(IL) = 1
            FFREEFLOWSPEED(N_FREEWAY_LINKS) = FFREEFLOWSPEED(IL)
            IF(LINKTYPE(N_FREEWAY_LINKS) .EQ. 0) THEN
              MAINLINE_APPROACH(FUSN(IL)) = N_FREEWAY_LINKS
            ELSE
              RAMP_APPROACH(FUSN(IL)) = N_FREEWAY_LINKS
            ENDIF
            DUMMY_ENTRY(FUSN(IL)) = .TRUE.
            NLINKS = NLINKS + 1
            IF(NLINKS .GT. MAXLINKSPERSEGMENT) CALL REALLOCATE_SEGLINKS
            LINKS_IN_SEGMENT(ISEG, NLINKS) = N_FREEWAY_LINKS
            NUMBER_OF_LINKS_IN_SEGMENT(ISEG) = NLINKS
            SEGMENT(N_FREEWAY_LINKS) = ISEG
            USN_TO_SEG_END(N_FREEWAY_LINKS) = USN_TO_SEG_END(IL)
            EXIT
            WRITE(MSGTEXT, '(A, I4)') 'Illegal Freeway Geometry at node ', FUSN(IL)
            CALL SENDTEXTMSG(M_ERROR)
            ERROR_FLAG = 1
            RETURN
          ENDIF
        ENDDO
      ENDIF
    ENDDO
 
  ! --- Define the distance from the upstream end of mainline links and off-ramp links to the
  ! --- end of the segment. It is the length of the link plus the combined length of the
  ! --- downstream links until the end of the segment.

    DO ISEG = 1, NUMBER_OF_SEGMENTS
      DO ILX = 1, NUMBER_OF_LINKS_IN_SEGMENT(ISEG) 
        ILNK = LINKS_IN_SEGMENT(ISEG, ILX)
        IF(LINKTYPE(ILNK) .NE. 0) CYCLE
        USN_TO_SEG_END(ILNK) = FLENGTH(ILNK)
        IL = FTHRU_LINK(ILNK)
        IF(IL .NE. 0) THEN
          USN_TO_SEG_END(ILNK) = USN_TO_SEG_END(ILNK) + USN_TO_SEG_END(IL)
        ENDIF

  ! --- If there is an off-ramp link, define the distance from the upstream
  ! --- end of the off-ramp to the end of the segment.
 
        IF(OFFRAMP_LINK(ILNK) .NE. 0) THEN
          IRAMP = OFFRAMP_LINK(ILNK)
          IL = FTHRU_LINK(IRAMP)
          IF(IL .NE. 0) THEN
            IF(NODE_TYPE(FDSN(IL)) .NE. NT_INTERN) THEN
              USN_TO_SEG_END(IRAMP) = USN_TO_SEG_END(ILNK) - FLENGTH(ILNK)
              USN_TO_SEG_END(IL) = USN_TO_SEG_END(ILNK) - FLENGTH(ILNK) - FLENGTH(IRAMP)
            ELSEIF(.NOT. DIVERGE_LINK(IRAMP)) THEN
              USN_TO_SEG_END(IRAMP) = USN_TO_SEG_END(ILNK) - FLENGTH(ILNK)
              TEMP = USN_TO_SEG_END(IRAMP) - FLENGTH(IRAMP)
              DO WHILE(NODE_TYPE(FDSN(IRAMP)) .EQ. NT_INTERN .AND. LINKTYPE(IRAMP) .LT. 0)
                DO WHILE(IL .NE. 0)
                  TEMP = TEMP - FLENGTH(IL) 
                  USN_TO_SEG_END(IL) = TEMP
                  IF(NODE_TYPE(FDSN(IL)) .NE. NT_INTERN) EXIT
                  IL = FTHRU_LINK(IL)
                ENDDO
                IRAMP = FTHRU_LINK(IRAMP)
              ENDDO
            ENDIF
          ELSE
            USN_TO_SEG_END(IRAMP) = USN_TO_SEG_END(ILNK) - FLENGTH(ILNK)
          ENDIF
        ENDIF
      ENDDO
    ENDDO
      
  ! --- Define the distance from the upstream end of the on-ramps
  ! --- to the end of the segment that they connect to. 
 
    DO ILNK = 1, N_FREEWAY_LINKS
      IF(LINKTYPE(ILNK) .GT. 0) THEN
        IL = FTHRU_LINK(ILNK)
        ILEN = FLENGTH(ILNK)
        DO WHILE(IL .NE. 0)
          IF(LINKTYPE(IL) .GT. 0) THEN
            ILEN = ILEN + FLENGTH(IL)
            IL = FTHRU_LINK(IL)
          ELSEIF(LINKTYPE(IL) .EQ. 0) THEN
            ILEN = ILEN + USN_TO_SEG_END(IL)
            EXIT
          ELSE
            IL = 0
            WRITE(MSGTEXT, '(A, I4)') 'Illegal Freeway Geometry at node ', FDSN(ILNK)
            CALL SENDTEXTMSG(M_ERROR)
          ENDIF
        ENDDO
        USN_TO_SEG_END(ILNK) = ILEN
      ENDIF
    ENDDO
  
  ! Continue processing freeway elements.

    CALL STORE_FREEWAY_OBJECTS
    CALL PROCESS_INCIDENTS
    DO IL = 1, N_FREEWAY_LINKS
      IF(RAMP_MERGE_LINK(IL)) THEN
        ILX = MAINLINE_APPROACH(FDSN(IL))
        IF(ILX .NE. 0) THEN
          MAIN_MERGE_LINK(ILX) = .TRUE.
        ELSE
          ILX = RAMP_APPROACH(FDSN(IL))
          IF(ILX .NE. 0) THEN
            MAIN_MERGE_LINK(ILX) = .TRUE.
          ENDIF
        ENDIF
        CALL PROCESS_MERGE_LINKS(IL, ILX)
      ENDIF
    ENDDO
  ENDIF
  
  RETURN
  END

! ==================================================================================================
  SUBROUTINE STORE_FREEWAY_OBJECTS
! ----------------------------------------------------------------------
! --- Store objects that represent geometric and operational objects.
! ----------------------------------------------------------------------
  USE SEGMENTS
  USE FREEWAY_LINKS
  USE OBJECTS
  USE FREEWAY_NODES
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER :: ISEG, IL, N, FIRST, NOBJECTS
! ----------------------------------------------------------------------
  NUMBER_OF_OBJECTS = 100
  CALL ALLOCATE_OBJECT_LIST
  NOBJECTS = 0
       
! --- Loop through segments and add geometric objects.
 
  FIRST_OBJECT(1) = 1
  DO ISEG = 1, NUMBER_OF_SEGMENTS
    
! --- Save the first object for the segment.

    FIRST = NOBJECTS + 1
    IF(ISEG .NE. 1) THEN
      LAST_OBJECT(ISEG - 1) = NOBJECTS
      FIRST_OBJECT(ISEG) = FIRST
    ENDIF

! --- Loop over all links in the segment.

    DO N = 1, NUMBER_OF_LINKS_IN_SEGMENT(ISEG)
      IL = LINKS_IN_SEGMENT(ISEG, N)

! --- Add objects for each type of link.

      IF(NODE_TYPE(FDSN(IL)) .EQ. NT_EXTERN) THEN

! --- Exit link.

        CALL LOAD_EXIT_OBJECT(IL, ISEG, NOBJECTS)
        
      ELSEIF(NODE_TYPE(FDSN(IL)) .NE. NT_INTERN) THEN

! --- Exit interface link.

        CALL LOAD_EXIT_INTERFACE_OBJECT(IL, ISEG, NOBJECTS)
        
      ELSEIF(NODE_TYPE(FUSN(IL)) .EQ. NT_EXTERN) THEN

! --- Entry link.

        CALL LOAD_ENTRY_OBJECT(IL, ISEG, NOBJECTS)
        
      ELSEIF(NODE_TYPE(FUSN(IL)) .NE. NT_INTERN) THEN

! --- Entry interface link.

        CALL LOAD_ENTRY_INTERFACE_OBJECT(IL, ISEG, NOBJECTS)
        
      ELSEIF(RAMP_MERGE_LINK(IL)) THEN

! --- Merge links.
        
        CALL LOAD_MERGE_POINT(IL, ISEG, NOBJECTS) 
        
      ELSE

! --- Internal link.

        CALL LOAD_INTERNAL_LINK_OBJECTS(IL, ISEG, NOBJECTS)
        
      ENDIF
    ENDDO
         
! --- Store all other types of objects.

    CALL LOAD_ALIGNMENTS(ISEG, NOBJECTS)
    CALL LOAD_ANTICIP_WARNING(ISEG, NOBJECTS)
    CALL LOAD_DIVERSIONS(ISEG, NOBJECTS) 
    CALL LOAD_HOV_LANES(ISEG, NOBJECTS) 
    CALL LOAD_INCIDENTS(ISEG, NOBJECTS) 
    CALL LOAD_OFFRAMP_WARNING(ISEG, NOBJECTS)
    CALL LOAD_TRUCK_LANES(ISEG, NOBJECTS) 
         
! --- Sort the objects by location and then by type.

    CALL SORT_BY_LOCATION(ISEG, FIRST, NOBJECTS)
    CALL SORT_BY_TYPE(ISEG, FIRST, NOBJECTS)
  ENDDO
  LAST_OBJECT(NUMBER_OF_SEGMENTS) = NOBJECTS
  CALL LOCATE_NODE_OBJECTS
  RETURN
  END

! ==================================================================================================
  SUBROUTINE ADD_OBJECT(NOBJECTS)
! ----------------------------------------------------------------------
! --- Increment the number of objects'
! ----------------------------------------------------------------------
  USE OBJECTS
  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: NOBJECTS
! ----------------------------------------------------------------------
  NOBJECTS = NOBJECTS + 1
  IF(NOBJECTS .GE. NUMBER_OF_OBJECTS) CALL REALLOCATE_OBJECT_LIST
  RETURN
  END

! ==================================================================================================
  SUBROUTINE LOAD_EXIT_OBJECT(IL, ISEG, NOBJECTS)
! ----------------------------------------------------------------------
! --- Load objects for exit links.
! ----------------------------------------------------------------------
  USE OBJECTS
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IL, ISEG
  INTEGER, INTENT(INOUT) :: NOBJECTS
! ----------------------------------------------------------------------
  CALL ADD_OBJECT(NOBJECTS)
  OBJECT_LIST(NOBJECTS)%ITYPE = M_NODE_EXIT
  OBJECT_LIST(NOBJECTS)%SEGMENT = ISEG
  OBJECT_LIST(NOBJECTS)%LOCATION = USN_TO_SEG_END(IL)
  OBJECT_LIST(NOBJECTS)%LINK = IL
  OBJECT_LIST(NOBJECTS)%LANE = 0
  OBJECT_LIST(NOBJECTS)%VALUE = FDSN(IL)
  RETURN
  END

! ==================================================================================================
  SUBROUTINE LOAD_EXIT_INTERFACE_OBJECT(IL, ISEG, NOBJECTS)
! ----------------------------------------------------------------------
! --- Load objects for exit interface links.
! ----------------------------------------------------------------------
  USE OBJECTS
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IL, ISEG
  INTEGER, INTENT(INOUT) :: NOBJECTS
! ----------------------------------------------------------------------
  CALL ADD_OBJECT(NOBJECTS)
  OBJECT_LIST(NOBJECTS)%ITYPE = M_EXIT_INTERFACE
  OBJECT_LIST(NOBJECTS)%SEGMENT = ISEG
  OBJECT_LIST(NOBJECTS)%LOCATION = USN_TO_SEG_END(IL) - FLENGTH(IL)
  OBJECT_LIST(NOBJECTS)%LINK = IL
  OBJECT_LIST(NOBJECTS)%LANE = 0
  OBJECT_LIST(NOBJECTS)%VALUE = FDSN(IL)
  CALL LOAD_DETECTORS(IL, ISEG, NOBJECTS)
  RETURN
  END 

! ==================================================================================================
  SUBROUTINE LOAD_ENTRY_OBJECT(IL, ISEG, NOBJECTS)
! ----------------------------------------------------------------------
! --- Load objects for entry links.
! ----------------------------------------------------------------------
  USE OBJECTS
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IL, ISEG
  INTEGER, INTENT(INOUT) :: NOBJECTS
! ----------------------------------------------------------------------
  CALL ADD_OBJECT(NOBJECTS)
  OBJECT_LIST(NOBJECTS)%ITYPE = M_NODE_ENTRY
  OBJECT_LIST(NOBJECTS)%SEGMENT = ISEG
  OBJECT_LIST(NOBJECTS)%LOCATION = USN_TO_SEG_END(IL)
  OBJECT_LIST(NOBJECTS)%LINK = IL
  OBJECT_LIST(NOBJECTS)%LANE = 0
  OBJECT_LIST(NOBJECTS)%VALUE = FUSN(IL)
  RETURN
  END

! ==================================================================================================
  SUBROUTINE LOAD_ENTRY_INTERFACE_OBJECT(IL, ISEG, NOBJECTS)
! ----------------------------------------------------------------------
! --- Load objects for entry interface links.
! ----------------------------------------------------------------------
  USE OBJECTS
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IL, ISEG
  INTEGER, INTENT(INOUT) :: NOBJECTS
! ----------------------------------------------------------------------
  CALL ADD_OBJECT(NOBJECTS)
  OBJECT_LIST(NOBJECTS)%ITYPE = M_NODE_INT
  OBJECT_LIST(NOBJECTS)%SEGMENT = ISEG
  OBJECT_LIST(NOBJECTS)%LOCATION = USN_TO_SEG_END(IL) - FLENGTH(IL)
  OBJECT_LIST(NOBJECTS)%LINK = IL
  OBJECT_LIST(NOBJECTS)%LANE = 0
  OBJECT_LIST(NOBJECTS)%VALUE = FDSN(IL)

  CALL ADD_OBJECT(NOBJECTS)
  OBJECT_LIST(NOBJECTS)%ITYPE = M_ENTRY_INTERFACE
  OBJECT_LIST(NOBJECTS)%SEGMENT = ISEG
  OBJECT_LIST(NOBJECTS)%LOCATION = USN_TO_SEG_END(IL)
  OBJECT_LIST(NOBJECTS)%LINK = IL
  OBJECT_LIST(NOBJECTS)%LANE = 0
  OBJECT_LIST(NOBJECTS)%VALUE = FUSN(IL)
  RETURN
  END

! ==================================================================================================
  SUBROUTINE LOAD_INTERNAL_LINK_OBJECTS(IL, ISEG, NOBJECTS)
! ----------------------------------------------------------------------
! --- Load objects for internal links.
! ----------------------------------------------------------------------
  USE OBJECTS
  USE FREEWAY_LINKS
  USE NODE_TABLE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IL, ISEG
  INTEGER, INTENT(INOUT) :: NOBJECTS
! ----------------------------------------------------------------------
  IF(NODE_TYPE(FDSN(IL)) .NE. NT_EXTERN) THEN
    CALL LOAD_NODE(IL, ISEG, NOBJECTS)
    CALL LOAD_AUX_LANES(IL, ISEG, NOBJECTS)       
    CALL LOAD_ADDS_DROPS(IL, ISEG, NOBJECTS)    
    CALL LOAD_DETECTORS(IL, ISEG, NOBJECTS)
    CALL LOAD_DATASTATIONS(IL, ISEG, NOBJECTS)        
    CALL LOAD_METER(IL, ISEG, NOBJECTS)      
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE LOAD_NODE(IL, ISEG, NOBJECTS)
! ----------------------------------------------------------------------
! --- Load objects for internal nodes.
! ----------------------------------------------------------------------
  USE OBJECTS
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IL, ISEG
  INTEGER, INTENT(INOUT) :: NOBJECTS
! ----------------------------------------------------------------------
  CALL ADD_OBJECT(NOBJECTS)
  OBJECT_LIST(NOBJECTS)%ITYPE = M_NODE_INT
  OBJECT_LIST(NOBJECTS)%SEGMENT = ISEG
  OBJECT_LIST(NOBJECTS)%LOCATION = USN_TO_SEG_END(IL) - FLENGTH(IL)
  OBJECT_LIST(NOBJECTS)%LINK = IL
  OBJECT_LIST(NOBJECTS)%LANE = 0
  OBJECT_LIST(NOBJECTS)%VALUE = FDSN(IL)
  RETURN
  END

! ==================================================================================================
  SUBROUTINE LOAD_DETECTORS(IL, ISEG, NOBJECTS)
! ----------------------------------------------------------------------
! --- Load objects for detectors.
! ----------------------------------------------------------------------
  USE OBJECTS
  USE FREEWAY_LINKS
  USE FREEWAY_DETECTORS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IL, ISEG
  INTEGER, INTENT(INOUT) :: NOBJECTS
  INTEGER :: IDET
! ----------------------------------------------------------------------
  IDET = FFIRST_DETECTOR(IL)
  DO WHILE(IDET .GT. 0) 
    CALL ADD_OBJECT(NOBJECTS)
    OBJECT_LIST(NOBJECTS)%ITYPE = M_DETECTOR
    OBJECT_LIST(NOBJECTS)%SEGMENT = ISEG
    OBJECT_LIST(NOBJECTS)%LOCATION = USN_TO_SEG_END(IL) - FDETECTOR(IDET)%LOCATION
    OBJECT_LIST(NOBJECTS)%LINK = IL
    OBJECT_LIST(NOBJECTS)%LANE = FDETECTOR(IDET)%LANE1
    OBJECT_LIST(NOBJECTS)%VALUE = IDET
    IDET = FDETECTOR(IDET)%NEXT_DET
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE LOAD_DATASTATIONS(IL, ISEG, NOBJECTS)
! ----------------------------------------------------------------------
! --- Load objects for data stations.
! ----------------------------------------------------------------------
  USE OBJECTS
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IL, ISEG
  INTEGER, INTENT(INOUT) :: NOBJECTS
! ----------------------------------------------------------------------
  IF(DATASTATION_ID(IL) .GT. 0) THEN
    CALL ADD_OBJECT(NOBJECTS)
    OBJECT_LIST(NOBJECTS)%ITYPE = M_DATASTAT
    OBJECT_LIST(NOBJECTS)%SEGMENT = ISEG
    OBJECT_LIST(NOBJECTS)%LOCATION = USN_TO_SEG_END(IL) - DATASTATION_LOCATION(IL)
    OBJECT_LIST(NOBJECTS)%LINK = IL
    OBJECT_LIST(NOBJECTS)%LANE = 0
    OBJECT_LIST(NOBJECTS)%VALUE = DATASTATION_ID(IL)
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE LOAD_METER(IL, ISEG, NOBJECTS)
! ----------------------------------------------------------------------
! --- Load objects for ramp meters.
! ----------------------------------------------------------------------
  USE OBJECTS
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IL, ISEG
  INTEGER, INTENT(INOUT) :: NOBJECTS
! ----------------------------------------------------------------------
  IF(RAMPMETER(IL) .GT. 0) THEN
    CALL ADD_OBJECT(NOBJECTS)
    OBJECT_LIST(NOBJECTS)%ITYPE = M_METER
    OBJECT_LIST(NOBJECTS)%SEGMENT = ISEG
    OBJECT_LIST(NOBJECTS)%LOCATION = USN_TO_SEG_END(IL) - FLENGTH(IL)
    OBJECT_LIST(NOBJECTS)%LINK = IL
    OBJECT_LIST(NOBJECTS)%LANE = 0
    OBJECT_LIST(NOBJECTS)%VALUE = RAMPMETER(IL)
  ENDIF 
  RETURN
  END

! ==================================================================================================
  SUBROUTINE LOAD_AUX_LANES(IL, ISEG, NOBJECTS)
! ----------------------------------------------------------------------
! --- Load objects for auxiliary lanes.
! ----------------------------------------------------------------------
  USE OBJECTS
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IL, ISEG
  INTEGER, INTENT(INOUT) :: NOBJECTS
  INTEGER :: IAUX, P1, P2
! ----------------------------------------------------------------------
  DO IAUX = 1, N_AUXLANES 
    IF(AUX_LANE_ID(IL, IAUX) .EQ. 0) EXIT !no more auxiliary lanes on this link
    IF(AUX_LANE_CODE(IL, IAUX) .EQ. AUX_ACCEL) THEN
      P1 = USN_TO_SEG_END(IL)
    ELSEIF(AUX_LANE_CODE(IL, IAUX) .EQ. AUX_DECEL) THEN
      P1 = USN_TO_SEG_END(IL) - FLENGTH(IL) + AUX_LANE_LENGTH(IL, IAUX)
    ELSE
      P1 = USN_TO_SEG_END(IL)
    ENDIF
    P2 = P1 - AUX_LANE_LENGTH(IL, IAUX)
    
    CALL ADD_OBJECT(NOBJECTS)
    OBJECT_LIST(NOBJECTS)%ITYPE = M_AUX_BEGIN
    OBJECT_LIST(NOBJECTS)%SEGMENT = ISEG
    OBJECT_LIST(NOBJECTS)%LOCATION = P1
    OBJECT_LIST(NOBJECTS)%LINK = IL
    OBJECT_LIST(NOBJECTS)%LANE = AUX_LANE_ID(IL, IAUX)
    OBJECT_LIST(NOBJECTS)%VALUE = AUX_LANE_CODE(IL, IAUX)

    CALL ADD_OBJECT(NOBJECTS)
    OBJECT_LIST(NOBJECTS)%ITYPE = M_AUX_END
    OBJECT_LIST(NOBJECTS)%SEGMENT = ISEG
    OBJECT_LIST(NOBJECTS)%LOCATION = P2
    OBJECT_LIST(NOBJECTS)%LINK = IL
    OBJECT_LIST(NOBJECTS)%LANE = AUX_LANE_ID(IL, IAUX)
    OBJECT_LIST(NOBJECTS)%VALUE = AUX_LANE_CODE(IL, IAUX)
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE LOAD_INCIDENTS(ISEG, NOBJECTS)
! ----------------------------------------------------------------------
! --- Load objects for incidents.
! ----------------------------------------------------------------------
  USE OBJECTS
  USE FREEWAY_LINKS
  USE INCIDENTS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ISEG
  INTEGER, INTENT(INOUT) :: NOBJECTS
  INTEGER :: INC, IL
  REAL :: INCIDENT_LOCATION
! ----------------------------------------------------------------------
  DO INC = 1, NUMBER_OF_INCIDENTS
    IL = INCIDENT_LINK(INC)
    IF(SEGMENT(IL) .EQ. ISEG) THEN
 
! --- Add an object for the start point of the incident.
 
      INCIDENT_LOCATION = USN_TO_SEG_END(IL) - INCIDENT_BEGIN_POINT(INC)

      CALL ADD_OBJECT(NOBJECTS)
      OBJECT_LIST(NOBJECTS)%ITYPE = M_INC_BEGIN
      OBJECT_LIST(NOBJECTS)%SEGMENT = SEGMENT(IL)
      OBJECT_LIST(NOBJECTS)%LOCATION = INCIDENT_LOCATION
      OBJECT_LIST(NOBJECTS)%LINK = IL
      OBJECT_LIST(NOBJECTS)%LANE = 0
      OBJECT_LIST(NOBJECTS)%VALUE = INC
 
! --- Add an object for the end point of the incident.
 
      CALL ADD_OBJECT(NOBJECTS)
      OBJECT_LIST(NOBJECTS)%ITYPE = M_INC_END
      OBJECT_LIST(NOBJECTS)%SEGMENT = SEGMENT(IL)
      OBJECT_LIST(NOBJECTS)%LOCATION = INCIDENT_LOCATION - INCIDENT_END_POINT(INC)
      OBJECT_LIST(NOBJECTS)%LINK = IL
      OBJECT_LIST(NOBJECTS)%LANE = 0
      OBJECT_LIST(NOBJECTS)%VALUE = INC
 
! --- Add an object for the warning sign for the incident.
 
      CALL ADD_OBJECT(NOBJECTS)
      OBJECT_LIST(NOBJECTS)%ITYPE = M_INC_WARN
      OBJECT_LIST(NOBJECTS)%SEGMENT = SEGMENT(IL)
      OBJECT_LIST(NOBJECTS)%LOCATION = INCIDENT_LOCATION + INCIDENT_WARN_POINT(INC)
      OBJECT_LIST(NOBJECTS)%LINK = IL
      OBJECT_LIST(NOBJECTS)%LANE = 0
      OBJECT_LIST(NOBJECTS)%VALUE = INC
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE LOAD_DIVERSIONS(ISEG, NOBJECTS)
! ----------------------------------------------------------------------
! --- Load objects for diversions.
! ----------------------------------------------------------------------
  USE OBJECTS
  USE FREEWAY_LINKS
  USE DIVERSIONS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ISEG
  INTEGER, INTENT(INOUT) :: NOBJECTS
  INTEGER :: IND, IL
! ----------------------------------------------------------------------
  DO IND = 1, NUMBER_OF_DIVERSIONS
    IL = DIVERSION_LINK(IND)
    IF(SEGMENT(IL) .EQ. ISEG) THEN
 
! --- Add an object for the start point of the diversion.
 
      CALL ADD_OBJECT(NOBJECTS)
      OBJECT_LIST(NOBJECTS)%ITYPE = M_DIVERGE
      OBJECT_LIST(NOBJECTS)%SEGMENT = SEGMENT(IL)
      OBJECT_LIST(NOBJECTS)%LOCATION = USN_TO_SEG_END(IL) - DIVERSION_LOCATION(IND)
      OBJECT_LIST(NOBJECTS)%LINK = IL
      OBJECT_LIST(NOBJECTS)%LANE = 0
      OBJECT_LIST(NOBJECTS)%VALUE = IND
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE LOAD_HOV_LANES(ISEG, NOBJECTS)
! ----------------------------------------------------------------------
! --- Load objects for HOV lanes.
! ----------------------------------------------------------------------
  USE OBJECTS
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ISEG
  INTEGER, INTENT(INOUT) :: NOBJECTS
  INTEGER :: IL
! ----------------------------------------------------------------------
  DO IL = 1, N_FREEWAY_LINKS
    IF(SEGMENT(IL) .EQ. ISEG .AND. NHOV_LANES(IL) .NE. 0) THEN
 
! --- Add an object for the start of the HOV lane.
 
      CALL ADD_OBJECT(NOBJECTS)
      OBJECT_LIST(NOBJECTS)%ITYPE = M_HOV_BEGIN
      OBJECT_LIST(NOBJECTS)%SEGMENT = SEGMENT(IL)
      OBJECT_LIST(NOBJECTS)%LOCATION = USN_TO_SEG_END(IL) - HOV_BEGIN(IL)
      OBJECT_LIST(NOBJECTS)%LINK = IL
      OBJECT_LIST(NOBJECTS)%LANE = 0
      OBJECT_LIST(NOBJECTS)%VALUE = IL
 
! --- Add an object for the end point of the HOV.
 
      CALL ADD_OBJECT(NOBJECTS)
      OBJECT_LIST(NOBJECTS)%ITYPE = M_HOV_END
      OBJECT_LIST(NOBJECTS)%SEGMENT = SEGMENT(IL)
      OBJECT_LIST(NOBJECTS)%LOCATION = USN_TO_SEG_END(IL) - HOV_END(IL)
      OBJECT_LIST(NOBJECTS)%LINK = IL
      OBJECT_LIST(NOBJECTS)%LANE = 0
      OBJECT_LIST(NOBJECTS)%VALUE = IL
 
! --- Add an object for the warning sign for the beginning of the HOV lane.
 
      CALL LOAD_HOV_WARNING_SIGN(ISEG, IL, NOBJECTS)
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE LOAD_HOV_WARNING_SIGN(ISEG, IL, NOBJECTS)
! ----------------------------------------------------------------------
! --- Load objects for HOV warning signs.
! ----------------------------------------------------------------------
  USE SEGMENTS
  USE FREEWAY_LINKS
  USE FREEWAY_NODES
  USE OBJECTS
  USE ADD_DROP_ALIGNMENTS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ISEG, IL
  INTEGER, INTENT(INOUT) :: NOBJECTS
  INTEGER :: I1, I2, ULINK, IL2
  INTEGER :: INODE, SIGNPOS, MAXPOS, GPOS, NMAX
! ----------------------------------------------------------------------
  NMAX = NUMBER_OF_LINKS_IN_SEGMENT(ISEG)
  MAXPOS = USN_TO_SEG_END(LINKS_IN_SEGMENT(ISEG, NMAX)) 
  SIGNPOS = MAXPOS
  IF(SIGNPOS .GE. USN_TO_SEG_END(FTHRU_LINK(IL)) + HOV_WARN(IL)) THEN
    SIGNPOS = USN_TO_SEG_END(FTHRU_LINK(IL)) + HOV_WARN(IL)
  ENDIF
  HOV_WARN(IL) = SIGNPOS

! --- Check if there are any on-ramps between this off-ramp and its
! --- associated warning sign location. If so, load a warning sign in
! --- the geometry list.

  DO I1 = 1, NMAX
    IF(IL .EQ. LINKS_IN_SEGMENT(ISEG, I1)) THEN
      DO I2 = I1, NUMBER_OF_LINKS_IN_SEGMENT(ISEG)
        IL2 = LINKS_IN_SEGMENT(ISEG, I2)

! --- Test if this is an on-ramp link.

        IF(LINKTYPE(IL2) .GT. 0) THEN

! --- Check if this on-ramp link is the last one in its roadway.

          IF(LINKTYPE(FTHRU_LINK(IL2)) .EQ. 0) THEN

! --- Set a warning sign at the on-ramp gore. Compute the on-ramp
! --- gore position.

            GPOS = USN_TO_SEG_END(IL2) - FLENGTH(IL2)
            IF(HOV_WARN(IL) .GT. GPOS) THEN
              INODE = FUSN(FTHRU_LINK(IL2))
              CALL LOAD_OBJECT(ISEG, FTHRU_LINK(IL2), 0, 0, GPOS - 1, NOBJECTS, M_HOV_WARN)
            ENDIF
          ENDIF
        ENDIF
      ENDDO
    ENDIF
  ENDDO

! --- Set the warning sign on the main line.

  INODE = FDSN(IL)
  SIGNPOS = HOV_WARN(IL)
  IF(SIGNPOS .EQ. MAXPOS) SIGNPOS = SIGNPOS - 1
  CALL LOAD_OBJECT(ISEG, MAINLINE_APPROACH(INODE), 0, 0, SIGNPOS, NOBJECTS, M_HOV_WARN)

  RETURN
  END
  
! ==================================================================================================
  SUBROUTINE LOAD_TRUCK_LANES(ISEG, NOBJECTS)
! ----------------------------------------------------------------------
! --- Load objects for exclusive truck lanes.
! ----------------------------------------------------------------------
  USE SEGMENTS
  USE FREEWAY_LINKS
  USE FREEWAY_NODES
  USE OBJECTS
  USE ADD_DROP_ALIGNMENTS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ISEG
  INTEGER, INTENT(INOUT) :: NOBJECTS
  INTEGER :: I1, I2, ULINK, IL, IL2
  INTEGER :: ZPOS, ZMAX, ZPON, UPDIST
! ----------------------------------------------------------------------
  ZMAX = USN_TO_SEG_END(LINKS_IN_SEGMENT(ISEG, NUMBER_OF_LINKS_IN_SEGMENT(ISEG))) - 1
  DO I1 = 1, NUMBER_OF_LINKS_IN_SEGMENT(ISEG)
    IL = LINKS_IN_SEGMENT(ISEG, I1)
 
! --- Check if there is a link with an exclusive truck lane.
 
    IF(TRUCK_CODE(IL) .GT. TRK_BIASED) THEN
 
! --- Check if this is the most upstream link with an exclusive truck lane.
 
      ULINK = MAINLINE_APPROACH(FUSN(IL))
      IF(ULINK .EQ. 0) CYCLE
      IF(TRUCK_CODE(ULINK) .LE. TRK_BIASED) THEN
        ZPOS = MIN(FLOAT(ZMAX), USN_TO_SEG_END(IL) + ETL_WARN(IL))
 
! --- Loop over all links to check if there are any on-ramps between
! --- this link and its associated warning sign location. If so,
! --- load a warning sign in the geometry list.
 
        DO I2 = I1, NUMBER_OF_LINKS_IN_SEGMENT(ISEG)
          IL2 = LINKS_IN_SEGMENT(ISEG, I2)
          IF(LINKTYPE(IL2) .GT. 0) THEN
                                                                      
! --- Set a warning sign at the on-ramp gore.                     
                                                                      
            IF(LINKTYPE(FTHRU_LINK(IL2)) .EQ. 0) THEN
              ZPON = USN_TO_SEG_END(LINKS_IN_SEGMENT(ISEG, I2 - 1))
              IF(ZPOS .GE. ZPON) THEN
                ZPON = ZPON - 1
                CALL LOAD_OBJECT(ISEG, FTHRU_LINK(IL2), 0, IL, ZPON, NOBJECTS, M_ETL_WARN)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
 
! --- Set the warning sign on the main line.
 
        ZPOS = MIN(FLOAT(ZMAX), USN_TO_SEG_END(IL) + ETL_WARN(IL))
        UPDIST = 0
        DO
          UPDIST = UPDIST + FLENGTH(ULINK)
          IF(ETL_WARN(IL) .LE. UPDIST) THEN
            EXIT
          ELSE
            ULINK = MAINLINE_APPROACH(FUSN(ULINK))
            IF(ULINK .EQ. 0) EXIT
          ENDIF
        ENDDO
        CALL LOAD_OBJECT(ISEG, ULINK, 0, IL, ZPOS, NOBJECTS, M_ETL_WARN)
      ENDIF
    ENDIF
  ENDDO
  RETURN
  END


! ==================================================================================================
  SUBROUTINE LOAD_ADDS_DROPS(IL, ISEG, NOBJECTS)
! ----------------------------------------------------------------------
! --- Load objects for lane adds and/or drops.
! ----------------------------------------------------------------------
  USE OBJECTS
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IL, ISEG
  INTEGER, INTENT(INOUT) :: NOBJECTS
  INTEGER :: I, LANE, ZPOS
! ----------------------------------------------------------------------
  IF(ADDDROP_LANE(IL, 1) .GT. 0) THEN
    DO I = 1, 3
      IF(ADDDROP_CODE(IL, I) .EQ. 0) EXIT !no more add/drops on this link
      ZPOS = USN_TO_SEG_END(IL) - ADDDROP_DIST(IL, I)
      LANE = ADDDROP_LANE(IL, I)
      IF(ADDDROP_CODE(IL, I) .EQ. I_DROP) THEN
        CALL LOAD_OBJECT(ISEG, IL, LANE, I_DROP, ZPOS, NOBJECTS, M_LANE_DROP)
        CALL LOAD_DROP_WARNING(IL, LANE, ISEG, NOBJECTS, I) 
      ELSE
        CALL LOAD_OBJECT(ISEG, IL, LANE, I_ADD, ZPOS, NOBJECTS, M_LANE_ADD)
      ENDIF
    ENDDO
    CALL ADDDROP_ALIGNMENT(IL, ISEG, NOBJECTS)
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE LOAD_OBJECT(ISEG, IL, LANE, IDAT, ZPOS, NOBJECTS, ITYPE)
! ----------------------------------------------------------------------
! --- Load prepared object data.
! ----------------------------------------------------------------------
  USE OBJECTS
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ISEG, IL, LANE, IDAT, ITYPE, ZPOS
  INTEGER, INTENT(INOUT) :: NOBJECTS
! ----------------------------------------------------------------------
  CALL ADD_OBJECT(NOBJECTS)
  OBJECT_LIST(NOBJECTS)%ITYPE = ITYPE
  OBJECT_LIST(NOBJECTS)%SEGMENT = ISEG
  OBJECT_LIST(NOBJECTS)%LOCATION = ZPOS
  OBJECT_LIST(NOBJECTS)%LINK = IL
  OBJECT_LIST(NOBJECTS)%LANE = LANE
  OBJECT_LIST(NOBJECTS)%VALUE = IDAT
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE LOAD_DROP_WARNING(IL, LANE, ISEG, NOBJECTS, IDROP)
! ----------------------------------------------------------------------
! --- Load objects for lane drop warning signs.
! ----------------------------------------------------------------------
  USE SEGMENTS
  USE FREEWAY_LINKS
  USE OBJECTS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IL, LANE, ISEG, IDROP
  INTEGER, INTENT(INOUT) :: NOBJECTS
  INTEGER :: ZMAX, ZWARN, ZDROP, ZPOS, ZPON
  INTEGER :: ILNUM, I1, IL1, ULINK, NLINKS
  INTEGER :: DROPDIST
! ----------------------------------------------------------------------
  NLINKS = NUMBER_OF_LINKS_IN_SEGMENT(ISEG)
 
! --- Determine the upstream end of the segment, ZMAX.
 
  ZMAX = USN_TO_SEG_END(LINKS_IN_SEGMENT(ISEG, NLINKS)) 
 
! --- Determine the location of the drop and the warning sign.
 
  ZDROP = USN_TO_SEG_END(IL) - ADDDROP_DIST(IL, IDROP)
  ZWARN = ZDROP + ADDDROP_WARN(IL, IDROP)
 
! --- Determine which link within the segment is link IL.
 
  ILNUM = 0
  DO I1 = 1, NLINKS         
    IL1 = LINKS_IN_SEGMENT(ISEG, I1)
    IF(IL1 .EQ. IL) THEN
      ILNUM = I1
      EXIT
    ENDIF
  ENDDO
 
  DROPDIST = ADDDROP_WARN(IL, IDROP)
  IF(LINKTYPE(IL) .EQ. 0) THEN
 
! --- Link IL is a mainline link.
 
    ZPOS = MIN(ZMAX - 1, ZWARN)
 
! --- Insert the warning sign for the drop.
 
    CALL LOAD_OBJECT(ISEG, IL, LANE, DROPDIST, ZPOS, NOBJECTS, M_WARN_DROP)   
 
! --- Copy the warning sign at any on-ramps between the warning sign and the drop.
 
    DO I1 = ILNUM, NLINKS
      IL1 = LINKS_IN_SEGMENT(ISEG, I1)
      ULINK = LINKS_IN_SEGMENT(ISEG, I1 - 1)
      IF(LINKTYPE(IL1) .LT. 0) CYCLE
      IF(LINKTYPE(IL1) .GT. 0) THEN
        IF(LINKTYPE(ULINK) .EQ. 0) THEN
          ZPON = USN_TO_SEG_END(ULINK) - FLENGTH(ULINK) - 1
          DROPDIST = DROPDIST - ZPOS + ZPON
          CALL LOAD_OBJECT(ISEG, IL, LANE, DROPDIST, ZPON, NOBJECTS, M_WARN_DROP)
        ENDIF
      ENDIF
      EXIT
    ENDDO
  ELSE
    IF(LINKTYPE(IL) .GT. 0) THEN
 
! --- Link IL is an on-ramp link. Determine the maximum
! --- upstream location for the warning sign.
 
      ZPOS = 0
      DO I1 = ILNUM + 1, NLINKS
        IL1 = LINKS_IN_SEGMENT(ISEG, I1)
        IF(LINKTYPE(IL1) .GT. 0) ZPOS = USN_TO_SEG_END(IL1)
      ENDDO
      IF(ZPOS .LE. ZWARN) THEN
        ZPOS = ZPOS - 1
      ELSE
        ZPOS = ZWARN
      ENDIF
 
! --- Insert the warning sign for the drop.
 
      CALL LOAD_OBJECT(ISEG, IL, LANE, DROPDIST, ZPOS, NOBJECTS, M_WARN_DROP)   
    ELSE
 
! --- Link IL is an off-ramp link.
 
      DO I1 = ILNUM, 1, -1
        IL1 = LINKS_IN_SEGMENT(ISEG, I1)
        IF(LINKTYPE(IL1) .EQ. 0) THEN
          ZPOS = USN_TO_SEG_END(IL)
          EXIT
        ENDIF
      ENDDO
      IF(ZPOS .LT. ZWARN) ZPOS = ZPOS - 1
      IF(ZPOS .LE. ZWARN) THEN
        ZPOS = ZPOS - 1
      ELSE
        ZPOS = ZWARN
      ENDIF
 
! --- Insert the warning sign for the drop.
 
      CALL LOAD_OBJECT(ISEG, IL, LANE, DROPDIST, ZPOS, NOBJECTS, M_WARN_DROP)   
    ENDIF
  ENDIF
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE ADDDROP_ALIGNMENT(IL, ISEG, NOBJECTS)
! ----------------------------------------------------------------------
! --- Load objects for lane alignments at lane adds and/or drops.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  USE OBJECTS
  USE ADD_DROP_ALIGNMENTS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IL, ISEG
  INTEGER, INTENT(INOUT) :: NOBJECTS
  INTEGER :: IADP, JADP, UPLANES, IAUX, ILANE
! ----------------------------------------------------------------------
  DO IADP = 1, 3
    IF(ADDDROP_LANE(IL, IADP) .GT. 0) THEN
      UPLANES = FNUMLANES(IL)
      DO JADP = 1, IADP - 1
        UPLANES = UPLANES + ADDDROP_CODE(IL, JADP)
      ENDDO
      NADDRP = NADDRP + 1
      DO IAUX = 1, N_AUXLANES
        IF(AUX_LANE_ID(IL, IAUX) .EQ. 0) EXIT !no more auxiliary lanes on this link
        IF(AUX_LANE_CODE(IL, IAUX) .EQ. AUX_FULL .OR.                          &
          (AUX_LANE_CODE(IL, IAUX) .EQ. AUX_ACCEL .AND.                        &
           AUX_LANE_LENGTH(IL, IAUX) .GE. ADDDROP_DIST(IL, IADP)) .OR.         &
          (AUX_LANE_CODE(IL, IAUX) .EQ. AUX_DECEL .AND.                        &
          (FLENGTH(IL) - AUX_LANE_LENGTH(IL, IAUX)) .LE.                       &
            ADDDROP_DIST(IL, IADP))) THEN
          ILANE = AUX_LANE_ID(IL, IAUX)
          CALL SET_ADDRP_ALIGNMENT(NADDRP, ILANE, ILANE) 
        ENDIF
      ENDDO
      IF(ADDDROP_CODE(IL, IADP) .EQ. I_ADD) THEN
        DO ILANE = 1, UPLANES
          IF(ILANE .LT. ADDDROP_LANE(IL, IADP)) THEN
            CALL SET_ADDRP_ALIGNMENT(NADDRP, ILANE, ILANE)
          ELSE
            CALL SET_ADDRP_ALIGNMENT(NADDRP, ILANE, ILANE + 1)
          ENDIF
        ENDDO
      ELSEIF(ADDDROP_CODE(IL, IADP) .EQ. I_DROP) THEN
        DO ILANE = 1, UPLANES
          IF(ILANE .LT. ADDDROP_LANE(IL, IADP)) THEN
            CALL SET_ADDRP_ALIGNMENT(NADDRP, ILANE, ILANE)
          ELSE
            IF(ILANE .GT. ADDDROP_LANE(IL, IADP)) THEN
              CALL SET_ADDRP_ALIGNMENT(NADDRP, ILANE, ILANE - 1)
            ELSE
              CALL SET_ADDRP_ALIGNMENT(NADDRP, ILANE, 0)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
      CALL ADD_OBJECT(NOBJECTS)
      OBJECT_LIST(NOBJECTS)%ITYPE = M_ALIGN_ADP
      OBJECT_LIST(NOBJECTS)%SEGMENT = ISEG
      OBJECT_LIST(NOBJECTS)%LOCATION = USN_TO_SEG_END(IL) - ADDDROP_DIST(IL, IADP)
      OBJECT_LIST(NOBJECTS)%LINK = IL
      OBJECT_LIST(NOBJECTS)%LANE = 0
      OBJECT_LIST(NOBJECTS)%VALUE = NADDRP
    ENDIF
  ENDDO
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE LOAD_ALIGNMENTS(ISEG, NOBJECTS)
! ----------------------------------------------------------------------
! --- Load objects for lane alignments.
! ----------------------------------------------------------------------
  USE OBJECTS
  USE FREEWAY_LINKS
  USE SEGMENTS
  USE FREEWAY_NODES
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ISEG
  INTEGER, INTENT(INOUT) :: NOBJECTS
  INTEGER :: IL, ULINK, DLINK, ILOFF, ILINK, ILOF
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
#endif

! --- Loop over all links in each segment.
 
  DO IL = 1, NUMBER_OF_LINKS_IN_SEGMENT(ISEG) - 1
    DLINK = LINKS_IN_SEGMENT(ISEG, IL)
    ULINK = LINKS_IN_SEGMENT(ISEG, IL + 1)
#ifdef DebugVersion
  temp = fusn(ulink)
#endif
    IF(DLINK .NE. 0) THEN
      IF(RAMP_MERGE_LINK(DLINK)) THEN
        IF(FTHRU_LINK(DLINK) .NE. 0) THEN
 
! --- Terminator mainline link to mainline link on next segment.
 
          CALL LOAD_ONRAMP_ALIGNMENT(FTHRU_LINK(DLINK), DLINK, NOBJECTS, ISEG)
        ENDIF
      ENDIF
      
      IF(LINKTYPE(DLINK) .EQ. 0) THEN
        IF(LINKTYPE(ULINK) .EQ. 0) THEN
          IF(OFFRAMP_LINK(ULINK) .EQ. 0) THEN
 
! --- Mainline link to mainline link.
 
            CALL LOAD_NODE_ALIGNMENT(ULINK, DLINK, NOBJECTS, ISEG)
            
          ELSE
            CALL LOAD_NODE_ALIGNMENT(ULINK, DLINK, NOBJECTS, ISEG)
            ILOF = OFFRAMP_LINK(ULINK)
            CALL LOAD_OFFRAMP_ALIGNMENT(ULINK, ILOF, NOBJECTS, ISEG)  
          ENDIF
        ELSEIF(LINKTYPE(ULINK) .GT. 0) THEN
 
! --- Onramp link to mainline link.
 
          CALL LOAD_ONRAMP_ALIGNMENT(DLINK, ULINK, NOBJECTS, ISEG)
        ELSE
 
! --- Mainline link to offramp link.
 
          ILOFF = ULINK
        ENDIF
      ELSEIF(LINKTYPE(DLINK) .GT. 0) THEN
        IF(LINKTYPE(ULINK) .GT. 0) THEN
 
! --- Onramp link to onramp link.
 
          CALL LOAD_NODE_ALIGNMENT(ULINK, DLINK, NOBJECTS, ISEG)
        ELSE
 
! --- Mainline link to onramp link.
 
          ILINK = FTHRU_LINK(ULINK)
          CALL LOAD_NODE_ALIGNMENT(ULINK, ILINK, NOBJECTS, ISEG)
        ENDIF
      ELSEIF(LINKTYPE(DLINK) .LT. 0) THEN
        IF(LINKTYPE(ULINK) .LT. 0) THEN
 
! --- Offramp link to offramp link.
 
          CALL LOAD_NODE_ALIGNMENT(DLINK, ULINK, NOBJECTS, ISEG)
        ELSE
 
! --- Mainline link to offramp link.
 
          DLINK = ILOFF
          CALL LOAD_OFFRAMP_ALIGNMENT(ULINK, DLINK, NOBJECTS, ISEG)
        ENDIF
      ENDIF
    ENDIF
  ENDDO
 
  RETURN
  END

! ==================================================================================================
  SUBROUTINE LOAD_NODE_ALIGNMENT(ULINK, DLINK, NOBJECTS, ISEG)
! ----------------------------------------------------------------------
! --- Load objects for alignments at internal nodes.
! ----------------------------------------------------------------------
  USE OBJECTS
  USE FREEWAY_LINKS
  USE ADD_DROP_ALIGNMENTS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ULINK, DLINK, ISEG
  INTEGER, INTENT(INOUT) :: NOBJECTS
  LOGICAL :: REALIGN
  INTEGER :: ALGNS(N_FREEWAY_LANES)
! ----------------------------------------------------------------------
  CALL CHECK_NODE_ALIGNMENT(ULINK, DLINK, REALIGN)
  CALL DETERMINE_LANE_ALIGNMENTS(ULINK, DLINK, ALGNS)
  RECEIVING_LANE(ULINK, 1:N_FREEWAY_LANES) = ALGNS
  IF(REALIGN) THEN
    CALL ADD_OBJECT(NOBJECTS)
    OBJECT_LIST(NOBJECTS)%ITYPE = M_ALIGN_INTNODE
    OBJECT_LIST(NOBJECTS)%SEGMENT = ISEG
    OBJECT_LIST(NOBJECTS)%LOCATION = USN_TO_SEG_END(DLINK)
    OBJECT_LIST(NOBJECTS)%LINK = ULINK
    OBJECT_LIST(NOBJECTS)%LANE = 0
    OBJECT_LIST(NOBJECTS)%VALUE = 0
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE LOAD_OFFRAMP_ALIGNMENT(ULINK, DLINK, NOBJECTS, ISEG)
! ----------------------------------------------------------------------
! --- Load objects for lane alignments at offramps.
! ----------------------------------------------------------------------
  USE OBJECTS
  USE FREEWAY_LINKS
  USE ADD_DROP_ALIGNMENTS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ULINK, DLINK, ISEG
  INTEGER, INTENT(INOUT) :: NOBJECTS
  INTEGER :: IL, ULANES, DLANES, OAL, ILANE, IAUX, I, NL, NRAUX, NLAUX
  INTEGER :: RLANES(N_FREEWAY_LANES)
! ----------------------------------------------------------------------
  ULANES = FNUMLANES(ULINK)
  DLANES = FNUMLANES(DLINK)
  DO I = 1, 3
    ULANES = ULANES + ADDDROP_CODE(ULINK, I)
  ENDDO
  OAL = OFFRAMP_SENDING_LANE(ULINK)
  IF(DIVERGE_LINK(DLINK)) THEN
    !Treat a diverge off-ramp link as a mainline link.
    CALL DETERMINE_DIVERGE_LANE_ALIGNMENTS(ULINK, DLINK, OAL, RLANES)
    DO I = 1, N_FREEWAY_LANES
      EXIT_LANE(ULINK, I) = RLANES(I)
    ENDDO
  ELSE
    EXIT_LANE(ULINK, OAL) = OFFRAMP_RECEIVING_LANE(ULINK)
    IF(OAL .EQ. 1 .OR. OAL .GT. 15) THEN
      DO I = 2, DLANES
        IF(OAL .GT. 15) THEN
          ILANE = OAL - I + OFFRAMP_RECEIVING_LANE(ULINK)
          IF(ILANE .LE. 8) ILANE = 16 - ILANE
        ELSE
          ILANE = OAL + I - OFFRAMP_RECEIVING_LANE(ULINK)                                
        ENDIF
        IF(ILANE .GT. 15) THEN
          DO IAUX = 1, N_AUXLANES
            IF(AUX_LANE_ID(ULINK, IAUX) .EQ. 0) EXIT !no more auxiliary lanes on this link
            IF(AUX_LANE_ID(ULINK, IAUX) .EQ. ILANE) THEN
              EXIT_LANE(ULINK, ILANE) = I
              EXIT
            ENDIF
          ENDDO
        ELSE
          IF(ILANE .LE. ULANES) EXIT_LANE(ULINK, ILANE) = I
        ENDIF
      ENDDO
    ELSE
      !Consider left-side auxiliary lanes
      I = OFFRAMP_RECEIVING_LANE(ULINK)
      DO IAUX = 1, N_AUXLANES
        IF(AUX_LANE_ID(ULINK, IAUX) .EQ. 0) EXIT !no more auxiliary lanes on this link
        IF(AUX_LANE_ID(ULINK, IAUX) .GE. 11 .AND. AUX_LANE_ID(ULINK, IAUX) .LE. 15) THEN
          IF(AUX_LANE_ID(ULINK, IAUX) .EQ. OAL) CYCLE
          I = I + 1
          IF(I .GT. DLANES) EXIT
          EXIT_LANE(ULINK, AUX_LANE_ID(ULINK, IAUX)) = I
        ENDIF
      ENDDO
    ENDIF
  ENDIF
  IL = FTHRU_LINK(ULINK)
  CALL DETERMINE_LANE_ALIGNMENTS(ULINK, IL, RLANES)
  DO I = 1, N_FREEWAY_LANES
    RECEIVING_LANE(ULINK, I) = RLANES(I)
  ENDDO
 
! --- Store the lane alignment object.
 
  CALL ADD_OBJECT(NOBJECTS)
  OBJECT_LIST(NOBJECTS)%ITYPE = M_ALIGN_OFF
  OBJECT_LIST(NOBJECTS)%SEGMENT = ISEG
  OBJECT_LIST(NOBJECTS)%LOCATION = USN_TO_SEG_END(ULINK) - FLENGTH(ULINK)
  OBJECT_LIST(NOBJECTS)%LINK = ULINK
  OBJECT_LIST(NOBJECTS)%LANE = 0
  OBJECT_LIST(NOBJECTS)%VALUE = 0
  RETURN
  END

! ==================================================================================================
  SUBROUTINE LOAD_ONRAMP_ALIGNMENT(IMAIN, ILON, NOBJECTS, ISEG)
! ----------------------------------------------------------------------
! --- Load objects for lane alignments at onramps.
! ----------------------------------------------------------------------
  USE OBJECTS
  USE FREEWAY_LINKS
  USE ADD_DROP_ALIGNMENTS
  USE FREEWAY_NODES
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IMAIN, ILON, ISEG
  INTEGER, INTENT(INOUT) :: NOBJECTS
  INTEGER :: NLANES, IADP, IRLANE, IAUX, ILANE
! ----------------------------------------------------------------------
  NLANES = FNUMLANES(ILON)
  DO IADP = 1, 3
    NLANES = NLANES + ADDDROP_CODE(ILON, IADP)
  ENDDO
  IRLANE = MAINLINE_RECEIVING_LANE(ILON) - MAINLINE_SENDING_LANE(ILON) + 1
  IF(IRLANE .GT. 10) THEN
    DO ILANE = 1, NLANES
      DO IAUX = 1, N_AUXLANES
        IF(AUX_LANE_ID(IMAIN, IAUX) .EQ. 0) EXIT !no more auxiliary lanes on this link
        IF(AUX_LANE_ID(IMAIN, IAUX) .EQ. IRLANE .AND. AUX_LANE_CODE(IMAIN, IAUX) .NE. AUX_DECEL) THEN
          RECEIVING_LANE(ILON, ILANE) = IRLANE
          IF(IRLANE .GT. 15) THEN
            IRLANE = IRLANE - 1
          ELSE
            IRLANE = IRLANE + 1
          ENDIF
          EXIT
        ENDIF
      ENDDO
    ENDDO
  ELSEIF(DUMMY_ENTRY(FDSN(ILON)) .OR. DUMMY_EXIT(FDSN(ILON)) .OR. IRLANE .EQ. 1) THEN
    CALL LOAD_NODE_ALIGNMENT(ILON, IMAIN, NOBJECTS, ISEG)
  ENDIF
  CALL ADD_OBJECT(NOBJECTS)
  OBJECT_LIST(NOBJECTS)%ITYPE = M_ALIGN_ON
  OBJECT_LIST(NOBJECTS)%SEGMENT = ISEG
  OBJECT_LIST(NOBJECTS)%LOCATION = USN_TO_SEG_END(IMAIN)
  OBJECT_LIST(NOBJECTS)%LINK = ILON
  OBJECT_LIST(NOBJECTS)%LANE = 0
  OBJECT_LIST(NOBJECTS)%VALUE = 0
  RETURN
  END

! ==================================================================================================
  SUBROUTINE DETERMINE_LANE_ALIGNMENTS(ULINK, DLINK, RLANES)
! ----------------------------------------------------------------------
! --- Determine lane alignments at an internal node.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ULINK, DLINK
  INTEGER, INTENT(OUT) :: RLANES(N_FREEWAY_LANES)
  INTEGER :: I, LANES_UP(N_FREEWAY_LANES)
  INTEGER :: LANES_DOWN(N_FREEWAY_LANES)
  INTEGER :: NLUP, NLDN
  INTEGER :: IUP, IDOWN, SHIFT
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = fusn(ulink)
#endif
 
! --- Initialize arrays.
 
  LANES_UP = 0
  LANES_DOWN = 0
 
! --- Get the number of through lanes on the upstream link and the downstream link.
 
  NLUP = FNUMLANES(ULINK)
  NLDN = FNUMLANES(DLINK)
 
! --- Count lane adds and drops on the upstream link.
 
  DO I = 1, 3
    NLUP = NLUP + ADDDROP_CODE(ULINK, I)
  ENDDO
 
! --- Add existing lanes into LANES_UP and LANES_DOWN, going from right to left.

  IUP = 0
  IDOWN = 0

! --- Start with right auxiliary lanes on the upstream link.

  DO I = N_AUXLANES, 1, -1                     
    IF(AUX_LANE_ID(ULINK, I) .GT. 15 .AND. AUX_LANE_CODE(ULINK, I) .NE. AUX_ACCEL) THEN
      IUP = IUP + 1
      LANES_UP(IUP) = AUX_LANE_ID(ULINK, I)
    ENDIF
  ENDDO

! --- Add full lanes on the upstream link.

  DO I = 1, NLUP
    IUP = IUP + 1
    LANES_UP(IUP) = I
  ENDDO

! --- Add left auxiliary lanes on the upstream link.

  DO I = 1, N_AUXLANES                           
    IF(AUX_LANE_ID(ULINK, I) .EQ. 0) EXIT
    IF(AUX_LANE_ID(ULINK, I) .LE. 15 .AND. AUX_LANE_CODE(ULINK, I) .NE. AUX_ACCEL) THEN
      IUP = IUP + 1
      LANES_UP(IUP) = AUX_LANE_ID(ULINK, I)
    ENDIF
  ENDDO

! --- Start with right auxiliary lanes on the downstream link.

  DO I = N_AUXLANES, 1, -1                    
    IF(AUX_LANE_ID(DLINK, I) .GT. 15 .AND. AUX_LANE_CODE(DLINK, I) .NE. AUX_DECEL) THEN
      IDOWN = IDOWN + 1
      LANES_DOWN(IDOWN) = AUX_LANE_ID(DLINK, I)
    ENDIF
  ENDDO

! --- Add full lanes on the downstream link.

  DO I = 1, NLDN
    IDOWN = IDOWN + 1
    LANES_DOWN(IDOWN) = I
  ENDDO

! --- Add left auxiliary lanes on the downstream link.

  DO I = 1, N_AUXLANES                           
    IF(AUX_LANE_ID(DLINK, I) .EQ. 0) EXIT
    IF(AUX_LANE_ID(DLINK, I) .LE. 15 .AND. AUX_LANE_CODE(DLINK, I) .NE. AUX_DECEL) THEN
      IDOWN = IDOWN + 1
      LANES_DOWN(IDOWN) = AUX_LANE_ID(DLINK, I)
    ENDIF
  ENDDO
 
! --- Find the index to sending lane on the upstream link.
 
  DO IUP = 1, N_FREEWAY_LANES
    IF(LANES_UP(IUP) .EQ. MAINLINE_SENDING_LANE(ULINK)) EXIT
  ENDDO
 
! --- Find the index to the receiving lane on the downstream link.
 
  DO IDOWN = 1, N_FREEWAY_LANES
    IF(LANES_DOWN(IDOWN) .EQ. MAINLINE_RECEIVING_LANE(ULINK)) EXIT
  ENDDO
 
! --- Determine if there is a lane shift from lane 1 on the upstream link
! --- to lane 1 on the downstream link.
 
  SHIFT = IDOWN - IUP
  IF(IDOWN .GE. IUP) THEN
    IUP = 1
  ELSE
    IUP = 1 - SHIFT
  ENDIF
 
! --- Store the downstream lanes that receive traffic from the upstream lanes.

  RLANES = 0
  DO I = IUP, N_FREEWAY_LANES
    IF(LANES_UP(I) .EQ. 0) EXIT
    RLANES(LANES_UP(I)) = LANES_DOWN(I + SHIFT)
  ENDDO
 
  RETURN
  END

! ==================================================================================================
  SUBROUTINE DETERMINE_DIVERGE_LANE_ALIGNMENTS(ULINK, DLINK, OAL, RLANES)
! ----------------------------------------------------------------------
! --- Determine lane alignments at an internal node to a diverge link.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ULINK, DLINK, OAL
  INTEGER, INTENT(OUT) :: RLANES(N_FREEWAY_LANES)
  INTEGER :: I, LANES_UP(N_FREEWAY_LANES)
  INTEGER :: LANES_DOWN(N_FREEWAY_LANES)
  INTEGER :: NLUP, NLDN
  INTEGER :: IUP, IDOWN, SHIFT
! ----------------------------------------------------------------------
#ifdef DebugVersion
  integer :: temp
  temp = fusn(ulink)
#endif
 
! --- Initialize arrays.
 
  LANES_UP = 0
  LANES_DOWN = 0
 
! --- Get the number of through lanes on the upstream link and the downstream link.
 
  NLUP = FNUMLANES(ULINK)
  NLDN = FNUMLANES(DLINK)
 
! --- Count lane adds and drops on the upstream link.
 
  DO I = 1, 3
    NLUP = NLUP + ADDDROP_CODE(ULINK, I)
  ENDDO
 
! --- Add existing lanes into LANES_UP and LANES_DOWN, going from right to left.

  IUP = 0
  IDOWN = 0

! --- Start with right auxiliary lanes on the upstream link.

  DO I = N_AUXLANES, 1, -1                     
    IF(AUX_LANE_ID(ULINK, I) .GT. 15 .AND. AUX_LANE_CODE(ULINK, I) .NE. AUX_ACCEL) THEN
      IUP = IUP + 1
      LANES_UP(IUP) = AUX_LANE_ID(ULINK, I)
    ENDIF
  ENDDO

! --- Add full lanes on the upstream link.

  DO I = 1, NLUP
    IUP = IUP + 1
    LANES_UP(IUP) = I
  ENDDO

! --- Add left auxiliary lanes on the upstream link.

  DO I = 1, N_AUXLANES                           
    IF(AUX_LANE_ID(ULINK, I) .EQ. 0) EXIT
    IF(AUX_LANE_ID(ULINK, I) .LE. 15 .AND. AUX_LANE_CODE(ULINK, I) .NE. AUX_ACCEL) THEN
      IUP = IUP + 1
      LANES_UP(IUP) = AUX_LANE_ID(ULINK, I)
    ENDIF
  ENDDO

! --- Start with right auxiliary lanes on the downstream link.

  DO I = N_AUXLANES, 1, -1                    
    IF(AUX_LANE_ID(DLINK, I) .GT. 15 .AND. AUX_LANE_CODE(DLINK, I) .NE. AUX_DECEL) THEN
      IDOWN = IDOWN + 1
      LANES_DOWN(IDOWN) = AUX_LANE_ID(DLINK, I)
    ENDIF
  ENDDO

! --- Add full lanes on the downstream link.

  DO I = 1, NLDN
    IDOWN = IDOWN + 1
    LANES_DOWN(IDOWN) = I
  ENDDO

! --- Add left auxiliary lanes on the downstream link.

  DO I = 1, N_AUXLANES                           
    IF(AUX_LANE_ID(DLINK, I) .EQ. 0) EXIT
    IF(AUX_LANE_ID(DLINK, I) .LE. 15 .AND. AUX_LANE_CODE(DLINK, I) .NE. AUX_DECEL) THEN
      IDOWN = IDOWN + 1
      LANES_DOWN(IDOWN) = AUX_LANE_ID(DLINK, I)
    ENDIF
  ENDDO
 
! --- Find the index to lane OAL on the upstream link.
 
  DO IUP = 1, N_FREEWAY_LANES
    IF(LANES_UP(IUP) .EQ. OAL) EXIT
  ENDDO
 
! --- Find the index to lane 1 on the downstream link.
 
  DO IDOWN = 1, N_FREEWAY_LANES
    IF(LANES_DOWN(IDOWN) .EQ. 1) EXIT
  ENDDO
 
! --- Determine if there is a lane shift from lane 1 on the upstream link
! --- to lane 1 on the downstream link.
 
  SHIFT = IDOWN - IUP
  IF(IDOWN .GE. IUP) THEN
    IUP = 1
  ELSE
    IUP = 1 - SHIFT
  ENDIF
 
! --- Store the downstream lanes that receive traffic from the upstream lanes.

  RLANES = 0
  DO I = IUP, N_FREEWAY_LANES
    IF(LANES_UP(I) .EQ. 0) EXIT
    RLANES(LANES_UP(I)) = LANES_DOWN(I + SHIFT)
  ENDDO
 
  RETURN
  END

! ==================================================================================================
  SUBROUTINE CHECK_NODE_ALIGNMENT(ULINK, DLINK, REALIGN)
! ----------------------------------------------------------------------
! --- Determine if there is a lane realignment from the upstream link to
! --- the downstream link.
! ----------------------------------------------------------------------
  USE FREEWAY_LINKS
  USE FREEWAY_NODES
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ULINK, DLINK
  LOGICAL, INTENT(OUT) :: REALIGN
  INTEGER :: I, J, NLANES, AUXID
  LOGICAL :: FOUND
! ----------------------------------------------------------------------
  REALIGN = .FALSE.
  IF(DUMMY_ENTRY(FDSN(ULINK)) .OR. DUMMY_EXIT(FDSN(ULINK))) THEN
    REALIGN = .TRUE.

! --- There is a realignment if the specified through alignment lane
! --- is not 1.
 
  ELSEIF(MAINLINE_RECEIVING_LANE(ULINK) .NE. MAINLINE_SENDING_LANE(ULINK)) THEN
    REALIGN = .TRUE.
 
  ELSEIF(FNUMLANES(DLINK) .NE. 0) THEN
 
! --- Determine the number of through lanes at the downstream end of
! --- the upstream link.
 
    NLANES = FNUMLANES(ULINK)
    DO I = 1, 3 
      NLANES = NLANES + ADDDROP_CODE(ULINK, I)
    ENDDO
 
! --- There is a realignment if the number of through lanes at the 
! --- downstream end of the upstream link doesn't equal the number
! --- of through lanes at the upstream end of the downstream link.
 
    IF(NLANES .NE. FNUMLANES(DLINK)) THEN
      REALIGN = .TRUE.
    ELSE
 
! --- There is a realignment if the full auxiliary lanes on the
! --- upstream link don't match the full auxiliary lanes on the
! --- downstream link.
 
      DO I = 1, N_AUXLANES
        IF(AUX_LANE_ID(ULINK, I) .EQ. 0) EXIT !no more auxiliary lanes on this link
        IF(AUX_LANE_CODE(ULINK, I) .EQ. AUX_FULL) THEN
          FOUND = .FALSE.
          AUXID = AUX_LANE_ID(ULINK, I)
          DO J = 1, N_AUXLANES
            IF(AUX_LANE_ID(DLINK, J) .EQ. 0) EXIT !no more auxiliary lanes on this link
            IF(AUX_LANE_CODE(DLINK, J) .EQ. AUX_FULL .AND. AUX_LANE_ID(DLINK, J) .EQ. AUXID) FOUND = .TRUE.
          ENDDO
          IF(.NOT. FOUND) REALIGN = .TRUE.
        ENDIF
      ENDDO
    ENDIF
  ENDIF
  RETURN
  END

! ==================================================================================================
  SUBROUTINE LOAD_OFFRAMP_WARNING(ISEG, NOBJECTS)
! ----------------------------------------------------------------------
! --- Load objects for offramp warning signs.
! ----------------------------------------------------------------------
  USE SEGMENTS
  USE FREEWAY_LINKS
  USE FREEWAY_NODES
  USE OBJECTS
  USE ADD_DROP_ALIGNMENTS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ISEG
  INTEGER, INTENT(INOUT) :: NOBJECTS
  INTEGER :: I1, I2, ULINK, IL, IL2
  INTEGER :: INODE, SIGNPOS, MAXPOS, GPOS, NMAX
! ----------------------------------------------------------------------
  NMAX = NUMBER_OF_LINKS_IN_SEGMENT(ISEG)
  MAXPOS = USN_TO_SEG_END(LINKS_IN_SEGMENT(ISEG, NMAX)) 
  DO I1 = 1, NMAX
    IL = LINKS_IN_SEGMENT(ISEG, I1)
 
! --- Check if there is an off-ramp link.
 
    IF(LINKTYPE(IL) .LT. 0) THEN
      
! --- Check if this is the most upstream off-ramp link.
 
      IF(LINKTYPE(LINKS_IN_SEGMENT(ISEG, I1-1)) .EQ. 0) THEN
 
! --- Get the link ID number of the off-ramp and test the validity
! --- of the warning sign position.
 
        ULINK = MAINLINE_APPROACH(FUSN(IL))

        SIGNPOS = MAXPOS
        IF(SIGNPOS .GE. USN_TO_SEG_END(ULINK) - FLENGTH(ULINK) + OFFRAMP_WARN_DISTANCE(ULINK)) THEN
          SIGNPOS = USN_TO_SEG_END(ULINK) - FLENGTH(ULINK) + OFFRAMP_WARN_DISTANCE(ULINK)
        ENDIF
        OFFRAMP_WARN_DISTANCE(ULINK) = SIGNPOS
 
        SIGNPOS = MAXPOS
        IF(SIGNPOS .GE. USN_TO_SEG_END(ULINK) - FLENGTH(ULINK) + HOV_OFFRAMP_WARN_DISTANCE(ULINK)) THEN
          SIGNPOS = USN_TO_SEG_END(ULINK) - FLENGTH(ULINK) + HOV_OFFRAMP_WARN_DISTANCE(ULINK)
        ENDIF
        HOV_OFFRAMP_WARN_DISTANCE(ULINK) = SIGNPOS
 
! --- Loop over all links to check if there are any on-ramps between
! --- this off-ramp and its associated warning sign location. If so,
! --- load a warning sign in the geometry list.
 
        DO I2 = I1, NUMBER_OF_LINKS_IN_SEGMENT(ISEG)
          IL2 = LINKS_IN_SEGMENT(ISEG, I2)
          IF(LINKTYPE(IL2) .GT. 0 .AND. .NOT. DUMMY_ENTRY(FDSN(IL2))) THEN
                                                                       
! --- Set a warning sign at the on-ramp gore.                     
                                                                       
            IF(LINKTYPE(FTHRU_LINK(IL2)) .EQ. 0) THEN
              GPOS = USN_TO_SEG_END(LINKS_IN_SEGMENT(ISEG, I2 - 1))
              IF(OFFRAMP_WARN_DISTANCE(ULINK) .GE. GPOS) THEN
                CALL LOAD_OBJECT(ISEG, ULINK, 0, 0, GPOS - 1, NOBJECTS, M_WARN_EXIT)
              ENDIF
              IF(HOV_OFFRAMP_WARN_DISTANCE(ULINK) .GE. GPOS) THEN
                CALL LOAD_OBJECT(ISEG, ULINK, 0, 0, GPOS - 1, NOBJECTS, M_HOV_WARN_EXIT)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
 
! --- Set the warning sign on the main line.
 
        SIGNPOS = OFFRAMP_WARN_DISTANCE(ULINK)
        IF(SIGNPOS .EQ. MAXPOS) SIGNPOS = SIGNPOS - 1
        CALL LOAD_OBJECT(ISEG, ULINK, 0, 0, SIGNPOS, NOBJECTS, M_WARN_EXIT)

        SIGNPOS = HOV_OFFRAMP_WARN_DISTANCE(ULINK)
        IF(SIGNPOS .EQ. MAXPOS) SIGNPOS = SIGNPOS - 1
        CALL LOAD_OBJECT(ISEG, ULINK, 0, 0, SIGNPOS, NOBJECTS, M_HOV_WARN_EXIT)
      ENDIF
    ELSE
      IF(OFFRAMP_LINK(IL) .NE. 0 .AND. I1 .GT. 1) THEN
        IF(LINKTYPE(LINKS_IN_SEGMENT(ISEG, I1-1)) .EQ. 0) THEN

! --- Upstream end of the off-ramp is found. Get the link ID number of
! --- the off-ramp and test the validity of the warning sign position.

          SIGNPOS = MAXPOS
          IF(SIGNPOS .GE. USN_TO_SEG_END(FTHRU_LINK(IL)) + OFFRAMP_WARN_DISTANCE(IL)) THEN
            SIGNPOS = USN_TO_SEG_END(FTHRU_LINK(IL)) + OFFRAMP_WARN_DISTANCE(IL)
          ENDIF
          OFFRAMP_WARN_DISTANCE(IL) = SIGNPOS

          SIGNPOS = MAXPOS
          IF(SIGNPOS .GE. USN_TO_SEG_END(FTHRU_LINK(IL)) + HOV_OFFRAMP_WARN_DISTANCE(IL)) THEN
            SIGNPOS = USN_TO_SEG_END(FTHRU_LINK(IL)) + HOV_OFFRAMP_WARN_DISTANCE(IL)
          ENDIF
          HOV_OFFRAMP_WARN_DISTANCE(IL) = SIGNPOS

! --- Check if there are any on-ramps between this off-ramp and its
! --- associated warning sign location. If so, load a warning sign in
! --- the geometry list.

          DO I2 = I1 - 1, NUMBER_OF_LINKS_IN_SEGMENT(ISEG)
            IL2 = LINKS_IN_SEGMENT(ISEG, I2)

! --- Test if this is an on-ramp link.

            IF(LINKTYPE(IL2) .GT. 0) THEN

! --- Check if this on-ramp link is the last one in its roadway.

              IF(LINKTYPE(FTHRU_LINK(IL2)) .EQ. 0) THEN

! --- Set a warning sign at the on-ramp gore. Compute the on-ramp
! --- gore position.

                GPOS = USN_TO_SEG_END(IL2) - FLENGTH(IL2)
                IF(OFFRAMP_WARN_DISTANCE(IL) .GT. GPOS) THEN
                  INODE = FUSN(FTHRU_LINK(IL2))
                  CALL LOAD_OBJECT(ISEG, FTHRU_LINK(IL2), 0, 0, GPOS - 1, NOBJECTS, M_WARN_EXIT)
                ENDIF
                IF(HOV_OFFRAMP_WARN_DISTANCE(IL) .GT. GPOS) THEN
                  INODE = FUSN(FTHRU_LINK(IL2))
                  CALL LOAD_OBJECT(ISEG, FTHRU_LINK(IL2), 0, 0, GPOS - 1, NOBJECTS, M_HOV_WARN_EXIT)
                ENDIF
              ENDIF
            ENDIF
          ENDDO

! --- Set the warning sign on the main line.

          INODE = FDSN(IL)
          SIGNPOS = OFFRAMP_WARN_DISTANCE(IL)
          IF(SIGNPOS .EQ. MAXPOS) SIGNPOS = SIGNPOS - 1
          CALL LOAD_OBJECT(ISEG, MAINLINE_APPROACH(INODE), 0, 0, SIGNPOS, NOBJECTS, M_WARN_EXIT)

          SIGNPOS = HOV_OFFRAMP_WARN_DISTANCE(IL)
          IF(SIGNPOS .EQ. MAXPOS) SIGNPOS = SIGNPOS - 1
          CALL LOAD_OBJECT(ISEG, MAINLINE_APPROACH(INODE), 0, 0, SIGNPOS, NOBJECTS, M_HOV_WARN_EXIT)

        ENDIF
      ENDIF
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE LOAD_ANTICIP_WARNING(ISEG, NOBJECTS)
! ----------------------------------------------------------------------
! --- Load objects for anticipatory lane change warning signs.
! ----------------------------------------------------------------------
  USE SEGMENTS
  USE FREEWAY_LINKS
  USE FREEWAY_NODES
  USE NODE_TABLE
  USE OBJECTS
  USE ADD_DROP_ALIGNMENTS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ISEG
  INTEGER, INTENT(INOUT) :: NOBJECTS
  INTEGER :: I, IL, ILON, ILANE
  INTEGER :: ZMAX, ZPOS
! ----------------------------------------------------------------------
  ZMAX = USN_TO_SEG_END(LINKS_IN_SEGMENT(ISEG, NUMBER_OF_LINKS_IN_SEGMENT(ISEG))) 
  DO I = 1, NUMBER_OF_LINKS_IN_SEGMENT(ISEG)
    IL = LINKS_IN_SEGMENT(ISEG, I)
    IF(LINKTYPE(IL) .EQ. 0 .AND. NODE_TYPE(FUSN(IL)) .EQ. NT_INTERN) THEN
      IF(RAMP_APPROACH(FUSN(IL)) .NE. 0) THEN
        ILON = RAMP_APPROACH(FUSN(IL))
        ILANE = 0
        IF(RECEIVING_LANE(ILON, 1) .GE. 16) ILANE = 1
        ZPOS = USN_TO_SEG_END(IL) + ANTICIP_WARNING_DISTANCE(IL)
        ZPOS = MIN(ZPOS, ZMAX)
        CALL LOAD_OBJECT(ISEG, IL, ILANE, ILON, ZPOS, NOBJECTS, M_WARN_ANTICIP)   
        ZPOS = USN_TO_SEG_END(IL) - FLENGTH(IL)
        CALL LOAD_OBJECT(ISEG, IL, ILANE, 0, ZPOS, NOBJECTS, M_END_ANTICIP)   
      ENDIF
    ENDIF
  ENDDO
  RETURN
  END

! ==================================================================================================
  SUBROUTINE LOAD_MERGE_POINT(IL, ISEG, NOBJECTS)
! ----------------------------------------------------------------------
! --- Load objects for merge points.
! ----------------------------------------------------------------------
  USE OBJECTS
  USE FREEWAY_LINKS
  USE FREEWAY_NODES
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IL, ISEG
  INTEGER, INTENT(INOUT) :: NOBJECTS
  INTEGER :: IAUX, LANE
! ----------------------------------------------------------------------
  CALL ADD_OBJECT(NOBJECTS)
  OBJECT_LIST(NOBJECTS)%ITYPE = M_MERGE_POINT
  OBJECT_LIST(NOBJECTS)%SEGMENT = ISEG
  OBJECT_LIST(NOBJECTS)%LOCATION = USN_TO_SEG_END(IL) - FLENGTH(IL)
  OBJECT_LIST(NOBJECTS)%LINK = IL
  IF(RAMP_MERGE_LINK(IL)) THEN
    !Find the far left lane.
    LANE = FNUMLANES(IL)
    DO IAUX = 1, N_AUXLANES
      IF(AUX_LANE_CODE(IL, IAUX) .EQ. 0) EXIT
      IF(AUX_LANE_CODE(IL, IAUX) .EQ. AUX_DECEL) CYCLE
      IF(AUX_LANE_ID(IL, IAUX) .GE. 11 .AND. AUX_LANE_ID(IL, IAUX) .LE. 15) LANE = AUX_LANE_ID(IL, IAUX)
    ENDDO
  ELSE
    !Find the far right lane.
    LANE = FNUMLANES(IL)
    DO IAUX = 1, N_AUXLANES
      IF(AUX_LANE_CODE(IL, IAUX) .EQ. 0) EXIT
      IF(AUX_LANE_CODE(IL, IAUX) .EQ. AUX_DECEL) CYCLE
      IF(AUX_LANE_ID(IL, IAUX) .GE. 16) LANE = AUX_LANE_ID(IL, IAUX)
    ENDDO
  ENDIF
  OBJECT_LIST(NOBJECTS)%LANE = LANE
  OBJECT_LIST(NOBJECTS)%VALUE = FDSN(IL)
  RETURN
  END

! ==================================================================================================
  SUBROUTINE SORT_BY_LOCATION(ISEG, START, NOBJECTS)
! ----------------------------------------------------------------------
! --- Sort objects by location.
! ----------------------------------------------------------------------
  USE OBJECTS
  USE SEGMENTS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ISEG, START, NOBJECTS
  INTEGER :: IEND, I
  INTEGER, SAVE :: LAST
  TYPE(OBJECT) :: TEMP
  LOGICAL :: SWAPPED
! ----------------------------------------------------------------------
 
! --- Sort the objects in each segment in ascending order.
 
  IF(ISEG .EQ. 1) LAST = 0         !Needed for multiple runs in the same memory space
  
  DO
    SWAPPED = .FALSE.
    IEND = START + NOBJECTS - LAST - 1
    DO I = START, IEND - 1
      IF(OBJECT_LIST(I)%LOCATION .GT. OBJECT_LIST(I+1)%LOCATION) THEN
 
        TEMP = OBJECT_LIST(I)
        OBJECT_LIST(I) = OBJECT_LIST(I+1)
        OBJECT_LIST(I+1) = TEMP
 
        SWAPPED = .TRUE.
        IEND = I
      ENDIF
    ENDDO
    IF(.NOT. SWAPPED) EXIT
  ENDDO
  LAST = NOBJECTS
 
  RETURN
  END
      
! ==================================================================================================
  SUBROUTINE SORT_BY_TYPE(ISEG, START, NOBJECTS)
! ----------------------------------------------------------------------
! --- Sort objects by type.
! ----------------------------------------------------------------------
  USE OBJECTS
  USE SEGMENTS
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ISEG, START, NOBJECTS
  INTEGER :: IEND, I
  INTEGER, SAVE :: LAST
  TYPE(OBJECT) :: TEMP
  LOGICAL :: SWAPPED
! ----------------------------------------------------------------------
 
! --- Sort the objects in each segment in ascending order.
 
  IF(ISEG .EQ. 1) LAST = 0         !Needed for multiple runs in the same memory space
  
  DO
    SWAPPED = .FALSE.
    IEND = START + NOBJECTS - LAST - 1
    DO I = START, IEND - 1
      IF(OBJECT_LIST(I)%LOCATION .EQ. OBJECT_LIST(I+1)%LOCATION) THEN
        IF(OBJECT_LIST(I)%ITYPE .GT. OBJECT_LIST(I+1)%ITYPE) THEN
 
          TEMP = OBJECT_LIST(I)
          OBJECT_LIST(I) = OBJECT_LIST(I+1)
          OBJECT_LIST(I+1) = TEMP
 
          SWAPPED = .TRUE.
          IEND = I
        ENDIF
      ENDIF
    ENDDO
    IF(.NOT. SWAPPED) EXIT
  ENDDO
  LAST = NOBJECTS
 
  RETURN
  END   

! ==================================================================================================
  SUBROUTINE LOCATE_NODE_OBJECTS
  USE OBJECTS
  USE SEGMENTS
  USE FREEWAY_NODES
  IMPLICIT NONE
  INTEGER :: IOBJ, ITYPE
! ----------------------------------------------------------------------
  DO IOBJ = 1, LAST_OBJECT(NUMBER_OF_SEGMENTS)
    ITYPE = OBJECT_LIST(IOBJ)%ITYPE
    IF(ITYPE .EQ. M_NODE_INT .OR. ITYPE .EQ. M_NODE_ENTRY .OR. ITYPE .EQ. M_NODE_EXIT &
      .OR. ITYPE .EQ. M_ENTRY_INTERFACE .OR. ITYPE .EQ. M_EXIT_INTERFACE) THEN
      NODE_OBJECT(OBJECT_LIST(IOBJ)%VALUE) = IOBJ
    ENDIF
  ENDDO
  RETURN
  END
  
  SUBROUTINE GET_NEXT_EXTERNAL_NODE_ID(INODE)
  USE NODE_TABLE
  USE GLOBAL_DATA
  IMPLICIT NONE
  INTEGER, INTENT(OUT) :: INODE
  INTEGER :: I
! ----------------------------------------------------------------------
  DO I = MAX_NODE_NUMBER + 1, MAX_NODE_NUMBER + 1000
    IF(IS_USED(I)) CYCLE
    INODE = I
    IS_USED(I) = .TRUE.
    NODE_TYPE(I) = NT_EXTERN
    EXIT
  ENDDO
  RETURN
  END  
