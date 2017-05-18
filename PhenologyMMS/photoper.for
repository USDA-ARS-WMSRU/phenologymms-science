!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   P H O T O P E R              *
!  *                                                      gm   1/31/05 *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
!  The PHOTOPER subroutine 

!  INPUTS:  daynum(R), hemisp(R), vernal(C), verns(C)          

!  OUTPUTS: vernal(C), verns(C)

      subroutine photoper(daynum, hemisp, vernal, verns)

      implicit none

      integer  daynum, verns
      
      real  vernal
      
      character *22  hemisp

! Assume that plant is fully vernalized after 1 Jan (northern hemisphere)
! or 1 July (southern hemisphere, ignore leap years for now):

      if (hemisp .eq. 'north' .and. daynum .eq. 1) then

	     vernal = 1.0
	     verns = daynum
	     print *, 'verns = ', verns

	elseif (hemisp .eq. 'south' .and. daynum .eq. 182) then

	     vernal = 1.0
	     verns = daynum
	     print *, 'verns = ', verns

	endif
      
      return
      end