!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   C A N O P Y C N              *
!  *                                                      de   7/15/05 *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
! This subroutine calculates the canopy height of corn in two stages.  
! The first stage is  *** FINISH THIS DESCRIPTION ***

! NOTE: Get the proper growth stages for gdds2 = ...

!  INPUTS:  antss(R), canht(C,R), dummy2(15)(R), ems(R), gddday(R), 
!           gdde(R), joints(R), maxht(R)

!  OUTPUTS: canht(C,R)

      subroutine canopycn(antss, canht, dummy2, ems, gddday, gdde,   
     c ies, maxht)

      implicit none

      integer  antss(4), ems(4), ies(4)
      
      real  canht, dummy2(15), gddday, gdde, maxht

!  Local Variables
	real hrate1, ecanht, hrate2, gdds2, gdds1
	
!  initialize local variables
      gdds1 = (dummy2(2) + dummy2(5)) !gdds1 = gdd for Stage 1            
      ecanht = 15.
      gdds2 = 0.  ! gdds = sum of GDD for growth stages in Stage 2

!  Growth Stage 1 - emergence to 15 cm (ecanht) in height within summed
!    GDD (gdds1).

!  If emergence has occurred, grow plant to 4th leaf stage from height (=15 cm):
      if (ems(1).ne.999.and.gdde .lt. gdds1) then
!  Calculate the growth rate for stage 1
            hrate1 = ecanht / gdds1
      	    canht = canht + hrate1*gddday
! Don't allow canopy to be greater than	
!    maximum canopy height for emergence growth stage.   
	    if(canht .gt. ecanht) canht = ecanht  
	    
!  Growth Stage 2 - between internode elongation and the start of anthesis
	elseif (ies(1) .ne. 999 .and. antss(1) .eq. 999) then
!  Add the growth stages gdd from internode elongation to anthesis start
	
! with corn's correct growth stages: de changed 8/15, 10/27
          gdds2 = ((dummy2(6) + dummy2(7)) - (dummy2(5))) 

!  Calculate the growth rate for this phase of canopy ht. growth
          hrate2 = (maxht - ecanht) / gdds2
          canht = canht + hrate2 * gddday
!         Don't let canopy height exceed maximum potential height:          	
          if (canht .gt. maxht) canht = maxht	    

	endif
      
      return
      end