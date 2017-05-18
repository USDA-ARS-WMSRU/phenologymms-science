!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   C A N O P Y S F              *
!  *                                                       de  7/15/05 *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
! This subroutine calculates the canopy height of sunflower in two stages.  
! The first stage is from emergence to 8 cm (ecanht) in height within 400
! GDD (gdds1). The second stage is from the 4th leaf stage and the start 
! of anthesis.

! NOTE: Get the proper growth stages for gdds2 = ...

!  INPUTS:  antss(R), canht(C,R), dummy2(15)(R), ems(R), gddday(R), 
!           gdde(R), joints(R), maxht(R)

!  OUTPUTS: canht(C,R)

      subroutine canopysf(antss, canht, dummy2, ems, gddday, gdde, 
     c  lf4s, maxht)

      implicit none

      integer  antss(4), ems(4), lf4s(4)
      
      real  canht, dummy2(15), gddday, gdde, maxht
      
!  Local Variables      
	real hrate1, ecanht, hrate2, gdds2, gdds1 !emgdd, 
	
!  initialize local variables
              !check if these are the right dummy2 variables
       !gdds1 = gdd for Stage 1        
      gdds1 = (dummy2(2))! + dummy2(3) + dummy2(4))
      ecanht = 15.
      gdds2 = 0.

!  Stage 1 - emergence to 8 cm (ecanht) in height within 400
!    GDD (gdds1).

!  If emergence has occurred, grow plant to 4 leaf stage (=15 cm):
      if (ems(1).ne.999.and.gdde .lt. gdds1) then
!  Calculate the growth rate for stage 1
            hrate1 = ecanht / gdds1
      	    canht = canht + hrate1*gddday
!  Don't allow canopy to be greater than	
!    maximum canopy height for emergence growth stage.   
	    if(canht .gt. ecanht) canht = ecanht  
	    
!  Stage 2 - between 4 leaf and the start of anthesis
	elseif (lf4s(1) .ne. 999 .and. antss(1) .eq. 999) then
!  Add the growth stages gdd from jointing to anthesis start	
         ! check if these are the correct dummy2 variables
          gdds2 = dummy2(3) + dummy2(4) + dummy2(5)!de add
     c             + dummy2(6)+ dummy2(7)+ dummy2(8)+ dummy2(9)
     
!  Calculate the growth rate for this phase of canopy ht. growth
          hrate2 = (maxht - ecanht) / gdds2
          canht = canht + hrate2 * gddday
!         Don't let canopy height exceed maximum potential height:          	
          if (canht .gt. maxht) canht = maxht	    

	endif
    
      return
      end