!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   C A N O P Y W W              *
!  *                                                      gm   1/29/05 *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
!  The CANOPYWW subroutine calculates the plant height of winter wheat.
!
!  The approach used is to initially grow the plant from emergence to the
!  rosette form (assume that rosette height is achieved by the time the 
!  fourth leaf is appearing = 400 GDD).  The rosette form is maintained
!  over winter, and when pseudojointing is reached, the leaves/plant becomes
!  taller.  However, for simplification, it is not assumed that plant height
!  increases until the beginning of jointing (which occurs shortly after
!  internode elongation begins and pushes the first node above the soil
!  surface).  Even though plant height via internode elongation continues
!  for a short period after anthesis starts (maybe up to about 2 weeks),
!  assume that final plant height is reached at anthesis.
!
!  Data for determining canopy height increase (in cm) per degree-day was
!  derived from 2-yr study of 12 winter wheat cultivars under irrigated
!  and dryland conditions at ARDEC in Fort Collins, Colorado.  Details of
!  the study (but not plant height data) can be found in McMaster and
!  Wilhelm, 2003, J. Agric. Sci. Camb. 141:129-147.  Based on potential
!  height under "optimal" conditions for that variety/ht class, it can then
!  be reduced if water stress indicator is present.

!  INPUTS:  antss(R), canht(C,R), dummy2(15)(R), ems(R), gddday(R), 
!           gdde(R), joints(R), maxht(R)

!  OUTPUTS: canht(C,R)

      subroutine canopyww(antss, canht, dummy2, ems, gddday, gdde,   
     c joints, maxht)

      implicit none

      integer  antss(4), ems(4), joints(4)
      
      real  canht, dummy2(15), gddday, gdde, maxht
      
!  Local Variables      
	real hrate1, ecanht, hrate2, gdds2, gdds1 !emgdd, 
	
!  initialize local variables
      gdds1 = 400.  !gdds1 = gdd for Stage 1
      ecanht = 8.
      gdds2 = 0.

!  Stage 1 - emergence to 8 cm (ecanht) in height within 400
!    GDD (gdds1).

!  If emergence has occurred, grow plant into rosette form height (=8 cm):
      if (ems(1).ne.999.and.gdde .lt. gdds1) then
!  Calculate the growth rate for stage 1
            hrate1 = ecanht / gdds1
      	    canht = canht + hrate1*gddday
!  Don't allow canopy to be greater than	
!    maximum canopy height for emergence growth stage.   
	    if(canht .gt. ecanht) canht = ecanht  
	    
!  Stage 2 - between jointing and the start of anthesis
	elseif (joints(1) .ne. 999 .and. antss(1) .eq. 999) then
!  Add the growth stages gdd from jointing to anthesis start	
          gdds2 = dummy2(7) + dummy2(8) + dummy2(9)
!  Calculate the growth rate for this phase of canopy ht. growth
          hrate2 = (maxht - ecanht) / gdds2
          canht = canht + hrate2 * gddday
!         Don't let canopy height exceed maximum potential height:          	
          if (canht .gt. maxht) canht = maxht	    

	endif
      
      return
      end