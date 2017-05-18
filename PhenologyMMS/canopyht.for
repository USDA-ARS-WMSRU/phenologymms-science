!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   C A N O P Y H T              *
!  *                                                      gm   1/29/05 *
!  *                                                      de   7/14/05 *
!  *                                                                   *
!  *********************************************************************
!  The CANOPYHT subroutine calculates the height of the canopy for
!  different crops by calling the appropriate subroutine for each crop.               

!  INPUTS:  antss(R), canht(R), cname(R), dummy2(R), ems(R), gddday(R), 
!           gdde(R), joints(R), maxht(R)           

!  OUTPUTS: canht(C,R), 

      subroutine canopyht(antss, canht, cname, dummy2, ems, gddday, 
     c                     gdde, ies, joints, lf4s, maxht)

      implicit none
      
      integer  antss(4), ems(4), ies(4), joints(4), lf4s(4), daynum
      
      real  canht, dummy2(15), gddday, gdde, maxht

      character *22  cname


!  Call appropriate crop canopy subroutine to calculate height
      if (cname .eq. 'Dry Beans') then

	     call canopybn(antss, canht, dummy2, ems, gddday, gdde, 
     c  lf4s, maxht)
     
      elseif (cname .eq. 'Corn') then

           call canopycn(antss, canht, dummy2, ems, gddday, gdde,   
     c ies, maxht)  
           
      elseif (cname .eq. 'Hay Millet') then

           call canopyhm(antss, canht, dummy2, ems, gddday, gdde,   
     c joints, maxht)   
		  
      elseif (cname .eq. 'Proso Millet') then

           call canopypm(antss, canht, dummy2, ems, gddday, gdde,   
     c joints, maxht)

	elseif (cname .eq. 'Sorghum') then

	     call canopysg(antss, canht, dummy2, ems, gddday, gdde,   
     c joints, maxht)

	elseif (cname .eq. 'Spring Barley') then

	     call canopysb(antss, canht, dummy2, ems, gddday, gdde,   
     c joints, maxht)

	elseif (cname .eq. 'Spring Wheat') then

	     call canopysw(antss, canht, dummy2, ems, gddday, gdde,   
     c joints, maxht)

	elseif (cname .eq. 'Sunflower') then

	     call canopysf(antss, canht, dummy2, ems, gddday, gdde, 
     c  lf4s, maxht)

	elseif (cname .eq. 'Winter Barley') then

	     call canopywb(antss, canht, dummy2, ems, gddday, gdde,   
     c joints, maxht)

      elseif (cname .eq. 'Winter Wheat') then

	     call canopyww(antss, canht, dummy2, ems, gddday, gdde,   
     c joints, maxht)

	endif
      
      return
      end