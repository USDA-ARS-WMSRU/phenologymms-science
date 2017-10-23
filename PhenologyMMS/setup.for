!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   S E T U P                    *
!  *                                                                   *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
!  The SETUP subroutine handles initial inputs and prepares to run the
!  simulation.  Input and output files are identified and opened.

!  INPUTS: cname(R), day(R), dummy1(15)(R), dummy2(15)(C,R), elrate(R),
!          ergdd(4)(C), germd(R), germgdd(4)(C), gmethod(R), 
!          latitude(R), maxht(R), mo(R), noseeds(R), pchron(C,R), 
!          pdate(R), pdepth(C,R), pequation(R), pmethod(R), seedsw(C),
!          soilwat(4)(C), swtype(R), tbase(R), topt(R), tupper(R), 
!          vname(R), weather(R), wfpslo(4)(C), wfpsup(4)(C), wlow(R),
!          wup(R), year(R)
                   
! OUTPUTS: dummy2(15)(C,R), ergdd(4)(C), germgdd(4)(C), pchron(C,R), 
!          pdepth(C,R), seedsw(C), soilwat(4)(C), wfpslo(4)(C), 
!          wfpsup(4)(C) 

      subroutine setup(cname, devern, dummy1, dummy2, ecanht, elrate, 
     c ergdd, gdds1, gdds2, germd, germgdd, gmethod, latitude, maxht, 
     c noseeds, p1v, pchron, pdate, pday, pdepth, pequation, pmethod, 
     c pmo, pyear, seedsw, soilwat, swtype, tbase, tempsw, toptlo, 
     c toptup, tupper, vname, vtbase, vtoptlo, vtoptup, vtupper, 
     c weather,  wfpslo, wfpsup, wlow, wup)
  
      implicit none
      
      integer  pday, gmethod, pmo, noseeds, pdate, pmethod,
     c seedsw, pyear, tempsw
          
      real  devern, dummy2(16), ecanht, elrate, ergdd(4), gdds1, gdds2, 
     c germd, germgdd(4), latitude, maxht, p1v, pchron, pdepth, tbase, 
     c toptlo, toptup, tupper, vtbase, vtoptlo, vtoptup, 
     c vtupper, wfpslo(4), wfpsup(4), wlow, wup
!debe 020309 moved pdepth to real instead of integer   
!debe added tempsw so that it can be set to the value read in for seedsw and thereby initialized.
   
      character *22  cname, dummy1(16), pequation, soilwat(4), swtype, 
     c vname
      
! 9/4/14 GM, debe and Mike Herder set the weather variable to a string length of 110 to allow reading
!   of the path name to include the state/region and the weather filename. The weather files are now located
!   in a more detailed folder structure to work with the new interface programmed by Mike H.
      character *110 weather
      
! Local Variables
      character *22 seedbed      
      integer i, row
      
      row = 4

! Read inputs and parameters from interface via TINPUTS.TXT file:
! debe changed the extension of the tinputs file to .dat

!      open (unit = 1, file = 'tinputs.txt', status = 'OLD')
      open (unit = 1, file = 'tinputs.dat', status = 'OLD')

! Read in crop name (must match specific name in input file to interface):
!      call echo(1)
	read (1,'(a22)') cname
      print *, 'cname = ', cname
! Read in variety name (must match specific name in input file to interface):
	read (1,'(a22)') vname

! Read in name of weather file to use:
	read (1,*) weather
	print *, 'weather = ', weather
!!Much of the weather file path is hard coded in the next statement.      
!      weather="/PhenologyMMS Working\Fortran\Working/PhenologyMMS/
!     .MMSWeather/" // weather


! Read in mo and day of planting:
! Change when working right in program, where month and day converted to
!   daynum and pdate = this daynum:
	read (1,*) pmo, pday, pyear, pdate
!      print *, 'after reading tinputs in setup.for: pdate = ', pdate
      
! Read in planting depth (cm) and convert to mm:
	read (1,*) pdepth
!	    pdepth = pdepth * 10.
! Read in number of seeds planted per m2:
	read (1,*) noseeds

! Read in seedbed soil water conditions:
! Options are "optimum", "medium", "dry", and "planted in dust".
	read (1,*) seedbed
	
! Convert seedbed character string to an integer value stored in seedsw	
	if(seedbed .eq. 'Optimum') then
	   seedsw = 1
	elseif(seedbed .eq. 'Medium') then
	   seedsw = 2    
	elseif(seedbed .eq. 'Dry') then
	   seedsw = 3
	elseif(seedbed .eq. 'Planted') then
	   seedsw = 4
	endif  

! set the value of tempsw initially to be equal to the value of seedsw that is read in.
!try the following to give tempsw a value that will work with the six element arrays in Emerge
      IF (seedsw.EQ.1) THEN
          tempsw = 1
      ELSEIF (seedsw.EQ.2) THEN
          tempsw = 3
      ELSEIF (seedsw.EQ.3) THEN
          tempsw = 5
      ELSEIF (seedsw.EQ.4) THEN
          tempsw = 6
      ENDIF
	
! Read in values from emergence data table written to tinputs.
! Put these values into 5 one dimensional arrays.
      Do 10 i = 1, row
        read (1,*) swtype    !Soil Moisture Condition. 
          soilwat(i) = swtype
        read (1,*) wlow      !lower range of soil moisture
          wfpslo(i) = wlow
        read (1,*) wup       !upper range of soil moisture
          wfpsup(i) = wup
        read (1,*) germd     !gdd for germination at soil moisture
          germgdd(i) = germd
        read (1,*) elrate    !elongation for emergence 
          ergdd(i) = elrate
 10   continue  

! Read in latitude:
! Check whether degrees or radians:
	read (1,*) latitude

! Read in method of calculating GDD.  Method 1 = "wheat" method, Method 2 =
! "corn" method:
	read (1,*) gmethod

! Read in base temperature (C):
	read (1,*) tbase

! Read in lower optimum temperature (C):
	read (1,*) toptlo

! Read in upper optimum temperature	(C):
	read (1,*) toptup

! Read in upper threshold temperature (C):
	read (1,*) tupper
	
! Read in phyllochron value	
	read (1,*) pchron
      
! Read in maximum canopy height(cm):
      read (1,*) maxht  
       
! Read in vernalization values:  
!   read in required vernalization days:
      read (1,*) p1v
      
!   read in base temperature below which no vernalization is achieved (C): 
      read (1,*) vtbase
      
!   read in lower optimum temperature for vernalization (C):
      read (1,*) vtoptlo
      
!   read in upper optimum temperature for vernalization (C):      
      read (1,*) vtoptup
      
!   read in upper temperature limit above which no vernalization 
!    is achieved (C):      
      read (1,*) vtupper
      
!   read in temperature above which de-vernalization can occur (C):      
      read (1,*) devern

! read in ecanht the maximum canopy height for phase 1 canopy height growth
      read (1,*) ecanht

! Read in phenology parameters:
! This will need to be done differently when phyllochron equations are used:
      do 100 i = 1, 16
         read (1,*) dummy1(i), dummy2(i)
         if (dummy1(i) .eq. 'LN' .or. dummy1(i) .eq. 'LS')then
             dummy2(i) = dummy2(i) * pchron

         endif
 100	   continue
   
      Close (Unit = 1) !close tinputs file

!debe added: initialize gdds1 and gdds2 based on crop name. 6/9/11
! set gdds1 value
      IF (cname.EQ.'Corn') THEN
          gdds1 = (dummy2(2)+dummy2(5))
      ELSEIF (cname.EQ.'Dry Beans') THEN
          gdds1 = dummy2(2)
      ELSEIF (cname.EQ.'Hay Millet') THEN
          gdds1 = (dummy2(3)+dummy2(4)+dummy2(5)) 
      ELSEIF (cname.EQ.'Proso Millet') THEN
          gdds1 = (dummy2(3)+dummy2(4)+dummy2(5))  
      ELSEIF (cname.EQ.'Sorghum') THEN
          gdds1 = dummy2(3)
      ELSEIF (cname.EQ.'Soybean') THEN
          gdds1 = dummy2(2)    
      ELSEIF (cname.EQ.'Spring Barley') THEN
          gdds1 = (dummy2(3)+dummy2(4)+dummy2(5)) 
      ELSEIF (cname.EQ.'Spring Wheat') THEN
          gdds1 = (dummy2(3)+dummy2(4)+dummy2(5))  
      ELSEIF (cname.EQ.'Sunflower') THEN
          gdds1 = dummy2(2)
      ELSEIF (cname.EQ.'Winter Barley') THEN
          gdds1 = (dummy2(2)+dummy2(3)+dummy2(4)+dummy2(5)) 
      ELSEIF (cname.EQ.'Winter Wheat') THEN
          gdds1 = (dummy2(2)+dummy2(3)+dummy2(4)+dummy2(5))  
      ENDIF

!set gdds2 value
        ! with corn's correct growth stages: de changed 8/15/08, 10/27/08, 3/20/09
        ! add the dummy2 elements for the appropriate stages. using gddwsf resulted in the sum
        ! to be divided in the hrate calculations being too large because the value for antss
        ! position 7 was 0.0 until the stage was reached.

            IF (cname.EQ.'Corn') THEN
          gdds2 = (dummy2(6)+dummy2(7))-(dummy2(5))
      ELSEIF (cname.EQ.'Dry Beans') THEN
          gdds2 = (dummy2(3)+dummy2(4)+dummy2(5)+dummy2(6)+dummy2(7)+
     c    dummy2(8)+dummy2(9)) 
      ELSEIF (cname.EQ.'Hay Millet') THEN
          gdds2 = (dummy2(6)+dummy2(7)+dummy2(8))+(dummy2(9)) 
      ELSEIF (cname.EQ.'Proso Millet') THEN
          gdds2 = (dummy2(6)+dummy2(7)+dummy2(8))+(dummy2(9)) 
      ELSEIF (cname.EQ.'Sorghum') THEN
          gdds2 = (dummy2(5)+dummy2(6)+dummy2(7)-dummy2(3)) ! equals 215
      ELSEIF (cname.EQ.'Soybean') THEN
          gdds2 = (dummy2(3)+dummy2(4)+dummy2(5)+dummy2(6)+dummy2(7)+
     c    dummy2(8)+dummy2(9)+dummy2(10)+dummy2(11)+dummy2(12)) 
      ELSEIF (cname.EQ.'Spring Barley') THEN
          gdds2 = (dummy2(6)+dummy2(7)+dummy2(8))+(dummy2(9)) 
      ELSEIF (cname.EQ.'Spring Wheat') THEN
          gdds2 = (dummy2(6)+dummy2(7)+dummy2(8))+(dummy2(9)) 
      ELSEIF (cname.EQ.'Sunflower') THEN !check if this is when max sunflower canopy height is reached (stage8)
          gdds2 = (dummy2(3)+dummy2(4)+dummy2(5)+dummy2(6)+dummy2(7)+
     c    dummy2(8)+dummy2(9))
      ELSEIF (cname.EQ.'Winter Barley') THEN
          gdds2 = (dummy2(6)+dummy2(7)+dummy2(8))+(dummy2(9)) 
      ELSEIF (cname.EQ.'Winter Wheat') THEN
          gdds2 = (dummy2(6)+dummy2(7)+dummy2(8))+(dummy2(9)) 
      END IF

      return
      end