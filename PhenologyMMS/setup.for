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

      subroutine setup(cname, pday, dummy1, dummy2, elrate, ergdd, 
     c germd, germgdd, gmethod, latitude, maxht, pmo, noseeds, 
     c pchron, pdate, pdepth, pequation, pmethod, seedsw, soilwat, 
     c swtype, tbase, toptlo, toptup, tupper, vname, weather, wfpslo,
     c wfpsup, wlow, wup, pyear)
  
      implicit none
      
      integer  pday, gmethod, pmo, noseeds, pdate, pdepth, pmethod,
     c seedsw, pyear
          
      real  dummy2(15), elrate, ergdd(4), germd, germgdd(4),  
     c latitude, maxht, pchron, tbase, toptlo, toptup, tupper, 
     c wfpslo(4), wfpsup(4), wlow, wup
      
      character *22  cname, dummy1(15), pequation, soilwat(4), swtype, 
     c vname, weather
      
! Local Variables
      character *22 seedbed      
      integer i, row
      
      row = 4

! Read inputs and parameters from interface via TINPUTS.TXT file:
!   Probably need to give path for TINPUTS.TXT file?

      open (unit = 1, file = 'tinputs.txt', status = 'OLD')

! Read in crop name (must match specific name in input file to interface):
!      call echo(1)
	read (1,'(a22)') cname

! Read in variety name (must match specific name in input file to interface):
	read (1,'(a22)') vname

! Read in name of weather file to use:
	read (1,*) weather
	print *, 'weather = ', weather

! Read in mo and day of planting:
! Change when working right in program, where month and day converted to
!   daynum and pdate = this daynum:
	read (1,*) pmo, pday, pyear, pdate

! Read in planting depth (cm) and convert to mm:
	read (1,*) pdepth
	    pdepth = pdepth * 10.

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

! Read in upper optimum temperature	
	read (1,*) toptup

! Read in upper threshold temperature (C):
	read (1,*) tupper
	
! Read in phyllochron value	
	read (1,*) pchron
      print *, 'pchron = ', pchron
      
! Read in maximum canopy height(cm):
      read (1,*) maxht   

! Read in phenology parameters.
! This will need to be done differently when phyllochron equations are used:
      do 100 i = 1, 15
         read (1,*) dummy1(i), dummy2(i)              
         if (dummy1(i) .eq. 'LN' .or. dummy1(i) .eq. 'LS')then
             dummy2(i) = dummy2(i) * pchron
         endif

 100	   continue
   
      Close (Unit = 1)

      return
      end