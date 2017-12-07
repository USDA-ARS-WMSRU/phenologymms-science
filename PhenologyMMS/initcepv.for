!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   I N I T C E P V              *
!  *                                                      de   2/27/06 *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
!  This INIT subroutine initializes variables for the canopy, emergence,
!  phyllochron and vernalization subroutines.


!  INPUTS:  canht(C), elong(C), ergdd(C), germgdd(C), nai(C), pchron(C), 
!           rai(C), soilwat(C), vernal(C), wai(C), wfpslo(C), wfpsup(C) 
            

!  OUTPUTS: canht(C), elong(C), ergdd(C), germgdd(C), nai(C), pchron(C), 
!           rai(C), soilwat(C), vernal(C), wai(C), wfpslo(C), wfpsup(C)  
 
	
      subroutine initcepv(canht, civilrise, cumvd, dayhrs, degtorad, 
     c devern, df, egdd, elong, ergdd, germgdd, ggdd, hrlt, mg, nai,    
     c p1d, p1v, pchron, pf, photocrit, photosen, ppsen, radtodeg, rai,
     c soilwat, vd, vernal, vtbase, vtoptlo, vtoptup, vtupper, wai, 
     c wfpslo, wfpsup) 	!vf, vf0, 
     
      implicit none
      
      integer  nai, rai, wai
          
      real  canht, civilrise, cumvd, dayhrs, degtorad, devern, df,  
     c elong, ergdd(4), germgdd(4), hrlt, mg, p1d, p1v, pchron, pd, pf, 
     c photocrit, photosen, ppsen, radtodeg, vd, vernal, vtbase,  
     c vtoptlo, vtoptup, vtupper, wfpslo(4), wfpsup(4) !vf, vf0, 

! debe added variables for photoperiod subroutine, Nov 2017
      
!debe added the arrays egdd and ggdd to be initialized, passed back to 
! PhenologyMMS and then to Emerge.
      REAL,DIMENSION(6) :: egdd, ggdd

      character *22  soilwat(4)

!  Local Variables
      integer i
      
!debe added civilrise, degtorad, radtodeg for day length calculations 
! from p1unconv.inc in UPGM/WEPS
!      parameter (civilrise = 96.0 )
!      parameter (degtorad = 0.017453293) !pi/180
!      parameter (radtodeg = 57.2957795) !180/pi    

! Initialize variables
!  Canopy Variables
      canht = 0.0
      
!  Emergence      
      elong = 0.0
      
! Initialize emergence arrays      
      do 50 i = 1, 4
        soilwat(i) = ""
        ergdd(i) = 0.0
        germgdd(i) = 0.0
        wfpslo(i) = 0.0
        wfpsup(i) = 0.0
 50   continue       

      do 60 i = 1, 6
        egdd(i) = 0.0
        ggdd(i) = 0.0        	
 60	continue
                    
      
!  Phyllochron Variables
      pchron = 0.0  
        
!  Stress Variables
      rai = 1.0
      wai = 1.0
      nai = 1.0

!  Vernalization Variables
	cumvd = 0.0
	devern = 0.0
	p1v = 0.0
	vd = 0.0
	vernal = 0.0
!	vf = 0.0
!	vf0 = 0.0
	vtbase = 0.0
	vtoptlo = 0.0
	vtoptup = 0.0
	vtupper = 0.0

!  Photoperiod variables
      civilrise = 96.0 
      degtorad = 0.017453293
      df = 0.0
      hrlt = 0.0
      pd = 0.0
      radtodeg = 57.2957795
      
! debe added new photoperiod variables      
      dayhrs = 0.0
      mg = 0.0
      pf = 0.0
      photocrit = 0.0
      photosen = 0.0
      ppsen = 0.0    

      return
      end