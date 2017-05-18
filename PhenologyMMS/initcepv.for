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
 
	
      subroutine initcepv(canht, civilrise, cumvd, degtorad, devern, df,
     c elong, ergdd, germgdd, hrlt, nai, pchron, pd, p1v, radtodeg,   
     c rai, soilwat, vd, vernal, vtbase, vtoptlo, vtoptup, 
     c vtupper, wai, wfpslo, wfpsup) 	!vf, vf0, 
    

      implicit none
      
      integer  nai, rai, wai
          
      real  canht, civilrise, cumvd, degtorad, devern, df, elong, 
     c ergdd(4), germgdd(4), hrlt, pchron, pd, p1v, radtodeg, vd, 
     c vernal, vtbase, vtoptlo, vtoptup, vtupper, wfpslo(4), 
     c wfpsup(4) !vf, vf0, 
      
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
      

      return
      end