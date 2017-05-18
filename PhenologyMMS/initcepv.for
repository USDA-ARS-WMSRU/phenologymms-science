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
 
	
      subroutine initcepv(canht, elong, ergdd, germgdd, nai, pchron, 
     c rai, soilwat, vernal, wai, wfpslo, wfpsup)
     

      implicit none
      
      integer  nai, rai, wai
          
      real  canht, elong, ergdd(4), germgdd(4), pchron, vernal, 
     c wfpslo(4), wfpsup(4)
      
      character *22  soilwat(4)

!  Local Variables
      integer i

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
	vernal = 0.0


      return
      end