!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   I N I T W T H R              *
!  *                                                      de   2/27/06 *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
!  The INIT subroutine initializes weather variables.


!  INPUTS:  precip(C), ri(C), soil3t(C), tavg(C), tmax(C), tmin(C)
 

!  OUTPUTS: precip(C), ri(C), soil3t(C), tavg(C), tmax(C), tmin(C)
 
	
      subroutine initwthr(precip, ri, soil3t, tavg, tmax, tmin)
     

      implicit none
      
      real  precip, ri, soil3t, tavg, tmax, tmin

! Initialize variables
!  Weather Variables
      precip = 0.0
      ri = 999.9
      soil3t = 999.9
      tavg = 999.9
      tmax = 999.9
      tmin = 999.9


      return
      end