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
 
	
      subroutine initwthr (aveprecip, avetmax, avetmin, precip, ri, 
     c              soil3t, tavg, tmax, tmin, uuday, uumonth, uuyear)
     

      implicit none

!debe added uuday, uumonth, uuyear. these are variables that are read in from
! the climate file but not used.
      
      integer uuday, uumonth, uuyear

      real  aveprecip, avetmax, avetmin, precip, ri, soil3t, tavg, tmax, 
     c      tmin

! Initialize variables
!  Weather Variables
      aveprecip = 0.0
      avetmax = 0.0
      avetmin = 0.0
      precip = 0.0
      ri = 999.9
      soil3t = 999.9
      tavg = 999.9
      tmax = 999.9
      tmin = 999.9
      uuday = 0
      uumonth = 0
      uuyear = 0


      return
      end