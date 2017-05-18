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
      
      integer uuday, uumonth, uuyear, i

      real  aveprecip(12), avetmax(12), avetmin(12), precip, ri, soil3t,
     c      tavg, tmax, tmin

! Initialize variables
!  Weather Variables
      do i=1,12
         aveprecip(i) = 0.0
         avetmax(i) = 0.0
         avetmin(i) = 0.0
      end do   
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