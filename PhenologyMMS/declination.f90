!*==DECLINATION.spg  processed by SPAG 6.55Dc at 23:48 on  8 Aug 2008
!$author: fredfox $
!$date: 2002-09-09 16:09:16 $
!$revision: 1.3 $
!$source: /weru/cvs/weps/weps.src/util/solar/declination.for,v $
 
FUNCTION declination(daynum, degtorad)
IMPLICIT NONE
!*--DECLINATION9
!INCLUDE 'p1unconv.inc'
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
INTEGER :: daynum
REAL :: declination, degtorad


!
! Local variables
!
REAL :: b
!
!*** End of declarations rewritten by SPAG
!
 
!     + + + purpose + + +
!     this function calculates the declination of the earth with respect
!     the sun based on the day of the year
 
!     + + + keywords + + +
!     solar declination
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     daynum   - day of year
 
!     + + + local variables + + +
 
!     + + + local definitions + + +
!     b      - sub calculation (time of year, radians)
 
!     + + + common blocks + + +
 
!     + + + end specifications + + +
 
!     calculate declination angle (dec)
b = (360.0/365.0)*(daynum-81.25)*degtorad          !h-55
declination = 23.45*sin(b)                         !h-58
 
END FUNCTION declination
