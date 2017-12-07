!*==DAYLEN.spg  processed by SPAG 6.55Dc at 23:48 on  8 Aug 2008
!
!$log: not supported by cvs2svn $
!revision 1.1.1.1  1999/03/12 17:05:31  wagner
!baseline version of weps with bill rust's modifications
!
! revision 1.1.1.1  1995/01/18  04:20:06  wagner
! initial checkin
!
! revision 2.1  1992/03/27  17:22:53  wagner
! version 2 code.
!
!$author: fredfox $
!$date: 2002-09-09 16:09:16 $
!$revision: 1.5 $
!$source: /weru/cvs/weps/weps.src/util/solar/daylen.for,v $
 
FUNCTION daylen(latitude, daynum, riseangle, degtorad, radtodeg)
!latitude,pdate,civilrise
IMPLICIT NONE
!*--DAYLEN20
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
REAL :: latitude,riseangle
INTEGER :: daynum
REAL :: daylen, degtorad, radtodeg
!
! Local variables
!
REAL :: dec,h
REAL :: declination,hourangle
!
!*** End of declarations rewritten by SPAG
!
 
!     + + + purpose + + +
!     this function calculates the daylength (hours) for any simulation
!     site based on the global position of the site, and day of the
!     year.  the inputs for the function are day of the year, and latitude
!     of the site.
 
!     + + + keywords + + +
!     day length
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     daylen - the length of the day. Returns the value of this function.
!     daynum - day of year
!     latitude - latitude of the site, degrees (north > 0, south < 0)
!     riseangle - angle of earths rotation where sunrise occurs.
!                 This varies depending on whether you are calculating
!                 direct beam, civil twilight, nautical twilight or
!                 astronomical twilight daylength
 
!     + + + local variables + + +
 
!     + + + local variable definitions + + +
!     dec - declination of earth with respect to the sun (degrees)
!     declination - 
!     h - hour angle (degrees)
!     hourangle - a function. This variable holds the value returned by 
!                 the Hourangle function. Debe assumed this definition
 
!     + + + common blocks + + +
!     include 'p1unconv.inc'
 
!     + + + function declarations + + +
 
!     + + + end specifications + + +
 
!     declination angle (dec)
dec = declination(daynum, degtorad)

!     sunrise or sunset hour angle
h = hourangle(latitude, dec, riseangle, degtorad, radtodeg)

!     calculate the length of the day
daylen = 2.0*h/15.0
 
END FUNCTION daylen
