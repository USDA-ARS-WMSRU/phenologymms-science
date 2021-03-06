!*==HOURANGLE.spg  processed by SPAG 6.55Dc at 23:48 on  8 Aug 2008
 
!$author: fredfox $
!$date: 2002-09-09 16:09:16 $
!$revision: 1.5 $
!$source: /weru/cvs/weps/weps.src/util/solar/hourangle.for,v $
 
FUNCTION hourangle(latitude, dec, riseangle, degtorad, radtodeg)
IMPLICIT NONE
!*--HOURANGLE10
!INCLUDE 'p1unconv.inc'
!
!*** Start of declarations rewritten by SPAG
!
! PARAMETER definitions
!
REAL,PARAMETER :: dlat_rad_lim = 1.57079
!
! Dummy arguments
!
REAL :: dec,latitude,riseangle
REAL :: degtorad, hourangle, radtodeg
!! from p1unconv.inc in UPGM/WEPS
!parameter (degtorad = 0.017453293) !pi/180
!parameter (radtodeg = 57.2957795) !180/pi

!
! Local variables
!
REAL :: coshr,dec_rad,dlat_rad
!
!*** End of declarations rewritten by SPAG
!
 
!     + + + purpose + + +
!     this function calculates the hour angle (degrees)
!     of sunrise (-), sunset (+) based on the declination of the earth
 
!     + + + keywords + + +
!     sunrise sunset hourangle
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     latitude   - latitude of the site, degrees (north > 0, south < 0)
!     dec    - declination of earth with respect to the sun (degrees)
!     riseangle - angle of earths rotation where sunrise occurs
!                 this varies depending on whether you are calculating
!                 direct beam, civil twilight, nautical twilight or
!                 astronomical twilight hourangle
 
!     + + + local variables + + +
 
!      parameter( dlat_rad_lim = 1.570796327 ) ! pi/2
 
!     + + + local definitions + + +
!     coshr   - cosine of hour angle at sunrise
!     dlat_rad - latitude of site, converted to radians
!     dec_rad - declination of earth wrt the sun (radians)
 
!     + + + common blocks + + +
 
!     + + + end specifications + + +
 
!     convert to radians
dlat_rad = latitude*degtorad                  ! pi/2 minus a small bit
dec_rad = dec*degtorad
 
!     calculate the cosine of hour angle (h) at sunset
!     to get the sunrise hour angle, take the negative.
!     using the equation from "solar thermal energy systems,
!     howell, bannerot, vliet, 1982, page 51 equation 3-4)
!     modified to account for atmospheric refraction as in
!     noaa document (it just indicates that the sun is seen
!     before it physically is above the horizon)
!     ie. not at 90 degrees, but 90.833 degrees
!     this expression is undefined at 90 and -90 degrees. if
!     roundoff error pushes it beyond the answer flips. limit
!     set here to get correct answer at 90 and -90 degrees.
dlat_rad = max(-dlat_rad_lim,min(dlat_rad_lim,dlat_rad))
coshr = cos(riseangle*degtorad)/(cos(dlat_rad)*cos(dec_rad)) - tan(dlat_rad)    &
      & *tan(dec_rad)
 
!     check for artic circle conditions
IF (coshr.GE.1.0) THEN
  hourangle = 0.0                  !sunrise occurs at solar noon
ELSE IF (coshr.LE.-1.0) THEN
  hourangle = 180.0                !the sun is always above the horizon
ELSE
  hourangle = acos(coshr)*radtodeg
END IF
 
END FUNCTION hourangle
