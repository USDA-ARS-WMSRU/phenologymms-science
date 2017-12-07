c  *********************************************************************
c  *                                                                   *
c  *               F U N C T I O N       D A Y H O U R S               *
c  *                                                     de   11/07/17 *
c  *********************************************************************
c  This function uses latitude, zenith angle, declination of the sun and
c  month, day and year to calculate the daylength for a day. The code was
c  was provided by Simon van Donk and the reference he used is found in
c  Keisling, T.C., 1982. Calculation of the length of day. Agron. J. 
c  74:758-759. 

!  INPUTS:  list variables here with C and R for each variable           

!  OUTPUTS: list variables here with C and R for each variable
      
      
      FUNCTION dayhours(latitude, day, month, year)
      IMPLICIT NONE
      
!     Variable declarations
      REAL :: dayhours, latitude
      INTEGER :: year
      
!     Local variables
      REAL :: declin, degtorad, julday, lambda, sunanom, sunbelow 
      REAL :: zenith
      REAL :: day, month 
      
!Initialize local variables           
      declin = 0.0
      lambda = 0.0
      sunanom = 0.0
      sunbelow = 0.0
      zenith = 0.0
      julday = 0.0
      
! end of declarations
      
      !     + + + purpose + + +
      ! This function calculates the length of the day, in hours, for a given day
      
      !     + + + argument definitions + + +
      ! many of these definitions came from the Keisling reference given above.
      ! day - the day of the year for which daylength is being calculated (1-31)
      ! dayhours - the hours of daylight calculated herein and returned from this function
      ! latitude - latitude of the site, degrees (north > 0, south < 0), in degrees
      ! mon - the month of the year (1-12)
      ! year - the year
      
      !     + + + local variable definitions + + +
      ! declin - the declination of the sun in degrees
      ! degtorad - parameter to convert values in degrees to radians. It is equal to pi/180
      ! lambda - ???
      ! sunanom - mean anomaly of the sun in degrees. This is M in Keisling ref.
      ! sunbelow - the angle of the sun below the horizon. This is variable B in Keisling ref.
      ! zenith - the zenithal distance of the sun at the event of interest in degrees

      ! calculate the Julian day
      julday = Day-32 + (Int(275*(Month/9))) + (2*Int(3/(Month+1))) + 
     c (Int((Month/100)-(Mod(Year,4)/4)+0.975))          

!     Calculate M use variable sunanom for M     
      sunanom = 0.985600 * julday - 3.251
      
!     Calculate the lambda value
      lambda = sunanom + 1.916 * sind(sunanom) + 0.020 *
     c         sind(2*sunanom) + 282.565

!     Calculate the declination of the sun (degrees)      
      declin = asind(0.39779 * sind(lambda))
      
!     Calculate zenith angle (degrees) sunbelow is used for B
      zenith = 90 - sunbelow
      
!     Calculate the daylength            
      dayhours = (2.0/15.0) * acosd((cosd(zenith))*(1/(cosd(latitude)))*
     c (1/(cosd(declin)))- ((tand(latitude))*(tand(declin))))
            
      END FUNCTION dayhours
      
      
      
      
      