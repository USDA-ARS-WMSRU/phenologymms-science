c from SHOOTGRO
c  *********************************************************************
c  *                                                                   *
c  *               F U N C T I O N       D A Y L T H                   *
c  *                                                     dmh   1/22/93 *
c  *********************************************************************
c  This function uses latitude and day of year to calculate 
c  day length (in hours) using code extracted from the WGEN model
c  (Richardson, C.W. andD.A. Wright.  1984.  WGEN: A model for generating
c  daily weather variables. USDA-ARS, ARS-8. 83 pp.).  Day length on
c  consecutive days is used to calculate the change in day length, which
c  subroutine PHYLLO uses to calculate the phyllochron for each seedling 
c  cohort. 

c  INPUTS:  lat(R)
c           dummy argument--stday(R)

c  OUTPUTS: function value--DAYLTH(C)

c      include 'shtgro.inc'

      subroutine daylenth(daylth, daynum, latitude)
      
      implicit none

      integer daynum, stday
      real ch, daylth, halfdy, latitude, latrad, sd
      
      stday = daynum

c  Change latitude in degrees (LAT) to radians (LATRAD).

      latrad = latitude * 6.2832 / 360.

c  Calculate SD, the angle (in radians) of declination of the sun 
c  the angular distance of the sun north(+) or south(-) of the equator
c  on the day of year.  STDAY is the julian date (day of year).

      sd = 0.4102 * SIN(0.0172 * (REAL(stday) - 80.25))

c  Calculate cos h (=CH) of the half-day length in radians.

      ch = -TAN(latrad) * TAN(sd)
      if(ch.gt.1.0) ch = 1.0
      if(ch.lt.-1.0) ch = -1.0

c  Calculate half-day length in radians.

      halfdy = ACOS(ch)

c  Calculate day length in hours.

      daylth = halfdy * 7.6394

      return
      end