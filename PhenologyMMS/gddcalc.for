!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   G D D C A L C                *
!  *                                                      gm   1/21/05 *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
! Subroutine GDDCALC:  Calculates growing degree-days by various
! methods.  See McMaster and Wilhelm ref here ***  Method 1 is
! most common for wheat and barley.  Method 2 is most common for corn,
! sorghum, and soybean.

!  INPUTS:  gddday(C,R), gmethod(R), tbase(R), tmax(R), tmin(R), tupper(R)          

!  OUTPUTS: gddday(C,R)

      subroutine gddcalc(gddday, gmethod, tbase, tmax, tmin, tupper) 
!         
      implicit none
      
      integer gmethod
     
      real  gddday, tbase, tmax, tmin, tupper
      
! Local variables
      real tavgtemp, tmaxtemp, tmintemp

      tavgtemp = 0.0
      tmaxtemp = tmax
      tmintemp = tmin

! Determine method of calculating growing degree-days
!      print *,'tupper = ', tupper
! Calculate using Method 1:

      if (gmethod .eq. 1) then
         gddday = ((tmax + tmin)/2.) - tbase
         if (gddday .lt. 0.) gddday = 0.

! Calculate using Method 2:

      elseif (gmethod .eq. 2) then
      
         if (tmax .lt. tbase) tmaxtemp = tbase
         if (tmax .gt. tupper) tmaxtemp = tupper
         if (tmin .lt. tbase) tmintemp = tbase
         if (tmin .gt. tupper) tmintemp = tupper

         tavgtemp = (tmaxtemp + tmintemp)/2.
         gddday = tavgtemp - tbase
         
         if (gddday .lt. 0.) gddday = 0.
         if (gddday .gt. tupper) gddday = tupper

! Calculate using Method 3: This is currently method 1 repeated to test

      else if (gmethod .eq. 3) then
         gddday = ((tmax + tmin)/2.) - tbase
         if (gddday .lt. 0.) gddday = 0.
         
      endif 

      return
      end
