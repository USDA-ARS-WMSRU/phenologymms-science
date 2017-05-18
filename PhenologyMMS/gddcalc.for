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

      subroutine gddcalc(daynum, df, gddday, gmethod, tbase, tmax, tmin,
     c toptlo, toptup, tupper) !, vf) 
!         
      implicit none
      
      integer daynum, gmethod
     
      real  df, gddday, tbase, tmax, tmin, toptlo, toptup, tupper !, vf
      
      character *256 outfgdd
      
! Local variables
      real m, tavg, tavgtemp, tf, tmaxtemp, tmintemp, topt, x, x1, x2,
     c x3, x4, y, y1, y2, y3, y4 
     
! Initialize local variables   
      tavg = 0.0
      tavgtemp = 0.0
      tmaxtemp = tmax
      tmintemp = tmin
      topt = (toptlo + toptup) / 2
 !     print *, 'topt average method = ', topt      

      !tbase point coordinates
      x1 = tbase
      y1 = 0.0
      !toplo point ccordinates
      x2 = toptlo
      y2 = 1.0 ! maximum development rate
      !toptup point coordinates
      x3= toptup
      y3 = 1.0
      !tupper point coordinates
      x4 = tupper
      y4 = 0.0
!      print *, 'tbase = ', tbase, 'toptlo = ', toptlo, 'toptup = ',
!     c toptup, 'tupper = ', tupper
! Determine method of calculating growing degree-days
! Calculate using Method 1:
! This is a linear model.

      if (gmethod .eq. 1) then
         tavg = (tmax + tmin)/2.0 !need to be able to print tavg 
                                     !for method 1
!         gddday = ((tmax + tmin)/2.) - tbase
         gddday = tavg - tbase 
         if (gddday .lt. 0.) gddday = 0.
         
! Calculate using Method 2:
! This is a linear model with an upper threshold limit where no 
! additional gdd's are accumulated.
      elseif (gmethod .eq. 2) then
! adjust tmax and tmin if need be and store values in tmaxtemp and tmintemp      
         if (tmax .lt. tbase) tmaxtemp = tbase
         if (tmax .gt. toptup) tmaxtemp = toptup
         if (tmin .lt. tbase) tmintemp = tbase
         if (tmin .gt. toptup) tmintemp = toptup

         tavgtemp = (tmaxtemp + tmintemp)/2.
         gddday = tavgtemp - tbase
         
         if (gddday .lt. 0.) gddday = 0.
         if (gddday .gt. toptup) gddday = toptup

! Calculate using Method 3: 
! This is a two-segmented linear model
      else if (gmethod .eq. 3) then
             tavg = (tmax + tmin)/2.

!      if (tavg .GE. tbase) then
!            print *, 'tavg = ', tavg
!      endif      

! use the slope equation to determine a temperature factor (tf) by which
! the day's gdd is adjusted.
        if ((tavg .GE. tbase) .AND. (tavg .LE. topt)) then
           m = (y2-y1)/(x2-x1) ! find the slope of the line tbase -topt 
      !next find the point on the first segment of the linear model where
      !a line drawn perpendicular from the tavg point on the x axis will 
      !interesect the first segment of the linear model. The y part of that
      !point will be the tf factor or the y axis portion of the intersected
      !point.  Know the slope of the line, now find the point of intersection. 
      !Use the point-slope equation
      !  y-y1 = m(x-x1) ! x2 and y2 could also be used in place of x1 and y1  
           x= tavg
           y = m*x - m*x1 + y1
           tf = y     
           gddday = tf*(tavg-tbase)
        elseif ((tavg .GT. topt) .AND. (tavg .LE. tupper)) then
           m = (y4-y2)/(x4-x2)! find the slope of the line topt-tupper
           x = tavg
           y = m*x - m*x4 + y4
           tf = y
           gddday = tf*(tavg-tbase)
        elseif ((tavg .LT. tbase) .OR. (tavg .GT. tupper)) then
           gddday = 0.0   
        endif

! Calculate using Method 4:
! This is a three segmented linear model
      elseif (gmethod .eq. 4) then !this is method 3 w/another line segment
             tavg = (tmax + tmin)/2. 
     
        if ((tavg .GE. tbase) .AND. (tavg .LT. toptlo)) then
           m = (y2-y1)/(x2-x1) !find the slope of the line tbase -toptlo
      !  y-y1 = m(x-x1) ! x2 and y2 could also be used in place of x1 and y1  
           x= tavg
           y = m*x - m*x1 + y1
           tf = y     
           gddday = tf*(tavg-tbase)
        elseif ((tavg .GE. toptlo) .AND. (tavg .LE. toptup)) then 
           m = (y3-y2)/(x3-x2)! find the slope of the line toptlo-toptup
           x = tavg       ! this slope should be 0
           y = m*x - m*x2 + y2
           tf = y
           gddday = tf*(tavg-tbase)
        elseif ((tavg .GT. toptup) .AND. (tavg .LE. tupper)) then 
           m = (y4-y3)/(x4-x3)! find the slope of the line toptup-tupper
           x = tavg           
           y = m*x - m*x3 + y3
           tf = y
           gddday = tf*(tavg-tbase)
! no growth when tavg is lower than tbase or greater than tupper.
        elseif ((tavg .LT. tbase) .OR. (tavg .GT. tupper)) then
           gddday = 0.0
        endif
      endif 

! Adjust thermal development units with photoperiod and vernalization
         !dtdu = gddday*(min(df, vf)) is this correct?         

!      if(gmethod .eq. 2) then
!        print *, 'tavgtemp = ', tavgtemp, 'method # ', gmethod,
!     c'gddday = ', gddday
!      elseif (gmethod .ne. 2) then
!        print *, 'tavg = ',tavg, 'method # ', gmethod, 'gddday = ', 
!     cgddday 
!      endif  

      return
      end
