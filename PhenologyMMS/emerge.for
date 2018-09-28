!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   E M E R G E                  *
!  *                                                      gm   1/21/05 *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
!  The emerge subroutine calculates the thermal time for germination (GERMGDD)
!  and elongation rate in mm/GDD (ERGDD) for different crops based on the
!  soil water content of the seedbed (tempsw). This subroutine is based on
!  the SHOOTGRO model (Wilhelm et al., 1993. Ecological Modelling 68:183-203).
!  The user inputs via the interface one of four soil water levels: 1 =
!  optimum, 2 = medium, 3 = dry, and 4 = planted in dust.  The GDD required
!  for germination increases and elongation rate decreases as water content
!  decreases.  These parameters for each crop are in the EMERGE.TXT file
!  located in the Interface directory. After planting, precipitation can
!  shift the level towards optimum conditions, but there is no
!  provision for reducing the soil level based on evaporation.

      SUBROUTINE emerge(dap,daynum,ddap,dgdds,egdd,elong,ems,ergdd,     
     c                  gddday,gddtbg,germgdd,germs,ggdd,pdepth,precip,
     c                  tempsw,year) 

        !  inputs:  dap(r), daynum(r), ddap(20)(c), dgdds(20)(c), elong(c,r),
        !           ems(c,r), ergdd(4)(r), gddday(r), gdds(r), germgdd(4)(r),
        !           germs(c,r), pdepth(r), precip(r), seedsw(c,r)
        ! 
        !  outputs: ddap(20)(c), dgdds(20)(c), elong(c,r), ems(c,r),
        !           germs(c,r), seedsw(c,r)
        !                
        !  debe added two new six element arrays ggdd and egdd with two positions for intermediate values
        !  at intermediate soil moisture levels in positions 2 and 4. Later passed in tempsw, the array 
        !  index value for these two arrays. It is initialized in Cinit.
        !
        !     + + + purpose + + +
        !     to calculate the day of emergence based on soil moisture and
        !     accumulated thermal time.
        ! 
        IMPLICIT NONE

        ! Subroutine arguments
        
        INTEGER :: dap,daynum,tempsw, year 
        REAL :: elong,gddday,gddtbg,pdepth
        INTEGER,DIMENSION(20) :: ddap
        REAL,DIMENSION(20) :: dgdds
        INTEGER,DIMENSION(4) :: ems,germs
        REAL,DIMENSION(4) :: ergdd,germgdd
        REAL,DIMENSION(6) :: egdd,ggdd
 
        ! Local variables
       
        INTEGER :: i,row
        REAL :: precip
       
        !debe added six element arrays to handle two intermediate soil moisture levels: ggdd, egdd.
 
        !     + + + argument definitions + + +
        !     dap - days after planting.
        !     daynum - day number of the year
        !     ddap - array holding the number of days after planting for up
        !            to 20 different stages.
        !     dgdds - array holding the number of gdd after seeding for up
        !            to 20 different stages.
        !     elong - total elongation of the emerging seedling based on the
        !             day's gdd (mm)
        !     ems - simulated day that emergence began.
        !     egdd - a 6 element array that holds the ergdd values plus calculated values
        !            for two intermediate soil moisture level values in elements 2 and 4.
        !     ergdd - an array holding 4 elongation rates in mm per gdd
        !             based on each soil moisture description.
        !     gddday - the number of gdd with 0°c base temperature for that day.
        !     gddtbg - used to accumulate gdd for seeds planted in dust after a
        !              rainfall event has moved the soil moisture condition to
        !              dry. then the seed can begin to accumulate gdd to begin
        !              germination.
        !     germgdd - an array holding 4 germination times in gdd at base 0°c for
        !               the soil moisture levels.
        !     germs - simulated day that germination occurs.
        !     ggdd - a 6 element array that holds the germgdd values plus calculated values for
        !           two intermediate soil moisture level values in elements 2 and 4.
 
        !     + + + local variable definitions + + +
        !     pdepth - planting depth; cm. 
        !     pdepthnew - holds the converted pdepth value to cm.
        !     precip - the amount of precipitation read from the weather file for
        !              a particular day.  variable precip is set equal to awzdpt
        !              which is found in the include file - w1clig.inc.
        !     row - the row of soil moisture information.
        !     tempsw - a new variable to designate the array subscripts for the new 6 element
        !              arrays: egdd, ggdd.   The soil water content at seed depth is read in as
        !              optimum, medium, dry or planted in dust and converted
        !              to an integer in Setup.	 1 = optimum, 2 = medium, 3 = dry and
        !              4 = planted in dust. Then tempsw is set to 1 if optimum, 3 if medium, 
        !              5 if dry and 6 if planted in dust.
        !     year - the calendar year of weather data. variable yy from weps/upgm is
        !            used for year. this is the year i of the rotation and not the
        !            calendar year.
 
        !     initialize variables
        row = 6  
! 
        !debe added 6 element array to hold two half steps between dry and medium
        ! and medium and optimum soil water.
        ! create the new arrays: ggdd for germgdd and egdd for ergdd.
! 
        DO i = 1,row
          IF (i.EQ.1) THEN      !Optimum
             ggdd(i) = germgdd(i)
             egdd(i) = ergdd(i)
          ELSE IF (i.EQ.2) THEN !between Medium and Optimum
             ggdd(i) = ((germgdd(1)+germgdd(2))/2)
             egdd(i) = ((ergdd(1)+ergdd(2))/2)
          ELSE IF (i.EQ.3) THEN !Medium
             ggdd(i) = germgdd(2)
             egdd(i) = ergdd(2)
          ELSE IF (i.EQ.4) THEN !between Dry and Medium
             ggdd(i) = ((germgdd(2)+germgdd(3))/2)
             egdd(i) = ((ergdd(2)+ergdd(3))/2)
          ELSE IF (i.EQ.5) THEN !Dry
             ggdd(i) = germgdd(3)
             egdd(i) = ergdd(3)
          ELSE IF (i.EQ.6) THEN !Planted in Dust
             ggdd(i) = germgdd(4)
             egdd(i) = ergdd(4)
          END IF
        END DO
!            print*, 'tempsw = ', tempsw 
        IF ((precip.GE.7).AND.(tempsw.LT.6)) THEN !move up to next level if NOT Planted in Dust and if precip > 7
                                                  !tempsw must be less than 6 to move up.
          tempsw = tempsw - 1
        END IF
!
!if Planted in Dust move to next levels based on amount of precip 
         IF (tempsw.EQ.6) THEN 
          IF ((precip.GE.7.0).AND.(precip.LT.12.0)) THEN
             tempsw = tempsw - 1
          ELSE IF ((precip.GE.12.0).AND.(precip.LT.20.0)) THEN
             tempsw = tempsw - 3
          ELSE IF (precip.GE.20.0) THEN
             tempsw = tempsw - 5
          END IF
        END IF
! 
        ! reset tempsw to 1 if it becomes less than 1
        IF (tempsw.LT.1) tempsw = 1
 
        ! Seeds Planted in Dust will not begin accumulating GDD's for emergence 
        ! until the soil moisture level moves up to at least Dry. When a precip  
        ! event or an irrigation event occurs, the soil moisture condition may 
        ! be adjusted depending on the amount of the water received/applied. 
        ! If moisture is received greater than or equal to 7mm but less than  
        ! 12 mm, the soil moisture condition will move up to Dry. 
        ! If the amount of moisture received is 12 mm but less than 20 mm, 
        ! the soil moisture condition will move up to Medium  and if more 
        ! than 20 mm are received the soil moisture condition will move up 
        ! to Optimum. Once, the soil moisture condition improves beyond
        ! Planted in Dust, the process of emergence can begin by 
        ! accumulating GDD's.
! 
        IF (tempsw.LT.6) THEN
          gddtbg = gddtbg + gddday
        ELSE
          gddtbg = 0.
        END IF
! 
        ! check if germination can occur
        IF ((germs(1).EQ.999).AND.(gddtbg.GE.ggdd(tempsw))) THEN
          germs(1) = daynum
          germs(2) = year
          CALL date1(germs)
          PRINT *,'germination = ',germs
        END IF
! 
        ! if germination has occurred then check if elongation is sufficient to
        !  allow emergence to occur.
        IF ((germs(1).NE.999).AND.(ems(1).EQ.999)) THEN
          elong = elong + (egdd(tempsw)*gddday)
          IF (elong.GE. pdepth*10)THEN !pdepth is converted to mm. elong (mm),pdepth (cm) 
             ems(1) = daynum
             ems(2) = year
             CALL date1(ems)
             ddap(1) = dap
             dgdds(1) = gddtbg            !debe changed gdds to gddbtg
             PRINT *,'ems = ',ems 
          END IF
        END IF
 
        END SUBROUTINE emerge 