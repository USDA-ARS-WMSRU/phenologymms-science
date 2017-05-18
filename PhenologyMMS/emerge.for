!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   E M E R G E                  *
!  *                                                      gm   1/21/05 *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
!  The emerge subroutine calculates the thermal time for germination (GERMGDD)
!  and elongation rate in mm/GDD (ERGDD) for different crops based on the
!  soil water content of the seedbed (SEEDSW). This subroutine is based on
!  the SHOOTGRO model (Wilhelm et al., 1993. Ecological Modelling 68:183-203).
!  The user inputs via the interface one of four soil water levels: 1 =
!  optimum, 2 = medium, 3 = dry, and 4 = planted in dust.  The GDD required
!  for germination increases and elongation rate decreases as water content
!  decreases.  These parameters for each crop are in the EMERGE.TXT file
!  located in the Interface directory. After planting, precipitation can
!  shift the level towards optimum conditions, but there is no provision
!  for reducing the soil level based on evaporation.


!  INPUTS:  dap(R), daynum(R), ddap(20)(C), dgdds(20)(C), elong(C,R),
!           ems(C,R), ergdd(4)(R), gddday(R), gdds(R), germgdd(4)(R), 
!           germs(C,R), pdepth(R), precip(R), seedsw(C,R)            
      
!  OUTPUTS: ddap(20)(C), dgdds(20)(C), elong(C,R), ems(C,R), 
!           germs(C,R), seedsw(C,R)

      subroutine emerge(dap, daynum, ddap, dgdds, elong, ems, ergdd,
     c gddday, gdds, gddtbg, germgdd, germs, pdepth, precip, seedsw, 
     c year)

      implicit none

      integer  daynum, ems(4), germs(4), pdepth, seedsw, year
      
      real  dap, ddap(20), dgdds(20), elong, ergdd(4), gddday, gdds, 
     c gddtbg, germgdd(4), precip 
     
     
!  check if enough precip (mm) occurred to increase the soil water category
      if ((precip .ge. 5.) .and. (precip .le. 7.) 
     .    .and. (seedsw .ne. 1)) then
              seedsw = seedsw - 1
      elseif ((precip .gt. 7.) .and. (precip .le. 12.) 
     .    .and. (seedsw .ne. 1)) then
              seedsw = seedsw - 2
      elseif (precip .gt. 12.) then
              seedsw = 1
      endif        
      
! Reset seedsw to 1 if it becomes less than 1      
      if(seedsw .lt. 1) then
         seedsw = 1
      endif   
      
! Seeds planted in dust cannot germinate as though they were planted in
! one of the other soil moisture levels just because a significant 
! rainfall event occurs.  The seeds planted in one of the other soil 
! moisture levels have already begun the germination process.  
! Therefore, seeds planted in dust receiving a significant rainfall 
! event should be moved up only to the level of planted in dry conditions
! and then begin to accumulate enough growing degree days to emerge.

      if (seedsw .lt. 4) then
          gddtbg = gddtbg + gddday
      else 
          gddtbg = 0.
      endif              
! check value of gddtbg
 !     print *, 'gddtbg = ', gddtbg

! Check if germination can occur.  This is for seedsw of 1, 2, or 3.
!      if ((germs(1) .eq. 999) .and. (gdds .ge. germgdd(seedsw))) then
! DE changed the folowing line to use gddtbg to handle accumulating gdd,
! for seeds Planted in Dust, on the day that the soil moisture condition
! moves to a better condition, i.e. seedsw = 1, 2, or 3.
      if ((germs(1) .eq. 999) .and. (gddtbg .ge. germgdd(seedsw))) then
          germs(1) = daynum ! day germination has occurred
!          call date1(germs)
      endif    

! If germination has occurred then check if elongation is sufficient to allow 
! emergence to occur.
      if ((germs(1) .ne. 999) .and. (ems(1) .eq. 999)) then
          elong = elong + ergdd(seedsw) * gddday
          if (elong .ge. pdepth)then !pdepth is converted to mm in setup
             ems(1) = daynum
             ems(2) = year
             call date1(ems)
              ddap(1) = dap
	        dgdds(1) = gddtbg ! DE changed gdds to gddbtg
	        print *, 'ems = ', ems !, 'gdds = ', gdds, 'gddtbg = ', gddtbg
	        go to 150
          endif    
      endif

 150  continue
   
      return
      end