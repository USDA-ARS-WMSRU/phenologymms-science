!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   P H E N O L C N	           *
!  *                                                      de   7/15/05 *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
!  The PHENOLCN subroutine ... finish description here.

!  INPUTS: aepacn(R), aifs(C,R), antes(C,R), antss(C,R), aspacn(R), 
!          boots(C,R), btpacn(R), daynum(R), dae(R), dap(R), dav(R),
!          ddae(C), ddap(C), ddav(C), dgdde(C), dgdds(C), dgddv(C),
!          drpacn(R), drs(C,R), empacn(R), ems(C), first7(C,R), 
!          fps(C,R), gdde(R), gdds(R), gddv(R), hdpacn(R), heads(C,R),
!          hrpacn(R), hrs(C,R), iepacn(R), ies(C,R), joints(C,R), 
!          jtpacn(R), mats(C,R), mtpacn(R), pchron(R), srpacn(R),  
!          srs(C, R), tipacn(R), tis(C,R), tspacn(R)           

! OUTPUTS: aifs(C,R), antes(C,R), antss(C,R), boots(C,R), ddae((20)C), 
!          ddap(20)(C), ddav(20)(C), dgdde(20)(C), dgdds(20)(C),  
!          dgddv(20)(C), drs(C,R), first7(C,R), fps(C,R), heads(C,R), 
!          hrs(C,R), ies(C,R), joints(C,R), mats(C,R), srs(C,R), 
!          tis(C,R), tss(C,R)      

      subroutine phenolcn(aepa, antss, blstrs, dae, dap, daynum, ddae, 
     c ddap, dents, dgdde, dgdds, doughs, dummy2, ears, first7, gdde, 
     c gdds, hrs, ies, lf12s, lf4s, mats, milks, silks, tsints, 
     c year)
 
      implicit none

      integer  antss(4), blstrs(4), dae, dap, daynum, ddae(20), 
     c ddap(20), dents(4), doughs(4), ears(4), first7, hrs(4), ies(4), 
     c lf12s(4), lf4s(4), mats(4), milks(4), silks(4), 
     c tsints(4), year  
    
      real  aepa, dgdde(20), dgdds(20), dummy2(16), gdde, gdds

! Check what aepa should be set to for corn.  
!   aepa = anthesis end parameter     
      if (first7 .eq. 0) then
         aepa = 120
         first7 = 1
      endif

!  4th leaf stage - V4:

      if ((lf4s(1) .eq. 999) .and. (gdde .ge. dummy2(2))) then
          lf4s(1) = daynum
          lf4s(2) = year
          call date1(lf4s)
	     ddap(2) = dap
	     ddae(2) = dae
	     dgdds(2) = gdds
	     dgdde(2) = gdde
	     print *, 'lf4s = ', lf4s
!      endif     
      
!  12th Leaf stage V12

      elseif ((lf12s(1) .eq. 999) .and. (gdde .ge. (dummy2(2))  
     c    + dummy2(6)  )) then
          lf12s(1) = daynum
          lf12s(2) = year
          call date1(lf12s)
	     ddap(6) = dap
	     ddae(6) = dae
	     dgdds(6) = gdds
	     dgdde(6) = gdde
	     print *, 'lf12s = ', lf12s
!      endif

!  Silking growth stage.  This is the first reproductive stage R1.

      elseif ((silks(1) .eq. 999) .and. (gdde .ge. (dummy2(2) 
     c  + dummy2(6) + dummy2(8)) )) then
              silks(1) = daynum
              silks(2) = year
              call date1(silks)
	         ddap(8) = dap
	         ddae(8) = dae
	         dgdds(8) = gdds
	         dgdde(8) = gdde
	         print *, 'silks = ', silks
!      endif

!  Blister growth stage - R2

      elseif ((blstrs(1) .eq. 999) .and. (gdde .ge. (dummy2(2) 
     1   + dummy2(6) + dummy2(8) + dummy2(9)) )) then
              blstrs(1) = daynum
              blstrs(2) = year
              call date1(blstrs)
	         ddap(9) = dap
	         ddae(9) = dae
	         dgdds(9) = gdds
	         dgdde(9) = gdde
	         print *, 'blstrs = ', blstrs
!      endif

!  Milk growth stage - R3

      elseif ((milks(1) .eq. 999) .and. (gdde .ge. (dummy2(2) 
     c  + dummy2(6) + dummy2(8) + dummy2(9)+ dummy2(10)))) 
     c  then
              milks(1) = daynum
              milks(2) = year
              call date1(milks)
	         ddap(10) = dap
	         ddae(10) = dae
	         dgdds(10) = gdds
	         dgdde(10) = gdde
	         print *, 'milks = ', milks
!      endif

!  Dough growth stage - R4

      elseif ((doughs(1) .eq. 999) .and. (gdde .ge. (dummy2(2) 
     c  + dummy2(6) + dummy2(8) + dummy2(9) + dummy2(10) 
     c  + dummy2(11)) )) then
              doughs(1) = daynum
              doughs(2) = year
              call date1(doughs)
	         ddap(11) = dap
	         ddae(11) = dae
	         dgdds(11) = gdds
	         dgdde(11) = gdde
	         print *, 'doughs = ', doughs

!      endif

!  Dent growth stage - R5

      elseif ((dents(1) .eq. 999) .and. (gdde .ge. (dummy2(2) 
     c  + dummy2(6) + dummy2(8) + dummy2(9) + dummy2(10) + dummy2(11) 
     c  + dummy2(12)) )) then
              dents(1) = daynum
              dents(2) = year
              call date1(dents)
	         ddap(12) = dap
	         ddae(12) = dae
	         dgdds(12) = gdds
	         dgdde(12) = gdde
	         print *, 'dents = ', dents

!      endif

! Physiological Maturity growth stage - R6

      elseif ((mats(1) .eq. 999) .and. (gdde .ge. (dummy2(2) + dummy2(6) 
     c  + dummy2(8) + dummy2(9) + dummy2(10) + dummy2(11) + dummy2(12) + 
     c  dummy2(13)) )) then
              mats(1) = daynum
              mats(2) = year
              call date1(mats)
	         ddap(13) = dap
	         ddae(13) = dae
	         dgdds(13) = gdds
	         dgdde(13) = gdde
	         print *, 'mats = ', mats

 !     endif
      
! Harvest Ready growth stage       
      elseif ((hrs(1) .eq. 999) .and. (gdde .ge. (dummy2(2)  
     c + dummy2(6) + dummy2(8) + dummy2(9) + dummy2(10) + dummy2(11)  
     c + dummy2(12) + dummy2(13) + dummy2(14)))) then
              hrs(1) = daynum
              hrs(2) = year
              call date1(hrs)
	         ddap(14) = dap
	         ddae(14) = dae
	         dgdds(14) = gdds
	         dgdde(14) = gdde
	         print *, 'hrs = ', hrs
      endif

!  Tassel initiation growth stage:

      if ((lf4s(1) .ne. 999) .and. (tsints(1) .eq. 999) .and.  
     c     (gdde .ge. (dummy2(2) + dummy2(3)))) then
          tsints(1) = daynum
          tsints(2) = year
          call date1(tsints)
	     ddap(3) = dap
	     ddae(3) = dae
	     dgdds(3) = gdds
	     dgdde(3) = gdde
	     print *, 'tsints = ', tsints
      endif      

! Ear formation stage
      if ((lf4s(1) .ne. 999) .and. (ears(1) .eq. 999) .and.  
     c  (gdde .ge. (dummy2(2)+ dummy2(4)))) then
          ears(1) = daynum
          ears(2) = year
          call date1(ears)
	     ddap(4) = dap
	     ddae(4) = dae
	     dgdds(4) = gdds
	     dgdde(4) = gdde
		 print *, 'ears = ', ears
      endif
      
!  Start of internode elongation:

      if ((lf4s(1) .ne. 999) .and. (ies(1) .eq. 999) .and.  
     c (gdde .ge. (dummy2(2) + dummy2(5)))) then
            ies(1) = daynum
            ies(2) = year
            call date1(ies)
	       ddap(5) = dap
	       ddae(5) = dae
	       dgdds(5) = gdds
	       dgdde(5) = gdde
	       print *, 'ies = ', ies
      endif

!  Tasseling stage
         if ((lf12s(1) .ne. 999) .and. (antss(1) .eq. 999) .and.  
     c     (gdde .ge. (dummy2(2) + dummy2(6) + dummy2(7)) )) then 
              antss(1) = daynum
              antss(2) = year
              call date1(antss)
	         ddap(7) = dap
	         ddae(7) = dae
	         dgdds(7) = gdds
	         dgdde(7) = gdde
	         print *, 'antss = ', antss
      endif

      return
      end