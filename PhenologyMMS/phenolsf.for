!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   P H E N O L S F	           *
!  *                                                      de   7/15/05 *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
!  The PHENOLSF subroutine ... finish description here.

!  INPUTS: aepasf(R), antes(C,R), antss(C,R), aspasf(R), 
!          boots(C,R), btpasf(R), dae(R), dap(R), dav(R), daynum(R),
!          ddae(C), ddap(C), ddav(C), dgdde(C), dgdds(C), dgddv(C),          
!          drpasf(R), drs(C,R), empasf(R), ems(C), first7(C,R), 
!          fps(C,R), gdde(R), gdds(R), gddv(R), hdpasf(R), heads(C,R),
!          hrpasf(R), hrs(C,R), iepasf(R), ies(C,R), joints(C,R),
!          jtpasf(R), mats(C,R), mtpasf(R), pchron(R), srpasf(R),
!          srs(C,R), tipasf(R), tis(C,R), tspasf(R), tss(C,R)

! OUTPUTS: antes(C,R), antss(C,R), boots(C,R), ddae((20)C), ddap(20)(C), 
!          ddav(20)(C), dgdde(20)(C), dgdds(20)(C), dgddv(20)(C), 
!          drs(C,R), first7(C,R), fps(C,R), heads(C,R), hrs(C,R), 
!          ies(C,R), joints(C,R), mats(C,R), srs(C,R), tis(C,R), 
!          tss(C,R)
              
      subroutine phenolsf(aepa, antes, antss, browns, dae, dap, daynum, 
     c ddae, ddap, dgdde, dgdds, dummy2, first7, gdde, gdds, hrs, ies, 
     c ies2, infls, lf12s, lf4s, lf8s, mats, opens, year, yelows)
     
      implicit none

      integer  antes(4), antss(4), browns(4), dae, dap, daynum, 
     c ddae(20), ddap(20), first7, hrs(4), ies(4), ies2(4), infls(4), 
     c lf12s(4), lf4s(4), lf8s(4), mats(4), opens(4), year, yelows(4)  
         
      real  aepa, dgdde(20), dgdds(20), dummy2(15), gdde, gdds

! Is aepa needed for sunflower and if so what should be the value?     
      if (first7 .eq. 0) then
        aepa = 120
        first7 = 1
      endif

!  4th leaf stage - V4

      if ((lf4s(1) .eq. 999) .and. (gdde .ge. dummy2(2))) then
          lf4s(1) = daynum
          lf4s(2) = year
          call date1(lf4s)
	     ddap(2) = dap
	     ddae(2) = dae
	     dgdds(2) = gdds
	     dgdde(2) = gdde
	     print *, 'lf4s = ', lf4s
         go to 150
      endif

! 8th leaf stage - V8
      if ((lf8s(1) .eq. 999) .and. (gdde .ge. dummy2(2) + dummy2(3))) 
     c  then
          lf8s(1) = daynum
          lf8s(2) = year
          call date1(lf8s)
	     ddap(3) = dap
	     ddae(3) = dae
	     dgdds(3) = gdds
	     dgdde(3) = gdde
		 print *, 'lf8s = ', lf8s
         go to 150

      endif

! 12th leaf stage - V12

      if ((lf12s(1) .eq. 999) .and. (gdde .ge. (dummy2(2) + dummy2(3) + 
     c dummy2(4))))  then
          lf12s(1) = daynum
          lf12s(2) = year
          call date1(lf12s)
	     ddap(4) = dap
	     ddae(4) = dae
	     dgdds(4) = gdds
	     dgdde(4) = gdde
	     print *, 'lf12s = ', lf12s
         go to 150

      endif

!  Inflorescence becomes visible - R1

      if ((infls(1) .eq. 999) .and. (gdde .ge. (dummy2(2) + dummy2(3) + 
     1  dummy2(4) + dummy2(5)) )) then
          infls(1) = daynum
          infls(2) = year
          call date1(infls)
	     ddap(5) = dap
	     ddae(5) = dae
	     dgdds(5) = gdds
	     dgdde(5) = gdde
	     print *, 'infls = ', infls
      endif

!  Start of internode elongation directly below the base of the 
!  inflorescence - R2

      if ((ies(1) .eq. 999) .and. (gdde .ge. (dummy2(2) + dummy2(3) + 
     c dummy2(4) + dummy2(5) + dummy2(6)) )) then
          ies(1) = daynum
          ies(2) = year
          call date1(ies)
	       ddap(6) = dap
	       ddae(6) = dae
	       dgdds(6) = gdds
	       dgdde(6) = gdde
	       print *, 'ies = ', ies
      endif

!  Internode elongation continues.  The internode below the 
!  reproductive bud continues to elongate and the inflorescence is lifted 
!  more than 2 cm above the surrounding leaves. - R3

         if ((ies2(1) .eq. 999) .and. (gdde .ge. (dummy2(2) + dummy2(3) 
     c     + dummy2(4) + dummy2(5) + dummy2(6) + dummy2(7)) )) then
              ies2(1) = daynum
              ies2(2) = year
              call date1(ies2)
	         ddap(7) = dap
	         ddae(7) = dae
	         dgdds(7) = gdds
	         dgdde(7) = gdde
	         print *, 'ies2 = ', ies2
 
      endif

!  The inflorescence begins to open and small ray flowers are visible - R4

      if ((opens(1) .eq. 999) .and. (gdde .ge. (dummy2(2) + dummy2(3) +
     c   dummy2(4) + dummy2(5) + dummy2(6) + dummy2(7) + dummy2(8)))) 
     c     then
              opens(1) = daynum
              opens(2) = year
              call date1(opens)
	         ddap(8) = dap
	         ddae(8) = dae
	         dgdds(8) = gdds
	         dgdde(8) = gdde
		     print *, 'opens = ', opens
      endif
      
150   continue

!  Anthesis begins.  The mature ray flowers are fully extended
!  and the disk flowers are visible - R5.

      if ((antss(1) .eq. 999) .and. (gdde .ge. (dummy2(2) + dummy2(3) +
     c   dummy2(4) + dummy2(5) + dummy2(6) + dummy2(7) + dummy2(8) + 
     c   dummy2(9)))) then
              antss(1) = daynum
              antss(2) = year
              call date1(antss)
	         ddap(9) = dap
	         ddae(9) = dae
	         dgdds(9) = gdds
	         dgdde(9) = gdde
	         print *, 'antss = ', antss

      endif

!  Anthesis is complete.  The ray flowers are wilting. - R6

      if ((antes(1) .eq. 999) .and. (gdde .ge. (dummy2(2) + dummy2(3) +
     c   dummy2(4) + dummy2(5) + dummy2(6) + dummy2(7) + dummy2(8) + 
     c   dummy2(9) + dummy2(10)))) then
              antes(1) = daynum
              antes(2) = year
              call date1(antes)
	         ddap(10) = dap
	         ddae(10) = dae
	         dgdds(10) = gdds
	         dgdde(10) = gdde
	         print *, 'antes = ', antes
      endif

!  The back of the inflorescence is turning to a light yellow color. - R7

      if ((yelows(1) .eq. 999) .and. (gdde .ge. (dummy2(2) + dummy2(3) +
     c   dummy2(4) + dummy2(5) + dummy2(6) + dummy2(7) + dummy2(8) + 
     c   dummy2(9) + dummy2(10) + dummy2(11)))) 
     c then
              yelows(1) = daynum
              yelows(2) = year
              call date1(yelows)
	         ddap(11) = dap
	         ddae(11) = dae
	         dgdds(11) = gdds
	         dgdde(11) = gdde
	         print *, 'yelows = ', yelows
      endif

!  The back of the inflorescence is yellow but the bracts are still green.  
!  There may be some brown spotting on the back of the head. - R8

      if ((browns(1) .eq. 999) .and. (gdde .ge. (dummy2(2) + dummy2(3) +
     c   dummy2(4) + dummy2(5) + dummy2(6) + dummy2(7) + dummy2(8) + 
     c   dummy2(9) + dummy2(10) + dummy2(11) + dummy2(12)))) then
              browns(1) = daynum
              browns(2) = year
              call date1(browns)
	         ddap(12) = dap
	         ddae(12) = dae
	         dgdds(12) = gdds
	         dgdde(12) = gdde
	         print *, 'browns = ', browns

      endif

!  Physiological maturity.  The bracts are yellow and brown and much of the back of 
!  the head may be brown. - R9

      if ((mats(1) .eq. 999) .and. (gdde .ge. (dummy2(2) + dummy2(3) +
     c   dummy2(4) + dummy2(5) + dummy2(6) + dummy2(7) + dummy2(8) + 
     c   dummy2(9) + dummy2(10) + dummy2(11) + dummy2(12) + dummy2(13))
     c   )) then
              mats(1) = daynum
              mats(2) = year
              call date1(mats)
	         ddap(13) = dap
	         ddae(13) = dae
	         dgdds(13) = gdds
	         dgdde(13) = gdde
	         print *, 'mats = ', mats

      endif
      
!  Harvest Ready.  

      if ((hrs(1) .eq. 999) .and. (gdde .ge. (dummy2(2) + dummy2(3) +
     c   dummy2(4) + dummy2(5) + dummy2(6) + dummy2(7) + dummy2(8) + 
     c   dummy2(9) + dummy2(10) + dummy2(11) + dummy2(12) + dummy2(13)
     c   + dummy2(14)))) then
              hrs(1) = daynum
              hrs(2) = year
              call date1(hrs)
	         ddap(14) = dap
	         ddae(14) = dae
	         dgdds(14) = gdds
	         dgdde(14) = gdde
	         print *, 'hrs = ', hrs

      endif
           
      return
      end