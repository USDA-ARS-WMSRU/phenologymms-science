!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   P H E N O L SY	           *
!  *                                                    de   10/12/17  *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
!  The PHENOLSY subroutine ... finish description here.

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
              
      subroutine phenolsy (aepa, antss, bmats, cots, dae, dap, daynum, 
     c ddae, ddap, dgdde, dgdds, dummy2, epods, eseeds, first7, gdde, 
     c gdds, hrs, lf1s, lf2s, lf3s, lf4s, lf5s, mats, mffls, mpods, 
     c mseeds, year)
     
      implicit none

      integer  antss(4), bmats(4), cots(4), dae, dap, daynum, 
     c ddae(20), ddap(20), epods(4), eseeds(4), first7, hrs(4), 
     c lf1s(4), lf2s(4), lf3s(4), lf4s(4), lf5s(4), mats(4), mffls(4),  
     c mpods(4), mseeds(4), year 
         
      real  aepa, dgdde(20), dgdds(20), dummy2(16), gdde, gdds

! Is aepa needed for soybean and if so what should be the value?     
      if (first7 .eq. 0) then
        aepa = 120
        first7 = 1
      endif

!  Cotyledonary leaf stage - VC
      if ((cots(1) .eq. 999) .and. (gdde .ge. dummy2(2))) then
          cots(1) = daynum
          cots(2) = year
          call date1(cots)
	     ddap(2) = dap
	     ddae(2) = dae
	     dgdds(2) = gdds
	     dgdde(2) = gdde
	     print *, 'cots = ', cots
         go to 150
!      endif

! 1st trifoliolate leaf stage - V1
      elseif ((lf1s(1) .eq. 999) .and. (gdde .ge. dummy2(2)  
     c  + dummy2(3))) then
          lf1s(1) = daynum
          lf1s(2) = year
          call date1(lf1s)
	     ddap(3) = dap
	     ddae(3) = dae
	     dgdds(3) = gdds
	     dgdde(3) = gdde
		 print *, 'lf1s = ', lf1s
         go to 150

!      endif

! 2nd trifoliolate leaf stage - V2
      elseif ((lf2s(1) .eq. 999) .and. (gdde .ge. (dummy2(2)  
     c + dummy2(3) + dummy2(4))))  then
          lf2s(1) = daynum
          lf2s(2) = year
          call date1(lf2s)
	     ddap(4) = dap
	     ddae(4) = dae
	     dgdds(4) = gdds
	     dgdde(4) = gdde
	     print *, 'lf2s = ', lf2s
         go to 150

!      endif

!  3rd trifoliolate leaf stage - V3
      elseif ((lf3s(1) .eq. 999) .and. (gdde .ge. (dummy2(2)  
     c  + dummy2(3) + dummy2(4) + dummy2(5)) )) then
          lf3s(1) = daynum
          lf3s(2) = year
          call date1(lf3s)
	     ddap(5) = dap
	     ddae(5) = dae
	     dgdds(5) = gdds
	     dgdde(5) = gdde
	     print *, 'lf3s = ', lf3s
!      endif

!  4th trifoliolate leaf stage - V4
      elseif ((lf4s(1) .eq. 999) .and. (gdde .ge. (dummy2(2)  
     c + dummy2(3) + dummy2(4) + dummy2(5) + dummy2(6)) )) then
          lf4s(1) = daynum
          lf4s(2) = year
          call date1(lf4s)
	       ddap(6) = dap
	       ddae(6) = dae
	       dgdds(6) = gdds
	       dgdde(6) = gdde
	       print *, 'lf4s = ', lf4s
!      endif
    
!  5th trifoliolate leaf stage - V5
      elseif ((lf5s(1) .eq. 999) .and. (gdde .ge. (dummy2(2) 
     c + dummy2(3) + dummy2(4) + dummy2(5) + dummy2(6) 
     c + dummy2(7)) )) then
          lf5s(1) = daynum
          lf5s(2) = year
          call date1(lf5s)
	       ddap(7) = dap
	       ddae(7) = dae
	       dgdds(7) = gdds
	       dgdde(7) = gdde
	       print *, 'lf5s = ', lf5s
!      endif
    
!  One open flower at any node. (Beginning Bloom) - R1
         elseif ((antss(1) .eq. 999) .and. (gdde .ge. (dummy2(2) 
     c     + dummy2(3) + dummy2(4) + dummy2(5) + dummy2(6) + dummy2(7) 
     c     + dummy2(8)))) then
              antss(1) = daynum
              antss(2) = year
              call date1(antss)
	         ddap(8) = dap
	         ddae(8) = dae
	         dgdds(8) = gdds
	         dgdde(8) = gdde
	         print *, 'antss = ', antss
 
!      endif

! One open flower at one of the 2 uppermost nodes (Full Bloom) - R2
      elseif ((mffls(1) .eq. 999) .and. (gdde .ge. (dummy2(2) 
     c   + dummy2(3) + dummy2(4) + dummy2(5) + dummy2(6) + dummy2(7) 
     c   + dummy2(8) + dummy2(9)))) then
              mffls(1) = daynum
              mffls(2) = year
              call date1(mffls)
	         ddap(9) = dap
	         ddae(9) = dae
	         dgdds(9) = gdds
	         dgdde(9) = gdde
		     print *, 'mffls = ', mffls
!      endif
      
150   continue

!  Pod 3/16" long at one of the four uppermost nodes (Beginning Pod) - R3
      elseif ((epods(1) .eq. 999) .and. (gdde .ge. (dummy2(2) 
     c   + dummy2(3) + dummy2(4) + dummy2(5) + dummy2(6) + dummy2(7) 
     c   + dummy2(8) + dummy2(9) + dummy2(10)))) then
              epods(1) = daynum
              epods(2) = year
              call date1(epods)
	         ddap(10) = dap
	         ddae(10) = dae
	         dgdds(10) = gdds
	         dgdde(10) = gdde
	         print *, 'epods = ', epods

!      endif

!  Pod is 3/4" long at one of the four uppermost nodes. (Full Pod) - R4
      elseif ((mpods(1) .eq. 999) .and. (gdde .ge. (dummy2(2) 
     c   + dummy2(3) + dummy2(4) + dummy2(5) + dummy2(6) + dummy2(7)  
     c   + dummy2(8) + dummy2(9) + dummy2(10) + dummy2(11)))) then
              mpods(1) = daynum
              mpods(2) = year
              call date1(mpods)
	         ddap(11) = dap
	         ddae(11) = dae
	         dgdds(11) = gdds
	         dgdde(11) = gdde
	         print *, 'mpods = ', mpods
!      endif

!  Seed is 1/8" long in a pod at one of the four uppermost nodes (Beginning Seed) - R5
      elseif ((eseeds(1) .eq. 999) .and. (gdde .ge. (dummy2(2) 
     c   + dummy2(3) + dummy2(4) + dummy2(5) + dummy2(6) + dummy2(7)  
     c   + dummy2(8) + dummy2(9) + dummy2(10) + dummy2(11)  
     c   + dummy2(12))))then
              eseeds(1) = daynum
              eseeds(2) = year
              call date1(eseeds)
	         ddap(12) = dap
	         ddae(12) = dae
	         dgdds(12) = gdds
	         dgdde(12) = gdde
	         print *, 'eseeds = ', eseeds
!      endif

!  Pod containing a green seed that fills the pod cavity at one of the four 
!  uppermost nodes. (Full Seed) - R6
      elseif ((mseeds(1) .eq. 999) .and. (gdde .ge. (dummy2(2) 
     c   + dummy2(3) + dummy2(4) + dummy2(5) + dummy2(6) + dummy2(7) 
     c   + dummy2(8) + dummy2(9) + dummy2(10) + dummy2(11) 
     c   + dummy2(12) + dummy2(13)))) then
              mseeds(1) = daynum
              mseeds(2) = year
              call date1(mseeds)
	         ddap(13) = dap
	         ddae(13) = dae
	         dgdds(13) = gdds
	         dgdde(13) = gdde
	         print *, 'mseeds = ', mseeds

!      endif

!  One pod anywhere with its mature color (Beginning Maturity) - R7
      elseif ((bmats(1) .eq. 999) .and. (gdde .ge. (dummy2(2) 
     c   + dummy2(3) + dummy2(4) + dummy2(5) + dummy2(6) + dummy2(7) 
     c   + dummy2(8) + dummy2(9) + dummy2(10) + dummy2(11) + dummy2(12) 
     c   + dummy2(13) + dummy2(14)))) then
              bmats(1) = daynum
              bmats(2) = year
              call date1(bmats)
	          ddap(14) = dap
	          ddae(14) = dae
	          dgdds(14) = gdds
              dgdde(14) = gdde
	         print *, 'bmats = ', bmats

!      endif
    
!  95% of the pods have reached their mature color (Full Maturity) - R8
      elseif ((mats(1) .eq. 999) .and. (gdde .ge. (dummy2(2) 
     c   + dummy2(3) + dummy2(4) + dummy2(5) + dummy2(6) + dummy2(7) 
     c   + dummy2(8) + dummy2(9) + dummy2(10) + dummy2(11) + dummy2(12) 
     c   + dummy2(13) + dummy2(14) + dummy2(15)))) then
              mats(1) = daynum
              mats(2) = year
              call date1(mats)
	          ddap(15) = dap
	          ddae(15) = dae
	          dgdds(15) = gdds
              dgdde(15) = gdde
	         print *, 'mats = ', mats

!      endif    
      
!  Soybeans are at 15% moisture or less (Harvest Ready) - R8  
      elseif ((hrs(1) .eq. 999) .and. (gdde .ge. (dummy2(2) + dummy2(3) 
     c   + dummy2(4) + dummy2(5) + dummy2(6) + dummy2(7) + dummy2(8)  
     c   + dummy2(9) + dummy2(10) + dummy2(11) + dummy2(12) 
     c   + dummy2(13) + dummy2(14) + dummy2(15) + dummy2(16)))) then
              hrs(1) = daynum
              hrs(2) = year
              call date1(hrs)
	         ddap(16) = dap
	         ddae(16) = dae
	         dgdds(16) = gdds
	         dgdde(16) = gdde
	         print *, 'hrs = ', hrs

      endif
           
      return
      end