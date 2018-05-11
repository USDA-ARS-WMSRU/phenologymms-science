!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   P H E N O L H M	           *
!  *                                                      de   7/14/05 *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
!  The PHENOLHM subroutine ... finish description here.

!  INPUTS:  aepa(C), antes(C,R), antss(C,R), boots(C,R), dae(R),  
!           dap(R), dav(R), daynum(R), ddae(20)(C), ddap(20)(C),  
!           ddav(20)(C), dgdde(20)(C), dgdds(20)(C), dgddv(20)(C), 
!           drs(C,R), dummy2(15)(R), first7(C,R), fps(C,R), 
!           gdde(R), gdds(R), gddv(R), heads(C,R), hrs(C,R), ies(C,R), 
!           joints(C,R), mats(C,R), nolvs(C), pchron(R), srs(C,R), 
!           tis(C,R), tss(C,R)             

!  OUTPUTS: antes(C,R), antss(C,R), boots(C,R), ddae(20)(C), 
!          ddap(20)(C), ddav((20)C), dgdde(20)(C), dgdds(20)(C), 
!          dgddv(20)(C), drs(C,R), first7(C,R), fps(C,R), heads(C,R), 
!          hrs(C,R), ies(C,R), joints(C,R), mats(C,R), nolvs(C), 
!          srs(C,R), tis(C,R), tss(C,R) 

      subroutine phenolhm(aepa, antes, antss, boots, dae, dap,  
     c daynum, ddae, ddap, dgdde, dgdds, drs, dummy2, 
     c first7, fps, gdde, gdds, heads, hrs, ies, joints, mats, 
     c nolvs, pchron, srs, tis, tss, year)

      implicit none

      integer  antes(4), antss(4), boots(4), dae, dap, daynum,  
     c ddae(20), ddap(20), drs(4), first7, fps(4), heads(4), 
     c hrs(4), ies(4), joints(4), mats(4), srs(4), tis(4), tss(4), 
     c year  

      real  aepa, dgdde(20), dgdds(20), dummy2(16),gdde, 
     c gdds, nolvs, pchron     

      if (first7 .eq. 0) then
         aepa = 120.
         first7 = 1
      endif

!  Start of tillering:

      if ((tis(1) .eq. 999) .and. (gdde .ge. dummy2(2))) then
          tis(1) = daynum
          tis(2) = year
          call date1(tis)
	     ddap(2) = dap
	     ddae(2) = dae
	     dgdds(2) = gdds
	     dgdde(2) = gdde
	     print *, 'tis = ', tis
!      endif

!  Single ridge growth stage:
!  Single ridge occurs dummy2(3) GDD after vernalization, which is
!  assumed to have occurred by midwinter (Jan. 1 or July 1) for winter
!  wheat, or after emergence for spring wheat and spring barley.

      elseif ((srs(1) .eq. 999) .and. (gdde .ge. dummy2(3))) then
          srs(1) = daynum
          srs(2) = year
          call date1(srs)
	     ddap(3) = dap
	     ddae(3) = dae
	     dgdds(3) = gdds
	     dgdde(3) = gdde
		 print *, 'srs = ', srs
!      endif

!  Double ridge growth stage:
!  Double ridge occurs dummy2(4) GDD phyllochrons after single ridge growth
!  stage.  Do not allow additional stages to occur on same day DR
!  is reached.

      elseif ((drs(1) .eq. 999) .and. 
     c     (gdde .ge. (dummy2(3) + dummy2(4)))) then
          drs(1) = daynum
          drs(2) = year
          call date1(drs)
	     ddap(4) = dap
	     ddae(4) = dae
	     dgdds(4) = gdds
	     dgdde(4) = gdde
	     print *, 'drs = ', drs
!      endif
      
      
!  Terminal spikelet stage:

      elseif ((tss(1) .eq. 999) .and. (ies(1) .eq. 999) .and.  
     1  (gdde .ge. (dummy2(3) + dummy2(4) + dummy2(5)) )) then
          tss(1) = daynum
          tss(2) = year
          call date1(tss)
	     ddap(5) = dap
	     ddae(5) = dae
	     dgdds(5) = gdds
	     dgdde(5) = gdde
	     print *, 'tss = ', tss
!      endif

!  Start of internode elongation:
!  Internode elongation begins dummy2(5) GDD phyllochrons after double ridge
!  growth stage.

!      if ((ies(1) .eq. 999) .and. (gdde .ge. (dummy2(3) + dummy2(4)
!     1   + dummy2(5)) )) then
          ies(1) = daynum
          ies(2) = year
          call date1(ies)
	       ddap(6) = dap
	       ddae(6) = dae
	       dgdds(6) = gdds
	       dgdde(6) = gdde
	       print *, 'ies = ', ies
!      endif

!  Jointing growth stage prediction:

      elseif ((joints(1) .eq. 999) .and. (gdde .ge. (dummy2(3)
     1   + dummy2(4) + dummy2(5) + dummy2(6)) )) then
              joints(1) = daynum
              joints(2) = year
              call date1(joints)
	         ddap(8) = dap
	         ddae(8) = dae
	         dgdds(8) = gdds
	         dgdde(8) = gdde
		     print *, 'joints = ', joints
!      endif

!  Booting growth stage: This is defined as flag leaf has
!  completed its growth.  

      elseif ((boots(1) .eq. 999) .and. (gdde .ge. (dummy2(3)
     1   + dummy2(4) + dummy2(5) + dummy2(6) + dummy2(7)) )) then
              boots(1) = daynum
              boots(2) = year
              call date1(boots)
	         ddap(9) = dap
	         ddae(9) = dae
	         dgdds(9) = gdds
	         dgdde(9) = gdde
	         print *, 'boots = ', boots
!      endif

!  Heading growth stage:
!  If enough GDD have passed, then heading begins.  Go on to
!  code for next stage since anthesis is allowed to occur on same day
!  as heading if enough degree-days accumulated today.

      elseif ((heads(1) .eq. 999) .and. (gdde .ge. (dummy2(3)
     c  + dummy2(4) + dummy2(5) + dummy2(6) + dummy2(7) 
     c  + dummy2(8)) )) then
              heads(1) = daynum
              heads(2) = year
              call date1(heads)
	         ddap(10) = dap
	         ddae(10) = dae
	         dgdds(10) = gdds
	         dgdde(10) = gdde
	         print *, 'heads = ', heads
!	     endif    

!  Beginnning of anthesis:
!  Allow end of anthesis to occur on same day if enough degree-days have
!  accumulated today.

      elseif ((antss(1) .eq. 999) .and. (gdde .ge. (dummy2(3)
     c   + dummy2(4) + dummy2(5) + dummy2(6) + dummy2(7)
     c   + dummy2(8) + dummy2(9)))) then
              antss(1) = daynum
              antss(2) = year
              call date1(antss)
	         ddap(11) = dap
	         ddae(11) = dae
	         dgdds(11) = gdds
	         dgdde(11) = gdde
	         print *, 'antss = ', antss	         
!      endif

!  Physiological maturity:

      elseif ((mats(1) .eq. 999) .and. (gdde .ge. (dummy2(3)
     c  + dummy2(4) + dummy2(5) + dummy2(6) + dummy2(7) 
     c  + dummy2(8) + dummy2(9) + dummy2(10)) )) then
              mats(1) = daynum
              mats(2) = year
              call date1(mats)
	         ddap(13) = dap
	         ddae(13) = dae
	         dgdds(13) = gdds
	         dgdde(13) = gdde
	         print *, 'mats = ', mats
!      endif

! Time to harvest ripe:

      elseif ((hrs(1) .eq. 999) .and. (gdde .ge. (dummy2(3)
     c  + dummy2(4) + dummy2(5) + dummy2(6) + dummy2(7) + dummy2(8) 
     c  + dummy2(9) + dummy2(10) + dummy2(11)) )) then
              hrs(1) = daynum
              hrs(2) = year
              call date1(hrs)
	         ddap(14) = dap
	         ddae(14) = dae
	         dgdds(14) = gdds
	         dgdde(14) = gdde
	         print *, 'hrs = ', hrs
      endif

!  Flower primordium initiation:
!  Flower primordium initiation begins 0.3 phyllochrons after
!  double ridge.

      if ((drs(1) .ne. 999) .and. (fps(1) .eq. 999) .and. 
     .     (gdde .ge. (dummy2(3) + dummy2(4) + (0.3*pchron)) )) then
              fps(1) = daynum
              fps(2) = year
              call date1(fps)
	         ddap(7) = dap
	         ddae(7) = dae
	         dgdds(7) = gdds
	         dgdde(7) = gdde
	         print *, 'fps = ', fps
      endif

!  End of anthesis:

      if ((antss(1) .ne. 999) .and. (antes(1) .eq. 999) .and. 
     c   (gdde .ge. (dummy2(3) + dummy2(4) + dummy2(5) + dummy2(6) 
     c   + dummy2(7) + dummy2(8) + dummy2(9) + aepa) )) then
              antes(1) = daynum
              antes(2) = year
              call date1(antes)
	         ddap(12) = dap
	         ddae(12) = dae
	         dgdds(12) = gdds
	         dgdde(12) = gdde
	         print *, 'antes = ', antes
      endif


!  Calculate number of leaves
      if(boots(1) .eq. 999) nolvs = gdde / pchron

      return
      end