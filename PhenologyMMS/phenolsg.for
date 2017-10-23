!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   P H E N O L S G	           *
!  *                                                      de   7/15/05 *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
!  The PHENOLSG subroutine ... finish description here.

!  INPUTS: aepa(C,R), antes(C,R), antss(C,R), dae(R), dap(R), dav(R),
!          daynum(R), ddae(20)(C), ddap(20)(C), ddav(20)(C),   
!          dgdde(20)(C), dgdds(20)(C), dgddv(20)(C), dummy2(15)(R),  
!          endlgs(C,R), first7(C,R), fullbs(C,R), gdde(R),
!          gdds(R), gddv(R), gpds(C,R), halfbs(C,R), hrs(C,R),
!          ies(C,R), joints(C,R), mats(C,R), nolvs(C), tis(C,R)

! OUTPUTS: antes(C,R), antss(C,R), ddae(20)(C), ddap(20)(C), 
!          ddav((20)C), dgdde(20)(C), dgdds(20)(C), dgddv(20)(C), 
!          endlgs(C,R), first7(C,R), fullbs(C,R), gpds(C,R), 
!          halfbs(C,R), hrs(C,R), ies(C,R), joints(C,R), mats(C,R), 
!          nolvs(C), tis(C,R)
             
      subroutine phenolsg(aepa, antes, antss, dae, dap, daynum, 
     c ddae, ddap, dgdde, dgdds, dummy2, endlgs, first7,
     c fullbs, gdde, gdds, gpds, halfbs, hrs, ies, joints, mats, 
     c nolvs, pchron, tis, year)

      implicit none

      integer  antes(4), antss(4), dae, dap, daynum, ddae(20), 
     c ddap(20), endlgs(4), first7, fullbs(4), gpds(4), 
     c halfbs(4), hrs(4), ies(4), joints(4), mats(4), tis(4), year 

      real  aepa, dgdde(20), dgdds(20), dummy2(16), gdde, 
     c gdds, nolvs, pchron    

      if (first7 .eq. 0) then
         aepa = 260. !aepa=120 old value DE changed to 260 to make 
                     ! antse occur after fullbs.
         first7 = 1
      endif

!  Start of tillering:

      if ((tis(1) .eq. 999) .and. (gdde .ge. dummy2(2)) ) then
          tis(1) = daynum
          tis(2) = year
          call date1(tis)
	     ddap(2) = dap
	     ddae(2) = dae
	     dgdds(2) = gdds
	     dgdde(2) = gdde
	     print *, 'tis = ', tis
         go to 150
      endif

!  Start of internode elongation:
!  Internode elongation begins dummy2(3) GDD after start of tillering

      if ((ies(1) .eq. 999) .and. (gdde .ge. (dummy2(3)) )) then
          ies(1) = daynum
          ies(2) = year
          call date1(ies)
	       ddap(3) = dap
	       ddae(3) = dae
	       dgdds(3) = gdds
	       dgdde(3) = gdde
	       print *, 'ies = ', ies
      endif
      
  !  Jointing growth stage prediction:

      if ((joints(1) .eq. 999) .and. 
     c (gdde .ge. (dummy2(3) + dummy2(4)))) then
          joints(1) = daynum
          joints(2) = year
          call date1(joints)
	         ddap(4) = dap
	         ddae(4) = dae
	         dgdds(4) = gdds
	         dgdde(4) = gdde
		     print *, 'joints = ', joints
      endif    
      
!  Growing point differentiation:

      if ((gpds(1) .eq. 999) .and. (gdde .ge. dummy2(5))) then
          gpds(1) = daynum
          gpds(2) = year
          call date1(gpds)
	     ddap(5) = dap
	     ddae(5) = dae
	     dgdds(5) = gdds
	     dgdde(5) = gdde
		 print *, 'gpds = ', gpds
         go to 150

      endif

!  End of leaf growth:

      if ((endlgs(1) .eq. 999) .and. 
     c (gdde .ge. (dummy2(5) + dummy2(6)))) then
          endlgs(1) = daynum
          endlgs(2) = year
          call date1(endlgs)
	     ddap(6) = dap
	     ddae(6) = dae
	     dgdds(6) = gdds
	     dgdde(6) = gdde
	     print *, 'endlgs = ', endlgs
         go to 150

      endif
      
!  Beginnning of anthesis:
!  Allow end of anthesis to occur on same day if enough degree-days have
!  accumulated today.

      if ((antss(1) .eq. 999) .and. (gdde .ge. (dummy2(5) + dummy2(6) +
     .   dummy2(7)))) then
              antss(1) = daynum
              antss(2) = year
              call date1(antss)
	         ddap(7) = dap
	         ddae(7) = dae
	         dgdds(7) = gdds
	         dgdde(7) = gdde
	         print *, 'antss = ', antss
      endif
      
!  End of anthesis:

      if ((antes(1) .eq. 999) .and. (gdde .ge. (dummy2(5) + dummy2(6) +
     .   dummy2(7) + aepa) )) then
!      if ((antes(1) .eq. 999) .and. (gdde .ge. (dummy2(5) + dummy2(6) +
!     .   dummy2(7) + dummy2(8) + dummy2(9) + aepa) )) then

              antes(1) = daynum
              antes(2) = year
              call date1(antes)
	         ddap(12) = dap
	         ddae(12) = dae
	         dgdds(12) = gdds
	         dgdde(12) = gdde
	         print *, 'antes = ', antes

      endif      
      
!  Half bloom growth stage:

      if ((halfbs(1) .eq. 999) .and. (gdde .ge. (dummy2(5) + dummy2(6) +
     .   dummy2(7) + dummy2(8))) ) then
          halfbs(1) = daynum
          halfbs(2) = year
          call date1(halfbs)
	     ddap(8) = dap
	     ddae(8) = dae
	     dgdds(8) = gdds
	     dgdde(8) = gdde
	     print *, 'halfbs = ', halfbs
      endif

!  Full bloom growth stage:

         if ((fullbs(1) .eq. 999) .and. 
     c    (gdde .ge. (dummy2(5) + dummy2(6) + dummy2(7) + dummy2(8) 
     c      + dummy2(9)) )) then
              fullbs(1) = daynum
              fullbs(2) = year
              call date1(fullbs)
	         ddap(9) = dap
	         ddae(9) = dae
	         dgdds(9) = gdds
	         dgdde(9) = gdde
	         print *, 'fullbs = ', fullbs
 
      endif

!  Physiological maturity:

!      if ((mats(1) .eq. 999) .and. (gdde .ge. (dummy2(10)) )) then
      if ((mats(1) .eq. 999) .and. (gdde .ge. (dummy2(5) + dummy2(6) + 
     c   dummy2(7) + dummy2(10)) )) then
              mats(1) = daynum
              mats(2) = year
              call date1(mats)
	         ddap(10) = dap
	         ddae(10) = dae
	         dgdds(10) = gdds
	         dgdde(10) = gdde
	         print *, 'mats = ', mats
      endif
      
! Time to harvest ready:

      if ((hrs(1) .eq. 999) .and. (
     c gdde .ge. (dummy2(5) + dummy2(6) + dummy2(7) + dummy2(10) + 
     c            dummy2(11) ))) then
              hrs(1) = daynum
              hrs(2) = year
              call date1(hrs)
	         ddap(11) = dap
	         ddae(11) = dae
	         dgdds(11) = gdds
	         dgdde(11) = gdde
	         print *, 'hrs = ', hrs
      endif
      
150   continue
  
!  Calculate number of leaves
      if(endlgs(1) .eq. 999) nolvs = gdde / pchron

      return
      end