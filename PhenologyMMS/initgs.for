!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   I N I T G S                  *
!  *                                                      de   2/27/06 *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
!  The INIT subroutine initializes variables relating to growth stages.


!  INPUTS:  aifs(C), antes(C), antss(C), boots(C), drs(C), ems(C), 
!           endlgs(C), first7(C), fps(C), fullbs(C), germs(C), gpds(C), 
!           halfbs(C), heads(C), hrs(C), ies(C), joints(C), mats(C), 
!           nolvs(C), srs(C), tis(C), tss(C), verns(C)
                
 

!  OUTPUTS: aifs(C), antes(C), antss(C), boots(C), drs(C), ems(C), 
!           endlgs(C), first7(C), fps(C), fullbs(C), germs(C), gpds(C), 
!           halfbs(C), heads(C), hrs(C), ies(C), joints(C), mats(C), 
!           nolvs(C), srs(C), tis(C), tss(C), verns(C)  
 
	
      subroutine initgs(aifs, antes, antss, blstrs, boots, browns, 
     c dents, doughs, drs, dummy1, dummy2, ears, ems, endlgs, first7, 
     c fps, fullbs, germs, gpds, halfbs, heads, hrs, ies, ies2, infls, 
     c joints, lf12s, lf4s, lf8s, mats, milks, nolvs, opens, silks, srs,
     c tis, tsints, tss, verns, yelows)
     

      implicit none
      
      integer  aifs(4), antes(4), antss(4), blstrs(4), boots(4), 
     c browns(4), d, dents(4), dn, doughs(4), drs(4), ears(4), ems(4), 
     c endlgs(4), first7, fps(4), fullbs(4), germs(4), gpds(4), 
     c halfbs(4), heads(4), hrs(4), i, ies(4), ies2(4), infls(4), 
     c joints(4), lf12s(4), lf4s(4), lf8s(4), m, mats(4), milks(4), 
     c nolvs, opens(4), silks(4), srs(4), tis(4), tsints(4), tss(4), 
     c verns, y, yelows(4) 
     
      real dummy2(15)
      
      character *22 dummy1(15)

! Initialize variables
      dn = 999 !daynum
      y = 0000 !year
      m = 0    !month
      d = 0    !day
      i = 0    !counter 
      verns = 999   !vernalization

! Initialize Arrays:

!  Character String Array      
      do 30 i = 1, 15
	  dummy1(i) = ""
 30	continue
      
!  Array for values from Customize Run screen
      do 40 i = 1, 15
        dummy2(i) = 0.0
 40   continue   
      
!  Growth Stages arrays	(daynum, year, mo, day)
      do 10 i = 1,4
       if (i .eq. 1) then
         aifs(i) = dn   
         antes(i) = dn  
         antss(i) = dn  
         blstrs(i) = dn
         browns(i) = dn 
         boots(i) = dn  
         dents(i) = dn
         doughs(i) = dn       
         drs(i) = dn
         ears(i) = dn 
         ems(i) = dn
         endlgs(i) = dn
         fps(i) = dn
         fullbs(i) = dn
         germs(i) = dn
         gpds(i) = dn
         halfbs(i) = dn
         heads(i) = dn
         hrs(i) = dn
         ies(i) = dn
         ies2(i) = dn 
         infls(i) = dn
         joints(i) = dn
         lf12s(i) = dn
         lf4s(i) = dn
         lf8s(i) = dn
         mats(i) = dn
         milks(i) = dn
         opens(i) = dn
         silks(i) = dn 
         srs(i) = dn
         tis(i) = dn
         tsints(i) = dn 
         tss(i) = dn
         yelows(i) = dn
       elseif (i .eq. 2) then      
         aifs(i) = y
         antes(i) = y
         antss(i) = y
         blstrs(i) = y
         browns(i) = y
         boots(i) = y
         dents(i) = y
         doughs(i) = y
         drs(i) = y
         ears(i) = y 
         ems(i) = y
         endlgs(i) = y
         fps(i) = y
         fullbs(i) = y
         germs(i) = y
         gpds(i) = y
         halfbs(i) = y
         heads(i) = y
         hrs(i) = y
         ies(i) = y
         ies2(i) = y
         infls(i) = y
         joints(i) = y
         lf12s(i) = y
         lf4s(i) = y
         lf8s(i) = y
         mats(i) = y
         milks(i) = y
         opens(i) = y
         silks(i) = y
         srs(i) = y
         tis(i) = y
         tsints(i) = y 
         tss(i) = y
         yelows(i) = y
       elseif (i .eq. 3) then
         aifs(i) = m
         antes(i) = m
         antss(i) = m
         blstrs(i) = m
         browns(i) = m
         boots(i) = m
         dents(i) = m
         doughs(i) = m
         drs(i) = m
         ears(i) = m 
         ems(i) = m
         endlgs(i) = m
         fps(i) = m
         fullbs(i) = m
         germs(i) = m
         gpds(i) = m
         halfbs(i) = m
         heads(i) = m
         hrs(i) = m
         ies(i) = m
         ies2(i) = m
         infls(i) = m
         joints(i) = m
         lf12s(i) = m
         lf4s(i) = m
         lf8s(i) = m
         mats(i) = m
         milks(i) = m
         opens(i) = m
         silks(i) = m
         srs(i) = m
         tis(i) = m
         tsints(i) = m 
         tss(i) = m
         yelows(i) = m
       elseif (i .eq. 4) then
         aifs(i) = d
         antes(i) = d
         antss(i) = d
         blstrs(i) = d
         browns(i) = d
         boots(i) = d 
         dents(i) = d
         doughs(i) = d
         drs(i) = d
         ears(i) = d 
         ems(i) = d
         endlgs(i) = d
         fps(i) = d
         fullbs(i) = d
         germs(i) = d
         gpds(i) = d
         halfbs(i) = d
         heads(i) = d
         hrs(i) = d
         ies(i) = d
         ies2(i) = d
         infls(i) = d 
         joints(i) = d
         lf12s(i) = d
         lf4s(i) = d
         lf8s(i) = d
         mats(i) = d
         milks(i) = d
         opens(i) = d
         silks(i) = d
         srs(i) = d
         tis(i) = d
         tsints(i) = d 
         tss(i) = d
         yelows(i) = d       
       endif 
     
 10   continue

! Other Variables
	first7 = 0
	nolvs = 0.

      return
      end