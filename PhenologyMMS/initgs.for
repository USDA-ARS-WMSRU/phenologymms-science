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
 
	
      subroutine initgs(aifs, antes, antss, blstrs, bmats, boots, 
     c browns, cots, dents, doughs, drs, dummy1, dummy2, ears, ems,  
     c endlgs, epods, eseeds, first7, fps, fullbs, germs, gpds, halfbs,
     c heads, hrs, ies, ies2, infls, joints, lf1s, lf12s, lf2s, lf3s,  
     c lf4s, lf5s, lf8s, mats, mffls, milks, mpods, mseeds, nolvs, 
     c opens, silks, srs, tis, tsints, tss, verns, yelows)
     
! debe added dry beans variables. Added 2 new soybean variables: bmats, lf5s
     

      implicit none
      
      integer  aifs(4), antes(4), antss(4), blstrs(4), bmats(4), 
     c boots(4), browns(4), cots(4), d, dents(4), dn, doughs(4), drs(4),
     c ears(4), ems(4), endlgs(4), epods(4), eseeds(4), first7, fps(4), 
     c fullbs(4), germs(4), gpds(4), halfbs(4), heads(4), hrs(4), i, 
     c ies(4), ies2(4), infls(4), joints(4), lf1s(4), lf12s(4), lf2s(4),
     c lf3s(4), lf4s(4), lf5s(4), lf8s(4), m, mats(4), mffls(4), 
     c milks(4), mpods(4), mseeds(4), opens(4), silks(4), srs(4), 
     c tis(4), tsints(4), tss(4), verns, y, yelows(4) 
     
      real dummy2(16), nolvs 
      
      character *22 dummy1(16)

! Initialize variables
      dn = 999 !daynum
      y = 0000 !year
      m = 0    !month
      d = 0    !day
      i = 0    !counter 
      verns = 999   !vernalization

! Initialize Arrays:

!debe changed size of dummy arrays to allow for 16 stages in soybeans
!  Character String Array      
      do 30 i = 1, 16
	  dummy1(i) = ""
 30	continue
      
!  Array for values from Customize Run screen
      do 40 i = 1, 16
        dummy2(i) = 0.0
 40   continue   
      
!  Growth Stages arrays	(daynum, year, mo, day)
      do 10 i = 1,4
       if (i .eq. 1) then
         aifs(i) = dn   
         antes(i) = dn  
         antss(i) = dn  
         blstrs(i) = dn
         bmats (i) = dn
         boots(i) = dn
         browns(i) = dn 
         cots(i) = dn  
         dents(i) = dn
         doughs(i) = dn       
         drs(i) = dn
         ears(i) = dn 
         ems(i) = dn
         endlgs(i) = dn
         epods(i) = dn
         eseeds(i) = dn
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
         lf1s(i) = dn
         lf12s(i) = dn
         lf2s(i) = dn
         lf3s(i) = dn
         lf4s(i) = dn
         lf5s(i) = dn
         lf8s(i) = dn
         mats(i) = dn
         mffls(i) = dn
         milks(i) = dn
         mpods(i) = dn
         mseeds(i) = dn
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
         bmats(i) = y
         boots(i) = y
         browns(i) = y
         cots(i) = dn
         dents(i) = y
         doughs(i) = y
         drs(i) = y
         ears(i) = y 
         ems(i) = y
         endlgs(i) = y
         epods(i) = y
         eseeds(i) = y
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
         lf1s(i) = y
         lf12s(i) = y
         lf2s(i) = y
         lf3s(i) = y
         lf4s(i) = y
         lf5s(i) = y
         lf8s(i) = y
         mats(i) = y
         mffls(i) = y
         milks(i) = y
         mpods(i) = y
         mseeds(i) = y
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
         bmats(i) = m
         boots(i) = m
         browns(i) = m
         cots(i) = m
         dents(i) = m
         doughs(i) = m
         drs(i) = m
         ears(i) = m 
         ems(i) = m
         endlgs(i) = m
         epods(i) = m
         eseeds(i) = m
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
         lf1s(i) = m
         lf12s(i) = m
         lf2s(i) = m
         lf3s(i) = m
         lf4s(i) = m
         lf5s(i) = m
         lf8s(i) = m
         mats(i) = m
         mffls(i) = m
         milks(i) = m
         mpods(i) = m
         mseeds(i) = m
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
         bmats(i) = d
         boots(i) = d
         browns(i) = d
         cots(i) = d
         dents(i) = d
         doughs(i) = d
         drs(i) = d
         ears(i) = d 
         ems(i) = d
         endlgs(i) = d
         epods(i) = d
         eseeds(i) = d
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
         lf1s(i) = d
         lf12s(i) = d
         lf2s(i) = d
         lf3s(i) = d
         lf4s(i) = d
         lf5s(i) = d
         lf8s(i) = d
         mats(i) = d
         mffls(i) = d
         milks(i) = d
         mpods(i) = d
         mseeds(i) = d
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