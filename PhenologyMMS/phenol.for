!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   P H E N O L                  *
!  *                                                      gm   1/21/05 *
!  *                                                      de   7/14/05 *
!  *                                                                   *
!  *********************************************************************
!  The PHENOL subroutine calls subroutines to calculate the phenology of
!  a specific crop.

!  INPUTS:  aepa(R), aepacn(R), aepasf(R), aifs(R), antes(R), antss(R), 
!           boots(R), btpacn(R), btpasf(R), cname(R), dae(R), dap(R), 
!           dav(R), daynum(R), ddae(R), ddap(R), ddav(R), dgdde(R), 
!           dgdds(R), dgddv(R), drpacn(R), drpasf(R), drs(R), dummy2(R),
!           empacn(R), empasf(R), ems(R), endlgs(R), first7(R), fps(R), 
!           fullbs(R), gdde(R), gdds(R), gddv(R), gpds(R), halfbs(R), 
!           hdpacn(R), hdpasf(R), heads(R), hrpacn(R), hrpasf(R), 
!           hrs(R), iepacn(R), iepasf(R), ies(R), joints(R), jtpacn(R),
!           jtpasf(R), mats(R), mtpacn(R), mtpasf(R), nolvs(R), 
!           pchron(R), srpacn(R), srpasf(R), srs(R), tipacn(R), 
!           tipasf(R), tis(R), tspacn(R), tspasf(R), tss(R)           

!  OUTPUTS: aepa(R), aepacn(R), aepasf(R), aifs(R), antes(R), antss(R), 
!           boots(R), btpacn(R), btpasf(R), cname(R), dae(R), dap(R), 
!           dav(R), daynum(R), ddae(R), ddap(R), ddav(R), dgdde(R), 
!           dgdds(R), dgddv(R), drpacn(R), drpasf(R), drs(R), dummy2(R),
!           empacn(R), empasf(R), ems(R), endlgs(R), first7(R), fps(R), 
!           fullbs(R), gdde(R), gdds(R), gddv(R), gpds(R), halfbs(R), 
!           hdpacn(R), hdpasf(R), heads(R), hrpacn(R), hrpasf(R), 
!           hrs(R), iepacn(R), iepasf(R), ies(R), joints(R), jtpacn(R),
!           jtpasf(R), mats(R), mtpacn(R), mtpasf(R), nolvs(R), 
!           pchron(R), srpacn(R), srpasf(R), srs(R), tipacn(R), 
!           tipasf(R), tis(R), tspacn(R), tspasf(R), tss(R)

      subroutine phenol(aepa, aifs, antes, antss, blstrs, boots, browns,
     c cname, dae, dap, dav, daynum, ddae, ddap, ddav, dents, dgdde, 
     c dgdds, dgddv, doughs, drs, dummy2, ears, endlgs, first7, fps, 
     c fullbs, gdde, gdds, gddv, gpds, halfbs, heads, hrs, ies, ies2, 
     c infls, joints, lf12s, lf4s, lf8s, mats, milks, nolvs, opens, 
     c pchron, silks, srs, tis, tsints, tss, year, yelows)

      implicit none
      
      integer  aifs(4), antes(4), antss(4), blstrs(4), boots(4), 
     c browns(4), dae, dap, dav, daynum, ddae(20), ddap(20), ddav(20), 
     c dents(4), doughs(4), drs(4), ears(4), endlgs(4), first7, fps(4),
     c fullbs(4),gpds(4), halfbs(4), heads(4), hrs(4), ies(4), ies2(4),
     c infls(4), joints(4), lf12s(4), lf4s(4), lf8s(4), milks(4), 
     c mats(4), opens(4), silks(4), srs(4), tis(4), tsints(4), tss(4), 
     c year, yelows(4)
     
          
      real  aepa, dgdde(20), dgdds(20), dgddv(20), dummy2(15), gdde, 
     c gdds, gddv, nolvs, pchron 
     

      character *22  cname

! Call the correct phenology subroutine for the selected crop:
!If beans: this is the sunflower code. Change to that appropriate for beans
      if (cname .eq. 'Dry Beans') then
	    call phenolbn(aepa, antes, antss, browns, dae, dap, daynum, ddae, 
     c ddap, dgdde, dgdds, dummy2, first7, gdde, gdds, hrs, ies, ies2, 
     c infls, lf12s, lf4s, lf8s, mats, opens, year, yelows)
     
! If corn:
      elseif (cname .eq. 'Corn') then
	    call phenolcn(aepa, antss, blstrs, dae, dap, daynum, ddae, 
     c ddap, dents, dgdde, dgdds, doughs, dummy2, ears, first7, gdde, 
     c gdds, hrs, ies, lf12s, lf4s, mats, milks, silks, tsints, 
     c year) 
     
! If hay millet:
      elseif(cname .eq. 'Hay Millet') then
	    call phenolhm(aepa, antes, antss, boots, dae, dap,  
     c daynum, ddae, ddap, dgdde, dgdds, drs, dummy2, 
     c first7, fps, gdde, gdds, heads, hrs, ies, joints, mats, 
     c nolvs, pchron, srs, tis, tss, year)

! If proso millet:
      elseif (cname .eq. 'Proso Millet') then
	    call phenolpm(aepa, antes, antss, boots, dae, dap,  
     c daynum, ddae, ddap, dgdde, dgdds, drs, dummy2, 
     c first7, fps, gdde, gdds, heads, hrs, ies, joints, mats, 
     c nolvs, pchron, srs, tis, tss, year)

! If sorghum:
      elseif (cname .eq. 'Sorghum') then
	    call phenolsg(aepa, antes, antss, dae, dap, daynum, 
     c ddae, ddap, dgdde, dgdds, dummy2, endlgs, first7,
     c fullbs, gdde, gdds, gpds, halfbs, hrs, ies, joints, mats, 
     c nolvs, pchron, tis, year)

! If spring barley:
      elseif (cname .eq. 'Spring Barley') then
	    call phenolsb(aepa, aifs, antes, antss, boots, dae, dap, 
     c daynum, ddae, ddap, dgdde, dgdds, drs, dummy2,
     c first7, fps, gdde, gdds, heads, hrs, ies, joints, mats, 
     c nolvs, pchron, srs, tis, year)

! If spring wheat:
      elseif (cname .eq. 'Spring Wheat') then
	    call phenolsw(aepa, antes, antss, boots, dae, dap,  
     c daynum, ddae, ddap, dgdde, dgdds, drs, dummy2, 
     c first7, fps, gdde, gdds, heads, hrs, ies, joints, mats, 
     c nolvs, pchron, srs, tis, tss, year)

! If sunflower:
      elseif (cname .eq. 'Sunflower') then
	    call phenolsf(aepa, antes, antss, browns, dae, dap, daynum, ddae, 
     c ddap, dgdde, dgdds, dummy2, first7, gdde, gdds, hrs, ies, ies2, 
     c infls, lf12s, lf4s, lf8s, mats, opens, year, yelows)

! If winter barley:
      elseif (cname .eq. 'Winter Barley') then
	    call phenolwb(aepa, aifs, antes, antss, boots, dae, dap, 
     c dav, daynum, ddae, ddap, ddav, dgdde, dgdds, dgddv, drs, dummy2, 
     c first7, fps, gdde, gdds, gddv, heads, hrs, ies, joints, mats, 
     c nolvs, pchron, srs, tis, year)

! If winter wheat:
	elseif (cname .eq. 'Winter Wheat') then
	    call phenolww(aepa, antes, antss, boots, dae, dap, dav,
     c daynum, ddae, ddap, ddav, dgdde, dgdds, dgddv, dummy2, drs,
     c first7, fps, gdde, gdds, gddv, heads, hrs, ies, joints, mats, 
     c nolvs, pchron, srs, tis, tss, year)
	         
	endif
      
      return
      end