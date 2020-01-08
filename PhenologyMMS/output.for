!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   O U T P U T                  *
!  *                                                      gm   1/31/05 *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
!  The OUTPUT subroutine prints output to ascii files and the screen.


!  INPUTS:  aifs(R), antes(R), antss(R), boots(R), canht(R), cname(R), 
!           daa(R), dae(R), dap(R), dav(R), ddae(R), ddap(R), ddav(R), 
!           dgdde(R), dgdds(R), dgddv(R), drs(R), ems(R), endlgs(R), 
!           fps(R), fullbs(R), gdda(R), gdde(R), gdds(R), gddv(R), 
!           gpds(R), halfbs(R), heads(R), hrs(R), ies(R), joints(R), 
!           mats(R), outf(C), pchron (R), pdate(R), srs(R), 
!           tis(R), tss(R)

!  OUTPUTS: aifs(R), antes(R), antss(R), boots(R), canht(R), cname(R),
!           daa(R), dae(R), dap(R), dav(R), ddae(R), ddap(R), ddav(R),
!           dgdde(R), dgdds(R), dgddv(R), drs(R), ems(R), endlgs(R), 
!           fps(R), fullbs(R), gdda(R), gdde(R), gdds(R), gddv(R), 
!           gpds(R), halfbs(R), heads(R), hrs(R), ies(R), joints(R), 
!           mats(R), outf(C), pdate(R), srs(R), tis(R), tss(R) 

      subroutine output(aifs, antes, antss, blstrs, bmats, boots,   
     c browns, canht, cname, cots, daa, dae, dap, dav, ddae, ddap, ddav,
     c dents, dgdde, dgdds, dgddv, doughs, drs, ears, ems, endlgs, 
     c epods, eseeds, fps, fullbs, gdda, gdde, gdds, gddv, gpds, halfbs,
     c heads, hrs, ies, ies2, infls, joints, lf1s, lf12s, lf2s, lf3s, 
     c lf4s, lf5s, lf8s, lnarray, lnpout, mats, milks, mffls, mpods, 
     c mseeds, nolvs, opens, outf, pchron, pdate, planted, pyear, silks,
     c srs, tis, tsints, tss, weather, year, yelows) 

!debe added logical variable 'planted' to be able to write an error
!message in phenol.out alerting the user that the planting date
!was not reached.
      
      
!debe added dry bean variables and two new soybean variables    
 
      implicit none
	 
      integer  aifs(4), antes(4), antss(4), blstrs(4), bmats(4), 
     c boots(4), browns(4), cots(4), daa, dae, dap, dav, ddae(20), 
     c ddap(20), ddav(20), dents(4), doughs(4), drs(4), ears(4), 
     c ems(4), endlgs(4), epods(4), eseeds(4), fps(4), fullbs(4), 
     c gpds(4), halfbs(4), heads(4), hrs(4), i, ies(4), icanht, 
     c ies2(4), infls(4), j, joints(4), lf1s(4), lf12s(4), lf2s(4), 
     c lf3s(4), lf4s(4), lf5s(4), lf8s(4), mats(4), mffls(4), milks(4), 
     c mpods(4), mseeds(4), opens(4), pdate, pdatearr(4), pyear, 
     c silks(4), srs(4), tis(4), tsints(4), tss(4), year, yelows(4)
     
!9/2/14 DE and GM decided to use one variable name for the integer zero  
!and one for the real zero when printing zeros in the output table.
      integer dumint
 !     real dumreal
!         
      real  canht, dgdde(20), dgdds(20), dgddv(20), gdda, gdde, gdds, 
     c gddv, lnarray (600,2), lnpout(100,2), nolvs, pchron, rboots
      
      character *22  cname
      character *256 outf !de added not enough characters for the full path
      character *110 weather !de added
      
      logical planted
      
! Initialize variables

!GM added 8/29/14 dummy output variables for 0's in table

      dumint = 0
  !    dumreal = 0.0
            
      do 10 i = 1,4
             pdatearr(i) = 0
 10   continue 
      pdatearr(2) = pyear  
      
      j = 1

!  Print out some stuff to the screen:
!  year after coming into output.for from PhenologyMMS.for
!        print *, 'year coming into output.for = ', year 
!        print *, 'pyear = ', pyear
        
        print *, 'Subroutine output was called'
	  print *, 'Crop is: ', cname  ! de added
	  print *, 'Canopy height is: ', canht  ! de added
	  print *, 'gdds = ', gdds
	  print *, 'dap = ', dap
	  print *, 'gdde = ', gdde
	  print *, 'dae = ', dae
	  print *, 'gddv = ', gddv
	  print *, 'dav = ', dav
	  print *, 'gdda = ', gdda
	  print *, 'daa = ', daa
	  print *, 'year = ', year

!  Want to echo out inputs and settings first

      outf = 'results/phenol.out'
      open (unit=14, file=outf)
      !print *, 'outf = ', outf
      
! Fill pdatearr
      pdatearr(1) = pdate      
      call date1(pdatearr)
      
! Round canopy height to an integer
      icanht = NINT(canht)      

! DE took out vernaliztion from the output for the spring crops that 
! still displayed it in the output. 7/16/07

! Determine number of leaves for each day.  Fill leafno array.
 !     call leafno(daynum, gdde, lnarray, lnpout, pchron)

      
      !Add a write statement to show the path to the outf file. DE added.
 !     write (14,143)
 !143  format ('Path to phenol.out is') file
      
! Warn user that if planting date is outside the weather years or 
! results in a harvest date outside of the weather years in the 
! selected location, '999' will display in the output.
      write (14, 145)     
 145  format ('NOTE: If 999 is displayed in the output, the planting', 
     . /1x,'date may be outside of the weather years in the selected',
     . /1x, 'weather file. Also, the selected planting date might',
     . ' result', /1x, 'in a harvest date outside of the years in the',
     .  /1x, 'weather file.', /1x) 
      
      if (planted .eq. .FALSE.) then
          write (14, 25) 
25        format ('ERROR 101: The planting date was not reached. Choose'
     . ,/1x,'another weather/location or change the planting date to be'
     . ,/1x 'within the weather file time range.', 2x)
      endif    
      
      
!  Heading for Leaf Number table     
      write (14,99)cname 
99    format (/1x, 42x, a14, /1x, 42x,'Leaf Number', /1x, 39x, 'DOY', 2x, 
     . 'Leaf Number', /1x, 38x, '------------------')

! write out year
!      write (14,20)year
!20    format(/1x, 'Year = ', i21)
      
      
! Write out winter wheat phenology results:

      if (cname .eq. 'Winter Wheat') then
!  Write out a table with leaf numbers by DOY
      do while (lnpout(j,2) .lt. dgdde(9)/pchron)
        write (14,50) lnpout(j,1), lnpout(j,2)
        j = j + 1
      end do      

! convert integer boots(1) to a real number
      write(14,50) real (boots(1)), dgdde(9)/pchron
50    format (40x, f5.1, 6x, f4.1) 

      write (14, 70)
70    format (/1x) ! write a blank line after outputting the 
!      leaf number table

      write (14, 150) pdatearr(1), pdatearr(3), pdatearr(4), dumint, 
     .       dumint, dumint, dumint, dumint, dumint, dumint, 
     .  ems(1), ems(3), ems(4), ddap(1), dumint, dumint, dgdds(1), 
     .       dumint, dumint, dumint, 
     .  tis(1), tis(3), tis(4), ddap(2), ddae(2), dumint, dgdds(2), 
     .       dgdde(2), dumint, dgdde(2)/pchron,
     .  srs(1), srs(3), srs(4), ddap(3), ddae(3), ddav(3), dgdds(3), 
     .       dgdde(3), dgddv(3), dgdde(3)/pchron,
     .  drs(1), drs(3), drs(4), ddap(4), ddae(4), ddav(4), dgdds(4), 
     .       dgdde(4), dgddv(4), dgdde(4)/pchron,
     .  fps(1), fps(3), fps(4), ddap(7), ddae(7), ddav(7), dgdds(7), 
     .       dgdde(7), dgddv(7), dgdde(7)/pchron,     
     .  ies(1), ies(3), ies(4), ddap(6), ddae(6), ddav(6), dgdds(6), 
     .       dgdde(6), dgddv(6), dgdde(6)/pchron,
     .  tss(1), tss(3), tss(4), ddap(5), ddae(5), ddav(5), dgdds(5), 
     .       dgdde(5), dgddv(5), dgdde(5)/pchron,
     .  joints(1), joints(3), joints(4), ddap(8), ddae(8), ddav(8), 
     .       dgdds(8), dgdde(8), dgddv(8), dgdde(8)/pchron,
     .  boots(1), boots(3), boots(4), ddap(9), ddae(9), ddav(9), 
     .       dgdds(9), dgdde(9), dgddv(9), dgdde(9)/pchron,
     .  heads(1), heads(3), heads(4), ddap(10),ddae(10),ddav(10),
     .       dgdds(10), dgdde(10), dgddv(10), dgdde(9)/pchron,
     .  antss(1), antss(3), antss(4), ddap(11), ddae(11),ddav(11),
     .       dgdds(11), dgdde(11), dgddv(11), dgdde(9)/pchron,
     .  antes(1), antes(3), antes(4), ddap(12), ddae(12), ddav(12),
     .       dgdds(12), dgdde(12), dgddv(12), dgdde(9)/pchron,
     .  mats(1), mats(3), mats(4), ddap(13), ddae(13), ddav(13),
     .       dgdds(13), dgdde(13), dgddv(13), dgdde(9)/pchron,
     .  hrs(1), hrs(3), hrs(4), ddap(14), ddae(14), ddav(14),
     .       dgdds(14), dgdde(14), dgddv(14), dgdde(9)/pchron,
     .  icanht
 150  format (' Phenological Event', 7x, 'Day of Year', 2x, 'Date', 2x, 
     .  'DAP', 5x, 'DAE', 5x, 'DAV', 5x, 'GDD AP', 5x, 'GDD AE', 5x, 
     .  'GDD AV', 5x, 'NOLVS', /1x
     . 'Planting Date', 18x, i4, 2x, i2, '/', i2, 4x, i1, 7x, i1, 7x,  
     .    i1, 10x, i1, 10x, i1, 10x, i1, 9x, i1, /1x,
     . 'Emergence', 22x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4,
     .    5x, f6.1, 7x, i4, 7x, i4, 6x, i4, /1x,
     . 'First tiller', 19x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4,
     .    5x, f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Single ridge', 19x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4,
     .    5x, f6.1, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,     
     . 'Double ridge', 19x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Floret primordia init begins', 3x, i4, 2x, i2, '/', i2, 1x, i4, 
     .    4x, i4, 4x, i4, 5x, f6.1, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Stem elongation begins', 9x, i4, 2x, i2, '/', i2, 1x, i4, 4x, 
     .     i4, 4x, i4, 5x, f6.1, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'End spikelet initiation', 8x, i4, 2x, i2, '/', i2, 1x, i4, 4x, 
     .    i4, 4x, i4, 5x, f6.1, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Jointing', 23x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Booting', 24x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4, 5x, 
     .    f6.1, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Heading', 24x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4, 5x, 
     .    f6.1, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Anthesis starts', 16x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Anthesis ends', 18x, i4, 2x, i2, '/', i2, 1x, i4,4x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Physiological maturity', 9x, i4, 2x, i2, '/', i2, 1x, i4, 4x, 
     .     i4, 4x, i4, 5x, f6.1, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Harvest ready', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, 
     .     i4, 5x, f6.1, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Canopy Height (cm)', 11x, i6)

      endif

! Write out spring wheat phenology resuts:
      
      if (cname .eq. 'Spring Wheat') then
!  Write out a table with leaf numbers by DOY
      do while (lnpout(j,2) .lt. dgdde(9)/pchron)
        write (14,51) lnpout(j,1), lnpout(j,2)
        j = j + 1
      end do      

! convert integer boots(1) to a real number
      write(14,51) real (boots(1)), dgdde(9)/pchron
51    format (40x, f5.1, 6x, f4.1) 

      write (14, 71)
71    format (/1x) ! write a blank line after outputting the 
!      leaf number table
      
      write (14, 155) pdatearr(1), pdatearr(3), pdatearr(4), dumint, 
     .      dumint, dumint, dumint, dumint, dumint, dumint, 
     .  ems(1), ems(3), ems(4), ddap(1), dumint, dumint, dgdds(1), 
     .      dumint, dumint, dumint, 
     .  tis(1), tis(3), tis(4), ddap(2), ddae(2), dumint, dgdds(2), 
     .       dgdde(2), dumint, dgdde(2)/pchron,
     .  srs(1), srs(3), srs(4), ddap(3), ddae(3),  dumint, dgdds(3),
     .        dgdde(3), dumint, dgdde(3)/pchron,
     .  drs(1), drs(3), drs(4), ddap(4), ddae(4), dumint, dgdds(4),
     .        dgdde(4), dumint, dgdde(4)/pchron,
     .  fps(1), fps(3), fps(4), ddap(7), ddae(7), dumint, dgdds(7),
     .        dgdde(7), dumint, dgdde(7)/pchron,     
     .  ies(1), ies(3), ies(4), ddap(6), ddae(6),  dumint, dgdds(6),
     .        dgdde(6), dumint, dgdde(6)/pchron,
     .  tss(1), tss(3), tss(4), ddap(5), ddae(5),  dumint, dgdds(5),
     .        dgdde(5), dumint, dgdde(5)/pchron,
     .  joints(1), joints(3), joints(4), ddap(8),  ddae(8), dumint,
     .        dgdds(8), dgdde(8), dumint, dgdde(8)/pchron,
     .  boots(1), boots(3), boots(4), ddap(9), ddae(9), dumint,
     .        dgdds(9), dgdde(9), dumint, dgdde(9)/pchron,
     .  heads(1), heads(3), heads(4), ddap(10),  ddae(10), dumint,
     .        dgdds(10), dgdde(10), dumint, dgdde(9)/pchron,
     .  antss(1), antss(3), antss(4), ddap(11), ddae(11), dumint,
     .        dgdds(11), dgdde(11), dumint, dgdde(9)/pchron,
     .  antes(1), antes(3), antes(4), ddap(12), ddae(12), dumint,
     .        dgdds(12), dgdde(12), dumint, dgdde(9)/pchron,
     .  mats(1), mats(3), mats(4), ddap(13), ddae(13), dumint,
     .        dgdds(13), dgdde(13), dumint, dgdde(9)/pchron,
     .  hrs(1), hrs(3), hrs(4), ddap(14), ddae(14), dumint, dgdds(14), 
     .       dgdde(14), dumint, dgdde(9)/pchron,
     .  icanht
155   format (' Phenological Event', 7x, 'Day of Year', 2x, 'Date', 2x, 
     .  'DAP', 5x, 'DAE', 5x, 'DAV', 5x, 'GDD AP', 5x, 'GDD AE', 5x, 
     .  'GDD AV', 5x, 'NOLVS', /1x
     . 'Planting Date', 18x, i4, 2x, i2, '/', i2, 4x, i1, 7x, i1, 7x,  
     .    i1, 10x, i1, 10x, i1, 10x, i1, 9x, i1, /1x,
     . 'Emergence', 22x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4,
     .    5x, f6.1, 7x, i4, 7x, i4, 6x, i4, /1x,
     . 'First tiller', 19x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4,
     .    5x, f6.1, 5x, f6.1, 7x, i4,  4x, f6.1, /1x,
     . 'Single ridge', 19x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4,
     .    5x, f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Double ridge', 19x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4,
     .    5x, f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Floret primordia init begins', 3x, i4, 2x, i2, '/', i2, 1x, i4,
     .    4x, i4, 4x, i4, 5x, f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Stem elongation begins',9x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4,
     .     4x, i4, 5x, f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'End spikelet initiation', 8x, i4, 2x, i2, '/', i2, 1x, i4, 4x, 
     .    i4, 4x, i4, 5x, f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Jointing', 23x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Booting', 24x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4, 5x, 
     .    f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Heading', 24x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4, 5x, 
     .    f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Anthesis starts', 16x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Anthesis ends', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Physiological maturity', 9x, i4, 2x, i2, '/', i2, 1x, i4, 4x, 
     .    i4, 4x, i4, 5x, f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Harvest ready', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Canopy Height (cm)', 11x, i6)

      endif


! Write out winter barley phenology results:

      if (cname .eq.'Winter Barley') then
      
!  Write out a table with leaf numbers by DOY
      do while (lnpout(j,2) .lt. dgdde(9)/pchron)
        write (14,52) lnpout(j,1), lnpout(j,2)
        j = j + 1
      end do      

! convert integer boots(1) to a real number
      write(14,52) real (boots(1)), dgdde(9)/pchron
52    format (40x, f5.1, 6x, f4.1) 

      write (14, 72)
72    format (/1x) ! write a blank line after outputting the 
!      leaf number table
      
      write (14, 160) pdatearr(1), pdatearr(3), pdatearr(4), dumint, 
     .       dumint, dumint, dumint, dumint, dumint, dumint, 
     .  ems(1), ems(3), ems(4), ddap(1), dumint, dumint, dgdds(1), 
     .       dumint, dumint, dumint, 
     .  tis(1), tis(3), tis(4), ddap(2), ddae(2), dumint, dgdds(2), 
     .       dgdde(2), dumint, dgdde(2)/pchron, 
     .  srs(1), srs(3), srs(4), ddap(3), ddae(3), ddav(3), 
     .       dgdds(3), dgdde(3), dgddv(3), dgdde(3)/pchron,
     .  drs(1), drs(3), drs(4), ddap(4), ddae(4), ddav(4), 
     .       dgdds(4), dgdde(4), dgddv(4), dgdde(4)/pchron,
     .  fps(1), fps(3), fps(4), ddap(7), ddae(7), ddav(7), 
     .       dgdds(7), dgdde(7), dgddv(7), dgdde(7)/pchron,     
     .  ies(1), ies(3), ies(4), ddap(6), ddae(6), ddav(6), 
     .       dgdds(6), dgdde(6), dgddv(6), dgdde(6)/pchron,
     .  aifs(1), aifs(3), aifs(4), ddap(5), ddae(5), ddav(5), 
     .        dgdds(5), dgdde(5), dgddv(5), dgdde(5)/pchron,
     .  joints(1), joints(3), joints(4), ddap(8), ddae(8), ddav(8), 
     .        dgdds(8), dgdde(8), dgddv(8), dgdde(8)/pchron,
     .  boots(1), boots(3), boots(4), ddap(9), ddae(9), ddav(9), 
     .       dgdds(9), dgdde(9), dgddv(9), dgdde(9)/pchron,
     .  heads(1), heads(3), heads(4), ddap(10),ddae(10),ddav(10),
     .       dgdds(10), dgdde(10), dgddv(10), dgdde(9)/pchron,
     .  antss(1), antss(3), antss(4), ddap(11), ddae(11), 
     .       ddav(11), dgdds(11), dgdde(11), dgddv(11), dgdde(9)/pchron,
     .  antes(1), antes(3), antes(4), ddap(12), ddae(12), 
     .       ddav(12), dgdds(12), dgdde(12),dgddv(12), dgdde(9)/pchron,
     .  mats(1), mats(3), mats(4), ddap(13), ddae(13), 
     .       ddav(13), dgdds(13), dgdde(13), dgddv(13), dgdde(9)/pchron,
     .  hrs(1), hrs(3), hrs(4), ddap(14), ddae(14), ddav(14),
     .        dgdds(14), dgdde(14), dgddv(14), dgdde(9)/pchron,
     .  icanht
 160  format (' Phenological Event', 7x, 'Day of Year', 2x, 'Date', 2x, 
     .  'DAP', 5x, 'DAE', 5x, 'DAV', 5x, 'GDD AP', 5x, 'GDD AE', 5x, 
     .  'GDD AV', 5x,'NOLVS', /1x
     . 'Planting Date', 18x, i4, 2x, i2, '/', i2, 4x, i1, 7x, i1, 7x,  
     .    i1, 10x, i1, 10x, i1, 10x, i1, 9x, i1, /1x,
     . 'Emergence', 22x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4,
     .    5x, f6.1, 7x, i4, 7x, i4, 6x, i4, /1x,
     . 'First tiller', 19x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4,
     .    5x, f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Single ridge', 19x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4,
     .    5x, f6.1, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Double ridge', 19x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4,
     .    5x, f6.1, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Floret primordia init begins', 3x, i4, 2x, i2, '/', i2, 1x, i4,
     .    4x, i4, 4x, i4, 5x, f6.1, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Stem elongation begins',9x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4,
     .     4x, i4, 5x, f6.1, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Awn Initials Formed', 12x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    4x, i4, 5x, f6.1, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Jointing', 23x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Booting', 24x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4, 5x, 
     .    f6.1, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Heading', 24x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4, 5x, 
     .    f6.1, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Anthesis starts', 16x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Anthesis ends', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Physiological maturity', 9x, i4, 2x, i2, '/', i2, 1x, i4, 4x, 
     .    i4, 4x, i4, 5x, f6.1, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Harvest ready', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Canopy Height (cm)', 11x, i6)

      endif

! Write out spring barley phenology results:
      
      if (cname .eq.'Spring Barley') then
      
!  Write out a table with leaf numbers by DOY
      do while (lnpout(j,2) .lt. dgdde(9)/pchron)
        write (14,53) lnpout(j,1), lnpout(j,2)
        j = j + 1
      end do      

! convert integer boots(1) to a real number
      write(14,53) real (boots(1)), dgdde(9)/pchron
53    format (40x, f5.1, 6x, f4.1) 

      write (14, 73)
73    format (/1x) ! write a blank line after outputting the 
!      leaf number table
 
      write (14, 165) pdatearr(1), pdatearr(3), pdatearr(4), 
     .       dumint, dumint, dumint, dumint, dumint, dumint, dumint,
     .  ems(1), ems(3), ems(4), ddap(1), dumint, dumint, dgdds(1), 
     .       dumint, dumint, dumint, 
     .  tis(1), tis(3), tis(4), ddap(2), ddae(2), dumint, dgdds(2), 
     .       dgdde(2), dumint, dgdde(2)/pchron, 
     .  srs(1), srs(3), srs(4), ddap(3), ddae(3), dumint, 
     .       dgdds(3), dgdde(3), dumint, dgdde(3)/pchron,
     .  drs(1), drs(3), drs(4), ddap(4), ddae(4), dumint, 
     .       dgdds(4), dgdde(4), dumint, dgdde(4)/pchron,
     .  fps(1), fps(3), fps(4), ddap(7), ddae(7), dumint, 
     .       dgdds(7), dgdde(7), dumint, dgdde(7)/pchron,     
     .  ies(1), ies(3), ies(4), ddap(6), ddae(6), dumint, 
     .       dgdds(6), dgdde(6), dumint, dgdde(6)/pchron,
     .  aifs(1), aifs(3), aifs(4), ddap(5), ddae(5), dumint, 
     .        dgdds(5), dgdde(5), dumint, dgdde(5)/pchron,
     .  joints(1), joints(3), joints(4), ddap(8), ddae(8), dumint, 
     .        dgdds(8), dgdde(8), dumint, dgdde(8)/pchron,
     .  boots(1), boots(3), boots(4), ddap(9), ddae(9), dumint, 
     .       dgdds(9), dgdde(9), dumint, dgdde(9)/pchron,
     .  heads(1), heads(3), heads(4), ddap(10),ddae(10),dumint,
     .       dgdds(10), dgdde(10), dumint, dgdde(9)/pchron,
     .  antss(1), antss(3), antss(4), ddap(11), ddae(11), 
     .       dumint, dgdds(11), dgdde(11), dumint, dgdde(9)/pchron,
     .  antes(1), antes(3), antes(4), ddap(12), ddae(12), 
     .       dumint, dgdds(12), dgdde(12),dumint,dgdde(9)/pchron,
     .  mats(1), mats(3), mats(4), ddap(13), ddae(13), 
     .       dumint, dgdds(13), dgdde(13), dumint, dgdde(9)/pchron,
     .  hrs(1), hrs(3), hrs(4), ddap(14), ddae(14), dumint,
     .        dgdds(14), dgdde(14), dumint, dgdde(9)/pchron,
     .  icanht
       
165   format (' Phenological Event', 7x, 'Day of Year', 2x, 'Date', 2x, 
     .  'DAP', 5x, 'DAE', 5x, 'DAV', 5x, 'GDD AP', 5x, 'GDD AE', 5x, 
     .  'GDD AV', 5x, 'NOLVS', /1x
     . 'Planting Date', 18x, i4, 2x, i2, '/', i2, 4x, i1, 7x, i1, 7x,  
     .    i1, 10x, i1, 10x, i1, 10x, i1, 9x, i1, /1x,
     . 'Emergence', 22x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4,
     .    5x, f6.1, 7x, i4, 7x, i4, 6x, i4, /1x,
     . 'First tiller', 19x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4,
     .    5x, f6.1, 5x, f6.1, 7x, i4,  4x, f6.1, /1x,
     . 'Single ridge', 19x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4,
     .    5x, f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Double ridge', 19x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4,
     .    5x, f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Floret primordia init begins', 3x, i4, 2x, i2, '/', i2, 1x, i4,
     .    4x, i4, 4x, i4, 5x, f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Stem elongation begins',9x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4,
     .     4x, i4, 5x, f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Awn Initials Formed', 12x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    4x, i4, 5x, f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Jointing', 23x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Booting', 24x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4, 5x, 
     .    f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Heading', 24x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, i4, 5x, 
     .    f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Anthesis starts', 16x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Anthesis ends', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Physiological maturity', 9x, i4, 2x, i2, '/', i2, 1x, i4, 4x, 
     .    i4, 4x, i4, 5x, f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Harvest ready', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 7x, i4, 4x, f6.1, /1x,
     . 'Canopy Height (cm)', 11x, i6)

      endif


! Write out hay millet phenology results:

      if (cname .eq. 'Hay Millet') then 
      
!  Write out a table with leaf numbers by DOY
      do while (lnpout(j,2) .lt. dgdde(9)/pchron)
        write (14,54) lnpout(j,1), lnpout(j,2)
        j = j + 1
      end do      

! convert integer boots(1) to a real number
      write(14,54) real (boots(1)), dgdde(9)/pchron
54    format (40x, f5.1, 6x, f4.1) 

      write (14, 74)
74    format (/1x) ! write a blank line after outputting the 
!      leaf number table
 
      write (14, 170) pdatearr(1), pdatearr(3), pdatearr(4), dumint, 
     .       dumint, dumint, dumint, dumint,
     .  ems(1), ems(3), ems(4), ddap(1), dumint, dgdds(1), dumint,
     .       dumint,
     .  tis(1), tis(3), tis(4), ddap(2), ddae(2), dgdds(2), dgdde(2), 
     .        dgdde(2)/pchron,
     .  srs(1), srs(3), srs(4), ddap(3), ddae(3), dgdds(3), dgdde(3), 
     .        dgdde(3)/pchron,
     .  drs(1), drs(3), drs(4), ddap(4), ddae(4), dgdds(4), dgdde(4), 
     .       dgdde(4)/pchron,
     .  fps(1), fps(3), fps(4), ddap(7), ddae(7), dgdds(7), dgdde(7), 
     .       dgdde(7)/pchron,    
     .  ies(1), ies(3), ies(4), ddap(6), ddae(6), dgdds(6), dgdde(6), 
     .       dgdde(6)/pchron,
     .  tss(1), tss(3), tss(4), ddap(5), ddae(5), dgdds(5), dgdde(5), 
     .       dgdde(5)/pchron,
     .  joints(1), joints(3), joints(4), ddap(8), ddae(8), dgdds(8), 
     .       dgdde(8), dgdde(8)/pchron,
     .  boots(1), boots(3), boots(4), ddap(9), ddae(9), dgdds(9), 
     .       dgdde(9), dgdde(9)/pchron,
     .  heads(1), heads(3), heads(4), ddap(10),ddae(10), dgdds(10), 
     .       dgdde(10), dgdde(9)/pchron,
     .  antss(1), antss(3), antss(4), ddap(11), ddae(11), dgdds(11), 
     .       dgdde(11), dgdde(9)/pchron,
     .  antes(1), antes(3), antes(4), ddap(12),ddae(12), dgdds(12), 
     .       dgdde(12), dgdde(9)/pchron,
     .  mats(1), mats(3), mats(4), ddap(13), ddae(13), dgdds(13), 
     .       dgdde(13), dgdde(9)/pchron,
     .  hrs(1), hrs(3), hrs(4), ddap(14), ddae(14), dgdds(14), 
     .       dgdde(14), dgdde(9)/pchron,
     ,  icanht
 170	format (' Phenological Event', 7x, 'Day of Year', 2x, 'Date', 2x, 
     .  'DAP', 5x, 'DAE', 5x, 'GDD AP', 5x, 'GDD AE', 5x, 'NOLVS', /1x
     . 'Planting Date', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 7x, 
     .    i4, 7x, i4, 6x, i4, /1x,
     . 'Emergence', 22x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, f6.1,
     .    7x, i4, 6x, i4, /1x,
     . 'First tiller', 19x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, 
     .    f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Single ridge', 19x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Double ridge', 19x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Floret primordia init begins', 3x, i4, 2x, i2, '/', i2, 1x, i4, 
     .    4x, i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Stem elongation begins', 9x, i4, 2x, i2, '/', i2, 1x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'End spikelet initiation', 8x, i4, 2x, i2, '/', i2, 1x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Jointing', 23x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Booting', 24x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Heading', 24x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4,  
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Anthesis starts', 16x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4,  
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Anthesis ends', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Physiological maturity', 9x, i4, 2x, i2, '/', i2, 1x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Harvest ready', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Canopy Height (cm)', 11x, i6)

      endif
      
! Write out Proso millet phenology results:
      if (cname .eq. 'Proso Millet') then 

!  Write out a table with leaf numbers by DOY
      do while (lnpout(j,2) .lt. dgdde(9)/pchron)
        write (14,55) lnpout(j,1), lnpout(j,2)
        j = j + 1
      end do      

! convert integer boots(1) to a real number
      write(14,55) real (boots(1)), dgdde(9)/pchron
55    format (40x, f5.1, 6x, f4.1) 

      write (14, 75)
75    format (/1x) ! write a blank line after outputting the 
!      leaf number table
         
105   format (40x, f5.1, 6x, f4.1)

 
      write (14, 175) pdatearr(1), pdatearr(3), pdatearr(4), dumint, 
     .       dumint, dumint, dumint, dumint,
     .  ems(1), ems(3), ems(4), ddap(1), dumint, dgdds(1), dumint,
     .       dumint,
     .  tis(1), tis(3), tis(4), ddap(2), ddae(2), dgdds(2), dgdde(2), 
     .        dgdde(2)/pchron,
     .  srs(1), srs(3), srs(4), ddap(3), ddae(3), dgdds(3), dgdde(3), 
     .        dgdde(3)/pchron,
     .  drs(1), drs(3), drs(4), ddap(4), ddae(4), dgdds(4), dgdde(4), 
     .       dgdde(4)/pchron,
     .  fps(1), fps(3), fps(4), ddap(7), ddae(7), dgdds(7), dgdde(7), 
     .       dgdde(7)/pchron,    
     .  ies(1), ies(3), ies(4), ddap(6), ddae(6), dgdds(6), dgdde(6), 
     .       dgdde(6)/pchron,
     .  tss(1), tss(3), tss(4), ddap(5), ddae(5), dgdds(5), dgdde(5), 
     .       dgdde(5)/pchron,
     .  joints(1), joints(3), joints(4), ddap(8), ddae(8), dgdds(8), 
     .       dgdde(8), dgdde(8)/pchron,
     .  boots(1), boots(3), boots(4), ddap(9), ddae(9), dgdds(9), 
     .       dgdde(9), dgdde(9)/pchron,
     .  heads(1), heads(3), heads(4), ddap(10),ddae(10), dgdds(10), 
     .       dgdde(10), dgdde(9)/pchron,
     .  antss(1), antss(3), antss(4), ddap(11), ddae(11), dgdds(11), 
     .       dgdde(11), dgdde(9)/pchron,
     .  antes(1), antes(3), antes(4), ddap(12),ddae(12), dgdds(12), 
     .       dgdde(12), dgdde(9)/pchron,
     .  mats(1), mats(3), mats(4), ddap(13), ddae(13), dgdds(13), 
     .       dgdde(13), dgdde(9)/pchron,
     .  hrs(1), hrs(3), hrs(4), ddap(14), ddae(14), dgdds(14), 
     .       dgdde(14), dgdde(9)/pchron,
     ,  icanht
 175	format (' Phenological Event', 7x, 'Day of Year', 2x, 'Date', 2x, 
     .  'DAP', 5x, 'DAE', 5x, 'GDD AP', 5x, 'GDD AE', 5x, 'NOLVS', /1x
     . 'Planting Date', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 7x, 
     .    i4, 7x, i4, 6x, i4, /1x,
     . 'Emergence', 22x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, f6.1,
     .    7x, i4, 6x, i4, /1x,
     . 'First tiller', 19x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, 
     .    f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Single ridge', 19x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Double ridge', 19x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Floret primordia init begins', 3x, i4, 2x, i2, '/', i2, 1x, i4, 
     .    4x, i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Stem elongation begins', 9x, i4, 2x, i2, '/', i2, 1x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'End spikelet initiation', 8x, i4, 2x, i2, '/', i2, 1x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Jointing', 23x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Booting', 24x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Heading', 24x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4,  
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Anthesis starts', 16x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4,  
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Anthesis ends', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Physiological maturity', 9x, i4, 2x, i2, '/', i2, 1x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Harvest ready', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Canopy Height (cm)', 11x, i6)

      endif      
      
! Write out sorghum phenology reults: 

      if (cname .eq. 'Sorghum') then 
      
!  Write out a table with leaf numbers by DOY
      do while (lnpout(j,2) .lt. dgdde(6)/pchron)
        write (14,56) lnpout(j,1), lnpout(j,2)
        j = j + 1
      end do  
          
! convert integer endlgs(1) to a real number
        write(14,56) real (endlgs(1)), dgdde(6)/pchron
56      format (40x, f5.1, 6x, f4.1) 

      write (14, 76)
76    format (/1x) ! write a blank line after outputting the 
!      leaf number table

      write (14, 180) pdatearr(1), pdatearr(3), pdatearr(4), dumint, 
     .       dumint, dumint, dumint, dumint,
     .  ems(1), ems(3), ems(4), ddap(1), dumint, dgdds(1), dumint,
     .       dumint,
     .  tis(1), tis(3), tis(4), ddap(2), ddae(2), dgdds(2), dgdde(2), 
     .       dgdde(2)/pchron, 
     .  gpds(1), gpds(3), gpds(4), ddap(3), ddae(3), dgdds(3), dgdde(3),
     .       dgdde(3)/pchron, 
     .  ies(1), ies(3), ies(4), ddap(4), ddae(4), dgdds(4), dgdde(4), 
     .       dgdde(4)/pchron,
     .  joints(1), joints(3), joints(4), ddap(5), ddae(5), dgdds(5), 
     .       dgdde(5), dgdde(5)/pchron,     
     .  endlgs(1), endlgs(3), endlgs(4), ddap(6), ddae(6), dgdds(6), 
     .       dgdde(6), dgdde(6)/pchron,  
     .  antss(1), antss(3), antss(4), ddap(7), ddae(7), dgdds(7), 
     .       dgdde(7), dgdde(6)/pchron,
     .  halfbs(1), halfbs(3), halfbs(4), ddap(8), ddae(8), dgdds(8), 
     .       dgdde(8), dgdde(6)/pchron,  
     .  fullbs(1), fullbs(3), fullbs(4), ddap(9), ddae(9), dgdds(9), 
     .       dgdde(9), dgdde(6)/pchron, 
     .  antes(1), antes(3), antes(4), ddap(12), ddae(12), dgdds(12), 
     .       dgdde(12), dgdde(6)/pchron,
     .  mats(1), mats(3), mats(4), ddap(10), ddae(10), dgdds(10), 
     .       dgdde(10), dgdde(6)/pchron,
     .  hrs(1), hrs(3), hrs(4), ddap(11), ddae(11), dgdds(11), 
     .       dgdde(11), dgdde(6)/pchron,
     .  icanht
 180	format (' Phenological Event', 7x, 'Day of Year', 2x, 'Date', 2x, 
     .  'DAP', 5x, 'DAE', 5x, 'GDD AP', 5x, 'GDD AE', 5x, 'NOLVS' /1x 
     . 'Planting Date', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 7x, 
     .    i4, 7x, i4, 6x, i4, /1x,
     . 'Emergence', 22x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, f6.1,
     .    7x, i4, 6x, i4, /1x,
     . 'First tiller', 19x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .     5x, f6.1, 5x, f6.1, 4x, f6.1, /1x, 
     . 'Growing point differentiation', 2x, i4,2x, i2, '/', i2, 1x,
     .     i4, 4x, i4, 5x, f6.1,5x, f6.1, 4x, f6.1, /1x,
     . 'Internode elongation begins', 4x, i4, 2x, i2, '/', i2, 1x, i4, 
     .     4x, i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x, 
     . 'Jointing', 23x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, 
     .     f6.1, 5x, f6.1, 4x, f6.1, /1x,  
     . 'End of leaf growth',13x, i4, 2x, i2, '/', i2, 1x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Anthesis starts', 16x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x, 
     . 'Half bloom', 21x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Full bloom', 21x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Anthesis ends', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x, 
     . 'Physiological maturity', 9x, i4, 2x, i2, '/', i2, 1x, i4, 
     .    4x, i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x, 
     . 'Harvest ready', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .     5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Canopy Height (cm)', 11x, i6)

      endif     

! Write out corn phenology results:

      if (cname .eq. 'Corn') then  
      
!  Write out a table with leaf numbers by DOY
      do while (lnpout(j,2) .lt. dgdde(7)/pchron)
        write (14,57) lnpout(j,1), lnpout(j,2)
        j = j + 1
      end do   
         
! convert integer antss(1) to a real number
        write(14,57) real (antss(1)), dgdde(7)/pchron
57      format (40x, f5.1, 6x, f4.1) 

      write (14, 77)
77    format (/1x) ! write a blank line after outputting the 
!      leaf number table
          
      write (14, 190) pdatearr(1), pdatearr(3), pdatearr(4), dumint, 
     .       dumint, dumint, dumint, dumint,
     .  ems(1), ems(3), ems(4), ddap(1), dumint, dgdds(1), dumint,
     .       dumint,
     .  lf4s(1), lf4s(3), lf4s(4), ddap(2), ddae(2), dgdds(2), 
     .       dgdde(2), dgdde(2)/pchron,
     .  tsints(1), tsints(3), tsints(4), ddap(3), ddae(3), dgdds(3), 
     .       dgdde(3), dgdde(3)/pchron,     
     .  ears(1), ears(3), ears(4), ddap(4), ddae(4), dgdds(4), dgdde(4),
     .       dgdde(4)/pchron,
     .  ies(1), ies(3), ies(4), ddap(5), ddae(5), dgdds(5), dgdde(5), 
     .       dgdde(5)/pchron,     
     .  lf12s(1), lf12s(3), lf12s(4), ddap(6), ddae(6), dgdds(6), 
     .       dgdde(6), dgdde(6)/pchron,
     .  antss(1), antss(3), antss(4), ddap(7), ddae(7), dgdds(7), 
     .       dgdde(7), dgdde(7)/pchron,
     .  silks(1), silks(3), silks(4), ddap(8), ddae(8), dgdds(8), 
     .       dgdde(8), dgdde(7)/pchron,
     .  blstrs(1), blstrs(3), blstrs(4), ddap(9), ddae(9), dgdds(9), 
     .       dgdde(9), dgdde(7)/pchron,
     .  milks(1), milks(3), milks(4), ddap(10), ddae(10), dgdds(10), 
     .       dgdde(10), dgdde(7)/pchron,
     .  doughs(1), doughs(3), doughs(4), ddap(11), ddae(11), 
     .       dgdds(11), dgdde(11), dgdde(7)/pchron,
     .  dents(1), dents(3), dents(4), ddap(12), ddae(12), dgdds(12), 
     .       dgdde(12), dgdde(7)/pchron,
     .  mats(1), mats(3), mats(4), ddap(13), ddae(13), dgdds(13), 
     .       dgdde(13), dgdde(7)/pchron,
     .  hrs(1), hrs(3), hrs(4), ddap(14), ddae(14), dgdds(14), 
     .       dgdde(14), dgdde(7)/pchron,
     ,  icanht
 190  format (' Phenological Event', 7x, 'Day of Year', 2x, 'Date', 2x, 
     . 'DAP', 5x, 'DAE', 5x, 'GDD AP', 5x, 'GDD AE', 5x, 'NOLVS', /1x
     . 'Planting Date', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 7x, 
     .    i4, 7x, i4, 6x, i4, /1x,
     . 'Emergence', 22x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, f6.1,
     .    7x, i4, 6x, i4, /1x,
     . 'Leaf 4 (V4)', 20x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, 
     .    f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Tassel initiation', 14x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,     
     . 'Ear initiation', 17x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, 
     .    f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Internode elongation begins', 4x, i4, 2x, i2, '/', i2, 1x, i4, 
     .    4x, i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Leaf 12 (V12)', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, 
     .    f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Tasseling', 22x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, f6.1, 
     .    5x, f6.1, 4x, f6.1, /1x,
     . 'Silking (R1)', 19x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, 
     .    f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Blister (R2)', 19x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, 
     .    f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Milk (R3)', 22x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, 
     .    f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Dough (R4)', 21x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, f6.1,
     .    5x, f6.1, 4x, f6.1, /1x,
     . 'Dent (R5)', 22x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, f6.1, 
     .    5x, f6.1, 4x, f6.1, /1x,
     . 'Maturity (R6)', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, 
     .    f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Harvest Ready', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, 
     .     f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Canopy Height (cm)', 11x, i6)
     
       endif
     
      if (cname .eq. 'Sunflower') then
      
!  Write out a table with leaf numbers by DOY
      do while (lnpout(j,2) .lt. dgdde(9)/pchron)
        write (14,58) lnpout(j,1), lnpout(j,2)
        j = j + 1
      end do      

! convert integer antss(1) to a real number
      write(14,58) real (antss(1)), dgdde(9)/pchron
58    format (40x, f5.1, 6x, f4.1) 

      write (14, 78)
78    format (/1x) ! write a blank line after outputting the 
!      leaf number table
      
      write (14, 200) pdatearr(1), pdatearr(3), pdatearr(4), dumint, 
     .       dumint, dumint, dumint, dumint,     
     .  ems(1), ems(3), ems(4), ddap(1), dumint, dgdds(1), dumint,
     .       dumint,
     .  lf4s(1), lf4s(3), lf4s(4), ddap(2), ddae(2), dgdds(2), 
     .       dgdde(2), dgdde(2)/pchron,
     .  lf8s(1), lf8s(3), lf8s(4), ddap(3), ddae(3), dgdds(3), dgdde(3),
     .       dgdde(3)/pchron,
     .  lf12s(1), lf12s(3), lf12s(4), ddap(4), ddae(4), dgdds(4), 
     .       dgdde(4), dgdde(4)/pchron,
     .  infls(1), infls(3), infls(4), ddap(5), ddae(5), dgdds(5), 
     .       dgdde(5), dgdde(5)/pchron,     
     .  ies(1), ies(3), ies(4), ddap(6), ddae(6), dgdds(6), dgdde(6), 
     .       dgdde(6)/pchron,
     .  ies2(1), ies2(3), ies2(4), ddap(7), ddae(7), dgdds(7), dgdde(7),
     .       dgdde(7)/pchron,
     .  opens(1), opens(3), opens(4), ddap(8), ddae(8), dgdds(8), 
     .       dgdde(8), dgdde(8)/pchron,
     .  antss(1), antss(3), antss(4), ddap(9), ddae(9), dgdds(9), 
     .       dgdde(9), dgdde(9)/pchron,
     .  antes(1), antes(3), antes(4), ddap(10), ddae(10), dgdds(10), 
     .       dgdde(10), dgdde(9)/pchron,
     .  yelows(1), yelows(3), yelows(4), ddap(11), ddae(11), dgdds(11), 
     .       dgdde(11), dgdde(9)/pchron,
     .  browns(1), browns(3), browns(4), ddap(12), ddae(12), dgdds(12), 
     .       dgdde(12), dgdde(9)/pchron,
     .  mats(1), mats(3), mats(4), ddap(13), ddae(13), dgdds(13), 
     .       dgdde(13), dgdde(9)/pchron,
     .  hrs(1), hrs(3), hrs(4), ddap(14), ddae(14), dgdds(14), 
     .       dgdde(14), dgdde(9)/pchron,
     .  icanht
 200	format (' Phenological Event', 7x, 'Day of Year', 2x, 'Date', 2x, 
     . 'DAP', 5x, 'DAE', 5x, 'GDD AP', 5x, 'GDD AE', 5x, 'NOLVS', /1x
     . 'Planting Date', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 7x, 
     .    i4, 7x, i4, 6x, i4, /1x,
     . 'Emergence', 22x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, f6.1,
     .    7x, i4, 6x, i4, /1x,
     . 'Leaf 4', 25x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, 
     .    f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Leaf 8', 25x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, f6.1, 5x,
     .    f6.1, 4x, f6.1, /1x,
     . 'Leaf 12', 24x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, f6.1, 
     .    5x, f6.1, 4x, f6.1, /1x,
     . 'Inflorescence visible', 10x, i4, 2x, i2, '/', i2, 1x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Internode elongation', 11x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4,
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Internode elongation > 2', 7x, i4, 2x, i2, '/', i2, 1x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Inflorescence opens', 12x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Anthesis starts', 16x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, 
     .    f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Anthesis ends',   18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, 
     .    f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Head back yellow', 15x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x,
     .    f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Head yellow brown', 14x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Physiological maturity', 9x, i4, 2x, i2, '/', i2, 1x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Harvest Ready',  18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, 
     .    f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Canopy Height (cm)', 11x, i6)

      endif

!debe added dry beans variables
            if (cname .eq. 'Dry Beans') then
      
!  Write out a table with leaf numbers by DOY
! Dry beans are determinate if bush type and indeterminate if any of the
! other growth types. Currently, set the leaf number calculation to be indeterminate 
! and therefore produce leaves up to growth stage R3 - early pod set. 
      do while (lnpout(j,2) .lt. dgdde(9)/pchron)
        write (14,59) lnpout(j,1), lnpout(j,2)
        j = j + 1
      end do      

! convert integer epods(1) to a real number
      write(14,59) real (epods(1)), dgdde(9)/pchron
59    format (40x, f5.1, 6x, f4.1) 

      write (14, 79)
79    format (/1x) ! write a blank line after outputting the 
!      leaf number table
      
      write (14, 210) pdatearr(1), pdatearr(3), pdatearr(4), dumint, 
     .       dumint, dumint, dumint, dumint,     
     .  ems(1), ems(3), ems(4), ddap(1), dumint, dgdds(1), dumint,
     .       dumint,
     .  cots(1), cots(3), cots(4), ddap(2), ddae(2), dgdds(2), 
     .       dgdde(2), dgdde(2)/pchron,
     .  lf1s(1), lf1s(3), lf1s(4), ddap(3), ddae(3), dgdds(3), 
     .       dgdde(3), dgdde(3)/pchron,
     .  lf2s(1), lf2s(3), lf2s(4), ddap(4), ddae(4), dgdds(4), 
     .       dgdde(4), dgdde(4)/pchron,
     .  lf3s(1), lf3s(3), lf3s(4), ddap(5), ddae(5), dgdds(5), 
     .       dgdde(5), dgdde(5)/pchron,     
     .  lf4s(1), lf4s(3), lf4s(4), ddap(6), ddae(6), dgdds(6), 
     .       dgdde(6), dgdde(6)/pchron,
     .  antss(1), antss(3), antss(4), ddap(7), ddae(7), dgdds(7), 
     .       dgdde(7), dgdde(7)/pchron,
     .  mffls(1), mffls(3), mffls(4), ddap(8), ddae(8), dgdds(8), 
     .       dgdde(8), dgdde(8)/pchron,
     .  epods(1), epods(3), epods(4), ddap(9), ddae(9), dgdds(9), 
     .       dgdde(9), dgdde(9)/pchron,
     .  mpods(1), mpods(3), mpods(4), ddap(10), ddae(10), dgdds(10), 
     .       dgdde(10), dgdde(9)/pchron,
     .  eseeds(1), eseeds(3), eseeds(4), ddap(11), ddae(11), dgdds(11), 
     .       dgdde(11), dgdde(9)/pchron,
     .  mseeds(1), mseeds(3), mseeds(4), ddap(12), ddae(12), dgdds(12), 
     .       dgdde(12), dgdde(9)/pchron,
     .  mats(1), mats(3), mats(4), ddap(13), ddae(13), dgdds(13), 
     .       dgdde(13), dgdde(9)/pchron,
     .  hrs(1), hrs(3), hrs(4), ddap(14), ddae(14), dgdds(14), 
     .       dgdde(14), dgdde(9)/pchron,
     .  icanht
 210	format (' Phenological Event', 7x, 'Day of Year', 2x, 'Date', 2x, 
     . 'DAP', 5x, 'DAE', 5x, 'GDD AP', 5x, 'GDD AE', 5x, 'NOLVS', /1x
     . 'Planting Date', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 7x, 
     .    i4, 7x, i4, 6x, i4, /1x,
     . 'Emergence', 22x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, f6.1,
     .    7x, i4, 6x, i4, /1x,
     . 'Cotyledonary lvs (VC)', 10x, i4, 2x, i2, '/', i2, 1x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . '1st trifoliolate lf (V1)', 7x, i4, 2x, i2, '/', i2, 1x, i4, 4x,
     .    i4, 5x, f6.1, 5x,f6.1, 4x, f6.1, /1x,
     . '2nd trifoliolate lf (V2)', 7x, i4, 2x, i2, '/', i2, 1x, i4, 4x,
     .    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . '3rd trifoliolate lf (V3)', 7x, i4, 2x, i2, '/', i2, 1x, i4, 4x,
     .    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . '4th trifoliolate lf (V4)', 7x, i4, 2x, i2, '/', i2, 1x, i4, 4x,
     .    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Bloom (R1)', 21x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Mid-full Flower (R2)', 11x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4,
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Early pod set (R3)', 13x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Mid pod set (R4)', 15x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x,
     .    f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Early seed fill (R5)', 11x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4,
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Mid seed fill (R6)', 13x, i4, 2x, i2, '/', i2, 1x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Physiological maturity (R7)',  4x, i4, 2x, i2, '/', i2, 1x, i4, 
     .    4x, i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Harvest Ready (R8)', 13x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Canopy Height (cm)', 11x, i6)

            endif

            !debe added soybean variables
            if (cname .eq. 'Soybean') then

                
!  Write out a table with leaf numbers by DOY
! Soybeans are indeterminate or determinate. Most of the varieties grown in 
! the northern US and in the Midwest are indeterminate and that is what is used here.
! Set the leaf number calculation to be indeterminate and therefore produce 
! leaves up to growth stage R5.5. Use R5 - Beginning Seed. 
      do while (lnpout(j,2) .lt. dgdde(12)/pchron)
 !       write (14,59) lnpout(j,1), lnpout(j,2)
        write (14,60) lnpout(j,1), lnpout(j,2)
        j = j + 1 
      end do    
        
! convert integer eseeds(1) to a real number because this stage (actaully R5.5) is the end of vegetative growth.
      write(14,60) real (eseeds(1)), dgdde(12)/pchron
60    format (40x, f5.1, 6x, f4.1) 

      write (14, 80)
80    format (/1x) ! write a blank line after outputting the 
!      leaf number table
      
      write (14, 215) pdatearr(1), pdatearr(3), pdatearr(4), dumint, 
     .       dumint, dumint, dumint, dumint,     
     .  ems(1), ems(3), ems(4), ddap(1), dumint, dgdds(1), dumint,
     .       dumint,
     .  cots(1), cots(3), cots(4), ddap(2), ddae(2), dgdds(2), 
     .       dgdde(2), dgdde(2)/pchron,
     .  lf1s(1), lf1s(3), lf1s(4), ddap(3), ddae(3), dgdds(3), 
     .       dgdde(3), dgdde(3)/pchron,
     .  lf2s(1), lf2s(3), lf2s(4), ddap(4), ddae(4), dgdds(4), 
     .       dgdde(4), dgdde(4)/pchron,
     .  lf3s(1), lf3s(3), lf3s(4), ddap(5), ddae(5), dgdds(5), 
     .       dgdde(5), dgdde(5)/pchron,     
     .  lf4s(1), lf4s(3), lf4s(4), ddap(6), ddae(6), dgdds(6), 
     .       dgdde(6), dgdde(6)/pchron,
     .  lf5s(1), lf5s(3), lf5s(4), ddap(7), ddae(7), dgdds(7),
     .       dgdde(7), dgdde(7)/pchron, 
     .  antss(1), antss(3), antss(4), ddap(8), ddae(8), dgdds(8), 
     .       dgdde(8), dgdde(8)/pchron,
     .  mffls(1), mffls(3), mffls(4), ddap(9), ddae(9), dgdds(9), 
     .       dgdde(9), dgdde(9)/pchron,
     .  epods(1), epods(3), epods(4), ddap(10), ddae(10), dgdds(10), 
     .       dgdde(10), dgdde(10)/pchron,
     .  mpods(1), mpods(3), mpods(4), ddap(11), ddae(11), dgdds(11), 
     .       dgdde(11), dgdde(11)/pchron,
     .  eseeds(1), eseeds(3), eseeds(4), ddap(12), ddae(12), dgdds(12), 
     .       dgdde(12), dgdde(12)/pchron,
     .  mseeds(1), mseeds(3), mseeds(4), ddap(13), ddae(13), dgdds(13), 
     .       dgdde(13), dgdde(12)/pchron,
     .  bmats(1), bmats(3), bmats(4), ddap(14), ddae(14), dgdds(14), 
     .       dgdde(14), dgdde(12)/pchron,    
     .  mats(1), mats(3), mats(4), ddap(15), ddae(15), dgdds(15), 
     .       dgdde(15), dgdde(12)/pchron,
     .  hrs(1), hrs(3), hrs(4), ddap(16), ddae(16), dgdds(16), 
     .       dgdde(16), dgdde(12)/pchron,
     .  icanht
 215  format (' Phenological Event', 7x, 'Day of Year', 2x, 'Date', 2x, 
     . 'DAP', 5x, 'DAE', 5x, 'GDD AP', 5x, 'GDD AE', 5x, 'NOLVS', /1x,
     . 'Planting Date', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 7x, 
     .    i4, 7x, i4, 6x, i4, /1x,
     . 'Emergence', 22x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x, f6.1,
     .    7x, i4, 6x, i4, /1x,
     . 'Cotyledonary lvs (VC)', 10x, i4, 2x, i2, '/', i2, 1x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . '1st trifoliolate lf (V1)', 7x, i4, 2x, i2, '/', i2, 1x, i4, 4x,
     .    i4, 5x, f6.1, 5x,f6.1, 4x, f6.1, /1x,
     . '2nd trifoliolate lf (V2)', 7x, i4, 2x, i2, '/', i2, 1x, i4, 4x,
     .    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . '3rd trifoliolate lf (V3)', 7x, i4, 2x, i2, '/', i2, 1x, i4, 4x,
     .    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . '4th trifoliolate lf (V4)', 7x, i4, 2x, i2, '/', i2, 1x, i4, 4x,
     .    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,     
     . '5th trifoliolate lf (V5)', 7x, i4, 2x, i2, '/', i2, 1x, i4, 4x,
     .    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Beginning Bloom (R1)', 11x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Full Bloom (R2)', 16x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4,
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Beginning Pod (R3)', 13x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Full Pod (R4)', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x,
     .    f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Beginning Seed (R5)', 12x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4,
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Full Seed (R6)', 17x, i4, 2x, i2, '/', i2, 1x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Beginning Maturity (R7)', 8x, i4, 2x, i2, '/', i2, 1x, i4, 4x, 
     .    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,  
     . 'Full Maturity (R8)',  13x, i4, 2x, i2, '/', i2, 1x, i4, 
     .    4x, i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Harvest Ready', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 
     .    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,
     . 'Canopy Height (cm)', 11x, i6)

      endif

	close (unit=14)
      
      return
      end