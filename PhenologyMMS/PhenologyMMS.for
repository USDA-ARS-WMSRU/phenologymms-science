!  PhenologyMMS_NCB.for 

!  FUNCTIONS:
!	PhenologyMMS      - Entry point of console application.


!****************************************************************************
!
!  PROGRAM: PhenologyMMS
!
!  PURPOSE:  Predict the growth stages of various crops.
!
!  Program developed by Greg McMaster (started 10/22/02)
!                                     (last changed 1/21/05) 
!
!****************************************************************************

!  INPUTS:  antss(R), daa(C,R), dae(C,R), dap(C,R), dav(C,R), daynum(R), 
!           ems(R), first1(C,R), first2(C,R), gdda(C,R), gddday(R), 
!           gdde(C,R), gdds(C,R), gddv(C,R), hrs(R), tmax(R), tmin(R), 
!           pdate(R), precip(R), ri(R), soil3t(R), verns(R), weather(R)

!  OUTPUTS: daa(C,R), dae(C,R), dap(C,R), dav(C,R), daynum(R), 
!           first1(C,R), first2(C,R), gdda(C,R), gdde(C,R), gdds(C,R), 
!           gddv(C,R)


      program PhenologyMMS

	implicit none

      integer  aifs(4), antes(4), antss(4), blstrs(4), boots(4), 
     c browns(4), daa, dae, dap, dav, day, daynum, ddae(20), ddap(20), 
     c ddav(20), dents(4), doughs(4), drs(4), ears(4), ems(4), 
     c endlgs(4), first1, first2, first7, fps(4), fullbs(4), germs(4), 
     c gmethod, gpds(4), halfbs(4), heads(4), hrs(4), ies(4), ies2(4), 
     c infls(4), joints(4), lf12s(4), lf4s(4), lf8s(4), lncntr, 
     c mats(4), milks(4), mo, pmo, mon, nai, noseeds, 
     c opens(4), pdate, pday, pmethod, rai, rowcntr, seedsw, 
     c silks(4), srs(4), tis(4), tsints(4), tss(4), verns, wai, pyear,
     c year, yelows(4)  
          
      real  aepa, canht, daylth, dgdde(20), dgdds(20), dgddv(20),   
     c dummy2(15), elong, elrate, ergdd(4), gdda, gddday, gdde, gdds, 
     c gddtbg, gddv, germd, germgdd(4), latitude, lnarray(400,2), 
     c lnpout(60,2), maxht, nolvs, pchron, pdepth, precip, ri, soil3t,  
     c tavg, tbase, tmax, tmin, todayln, toptlo, toptup, tupper, vernal,
     c wfpslo(4), wfpsup(4), wlow, wup, yestln
!debe moved pdepth to real from integer.
         
      character *22  cname, dummy1(15), hemisp, outf, pequation, 
     c soilwat(4), swtype, vname, weather
               
        
! Initialize the variables in several init subroutines.

      call initparm(cname, day, elrate, germd, gmethod, hemisp, 
     c latitude, maxht, mo, noseeds, pdate, pdepth, pequation, pmethod, 
     c seedsw, swtype, tbase, toptlo, toptup, tupper, vname, weather, 
     c wlow, wup, year)
     
      call initgs(aifs, antes, antss, blstrs, boots, browns, 
     c dents, doughs, drs, dummy1, dummy2, ears, ems, endlgs, first7, 
     c fps, fullbs, germs, gpds, halfbs, heads, hrs, ies, ies2, infls, 
     c joints, lf12s, lf4s, lf8s, mats, milks, nolvs, opens, silks, srs,
     c tis, tsints, tss, verns, yelows)
     
      call initday(daa, dae, dap, dav, daynum, ddae, ddap, ddav, 
     c dgdde, dgdds, dgddv, first1, first2, gdda, gddday, gdde, gdds, 
     c gddtbg, gddv, lnarray, lncntr, lnpout, mon, outf, rowcntr, 
     c todayln, yestln)
     
      call initcepv(canht, elong, ergdd, germgdd, nai, pchron, 
     c rai, soilwat, vernal, wai, wfpslo, wfpsup)
     
      call initwthr(precip, ri, soil3t, tavg, tmax, tmin)  
           
! End of initializing routines             
!  Call SETUP to read in various input files and set model up for running.
!   Start with weather in declaring the variables type up above.
      call setup(cname, pday, dummy1, dummy2, elrate, ergdd, 
     c germd, germgdd, gmethod, latitude, maxht, pmo, noseeds, 
     c pchron, pdate, pdepth, pequation, pmethod, seedsw, soilwat, 
     c swtype, tbase, toptlo, toptup, tupper, vname, weather, wfpslo,
     c wfpsup, wlow, wup, pyear)

! Read from weather file here.  Will need to read in comment lines.
! 90   continue
 95   continue
       open (unit=14, file= 'MMSWeather/' // weather, status='OLD')

      read(14,*,end=640,err=101) year,daynum,tmax,tmin,ri,precip,soil3t 
                 
! Do error checking on input weather variables
      if ((year .lt. 0000) .or. (year .gt. 2100)) then
          print *, 'Variable year is out of range in weather file'
            print *, 'year = ', year
      elseif ((daynum .lt. 1) .or. (daynum .gt. 366)) then
          print *, 'Variable daynum is out of range in weather file'
	      print *, 'daynum = ', daynum 
      elseif ((tmax .lt. -50.0) .or. (tmax .gt. 60.0)) then
          print *, 'Variable tmax is out of range in weather file'
	      print *, 'daynum = ', daynum
      elseif ((tmin .lt. -50.0) .or. (tmin .gt. 60.0)) then
          print *, 'Variable tmin is out of range in weather file'
	      print *, 'daynum = ', daynum	      	     
      elseif ((ri.ne.999.9).and.((ri .lt. 0.0).or.(ri.gt.41.7))) then
          print *, 'Variable ri is out of range in weather file'
	      print *, 'daynum = ', daynum
!DE changed next statement to .gt. 9999.9 to test weather file	      
      elseif ((precip .lt. 0.0) .or. (precip .gt. 9999.9)) then
          print *, 'Variable precip is out of range in weather file'
	      print *, 'daynum = ', daynum
      elseif ((soil3t.ne.999.9).and.((soil3t.lt.-50.0).or.
     .	  (soil3t.gt.60.0))) then
        print *, 'Variable soil3t is out of range in weather file'
        print *, 'daynum = ', daynum
      endif
 
!  If planting date (PDATE) has not been reached, then skip to next day by
!  going to the end of this program:

        if(pdate .ne. daynum .and. first1 .eq. 0)then
         	go to 540
        elseif (pyear .ne. year .and. first1 .eq. 0) then
            go to 540 
        else    
	      first1 = 1

	endif


! Call subroutine GDDCALC to calculate growing degree-days.  Subroutine
!   GDDCALC must know which method of calculation is to be used, and
!   what the base and upper temperature thresholds are.

      call gddcalc(gddday, gmethod, tbase, tmax, tmin, tupper)

!  Accumulate GDD from sowing (GDDS) and days after planting (DAP):

	  gdds = gdds + gddday
	  dap = dap + 1

!  Determine if seedling emergence has occurred:

      call emerge(dap, daynum, ddap, dgdds, elong, ems, ergdd,
     c gddday, gdds, gddtbg, germgdd, germs, pdepth, precip, seedsw, 
     c year)

! The day that 50% seedling emergence has occurred, call PHYLLO:

      if (ems(1) .ne. 999 .and. first2 .eq. 0) then
	      first2 = 1
!         Get the change in daylength first to pass to phyllo
          call daylenth(daylth, daynum, latitude)      
		  call phyllo(daylth)
		  
	  endif

!  Accumulate GDD from emergence (GDDE) and days after emergence (DAE)
!  once it has occurred:

	  if (ems(1) .ne. 999) then
			gdde = gdde + gddday
			dae = dae + 1
			
!  Call the leafno subroutine to calculate the number of leaves once
!  emergence has occurred for the leaf number output table.
         call leafno (daynum, gdde, lnarray, lncntr, lnpout,  
     .       pchron, rowcntr, todayln, yestln )
			
	  endif
	  

! Once seedling emergence has occurred, call phenology, and 
! vernalization subroutines:

      if (ems(1) .ne. 999) then       
!  Call vernalization subroutine if vernalization requirement has not
!  been satisfied:
           if (verns .eq. 999) 
     c          call vernaliz(daynum, hemisp, vernal, verns)

! Calculate growing degree-days (GDDV) and number of days after
! vernalization requirement was satisfied (DAV) for winter crops.
! For spring crops, somewhere want to set verns = 1.0, meaning no
! vernalization requirement.

	       if (verns .ne. 999) then
	           gddv = gddv + gddday
		         dav = dav + 1
	       endif

	  endif	 
	     
! Calculate growing degree-days (GDDA) and # of days (DAA) after anthesis:
      if (antss(1) .ne. 999) then
            gdda = gdda + gddday
			daa = daa + 1
	endif

! moved canopy height below updating of antss so that the current days value
! is input to canopy height.	
!  Call canopy height subroutine  (DE added)
      call canopyht(antss, canht, cname, dummy2, ems, gddday, gdde,  
     c              ies, joints, lf4s, maxht)

! moved the call to phenol after other items above are updated to pass into
! phenol.
      call phenol(aepa, aifs, antes, antss, blstrs, boots, browns, 
     c cname, dae, dap, dav, daynum, ddae, ddap, ddav, dents, dgdde, 
     c dgdds, dgddv, doughs, drs, dummy2, ears, endlgs, first7, fps, 
     c fullbs, gdde, gdds, gddv, gpds, halfbs, heads, hrs, ies, ies2, 
     c infls, joints, lf12s, lf4s, lf8s, mats, milks, nolvs, opens, 
     c pchron, silks, srs, tis, tsints, tss, year, yelows)


!  Go back and read the next day's weather. When harvest ripe has been
!  reached, then finish program:

 540  continue

! If harvest ripe has not been reached, go back and read in next day's
! weather:

	if (hrs(1) .eq. 999) go to 95
 
 640  continue

 101  continue

!  Call the output file when simulation is done:

      call output(aifs, antes, antss, blstrs, boots, browns, 
     c canht, cname, daa, dae, dap, dav, ddae, ddap, ddav, dents, dgdde,
     c dgdds, dgddv, doughs, drs, ears, ems, endlgs, fps, fullbs, gdda, 
     c gdde, gdds, gddv, gpds, halfbs, heads, hrs, ies, ies2, infls, 
     c joints, lf12s, lf4s, lf8s, lnarray, lnpout, mats, milks, nolvs, 
     c opens, outf, pchron, pdate, pyear, silks, srs, tis, tsints, tss,
     c year, yelows)

      end program PhenologyMMS
