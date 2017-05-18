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
     c browns(4), cots(4), daa, dae, dap, dav, day, daynum, ddae(20), 
     c ddap(20), ddav(20), dents(4), doughs(4), drs(4), ears(4), ems(4),
     c endlgs(4), epods(4), eseeds(4), first1, first2, first7, fps(4), 
     c fullbs(4), germs(4), gmethod, gpds(4), halfbs(4), heads(4), 
     c hrs(4), ies(4), ies2(4), infls(4), joints(4), lf1s(4), lf12s(4), 
     c lf2s(4), lf3s(4), lf4s(4), lf8s(4), lncntr, mats(4), mffls(4), 
     c milks(4), mo, mpods(4), mseeds(4), pmo, mon, nai, noseeds, 
     c opens(4), pdate, pday, pmethod, rai, rowcntr, seedsw, 
     c silks(4), srs(4), tis(4), tsints(4), tss(4), verns, wai, pyear,
     c year, yelows(4)  
!debe added variables for dry beans. 
         
      real  aepa, canht, civilrise, cumvd, daylen, daylth, degtorad, 
     c devern, df, dgdde(20), dgdds(20), dgddv(20), dummy2(15), elong, 
     c elrate, ergdd(4), gdda, gddday, gdde, gdds, gddtbg, gddv, germd, 
     c germgdd(4), hrlt, latitude, lnarray(400,2), lnpout(60,2), maxht, 
     c nolvs, pchron, p1d, pdepth, precip, p1v, radtodeg, ri, soil3t, 
     c tavg, tbase, tmax, tmin, todayln, toptlo, toptup, tupper, vernal,
     c vd, vtbase, vtoptlo, vtoptup, vtupper, wfpslo(4), 
     c wfpsup(4), wlow, wup, yestln !vf, vf0,   
     
!debe moved pdepth to real from integer.
!debe added variables for photoperiod: civilrise, hrlt, daylen, df

         
      character *22  cname, dummy1(15), hemisp, outf, pequation, 
     c soilwat(4), swtype, vname, weather
               
        
! Initialize the variables in several init subroutines.

      call initparm(cname, day, elrate, germd, gmethod, hemisp, 
     c latitude, maxht, mo, noseeds, pdate, pdepth, pequation, pmethod,
     c seedsw, swtype, tbase, toptlo, toptup, tupper, vname, weather, 
     c wlow, wup, year)
     
!debe added dry bean variables.     
      call initgs(aifs, antes, antss, blstrs, boots, browns, cots,
     c dents, doughs, drs, dummy1, dummy2, ears, ems, endlgs, epods, 
     c eseeds, first7, fps, fullbs, germs, gpds, halfbs, heads, hrs, 
     c ies, ies2, infls, joints, lf1s, lf12s, lf2s, lf3s, lf4s, lf8s, 
     c mats, mffls, milks, mpods, mseeds, nolvs, opens, silks, srs,
     c tis, tsints, tss, verns, yelows)
     
      call initday(daa, dae, dap, dav, daynum, ddae, ddap, ddav, 
     c dgdde, dgdds, dgddv, first1, first2, gdda, gddday, gdde, gdds, 
     c gddtbg, gddv, lnarray, lncntr, lnpout, mon, outf, rowcntr, 
     c todayln, yestln)
           
      call initcepv(canht, civilrise, cumvd, degtorad, devern, df,  
     c elong, ergdd, germgdd, hrlt, nai, pchron, p1d, p1v, radtodeg, 
     c rai,soilwat, vd, vernal, vtbase, vtoptlo, vtoptup, 
     c vtupper, wai, wfpslo, wfpsup) !vf, vf0, 
     
      call initwthr(precip, ri, soil3t, tavg, tmax, tmin)
      
!debe added the photoperiod coefficient to be set here:
      p1d = 30.0 
             
!debe added the following vernalization stuff:
!Set p1v and vf0 for vernalization. This doesn't need to be done every day.
!  p1v is the vernalization coefficient and should be read in from a file.
!  For now, hardwire it here. 
      p1v = 50
! vf0 is the relative development rate when the crop is unvernalized	
!      vf0 = 1-(p1v/50)
!Need to read in vtbase, vtoptlo, vtoptup, vtupper from a file. Read in 
!devern value too. These are constant values for a crop and only need to 
!be read in once.
!For now hardwire these values as follows:
      vtbase = -5.0
      vtoptlo = 0.0
      vtoptup = 15.0
      vtupper = 30.0
!devern - value above which devernalization occurs
      devern = 30.0
! End of initializing routines and variables
            
!  Call SETUP to read in various input files and set model up for running.
!   Start with weather in declaring the variables type up above.
      call setup(cname, pday, dummy1, dummy2, elrate, ergdd, 
     c germd, germgdd, gmethod, latitude, maxht, pmo, noseeds, 
     c pchron, pdate, pdepth, pequation, pmethod, seedsw, soilwat, 
     c swtype, tbase, toptlo, toptup, tupper, vname, weather, wfpslo,
     c wfpsup, wlow, wup, pyear)

! Read from weather file here.  Will need to read in comment lines.
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
      
! call daylen subroutine to calculate the day's daylength in hours
!   initial daylength calculations (From UPGM) debe added.
!   Variable definitions:
!   amalat - latitude of simulation site
!   hrlt - day length on day i. hr.
!   pdate - day of year planting can occur
!   civilrise - parameter. solar altitude angle defined as civil 
!               twilight.

        ! Should it be pdate or day of year? It is initialized with planting date.
! Other dates are used in UPGM as appropriate. It seems daynum would be
! sufficient so will use it here.
        hrlt = daylen(latitude, daynum, civilrise, degtorad, radtodeg)
!        hrlty = daylen(amalat,pdate-1,civilrise)
        
        ! call photoper to calculate the daylength factor.
        call photoper(daynum, df, hrlt, p1d) !hemisp, , vernal, verns)
!         print *, 'daylength factor = ', df

!       Get the change in daylength first to pass to phyllo
        call daylenth(daylth, daynum, latitude)!is this still needed?   
        call phyllo(daylth)!is this still needed?		  

!debe added the vernaliz call and changes     
! Call vernaliz to calculate the vernalization factor. 
! When vd is equal to or greater than 40.0 for winter wheat, don't call 
! vernaliz any more.
        if(vd .LT. 50.0) then  
          call vernaliz(cumvd, daynum, devern, hemisp, tmax, tmin, vd, 
     c    vtbase, vtoptlo, vtoptup, vtupper) !vernal, verns, 
          vd = vd + cumvd  
          !calculate the vernalization factor vf
!          vf = vf0 + (cumvd/p1v)
!          if (vd .ge. 10.0) then
    !        print*,'vd = ', vd, 'daynum = ', daynum
!          endif  
  !      print *, 'vernalization factor = ', vf, 'vd = ', vd
        endif
        
! Call subroutine GDDCALC to calculate growing degree-days.  Subroutine
!   GDDCALC must know which method of calculation is to be used, and
!   what the base, optimal range and upper temperature thresholds are.
! Need to calculate the day's gddday value before calling emerge.
        call gddcalc(daynum, df, gddday, gmethod, tbase, tmax, tmin, 
     c               toptlo, toptup, tupper) !, vf)
        
!!  Call vernalization subroutine if vernalization requirement has not
!!  been satisfied:
!           if (verns .eq. 999) 
!     c          call vernaliz(daynum, hemisp, vernal, verns)
       
! Calculate growing degree-days (GDDV) and number of days after
! vernalization requirement was satisfied (DAV) for all crops.
   !   print*,'before first if vernal =', vernal
        if ((vd .ge. 50.0) .and. (vernal .eq. 0)) then
            verns = daynum
            vernal = 1
  !      print *, 'in first if verns = ', verns
        endif
        
        if ((vd .ge. 50.0) .and. (verns .ne. daynum)) then 
	      gddv = gddv + gddday !this is happening too early for single ridge in winter wheat
		    dav = dav + 1
!		    if (dav .eq. 1) then
!		print *, 'in second if verns=',verns,'dav=',dav, 'vernal=',vernal,
!     c 'daynum= ', daynum 
!            endif   
	  endif

!Sum gdd and days after planting:      
!debe don't allow planting day to count as one day after planting	
 !     print *, 'year = ', year, 'pyear = ', pyear
	  if ((daynum .GT. pdate) .OR. (year .GT. pyear)) then
	      gdds = gdds + gddday !moved gdds code inside the if statment
	      dap = dap + 1        !to match the way it is done for gdde below
	  endif
!	  print*, 'dap = ', dap, 'gddday = ', gddday
  !      print *, 'daynum = ', daynum, 'gddday = ', gddday

!  Determine if seedling emergence has occurred:
      call emerge(dap, daynum, ddap, dgdds, elong, ems, ergdd,
     c gddday, gdds, gddtbg, germgdd, germs, pdepth, precip, seedsw, 
     c year)

!Is this next if statement needed? first2 is not used.
! The day that 50% seedling emergence has occurred, call PHYLLO:
      if (ems(1) .ne. 999 .and. first2 .eq. 0) then
	      first2 = 1
	endif
	
!  Accumulate GDD from emergence (GDDE) and days after emergence (DAE)
!  once it has occurred, but don't include the day of emergence in the
!  dae emergence count:
	if ((ems(1) .ne. 999) .and. (ems(1) .ne. daynum)) then
	   gdde = gdde + gddday
	   dae = dae + 1
	endif
	  
! Once seedling emergence has occurred, call the rest of the subroutines
! and increment dap, dav, and daa:

      if (ems(1) .ne. 999) then 
  
!  Call the leafno subroutine to calculate the number of leaves once
!  emergence has occurred for the leaf number output table.
        call leafno (daynum, gdde, lnarray, lncntr, lnpout,  
     c               pchron, rowcntr, todayln, yestln )

! Calculate growing degree-days (GDDA) and # of days (DAA) after anthesis:
        if ((antss(1) .ne. 999) .and. (antss(1) .ne. daynum)) then
            gdda = gdda + gddday
		  	daa = daa + 1
	  endif

! moved canopy height below updating of antss so that the current days value
! is input to canopy height.	
!  Call canopy height subroutine  (DE added)
        call canopyht(antss, canht, cname, cots, dummy2, ems, 
     c                gddday, gdde, ies, joints, lf4s, maxht)

! moved the call to phenol after other items above are updated to pass into
! phenol. debe added dry bean variables.
        call phenol(aepa, aifs, antes, antss, blstrs, boots, browns, 
     c   cname, cots, dae, dap, dav, daynum, ddae, ddap, ddav, dents, 
     c   dgdde, dgdds, dgddv, doughs, drs, dummy2, ears, endlgs, epods, 
     c   eseeds, first7, fps, fullbs, gdde, gdds, gddv, gpds, halfbs, 
     c   heads, hrs, ies, ies2, infls, joints, lf1s, lf12s, lf2s, lf3s, 
     c   lf4s, lf8s, mats, mffls, milks, mpods, mseeds, nolvs, opens, 
     c   pchron, silks, srs, tis, tsints, tss, year, yelows)

	endif	 

!  Go back and read the next day's weather. When harvest ripe has been
!  reached, then finish program:

 540  continue

! If harvest ripe has not been reached, go back and read in next day's
! weather:

	if (hrs(1) .eq. 999) go to 95
 
 640  continue

 101  continue

!  Call the output file when simulation is done:
!debe added dry bean variables.      
      call output(aifs, antes, antss, blstrs, boots, browns, 
     c canht, cname, cots, daa, dae, dap, dav, ddae, ddap, ddav, dents, 
     c dgdde, dgdds, dgddv, doughs, drs, ears, ems, endlgs, epods, 
     c eseeds, fps, fullbs, gdda, gdde, gdds, gddv, gpds, halfbs, heads,
     c hrs, ies, ies2, infls, joints, lf1s, lf12s, lf2s, lf3s, lf4s, 
     c lf8s, lnarray, lnpout, mats, milks, mffls, mpods, mseeds, nolvs, 
     c opens, outf, pchron, pdate, pyear, silks, srs, tis, tsints, tss,
     c year, yelows)

      end program PhenologyMMS
