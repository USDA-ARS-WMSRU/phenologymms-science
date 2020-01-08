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

      integer  aifs(4), antes(4), antss(4), blstrs(4), bmats(4), 
     c boots(4), browns(4), cots(4), daa, dae, dap, dav, day, daynum, 
     c ddae(20), ddap(20), ddav(20), dents(4), doughs(4), drs(4), 
     c ears(4), ems(4), endlgs(4), epods(4), eseeds(4), first1, first2, 
     c first7, fps(4), fullbs(4), germs(4), gmethod, gpds(4), halfbs(4),
     c heads(4), hrs(4), idx, ies(4), ies2(4), infls(4), joints(4), 
     c lf1s(4), lf12s(4), lf2s(4), lf3s(4), lf4s(4), lf5s(4), lf8s(4), 
     c lncntr, mats(4), mffls(4), milks(4), mo, mpods(4), mseeds(4),  
     c pmo, mon, nai, noseeds, opens(4), pdate, pday, pmethod, rai,  
     c rowcntr, seedsw, silks(4), srs(4), tis(4), tsints(4), tss(4), 
     c uuyear, verns, wai, pyear, year, yelows(4) !tempsw,  
! debe added variables for dry beans. 
! Oct, 2017 - added 2 new variables for soybeans, used dry bean 
! variables for the other soybean stages.
! debe added uuday, uumonth, uuyear for the day, month and year variables 
! that are read in from the climate file (needed in the UPGM model) but not
! used in PhenologyMMS. The variable idx is added to enable reading the 
! header information from the climate file. idx is the counter for the three 
! arrays that will hold the tmax, tmin and precip averages.
! debe added gdds1 and gdds2 to be initialized in initparm and passed on to 
! canopyht rather than calculating the same values each day of the run. 6/9/11
! debe added tempsw for emerge subroutine. It is initialized to the value read in 
! for seedsw in Setup. 
! debe added variables for photoperiod subroutine; declare new function dayhours Nov. 2017
      real  aepa,  aveprecip(12), avetmax(12), avetmin(12), canht, 
     c civilrise, cumvd, dayhours, dayhrs, daylen, daylth, degtorad,  
     c devern, df, dgdde(20), dgdds(20), dgddv(20), dummy2(16), ecanht, 
     c elong, elrate, ergdd(4), gdda, gddday, gdde, gdds, gdds1, gdds2, 
     c gddtbg, gddv, germd, germgdd(4), hrlt, latitude, lnarray(600,2),
     c lnpout(100,2), maxht, mg, month, nolvs, p1d, pchron, pdepth, pf, 
     c photocrit, photosen, ppsen, precip, p1v,radtodeg, ri, soil3t, 
     c tavg, tbase, tmax, tmin, todayln, toptlo, toptup, tupper,  
     c vernal, vd, vtbase, vtoptlo, vtoptup,vtupper, uuday, uumonth, 
     c wfpslo(4), wfpsup(4), wlow, wup, yestln !vf, vf0,   
 
      
!debe moved pdepth to real from integer.
!debe added variables for photoperiod: civilrise, hrlt, daylen, df
!debe added the variables for average tmax, tmin and average precip that will 
! be used in reading in the header information from the climate file. These variables
! are aveprecip, avetmax, avetmin

!debe added egdd and ggdd arrays to be initialized in initcepv and then passed to emerge.
        REAL,DIMENSION(6) :: egdd,ggdd
        
!debe added the errorarr array to hold error numbers and messages to print in phenol.out      
        CHARACTER,DIMENSION (20,20) ::errorarr
        
!debe added cliname to enable using the same climate file format as that used for UPGM.
! The header rows will be read in and not used. Cliname is the first header row.         
      character *22  cname, cliname, dummy1(16), hemisp,  
     c pequation, soilwat(4), swtype, vname
       character *256 outf !trying to get the phenol.out to always be overwritten when a new run is made. Perhaps, the file path is too long?
       
! 9/4/14 GM, debe and Mike Herder set the weather variable to a string length of 110 to allow reading
!   of the path name to include the state/region and the weather filename. The weather files are now located
!   in a more detailed folder structure to work with the new interface programmed by Mike H.
      character *110 weather, line
      
!  Logical variable to determine if the planting date was reached. 
!      It is set to FALSE and will change to TRUE when the date is reached.      
      logical planted
               
! Local variables
      INTEGER,SAVE :: tempsw  
      
      INTEGER :: stagestart, stageend
      
! Initialize the variables in several init subroutines.

      call initparm(cname, day, ecanht, elrate, errorarr, germd,  
     c gmethod, hemisp, latitude, maxht, mo, noseeds, pdate, pdepth,  
     c pequation, planted, pmethod, seedsw, swtype, tbase, toptlo, 
     c toptup, tupper, vname, weather, wlow, wup, year)
 
!What is year set to?      
!      print *, 'year = ', year
      
      !debe added dry bean variables to initgs. Added 2 new soybean variables    
      call initgs(aifs, antes, antss, blstrs, bmats, boots, browns, 
     c cots, dents, doughs, drs, dummy1, dummy2, ears, ems, endlgs, 
     c epods, eseeds, first7, fps, fullbs, germs, gpds, halfbs, heads, 
     c hrs, ies, ies2, infls, joints, lf1s, lf12s, lf2s, lf3s, lf4s, 
     c lf5s, lf8s, mats, mffls, milks, mpods, mseeds, nolvs, opens, 
     c silks, srs, tis, tsints, tss, verns, yelows)
     
      call initday(daa, dae, dap, dav, daynum, ddae, ddap, ddav, 
     c dgdde, dgdds, dgddv, first1, first2, gdda, gddday, gdde, gdds, 
     c gddtbg, gddv, lnarray, lncntr, lnpout, mon, month, outf, rowcntr,
     c todayln, yestln)
           
      call initcepv(canht, civilrise, cumvd, dayhrs, degtorad, devern, 
     c df, egdd, elong, ergdd, germgdd, ggdd, hrlt, mg, nai, p1d, p1v,
     c pchron, pf, photocrit, photosen, ppsen, radtodeg, rai,soilwat, 
     c vd, vernal, vtbase, vtoptlo, vtoptup, vtupper, wai, wfpslo, 
     c wfpsup) !vf, vf0, 
     
      call initwthr(aveprecip, avetmax, avetmin, precip, ri, soil3t, 
     c tavg, tmax, tmin, uuday, uumonth, uuyear)
      
!debe added the photoperiod coefficient to be set here:
      p1d = 30.0 
             
!debe added the following vernalization stuff:
!Set p1v and vf0 for vernalization. This doesn't need to be done every day.
!  p1v is the vernalization coefficient and should be read in from a file.
!  For now, hardwire it here. 
      !p1v = 50
! vf0 is the relative development rate when the crop is unvernalized	
!      vf0 = 1-(p1v/50)
!Need to read in vtbase, vtoptlo, vtoptup, vtupper from a file. Read in 
!devern value too. These are constant values for a crop and only need to 
!be read in once.
!For now hardwire these values as follows:
!      vtbase = -5.0
!      vtoptlo = 0.0
!      vtoptup = 15.0
!      vtupper = 30.0
!devern - value above which devernalization occurs
    !  devern = 30.0
! End of initializing routines and variables
            
!  Call SETUP to read in various input files and set model up for running.
!   Start with weather in declaring the variables type up above.
      call setup(cname, devern, dummy1, dummy2, elrate, ecanht, 
     c ergdd, gdds1, gdds2, germd, germgdd, gmethod, latitude, maxht,mg,
     c noseeds, p1v, pchron, pdate, pday, pdepth, pequation, photocrit, 
     c photosen, ppsen, pmethod, pmo, pyear, seedsw, soilwat, swtype,  
     c tbase, tempsw, toptlo, toptup, tupper, vname, vtbase, vtoptlo,   
     c vtoptup, vtupper, weather, wfpslo, wfpsup, wlow, wup)
       
      
! Read from weather file here.  Will need to read in comment lines.
 95   continue 
            
!       open (unit=14, file= weather, status='OLD')
       open (unit=14, file= 'MMSWeather/' // weather, status='OLD')
       
       !debe added
          IF (daynum.eq.0) THEN !first day the climate file is read.
            READ (14,*) cliname
            READ (14,'(A)') line !(avetmax(idx),idx=1,12)
            READ (14,'(A)') line !(avetmin(idx),idx=1,12)
            READ (14,'(A)') line !(aveprecip(idx),idx=1,12)

          ENDIF
          
            read(14,*,end=640,err=101) uuday,uumonth,uuyear, 
     c                        precip,tmax,tmin, ri,year,daynum,soil3t
    
          
! debe uuday, uumonth, uuyear are the 'unused' day, month, and year variables that are read in
! from the climate file that now has the merged format enabling use of the same climate files 
! in more than one model, currently, PhenologyMMS and UPGM.
! de later used these values in the function dayhours. 11/9/17
            
!convert solar radiation to the units need by PhenologyMMS MJ/m^2/day
      if (ri .ne. 999.9) then
        ri = ri / 23.895
      endif
      
      
! Do error checking on input weather variables
!DE made changes below to allow the use of 999.9 for missing data
      if ((year .lt. 0000) .or. (year .gt. 2100)) then
          print *, 'Variable year is out of range in weather file'
            print *, 'year = ', year
      elseif ((daynum .lt. 1) .or. (daynum .gt. 366)) then
          print *, 'Variable daynum is out of range in weather file'
	      print *, 'daynum = ', daynum 
      elseif (((tmax .lt. -50.0) .or. (tmax .gt. 60.0)) .and. 
     c (tmax.ne.999.9)) then
          print *, 'Variable tmax is out of range in weather file'
	      print *, 'daynum = ', daynum
      elseif (((tmin .lt. -50.0) .or. (tmin .gt. 60.0)) .and.
     c (tmin.ne.999.9)) then
          print *, 'Variable tmin is out of range in weather file'
	      print *, 'daynum = ', daynum	      	     
      elseif (((ri .lt. 0.0).or.(ri.gt. 41.7)) .and. (ri.ne.999.9)) then
          print *, 'Variable ri is out of range in weather file'
            print *, 'ri = ', ri
	      print *, 'daynum = ', daynum
      elseif ((precip .lt. 0.0) .or. (precip .gt. 999.9)) then
          print *, 'Variable precip is out of range in weather file'
	      print *, 'daynum = ', daynum
      elseif ((soil3t.ne.999.9).and.((soil3t.lt.-50.0).or.
     .	  (soil3t.gt.60.0))) then
          print *, 'Variable soil3t is out of range in weather file'
            print *, 'daynum = ', daynum
      endif
     
!debe added the following check to determine if the planting date was reached in the
!  chosen weather file/location. It will be checked every day until planting date is reached.
   ! If it is reached, the logical variable 'planted' will be set to TRUE. 
   ! Planted is initialized to FALSE in initparm.
   ! This check is made so that if planting date is not found in the weather file, output.for
   ! can write an error message to the phenol.out to alert the user that the weather file did
   ! possess the planting date value.
      if(planted .eq. .FALSE.) then
         if(pdate .eq. daynum .and. pyear .eq. year) then !planting date is reached
             planted = .TRUE.
!             print*, 'planted = ', planted, 'daynum = ', daynum, 
!     .          'year = ', year 
         endif !ends pdate and pyear check
        if (planted .eq. .FALSE.) then
            errorarr(1,1) = '101'
            errorarr(1,2) = 'The planting date was not reached. 
     .       Choose another location/weather file or change the planting
     .       date.' 
        endif !ends check of value of planted variable   
      endif !ends outer loop  
 
!  If planting date (PDATE) has not been reached, then skip to next day by
!  going to the end of this program:
      !  planting date has not yet occurred
      if(pdate .ne. daynum .and. first1 .eq. 0)then 
       	go to 540
      elseif (pyear .ne. year .and. first1 .eq. 0) then !year is not the planting year.
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
!   civilrise - parameter. solar altitude angle defined as civil twilight.

        ! Should it be pdate or day of year? It is initialized with planting date.
! Other dates are used in UPGM as appropriate. It seems daynum would be
! sufficient so will use it here.
        hrlt = daylen(latitude, daynum, civilrise, degtorad, radtodeg)
!        hrlty = daylen(amalat,pdate-1,civilrise)
        
        ! call photoper to calculate the daylength factor.(wheat)
        call photoper(daynum, df, hrlt, p1d) !hemisp, , vernal, verns)

!       Get the change in daylength first to pass to phyllo
        call daylenth(daylth, daynum, latitude)!is this still needed?   
        call phyllo(daylth)!is this still needed?		  
        
! debe added daylength and photoperiod calculation functions from Simon van Donk,
! which are based on DSSAT.
        ! use the function dayhours to set the daylength value in dayhrs
        ! use the day and month read in from the climate file and previously not used
        ! in PhenologyMMS. Year is read in from the climate file and is the calendar year
        dayhrs = dayhours(latitude, uuday, uumonth, year)
        ! print *, 'dayhrs = ', dayhrs
        
        ! call new photoperiod subroutine (soybeans)
        !call photoperiod(dayhrs, pf, photocrit, photosen, ppsen)
        ! print *, 'before call to photoperiod and photocrit = ',photocrit
        call photoperiod(dayhrs, pf, photocrit, photosen, ppsen)

!debe added the vernaliz call and changes     
! Call vernaliz to calculate the vernalization factor. 
! When vd is equal to or greater than 40.0 for winter wheat, don't call 
! vernaliz any more.
! debe added (pdate .GE. 244) to avoid calling vernaliz for spring crops        
        if(vd .LT. p1v .and. pdate .GE. 244) then  
          call vernaliz(cumvd, daynum, devern, hemisp, tmax, tmin, vd, 
     c    vtbase, vtoptlo, vtoptup, vtupper) !vernal, verns, 
          vd = vd + cumvd  
!          print *, 'called vernaliz'
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
! debe added canme to call to gddcalc so that photoperiod factor
! can be applied to the gddday value if the crop is soybean.
        call gddcalc(cname, daynum, df, gddday, gmethod, pf, tbase,
     c               tmax, tmin, toptlo, toptup, tupper) !, vf)
        
!!  Call vernalization subroutine if vernalization requirement has not
!!  been satisfied:
!           if (verns .eq. 999) 
!     c          call vernaliz(daynum, hemisp, vernal, verns)
       
! Calculate growing degree-days (GDDV) and number of days after
! vernalization requirement was satisfied (DAV) for all crops.
   !   print*,'before first if vernal =', vernal
        if ((vd .ge. p1v) .and. (vernal .eq. 0)) then
            verns = daynum
            vernal = 1
  !      print *, 'in first if verns = ', verns
        endif
        
        if ((vd .ge. p1v) .and. (verns .ne. daynum)) then 
	      gddv = gddv + gddday !this is happening too early for single ridge in winter wheat
		    dav = dav + 1
!        print *, 'in PhenologyMMS.for and dav = ', dav      
	  endif

!Sum gdd and days after planting:      
!debe - don't allow planting day to count as one day after planting	
	  if ((daynum .GT. pdate) .OR. (year .GT. pyear)) then
	      gdds = gdds + gddday !moved gdds code inside the if statment
	      dap = dap + 1        !to match the way it is done for gdde below
	  endif

!NEW WAY: 
      call emerge(dap, daynum, ddap, dgdds, egdd, elong, ems, ergdd,
     c gddday, gddtbg, germgdd, germs, ggdd, pdepth, precip, tempsw,
     c year) 
!debe added egdd and ggdd being initialized in initcepv and sent to Emerge.

!Is this next if statement needed? first2 is not used.
! The day that 50% seedling emergence has occurred, call PHYLLO:
!!!      if (ems(1) .ne. 999 .and. first2 .eq. 0) then
!!!	      first2 = 1
!!!	endif
	
!  Accumulate GDD from emergence (GDDE) and days after emergence (DAE)
!  once it has occurred, but don't include the day of emergence in the
!  dae emergence count:
        if ((ems(1) .ne. 999) .and. (ems(1) .ne. daynum)) then
            if (trim(cname) .eq. 'Soybean') then
                stagestart = sum(dummy2(2:8))
                stageend = sum(dummy2(2:14)) !go the stage (R7) after the stopping stage (R6) and then subtract 1
                ! print *,'daynum = ', daynum, 'gddday = ', gddday
                if (gdde >= stagestart .and. gdde <= stageend-1) then
                    gddday = gddday*pf
                endif
                ! print *,'daynum = ', daynum, 'gddday = ', gddday
            endif
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

! moved the call to phenol after other items above are updated to pass into
! phenol. debe added dry bean variables. Added 2 new soybean variables.
! DE added 'ems' to pass to phenol to pass to phenolsg.for
        call phenol(aepa, aifs, antes, antss, blstrs, bmats, boots, 
     c   browns, cname, cots, dae, dap, dav, daynum, ddae, ddap, ddav, 
     c   dents, dgdde, dgdds, dgddv, doughs, drs, dummy2, ears, ems,  
     c   endlgs, epods, eseeds, first7, fps, fullbs, gdde, gdds, gddv,  
     c   gpds, halfbs, heads, hrs, ies, ies2, infls, joints, lf1s,  
     c   lf12s, lf2s, lf3s, lf4s, lf5s, lf8s, mats, mffls, milks,  
     c   mpods, mseeds, nolvs, opens, pchron, silks, srs, tis, tsints,  
     c   tss, year, yelows)
     
! moved canopy height below updating of antss so that the current days value
! is input to canopy height.	

!  Call canopy height subroutine  (DE added)
! moved canopy height after call to phenol so that today's values for the stages that are
! passed to canopyht are updated.


!NEW way of calling canopyht
!debe changed to setting value of gdds1 and gdds2 in setup and passing to canopyht.
      IF (antss(1).EQ.999) THEN
        IF ((cname.EQ.'Corn') .OR. (cname.EQ.'Sorghum')) THEN
            CALL canopyht(antss,canht,ecanht,ems,gddday,gdde,gdds1,
     c                    gdds2,ies,maxht)
        ELSE IF ((cname.EQ.'Dry Beans') .OR. (cname.EQ.'Soybean')) THEN
            CALL canopyht(antss,canht,ecanht,ems,gddday,gdde,gdds1,
     c                    gdds2,cots,maxht)
        ELSE IF ((cname.EQ.'Spring Barley').OR.
     c             (cname.EQ.'Winter Barley')) THEN
            CALL canopyht(antss,canht,ecanht,ems,gddday,gdde,gdds1, 
     c                    gdds2,aifs,maxht)
        ELSE IF (cname.EQ.'Sunflower') THEN
            CALL canopyht(antss,canht,ecanht,ems,gddday,gdde,gdds1, 
     c                    gdds2,lf4s,maxht)
        ELSE    
!rest of the crops: hay millet, proso millet, spring wheat, winter wheat
            CALL canopyht(antss,canht,ecanht,ems,gddday,gdde,gdds1, 
     c                    gdds2,tss,maxht)
        ENDIF !end if for cname
      ENDIF !end if for antss
	endif	 

!  Go back and read the next day's weather. When harvest ripe has been
!  reached, then finish program:

 540  continue

! If harvest ripe has not been reached, go back and read in next day's
! weather:

	if (hrs(1) .eq. 999) go to 95
 
 640  continue

 101  continue
        !print *, 'error in weather file daynum = ', daynum, year       

        !  Call the output file when simulation is done:
!debe added dry bean variables. Added 2 new soybean variables.      
!      print*, 'planted = ', planted, 'before the call to output'
      
! year value just before call to output      
!      print *, 'year just before call to output = ', year 
      
      call output(aifs, antes, antss, blstrs, bmats, boots, browns, 
     c canht, cname, cots, daa, dae, dap, dav, ddae, ddap, ddav, dents, 
     c dgdde, dgdds, dgddv, doughs, drs, ears, ems, endlgs, epods, 
     c eseeds, fps, fullbs, gdda, gdde, gdds, gddv, gpds, halfbs, heads,
     c hrs, ies, ies2, infls, joints, lf1s, lf12s, lf2s, lf3s, lf4s, 
     c lf5s, lf8s, lnarray, lnpout, mats, milks, mffls, mpods, mseeds, 
     c nolvs, opens, outf, pchron, pdate, planted, pyear, silks, 
     c srs, tis, tsints, tss, weather, year, yelows)  
!      soilwat, 
!DE added weather to be passed to output.for for sorghum probability runs. 
      
      end program PhenologyMMS
