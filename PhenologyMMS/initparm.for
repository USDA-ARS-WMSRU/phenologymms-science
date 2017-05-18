!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   I N I T P A R M              *
!  *                                                      de   1/03/06 *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
!  The INITPARM subroutine initializes variables that are parameters.


!  INPUTS:  cname(C), day(C), elrate(C), germd(C), gmethod(C), hemisp(C), 
!           latitude(C), maxht(C), mo(C), noseeds(C), pdate(C), 
!           pdepth(C), pequation(C), pmethod(C), seedsw(C), swtype(C),
!           tbase(C), topt(C), tupper(C), vname(C), weather(C), wlow(C),
!           wup(C), year(C)
            
 

!  OUTPUTS: cname(C), day(C), elrate(C), germd(C), gmethod(C), hemisp(C), 
!           latitude(C), maxht(C), mo(C), noseeds(C), pdate(C), 
!           pdepth(C), pequation(C), pmethod(C), seedsw(C), swtype(C),
!           tbase(C), topt(C), tupper(C), vname(C), weather(C), wlow(C),
!           wup(C), year(C)  
 
    ! subroutine initparm(cname, day, ecanht, elrate, germd, gmethod,  
    !c hemisp, latitude, maxht, mo, noseeds, pdate, pdepth, pequation, 
    !c planted, pmethod, seedsw, swtype, tbase, toptlo, toptup, tupper, 
    !c vname, weather, wlow, wup, year)
      subroutine initparm(cname, day, ecanht, elrate, errorarr, germd,  
     c gmethod, hemisp, latitude, maxht, mo, noseeds, pdate, pdepth,  
     c pequation, planted, pmethod, seedsw, swtype, tbase, toptlo, 
     c toptup, tupper, vname, weather, wlow, wup, year)
      implicit none

!debe added i, j as counter variables in For loop to initialize errorarray
      integer  day, gmethod, i, j, mo, noseeds, pdate, pmethod, 
     c          seedsw, year  
      
      real  ecanht, elrate, germd, latitude, maxht, pdepth, 
     c tbase, toptlo, toptup, tupper, wlow, wup
!debe 020309 moved pdepth to real from integer

      character *22  cname, hemisp, pequation, swtype, vname 
      
      character *110 weather

!debe added the logical variable to be initialized to FALSE. It is to 
  !detect if the planting date was reached.
      logical planted
      
!debe added the errorarr array to hold error numbers and messages to print in phenol.out      
      CHARACTER,DIMENSION (20,20) ::errorarr
      
! Initialize variables   
!  Character Strings
	cname = ""
	hemisp = "north"
	pequation = ""
      swtype = ""
      vname = ""
      weather = ""
      
      !trying to initialize the errorarr to empty strings      
!  Character String Array  
      do 15 i = 1, 20
        do 25 j = 1, 20
	  errorarr(i,j) = ""
   25     continue
 15   continue     
     
!  Emergence      
      elrate = 0.0
      germd = 0.0
      wlow = 0.0
      wup = 0.0
      
!  General Input Parameters
      day = 0
      ecanht = 0.0
      gmethod = 0
      latitude = 0.0
      maxht = 0.0
      mo = 0
      noseeds = 0
      pdate = 0
      pdepth = 0.0
      pmethod = 0
      seedsw = 0
      tbase = 0.0
      toptlo = 0.0
      toptup = 0.0
      tupper = 0.0
      year = 0000

  ! logical variable
      planted = .FALSE.
      
      return
      end