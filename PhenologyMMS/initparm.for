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
 
	
      subroutine initparm(cname, day, elrate, germd, gmethod, hemisp, 
     c latitude, maxht, mo, noseeds, pdate, pdepth, pequation, pmethod, 
     c seedsw, swtype, tbase, toptlo, toptup, tupper, vname, weather, 
     c wlow, wup, year)
     
      implicit none
      
      integer  day, gmethod, mo, noseeds, pdate, pdepth, 
     c pmethod, seedsw, year 
          
      real  elrate, germd, latitude, maxht, tbase, toptlo, toptup, 
     c tupper, wlow, wup
      
      character *22  cname, hemisp, pequation, swtype, vname, 
     c weather

! Initialize variables   
!  Character Strings
	cname = ""
	hemisp = "north"
	pequation = ""
      swtype = ""
      vname = ""
      weather = ""
           
!  Emergence      
      elrate = 0.0
      germd = 0.0
      wlow = 0.0
      wup = 0.0
      
!  General Input Parameters
      day = 0
      gmethod = 0
      latitude = 0.0
      maxht = 0.0
      mo = 0
      noseeds = 0
      pdate = 0
      pdepth = 0
      pmethod = 0
      seedsw = 0
      tbase = 0.0
      toptlo = 0.0
      toptup = 0.0
      tupper = 0.0
      year = 0000

      return
      end