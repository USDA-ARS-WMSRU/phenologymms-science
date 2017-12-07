!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   I N I T D A Y                *
!  *                                                      de   2/27/06 *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
!  The INIT subroutine initializes variables relating to day, days after 
!  a specific event and growing degree-days.


!  INPUTS:  daa(C), dae(C), dap(C), dav(C), daynum(C), ddae(C), ddap(C),
!           ddav(C), dgdde(C), dgdds(C), dgddv(C), dummy1(C), dummy2(C),
!           first1(C), first2(C), gdda(C), gddday(C), gdde(C), gdds(C),
!           gddv(C), outf(C)


!  OUTPUTS: daa(C), dae(C), dap(C), dav(C), daynum(C), ddae(C), ddap(C),
!           ddav(C), dgdde(C), dgdds(C), dgddv(C), dummy1(C), dummy2(C),
!           first1(C), first2(C), gdda(C), gddday(C), gdde(C), gdds(C),
!           gddv(C), outf(C)  

	
      subroutine initday(daa, dae, dap, dav, daynum, ddae, ddap, ddav, 
     c dgdde, dgdds, dgddv, first1, first2, gdda, gddday, gdde, gdds, 
     c gddtbg, gddv, lnarray, lncntr, lnpout, mon, month, outf, rowcntr,
     c todayln, yestln)
     

      implicit none
      
      integer  daa, dae, dap, dav, daynum, ddae(20), ddap(20), ddav(20),
     c first1, first2, lncntr, mon, rowcntr
          
      real  dgdde(20), dgdds(20), dgddv(20), gdda, gddday, gdde, gdds, 
     c gddtbg, gddv, lnarray(400, 2), lnpout(100,2), month, todayln, 
     c yestln
      
!      character *22 outf
      character *256 outf

!  Local Variables
      integer i, j

! Initialize variables          
      
!  Arrays of Days After Planting, Emergence, 
!    or Vernalization for growth stages
      do 50 i = 1, 20
        ddap(i) = 999
        ddae(i) = 999
        ddav(i) = 999
!  Arrays of Growing Degree-days from Seeding, Emergence, 
!     or Vernalizaton for growth stages	
        dgdds(i) = 999.9
        dgdde(i) = 999.9
        dgddv(i) = 999.9
        	
 50	continue
 
 !  Arrays for leaf number output table
              
      do 55 i = 1, 400
        do 60 j = 1, 2
            lnarray(i,j) = 0.0
 60   continue
 55   continue     
 
      do 65 i = 1, 100
        do 70 j = 1, 2
            lnpout(i,j) = 0.0
 70   continue
 65   continue
	
!  Growing Degree-Days
      gddday = 0.
      gdds = 0.
      gdde = 0.
	gddv = 0.
	gdda = 0.
	gddtbg = 0. ! growing degree days to begin germination
      
!  Days After
	dap = 0
	dae = 0
	dav = 0
      daa = 0
      
!  Character String
	outf = ""
      
!  Other Variables
      first1 = 0
	first2 = 0

      mon = 1
      month = 1.0
	daynum = 0
	
	lncntr = 0
	rowcntr = 0
	todayln = 0.0
	yestln = 0.0

	
      return
      end