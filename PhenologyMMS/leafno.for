!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   L E A F N O                  *
!  *                                                      de   2/05/08 *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
!  The LEAFNO subroutine calculates the number of leaves per day.
!  Variable definitions:
!     daynum = the integer version of day of year
!     gdde = the accumulated growing degree-days since emergence
!     lnarray = an array to hold the leaf number calculated for each day
!     lncntr = counter for the leafno subroutine
!     lnpout = an array to hold the leaf number when it changes to the 
!       next whole number, i.e. the interger version of the real leaf number
!     pchron = the number of growing degree-days required to produce another leaf
!     rowcntr = a counter for the rows in an array
!     todayln = the value of the current day's leaf number
!     yestln = the value of yesterday's leaf number

      subroutine leafno(daynum, gdde, lnarray, lncntr, lnpout, pchron,
     c  rowcntr, todayln, yestln)
      
      implicit none
      
      integer daynum, lncntr, rowcntr
      
      real gdde, lnarray(400,2), lnpout(60,2), pchron, todayln, 
     c yestln  
      
! Local variables
      integer  col, nextcol  
      real doy, ln
      
      
! Initialize variables
      doy = daynum ! day of year.  doy is real and daynum is integer
      ln = 0.0     ! leaf number
      col = 1      ! column number one
      nextcol = 2  ! column number two
      
                
! Fill arrays 
!         print *, 'in leafno and lncntr = ', lncntr, 'daynum = ', daynum   
        ln = gdde/pchron  ! Calculate leaf number for current day
        lncntr = lncntr + 1 ! increment counter
        lnarray (lncntr,col) = daynum ! Fill leaf number array 
        lnarray (lncntr,nextcol) = ln  
        
 !Set todayln to the value in the lnarray for today.       
        todayln = lnarray(lncntr, nextcol)
                     
      ! fill leaf output array when the integer version of yesterday's  
      ! leaf number is less than the integer version of today's leaf 
      ! number
        if ((int(yestln)) .lt. (int(todayln))) then
              rowcntr = rowcntr + 1 ! increment the row counter
              lnpout(rowcntr,col) = doy
              lnpout(rowcntr, nextcol) = ln
        end if
        yestln = lnarray(lncntr, nextcol)    
      
      return
      end
      
