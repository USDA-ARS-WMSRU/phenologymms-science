!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   D A T E 1                    *
!  *                                                      gm   1/21/05 *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
!  The ###### subroutine ... finish description here.

! CHECK INPUTS AND OUTPUTS!!!!!!!!

!  INPUTS:  list variables here with C and R for each variable           

!  OUTPUTS: list variables here with C and R for each variable

      subroutine date1(gsarray)
 
      implicit none
      
      integer gsarray(4), month, ndays(12), ndayyr, year
      
!  Initialize variables
      year = gsarray(2)       
             
! Initialize ndays array to hold the days of the months
      ndays(1) = 31
      ndays(2) = 28
      ndays(3) = 31
      ndays(4) = 30
      ndays(5) = 31
      ndays(6) = 30
      ndays(7) = 31
      ndays(8) = 31
      ndays(9) = 30
      ndays(10) = 31
      ndays(11) = 30
      ndays(12) = 31      
      
!  check if year is a leap year      
      if (MOD(year,4) .EQ. 0) then
          ndays(2) = 29
          ndayyr = 366
      else 
          ndays(2) = 28
          ndayyr = 365
      endif   

 
!     Subtract the numbers of days in each month until the day is within
!     the month.  This will give the day and month.
         gsarray(4) = gsarray(1)
         do 20 month = 1,12
            if (gsarray(4) .le. ndays(month)) go to 21
                gsarray(4) = gsarray(4) - ndays(month) 
                
      
             
 20   continue
 21   continue  
  
      gsarray(3) = month 

100   continue  

      return
      end