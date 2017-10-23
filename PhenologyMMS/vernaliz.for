!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   V E R N A L I Z              *
!  *                                                      gm   1/31/05 *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
!  The VERNALIZ subroutine determines whether the vernalization requirement
!  is satisfied for winter crops. When the accumulated vernalization days 
!  (vd) are equal to or greater than 10.0, VERNALIZ will not be called any
!  more.

!  INPUTS:  daynum(R), hemisp(R), vernal(C), verns(C)          

!  OUTPUTS: vernal(C), verns(C)

      subroutine vernaliz(cumvd, daynum, devern, hemisp, tmax,  
     c tmin, vd, vtbase, vtoptlo, vtoptup, vtupper) !vernal, verns, 

      implicit none

      integer  daynum !, verns
      
      real cumvd, devern, tmax, tmin, vd, vtbase, 
     c vtoptlo, vtoptup, vtupper 
      !vernal, 
      
      character *22  hemisp
      
! Local Variables
      real m, tavg, x, x1, x2, x3, x4, y, y1, y2, y3, y4
 
      ! initialize cumvd
      cumvd = 0.0
      !vtbase point coordinates
      x1 = vtbase
      y1 = 0.0
      !vtoplo point ccordinates
      x2 = vtoptlo
      y2 = 1.0 ! maximum development rate
      !vtoptup point coordinates
      x3= vtoptup
      y3 = 1.0
      !vtupper point coordinates
      x4 = vtupper
      y4 = 0.0

      print *, 'in vernaliz and vd = ', vd
      !calculate tavg
      tavg = (tmin + tmax)/2
   !   print *, 'in vernaliz daynum =', daynum, 'tavg =', tavg
!!Need to read in vtbase, vtoptlo, vtoptup, vtupper from a file. These
!! are not necessarily the same values as those for calculating gdd.
!!Read in devern value too.
!!For now hardwire these values as follows:
!      vtbase = -5.0
!      vtoptlo = 0.0
!      vtoptup = 15.0
!      vtupper = 30.0
!!devern - value above which devernalization occurs
!      devern = 30.0


! OLD WAY OF DOING VERNALIZATION:
!! Assume that plant is fully vernalized after 1 Jan (northern hemisphere)
!! or 1 July (southern hemisphere, ignore leap years for now):
!
!!probably don't need hemisp any more???
!      if (hemisp .eq. 'north' .and. daynum .eq. 1) then
!!      if (hemisp .eq. 'north' .and. daynum .eq. 328) then !testing vernalization with UPGM
!! what is vernal for and is it still needed?
!	     vernal = 1.0
!	     verns = daynum
!	     print *, 'verns = ', verns
!
!	elseif (hemisp .eq. 'south' .and. daynum .eq. 182) then
!
!	     vernal = 1.0
!	     verns = daynum
!	     print *, 'verns = ', verns
!
!	endif
! END OF OLD WAY OF DOING VERNALIZATION
	
!Decided to move this section to PhenologyMMS.for p1v and vf0 only need to
!be calculated once.	
!Calculate the vernalization factor
!!  p1v is the vernalization coefficient and should be read in from a file.
!!  For now, hardwire it here. 
!      p1v = 15
!! vf is the relative development rate when the crop is unvernalized	
!      vf0 = 1-(p1v/50)
      
!  Calculate cumvd = the accumulated vernalization-days
! find the slope of the line for the vernalization relationship with daily ave temp
        if ((tavg .GE. vtbase) .AND. (tavg .LT. vtoptlo)) then
           m = (y2-y1)/(x2-x1) !find the slope of the line vtbase - vtoptlo
      !  y-y1 = m(x-x1) ! x2 and y2 could also be used in place of x1 and y1  
           x= tavg
           y = m*x - m*x1 + y1
           cumvd = cumvd + y
        elseif ((tavg .GE. vtoptlo) .AND. 
     c   (tavg .LE. vtoptup)) then
           m = (y3-y2)/(x3-x2)! find the slope of the line vtoptlo-vtoptup
           x = tavg            ! this slope should be 0
           y = m*x - m*x2 + y2
           cumvd = cumvd + y
        elseif ((tavg .GT. vtoptup) .AND. 
     c   (tavg .LE. vtupper)) then
           m = (y4-y3)/(x4-x3)! find the slope of the line vtoptup-vtupper
           x = tavg           
           y = m*x - m*x3 + y3
           cumvd = cumvd + y
! no vernalization when tavg is less than vtbase
        elseif (tavg .LT. vtbase) then
           y = 0.0
           cumvd = cumvd + y
        elseif ((tavg .GT. vtupper) .and. (tavg .LT. devern)) then
           y = 0.0
           cumvd = cumvd + y
! de-vernalization when daily tmax is greater than a set value        
        elseif ((tavg .GT. devern) .and. (vd .LT. 10)) then
           cumvd = cumvd - 1
        endif   
!debe decided to move this code to PhenologyMMS. vf is not used in 
!vernaliz but will be used to affect development rate and needs to be
! available in PhenologyMMS.for.
!!  calculate the vernalization factor vf
!      vf = vf0 + (cumvd/p1v)      
	
   !   print *,'in vernaliz and cumvd = ', cumvd, 'daynum = ', daynum
      return
      end