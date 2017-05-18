!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   P H Y L L O                  *
!  *                                                      gm   1/21/05 *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
!  The PHYLLO subroutine calculates the phyllochron (i.e., rate of leaf
!  appearance).  The user can either use a set phyllochron or it can use
!  an equation.

! Following is from SHOOTGRO:
c  Calculate phyllochron for each seedling cohort according to
c  Baker et al. (1980) in Plant, Cell, and Environment 3:285-7.  Pchron
c  is a function of change in day length at emergence.
c  Errors in day length published in Figure 3 were corrected to give a 
c  slope of 0.026, a y-intercept of 0.0104, and r**2 of 0.95.  Day 
c  length values were derived from WGEN (used in function DAYLTH) and
c  latitude of 51.50.


!  NOTE:  Equations still need to be added.  For now, values are set in 
!         setup.for

!  INPUTS:  list variables here with C and R for each variable           

!  OUTPUTS: list variables here with C and R for each variable

      subroutine phyllo(chg)

      implicit none

      real chg, phyllonw

      phyllonw = 1.0 / (0.026 * chg + 0.0104)
      
!      print *, 'phyllonw = ', phyllonw 
     
      return
      end
