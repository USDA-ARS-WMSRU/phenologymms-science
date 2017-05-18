!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   P H O T O P E R              *
!  *                                                      gm   1/31/05 *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
!  The PHOTOPER subroutine 
! the equation to calculate the daylength factor which will be used to 
! affect development rate comes from the publication by 
! 'McMaster et al., 2008, Simulating the influence of vernalization, 
! photoperiod and optimum temperature on wheat development rates.'

!  INPUTS:  daynum(R), hemisp(R), vernal(C), verns(C)          

!  OUTPUTS: vernal(C), verns(C)

      subroutine photoper(daynum, df, pp, p1d) !,hemisp, vernal, verns)
!hrlt is passed from PhenologyMMS into pp. 
      implicit none

      integer  daynum !, verns
      
      real  df, p1d !, vernal   
      
    !  character *22  hemisp
      
!     Local Variables
      real pp
      ! df = daylength factor 
      
! Calculate the day length factor 
!     p1d is the photoperiod coefficient and is the sensitivity 
!     of a cultivar to photoperiod. It is a DSSAT parameter.
!     It should be read in from a file but for now set it here.
!     Perhaps, photoperiod and vernalization coefficients could 
!     be in a text file with values for each crop/cultivar and read in
!     like emerge.txt. These values, based on the crop/cultivar, could 
!     then be written to tinputs. But are the cultivars known before 
!     tinputs is written to? The coefficients file will have to be 
!     searched with the crop/cultivar names in order to get the values 
!     specific to that crop/cultivar and then be written to the 
!     tinputs file. This will be done via the interface code. For now, 
!     pd is initialized in initcepv.for and passed here where it is set
!     to a value.
 
 !debe moved setting of p1d to PhenologyMMS subroutine.
!      p1d = 30.0

! Should '20' be set in a file?
      df = 1-((p1d/10000) * ((20-pp)**2))
 !     print *, 'df = ', df
      
      return
      end