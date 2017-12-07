c  *********************************************************************
c  *                                                                   *
c  *               F U N C T I O N       P H O T O P E R I O D         *
c  *                                                     de   11/07/17 *
c  *********************************************************************
c  This function uses the daylength in the dayhours function. It returns
c  a value for the photoperiod for the day. This value is used to
c  affect the thermal time for the day and 
c  subsequently the progress of the plant to the next growth stage.
c  This subroutine is based on the code sent by Simon van Donk which
c  was based on DSSAT: J. W. Jones, G. Hoogenboom, P.W. Wilkens, C.H. 
c  Porter, and G.Y. Tsuji (Editors). 2010. Decision Support System for 
c  Agrotechnology Transfer Version 4.0. Volume 4. DSSAT v4.5: Crop Model
c  Documentation. University of Hawaii, Honolulu, HI.

!  INPUTS:  list variables here with C and R for each variable           

!  OUTPUTS: list variables here with C and R for each variable
      
      
      subroutine photoperiod(dayhrs, pf, photocrit, photosen, ppsen)
      IMPLICIT NONE
      
!     Variable declarations
      REAL :: dayhrs, pf, photocrit, photosen, ppsen 
      
!     Local variables
      REAL :: pcrit
      
      !     + + + purpose + + +
      ! the purpose of this function is to return a photoperiod factor (pf)
      ! that will be used to adjust the Growing Degree-days value for the day 
      ! and thereby the rate of development of the crop.
      
      !     + + + argument definitions + + +
      ! the DSSAT definitions came from the DSSAT documentation noted above.
      ! dayhrs - the hours of daylight, i.e. the daylength calculated in and 
      !            returned from the dayhours.for function
      ! pf - the photoperiod factor used to adjust the development of the crop.
      ! photocrit - this is the CSDL parameter in DSSAT. It is the critical 
      !             photoperiod and is presented in hours.
      ! photosen - this is the R1PPO parameter in DSSAT. It is the sensitivity to 
      !           photoperiod. It is increased (+) or decreased (-) sensitivity to 
      !           photoperiod, expressed as change in critical daylength.
      ! ppsen - this is the PPSEN parameter in DSSAT (1\hour). It is the photoperiod  
      !         sensitivity and is the slope of the relative rate of development 
      !         for daylengths above CSDVAR. CSDVAR is also a DSSAT parameter and
      !         is the critical daylength below which reproductive development 
      !         proceeds unaffected by daylength, and above which development rate
      !         is reduced in proportion to hours above CSDVAR.
      
      !     + + + local variable definitions + + +
      ! pcrit - the critical photoperiod for the day. it is the critical photoperiod (photocrit)
      !         adjusted by the photosen parameter. It is compared to the daylength passed in from
      !         the dayhours.for function and used to set pf.

      ! the photoperiod function
      pcrit = photocrit - photosen
      if(dayhrs .le. pcrit) then
          pf = 1
      elseif(dayhrs .gt. pcrit) then 
          pf = 1 - ppsen*(dayhrs-pcrit)
      endif
      
      if (pf .lt. 0.0) then
          pf = 0.0
      endif
      print *, 'in photoperiod and pf = ', pf
      return
      end !photoperiod



