!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   C A N O P Y H T              *
!  *                                                      gm   1/29/05 *
!  *                                                      de   7/14/05 *
!  *                                                                   *
!  *********************************************************************
!  The CANOPYHT subroutine calculates the height of the canopy for different crops.
!The gdds1 and gdds2 values are used in calculating the growth for both phases of 
! canopy growth. These values are set in the setup subroutine and are based on the 
! values in various positions in the dummy2 array. These two values are used in 
! calculating the growth rate which is then multiplied by the gdd for the day and that 
! amount is added to the canopy height for that phase. 

!  INPUTS:  antss(R), canht(R), cname(R), dummy2(R), ems(R), gddday(R), 
!           gdde(R), joints(R), maxht(R)           

!  OUTPUTS: canht(C,R), 

      SUBROUTINE canopyht(antss,canht,ecanht,ems,gddday,gdde,gdds1,
     c                    gdds2,stemelong,maxht) 

    !debe added a generic stage - stemelong to use for the middle stage because 
    ! the name for this stage varies among crops. Hay millet, proso millet, spring 
    ! wheat and winter wheat use tss, corn and sorghum use ies, spring and winter 
    ! barley use aifs, dry beans use cots and sunflower uses lf4s.
    !debe added ecanht so that it can be read in instead of set in the code for each crop.
    !debe changed setting the value of gdds1 and gdds2 in Setup instead of here. This way, 
    ! these values are caluculated only once instead of every day.
    
      IMPLICIT NONE

        ! Subroutine arguments
      REAL :: canht,ecanht,gddday,gdde,gdds1,gdds2,maxht
!     CHARACTER(22) :: cname
      INTEGER,DIMENSION(4) :: antss,ems
!    ! REAL,DIMENSION(30) :: dummy2

        ! Local Variables
!     ! INTEGER :: i
      INTEGER,DIMENSION(4) :: stemelong
      REAL :: hrate1,hrate2

        !     + + + argument definitions + + +
        !     antss - start of anthesis growth stage. this array includes daynum,
        !             year, month and day of when this stage was reached.
        !     canht - the height of the plant canopy.
        !     cname - name of the crop.
        !     dayhtinc - the increase in plant height for today.
        !     dummy2 - an array to hold the gdd values, both under stressed
        !              and non- stressed conditions,required to reach each growth
        !              stage of the current crop.
        !     ecanht - this is the maximum canopy height of the crop in phase 1 of 
        !              the canopy height growth.  This is usually from emergence to 
        !              when the plant begins elongating stems but this stage varies 
        !              among crops. it is an input parameter and is read in from upgm_crop.dat.
        !     ems - day when emergence occurred in all crops. this array includes
        !           daynum, year, month and day of when this event occurred.
        !     gddday - the number of gdd with 0°c base temperature for that day.
        !     gdde - accumulation of growing degree days since emergence.
        !     gdds1 - this is the amount of gdd needed to set the end of phase one growth in 
        !             canopy height. It is used in calculating the growth in phase one of 
        !             canopy height growth.
        !     gdds2 - this is used in calculating the growth in canopy height in phase two.
        !     maxht - the maximum height of the plant canopy.
        !     stemelong - is a generic stage that holds the middle stage passed in by a crop. 
        !                 This stage is variable in name among crops so in canopyht, 
        !                 stemelong holds the stage that is the ending of phase 1 canopy 
        !                 growth and the beginning of phase2 canopy growth.
 

!debe made canopyht more generic so it can be used for all crops. Individual subroutines
! will not be needed for each crop.

! Phase 1 - emergence to 'stemelong'
      IF (ems(1).NE.999.AND.gdde.LT.gdds1) THEN
        !  calculate the growth rate for phase 1
           hrate1 = ecanht/gdds1
           canht = canht + (hrate1 * gddday)
!  don't allow canopy to be greater than maximum canopy height for emergence growth stage.
           IF (canht.GT.ecanht) canht = ecanht
! Phase 2 - from 'stemelong' to the start of anthesis
! add the growth stage's gdd values for this interval
      ELSE IF (stemelong(1).NE.999.AND.antss(1).EQ.999) THEN
        !  calculate the growth rate for this phase of canopy ht. growth
           hrate2 = (maxht-ecanht)/gdds2
           canht = canht + (hrate2 * gddday)
        !  don't let canopy height exceed maximum potential height:
          IF (canht.GT.maxht) canht = maxht
      END IF
        ! 
      END SUBROUTINE canopyht
