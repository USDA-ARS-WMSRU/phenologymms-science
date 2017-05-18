!  *********************************************************************
!  *                                                                   *
!  *                S U B R O U T I N E   R A I C A L C                *
!  *                                                      gm   1/21/05 *
!  *                                                                   *
!  *                                                                   *
!  *********************************************************************
! Subroutine RAICALC:  This subroutine calls subroutines to calculate
! different stresses, or to input different stress values.  RAI is the
! the multiplication of different 0 to 1 stress values for water, N, and
! whatever else is desired.

!  INPUTS:  nai(C,R), rai(C), wai(C,R)           

!  OUTPUTS: rai(C)

      subroutine raicalc(nai, wai)

      implicit none

      real nai, rai, wai

!  Initially set wai (water stress) and nai (N stress) values to 1 (= no
!  stress).

! Call water stress calculator:

!     call waistress

      wai = 1.0

! Call N stress calculator:

!     call naistress

      nai = 1.0

! calculate the total resource availability index (RAI):
      
      rai = wai * nai

      return
      end
