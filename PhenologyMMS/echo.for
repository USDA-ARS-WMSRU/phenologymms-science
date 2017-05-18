!  ************************************************************************
!  *                                                                      *
!  *                 S U B R O U T I N E   E C H O                        *
!  *                                                                      *
!  *                                                         gm   1/21/05 *
!  *                                                                      *
!  ************************************************************************
!  Used in reading input data files.  Skips any line in a file which has
!  an "=" sign in the first column!

!       variable definitions:
!       variable  i/o   description
!       --------  ---   -----------

!       external references:  file associated with ===> inp

!       called from: main, input

!       programmer: ken rojas

      subroutine ECHO(inp) 

!      implicit double precision (a-h,o-z)

!  Local Variables
      character*80 line
      integer inp

      if (inp .ne. 5) then

10       read (unit=inp,fmt=100,end=9999) line
         if (line(1:1) .eq. '=') go to 10
         backspace (unit=inp)

      endif

9999  return

100   format(a80)

      end