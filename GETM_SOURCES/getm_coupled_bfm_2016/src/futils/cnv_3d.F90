#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: cnv_3d() - convert 3D-scalar fields to real*4.
!
! !INTERFACE:
   subroutine cnv_3d(imin,jmin,imax,jmax,kmin,kmax,mask,var,missing, &
                     il,ih,jl,jh,kl,kh,ws)
   IMPLICIT NONE
!
! !DESCRIPTION:
!
! !USES:
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: imin,jmin,imax,jmax
   integer, intent(in)                 :: kmin(I2DFIELD)
   integer, intent(in)                 :: kmax
   integer, intent(in)                 :: mask(E2DFIELD)
   REALTYPE, intent(in)                :: var(I3DFIELD)
   REALTYPE, intent(in)                :: missing
   integer, intent(in)                 :: il,jl,ih,jh,kl,kh
!
! !OUTPUT PARAMETERS:
   REALTYPE, intent(out)               :: ws(I3DFIELD)
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
! !LOCAL VARIABLES:
   integer                   :: i,j,k
#ifdef BFM_GOTM
   REAL(kind=4)              :: short
   REALTYPE                  :: r
   REALTYPE,parameter        :: max_value=2.0D+00**(maxexponent(short)-1)
   REALTYPE,parameter        :: min_value=2.0D+00**(minexponent(short)+1)
#endif
!EOP
!-----------------------------------------------------------------------
!BOC
   do k=kl,kh
      do j=jl,jh
         do i=il,ih
            if (mask(i,j) .gt. 0 .and. k .ge. kmin(i,j) ) then
               ws(i,j,k) = var(i,j,k)
#ifdef BFM_GOTM
! account for finite range real*4
               r=var(i,j,k)
               if (abs(r).lt.min_value) r=_ZERO_
               if (abs(r).gt.max_value) r=sign(max_value,r)
               ws(i,j,k) =r
#endif
            else
               ws(i,j,k) = missing
            end if
         end do
      end do
   end do
   return
   end subroutine cnv_3d
!EOC

!-----------------------------------------------------------------------
! Copyright (C) 2001 - Hans Burchard and Karsten Bolding (BBH)         !
!-----------------------------------------------------------------------
