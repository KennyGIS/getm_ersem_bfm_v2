#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: dryfac_update - adjust the drying masks to new elevations.
!
! !INTERFACE:
   subroutine dryfac_update( D, DU, DV, dry_z, dry_u, dry_v)

!  Note (KK): keep in sync with interface in m2d.F90
!
! !DESCRIPTION:
!
! This routine which is called at every micro time step updates 
! the drying value $\alpha$ defined in equation (\ref{alpha})
! on page \pageref{alpha} in the T-, the U- and the V-points
! ({\tt dry\_z}, {\tt dry\_u} and {\tt dry\_v}).
!
! !USES:
   use domain     , only: imin, imax, jmin, jmax
   use domain     , only: az, au, av
   use domain     , only: min_depth, crit_depth
   use getm_timers, only: tic, toc, TIM_DRYFACUPDATE
!$ use omp_lib
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE,dimension(E2DFIELD),intent(in)  :: D, DU, DV
!
! !OUTPUT PARAMETERS:
   REALTYPE,dimension(E2DFIELD),intent(out) :: dry_z, dry_u, dry_v
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!                      Knut Klingbeil
!
! !LOCAL VARIABLES:
   integer                   :: i, j
#ifdef _NEW_DAF_
   REALTYPE                  :: hcritm1
#else
   REALTYPE                  :: d1, d2i
#endif
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'dryfac_update() # ',Ncall
#endif
   CALL tic(TIM_DRYFACUPDATE)

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE( i, j, d1, d2i)

#ifdef _NEW_DAF_

   hcritm1 = _ONE_ / (crit_depth - min_depth)
!$OMP DO SCHEDULE(RUNTIME)
   do j=jmin-HALO,jmax+HALO
      do i=imin-HALO,imax+HALO
         if (az(i,j) .gt. 0) then
            dry_z(i,j)=max(_ZERO_,min(_ONE_,(D(i,j)-min_depth)*hcritm1))
         end if
     end do
  end do
!$OMP END DO
!$OMP DO SCHEDULE(RUNTIME)
   do j=jmin-HALO,jmax+HALO
      do i=imin-HALO,imax+HALO-1
         if (au(i,j) .gt. 0) then
            dry_u(i,j) = min( dry_z(i,j) , dry_z(i+1,j) )
         end if
     end do
  end do
!$OMP END DO NOWAIT
!$OMP DO SCHEDULE(RUNTIME)
   do j=jmin-HALO,jmax+HALO-1
      do i=imin-HALO,imax+HALO
         if (av(i,j) .gt. 0) then
            dry_v(i,j) = min( dry_z(i,j) , dry_z(i,j+1) )
         end if
     end do
  end do
!$OMP END DO

#else

   d1  = 2*min_depth
   d2i = _ONE_/(crit_depth-2*min_depth)
!$OMP DO SCHEDULE(RUNTIME)
   do j=jmin-HALO,jmax+HALO
      do i=imin-HALO,imax+HALO
         if (az(i,j) .gt. 0) then
            dry_z(i,j)=max(_ZERO_,min(_ONE_,(D(i,j)-_HALF_*d1)*d2i))
         end if
         if (au(i,j) .gt. 0) then
            dry_u(i,j) = max(_ZERO_,min(_ONE_,(DU(i,j)-d1)*d2i))
         end if
         if (av(i,j) .gt. 0) then
            dry_v(i,j) = max(_ZERO_,min(_ONE_,(DV(i,j)-d1)*d2i))
         end if
     end do
  end do
!$OMP END DO

#endif

!$OMP END PARALLEL

#ifdef SLICE_MODEL
   j = jmax/2
   dry_v(:,j-1) = dry_v(:,j)
   dry_v(:,j+1) = dry_v(:,j)
#endif


   CALL toc(TIM_DRYFACUPDATE)
#ifdef DEBUG
   write(debug,*) 'Leaving dryfac_update()'
   write(debug,*)
#endif
   return
   end subroutine dryfac_update
!EOC

!-----------------------------------------------------------------------
! Copyright (C) 2001 - Hans Burchard and Karsten Bolding               !
!-----------------------------------------------------------------------
