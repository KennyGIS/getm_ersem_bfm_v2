#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: depth_update - adjust the depth to new elevations.
!
! !INTERFACE:
   subroutine depth_update(zo,z,D,Dvel,DU,DV,loop)

!  Note (KK): keep in sync with interface in m2d.F90
!
! !DESCRIPTION:
!
! This routine which is called at every micro time step updates all
! necessary depth related information. These are the water depths in the
! T-, U- and V-points, {\tt D}, {\tt DU} and {\tt DV}, respectively.
!
! When working with the option {\tt SLICE\_MODEL}, the water depths in the
! V-points are mirrored from $j=2$ to $j=1$ and $j=3$.
!
! !USES:
   use exceptions
   use domain, only: imin,imax,jmin,jmax,H,HU,HV,min_depth
   use domain, only: ioff,joff,az,au,av
   use m2d, only: depth_check
   use halo_zones, only: U_TAG,V_TAG
   use getm_timers,  only: tic, toc, TIM_DPTHUPDATE
!$ use omp_lib
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE,dimension(E2DFIELD),intent(in)  :: zo,z
   integer,intent(in),optional              :: loop
!
! !OUTPUT PARAMETERS:
   REALTYPE,dimension(E2DFIELD),intent(out) :: D,Dvel,DU,DV
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard & Karsten Bolding
!
! !LOCAL VARIABLES:
   integer                   :: i,j
   REALTYPE,dimension(E2DFIELD) :: zvel
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'depth_update() # ',Ncall
#endif
   CALL tic(TIM_DPTHUPDATE)

! TODO/BJB: Why is this turned off?
! KK: ...because we need to have non-zero DU/DV at land-sea-interfaces
#undef USE_MASK

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i,j)

!  Depth in elevation points

!$OMP DO SCHEDULE(RUNTIME)
   do j=jmin-HALO,jmax+HALO
      do i=imin-HALO,imax+HALO
         D(i,j) = z(i,j)+H(i,j)
         zvel(i,j) = _HALF_ * ( zo(i,j) + z(i,j) )
         Dvel(i,j) = zvel(i,j) + H(i,j)
      end do
   end do
!$OMP END DO

   if (depth_check.ne.0 .and. present(loop)) then
      if (mod(loop,abs(depth_check)) .eq. 0) then
         do j=jmin,jmax
            do i=imin,imax
               if (az(i,j) .ne. 0) then
                  if (D(i,j) .le. _ZERO_) then
                     STDERR 'non-positive waterdepth',real(D(i,j)),' at global',i+ioff,j+joff
                     if (depth_check .gt. 0) then
                        call getm_error("depth_update()","non-positive waterdepth")
                     end if
                  end if
               end if
            end do
         end do
      end if
   end if

!  U-points
!$OMP DO SCHEDULE(RUNTIME)
   do j=jmin-HALO,jmax+HALO
      do i=imin-HALO,imax+HALO-1
#ifdef USE_MASK
         if(au(i,j) .gt. 0) then
#endif
#ifdef _NEW_DAF_
         DU(i,j) = _HALF_*(zvel(i,j)+zvel(i+1,j)) + HU(i,j)
#else
         DU(i,j) = max( min_depth                                , &
                        _HALF_*(zvel(i,j)+zvel(i+1,j)) + HU(i,j) )
#endif
#ifdef USE_MASK
         end if
#endif
      end do
   end do
!$OMP END DO NOWAIT

!  V-points
!$OMP DO SCHEDULE(RUNTIME)
   do j=jmin-HALO,jmax+HALO-1
      do i=imin-HALO,imax+HALO
#ifdef USE_MASK
         if(av(i,j) .gt. 0) then
#endif
#ifdef _NEW_DAF_
         DV(i,j) = _HALF_*(zvel(i,j)+zvel(i,j+1)) + HV(i,j)
#else
         DV(i,j) = max( min_depth                                , &
                        _HALF_*(zvel(i,j)+zvel(i,j+1)) + HV(i,j) )
#endif
#ifdef USE_MASK
         end if
#endif
      end do
   end do
!$OMP END DO

!$OMP END PARALLEL

#ifdef _MIRROR_BDY_EXTRA_
   call mirror_bdy_2d(DU,U_TAG)
   call mirror_bdy_2d(DV,V_TAG)
#endif

#ifdef SLICE_MODEL
   j = jmax/2
   DV(:,j-1) = DV(:,j)
   DV(:,j+1) = DV(:,j)
#endif

#ifdef DEBUG
   do j=jmin,jmax
      do i=imin,imax

         if(D(i,j) .le. _ZERO_ .and. az(i,j) .gt. 0) then
            STDERR 'depth_update: D  ',i,j,H(i,j),D(i,j)
         end if

         if(DU(i,j) .le. _ZERO_ .and. au(i,j) .gt. 0) then
            STDERR 'depth_update: DU ',i,j,HU(i,j),DU(i,j)
         end if

         if(DV(i,j) .le. _ZERO_ .and. av(i,j) .gt. 0) then
            STDERR 'depth_update: DV ',i,j,HV(i,j),DV(i,j)
         end if

      end do
   end do
#endif

   CALL toc(TIM_DPTHUPDATE)
#ifdef DEBUG
   write(debug,*) 'Leaving depth_update()'
   write(debug,*)
#endif
   return
   end subroutine depth_update
!EOC

!-----------------------------------------------------------------------
! Copyright (C) 2001 - Hans Burchard and Karsten Bolding               !
!-----------------------------------------------------------------------
