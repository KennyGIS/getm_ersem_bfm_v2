#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: interface_velocities_3d - calculate interface velocities.
!
! !INTERFACE:
   subroutine interface_velocities_3d()
!
! !DESCRIPTION:
!
! !USES:
   use domain      , only: kmax
   use variables_3d, only: uu,vv,uuEuler,vvEuler,hun,hvn,velu3d,velv3d,veluEuler3d,velvEuler3d
   use waves       , only: waveforcing_method,NO_WAVES
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Knut Klingbeil
!
! !LOCAL VARIABLES:
   integer :: k
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'interface_velocities_3d() # ',Ncall
#endif

   do k=1,kmax
      call interface_velocities(uuEuler(:,:,k),vvEuler(:,:,k),hun(:,:,k),hvn(:,:,k),veluEuler3d(:,:,k),velvEuler3d(:,:,k))
   end do

   if (waveforcing_method .ne. NO_WAVES) then
      do k=1,kmax
         call interface_velocities(uu(:,:,k),vv(:,:,k),hun(:,:,k),hvn(:,:,k),velu3d(:,:,k),velv3d(:,:,k))
      end do
   end if

#ifdef DEBUG
   write(debug,*) 'Leaving interface_velocities_3d()'
   write(debug,*)
#endif
   return
   end subroutine interface_velocities_3d
!EOC

!-----------------------------------------------------------------------
! Copyright (C) 2020 - Knut Klingbeil (IOW)                            !
!-----------------------------------------------------------------------
