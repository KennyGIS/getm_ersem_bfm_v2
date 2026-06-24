#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: interface_velocities - calculate interface velocities.
!
! !INTERFACE:
   subroutine interface_velocities(U,V,DU,DV,velu,velv)

!  Note (KK): keep in sync with interface in m2d.F90
!
! !DESCRIPTION:
!
! !USES:
   use domain
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE,dimension(E2DFIELD),intent(in)  :: U,V,DU,DV
!
! !OUTPUT PARAMETERS:
   REALTYPE,dimension(E2DFIELD),intent(out) :: velu,velv
!
! !REVISION HISTORY:
!  Original author(s): Knut Klingbeil
!
! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'interface_velocities() # ',Ncall
#endif

   where (au(_U2DFIELD_) .gt. 0) velu(_U2DFIELD_) = U(_U2DFIELD_)/DU(_U2DFIELD_)
   where (av(_V2DFIELD_) .gt. 0) velv(_V2DFIELD_) = V(_V2DFIELD_)/DV(_V2DFIELD_)

#ifdef DEBUG
   write(debug,*) 'Leaving interface_velocities()'
   write(debug,*)
#endif
   return
   end subroutine interface_velocities
!EOC

!-----------------------------------------------------------------------
! Copyright (C) 2020 - Knut Klingbeil (IOW)                            !
!-----------------------------------------------------------------------
