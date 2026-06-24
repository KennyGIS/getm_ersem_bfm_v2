#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: get_ssts_data - read ssts data from file.
!
! !INTERFACE:
   subroutine get_ssts_data(n)
!
! !DESCRIPTION:
!  Reads specified ssts data from file(s)
!
! !USES:
   use ncdf_ssts, only: get_ssts_data_ncdf
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: n
!
! !INPUT/OUTPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!-------------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   ncall = ncall+1
   write(debug,*) 'get_ssts_data() # ',ncall
#endif

   select case (NETCDF)
      case (ANALYTICAL)
      case (ASCII)
         STDERR 'should get ASCII ssts data'
      case (NETCDF)
         call get_ssts_data_ncdf(n)
      case DEFAULT
         FATAL 'A non valid input format has been chosen'
         stop 'get_ssts_data'
   end select

#ifdef DEBUG
   write(debug,*) 'Leaving get_ssts_data()'
   write(debug,*)
#endif
   return
   end subroutine get_ssts_data
!EOC

!-----------------------------------------------------------------------
! Copyright (C) 2001 - Hans Burchard and Karsten Bolding               !
!-----------------------------------------------------------------------
