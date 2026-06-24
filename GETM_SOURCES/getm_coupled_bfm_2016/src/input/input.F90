#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE:  input - input specifications
!
! !INTERFACE:
   module input
!
! !DESCRIPTION:
!
! !USES:
   use meteo, only: met_method,METEO_FROMFILE,meteo_file
   use waves, only: waveforcing_method,WAVES_FROMFILE,waves_file
   use m2d, only: bdy2d
   use bdy_2d, only: bdyfile_2d,bdyfmt_2d
#ifndef NO_3D
   use m3d, only: bdy3d
   use variables_3d, only: sst,sss
   use bdy_3d, only: bdyfile_3d,bdyfmt_3d
   use rivers, only: river_method,nriver,river_data
   !added KLL 01/05/2026 calls new atmdep_input module
   use atmdep_input, only: init_atmdep_input, get_atmdep_data
#endif
   IMPLICIT NONE
!
! !PUBLIC DATA MEMBERS:
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!-----------------------------------------------------------------------

   interface
      subroutine init_2d_bdy(fn,fmt,n)
         character(len=*), intent(in)  :: fn
         integer, intent(in)           :: fmt,n
      end subroutine init_2d_bdy
   end interface

   interface
      subroutine get_2d_bdy(fmt,n)
         integer, intent(in)           :: fmt,n
      end subroutine get_2d_bdy
   end interface

   interface
      subroutine init_3d_bdy(fn,fmt,n)
         character(len=*), intent(in)  :: fn
         integer, intent(in)           :: fmt,n
      end subroutine init_3d_bdy
   end interface

   interface
      subroutine get_3d_bdy(fmt,n)
         integer, intent(in)           :: fmt,n
      end subroutine get_3d_bdy
   end interface

   interface
      subroutine get_2d_field(fn,varname,il,ih,jl,jh,f)
         character(len=*),intent(in)   :: fn,varname
         integer, intent(in)           :: il,ih,jl,jh
         REALTYPE, intent(out)         :: f(:,:)
      end subroutine get_2d_field
   end interface

   interface
      subroutine get_3d_field(fname,var,n,break_on_missing,f)
         character(len=*),intent(in)   :: fname,var
         integer, intent(in)           :: n
         logical, intent(in)           :: break_on_missing
         REALTYPE, intent(out)         :: f
      end subroutine get_3d_field
   end interface

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_input - initialise all external files and units
!
! !INTERFACE:
   subroutine init_input(input_dir,n)
!
! !DESCRIPTION:
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   character(len=*)                    :: input_dir
   integer, intent(in)                 :: n
!
! !REVISION HISTORY:
!  22Nov Author name Initial code
!
!EOP
!-------------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'init_input() # ',Ncall
#endif

   LEVEL1 'init_input'
   if (met_method .eq. METEO_FROMFILE) then
      call init_meteo_input(meteo_file,n)
   end if

   if (waveforcing_method .eq. WAVES_FROMFILE) then
      call init_waves_input(waves_file,n)
   end if

#ifndef NO_3D
   if (river_method .gt. 0 .and. nriver .gt. 0) then
      call init_river_input(river_data,n)
   end if
#endif

!Added by KLL 01/05/2026 
#ifndef NO_3D
   call init_atmdep_input(input_dir,n)
#endif
  
   if(bdy2d) then
      call init_2d_bdy(bdyfile_2d,bdyfmt_2d,n)
   end if

#ifndef NO_3D
   if(bdy3d) then
      call init_3d_bdy(bdyfile_3d,bdyfmt_3d,n)
   end if

   if (allocated(sst) .or. allocated(sss)) then
      call init_ssts_input(trim(input_dir) // 'ssts_files.dat',n)
   end if
#endif

#ifdef DEBUG
   write(debug,*) 'Leaving init_input()'
   write(debug,*)
#endif
   return
   end subroutine init_input
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: do_input - cleans up after run
!
! !INTERFACE:
   subroutine do_input(n,do_3d)
   use getm_timers, only: tic, toc, TIM_INPUT
   IMPLICIT NONE
!
! !DESCRIPTION:
!  To be written
!
! !INPUT PARAMETERS:
   integer, intent(in) :: n
   logical, intent(in) :: do_3d
!
! !REVISION HISTORY:
!
!EOP
!-------------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'do_input() # ',Ncall
#endif
   call tic(TIM_INPUT)

   if(met_method .eq. METEO_FROMFILE) then
      call get_meteo_data(n)
   end if

   if (waveforcing_method .eq. WAVES_FROMFILE) then
      call get_waves_data(n)
   end if

#ifndef NO_3D
   if(river_method .eq. 2) then
      call get_river_data(n)
   end if
#endif

!Added KLL 01/05/2026
#ifndef NO_3D
   call get_atmdep_data(n)
#endif

   if(bdy2d) then
      call get_2d_bdy(bdyfmt_2d,n)
   end if

#ifndef NO_3D
   if (do_3d) then
!     Note (KK): data are assumed to be provided less frequently than 3D time step
!                thus no data set is assumed to be skipped

   if (bdy3d) then
      call get_3d_bdy(bdyfmt_3d,n)
   end if
   if (allocated(sst) .or. allocated(sss)) then
      call get_ssts_data(n)
   end if

   end if
#endif

   call toc(TIM_INPUT)
#ifdef DEBUG
   write(debug,*) 'Leaving do_input()'
   write(debug,*)
#endif
   return
   end subroutine do_input
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: clean_input - cleans up after run
!
! !INTERFACE:
   subroutine clean_input()
   IMPLICIT NONE
!
! !DESCRIPTION:
!  Writes calculated fields to files.
!
! !REVISION HISTORY:
!EOP
!-------------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'clean_input() # ',Ncall
#endif

   LEVEL1 'clean_input'

#ifdef DEBUG
   write(debug,*) 'Leaving clean_io()'
   write(debug,*)
#endif
   return
   end subroutine clean_input
!EOC

!-----------------------------------------------------------------------

   end module input

!-----------------------------------------------------------------------
! Copyright (C) 2001 - Hans Burchard and Karsten Bolding               !
!-----------------------------------------------------------------------
