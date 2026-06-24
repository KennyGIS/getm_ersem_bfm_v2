#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE:  integration - Initialise the time and do the time loop
!
! !INTERFACE:
   module integration
!
! !DESCRIPTION:
!
! !USES:
   IMPLICIT NONE
!
! !PUBLIC DATA MEMBERS:
   integer                             :: MinN=1,MaxN=-1
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: time_loop - the main loop of getm
!
! !INTERFACE:
   subroutine time_loop(runtype)
!
! !DESCRIPTION:
!  A wrapper that calls time_step within a time loop.
!
! !USES:
#ifdef BFM_GOTM
   use variables_bio_3d, only: counter_reset
#endif

   IMPLICIT NONE


!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: runtype
!
! !REVISION HISTORY:
!
! !LOCAL VARIABLES
   integer                   :: n
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'time_loop() # ',Ncall
#endif

#ifdef BFM_GOTM
   counter_reset=.true.
#endif

   STDERR LINE
   LEVEL1 'integrating....'
   STDERR LINE

   do n=MinN,MaxN
      call time_step(runtype,n)
   end do

#ifdef DEBUG
   write(debug,*) 'Leaving time_loop()'
   write(debug,*)
#endif
   return
   end subroutine time_loop
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: time_step - a single time step of getm
!
! !INTERFACE:
   subroutine time_step(runtype,n)
!
! !DESCRIPTION:
!  A wrapper that calls meteo\_forcing, integrate\_2d, integrate\_3d,
!  do\_getm\_bio and output for one time step.
!
! !USES:
   use time,     only: update_time,timestep
   use time,     only: julianday,secondsofday
   use domain,   only: kmax,az,clip_depth
   use meteo,    only: do_meteo,tausx,tausy,airp,swr,albedo
   use meteo,    only: ssu,ssv
   use meteo,    only: fwf_method,evap,precip
   use waves,    only: do_waves,waveforcing_method,NO_WAVES
   use m2d,      only: no_2d,integrate_2d
   use variables_2d, only: fwf,fwf_int,D,Dvel
#ifndef NO_3D
   use m3d,      only: integrate_3d,M
   use variables_3d, only: sseo,ssen,ho,hn
#ifndef NO_BAROCLINIC
   use variables_3d, only: T
#endif
   use rivers,   only: do_rivers
#ifdef _FABM_
   use getm_fabm, only: fabm_calc,do_getm_fabm
#endif
#ifdef GETM_BIO
   use bio, only: bio_calc
   use getm_bio, only: do_getm_bio
#ifdef BFM_GOTM
   use modify_meteo_input,only:do_modify_meteo_input
   use variables_bio_3d, only: counter_reset
   use output, only: step_3d
#endif
#endif
#endif
#ifdef SPM
   use suspended_matter, only: spm_calc,do_spm
#endif
   use input,    only: do_input
   use output_processing, only: do_output_processing
   use output,   only: do_output
#ifdef TEST_NESTING
   use nesting,   only: nesting_file
#endif
   use output_manager
   use getm_timers, only: tic, toc, TIM_FLEX_OUTPUT, TIM_OUTPUT_PROC
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: runtype,n
!
! !REVISION HISTORY:
!
! !LOCAL VARIABLES
   logical                   :: do_3d=.false.
   integer                   :: progress=100
   character(8)              :: d_
   character(10)             :: t_
#ifdef BFM_GOTM
   logical                   :: write_3d=.true.  !JM hardwire for BFM as now obsolete in GETM
#endif
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'time_step() # ',Ncall
#endif

      if (progress .gt. 0 .and. mod(n,progress) .eq. 0) then
         call date_and_time(date=d_,time=t_)
         LEVEL1 t_(1:2),':',t_(3:4),':',t_(5:10),' n=',n
      end if

#ifndef NO_3D
      do_3d = (runtype .ge. 2 .and. mod(n,M) .eq. 0)
#endif
      call do_input(n,do_3d)
#ifdef BFM_GOTM
      call do_modify_meteo_input('modify_meteo.inp')
#endif
      if(runtype .le. 2) then
         call do_meteo(n)
#ifndef NO_3D
#ifndef NO_BAROCLINIC
      else
         call do_meteo(n,T(:,:,kmax))
         swr = swr*(_ONE_-albedo)
#endif
#endif
      end if

      if (waveforcing_method .ne. NO_WAVES) then
         call do_waves(n,Dvel)
      end if

      if (fwf_method .ge. 1) then
         fwf = evap+precip ! positive for incoming
#ifdef _NEW_DAF_
         where (az.eq.1 .and. D.lt.clip_depth) fwf = max( _ZERO_ , fwf )
#endif
#ifndef NO_3D
         fwf_int = fwf_int+timestep*fwf
#endif
      end if

#ifndef NO_BAROTROPIC
      if (.not. no_2d) call integrate_2d(runtype,n,tausx,tausy,airp)
#endif
#ifndef NO_3D
      if (do_3d) then
         sseo = ssen ! true sseo (without rivers and fwf)
         ho   = hn   ! true ho   (without rivers and fwf)
      end if
      call do_rivers(n,do_3d)
      if (do_3d) then
         call integrate_3d(runtype,n)
#ifdef SPM
         if (spm_calc) call do_spm()
#endif
#ifdef _FABM_
         if (fabm_calc) call do_getm_fabm(M*timestep)
#endif
#ifdef GETM_BIO
#ifdef BFM_GOTM
         counter_reset=(mod(n-1,step_3d).eq.0)   !JM needed for average horizontal flux calculations over the output time interval
         if (bio_calc) call do_getm_bio(M*timestep,write_3d)
#else
         if (bio_calc) call do_getm_bio(M*timestep)
#endif
#endif
      end if
#endif

      call set_sea_surface_state(runtype,ssu,ssv,do_3d)

#ifdef TEST_NESTING
      if (mod(n,80) .eq. 0) then
         call nesting_file(WRITING)
      end if
#endif

      call update_time(n)

      call tic(TIM_FLEX_OUTPUT)
      call output_manager_prepare_save(julianday, int(secondsofday), 0, int(n))
      call toc(TIM_FLEX_OUTPUT)
      call tic(TIM_OUTPUT_PROC)
      call do_output_processing()
      call toc(TIM_OUTPUT_PROC)
      call tic(TIM_FLEX_OUTPUT)
      call output_manager_save(julianday,secondsofday,n)
      call toc(TIM_FLEX_OUTPUT)
      call do_output(runtype,n,timestep)
#ifdef DIAGNOSE
      call diagnose(n,MaxN,runtype)
#endif

#ifndef NO_3D
      if (do_3d) then
         if (fwf_method .ge. 1) fwf_int = _ZERO_
      end if
#endif


#ifdef DEBUG
   write(debug,*) 'Leaving time_step()'
   write(debug,*)
#endif
   return
   end subroutine time_step
!EOC

!-----------------------------------------------------------------------

   end module integration

!-----------------------------------------------------------------------
! Copyright (C) 2001 - Hans Burchard and Karsten Bolding               !
!-----------------------------------------------------------------------
