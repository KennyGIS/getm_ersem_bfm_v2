#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: meteo - provides meteorological forcing for \emph{getm}.
!
! !INTERFACE:
   module meteo
!
! !DESCRIPTION:
!  The meteo module provides meteorological forcing for \emph{getm}.
!  The main role of the module is to supply the following 4 fields with
!  sane values - \emph{airp, tausx, tausy, swr} and \emph{shf} - i.e.
!  air pressure [$Pa$], surface stresses [$N/m^2$], short wave radiation
!  [$W/m^2$] and surface heat fluxes [$W/m^2$] on the computational grid.
!  The module provides 3 public functions - \emph{init\_meteo()},
!  \emph{do\_meteo()} and \emph{clean\_meteo()} - and a number of public
!  data members to hold the actual meteorological fields. Also included
!  in the module are various constants related to calculating the
!  meteorological forcing.
!  Information about the calculation domain is obtained from the module
!  \emph{domain} and time related information comes from the module
!  \emph{time}.
!  The meteo module is initialised via a call to \emph{init\_meteo()}
!  that will read a namelist providing all necessary information. Memory
!  allocation is also done in \emph{init\_meteo()}.
!  Obtaining the actual forcing - either by reading from a file or
!  calculating is done via calls to \emph{do\_meteo()}. The actual
!  reading of external data from files is separated completely from
!  \emph{do\_meteo()} and is done in the main time loop via a call to
!  \emph{do\_input()} where all external file input is handled.
!  \emph{meteo} supplies 3 variables which can be used by routines for
!  reading variables. \emph{new\_meteo} is a logical switch which should
!  be set to .true. when new fields have been read. \emph{t\_1} and
!  \emph{t\_2} holds the time (in seconds) since the model run of the two
!  fields surrounding the actual model time - to be used by the temporal
!  interpolation. Finally \emph{clean\_meteo()} should be called when
!  the simulation is over as part of the overall procedure of finalising
!  the model run.
!
! !SEE ALSO:
!  short_wave_radiation.F90, solar_zenith_angle.F90, albedo_water.F90, 
!  fluxes.F90, exchange_coefficients.F90
!
! !USES:
   use time, only: yearday,secondsofday,timestep
   use time, only: write_time_string,timestr
   use halo_zones, only : H_TAG,update_2d_halo,wait_halo,periodic_domain
   use domain
   use exceptions
   IMPLICIT NONE
!
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public                              :: init_meteo, do_meteo, clean_meteo
!
! !PUBLIC DATA MEMBERS:
   character(LEN = PATH_MAX), public   :: meteo_file
   logical, public                     :: metforcing=.false.
   logical, public                     :: on_grid=.true.
   logical, public                     :: calc_met=.false.
   logical, public                     :: interpolate_meteo=.false.
   integer, public, parameter          :: NO_METEO=0
   integer, public, parameter          :: METEO_CONST=1
   integer, public, parameter          :: METEO_FROMFILE=2
   integer, public, parameter          :: METEO_FROMEXT=3
   integer, public                     :: met_method=NO_METEO
   integer, public                     :: back_radiation_method=1
   integer, public                     :: albedo_method=1
   integer, public                     :: fwf_method=0
   REALTYPE, public                    :: wind_factor=_ONE_
   REALTYPE, public                    :: evap_factor = _ONE_
   REALTYPE, public                    :: precip_factor = _ONE_
   logical, public                     :: calc_relative_wind=.false.
   logical, public                     :: constant_cd=.false.
   REALTYPE, public                    :: w,L,rho_air,qs,qa,ea,es
   REALTYPE,public,dimension(:,:),allocatable,target :: airp
   REALTYPE,public,dimension(:,:),pointer            :: airp_new
   REALTYPE,public,dimension(:,:),allocatable,target :: tausx
   REALTYPE,public,dimension(:,:),pointer            :: tausx_input
   REALTYPE,public,dimension(:,:),allocatable,target :: tausy
   REALTYPE,public,dimension(:,:),pointer            :: tausy_input
   REALTYPE,public,dimension(:,:),allocatable,target :: shf
   REALTYPE,public,dimension(:,:),pointer            :: shf_input
   REALTYPE,public,dimension(:,:),allocatable,target :: evap
   REALTYPE,public,dimension(:,:),pointer            :: evap_input
   REALTYPE,public,dimension(:,:),allocatable,target :: u10
   REALTYPE,public,dimension(:,:),pointer            :: u10_new
   REALTYPE,public,dimension(:,:),allocatable,target :: v10
   REALTYPE,public,dimension(:,:),pointer            :: v10_new
   REALTYPE,public,dimension(:,:),allocatable,target :: tcc
   REALTYPE,public,dimension(:,:),pointer            :: tcc_new
   REALTYPE,public,dimension(:,:),allocatable,target :: precip
   REALTYPE,public,dimension(:,:),pointer            :: precip_new
   REALTYPE,public,dimension(:,:),allocatable,target :: t2
   REALTYPE,public,dimension(:,:),allocatable,target :: hum
   REALTYPE,public,dimension(:,:),allocatable,target :: swr
   REALTYPE,public,dimension(:,:),allocatable,target :: wind
   REALTYPE,public,dimension(:,:),pointer            :: u10r,v10r
   REALTYPE,public,dimension(:,:),allocatable,target :: zenith_angle,albedo
   REALTYPE,public,dimension(:,:),allocatable        :: ssu,ssv
#ifdef BFM_GOTM
   REALTYPE, public, dimension(:,:), allocatable  :: dl
#endif
   REALTYPE, public                    :: cd_mom,cd_heat,cd_latent
   REALTYPE, public                    :: cd_precip = _ZERO_
   REALTYPE, public                    :: t_1=-_ONE_,t_2=-_ONE_
   logical, public                     :: new_meteo=.false.
   integer, public                     :: meteo_ramp=0
   integer, public, parameter          :: RELATIVE_HUM=1
   integer, public, parameter          :: WET_BULB=2
   integer, public, parameter          :: DEW_POINT=3
   integer, public, parameter          :: SPECIFIC_HUM=4
   integer, public                     :: hum_method=-1
!
! !DEFINED PARAMETERS:
   REALTYPE,public,parameter           :: cpa=1008.d0  !AS that is not exact !
   REALTYPE,public,parameter           :: KELVIN=273.15d0
   REALTYPE,public,parameter           :: emiss=0.97d0
   REALTYPE,public,parameter           :: bolz=5.67d-8
!  REALTYPE,public,parameter           :: cpa=1004.67 ! specific heat of dry air- correct
   REALTYPE,public,parameter           :: cpw=4192.d0   ! specific heat of sea water
   REALTYPE,public,parameter           :: rho_precip = 1000.0d0
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
! !LOCAL VARIABLES:
   integer                   :: metfmt=2
   integer, save             :: exchange_method = 1
   REALTYPE                  :: tx= _ZERO_ ,ty= _ZERO_
   REALTYPE                  :: albedo_const= _ZERO_
   REALTYPE                  :: swr_const= _ZERO_ ,shf_const= _ZERO_
   REALTYPE                  :: evap_const= _ZERO_ ,precip_const= _ZERO_
   REALTYPE, dimension(:,:), allocatable :: tausx_const,tausy_const
   REALTYPE, dimension(:,:), pointer     :: tausx_new,d_tausx
   REALTYPE, dimension(:,:), pointer     :: tausy_new,d_tausy
   REALTYPE, dimension(:,:), pointer     :: shf_new,d_shf
   REALTYPE, dimension(:,:), pointer     :: evap_new,d_evap
   REALTYPE                              :: ramp=_ONE_
   logical                               :: ramp_is_active=.false.
   logical                               :: calc_evap=.false.
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_meteo - initialise the \emph{meteo} module.
!
! !INTERFACE:
   subroutine init_meteo(runtype,hotstart)
!
! !DESCRIPTION:
!  Basically reads the namelist \emph{meteo} from unit NAMLST.  According to
!  the content of the namelist various variables are allocated and initialised.
!
! !USES:
   use t10m_mod,        only: t10m_init
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: runtype
   logical, intent(in)                 :: hotstart
!
! !REVISION HISTORY:
!
!  See log for module.
!
! !LOCAL VARIABLES:
   integer                   :: rc
   namelist /meteo/ metforcing,on_grid,calc_met,met_method,interpolate_meteo, &
                    exchange_method,back_radiation_method,albedo_method,fwf_method, &
                    meteo_ramp,metfmt,meteo_file, &
                    tx,ty,albedo_const, &
                    swr_const,shf_const,evap_const,precip_const, &
                    wind_factor,precip_factor,evap_factor, &
                    calc_relative_wind
!EOP
!-------------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'init_meteo() # ',Ncall
#endif
   LEVEL1 'init_meteo'

! Allocates memory for the public data members
!  KK-TODO: Why is this not static #ifdef STATIC?

   allocate(airp(E2DFIELD),stat=rc)
   if (rc /= 0) stop 'init_meteo: Error allocating memory (airp)'
   airp = _ZERO_

   allocate(u10(E2DFIELD),stat=rc)
   if (rc /= 0) stop 'init_meteo: Error allocating memory (u10)'
   u10 = _ZERO_
   u10r => u10

   allocate(v10(E2DFIELD),stat=rc)
   if (rc /= 0) stop 'init_meteo: Error allocating memory (v10)'
   v10 = _ZERO_
   v10r => v10

   allocate(wind(E2DFIELD),stat=rc)
   if (rc /= 0) stop 'init_meteo: Error allocating memory (wind)'
   wind = _ZERO_

   allocate(tausx(E2DFIELD),stat=rc)
   if (rc /= 0) stop 'init_meteo: Error allocating memory (tausx)'
   tausx = _ZERO_
   tausx_input => tausx

   allocate(tausy(E2DFIELD),stat=rc)
   if (rc /= 0) stop 'init_meteo: Error allocating memory (tausy)'
   tausy = _ZERO_
   tausy_input => tausy

#ifdef BFM_GOTM
   allocate(dl(E2DFIELD),stat=rc)
   if (rc /= 0) stop 'init_meteo: Error allocating memory (dl)'
   dl = _ZERO_
#endif

   allocate(zenith_angle(E2DFIELD),stat=rc)
   if (rc /= 0) stop 'init_meteo: Error allocating memory (zenith_angle)'
   zenith_angle = _ZERO_

   allocate(swr(E2DFIELD),stat=rc)
   if (rc /= 0) stop 'init_meteo: Error allocating memory (swr)'
   swr = _ZERO_

   allocate(albedo(E2DFIELD),stat=rc)
   if (rc /= 0) stop 'init_meteo: Error allocating memory (albedo)'
   albedo = _ZERO_

   allocate(shf(E2DFIELD),stat=rc)
   if (rc /= 0) stop 'init_meteo: Error allocating memory (shf)'
   shf = _ZERO_
   shf_input => shf

   allocate(evap(E2DFIELD),stat=rc)
   if (rc /= 0) stop 'init_meteo: Error allocating memory (evap)'
   evap = _ZERO_
   evap_input => evap

   allocate(precip(E2DFIELD),stat=rc)
   if (rc /= 0) stop 'init_meteo: Error allocating memory (precip)'
   precip = _ZERO_
   precip_new => precip

   allocate(ssu(E2DFIELD),stat=rc)
   if (rc /= 0) stop 'init_meteo: Error allocating memory (ssu)'
   ssu = _ZERO_

   allocate(ssv(E2DFIELD),stat=rc)
   if (rc /= 0) stop 'init_meteo: Error allocating memory (ssv)'
   ssv = _ZERO_

   read(NAMLST,meteo)

!  KK-TODO: replace metforcing by met_method=0

   LEVEL2 'Metforcing=',metforcing
   if (.not. metforcing) then
      met_method = NO_METEO
      calc_met = .false.
      return
   end if

   select case (met_method)
      case (METEO_CONST)
         LEVEL2 'Constant forcing is used:'
         LEVEL3 'tx     = ',tx
         LEVEL3 'ty     = ',ty
         LEVEL3 'swr    = ',swr_const
         LEVEL3 'shf    = ',shf_const
         if (fwf_method .eq. 1) then
            LEVEL3 'evap   = ',evap_const
            LEVEL3 'precip = ',precip_const
         end if
         albedo_method = 0
         calc_met = .false.
      case (METEO_FROMFILE)
         LEVEL2 'Meteo forcing read from file'
         if(on_grid) then
            LEVEL2 'Meteorological fields are on the computational grid'
         else
            LEVEL2 'Meteorological fields needs to be interpolated'
         end if
      case (METEO_FROMEXT)
         LEVEL2 'Meteo forcing provided from external'
         interpolate_meteo = .false.
      case default
         call getm_error("init_meteo()", &
                         "no valid met_method")
   end select

   if (met_method.eq.METEO_FROMFILE .or. met_method.eq.METEO_FROMEXT) then

         if(calc_met) then
            LEVEL2 'Stresses and fluxes will be calculated'
            if (met_method .eq. METEO_FROMFILE) then
            if (interpolate_meteo) then
               LEVEL3 'will interpolate input meteo data'
            else
               LEVEL3 'will interpolate calculated fluxes'
            end if
            end if
            if (calc_relative_wind) then
               LEVEL3 'will consider surface currents for relative wind'
               if (.not. interpolate_meteo) then
                  LEVEL3 'WARNING: calc_relative_wind is .true. AND interpolate_meteo is .false.'
                  LEVEL3 'WARNING: restart will not be identical to continuous run !!!'
               end if
            end if
            select case (exchange_method)
                  case(1)
                     ! default is original GETM formulation
                     LEVEL3 'Use Kondo for runtype=3,4; and constant Cd for runtype=1,2'
                  case(2)
                     ! GETM formulation CD(wind,neutral) for no sst instead of the
                     ! constant the CD=0.00125
                     LEVEL3 'Use Kondo, assuming neutral stability for runtype=1,2'
                  case(3)
                     ! CD, CL from Kara et al 2000 
                     LEVEL3 'Use exchange coefficients from Kara et al 2000'
                  case(4)
                     ! CD, CL from Kara et al 2005
                     LEVEL3 'Use exchange coefficients from Kara et al 2005 with direct Ta,qa (if runtype>2)'
                  case(5)
                     ! CD, CL from Kara et al 2005, but with meteo adjustments from 2m to 10m
                     LEVEL3 'Use exchange coefficients from Kara et al 2005 with adjustments (2m->10) for Ta,qa (if runtype>2)'
                     call t10m_init()
               case default
                  ! invalid exchange_coef
                  LEVEL3 'Invalid exchange_method. Will revert to default =1:'
                  exchange_method = 1
                  LEVEL3 'Use original GETM exchange coefficients (default)'
            end select
            LEVEL2 'long-wave back radiation:'
            select case (back_radiation_method)
               case(1)
                  LEVEL3 'using Clark formulation'
               case(2)
                  LEVEL3 'using Hastenrath formulation'
               case(3)
                  LEVEL3 'using Bignami formulation'
               case(4)
                  LEVEL3 'using Berliand formulation'
               case(5)
                  LEVEL3 'using Josey formulation 1'
               case(6)
                  LEVEL3 'using Josey formulation 2'
               case default
            end select
         else
            LEVEL2 'Stresses and fluxes are already calculated'
            interpolate_meteo = .false.
         end if

         calc_evap = (runtype.gt.2 .and. fwf_method.gt.2 .and. calc_met)
         select case (fwf_method)
            case (1)
               LEVEL2 'Constant evaporation/precipitation'
               LEVEL3 'evap   = ',evap_const
               LEVEL3 'precip = ',precip_const
            case (2)
               LEVEL2 'Evaporation/precipitation read'
            case (3)
               LEVEL2 'Precipitation read'
               if (calc_evap) then
                  LEVEL2 'Evaporation will be calculated'
               else
                  LEVEL2 'No evaporation'
               end if
            case (4)
               LEVEL2 'No precipitation'
               if (calc_evap) then
                  LEVEL2 'Evaporation will be calculated'
               else
                  LEVEL2 'No evaporation'
               end if
            case default
         end select
   end if

   LEVEL2 'Albedo method =',albedo_method
   select case (albedo_method)
         case (0)
            LEVEL3 'albedo = ',albedo_const
            albedo = albedo_const
         case (1)
            LEVEL3 'Albedo according to Payne'
         case (2)
            LEVEL3 'Albedo according to Cogley'
      case default
   end select

   if (meteo_ramp .gt. 1) then
      LEVEL2 'meteo_ramp=',meteo_ramp
      ramp_is_active = .true.
      if (hotstart) then
         LEVEL3 'WARNING: hotstart is .true. AND meteo_ramp .gt. 1'
         LEVEL3 'WARNING: .. be sure you know what you are doing ..'
      end if
   end if

   if (met_method .eq. METEO_CONST) then
!     Rotation of wind stress due to grid convergence
      allocate(tausx_const(E2DFIELD),stat=rc)
      if (rc /= 0) stop 'init_meteo: Error allocating memory (tausx_const)'
      allocate(tausy_const(E2DFIELD),stat=rc)
      if (rc /= 0) stop 'init_meteo: Error allocating memory (tausy_const)'
      tausx_const = cosconv*tx - sinconv*ty
      tausy_const = sinconv*tx + cosconv*ty
      tausx = tausx_const
      tausy = tausy_const
      shf   = shf_const
      swr   = swr_const
   end if

   if (fwf_method .eq. 1) then
      evap = evap_const
      precip = precip_const
   end if

   if (met_method.eq.METEO_FROMFILE .or. met_method.eq.METEO_FROMEXT) then
      if (calc_met) then

         if (calc_relative_wind) then
            allocate(u10r(E2DFIELD),stat=rc)
            if (rc /= 0) stop 'init_meteo: Error allocating memory (u10r)'
            u10r = _ZERO_

            allocate(v10r(E2DFIELD),stat=rc)
            if (rc /= 0) stop 'init_meteo: Error allocating memory (v10r)'
            v10r = _ZERO_
         end if

         allocate(t2(E2DFIELD),stat=rc)
         if (rc /= 0) stop 'init_meteo: Error allocating memory (t2)'
         t2 = _ZERO_

         allocate(hum(E2DFIELD),stat=rc)
         if (rc /= 0) stop 'init_meteo: Error allocating memory (hum)'
         hum = _ZERO_

         allocate(tcc(E2DFIELD),stat=rc)
         if (rc /= 0) stop 'init_meteo: Error allocating memory (tcc)'
         tcc = _ZERO_
      end if
   end if


   if (met_method .eq. METEO_FROMFILE) then

      allocate(airp_new(E2DFIELD),stat=rc)
      if (rc /= 0) stop 'init_meteo: Error allocating memory (airp_new)'
      airp_new = _ZERO_

      if (.not. interpolate_meteo) then
         allocate(tausx_new(E2DFIELD),stat=rc)
         if (rc /= 0) stop 'init_meteo: Error allocating memory (tausx_new)'
         tausx_new = _ZERO_
         allocate(d_tausx(E2DFIELD),stat=rc)
         if (rc /= 0) stop 'init_meteo: Error allocating memory (d_tausx)'
         d_tausx = _ZERO_
         tausx_input => d_tausx

         allocate(tausy_new(E2DFIELD),stat=rc)
         if (rc /= 0) stop 'init_meteo: Error allocating memory (tausy_new)'
         tausy_new = _ZERO_
         allocate(d_tausy(E2DFIELD),stat=rc)
         if (rc /= 0) stop 'init_meteo: Error allocating memory (d_tausy)'
         d_tausy = _ZERO_
         tausy_input => d_tausy

         allocate(shf_new(E2DFIELD),stat=rc)
         if (rc /= 0) stop 'init_meteo: Error allocating memory (shf_new)'
         shf_new = _ZERO_
         allocate(d_shf(E2DFIELD),stat=rc)
         if (rc /= 0) stop 'init_meteo: Error allocating memory (d_shf)'
         d_shf = _ZERO_
         shf_input => d_shf
      end if

      if (calc_met) then
         allocate(u10_new(E2DFIELD),stat=rc)
         if (rc /= 0) stop 'init_meteo: Error allocating memory (u10_new)'
         u10_new = _ZERO_

         allocate(v10_new(E2DFIELD),stat=rc)
         if (rc /= 0) stop 'init_meteo: Error allocating memory (v10_new)'
         v10_new = _ZERO_

         allocate(tcc_new(E2DFIELD),stat=rc)
         if (rc /= 0) stop 'init_meteo: Error allocating memory (tcc_new)'
         tcc_new = _ZERO_
      end if

      if (fwf_method.eq.2 .or. (calc_evap .and. .not.interpolate_meteo)) then
         allocate(evap_new(E2DFIELD),stat=rc)
         if (rc /= 0) stop 'init_meteo: Error allocating memory (evap_new)'
         evap_new = _ZERO_
         allocate(d_evap(E2DFIELD),stat=rc)
         if (rc /= 0) stop 'init_meteo: Error allocating memory (d_evap)'
         d_evap = _ZERO_
         evap_input => d_evap
      end if

      if (fwf_method .eq. 2 .or. fwf_method .eq. 3) then
         allocate(precip_new(E2DFIELD),stat=rc)
         if (rc /= 0) stop 'init_meteo: Error allocating memory (precip_new)'
         precip_new = _ZERO_
      end if

   end if

   if (met_method .eq. METEO_FROMEXT) then
      if (ramp_is_active) then
         allocate(tausx_input(E2DFIELD),stat=rc)
         if (rc /= 0) stop 'init_meteo: Error allocating memory (tausx_input)'
         tausx_input = _ZERO_
         allocate(tausy_input(E2DFIELD),stat=rc)
         if (rc /= 0) stop 'init_meteo: Error allocating memory (tausy_input)'
         tausy_input = _ZERO_
      end if
   end if

#ifdef DEBUG
   write(debug,*) 'Leaving init_meteo()'
   write(debug,*)
#endif
   return
   end subroutine init_meteo
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: do_meteo - update the meteo forcing
!
! !INTERFACE:
   subroutine do_meteo(n,sst)
!$ use omp_lib
!
! !DESCRIPTION:
!  Should be called once every time step to update the meteorological forcing.
!  \emph{do\_meteo()} is called with two arguments - n is the loop number and
!  the sea surface temperature.
!  The modelled SST \emph{sst} is only used in the case where fluxes and stresses are
!  calculated as part of the model simulation.
!  The forcing can be obtained in 3 different way - using constant values,
!  using pre-calculated stresses and heat-fluxes or by calculating the
!  fluxes as part of the model integration. In all 3 cases the following
!  fields are the result \emph{, airp, tausx, tausy, swr} and \emph{shf} - i.e.
!  air pressure, stresses in x and y direction, short wave radiation and
!  surface heat fluxes.
!  The surface heat flux is the sum of the latent and sensible heat + the
!  net back radiation. Additional if available the surface freshwater fluxes
!  can be set const or read in from the meteo file. The unit in the meteo file
!  is assumed to be meter (/day).
!  The structure of this routine looks at first glance a bit more complicated
!  than should be necessary. The main reason is we need two fields in order to
!  do any time interpolation - which explains the use of \emph{first}.
!  In addition checks of the logical \emph{new\_meteo} is checked - set by the
!  reading subroutine.
!  Temporal interpolation is done in the principal variables.
!  It is possible to specify a soft start - via the {\tt meteo\_ramp} variable -
!  which is used to calculate a ramp (linearly from 0 to one over {\tt meteo\_ramp}
!  time steps).
!  To implement an use a different set of formulae for flux calculations
!  should be a matter of only changing the involved subroutines.
!
! !USES:
   use getm_timers,     only: tic, toc, TIM_METEO
   use exchcoeffs_kara, only: temp_diff,                            &
                              kara2000_cd, kara2000_clcs,           &
                              kara2005_cd, kara2005_clcs
   use t10m_mod,        only: t10m
   IMPLICIT NONE
!
! !INPUT/OUTPUT PARAMETERS:
   integer, intent(in)                 :: n
   REALTYPE, optional, intent(inout)   :: sst(I2DFIELD)
!
! !REVISION HISTORY:
!  See module for log.
!
! !LOCAL VARIABLES:
   integer                   :: i,j,rc
   REALTYPE                  :: hh,t,t_minus_t2
   REALTYPE, save            :: deltm1=_ZERO_
   REALTYPE                  :: solar_zenith_angle
   REALTYPE                  :: short_wave_radiation
   REALTYPE                  :: albedo_water
   REALTYPE                  :: taus,tausm1
   REALTYPE                  :: td, tc, qc, tmp, wloc, cdm
   logical,save              :: first=.true.
   REALTYPE,dimension(E2DFIELD),target :: work1,work2
   REALTYPE, dimension(:,:), pointer :: tausx_old
   REALTYPE, dimension(:,:), pointer :: tausy_old
   REALTYPE, dimension(:,:), pointer :: shf_old
   REALTYPE, dimension(:,:), pointer :: evap_old
   REALTYPE, dimension(:,:), pointer :: u10r_x,v10r_x,airp_x,precip_x,tcc_x
   REALTYPE,parameter :: rho_air_const = 1.25
   REALTYPE,parameter :: cd_mom_const  = 0.00125
   REALTYPE,parameter :: wind2taus = cd_mom_const * rho_air_const
   REALTYPE,parameter :: taus2wind = _ONE_ / sqrt(wind2taus)
   REALTYPE,parameter :: k00_tdn = _ZERO_
   REALTYPE,parameter :: k05_tdn = -0.098
!EOP
!-------------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'do_meteo() # ',Ncall
#endif
   call tic(TIM_METEO)
! OMP-NOTE: In this routine some loops, which have to do with read-in of
!    new meteo have not been threaded due to use of heap-allocated scalars
!    in exchange_coefficients() and fluxes().
!    However, for the cases I have tested, calls to short_wave_radiation
!    is by far the most expensive of the present routine.
!       BJB 2009-09-30.
!    Update 2019-12: exchange_coefficients() now split into air_coefficients()
!      and kondo_exchange(), but issue with passing info through module variables
!      rather than through the subroutine argument interface still remains.
   if (metforcing) then

      t = n*timestep
      hh = secondsofday*(_ONE_/3600)

      if (ramp_is_active) then
         if (n .ge. meteo_ramp) then
            ramp = _ONE_
            ramp_is_active = .false.
            STDERR LINE
            call write_time_string()
            LEVEL3 timestr,': finished meteo_ramp=',meteo_ramp
            STDERR LINE
         select case (met_method)
            case (METEO_CONST)
               where (az.ne.0)
                  tausx = tausx_const
                  tausy = tausy_const
               end where
            case (METEO_FROMEXT)
               where (az.ne.0)
                  tausx = tausx_input
                  tausy = tausy_input
               end where
               deallocate(tausx_input,stat=rc)
               if (rc /= 0) stop 'do_meteo: Error deallocating memory (tausx_input)'
               deallocate(tausy_input,stat=rc)
               if (rc /= 0) stop 'do_meteo: Error deallocating memory (tausy_input)'
               tausx_input => tausx
               tausy_input => tausy
         end select
         else
            ramp = _ONE_*n/meteo_ramp
         end if
      end if

   if (met_method.eq.METEO_FROMFILE .or. met_method.eq.METEO_FROMEXT) then

!           Note (KK): old and new meteo data cannot be read at once
!                      since they might come from different files.
!                      Thus only new data is read and new_meteo is set to true.
!                      first call with sst at t_1, later with sst at t_2

!           KK-TODO: for consistency of data provided to exchange_coefficients()
!                    and fluxes() we cannot interpolate u10 and v10 isolated
!                    in the beginning (usually t<t_2 and new_meteo data from t_2)!!!
!                    (for alternative see INTERPOLATE_METEO in GOTM...)


#ifdef SLICE_MODEL
            j = jmax/2
#endif
!$OMP PARALLEL DEFAULT(SHARED)                                         &
!$OMP          FIRSTPRIVATE(j)                                         &
!$OMP          PRIVATE(i)
!$OMP SINGLE

            if (calc_met) then

               if (present(sst)) then
!              Note (KK): tcc kept at initial zero if no data provided
!$OMP END SINGLE
!$OMP DO SCHEDULE(RUNTIME)
#ifndef SLICE_MODEL
               do j=jll,jhl
#endif
                  do i=ill,ihl
                     if (az(i,j) .ne. 0) then
                        zenith_angle(i,j) = solar_zenith_angle             &
                                (yearday,hh,lonc(i,j),latc(i,j))
                        swr(i,j) = short_wave_radiation(zenith_angle(i,j), &
                                 yearday,lonc(i,j),latc(i,j),tcc(i,j))
#ifdef BFM_GOTM
                        dl(i,j) = solar_day_length(yearday,dlat)
#endif
                        if (albedo_method .gt. 0) then
                           albedo(i,j) = albedo_water                      &
                                   (albedo_method,zenith_angle(i,j),yearday)
                        end if
                     end if
                  end do
#ifndef SLICE_MODEL
               end do
#endif
!$OMP END DO
!$OMP SINGLE
               end if

               if (calc_relative_wind) then
                  call update_2d_halo(ssu,ssu,az,imin,jmin,imax,jmax,H_TAG)
                  call wait_halo(H_TAG)
                  call update_2d_halo(ssv,ssv,az,imin,jmin,imax,jmax,H_TAG)
                  call wait_halo(H_TAG)

!                 update with latest surface currents
                  u10r = u10 - ssu
                  v10r = v10 - ssv
               end if
               where (az.ne.0) wind = sqrt( u10r*u10r + v10r*v10r )


               if (new_meteo .or. interpolate_meteo) then

                  if (met_method.eq.METEO_FROMFILE .and. .not.interpolate_meteo) then
                     if (calc_relative_wind) then
                        work1 = u10_new - ssu
                        work2 = v10_new - ssv
                        u10r_x => work1
                        v10r_x => work2
                     else
                        u10r_x => u10_new
                        v10r_x => v10_new
                     end if
                     airp_x   => airp_new
                     precip_x => precip_new
                     tcc_x    => tcc_new
                  else
                     u10r_x   => u10r
                     v10r_x   => v10r
                     airp_x   => airp
                     precip_x => precip
                     tcc_x    => tcc
                  end if

!                 Note (KK): t2 and hum are either now (interpolate_meteo=T)
!                            or at new input stage (interpolate_meteo=F)

                  if (present(sst) .and. .not.constant_cd) then

                     ! stability dependent Cd, Cl, Cs
                     if (exchange_method == 1 .or. exchange_method == 2) then
                        ! Kondo

! OMP-NOTE: This is an expensive loop, but we cannot thread it as long
!    as exchange_coefficients() and fluxes() pass information through
!    scalars in the meteo module. BJB 2009-09-30.
!     2019-12 update: This issue still remains after the division of
!       exchange_coefficients() -> air_coefficients() + kondo_exchange()

#ifndef SLICE_MODEL
                        do j=jll,jhl
#endif
                           do i=ill,ihl
                              if (az(i,j) .ge. 1) then
                                 call air_coefficients(u10r_x(i,j),v10r_x(i,j), &
                                      t2(i,j),airp_x(i,j),sst(i,j),             &
                                      hum(i,j),hum_method)
                                 call precip_coefficients(t2(i,j),sst(i,j))
                                 call kondo_exchange(t2(i,j),sst(i,j))
                                 call fluxes(latc(i,j),u10r_x(i,j),v10r_x(i,j),  &
                                      t2(i,j),tcc_x(i,j),sst(i,j),precip_x(i,j), &
                                      shf_input(i,j),tausx_input(i,j),tausy_input(i,j),evap_input(i,j))
                              end if
                           end do
#ifndef SLICE_MODEL
                        end do
#endif
                     else if (exchange_method == 3) then
                        ! Cd and Cl,Cs from Kara et al using:
                        !   3: Kara et al (2000):
                        !      cd_precip, qa, qs, rho_air are still from GETM routine
                        !       air_coefficients().
#ifndef SLICE_MODEL
                        do j=jll,jhl
#endif
                           do i=ill,ihl
                              if (az(i,j) >= 1) then
                                 call air_coefficients(u10r_x(i,j),v10r_x(i,j), &
                                      t2(i,j),airp_x(i,j),sst(i,j),             &
                                      hum(i,j),hum_method)
                                 call precip_coefficients(t2(i,j),sst(i,j))
                                 call temp_diff(t2(i,j),sst(i,j),qs,qa,td)
                                 call kara2000_cd(td, w, cd_mom)
                                 call kara2000_clcs(td, w, cd_latent, cd_heat)
                                 call fluxes(latc(i,j),u10r_x(i,j),v10r_x(i,j),  &
                                      t2(i,j),tcc_x(i,j),sst(i,j),precip_x(i,j), &
                                      shf_input(i,j),tausx_input(i,j),tausy_input(i,j),evap_input(i,j))
                              end if
                           end do
#ifndef SLICE_MODEL
                        end do
#endif
                     else if (exchange_method == 4) then
                        ! Cd and Cl,Cs from Kara et al using:
                        !   4: Kara et al (2005)
                        !       t2 directly, i.e. use Tair@2m in the formulas
                        !       which is a deviation from Kara's formulation.
#ifndef SLICE_MODEL
                        do j=jll,jhl
#endif
                           do i=ill,ihl
                              if (az(i,j) >= 1) then
                                 call air_coefficients(u10r_x(i,j),v10r_x(i,j), &
                                      t2(i,j),airp_x(i,j),sst(i,j),             &
                                      hum(i,j),hum_method)
                                 call precip_coefficients(t2(i,j),sst(i,j))
                                 call temp_diff(t2(i,j),sst(i,j),qs,qa,td)
                                 call kara2005_cd(td, w, cd_mom)
                                 call kara2005_clcs(td, w, cd_latent, cd_heat)
                                 call fluxes(latc(i,j),u10r_x(i,j),v10r_x(i,j),  &
                                      t2(i,j),tcc_x(i,j),sst(i,j),precip_x(i,j), &
                                      shf_input(i,j),tausx_input(i,j),tausy_input(i,j),evap_input(i,j))
                              end if
                           end do
#ifndef SLICE_MODEL
                        end do
#endif
                     else if (exchange_method == 5) then
                        ! Cd and Cl,Cs from Kara et al using:
                        !   5: Kara et al (2005)
                        !       adjust t2 and qa to 10m and use that in Kara's 
                        !       formulas. Tair@10m and qa@10m are calculated
                        !       from the quantities at 2m by use of an
                        !       adaptation (that is, a complete re-write 
                        !       specifically for this purpose) of the COARE
                        !       algorithm. 
                        !
                        ! cd_precip, qa, qs, rho_air are still from GETM routine
                        ! air_coefficients():
#if 0
                        if ( first ) then
                           ! Initialize on first entry:
                           call t10m_init()
                        end if
#endif
#ifndef SLICE_MODEL
                        do j=jll,jhl
#endif
                           do i=ill,ihl
                              if (az(i,j) >= 1) then
                                 call air_coefficients(u10r_x(i,j),v10r_x(i,j), &
                                      t2(i,j),airp_x(i,j),sst(i,j),             &
                                      hum(i,j),hum_method)
                                 call precip_coefficients(t2(i,j),sst(i,j))
                                 ! adjust t,q air @2m to 10m, so t2,qa -> tc,qc
                                 call t10m(w,sst(i,j),t2(i,j),qs,qa,tc,qc)
                                 call temp_diff(tc,sst(i,j),qs,qc,td)
                                 call kara2005_cd(td, w, cd_mom)
                                 call kara2005_clcs(td, w, cd_latent, cd_heat)
                                 call fluxes(latc(i,j),u10r_x(i,j),v10r_x(i,j),  &
                                      t2(i,j),tcc_x(i,j),sst(i,j),precip_x(i,j), &
                                      shf_input(i,j),tausx_input(i,j),tausy_input(i,j),evap_input(i,j))
                              end if
                           end do
#ifndef SLICE_MODEL
                        end do
#endif
                     end if
                  else
                     ! (have_sst==.false.): no sst, use either constant Cd (=1), or Cd with neutral
                     ! stability and constant rho_air assumed (=2,3,4)
                     ! Note: As the model does not use/have SST, we do not 
                     !  compute cd_precip (heat flux from precipitation) at all
                     !  (only used in fluxes(), which is not called in this case).
                     !    BJB 2019-12-09.
                     if (exchange_method == 1) then
                        ! Constant drag Cd=0.00125, rho_air=1.25
#ifndef SLICE_MODEL
                        do j=jll,jhl
#endif
                           do i=ill,ihl
                              if (az(i,j) .ge. 1) then
                                 wloc = sqrt( u10r_x(i,j)*u10r_x(i,j) + v10r_x(i,j)*v10r_x(i,j) )
                                 tmp = wind2taus * wloc
                                 tausx_input(i,j) = tmp * u10r_x(i,j)
                                 tausy_input(i,j) = tmp * v10r_x(i,j)
                              end if
                           end do
#ifndef SLICE_MODEL
                        end do
#endif
                     else if (exchange_method == 2) then
                        ! Kondo Cd(w,neutral t2-stt=0)
#ifndef SLICE_MODEL
                        do j=jll,jhl
#endif
                           do i=ill,ihl
                              if (az(i,j) >= 1) then
!                                KK-TODO: split kondo_exchange() into kondo_cd() and kondo_clcs()
!                                         and only call kondo_cd() here with td=0
!                                         without calling air_coefficients() which requires t2 and hum
                                 call air_coefficients(u10r_x(i,j),v10r_x(i,j), &
                                      t2(i,j),airp_x(i,j),t2(i,j),              &
                                      hum(i,j),hum_method)
                                 call kondo_exchange(t2(i,j),t2(i,j))
                                 tmp = cd_mom * rho_air * w
                                 tausx_input(i,j) = tmp * u10r_x(i,j)
                                 tausy_input(i,j) = tmp * v10r_x(i,j)
                              end if
                           end do
#ifndef SLICE_MODEL
                        end do
#endif
                     else if (exchange_method == 3) then
                        ! Kara et al 2000 with neutral stability and 
                        ! constant rho_air assumed
#ifndef SLICE_MODEL
                        do j=jll,jhl
#endif
                           do i=ill,ihl
                              if (az(i,j) >= 1) then
                                 wloc = sqrt( u10r_x(i,j)*u10r_x(i,j) + v10r_x(i,j)*v10r_x(i,j) )
                                 call kara2000_cd(k00_tdn, wloc, cdm)
                                 tmp = cdm * rho_air_const * wloc
                                 tausx_input(i,j) = tmp * u10r_x(i,j)
                                 tausy_input(i,j) = tmp * v10r_x(i,j)
                              end if
                           end do
#ifndef SLICE_MODEL
                        end do
#endif
                     else if (exchange_method == 4 .or. exchange_method == 5) then
                        ! Kara et al 2005 with neutral stability and 
                        ! constant rho_air assumed
                        ! Note: Neutral is td=-0.098 in Kara et al 2005.
#ifndef SLICE_MODEL
                        do j=jll,jhl
#endif
                           do i=ill,ihl
                              if (az(i,j) >= 1) then
                                 wloc = sqrt( u10r_x(i,j)*u10r_x(i,j) + v10r_x(i,j)*v10r_x(i,j) )
                                 call kara2005_cd(k05_tdn, wloc, cdm)
                                 tmp = cdm * rho_air_const * wloc
                                 tausx_input(i,j) = tmp * u10r_x(i,j)
                                 tausy_input(i,j) = tmp * v10r_x(i,j)
                              end if
                           end do
#ifndef SLICE_MODEL
                        end do
#endif
                     end if
                  end if

               end if

            end if !if calc_met

            if (new_meteo .or. interpolate_meteo) then
               if (periodic_domain) then
!                 need tausx(ihl+1,:) and/or tausy(:,jhl+1) for periodic velocity points
                  call update_2d_halo(tausx_input,tausx_input,az,imin,jmin,imax,jmax,H_TAG)
                  call wait_halo(H_TAG)
                  call update_2d_halo(tausy_input,tausy_input,az,imin,jmin,imax,jmax,H_TAG)
                  call wait_halo(H_TAG)
               end if
            end if

            if (met_method .eq. METEO_FROMFILE) then

               if (new_meteo) then

                  if (.not. interpolate_meteo) then
                     tausx_old=>tausx_new;tausx_new=>d_tausx;d_tausx=>tausx_old;tausx_input=>tausx_old
                     tausy_old=>tausy_new;tausy_new=>d_tausy;d_tausy=>tausy_old;tausy_input=>tausy_old
                     shf_old=>shf_new;shf_new=>d_shf;d_shf=>shf_old;shf_input=>shf_old
                  end if
                  if (fwf_method.eq.2 .or. (calc_evap .and. .not.interpolate_meteo)) then
                     evap_old=>evap_new;evap_new=>d_evap;d_evap=>evap_old;evap_input=>d_evap
                  end if

                  if (.not. first) then
!$OMP END SINGLE
!$OMP DO SCHEDULE(RUNTIME)
#ifndef SLICE_MODEL
                     do j=jmin-HALO,jmax+HALO
#endif
                        do i=imin-HALO,imax+HALO
                           if (az(i,j) .ne. 0) then
                              if (.not. interpolate_meteo) then
                                 d_tausx(i,j) = tausx_new(i,j) - tausx_old(i,j)
                                 d_tausy(i,j) = tausy_new(i,j) - tausy_old(i,j)
                                 d_shf  (i,j) = shf_new  (i,j) - shf_old  (i,j)
                              end if
                              if (fwf_method.eq.2 .or. (calc_evap .and. .not.interpolate_meteo)) then
                                 d_evap(i,j) = evap_new(i,j) - evap_old(i,j)
                              end if
                           end if
                        end do
#ifndef SLICE_MODEL
                     end do
#endif
!$OMP END DO
!$OMP SINGLE
                     deltm1 = _ONE_ / (t_2 - t_1)
                  end if !if (.not. first) then

               end if !if (new_meteo)

               t_minus_t2 = t - t_2
!$OMP END SINGLE
!$OMP DO SCHEDULE(RUNTIME)
#ifndef SLICE_MODEL
               do j=jmin-HALO,jmax+HALO
#endif
                  do i=imin-HALO,imax+HALO
                     if (az(i,j) .ne. 0) then
                        if (.not. interpolate_meteo) then
                           tausx(i,j) = tausx_new(i,j) + d_tausx(i,j)*deltm1*t_minus_t2
                           tausy(i,j) = tausy_new(i,j) + d_tausy(i,j)*deltm1*t_minus_t2
                           shf  (i,j) = shf_new  (i,j) + d_shf  (i,j)*deltm1*t_minus_t2
                        end if
                        if (fwf_method.eq.2 .or. (calc_evap .and. .not.interpolate_meteo)) then
                           evap(i,j) = evap_new(i,j) + d_evap(i,j)*deltm1*t_minus_t2
                        end if
                     end if
                  end do
#ifndef SLICE_MODEL
               end do
#endif
!$OMP END DO
!$OMP SINGLE
            end if !if (met_method .eq. METEO_FROMFILE) then

#ifdef SLICE_MODEL
               airp (:,j+1) = airp (:,j)
               tausx(:,j+1) = tausx(:,j)
               tausy(:,j+1) = tausy(:,j)
               u10  (:,j+1) = u10  (:,j)
               v10  (:,j+1) = v10  (:,j)
               wind (:,j+1) = wind (:,j)
               shf  (:,j+1) = shf  (:,j)
               swr  (:,j+1) = swr  (:,j)
               if (fwf_method.eq.2 .or. (calc_evap .and. .not.interpolate_meteo)) then
                  evap(:,j+1) = evap(:,j)
               end if
               if (fwf_method.eq.2 .or. fwf_method.eq.3) then
                  precip(:,j+1) = precip(:,j)
               end if
               zenith_angle(:,j+1) = zenith_angle(:,j)
#endif

!$OMP END PARALLEL

   end if

   if (ramp_is_active) then
      select case (met_method)
         case (METEO_CONST)
            where (az.ne.0)
               tausx = ramp * tausx_const
               tausy = ramp * tausy_const
            end where
         case (METEO_FROMFILE)
            where (az.ne.0)
               tausx = ramp * tausx
               tausy = ramp * tausy
            end where
         case (METEO_FROMEXT)
            where (az.ne.0)
               tausx = ramp * tausx_input
               tausy = ramp * tausy_input
            end where
      end select
   end if


!     KK-TODO: Jorn prefers zero wind instead of poor approximation...
      if (.not. calc_met) then
         if (.not. (met_method.eq.METEO_FROMEXT .and. .not.new_meteo)) then
            do j=jmin-HALO,jmax+HALO
               do i=imin-HALO,imax+HALO
                  if (az(i,j) .ne. 0) then
                     taus = sqrt( tausx(i,j)*tausx(i,j) + tausy(i,j)*tausy(i,j) )
                     if (taus .gt. _ZERO_) then
                        tausm1 = _ONE_ / taus
                        wind(i,j) = sqrt( taus ) * taus2wind
                        u10 (i,j) = tausx(i,j) * tausm1 * wind(i,j)
                        v10 (i,j) = tausy(i,j) * tausm1 * wind(i,j)
                     else
                        wind(i,j) = _ZERO_
                        u10 (i,j) = _ZERO_
                        v10 (i,j) = _ZERO_
                     end if
                  end if
               end do
            end do
         end if
      end if

   end if
   first = .false.

   call toc(TIM_METEO)
#ifdef DEBUG
     write(debug,*) 'Leaving do_meteo()'
     write(debug,*)
#endif
   return
   end subroutine do_meteo
!EOC
!-----------------------------------------------------------------------
!BOP
#ifdef BFM_GOTM
! !ROUTINE: Calculate the solar day length
!
! !INTERFACE:
   REALTYPE function solar_day_length(yday,lat)
!
! !DESCRIPTION:
!  This subroutine calculates the solar day length for ERSEM-BFM. Based on 
!  an modifications to an earlier version of short_wave_radiation.F90
!  and solar_zenith_angle.F90 for gotm
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: yday
   REALTYPE, intent(in)                :: lat
!
! !REVISION HISTORY:
!  Original author(s): Johan van der Molen
!
! !LOCAL VARIABLES:
   REALTYPE                  :: yrdays,days,stdwin
   REALTYPE                  :: th0,th02,th03,sundec
   REALTYPE, parameter       :: pi=3.14159265358979323846
!EOP
!-----------------------------------------------------------------------
!BOC
   yrdays=365.

   th0 = 2.*pi*yday/yrdays
   th02 = 2.*th0
   th03 = 3.*th0
!  sun declination :
   sundec = 0.006918 - 0.399912*cos(th0) + 0.070257*sin(th0)         &
           - 0.006758*cos(th02) + 0.000907*sin(th02)                 &
           - 0.002697*cos(th03) + 0.001480*sin(th03)
   stdwin = 180./pi*acos( - tan(lat) * tan(sundec))
   solar_day_length = 2.0* stdwin /15.0

   return
   end function solar_day_length
#endif
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: clean_meteo() - cleanup the emph{meteo} module.
!
! !INTERFACE:
   subroutine clean_meteo()
   IMPLICIT NONE
!
! !DESCRIPTION:
!  This routine cleans up the \emph{meteo} module.
!
! !REVISION HISTORY:
!  See module for log.
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'clean_meteo() # ',Ncall
#endif

#ifdef DEBUG
     write(debug,*) 'Leaving clean_meteo()'
     write(debug,*)
#endif
   return
   end subroutine clean_meteo
!EOC

!-----------------------------------------------------------------------

   end module meteo

!-----------------------------------------------------------------------
! Copyright (C) 2001 - Hans Burchard and Karsten Bolding (BBH)         !
!-----------------------------------------------------------------------
