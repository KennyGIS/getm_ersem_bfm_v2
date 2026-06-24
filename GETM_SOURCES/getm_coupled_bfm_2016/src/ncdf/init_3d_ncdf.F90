#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialise 3D netCDF variables
!
! !INTERFACE:
   subroutine init_3d_ncdf(fn,title,starttime,runtype)
!
! !DESCRIPTION:
!
! !USES:
   use netcdf
   use exceptions
   use ncdf_common
   use ncdf_3d
   use domain, only: ioff,joff
   use domain, only: imin,imax,jmin,jmax,kmax
   use domain, only: vert_cord
   use m2d, only: no_2d
   use m3d, only: update_temp,update_salt
   use nonhydrostatic, only: nonhyd_iters,bnh_filter,bnh_weight,calc_hs2d,sbnh_filter
#ifdef SPM
   use suspended_matter, only: spm_save
#endif
#ifdef GETM_BIO

#ifdef BFM_GOTM
   use bio, only:bio_calc
   use bio_var, only: bio_setup  
   use bfm_output, only: stPelStateS,stPelDiagS,stPelFluxS,stBenStateS,stBenDiagS,stBenFluxS
   use bfm_output, only: stPelStateE,stPelDiagE,stPelFluxE,stBenStateE,stBenDiagE,stBenFluxE
#ifdef INCLUDE_DIAGNOS_PRF
   use bio_var, only: nprf
   use bfm_output, only: stPRFDiagS,stPRFDiagE
#endif
#else
   use bio_var, only: numc,var_names,var_units,var_long
#endif

#endif
#ifdef _FABM_
   use getm_fabm, only: model,fabm_calc,output_none
#endif
   use getm_version
!
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   character(len=*), intent(in)        :: fn,title,starttime
   integer, intent(in)                 :: runtype
!
! !DEFINED PARAMETERS:
   logical,    parameter               :: init3d=.true.
!
! !REVISION HISTORY:
!
! !LOCAL VARIABLES:
   integer                   :: err
   integer                   :: n,rc
   integer                   :: scalar(1),f3_dims(3),f4_dims(4)
   REALTYPE                  :: fv,mv,vr(2)
   character(len=80)         :: history,ts
#ifdef BFM_GOTM
   integer                   :: zlen
#endif
!EOP
!-------------------------------------------------------------------------
!BOC
!  create netCDF file
   err = nf90_create(fn, NF90_CLOBBER, ncid)
   if (err .NE. NF90_NOERR) go to 10

!  initialize all time-independent, grid related variables
   call init_grid_ncdf(ncid,init3d,x_dim,y_dim,z_dim)

!  define unlimited dimension
   err = nf90_def_dim(ncid,'time',NF90_UNLIMITED,time_dim)
   if (err .NE. NF90_NOERR) go to 10

!  netCDF dimension vectors
   f3_dims(3)= time_dim
   f3_dims(2)= y_dim
   f3_dims(1)= x_dim

   f4_dims(4)= time_dim
   f4_dims(3)= z_dim
   f4_dims(2)= y_dim
   f4_dims(1)= x_dim

!  gobal settings
   history = 'GETM - www.getm.eu'
   ts = 'seconds since '//starttime

!  time
   err = nf90_def_var(ncid,'time',NF90_DOUBLE,time_dim,time_id)
   if (err .NE. NF90_NOERR) go to 10
   call set_attributes(ncid,time_id,units=trim(ts),long_name='time')

!  elevation
   err = nf90_def_var(ncid,'elev',NCDF_FLOAT_PRECISION,f3_dims,elev_id)
   if (err .NE. NF90_NOERR) go to 10
   fv = elev_missing
   mv = elev_missing
   vr(1) = -15.
   vr(2) =  15.
   call set_attributes(ncid,elev_id,long_name='elevation',units='m', &
                       FillValue=fv,missing_value=mv,valid_range=vr)


   if (save_fluxes) then

      fv = flux_missing
      mv = flux_missing
      vr(1) = -10000.
      vr(2) =  10000.

      err = nf90_def_var(ncid,'fluxu',NCDF_FLOAT_PRECISION,f3_dims,fluxu_id)
      if (err .NE. NF90_NOERR) go to 10
      call set_attributes(ncid,fluxu_id,long_name='avg. grid-related volume flux in local x-direction (U-point)',units='m3/s', &
                          FillValue=fv,missing_value=mv,valid_range=vr)

      err = nf90_def_var(ncid,'fluxv',NCDF_FLOAT_PRECISION,f3_dims,fluxv_id)
      if (err .NE. NF90_NOERR) go to 10
      call set_attributes(ncid,fluxv_id,long_name='avg. grid-related volume flux in local y-direction (V-point)',units='m3/s', &
                          FillValue=fv,missing_value=mv,valid_range=vr)

      err = nf90_def_var(ncid,'fluxuu',NCDF_FLOAT_PRECISION,f4_dims,fluxuu_id)
      if (err .NE. NF90_NOERR) go to 10
      call set_attributes(ncid,fluxuu_id,long_name='grid-related volume flux in local x-direction (U-point)',units='m3/s', &
                          FillValue=fv,missing_value=mv,valid_range=vr)

      err = nf90_def_var(ncid,'fluxvv',NCDF_FLOAT_PRECISION,f4_dims,fluxvv_id)
      if (err .NE. NF90_NOERR) go to 10
      call set_attributes(ncid,fluxvv_id,long_name='grid-related volume flux in local y-direction (V-point)',units='m3/s', &
                          FillValue=fv,missing_value=mv,valid_range=vr)

      err = nf90_def_var(ncid,'fluxw',NCDF_FLOAT_PRECISION,f4_dims,fluxw_id)
      if (err .NE. NF90_NOERR) go to 10
      call set_attributes(ncid,fluxw_id,long_name='vertical volume flux (W-point)',units='m3/s', &
                          FillValue=fv,missing_value=mv,valid_range=vr)

   end if


   fv = vel_missing
   mv = vel_missing
   vr(1) = -3.
   vr(2) =  3.

   if (save_vel2d) then

      err = nf90_def_var(ncid,'u',NCDF_FLOAT_PRECISION,f3_dims,u_id)
      if (err .NE. NF90_NOERR) go to 10
      call set_attributes(ncid,u_id,long_name='avg. velocity in global x-direction (T-point)',units='m/s', &
                          FillValue=fv,missing_value=mv,valid_range=vr)

      err = nf90_def_var(ncid,'v',NCDF_FLOAT_PRECISION,f3_dims,v_id)
      if (err .NE. NF90_NOERR) go to 10
      call set_attributes(ncid,v_id,long_name='avg. velocity in global y-direction (T-point)',units='m/s', &
                          FillValue=fv,missing_value=mv,valid_range=vr)

   end if

   if (save_vel3d) then

      err = nf90_def_var(ncid,'uu',NCDF_FLOAT_PRECISION,f4_dims,uu_id)
      if (err .NE. NF90_NOERR) go to 10
      call set_attributes(ncid,uu_id,long_name='velocity in global x-direction (T-point)',units='m/s', &
                          FillValue=fv,missing_value=mv,valid_range=vr)

      err = nf90_def_var(ncid,'vv',NCDF_FLOAT_PRECISION,f4_dims,vv_id)
      if (err .NE. NF90_NOERR) go to 10
      call set_attributes(ncid,vv_id,long_name='velocity in global y-direction (T-point)',units='m/s', &
                          FillValue=fv,missing_value=mv,valid_range=vr)

      err = nf90_def_var(ncid,'w',NCDF_FLOAT_PRECISION,f4_dims,w_id)
      if (err .NE. NF90_NOERR) go to 10
      call set_attributes(ncid,w_id,long_name='vertical velocity (T-point)',units='m/s', &
                          FillValue=fv,missing_value=mv,valid_range=vr)

   end if

   if (save_taub) then

      !  bottom stress in x-direction
      err = nf90_def_var(ncid,'taubx',NCDF_FLOAT_PRECISION,f3_dims,taubx_id)
      if (err .NE. NF90_NOERR) go to 10
      fv = tau_missing
      mv = tau_missing
      vr(1) = -10.
      vr(2) =  10.
      call set_attributes(ncid,taubx_id,long_name='bottom stress (x)',units='Pa', &
           FillValue=fv,missing_value=mv,valid_range=vr)

      !  bottom stress in y-direction
      err = nf90_def_var(ncid,'tauby',NCDF_FLOAT_PRECISION,f3_dims,tauby_id)
      if (err .NE. NF90_NOERR) go to 10
      fv = tau_missing
      mv = tau_missing
      vr(1) = -10.
      vr(2) =  10.
      call set_attributes(ncid,tauby_id,long_name='bottom stress (y)',units='Pa', &
           FillValue=fv,missing_value=mv,valid_range=vr)

      !  maximum bottom stress
      err = nf90_def_var(ncid,'taubmax_3d',NCDF_FLOAT_PRECISION,f3_dims,taubmax_3d_id)
      if (err .NE. NF90_NOERR) go to 10
      fv = tau_missing
      mv = tau_missing
      vr(1) =  0.
      vr(2) = 20.
      call set_attributes(ncid,taubmax_3d_id,  &
                          long_name='max. bottom stress',units='N/m2', &
                          FillValue=fv,missing_value=mv,valid_range=vr)

   endif

   if (save_h) then
      fv = hh_missing
      mv = hh_missing
      err = nf90_def_var(ncid,'h',NCDF_FLOAT_PRECISION,f4_dims,h_id)
      if (err .NE. NF90_NOERR) go to 10
      call set_attributes(ncid,h_id,long_name='layer thickness',  &
                          units='m',FillValue=fv,missing_value=mv)
   end if

   if (save_z) then
      fv = zc_missing
      mv = zc_missing
      err = nf90_def_var(ncid,'zc',NCDF_FLOAT_PRECISION,f4_dims,zc_id)
      if (err .NE. NF90_NOERR) go to 10
      call set_attributes(ncid,zc_id,long_name='vertical position',  &
                          units='m',FillValue=fv,missing_value=mv)
   end if

!  hydrostatic consistency criterion
   err = nf90_def_var(ncid,'hcc',NCDF_FLOAT_PRECISION,f4_dims(1:3),hcc_id)
   if (err .NE. NF90_NOERR) go to 10
   fv = -_ONE_
   mv = -_ONE_
   vr(1) = 0.
   vr(2) = 1.
   call set_attributes(ncid,hcc_id,  &
                       long_name='hcc',units=' ',          &
                       FillValue=fv,missing_value=mv,valid_range=vr)

#ifdef _MOMENTUM_TERMS_
   fv = vel_missing
   mv = vel_missing
   vr(1) = -3.
   vr(2) =  3.

   err = nf90_def_var(ncid,'tdv_u',NCDF_FLOAT_PRECISION,f4_dims,tdv_u_id)
   if (err .NE. NF90_NOERR) go to 10
   call set_attributes(ncid,tdv_u_id,long_name='time tendency (u)',units='m2/s2', &
                       FillValue=fv,missing_value=mv,valid_range=vr)

   err = nf90_def_var(ncid,'adv_u',NCDF_FLOAT_PRECISION,f4_dims,adv_u_id)
   if (err .NE. NF90_NOERR) go to 10
   call set_attributes(ncid,adv_u_id,long_name='advection (u).',units='m2/s2', &
                       FillValue=fv,missing_value=mv,valid_range=vr)

   err = nf90_def_var(ncid,'vsd_u',NCDF_FLOAT_PRECISION,f4_dims,vsd_u_id)
   if (err .NE. NF90_NOERR) go to 10
   call set_attributes(ncid,vsd_u_id,long_name='vertical stress divergence (u).',units='m2/s2', &
                       FillValue=fv,missing_value=mv,valid_range=vr)

   err = nf90_def_var(ncid,'hsd_u',NCDF_FLOAT_PRECISION,f4_dims,hsd_u_id)
   if (err .NE. NF90_NOERR) go to 10
   call set_attributes(ncid,hsd_u_id,long_name='horizontal stress divergence (u).',units='m2/s2', &
                       FillValue=fv,missing_value=mv,valid_range=vr)

   err = nf90_def_var(ncid,'cor_u',NCDF_FLOAT_PRECISION,f4_dims,cor_u_id)
   if (err .NE. NF90_NOERR) go to 10
   call set_attributes(ncid,cor_u_id,long_name='Coriolis term (u).',units='m2/s2', &
                       FillValue=fv,missing_value=mv,valid_range=vr)

   err = nf90_def_var(ncid,'epg_u',NCDF_FLOAT_PRECISION,f4_dims,epg_u_id)
   if (err .NE. NF90_NOERR) go to 10
   call set_attributes(ncid,epg_u_id,long_name='extenal pressure gradient (u).',units='m2/s2', &
                       FillValue=fv,missing_value=mv,valid_range=vr)

   err = nf90_def_var(ncid,'ipg_u',NCDF_FLOAT_PRECISION,f4_dims,ipg_u_id)
   if (err .NE. NF90_NOERR) go to 10
   call set_attributes(ncid,ipg_u_id,long_name='internal pressure gradient (u).',units='m2/s2', &
                       FillValue=fv,missing_value=mv,valid_range=vr)

   err = nf90_def_var(ncid,'tdv_v',NCDF_FLOAT_PRECISION,f4_dims,tdv_v_id)
   if (err .NE. NF90_NOERR) go to 10
   call set_attributes(ncid,tdv_v_id,long_name='time tendency (v).',units='m2/s2', &
                       FillValue=fv,missing_value=mv,valid_range=vr)

   err = nf90_def_var(ncid,'adv_v',NCDF_FLOAT_PRECISION,f4_dims,adv_v_id)
   if (err .NE. NF90_NOERR) go to 10
   call set_attributes(ncid,adv_v_id,long_name='advection (v).',units='m2/s2', &
                       FillValue=fv,missing_value=mv,valid_range=vr)

   err = nf90_def_var(ncid,'vsd_v',NCDF_FLOAT_PRECISION,f4_dims,vsd_v_id)
   if (err .NE. NF90_NOERR) go to 10
   call set_attributes(ncid,vsd_v_id,long_name='vertical stress divergence (v).',units='m2/s2', &
                       FillValue=fv,missing_value=mv,valid_range=vr)

   err = nf90_def_var(ncid,'hsd_v',NCDF_FLOAT_PRECISION,f4_dims,hsd_v_id)
   if (err .NE. NF90_NOERR) go to 10
   call set_attributes(ncid,hsd_v_id,long_name='horizontal stress divergence (v).',units='m2/s2', &
                       FillValue=fv,missing_value=mv,valid_range=vr)

   err = nf90_def_var(ncid,'cor_v',NCDF_FLOAT_PRECISION,f4_dims,cor_v_id)
   if (err .NE. NF90_NOERR) go to 10
   call set_attributes(ncid,cor_v_id,long_name='Coriolis term (v).',units='m2/s2', &
                       FillValue=fv,missing_value=mv,valid_range=vr)

   err = nf90_def_var(ncid,'epg_v',NCDF_FLOAT_PRECISION,f4_dims,epg_v_id)
   if (err .NE. NF90_NOERR) go to 10
   call set_attributes(ncid,epg_v_id,long_name='external pressure gradient (v).',units='m2/s2', &
                       FillValue=fv,missing_value=mv,valid_range=vr)

   err = nf90_def_var(ncid,'ipg_v',NCDF_FLOAT_PRECISION,f4_dims,ipg_v_id)
   if (err .NE. NF90_NOERR) go to 10
   call set_attributes(ncid,ipg_v_id,long_name='internal pressure gradient (v).',units='m2/s2', &
                       FillValue=fv,missing_value=mv,valid_range=vr)
#endif

   if (save_s) then
      fv = salt_missing
      mv = salt_missing
      vr(1) =  0.
      vr(2) = 42.
      err = nf90_def_var(ncid,'salt',NCDF_FLOAT_PRECISION,f4_dims,salt_id)
      if (err .NE. NF90_NOERR) go to 10
      call set_attributes(ncid,salt_id,long_name='salinity',units='PSU', &
                          FillValue=fv,missing_value=mv,valid_range=vr)
   end if

   if (save_t) then
      fv = temp_missing
      mv = temp_missing
      vr(1) = -2.
      vr(2) = 40.
      err = nf90_def_var(ncid,'temp',NCDF_FLOAT_PRECISION,f4_dims,temp_id)
      if (err .NE. NF90_NOERR) go to 10
      call set_attributes(ncid,temp_id,long_name='temperature',units='degC',&
                          FillValue=fv,missing_value=mv,valid_range=vr)
   end if

   if (save_rho) then
      fv = rho_missing
      mv = rho_missing
      vr(1) =  0.
      vr(2) = 30.
      err = nf90_def_var(ncid,'sigma_t',NCDF_FLOAT_PRECISION,f4_dims,sigma_t_id)
      if (err .NE. NF90_NOERR) go to 10
      call set_attributes(ncid,sigma_t_id,long_name='sigma_t',units='kg/m3',&
                          FillValue=fv,missing_value=mv,valid_range=vr)
   end if

   if (save_strho) then
      if (save_rad) then
         fv = rad_missing
         mv = rad_missing
         vr(1) =  0.
         vr(2) = 1354.
         err = nf90_def_var(ncid,'radiation',NCDF_FLOAT_PRECISION,f4_dims,rad_id)
         if (err .NE. NF90_NOERR) go to 10
         call set_attributes(ncid,rad_id,long_name='radiation',units='W/m2',&
                             FillValue=fv,missing_value=mv,valid_range=vr)
      end if
   end if

#ifndef NO_BAROCLINIC
   if (calc_stirr .and. save_stirr) then
      fv = stirr_missing
      mv = stirr_missing
      vr(1) = -500.
      vr(2) =  500.

      err = nf90_def_var(ncid,'diffxx',NCDF_FLOAT_PRECISION,f4_dims,diffxx_id)
      if (err .NE. NF90_NOERR) go to 10
      call set_attributes(ncid,diffxx_id,long_name='zonal stirring diffusivity',units='m2/s',&
                          FillValue=fv,missing_value=mv,valid_range=vr)

#ifndef SLICE_MODEL
      err = nf90_def_var(ncid,'diffyy',NCDF_FLOAT_PRECISION,f4_dims,diffyy_id)
      if (err .NE. NF90_NOERR) go to 10
      call set_attributes(ncid,diffyy_id,long_name='meridional stirring diffusivity',units='m2/s',&
                          FillValue=fv,missing_value=mv,valid_range=vr)

      err = nf90_def_var(ncid,'diffxy',NCDF_FLOAT_PRECISION,f4_dims,diffxy_id)
      if (err .NE. NF90_NOERR) go to 10
      call set_attributes(ncid,diffxy_id,long_name='cross stirring diffusivity',units='m2/s',&
                          FillValue=fv,missing_value=mv,valid_range=vr)
#endif
   end if
#endif

   if (save_turb) then

      if (save_tke) then
         fv = tke_missing
         mv = tke_missing
         vr(1) = 0.
         vr(2) = 0.2
         err = nf90_def_var(ncid,'tke',NCDF_FLOAT_PRECISION,f4_dims,tke_id)
         if (err .NE. NF90_NOERR) go to 10
         call set_attributes(ncid,tke_id,long_name='TKE',units='m2/s2', &
                             FillValue=fv,missing_value=mv,valid_range=vr)
      end if

      if (save_num) then
         fv = num_missing
         mv = num_missing
         vr(1) = 0.
         vr(2) = 0.2
         err = nf90_def_var(ncid,'num',NCDF_FLOAT_PRECISION,f4_dims,num_id)
         if (err .NE. NF90_NOERR) go to 10
         call set_attributes(ncid,num_id,long_name='viscosity',units='m2/s', &
                             FillValue=fv,missing_value=mv,valid_range=vr)
      end if

      if (save_nuh) then
         fv = nuh_missing
         mv = nuh_missing
         vr(1) = 0.
         vr(2) = 0.2
         err = nf90_def_var(ncid,'nuh',NCDF_FLOAT_PRECISION,f4_dims,nuh_id)
         if (err .NE. NF90_NOERR) go to 10
         call set_attributes(ncid,nuh_id,long_name='diffusivity',units='m2/s', &
                             FillValue=fv,missing_value=mv,valid_range=vr)
      end if

      if (save_eps) then
         fv = eps_missing
         mv = eps_missing
         vr(1) = 0.
         vr(2) = 0.2
         err = nf90_def_var(ncid,'diss',NCDF_FLOAT_PRECISION,f4_dims,eps_id)
         if (err .NE. NF90_NOERR) go to 10
         call set_attributes(ncid,eps_id,long_name='dissipation',units='m2/s3',&
                             FillValue=fv,missing_value=mv,valid_range=vr)
      end if
   end if

   if (save_SS_NN) then

      fv = SS_missing
      mv = SS_missing
      vr(1) = 0.
      vr(2) = 0.01
      err = nf90_def_var(ncid,'SS',NCDF_FLOAT_PRECISION,f4_dims,SS_id)
      if (err .NE. NF90_NOERR) go to 10
      call set_attributes(ncid,SS_id,long_name='shear frequency squared',units='s-2',&
                          FillValue=fv,missing_value=mv,valid_range=vr)
#ifndef NO_BAROCLINIC
      fv = NN_missing
      mv = NN_missing
      vr(1) = -0.001
      vr(2) = 0.01
      err = nf90_def_var(ncid,'NN',NCDF_FLOAT_PRECISION,f4_dims,NN_id)
      if (err .NE. NF90_NOERR) go to 10
      call set_attributes(ncid,NN_id,long_name='buoyancy frequency squared', &
                          units='s-2',&
                          FillValue=fv,missing_value=mv,valid_range=vr)
#endif

   end if

   if (save_numerical_analyses) then

      fv = nummix_missing
      mv = nummix_missing
      vr(1) = -100.0
      vr(2) = 100.0
      err = nf90_def_var(ncid,'numdis_3d',NCDF_FLOAT_PRECISION,f4_dims,nm3d_id)
      if (err .NE. NF90_NOERR) go to 10
      call set_attributes(ncid,nm3d_id, &
          long_name='numerical dissipation content', &
          units='m*W/kg',&
          FillValue=fv,missing_value=mv,valid_range=vr)

      if (update_salt) then
         err = nf90_def_var(ncid,'nummix_S',NCDF_FLOAT_PRECISION,f4_dims,nm3dS_id)
         if (err .NE. NF90_NOERR) go to 10
         call set_attributes(ncid,nm3dS_id, &
             long_name='numerical mixing content of salinity', &
             units='m*psu**2/s',&
             FillValue=fv,missing_value=mv,valid_range=vr)

         err = nf90_def_var(ncid,'phymix_S',NCDF_FLOAT_PRECISION,f4_dims,pm3dS_id)
         if (err .NE. NF90_NOERR) go to 10
         call set_attributes(ncid,pm3dS_id, &
             long_name='physical mixing content of salinity', &
             units='m*psu**2/s',&
             FillValue=fv,missing_value=mv,valid_range=vr)
      end if

      if (update_temp) then
         err = nf90_def_var(ncid,'nummix_T',NCDF_FLOAT_PRECISION,f4_dims,nm3dT_id)
         if (err .NE. NF90_NOERR) go to 10
         call set_attributes(ncid,nm3dT_id, &
             long_name='numerical mixing content of temperature', &
             units='m*degC**2/s',&
             FillValue=fv,missing_value=mv,valid_range=vr)

         err = nf90_def_var(ncid,'phymix_T',NCDF_FLOAT_PRECISION,f4_dims,pm3dT_id)
         if (err .NE. NF90_NOERR) go to 10
         call set_attributes(ncid,pm3dT_id, &
             long_name='physical mixing content of temperature', &
             units='m*degC**2/s',&
             FillValue=fv,missing_value=mv,valid_range=vr)
      end if

   end if

   if (nonhyd_method .ne. 0) then
      fv = bnh_missing
      mv = bnh_missing
      if (runtype.eq.2 .or. nonhyd_method.eq.1) then
         vr(1) = -10.
         vr(2) = 10.
         err = nf90_def_var(ncid,'bnh',NCDF_FLOAT_PRECISION,f4_dims,bnh_id)
         if (err .NE. NF90_NOERR) go to 10
         call set_attributes(ncid,bnh_id,long_name='nh buoyancy correction',units='m/s2',&
                             FillValue=fv,missing_value=mv,valid_range=vr)
         if (nonhyd_method .eq. 1) then
            err = nf90_put_att(ncid,bnh_id,'nonhyd_iters',nonhyd_iters)
            err = nf90_put_att(ncid,bnh_id,'bnh_filter',bnh_filter)
            if (bnh_filter .eq. 1 .or. bnh_filter .eq. 3) then
               err = nf90_put_att(ncid,bnh_id,'bnh_weight',bnh_weight)
            end if
            if (.not. no_2d) then
               if (calc_hs2d) then
                  err = nf90_put_att(ncid,bnh_id,'calc_hs2d','true')
               else
                  err = nf90_put_att(ncid,bnh_id,'calc_hs2d','false')
                  if (sbnh_filter) then
                     err = nf90_put_att(ncid,bnh_id,'sbnh_filter','true')
                  else
                     err = nf90_put_att(ncid,bnh_id,'sbnh_filter','false')
                  end if
               end if
            end if
         end if
      else
         vr(1) = -10./SMALL
         vr(2) =  10./SMALL
         err = nf90_def_var(ncid,'nhsp',NCDF_FLOAT_PRECISION,f4_dims,bnh_id)
         if (err .NE. NF90_NOERR) go to 10
         call set_attributes(ncid,bnh_id,long_name='nh screening parameter',units=' ',&
                             FillValue=fv,missing_value=mv,valid_range=vr)
      end if
   end if

   if (Am_method.eq.AM_LES .and. save_Am_3d) then
      fv = Am_3d_missing
      mv = Am_3d_missing
      vr(1) = 0.
      vr(2) = 500.
      err = nf90_def_var(ncid,'Am_3d',NCDF_FLOAT_PRECISION,f4_dims,Am_3d_id)
      if (err .NE. NF90_NOERR) go to 10
      call set_attributes(ncid,Am_3d_id,long_name='hor. eddy viscosity',units='m2/s',&
                          FillValue=fv,missing_value=mv,valid_range=vr)
   end if


   if (save_waves) then

      fv = waves_missing
      mv = waves_missing

      if (save_fluxes) then
         vr(1) = -3.
         vr(2) =  3.
         err = nf90_def_var(ncid,'fluxuuStokes',NCDF_FLOAT_PRECISION,f4_dims,fluxuuStokes_id)
         if (err .NE. NF90_NOERR) go to 10
         call set_attributes(ncid,fluxuuStokes_id,long_name='grid-related volume Stokes flux in local x-direction (U-point)', &
                             units='m3/s',                                                                                    &
                             FillValue=fv,missing_value=mv,valid_range=vr)
         err = nf90_def_var(ncid,'fluxvvStokes',NCDF_FLOAT_PRECISION,f4_dims,fluxvvStokes_id)
         if (err .NE. NF90_NOERR) go to 10
         call set_attributes(ncid,fluxvvStokes_id,long_name='grid-related volume Stokes flux in local y-direction (V-point)', &
                             units='m3/s',                                                                                    &
                             FillValue=fv,missing_value=mv,valid_range=vr)
      end if

      if (save_vel3d) then
         vr(1) = -1.
         vr(2) =  1.
         err = nf90_def_var(ncid,'uuStokes',NCDF_FLOAT_PRECISION,f4_dims,uuStokes_id)
         if (err .NE. NF90_NOERR) go to 10
         call set_attributes(ncid,uuStokes_id,long_name='Stokes drift in global x-direction (T-point)', &
                             units='m/s',                                                               &
                             FillValue=fv,missing_value=mv,valid_range=vr)
         err = nf90_def_var(ncid,'vvStokes',NCDF_FLOAT_PRECISION,f4_dims,vvStokes_id)
         if (err .NE. NF90_NOERR) go to 10
         call set_attributes(ncid,vvStokes_id,long_name='Stokes drift in global y-direction (T-point)', &
                             units='m/s',                                                               &
                             FillValue=fv,missing_value=mv,valid_range=vr)
      end if

   end if

#ifdef SPM
   if (spm_save) then
      fv = spm_missing
      mv = spm_missing
      err = nf90_def_var(ncid,'spm_pool',NCDF_FLOAT_PRECISION,f3_dims,spmpool_id)
      if (err .NE. NF90_NOERR) go to 10
      vr(1) = 0.
      vr(2) = 10.
      call set_attributes(ncid,spmpool_id,long_name='bottom spm pool', &
                          units='kg/m2', &
                          FillValue=fv,missing_value=mv,valid_range=vr)
      vr(1) =  0.
      vr(2) = 30.
      err = nf90_def_var(ncid,'spm',NCDF_FLOAT_PRECISION,f4_dims,spm_id)
      if (err .NE. NF90_NOERR) go to 10
      call set_attributes(ncid,spm_id,  &
                          long_name='suspended particulate matter', &
                          units='kg/m3', &
                          FillValue=fv,missing_value=mv,valid_range=vr)
   end if
#endif

#ifdef GETM_BIO
#ifdef BFM_GOTM
    if ( bio_calc) then
      if ( bio_setup /=2 ) then
        zlen=kmax+1
        call init_3d_ncdf_biovars(1,zlen,stPelStateS,stPelStateE, 4,f4_dims,err)
        if (err .NE.  NF90_NOERR) go to 10
        call init_3d_ncdf_biovars(2,zlen,stPelDiagS,stPelFluxE, 4,f4_dims,err)
        if (err .NE.  NF90_NOERR) go to 10
      endif
      if ( bio_setup >=2 ) then
        call init_3d_ncdf_biovars(1,1,stBenStateS,stBenStateE, 3,f3_dims,err)
        if (err .NE.  NF90_NOERR) go to 10
        call init_3d_ncdf_biovars(2,1,stBenDiagS,stBenFluxE, 3,f3_dims,err)
        if (err .NE.  NF90_NOERR) go to 10
#ifdef INCLUDE_DIAGNOS_PRF
        n=nprf+1
        call init_3d_ncdf_biovars(2,n,stPRFDiagS,stPRFDiagE, 4,f4_dims,err)
        if (err .NE.  NF90_NOERR) go to 10
#endif
      endif
    endif
!END_BFM
#else
   allocate(bio_ids(numc),stat=rc)
   if (rc /= 0) stop 'init_3d_ncdf(): Error allocating memory (bio_ids)'
   STDERR numc
   fv = bio_missing
   mv = bio_missing
   vr(1) = _ZERO_
   vr(2) = 9999.
   do n=1,numc
      err = nf90_def_var(ncid,var_names(n),NCDF_FLOAT_PRECISION,f4_dims,bio_ids(n))
      if (err .NE.  NF90_NOERR) go to 10
      call set_attributes(ncid,bio_ids(n), &
                          long_name=trim(var_long(n)), &
                          units=trim(var_units(n)), &
                          FillValue=fv,missing_value=mv,valid_range=vr)
   end do
#endif
#endif

#ifdef _FABM_
   if (fabm_calc) then
      allocate(fabm_ids(size(model%state_variables)),stat=rc)
      if (rc /= 0) stop 'init_3d_ncdf(): Error allocating memory (fabm_ids)'
      fabm_ids = -1
      do n=1,size(model%state_variables)
         if (model%state_variables(n)%output==output_none) cycle
         err = nf90_def_var(ncid,model%state_variables(n)%name,NCDF_FLOAT_PRECISION,f4_dims,fabm_ids(n))
         if (err .NE.  NF90_NOERR) go to 10
         call set_attributes(ncid,fabm_ids(n), &
                          long_name    =trim(model%state_variables(n)%long_name), &
                          units        =trim(model%state_variables(n)%units),    &
                          FillValue    =model%state_variables(n)%missing_value,  &
                          missing_value=model%state_variables(n)%missing_value,  &
                          valid_min    =model%state_variables(n)%minimum,        &
                          valid_max    =model%state_variables(n)%maximum)
      end do

      allocate(fabm_ids_ben(size(model%bottom_state_variables)),stat=rc)
      if (rc /= 0) stop 'init_3d_ncdf(): Error allocating memory (fabm_ids_ben)'
      fabm_ids_ben = -1
      do n=1,size(model%bottom_state_variables)
         if (model%bottom_state_variables(n)%output==output_none) cycle
         err = nf90_def_var(ncid,model%bottom_state_variables(n)%name,NCDF_FLOAT_PRECISION,f3_dims,fabm_ids_ben(n))
         if (err .NE.  NF90_NOERR) go to 10
         call set_attributes(ncid,fabm_ids_ben(n), &
                          long_name    =trim(model%bottom_state_variables(n)%long_name), &
                          units        =trim(model%bottom_state_variables(n)%units),    &
                          FillValue    =model%bottom_state_variables(n)%missing_value,  &
                          missing_value=model%bottom_state_variables(n)%missing_value,  &
                          valid_min    =model%bottom_state_variables(n)%minimum,        &
                          valid_max    =model%bottom_state_variables(n)%maximum)
      end do

      allocate(fabm_ids_diag(size(model%diagnostic_variables)),stat=rc)
      if (rc /= 0) stop 'init_3d_ncdf(): Error allocating memory (fabm_ids_diag)'
      fabm_ids_diag = -1
      do n=1,size(model%diagnostic_variables)
         if (model%diagnostic_variables(n)%output==output_none) cycle
         err = nf90_def_var(ncid,model%diagnostic_variables(n)%name,NCDF_FLOAT_PRECISION,f4_dims,fabm_ids_diag(n))
         if (err .NE.  NF90_NOERR) go to 10
         call set_attributes(ncid,fabm_ids_diag(n), &
                          long_name    =trim(model%diagnostic_variables(n)%long_name), &
                          units        =trim(model%diagnostic_variables(n)%units),    &
                          FillValue    =model%diagnostic_variables(n)%missing_value,  &
                          missing_value=model%diagnostic_variables(n)%missing_value,  &
                          valid_min    =model%diagnostic_variables(n)%minimum,        &
                          valid_max    =model%diagnostic_variables(n)%maximum)
      end do

      allocate(fabm_ids_diag_hz(size(model%horizontal_diagnostic_variables)),stat=rc)
      if (rc /= 0) stop 'init_3d_ncdf(): Error allocating memory (fabm_ids_diag_hz)'
      fabm_ids_diag_hz = -1
      do n=1,size(model%horizontal_diagnostic_variables)
         if (model%horizontal_diagnostic_variables(n)%output==output_none) cycle
         err = nf90_def_var(ncid,model%horizontal_diagnostic_variables(n)%name,NCDF_FLOAT_PRECISION,f3_dims,fabm_ids_diag_hz(n))
         if (err .NE.  NF90_NOERR) go to 10
         call set_attributes(ncid,fabm_ids_diag_hz(n), &
                          long_name    =trim(model%horizontal_diagnostic_variables(n)%long_name), &
                          units        =trim(model%horizontal_diagnostic_variables(n)%units),    &
                          FillValue    =model%horizontal_diagnostic_variables(n)%missing_value,  &
                          missing_value=model%horizontal_diagnostic_variables(n)%missing_value,  &
                          valid_min    =model%horizontal_diagnostic_variables(n)%minimum,        &
                          valid_max    =model%horizontal_diagnostic_variables(n)%maximum)
      end do
   end if
#endif

!  globals
   err = nf90_put_att(ncid,NF90_GLOBAL,'title',trim(title))
   if (err .NE. NF90_NOERR) go to 10

   err = nf90_put_att(ncid,NF90_GLOBAL,'model ',trim(history))
   if (err .NE. NF90_NOERR) go to 10

#if 0
   err = nf90_put_att(ncid,NF90_GLOBAL,'git hash:   ',trim(git_commit_id))
   if (err .NE. NF90_NOERR) go to 10

   err = nf90_put_att(ncid,NF90_GLOBAL,'git branch: ',trim(git_branch_name))
   if (err .NE. NF90_NOERR) go to 10
#endif

!   history = FORTRAN_VERSION
!   err = nf90_put_att(ncid,NF90_GLOBAL,'compiler',trim(history))
!   if (err .NE. NF90_NOERR) go to 10

   ! leave define mode
   err = nf90_enddef(ncid)
   if (err .NE. NF90_NOERR) go to 10

   return

   10 FATAL 'init_3d_ncdf: ',nf90_strerror(err)
   stop 'init_3d_ncdf'
   end subroutine init_3d_ncdf
!EOC
!-------------------------------------------------------------------------
#ifdef GETM_BIO
!BOP
!
! !IROUTINE: Initialise 3D netCDF variables
!
! !INTERFACE:
   subroutine init_3d_ncdf_biovars(mode,zlen,from,to,n_dims,f_dims,err)
!
! !USES:
   use netcdf
   use exceptions
   use ncdf_common
   use ncdf_3d
! !DESCRIPTION:
!
#ifdef BFM_GOTM
   use bfm_output, only: var_ave       !BFM
   use bfm_output, only: var_names,var_units,var_long,var_ids       !BFM
#else
   use bio_var, only: var_names,var_units,var_long,var_ids       !BFM
#endif
   IMPLICIT NONE
! !INPUT PARAMETERS:
   integer,intent(IN)                           :: mode
   integer,intent(IN)                           :: zlen
   integer,intent(IN)                           :: from
   integer,intent(IN)                           :: to
   integer,intent(IN)                           :: n_dims
   integer,intent(IN)                           :: f_dims(n_dims)
! !OUTPUT PARAMETERS:
   integer,intent(OUT)                          :: err
! !LOCAL PARAMETERS:
   REALTYPE               :: fv,mv,vr(2)
   integer                :: n

   integer,external          :: special_dims
!EOP
!-------------------------------------------------------------------------
!BOC

   fv = bio_missing
   mv = bio_missing
   vr(1) = _ZERO_ ; if ( mode== 2) vr(1)=-1.0D+20
   vr(2) = 1.0D+20
   if ( from ==0 ) return
   do n= from, to
#ifdef BFM_GOTM
     if ( zlen.gt.1 .and.var_ids(n) < 0)  then
       err= special_dims(2,ncid,zlen,var_names(n),var_long(n),var_units(n), &
                                    n_dims,f_dims,bio_missing,var_ids(n))
       
       if (err /=  NF90_NOERR)  return
     endif
#endif
     if (var_ids(n) < 0) then                                          !BFM
       err = nf90_def_var(ncid,var_names(n),NCDF_FLOAT_PRECISION,f_dims,var_ids(n))
       if (err /=  NF90_NOERR)  return

       call set_attributes(ncid,var_ids(n), &
                          long_name=trim(var_long(n)), &
                          units=trim(var_units(n)), &
                          FillValue=fv,missing_value=mv,valid_range=vr)
     end if                                                              !BFM
#ifdef BFM_GOTM
     if (var_ave(n)) & 
       err= special_dims(1,ncid,zlen,var_names(n),var_long(n),var_units(n), &
                                     n_dims,f_dims,bio_missing,var_ids(n))
       if (err /=  NF90_NOERR)  return
#endif
   end do

   return

   end subroutine init_3d_ncdf_biovars

!EOC
!-----------------------------------------------------------------------
!BOP
! !ROUTINE: Defining extra dimension vars
!
! !INTERFACE:
   integer function special_dims(mode,ncid,zlen,name,extname,units, &
                                        n_dims,f_dims,bio_missing,var_ids)
!
! !DESCRIPTION:
! Here, the output of biogeochemical parameters either as ascii or as
! NetCDF files is managed.
!
! !USES:
   use netcdf
   use ncdf_common
#ifdef BFM_GOTM
   use bio_bfm, only: calc_sigma_depth
#endif

   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: mode
   integer, intent(in)                 :: ncid
   integer, intent(in)                 :: zlen
   character(*), intent(in)            :: name
   character(*), intent(in)            :: extname
   character(*), intent(in)            :: units
   integer, intent(in)                 :: n_dims
   integer, intent(in)                 :: f_dims(n_dims)
   REALTYPE,intent(IN)                 :: bio_missing
   integer, intent(inout)              :: var_ids
!
! !REVISION HISTORY:
!  Original author(s): Piet Ruardij
!
! !LOCAL VARIABLES:
   logical, save             :: first=.true.
   integer, save             :: nn
   REALTYPE,parameter        :: ddu=2.0
   REALTYPE                  :: zz
   integer                   :: dims(1)
   integer                   :: dims4(4)
   integer                   :: i,j,n,status,altZ_id,dim_altZ
   REALTYPE                  :: darr(1:zlen-1)
   REAL*4                   :: arr(1:zlen)
   character(len=30)         :: altZ,altZ_longname
   character(len=6)          :: dum,alt_unit
   REALTYPE                  :: fv,mv,vr(2)
   REAL*4                  :: vals(2)
!EOP
#ifdef BFM_GOTM
   select case (mode)
     case (1)
      vals(1) = 1.0
       status = nf90_put_att(ncid,var_ids,'averaged',vals)
       if (status.ne.NF90_NOERR) goto 10
     case (2)
       if ( index(extname,'__Z' ) ==1 ) then
          j=index(extname,':')-1
          read(extname(1:j),*) dum,altZ, zz,alt_unit, altZ_longname
          status = nf90_inq_dimid(ncid, altZ, dim_altZ)
          if (status.ne.NF90_NOERR) then
            status=nf90_def_dim(ncid,altZ,zlen,dim_altZ)
            if (status.eq.NF90_NOERR) then
               dims(1)=dim_altZ
               status = nf90_def_var(ncid,altZ,NF90_REAL,dims,altZ_id)
               if (status.eq.NF90_NOERR) then
                  i=len_trim(altZ_longname);
                  i=index(extname(1:j),altZ_longname(1:i))
                  call set_attributes(ncid,altZ_id,units=trim(alt_unit))
                  arr(1)=0.0;
                  call calc_sigma_depth(zlen-1,ddu,zz,darr)
                  arr(2:zlen)=darr(1:zlen-1)
                  status = nf90_enddef(ncid)
                  if (status.ne.NF90_NOERR) goto 10
                  status = nf90_put_var(ncid,altZ_id,arr(1:zlen))
                  if (status.ne.NF90_NOERR) goto 10
                  status = nf90_redef(ncid)
                  if (status.ne.NF90_NOERR) goto 10
               else
                  goto 10
               endif
            endif
          endif
          if ( var_ids.eq.0 ) goto 10
          dims4(1:n_dims)=f_dims(1:n_dims)
          dims4(3)=dim_altZ;
          status = nf90_def_var(ncid,name,NF90_FLOAT,dims4,var_ids)
          if (status.ne.NF90_NOERR) goto 10
          call set_attributes(ncid,var_ids,units=trim(units))
          call set_attributes(ncid,var_ids,long_name=trim(extname(j+2:)))
          fv = bio_missing; mv = bio_missing
          vr(1) = _ZERO_; vr(2) = 1.0D+10
          call set_attributes(ncid,var_ids, &
               FillValue=fv,missing_value=mv,valid_range=vr)
       endif
   end select
   special_dims=NF90_NOERR
   return
   10    special_dims=status
#endif
   end function 
#endif

!-----------------------------------------------------------------------
! Copyright (C) 2001 - Hans Burchard and Karsten Bolding (BBH)         !
!-----------------------------------------------------------------------
