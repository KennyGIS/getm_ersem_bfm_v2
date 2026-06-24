#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: variables_3d - global 3D related variables \label{sec-variables-3d}
!
! !INTERFACE:
   module variables_3d
!
! !DESCRIPTION:
!  This modules contains declarations for all variables related to 3D
!  hydrodynamical calculations. Information about the calculation domain
!  is included from the {\tt domain} module.
!  The variables are either statically defined in {\tt static\_3d.h} or
!  dynamically allocated in {\tt dynamic\_declarations\_3d.h}.
!  The variables which need to be declared have the following dimensions,
!  units and meanings:
!
! \vspace{0.5cm}
! \begin{supertabular}{llll}
! {\tt kmin} & 2D & [-] & lowest index in T-point \\
! {\tt kumin} & 2D &[-]  & lowest index in U-point \\
! {\tt kvmin} & 2D &[-]  & lowest index in V-point \\
! {\tt kmin\_pmz} & 2D &[-]  & lowest index in T-point (poor man's
! $z$-coordinate)\\
! {\tt kumin\_pmz} & 2D &[-]  & lowest index in U-point (poor man's
! $z$-coordinate)\\
! {\tt kvmin\_pmz} & 2D &[-]  & lowest index in V-point (poor man's
! $z$-coordinate)\\
! {\tt uu} & 3D & [m$^2$s$^{-1}$] & layer integrated $u$ transport
! $p_k$\\
! {\tt vv} & 3D & [m$^2$s$^{-1}$] & layer integrated $v$ transport
! $q_k$\\
! {\tt ww} & 3D & [m\,s$^{-1}$] & grid-related vertical velocity
! $\bar w_k$\\
! {\tt ho} & 3D & [m] & old layer height in T-point \\
! {\tt hn} & 3D & [m]& new layer height in T-point \\
! {\tt huo} & 3D &[m]& old layer height in U-point \\
! {\tt hun} & 3D & [m]& new layer height in U-point \\
! {\tt hvo} & 3D & [m]& old layer height in V-point \\
! {\tt hvn} & 3D & [m]& new layer height in V-point \\
! {\tt hcc} & 3D &[-] & hydrostatic consistency index in T-points\\
! {\tt uuEx} & 3D & [m$^2$s$^{-2}$] & sum of advection and
! diffusion for $u$-equation\\
! {\tt vvEx} & 3D &  [m$^2$s$^{-2}$]& sum of advection and
! diffusion for $v$-equation\\
! {\tt num} & 3D &  [m$^2$s$^{-1}$]& eddy viscosity on $w$-points
! $\nu_t$\\
! {\tt nuh} & 3D &  [m$^2$s$^{-1}$]& eddy diffusivity on $w$-points $\nu'_t$\\
! {\tt tke} & 3D &  [m$^2$s$^{-2}$]& turbulent kinetic energy $k$\\
! {\tt eps} & 3D &  [m$^2$s$^{-3}$]& turbulent dissipation rate
! $\eps$ \\
! {\tt SS} & 3D & [s$^{-2}$]& shear-frequency squared $M^2$ \\
! {\tt NN} & 3D &  [s$^{-2}$]& Brunt-V\"ais\"al\"a frequency squared$N^2$ \\
! {\tt S} & 3D & [psu] & salinity $S$ \\
! {\tt T} & 3D & [$^{\circ}$C]& potential temperature $\theta$ \\
! {\tt rad} & 3D & [Wm$^{-2}$]& Short wave penetration \\
! {\tt rho} & 3D & [kg\,m$^{-3}$]& density $rho$ \\
! {\tt buoy} & 3D & [m\,s$^{-2}$]& buoyancy $b$ \\
! {\tt idpdx} & 3D & [m$^2$s$^{-2}$] & $x$-component of internal
! pressure gradient \\
! {\tt idpdy} & 3D & [m$^2$s$^{-2}$]& $y$-component of internal
! pressure gradient\\
! {\tt spm} & 3D & [kg\,m$^{-3}$] & suspended matter concentration \\
! {\tt spm\_ws} & 3D & [m\,s$^{-1}$] & settling velocity of
! suspended matter \\
! {\tt spm\_pool} & 2D & [kg\,m$^{-2}$] & bottom pool of suspended
! matter\\
! {\tt uadv} & 3D & [m\,s$^{-1}$] & interpolated $x$-component of
! momentum advection velocity \\
! {\tt vadv} & 3D &  [m\,s$^{-1}$]& interpolated $y$-component of
! momentum advection velocity \\
! {\tt wadv} & 3D &  [m\,s$^{-1}$]& interpolated  vertical component of
! momentum advection velocity \\
! {\tt huadv} & 3D &[m] & interpolated height of advective flux
! layer ($x$-component) \\
! {\tt hvadv} & 3D &[m] & interpolated height of advective flux
! layer ($y$-component) \\
! {\tt hoadv} & 3D &[m] & old height of advective finite volume cell
! \\
! {\tt hnadv} & 3D &[m] & new height of advective finite volume
! cell\\
! {\tt sseo} & 2D & [m]& sea surface elevation before macro time
! step (T-point)\\
! {\tt ssen} & 2D & [m]& sea surface elevation after macro time
! step (T-point)\\
! {\tt ssuo} & 2D & [m]& sea surface elevation before macro time
! step (U-point)\\
! {\tt ssun} & 2D & [m]&sea surface elevation after macro time step
! (U-point)\\
! {\tt ssvo} & 2D & [m]& sea surface elevation before macro time
! step (V-point)\\
! {\tt ssvn} & 2D & [m]& sea surface elevation after macro time
! step (V-point)\\
! {\tt rru} & 2D & [m\,s$^{-1}$]&drag coefficient times curret speed
! in U-point\\
! {\tt rrv} & 2D & [m\,s$^{-1}$]&drag coefficient times curret speed
! in V-point\\
! {\tt taus} & 2D & [m$^2$s$^{-2}$]& normalised surface stress
! (T-point) \\
! {\tt taub} & 2D & [m$^2$s$^{-2}$]& normalised bottom stress
! (T-point) \\
! \end{supertabular}
!
! \vspace{0.5cm}
!
! It should be noted that depending on compiler options and runtype not
! all these variables are defined.
!
! The module contains public subroutines to initialise (see
! {\tt init\_variables\_3d}) and cleanup (see {\tt clean\_variables\_3d}).
!
! !USES:
   use domain, only: imin,imax,jmin,jmax,kmax,az,bottfric_method,rdrag
   use waves , only: waveforcing_method,waves_method,NO_WAVES,WAVES_VF
   use waves , only: waves_bbl_method,NO_WBBL
   IMPLICIT NONE
!
! !PUBLIC DATA MEMBERS:
   integer, parameter                  :: rk = kind(_ONE_)
   REALTYPE                            :: dt,cnpar=0.9
   REALTYPE                            :: avmback=_ZERO_,avhback=_ZERO_
   logical                             :: deformC_3d=.false.
   logical                             :: deformX_3d=.false.
   logical                             :: deformUV_3d=.false.
   logical                             :: calc_stirr=.false.
   logical                             :: save_phymix_3d=.false.
   logical                             :: do_numerical_analyses=.false.
   logical                             :: save_Sfluxu=.false.
   logical                             :: save_Sfluxv=.false.
   logical                             :: save_Sfluxw=.false.
   logical                             :: save_Tfluxu=.false.
   logical                             :: save_Tfluxv=.false.
   logical                             :: save_Tfluxw=.false.
   logical                             :: save_numdis_3d=.false.
   logical                             :: save_phydis_3d=.false.
   logical                             :: save_nummix_S=.false.
   logical                             :: save_phymix_S=.false.
   logical                             :: save_nummix_T=.false.
   logical                             :: save_phymix_T=.false.
!
#ifdef STATIC
#include "static_3d.h"
#else
#include "dynamic_declarations_3d.h"
#endif

!  the following fields will be allocated for waves
   REALTYPE, dimension(:,:,:), pointer,contiguous :: uuEuler=>NULL(),vvEuler=>NULL()
   REALTYPE, dimension(:,:,:), pointer,contiguous :: veluEuler3d=>NULL(),velvEuler3d=>NULL()
   REALTYPE, dimension(:,:,:), pointer,contiguous :: veluf3d=>NULL(),velvf3d=>NULL()
   REALTYPE, dimension(:,:  ), pointer,contiguous :: UEulerAdv=>NULL(),VEulerAdv=>NULL()
   REALTYPE, dimension(:,:  ), pointer,contiguous :: veluEulerAdv=>NULL(),velvEulerAdv=>NULL()
   REALTYPE, dimension(:,:  ), pointer,contiguous :: veluAdvf=>NULL(),velvAdvf=>NULL()
   REALTYPE, dimension(:,:  ), pointer,contiguous :: taubmax_3d=>NULL()

   REALTYPE,dimension(:,:,:),pointer,contiguous :: Sfluxu=>null()
   REALTYPE,dimension(:,:,:),pointer,contiguous :: Sfluxv=>null()
   REALTYPE,dimension(:,:,:),pointer,contiguous :: Sfluxw=>null()
   REALTYPE,dimension(:,:,:),pointer,contiguous :: Tfluxu=>null()
   REALTYPE,dimension(:,:,:),pointer,contiguous :: Tfluxv=>null()
   REALTYPE,dimension(:,:,:),pointer,contiguous :: Tfluxw=>null()

!  KK-TODO: make *dis_3d allocatable
   REALTYPE,dimension(:,:,:),pointer,contiguous :: numdis_3d=>null()
   REALTYPE,dimension(:,:,:),pointer,contiguous :: phydis_3d=>null()
   REALTYPE,dimension(:,:,:),pointer,contiguous :: nummix_S=>null()
   REALTYPE,dimension(:,:,:),pointer,contiguous :: nummix_T=>null()
   REALTYPE,dimension(:,:,:),pointer,contiguous :: phymix_S=>null()
   REALTYPE,dimension(:,:,:),pointer,contiguous :: phymix_T=>null()

   REALTYPE,public,dimension(:,:),allocatable,target :: sst,sss

!  the following fields will only be allocated if deformCX_3d=.true.
   REALTYPE,dimension(:,:,:),allocatable :: dudxC_3d,dvdyC_3d
   REALTYPE,dimension(:,:,:),pointer,contiguous :: dudyX_3d=>null()
   REALTYPE,dimension(:,:,:),pointer,contiguous :: dvdxX_3d=>null()
   REALTYPE,dimension(:,:,:),allocatable :: shearX_3d

!  the following fields will only be allocated if deformUV_3d=.true.
   REALTYPE,dimension(:,:,:),allocatable :: dudxV_3d,dvdyU_3d,shearU_3d

!  the following fields will only be allocated if calc_stirring=.true.
   REALTYPE,dimension(:,:,:),allocatable :: diffxx,diffxy,diffyx,diffyy

!  the following fields will be allocated in init_nonhydrostatic
   REALTYPE,dimension(:,:,:),allocatable,target :: minus_bnh
   REALTYPE,dimension(:,:,:),allocatable :: wco

!  the following fields will be allocated in init_internal_pressure
   REALTYPE,dimension(:,:,:),pointer            :: idpdx,idpdy
   REALTYPE,dimension(:,:,:),allocatable,target :: idpdx_hs,idpdy_hs
   REALTYPE,dimension(:,:,:),allocatable,target :: idpdx_nh,idpdy_nh
   REALTYPE,dimension(:,:,:),allocatable,target :: idpdx_full,idpdy_full

#ifdef GETM_BIO
#ifndef BFM_GOTM
   REALTYPE, allocatable               :: cc3d(:,:,:,:)
   REALTYPE, allocatable               :: ws3d(:,:,:,:)
#endif
#endif
#ifdef _FABM_
   REALTYPE, allocatable, dimension(:,:,:,:) :: fabm_pel,fabm_diag
   REALTYPE, allocatable, dimension(:,:,:)   :: fabm_ben,fabm_diag_hz
#endif
   integer                             :: size3d_field
   integer                             :: mem3d
   integer                             :: preadapt
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
! !IROUTINE: init_variables_3d - initialise 3D related stuff
! \label{sec-init-variables}
!
! !INTERFACE:
   subroutine init_variables_3d(runtype)
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: runtype
!
! !DESCRIPTION:
!  Dynamic allocation of memory for 3D related fields via
!  {\tt dynamic\_allocations\_3d.h} (unless the compiler option
!  {\tt STATIC} is set). Furthermore, most variables are initialised here.
!
! !LOCAL VARIABLES:
   integer                   :: i,j, rc
!EOP
!-------------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'init_variables_3d() # ',Ncall
#endif

   LEVEL2 'init_variables_3d'
   size3d_field=((imax+HALO)-(imin+HALO)+1)*        &
                ((jmax+HALO)-(jmin+HALO)+1)*(kmax+1)
   mem3d=n3d_fields*size3d_field*REAL_SIZE

!  Allocates memory for the public data members - if not static
#ifndef STATIC
#include "dynamic_allocations_3d.h"
#endif

   kmin = 1

   ho = _ZERO_ ; hn = _ZERO_ ; hvel = _ZERO_ ; hun = _ZERO_ ; hvn = _ZERO_
   uu = _ZERO_ ; vv = _ZERO_ ; ww = _ZERO_
   velu3d = _ZERO_  ; velv3d = _ZERO_
   velx3d = -9999.0 ; vely3d = -9999.0 ; w = -9999.0
   veluAdv = _ZERO_ ; velvAdv = _ZERO_
   velxAdv = -9999.0 ; velyAdv = -9999.0
   forall(i=imin-HALO:imax+HALO, j=jmin-HALO:jmax+HALO, az(i,j).ne.0)
      velx3d (i,j,1:kmax) = _ZERO_
      vely3d (i,j,1:kmax) = _ZERO_
      w      (i,j,1:kmax) = _ZERO_
      velxAdv(i,j)        = _ZERO_
      velyAdv(i,j)        = _ZERO_
   end forall

#ifdef _MOMENTUM_TERMS_
   tdv_u = _ZERO_ ; adv_u = _ZERO_ ; vsd_u = _ZERO_ ; hsd_u = _ZERO_
   cor_u = _ZERO_ ; epg_u = _ZERO_ ; ipg_u = _ZERO_
   tdv_v = _ZERO_ ; adv_v = _ZERO_ ; vsd_v = _ZERO_ ; hsd_v = _ZERO_
   cor_v = _ZERO_ ; epg_v = _ZERO_ ; ipg_v = _ZERO_
#endif
   ssen = _ZERO_ ; ssun = _ZERO_ ; ssvn = _ZERO_
   Dn = _ZERO_ ; Dveln = _ZERO_ ; Dun = _ZERO_ ; Dvn = _ZERO_
   Uadv = _ZERO_ ; Vadv = _ZERO_

   uuEuler      => uu      ; vvEuler      => vv
   veluEuler3d  => velu3d  ; velvEuler3d  => velv3d
   veluf3d      => velu3d  ; velvf3d      => velv3d
   UEulerAdv    => Uadv    ; VEulerAdv    => Vadv
   veluEulerAdv => veluAdv ; velvEulerAdv => velvAdv
   veluAdvf     => veluAdv ; velvAdvf     => velvAdv
   taubmax_3d => taub


   if (waveforcing_method .ne. NO_WAVES) then

      allocate(uuEuler(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'init_3d: Error allocating memory (uuEuler)'
      uuEuler = _ZERO_
      allocate(vvEuler(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'init_3d: Error allocating memory (vvEuler)'
      vvEuler = _ZERO_

      allocate(veluEuler3d(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'init_2d: Error allocating memory (veluEuler3d)'
      veluEuler3d = _ZERO_
      allocate(velvEuler3d(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'init_2d: Error allocating memory (velvEuler3d)'
      velvEuler3d = _ZERO_

      allocate(UEulerAdv(I2DFIELD),stat=rc)
      if (rc /= 0) stop 'init_3d: Error allocating memory (UEulerAdv)'
      UEulerAdv = _ZERO_
      allocate(VEulerAdv(I2DFIELD),stat=rc)
      if (rc /= 0) stop 'init_3d: Error allocating memory (VEulerAdv)'
      VEulerAdv = _ZERO_

      allocate(veluEulerAdv(I2DFIELD),stat=rc)
      if (rc /= 0) stop 'init_3d: Error allocating memory (veluEulerAdv)'
      veluEulerAdv = _ZERO_
      allocate(velvEulerAdv(I2DFIELD),stat=rc)
      if (rc /= 0) stop 'init_3d: Error allocating memory (velvEulerAdv)'
      velvEulerAdv = _ZERO_

      if (waves_method .eq. WAVES_VF) then
         veluf3d  => veluEuler3d  ; velvf3d  => velvEuler3d
         veluAdvf => veluEulerAdv ; velvAdvf => velvEulerAdv
      end if

      if (waves_bbl_method .ne. NO_WBBL) then
         allocate(taubmax_3d(I2DFIELD),stat=rc)
         if (rc /= 0) stop 'init_3d: Error allocating memory (taubmax_3d)'
      end if

   end if


   zub = -9999.0 ; zvb = -9999.0 ! must be initialised for gotm
   if (bottfric_method .eq. 1) then
      rru = rdrag
      rrv = rdrag
   else
      rru = _ZERO_
      rrv = _ZERO_
   end if

   uuEx= _ZERO_ ; vvEx= _ZERO_
   SS=_ZERO_
   tke=1.e-10 ; eps=1.e-10
   preadapt=0

#ifndef NO_BAROCLINIC
   NN=_ZERO_
   rad=_ZERO_
   heatflux_net = _ZERO_
   light=_ONE_
   bioshade = _ONE_
#endif

!  must be nonzero for gotm_fabm in case of calc_temp=F
   g1 = -9999._rk
   g2 = -9999._rk

#ifdef STRUCTURE_FRICTION
   sf = _ZERO_
#endif

#ifdef DEBUG
   write(debug,*) 'Leaving init_variables_3d()'
   write(debug,*)
#endif
   return
   end subroutine init_variables_3d
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: postinit_variables_3d - re-initialise some 3D stuff.
!
! !INTERFACE:
   subroutine postinit_variables_3d(update_temp,update_salt)
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   logical, intent(in)                 :: update_temp,update_salt
!
! !DESCRIPTION:
!
! !LOCAL VARIABLES:
   integer                   :: rc
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'postinit_variables_3d() # ',Ncall
#endif

!  must be in postinit because flags are set init_getm_fabm
   if (deformC_3d) then
      allocate(dudxC_3d(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'postinit_variables_3d: Error allocating memory (dudxC_3d)'
      dudxC_3d=_ZERO_
#ifndef SLICE_MODEL
      allocate(dvdyC_3d(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'postinit_variables_3d: Error allocating memory (dvdyC_3d)'
      dvdyC_3d=_ZERO_
#endif
   end if
   if (deformX_3d) then
      allocate(shearX_3d(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'postinit_variables_3d: Error allocating memory (shearX_3d)'
      shearX_3d=_ZERO_

      if (save_phydis_3d) then
            allocate(dvdxX_3d(I3DFIELD),stat=rc)
            if (rc /= 0) stop 'postinit_variables_3d: Error allocating memory (dvdxX_3d)'
            dvdxX_3d=_ZERO_
#ifndef SLICE_MODEL
            allocate(dudyX_3d(I3DFIELD),stat=rc)
            if (rc /= 0) stop 'postinit_variables_3d: Error allocating memory (dudyX_3d)'
            dudyX_3d=_ZERO_
#endif
      end if
   end if
   if (deformUV_3d) then
      allocate(dudxV_3d(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'postinit_variables_3d: Error allocating memory (dudxV_3d)'
      dudxV_3d=_ZERO_

#ifndef SLICE_MODEL
      allocate(dvdyU_3d(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'postinit_variables_3d: Error allocating memory (dvdyU_3d)'
      dvdyU_3d=_ZERO_
#endif

      allocate(shearU_3d(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'postinit_variables_3d: Error allocating memory (shearU_3d)'
      shearU_3d=_ZERO_
   end if
   if (calc_stirr) then
      allocate(diffxx(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'postinit_variables_3d: Error allocating memory (diffxx)'
      diffxx=_ZERO_

#ifndef SLICE_MODEL
      allocate(diffxy(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'postinit_variables_3d: Error allocating memory (diffxy)'
      diffxy=_ZERO_

      allocate(diffyx(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'postinit_variables_3d: Error allocating memory (diffyx)'
      diffyx=_ZERO_

      allocate(diffyy(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'postinit_variables_3d: Error allocating memory (diffyy)'
      diffyy=_ZERO_
#endif
   end if

   if (save_Sfluxu) then
      allocate(Sfluxu(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'postinit_3d: Error allocating memory (Sfluxu)'
      Sfluxu = _ZERO_
   end if
   if (save_Sfluxv) then
      allocate(Sfluxv(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'postinit_3d: Error allocating memory (Sfluxv)'
      Sfluxv = _ZERO_
   end if
   if (save_Sfluxw) then
      allocate(Sfluxw(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'postinit_3d: Error allocating memory (Sfluxw)'
      Sfluxw = _ZERO_
   end if
   if (save_Tfluxu) then
      allocate(Tfluxu(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'postinit_3d: Error allocating memory (Tfluxu)'
      Tfluxu = _ZERO_
   end if
   if (save_Tfluxv) then
      allocate(Tfluxv(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'postinit_3d: Error allocating memory (Tfluxv)'
      Tfluxv = _ZERO_
   end if
   if (save_Tfluxw) then
      allocate(Tfluxw(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'postinit_3d: Error allocating memory (Tfluxw)'
      Tfluxw = _ZERO_
   end if

!  must be in postinit because do_numerical_analyses is set in init_output
!  Note (KK): only for bwd compatibility with old output
   if (do_numerical_analyses) then
      save_numdis_3d = .true.
      save_phymix_T  = update_temp
      save_nummix_T  = update_temp
      save_phymix_S  = update_salt
      save_nummix_S  = update_salt
   end if

   if (save_phydis_3d) then
      allocate(phydis_3d(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'postinit_3d: Error allocating memory (phydis_3d)'
      phydis_3d = _ZERO_
   end if
   if (save_numdis_3d) then
      allocate(numdis_3d(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'postinit_3d: Error allocating memory (numdis_3d)'
      numdis_3d = _ZERO_
   end if

   if (save_phymix_T) then
      save_phymix_3d=.true.
      allocate(phymix_T(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'postinit_3d: Error allocating memory (phymix_T)'
      phymix_T = _ZERO_
   end if
   if (save_nummix_T) then
      allocate(nummix_T(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'postinit_3d: Error allocating memory (nummix_T)'
      nummix_T = _ZERO_
   end if

   if (save_phymix_S) then
      save_phymix_3d=.true.
      allocate(phymix_S(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'postinit_3d: Error allocating memory (phymix_S)'
      phymix_S = _ZERO_
   end if
   if (save_nummix_S) then
      allocate(nummix_S(I3DFIELD),stat=rc)
      if (rc /= 0) stop 'postinit_3d: Error allocating memory (nummix_S)'
      nummix_S = _ZERO_
   end if

#ifdef DEBUG
   write(debug,*) 'Leaving postinit_variables_3d()'
   write(debug,*)
#endif
   return
   end subroutine postinit_variables_3d
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: register_3d_variables() - register GETM variables.
!
! !INTERFACE:
   subroutine register_3d_variables(fm,runtype,update_temp,update_salt)
!
! !DESCRIPTION:
!
! !USES:
   use field_manager
   use meteo       , only: fwf_method
   use variables_2d, only: fwf_int
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   type (type_field_manager) :: fm
   integer, intent(in)       :: runtype
   logical, intent(in)       :: update_temp,update_salt
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Jorn Bruggeman
!
! !LOCAL VARIABLES:
!
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL2 'register_3d_variables()'

!:: kmin(I2DFIELD)
!:: kumin(I2DFIELD)
!:: kvmin(I2DFIELD)
!:: kmin_pmz(I2DFIELD)
!:: kumin_pmz(I2DFIELD)
!:: kvmin_pmz(I2DFIELD)

!:: uu(I3DFIELD)
!:: vv(I3DFIELD)
!:: ww(I3DFIELD)
#ifdef _MOMENTUM_TERMS_
!:: tdv_u(I3DFIELD)
!:: adv_u(I3DFIELD)
!:: vsd_u(I3DFIELD)
!:: hsd_u(I3DFIELD)
!:: cor_u(I3DFIELD)
!:: epg_u(I3DFIELD)
!:: ipg_u(I3DFIELD)

!:: tdv_v(I3DFIELD)
!:: adv_v(I3DFIELD)
!:: vsd_v(I3DFIELD)
!:: hsd_v(I3DFIELD)
!:: cor_v(I3DFIELD)
!:: epg_v(I3DFIELD)
!:: ipg_v(I3DFIELD)
#endif
#ifdef STRUCTURE_FRICTION
!:: sf(I3DFIELD)
#endif
!:: ho(I3DFIELD)
!:: hn(I3DFIELD)
!:: huo(I3DFIELD)
!:: hun(I3DFIELD)
!:: hvo(I3DFIELD)
!:: hvn(I3DFIELD)
!:: hcc(I3DFIELD)
!:: uuEx(I3DFIELD)
!:: vvEx(I3DFIELD)
!:: num(I3DFIELD)
!:: nuh(I3DFIELD)

! 3D turbulent fields
!:: tke(I3DFIELD)
!:: eps(I3DFIELD)
!:: SS(I3DFIELD)
#ifndef NO_BAROCLINIC
! 3D baroclinic fields
!:: NN(I3DFIELD)
!:: S(I3DFIELD)
!:: T(I3DFIELD)
!:: rho(I3DFIELD)
!:: rad(I3DFIELD)
!:: buoy(I3DFIELD)
!:: alpha(I3DFIELD)
!:: beta(I3DFIELD)
!:: idpdx(I3DFIELD)
!:: idpdy(I3DFIELD)
!:: light(I3DFIELD)
#endif

#ifdef SPM
! suspended matter
!:: spm(I3DFIELD)
!:: spm_ws(I3DFIELD)
!:: spm_pool(I2DFIELD)
#endif

! 2D fields in 3D domain
!:: sseo(I2DFIELD)
!:: ssen(I2DFIELD)
!:: Dn(I2DFIELD)
!:: ssuo(I2DFIELD)
!:: ssun(I2DFIELD)
!:: ssvo(I2DFIELD)
!:: ssvn(I2DFIELD)
!:: Dun,Dvn

! 3D friction in 3D domain
!:: rru(I2DFIELD)
!:: rrv(I2DFIELD)
!:: taus(I2DFIELD)
!:: taubx(I2DFIELD)
!:: tauby(I2DFIELD)
!:: taub(I2DFIELD)

! light attenuation
!:: A(I2DFIELD)
!:: g1(I2DFIELD)
!:: g2(I2DFIELD)

!  category - 3d
   if (runtype .ge. 2) then
      call fm%register('zc', 'm', 'center coordinate', standard_name='', dimensions=(/id_dim_z/),data3d=zc(_3D_W_), category='grid', part_of_state=.false.)
      call fm%register('zcn', 'm', 'z', standard_name='', dimensions=(/id_dim_z/), data3d=zcn(_3D_W_), category='grid')
      call fm%register('hn', 'm', 'layer thickness', standard_name='cell_thickness', dimensions=(/id_dim_z/),data3d=hn(_3D_W_), category='grid', part_of_state=.true.)
      call fm%register('hun', 'm', 'layer thickness - U-points', standard_name='cell_thickness', dimensions=(/id_dim_z/),data3d=hun(_3D_W_), category='grid', output_level=output_level_debug)
      call fm%register('hvn', 'm', 'layer thickness - V-points', standard_name='cell_thickness', dimensions=(/id_dim_z/),data3d=hvn(_3D_W_), category='grid', output_level=output_level_debug)
      call fm%register('ho', 'm', 'old layer thickness', standard_name='cell_thickness', dimensions=(/id_dim_z/),data3d=ho(_3D_W_), category='grid', output_level=output_level_debug)
      call fm%register('ssen', 'm', 'elevation at T-points (3D)', standard_name='', data2d=ssen(_2D_W_), category='3d', fill_value=-9999.0_rk, output_level=output_level_debug, part_of_state=.true.)
      call fm%register('ssun', 'm', 'elevation at U-points (3D)', standard_name='', data2d=ssun(_2D_W_), category='3d', output_level=output_level_debug, part_of_state=.true.)
      call fm%register('ssvn', 'm', 'elevation at V-points (3D)', standard_name='', data2d=ssvn(_2D_W_), category='3d', output_level=output_level_debug, part_of_state=.true.)
      call fm%register('sseo', 'm', 'old elevation at T-points (3D)', standard_name='', data2d=sseo(_2D_W_), category='3d', fill_value=-9999.0_rk, output_level=output_level_debug, part_of_state=.true.)
      call fm%register('uu', 'm2/s', 'transport in local x-direction (3D)', standard_name='', dimensions=(/id_dim_z/), data3d=uu(_3D_W_), category='3d', output_level=output_level_debug, part_of_state=.true.)
      call fm%register('vv', 'm2/s', 'transport in local y-direction (3D)', standard_name='', dimensions=(/id_dim_z/), data3d=vv(_3D_W_), category='3d', output_level=output_level_debug, part_of_state=.true.)
      call fm%register('ww', 'm/s', 'grid-related vertical velocity', standard_name='', dimensions=(/id_dim_z/), data3d=ww(_3D_W_), category='3d', output_level=output_level_debug)
      call fm%register('velx3d', 'm/s', 'velocity in global x-direction (3D)', standard_name='', dimensions=(/id_dim_z/), data3d=velx3d(_3D_W_), category='3d', fill_value=-9999.0_rk, output_level=output_level_debug)
      call fm%register('vely3d', 'm/s', 'velocity in global y-direction (3D)', standard_name='', dimensions=(/id_dim_z/), data3d=vely3d(_3D_W_), category='3d', fill_value=-9999.0_rk, output_level=output_level_debug)
      call fm%register('w', 'm/s', 'vertical velocity', standard_name='', dimensions=(/id_dim_z/), data3d=w(_3D_W_), category='3d', fill_value=-9999.0_rk, output_level=output_level_debug)
      call fm%register('velxAdv', 'm/s', 'depth-avg. velocity in global x-direction (3D)', standard_name='', data2d=velxAdv(_2D_W_), category='3d', fill_value=-9999.0_rk, output_level=output_level_debug)
      call fm%register('velyAdv', 'm/s', 'depth-avg. velocity in global y-direction (3D)', standard_name='', data2d=velyAdv(_2D_W_), category='3d', fill_value=-9999.0_rk, output_level=output_level_debug)
      call fm%register('SS', 's-2', 'shear frequency squared', standard_name='', dimensions=(/id_dim_z/), data3d=SS(_3D_W_), category='3d', output_level=output_level_debug)

      if (associated(taubmax_3d)) then
      call fm%register('taubmax_3d', 'm2/s2', 'max. bottom stress', standard_name='', data2d=taubmax_3d(_2D_W_), category='3d', fill_value=-9999.0_rk, output_level=output_level_debug)
      end if

      if (allocated(minus_bnh)) then
      call fm%register('minus_bnh', 'm/s2', 'neg. nh buoyancy correction', standard_name='', dimensions=(/id_dim_z/), data3d=minus_bnh(_3D_W_), category='3d', output_level=output_level_debug)
      end if

      if (fwf_method .ge. 1) then
      call fm%register('fwf_int', 'm', 'surface freshwater fluxes (3D)', standard_name='', data2d=fwf_int(_2D_W_), category='3d', output_level=output_level_debug)
      end if
   end if

!  category - turbulence
   if (runtype .ge. 2) then
      call fm%register('tke' , 'm2/s2', 'TKE'        , standard_name='', dimensions=(/id_dim_z/), data3d=tke(_3D_W_), category='turbulence', output_level=output_level_debug)
      call fm%register('diss', 'm2/s3', 'dissipation', standard_name='', dimensions=(/id_dim_z/), data3d=eps(_3D_W_), category='turbulence', output_level=output_level_debug)
      call fm%register('num' , 'm2/s' , 'viscosity'  , standard_name='', dimensions=(/id_dim_z/), data3d=num(_3D_W_), category='turbulence', output_level=output_level_debug)
      call fm%register('nuh' , 'm2/s' , 'diffusivity', standard_name='', dimensions=(/id_dim_z/), data3d=nuh(_3D_W_), category='turbulence', output_level=output_level_debug)
   end if

#ifndef NO_BAROCLINIC
!  category - baroclinic
   if (runtype .ge. 3) then
      call fm%register('temp', 'Celsius', 'temperature', standard_name='', dimensions=(/id_dim_z/), fill_value=-9999.0_rk, data3d=T  (_3D_W_), category='baroclinic', part_of_state=.true.)
      call fm%register('salt', '1e-3'   , 'salinity'   , standard_name='', dimensions=(/id_dim_z/), fill_value=-9999.0_rk, data3d=S  (_3D_W_), category='baroclinic', part_of_state=.true.)
      call fm%register('rho' , 'kg/m3'  , 'density'    , standard_name='', dimensions=(/id_dim_z/), fill_value=-9999.0_rk, data3d=rho(_3D_W_), category='baroclinic', output_level=output_level_debug)
      call fm%register('NN', 's-2', 'buoyancy frequency squared', standard_name='', dimensions=(/id_dim_z/), data3d=NN(_3D_W_), category='baroclinic', output_level=output_level_debug)
      call fm%register('idpdx', 'm2/s2', 'baroclinic pressure gradient - x', standard_name='', dimensions=(/id_dim_z/),data3d=idpdx(_3D_W_), category='baroclinic', output_level=output_level_debug)
#ifndef SLICE_MODEL
      call fm%register('idpdy', 'm2/s2', 'baroclinic pressure gradient - y', standard_name='', dimensions=(/id_dim_z/),data3d=idpdy(_3D_W_), category='baroclinic', output_level=output_level_debug)
#endif
   end if
#endif

   if (update_salt) then
      call fm%register('Sfluxu', 'g/kg*m3/s', 'salt flux in local x-direction', dimensions=(/id_dim_z/), category='3d', output_level=output_level_debug, used=save_Sfluxu)
      call fm%register('Sfluxv', 'g/kg*m3/s', 'salt flux in local y-direction', dimensions=(/id_dim_z/), category='3d', output_level=output_level_debug, used=save_Sfluxv)
      call fm%register('Sfluxw', 'g/kg*m/s', 'vertical salt flux', dimensions=(/id_dim_z/), category='3d', output_level=output_level_debug, used=save_Sfluxw)
   end if
   if (update_temp) then
      call fm%register('Tfluxu', 'degree_C*m3/s', 'temp flux in local x-direction', dimensions=(/id_dim_z/), category='3d', output_level=output_level_debug, used=save_Tfluxu)
      call fm%register('Tfluxv', 'degree_C*m3/s', 'temp flux in local y-direction', dimensions=(/id_dim_z/), category='3d', output_level=output_level_debug, used=save_Tfluxv)
      call fm%register('Tfluxw', 'degree_C*m/s', 'vertical temp flux', dimensions=(/id_dim_z/), category='3d', output_level=output_level_debug, used=save_Tfluxw)
   end if

   call fm%register('numdis_3d', 'm*W/kg', 'numerical dissipation content (3D)', standard_name='', dimensions=(/id_dim_z/), category='3d', output_level=output_level_debug, used=save_numdis_3d)
   call fm%register('phydis_3d', 'W/kg', 'physical dissipation (3D)' , standard_name='', dimensions=(/id_dim_z/), category='3d', output_level=output_level_debug, used=save_phydis_3d)
   if (update_temp) then
      call fm%register('nummix_T', 'm*degC**2/s', 'numerical mixing content of temperature', standard_name='', dimensions=(/id_dim_z/), category='3d', output_level=output_level_debug, used=save_nummix_T)
      call fm%register('phymix_T', 'm*degC**2/s', 'physical mixing content of temperature' , standard_name='', dimensions=(/id_dim_z/), category='3d', output_level=output_level_debug, used=save_phymix_T)
   end if
   if (update_salt) then
      call fm%register('nummix_S', 'm*(g/kg)**2/s', 'numerical mixing content of salinity', standard_name='', dimensions=(/id_dim_z/), category='3d', output_level=output_level_debug, used=save_nummix_S)
      call fm%register('phymix_S', 'm*(g/kg)**2/s', 'physical mixing content of salinity' , standard_name='', dimensions=(/id_dim_z/), category='3d', output_level=output_level_debug, used=save_phymix_S)

   end if

   return
   end subroutine register_3d_variables
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: finalize_register_3d_variables() - send optional variables.
!
! !INTERFACE:
   subroutine finalize_register_3d_variables(fm,update_temp,update_salt)
!
! !DESCRIPTION:
!
! !USES:
   use field_manager
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   type (type_field_manager) :: fm
   logical, intent(in)       :: update_temp,update_salt
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Jorn Bruggeman
!
! !LOCAL VARIABLES:
!EOP
!-----------------------------------------------------------------------
!BOC
   LEVEL1 'finalize_register_3d_variables()'

   if (associated(Sfluxu)) call fm%send_data('Sfluxu', Sfluxu(_3D_W_))
   if (associated(Sfluxv)) call fm%send_data('Sfluxv', Sfluxv(_3D_W_))
   if (associated(Sfluxw)) call fm%send_data('Sfluxw', Sfluxw(_3D_W_))
   if (associated(Tfluxu)) call fm%send_data('Tfluxu', Tfluxu(_3D_W_))
   if (associated(Tfluxv)) call fm%send_data('Tfluxv', Tfluxv(_3D_W_))
   if (associated(Tfluxw)) call fm%send_data('Tfluxw', Tfluxw(_3D_W_))
   if (associated(numdis_3d)) call fm%send_data('numdis_3d', numdis_3d(_3D_W_))
   if (associated(phydis_3d)) call fm%send_data('phydis_3d', phydis_3d(_3D_W_))
   if (associated(nummix_T )) call fm%send_data('nummix_T' , nummix_T (_3D_W_))
   if (associated(phymix_T )) call fm%send_data('phymix_T' , phymix_T (_3D_W_))
   if (associated(nummix_S )) call fm%send_data('nummix_S' , nummix_S (_3D_W_))
   if (associated(phymix_S )) call fm%send_data('phymix_S' , phymix_S (_3D_W_))

   return
   end subroutine finalize_register_3d_variables
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: clean_variables_3d - cleanup after 3D run.
!
! !INTERFACE:
   subroutine clean_variables_3d()
   IMPLICIT NONE
!
! !DESCRIPTION:
!  This routine cleans up after a 3D integrationby doing nothing so far.
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'clean_3d() # ',Ncall
#endif

! Deallocates memory for the public data members

#ifdef DEBUG
     write(debug,*) 'Leaving clean_variables_3d()'
     write(debug,*)
#endif
   return
   end subroutine clean_variables_3d
!EOC

!-----------------------------------------------------------------------

   end module variables_3d

!-----------------------------------------------------------------------
! Copyright (C) 2001 - Hans Burchard and Karsten Bolding (BBH)         !
!-----------------------------------------------------------------------
