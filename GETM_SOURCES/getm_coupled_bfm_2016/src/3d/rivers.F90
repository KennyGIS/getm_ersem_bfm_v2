#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE:  rivers \label{sec-rivers}
!
! !INTERFACE:
   module rivers
!
! !DESCRIPTION:
!
!  This module includes support for river input. Rivers are treated the same
!  way as meteorology, i.e.\ as external module to the hydrodynamic model
!  itself.
!  The module follows the same scheme as all other modules, i.e.\
!  {\tt init\_rivers}
!  sets up necessary information, and {\tt do\_rivers} updates
!  the relevant variables.
!  {\tt do\_river} is called in {\tt getm/integration.F90}
!  between the {\tt 2d} and {\tt 3d} routines as it only
!  updates the sea surface elevation (in {\tt 2d}) and sea surface elevation,
!  and
!  optionally salinity and temperature (in {\tt 3d}).
!  At present the momentum of the river water is not include, the model
!  however has a direct response to the river water because of the
!  pressure gradient introduced.
!
! !USES:
   use domain, only: imin,jmin,imax,jmax,ioff,joff
   use domain, only: H,az,kmax,arcd1
   use time, only: write_time_string,timestr
   use variables_2d, only: dtm,z
#ifndef NO_BAROCLINIC
   use m3d, only: update_salt,update_temp
   use variables_3d, only: hn,ssen,T,S
#endif
#ifdef GETM_BIO
   use bio, only: bio_calc
   use bio_var, only: numc
#ifdef BFM_GOTM
   use bio_var, only: c1dimnumc,cc !AN
   use variables_bio_3d, only: cc3d
#else
   use variables_3d, only: cc3d
#endif
#endif
#ifdef _FABM_
   use getm_fabm, only: model,fabm_pel
#endif
   IMPLICIT NONE
!
   private
!
! !PUBLIC DATA MEMBERS:
   public init_rivers, do_rivers, clean_rivers
#ifdef GETM_BIO
   public init_rivers_bio
#endif
#ifdef _FABM_
   public init_rivers_fabm
#endif
   integer, public                     :: river_method=0,nriver=0,rriver=0
   logical,public                      :: use_river_temp = .false.
   logical,public                      :: use_river_salt = .false.
   character(len=64), public           :: river_data="rivers.nc"
   character(len=64), public, allocatable  :: river_name(:)
   character(len=64), public, allocatable  :: real_river_name(:)
   integer, public, allocatable        :: ok(:)
   REALTYPE, public, allocatable       :: river_flow(:)
   REALTYPE, public, allocatable       :: river_salt(:)
   REALTYPE, public, allocatable       :: river_temp(:)
   integer, public                     :: river_ramp= -1
   REALTYPE, public                    :: river_factor= _ONE_
   REALTYPE, public,parameter          :: temp_missing=-9999.0
   REALTYPE, public,parameter          :: salt_missing=-9999.0
   integer,  public, allocatable       :: river_split(:)
#ifdef GETM_BIO
   REALTYPE, public, allocatable       :: river_bio(:,:)
   REALTYPE, public, parameter         :: bio_missing=-9999.0
#ifdef BFM_GOTM
   REALTYPE, public, allocatable       :: river_min(:),river_max(:)
   integer,public,allocatable          :: river_func(:)                !concentration is calculated using a function!
   REALTYPE, public, allocatable       :: river_frc(:)              !one can modify concentrations#endif
   REALTYPE, public, allocatable       :: river_add(:)              !one can modify concentrations
   integer, public, allocatable        :: seq_pointer_river(:)      !on the rivers #endif
   integer, public                     :: nriver_sa=0
#endif
#endif
#ifdef _FABM_
   REALTYPE, public, allocatable       :: river_fabm(:,:)
   integer, public, allocatable        :: seq_pointer_river(:)      !on the rivers 
#endif
!
! !PRIVATE DATA MEMBERS:
   integer                   :: river_format=2
   character(len=64)         :: river_info="riverinfo.dat"
   integer, allocatable      :: ir(:),jr(:)
   REALTYPE, allocatable     :: rzl(:),rzu(:)
   REALTYPE, allocatable     :: irr(:)
   REALTYPE, allocatable     :: macro_height(:)
   REALTYPE, allocatable     :: flow_fraction(:),flow_fraction_rel(:)
   REALTYPE                  :: ramp=_ONE_
   logical                   :: ramp_is_active=.false.
   logical                   :: river_outflow_properties_follow_source_cell=.true.
#ifdef BFM_GOTM
   character(len=64)         :: river_sa="river-sa.dat"
#endif
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
! !IROUTINE: init_rivers
!
! !INTERFACE:
   subroutine init_rivers(hotstart)
!
! !DESCRIPTION:
!
! First of all, the namelist {\tt rivers} is read from getm.F90 and
! a number of vectors with the length of {\tt nriver} (number of
! rivers) is allocated. Then, by looping over all rivers, the
! ascii file {\tt river\_info} is read, and checked for consistency.
! The number of used rivers {\tt rriver} is calculated and it is checked
! whether they are on land (which gives a warning) or not. When a river name
! occurs more than once in {\tt river\_info}, it means that its runoff
! is split among several grid boxed (for wide river mouths).
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   logical,intent(in)        :: hotstart
!
! !LOCAL VARIABLES:
   integer                   :: i,j,n,nn,ni,rc,m,iriver,jriver,numcells
   logical                   :: outside,outsidehalo
   REALTYPE                  :: bathy, area, total_weight
   character(len=255)        :: line,xxx
   NAMELIST /rivers/ &
            river_method,river_info,river_format,river_data,river_ramp, &
            river_factor,use_river_salt,use_river_temp,river_outflow_properties_follow_source_cell
!EOP
!-------------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'init_rivers() # ',Ncall
#endif

   LEVEL1 'init_rivers()'
   read(NAMLST,rivers)

   select case (river_method)
      case (0)
         LEVEL3 'River runoff not included.'
      case (1,2)
         LEVEL2 'river_method= ',river_method
         LEVEL2 'river_data=   ',trim(river_data)
         LEVEL2 'river_format= ',river_format
         if (river_ramp .gt. 1) then
            LEVEL2 'river_ramp=',river_ramp
            ramp_is_active = .true.
            if (hotstart) then
               LEVEL3 'WARNING: hotstart is .true. AND river_ramp .gt. 1'
               LEVEL3 'WARNING: .. be sure you know what you are doing ..'
            end if
         end if
         LEVEL2 'river_factor= ',river_factor
         LEVEL2 'use_river_temp= ',use_river_temp
         LEVEL2 'use_river_salt= ',use_river_salt
         LEVEL2 'river_outflow_properties_follow_source_cell=',river_outflow_properties_follow_source_cell
         call read_river_info()
         allocate(ok(nriver),stat=rc) ! valid river spec. 1=yes, 0=other domain, -1=other domain, but need read.
         if (rc /= 0) stop 'rivers: Error allocating memory (ok)'
         allocate(river_flow(nriver),stat=rc) ! river flux
         if (rc /= 0) stop 'rivers: Error allocating memory (river_flow)'
         allocate(macro_height(nriver),stat=rc) ! height over a macro tims-step
         if (rc /= 0) stop 'rivers: Error allocating memory (macro_height)'
         allocate(river_temp(nriver),stat=rc) ! temperature of river water
         if (rc /= 0) stop 'rivers: Error allocating memory (river_temp)'
         allocate(river_salt(nriver),stat=rc) ! salinity of river water
         if (rc /= 0) stop 'rivers: Error allocating memory (river_salt)'
         allocate(river_split(nriver),stat=rc) ! split factor for river water
         if (rc /= 0) stop 'rivers: Error allocating memory (river_split)'
         allocate(flow_fraction_rel(nriver),stat=rc) ! Weight factor of data for river
         if (rc /= 0) stop 'rivers: Error allocating memory (flow_fraction_rel)'
         allocate(flow_fraction(nriver),stat=rc) ! Weight factor of data for river - scaled to unity sum for river
         if (rc /= 0) stop 'rivers: Error allocating memory (flow_fraction)'
         allocate(irr(nriver),stat=rc) ! integrated river runoff
         if (rc /= 0) stop 'rivers: Error allocating memory (irr)'

! ok(:) flags the location of each river relative to present subdomain:
!   1: River is present domain
!   2: River is in HALO of present domain
!  -1: River is multi-cell, where another part is inside present domain
!   0: River is outside present domain (no association)
         ok = 0
         rriver = 0 ! number of real existing rivers...
         flow_fraction_rel = _ZERO_
         do n=1,nriver
            i = ir(n)-ioff
            j = jr(n)-joff
            river_temp(n) = temp_missing
            river_salt(n) = salt_missing
            river_flow(n) = _ZERO_
            irr(n) = _ZERO_
            macro_height(n) = _ZERO_
!           calculate the number of used rivers, they must be
!           in sequence !
            rriver = rriver +1
            if ( n .gt. 1 ) then
               if (river_name(n) .eq. river_name(n-1)) then
                  rriver = rriver-1
               end if
            end if
! Other weighting schemes could be implemented here. But we can only use
! information, which is available for cells also outside the present subdomain.
!           flow_fraction(n) = _ONE_/ARCD1 ! This does not work.
            flow_fraction_rel(n) = _ONE_
            outside= &
                    i .lt. imin .or. i .gt. imax .or.  &
                    j .lt. jmin .or. j .gt. jmax
            outsidehalo= &
                    (i .lt. imin-HALO) .or. (i .gt. imax+HALO) .or.  &
                    (j .lt. jmin-HALO) .or. (j .gt. jmax+HALO)
            if( .not. outsidehalo) then
               if(az(i,j) .eq. 0) then
                  xxx = ' on land'
                  ok(n) = 0
               else if (.not. outside) then
                  xxx = ' inside'
                  ok(n) = 1
               else
                  xxx = ' in halo'
                  ok(n) = 2
               end if
               bathy = H(i,j)
               if (rzu(n) .gt. rzl(n)) then
                  rzl(n) = -1.
                  rzu(n) = -1.
                  LEVEL3 trim(river_name(n)),' rzu > rzl setting both to -1.'
               end if
               if (rzl(n) .gt. H(i,j)) then
                  rzl(n) = -1.
                  LEVEL3 trim(river_name(n)),' setting rzl=-1.'
               end if
               if (rzu(n) .gt. H(i,j)) then
                  rzu(n) = -1.
                  LEVEL3 trim(river_name(n)),' setting rzu=-1.'
               end if
            else
              xxx = ' outside'
              bathy = -9999.9
            end if
            write(line,'(I4,A20,2I5,3F8.1,A11)') n,trim(river_name(n)), &
                  ir(n),jr(n),bathy,rzl(n),rzu(n),xxx
            LEVEL3 trim(line)
         end do

!  Calculate the number of used gridboxes.
!  This particular section is prepared for multi-cell rivers not-in-sequence.
         LEVEL3 'Number of unique rivers: ',rriver
         allocate(real_river_name(rriver),stat=rc) ! NetCDF name of river.
         if (rc /= 0) stop 'rivers: Error allocating memory (real_river_name)'
         river_split(:) = 1    ! normal case
!  This is a brute-force implementation, N**2, but N is presumed low.
         do iriver=1,nriver
            numcells=0
            total_weight=_ZERO_
            do jriver=1,nriver
               if (river_name(iriver) .eq. river_name(jriver)) then
                  numcells     = numcells+1
                  total_weight = total_weight+flow_fraction_rel(jriver)
                  if (ok(iriver).gt.0 .and. ok(jriver).eq.0) ok(jriver)=-1
               end if
            end do
            river_split(iriver)   = numcells
            flow_fraction(iriver) = flow_fraction_rel(iriver)/total_weight
            if (numcells.ne.1 .and. ok(iriver).ne.0) then
               LEVEL3 'Multicell river (',trim(river_name(iriver)),'):# ',iriver, &
                    'w=',real(flow_fraction(iriver))
            end if
         end do

         LEVEL3 'split:',river_split
!  Create a list with the river names without multiple-cells, i.e. just a
!  single entry per real river name.
         nn = 1
         ni = 1
         do n=1,nriver
            if (ni .le. nriver) then
               real_river_name(nn) = river_name(ni)
               nn = nn + 1
               ni = ni + river_split(ni)
            end if
            if (ok(n) .eq. 0) then
               flow_fraction(n) = _ZERO_
            end if
         end do

      case default
         FATAL 'A non valid river_method has been selected'
         stop 'init_rivers'
   end select
   return

#ifdef DEBUG
   write(debug,*) 'Leaving init_rivers()'
   write(debug,*)
#endif
   return
   end subroutine init_rivers
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: read_river_info
!
! !INTERFACE:
   subroutine read_river_info()
!
! !DESCRIPTION:
!  Read global indices for river positions, the river name and optionally
!  depth range over which to distribute the water - zl:zu. Negative values
!  imply 'bottom' for zl and 'surface' for zu.
!
! !USES:
   IMPLICIT NONE
!
! !LOCAL VARIABLES:
   logical                   :: exist
   integer                   :: unit = 25 ! kbk
   integer                   :: n,rc,ios
   character(len=255)        :: line
!EOP
!-------------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'read_river_info() # ',Ncall
#endif

   LEVEL2 'read_river_info()'
!KB   inquire(file=river_info, exist=exist) 
!KB   if (exists) then
   open(unit,file=river_info,action='read',iostat=ios,status='old',err=90)
   do while (nriver == 0 .and. ios == 0)
      read(unit,'(A)',iostat=ios,end=91,err=92) line
      call strip_string(line)
      if (len_trim(line) .gt. 0 .and. ios == 0) then
         read(line,*,iostat=ios,err=92) nriver
      end if
   end do
   allocate(ir(nriver),stat=rc) ! i index of rivers
   if (rc /= 0) stop 'rivers: Error allocating memory (ir)'
   allocate(jr(nriver),stat=rc) ! j index of rivers
   if (rc /= 0) stop 'rivers: Error allocating memory (jr)'
   allocate(river_name(nriver),stat=rc) ! NetCDF name of river.
   if (rc /= 0) stop 'rivers: Error allocating memory (river_name)'
   allocate(rzl(nriver),stat=rc) ! Lower value for inflow range
   if (rc /= 0) stop 'rivers: Error allocating memory (rlz)'
   allocate(rzu(nriver),stat=rc) ! Upper value for inflow range
   if (rc /= 0) stop 'rivers: Error allocating memory (rzu)'

   n = 0
   do while (n .ne. nriver .and. ios == 0)
      read(unit,'(A)',iostat=ios,end=91,err=92) line
      call strip_string(line)
      if (len_trim(line) .gt. 0 .and. ios == 0) then
         n = n + 1
         read(line,*,iostat=ios) ir(n),jr(n),river_name(n),rzl(n),rzu(n)
         if (ios .ne. 0) then
            read(line,*,iostat=ios) ir(n),jr(n),river_name(n)
            rzl(n) = -1.
            rzu(n) = -1.
         end if
         river_name(n) = trim(river_name(n))
      end if
   end do

   if (n .ne. nriver) then
      FATAL 'read_river_info(): Could not read number of specified rivers'
      FATAL 'read_par_setup(): nriver =',nriver
      FATAL 'read_par_setup(): nread = ',n
      stop
   end if

#ifdef DEBUG
   write(debug,*) 'Leaving read_river_info()'
   write(debug,*)
#endif
   return

90 LEVEL2 'could not open ',trim(river_info),' for reading info on rivers'
   stop 'read_river_info()'
91 LEVEL2 'end of file reached'
   stop 'read_river_info()'
92 LEVEL2 'IO error condition'
   stop 'read_river_info()'

   end subroutine read_river_info
!EOC

#ifdef GETM_BIO
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_rivers_bio
!
! !INTERFACE:
   subroutine init_rivers_bio()
!
! !DESCRIPTION:
! First, memory for storing the biological loads from rivers is
! allocated.
! The variable - {\tt river\_bio} - is initialised to  - {\tt bio\_missing}.

!
! !USES:
#ifdef BFM_GOTM
   use coupling_getm_bfm_rivers,ONLY: assign_river_inputs_to_bio_states
   use string_functions, ONLY: getseq_number
!JM   use bio_var, only: var_names
   use bfm_output, only: var_names
#endif
   IMPLICIT NONE
!
! !LOCAL VARIABLES:
   integer                   :: rc
#ifdef BFM_GOTM
   integer                   :: i,j,k,l,m,n
   REALTYPE                  :: add=_ZERO_,frc=_ZERO_
   character(len=64)         :: l_river_name='',l_var_name=''
   logical                   :: ll_exist=.false.
   integer                   :: unit = 25 ! kbk
#endif
!EOP
!-------------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'init_rivers_bio() # ',Ncall
#endif

#ifndef BFM_GOTM
   LEVEL1 'init_rivers_bio()'

   allocate(river_bio(nriver,numc),stat=rc)
   if (rc /= 0) stop 'rivers: Error allocating memory (river_bio)'

   river_bio = bio_missing
#else
   LEVEL1 'init_rivers_bio()'

   select case (river_method)
      case(1,2)
         allocate(river_bio(rriver,numc),stat=rc)
         if (rc /= 0) stop 'rivers: Error allocating memory (river_bio)'
         river_bio = bio_missing

         allocate(river_max(numc),stat=rc)
         if (rc /= 0) stop 'rivers: Error allocating memory (river_max)'
         river_max=bio_missing
         allocate(river_min(numc),stat=rc)
         if (rc /= 0) stop 'rivers: Error allocating memory (river_min)'
         river_min=bio_missing
         allocate(river_func(numc),stat=rc)
         if (rc /= 0) stop 'rivers: Error allocating memory (river_func)'
         river_func=0

         k=0
         rc= assign_river_inputs_to_bio_states(0,'',0,0,k)
!JM bug? undefined!        if ( rc.ne.0) call errsns(rc)

         inquire(file=river_sa,exist=ll_exist)
         if ( ll_exist) then
           open(unit,file=river_sa,action='read',status='old',err=90)
           read(unit,*) nriver_sa
           allocate(river_frc(nriver_sa),stat=rc)
           if (rc /= 0) stop 'rivers: Error allocating memory (river_frc)'
           allocate(river_add(nriver_sa),stat=rc)
           if (rc /= 0) stop 'rivers: Error allocating memory (river_add)'
           allocate(seq_pointer_river(nriver_sa),stat=rc)
           if (rc /= 0) stop 'rivers: Error allocating memory (seq_pointer_river)'
           n=0
           do i=1,nriver_sa
              read(unit,*)  l_river_name,l_var_name, frc,add
              m=getseq_number(l_var_name,var_names,numc,.TRUE.)
              k=getseq_number(l_river_name,river_name,nriver,.TRUE.)
              if ( ok(k).gt. 0 ) then
                 j=m+ 100 *k
                 n=n+1 ; l=1
                 if ( n>1 ) then
                   do k=1,n-1
                     if ( j > seq_pointer_river(k) ) l=k+1
                   enddo
                   do k=n,l+1,-1
                     seq_pointer_river(k)= seq_pointer_river(k-1)
                     river_frc(k)= river_frc(k-1)
                     river_add(k)= river_add(k-1)
                   enddo
                 endif
                 seq_pointer_river(l)=j
                 river_frc(l)=frc
                 river_add(l)=add
                 LEVEL2 'Sensitivity analysis for river. shortname:',l_river_name
                 LEVEL2 'Multiplication fraction :', river_frc(l)
                 LEVEL2 'Load addition:', river_add(l)
              endif
           enddo
           nriver_sa=n
           close(unit)
         endif
   end select
#endif


#ifdef DEBUG
   write(debug,*) 'Leaving init_rivers_bio()'
   write(debug,*)
#endif
   return
#ifdef BFM_GOTM
90 LEVEL2 'could not open ',trim(river_sa),' for reading info on rivers'
   stop 'init_rivers_bio()'
#endif
   end subroutine init_rivers_bio
!EOC
#endif

#ifdef _FABM_
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_rivers_fabm
!
! !INTERFACE:
   subroutine init_rivers_fabm()
!
! !DESCRIPTION:
! First, memory for storing the biological loads from rivers is
! allocated.
! The variable - {\tt river\_fabm} - is initialised to  - variable-
! specific missing values obtained provided by FABM.
!
! !USES:
   IMPLICIT NONE
!
! !LOCAL VARIABLES:
   integer                   :: rc,m
!EOP
!-------------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'init_rivers_fabm() # ',Ncall
#endif

   if (allocated(fabm_pel)) then
      LEVEL1 'init_rivers_fabm()'

      allocate(river_fabm(nriver,size(model%state_variables)),stat=rc)
      if (rc /= 0) stop 'rivers: Error allocating memory (river_fabm)'

      do m=1,size(model%state_variables)
         if (model%state_variables(m)%no_river_dilution) then
            river_fabm(:,m) = model%state_variables(m)%missing_value
         else
            river_fabm(:,m) = _ZERO_
         end if
      end do
   end if

#ifdef DEBUG
   write(debug,*) 'Leaving init_rivers_fabm()'
   write(debug,*)
#endif
   return
   end subroutine init_rivers_fabm
!EOC
#endif

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE:  do_rivers - updating river points \label{sec-do-rivers}
!
! !INTERFACE:
   subroutine do_rivers(loop,do_3d)
!
! !DESCRIPTION:
!
! Here, the temperature, salinity, sea surface elevation and layer heights
! are updated in the river inflow grid boxes. Temperature and salinity
! are mixed with riverine values proportional to the old volume and the
! river inflow volume at that time step, sea surface elevation is simply
! increased by the inflow volume divided by the grid box area, and
! the layer heights are increased proportionally.
!
! !USES:
#ifdef BFM_GOTM
   use coupling_getm_bfm_rivers,only:make_river_flux_output
   use global_interface,only:CalcRiverConcentration   !JM BFM2016
!JM   use BFMCodeForGetm,only:CalcRiverConcentration  !JM BFM2024 
#endif
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: loop
   logical, intent(in)                 :: do_3d
!
! !LOCAL VARIABLES:
   integer                   :: i,j,k,m,n
   integer                   :: kl,kh
   REALTYPE                  :: rvol,height
   REALTYPE                  :: river_depth,x
#ifdef BFM_GOTM
   REALTYPE                  :: cor_dep,r_av,r_extra,rivinput
   integer                   :: kcor,macro_steps,it_thermo,nk,sa_l,sa_n
#endif
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'do_rivers() # ',Ncall
#endif

!  river spin-up
   if (ramp_is_active) then
      if (loop .ge. river_ramp) then
         ramp = _ONE_
         ramp_is_active = .false.
         STDERR LINE
         call write_time_string()
         LEVEL3 timestr,': finished river_ramp=',river_ramp
         STDERR LINE
      else
         ramp = _ONE_*loop/river_ramp
      end if
   end if

   select case (river_method)
      case(0)
      case(1,2)
#ifdef BFM_GOTM
       !code to do sensitivity analysis with rivers
       sa_n=0;if ( nriver_sa > 0 ) sa_n=1
#endif
         do n=1,nriver
            if(ok(n) .gt. 0) then
               i = ir(n)-ioff; j = jr(n)-joff
               rvol = ramp * dtm * river_flow(n) * flow_fraction(n)
               if (ok(n) .eq. 1) then
                  irr(n) = irr(n) + rvol
               end if
               height = rvol * ARCD1
               z(i,j) = z(i,j) + height
#ifndef NO_BAROCLINIC
               macro_height(n)=macro_height(n)+height

#ifdef BFM_GOTM
               kcor=kmax
#endif
               if (do_3d) then
                  if (rzl(n) .lt. _ZERO_) then
                     kl = 1
                  else
                     x = _ZERO_
                     do m=kmax,1,-1
                        x = x + hn(i,j,m)
                        if (rzl(n) .le. x) exit
                     end do
                     kl = m
                  end if
                  if (rzu(n) .lt. _ZERO_) then
                     kh = kmax
                  else
                     x = _ZERO_
                     do m=kmax,1,-1
                        x = x + hn(i,j,m)
                        if (rzu(n) .le. x) exit
                     end do
                     kh = m
                  end if
                  river_depth = sum(hn(i,j,kl:kh))
                  if (macro_height(n).gt._ZERO_ .or. .not.river_outflow_properties_follow_source_cell) then
                     if (update_salt ) then
                        if ( river_salt(n) .ne. salt_missing) then
                           S(i,j,kl:kh) = (S(i,j,kl:kh)*river_depth   &
                                         + river_salt(n)*macro_height(n))      &
                                         / (river_depth+macro_height(n))
                        else
                           S(i,j,kl:kh) = S(i,j,kl:kh)*river_depth   &
                                         / (river_depth+macro_height(n))
                        end if
                     end if
                     if (update_temp .and. river_temp(n) .ne. temp_missing) then
                        T(i,j,kl:kh) = (T(i,j,kl:kh)*river_depth   &
                                         + river_temp(n)*macro_height(n))      &
                                         / (river_depth+macro_height(n))
                     end if
#ifdef GETM_BIO
#ifndef BFM_GOTM
                     if (bio_calc) then
                        do m=1,numc
                           if ( river_bio(n,m) .ne. bio_missing ) then
                              cc3d(i,j,kl:kh,m) = &
                                    (cc3d(i,j,kl:kh,m)*river_depth &
                                    + river_bio(n,m)*macro_height(n))      &
                                    / (river_depth+macro_height(n))
                           end if
                        end do
                     end if
#else
            if (bio_calc) then
            cc=cc3d(i,j,:,:) !AN
            c1dimnumc=-1.0
              do m=1,numc
                !if function is used to caluclate the conc, of a constituent
                ! (see further: river_func ) river_bio(nk,m)=0.0
                !                            river_max(m)=-99999
                ! resulting in the fact that the program will NOT pass any of 
                ! if/elseifstatements in this loop.
                rivinput=-1.
                !if there is a timeseries river_bio(nk,m) >0.0
!if (n==96) then    !96 is the Rhine
!LEVEL1 'rivers.f90'
!LEVEL1 'rriver,numc',rriver,numc
!LEVEL1 'n,m',n,m
!LEVEL1 'i,j',i,j
!LEVEL1 'rivername',river_name(n)
!LEVEL1 'river_bio',river_bio(n,m)
!endif
                if ( river_bio(n,m) .gt.1.0D-80 ) then
                  !JM this is the normal operation: river_bio is read from file in ncdf_rivers.nc
                  c1dimnumc(m)=river_bio(n,m)            
                  !code to do sensitivity analysis with rivers
                  if ( sa_n > 0 ) then
                    sa_l=100*n+m
                    if ( sa_l == seq_pointer_river(sa_n)) then
                      rivinput=rivinput * river_frc(sa_n)
                      rivinput=rivinput+river_add(sa_n)*dtm *flow_fraction(n) *ARCD1
                      sa_n=sa_n+1;if ( sa_n > nriver_sa) sa_n=0
                    endif
                  endif
                  !end code to do sensitivity analysis with rivers
                elseif (abs(river_bio(n,m)-bio_missing).lt.1.0E-3) then
                  !if river_bio== bio-missing:there is no boundary condition....
                  !all tracked states will not modified by a not-tracked river.
                  ! HOWEVER THERE IS A DILUTION OF THE CONCENTRATION!
                  rivinput=1.0D-80                   
                  c1dimnumc(m)=rivinput
                elseif (abs(river_max(m)-bio_missing).gt.1.0E-3) then
                  ! code to limit riverinput if no data are given:
                  ! it assumed that concentration on the river is the same as 
                  ! in grid point where the river enters but limited!
                  r_av=sum(cc3d(i,j,1:kcor,m)*hn(i,j,1:kcor))/sum(hn(i,j,1:kcor))
                  rivinput=max(river_min(m),min(river_max(m),r_av))
                  c1dimnumc(m)=rivinput
                endif
              end do
              do m=1,numc
                r_extra=cc3d(i,j,kcor,m) 
                !code to calculate riverconcentration of a state var on basis 
                !of concentrations of other constituents in the same river.
                if ( river_func(m) == 1) then
!AN                  call CalcRiverConcentration(m,1,numc,T(i,j,kmax), &
!AN                      cc3d(1:numc,i,j,kmax),c1dimnumc)
!JM bfm2024                     call CalcRiverConcentration(m,T(i,j,1:kmax), &
!JM bfm2024                         cc(1:kmax,1:numc),cc3d(i,j,1:kmax,1:numc)) 
                  !JM CalcRiverConcentration is coded to set R6 components to disolved nutrient concentrations, modifies cc3d
                  call CalcRiverConcentration(m,kmax,numc,T(i,j,1:kmax), &
                      cc(1:kmax,1:numc),cc3d(i,j,1:kmax,1:numc))
                                                                    ! WARNING: n instead of 1:numc ?
                                                                    ! FIX: should be (mode,ETW, cc_sea, cc_river) cc and cc3d and
                                                                    ! the same

                endif
                !here the concentrations at the river points are modified::
                if ( c1dimnumc(m) >= 0.0) then
                  rivinput=c1dimnumc(m)
                  if (kcor<kmax) & 
                     rivinput=c1dimnumc(m) +cor_dep*sum(hn(i,j,it_thermo:kmax) &
                                   *(c1dimnumc(m)-cc3d(i,j,it_thermo:kmax,m)))
                  cc3d(i,j,1:kcor,m) = (cc3d(i,j,1:kcor,m)* &
                     (H(i,j)+ssen(i,j)) + rivinput*macro_height(n)) &
                                / (H(i,j)+ssen(i,j)+macro_height(n))
                   r_extra=(cc3d(i,j,kcor,m)-r_extra)/(1.0D-80+r_extra)
!                  if (r_extra> 5.0) &
!                      STDERR "WarningRivers: large change in:",m,r_extra 
!if (n==96) then    !96 is the Rhine
!LEVEL1 'm,rivinput',m,rivinput
!LEVEL1 'macro_heigth',macro_height(n)
!endif
                endif
              end do
              !JM this calculates fluxes to the benthos, not sure what the purpose of this is?
              call make_river_flux_output(macro_height(n), &
                dtm*macro_steps,ARCD1, c1dimnumc,numc,i,j)
              end if !JM bio_calc
!if (n==96) then    !96 is the Rhine
!stop
!endif
#endif
#endif
#ifdef _FABM_
                     if (allocated(fabm_pel)) then
                        do m=1,size(model%state_variables)
                           if ( river_fabm(n,m) .ne. model%state_variables(m)%missing_value ) then
                              fabm_pel(i,j,kl:kh,m) = &
                                    (fabm_pel(i,j,kl:kh,m)*river_depth &
                                    + river_fabm(n,m)*macro_height(n))      &
                                    / (river_depth+macro_height(n))
                           end if
                        end do
                     end if
#endif
                  end if

!                 Changes of total and layer height due to river inflow:
                  hn(i,j,kl:kh) = hn(i,j,kl:kh)/river_depth &
                                  *(river_depth+macro_height(n))
                  ssen(i,j) = ssen(i,j)+macro_height(n)
                  macro_height(n) = _ZERO_
               end if
#endif
            end if
         end do
      case default
         FATAL 'Not valid rivers_method specified'
         stop 'do_rivers'
   end select

#ifdef DEBUG
   write(debug,*) 'Leaving do_rivers()'
   write(debug,*)
#endif
   return
   end subroutine do_rivers
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE:  clean_rivers
!
! !INTERFACE:
   subroutine clean_rivers
!
! !DESCRIPTION:
!
! This routine closes the river handling by writing the integrated
! river run-off for each river to standard output.
!
! !USES:
   IMPLICIT NONE
!
! !LOCAL VARIABLES:
   integer                   :: i,j,n
   REALTYPE                  :: tot=_ZERO_
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'clean_rivers() # ',Ncall
#endif

   select case (river_method)
      case(0)
      case(1,2)
         do n=1,nriver
            if(ok(n) .gt. 0) then
               i = ir(n); j = jr(n)
               LEVEL2 trim(river_name(n)),':  ' ,irr(n)/1.e6, '10^6 m3'
               tot = tot+irr(n)
            end if
         end do
#ifdef _FABM_
         if (allocated(river_fabm)) deallocate(river_fabm)
#endif
      case default
         FATAL 'Not valid rivers_method specified'
         stop 'clean_rivers'
   end select

#ifdef DEBUG
   write(debug,*) 'Leaving clean_rivers()'
   write(debug,*)
#endif
   return
   end subroutine clean_rivers
!EOC

!-----------------------------------------------------------------------

   end module rivers

!-----------------------------------------------------------------------
! Copyright (C) 2001 - Karsten Bolding and Hans Burchard               !
!-----------------------------------------------------------------------
