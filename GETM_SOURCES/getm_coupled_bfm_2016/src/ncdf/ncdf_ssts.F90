#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: ncdf_ssts -
!
! !INTERFACE:
   module ncdf_ssts
!
! !DESCRIPTION:
!
! !USES:
   use netcdf
   use exceptions
   use time         ,only: string_to_julsecs,time_diff,add_secs,in_interval
   use time         ,only: jul0,secs0,julianday,secondsofday,timestep
   use time         ,only: write_time_string,timestr
   use domain       ,only: imin,imax,jmin,jmax,iextr,jextr
   use domain       ,only: ill,ihl,jll,jhl,ilg,ihg,jlg,jhg
   use domain       ,only: az,lonc,latc
   use grid_interpol,only: init_grid_interpol,do_grid_interpol
   use grid_interpol,only: to_rotated_lat_lon
   use variables_3d ,only: sst,sss
   IMPLICIT NONE
!
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_ssts_input_ncdf,get_ssts_data_ncdf
!
! !PRIVATE DATA MEMBERS:
   integer         :: ncid
   integer         :: sst_id=-1
   integer         :: sss_id=-1
   integer         :: ilen,jlen,nlen=0
   integer         :: start(3),edges(3)
   REALTYPE        :: offset
   logical         :: stationary,on_grid

   integer         :: grid_scan=1
   logical         :: point_source=.false.
   logical         :: rotated_ssts_grid=.false.

   REALTYPE, allocatable     :: ssts_lon(:),ssts_lat(:)

!  For gridinterpolation
   REALTYPE, allocatable     :: beta(:,:)
   REALTYPE, allocatable     :: ti(:,:),ui(:,:)
   integer, allocatable      :: gridmap(:,:,:)
!
   REALTYPE, parameter       :: pi=3.1415926535897932384626433832795029
   REALTYPE, parameter       :: deg2rad=pi/180.,rad2deg=180./pi
   REALTYPE                  :: southpole(3) = (/0.0,-90.0,0.0/)


   REALTYPE,dimension(:),allocatable   :: ssts_times(:)
   REALTYPE,dimension(:,:),pointer     :: sst_new,d_sst,sst_input
   REALTYPE,dimension(:,:),pointer     :: sss_new,d_sss,sss_input
   REALTYPE,dimension(:,:),allocatable :: wrk

#ifdef INPUT_DIR
   character(len=PATH_MAX)    :: ssts_file=trim(INPUT_DIR) // '/ssts_files.dat'
#else
   character(len=PATH_MAX)    :: ssts_file='ssts_files.dat'
#endif
   character(len=*),parameter :: name_sst="sst"
   character(len=*),parameter :: name_sss="sss"
!
! !REVISION HISTORY:
!  Original author(s): Knut Klingbeil
!
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_ssts_input_ncdf -
!
! !INTERFACE:
   subroutine init_ssts_input_ncdf(fn,nstart)
!
! !DESCRIPTION:
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   character(len=*), intent(in)        :: fn
   integer, intent(in)                 :: nstart
!
! !LOCAL VARIABLES:
   integer         :: il,ih,jl,jh
   integer         :: rc
!EOP
!-------------------------------------------------------------------------
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'init_ssts_input_ncdf() # ',Ncall
#endif

   call open_ssts_file(ssts_file)

   if (ilen.eq.iextr .and. jlen.eq.jextr) then
      LEVEL3 'Assuming On-Grid ssts forcing'
      on_grid = .true.
      il = ilg ; jl = jlg ; ih = ihg ; jh = jhg
   else if (ilen.eq.1 .and. jlen.eq.1) then
      LEVEL3 'Assuming Point Source ssts forcing'
      point_source = .true.
      on_grid = .true.
      il = 1 ; jl = 1 ; ih = 1 ; jh = 1
   else
      on_grid = .false.
      il = 1 ; jl = 1 ; ih = ilen ; jh = jlen

      allocate(ti(E2DFIELD),stat=rc)
      if (rc /= 0) &
          stop 'init_ssts_input_ncdf: Error allocating memory (ti)'
      ti = -999.

      allocate(ui(E2DFIELD),stat=rc)
      if (rc /= 0) stop &
              'init_ssts_input_ncdf: Error allocating memory (ui)'
      ui = -999.

      allocate(gridmap(E2DFIELD,1:2),stat=rc)
      if (rc /= 0) stop &
              'init_ssts_input_ncdf: Error allocating memory (gridmap)'
      gridmap(:,:,:) = -999

      allocate(beta(E2DFIELD),stat=rc)
      if (rc /= 0) &
          stop 'init_ssts_input_ncdf: Error allocating memory (beta)'
      beta = _ZERO_

      call init_grid_interpol(imin,imax,jmin,jmax,az,  &
                lonc,latc,ssts_lon,ssts_lat,southpole,gridmap,beta,ti,ui)

   end if

   start(1) = il; start(2) = jl;
   edges(1) = ih-il+1; edges(2) = jh-jl+1;
   edges(3) = 1

   allocate(wrk(edges(1),edges(2)),stat=rc)
   if (rc /= 0) call getm_error('init_ssts_input_ncdf()',               &
                                'Error allocating memory (wrk)')

   if (allocated(sst)) then
      allocate(sst_new(E2DFIELD),stat=rc)
      if (rc /= 0) stop 'do_meteo: Error allocating memory (sst_new)'
      sst_new = -9999*_ONE_
   end if
   if (allocated(sss)) then
      allocate(sss_new(E2DFIELD),stat=rc)
      if (rc /= 0) stop 'do_meteo: Error allocating memory (sss_new)'
      sss_new = -9999*_ONE_
   end if


   if ( stationary ) then

      sst_input => sst_new
      sss_input => sss_new
      call read_data(0)
      if (allocated(sst)) sst = sst_new
      if (allocated(sss)) sss = sss_new

   else

      if (allocated(sst)) then
         allocate(d_sst(E2DFIELD),stat=rc)
         if (rc /= 0) stop 'do_meteo: Error allocating memory (d_sst)'
         d_sst = _ZERO_
         sst_input => d_sst
      end if
      if (allocated(sss)) then
         allocate(d_sss(E2DFIELD),stat=rc)
         if (rc /= 0) stop 'do_meteo: Error allocating memory (d_sss)'
         d_sss = _ZERO_
         sss_input => d_sss
      end if

      call get_ssts_data_ncdf(nstart-1)

   end if

#ifdef DEBUG
   write(debug,*) 'Leaving init_ssts_input_ncdf()'
   write(debug,*)
#endif
   return
   end subroutine init_ssts_input_ncdf
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: get_ssts_data_ncdf - .
!
! !INTERFACE:
   subroutine get_ssts_data_ncdf(loop)
!
! !DESCRIPTION:
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer, intent(in)                 :: loop
!
! !LOCAL VARIABLES:
   integer                         :: indx
   integer, save                   :: save_n=1
   REALTYPE                        :: t,t_minus_t2
   REALTYPE,save                   :: t1,t2=-_ONE_
   REALTYPE,save                   :: deltm1=_ZERO_
   REALTYPE,dimension(:,:),pointer :: sst_old
   REALTYPE,dimension(:,:),pointer :: sss_old
   logical, save                   :: first=.true.
!EOP
!-------------------------------------------------------------------------
#ifdef DEBUG
   integer, save   :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'get_ssts_data_ncdf() # ',Ncall
#endif

   if (stationary) then
      if (allocated(sst)) sst = sst_new
      if (allocated(sss)) sst = sss_new
      return
   end if

!  find the right index

   t = loop*timestep

   if ( t .gt. t2 ) then

      t1 = t2

!     Note (KK): Even if the first time stage is the last entry of an
!                input file, we must not jump to the next file because
!                then indx-1 does not work!
!                Therefore .ge. and save_n must be initialised to 1!
      do indx=save_n,nlen
         t2 = ssts_times(indx) - offset
         if ( t2 .ge. t ) then
            EXIT
         end if
      end do

!     end of simulation?
      if (indx .gt. nlen) then
!        Note (KK): here we are not in case of first
!                   (because of in_interval check in open_ssts_file)
         LEVEL2 'Need new ssts file'
         call open_ssts_file(ssts_file)
         do indx=1,nlen
            t2 = ssts_times(indx) - offset
!           Note (KK): For ssts there is no check for long enough
!                      data sets. Therefore .ge.!
            if ( t2 .ge. t ) then
               EXIT
            end if
         end do
      end if

      if (first) then
         if ( t2 .gt. t ) then
            indx = indx-1
         end if
         t2 = ssts_times(indx) - offset
      end if

      call read_data(indx)
      save_n = indx+1

      if (allocated(sst)) then
         sst_old=>sst_new;sst_new=>d_sst;d_sst=>sst_old;sst_input=>d_sst
      end if
      if (allocated(sss)) then
         sss_old=>sss_new;sss_new=>d_sss;d_sss=>sss_old;sss_input=>d_sss
      end if

      if ( .not. first ) then
         if (allocated(sst)) then
            d_sst = sst_new - sst_old
         end if
         if (allocated(sss)) then
            d_sss = sss_new - sss_old
         end if
         deltm1 = _ONE_ / (t2 - t1)
      end if

   end if


   t_minus_t2 = t - t2

   if (allocated(sst)) then
      sst = sst_new + d_sst*deltm1*t_minus_t2
   end if
   if (allocated(sss)) then
      sss = sss_new + d_sss*deltm1*t_minus_t2
   end if

   first = .false.

#ifdef DEBUG
   write(debug,*) 'Leaving get_ssts_data_ncdf()'
   write(debug,*)
#endif
   return
   end subroutine get_ssts_data_ncdf
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: open_ssts_file - .
!
! !INTERFACE:
   subroutine open_ssts_file(ssts_file)
!
! !DESCRIPTION:
!  Instead of specifying the name of one single ncdf file directly - a list
!  of names can be specified in \emph{ssts\_file}. The rationale for this
!  approach is that output from operational meteorological models are of
!  typically 2-5 days length. Collecting a number of these files allows for
!  longer model integrations without have to reformat the data.
!  It is assumed that the different files contains the same variables
!  and that they are of the same shape.
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   character(len=*), intent(in)        :: ssts_file
!
! !LOCAL VARIABLES:
   integer, parameter        :: iunit=62
   character(len=256)        :: fn,time_units
   integer         :: junit,sunit,j1,s1,j2,s2
   integer         :: n,err
   logical,save       :: first=.true.
   logical,save       :: first_open=.true.
   logical,save       :: found=.false.

   integer            :: ndims,nvardims
   integer            :: lon_dim,lat_dim,time_dim=-1
   integer            :: lon_id,lat_id,time_id=-1
   integer            :: dim_len(3),vardim_ids(3)
   character(len=16)  :: dim_name(3)
   integer            :: id
   logical            :: have_southpole
!
!EOP
!-------------------------------------------------------------------------
#ifdef DEBUG
   integer, save   :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'open_ssts_file() # ',Ncall
#endif

   if (first) open(iunit,file=ssts_file,status='old',action='read',err=80)

   found = .false.


   do

      if (.not. first_open) then
         err = nf90_close(ncid)
         if (err .NE. NF90_NOERR) go to 10
      end if

      read(iunit,*,err=85,end=90) fn
      LEVEL3 'Trying ssts from:'
      LEVEL4 trim(fn)

      err = nf90_open(fn,NF90_NOWRITE,ncid)
      if (err .NE. NF90_NOERR) go to 10

      err = nf90_inquire(ncid, nDimensions = ndims)
      if (err .NE. NF90_NOERR) go to 10

      LEVEL4 'dimensions'
      do n=1,ndims
         err = nf90_inquire_dimension(ncid,n,name=dim_name(n),len=dim_len(n))
         if (err .NE. NF90_NOERR) go to 10
         LEVEL4 n,dim_name(n), dim_len(n)
      end do

      select case (ndims)
         case(2)
            LEVEL4 'stationary ssts fields'
            stationary=.true.
            if (.not. first_open) call getm_error('open_ssts_file()',   &
                        'stationary fields only possible in first file')
         case(3)
            LEVEL4 'non-stationary ssts fields'
            stationary=.false.
         case default
           call getm_error('open_ssts_file()', &
                           'invalid number of dimensions')
      end select


      if (allocated(sst)) then
         LEVEL4 ' ... checking variable ',name_sst
         err = nf90_inq_varid(ncid,name_sst,sst_id)
         if (err .NE. NF90_NOERR) go to 10
         err = nf90_inquire_variable(ncid,sst_id,ndims=nvardims)
         if (err .NE. NF90_NOERR) go to 10
         if (nvardims .NE. ndims) call getm_error('open_ssts_file()',      &
                                   'Wrong number of dims in '//name_sst)
         err = nf90_inquire_variable(ncid,sst_id,dimids=vardim_ids)
         if (err .NE. NF90_NOERR) go to 10
      end if

      if (allocated(sss)) then
         LEVEL4 ' ... checking variable ',name_sss
         err = nf90_inq_varid(ncid,name_sss,sss_id)
         if (err .NE. NF90_NOERR) go to 10
         err = nf90_inquire_variable(ncid,sss_id,ndims=nvardims)
         if (err .NE. NF90_NOERR) go to 10
         if (nvardims .NE. ndims) call getm_error('open_ssts_file()',      &
                                   'Wrong number of dims in '//name_sss)
         err = nf90_inquire_variable(ncid,sss_id,dimids=vardim_ids)
         if (err .NE. NF90_NOERR) go to 10
      end if

      lon_dim = vardim_ids(1)
      lat_dim = vardim_ids(2)

      if (stationary) exit

      time_dim = vardim_ids(3)

      err = nf90_inq_varid(ncid,dim_name(time_dim),time_id)
      if (err .ne. NF90_NOERR) go to 10

      if (dim_len(time_dim) > nlen) then
         if (.not. first) then
            deallocate(ssts_times,stat=err)
            if (err /= 0) call getm_error('open_ssts_file()',           &
                               'Error de-allocating memory (ssts_times)')
         end if
         allocate(ssts_times(dim_len(time_dim)),stat=err)
         if (err /= 0) call getm_error('open_ssts_file()',              &
                                  'Error allocating memory (ssts_times)')
      end if
      nlen = dim_len(time_dim)
      err = nf90_get_var(ncid,time_id,ssts_times(1:nlen))
      if (err .ne. NF90_NOERR) go to 10
      err =  nf90_get_att(ncid,time_id,'units',time_units)
      if (err .NE. NF90_NOERR) go to 10
      call string_to_julsecs(time_units,junit,sunit)

      offset = time_diff(jul0,secs0,junit,sunit)

      call add_secs(junit,sunit,nint(ssts_times(1   )),j1,s1)
      call add_secs(junit,sunit,nint(ssts_times(nlen)),j2,s2)

      if (first) then
         if (in_interval(j1,s1,julianday,secondsofday,j2,s2)) then
            found = .true.
         end if
      else
         if (time_diff(j2,s2,julianday,secondsofday) > _ZERO_) then
            found = .true.
         else
            LEVEL0 'WARNING: skipping ssts file ',trim(fn)
         end if
      end if

      if (found) exit
      first_open = .false.

   end do

   if ( .not. found ) call getm_error('open_ssts_file()',              &
                  'Could not find valid sstsforcing in '//trim(ssts_file))

   LEVEL3 'Using ssts from:'
   LEVEL4 trim(fn)
   if ( .not. stationary ) then
      LEVEL3 'ssts offset time ',offset
   end if
   ilen = dim_len(lon_dim)
   jlen = dim_len(lat_dim)

   if (first) then

      err = nf90_inq_varid(ncid,dim_name(lon_dim),lon_id)
      if (err .ne. NF90_NOERR) go to 10
      allocate(ssts_lon(ilen),stat=err)
      if (err /= 0) call getm_error('open_ssts_file()',                &
                                    'Error allocating memory (ssts_lon)')
      err = nf90_get_var(ncid,lon_id,ssts_lon(1:ilen))
      if (err .ne. NF90_NOERR) go to 10

      err = nf90_inq_varid(ncid,dim_name(lat_dim),lat_id)
      if (err .ne. NF90_NOERR) go to 10
      allocate(ssts_lat(ilen),stat=err)
      if (err /= 0) call getm_error('open_ssts_file()',                &
                                    'Error allocating memory (ssts_lat)')
      err = nf90_get_var(ncid,lat_id,ssts_lat(1:jlen))
      if (err .ne. NF90_NOERR) go to 10

!           first we check for CF compatible grid_mapping_name
            err = nf90_inq_varid(ncid,'rotated_pole',id)
            if (err .eq. NF90_NOERR) then
               LEVEL4 'Reading CF-compliant rotated grid specification'
               err = nf90_get_att(ncid,id, &
                                  'grid_north_pole_latitude',southpole(1))
               if (err .ne. NF90_NOERR) go to 10
               err = nf90_get_att(ncid,id, &
                                  'grid_north_pole_longitude',southpole(2))
               if (err .ne. NF90_NOERR) go to 10
               err = nf90_get_att(ncid,id, &
                                  'north_pole_grid_longitude',southpole(3))
               if (err .ne. NF90_NOERR) then
                  southpole(3) = _ZERO_
               end if
!              Northpole ---> Southpole transformation
               LEVEL4 'Transforming North Pole to South Pole specification'
               if (southpole(2) .ge. 0) then
                  southpole(2) = southpole(2) - 180.
               else
                  southpole(2) = southpole(2) + 180.
               end if 
               southpole(1) = -southpole(1)
               southpole(3) = _ZERO_
               have_southpole = .true.
               rotated_ssts_grid = .true.
            else
               have_southpole = .false.
            end if

!           and then we revert to the old way - checking 'southpole' directly
            if (.not. have_southpole) then
               err = nf90_inq_varid(ncid,'southpole',id)
               if (err .ne. NF90_NOERR) then
                  LEVEL4 'Setting southpole to (0,-90,0)'
               else
                  err = nf90_get_var(ncid,id,southpole)
                  if (err .ne. NF90_NOERR) go to 10
                  rotated_ssts_grid = .true.
               end if
            end if
            if (rotated_ssts_grid) then
               LEVEL4 'south pole:'
!              changed indices - kb 2014-12-15
               LEVEL4 '      lon ',southpole(2)
               LEVEL4 '      lat ',southpole(1)
            end if

   end if


   first = .false.

#ifdef DEBUG
   write(debug,*) 'Leaving open_ssts_file()'
   write(debug,*)
#endif
   return

10 FATAL 'open_ssts_file: ',nf90_strerror(err)
   stop 'open_ssts_file()'
80 FATAL 'I could not open: ',trim(ssts_file)
   stop 'open_ssts_file()'
85 FATAL 'Error reading: ',trim(ssts_file)
   stop 'open_ssts_file()'
90 FATAL 'Reached eof in: ',trim(ssts_file)
   stop 'open_ssts_file()'

   end subroutine open_ssts_file
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: read_data -
!
! !INTERFACE:
   subroutine read_data(indx)
!
! !DESCRIPTION:
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   integer,intent(in) :: indx
!
! !LOCAL VARIABLES:
   integer                      :: err
!EOP
!-----------------------------------------------------------------------

   call write_time_string()
   LEVEL3 timestr,': reading ssts data ...',indx
   start(3) = indx

   if (sst_id .gt. 0) then
      err = nf90_get_var(ncid,sst_id,wrk,start,edges)
      if (err .ne. NF90_NOERR) go to 10
      if (on_grid) then
         if (point_source) then
            sst_input = wrk(1,1)
         else
            sst_input(ill:ihl,jll:jhl) = wrk
         end if
      else! if (calc_met) then
         call do_grid_interpol(az,wrk,gridmap,ti,ui,sst_input)
      end if
   end if

   if (sss_id .gt. 0) then
      err = nf90_get_var(ncid,sss_id,wrk,start,edges)
      if (err .ne. NF90_NOERR) go to 10
      if (on_grid) then
         if (point_source) then
            sss_input = wrk(1,1)
         else
            sss_input(ill:ihl,jll:jhl) = wrk
         end if
      else! if (calc_met) then
         call do_grid_interpol(az,wrk,gridmap,ti,ui,sss_input)
      end if
   end if

#ifdef DEBUG
   write(debug,*) 'Leaving read_data()'
   write(debug,*)
#endif
   return

10 FATAL 'read_data: ',nf90_strerror(err)
   stop

   end subroutine read_data
!EOC

!-----------------------------------------------------------------------

   end module ncdf_ssts

!-----------------------------------------------------------------------
! Copyright (C) 2020 - Knut Klingbeil (IOW)                            !
!-----------------------------------------------------------------------
