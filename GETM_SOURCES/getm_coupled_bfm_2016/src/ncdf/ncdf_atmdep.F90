#include "cppdefs.h"
module ncdf_atmdep
   use netcdf
   use exceptions, only: getm_error, netcdf_error
   use domain, only: imin, imax, jmin, jmax, ioff, joff
   use domain, only: ill, ihl, jll, jhl, ilg, ihg, jlg, jhg
   use domain, only: az, lonc, latc
   use grid_interpol, only: init_grid_interpol, do_grid_interpol
   use time, only: julianday, secondsofday, CalDat, string_to_julsecs, &
                   write_time_string

   implicit none

   private
   public init_atmdep_input_ncdf, get_atmdep_data_ncdf

   integer :: ncid = -1
   integer :: n_ncdf_fields = 0
   integer, allocatable :: atmdep_var_ids(:)

   logical :: atmdep_on_model_grid = .true.
   integer :: nx_source = -1
   integer :: ny_source = -1
   integer :: grid_scan = 1
   integer :: read_start(3) = 1
   integer :: read_count(3) = 1
   integer :: n_time_records = 1
   integer :: time_origin_year = -1
   integer :: time_origin_month = 1
   integer :: time_origin_day = 1
   integer :: current_record = -1

   REALTYPE, allocatable :: atmdep_lon(:)
   REALTYPE, allocatable :: atmdep_lat(:)
   REALTYPE, allocatable :: beta(:,:)
   REALTYPE, allocatable :: ti(:,:)
   REALTYPE, allocatable :: ui(:,:)
   REALTYPE, allocatable :: interp_field(:,:)
   integer, allocatable :: gridmap(:,:,:)

   REALTYPE :: southpole(3) = (/0.0,-90.0,0.0/)

contains

   subroutine check_ncdf(err, where)
      integer, intent(in) :: err
      character(len=*), intent(in) :: where

      if (err .ne. NF90_NOERR) then
         call netcdf_error(err, trim(where), 'atmospheric deposition input')
      endif
   end subroutine check_ncdf

   subroutine init_atmdep_input_ncdf(atmdep_file, n_fields, atmdep_var_names, &
                                     atmdep_flux, on_grid, lon_name, lat_name)
      character(len=*), intent(in) :: atmdep_file
      integer, intent(in) :: n_fields
      character(len=*), intent(in) :: atmdep_var_names(:)
      REALTYPE, intent(inout) :: atmdep_flux(:,:,:)
      logical, intent(in) :: on_grid
      character(len=*), intent(in) :: lon_name
      character(len=*), intent(in) :: lat_name

      integer :: err
      integer :: field
      integer :: dimids(3)

      LEVEL1 'init_atmdep_input_ncdf'
      LEVEL2 'Atmospheric deposition file: ',trim(atmdep_file)

      err = nf90_open(trim(atmdep_file), NF90_NOWRITE, ncid)
      call check_ncdf(err, 'init_atmdep_input_ncdf: open atmdep file')

      atmdep_on_model_grid = on_grid
      n_ncdf_fields = n_fields
      if (allocated(atmdep_var_ids)) deallocate(atmdep_var_ids)
      allocate(atmdep_var_ids(n_ncdf_fields))
      atmdep_var_ids = -1

      do field = 1,n_ncdf_fields
         if (len_trim(atmdep_var_names(field)) == 0) then
            stop 'init_atmdep_input_ncdf: empty atmospheric deposition variable name'
         endif
         err = nf90_inq_varid(ncid, trim(atmdep_var_names(field)), &
                              atmdep_var_ids(field))
         call check_ncdf(err, 'init_atmdep_input_ncdf: find deposition variable')
      enddo

      err = nf90_inquire_variable(ncid, atmdep_var_ids(1), dimids=dimids)
      call check_ncdf(err, 'init_atmdep_input_ncdf: inquire deposition variable')
      err = nf90_inquire_dimension(ncid, dimids(1), len=nx_source)
      call check_ncdf(err, 'init_atmdep_input_ncdf: inquire x dimension')
      err = nf90_inquire_dimension(ncid, dimids(2), len=ny_source)
      call check_ncdf(err, 'init_atmdep_input_ncdf: inquire y dimension')
      call init_atmdep_time_axis(dimids(3))

      if (atmdep_on_model_grid) then
         call init_on_grid_read()
      else
         call init_off_grid_read(lon_name, lat_name)
      endif

      call get_atmdep_data_ncdf(1, atmdep_flux)

   end subroutine init_atmdep_input_ncdf

   subroutine init_atmdep_time_axis(time_dimid)
      integer, intent(in) :: time_dimid

      integer :: err
      integer :: time_var_id
      integer :: origin_jul
      integer :: origin_secs
      character(len=NF90_MAX_NAME) :: time_dim_name
      character(len=256) :: time_units

      err = nf90_inquire_dimension(ncid, time_dimid, name=time_dim_name, &
                                   len=n_time_records)
      call check_ncdf(err, 'init_atmdep_input_ncdf: inquire time dimension')

      if (n_time_records < 1) then
         call getm_error('init_atmdep_input_ncdf', &
                         'atmospheric deposition time dimension is empty')
      endif

      err = nf90_inq_varid(ncid, trim(time_dim_name), time_var_id)
      if (err .ne. NF90_NOERR) then
         err = nf90_inq_varid(ncid, 'time', time_var_id)
      endif
      call check_ncdf(err, 'init_atmdep_input_ncdf: find time variable')

      err = nf90_get_att(ncid, time_var_id, 'units', time_units)
      call check_ncdf(err, 'init_atmdep_input_ncdf: read time units')

      call string_to_julsecs(time_units, origin_jul, origin_secs)
      call CalDat(origin_jul, time_origin_year, time_origin_month, &
                  time_origin_day)

      LEVEL3 'atmdep time records: ',n_time_records
      LEVEL3 'atmdep time origin year/month/day: ',time_origin_year, &
             time_origin_month,time_origin_day

   end subroutine init_atmdep_time_axis

   subroutine init_on_grid_read()

      read_start(1) = max(imin + ioff, 1)
      read_start(2) = max(jmin + joff, 1)
      read_start(3) = 1
      read_count(1) = min(imax + ioff, nx_source) - read_start(1) + 1
      read_count(2) = min(jmax + joff, ny_source) - read_start(2) + 1
      read_count(3) = 1

      if (read_count(1) < 1 .or. read_count(2) < 1) then
         call getm_error('init_atmdep_input_ncdf', &
                         'on-grid atmospheric deposition does not overlap this rank')
      endif

   end subroutine init_on_grid_read

   subroutine init_off_grid_read(lon_name, lat_name)
      character(len=*), intent(in) :: lon_name
      character(len=*), intent(in) :: lat_name

      integer :: err
      integer :: id
      integer :: i, j
      logical :: ok
      REALTYPE :: tmp

      LEVEL2 'Initialising atmospheric deposition grid interpolation'

      allocate(atmdep_lon(nx_source),stat=err)
      if (err /= 0) stop 'init_atmdep_input_ncdf: Error allocating atmdep_lon'
      err = nf90_inq_varid(ncid,trim(lon_name),id)
      call check_ncdf(err, 'init_atmdep_input_ncdf: find longitude variable')
      err = nf90_get_var(ncid,id,atmdep_lon)
      call check_ncdf(err, 'init_atmdep_input_ncdf: read longitude variable')

      allocate(atmdep_lat(ny_source),stat=err)
      if (err /= 0) stop 'init_atmdep_input_ncdf: Error allocating atmdep_lat'
      err = nf90_inq_varid(ncid,trim(lat_name),id)
      call check_ncdf(err, 'init_atmdep_input_ncdf: find latitude variable')
      err = nf90_get_var(ncid,id,atmdep_lat)
      call check_ncdf(err, 'init_atmdep_input_ncdf: read latitude variable')

      if (atmdep_lat(1) .gt. atmdep_lat(2)) then
         LEVEL3 'Reverting atmospheric deposition lat-axis and setting grid_scan to 0'
         grid_scan = 0
         do j=1,ny_source/2
            tmp = atmdep_lat(j)
            atmdep_lat(j) = atmdep_lat(ny_source-j+1)
            atmdep_lat(ny_source-j+1) = tmp
         enddo
      endif

      allocate(beta(E2DFIELD),stat=err)
      if (err /= 0) stop 'init_atmdep_input_ncdf: Error allocating beta'
      beta = _ZERO_
      allocate(ti(E2DFIELD),stat=err)
      if (err /= 0) stop 'init_atmdep_input_ncdf: Error allocating ti'
      ti = -999.
      allocate(ui(E2DFIELD),stat=err)
      if (err /= 0) stop 'init_atmdep_input_ncdf: Error allocating ui'
      ui = -999.
      allocate(gridmap(E2DFIELD,1:2),stat=err)
      if (err /= 0) stop 'init_atmdep_input_ncdf: Error allocating gridmap'
      gridmap = -999
      allocate(interp_field(E2DFIELD),stat=err)
      if (err /= 0) stop 'init_atmdep_input_ncdf: Error allocating interp_field'
      interp_field = _ZERO_

      call init_grid_interpol(ill,ihl,jll,jhl,az,lonc,latc, &
                              atmdep_lon,atmdep_lat,southpole, &
                              gridmap,beta,ti,ui)

      ok = .true.
      do j=jmin,jmax
         do i=imin,imax
            if (az(i,j) .gt. 0 .and. &
                (ui(i,j) .lt. _ZERO_ .or. ti(i,j) .lt. _ZERO_)) then
               ok = .false.
               LEVEL3 'atmdep interpolation error at (i,j) ',i,j
            endif
         enddo
      enddo
      if (.not. ok) then
         call getm_error('init_atmdep_input_ncdf', &
                         'Some atmospheric deposition interpolation coefficients are not valid')
      endif

      read_start(1) = minval(gridmap(:,:,1),mask=(gridmap(:,:,1).gt.0))
      read_start(2) = minval(gridmap(:,:,2),mask=(gridmap(:,:,2).gt.0))
      read_count(1) = min(maxval(gridmap(:,:,1))+1,nx_source) - read_start(1) + 1
      read_count(2) = min(maxval(gridmap(:,:,2))+1,ny_source) - read_start(2) + 1
      read_start(3) = 1
      read_count(3) = 1

      where (gridmap(:,:,1) .gt. 0) gridmap(:,:,1) = gridmap(:,:,1) - read_start(1) + 1
      where (gridmap(:,:,2) .gt. 0) gridmap(:,:,2) = gridmap(:,:,2) - read_start(2) + 1

      if (grid_scan .eq. 0) then
         j = read_start(2)
         read_start(2) = ny_source - (read_start(2) + read_count(2) - 1) + 1
         read_count(2) = ny_source - j + 1 - read_start(2) + 1
      endif

      LEVEL3 'atmdep source x start/count: ',read_start(1),read_count(1)
      LEVEL3 'atmdep source y start/count: ',read_start(2),read_count(2)

   end subroutine init_off_grid_read

   subroutine get_atmdep_data_ncdf(n, atmdep_flux)
      integer, intent(in) :: n
      REALTYPE, intent(inout) :: atmdep_flux(:,:,:)

      integer :: err
      integer :: record
      integer :: field
      integer :: il, ih, jl, jh
      REALTYPE, allocatable :: work(:,:)

      if (ncid < 0 .or. n_ncdf_fields < 1) then
         stop 'get_atmdep_data_ncdf: NetCDF reader not initialized'
      endif

      if (size(atmdep_flux,3) /= n_ncdf_fields) then
         stop 'get_atmdep_data_ncdf: field count changed after init'
      endif

      record = atmdep_record_for_time()
      read_start(3) = record
      atmdep_flux = _ZERO_

      if (record .ne. current_record) then
         LEVEL3 'atmdep time record: ',record
         current_record = record
      endif

      allocate(work(read_count(1),read_count(2)))

      do field = 1,n_ncdf_fields
         err = nf90_get_var(ncid, atmdep_var_ids(field), work, &
                            start=read_start, count=read_count)
         call check_ncdf(err, 'get_atmdep_data_ncdf: read deposition field')

         if (atmdep_on_model_grid) then
            il = read_start(1) - ioff
            ih = il + read_count(1) - 1
            jl = read_start(2) - joff
            jh = jl + read_count(2) - 1
            atmdep_flux(il:ih,jl:jh,field) = work
         else
            call flip_var(work)
            call do_grid_interpol(az,work,gridmap,ti,ui,interp_field)
            atmdep_flux(:,:,field) = interp_field(imin:imax,jmin:jmax)
         endif
      enddo

      deallocate(work)

   end subroutine get_atmdep_data_ncdf

   integer function atmdep_record_for_time()
      integer :: year
      integer :: month
      integer :: day
      character(len=19) :: nowstr
      character(len=256) :: msg

      if (n_time_records <= 1) then
         atmdep_record_for_time = 1
         return
      endif

      call CalDat(julianday, year, month, day)
      atmdep_record_for_time = (year - time_origin_year) * 12 + &
                               (month - time_origin_month) + 1

      if (atmdep_record_for_time < 1) then
         call write_time_string(julianday, secondsofday, nowstr)
         write(msg,'(a,a,a,i4.4,a1,i2.2,a)') &
            'atmospheric deposition time ',trim(nowstr), &
            ' is before the first forcing month ',time_origin_year,'-', &
            time_origin_month,'.'
         call getm_error('get_atmdep_data_ncdf', trim(msg))
      endif

      if (atmdep_record_for_time > n_time_records) then
         call write_time_string(julianday, secondsofday, nowstr)
         write(msg,'(a,a,a,i8,a)') &
            'atmospheric deposition time ',trim(nowstr), &
            ' is after the last forcing record ',n_time_records,'.'
         call getm_error('get_atmdep_data_ncdf', trim(msg))
      endif

   end function atmdep_record_for_time

   subroutine flip_var(var)
      REALTYPE, intent(inout) :: var(read_count(1),read_count(2))

      select case (grid_scan)
         case (0)
            var = var(:,read_count(2):1:-1)
      end select

   end subroutine flip_var

end module ncdf_atmdep
