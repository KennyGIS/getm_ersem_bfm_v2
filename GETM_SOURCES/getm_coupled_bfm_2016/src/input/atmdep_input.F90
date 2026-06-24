#include "cppdefs.h"
module atmdep_input

   use domain, only: imin, imax, jmin, jmax
   use atmdep_state, only: use_atmdep, max_atmdep_fields, n_atmdep_fields, &
                           atmdep_flux_vars, atmdep_target_vars, atmdep_flux
   use ncdf_atmdep, only: init_atmdep_input_ncdf, get_atmdep_data_ncdf
   implicit none

   logical :: atmdep_on_grid = .true.

   character(len=256) :: atmdep_file = 'atmdep_n.nc'
   character(len=64)  :: atmdep_var_noy_total = 'dep_noy_total'
   character(len=64)  :: atmdep_var_nhx_total = 'dep_nhx_total'
   character(len=64)  :: atmdep_lon_name = 'lon'
   character(len=64)  :: atmdep_lat_name = 'lat'

   namelist /atmdep_nml/ use_atmdep, atmdep_file, &
                         atmdep_var_noy_total, atmdep_var_nhx_total, &
                         n_atmdep_fields, atmdep_flux_vars, &
                         atmdep_target_vars, &
                         atmdep_on_grid, &
                         atmdep_lon_name, atmdep_lat_name

contains

!-----------------------------------------------------------------------
! Return path relative to input_dir unless file_name is already absolute.
!-----------------------------------------------------------------------
   function resolve_input_path(input_dir,file_name) result(path)
      character(len=*), intent(in) :: input_dir
      character(len=*), intent(in) :: file_name
      character(len=512)           :: path

      if (len_trim(file_name) == 0) then
         path = ''
      else if (file_name(1:1) == '/') then
         path = trim(file_name)
      else
         path = trim(input_dir) // trim(file_name)
      end if
   end function resolve_input_path

!-----------------------------------------------------------------------
! Preserve the first atmospheric N implementation as the default mapping.
! Future atmospheric deposition fields should use n_atmdep_fields,
! atmdep_flux_vars, and atmdep_target_vars directly in atmdep.nml.
!-----------------------------------------------------------------------
   subroutine set_default_nitrogen_fields()

      n_atmdep_fields = 2
      atmdep_flux_vars = ''
      atmdep_target_vars = ''
      atmdep_flux_vars(1) = trim(atmdep_var_noy_total)
      atmdep_target_vars(1) = 'N3n'
      atmdep_flux_vars(2) = trim(atmdep_var_nhx_total)
      atmdep_target_vars(2) = 'N4n'

   end subroutine set_default_nitrogen_fields

!-----------------------------------------------------------------------
! Initialise atmospheric deposition forcing.
!-----------------------------------------------------------------------
   subroutine init_atmdep_input(input_dir,n)
      character(len=*), intent(in) :: input_dir
      integer, intent(in)          :: n

      integer :: rc
      integer :: field
      integer, parameter :: atmdep_nml_unit = 97
      character(len=256) :: nml_file
      character(len=512) :: atmdep_path

      LEVEL1 'init_atmdep_input'

      nml_file = trim(input_dir) // 'atmdep.nml'
      open(atmdep_nml_unit,status='old',action='read',file=trim(nml_file), &
           iostat=rc)
      if (rc /= 0) stop 'init_atmdep_input: could not open atmdep.nml'
      read(atmdep_nml_unit,NML=atmdep_nml)
      close(atmdep_nml_unit)

      if (.not. use_atmdep) then
         LEVEL2 'Atmospheric deposition disabled'
         return
      end if

      if (n_atmdep_fields == 0) then
         call set_default_nitrogen_fields()
      end if

      if (n_atmdep_fields < 1 .or. n_atmdep_fields > max_atmdep_fields) then
         stop 'init_atmdep_input: invalid n_atmdep_fields'
      end if

      do field = 1,n_atmdep_fields
         if (len_trim(atmdep_flux_vars(field)) == 0) then
            stop 'init_atmdep_input: empty atmospheric deposition flux variable'
         end if
         if (len_trim(atmdep_target_vars(field)) == 0) then
            stop 'init_atmdep_input: empty atmospheric deposition target variable'
         end if
      enddo

      allocate(atmdep_flux(imin:imax,jmin:jmax,n_atmdep_fields),stat=rc)
      if (rc /= 0) stop 'init_atmdep_input: Error allocating atmdep_flux'

      atmdep_flux = _ZERO_

      LEVEL2 'Atmospheric deposition enabled'
      LEVEL3 'atmdep_file=          ',trim(atmdep_file)
      LEVEL3 'atmdep_on_grid=       ',atmdep_on_grid
      LEVEL3 'n_atmdep_fields=      ',n_atmdep_fields

      atmdep_path = resolve_input_path(input_dir,atmdep_file)
      call init_atmdep_input_ncdf(atmdep_path,n_atmdep_fields, &
                                  atmdep_flux_vars,atmdep_flux, &
                                  atmdep_on_grid,atmdep_lon_name, &
                                  atmdep_lat_name)

   end subroutine init_atmdep_input

!-----------------------------------------------------------------------
! Update atmospheric deposition forcing for model step n.
!-----------------------------------------------------------------------
   subroutine get_atmdep_data(n)
      integer, intent(in) :: n

      if (.not. use_atmdep) return

      call get_atmdep_data_ncdf(n,atmdep_flux)
   end subroutine get_atmdep_data

end module atmdep_input
