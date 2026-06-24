#include "cppdefs.h"
module atmdep_state

   implicit none

   integer, parameter, public :: max_atmdep_fields = 16

   logical, public :: use_atmdep = .false.
   integer, public :: n_atmdep_fields = 0
   character(len=64), public :: atmdep_flux_vars(max_atmdep_fields) = ''
   character(len=64), public :: atmdep_target_vars(max_atmdep_fields) = ''

   REALTYPE, allocatable, public :: atmdep_flux(:,:,:)

end module atmdep_state
