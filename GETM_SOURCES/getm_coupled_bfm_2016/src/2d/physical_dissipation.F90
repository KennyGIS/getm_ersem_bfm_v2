#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: physical_dissipation()
!
! !INTERFACE:
   subroutine physical_dissipation(velu,velv,Am,phydis)
!
! !DESCRIPTION:
!
! Here, the physical dissipation of mean kinetic energy is calculated as 
! \begin{equation}
! D^{phy} = D^{phy}_h + D^{phy}_v = 2 \alpha A^M_h S_h^2 +(\nu_t+\nu) S_v^2
! \end{equation}
! with the horizontal eddy viscosity, $A^M_h$, the vertical eddy viscosity,
! $\nu_t$, the molecular viscosity, $\nu$, the drying parameter, $\alpha$
! (see equations (\ref{uEq}) and (\ref{vEq})), the horizontal shear tensor,
! \begin{equation}
! S_{ij} = \frac12 \left(\partial_{x_i} u_j + \partial_{x_j}u_i \right),
! \quad i,j=1,2
! \end{equation}
! (with $x_1=x$, $x_2=y$, $u_1=u$ and $u_2=u$) and the scalar vertical shear
! \begin{equation}
! S_v = \left[\left(\partial_zu\right)^2
! +\left(\partial_zv\right)^2\right]^{1/2}.
! \end{equation}
! The horizontal shear square is then denoted as
! \begin{equation}
! S_h^2 = \sum_{i,j=1}^2 
! \left[\frac12 \left(\partial_{x_i} u_j + \partial_{x_j}u_i \right)\right]^2.
! \end{equation}
! Note that the dissipation term is obtained from the multiplication of the
! $u$-equation (\ref{uEq}) by $u$, the multiplicationof the $v$-equation
! (\ref{vEq}) by $v$ and subsequent addition of the two resulting equations. 
!
! This routine is called only for {\tt save\_numerical\_analyses = .true.}
!
!
! !USES:
   use domain,       only: imin,imax,jmin,jmax,az,au,av,ax
   use domain, only: dxc,dyc,dxx,dyx
   use domain, only: dry_z

   IMPLICIT NONE

! !INPUT PARAMETERS
   REALTYPE,dimension(E2DFIELD),intent(in)  :: velu,velv
   REALTYPE,intent(in)                      :: Am

! !OUTPUT PARAMETERS
   REALTYPE,dimension(E2DFIELD),intent(out) :: phydis
!
! !REVISION HISTORY:
!  Original author(s): Hans Burchard
!
! !LOCAL VARIABLES:
   integer                                  :: i,j
   REALTYPE,dimension(E2DFIELD)             :: shear
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'physical_dissipation() # ',Ncall
#endif
#ifdef SLICE_MODEL
   j = jmax/2 ! this MUST NOT be changed!!!
#endif

   if (Am .gt. _ZERO_) then

#ifndef SLICE_MODEL
      do j=jmin-HALO,jmax+HALO-1
#endif
         do i=imin-HALO,imax+HALO-1
            shear(i,j) = _ZERO_
            if (ax(i,j) .eq. 1) then
!              calculate shearX
               !if (au(i,j).eq.1 .or. au(i,j+1).eq.1) then ! HB
               if (av(i,j).eq.1 .or. av(i+1,j).eq.1) then ! KK
!                 Note (KK): excludes concave and W/E open boundaries (dvdxX=0)
!                            includes convex open boundaries (no mirroring)
                  shear(i,j) = (velv(i+1,j) - velv(i,j)) / DXX
               end if
#ifndef SLICE_MODEL
               !if (av(i,j).eq.1 .or. av(i+1,j).eq.1) then ! HB
               if (au(i,j).eq.1 .or. au(i,j+1).eq.1) then ! KK
!                 Note (KK): excludes concave and N/S open boundaries (dudyX=0)
!                            includes convex open boundaries (no mirroring)
                  shear(i,j) = shear(i,j) + (velu(i,j+1) - velu(i,j)) / DYX
               end if
#endif
            end if
         end do
#ifndef SLICE_MODEL
      end do
      do j=jmin-HALO,jmax+HALO-1
#endif
         do i=imax+HALO-1,imin-HALO+1,-1 ! loop order MUST NOT be changed!!!
!           Note (KK): slip condition dudyV(av=0)=0
!                      prolonged outflow condition dvdxV(av=3)=0
!                      shearV(av=3) would require shearX outside open boundary
!                      (however shearV(av=3) not needed, therefore not calculated)
            if (az(i,j).eq.1 .or. az(i,j+1).eq.1) then
            ! calculate shearV
               shear(i,j) = _HALF_ * ( shear(i-1,j) + shear(i,j) )
            else
               shear(i,j) = _ZERO_
            end if
         end do
#ifndef SLICE_MODEL
      end do
      do j=jmax+HALO-1,jmin-HALO+1,-1 ! loop order MUST NOT be changed!!!
         do i=imin-HALO+1,imax+HALO-1
            if (az(i,j) .eq. 1) then
               ! calculate shearC
               shear(i,j) = _HALF_ * ( shear(i,j-1) + shear(i,j) )
            end if
         end do
      end do
#endif

      ! 2*Am*((du/dx)**2+0.5*(dv/dx + du/dy)**2+(dv/dy)**2) on T-points
#ifndef SLICE_MODEL
      do j=jmin,jmax
#endif
         do i=imin,imax
            if (az(i,j) .eq. 1) then
               phydis(i,j) = _TWO_*Am*                                     &
                                       (                                    &
                                         ((velu(i,j)-velu(i-1,j))/DXC)**2 &
#ifndef SLICE_MODEL
                                        +((velv(i,j)-velv(i,j-1))/DYC)**2 &
#endif
                                        +_HALF_*shear(i,j)**2               &
                                       )
               phydis(i,j) = dry_z(i,j) * phydis(i,j)
            end if
         end do
#ifndef SLICE_MODEL
      end do
#endif

   end if
 
#ifdef DEBUG
   write(debug,*) 'Leaving physical_dissipation()'
   write(debug,*)
#endif
   return
   end subroutine physical_dissipation
!EOC
!-----------------------------------------------------------------------
! Copyright (C) 2012 - Hans Burchard and Karsten Bolding               !
!-----------------------------------------------------------------------
