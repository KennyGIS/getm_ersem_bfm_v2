#include "cppdefs.h"

! This module contains an adaptation of formulations from Kara et al (2000) 
! and (2005) papers in J. Atmos. Oceanic Technol.
!
! The code for the (2000) formulation is derived directly from the paper.
!
! For the (2005) formulation, the original code was obtained from 
!     https://www7320.nrlssc.navy.mil/nasec/
! and completely re-written.
!
! Please note, "pure" fortran subroutines were developed in order to ease
! code optimization (inlining, etc). The PURE prefix-spec is not standard
! Fortran 90 but was introduced from standard Fortran 95.
!
! /Per Berg


module exchcoeffs_kara
! !USES:
 !!  use meteo, only: KELVIN

   implicit none
   private
   REALTYPE, parameter :: KELVIN=273.15d0
   REALTYPE, parameter :: one = _ONE_, half = _HALF_, milli = _ONE_/1000, hundred = 100*_ONE_
   REALTYPE, parameter :: tl = -8*_ONE_,  tu = 7*_ONE_
   REALTYPE, parameter :: wl  = 1.2,  wu = 5.0,  wmx = 32.5
   REALTYPE, parameter :: tdl = 0.75, tn = -0.098
   REALTYPE, parameter :: CMIN = milli/100, dtp = tdl-tn, dtm = tdl+tn

   public :: kara2005_cd, kara2005_clcs, temp_diff
   public :: kara2000_cd, kara2000_clcs

contains

   !============================================================================

   pure subroutine temp_diff (airt, sst, qs, qa, td)
      ! ta-tw diff with a virtual temperature based correction

      implicit none

      ! arguments:
      REALTYPE, intent(in)  :: airt, sst, qs, qa
      REALTYPE, intent(out) :: td

      ! local vars:
      REALTYPE, parameter :: qra = 0.6
      REALTYPE :: ta, tw

      ta = airt - KELVIN*(half+sign(half,airt-hundred))
      tw = sst  - KELVIN*(half+sign(half,sst-hundred))

      td = ta - tw - qra*(ta+KELVIN)*(qs - qa)

   end subroutine temp_diff

   !============================================================================

   pure subroutine kara2005_clcs (td, wind, cl, cs)
      ! Full Kara et al 2005 corrected implementation of latent heat exchange 
      ! coefficient, and exchange coefficient for sensible heat is cs=0.96*cl

      implicit none

      ! arguments:
      REALTYPE, intent(in)  :: td, wind
      REALTYPE, intent(out) :: cl, cs

      ! constants:
      REALTYPE, parameter :: cl2cs = 0.96  !  cs/cl ratio

      ! Low wind speed:
      !   Neutral coefficient:
      REALTYPE, parameter :: Cl00n= 1.14086,Cl01n=-0.00312, Cl02n=-0.00093
      !   Unstable range:
      REALTYPE, parameter :: Cl00u= 2.07700,Cl01u=-0.39330, Cl02u= 0.03971
      REALTYPE, parameter :: Cl10u=-0.28990,Cl11u= 0.07350, Cl12u=-0.006267
      REALTYPE, parameter :: Cl20u=-0.01954,Cl21u= 0.005483,Cl22u=-0.0004867
      !   neutral-unstable range:
      REALTYPE, parameter :: Cl00m= Cl00u + (-Cl10u + Cl20u*tdl)*tdl
      REALTYPE, parameter :: Cl01m= Cl01u + (-Cl11u + Cl21u*tdl)*tdl
      REALTYPE, parameter :: Cl02m= Cl02u + (-Cl12u + Cl22u*tdl)*tdl
      !   Stable range:
      REALTYPE, parameter :: Cl00s=-0.29250, Cl01s= 0.54980,Cl02s=-0.05544
      REALTYPE, parameter :: Cl10s= 0.07272, Cl11s=-0.17400,Cl12s= 0.02489
      REALTYPE, parameter :: Cl20s=-0.006958,Cl21s= 0.01637,Cl22s=-0.002618
      !   neutral-stable range:
      REALTYPE, parameter :: Cl00p= Cl00s + (Cl10s + Cl20s*tdl)*tdl
      REALTYPE, parameter :: Cl01p= Cl01s + (Cl11s + Cl21s*tdl)*tdl
      REALTYPE, parameter :: Cl02p= Cl02s + (Cl12s + Cl22s*tdl)*tdl

      ! High wind speed:
      !   Neutral coefficient:
      REALTYPE, parameter :: Ch00n= 1.07300,  Ch01n= 0.005531,Ch02n= 0.00005433
      !   Unstable range:
      REALTYPE, parameter :: Ch00u= 1.07400,  Ch01u= 0.005579,Ch02u= 0.00005263
      REALTYPE, parameter :: Ch10u= 0.006912, Ch11u=-0.22440, Ch12u=-1.02700
      REALTYPE, parameter :: Ch20u= 0.0001849,Ch21u=-0.002167,Ch22u=-0.10100
      !   neutral-unstable range:
      REALTYPE, parameter :: Ch00m= Ch00u + (-Ch10u + Ch20u*tdl)*tdl
      REALTYPE, parameter :: Ch01m= Ch01u
      REALTYPE, parameter :: Ch02m= Ch02u
      REALTYPE, parameter :: Ch11m=         (-Ch11u + Ch21u*tdl)*tdl
      REALTYPE, parameter :: Ch12m=         (-Ch12u + Ch22u*tdl)*tdl
      !   Stable range:
      REALTYPE, parameter :: Ch00s= 1.02300, Ch01s= 0.009657,Ch02s=-0.00002281
      REALTYPE, parameter :: Ch10s=-0.002672,Ch11s= 0.21030, Ch12s=-5.32900
      REALTYPE, parameter :: Ch20s= 0.001546,Ch21s=-0.06228, Ch22s= 0.50940
      !   neutral-stable range:
      REALTYPE, parameter :: Ch00p= Ch00s + (Ch10s + Ch20s*tdl)*tdl
      REALTYPE, parameter :: Ch01p= Ch01s
      REALTYPE, parameter :: Ch02p= Ch02s
      REALTYPE, parameter :: Ch11p=         (Ch11s + Ch21s*tdl)*tdl
      REALTYPE, parameter :: Ch12p=         (Ch12s + Ch22s*tdl)*tdl

      ! local vars:
      REALTYPE            :: wa, tc, wi, q

      ! set CL according to actual regime:
      if (wind > wu) then
         wa = min(wmx,wind)
         wi = one/wa

         if (td > tdl) then
            tc = min(td,tu)
            cl = milli*(    (Ch00s + (Ch01s + Ch02s*wa)*wa)                    &
                        + ( (Ch10s + (Ch11s + Ch12s*wi)*wi)                    &
                           +(Ch20s + (Ch21s + Ch22s*wi)*wi)*tc)*tc )
            cl = max(CMIN,cl)
         elseif (td < -tdl) then
            tc = max(td,tl)
            cl = milli*(    (Ch00u + (Ch01u + Ch02u*wa)*wa)                    &
                        + ( (Ch10u + (Ch11u + Ch12u*wi)*wi)                    &
                           +(Ch20u + (Ch21u + Ch22u*wi)*wi)*tc)*tc )
         elseif (td >= tn) then ! tn <= td <= tdl
            q = -(td-tdl)/dtp
            cl = milli*(    (Ch00n + (Ch01n + Ch02n*wa)*wa)*q                  &
                        + ( (Ch00p + (Ch01p + Ch02p*wa)*wa)                    &
                                + (Ch11p + Ch12p*wi)*wi  )*(one-q) )
         else ! -tdl <= td < tn
            q = (td+tdl)/dtm
            cl = milli*(    (Ch00n + (Ch01n + Ch02n*wa)*wa)*q                  &
                        + ( (Ch00m + (Ch01m + Ch02m*wa)*wa)                    &
                                + (Ch11m + Ch12m*wi)*wi  )*(one-q) )
         endif

      else
         wa = max(wl,wind)

         if (td > tdl) then
            tc = min(td,tu)
            cl = milli*(    (Cl00s + (Cl01s + Cl02s*wa)*wa)                    &
                        + ( (Cl10s + (Cl11s + Cl12s*wa)*wa)                    &
                           +(Cl20s + (Cl21s + Cl22s*wa)*wa)*tc)*tc )
            cl = max(CMIN,cl)

         elseif (td < -tdl) then
            tc = max(td,tl)
            cl = milli*(    (Cl00u + (Cl01u + Cl02u*wa)*wa)                    &
                        + ( (Cl10u + (Cl11u + Cl12u*wa)*wa)                    &
                           +(Cl20u + (Cl21u + Cl22u*wa)*wa)*tc)*tc )

         elseif (td >= tn) then ! tn <= td <= tdl
            q = -(td-tdl)/dtp
            cl = milli*(    (Cl00n + (Cl01n + Cl02n*wa)*wa)*q                  &
                        +   (Cl00p + (Cl01p + Cl02p*wa)*wa)*(one-q) )

         else ! -tdl <= td < tn
            q = (td+tdl)/dtm
            cl = milli*(    (Cl00n + (Cl01n + Cl02n*wa)*wa)*q                  &
                        +   (Cl00m + (Cl01m + Cl02m*wa)*wa)*(one-q) )
         endif

      endif

      cs = cl2cs*cl

   end subroutine kara2005_clcs

   !============================================================================

   pure subroutine kara2005_cd (td, wind, cd)
      ! Full Kara et al 2005 corrected implementation
      ! of momentum exchange coefficient

      implicit none

      ! arguments:
      REALTYPE, intent(in)  :: td, wind
      REALTYPE, intent(out) :: cd

      ! constants:
      REALTYPE, parameter :: as000=-0.06695,  as010= 0.09966, as020=-0.02477,  &
                             as001= 0.3133,   as011=-2.116,   as021= 0.2726,   &
                             as002=-0.001473, as012= 4.626,   as022=-0.5558,   &
                             as003=-0.004056, as013=-2.680,   as023= 0.3139

      REALTYPE, parameter :: as500= 0.55815,  as510=-0.005593,as520= 0.0006024,&
                             as501= 0.08174,  as511= 0.2096,  as521=-0.02629,  &
                             as502=-0.0004472,as512=-8.634,   as522= 0.2121,   &
                             as503= 2.666e-6, as513= 18.63,   as523= 0.7755

      REALTYPE, parameter :: au000= 1.891,    au010=-0.006304,au020= 0.0004406,&
                             au001=-0.7182,   au011=-0.3028,  au021=-0.01769,  &
                             au002= 0.1975,   au012= 0.3120,  au022= 0.01303,  &
                             au003=-0.01790,  au013=-0.1210,  au023=-0.003394

      REALTYPE, parameter :: au500= 0.6497,   au510= 0.003827,au520=-4.83e-5,  &
                             au501= 0.06993,  au511=-0.2756,  au521= 0.007710, &
                             au502= 3.541e-5, au512=-1.091,   au522=-0.2555,   &
                             au503=-3.428e-6, au513= 4.946,   au523= 0.7654

      REALTYPE, parameter :: an000= 1.057,    an500= 0.6825,                   &
                             an001=-0.06949,  an501= 0.06945,                  &
                             an002= 0.01271,  an502=-0.0001029

      REALTYPE, parameter :: ap010= as000 + as010*tdl + as020*tdl*tdl,         &
                             ap011=         as011*tdl + as021*tdl*tdl,         &
                             ap012=         as012*tdl + as022*tdl*tdl,         &
                             ap013=         as013*tdl + as023*tdl*tdl

      REALTYPE, parameter :: ap510= as500 + as510*tdl + as520*tdl*tdl,         &
                             ap511=         as511*tdl + as521*tdl*tdl,         &
                             ap512=         as512*tdl + as522*tdl*tdl,         &
                             ap513=         as513*tdl + as523*tdl*tdl

      REALTYPE, parameter :: am010= au000 - au010*tdl + au020*tdl*tdl,         &
                             am011=       - au011*tdl + au021*tdl*tdl,         &
                             am012=       - au012*tdl + au022*tdl*tdl,         &
                             am013=       - au013*tdl + au023*tdl*tdl

      REALTYPE, parameter :: am510= au500 - au510*tdl + au520*tdl*tdl,         &
                             am511=       - au511*tdl + au521*tdl*tdl,         &
                             am512=       - au512*tdl + au522*tdl*tdl,         &
                             am513=       - au513*tdl + au523*tdl*tdl

      ! local vars:
      REALTYPE            :: wa, tc, wi, q


      tc = min(tu,max(tl,td))
      wa = max(wl,min(wmx,wind))
      wi = one/wa

      ! set CD according to actual regime:
      if (wa <= wu) then

         if     (tc >= tdl) then
            cd =   (as000 + (as001 + (as002 + as003*wa)*wa)*wa)                &
               + ( (as010 + (as011 + (as012 + as013*wi)*wi)*wi)                &
                  +(as020 + (as021 + (as022 + as023*wi)*wi)*wi)*tc)*tc
            cd = milli*cd
            cd = max(CMIN,cd)
         elseif (tc <=-tdl) then
            cd =   (au000 + (au001 + (au002 + au003*wa)*wa)*wa)                &
               + ( (au010 + (au011 + (au012 + au013*wi)*wi)*wi)                &
                  +(au020 + (au021 + (au022 + au023*wi)*wi)*wi)*tc)*tc
            cd = milli*cd
         elseif (tc >= tn)  then
            q =  (tc-tn)/(tdl-tn)  !linear between  0.75 and -0.098
            cd = q*(  (        (as001 + (as002 + as003*wa)*wa)*wa)             &
                    + (ap010 + (ap011 + (ap012 + ap013*wi)*wi)*wi))            &
               + (one-q)*(an000 + (an001 + an002*wa)*wa)
            cd = milli*cd
         else
            q = (-tc+tn)/(tdl+tn)  !linear between -0.75 and -0.098
            cd = q*(  (        (au001 + (au002 + au003*wa)*wa)*wa)             &
                    + (am010 + (am011 + (am012 + am013*wi)*wi)*wi))            &
               + (one-q)*(an000 + (an001 + an002*wa)*wa)
            cd = milli*cd
         endif !tc

      else !wa>5
         if     (tc >= tdl) then
            cd =   (as500 + (as501 + (as502 + as503*wa)*wa)*wa)                &
               + ( (as510 + (as511 + (as512 + as513*wi)*wi)*wi)                &
                  +(as520 + (as521 + (as522 + as523*wi)*wi)*wi)*tc)*tc
            cd = milli*cd
            cd = max(CMIN,cd)
         elseif (tc <=-tdl) then
            cd =   (au500 + (au501 + (au502 + au503*wa)*wa)*wa)                &
               + ( (au510 + (au511 + (au512 + au513*wi)*wi)*wi)                &
                  +(au520 + (au521 + (au522 + au523*wi)*wi)*wi)*tc)*tc
            cd = milli*cd
         elseif (tc >= tn)  then
            q =  (tc-tn)/(tdl-tn)  !linear between  0.75 and -0.098
            cd = q*(  (        (as501 + (as502 + as503*wa)*wa)*wa)             &
                    + (ap510 + (ap511 + (ap512 + ap513*wi)*wi)*wi))            &
               + (one-q)*(an500 + (an501 + an502*wa)*wa)
            cd = milli*cd
         else
            q = (-tc+tn)/(tdl+tn)  !linear between -0.75 and -0.098
            cd = q*(  (        (au501 + (au502 + au503*wa)*wa)*wa)             &
                    + (am510 + (am511 + (am512 + am513*wi)*wi)*wi))            &
               + (one-q)*(an500 + (an501 + an502*wa)*wa)
            cd = milli*cd
         endif !tc
      endif !wa
   end subroutine kara2005_cd

   !============================================================================

   pure subroutine kara2000_cd (tatsd, wind, cd)
      ! Kara et al 2000 implementation of momentum exchange coefficient.
      ! Note: Eqs(1-3) of Kara et al 2000 uses (Ts-Ta) instead of the 
      ! usual (Ta-Ts), therefore the minus sign on the td term.

      implicit none

      ! arguments:
      REALTYPE, intent(in)  :: tatsd, wind
      REALTYPE, intent(out) :: cd

      ! local parameters and variables:
      REALTYPE, parameter :: cd00=0.862, cd01=0.088, cd02=-0.00089
      REALTYPE, parameter :: cd10=0.1034, cd11=-0.00678, cd12=0.0001147
      REALTYPE, parameter :: wmn=2.5, wmx=32.5, tmn=-5.0, tmx=5.0
      REALTYPE :: w, td

      w  = max(wmn,min(wmx,wind))
      td = -max(tmn,min(tmx,tatsd))
      cd = cd00 + (cd01 + cd02*w)*w + (cd10 + (cd11 + cd12*w)*w)*td
      cd = milli*cd
   end subroutine kara2000_cd

   !============================================================================

   pure subroutine kara2000_clcs (tatsd, wind, cl, cs)
      ! Kara et al 2000 implementation of exchange coefficient for latent heat 
      ! and sensible heat.
      ! cs = 0.96 * cl according to Kara et al 2000.
      ! Note: Eqs(4-6) of Kara et al 2000 uses (Ts-Ta) instead of the usual
      ! (Ta-Ts), therefore the minus sign on the td term.

      implicit none

      ! arguments:
      REALTYPE, intent(in)  :: tatsd, wind
      REALTYPE, intent(out) :: cl, cs

      ! local parameters and variables:
      REALTYPE, parameter :: cl00=0.994, cl01=0.061, cl02=0.001
      REALTYPE, parameter :: cl10=-0.020, cl11=0.691, cl12=-0.817
      REALTYPE, parameter :: wmn=3.0, wmx=27.5, tmn=-5.0, tmx=5.0
      REALTYPE, parameter :: cl2cs=0.96  ! cs/cl ratio
      REALTYPE :: w, wi, td

      w  = max(wmn,min(wmx,wind))
      wi = one/w
      td = -max(tmn,min(tmx,tatsd))
      cl = cl00 + (cl01 + cl02*w)*w + (cl10 + (cl11 + cl12*wi)*wi)*td
      cl = milli*cl
      cs = cl2cs*cl
   end subroutine kara2000_clcs

   !============================================================================

end module exchcoeffs_kara
