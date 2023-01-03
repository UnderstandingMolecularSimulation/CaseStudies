Ccccccccccccccccccccccccccc
C     Global Variables    C
Ccccccccccccccccccccccccccc

      Integer           Maxtraject,Maxumbrel,Nslice,Ncycle,Nminit
      Double Precision  Maxdslice

      Parameter        (Maxtraject = 500000)
      Parameter        (Maxumbrel  = 500000)
      Parameter        (Maxdslice  = 0.001d0)

      Logical           Lb(Maxtraject),Lb_Old(Maxtraject),
     &                  La(Maxtraject),La_Old(Maxtraject),
     &                  Lumbrella

      Double Precision Temp,Onesixth,Beta,Tstep,Tstep2,Tstep3,
     &                 Xxtra(Maxtraject),Yytra(Maxtraject),
     &                 Vxtra(Maxtraject),Vytra(Maxtraject),
     &                 Xxold(Maxtraject),Yyold(Maxtraject),
     &                 Vxold(Maxtraject),Vyold(Maxtraject),
     &                 Eeold(Maxtraject),Eetra(Maxtraject),
     &                 Deltap,Drift1,Drift2,Shoot1,Shoot2,Shift1,
     &                 Shift2,Radius,Lmin,Lmax,Rxx,Ryy,Vxx,Vyy,
     &                 Shoota,Shootb

      Common /Mmyyq/   Temp,Onesixth,Beta,Tstep,Tstep2,Tstep3,
     &                 Xxtra,Yytra,Vxtra,Vytra,Xxold,
     &                 Yyold,Vxold,Vyold,Eeold,Eetra,Deltap,
     &                 Drift1,Drift2,Shoot1,Shoot2,Shift1,
     &                 Shift2,Radius,Lmin,Lmax,Rxx,Ryy,Vxx,
     &                 Vyy,Shoota,Shootb,Nslice,Ncycle,
     &                 Nminit,Lb,Lb_Old,La,La_Old,Lumbrella
