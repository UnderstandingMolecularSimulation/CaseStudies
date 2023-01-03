      Program Dpd
      Implicit None

Cccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Dpd Program Of Linear Molecules              C
C     Self-Consisting Integration                  C
C     Algorithm By Pagonabarraga, Hagen            C
C     And Frenkel                                  C
C     (Europhys. Lett. 42(4), Pp 377-382 (1998))   C
C                                                  C
C     Suggested Reading:                           C
C     Groot&Warren, J. Chem. Phys. 107(11),        C
C     Pp 4423-4435 (1997)                          C
C                                                  C
C     Code Written By Thijs J.H. Vlugt             C
C     With Help From Maddalena Venturoli,          C
C     Roland Van Vliet and Sandrer Willemsen       C
C                                                  C
C     September 1999                               C
Cccccccccccccccccccccccccccccccccccccccccccccccccccc

      Include 'maxarray.inc'
      Include 'selflist.inc'
      Include 'system.inc'

      Double Precision M1,Vnx(Maxtotal),Vny(Maxtotal),
     &     Vnz(Maxtotal),Fdx(Maxtotal),Fdy(Maxtotal),
     &     Fdz(Maxtotal),Vdot,Impx,Impy,Impz,Av(7),Ukin,
     &     Utot,Mytemp,Vxx(Maxtotal),Vyy(Maxtotal),
     &     Vzz(Maxtotal),Eeold,E1,E2,Tttt,Ttime,Scale,
     &     Iter1,Iter2,Vxo(Maxtotal),Vyo(Maxtotal),
     &     Vzo(Maxtotal),Ffx,Ffy,Ffz

      Integer Sstmm,I,J,Kii,Kjj,Ii

Ccccccccccccccccccccccccccccccccccccc
C     Data For The Linked Cellist   C
Ccccccccccccccccccccccccccccccccccccc

      Data Dcx/  0, 1,-1, 0, 1,-1, 0, 1,-1, 0, 1,-1, 0, 1/
      Data Dcy/  0, 0, 1, 1, 1,-1,-1,-1, 0, 0, 0, 1, 1, 1/
      Data Dcz/  0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1/

Cccccccccccccccccccccccccccccc
C     Initialize Rng         C
Cccccccccccccccccccccccccccccc

      M1 = 0.001d0*Dble(Mod((10+10*Sstmm()),1000))

      If(M1.Lt.0.001d0) M1 = 0.001d0
      If(M1.Gt.0.999d0) M1 = 0.999d0

      Call Genrand(M1) 

Ccccccccccccccccccccccc
C     Read In Data    C
Ccccccccccccccccccccccc

      Ukin    = 0.0d0
      Upot    = 0.0d0
      Utot    = 0.0d0
      Surface = 0.0d0
      Press   = 0.0d0
      Mytemp  = 0.0d0
      Mlll    = 0
      
      Call Readdat
      Call Sample_Dens(1)
      Call Sample_Gyra(1)

      Do J=1,7
         Av(7) = 0.0d0
      Enddo

      Ukin    = 0.0d0
      Upot    = 0.0d0
      Utot    = 0.0d0
      Surface = 0.0d0
      Press   = 0.0d0
      Mytemp  = 0.0d0
      Eeold   = 0.0d0
      E1      = 0.0d0
      E2      = 0.0d0
      Mlll    = 0
      Tttt    = Ttime()
      Iter1   = 0.0d0
      Iter2   = 0.0d0
      
Cccccccccccccccccccccccccccccc
C     Loop Over All Cycles   C
Cccccccccccccccccccccccccccccc

      Do I=1,Nstep
         
Ccccccccccccccccccccccccccccc
C     Calculate The Force   C
Ccccccccccccccccccccccccccccc

         Call Force
         Call Sample_Dens(2)

         If(I.Eq.1) Then
            Impx = 0.0d0
            Impy = 0.0d0
            Impz = 0.0d0

            Do J=1,Npart
               Impx = Impx + Vx(J)
               Impy = Impy + Vy(J)
               Impz = Impz + Vz(J)
            Enddo
            
            Write(6,*) 'Initial Impulse X          : ',Impx
            Write(6,*) 'Initial Impulse Y          : ',Impy
            Write(6,*) 'Initial Impulse Z          : ',Impz
            Write(6,*)
            Write(6,*)
            Write(6,'(3A)') '     Nstep           Utot           Ukin',
     &           '           Upot          Press',
     &           '           Temp        Surface'

         Elseif(Mod(I,Nprint).Eq.0) Then
            Write(6,'(I10,6E15.7)') 
     &           I,Utot,Ukin,Upot,Press,Mytemp,Surface
         Endif

         If(I.Gt.Ninit.And.Mod(I,25).Eq.0)
     &        Call Sample_Gyra(2)

         If(I.Gt.Ninit.And.Mod(I,500).Eq.0) Then
            Call Writepdb
         Endif

Cccccccccccccccccccccccccccccccccccc
C     Calculate Energy Drift       C
C     Only Usefull When Sigma=0    C
C     (Normal Md)                  C
Cccccccccccccccccccccccccccccccccccc

         If(I.Le.Ninit) Then
            Eeold = Utot
         Else
            E1 = E1 + Dabs(Utot-Eeold)/Eeold
            E2 = E2 + 1.0d0
         Endif
         
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Itterative Scheme; Add Disipative Force              C
C     Initial Guess For New Velocities                     C
C     Leap-Frog Algorithm                                  C
C                                                          C
C     Vx/Vy/Vz    = Velocity At T-0.5*Timestep             C
C     Vxx/Vyy/Vzz = Velocity At T+0.5*Timestep             C
C     Vnx/Vny/Vnz = Velocity At T                          C
C     Vxo/Vyo/Vzo = Velocity From A Pervious Itteration    C
C                                                          C
C     Fx/Fy/Fz    = Force (Excluding Dragforce) At T       C
C     Fdz         = Dragforce At T                         C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         Do J=1,Npart
            Vnx(J) = Vx(J) + Fx(J)*Halftime
            Vny(J) = Vy(J) + Fy(J)*Halftime
            Vnz(J) = Vz(J) + Fz(J)*Halftime
         Enddo
 
         Do Ii=1,Niter
            Do J=1,Npart
               Fdx(J) = 0.0d0
               Fdy(J) = 0.0d0
               Fdz(J) = 0.0d0
            Enddo

            Do J=1,Selflist
               Kii = Spi(J)
               Kjj = Spj(J)

               Vdot = -Srsq(J)*(Sdpx(J)*(Vnx(Kii)-Vnx(Kjj)) +
     &                          Sdpy(J)*(Vny(Kii)-Vny(Kjj)) +
     &                          Sdpz(J)*(Vnz(Kii)-Vnz(Kjj)))

               Ffx = Vdot*Sdpx(J)
               Ffy = Vdot*Sdpy(J)
               Ffz = Vdot*Sdpz(J)

               Fdx(Kii) = Fdx(Kii) + Ffx
               Fdy(Kii) = Fdy(Kii) + Ffy
               Fdz(Kii) = Fdz(Kii) + Ffz

               Fdx(Kjj) = Fdx(Kjj) - Ffx
               Fdy(Kjj) = Fdy(Kjj) - Ffy
               Fdz(Kjj) = Fdz(Kjj) - Ffz
            Enddo

            Do J=1,Npart
               Vxo(J) = Vnx(J)
               Vyo(J) = Vny(J)
               Vzo(J) = Vnz(J)

               Ffx = Halftime*(Fx(J)+Fdx(J))
               Ffy = Halftime*(Fy(J)+Fdy(J))
               Ffz = Halftime*(Fz(J)+Fdz(J))

               Vnx(J) = Vx(J) + Ffx
               Vny(J) = Vy(J) + Ffy
               Vnz(J) = Vz(J) + Ffz

               Vxx(J) = Vnx(J) + Ffx
               Vyy(J) = Vny(J) + Ffy
               Vzz(J) = Vnz(J) + Ffz
            Enddo
         Enddo

Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Calculate Average Error In This Itterative Scheme    C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         Do J=1,Npart
            Iter2 = Iter2 + 3.0d0
            Iter1 = Iter1 + Dabs(Vxo(J)-Vnx(J)) + 
     &                      Dabs(Vyo(J)-Vny(J)) + 
     &                      Dabs(Vzo(J)-Vnz(J))

         Enddo
         
Ccccccccccccccccccccccccccccccccccc
C     Scale For Initialization    C
Ccccccccccccccccccccccccccccccccccc

         If(I.Lt.Ninit) Then
            Ukin = 0.0d0

            Do J=1,Npart
               Ukin = Ukin + Vnx(J)**2 + 
     &                       Vny(J)**2 + 
     &                       Vnz(J)**2
            Enddo

            Scale = Dsqrt(Dble(3*Npart-3)/(Beta*Ukin))

            Do J=1,Npart
               Vxx(J) = Vxx(J)*Scale
               Vyy(J) = Vyy(J)*Scale
               Vzz(J) = Vzz(J)*Scale
               Vnx(J) = Vnx(J)*Scale
               Vny(J) = Vny(J)*Scale
               Vnz(J) = Vnz(J)*Scale
            Enddo
         Endif

Ccccccccccccccccccccccccccccccccccccccccccccc
C     Calculate New Positions/Velocities    C
C     Put Particles Back In The Box         C
Ccccccccccccccccccccccccccccccccccccccccccccc

         Ukin = 0.0d0

         Do J=1,Npart
            Vx(J) = Vxx(J)
            Vy(J) = Vyy(J)
            Vz(J) = Vzz(J)

            Ukin  = Ukin +  Vnx(J)**2 + 
     &                      Vny(J)**2 + 
     &                      Vnz(J)**2

            Xx(J) = Xx(J) + Timestep*Vx(J)
            Yy(J) = Yy(J) + Timestep*Vy(J)
            Zz(J) = Zz(J) + Timestep*Vz(J)

            If (Xx(J).Ge.Boxlx) Then
               Xx(J) = Xx(J) - Boxlx
            Elseif (Xx(J).Lt.0.0d0) Then
               Xx(J) = Xx(J) + Boxlx
            Endif

            If (Yy(J).Ge.Boxly) Then
               Yy(J) = Yy(J) - Boxly
            Elseif (Yy(J).Lt.0.0d0) Then
               Yy(J) = Yy(J) + Boxly
            Endif

            If (Zz(J).Ge.Boxlz) Then
               Zz(J) = Zz(J) - Boxlz
            Elseif (Zz(J).Lt.0.0d0) Then
               Zz(J) = Zz(J) + Boxlz
            Endif
         Enddo

Ccccccccccccccccccccccccccccc
C     Calculate Averages    C
Ccccccccccccccccccccccccccccc

         Ukin   = 0.5d0*Ukin
         Mytemp = 2.0d0*Ukin/Dble(3*Npart-3)
         Utot   = Upot + Ukin 

         Av(1) = Av(1) + Press
         Av(2) = Av(2) + Mytemp
         Av(3) = Av(3) + Utot
         Av(4) = Av(4) + Ukin
         Av(5) = Av(5) + Upot
         Av(6) = Av(6) + Surface
         Av(7) = Av(7) + 1.0d0
      Enddo

Cccccccccccccccccccccccccccccccc
C     End Of The Simulation    C
C     Calculate Averages And   C
C     Write Info To Screen     C
Cccccccccccccccccccccccccccccccc

      Av(7) = 1.0d0/Av(7)

      Do J=1,6
         Av(J) = Av(J)*Av(7)
      Enddo

      Impx = 0.0d0
      Impy = 0.0d0
      Impz = 0.0d0

      Do J=1,Npart
         Impx = Impx + Vx(J)
         Impy = Impy + Vy(J)
         Impz = Impz + Vz(J)
      Enddo
      
      Write(6,*)
      Write(6,*) 'Final Impulse X            : ',Impx
      Write(6,*) 'Final Impulse Y            : ',Impy
      Write(6,*) 'Final Impulse Z            : ',Impz
      Write(6,*) 'Average Energy Drift       : ',E1/E2
      Write(6,*) 'Error Niter                : ',Iter1/Iter2
      Write(6,*) 'Average Pressure           : ',Av(1)
      Write(6,*) 'Average Temperature        : ',Av(2)
      Write(6,*) 'Average Utot               : ',Av(3)
      Write(6,*) 'Average Ukin               : ',Av(4)
      Write(6,*) 'Average Upot               : ',Av(5)
      Write(6,*) 'Average Surf. Tens.        : ',Av(6)
      Write(6,*) 'Max. Interact.             : ',Mlll
      Write(6,*) 'Elapsed Time [s]           : ',Ttime()-Tttt

Ccccccccccccccccccccccccccccccccccccc
C     Write Final Config To Disk    C
Ccccccccccccccccccccccccccccccccccccc

      Open(21,File="Config")

      Write(21,*) Numcom
      Write(21,*) (Nuall(I),I=1,Numcom)
      Write(21,*) Npart,Nspring,Molecule
      Write(21,*) Boxlx,Boxly,Boxlz
         
      Do I=1,Npart
         Write(21,'(3E20.10,I9)') 
     &        Xx(I),Yy(I),Zz(I),Type(I)
      Enddo

      Do I=1,Nspring
         Write(21,*) Aspring(I)
      Enddo

      Do I=1,Molecule
         Write(21,*) Smolecule(I),Tmolecule(I)
      Enddo

      Close(21)

Cccccccccccccccccccccccccc
C     End Of The Code    C
Cccccccccccccccccccccccccc

      Call Sample_Dens(3)
      Call Sample_Gyra(3)
      Call Writepdb
      Call Exitt(1)

      End
