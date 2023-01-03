      Subroutine Readdat
      Implicit None

Cccccccccccccccccccccccccccccc
C     Read Stuff From Disk   C
Cccccccccccccccccccccccccccccc

      Include 'maxarray.inc'
      Include 'system.inc'

      Integer I,Ii,J,Ninact,Many,Typmol(Maxtotal)

      Logical Linit

      Double Precision Temp,Sigma,Dummy,
     &     Ran_Uniform,Ran_Gauss,Ukin,
     &     Impx,Impy,Impz,X,Y,Z,Scale

Cccccccccccccccccccccccccccccc
C     Read Stuff From Disk   C
Cccccccccccccccccccccccccccccc

      Do I=1,Maxtp
         Do Ii=1,Maxtp
            Phrep(I,Ii) = 0.0d0
         Enddo
      Enddo

      Write(6,*)
      Write(6,*) 'Dpd Simulation'
      Write(6,*)
      Write(6,*)

      Open(21,File="Input")

      Read(21,*)
      Read(21,*) Nstep,Nprint,Ninit
      Read(21,*)
      Read(21,*) Boxlx,Boxly,Boxlz,Temp
      Read(21,*)
      Read(21,*) Bondkb,Sigma,Timestep
      Read(21,*)
      Read(21,*) Linit,Ninact,Niter
      
      If(Ninit.Lt.2)   Ninit  = 2
      If(Nprint.Lt.10) Nprint = 10

      Write(6,*) 'Number Of Steps            : ',Nstep
      Write(6,*) 'Number Of Init.            : ',Ninit
      Write(6,*) 'Output Freq.               : ',Nprint
      Write(6,*) 'Temperature                : ',Temp
      Write(6,*) 'Beta                       : ',
     &     1.0d0/Temp

      Write(6,*) 'Bondkb                     : ',Bondkb
      Write(6,*) 'Sigma                      : ',Sigma
      Write(6,*) 'Gamma                      : ',
     &     0.5d0*Sigma*Sigma/Temp

      Write(6,*) 'Timestep                   : ',Timestep
      Write(6,*) 'Niter                      : ',Niter
      
      If(Niter.Lt.1.Or.Niter.Gt.25) Then
         Write(6,*) 'Niter Out Of Range !!'
         Call Exitt(2)
      Endif

      If(Linit) Then
         Write(6,*) 'Read From Disk             :  False'
      Else
         Write(6,*) 'Read From Disk             :  True'
      Endif

      Write(6,*) 'Number Of Interactions     : ',Ninact

      Do J=1,Ninact
         Read(21,*)
         Read(21,*) I,Ii,Dummy

         If(Min(I,Ii).Lt.1.Or.Max(I,Ii).Gt.Maxtp) Then
            Write(6,*) 'Error In Interaction !!'
            Call Exitt(2)
         Endif

         Phrep(I,Ii) = Dummy
         Phrep(Ii,I) = Dummy

         Write(6,'(A,2i7,E20.10)') ' Type [I,J], Value          :  ',
     &        I,Ii,Dummy
      Enddo

      Close(21)

Cccccccccccccccccccccccccc
C     Initialize Stuff   C
Cccccccccccccccccccccccccc

      Beta     = 1.0d0/Temp
      Sigmat   = Dsqrt(12.0d0)*Sigma/Dsqrt(Timestep)
      Gamma    = 0.5d0*Sigma*Sigma*Beta
      Halftime = 0.5d0*Timestep

      If(.Not.Linit) Then

Cccccccccccccccccccccccccccccccccccc
C     Read Old Config From Disk    C
Cccccccccccccccccccccccccccccccccccc

         Open(21,File="Config")
         
         Read(21,*) Numcom
         Read(21,*) (Nuall(I),I=1,Numcom)

         Write(6,*) 'Number Of Components       : ',Numcom

         If(Numcom.Lt.1.Or.Numcom.Gt.Maxcom) Then
            Write(6,*) 'Wrong Number Of Components !!'
            Call Exitt(2)
         Endif

         Read(21,*) Npart,Nspring,Molecule
         Read(21,*) Boxlx,Boxly,Boxlz
         
         If(Max(Npart,Nspring,Molecule).Gt.Maxtotal.Or.
     &        Min(Npart,Molecule).Lt.1) Then

            Write(6,*) 'Npart-Nspring-Molecules Is Out Of Range !!'
            Call Exitt(2)
         Endif

         Do I=1,Npart
            Read(21,*) Xx(I),Yy(I),Zz(I),Type(I)
         Enddo

         Do I=1,Nspring
            Read(21,*) Aspring(I)
         Enddo

         Do I=1,Molecule
            Read(21,*) Smolecule(I),Tmolecule(I)
         Enddo

         Close(21)
      
      Else

Ccccccccccccccccccccccccccccccccccccccccccc
C     Generate Random Initial Config      C
C     Later, The Energy Of This Config    C
C     Is Minimized...                     C
Ccccccccccccccccccccccccccccccccccccccccccc

         Open(21,File="Molec")
         
         Npart    = 0
         Nspring  = 0
         Molecule = 0

         Read(21,*)
         Read(21,*) Numcom

         Write(6,*) 'Number Of Components       : ',Numcom

         If(Numcom.Lt.1.Or.Numcom.Gt.Maxcom) Then
            Write(6,*) 'Wrong Number Of Components !!'
            Call Exitt(2)
         Endif

         Do Ii=1,Numcom
            Read(21,*)
            Read(21,*) Nuall(Ii),Many
            Read(21,*)
            Read(21,*) (Typmol(I),I=1,Nuall(Ii))

            Write(6,*)
            Write(6,*)
            Write(6,*) 'Component Number           : ',Ii
            Write(6,*) 'Number Of Molecules        : ',Many
            Write(6,*) 'Number Of Atoms/Molec      : ',Nuall(Ii)
            
            If(Nuall(Ii).Lt.1.Or.Nuall(Ii).Gt.Maxtotal) Then
               Write(6,*) 'Nuall Out Of Range !!!'
               Call Exitt(2)
            Endif

            Do I=1,Nuall(Ii)
               Write(6,*) 'Number Bead, Type          : ',I,Typmol(I)
               
               If(Typmol(I).Lt.1.Or.Typmol(I).Gt.Maxtp) Then
                  Write(6,*) 'Type Out Of Order !!'
                  Call Exitt(2)
               Endif
            Enddo

            Write(6,*)

            Do J=1,Many
               Do I=1,Nuall(Ii)
                  Npart = Npart + 1

                  If(Npart.Gt.Maxtotal) Then
                     Write(6,*) 'Npart Is Out Of Range !!'
                     Call Exitt(2)
                  Endif

                  Type(Npart) = Typmol(I)
                        
Cccccccccccccccccccccccccccccccccccccccc
C     First Atom: Random In The Box    C
C     Next Atoms; Random Unit Vector   C
C                 Create A Bond With   C
C                 Previous Bead        C
Cccccccccccccccccccccccccccccccccccccccc
            
                  If(I.Eq.1) Then
                     Molecule            = Molecule + 1
                     Smolecule(Molecule) = Npart
                     Tmolecule(Molecule) = Ii

                     Xx(Npart) = Ran_Uniform()*Boxlx
                     Yy(Npart) = Ran_Uniform()*Boxly
                     Zz(Npart) = Ran_Uniform()*Boxlz
                  Else
                     Call Ran_Sphere(X,Y,Z)
                     
                     Xx(Npart) = Xx(Npart-1) + X
                     Yy(Npart) = Yy(Npart-1) + Y
                     Zz(Npart) = Zz(Npart-1) + Z
                     
                     Nspring          = Nspring + 1
                     Aspring(Nspring) = Npart
                  Endif

Cccccccccccccccccccccccccccccccccccccc
C     Put Particle Back In The Box   C
Cccccccccccccccccccccccccccccccccccccc  

                  If (Xx(Npart).Ge.Boxlx) Then
                     Xx(Npart) = Xx(Npart) - Boxlx
                  Elseif (Xx(Npart).Lt.0.0d0) Then
                     Xx(Npart) = Xx(Npart) + Boxlx
                  Endif
                  
                  If (Yy(Npart).Ge.Boxly) Then
                     Yy(Npart) = Yy(Npart) - Boxly
                  Elseif (Yy(Npart).Lt.0.0d0) Then
                     Yy(Npart) = Yy(Npart) + Boxly
                  Endif

                  If (Zz(Npart).Ge.Boxlz) Then
                     Zz(Npart) = Zz(Npart) - Boxlz
                  Elseif (Zz(Npart).Lt.0.0d0) Then
                     Zz(Npart) = Zz(Npart) + Boxlz
                  Endif
               Enddo
            Enddo
         Enddo
         
         Close(21)
      Endif

      Write(6,*) 'Boxlx                      : ',Boxlx
      Write(6,*) 'Boxly                      : ',Boxly
      Write(6,*) 'Boxlz                      : ',Boxlz
      Write(6,*) 'Total Number Of Particles  : ',Npart
      Write(6,*) 'Total Number Of Molecules  : ',Molecule
      Write(6,*) 'Total Number Of Bonds      : ',Nspring
      Write(6,*) 'Segment Density            : ',
     &     Dble(Npart)/(Boxlx*Boxly*Boxlz)
      
Cccccccccccccccccccccccccccccc
C     Generate Velocities    C
C     Set Impulse To Zero    C
Cccccccccccccccccccccccccccccc

      Impx = 0.0d0
      Impy = 0.0d0
      Impz = 0.0d0

      Do I=1,Npart
         Vx(I) = Ran_Gauss()
         Vy(I) = Ran_Gauss()
         Vz(I) = Ran_Gauss()
         Impx  = Impx + Vx(I)
         Impy  = Impy + Vy(I)
         Impz  = Impz + Vz(I)
      Enddo

      Impx = Impx/Dble(Npart)
      Impy = Impy/Dble(Npart)
      Impz = Impz/Dble(Npart)
      Ukin = 0.0d0
 
      Do I = 1,Npart
         Vx(I) = Vx(I) - Impx
         Vy(I) = Vy(I) - Impy
         Vz(I) = Vz(I) - Impz
 
         Ukin = Ukin + Vx(I)**2 + Vy(I)**2 + Vz(I)**2
      Enddo
 
      Scale = Dsqrt(Dble(3*Npart-3)/(Beta*Ukin))
 
      Do I = 1,Npart
         Vx(I) = Vx(I)*Scale
         Vy(I) = Vy(I)*Scale
         Vz(I) = Vz(I)*Scale
      Enddo

Cccccccccccccccccccccccccccc
C     Cellist Parameters   C
Cccccccccccccccccccccccccccc

      Hbx   = 0.5d0*Boxlx
      Hby   = 0.5d0*Boxly
      Hbz   = 0.5d0*Boxlz
      Ncx   = Max(3,Idint(Boxlx))
      Ncy   = Max(3,Idint(Boxly))
      Ncz   = Max(3,Idint(Boxlz))
      Rlx   = Dble(Ncx)/Boxlx
      Rly   = Dble(Ncy)/Boxly
      Rlz   = Dble(Ncz)/Boxlz
      Ncell = Ncx*Ncy*Ncz
      
      Write(6,*) 'Number Of Cells X-Dir      : ',Ncx
      Write(6,*) 'Number Of Cells Y-Dir      : ',Ncy
      Write(6,*) 'Number Of Cells Z-Dir      : ',Ncz
      Write(6,*) 'Total Number Cells         : ',Ncell

      If(Ncell.Gt.Maxcell) Then
         Write(6,*) 'Too Many Cells !!!'
         Call Exitt(2)
      Endif

Ccccccccccccccccccccccccccccccc
C     Minimize The Energy     C
Ccccccccccccccccccccccccccccccc

      If(Linit) Call Minim

Cccccccccccccccccccccccccccccccccccccccc
C     Put Particles Back In The Box    C
Cccccccccccccccccccccccccccccccccccccccc

      Do J = 1,Npart
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

      Return
      End
