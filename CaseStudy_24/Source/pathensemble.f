      Subroutine Pathensemble
      Implicit None

      Include 'system.inc'
      
Cccccccccccccccccccccccccccccccccccccccc
C     Generates An Initial Path        C
C     Input: Molecule At An Arbitrary  C
C     Position                         C
Cccccccccccccccccccccccccccccccccccccccc

      Integer          I,J,Islice
      Logical          Lsuccess,Ldisk

      Double Precision Ran_Uniform,Pshift,Rm,M1,M2,
     &                 Frac,S1,S2,Mm1,Ttime

Cccccccccccccccccccccccccccccccccc
C     Read Path-Sampling Input   C
Cccccccccccccccccccccccccccccccccc

      Open(32,File='input')

      Read(32,*)
      Read(32,*) Ldisk,Pshift,Radius
      Read(32,*)
      Read(32,*) Ncycle,Nminit,Nslice
      Read(32,*)
      Read(32,*) Lumbrella,Lmin,Lmax
      Read(32,*)
      Read(32,*) Temp,Deltap,Tstep

      Beta   = 1.0d0/Temp
      Tstep2 = 0.5d0*Tstep*Tstep
      Tstep3 = 0.5d0*Tstep
      
      Close(32)

      Write(6,*)
      Write(6,*) 
      Write(6,*) 'Prob. Shifting                         : ',
     &     Pshift
      Write(6,*) 'Number Of Slices                       : ',
     &     Nslice
      Write(6,*) 'Number Of Mc Cycles                    : ',
     &     Ncycle
      Write(6,*) 'Radius                                 : ',
     &     Radius
      Write(6,*) 'Temperature                            : ',
     &     Temp
      Write(6,*) 'Beta                                   : ',
     &     Beta
      Write(6,*) 'Impulse Difference                     : ',
     &     Deltap
      Write(6,*) 'Tstep                                  : ',
     &     Tstep
      Write(6,*)
      Write(6,*)

      If(Lumbrella) Then
         Write(6,*)
         Write(6,*) 'Umbrella Sampling !!!'
         Write(6,*)
         Write(6,*) 'Lmin                                   : ',
     &        Lmin
         Write(6,*) 'Lmax                                   : ',
     &        Lmax
         Write(6,*)

         If(Lmin.Ge.Lmax) Then
            Write(6,*) 'Lmin < Lmax !!!'
            Call Exitt(2)
         Endif
      Endif

      If(Nslice.Ge.Maxtraject) Then
         Write(6,*) 'Nslice Too Large !!!'
         Call Exitt(2)
      Elseif(Radius.Lt.0.01d0.Or.Radius.Gt.0.9d0) Then
         Write(6,*) 'Radius Out Of Range !!!'
         Call Exitt(2)
      Endif

Cccccccccccccccccccccccccccccc
C     Set Verything To Zero  C
Cccccccccccccccccccccccccccccc

      S1     = 0.0d0
      S2     = 0.0d0
      Mm1    = 0.0d0
      Shoot1 = 0.0d0
      Shoot2 = 0.0d0
      Shoota = 0.0d0
      Shootb = 0.0d0
      Shift1 = 0.0d0
      Shift2 = 0.0d0
      Drift1 = 0.0d0
      Drift2 = 0.0d0
            
      Do J=1,Maxtraject
         La_Old(J) = .False.
         Lb_Old(J) = .False.
         Eeold(J)  = 0.0d0
         Xxold(J)  = 0.0d0
         Yyold(J)  = 0.0d0
         Vxold(J)  = 0.0d0
         Vyold(J)  = 0.0d0
      Enddo
            
      If(Ldisk) Then

Ccccccccccccccccccccccccccccc
C     Read Stuff From Disk  C
Ccccccccccccccccccccccccccccc

         Call Readpath

      Else

Cccccccccccccccccccccccccccc
C     Initial Coordinates  C
Cccccccccccccccccccccccccccc
      
 100     Rxx           = 0.5d0*(Ran_Uniform()-0.5d0)
         Ryy           = 3.0d0*(Ran_Uniform()-0.5d0)
         Islice        = Nslice/2
         Xxold(Islice) = Rxx
         Yyold(Islice) = Ryy
         Xxtra(1)      = 0.0d0
         Yytra(1)      = 0.0d0
         Xxtra(Nslice) = 0.0d0
         Yytra(Nslice) = 0.0d0
         
         Call Shooting(.True.,Lsuccess)
         
         If(.Not.Lsuccess) Then

            Write(6,*)
            Write(6,*) 'Initial Path Failed'
            Write(6,*)
            Write(6,'(A,2e20.10)') 'A : ',Xxtra(1),
     &                                    Yytra(1)
                                                
            Write(6,'(A,2e20.10)') 'B : ',Xxtra(Nslice),
     &                                    Yytra(Nslice)
            Write(6,*)

            Xxtra(1)      = 0.0d0
            Yytra(1)      = 0.0d0
            Xxtra(Nslice) = 0.0d0
            Yytra(Nslice) = 0.0d0
            
            Goto 100
         Endif
      Endif


Cccccccccccccccccccccccccccccccccccccccc
C     Print Initial Path...Energies    C
Cccccccccccccccccccccccccccccccccccccccc

      Write(6,*)
      Write(6,*) 'Succeeded In Finding A Path'
      Write(6,*)
      
      Write(6,'(A,2e20.10)') 'A : ',Xxold(1),
     &                              Yyold(1)
     
      Write(6,'(A,2e20.10)') 'B : ',Xxold(Nslice),
     &                              Yyold(Nslice)

      Write(6,*)
      Write(6,*)

      If(.Not.Ldisk) Then
         Call Writepath
         Call Exitt(1)
      Else
         S1     = 0.0d0
         S2     = 0.0d0
         Mm1    = 0.0d0
         Shoot1 = 0.0d0
         Shoot2 = 0.0d0
         Shoota = 0.0d0
         Shootb = 0.0d0
         Shift1 = 0.0d0
         Shift2 = 0.0d0
         Drift1 = 0.0d0
         Drift2 = 0.0d0
      Endif
      
Ccccccccccccccccccccccccccccccccccc
C     Loop Over All Cycles        C
C     Set Calc. Averages To Zero  C
Ccccccccccccccccccccccccccccccccccc

      If(Lumbrella) Then
         Call Sample_Umbrella(1)
      Else
         Call Sample_Avhb(1)
      Endif

      Write(6,*)
      Write(6,*)

      Do I=1,Ncycle
      
Ccccccccccccccccccccccccccccc
C     Choose A Trialmove    C
C     Choose A System       C
Ccccccccccccccccccccccccccccc

         Rm  = Ran_Uniform()
         Mm1 = Ttime()

Cccccccccccccccccccccccccccc
C     Here Are The Moves   C
Cccccccccccccccccccccccccccc

         If(Rm.Lt.Pshift) Then

Cccccccccccccccccccccccc
C     Shifting         C
Cccccccccccccccccccccccc
                              
            Call Shifting

            Mm1 = Ttime() - Mm1
            S1  = S1      + Mm1
                           
         Else

Cccccccccccccccccccccccc
C     Shooting         C
Cccccccccccccccccccccccc

            Call Shooting(.False.,Lsuccess)

            Mm1 = Ttime() - Mm1
            S2  = S2      + Mm1
            
         Endif

Ccccccccccccccccccccccccc
C     Sample Averages   C
Ccccccccccccccccccccccccc            

         If(I.Gt.Nminit) Then
            If(Mod(I,5).Eq.0) Then

               If(Lumbrella) Then
                  Call Sample_Umbrella(2)
               Else
                  Call Sample_Avhb(2)
               Endif
            Endif
         
            If((Mod(I,50000).Eq.0).Or.
     &           (I.Eq.Ncycle).Or.(I.Eq.1)) Then
               Call Movie
               Call Writepath
               
               If(Lumbrella) Then
                  Call Sample_Umbrella(3)
               Else
                  Call Sample_Avhb(3)
               Endif
            Endif
         Endif
         
Ccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Adjust Maximum Shooting Momentum Change     C
C     Same For Displacement And Rotation          C
Ccccccccccccccccccccccccccccccccccccccccccccccccccc

         If(Shootb.Gt.2000.0d0) Then
            Frac = 4.0d0*Shoota/Shootb

            If(Frac.Gt.2.0d0) Frac = 2.0d0
            If(Frac.Lt.0.5d0) Frac = 0.5d0
            
            Deltap = Deltap*Frac
            Shoota = 0.0d0
            Shootb = 0.0d0
         Endif
                  
         If((Mod(I,50000).Eq.0).Or.(I.Eq.1).Or.(I.Eq.Ncycle)) 
     &        Then
            Write(6,*)
            Write(6,*) 'Cycle                      : ',I
            Write(6,*)
            Write(6,*) 'Cpu Spend In Shifting      : ',S1
            Write(6,*) 'Cpu Spend In Shooting      : ',S2
      
            Write(6,*) 'Number Of Shifting Moves   : ',
     &           Shift2
      
            If(Shift2.Gt.0.5d0) Write(6,*) 
     &           'Fraction Accepted Shifting : ',
     &           Shift1/Shift2
              
            Write(6,*) 'Number Of Shooting Moves   : ',
     &           Shoot2

            If(Shoot2.Gt.0.5d0) Write(6,*) 
     &           'Fraction Accepted Shooting : ',
     &           Shoot1/Shoot2

            Write(6,*) 'Deltap (Shoot)             : ',
     &           Deltap
                  
            If(Drift2.Gt.0.5d0) Write(6,*) 
     &           'Average Energy Drift       : ',
     &           Drift1/Drift2
    
            M1 = Eeold(1)
            M2 = Eeold(1)

            Do J=1,Nslice
               M1 = Max(M1,Eeold(J))
               M2 = Min(M2,Eeold(J))
            Enddo

            Write(6,*) 'Path Energy Drift          : ',
     &           Dabs(M1-M2)/Dabs(Eeold(1))
            Write(6,*) 'Path Energy                : ',
     &           Eeold(1)

            Write(6,*)
            Write(6,*)
         Endif
      Enddo

      Return
      End
