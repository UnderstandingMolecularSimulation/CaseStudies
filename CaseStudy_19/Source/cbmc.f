      Program Cbmc
      Implicit None

      Include 'system.inc'
            
Cccccccccccccccccccccccccccccccccccccccccccccccc
C     Casestudy 20: Cbmc Of An Ideal Chain     C
C     With Bond Stretching...                  C
Cccccccccccccccccccccccccccccccccccccccccccccccc

      Integer I,Ic,Icc,Nstep,Ninit,Sstmm
      Double Precision Movecb,Acccb,Av1,Av2,Urig,M1
       
Ccccccccccccccccccccccccccccccccccccccccccccccc
C     Initialize System + Sampling Functions  C
Ccccccccccccccccccccccccccccccccccccccccccccccc

      M1 = 0.001d0*Dble(Mod((10+10*Sstmm()),1000))

      If(M1.Lt.0.001d0) M1 = 0.001d0
      If(M1.Gt.0.999d0) M1 = 0.999d0

      Call Genrand(M1) 

      Open(21,File='input')

      Read(21,*)
      Read(21,*) Nstep,Ninit,Ladvanced
      Read(21,*)
      Read(21,*) Ell,Kv,Beta,Nchoi

      Close(21)

      Write(6,*) 'Number Of Cycles              : ',Nstep
      Write(6,*) 'Numberof Init.                : ',Ninit
      Write(6,*) 'Kv                            : ',Kv
      Write(6,*) 'Beta                          : ',Beta
      Write(6,*) 'Chainlength                   : ',Ell
      Write(6,*) 'Nchoi                         : ',Nchoi
      Write(6,*)

      If(Ell.Le.1.Or.Ell.Gt.Ellmax.Or.
     &     Nchoi.Lt.1.Or.Nchoi.Gt.Kmax.Or.
     &     Kv.Le.0.0d0.Or.Ninit.Ge.Nstep) Stop

      If(Ladvanced) Then
         Write(6,*) 'Advanced (Second) Scheme Is Used'
      Else
         Write(6,*) 'Naive Scheme Is Used'
      Endif

      Call Initlat
      Call Sample_Gyra(1)
      Call Sample_Bond(1)
      
      Movecb = 0.0d0
      Acccb  = 0.0d0
      Av1    = 0.0d0
      Av2    = 0.0d0
      
Cccccccccccccccccccccccccccccc
C     Loop Over All Cycles   C
Cccccccccccccccccccccccccccccc

      Do Ic = 1,Nstep
         Do Icc = 1,1000
            
            Call Mccbmc(Movecb,Acccb)

            If(Ic.Gt.Ninit) Then
               If(Ladvanced.Or.Mod(Icc,5).Eq.0) Then
                  Call Sample_Gyra(2)
                  Call Sample_Bond(2)

                  Av1 = Av1 + Utot
                  Av2 = Av2 + 1.0d0
               Endif
            Endif
         Enddo
      Enddo

      Urig = 0.0d0

      Do I=2,Ell
         Urig = Urig + 0.5d0*Kv*
     &        ((Dsqrt((X(I) - X(I-1))**2 + 
     &                (Y(I) - Y(I-1))**2 + 
     &                (Z(I) - Z(I-1))**2) - 1.0d0)**2)
      Enddo

      Write(6,*)
      Write(6,*)
      Write(6,*) 'Energy (Simulation)           : ',Utot
      Write(6,*) 'Energy (Rigor)                : ',Urig
      Write(6,*) 'Number Of Cbmc Moves          : ',Movecb
      Write(6,*) 'Accepted                      : ',Acccb
      Write(6,*) 'Percentage                    : ',
     &     100.0d0*Acccb/Movecb
      Write(6,*) 'Average Energy                : ',Av1/Av2
       
      Call Sample_Bond(3)
      Call Sample_Gyra(3)
      
      Stop
      End
