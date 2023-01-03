      Program Recoil
      Implicit None

      Include 'system.inc'
      
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Cbmc Versus Recoil Growth For Continuous Potentials        C
C     Fortran 77 Code: Programmed By Thijs J.H. Vlugt            C
C                                                                C
C     Special Thanks To Daan Frenkel And Jochem Wichers Hoeth    C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      Integer          Ncycl,Ic,Ndisp,Ncbmc,Icc,Iccc,Nrota,Sstmm

      Double Precision Etot,Etest,Dx,Ran,Ran_Uniform,Movecb,Acccb,Fff,
     &                 Movedis,Accdis,Err,Growok,Av1,Av2,Ttime,T1,
     &                 S1,S2,M1,D1,T2,Moverot,Accrot,Drot
       
Ccccccccccccccccccccccccccccccccccccccccccccccc
C     Initialize System + Sampling Functions  C
Ccccccccccccccccccccccccccccccccccccccccccccccc

      M1 = 0.001d0*Dble(Mod((10+10*Sstmm()),1000))

      If(M1.Lt.0.001d0) M1 = 0.001d0
      If(M1.Gt.0.999d0) M1 = 0.999d0

      Call Genrand(M1) 

      Call Init(Ncycl,Nrota,Ndisp,Ncbmc,Dx,Drot)
      Call Toterg(Etot)

      Call Sample_Gyra(1)
      Call Sample_Rad(1)
      
      Moverot = 0.0d0
      Accrot  = 0.0d0
      Movedis = 0.0d0
      Accdis  = 0.0d0
      Movecb  = 0.0d0
      Acccb   = 0.0d0
      Growok  = 0.0d0
      Av1     = 0.0d0
      Av2     = 0.0d0
      T1      = 0.0d0
      T2      = 0.0d0
      D1      = 0.0d0
      M1      = Ttime()
      
Cccccccccccccccccccccccccccccc
C     Loop Over All Cycles   C
Cccccccccccccccccccccccccccccc

      Do Ic = 1,Ncycl
         Do Icc = 1,10
            
            If(Nchain.Ne.0) Then
            
               Do Iccc = 1,(Ndisp + Ncbmc)
            
                  Ran = Ran_Uniform()*Dble(Ndisp+Ncbmc+Nrota)

                  If (Ran.Le.Dble(Ncbmc)) Then

                     S1 = Ttime()

                     Call Mccbmc(.True.,Growok,Movecb,Acccb,Etot)

                     S2 = Ttime() - S1
                     T1 = T1      + S2
                     T2 = T2      + 1.0d0
                  
                   Elseif(Ran.Le.Dble(Ncbmc+Ndisp)) Then

                     S1 = Ttime()
                     
                     Call Mcmove(Movedis,Accdis,Etot,Dx)
                     
                     S2 = Ttime() - S1
                     D1 = D1      + S2

                  Else

                     S1 = Ttime()
                     
                     Call Mcrota(Moverot,Accrot,Etot,Drot)
                     
                     S2 = Ttime() - S1
                     D1 = D1      + S2
                  Endif
                  
                  If(Ic.Gt.Ncycl/3) Then
                     Av1 = Av1 + Etot
                     Av2 = Av2 + 1.0d0
                  Endif
               Enddo
            Endif

            If(Ic.Gt.Ncycl/3) Then
               Call Sample_Gyra(2)
               Call Sample_Rad(2)
            Endif
         Enddo

         If (Mod(Ic,Ncycl/5).Eq.0) Then

            Call Store(Dx,Drot)
            Call Toterg(Etest)

            If(Dabs(Etot).Lt.1.0d-9.And.Dabs(Etest).Lt.1.0d-9) Then
               Err = 0.0d0
            Else
               Err = Dabs((Etot-Etest)/Etest)
            Endif

            Write(6,*)
            Write(6,*)
            Write(6,*) 'Cycle                         : ',Ic
            Write(6,*)
            
            If(Movedis.Gt.0.5d0) Then
               Write(6,*) 'Number Of Displacements       : ',Movedis
               Write(6,*) 'Accepted                      : ',Accdis
               Write(6,*) 'Percentage                    : ',
     &              100.0d0*Accdis/Movedis
               Write(6,*) 'Maximum Displacement          : ',Dx
               Write(6,*)
            Endif

            If(Moverot.Gt.0.5d0) Then
               Write(6,*) 'Number Of Rotations           : ',Moverot
               Write(6,*) 'Accepted                      : ',Accrot
               Write(6,*) 'Percentage                    : ',
     &              100.0d0*Accrot/Moverot
               Write(6,*) 'Maximum Rotation              : ',Drot
               Write(6,*)
            Endif
       
            If(Movecb.Gt.0.5d0) Then
               Write(6,*) 'Number Of Cbmc Moves          : ',Movecb
               Write(6,*) 'Successful Growth             : ',Growok
               Write(6,*) 'Percentage                    : ',
     &              100.0d0*Growok/Movecb
               Write(6,*) 'Accepted                      : ',Acccb
               Write(6,*) 'Percentage                    : ',
     &              100.0d0*Acccb/Movecb
               Write(6,*) 'Acc Moves Per Time Unit       : ',Acccb/T1
               Write(6,*)
            Endif

            Write(6,*) 'Time Spend In Disp./Rot.      : ',D1
            Write(6,*) 'Time Spend In Cbmc            : ',T1
            Write(6,*) 'Total Time Spend              : ',Ttime()-M1
            Write(6,*) 'Total Energy                  : ',Etot
            Write(6,*) 'Test Energy                   : ',Etest
            Write(6,*) 'Fraction                      : ',Err

            If(Av2.Gt.0.5d0) Then
               Write(6,*)
               Write(6,*) 'Average Energy                : ',Av1/Av2
            Endif        
                        
            If(Err.Gt.1.0d-5.And.
     &           Dabs(Etest).Lt.1.0d7.And.Nchain.Ne.0) Then
               Write(6,*) 'Something Wrong In Energy Conservation'
            Endif

            Etot = Etest

            If(Movedis.Gt.0.001d0) Then
               Fff = 2.0d0*Accdis/Movedis
            Else
               Fff = 1.0d0
            Endif

            If(Fff.Lt.0.5d0) Fff = 0.5d0
            If(Fff.Gt.2.0d0) Fff = 2.0d0

            Dx      = Dx*Fff
            Movedis = 0.0d0
            Accdis  = 0.0d0

            If(Moverot.Gt.0.001d0) Then
               Fff = 2.0d0*Accrot/Moverot
            Else
               Fff = 1.0d0
            Endif
            
            If(Fff.Lt.0.5d0) Fff = 0.5d0
            If(Fff.Gt.2.0d0) Fff = 2.0d0

            Drot    = Drot*Fff
            Moverot = 0.0d0
            Accrot  = 0.0d0

            If(Drot.Lt.1.0d-2)     Drot = 1.0d-2
            If(Drot.Gt.1.5d0)      Drot = 1.5d0
            If(Dx.Lt.(1.0d-4*Box)) Dx   = 1.0d-4*Box
            If(Dx.Gt.(0.5d0*Box))  Dx   = 0.5d0*Box

            Call Sample_Gyra(3)
            Call Sample_Rad(3)
            Call Store(Dx,Drot)
         Endif
      Enddo
      
      Call Sample_Gyra(3)
      Call Sample_Rad(3)
      Call Store(Dx,Drot)

      Stop
      End
