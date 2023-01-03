      Subroutine Init(Ncycl,Nrota,Ndisp,Ncbmc,Dx,Drot)
      Implicit None

      Include 'system.inc'

Cccccccccccccccccccccccccccc
C     Read In System Data  C
Cccccccccccccccccccccccccccc
            
      Logical          Lattice
      Integer          Ncycl,Ndisp,Ncbmc,I,J,Nrota

      Double Precision Temp,Rho,Vol,Dx,Etot,Q1,Q2,Q3,Q4,
     &                 Xn(Ellmax),Yn(Ellmax),Zn(Ellmax),
     &                 Drot
 
Ccccccccccccccccccc
C     Read Data   C
Ccccccccccccccccccc

      Open(24,File="input")

      Read(24,*)
      Read(24,*) Lrecoil,Lattice,Ncycl
      Read(24,*)
      Read(24,*) Nrota,Ndisp,Ncbmc
      Read(24,*)
      Read(24,*) Ell,Nchain,Box,Temp
      Read(24,*)
      Read(24,*) D_Second,Nrecoil
      Read(24,*)
      Read(24,*) Rccc,Dx,Drot
      Read(24,*)
      Read(24,*) Bondl,ThetaT
      
      Close(24)

      If(Lrecoil) Then
         Write(6,*) 'Recoil Growth !!!'
         Write(6,*)
      Else
         Write(6,*) 'Cbmc          !!!'
      Endif

      Write(6,*)
      Write(6,*)
 
Ccccccccccccccccccccccccc
C     Check Some Stuff  C
Ccccccccccccccccccccccccc

      If(Nchain.Gt.Npmax.Or.Nchain.Lt.0) Then
         Write(6,*) 'Too Many Chains !!!'
         Stop
      Elseif(Ncycl.Lt.5) Then
         Write(6,*) 'At Least 5 Cycles !!!'
         Stop
      Elseif(Bondl.Le.0.0d0) Then
         Write(6,*) 'Bond-Length Too Small !!!'
         Stop
      Elseif(D_Second.Lt.1) Then
         Write(6,*) 'Too Little Number K !!!'
         Stop
      Elseif(D_Second.Gt.Kmax) Then
         Write(6,*) 'Too Large Number K !!!'
         Stop
      Elseif(2.0d0*Rccc.Ge.Box) Then
         Write(6,*) 'Box Is Too Small !!!'
         Stop
      Elseif(Temp.Lt.0.00001d0) Then
         Write(6,*) 'Temp Is Too Low !!!'
         Stop
      Elseif(Lrecoil.And.(Nrecoil.Ge.Ell.And.Ncbmc.Ne.0)) Then
         Write(6,*) 'Nrecoil Too Large !!!'
         Stop
      Elseif(Thetat.Lt.0.1d0.Or.Thetat.Gt.3.0d0) Then
         Write(6,*) 'Thetat Out Of Range !!!'
         Stop
      Elseif(Nrecoil.Le.0) Then
         Write(6,*) 'Nrecoil Too Low !!!'
      Endif

Cccccccccccccccccccccccccccccc
C     Determine Variables    C
Cccccccccccccccccccccccccccccc

      Beta     = 1.0d0/Temp
      Vol      = Box*Box*Box
      Boxi     = 1.0d0/Box
      Rho      = Dble(Nchain)/Vol
      Rc2      = Rccc*Rccc
                  
Cccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Generate Coordinates Or Read Them From Disk  C
C     Reset Bond Lengths                           C
Cccccccccccccccccccccccccccccccccccccccccccccccccccc

      If (Lattice) Then
         Call Initlat
      Else

         Open(24,File="chain.res")

         Read(24,*) Box,Nchain,Ell,Dx,Drot

         Boxi   = 1.0d0/Box
         Vol    = Box*Box*Box
         Rho    = Dble(Nchain)/Vol
         
         Do I = 1,Nchain
            Do J = 1,Ell
               Read(24,*) X(J,I),Y(J,I),Z(J,I)

               If(J.Ge.2) Then
                  Q1 = X(J,I) - X(J-1,I)
                  Q2 = Y(J,I) - Y(J-1,I)
                  Q3 = Z(J,I) - Z(J-1,I)

                  Q4 = Bondl/Dsqrt(Q1*Q1 + Q2*Q2 + Q3*Q3)

                  X(J,I) = X(J-1,I) + Q1*Q4
                  Y(J,I) = Y(J-1,I) + Q2*Q4
                  Z(J,I) = Z(J-1,I) + Q3*Q4
               Endif

               Xn(J) = X(J,I)
               Yn(J) = Y(J,I)
               Zn(J) = Z(J,I)
            Enddo

            Call Backbox(Xn,Yn,Zn)

         Enddo

         Close(24)

      Endif

      If(Dsqrt(Rc2).Gt.0.5d0*Box) Then
         Write(6,*) 'Rcut Is Too Large !!!'
         Stop
      Endif
      
Cccccccccccccccccccccccccccccccc
C     Calculate Total Energy   C
Cccccccccccccccccccccccccccccccc

      Call Toterg(Etot)
      
Cccccccccccccccccccc
C     Write Info   C
Cccccccccccccccccccc

      Write (6,99001) Nchain,Ell,Temp,Rho,Rho*Dble(Ell),Box,Rccc,Bondl,
     &                ThetaT,Ncycl,Ndisp,Ncbmc,Nrota,Dx,Drot,Nrecoil,
     &                D_Second,Etot

99001 Format (' Number Of Particles           : ',I20,/,
     &     ' Chain Length                  : ',I20,/,
     &     ' Temperature                   : ',F20.5,/,
     &     ' Density                       : ',F20.5,/,
     &     ' Segment Density               : ',F20.5,/,
     &     ' Box Length                    : ',F20.5,/,
     &     ' Cut-Off Radius                : ',F20.5,/,
     &     ' Bond-Length                   : ',F20.5,/,
     &     ' ThetaT                        : ',F20.5,/,
     &     ' Number Of Monte Carlo Cycles  : ',I20,/,
     &     ' Number Of Disp Per Cycle      : ',I20,/,
     &     ' Number Of Cbmc Per Cycle      : ',I20,/,
     &     ' Number Of Rot Per Cycle       : ',I20,/,
     &     ' Max Displ                     : ',F20.5,/,
     &     ' Max Rot.                      : ',F20.5,/,
     &     ' Recoil Length                 : ',I20,/,
     &     ' Number Of Trials              : ',I20,/,
     &     ' Total Initial Energy          : ',G20.5,///)

      Return 
      End
