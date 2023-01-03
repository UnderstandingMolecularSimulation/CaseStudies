      Subroutine Initlat
      Implicit None

      Include 'system.inc'

Ccccccccccccccccccccccccccccccccccccccccccccc
C     Generate Initial Config; Everything   C
C     Without Overlap Is Ok !!              C
C     Use Cbmc To Generate Initial Config   C
Ccccccccccccccccccccccccccccccccccccccccccccc
            
      Integer Nchainold,I
      Logical Lovrlap

      Double Precision Xn(Ellmax),Yn(Ellmax),Zn(Ellmax),D1,D2,
     &                 Etot,Dx,Move,Acc,Ran_Uniform,Growok,Ran

      If(Nchain.Eq.0) Return

      Nchainold = Nchain
      Nchain    = 0
     
 10   If(Lrecoil) Then
         Call Grow_Recoil(Lovrlap,.False.,D1,Xn,Yn,Zn,0)
      Else
         Call Grow_Cbmc(Lovrlap,.False.,D1,Xn,Yn,Zn,0,D2)
      Endif

      If(Lovrlap) Goto 10

      Nchain = Nchain + 1

      Do I=1,Ell
         X(I,Nchain) = Xn(I)
         Y(I,Nchain) = Yn(I)
         Z(I,Nchain) = Zn(I)
      Enddo

      If(Nchain.Ne.Nchainold) Goto 10

Cccccccccccccccccccccccccccccccccccccccccc
C     Lower The Energy By Translations   C
Cccccccccccccccccccccccccccccccccccccccccc

      Etot   = 0.0d0
      Dx     = 0.1d0
      Move   = 0.0d0
      Acc    = 0.0d0
      Growok = 0.0d0

      Do I=1,(10*Nchain)

         Ran = Ran_Uniform()

         If(Ran.Lt.0.25d0) Then
            Call Mcmove(Move,Acc,Etot,Dx)
         Elseif(Ran.Lt.0.50d0) Then
            Call Mcrota(Move,Acc,Etot,Dx)
         Else
            Call Mccbmc(.False.,Growok,Move,Acc,Etot)
         Endif
      Enddo
      
      Return
      End
