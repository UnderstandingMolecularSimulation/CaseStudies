      Subroutine Mccbmc(Lprint,Growok,Move,Acc,Etot)
      Implicit None

      Include 'system.inc'

Ccccccccccccccccccccccccccccccccccc
C     Regrow A Chain Using Cbmc   C
Ccccccccccccccccccccccccccccccccccc
      
      Integer          I,Ichain
      Logical          Lovrlap,Lready,Lprint
      
      Double Precision Wn,Wo,Ran_Uniform,Etot,Ennew,Enold,
     &                 Xn(Ellmax),Yn(Ellmax),Zn(Ellmax),Acc,Move,
     &                 Xnn(Ellmax),Ynn(Ellmax),Znn(Ellmax),Growok
       
Cccccccccccccccccccccccc
C     Select A Chain   C
Cccccccccccccccccccccccc

      Ichain = Idint(Ran_Uniform()*Dble(Nchain)) + 1
      Move   = Move + 1.0d0
      Lready = .False.
      Ennew  = 0.0d0
      Enold  = 0.0d0

Ccccccccccccccccccc
C     New Chain   C
Ccccccccccccccccccc

      If(Lrecoil) Then
         Call Grow_Recoil(Lovrlap,.False.,Wn,Xn,Yn,Zn,Ichain)
      Else
         Call Grow_Cbmc(Lovrlap,.False.,Wn,Xn,Yn,Zn,Ichain,Ennew)
      Endif

      If(Lovrlap) Return

      Growok = Growok + 1.0d0

Ccccccccccccccccccc
C     Old Chain   C
Ccccccccccccccccccc

      Do I=1,Ell
         Xnn(I) = X(I,Ichain)
         Ynn(I) = Y(I,Ichain)
         Znn(I) = Z(I,Ichain)
      Enddo

      If(Lrecoil) Then
         Call Grow_Recoil(Lovrlap,.True.,Wo,Xnn,Ynn,Znn,Ichain)
      Else
         Call Grow_Cbmc(Lovrlap,.True.,Wo,Xnn,Ynn,Znn,Ichain,Enold)
      Endif

      Do I=1,Ell
         Xnn(I) = X(I,Ichain)
         Ynn(I) = Y(I,Ichain)
         Znn(I) = Z(I,Ichain)
      Enddo

      If(Lovrlap) Then

Ccccccccccccccccccccccccccccccccccccccccccccccc
C     Always Accept New Configuration When    C
C     The Old One Has An Overlap... This      C
C     Is Only Allowed During Equilibration.   C
C     Otherwise, There Might Be An Error      C
C     Somewhere....                           C
Ccccccccccccccccccccccccccccccccccccccccccccccc

         If(Lprint) Write(6,*) 'Overlap In Old Configuration !!!'

         Lready = .True.
         Enold  = 0.0d0
         Ennew  = 0.0d0

         Call Echain(Xnn,Ynn,Znn,Ichain,-1,Enold)
         Call Echain(Xn,Yn,Zn,Ichain,-1,Ennew)

      Elseif(Lrecoil) Then

Ccccccccccccccccccccccccccccccccccccccccccc
C     Recalculate Energies Of The Chain   C
C     For Recoil Growth                   C
Ccccccccccccccccccccccccccccccccccccccccccc
         
         Call Echain(Xnn,Ynn,Znn,Ichain,-1,Enold)
         Call Echain(Xn,Yn,Zn,Ichain,-1,Ennew)

         If(Ran_Uniform().Lt.((Wn/Wo)*Dexp(-Beta*(Ennew-Enold)))) 
     &        Lready = .True.
      
      Else

Cccccccccccccccccccccccccccccc
C     Cbmc Acceptance Rule   C
Cccccccccccccccccccccccccccccc

         If(Ran_Uniform().Lt.(Wn/Wo)) Lready = .True.
      Endif

      If (Lready) Then

Ccccccccccccccccccc
C     Accepted    C
Ccccccccccccccccccc

         Acc  = Acc  + 1.0d0
         Etot = Etot + Ennew - Enold
         
         Do I = 1,Ell
            X(I,Ichain) = Xn(I)
            Y(I,Ichain) = Yn(I)
            Z(I,Ichain) = Zn(I)
         Enddo
      Endif
      
      Return
      End
