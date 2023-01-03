      Subroutine Mcrota(Move,Acc,Etot,Dx)
      Implicit None

      Include 'system.inc'

Cccccccccccccccccccccccccccccc
C     Particle Displacement  C
Cccccccccccccccccccccccccccccc
            
      Integer          Ichain,I

      Double Precision Ran_Uniform,Etot,Ennew,Enold,
     &                 Xn(Ellmax),Yn(Ellmax),Zn(Ellmax),Dx,
     &                 Move,Acc,Rx,Ry,Rz,Rxnew,Rynew,Rznew,
     &                 Rxorig,Ryorig,Rzorig,Deltv,Dgamma,
     &                 Cosdg,Sindg
     
      If(Ell.Le.1) Return

      Ichain = Idint(Ran_Uniform()*Dble(Nchain)) + 1
      Move   = Move + 1.0d0
      Rxorig = 0.0d0
      Ryorig = 0.0d0
      Rzorig = 0.0d0

      Do I = 1,Ell
         Xn(I) = X(I,Ichain)
         Yn(I) = Y(I,Ichain)
         Zn(I) = Z(I,Ichain)

         Rxorig = Rxorig + Xn(I)
         Ryorig = Ryorig + Yn(I)
         Rzorig = Rzorig + Zn(I)
      Enddo

      Rxorig = Rxorig/Dble(Ell)
      Ryorig = Ryorig/Dble(Ell)
      Rzorig = Rzorig/Dble(Ell)

      Call Echain(Xn,Yn,Zn,Ichain,-1,Enold)
       
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Center Of Mass Is Used As Origin For Rotation Of Units;     C
C     Set Up The Rotation Marix                                   C
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
      Deltv  = 3.0d0*Ran_Uniform()
      Dgamma = Dx*(Ran_Uniform()-0.5d0)
      Cosdg  = Dcos(Dgamma)
      Sindg  = Dsin(Dgamma)
      Rx     = 0.0d0
      Ry     = 0.0d0
      Rz     = 0.0d0
      Rxnew  = 0.0d0
      Rynew  = 0.0d0
      Rznew  = 0.0d0
       
      If (Deltv.Lt.1.0d0) Then
 
Cccccccccccccccccccccccccccc
C        Rotate X-Axis     C
Cccccccccccccccccccccccccccc
 
         Do I = 1,Ell
            Ry    = Y(I,Ichain) - Ryorig
            Rz    = Z(I,Ichain) - Rzorig
            Rynew = Cosdg*Ry + Sindg*Rz
            Rznew = Cosdg*Rz - Sindg*Ry
            Xn(I) = X(I,Ichain)
            Yn(I) = Ryorig + Rynew
            Zn(I) = Rzorig + Rznew
         Enddo
 
      Elseif(Deltv.Lt.2.0d0) Then
 
Cccccccccccccccccccccccc
C     Rotate Y-Axis    C
Cccccccccccccccccccccccc
 
         Do I = 1,Ell
            Rx    = X(I,Ichain) - Rxorig
            Rz    = Z(I,Ichain) - Rzorig
            Rxnew = Cosdg*Rx - Sindg*Rz
            Rznew = Cosdg*Rz + Sindg*Rx
            Xn(I) = Rxorig + Rxnew
            Yn(I) = Y(I,Ichain)
            Zn(I) = Rzorig + Rznew
         Enddo
 
      Else

Cccccccccccccccccccccccc 
C     Rotate Z-Axis    C
Cccccccccccccccccccccccc
 
         Do I = 1,Ell
            Rx    = X(I,Ichain) - Rxorig
            Ry    = Y(I,Ichain) - Ryorig
            Rxnew = Cosdg*Rx + Sindg*Ry
            Rynew = Cosdg*Ry - Sindg*Rx
            Xn(I) = Rxorig + Rxnew
            Yn(I) = Ryorig + Rynew
            Zn(I) = Z(I,Ichain)
         Enddo
      Endif
 
      Call Backbox(Xn,Yn,Zn)

      Call Echain(Xn,Yn,Zn,Ichain,-1,Ennew)

Cccccccccccccccccccccccccccccc
C     Check For Acceptance   C
Cccccccccccccccccccccccccccccc

      If (Ran_Uniform().Lt.Dexp(-Beta*(Ennew-Enold))) Then
         Acc   = Acc  + 1.0d0
         Etot  = Etot + Ennew - Enold

         Do I = 1,Ell
            X(I,Ichain) = Xn(I)
            Y(I,Ichain) = Yn(I)
            Z(I,Ichain) = Zn(I)
         Enddo  
      Endif

      Return
      End
