      Subroutine Mcmove(Move,Acc,Etot,Dx)
      Implicit None

      Include 'system.inc'

Cccccccccccccccccccccccccccccc
C     Particle Displacement  C
Cccccccccccccccccccccccccccccc
            
      Integer          Ichain,I

      Double Precision Ran_Uniform,Etot,Ennew,Enold,Bx,By,Bz,
     &                 Xn(Ellmax),Yn(Ellmax),Zn(Ellmax),Dx,
     &                 Move,Acc
     
      Ichain = Idint(Ran_Uniform()*Dble(Nchain)) + 1
      Move   = Move + 1.0d0

      Bx     = (Ran_Uniform()-0.5d0)*Dx
      By     = (Ran_Uniform()-0.5d0)*Dx
      Bz     = (Ran_Uniform()-0.5d0)*Dx
      
      Do I = 1,Ell
         Xn(I) = X(I,Ichain)
         Yn(I) = Y(I,Ichain)
         Zn(I) = Z(I,Ichain)
      Enddo

      Call Echain(Xn,Yn,Zn,Ichain,-1,Enold)

      Do I = 1,Ell
         Xn(I) = X(I,Ichain) + Bx
         Yn(I) = Y(I,Ichain) + By
         Zn(I) = Z(I,Ichain) + Bz
      Enddo

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
