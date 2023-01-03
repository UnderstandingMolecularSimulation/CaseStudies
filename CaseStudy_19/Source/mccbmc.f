      Subroutine Mccbmc(Move,Acc)
      Implicit None

      Include 'system.inc'

Ccccccccccccccccccccccccccccccccccc
C     Regrow A Chain Using Cbmc   C
Ccccccccccccccccccccccccccccccccccc
      
      Integer          I
      Logical          Lready
      
      Double Precision Wn,Wo,Ran_Uniform,Ennew,Enold,
     &                 Xn(Ellmax),Yn(Ellmax),Zn(Ellmax),Acc,Move,
     &                 Xnn(Ellmax),Ynn(Ellmax),Znn(Ellmax)
       
      Move = Move + 1.0d0

      Call Grow(.False.,Wn,Xn,Yn,Zn,Ennew)
      
      Do I=1,Ell
         Xnn(I) = X(I)
         Ynn(I) = Y(I)
         Znn(I) = Z(I)
      Enddo

      Call Grow(.True.,Wo,Xnn,Ynn,Znn,Enold)
      
      If(Ladvanced) Then
         Lready = .True.
      Else
         Lready = (Ran_Uniform().Lt.(Wn/Wo))
      Endif

      If (Lready) Then
         Acc  = Acc  + 1.0d0
         Utot = Utot + Ennew - Enold
         
         Do I = 1,Ell
            X(I) = Xn(I)
            Y(I) = Yn(I)
            Z(I) = Zn(I)
         Enddo
      Endif
      
      Return
      End
