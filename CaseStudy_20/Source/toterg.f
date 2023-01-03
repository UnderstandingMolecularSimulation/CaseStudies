      Subroutine Toterg(Etot)
      Implicit None

      Include 'system.inc'

Ccccccccccccccccccccccccccccccccccccccccccccccccc
C     Calculate The Total Energy Of The System  C
Ccccccccccccccccccccccccccccccccccccccccccccccccc
      
      Integer J,Ichain

      Double Precision Enij,Etot,Xn(Ellmax),Yn(Ellmax),Zn(Ellmax)
 
      Etot = 0.0d0

      Do Ichain = 1, Nchain

         Do J=1,Ell
            Xn(J) = X(J,Ichain)
            Yn(J) = Y(J,Ichain)
            Zn(J) = Z(J,Ichain)
         Enddo

         Call Echain(Xn,Yn,Zn,Ichain,Ichain,Enij)
         Etot = Etot + Enij
      Enddo

      Return
      End
