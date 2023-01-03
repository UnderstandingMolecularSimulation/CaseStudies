      Subroutine Backbox(Xn,Yn,Zn)
      Implicit None
 
Cccccccccccccccccccccccccccccccccc
C     Put Particle Back In Box   C
Cccccccccccccccccccccccccccccccccc
 
      Include 'system.inc'
 
      Integer          J
      Double Precision Bx,By,Bz,Xn(Ellmax),Yn(Ellmax),Zn(Ellmax)
 
      Bx = Xn(1)
      By = Yn(1)
      Bz = Zn(1)
 
      Bx = -Box*(Dble(Idint(Bx*Boxi + 9999.0d0) - 9999))
      By = -Box*(Dble(Idint(By*Boxi + 9999.0d0) - 9999))
      Bz = -Box*(Dble(Idint(Bz*Boxi + 9999.0d0) - 9999))
 
      Do J = 1,Ell
         Xn(J) = Xn(J) + Bx
         Yn(J) = Yn(J) + By
         Zn(J) = Zn(J) + Bz
      Enddo
 
      Return
      End
