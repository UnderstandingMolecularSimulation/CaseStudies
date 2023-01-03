      Subroutine Order(Myorder)
      Implicit None

Ccccccccccccccccccccccccccccccccccccccccccccccccc
C     Calculate The Order Paraneter Of A Path   C
Ccccccccccccccccccccccccccccccccccccccccccccccccc 

      Include 'system.inc'
            
      Double Precision Myorder(Maxtraject),R2
      Integer          J

      Do J=1,Nslice
         R2         = (1.0d0 - Xxold(J))**2 + Yyold(J)**2 
         Myorder(J) = 1.0d0 - (Dsqrt(R2)*0.5d0)
      Enddo

      Return
      End
