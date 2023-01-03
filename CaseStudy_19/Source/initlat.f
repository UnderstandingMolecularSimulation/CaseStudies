      Subroutine Initlat
      Implicit None

Cccccccccccccccccccccccccccccccccccccccc
C     Generate Initial Configuration   C
Cccccccccccccccccccccccccccccccccccccccc

      Include 'system.inc'

      Double Precision Bx,By,Bz
      Integer I
      
      X(1) = 0.0d0
      Y(1) = 0.0d0
      Z(1) = 0.0d0
      Utot = 0.0d0

      Do I=2,Ell
         Call Sphere(Bx,By,Bz)
         
         X(I) = X(I-1) + Bx
         Y(I) = Y(I-1) + By
         Z(I) = Z(I-1) + Bz

         Utot = Utot   + 0.5d0*Kv*
     &        ((Dsqrt((X(I) - X(I-1))**2 + 
     &                (Y(I) - Y(I-1))**2 + 
     &                (Z(I) - Z(I-1))**2) - 1.0d0)**2)
      Enddo
      
      Return
      End
