      Subroutine Ran_Sphere(Bx,By,Bz)
      Implicit None

Cccccccccccccccccccccccccccccccccccccccc
C     Random Unit Vector On A Spere    C
Cccccccccccccccccccccccccccccccccccccccc

      Double Precision Bx,By,Bz,Ran_Uniform,Ransq,Ranh
 
 1    Bx    = 1.0d0 - 2.0d0*Ran_Uniform()
      By    = 1.0d0 - 2.0d0*Ran_Uniform()
      Ransq = Bx*Bx + By*By
      
      If(Ransq.Ge.1.0d0) Goto 1

      Ranh = 2.0d0*Dsqrt(1.0d0-Ransq)
      Bx   = Bx*Ranh
      By   = By*Ranh
      Bz   = 1.0d0-2.0d0*Ransq

      Return
      End
