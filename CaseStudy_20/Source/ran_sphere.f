      Subroutine Ran_Sphere(Bx,By,Bz)
      Implicit None

      Include 'system.inc'

Ccccccccccccccccccccccccccccccccccccccccccccccccc
C     Generate A Random Vector Of Length Bondl  C
Ccccccccccccccccccccccccccccccccccccccccccccccccc

      Double Precision Bx,By,Bz,Ran_Uniform,Ransq,Ranh,Def
 
 1    Bx    = 1.0d0 - 2.0d0*Ran_Uniform()
      By    = 1.0d0 - 2.0d0*Ran_Uniform()
      Ransq = Bx*Bx + By*By
      
      If(Ransq.Ge.1.0d0) Goto 1

      Ranh = 2.0d0*Dsqrt(1.0d0-Ransq)
      Bx   = Bx*Ranh
      By   = By*Ranh
      Bz   = 1.0d0-2.0d0*Ransq

      Def  = Bondl/Dsqrt(Bx*Bx + By*By + Bz*Bz)
      Bx   = Bx*Def
      By   = By*Def
      Bz   = Bz*Def

      Return
      End
