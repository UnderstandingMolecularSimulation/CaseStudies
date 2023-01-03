      Subroutine Sphere(Bx,By,Bz)
      Implicit None

      Include 'system.inc'

Cccccccccccccccccccccccccccccccccccccc
C     Generate A New Orientation     C
Cccccccccccccccccccccccccccccccccccccc

      Double Precision Bx,By,Bz,Ran_Uniform,
     &     Ransq,Ranh,Bondnorm,Bondl
 
Ccccccccccccccccccccccccccccccccccc
C     First: Vector On A Sphere   C
Ccccccccccccccccccccccccccccccccccc

 1    Bx    = 1.0d0 - 2.0d0*Ran_Uniform()
      By    = 1.0d0 - 2.0d0*Ran_Uniform()
      Ransq = Bx*Bx + By*By
      
      If(Ransq.Ge.1.0d0) Goto 1

      Ranh = 2.0d0*Dsqrt(1.0d0-Ransq)
      Bx   = Bx*Ranh
      By   = By*Ranh
      Bz   = 1.0d0-2.0d0*Ransq

Ccccccccccccccccccccccccccccc
C     Generate Bondlength   C
C     Between 0.5 And 1.5   C
Ccccccccccccccccccccccccccccc

      Bondnorm  = 1.0d0/(1.5d0**2)

      If(Ladvanced) Then
 2       Bondl = 0.5d0 + Ran_Uniform()
         
         If(Ran_Uniform().Gt.
     &        (Bondl*Bondl*Bondnorm*
     &        Dexp(-Beta*0.5d0*Kv*
     &        ((Bondl-1.0d0)**2)))) Goto 2
      Else
 3       Bondl = 0.5d0 + Ran_Uniform()
         
         If(Ran_Uniform().Gt.(Bondl*Bondl*Bondnorm)) Goto 3
      Endif

      Bondl = Bondl/Dsqrt(Bx*Bx + By*By + Bz*Bz)
      Bx    = Bx*Bondl
      By    = By*Bondl
      Bz    = Bz*Bondl

      Return
      End
