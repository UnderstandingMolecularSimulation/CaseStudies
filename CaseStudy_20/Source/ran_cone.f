      Subroutine Ran_Cone(Bx,By,Bz)
      Implicit None
 
      Include 'system.inc'

Cccccccccccccccccccccccccccccccccccccccccccccc
C                                            C
C     Random Number On A Cone With           C      
C     Angle Theta (Between 0 And Pi)         C      
C                                            C
Cccccccccccccccccccccccccccccccccccccccccccccc
C                                            C
C     Bx,By,Bz = Output Vector Length Bondl  C
C     Theta = Cone Angle                     C
Cccccccccccccccccccccccccccccccccccccccccccccc
 
      Double Precision Bx,By,Bz,Xprev,Yprev,Zprev,Ran1,Cosin
 
      Cosin = Dcos(Thetat)
      Ran1  = 1.0d0/Dsqrt(Bx*Bx+By*By+Bz*Bz)
      Xprev = Bx*Ran1
      Yprev = By*Ran1
      Zprev = Bz*Ran1
 
      Call Ran_Sphere(Bx,By,Bz)
 
      Ran1 = Bx*Xprev + By*Yprev + Bz*Zprev
      Bx   = Bx - Ran1*Xprev
      By   = By - Ran1*Yprev
      Bz   = Bz - Ran1*Zprev
      Ran1 = Dsqrt((1.0d0-Cosin*Cosin)/(Bx*Bx+By*By+Bz*Bz))

      Bx   = Bx*Ran1 + Cosin*Xprev
      By   = By*Ran1 + Cosin*Yprev
      Bz   = Bz*Ran1 + Cosin*Zprev
      Ran1 = Bondl/Dsqrt(Bx*Bx+By*By+Bz*Bz)

      Bx    = Bx*Ran1
      By    = By*Ran1
      Bz    = Bz*Ran1
 
      Return
      End
