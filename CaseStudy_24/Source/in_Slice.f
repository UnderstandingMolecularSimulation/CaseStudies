      Subroutine In_Slice(Lslice,Myorder)
      Implicit None

CCccccccccccccccccccccccccccccccccccccccccc
C     Checks If A Particle Is In A Slice  C
C     (Umbrella Sampling)                 C
C                                         C
C     The Reaction Coordinate Is The      C
C     Distance To B (Not A !!!!!)         C
Ccccccccccccccccccccccccccccccccccccccccccc

      Include 'system.inc'

      Logical          Lslice
      Double Precision Myorder,R2

      R2 = (1.0d0 - Rxx)**2 + Ryy**2

      Myorder  = 1.0d0 - (Dsqrt(R2)*0.5d0)

      If(Myorder.Gt.Lmin.And.Myorder.Lt.Lmax) Then
         Lslice = .True.
      Else
         Lslice  = .False.
      Endif

      Return
      End
