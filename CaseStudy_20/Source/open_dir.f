      Subroutine Open_Dir(Etot,Lopen,P_Open)
      Implicit None

      Include 'system.inc'

Cccccccccccccccccccccccccccccccccccccccccccccccccc
C     Decide If A Bead Is Either Open Or Closed  C
C     This Is Quite General; In Principle You    C
C     Are Allowed To Design Your Own Criteria !  C
Cccccccccccccccccccccccccccccccccccccccccccccccccc

      Logical          Lopen
      Double Precision Etot,P_Open,Ran_Uniform

      P_Open = Min(1.0d0,Dexp(-Beta*Etot))
      Lopen  = .False.

      If(Ran_Uniform().Lt.P_Open) Lopen = .True.
      
      Return
      End
