      Function In_B()
      Implicit None

      Include 'system.inc'

Cccccccccccccccccccccccccccccccccccccccccccccc
C     Check If A Point Of The Path Is In B   C
Cccccccccccccccccccccccccccccccccccccccccccccc

      Logical          In_B
      Double Precision R2

      R2 = (1.0d0 - Rxx)**2 + Ryy**2

      If(R2.Lt.(Radius**2)) Then
         In_B = .True.
      Else
         In_B = .False.
      Endif

      Return
      End
