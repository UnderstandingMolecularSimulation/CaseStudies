      Subroutine Checkdis(Lovrlap,Number)
      Implicit None
 
      Include 'system.inc'
 
C     Check The Distance Between 2 Beads
 
      Double Precision Dx,Dy,Dz
      Integer I,J,K,Number
      Logical Lovrlap
 
      Lovrlap = .True.
 
      Do J = 0,1
 
         I = Number + J
 
         Do K = 1,Number - 1
 
            Dx = Rxx(I) - Rxx(K)
            Dy = Ryy(I) - Ryy(K)
            Dz = Rzz(I) - Rzz(K)
 
            If (Dx.Gt.Hbox) Then
               Dx = Dx - Box
            Elseif (Dx.Lt. - Hbox) Then
               Dx = Dx + Box
            Endif
 
            If (Dy.Gt.Hbox) Then
               Dy = Dy - Box
            Elseif (Dy.Lt. - Hbox) Then
               Dy = Dy + Box
            Endif
 
            If (Dz.Gt.Hbox) Then
               Dz = Dz - Box
            Elseif (Dz.Lt. - Hbox) Then
               Dz = Dz + Box
            Endif
 
            If ((Dx*Dx+Dy*Dy+Dz*Dz).Lt.0.02d0) Return
 
         Enddo
      Enddo
 
      Lovrlap = .False.
 
      Return
      End
