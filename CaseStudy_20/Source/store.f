      Subroutine Store(Dr,Drot)
      Implicit None

      Include 'system.inc'

Cccccccccccccccccccccccccccccccccccc
C     Save Configuration To Disk   C
Cccccccccccccccccccccccccccccccccccc
      
      Integer          I,J,Countatom
      Double Precision Dr,Drot
 
      Open(24,File="chain.res")

      Write (24,'(G20.10,1x,I6,1x,I6,1x,2G20.10)') 
     &     Box,Nchain,Ell,Dr,Drot

      Do I = 1, Nchain
         Do J = 1, Ell
            Write (24,'(3(1x,G30.15),5x,2i5)') 
     &           X(J,I),Y(J,I),Z(J,I),I,J
         Enddo
      Enddo

      Close(24)

Cccccccccccccccccccccccccccc
C     Produce A Pdb File   C
Cccccccccccccccccccccccccccc

      Open(24,File="system.pdb")
      
      Countatom = 0

      Write(24,'(A,I9)') 'MODEL',1

      Do I=1,Nchain
         Do J=1,Ell

            Countatom = Countatom + 1

            Write(24,'(A,I7,A,I12,4x,3f8.3)') 'ATOM',Countatom,'  O',
     &           Countatom,X(J,I),Y(J,I),Z(J,I)
         Enddo
      Enddo

      Write(24,'(A)') 'ENDMDL'

      Close(24)

      Return
      End
