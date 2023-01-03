      Subroutine Writepdb
      Implicit None

Cccccccccccccccccccccccccccccccccccccccccccccccccc
C     Write Final Configuration Of The System    C
Cccccccccccccccccccccccccccccccccccccccccccccccccc

      Include 'maxarray.inc'
      Include 'system.inc'

      Integer     I
      Character*3 Nnn

      I = 1

      Open(21,File="Config.Pdb")

      Write(21,'(A,I9)') 'MODEL',I

      Do I=1,Npart

         If(Type(I).Eq.1) Then
            Nnn = '  O'
         Else
            Nnn = '  N'
         Endif

         Write(21,'(A,I7,A,I12,4x,3f8.3)') 'ATOM',I,Nnn,
     &        I,Xx(I),Yy(I),Zz(I)
      Enddo

      Write(21,'(A)') 'ENDMDL'

      Close(21)

      Return
      End
