      Subroutine Movie
      Implicit None

      Include 'system.inc'
      
Ccccccccccccccccccccccccccccccccccccc
C     Write A Movie Of The System:  C
C     A,B And The Branched Atom     C
Ccccccccccccccccccccccccccccccccccccc

      Integer          Model,Atom,J,Mymodel
      Double Precision Dummy
      Character*5      Short
      Character*14     Name1

      Save Mymodel
      Data Mymodel/0/

      Mymodel = Mymodel + 1

      Call Convert_Num(Mymodel,Short)

      Name1 = 'movie'//short//'.pdb'
         
      Open(32,File=name1)

      Model = 0
      Atom  = 0
      Dummy = 0.0d0

      Do J=1,Nslice

         Model = Model + 1

         Write(32,'(A,I9)') 'MODEL',Model

         Atom = Atom + 1

         Write(32,'(A,I7,A,I12,4x,3f8.3)') 'ATOM',Atom,'  O',
     &        Atom,5.0d0*Xxold(J),5.0d0*Yyold(J),Dummy
           
         Write(32,'(A)') 'ENDMDL'

      Enddo

      Close(32) 
      
      Return
      End
