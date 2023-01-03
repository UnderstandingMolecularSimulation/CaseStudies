      Program Umbrella
      Implicit None

Cccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Calculates The Prob. To Find The System      C
C     In B. Match The Different Slices             C
C                                                  C
C     Input Files Should Be Called                 C
C     Umbr001, Umbr002 Etc.                        C
C                                                  C
C     Written By Thijs J.H. Vlugt, 1999            C
Cccccccccccccccccccccccccccccccccccccccccccccccccccc

      Character*7 Number
      Integer     Numbrella,Maxumbrella,I,J,Jj,
     &            Maxslice,Nstart,Nend

      Parameter (Maxslice    = 10000)
      Parameter (Maxumbrella = 10)

      Double Precision Av1,Av2,Dummy,
     &     Slice(-Maxslice:Maxslice,Maxumbrella)

      Integer Islice(-Maxslice:Maxslice)

      Write(*,*) 'How Many Umbrellas ?'
      Read(*,*) Numbrella

      If(Numbrella.Gt.Maxumbrella.Or.
     &     Numbrella.Lt.1) Stop

      Write(*,*) 'Start Region B     ?'
      Read(*,*) Nstart

      Write(*,*) 'End Region B       ?'
      Read(*,*) Nend

      If(Nstart.Ge.Nend.Or.Nstart.Lt.-Maxslice.Or.
     &     Nend.Gt.Maxslice) Stop

Ccccccccccccccccccccccccccccccc
C     Initialize Variables    C
Ccccccccccccccccccccccccccccccc

      Do J=1,Maxumbrella
         Do I=-Maxslice,Maxslice
            Slice(I,J)  = -10.0d0
            Islice(I)   = 0 
         Enddo
      Enddo

Cccccccccccccccccccccccccccccc
C     Read Data From Disk    C
Cccccccccccccccccccccccccccccc

      Do J=1,Numbrella
         Call Convert(J,Number)

         Open(32,File=Number)
         
 99      Read(32,*,End=100) I,Dummy

         If(I.Lt.-Maxslice.Or.I.Gt.Maxslice) Then
            Write(6,*) 'Slice (I) Out Of Range !!!'
            Write(6,*) 'I : ',I
            Stop
         Elseif(Dummy.Le.0.0d0) Then
            Write(6,*) 'Input < 0 !!!'
            Stop
         Endif

         Slice(I,J) = Dummy

         Goto 99

 100     Close(32)

      Enddo

Ccccccccccccccccccccccccccc
C     Match The Overlap   C
Ccccccccccccccccccccccccccc

      Do J=1,Numbrella
         Do Jj=1,Numbrella

            If(J.Ne.Jj) Then
         
               Av1 = 0.0d0
               Av2 = 0.0d0

               Do I=-Maxslice,Maxslice
                  If(Slice(I,J).Gt.0.0d0.And.
     &                 Slice(I,Jj).Gt.0.0d0) Then
                     Av1 = Av1 + Slice(I,J)/Slice(I,Jj)
                     Av2 = Av2 + 1.0d0
                  Endif
               Enddo

               If(Av2.Gt.0.5d0) Then
                  Av1 = Av1/Av2

                  Do I=-Maxslice,Maxslice
                     If(Slice(I,Jj).Gt.0.0d0) Then
                        Slice(I,Jj) = Slice(I,Jj)*Av1
                     Endif
                  Enddo
               Endif
            Endif
         Enddo
      Enddo

Ccccccccccccccccccccccccccccccccccccccccccccccc
C     Calculate The Average Over All Slices   C
Ccccccccccccccccccccccccccccccccccccccccccccccc

      Do I=-Maxslice,Maxslice
         Av1 = 0.0d0
         Av2 = 0.0d0

         Do J=1,Numbrella
            If(Slice(I,J).Gt.0.0d0) Then
               Av1 = Av1 + Slice(I,J)
               Av2 = Av2 + 1.0d0
            Endif
         Enddo

         If(Av2.Gt.0.5d0) Then
            Av1       = Av1/Av2
            Islice(I) = 1
         Endif

         Do J=1,Numbrella
            Slice(I,J) = Av1
         Enddo
      Enddo

Cccccccccccccccccccc
C     Normalize    C
Cccccccccccccccccccc

      Av1 = 0.0d0
      Av2 = 0.0d0

      Do I=-Maxslice,Maxslice
         Av1 = Av1 + Slice(I,1)
      Enddo

      Av1 = 1.0d0/Av1

      Do I=-Maxslice,Maxslice
         Slice(I,1) = Slice(I,1)*Av1
      Enddo      

      Do I=Nstart,Nend
         Av2 = Av2 + Slice(I,1)
      Enddo

      Write(6,*)
      Write(6,*) 'Fraction In B : ',Av2
      Write(6,*)

      Open(32,File='Plambda')

      Do I=-Maxslice,Maxslice
         If(Islice(I).Ne.0) Then
            Write(32,'(I9,2x,E20.10)') 
     &           I,Slice(I,1)
         Endif
      Enddo

      Close(32)

      Stop
      End

      Subroutine Convert(I,Number)
      Implicit None

Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Convert A Number Between 0 And 99 To A String     C
C     Add The Number Of The Temperature...              C
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      Integer     I
      Character*3 Num
      Character*7 Number

      If(I.Lt.0) Then
         Write(6,*) 'Error In Convert !!!'
         Stop
      Elseif(I.Lt.10) Then
         Num = '00'//Char(48+I)
      Elseif(I.Lt.100) Then
         Num = '0'//Char(48+(I/10))//
     &        Char(48+I-((I/10)*10))
      Else
         Stop
      Endif

      Number = 'Umbr'//Num

      Return
      End
