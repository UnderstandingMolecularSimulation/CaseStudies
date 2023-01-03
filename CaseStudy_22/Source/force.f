      Subroutine Force(Lshort,Llong)
      Implicit None
 
C     Calculate The Forces And Potential Energy
 
      Include 'system.inc'
 
      Integer I,J,Jbeg,Jend,Jnab,Nlist1,Nlist2,Na,Nb,Nc
      Double Precision Dx,Dy,Dz,Ff,R2i,R6i,Ilambda,Preforce,
     &     Rversq,Rversq2,Dsp
      Logical Lshort,Llong
 
      Ilambda = 1.0d0/Lambda
      Rversq = (Dsqrt(Rcutsq) + Rver)**2
      Rversq2 = (Rcut2 + Rver)**2
 
Ccccccccccccccccccccc
C     Long Forces   C
Ccccccccccccccccccccc

      If(Llong) Then

         Dsp = 0.0d0
         Upotl = 0.0d0

         Do I = 1,Npart
            Dsp = Max(Dsp,Dabs(Rxx(I)-Xoud(I)),
     &           Dabs(Ryy(I)-Youd(I)),Dabs(Rzz(I)-Zoud(I)))

            Fxxl(I) = 0.0d0
            Fyyl(I) = 0.0d0
            Fzzl(I) = 0.0d0
         Enddo
 
Ccccccccccccccccccccccccc
C     Make A New List   C
Ccccccccccccccccccccccccc

         If (Dsp.Gt.(0.288d0*Rver)) Then

            Do I = 1,Npart - 1,2
 
               J = I + 1
 
               If (Rxx(I).Gt.Box) Then
                  Rxx(I) = Rxx(I) - Box
                  Rxx(J) = Rxx(J) - Box
               Elseif (Rxx(I).Lt.0.0d0) Then
                  Rxx(I) = Rxx(I) + Box
                  Rxx(J) = Rxx(J) + Box
               Endif
 
               If (Ryy(I).Gt.Box) Then
                  Ryy(I) = Ryy(I) - Box
                  Ryy(J) = Ryy(J) - Box
               Elseif (Ryy(I).Lt.0.0d0) Then
                  Ryy(I) = Ryy(I) + Box
                  Ryy(J) = Ryy(J) + Box
               Endif
 
               If (Rzz(I).Gt.Box) Then
                  Rzz(I) = Rzz(I) - Box
                  Rzz(J) = Rzz(J) - Box
               Elseif (Rzz(I).Lt.0.0d0) Then
                  Rzz(I) = Rzz(I) + Box
                  Rzz(J) = Rzz(J) + Box
               Endif
            Enddo

            Nlist1 = 0
            Nlist2 = 0
 
            Do I = 1,Npart - 1
 
               Point1(I) = Nlist1 + 1
               Point2(I) = Nlist2 + 1
 
               Do J = I + 1,Npart
 
                  If ((I+1.Ne.J) .Or. (Mod(J,2).Ne.0)) Then
 
                     Dx = Rxx(I) - Rxx(J)
                     Dy = Ryy(I) - Ryy(J)
                     Dz = Rzz(I) - Rzz(J)
 
                     Na = 0
                     Nb = 0
                     Nc = 0

                     If (Dx.Gt.Hbox) Then
                        Na = -1
                     Elseif (Dx.Lt. - Hbox) Then
                        Na = 1
                     Endif
 
                     If (Dy.Gt.Hbox) Then
                        Nb = -1
                     Elseif (Dy.Lt. - Hbox) Then
                        Nb = 1
                     Endif
 
                     If (Dz.Gt.Hbox) Then
                        Nc = -1
                     Elseif (Dz.Lt. - Hbox) Then
                        Nc = 1
                     Endif
 
                     Na = 9*Na + 3*Nb + Nc

                     Dx = Dx + Tx(Na)
                     Dy = Dy + Ty(Na)
                     Dz = Dz + Tz(Na)

                     R2i = Dx*Dx + Dy*Dy + Dz*Dz
 
                     If (R2i.Lt.Rversq) Then
 
                        Nlist1 = Nlist1 + 1
                        List1(Nlist1) = J
                        Imgg1(Nlist1) = Na

                        If (Nlist1.Ge.Maxlist) Then
                           Write (6,*) 
     &                          'Too Many Particles In The List !!!'
                           Stop
                        Elseif (R2i.Lt.Rversq2) Then
                           Nlist2 = Nlist2 + 1
                           List2(Nlist2) = J
                           Imgg2(Nlist2) = Na
                        Endif

                        If (R2i.Lt.Rcutsq) Then
 
                           Preforce = 1.0d0
                        
                           If (Lrespa) Then
                              If (R2i.Lt.Rminlab) Then
                                 Preforce = 0.0d0
                              Elseif (R2i.Lt.Rcut2sq) Then
                                 Preforce = 
     &                                Ilambda*
     &                                (Dsqrt(R2i) - Rcut2 + Lambda)
                                 Preforce = 
     &                                -Preforce*Preforce*
     &                                (2.0d0*Preforce - 3.0d0)
                              Endif
                           Endif
 
                           R2i = 1.0d0/R2i
                           R6i = R2i*R2i*R2i
 
                           Upotl = Upotl + 
     &                          4.0d0*R6i*(R6i - 1.0d0) - Ecut
 
                           Ff = 48.0d0*R6i*R2i*(R6i - 0.5d0)*Preforce
                           
                           Fxxl(I) = Fxxl(I) + Ff*Dx
                           Fyyl(I) = Fyyl(I) + Ff*Dy
                           Fzzl(I) = Fzzl(I) + Ff*Dz
                           
                           Fxxl(J) = Fxxl(J) - Ff*Dx
                           Fyyl(J) = Fyyl(J) - Ff*Dy
                           Fzzl(J) = Fzzl(J) - Ff*Dz
                        
                        Endif
                     Endif
                  Endif
               Enddo
            Enddo
            
            Point1(Npart) = Nlist1 + 1
            Point2(Npart) = Nlist2 + 1
            
            Do I = 1,Npart
               Xoud(I) = Rxx(I)
               Youd(I) = Ryy(I)
               Zoud(I) = Rzz(I)
            Enddo

         Else

Ccccccccccccccccccccccccc
C     Use The Old List  C
Ccccccccccccccccccccccccc
 
            Do I = 1,Npart - 1
 
               Jbeg = Point1(I)
               Jend = Point1(I + 1) - 1
 
               If (Jbeg.Le.Jend) Then
 
                  Do Jnab = Jbeg,Jend
 
                     J = List1(Jnab)
                     Na = Imgg1(Jnab)
 
                     Dx = Rxx(I) - Rxx(J) + Tx(Na)
                     Dy = Ryy(I) - Ryy(J) + Ty(Na)
                     Dz = Rzz(I) - Rzz(J) + Tz(Na)
 
                     R2i = Dx*Dx + Dy*Dy + Dz*Dz
 
                     If (R2i.Lt.Rcutsq) Then
 
                        Preforce = 1.0d0
 
                        If (Lrespa) Then
                           If (R2i.Lt.Rminlab) Then
                              Preforce = 0.0d0
                           Elseif (R2i.Lt.Rcut2sq) Then
                              Preforce = 
     &                             Ilambda*
     &                             (Dsqrt(R2i) - Rcut2 + Lambda)
                              Preforce = 
     &                             -Preforce*Preforce*
     &                             (2.0d0*Preforce - 3.0d0)
                           Endif
                        Endif
 
                        R2i = 1.0d0/R2i
                        R6i = R2i*R2i*R2i
 
                        Upotl = Upotl + 4.0d0*R6i*(R6i - 1.0d0) - Ecut
 
                        Ff = 48.0d0*R6i*R2i*(R6i - 0.5d0)*Preforce
 
                        Fxxl(I) = Fxxl(I) + Ff*Dx
                        Fyyl(I) = Fyyl(I) + Ff*Dy
                        Fzzl(I) = Fzzl(I) + Ff*Dz
                        
                        Fxxl(J) = Fxxl(J) - Ff*Dx
                        Fyyl(J) = Fyyl(J) - Ff*Dy
                        Fzzl(J) = Fzzl(J) - Ff*Dz
                        
                     Endif
                  Enddo
               Endif
            Enddo
         Endif
      Endif

      If (Lshort) Then
 
Ccccccccccccccccccccc
C     Short Forces  C
Ccccccccccccccccccccc
 
         Do I = 1,Npart
            Fxxs(I) = 0.0d0
            Fyys(I) = 0.0d0
            Fzzs(I) = 0.0d0
         Enddo
 
         Upots = 0.0d0
 
         If (.Not.Lcons) Then
 
C     Bond-Stretching
 
            Do I = 1,Npart - 1,2
 
               J = I + 1
 
               Dx = Rxx(I) - Rxx(J)
               Dy = Ryy(I) - Ryy(J)
               Dz = Rzz(I) - Rzz(J)
 
               R2i = Dsqrt(Dx*Dx + Dy*Dy + Dz*Dz)
 
               Upots = Upots + 0.5d0*Bondkb*((R2i-Bondl)**2)
 
               R2i = Bondkb*(Bondl - R2i)/R2i
 
               Dx = Dx*R2i
               Dy = Dy*R2i
               Dz = Dz*R2i
 
               Fxxs(I) = Fxxs(I) + Dx
               Fyys(I) = Fyys(I) + Dy
               Fzzs(I) = Fzzs(I) + Dz
 
               Fxxs(J) = Fxxs(J) - Dx
               Fyys(J) = Fyys(J) - Dy
               Fzzs(J) = Fzzs(J) - Dz
 
            Enddo
         Endif
 
         If (Lrespa) Then
 
C     Short Range Lj Part
 
            Do I = 1,Npart - 1
 
               Jbeg = Point2(I)
               Jend = Point2(I + 1) - 1
 
               If (Jbeg.Le.Jend) Then
 
                  Do Jnab = Jbeg,Jend
 
                     J = List2(Jnab)
                     Na = Imgg2(Jnab)
 
                     Dx = Rxx(I) - Rxx(J) + Tx(Na)
                     Dy = Ryy(I) - Ryy(J) + Ty(Na)
                     Dz = Rzz(I) - Rzz(J) + Tz(Na)
 
                     R2i = Dx*Dx + Dy*Dy + Dz*Dz
 
                     If (R2i.Lt.Rcut2sq) Then
 
                        Preforce = 1.0d0
 
                        If (R2i.Gt.Rminlab) Then
                           Preforce = 
     &                          Ilambda*(Dsqrt(R2i) - Rcut2 + Lambda)
                           Preforce = 1.0d0 + 
     &                          Preforce*Preforce*
     &                          (2.0d0*Preforce - 3.0d0)
                        Endif
 
                        R2i = 1.0d0/R2i
                        R6i = R2i*R2i*R2i
 
                        Ff = 48.0d0*R6i*R2i*(R6i - 0.5d0)*Preforce
 
                        Fxxs(I) = Fxxs(I) + Ff*Dx
                        Fyys(I) = Fyys(I) + Ff*Dy
                        Fzzs(I) = Fzzs(I) + Ff*Dz
 
                        Fxxs(J) = Fxxs(J) - Ff*Dx
                        Fyys(J) = Fyys(J) - Ff*Dy
                        Fzzs(J) = Fzzs(J) - Ff*Dz
 
                     Endif
                  Enddo
               Endif
            Enddo
         Endif
      Endif
 
      Return
      End
