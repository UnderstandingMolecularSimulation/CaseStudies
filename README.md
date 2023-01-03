# Case Studies for "Understanding Molecular Simulations: From Algorithms to Applications"

*Daan Frenkel and Berend Smit*

## Overview 

The following Case Studies are presented in the book: 

- Appendix      : Statistical Errors 
- [Case Study 1  : Equation Of State Of The Lennard-Jones Fluid (Nvt)](CaseStudy_1)
- Case Study 2  : Importance Of Detailed Balance 
- Case Study 3  : Why Count The Old Configuration Again? 
- [Case Study 4  : Static Properties Of The Lennard-Jones Fluid](CaseStudy_4)
- [Case Study 5  : Dynamic Properties Of The Lennard-Jones Fluid](CaseStudy_5)
- Case Study 6  : Algorithms To Calculate The Mean Square Displacement 
- Case Study 7  : Equation Of State Of The Lennard-Jones Fluid (Npt) 
- Case Study 8  : Phase Equilibria From Constant Pressure Simulations 
- Case Study 9  : Equation Of State Of The Lennard-Jones Fluid (Muvt) 
- Case Study 10 : Lennard-Jones: Andersen Thermostat 
- Case Study 11 : Lennard-Jones: Nose-Hoover Thermostat 
- Case Study 12 : Harmonic Oscillator (I) 
- Case Study 13 : Harmonic Oscillator (II) 
- Case Study 14 : Chemical Potential: Widom Method 
- Case Study 15 : Chemical Potential: Overlapping Distribution 
- Case Study 16 : Phase Equilibria Of The Lennard-Jones Fluid 
- Case Study 17 : Solid-Liquid Equilibrium Of Hard Spheres 
- Case Study 18 : Equation Of State Of Lennard-Jones Chains 
- Case Study 19 : Generation Of Trial Configurations Of Ideal Chains 
- Case Study 20 : Recoil Growth Simulation of Lennard-Jones Chains
- Case Study 21 : Parallel Tempering of a Single Particle
- Case Study 22 : Multiple Timesteps vs. Constraints               
- Case Study 23 : Ideal Gas Particle Over A Barrier 
- Case Study 24 : Ideal Gas Particle in a Two-dimensional Potential
- Case Study 25 : Dissipative Particle Dynamics                  
- Case Study 26 : Comparison Of Schemes For The Lennard-Jones Fluid 
- Tools         : 
   - Random Number Generators In C And Fortran 77
   - Random Vector On A Sphere
    - Gaussian Random Numbers

## Usage 

To compile the source of a Casestudy, simply type "make" in the
directory containing the source code. All makefiles have been prepared
for RedHat Linux 6.1 (Cartman) on an i386 using the GNU compilers. For
other Unix systems, the Makefile may have to be modified. The random
number generator provided in this distribution was written in C
(ran_uniform.c) for some Casestudies. Some care have to be taken,
because some compilers put underscores after FORTRAN77
subroutines. For example, for SGI machines ran_uniform__ has to be
changed into ran_uniform_ (see Tools/ran_uniform_SGI.c).



## Credits 
Casestudies
19,20,21,22,23,24 have been written by Thijs J.H. Vlugt; Casestudy
27 written by Thijs J.H. Vlugt and Maddalena Venturoli. Additional
material about molecular simulations can be found on the web:
http://molsim.chem.uva.nl/course/material.html



We have done our best to remove all errors from this document and the
programs. However, we have to make the following statement:

We make no warranties, express or implied, that the programs contained
in this work are free of error, or that they will meet your
requirements for any particular application. They should not be
relied on for solving problems whose incorrect solution could results
in injury, damage, or loss of property. The authors and publishers
disclaim all liability for direct or consequential damages resulting
from your use of the programs.

## Contact 
If you have any suggestions or remarks, please send them to Berend
Smit (bsmit@science.uva.nl).

Berend Smit 
Department of Chemical Engineering 
University of Amsterdam 
Nieuwe Achtergracht 166 
1018 WV Amsterdam, The Netherlands 
Email: bsmit@science.uva.nl 
Homepage: http://molsim.chem.uva.nl

Daan Frenkel
FOM Institute for Atomic and Molecular Physics
Kruislaan 407 
1098 SJ Amsterdam
Email: frenkel@amolf.nl 
Homepage: http://www.amolf.nl/research/computational_physics/index.html
