/* A C-program for MT19937: Real number version  (1998/4/6)        */
/*   genrand() generates one pseudorandom real number (double)     */
/* which is uniformly distributed on [0,1]-interval, for each      */
/* call. sgenrand(seed) set initial values to the working area     */
/* of 624 words. Before genrand(), sgenrand(seed) must be          */
/* called once. (seed is any 32-bit integer except for 0).         */
/* Integer generator is obtained by modifying two lines.           */
/*   Coded by Takuji Nishimura, considering the suggestions by     */
/* Topher Cooper and Marc Rieffel in July-Aug. 1997.               */
/*                                                                 */
/* This library is free software; you can redistribute it and/or   */
/* modify it under the terms of the GNU Library General Public     */
/* License as published by the Free Software Foundation; either    */
/* version 2 of the License, or (at your option) any later         */
/* version.                                                        */
/* This library is distributed in the hope that it will be useful, */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of  */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.            */
/* See the GNU Library General Public License for more details.    */
/* You should have received a copy of the GNU Library General      */
/* Public License along with this library; if not, write to the    */
/* Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA   */ 
/* 02111-1307  USA                                                 */
/*                                                                 */
/* Copyright (C) 1997 Makoto Matsumoto and Takuji Nishimura.       */
/* When you use this, send an email to: matumoto@math.keio.ac.jp   */
/* with an appropriate reference to your work.                     */
/*                                                                 */
/* REFERENCE                                                       */
/* M. Matsumoto and T. Nishimura,                                  */
/* "Mersenne Twister: A 623-Dimensionally Equidistributed Uniform  */
/* Pseudo-Random Number Generator",                                */
/* ACM Transactions on Modeling and Computer Simulation,           */
/* Vol. 8, No. 1, January 1998, pp 3--30.                          */
/*                                                                 */
/* See also 1) http://random.mat.sbg.ac.at                         */
/*          2) http://www.math.keio.ac.jp/~matumoto/emt.html       */
/*                                                                 */
/* Slightly modified by Thijs J.H. Vlugt on 21/12/1998             */

#include<stdio.h>
#define N                     624
#define M                     397
#define MATRIX_A              0x9908b0df   
#define UPPER_MASK            0x80000000 
#define LOWER_MASK            0x7fffffff 
#define TEMPERING_MASK_B      0x9d2c5680
#define TEMPERING_MASK_C      0xefc60000
#define TEMPERING_SHIFT_U(y)  (y >> 11)
#define TEMPERING_SHIFT_S(y)  (y << 7)
#define TEMPERING_SHIFT_T(y)  (y << 15)
#define TEMPERING_SHIFT_L(y)  (y >> 18)

static unsigned long mt[N];
static int mti;

double ran_uniform__()
{
  unsigned long y;
  static unsigned long mag01[2]={0x0, MATRIX_A};
  double zzz=2.0;

    while (zzz < 0.0000000000001e0 || zzz > 0.9999999999999e0)
      {
	if (mti >= N) 
	  { 
	    int kk;

	    for (kk=0;kk<N-M;kk++) 
	      {
		y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
		mt[kk] = mt[kk+M] ^ (y >> 1) ^ mag01[y & 0x1];
	      }
	  
	    for (;kk<N-1;kk++) 
	      {
		y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
		mt[kk] = mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1];
	      }
	  
	    y = (mt[N-1]&UPPER_MASK)|(mt[0]&LOWER_MASK);
	    mt[N-1] = mt[M-1] ^ (y >> 1) ^ mag01[y & 0x1];

	    mti = 0;
	  }
  
	y = mt[mti++];
	y ^= TEMPERING_SHIFT_U(y);
	y ^= TEMPERING_SHIFT_S(y) & TEMPERING_MASK_B;
	y ^= TEMPERING_SHIFT_T(y) & TEMPERING_MASK_C;
	y ^= TEMPERING_SHIFT_L(y);
	
	zzz = ((double)y/(unsigned long)0xffffffff);
      }
    return(zzz);
}

void genrand_(double *seed)
{
  unsigned long myint;
  int kk;
  double dummy;

  myint = (unsigned long)((*seed)*429496729);
  myint = myint + 1000;

  if (myint%2 == 0) 
    myint = myint + 1;
  else
    myint = myint + 2;

  mt[0]= myint & 0xffffffff;

  for (mti=1; mti<N; mti++)
    mt[mti] = (69069 * mt[mti-1]) & 0xffffffff;

  for (kk=1; kk<10000; kk++)
    dummy = ran_uniform__();

  return;
}
