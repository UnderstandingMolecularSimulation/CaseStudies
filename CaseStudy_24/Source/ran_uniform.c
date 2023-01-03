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
