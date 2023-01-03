#include <sys/time.h>

double
ttime_()

{
  struct  timeval now;
  double          sec;

  if (gettimeofday(&now, (struct timezone *) 0)) 
    {
      return(0);
    }

  sec = (double) now.tv_sec + (double) now.tv_usec / 1000000;
  return(sec);
}
