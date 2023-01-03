#include<time.h>

int
sstmm_()

{time_t ourtime;
 time(&ourtime);
 return (int)(ourtime&0x00000fff);
}
