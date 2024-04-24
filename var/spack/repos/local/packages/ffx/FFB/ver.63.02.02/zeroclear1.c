#include <stdio.h>
#include <string.h>

void zeroclear1_(float* PA, int* PSZ)
{
	int SZ = *PSZ;

//	printf("PA=%x, SZ=%d\n", PA, SZ);

	memset((void*)PA,
		   0,
		   SZ*sizeof(float));
}
