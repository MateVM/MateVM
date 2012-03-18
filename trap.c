#include <stdio.h>

void callertrap(void)
{
	char buf[5];
	unsigned int *ptr = (unsigned int) (buf + 1);

	printf("callertrap by 0x%08x\n", *(ptr + 4));
	/* TODO:
	 * call magic haskell function
	 * with environment information */
}

unsigned int getaddr(void)
{
	return (unsigned int) callertrap;
}
