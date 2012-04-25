#include <stdio.h>

void tests_Native1__printSomething____V(void)
{
	printf("printSomething: woot \\o/\n");
}

void tests_Native1__printSomething___I_V(int a)
{
	printf("printSomething: 0x%08x\n", a);
}

void tests_Instance5__printX___I_V(int a)
{
	printf("printX: 0x%08x\n", a);
}

void java_io_PrintStream__printf___I_V(int a)
{
	printf("printstream: 0x%08x\n", a);
}
