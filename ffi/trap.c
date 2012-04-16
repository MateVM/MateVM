#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <asm/ucontext.h>

unsigned int getMethodEntry(unsigned int, void *, void *);

void *method_map = NULL;
void *caller_map = NULL;

void set_mmap(void *mmap)
{
	printf("set_mmap: 0x%08x\n", (unsigned int) mmap);
	method_map = mmap;
}

void *get_mmap()
{
	printf("get_mmap: 0x%08x\n", (unsigned int) method_map);
	return method_map;
}

void set_cmap(void *cmap)
{
	printf("set_cmap: 0x%08x\n", (unsigned int) cmap);
	caller_map = cmap;
}

void *get_cmap()
{
	printf("get_cmap: 0x%08x\n", (unsigned int) caller_map);
	return caller_map;
}


void mainresult(unsigned int a)
{
	printf("mainresult: 0x%08x\n", a);
}

void callertrap(int nSignal, siginfo_t *info, void *ctx)
{
	struct ucontext *uctx = (struct ucontext *) ctx;
	unsigned int from = (unsigned int) uctx->uc_mcontext.eip;
	unsigned int patchme = getMethodEntry(from, method_map, caller_map);

	printf("callertrap(mctx)  by 0x%08x\n", from);
	// printf("callertrap(addr)  by 0x%08x\n", info->si_addr);
	// printf("callertrap(*esp)  by 0x%08x\n", * (unsigned int *) uctx->uc_mcontext.esp);

	unsigned int *to_patch = (unsigned int *) (uctx->uc_mcontext.eip + 2);
	unsigned char *insn = (unsigned char *) (uctx->uc_mcontext.eip);
	*insn = 0x90; // nop
	insn++;
	*insn = 0xe8; // call
	printf(" to_patch: 0x%08x\n", (unsigned int) to_patch);
	printf("*to_patch: 0x%08x\n", *to_patch);
	if (*to_patch != 0x00000000) {
		printf("something is wrong here. abort\n");
		exit(0);
	}
	*to_patch = (unsigned int) patchme - ((unsigned int) insn + 5);
	printf("*to_patch: 0x%08x\n", *to_patch);
	uctx->uc_mcontext.eip = (unsigned long) insn;
	// while (1) ;
}

void register_signal(void)
{
	struct sigaction segvaction;
	segvaction.sa_sigaction = callertrap;
	sigemptyset(&segvaction.sa_mask);
	segvaction.sa_flags = SA_SIGINFO | SA_RESTART;
	sigaction(SIGSEGV, &segvaction, NULL);
}

unsigned int getaddr(void)
{
	return (unsigned int) mainresult;
}
