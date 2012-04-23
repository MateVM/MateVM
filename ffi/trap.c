#include <stdio.h>
#include <stdlib.h>

// Note by hs: my signal.h includes sys/uconctext which conflicts with
// asm/ucontext - this hack kinda solves the problem for me ;-) 
// so feel free to blame me for that s**t
#if defined __USE_XOPEN2K8
#undef __USE_XOPEN2K8
#define RESTORE
#warning hs-hack: undefining __USE_XOPEN2K8 for signal.h
#endif
#include <signal.h>
#ifdef RESTORE
#define __USE_XOPEN2K8
#endif

#include <asm/ucontext.h>

unsigned int getMethodEntry(unsigned int, void *, void *);
unsigned int getFieldAddr(unsigned int, void*);

#define NEW_MAP(prefix) \
	void* prefix ## _map = NULL; \
	void set_ ## prefix ## map(void *map) \
	{ \
		printf("set_%s: 0x%08x\n", #prefix , (unsigned int) map); \
		prefix ## _map = map; \
	} \
	void *get_ ## prefix ## map() \
	{ \
		printf("get_%s: 0x%08x\n", #prefix , (unsigned int) prefix ## _map); \
		return prefix ## _map; \
	}

NEW_MAP(method)
NEW_MAP(trap)
NEW_MAP(class)


void mainresult(unsigned int a)
{
	printf("mainresult: 0x%08x\n", a);
}

void callertrap(int nSignal, siginfo_t *info, void *ctx)
{
	struct ucontext *uctx = (struct ucontext *) ctx;
	unsigned int from = (unsigned int) uctx->uc_mcontext.eip - 2;
	unsigned int patchme = getMethodEntry(from, method_map, trap_map);

	printf("callertrap(mctx)  by 0x%08x\n", from);

	unsigned int *to_patch = (unsigned int *) (from + 1);
	unsigned char *insn = (unsigned char *) from;
	*insn = 0xe8; // call opcode
	printf(" to_patch: 0x%08x\n", (unsigned int) to_patch);
	printf("*to_patch: 0x%08x\n", *to_patch);
	if (*to_patch != 0x90ffff90) {
		printf("something is wrong here. abort\n");
		exit(0);
	}
	*to_patch = patchme - (from + 5);
	printf("*to_patch: 0x%08x\n", *to_patch);
	uctx->uc_mcontext.eip = (unsigned long) insn;
	// while (1) ;
}

void staticfieldtrap(int nSignal, siginfo_t *info, void *ctx)
{
	struct ucontext *uctx = (struct ucontext *) ctx;
	unsigned int from = (unsigned int) uctx->uc_mcontext.eip;
	unsigned int patchme = getFieldAddr(from, trap_map);
	unsigned int *to_patch = (unsigned int *) (from + 2);

	printf("staticfieldtrap by 0x%08x\n", from);
	printf(" to_patch: 0x%08x\n", (unsigned int) to_patch);
	printf("*to_patch: 0x%08x\n", *to_patch);
	if (*to_patch != 0x00000000) {
		printf("something is wrong here. abort\n");
		exit(0);
	}
	*to_patch = patchme;
	printf("*to_patch: 0x%08x\n", *to_patch);
}

void register_signal(void)
{
	struct sigaction illaction;
	illaction.sa_sigaction = callertrap;
	sigemptyset(&illaction.sa_mask);
	illaction.sa_flags = SA_SIGINFO | SA_RESTART;
	sigaction(SIGILL, &illaction, NULL);

	struct sigaction segvaction;
	segvaction.sa_sigaction = staticfieldtrap;
	sigemptyset(&segvaction.sa_mask);
	segvaction.sa_flags = SA_SIGINFO | SA_RESTART;
	sigaction(SIGSEGV, &segvaction, NULL);
}

unsigned int getaddr(void)
{
	return (unsigned int) mainresult;
}
