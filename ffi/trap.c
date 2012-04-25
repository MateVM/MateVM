#include <stdio.h>
#include <stdlib.h>

/* TODO(bernhard): use {u,}int* types */

#define __USE_GNU
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

#include <sys/ucontext.h>

unsigned int getMethodEntry(unsigned int, unsigned int);
unsigned int getStaticFieldAddr(unsigned int, void*);

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
NEW_MAP(virtual)


void mainresult(unsigned int a)
{
	printf("mainresult: 0x%08x\n", a);
}

void callertrap(int nSignal, siginfo_t *info, void *ctx)
{
	mcontext_t *mctx = &((ucontext_t *) ctx)->uc_mcontext;
	unsigned int from = (unsigned int) mctx->gregs[REG_EIP] - 2;
	unsigned int *to_patch = (unsigned int *) (from + 1);
	printf("callertrap(mctx)  by 0x%08x\n", from);
	if (*to_patch != 0x90ffff90) {
		printf("callertrap: something is wrong here. abort\n");
		exit(0);
	}
	unsigned int patchme = getMethodEntry(from, 0);

	unsigned char *insn = (unsigned char *) from;
	*insn = 0xe8; // call opcode
	printf(" to_patch: 0x%08x\n", (unsigned int) to_patch);
	printf("*to_patch: 0x%08x\n", *to_patch);
	*to_patch = patchme - (from + 5);
	printf("*to_patch: 0x%08x\n", *to_patch);
	mctx->gregs[REG_EIP] = (unsigned long) insn;
}

void staticfieldtrap(int nSignal, siginfo_t *info, void *ctx)
{
	/* TODO(bernhard): more generic and cleaner please... */
	mcontext_t *mctx = &((ucontext_t *) ctx)->uc_mcontext;
	unsigned int from = (unsigned int) mctx->gregs[REG_EIP];
	if (from < 0x10000) { // invokevirtual
		if (from > 0) {
			printf("from: 0x%08x but should be 0 :-(\n", from);
		}
		unsigned int method_table_ptr = (unsigned int) mctx->gregs[REG_EAX];
		unsigned int *esp = (unsigned int *) mctx->gregs[REG_ESP];
		/* get actual eip from stack storage */
		unsigned int from = (*esp) - 3;
		unsigned char offset = *((unsigned char *) (*esp) - 1);
		/* method entry to patch */
		unsigned int *to_patch = (unsigned int*) (method_table_ptr + offset);
		printf("invokevirtual by 0x%08x with offset 0x%08x\n", from, offset);
		printf(" to_patch: 0x%08x\n", (unsigned int) to_patch);
		printf("*to_patch: 0x%08x\n", *to_patch);
		*to_patch = getMethodEntry(from, method_table_ptr);
		mctx->gregs[REG_EIP] = *to_patch;
		printf("*to_patch: 0x%08x\n", *to_patch);
	} else {
	unsigned int *to_patch = (unsigned int *) (from + 2);
	printf("staticfieldtrap by 0x%08x\n", from);
	if (*to_patch != 0x00000000) {
		printf("staticfieldtrap: something is wrong here. abort\n");
		exit(0);
	}
	unsigned int patchme = getStaticFieldAddr(from, trap_map);

	printf(" to_patch: 0x%08x\n", (unsigned int) to_patch);
	printf("*to_patch: 0x%08x\n", *to_patch);
	*to_patch = patchme;
	printf("*to_patch: 0x%08x\n", *to_patch);
	}
}

void register_signal(void)
{
	struct sigaction illaction;
	illaction.sa_sigaction = callertrap;
	sigemptyset(&illaction.sa_mask);
	illaction.sa_flags = SA_SIGINFO | SA_RESTART | SA_NODEFER;
	sigaction(SIGILL, &illaction, NULL);

	struct sigaction segvaction;
	segvaction.sa_sigaction = staticfieldtrap;
	sigemptyset(&segvaction.sa_mask);
	segvaction.sa_flags = SA_SIGINFO | SA_RESTART | SA_NODEFER;
	sigaction(SIGSEGV, &segvaction, NULL);
}

unsigned int getaddr(void)
{
	return (unsigned int) mainresult;
}

unsigned int getMallocAddr(void)
{
	return (unsigned int) malloc;
}
