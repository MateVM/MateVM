#include <stdio.h>
#include <stdlib.h>

#include "../debug.h"

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

unsigned int mallocObject(int);
unsigned int mateHandler(unsigned int, unsigned int, unsigned int, unsigned int);

#ifdef DBG_TRAP
#define dprintf(args...) do { printf (args); } while (0);
#else
#define dprintf(args...)
#endif

void mainresult(unsigned int a)
{
	dprintf("mainresult: 0x%08x\n", a);
}

void chandler(int nSignal, siginfo_t *info, void *ctx)
{
	mcontext_t *mctx = &((ucontext_t *) ctx)->uc_mcontext;

	unsigned int eip = (unsigned int) mctx->gregs[REG_EIP];
	unsigned int eax = (unsigned int) mctx->gregs[REG_EAX];
	unsigned int ebx = (unsigned int) mctx->gregs[REG_EBX];
	unsigned int esp = (unsigned int) mctx->gregs[REG_ESP];
	dprintf("trap: type %d, eip 0x%08x, eax 0x%08x, ebx 0x%08x, "
			"esp 0x%08x, *esp 0x%08x\n", nSignal, eip,
			eax, ebx, esp, *(unsigned int*) esp);

	mctx->gregs[REG_EIP] = mateHandler(eip, eax, ebx, esp);
}

void register_signal(void)
{
	struct sigaction illaction;
	illaction.sa_sigaction = chandler;
	sigemptyset(&illaction.sa_mask);
	illaction.sa_flags = SA_SIGINFO | SA_RESTART | SA_NODEFER;
	sigaction(SIGILL, &illaction, NULL);

	struct sigaction segvaction;
	segvaction.sa_sigaction = chandler;
	sigemptyset(&segvaction.sa_mask);
	segvaction.sa_flags = SA_SIGINFO | SA_RESTART | SA_NODEFER;
	sigaction(SIGSEGV, &segvaction, NULL);
}

unsigned int getaddr(void)
{
	return (unsigned int) mainresult;
}

unsigned int getMallocObjectAddr(void)
{
	return (unsigned int) mallocObject;
}
