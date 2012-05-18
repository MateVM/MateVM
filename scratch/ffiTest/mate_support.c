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

#include "prototypes.h"

void trap(int nSignal, siginfo_t *info, void *ctx)
{
	printf("sig: %d\n", nSignal);
	
	mcontext_t *mctx = &((ucontext_t *) ctx)->uc_mcontext;
	unsigned int from = (unsigned int) mctx->gregs[REG_EIP];

	//mateTrapHandler(nSignal, info, ctx, from);

	printf("from: 0x%08x\n", from);
	exit(0);
}


void registerSignalHandlers2(void (*f)(int, siginfo_t*,void*))
{
	printf("registering2\n");
	struct sigaction illaction;
	illaction.sa_sigaction = f;
	sigemptyset(&illaction.sa_mask);
	illaction.sa_flags = SA_SIGINFO | SA_RESTART | SA_NODEFER;
	sigaction(SIGILL, &illaction, NULL);
}

