#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>

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

void mateHandler(ptrdiff_t*);

#ifdef DBG_TRAP
#define dprintf(args...) do { printf (args); } while (0);
#else
#define dprintf(args...)
#endif

void chandler(int nSignal, siginfo_t *info, void *ctx)
{
	mcontext_t *mctx = &((ucontext_t *) ctx)->uc_mcontext;
	greg_t *regs = mctx->gregs;

#define HWREGS 9
	ptrdiff_t ret[HWREGS] = {0};
	int order[HWREGS] = {REG_EAX, REG_ECX, REG_EDX, REG_EBX,
		            REG_ESP, REG_EBP, REG_ESI, REG_EDI,
					REG_EIP};
	int i;
	for (i = 0; i < HWREGS; i++)
		ret[i] = (ptrdiff_t) regs[order[i]];

	mateHandler(ret);
	if (ret[0] == -1) {
		dprintf("regdump @ EIP: 0x%08x\n", regs[REG_EIP]);
		dprintf("\tEAX: 0x%08lx EBX: 0x%08lx ECX: 0x%08lx EDX: 0x%08lx\n",
			regs[REG_EAX], regs[REG_EBX], regs[REG_ECX], regs[REG_EDX]);
		dprintf("\tESI: 0x%08lx EDI: 0x%08lx EBP: 0x%08lx ESP: 0x%08lx\n",
			regs[REG_ESI], regs[REG_EDI], regs[REG_EBP], regs[REG_ESP]);
		mctx->gregs[REG_EIP] = regs[REG_EIP] + 6;
	} else {
		for (i = 0; i < HWREGS; i++)
			regs[order[i]] = ret[i];
	}
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
