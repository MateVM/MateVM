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

ptrdiff_t mateHandler(ptrdiff_t, ptrdiff_t, ptrdiff_t, ptrdiff_t);

#ifdef DBG_TRAP
#define dprintf(args...) do { printf (args); } while (0);
#else
#define dprintf(args...)
#endif

void chandler(int nSignal, siginfo_t *info, void *ctx)
{
	mcontext_t *mctx = &((ucontext_t *) ctx)->uc_mcontext;
	greg_t *regs = mctx->gregs;

	ptrdiff_t eip = (ptrdiff_t) regs[REG_EIP];
	ptrdiff_t eax = (ptrdiff_t) regs[REG_EAX];
	ptrdiff_t ebx = (ptrdiff_t) regs[REG_EBX];
	ptrdiff_t esp = (ptrdiff_t) regs[REG_ESP];
	ptrdiff_t esi = (ptrdiff_t) regs[REG_ESI];
	ptrdiff_t ebp = (ptrdiff_t) regs[REG_EBP];
	dprintf("trap: type %d, eip 0x%08x, eax 0x%08x, ebx 0x%08x, \n"
			"esp 0x%08x, *esp 0x%08x, *(ebp+8) 0x%08x\n", nSignal, eip,
			eax, ebx, esp, *(ptrdiff_t*) esp, *(ptrdiff_t *) (ebp + 8));

	ptrdiff_t ret = mateHandler(eip, eax, ebx, esi);
	if (ret == -1) {
		dprintf("regdump @ EIP: 0x%08x\n", regs[REG_EIP]);
		dprintf("\tEAX: 0x%08lx EBX: 0x%08lx ECX: 0x%08lx EDX: 0x%08lx\n",
			regs[REG_EAX], regs[REG_EBX], regs[REG_ECX], regs[REG_EDX]);
		dprintf("\tESI: 0x%08lx EDI: 0x%08lx EBP: 0x%08lx ESP: 0x%08lx\n",
			regs[REG_ESI], regs[REG_EDI], regs[REG_EBP], regs[REG_ESP]);
		mctx->gregs[REG_EIP] = eip + 6;
	} else {
		mctx->gregs[REG_EIP] = ret;
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
