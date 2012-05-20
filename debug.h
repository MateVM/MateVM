/* DBG_JIT .... see generated code and CodeGen information
 * DBG_BB ..... BasicBlock information
 * DBG_MP ..... MethodPool.hs
 * DBG_CP ..... ClassPool.hs
 * DBG_STR .... Strings.hs
 * // no printf* defined
 * DBG_TRAP ... show information @ trap.c
 * DBG_CLASS .. dump classfile
 */

/* ooops defines */
#ifdef BG_ALL
#define DBG_ALL
#endif

#ifdef BG_JIT
#define DBG_JIT
#endif

#ifdef BG_BB
#define DBG_BB
#endif

#ifdef BG_MP
#define DBG_MP
#endif

#ifdef BG_CP
#define DBG_CP
#endif

#ifdef BG_STR
#define DBG_STR
#endif

#ifdef BG_TRAP
#define DBG_TRAP
#endif

#ifdef BG_CLASS
#define DBG_CLASS
#endif

/* if one constant from above is defined, we want to import
 * libraries like Text.Printf
 * needed for gettting proper `-Wall' output on a release build */

#if defined(DBG_ALL) || defined(DBG_JIT) || defined(DBG_BB) || defined(DBG_MP) || defined(DBG_CP) || defined(DBG_STR)
#define DEBUG
#endif

#if defined(DBG_ALL)
#define DBG_JIT
#define DBG_BB
#define DBG_MP
#define DBG_CP
#define DBG_STR
#define DBG_TRAP
#if 0
#define DBG_CLASS
#endif
#endif

/* it would be awesome if we could just write
 * > printfFake = printf
 * here, but the type can't be infered, since `PrintfType'
 * isn't visible (at least this is my explanation :/).
 * if I'm wrong, move this to `Mate/Debug.hs'
 */
#ifdef DBG_JIT
#define printfJit printf
#endif

#ifdef DBG_BB
#define printfBb printf
#endif

#ifdef DBG_MP
#define printfMp printf
#endif

#ifdef DBG_CP
#define printfCp printf
#endif

#ifdef DBG_STR
#define printfStr printf
#endif
