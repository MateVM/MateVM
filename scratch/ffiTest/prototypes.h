extern void registerSignalHandlers(void);
extern void mateTrapHandler(unsigned int signal,
                            siginfo_t *sigInfo,
                            void *ctx,
                            unsigned int eip);


extern void registerSignalHandlers2(void (*f)(int, siginfo_t*,void*));
