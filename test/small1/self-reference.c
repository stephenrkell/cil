extern void *myrealloc(void *__ptr) __attribute__ ((__malloc__ (__builtin_free, 1)));
/* Add also as its own deallocator. */
extern void *myrealloc(void *__ptr) __attribute__ ((__malloc__ (myrealloc, 1)));

void *myrealloc(void *__ptr) { return (void*)0; }