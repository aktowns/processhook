#include <HsFFI.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

static bool lock = false;

bool his_locked() {
    return lock;
}

void hlock() {
    lock = true;
}

void hunlock() {
    lock = false;
}

static void hook_enter(void) __attribute__((constructor));
static void hook_enter(void) {
    // char* hooked = getenv("IS_HOOKED");

    // if (hooked == NULL) {
        printf("* Initializing haskell\n");
        static char *argv[] = { "phook.so", 0 }, **argv_ = argv;
        static int argc = 1;
        hs_init(&argc, &argv_);

        setenv("IS_HOOKED", "1", true);
    // }
}

static void hook_exit(void) __attribute__((destructor)); 
static void hook_exit(void) {
    // char* hooked = getenv("IS_HOOKED");

    // if (hooked != NULL) {
        printf("* exiting hook\n");
        hs_exit();

        unsetenv("IS_HOOKED");
    // }
}
