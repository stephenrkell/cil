// From some new MacOS headers: https://github.com/goblint/cil/issues/168

void foo(void) __attribute__((availability(macos,introduced=10.15)));

void foo(void) {
    return;
}

// Version numbers may have multiple dots: https://github.com/goblint/cil/pull/171#issuecomment-2250670652
void bar(void) __attribute__((availability(macos,introduced=10.13.4)));

void bar(void) {
    return;
}
