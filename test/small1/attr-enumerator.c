// From some new MacOS headers

enum {
    A,
    B __attribute__((availability(macos,introduced=10.15))),
    C = 5,
    D __attribute__((availability(macos,introduced=10.15))) = 7
} E;
