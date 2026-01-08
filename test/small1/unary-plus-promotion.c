#include "testharness.h"
#include <stdlib.h>

int main() {
  char c;
  size_t csize = sizeof(c);
  size_t pluscsize = sizeof(+c);
  if (csize != sizeof(char))
    E(1);
  if (pluscsize != sizeof(int))
    E(2);
  SUCCESS;
}
