#include <stdio.h>

static int bar(int x) {
    return x + 1;
}

int foo(int a) {
    int b = a;
    while (b < 45) {
      b = bar(b);
    }    /* single call site */
    return b;
}

int main(void) {
    int r = foo(41);
    printf("%d\n", r);
    return 0;
}
