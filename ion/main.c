#include "ion.c"

void run_tests() {
    buf_test();
    lex_test();
    str_intern_test();
    printf("all passed\n");
}


int main(int argc, char** argv) {
    run_tests();
    return 0;
}