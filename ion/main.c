#include "common.h"
#include "lex.h"
#include "print.h"
#include "parse.h"
#include "resolve.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

void buf_test() {
    int* asd = NULL;
    for (int i = 0; i < 5; i++) {
        buf_push(asd, i*i);
    }
    assert(buf_len(asd) == 5);
    for (int i = 0; i < buf_len(asd); i++) {
        assert(asd[i] == i*i);
    }
    buf_free(asd);
    assert(asd == NULL);
    char *str = NULL;
    buf_printf(str, "One: %d\n", 1);
    assert(strcmp(str, "One: 1\n") == 0);
    buf_printf(str, "Hex: 0x%x\n", 0x12345678);
    assert(strcmp(str, "One: 1\nHex: 0x12345678\n") == 0);
}

void str_intern_test() {
    char x[] = "hello";
    char y[] = "hello";
    assert(x != y);
    assert(str_intern(x) == str_intern(y));
    char z[] = "hello!";
    assert(str_intern(z) != str_intern(x));
}


void run_tests() {
    buf_test();
    str_intern_test();
    lex_test();
    print_test();
    parse_test();
    resolve_test();
    printf("\nall passed\n");
}


int main(int argc, char** argv) {
    run_tests();
    return 0;
}