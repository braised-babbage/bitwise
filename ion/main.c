#include "common.h"
#include "lex.h"
#include "ast.h"

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
    lex_test();
    str_intern_test();
    ast_test();
    printf("\nall passed\n");
}


int main(int argc, char** argv) {
    run_tests();
    return 0;
}