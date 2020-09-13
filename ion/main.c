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

void lex_test() {
#define assert_token_kind(x) assert(match_token(x))
#define assert_token_float(x) assert(token.float_val == (x) && match_token(TOKEN_FLOAT))
#define assert_token_int(x) assert(token.int_val == (x) && match_token(TOKEN_INT))
#define assert_token_str(x) assert(strcmp(token.str_val, x) == 0 && match_token(TOKEN_STRING))
#define assert_token_eof() assert(token.kind == TOKEN_EOF)

    init_stream("0 1");
    assert_token_int(0);
    assert_token_int(1);
    assert_token_eof();
    

    // float literal
    init_stream("3.14 .123 42. 1e-9");
    assert_token_float(3.14);
    assert_token_float(.123);
    assert_token_float(42.);
    assert_token_float(1.e-9);
    assert_token_eof();

    // char literal tests
    init_stream("'a' '\\n'");
    assert_token_int('a');
    assert_token_int('\n');
    assert_token_eof();

    // string literal tests
    init_stream("\"foo bar\"");
    assert_token_str("foo bar");
    assert_token_eof();

    char *src = "+()1234+4589-_foo123";
    init_stream(src);
    expect_token('+');
    expect_token('(');
    expect_token(')');
    assert(token.int_val == 1234);
    expect_token(TOKEN_INT);
    expect_token('+');
    assert(token.int_val == 4589);
    expect_token(TOKEN_INT);
    expect_token('-');
    assert(token.name == str_intern("_foo123"));
    expect_token(TOKEN_NAME);
    expect_token(TOKEN_EOF);

    init_stream("18446744073709551615");
    assert(token.mod == TOKENMOD_NONE);
    assert_token_int(UINT64_MAX);
    assert_token_eof();

    init_stream("0xFf");
    assert(token.mod == TOKENMOD_HEX);
    assert_token_int(255);
    assert_token_eof();

    init_stream("0b11");
    assert(token.mod == TOKENMOD_BIN);
    assert_token_int(3);
    assert_token_eof();

    // ops and assignments
    init_stream("+ ++ += - -- -= : := < << <<=");
    assert_token_kind('+');
    assert_token_kind(TOKEN_INC);
    assert_token_kind(TOKEN_ADD_ASSIGN);
    assert_token_kind('-');
    assert_token_kind(TOKEN_DEC);
    assert_token_kind(TOKEN_SUB_ASSIGN);
    assert_token_kind(':');
    assert_token_kind(TOKEN_COLON_ASSIGN);
    assert_token_kind('<');
    assert_token_kind(TOKEN_LSHIFT);
    assert_token_kind(TOKEN_LSHIFT_ASSIGN);
    assert_token_eof();
#undef assert_token_kind
#undef assert_token_float
#undef assert_token_int
#undef assert_token_str
#undef assert_token_eof
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