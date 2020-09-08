#include <ctype.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include <stdarg.h>
#include <assert.h>

#define MAX(x,y) ((x) >= (y) ? (x) : (y))

void *xrealloc(void *ptr, size_t num_bytes) {
    ptr = realloc(ptr, num_bytes);
    if (!ptr) {
        perror("xrealloc failed");
        exit(1);    
    }
    return ptr;
}

void *xmalloc(size_t num_bytes) {
    void *ptr = malloc(num_bytes);
    if (!ptr) {
        perror("malloc failed");
        exit(1);
    }
    return ptr;
}


void fatal(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    printf("FATAL: ");
    vprintf(fmt, args);
    printf("\n");
    va_end(args);
    exit(1);
}

typedef struct BufHdr {
    size_t len;
    size_t cap;
    char buf[0];
} BufHdr;


#define buf__hdr(b) ((BufHdr *)((char *)b - offsetof(BufHdr, buf)))
#define buf__fits(b, n) (buf_len(b) + (n) <= buf_cap(b))
#define buf__fit(b, n) ((buf__fits(b, n)) ? 0 : ((b) = (buf__grow(b, buf_len(b) + (n), sizeof(*(b))))))


#define buf_len(b) ((b) ? (buf__hdr(b)->len) : 0U)
#define buf_cap(b) ((b) ? (buf__hdr(b)->cap) : 0U)
#define buf_push(b, x) (buf__fit(b, 1), (b)[buf_len(b)] = (x), buf__hdr(b)->len++)
#define buf_free(b) ((b) ? (free(buf__hdr(b)), (b) = NULL) : NULL)

void* buf__grow(const void *buf, size_t new_len, size_t elem_size) {
    size_t new_cap = MAX(1 + 2*buf_len(buf), new_len);
    assert(new_len <= new_cap);
    size_t new_size = new_cap * elem_size + offsetof(BufHdr, buf);
    BufHdr *new_hdr;
    if (buf) {
        new_hdr = xrealloc(buf__hdr(buf), new_size);
    } else {
        new_hdr = xmalloc(new_size);
        new_hdr->len = 0;
    }
    new_hdr->cap = new_cap;
    return new_hdr->buf;
}


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

typedef struct InternStr {
    size_t len;
    const char *str;
} InternStr;

static InternStr *interns;

const char *str_intern_range(const char *start, const char *end) {
    size_t len = end - start;
    for (size_t i = 0; i < buf_len(interns); i++) {
        if (interns[i].len == len && strncmp(start, interns[i].str, len) == 0) {
            return interns[i].str;
        }
    }
    char *str = xmalloc(len + 1);
    memcpy(str, start, len);
    str[len] = 0;
    buf_push(interns, ((InternStr){len, str}));
    return str;
}

const char *str_intern(const char *str) {
    return str_intern_range(str, str + strlen(str));
}

void str_intern_test() {
    char x[] = "hello";
    char y[] = "hello";
    assert(x != y);
    assert(str_intern(x) == str_intern(y));
    char z[] = "hello!";
    assert(str_intern(z) != str_intern(x));
}

typedef enum TokenKind {
    TOKEN_INT = 128,
    TOKEN_NAME,
} TokenKind;

// Warning: This returns a pointer to a static internal buffer, 
// so it'll be overwritten next call
const char *token_kind_name(TokenKind kind) {
    static char buf[256];
    switch (kind) {
    case TOKEN_INT:
        sprintf(buf, "integer");
        break;
    case TOKEN_NAME:
        sprintf(buf, "name");
        break;
    default:
        if (kind < 128 && isprint(kind)) {
            sprintf(buf, "%c", kind);
        } else {
            sprintf(buf, "<ASCII %d>", kind);
        }
    }
    return buf;
}

typedef struct Token {
    TokenKind kind;
    const char *start;
    const char *end;
    union {
        uint64_t val;
        const char *name;
    };
} Token;

Token token;
const char *stream;

const char *keyword_if;
const char *keyword_for;
const char *keyword_while;

void init_keywords() {
    keyword_if = str_intern("if");
    keyword_for = str_intern("for");
    keyword_while = str_intern("while");
}

void next_token() {
    token.start = stream;
    switch (*stream) {
        case '0':   
        case '1':
        case '2':
        case '3':   
        case '4':
        case '5':
        case '6':   
        case '7':
        case '8':
        case '9': {
            // todo: handle overflow
            uint64_t val = 0;
            while (isdigit(*stream)) {
                val *= 10;
                val += *stream++ - '0';
            }
            token.kind = TOKEN_INT;
            token.val = val;
            break;
        }
        case 'a':
        case 'b':
        case 'c':
        case 'd':
        case 'e':
        case 'f':
        case 'g':
        case 'h':
        case 'i':
        case 'j':
        case 'k':
        case 'l':
        case 'm':
        case 'n':
        case 'o':
        case 'p':
        case 'q':
        case 'r':
        case 's':
        case 't':
        case 'u':
        case 'v':
        case 'w':
        case 'x':
        case 'y':
        case 'z':
        case 'A':
        case 'B':
        case 'C':
        case 'D':
        case 'E':
        case 'F':
        case 'G':
        case 'H':
        case 'I':
        case 'J':
        case 'K':
        case 'L':
        case 'M':
        case 'N':
        case 'O':
        case 'P':
        case 'Q':
        case 'R':
        case 'S':
        case 'T':
        case 'U':
        case 'V':
        case 'W':
        case 'X':
        case 'Y':
        case 'Z': 
        case '_': {
            while (isalnum(*stream) || *stream == '_') {
                stream++;
            }
            token.name = str_intern_range(token.start, stream);
            token.kind = TOKEN_NAME;
            break;
        }
        default:
            token.kind = *stream++;
            break;
    }
    token.end = stream;

}

void init_stream(const char *str) {
    stream = str;
    next_token();
}

bool is_token(TokenKind kind) {
    return token.kind == kind;
}

bool is_token_name(const char *name) {
    return token.kind == TOKEN_NAME && token.name == name;
}

bool match_token(TokenKind kind) {
    if (is_token(kind)) {
        next_token();
        return true;
    } else {
        return false;
    }
}

bool expect_token(TokenKind kind) {
    if (!match_token(kind)) {
        fatal("expected token %s: %s", 
              token_kind_name(kind),
              token_kind_name(token.kind));
        return false;
    } else {
        return true;
    }
}

void lex_test() {
    char *src = "+()1234+4589-_foo123";
    init_stream(src);
    expect_token('+');
    expect_token('(');
    expect_token(')');
    expect_token(TOKEN_INT);
    assert(token.val == 1234);
    expect_token('+');
    expect_token(TOKEN_INT);
    assert(token.val == 4589);
    expect_token('-');
    expect_token(TOKEN_NAME);
    assert(token.name == str_intern("_foo123"));
}

/*
expr3 = INT | '(' expr ')'
expr2 = [-]? expr3
expr1 = expr2 ([* /] expr2)*
expr0 = expr1 ([+ -] expr1)*
expr = expr0
*/

int parse_expr();

int parse_expr3() {
    if (is_token(TOKEN_INT)) {
        int val = token.val;
        next_token();
        return val;
    } else if (match_token('(')) {
        int val = parse_expr();
        expect_token(')');
        return val;
    } else {
        fatal("expected integer or (, got %s", 
              token_kind_name(token.kind));
        // suppress warnings
        return 0;
    }
}

int parse_expr2() {
    if (match_token('-')) {
        // ...
        return -parse_expr3();
    } else {
        return parse_expr3();
    }
}

int parse_expr1() {
    int val = parse_expr2();
    while (is_token('*') || is_token('/')) {
        char op = token.kind;
        next_token();
        int rval = parse_expr2();
        if (op == '*') {
            val *= rval;
        } else {
            assert(op == '/');
            assert(rval != 0);
            val /= rval;            
        }
    }
    return val;
}

int parse_expr0() {
    int val = parse_expr1();
    while (is_token('+') || is_token('-')) {
        char op = token.kind;
        next_token();
        int rval = parse_expr1();
        if (op == '+') {
            val += rval;
        } else {
            assert(op == '-');
            val -= rval;
        }
    }
    return val;
}

int parse_expr() {
    return parse_expr0();
}

void test_parse_expr(const char *expr, int expected) {
    init_stream(expr);
    int result = parse_expr();
    assert(result == expected);
}

void parse_test() {
#define TEST_EXPR(x) test_parse_expr(#x, x)
    TEST_EXPR(1);
    TEST_EXPR((1));
    TEST_EXPR(-1);
    TEST_EXPR(1-2-3);
    TEST_EXPR(1*2+3);
    TEST_EXPR((1+2)*3);
#undef TEST_EXPR
}

int main(int argc, char** argv) {
    buf_test();
    lex_test();
    str_intern_test();
    parse_test();
    printf("all passed\n");
    return 0;
}