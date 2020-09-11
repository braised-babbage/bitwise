#include <ctype.h>
#include <limits.h>
#include <math.h>
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

void syntax_error(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    printf("Syntax Error: ");
    vprintf(fmt, args);
    printf("\n");
    va_end(args);
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
#define buf_end(b) ((b) + buf_len(b))
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

typedef struct Intern {
    size_t len;
    const char *str;
} Intern;

static Intern *interns;

const char *str_intern_range(const char *start, const char *end) {
    size_t len = end - start;
    for (Intern* it = interns; it != buf_end(interns); it++) {
        if (it->len == len && strncmp(start, it->str, len) == 0) {
            return it->str;
        }
    }
    char *str = xmalloc(len + 1);
    memcpy(str, start, len);
    str[len] = 0;
    buf_push(interns, ((Intern){len, str}));
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
    TOKEN_EOF = 0,
    TOKEN_INT = 128,
    TOKEN_FLOAT,
    TOKEN_NAME,
    TOKEN_STRING,
    TOKEN_INC,
    TOKEN_DEC,
    TOKEN_LSHIFT,
    TOKEN_RSHIFT,
    TOKEN_EQ,
    TOKEN_NOTEQ,
    TOKEN_LTEQ,
    TOKEN_GTEQ,
    TOKEN_AND,
    TOKEN_OR,
    TOKEN_ADD_ASSIGN,
    TOKEN_SUB_ASSIGN,
    TOKEN_COLON_ASSIGN,
    TOKEN_AND_ASSIGN,
    TOKEN_OR_ASSIGN,
    TOKEN_XOR_ASSIGN,
    TOKEN_LSHIFT_ASSIGN,
    TOKEN_RSHIFT_ASSIGN,
    TOKEN_MUL_ASSIGN,
    TOKEN_DIV_ASSIGN,
    TOKEN_MOD_ASSIGN,
} TokenKind;

typedef enum TokenMod {
    TOKENMOD_NONE,
    TOKENMOD_HEX,
    TOKENMOD_BIN,
    TOKENMOD_CHAR,
} TokenMod;

size_t copy_token_kind_str(char *dest, size_t dest_size, TokenKind kind) {
    size_t n = 0;
    switch (kind) {
    case TOKEN_EOF:
        n = snprintf(dest, dest_size, "end of file");
        break;
    case TOKEN_INT:
        n = snprintf(dest, dest_size, "integer");
        break;
    case TOKEN_NAME:
        n = snprintf(dest, dest_size, "name");
        break;
    default:
        if (kind < 128 && isprint(kind)) {
            n = snprintf(dest, dest_size, "%c", kind);
        } else {
            n = snprintf(dest, dest_size, "<ASCII %d>", kind);
        }
    }
    return n;
}

const char *token_kind_str(TokenKind kind) {
    static char buf[256];
    size_t n = copy_token_kind_str(buf, sizeof(buf), kind);
    assert(n + 1 <= sizeof(buf));
    return buf;
}

typedef struct Token {
    TokenKind kind;
    TokenMod mod;
    const char *start;
    const char *end;
    union {
        uint64_t int_val;
        double float_val;
        const char *name;
        const char *str_val;
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

uint8_t char_to_digit[256] = {
    ['0'] = 0,
    ['1'] = 1,
    ['2'] = 2,
    ['3'] = 3,
    ['4'] = 4,
    ['5'] = 5,
    ['6'] = 6,
    ['7'] = 7,
    ['8'] = 8,
    ['9'] = 9,
    ['a'] = 10, ['A'] = 10,
    ['b'] = 11, ['B'] = 11,
    ['c'] = 12, ['C'] = 12,
    ['d'] = 13, ['D'] = 12,
    ['e'] = 14, ['E'] = 14,
    ['f'] = 15, ['F'] = 15,
};

void scan_int() {
    uint64_t base = 10;
    token.mod = TOKENMOD_NONE;
    if (*stream == '0') {
        stream++;
        if (tolower(*stream) == 'x') {
            stream++;
            base = 16;
            token.mod = TOKENMOD_HEX;
        } else if (tolower(*stream) == 'b') {
            stream++;
            base = 2;
            token.mod = TOKENMOD_BIN;
        } else if(!isspace(*stream)) {
            syntax_error("Invalid integer literal suffix '%c'", stream);
            stream++;
        }
    }
    uint64_t val = 0;
    for (;;) {
        uint64_t digit = char_to_digit[*stream];
        if (digit == 0 && *stream != '0') {
            break;
        }
        if (digit >= base) {
            syntax_error("Digit '%c' out of range for base %llu", *stream, base);
            digit = 0;
        }
        if (val > ((UINT64_MAX - digit) / base)) {
            syntax_error("Integer literal overflow.");
            while (isdigit(*stream)) {
                stream++;
            }
            val = 0;
            break;
        }
        val = val*base + digit;
        stream++;
    }
    token.int_val = val;
    token.kind = TOKEN_INT;
}

void scan_float() {
    const char *start = stream;
    while (isdigit(*stream)) {
        stream++;
    }
    if (*stream == '.') {
        stream++;
    }
    while (isdigit(*stream)) {
        stream++;
    }
    if (tolower(*stream) == 'e') {
        stream++;
        if (*stream == '+' || *stream == '-') {
            stream++;
        }
        if (!isdigit(*stream)) {
            syntax_error("Expected digit after float literal exponent, found '%c'.", stream);
        }
        while (isdigit(*stream)) {
            stream++;
        }
    }
    const char *end = stream;
    double val = strtod(start, NULL);
    if (val == HUGE_VAL || val == -HUGE_VAL) {
        syntax_error("Float literal overflow.");
    }
    token.float_val = val;
    token.kind = TOKEN_FLOAT;
}

char escape_to_char[256] = {
    ['n'] = '\n',
    ['r'] = '\r',
    ['t'] = '\t',
    ['v'] = '\v',
    ['b'] = '\b',
    ['a'] = '\a',
    ['0'] = 0,
};

void scan_char() {
    assert(*stream == '\'');
    stream++;

    char val = 0;
    if (*stream == '\'') {
        syntax_error("Char literal cannot be empty");
        stream++;
    } else if (*stream == '\n') {
        syntax_error("Char literal cannot contain newline");
        stream++;
    } else if(*stream == '\\') {
        stream++;
        val = escape_to_char[*stream];
        if (val == 0 && *stream != '0') {
            syntax_error("Invalid char literal escape '\\%c'", *stream);
        }
        stream++;
    } else {
        val = *stream;
        stream++;
    }
    if (*stream != '\'') {
        syntax_error("Expected closing char quote, got '%c'", *stream);
    }
    stream++;
    token.kind = TOKEN_INT;
    token.int_val = val;
    token.mod = TOKENMOD_CHAR;
}

void scan_str() {
    assert(*stream == '"');
    stream++;

    char *str = NULL;
    while (*stream && *stream != '"') {
        char val = *stream;
        if (val == '\n') {
            syntax_error("Char literal cannot contain newline");
        } else if(val == '\\') {
            stream++;
            val = escape_to_char[*stream];
            if (val == 0 && *stream != '0') {
                syntax_error("Invalid char literal escape '\\%c'", *stream);
            }
          
        } else {
            // default case
        }
        buf_push(str, val);
        stream++;
    }
    if (*stream) {
        assert(*stream == '"');
        stream++;
    } else {
        syntax_error("unexpected end of file in string literal");
    }
    buf_push(str, 0);
    token.kind = TOKEN_STRING;
    token.str_val = str;
}

void next_token() {
top:
    token.start = stream;
    switch (*stream) {
        case ' ': case '\n': case '\r': case '\t': case '\v': {
            while (isspace(*stream)) {
                stream++;
            }
            goto top;
        }
        case '.': {
            scan_float();
            break;
        }
        case '\'': {
            scan_char();
            break;
        }
        case '"': {
            scan_str();
            break;
        }
        case '0':  case '1': case '2': case '3': case '4':
        case '5':  case '6': case '7': case '8': case '9': {
            while (isdigit(*stream)) {
                stream++;
            }
            if (*stream == '.' || *stream == 'e') {
                stream = token.start;
                scan_float();
            } else {    
                stream = token.start;
                scan_int();
            }
            break;
        }
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
        case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
        case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
        case 's': case 't': case 'u': case 'v': case 'w': case 'x':
        case 'y': case 'z': case 'A': case 'B': case 'C': case 'D': 
        case 'E': case 'F': case 'G': case 'H': case 'I': case 'J':
        case 'K': case 'L': case 'M': case 'N': case 'O': case 'P':
        case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V':
        case 'W': case 'X': case 'Y': case 'Z': case '_': {
            while (isalnum(*stream) || *stream == '_') {
                stream++;
            }
            token.name = str_intern_range(token.start, stream);
            token.kind = TOKEN_NAME;
            break;       
        }

       case '<': {
            token.kind = *stream;
            stream++;
            if (*stream == '<') {
                token.kind = TOKEN_LSHIFT;
                stream++;
                if (*stream == '=') {
                    token.kind = TOKEN_LSHIFT_ASSIGN;
                    stream++;
                }
            } else if (*stream == '=') {
                token.kind = TOKEN_LTEQ;
                stream++;
            }
            break;
        }

        case '>': {
            token.kind = *stream;
            stream++;
            if (*stream == '>') {
                token.kind = TOKEN_RSHIFT;
                stream++;
                if (*stream == '=') {
                    token.kind = TOKEN_RSHIFT_ASSIGN;
                    stream++;
                }
            } else if (*stream == '=') {
                token.kind = TOKEN_GTEQ;
                stream++;
            }
            break;
        }

#define CASE1(c, c1, k1) \
        case c: { \
            token.kind = *stream++; \
            if (*stream == c1) { \
                token.kind = k1; \
                stream++; \
            } \
            break; \
        }

#define CASE2(c, c1, k1, c2, k2) \
        case c: { \
            token.kind = *stream++; \
            if (*stream == c1) { \
                token.kind = k1; \
                stream++; \
            } \
            else if (*stream == c2) { \
                token.kind = k2; \
                stream++; \
            } \
            break; \
        }

        CASE1('^', '=', TOKEN_XOR_ASSIGN)
        CASE1(':', '=', TOKEN_COLON_ASSIGN)
        CASE1('*', '=', TOKEN_MUL_ASSIGN)
        CASE1('/', '=', TOKEN_DIV_ASSIGN)
        CASE1('%', '=', TOKEN_MOD_ASSIGN)
        CASE2('+', '+', TOKEN_INC, '=', TOKEN_ADD_ASSIGN)
        CASE2('-', '-', TOKEN_DEC, '=', TOKEN_SUB_ASSIGN)
        CASE2('&', '&', TOKEN_AND, '=', TOKEN_AND_ASSIGN)
        CASE2('|', '|', TOKEN_OR, '=', TOKEN_OR_ASSIGN)

        default:
            token.kind = *stream++;
            break;
    }
    token.end = stream;

#undef CASE1
#undef CASE2
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
        char buf[256];
        copy_token_kind_str(buf, sizeof(buf), kind);
        fatal("expected token %s: %s", 
              buf,
              token_kind_str(token.kind));
        return false;
    } else {
        return true;
    }
}

#define assert_token_kind(x) assert(match_token(x))
#define assert_token_float(x) assert(token.float_val == (x) && match_token(TOKEN_FLOAT))
#define assert_token_int(x) assert(token.int_val == (x) && match_token(TOKEN_INT))
#define assert_token_str(x) assert(strcmp(token.str_val, x) == 0 && match_token(TOKEN_STRING))
#define assert_token_eof() assert(token.kind == TOKEN_EOF)

void lex_test() {
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
        int val = token.int_val;
        next_token();
        return val;
    } else if (match_token('(')) {
        int val = parse_expr();
        expect_token(')');
        return val;
    } else {
        fatal("expected integer or (, got %s", 
              token_kind_str(token.kind));
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

void run_tests() {
    buf_test();
    lex_test();
    str_intern_test();
    /* parse_test(); */
    printf("all passed\n");
}


int main(int argc, char** argv) {
    run_tests();
    return 0;
}