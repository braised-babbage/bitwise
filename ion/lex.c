#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include <assert.h>
#include <string.h>

#include "common.h"
#include "lex.h"


const char *typedef_keyword;
const char *enum_keyword;
const char *struct_keyword;
const char *union_keyword;
const char *var_keyword;
const char *const_keyword;
const char *func_keyword;
const char *sizeof_keyword;
const char *break_keyword;
const char *continue_keyword;
const char *return_keyword;
const char *if_keyword;
const char *else_keyword;
const char *while_keyword;
const char *do_keyword;
const char *for_keyword;
const char *switch_keyword;
const char *case_keyword;
const char *default_keyword;

const char *first_keyword;
const char *last_keyword;
const char **keywords;

#define KEYWORD(name) name##_keyword = str_intern(#name); buf_push(keywords, name##_keyword)

void init_keywords() {
    static bool inited;
    if (inited) {
        return;
    }
    char *arena_end = str_arena.end;
    KEYWORD(typedef);
    KEYWORD(enum);
    KEYWORD(struct);
    KEYWORD(union);
    KEYWORD(const);
    KEYWORD(var);
    KEYWORD(func);
    KEYWORD(sizeof);
    KEYWORD(break);
    KEYWORD(continue);
    KEYWORD(return);
    KEYWORD(if);
    KEYWORD(else);
    KEYWORD(while);
    KEYWORD(do);
    KEYWORD(for);
    KEYWORD(switch);
    KEYWORD(case);
    KEYWORD(default);
    assert(str_arena.end == arena_end);
    first_keyword = typedef_keyword;
    last_keyword = default_keyword;
    inited = true;
}

#undef KEYWORD

bool is_keyword_str(const char *str) {
    return first_keyword <= str && str <= last_keyword;
}

const char *token_kind_names[] = {
    [TOKEN_EOF] = "EOF",
    [TOKEN_INT] = "int",
    [TOKEN_FLOAT] = "float",
    [TOKEN_STRING] = "string",
    [TOKEN_NAME] = "name",
    [TOKEN_LSHIFT] = "<<",
    [TOKEN_RSHIFT] = ">>",
    [TOKEN_EQ] = "==",
    [TOKEN_NOTEQ] = "!=",
    [TOKEN_LTEQ] = "<=",
    [TOKEN_GTEQ] = ">=",
    [TOKEN_AND] = "&&",
    [TOKEN_OR] = "||",
    [TOKEN_INC] = "++",
    [TOKEN_DEC] = "--",
    [TOKEN_COLON_ASSIGN] = ":=",
    [TOKEN_ADD_ASSIGN] = "+=",
    [TOKEN_SUB_ASSIGN] = "-=",
    [TOKEN_OR_ASSIGN] = "|=",
    [TOKEN_LSHIFT_ASSIGN] = "<<=",
    [TOKEN_RSHIFT_ASSIGN] = ">>=",
    [TOKEN_AND_ASSIGN] = "&=",
    [TOKEN_XOR_ASSIGN] = "^=",
    [TOKEN_DIV_ASSIGN] = "/=",
    [TOKEN_MOD_ASSIGN] = "%=",
};

const char *token_kind_name(TokenKind kind) {
    if (kind < sizeof(token_kind_names)/sizeof(*token_kind_names)) {
        return token_kind_names[kind];
    } else {
        return NULL;
    }
}

size_t copy_token_kind_str(char *dest, size_t dest_size, TokenKind kind) {
    size_t n = 0;
    const char *name = token_kind_name(kind);
    if (name) {
        n = snprintf(dest, dest_size, "%s", name);
    } else if (kind < 128 && isprint(kind)) {
        n = snprintf(dest, dest_size, "%c", kind);
    } else {
        n = snprintf(dest, dest_size, "<ASCII %d>", kind);
    }
    return n;
}

const char *token_kind_str(TokenKind kind) {
    static char buf[256];
    size_t n = copy_token_kind_str(buf, sizeof(buf), kind);
    assert(n + 1 <= sizeof(buf));
    return buf;
}

Token token;
const char *stream;

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
            syntax_error("String literal cannot contain newline");
            break;
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
            token.kind = is_keyword_str(token.name) ? TOKEN_KEYWORD : TOKEN_NAME;
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

        CASE1('=', '=', TOKEN_EQ);
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

bool is_token_eof() {
    return token.kind == TOKEN_EOF;
}

bool is_token_name(const char *name) {
    return token.kind == TOKEN_NAME && token.name == name;
}

bool match_keyword(const char *name) {
    if (is_keyword(name)) {
        next_token();
        return true;
    } else {
        return false;
    }
}

bool is_keyword(const char *name) {
    return is_token(TOKEN_KEYWORD) && token.name == name;
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

void keyword_test() {
    init_keywords();
    assert(is_keyword_str(first_keyword));
    assert(is_keyword_str(last_keyword));
    for (const char **it = keywords; it != buf_end(keywords); it++) {
        assert(is_keyword_str(*it));
    }
    assert(!is_keyword_str(str_intern("foo")));
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