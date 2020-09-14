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

bool is_keyword_name(const char *name) {
    return first_keyword <= name && name <= last_keyword;
}

const char *token_kind_names[] = {
    [TOKEN_EOF] = "EOF",
    [TOKEN_COLON] = ":",
    [TOKEN_LPAREN] = "(",
    [TOKEN_RPAREN] = ")",
    [TOKEN_LBRACE] = "{",
    [TOKEN_RBRACE] = "}",
    [TOKEN_LBRACKET] = "[",
    [TOKEN_RBRACKET] = "]",
    [TOKEN_COMMA] = ",",
    [TOKEN_DOT] = ".",
    [TOKEN_QUESTION] = "?",
    [TOKEN_SEMICOLON] = ";",
    [TOKEN_KEYWORD] = "keyword",
    [TOKEN_INT] = "int",
    [TOKEN_FLOAT] = "float",
    [TOKEN_STR] = "string",
    [TOKEN_NAME] = "name",
    [TOKEN_MUL] = "*",
    [TOKEN_DIV] = "/",
    [TOKEN_MOD] = "%",
    [TOKEN_BAND] = "&",
    [TOKEN_LSHIFT] = "<<",
    [TOKEN_RSHIFT] = ">>",
    [TOKEN_ADD] = "+",
    [TOKEN_SUB] = "-",
    [TOKEN_BOR] = "|",
    [TOKEN_XOR] = "^",
    [TOKEN_EQ] = "==",
    [TOKEN_NOTEQ] = "!=",
    [TOKEN_LT] = "<",
    [TOKEN_GT] = ">",
    [TOKEN_LTEQ] = "<=",
    [TOKEN_GTEQ] = ">=",
    [TOKEN_AND] = "&&",
    [TOKEN_OR] = "||",
    [TOKEN_ASSIGN] = "=",
    [TOKEN_ADD_ASSIGN] = "+=",
    [TOKEN_SUB_ASSIGN] = "-=",
    [TOKEN_OR_ASSIGN] = "|=",
    [TOKEN_AND_ASSIGN] = "&=",
    [TOKEN_XOR_ASSIGN] = "^=",
    [TOKEN_MUL_ASSIGN] = "*=",
    [TOKEN_DIV_ASSIGN] = "/=",
    [TOKEN_MOD_ASSIGN] = "%=",
    [TOKEN_LSHIFT_ASSIGN] = "<<=",
    [TOKEN_RSHIFT_ASSIGN] = ">>=",
    [TOKEN_INC] = "++",
    [TOKEN_DEC] = "--",
    [TOKEN_COLON_ASSIGN] = ":=",
};

const char *token_kind_name(TokenKind kind) {
    if (kind < sizeof(token_kind_names)/sizeof(*token_kind_names)) {
        return token_kind_names[kind];
    } else {
        return "<unknown>";
    }
}

Token token;
const char *stream;

const char *token_info() {
    if (token.kind == TOKEN_NAME || token.kind == TOKEN_KEYWORD) {
        return token.name;
    } else {
        return token_kind_name(token.kind);
    }
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
        }
    }
    uint64_t val = 0;
    for (;;) {
        uint64_t digit = char_to_digit[*(unsigned char*)stream];
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
    if (val == HUGE_VAL) {
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
        val = escape_to_char[*(unsigned char *)stream];
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
            val = escape_to_char[*(unsigned char *)stream];
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
    token.kind = TOKEN_STR;
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
            if (isdigit(stream[1])) {
                scan_float();
            } else {
                token.kind = TOKEN_DOT;
                stream++;
            }
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
            token.kind = is_keyword_name(token.name) ? TOKEN_KEYWORD : TOKEN_NAME;
            break;       
        }

       case '<': {
            token.kind = TOKEN_LT;
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
            token.kind = TOKEN_GT;
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

#define CASE1(c1, k1) \
        case c1: \
            token.kind = k1; \
            stream++; \
            break;

#define CASE2(c1, k1, c2, k2) \
        case c1: \
            token.kind = k1; \
            stream++; \
            if (*stream == c2) { \
                token.kind = k2; \
                stream++; \
            } \
            break;

#define CASE3(c1, k1, c2, k2, c3, k3) \
        case c1: \
            token.kind = k1; \
            stream++; \
            if (*stream == c2) { \
                token.kind = k2; \
                stream++; \
            } else if (*stream == c3) { \
                token.kind = k3; \
                stream++; \
            } \
            break;

        CASE1('\0', TOKEN_EOF)
        CASE1('(', TOKEN_LPAREN)
        CASE1(')', TOKEN_RPAREN)
        CASE1('{', TOKEN_LBRACE)
        CASE1('}', TOKEN_RBRACE)
        CASE1('[', TOKEN_LBRACKET)
        CASE1(']', TOKEN_RBRACKET)
        CASE1(',', TOKEN_COMMA)
        CASE1('?', TOKEN_QUESTION)
        CASE1(';', TOKEN_SEMICOLON)
        CASE2(':', TOKEN_COLON, '=', TOKEN_COLON_ASSIGN)
        CASE2('=', TOKEN_ASSIGN, '=', TOKEN_EQ)
        CASE2('^', TOKEN_XOR, '=', TOKEN_XOR_ASSIGN)
        CASE2('*', TOKEN_MUL, '=', TOKEN_MUL_ASSIGN)
        CASE2('/', TOKEN_DIV, '=', TOKEN_DIV_ASSIGN)
        CASE2('%', TOKEN_MOD, '=', TOKEN_MOD_ASSIGN)
        CASE3('+', TOKEN_ADD, '=', TOKEN_ADD_ASSIGN, '+', TOKEN_INC)
        CASE3('-', TOKEN_SUB, '=', TOKEN_SUB_ASSIGN, '-', TOKEN_DEC)
        CASE3('&', TOKEN_BAND, '=', TOKEN_AND_ASSIGN, '&', TOKEN_AND)
        CASE3('|', TOKEN_BOR, '=', TOKEN_OR_ASSIGN, '|', TOKEN_OR)

        default:
            syntax_error("Invalid '%c' token character, skipping", *stream);
            token.kind = *stream++;
            goto top;
    }
    token.end = stream;

#undef CASE1
#undef CASE2
#undef CASE3
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
    if (is_token(kind)) {
        next_token();
        return true;
    } else {
        fatal("expected token %s, got %s", token_kind_name(kind), token_info());
        return false;
    }
}

void keyword_test() {
    init_keywords();
    assert(is_keyword_name(first_keyword));
    assert(is_keyword_name(last_keyword));
    for (const char **it = keywords; it != buf_end(keywords); it++) {
        assert(is_keyword_name(*it));
    }
    assert(!is_keyword_name(str_intern("foo")));
}

void lex_test() {
    keyword_test();

#define assert_token_kind(x) assert(match_token(x))
#define assert_token_keyword(x) assert(is_keyword(x) && match_token(TOKEN_KEYWORD))
#define assert_token_name(x) assert(strcmp(token.name, x) == 0 && match_token(TOKEN_NAME))
#define assert_token_float(x) assert(token.float_val == (x) && match_token(TOKEN_FLOAT))
#define assert_token_int(x) assert(token.int_val == (x) && match_token(TOKEN_INT))
#define assert_token_str(x) assert(strcmp(token.str_val, x) == 0 && match_token(TOKEN_STR))
#define assert_token_eof() assert(token.kind == TOKEN_EOF)

    init_stream("0 1");
    assert_token_int(0);
    assert_token_int(1);
    assert_token_eof();
    
    init_stream("var x = 0");
    assert_token_keyword(var_keyword);
    assert_token_name("x");
    assert_token_kind(TOKEN_ASSIGN);
    assert_token_int(0);
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
    expect_token(TOKEN_ADD);
    expect_token(TOKEN_LPAREN);
    expect_token(TOKEN_RPAREN);
    assert(token.int_val == 1234);
    expect_token(TOKEN_INT);
    expect_token(TOKEN_ADD);
    assert(token.int_val == 4589);
    expect_token(TOKEN_INT);
    expect_token(TOKEN_SUB);
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
    assert_token_kind(TOKEN_ADD);
    assert_token_kind(TOKEN_INC);
    assert_token_kind(TOKEN_ADD_ASSIGN);
    assert_token_kind(TOKEN_SUB);
    assert_token_kind(TOKEN_DEC);
    assert_token_kind(TOKEN_SUB_ASSIGN);
    assert_token_kind(TOKEN_COLON);
    assert_token_kind(TOKEN_COLON_ASSIGN);
    assert_token_kind(TOKEN_LT);
    assert_token_kind(TOKEN_LSHIFT);
    assert_token_kind(TOKEN_LSHIFT_ASSIGN);
    assert_token_eof();
#undef assert_token_kind
#undef assert_token_keyword
#undef assert_token_name
#undef assert_token_float
#undef assert_token_int
#undef assert_token_str
#undef assert_token_eof
}