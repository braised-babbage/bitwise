#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include <assert.h>

#include "common.h"
#include "lex.h"

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