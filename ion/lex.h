#pragma once

#include <stdint.h>
#include <stdbool.h>

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

extern Token token;
extern const char *stream;
extern const char *token_kind_names[];

void init_stream(const char *str);
void next_token();
bool is_token(TokenKind kind);
bool is_token_name(const char *name);
bool match_token(TokenKind kind);
bool expect_token(TokenKind kind);

void lex_test();