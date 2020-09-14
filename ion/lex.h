#pragma once

#include <stdint.h>
#include <stdbool.h>

extern const char *typedef_keyword;
extern const char *enum_keyword;
extern const char *struct_keyword;
extern const char *union_keyword;
extern const char *var_keyword;
extern const char *const_keyword;
extern const char *func_keyword;
extern const char *sizeof_keyword;
extern const char *break_keyword;
extern const char *continue_keyword;
extern const char *return_keyword;
extern const char *if_keyword;
extern const char *else_keyword;
extern const char *while_keyword;
extern const char *do_keyword;
extern const char *for_keyword;
extern const char *switch_keyword;
extern const char *case_keyword;
extern const char *default_keyword;
 
extern const char *first_keyword;
extern const char *last_keyword;
extern const char **keywords;

void init_keywords();

typedef enum TokenKind {
    TOKEN_EOF,
    TOKEN_COLON,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LBRACE,
    TOKEN_RBRACE,
    TOKEN_LBRACKET,
    TOKEN_RBRACKET,
    TOKEN_COMMA,
    TOKEN_DOT,
    TOKEN_QUESTION,
    TOKEN_SEMICOLON,
    TOKEN_KEYWORD,
    TOKEN_INT,
    TOKEN_FLOAT,
    TOKEN_STR,
    TOKEN_NAME,
    // Multiplicative precedence
    TOKEN_MUL,
    TOKEN_FIRST_MUL = TOKEN_MUL,
    TOKEN_DIV,
    TOKEN_MOD,
    TOKEN_AND,
    TOKEN_LSHIFT,
    TOKEN_RSHIFT,
    TOKEN_LAST_MUL = TOKEN_RSHIFT,
    // Additive precedence
    TOKEN_ADD,
    TOKEN_FIRST_ADD = TOKEN_ADD,
    TOKEN_SUB,
    TOKEN_XOR,
    TOKEN_OR,
    TOKEN_LAST_ADD = TOKEN_OR,
    // Comparative precedence
    TOKEN_EQ,
    TOKEN_FIRST_CMP = TOKEN_EQ,
    TOKEN_NOTEQ,
    TOKEN_LT,
    TOKEN_GT,
    TOKEN_LTEQ,
    TOKEN_GTEQ,
    TOKEN_LAST_CMP = TOKEN_GTEQ,
    TOKEN_AND_AND,
    TOKEN_OR_OR,
    // Assignment operators
    TOKEN_ASSIGN,
    TOKEN_FIRST_ASSIGN = TOKEN_ASSIGN,
    TOKEN_ADD_ASSIGN,
    TOKEN_SUB_ASSIGN,
    TOKEN_OR_ASSIGN,
    TOKEN_AND_ASSIGN,
    TOKEN_XOR_ASSIGN,
    TOKEN_LSHIFT_ASSIGN,
    TOKEN_RSHIFT_ASSIGN,
    TOKEN_MUL_ASSIGN,
    TOKEN_DIV_ASSIGN,
    TOKEN_MOD_ASSIGN,
    TOKEN_LAST_ASSIGN = TOKEN_MOD_ASSIGN,
    TOKEN_INC,
    TOKEN_DEC,
    TOKEN_COLON_ASSIGN,
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

const char *token_kind_name(TokenKind kind);
const char *token_info();

void init_stream(const char *str);
void next_token();
bool is_token(TokenKind kind);
bool is_token_eof();
bool is_token_name(const char *name);
bool is_keyword(const char *name);
bool match_keyword(const char *name);
bool match_token(TokenKind kind);
bool expect_token(TokenKind kind);

void lex_test();
void keyword_test();