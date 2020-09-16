#pragma once

#include <stdlib.h>

#include "common.h"
#include "lex.h"

typedef struct Expr Expr;
typedef struct Stmt Stmt;
typedef struct Decl Decl;
typedef struct TypeSpec TypeSpec;

void *ast_alloc(size_t size);
void *ast_dup(const void *src, size_t size);

typedef struct StmtList {
    Stmt **stmts;
    size_t num_stmts;
} StmtList;

typedef enum TypeSpecKind {
    TYPESPEC_NONE,
    TYPESPEC_NAME,
    TYPESPEC_FUNC,
    TYPESPEC_ARRAY,
    TYPESPEC_PTR
} TypeSpecKind;

struct TypeSpec {
    TypeSpecKind kind;
    union {
        const char *name;
        struct {
            TypeSpec **args;
            size_t num_args;
            TypeSpec *ret;
        } func;
        struct {
            TypeSpec *elem;
            Expr *size;
        } array;
        struct {
            TypeSpec *elem;
        } ptr;
    };
};

TypeSpec *typespec_new(TypeSpecKind kind);
TypeSpec *typespec_name(const char* name);
TypeSpec *typespec_ptr(TypeSpec *base);
TypeSpec *typespec_array(TypeSpec *base, Expr *size);
TypeSpec *typespec_func(TypeSpec **args, size_t num_args, TypeSpec *ret);

void print_typespec(TypeSpec *type); 

typedef enum DeclKind {
    DECL_NONE,
    DECL_ENUM,
    DECL_STRUCT,
    DECL_UNION,
    DECL_VAR,
    DECL_CONST,
    DECL_TYPEDEF,
    DECL_FUNC,
} DeclKind;

typedef struct FuncParam {
    const char *name;
    TypeSpec *type;
} FuncParam;

typedef struct EnumItem {
    const char *name;
    Expr *expr;
} EnumItem;

typedef struct AggregateItem {
    const char **names;
    size_t num_names;
    TypeSpec *type;
} AggregateItem;

struct Decl {
    DeclKind kind;
    const char *name;
    union {
        struct {
            EnumItem *items;
            size_t num_items;
        } enum_decl;
        struct {
            AggregateItem *items;
            size_t num_items;
        } aggregate;
        struct {
            FuncParam *params;
            size_t num_params;
            TypeSpec *ret_type;
            StmtList block;
        } func;
        struct {
            TypeSpec *type;
        } typedef_decl;
        struct {
            TypeSpec *type;
            Expr *expr;
        } var;
        struct {
            Expr *expr;
        } const_decl;
    };
};

Decl *decl_new(DeclKind kind, const char *name);
Decl *decl_enum(const char *name, EnumItem *items, size_t num_items);
Decl *decl_aggregte(DeclKind kind, const char *name, AggregateItem *items, size_t num_items);
Decl *decl_var(const char *name, TypeSpec *type, Expr *expr);
Decl *decl_func(const char *name, FuncParam *params, size_t num_params, TypeSpec *ret_type, StmtList block);
Decl *decl_const(const char *name, Expr *expr);
Decl *decl_typedef(const char *name, TypeSpec *type);

typedef enum ExprKind {
    EXPR_NONE,
    EXPR_INT,
    EXPR_FLOAT,
    EXPR_STR,
    EXPR_NAME,
    EXPR_CALL,
    EXPR_CAST,
    EXPR_COMPOUND,
    EXPR_INDEX,
    EXPR_FIELD,
    EXPR_UNARY,
    EXPR_BINARY,
    EXPR_TERNARY,
    EXPR_SIZEOF_EXPR,
    EXPR_SIZEOF_TYPE,
} ExprKind;

struct Expr {
    ExprKind kind;
    union {
        // literal/names
        int64_t int_val;
        double float_val;
        const char *str_val;
        const char *name;
        
        Expr *sizeof_expr;
        TypeSpec *sizeof_type;
        struct {
            TypeSpec *type;
            Expr **args;
            size_t num_args;            
        } compound;
        struct {
            TypeSpec *type;
            Expr *expr;            
        } cast;
        struct {
            TokenKind op;
            Expr *expr;
        } unary;
        struct {
            TokenKind op;
            Expr *left;
            Expr *right;
        } binary;
        struct {
            Expr *cond;
            Expr *then_expr;
            Expr *else_expr;
        } ternary;
        struct {
            Expr *expr;
            Expr **args;
            size_t num_args;            
        } call;
        struct {
            Expr *expr;
            Expr *index;
        } index;
        struct {
            Expr *expr;
            const char *name;
        } field;
    };
};

Expr *expr_new(ExprKind kind);
Expr *expr_int(int64_t int_val);
Expr *expr_float(double float_val);
Expr *expr_str(const char *str);
Expr *expr_name(const char *name);
Expr *expr_call(Expr *expr, Expr **args, size_t num_args);
Expr *expr_compound(TypeSpec *type, Expr **args, size_t num_args);

Expr *expr_index(Expr *expr, Expr *index);
Expr *expr_field(Expr *expr, const char *name);
Expr *expr_cast(TypeSpec *cast_type, Expr *cast);
Expr *expr_unary(TokenKind op, Expr *expr);
Expr *expr_binary(TokenKind op, Expr *left, Expr *right);
Expr *expr_ternary(Expr *cond, Expr *then_expr, Expr *else_expr);
Expr *expr_sizeof_expr(Expr *expr);
Expr *expr_sizeof_type(TypeSpec *type);

typedef enum StmtKind {
    STMT_NONE,
    STMT_DECL,
    STMT_RETURN,
    STMT_BREAK,
    STMT_CONTINUE,
    STMT_BLOCK,
    STMT_IF,
    STMT_WHILE,
    STMT_FOR,
    STMT_DO_WHILE,
    STMT_SWITCH,
    STMT_ASSIGN,
    STMT_INIT,
    STMT_EXPR,
} StmtKind;

typedef struct ElseIf {
    Expr *cond;
    StmtList block;
} ElseIf;

typedef struct SwitchCase {
    Expr **exprs;
    size_t num_exprs;
    StmtList block;
    bool is_default;
} SwitchCase;

struct Stmt {
    StmtKind kind;
    union {
        Decl *decl;
        Expr *expr;
                struct {
            Expr *cond;
            StmtList then_block;
            ElseIf *elseifs;
            size_t num_elseifs;
            StmtList else_block;            
        } if_stmt;
        struct {
            Expr *cond;
            StmtList block;
        } while_stmt;
        struct {
            Stmt *init;
            Expr *cond;
            Stmt *next;
            StmtList block;
        } for_stmt;
        struct {
            Expr *expr;
            SwitchCase *cases;
            size_t num_cases;            
        } switch_stmt;
        StmtList block;
        struct {
            TokenKind op;
            Expr *left;
            Expr *right;
        } assign;
        struct {
            const char *name;
            Expr *expr;
        } init;
    };
};

StmtList stmt_list(Stmt **stmts, size_t num_stmts);

Stmt *stmt_new(StmtKind kind);
Stmt *stmt_decl(Decl *decl);
Stmt *stmt_return(Expr *expr);
Stmt *stmt_break();
Stmt *stmt_continue();
Stmt *stmt_block(StmtList block);
Stmt *stmt_if(Expr *cond, StmtList then_block, ElseIf *elseifs, size_t num_elseifs, StmtList else_block);
Stmt *stmt_while(Expr *cond, StmtList block);
Stmt *stmt_do_while(Expr *cond, StmtList block);
Stmt *stmt_for(Stmt* init, Expr *cond, Stmt* next, StmtList block);
Stmt *stmt_switch(Expr *expr, SwitchCase *cases, size_t num_cases);
Stmt *stmt_assign(TokenKind op, Expr *left, Expr *right);
Stmt *stmt_init(const char *name, Expr *expr);
Stmt *stmt_expr(Expr *expr);