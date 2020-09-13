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

typedef struct StmtBlock {
    Stmt **stmts;
    size_t num_stmts;
} StmtBlock;

typedef enum TypeSpecKind {
    TYPESPEC_NONE,
    TYPESPEC_NAME,
    TYPESPEC_FUNC,
    TYPESPEC_ARRAY,
    TYPESPEC_PTR
} TypeSpecKind;

typedef struct FuncTypeSpec {
    TypeSpec **args;
    size_t num_args;
    TypeSpec *ret;
} FuncTypeSpec;

typedef struct PtrTypeSpec {
    TypeSpec *elem;
} PtrTypeSpec;

typedef struct ArrayTypeSpec {
    TypeSpec *elem;
    Expr *size;
} ArrayTypeSpec;

struct TypeSpec {
    TypeSpecKind kind;
    union {
        const char *name;
        FuncTypeSpec func;
        PtrTypeSpec ptr;
        ArrayTypeSpec array;
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

typedef struct FuncDecl {
    FuncParam *params;
    size_t num_params;
    TypeSpec *ret_type;
    StmtBlock block;
} FuncDecl;

typedef struct EnumItem {
    const char *name;
    Expr *expr;
} EnumItem;

typedef struct EnumDecl {
    EnumItem *items;
    size_t num_items;
} EnumDecl;

typedef struct AggregateItem {
    const char **names;
    size_t num_names;
    TypeSpec *type;
} AggregateItem;

typedef struct AggregateDecl {
    AggregateItem *items;
    size_t num_items;
} AggregateDecl;

typedef struct TypedefDecl {
    TypeSpec *type;
} TypedefDecl;

typedef struct VarDecl {
    TypeSpec *type;
    Expr *expr;
} VarDecl;

typedef struct ConstDecl {
    Expr *expr;
} ConstDecl;

struct Decl {
    DeclKind kind;
    const char *name;
    union {
        EnumDecl enum_decl;
        AggregateDecl aggregate;
        FuncDecl func;
        TypedefDecl typedef_decl;
        VarDecl var;
        ConstDecl const_decl;
    };
};

Decl *decl_new(DeclKind kind, const char *name);
Decl *decl_enum(const char *name, EnumItem *items, size_t num_items);
Decl *decl_aggregte(DeclKind kind, const char *name, AggregateItem *items, size_t num_items);
Decl *decl_var(const char *name, TypeSpec *type, Expr *expr);
Decl *decl_func(const char *name, FuncParam *params, size_t num_params, TypeSpec *ret_type, StmtBlock block);
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
    EXPR_SIZEOF,
} ExprKind;

typedef enum SizeofKind {
    SIZEOF_EXPR,
    SIZEOF_TYPE,
} SizeofKind;

typedef struct SizeofExpr {
    SizeofKind kind;
    union {
        Expr *expr;
        TypeSpec *type;

    };
} SizeofExpr;

typedef struct CompoundExpr {
    TypeSpec *type;
    Expr **args;
    size_t num_args;
} CompoundExpr;

typedef struct CastExpr {
    TypeSpec *type;
    Expr *expr;
} CastExpr;

typedef struct UnaryExpr {
    TokenKind op;
    Expr *expr;
} UnaryExpr;

typedef struct BinaryExpr {
    TokenKind op;
    Expr *left;
    Expr *right;
} BinaryExpr;

typedef struct TernaryExpr {
    Expr *cond;
    Expr *then_expr;
    Expr *else_expr;
} TernaryExpr;

typedef struct CallExpr {
    Expr *expr;
    Expr **args;
    size_t num_args;
} CallExpr;

typedef struct IndexExpr {
    Expr *expr;
    Expr *index;
} IndexExpr;

typedef struct FieldExpr {
    Expr *expr;
    const char *name;
} FieldExpr;

struct Expr {
    ExprKind kind;
    union {
        // literal/names
        uint64_t int_val;
        double float_val;
        const char *str_val;
        const char *name;
        
        // the rest
        SizeofExpr sizeof_expr;
        CompoundExpr compound;
        CastExpr cast;
        UnaryExpr unary;
        BinaryExpr binary;
        TernaryExpr ternary;
        CallExpr call;
        IndexExpr index;
        FieldExpr field;
    };
};

Expr *expr_new(ExprKind kind);
Expr *expr_int(uint64_t int_val);
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
    StmtBlock block;
} ElseIf;

typedef struct IfStmt {
    Expr *cond;
    StmtBlock then_block;
    ElseIf *elseifs;
    size_t num_elseifs;
    StmtBlock else_block;
} IfStmt;

typedef struct WhileStmt {
    Expr *cond;
    StmtBlock block;
} WhileStmt;

typedef struct ForStmt {
    Stmt* init;
    Expr *cond;
    Stmt* next;
    StmtBlock block;
} ForStmt;

typedef struct SwitchCase {
    Expr **exprs;
    size_t num_exprs;
    StmtBlock block;
    bool is_default;
} SwitchCase;

typedef struct SwitchStmt {
    Expr *expr;
    SwitchCase *cases;
    size_t num_cases;
} SwitchStmt;

typedef struct AssignStmt {
    TokenKind op;
    Expr *left;
    Expr *right;
} AssignStmt;

typedef struct InitStmt {
    const char *name;
    Expr *expr;
} InitStmt;

typedef struct ReturnStmt {
    Expr *expr;
} ReturnStmt;

struct Stmt {
    StmtKind kind;
    union {
        ReturnStmt return_stmt;
        StmtBlock block;
        IfStmt if_stmt;
        WhileStmt while_stmt;
        ForStmt for_stmt;
        SwitchStmt switch_stmt;
        AssignStmt assign;
        InitStmt init;
        Expr *expr;
    };
};

Stmt *stmt_new(StmtKind kind);
Stmt *stmt_return(Expr *expr);
Stmt *stmt_break();
Stmt *stmt_continue();
Stmt *stmt_block(StmtBlock block);
Stmt *stmt_if(Expr *cond, StmtBlock then_block, ElseIf *elseifs, size_t num_elseifs, StmtBlock else_block);
Stmt *stmt_while(Expr *cond, StmtBlock block);
Stmt *stmt_do_while(Expr *cond, StmtBlock block);
Stmt *stmt_for(Stmt* init, Expr *cond, Stmt* next, StmtBlock block);
Stmt *stmt_switch(Expr *expr, SwitchCase *cases, size_t num_cases);
Stmt *stmt_assign(TokenKind op, Expr *left, Expr *right);
Stmt *stmt_init(const char *name, Expr *expr);
Stmt *stmt_expr(Expr *expr);