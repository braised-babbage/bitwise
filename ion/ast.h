#pragma once

#include <stdlib.h>

#include "common.h"
#include "lex.h"

typedef struct Expr Expr;
typedef struct Stmt Stmt;
typedef struct Decl Decl;
typedef struct TypeSpec TypeSpec;

typedef enum TypeSpecKind {
    TYPESPEC_NONE,
    TYPESPEC_NAME,
    TYPESPEC_FUNC,
    TYPESPEC_ARRAY,
    TYPESPEC_PTR
} TypeSpecKind;

typedef struct FuncTypeSpec {
    TypeSpec **arg_types;
    size_t num_args;
    TypeSpec *ret_type;
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

TypeSpec *typespec_alloc(TypeSpecKind kind);
TypeSpec *typespec_name(const char* name);
TypeSpec *typespec_ptr(TypeSpec *base);
TypeSpec *typespec_array(TypeSpec *base, Expr *size);
TypeSpec *typespec_func(FuncTypeSpec func);

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
} FuncDecl;

typedef struct EnumItem {
    const char *name;
    TypeSpec *type;
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
} ExprKind;

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
    Expr *if_true;
    Expr *if_false;
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

Expr *expr_alloc(ExprKind kind);
Expr *expr_int(uint64_t int_val);
Expr *expr_float(double float_val);
Expr *expr_str(const char *str);
Expr *expr_name(const char *name);
Expr *expr_call(Expr *expr, Expr **args, size_t num_args);
Expr *expr_index(Expr *expr, Expr *index);
Expr *expr_field(Expr *expr, const char *name);
Expr *expr_cast(TypeSpec *cast_type, Expr *cast);
Expr *expr_unary(TokenKind op, Expr *expr);
Expr *expr_binary(TokenKind op, Expr *left, Expr *right);
Expr *expr_ternary(Expr *cond, Expr *if_true, Expr *if_false);

void print_expr(Expr *expr);
void print_expr_line(Expr *expr);

typedef enum StmtKind {
    STMT_NONE,
    STMT_RETURN,
    STMT_BREAK,
    STMT_CONTINUE,
    STMT_BLOCK,
    STMT_IF,
    STMT_WHILE,
    STMT_FOR,
    STMT_DO,
    STMT_SWITCH,
    STMT_ASSIGN,
    STMT_AUTO_ASSIGN,
    STMT_EXPR,
} StmtKind;

typedef struct StmtBlock {
    Stmt **stmts;
    size_t num_stmts;
} StmtBlock;

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
    StmtBlock init;
    Expr *cond;
    StmtBlock next;
} ForStmt;

typedef struct SwitchCase {
    Expr **exprs;
    size_t num_exprs;
    StmtBlock *block;
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

typedef struct AutoAssignStmt {
    const char *name;
    Expr *init;
} AutoAssignStmt;

struct Stmt {
    StmtKind kind;
    union {
        IfStmt if_stmt;
        WhileStmt while_stmt;
        ForStmt for_stmt;
        SwitchStmt switch_stmt;
        AssignStmt assign;
        AutoAssignStmt autoassign;
    };
};

void ast_test();