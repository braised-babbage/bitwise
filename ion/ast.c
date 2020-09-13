#include "ast.h"
#include "common.h"
#include "lex.h"

#include <assert.h>
#include <stdint.h>
#include <stdio.h>


TypeSpec *typespec_alloc(TypeSpecKind kind) {
    TypeSpec *t = xcalloc(1, sizeof(TypeSpec));
    t->kind = kind;
    return t;
}

TypeSpec *typespec_name(const char* name) {
    TypeSpec *t = typespec_alloc(TYPESPEC_NAME);
    t->name = name;
    return t;
}

TypeSpec *typespec_ptr(TypeSpec *elem) {
    TypeSpec *t = typespec_alloc(TYPESPEC_PTR);
    t->ptr.elem = elem;
    return t;
}

TypeSpec *typespec_array(TypeSpec *elem, Expr *size) {
    TypeSpec *t = typespec_alloc(TYPESPEC_ARRAY);
    t->array.elem = elem;
    t->array.size = size;
    return t;
}

TypeSpec *typespec_func(FuncTypeSpec func) {
    TypeSpec *t = typespec_alloc(TYPESPEC_FUNC);
    t->func = func;
    return t;
}


Expr *expr_alloc(ExprKind kind) {
    Expr *e = xcalloc(1, sizeof(Expr));
    e->kind = kind;
    return e;
}

Expr *expr_int(uint64_t int_val) {
    Expr *e = expr_alloc(EXPR_INT);
    e->int_val = int_val;
    return e;
}

Expr *expr_float(double float_val) {
    Expr *e = expr_alloc(EXPR_FLOAT);
    e->float_val = float_val;
    return e;
}

Expr *expr_str(const char *str) {
    Expr *e = expr_alloc(EXPR_STR);
    e->str_val = str;
    return e;
}

Expr *expr_name(const char *name) {
    Expr *e = expr_alloc(EXPR_NAME);
    e->name = name;
    return e;
}

Expr *expr_call(Expr *expr, Expr **args, size_t num_args) {
    Expr *e = expr_alloc(EXPR_CALL);
    e->call.expr = expr;
    e->call.args = args;
    e->call.num_args = num_args;
    return e;
}

Expr *expr_index(Expr *expr, Expr *index) {
    Expr *e = expr_alloc(EXPR_INDEX);
    e->index.expr = expr;
    e->index.index = index;
    return e;
}

Expr *expr_field(Expr *expr, const char *name) {
    Expr *e = expr_alloc(EXPR_FIELD);
    e->field.expr = expr;
    e->field.name = name;
    return e;
}

Expr *expr_cast(TypeSpec *cast_type, Expr *cast) {
    Expr *e = expr_alloc(EXPR_CAST);
    e->cast.type = cast_type;
    e->cast.expr = cast;
    return e;
}

Expr *expr_compound(TypeSpec *type, Expr **args, size_t num_args) {
    Expr *e = expr_alloc(EXPR_COMPOUND);
    e->compound.type = type;
    e->compound.args = args;
    e->compound.num_args = num_args;
    return e;
}
 
Expr *expr_unary(TokenKind op, Expr *expr) {
    Expr *e = expr_alloc(EXPR_UNARY);
    e->unary.op = op;
    e->unary.expr = expr;
    return e;
}

Expr *expr_binary(TokenKind op, Expr *left, Expr *right) {
    Expr *e = expr_alloc(EXPR_BINARY);
    e->binary.op = op;
    e->binary.left = left;
    e->binary.right = right;
    return e;
}

Expr *expr_ternary(Expr *cond, Expr *if_true, Expr *if_false) {
    Expr *e = expr_alloc(EXPR_TERNARY);
    e->ternary.cond = cond;
    e->ternary.if_true = if_true;
    e->ternary.if_false = if_false;
    return e;
}

void print_typespec(TypeSpec *type) {
    switch (type->kind) {
    case TYPESPEC_NAME:
        printf("%s", type->name);
        break;
    case TYPESPEC_FUNC:
        printf("(func (");
        for (TypeSpec **it = type->func.arg_types; it != type->func.arg_types + type->func.num_args; it++) {
            printf(" ");
            print_typespec(*it);
        }
        printf(") ");
        print_typespec(type->func.ret_type);
        printf(")");
        break;
    case TYPESPEC_ARRAY:
        printf("(arr ");
        print_typespec(type->array.elem);
        printf(" ");
        print_expr(type->array.size);
        printf(")");
        break;
    case TYPESPEC_PTR:
    printf("(ptr ");
    print_typespec(type->ptr.elem);
    printf(")");
        break;
    default:
        assert(0);
        break;
    }
}

void print_expr(Expr *expr) {
    switch (expr->kind) {
    case EXPR_NONE:
        assert(0);
        break;
    case EXPR_INT:
        printf("%llu", expr->int_val);
        break;
    case EXPR_FLOAT:
        printf("%f", expr->float_val);
        break;
    case EXPR_STR:
        printf("\"%s\"", expr->str_val);
        break;
    case EXPR_NAME:
        printf("%s", expr->name);
        break;
    case EXPR_CALL:
        printf("(");
        print_expr(expr->call.expr);
        for (Expr **it = expr->call.args; it != expr->call.args + expr->call.num_args; it++) {
            printf(" ");
            print_expr(*it);
        }
        printf(")");
        break;
    case EXPR_CAST:
        printf("(cast ");
        print_typespec(expr->cast.type);
        printf(" ");
        print_expr(expr->cast.expr);
        printf(")");
        break;
    case EXPR_COMPOUND:
        printf("(compound ");
        if (expr->compound.type) {
            print_typespec(expr->compound.type);
        } else {
            printf("nil");
        }
        for (Expr **it = expr->compound.args; it != expr->compound.args + expr->compound.num_args; it++) {
            printf(" ");
            print_expr(*it);
        }
        printf(")");
        break;
    case EXPR_INDEX:
        printf("(index ");
        print_expr(expr->index.expr);
        printf(" ");
        print_expr(expr->index.index);
        printf(")");
        break;
    case EXPR_UNARY:
        printf("(");
        printf("%c", expr->unary.op);
        printf(" ");
        print_expr(expr->unary.expr);
        printf(")");
        break;
    case EXPR_BINARY:
        printf("(");
        printf("%c", expr->binary.op);
        printf(" ");
        print_expr(expr->binary.left);
        printf(" ");
        print_expr(expr->binary.right);
        printf(")");
        break;
    case EXPR_TERNARY:
        printf("(ternary ");
        print_expr(expr->ternary.cond);
        printf(" ");
        print_expr(expr->ternary.if_true);
        printf(" ");
        print_expr(expr->ternary.if_false);
        printf(")");
        break;
    case EXPR_FIELD:
        printf("(field ");
        print_expr(expr->field.expr);
        printf(" %s)", expr->field.name);
        break;
    default:
        assert(0);
        break;
    }
}

void print_expr_line(Expr *expr) {
    print_expr(expr);
    printf("\n");
}

void expr_test() {
    Expr **fact_args = NULL;
    buf_push(fact_args, expr_int(42));

    print_expr_line(expr_binary('+', expr_int(1), expr_int(42)));
    print_expr_line(expr_unary('-', expr_float(3.14)));
    print_expr_line(
        expr_ternary(expr_name("flag"), expr_str("true"), expr_str("false"))
    );
    print_expr_line(expr_field(expr_name("person"), "name"));
    print_expr_line(expr_call(expr_name("fact"), fact_args,  buf_len(fact_args)));
    print_expr_line(
        expr_index(expr_field(expr_name("person"), "siblings"), expr_int(3))
    );
    print_expr_line(
        expr_cast(typespec_ptr(typespec_name("int_ptr")), expr_name("void_ptr"))
    );
    print_expr_line(
        expr_compound(typespec_name("Vector"), (Expr*[]){expr_int(1), expr_int(2)}, 2)
    );
}


void ast_test() {
    expr_test();
}