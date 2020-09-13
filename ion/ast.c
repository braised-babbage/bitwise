#include "ast.h"
#include "common.h"
#include "lex.h"

#include <assert.h>
#include <stdint.h>
#include <stdio.h>


int indent;

void print_newline() {
    printf("\n%.*s", 2*indent, "                                                                      ");
}


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


Decl *decl_alloc(DeclKind kind, const char *name) {
    Decl *d = xcalloc(1, sizeof(Decl));
    d->kind = kind;
    d->name = name;
    return d;
}

Decl *decl_enum(const char *name, EnumItem *items, size_t num_items) {
    Decl *d = decl_alloc(DECL_ENUM, name);
    d->enum_decl.items = items;
    d->enum_decl.num_items = num_items;
    return d;
}

Decl *decl_struct(const char *name, AggregateItem *items, size_t num_items) {
    Decl *d = decl_alloc(DECL_STRUCT, name);
    d->aggregate.items = items;
    d->aggregate.num_items = num_items;
    return d;
}

Decl *decl_union(const char *name, AggregateItem *items, size_t num_items) {
    Decl *d = decl_alloc(DECL_UNION, name);
    d->aggregate.items = items;
    d->aggregate.num_items = num_items;
    return d;
}

Decl *decl_var(const char *name, TypeSpec *type, Expr *expr) {
    Decl *d = decl_alloc(DECL_VAR, name);
    d->var.type = type;
    d->var.expr = expr;
    return d;
}

Decl *decl_func(const char *name, FuncParam *params, size_t num_params, TypeSpec *ret_type, StmtBlock block) {
    Decl *d = decl_alloc(DECL_FUNC, name);
    d->func.params = params;
    d->func.num_params = num_params;
    d->func.ret_type = ret_type;
    d->func.block = block;
    return d;
}

Decl *decl_const(const char *name, Expr *expr) {
    Decl *d = decl_alloc(DECL_CONST, name);
    d->const_decl.expr = expr;
    return d;
}

Decl *decl_typedef(const char *name, TypeSpec *type) {
    Decl *d = decl_alloc(DECL_TYPEDEF, name);
    d->typedef_decl.type = type;
    return d;
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

void print_aggregate_decl(Decl *decl) {
    Decl *d = decl;
    for (AggregateItem *it = d->aggregate.items; it != d->aggregate.items + d->aggregate.num_items; it++) {
        print_newline();
        printf("(");
        print_typespec(it->type);
        for (const char **name = it->names; name != it->names + it->num_names; name++) {
            printf(" %s", *name);
        }
        printf(")");
    }
}

void print_decl(Decl *decl) {
    Decl *d = decl;
    switch (d->kind) {
    case DECL_ENUM:
        printf("(enum %s", d->name);
        indent++;
        for (EnumItem *it = d->enum_decl.items; it != d->enum_decl.items + d->enum_decl.num_items; it++) {
            print_newline();
            printf("(%s ", it->name);
            if (it->expr) {
                print_expr(it->expr);
            } else {
                printf("nil");
            }
            printf(")");
        }
        indent--;
        printf(")");
        break;
    case DECL_STRUCT:
        printf("(struct %s", d->name);
        indent++;
        print_aggregate_decl(d);
        indent--;
        printf(")");
        break;
    case DECL_UNION:
        printf("(union %s", d->name);
        indent++;
        print_aggregate_decl(d);
        indent--;
        printf(")");
        break;
    case DECL_VAR:
        printf("(var %s ", d->name);
        print_typespec(d->var.type);
        printf(" ");
        print_expr(d->var.expr);
        printf(")");
        break;
    case DECL_CONST:
        printf("(const %s ", d->name);
        print_expr(d->var.expr);
        printf(")");
        break;
    case DECL_TYPEDEF:
        printf("(typedef %s ", d->name);
        print_typespec(d->typedef_decl.type);
        printf(")");
        break;
    case DECL_FUNC:
        printf("(func %s ", d->name);
        printf("(");
        for (FuncParam *it = d->func.params; it != d->func.params + d->func.num_params; it++) {
            printf(" %s", it->name);
            print_typespec(it->type);
        }
        printf(" ) ");
        print_typespec(d->func.ret_type);
        indent++;
        print_newline();
        print_stmt_block(d->func.block, true);
        indent--;
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


Stmt *stmt_alloc(StmtKind kind) {
    Stmt *s = xcalloc(1, sizeof(Stmt));
    s->kind = kind;
    return s;
}

Stmt *stmt_return(Expr *expr) {
    Stmt *s = stmt_alloc(STMT_RETURN);
    s->return_stmt.expr = expr;
    return s;
}

Stmt *stmt_break() {
    return stmt_alloc(STMT_BREAK);
}

Stmt *stmt_continue() {
    return stmt_alloc(STMT_CONTINUE);
}

Stmt *stmt_block(StmtBlock block) {
    Stmt *s = stmt_alloc(STMT_BLOCK);
    s->block = block;
    return s;
}

Stmt *stmt_if(Expr *cond, StmtBlock then_block, ElseIf *elseifs, size_t num_elseifs, StmtBlock else_block) {
    Stmt *s = stmt_alloc(STMT_IF);
    s->if_stmt.cond = cond;
    s->if_stmt.then_block = then_block;
    s->if_stmt.elseifs = elseifs;
    s->if_stmt.num_elseifs = num_elseifs;
    s->if_stmt.else_block = else_block;
    return s;
}

Stmt *stmt_while(Expr *cond, StmtBlock block) {
    Stmt *s = stmt_alloc(STMT_WHILE);
    s->while_stmt.cond = cond;
    s->while_stmt.block = block;
    return s;
}

Stmt *stmt_do_while(Expr *cond, StmtBlock block) {
    Stmt *s = stmt_alloc(STMT_DO_WHILE);
    s->while_stmt.cond = cond;
    s->while_stmt.block = block;
    return s;
}

Stmt *stmt_for(StmtBlock init, Expr *cond, StmtBlock next, StmtBlock block) {
    Stmt *s = stmt_alloc(STMT_FOR);
    s->for_stmt.init = init;
    s->for_stmt.cond = cond;
    s->for_stmt.next = next;
    s->for_stmt.block = block;
    return s;
}

Stmt *stmt_switch(Expr *expr, SwitchCase *cases, size_t num_cases) {
    Stmt *s = stmt_alloc(STMT_SWITCH);
    s->switch_stmt.expr = expr;
    s->switch_stmt.cases = cases;
    s->switch_stmt.num_cases = num_cases;
    return s;
}

Stmt *stmt_assign(TokenKind op, Expr *left, Expr *right) {
    Stmt *s = stmt_alloc(STMT_ASSIGN);
    s->assign.op = op;
    s->assign.left = left;
    s->assign.right = right;
    return s;
}

Stmt *stmt_init(const char *name, Expr *expr) {
    Stmt *s = stmt_alloc(STMT_INIT);
    s->init.name = name;
    s->init.expr = expr;
    return s;
}

Stmt *stmt_expr(Expr *expr) {
    Stmt *s = stmt_alloc(STMT_EXPR);
    s->expr = expr;
    return s;
}

void print_stmt_block(StmtBlock block, bool newlines) {
    assert(block.num_stmts != 0);
    printf("(block");
    indent++;
    for (Stmt **it = block.stmts; it != block.stmts + block.num_stmts; it++) {
        if (newlines) {
            print_newline();
        } else {
            printf(" ");
        }
        print_stmt(*it);
    }
    indent--;
    printf(")");
}

void print_stmt(Stmt *stmt) {
    Stmt *s = stmt;
    switch (s->kind) {
    case STMT_RETURN:
        printf("(return ");
        print_expr(s->return_stmt.expr);
        printf(")");
        break;
    case STMT_BREAK:
        printf("(break)");
        break;
    case STMT_CONTINUE:
        printf("(continue)");
        break;
    case STMT_BLOCK:
        print_stmt_block(s->block, true);
        break;
    case STMT_IF:
        printf("(if ");
        print_expr(s->if_stmt.cond);
        indent++;
        print_newline();
        print_stmt_block(s->if_stmt.then_block, true);
        for (ElseIf *it = s->if_stmt.elseifs; it != s->if_stmt.elseifs + s->if_stmt.num_elseifs; it++) {
            print_newline();
            printf("elseif ");
            print_expr(it->cond);
            print_newline();
            print_stmt_block(it->block, true);
        }
        if (s->if_stmt.else_block.num_stmts != 0) {
            print_newline();
            printf("else ");
            print_newline();
            print_stmt_block(s->if_stmt.else_block, true);
        }
        printf(")");
        indent--;
        break;
    case STMT_WHILE:
        printf("(while ");
        print_expr(s->while_stmt.cond);
        indent++;
        print_newline();
        print_stmt_block(s->while_stmt.block, true);
        indent--;
        printf(")");
        break;
    case STMT_DO_WHILE:
        printf("(do-while ");
        print_expr(s->while_stmt.cond);
        indent++;
        print_newline();
        print_stmt_block(s->while_stmt.block, true);
        indent--;
        printf(")");
        break;
    case STMT_FOR:
        printf("(for ");
        print_stmt_block(s->for_stmt.init, false);
        print_expr(s->for_stmt.cond);
        print_stmt_block(s->for_stmt.next, false);
        indent++;
        print_newline();
        print_stmt_block(s->for_stmt.block, true);
        indent--;
        break;
    case STMT_SWITCH:
        printf("(switch ");
        print_expr(s->switch_stmt.expr);
        indent++;
        for (SwitchCase *it = s->switch_stmt.cases; it != s->switch_stmt.cases + s->switch_stmt.num_cases; it++) {
            print_newline();
            printf("(case (");
            if (it->is_default) {
                printf("default");
            } else {
                printf("nil");
            }
            for (Expr **expr = it->exprs; expr != it->exprs + it->num_exprs; expr++) {
                printf(" ");
                print_expr(*expr);
            }
            printf(") ");
            indent++;
            print_newline();
            print_stmt_block(it->block, true);
            indent--;
        }
        printf(")");
        indent--;
        break;
    case STMT_ASSIGN:
        printf("(%s ", token_kind_names[s->assign.op]);
        print_expr(s->assign.left);
        printf(" ");
        print_expr(s->assign.right);
        printf(")");
        break;
    case STMT_INIT:
        printf("(:= %s ", s->init.name);
        print_expr(s->init.expr);
        printf(")");
        break;
    case STMT_EXPR:
        print_expr(s->expr);
        break;
    default:
        assert(0);
        break;
    }
}

void print_stmt_line(Stmt *stmt) {
    print_stmt(stmt);
    print_newline();
}

void stmt_test() {
    print_stmt_line(stmt_return(expr_int(42)));
    print_stmt_line(stmt_break());
    print_stmt_line(stmt_continue());
    print_stmt_line(
        stmt_block(
            (StmtBlock){
                (Stmt*[]){
                    stmt_break(),
                    stmt_continue()
                },
                2,
            }
        )
    );
    print_stmt_line(stmt_expr(expr_call(expr_name("print"), (Expr*[]){expr_int(1), expr_int(2)}, 2)));
    print_stmt_line(stmt_init("x", expr_int(42)));
    print_stmt_line(
        stmt_if(
            expr_name("flag1"),
            (StmtBlock){
                (Stmt*[]){
                    stmt_return(expr_int(1))
                },
                1,
            },
            (ElseIf[]){
                expr_name("flag2"),
                (StmtBlock){
                    (Stmt*[]){
                        stmt_return(expr_int(2))
                    },
                    1,
                }
            },
            1,
            (StmtBlock){
                (Stmt*[]){
                    stmt_return(expr_int(3))
                },
                1,
            }
        )
    );
    print_stmt_line(
        stmt_while(
            expr_name("running"),
            (StmtBlock){
                (Stmt*[]){
                    stmt_assign(TOKEN_ADD_ASSIGN, expr_name("i"), expr_int(16)),
                },
                1,
            }
        )
    );
}

void ast_test() {
    expr_test();
    stmt_test();
}