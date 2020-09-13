#include "common.h"
#include "ast.h"
#include "lex.h"
#include "print.h"

#include <assert.h>
#include <stdio.h>

int indent;

void print_newline() {
    printf("\n%.*s", 2*indent, "                                                                      ");
}

void print_typespec(TypeSpec *type) {
    switch (type->kind) {
    case TYPESPEC_NAME:
        printf("%s", type->name);
        break;
    case TYPESPEC_FUNC:
        printf("(func (");
        for (TypeSpec **it = type->func.args; it != type->func.args + type->func.num_args; it++) {
            printf(" ");
            print_typespec(*it);
        }
        printf(") ");
        print_typespec(type->func.ret);
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
        if (d->var.type) {
            print_typespec(d->var.type);
        } else {
            printf("nil");
        }
        printf(" ");
        print_expr(d->var.expr);
        printf(")");
        break;
    case DECL_CONST:
        printf("(const %s ", d->name);
        print_expr(d->const_decl.expr);
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
            printf(" %s ", it->name);
            print_typespec(it->type);
        }
        printf(" ) ");
        if (d->func.ret_type) {
            print_typespec(d->func.ret_type);
        } else {
            printf("nil");
        }
        indent++;
        print_newline();
        print_stmt_block(d->func.block);
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
    case EXPR_SIZEOF:
        printf("(sizeof ");
        if (expr->sizeof_expr.kind == SIZEOF_EXPR) {
            print_expr(expr->sizeof_expr.expr);
        } else {
            assert(expr->sizeof_expr.kind == SIZEOF_TYPE);
            print_typespec(expr->sizeof_expr.type);
        }
        printf(")");
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
        printf("%s", token_kind_str(expr->unary.op));
        printf(" ");
        print_expr(expr->unary.expr);
        printf(")");
        break;
    case EXPR_BINARY:
        printf("(");
        printf("%s", token_kind_str(expr->binary.op));
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
        print_expr(expr->ternary.then_expr);
        printf(" ");
        print_expr(expr->ternary.else_expr);
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

void print_stmt_block(StmtBlock block) {
    printf("(block");
    indent++;
    for (Stmt **it = block.stmts; it != block.stmts + block.num_stmts; it++) {
        print_newline();
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
        print_stmt_block(s->block);
        break;
    case STMT_IF:
        printf("(if ");
        print_expr(s->if_stmt.cond);
        indent++;
        print_newline();
        print_stmt_block(s->if_stmt.then_block);
        for (ElseIf *it = s->if_stmt.elseifs; it != s->if_stmt.elseifs + s->if_stmt.num_elseifs; it++) {
            print_newline();
            printf("elseif ");
            print_expr(it->cond);
            print_newline();
            print_stmt_block(it->block);
        }
        if (s->if_stmt.else_block.num_stmts != 0) {
            print_newline();
            printf("else ");
            print_newline();
            print_stmt_block(s->if_stmt.else_block);
        }
        printf(")");
        indent--;
        break;
    case STMT_WHILE:
        printf("(while ");
        print_expr(s->while_stmt.cond);
        indent++;
        print_newline();
        print_stmt_block(s->while_stmt.block);
        indent--;
        printf(")");
        break;
    case STMT_DO_WHILE:
        printf("(do-while ");
        print_expr(s->while_stmt.cond);
        indent++;
        print_newline();
        print_stmt_block(s->while_stmt.block);
        indent--;
        printf(")");
        break;
    case STMT_FOR:
        printf("(for ");
        print_stmt(s->for_stmt.init);
        print_expr(s->for_stmt.cond);
        print_stmt(s->for_stmt.next);
        indent++;
        print_newline();
        print_stmt_block(s->for_stmt.block);
        indent--;
        printf(")");
        break;
    case STMT_SWITCH:
        printf("(switch ");
        print_expr(s->switch_stmt.expr);
        indent++;
        for (SwitchCase *it = s->switch_stmt.cases; it != s->switch_stmt.cases + s->switch_stmt.num_cases; it++) {
            print_newline();
            printf("(case (%s", it->is_default ? " default" : "");
            for (Expr **expr = it->exprs; expr != it->exprs + it->num_exprs; expr++) {
                printf(" ");
                print_expr(*expr);
            }
            printf(" ) ");
            indent++;
            print_newline();
            print_stmt_block(it->block);
            indent--;
        }
        indent--;
        printf(")");
        break;
    case STMT_ASSIGN:
        printf("(%s ", token_kind_names[s->assign.op]);
        print_expr(s->assign.left);
        if (s->assign.right) {
            printf(" ");
            print_expr(s->assign.right);
        }
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
    Expr *e = expr_compound(typespec_name("Vector"), (Expr*[]){expr_int(1), expr_int(2)}, 2);
    print_expr_line(
        e
    );
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

void print_test() {
    expr_test();
    stmt_test();
}