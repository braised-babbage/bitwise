#pragma once

#include "ast.h"

Decl *parse_decl();
TypeSpec *parse_type();
Stmt *parse_stmt();
Expr *parse_expr();

void parse_test();