#pragma once

#include "lexer.h"

typedef enum {
	A_AstBlock,
	A_AstExpr,

	A_AstIdent,
	A_AstBinop,
	A_AstParen,
} AstType;

typedef struct {
	AstType type;
	SourceSpan span;
} Ast;

typedef struct {
	Ast ast;
	uint32_t numStatements;
	Ast *statements[];
} AstBlock;

typedef struct {
	Ast ast;
	Ast *expr;
} AstExpr;

typedef struct {
	Ast ast;
	Token name;
} AstIdent;

typedef struct {
	Ast ast;
	Token op;
	Ast *left, *right;
} AstBinop;

typedef struct {
	Ast ast;
	Ast *expr;
} AstParen;

Ast *parse(const LexerInput *lexerInput);
void freeAst(Ast *ast);

void dumpAst(Ast *ast);
