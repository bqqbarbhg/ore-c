#pragma once

#include "lexer.h"

typedef enum {
	A_AstDef,

	A_AstBlock,
	A_AstExpr,
	A_AstIf,
	A_AstReturn,

	A_AstIdent,
	A_AstNumber,
	A_AstUnop,
	A_AstBinop,
	A_AstParen,
} AstType;

typedef struct Ast_s {
	AstType type;
	SourceSpan span;
} Ast;

typedef struct {
	Token name;
	Ast *type;
} DeclAst;

typedef struct AstDef_s {
	Ast ast;
	struct AstBlock_s *body;
	Ast *returnType;
	uint32_t numParams;
	DeclAst params[];
} AstDef;

typedef struct AstBlock_s {
	Ast ast;
	uint32_t numStatements;
	Ast *statements[];
} AstBlock;

typedef struct AstExpr_s {
	Ast ast;
	Ast *expr;
} AstExpr;

typedef struct {
	Ast *cond, *body;
} IfBranch;

typedef struct AstIf_s {
	Ast ast;
	Ast *elseBranch;
	uint32_t numBranches;
	IfBranch branches[];
} AstIf;

typedef struct AstReturn_s {
	Ast ast;
	Ast *expr;
} AstReturn;

typedef struct AstIdent_s {
	Ast ast;
	Token name;
} AstIdent;

typedef struct AstNumber_s {
	Ast ast;
	Token value;
} AstNumber;

typedef struct AstUnop_s {
	Ast ast;
	Token op;
	Ast *expr;
} AstUnop;

typedef struct AstBinop_s {
	Ast ast;
	Token op;
	Ast *left, *right;
} AstBinop;

typedef struct AstParen_s {
	Ast ast;
	Ast *expr;
} AstParen;

Ast *parse(const LexerInput *lexerInput);
void freeAst(Ast *ast);

void dumpAst(Ast *ast);
