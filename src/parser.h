#pragma once

#include "lexer.h"

typedef enum {
	A_AstToplevel,

	A_AstDef,

	A_AstBlock,
	A_AstExpr,
	A_AstIf,
	A_AstWhile,
	A_AstVar,
	A_AstReturn,

	A_AstIdent,
	A_AstNumber,
	A_AstUnop,
	A_AstBinop,
	A_AstCall,
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
	Token name;
	struct AstBlock_s *body;
	Ast *returnType;
	uint32_t numParams;
	DeclAst params[];
} AstDef;

typedef struct AstToplevel_s {
	Ast ast;
	void *allocation;
	uint32_t numToplevels;
	Ast *toplevels[];
} AstToplevel;

typedef struct AstBlock_s {
	Ast ast;
	int toplevel;
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

typedef struct AstWhile_s {
	Ast ast;
	Ast *cond;
	Ast *body;
} AstWhile;

typedef struct AstVar_s {
	Ast ast;
	Token name;
	Ast *type;
	Ast *init;
} AstVar;

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

typedef struct AstCall_s {
	Ast ast;
	Ast *func;
	uint32_t numArgs;
	Ast *args[];
} AstCall;

typedef struct AstParen_s {
	Ast ast;
	Ast *expr;
} AstParen;

Ast *parse(const LexerInput *lexerInput);
void freeAst(Ast *ast);

const char *getAstTypeName(AstType type);

typedef struct AstDumper_s {
	int (*pre_fn)(struct AstDumper_s *d, Ast *ast);
	void (*print_fn)(struct AstDumper_s *d, const char *text);
	void (*post_fn)(struct AstDumper_s *d, Ast *ast);
} AstDumper;

void dumpAst(Ast *ast, AstDumper *dumper);
