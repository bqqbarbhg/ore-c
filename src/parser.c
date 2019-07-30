#include "base.h"
#include "parser.h"
#include "error.h"

#include "arena.h"

#include <stdio.h>

typedef struct {
	Lexer *lexer;
	ErrorList *errorList;

	Token tokenBuffer[64];
	uint32_t tokenIndex, tokenAmount;

	Token prev;
	Token token;

	arena astArena;

	int failed;

} Parser;

typedef buf_type(Ast*) Ast_ptr_buf;

static void errorAt(Parser *p, SourceSpan span, const char *message)
{
	pushError(p->errorList, span, message);
	p->failed = 1;
}

static void errorAtPrev(Parser *p, const char *message)
{
	pushError(p->errorList, p->prev.span, message);
	p->failed = 1;
}

static void errorAtToken(Parser *p, const char *message)
{
	pushError(p->errorList, p->token.span, message);
	p->failed = 1;
}

static SourceSpan mergeSpan(SourceSpan left, SourceSpan right)
{
	assert(left.file == right.file);
	assert(left.offset < right.offset);
	uint32_t length = (right.offset + right.length) - left.offset;
	return (SourceSpan){
		.offset = left.offset,
		.file = left.file,
		.length = length < 0xffff ? (uint16_t)length : UINT16_C(0xffff),
	};
}

static int isEndOrError(TokenType type) {
	return type == T_End || type == T_Error;
}

static void nextToken(Parser *p)
{
	p->prev = p->token;
	if (p->tokenIndex < p->tokenAmount) {
		p->token = p->tokenBuffer[p->tokenIndex++];
	} else {
		if (!isEndOrError(p->token.type)) {
			p->token = scan(p->lexer);
		}
		if (!isEndOrError(p->token.type)) {
			p->tokenIndex = 0;
			p->tokenAmount = 0;
			while (p->tokenAmount < arraySize(p->tokenBuffer)) {
				Token token = scan(p->lexer);
				p->tokenBuffer[p->tokenAmount++] = token;
				if (isEndOrError(token.type)) break;
			}
		}
	}
}

static int accept(Parser *p, TokenType type)
{
	if (p->token.type == type) {
		nextToken(p);
		return 1;
	} else {
		return 0;
	}
}

static Ast *pushAstSize(Parser *p, AstType type, size_t size)
{
	Ast *ast = arena_push_size_uninit(&p->astArena, size);
	ast->type = type;
	return ast;
}

#define pushAst(p, type) (type*)pushAstSize((p), A_##type, sizeof(type))
#define pushAstExtra(p, type, extra) (type*)pushAstSize((p), A_##type, sizeof(type) + (extra))

static Ast *parseExpr(Parser *p);

static Ast *parseAtom(Parser *p)
{
	if (accept(p, T_Identifier)) {
		AstIdent *ident = pushAst(p, AstIdent);
		ident->name = p->prev;
		ident->ast.span = p->prev.span;
		return &ident->ast;
	} else if (accept(p, T_ParenOpen)) {
		SourceSpan begin = p->prev.span;
		AstParen *paren = pushAst(p, AstParen);
		paren->expr = parseExpr(p);
		if (!paren->expr) return NULL;
		if (!accept(p, T_ParenClose)) {
			errorAt(p, begin, "Unclosed parenthesis");
			errorAtToken(p, "Expected closing parenthesis");
			return NULL;
		}
		paren->ast.span = mergeSpan(begin, p->prev.span);
		return &paren->ast;
	} else {
		errorAtToken(p, "Expected an expression");
		return NULL;
	}
}

static Ast *parseFactor(Parser *p)
{
	Ast *left = parseAtom(p);
	while (accept(p, T_Mul) || accept(p, T_Div) || accept(p, T_Mod)) {
		AstBinop *binop = pushAst(p, AstBinop);
		binop->op = p->prev;
		binop->left = left;
		binop->right = parseAtom(p);
		if (!binop->right) return NULL;
		binop->ast.span = mergeSpan(binop->left->span, binop->right->span);
		left = &binop->ast;
	}

	return left;
}

static Ast *parseTerm(Parser *p)
{
	Ast *left = parseFactor(p);
	while (accept(p, T_Add) || accept(p, T_Sub)) {
		AstBinop *binop = pushAst(p, AstBinop);
		binop->op = p->prev;
		binop->left = left;
		binop->right = parseFactor(p);
		if (!binop->right) return NULL;
		binop->ast.span = mergeSpan(binop->left->span, binop->right->span);
		left = &binop->ast;
	}

	return left;
}

static Ast *parseExpr(Parser *p)
{
	return parseTerm(p);
}

static Ast *parseToplevel(Parser *p)
{
	if (accept(p, T_Newline)) {
		return NULL;
	} else {
		AstExpr *expr = pushAst(p, AstExpr);
		expr->expr = parseExpr(p);
		if (!expr->expr) return NULL;
		expr->ast.span = expr->expr->span;
		return &expr->ast;
	}
}

typedef struct {
	arena arena;
	AstBlock block;
} AstToplevelBlock;

Ast *parse(const LexerInput *input)
{
	Lexer *lexer = createLexer(input);

	Parser parser = {
		.lexer = lexer,
		.errorList = input->errorList,
	}, *p = &parser;

	p->token = scan(lexer);

	Ast_ptr_buf toplevelAsts = { 0 };
	while (!accept(p, T_End) && !p->failed) {
		Ast *ast = parseToplevel(&parser);
		if (ast) {
			buf_push(&toplevelAsts, &ast);
		}
	}

	AstToplevelBlock *toplevel = arena_push_size_zero(&p->astArena,
		sizeof(AstToplevelBlock) + sizeof(Ast*) * toplevelAsts.size);
	toplevel->block.numStatements = toplevelAsts.size;
	memcpy(toplevel->block.statements, toplevelAsts.data, sizeof(Ast*) * toplevelAsts.size);

	freeLexer(lexer);

	if (p->failed) {
		arena_reset(&p->astArena);
		return NULL;
	}

	return &toplevel->block.ast;
}

void freeAst(Ast *ast)
{
	AstToplevelBlock *toplevel = (AstToplevelBlock*)((char*)ast - offsetof(AstToplevelBlock, block));
	arena_reset(&toplevel->arena);
}

static void dumpAstRecursive(Ast *ast, int indent)
{
	switch (ast->type) {

	case A_AstBlock: {
		AstBlock *block = (AstBlock*)ast;
		if (indent > 0)
			printf("{\n");
		for (uint32_t i = 0; i < block->numStatements; i++) {
			dumpAstRecursive(block->statements[i], indent + 1);
		}
		if (indent > 0)
			printf("}\n");
	} break;

	case A_AstExpr: {
		AstExpr *expr = (AstExpr*)ast;
		dumpAstRecursive(expr->expr, indent);
		printf("\n");
	} break;

	case A_AstIdent: {
		AstIdent *ident = (AstIdent*)ast;
		printf("%s", getCString(ident->name.symbol));
	} break;

	case A_AstBinop: {
		AstBinop *binop = (AstBinop*)ast;
		dumpAstRecursive(binop->left, indent);
		printf(" %s ", getCString(binop->op.symbol));
		dumpAstRecursive(binop->right, indent);
	} break;

	case A_AstParen: {
		AstParen *paren = (AstParen*)ast;
		printf("(");
		dumpAstRecursive(paren->expr, indent);
		printf(")");
	} break;

	}
}

void dumpAst(Ast *ast)
{
	dumpAstRecursive(ast, 0);
}
