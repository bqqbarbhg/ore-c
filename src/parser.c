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
static Ast *parseStatement(Parser *p);

static Ast *parseType(Parser *p)
{
	if (accept(p, T_Identifier)) {
		AstIdent *ident = pushAst(p, AstIdent);
		ident->name = p->prev;
		ident->ast.span = p->prev.span;
		return &ident->ast;
	} else {
		errorAtToken(p, "Expected a type");
		return NULL;
	}
}

static Ast *parseAtom(Parser *p)
{
	if (accept(p, T_Identifier)) {
		AstIdent *ident = pushAst(p, AstIdent);
		ident->name = p->prev;
		ident->ast.span = p->prev.span;
		return &ident->ast;
	} else if (accept(p, T_Number)) {
		AstNumber *number = pushAst(p, AstNumber);
		number->value = p->prev;
		number->ast.span = p->prev.span;
		return &number->ast;
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
	} else if (accept(p, T_Sub)) {
		AstUnop *unop = pushAst(p, AstUnop);
		unop->op = p->prev;
		unop->expr = parseAtom(p);
		if (!unop->expr) return NULL;
		unop->ast.span = mergeSpan(unop->op.span, unop->expr->span);
		return &unop->ast;
	} else {
		errorAtToken(p, "Expected an expression");
		return NULL;
	}
}

static Ast *parseCall(Parser *p)
{
	Ast *left = parseAtom(p);

	while (!p->failed) {
		if (accept(p, T_ParenOpen)) {
			Token open = p->prev;
			Ast *localArgs[32];
			Ast_ptr_buf args = buf_local(localArgs);
			Token close = p->token;
			if (!accept(p, T_ParenClose)) {
				do {
					Ast *expr = parseExpr(p);
					if (!expr) break;
					buf_push(&args, &expr);
				} while (accept(p, T_Comma) && !p->failed);
				close = p->token;
				if (!accept(p, T_ParenClose)) {
					errorAt(p, open.span, "Unclosed '(' for function call");
					errorAtToken(p, "Expected closing ')'");
				}
			}
			if (p->failed) {
				buf_reset(&args);
				return NULL;
			}

			AstCall *call = pushAstExtra(p, AstCall, sizeof(Ast*) * args.size);
			call->func = left;
			call->numArgs = args.size;
			memcpy(call->args, args.data, sizeof(Ast*) * args.size);
			call->ast.span = mergeSpan(open.span, close.span);
			buf_reset(&args);
			left = &call->ast;
		} else if (accept(p, T_BlockOpen)) {
			errorAtToken(p, "Indexing is unimplemented");
			return NULL;
		} else {
			return left;
		}
	}

	return NULL;
}

static Ast *parseFactor(Parser *p)
{
	Ast *left = parseCall(p);
	if (!left) return NULL;
	while (accept(p, T_Mul) || accept(p, T_Div) || accept(p, T_Mod)) {
		AstBinop *binop = pushAst(p, AstBinop);
		binop->op = p->prev;
		binop->left = left;
		binop->right = parseCall(p);
		if (!binop->right) return NULL;
		binop->ast.span = mergeSpan(binop->left->span, binop->right->span);
		left = &binop->ast;
	}

	return left;
}

static Ast *parseTerm(Parser *p)
{
	Ast *left = parseFactor(p);
	if (!left) return NULL;
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

static Ast *parseCompare(Parser *p)
{
	Ast *left = parseTerm(p);
	if (!left) return NULL;
	while (accept(p, T_Equal) || accept(p, T_NotEqual)
		|| accept(p, T_Less) || accept(p, T_Greater)) {
		AstBinop *binop = pushAst(p, AstBinop);
		binop->op = p->prev;
		binop->left = left;
		binop->right = parseTerm(p);
		if (!binop->right) return NULL;
		binop->ast.span = mergeSpan(binop->left->span, binop->right->span);
		left = &binop->ast;
	}

	return left;
}

static Ast *parseAndOr(Parser *p)
{
	Ast *left = parseCompare(p);
	if (!left) return NULL;
	while (accept(p, T_And) || accept(p, T_Or)) {
		AstBinop *binop = pushAst(p, AstBinop);
		binop->op = p->prev;
		binop->left = left;
		binop->right = parseCompare(p);
		if (!binop->right) return NULL;
		binop->ast.span = mergeSpan(binop->left->span, binop->right->span);
		left = &binop->ast;
	}

	return left;
}

static Ast *parseAssign(Parser *p)
{
	Ast *left = parseAndOr(p);
	if (!left) return NULL;
	if (accept(p, T_Assign)) {
		AstBinop *binop = pushAst(p, AstBinop);
		binop->op = p->prev;
		binop->left = left;
		binop->right = parseAssign(p);
		if (!binop->right) return NULL;
		binop->ast.span = mergeSpan(binop->left->span, binop->right->span);
		left = &binop->ast;
	}
	return left;
}

static Ast *parseExpr(Parser *p)
{
	return parseAssign(p);
}

static IfBranch finishIf(Parser *p, Token ifToken)
{
	IfBranch result = { 0 };
	Token open = p->token;
	if (!accept(p, T_ParenOpen)) {
		errorAt(p, ifToken.span, "Expected '(' before if condition");
		return result;
	}

	result.cond = parseExpr(p);
	if (!result.cond) return result;
	if (!accept(p, T_ParenClose)) {
		errorAt(p, open.span, "Unclosed if condition '('");
		errorAtToken(p, "Expected closing ')'");
		return result;
	}

	result.body = parseStatement(p);
	if (!result.body) {
		if (!p->failed) {
			errorAt(p, ifToken.span, "Missing 'if' body");
			errorAtToken(p, "Expected 'if' body here");
		}
		return result;
	}

	return result;
}

static AstBlock *finishBlock(Parser *p, Token blockOpen)
{
	Ast *localStatements[128];
	Ast_ptr_buf statements = buf_local(localStatements);

	while (!accept(p, T_BlockClose) && !p->failed) {
		if (p->token.type == T_End) {
			errorAt(p, blockOpen.span, "Unclosed function body");
			errorAtToken(p, "Expected closing '}'");
			break;
		}

		Ast *ast = parseStatement(p);
		if (ast) {
			buf_push(&statements, &ast);
		}
	}
	Token blockClose = p->prev;

	AstBlock *block = NULL;
	if (!p->failed) {
		block = pushAstExtra(p, AstBlock, sizeof(Ast*) * statements.size);
		block->numStatements = statements.size;
		memcpy(block->statements, statements.data, sizeof(Ast*) * statements.size);
		block->ast.span = mergeSpan(blockOpen.span, blockClose.span);
	}
	buf_reset(&statements);
	return block;
}

static AstVar *finishVar(Parser *p, Token varTok)
{
	AstVar *var = pushAst(p, AstVar);
	var->name = p->token;
	if (!accept(p, T_Identifier)) {
		errorAtPrev(p, "Expected variable name");
		return NULL;
	}

	if (accept(p, T_Colon)) {
		var->type = parseType(p);
	} else {
		var->type = NULL;
	}

	if (accept(p, T_Assign)) {
		var->init = parseExpr(p);
	} else {
		var->init = NULL;
	}

	if (!accept(p, T_Newline) && !accept(p, T_End)) {
		errorAtToken(p, "Expected a newline after variable declaration");
	}

	if (!var->type && !var->init) {
		errorAt(p, var->name.span, "Cannot infer type without initializer");
	}

	var->ast.span = mergeSpan(varTok.span, p->prev.span);
	return !p->failed ? var : NULL;
}

static Ast *parseStatement(Parser *p)
{
	if (accept(p, T_Newline)) {
		return NULL;
	} else if (accept(p, T_BlockOpen)) {
		AstBlock *block = finishBlock(p, p->prev);
		return block ? &block->ast : NULL;
	} else if (accept(p, KW_If)) {
		Token firstIf = p->prev;

		IfBranch localBranches[32];
		buf_type(IfBranch) branches = buf_local(localBranches);

		IfBranch *firstBranch = buf_push_uninit(&branches);
		*firstBranch = finishIf(p, firstIf);

		Ast *elseBody = NULL;
		while (accept(p, KW_Else) && !p->failed) {
			Token nextElse = p->prev;
			if (accept(p, KW_If)) {
				Token nextIf = p->prev;
				IfBranch *nextBranch = buf_push_uninit(&branches);
				*nextBranch = finishIf(p, nextIf);
			} else {
				elseBody = parseStatement(p);
				if (!elseBody && !p->failed) {
					errorAt(p, nextElse.span, "Missing 'else' body");
				}
				break;
			}
		}

		AstIf *ast = NULL;
		if (!p->failed) {
			ast = pushAstExtra(p, AstIf, sizeof(IfBranch) * branches.size);
			ast->numBranches = branches.size;
			memcpy(ast->branches, branches.data, sizeof(IfBranch) * branches.size);
			ast->elseBranch = elseBody;
			ast->ast.span = mergeSpan(firstIf.span, p->prev.span);
		}
		buf_reset(&branches);
		return &ast->ast;
	} else if (accept(p, KW_While)) {
		AstWhile *ast = pushAst(p, AstWhile);
		Token whileTok = p->token;

		Token open = p->token;
		if (!accept(p, T_ParenOpen)) {
			errorAt(p, whileTok.span, "Expected '(' before while condition");
			return NULL;
		}

		ast->cond = parseExpr(p);
		if (!ast->cond) return NULL;
		if (!accept(p, T_ParenClose)) {
			errorAt(p, open.span, "Unclosed while condition '('");
			errorAtToken(p, "Expected closing ')'");
			return NULL;
		}

		ast->body = parseStatement(p);
		if (!ast->body) {
			if (!p->failed) {
				errorAt(p, whileTok.span, "Missing 'while' body");
				errorAtToken(p, "Expected 'while' body here");
			}
			return NULL;
		}

		ast->ast.span = mergeSpan(whileTok.span, p->prev.span);
		return &ast->ast;
	} else if (accept(p, KW_Var)) {
		AstVar *var = finishVar(p, p->prev);
		return &var->ast;
	} else if (accept(p, KW_Return)) {
		Token returnTok = p->prev;
		AstReturn *ast = pushAst(p, AstReturn);
		if (!accept(p, T_Newline)) {
			ast->expr = parseExpr(p);
			if (!ast->expr) return NULL;
			if (!accept(p, T_Newline) && !accept(p, T_End)) {
				errorAtToken(p, "Expected a newline after return");
			}
			ast->ast.span = mergeSpan(returnTok.span, ast->expr->span);
		} else {
			ast->ast.span = returnTok.span;
			ast->expr = NULL;
		}
		return &ast->ast;
	} else {
		AstExpr *expr = pushAst(p, AstExpr);
		expr->expr = parseExpr(p);
		if (!expr->expr) return NULL;
		expr->ast.span = expr->expr->span;
		if (!accept(p, T_Newline) && !accept(p, T_End)) {
			errorAtToken(p, "Expected a newline after statement");
		}
		return &expr->ast;
	}
}

static Ast *parseToplevel(Parser *p)
{
	if (accept(p, T_Newline)) {
		return NULL;
	} else if (accept(p, KW_Def)) {
		Token def = p->prev;
		Token name = p->token;
		if (!accept(p, T_Identifier)) {
			errorAt(p, def.span, "Expected function name");
			return NULL;
		}
		Token parenOpen = p->token;
		if (!accept(p, T_ParenOpen)) {
			errorAt(p, name.span, "Expected function argument list");
			return NULL;
		}

		DeclAst localDecls[16];
		buf_type(DeclAst) decls = buf_local(localDecls);

		Token parenClose = p->token;
		if (!accept(p, T_ParenClose)) {
			do {
				DeclAst *decl = buf_push_uninit(&decls);
				decl->name = p->token;
				if (!accept(p, T_Identifier)) {
					errorAtPrev(p, "Expected argument name");
					break;
				}
				if (!accept(p, T_Colon)) {
					errorAtPrev(p, "Expected ':' before argument type");
					break;
				}
				decl->type = parseType(p);
				if (!decl->type) break;
			} while (accept(p, T_Comma));

			parenClose = p->token;
			if (!accept(p, T_ParenClose)) {
				errorAt(p, parenOpen.span, "Expected closing ')'");
			}
		}

		Ast *returnType = NULL;
		if (accept(p, T_Colon)) {
			returnType = parseType(p);
		} else {
			// TODO: Void/infered by default?
			errorAt(p, parenClose.span, "Expected ':' before return type");
			return NULL;
		}

		AstBlock *block;
		if (!p->failed) {
			Token blockOpen = p->token;
			if (accept(p, T_BlockOpen)) {
				block = finishBlock(p, blockOpen);
			} else {
				// TODO: Function prototypes?
				errorAtToken(p, "Expected function body");
			}
		}

		AstDef *ast = NULL;
		if (!p->failed) {
			ast = pushAstExtra(p, AstDef, sizeof(DeclAst) * decls.size);
			ast->name = name;
			ast->numParams = decls.size;
			memcpy(ast->params, decls.data, sizeof(DeclAst) * decls.size);
			ast->body = block;
			ast->returnType = returnType;
			ast->ast.span = mergeSpan(def.span, parenClose.span);
		}
		buf_reset(&decls);
		return &ast->ast;
	} else if (accept(p, KW_Var)) {
		AstVar *var = finishVar(p, p->prev);
		if (!var->type) {
			errorAt(p, var->name.span, "Top-level variables must have types");
			return NULL;
		}
		return &var->ast;
	} else {
		errorAtToken(p, "Invalid top-level declaration");
		return NULL;
	}
}

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

	AstToplevel *toplevel = pushAstExtra(p, AstToplevel, sizeof(Ast*) * toplevelAsts.size);
	toplevel->numToplevels = toplevelAsts.size;
	memcpy(toplevel->toplevels, toplevelAsts.data, sizeof(Ast*) * toplevelAsts.size);

	freeLexer(lexer);

	if (p->failed) {
		arena_reset(&p->astArena);
		return NULL;
	} else {
		toplevel->allocation = p->astArena.data;
	}

	return &toplevel->ast;
}

void freeAst(Ast *ast)
{
	assert(ast->type == A_AstToplevel);
	AstToplevel *toplevel = (AstToplevel*)ast;
	arena_free_data(toplevel->allocation);
}

const char *getAstTypeName(AstType type)
{
	switch (type) {
	case A_AstToplevel: return "Toplevel";
	case A_AstDef: return "Def";
	case A_AstBlock: return "Block";
	case A_AstExpr: return "Expr";
	case A_AstIf: return "If";
	case A_AstWhile: return "While";
	case A_AstVar: return "Var";
	case A_AstReturn: return "Return";
	case A_AstIdent: return "Ident";
	case A_AstNumber: return "Number";
	case A_AstUnop: return "Unop";
	case A_AstBinop: return "Binop";
	case A_AstCall: return "Call";
	case A_AstParen: return "Paren";
	default:
		assert(0 && "Unhandled AST type");
		return "";
	}
}

static void dump(AstDumper *d, const char *str)
{
	d->print_fn(d, str);
}

static void dumpIndent(AstDumper *d, int indent)
{
	for (int i = 0; i < indent; i++) {
		dump(d, "  ");
	}
}

static void dumpAstRecursive(AstDumper *d, Ast *ast, int indent)
{
	if (d->pre_fn) {
		if (!d->pre_fn(d, ast)) return;
	}

	switch (ast->type) {

	case A_AstToplevel: {
		AstToplevel *block = (AstToplevel*)ast;
		for (uint32_t i = 0; i < block->numToplevels; i++) {
			if (block->toplevels[i]->type == A_AstDef) dump(d, "\n");
			dumpIndent(d, indent);
			dumpAstRecursive(d, block->toplevels[i], indent);
			dump(d, "\n");
		}
	} break;

	case A_AstDef: {
		AstDef *def = (AstDef*)ast;
		dump(d, "def ");
		dump(d, getCString(def->name.symbol));
		dump(d, "(");
		for (uint32_t i = 0; i < def->numParams; i++) {
			if (i != 0) dump(d, ", ");
			DeclAst decl = def->params[i];
			dump(d, getCString(decl.name.symbol));
			dump(d, ": ");
			dumpAstRecursive(d, decl.type, indent);
		}
		dump(d, "): ");
		dumpAstRecursive(d, def->returnType, indent);
		dump(d, " ");
		dumpAstRecursive(d, &def->body->ast, indent);
	} break;

	case A_AstBlock: {
		AstBlock *block = (AstBlock*)ast;
		dump(d, "{\n");
		for (uint32_t i = 0; i < block->numStatements; i++) {
			dumpIndent(d, indent + 1);
			dumpAstRecursive(d, block->statements[i], indent + 1);
			dump(d, "\n");
		}
		dumpIndent(d, indent);
		dump(d, "}");
	} break;

	case A_AstExpr: {
		AstExpr *expr = (AstExpr*)ast;
		dumpAstRecursive(d, expr->expr, indent);
	} break;

	case A_AstIf: {
		AstIf *if_ = (AstIf*)ast;
		for (uint32_t i = 0; i < if_->numBranches; i++) {
			IfBranch branch = if_->branches[i];
			if (i != 0) {
				dump(d, " else ");
			}
			dump(d, "if (");
			dumpAstRecursive(d, branch.cond, indent);
			dump(d, ") ");
			dumpAstRecursive(d, branch.body, indent);
		}
		if (if_->elseBranch) {
			dump(d, " else ");
			dumpAstRecursive(d, if_->elseBranch, indent);
		}
	} break;

	case A_AstWhile: {
		AstWhile *while_ = (AstWhile*)ast;
		dump(d, "while (");
		dumpAstRecursive(d, while_->cond, indent);
		dump(d, ") ");
		dumpAstRecursive(d, while_->body, indent);
	} break;

	case A_AstVar: {
		AstVar *var = (AstVar*)ast;
		dump(d, "var ");
		dump(d, getCString(var->name.symbol));
		if (var->type) {
			dump(d, ": ");
			dumpAstRecursive(d, var->type, indent);
		}
		if (var->init) {
			dump(d, " = ");
			dumpAstRecursive(d, var->init, indent);
		}
	} break;

	case A_AstReturn: {
		AstReturn *return_ = (AstReturn*)ast;
		dump(d, "return ");
		if (return_->expr) dumpAstRecursive(d, return_->expr, indent);
	} break;

	case A_AstIdent: {
		AstIdent *ident = (AstIdent*)ast;
		dump(d, getCString(ident->name.symbol));
	} break;

	case A_AstNumber: {
		AstNumber *number = (AstNumber*)ast;
		dump(d, getCString(number->value.symbol));
	} break;

	case A_AstUnop: {
		AstUnop *unop = (AstUnop*)ast;
		dump(d, getCString(unop->op.symbol));
		dumpAstRecursive(d, unop->expr, indent);
	} break;

	case A_AstBinop: {
		AstBinop *binop = (AstBinop*)ast;
		dumpAstRecursive(d, binop->left, indent);
		dump(d, " ");
		dump(d, getCString(binop->op.symbol));
		dump(d, " ");
		dumpAstRecursive(d, binop->right, indent);
	} break;

	case A_AstCall: {
		AstCall *call = (AstCall*)ast;
		dumpAstRecursive(d, call->func, indent);
		dump(d, "(");
		for (uint32_t i = 0; i < call->numArgs; i++) {
			if (i != 0) dump(d, ", ");
			dumpAstRecursive(d, call->args[i], indent);
		}
		dump(d, ")");
	} break;

	case A_AstParen: {
		AstParen *paren = (AstParen*)ast;
		dump(d, "(");
		dumpAstRecursive(d, paren->expr, indent);
		dump(d, ")");
	} break;

	default: {
		assert(0 && "Unhandled AST type");
	} break;

	}

	if (d->post_fn) {
		d->post_fn(d, ast);
	}
}

void dumpAst(Ast *ast, AstDumper *dumper)
{
	dumpAstRecursive(dumper, ast, 0);
}
