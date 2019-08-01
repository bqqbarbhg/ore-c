#include "base.h"
#include "compiler.h"

#include <stdio.h>
#include <stdarg.h>

typedef struct Scope_s {
	struct Scope_s *parent;
	SymbolMap locals;
} Scope;

struct Compiler_s {
	ErrorList *errorList;

	Func *func;
	Scope *scope;

	SymbolMap globals;

	Module module;

	uint32_t orCounter, andCounter;
	uint32_t ifCounter, whileCounter;
	uint32_t returnCounter;

	uint32_t blockIndex;
	Block *block;

	int failed;
};

static void errorFmt(Compiler *c, SourceSpan span, const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	char buf[512];
	vsnprintf(buf, sizeof(buf), fmt, args);
	va_end(args);
	pushError(c->errorList, span, buf);
	c->failed = 1;
}

static void gatherToplevel(Compiler *c, Ast *ast)
{
	AstToplevel *toplevel = (AstToplevel*)ast;
	for (uint32_t i = 0; i < toplevel->numToplevels; i++) {
		Ast *ast = toplevel->toplevels[i];
		switch (ast->type) {
		case A_AstDef: {
			AstDef *def = (AstDef*)ast;
			Func *func = buf_push_zero(&c->module.funcs);
			func->name = def->name;
			func->ast = def;
			uint32_t globalIndex = c->module.globals.size;
			Global *global = buf_push_zero(&c->module.globals);
			global->name = func->name;
			global->ast = &func->ast->ast;
			global->type = (TypeRef) { 10 };
			global->constant = 1;
			symbolMapInsert(&c->globals, def->name.symbol, globalIndex);
		} break;

		case A_AstVar: {
			AstVar *var = (AstVar*)ast;
			uint32_t globalIndex = c->module.globals.size;
			Global *global = buf_push_zero(&c->module.globals);
			global->name = var->name;
			global->ast = &var->ast;
			global->type = (TypeRef) { 10 };
			symbolMapInsert(&c->globals, var->name.symbol, globalIndex);
		} break;

		default:
			errorFmt(c, ast->span, "Internal: Unexpected toplevel AST type '%s'",
				getAstTypeName(ast->type));
			break;
		}
	}
}

static uint32_t pushBlock(Compiler *c, const char *fmt, ...)
{
	Block *block = buf_push_zero(&c->func->blocks);
	va_list args;
	va_start(args, fmt);
	vsnprintf(block->label, sizeof(block->label), fmt, args);
	va_end(args);

	c->block = &c->func->blocks.data[c->blockIndex];
	return c->func->blocks.size - 1;
}

static void switchBlock(Compiler *c, uint32_t index)
{
	c->blockIndex = index;
	c->block = &c->func->blocks.data[index];
}

static Value pushTemp(Compiler *c, TypeRef type)
{
	Temp *temp = buf_push_uninit(&c->func->temps);
	temp->type = type;
	return (Value) { type, VK_Temp, c->func->temps.size - 1 };
}

static Value pushConst(Compiler *c, TypeRef type, const void *data, size_t size)
{
	Const *const_ = buf_push_uninit(&c->func->consts);
	const_->type = type;
	return (Value) { type, VK_Temp, c->func->consts.size - 1 };
}

static Inst *pushInst(Compiler *c, Ast *ast, Op op, TypeRef type)
{
	Inst *inst = buf_push_zero(&c->block->insts);
	inst->ast = ast;
	inst->op = op;
	inst->type = type;
	return inst;
}

static Inst *pushInstA(Compiler *c, Ast *ast, Op op, TypeRef type, ValueRef a)
{
	Inst *inst = buf_push_zero(&c->block->insts);
	inst->ast = ast;
	inst->op = op;
	inst->type = type;
	inst->a = a;
	return inst;
}

static Inst *pushInst0(Compiler *c, Ast *ast, Op op, TypeRef type, ValueRef dst)
{
	Inst *inst = buf_push_zero(&c->block->insts);
	inst->ast = ast;
	inst->op = op;
	inst->type = type;
	inst->dst = dst;
	return inst;
}

static Inst *pushInst1(Compiler *c, Ast *ast, Op op, TypeRef type, ValueRef dst, ValueRef a)
{
	Inst *inst = buf_push_zero(&c->block->insts);
	inst->ast = ast;
	inst->op = op;
	inst->type = type;
	inst->dst = dst;
	inst->a = a;
	return inst;
}

static Inst *pushInst2(Compiler *c, Ast *ast, Op op, TypeRef type, ValueRef dst, ValueRef a, ValueRef b)
{
	Inst *inst = buf_push_zero(&c->block->insts);
	inst->ast = ast;
	inst->op = op;
	inst->type = type;
	inst->dst = dst;
	inst->a = a;
	inst->b = b;
	return inst;
}

static Value compileImplicitCast(Value src, TypeRef dstType)
{
	return src;
}

static uint32_t findLocal(Compiler *c, Symbol symbol)
{
	Scope *scope = c->scope;
	while (scope) {
		uint32_t index = symbolMapFind(&scope->locals, symbol);
		if (index != ~0u) return index;
		scope = scope->parent;
	}
	return ~0u;
}

static Value compileExpr(Compiler *c, Ast *ast)
{
	switch (ast->type) {

	case A_AstAssign: {
		AstAssign *assign = (AstAssign*)ast;
		Value lhs = compileExpr(c, assign->left);
		Value rhs = compileExpr(c, assign->right);
		rhs = compileImplicitCast(rhs, lhs.type);
		pushInst1(c, ast, O_Assign, lhs.type, lhs.ref, rhs.ref);
		return lhs;
	} break;

	case A_AstIdent: {
		AstIdent *ident = (AstIdent*)ast;
		uint32_t local = findLocal(c, ident->name.symbol);
		if (local != ~0u) {
			TypeRef type = c->func->locals.data[local].type;
			return (Value) { type, VK_Local, local };
		}

		uint32_t globalIndex = symbolMapFind(&c->globals, ident->name.symbol);
		if (globalIndex != ~0u) {
			Global *global = &c->module.globals.data[globalIndex];
			if (global->constant) {
				return (Value) { global->type, VK_GlobalConst, globalIndex };
			} else {
				Value temp = pushTemp(c, global->type);
				Inst *inst = pushInst0(c, ast, O_Global, global->type, temp.ref);
				inst->ext = globalIndex;
				return temp;
			}
		}

		errorFmt(c, ast->span, "Could not resolve identifier '%s'",
			getCString(ident->name.symbol));
		return ErrorValue;
	} break;

	case A_AstNumber: {
		AstNumber *number = (AstNumber*)ast;
		// TEMP TEMP
		int32_t value = atoi(getCString(number->value.symbol));
		TypeRef intType = { 1 };
		return pushConst(c, intType, &value, sizeof(value));
	} break;

	case A_AstUnop: {
		AstUnop *unop = (AstUnop*)ast;
		Value expr = compileExpr(c, unop->expr);
		Value dst = pushTemp(c, expr.type);
		Inst *inst = pushInst1(c, ast, O_Arith, expr.type, dst.ref, expr.ref);
		switch (unop->op.type) {
		case T_Sub: inst->ext = OA_Neg; break;
		default:
			errorFmt(c, unop->op.span, "Internal: Invalid unary operator");
			return ErrorValue;
		}
		return dst;
	} break;

	case A_AstBinop: {
		AstBinop *binop = (AstBinop*)ast;
		Value lhs = compileExpr(c, binop->left);
		Value rhs = compileExpr(c, binop->right);
		Value dst = pushTemp(c, lhs.type);
		Op op;
		uint32_t ext;
		switch (binop->op.type) {
		case T_Add: op = O_Arith; ext = OA_Add; break;
		case T_Sub: op = O_Arith; ext = OA_Sub; break;
		case T_Mul: op = O_Arith; ext = OA_Mul; break;
		case T_Div: op = O_Arith; ext = OA_Div; break;
		case T_Mod: op = O_Arith; ext = OA_Mod; break;
		case T_Equal:     op = O_Compare; ext = OA_Add; break;
		case T_NotEqual:  op = O_Compare; ext = OA_Sub; break;
		case T_Less:      op = O_Compare; ext = OA_Mul; break;
		case T_LessEq:    op = O_Compare; ext = OA_Mul; break;
		case T_Greater:   op = O_Compare; ext = OA_Mul; break;
		case T_GreaterEq: op = O_Compare; ext = OA_Mul; break;
		default:
			errorFmt(c, binop->op.span, "Internal: Invalid binary operator");
			return ErrorValue;
		}
		rhs = compileImplicitCast(rhs, lhs.type);
		Inst *inst = pushInst2(c, ast, op, lhs.type, dst.ref, lhs.ref, rhs.ref);
		inst->ext = ext;
		return dst;
	} break;

	case A_AstLogic: {
		AstLogic *logic = (AstLogic*)ast;
		Value lhs = compileExpr(c, logic->left);
		pushInstA(c, logic->left, O_Branch, lhs.type, lhs.ref);

		if (logic->op.type == T_And) {
			uint32_t index = ++c->andCounter;
			uint32_t trueBlock = pushBlock(c, "and%u_true", index);
			uint32_t falseBlock = pushBlock(c, "and%u_false", index);
			c->block->blockTrue = trueBlock;
			c->block->blockFalse = falseBlock;

			switchBlock(c, trueBlock);

			Value rhs = compileExpr(c, logic->right);
			pushInstA(c, logic->right, O_Branch, rhs.type, rhs.ref);
			c->block->blockTrue = trueBlock;
			c->block->blockFalse = falseBlock;
		} else if (logic->op.type == T_Or) {
			uint32_t index = ++c->orCounter;
			uint32_t trueBlock = pushBlock(c, "or%u_true", index);
			uint32_t falseBlock = pushBlock(c, "or%u_false", index);
			c->block->blockTrue = trueBlock;
			c->block->blockFalse = falseBlock;

			switchBlock(c, falseBlock);

			Value rhs = compileExpr(c, logic->right);
			pushInstA(c, logic->right, O_Branch, rhs.type, rhs.ref);
			c->block->blockTrue = trueBlock;
			c->block->blockFalse = falseBlock;

			switchBlock(c, trueBlock);
		} else {
			errorFmt(c, logic->op.span, "Internal: Invalid logic operator");
			return ErrorValue;
		}

		TypeRef boolType = { 2 };
		return (Value) { boolType, VK_Block, c->blockIndex };
	} break;

	case A_AstCall: {
		AstCall *call = (AstCall*)ast;

		Value localArgs[32];
		Value_buf args = buf_local(localArgs);
		buf_reserve(&args, call->numArgs);
		for (uint32_t i = 0; i < call->numArgs; i++) {
			Value value = compileExpr(c, call->args[i]);
			buf_push(&args, &value);
		}

		for (uint32_t i = 0; i < call->numArgs; i++) {
			Inst *inst = pushInst(c, ast, O_Arg, args.data[i].type);
			inst->a = args.data[i].ref;
			inst->ext = i;
		}

		Value funcVal = compileExpr(c, call->func);

		TypeRef returnType = { 3 };
		Value result = pushTemp(c, returnType);
		Inst *inst = pushInst1(c, ast, O_Call, funcVal.type, result.ref, funcVal.ref);

		buf_reset(&args);
		return result;
	} break;

	case A_AstParen: {
		AstParen *paren = (AstParen*)ast;
		return compileExpr(c, paren->expr);
	} break;

	default: {
		errorFmt(c, ast->span, "Internal: Invalid expression AST type");
		return ErrorValue;
	} break;

	}
}

static void compileStatement(Compiler *c, Ast *ast)
{
	switch (ast->type) {
	case A_AstBlock: {
		AstBlock *block = (AstBlock*)ast;

		uint64_t localScope[64];
		Scope scope = { .parent = c->scope };
		symbolMapInitLocal(&scope.locals, localScope, sizeof(localScope));
		c->scope = &scope;

		for (uint32_t i = 0; i < block->numStatements; i++) {
			compileStatement(c, block->statements[i]);
		}

		c->scope = scope.parent;
	} break;

	case A_AstExpr: {
		AstExpr *expr = (AstExpr*)ast;
		compileExpr(c, ast);
	} break;

	case A_AstIf: {
		AstIf *if_ = (AstIf*)ast;
		uint32_t ifIndex = ++c->ifCounter;

		uint32_t endBlock = pushBlock(c, "if%u_end", ifIndex);

		for (uint32_t i = 0; i < if_->numBranches; i++) {
			IfBranch branch = if_->branches[i];
			uint32_t bodyBlock = pushBlock(c, "if%u_body%u", ifIndex, i + 1);
			uint32_t elseBlock = endBlock;
			if (i + 1 < if_->numBranches || if_->elseBranch) {
				elseBlock = pushBlock(c, "if%u_else%u", ifIndex, i + 1);
			}

			Value cond = compileExpr(c, branch.cond);
			// TODO: Convert to bool

			pushInstA(c, branch.cond, O_Branch, cond.type, cond.ref);

			c->block->branchAst = branch.cond;
			c->block->blockTrue = bodyBlock;
			c->block->blockFalse = elseBlock;

			switchBlock(c, bodyBlock);

			compileStatement(c, branch.body);

			c->block->blockTrue = endBlock;
			switchBlock(c, elseBlock);
		}

		if (if_->elseBranch) {
			compileStatement(c, if_->elseBranch);
		}
	} break;

	case A_AstWhile: {
		AstWhile *while_ = (AstWhile*)ast;
		uint32_t whileIndex = ++c->whileCounter;

		uint32_t condBlock = pushBlock(c, "while%u_cond", whileIndex);
		uint32_t bodyBlock = pushBlock(c, "while%u_body", whileIndex);
		uint32_t endBlock = pushBlock(c, "while%u_end", whileIndex);

		c->block->blockTrue = condBlock;
		switchBlock(c, condBlock);

		Value cond = compileExpr(c, while_->cond);
		// TODO: Convert to bool
		pushInstA(c, while_->cond, O_Branch, cond.type, cond.ref);
		c->block->branchAst = while_->cond;
		c->block->blockTrue = bodyBlock;
		c->block->blockFalse = endBlock;

		switchBlock(c, bodyBlock);

		compileStatement(c, while_->body);

		c->block->blockTrue = condBlock;

		switchBlock(c, endBlock);
	} break;

	case A_AstVar: {
		AstVar *var = (AstVar*)ast;
		uint32_t index = c->func->locals.size;
		Local *local = buf_push_zero(&c->func->locals);
		local->name = var->name;
		local->type = (TypeRef){ 1 };
		symbolMapInsert(&c->scope->locals, var->name.symbol, index);

		if (var->init) {
			ValueRef dst = (ValueRef) { VK_Local, index };
			Value init = compileExpr(c, var->init);
			if (local->type.index) {
				init = compileImplicitCast(init, local->type);
			}
			pushInst1(c, ast, O_Assign, local->type, dst, init.ref);
		}

	} break;

	case A_AstReturn: {
		AstReturn *return_ = (AstReturn*)ast;
		if (return_->expr) {
			Value val = compileExpr(c, return_->expr);
			// TODO: Cast
			Inst *inst = pushInstA(c, ast, O_Return, val.type, val.ref);
			c->block->blockTrue = ~0u;

			uint32_t block = pushBlock(c, "return%u", ++c->returnCounter);
			switchBlock(c, block);
		}
	} break;

	default: {
		errorFmt(c, ast->span, "Internal: Invalid statement AST type");
	} break;

	}
}

static void compileFunc(Compiler *c, Func *func)
{
	c->func = func;

	c->andCounter = 0;
	c->orCounter = 0;
	c->ifCounter = 0;
	c->whileCounter = 0;
	c->returnCounter = 0;

	uint32_t block = pushBlock(c, "%s", getCString(func->name.symbol));
	switchBlock(c, block);

	compileStatement(c, &func->ast->body->ast);
}

Compiler *createCompiler(ErrorList *errors)
{
	Compiler *c = malloc(sizeof(Compiler));
	memset(c, 0, sizeof(Compiler));
	c->errorList = errors;
	return c;
}

void freeCompiler(Compiler *c)
{
	free(c);
}

int addCompileAst(Compiler *c, Ast *ast)
{
	if (c->failed) return 0;
	gatherToplevel(c, ast);
	return !c->failed;
}

int compile(Compiler *c, Module *result)
{
	if (c->failed) return 0;

	for (uint32_t i = 0; i < c->module.funcs.size; i++) {
		Func *func = &c->module.funcs.data[i];
		compileFunc(c, func);
	}

	*result = c->module;
	return 1;
}
