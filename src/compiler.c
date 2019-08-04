#include "base.h"
#include "compiler.h"

#define RHMAP_INLINE static
#include "rhmap.h"

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

	uint32_t branchCounter;
	uint32_t andCounter, orCounter;
	uint32_t ifCounter, whileCounter;
	uint32_t returnCounter;

	uint32_t branchTrue, branchFalse;

	uint32_t blockIndex;
	Block *block;

	int failed;
};

static uint32_t byteHash(const void *data, size_t size)
{
	// TODO: Do something faster than FNV-1a...
	uint32_t hash = 2166136261u;
	for (size_t i = 0; i < size; i++) {
		hash = (hash ^ ((const char*)data)[i]) * 16777619u;
	}
	return hash;
}

size_t typeStructSize(Type *type)
{
	switch (type->kind) {
	case TK_TypeInteger: return sizeof(TypeInteger);
	case TK_TypePointer: return sizeof(TypePointer);
	case TK_TypeStruct: {
		TypeStruct *struct_ = (TypeStruct*)type;
		return sizeof(TypeStruct) * sizeof(FieldType) * struct_->numFields;
	}
	default:
		assert(0 && "Unhandled TypeKind");
		return 0;
	}
}

static uint32_t typeHash(Type *type)
{
	size_t size = typeStructSize(type) - sizeof(TypeInfo);
	return byteHash((char*)type + sizeof(TypeInfo), size);
}

static int typeEqual(Type *a, Type *b) {
	size_t sizeA = typeStructSize(a) - sizeof(TypeInfo);
	size_t sizeB = typeStructSize(b) - sizeof(TypeInfo);
	if (sizeA != sizeB) return 0;
	char *baseA = (char*)a + sizeof(TypeInfo);
	char *baseB = (char*)b + sizeof(TypeInfo);
	return !memcmp(baseA, baseB, sizeA);
}

static void typeMapGrow(Module *m)
{
	uint32_t minSize = m->numTypes;
	if (minSize < 32) minSize = 32;
	size_t count, allocSize;
	rhmap_grow(&m->typeMap, &count, &allocSize, minSize, 0.8);
	void *data = malloc(allocSize + sizeof(Type*) * count);
	Type **types = (Type**)((char*)data + allocSize);
	memcpy(types, m->types, sizeof(Type*) * m->numTypes);
	memset(types + m->numTypes, 0, (sizeof(Type*)) * (count - m->numTypes));
	m->types = types;
	void *oldData = rhmap_rehash(&m->typeMap, count, allocSize, data);
	free(oldData);
}

static TypeRef internType(Module *m, Type *type)
{
	if (m->numTypes >= m->typeMap.capacity) {
		typeMapGrow(m);
	}

	uint32_t hash = typeHash(type);
	rhmap_iter iter = { &m->typeMap, hash };
	uint32_t index;
	while (rhmap_find_inline(&iter, &index)) {
		if (typeEqual(m->types[index], type)) {
			return (TypeRef){ index };
		}
	}

	index = m->numTypes++;
	rhmap_insert_inline(&iter, index);

	size_t size = typeStructSize(type);
	Type *copy = arena_push_size_copy(&m->typeArena, size, type);
	m->types[index] = copy;

	return (TypeRef){ index };
}

static void internReservedType(Module *m, Type *type, TypeRef ref)
{
	if (m->numTypes >= m->typeMap.capacity) {
		typeMapGrow(m);
	}

	uint32_t hash = typeHash(type);
	rhmap_iter iter = { &m->typeMap, hash };
	rhmap_insert_inline(&iter, ref.index);

	size_t size = typeStructSize(type);
	m->types[ref.index] = type;
}

static TypeRef reserveType(Module *m)
{
	if (m->numTypes >= m->typeMap.capacity) {
		typeMapGrow(m);
	}

	return (TypeRef) { m->numTypes++ };
}

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
			global->constant = 1;
			symbolMapInsert(&c->globals, def->name.symbol, globalIndex);
		} break;

		case A_AstStruct: {
			AstStruct *struct_ = (AstStruct*)ast;
			uint32_t typeIndex = c->module.toplevelTypes.size;
			ToplevelType *type = buf_push_zero(&c->module.toplevelTypes);
			type->ast = ast;
			type->type = reserveType(&c->module);
			symbolMapInsert(&c->module.toplevelTypeNames, struct_->name.symbol, typeIndex);
		} break;

		case A_AstTypeDef: {
			AstTypeDef *typeDef = (AstTypeDef*)ast;
			uint32_t typeIndex = c->module.toplevelTypes.size;
			ToplevelType *type = buf_push_zero(&c->module.toplevelTypes);
			type->ast = ast;
			symbolMapInsert(&c->module.toplevelTypeNames, typeDef->name.symbol, typeIndex);
		} break;

		case A_AstVar: {
			AstVar *var = (AstVar*)ast;
			uint32_t globalIndex = c->module.globals.size;
			Global *global = buf_push_zero(&c->module.globals);
			global->name = var->name;
			global->ast = &var->ast;
			symbolMapInsert(&c->globals, var->name.symbol, globalIndex);
		} break;

		default:
			errorFmt(c, ast->span, "Internal: Unexpected toplevel AST type '%s'",
				getAstTypeName(ast->type));
			break;
		}
	}
}

static TypeRef resolveToplevelTypeDecl(Compiler *c, ToplevelType *type);

static TypeRef resolveToplevelType(Compiler *c, Ast *ast)
{
	switch (ast->type) {

	case A_AstIdent: {
		AstIdent *ident = (AstIdent*)ast;
		uint32_t toplevelIndex = symbolMapFind(&c->module.toplevelTypeNames, ident->name.symbol);
		if (toplevelIndex == ~0u) {
			errorFmt(c, ident->name.span, "Type name '%s' not found",
				getCString(ident->name.symbol));
			return (TypeRef){ 0 };
		}

		ToplevelType *toplevel = &c->module.toplevelTypes.data[toplevelIndex];
		return resolveToplevelTypeDecl(c, toplevel);
	} break;

	case A_AstTypePtr: {
		AstTypePtr *ptr = (AstTypePtr*)ast;
		TypePointer pointer = {
			.base = {
				.info = { .size = 8 },
				.kind = TK_TypePointer
			},
			.type = resolveToplevelType(c, ptr->type),
		};
		if (!pointer.type.index) return (TypeRef){ 0 };
		return internType(&c->module, &pointer.base);
	} break;

	default:
		errorFmt(c, ast->span, "Internal: Unexpected type AST type '%s'",
			getAstTypeName(ast->type));
		return (TypeRef){ 0 };
	}
}

static TypeRef resolveToplevelTypeDecl(Compiler *c, ToplevelType *type)
{
	if (type->type.index) {
		if (type->type.index == ~0u) {
			errorFmt(c, type->ast->span, "Recursive type declaration");
			return (TypeRef){ 0 };
		}
		return type->type;
	}
	assert(type->ast != NULL);
	type->type.index = ~0u;

	switch (type->ast->type) {

	case A_AstTypeDef: {
		AstTypeDef *typeDef = (AstTypeDef*)type->ast;
		type->type = resolveToplevelType(c, typeDef->init);
		return type->type;
	} break;

	default:
		assert(0 && "Unhandled toplevel type declaration AST type");
		return (TypeRef){ 0 };
	}
}

static void setupToplevelTypeDecl(Compiler *c, ToplevelType *toplevel)
{
	if (!toplevel->ast) return;
	switch (toplevel->ast->type) {

	case A_AstTypeDef:
		resolveToplevelTypeDecl(c, toplevel);
		break;

	case A_AstStruct: {
		AstStruct *ast = (AstStruct*)toplevel->ast;
		TypeStruct *type = arena_push_size_zero(&c->module.typeArena, sizeof(TypeStruct) + sizeof(FieldType) * ast->numFields);
		type->base.kind = TK_TypeStruct;
		type->base.info.name = ast->name.symbol;
		type->base.info.span = ast->name.span;
		type->numFields = ast->numFields;
		for (uint32_t i = 0; i < ast->numFields; i++) {
			type->fields[i].name = ast->fields[i].name.symbol;
			type->fields[i].span = ast->fields[i].name.span;
			type->fields[i].type = resolveToplevelType(c, ast->fields[i].type);
			if (c->failed) return;
		}
		internReservedType(&c->module, &type->base, toplevel->type);
	} break;

	default:
		assert(0 && "Unhandled toplevel type declaration AST type");
	}
}

static uint32_t resolveTypeSize(Compiler *c, Type *type)
{
	if (type->info.size == ~0u) {
		errorFmt(c, type->info.span, "Type '%s' has recursive size",
			getCString(type->info.name));
		return 0;
	}
	if (type->info.size) return type->info.size;
	type->info.size = ~0u;

	switch (type->kind) {

	case TK_TypeStruct: {
		TypeStruct *struct_ = (TypeStruct*)type;
		uint32_t size = 0;
		for (uint32_t i = 0; i < struct_->numFields; i++) {
			FieldType field = struct_->fields[i];
			size += resolveTypeSize(c, c->module.types[field.type.index]);
			if (c->failed) return 0;
		}
		struct_->base.info.size = size;
		return size;
	} break;

	default:
		assert(0 && "Type kind should have statically known size");
		return 0;
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

static void renameBlock(Compiler *c, uint32_t index, const char *fmt, ...)
{
	Block *block = &c->func->blocks.data[index];
	va_list args;
	va_start(args, fmt);
	vsnprintf(block->label, sizeof(block->label), fmt, args);
	va_end(args);
}

static void switchBlock(Compiler *c, uint32_t index)
{
	c->blockIndex = index;
	c->block = &c->func->blocks.data[index];
}

static void threadBlock(Compiler *c, uint32_t from, uint32_t to)
{
	assert(c->func->blocks.data[from].blockTrue == 0);
	c->func->blocks.data[from].blockTrue = to;
}

static void disableBlock(Compiler *c)
{
	c->blockIndex = ~0u;
	c->block = NULL;
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
	return (Value) { type, VK_Const, c->func->consts.size - 1 };
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

static void pushAssign(Compiler *c, Ast *ast, Value lhs, Value rhs) {
	// If `rhs` is a temporary result generated by the previous
	// instruction patch `lhs` as `dst`.
	Inst *prevInst = NULL;
	if (c->block->insts.size > 0) {
		prevInst = &c->block->insts.data[c->block->insts.size - 1];
	}
	if (rhs.ref.kind == VK_Temp && prevInst
		&& prevInst->dst.packed == rhs.ref.packed) {
		prevInst->dst = lhs.ref;
	} else {
		pushInst1(c, ast, O_Assign, lhs.type, lhs.ref, rhs.ref);
	}
}

static Value compileExpr(Compiler *c, Ast *ast)
{
	switch (ast->type) {

	case A_AstAssign: {
		AstAssign *assign = (AstAssign*)ast;
		Value lhs = compileExpr(c, assign->left);
		Value rhs = compileExpr(c, assign->right);
		rhs = compileImplicitCast(rhs, lhs.type);
		pushAssign(c, ast, lhs, rhs);
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
			return (Value) { global->type, VK_Global, globalIndex };
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
		Inst *inst = pushInst1(c, ast, O_Unary, expr.type, dst.ref, expr.ref);
		switch (unop->op.type) {
		case T_Sub: inst->ext = OU_Neg; break;
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
		case T_Equal:     op = O_Compare; ext = OC_Eq; break;
		case T_NotEqual:  op = O_Compare; ext = OC_Ne; break;
		case T_Less:      op = O_Compare; ext = OC_Lt; break;
		case T_LessEq:    op = O_Compare; ext = OC_Le; break;
		case T_Greater:   op = O_Compare; ext = OC_Gt; break;
		case T_GreaterEq: op = O_Compare; ext = OC_Ge; break;
		default:
			errorFmt(c, binop->op.span, "Internal: Invalid binary operator");
			return ErrorValue;
		}
		rhs = compileImplicitCast(rhs, lhs.type);
		Inst *inst = pushInst2(c, ast, op, lhs.type, dst.ref, lhs.ref, rhs.ref);
		inst->ext = ext;
		return dst;
	} break;

	case A_AstNot: {
		TypeRef boolType = { 2 };

		AstNot *not = (AstNot*)ast;
		Value expr = compileExpr(c, not->expr);

		uint32_t branchTrue, branchFalse;
		if (expr.ref.kind != VK_Branch) {
			pushInstA(c, not->expr, O_Branch, expr.type, expr.ref);
			uint32_t index = ++c->branchCounter;
			branchTrue = pushBlock(c, "branch%u_true", index);
			branchFalse = pushBlock(c, "branch%u_false", index);
			c->block->blockTrue = branchTrue;
			c->block->blockFalse = branchFalse;
		} else {
			branchTrue = c->branchTrue;
			branchFalse = c->branchFalse;
		}

		c->branchTrue = branchFalse;
		c->branchFalse = branchTrue;
		return (Value) { boolType, VK_Branch };
	} break;

	case A_AstLogic: {
		TypeRef boolType = { 2 };

		AstLogic *logic = (AstLogic*)ast;
		Value lhs = compileExpr(c, logic->left);

		uint32_t branchTrue, branchFalse;
		if (lhs.ref.kind != VK_Branch) {
			pushInstA(c, logic->left, O_Branch, lhs.type, lhs.ref);
			uint32_t index = ++c->branchCounter;
			branchTrue = pushBlock(c, "branch%u_true", index);
			branchFalse = pushBlock(c, "branch%u_false", index);
			c->block->blockTrue = branchTrue;
			c->block->blockFalse = branchFalse;
		} else {
			branchTrue = c->branchTrue;
			branchFalse = c->branchFalse;
		}

		if (logic->op.type == T_And) {
			switchBlock(c, branchTrue);
			Value rhs = compileExpr(c, logic->right);
			uint32_t blockTrue = pushBlock(c, "and%u_true", ++c->andCounter);
			if (rhs.ref.kind == VK_Branch) {
				threadBlock(c, c->branchTrue, blockTrue);
				threadBlock(c, c->branchFalse, branchFalse);
			} else {
				pushInstA(c, logic->right, O_Branch, rhs.type, rhs.ref);
				c->block->blockTrue = blockTrue;
				c->block->blockFalse = branchFalse;
			}
			branchTrue = blockTrue;
		} else if (logic->op.type == T_Or) {
			switchBlock(c, branchFalse);
			Value rhs = compileExpr(c, logic->right);
			uint32_t blockFalse = pushBlock(c, "or%u_false", ++c->orCounter);
			if (rhs.ref.kind == VK_Branch) {
				threadBlock(c, c->branchTrue, branchTrue);
				threadBlock(c, c->branchFalse, blockFalse);
			} else {
				pushInstA(c, logic->right, O_Branch, rhs.type, rhs.ref);
				c->block->blockTrue = branchTrue;
				c->block->blockFalse = blockFalse;
			}
			branchFalse = blockFalse;
		} else {
			errorFmt(c, logic->op.span, "Internal: Invalid logic operator");
			return ErrorValue;
		}

		c->branchTrue = branchTrue;
		c->branchFalse = branchFalse;
		disableBlock(c);
		return (Value) { boolType, VK_Branch };
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
			Value arg = (Value) { args.data[i].type, VK_Argument, i };
			pushAssign(c, ast, arg, args.data[i]);
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
		compileExpr(c, expr->expr);
	} break;

	case A_AstIf: {
		AstIf *if_ = (AstIf*)ast;
		uint32_t ifIndex = ++c->ifCounter;

		int endReferenced = 0;
		uint32_t endBlock = pushBlock(c, "if%u_end", ifIndex);
		for (uint32_t i = 0; i < if_->numBranches; i++) {
			IfBranch branch = if_->branches[i];

			Value cond = compileExpr(c, branch.cond);
			// TODO: Convert to bool

			uint32_t bodyBlock, elseBlock;
			if (cond.ref.kind == VK_Branch) {
				bodyBlock = c->branchTrue;
				renameBlock(c, bodyBlock, "if%u_body%u", ifIndex, i + 1);
				if (i + 1 < if_->numBranches || if_->elseBranch) {
					elseBlock = c->branchFalse;
					renameBlock(c, elseBlock, "if%u_else%u", ifIndex, i + 1);
				} else {
					threadBlock(c, elseBlock, endBlock);
					endReferenced = 1;
				}
			} else {
				pushInstA(c, branch.cond, O_Branch, cond.type, cond.ref);
				bodyBlock = pushBlock(c, "if%u_body%u", ifIndex, i + 1);
				if (i + 1 < if_->numBranches || if_->elseBranch) {
					elseBlock = pushBlock(c, "if%u_else%u", ifIndex, i + 1);
				} else {
					elseBlock = endBlock;
					endReferenced = 1;
				}
				c->block->branchAst = branch.cond;
				c->block->blockTrue = bodyBlock;
				c->block->blockFalse = elseBlock;
			}

			switchBlock(c, bodyBlock);

			compileStatement(c, branch.body);

			// If the 'if' body ends in an unreferenced block remove it.
			if (c->block->unreferenced && c->blockIndex == c->func->blocks.size - 1) {
				c->func->blocks.size--;
			} else {
				c->block->blockTrue = endBlock;
				endReferenced = 1;
			}
			switchBlock(c, elseBlock);
		}

		if (if_->elseBranch) {
			compileStatement(c, if_->elseBranch);

			// If the 'else' body ends in an unreferenced block remove it.
			if (c->block->unreferenced && c->blockIndex == c->func->blocks.size - 1) {
				c->func->blocks.size--;
			} else {
				c->block->blockTrue = endBlock;
				endReferenced = 1;
			}
		}

		switchBlock(c, endBlock);
		if (!endReferenced) c->block->unreferenced = 1;
	} break;

	case A_AstWhile: {
		AstWhile *while_ = (AstWhile*)ast;
		uint32_t whileIndex = ++c->whileCounter;

		uint32_t condBlock = pushBlock(c, "while%u_cond", whileIndex);

		c->block->blockTrue = condBlock;
		switchBlock(c, condBlock);

		Value cond = compileExpr(c, while_->cond);
		// TODO: Convert to bool
		uint32_t bodyBlock, endBlock;
		if (cond.ref.kind == VK_Branch) {
			bodyBlock = c->branchTrue;
			endBlock = c->branchFalse;
			renameBlock(c, bodyBlock, "while%u_body", whileIndex);
			renameBlock(c, endBlock, "while%u_end", whileIndex);
		} else {
			bodyBlock = pushBlock(c, "while%u_body", whileIndex);
			endBlock = pushBlock(c, "while%u_end", whileIndex);
			pushInstA(c, while_->cond, O_Branch, cond.type, cond.ref);
			c->block->branchAst = while_->cond;
			c->block->blockTrue = bodyBlock;
			c->block->blockFalse = endBlock;
		}

		switchBlock(c, bodyBlock);

		compileStatement(c, while_->body);

		// If the 'while' body ends in an unreferenced block remove it.
		if (c->block->unreferenced && c->blockIndex == c->func->blocks.size - 1) {
			c->func->blocks.size--;
		} else {
			c->block->blockTrue = condBlock;
		}

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
			Value dst = (Value) { local->type, VK_Local, index };
			Value init = compileExpr(c, var->init);
			if (local->type.index) {
				init = compileImplicitCast(init, local->type);
			}
			pushAssign(c, ast, dst, init);
		}

	} break;

	case A_AstReturn: {
		AstReturn *return_ = (AstReturn*)ast;
		if (return_->expr) {
			Value val = compileExpr(c, return_->expr);
			Value ret = (Value) { val.type, VK_Return };
			pushAssign(c, ast, ret, val);

			uint32_t block = pushBlock(c, "return%u", ++c->returnCounter);
			switchBlock(c, block);
			c->block->unreferenced = 1;
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

	c->branchCounter = 0;
	c->andCounter = 0;
	c->orCounter = 0;
	c->ifCounter = 0;
	c->whileCounter = 0;
	c->returnCounter = 0;

	uint64_t localScope[64];
	Scope scope = { .parent = c->scope };
	symbolMapInitLocal(&scope.locals, localScope, sizeof(localScope));
	c->scope = &scope;

	uint32_t block = pushBlock(c, "%s", getCString(func->name.symbol));
	switchBlock(c, block);

	for (uint32_t i = 0; i < func->ast->numParams; i++) {
		DeclAst decl = func->ast->params[i];

		uint32_t index = c->func->locals.size;
		Local *local = buf_push_zero(&c->func->locals);
		local->name = decl.name;
		local->type = (TypeRef){ 1 };
		symbolMapInsert(&c->scope->locals, decl.name.symbol, index);
	}

	compileStatement(c, &func->ast->body->ast);

	c->scope = scope.parent;

	// If the function body ends in an unreferenced block remove it.
	if (c->block->unreferenced && c->blockIndex == c->func->blocks.size - 1) {
		c->func->blocks.size--;
	}
}

static void pushInternalType(Compiler *c, const char *name, TypeKind kind, void *typeVoid)
{
	Symbol symbol = internZ(name);
	Type *type = (Type*)typeVoid;
	type->kind = kind;
	type->info.name = symbol;

	TypeRef ref = internType(&c->module, type);
	symbolMapInsert(&c->module.toplevelTypeNames, symbol, c->module.toplevelTypes.size);
	ToplevelType *topType = buf_push_uninit(&c->module.toplevelTypes);
	topType->ast = NULL;
	topType->type = ref;
}

Compiler *createCompiler(ErrorList *errors)
{
	Compiler *c = malloc(sizeof(Compiler));
	memset(c, 0, sizeof(Compiler));
	c->errorList = errors;

	// Reserve NULL type
	TypeRef nullType = reserveType(&c->module);
	assert(nullType.index == 0);

	pushInternalType(c, "Int", TK_TypeInteger, &(TypeInteger) {
		.base = { .info = { .size = 4 } },
		.hasSign = 1,
		.bits = 32,
	});

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

	for (uint32_t i = 0; i < c->module.toplevelTypes.size; i++) {
		setupToplevelTypeDecl(c, &c->module.toplevelTypes.data[i]);
	}

	if (c->failed) return 0;

	for (uint32_t i = 0; i < c->module.toplevelTypes.size; i++) {
		ToplevelType *toplevel = &c->module.toplevelTypes.data[i];
		resolveTypeSize(c, c->module.types[toplevel->type.index]);
	}

	if (c->failed) return 0;

	for (uint32_t i = 0; i < c->module.funcs.size; i++) {
		Func *func = &c->module.funcs.data[i];
		compileFunc(c, func);
	}

	*result = c->module;
	return !c->failed;
}

static void dumpValueRef(Module *module, Func *func, ValueRef ref)
{
	switch (ref.kind) {
	case VK_Error: printf("(error)"); break;
	case VK_Local: printf("%s", getCString(func->locals.data[ref.index].name.symbol)); break;
	case VK_Temp: printf("t%u", ref.index); break;
	case VK_Global: printf("%s", getCString(module->globals.data[ref.index].name.symbol)); break;
	case VK_LocalRef: printf("*%s", getCString(func->locals.data[ref.index].name.symbol)); break;
	case VK_TempRef: printf("*t%u", ref.index); break;
	case VK_GlobalRef: printf("*%s", getCString(module->globals.data[ref.index].name.symbol)); break;
	case VK_Const: printf("const:%u", ref.index); break;
	case VK_Branch: printf("(branch)"); break;
	case VK_Argument: printf("arg%u", ref.index); break;
	case VK_Return: printf("return%u", ref.index); break;
	default: assert(0 && "Unhandled ref kind");
	}
}

void dumpFunc(Module *module, Func *func)
{
	for (uint32_t blockI = 0; blockI < func->blocks.size; blockI++) {
		Block *block = &func->blocks.data[blockI];

		uint32_t prevLine = 0;

		printf("%s:\n", block->label);
		for (uint32_t i = 0; i < block->insts.size; i++) {
			Inst inst = block->insts.data[i];
			SourceData data = getSourceData(inst.ast->span);
			if (data.line != prevLine) {
				const char *source;
				size_t length;
				if (getSourceLine(inst.ast->span.file, data.line, &source, &length)) {
					while (length > 0 && (*source == ' ' || *source == '\t')) {
						source++;
						length--;
					}
					printf("  // %.*s\n", (int)length, source);
				}
				prevLine = data.line;
			}

			printf("  ");

			switch (inst.op) {
			case O_Assign:
				dumpValueRef(module, func, inst.dst);
				printf(" = ");
				dumpValueRef(module, func, inst.a);
				break;
			case O_Unary:
				dumpValueRef(module, func, inst.dst);
				printf(" = ");
				switch (inst.ext) {
				case OU_Neg: printf("- "); break;
				}
				dumpValueRef(module, func, inst.a);
				break;
			case O_Arith:
				dumpValueRef(module, func, inst.dst);
				printf(" = ");
				dumpValueRef(module, func, inst.a);
				switch (inst.ext) {
				case OA_Add: printf(" + "); break;
				case OA_Sub: printf(" - "); break;
				case OA_Mul: printf(" * "); break;
				case OA_Div: printf(" / "); break;
				case OA_Mod: printf(" %% "); break;
				}
				dumpValueRef(module, func, inst.b);
				break;
			case O_Compare:
				dumpValueRef(module, func, inst.dst);
				printf(" = ");
				dumpValueRef(module, func, inst.a);
				switch (inst.ext) {
				case OC_Eq: printf(" == "); break;
				case OC_Ne: printf(" != "); break;
				case OC_Lt: printf(" < "); break;
				case OC_Le: printf(" >= "); break;
				case OC_Gt: printf(" > "); break;
				case OC_Ge: printf(" >= "); break;
				}
				dumpValueRef(module, func, inst.b);
				break;
			case O_Call:
				dumpValueRef(module, func, inst.dst);
				printf(" = ");
				dumpValueRef(module, func, inst.a);
				printf("()");
				break;
			case O_Branch:
				printf("if (");
				dumpValueRef(module, func, inst.a);
				printf(")");
				break;
			default:
				assert(0 && "Unexpected inst type");
			}

			printf("\n");
		}

		if (block->blockFalse != 0) {
			printf("  goto %s else %s\n",
				func->blocks.data[block->blockTrue].label,
				func->blocks.data[block->blockFalse].label);
		} else if (block->blockTrue != 0) {
			printf("  goto %s\n",
				func->blocks.data[block->blockTrue].label);
		} else {
			printf("  return\n");
		}
	}
}

void dumpModule(Module *module)
{
	for (uint32_t i = 0; i < module->funcs.size; i++) {
		dumpFunc(module, &module->funcs.data[i]);
		printf("\n");
	}
}
