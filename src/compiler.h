#pragma once

#include "base.h"
#include "parser.h"
#include "error.h"

typedef struct {
	int todo;
} Type;

typedef struct {
	uint32_t index;
} TypeRef;

typedef enum {
	O_Assign,  // dst = a
	O_Arith,   // dst = a <ext> b  (+ - * /)
	O_Unary,   // dst = <ext> a    (-)
	O_Compare, // dst = a <ext> b  (== != < <= > >=)
	O_Arg,     // args[ext] = a
	O_Call,    // dst = a(args)
	O_Return,  // result = a
	O_Branch,  // if (a)
} Op;

typedef enum {
	OA_Add, // dst = a + b
	OA_Sub, // dst = a - b
	OA_Mul, // dst = a * b
	OA_Div, // dst = a / b
	OA_Mod, // dst = a % b
} OpArith;

typedef enum {
	OU_Neg, // dst = -a
} OpUnary;

typedef enum {
	OC_Eq, // dst = a == b
	OC_Ne, // dst = a != b
	OC_Lt, // dst = a < b
	OC_Le, // dst = a <= b
	OC_Gt, // dst = a > b
	OC_Ge, // dst = a >= b
} OpCompare;

typedef enum {
	VK_Error,
	VK_Local,
	VK_Temp,
	VK_Global,
	VK_LocalRef,
	VK_TempRef,
	VK_GlobalRef,
	VK_Const,
	VK_Block,
} ValueKind;

typedef struct {
	union {
		struct {
			ValueKind kind : 5;
			uint32_t index : 27;
		};
		uint32_t packed;
	};
} ValueRef;

typedef struct {
	TypeRef type;
	ValueRef ref;
} Value;

#define ErrorValue ((Value) { VK_Error })

typedef buf_type(Value) Value_buf;

typedef struct {
	Op op;
	TypeRef type;
	Ast *ast;

	ValueRef dst;
	ValueRef a, b;
	uint32_t ext;
} Inst;

typedef buf_type(Inst) Inst_buf;

typedef struct {
	char label[32];

	Ast *branchAst;
	uint32_t blockTrue, blockFalse;

	Inst_buf insts;
} Block;

typedef buf_type(Block) Block_buf;

typedef struct {
	Token name;
	TypeRef type;
} Local;

typedef buf_type(Local) Local_buf;

typedef struct {
	TypeRef type;
} Temp;

typedef buf_type(Temp) Temp_buf;

typedef struct {
	TypeRef type;
} Const;

typedef buf_type(Const) Const_buf;

typedef struct {
	Token name;
	AstDef *ast;

	Temp_buf temps;
	Const_buf consts;
	Local_buf locals;
	Block_buf blocks;
} Func;

typedef buf_type(Func) Func_buf;

typedef struct {
	Ast *ast;
	Token name;
	TypeRef type;
	int constant;
} Global;

typedef buf_type(Global) Global_buf;

typedef struct {
	Global_buf globals;
	Func_buf funcs;
} Module;

typedef struct Compiler_s Compiler;

Compiler *createCompiler(ErrorList *errors);
void freeCompiler(Compiler *c);

int addCompileAst(Compiler *c, Ast *ast);
int compile(Compiler *c, Module *result);

void dumpFunc(Module *module, Func *func);
void dumpModule(Module *module);
