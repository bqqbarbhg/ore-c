#pragma once

#include "base.h"
#include "parser.h"
#include "error.h"

typedef struct {
	uint32_t index;
} TypeRef;

typedef enum {
	TK_TypeInteger,
	TK_TypePointer,
	TK_TypeStruct,
} TypeKind;

// Parts of the type that are not compared
typedef struct {
	Symbol name;
	SourceSpan span;
	uint32_t size;
} TypeInfo;

typedef struct Type_s {
	TypeInfo info;
	TypeKind kind;
} Type;

typedef struct TypeInteger_s {
	Type base;
	unsigned hasSign : 1;
	unsigned bits : 7;
} TypeInteger;

typedef struct TypePointer_s {
	Type base;
	TypeRef type;
} TypePointer;

typedef struct {
	Symbol name;
	SourceSpan span;
	TypeRef type;
} FieldType;

typedef struct TypeStruct_s {
	Type base;
	uint32_t numFields;
	FieldType fields[];
} TypeStruct;

typedef struct {
	Ast *ast;
	TypeRef type;
} ToplevelType;

typedef buf_type(ToplevelType) ToplevelType_buf;

typedef enum {
	O_Assign,  // dst = a
	O_Arith,   // dst = a <ext> b  (+ - * /)
	O_Unary,   // dst = <ext> a    (-)
	O_Compare, // dst = a <ext> b  (== != < <= > >=)
	O_Call,    // dst = a(args)
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
	VK_Branch,
	VK_Argument,
	VK_Return,
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

#define ErrorValue ((Value) { 0, VK_Error })

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
	char label[16];

	uint8_t unreferenced;

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
	ToplevelType_buf toplevelTypes;
	SymbolMap toplevelTypeNames;

	arena typeArena;
	rhmap typeMap;
	uint32_t numTypes;
	Type **types;

} Module;

typedef struct Compiler_s Compiler;

Compiler *createCompiler(ErrorList *errors);
void freeCompiler(Compiler *c);

int addCompileAst(Compiler *c, Ast *ast);
int compile(Compiler *c, Module *result);

void dumpFunc(Module *module, Func *func);
void dumpModule(Module *module);
