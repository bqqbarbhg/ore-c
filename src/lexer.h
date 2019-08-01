#pragma once

typedef enum {

	T_Error = 0,
	T_End,

	T_Newline,
	T_Identifier,
	T_String,
	T_Number,

	T_ParenOpen, T_ParenClose,
	T_BlockOpen, T_BlockClose,
	T_IndexOpen, T_IndexClose,
	T_AngleOpen, T_AngleClose,

	T_Colon,
	T_Comma,

	T_Add, T_Sub, T_Mul, T_Div, T_Mod,

	T_Assign,
	T_Equal,
	T_NotEqual,
	T_LessEq,
	T_GreaterEq,

	T_Not, T_Or, T_And,

	KW_Def,
	KW_If,
	KW_Else,
	KW_While,
	KW_Return,
	KW_Var,

	TokenType_Amount,

	// Aliases
	T_Less = T_AngleOpen,
	T_Greater = T_AngleClose,

} TokenType;

typedef struct {
	uint32_t offset;
	uint16_t file;
	uint16_t length;
} SourceSpan;

typedef struct {
	const char *filename;
	uint32_t line, col, offset;
	const char *data;
	uint32_t length;
} SourceData;

typedef struct {
	TokenType type;
	Symbol symbol;
	SourceSpan span;
} Token;

typedef struct Lexer Lexer;

typedef struct {
	const char *source;
	size_t size;
	const char *filename;
	struct ErrorList_s *errorList;
} LexerInput;

Lexer *createLexer(const LexerInput *input);
void freeLexer(Lexer *l);

Token scan(Lexer *l);

SourceData getSourceData(SourceSpan span);
int getSourceLine(uint32_t fileIndex, uint32_t line, const char **ptr, size_t *length);
