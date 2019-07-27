#pragma once

typedef enum {

	T_Error = 0,
	T_End,

	T_Newline,
	T_Identifier,

	KW_Def,
	KW_Struct,

	TokenType_Amount,

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
} LexerInput;

Lexer *createLexer(const LexerInput *input);

Token scan(Lexer *l);

SourceData getSourceData(SourceSpan span);
