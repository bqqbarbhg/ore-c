#include "base.h"
#include "lexer.h"

typedef struct {
	const char *filename;
	const char *data;
	size_t size;
	uint32_t *lineBreaks;
	uint32_t numLineBreaks;
} InputFile;

InputFile *g_inputFiles;
uint32_t g_numInputFiles;

typedef struct {
	uint8_t symbol;
	uint8_t length;
} TokenInfo;

struct Lexer {
	SymbolMap keywordMap;
	uint32_t fileIndex;

	TokenInfo tokenInfo[TokenType_Amount];

	const char *begin, *ptr, *end;
};

static void addToken(Lexer *l, TokenType type, const char *str)
{
	Symbol sym = internZ(str);
	l->tokenInfo[type].symbol = sym.index;
	l->tokenInfo[type].length = (uint8_t)strlen(str);
}

static void addKeyword(Lexer *l, TokenType type, const char *str)
{
	Symbol sym = internZ(str);
	symbolMapInsert(&l->keywordMap, sym, type);
	l->tokenInfo[type].symbol = sym.index;
	l->tokenInfo[type].length = (uint8_t)strlen(str);
}

Lexer *createLexer(const LexerInput *input)
{
	Lexer *l = malloc(sizeof(Lexer));
	memset(l, 0, sizeof(Lexer));

	l->fileIndex = g_numInputFiles;
	InputFile *file = bufPush(g_inputFiles, g_numInputFiles);
	file->filename = input->filename;
	file->data = input->source;
	file->size = input->size;

	addToken(l, T_Newline, "\n");

	addKeyword(l, KW_Def, "def");
	addKeyword(l, KW_Struct, "struct");

	TokenType kw = symbolMapFind(&l->keywordMap, internZ("struct"));
}

static void addLineBreak(Lexer *l, const char *ptr)
{
	InputFile *file = &g_inputFiles[l->fileIndex];
	uint32_t *offset = bufPush(file->lineBreaks, file->numLineBreaks);
	*offset = ptr - l->begin;
}

Token scan(Lexer *l)
{
	const char *ptr = l->ptr, *end = l->end;
	const char *begin = ptr;
	size_t left = end - ptr;
	char c = left > 0 ? ptr[0] : '\0';
	char next = left > 1 ? ptr[1] : '\0';

	TokenType type;
	Token token;
	token.span.file = l->fileIndex;	
	token.span.offset = ptr - l->begin;

	switch (c) {
	case '\n':
		addLineBreak(l, ptr);
		type = T_Newline;
		break;

	default: {
		uint32_t hash = symbolHashInit();
		do {
			hash = symbolHashStep(hash, c);
			c = *ptr++;
		} while (ptr != end);
	} break;

	}

	TokenInfo info = l->tokenInfo[type];
	l->ptr = ptr + info.length;
	token.type = type;
	token.symbol.index = info.symbol;
	token.span.length = info.length;

}

SourceData getSourceData(SourceSpan span)
{
	SourceData data = { 0 };
	InputFile *file = &g_inputFiles[span.file];

	uint32_t offset = span.offset;

	return data;
}
