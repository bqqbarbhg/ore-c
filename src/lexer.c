#include "base.h"
#include "lexer.h"
#include "bsearch.h"

typedef struct {
	const char *filename;
	const char *data;
	size_t size;
	u32_buf lineBreaks;
} InputFile;

typedef buf_type(InputFile) InputFile_buf;
InputFile_buf g_inputFiles;

typedef struct {
	uint8_t symbol;
	uint8_t length;
} TokenInfo;

struct Lexer {
	uint32_t fileIndex;

	const char *begin, *ptr, *end;
};

typedef struct {
	int initialized;
	SymbolMap keywordMap;
	TokenInfo tokenInfo[TokenType_Amount];
} LexerRules;

static LexerRules g_rules;

static void addToken(LexerRules *lr, TokenType type, const char *str)
{
	Symbol sym = internZ(str);
	lr->tokenInfo[type].symbol = sym.index;
	lr->tokenInfo[type].length = (uint8_t)strlen(str);
}

static void addKeyword(LexerRules *lr, TokenType type, const char *str)
{
	Symbol sym = internZ(str);
	symbolMapInsert(&lr->keywordMap, sym, type);
	lr->tokenInfo[type].symbol = sym.index;
	lr->tokenInfo[type].length = (uint8_t)strlen(str);
}

Lexer *createLexer(const LexerInput *input)
{
	Lexer *l = malloc(sizeof(Lexer));
	memset(l, 0, sizeof(Lexer));

	l->fileIndex = g_inputFiles.size;
	InputFile *file = buf_push_zero(&g_inputFiles);
	file->filename = input->filename;
	file->data = input->source;
	file->size = input->size;

	buf_push(&file->lineBreaks, &(uint32_t) { 0 });

	l->begin = l->ptr = input->source;
	l->end = l->ptr + input->size;

	if (!g_rules.initialized) {
		g_rules.initialized = 1;
		LexerRules *lr = &g_rules;

		addToken(lr, T_Newline, "\n");

		addToken(lr, T_ParenOpen, "(");
		addToken(lr, T_ParenClose, ")");
		addToken(lr, T_BlockOpen, "{");
		addToken(lr, T_BlockClose, "}");
		addToken(lr, T_IndexOpen, "[");
		addToken(lr, T_IndexClose, "]");
		addToken(lr, T_AngleOpen, "<");
		addToken(lr, T_AngleClose, ">");

		addToken(lr, T_Colon, ":");
		addToken(lr, T_Comma, ",");
		addToken(lr, T_Dot, ",");

		addToken(lr, T_Add, "+");
		addToken(lr, T_Sub, "-");
		addToken(lr, T_Mul, "*");
		addToken(lr, T_Div, "/");
		addToken(lr, T_Mod, "%");

		addToken(lr, T_Assign , "=");
		addToken(lr, T_Equal , "==");
		addToken(lr, T_NotEqual , "!=");
		addToken(lr, T_LessEq, "<=");
		addToken(lr, T_GreaterEq, ">=");

		addToken(lr, T_Not, "!");
		addToken(lr, T_Or, "||");
		addToken(lr, T_And, "&&");

		addKeyword(lr, KW_Def, "def");
		addKeyword(lr, KW_If, "if");
		addKeyword(lr, KW_Else, "else");
		addKeyword(lr, KW_While, "while");
		addKeyword(lr, KW_Return, "return");
		addKeyword(lr, KW_Var, "var");
		addKeyword(lr, KW_Struct, "struct");
	}

	return l;
}

void freeLexer(Lexer *l)
{
	free(l);
}

static void addLineBreak(Lexer *l, const char *ptr)
{
	InputFile *file = &g_inputFiles.data[l->fileIndex];
	uint32_t offset = ptr - l->begin;
	buf_push(&file->lineBreaks, &offset);
}

typedef struct {
	uint32_t lo, hi;
} CodepointRange;

static const CodepointRange identInitRanges[] = {
	0x000a8,0x000a8, 0x000aa,0x000aa, 0x000ad,0x000ad, 0x000af,0x000af, 
	0x000b2,0x000b5, 0x000b7,0x000ba, 0x000bc,0x000be, 0x000c0,0x000d6, 
	0x000d8,0x000f6, 0x000f8,0x000ff, 0x00100,0x0167f, 0x01681,0x0180d, 
	0x0180f,0x01fff, 0x0200b,0x0200d, 0x0202a,0x0202e, 0x0203f,0x02040, 
	0x02054,0x02054, 0x02060,0x0206f, 0x02070,0x0218f, 0x02460,0x024ff, 
	0x02776,0x02793, 0x02c00,0x02dff, 0x02e80,0x02fff, 0x03004,0x03007, 
	0x03021,0x0302f, 0x03031,0x0d7ff, 0x0f900,0x0fd3d, 0x0fd40,0x0fdcf, 
	0x0fdf0,0x0fe44, 0x0fe47,0x0fffd, 0x10000,0x1fffd, 0x20000,0x2fffd, 
	0x30000,0x3fffd, 0x40000,0x4fffd, 0x50000,0x5fffd, 0x60000,0x6fffd, 
	0x70000,0x7fffd, 0x80000,0x8fffd, 0x90000,0x9fffd, 0xa0000,0xafffd, 
	0xb0000,0xbfffd, 0xc0000,0xcfffd, 0xd0000,0xdfffd, 0xe0000,0xefffd,
};

static const CodepointRange identContRanges[] = {
	0x00300,0x0036f, 0x01dc0,0x01dff, 0x020d0,0x020ff, 0x0fe20,0x0fe2f,
};

static int codepointInRange(uint32_t c, const CodepointRange *ranges, size_t count)
{
	bsearch_range range = { 0, count };
	size_t i;
	while (bsearch_next_until(&range, &i, 16)) {
		bsearch_step(&range, ranges[i].hi >= c);
	}
	for (i = range.lo; i != range.hi; i++) {
		if (ranges[i].hi >= c) {
			return ranges[i].lo <= c;
		}
	}
	return 0;
}

static uint32_t decodeUtf8(const char **ptr, const char *end, uint32_t *hash)
{
	const char *p = *ptr;
	size_t left = end - p;
	if (left >= 1 && p[0] <= 0x7f) {
		*ptr = p + 1;
		*hash = symbolHashStep(*hash, p[0]);
		return p[0];
	} else if (left >= 2 && (p[0] & 0xe0) == 0xc0) {
		*ptr = p + 2;
		*hash = symbolHashStep(symbolHashStep(*hash, p[0]), p[1]);
		return ((uint32_t)p[0] & 0x1f) << 6 | ((uint32_t)p[1] & 0x3f);
	} else if (left >= 3 && (p[0] & 0xf0) == 0xe0) {
		*ptr = p + 3;
		*hash = symbolHashStep(symbolHashStep(symbolHashStep(*hash, p[0]), p[1]), p[2]);
		return ((uint32_t)p[0] & 0x0f) << 12 | ((uint32_t)p[1] & 0x3f) << 6
			| ((uint32_t)p[2] & 0x3f);
	} else if (left >= 4 && (p[0] & 0xf8) == 0xf0) {
		*ptr = p + 4;
		*hash = symbolHashStep(symbolHashStep(symbolHashStep(symbolHashStep(*hash, p[0]), p[1]), p[2]), p[3]);
		return ((uint32_t)p[0] & 0x0f) << 18 | ((uint32_t)p[1] & 0x3f) << 12
			| ((uint32_t)p[2] & 0x3f) << 6 | ((uint32_t)p[3] & 0x3f);
	} else {
		return ~0u;
	}
}

static int eatIdentInit(const char **ptr, const char *end, uint32_t *hash)
{
	const char *p = *ptr;
	if (p != end && p[0] <= 0x7f) {
		char c = p[0];
		*hash = symbolHashStep(*hash, c);
		*ptr = p + 1;
		return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_';
	} else {
		uint32_t c = decodeUtf8(ptr, end, hash);
		return codepointInRange(c, identInitRanges, arraySize(identInitRanges));
	}
}

static int eatIdentCont(const char **ptr, const char *end, uint32_t *hash)
{
	const char *p = *ptr;
	if (p != end && p[0] <= 0x7f) {
		char c = p[0];
		*hash = symbolHashStep(*hash, c);
		*ptr = p + 1;
		return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '_';
	} else {
		uint32_t c = decodeUtf8(ptr, end, hash);
		return codepointInRange(c, identInitRanges, arraySize(identInitRanges))
			|| codepointInRange(c, identContRanges, arraySize(identContRanges));
	}
}

static int isWhitespace(char c)
{
	return c == ' ' || c == '\t' || c == '\r';
}

Token scan(Lexer *l)
{
	const char *ptr = l->ptr, *end = l->end;
	while (ptr != end && isWhitespace(*ptr)) ++ptr;

	const char *begin = ptr;
	size_t left = end - ptr;
	char c = left > 0 ? ptr[0] : '\0';
	char next = left > 1 ? ptr[1] : '\0';

	TokenType type;
	Token token;
	token.span.file = l->fileIndex;	
	token.span.offset = ptr - l->begin;
	token.symbol.index = 0;

	switch (c) {

	case '\0':
		type = T_End;
		break;

	case '(': type = T_ParenOpen; break;
	case ')': type = T_ParenClose; break;
	case '{': type = T_BlockOpen; break;
	case '}': type = T_BlockClose; break;
	case '[': type = T_IndexOpen; break;
	case ']': type = T_IndexClose; break;
	case '<':
		if (next == '=') type = T_LessEq;
		else type = T_AngleOpen;
		break;
	case '>':
		if (next == '=') type = T_GreaterEq;
		else type = T_AngleClose;
		break;

	case ':': type = T_Colon; break;
	case ',': type = T_Comma; break;
	case '.': type = T_Dot; break;

	case '+': type = T_Add; break;
	case '-': type = T_Sub; break;
	case '*': type = T_Mul; break;
	case '/': type = T_Div; break;
	case '%': type = T_Mod; break;

	case '|':
		if (next == '|') type = T_Or;
		break;
	case '&':
		if (next == '&') type = T_And;
		break;

	case '!':
		if (next == '=') type = T_NotEqual;
		else type = T_Not;
		break;

	case '=':
		if (next == '=') type = T_Equal;
		else type = T_Assign;
		break;

	case '\n':
		addLineBreak(l, ptr);
		type = T_Newline;
		break;

	case '"': {
		uint32_t hash = symbolHashInit();
		hash = symbolHashStep(hash, '"');
		type = T_String;
		ptr++;
		while (ptr != end && *ptr != '"') {
			hash = symbolHashStep(hash, *ptr);
			if (*ptr == '\\') {
				ptr++;
				if (ptr != end) {
					hash = symbolHashStep(hash, *ptr);
				} else {
					type = T_Error;
					break;
				}
			}
			ptr++;
		}
		if (ptr != end) {
			hash = symbolHashStep(hash, '"');
			ptr++;
			token.symbol = internHash(begin, ptr - begin, hash);
		} else {
			type = T_Error;
		}
	  } break;

	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9': {
		uint32_t hash = symbolHashInit();
		while (ptr != end && *ptr >= '0' && *ptr <= '9') {
			hash = symbolHashStep(hash, *ptr);
			ptr++;
		}
		token.symbol = internHash(begin, ptr - begin, hash);
		type = T_Number;
	} break;

	default: {
		uint32_t hash = symbolHashInit();
		const char *next = ptr;
		uint32_t nextHash = hash;
		if (eatIdentInit(&next, end, &nextHash)) {
			ptr = next;
			hash = nextHash;
			type = T_Identifier;
			while (eatIdentCont(&next, end, &nextHash)) {
				ptr = next;
				hash = nextHash;
			}
			Symbol symbol = internHash(begin, ptr - begin, hash);
			token.symbol = symbol;
			uint32_t keyword = symbolMapFind(&g_rules.keywordMap, symbol);
			if (keyword != ~0u) {
				type = keyword;
			}
		} else {
			type = T_Error;
		}
	} break;

	}

	token.type = type;
	if (token.symbol.index == 0) {
		TokenInfo info = g_rules.tokenInfo[type];
		l->ptr = ptr + info.length;
		token.symbol.index = info.symbol;
		token.span.length = info.length;
	} else {
		l->ptr = ptr;
		token.span.length = ptr - begin;
	}

	return token;
}

SourceData getSourceData(SourceSpan span)
{
	InputFile *file = &g_inputFiles.data[span.file];
	SourceData data = {
		.filename = file->filename,
		.data = file->data + span.offset,
		.length = span.length,
		.offset = span.offset,
	};

	uint32_t offset = span.offset;

	size_t line;
	bsearch_range range = { 0, file->lineBreaks.size };
	while (bsearch_next(&range, &line)) {
		bsearch_step(&range, file->lineBreaks.data[line] >= offset);
	}
	uint32_t lineBegin = 0;
	if (line > 0) {
		lineBegin = file->lineBreaks.data[line - 1];
	}

	// Rewind to first non-newline character for column
	uint32_t endOffset = 0;
	if (file->size > 0) {
		const char *src = file->data;
		endOffset = offset < file->size ? offset : file->size - 1;
		while (endOffset > lineBegin) {
			char c = src[endOffset];
			if (c != '\n' && c != '\r')
				break;
			endOffset--;
		}
	}

	data.line = (uint32_t)line;
	data.col = endOffset - lineBegin;

	return data;
}

int getSourceLine(uint32_t fileIndex, uint32_t line, const char **ptr, size_t *length)
{
	InputFile *file = &g_inputFiles.data[fileIndex];
	uint32_t lineIndex = line - 1;
	if (lineIndex >= file->lineBreaks.size) return 0;

	uint32_t begin = file->lineBreaks.data[lineIndex] + 1;
	uint32_t end = file->size;
	if (lineIndex + 1 < file->lineBreaks.size) {
		end = file->lineBreaks.data[lineIndex + 1];
		// Rewind to first non-newline character for column
		while (end > begin) {
			char c = file->data[end - 1];
			if (c != '\n' && c != '\r')
				break;
			end--;
		}
	}

	if (begin > end) return 0;

	*ptr = file->data + begin;
	*length = end - begin;
	return 1;
}
