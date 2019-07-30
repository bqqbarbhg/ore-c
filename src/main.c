#define _CRT_SECURE_NO_WARNINGS
#include "base.h"
#include "lexer.h"

#include <stdio.h>

char *readFile(const char *filename, size_t *outSize)
{
	FILE *f = fopen(filename, "rb");
	fseek(f, 0, SEEK_END);
	size_t size = ftell(f);
	*outSize = size;
	char *data = malloc(size);
	fseek(f, 0, SEEK_SET);
	fread(data, 1, size, f);
	fclose(f);
	return data;
}

int main(int argc, char **argv)
{
	if (argc <= 1) {
		fprintf(stderr, "Usage: ore-c <file>\n");
		return 1;
	}

	const char *filename = argv[1];

	size_t size;
	char *src = readFile(filename, &size);

	LexerInput input = {
		.source = src,
		.size = size,
		.filename = filename,
	};
	Lexer *l = createLexer(&input);

	Token tok;
	do {
		tok = scan(l);
		SourceData sd = getSourceData(tok.span);

		char line[64];
		sprintf(line, "%u:%u:", sd.line, sd.col);

		if (tok.type != T_Newline) {
			printf("%10s (%02x) %s\n", line, tok.type, getCString(tok.symbol));
		} else {
			printf("%10s (%02x) \\n\n", line, tok.type);
		}

		if (tok.type == T_Error) {
			fprintf(stderr, "%s:%u:%u: Error: Bad token\n",
				sd.filename, sd.line, sd.col);
			return 1;
		}

	} while (tok.type != T_End);

	return 0;
}

