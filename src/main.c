#include "base.h"
#include "lexer.h"

#include <stdio.h>

int main(int argc, char **argv)
{
	const char *src = "def foo()";

	LexerInput input = {
		.source = src,
		.size = strlen(src),
		.filename = "test",
	};
	Lexer *l = createLexer(&input);

	for (;;) {
		Token tok = scan(l);
		SourceData sd = getSourceData(tok.span);

		printf("%u:%u: (%u) %s\n", sd.line, sd.col, tok.type, getCString(tok.symbol));

		if (tok.type == T_Error || tok.type == T_End)
			break;
	}

	return 0;
}
