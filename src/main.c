#define _CRT_SECURE_NO_WARNINGS
#include "base.h"
#include "lexer.h"
#include "parser.h"
#include "error.h"

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

typedef struct {
	AstDumper dumper;
	FILE *file;
	uint32_t level;
} HtmlDumper;

void printStdio(AstDumper *d, const char *str)
{
	printf("%s", str);
}

const char *htmlHoverColors[] = {
	"#FDDFDF",
	"#DEFDE0",
	"#F0DEFD",
	"#FCF7DE",
	"#DEF3FD",
};

int dumpHtmlPre(AstDumper *ad, Ast *ast)
{
	HtmlDumper *d = (HtmlDumper*)ad;
	const char *name = getAstTypeName(ast->type);
	if (d->level > 0) {
		fprintf(d->file, "<span class=\"ast-%u\" title=\"%s\">",
			d->level % arraySize(htmlHoverColors), name);
	} else {
		fprintf(d->file, "<span>");
	}
	d->level++;
	return 1;
}

void dumpHtmlPost(AstDumper *ad, Ast *ast)
{
	HtmlDumper *d = (HtmlDumper*)ad;
	fprintf(d->file, "</span>");
	d->level--;
}

int dumpHtmlPrint(AstDumper *ad, const char *str)
{
	HtmlDumper *d = (HtmlDumper*)ad;
	for (const char *s = str; *s; s++) {
		char c = *s;
		switch (c) {
		case '<': fputs("&lt;", d->file); break;
		case '>': fputs("&gt;", d->file); break;
		case '&': fputs("&amp;", d->file); break;
		default: putc(c, d->file); break;
		}
	}
	return 1;
}

void beginHtmlDump(HtmlDumper *dumper, const char *file)
{
	dumper->dumper.pre_fn = &dumpHtmlPre;
	dumper->dumper.print_fn = &dumpHtmlPrint;
	dumper->dumper.post_fn = &dumpHtmlPost;
	dumper->file = fopen(file, "wb");
	dumper->level = 0;
	fputs("<html><head><title>Ore AST</title><style>\n", dumper->file);
	for (uint32_t i = 0; i < arraySize(htmlHoverColors); i++) {
		fprintf(dumper->file, ".ast-%u:hover { background-color: %s }\n",
			i, htmlHoverColors[i]);
	}
	fputs("</style></head><body><pre>\n", dumper->file);
}

void endHtmlDump(HtmlDumper *dumper)
{
	fputs("</pre></body></html>", dumper->file);
	fclose(dumper->file);
}

int main(int argc, char **argv)
{
	const char *filename = NULL;
	const char *astFilename = NULL;
	for (int i = 1; i < argc; i++) {
		if (!strcmp(argv[i], "--dump-ast")) {
			astFilename = argv[++i];
		} else {
			if (filename) {
				fprintf(stderr, "Multiple files are not supported\n");
				return 1;
			}
			filename = argv[i];
		}
	}

	if (filename == NULL) {
		fprintf(stderr, "No file specified\n");
		return 1;
	}

	size_t size;
	char *src = readFile(filename, &size);

	ErrorList errors = { 0 };

	LexerInput input = {
		.source = src,
		.size = size,
		.filename = filename,
		.errorList = &errors,
	};


#if 0
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
#else
	Ast *ast = parse(&input);
	if (ast) {
		if (astFilename) {
			HtmlDumper hd;
			beginHtmlDump(&hd, astFilename);
			dumpAst(ast, &hd.dumper);
			endHtmlDump(&hd);
		}

		freeAst(ast);
	} else {
		for (uint32_t i = 0; i < errors.errors.size; i++) {
			Error *error = &errors.errors.data[i];
			SourceData sd = getSourceData(error->span);
			fprintf(stderr, "%s:%u:%u: Error: %s\n",
				sd.filename, sd.line, sd.col, error->message);
		}
	}
#endif


	return 0;
}

