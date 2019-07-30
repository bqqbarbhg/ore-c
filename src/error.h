#pragma once

#include "buf.h"
#include "arena.h"
#include "lexer.h"

typedef struct Error_s {
	SourceSpan span;
	const char *message;
} Error;

typedef buf_type(Error) Error_buf;

typedef struct ErrorList_s {
	Error_buf errors;
	arena arena;
} ErrorList;

void pushError(ErrorList *list, SourceSpan span, const char *message);
void freeErrors(ErrorList *list);
