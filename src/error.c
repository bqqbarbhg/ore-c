#include "base.h"
#include "error.h"

void pushError(ErrorList *list, SourceSpan span, const char *message)
{
	if (!list) return;
	Error err;
	err.span = span;
	err.message = arena_push_size_copy(&list->arena, strlen(message + 1), message);
	buf_push(&list->errors, &err);
}

void freeErrors(ErrorList *list)
{
	if (!list) return;
	buf_reset(&list->errors);
	arena_reset(&list->arena);
}
