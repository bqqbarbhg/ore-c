#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "rhmap.h"

void *bufGrowSize(void *buf, size_t elemSize, size_t size);

static void bufFitSize(void **buf, size_t elemSize, size_t size)
{
	if (*buf && ((size_t*)*buf)[-1] <= size) { } else {
		*buf = bufGrowSize(*buf, elemSize, size);
	}
}
#define bufFit(buf, size) bufFitSize(&(buf), sizeof(*(buf)), (size))
#define bufPush(buf, size) (bufFitSize((void**)&(buf), sizeof(*(buf)), (size + 1)), (buf) + (size)++)

typedef struct {
	void *page;
	size_t pos, size;
} Arena;

void *pushSize(Arena *arena, size_t size);
void freeArena(Arena *arena);

typedef struct {
	uint32_t index;
} Symbol;

typedef struct {
	uint32_t length;
	char data[];
} SymbolString;

static uint32_t symbolHashInit() { return 2166136261u; }
static uint32_t symbolHashStep(uint32_t state, char c) { return (state ^ c) * 16777619u; }
uint32_t symbolHash(const char *str, size_t len);

Symbol intern(const char *str, size_t len);
Symbol internZ(const char *str);
Symbol internHash(const char *str, size_t len, uint32_t hash);
SymbolString *getString(Symbol symbol);
static const char *getCString(Symbol symbol) { return getString(symbol)->data; }

typedef struct {
	rhmap map;
} SymbolMap;

uint32_t symbolMapInsert(SymbolMap *map, Symbol symbol, uint32_t value);
uint32_t symbolMapInsertArrSize(SymbolMap *map, void **arr, size_t elemSize, Symbol symbol);
uint32_t symbolMapFind(SymbolMap *map, Symbol symbol);

#define symbolMapInsertArr(map, arr, symbol) symbolMapInsertArrSize((map), (arr), sizeof(**(arr)), (symbol))
