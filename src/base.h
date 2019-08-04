#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "rhmap.h"

#include "buf.h"

typedef buf_type(uint32_t) u32_buf;

#define arraySize(arr) (sizeof(arr) / sizeof(*(arr)))

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
} U32Map;

void u32MapInitLocal(U32Map *map, uint64_t *local, size_t localSize);
void u32MapFree(U32Map *map);

uint32_t u32MapInsert(U32Map *map, uint32_t key, uint32_t value);
uint32_t u32MapInsertArrSize(U32Map *map, void **arr, size_t elemSize, uint32_t key);
uint32_t u32MapFind(U32Map *map, uint32_t key);

#define u32MapInsertArr(map, arr, key) u32MapInsertArrSize((map), (arr), sizeof(**(arr)), (key))

typedef struct {
	U32Map map;
} SymbolMap;

static void symbolMapInitLocal(SymbolMap *map, uint64_t *local, size_t localSize)
{
	u32MapInitLocal(&map->map, local, localSize);
}
static void symbolMapFree(SymbolMap *map)
{
	u32MapFree(&map->map);
}

static uint32_t symbolMapInsert(SymbolMap *map, Symbol symbol, uint32_t value)
{
	return u32MapInsert(&map->map, symbol.index, value);
}
static uint32_t symbolMapInsertArrSize(SymbolMap *map, void **arr, size_t elemSize, Symbol symbol)
{
	return u32MapInsertArrSize(&map->map, arr, elemSize, symbol.index);
}
static uint32_t symbolMapFind(SymbolMap *map, Symbol symbol)
{
	return u32MapFind(&map->map, symbol.index);
}

#define symbolMapInsertArr(map, arr, symbol) symbolMapInsertArrSize((map), (arr), sizeof(**(arr)), (symbol))

