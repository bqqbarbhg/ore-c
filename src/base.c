#include "base.h"
#include <stdlib.h>

#define RHMAP_INLINE static
#include "rhmap.h"

void bufFree(void *buf)
{
	if (!buf) return;
	free((char*)buf - 8);
}

void *bufGrowSize(void *buf, size_t elemSize, size_t size)
{
	if (buf) {
		size_t oldCap = ((size_t*)buf)[-1];
		size_t cap = oldCap * 2;
		if (cap < size) cap = size;
		char *newBuf = realloc((char*)buf - 8, 8 + elemSize * cap);
		newBuf += 8;
		memset((char*)newBuf + oldCap * elemSize, 0, (cap - oldCap) * elemSize);
		return newBuf;
	} else {
		size_t cap = 128 / elemSize;
		if (cap < size) cap = size;
		char *newBuf = malloc(8 + elemSize * cap);
		newBuf += 8;
		memset(newBuf, 0, elemSize * cap);
		((size_t*)newBuf)[-1] = cap;
		return newBuf;
	}
}

void *pushSize(Arena *arena, size_t size)
{
	size = (size + 7u) & ~7u;
	size_t pos = arena->pos;
	if (pos + size <= arena->size) {
		arena->pos = pos + size;
		return (char*)arena->page + pos;
	} else {
		size_t pageSize = arena->size * 2;
		if (pageSize < size + 8) pageSize = size + 8;
		if (pageSize < 4096) pageSize = 4096;
		char *newPage = (char*)malloc(pageSize);
		*(void**)newPage = arena->page;
		arena->page = newPage;
		arena->pos = 8;
		arena->size = size;
		return newPage + 8;
	}
}

void freeArena(Arena *arena)
{
	void *page = arena->page;
	while (page) {
		void *toFree = page;
		page = *(void**)page;
		free(toFree);
	}
}

uint32_t symbolHash(const char *str, size_t len)
{
	uint32_t hash = symbolHashInit();
	for (size_t i = 0; i < len; i++) {
		hash = symbolHashStep(hash, str[i]);
	}
	return hash;
}

Symbol intern(const char *str, size_t len)
{
	return internHash(str, len, symbolHash(str, len));
}

Symbol internZ(const char *str)
{
	size_t len = strlen(str);
	return internHash(str, len, symbolHash(str, len));
}

typedef struct {
	rhmap map;
	Arena arena;
	SymbolString **strings;
} StringPool;

StringPool g_stringPool;

SymbolString g_nullSymbol = {
	.length = 6,
	.data = "(null)",
};

static void stringPoolGrow(StringPool *pool)
{
	size_t count, allocSize;
	rhmap_grow(&pool->map, &count, &allocSize, 16, 0.8);
	size_t stringsSize = (count + 1) * sizeof(SymbolString*);
	void *data = malloc(allocSize + stringsSize);
	SymbolString **newStrings = (SymbolString**)((char*)data + allocSize);
	newStrings[0] = &g_nullSymbol;
	memcpy(newStrings + 1, pool->strings + 1, pool->map.size * sizeof(SymbolString*));
	pool->strings = newStrings;
	void *old = rhmap_rehash(&pool->map, count, allocSize, data);
	free(old);
}

Symbol internHash(const char *str, size_t len, uint32_t hash)
{
	StringPool *pool = &g_stringPool;
	if (pool->map.size >= pool->map.capacity) {
		stringPoolGrow(&g_stringPool);
	}
	rhmap_iter iter = { &pool->map, hash };
	uint32_t symbol;
	while (rhmap_find_inline(&iter, &symbol)) {
		SymbolString *ref = pool->strings[symbol];
		if (ref->length == len && !memcmp(ref->data, str, len)) {
			return (Symbol){ symbol };
		}
	}

	symbol = pool->map.size + 1;
	rhmap_insert_inline(&iter, symbol);
	SymbolString *copy = (SymbolString*)pushSize(&pool->arena, sizeof(SymbolString) + len + 1);
	copy->length = len;
	memcpy(copy->data, str, len);
	copy->data[len] = '\0';
	pool->strings[symbol] = copy;
	return (Symbol) { symbol };
}

SymbolString *getString(Symbol symbol)
{
	StringPool *pool = &g_stringPool;
	return pool->strings[symbol.index];
}

static uint32_t lowbias32(uint32_t x)
{
    x ^= x >> 16;
    x *= UINT32_C(0x7feb352d);
    x ^= x >> 15;
    x *= UINT32_C(0x846ca68b);
    x ^= x >> 16;
    return x;
}

uint32_t symbolMapInsert(SymbolMap *map, Symbol symbol, uint32_t value)
{
	if (map->map.size >= map->map.capacity) {
		size_t count, allocSize;
		rhmap_grow(&map->map, &count, &allocSize, 16, 0.8);
		void *newData = malloc(allocSize);
		void *prev = rhmap_rehash(&map->map, count, allocSize, newData);
		free(prev);
	}
	rhmap_iter iter = { &map->map, lowbias32(symbol.index) };
	uint32_t ref;
	if (rhmap_find_inline(&iter, &ref)) {
		return ref;
	}
	rhmap_insert_inline(&iter, value);
	return value;
}

uint32_t symbolMapInsertArrSize(SymbolMap *map, void **arr, size_t elemSize, Symbol symbol)
{
	if (map->map.size >= map->map.capacity) {
		size_t count, allocSize;
		rhmap_grow(&map->map, &count, &allocSize, 16, 0.8);
		size_t arrSize = count * elemSize;
		void *newData = malloc(allocSize + arrSize);
		void *newArr = (char*)newData + allocSize;
		memcpy(newArr, *arr, map->map.size * elemSize);
		*arr = newArr;
		void *prev = rhmap_rehash(&map->map, count, allocSize, newData);
		free(prev);
	}
	rhmap_iter iter = { &map->map, lowbias32(symbol.index) };
	uint32_t ref;
	if (rhmap_find_inline(&iter, &ref)) {
		return ref;
	}
	uint32_t index = map->map.size;
	memset((char*)*arr + index * elemSize, 0, elemSize);
	rhmap_insert_inline(&iter, index);
	return index;
}

uint32_t symbolMapFind(SymbolMap *map, Symbol symbol)
{
	rhmap_iter iter = { &map->map, lowbias32(symbol.index) };
	uint32_t ref;
	if (rhmap_find_inline(&iter, &ref)) {
		return ref;
	}
	return ~0u;
}
