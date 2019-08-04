#include "base.h"
#include <stdlib.h>

#include "arena.h"

#define RHMAP_INLINE static
#include "rhmap.h"

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
	arena arena;
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
	SymbolString *copy = (SymbolString*)arena_push_size_uninit(&pool->arena, sizeof(SymbolString) + len + 1);
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

void u32MapInitLocal(U32Map *map, uint64_t *local, size_t localSize)
{
	size_t count = (size_t)(localSize / sizeof(uint64_t) * 0.8);
	rhmap_init_inline(&map->map);
	rhmap_rehash(&map->map, count, localSize, local);
}

void u32MapFree(U32Map *map)
{
	rhmap_reset_inline(&map->map);
}

uint32_t u32MapInsert(U32Map *map, uint32_t key, uint32_t value)
{
	if (map->map.size >= map->map.capacity) {
		size_t count, allocSize;
		rhmap_grow(&map->map, &count, &allocSize, 16, 0.8);
		void *newData = malloc(allocSize);
		void *prev = rhmap_rehash(&map->map, count, allocSize, newData);
		free(prev);
	}
	rhmap_iter iter = { &map->map, lowbias32(key) };
	uint32_t ref;
	if (rhmap_find_inline(&iter, &ref)) {
		return ref;
	}
	rhmap_insert_inline(&iter, value);
	return value;
}

uint32_t u32MapInsertArrSize(U32Map *map, void **arr, size_t elemSize, uint32_t key)
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
	rhmap_iter iter = { &map->map, lowbias32(key) };
	uint32_t ref;
	if (rhmap_find_inline(&iter, &ref)) {
		return ref;
	}
	uint32_t index = map->map.size;
	memset((char*)*arr + index * elemSize, 0, elemSize);
	rhmap_insert_inline(&iter, index);
	return index;
}

uint32_t u32MapFind(U32Map *map, uint32_t key)
{
	rhmap_iter iter = { &map->map, lowbias32(key) };
	uint32_t ref;
	if (rhmap_find_inline(&iter, &ref)) {
		return ref;
	}
	return ~0u;
}
