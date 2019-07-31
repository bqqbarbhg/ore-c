#ifndef ARENA_H_INCLUDED
#define ARENA_H_INCLUDED

#ifndef ARENA_INLINE
	#define ARENA_INLINE static
#endif

#include <stddef.h>
#include <stdint.h>
#include <string.h>

typedef struct arena_s {
	void *data;
	size_t pos, capacity;
} arena;

void arena_init(arena *arena);
void arena_reset(arena *arena);

uint32_t arena_new_page(arena *arena, size_t size);

ARENA_INLINE void *arena_push_size_zero(arena *arena, size_t size)
{
	size_t pos = arena->pos;
	void *result;
	size = (size + 7u) & ~(size_t)7u;
	if (pos + size <= arena->capacity) { } else {
		pos = arena_new_page(arena, size);
	}
	arena->pos += size;
	result = (char*)arena->data + pos;
	memset(result, 0, size);
	return result;
}

ARENA_INLINE void *arena_push_size_uninit(arena *arena, size_t size)
{
	size_t pos = arena->pos;
	size = (size + 7u) & ~(size_t)7u;
	if (pos + size <= arena->capacity) { } else {
		pos = arena_new_page(arena, size);
	}
	arena->pos += size;
	return (char*)arena->data + pos;
}

ARENA_INLINE void *arena_push_size_copy(arena *arena, size_t size, const void *data)
{
	size_t pos = arena->pos, copy_size = size;
	void *result;
	size = (size + 7u) & ~(size_t)7u;
	if (pos + size <= arena->capacity) { } else {
		pos = arena_new_page(arena, size);
	}
	arena->pos += size;
	result = (char*)arena->data + pos;
	memcpy(result, data, copy_size);
	return result;
}

#define arena_push_zero(p_arena, p_type) (p_type*)arena_push_size_zero((p_arena), sizeof(p_type))
#define arena_push_uninit(p_arena, p_type) (p_type*)arena_push_size_uninit((p_arena), sizeof(p_type))
#define arena_push_copy(p_arena, p_type, p_data) (p_type*)arena_push_size_copy((p_arena), sizeof(p_type), (p_data))

#endif

#ifdef ARENA_IMPLEMENTATION
#ifndef ARENA_H_IMPLEMENTED
#define ARENA_H_IMPLEMENTED

#include <stdlib.h>

void arena_init(arena *arena)
{
	arena->data = 0;
	arena->pos = arena->capacity = 0;
}

void arena_reset(arena *arena)
{
	void *data = arena->data;
	while (data) {
		void *to_free = data;
		data = *(void**)data;
		free(to_free);
	}
	arena->data = 0;
	arena->pos = arena->capacity = 0;
}

uint32_t arena_new_page(arena *arena, size_t size)
{
	size_t capacity = arena->capacity * 2;
	void *old_page = arena->data;
	if (capacity < 4096) capacity = 4096;
	if (capacity < size + 8) capacity = size + 8;
	arena->data = malloc(capacity);
	arena->pos = 8;
	arena->capacity = capacity;
	*(void**)arena->data = old_page;
	return 8;
}

#endif
#endif