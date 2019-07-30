#ifndef BUF_H_INCLUDED
#define BUF_H_INCLUDED

#ifndef BUF_INLINE
	#define BUF_INLINE static
#endif

#include <stdint.h>
#include <string.h>

typedef struct buf_base_s {
	void *data;
	uint32_t size, capacity;
} buf_base;

void buf_base_init(buf_base *buf);
void buf_base_init_local(buf_base *buf, void *data, uint32_t size);
void buf_base_reset(buf_base *buf);
void buf_base_reserve(buf_base *buf, uint32_t item_size, uint32_t size);

void buf_base_grow_one(buf_base *buf, uint32_t item_size);

BUF_INLINE void buf_base_clear(buf_base *buf)
{
	buf->size = 0;
}

BUF_INLINE void buf_base_push(buf_base *buf, uint32_t item_size, const void *data)
{
	uint32_t size = buf->size;
	if (size < buf->capacity) { } else {
		buf_base_grow_one(buf, item_size);
	}
	memcpy((char*)buf->data + size * item_size, data, item_size);
	buf->size = size + 1;
}

BUF_INLINE void *buf_base_push_zero(buf_base *buf, uint32_t item_size)
{
	uint32_t size = buf->size;
	void *result;
	if (size < buf->capacity) { } else {
		buf_base_grow_one(buf, item_size);
	}
	buf->size = size + 1;
	result = (char*)buf->data + size * item_size;
	memset(result, 0, item_size);
	return result;
}

BUF_INLINE void *buf_base_push_uninit(buf_base *buf, uint32_t item_size)
{
	uint32_t size = buf->size;
	if (size < buf->capacity) { } else {
		buf_base_grow_one(buf, item_size);
	}
	buf->size = size + 1;
	return (char*)buf->data + size * item_size;
}

BUF_INLINE void buf_base_remove_swap(buf_base *buf, uint32_t item_size, uint32_t index)
{
	buf->size--;
	if (index != buf->size) {
		char *data = (char*)buf->data;
		memcpy(data + index * item_size, data + buf->size * item_size, item_size);
	}
}

void buf_base_remove_ordered(buf_base *buf, uint32_t item_size, uint32_t index);
void buf_base_insert_ordered(buf_base *buf, uint32_t item_size, uint32_t index, const void *data);

#define buf_local(p_array) { (p_array), 0, \
	(uint32_t)(sizeof(p_array)/sizeof(*(p_array))) & ~1u }

#define buf_type(p_type) union { buf_base buf; \
	struct { p_type *data; uint32_t size, capacity; }; }

#define buf_item_size(p_buf) (uint32_t)sizeof(*(p_buf)->data)

#define buf_init(p_buf) buf_base_init(&(p_buf)->buf)
#define buf_init_local(p_buf, p_data, p_size) buf_base_init_local(&(p_buf)->buf, p_data, (p_size))
#define buf_reset(p_buf) buf_base_reset(&(p_buf)->buf)
#define buf_clear(p_buf) buf_base_clear(&(p_buf)->buf)

#define buf_reserve(p_buf, p_size) \
	buf_base_reserve(&(p_buf)->buf, buf_item_size(p_buf), (p_size))
#define buf_push(p_buf, p_item) \
	buf_base_push(&(p_buf)->buf, buf_item_size(p_buf), (p_item))
#define buf_push_zero(p_buf) \
	buf_base_push_zero(&(p_buf)->buf, buf_item_size(p_buf))
#define buf_push_uninit(p_buf) \
	buf_base_push_uninit(&(p_buf)->buf, buf_item_size(p_buf))
#define buf_remove_swap(p_buf, p_index) \
	buf_base_remove_swap(&(p_buf)->buf, buf_item_size(p_buf), (p_index))
#define buf_remove_ordered(p_buf, p_index) \
	buf_base_remove_ordered(&(p_buf)->buf, buf_item_size(p_buf), (p_index))
#define buf_insert_ordered(p_buf, p_index, p_data) \
	buf_base_insert_ordered(&(p_buf)->buf, buf_item_size(p_buf), (p_index), (p_data))

#endif

#ifdef BUF_IMPLEMENTATION
#ifndef BUF_H_IMPLEMENTED
#define BUF_H_IMPLEMENTED

#include <stdlib.h>

void buf_base_init(buf_base *buf)
{
	buf->data = 0;
	buf->size = buf->capacity = 0;
}

void buf_base_init_local(buf_base *buf, void *data, uint32_t size)
{
	buf->data = data;
	buf->size = 0;
	buf->capacity = size;
}

void buf_base_reset(buf_base *buf)
{
	if (buf->capacity & 1) free(buf->data);
	buf->data = 0;
	buf->size = buf->capacity = 0;
}

void buf_base_reserve(buf_base *buf, uint32_t item_size, uint32_t size)
{
	if (buf->capacity >= size) return;
	buf->capacity = size|1;
	if (buf->capacity & 1) {
		buf->data = realloc(buf->data, buf->capacity * item_size);
	} else {
		buf->data = malloc(buf->capacity * item_size);
	}
}

void buf_base_grow_one(buf_base *buf, uint32_t item_size)
{
	uint32_t capacity = buf->capacity;
	if (capacity > 0) {
		buf->capacity = (capacity * 2) + 1 - (capacity >> 8) * 2;
		if (capacity & 1) {
			buf->data = realloc(buf->data, buf->capacity * item_size);
		} else {
			buf->data = malloc(buf->capacity * item_size);
		}
	} else {
		uint32_t init_cap = (128 / item_size) | 1;
		buf->capacity = init_cap;
		buf->data = malloc(init_cap * item_size);
	}
}

void buf_base_remove_ordered(buf_base *buf, uint32_t item_size, uint32_t index)
{
	uint32_t count = buf->size - index - 1;
	buf->size--;
	if (count > 0) {
		char *base = (char*)buf->data + index * item_size;
		memmove(base, base + item_size, count * item_size);
	}
}

void buf_base_insert_ordered(buf_base *buf, uint32_t item_size, uint32_t index, const void *data)
{
	uint32_t size = buf->size;
	uint32_t count = size - index;
	char *base;
	if (size < buf->capacity) { } else {
		buf_base_grow_one(buf, item_size);
	}
	base = (char*)buf->data + index * item_size;
	buf->size = size + 1;
	if (count > 0) {
		memmove(base + item_size, base, count * item_size);
	}
	memcpy(base, data, item_size);
}

#endif
#endif