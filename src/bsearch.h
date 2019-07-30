#pragma once

#include <stddef.h>

typedef struct {
	size_t lo, hi;
} bsearch_range;

static int bsearch_next(bsearch_range *range, size_t *index)
{
	if (range->lo == range->hi) {
		*index = range->lo;
		return 0;
	} else {
		*index = (range->lo + range->hi) >> 1;
		return 1;
	}
}

static int bsearch_next_until(bsearch_range *range, size_t *index, size_t size)
{
	if (range->lo + size >= range->hi) {
		return 0;
	} else {
		*index = (range->lo + range->hi) >> 1;
		return 1;
	}
}

static void bsearch_step(bsearch_range *range, int cond)
{
	size_t mid = (range->lo + range->hi) >> 1;
	if (cond) {
		range->hi = mid;
	} else {
		range->lo = mid + 1;
	}
}
