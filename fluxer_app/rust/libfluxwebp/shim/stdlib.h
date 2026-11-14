// SPDX-License-Identifier: AGPL-3.0-or-later

#ifndef FLUXWEBP_STDLIB_H
#define FLUXWEBP_STDLIB_H
#include <stddef.h>
void* fluxwebp_shim_malloc(size_t size);
void* fluxwebp_shim_calloc(size_t nmemb, size_t size);
void fluxwebp_shim_free(void* ptr);
void fluxwebp_shim_qsort(void* base, size_t nitems, size_t size, int (*compar)(const void*, const void*));
#define malloc(size) fluxwebp_shim_malloc(size)
#define calloc(nmemb, size) fluxwebp_shim_calloc(nmemb, size)
#define free(ptr) fluxwebp_shim_free(ptr)
#define qsort(base, nitems, size, compar) fluxwebp_shim_qsort(base, nitems, size, compar)
#define abort() __builtin_trap()
static inline int abs(int x) { return x < 0 ? -x : x; }
#endif
