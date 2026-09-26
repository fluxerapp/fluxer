// SPDX-License-Identifier: AGPL-3.0-or-later

#ifndef FLUXWEBP_STRING_H
#define FLUXWEBP_STRING_H
#include <stddef.h>
#define memcpy(d, s, n) __builtin_memcpy(d, s, n)
#define memmove(d, s, n) __builtin_memmove(d, s, n)
#define memset(d, c, n) __builtin_memset(d, c, n)
#define memcmp(a, b, n) __builtin_memcmp(a, b, n)
#define strlen(s) __builtin_strlen(s)
#endif
