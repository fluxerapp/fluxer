// SPDX-License-Identifier: AGPL-3.0-or-later

#ifndef FLUXWEBP_STDIO_H
#define FLUXWEBP_STDIO_H
#include <stddef.h>
#include <stdarg.h>
typedef struct FluxWebpFile FILE;
#define stderr ((FILE*)0)
#define stdout ((FILE*)0)
#define fprintf(f, ...) ((void)0)
#define printf(...) ((void)0)
#define fflush(f) ((void)0)

static inline void fluxwebp_shim_put(char* buf, size_t cap, size_t* n, char c) {
  if (*n + 1 < cap) buf[*n] = c;
  (*n)++;
}

static inline int snprintf(char* buf, size_t cap, const char* fmt, ...) {
  va_list ap;
  size_t n = 0;
  va_start(ap, fmt);
  for (; *fmt; fmt++) {
    if (*fmt != '%') {
      fluxwebp_shim_put(buf, cap, &n, *fmt);
      continue;
    }
    fmt++;
    if (*fmt == 's') {
      const char* s = va_arg(ap, const char*);
      while (s && *s) fluxwebp_shim_put(buf, cap, &n, *s++);
    } else if (*fmt == 'd') {
      int v = va_arg(ap, int);
      char tmp[12];
      int i = 0;
      unsigned int u = v < 0 ? 0u - (unsigned int)v : (unsigned int)v;
      if (v < 0) fluxwebp_shim_put(buf, cap, &n, '-');
      do { tmp[i++] = (char)('0' + u % 10); u /= 10; } while (u);
      while (i) fluxwebp_shim_put(buf, cap, &n, tmp[--i]);
    } else if (*fmt == '%') {
      fluxwebp_shim_put(buf, cap, &n, '%');
    } else {
      break;
    }
  }
  va_end(ap);
  if (cap) buf[n < cap ? n : cap - 1] = '\0';
  return (int)n;
}
#endif
