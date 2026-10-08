// SPDX-License-Identifier: AGPL-3.0-or-later

#ifndef FLUXWEBP_MATH_H
#define FLUXWEBP_MATH_H
double fluxwebp_shim_pow(double x, double y);
double fluxwebp_shim_log(double x);
float fluxwebp_shim_expf(float x);
double fluxwebp_shim_log10(double x);
float fluxwebp_shim_logf(float x);
double fluxwebp_shim_round(double x);
#define pow(x, y) fluxwebp_shim_pow(x, y)
#define log(x) fluxwebp_shim_log(x)
#define expf(x) fluxwebp_shim_expf(x)
#define log10(x) fluxwebp_shim_log10(x)
#define logf(x) fluxwebp_shim_logf(x)
#define round(x) fluxwebp_shim_round(x)
#define fabs(x) __builtin_fabs(x)
#define floor(x) __builtin_floor(x)
#define ceil(x) __builtin_ceil(x)
#define sqrt(x) __builtin_sqrt(x)
#define sqrtf(x) __builtin_sqrtf(x)
#define rint(x) __builtin_rint(x)
#define rintf(x) __builtin_rintf(x)
#define fabsf(x) __builtin_fabsf(x)
#define floorf(x) __builtin_floorf(x)
#define ceilf(x) __builtin_ceilf(x)
#define isnan(x) __builtin_isnan(x)
#define isinf(x) __builtin_isinf(x)
#define lrint(x) ((long)__builtin_rint(x))
#define llrint(x) ((long long)__builtin_rint(x))
#define lrintf(x) ((long)__builtin_rintf(x))
#define llrintf(x) ((long long)__builtin_rintf(x))
#endif
