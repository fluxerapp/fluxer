// SPDX-License-Identifier: AGPL-3.0-or-later

const pad2 = (value: string): string => (value.length === 1 ? `0${value}` : value);
const INT2HEX_CACHE = new Map<number, string>();
const INT2RGB_CACHE = new Map<number, string>();
const INT2RGBA_CACHE = new Map<string, string>();
const CONTRAST_CACHE = new Map<number, 'black' | 'white'>();
const COLOR_CACHE_LIMIT = 1024;
const DIM_CACHE_LIMIT = 512;

function rememberColor<K, V>(cache: Map<K, V>, key: K, value: V, limit: number): V {
	if (cache.size >= limit) cache.clear();
	cache.set(key, value);
	return value;
}

export function int2hex(colorInt: number) {
	const cached = INT2HEX_CACHE.get(colorInt);
	if (cached !== undefined) return cached;
	const r = (colorInt >> 16) & 0xff;
	const g = (colorInt >> 8) & 0xff;
	const b = colorInt & 0xff;
	return rememberColor(
		INT2HEX_CACHE,
		colorInt,
		`#${pad2(r.toString(16))}${pad2(g.toString(16))}${pad2(b.toString(16))}`,
		COLOR_CACHE_LIMIT,
	);
}

export function int2rgba(colorInt: number, alpha?: number) {
	const resolvedAlpha = alpha == null ? ((colorInt >> 24) & 0xff) / 255 : alpha;
	const key = `${colorInt}:${resolvedAlpha}`;
	const cached = INT2RGBA_CACHE.get(key);
	if (cached !== undefined) return cached;
	const r = (colorInt >> 16) & 0xff;
	const g = (colorInt >> 8) & 0xff;
	const b = colorInt & 0xff;
	return rememberColor(INT2RGBA_CACHE, key, `rgba(${r}, ${g}, ${b}, ${resolvedAlpha})`, COLOR_CACHE_LIMIT);
}

export function int2rgb(colorInt: number) {
	if (colorInt === 0) {
		return 'rgb(219, 222, 225)';
	}
	const cached = INT2RGB_CACHE.get(colorInt);
	if (cached !== undefined) return cached;
	const r = (colorInt >> 16) & 0xff;
	const g = (colorInt >> 8) & 0xff;
	const b = colorInt & 0xff;
	return rememberColor(INT2RGB_CACHE, colorInt, `rgb(${r}, ${g}, ${b})`, COLOR_CACHE_LIMIT);
}

export function getBestContrastColor(colorInt: number): 'black' | 'white' {
	if (colorInt === 0) {
		return 'black';
	}
	const cached = CONTRAST_CACHE.get(colorInt);
	if (cached !== undefined) return cached;
	const r = (colorInt >> 16) & 0xff;
	const g = (colorInt >> 8) & 0xff;
	const b = colorInt & 0xff;
	const rsRGB = r / 255;
	const gsRGB = g / 255;
	const bsRGB = b / 255;
	const rLinear = rsRGB <= 0.03928 ? rsRGB / 12.92 : ((rsRGB + 0.055) / 1.055) ** 2.4;
	const gLinear = gsRGB <= 0.03928 ? gsRGB / 12.92 : ((gsRGB + 0.055) / 1.055) ** 2.4;
	const bLinear = bsRGB <= 0.03928 ? bsRGB / 12.92 : ((bsRGB + 0.055) / 1.055) ** 2.4;
	const luminance = 0.2126 * rLinear + 0.7152 * gLinear + 0.0722 * bLinear;
	return rememberColor(CONTRAST_CACHE, colorInt, luminance > 0.5 ? 'black' : 'white', COLOR_CACHE_LIMIT);
}

const AVATAR_BACKGROUND_DIM_AMOUNT = 0.12;

function clampChannel(value: number): number {
	return Math.max(0, Math.min(255, Math.round(value)));
}

const HEX_COLOR_RE = /^#([0-9a-f]{6})$/i;
const RGB_COLOR_RE = /^rgb\(\s*([0-9]{1,3})\s*,\s*([0-9]{1,3})\s*,\s*([0-9]{1,3})\s*\)$/i;
const DIM_RESULT_CACHE = new Map<string, string | null>();

function dimHexColor(color: string, amount: number): string | null {
	const match = HEX_COLOR_RE.exec(color.trim());
	if (!match) return null;
	const hex = match[1];
	const r = Number.parseInt(hex.slice(0, 2), 16);
	const g = Number.parseInt(hex.slice(2, 4), 16);
	const b = Number.parseInt(hex.slice(4, 6), 16);
	const factor = 1 - amount;
	return `rgb(${clampChannel(r * factor)}, ${clampChannel(g * factor)}, ${clampChannel(b * factor)})`;
}

function dimRgbColor(color: string, amount: number): string | null {
	const match = RGB_COLOR_RE.exec(color.trim());
	if (!match) return null;
	const r = Number(match[1]);
	const g = Number(match[2]);
	const b = Number(match[3]);
	const factor = 1 - amount;
	return `rgb(${clampChannel(r * factor)}, ${clampChannel(g * factor)}, ${clampChannel(b * factor)})`;
}

export function dimColor(color: string, amount = AVATAR_BACKGROUND_DIM_AMOUNT): string {
	const clampedAmount = Math.max(0, Math.min(1, amount));
	const key = `${color}|${clampedAmount}`;
	const cached = DIM_RESULT_CACHE.get(key);
	if (cached !== undefined) return cached ?? color;
	const result = dimHexColor(color, clampedAmount) ?? dimRgbColor(color, clampedAmount);
	if (DIM_RESULT_CACHE.size >= DIM_CACHE_LIMIT) DIM_RESULT_CACHE.clear();
	DIM_RESULT_CACHE.set(key, result);
	return result ?? color;
}
