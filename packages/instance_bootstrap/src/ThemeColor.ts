// SPDX-License-Identifier: AGPL-3.0-or-later

const HEX_COLOR_PATTERN = /^#?([0-9a-f]{3}|[0-9a-f]{6})$/i;
const UNSET_THEME_COLOR = '#000000';

export function normalizeHexColor(value: unknown): string | null {
	if (typeof value !== 'string') return null;
	const match = HEX_COLOR_PATTERN.exec(value.trim());
	if (match === null) return null;
	const digits = match[1].toLowerCase();
	return `#${digits.length === 3 ? digits.replace(/./g, '$&$&') : digits}`;
}

export function normalizeThemeColor(value: unknown): string | null {
	const color = normalizeHexColor(value);
	return color === UNSET_THEME_COLOR ? null : color;
}
