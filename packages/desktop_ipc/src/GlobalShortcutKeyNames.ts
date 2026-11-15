// SPDX-License-Identifier: AGPL-3.0-or-later

export const PUNCTUATION_KEY_CODES: Readonly<Record<string, string>> = {
	' ': 'Space',
	'-': 'Minus',
	_: 'Minus',
	'=': 'Equal',
	'+': 'Equal',
	',': 'Comma',
	'<': 'Comma',
	'.': 'Period',
	'>': 'Period',
	';': 'Semicolon',
	':': 'Semicolon',
	"'": 'Quote',
	'"': 'Quote',
	'/': 'Slash',
	'?': 'Slash',
	'\\': 'Backslash',
	'|': 'Backslash',
	'[': 'BracketLeft',
	'{': 'BracketLeft',
	']': 'BracketRight',
	'}': 'BracketRight',
	'`': 'Backquote',
	'~': 'Backquote',
};

export function isPrintableShortcutKey(key: string | null | undefined): key is string {
	if (!key) return false;
	if (key === 'Dead' || key === 'Unidentified' || key === 'Process') return false;
	return key.length === 1;
}

export function shouldPreferLayoutKey(combo: {key?: string | null; code?: string}): boolean {
	if (!isPrintableShortcutKey(combo.key)) return false;
	return !(combo.code && /^Numpad/.test(combo.code));
}

export function hookKeyNameForLayoutKey(key: string): string | null {
	if (key.length !== 1) return null;
	const punctuation = PUNCTUATION_KEY_CODES[key];
	if (punctuation) return punctuation;
	const upper = key.toUpperCase();
	return /^[A-Z0-9]$/.test(upper) ? upper : null;
}
