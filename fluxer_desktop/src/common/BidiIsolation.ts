// SPDX-License-Identifier: AGPL-3.0-or-later

const BIDI_FIRST_STRONG_ISOLATE = '\u2068';
const BIDI_POP_DIRECTIONAL_ISOLATE = '\u2069';

export function isolateBidiText(value: string): string {
	return `${BIDI_FIRST_STRONG_ISOLATE}${value}${BIDI_POP_DIRECTIONAL_ISOLATE}`;
}
