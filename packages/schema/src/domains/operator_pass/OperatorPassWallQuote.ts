// SPDX-License-Identifier: AGPL-3.0-or-later

import {segmentOperatorPassText} from '@fluxer/schema/src/domains/operator_pass/OperatorPassGraphemes';

export const OPERATOR_PASS_WALL_QUOTE_MAX_LENGTH = 80;

export const OPERATOR_PASS_WALL_QUOTE_MAX_CODE_UNITS = 2048;

export type OperatorPassWallQuoteProblem = 'too_long' | 'line_break' | 'characters' | 'link';

const MAX_CODE_POINTS_PER_CHARACTER = 12;
const LINE_BREAK = /[\n\v\f\r\u0085\u2028\u2029]/u;
const FORBIDDEN = /[\p{Cc}\p{Cf}\p{Cs}\p{Co}\u115F\u1160\u2800\u3164\uFFA0\uFFFE\uFFFF]|\u034F|\u17B4|\u17B5/u;
const TAG_FLAG = /\u{1F3F4}[\u{E0030}-\u{E0039}\u{E0061}-\u{E007A}]{1,6}\u{E007F}/gu;
const LETTER = /[\p{L}\p{M}]/u;
const EMOJI_BEFORE_JOINER = /^(?:[\p{Extended_Pictographic}\p{Emoji_Modifier}]|\uFE0F)$/u;
const EMOJI = /\p{Extended_Pictographic}/u;
const LINK_PATTERNS: ReadonlyArray<RegExp> = [
	/https?:\/\//iu,
	/(?<![\p{L}\p{N}])www[.\u2024\u3002\uFE52\uFF0E\uFF61]/iu,
	/[@\uFF20][\p{L}\p{N}_]/u,
	/[\p{L}\p{N}][.\u2024\u3002\uFE52\uFF0E\uFF61](?:\p{L}{2,24}|xn--[a-z0-9-]+)(?![\p{L}\p{N}])/iu,
	/(?<![\p{N}.\u2024\u3002\uFE52\uFF0E\uFF61])\p{N}{1,3}(?:[.\u2024\u3002\uFE52\uFF0E\uFF61]\p{N}{1,3}){3}(?!\p{N})/u,
	/(?<![\p{L}\p{N}])(?:invite|join)\s*\/\s*[\p{L}\p{N}]/iu,
];

export function normalizeOperatorPassWallQuote(value: string | null | undefined): string | null {
	const normalized = (value ?? '').normalize('NFC').trim();
	return normalized.length > 0 ? normalized : null;
}

export function countOperatorPassWallQuoteCharacters(quote: string): number {
	let count = 0;
	for (const _ of segmentOperatorPassText(quote)) {
		count++;
	}
	return count;
}

function isAllowedFormatCharacter(characters: ReadonlyArray<string>, index: number): boolean {
	const character = characters[index];
	const previous = characters[index - 1] ?? '';
	const next = characters[index + 1] ?? '';
	if (character === '\u200D' && EMOJI_BEFORE_JOINER.test(previous) && EMOJI.test(next)) {
		return true;
	}
	return (character === '\u200C' || character === '\u200D') && LETTER.test(previous) && LETTER.test(next);
}

function hasForbiddenCharacters(quote: string): boolean {
	const characters = Array.from(quote.replace(TAG_FLAG, '\u{1F3F4}'));
	return characters.some(
		(character, index) => FORBIDDEN.test(character) && !isAllowedFormatCharacter(characters, index),
	);
}

export function getOperatorPassWallQuoteProblem(quote: string): OperatorPassWallQuoteProblem | null {
	const graphemes = Array.from(segmentOperatorPassText(quote), ({segment}) => segment);
	if (graphemes.length > OPERATOR_PASS_WALL_QUOTE_MAX_LENGTH) {
		return 'too_long';
	}
	if (LINE_BREAK.test(quote)) {
		return 'line_break';
	}
	if (
		hasForbiddenCharacters(quote) ||
		graphemes.some((grapheme) => Array.from(grapheme).length > MAX_CODE_POINTS_PER_CHARACTER)
	) {
		return 'characters';
	}
	if (LINK_PATTERNS.some((pattern) => pattern.test(quote))) {
		return 'link';
	}
	return null;
}
