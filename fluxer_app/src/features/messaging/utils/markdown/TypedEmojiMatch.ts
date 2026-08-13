// SPDX-License-Identifier: AGPL-3.0-or-later

import {findUrlSpans, isInsideSpan, type TextSpan} from '@app/features/messaging/utils/markdown/UrlSpanUtils';

export const TYPED_EMOJI_SHORTCODE_PATTERN = /:([\p{L}\p{N}_+~.-]{2,}):/u;

export interface TypedEmojiMatch {
	start: number;
	end: number;
	name: string;
}

const SHORTCODE_WORD_BOUNDARY_CHAR = /[\p{L}\p{N}_]/u;

export function isTypedEmojiShortcodeBoundary(text: string, start: number, end: number): boolean {
	const previous = start > 0 ? text.charAt(start - 1) : '';
	const next = end < text.length ? text.charAt(end) : '';
	return !SHORTCODE_WORD_BOUNDARY_CHAR.test(previous) && !SHORTCODE_WORD_BOUNDARY_CHAR.test(next);
}

export function isExistingCustomEmojiMarkdown(content: string, matchIndex: number): boolean {
	return content[matchIndex - 1] === '<' || (content[matchIndex - 2] === '<' && content[matchIndex - 1] === 'a');
}

export function findTypedEmojiShortcode(text: string, startIndex = 0): TypedEmojiMatch | null {
	if (!text.includes(':')) {
		return null;
	}
	const pattern = new RegExp(TYPED_EMOJI_SHORTCODE_PATTERN.source, 'gu');
	pattern.lastIndex = startIndex;
	let urlSpans: Array<TextSpan> | null = null;
	let match: RegExpExecArray | null = pattern.exec(text);
	while (match != null) {
		const matchIndex = match.index;
		const matchEnd = matchIndex + match[0].length;
		if (!isExistingCustomEmojiMarkdown(text, matchIndex) && isTypedEmojiShortcodeBoundary(text, matchIndex, matchEnd)) {
			if (urlSpans === null) {
				urlSpans = findUrlSpans(text);
			}
			if (!isInsideSpan(urlSpans, matchIndex)) {
				return {start: matchIndex, end: matchEnd, name: match[1]!};
			}
		}
		match = pattern.exec(text);
	}
	return null;
}
