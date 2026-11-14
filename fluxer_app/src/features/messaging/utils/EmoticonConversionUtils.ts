// SPDX-License-Identifier: AGPL-3.0-or-later

import UnicodeEmojis from '@app/features/expressions/utils/UnicodeEmojis';
import {findUrlEnd, isUrlStart} from '@app/features/messaging/utils/markdown/UrlSpanUtils';

function isEscaped(content: string, index: number): boolean {
	let backslashCount = 0;
	for (let cursor = index - 1; cursor >= 0 && content[cursor] === '\\'; cursor--) {
		backslashCount++;
	}
	return backslashCount % 2 === 1;
}

function findClosingBracket(content: string, start: number): number | null {
	let depth = 0;
	for (let index = start + 1; index < content.length; index++) {
		if (isEscaped(content, index)) {
			continue;
		}
		const char = content[index];
		if (char === '[') {
			depth++;
			continue;
		}
		if (char !== ']') {
			continue;
		}
		if (depth === 0) {
			return index;
		}
		depth--;
	}
	return null;
}

function findMarkdownDestinationEnd(content: string, openParenIndex: number): number | null {
	let depth = 0;
	for (let index = openParenIndex + 1; index < content.length; index++) {
		if (isEscaped(content, index)) {
			continue;
		}
		const char = content[index];
		if (char === '(') {
			depth++;
			continue;
		}
		if (char !== ')') {
			continue;
		}
		if (depth === 0) {
			return index + 1;
		}
		depth--;
	}
	return null;
}

function findMarkdownLinkEnd(content: string, start: number): number | null {
	const bracketStart = content[start] === '!' && content[start + 1] === '[' ? start + 1 : start;
	if (content[bracketStart] !== '[') {
		return null;
	}
	const closeBracket = findClosingBracket(content, bracketStart);
	if (closeBracket == null || content[closeBracket + 1] !== '(') {
		return null;
	}
	return findMarkdownDestinationEnd(content, closeBracket + 1);
}

function readBacktickRun(content: string, start: number): number {
	let length = 0;
	while (content[start + length] === '`') {
		length++;
	}
	return length;
}

function findClosingBacktickRun(content: string, start: number, runLength: number): number | null {
	let index = start;
	while (index < content.length) {
		if (content[index] !== '`') {
			index++;
			continue;
		}
		const length = readBacktickRun(content, index);
		if (length === runLength) {
			return index + length;
		}
		index += length;
	}
	return null;
}

function findProtectedSpanEnd(content: string, index: number): number | null {
	const backtickRunLength = readBacktickRun(content, index);
	if (backtickRunLength > 0) {
		return findClosingBacktickRun(content, index + backtickRunLength, backtickRunLength);
	}

	const markdownLinkEnd = findMarkdownLinkEnd(content, index);
	if (markdownLinkEnd != null) {
		return markdownLinkEnd;
	}

	if (!isUrlStart(content, index)) {
		return null;
	}
	const urlEnd = findUrlEnd(content, index, true);
	return urlEnd > index ? urlEnd : null;
}

function hasLeadingBoundary(content: string, index: number): boolean {
	return index === 0 || /\s/u.test(content[index - 1] ?? '');
}

function hasTrailingBoundary(content: string, index: number): boolean {
	return index >= content.length || content[index] === ' ';
}

function shortcutToEmoji(shortcut: string): string | null {
	const name = UnicodeEmojis.convertShortcutToName(shortcut, false);
	if (!name) {
		return null;
	}
	const surrogate = UnicodeEmojis.surrogateForName(name);
	return surrogate || null;
}

function matchEmoticonAt(content: string, index: number): {shortcut: string; emoji: string} | null {
	if (!hasLeadingBoundary(content, index)) {
		return null;
	}
	const match = UnicodeEmojis.EMOTICON_PREFIX_RE.exec(content.slice(index));
	if (match == null) {
		return null;
	}
	const shortcut = match[1] ?? match[0];
	if (!hasTrailingBoundary(content, index + shortcut.length)) {
		return null;
	}
	const emoji = shortcutToEmoji(shortcut);
	return emoji == null ? null : {shortcut, emoji};
}

export function convertEmoticonsToEmoji(content: string): string {
	if (content.length === 0) {
		return content;
	}

	const pieces: Array<string> = [];
	let index = 0;
	while (index < content.length) {
		const protectedEnd = findProtectedSpanEnd(content, index);
		if (protectedEnd != null && protectedEnd > index) {
			pieces.push(content.slice(index, protectedEnd));
			index = protectedEnd;
			continue;
		}

		const match = matchEmoticonAt(content, index);
		if (match != null) {
			pieces.push(match.emoji);
			index += match.shortcut.length;
			continue;
		}

		pieces.push(content[index] ?? '');
		index++;
	}

	return pieces.join('');
}
