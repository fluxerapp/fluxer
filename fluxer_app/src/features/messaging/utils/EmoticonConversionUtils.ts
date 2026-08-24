// SPDX-License-Identifier: AGPL-3.0-or-later

import UnicodeEmojis from '@app/features/expressions/utils/UnicodeEmojis';

const HTTP_PREFIX = 'http://';
const HTTPS_PREFIX = 'https://';
const APP_PROTOCOL_SCHEME = 'fluxer:';
const TRIMMED_AUTOLINK_PUNCTUATION = new Set(['.', ',', ';', ':', '!', '?']);

function isUrlStart(content: string, index: number): boolean {
	if (content.startsWith(HTTP_PREFIX, index) || content.startsWith(HTTPS_PREFIX, index)) {
		return true;
	}
	if (!content.startsWith(APP_PROTOCOL_SCHEME, index)) {
		return false;
	}
	const nextChar = content[index + APP_PROTOCOL_SCHEME.length] ?? '';
	return nextChar === '/' || /[A-Za-z0-9_-]/u.test(nextChar);
}

function isUrlTerminationChar(char: string): boolean {
	return (
		char === '' ||
		char === ' ' ||
		char === '\t' ||
		char === '\n' ||
		char === '\r' ||
		char === ')' ||
		char === '"' ||
		char === '<' ||
		char === '>'
	);
}

function hasTerminalTld(text: string): boolean {
	let index = text.length;
	let letterCount = 0;
	while (index > 0 && /[A-Za-z]/u.test(text[index - 1] ?? '')) {
		letterCount++;
		index--;
	}
	return letterCount >= 2 && index > 0 && text[index - 1] === '.';
}

function findUrlEnd(content: string, start: number): number {
	let end = start;
	let parenDepth = 0;
	while (end < content.length) {
		const char = content[end] ?? '';
		if (char === '(') {
			parenDepth++;
			end++;
			continue;
		}
		if (char === ')') {
			if (parenDepth > 0) {
				parenDepth--;
				end++;
				continue;
			}
			break;
		}
		if (isUrlTerminationChar(char)) {
			break;
		}
		end++;
	}
	while (
		end > start &&
		TRIMMED_AUTOLINK_PUNCTUATION.has(content[end - 1] ?? '') &&
		!hasTerminalTld(content.slice(start, end))
	) {
		end--;
	}
	return end;
}

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
	const urlEnd = findUrlEnd(content, index);
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
