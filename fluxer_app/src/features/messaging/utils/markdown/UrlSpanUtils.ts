// SPDX-License-Identifier: AGPL-3.0-or-later

const HTTP_PREFIX = 'http://';
const HTTPS_PREFIX = 'https://';
const APP_PROTOCOL_SCHEME = 'fluxer:';
const TRIMMED_AUTOLINK_PUNCTUATION = new Set(['.', ',', ';', ':', '!', '?']);

export interface TextSpan {
	start: number;
	end: number;
}

export function isUrlStart(content: string, index: number): boolean {
	if (content.startsWith(HTTP_PREFIX, index) || content.startsWith(HTTPS_PREFIX, index)) {
		return true;
	}
	if (!content.startsWith(APP_PROTOCOL_SCHEME, index)) {
		return false;
	}
	const nextCharValue = content[index + APP_PROTOCOL_SCHEME.length];
	const nextChar = nextCharValue === undefined ? '' : nextCharValue;
	return nextChar === '/' || /[A-Za-z0-9_-]/u.test(nextChar);
}

function isUrlTerminationChar(char: string): boolean {
	return char === '' || char === ' ' || char === '\t' || char === '\n' || char === '\r' || char === ')' || char === '"';
}

function hasTerminalTld(text: string): boolean {
	let index = text.length;
	let letterCount = 0;
	while (index > 0 && /[A-Za-z]/u.test(text[index - 1]!)) {
		letterCount++;
		index--;
	}
	return letterCount >= 2 && index > 0 && text[index - 1] === '.';
}

export function findUrlEnd(content: string, start: number): number {
	let end = start;
	let parenDepth = 0;
	while (end < content.length) {
		const char = content[end]!;
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
		TRIMMED_AUTOLINK_PUNCTUATION.has(content[end - 1]!) &&
		!hasTerminalTld(content.slice(start, end))
	) {
		end--;
	}
	return end;
}

const ANGLE_BRACKET_SYNTAX_NEEDLES = ['<:', '<a:', '<id:', '<@', '<#', '</', '<t:', '<+', '<sms:'];

function containsAngleBracketSyntax(value: string): boolean {
	return ANGLE_BRACKET_SYNTAX_NEEDLES.some((needle) => value.includes(needle));
}

export function findUrlSpans(content: string): Array<TextSpan> {
	const spans: Array<TextSpan> = [];
	let index = 0;
	while (index < content.length) {
		if (!isUrlStart(content, index)) {
			index++;
			continue;
		}
		const end = findUrlEnd(content, index);
		if (end > index && !containsAngleBracketSyntax(content.slice(index, end))) {
			spans.push({start: index, end});
			index = end;
			continue;
		}
		index++;
	}
	return spans;
}

export function isInsideSpan(spans: ReadonlyArray<TextSpan>, index: number): boolean {
	return spans.some((span) => index >= span.start && index < span.end);
}
