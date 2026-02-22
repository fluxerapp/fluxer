/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {parseMention} from '@fluxer/markdown_parser/src/parsers/MentionParsers';
import {NodeType, ParserFlags} from '@fluxer/markdown_parser/src/types/Enums';
import {MAX_LINK_URL_LENGTH} from '@fluxer/markdown_parser/src/types/MarkdownConstants';
import type {Node, ParserResult} from '@fluxer/markdown_parser/src/types/Nodes';
import * as StringUtils from '@fluxer/markdown_parser/src/utils/StringUtils';
import * as URLUtils from '@fluxer/markdown_parser/src/utils/UrlUtils';

const SPOOFED_LINK_PATTERN = /^\[https?:\/\/[^\s[\]]+\]\(https?:\/\/[^\s[\]]+\)$/;
const EMAIL_PATTERN = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;

const OPEN_BRACKET = 91;
const CLOSE_BRACKET = 93;
const OPEN_PAREN = 40;
const CLOSE_PAREN = 41;
const BACKSLASH = 92;
const LESS_THAN = 60;
const GREATER_THAN = 62;
const DOUBLE_QUOTE = 34;
const SINGLE_QUOTE = 39;
const PLUS_SIGN = 43;

function containsLinkSyntax(text: string): boolean {
	const bracketIndex = text.indexOf('[');
	if (bracketIndex === -1) return false;

	const closeBracketIndex = text.indexOf(']', bracketIndex);
	if (closeBracketIndex === -1) return false;

	if (closeBracketIndex + 1 < text.length && text[closeBracketIndex + 1] === '(') {
		return true;
	}

	return containsLinkSyntax(text.substring(closeBracketIndex + 1));
}

export function parseLink(
	text: string,
	parserFlags: number,
	parseInline: (text: string) => Array<Node>,
): ParserResult | null {
	if (text.charCodeAt(0) !== OPEN_BRACKET) return null;

	const linkParts = extractLinkParts(text);

	if (!linkParts) {
		if (SPOOFED_LINK_PATTERN.test(text)) {
			return {
				node: {type: NodeType.Text, content: text},
				advance: text.length,
			};
		}

		const bracketResult = findClosingBracket(text);

		if (bracketResult) {
			const {bracketPosition, linkText} = bracketResult;
			const trimmedLinkText = linkText.trim();

			const mentionResult = parseMention(trimmedLinkText, parserFlags);

			if (mentionResult && mentionResult.advance === trimmedLinkText.length) {
				return null;
			}

			if (containsLinkSyntax(linkText)) {
				return {
					node: {type: NodeType.Text, content: text},
					advance: text.length,
				};
			}

			return {
				node: {type: NodeType.Text, content: text.slice(0, bracketPosition + 1)},
				advance: bracketPosition + 1,
			};
		}

		return null;
	}

	try {
		const normalizedUrl = URLUtils.normalizeUrl(linkParts.url);
		const isValid = URLUtils.isValidUrl(normalizedUrl);

		if (isValid) {
			if (linkParts.url.startsWith('/') && !linkParts.url.startsWith('//')) {
				return {
					node: {type: NodeType.Text, content: text.slice(0, linkParts.advanceBy)},
					advance: linkParts.advanceBy,
				};
			}

			let finalUrl = normalizedUrl;

			if (finalUrl.startsWith('tel:') || finalUrl.startsWith('sms:')) {
				const protocol = finalUrl.substring(0, finalUrl.indexOf(':') + 1);
				const phoneNumber = finalUrl.substring(finalUrl.indexOf(':') + 1);

				if (phoneNumber.startsWith('+')) {
					const normalizedPhone = URLUtils.normalizePhoneNumber(phoneNumber);
					finalUrl = protocol + normalizedPhone;
				}
			} else {
				finalUrl = URLUtils.convertToAsciiUrl(finalUrl);
			}

			const inlineNodes = parseInline(linkParts.linkText);

			return {
				node: {
					type: NodeType.Link,
					text: inlineNodes.length === 1 ? inlineNodes[0] : {type: NodeType.Sequence, children: inlineNodes},
					url: finalUrl,
					escaped: linkParts.isEscaped,
				},
				advance: linkParts.advanceBy,
			};
		}
	} catch {
		return {
			node: {type: NodeType.Text, content: text.slice(0, linkParts.advanceBy)},
			advance: linkParts.advanceBy,
		};
	}

	return null;
}

function extractLinkParts(text: string): {linkText: string; url: string; isEscaped: boolean; advanceBy: number} | null {
	const bracketResult = findClosingBracket(text);
	if (!bracketResult) return null;

	const {bracketPosition, linkText} = bracketResult;

	if (bracketPosition + 1 >= text.length || text.charCodeAt(bracketPosition + 1) !== OPEN_PAREN) return null;

	const trimmedLinkText = linkText.trim();

	if (containsLinkSyntax(trimmedLinkText)) {
		return null;
	}

	const isEmailSpoofing = EMAIL_PATTERN.test(trimmedLinkText);

	if (isEmailSpoofing) {
		return null;
	}

	const urlInfo = extractUrl(text, bracketPosition + 2);
	if (!urlInfo) return null;

	if (urlInfo.url.includes('"') || urlInfo.url.includes("'")) {
		return null;
	}

	const isLinkTextUrlWithProtocol = StringUtils.startsWithUrl(trimmedLinkText);

	if (isLinkTextUrlWithProtocol) {
		if (shouldTreatAsMaskedLink(trimmedLinkText, urlInfo.url)) {
			return null;
		}
	}

	return {
		linkText,
		...urlInfo,
	};
}

function findClosingBracket(text: string): {bracketPosition: number; linkText: string} | null {
	let position = 1;
	let nestedBrackets = 0;
	const textLength = text.length;

	while (position < textLength) {
		const currentChar = text.charCodeAt(position);

		if (currentChar === OPEN_BRACKET) {
			nestedBrackets++;
			position++;
		} else if (currentChar === CLOSE_BRACKET) {
			if (nestedBrackets > 0) {
				nestedBrackets--;
				position++;
			} else {
				return {
					bracketPosition: position,
					linkText: text.slice(1, position),
				};
			}
		} else if (currentChar === BACKSLASH && position + 1 < textLength) {
			position += 2;
		} else {
			position++;
		}

		if (position > MAX_LINK_URL_LENGTH) break;
	}

	return null;
}

function extractUrl(text: string, startPos: number): {url: string; isEscaped: boolean; advanceBy: number} | null {
	if (startPos >= text.length) return null;

	return text.charCodeAt(startPos) === LESS_THAN
		? extractEscapedUrl(text, startPos + 1)
		: extractUnescapedUrl(text, startPos);
}

function extractEscapedUrl(
	text: string,
	urlStart: number,
): {url: string; isEscaped: boolean; advanceBy: number} | null {
	const textLength = text.length;
	let currentPos = urlStart;

	while (currentPos < textLength) {
		if (text.charCodeAt(currentPos) === GREATER_THAN) {
			const url = text.slice(urlStart, currentPos);

			currentPos++;
			while (currentPos < textLength && text.charCodeAt(currentPos) !== CLOSE_PAREN) {
				currentPos++;
			}

			return {
				url,
				isEscaped: true,
				advanceBy: currentPos + 1,
			};
		}
		currentPos++;
	}

	return null;
}

function extractUnescapedUrl(
	text: string,
	urlStart: number,
): {url: string; isEscaped: boolean; advanceBy: number} | null {
	const textLength = text.length;
	let currentPos = urlStart;
	let nestedParens = 0;

	while (currentPos < textLength) {
		const currentChar = text.charCodeAt(currentPos);

		if (currentChar === OPEN_PAREN) {
			nestedParens++;
			currentPos++;
		} else if (currentChar === CLOSE_PAREN) {
			if (nestedParens > 0) {
				nestedParens--;
				currentPos++;
			} else {
				const url = text.slice(urlStart, currentPos);

				return {
					url,
					isEscaped: false,
					advanceBy: currentPos + 1,
				};
			}
		} else {
			currentPos++;
		}
	}

	return null;
}

export function extractUrlSegment(text: string, parserFlags: number): ParserResult | null {
	if (!(parserFlags & ParserFlags.ALLOW_AUTOLINKS)) return null;

	let prefixLength = 0;
	if (text.startsWith('https://')) {
		prefixLength = 8;
	} else if (text.startsWith('http://')) {
		prefixLength = 7;
	} else {
		return null;
	}

	let end = prefixLength;
	const textLength = text.length;
	let parenthesesDepth = 0;

	while (end < textLength) {
		const currentChar = text[end];

		if (currentChar === '(') {
			parenthesesDepth++;
			end++;
		} else if (currentChar === ')') {
			if (parenthesesDepth > 0) {
				parenthesesDepth--;
				end++;
			} else {
				break;
			}
		} else if (StringUtils.isUrlTerminationChar(currentChar)) {
			break;
		} else {
			end++;
		}

		if (end - prefixLength > MAX_LINK_URL_LENGTH) {
			end = prefixLength + MAX_LINK_URL_LENGTH;
			break;
		}
	}

	let urlString = text.slice(0, end);

	const punctuation = '.,;:!?';
	while (
		urlString.length > 0 &&
		punctuation.includes(urlString[urlString.length - 1]) &&
		!urlString.match(/\.[a-zA-Z]{2,}$/)
	) {
		urlString = urlString.slice(0, -1);
		end--;
	}

	const isInQuotes =
		text.charAt(0) === '"' ||
		text.charAt(0) === "'" ||
		(end < textLength && (text.charAt(end) === '"' || text.charAt(end) === "'"));

	try {
		const normalizedUrl = URLUtils.normalizeUrl(urlString);
		const isValid = URLUtils.isValidUrl(normalizedUrl);

		if (isValid) {
			if (normalizedUrl.startsWith('mailto:') || normalizedUrl.startsWith('tel:') || normalizedUrl.startsWith('sms:')) {
				return null;
			}

			const finalUrl = URLUtils.convertToAsciiUrl(normalizedUrl);

			return {
				node: {type: NodeType.Link, text: undefined, url: finalUrl, escaped: isInQuotes},
				advance: urlString.length,
			};
		}
	} catch (_e) {}

	return null;
}

export function parseAutolink(text: string, parserFlags: number): ParserResult | null {
	if (!(parserFlags & ParserFlags.ALLOW_AUTOLINKS)) return null;

	if (text.charCodeAt(0) !== LESS_THAN) return null;

	if (text.length > 1 && (text.charCodeAt(1) === DOUBLE_QUOTE || text.charCodeAt(1) === SINGLE_QUOTE)) {
		return null;
	}

	if (!StringUtils.startsWithUrl(text.slice(1))) return null;

	const end = text.indexOf('>', 1);
	if (end === -1) return null;

	const urlString = text.slice(1, end);
	if (urlString.length > MAX_LINK_URL_LENGTH) return null;

	try {
		const normalizedUrl = URLUtils.normalizeUrl(urlString);
		const isValid = URLUtils.isValidUrl(normalizedUrl);

		if (isValid) {
			if (normalizedUrl.startsWith('mailto:') || normalizedUrl.startsWith('tel:') || normalizedUrl.startsWith('sms:')) {
				return null;
			}

			const finalUrl = URLUtils.convertToAsciiUrl(normalizedUrl);

			return {
				node: {type: NodeType.Link, text: undefined, url: finalUrl, escaped: true},
				advance: end + 1,
			};
		}
	} catch (_e) {}

	return null;
}

export function parseEmailLink(text: string, parserFlags: number): ParserResult | null {
	if (!(parserFlags & ParserFlags.ALLOW_AUTOLINKS)) return null;

	if (text.charCodeAt(0) !== LESS_THAN) return null;

	const end = text.indexOf('>', 1);
	if (end === -1) return null;

	const content = text.slice(1, end);

	if (content.startsWith('http://') || content.startsWith('https://')) return null;
	if (content.charCodeAt(0) === PLUS_SIGN) return null;
	if (content.indexOf('@') === -1) return null;

	const isValid = URLUtils.isValidEmail(content);

	if (isValid) {
		return {
			node: {
				type: NodeType.Link,
				text: {type: NodeType.Text, content: content},
				url: `mailto:${content}`,
				escaped: true,
			},
			advance: end + 1,
		};
	}

	return null;
}

export function parsePhoneLink(text: string, parserFlags: number): ParserResult | null {
	if (!(parserFlags & ParserFlags.ALLOW_AUTOLINKS)) return null;

	if (text.charCodeAt(0) !== LESS_THAN) return null;

	const end = text.indexOf('>', 1);
	if (end === -1) return null;

	const content = text.slice(1, end);

	if (content.charCodeAt(0) !== PLUS_SIGN) return null;

	const isValid = URLUtils.isValidPhoneNumber(content);

	if (isValid) {
		const normalizedPhone = URLUtils.normalizePhoneNumber(content);

		return {
			node: {
				type: NodeType.Link,
				text: {type: NodeType.Text, content: content},
				url: `tel:${normalizedPhone}`,
				escaped: true,
			},
			advance: end + 1,
		};
	}

	return null;
}

export function parseSmsLink(text: string, parserFlags: number): ParserResult | null {
	if (!(parserFlags & ParserFlags.ALLOW_AUTOLINKS)) return null;

	if (text.charCodeAt(0) !== LESS_THAN) return null;

	if (!text.startsWith('<sms:')) return null;

	const end = text.indexOf('>', 1);
	if (end === -1) return null;

	const content = text.slice(1, end);
	const phoneNumber = content.slice(4);

	if (phoneNumber.charCodeAt(0) !== PLUS_SIGN || !URLUtils.isValidPhoneNumber(phoneNumber)) {
		return null;
	}

	const normalizedPhone = URLUtils.normalizePhoneNumber(phoneNumber);

	return {
		node: {
			type: NodeType.Link,
			text: {type: NodeType.Text, content: phoneNumber},
			url: `sms:${normalizedPhone}`,
			escaped: true,
		},
		advance: end + 1,
	};
}

function shouldTreatAsMaskedLink(trimmedLinkText: string, url: string): boolean {
	const normalizedText = trimmedLinkText.trim();

	try {
		const normalizedUrl = URLUtils.normalizeUrl(url);
		const urlObj = new URL(normalizedUrl);
		const textUrl = new URL(normalizedText);

		if (
			urlObj.origin === textUrl.origin &&
			urlObj.pathname === textUrl.pathname &&
			urlObj.search === textUrl.search &&
			urlObj.hash === textUrl.hash
		) {
			return false;
		}
	} catch {}

	return true;
}
