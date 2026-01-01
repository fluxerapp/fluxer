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

import {FormattingContext} from '../parser/formatting-context';
import {MAX_LINE_LENGTH} from '../types/constants';
import {MentionKind, NodeType, ParserFlags} from '../types/enums';
import type {Node, ParserResult} from '../types/nodes';
import * as ASTUtils from '../utils/ast-utils';
import * as StringUtils from '../utils/string-utils';
import * as EmojiParsers from './emoji-parsers';
import * as LinkParsers from './link-parsers';
import * as MentionParsers from './mention-parsers';
import * as TimestampParsers from './timestamp-parsers';

const BACKSLASH = 92;
const UNDERSCORE = 95;
const ASTERISK = 42;
const TILDE = 126;
const PIPE = 124;
const BACKTICK = 96;
const LESS_THAN = 60;
const AT_SIGN = 64;
const HASH = 35;
const AMPERSAND = 38;
const SLASH = 47;
const OPEN_BRACKET = 91;
const COLON = 58;
const LETTER_A = 97;
const LETTER_I = 105;
const LETTER_M = 109;
const LETTER_S = 115;
const LETTER_T = 116;
const PLUS_SIGN = 43;

const FORMATTING_CHARS = new Set([ASTERISK, UNDERSCORE, TILDE, PIPE, BACKTICK]);

const parseInlineCache = new Map<string, Array<Node>>();
const formattingMarkerCache = new Map<string, ReturnType<typeof getFormattingMarkerInfo>>();
const MAX_CACHE_SIZE = 500;
const cacheHitCount = new Map<string, number>();

export function parseInline(text: string, parserFlags: number): Array<Node> {
	if (!text || text.length === 0) {
		return [];
	}

	const cacheKey = `${text}:${parserFlags}`;
	if (parseInlineCache.has(cacheKey)) {
		const cachedResult = parseInlineCache.get(cacheKey)!;

		const hitCount = cacheHitCount.get(cacheKey) || 0;
		cacheHitCount.set(cacheKey, hitCount + 1);

		return [...cachedResult];
	}

	const context = new FormattingContext();

	const nodes = parseInlineWithContext(text, context, parserFlags);

	ASTUtils.flattenAST(nodes);

	if (text.length < 1000) {
		parseInlineCache.set(cacheKey, [...nodes]);
		cacheHitCount.set(cacheKey, 1);

		if (parseInlineCache.size > MAX_CACHE_SIZE) {
			const entries = Array.from(cacheHitCount.entries())
				.sort((a, b) => a[1] - b[1])
				.slice(0, 100);

			for (const [key] of entries) {
				parseInlineCache.delete(key);
				cacheHitCount.delete(key);
			}
		}
	}

	return nodes;
}

function parseInlineWithContext(text: string, context: FormattingContext, parserFlags: number): Array<Node> {
	if (!text) {
		return [];
	}

	const nodes: Array<Node> = [];
	let accumulatedText = '';
	let position = 0;

	const textLength = text.length;

	let characters: Array<string> | null = null;

	while (position < textLength) {
		const currentChar = text.charAt(position);
		const currentCharCode = text.charCodeAt(position);

		if (currentCharCode === BACKSLASH && position + 1 < textLength) {
			const nextChar = text.charAt(position + 1);

			if (nextChar === '_' && position > 0 && text.charAt(position - 1) === '¯') {
				accumulatedText += `\\${nextChar}`;
				position += 2;
				continue;
			}

			if (StringUtils.isEscapableCharacter(nextChar)) {
				accumulatedText += nextChar;
				position += 2;
				continue;
			}
		}

		const remainingText = text.slice(position);

		const insideQuotedAngleBracket = accumulatedText.endsWith('<"') || accumulatedText.endsWith("<'");

		if (
			!insideQuotedAngleBracket &&
			parserFlags & ParserFlags.ALLOW_AUTOLINKS &&
			StringUtils.startsWithUrl(remainingText)
		) {
			const urlResult = LinkParsers.extractUrlSegment(remainingText, parserFlags);

			if (urlResult) {
				if (accumulatedText.length > 0) {
					ASTUtils.addTextNode(nodes, accumulatedText);
					accumulatedText = '';
				}
				nodes.push(urlResult.node);
				position += urlResult.advance;
				continue;
			}
		}

		if (currentCharCode === UNDERSCORE) {
			if (characters == null) {
				characters = [...text];
			}
			const isDoubleUnderscore = position + 1 < textLength && text.charCodeAt(position + 1) === UNDERSCORE;
			if (!isDoubleUnderscore) {
				const isWordUnderscore = StringUtils.isWordUnderscore(characters, position);
				if (isWordUnderscore) {
					accumulatedText += '_';
					position += 1;
					continue;
				}
			}
		}

		const emojiResult = EmojiParsers.parseStandardEmoji(text, position);
		if (emojiResult) {
			if (accumulatedText.length > 0) {
				ASTUtils.addTextNode(nodes, accumulatedText);
				accumulatedText = '';
			}
			nodes.push(emojiResult.node);
			position += emojiResult.advance;
			continue;
		}

		if (currentCharCode === LESS_THAN && position + 2 < textLength) {
			const nextCharCode = text.charCodeAt(position + 1);
			const thirdCharCode = position + 2 < textLength ? text.charCodeAt(position + 2) : 0;
			if (nextCharCode === COLON || (nextCharCode === LETTER_A && thirdCharCode === COLON)) {
				const customEmojiResult = EmojiParsers.parseCustomEmoji(remainingText);

				if (customEmojiResult) {
					if (accumulatedText.length > 0) {
						ASTUtils.addTextNode(nodes, accumulatedText);
						accumulatedText = '';
					}
					nodes.push(customEmojiResult.node);
					position += customEmojiResult.advance;
					continue;
				}
			}
		}

		if (
			currentCharCode === LESS_THAN &&
			position + 3 < textLength &&
			text.charCodeAt(position + 1) === LETTER_T &&
			text.charCodeAt(position + 2) === COLON
		) {
			const timestampResult = TimestampParsers.parseTimestamp(remainingText);

			if (timestampResult) {
				if (accumulatedText.length > 0) {
					ASTUtils.addTextNode(nodes, accumulatedText);
					accumulatedText = '';
				}
				nodes.push(timestampResult.node);
				position += timestampResult.advance;
				continue;
			}
		}

		if (currentCharCode === COLON) {
			const emojiResult = EmojiParsers.parseEmojiShortcode(remainingText);

			if (emojiResult) {
				if (accumulatedText.length > 0) {
					ASTUtils.addTextNode(nodes, accumulatedText);
					accumulatedText = '';
				}
				nodes.push(emojiResult.node);
				position += emojiResult.advance;
				continue;
			}
		}

		if (
			currentCharCode === LESS_THAN &&
			position + 1 < textLength &&
			text.charCodeAt(position + 1) === PLUS_SIGN &&
			parserFlags & ParserFlags.ALLOW_AUTOLINKS
		) {
			const phoneResult = LinkParsers.parsePhoneLink(remainingText, parserFlags);

			if (phoneResult) {
				if (accumulatedText.length > 0) {
					ASTUtils.addTextNode(nodes, accumulatedText);
					accumulatedText = '';
				}
				nodes.push(phoneResult.node);
				position += phoneResult.advance;
				continue;
			}
		}

		if (
			currentCharCode === LESS_THAN &&
			position + 4 < textLength &&
			text.charCodeAt(position + 1) === LETTER_S &&
			text.charCodeAt(position + 2) === LETTER_M &&
			text.charCodeAt(position + 3) === LETTER_S &&
			text.charCodeAt(position + 4) === COLON &&
			parserFlags & ParserFlags.ALLOW_AUTOLINKS
		) {
			const smsResult = LinkParsers.parseSmsLink(remainingText, parserFlags);

			if (smsResult) {
				if (accumulatedText.length > 0) {
					ASTUtils.addTextNode(nodes, accumulatedText);
					accumulatedText = '';
				}
				nodes.push(smsResult.node);
				position += smsResult.advance;
				continue;
			}
		}

		if (currentCharCode === LESS_THAN && position + 1 < textLength) {
			const nextCharCode = text.charCodeAt(position + 1);

			if (
				nextCharCode === AT_SIGN ||
				nextCharCode === HASH ||
				nextCharCode === AMPERSAND ||
				nextCharCode === SLASH ||
				nextCharCode === LETTER_I
			) {
				if (
					nextCharCode === AT_SIGN &&
					position + 2 < textLength &&
					text.charCodeAt(position + 2) === AMPERSAND &&
					parserFlags & ParserFlags.ALLOW_ROLE_MENTIONS
				) {
					const mentionResult = MentionParsers.parseMention(remainingText, parserFlags);
					if (mentionResult) {
						if (accumulatedText.length > 0) {
							ASTUtils.addTextNode(nodes, accumulatedText);
							accumulatedText = '';
						}
						nodes.push(mentionResult.node);
						position += mentionResult.advance;
						continue;
					}
				} else if (nextCharCode === AT_SIGN && parserFlags & ParserFlags.ALLOW_USER_MENTIONS) {
					const mentionResult = MentionParsers.parseMention(remainingText, parserFlags);
					if (mentionResult) {
						if (accumulatedText.length > 0) {
							ASTUtils.addTextNode(nodes, accumulatedText);
							accumulatedText = '';
						}
						nodes.push(mentionResult.node);
						position += mentionResult.advance;
						continue;
					}
				} else if (nextCharCode === HASH && parserFlags & ParserFlags.ALLOW_CHANNEL_MENTIONS) {
					const mentionResult = MentionParsers.parseMention(remainingText, parserFlags);
					if (mentionResult) {
						if (accumulatedText.length > 0) {
							ASTUtils.addTextNode(nodes, accumulatedText);
							accumulatedText = '';
						}
						nodes.push(mentionResult.node);
						position += mentionResult.advance;
						continue;
					}
				} else if (nextCharCode === SLASH && parserFlags & ParserFlags.ALLOW_COMMAND_MENTIONS) {
					const mentionResult = MentionParsers.parseMention(remainingText, parserFlags);
					if (mentionResult) {
						if (accumulatedText.length > 0) {
							ASTUtils.addTextNode(nodes, accumulatedText);
							accumulatedText = '';
						}
						nodes.push(mentionResult.node);
						position += mentionResult.advance;
						continue;
					}
				} else if (
					nextCharCode === LETTER_I &&
					remainingText.startsWith('<id:') &&
					parserFlags & ParserFlags.ALLOW_GUILD_NAVIGATIONS
				) {
					const mentionResult = MentionParsers.parseMention(remainingText, parserFlags);
					if (mentionResult) {
						if (accumulatedText.length > 0) {
							ASTUtils.addTextNode(nodes, accumulatedText);
							accumulatedText = '';
						}
						nodes.push(mentionResult.node);
						position += mentionResult.advance;
						continue;
					}
				}
			}

			if (parserFlags & ParserFlags.ALLOW_AUTOLINKS) {
				const autolinkResult = LinkParsers.parseAutolink(remainingText, parserFlags);

				if (autolinkResult) {
					if (accumulatedText.length > 0) {
						ASTUtils.addTextNode(nodes, accumulatedText);
						accumulatedText = '';
					}
					nodes.push(autolinkResult.node);
					position += autolinkResult.advance;
					continue;
				}

				// Try email links: <user@example.com>
				const emailLinkResult = LinkParsers.parseEmailLink(remainingText, parserFlags);

				if (emailLinkResult) {
					if (accumulatedText.length > 0) {
						ASTUtils.addTextNode(nodes, accumulatedText);
						accumulatedText = '';
					}
					nodes.push(emailLinkResult.node);
					position += emailLinkResult.advance;
					continue;
				}
			}
		}

		if (currentCharCode === AT_SIGN && parserFlags & ParserFlags.ALLOW_EVERYONE_MENTIONS) {
			const isEscaped = position > 0 && text.charCodeAt(position - 1) === BACKSLASH;

			if (!isEscaped && remainingText.startsWith('@everyone')) {
				if (accumulatedText.length > 0) {
					ASTUtils.addTextNode(nodes, accumulatedText);
					accumulatedText = '';
				}
				nodes.push({
					type: NodeType.Mention,
					kind: {kind: MentionKind.Everyone},
				});
				position += 9;
				continue;
			}

			if (!isEscaped && remainingText.startsWith('@here')) {
				if (accumulatedText.length > 0) {
					ASTUtils.addTextNode(nodes, accumulatedText);
					accumulatedText = '';
				}
				nodes.push({
					type: NodeType.Mention,
					kind: {kind: MentionKind.Here},
				});
				position += 5;
				continue;
			}
		}

		const isDoubleUnderscore =
			currentCharCode === UNDERSCORE && position + 1 < textLength && text.charCodeAt(position + 1) === UNDERSCORE;

		if (
			(FORMATTING_CHARS.has(currentCharCode) || currentCharCode === OPEN_BRACKET) &&
			(isDoubleUnderscore ||
				!(
					currentCharCode === UNDERSCORE &&
					accumulatedText.length > 0 &&
					StringUtils.isAlphaNumeric(accumulatedText.charCodeAt(accumulatedText.length - 1))
				))
		) {
			context.setCurrentText(accumulatedText);

			const specialResult = parseSpecialSequence(remainingText, context, parserFlags);

			if (specialResult) {
				if (accumulatedText.length > 0) {
					ASTUtils.addTextNode(nodes, accumulatedText);
					accumulatedText = '';
				}
				nodes.push(specialResult.node);
				position += specialResult.advance;
				continue;
			}
		}

		accumulatedText += currentChar;
		position += 1;

		if (accumulatedText.length > MAX_LINE_LENGTH) {
			ASTUtils.addTextNode(nodes, accumulatedText);
			accumulatedText = '';
			break;
		}
	}

	if (accumulatedText.length > 0) {
		ASTUtils.addTextNode(nodes, accumulatedText);
	}

	const result = ASTUtils.mergeTextNodes(nodes);

	return result;
}

function parseSpecialSequence(text: string, context: FormattingContext, parserFlags: number): ParserResult | null {
	if (text.length === 0) return null;

	const firstCharCode = text.charCodeAt(0);

	switch (firstCharCode) {
		case LESS_THAN:
			if (text.length > 1) {
				const nextCharCode = text.charCodeAt(1);

				if (nextCharCode === SLASH) {
					if (parserFlags & ParserFlags.ALLOW_COMMAND_MENTIONS) {
						const mentionResult = MentionParsers.parseMention(text, parserFlags);

						if (mentionResult) return mentionResult;
					}
				} else if (nextCharCode === LETTER_I && text.startsWith('<id:')) {
					if (parserFlags & ParserFlags.ALLOW_GUILD_NAVIGATIONS) {
						const mentionResult = MentionParsers.parseMention(text, parserFlags);

						if (mentionResult) return mentionResult;
					}
				} else if (nextCharCode === PLUS_SIGN && parserFlags & ParserFlags.ALLOW_AUTOLINKS) {
					const phoneResult = LinkParsers.parsePhoneLink(text, parserFlags);

					if (phoneResult) return phoneResult;
				} else if (
					nextCharCode === LETTER_S &&
					text.length > 4 &&
					text.charCodeAt(2) === LETTER_S &&
					text.charCodeAt(3) === COLON &&
					parserFlags & ParserFlags.ALLOW_AUTOLINKS
				) {
					const smsResult = LinkParsers.parseSmsLink(text, parserFlags);

					if (smsResult) return smsResult;
				}
			}

			break;

		case ASTERISK:
		case UNDERSCORE:
		case TILDE:
		case PIPE:
		case BACKTICK: {
			const formattingResult = parseFormatting(text, context, parserFlags);

			if (formattingResult) return formattingResult;

			break;
		}

		case AT_SIGN:
			if (parserFlags & ParserFlags.ALLOW_EVERYONE_MENTIONS) {
				if (text.startsWith('@everyone')) {
					return {
						node: {
							type: NodeType.Mention,
							kind: {kind: MentionKind.Everyone},
						},
						advance: 9,
					};
				}

				if (text.startsWith('@here')) {
					return {
						node: {
							type: NodeType.Mention,
							kind: {kind: MentionKind.Here},
						},
						advance: 5,
					};
				}
			}

			break;

		case OPEN_BRACKET: {
			const timestampResult = TimestampParsers.parseTimestamp(text);

			if (timestampResult) return timestampResult;

			if (parserFlags & ParserFlags.ALLOW_MASKED_LINKS) {
				const linkResult = LinkParsers.parseLink(text, parserFlags, (t) => parseInline(t, parserFlags));

				if (linkResult) return linkResult;
			}

			break;
		}
	}

	if (firstCharCode !== OPEN_BRACKET) {
		const timestampResult = TimestampParsers.parseTimestamp(text);

		if (timestampResult) return timestampResult;
	}

	if (firstCharCode !== LESS_THAN && firstCharCode !== OPEN_BRACKET && parserFlags & ParserFlags.ALLOW_MASKED_LINKS) {
		const linkResult = LinkParsers.parseLink(text, parserFlags, (t) => parseInline(t, parserFlags));

		if (linkResult) return linkResult;
	}

	return null;
}

function parseFormatting(text: string, context: FormattingContext, parserFlags: number): ParserResult | null {
	if (text.length < 2) {
		return null;
	}

	let markerInfo: FormattingMarkerInfo | null | undefined;
	const prefix = text.slice(0, Math.min(3, text.length));

	if (formattingMarkerCache.has(prefix)) {
		markerInfo = formattingMarkerCache.get(prefix);
		const hitCount = cacheHitCount.get(prefix) || 0;
		cacheHitCount.set(prefix, hitCount + 1);
	} else {
		markerInfo = getFormattingMarkerInfo(text);
		formattingMarkerCache.set(prefix, markerInfo);
		cacheHitCount.set(prefix, 1);

		if (formattingMarkerCache.size > MAX_CACHE_SIZE) {
			const entries = Array.from(cacheHitCount.entries())
				.filter(([key]) => formattingMarkerCache.has(key))
				.sort((a, b) => a[1] - b[1])
				.slice(0, 50);

			for (const [key] of entries) {
				formattingMarkerCache.delete(key);
				cacheHitCount.delete(key);
			}
		}
	}

	if (!markerInfo) return null;

	const {marker, nodeType, markerLength} = markerInfo;

	if (nodeType === NodeType.Spoiler && !(parserFlags & ParserFlags.ALLOW_SPOILERS)) {
		return null;
	}

	if (!context.canEnterFormatting(marker[0], marker.length > 1)) return null;

	const endResult = findFormattingEnd(text, marker, markerLength, nodeType);

	if (!endResult) return null;

	const {endPosition, innerContent} = endResult;
	const isBlock = context.isFormattingActive(marker[0], marker.length > 1);

	const formattingNode = createFormattingNode(
		nodeType,
		innerContent,
		marker,
		isBlock,
		(text: string, ctx: FormattingContext) => parseInlineWithContext(text, ctx, parserFlags),
	);

	return {node: formattingNode, advance: endPosition + markerLength};
}

interface FormattingMarkerInfo {
	marker: string;
	nodeType: NodeType;
	markerLength: number;
}

function getFormattingMarkerInfo(text: string): FormattingMarkerInfo | null {
	if (!text || text.length === 0) return null;

	const firstCharCode = text.charCodeAt(0);

	if (!FORMATTING_CHARS.has(firstCharCode)) return null;

	const secondCharCode = text.length > 1 ? text.charCodeAt(1) : 0;
	const thirdCharCode = text.length > 2 ? text.charCodeAt(2) : 0;

	if (firstCharCode === ASTERISK && secondCharCode === ASTERISK && thirdCharCode === ASTERISK) {
		return {marker: '***', nodeType: NodeType.Emphasis, markerLength: 3};
	}
	if (firstCharCode === UNDERSCORE && secondCharCode === UNDERSCORE && thirdCharCode === UNDERSCORE) {
		return {marker: '___', nodeType: NodeType.Emphasis, markerLength: 3};
	}

	if (firstCharCode === PIPE && secondCharCode === PIPE) {
		return {marker: '||', nodeType: NodeType.Spoiler, markerLength: 2};
	}
	if (firstCharCode === TILDE && secondCharCode === TILDE) {
		return {marker: '~~', nodeType: NodeType.Strikethrough, markerLength: 2};
	}
	if (firstCharCode === ASTERISK && secondCharCode === ASTERISK) {
		return {marker: '**', nodeType: NodeType.Strong, markerLength: 2};
	}
	if (firstCharCode === UNDERSCORE && secondCharCode === UNDERSCORE) {
		return {marker: '__', nodeType: NodeType.Underline, markerLength: 2};
	}

	if (firstCharCode === BACKTICK) {
		let backtickCount = 1;
		while (backtickCount < text.length && text.charCodeAt(backtickCount) === BACKTICK) {
			backtickCount++;
		}
		return {marker: '`'.repeat(backtickCount), nodeType: NodeType.InlineCode, markerLength: backtickCount};
	}
	if (firstCharCode === ASTERISK) {
		return {marker: '*', nodeType: NodeType.Emphasis, markerLength: 1};
	}
	if (firstCharCode === UNDERSCORE) {
		return {marker: '_', nodeType: NodeType.Emphasis, markerLength: 1};
	}

	return null;
}

function findFormattingEnd(
	text: string,
	marker: string,
	markerLength: number,
	nodeType: NodeType,
): {endPosition: number; innerContent: string} | null {
	let position = markerLength;
	let nestedLevel = 0;
	let endPosition: number | null = null;
	const textLength = text.length;

	if (textLength < markerLength * 2) return null;

	if (nodeType === NodeType.InlineCode && markerLength > 1) {
		while (position < textLength) {
			if (text.charCodeAt(position) === BACKTICK) {
				let backtickCount = 0;
				let checkPos = position;
				while (checkPos < textLength && text.charCodeAt(checkPos) === BACKTICK) {
					backtickCount++;
					checkPos++;
				}

				if (backtickCount === markerLength) {
					endPosition = position;
					break;
				}

				position = checkPos;
				continue;
			}
			position++;
			if (position > MAX_LINE_LENGTH) break;
		}

		if (endPosition == null) return null;

		return {
			endPosition,
			innerContent: text.slice(markerLength, endPosition),
		};
	}

	if (markerLength === 1 && (nodeType === NodeType.Emphasis || nodeType === NodeType.InlineCode)) {
		const markerChar = marker.charCodeAt(0);
		while (position < textLength) {
			const currentChar = text.charCodeAt(position);

			if (currentChar === BACKSLASH && position + 1 < textLength) {
				position += 2;
				continue;
			}

			if (currentChar === markerChar) {
				if (markerChar === BACKTICK && position + 1 < textLength && text.charCodeAt(position + 1) === BACKTICK) {
					let _backtickCount = 0;
					let checkPos = position;
					while (checkPos < textLength && text.charCodeAt(checkPos) === BACKTICK) {
						_backtickCount++;
						checkPos++;
					}
					position = checkPos;
					continue;
				}

				if (markerChar === UNDERSCORE && position + 1 < textLength && text.charCodeAt(position + 1) === UNDERSCORE) {
					position += 2;
					continue;
				}

				endPosition = position;
				break;
			}

			position++;
			if (position > MAX_LINE_LENGTH) break;
		}

		if (endPosition == null) return null;

		return {
			endPosition,
			innerContent: text.slice(markerLength, endPosition),
		};
	}

	if (nodeType === NodeType.InlineCode) {
		while (position < textLength) {
			if (text.charCodeAt(position) === BACKTICK) {
				endPosition = position;
				break;
			}
			position++;
			if (position > MAX_LINE_LENGTH) break;
		}
	} else {
		const firstMarkerChar = marker.charCodeAt(0);
		const isDoubleMarker = marker.length > 1;

		while (position < textLength) {
			if (text.charCodeAt(position) === BACKSLASH && position + 1 < textLength) {
				position += 2;
				continue;
			}

			let isClosingMarker = true;
			if (position + marker.length <= textLength) {
				for (let i = 0; i < marker.length; i++) {
					if (text.charCodeAt(position + i) !== marker.charCodeAt(i)) {
						isClosingMarker = false;
						break;
					}
				}
			} else {
				isClosingMarker = false;
			}

			if (isClosingMarker) {
				if (nestedLevel === 0) {
					if (nodeType === NodeType.Spoiler && position === markerLength && position + marker.length < textLength) {
						position += 1;
						continue;
					}
					endPosition = position;
					break;
				}
				nestedLevel--;
				position += marker.length;
				continue;
			}

			if (
				isDoubleMarker &&
				position + 1 < textLength &&
				text.charCodeAt(position) === firstMarkerChar &&
				text.charCodeAt(position + 1) === firstMarkerChar
			) {
				nestedLevel++;
			}

			position++;
			if (position > MAX_LINE_LENGTH) break;
		}
	}

	if (endPosition == null) return null;

	const innerContent = text.slice(markerLength, endPosition);
	return {endPosition, innerContent};
}

function createFormattingNode(
	nodeType: NodeType,
	innerContent: string,
	marker: string,
	isBlock: boolean,
	parseInlineWithContext: (text: string, context: FormattingContext) => Array<Node>,
): Node {
	if (nodeType === NodeType.InlineCode) {
		return {type: NodeType.InlineCode, content: innerContent};
	}

	if (innerContent.length === 0) {
		return {
			type: nodeType as any,
			children: [],
			...(isBlock ? {isBlock} : {}),
		};
	}

	const newContext = new FormattingContext();
	newContext.pushFormatting(marker[0], marker.length > 1);

	if (marker === '***' || marker === '___') {
		const emphasisContext = new FormattingContext();
		emphasisContext.pushFormatting('*', true);

		const innerNodes = parseInlineWithContext(innerContent, emphasisContext);

		return {
			type: NodeType.Emphasis,
			children: [{type: NodeType.Strong, children: innerNodes}],
		};
	}

	const innerNodes = parseInlineWithContext(innerContent, newContext);

	return {
		type: nodeType as any,
		children: innerNodes,
		...(isBlock || nodeType === NodeType.Spoiler ? {isBlock} : {}),
	};
}
