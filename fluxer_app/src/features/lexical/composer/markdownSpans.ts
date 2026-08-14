// SPDX-License-Identifier: AGPL-3.0-or-later

import {ParserFlags} from '@app/features/messaging/utils/markdown/parser/Enums';
import {parseMarkdownAstWithWasm} from '@app/features/messaging/utils/markdown/parser/MarkdownParserWasm';
import type {Node} from '@app/features/messaging/utils/markdown/parser/Nodes';
import {findUrlEnd} from '@app/features/messaging/utils/markdown/UrlSpanUtils';

export const MarkdownHl = {
	none: 0,
	bold: 1 << 0,
	italic: 1 << 1,
	underline: 1 << 2,
	strike: 1 << 3,
	spoiler: 1 << 4,
	code: 1 << 5,
	heading: 1 << 6,
	blockquote: 1 << 7,
	subtext: 1 << 8,
	link: 1 << 9,
	codeBlock: 1 << 10,
} as const;

export type MarkdownHlFormat = number;

export interface MarkdownSpan {
	start: number;
	end: number;
	role: 'marker' | 'content';
	format: MarkdownHlFormat;
}

export const DEFAULT_COMPOSER_MARKDOWN_FLAGS =
	ParserFlags.ALLOW_SPOILERS |
	ParserFlags.ALLOW_HEADINGS |
	ParserFlags.ALLOW_CODE_BLOCKS |
	ParserFlags.ALLOW_BLOCKQUOTES |
	ParserFlags.ALLOW_MULTILINE_BLOCKQUOTES |
	ParserFlags.ALLOW_SUBTEXT |
	ParserFlags.ALLOW_LISTS |
	ParserFlags.ALLOW_MASKED_LINKS |
	ParserFlags.ALLOW_AUTOLINKS |
	ParserFlags.ALLOW_USER_MENTIONS |
	ParserFlags.ALLOW_ROLE_MENTIONS |
	ParserFlags.ALLOW_CHANNEL_MENTIONS |
	ParserFlags.ALLOW_EVERYONE_MENTIONS |
	ParserFlags.ALLOW_COMMAND_MENTIONS |
	ParserFlags.ALLOW_GUILD_NAVIGATIONS |
	ParserFlags.ALLOW_TABLES |
	ParserFlags.ALLOW_ALERTS;

interface AlignContext {
	source: string;
	pos: number;
	spans: Array<MarkdownSpan>;
	failed: boolean;
	listIndentLevel: number | null;
}

export function computeMarkdownHighlightSpans(
	source: string,
	parserFlags = DEFAULT_COMPOSER_MARKDOWN_FLAGS,
): Array<MarkdownSpan> {
	if (source.length === 0) {
		return [];
	}
	let nodes: Array<Node>;
	try {
		nodes = parseMarkdownAstWithWasm(source, parserFlags & DEFAULT_COMPOSER_MARKDOWN_FLAGS).nodes;
	} catch {
		return [{start: 0, end: source.length, role: 'content', format: MarkdownHl.none}];
	}
	const ctx: AlignContext = {source, pos: 0, spans: [], failed: false, listIndentLevel: null};
	while (ctx.pos < source.length) {
		const blank = /^[ \t\r]*\n/.exec(source.slice(ctx.pos));
		if (blank == null) {
			break;
		}
		pushContent(ctx, ctx.pos + blank[0].length, MarkdownHl.none);
	}
	for (let index = 0; index < nodes.length; index += 1) {
		if (ctx.failed) {
			break;
		}
		if (index > 0) {
			advanceSequenceGap(nodes, index, MarkdownHl.none, ctx);
		}
		alignNode(nodes[index]!, MarkdownHl.none, ctx);
	}
	if (ctx.failed) {
		return [{start: 0, end: source.length, role: 'content', format: MarkdownHl.none}];
	}
	if (ctx.pos < source.length) {
		if (!/^[ \t\r\n]*$/.test(source.slice(ctx.pos))) {
			return [{start: 0, end: source.length, role: 'content', format: MarkdownHl.none}];
		}
		pushContent(ctx, source.length, MarkdownHl.none);
	}
	return coalesce(ctx.spans);
}

function pushMarker(ctx: AlignContext, length: number, format: MarkdownHlFormat): void {
	if (length <= 0) {
		return;
	}
	ctx.spans.push({start: ctx.pos, end: ctx.pos + length, role: 'marker', format});
	ctx.pos += length;
}

function pushContent(ctx: AlignContext, endPos: number, format: MarkdownHlFormat): void {
	if (endPos > ctx.pos) {
		ctx.spans.push({start: ctx.pos, end: endPos, role: 'content', format});
		ctx.pos = endPos;
	}
}

function expectMarker(ctx: AlignContext, marker: string, format: MarkdownHlFormat): boolean {
	if (ctx.source.startsWith(marker, ctx.pos)) {
		pushMarker(ctx, marker.length, format);
		return true;
	}
	ctx.failed = true;
	return false;
}

function alignChildren(children: Array<Node>, format: MarkdownHlFormat, ctx: AlignContext): void {
	for (const child of children) {
		if (ctx.failed) {
			return;
		}
		alignNode(child, format, ctx);
	}
}

const WRAPPERS: Partial<Record<Node['type'], {marker: string; bit: number}>> = {
	Underline: {marker: '__', bit: MarkdownHl.underline},
	Strikethrough: {marker: '~~', bit: MarkdownHl.strike},
};

function alignNode(node: Node, format: MarkdownHlFormat, ctx: AlignContext): void {
	const wrapper = WRAPPERS[node.type];
	if (wrapper != null && 'children' in node) {
		const next = format | wrapper.bit;
		if (!expectMarker(ctx, wrapper.marker, next)) {
			return;
		}
		alignChildren(node.children, next, ctx);
		expectMarker(ctx, wrapper.marker, next);
		return;
	}
	switch (node.type) {
		case 'Text': {
			const end = advancePastText(ctx.source, ctx.pos, node.content, ctx.listIndentLevel);
			if (end == null) {
				ctx.failed = true;
				return;
			}
			pushContent(ctx, end, format);
			return;
		}
		case 'Strong': {
			const delimiter = ctx.source.startsWith('**', ctx.pos)
				? '**'
				: ctx.source.startsWith('__', ctx.pos)
					? '__'
					: null;
			if (delimiter == null) {
				ctx.failed = true;
				return;
			}
			const next = format | MarkdownHl.bold;
			pushMarker(ctx, delimiter.length, next);
			alignChildren(node.children, next, ctx);
			expectMarker(ctx, delimiter, next);
			return;
		}
		case 'Emphasis': {
			const delimiter = ctx.source[ctx.pos];
			if (delimiter !== '*' && delimiter !== '_') {
				ctx.failed = true;
				return;
			}
			const next = format | MarkdownHl.italic;
			pushMarker(ctx, 1, next);
			alignChildren(node.children, next, ctx);
			if (ctx.source[ctx.pos] === delimiter) {
				pushMarker(ctx, 1, next);
			} else {
				ctx.failed = true;
			}
			return;
		}
		case 'Spoiler': {
			const next = format | MarkdownHl.spoiler;
			if ('isBlock' in node && node.isBlock) {
				alignBlockSpoiler(node.children, next, ctx);
				return;
			}
			if (!expectMarker(ctx, '||', next)) {
				return;
			}
			alignChildren(node.children, next, ctx);
			expectMarker(ctx, '||', next);
			return;
		}
		case 'InlineCode': {
			alignInlineCode(node.content, format, ctx);
			return;
		}
		case 'CodeBlock': {
			alignCodeBlock(node, format, ctx);
			return;
		}
		case 'Heading': {
			const match = /^[ \t\r]*#{1,4} /.exec(ctx.source.slice(ctx.pos));
			if (match == null) {
				ctx.failed = true;
				return;
			}
			pushContent(ctx, ctx.pos + match[0].length, format);
			alignChildren(node.children, format | MarkdownHl.heading, ctx);
			return;
		}
		case 'Subtext': {
			alignLinePrefixed(node.children, format | MarkdownHl.subtext, ctx, /^[ \t\r]*-# /);
			return;
		}
		case 'Blockquote': {
			alignBlockquote(node.children, format | MarkdownHl.blockquote, ctx);
			return;
		}
		case 'Sequence': {
			alignChildren(node.children, format, ctx);
			return;
		}
		case 'Link': {
			alignLink(node, format, ctx);
			return;
		}
		case 'List': {
			alignList(node, format, ctx);
			return;
		}
		case 'Emoji': {
			alignOpaque(EMOJI_TOKEN_RE, node.kind.kind === 'Standard' ? node.kind.raw : null, format, ctx);
			return;
		}
		case 'Timestamp': {
			alignOpaque(TIMESTAMP_TOKEN_RE, null, format, ctx);
			return;
		}
		case 'Mention': {
			alignOpaque(MENTION_TOKEN_RE, null, format, ctx);
			return;
		}
		case 'Alert': {
			alignAlert(format, ctx);
			return;
		}
		case 'Table': {
			alignTable(node, format, ctx);
			return;
		}
		default: {
			ctx.failed = true;
			return;
		}
	}
}

const EMOJI_TOKEN_RE = /^(?:<a?:[A-Za-z0-9_+~-]+:\d+>|:[\p{L}\p{N}_+~.-]{2,}:(?::skin-tone-[1-5]:)?)/u;
const TIMESTAMP_TOKEN_RE = /^<t:-?\d+(?::[tTdDfFsSR])?>/;
const MENTION_TOKEN_RE =
	/^(?:<@!?\d+>|<@&\d+>|<#\d+>|<\/[^:>\n]+:\d+>|<id:(?:customize|browse|guide|linked-roles(?::[^:>\n]*)?)>|@everyone|@here)/;

function alignOpaque(tokenRe: RegExp, literal: string | null, format: MarkdownHlFormat, ctx: AlignContext): void {
	const match = tokenRe.exec(ctx.source.slice(ctx.pos));
	if (match != null) {
		pushContent(ctx, ctx.pos + match[0].length, format);
		return;
	}
	if (literal != null && ctx.source.startsWith(literal, ctx.pos)) {
		pushContent(ctx, ctx.pos + literal.length, format);
		return;
	}
	ctx.failed = true;
}

function alignLink(node: Extract<Node, {type: 'Link'}>, format: MarkdownHlFormat, ctx: AlignContext): void {
	if (node.text == null) {
		if (ctx.source[ctx.pos] === '<') {
			const close = ctx.source.indexOf('>', ctx.pos + 1);
			if (close === -1) {
				ctx.failed = true;
				return;
			}
			pushContent(ctx, ctx.pos + 1, format);
			pushContent(ctx, close, format | MarkdownHl.link);
			pushContent(ctx, close + 1, format);
			return;
		}
		const end = findUrlEnd(ctx.source, ctx.pos);
		if (end <= ctx.pos) {
			ctx.failed = true;
			return;
		}
		pushContent(ctx, end, format | MarkdownHl.link);
		return;
	}
	if (!expectMarker(ctx, '[', format)) {
		return;
	}
	alignNode(node.text, format | MarkdownHl.link, ctx);
	if (ctx.failed) {
		return;
	}
	if (!expectMarker(ctx, ']', format) || !expectMarker(ctx, '(', format)) {
		return;
	}
	const urlEnd = scanMaskedUrlEnd(ctx.source, ctx.pos);
	if (urlEnd < 0) {
		ctx.failed = true;
		return;
	}
	pushMarker(ctx, urlEnd - ctx.pos, format);
	expectMarker(ctx, ')', format);
}

function scanMaskedUrlEnd(source: string, start: number): number {
	if (source[start] === '<') {
		const close = source.indexOf('>', start + 1);
		if (close === -1) {
			return -1;
		}
		return source.indexOf(')', close + 1);
	}
	let depth = 0;
	for (let i = start; i < source.length; i += 1) {
		const ch = source[i]!;
		if (ch === '(') {
			depth += 1;
		} else if (ch === ')') {
			if (depth === 0) {
				return i;
			}
			depth -= 1;
		}
	}
	return -1;
}

const UNORDERED_LIST_RE = /^( *)([-*]) /;
const ORDERED_LIST_RE = /^( *)(\d+)\. /;

function alignList(node: Extract<Node, {type: 'List'}>, format: MarkdownHlFormat, ctx: AlignContext): void {
	for (let index = 0; index < node.items.length; index += 1) {
		if (ctx.failed) {
			return;
		}
		if (index > 0 && ctx.source[ctx.pos] === '\n') {
			pushContent(ctx, ctx.pos + 1, format);
		}
		const item = node.items[index]!;
		const match = (node.ordered ? ORDERED_LIST_RE : UNORDERED_LIST_RE).exec(ctx.source.slice(ctx.pos));
		if (match == null) {
			ctx.failed = true;
			return;
		}
		pushContent(ctx, ctx.pos + match[0].length, format);
		alignListItemChildren(item.children, format, Math.floor(match[1]!.length / 2), ctx);
	}
}

function alignListItemChildren(
	children: Array<Node>,
	format: MarkdownHlFormat,
	indentLevel: number,
	ctx: AlignContext,
): void {
	const parentIndentLevel = ctx.listIndentLevel;
	ctx.listIndentLevel = indentLevel;
	for (let index = 0; index < children.length; index += 1) {
		const child = children[index]!;
		alignListItemChildGap(child, format, indentLevel, ctx);
		alignNode(child, format, ctx);
		if (ctx.failed) {
			break;
		}
	}
	ctx.listIndentLevel = parentIndentLevel;
}

function alignListItemChildGap(child: Node, format: MarkdownHlFormat, indentLevel: number, ctx: AlignContext): void {
	const continuationEnd = listContinuationEnd(ctx.source, ctx.pos, indentLevel);
	if (continuationEnd === ctx.pos) {
		return;
	}
	if (child.type !== 'List' && child.type !== 'CodeBlock') {
		pushContent(ctx, continuationEnd, format);
		return;
	}
	const childLineStart = ctx.source.lastIndexOf('\n', continuationEnd - 1) + 1;
	pushContent(ctx, childLineStart, format);
}

function listContinuationEnd(source: string, start: number, indentLevel: number): number {
	let position = start;
	while (source[position] === '\n') {
		let contentStart = position + 1;
		let spaces = 0;
		while (source[contentStart + spaces] === ' ') {
			spaces += 1;
		}
		if (spaces <= indentLevel * 2) {
			break;
		}
		contentStart += spaces;
		while (source[contentStart] === '\t' || source[contentStart] === '\r') {
			contentStart += 1;
		}
		position = contentStart;
	}
	return position;
}

function alignCodeBlock(node: Extract<Node, {type: 'CodeBlock'}>, format: MarkdownHlFormat, ctx: AlignContext): void {
	const start = ctx.pos;
	let fenceStart = start;
	while (ctx.source[fenceStart] === ' ' || ctx.source[fenceStart] === '\t' || ctx.source[fenceStart] === '\r') {
		fenceStart += 1;
	}
	let fenceLength = 0;
	while (ctx.source[fenceStart + fenceLength] === '`') {
		fenceLength += 1;
	}
	if (fenceLength < 3) {
		ctx.failed = true;
		return;
	}
	const fence = '`'.repeat(fenceLength);
	const openingLineEnd = ctx.source.indexOf('\n', fenceStart + fenceLength);
	const lineEnd = openingLineEnd < 0 ? ctx.source.length : openingLineEnd;
	const languagePartStart = fenceStart + fenceLength;
	const languagePart = ctx.source.slice(languagePartStart, lineEnd);
	const inlineClosing = languagePart.indexOf(fence);
	const next = format | MarkdownHl.code | MarkdownHl.codeBlock;
	if (inlineClosing >= 0) {
		const contentEnd = languagePartStart + inlineClosing;
		if (node.language != null || ctx.source.slice(languagePartStart, contentEnd) !== node.content) {
			ctx.failed = true;
			return;
		}
		pushMarker(ctx, fenceStart + fenceLength - ctx.pos, next);
		pushContent(ctx, contentEnd, next);
		pushMarker(ctx, fenceLength, next);
		return;
	}
	if (openingLineEnd < 0) {
		ctx.failed = true;
		return;
	}
	const language = node.language;
	if (language != null && trimCodeFenceInfo(languagePart) !== language) {
		ctx.failed = true;
		return;
	}
	const openingIndent = ctx.source.slice(start, fenceStart);
	let expectedContent = language == null && languagePart.length > 0 ? `${languagePart}\n` : '';
	let currentLineStart = openingLineEnd + 1;
	let closingStart = -1;
	let closingLength = fenceLength;
	while (currentLineStart <= ctx.source.length) {
		const newline = ctx.source.indexOf('\n', currentLineStart);
		const currentLineEnd = newline < 0 ? ctx.source.length : newline;
		const currentLine = ctx.source.slice(currentLineStart, currentLineEnd);
		const closing = findCodeBlockClosing(currentLine, fence, fenceLength);
		if (closing != null) {
			const prefix = currentLine.slice(0, closing.fenceIndex);
			const contentLine =
				openingIndent.length > 0 && prefix.startsWith(openingIndent) ? prefix.slice(openingIndent.length) : prefix;
			if (contentLine.length > 0) {
				expectedContent += `${contentLine}\n`;
			}
			closingStart = currentLineStart + closing.fenceIndex;
			closingLength = closing.trailing.length > 0 ? closing.runLength : fenceLength;
			break;
		}
		const contentLine =
			openingIndent.length > 0 && currentLine.startsWith(openingIndent)
				? currentLine.slice(openingIndent.length)
				: currentLine;
		expectedContent += `${contentLine}\n`;
		if (newline < 0) {
			break;
		}
		currentLineStart = newline + 1;
	}
	if (closingStart < 0 || expectedContent !== node.content) {
		ctx.failed = true;
		return;
	}
	if (language == null && languagePart.length > 0) {
		pushMarker(ctx, fenceStart + fenceLength - ctx.pos, next);
		pushContent(ctx, closingStart, next);
	} else {
		pushMarker(ctx, openingLineEnd + 1 - ctx.pos, next);
		pushContent(ctx, closingStart, next);
	}
	pushMarker(ctx, closingLength, next);
}

function trimCodeFenceInfo(value: string): string {
	return value.replace(/^[ \t\r\n]+|[ \t\r\n]+$/g, '');
}

function findCodeBlockClosing(
	line: string,
	fence: string,
	fenceLength: number,
): {fenceIndex: number; runLength: number; trailing: string} | null {
	let trimmedStart = 0;
	while (line[trimmedStart] === ' ' || line[trimmedStart] === '\t' || line[trimmedStart] === '\r') {
		trimmedStart += 1;
	}
	const trimmed = line.slice(trimmedStart);
	const relativeFenceIndex = trimmed.indexOf(fence);
	if (relativeFenceIndex < 0) {
		return null;
	}
	let runEnd = relativeFenceIndex;
	while (trimmed[runEnd] === '`') {
		runEnd += 1;
	}
	const count = runEnd - relativeFenceIndex;
	const after = trimmed[runEnd];
	const trailing = trimmed.slice(runEnd);
	if (
		count < fenceLength ||
		(after != null && after !== ' ' && after !== '\t' && after !== '`' && !trailing.includes(fence))
	) {
		return null;
	}
	return {fenceIndex: trimmedStart + relativeFenceIndex, runLength: count, trailing};
}

function alignInlineCode(content: string, format: MarkdownHlFormat, ctx: AlignContext): void {
	const openingStart = ctx.pos;
	let ticks = 0;
	while (ctx.source[openingStart + ticks] === '`') {
		ticks += 1;
	}
	if (ticks === 0) {
		ctx.failed = true;
		return;
	}
	const contentStart = openingStart + ticks;
	const closingStart = findInlineCodeClosing(ctx.source, contentStart, ticks);
	if (closingStart < 0 || unescapeInlineCode(ctx.source.slice(contentStart, closingStart)) !== content) {
		ctx.failed = true;
		return;
	}
	const next = format | MarkdownHl.code;
	pushMarker(ctx, ticks, next);
	pushContent(ctx, closingStart, next);
	pushMarker(ctx, ticks, next);
}

function findInlineCodeClosing(source: string, start: number, ticks: number): number {
	let position = start;
	while (position < source.length) {
		const current = source[position];
		if (current === '\n' || current === '\r') {
			return -1;
		}
		if (ticks === 1 && current === '\\' && position + 1 < source.length) {
			position += 2;
			continue;
		}
		if (current !== '`') {
			position += 1;
			continue;
		}
		let runEnd = position;
		while (source[runEnd] === '`') {
			runEnd += 1;
		}
		const count = runEnd - position;
		if (ticks === 1) {
			if (count === 1) {
				return position;
			}
			position = runEnd;
			continue;
		}
		if (count >= ticks) {
			return position + count - ticks;
		}
		position = runEnd;
	}
	return -1;
}

function unescapeInlineCode(content: string): string {
	if (!content.includes('\\`')) {
		return content;
	}
	let result = '';
	let position = 0;
	while (position < content.length) {
		if (content[position] !== '\\') {
			result += content[position];
			position += 1;
			continue;
		}
		let runEnd = position;
		while (content[runEnd] === '\\') {
			runEnd += 1;
		}
		if (content[runEnd] === '`') {
			result += '\\'.repeat(Math.floor((runEnd - position) / 2));
			result += '`';
			position = runEnd + 1;
			continue;
		}
		result += content.slice(position, runEnd);
		position = runEnd;
	}
	return result;
}

function alignLinePrefixed(children: Array<Node>, format: MarkdownHlFormat, ctx: AlignContext, prefix: RegExp): void {
	const match = prefix.exec(ctx.source.slice(ctx.pos));
	if (match == null) {
		ctx.failed = true;
		return;
	}
	pushMarker(ctx, match[0].length, format);
	alignChildren(children, format, ctx);
}

function alignBlockquote(children: Array<Node>, format: MarkdownHlFormat, ctx: AlignContext): void {
	const start = ctx.pos;
	let trimmedStart = start;
	while (ctx.source[trimmedStart] === ' ' || ctx.source[trimmedStart] === '\t' || ctx.source[trimmedStart] === '\r') {
		trimmedStart += 1;
	}
	const markerRanges: Array<{start: number; end: number}> = [];
	const sourceOffsets: Array<number> = [];
	let virtualSource = '';
	let blockEnd = start;
	if (ctx.source.startsWith('>>> ', trimmedStart)) {
		const contentStart = trimmedStart + 4;
		markerRanges.push({start, end: contentStart});
		virtualSource = ctx.source.slice(contentStart);
		for (let position = contentStart; position < ctx.source.length; position += 1) {
			sourceOffsets.push(position);
		}
		blockEnd = ctx.source.length;
	} else {
		const lines: Array<{start: number; end: number; contentStart: number}> = [];
		let lineStart = start;
		while (lineStart <= ctx.source.length) {
			const newline = ctx.source.indexOf('\n', lineStart);
			const lineEnd = newline < 0 ? ctx.source.length : newline;
			let quoteStart = lineStart;
			while (ctx.source[quoteStart] === ' ' || ctx.source[quoteStart] === '\t' || ctx.source[quoteStart] === '\r') {
				quoteStart += 1;
			}
			if (!ctx.source.startsWith('> ', quoteStart)) {
				break;
			}
			const rest = ctx.source.slice(quoteStart + 2, lineEnd);
			const contentStart = /^[ \t\r]*$/.test(rest) ? lineEnd : quoteStart + 2;
			lines.push({start: lineStart, end: lineEnd, contentStart});
			if (newline < 0) {
				break;
			}
			lineStart = newline + 1;
		}
		if (lines.length === 0) {
			ctx.failed = true;
			return;
		}
		for (let index = 0; index < lines.length; index += 1) {
			const line = lines[index]!;
			markerRanges.push({start: line.start, end: line.contentStart});
			let repeatedPrefix = line.contentStart;
			while (ctx.source.startsWith('> ', repeatedPrefix) && repeatedPrefix < line.end) {
				markerRanges.push({start: repeatedPrefix, end: repeatedPrefix + 2});
				repeatedPrefix += 2;
			}
			for (let position = line.contentStart; position < line.end; position += 1) {
				virtualSource += ctx.source[position];
				sourceOffsets.push(position);
			}
			if (index + 1 < lines.length) {
				virtualSource += '\n';
				sourceOffsets.push(line.end);
			}
		}
		blockEnd = lines[lines.length - 1]!.end;
	}
	const virtualContext: AlignContext = {
		source: virtualSource,
		pos: 0,
		spans: [],
		failed: false,
		listIndentLevel: null,
	};
	if (children.length === 0) {
		pushContent(virtualContext, virtualSource.length, format);
	} else {
		while (virtualContext.pos < virtualSource.length) {
			const blank = /^[ \t\r]*\n/.exec(virtualSource.slice(virtualContext.pos));
			if (blank == null) {
				break;
			}
			pushContent(virtualContext, virtualContext.pos + blank[0].length, format);
		}
		alignNodeSequence(children, format, virtualContext);
		if (
			!virtualContext.failed &&
			virtualContext.pos < virtualSource.length &&
			/^[ \t\r\n]*$/.test(virtualSource.slice(virtualContext.pos))
		) {
			pushContent(virtualContext, virtualSource.length, format);
		}
	}
	if (virtualContext.failed || virtualContext.pos !== virtualSource.length) {
		ctx.failed = true;
		return;
	}
	const roles: Array<MarkdownSpan['role'] | null> = new Array(blockEnd - start).fill(null);
	const formats: Array<MarkdownHlFormat> = new Array(blockEnd - start).fill(MarkdownHl.none);
	for (const span of virtualContext.spans) {
		for (let position = span.start; position < span.end; position += 1) {
			const sourceOffset = sourceOffsets[position];
			if (sourceOffset == null) {
				ctx.failed = true;
				return;
			}
			roles[sourceOffset - start] = span.role;
			formats[sourceOffset - start] = span.format;
		}
	}
	for (const range of markerRanges) {
		for (let position = range.start; position < range.end; position += 1) {
			roles[position - start] = 'marker';
			formats[position - start] = format;
		}
	}
	if (roles.some((role) => role == null)) {
		ctx.failed = true;
		return;
	}
	let runStart = 0;
	for (let position = 1; position <= roles.length; position += 1) {
		if (position < roles.length && roles[position] === roles[runStart] && formats[position] === formats[runStart]) {
			continue;
		}
		ctx.spans.push({
			start: start + runStart,
			end: start + position,
			role: roles[runStart]!,
			format: formats[runStart]!,
		});
		runStart = position;
	}
	ctx.pos = blockEnd;
}

function alignAlert(format: MarkdownHlFormat, ctx: AlignContext): void {
	const bodyFormat = format | MarkdownHl.blockquote;
	let firstLine = true;
	while (true) {
		let quoteStart = ctx.pos;
		while (ctx.source[quoteStart] === ' ' || ctx.source[quoteStart] === '\t' || ctx.source[quoteStart] === '\r') {
			quoteStart += 1;
		}
		if (!ctx.source.startsWith('> ', quoteStart)) {
			ctx.failed = true;
			return;
		}
		const newline = ctx.source.indexOf('\n', ctx.pos);
		const lineEnd = newline < 0 ? ctx.source.length : newline;
		pushMarker(ctx, quoteStart + 2 - ctx.pos, bodyFormat);
		if (firstLine) {
			const close = ctx.source.indexOf(']', ctx.pos);
			if (!ctx.source.startsWith('[!', ctx.pos) || close < 0 || close >= lineEnd) {
				ctx.failed = true;
				return;
			}
			pushMarker(ctx, close + 1 - ctx.pos, bodyFormat);
			firstLine = false;
		}
		pushContent(ctx, lineEnd, bodyFormat);
		if (newline < 0) {
			return;
		}
		let nextQuote = newline + 1;
		while (ctx.source[nextQuote] === ' ' || ctx.source[nextQuote] === '\t' || ctx.source[nextQuote] === '\r') {
			nextQuote += 1;
		}
		if (!ctx.source.startsWith('> ', nextQuote)) {
			return;
		}
		pushContent(ctx, newline + 1, bodyFormat);
	}
}

function alignTable(node: Extract<Node, {type: 'Table'}>, format: MarkdownHlFormat, ctx: AlignContext): void {
	const lineCount = 2 + node.rows.length;
	for (let lineIndex = 0; lineIndex < lineCount; lineIndex += 1) {
		const newline = ctx.source.indexOf('\n', ctx.pos);
		const lineEnd = newline < 0 ? ctx.source.length : newline;
		if (lineIndex === 1) {
			pushMarker(ctx, lineEnd - ctx.pos, format);
		} else {
			alignTablePipes(ctx, lineEnd, format);
		}
		if (newline < 0) {
			return;
		}
		if (lineIndex + 1 < lineCount) {
			pushMarker(ctx, 1, format);
		}
	}
}

function alignTablePipes(ctx: AlignContext, lineEnd: number, format: MarkdownHlFormat): void {
	while (ctx.pos < lineEnd) {
		let scan = ctx.pos;
		while (scan < lineEnd) {
			if (ctx.source[scan] === '\\' && scan + 1 < lineEnd && ctx.source[scan + 1] === '|') {
				scan += 2;
				continue;
			}
			if (ctx.source[scan] === '|') {
				break;
			}
			scan += 1;
		}
		pushContent(ctx, scan, format);
		let pipeEnd = scan;
		while (pipeEnd < lineEnd && ctx.source[pipeEnd] === '|') {
			pipeEnd += 1;
		}
		pushMarker(ctx, pipeEnd - ctx.pos, format);
	}
}

function alignBlockSpoiler(children: Array<Node>, format: MarkdownHlFormat, ctx: AlignContext): void {
	const open = /^[ \t\r]*\|\|/.exec(ctx.source.slice(ctx.pos));
	if (open == null) {
		ctx.failed = true;
		return;
	}
	const afterOpen = ctx.pos + open[0].length;
	const closingStart = ctx.source.indexOf('||', afterOpen);
	if (closingStart < 0) {
		ctx.failed = true;
		return;
	}
	pushMarker(ctx, open[0].length, format);
	while (ctx.pos < closingStart) {
		const blank = /^[ \t\r]*\n/.exec(ctx.source.slice(ctx.pos, closingStart));
		if (blank == null) {
			break;
		}
		pushContent(ctx, ctx.pos + blank[0].length, format);
	}
	alignNodeSequence(children, format, ctx);
	if (ctx.failed) {
		return;
	}
	if (ctx.pos < closingStart && /^[ \t\r\n]*$/.test(ctx.source.slice(ctx.pos, closingStart))) {
		pushContent(ctx, closingStart, format);
	}
	if (ctx.pos !== closingStart) {
		ctx.failed = true;
		return;
	}
	expectMarker(ctx, '||', format);
}

function alignNodeSequence(nodes: Array<Node>, format: MarkdownHlFormat, ctx: AlignContext): void {
	for (let index = 0; index < nodes.length; index += 1) {
		if (index > 0) {
			advanceSequenceGap(nodes, index, format, ctx);
		}
		alignNode(nodes[index]!, format, ctx);
		if (ctx.failed) {
			return;
		}
	}
}

function advanceSequenceGap(nodes: Array<Node>, index: number, format: MarkdownHlFormat, ctx: AlignContext): void {
	let sourceLineBreaks = 0;
	while (ctx.source[ctx.pos + sourceLineBreaks] === '\n') {
		sourceLineBreaks += 1;
	}
	let contentLineBreaks = 0;
	for (let i = index; i < nodes.length; i += 1) {
		const next = nodes[i]!;
		if (next.type !== 'Text') {
			break;
		}
		let leading = 0;
		while (next.content[leading] === '\n') {
			leading += 1;
		}
		contentLineBreaks += leading;
		if (leading < next.content.length) {
			break;
		}
	}
	const structuralLineBreaks = Math.max(0, sourceLineBreaks - contentLineBreaks);
	pushContent(ctx, ctx.pos + structuralLineBreaks, format);
}

function advancePastText(
	source: string,
	start: number,
	content: string,
	listIndentLevel: number | null,
): number | null {
	let pos = start;
	let ci = 0;
	while (ci < content.length && pos < source.length) {
		if (source[pos] === '\\' && pos + 1 < source.length && source[pos + 1] === content[ci]) {
			pos += 2;
			ci += 1;
			continue;
		}
		if (source[pos] === content[ci]) {
			pos += 1;
			ci += 1;
			continue;
		}
		if (/\s/.test(source[pos]!) && /\s/.test(content[ci]!)) {
			pos += 1;
			ci += 1;
			continue;
		}
		if (listIndentLevel != null) {
			const continuationEnd = listContinuationEnd(source, pos, listIndentLevel);
			if (continuationEnd > pos) {
				pos = continuationEnd;
				continue;
			}
		}
		break;
	}
	return ci === content.length ? pos : null;
}

function coalesce(spans: Array<MarkdownSpan>): Array<MarkdownSpan> {
	const out: Array<MarkdownSpan> = [];
	for (const span of spans) {
		if (span.end <= span.start) {
			continue;
		}
		const last = out[out.length - 1];
		if (last != null && last.role === span.role && last.format === span.format && last.end === span.start) {
			last.end = span.end;
		} else {
			out.push({...span});
		}
	}
	return out;
}
