// SPDX-License-Identifier: AGPL-3.0-or-later

import {type CodeBlockBody, scanCodeBlocks} from '@app/features/lexical/composer/codeBlockIndent';
import {
	DEFAULT_COMPOSER_MARKDOWN_FLAGS,
	MarkdownHl,
	type MarkdownSpan,
} from '@app/features/lexical/composer/markdownSpans';
import {ParserFlags} from '@app/features/messaging/utils/markdown/parser/Enums';
import {parseMarkdownAstWithWasm} from '@app/features/messaging/utils/markdown/parser/MarkdownParserWasm';
import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';

export interface BlockquoteLine {
	start: number;
	contentStart: number;
	end: number;
}

export interface MultilineBlockquoteMarker {
	line: number;
	indent: number;
	splits: boolean;
}

export interface BlockquoteEdit {
	start: number;
	end: number;
	text: string;
	caret: number;
}

export type BlockquoteDeleteUnit = 'character' | 'word' | 'line';

const CONTINUATION = '\n> ';
const GRAPHEME_RE =
	/(?:\p{Regional_Indicator}{2}|[\u1100-\u115F\uA960-\uA97C]*(?:[\u1160-\u11A7\uD7B0-\uD7C6]+|[\uAC00-\uD7A3][\u1160-\u11A7\uD7B0-\uD7C6]*)[\u11A8-\u11FF\uD7CB-\uD7FB]*|[\u1100-\u115F\uA960-\uA97C]+|[\u11A8-\u11FF\uD7CB-\uD7FB]+|[\s\S])(?:[\p{M}\p{Grapheme_Extend}\p{Emoji_Modifier}\u200D]|(?<=\p{Extended_Pictographic}[\p{Grapheme_Extend}\p{Emoji_Modifier}]*\u200D)\p{Extended_Pictographic}|(?<=[\u0900-\u0D7F][\p{M}\u200D]*[\u094D\u09CD\u0ACD\u0B4D\u0C4D\u0D4D][\p{M}\u200D]*)[\u0900-\u0D7F])*/uy;
const WORD_RE =
	/\s*(?:\p{Script=Han}+|\p{Script=Hiragana}+|\p{Script=Katakana}+|\p{Extended_Pictographic}(?:[\p{M}\p{Grapheme_Extend}\p{Emoji_Modifier}\u200D]|(?<=\u200D)\p{Extended_Pictographic})*|[\p{L}\p{N}\p{M}\p{Pc}\u200C\u200D]+(?:(?:['\u2019]|(?<=\p{N})[.,](?=\p{N}))[\p{L}\p{N}\p{M}\p{Pc}\u200C\u200D]+)*|(?:[^\s\p{L}\p{N}\p{M}\p{Pc}]\p{M}*)+)/uy;
const WHOLE_GRAPHEME_RE = /[\u{10000}-\u{10FFFF}]|\p{Emoji}/u;
const QUOTE_PREFIX_RE = /^[ \t]*> /;
const DROPPABLE_TRAILING_LINE_RE = /^[ \t]*(?:> [ \t]*)?$/;
const MULTILINE_ALERT_RE = /^[ \t]*>>> \[!(?:NOTE|TIP|IMPORTANT|WARNING|CAUTION)\]/i;
const QUOTE_LINE_START_RE = /^[ \t\r]*> /;
const QUOTE_BLOCK_START_RE = /^[ \t\r]*>(?:>>)? /;
const BLANK_LINE_RE = /^[ \t\r]*$/;

function scanAllowedCodeBlocks(text: string, parserFlags: number): Array<CodeBlockBody> {
	return (parserFlags & ParserFlags.ALLOW_CODE_BLOCKS) === 0 ? [] : scanCodeBlocks(text);
}

function isInsideCodeBlock(blocks: ReadonlyArray<CodeBlockBody>, offset: number): boolean {
	return blocks.some((block) => block.start <= offset && offset <= block.end);
}

function isEmptyLine(line: BlockquoteLine): boolean {
	return line.contentStart === line.end;
}

function isDirectlyAbove(upper: BlockquoteLine, lower: BlockquoteLine): boolean {
	return upper.end + 1 === lower.start;
}

function isMultilineMarker(line: string, markerEnd: number): boolean {
	return line.slice(0, markerEnd).trimStart().startsWith('>>> ');
}

export function findBlockquoteMarkers(
	source: string,
	spans: ReadonlyArray<MarkdownSpan>,
	parserFlags = DEFAULT_COMPOSER_MARKDOWN_FLAGS,
	maxWireLength?: number,
): {markerEnds: Array<number>; multiline: MultilineBlockquoteMarker | null} {
	const lines = source.split('\n');
	const ends = scanMarkerEnds(source, lines, spans, parserFlags);
	const markerEnds = ends.map((end, index) => (isMultilineMarker(lines[index]!, end) ? 0 : end));
	const line = ends.findIndex((end, index) => isMultilineMarker(lines[index]!, end));
	if (line < 0) {
		return {markerEnds, multiline: null};
	}
	const splits =
		(line === 0 || !QUOTE_LINE_START_RE.test(lines[line - 1]!)) &&
		![lines[line]!.slice(ends[line]), ...lines.slice(line + 1)].some((text) => QUOTE_BLOCK_START_RE.test(text)) &&
		!BLANK_LINE_RE.test(lines[lines.length - 1]!) &&
		(maxWireLength == null || source.length - 2 + 2 * (lines.length - line - 1) <= maxWireLength) &&
		parseMarkdownAstWithWasm(source, parserFlags & DEFAULT_COMPOSER_MARKDOWN_FLAGS).nodes.at(-1)?.type === 'Blockquote';
	return {markerEnds, multiline: {line, indent: ends[line]! - 4, splits}};
}

function scanMarkerEnds(
	source: string,
	lines: ReadonlyArray<string>,
	spans: ReadonlyArray<MarkdownSpan>,
	parserFlags: number,
): Array<number> {
	const markerEndByStart = new Map<number, number>();
	for (const span of spans) {
		if (span.role === 'marker' && (span.format & MarkdownHl.blockquoteMarker) !== 0) {
			markerEndByStart.set(span.start, span.end);
		}
	}
	const blocks = scanAllowedCodeBlocks(source, parserFlags);
	const ends: Array<number> = [];
	let lineStart = 0;
	for (const line of lines) {
		const markerEnd = markerEndByStart.get(lineStart);
		const suppressed = isInsideCodeBlock(blocks, lineStart) || MULTILINE_ALERT_RE.test(line);
		ends.push(markerEnd == null || suppressed ? 0 : Math.min(markerEnd, lineStart + line.length) - lineStart);
		lineStart += line.length + 1;
	}
	return ends;
}

export function planBlockquoteLineBreak(
	text: string,
	lines: ReadonlyArray<BlockquoteLine>,
	caret: number,
	parserFlags = DEFAULT_COMPOSER_MARKDOWN_FLAGS,
): BlockquoteEdit | null {
	if (isInsideCodeBlock(scanAllowedCodeBlocks(text, parserFlags), caret)) {
		return null;
	}
	const index = lines.findIndex((line) => line.contentStart <= caret && caret <= line.end);
	if (index < 0) {
		return null;
	}
	const line = lines[index]!;
	const above = index > 0 ? lines[index - 1]! : null;
	if (!isEmptyLine(line) || above == null || !isDirectlyAbove(above, line)) {
		return {start: caret, end: caret, text: CONTINUATION, caret: caret + CONTINUATION.length};
	}
	let top = index;
	while (top > 0 && isDirectlyAbove(lines[top - 1]!, lines[top]!) && isEmptyLine(lines[top - 1]!)) {
		top -= 1;
	}
	return {start: lines[top]!.start, end: line.contentStart, text: '', caret: lines[top]!.start};
}

export function planBlockquoteBackspace(
	text: string,
	lines: ReadonlyArray<BlockquoteLine>,
	caret: number,
	parserFlags = DEFAULT_COMPOSER_MARKDOWN_FLAGS,
): BlockquoteEdit | null {
	if (isInsideCodeBlock(scanAllowedCodeBlocks(text, parserFlags), caret)) {
		return null;
	}
	const line = lines.find((candidate) => candidate.contentStart === caret);
	if (line != null) {
		return {start: line.start, end: line.contentStart, text: '', caret: line.start};
	}
	const above = lines.find((candidate) => candidate.end + 1 === caret && isEmptyLine(candidate));
	if (above == null || lines.some((candidate) => candidate.start === caret)) {
		return null;
	}
	return {start: above.start, end: caret, text: '', caret: above.start};
}

export function planBlockquoteForwardDelete(
	text: string,
	lines: ReadonlyArray<BlockquoteLine>,
	caret: number,
	parserFlags = DEFAULT_COMPOSER_MARKDOWN_FLAGS,
): BlockquoteEdit | null {
	if (text[caret] !== '\n' || isInsideCodeBlock(scanAllowedCodeBlocks(text, parserFlags), caret)) {
		return null;
	}
	const below = lines.find((candidate) => candidate.start === caret + 1);
	if (below != null && caret > 0 && text[caret - 1] !== '\n') {
		return {start: caret, end: below.contentStart, text: '', caret};
	}
	const line = lines.find((candidate) => candidate.end === caret && isEmptyLine(candidate));
	return line == null ? null : {start: line.start, end: caret + 1, text: '', caret: line.start};
}

function matchEnd(pattern: RegExp, text: string, offset: number): number {
	pattern.lastIndex = offset;
	return pattern.test(text) ? pattern.lastIndex : offset;
}

export function planBlockquoteContentStartDelete(
	text: string,
	lines: ReadonlyArray<BlockquoteLine>,
	atoms: ReadonlyArray<{start: number; end: number}>,
	caret: number,
	unit: BlockquoteDeleteUnit,
): BlockquoteEdit | null {
	const line = lines.find((candidate) => candidate.contentStart === caret && caret < candidate.end);
	if (line == null) {
		return null;
	}
	if (unit === 'line') {
		return {start: caret, end: line.end, text: '', caret};
	}
	const atom = atoms.find((candidate) => candidate.start === caret);
	if (atom != null) {
		return {start: caret, end: atom.end, text: '', caret};
	}
	const content = text.slice(0, line.end);
	if (unit === 'word') {
		const end = matchEnd(WORD_RE, content, caret);
		return {start: caret, end: end === caret ? line.end : end, text: '', caret};
	}
	const end = matchEnd(GRAPHEME_RE, content, caret);
	if (end - caret > 1 && !WHOLE_GRAPHEME_RE.test(content.slice(caret, end))) {
		return {start: end - 1, end, text: '', caret: end - 1};
	}
	return {start: caret, end, text: '', caret};
}

interface PastedText {
	display: string;
	segments: Array<MentionSegment>;
}

export function isInsideBlockquoteContent(lines: ReadonlyArray<BlockquoteLine>, offset: number): boolean {
	return lines.some((line) => line.contentStart <= offset && offset <= line.end);
}

export function planBlockquotePaste(
	text: string,
	lines: ReadonlyArray<BlockquoteLine>,
	start: number,
	end: number,
	pasted: PastedText,
	parserFlags = DEFAULT_COMPOSER_MARKDOWN_FLAGS,
): PastedText {
	let scan = pasted.display;
	for (const segment of pasted.segments) {
		scan = `${scan.slice(0, segment.start)}${'x'.repeat(segment.end - segment.start)}${scan.slice(segment.end)}`;
	}
	const starts: Array<number> = [];
	const ends: Array<number> = [];
	const sizes: Array<number> = [];
	let lineStart = 0;
	for (const line of pasted.display.split('\n')) {
		const lineEnd = lineStart + line.length;
		starts.push(lineStart);
		ends.push(lineEnd);
		sizes.push(lineEnd < pasted.display.length && line.endsWith('\r') ? line.length - 1 : line.length);
		lineStart = lineEnd + 1;
	}
	const sliceOf = (source: string, index: number) => source.slice(starts[index]!, starts[index]! + sizes[index]!);
	const firstPrefix = lines.some((line) => line.contentStart === start) ? QUOTE_PREFIX_RE.exec(sliceOf(scan, 0)) : null;
	if (firstPrefix != null) {
		starts[0] = firstPrefix[0].length;
		sizes[0]! -= firstPrefix[0].length;
	}
	const quoted = isInsideBlockquoteContent(lines, start);
	const prefixed = starts.map((_, index) => quoted && index > 0 && !QUOTE_PREFIX_RE.test(sliceOf(scan, index)));
	if (quoted) {
		const breaks: Array<number> = [];
		let merged = text.slice(0, start);
		for (let index = 0; index < starts.length; index += 1) {
			if (index > 0) {
				breaks.push(merged.length);
				merged += prefixed[index] ? CONTINUATION : '\n';
			}
			merged += sliceOf(scan, index);
		}
		const blocks = scanAllowedCodeBlocks(merged + text.slice(end), parserFlags);
		const stop = breaks.findIndex((offset) => isInsideCodeBlock(blocks, offset));
		if (stop >= 0) {
			prefixed.fill(false, stop + 1);
		}
	}
	let display = '';
	const segments: Array<MentionSegment> = [];
	let next = 0;
	for (let index = 0; index < starts.length; index += 1) {
		if (index > 0) {
			display += prefixed[index] ? CONTINUATION : '\n';
		}
		const shift = display.length - starts[index]!;
		while (next < pasted.segments.length && pasted.segments[next]!.start < ends[index]!) {
			const segment = pasted.segments[next]!;
			segments.push({...segment, start: segment.start + shift, end: segment.end + shift});
			next += 1;
		}
		display += sliceOf(pasted.display, index);
	}
	return {display, segments};
}

export function planBlockquoteDropCaret(start: number, end: number, dropped: number): number | null {
	if (dropped > start && dropped < end) {
		return null;
	}
	return dropped <= start ? dropped : dropped - (end - start);
}

function resolveBlockquoteCaret(lines: ReadonlyArray<BlockquoteLine>, caret: number): number {
	const line = lines.find((candidate) => candidate.start <= caret && caret < candidate.contentStart);
	return line == null ? caret : line.contentStart;
}

export function resolveBlockquoteSelection(
	lines: ReadonlyArray<BlockquoteLine>,
	anchor: number,
	focus: number,
): {anchor: number; focus: number} {
	const rangeStart = anchor === focus ? null : Math.min(anchor, focus);
	const resolve = (offset: number): number =>
		offset === rangeStart && lines.some((candidate) => candidate.start === offset)
			? offset
			: resolveBlockquoteCaret(lines, offset);
	return {anchor: resolve(anchor), focus: resolve(focus)};
}

export function blockquoteContentRange(
	lines: ReadonlyArray<BlockquoteLine>,
	start: number,
	end: number,
): {start: number; end: number} {
	return {
		start: resolveBlockquoteCaret(lines, Math.min(start, end)),
		end: resolveBlockquoteCaret(lines, Math.max(start, end)),
	};
}

export function planBlockquoteMove(
	lines: ReadonlyArray<BlockquoteLine>,
	offset: number,
	isBackward: boolean,
): number | null {
	if (!isBackward) {
		const below = lines.find((candidate) => candidate.start === offset + 1);
		return below == null ? null : below.contentStart;
	}
	const line = lines.find((candidate) => candidate.contentStart === offset);
	if (line == null) {
		return null;
	}
	return line.start > 0 ? line.start - 1 : line.contentStart;
}

export function dropTrailingEmptyBlockquoteLines(
	content: string,
	parserFlags = DEFAULT_COMPOSER_MARKDOWN_FLAGS,
): string {
	const blocks = scanAllowedCodeBlocks(content, parserFlags);
	let end = content.length;
	while (end > 0) {
		const lineStart = content.lastIndexOf('\n', end - 1) + 1;
		if (!DROPPABLE_TRAILING_LINE_RE.test(content.slice(lineStart, end)) || isInsideCodeBlock(blocks, lineStart)) {
			break;
		}
		end = Math.max(0, lineStart - 1);
	}
	return content.slice(0, end);
}
