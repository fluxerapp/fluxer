// SPDX-License-Identifier: AGPL-3.0-or-later

import {isOffsetInsideCodeBlock, scanCodeBlocks} from '@app/features/lexical/composer/codeBlockIndent';
import {MarkdownHl, type MarkdownSpan} from '@app/features/lexical/composer/markdownSpans';

export interface BlockquoteLine {
	start: number;
	contentStart: number;
	end: number;
}

export interface BlockquoteEdit {
	start: number;
	end: number;
	text: string;
	caret: number;
}

const CONTINUATION = '\n> ';
const DROPPABLE_TRAILING_LINE_RE = /^[ \t]*(?:> [ \t]*)?$/;
const MULTILINE_ALERT_RE = /^[ \t]*>>> \[!(?:NOTE|TIP|IMPORTANT|WARNING|CAUTION)\]/i;

function isEmptyLine(line: BlockquoteLine): boolean {
	return line.contentStart === line.end;
}

function isDirectlyAbove(upper: BlockquoteLine, lower: BlockquoteLine): boolean {
	return upper.end + 1 === lower.start;
}

export function findBlockquoteMarkerEnds(source: string, spans: ReadonlyArray<MarkdownSpan>): Array<number> {
	const markerEndByStart = new Map<number, number>();
	for (const span of spans) {
		if (span.role === 'marker' && (span.format & MarkdownHl.blockquoteMarker) !== 0) {
			markerEndByStart.set(span.start, span.end);
		}
	}
	const blocks = scanCodeBlocks(source);
	const ends: Array<number> = [];
	let lineStart = 0;
	for (const line of source.split('\n')) {
		const markerEnd = markerEndByStart.get(lineStart);
		const insideCode = blocks.some((block) => block.start <= lineStart && lineStart <= block.end);
		const suppressed = insideCode || MULTILINE_ALERT_RE.test(line);
		ends.push(markerEnd == null || suppressed ? 0 : Math.min(markerEnd, lineStart + line.length) - lineStart);
		lineStart += line.length + 1;
	}
	return ends;
}

export function planBlockquoteLineBreak(
	text: string,
	lines: ReadonlyArray<BlockquoteLine>,
	caret: number,
): BlockquoteEdit | null {
	if (isOffsetInsideCodeBlock(text, caret)) {
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
): BlockquoteEdit | null {
	if (isOffsetInsideCodeBlock(text, caret)) {
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

export function resolveBlockquoteCaret(lines: ReadonlyArray<BlockquoteLine>, caret: number): number {
	const line = lines.find((candidate) => candidate.start <= caret && caret < candidate.contentStart);
	return line == null ? caret : line.contentStart;
}

export function planBlockquoteArrowLeft(lines: ReadonlyArray<BlockquoteLine>, caret: number): number | null {
	const line = lines.find((candidate) => candidate.contentStart === caret);
	if (line == null) {
		return null;
	}
	return line.start > 0 ? line.start - 1 : line.contentStart;
}

export function dropTrailingEmptyBlockquoteLines(content: string): string {
	const blocks = scanCodeBlocks(content);
	let end = content.length;
	while (end > 0) {
		const lineStart = content.lastIndexOf('\n', end - 1) + 1;
		if (
			!DROPPABLE_TRAILING_LINE_RE.test(content.slice(lineStart, end)) ||
			blocks.some((block) => block.start <= lineStart && lineStart <= block.end)
		) {
			break;
		}
		end = Math.max(0, lineStart - 1);
	}
	return content.slice(0, end);
}
