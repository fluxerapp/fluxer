// SPDX-License-Identifier: AGPL-3.0-or-later

import {findCodeBlockClosing} from '@app/features/lexical/composer/markdownSpans';

const INDENT = '\t';
const SPACE_INDENT_WIDTH = 4;
const FENCE = '```';
const LEADING_SPACE_RE = /^[ \t\r]+/;
const TRAILING_SPACE_RE = /[ \t\r]+$/;
export const FENCE_LANGUAGE_RE = /^[\w+.#/-]+[ \t\r]*$/;
const LIST_ITEM_RE = /^( *)(?:[-*]|\d+\.) /;
const INVISIBLE_RE =
	/[\p{White_Space}\0\u00ad\u061c\u115f\u1160\u180e\u200b-\u200f\u202a-\u202e\u2060-\u2069\u2800\u3164\ufeff\uffa0]|\u034f|\u17b4|\u17b5|[\ufe00-\ufe0f]|[\u{e0100}-\u{e01ef}]/gu;

export interface CodeIndentEdit {
	start: number;
	end: number;
	text: string;
}

export interface CodeIndentPlan {
	edits: Array<CodeIndentEdit>;
	selectionStart: number;
	selectionEnd: number;
}

export interface CodeBlockBody {
	start: number;
	end: number;
	rendered: boolean;
}

interface SourceLine {
	text: string;
	offset: number;
}

interface LinePoint {
	index: number;
	column: number;
}

interface ListItem {
	level: number;
	content: string;
}

interface CodeBlockParse {
	body: CodeBlockBody | null;
	resume: LinePoint | null;
}

interface ScanScope {
	bodies: Array<CodeBlockBody>;
	end: number;
	quotes: boolean;
	multilineQuotes: boolean;
	rendered: boolean;
}

interface PhysicalLine {
	start: number;
	end: number;
	text: string;
	isCodeContent: boolean;
}

function trimStart(text: string): string {
	return text.replace(LEADING_SPACE_RE, '');
}

function trimEnd(text: string): string {
	return text.replace(TRAILING_SPACE_RE, '');
}

export function hasVisibleContent(text: string): boolean {
	return text.replace(INVISIBLE_RE, '') !== '';
}

function hasOpenInlineCode(text: string): boolean {
	let open = 0;
	for (const [run] of text.matchAll(/`+/g)) {
		if (open === 0) {
			open = run.length;
		} else if (open === run.length) {
			open = 0;
		}
	}
	return open > 0;
}

function countLeadingSpaces(text: string): number {
	let spaces = 0;
	while (text[spaces] === ' ') {
		spaces += 1;
	}
	return spaces;
}

function matchListItem(text: string): ListItem | null {
	const match = LIST_ITEM_RE.exec(text);
	if (match == null || match[1]!.length === 1) {
		return null;
	}
	return {level: match[1]!.length >> 1, content: text.slice(match[0]!.length)};
}

function isBulletPointText(text: string): boolean {
	return matchListItem(text) == null && trimStart(text).startsWith('- ') && !text.startsWith('  ');
}

function startsBlockquote(trimmed: string, scope: ScanScope): boolean {
	return (scope.multilineQuotes && trimmed.startsWith('>>> ')) || (scope.quotes && trimmed.startsWith('> '));
}

function findOpeningFence(text: string): number {
	for (let fence = text.indexOf(FENCE); fence >= 0; fence = text.indexOf(FENCE, fence + 1)) {
		let backslashes = 0;
		while (text[fence - backslashes - 1] === '\\') {
			backslashes += 1;
		}
		if (backslashes % 2 === 0) {
			return hasOpenInlineCode(text.slice(0, fence)) ? -1 : fence;
		}
	}
	return -1;
}

function splitLines(text: string): Array<SourceLine> {
	const lines: Array<SourceLine> = [];
	let offset = 0;
	for (const line of text.split('\n')) {
		lines.push({text: line, offset});
		offset += line.length + 1;
	}
	return lines;
}

function linesEnd(lines: Array<SourceLine>): number {
	const last = lines[lines.length - 1]!;
	return last.offset + last.text.length;
}

function trimLineWindow(lines: Array<SourceLine>): Array<SourceLine> {
	let start = 0;
	let end = lines.length;
	while (start < end && trimStart(lines[start]!.text) === '') {
		start += 1;
	}
	while (end > start && trimStart(lines[end - 1]!.text) === '') {
		end -= 1;
	}
	const window = lines.slice(start, end);
	if (window.length === 0) {
		return window;
	}
	const first = window[0]!;
	const trimmed = trimStart(first.text);
	window[0] = {text: trimmed, offset: first.offset + first.text.length - trimmed.length};
	const last = window[window.length - 1]!;
	window[window.length - 1] = {text: trimEnd(last.text), offset: last.offset};
	return window;
}

function opensUnclosedBlock(line: SourceLine, column: number, info: string): boolean {
	return (
		column === line.text.length - trimStart(line.text).length || FENCE_LANGUAGE_RE.test(info) || trimStart(info) === ''
	);
}

function resumeAt(lines: Array<SourceLine>, {index, column}: LinePoint): number {
	const line = lines[index]!;
	if (column >= line.text.length) {
		return index + 1;
	}
	lines[index] = {text: line.text.slice(column), offset: line.offset + column};
	return index;
}

function resumeIndex(lines: Array<SourceLine>, {index, column}: LinePoint): number {
	return column >= lines[index]!.text.length ? index + 1 : index;
}

function parseCodeBlock(lines: Array<SourceLine>, index: number, column: number, scope: ScanScope): CodeBlockParse {
	const line = lines[index]!;
	let infoStart = column;
	while (line.text[infoStart] === '`') {
		infoStart += 1;
	}
	const fence = line.text.slice(column, infoStart);
	const start = line.offset + infoStart;
	const info = line.text.slice(infoStart);
	const inlineEnd = info.indexOf(fence);
	if (inlineEnd >= 0) {
		const visible = hasVisibleContent(info.slice(0, inlineEnd));
		return {
			body: {start, end: start + inlineEnd, rendered: visible && scope.rendered},
			resume: visible ? {index, column: infoStart + inlineEnd + fence.length} : null,
		};
	}
	let content = FENCE_LANGUAGE_RE.test(info) ? '' : info;
	for (let next = index + 1; next < lines.length; next += 1) {
		const text = lines[next]!.text;
		const closing = findCodeBlockClosing(text, fence, fence.length);
		if (closing == null) {
			content += text;
			continue;
		}
		const visible = hasVisibleContent(content + text.slice(0, closing.fenceIndex));
		const trailingStart = closing.fenceIndex + (closing.trailing.length > 0 ? closing.runLength : fence.length);
		return {
			body: {start, end: lines[next]!.offset + closing.fenceIndex, rendered: visible && scope.rendered},
			resume: visible ? {index: next, column: trailingStart} : null,
		};
	}
	if (!opensUnclosedBlock(line, column, info)) {
		return {body: null, resume: null};
	}
	return {body: {start, end: scope.end, rendered: false}, resume: null};
}

function scanParagraph(lines: Array<SourceLine>, index: number, scope: ScanScope): number {
	let next = index + 1;
	while (next < lines.length) {
		const trimmed = trimStart(lines[next]!.text);
		if (
			trimmed === '' ||
			trimmed.startsWith(FENCE) ||
			matchListItem(trimmed) != null ||
			startsBlockquote(trimmed, scope)
		) {
			return next;
		}
		const fence = findOpeningFence(lines[next]!.text);
		if (fence >= 0) {
			const parsed = parseCodeBlock(lines, next, fence, scope);
			if (parsed.resume != null) {
				return next;
			}
			if (parsed.body != null) {
				scope.bodies.push(parsed.body);
			}
		}
		next += 1;
	}
	return next;
}

function scanQuote(lines: Array<SourceLine>, index: number, scope: ScanScope): number {
	const quoted: Array<SourceLine> = [];
	let next = index;
	while (next < lines.length) {
		const line = lines[next]!;
		const trimmed = trimStart(line.text);
		if (!trimmed.startsWith('> ')) {
			break;
		}
		quoted.push({text: trimmed.slice(2), offset: line.offset + line.text.length - trimmed.length + 2});
		next += 1;
	}
	scanBlocks(quoted, {...scope, quotes: false, end: linesEnd(quoted)});
	return next;
}

function scanSpoiler(lines: Array<SourceLine>, index: number, scope: ScanScope): number {
	const line = lines[index]!;
	const open = line.text.indexOf('||') + 2;
	const children: Array<SourceLine> = [{text: line.text.slice(open), offset: line.offset + open}];
	for (let next = index + 1; next < lines.length; next += 1) {
		const close = lines[next]!.text.indexOf('||');
		if (close < 0) {
			children.push(lines[next]!);
			continue;
		}
		children.push({text: lines[next]!.text.slice(0, close), offset: lines[next]!.offset});
		if (!children.some((child) => hasVisibleContent(child.text))) {
			return next + 1;
		}
		const window = trimLineWindow(children);
		scanBlocks(window, {...scope, end: linesEnd(window)});
		return resumeAt(lines, {index: next, column: close + 2});
	}
	scanBlocks(children, {...scope, rendered: false, end: linesEnd(children)});
	return lines.length;
}

function scanListFence(lines: Array<SourceLine>, index: number, scope: ScanScope): number {
	if (index >= lines.length) {
		return index;
	}
	const line = lines[index]!;
	const trimmed = trimStart(line.text);
	if (!trimmed.startsWith(FENCE)) {
		return index;
	}
	const parsed = parseCodeBlock(lines, index, line.text.length - trimmed.length, scope);
	if (parsed.body == null || parsed.resume == null) {
		return index;
	}
	scope.bodies.push(parsed.body);
	return resumeIndex(lines, parsed.resume);
}

function scanList(lines: Array<SourceLine>, index: number, scope: ScanScope): number {
	const level = matchListItem(lines[index]!.text)?.level ?? 0;
	let next = index;
	while (next < lines.length) {
		const text = lines[next]!.text;
		const trimmed = trimStart(text);
		if (trimmed.startsWith('#') || startsBlockquote(trimmed, scope)) {
			return next;
		}
		const item = matchListItem(text);
		if (item != null) {
			next = matchListItem(item.content) == null ? scanListFence(lines, next + 1, scope) : next + 1;
			continue;
		}
		if (!isBulletPointText(text) && countLeadingSpaces(text) <= level * 2) {
			return next;
		}
		next += 1;
	}
	return next;
}

function scanBlock(lines: Array<SourceLine>, index: number, scope: ScanScope): number {
	const line = lines[index]!;
	const trimmed = trimStart(line.text);
	if (trimmed.startsWith('>>> ')) {
		if (!scope.multilineQuotes) {
			return index + 1;
		}
		const quoted = {text: trimmed.slice(4), offset: line.offset + line.text.length - trimmed.length + 4};
		scanBlocks([quoted, ...lines.slice(index + 1)], {...scope, quotes: true, multilineQuotes: false});
		return lines.length;
	}
	if (trimmed.startsWith('> ')) {
		return scope.quotes ? scanQuote(lines, index, scope) : scanParagraph(lines, index, scope);
	}
	if (matchListItem(line.text) != null) {
		return scanList(lines, index, scope);
	}
	if (trimmed.startsWith('||') && !trimmed.includes('||', 2)) {
		return scanSpoiler(lines, index, scope);
	}
	const fence = findOpeningFence(line.text);
	if (fence >= 0) {
		const parsed = parseCodeBlock(lines, index, fence, scope);
		if (parsed.body != null) {
			scope.bodies.push(parsed.body);
		}
		if (parsed.resume != null) {
			return resumeAt(lines, parsed.resume);
		}
	}
	return scanParagraph(lines, index, scope);
}

function scanBlocks(lines: Array<SourceLine>, scope: ScanScope): void {
	let index = 0;
	while (index < lines.length) {
		index = trimStart(lines[index]!.text) === '' ? index + 1 : scanBlock(lines, index, scope);
	}
}

export function scanCodeBlocks(text: string): Array<CodeBlockBody> {
	const scope: ScanScope = {bodies: [], end: text.length, quotes: true, multilineQuotes: true, rendered: true};
	scanBlocks(splitLines(text), scope);
	return scope.bodies;
}

export function isOffsetInsideCodeBlock(text: string, offset: number): boolean {
	return scanCodeBlocks(text).some((body) => body.start <= offset && offset <= body.end);
}

function scanLines(text: string): Array<PhysicalLine> {
	const bodies = scanCodeBlocks(text);
	return splitLines(text).map(({text: content, offset: start}) => {
		const end = start + content.length;
		return {
			start,
			end,
			text: content,
			isCodeContent: bodies.some(
				(body) => start >= body.start && (end <= body.end || trimStart(text.slice(start, body.end)) !== ''),
			),
		};
	});
}

function lineAt(lines: Array<PhysicalLine>, offset: number): number {
	for (let i = 0; i < lines.length; i += 1) {
		if (offset >= lines[i]!.start && offset <= lines[i]!.end) {
			return i;
		}
	}
	return lines.length - 1;
}

export function analyzeCodeIndent(
	text: string,
	selectionStart: number,
	selectionEnd: number,
	unindent: boolean,
): CodeIndentPlan | null {
	const lines = scanLines(text);
	const startLine = lineAt(lines, selectionStart);
	let endLine = lineAt(lines, selectionEnd);
	if (selectionEnd > selectionStart && selectionEnd === lines[endLine]!.start && endLine > 0) {
		endLine -= 1;
	}
	for (let i = startLine; i <= endLine; i += 1) {
		if (!lines[i]!.isCodeContent) {
			return null;
		}
	}
	if (!unindent && selectionStart === selectionEnd) {
		return {
			edits: [{start: selectionStart, end: selectionStart, text: INDENT}],
			selectionStart: selectionStart + INDENT.length,
			selectionEnd: selectionStart + INDENT.length,
		};
	}
	const edits: Array<CodeIndentEdit> = [];
	let startDelta = 0;
	let endDelta = 0;
	for (let i = startLine; i <= endLine; i += 1) {
		const line = lines[i]!;
		if (unindent) {
			let removed = 0;
			if (line.text.startsWith(INDENT)) {
				removed = 1;
			} else {
				while (removed < SPACE_INDENT_WIDTH && line.text[removed] === ' ') {
					removed += 1;
				}
			}
			if (removed === 0) {
				continue;
			}
			edits.push({start: line.start, end: line.start + removed, text: ''});
			startDelta -= Math.min(removed, Math.max(0, selectionStart - line.start));
			endDelta -= Math.min(removed, Math.max(0, selectionEnd - line.start));
		} else {
			edits.push({start: line.start, end: line.start, text: INDENT});
			if (line.start <= selectionStart) {
				startDelta += INDENT.length;
			}
			if (line.start <= selectionEnd) {
				endDelta += INDENT.length;
			}
		}
	}
	if (edits.length === 0) {
		return null;
	}
	return {
		edits,
		selectionStart: Math.max(0, selectionStart + startDelta),
		selectionEnd: Math.max(0, selectionEnd + endDelta),
	};
}
