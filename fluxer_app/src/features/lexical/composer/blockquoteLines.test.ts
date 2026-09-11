// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type BlockquoteEdit,
	type BlockquoteLine,
	dropTrailingEmptyBlockquoteLines,
	findBlockquoteMarkerEnds,
	planBlockquoteArrowLeft,
	planBlockquoteBackspace,
	planBlockquoteLineBreak,
	resolveBlockquoteCaret,
} from '@app/features/lexical/composer/blockquoteLines';
import {computeMarkdownHighlightSpans} from '@app/features/lexical/composer/markdownSpans';
import {describe, expect, it} from 'vitest';

const QUOTE_PREFIX_RE = /^[ \t]*> /;

function quoteLines(text: string): Array<BlockquoteLine> {
	const lines: Array<BlockquoteLine> = [];
	let start = 0;
	for (const line of text.split('\n')) {
		const prefix = QUOTE_PREFIX_RE.exec(line);
		if (prefix != null) {
			lines.push({start, contentStart: start + prefix[0].length, end: start + line.length});
		}
		start += line.length + 1;
	}
	return lines;
}

function applyEdit(text: string, edit: BlockquoteEdit | null): string | null {
	return edit == null ? null : `${text.slice(0, edit.start)}${edit.text}${text.slice(edit.end)}`;
}

describe('planBlockquoteLineBreak', () => {
	const ROWS: Array<[string, number, string, number]> = [
		['> test', 6, '> test\n> ', 9],
		['> test', 4, '> te\n> st', 7],
		['> test', 2, '> \n> test', 5],
		['> ', 2, '> \n> ', 5],
		['x\n> ', 4, 'x\n> \n> ', 7],
		['> test\n> ', 9, '> test\n', 7],
		['> \n> ', 5, '', 0],
		['> \n> \n> ', 8, '', 0],
		['> a\n> \n> ', 9, '> a\n', 4],
		['> a\n> \n> b', 6, '> a\n\n> b', 4],
		['x\n> \n> ', 7, 'x\n', 2],
		['>  ', 3, '>  \n> ', 6],
		['  > a', 5, '  > a\n> ', 8],
	];

	it.each(ROWS)('on %j at %i gives %j with the caret at %i', (text, caret, expected, expectedCaret) => {
		const edit = planBlockquoteLineBreak(text, quoteLines(text), caret);
		expect(applyEdit(text, edit)).toBe(expected);
		expect(edit?.caret).toBe(expectedCaret);
	});

	it.each<[string, number]>([
		['abc', 3],
		['', 0],
		['> test', 1],
		['x\n> a', 1],
	])('declines on %j at %i because the caret is not on a quote line', (text, caret) => {
		expect(planBlockquoteLineBreak(text, quoteLines(text), caret)).toBeNull();
	});

	it.each<[string, number]>([
		['```\n> a', 7],
		['> ```', 5],
		['```\n> a\n```', 6],
	])('declines on %j at %i because the caret is inside a code block', (text, caret) => {
		expect(planBlockquoteLineBreak(text, quoteLines(text), caret)).toBeNull();
	});
});

describe('planBlockquoteBackspace', () => {
	const ROWS: Array<[string, number, string, number]> = [
		['> test', 2, 'test', 0],
		['> ', 2, '', 0],
		['> \n> ', 5, '> \n', 3],
		['> \n', 3, '', 0],
		['> \nabc', 3, 'abc', 0],
		['a\n> \nb', 5, 'a\nb', 2],
		['  > a', 4, 'a', 0],
		['x\n> test', 4, 'x\ntest', 2],
	];

	it.each(ROWS)('on %j at %i gives %j with the caret at %i', (text, caret, expected, expectedCaret) => {
		const edit = planBlockquoteBackspace(text, quoteLines(text), caret);
		expect(applyEdit(text, edit)).toBe(expected);
		expect(edit?.caret).toBe(expectedCaret);
	});

	it.each<[string, number]>([
		['> a\nb', 4],
		['> test', 4],
		['>  ', 3],
		['abc', 3],
		['> \n> ', 3],
		['```\n> a', 6],
	])('declines on %j at %i', (text, caret) => {
		expect(planBlockquoteBackspace(text, quoteLines(text), caret)).toBeNull();
	});
});

describe('resolveBlockquoteCaret', () => {
	it.each<[string, number, number]>([
		['> test', 0, 2],
		['> test', 1, 2],
		['> test', 2, 2],
		['> test', 5, 5],
		['  > a', 3, 4],
		['ab\n> cd', 3, 5],
		['ab\n> cd', 4, 5],
		['ab\n> cd', 1, 1],
	])('maps %j at %i to %i', (text, caret, expected) => {
		expect(resolveBlockquoteCaret(quoteLines(text), caret)).toBe(expected);
	});
});

describe('planBlockquoteArrowLeft', () => {
	it('moves to the end of the line above from a later quote line', () => {
		expect(planBlockquoteArrowLeft(quoteLines('ab\n> cd'), 5)).toBe(2);
	});

	it('keeps the caret at content start on the first line', () => {
		expect(planBlockquoteArrowLeft(quoteLines('> test'), 2)).toBe(2);
	});

	it.each<[string, number]>([
		['> test', 4],
		['> test', 0],
		['abc', 2],
	])('declines on %j at %i', (text, caret) => {
		expect(planBlockquoteArrowLeft(quoteLines(text), caret)).toBeNull();
	});
});

describe('findBlockquoteMarkerEnds', () => {
	it.each<[string, Array<number>]>([
		['> a', [2]],
		['  > a', [4]],
		['>  ', [2]],
		['>>> a\nb', [4, 0]],
		['>>> [!NOTE]\nbody', [0, 0]],
		['  >>> [!tip] x\nbody', [0, 0]],
		['> > a', [2]],
		['> >>> a', [2]],
		['> [!NOTE]\n> body', [2, 2]],
		['> a\n> b', [2, 2]],
		['>a', [0]],
		['\\> a', [0]],
		['`> a`', [0]],
		['```\n> a', [0, 0]],
		['```\n> a\n```', [0, 0, 0]],
		['```\nx\n```\n> a', [0, 0, 0, 2]],
	])('gives %j the marker ends %j', (text, ends) => {
		expect(findBlockquoteMarkerEnds(text, computeMarkdownHighlightSpans(text))).toEqual(ends);
	});

	it('treats whitespace after the prefix as content rather than marker', () => {
		expect(computeMarkdownHighlightSpans('>  ').find((span) => span.start === 2)).toMatchObject({
			role: 'content',
			end: 3,
		});
	});
});

describe('dropTrailingEmptyBlockquoteLines', () => {
	it.each<[string, string]>([
		['> test\n> ', '> test'],
		['> ', ''],
		['> test\n> \n', '> test'],
		['> test\n', '> test'],
		['> \n> ', ''],
		['a\n  >  ', 'a'],
		['> a\n>  ', '> a'],
		['> a\n>', '> a\n>'],
		['```\n> ', '```\n> '],
		['> a\n> b', '> a\n> b'],
		['', ''],
	])('turns %j into %j', (content, expected) => {
		expect(dropTrailingEmptyBlockquoteLines(content)).toBe(expected);
	});
});
