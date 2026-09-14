// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type BlockquoteDeleteUnit,
	type BlockquoteEdit,
	type BlockquoteLine,
	blockquoteContentRange,
	dropTrailingEmptyBlockquoteLines,
	findBlockquoteMarkers,
	planBlockquoteBackspace,
	planBlockquoteContentStartDelete,
	planBlockquoteDropCaret,
	planBlockquoteForwardDelete,
	planBlockquoteLineBreak,
	planBlockquoteMove,
	planBlockquotePaste,
	resolveBlockquoteSelection,
} from '@app/features/lexical/composer/blockquoteLines';
import {
	computeMarkdownHighlightSpans,
	DEFAULT_COMPOSER_MARKDOWN_FLAGS,
} from '@app/features/lexical/composer/markdownSpans';
import {ParserFlags} from '@app/features/messaging/utils/markdown/parser/Enums';
import {parseMarkdownAstWithWasm} from '@app/features/messaging/utils/markdown/parser/MarkdownParserWasm';
import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';
import {describe, expect, it} from 'vitest';

const QUOTE_PREFIX_RE = /^[ \t]*> /;
const NO_CODE_BLOCK_FLAGS = DEFAULT_COMPOSER_MARKDOWN_FLAGS & ~ParserFlags.ALLOW_CODE_BLOCKS;

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

	it.each<[string, number, string, number]>([
		['> ```', 5, '> ```\n> ', 8],
		['> ```\n> b', 9, '> ```\n> b\n> ', 12],
	])('continues %j at %i into %j with the caret at %i when code blocks are off', (text, caret, expected, caretAfter) => {
		expect(planBlockquoteLineBreak(text, quoteLines(text), caret)).toBeNull();
		const edit = planBlockquoteLineBreak(text, quoteLines(text), caret, NO_CODE_BLOCK_FLAGS);
		expect(applyEdit(text, edit)).toBe(expected);
		expect(edit?.caret).toBe(caretAfter);
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

	it('removes the marker below a quoted fence when code blocks are off', () => {
		const text = '> ```\n> b';
		expect(planBlockquoteBackspace(text, quoteLines(text), 8)).toBeNull();
		const edit = planBlockquoteBackspace(text, quoteLines(text), 8, NO_CODE_BLOCK_FLAGS);
		expect(applyEdit(text, edit)).toBe('> ```\nb');
		expect(edit?.caret).toBe(6);
	});
});

describe('planBlockquoteForwardDelete', () => {
	const ROWS: Array<[string, number, string, number]> = [
		['> a\n> b', 3, '> ab', 3],
		['a\n> b', 1, 'ab', 1],
		['> a\n> ', 3, '> a', 3],
		['  > a\n> b', 5, '  > ab', 5],
		['> \n> b', 2, '> b', 2],
		['a\n> \n> b', 4, 'a\n> b', 4],
		['> \nb', 2, 'b', 0],
		['> \n', 2, '', 0],
		['a\n> \nb', 4, 'a\nb', 2],
	];

	it.each(ROWS)('on %j at %i gives %j with the caret at %i', (text, caret, expected, expectedCaret) => {
		const edit = planBlockquoteForwardDelete(text, quoteLines(text), caret);
		expect(applyEdit(text, edit)).toBe(expected);
		expect(edit?.caret).toBe(expectedCaret);
	});

	it.each<[string, number]>([
		['\n> b', 0],
		['a\n\n> b', 2],
		['> a\nb', 3],
		['> test', 4],
		['> ', 2],
		['abc', 1],
		['```\n> a\n> b', 7],
	])('declines on %j at %i', (text, caret) => {
		expect(planBlockquoteForwardDelete(text, quoteLines(text), caret)).toBeNull();
	});

	it('joins the line below a quoted fence when code blocks are off', () => {
		const text = '> ```\n> b';
		expect(planBlockquoteForwardDelete(text, quoteLines(text), 5)).toBeNull();
		const edit = planBlockquoteForwardDelete(text, quoteLines(text), 5, NO_CODE_BLOCK_FLAGS);
		expect(applyEdit(text, edit)).toBe('> ```b');
		expect(edit?.caret).toBe(5);
	});
});

describe('planBlockquoteContentStartDelete', () => {
	function plan(
		text: string,
		caret: number,
		unit: BlockquoteDeleteUnit,
		atoms: Array<{start: number; end: number}> = [],
	) {
		return planBlockquoteContentStartDelete(text, quoteLines(text), atoms, caret, unit);
	}

	it.each<[string, number, string, number]>([
		['> ab', 2, '> b', 2],
		['x\n> ab', 4, 'x\n> b', 4],
		['  > ab', 4, '  > b', 4],
		['> e\u0301x', 2, '> ex', 3],
		['> a\u0301\u0302x', 2, '> a\u0301x', 4],
		['> क\u093Fx', 2, '> कx', 3],
		['> क\u094Dषx', 2, '> क\u094Dx', 4],
		['> \u1100\u1161x', 2, '> \u1100x', 3],
		['> 가각', 2, '> 각', 2],
		['> a\u200Db', 2, '> ab', 3],
		['> \u{1F468}\u200D\u{1F469}\u200D\u{1F467}x', 2, '> x', 2],
		['> \u{1F44B}\u200D\u{1F3FF}x', 2, '> x', 2],
		['> \u{1F44D}\u{1F3FD}x', 2, '> x', 2],
		['> \u{1F1F8}\u{1F1EA}\u{1F1F3}\u{1F1F4}', 2, '> \u{1F1F3}\u{1F1F4}', 2],
		['> #\uFE0F\u20E3x', 2, '> x', 2],
		['> ❤\uFE0Fx', 2, '> x', 2],
		['> 1\uFE0Fx', 2, '> x', 2],
		['> \u{1D4B3}x', 2, '> x', 2],
	])('deletes one character from the content start of %j at %i, giving %j with the caret at %i', (text, caret, expected, expectedCaret) => {
		const edit = plan(text, caret, 'character');
		expect(applyEdit(text, edit)).toBe(expected);
		expect(edit?.caret).toBe(expectedCaret);
	});

	it.each<[string, number, string]>([
		['> ab ef', 2, '>  ef'],
		['> ab', 2, '> '],
		['>   ab cd', 2, '>  cd'],
		['> \tab c', 2, '>  c'],
		['> foo.bar baz', 2, '> .bar baz'],
		["> don't stop", 2, '>  stop'],
		['> it’s x', 2, '>  x'],
		["> ab' x", 2, "> ' x"],
		['> a_b c', 2, '>  c'],
		['> a-b c', 2, '> -b c'],
		['> 3.14 x', 2, '>  x'],
		['> ...ab cd', 2, '> ab cd'],
		['> 日本語テキスト x', 2, '> テキスト x'],
		['> \u{1F600} ab', 2, '>  ab'],
		['> ab\u{1F600}cd x', 2, '> \u{1F600}cd x'],
		['> #\uFE0F\u20E3x', 2, '> x'],
		['> क\u094Dषx y', 2, '>  y'],
		['>    ', 2, '> '],
		['x\n> ab\n> cd', 4, 'x\n> \n> cd'],
	])('deletes one word from the content start of %j at %i, giving %j', (text, caret, expected) => {
		const edit = plan(text, caret, 'word');
		expect(applyEdit(text, edit)).toBe(expected);
		expect(edit?.caret).toBe(caret);
	});

	it.each<[string, number, string]>([
		['> ab ef', 2, '> '],
		['x\n> ab ef\n> gh', 4, 'x\n> \n> gh'],
	])('deletes to the line end from the content start of %j at %i, giving %j', (text, caret, expected) => {
		const edit = plan(text, caret, 'line');
		expect(applyEdit(text, edit)).toBe(expected);
		expect(edit?.caret).toBe(caret);
	});

	it.each<[BlockquoteDeleteUnit, string, Array<{start: number; end: number}>, string]>([
		[
			'character',
			'x\n> xxxxxxxxxx y',
			[
				{start: 4, end: 9},
				{start: 9, end: 14},
			],
			'x\n> xxxxx y',
		],
		[
			'word',
			'x\n> xxxxxxxxxx y',
			[
				{start: 4, end: 9},
				{start: 9, end: 14},
			],
			'x\n> xxxxx y',
		],
		[
			'line',
			'x\n> xxxxxxxxxx y',
			[
				{start: 4, end: 9},
				{start: 9, end: 14},
			],
			'x\n> ',
		],
		['word', 'x\n> abxxxxx y', [{start: 6, end: 11}], 'x\n>  y'],
	])('deletes by %s over whole atoms from the content start of %j', (unit, text, atoms, expected) => {
		expect(applyEdit(text, plan(text, 4, unit, atoms))).toBe(expected);
	});

	it.each<[string, number]>([
		['> ab', 3],
		['> ab', 1],
		['> ab', 0],
		['> ', 2],
		['ab\n> cd', 2],
		['abc', 0],
	])('declines on %j at %i', (text, caret) => {
		for (const unit of ['character', 'word', 'line'] as const) {
			expect(plan(text, caret, unit)).toBeNull();
		}
	});
});

describe('planBlockquotePaste', () => {
	function plan(text: string, start: number, end: number, display: string, parserFlags?: number): string {
		return planBlockquotePaste(text, quoteLines(text), start, end, {display, segments: []}, parserFlags).display;
	}

	const ROWS: Array<[string, number, number, string, string]> = [
		['> abcd', 4, 4, 'x\ny', 'x\n> y'],
		['> abcd', 2, 2, 'x\ny', 'x\n> y'],
		['> ', 2, 2, 'x\n\ny', 'x\n> \n> y'],
		['  > a', 5, 5, 'x\ny', 'x\n> y'],
		['> abcd', 6, 6, '\n', '\n> '],
		['> abcd', 6, 6, 'x\n', 'x\n> '],
		['> ab\n> cd', 3, 8, 'x\ny', 'x\n> y'],
		['> ab\ncd', 3, 6, 'x\ny', 'x\n> y'],
		['> ab', 4, 4, 'x\n>>> y', 'x\n> >>> y'],
		['> ab', 4, 4, 'x\n```\ncode\n```\ny', 'x\n> ```\ncode\n```\ny'],
		['```\nx\n```\n> a', 13, 13, 'x\ny', 'x\n> y'],
		['> abcd', 4, 4, 'x\r\ny', 'x\n> y'],
		['abcd', 2, 2, 'x\r\ny', 'x\ny'],
		['> ', 2, 2, '> x\n> y', 'x\n> y'],
		['> ', 2, 2, '> x', 'x'],
		['> ', 2, 2, '> > x', '> x'],
		['> ab', 2, 2, '\t> x', 'x'],
		['x\n> ', 4, 4, '> x\r\n> y', 'x\n> y'],
		['> ab\n> cd', 2, 8, '> x\ny', 'x\n> y'],
	];

	it.each(ROWS)('on %j at %i..%i pasting %j gives %j', (text, start, end, display, expected) => {
		expect(plan(text, start, end, display)).toBe(expected);
	});

	const UNCHANGED_ROWS: Array<[string, number, number, string]> = [
		['> ab', 4, 4, 'b\n> c'],
		['> ab', 4, 4, 'x\n  > y'],
		['abcd', 2, 2, 'x\ny'],
		['> abcd', 1, 1, 'x\ny'],
		['> ```', 5, 5, 'x\ny'],
		['```\n> a', 7, 7, 'x\ny'],
		['> ab', 4, 4, 'x\r'],
		['> ab', 4, 4, '> x'],
		['> ', 1, 1, '> x'],
		['abcd', 0, 0, '> x'],
	];

	it.each(UNCHANGED_ROWS)('on %j at %i..%i leaves %j as it is', (text, start, end, display) => {
		expect(plan(text, start, end, display)).toBe(display);
	});

	it.each<[string, number, string, string, string]>([
		['> ab', 4, 'x\n```\ncode\n```\ny', 'x\n> ```\ncode\n```\ny', 'x\n> ```\n> code\n> ```\n> y'],
		['> ```', 5, 'x\ny', 'x\ny', 'x\n> y'],
	])('on %j at %i pasting %j gives %j and %j when code blocks are off', (text, caret, display, expected, offValue) => {
		expect(plan(text, caret, caret, display)).toBe(expected);
		expect(plan(text, caret, caret, display, NO_CODE_BLOCK_FLAGS)).toBe(offValue);
	});

	it('shifts mention segments and hides mention text from the fence scan', () => {
		const segments: Array<MentionSegment> = [
			{type: 'user', id: '1', displayText: '@name', actualText: '<@1>', start: 3, end: 8},
			{type: 'user', id: '2', displayText: '@a```b', actualText: '<@2>', start: 12, end: 18},
		];
		expect(
			planBlockquotePaste('> abcd', quoteLines('> abcd'), 4, 4, {display: 'x\r\n@name y\n\n@a```b', segments}),
		).toEqual({
			display: 'x\n> @name y\n> \n> @a```b',
			segments: [
				{type: 'user', id: '1', displayText: '@name', actualText: '<@1>', start: 4, end: 9},
				{type: 'user', id: '2', displayText: '@a```b', actualText: '<@2>', start: 17, end: 23},
			],
		});
	});

	it('shifts a mention on a first line whose quote prefix is dropped', () => {
		const mention = (start: number): MentionSegment => ({
			type: 'user',
			id: '1',
			displayText: '@name',
			actualText: '<@1>',
			start,
			end: start + 5,
		});
		expect(
			planBlockquotePaste('> ', quoteLines('> '), 2, 2, {
				display: '> @name\n@name',
				segments: [mention(2), mention(8)],
			}),
		).toEqual({display: '@name\n> @name', segments: [mention(0), mention(8)]});
	});

	it('quotes every line of a 200 line paste', () => {
		const display = Array.from({length: 200}, (_, index) => `line ${index}`).join('\n');
		const lines = `> ${plan('> ', 2, 2, display)}`.split('\n');
		expect(lines).toHaveLength(200);
		expect(lines.filter((line) => QUOTE_PREFIX_RE.test(line))).toHaveLength(200);
	});
});

describe('planBlockquoteDropCaret', () => {
	const ROWS: Array<[number, number, number, number | null]> = [
		[0, 7, 10, 3],
		[0, 7, 0, 0],
		[0, 7, 7, 0],
		[4, 11, 2, 2],
		[0, 7, 4, null],
		[0, 4, 10, 6],
		[0, 5, 5, 0],
		[2, 7, 7, 2],
		[0, 8, 12, 4],
		[0, 10, 12, 2],
	];

	it.each(ROWS)('maps a drop of the range %i-%i at %i', (start, end, dropped, expected) => {
		expect(planBlockquoteDropCaret(start, end, dropped)).toBe(expected);
	});
});

describe('resolveBlockquoteSelection', () => {
	it.each<[string, number, number]>([
		['> test', 0, 2],
		['> test', 1, 2],
		['> test', 2, 2],
		['> test', 5, 5],
		['  > a', 3, 4],
		['ab\n> cd', 3, 5],
		['ab\n> cd', 4, 5],
		['ab\n> cd', 1, 1],
	])('maps the caret on %j at %i to %i', (text, caret, expected) => {
		expect(resolveBlockquoteSelection(quoteLines(text), caret, caret)).toEqual({anchor: expected, focus: expected});
	});

	it.each<[string, number, number, number, number]>([
		['ab\n> cd', 1, 3, 1, 5],
		['ab\n> cd', 3, 1, 5, 1],
		['ab\n> cd', 1, 4, 1, 5],
		['ab\n> cd', 4, 7, 5, 7],
		['ab\n> cd', 3, 7, 3, 7],
		['> a\n> b', 0, 7, 0, 7],
		['x\n> ab', 4, 1, 4, 1],
	])('maps the range on %j from %i..%i to %i..%i', (text, anchor, focus, expectedAnchor, expectedFocus) => {
		expect(resolveBlockquoteSelection(quoteLines(text), anchor, focus)).toEqual({
			anchor: expectedAnchor,
			focus: expectedFocus,
		});
	});
});

describe('blockquoteContentRange', () => {
	it.each<[string, number, number, number, number]>([
		['> hello', 0, 7, 2, 7],
		['> a\n> b', 2, 4, 2, 6],
		['> a\n> b', 4, 7, 6, 7],
		['ab\n> cd', 1, 4, 1, 5],
		['> test', 2, 2, 2, 2],
		['abc', 0, 3, 0, 3],
	])('clamps %j from %i..%i to %i..%i', (text, start, end, expectedStart, expectedEnd) => {
		expect(blockquoteContentRange(quoteLines(text), start, end)).toEqual({start: expectedStart, end: expectedEnd});
	});
});

describe('planBlockquoteMove', () => {
	it.each<[string, number, boolean, number]>([
		['ab\n> cd', 5, true, 2],
		['> test', 2, true, 2],
		['ab\n> cd', 2, false, 5],
		['> a\n> b', 3, false, 6],
		['\n> b', 0, false, 3],
	])('moves across the marker on %j from %i backward %s to %i', (text, offset, isBackward, expected) => {
		expect(planBlockquoteMove(quoteLines(text), offset, isBackward)).toBe(expected);
	});

	it.each<[string, number, boolean]>([
		['> test', 4, true],
		['> test', 0, true],
		['abc', 2, true],
		['> test', 2, false],
		['ab\n> cd', 1, false],
		['abc', 3, false],
	])('declines on %j at %i backward %s', (text, offset, isBackward) => {
		expect(planBlockquoteMove(quoteLines(text), offset, isBackward)).toBeNull();
	});
});

describe('findBlockquoteMarkers marker ends', () => {
	function markerEnds(text: string, parserFlags = DEFAULT_COMPOSER_MARKDOWN_FLAGS): Array<number> {
		return findBlockquoteMarkers(text, computeMarkdownHighlightSpans(text, parserFlags), parserFlags).markerEnds;
	}

	it.each<[string, Array<number>]>([
		['> a', [2]],
		['  > a', [4]],
		['>  ', [2]],
		['>>> a\nb', [0, 0]],
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
		expect(markerEnds(text)).toEqual(ends);
	});

	it('marks every line of a quoted fence when code blocks are off', () => {
		const text = '> ```\n> b\n> ```';
		expect(markerEnds(text)).toEqual([2, 0, 0]);
		expect(markerEnds(text, NO_CODE_BLOCK_FLAGS)).toEqual([2, 2, 2]);
	});

	it('treats whitespace after the prefix as content rather than marker', () => {
		expect(computeMarkdownHighlightSpans('>  ').find((span) => span.start === 2)).toMatchObject({
			role: 'content',
			end: 3,
		});
	});
});

describe('findBlockquoteMarkers', () => {
	function findMarker(text: string, maxWireLength?: number) {
		return findBlockquoteMarkers(
			text,
			computeMarkdownHighlightSpans(text),
			DEFAULT_COMPOSER_MARKDOWN_FLAGS,
			maxWireLength,
		).multiline;
	}

	function splitIntoLineQuotes(text: string, line: number, indent: number): string {
		return text
			.split('\n')
			.map((source, index) => {
				if (index < line) {
					return source;
				}
				return index === line ? `${source.slice(0, indent)}> ${source.slice(indent + 4)}` : `> ${source}`;
			})
			.join('\n');
	}

	function renderedNodes(text: string): string {
		return JSON.stringify(parseMarkdownAstWithWasm(text, DEFAULT_COMPOSER_MARKDOWN_FLAGS).nodes);
	}

	it.each<[string, number, number]>([
		['>>> a\nb', 0, 0],
		['  >>> a\n  b', 0, 2],
		['x\n>>> a\nb', 1, 0],
		['>>> a\n\nb', 0, 0],
		['>>> a\n\\> b', 0, 0],
		['>>> a\n`> b`', 0, 0],
		['>>> a\n>b', 0, 0],
		['> x\n\n>>> a\nb', 2, 0],
	])('splits %j at line %i with indent %i and keeps the rendered message', (text, line, indent) => {
		expect(findMarker(text)).toEqual({line, indent, splits: true});
		expect(renderedNodes(splitIntoLineQuotes(text, line, indent))).toBe(renderedNodes(text));
	});

	it.each<[string, number]>([
		['>>> a\n> b', 0],
		['>>> intro\n> quoted reply\nclosing words', 0],
		['>>> > a\nb', 0],
		['>>> a\n> [!NOTE]\n> b', 0],
		['>>> a\n>>> b', 0],
		['>>> a\n  > b', 0],
		['>>> a\n\t> b', 0],
		['> x\n>>> a\nb', 1],
		['> [!NOTE]\n>>> a\nb', 1],
		['||s\n>>> a\nb||\nc', 1],
		['>>> a\n', 0],
		['>>> a\nb\n', 0],
	])('does not split %j because line quotes would change the rendered message', (text, line) => {
		expect(findMarker(text)).toEqual({line, indent: 0, splits: false});
		expect(renderedNodes(splitIntoLineQuotes(text, line, 0))).not.toBe(renderedNodes(text));
	});

	it.each<[number, boolean]>([
		[10, false],
		[11, true],
	])('splits ">>> a\\nb\\nc" under a max wire length of %i: %s', (maxWireLength, splits) => {
		expect(findMarker('>>> a\nb\nc', maxWireLength)).toMatchObject({splits});
	});

	it.each(['> a', '>>>a', '>>> [!NOTE]\nbody', '```\n>>> a', '> >>> a'])('finds no marker in %j', (text) => {
		expect(findMarker(text)).toBeNull();
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

	it('drops a trailing empty quote line below a quoted fence when code blocks are off', () => {
		expect(dropTrailingEmptyBlockquoteLines('> ```\n> ')).toBe('> ```\n> ');
		expect(dropTrailingEmptyBlockquoteLines('> ```\n> ', NO_CODE_BLOCK_FLAGS)).toBe('> ```');
	});
});
