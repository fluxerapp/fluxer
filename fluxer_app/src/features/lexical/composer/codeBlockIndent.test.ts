// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	analyzeCodeIndent,
	type CodeIndentPlan,
	isOffsetInsideCodeBlock,
	scanCodeBlocks,
} from '@app/features/lexical/composer/codeBlockIndent';
import {
	computeMarkdownHighlightSpans,
	DEFAULT_COMPOSER_MARKDOWN_FLAGS,
	MarkdownHl,
} from '@app/features/lexical/composer/markdownSpans';
import {parseMarkdownAstWithWasm} from '@app/features/messaging/utils/markdown/parser/MarkdownParserWasm';
import type {CodeBlockNode, Node} from '@app/features/messaging/utils/markdown/parser/Nodes';
import {describe, expect, it} from 'vitest';

type OffsetRow = [label: string, text: string, offset: number, expected: boolean];

const OFFSET_ROWS: Array<OffsetRow> = [
	['a caret right after a lone opening fence is inside', '```', 3, true],
	['a caret before the opening fence is outside', '```', 0, false],
	['a caret within the opening backtick run is outside', '```', 2, false],
	['two backticks do not open a block', '``', 2, false],
	['a caret after the language of an unclosed fence is inside', '```js', 5, true],
	['a caret between the fence and its language is inside', '```js', 3, true],
	['the empty line after an opening fence is inside', '```\n', 4, true],
	['the end of an unclosed block is inside', '```js\nconst a = 1', 17, true],
	['a fence indented by four spaces opens a block', '    ```\ncode', 12, true],
	['a tab-indented fence opens a block', '\t```\ncode', 9, true],
	['a caret after the language on the opening line of a closed block is inside', '```js\ncode\n```', 5, true],
	['a caret in the body of a closed block is inside', '```\ncode\n```', 6, true],
	['the start of the closing fence line is inside', '```\ncode\n```', 9, true],
	['a caret within the closing backtick run is outside', '```\ncode\n```', 10, false],
	['a caret after the closing fence is outside', '```\ncode\n```', 12, false],
	['trailing spaces after the closing fence stay outside', '```\ncode\n```  ', 14, false],
	['the indent before an indented closing fence is inside', '```\ncode\n  ```', 11, true],
	['a caret after an indented closing fence is outside', '```\ncode\n  ```', 14, false],
	['content before a closing fence on the same line is inside', '```\ncode ```', 9, true],
	['a caret after a closing fence that follows content is outside', '```\ncode ```', 12, false],
	['a fence followed by text does not close the block', '```\ncode\n```x', 13, true],
	['a longer backtick run closes the block', '```\ncode\n````', 13, false],
	['a caret right after a closing fence with text after it is outside', '```\na\n``` x ```', 9, false],
	['text after a closing fence can open a new block', '```\na\n``` x ```', 15, true],
	['a caret inside a one-line block is inside', '```hello```', 5, true],
	['a caret after a one-line block is outside', '```hello```', 11, false],
	['the line after a one-line block is outside', '```hello```\nmore', 16, false],
	['a one-line block inside prose holds the caret', 'use ```foo``` here', 9, true],
	['prose after a one-line block is outside', 'use ```foo``` here', 18, false],
	['a backtick left over after a one-line block is outside', '```a````', 8, false],
	['a caret after a midline opening fence is inside', 'label```js\ncode', 15, true],
	['a caret before a midline opening fence is outside', 'label```js\ncode', 5, false],
	['a caret before the midline fence of a closed block is outside', 'label```js\ncode\n```', 5, false],
	['an unclosed fence after text on the same line is inside', 'hello ```', 9, true],
	['a sentence that mentions a fence stays outside', 'type ``` to start a block', 25, false],
	['prose that holds an unmatched fence stays outside', 'use ``` for code blocks', 23, false],
	['an unclosed fence whose info is not a language opens a block at the start of a line', '```js hello', 11, true],
	['a lone fence followed by a space is inside', '``` ', 4, true],
	['a midline fence after a preceding line holds its body', 'intro line\nlabel```rust\nfn main() {}\n```', 29, true],
	[
		'a caret after a midline block that follows a line is outside',
		'intro line\nlabel```rust\nfn main() {}\n```',
		40,
		false,
	],
	['an escaped fence does not open a block', '\\```js\ncode', 11, false],
	['an escaped backslash leaves the fence live', '\\\\```js\ncode', 12, true],
	['a one-line block after an escaped backslash holds the caret', '\\\\```hello```', 7, true],
	['a caret after a one-line block that follows an escaped backslash is outside', '\\\\```hello```', 13, false],
	['a longer escaped fence stays text', '\\````hello````', 7, false],
	['the end of a longer escaped fence stays outside', '\\````hello````', 14, false],
	['an escaped midline fence stays text', 'label\\```rust\nfn main() {}\n```', 19, false],
	['a fence inside inline code does not open a block', '`` ``` ``\ncode', 14, false],
	['a fence inside inline code in prose does not open a block', 'use `` ``` `` to open a block', 29, false],
	['a caret on a fence inside inline code is outside', 'use `` ``` `` to open a block', 8, false],
	['a fence after closed inline code opens a block', '`code` ```js\nx', 14, true],
	['a block opened after closed inline code holds its body', '`code` ```js\nxy\n```', 14, true],
	['a caret after a block opened after closed inline code is outside', '`code` ```js\nxy\n```', 19, false],
	['three backticks do not close a four-backtick fence', '````\ncode\n```\nmore', 18, true],
	['a four-backtick block holds a three-backtick line', '````\ncode\n```\nmore\n````', 16, true],
	['a caret after a four-backtick block is outside', '````\ncode\n```\nmore\n````', 23, false],
	[
		'an unclosed longer outer fence wins over a closed inner block because the user is still writing the outer block',
		'````\ncode\n```\nmore\n```',
		22,
		true,
	],
	['the end of the first closing fence line is outside', '```\na\n```\ntext\n```\nb\n```', 9, false],
	['text between two blocks is outside', '```\na\n```\ntext\n```\nb\n```', 12, false],
	['the end of the text between two blocks is outside', '```\na\n```\ntext\n```\nb\n```', 14, false],
	['the body of the second of two blocks is inside', '```\na\n```\ntext\n```\nb\n```', 20, true],
	['a caret after the second of two blocks is outside', '```\na\n```\ntext\n```\nb\n```', 24, false],
	['a block that opens after a closed block is inside', '```\na\n```\n```\n', 14, true],
	['the caret after the opening fence of an empty block is inside', '```\n```', 3, true],
	['the body of an empty block is inside', '```\n```', 4, true],
	['the closing fence of an empty block reopens a block, as the parser reads it', '```\n```', 7, true],
	['the empty middle line of an empty block is inside', '```\n\n```', 4, true],
	['a closed block inside a block spoiler holds the caret', '||```\ncode\n```||', 10, true],
	['a caret after a block spoiler around a closed block is outside', '||```\ncode\n```||', 16, false],
	['a fence in an unclosed block spoiler opens a block', '||```\ncode', 10, true],
	['an unclosed quoted fence ends with its quote', '> ```\ncode\n```', 10, false],
	['an unquoted fence after a quoted fence opens another block', '> ```\ncode\n```', 14, true],
	['a block closed on a quoted line holds the caret', '> ```\n> code\n> ```', 12, true],
	['a caret after a block closed on a quoted line is outside', '> ```\n> code\n> ```', 18, false],
	['a multiline quote holds a closed block', '>>> ```\ncode\n```', 12, true],
	['a caret after a closed block in a multiline quote is outside', '>>> ```\ncode\n```', 16, false],
	['an unclosed quoted fence does not hold the text after the quote', '> ```\ncode\nnormal text', 22, false],
	['an unclosed quoted fence does not hold a later unquoted line', '> ```\n> code\nplain', 18, false],
	['an unclosed spoilered fence does not hold the text after the spoiler', '||```\ncode|| after', 18, false],
	['a fence on a list item line does not open a block', '- ```', 5, false],
	['a fence on a numbered list item line does not open a block', '1. ```\ncode', 11, false],
	['a closed block written on a list item line does not open a block', '- ```\ncode\n```', 8, false],
	['a fence in prose on a list item line does not open a block', '- use ``` for code', 18, false],
	['a fence on the line after a list item opens a block', '- a\n```\ncode\n```', 9, true],
	['empty text is outside', '', 0, false],
	['plain text is outside', 'hello', 5, false],
];

type BodyRow = [label: string, text: string, bodies: ReturnType<typeof scanCodeBlocks>];

const BODY_ROWS: Array<BodyRow> = [
	['a closed block', '```\ncode\n```', [{start: 3, end: 9, rendered: true}]],
	['an unclosed block runs to the end of the text', '```js\ncode', [{start: 3, end: 10, rendered: false}]],
	[
		'an empty block is not rendered and its closing fence opens another block',
		'```\n\n```',
		[
			{start: 3, end: 5, rendered: false},
			{start: 8, end: 8, rendered: false},
		],
	],
	[
		'an unclosed outer fence and the closed block inside it',
		'````\ncode\n```\nmore\n```',
		[
			{start: 4, end: 22, rendered: false},
			{start: 13, end: 19, rendered: true},
		],
	],
	[
		'a closed block inside an unclosed block spoiler is not rendered',
		'||```\ncode\n```',
		[{start: 5, end: 11, rendered: false}],
	],
	['a closed block inside a block spoiler', '||```\ncode\n```||', [{start: 5, end: 11, rendered: true}]],
	['a fence written on a list item line', '- ```\ncode\n```', [{start: 14, end: 14, rendered: false}]],
	['an unclosed quoted fence bounded by its quote', '> ```\ncode\nnormal text', [{start: 5, end: 5, rendered: false}]],
	['an unclosed spoilered fence bounded by its spoiler', '||```\ncode|| after', [{start: 5, end: 10, rendered: false}]],
	['prose that holds an unmatched fence', 'use ``` for code blocks', []],
];

type IndentRow = [
	label: string,
	text: string,
	selectionStart: number,
	selectionEnd: number,
	unindent: boolean,
	expected: CodeIndentPlan | null,
];

function tabAt(offset: number): CodeIndentPlan {
	return {edits: [{start: offset, end: offset, text: '\t'}], selectionStart: offset + 1, selectionEnd: offset + 1};
}

const INDENT_ROWS: Array<IndentRow> = [
	['still indents the caret in the body of a closed block', '```\ncode\n```', 4, 4, false, tabAt(4)],
	['still ignores a caret on the closing fence line', '```\ncode\n```', 12, 12, false, null],
	['still indents the caret at the end of an unclosed block', '```\ncode', 8, 8, false, tabAt(8)],
	[
		'still unindents a tab in the body',
		'```\n\tcode\n```',
		5,
		5,
		true,
		{edits: [{start: 4, end: 5, text: ''}], selectionStart: 4, selectionEnd: 4},
	],
	[
		'still indents every selected body line',
		'```\na\nb\n```',
		4,
		7,
		false,
		{
			edits: [
				{start: 4, end: 4, text: '\t'},
				{start: 6, end: 6, text: '\t'},
			],
			selectionStart: 5,
			selectionEnd: 9,
		},
	],
	['still ignores an indented closing fence line', '```\ncode\n  ```', 14, 14, false, null],
	['still treats content before a closing fence on its line as code', '```\ncode ```', 8, 8, false, tabAt(8)],
	['no longer treats the line after a one-line block as code', '```hello```\nmore', 16, 16, false, null],
	[
		'no longer lets three backticks close a four-backtick fence',
		'````\ncode\n```\nmore\n````',
		18,
		18,
		false,
		tabAt(18),
	],
	['recognises a midline opening fence', 'label```js\ncode\n```', 15, 15, false, tabAt(15)],
	['recognises a fence indented by four spaces', '    ```\ncode', 12, 12, false, tabAt(12)],
	['no longer indents the line after an unclosed quoted fence', '> ```\ncode\nnormal text', 22, 22, false, null],
	['no longer indents the line after a fence on a list item line', '- ```\ncode', 10, 10, false, null],
];

const HIGHLIGHT_ROWS: Array<[text: string, offset: number, expected: boolean]> = [
	['```\ncode\n```', 6, true],
	['```hello```', 5, true],
	['use ```foo``` here', 9, true],
	['intro line\nlabel```rust\nfn main() {}\n```', 29, true],
	['\\\\```hello```', 7, true],
	['`code` ```js\nxy\n```', 14, true],
	['````\ncode\n```\nmore\n````', 16, true],
	['```\na\n```\ntext\n```\nb\n```', 20, true],
	['```\na\n```\ntext\n```\nb\n```', 12, false],
	['\\````hello````', 7, false],
	['label\\```rust\nfn main() {}\n```', 19, false],
	['use `` ``` `` to open a block', 8, false],
];

const CONTAINER_FIXTURES = [
	'hi\n||```\ncode\n```||',
	'||a||\n```\ncode\n```',
	'||\n```\ncode\n```\n||',
	'> > ```\n> code\n> ```',
	'>>> a\n> ```\n> code\n> ```',
	'> [!NOTE]\n> ```\n> code\n> ```',
	'- a\n```\ncode\n```',
	'```\n```\ncode\n```',
	'```js title\ncode\n```',
];

const INVISIBLE_CONTENT_FIXTURES = [
	'```\n\u200b\n```',
	'```\n\u00a0\u3164\n```',
	'```\u034f```',
	'```\na\u034f\n```',
	'```\n\ufe0f\n```\ncode\n```',
	'||\u200b\n```\n||',
];

const CORPUS_PIECES = [
	'```',
	'```',
	'`',
	'\\',
	' ',
	'  ',
	'\t',
	'\n',
	'\n',
	'\n',
	'> ',
	'>>> ',
	'||',
	'|',
	'>',
	'- ',
	'* ',
	'1. ',
	'a',
	'x',
];
const CORPUS_SIZE = 5000;
const CORPUS_MAX_PIECES = 24;

function createCorpus(): Array<string> {
	let state = 0x2545f491;
	const nextIndex = (bound: number): number => {
		state ^= state << 13;
		state ^= state >>> 17;
		state ^= state << 5;
		return (state >>> 0) % bound;
	};
	return Array.from({length: CORPUS_SIZE}, () =>
		Array.from({length: 1 + nextIndex(CORPUS_MAX_PIECES)}, () => CORPUS_PIECES[nextIndex(CORPUS_PIECES.length)]).join(
			'',
		),
	);
}

function collectCodeBlocks(nodes: ReadonlyArray<Node>): Array<CodeBlockNode> {
	return nodes.flatMap((node) => {
		if (node.type === 'CodeBlock') {
			return [node];
		}
		if (node.type === 'List') {
			return node.items.flatMap((item) => collectCodeBlocks(item.children));
		}
		return 'children' in node ? collectCodeBlocks(node.children) : [];
	});
}

function withoutQuoteMarkersOrWhitespace(value: string): string {
	return value.replace(/[\s>]/g, '');
}

function parserCodeBlocks(text: string): Array<string> {
	return collectCodeBlocks(parseMarkdownAstWithWasm(text, DEFAULT_COMPOSER_MARKDOWN_FLAGS).nodes).map((node) =>
		withoutQuoteMarkersOrWhitespace(`${node.language ?? ''}${node.content}`),
	);
}

function scannedCodeBlocks(text: string): Array<string> {
	return scanCodeBlocks(text)
		.filter((body) => body.rendered)
		.map((body) => withoutQuoteMarkersOrWhitespace(text.slice(body.start, body.end)));
}

describe('isOffsetInsideCodeBlock', () => {
	it.each(OFFSET_ROWS)('%s', (_label, text, offset, expected) => {
		expect(isOffsetInsideCodeBlock(text, offset)).toBe(expected);
	});
});

describe('scanCodeBlocks', () => {
	it.each(BODY_ROWS)('reports %s', (_label, text, bodies) => {
		expect(scanCodeBlocks(text)).toEqual(bodies);
	});
});

describe('scanCodeBlocks against the markdown parser', () => {
	const fixtures = new Set([
		...OFFSET_ROWS.map(([, text]) => text),
		...BODY_ROWS.map(([, text]) => text),
		...INDENT_ROWS.map(([, text]) => text),
		...CONTAINER_FIXTURES,
		...INVISIBLE_CONTENT_FIXTURES,
	]);

	it.each([...fixtures])('closes the same blocks as the parser in %j', (text) => {
		expect(scannedCodeBlocks(text)).toEqual(parserCodeBlocks(text));
	});

	it('closes the same blocks as the parser across a seeded generated corpus', () => {
		const containers = new Set<string>();
		for (const text of createCorpus()) {
			for (const node of parseMarkdownAstWithWasm(text, DEFAULT_COMPOSER_MARKDOWN_FLAGS).nodes) {
				if (collectCodeBlocks([node]).length > 0) {
					containers.add(node.type);
				}
			}
			expect({text, blocks: scannedCodeBlocks(text)}).toEqual({text, blocks: parserCodeBlocks(text)});
		}
		expect([...containers.keys()].sort()).toEqual(['Blockquote', 'CodeBlock', 'List', 'Spoiler']);
	});

	it.each(HIGHLIGHT_ROWS)('agrees with the composer highlight in %j at %i', (text, offset, expected) => {
		expect(isOffsetInsideCodeBlock(text, offset)).toBe(expected);
		expect(
			computeMarkdownHighlightSpans(text).some(
				(span) =>
					span.role === 'content' &&
					(span.format & MarkdownHl.codeBlock) !== 0 &&
					span.start < offset &&
					offset < span.end,
			),
		).toBe(expected);
	});
});

describe('analyzeCodeIndent', () => {
	it.each(INDENT_ROWS)('%s', (_label, text, selectionStart, selectionEnd, unindent, expected) => {
		expect(analyzeCodeIndent(text, selectionStart, selectionEnd, unindent)).toEqual(expected);
	});
});
