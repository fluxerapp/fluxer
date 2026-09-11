// SPDX-License-Identifier: AGPL-3.0-or-later

import {registerComposerMarkdownHighlight} from '@app/features/lexical/composer/ComposerMarkdownHighlight';
import {type CodeBlockWrapPlan, planCodeBlockWrap} from '@app/features/lexical/composer/codeBlockWrap';
import {
	$captureSelectionOffsets,
	$getComposerDisplayText,
	$isComposerSelectionWrapped,
	$replaceComposerRange,
	$selectComposerRange,
	$wrapComposerSelection,
} from '@app/features/lexical/composer/composerOffsets';
import {DEFAULT_COMPOSER_MARKDOWN_FLAGS} from '@app/features/lexical/composer/markdownSpans';
import {
	$isComposerCustomEmojiNode,
	ComposerCustomEmojiNode,
} from '@app/features/lexical/composer/nodes/ComposerCustomEmojiNode';
import {$isComposerMentionNode, ComposerMentionNode} from '@app/features/lexical/composer/nodes/ComposerMentionNode';
import {$isSyntaxMarkerNode, SyntaxMarkerNode} from '@app/features/lexical/composer/nodes/SyntaxMarkerNode';
import {parseMarkdownAstWithWasm} from '@app/features/messaging/utils/markdown/parser/MarkdownParserWasm';
import type {CodeBlockNode, Node} from '@app/features/messaging/utils/markdown/parser/Nodes';
import {createEmptyHistoryState, registerHistory} from '@lexical/history';
import {
	$getNodeByKey,
	$getRoot,
	createEditor,
	type ElementNode,
	type LexicalEditor,
	type NodeKey,
	REDO_COMMAND,
	UNDO_COMMAND,
} from 'lexical';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/lexical/composer/nodes/ComposerMentionPill', () => ({ComposerMentionPill: () => null}));
vi.mock('@app/features/lexical/composer/nodes/ComposerCustomEmoji', () => ({ComposerCustomEmoji: () => null}));
vi.mock('@app/features/lexical/composer/nodes/ComposerStandardEmoji', () => ({ComposerStandardEmoji: () => null}));

interface PlanResult {
	wrapped: boolean;
	text: string;
	selection: [number, number];
}

type PlanRow = [label: string, text: string, start: number, end: number, expected: PlanResult | null];

function applyPlan(
	text: string,
	{wrapped, opening, closing, selectionStart, selectionEnd}: CodeBlockWrapPlan,
): PlanResult {
	const head = text.slice(0, opening.start);
	const middle = text.slice(opening.end, closing.start);
	return {
		wrapped,
		text: `${head}${opening.text}${middle}${closing.text}${text.slice(closing.end)}`,
		selection: [selectionStart, selectionEnd],
	};
}

function planned(text: string, start: number, end: number): PlanResult | null {
	const plan = planCodeBlockWrap(text, start, end);
	return plan == null ? null : applyPlan(text, plan);
}

const PLAN_ROWS: Array<PlanRow> = [
	['keeps inline code for a single-line selection', 'say hello there', 4, 9, null],
	['keeps inline code for a single-line selection beside a block', '```\na\n```\nhello there', 10, 15, null],
	['keeps inline code for a collapsed caret', 'a\nb', 1, 1, null],
	['keeps inline code for one line inside a block', '```\na\nb\n```', 4, 5, null],
	['declines a selection of newlines only', 'a\n\nb', 1, 3, null],
	['declines part of a block body', '```\na\nb\nc\n```', 4, 7, null],
	['declines a selection that ends inside a block', 'x\n```\na\nb\n```', 0, 7, null],
	['declines a block whose opening line holds content rather than a language', '```a b\nc\nd\n```', 7, 10, null],
	['declines a fenced block inside a quote', '> ```\n> a\n> b\n> ```', 6, 13, null],
	['declines a block inside a spoiler that never closed', '||\n```\na\nb\n```', 7, 10, null],
	['declines whitespace-only lines the parser would not render as a block', ' \n ', 0, 3, null],
	['declines a selection across a block, text and another block', '```\na\n```\nmid\n```\nb\n```', 4, 19, null],
	[
		'wraps a multiline selection inside a block that is still open',
		'```\na\nb',
		4,
		7,
		{wrapped: false, text: '```\n```\na\nb\n```', selection: [8, 11]},
	],
	['wraps a whole two-line message', 'a\nb', 0, 3, {wrapped: false, text: '```\na\nb\n```', selection: [4, 7]}],
	[
		'wraps whole lines in the middle without inserting breaks',
		'x\na\nb\ny',
		2,
		5,
		{wrapped: false, text: 'x\n```\na\nb\n```\ny', selection: [6, 9]},
	],
	[
		'inserts a break before the opening fence when the selection starts mid-line',
		'hi a\nb',
		3,
		6,
		{wrapped: false, text: 'hi \n```\na\nb\n```', selection: [8, 11]},
	],
	[
		'inserts a break after the closing fence when the selection ends mid-line',
		'a\nb bye',
		0,
		3,
		{wrapped: false, text: '```\na\nb\n```\n bye', selection: [4, 7]},
	],
	[
		'inserts a break at both edges when the selection starts and ends mid-line',
		'hi a\nb bye',
		3,
		6,
		{wrapped: false, text: 'hi \n```\na\nb\n```\n bye', selection: [8, 11]},
	],
	[
		'keeps a selected trailing newline outside the block',
		'a\nb\nc',
		0,
		4,
		{wrapped: false, text: '```\na\nb\n```\nc', selection: [4, 7]},
	],
	[
		'keeps a selected leading newline outside the block',
		'x\na\nb',
		1,
		5,
		{wrapped: false, text: 'x\n```\na\nb\n```', selection: [6, 9]},
	],
	[
		'keeps several selected edge newlines outside the block',
		'x\n\na\nb\n\ny',
		1,
		8,
		{wrapped: false, text: 'x\n\n```\na\nb\n```\n\ny', selection: [7, 10]},
	],
	[
		'treats one line with its selected line break as a block',
		'foo\nbar',
		0,
		4,
		{wrapped: false, text: '```\nfoo\n```\nbar', selection: [4, 7]},
	],
	[
		'wraps the lines between two blocks',
		'```\nA\n```\nB\nC\n```\nD\n```',
		10,
		13,
		{wrapped: false, text: '```\nA\n```\n```\nB\nC\n```\n```\nD\n```', selection: [14, 17]},
	],
	[
		'wraps a multiline selection of inline code that never closed',
		'`a\nb`',
		0,
		5,
		{wrapped: false, text: '```\n`a\nb`\n```', selection: [4, 9]},
	],
	[
		'wraps two whole blocks in a longer fence',
		'```\na\n```\n```\nb\n```',
		0,
		19,
		{wrapped: false, text: '````\n```\na\n```\n```\nb\n```\n````', selection: [5, 24]},
	],
	[
		'wraps content that already holds a fence in a longer fence',
		'x\n```\ny\n```\nz',
		0,
		13,
		{wrapped: false, text: '````\nx\n```\ny\n```\nz\n````', selection: [5, 18]},
	],
	[
		'unwraps when the block content is selected',
		'```\na\nb\n```',
		4,
		7,
		{wrapped: true, text: 'a\nb', selection: [0, 3]},
	],
	[
		'unwraps a block and leaves the text around it alone',
		'x\n```\na\nb\n```\ny',
		6,
		9,
		{wrapped: true, text: 'x\na\nb\ny', selection: [2, 5]},
	],
	[
		'unwraps a block that is followed by a line the wrap kept outside',
		'```\na\nb\n```\nc',
		4,
		7,
		{wrapped: true, text: 'a\nb\nc', selection: [0, 3]},
	],
	[
		'unwraps a block whose content is a single line',
		'```\nfoo\n```\nbar',
		4,
		7,
		{wrapped: true, text: 'foo\nbar', selection: [0, 3]},
	],
	[
		'unwraps when the fences are selected too',
		'```\na\nb\n```',
		0,
		11,
		{wrapped: true, text: 'a\nb', selection: [0, 3]},
	],
	[
		'unwraps when the fences and the newlines around them are selected',
		'x\n```\na\nb\n```\ny',
		1,
		14,
		{wrapped: true, text: 'x\na\nb\ny', selection: [2, 5]},
	],
	['unwraps a block with a language fence', '```js\na\nb\n```', 6, 9, {wrapped: true, text: 'a\nb', selection: [0, 3]}],
	[
		'unwraps a block that follows an escaped fence',
		'\\```\n```\na\nb\n```',
		9,
		12,
		{wrapped: true, text: '\\```\na\nb', selection: [5, 8]},
	],
	[
		'unwraps a block written with a longer fence',
		'````\nx\n```\ny\n```\nz\n````',
		5,
		18,
		{wrapped: true, text: 'x\n```\ny\n```\nz', selection: [0, 13]},
	],
	[
		'unwraps a block whose closing fence has trailing spaces',
		'```\na\nb\n```   ',
		4,
		7,
		{wrapped: true, text: 'a\nb', selection: [0, 3]},
	],
	['declines a block whose closing fence is longer than its opening fence', '```\na\nb\n`````', 4, 7, null],
	['declines a block whose opening fence is followed by spaces', '```  \na\nb\n```', 6, 9, null],
];

const ROUND_TRIP_ROWS: Array<[text: string, start: number, end: number]> = [
	['a\nb', 0, 3],
	['x\na\nb\ny', 2, 5],
	['a\nb\nc', 0, 4],
	['x\na\nb', 1, 5],
	['x\n\na\nb\n\ny', 1, 8],
	['foo\nbar', 0, 4],
	['```\nA\n```\nB\nC\n```\nD\n```', 10, 13],
	['`a\nb`', 0, 5],
	['```\na\n```\n```\nb\n```', 0, 19],
	['x\n```\ny\n```\nz', 0, 13],
	['```\na\nb', 4, 7],
	['> a\n> b', 0, 7],
];

const PARSER_ROWS: Array<[text: string, start: number, end: number, content: string]> = [
	['a\nb', 0, 3, 'a\nb\n'],
	['hi a\nb bye', 3, 6, 'a\nb\n'],
	['```\na\nb', 4, 7, 'a\nb\n'],
	['x\n```\ny\n```\nz', 0, 13, 'x\n```\ny\n```\nz\n'],
	['> a\n> b', 0, 7, '> a\n> b\n'],
];

function createComposer(): LexicalEditor {
	const editor = createEditor({
		namespace: 'code-block-wrap-test',
		nodes: [ComposerCustomEmojiNode, ComposerMentionNode, SyntaxMarkerNode],
		onError: (error) => {
			throw error;
		},
	});
	registerComposerMarkdownHighlight(editor);
	return editor;
}

function seeded(text: string, anchor: number, focus: number): LexicalEditor {
	const editor = createComposer();
	editor.update(
		() => {
			$replaceComposerRange(0, 0, {kind: 'text', text}, {leading: false, trailing: false});
			$selectComposerRange(anchor, focus);
		},
		{discrete: true},
	);
	return editor;
}

function clickFormat(editor: LexicalEditor, wrapper: string): void {
	editor.update(() => $wrapComposerSelection(wrapper, wrapper), {discrete: true});
}

function composerState(editor: LexicalEditor): {
	text: string;
	selection: {anchor: number; focus: number} | null;
	pressed: boolean;
} {
	return editor.read(() => ({
		text: $getComposerDisplayText(),
		selection: $captureSelectionOffsets(),
		pressed: $isComposerSelectionWrapped('`', '`'),
	}));
}

function atomicKeys(editor: LexicalEditor): Array<NodeKey> {
	return editor.read(() =>
		$getRoot()
			.getFirstChildOrThrow<ElementNode>()
			.getChildren()
			.filter((node) => $isComposerMentionNode(node) || $isComposerCustomEmojiNode(node))
			.map((node) => node.getKey()),
	);
}

function atomicState(editor: LexicalEditor, keys: Array<NodeKey>): Array<{attached: boolean; literal: boolean} | null> {
	return editor.read(() =>
		keys.map((key) => {
			const node = $getNodeByKey(key);
			if (!$isComposerMentionNode(node) && !$isComposerCustomEmojiNode(node)) {
				return null;
			}
			return {attached: node.isAttached(), literal: node.isLiteral()};
		}),
	);
}

function markerTexts(editor: LexicalEditor): Array<string> {
	return editor.read(() =>
		$getRoot()
			.getAllTextNodes()
			.filter($isSyntaxMarkerNode)
			.map((node) => node.getTextContent()),
	);
}

describe('planCodeBlockWrap', () => {
	it.each(PLAN_ROWS)('%s', (_label, text, start, end, expected) => {
		expect(planned(text, start, end)).toEqual(expected);
	});

	it.each(ROUND_TRIP_ROWS)('round trips %j selected from %i to %i', (text, start, end) => {
		const wrapped = planned(text, start, end);
		const unwrapped = wrapped == null ? null : planned(wrapped.text, ...wrapped.selection);
		expect({wrapped: unwrapped?.wrapped, text: unwrapped?.text}).toEqual({wrapped: true, text});
	});

	it('keeps the breaks a mid-line wrap inserted when the block is unwrapped again', () => {
		const wrapped = planned('hi a\nb bye', 3, 6);
		expect(wrapped).toEqual({wrapped: false, text: 'hi \n```\na\nb\n```\n bye', selection: [8, 11]});
		expect(wrapped == null ? null : planned(wrapped.text, ...wrapped.selection)).toEqual({
			wrapped: true,
			text: 'hi \na\nb\n bye',
			selection: [4, 7],
		});
	});
});

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

function parsedCodeBlocks(text: string): Array<string> {
	return collectCodeBlocks(parseMarkdownAstWithWasm(text, DEFAULT_COMPOSER_MARKDOWN_FLAGS).nodes).map(
		(node) => node.content,
	);
}

describe('planCodeBlockWrap against the markdown parser', () => {
	it.each(PARSER_ROWS)('wraps %j from %i to %i into one block the parser renders', (text, start, end, content) => {
		expect(parsedCodeBlocks(planned(text, start, end)?.text ?? '')).toEqual([content]);
	});
});

describe('the composer code option', () => {
	it('wraps a mid-line multiline selection in fence lines', () => {
		const editor = seeded('hi a\nb bye', 3, 6);
		clickFormat(editor, '`');
		expect(composerState(editor)).toEqual({
			text: 'hi \n```\na\nb\n```\n bye',
			selection: {anchor: 8, focus: 11},
			pressed: true,
		});
		expect(markerTexts(editor)).toEqual(['```', '```']);
	});

	it('restores the text when it is used twice on whole lines', () => {
		const editor = seeded('x\na\nb\ny', 2, 5);
		clickFormat(editor, '`');
		expect(composerState(editor)).toEqual({
			text: 'x\n```\na\nb\n```\ny',
			selection: {anchor: 6, focus: 9},
			pressed: true,
		});
		clickFormat(editor, '`');
		expect(composerState(editor)).toEqual({text: 'x\na\nb\ny', selection: {anchor: 2, focus: 5}, pressed: false});
	});

	it('keeps a backward selection backward through both clicks', () => {
		const editor = seeded('a\nb', 3, 0);
		clickFormat(editor, '`');
		expect(composerState(editor)).toEqual({
			text: '```\na\nb\n```',
			selection: {anchor: 7, focus: 4},
			pressed: true,
		});
		clickFormat(editor, '`');
		expect(composerState(editor)).toEqual({text: 'a\nb', selection: {anchor: 3, focus: 0}, pressed: false});
	});

	it('unwraps a block whose fences are selected as well', () => {
		const editor = seeded('```\na\nb\n```', 0, 11);
		expect(composerState(editor).pressed).toBe(true);
		clickFormat(editor, '`');
		expect(composerState(editor)).toEqual({text: 'a\nb', selection: {anchor: 0, focus: 3}, pressed: false});
	});

	it('unwraps a block with a language fence', () => {
		const editor = seeded('```js\na\nb\n```', 6, 9);
		expect(composerState(editor).pressed).toBe(true);
		clickFormat(editor, '`');
		expect(composerState(editor)).toEqual({text: 'a\nb', selection: {anchor: 0, focus: 3}, pressed: false});
	});

	it('still writes inline code for a single-line selection', () => {
		const editor = seeded('say hello there', 4, 9);
		clickFormat(editor, '`');
		expect(composerState(editor)).toEqual({
			text: 'say `hello` there',
			selection: {anchor: 5, focus: 10},
			pressed: true,
		});
	});

	it('still leaves other wrappers symmetric on a multiline selection', () => {
		const editor = seeded('a\nb', 0, 3);
		clickFormat(editor, '**');
		expect(composerState(editor)).toEqual({text: '**a\nb**', selection: {anchor: 2, focus: 5}, pressed: false});
	});

	it('wraps a multiline selection inside a block that is still open', () => {
		const editor = seeded('```\na\nb', 4, 7);
		clickFormat(editor, '`');
		expect(composerState(editor)).toEqual({
			text: '```\n```\na\nb\n```',
			selection: {anchor: 8, focus: 11},
			pressed: true,
		});
	});

	it('makes no change for a multiline selection inside a block', () => {
		const editor = seeded('```\na\nb\nc\n```', 4, 7);
		clickFormat(editor, '`');
		expect(composerState(editor)).toEqual({
			text: '```\na\nb\nc\n```',
			selection: {anchor: 4, focus: 7},
			pressed: false,
		});
	});

	it('makes no change for a multiline selection inside a quoted block', () => {
		const editor = seeded('> ```\n> a\n> b\n> ```', 6, 13);
		clickFormat(editor, '`');
		expect(composerState(editor)).toEqual({
			text: '> ```\n> a\n> b\n> ```',
			selection: {anchor: 6, focus: 13},
			pressed: false,
		});
	});

	it('makes no change for whitespace-only lines however often it is used', () => {
		const editor = seeded(' \n ', 0, 3);
		clickFormat(editor, '`');
		clickFormat(editor, '`');
		expect(composerState(editor)).toEqual({text: ' \n ', selection: {anchor: 0, focus: 3}, pressed: false});
	});

	it('removes single backticks left around a multiline selection', () => {
		const editor = seeded('`a\nb`', 1, 4);
		expect(composerState(editor).pressed).toBe(true);
		clickFormat(editor, '`');
		expect(composerState(editor)).toEqual({text: 'a\nb', selection: {anchor: 0, focus: 3}, pressed: false});
	});

	it('undoes the unwrap in one step', async () => {
		const editor = createComposer();
		const history = createEmptyHistoryState();
		registerHistory(editor, history, 300);
		editor.update(
			() => {
				$replaceComposerRange(0, 0, {kind: 'text', text: '```\na\nb\n```'}, {leading: false, trailing: false});
				$selectComposerRange(4, 7);
			},
			{discrete: true},
		);
		clickFormat(editor, '`');
		expect(composerState(editor)).toEqual({text: 'a\nb', selection: {anchor: 0, focus: 3}, pressed: false});
		expect(history.undoStack).toHaveLength(1);
		editor.dispatchCommand(UNDO_COMMAND, undefined);
		await Promise.resolve();
		expect(composerState(editor)).toEqual({
			text: '```\na\nb\n```',
			selection: {anchor: 4, focus: 7},
			pressed: true,
		});
	});

	it('keeps mention and emoji nodes when it wraps and unwraps them', () => {
		const editor = createComposer();
		editor.update(
			() => {
				$replaceComposerRange(
					0,
					0,
					{kind: 'mention', mentionType: 'user', id: '1', display: '@alice', wire: '<@1>'},
					{trailing: false},
				);
				$replaceComposerRange(6, 6, {kind: 'text', text: '\n'}, {leading: false, trailing: false});
				$replaceComposerRange(
					7,
					7,
					{kind: 'customEmoji', emojiId: '2', animated: false, display: ':blob:', wire: '<:blob:2>'},
					{trailing: false},
				);
				$selectComposerRange(0, 13);
			},
			{discrete: true},
		);
		const keys = atomicKeys(editor);
		expect(atomicState(editor, keys)).toEqual([
			{attached: true, literal: false},
			{attached: true, literal: false},
		]);
		clickFormat(editor, '`');
		expect(composerState(editor)).toEqual({
			text: '```\n@alice\n:blob:\n```',
			selection: {anchor: 4, focus: 17},
			pressed: true,
		});
		expect(atomicState(editor, keys)).toEqual([
			{attached: true, literal: true},
			{attached: true, literal: true},
		]);
		clickFormat(editor, '`');
		expect(composerState(editor)).toEqual({
			text: '@alice\n:blob:',
			selection: {anchor: 0, focus: 13},
			pressed: false,
		});
		expect(atomicState(editor, keys)).toEqual([
			{attached: true, literal: false},
			{attached: true, literal: false},
		]);
	});

	it('reads a mention as filler so backticks in its name cannot hide the block', () => {
		const editor = createComposer();
		editor.update(
			() => {
				$replaceComposerRange(0, 0, {kind: 'text', text: '```\n'}, {leading: false, trailing: false});
				$replaceComposerRange(
					4,
					4,
					{kind: 'mention', mentionType: 'user', id: '1', display: '@```', wire: '<@1>'},
					{trailing: false},
				);
				$replaceComposerRange(8, 8, {kind: 'text', text: '\n```'}, {leading: false, trailing: false});
				$selectComposerRange(4, 8);
			},
			{discrete: true},
		);
		expect(composerState(editor).pressed).toBe(true);
		clickFormat(editor, '`');
		expect(composerState(editor)).toEqual({text: '@```', selection: {anchor: 0, focus: 4}, pressed: false});
	});

	it('undoes the wrap in one step and redoes it', async () => {
		const editor = createComposer();
		const history = createEmptyHistoryState();
		registerHistory(editor, history, 300);
		editor.update(
			() => {
				$replaceComposerRange(0, 0, {kind: 'text', text: 'hi a\nb bye'}, {leading: false, trailing: false});
				$selectComposerRange(3, 6);
			},
			{discrete: true},
		);
		clickFormat(editor, '`');
		expect(history.undoStack).toHaveLength(1);
		editor.dispatchCommand(UNDO_COMMAND, undefined);
		await Promise.resolve();
		expect(composerState(editor)).toEqual({
			text: 'hi a\nb bye',
			selection: {anchor: 3, focus: 6},
			pressed: false,
		});
		editor.dispatchCommand(REDO_COMMAND, undefined);
		await Promise.resolve();
		expect(composerState(editor)).toEqual({
			text: 'hi \n```\na\nb\n```\n bye',
			selection: {anchor: 8, focus: 11},
			pressed: true,
		});
	});
});
