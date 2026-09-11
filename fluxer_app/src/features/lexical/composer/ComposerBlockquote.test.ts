// SPDX-License-Identifier: AGPL-3.0-or-later

import type {BlockquoteLine} from '@app/features/lexical/composer/blockquoteLines';
import {registerComposerBlockquote} from '@app/features/lexical/composer/ComposerBlockquote';
import {$insertComposerClipboardSlice} from '@app/features/lexical/composer/ComposerClipboard';
import {registerComposerEmojiShortcode} from '@app/features/lexical/composer/ComposerEmojiShortcode';
import {registerComposerMarkdownHighlight} from '@app/features/lexical/composer/ComposerMarkdownHighlight';
import {$hydrateComposerFromDraft, $projectComposer} from '@app/features/lexical/composer/ComposerSerialization';
import {registerComposerSoftWrapDeletion} from '@app/features/lexical/composer/ComposerSoftWrapDeletion';
import {$getComposerBlockquoteState, $selectComposerRange} from '@app/features/lexical/composer/composerOffsets';
import {DEFAULT_COMPOSER_MARKDOWN_FLAGS} from '@app/features/lexical/composer/markdownSpans';
import {
	$isComposerBlockquoteLineNode,
	ComposerBlockquoteLineNode,
} from '@app/features/lexical/composer/nodes/ComposerBlockquoteLineNode';
import {
	$isComposerBlockquoteMarkerNode,
	ComposerBlockquoteMarkerNode,
} from '@app/features/lexical/composer/nodes/ComposerBlockquoteMarkerNode';
import {ComposerCommandNode} from '@app/features/lexical/composer/nodes/ComposerCommandNode';
import {
	$isComposerCustomEmojiNode,
	ComposerCustomEmojiNode,
} from '@app/features/lexical/composer/nodes/ComposerCustomEmojiNode';
import {$isComposerMentionNode, ComposerMentionNode} from '@app/features/lexical/composer/nodes/ComposerMentionNode';
import {ComposerPlainSegmentNode} from '@app/features/lexical/composer/nodes/ComposerPlainSegmentNode';
import {ComposerStandardEmojiNode} from '@app/features/lexical/composer/nodes/ComposerStandardEmojiNode';
import {SlashOptionalHintNode} from '@app/features/lexical/composer/nodes/SlashOptionalHintNode';
import {SlashSeparatorNode} from '@app/features/lexical/composer/nodes/SlashSeparatorNode';
import {SlashSlotNode} from '@app/features/lexical/composer/nodes/SlashSlotNode';
import {SlashSlotPlaceholderNode} from '@app/features/lexical/composer/nodes/SlashSlotPlaceholderNode';
import {$createSyntaxMarkerNode, SyntaxMarkerNode} from '@app/features/lexical/composer/nodes/SyntaxMarkerNode';
import {ParserFlags} from '@app/features/messaging/utils/markdown/parser/Enums';
import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';
import {createEmptyHistoryState, registerHistory} from '@lexical/history';
import {
	$createParagraphNode,
	$getNodeByKey,
	$getRoot,
	$getSelection,
	$isElementNode,
	$isLineBreakNode,
	$isRangeSelection,
	$isTextNode,
	$setCompositionKey,
	COMMAND_PRIORITY_EDITOR,
	COMMAND_PRIORITY_HIGH,
	COMPOSITION_START_COMMAND,
	CONTROLLED_TEXT_INSERTION_COMMAND,
	createEditor,
	DELETE_CHARACTER_COMMAND,
	INSERT_LINE_BREAK_COMMAND,
	INSERT_PARAGRAPH_COMMAND,
	KEY_ARROW_LEFT_COMMAND,
	type LexicalCommand,
	type LexicalEditor,
	REDO_COMMAND,
	SELECTION_CHANGE_COMMAND,
	UNDO_COMMAND,
} from 'lexical';
import {afterEach, describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/lexical/composer/nodes/ComposerMentionPill', () => ({ComposerMentionPill: () => null}));
vi.mock('@app/features/lexical/composer/nodes/ComposerCustomEmoji', () => ({ComposerCustomEmoji: () => null}));
vi.mock('@app/features/lexical/composer/nodes/ComposerStandardEmoji', () => ({ComposerStandardEmoji: () => null}));
vi.mock('@app/features/lexical/composer/nodes/SlashOptionalHintPill', () => ({SlashOptionalHintPill: () => null}));
vi.mock('@app/features/expressions/utils/EmojiUtils', () => ({getEmojiURL: () => null}));
vi.mock('@lingui/core/macro', () => ({msg: (descriptor: unknown) => descriptor}));

const NODES = [
	ComposerMentionNode,
	ComposerCustomEmojiNode,
	ComposerStandardEmojiNode,
	ComposerPlainSegmentNode,
	ComposerCommandNode,
	SlashSlotNode,
	SlashSlotPlaceholderNode,
	SlashSeparatorNode,
	SlashOptionalHintNode,
	SyntaxMarkerNode,
	ComposerBlockquoteLineNode,
	ComposerBlockquoteMarkerNode,
];

const NO_BLOCKQUOTE_FLAGS =
	DEFAULT_COMPOSER_MARKDOWN_FLAGS & ~(ParserFlags.ALLOW_BLOCKQUOTES | ParserFlags.ALLOW_MULTILINE_BLOCKQUOTES);
const NO_MULTILINE_FLAGS = DEFAULT_COMPOSER_MARKDOWN_FLAGS & ~ParserFlags.ALLOW_MULTILINE_BLOCKQUOTES;
const BIO_LIKE_FLAGS =
	DEFAULT_COMPOSER_MARKDOWN_FLAGS &
	~(
		ParserFlags.ALLOW_HEADINGS |
		ParserFlags.ALLOW_CODE_BLOCKS |
		ParserFlags.ALLOW_ROLE_MENTIONS |
		ParserFlags.ALLOW_EVERYONE_MENTIONS |
		ParserFlags.ALLOW_SUBTEXT |
		ParserFlags.ALLOW_TABLES |
		ParserFlags.ALLOW_ALERTS
	);

const COMPOSITION_EVENT = {timeStamp: Number.MAX_SAFE_INTEGER, data: ''} as unknown as CompositionEvent;

const disposers: Array<() => void> = [];

afterEach(() => {
	while (disposers.length > 0) {
		disposers.pop()!();
	}
});

function createBareEditor(): LexicalEditor {
	return createEditor({
		namespace: 'composer-blockquote-test',
		nodes: NODES,
		onError: (error) => {
			throw error;
		},
	});
}

function registerPlainTextFallbacks(editor: LexicalEditor): void {
	disposers.push(
		editor.registerCommand(
			INSERT_LINE_BREAK_COMMAND,
			(selectStart) => {
				const selection = $getSelection();
				if (!$isRangeSelection(selection)) {
					return false;
				}
				selection.insertLineBreak(selectStart);
				return true;
			},
			COMMAND_PRIORITY_EDITOR,
		),
		editor.registerCommand(
			CONTROLLED_TEXT_INSERTION_COMMAND,
			(payload) => {
				const selection = $getSelection();
				if (!$isRangeSelection(selection) || typeof payload !== 'string') {
					return false;
				}
				selection.insertText(payload);
				return true;
			},
			COMMAND_PRIORITY_EDITOR,
		),
	);
}

function createHarness(options: {flags?: number; history?: boolean} = {}): LexicalEditor {
	const editor = createBareEditor();
	disposers.push(registerComposerMarkdownHighlight(editor, options.flags), registerComposerBlockquote(editor));
	registerPlainTextFallbacks(editor);
	if (options.history === true) {
		disposers.push(registerHistory(editor, createEmptyHistoryState(), 0));
	}
	return editor;
}

function update(editor: LexicalEditor, fn: () => void): void {
	editor.update(fn, {discrete: true});
}

function setComposer(
	editor: LexicalEditor,
	text: string,
	anchor?: number,
	focus?: number,
	segments: ReadonlyArray<MentionSegment> = [],
): void {
	update(editor, () => {
		$hydrateComposerFromDraft(text, segments);
		if (anchor != null) {
			$selectComposerRange(anchor, focus == null ? anchor : focus);
		}
	});
}

function select(editor: LexicalEditor, anchor: number, focus = anchor): void {
	update(editor, () => {
		$selectComposerRange(anchor, focus);
	});
}

function type(editor: LexicalEditor, text: string): void {
	update(editor, () => {
		const selection = $getSelection();
		if ($isRangeSelection(selection)) {
			selection.insertText(text);
		}
	});
}

function run<T>(editor: LexicalEditor, command: LexicalCommand<T>, payload: T): boolean {
	let handled = false;
	update(editor, () => {
		handled = editor.dispatchCommand(command, payload);
	});
	return handled;
}

function historyStep(editor: LexicalEditor, command: typeof UNDO_COMMAND | typeof REDO_COMMAND): void {
	editor.dispatchCommand(command, undefined);
	update(editor, () => {});
}

interface Snapshot {
	wire: string;
	selection: {anchor: number; focus: number} | null;
	lines: Array<BlockquoteLine>;
	wrappers: number;
	markers: Array<string>;
	breaksInsideWrappers: number;
	hasSelection: boolean;
}

function snapshot(editor: LexicalEditor): Snapshot {
	return editor.getEditorState().read(
		() => {
			const state = $getComposerBlockquoteState();
			const paragraph = $getRoot().getFirstChild();
			const children = $isElementNode(paragraph) ? paragraph.getChildren() : [];
			const wrappers = children.filter($isComposerBlockquoteLineNode);
			return {
				wire: $projectComposer().wire,
				selection: state.selection,
				lines: state.lines,
				wrappers: wrappers.length,
				markers: wrappers.map((wrapper) => {
					const first = wrapper.getFirstChild();
					return $isComposerBlockquoteMarkerNode(first) ? first.getTextContent() : '<none>';
				}),
				breaksInsideWrappers: wrappers.reduce(
					(count, wrapper) => count + wrapper.getChildren().filter($isLineBreakNode).length,
					0,
				),
				hasSelection: $getSelection() !== null,
			};
		},
		{editor},
	);
}

function expectedQuoteLines(text: string): number {
	return text.split('\n').filter((line) => /^[ \t]*> /.test(line)).length;
}

function caretInsideMarker(state: Snapshot): boolean {
	const caret = state.selection == null ? null : state.selection.anchor;
	return caret != null && state.lines.some((line) => line.start <= caret && caret < line.contentStart);
}

function keyEvent(overrides: Record<string, unknown> = {}): KeyboardEvent & {preventDefault: ReturnType<typeof vi.fn>} {
	return {
		key: 'ArrowLeft',
		keyCode: 37,
		shiftKey: false,
		altKey: false,
		ctrlKey: false,
		metaKey: false,
		isComposing: false,
		preventDefault: vi.fn(),
		...overrides,
	} as unknown as KeyboardEvent & {preventDefault: ReturnType<typeof vi.fn>};
}

function compositionKey(editor: LexicalEditor): string | null {
	return (editor as unknown as {_compositionKey: string | null})._compositionKey;
}

function selectFirstMarkerText(editor: LexicalEditor, offset: number): string {
	let key = '';
	update(editor, () => {
		const paragraph = $getRoot().getFirstChild();
		const wrapper = $isElementNode(paragraph) ? paragraph.getFirstChild() : null;
		const marker = $isComposerBlockquoteLineNode(wrapper) ? wrapper.getFirstChild() : null;
		if (!$isComposerBlockquoteMarkerNode(marker)) {
			throw new Error('expected a quote marker');
		}
		marker.select(offset, offset);
		key = marker.getKey();
	});
	return key;
}

describe('blockquote line splits', () => {
	it('moves the whole content down when the caret is at content start', () => {
		const editor = createHarness();
		setComposer(editor, '> test', 2);
		expect(run(editor, INSERT_LINE_BREAK_COMMAND, false)).toBe(true);
		expect(snapshot(editor)).toMatchObject({
			wire: '> \n> test',
			selection: {anchor: 5, focus: 5},
			wrappers: 2,
			breaksInsideWrappers: 0,
		});
	});

	it('moves the tail to the new quote line when the caret is mid line', () => {
		const editor = createHarness();
		setComposer(editor, '> test', 4);
		expect(run(editor, INSERT_LINE_BREAK_COMMAND, false)).toBe(true);
		expect(snapshot(editor)).toMatchObject({
			wire: '> te\n> st',
			selection: {anchor: 7, focus: 7},
			wrappers: 2,
			breaksInsideWrappers: 0,
		});
	});

	it('keeps text order when insertLineBreak runs over a range inside a quote line', () => {
		const editor = createHarness();
		setComposer(editor, '> abcd', 3, 5);
		update(editor, () => {
			const selection = $getSelection();
			if ($isRangeSelection(selection)) {
				selection.insertLineBreak();
			}
		});
		expect(snapshot(editor)).toMatchObject({wire: '> a\nd', wrappers: 1, breaksInsideWrappers: 0});
	});

	it('declines a range selection and falls back to a plain break in order', () => {
		const editor = createHarness();
		setComposer(editor, '> abcd', 3, 5);
		run(editor, INSERT_LINE_BREAK_COMMAND, false);
		expect(snapshot(editor)).toMatchObject({wire: '> a\nd', wrappers: 1, breaksInsideWrappers: 0});
	});

	it('keeps text order when raw text with a newline is inserted mid quote line', () => {
		const editor = createHarness();
		setComposer(editor, '> abcd', 4);
		update(editor, () => {
			const selection = $getSelection();
			if ($isRangeSelection(selection)) {
				selection.insertRawText('x\ny');
			}
		});
		expect(snapshot(editor)).toMatchObject({
			wire: '> abx\nycd',
			wrappers: 1,
			breaksInsideWrappers: 0,
			selection: {anchor: 7, focus: 7},
		});
	});

	it('keeps text order when a clipboard slice with a newline lands mid quote line', () => {
		const editor = createHarness();
		setComposer(editor, '> abcd', 4);
		update(editor, () => {
			$insertComposerClipboardSlice({display: 'x\ny', segments: []}, false);
		});
		expect(snapshot(editor)).toMatchObject({
			wire: '> abx\nycd',
			wrappers: 1,
			breaksInsideWrappers: 0,
			selection: {anchor: 7, focus: 7},
		});
	});

	it('leaves the tail on the first line when insertNewAfter returns null', () => {
		const spy = vi
			.spyOn(ComposerBlockquoteLineNode.prototype, 'insertNewAfter')
			.mockImplementation(() => null as unknown as ComposerBlockquoteLineNode);
		try {
			const editor = createHarness();
			setComposer(editor, '> test', 4);
			run(editor, INSERT_LINE_BREAK_COMMAND, false);
			expect(snapshot(editor).wire).toBe('> test\n> ');
		} finally {
			spy.mockRestore();
		}
	});
});

describe('blockquote line breaks', () => {
	const LINE_BREAK_ROWS: Array<[string, number, string, number]> = [
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

	it.each(LINE_BREAK_ROWS)('on %j at %i gives %j with the caret at %i', (text, before, expected, after) => {
		const editor = createHarness();
		setComposer(editor, text, before);
		expect(run(editor, INSERT_LINE_BREAK_COMMAND, false)).toBe(true);
		const state = snapshot(editor);
		expect(state).toMatchObject({
			wire: expected,
			selection: {anchor: after, focus: after},
			wrappers: expectedQuoteLines(expected),
			breaksInsideWrappers: 0,
		});
		expect(caretInsideMarker(state)).toBe(false);
	});

	it('treats INSERT_PARAGRAPH_COMMAND like a line break', () => {
		const editor = createHarness();
		setComposer(editor, '> test', 6);
		expect(run(editor, INSERT_PARAGRAPH_COMMAND, undefined)).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire: '> test\n> ', selection: {anchor: 9, focus: 9}});
	});

	it('removes the whole quote when an empty quote is left twice', () => {
		const editor = createHarness();
		setComposer(editor, '', 0);
		type(editor, '>');
		type(editor, ' ');
		expect(snapshot(editor)).toMatchObject({
			wire: '> ',
			selection: {anchor: 2, focus: 2},
			wrappers: 1,
			markers: ['> '],
		});
		run(editor, INSERT_LINE_BREAK_COMMAND, false);
		expect(snapshot(editor)).toMatchObject({wire: '> \n> ', selection: {anchor: 5, focus: 5}, wrappers: 2});
		run(editor, INSERT_LINE_BREAK_COMMAND, false);
		expect(snapshot(editor)).toMatchObject({wire: '', selection: {anchor: 0, focus: 0}, wrappers: 0});
	});

	it('leaves a plain line below the quote when the empty continuation is left', () => {
		const editor = createHarness();
		setComposer(editor, '', 0);
		for (const character of '> test') {
			type(editor, character);
		}
		expect(snapshot(editor)).toMatchObject({wire: '> test', selection: {anchor: 6, focus: 6}, wrappers: 1});
		run(editor, INSERT_LINE_BREAK_COMMAND, false);
		expect(snapshot(editor)).toMatchObject({wire: '> test\n> ', selection: {anchor: 9, focus: 9}, wrappers: 2});
		run(editor, INSERT_LINE_BREAK_COMMAND, false);
		expect(snapshot(editor)).toMatchObject({wire: '> test\n', selection: {anchor: 7, focus: 7}, wrappers: 1});
		type(editor, 'x');
		expect(snapshot(editor)).toMatchObject({wire: '> test\nx', wrappers: 1});
	});

	it('declines outside quote lines, with selectStart, and while composing', () => {
		const editor = createHarness();
		setComposer(editor, 'abc', 3);
		run(editor, INSERT_LINE_BREAK_COMMAND, false);
		expect(snapshot(editor).wire).toBe('abc\n');
		setComposer(editor, '> test', 6);
		run(editor, INSERT_LINE_BREAK_COMMAND, true);
		expect(snapshot(editor).wire).toBe('> test\n');
		setComposer(editor, '> test', 6);
		update(editor, () => {
			const selection = $getSelection();
			if ($isRangeSelection(selection)) {
				$setCompositionKey(selection.anchor.key);
			}
		});
		run(editor, INSERT_LINE_BREAK_COMMAND, false);
		expect(snapshot(editor).wire).toBe('> test\n');
		update(editor, () => {
			$setCompositionKey(null);
		});
	});
});

describe('blockquote backspace', () => {
	const BACKSPACE_ROWS: Array<[string, number, string, number]> = [
		['> test', 2, 'test', 0],
		['> ', 2, '', 0],
		['> \n> ', 5, '> \n', 3],
		['> \n', 3, '', 0],
		['> \nabc', 3, 'abc', 0],
		['a\n> \nb', 5, 'a\nb', 2],
		['  > a', 4, 'a', 0],
		['x\n> test', 4, 'x\ntest', 2],
	];

	it.each(BACKSPACE_ROWS)('on %j at %i gives %j with the caret at %i', (text, before, expected, after) => {
		const editor = createHarness();
		setComposer(editor, text, before);
		expect(run(editor, DELETE_CHARACTER_COMMAND, true)).toBe(true);
		expect(snapshot(editor)).toMatchObject({
			wire: expected,
			selection: {anchor: after, focus: after},
			wrappers: expectedQuoteLines(expected),
		});
	});

	it('removes two empty quote lines with two presses', () => {
		const editor = createHarness();
		setComposer(editor, '> \n> ', 5);
		run(editor, DELETE_CHARACTER_COMMAND, true);
		expect(snapshot(editor)).toMatchObject({wire: '> \n', selection: {anchor: 3, focus: 3}, wrappers: 1});
		run(editor, DELETE_CHARACTER_COMMAND, true);
		expect(snapshot(editor)).toMatchObject({wire: '', selection: {anchor: 0, focus: 0}, wrappers: 0});
	});

	const BACKSPACE_DECLINED: Array<[string, number, number?]> = [
		['> a\nb', 4],
		['> test', 4],
		['>  ', 3],
		['abc', 3],
		['```\n> a', 6],
		['> test', 2, 4],
	];

	it.each(BACKSPACE_DECLINED)('declines on %j over %i..%s', (text, anchor, focus) => {
		const editor = createHarness();
		setComposer(editor, text, anchor, focus);
		expect(run(editor, DELETE_CHARACTER_COMMAND, true)).toBe(false);
	});

	it('declines a forward delete and a backspace while composing', () => {
		const editor = createHarness();
		setComposer(editor, '> test', 2);
		expect(run(editor, DELETE_CHARACTER_COMMAND, false)).toBe(false);
		update(editor, () => {
			const selection = $getSelection();
			if ($isRangeSelection(selection)) {
				$setCompositionKey(selection.anchor.key);
			}
		});
		expect(run(editor, DELETE_CHARACTER_COMMAND, true)).toBe(false);
		update(editor, () => {
			$setCompositionKey(null);
		});
	});

	it.each(['before', 'after'])('wins over a high priority delete handler registered %s it', (order) => {
		const editor = createBareEditor();
		const spy = vi.fn(() => true);
		const registerCompetitors = () => {
			disposers.push(
				registerComposerSoftWrapDeletion(editor),
				editor.registerCommand(DELETE_CHARACTER_COMMAND, spy, COMMAND_PRIORITY_HIGH),
			);
		};
		if (order === 'before') {
			registerCompetitors();
		}
		disposers.push(registerComposerMarkdownHighlight(editor), registerComposerBlockquote(editor));
		if (order === 'after') {
			registerCompetitors();
		}
		setComposer(editor, '> test', 2);
		expect(run(editor, DELETE_CHARACTER_COMMAND, true)).toBe(true);
		expect(spy).not.toHaveBeenCalled();
		expect(snapshot(editor).wire).toBe('test');
	});
});

describe('blockquote history', () => {
	it('treats one line break exit and one backspace as single steps', () => {
		const editor = createHarness({history: true});
		setComposer(editor, '> test\n> ', 9);
		run(editor, INSERT_LINE_BREAK_COMMAND, false);
		expect(snapshot(editor)).toMatchObject({wire: '> test\n', selection: {anchor: 7, focus: 7}});
		historyStep(editor, UNDO_COMMAND);
		expect(snapshot(editor)).toMatchObject({wire: '> test\n> ', selection: {anchor: 9, focus: 9}, wrappers: 2});
		historyStep(editor, REDO_COMMAND);
		expect(snapshot(editor)).toMatchObject({wire: '> test\n', selection: {anchor: 7, focus: 7}, wrappers: 1});
		select(editor, 2);
		run(editor, DELETE_CHARACTER_COMMAND, true);
		expect(snapshot(editor).wire).toBe('test\n');
		historyStep(editor, UNDO_COMMAND);
		expect(snapshot(editor)).toMatchObject({wire: '> test\n', wrappers: 1});
		historyStep(editor, REDO_COMMAND);
		expect(snapshot(editor)).toMatchObject({wire: 'test\n', wrappers: 0});
	});
});

describe('blockquote caret, typing and composition', () => {
	it.each([
		['> test', 0, 2],
		['> test', 1, 2],
		['ab\n> cd', 3, 5],
		['ab\n> cd', 4, 5],
		['> test', 3, 3],
	])('moves the caret on %j from %i to %i on a selection change', (text, before, after) => {
		const editor = createHarness();
		setComposer(editor, text);
		select(editor, before);
		expect(run(editor, SELECTION_CHANGE_COMMAND, undefined)).toBe(false);
		expect(snapshot(editor).selection).toEqual({anchor: after, focus: after});
	});

	it('leaves range selections alone', () => {
		const editor = createHarness();
		setComposer(editor, '> test');
		select(editor, 0, 4);
		run(editor, SELECTION_CHANGE_COMMAND, undefined);
		expect(snapshot(editor).selection).toEqual({anchor: 0, focus: 4});
	});

	it('snaps a text point inside the marker to content start', () => {
		const editor = createHarness();
		setComposer(editor, '> ');
		selectFirstMarkerText(editor, 1);
		run(editor, SELECTION_CHANGE_COMMAND, undefined);
		expect(snapshot(editor).selection).toEqual({anchor: 2, focus: 2});
	});

	it('moves the caret out of a marker the highlight pass has just created', () => {
		const editor = createHarness();
		setComposer(editor, 'x> test', 1);
		update(editor, () => {
			$selectComposerRange(0, 1);
			const selection = $getSelection();
			if ($isRangeSelection(selection)) {
				selection.insertText('');
			}
		});
		expect(snapshot(editor)).toMatchObject({wire: '> test', selection: {anchor: 2, focus: 2}, wrappers: 1});
	});

	it('creates a sibling content node when typing at the end of the marker text', () => {
		const editor = createHarness();
		setComposer(editor, '> ');
		selectFirstMarkerText(editor, 2);
		type(editor, 'x');
		expect(snapshot(editor)).toMatchObject({
			wire: '> x',
			wrappers: 1,
			markers: ['> '],
			selection: {anchor: 3, focus: 3},
		});
	});

	it('creates a content node when typing at the element point after the marker', () => {
		const editor = createHarness();
		setComposer(editor, '> ', 2);
		type(editor, 'x');
		expect(snapshot(editor)).toMatchObject({wire: '> x', wrappers: 1, markers: ['> ']});
	});

	it('refuses text inserted before or after the marker', () => {
		const editor = createHarness();
		setComposer(editor, '> ');
		const guards = editor.getEditorState().read(
			() => {
				const paragraph = $getRoot().getFirstChild();
				const wrapper = $isElementNode(paragraph) ? paragraph.getFirstChild() : null;
				const marker = $isComposerBlockquoteLineNode(wrapper) ? wrapper.getFirstChild() : null;
				if (!$isComposerBlockquoteMarkerNode(marker)) {
					throw new Error('expected a quote marker');
				}
				return {before: marker.canInsertTextBefore(), after: marker.canInsertTextAfter()};
			},
			{editor},
		);
		expect(guards).toEqual({before: false, after: false});
	});

	it('diverts a composition that starts at the end of the marker text', () => {
		const editor = createHarness();
		setComposer(editor, '> ');
		const markerKey = selectFirstMarkerText(editor, 2);
		run(editor, COMPOSITION_START_COMMAND, COMPOSITION_EVENT);
		const key = compositionKey(editor);
		expect(key).not.toBeNull();
		expect(key).not.toBe(markerKey);
		update(editor, () => {
			const node = $getNodeByKey(key!);
			if ($isTextNode(node)) {
				node.setTextContent('あ');
				node.select(1, 1);
			}
		});
		expect(editor.isComposing()).toBe(true);
		expect(snapshot(editor).markers).toEqual(['> ']);
		expect(run(editor, DELETE_CHARACTER_COMMAND, true)).toBe(false);
		update(editor, () => {
			$setCompositionKey(null);
			for (const node of $getRoot().getAllTextNodes()) {
				node.markDirty();
			}
		});
		expect(snapshot(editor)).toMatchObject({wire: '> あ', wrappers: 1, markers: ['> ']});
	});

	it('diverts a composition that starts at the element point after the marker', () => {
		const editor = createHarness();
		setComposer(editor, '> ', 2);
		run(editor, COMPOSITION_START_COMMAND, COMPOSITION_EVENT);
		const key = compositionKey(editor);
		const target = editor.getEditorState().read(() => $getNodeByKey(key!), {editor});
		expect(target).not.toBeNull();
		expect($isComposerBlockquoteMarkerNode(target)).toBe(false);
		update(editor, () => {
			$setCompositionKey(null);
		});
	});

	it('lets a plain syntax marker take both the typed text and the composition', () => {
		const editor = createBareEditor();
		registerPlainTextFallbacks(editor);
		let markerKey = '';
		update(editor, () => {
			const paragraph = $createParagraphNode();
			const marker = $createSyntaxMarkerNode('> ');
			paragraph.append(marker);
			$getRoot().clear().append(paragraph);
			marker.select(2, 2);
			markerKey = marker.getKey();
		});
		run(editor, COMPOSITION_START_COMMAND, COMPOSITION_EVENT);
		expect(compositionKey(editor)).toBe(markerKey);
		update(editor, () => {
			$setCompositionKey(null);
			const marker = $getNodeByKey(markerKey);
			if ($isTextNode(marker)) {
				marker.select(2, 2);
			}
		});
		type(editor, 'x');
		expect(editor.getEditorState().read(() => $getNodeByKey(markerKey)?.getTextContent(), {editor})).toBe('> x');
	});

	it('jumps over the hidden marker on ArrowLeft', () => {
		const editor = createHarness();
		setComposer(editor, 'ab\n> cd', 5);
		const event = keyEvent();
		expect(run(editor, KEY_ARROW_LEFT_COMMAND, event)).toBe(true);
		expect(event.preventDefault).toHaveBeenCalled();
		expect(snapshot(editor).selection).toEqual({anchor: 2, focus: 2});
		setComposer(editor, '> test', 2);
		expect(run(editor, KEY_ARROW_LEFT_COMMAND, keyEvent())).toBe(true);
		expect(snapshot(editor).selection).toEqual({anchor: 2, focus: 2});
		setComposer(editor, 'ab\n> cd', 5);
		expect(run(editor, KEY_ARROW_LEFT_COMMAND, keyEvent({altKey: true}))).toBe(true);
	});

	it('declines ArrowLeft with shift, meta, composition and away from content start', () => {
		const editor = createHarness();
		setComposer(editor, 'ab\n> cd', 5);
		expect(run(editor, KEY_ARROW_LEFT_COMMAND, keyEvent({shiftKey: true}))).toBe(false);
		expect(run(editor, KEY_ARROW_LEFT_COMMAND, keyEvent({metaKey: true}))).toBe(false);
		expect(run(editor, KEY_ARROW_LEFT_COMMAND, keyEvent({isComposing: true}))).toBe(false);
		setComposer(editor, '> test', 4);
		expect(run(editor, KEY_ARROW_LEFT_COMMAND, keyEvent())).toBe(false);
	});
});

describe('multi-line blockquote markers', () => {
	it('rewrites a typed ">>> " to "> " and restores it on undo', () => {
		const editor = createHarness({history: true});
		setComposer(editor, '>>>', 3);
		type(editor, ' ');
		expect(snapshot(editor)).toMatchObject({wire: '> ', selection: {anchor: 2, focus: 2}, wrappers: 1});
		historyStep(editor, UNDO_COMMAND);
		expect(snapshot(editor)).toMatchObject({wire: '>>>', selection: {anchor: 3, focus: 3}, wrappers: 0});
		historyStep(editor, REDO_COMMAND);
		expect(snapshot(editor)).toMatchObject({wire: '> ', selection: {anchor: 2, focus: 2}, wrappers: 1});
	});

	it('rewrites when the completing character is a ">"', () => {
		const editor = createHarness();
		setComposer(editor, '>> a', 1);
		type(editor, '>');
		expect(snapshot(editor)).toMatchObject({wire: '> a', selection: {anchor: 2, focus: 2}, wrappers: 1});
	});

	it('rewrites only the marker that was typed on a later line', () => {
		const editor = createHarness();
		setComposer(editor, 'a\n>>>\nb', 5);
		type(editor, ' ');
		expect(snapshot(editor)).toMatchObject({wire: 'a\n> \nb', selection: {anchor: 4, focus: 4}, wrappers: 1});
	});

	it('keeps the rendered meaning of arrived text and creates no selection', () => {
		const editor = createHarness();
		update(editor, () => {
			$hydrateComposerFromDraft('>>> a\n\nb', []);
		});
		expect(snapshot(editor)).toMatchObject({wire: '> a\n> \n> b', wrappers: 3, hasSelection: false});
	});

	it('keeps a caret placed at the end after hydration', () => {
		const editor = createHarness();
		setComposer(editor, '>>> a\nb', 7);
		expect(snapshot(editor)).toMatchObject({wire: '> a\n> b', selection: {anchor: 7, focus: 7}, wrappers: 2});
	});

	it('keeps the rendered meaning of pasted text', () => {
		const editor = createHarness();
		setComposer(editor, '', 0);
		update(editor, () => {
			$insertComposerClipboardSlice({display: '>>> a\nb', segments: []}, false);
		});
		expect(snapshot(editor)).toMatchObject({wire: '> a\n> b', selection: {anchor: 7, focus: 7}, wrappers: 2});
	});

	it('keeps the indent of an indented marker', () => {
		const editor = createHarness();
		update(editor, () => {
			$hydrateComposerFromDraft('  >>> a', []);
		});
		expect(snapshot(editor)).toMatchObject({wire: '  > a', wrappers: 1, markers: ['  > '], hasSelection: false});
	});

	it('leaves an arrived alert marker alone so the stored text keeps its meaning', () => {
		const editor = createHarness();
		update(editor, () => {
			$hydrateComposerFromDraft('>>> [!NOTE]\nbody', []);
		});
		expect(snapshot(editor)).toMatchObject({wire: '>>> [!NOTE]\nbody', wrappers: 0});
	});

	it('leaves ">>> a" as text without ALLOW_MULTILINE_BLOCKQUOTES', () => {
		const editor = createHarness({flags: NO_MULTILINE_FLAGS});
		update(editor, () => {
			$hydrateComposerFromDraft('>>> a', []);
		});
		expect(snapshot(editor)).toMatchObject({wire: '>>> a', wrappers: 0});
	});
});

describe('blockquote transform convergence', () => {
	it.each([
		'> > a',
		'> [!NOTE]\n> body',
		'> >>> a',
		'> a\n> \n> b',
		'```\n> a',
		'>  ',
		'> a\n\nb',
	])('reaches a fixed point on %j', (text) => {
		const editor = createHarness();
		setComposer(editor, text);
		const before = JSON.stringify(editor.getEditorState().toJSON());
		const wire = snapshot(editor).wire;
		const updates: Array<{intentionalElements: number; leaves: number}> = [];
		disposers.push(
			editor.registerUpdateListener(({dirtyElements, dirtyLeaves}) => {
				updates.push({
					intentionalElements: [...dirtyElements].filter(([key, intentional]) => intentional && key !== 'root').length,
					leaves: dirtyLeaves.size,
				});
			}),
		);
		update(editor, () => {});
		expect(updates.every((entry) => entry.intentionalElements === 0 && entry.leaves === 0)).toBe(true);
		let marked = 0;
		update(editor, () => {
			for (const node of $getRoot().getAllTextNodes()) {
				node.markDirty();
				marked += 1;
			}
		});
		expect(JSON.stringify(editor.getEditorState().toJSON())).toBe(before);
		expect(updates[updates.length - 1]).toEqual({intentionalElements: 0, leaves: marked});
		expect(snapshot(editor).wire).toBe(wire);
	});

	it('keeps nested and alert quotes in a single wrapper per line', () => {
		const editor = createHarness();
		setComposer(editor, '> > a');
		expect(snapshot(editor)).toMatchObject({wire: '> > a', wrappers: 1, markers: ['> ']});
		setComposer(editor, '> [!NOTE]\n> body');
		expect(snapshot(editor)).toMatchObject({wrappers: 2, markers: ['> ', '> ']});
		setComposer(editor, '> >>> a');
		expect(snapshot(editor)).toMatchObject({wire: '> >>> a', wrappers: 1, markers: ['> ']});
	});

	it('keeps whitespace typed after the prefix as content of the same quote line', () => {
		const editor = createHarness();
		setComposer(editor, '> ', 2);
		type(editor, ' ');
		expect(snapshot(editor)).toMatchObject({
			wire: '>  ',
			markers: ['> '],
			lines: [{start: 0, contentStart: 2, end: 3}],
			selection: {anchor: 3, focus: 3},
		});
	});
});

describe('blockquote code blocks', () => {
	it('gives no wrapper to lines inside closed and unclosed fences', () => {
		const editor = createHarness();
		setComposer(editor, '```\n> a', 7);
		expect(snapshot(editor).wrappers).toBe(0);
		run(editor, INSERT_LINE_BREAK_COMMAND, false);
		expect(snapshot(editor).wire).toBe('```\n> a\n');
		setComposer(editor, '```\n> a\n```');
		expect(snapshot(editor).wrappers).toBe(0);
	});

	it('keeps a quote line that follows a closed fence', () => {
		const editor = createHarness();
		setComposer(editor, '```\nx\n```\n> a');
		expect(snapshot(editor)).toMatchObject({wrappers: 1, lines: [{start: 10, contentStart: 12, end: 13}]});
	});

	it('does not continue the quote from a fence opened on a quote line', () => {
		const editor = createHarness();
		setComposer(editor, '> ```', 5);
		expect(snapshot(editor).wrappers).toBe(1);
		run(editor, INSERT_LINE_BREAK_COMMAND, false);
		expect(snapshot(editor).wire).toBe('> ```\n');
	});

	it('keeps the wrapper only on the opening line of a fence closed inside a quote', () => {
		const editor = createHarness();
		setComposer(editor, '> ```\n> code\n> ```');
		expect(snapshot(editor)).toMatchObject({wrappers: 1, lines: [{start: 0, contentStart: 2, end: 5}]});
	});
});

describe('blockquote parser flags', () => {
	it('gives no wrappers and declines every handler without ALLOW_BLOCKQUOTES', () => {
		const editor = createHarness({flags: NO_BLOCKQUOTE_FLAGS});
		setComposer(editor, '> a', 2);
		expect(snapshot(editor).wrappers).toBe(0);
		expect(run(editor, DELETE_CHARACTER_COMMAND, true)).toBe(false);
		expect(run(editor, KEY_ARROW_LEFT_COMMAND, keyEvent())).toBe(false);
		select(editor, 0);
		run(editor, SELECTION_CHANGE_COMMAND, undefined);
		expect(snapshot(editor).selection).toEqual({anchor: 0, focus: 0});
		select(editor, 3);
		run(editor, INSERT_LINE_BREAK_COMMAND, false);
		expect(snapshot(editor).wire).toBe('> a\n');
	});

	it('continues quotes under bio style flags', () => {
		const editor = createHarness({flags: BIO_LIKE_FLAGS});
		setComposer(editor, '> a', 3);
		expect(run(editor, INSERT_LINE_BREAK_COMMAND, false)).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire: '> a\n> ', selection: {anchor: 6, focus: 6}});
	});
});

describe('nodes inside quote lines', () => {
	it('converts emoji shortcodes inside a quote line', () => {
		const editor = createHarness();
		disposers.push(
			registerComposerEmojiShortcode(editor, (name) =>
				name === 'blob' ? {kind: 'custom', emojiId: '1', animated: false, display: ':blob:', wire: '<:blob:1>'} : null,
			),
		);
		setComposer(editor, '> hi :blob:', 11);
		expect(snapshot(editor)).toMatchObject({wire: '> hi <:blob:1>', wrappers: 1});
		const emojiInWrapper = editor.getEditorState().read(
			() => {
				const paragraph = $getRoot().getFirstChild();
				const wrapper = $isElementNode(paragraph) ? paragraph.getFirstChild() : null;
				return $isComposerBlockquoteLineNode(wrapper) && wrapper.getChildren().some($isComposerCustomEmojiNode);
			},
			{editor},
		);
		expect(emojiInWrapper).toBe(true);
	});

	it('continues the quote when a mention display name holds a fence', () => {
		const editor = createHarness();
		const segment: MentionSegment = {
			type: 'user',
			id: '1',
			displayText: '@a```b',
			actualText: '<@1>',
			start: 2,
			end: 8,
		};
		setComposer(editor, '> @a```b', 8, 8, [segment]);
		expect(run(editor, INSERT_LINE_BREAK_COMMAND, false)).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire: '> <@1>\n> ', selection: {anchor: 11, focus: 11}, wrappers: 2});
	});

	it('removes the marker below a mention display name holding a fence', () => {
		const editor = createHarness();
		const segment: MentionSegment = {
			type: 'user',
			id: '1',
			displayText: '@a```b',
			actualText: '<@1>',
			start: 2,
			end: 8,
		};
		setComposer(editor, '> @a```b\n> q', 11, 11, [segment]);
		expect(run(editor, DELETE_CHARACTER_COMMAND, true)).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire: '> <@1>\nq', selection: {anchor: 9, focus: 9}, wrappers: 1});
	});

	it('projects a mention inside a quote line and survives a round trip', () => {
		const editor = createHarness();
		const segment: MentionSegment = {
			type: 'user',
			id: '1',
			displayText: '@name',
			actualText: '<@1>',
			start: 2,
			end: 7,
		};
		setComposer(editor, '> @name', 7, 7, [segment]);
		const projection = editor.getEditorState().read(() => $projectComposer(), {editor});
		expect(projection).toEqual({display: '> @name', wire: '> <@1>', segments: [segment]});
		const mentionInWrapper = editor.getEditorState().read(
			() => {
				const paragraph = $getRoot().getFirstChild();
				const wrapper = $isElementNode(paragraph) ? paragraph.getFirstChild() : null;
				return $isComposerBlockquoteLineNode(wrapper) && wrapper.getChildren().some($isComposerMentionNode);
			},
			{editor},
		);
		expect(mentionInWrapper).toBe(true);
		setComposer(editor, projection.display, undefined, undefined, projection.segments);
		expect(editor.getEditorState().read(() => $projectComposer(), {editor})).toEqual(projection);
		expect(snapshot(editor).wrappers).toBe(1);
	});
});
