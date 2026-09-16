// SPDX-License-Identifier: AGPL-3.0-or-later

import type {BlockquoteLine} from '@app/features/lexical/composer/blockquoteLines';
import {registerComposerBlockquote} from '@app/features/lexical/composer/ComposerBlockquote';
import {
	$getComposerClipboardSelection,
	$insertComposerClipboardSlice,
	$insertComposerPastedText,
	COMPOSER_CLIPBOARD_MAX_TRUSTED_PAYLOADS,
	FLUXER_COMPOSER_CLIPBOARD_MIME,
	parseComposerClipboardSlice,
	registerComposerClipboardCommands,
} from '@app/features/lexical/composer/ComposerClipboard';
import {registerComposerCodeIndent} from '@app/features/lexical/composer/ComposerCodeIndent';
import {registerComposerEmojiShortcode} from '@app/features/lexical/composer/ComposerEmojiShortcode';
import type {ComposerHandle} from '@app/features/lexical/composer/ComposerHandle';
import {registerComposerMarkdownHighlight} from '@app/features/lexical/composer/ComposerMarkdownHighlight';
import {registerComposerMarkdownShortcuts} from '@app/features/lexical/composer/ComposerMarkdownShortcuts';
import {$hydrateComposerFromDraft, $projectComposer} from '@app/features/lexical/composer/ComposerSerialization';
import {registerComposerSoftWrapDeletion} from '@app/features/lexical/composer/ComposerSoftWrapDeletion';
import {
	$getComposerBlockquoteState,
	$queryComposerSelectionWrappers,
	$selectComposerRange,
	$wrapComposerSelection,
} from '@app/features/lexical/composer/composerOffsets';
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
import {
	$isComposerStandardEmojiNode,
	ComposerStandardEmojiNode,
} from '@app/features/lexical/composer/nodes/ComposerStandardEmojiNode';
import {SlashOptionalHintNode} from '@app/features/lexical/composer/nodes/SlashOptionalHintNode';
import {SlashSeparatorNode} from '@app/features/lexical/composer/nodes/SlashSeparatorNode';
import {SlashSlotNode} from '@app/features/lexical/composer/nodes/SlashSlotNode';
import {SlashSlotPlaceholderNode} from '@app/features/lexical/composer/nodes/SlashSlotPlaceholderNode';
import {$createSyntaxMarkerNode, SyntaxMarkerNode} from '@app/features/lexical/composer/nodes/SyntaxMarkerNode';
import {ParserFlags} from '@app/features/messaging/utils/markdown/parser/Enums';
import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';
import type {ResolvedTypedEmoji} from '@app/features/messaging/utils/TypedEmojiShortcodeUtils';
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
	COPY_COMMAND,
	createEditor,
	DELETE_CHARACTER_COMMAND,
	DELETE_LINE_COMMAND,
	DELETE_WORD_COMMAND,
	DRAGSTART_COMMAND,
	DROP_COMMAND,
	HISTORY_MERGE_TAG,
	INSERT_LINE_BREAK_COMMAND,
	INSERT_PARAGRAPH_COMMAND,
	KEY_ARROW_LEFT_COMMAND,
	KEY_ARROW_RIGHT_COMMAND,
	KEY_DOWN_COMMAND,
	KEY_TAB_COMMAND,
	type LexicalCommand,
	type LexicalEditor,
	PASTE_COMMAND,
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

const platform = vi.hoisted(() => ({apple: false}));
const dropOffset = vi.hoisted(() => ({value: null as number | null}));

vi.mock('lexical', async (importOriginal) => {
	const actual = await importOriginal<typeof import('lexical')>();
	return {
		...actual,
		get IS_APPLE() {
			return platform.apple;
		},
	};
});

vi.mock('@app/features/lexical/composer/composerOffsets', async (importOriginal) => {
	const actual = await importOriginal<typeof import('@app/features/lexical/composer/composerOffsets')>();
	return {...actual, $getComposerDropOffset: () => dropOffset.value};
});

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

const GRINNING: ResolvedTypedEmoji = {
	kind: 'standard',
	name: 'grinning',
	surrogate: '😀',
	url: null,
	display: ':grinning:',
};

function parserFlagsHandle(flags: number): ComposerHandle {
	return {getMarkdownParserFlags: () => flags} as unknown as ComposerHandle;
}

const RTL_ELEMENT = {
	ownerDocument: {defaultView: {getComputedStyle: () => ({direction: 'rtl'})}},
} as unknown as HTMLElement;

const disposers: Array<() => void> = [];

afterEach(() => {
	while (disposers.length > 0) {
		disposers.pop()!();
	}
	platform.apple = false;
	dropOffset.value = null;
	vi.unstubAllGlobals();
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

function createHarness(options: {flags?: number; history?: boolean; maxWireLength?: number} = {}): LexicalEditor {
	const editor = createBareEditor();
	disposers.push(
		registerComposerMarkdownHighlight(editor, options.flags, false, options.maxWireLength),
		registerComposerBlockquote(editor, options.flags),
	);
	registerPlainTextFallbacks(editor);
	if (options.history === true) {
		disposers.push(registerHistory(editor, createEmptyHistoryState(), 0));
	}
	return editor;
}

function createFormattingHarness(flags = DEFAULT_COMPOSER_MARKDOWN_FLAGS): LexicalEditor {
	const editor = createHarness({flags});
	disposers.push(
		registerComposerMarkdownShortcuts(editor),
		registerComposerCodeIndent(editor, {current: false}),
		registerComposerClipboardCommands(editor, {
			getPlainText: () => false,
			isEditable: () => true,
			getMarkdownParserFlags: () => flags,
		}),
	);
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

	it('continues the quote when a clipboard slice with a newline lands mid quote line', () => {
		const editor = createHarness();
		setComposer(editor, '> abcd', 4);
		update(editor, () => {
			$insertComposerClipboardSlice({display: 'x\ny', segments: []}, false);
		});
		expect(snapshot(editor)).toMatchObject({
			wire: '> abx\n> ycd',
			wrappers: 2,
			breaksInsideWrappers: 0,
			selection: {anchor: 9, focus: 9},
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

describe('blockquote paste', () => {
	function pasteEvent(text: string): {clipboardData: DataTransfer; preventDefault: ReturnType<typeof vi.fn>} {
		return {
			clipboardData: {getData: (type: string) => (type === 'text/plain' ? text : '')} as unknown as DataTransfer,
			preventDefault: vi.fn(),
		};
	}

	function paste(editor: LexicalEditor, text: string): {handled: boolean; prevented: boolean} {
		const event = pasteEvent(text);
		const handled = run(editor, PASTE_COMMAND, event as unknown as ClipboardEvent);
		return {handled, prevented: event.preventDefault.mock.calls.length > 0};
	}

	function createRawPasteHarness(): LexicalEditor {
		const editor = createHarness();
		disposers.push(
			editor.registerCommand(
				PASTE_COMMAND,
				(event) => {
					const selection = $getSelection();
					const data = (event as ClipboardEvent).clipboardData;
					if (!$isRangeSelection(selection) || data == null) {
						return false;
					}
					selection.insertRawText(data.getData('text/plain'));
					return true;
				},
				COMMAND_PRIORITY_EDITOR,
			),
		);
		return editor;
	}

	const PASTE_ROWS: Array<[string, number, number, string, string, number]> = [
		['> abcd', 4, 4, 'x\ny', '> abx\n> ycd', 9],
		['> abcd', 2, 2, 'x\ny', '> x\n> yabcd', 7],
		['> ', 2, 2, 'x\ny', '> x\n> y', 7],
		['  > a', 5, 5, 'x\ny', '  > ax\n> y', 10],
		['> abcd', 4, 4, 'x\n\ny', '> abx\n> \n> ycd', 12],
		['> abcd', 6, 6, '\n', '> abcd\n> ', 9],
		['> a\n> ', 6, 6, '\n', '> a\n> \n> ', 9],
		['> abcd', 6, 6, 'x\r\ny', '> abcdx\n> y', 11],
		['> ab\n> cd', 3, 8, 'x\ny', '> ax\n> yd', 8],
		['> ', 2, 2, '> x\n> y', '> x\n> y', 7],
		['> ', 2, 2, '> x', '> x', 3],
		['x\n> ', 4, 4, '  > a\r\n> b', 'x\n> a\n> b', 9],
		['> ab', 2, 2, '> x', '> xab', 3],
		['> ab', 2, 4, '> x\n> y', '> x\n> y', 7],
	];

	it.each(
		PASTE_ROWS,
	)('on %j at %i..%i pasting %j gives %j with the caret at %i', (text, anchor, focus, pasted, wire, caret) => {
		const editor = createHarness();
		setComposer(editor, text, anchor, focus);
		expect(paste(editor, pasted)).toEqual({handled: true, prevented: true});
		expect(snapshot(editor)).toMatchObject({
			wire,
			selection: {anchor: caret, focus: caret},
			wrappers: expectedQuoteLines(wire),
			breaksInsideWrappers: 0,
		});
	});

	const DECLINE_ROWS: Array<[string, number, number, string]> = [
		['abcd', 2, 2, 'x\ny'],
		['> abcd', 4, 4, 'xy'],
		['```\n> a', 7, 7, 'x\ny'],
		['> ab', 4, 4, 'x\n> y'],
		['> ```', 5, 5, 'x\ny'],
		['> ab', 4, 4, '> x'],
		['> ', 2, 2, 'x'],
	];

	it.each(DECLINE_ROWS)('declines %j at %i..%i pasting %j', (text, anchor, focus, pasted) => {
		const editor = createHarness();
		setComposer(editor, text, anchor, focus);
		expect(paste(editor, pasted)).toEqual({handled: false, prevented: false});
		expect(snapshot(editor).wire).toBe(text);
	});

	it('declines a paste inside a quote line without ALLOW_BLOCKQUOTES', () => {
		const editor = createHarness({flags: NO_BLOCKQUOTE_FLAGS});
		setComposer(editor, '> abcd', 4);
		expect(paste(editor, 'x\ny')).toEqual({handled: false, prevented: false});
		expect(snapshot(editor)).toMatchObject({wire: '> abcd', wrappers: 0});
	});

	it('plans a paste below a quoted fence with the flags the handler was registered with', () => {
		const editor = createHarness({flags: BIO_LIKE_FLAGS});
		setComposer(editor, '> ```', 5);
		expect(paste(editor, 'x\ny')).toEqual({handled: true, prevented: true});
		expect(snapshot(editor)).toMatchObject({
			wire: '> ```x\n> y',
			selection: {anchor: 10, focus: 10},
			wrappers: 2,
		});
	});

	const BARE_PREFIX_ROWS: Array<[string, number, number, string, string, number]> = [
		['> ', 2, 2, '> ', '> ', 2],
		['> ab', 2, 4, '> ', '> ', 2],
		['  > ab', 4, 6, '  > ', '  > ', 4],
		['> ab', 2, 4, '\t> ', '> ', 2],
	];

	it.each(
		BARE_PREFIX_ROWS,
	)('on %j at %i..%i pasting the bare prefix %j gives %j with the caret at %i', (text, anchor, focus, pasted, wire, caret) => {
		const editor = createHarness();
		setComposer(editor, text, anchor, focus);
		expect(paste(editor, pasted)).toEqual({handled: true, prevented: true});
		expect(snapshot(editor)).toMatchObject({
			wire,
			selection: {anchor: caret, focus: caret},
			wrappers: expectedQuoteLines(wire),
		});
	});

	it.each<[string, number, number, string, number]>([
		['> ', 2, 2, '> ', 2],
		['> ab', 2, 4, '> ', 2],
	])('inserts a bare prefix slice over %j at %i..%i, giving %j with the caret at %i', (text, anchor, focus, wire, caret) => {
		const editor = createHarness();
		setComposer(editor, text, anchor, focus);
		let inserted = false;
		update(editor, () => {
			inserted = $insertComposerClipboardSlice({display: '> ', segments: []}, false);
		});
		expect(inserted).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire, selection: {anchor: caret, focus: caret}, wrappers: 1});
	});

	it('removes the selected range when a dropped bare prefix plans to nothing', () => {
		const editor = createHarness();
		setComposer(editor, '> ab', 2, 4);
		const payload = {dataTransfer: {getData: () => '> '}} as unknown as InputEvent;
		expect(run(editor, CONTROLLED_TEXT_INSERTION_COMMAND, payload)).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire: '> ', selection: {anchor: 2, focus: 2}, wrappers: 1});
	});

	it('keeps every line quoted when multi-line text is inserted at the cursor inside a quote', () => {
		const editor = createHarness();
		setComposer(editor, '> abcd', 4);
		let inserted = false;
		update(editor, () => {
			inserted = $insertComposerClipboardSlice({display: 'x\ny\nz', segments: []}, false);
		});
		expect(inserted).toBe(true);
		expect(snapshot(editor)).toMatchObject({
			wire: '> abx\n> y\n> zcd',
			selection: {anchor: 13, focus: 13},
			wrappers: 3,
		});
	});

	it.each<[string, number, string, string, number]>([
		['> ab', 4, 'x\n> y', '> abx\n> y', 9],
		['> ```', 5, 'x\ny', '> ```x\ny', 8],
		['> ab', 4, '> x', '> ab> x', 7],
	])('leaves %j at %i pasting %j to lexical, giving %j and the caret at %i', (text, caret, pasted, wire, after) => {
		const editor = createRawPasteHarness();
		setComposer(editor, text, caret);
		expect(paste(editor, pasted)).toEqual({handled: true, prevented: false});
		expect(snapshot(editor)).toMatchObject({
			wire,
			selection: {anchor: after, focus: after},
			wrappers: expectedQuoteLines(wire),
		});
	});

	it('continues the quote when text is dropped on a quote line', () => {
		const editor = createHarness();
		setComposer(editor, '> abcd', 4);
		const payload = {dataTransfer: {getData: () => 'x\ny'}} as unknown as InputEvent;
		expect(run(editor, CONTROLLED_TEXT_INSERTION_COMMAND, payload)).toBe(true);
		expect(snapshot(editor)).toMatchObject({
			wire: '> abx\n> ycd',
			selection: {anchor: 9, focus: 9},
			wrappers: 2,
		});
	});

	it('merges a dropped quote line into an empty quote line', () => {
		const editor = createHarness();
		setComposer(editor, '> ', 2);
		const payload = {dataTransfer: {getData: () => '> x'}} as unknown as InputEvent;
		expect(run(editor, CONTROLLED_TEXT_INSERTION_COMMAND, payload)).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire: '> x', selection: {anchor: 3, focus: 3}, wrappers: 1});
	});

	it('drops the quote prefix of the first slice line at a quote content start and shifts its mention', () => {
		const editor = createHarness();
		setComposer(editor, '> ', 2);
		const segment: MentionSegment = {
			type: 'user',
			id: '1',
			displayText: '@name',
			actualText: '<@1>',
			start: 2,
			end: 7,
		};
		update(editor, () => {
			$insertComposerClipboardSlice({display: '> @name\n> b', segments: [segment]}, false);
		});
		expect(snapshot(editor)).toMatchObject({wire: '> <@1>\n> b', selection: {anchor: 11, focus: 11}, wrappers: 2});
		expect(editor.getEditorState().read(() => $projectComposer(), {editor}).segments).toMatchObject([
			{start: 2, end: 7},
		]);
	});

	it('deletes forward from the quote content start a pasted trailing newline leaves the caret at', () => {
		const editor = createHarness();
		setComposer(editor, '> abcd', 4);
		expect(paste(editor, 'x\n').handled).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire: '> abx\n> cd', selection: {anchor: 8, focus: 8}});
		expect(run(editor, DELETE_CHARACTER_COMMAND, false)).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire: '> abx\n> d', selection: {anchor: 8, focus: 8}, wrappers: 2});
	});

	it('declines a paste and a drop while composing', () => {
		const editor = createHarness();
		setComposer(editor, '> abcd', 4);
		update(editor, () => {
			$setCompositionKey($getRoot().getAllTextNodes()[1]!.getKey());
		});
		expect(editor.isComposing()).toBe(true);
		expect(paste(editor, 'x\ny')).toEqual({handled: false, prevented: false});
		const payload = {dataTransfer: {getData: () => 'x\ny'}} as unknown as InputEvent;
		expect(run(editor, CONTROLLED_TEXT_INSERTION_COMMAND, payload)).toBe(false);
		expect(snapshot(editor).wire).toBe('> abcd');
	});

	it('leaves a controlled insertion without a data transfer to the editor fallback', () => {
		const editor = createHarness();
		setComposer(editor, '> abcd', 4);
		expect(run(editor, CONTROLLED_TEXT_INSERTION_COMMAND, {dataTransfer: null} as unknown as InputEvent)).toBe(false);
		expect(run(editor, CONTROLLED_TEXT_INSERTION_COMMAND, 'xy')).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire: '> abxycd', wrappers: 1});
	});

	it('remaps a mention on a continued slice line', () => {
		const editor = createHarness();
		setComposer(editor, '> abcd', 4);
		const segment: MentionSegment = {
			type: 'user',
			id: '1',
			displayText: '@name',
			actualText: '<@1>',
			start: 3,
			end: 8,
		};
		update(editor, () => {
			$insertComposerClipboardSlice({display: 'x\r\n@name y', segments: [segment]}, false);
		});
		expect(snapshot(editor)).toMatchObject({
			wire: '> abx\n> <@1> ycd',
			selection: {anchor: 15, focus: 15},
			wrappers: 2,
		});
		expect(editor.getEditorState().read(() => $projectComposer(), {editor})).toMatchObject({
			display: '> abx\n> @name ycd',
			segments: [{...segment, start: 8, end: 13}],
		});
	});

	it('quotes both slice lines when a mention lands on each', () => {
		const editor = createHarness();
		setComposer(editor, '> ', 2);
		const mention = (start: number): MentionSegment => ({
			type: 'user',
			id: '1',
			displayText: '@name',
			actualText: '<@1>',
			start,
			end: start + 5,
		});
		update(editor, () => {
			$insertComposerClipboardSlice({display: '@name\n@name', segments: [mention(0), mention(6)]}, false);
		});
		expect(snapshot(editor)).toMatchObject({
			wire: '> <@1>\n> <@1>',
			selection: {anchor: 15, focus: 15},
			wrappers: 2,
		});
		expect(editor.getEditorState().read(() => $projectComposer(), {editor}).segments).toMatchObject([
			{start: 2, end: 7},
			{start: 10, end: 15},
		]);
	});

	it('keeps a mention quoted after a blank slice line', () => {
		const editor = createHarness();
		setComposer(editor, '> ab', 4);
		const segment: MentionSegment = {
			type: 'user',
			id: '1',
			displayText: '@name',
			actualText: '<@1>',
			start: 3,
			end: 8,
		};
		update(editor, () => {
			$insertComposerClipboardSlice({display: 'x\n\n@name', segments: [segment]}, false);
		});
		expect(snapshot(editor)).toMatchObject({wire: '> abx\n> \n> <@1>', wrappers: 3});
		expect(editor.getEditorState().read(() => $projectComposer(), {editor}).segments).toMatchObject([
			{start: 11, end: 16},
		]);
	});

	it('continues the quote when a slice replaces a range across two quote lines', () => {
		const editor = createHarness();
		setComposer(editor, '> ab\n> cd', 3, 8);
		const segment: MentionSegment = {
			type: 'user',
			id: '1',
			displayText: '@name',
			actualText: '<@1>',
			start: 2,
			end: 7,
		};
		update(editor, () => {
			$insertComposerClipboardSlice({display: 'x\n@name', segments: [segment]}, false);
		});
		expect(snapshot(editor)).toMatchObject({
			wire: '> ax\n> <@1>d',
			selection: {anchor: 12, focus: 12},
			wrappers: 2,
		});
		expect(editor.getEditorState().read(() => $projectComposer(), {editor}).segments).toMatchObject([
			{start: 7, end: 12},
		]);
	});

	it('drops carriage returns from a slice so a pasted blank quote line exits like a typed one', () => {
		const editor = createHarness();
		setComposer(editor, '', 0);
		update(editor, () => {
			$insertComposerClipboardSlice({display: '> a\r\n> \r\n> b', segments: []}, false);
		});
		expect(snapshot(editor)).toMatchObject({wire: '> a\n> \n> b', wrappers: 3});
		select(editor, 6);
		expect(run(editor, INSERT_LINE_BREAK_COMMAND, false)).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire: '> a\n\n> b', wrappers: 2});
	});

	it('restores both quote lines when a cut slice is pasted back', () => {
		const editor = createHarness();
		setComposer(editor, '> ab\n> cd', 3, 8);
		update(editor, () => {
			const selection = $getSelection();
			if ($isRangeSelection(selection)) {
				selection.removeText();
			}
		});
		update(editor, () => {
			$insertComposerClipboardSlice({display: 'b\n> c', segments: []}, false);
		});
		expect(snapshot(editor)).toMatchObject({
			wire: '> ab\n> cd',
			selection: {anchor: 8, focus: 8},
			wrappers: 2,
		});
	});

	it('reverts a whole paste in one undo step', () => {
		const editor = createHarness({history: true});
		setComposer(editor, '> abcd', 4);
		type(editor, 'q');
		expect(paste(editor, 'x\ny').handled).toBe(true);
		expect(snapshot(editor).wire).toBe('> abqx\n> ycd');
		historyStep(editor, UNDO_COMMAND);
		expect(snapshot(editor)).toMatchObject({wire: '> abqcd', selection: {anchor: 5, focus: 5}, wrappers: 1});
	});

	it('plans pasted text with the parser flags its composer handle carries', () => {
		const editor = createHarness({flags: BIO_LIKE_FLAGS});
		setComposer(editor, '> ```', 5);
		let inserted = false;
		update(editor, () => {
			inserted = $insertComposerPastedText('x\ny', [], false, parserFlagsHandle(BIO_LIKE_FLAGS));
		});
		expect(inserted).toBe(true);
		expect(snapshot(editor)).toMatchObject({
			wire: '> ```x\n> y',
			selection: {anchor: 10, focus: 10},
			wrappers: 2,
		});
	});

	it('plans pasted text with the default flags when no composer handle is mounted', () => {
		const editor = createHarness({flags: BIO_LIKE_FLAGS});
		setComposer(editor, '> ```', 5);
		update(editor, () => {
			$insertComposerPastedText('x\ny', [], false, null);
		});
		expect(snapshot(editor).wire).toBe('> ```x\ny');
	});

	it('maps detected segment offsets from the pasted text onto the display', () => {
		const editor = createHarness();
		setComposer(editor, '> ', 2);
		update(editor, () => {
			$insertComposerPastedText(
				'hi <@1>!',
				[{type: 'user', id: '1', displayText: '@name', actualText: '<@1>', start: 3, end: 7}],
				false,
				null,
			);
		});
		expect(snapshot(editor).wire).toBe('> hi <@1>!');
		expect(editor.getEditorState().read(() => $projectComposer(), {editor})).toMatchObject({
			display: '> hi @name!',
			segments: [{start: 5, end: 10, actualText: '<@1>'}],
		});
	});

	it('quotes every line of a 200 line paste', () => {
		const editor = createHarness();
		setComposer(editor, '> ', 2);
		expect(paste(editor, Array.from({length: 200}, (_, index) => `line ${index}`).join('\n')).handled).toBe(true);
		const state = snapshot(editor);
		expect(state.wrappers).toBe(200);
		expect(state.selection).toEqual({anchor: state.wire.length, focus: state.wire.length});
	});
});

describe('blockquote drag and drop', () => {
	function dropEvent(data: Record<string, string>): {
		clientX: number;
		clientY: number;
		dataTransfer: DataTransfer;
		preventDefault: ReturnType<typeof vi.fn>;
	} {
		return {
			clientX: 0,
			clientY: 0,
			dataTransfer: {getData: (type: string) => data[type] ?? ''} as unknown as DataTransfer,
			preventDefault: vi.fn(),
		};
	}

	function drop(editor: LexicalEditor, data: Record<string, string>): {handled: boolean; prevented: boolean} {
		const event = dropEvent(data);
		const handled = run(editor, DROP_COMMAND, event as unknown as DragEvent);
		return {handled, prevented: event.preventDefault.mock.calls.length > 0};
	}

	function ownMarker(editor: LexicalEditor): string {
		return JSON.stringify({editorKey: editor.getKey()});
	}

	function createDragHarness(): LexicalEditor {
		const editor = createHarness();
		disposers.push(
			registerComposerClipboardCommands(editor, {
				getPlainText: () => false,
				isEditable: () => true,
				getMarkdownParserFlags: () => DEFAULT_COMPOSER_MARKDOWN_FLAGS,
			}),
		);
		return editor;
	}

	function copy(editor: LexicalEditor): Record<string, string> {
		const data: Record<string, string> = {};
		const clipboardData = {
			getData: (type: string) => data[type] ?? '',
			setData: (type: string, value: string) => {
				data[type] = value;
			},
		} as unknown as DataTransfer;
		run(editor, COPY_COMMAND, {clipboardData, preventDefault: vi.fn()} as unknown as ClipboardEvent);
		return data;
	}

	function dragStart(editor: LexicalEditor, moved: string): Record<string, string> {
		const data: Record<string, string> = {};
		const dataTransfer = {
			getData: (type: string) => data[type] ?? '',
			setData: (type: string, value: string) => {
				data[type] = value;
			},
		} as unknown as DataTransfer;
		run(editor, DRAGSTART_COMMAND, {dataTransfer} as unknown as DragEvent);
		data['text/plain'] = moved;
		data['application/x-lexical-drag'] = ownMarker(editor);
		return data;
	}

	it('declines a drop carrying no lexical drag marker', () => {
		const editor = createHarness();
		setComposer(editor, 'one\ntwo\n> alpha', 0, 7);
		dropOffset.value = 10;
		expect(drop(editor, {'text/plain': 'one\ntwo'})).toEqual({handled: false, prevented: false});
		expect(snapshot(editor).wire).toBe('one\ntwo\n> alpha');
	});

	it('declines a drop whose lexical marker is unreadable', () => {
		const editor = createHarness();
		setComposer(editor, 'one\ntwo\n> alpha', 0, 7);
		dropOffset.value = 10;
		for (const marker of ['not json', JSON.stringify({}), JSON.stringify({editorKey: 7})]) {
			expect(drop(editor, {'text/plain': 'one\ntwo', 'application/x-lexical-drag': marker}).handled).toBe(false);
		}
		expect(snapshot(editor).wire).toBe('one\ntwo\n> alpha');
	});

	it('declines a cross-editor drop whose source editor root is not on the page', () => {
		const editor = createHarness();
		setComposer(editor, 'one\ntwo\n> alpha', 0, 7);
		dropOffset.value = 10;
		expect(
			drop(editor, {'text/plain': 'one\ntwo', 'application/x-lexical-drag': JSON.stringify({editorKey: 'other'})}),
		).toEqual({handled: false, prevented: false});
		expect(snapshot(editor).wire).toBe('one\ntwo\n> alpha');
	});

	it('keeps a dragged mention a mention by preferring the composer payload over text/plain', () => {
		const editor = createDragHarness();
		setComposer(editor, 'hello @name world', 6, 11, [
			{type: 'user', id: '1', displayText: '@name', actualText: '<@1>', start: 6, end: 11},
		]);
		const data = dragStart(editor, '@name');
		expect(parseComposerClipboardSlice(data[FLUXER_COMPOSER_CLIPBOARD_MIME] ?? '')).toMatchObject({
			display: '@name',
			segments: [{start: 0, end: 5, actualText: '<@1>'}],
		});
		dropOffset.value = 17;
		expect(drop(editor, data)).toEqual({handled: true, prevented: true});
		expect(snapshot(editor).wire).toBe('hello  world<@1>');
		expect(editor.getEditorState().read(() => $projectComposer(), {editor})).toMatchObject({
			display: 'hello  world@name',
			segments: [{start: 12, end: 17, actualText: '<@1>'}],
		});
	});

	it('continues the quote and keeps the segment offsets when a mention is dragged into a quote line', () => {
		const editor = createDragHarness();
		setComposer(editor, 'hi @name\n> quoted', 3, 8, [
			{type: 'user', id: '1', displayText: '@name', actualText: '<@1>', start: 3, end: 8},
		]);
		const data = dragStart(editor, '@name');
		dropOffset.value = 11;
		expect(drop(editor, data).handled).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire: 'hi \n> <@1>quoted', wrappers: 1});
		expect(editor.getEditorState().read(() => $projectComposer(), {editor}).segments).toMatchObject([
			{start: 6, end: 11, actualText: '<@1>'},
		]);
	});

	it('keeps a dragged standard emoji an emoji where its shortcode would not convert back', () => {
		const editor = createDragHarness();
		disposers.push(registerComposerEmojiShortcode(editor, (name) => (name === 'grinning' ? GRINNING : null)));
		setComposer(editor, 'hi :grinning:\n> quoted', 3, 13);
		expect(snapshot(editor).wire).toBe('hi 😀\n> quoted');
		const data = dragStart(editor, ':grinning:');
		expect(parseComposerClipboardSlice(data[FLUXER_COMPOSER_CLIPBOARD_MIME] ?? '')).toMatchObject({
			display: ':grinning:',
			segments: [{type: 'emoji', id: 'grinning', actualText: '😀', start: 0, end: 10}],
		});
		dropOffset.value = 16;
		expect(drop(editor, data).handled).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire: 'hi \n> 😀quoted', wrappers: 1});
		const emojiInWrapper = editor.getEditorState().read(
			() => {
				const paragraph = $getRoot().getFirstChild();
				const wrapper = $isElementNode(paragraph) ? paragraph.getLastChild() : null;
				return $isComposerBlockquoteLineNode(wrapper) && wrapper.getChildren().some($isComposerStandardEmojiNode);
			},
			{editor},
		);
		expect(emojiInWrapper).toBe(true);
	});

	it('falls back to text/plain when the drag carries no composer payload', () => {
		const editor = createHarness();
		setComposer(editor, 'one\ntwo\n> alpha', 0, 7);
		dropOffset.value = 10;
		expect(drop(editor, {'text/plain': 'one\ntwo', 'application/x-lexical-drag': ownMarker(editor)}).handled).toBe(
			true,
		);
		expect(snapshot(editor)).toMatchObject({wire: '\n> one\n> twoalpha', wrappers: 2});
	});

	it('trusts only the newest drag payload', () => {
		const editor = createDragHarness();
		setComposer(editor, 'alpha', 0, 5);
		const oldest = dragStart(editor, 'alpha')[FLUXER_COMPOSER_CLIPBOARD_MIME] ?? '';
		expect(parseComposerClipboardSlice(oldest)).not.toBeNull();
		const newest = dragStart(editor, 'alpha')[FLUXER_COMPOSER_CLIPBOARD_MIME] ?? '';
		expect(parseComposerClipboardSlice(oldest)).toBeNull();
		expect(parseComposerClipboardSlice(newest)).not.toBeNull();
	});

	it('keeps a copied slice trusted while more drags than the cache holds accumulate', () => {
		const editor = createDragHarness();
		setComposer(editor, 'hello @name world', 6, 11, [
			{type: 'user', id: '1', displayText: '@name', actualText: '<@1>', start: 6, end: 11},
		]);
		const copied = copy(editor)[FLUXER_COMPOSER_CLIPBOARD_MIME] ?? '';
		expect(parseComposerClipboardSlice(copied)).toMatchObject({display: '@name', segments: [{actualText: '<@1>'}]});
		let dragged = '';
		for (let index = 0; index < COMPOSER_CLIPBOARD_MAX_TRUSTED_PAYLOADS + 8; index += 1) {
			dragged = dragStart(editor, '@name')[FLUXER_COMPOSER_CLIPBOARD_MIME] ?? '';
		}
		expect(parseComposerClipboardSlice(copied)).toMatchObject({display: '@name', segments: [{actualText: '<@1>'}]});
		expect(parseComposerClipboardSlice(dragged)).not.toBeNull();
	});

	it('keeps the copied payload cache bounded', () => {
		const editor = createDragHarness();
		setComposer(editor, 'alpha', 0, 5);
		const oldest = copy(editor)[FLUXER_COMPOSER_CLIPBOARD_MIME] ?? '';
		expect(parseComposerClipboardSlice(oldest)).not.toBeNull();
		for (let index = 0; index < COMPOSER_CLIPBOARD_MAX_TRUSTED_PAYLOADS; index += 1) {
			copy(editor);
		}
		expect(parseComposerClipboardSlice(oldest)).toBeNull();
	});

	it('declines a same-editor drag from a collapsed selection', () => {
		const editor = createHarness();
		setComposer(editor, 'one\ntwo\n> alpha', 4);
		expect(drop(editor, {'text/plain': 'one\ntwo', 'application/x-lexical-drag': ownMarker(editor)}).handled).toBe(
			false,
		);
		expect(snapshot(editor).wire).toBe('one\ntwo\n> alpha');
	});

	it('keeps the dragged range when the drop caret cannot be resolved', () => {
		const editor = createHarness();
		setComposer(editor, 'one\ntwo\n> alpha', 0, 7);
		expect(drop(editor, {'text/plain': 'one\ntwo', 'application/x-lexical-drag': ownMarker(editor)})).toEqual({
			handled: false,
			prevented: false,
		});
		expect(snapshot(editor).wire).toBe('one\ntwo\n> alpha');
		expect(snapshot(editor).selection).toEqual({anchor: 0, focus: 7});
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

	it.each(
		BACKSPACE_ROWS,
	)('removes the same text on a backward word delete on %j at %i, giving %j with the caret at %i', (text, before, expected, after) => {
		const editor = createHarness();
		setComposer(editor, text, before);
		expect(run(editor, DELETE_WORD_COMMAND, true)).toBe(true);
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

	it.each<[string, number]>([
		['> a\nb', 4],
		['> test', 4],
		['>  ', 3],
		['abc', 3],
		['```\n> a', 6],
	])('declines a backspace on %j with the caret at %i', (text, caret) => {
		const editor = createHarness();
		setComposer(editor, text, caret);
		expect(run(editor, DELETE_CHARACTER_COMMAND, true)).toBe(false);
	});

	it('declines a backspace over a range inside a quote line', () => {
		const editor = createHarness();
		setComposer(editor, '> test', 2, 4);
		expect(run(editor, DELETE_CHARACTER_COMMAND, true)).toBe(false);
	});

	it('declines every delete while composing', () => {
		const editor = createHarness();
		setComposer(editor, '> test', 2);
		update(editor, () => {
			const selection = $getSelection();
			if ($isRangeSelection(selection)) {
				$setCompositionKey(selection.anchor.key);
			}
		});
		for (const command of [DELETE_CHARACTER_COMMAND, DELETE_WORD_COMMAND, DELETE_LINE_COMMAND]) {
			expect(run(editor, command, true)).toBe(false);
			expect(run(editor, command, false)).toBe(false);
		}
		expect(snapshot(editor).wire).toBe('> test');
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

describe('blockquote forward delete', () => {
	const FORWARD_DELETE_COMMANDS = {
		character: DELETE_CHARACTER_COMMAND,
		word: DELETE_WORD_COMMAND,
		line: DELETE_LINE_COMMAND,
	};

	const FORWARD_DELETE_ROWS: Array<[string, number, string, number]> = [
		['> a\n> b', 3, '> ab', 3],
		['a\n> b', 1, 'ab', 1],
		['> a\n> ', 3, '> a', 3],
		['> \n> b', 2, '> b', 2],
		['a\n> \n> b', 4, 'a\n> b', 4],
		['> \nb', 2, 'b', 0],
		['a\n> \nb', 4, 'a\nb', 2],
	];

	it.each(FORWARD_DELETE_ROWS)('on %j at %i gives %j with the caret at %i', (text, before, expected, after) => {
		const editor = createHarness();
		setComposer(editor, text, before);
		expect(run(editor, DELETE_CHARACTER_COMMAND, false)).toBe(true);
		expect(snapshot(editor)).toMatchObject({
			wire: expected,
			selection: {anchor: after, focus: after},
			wrappers: expectedQuoteLines(expected),
		});
	});

	it('removes an empty quote line when the caret sits at the end of its marker text', () => {
		const editor = createHarness();
		setComposer(editor, '> \n> b');
		selectFirstMarkerText(editor, 2);
		expect(run(editor, DELETE_CHARACTER_COMMAND, false)).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire: '> b', selection: {anchor: 2, focus: 2}, wrappers: 1});
	});

	it('plans line deletes that start at a quote line boundary', () => {
		const editor = createHarness();
		setComposer(editor, '> a\n> b', 3);
		expect(run(editor, DELETE_LINE_COMMAND, false)).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire: '> ab', selection: {anchor: 3, focus: 3}, wrappers: 1});
		setComposer(editor, '> \nabc', 3);
		expect(run(editor, DELETE_LINE_COMMAND, true)).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire: 'abc', selection: {anchor: 0, focus: 0}, wrappers: 0});
	});

	it.each<[keyof typeof FORWARD_DELETE_COMMANDS, string, number, string, number]>([
		['character', '> ab', 2, '> b', 2],
		['character', 'x\n> ab', 4, 'x\n> b', 4],
		['character', '> \u{1F468}\u200D\u{1F469}\u200D\u{1F467}x', 2, '> x', 2],
		['character', '> e\u0301x', 2, '> ex', 3],
		['word', '> ab ef', 2, '>  ef', 2],
		['word', 'x\n> ab\n> cd', 4, 'x\n> \n> cd', 4],
		['word', '>    \n> cd', 2, '> \n> cd', 2],
		['line', 'x\n> ab ef\n> gh', 4, 'x\n> \n> gh', 4],
	])('deletes forward by %s from the quote content start of %j at %i, giving %j with the caret at %i', (unit, text, before, expected, after) => {
		const editor = createHarness();
		setComposer(editor, text, before);
		expect(run(editor, FORWARD_DELETE_COMMANDS[unit], false)).toBe(true);
		expect(snapshot(editor)).toMatchObject({
			wire: expected,
			selection: {anchor: after, focus: after},
			wrappers: expectedQuoteLines(expected),
		});
	});

	it.each<[keyof typeof FORWARD_DELETE_COMMANDS, string, Array<number>, string]>([
		['character', 'x\n> @name@name y', [4, 9], 'x\n> <@1> y'],
		['word', 'x\n> @name@name y', [4, 9], 'x\n> <@1> y'],
		['line', 'x\n> @name@name y', [4, 9], 'x\n> '],
		['word', 'x\n> ab@name y', [6], 'x\n>  y'],
	])('deletes forward by %s over whole mentions from the quote content start of %j', (unit, text, starts, wire) => {
		const editor = createHarness();
		const segments = starts.map(
			(start): MentionSegment => ({
				type: 'user',
				id: '1',
				displayText: '@name',
				actualText: '<@1>',
				start,
				end: start + 5,
			}),
		);
		setComposer(editor, text, 4, 4, segments);
		expect(run(editor, FORWARD_DELETE_COMMANDS[unit], false)).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire, selection: {anchor: 4, focus: 4}, wrappers: 1});
	});

	it('restores the text of a forward delete at a quote content start in one undo step', () => {
		const editor = createHarness({history: true});
		setComposer(editor, 'x\n> ab ef', 4);
		expect(run(editor, DELETE_WORD_COMMAND, false)).toBe(true);
		expect(snapshot(editor).wire).toBe('x\n>  ef');
		historyStep(editor, UNDO_COMMAND);
		expect(snapshot(editor)).toMatchObject({wire: 'x\n> ab ef', selection: {anchor: 4, focus: 4}, wrappers: 1});
	});

	function expectForwardDeleteDeclined(text: string, anchor: number, focus?: number): void {
		const editor = createHarness();
		setComposer(editor, text, anchor, focus);
		for (const command of Object.values(FORWARD_DELETE_COMMANDS)) {
			expect(run(editor, command, false)).toBe(false);
		}
		expect(snapshot(editor).wire).toBe(text);
	}

	it.each<[string, number]>([
		['\n> b', 0],
		['a\n\n> b', 2],
		['> a\nb', 3],
		['> test', 4],
		['> ', 2],
		['```\n> a\n> b', 7],
	])('declines every forward delete on %j with the caret at %i', (text, caret) => {
		expectForwardDeleteDeclined(text, caret);
	});

	it.each<[string, number, number]>([
		['> a\n> b', 3, 6],
		['> a\n> b', 2, 3],
	])('declines every forward delete on %j over the range %i..%i', (text, anchor, focus) => {
		expectForwardDeleteDeclined(text, anchor, focus);
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

	it('keeps a range that starts at the start of a quote line', () => {
		const editor = createHarness();
		setComposer(editor, '> test');
		select(editor, 0, 4);
		run(editor, SELECTION_CHANGE_COMMAND, undefined);
		expect(snapshot(editor).selection).toEqual({anchor: 0, focus: 4});
	});

	it.each([
		['ab\n> cd', 1, 3, 1, 5],
		['ab\n> cd', 4, 1, 5, 1],
		['ab\n> cd', 4, 7, 5, 7],
		['ab\n> cd', 3, 7, 3, 7],
		['> a\n> b', 0, 7, 0, 7],
	])('moves range endpoints on %j from %i..%i to %i..%i on a selection change', (text, anchor, focus, expectedAnchor, expectedFocus) => {
		const editor = createHarness();
		setComposer(editor, text);
		select(editor, anchor, focus);
		run(editor, SELECTION_CHANGE_COMMAND, undefined);
		expect(snapshot(editor).selection).toEqual({anchor: expectedAnchor, focus: expectedFocus});
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
	});

	it('declines ArrowLeft while composing and away from content start', () => {
		const editor = createHarness();
		setComposer(editor, 'ab\n> cd', 5);
		expect(run(editor, KEY_ARROW_LEFT_COMMAND, keyEvent({isComposing: true}))).toBe(false);
		setComposer(editor, '> test', 4);
		expect(run(editor, KEY_ARROW_LEFT_COMMAND, keyEvent())).toBe(false);
	});

	it.each<[boolean, Record<string, boolean>, string, number, number, number]>([
		[true, {altKey: true}, 'ab\n> cd', 5, 2, 2],
		[false, {ctrlKey: true}, 'ab\n> cd', 5, 2, 2],
		[true, {altKey: true, shiftKey: true}, 'ab\n> cd', 5, 5, 2],
		[false, {ctrlKey: true, shiftKey: true}, 'ab\n> cd', 5, 5, 2],
		[true, {altKey: true}, '> a\n> cd', 6, 3, 3],
		[true, {altKey: true}, '> cd', 2, 2, 2],
		[true, {altKey: true, shiftKey: true}, '> cd', 2, 2, 2],
	])('with apple=%s and %j moves a word back from the quote content start of %j at %i to %i..%i', (apple, modifiers, text, caret, anchor, focus) => {
		platform.apple = apple;
		const editor = createHarness();
		setComposer(editor, text, caret);
		const event = keyEvent(modifiers);
		expect(run(editor, KEY_DOWN_COMMAND, event)).toBe(true);
		expect(event.preventDefault).toHaveBeenCalled();
		expect(snapshot(editor).selection).toEqual({anchor, focus});
	});

	it.each<[boolean, Record<string, unknown>, string, number]>([
		[true, {ctrlKey: true}, 'ab\n> cd', 5],
		[false, {altKey: true}, 'ab\n> cd', 5],
		[true, {altKey: true, metaKey: true}, 'ab\n> cd', 5],
		[false, {ctrlKey: true, altKey: true}, 'ab\n> cd', 5],
		[true, {altKey: true, key: 'ArrowRight', keyCode: 39}, 'ab\n> cd', 2],
		[true, {altKey: true, shiftKey: true, key: 'ArrowRight', keyCode: 39}, 'ab\n> cd', 2],
		[true, {altKey: true}, 'ab\n> cd', 6],
		[true, {altKey: true, isComposing: true}, 'ab\n> cd', 5],
		[true, {altKey: true, key: 'b', keyCode: 66}, 'ab\n> cd', 5],
	])('with apple=%s leaves %j on %j at %i to the browser', (apple, overrides, text, caret) => {
		platform.apple = apple;
		const editor = createHarness();
		setComposer(editor, text, caret);
		const event = keyEvent(overrides);
		run(editor, KEY_DOWN_COMMAND, event);
		expect(event.preventDefault).not.toHaveBeenCalled();
		expect(snapshot(editor).selection).toEqual({anchor: caret, focus: caret});
	});

	it('replaces a word selection extended over the hidden marker without leaving marker text', () => {
		platform.apple = false;
		const editor = createHarness();
		setComposer(editor, 'ab\n> cd', 5);
		expect(run(editor, KEY_DOWN_COMMAND, keyEvent({ctrlKey: true, shiftKey: true}))).toBe(true);
		type(editor, 'y');
		expect(snapshot(editor)).toMatchObject({wire: 'abycd', selection: {anchor: 3, focus: 3}, wrappers: 0});
	});

	it('moves a word back on the arrow that points backward in right-to-left text', () => {
		platform.apple = true;
		const editor = createHarness();
		vi.spyOn(editor, 'getElementByKey').mockReturnValue(RTL_ELEMENT);
		setComposer(editor, 'سطر\n> مرحبا', 6);
		const forward = keyEvent({altKey: true});
		run(editor, KEY_DOWN_COMMAND, forward);
		expect(forward.preventDefault).not.toHaveBeenCalled();
		expect(snapshot(editor).selection).toEqual({anchor: 6, focus: 6});
		expect(run(editor, KEY_DOWN_COMMAND, keyEvent({key: 'ArrowRight', keyCode: 39, altKey: true}))).toBe(true);
		expect(snapshot(editor).selection).toEqual({anchor: 3, focus: 3});
	});

	it('jumps over the hidden marker on ArrowRight at the end of the line above', () => {
		const editor = createHarness();
		setComposer(editor, 'ab\n> cd', 2);
		const event = keyEvent({key: 'ArrowRight', keyCode: 39});
		expect(run(editor, KEY_ARROW_RIGHT_COMMAND, event)).toBe(true);
		expect(event.preventDefault).toHaveBeenCalled();
		expect(snapshot(editor).selection).toEqual({anchor: 5, focus: 5});
		expect(run(editor, KEY_ARROW_RIGHT_COMMAND, keyEvent({key: 'ArrowRight', keyCode: 39}))).toBe(false);
	});

	it('extends a shift selection over the hidden marker and back', () => {
		const editor = createHarness();
		setComposer(editor, 'x\n> ab', 4);
		expect(run(editor, KEY_ARROW_LEFT_COMMAND, keyEvent({shiftKey: true}))).toBe(true);
		expect(snapshot(editor).selection).toEqual({anchor: 4, focus: 1});
		expect(run(editor, KEY_ARROW_RIGHT_COMMAND, keyEvent({key: 'ArrowRight', keyCode: 39, shiftKey: true}))).toBe(true);
		expect(snapshot(editor).selection).toEqual({anchor: 4, focus: 4});
	});

	it.each<[string, number, 'left' | 'right', number, string, string, number]>([
		['x\n> ab', 4, 'left', 1, 'y', 'xyab', 2],
		['> a\n> b', 3, 'right', 6, 'x', '> axb', 4],
	])('replaces a shift selection on %j from %i without leaving marker text', (text, caret, direction, extended, typed, expected, after) => {
		const editor = createHarness();
		setComposer(editor, text, caret);
		const command = direction === 'left' ? KEY_ARROW_LEFT_COMMAND : KEY_ARROW_RIGHT_COMMAND;
		expect(run(editor, command, keyEvent({shiftKey: true}))).toBe(true);
		expect(snapshot(editor).selection).toEqual({anchor: caret, focus: extended});
		type(editor, typed);
		expect(snapshot(editor)).toMatchObject({
			wire: expected,
			selection: {anchor: after, focus: after},
			wrappers: expectedQuoteLines(expected),
		});
	});

	it('keeps a shift selection at content start of the first quote line', () => {
		const editor = createHarness();
		setComposer(editor, '> ab', 2);
		expect(run(editor, KEY_ARROW_LEFT_COMMAND, keyEvent({shiftKey: true}))).toBe(true);
		expect(snapshot(editor).selection).toEqual({anchor: 2, focus: 2});
		expect(run(editor, DELETE_CHARACTER_COMMAND, true)).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire: 'ab', selection: {anchor: 0, focus: 0}, wrappers: 0});
	});

	it('swaps the arrow keys in right-to-left text', () => {
		const editor = createHarness();
		vi.spyOn(editor, 'getElementByKey').mockReturnValue(RTL_ELEMENT);
		setComposer(editor, 'سطر\n> مرحبا', 6);
		expect(run(editor, KEY_ARROW_LEFT_COMMAND, keyEvent())).toBe(false);
		expect(run(editor, KEY_ARROW_RIGHT_COMMAND, keyEvent({key: 'ArrowRight', keyCode: 39}))).toBe(true);
		expect(snapshot(editor).selection).toEqual({anchor: 3, focus: 3});
		expect(run(editor, KEY_ARROW_RIGHT_COMMAND, keyEvent({key: 'ArrowRight', keyCode: 39}))).toBe(false);
		expect(run(editor, KEY_ARROW_LEFT_COMMAND, keyEvent())).toBe(true);
		expect(snapshot(editor).selection).toEqual({anchor: 6, focus: 6});
		setComposer(editor, '> مرحبا', 2);
		expect(run(editor, KEY_ARROW_LEFT_COMMAND, keyEvent())).toBe(false);
		expect(run(editor, KEY_ARROW_RIGHT_COMMAND, keyEvent({key: 'ArrowRight', keyCode: 39}))).toBe(true);
		expect(snapshot(editor).selection).toEqual({anchor: 2, focus: 2});
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

	it.each<[string, number, string, number, number]>([
		['a\n>>>\nb', 5, 'a\n> \n> b', 4, 2],
		['>>>\nb\nc', 3, '> \n> b\n> c', 2, 3],
	])('quotes every line below a marker typed on %j at %i and restores it on undo', (text, caret, wire, after, wrappers) => {
		const editor = createHarness({history: true});
		setComposer(editor, text, caret);
		type(editor, ' ');
		expect(snapshot(editor)).toMatchObject({wire, selection: {anchor: after, focus: after}, wrappers});
		historyStep(editor, UNDO_COMMAND);
		expect(snapshot(editor)).toMatchObject({wire: text, selection: {anchor: caret, focus: caret}});
	});

	it.each<[string, number, string, number]>([
		['>>>\n> b', 3, '>>> \n> b', 4],
		['> x\n>>>\nb', 7, '> x\n>>> \nb', 8],
	])('leaves a marker typed on %j at %i as text because line quotes would change the rendered message', (text, caret, wire, after) => {
		const editor = createHarness();
		setComposer(editor, text, caret);
		type(editor, ' ');
		expect(snapshot(editor)).toMatchObject({wire, selection: {anchor: after, focus: after}});
	});

	it('quotes every line below a pasted ">>> "', () => {
		const editor = createHarness();
		setComposer(editor, 'a\nb', 0);
		update(editor, () => {
			$insertComposerClipboardSlice({display: '>>> ', segments: []}, false);
		});
		expect(snapshot(editor)).toMatchObject({wire: '> a\n> b', selection: {anchor: 2, focus: 2}, wrappers: 2});
	});

	it('leaves a ">>> " pasted on the last line below a quote as text instead of taking the typed branch', () => {
		const editor = createHarness();
		setComposer(editor, '> x\n', 4);
		update(editor, () => {
			$insertComposerClipboardSlice({display: '>>> ', segments: []}, false);
		});
		expect(snapshot(editor)).toMatchObject({wire: '> x\n>>> ', selection: {anchor: 8, focus: 8}});
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

	it.each<[string, number]>([
		['>>> a\n> b', 0],
		['>>> > a\nb', 0],
		['>>> a\n>>> b', 0],
		['>>> a\n> [!NOTE]\n> b', 0],
		['> x\n>>> a\nb', 1],
		['> [!NOTE]\n>>> a\nb', 1],
		['||s\n>>> a\nb||\nc', 0],
	])('leaves arrived %j as text because line quotes would change the rendered message', (text, wrappers) => {
		const editor = createHarness();
		update(editor, () => {
			$hydrateComposerFromDraft(text, []);
		});
		expect(snapshot(editor)).toMatchObject({wire: text, wrappers});
	});

	it('leaves a pasted ">>> " block that holds a quote line as text', () => {
		const editor = createHarness();
		setComposer(editor, '', 0);
		update(editor, () => {
			$insertComposerClipboardSlice({display: '>>> a\n> b', segments: []}, false);
		});
		expect(snapshot(editor)).toMatchObject({wire: '>>> a\n> b', selection: {anchor: 9, focus: 9}, wrappers: 0});
	});

	it('inserts a plain line break inside an arrived block that stays text', () => {
		const editor = createHarness();
		setComposer(editor, '>>> a\n> b', 5);
		run(editor, INSERT_LINE_BREAK_COMMAND, false);
		expect(snapshot(editor)).toMatchObject({wire: '>>> a\n\n> b', wrappers: 0});
	});

	it('rewrites every line once an edit removes the nested quote line', () => {
		const editor = createHarness();
		setComposer(editor, '>>> a\n> b', 6, 8);
		type(editor, '');
		expect(snapshot(editor)).toMatchObject({wire: '> a\n> b', selection: {anchor: 6, focus: 6}, wrappers: 2});
	});

	it.each([2, 4])('rewrites every line when the transforms register over a caret at %i on the marker', (caret) => {
		const editor = createBareEditor();
		setComposer(editor, '>>> a\nb\nc', caret);
		disposers.push(registerComposerMarkdownHighlight(editor), registerComposerBlockquote(editor));
		editor.update(
			() => {
				for (const node of $getRoot().getAllTextNodes()) {
					node.markDirty();
				}
			},
			{discrete: true, tag: HISTORY_MERGE_TAG},
		);
		expect(snapshot(editor)).toMatchObject({wire: '> a\n> b\n> c', selection: {anchor: 2, focus: 2}, wrappers: 3});
	});

	it('keeps an arrived block that stays text when a pass runs with the caret on its marker', () => {
		const editor = createHarness();
		setComposer(editor, '>>> a\n> b');
		select(editor, 4);
		update(editor, () => {
			for (const node of $getRoot().getAllTextNodes()) {
				node.markDirty();
			}
		});
		expect(snapshot(editor)).toMatchObject({wire: '>>> a\n> b', selection: {anchor: 4, focus: 4}, wrappers: 0});
	});

	it('rewrites a marker typed on the last line where an arrived block would stay text', () => {
		const editor = createHarness();
		setComposer(editor, '> x\n>>>', 7);
		type(editor, ' ');
		expect(snapshot(editor)).toMatchObject({wire: '> x\n> ', selection: {anchor: 6, focus: 6}});
	});

	it.each<[number, string]>([
		[10, '>>> a\nb\nc'],
		[11, '> a\n> b\n> c'],
	])('with a max wire length of %i turns arrived ">>> a\\nb\\nc" into %j', (maxWireLength, wire) => {
		const editor = createHarness({maxWireLength});
		update(editor, () => {
			$hydrateComposerFromDraft('>>> a\nb\nc', []);
		});
		expect(snapshot(editor).wire).toBe(wire);
	});

	it.each<[number, string]>([
		[10, '>>> a\nb\nc'],
		[11, '> a\n> b\n> c'],
	])('with a max wire length of %i the mount pass over a hydrated ">>> a\\nb\\nc" gives %j', (maxWireLength, wire) => {
		const editor = createBareEditor();
		update(editor, () => {
			$hydrateComposerFromDraft('>>> a\nb\nc', []);
		});
		disposers.push(
			registerComposerMarkdownHighlight(editor, undefined, false, maxWireLength),
			registerComposerBlockquote(editor),
		);
		editor.update(
			() => {
				for (const node of $getRoot().getAllTextNodes()) {
					node.markDirty();
				}
			},
			{discrete: true, tag: HISTORY_MERGE_TAG},
		);
		expect(snapshot(editor).wire).toBe(wire);
	});

	it.each([1, 2, 3, 4])('leaves arrived ">>> a\\n> b" as text when the hydrate places the caret at %i', (caret) => {
		const editor = createHarness();
		setComposer(editor, '>>> a\n> b', caret);
		expect(snapshot(editor)).toMatchObject({wire: '>>> a\n> b', selection: {anchor: caret, focus: caret}, wrappers: 0});
	});

	it('leaves an arrived block as text when deleting its first character leaves the caret after the marker', () => {
		const editor = createHarness();
		setComposer(editor, '>>> za\n> b', 4, 5);
		type(editor, '');
		expect(snapshot(editor)).toMatchObject({wire: '>>> a\n> b', selection: {anchor: 4, focus: 4}, wrappers: 0});
	});

	it.each<[number, number, string]>([
		[4, 4, 'z'],
		[3, 4, ''],
	])('keeps arrived ">>> a\\n> b" as text after undoing an edit over %i..%i', (anchor, focus, text) => {
		const editor = createHarness({history: true});
		setComposer(editor, '>>> a\n> b');
		select(editor, 4);
		update(editor, () => {
			$selectComposerRange(anchor, focus);
			const selection = $getSelection();
			if ($isRangeSelection(selection)) {
				selection.insertText(text);
			}
		});
		historyStep(editor, UNDO_COMMAND);
		expect(snapshot(editor)).toMatchObject({wire: '>>> a\n> b', selection: {anchor: 4, focus: 4}, wrappers: 0});
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
		'>>> a\n> b',
		'> x\n>>> a\nb',
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
	it('makes no quote markers without ALLOW_BLOCKQUOTES, so every handler finds no quote line', () => {
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
		setComposer(editor, '> a\n> b', 3);
		expect(run(editor, DELETE_CHARACTER_COMMAND, false)).toBe(false);
		expect(run(editor, DELETE_LINE_COMMAND, false)).toBe(false);
		expect(run(editor, KEY_ARROW_RIGHT_COMMAND, keyEvent({key: 'ArrowRight', keyCode: 39}))).toBe(false);
		select(editor, 6);
		expect(run(editor, DELETE_WORD_COMMAND, false)).toBe(false);
		expect(run(editor, DELETE_WORD_COMMAND, true)).toBe(false);
		platform.apple = true;
		const wordMove = keyEvent({altKey: true});
		run(editor, KEY_DOWN_COMMAND, wordMove);
		expect(wordMove.preventDefault).not.toHaveBeenCalled();
		expect(snapshot(editor).selection).toEqual({anchor: 6, focus: 6});
		const payload = {dataTransfer: {getData: () => 'x\ny'}} as unknown as InputEvent;
		expect(run(editor, CONTROLLED_TEXT_INSERTION_COMMAND, payload)).toBe(false);
		expect(snapshot(editor)).toMatchObject({wire: '> a\n> b', wrappers: 0});
	});

	it('continues quotes under bio style flags', () => {
		const editor = createHarness({flags: BIO_LIKE_FLAGS});
		setComposer(editor, '> a', 3);
		expect(run(editor, INSERT_LINE_BREAK_COMMAND, false)).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire: '> a\n> ', selection: {anchor: 6, focus: 6}});
	});

	it('treats a fence as text under bio style flags, so every quoted line keeps its wrapper', () => {
		const editor = createHarness({flags: BIO_LIKE_FLAGS});
		setComposer(editor, '> ```\n> b\n> ```');
		expect(snapshot(editor)).toMatchObject({wrappers: 3, breaksInsideWrappers: 0});
	});

	it('continues the quote on and below a quoted fence under bio style flags', () => {
		const editor = createHarness({flags: BIO_LIKE_FLAGS});
		setComposer(editor, '> ```', 5);
		expect(run(editor, INSERT_LINE_BREAK_COMMAND, false)).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire: '> ```\n> ', selection: {anchor: 8, focus: 8}, wrappers: 2});
		setComposer(editor, '> ```\n> b', 9);
		expect(run(editor, INSERT_LINE_BREAK_COMMAND, false)).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire: '> ```\n> b\n> ', selection: {anchor: 12, focus: 12}, wrappers: 3});
	});

	it('removes only the marker below a quoted fence under bio style flags', () => {
		const editor = createHarness({flags: BIO_LIKE_FLAGS});
		setComposer(editor, '> ```\n> b', 8);
		expect(run(editor, DELETE_CHARACTER_COMMAND, true)).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire: '> ```\nb', selection: {anchor: 6, focus: 6}, wrappers: 1});
	});
});

describe('formatting over hidden quote markers', () => {
	const LINK = 'https://fluxer.app';

	function wrap(editor: LexicalEditor, wrapper: string): void {
		update(editor, () => {
			$wrapComposerSelection(wrapper, wrapper);
		});
	}

	function wrappedFlags(editor: LexicalEditor, wrappers: ReadonlyArray<string>): Array<boolean> {
		return editor
			.getEditorState()
			.read(
				() => $queryComposerSelectionWrappers(wrappers.map((wrapper) => ({prefix: wrapper, suffix: wrapper}))).wrapped,
				{editor},
			);
	}

	function pasteLink(editor: LexicalEditor): boolean {
		return run(editor, PASTE_COMMAND, {
			clipboardData: {getData: (type: string) => (type === 'text/plain' ? LINK : '')},
			preventDefault: vi.fn(),
		} as unknown as ClipboardEvent);
	}

	it.each<[string, number, number, string, string, number, number, number]>([
		['> hello', 0, 7, '**', '> **hello**', 4, 9, 1],
		['> hello', 7, 0, '**', '> **hello**', 9, 4, 1],
		['> hello', 0, 7, '`', '> `hello`', 3, 8, 1],
		['> a\n> b', 4, 7, '~~', '> a\n> ~~b~~', 8, 9, 2],
		['> a\n> b', 2, 4, '**', '> **a\n> **b', 4, 8, 2],
	])('wraps only the content of %j selected from %i to %i with %j', (text, anchor, focus, wrapper, wire, expectedAnchor, expectedFocus, wrappers) => {
		const editor = createFormattingHarness();
		setComposer(editor, text, anchor, focus);
		wrap(editor, wrapper);
		expect(snapshot(editor)).toMatchObject({
			wire,
			selection: {anchor: expectedAnchor, focus: expectedFocus},
			wrappers,
		});
	});

	it.each<[string, string]>([
		['> *hi*', '*'],
		['> **hi**', '**'],
	])('reports %j selected from its line start as wrapped in %j', (text, wrapper) => {
		const editor = createFormattingHarness();
		setComposer(editor, text, 0, text.length);
		expect(wrappedFlags(editor, [wrapper])).toEqual([true]);
	});

	it('removes the bold of a quote line selected from its line start', () => {
		const editor = createFormattingHarness();
		setComposer(editor, '> **hi**', 0, 8);
		wrap(editor, '**');
		expect(snapshot(editor)).toMatchObject({wire: '> hi', selection: {anchor: 2, focus: 4}, wrappers: 1});
	});

	it('bolds only the content on Ctrl+B over a select all', () => {
		const editor = createFormattingHarness();
		setComposer(editor, '> hello', 0, 7);
		run(editor, KEY_DOWN_COMMAND, keyEvent({key: 'b', ctrlKey: true}));
		expect(snapshot(editor).wire).toBe('> **hello**');
	});

	it('leaves the text alone on Ctrl+B over a range that covers only the marker', () => {
		const editor = createFormattingHarness();
		setComposer(editor, '> hello', 2, 0);
		run(editor, KEY_DOWN_COMMAND, keyEvent({key: 'b', ctrlKey: true}));
		expect(snapshot(editor).wire).toBe('> hello');
	});

	it.each<[number, number]>([
		[2, 7],
		[0, 7],
	])('declines the code block toggle over quote lines selected from %i to %i', (anchor, focus) => {
		const editor = createFormattingHarness();
		setComposer(editor, '> a\n> b', anchor, focus);
		expect(wrappedFlags(editor, ['`'])).toEqual([false]);
		wrap(editor, '`');
		expect(snapshot(editor)).toMatchObject({wire: '> a\n> b', selection: {anchor, focus}, wrappers: 2});
	});

	it('indents the code of selected quoted fence lines and unindents it again', () => {
		const editor = createFormattingHarness();
		setComposer(editor, '> ```\n> a\n> b\n> ```', 6, 13);
		expect(run(editor, KEY_TAB_COMMAND, keyEvent({key: 'Tab'}))).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire: '> ```\n> \ta\n> \tb\n> ```', selection: {anchor: 9, focus: 15}});
		expect(run(editor, KEY_TAB_COMMAND, keyEvent({key: 'Tab', shiftKey: true}))).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire: '> ```\n> a\n> b\n> ```', selection: {anchor: 8, focus: 13}});
	});

	it('unindents the code of a selected indented quoted fence line', () => {
		const editor = createFormattingHarness();
		setComposer(editor, '> ```\n> \tcode\n> ```', 6, 13);
		expect(run(editor, KEY_TAB_COMMAND, keyEvent({key: 'Tab', shiftKey: true}))).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire: '> ```\n> code\n> ```', selection: {anchor: 8, focus: 12}});
	});

	it.each<[string, number, number, string, number]>([
		['> hello world', 0, 13, `> [hello world](<${LINK}>)`, 37],
		['intro\n> hello world', 6, 19, `intro\n> [hello world](<${LINK}>)`, 43],
		['> hello world', 13, 8, `> hello [world](<${LINK}>)`, 37],
	])('keeps the quote outside a link label pasted over %j from %i to %i', (text, anchor, focus, wire, caret) => {
		const editor = createFormattingHarness();
		setComposer(editor, text, anchor, focus);
		expect(pasteLink(editor)).toBe(true);
		expect(snapshot(editor)).toMatchObject({wire, selection: {anchor: caret, focus: caret}, wrappers: 1});
	});

	it('declines a link paste over a range that covers only the marker', () => {
		const editor = createFormattingHarness();
		setComposer(editor, '> hello world', 2, 0);
		expect(pasteLink(editor)).toBe(false);
		expect(snapshot(editor).wire).toBe('> hello world');
	});

	it('formats the literal "> " without ALLOW_BLOCKQUOTES', () => {
		const editor = createFormattingHarness(NO_BLOCKQUOTE_FLAGS);
		setComposer(editor, '> a', 0, 3);
		wrap(editor, '**');
		expect(snapshot(editor)).toMatchObject({wire: '**> a**', selection: {anchor: 2, focus: 5}, wrappers: 0});
		setComposer(editor, '> a\n> b', 0, 7);
		wrap(editor, '`');
		expect(snapshot(editor).wire).toBe('```\n> a\n> b\n```');
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

	it('projects a standard emoji inside a quote line and copies its surrogate', () => {
		const editor = createHarness();
		disposers.push(registerComposerEmojiShortcode(editor, (name) => (name === 'grinning' ? GRINNING : null)));
		setComposer(editor, '> :grinning:', 12, 12);
		const projection = editor.getEditorState().read(() => $projectComposer(), {editor});
		expect(projection).toEqual({
			display: '> :grinning:',
			wire: '> 😀',
			segments: [{type: 'emoji', id: 'grinning', displayText: ':grinning:', actualText: '😀', start: 2, end: 12}],
		});
		select(editor, 2, 12);
		const copied = editor.getEditorState().read(() => $getComposerClipboardSelection(), {editor});
		expect(copied).toMatchObject({display: ':grinning:', textPlain: '😀'});
		setComposer(editor, projection.display, undefined, undefined, projection.segments);
		expect(editor.getEditorState().read(() => $projectComposer(), {editor})).toEqual(projection);
		const emojiInWrapper = editor.getEditorState().read(
			() => {
				const paragraph = $getRoot().getFirstChild();
				const wrapper = $isElementNode(paragraph) ? paragraph.getFirstChild() : null;
				return $isComposerBlockquoteLineNode(wrapper) && wrapper.getChildren().some($isComposerStandardEmojiNode);
			},
			{editor},
		);
		expect(emojiInWrapper).toBe(true);
	});

	it.each<[string, string]>([
		['zero', '0️⃣'],
		['one', '1️⃣'],
		['two', '2️⃣'],
		['three', '3️⃣'],
		['four', '4️⃣'],
		['five', '5️⃣'],
		['six', '6️⃣'],
		['seven', '7️⃣'],
		['eight', '8️⃣'],
		['nine', '9️⃣'],
		['hash', '#️⃣'],
		['asterisk', '*️⃣'],
	])('keeps the keycap emoji :%s: an emoji when it is copied into the middle of a quoted word', (name, surrogate) => {
		const editor = createHarness();
		const display = `:${name}:`;
		disposers.push(
			registerComposerEmojiShortcode(editor, (shortcode) =>
				shortcode === name ? {kind: 'standard', name, surrogate, url: null, display} : null,
			),
		);
		setComposer(editor, `> ${display} word`, 2, 2 + display.length);
		const copied = editor.getEditorState().read(() => $getComposerClipboardSelection(), {editor});
		expect(copied).toMatchObject({
			display,
			textPlain: surrogate,
			segments: [{type: 'emoji', displayText: display, actualText: surrogate, start: 0, end: display.length}],
		});
		select(editor, display.length + 5);
		update(editor, () => {
			$insertComposerClipboardSlice(copied!, false);
		});
		expect(snapshot(editor)).toMatchObject({wire: `> ${surrogate} wo${surrogate}rd`, wrappers: 1});
		const emojiWires = editor.getEditorState().read(
			() => {
				const paragraph = $getRoot().getFirstChild();
				const wrapper = $isElementNode(paragraph) ? paragraph.getFirstChild() : null;
				return $isComposerBlockquoteLineNode(wrapper)
					? wrapper
							.getChildren()
							.filter($isComposerStandardEmojiNode)
							.map((node) => node.getWireText())
					: [];
			},
			{editor},
		);
		expect(emojiWires).toEqual([surrogate, surrogate]);
	});

	it.each([
		'*😀*',
		'😀<@1>',
		'`😀`',
		'1😀',
		'#😀',
		':one:',
	])('keeps an emoji segment whose wire %j is shaped like markup as text', (actualText) => {
		const editor = createHarness();
		setComposer(editor, '> :x:', undefined, undefined, [
			{type: 'emoji', id: 'x', displayText: ':x:', actualText, start: 2, end: 5},
		]);
		expect(editor.getEditorState().read(() => $projectComposer(), {editor})).toEqual({
			display: '> :x:',
			wire: '> :x:',
			segments: [],
		});
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
