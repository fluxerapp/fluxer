// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {
	$getComposerClipboardSelection,
	$insertComposerClipboardSlice,
} from '@app/features/lexical/composer/ComposerClipboard';
import {resetComposerHistory} from '@app/features/lexical/composer/ComposerHistory';
import {registerComposerMarkdownHighlight} from '@app/features/lexical/composer/ComposerMarkdownHighlight';
import {registerComposerPlainText} from '@app/features/lexical/composer/ComposerPlainText';
import {
	$hydrateComposerFromDraft,
	$projectComposer,
	isValidComposerSegment,
} from '@app/features/lexical/composer/ComposerSerialization';
import {
	COMPOSER_RESCAN_TAG,
	registerComposerSpecialMention,
} from '@app/features/lexical/composer/ComposerSpecialMention';
import {
	$captureSelectionOffsets,
	$replaceComposerRange,
	$selectComposerOffset,
	$selectComposerRange,
} from '@app/features/lexical/composer/composerOffsets';
import {
	$createComposerCommandNode,
	ComposerCommandNode,
} from '@app/features/lexical/composer/nodes/ComposerCommandNode';
import {ComposerCustomEmojiNode} from '@app/features/lexical/composer/nodes/ComposerCustomEmojiNode';
import {$isComposerMentionNode, ComposerMentionNode} from '@app/features/lexical/composer/nodes/ComposerMentionNode';
import {ComposerPlainSegmentNode} from '@app/features/lexical/composer/nodes/ComposerPlainSegmentNode';
import {$createSlashSlotNode, SlashSlotNode} from '@app/features/lexical/composer/nodes/SlashSlotNode';
import {SlashSlotPlaceholderNode} from '@app/features/lexical/composer/nodes/SlashSlotPlaceholderNode';
import {SyntaxMarkerNode} from '@app/features/lexical/composer/nodes/SyntaxMarkerNode';
import {createSpecialMentionPayload, type SpecialMentionKind} from '@app/features/lexical/composer/specialMentions';
import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';
import {createEmptyHistoryState, registerHistory} from '@lexical/history';
import {
	$addUpdateTag,
	$createParagraphNode,
	$createRangeSelection,
	$createTextNode,
	$getRoot,
	$getSelection,
	$isElementNode,
	$isRangeSelection,
	$setCompositionKey,
	$setSelection,
	createEditor,
	type ElementNode,
	HISTORY_MERGE_TAG,
	type LexicalEditor,
	type LexicalNode,
	PASTE_TAG,
	REDO_COMMAND,
	UNDO_COMMAND,
} from 'lexical';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/lexical/composer/nodes/ComposerMentionPill', () => ({ComposerMentionPill: () => null}));
vi.mock('@app/features/lexical/composer/nodes/ComposerCustomEmoji', () => ({ComposerCustomEmoji: () => null}));
vi.mock('@app/features/lexical/composer/nodes/ComposerStandardEmoji', () => ({ComposerStandardEmoji: () => null}));

const ALICE: MentionSegment = {type: 'user', id: '1', displayText: '@Alice', actualText: '<@1>', start: 0, end: 6};

function specialSegment(kind: SpecialMentionKind, start: number): MentionSegment {
	return {type: 'special', id: kind, displayText: kind, actualText: kind, start, end: start + kind.length};
}

function update(editor: LexicalEditor, fn: () => void, tag?: string): void {
	editor.update(fn, {discrete: true, tag});
}

function createComposer(
	display: string,
	segments: ReadonlyArray<MentionSegment> = [],
	plainText = false,
): LexicalEditor {
	const editor = createEditor({
		namespace: 'test',
		nodes: [
			ComposerMentionNode,
			ComposerCustomEmojiNode,
			ComposerPlainSegmentNode,
			ComposerCommandNode,
			SlashSlotNode,
			SlashSlotPlaceholderNode,
			SyntaxMarkerNode,
		],
		onError: (error) => {
			throw error;
		},
	});
	update(editor, () => {
		$hydrateComposerFromDraft(display, segments, plainText);
		$selectComposerOffset(display.length);
	});
	return editor;
}

function register(editor: LexicalEditor, allowed: boolean, plainText = false): () => void {
	const unregister = registerComposerSpecialMention(editor, allowed, plainText);
	editor.read(() => undefined);
	return unregister;
}

function type(editor: LexicalEditor, text: string): void {
	for (const character of text) {
		update(editor, () => {
			const selection = $getSelection();
			if ($isRangeSelection(selection)) {
				selection.insertText(character);
			}
		});
	}
}

function insertLineBreak(editor: LexicalEditor): void {
	update(editor, () => {
		const selection = $getSelection();
		if ($isRangeSelection(selection)) {
			selection.insertLineBreak();
		}
	});
}

function paste(editor: LexicalEditor, insert: () => void): void {
	update(editor, () => {
		insert();
		$addUpdateTag(PASTE_TAG);
	});
}

function insertRawText(text: string): void {
	const selection = $getSelection();
	if ($isRangeSelection(selection)) {
		selection.insertRawText(text);
	}
}

function describeNode(node: LexicalNode): string {
	return `${node.getType()}:${node.getTextContent()}`;
}

function $paragraph(): ElementNode {
	const paragraph = $getRoot().getFirstChildOrThrow();
	assert($isElementNode(paragraph), 'Expected composer paragraph');
	return paragraph;
}

function children(editor: LexicalEditor): Array<string> {
	return editor.read(() => $paragraph().getChildren().map(describeNode));
}

function snapshot(editor: LexicalEditor) {
	return editor.read(() => ({
		children: $paragraph().getChildren().map(describeNode),
		projection: $projectComposer(),
		selection: $captureSelectionOffsets(),
	}));
}

function childrenJSON(editor: LexicalEditor) {
	return editor.read(() =>
		$paragraph()
			.getChildren()
			.map((node) => node.exportJSON()),
	);
}

describe('typed special mentions', () => {
	it('converts @everyone once a space follows it', () => {
		const editor = createComposer('');
		register(editor, true);
		type(editor, 'hi @everyone ');
		expect(snapshot(editor)).toEqual({
			children: ['text:hi ', 'composer-mention:@everyone', 'text: '],
			projection: {display: 'hi @everyone ', segments: [specialSegment('@everyone', 3)], wire: 'hi @everyone '},
			selection: {anchor: 13, focus: 13},
		});
	});

	it('produces exactly the nodes and projection the autocomplete inserts', () => {
		const typed = createComposer('');
		register(typed, true);
		type(typed, 'hi @everyone ');
		const picked = createComposer('hi @every');
		register(picked, true);
		update(picked, () => $replaceComposerRange(3, 9, createSpecialMentionPayload('@everyone')));
		expect(childrenJSON(typed)).toEqual(childrenJSON(picked));
		expect(snapshot(typed)).toEqual(snapshot(picked));
	});

	it('keeps a token at the caret as text, so @herenow never converts', () => {
		const editor = createComposer('');
		register(editor, true);
		type(editor, 'hi @here');
		expect(snapshot(editor)).toEqual({
			children: ['text:hi @here'],
			projection: {display: 'hi @here', segments: [], wire: 'hi @here'},
			selection: {anchor: 8, focus: 8},
		});
		type(editor, 'now ');
		expect(snapshot(editor)).toEqual({
			children: ['text:hi @herenow '],
			projection: {display: 'hi @herenow ', segments: [], wire: 'hi @herenow '},
			selection: {anchor: 12, focus: 12},
		});
	});

	it('converts a finished token when a line break follows it', () => {
		const editor = createComposer('');
		register(editor, true);
		type(editor, 'hi @everyone');
		insertLineBreak(editor);
		expect(snapshot(editor)).toEqual({
			children: ['text:hi ', 'composer-mention:@everyone', 'linebreak:\n'],
			projection: {display: 'hi @everyone\n', segments: [specialSegment('@everyone', 3)], wire: 'hi @everyone\n'},
			selection: {anchor: 13, focus: 13},
		});
	});

	it('does not convert when only the caret moves away, and converts on the next edit', () => {
		const editor = createComposer('');
		register(editor, true);
		type(editor, '@here');
		update(editor, () => $selectComposerOffset(0));
		expect(children(editor)).toEqual(['text:@here']);
		type(editor, ' ');
		expect(snapshot(editor)).toEqual({
			children: ['text: ', 'composer-mention:@here'],
			projection: {display: ' @here', segments: [specialSegment('@here', 1)], wire: ' @here'},
			selection: {anchor: 1, focus: 1},
		});
	});

	it('converts a token completed with the caret inside it and moves the caret past the mention', () => {
		const editor = createComposer('@hre');
		register(editor, true);
		update(editor, () => $selectComposerOffset(2));
		type(editor, 'e');
		expect(snapshot(editor)).toEqual({
			children: ['composer-mention:@here'],
			projection: {display: '@here', segments: [specialSegment('@here', 0)], wire: '@here'},
			selection: {anchor: 5, focus: 5},
		});
	});

	it('treats a preceding or following mention or emoji as a boundary', () => {
		const emoji: MentionSegment = {
			type: 'emoji',
			id: '2',
			displayText: ':wave:',
			actualText: '<:wave:2>',
			start: 0,
			end: 6,
		};
		const alice = {...ALICE, start: 6, end: 12};
		const editor = createComposer(':wave:@Alice', [emoji, alice]);
		register(editor, true);
		type(editor, '@here ');
		expect(snapshot(editor).projection).toEqual({
			display: ':wave:@Alice@here ',
			segments: [emoji, alice, specialSegment('@here', 12)],
			wire: '<:wave:2><@1>@here ',
		});
		const before = createComposer('@here@Alice', [{...ALICE, start: 5, end: 11}]);
		register(before, true);
		expect(children(before)).toEqual(['composer-mention:@here', 'composer-mention:@Alice']);
	});

	it('treats a preceding special mention pill or slash command as a boundary', () => {
		const editor = createComposer('@here', [specialSegment('@here', 0)]);
		register(editor, true);
		type(editor, '@everyone ');
		expect(snapshot(editor).projection).toEqual({
			display: '@here@everyone ',
			segments: [specialSegment('@here', 0), specialSegment('@everyone', 5)],
			wire: '@here@everyone ',
		});
		const command = createComposer('');
		update(command, () => {
			$paragraph().append($createComposerCommandNode('/giphy'), $createTextNode('@here '));
		});
		register(command, true);
		expect(children(command)).toEqual(['composer-command:/giphy', 'composer-mention:@here', 'text: ']);
	});

	it('converts a token in a second paragraph', () => {
		const editor = createComposer('a');
		register(editor, true);
		update(editor, () => {
			const paragraph = $createParagraphNode();
			paragraph.append($createTextNode('@everyone x'));
			$getRoot().append(paragraph);
		});
		expect(snapshot(editor).projection).toEqual({
			display: 'a\n@everyone x',
			segments: [specialSegment('@everyone', 2)],
			wire: 'a\n@everyone x',
		});
	});

	it('keeps the caret after a later pill when a conversion splits the text before it', () => {
		const editor = createComposer('@here @Alice', [{...ALICE, start: 6, end: 12}]);
		register(editor, true);
		expect(snapshot(editor)).toEqual({
			children: ['composer-mention:@here', 'text: ', 'composer-mention:@Alice'],
			projection: {
				display: '@here @Alice',
				segments: [specialSegment('@here', 0), {...ALICE, start: 6, end: 12}],
				wire: '@here <@1>',
			},
			selection: {anchor: 12, focus: 12},
		});
	});

	it('keeps an element caret between a pill and the text a conversion splits', () => {
		const editor = createComposer('@Alice@here x', [ALICE]);
		update(editor, () => {
			const selection = $createRangeSelection();
			selection.anchor.set($paragraph().getKey(), 1, 'element');
			selection.focus.set($paragraph().getKey(), 1, 'element');
			$setSelection(selection);
		});
		register(editor, true);
		expect(snapshot(editor)).toEqual({
			children: ['composer-mention:@Alice', 'composer-mention:@here', 'text: x'],
			projection: {
				display: '@Alice@here x',
				segments: [ALICE, specialSegment('@here', 6)],
				wire: '<@1>@here x',
			},
			selection: {anchor: 6, focus: 6},
		});
	});

	it('gives the conversion one undo entry that restores the text before the keystroke, and redo restores it', () => {
		const editor = createComposer('');
		const history = createEmptyHistoryState();
		registerHistory(editor, history, 1000);
		register(editor, true);
		type(editor, 'hi @everyone');
		const depth = history.undoStack.length;
		type(editor, ' ');
		expect(history.undoStack).toHaveLength(depth + 1);
		editor.dispatchCommand(UNDO_COMMAND, undefined);
		expect(snapshot(editor)).toEqual({
			children: ['text:hi @everyone'],
			projection: {display: 'hi @everyone', segments: [], wire: 'hi @everyone'},
			selection: {anchor: 12, focus: 12},
		});
		editor.dispatchCommand(REDO_COMMAND, undefined);
		expect(snapshot(editor)).toEqual({
			children: ['text:hi ', 'composer-mention:@everyone', 'text: '],
			projection: {display: 'hi @everyone ', segments: [specialSegment('@everyone', 3)], wire: 'hi @everyone '},
			selection: {anchor: 13, focus: 13},
		});
	});

	it('leaves a token at the caret alone when the mention menu opens or closes', () => {
		const editor = createComposer('');
		const history = createEmptyHistoryState();
		registerHistory(editor, history, 1000);
		register(editor, true);
		type(editor, 'hi @here');
		const depth = history.undoStack.length;
		update(editor, () => $getRoot().markDirty(), HISTORY_MERGE_TAG);
		expect(children(editor)).toEqual(['text:hi @here']);
		expect(history.undoStack).toHaveLength(depth);
	});

	it('converts a token at the caret when the transform is registered, without adding an undo entry', () => {
		const editor = createComposer('hi @here');
		const history = createEmptyHistoryState();
		registerHistory(editor, history, 1000);
		const depth = history.undoStack.length;
		register(editor, true);
		expect(children(editor)).toEqual(['text:hi ', 'composer-mention:@here']);
		expect(history.undoStack).toHaveLength(depth);
	});

	it('converts a hydrated draft whose token ends at the caret without resetting the history', () => {
		const editor = createComposer('');
		register(editor, true);
		update(
			editor,
			() => {
				$hydrateComposerFromDraft('hi @everyone', []);
				$selectComposerOffset(12);
			},
			COMPOSER_RESCAN_TAG,
		);
		expect(snapshot(editor)).toEqual({
			children: ['text:hi ', 'composer-mention:@everyone'],
			projection: {display: 'hi @everyone', segments: [specialSegment('@everyone', 3)], wire: 'hi @everyone'},
			selection: {anchor: 12, focus: 12},
		});
	});

	it('converts a token at the caret after a render mode switch', () => {
		const editor = createComposer('');
		const unregister = register(editor, true);
		type(editor, '@here');
		unregister();
		update(
			editor,
			() => {
				const selection = $captureSelectionOffsets()!;
				const projection = $projectComposer();
				$hydrateComposerFromDraft(projection.display, projection.segments, true);
				$selectComposerRange(selection.anchor, selection.focus);
			},
			'composer-render-mode',
		);
		resetComposerHistory(editor);
		registerComposerPlainText(editor);
		register(editor, true, true);
		expect(snapshot(editor)).toEqual({
			children: ['composer-plain-segment:@here'],
			projection: {display: '@here', segments: [specialSegment('@here', 0)], wire: '@here'},
			selection: {anchor: 5, focus: 5},
		});
	});

	it('converts to the plain segment the autocomplete inserts in plain text mode', () => {
		const typed = createComposer('', [], true);
		registerComposerPlainText(typed);
		register(typed, true, true);
		type(typed, 'hi @everyone ');
		const picked = createComposer('hi @every', [], true);
		registerComposerPlainText(picked);
		register(picked, true, true);
		update(picked, () => $replaceComposerRange(3, 9, createSpecialMentionPayload('@everyone'), undefined, true));
		expect(childrenJSON(typed)).toEqual(childrenJSON(picked));
		expect(snapshot(typed)).toEqual({
			children: ['text:hi ', 'composer-plain-segment:@everyone', 'text: '],
			projection: {display: 'hi @everyone ', segments: [specialSegment('@everyone', 3)], wire: 'hi @everyone '},
			selection: {anchor: 13, focus: 13},
		});
	});

	it('converts a pasted token at the caret and several pasted tokens at once', () => {
		const tail = createComposer('ping ');
		register(tail, true);
		paste(tail, () => insertRawText('@here'));
		expect(snapshot(tail)).toEqual({
			children: ['text:ping ', 'composer-mention:@here'],
			projection: {display: 'ping @here', segments: [specialSegment('@here', 5)], wire: 'ping @here'},
			selection: {anchor: 10, focus: 10},
		});
		const several = createComposer('');
		register(several, true);
		paste(several, () => insertRawText('@everyone and @here'));
		expect(snapshot(several)).toEqual({
			children: ['composer-mention:@everyone', 'text: and ', 'composer-mention:@here'],
			projection: {
				display: '@everyone and @here',
				segments: [specialSegment('@everyone', 0), specialSegment('@here', 14)],
				wire: '@everyone and @here',
			},
			selection: {anchor: 19, focus: 19},
		});
	});

	it('converts typed tokens beside wire mentions in a channel paste slice', () => {
		const editor = createComposer('hi ');
		register(editor, true);
		paste(editor, () => $insertComposerClipboardSlice({display: '@Alice @everyone', segments: [ALICE]}, false));
		expect(snapshot(editor)).toEqual({
			children: ['text:hi ', 'composer-mention:@Alice', 'text: ', 'composer-mention:@everyone'],
			projection: {
				display: 'hi @Alice @everyone',
				segments: [{...ALICE, start: 3, end: 9}, specialSegment('@everyone', 10)],
				wire: 'hi <@1> @everyone',
			},
			selection: {anchor: 19, focus: 19},
		});
	});

	it('gives a channel paste its own undo entry once it carries the paste tag', () => {
		const editor = createComposer('');
		const history = createEmptyHistoryState();
		registerHistory(editor, history, 1000);
		register(editor, true);
		type(editor, 'a');
		paste(editor, () => $insertComposerClipboardSlice({display: 'b', segments: []}, false));
		type(editor, 'c');
		editor.dispatchCommand(UNDO_COMMAND, undefined);
		expect(editor.read(() => $projectComposer().display)).toBe('ab');
		editor.dispatchCommand(UNDO_COMMAND, undefined);
		expect(editor.read(() => $projectComposer().display)).toBe('a');
	});

	it('waits for IME composition to end before converting', () => {
		const editor = createComposer('hi ');
		register(editor, true);
		update(editor, () => {
			const selection = $getSelection();
			if ($isRangeSelection(selection)) {
				selection.insertText('@everyone ');
				$setCompositionKey(selection.anchor.key);
			}
		});
		expect(children(editor)).toEqual(['text:hi @everyone ']);
		update(editor, () => $setCompositionKey(null));
		expect(children(editor)).toEqual(['text:hi ', 'composer-mention:@everyone', 'text: ']);
	});

	it('leaves code formatted text and slash command slot text alone', () => {
		const editor = createComposer('');
		register(editor, true);
		update(editor, () => {
			const code = $createTextNode('@everyone ').toggleFormat('code');
			const slot = $createSlashSlotNode('message', 'string', true).append($createTextNode('@here '));
			$paragraph().append(code, slot);
			code.select(10, 10);
		});
		expect(editor.read(() => $paragraph().getChildren().map(describeNode))).toEqual([
			'text:@everyone ',
			'slash-slot:@here ',
		]);
		expect(
			editor.read(() => {
				const slot = $paragraph().getLastChildOrThrow();
				assert($isElementNode(slot), 'Expected slash command slot element');
				return slot.getChildren().map(describeNode);
			}),
		).toEqual(['text:@here ']);
	});

	it('restores a draft special mention and projects it back unchanged', () => {
		const segments = [specialSegment('@everyone', 0)];
		const editor = createComposer('@everyone hi', segments);
		expect(snapshot(editor)).toEqual({
			children: ['composer-mention:@everyone', 'text: hi'],
			projection: {display: '@everyone hi', segments, wire: '@everyone hi'},
			selection: {anchor: 12, focus: 12},
		});
	});

	it('includes the special segment in the clipboard selection', () => {
		const editor = createComposer('');
		register(editor, true);
		type(editor, 'hi @everyone ');
		update(editor, () => $selectComposerRange(0, 13));
		expect(editor.read(() => $getComposerClipboardSelection())).toEqual({
			display: 'hi @everyone ',
			segments: [specialSegment('@everyone', 3)],
			textPlain: 'hi @everyone ',
		});
	});
});

describe('typed special mentions with the markdown highlight', () => {
	function createHighlightedComposer(): LexicalEditor {
		const editor = createComposer('');
		registerComposerMarkdownHighlight(editor);
		register(editor, true);
		return editor;
	}

	it.each([
		['an escaped token', '\\@everyone '],
		['closed inline code', '`@everyone` '],
		['open inline code', '`@everyone '],
		['a url', 'https://fluxer.app/@everyone '],
	])('leaves %s as text', (_name, text) => {
		const editor = createHighlightedComposer();
		type(editor, text);
		expect(snapshot(editor).projection).toEqual({display: text, segments: [], wire: text});
	});

	it('leaves a token inside a fenced code block as text and converts one after it', () => {
		const editor = createHighlightedComposer();
		type(editor, '```');
		insertLineBreak(editor);
		type(editor, '@everyone ');
		insertLineBreak(editor);
		type(editor, '```');
		insertLineBreak(editor);
		type(editor, '@here ');
		expect(snapshot(editor).projection).toEqual({
			display: '```\n@everyone \n```\n@here ',
			segments: [specialSegment('@here', 19)],
			wire: '```\n@everyone \n```\n@here ',
		});
	});

	it('converts a bold token into a pill that is not literal', () => {
		const editor = createHighlightedComposer();
		type(editor, '**@everyone** ');
		expect(snapshot(editor).projection).toEqual({
			display: '**@everyone** ',
			segments: [specialSegment('@everyone', 2)],
			wire: '**@everyone** ',
		});
		expect(
			editor.read(() =>
				$paragraph()
					.getChildren()
					.filter($isComposerMentionNode)
					.map((node) => node.isLiteral()),
			),
		).toEqual([false]);
	});
});

describe('special mentions where they are not allowed', () => {
	it('turns a restored special mention into text', () => {
		const editor = createComposer('@everyone hi', [specialSegment('@everyone', 0)]);
		register(editor, false);
		expect(snapshot(editor)).toEqual({
			children: ['text:@everyone hi'],
			projection: {display: '@everyone hi', segments: [], wire: '@everyone hi'},
			selection: {anchor: 12, focus: 12},
		});
	});

	it('turns a pasted special mention into text', () => {
		const editor = createComposer('hi ');
		register(editor, false);
		paste(editor, () =>
			$insertComposerClipboardSlice({display: '@here', segments: [specialSegment('@here', 0)]}, false),
		);
		expect(snapshot(editor)).toEqual({
			children: ['text:hi @here'],
			projection: {display: 'hi @here', segments: [], wire: 'hi @here'},
			selection: {anchor: 8, focus: 8},
		});
	});

	it('turns a restored special mention into text in plain text mode', () => {
		const editor = createComposer('@everyone hi', [specialSegment('@everyone', 0)], true);
		registerComposerPlainText(editor);
		register(editor, false, true);
		expect(snapshot(editor)).toEqual({
			children: ['text:@everyone hi'],
			projection: {display: '@everyone hi', segments: [], wire: '@everyone hi'},
			selection: {anchor: 12, focus: 12},
		});
	});

	it('turns a pasted special mention into text in plain text mode', () => {
		const editor = createComposer('hi ', [], true);
		registerComposerPlainText(editor);
		register(editor, false, true);
		paste(editor, () =>
			$insertComposerClipboardSlice({display: '@here', segments: [specialSegment('@here', 0)]}, true),
		);
		expect(snapshot(editor)).toEqual({
			children: ['text:hi @here'],
			projection: {display: 'hi @here', segments: [], wire: 'hi @here'},
			selection: {anchor: 8, focus: 8},
		});
	});

	it('leaves typed tokens as text, then converts them once special mentions become allowed', () => {
		const editor = createComposer('');
		const unregister = register(editor, false);
		type(editor, '@here ');
		expect(children(editor)).toEqual(['text:@here ']);
		unregister();
		register(editor, true);
		expect(children(editor)).toEqual(['composer-mention:@here', 'text: ']);
	});
});

describe('isValidComposerSegment', () => {
	it('accepts the special mention ids the autocomplete writes', () => {
		expect(isValidComposerSegment('@everyone hi', specialSegment('@everyone', 0))).toBe(true);
		expect(isValidComposerSegment('hi @here', specialSegment('@here', 3))).toBe(true);
	});

	it('rejects the bare id form and a mismatched id', () => {
		expect(isValidComposerSegment('@everyone hi', {...specialSegment('@everyone', 0), id: 'everyone'})).toBe(false);
		expect(isValidComposerSegment('@everyone hi', {...specialSegment('@everyone', 0), id: '@here'})).toBe(false);
	});
});
