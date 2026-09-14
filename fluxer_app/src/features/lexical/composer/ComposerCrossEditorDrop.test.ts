// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {registerComposerBlockquote} from '@app/features/lexical/composer/ComposerBlockquote';
import {registerComposerClipboardCommands} from '@app/features/lexical/composer/ComposerClipboard';
import {registerComposerMarkdownHighlight} from '@app/features/lexical/composer/ComposerMarkdownHighlight';
import {
	$hydrateComposerFromDraft,
	$projectComposer,
	type ComposerProjection,
} from '@app/features/lexical/composer/ComposerSerialization';
import {$selectComposerRange} from '@app/features/lexical/composer/composerOffsets';
import {DEFAULT_COMPOSER_MARKDOWN_FLAGS} from '@app/features/lexical/composer/markdownSpans';
import {ComposerBlockquoteLineNode} from '@app/features/lexical/composer/nodes/ComposerBlockquoteLineNode';
import {ComposerBlockquoteMarkerNode} from '@app/features/lexical/composer/nodes/ComposerBlockquoteMarkerNode';
import {ComposerCommandNode} from '@app/features/lexical/composer/nodes/ComposerCommandNode';
import {ComposerCustomEmojiNode} from '@app/features/lexical/composer/nodes/ComposerCustomEmojiNode';
import {ComposerMentionNode} from '@app/features/lexical/composer/nodes/ComposerMentionNode';
import {ComposerPlainSegmentNode} from '@app/features/lexical/composer/nodes/ComposerPlainSegmentNode';
import {ComposerStandardEmojiNode} from '@app/features/lexical/composer/nodes/ComposerStandardEmojiNode';
import {SlashOptionalHintNode} from '@app/features/lexical/composer/nodes/SlashOptionalHintNode';
import {SlashSeparatorNode} from '@app/features/lexical/composer/nodes/SlashSeparatorNode';
import {SlashSlotNode} from '@app/features/lexical/composer/nodes/SlashSlotNode';
import {SlashSlotPlaceholderNode} from '@app/features/lexical/composer/nodes/SlashSlotPlaceholderNode';
import {SyntaxMarkerNode} from '@app/features/lexical/composer/nodes/SyntaxMarkerNode';
import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';
import {
	$getSelection,
	$isRangeSelection,
	COMMAND_PRIORITY_EDITOR,
	createEditor,
	DRAGSTART_COMMAND,
	DROP_COMMAND,
	type LexicalEditor,
	REMOVE_TEXT_COMMAND,
} from 'lexical';
import {afterEach, describe, expect, it, vi} from 'vitest';

vi.hoisted(() => {
	if (!('getTargetRanges' in InputEvent.prototype)) {
		Object.defineProperty(InputEvent.prototype, 'getTargetRanges', {configurable: true, value: () => []});
	}
});

vi.mock('@app/features/lexical/composer/nodes/ComposerMentionPill', () => ({ComposerMentionPill: () => null}));
vi.mock('@app/features/lexical/composer/nodes/ComposerCustomEmoji', () => ({ComposerCustomEmoji: () => null}));
vi.mock('@app/features/lexical/composer/nodes/ComposerStandardEmoji', () => ({ComposerStandardEmoji: () => null}));
vi.mock('@app/features/lexical/composer/nodes/SlashOptionalHintPill', () => ({SlashOptionalHintPill: () => null}));
vi.mock('@app/features/expressions/utils/EmojiUtils', () => ({getEmojiURL: () => null}));
vi.mock('@lingui/core/macro', () => ({msg: (descriptor: unknown) => descriptor}));

const dropOffset = vi.hoisted(() => ({value: null as number | null}));

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

const LEXICAL_DRAG_MIME = 'application/x-lexical-drag';

interface MountedComposer {
	editor: LexicalEditor;
	element: HTMLDivElement;
	removals: {count: number};
}

const disposers: Array<() => void> = [];

afterEach(() => {
	while (disposers.length > 0) {
		disposers.pop()!();
	}
	dropOffset.value = null;
	document.body.replaceChildren();
});

function mount(
	text: string,
	anchor: number,
	focus: number,
	segments: ReadonlyArray<MentionSegment> = [],
): MountedComposer {
	const element = document.createElement('div');
	element.contentEditable = 'true';
	document.body.append(element);
	const editor = createEditor({
		namespace: 'composer-cross-editor-drop-test',
		nodes: NODES,
		onError: (error) => {
			throw error;
		},
	});
	editor.setRootElement(element);
	const removals = {count: 0};
	disposers.push(
		() => editor.setRootElement(null),
		registerComposerMarkdownHighlight(editor, DEFAULT_COMPOSER_MARKDOWN_FLAGS, false),
		registerComposerBlockquote(editor, DEFAULT_COMPOSER_MARKDOWN_FLAGS),
		registerComposerClipboardCommands(editor, {
			getPlainText: () => false,
			isEditable: () => true,
			getMarkdownParserFlags: () => DEFAULT_COMPOSER_MARKDOWN_FLAGS,
		}),
		editor.registerCommand(
			REMOVE_TEXT_COMMAND,
			() => {
				const selection = $getSelection();
				if (!$isRangeSelection(selection)) {
					return false;
				}
				removals.count += 1;
				selection.removeText();
				return true;
			},
			COMMAND_PRIORITY_EDITOR,
		),
	);
	editor.update(
		() => {
			$hydrateComposerFromDraft(text, segments);
			$selectComposerRange(anchor, focus);
		},
		{discrete: true},
	);
	return {editor, element, removals};
}

function projection(composer: MountedComposer): ComposerProjection {
	return composer.editor.read(() => $projectComposer());
}

function dragStart(source: MountedComposer, moved: string): Record<string, string> {
	const data: Record<string, string> = {};
	const dataTransfer = {
		getData: (type: string) => data[type] ?? '',
		setData: (type: string, value: string) => {
			data[type] = value;
		},
	} as unknown as DataTransfer;
	source.editor.update(
		() => {
			source.editor.dispatchCommand(DRAGSTART_COMMAND, {dataTransfer} as unknown as DragEvent);
		},
		{discrete: true},
	);
	data['text/plain'] = moved;
	data[LEXICAL_DRAG_MIME] = JSON.stringify({editorKey: source.editor.getKey()});
	return data;
}

function drop(target: MountedComposer, data: Record<string, string>, offset: number): boolean {
	dropOffset.value = offset;
	const event = {
		clientX: 0,
		clientY: 0,
		dataTransfer: {getData: (type: string) => data[type] ?? ''} as unknown as DataTransfer,
		preventDefault: vi.fn(),
	};
	let handled = false;
	target.editor.update(
		() => {
			handled = target.editor.dispatchCommand(DROP_COMMAND, event as unknown as DragEvent);
		},
		{discrete: true},
	);
	return handled && event.preventDefault.mock.calls.length > 0;
}

describe('drops carrying another composer as their drag source', () => {
	it('continues the quote in the target and removes the dragged range from the source once', () => {
		const target = mount('> alpha', 2, 2);
		const source = mount('one\ntwo keep', 0, 7);
		expect(drop(target, dragStart(source, 'one\ntwo'), 2)).toBe(true);
		expect(projection(target).wire).toBe('> one\n> twoalpha');
		expect(projection(source).wire).toBe(' keep');
		expect(source.removals.count).toBe(1);
		expect(target.removals.count).toBe(0);
	});

	it('keeps a mention dragged out of another composer a mention', () => {
		const target = mount('> quoted', 2, 2);
		const source = mount('hi @name', 3, 8, [
			{type: 'user', id: '1', displayText: '@name', actualText: '<@1>', start: 3, end: 8},
		]);
		expect(drop(target, dragStart(source, '@name'), 2)).toBe(true);
		expect(projection(target).wire).toBe('> <@1>quoted');
		expect(projection(target).segments).toMatchObject([{type: 'user', id: '1', start: 2, end: 7}]);
		expect(projection(source).wire).toBe('hi ');
		expect(source.removals.count).toBe(1);
	});
});
