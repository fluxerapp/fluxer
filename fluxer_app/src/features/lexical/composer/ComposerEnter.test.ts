// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {registerComposerEnter} from '@app/features/lexical/composer/ComposerEnter';
import {$replaceComposerRange, $selectComposerRange} from '@app/features/lexical/composer/composerOffsets';
import {ComposerMentionNode} from '@app/features/lexical/composer/nodes/ComposerMentionNode';
import {ComposerPlainSegmentNode} from '@app/features/lexical/composer/nodes/ComposerPlainSegmentNode';
import {
	$createSlashSlotNode,
	$isSlashSlotNode,
	SlashSlotNode,
} from '@app/features/lexical/composer/nodes/SlashSlotNode';
import {SlashSlotPlaceholderNode} from '@app/features/lexical/composer/nodes/SlashSlotPlaceholderNode';
import {$getRoot, $isElementNode, $setSelection, createEditor, KEY_ENTER_COMMAND, type LexicalEditor} from 'lexical';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/lexical/composer/nodes/ComposerMentionPill', () => ({ComposerMentionPill: () => null}));
vi.mock('@app/features/lexical/composer/nodes/ComposerCustomEmoji', () => ({ComposerCustomEmoji: () => null}));
vi.mock('@app/features/lexical/composer/nodes/ComposerStandardEmoji', () => ({ComposerStandardEmoji: () => null}));

interface EnterOptions {
	modifiers?: Partial<Pick<KeyboardEvent, 'altKey' | 'ctrlKey' | 'metaKey' | 'shiftKey'>>;
	submitOnEnter?: boolean;
	typeaheadActive?: boolean;
}

const NEWLINE = {handled: false, prevented: false, sent: 0};
const SENT = {handled: true, prevented: true, sent: 1};

function createComposer(seed: () => void): LexicalEditor {
	const editor = createEditor({
		namespace: 'composer-enter-test',
		nodes: [ComposerMentionNode, ComposerPlainSegmentNode, SlashSlotNode, SlashSlotPlaceholderNode],
		onError: (error) => {
			throw error;
		},
	});
	editor.update(seed, {discrete: true});
	return editor;
}

function typed(text: string, anchor: number, focus = anchor): LexicalEditor {
	return createComposer(() => {
		$replaceComposerRange(0, 0, {kind: 'text', text}, {leading: false, trailing: false});
		$selectComposerRange(anchor, focus);
	});
}

function typedWithRequiredSlot(text: string, offset: number): LexicalEditor {
	return createComposer(() => {
		$replaceComposerRange(0, 0, {kind: 'text', text}, {leading: false, trailing: false});
		const slot = $createSlashSlotNode('value', 'string', true);
		const paragraph = $getRoot().getFirstChildOrThrow();
		assert($isElementNode(paragraph), 'Expected composer paragraph');
		paragraph.append(slot);
		slot.ensurePlaceholder();
		$selectComposerRange(offset, offset);
	});
}

function slotValidity(editor: LexicalEditor): string {
	return editor.read(() => {
		const paragraph = $getRoot().getFirstChildOrThrow();
		assert($isElementNode(paragraph), 'Expected composer paragraph');
		const slot = paragraph.getLastChildOrThrow();
		assert($isSlashSlotNode(slot), 'Expected required slash command slot');
		return slot.getValidity();
	});
}

function pressEnter(
	editor: LexicalEditor,
	{modifiers = {}, submitOnEnter = true, typeaheadActive = false}: EnterOptions = {},
): typeof SENT {
	const onEnter = vi.fn();
	registerComposerEnter(editor, {
		typeaheadActiveState: {current: typeaheadActive},
		getSubmitOnEnter: () => submitOnEnter,
		getOnEnter: () => onEnter,
	});
	const event = Object.assign(
		new Event('keydown', {cancelable: true}),
		{altKey: false, ctrlKey: false, metaKey: false, shiftKey: false},
		modifiers,
	) as KeyboardEvent;
	const handled = editor.dispatchCommand(KEY_ENTER_COMMAND, event);
	return {handled, prevented: event.defaultPrevented, sent: onEnter.mock.calls.length};
}

describe('registerComposerEnter', () => {
	it.each([
		['after a lone opening fence', '```\ncode', 8],
		['on the empty line after an opening fence', '```\n', 4],
		['right before the closing fence', '```\ncode\n```', 9],
	])('inserts a newline %s without preventing the key', (_label, text, offset) => {
		expect(pressEnter(typed(text, offset))).toEqual(NEWLINE);
	});

	it('sends after the closing fence', () => {
		expect(pressEnter(typed('```\ncode\n```', 12))).toEqual(SENT);
	});

	it('still sends plain text', () => {
		expect(pressEnter(typed('hello', 5))).toEqual(SENT);
	});

	it('still sends on Ctrl+Enter inside a block', () => {
		expect(pressEnter(typed('```\ncode', 8), {modifiers: {ctrlKey: true}})).toEqual(SENT);
	});

	it('still sends on Cmd+Enter inside a block', () => {
		expect(pressEnter(typed('```\ncode', 8), {modifiers: {metaKey: true}})).toEqual(SENT);
	});

	it('inserts a newline on Alt+Enter inside a block', () => {
		expect(pressEnter(typed('```\ncode', 8), {modifiers: {altKey: true}})).toEqual(NEWLINE);
	});

	it('still leaves Shift+Enter to the line break', () => {
		expect(pressEnter(typed('hello', 5), {modifiers: {shiftKey: true}})).toEqual(NEWLINE);
	});

	it('still leaves Shift+Enter to the line break inside a block', () => {
		expect(pressEnter(typed('```\ncode', 8), {modifiers: {shiftKey: true}})).toEqual(NEWLINE);
	});

	it('sends on a list item line that holds a fence', () => {
		expect(pressEnter(typed('- ```', 5))).toEqual(SENT);
	});

	it('sends in prose that holds an unmatched fence', () => {
		expect(pressEnter(typed('use ``` for code blocks', 23))).toEqual(SENT);
	});

	it('decides on the start of a backward selection inside a block', () => {
		expect(pressEnter(typed('```\ncode', 7, 5))).toEqual(NEWLINE);
	});

	it('sends when the selection starts before the opening fence', () => {
		expect(pressEnter(typed('```\ncode', 0, 8))).toEqual(SENT);
	});

	it('sends when nothing is selected', () => {
		const editor = createComposer(() => {
			$replaceComposerRange(0, 0, {kind: 'text', text: '```\ncode'}, {leading: false, trailing: false});
			$setSelection(null);
		});
		expect(pressEnter(editor)).toEqual(SENT);
	});

	it('still lets an open typeahead take Enter inside a block', () => {
		expect(pressEnter(typed('```\ncode', 8), {typeaheadActive: true})).toEqual(NEWLINE);
	});

	it('still inserts a newline on plain Enter when submit on Enter is off', () => {
		expect(pressEnter(typed('```\ncode', 8), {submitOnEnter: false})).toEqual(NEWLINE);
	});

	it('still sends on Ctrl+Enter inside a block when submit on Enter is off', () => {
		expect(pressEnter(typed('```\ncode', 8), {submitOnEnter: false, modifiers: {ctrlKey: true}})).toEqual(SENT);
	});

	it('inserts a newline inside a block without flagging an empty required slash slot', () => {
		const editor = typedWithRequiredSlot('```\ncode', 8);
		expect(pressEnter(editor)).toEqual(NEWLINE);
		expect(slotValidity(editor)).toBe('neutral');
	});

	it('still flags an empty required slash slot instead of sending outside a block', () => {
		const editor = typedWithRequiredSlot('hello', 5);
		expect(pressEnter(editor)).toEqual({handled: true, prevented: true, sent: 0});
		expect(slotValidity(editor)).toBe('invalid');
	});

	it.each([
		['a mention', false],
		['a plain text mention', true],
	])('reads %s as filler so a backtick in its name cannot hide a fence', (_label, plainText) => {
		const editor = createComposer(() => {
			$replaceComposerRange(
				0,
				0,
				{kind: 'mention', mentionType: 'user', id: '1', display: '@`bob', wire: '<@1>'},
				{trailing: false},
				plainText,
			);
			$replaceComposerRange(5, 5, {kind: 'text', text: ' hi ```js'}, {leading: false, trailing: false});
			$selectComposerRange(14, 14);
		});
		expect(pressEnter(editor)).toEqual(NEWLINE);
	});
});
