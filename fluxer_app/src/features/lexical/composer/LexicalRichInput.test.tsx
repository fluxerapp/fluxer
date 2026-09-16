// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {LexicalRichInput, type LexicalRichInputHandle} from '@app/features/lexical/composer/LexicalRichInput';
import type {I18n} from '@lingui/core';
import {act, createElement, createRef} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const stub = vi.hoisted(() => ({
	handle: {
		getDisplayValue: () => '',
		getSegments: () => [],
		getSelection: () => ({start: 0, end: 0}),
		getMarkdownParserFlags: () => 0,
		replaceRange: () => undefined,
	},
	composerMaxWireLength: undefined as number | undefined,
	autocompleteMaxActualLength: undefined as number | undefined,
	emojiLimits: [] as Array<{maxWireLength?: number}>,
}));

vi.mock('@app/features/lexical/composer/LexicalComposerInput', () => ({
	LexicalComposerInput: (props: {maxWireLength?: number; handleRef: {current: unknown}}) => {
		stub.composerMaxWireLength = props.maxWireLength;
		props.handleRef.current = stub.handle;
		return null;
	},
}));

vi.mock('@app/features/lexical/composer/useLexicalAutocomplete', () => ({
	useLexicalAutocomplete: (params: {maxActualLength?: number}) => {
		stub.autocompleteMaxActualLength = params.maxActualLength;
		return {
			autocompleteQuery: '',
			autocompleteOptions: [],
			autocompleteType: null,
			isSlotMenu: false,
			onCursorMove: () => undefined,
			handleSelect: () => undefined,
			specialMentionsAllowed: true,
		};
	},
}));

vi.mock('@app/features/lexical/composer/ComposerInsertion', () => ({
	insertComposerEmoji: (_handle: unknown, _emoji: unknown, limit: {maxWireLength?: number}) => {
		stub.emojiLimits.push(limit);
		return true;
	},
}));

vi.mock('@app/features/messaging/utils/TypedEmojiShortcodeUtils', () => ({resolveTypedEmojiToken: () => null}));

(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;

const I18N = {} as I18n;
const EMOJI = {} as FlatEmoji;

describe('LexicalRichInput length limits', () => {
	let host: HTMLDivElement;
	let root: Root;

	beforeEach(() => {
		stub.composerMaxWireLength = undefined;
		stub.autocompleteMaxActualLength = undefined;
		stub.emojiLimits.length = 0;
		host = document.createElement('div');
		document.body.append(host);
		root = createRoot(host);
	});

	afterEach(() => {
		act(() => {
			root.unmount();
		});
		document.body.replaceChildren();
	});

	it.each<[string, {maxLength?: number; maxWireLength?: number}, number, number | undefined]>([
		['only maxWireLength', {maxWireLength: 40}, 40, undefined],
		['only maxLength', {maxLength: 40}, 40, 40],
		['both', {maxLength: 30, maxWireLength: 40}, 40, 30],
	])('budgets the composer by maxWireLength and caps autocomplete and emoji inserts by maxLength when a host passes %s', (_name, props, composerLimit, insertLimit) => {
		const richInputRef = createRef<LexicalRichInputHandle>();
		act(() => {
			root.render(createElement(LexicalRichInput, {placeholder: 'placeholder', i18n: I18N, richInputRef, ...props}));
		});
		expect(stub.composerMaxWireLength).toBe(composerLimit);
		expect(stub.autocompleteMaxActualLength).toBe(insertLimit);
		act(() => {
			richInputRef.current?.insertEmoji(EMOJI);
		});
		expect(stub.emojiLimits).toEqual([{maxWireLength: insertLimit, onExceedMaxLength: undefined}]);
	});
});
