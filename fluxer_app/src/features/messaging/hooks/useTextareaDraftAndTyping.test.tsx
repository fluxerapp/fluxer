// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {act, createElement, useRef, useState} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const doubles = vi.hoisted(() => ({
	post: vi.fn((_path: string) => Promise.resolve({ok: true})),
}));

vi.mock('@app/features/messaging/commands/DraftCommands', () => ({createDraft: vi.fn(), deleteDraft: vi.fn()}));
vi.mock('@app/features/platform/utils/AppLogger', () => ({
	Logger: class {
		debug = vi.fn();
		info = vi.fn();
		warn = vi.fn();
		error = vi.fn();
	},
}));
vi.mock('@app/features/platform/transport/RestTransport', () => ({http: {get: vi.fn(), post: doubles.post}}));
vi.mock('@app/features/auth/state/Authentication', () => ({default: {currentUserId: 'me'}}));
vi.mock('@app/features/devtools/state/DeveloperOptions', () => ({default: {showMyselfTyping: false}}));
vi.mock('@app/features/relationship/state/Relationships', () => ({default: {isBlocked: () => false}}));
vi.mock('@app/features/user/state/Users', () => ({default: {getUser: () => undefined}}));

const {useTextareaDraftAndTyping} = await import('@app/features/messaging/hooks/useTextareaDraftAndTyping');
const {default: RollingTypingSender} = await import('@app/features/typing/rolling/RollingTypingSender');
const {default: RollingTypingStore} = await import('@app/features/typing/rolling/RollingTypingStore');

interface ComposerProps {
	channelId: string;
	draft?: string | null;
	enabled?: boolean;
	typingEnabled?: boolean;
	isEditingMessageInComposer?: boolean;
}

let host: HTMLDivElement;
let root: Root;
let setComposerValue: (value: string) => void = () => undefined;

function Composer({
	channelId,
	draft = null,
	enabled = true,
	typingEnabled,
	isEditingMessageInComposer = false,
}: ComposerProps) {
	const [value, setValue] = useState('');
	const previousValueRef = useRef('');
	setComposerValue = setValue;
	useTextareaDraftAndTyping({
		channelId,
		value,
		setValue,
		draft,
		previousValueRef,
		enabled,
		typingEnabled,
		isEditingMessageInComposer,
	});
	return null;
}

function render(props: ComposerProps): void {
	act(() => {
		root.render(createElement(Composer, props));
	});
}

function type(value: string): void {
	act(() => {
		setComposerValue(value);
	});
}

function advance(ms: number): void {
	act(() => {
		vi.advanceTimersByTime(ms);
	});
}

beforeEach(() => {
	(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;
	vi.useFakeTimers();
	host = document.createElement('div');
	document.body.append(host);
	root = createRoot(host);
});

afterEach(() => {
	act(() => {
		root.unmount();
	});
	RollingTypingSender.reset();
	RollingTypingStore.reset();
	document.body.replaceChildren();
	vi.useRealTimers();
	vi.clearAllMocks();
});

describe('useTextareaDraftAndTyping', () => {
	it('sends typing once for a burst of keystrokes', () => {
		render({channelId: 'c1'});

		let draft = '';
		for (const character of 'hello there, how are you') {
			draft += character;
			type(draft);
			advance(150);
		}
		advance(3000);

		expect(doubles.post).toHaveBeenCalledExactlyOnceWith(Endpoints.CHANNEL_TYPING('c1'));
	});

	it('never sends typing when entering mobile edit mode', () => {
		render({channelId: 'c1'});

		act(() => {
			root.render(createElement(Composer, {channelId: 'c1', isEditingMessageInComposer: true}));
			setComposerValue('the message being edited');
		});
		type('the message being edited again');
		advance(20000);
		render({channelId: 'c1', isEditingMessageInComposer: false});
		render({channelId: 'c1', isEditingMessageInComposer: true});
		type('a later edit keystroke');
		advance(20000);

		expect(doubles.post).not.toHaveBeenCalled();
		expect(RollingTypingStore.isTyping('c1', 'me')).toBe(false);
	});

	it('sends nothing for a draft present at mount', () => {
		render({channelId: 'c1', draft: 'saved draft'});
		advance(0);
		render({channelId: 'c1', draft: 'saved draft', typingEnabled: false});
		render({channelId: 'c1', draft: 'saved draft', typingEnabled: true});
		advance(20000);

		expect(doubles.post).not.toHaveBeenCalled();
	});

	it('sends nothing for a draft restored after mount', () => {
		render({channelId: 'c1'});
		advance(1000);

		render({channelId: 'c1', draft: 'synced from another device'});
		advance(0);
		render({channelId: 'c1', draft: 'synced from another device', typingEnabled: false});
		render({channelId: 'c1', draft: 'synced from another device', typingEnabled: true});
		advance(20000);

		expect(doubles.post).not.toHaveBeenCalled();
	});

	it('does not cancel a pending send when the channel changes', () => {
		render({channelId: 'c1'});
		type('hello');
		advance(500);

		render({channelId: 'c2'});
		advance(1000);

		expect(doubles.post).toHaveBeenCalledExactlyOnceWith(Endpoints.CHANNEL_TYPING('c1'));
		advance(20000);
		expect(doubles.post).toHaveBeenCalledTimes(1);
	});

	it('cancels a pending send when the composer becomes disabled', () => {
		render({channelId: 'c1'});
		type('hello');
		advance(500);

		render({channelId: 'c1', enabled: false});
		advance(20000);

		expect(doubles.post).not.toHaveBeenCalled();
		expect(RollingTypingStore.isTyping('c1', 'me')).toBe(false);
	});

	it('posts nothing for a spaces-only draft', () => {
		render({channelId: 'c1'});

		type(' ');
		type('   ');
		type('\t ');
		advance(20000);

		expect(doubles.post).not.toHaveBeenCalled();
		expect(RollingTypingStore.isTyping('c1', 'me')).toBe(false);
	});
});
