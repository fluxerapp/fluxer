// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const doubles = vi.hoisted(() => ({
	post: vi.fn((_path: string) => Promise.resolve({ok: true})),
}));

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

const {TypingUtils} = await import('@app/features/typing/utils/TypingUtils');
const {default: RollingTypingSender} = await import('@app/features/typing/rolling/RollingTypingSender');
const {default: RollingTypingStore} = await import('@app/features/typing/rolling/RollingTypingStore');

type ComposerTypingInput = Parameters<typeof TypingUtils.handleComposerChange>[0];

const CHANNEL = 'channel';

function composerChange(change: Partial<ComposerTypingInput>): ComposerTypingInput {
	return {
		channelId: CHANNEL,
		value: 'hello',
		previousValue: 'hell',
		enabled: true,
		typingEnabled: true,
		isEditingMessageInComposer: false,
		...change,
	};
}

beforeEach(() => {
	vi.useFakeTimers();
});

afterEach(() => {
	RollingTypingSender.reset();
	RollingTypingStore.reset();
	vi.useRealTimers();
	vi.clearAllMocks();
});

describe('TypingUtils', () => {
	it('cancels a pending send for clear', () => {
		TypingUtils.handleComposerChange(composerChange({}));
		vi.advanceTimersByTime(400);

		TypingUtils.clear(CHANNEL);
		vi.advanceTimersByTime(20000);

		expect(doubles.post).not.toHaveBeenCalled();
		expect(RollingTypingStore.isTyping(CHANNEL, 'me')).toBe(false);
	});

	it('drops the send slot on own message sent', () => {
		TypingUtils.handleComposerChange(composerChange({}));
		vi.advanceTimersByTime(1500);
		expect(doubles.post).toHaveBeenCalledTimes(1);

		TypingUtils.handleOwnMessageSent(CHANNEL);
		expect(RollingTypingStore.isTyping(CHANNEL, 'me')).toBe(false);
		vi.advanceTimersByTime(100);
		TypingUtils.handleComposerChange(composerChange({previousValue: '', value: 'n'}));
		vi.advanceTimersByTime(1500);

		expect(doubles.post).toHaveBeenCalledTimes(2);
	});

	it('starts typing while an autocomplete trigger is typed', () => {
		TypingUtils.handleComposerChange(composerChange({previousValue: '@al', value: '@ali'}));
		vi.advanceTimersByTime(1500);

		expect(doubles.post).toHaveBeenCalledTimes(1);
	});

	it('posts nothing for a spaces-only draft', () => {
		TypingUtils.handleComposerChange(composerChange({previousValue: '', value: ' '}));
		TypingUtils.handleComposerChange(composerChange({previousValue: ' ', value: '    '}));
		vi.advanceTimersByTime(20000);

		expect(doubles.post).not.toHaveBeenCalled();
		expect(RollingTypingStore.isTyping(CHANNEL, 'me')).toBe(false);
	});

	it('treats a composer with typing turned off as disabled', () => {
		TypingUtils.handleComposerChange(composerChange({typingEnabled: false}));
		vi.advanceTimersByTime(20000);

		expect(doubles.post).not.toHaveBeenCalled();
	});
});
