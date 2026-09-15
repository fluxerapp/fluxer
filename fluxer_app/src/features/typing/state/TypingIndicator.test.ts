// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Message} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
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

const {default: TypingIndicator} = await import('@app/features/typing/state/TypingIndicator');
const {default: RollingTypingSender} = await import('@app/features/typing/rolling/RollingTypingSender');
const {default: RollingTypingStore} = await import('@app/features/typing/rolling/RollingTypingStore');

const CHANNEL = 'channel';

function messageFrom(authorId: string): Message {
	return {channel_id: CHANNEL, author: {id: authorId}} as unknown as Message;
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

describe('TypingIndicator', () => {
	it('records gateway typing starts as confirmed', () => {
		TypingIndicator.startRemoteTyping(CHANNEL, 'alice');

		expect(RollingTypingStore.isConfirmedTyping(CHANNEL, 'alice')).toBe(true);
		expect(TypingIndicator.isTyping(CHANNEL, 'alice')).toBe(true);
		expect(TypingIndicator.isMemberListTyping(CHANNEL, 'alice', 'me')).toBe(true);
	});

	it('reports self in the member list only after the server echo', () => {
		RollingTypingSender.startTyping(CHANNEL);
		expect(TypingIndicator.isTyping(CHANNEL, 'me')).toBe(true);
		expect(TypingIndicator.isMemberListTyping(CHANNEL, 'me', 'me')).toBe(false);

		vi.advanceTimersByTime(1500);
		expect(doubles.post).toHaveBeenCalledTimes(1);
		expect(TypingIndicator.isMemberListTyping(CHANNEL, 'me', 'me')).toBe(false);

		TypingIndicator.startRemoteTyping(CHANNEL, 'me');

		expect(TypingIndicator.isMemberListTyping(CHANNEL, 'me', 'me')).toBe(true);
	});

	it('keeps the self row dark when the post was skipped', () => {
		for (const userId of ['a', 'b', 'c', 'd', 'e']) {
			TypingIndicator.startRemoteTyping(CHANNEL, userId);
		}

		RollingTypingSender.startTyping(CHANNEL);
		vi.advanceTimersByTime(1500);

		expect(doubles.post).not.toHaveBeenCalled();
		expect(TypingIndicator.isMemberListTyping(CHANNEL, 'me', 'me')).toBe(false);
		expect(TypingIndicator.isMemberListTyping(CHANNEL, 'a', 'me')).toBe(true);
	});

	it('keeps the send slot when the gateway resets', () => {
		TypingIndicator.startRemoteTyping(CHANNEL, 'alice');
		RollingTypingSender.startTyping(CHANNEL);
		vi.advanceTimersByTime(500);

		TypingIndicator.reset();

		expect(TypingIndicator.isTyping(CHANNEL, 'alice')).toBe(false);
		expect(TypingIndicator.isTyping(CHANNEL, 'me')).toBe(false);
		vi.advanceTimersByTime(1000);
		expect(doubles.post).toHaveBeenCalledTimes(1);
	});

	it('clears any author on message create', () => {
		TypingIndicator.startRemoteTyping(CHANNEL, 'alice');
		RollingTypingSender.startTyping(CHANNEL);

		TypingIndicator.stopTypingOnMessageCreate(messageFrom('alice'));
		TypingIndicator.stopTypingOnMessageCreate(messageFrom('me'));

		expect(TypingIndicator.isTyping(CHANNEL, 'alice')).toBe(false);
		expect(TypingIndicator.isTyping(CHANNEL, 'me')).toBe(false);
		vi.advanceTimersByTime(1500);
		expect(doubles.post).toHaveBeenCalledTimes(1);
	});
});
