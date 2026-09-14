// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Message} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {configure} from 'mobx';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

configure({safeDescriptors: false});

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
const {default: TypingPolicy} = await import('@app/features/typing/state/TypingPolicy');
const {default: LegacyTypingIndicator} = await import('@app/features/typing/legacy/LegacyTypingIndicator');
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
	vi.restoreAllMocks();
	TypingPolicy.applyPolicy('legacy');
	LegacyTypingIndicator.reset();
	RollingTypingSender.reset();
	RollingTypingStore.reset();
	vi.useRealTimers();
	vi.clearAllMocks();
});

describe('TypingIndicator facade', () => {
	it('forwards every exposed store method to the legacy singleton with identical arguments under control', () => {
		const startRemoteTyping = vi.spyOn(LegacyTypingIndicator, 'startRemoteTyping');
		const stopTypingOnMessageCreate = vi.spyOn(LegacyTypingIndicator, 'stopTypingOnMessageCreate');
		const reset = vi.spyOn(LegacyTypingIndicator, 'reset');
		const isTyping = vi.spyOn(LegacyTypingIndicator, 'isTyping');
		const isMemberListTyping = vi.spyOn(LegacyTypingIndicator, 'isMemberListTyping').mockReturnValue(false);
		const message = messageFrom('alice');

		TypingIndicator.startRemoteTyping(CHANNEL, 'alice');
		TypingIndicator.stopTypingOnMessageCreate(message);
		TypingIndicator.isTyping(CHANNEL, 'bob');
		TypingIndicator.isMemberListTyping(CHANNEL, 'me', 'me');
		TypingIndicator.isMemberListTyping(CHANNEL, 'bob', null);
		TypingIndicator.reset();

		expect(startRemoteTyping).toHaveBeenCalledExactlyOnceWith(CHANNEL, 'alice');
		expect(stopTypingOnMessageCreate).toHaveBeenCalledExactlyOnceWith(message);
		expect(isTyping).toHaveBeenCalledExactlyOnceWith(CHANNEL, 'bob');
		expect(isMemberListTyping.mock.calls).toEqual([
			[CHANNEL, 'me', 'me'],
			[CHANNEL, 'bob', null],
		]);
		expect(reset).toHaveBeenCalledExactlyOnceWith();
	});

	it('returns the legacy results for typing and member list readers under control', () => {
		vi.spyOn(LegacyTypingIndicator, 'isTyping').mockReturnValueOnce(true).mockReturnValueOnce(false);
		vi.spyOn(LegacyTypingIndicator, 'isMemberListTyping').mockReturnValueOnce(false).mockReturnValueOnce(true);

		expect(TypingIndicator.isTyping(CHANNEL, 'alice')).toBe(true);
		expect(TypingIndicator.isTyping(CHANNEL, 'alice')).toBe(false);
		expect(TypingIndicator.isMemberListTyping(CHANNEL, 'me', 'me')).toBe(false);
		expect(TypingIndicator.isMemberListTyping(CHANNEL, 'me', 'me')).toBe(true);
	});

	it('arms no rolling timer under control', () => {
		TypingIndicator.startRemoteTyping(CHANNEL, 'alice');

		expect(LegacyTypingIndicator.isTyping(CHANNEL, 'alice')).toBe(true);
		expect(RollingTypingStore.countTypists(CHANNEL)).toBe(0);
		expect(vi.getTimerCount()).toBe(1);

		TypingIndicator.reset();

		expect(vi.getTimerCount()).toBe(0);
	});

	it('reports the member list self row from local typing under control', () => {
		TypingIndicator.startRemoteTyping(CHANNEL, 'me');
		expect(TypingIndicator.isMemberListTyping(CHANNEL, 'me', 'me')).toBe(false);
		expect(TypingIndicator.isMemberListTyping(CHANNEL, 'me', 'alice')).toBe(true);

		LegacyTypingIndicator.startLocalTyping(CHANNEL, 'me');

		expect(TypingIndicator.isMemberListTyping(CHANNEL, 'me', 'me')).toBe(true);
	});

	it('routes gateway typing starts to the rolling store under treatment', () => {
		TypingPolicy.applyPolicy('rolling');
		const legacyStartRemoteTyping = vi.spyOn(LegacyTypingIndicator, 'startRemoteTyping');

		TypingIndicator.startRemoteTyping(CHANNEL, 'alice');

		expect(legacyStartRemoteTyping).not.toHaveBeenCalled();
		expect(RollingTypingStore.isConfirmedTyping(CHANNEL, 'alice')).toBe(true);
		expect(TypingIndicator.isTyping(CHANNEL, 'alice')).toBe(true);
		expect(TypingIndicator.isMemberListTyping(CHANNEL, 'alice', 'me')).toBe(true);
		expect(LegacyTypingIndicator.isTyping(CHANNEL, 'alice')).toBe(false);
	});

	it('reports self in the member list only after the server echo under treatment', () => {
		TypingPolicy.applyPolicy('rolling');

		RollingTypingSender.startTyping(CHANNEL);
		expect(TypingIndicator.isTyping(CHANNEL, 'me')).toBe(true);
		expect(TypingIndicator.isMemberListTyping(CHANNEL, 'me', 'me')).toBe(false);

		vi.advanceTimersByTime(1500);
		expect(doubles.post).toHaveBeenCalledTimes(1);
		expect(TypingIndicator.isMemberListTyping(CHANNEL, 'me', 'me')).toBe(false);

		TypingIndicator.startRemoteTyping(CHANNEL, 'me');

		expect(TypingIndicator.isMemberListTyping(CHANNEL, 'me', 'me')).toBe(true);
	});

	it('keeps the self row dark when the post was skipped under treatment', () => {
		TypingPolicy.applyPolicy('rolling');
		for (const userId of ['a', 'b', 'c', 'd', 'e']) {
			TypingIndicator.startRemoteTyping(CHANNEL, userId);
		}

		RollingTypingSender.startTyping(CHANNEL);
		vi.advanceTimersByTime(1500);

		expect(doubles.post).not.toHaveBeenCalled();
		expect(TypingIndicator.isMemberListTyping(CHANNEL, 'me', 'me')).toBe(false);
		expect(TypingIndicator.isMemberListTyping(CHANNEL, 'a', 'me')).toBe(true);
	});

	it('keeps the rolling send slot when the gateway resets under treatment', () => {
		TypingPolicy.applyPolicy('rolling');
		TypingIndicator.startRemoteTyping(CHANNEL, 'alice');
		RollingTypingSender.startTyping(CHANNEL);
		vi.advanceTimersByTime(500);

		TypingIndicator.reset();

		expect(TypingIndicator.isTyping(CHANNEL, 'alice')).toBe(false);
		expect(TypingIndicator.isTyping(CHANNEL, 'me')).toBe(false);
		vi.advanceTimersByTime(1000);
		expect(doubles.post).toHaveBeenCalledTimes(1);
	});

	it('clears any author on message create under treatment', () => {
		TypingPolicy.applyPolicy('rolling');
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
