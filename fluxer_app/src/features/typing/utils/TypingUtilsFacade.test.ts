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
const {default: TypingPolicy} = await import('@app/features/typing/state/TypingPolicy');
const {TypingUtils: LegacyTypingUtils} = await import('@app/features/typing/legacy/LegacyTypingUtils');
const {default: LegacyTypingIndicator} = await import('@app/features/typing/legacy/LegacyTypingIndicator');
const {default: RollingTypingSender} = await import('@app/features/typing/rolling/RollingTypingSender');
const {default: RollingTypingStore} = await import('@app/features/typing/rolling/RollingTypingStore');

type ComposerTypingInput = Parameters<typeof TypingUtils.handleComposerChange>[0];

const CHANNEL = 'channel';

function composerChange(change: Partial<ComposerTypingInput>): ComposerTypingInput {
	return {
		channelId: CHANNEL,
		value: 'hello',
		previousValue: 'hell',
		isAutocompleteAttached: false,
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
	vi.restoreAllMocks();
	TypingPolicy.applyPolicy('legacy');
	LegacyTypingIndicator.reset();
	RollingTypingSender.reset();
	RollingTypingStore.reset();
	vi.useRealTimers();
	vi.clearAllMocks();
});

describe('TypingUtils facade', () => {
	it('maps clear, release composer and own message sent to the legacy clear under control', () => {
		const legacyClear = vi.spyOn(LegacyTypingUtils, 'clear');

		TypingUtils.clear(CHANNEL);
		TypingUtils.releaseComposer('previous-channel');
		TypingUtils.handleOwnMessageSent(CHANNEL);

		expect(legacyClear.mock.calls).toEqual([[CHANNEL], ['previous-channel'], [CHANNEL]]);
	});

	it('runs the legacy composer effect with the head inputs under control', () => {
		const legacyTyping = vi.spyOn(LegacyTypingUtils, 'typing').mockImplementation(() => undefined);
		const legacyClear = vi.spyOn(LegacyTypingUtils, 'clear').mockImplementation(() => undefined);
		const cases: Array<[Partial<ComposerTypingInput>, 'typing' | 'clear']> = [
			[{value: 'hello'}, 'typing'],
			[{value: '  hello  '}, 'typing'],
			[{value: 'hello', previousValue: 'hello'}, 'typing'],
			[{value: 'hello', previousValue: null}, 'typing'],
			[{value: 'hello', isEditingMessageInComposer: true}, 'typing'],
			[{value: '+:smile'}, 'typing'],
			[{value: ''}, 'clear'],
			[{value: '   '}, 'clear'],
			[{value: 'hello', isAutocompleteAttached: true}, 'clear'],
			[{value: '/gif cats'}, 'clear'],
			[{value: '   /gif'}, 'clear'],
			[{value: 's/teh/the/'}, 'clear'],
			[{value: 'hello', enabled: false}, 'clear'],
			[{value: 'hello', typingEnabled: false}, 'clear'],
		];

		for (const [change, expected] of cases) {
			legacyTyping.mockClear();
			legacyClear.mockClear();

			TypingUtils.handleComposerChange(composerChange(change));

			const called = expected === 'typing' ? legacyTyping : legacyClear;
			const skipped = expected === 'typing' ? legacyClear : legacyTyping;
			expect(called, JSON.stringify(change)).toHaveBeenCalledExactlyOnceWith(CHANNEL);
			expect(skipped, JSON.stringify(change)).not.toHaveBeenCalled();
		}
	});

	it('treats release composer as a no-op under treatment', () => {
		TypingPolicy.applyPolicy('rolling');
		const legacyClear = vi.spyOn(LegacyTypingUtils, 'clear');
		TypingUtils.handleComposerChange(composerChange({}));
		vi.advanceTimersByTime(400);

		TypingUtils.releaseComposer(CHANNEL);
		vi.advanceTimersByTime(1100);

		expect(doubles.post).toHaveBeenCalledTimes(1);
		expect(RollingTypingStore.isTyping(CHANNEL, 'me')).toBe(true);
		expect(legacyClear).not.toHaveBeenCalled();
	});

	it('cancels a pending send for clear under treatment', () => {
		TypingPolicy.applyPolicy('rolling');
		TypingUtils.handleComposerChange(composerChange({}));
		vi.advanceTimersByTime(400);

		TypingUtils.clear(CHANNEL);
		vi.advanceTimersByTime(20000);

		expect(doubles.post).not.toHaveBeenCalled();
		expect(RollingTypingStore.isTyping(CHANNEL, 'me')).toBe(false);
	});

	it('drops the send slot on own message sent under treatment', () => {
		TypingPolicy.applyPolicy('rolling');
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

	it('starts typing while autocomplete is open under treatment', () => {
		TypingPolicy.applyPolicy('rolling');

		TypingUtils.handleComposerChange(
			composerChange({previousValue: '@al', value: '@ali', isAutocompleteAttached: true}),
		);
		vi.advanceTimersByTime(1500);

		expect(doubles.post).toHaveBeenCalledTimes(1);
	});

	it('posts nothing for a spaces-only draft under treatment', () => {
		TypingPolicy.applyPolicy('rolling');

		TypingUtils.handleComposerChange(composerChange({previousValue: '', value: ' '}));
		TypingUtils.handleComposerChange(composerChange({previousValue: ' ', value: '    '}));
		vi.advanceTimersByTime(20000);

		expect(doubles.post).not.toHaveBeenCalled();
		expect(RollingTypingStore.isTyping(CHANNEL, 'me')).toBe(false);
	});

	it('treats a composer with typing turned off as disabled under treatment', () => {
		TypingPolicy.applyPolicy('rolling');

		TypingUtils.handleComposerChange(composerChange({typingEnabled: false}));
		vi.advanceTimersByTime(20000);

		expect(doubles.post).not.toHaveBeenCalled();
	});
});
