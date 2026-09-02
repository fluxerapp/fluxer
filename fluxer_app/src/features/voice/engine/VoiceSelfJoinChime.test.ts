// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';
import {
	discardVoiceJoinChimeSequence,
	resetSelfJoinChimesForTests,
	SELF_JOIN_CHIME_DEDUPE_WINDOW_MS,
	SELF_JOIN_CHIME_START_DEADLINE_MS,
	startVoiceJoinChimeSequence,
	VOICE_JOIN_CHIME_SEQUENCE_MAX_ENTRIES,
	VOICE_JOIN_CHIME_SEQUENCE_RETENTION_MS,
	type VoiceJoinChimeSequenceResult,
	waitForVoiceJoinChimeSequence,
} from './VoiceSelfJoinChime';

vi.mock('@app/features/ui/commands/SoundCommands', () => ({
	playOneShotSoundImmediatelyBypassingSelfDeafened: vi.fn(() => Promise.resolve(true)),
}));

vi.mock('@app/features/voice/state/VoiceRegionTeleport', () => ({
	default: {shouldSuppressRejoinSounds: () => false},
}));

const identity = {userId: 'user-1', channelId: 'channel-1'};

function neverSettles(): Promise<boolean> {
	return new Promise<boolean>(() => {});
}

function track(promise: Promise<VoiceJoinChimeSequenceResult>): {value: VoiceJoinChimeSequenceResult | null} {
	const state: {value: VoiceJoinChimeSequenceResult | null} = {value: null};
	void promise.then((result) => {
		state.value = result;
	});
	return state;
}

describe('voice join chime sequence', () => {
	beforeEach(() => {
		vi.useFakeTimers();
		resetSelfJoinChimesForTests();
	});

	afterEach(() => {
		resetSelfJoinChimesForTests();
		vi.useRealTimers();
	});

	it('dedupes two starts sharing the same join token', async () => {
		const start = vi.fn(() => Promise.resolve(true));
		const first = startVoiceJoinChimeSequence(identity, 'connection-1', start);
		const second = startVoiceJoinChimeSequence(identity, 'connection-1', start);
		expect(second).toBe(first);
		await vi.advanceTimersByTimeAsync(0);
		expect(start).toHaveBeenCalledTimes(1);
		await expect(first).resolves.toBe('started');
		await expect(second).resolves.toBe('started');
	});

	it('does not swallow a genuinely new connection token', async () => {
		const signals: Array<AbortSignal> = [];
		const start = vi.fn((signal: AbortSignal) => {
			signals.push(signal);
			return neverSettles();
		});
		const first = startVoiceJoinChimeSequence(identity, 'connection-1', start);
		await vi.advanceTimersByTimeAsync(0);
		const second = startVoiceJoinChimeSequence(identity, 'connection-2', start);
		await vi.advanceTimersByTimeAsync(0);
		expect(start).toHaveBeenCalledTimes(2);
		expect(signals[0]?.aborted).toBe(true);
		expect(signals[1]?.aborted).toBe(false);
		expect(second).not.toBe(first);
		await expect(first).resolves.toBe('unavailable');
	});

	it('adopts an existing entry when the join token is null', async () => {
		const firstStart = vi.fn(() => Promise.resolve(true));
		const secondStart = vi.fn(() => Promise.resolve(true));
		const first = startVoiceJoinChimeSequence(identity, 'connection-1', firstStart);
		const second = startVoiceJoinChimeSequence(identity, null, secondStart);
		expect(second).toBe(first);
		await vi.advanceTimersByTimeAsync(0);
		expect(firstStart).toHaveBeenCalledTimes(1);
		expect(secondStart).not.toHaveBeenCalled();
	});

	it('maps the start outcome onto the sequence result', async () => {
		const started = startVoiceJoinChimeSequence({userId: 'a', channelId: 'c'}, 'connection-a', () =>
			Promise.resolve(true),
		);
		const unavailable = startVoiceJoinChimeSequence({userId: 'b', channelId: 'c'}, 'connection-b', () =>
			Promise.resolve(false),
		);
		const threw = startVoiceJoinChimeSequence({userId: 'c', channelId: 'c'}, 'connection-c', () =>
			Promise.reject(new Error('no audio device')),
		);
		await vi.advanceTimersByTimeAsync(0);
		await expect(started).resolves.toBe('started');
		await expect(unavailable).resolves.toBe('unavailable');
		await expect(threw).resolves.toBe('unavailable');
	});

	it('expires a wait that no start ever adopts, and not one tick early', async () => {
		const waiting = track(waitForVoiceJoinChimeSequence(identity));
		await vi.advanceTimersByTimeAsync(SELF_JOIN_CHIME_START_DEADLINE_MS - 1);
		expect(waiting.value).toBeNull();
		await vi.advanceTimersByTimeAsync(1);
		expect(waiting.value).toBe('expired-before-start');
	});

	it('lets a later start adopt the entry a wait created', async () => {
		const waiting = waitForVoiceJoinChimeSequence(identity);
		await vi.advanceTimersByTimeAsync(100);
		const start = vi.fn(() => Promise.resolve(true));
		const started = startVoiceJoinChimeSequence(identity, 'connection-1', start);
		await vi.advanceTimersByTimeAsync(0);
		expect(start).toHaveBeenCalledTimes(1);
		await expect(waiting).resolves.toBe('started');
		await expect(started).resolves.toBe('started');
	});

	it('bounds a start that never settles and aborts its signal', async () => {
		let captured: AbortSignal | null = null;
		const started = startVoiceJoinChimeSequence(identity, 'connection-1', (signal) => {
			captured = signal;
			return neverSettles();
		});
		await vi.advanceTimersByTimeAsync(SELF_JOIN_CHIME_START_DEADLINE_MS);
		await expect(started).resolves.toBe('unavailable');
		expect(captured).not.toBeNull();
		expect((captured as unknown as AbortSignal).aborted).toBe(true);
	});

	it('keeps a settled started result across a discard and issues a fresh entry afterwards', async () => {
		const started = startVoiceJoinChimeSequence(identity, 'connection-1', () => Promise.resolve(true));
		await vi.advanceTimersByTimeAsync(0);
		await expect(started).resolves.toBe('started');
		discardVoiceJoinChimeSequence(identity);
		await expect(started).resolves.toBe('started');
		const waiting = track(waitForVoiceJoinChimeSequence(identity));
		await vi.advanceTimersByTimeAsync(SELF_JOIN_CHIME_START_DEADLINE_MS);
		expect(waiting.value).toBe('expired-before-start');
	});

	it('expires an unsettled entry on discard and aborts its signal', async () => {
		let captured: AbortSignal | null = null;
		const started = startVoiceJoinChimeSequence(identity, 'connection-1', (signal) => {
			captured = signal;
			return neverSettles();
		});
		await vi.advanceTimersByTimeAsync(0);
		discardVoiceJoinChimeSequence(identity);
		await expect(started).resolves.toBe('expired-before-start');
		expect((captured as unknown as AbortSignal).aborted).toBe(true);
	});

	it('evicts the oldest entry rather than leaving its promise hanging', async () => {
		const evicted = track(waitForVoiceJoinChimeSequence({userId: 'user-0', channelId: 'channel-1'}));
		for (let index = 1; index <= VOICE_JOIN_CHIME_SEQUENCE_MAX_ENTRIES; index++) {
			waitForVoiceJoinChimeSequence({userId: `user-${index}`, channelId: 'channel-1'});
		}
		await vi.advanceTimersByTimeAsync(0);
		expect(evicted.value).toBe('expired-before-start');
	});

	it('keeps a settled outcome claimable by a late entrance event past the dedupe window', async () => {
		const startedIdentity = {userId: 'user-started', channelId: 'channel-1'};
		const unavailableIdentity = {userId: 'user-unavailable', channelId: 'channel-1'};
		const started = startVoiceJoinChimeSequence(startedIdentity, 'connection-started', () => Promise.resolve(true));
		const unavailable = startVoiceJoinChimeSequence(unavailableIdentity, 'connection-unavailable', () =>
			Promise.resolve(false),
		);
		await vi.advanceTimersByTimeAsync(0);
		await expect(started).resolves.toBe('started');
		await expect(unavailable).resolves.toBe('unavailable');
		await vi.advanceTimersByTimeAsync(SELF_JOIN_CHIME_DEDUPE_WINDOW_MS + 1);
		const lateStarted = track(waitForVoiceJoinChimeSequence(startedIdentity));
		const lateUnavailable = track(waitForVoiceJoinChimeSequence(unavailableIdentity));
		await vi.advanceTimersByTimeAsync(SELF_JOIN_CHIME_START_DEADLINE_MS);
		expect(lateStarted.value).toBe('started');
		expect(lateUnavailable.value).toBe('unavailable');
	});

	it('removes a settled entry after the retention window and leaves no timers behind', async () => {
		const started = startVoiceJoinChimeSequence(identity, 'connection-1', () => Promise.resolve(true));
		await vi.advanceTimersByTimeAsync(0);
		await expect(started).resolves.toBe('started');
		expect(vi.getTimerCount()).toBe(1);
		await vi.advanceTimersByTimeAsync(VOICE_JOIN_CHIME_SEQUENCE_RETENTION_MS);
		expect(vi.getTimerCount()).toBe(0);
		const late = track(waitForVoiceJoinChimeSequence(identity));
		await vi.advanceTimersByTimeAsync(SELF_JOIN_CHIME_START_DEADLINE_MS);
		expect(late.value).toBe('expired-before-start');
	});

	it('plays the chime once across the gateway and livekit-room sources for the same join', async () => {
		const gatewayStart = vi.fn(() => Promise.resolve(true));
		const livekitRoomStart = vi.fn(() => Promise.resolve(true));
		const fromGateway = startVoiceJoinChimeSequence(identity, 'connection-1', gatewayStart);
		const fromLivekitRoom = startVoiceJoinChimeSequence(identity, 'connection-1', livekitRoomStart);
		expect(fromLivekitRoom).toBe(fromGateway);
		await vi.advanceTimersByTimeAsync(0);
		expect(gatewayStart).toHaveBeenCalledTimes(1);
		expect(livekitRoomStart).not.toHaveBeenCalled();
		await expect(fromGateway).resolves.toBe('started');
		await expect(fromLivekitRoom).resolves.toBe('started');
	});

	it('discards the previous channel sequence on a quick channel hop and starts a fresh one', async () => {
		const firstChannel = {userId: 'user-1', channelId: 'channel-1'};
		const secondChannel = {userId: 'user-1', channelId: 'channel-2'};
		let firstSignal: AbortSignal | null = null;
		const firstStart = vi.fn((signal: AbortSignal) => {
			firstSignal = signal;
			return neverSettles();
		});
		const first = startVoiceJoinChimeSequence(firstChannel, 'connection-1', firstStart);
		const firstEntrance = track(waitForVoiceJoinChimeSequence(firstChannel));
		await vi.advanceTimersByTimeAsync(0);
		discardVoiceJoinChimeSequence(firstChannel);
		await expect(first).resolves.toBe('expired-before-start');
		expect((firstSignal as unknown as AbortSignal).aborted).toBe(true);
		expect(firstEntrance.value).toBe('expired-before-start');
		const secondStart = vi.fn(() => Promise.resolve(true));
		const second = startVoiceJoinChimeSequence(secondChannel, 'connection-2', secondStart);
		await vi.advanceTimersByTimeAsync(0);
		expect(secondStart).toHaveBeenCalledTimes(1);
		await expect(second).resolves.toBe('started');
		await expect(waitForVoiceJoinChimeSequence(secondChannel)).resolves.toBe('started');
	});
});
