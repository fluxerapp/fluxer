// SPDX-License-Identifier: AGPL-3.0-or-later

import {screenShareWatchAttemptKey} from '@app/features/voice/components/useScreenShareWatchFailure';
import {
	selectVoiceMediaGraphFailure,
	selectVoiceMediaGraphViewerStreamKeys,
	voiceMediaGraphWatchAttemptDeadlineKey,
	WATCH_ATTEMPT_TIMEOUT_MS,
} from '@app/features/voice/engine/VoiceMediaGraph';
import {VoiceMediaGraphStore} from '@app/features/voice/engine/VoiceMediaGraphStore';
import {
	startVoiceMediaGraphTimerScheduler,
	stopVoiceMediaGraphTimerScheduler,
	VoiceMediaGraphTimerScheduler,
	type VoiceMediaGraphTimerSchedulerPlatform,
} from '@app/features/voice/engine/VoiceMediaGraphTimerScheduler';
import {afterEach, describe, expect, it} from 'vitest';

const STREAM_KEY = 'dm:channel-a:connection-a';

interface FakeTimer {
	id: number;
	callback: () => void;
	delayMs: number;
	cancelled: boolean;
	fired: boolean;
}

class FakePlatform implements VoiceMediaGraphTimerSchedulerPlatform {
	timers: Array<FakeTimer> = [];
	private nextId = 1;

	setTimeout(callback: () => void, delayMs: number): unknown {
		const timer: FakeTimer = {id: this.nextId, callback, delayMs, cancelled: false, fired: false};
		this.nextId += 1;
		this.timers.push(timer);
		return timer.id;
	}

	clearTimeout(handle: unknown): void {
		const timer = this.timers.find((candidate) => candidate.id === handle);
		if (timer) timer.cancelled = true;
	}

	fire(timer: FakeTimer): void {
		timer.fired = true;
		timer.callback();
	}

	get active(): Array<FakeTimer> {
		return this.timers.filter((timer) => !timer.cancelled && !timer.fired);
	}
}

function createHarness(startMs = 0) {
	let now = startMs;
	const store = new VoiceMediaGraphStore({now: () => now});
	const platform = new FakePlatform();
	const scheduler = new VoiceMediaGraphTimerScheduler(store, platform);
	return {
		store,
		platform,
		scheduler,
		advance(toMs: number) {
			now = toMs;
		},
	};
}

describe('VoiceMediaGraphTimerScheduler', () => {
	it('schedules exactly one timer per deadline', () => {
		const {store, platform, scheduler} = createHarness();
		scheduler.start();

		store.transition({type: 'watch.started', streamKey: STREAM_KEY, at: store.nowMs()});
		expect(platform.active).toHaveLength(1);
		expect(platform.active[0].delayMs).toBe(WATCH_ATTEMPT_TIMEOUT_MS);

		store.transition({type: 'watchIntent.add', key: STREAM_KEY});
		expect(platform.active).toHaveLength(1);

		scheduler.stop();
	});

	it('cancels timers when their deadline is removed', () => {
		const {store, platform, scheduler} = createHarness();
		scheduler.start();

		store.transition({type: 'watch.started', streamKey: STREAM_KEY, at: store.nowMs()});
		store.transition({type: 'watch.stopped', streamKey: STREAM_KEY});

		expect(platform.active).toHaveLength(0);
		scheduler.stop();
	});

	it('dispatches time.deadlineFired with the clock time when a timer fires', () => {
		const harness = createHarness();
		const {store, platform, scheduler} = harness;
		scheduler.start();

		store.transition({type: 'watch.started', streamKey: STREAM_KEY, at: store.nowMs()});
		store.transition({
			type: 'watch.attemptEnsured',
			streamKey: STREAM_KEY,
			attemptKey: screenShareWatchAttemptKey({streamKey: STREAM_KEY, watchGeneration: 1}),
			startedAt: store.nowMs(),
		});
		harness.advance(WATCH_ATTEMPT_TIMEOUT_MS);
		platform.fire(platform.active[0]);

		const recorded = selectVoiceMediaGraphFailure(store.getGraphSnapshot(), {streamKey: STREAM_KEY});
		expect(recorded?.reportedAt).toBe(WATCH_ATTEMPT_TIMEOUT_MS);
		expect(recorded?.reason).toBe('subscription-attach-timeout');
		expect(store.getGraphSnapshot().deadlinesByKey.size).toBe(0);
		expect(platform.active).toHaveLength(0);

		scheduler.stop();
	});

	it('reschedules when a deadline is replaced with a new due time', () => {
		const harness = createHarness();
		const {store, platform, scheduler} = harness;
		scheduler.start();

		store.transition({type: 'watch.started', streamKey: STREAM_KEY, at: 0});
		harness.advance(1000);
		store.transition({type: 'watch.started', streamKey: STREAM_KEY, at: 1000});

		expect(platform.active).toHaveLength(1);
		expect(platform.active[0].delayMs).toBe(WATCH_ATTEMPT_TIMEOUT_MS);
		expect(platform.timers).toHaveLength(2);

		scheduler.stop();
	});

	it('drives the deferred stop grace deadline end to end', () => {
		const harness = createHarness();
		const {store, platform, scheduler} = harness;
		scheduler.start();

		store.transition({type: 'watchIntent.add', key: STREAM_KEY});
		store.transition({type: 'watchIntent.deferRemove', key: STREAM_KEY, at: store.nowMs()});
		expect(platform.active).toHaveLength(1);

		harness.advance(platform.active[0].delayMs);
		platform.fire(platform.active[0]);

		expect(selectVoiceMediaGraphViewerStreamKeys(store.getGraphSnapshot())).toEqual([]);
		expect(platform.active).toHaveLength(0);

		scheduler.stop();
	});

	it('ignores stale fires after the deadline was already removed', () => {
		const {store, platform, scheduler} = createHarness();
		scheduler.start();

		store.transition({type: 'watch.started', streamKey: STREAM_KEY, at: 0});
		const timer = platform.active[0];
		store.transition({type: 'watch.stopped', streamKey: STREAM_KEY});

		platform.fire(timer);

		expect(selectVoiceMediaGraphFailure(store.getGraphSnapshot(), {streamKey: STREAM_KEY})).toBeNull();
		scheduler.stop();
	});

	it('cancels all timers on stop', () => {
		const {store, platform, scheduler} = createHarness();
		scheduler.start();
		store.transition({type: 'watch.started', streamKey: STREAM_KEY, at: 0});

		scheduler.stop();

		expect(platform.active).toHaveLength(0);
		expect(
			store.getGraphSnapshot().deadlinesByKey.get(voiceMediaGraphWatchAttemptDeadlineKey(STREAM_KEY)),
		).toBeDefined();
	});
});

describe('startVoiceMediaGraphTimerScheduler', () => {
	afterEach(() => {
		stopVoiceMediaGraphTimerScheduler();
	});

	it('keeps a single active scheduler across repeated starts', () => {
		const {store, platform} = createHarness();
		const first = startVoiceMediaGraphTimerScheduler(store, platform);
		const second = startVoiceMediaGraphTimerScheduler(store, platform);
		expect(second).toBe(first);

		store.transition({type: 'watch.started', streamKey: STREAM_KEY, at: store.nowMs()});
		expect(platform.active).toHaveLength(1);
	});

	it('cancels timers on stop and allows a fresh scheduler afterwards', () => {
		const {store, platform} = createHarness();
		const first = startVoiceMediaGraphTimerScheduler(store, platform);
		store.transition({type: 'watch.started', streamKey: STREAM_KEY, at: store.nowMs()});
		expect(platform.active).toHaveLength(1);

		stopVoiceMediaGraphTimerScheduler();
		expect(platform.active).toHaveLength(0);

		const next = startVoiceMediaGraphTimerScheduler(store, platform);
		expect(next).not.toBe(first);
		expect(platform.active).toHaveLength(1);
	});
});
