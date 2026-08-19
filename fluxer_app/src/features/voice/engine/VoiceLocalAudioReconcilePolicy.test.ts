// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {
	createVoiceLocalAudioReconcileCoalescerSnapshot,
	createVoiceMicrophoneFailureLatchSnapshot,
	isVoiceMicrophoneFailureLatchActive,
	selectVoiceLocalAudioEffectiveSelfMute,
	selectVoiceLocalAudioReconcileCoalescedCount,
	selectVoiceLocalAudioReconcileRunReasons,
	selectVoiceLocalAudioReconcileStartedRuns,
	selectVoiceMicrophoneFailureCount,
	shouldStartVoiceLocalAudioReconcileRun,
	shouldWarnAboutVoiceLocalAudioReconcileFollowUpRuns,
	transitionVoiceLocalAudioReconcileCoalescerSnapshot,
	transitionVoiceMicrophoneFailureLatchSnapshot,
	VOICE_LOCAL_AUDIO_RECONCILE_FOLLOW_UP_WARN_LIMIT,
	VOICE_LOCAL_AUDIO_RECONCILE_REASON_LIMIT,
	type VoiceLocalAudioReconcileCoalescerEvent,
	type VoiceLocalAudioReconcileCoalescerSnapshot,
	type VoiceMicrophoneFailureLatchEvent,
	type VoiceMicrophoneFailureLatchSnapshot,
} from './VoiceLocalAudioReconcilePolicy';

const RUNAWAY_RUN_LIMIT = 512;

function sendCoalescer(
	snapshot: VoiceLocalAudioReconcileCoalescerSnapshot,
	event: VoiceLocalAudioReconcileCoalescerEvent,
): VoiceLocalAudioReconcileCoalescerSnapshot {
	return transitionVoiceLocalAudioReconcileCoalescerSnapshot(snapshot, event);
}

function sendLatch(
	snapshot: VoiceMicrophoneFailureLatchSnapshot,
	event: VoiceMicrophoneFailureLatchEvent,
): VoiceMicrophoneFailureLatchSnapshot {
	return transitionVoiceMicrophoneFailureLatchSnapshot(snapshot, event);
}

class ReconcileHarness {
	snapshot = createVoiceLocalAudioReconcileCoalescerSnapshot();
	startedRuns: Array<ReadonlyArray<string>> = [];
	private runningDepth = 0;

	constructor(private readonly body: (harness: ReconcileHarness) => void) {}

	request(reason: string): void {
		const previous = this.snapshot;
		this.snapshot = sendCoalescer(previous, {type: 'run.requested', reason});
		if (shouldStartVoiceLocalAudioReconcileRun(previous, this.snapshot)) {
			this.drain();
		}
	}

	private drain(): void {
		expect(this.runningDepth).toBe(0);
		this.runningDepth += 1;
		for (;;) {
			if (this.startedRuns.length >= RUNAWAY_RUN_LIMIT) {
				throw new Error('runaway local audio reconciliation');
			}
			this.startedRuns.push(selectVoiceLocalAudioReconcileRunReasons(this.snapshot));
			this.body(this);
			const previous = this.snapshot;
			this.snapshot = sendCoalescer(previous, {type: 'run.settled'});
			if (!shouldStartVoiceLocalAudioReconcileRun(previous, this.snapshot)) break;
		}
		this.runningDepth -= 1;
	}
}

describe('voice local audio reconcile coalescer', () => {
	it('starts a run when a request arrives while idle', () => {
		const initial = createVoiceLocalAudioReconcileCoalescerSnapshot();
		const next = sendCoalescer(initial, {type: 'run.requested', reason: 'voice room connected'});
		expect(shouldStartVoiceLocalAudioReconcileRun(initial, next)).toBe(true);
		expect(next.value).toBe('running');
		expect(selectVoiceLocalAudioReconcileStartedRuns(next)).toBe(1);
		expect(selectVoiceLocalAudioReconcileRunReasons(next)).toEqual(['voice room connected']);
	});

	it('absorbs a request that arrives while a run is in flight instead of starting a second run', () => {
		const running = sendCoalescer(createVoiceLocalAudioReconcileCoalescerSnapshot(), {
			type: 'run.requested',
			reason: 'voice room connected',
		});
		const dirty = sendCoalescer(running, {type: 'run.requested', reason: 'local audio state change'});
		expect(shouldStartVoiceLocalAudioReconcileRun(running, dirty)).toBe(false);
		expect(dirty.value).toBe('runningDirty');
		expect(selectVoiceLocalAudioReconcileStartedRuns(dirty)).toBe(1);
		expect(selectVoiceLocalAudioReconcileCoalescedCount(dirty)).toBe(1);
	});

	it('collapses K triggers arriving during one run into exactly one follow-up run', () => {
		const triggerCount = 10;
		let pendingTriggers = triggerCount;
		const harness = new ReconcileHarness((self) => {
			for (let index = 0; index < pendingTriggers; index++) {
				self.request(`trigger ${index}`);
			}
			pendingTriggers = 0;
		});

		harness.request('voice room connected');

		expect(harness.startedRuns).toHaveLength(2);
		expect(selectVoiceLocalAudioReconcileStartedRuns(harness.snapshot)).toBe(2);
		expect(harness.snapshot.value).toBe('idle');
	});

	it('records how many triggers were coalesced into the follow-up run', () => {
		let snapshot = sendCoalescer(createVoiceLocalAudioReconcileCoalescerSnapshot(), {
			type: 'run.requested',
			reason: 'voice room connected',
		});
		for (let index = 0; index < 10; index++) {
			snapshot = sendCoalescer(snapshot, {type: 'run.requested', reason: `trigger ${index}`});
		}
		expect(selectVoiceLocalAudioReconcileCoalescedCount(snapshot)).toBe(10);
		expect(selectVoiceLocalAudioReconcileStartedRuns(snapshot)).toBe(1);

		const followUp = sendCoalescer(snapshot, {type: 'run.settled'});
		expect(followUp.value).toBe('running');
		expect(selectVoiceLocalAudioReconcileStartedRuns(followUp)).toBe(2);
		expect(selectVoiceLocalAudioReconcileCoalescedCount(followUp)).toBe(0);
	});

	it('returns to idle when a clean run settles and does not start another run', () => {
		const running = sendCoalescer(createVoiceLocalAudioReconcileCoalescerSnapshot(), {
			type: 'run.requested',
			reason: 'voice state update',
		});
		const settled = sendCoalescer(running, {type: 'run.settled'});
		expect(settled.value).toBe('idle');
		expect(shouldStartVoiceLocalAudioReconcileRun(running, settled)).toBe(false);
		expect(selectVoiceLocalAudioReconcileRunReasons(settled)).toEqual([]);
	});

	it('promotes the pending reasons into the follow-up run exactly once', () => {
		let snapshot = sendCoalescer(createVoiceLocalAudioReconcileCoalescerSnapshot(), {
			type: 'run.requested',
			reason: 'voice room connected',
		});
		snapshot = sendCoalescer(snapshot, {type: 'run.requested', reason: 'local audio state change'});
		snapshot = sendCoalescer(snapshot, {type: 'run.requested', reason: 'voice state update'});
		snapshot = sendCoalescer(snapshot, {type: 'run.settled'});
		expect(selectVoiceLocalAudioReconcileRunReasons(snapshot)).toEqual([
			'local audio state change',
			'voice state update',
		]);
		expect(selectVoiceLocalAudioReconcileStartedRuns(snapshot)).toBe(2);
		const idle = sendCoalescer(snapshot, {type: 'run.settled'});
		expect(idle.value).toBe('idle');
		expect(selectVoiceLocalAudioReconcileStartedRuns(idle)).toBe(2);
	});

	it('ignores a settle that arrives while idle', () => {
		const initial = createVoiceLocalAudioReconcileCoalescerSnapshot();
		const settled = sendCoalescer(initial, {type: 'run.settled'});
		expect(settled.value).toBe('idle');
		expect(selectVoiceLocalAudioReconcileStartedRuns(settled)).toBe(0);
	});

	it('keeps the reconcile-storm ordering linear instead of doubling', () => {
		const failingRuns = 8;
		let remainingFailures = failingRuns;
		const harness = new ReconcileHarness((self) => {
			if (remainingFailures === 0) return;
			remainingFailures -= 1;
			self.request('local audio state change');
			self.request('voice state update');
		});

		harness.request('voice room connected');

		expect(harness.startedRuns).toHaveLength(failingRuns + 1);
		expect(harness.startedRuns.length).toBeLessThan(2 ** failingRuns);
		expect(harness.snapshot.value).toBe('idle');
	});

	it('settles after two runs once the failure latches self-mute and stops emitting triggers', () => {
		let latched = false;
		const harness = new ReconcileHarness((self) => {
			if (latched) return;
			latched = true;
			self.request('local audio state change');
			self.request('voice state update');
		});

		harness.request('voice room connected');

		expect(harness.startedRuns).toEqual([['voice room connected'], ['local audio state change', 'voice state update']]);
		expect(harness.snapshot.value).toBe('idle');
	});

	it('never drops a trigger: a run always starts after the last request in a burst', () => {
		const observedAtRunStart: Array<number> = [];
		let requestCounter = 0;
		let burst = 3;
		const harness = new ReconcileHarness((self) => {
			observedAtRunStart.push(requestCounter);
			for (let index = 0; index < burst; index++) {
				requestCounter += 1;
				self.request(`burst ${requestCounter}`);
			}
			burst = 0;
		});

		requestCounter += 1;
		harness.request(`burst ${requestCounter}`);

		expect(observedAtRunStart).toEqual([1, 4]);
		expect(observedAtRunStart[observedAtRunStart.length - 1]).toBe(requestCounter);
	});

	it('caps the reason list while still counting every coalesced trigger', () => {
		let snapshot = sendCoalescer(createVoiceLocalAudioReconcileCoalescerSnapshot(), {
			type: 'run.requested',
			reason: 'voice room connected',
		});
		const triggerCount = VOICE_LOCAL_AUDIO_RECONCILE_REASON_LIMIT + 5;
		for (let index = 0; index < triggerCount; index++) {
			snapshot = sendCoalescer(snapshot, {type: 'run.requested', reason: `trigger ${index}`});
		}
		expect(selectVoiceLocalAudioReconcileCoalescedCount(snapshot)).toBe(triggerCount);
		const followUp = sendCoalescer(snapshot, {type: 'run.settled'});
		expect(selectVoiceLocalAudioReconcileRunReasons(followUp)).toHaveLength(VOICE_LOCAL_AUDIO_RECONCILE_REASON_LIMIT);
	});

	it('deduplicates repeated reasons so a storm stays readable', () => {
		let snapshot = sendCoalescer(createVoiceLocalAudioReconcileCoalescerSnapshot(), {
			type: 'run.requested',
			reason: 'voice room connected',
		});
		for (let index = 0; index < 50; index++) {
			snapshot = sendCoalescer(snapshot, {type: 'run.requested', reason: 'local audio state change'});
		}
		const followUp = sendCoalescer(snapshot, {type: 'run.settled'});
		expect(selectVoiceLocalAudioReconcileRunReasons(followUp)).toEqual(['local audio state change']);
		expect(selectVoiceLocalAudioReconcileCoalescedCount(snapshot)).toBe(50);
	});

	it('flags a livelock once the follow-up runs never stop chaining', () => {
		let snapshot = createVoiceLocalAudioReconcileCoalescerSnapshot();
		snapshot = sendCoalescer(snapshot, {type: 'run.requested', reason: 'voice room connected'});
		for (let index = 0; index < VOICE_LOCAL_AUDIO_RECONCILE_FOLLOW_UP_WARN_LIMIT; index++) {
			expect(shouldWarnAboutVoiceLocalAudioReconcileFollowUpRuns(snapshot)).toBe(false);
			snapshot = sendCoalescer(snapshot, {type: 'run.requested', reason: 'local audio state change'});
			snapshot = sendCoalescer(snapshot, {type: 'run.settled'});
		}
		expect(shouldWarnAboutVoiceLocalAudioReconcileFollowUpRuns(snapshot)).toBe(true);

		const drained = sendCoalescer(snapshot, {type: 'run.settled'});
		expect(drained.value).toBe('idle');
		expect(shouldWarnAboutVoiceLocalAudioReconcileFollowUpRuns(drained)).toBe(false);
	});

	it('crosses the livelock threshold once so the drain can warn a single time', () => {
		let snapshot = createVoiceLocalAudioReconcileCoalescerSnapshot();
		snapshot = sendCoalescer(snapshot, {type: 'run.requested', reason: 'voice room connected'});
		let crossings = 0;
		for (let index = 0; index < VOICE_LOCAL_AUDIO_RECONCILE_FOLLOW_UP_WARN_LIMIT * 4; index++) {
			const before = snapshot;
			snapshot = sendCoalescer(snapshot, {type: 'run.requested', reason: 'local audio state change'});
			snapshot = sendCoalescer(snapshot, {type: 'run.settled'});
			if (
				shouldWarnAboutVoiceLocalAudioReconcileFollowUpRuns(snapshot) &&
				!shouldWarnAboutVoiceLocalAudioReconcileFollowUpRuns(before)
			) {
				crossings += 1;
			}
		}
		expect(crossings).toBe(1);
	});

	it('is level-triggered: the reason never selects control flow', () => {
		const script: Array<VoiceLocalAudioReconcileCoalescerEvent> = [
			{type: 'run.requested', reason: 'first'},
			{type: 'run.requested', reason: 'second'},
			{type: 'run.settled'},
			{type: 'run.settled'},
		];
		const replay = (reasonFor: (index: number) => string) => {
			let snapshot = createVoiceLocalAudioReconcileCoalescerSnapshot();
			const trace: Array<string> = [];
			script.forEach((event, index) => {
				snapshot = sendCoalescer(
					snapshot,
					event.type === 'run.requested' ? {type: 'run.requested', reason: reasonFor(index)} : event,
				);
				trace.push(`${String(snapshot.value)}:${selectVoiceLocalAudioReconcileStartedRuns(snapshot)}`);
			});
			return trace;
		};
		expect(replay((index) => `reason-${index}`)).toEqual(replay(() => 'voice state update'));
	});
});

describe('voice microphone failure latch', () => {
	const scope = {channelId: 'channel-1', inputDeviceId: 'mic-1'};

	function latchedSnapshot(): VoiceMicrophoneFailureLatchSnapshot {
		return sendLatch(createVoiceMicrophoneFailureLatchSnapshot(), {type: 'microphone.enableFailed', ...scope});
	}

	it('starts clear', () => {
		const initial = createVoiceMicrophoneFailureLatchSnapshot();
		expect(isVoiceMicrophoneFailureLatchActive(initial)).toBe(false);
		expect(selectVoiceMicrophoneFailureCount(initial)).toBe(0);
	});

	it('latches when enabling the microphone fails', () => {
		const latched = latchedSnapshot();
		expect(isVoiceMicrophoneFailureLatchActive(latched)).toBe(true);
		expect(selectVoiceMicrophoneFailureCount(latched)).toBe(1);
	});

	it('keeps forcing self-mute when the server echoes a stale self_mute:false', () => {
		const latched = latchedSnapshot();
		expect(selectVoiceLocalAudioEffectiveSelfMute({localSelfMute: true, microphoneFailureLatched: true})).toBe(true);
		const afterStaleEcho = sendLatch(latched, {type: 'scope.observed', ...scope});
		expect(isVoiceMicrophoneFailureLatchActive(afterStaleEcho)).toBe(true);
		expect(
			selectVoiceLocalAudioEffectiveSelfMute({
				localSelfMute: false,
				microphoneFailureLatched: isVoiceMicrophoneFailureLatchActive(afterStaleEcho),
			}),
		).toBe(true);
	});

	it('survives repeated recomputes of the audio-controls snapshot', () => {
		let snapshot = latchedSnapshot();
		for (let index = 0; index < 20; index++) {
			snapshot = sendLatch(snapshot, {type: 'scope.observed', ...scope});
			expect(
				selectVoiceLocalAudioEffectiveSelfMute({
					localSelfMute: false,
					microphoneFailureLatched: isVoiceMicrophoneFailureLatchActive(snapshot),
				}),
			).toBe(true);
		}
		expect(selectVoiceMicrophoneFailureCount(snapshot)).toBe(1);
	});

	it('clears when the microphone is enabled successfully', () => {
		const cleared = sendLatch(latchedSnapshot(), {type: 'microphone.enableSucceeded'});
		expect(isVoiceMicrophoneFailureLatchActive(cleared)).toBe(false);
		expect(selectVoiceMicrophoneFailureCount(cleared)).toBe(0);
	});

	it('clears when the user changes their own mute intent', () => {
		const cleared = sendLatch(latchedSnapshot(), {type: 'mute.userIntentChanged'});
		expect(isVoiceMicrophoneFailureLatchActive(cleared)).toBe(false);
	});

	it('clears when the voice connection is torn down or re-established', () => {
		const cleared = sendLatch(latchedSnapshot(), {type: 'latch.reset'});
		expect(isVoiceMicrophoneFailureLatchActive(cleared)).toBe(false);
	});

	it('clears when the active channel changes', () => {
		const cleared = sendLatch(latchedSnapshot(), {
			type: 'scope.observed',
			channelId: 'channel-2',
			inputDeviceId: scope.inputDeviceId,
		});
		expect(isVoiceMicrophoneFailureLatchActive(cleared)).toBe(false);
	});

	it('clears when the input device changes', () => {
		const cleared = sendLatch(latchedSnapshot(), {
			type: 'scope.observed',
			channelId: scope.channelId,
			inputDeviceId: 'mic-2',
		});
		expect(isVoiceMicrophoneFailureLatchActive(cleared)).toBe(false);
	});

	it('re-latches after a cleared latch fails again and counts the failures', () => {
		let snapshot = latchedSnapshot();
		snapshot = sendLatch(snapshot, {type: 'mute.userIntentChanged'});
		snapshot = sendLatch(snapshot, {type: 'microphone.enableFailed', ...scope});
		expect(isVoiceMicrophoneFailureLatchActive(snapshot)).toBe(true);
		expect(selectVoiceMicrophoneFailureCount(snapshot)).toBe(1);
		snapshot = sendLatch(snapshot, {type: 'microphone.enableFailed', ...scope});
		expect(selectVoiceMicrophoneFailureCount(snapshot)).toBe(2);
	});

	it('does not latch on recovery events observed while already clear', () => {
		let snapshot = createVoiceMicrophoneFailureLatchSnapshot();
		for (const event of [
			{type: 'microphone.enableSucceeded'},
			{type: 'mute.userIntentChanged'},
			{type: 'latch.reset'},
			{type: 'scope.observed', ...scope},
		] as Array<VoiceMicrophoneFailureLatchEvent>) {
			snapshot = sendLatch(snapshot, event);
			expect(isVoiceMicrophoneFailureLatchActive(snapshot)).toBe(false);
		}
	});

	it('leaves self-mute untouched when nothing has failed', () => {
		expect(selectVoiceLocalAudioEffectiveSelfMute({localSelfMute: false, microphoneFailureLatched: false})).toBe(false);
		expect(selectVoiceLocalAudioEffectiveSelfMute({localSelfMute: true, microphoneFailureLatched: false})).toBe(true);
	});
});
