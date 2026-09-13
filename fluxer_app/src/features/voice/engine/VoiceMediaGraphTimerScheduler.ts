// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {
	VOICE_MEDIA_GRAPH_ENTRY_LIMIT,
	type VoiceMediaGraphEvent,
	type VoiceMediaGraphSnapshot,
} from '@app/features/voice/engine/VoiceMediaGraph';
import {voiceMediaGraphStore} from '@app/features/voice/engine/VoiceMediaGraphStore';

export interface VoiceMediaGraphTimerSchedulerPlatform {
	setTimeout(callback: () => void, delayMs: number): unknown;
	clearTimeout(handle: unknown): void;
}

export interface VoiceMediaGraphTimerSchedulerStore {
	subscribe(listener: () => void): () => void;
	getGraphSnapshot(): VoiceMediaGraphSnapshot;
	transition(event: VoiceMediaGraphEvent): VoiceMediaGraphSnapshot;
	nowMs(): number;
}

export const systemVoiceMediaGraphTimerSchedulerPlatform: VoiceMediaGraphTimerSchedulerPlatform = {
	setTimeout: (callback, delayMs) => globalThis.setTimeout(callback, delayMs),
	clearTimeout: (handle) => globalThis.clearTimeout(handle as Parameters<typeof globalThis.clearTimeout>[0]),
};

interface ScheduledTimer {
	handle: unknown;
	dueAt: number;
}

export class VoiceMediaGraphTimerScheduler {
	private readonly store: VoiceMediaGraphTimerSchedulerStore;
	private readonly platform: VoiceMediaGraphTimerSchedulerPlatform;
	private readonly timers = new Map<string, ScheduledTimer>();
	private unsubscribe: (() => void) | null = null;

	constructor(
		store: VoiceMediaGraphTimerSchedulerStore,
		platform: VoiceMediaGraphTimerSchedulerPlatform = systemVoiceMediaGraphTimerSchedulerPlatform,
	) {
		this.store = store;
		this.platform = platform;
	}

	start(): void {
		if (this.unsubscribe) return;
		this.unsubscribe = this.store.subscribe(() => this.sync());
		this.sync();
	}

	stop(): void {
		this.unsubscribe?.();
		this.unsubscribe = null;
		let visited = 0;
		for (const timer of this.timers.values()) {
			visited += 1;
			assert.ok(visited <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'timer cancellation exceeded graph limit');
			this.platform.clearTimeout(timer.handle);
		}
		this.timers.clear();
	}

	private sync(): void {
		const deadlines = this.store.getGraphSnapshot().deadlinesByKey;
		assert.ok(deadlines.size <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'deadline map exceeded graph limit');
		let visited = 0;
		for (const [key, timer] of [...this.timers]) {
			visited += 1;
			assert.ok(visited <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'timer sync scan exceeded graph limit');
			const deadline = deadlines.get(key);
			if (deadline && deadline.dueAt === timer.dueAt) continue;
			this.platform.clearTimeout(timer.handle);
			this.timers.delete(key);
		}
		for (const [key, deadline] of deadlines) {
			if (this.timers.has(key)) continue;
			const delayMs = Math.max(0, deadline.dueAt - this.store.nowMs());
			const handle = this.platform.setTimeout(() => this.fire(key), delayMs);
			this.timers.set(key, {handle, dueAt: deadline.dueAt});
		}
	}

	private fire(key: string): void {
		this.timers.delete(key);
		this.store.transition({type: 'time.deadlineFired', key, at: this.store.nowMs()});
	}
}

let activeVoiceMediaGraphTimerScheduler: VoiceMediaGraphTimerScheduler | null = null;
let activeVoiceMediaGraphTimerSchedulerStore: VoiceMediaGraphTimerSchedulerStore | null = null;
let activeVoiceMediaGraphTimerSchedulerPlatform: VoiceMediaGraphTimerSchedulerPlatform | null = null;

export function startVoiceMediaGraphTimerScheduler(
	store: VoiceMediaGraphTimerSchedulerStore = voiceMediaGraphStore,
	platform: VoiceMediaGraphTimerSchedulerPlatform = systemVoiceMediaGraphTimerSchedulerPlatform,
): VoiceMediaGraphTimerScheduler {
	if (activeVoiceMediaGraphTimerScheduler) {
		assert.ok(
			activeVoiceMediaGraphTimerSchedulerStore === store,
			'voice media graph timer scheduler is already running with a different store',
		);
		assert.ok(
			activeVoiceMediaGraphTimerSchedulerPlatform === platform,
			'voice media graph timer scheduler is already running with a different platform',
		);
		return activeVoiceMediaGraphTimerScheduler;
	}
	const scheduler = new VoiceMediaGraphTimerScheduler(store, platform);
	scheduler.start();
	activeVoiceMediaGraphTimerScheduler = scheduler;
	activeVoiceMediaGraphTimerSchedulerStore = store;
	activeVoiceMediaGraphTimerSchedulerPlatform = platform;
	return scheduler;
}

export function stopVoiceMediaGraphTimerScheduler(): void {
	if (!activeVoiceMediaGraphTimerScheduler) return;
	activeVoiceMediaGraphTimerScheduler.stop();
	activeVoiceMediaGraphTimerScheduler = null;
	activeVoiceMediaGraphTimerSchedulerStore = null;
	activeVoiceMediaGraphTimerSchedulerPlatform = null;
}
