// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Store} from '@app/features/voice/engine/Store';
import type {VoiceEngineV2AppControllerHost} from '@app/features/voice/engine/v2/VoiceEngineV2AppControllerHost';
import type {VoiceEngineV2Model, VoiceEngineV2Snapshot} from '@fluxer/voice_engine_v2';
import {isVoiceEngineV2FrameReceivedEvent} from '@fluxer/voice_engine_v2/runtime';

export const FRAME_NOTIFY_FLUSH_INTERVAL_MS = 1000;

export class VoiceEngineV2AppProjectionStore extends Store {
	private readonly unsubscribeHost: () => void;
	private frameFlushTimer: ReturnType<typeof setTimeout> | null = null;
	private notifyFlushScheduled = false;
	private disposed = false;

	constructor(private readonly host: VoiceEngineV2AppControllerHost) {
		super();
		this.unsubscribeHost = host.subscribe(({event}) => {
			if (isVoiceEngineV2FrameReceivedEvent(event)) {
				this.scheduleFrameFlush();
				return;
			}
			this.scheduleNotifyFlush();
		});
	}

	private scheduleFrameFlush(): void {
		if (this.frameFlushTimer !== null) return;
		this.frameFlushTimer = setTimeout(() => {
			assert.notEqual(this.frameFlushTimer, null, 'frame flush fired without a scheduled timer');
			this.frameFlushTimer = null;
			this.update(() => {});
		}, FRAME_NOTIFY_FLUSH_INTERVAL_MS);
		assert.notEqual(this.frameFlushTimer, null, 'frame flush scheduling must record the timer handle');
	}

	private scheduleNotifyFlush(): void {
		assert.equal(this.disposed, false, 'notify flush scheduling requires a live store');
		if (this.notifyFlushScheduled) return;
		this.notifyFlushScheduled = true;
		queueMicrotask(() => {
			if (!this.notifyFlushScheduled) return;
			this.notifyFlushScheduled = false;
			if (this.disposed) return;
			this.update(() => {});
		});
		assert.equal(this.notifyFlushScheduled, true, 'notify flush scheduling must record the pending flush');
	}

	get snapshot(): VoiceEngineV2Snapshot {
		return this.host.snapshot;
	}

	get model(): VoiceEngineV2Model {
		return this.host.model;
	}

	dispose(): void {
		this.disposed = true;
		this.notifyFlushScheduled = false;
		if (this.frameFlushTimer !== null) {
			clearTimeout(this.frameFlushTimer);
			this.frameFlushTimer = null;
		}
		this.unsubscribeHost();
	}
}

export function createVoiceEngineV2AppProjectionStore(
	host: VoiceEngineV2AppControllerHost,
): VoiceEngineV2AppProjectionStore {
	return new VoiceEngineV2AppProjectionStore(host);
}
