// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {
	createVoiceScreenShareSnapshot,
	transitionVoiceScreenShareSnapshot,
	type VoiceScreenShareEvent,
	type VoiceScreenShareSnapshot,
} from '@app/features/voice/engine/VoiceScreenShareStateMachine';
import type {PendingScreenShareStopRequest} from '@app/features/voice/engine/voice_screen_share_manager/shared';

export interface VoiceScreenShareLifecycleHost {
	update(fn: () => void): void;
}

export interface VoiceScreenShareQueuedRequestExecutors {
	isScreenShareEnabled: () => boolean;
	applyStop: (request: PendingScreenShareStopRequest) => Promise<void>;
}

export class VoiceScreenShareLifecycleStore {
	private readonly host: VoiceScreenShareLifecycleHost;
	private snapshotInternal: VoiceScreenShareSnapshot = createVoiceScreenShareSnapshot();

	constructor(host: VoiceScreenShareLifecycleHost) {
		assert.ok(host, 'lifecycle host is required');
		assert.equal(typeof host.update, 'function', 'lifecycle host must expose update');
		this.host = host;
	}

	get snapshot(): VoiceScreenShareSnapshot {
		return this.snapshotInternal;
	}

	get pendingOperationActive(): boolean {
		return this.snapshotInternal.context.pendingOperation != null;
	}

	get publicationReplaceInFlight(): boolean {
		return this.snapshotInternal.context.publicationReplaceInFlight;
	}

	get streamingPriorityHeld(): boolean {
		return this.snapshotInternal.context.streamingPriorityHeld;
	}

	transition(event: VoiceScreenShareEvent): void {
		assert.ok(event, 'lifecycle event is required');
		assert.equal(typeof event.type, 'string', 'lifecycle event must have a type');
		this.host.update(() => {
			this.snapshotInternal = transitionVoiceScreenShareSnapshot(this.snapshotInternal, event);
		});
	}

	queueStopRequest(options?: {sendUpdate?: boolean; playSound?: boolean}): void {
		this.transition({
			type: 'share.stop',
			request: {
				sendUpdate: options?.sendUpdate ?? true,
				playSound: options?.playSound ?? true,
			},
		});
	}

	takeQueuedStopRequest(): PendingScreenShareStopRequest | null {
		const request = this.snapshotInternal.context.queuedStopRequest;
		this.transition({type: 'share.queuedStop.clear'});
		assert.equal(this.snapshotInternal.context.queuedStopRequest, null, 'queued stop request must clear on take');
		return request;
	}

	async drainQueuedRequests(executors: VoiceScreenShareQueuedRequestExecutors): Promise<void> {
		assert.ok(executors, 'queued request executors are required');
		const stopRequest = this.takeQueuedStopRequest();
		if (stopRequest && executors.isScreenShareEnabled()) {
			await executors.applyStop(stopRequest);
		}
	}
}
