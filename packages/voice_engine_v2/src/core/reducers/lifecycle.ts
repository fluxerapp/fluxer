// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {markOperation} from '@fluxer/voice_engine_v2/src/core/reducers/_helpers';
import {resetPublishedMedia} from '@fluxer/voice_engine_v2/src/core/reducers/_media';
import {
	planPendingConnectionTeardown,
	resetAfterDisconnect,
} from '@fluxer/voice_engine_v2/src/core/reducers/connection';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';

type VoiceEngineV2LifecycleEvent = Extract<VoiceEngineV2Event, {type: `lifecycle.${string}`}>;

export function transitionLifecycle(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2LifecycleEvent,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionLifecycle snapshot must not be null');
	assert.ok(event != null, 'transitionLifecycle event must not be null');
	assert.equal(typeof event.type, 'string', 'lifecycle event type must be a string');
	assert.ok(event.type.startsWith('lifecycle.'), 'lifecycle reducer received unrelated event');
	switch (event.type) {
		case 'lifecycle.teardownRequested':
			return planPendingConnectionTeardown({
				...resetPublishedMedia(snapshot),
				connection: {
					...snapshot.connection,
					desired: null,
					disconnectReason: 'shutdown',
					failure: null,
				},
				lifecycle: {
					tearingDown: true,
					reason: event.reason,
					operationId: null,
					failure: null,
				},
			});
		case 'lifecycle.teardownSucceeded':
			if (snapshot.lifecycle.operationId !== event.operationId) return {snapshot, commands: []};
			return {
				snapshot: {
					...markOperation(resetAfterDisconnect(snapshot), event.operationId, 'succeeded'),
					lifecycle: {tearingDown: false, reason: null, operationId: null, failure: null},
				},
				commands: [],
			};
		case 'lifecycle.teardownFailed':
			if (snapshot.lifecycle.operationId !== event.operationId) return {snapshot, commands: []};
			return {
				snapshot: {
					...markOperation(snapshot, event.operationId, 'failed', event.error),
					lifecycle: {...snapshot.lifecycle, tearingDown: true, operationId: null, failure: event.error},
					lastFailure: event.error,
				},
				commands: [],
			};
	}
}
