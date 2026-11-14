// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {allocateOperation, markOperation, queueCommand} from '@fluxer/voice_engine_v2/src/core/reducers/_helpers';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';

type VoiceEngineV2E2eeEvent = Extract<VoiceEngineV2Event, {type: `e2ee.${string}`}>;

export function transitionE2ee(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2E2eeEvent,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionE2ee snapshot must not be null');
	assert.ok(event != null, 'transitionE2ee event must not be null');
	assert.equal(typeof event.type, 'string', 'e2ee event type must be a string');
	assert.ok(event.type.startsWith('e2ee.'), 'e2ee reducer received unrelated event');
	switch (event.type) {
		case 'e2ee.setEnabledRequested': {
			const allocated = allocateOperation(snapshot);
			return queueCommand(
				{
					...allocated.snapshot,
					e2ee: {
						...allocated.snapshot.e2ee,
						status: event.enabled ? 'pendingKey' : 'disabled',
						keyId: event.keyId ?? allocated.snapshot.e2ee.keyId,
						failure: null,
						operationId: allocated.operationId,
					},
				},
				{type: 'e2ee.setEnabled', operationId: allocated.operationId, enabled: event.enabled, keyId: event.keyId},
			);
		}
		case 'e2ee.enabled':
			if (event.operationId != null && snapshot.e2ee.operationId !== event.operationId) {
				return {snapshot, commands: []};
			}
			return {
				snapshot: {
					...(event.operationId != null ? markOperation(snapshot, event.operationId, 'succeeded') : snapshot),
					e2ee: {status: 'enabled', keyId: event.keyId, failure: null, operationId: null},
				},
				commands: [],
			};
		case 'e2ee.disabled':
			if (event.operationId != null && snapshot.e2ee.operationId !== event.operationId) {
				return {snapshot, commands: []};
			}
			return {
				snapshot: {
					...(event.operationId != null ? markOperation(snapshot, event.operationId, 'succeeded') : snapshot),
					e2ee: {status: 'disabled', keyId: null, failure: null, operationId: null},
				},
				commands: [],
			};
		case 'e2ee.failed':
			if (event.operationId != null && snapshot.e2ee.operationId !== event.operationId) {
				return {snapshot, commands: []};
			}
			return {
				snapshot: {
					...(event.operationId != null ? markOperation(snapshot, event.operationId, 'failed', event.error) : snapshot),
					e2ee: {...snapshot.e2ee, status: 'failed', failure: event.error, operationId: null},
					lastFailure: event.error,
				},
				commands: [],
			};
	}
}
