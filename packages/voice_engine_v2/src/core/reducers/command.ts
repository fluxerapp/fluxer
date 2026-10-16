// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {
	allocateOperation,
	clearOperationForResource,
	markOperation,
	queueCommand,
} from '@fluxer/voice_engine_v2/src/core/reducers/_helpers';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '@fluxer/voice_engine_v2/src/core/state';
import {getVoiceEngineV2CommandTypeResourceKey} from '@fluxer/voice_engine_v2/src/protocol/commands';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';

type VoiceEngineV2CommandEvent = Extract<VoiceEngineV2Event, {type: `command.${string}` | `operation.${string}`}>;

export function transitionCommand(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2CommandEvent,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionCommand snapshot must not be null');
	assert.ok(event != null, 'transitionCommand event must not be null');
	assert.equal(typeof event.type, 'string', 'command event type must be a string');
	assert.ok(
		event.type.startsWith('command.') || event.type.startsWith('operation.'),
		'command reducer received unrelated event',
	);
	switch (event.type) {
		case 'command.succeeded': {
			const resourceKey = getVoiceEngineV2CommandTypeResourceKey(event.commandType);
			const marked = markOperation(snapshot, event.operationId, 'succeeded');
			return {
				snapshot: resourceKey ? clearOperationForResource(marked, resourceKey, event.operationId) : marked,
				commands: [],
			};
		}
		case 'command.failed': {
			const resourceKey = getVoiceEngineV2CommandTypeResourceKey(event.commandType);
			const marked = markOperation(snapshot, event.operationId, 'failed', event.error);
			return {
				snapshot: {
					...(resourceKey ? clearOperationForResource(marked, resourceKey, event.operationId) : marked),
					lastFailure: event.error,
				},
				commands: [],
			};
		}
		case 'command.staleCompletionRejected':
			return {snapshot: markOperation(snapshot, event.operationId, 'stale'), commands: []};
		case 'operation.cancelRequested': {
			const allocated = allocateOperation(snapshot);
			return queueCommand(allocated.snapshot, {
				type: 'operation.cancel',
				operationId: allocated.operationId,
				targetOperationId: event.operationId,
				resourceKey: event.resourceKey,
				reason: event.reason,
			});
		}
		case 'operation.cancelled':
			return {
				snapshot: clearOperationForResource(
					markOperation(snapshot, event.targetOperationId, 'cancelled'),
					event.resourceKey,
					event.targetOperationId,
				),
				commands: [],
			};
	}
}
