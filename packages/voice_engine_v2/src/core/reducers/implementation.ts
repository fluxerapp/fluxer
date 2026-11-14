// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {allocateOperation, recordFailure} from '@fluxer/voice_engine_v2/src/core/reducers/_helpers';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';

type VoiceEngineV2ImplementationEvent = Extract<VoiceEngineV2Event, {type: `implementation.${string}`}>;

export function transitionImplementation(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2ImplementationEvent,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionImplementation snapshot must not be null');
	assert.ok(event != null, 'transitionImplementation event must not be null');
	assert.equal(typeof event.type, 'string', 'implementation event type must be a string');
	assert.ok(event.type.startsWith('implementation.'), 'implementation reducer received unrelated event');
	switch (event.type) {
		case 'implementation.prewarmRequested': {
			const allocated = allocateOperation(snapshot);
			return {
				snapshot: allocated.snapshot,
				commands: [{type: 'implementation.prewarm', operationId: allocated.operationId}],
			};
		}
		case 'implementation.prewarmSucceeded':
			return {snapshot, commands: []};
		case 'implementation.prewarmFailed':
			return recordFailure(snapshot, event.error);
	}
}
