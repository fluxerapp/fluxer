// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {commandIfConnected} from '@fluxer/voice_engine_v2/src/core/reducers/_helpers';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';

type VoiceEngineV2DataEvent = Extract<VoiceEngineV2Event, {type: `data.${string}`}>;

export function transitionData(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2DataEvent,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionData snapshot must not be null');
	assert.ok(event != null, 'transitionData event must not be null');
	assert.equal(typeof event.type, 'string', 'data event type must be a string');
	assert.ok(event.type.startsWith('data.'), 'data reducer received unrelated event');
	switch (event.type) {
		case 'data.publishRequested':
			return commandIfConnected(snapshot, 'dataChannel', {type: 'data.publish', options: event.options});
		case 'data.publishSucceeded':
			return {snapshot, commands: []};
		case 'data.publishFailed':
			return {snapshot: {...snapshot, lastFailure: event.error}, commands: []};
	}
}
