// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {commandIfConnected} from '@fluxer/voice_engine_v2/src/core/reducers/_helpers';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';

type VoiceEngineV2StatsEvent = Extract<
	VoiceEngineV2Event,
	{type: 'stats.collectRequested' | 'stats.collected' | 'stats.collectFailed'}
>;

export function transitionStats(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2StatsEvent,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionStats snapshot must not be null');
	assert.ok(event != null, 'transitionStats event must not be null');
	assert.equal(typeof event.type, 'string', 'stats event type must be a string');
	assert.ok(event.type.startsWith('stats.'), 'stats reducer received unrelated event');
	switch (event.type) {
		case 'stats.collectRequested':
			return commandIfConnected(snapshot, 'stats', {type: 'stats.collect'});
		case 'stats.collected':
			if (snapshot.statsOperationId !== event.operationId) return {snapshot, commands: []};
			return {
				snapshot: {...snapshot, stats: event.stats, statsOperationId: null, statsFailure: null},
				commands: [],
			};
		case 'stats.collectFailed':
			if (snapshot.statsOperationId !== event.operationId) return {snapshot, commands: []};
			return {
				snapshot: {...snapshot, statsOperationId: null, statsFailure: event.error, lastFailure: event.error},
				commands: [],
			};
	}
}
