// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {
	dispatchLocalMediaEvent,
	dispatchObservabilityEvent,
	dispatchPlatformEvent,
	dispatchRuntimeEvent,
	dispatchSessionEvent,
} from '@fluxer/voice_engine_v2/src/core/reducers/_dispatch';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';

export {isVoiceEngineV2CommandCompletionStale} from '@fluxer/voice_engine_v2/src/core/commandCompletion';

export function transitionVoiceEngineV2(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2Event,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionVoiceEngineV2 snapshot must not be null');
	assert.ok(event != null, 'transitionVoiceEngineV2 event must not be null');
	assert.equal(typeof event.type, 'string', 'event.type must be a string');
	assert.ok(event.type.length > 0, 'event.type must not be empty');
	const local = dispatchLocalMediaEvent(snapshot, event);
	if (local) return local;
	const session = dispatchSessionEvent(snapshot, event);
	if (session) return session;
	const platform = dispatchPlatformEvent(snapshot, event);
	if (platform) return platform;
	const runtime = dispatchRuntimeEvent(snapshot, event);
	if (runtime) return runtime;
	const observability = dispatchObservabilityEvent(snapshot, event);
	if (observability) return observability;
	return {snapshot, commands: []};
}
