// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {commandIfConnected} from '@fluxer/voice_engine_v2/src/core/reducers/_helpers';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';

type VoiceEngineV2ParticipantVolumeEvent = Extract<VoiceEngineV2Event, {type: `participantVolume.${string}`}>;

export function transitionParticipantVolume(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2ParticipantVolumeEvent,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionParticipantVolume snapshot must not be null');
	assert.ok(event != null, 'transitionParticipantVolume event must not be null');
	assert.equal(typeof event.type, 'string', 'participantVolume event type must be a string');
	assert.ok(event.type.startsWith('participantVolume.'), 'participantVolume reducer received unrelated event');
	switch (event.type) {
		case 'participantVolume.setRequested': {
			if (snapshot.participantVolumes[event.options.participantIdentity] === event.options.volume) {
				return {snapshot, commands: []};
			}
			const base = {
				...snapshot,
				participantVolumes: {
					...snapshot.participantVolumes,
					[event.options.participantIdentity]: event.options.volume,
				},
			};
			return commandIfConnected(base, 'participantVolume', {
				type: 'participantVolume.set',
				options: event.options,
			});
		}
		case 'participantVolume.setSucceeded':
			return {snapshot, commands: []};
		case 'participantVolume.setFailed':
			return {snapshot: {...snapshot, lastFailure: event.error}, commands: []};
	}
}
