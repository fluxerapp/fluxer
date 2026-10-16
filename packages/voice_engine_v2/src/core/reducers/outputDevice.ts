// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {commandIfConnected} from '@fluxer/voice_engine_v2/src/core/reducers/_helpers';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';

type VoiceEngineV2OutputDeviceEvent = Extract<VoiceEngineV2Event, {type: `outputDevice.${string}`}>;

export function transitionOutputDevice(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2OutputDeviceEvent,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionOutputDevice snapshot must not be null');
	assert.ok(event != null, 'transitionOutputDevice event must not be null');
	assert.equal(typeof event.type, 'string', 'outputDevice event type must be a string');
	assert.ok(event.type.startsWith('outputDevice.'), 'outputDevice reducer received unrelated event');
	switch (event.type) {
		case 'outputDevice.setRequested': {
			const base = {
				...snapshot,
				outputDevice: {...snapshot.outputDevice, desiredDeviceId: event.options.deviceId},
			};
			return commandIfConnected(base, 'outputDevice', {
				type: 'outputDevice.set',
				options: event.options,
			});
		}
		case 'outputDevice.setSucceeded':
			if (snapshot.outputDevice.operationId !== event.operationId) return {snapshot, commands: []};
			return {
				snapshot: {
					...snapshot,
					outputDevice: {
						...snapshot.outputDevice,
						activeDeviceId: snapshot.outputDevice.desiredDeviceId,
						operationId: null,
						failure: null,
					},
				},
				commands: [],
			};
		case 'outputDevice.setFailed':
			if (snapshot.outputDevice.operationId !== event.operationId) return {snapshot, commands: []};
			return {
				snapshot: {
					...snapshot,
					outputDevice: {...snapshot.outputDevice, operationId: null, failure: event.error},
					lastFailure: event.error,
				},
				commands: [],
			};
	}
}
