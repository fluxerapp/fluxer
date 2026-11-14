// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {
	allocateOperation,
	beginUnpublish,
	failUnpublish,
	invalidArgument,
	isConnected,
	unsupportedCapability,
} from '@fluxer/voice_engine_v2/src/core/reducers/_helpers';
import {
	applyMediaFailure,
	applyMediaSuccess,
	completeUnpublish,
} from '@fluxer/voice_engine_v2/src/core/reducers/_media';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import type {
	VoiceEngineV2Error,
	VoiceEngineV2OperationId,
	VoiceEngineV2ScreenAudioOptions,
} from '@fluxer/voice_engine_v2/src/protocol/types';

type VoiceEngineV2ScreenAudioEvent = Extract<VoiceEngineV2Event, {type: `screenAudio.${string}`}>;

function sameScreenAudioOptions(
	a: VoiceEngineV2ScreenAudioOptions | null,
	b: VoiceEngineV2ScreenAudioOptions | null,
): boolean {
	assert.ok(a !== undefined, 'sameScreenAudioOptions a must not be undefined');
	assert.ok(b !== undefined, 'sameScreenAudioOptions b must not be undefined');
	return (
		a?.sampleRate === b?.sampleRate &&
		a?.numChannels === b?.numChannels &&
		a?.route === b?.route &&
		a?.captureId === b?.captureId &&
		a?.tapId === b?.tapId
	);
}

function validateScreenAudioPublish(
	snapshot: VoiceEngineV2Snapshot,
	desired: VoiceEngineV2ScreenAudioOptions,
): VoiceEngineV2Error | null {
	assert.ok(snapshot != null, 'validateScreenAudioPublish snapshot must not be null');
	assert.ok(desired != null, 'validateScreenAudioPublish desired must not be null');
	if (desired.route !== 'native') return null;
	if (!snapshot.capabilities.nativeAudioTaps) {
		return unsupportedCapability('nativeAudioTaps');
	}
	if (!desired.captureId && !desired.tapId) {
		return invalidArgument('Native screen audio routing requires a capture id or tap id', 'screenAudio');
	}
	return null;
}

export function beginScreenAudioPublish(snapshot: VoiceEngineV2Snapshot): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'beginScreenAudioPublish snapshot must not be null');
	assert.ok(snapshot.screenAudio != null, 'beginScreenAudioPublish snapshot.screenAudio must not be null');
	const desired = snapshot.screenAudio.desired;
	if (!desired) return {snapshot, commands: []};
	if (!snapshot.capabilities.screenAudio) {
		const error = unsupportedCapability('screenAudio');
		return {
			snapshot: {
				...snapshot,
				screenAudio: {...snapshot.screenAudio, status: 'failed', failure: error},
				lastFailure: error,
			},
			commands: [],
		};
	}
	const validationError = validateScreenAudioPublish(snapshot, desired);
	if (validationError) {
		return {
			snapshot: {
				...snapshot,
				screenAudio: {...snapshot.screenAudio, status: 'failed', failure: validationError},
				lastFailure: validationError,
			},
			commands: [],
		};
	}
	if (!isConnected(snapshot)) return {snapshot, commands: []};
	if (snapshot.screenAudio.status === 'published' && sameScreenAudioOptions(snapshot.screenAudio.published, desired)) {
		return {snapshot, commands: []};
	}
	const allocated = allocateOperation(snapshot);
	return {
		snapshot: {
			...allocated.snapshot,
			screenAudio: {
				...allocated.snapshot.screenAudio,
				status: 'publishing',
				operationId: allocated.operationId,
				failure: null,
			},
		},
		commands: [{type: 'screenAudio.publish', operationId: allocated.operationId, options: desired}],
	};
}

function failScreenAudio(
	snapshot: VoiceEngineV2Snapshot,
	operationId: VoiceEngineV2OperationId,
	error: VoiceEngineV2Error,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'failScreenAudio snapshot must not be null');
	assert.ok(Number.isInteger(operationId), 'failScreenAudio operationId must be an integer');
	assert.ok(error != null, 'failScreenAudio error must not be null');
	const state = snapshot.screenAudio;
	return {
		snapshot: {
			...snapshot,
			screenAudio: applyMediaFailure(state, operationId, error),
			lastFailure: state.operationId === operationId ? error : snapshot.lastFailure,
		},
		commands: [],
	};
}

export function transitionScreenAudio(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2ScreenAudioEvent,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionScreenAudio snapshot must not be null');
	assert.ok(event != null, 'transitionScreenAudio event must not be null');
	assert.equal(typeof event.type, 'string', 'screenAudio event type must be a string');
	assert.ok(event.type.startsWith('screenAudio.'), 'screenAudio reducer received unrelated event');
	switch (event.type) {
		case 'screenAudio.publishRequested':
			return beginScreenAudioPublish({
				...snapshot,
				screenAudio: {...snapshot.screenAudio, desired: event.options},
			});
		case 'screenAudio.publishSucceeded':
			return {
				snapshot: {...snapshot, screenAudio: applyMediaSuccess(snapshot.screenAudio, event.operationId)},
				commands: [],
			};
		case 'screenAudio.publishFailed':
			return failScreenAudio(snapshot, event.operationId, event.error);
		case 'screenAudio.unpublishRequested':
			return beginUnpublish(snapshot, 'screenAudio', 'screenAudio.unpublish');
		case 'screenAudio.unpublishSucceeded':
			return {
				snapshot: {...snapshot, screenAudio: completeUnpublish(snapshot.screenAudio, event.operationId)},
				commands: [],
			};
		case 'screenAudio.unpublishFailed':
			return failUnpublish(snapshot, event.operationId, event.error, 'screenAudio');
	}
}
