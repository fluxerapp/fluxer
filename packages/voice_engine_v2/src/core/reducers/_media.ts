// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {VoiceEngineV2LocalMediaState, VoiceEngineV2Snapshot} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Error, VoiceEngineV2OperationId} from '@fluxer/voice_engine_v2/src/protocol/types';

export function applyMediaSuccess<Options>(
	state: VoiceEngineV2LocalMediaState<Options>,
	operationId: VoiceEngineV2OperationId,
): VoiceEngineV2LocalMediaState<Options> {
	assert.ok(state != null, 'applyMediaSuccess state must not be null');
	assert.ok(Number.isInteger(operationId), 'applyMediaSuccess operationId must be an integer');
	if (state.operationId !== operationId) return state;
	return {
		...state,
		status: state.desired ? 'published' : 'idle',
		published: state.desired,
		operationId: null,
		failure: null,
	};
}

export function applyMediaFailure<Options>(
	state: VoiceEngineV2LocalMediaState<Options>,
	operationId: VoiceEngineV2OperationId,
	error: VoiceEngineV2Error,
): VoiceEngineV2LocalMediaState<Options> {
	assert.ok(state != null, 'applyMediaFailure state must not be null');
	assert.ok(Number.isInteger(operationId), 'applyMediaFailure operationId must be an integer');
	assert.ok(error != null, 'applyMediaFailure error must not be null');
	if (state.operationId !== operationId) return state;
	return {
		...state,
		status: 'failed',
		published: null,
		operationId: null,
		failure: error,
	};
}

export function completeUnpublish<Options>(
	state: VoiceEngineV2LocalMediaState<Options>,
	operationId: VoiceEngineV2OperationId,
): VoiceEngineV2LocalMediaState<Options> {
	assert.ok(state != null, 'completeUnpublish state must not be null');
	assert.ok(Number.isInteger(operationId), 'completeUnpublish operationId must be an integer');
	if (state.operationId !== operationId) return state;
	return {
		...state,
		status: 'idle',
		published: null,
		operationId: null,
		failure: null,
	};
}

export function resetPublishedMedia(snapshot: VoiceEngineV2Snapshot): VoiceEngineV2Snapshot {
	assert.ok(snapshot != null, 'resetPublishedMedia snapshot must not be null');
	assert.ok(snapshot.microphone != null, 'resetPublishedMedia snapshot.microphone must not be null');
	return {
		...snapshot,
		microphone: {
			...snapshot.microphone,
			status: 'idle',
			published: null,
			operationId: null,
			failure: null,
			setEnabledOperationId: null,
			localSpeakingOverride: false,
		},
		camera: {
			...snapshot.camera,
			status: 'idle',
			published: null,
			operationId: null,
			failure: null,
		},
		screen: {
			...snapshot.screen,
			status: 'idle',
			published: null,
			operationId: null,
			failure: null,
		},
		screenAudio: {
			...snapshot.screenAudio,
			status: 'idle',
			published: null,
			operationId: null,
			failure: null,
		},
	};
}
