// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {
	allocateOperation,
	markOperation,
	queueCommand,
	unsupportedCapability,
} from '@fluxer/voice_engine_v2/src/core/reducers/_helpers';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import type {
	VoiceEngineV2Error,
	VoiceEngineV2HardwareEncoderCapabilities,
	VoiceEngineV2OperationId,
} from '@fluxer/voice_engine_v2/src/protocol/types';

type VoiceEngineV2CapabilitiesEvent = Extract<VoiceEngineV2Event, {type: `capabilities.${string}`}>;

export function transitionCapabilities(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2CapabilitiesEvent,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionCapabilities snapshot must not be null');
	assert.ok(event != null, 'transitionCapabilities event must not be null');
	assert.equal(typeof event.type, 'string', 'capabilities event type must be a string');
	assert.ok(event.type.startsWith('capabilities.'), 'capabilities reducer received unrelated event');
	switch (event.type) {
		case 'capabilities.changed':
			return {
				snapshot: {...snapshot, capabilities: event.capabilities},
				commands: [],
			};
		case 'capabilities.hardwareEncoderQueryRequested':
			return onHardwareEncoderQueryRequested(snapshot);
		case 'capabilities.hardwareEncoderChanged':
			return onHardwareEncoderChanged(snapshot, event.operationId, event.capabilities);
		case 'capabilities.hardwareEncoderQueryFailed':
			return onHardwareEncoderQueryFailed(snapshot, event.operationId, event.error);
	}
}

function onHardwareEncoderQueryRequested(snapshot: VoiceEngineV2Snapshot): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onHardwareEncoderQueryRequested snapshot must not be null');
	assert.ok(snapshot.capabilities != null, 'onHardwareEncoderQueryRequested snapshot.capabilities must not be null');
	if (!snapshot.capabilities.hardwareEncoding) {
		const error = unsupportedCapability('hardwareEncoding');
		return {
			snapshot: {
				...snapshot,
				hardwareEncoder: {...snapshot.hardwareEncoder, failure: error},
				lastFailure: error,
			},
			commands: [],
		};
	}
	const allocated = allocateOperation(snapshot);
	return queueCommand(
		{
			...allocated.snapshot,
			hardwareEncoder: {
				...allocated.snapshot.hardwareEncoder,
				operationId: allocated.operationId,
				failure: null,
			},
		},
		{type: 'capabilities.queryHardwareEncoder', operationId: allocated.operationId},
	);
}

function onHardwareEncoderChanged(
	snapshot: VoiceEngineV2Snapshot,
	operationId: VoiceEngineV2OperationId | null,
	capabilities: VoiceEngineV2HardwareEncoderCapabilities,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onHardwareEncoderChanged snapshot must not be null');
	assert.ok(capabilities != null, 'onHardwareEncoderChanged capabilities must not be null');
	if (operationId != null && snapshot.hardwareEncoder.operationId !== operationId) {
		return {snapshot, commands: []};
	}
	return {
		snapshot: {
			...(operationId != null ? markOperation(snapshot, operationId, 'succeeded') : snapshot),
			hardwareEncoder: {
				capabilities,
				operationId: operationId === snapshot.hardwareEncoder.operationId ? null : snapshot.hardwareEncoder.operationId,
				failure: null,
			},
		},
		commands: [],
	};
}

function onHardwareEncoderQueryFailed(
	snapshot: VoiceEngineV2Snapshot,
	operationId: VoiceEngineV2OperationId,
	error: VoiceEngineV2Error,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onHardwareEncoderQueryFailed snapshot must not be null');
	assert.ok(Number.isInteger(operationId), 'onHardwareEncoderQueryFailed operationId must be an integer');
	assert.ok(error != null, 'onHardwareEncoderQueryFailed error must not be null');
	if (snapshot.hardwareEncoder.operationId !== operationId) return {snapshot, commands: []};
	return {
		snapshot: {
			...markOperation(snapshot, operationId, 'failed', error),
			hardwareEncoder: {...snapshot.hardwareEncoder, operationId: null, failure: error},
			lastFailure: error,
		},
		commands: [],
	};
}
