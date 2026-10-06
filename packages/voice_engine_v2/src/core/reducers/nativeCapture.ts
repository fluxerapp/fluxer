// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {allocateOperation, markOperation, queueCommand} from '@fluxer/voice_engine_v2/src/core/reducers/_helpers';
import {
	nativeZeroCopyRequiredError,
	unavailableZeroCopyTransportError,
} from '@fluxer/voice_engine_v2/src/core/reducers/_zeroCopy';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Command} from '@fluxer/voice_engine_v2/src/protocol/commands';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import type {
	VoiceEngineV2Error,
	VoiceEngineV2NativeCaptureOptions,
	VoiceEngineV2OperationId,
} from '@fluxer/voice_engine_v2/src/protocol/types';

type VoiceEngineV2NativeCaptureEvent = Extract<VoiceEngineV2Event, {type: `nativeCapture.${string}`}>;

function failWithZeroCopyRequired(
	snapshot: VoiceEngineV2Snapshot,
	resource: 'capture' | 'frame',
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'failWithZeroCopyRequired snapshot must not be null');
	assert.equal(typeof resource, 'string', 'failWithZeroCopyRequired resource must be a string');
	const error = nativeZeroCopyRequiredError(resource);
	return {
		snapshot: {
			...snapshot,
			nativeCapture: {...snapshot.nativeCapture, failure: error},
			lastFailure: error,
		},
		commands: [],
	};
}

function failWithUnavailableTransport(snapshot: VoiceEngineV2Snapshot): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'failWithUnavailableTransport snapshot must not be null');
	assert.ok(snapshot.nativeCapture != null, 'failWithUnavailableTransport snapshot.nativeCapture must not be null');
	const error = unavailableZeroCopyTransportError('capture');
	return {
		snapshot: {
			...snapshot,
			nativeCapture: {...snapshot.nativeCapture, failure: error},
			lastFailure: error,
		},
		commands: [],
	};
}

function onStartOrUpdate(
	snapshot: VoiceEngineV2Snapshot,
	options: VoiceEngineV2NativeCaptureOptions,
	commandType: 'nativeCapture.start' | 'nativeCapture.update',
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onStartOrUpdate snapshot must not be null');
	assert.ok(options != null, 'onStartOrUpdate options must not be null');
	assert.equal(typeof options.captureId, 'string', 'options.captureId must be a string');
	if (options.zeroCopyRequired !== true) return failWithZeroCopyRequired(snapshot, 'capture');
	if (!snapshot.capabilities.zeroCopyScreenTransport) return failWithUnavailableTransport(snapshot);
	const allocated = allocateOperation(snapshot);
	return queueCommand(
		{
			...allocated.snapshot,
			nativeCapture: {
				...allocated.snapshot.nativeCapture,
				captures: {
					...allocated.snapshot.nativeCapture.captures,
					[options.captureId]: options,
				},
				operationIds: {
					...allocated.snapshot.nativeCapture.operationIds,
					[options.captureId]: allocated.operationId,
				},
				failure: null,
			},
		},
		{type: commandType, operationId: allocated.operationId, options} as VoiceEngineV2Command,
	);
}

function onStopRequested(snapshot: VoiceEngineV2Snapshot, captureId: string): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onStopRequested snapshot must not be null');
	assert.equal(typeof captureId, 'string', 'onStopRequested captureId must be a string');
	assert.ok(captureId.length > 0, 'onStopRequested captureId must not be empty');
	const allocated = allocateOperation(snapshot);
	return queueCommand(
		{
			...allocated.snapshot,
			nativeCapture: {
				...allocated.snapshot.nativeCapture,
				operationIds: {
					...allocated.snapshot.nativeCapture.operationIds,
					[captureId]: allocated.operationId,
				},
				failure: null,
			},
		},
		{type: 'nativeCapture.stop', operationId: allocated.operationId, captureId},
	);
}

function completeNativeCaptureOperation(
	snapshot: VoiceEngineV2Snapshot,
	captureId: string,
	operationId: VoiceEngineV2OperationId | null,
	error: VoiceEngineV2Error | null,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'completeNativeCaptureOperation snapshot must not be null');
	assert.equal(typeof captureId, 'string', 'completeNativeCaptureOperation captureId must be a string');
	if (operationId != null && snapshot.nativeCapture.operationIds[captureId] !== operationId) {
		return {snapshot, commands: []};
	}
	const operationIds = {...snapshot.nativeCapture.operationIds};
	if (operationId != null) delete operationIds[captureId];
	return {
		snapshot: {
			...(operationId != null ? markOperation(snapshot, operationId, error ? 'failed' : 'succeeded', error) : snapshot),
			nativeCapture: {
				...snapshot.nativeCapture,
				operationIds,
				failure: error,
			},
			lastFailure: error ?? snapshot.lastFailure,
		},
		commands: [],
	};
}

function stopNativeCapture(
	snapshot: VoiceEngineV2Snapshot,
	captureId: string,
	operationId: VoiceEngineV2OperationId | null,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'stopNativeCapture snapshot must not be null');
	assert.equal(typeof captureId, 'string', 'stopNativeCapture captureId must be a string');
	if (operationId != null && snapshot.nativeCapture.operationIds[captureId] !== operationId) {
		return {snapshot, commands: []};
	}
	const captures = {...snapshot.nativeCapture.captures};
	const operationIds = {...snapshot.nativeCapture.operationIds};
	delete captures[captureId];
	delete operationIds[captureId];
	return {
		snapshot: {
			...(operationId != null ? markOperation(snapshot, operationId, 'succeeded') : snapshot),
			nativeCapture: {
				...snapshot.nativeCapture,
				captures,
				operationIds,
				failure: null,
			},
		},
		commands: [],
	};
}

export function transitionNativeCapture(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2NativeCaptureEvent,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionNativeCapture snapshot must not be null');
	assert.ok(event != null, 'transitionNativeCapture event must not be null');
	assert.equal(typeof event.type, 'string', 'nativeCapture event type must be a string');
	assert.ok(event.type.startsWith('nativeCapture.'), 'nativeCapture reducer received unrelated event');
	switch (event.type) {
		case 'nativeCapture.startRequested':
			return onStartOrUpdate(snapshot, event.options, 'nativeCapture.start');
		case 'nativeCapture.updateRequested':
			return onStartOrUpdate(snapshot, event.options, 'nativeCapture.update');
		case 'nativeCapture.stopRequested':
			return onStopRequested(snapshot, event.captureId);
		case 'nativeCapture.started':
			return completeNativeCaptureOperation(snapshot, event.captureId, event.operationId, null);
		case 'nativeCapture.stopped':
			return stopNativeCapture(snapshot, event.captureId, event.operationId);
		case 'nativeCapture.failed':
			return completeNativeCaptureOperation(snapshot, event.captureId, event.operationId, event.error);
		case 'nativeCapture.frame':
			if (event.frame.zeroCopy !== true) return failWithZeroCopyRequired(snapshot, 'frame');
			return {snapshot, commands: []};
	}
}
