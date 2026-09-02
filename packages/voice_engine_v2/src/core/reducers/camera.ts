// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {planVoiceEngineV2CameraEncodingChange} from '../../policies/cameraShare';
import type {VoiceEngineV2Event} from '../../protocol/events';
import type {
	VoiceEngineV2CameraEncodingOptions,
	VoiceEngineV2CameraOptions,
	VoiceEngineV2Error,
	VoiceEngineV2OperationId,
} from '../../protocol/types';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '../state';
import {allocateOperation, failUnpublish, invalidArgument, isConnected, unsupportedCapability} from './_helpers';
import {applyMediaFailure, applyMediaSuccess, completeUnpublish} from './_media';

type VoiceEngineV2CameraEvent = Extract<VoiceEngineV2Event, {type: `camera.${string}`}>;

function sameCameraOptions(a: VoiceEngineV2CameraOptions | null, b: VoiceEngineV2CameraOptions | null): boolean {
	assert.ok(a !== undefined, 'sameCameraOptions a must not be undefined');
	assert.ok(b !== undefined, 'sameCameraOptions b must not be undefined');
	return (
		a?.deviceId === b?.deviceId && a?.width === b?.width && a?.height === b?.height && a?.frameRate === b?.frameRate
	);
}

function sameCameraPublishCommandOptions(
	a: VoiceEngineV2CameraOptions | null,
	b: VoiceEngineV2CameraOptions | null,
): boolean {
	assert.ok(a !== undefined, 'sameCameraPublishCommandOptions a must not be undefined');
	assert.ok(b !== undefined, 'sameCameraPublishCommandOptions b must not be undefined');
	if (!sameCameraOptions(a, b)) return false;
	return (a?.sendUpdate !== false) === (b?.sendUpdate !== false);
}

function isCameraPublishInFlight(snapshot: VoiceEngineV2Snapshot): boolean {
	assert.ok(snapshot != null, 'isCameraPublishInFlight snapshot must not be null');
	assert.ok(snapshot.camera != null, 'isCameraPublishInFlight snapshot.camera must not be null');
	if (snapshot.camera.status !== 'publishing') return false;
	return snapshot.camera.operationId !== null;
}

function isDuplicateInFlightCameraPublish(
	snapshot: VoiceEngineV2Snapshot,
	options: VoiceEngineV2CameraOptions,
): boolean {
	assert.ok(snapshot != null, 'isDuplicateInFlightCameraPublish snapshot must not be null');
	assert.ok(options != null, 'isDuplicateInFlightCameraPublish options must not be null');
	if (!isCameraPublishInFlight(snapshot)) return false;
	assert.ok(snapshot.camera.desired !== null, 'in-flight camera publish must have desired options');
	return sameCameraPublishCommandOptions(snapshot.camera.desired, options);
}

export function beginCameraPublish(snapshot: VoiceEngineV2Snapshot): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'beginCameraPublish snapshot must not be null');
	assert.ok(snapshot.camera != null, 'beginCameraPublish snapshot.camera must not be null');
	const desired = snapshot.camera.desired;
	if (!desired) return {snapshot, commands: []};
	if (!snapshot.capabilities.camera) {
		const error = unsupportedCapability('camera');
		return {
			snapshot: {
				...snapshot,
				camera: {...snapshot.camera, status: 'failed', failure: error},
				lastFailure: error,
			},
			commands: [],
		};
	}
	if (!isConnected(snapshot)) return {snapshot, commands: []};
	if (snapshot.camera.status === 'published' && sameCameraOptions(snapshot.camera.published, desired)) {
		return {snapshot, commands: []};
	}
	const allocated = allocateOperation(snapshot);
	return {
		snapshot: {
			...allocated.snapshot,
			camera: {
				...allocated.snapshot.camera,
				status: 'publishing',
				operationId: allocated.operationId,
				failure: null,
			},
		},
		commands: [{type: 'camera.publish', operationId: allocated.operationId, options: desired}],
	};
}

function failCamera(
	snapshot: VoiceEngineV2Snapshot,
	operationId: VoiceEngineV2OperationId,
	error: VoiceEngineV2Error,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'failCamera snapshot must not be null');
	assert.ok(Number.isInteger(operationId), 'failCamera operationId must be an integer');
	assert.ok(error != null, 'failCamera error must not be null');
	const state = snapshot.camera;
	return {
		snapshot: {
			...snapshot,
			camera: applyMediaFailure(state, operationId, error),
			lastFailure: state.operationId === operationId ? error : snapshot.lastFailure,
		},
		commands: [],
	};
}

function beginCameraEncodingUpdate(
	snapshot: VoiceEngineV2Snapshot,
	options: VoiceEngineV2CameraEncodingOptions,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onCameraUpdateEncodingRequested snapshot must not be null');
	assert.ok(options != null, 'onCameraUpdateEncodingRequested options must not be null');
	const plan = planVoiceEngineV2CameraEncodingChange({
		published: snapshot.camera.published,
		desired: snapshot.camera.desired,
		update: options,
	});
	if (plan.action === 'reject' || !plan.desired) {
		const error =
			plan.error ?? invalidArgument('Cannot update camera encoding without a matching published camera', 'camera');
		return {
			snapshot: {...snapshot, camera: {...snapshot.camera, failure: error}, lastFailure: error},
			commands: [],
		};
	}
	if (plan.action === 'noop') {
		return {snapshot: {...snapshot, camera: {...snapshot.camera, desired: plan.desired}}, commands: []};
	}
	if (!isConnected(snapshot)) {
		return {snapshot: {...snapshot, camera: {...snapshot.camera, desired: plan.desired}}, commands: []};
	}
	const allocated = allocateOperation({...snapshot, camera: {...snapshot.camera, desired: plan.desired}});
	const command =
		plan.action === 'republish'
			? ({type: 'camera.publish', operationId: allocated.operationId, options: plan.desired} as const)
			: ({type: 'camera.updateEncoding', operationId: allocated.operationId, options} as const);
	return {
		snapshot: {
			...allocated.snapshot,
			camera: {...allocated.snapshot.camera, status: 'publishing', operationId: allocated.operationId, failure: null},
		},
		commands: [command],
	};
}

function beginCameraUnpublish(
	snapshot: VoiceEngineV2Snapshot,
	options: VoiceEngineV2CameraOptions | undefined,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'beginCameraUnpublish snapshot must not be null');
	assert.ok(options === undefined || typeof options === 'object', 'beginCameraUnpublish options must be an object');
	const base = {
		...snapshot,
		camera: {
			...snapshot.camera,
			desired: null,
		},
	};
	if (!isConnected(base) || (snapshot.camera.status === 'idle' && snapshot.camera.published == null)) {
		return {snapshot: base, commands: []};
	}
	const allocated = allocateOperation(base);
	return {
		snapshot: {
			...allocated.snapshot,
			camera: {
				...allocated.snapshot.camera,
				status: 'unpublishing',
				operationId: allocated.operationId,
				failure: null,
			},
		},
		commands: [{type: 'camera.unpublish', operationId: allocated.operationId, options}],
	};
}

export function transitionCamera(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2CameraEvent,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionCamera snapshot must not be null');
	assert.ok(event != null, 'transitionCamera event must not be null');
	assert.equal(typeof event.type, 'string', 'camera event type must be a string');
	assert.ok(event.type.startsWith('camera.'), 'camera reducer received unrelated event');
	switch (event.type) {
		case 'camera.publishRequested':
			if (isDuplicateInFlightCameraPublish(snapshot, event.options)) return {snapshot, commands: []};
			return beginCameraPublish({...snapshot, camera: {...snapshot.camera, desired: event.options}});
		case 'camera.publishSucceeded':
			return {
				snapshot: {...snapshot, camera: applyMediaSuccess(snapshot.camera, event.operationId)},
				commands: [],
			};
		case 'camera.publishFailed':
			return failCamera(snapshot, event.operationId, event.error);
		case 'camera.updateEncodingRequested':
			return beginCameraEncodingUpdate(snapshot, event.options);
		case 'camera.updateEncodingSucceeded':
			return {
				snapshot: {...snapshot, camera: applyMediaSuccess(snapshot.camera, event.operationId)},
				commands: [],
			};
		case 'camera.updateEncodingFailed':
			return failCamera(snapshot, event.operationId, event.error);
		case 'camera.unpublishRequested':
			return beginCameraUnpublish(snapshot, event.options);
		case 'camera.unpublishSucceeded':
			return {
				snapshot: {...snapshot, camera: completeUnpublish(snapshot.camera, event.operationId)},
				commands: [],
			};
		case 'camera.unpublishFailed':
			return failUnpublish(snapshot, event.operationId, event.error, 'camera');
	}
}
