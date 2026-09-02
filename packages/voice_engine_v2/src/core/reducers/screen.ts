// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {planVoiceEngineV2ScreenEncodingChange} from '../../policies/screenShare';
import type {VoiceEngineV2Event} from '../../protocol/events';
import type {
	VoiceEngineV2Error,
	VoiceEngineV2OperationId,
	VoiceEngineV2ScreenEncodingOptions,
	VoiceEngineV2ScreenOptions,
} from '../../protocol/types';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '../state';
import {
	allocateOperation,
	beginUnpublish,
	failUnpublish,
	invalidArgument,
	isConnected,
	unsupportedCapability,
} from './_helpers';
import {applyMediaFailure, applyMediaSuccess, completeUnpublish} from './_media';
import {nativeZeroCopyRequiredError, unavailableZeroCopyTransportError} from './_zeroCopy';

type VoiceEngineV2ScreenEvent = Extract<VoiceEngineV2Event, {type: `screen.${string}`}>;

function sameScreenOptions(a: VoiceEngineV2ScreenOptions | null, b: VoiceEngineV2ScreenOptions | null): boolean {
	assert.ok(a !== undefined, 'sameScreenOptions a must not be undefined');
	assert.ok(b !== undefined, 'sameScreenOptions b must not be undefined');
	return (
		a?.captureId === b?.captureId &&
		a?.width === b?.width &&
		a?.height === b?.height &&
		a?.codec === b?.codec &&
		a?.hardwareEncoding === b?.hardwareEncoding &&
		a?.zeroCopyRequired === b?.zeroCopyRequired &&
		a?.maxBitrateBps === b?.maxBitrateBps &&
		a?.maxFramerate === b?.maxFramerate &&
		a?.adaptiveSend === b?.adaptiveSend &&
		a?.minVideoFps === b?.minVideoFps &&
		a?.maxAudioBufferMs === b?.maxAudioBufferMs &&
		a?.pacing === b?.pacing
	);
}

function validateScreenPublish(
	snapshot: VoiceEngineV2Snapshot,
	desired: VoiceEngineV2ScreenOptions,
): VoiceEngineV2Error | null {
	assert.ok(snapshot != null, 'validateScreenPublish snapshot must not be null');
	assert.ok(desired != null, 'validateScreenPublish desired must not be null');
	if (desired.zeroCopyRequired === true && !snapshot.capabilities.zeroCopyScreenTransport) {
		return unavailableZeroCopyTransportError('capture');
	}
	if (desired.hardwareEncoding === true) {
		if (!snapshot.capabilities.hardwareEncoding) {
			return unsupportedCapability('hardwareEncoding');
		}
		if (desired.zeroCopyRequired !== true) {
			return nativeZeroCopyRequiredError('hardwareEncoder');
		}
		if (!snapshot.capabilities.zeroCopyScreenTransport) {
			return unavailableZeroCopyTransportError('hardwareEncoder');
		}
	}
	return null;
}

export function beginScreenPublish(snapshot: VoiceEngineV2Snapshot): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'beginScreenPublish snapshot must not be null');
	assert.ok(snapshot.screen != null, 'beginScreenPublish snapshot.screen must not be null');
	const desired = snapshot.screen.desired;
	if (!desired) return {snapshot, commands: []};
	if (!snapshot.capabilities.screen) {
		const error = unsupportedCapability('screen');
		return {
			snapshot: {
				...snapshot,
				screen: {...snapshot.screen, status: 'failed', failure: error},
				lastFailure: error,
			},
			commands: [],
		};
	}
	const validationError = validateScreenPublish(snapshot, desired);
	if (validationError) {
		return {
			snapshot: {
				...snapshot,
				screen: {...snapshot.screen, status: 'failed', failure: validationError},
				lastFailure: validationError,
			},
			commands: [],
		};
	}
	if (!isConnected(snapshot)) return {snapshot, commands: []};
	if (snapshot.screen.status === 'published' && sameScreenOptions(snapshot.screen.published, desired)) {
		return {snapshot, commands: []};
	}
	const allocated = allocateOperation(snapshot);
	return {
		snapshot: {
			...allocated.snapshot,
			screen: {
				...allocated.snapshot.screen,
				status: 'publishing',
				operationId: allocated.operationId,
				failure: null,
			},
		},
		commands: [{type: 'screen.publish', operationId: allocated.operationId, options: desired}],
	};
}

function failScreen(
	snapshot: VoiceEngineV2Snapshot,
	operationId: VoiceEngineV2OperationId,
	error: VoiceEngineV2Error,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'failScreen snapshot must not be null');
	assert.ok(Number.isInteger(operationId), 'failScreen operationId must be an integer');
	assert.ok(error != null, 'failScreen error must not be null');
	const state = snapshot.screen;
	return {
		snapshot: {
			...snapshot,
			screen: applyMediaFailure(state, operationId, error),
			lastFailure: state.operationId === operationId ? error : snapshot.lastFailure,
		},
		commands: [],
	};
}

function beginScreenEncodingUpdate(
	snapshot: VoiceEngineV2Snapshot,
	options: VoiceEngineV2ScreenEncodingOptions,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onUpdateEncodingRequested snapshot must not be null');
	assert.ok(options != null, 'onUpdateEncodingRequested options must not be null');
	const plan = planVoiceEngineV2ScreenEncodingChange({
		published: snapshot.screen.published,
		desired: snapshot.screen.desired,
		update: options,
	});
	if (plan.action === 'reject' || !plan.desired) {
		const error =
			plan.error ?? invalidArgument('Cannot update screen encoding without a matching published screen', 'screen');
		return {
			snapshot: {...snapshot, screen: {...snapshot.screen, failure: error}, lastFailure: error},
			commands: [],
		};
	}
	const validationError = validateScreenPublish(snapshot, plan.desired);
	if (validationError) {
		return {
			snapshot: {
				...snapshot,
				screen: {...snapshot.screen, status: 'failed', desired: plan.desired, failure: validationError},
				lastFailure: validationError,
			},
			commands: [],
		};
	}
	if (plan.action === 'noop') {
		return {snapshot: {...snapshot, screen: {...snapshot.screen, desired: plan.desired}}, commands: []};
	}
	if (!isConnected(snapshot)) {
		return {snapshot: {...snapshot, screen: {...snapshot.screen, desired: plan.desired}}, commands: []};
	}
	const allocated = allocateOperation({...snapshot, screen: {...snapshot.screen, desired: plan.desired}});
	const command =
		plan.action === 'republish'
			? ({type: 'screen.publish', operationId: allocated.operationId, options: plan.desired} as const)
			: ({type: 'screen.updateEncoding', operationId: allocated.operationId, options} as const);
	return {
		snapshot: {
			...allocated.snapshot,
			screen: {...allocated.snapshot.screen, status: 'publishing', operationId: allocated.operationId},
		},
		commands: [command],
	};
}

export function transitionScreen(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2ScreenEvent,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionScreen snapshot must not be null');
	assert.ok(event != null, 'transitionScreen event must not be null');
	assert.equal(typeof event.type, 'string', 'screen event type must be a string');
	assert.ok(event.type.startsWith('screen.'), 'screen reducer received unrelated event');
	switch (event.type) {
		case 'screen.publishRequested':
			return beginScreenPublish({...snapshot, screen: {...snapshot.screen, desired: event.options}});
		case 'screen.publishSucceeded':
			return {
				snapshot: {...snapshot, screen: applyMediaSuccess(snapshot.screen, event.operationId)},
				commands: [],
			};
		case 'screen.publishFailed':
			return failScreen(snapshot, event.operationId, event.error);
		case 'screen.updateEncodingRequested':
			return beginScreenEncodingUpdate(snapshot, event.options);
		case 'screen.updateEncodingSucceeded':
			return {
				snapshot: {...snapshot, screen: applyMediaSuccess(snapshot.screen, event.operationId)},
				commands: [],
			};
		case 'screen.updateEncodingFailed':
			return failScreen(snapshot, event.operationId, event.error);
		case 'screen.unpublishRequested':
			return beginUnpublish(snapshot, 'screen', 'screen.unpublish');
		case 'screen.unpublishSucceeded':
			return {
				snapshot: {...snapshot, screen: completeUnpublish(snapshot.screen, event.operationId)},
				commands: [],
			};
		case 'screen.unpublishFailed':
			return failUnpublish(snapshot, event.operationId, event.error, 'screen');
	}
}
