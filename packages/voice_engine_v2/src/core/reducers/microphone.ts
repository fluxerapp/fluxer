// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {
	getVoiceEngineV2MicrophoneOperationFailureAction,
	type VoiceEngineV2MicrophoneOperationFailureAction,
} from '../../policies/microphoneFailureAction';
import type {VoiceEngineV2Event} from '../../protocol/events';
import type {
	VoiceEngineV2AudioControls,
	VoiceEngineV2Error,
	VoiceEngineV2MicrophoneOptions,
	VoiceEngineV2OperationId,
	VoiceEngineV2PermissionResult,
} from '../../protocol/types';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '../state';
import {
	allocateOperation,
	beginUnpublish,
	failUnpublish,
	isConnected,
	markOperation,
	unsupportedCapability,
} from './_helpers';
import {applyMediaFailure, completeUnpublish} from './_media';

type VoiceEngineV2MicrophoneEvent = Extract<
	VoiceEngineV2Event,
	{
		type: `microphone.${string}` | 'localAudio.muteRequested' | 'localAudio.deafenRequested' | 'audioControls.changed';
	}
>;

function sameMicrophoneOptions(
	a: VoiceEngineV2MicrophoneOptions | null,
	b: VoiceEngineV2MicrophoneOptions | null,
): boolean {
	assert.ok(a !== undefined, 'sameMicrophoneOptions a must not be undefined');
	assert.ok(b !== undefined, 'sameMicrophoneOptions b must not be undefined');
	return (
		a?.deviceId === b?.deviceId &&
		a?.echoCancellation === b?.echoCancellation &&
		a?.noiseSuppression === b?.noiseSuppression &&
		a?.autoGainControl === b?.autoGainControl &&
		a?.deepFilter === b?.deepFilter &&
		a?.deepFilterNoiseReductionLevel === b?.deepFilterNoiseReductionLevel &&
		a?.maxBitrateBps === b?.maxBitrateBps
	);
}

function isMicrophonePublished(snapshot: VoiceEngineV2Snapshot): boolean {
	assert.ok(snapshot != null, 'isMicrophonePublished snapshot must not be null');
	assert.ok(snapshot.microphone != null, 'isMicrophonePublished snapshot.microphone must not be null');
	return snapshot.microphone.status === 'published' && snapshot.microphone.published != null;
}

function computeEffectiveMicrophoneEnabled(audioControls: VoiceEngineV2AudioControls): boolean {
	assert.ok(audioControls != null, 'computeEffectiveMicrophoneEnabled audioControls must not be null');
	assert.equal(typeof audioControls.mode, 'string', 'audioControls.mode must be a string');
	if (audioControls.locallyMuted || audioControls.locallyDeafened || audioControls.mutedByPermission) return false;
	if (audioControls.mode === 'pushToTalk') return audioControls.pushToTalkActive;
	if (audioControls.mode === 'pushToMute') return !audioControls.pushToMuteActive;
	return true;
}

function computeLocalSpeakingOverride(
	snapshot: VoiceEngineV2Snapshot,
	effectiveMicrophoneEnabled: boolean,
): boolean | null {
	assert.ok(snapshot != null, 'computeLocalSpeakingOverride snapshot must not be null');
	assert.equal(typeof effectiveMicrophoneEnabled, 'boolean', 'effectiveMicrophoneEnabled must be a boolean');
	const hasPublication = isMicrophonePublished(snapshot);
	const effectivelyMuted =
		!hasPublication ||
		!effectiveMicrophoneEnabled ||
		snapshot.audioControls.locallyMuted ||
		snapshot.audioControls.locallyDeafened ||
		snapshot.audioControls.mutedByPermission;
	if (snapshot.audioControls.mode === 'pushToTalk') return !effectivelyMuted;
	if (snapshot.audioControls.mode === 'pushToMute' && snapshot.audioControls.pushToMuteActive) return false;
	if (effectivelyMuted) return false;
	return null;
}

function refreshMicrophoneDerivedState(snapshot: VoiceEngineV2Snapshot): VoiceEngineV2Snapshot {
	assert.ok(snapshot != null, 'refreshMicrophoneDerivedState snapshot must not be null');
	assert.ok(snapshot.audioControls != null, 'refreshMicrophoneDerivedState snapshot.audioControls must not be null');
	const enabled = computeEffectiveMicrophoneEnabled(snapshot.audioControls);
	return {
		...snapshot,
		microphone: {
			...snapshot.microphone,
			enabled,
			localSpeakingOverride: computeLocalSpeakingOverride(snapshot, enabled),
		},
	};
}

function planMicrophoneEnabled(snapshot: VoiceEngineV2Snapshot, forceCommand = false): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'planMicrophoneEnabled snapshot must not be null');
	assert.equal(typeof forceCommand, 'boolean', 'planMicrophoneEnabled forceCommand must be a boolean');
	const enabled = computeEffectiveMicrophoneEnabled(snapshot.audioControls);
	const previousEnabled = snapshot.microphone.enabled;
	const refreshed = refreshMicrophoneDerivedState(snapshot);
	if (!isConnected(refreshed) || !isMicrophonePublished(refreshed) || !refreshed.capabilities.microphone) {
		return {snapshot: refreshed, commands: []};
	}
	if (!forceCommand && previousEnabled === enabled) return {snapshot: refreshed, commands: []};
	if (forceCommand && enabled) return {snapshot: refreshed, commands: []};
	const allocated = allocateOperation(refreshed);
	return {
		snapshot: {
			...allocated.snapshot,
			microphone: {
				...allocated.snapshot.microphone,
				setEnabledOperationId: allocated.operationId,
				failure: null,
			},
		},
		commands: [{type: 'microphone.setEnabled', operationId: allocated.operationId, enabled}],
	};
}

function setLocalAudioMute(snapshot: VoiceEngineV2Snapshot, muted: boolean): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'setLocalAudioMute snapshot must not be null');
	assert.equal(typeof muted, 'boolean', 'setLocalAudioMute muted must be a boolean');
	const current = snapshot.audioControls;
	const controls: VoiceEngineV2AudioControls = {
		...current,
		preferredLocallyMuted: muted,
		hasUserSetMute: true,
		locallyMuted: current.mutedByPermission ? true : muted,
		locallyDeafened: current.locallyDeafened && muted ? current.locallyDeafened : false,
		hasUserSetDeaf: current.locallyDeafened && !muted ? true : current.hasUserSetDeaf,
		shouldUnmuteOnUndeafen: current.locallyDeafened && !muted ? false : current.shouldUnmuteOnUndeafen,
	};
	return planMicrophoneEnabled({...snapshot, audioControls: controls});
}

function setLocalAudioDeafen(snapshot: VoiceEngineV2Snapshot, deafened: boolean): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'setLocalAudioDeafen snapshot must not be null');
	assert.equal(typeof deafened, 'boolean', 'setLocalAudioDeafen deafened must be a boolean');
	const current = snapshot.audioControls;
	const controls: VoiceEngineV2AudioControls = deafened
		? {
				...current,
				locallyMuted: true,
				locallyDeafened: true,
				hasUserSetDeaf: true,
				shouldUnmuteOnUndeafen: !current.locallyMuted && !current.mutedByPermission,
			}
		: {
				...current,
				locallyMuted: current.shouldUnmuteOnUndeafen && !current.mutedByPermission ? false : current.locallyMuted,
				preferredLocallyMuted:
					current.shouldUnmuteOnUndeafen && !current.mutedByPermission ? false : current.preferredLocallyMuted,
				locallyDeafened: false,
				hasUserSetDeaf: true,
				shouldUnmuteOnUndeafen: false,
			};
	return planMicrophoneEnabled({...snapshot, audioControls: controls});
}

export function applyMicrophonePermissionResult(
	snapshot: VoiceEngineV2Snapshot,
	result: VoiceEngineV2PermissionResult,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'applyMicrophonePermissionResult snapshot must not be null');
	assert.ok(result != null, 'applyMicrophonePermissionResult result must not be null');
	if (result.name !== 'microphone') return {snapshot, commands: []};
	if (result.status === 'denied' || result.status === 'restricted') {
		return planMicrophoneEnabled({
			...snapshot,
			audioControls: {
				...snapshot.audioControls,
				locallyMuted: true,
				mutedByPermission: true,
			},
		});
	}
	if (result.status !== 'granted') return {snapshot: refreshMicrophoneDerivedState(snapshot), commands: []};
	const locallyMuted = snapshot.audioControls.hasUserSetMute ? snapshot.audioControls.preferredLocallyMuted : false;
	return planMicrophoneEnabled({
		...snapshot,
		audioControls: {
			...snapshot.audioControls,
			locallyMuted,
			mutedByPermission: false,
		},
	});
}

function microphoneOptionsWithAudioInputDevice(
	options: VoiceEngineV2MicrophoneOptions,
	deviceId: string | null,
): VoiceEngineV2MicrophoneOptions {
	assert.ok(options != null, 'microphoneOptionsWithAudioInputDevice options must not be null');
	assert.ok(deviceId === null || typeof deviceId === 'string', 'deviceId must be string or null');
	if (deviceId == null) {
		const {deviceId: _deviceId, ...rest} = options;
		return rest;
	}
	return {...options, deviceId};
}

export function planMicrophoneDeviceChange(
	snapshot: VoiceEngineV2Snapshot,
	deviceId: string | null,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'planMicrophoneDeviceChange snapshot must not be null');
	assert.ok(deviceId === null || typeof deviceId === 'string', 'deviceId must be string or null');
	const current = snapshot.microphone.desired ?? snapshot.microphone.published;
	if (!current) return {snapshot, commands: []};
	const desired = microphoneOptionsWithAudioInputDevice(current, deviceId);
	if (sameMicrophoneOptions(snapshot.microphone.desired, desired)) return {snapshot, commands: []};
	return beginMicrophonePublish({
		...snapshot,
		microphone: {
			...snapshot.microphone,
			desired,
		},
	});
}

function isMicrophoneFailureDuringReconnect(snapshot: VoiceEngineV2Snapshot): boolean {
	assert.ok(snapshot != null, 'isMicrophoneFailureDuringReconnect snapshot must not be null');
	assert.ok(snapshot.connection != null, 'isMicrophoneFailureDuringReconnect snapshot.connection must not be null');
	if (snapshot.connection.status === 'reconnecting') return true;
	return snapshot.liveKit.connectionState === 'reconnecting';
}

function microphoneFailureAction(
	snapshot: VoiceEngineV2Snapshot,
	error: VoiceEngineV2Error,
	requestedEnabled: boolean,
): VoiceEngineV2MicrophoneOperationFailureAction {
	assert.ok(error != null, 'microphoneFailureAction error must not be null');
	assert.equal(typeof requestedEnabled, 'boolean', 'requestedEnabled must be a boolean');
	return getVoiceEngineV2MicrophoneOperationFailureAction(
		{
			ok: false,
			error: {
				code: error.code,
				message: error.message,
				capability: typeof error.capability === 'string' ? error.capability : undefined,
			},
		},
		requestedEnabled,
		{reconnecting: isMicrophoneFailureDuringReconnect(snapshot)},
	);
}

function applyMicrophoneFailureRecovery(
	snapshot: VoiceEngineV2Snapshot,
	error: VoiceEngineV2Error,
	requestedEnabled: boolean,
): VoiceEngineV2Snapshot {
	assert.ok(snapshot != null, 'applyMicrophoneFailureRecovery snapshot must not be null');
	assert.ok(error != null, 'applyMicrophoneFailureRecovery error must not be null');
	const action = microphoneFailureAction(snapshot, error, requestedEnabled);
	if (action !== 'mute') return snapshot;
	return refreshMicrophoneDerivedState({
		...snapshot,
		audioControls: {
			...snapshot.audioControls,
			locallyMuted: true,
			preferredLocallyMuted: true,
			hasUserSetMute: true,
		},
	});
}

export function beginMicrophonePublish(snapshot: VoiceEngineV2Snapshot): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'beginMicrophonePublish snapshot must not be null');
	assert.ok(snapshot.microphone != null, 'beginMicrophonePublish snapshot.microphone must not be null');
	const desired = snapshot.microphone.desired;
	if (!desired) return {snapshot, commands: []};
	if (!snapshot.capabilities.microphone) {
		const error = unsupportedCapability('microphone');
		return {
			snapshot: {
				...snapshot,
				microphone: {...snapshot.microphone, status: 'failed', failure: error},
				lastFailure: error,
			},
			commands: [],
		};
	}
	if (!isConnected(snapshot)) return {snapshot, commands: []};
	if (snapshot.microphone.status === 'publishing') return {snapshot, commands: []};
	if (snapshot.microphone.status === 'published' && sameMicrophoneOptions(snapshot.microphone.published, desired)) {
		return {snapshot, commands: []};
	}
	const allocated = allocateOperation(snapshot);
	return {
		snapshot: {
			...allocated.snapshot,
			microphone: {
				...allocated.snapshot.microphone,
				status: 'publishing',
				operationId: allocated.operationId,
				failure: null,
			},
		},
		commands: [{type: 'microphone.publish', operationId: allocated.operationId, options: desired}],
	};
}

export function transitionMicrophone(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2MicrophoneEvent,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionMicrophone snapshot must not be null');
	assert.ok(event != null, 'transitionMicrophone event must not be null');
	assert.equal(typeof event.type, 'string', 'microphone event type must be a string');
	assert.ok(
		event.type.startsWith('microphone.') ||
			event.type === 'localAudio.muteRequested' ||
			event.type === 'localAudio.deafenRequested' ||
			event.type === 'audioControls.changed',
		'microphone reducer received unrelated event',
	);
	switch (event.type) {
		case 'microphone.publishRequested':
			return beginMicrophonePublish({
				...snapshot,
				microphone: {...snapshot.microphone, desired: event.options},
			});
		case 'microphone.publishSucceeded':
			return onPublishSucceeded(snapshot, event.operationId);
		case 'microphone.publishFailed':
			return onPublishFailed(snapshot, event.operationId, event.error);
		case 'microphone.unpublishRequested':
			return beginUnpublish(snapshot, 'microphone', 'microphone.unpublish');
		case 'microphone.unpublishSucceeded':
			return onUnpublishSucceeded(snapshot, event.operationId);
		case 'microphone.unpublishFailed':
			return failUnpublish(snapshot, event.operationId, event.error, 'microphone');
		case 'microphone.setEnabledRequested':
			return onSetEnabledRequested(snapshot, event.enabled);
		case 'microphone.setEnabledSucceeded':
			return onSetEnabledSucceeded(snapshot, event.operationId);
		case 'microphone.setEnabledFailed':
			return onSetEnabledFailed(snapshot, event.operationId, event.error);
		case 'localAudio.muteRequested':
			return setLocalAudioMute(snapshot, event.muted);
		case 'localAudio.deafenRequested':
			return setLocalAudioDeafen(snapshot, event.deafened);
		case 'audioControls.changed':
			return planMicrophoneEnabled({
				...snapshot,
				audioControls: {...snapshot.audioControls, ...event.controls},
			});
	}
}

function onPublishSucceeded(
	snapshot: VoiceEngineV2Snapshot,
	operationId: VoiceEngineV2OperationId,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onPublishSucceeded snapshot must not be null');
	assert.ok(Number.isInteger(operationId), 'onPublishSucceeded operationId must be an integer');
	if (snapshot.microphone.operationId !== operationId) return {snapshot, commands: []};
	return planMicrophoneEnabled(
		{
			...markOperation(snapshot, operationId, 'succeeded'),
			microphone: {
				...snapshot.microphone,
				status: snapshot.microphone.desired ? 'published' : 'idle',
				published: snapshot.microphone.desired,
				operationId: null,
				failure: null,
			},
		},
		true,
	);
}

function onPublishFailed(
	snapshot: VoiceEngineV2Snapshot,
	operationId: VoiceEngineV2OperationId,
	error: VoiceEngineV2Error,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onPublishFailed snapshot must not be null');
	assert.ok(Number.isInteger(operationId), 'onPublishFailed operationId must be an integer');
	assert.ok(error != null, 'onPublishFailed error must not be null');
	if (snapshot.microphone.operationId !== operationId) return {snapshot, commands: []};
	const failed = {
		...markOperation(snapshot, operationId, 'failed', error),
		microphone: {
			...applyMediaFailure(snapshot.microphone, operationId, error),
			enabled: snapshot.microphone.enabled,
			localSpeakingOverride: snapshot.microphone.localSpeakingOverride,
			setEnabledOperationId: snapshot.microphone.setEnabledOperationId,
		},
		lastFailure: error,
	};
	return {
		snapshot: applyMicrophoneFailureRecovery(failed, error, snapshot.microphone.enabled),
		commands: [],
	};
}

function onUnpublishSucceeded(
	snapshot: VoiceEngineV2Snapshot,
	operationId: VoiceEngineV2OperationId,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onUnpublishSucceeded snapshot must not be null');
	assert.ok(Number.isInteger(operationId), 'onUnpublishSucceeded operationId must be an integer');
	return {
		snapshot: {
			...snapshot,
			microphone: {
				...completeUnpublish(snapshot.microphone, operationId),
				enabled: snapshot.microphone.enabled,
				localSpeakingOverride: false,
				setEnabledOperationId: snapshot.microphone.setEnabledOperationId,
			},
		},
		commands: [],
	};
}

function onSetEnabledRequested(snapshot: VoiceEngineV2Snapshot, enabled: boolean): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onSetEnabledRequested snapshot must not be null');
	assert.equal(typeof enabled, 'boolean', 'onSetEnabledRequested enabled must be a boolean');
	const base = {
		...snapshot,
		microphone: {
			...snapshot.microphone,
			enabled,
		},
	};
	if (!isConnected(base) || !base.capabilities.microphone) return {snapshot: base, commands: []};
	if (!isMicrophonePublished(base) && base.microphone.status !== 'publishing') return {snapshot: base, commands: []};
	const allocated = allocateOperation(base);
	return {
		snapshot: {
			...allocated.snapshot,
			microphone: {
				...allocated.snapshot.microphone,
				setEnabledOperationId: allocated.operationId,
			},
		},
		commands: [{type: 'microphone.setEnabled', operationId: allocated.operationId, enabled}],
	};
}

function onSetEnabledSucceeded(
	snapshot: VoiceEngineV2Snapshot,
	operationId: VoiceEngineV2OperationId,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onSetEnabledSucceeded snapshot must not be null');
	assert.ok(Number.isInteger(operationId), 'onSetEnabledSucceeded operationId must be an integer');
	if (snapshot.microphone.setEnabledOperationId !== operationId) return {snapshot, commands: []};
	return {
		snapshot: {
			...markOperation(snapshot, operationId, 'succeeded'),
			microphone: {...snapshot.microphone, setEnabledOperationId: null, failure: null},
		},
		commands: [],
	};
}

function onSetEnabledFailed(
	snapshot: VoiceEngineV2Snapshot,
	operationId: VoiceEngineV2OperationId,
	error: VoiceEngineV2Error,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onSetEnabledFailed snapshot must not be null');
	assert.ok(Number.isInteger(operationId), 'onSetEnabledFailed operationId must be an integer');
	assert.ok(error != null, 'onSetEnabledFailed error must not be null');
	if (snapshot.microphone.setEnabledOperationId !== operationId) return {snapshot, commands: []};
	return {
		snapshot: applyMicrophoneFailureRecovery(
			{
				...markOperation(snapshot, operationId, 'failed', error),
				microphone: {...snapshot.microphone, setEnabledOperationId: null, failure: error},
				lastFailure: error,
			},
			error,
			snapshot.microphone.enabled,
		),
		commands: [],
	};
}
