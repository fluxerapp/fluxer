// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {
	allocateOperation,
	appendTransition,
	markOperation,
	queueCommand,
} from '@fluxer/voice_engine_v2/src/core/reducers/_helpers';
import {planMicrophoneDeviceChange} from '@fluxer/voice_engine_v2/src/core/reducers/microphone';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Command} from '@fluxer/voice_engine_v2/src/protocol/commands';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import type {
	VoiceEngineV2DeviceInventory,
	VoiceEngineV2Error,
	VoiceEngineV2OperationId,
} from '@fluxer/voice_engine_v2/src/protocol/types';

type VoiceEngineV2DevicesEvent = Extract<VoiceEngineV2Event, {type: `devices.${string}`}>;

function beginDeviceSelection(
	snapshot: VoiceEngineV2Snapshot,
	commandType: 'devices.selectAudioInput' | 'devices.selectAudioOutput' | 'devices.selectCamera',
	deviceId: string | null,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'beginDeviceSelection snapshot must not be null');
	assert.equal(typeof commandType, 'string', 'beginDeviceSelection commandType must be a string');
	const allocated = allocateOperation(snapshot);
	const inventory = {
		...allocated.snapshot.devices.inventory,
		selectedAudioInputId:
			commandType === 'devices.selectAudioInput' ? deviceId : allocated.snapshot.devices.inventory.selectedAudioInputId,
		selectedAudioOutputId:
			commandType === 'devices.selectAudioOutput'
				? deviceId
				: allocated.snapshot.devices.inventory.selectedAudioOutputId,
		selectedCameraId:
			commandType === 'devices.selectCamera' ? deviceId : allocated.snapshot.devices.inventory.selectedCameraId,
	};
	return queueCommand(
		{
			...allocated.snapshot,
			devices: {...allocated.snapshot.devices, inventory, operationId: allocated.operationId, failure: null},
		},
		{type: commandType, operationId: allocated.operationId, deviceId} as VoiceEngineV2Command,
	);
}

export function transitionDevices(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2DevicesEvent,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'transitionDevices snapshot must not be null');
	assert.ok(event != null, 'transitionDevices event must not be null');
	assert.equal(typeof event.type, 'string', 'devices event type must be a string');
	assert.ok(event.type.startsWith('devices.'), 'devices reducer received unrelated event');
	switch (event.type) {
		case 'devices.enumerateRequested':
			return onEnumerateRequested(snapshot);
		case 'devices.changed':
			return onChanged(snapshot, event.operationId, event.devices);
		case 'devices.enumerateFailed':
			return onEnumerateFailed(snapshot, event.operationId, event.error);
		case 'devices.selectAudioInputRequested': {
			const selection = beginDeviceSelection(snapshot, 'devices.selectAudioInput', event.deviceId);
			return appendTransition(selection, planMicrophoneDeviceChange(selection.snapshot, event.deviceId));
		}
		case 'devices.selectAudioOutputRequested':
			return beginDeviceSelection(snapshot, 'devices.selectAudioOutput', event.deviceId);
		case 'devices.selectCameraRequested':
			return beginDeviceSelection(snapshot, 'devices.selectCamera', event.deviceId);
	}
}

function onEnumerateRequested(snapshot: VoiceEngineV2Snapshot): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onEnumerateRequested snapshot must not be null');
	assert.ok(snapshot.devices != null, 'onEnumerateRequested snapshot.devices must not be null');
	const allocated = allocateOperation(snapshot);
	return queueCommand(
		{
			...allocated.snapshot,
			devices: {...allocated.snapshot.devices, operationId: allocated.operationId, failure: null},
		},
		{type: 'devices.enumerate', operationId: allocated.operationId},
	);
}

function onChanged(
	snapshot: VoiceEngineV2Snapshot,
	operationId: VoiceEngineV2OperationId | null,
	devices: VoiceEngineV2DeviceInventory,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onChanged snapshot must not be null');
	assert.ok(devices != null, 'onChanged devices must not be null');
	if (operationId != null && snapshot.devices.operationId !== operationId) return {snapshot, commands: []};
	const previousAudioInputId = snapshot.devices.inventory.selectedAudioInputId;
	const base = {
		...(operationId != null ? markOperation(snapshot, operationId, 'succeeded') : snapshot),
		devices: {
			inventory: devices,
			operationId: operationId === snapshot.devices.operationId ? null : snapshot.devices.operationId,
			failure: null,
		},
	};
	if (previousAudioInputId === devices.selectedAudioInputId) return {snapshot: base, commands: []};
	return planMicrophoneDeviceChange(base, devices.selectedAudioInputId);
}

function onEnumerateFailed(
	snapshot: VoiceEngineV2Snapshot,
	operationId: VoiceEngineV2OperationId,
	error: VoiceEngineV2Error,
): VoiceEngineV2Transition {
	assert.ok(snapshot != null, 'onEnumerateFailed snapshot must not be null');
	assert.ok(Number.isInteger(operationId), 'onEnumerateFailed operationId must be an integer');
	assert.ok(error != null, 'onEnumerateFailed error must not be null');
	if (snapshot.devices.operationId !== operationId) return {snapshot, commands: []};
	return {
		snapshot: {
			...markOperation(snapshot, operationId, 'failed', error),
			devices: {...snapshot.devices, operationId: null, failure: error},
			lastFailure: error,
		},
		commands: [],
	};
}
