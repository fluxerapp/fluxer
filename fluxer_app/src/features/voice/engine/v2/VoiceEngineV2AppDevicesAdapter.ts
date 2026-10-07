// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {
	type DevicePort,
	emptyVoiceEngineV2DeviceInventory,
	type VoiceEngineV2DeviceChangeReason,
	type VoiceEngineV2DeviceInventory,
	type VoiceEngineV2Error,
} from '@fluxer/voice_engine_v2';

const VOICE_ENGINE_V2_APP_DEVICES_MAX_DEVICES_PER_KIND = 256;

type VoiceEngineV2AppDevicesEnumerator = Pick<MediaDevices, 'enumerateDevices'>;

type VoiceEngineV2AppDevicesKind = 'audioInput' | 'audioOutput' | 'camera';

interface VoiceEngineV2AppDevicesChangedEvent {
	type: 'devices.changed';
	reason: VoiceEngineV2DeviceChangeReason;
	devices: VoiceEngineV2DeviceInventory;
	requiresPermission: boolean;
}

interface VoiceEngineV2AppDevicesSelectFailedEvent {
	type: 'devices.selectFailed';
	kind: VoiceEngineV2AppDevicesKind;
	deviceId: string | null;
	error: VoiceEngineV2Error;
}

type VoiceEngineV2AppDevicesEvent = VoiceEngineV2AppDevicesChangedEvent | VoiceEngineV2AppDevicesSelectFailedEvent;

type VoiceEngineV2AppDevicesListener = (event: VoiceEngineV2AppDevicesEvent) => void;

export interface VoiceEngineV2AppDevicesAdapterOptions {
	enumerator?: VoiceEngineV2AppDevicesEnumerator | null;
}

export interface VoiceEngineV2AppDevicesAdapter extends DevicePort {
	subscribe(listener: VoiceEngineV2AppDevicesListener): () => void;
	getLastInventory(): VoiceEngineV2DeviceInventory;
	getRequiresPermission(): boolean;
}

interface DevicesAdapterState {
	inventory: VoiceEngineV2DeviceInventory;
	requiresPermission: boolean;
	listeners: Set<VoiceEngineV2AppDevicesListener>;
}

export function createVoiceEngineV2AppDevicesAdapter(
	options: VoiceEngineV2AppDevicesAdapterOptions = {},
): VoiceEngineV2AppDevicesAdapter {
	assert.ok(options !== null && typeof options === 'object', 'devices adapter requires options object');
	const enumerator = resolveEnumerator(options.enumerator);
	const state: DevicesAdapterState = {
		inventory: emptyVoiceEngineV2DeviceInventory(),
		requiresPermission: false,
		listeners: new Set(),
	};
	return {
		enumerateDevices: () => enumerateDevices(state, enumerator),
		selectAudioInput: (deviceId) => applySelectionAsync(state, 'audioInput', deviceId),
		selectAudioOutput: (deviceId) => applySelectionAsync(state, 'audioOutput', deviceId),
		selectCamera: (deviceId) => applySelectionAsync(state, 'camera', deviceId),
		subscribe: (listener) => subscribe(state, listener),
		getLastInventory: () => {
			assertInventoryShape(state.inventory);
			return state.inventory;
		},
		getRequiresPermission: () => {
			assert.equal(typeof state.requiresPermission, 'boolean', 'requiresPermission must be boolean');
			return state.requiresPermission;
		},
	};
}

async function enumerateDevices(
	state: DevicesAdapterState,
	enumerator: VoiceEngineV2AppDevicesEnumerator | null,
): Promise<VoiceEngineV2DeviceInventory> {
	assert.ok(state !== null && typeof state === 'object', 'enumerateDevices requires state');
	assert.ok(
		enumerator === null || typeof enumerator.enumerateDevices === 'function',
		'enumerator must expose enumerateDevices',
	);
	if (enumerator === null) {
		state.inventory = emptyVoiceEngineV2DeviceInventory();
		state.requiresPermission = true;
		emitChanged(state, 'initial');
		return state.inventory;
	}
	const raw = await safeEnumerate(enumerator);
	const built = buildInventory(raw, state.inventory);
	assertInventoryShape(built.inventory);
	state.inventory = built.inventory;
	state.requiresPermission = built.requiresPermission;
	emitChanged(state, 'initial');
	return state.inventory;
}

async function applySelectionAsync(
	state: DevicesAdapterState,
	kind: VoiceEngineV2AppDevicesKind,
	deviceId: string | null,
): Promise<void> {
	assert.equal(typeof kind, 'string', 'selection kind must be a string');
	assert.ok(deviceId === null || typeof deviceId === 'string', 'deviceId must be a string or null');
	const next = applyInventorySelection(state.inventory, kind, deviceId);
	if (next.error !== null) {
		emit(state, {type: 'devices.selectFailed', kind, deviceId, error: next.error});
		return;
	}
	assertInventoryShape(next.inventory);
	state.inventory = next.inventory;
	emitChanged(state, 'selectionChanged');
}

function subscribe(state: DevicesAdapterState, listener: VoiceEngineV2AppDevicesListener): () => void {
	assert.equal(typeof listener, 'function', 'devices subscribe listener must be a function');
	state.listeners.add(listener);
	return () => {
		state.listeners.delete(listener);
	};
}

function emit(state: DevicesAdapterState, event: VoiceEngineV2AppDevicesEvent): void {
	assert.ok(event !== null && typeof event === 'object', 'devices adapter event must be an object');
	assert.equal(typeof event.type, 'string', 'devices adapter event.type must be a string');
	for (const listener of state.listeners) {
		listener(event);
	}
}

function emitChanged(state: DevicesAdapterState, reason: VoiceEngineV2DeviceChangeReason): void {
	assert.equal(typeof reason, 'string', 'devices changed reason must be a string');
	assert.ok(reason.length > 0, 'devices changed reason must not be empty');
	emit(state, {
		type: 'devices.changed',
		reason,
		devices: state.inventory,
		requiresPermission: state.requiresPermission,
	});
}

function resolveEnumerator(
	candidate: VoiceEngineV2AppDevicesEnumerator | null | undefined,
): VoiceEngineV2AppDevicesEnumerator | null {
	if (candidate === null) return null;
	if (candidate !== undefined) {
		if (typeof candidate.enumerateDevices !== 'function') return null;
		return candidate;
	}
	if (typeof navigator === 'undefined') return null;
	const media = navigator.mediaDevices;
	if (!media || typeof media.enumerateDevices !== 'function') return null;
	return media;
}

async function safeEnumerate(enumerator: VoiceEngineV2AppDevicesEnumerator): Promise<ReadonlyArray<MediaDeviceInfo>> {
	assert.equal(typeof enumerator.enumerateDevices, 'function', 'enumerator must expose enumerateDevices');
	try {
		const result = await enumerator.enumerateDevices();
		if (!Array.isArray(result)) return [];
		return result;
	} catch {
		return [];
	}
}

interface BuildResult {
	inventory: VoiceEngineV2DeviceInventory;
	requiresPermission: boolean;
}

function buildInventory(raw: ReadonlyArray<MediaDeviceInfo>, previous: VoiceEngineV2DeviceInventory): BuildResult {
	assert.ok(Array.isArray(raw), 'raw devices must be an array');
	assert.ok(raw.length <= VOICE_ENGINE_V2_APP_DEVICES_MAX_DEVICES_PER_KIND * 3, 'enumeration exceeds per-call cap');
	const audioInputs: VoiceEngineV2DeviceInventory['audioInputs'] = [];
	const audioOutputs: VoiceEngineV2DeviceInventory['audioOutputs'] = [];
	const cameras: VoiceEngineV2DeviceInventory['cameras'] = [];
	let labelled = 0;
	let total = 0;
	for (const info of raw) {
		if (info === null || typeof info !== 'object') continue;
		if (typeof info.deviceId !== 'string' || info.deviceId.length === 0) continue;
		const label = typeof info.label === 'string' ? info.label : '';
		if (label.length > 0) labelled += 1;
		total += 1;
		const isDefault = info.deviceId === 'default';
		pushDevice(info.kind, {deviceId: info.deviceId, label, isDefault}, audioInputs, audioOutputs, cameras);
	}
	const requiresPermission = total > 0 && labelled === 0;
	return {
		inventory: {
			audioInputs,
			audioOutputs,
			cameras,
			selectedAudioInputId: preserveSelection(previous.selectedAudioInputId, audioInputs),
			selectedAudioOutputId: preserveSelection(previous.selectedAudioOutputId, audioOutputs),
			selectedCameraId: preserveSelection(previous.selectedCameraId, cameras),
		},
		requiresPermission,
	};
}

interface PushDeviceShape {
	deviceId: string;
	label: string;
	isDefault: boolean;
}

function pushDevice(
	kind: MediaDeviceKind | string,
	shape: PushDeviceShape,
	audioInputs: VoiceEngineV2DeviceInventory['audioInputs'],
	audioOutputs: VoiceEngineV2DeviceInventory['audioOutputs'],
	cameras: VoiceEngineV2DeviceInventory['cameras'],
): void {
	assert.equal(typeof shape.deviceId, 'string', 'device shape requires a string deviceId');
	assert.equal(typeof shape.label, 'string', 'device shape requires a string label');
	switch (kind) {
		case 'audioinput':
			if (audioInputs.length < VOICE_ENGINE_V2_APP_DEVICES_MAX_DEVICES_PER_KIND) {
				audioInputs.push({deviceId: shape.deviceId, label: shape.label, isDefault: shape.isDefault});
			}
			return;
		case 'audiooutput':
			if (audioOutputs.length < VOICE_ENGINE_V2_APP_DEVICES_MAX_DEVICES_PER_KIND) {
				audioOutputs.push({deviceId: shape.deviceId, label: shape.label, isDefault: shape.isDefault});
			}
			return;
		case 'videoinput':
			if (cameras.length < VOICE_ENGINE_V2_APP_DEVICES_MAX_DEVICES_PER_KIND) {
				cameras.push({deviceId: shape.deviceId, label: shape.label});
			}
			return;
		default:
			return;
	}
}

function preserveSelection(previous: string | null, devices: ReadonlyArray<{deviceId: string}>): string | null {
	if (previous === null) return null;
	for (const device of devices) {
		if (device.deviceId === previous) return previous;
	}
	return null;
}

interface SelectionResult {
	inventory: VoiceEngineV2DeviceInventory;
	error: VoiceEngineV2Error | null;
}

function applyInventorySelection(
	inventory: VoiceEngineV2DeviceInventory,
	kind: VoiceEngineV2AppDevicesKind,
	deviceId: string | null,
): SelectionResult {
	assert.equal(typeof kind, 'string', 'selection kind must be a string');
	assert.ok(inventory !== null && typeof inventory === 'object', 'inventory must be an object');
	const list = pickList(inventory, kind);
	if (deviceId !== null) {
		const present = list.some((device) => device.deviceId === deviceId);
		if (!present) {
			return {
				inventory,
				error: {
					code: 'deviceUnavailable',
					message: `Voice engine v2 device adapter cannot select ${kind} device ${deviceId}: not present in inventory`,
					capability: 'devices',
				},
			};
		}
	}
	return {
		inventory: {
			audioInputs: inventory.audioInputs,
			audioOutputs: inventory.audioOutputs,
			cameras: inventory.cameras,
			selectedAudioInputId: kind === 'audioInput' ? deviceId : inventory.selectedAudioInputId,
			selectedAudioOutputId: kind === 'audioOutput' ? deviceId : inventory.selectedAudioOutputId,
			selectedCameraId: kind === 'camera' ? deviceId : inventory.selectedCameraId,
		},
		error: null,
	};
}

function pickList(
	inventory: VoiceEngineV2DeviceInventory,
	kind: VoiceEngineV2AppDevicesKind,
): ReadonlyArray<{deviceId: string}> {
	assert.ok(inventory !== null && typeof inventory === 'object', 'inventory must be an object');
	switch (kind) {
		case 'audioInput':
			return inventory.audioInputs;
		case 'audioOutput':
			return inventory.audioOutputs;
		case 'camera':
			return inventory.cameras;
		default: {
			const exhaustive: never = kind;
			throw new Error(`Voice engine v2 device adapter received an unhandled kind: ${String(exhaustive)}`);
		}
	}
}

function assertInventoryShape(value: VoiceEngineV2DeviceInventory): void {
	assert.ok(value !== null && typeof value === 'object', 'inventory must be an object');
	assert.ok(Array.isArray(value.audioInputs), 'inventory.audioInputs must be an array');
	assert.ok(Array.isArray(value.audioOutputs), 'inventory.audioOutputs must be an array');
	assert.ok(Array.isArray(value.cameras), 'inventory.cameras must be an array');
	assert.ok(
		value.selectedAudioInputId === null || typeof value.selectedAudioInputId === 'string',
		'inventory.selectedAudioInputId must be a string or null',
	);
	assert.ok(
		value.selectedAudioOutputId === null || typeof value.selectedAudioOutputId === 'string',
		'inventory.selectedAudioOutputId must be a string or null',
	);
	assert.ok(
		value.selectedCameraId === null || typeof value.selectedCameraId === 'string',
		'inventory.selectedCameraId must be a string or null',
	);
}
