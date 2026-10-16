// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type DomainMigrationDeviceMap,
	type DomainMigrationMediaDevice,
	type DomainMigrationMediaDeviceKind,
	readDomainMigrationDeviceMap,
	writeDomainMigrationDeviceMap,
} from '@app/features/app/domain_migration/DomainMigrationCore';
import {getProtectedLocalStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {awaitHydration} from '@app/features/platform/utils/MobXPersistence';
import VoiceDevicePermissionState from '@app/features/voice/engine/VoiceDevicePermissionState';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {
	getVoiceAudioDeviceMetadata,
	normalizeDeviceMatchLabel,
	type VoiceDeviceState,
} from '@app/features/voice/utils/VoiceDeviceManager';

const remappedDeviceIds = new Map<DomainMigrationMediaDeviceKind, Map<string, string>>();
let installed = false;
let pendingDeviceMap: DomainMigrationDeviceMap | null = null;

function devicesOfKind(state: VoiceDeviceState, kind: DomainMigrationMediaDeviceKind): Array<MediaDeviceInfo> {
	const devices =
		kind === 'audioinput' ? state.inputDevices : kind === 'audiooutput' ? state.outputDevices : state.videoDevices;
	return devices.filter(
		(device) =>
			device.deviceId.trim().length > 0 &&
			device.deviceId !== 'default' &&
			device.deviceId !== 'communications' &&
			getVoiceAudioDeviceMetadata(device) === null,
	);
}

function hasLabelledDevices(state: VoiceDeviceState, kind: DomainMigrationMediaDeviceKind): boolean {
	const devices = devicesOfKind(state, kind);
	const permission = kind === 'videoinput' ? state.permissionStatus.video : state.permissionStatus.audio;
	return devices.some((device) => device.label.trim().length > 0) || (permission === 'granted' && devices.length > 0);
}

function groupByLabel<T>(
	entries: ReadonlyArray<T>,
	labelOf: (entry: T) => string,
	idOf: (entry: T) => string,
): Map<string, Array<string>> {
	const groups = new Map<string, Array<string>>();
	for (const entry of entries) {
		const label = labelOf(entry);
		if (label.length === 0) {
			continue;
		}
		groups.set(label, [...(groups.get(label) ?? []), idOf(entry)]);
	}
	return groups;
}

function mapDevicesOfKind(
	kind: DomainMigrationMediaDeviceKind,
	sourceDevices: ReadonlyArray<DomainMigrationMediaDevice>,
	targetDevices: ReadonlyArray<MediaDeviceInfo>,
): Map<string, string> {
	const sourceByLabel = groupByLabel(
		sourceDevices.filter((device) => device.kind === kind),
		(device) => normalizeDeviceMatchLabel(kind, device.device_id, device.label),
		(device) => device.device_id,
	);
	const targetByLabel = groupByLabel(
		targetDevices,
		(device) => normalizeDeviceMatchLabel(kind, device.deviceId, device.label),
		(device) => device.deviceId,
	);
	const mapping = new Map<string, string>();
	for (const [label, sourceIds] of sourceByLabel) {
		const targetIds = targetByLabel.get(label);
		if (sourceIds.length === 1 && targetIds?.length === 1 && sourceIds[0] !== targetIds[0]) {
			mapping.set(sourceIds[0], targetIds[0]);
		}
	}
	return mapping;
}

export function remapMigratedDeviceId(kind: DomainMigrationMediaDeviceKind, deviceId: string): string {
	return remappedDeviceIds.get(kind)?.get(deviceId) ?? deviceId;
}

export function remapMigratedDeviceIds(
	deviceIds: Array<string>,
	kinds: ReadonlyArray<DomainMigrationMediaDeviceKind>,
): Array<string> {
	let changed = false;
	const remapped = deviceIds.flatMap((deviceId) => {
		const targets = [...new Set(kinds.map((kind) => remapMigratedDeviceId(kind, deviceId)))].filter(
			(target) => target !== deviceId,
		);
		if (targets.length === 0) {
			return [deviceId];
		}
		changed = true;
		return targets;
	});
	return changed ? [...new Set(remapped)] : deviceIds;
}

export function isPendingMigratedDeviceId(deviceId: string): boolean {
	const map = pendingDeviceMap;
	return map?.devices.some((device) => device.device_id === deviceId && !map.resolved.includes(device.kind)) ?? false;
}

function applyToVoiceSettings(): void {
	const current = {
		inputDeviceId: VoiceSettings.getInputDeviceId(),
		outputDeviceId: VoiceSettings.getOutputDeviceId(),
		videoDeviceId: VoiceSettings.getVideoDeviceId(),
		screenShareAudioDeviceId: VoiceSettings.getScreenShareAudioDeviceId(),
	};
	const next = {
		inputDeviceId: remapMigratedDeviceId('audioinput', current.inputDeviceId),
		outputDeviceId: remapMigratedDeviceId('audiooutput', current.outputDeviceId),
		videoDeviceId: remapMigratedDeviceId('videoinput', current.videoDeviceId),
		screenShareAudioDeviceId: remapMigratedDeviceId('audioinput', current.screenShareAudioDeviceId),
	};
	if (
		next.inputDeviceId !== current.inputDeviceId ||
		next.outputDeviceId !== current.outputDeviceId ||
		next.videoDeviceId !== current.videoDeviceId ||
		next.screenShareAudioDeviceId !== current.screenShareAudioDeviceId
	) {
		VoiceSettings.updateSettings(next);
	}
}

function reconcileMigratedDevices(state: VoiceDeviceState): void {
	const map = pendingDeviceMap;
	if (map === null) {
		return;
	}
	const pendingKinds = [...new Set(map.devices.map((device) => device.kind))].filter(
		(kind) => !map.resolved.includes(kind),
	);
	const resolvableKinds = pendingKinds.filter((kind) => hasLabelledDevices(state, kind));
	if (pendingKinds.length > 0 && resolvableKinds.length === 0) {
		return;
	}
	for (const kind of resolvableKinds) {
		const mapping = mapDevicesOfKind(kind, map.devices, devicesOfKind(state, kind));
		remappedDeviceIds.set(kind, new Map([...(remappedDeviceIds.get(kind) ?? []), ...mapping]));
	}
	applyToVoiceSettings();
	const resolved = [...map.resolved, ...resolvableKinds];
	const allResolved = pendingKinds.every((kind) => resolved.includes(kind));
	pendingDeviceMap = allResolved ? null : {...map, resolved};
	const storage = getProtectedLocalStorage();
	const stored = readDomainMigrationDeviceMap(storage, Date.now());
	if (stored === null) {
		return;
	}
	const storedResolved = [...new Set([...stored.resolved, ...resolved])];
	const storedKinds = new Set(stored.devices.map((device) => device.kind));
	writeDomainMigrationDeviceMap(
		storage,
		[...storedKinds].every((kind) => storedResolved.includes(kind)) ? null : {...stored, resolved: storedResolved},
	);
}

export async function installMigratedDeviceRemap(): Promise<void> {
	if (installed) {
		return;
	}
	const map = readDomainMigrationDeviceMap(getProtectedLocalStorage(), Date.now());
	if (map === null) {
		return;
	}
	installed = true;
	pendingDeviceMap = {...map, resolved: []};
	await awaitHydration('VoiceSettings');
	VoiceDevicePermissionState.addDeviceStateReconciler(reconcileMigratedDevices);
}
