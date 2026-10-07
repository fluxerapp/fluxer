// SPDX-License-Identifier: AGPL-3.0-or-later

import {getElectronAPI, isNativeMacOS} from '@app/features/ui/utils/NativeUtils';

export type PermissionKind = 'microphone' | 'camera' | 'screen' | 'input-monitoring';
export type NativePermissionResult = 'granted' | 'denied' | 'not-determined' | 'unsupported';

export const MAC_PERMISSION_KINDS: ReadonlyArray<PermissionKind> = Object.freeze([
	'microphone',
	'camera',
	'screen',
	'input-monitoring',
]);

export function settledPermissionKinds(
	statuses: Readonly<Record<PermissionKind, NativePermissionResult>>,
): Array<PermissionKind> {
	return MAC_PERMISSION_KINDS.filter((kind) => statuses[kind] === 'granted' || statuses[kind] === 'unsupported');
}

const normalizeNativePermissionResult = (value: unknown): NativePermissionResult => {
	switch (value) {
		case 'granted':
		case 'denied':
		case 'not-determined':
		case 'unsupported':
			return value;
		default:
			return 'not-determined';
	}
};

export async function checkNativePermission(kind: PermissionKind): Promise<NativePermissionResult> {
	const electronApi = getElectronAPI();
	if (!electronApi) {
		return 'unsupported';
	}
	if (!isNativeMacOS()) {
		return 'granted';
	}
	if (kind === 'input-monitoring') {
		if (typeof electronApi.getInputMonitoringPermissionStatus === 'function') {
			return normalizeNativePermissionResult(await electronApi.getInputMonitoringPermissionStatus());
		}
		const hasAccess = await electronApi.checkInputMonitoringAccess();
		return hasAccess ? 'granted' : 'denied';
	}
	if (kind === 'screen' && typeof electronApi.getScreenRecordingPermissionStatus === 'function') {
		return normalizeNativePermissionResult(await electronApi.getScreenRecordingPermissionStatus());
	}
	const status = await electronApi.checkMediaAccess(kind);
	switch (status) {
		case 'granted':
			return 'granted';
		case 'denied':
		case 'restricted':
			return 'denied';
		case 'not-determined':
			return 'not-determined';
		default:
			return 'not-determined';
	}
}

export async function requestNativePermission(kind: PermissionKind): Promise<NativePermissionResult> {
	const electronApi = getElectronAPI();
	if (!electronApi) return 'unsupported';
	if (!isNativeMacOS()) {
		return 'granted';
	}
	if (kind === 'input-monitoring') {
		const current = await checkNativePermission(kind);
		if (current !== 'not-determined') {
			return current;
		}
		if (typeof electronApi.requestInputMonitoringPermission !== 'function') {
			return current;
		}
		return normalizeNativePermissionResult(await electronApi.requestInputMonitoringPermission());
	}
	if (kind === 'screen' && typeof electronApi.requestScreenRecordingPermission === 'function') {
		return normalizeNativePermissionResult(await electronApi.requestScreenRecordingPermission());
	}
	const granted = await electronApi.requestMediaAccess(kind);
	return granted ? 'granted' : 'denied';
}

export async function ensureNativePermission(kind: PermissionKind): Promise<NativePermissionResult> {
	const current = await checkNativePermission(kind);
	if (current === 'granted' || current === 'unsupported') {
		return current;
	}
	if (kind === 'input-monitoring') {
		return current;
	}
	if (current === 'not-determined') {
		return requestNativePermission(kind);
	}
	return 'denied';
}

export async function openNativePermissionSettings(kind: PermissionKind): Promise<void> {
	const electronApi = getElectronAPI();
	if (!electronApi) return;
	if (!isNativeMacOS()) {
		return;
	}
	switch (kind) {
		case 'input-monitoring':
			await electronApi.openInputMonitoringSettings();
			break;
		case 'microphone':
		case 'camera':
		case 'screen':
			await electronApi.openMediaAccessSettings(kind);
			break;
	}
}
