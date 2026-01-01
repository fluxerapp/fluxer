/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {getElectronAPI, isNativeMacOS} from '~/utils/NativeUtils';

type PermissionKind = 'microphone' | 'camera' | 'screen' | 'accessibility' | 'input-monitoring';

export type NativePermissionResult = 'granted' | 'denied' | 'not-determined' | 'unsupported';

const permissionCache = new Map<
	PermissionKind,
	{
		value: NativePermissionResult;
		timestamp: number;
	}
>();

const CACHE_DURATION = 1000;

export const getCachedPermission = (kind: PermissionKind): NativePermissionResult | null => {
	const cached = permissionCache.get(kind);
	if (!cached) return null;

	const age = Date.now() - cached.timestamp;
	if (age > CACHE_DURATION) {
		permissionCache.delete(kind);
		return null;
	}

	return cached.value;
};

const setCachedPermission = (kind: PermissionKind, value: NativePermissionResult): void => {
	permissionCache.set(kind, {value, timestamp: Date.now()});
};

export const checkNativePermission = async (kind: PermissionKind): Promise<NativePermissionResult> => {
	const electronApi = getElectronAPI();
	if (!electronApi) {
		const result = 'unsupported';
		setCachedPermission(kind, result);
		return result;
	}

	if (!isNativeMacOS()) {
		const result = 'granted';
		setCachedPermission(kind, result);
		return result;
	}

	let result: NativePermissionResult;

	if (kind === 'input-monitoring') {
		const hasAccess = await electronApi.checkInputMonitoringAccess();
		result = hasAccess ? 'granted' : 'denied';
		setCachedPermission(kind, result);
		return result;
	}

	if (kind === 'accessibility') {
		const isTrusted = await electronApi.checkAccessibility(false);
		result = isTrusted ? 'granted' : 'denied';
		setCachedPermission(kind, result);
		return result;
	}

	const status = await electronApi.checkMediaAccess(kind);
	switch (status) {
		case 'granted':
			result = 'granted';
			break;
		case 'denied':
		case 'restricted':
			result = 'denied';
			break;
		case 'not-determined':
			result = 'not-determined';
			break;
		default:
			result = 'not-determined';
			break;
	}

	setCachedPermission(kind, result);
	return result;
};

export const requestNativePermission = async (kind: PermissionKind): Promise<NativePermissionResult> => {
	const electronApi = getElectronAPI();
	if (!electronApi) return 'unsupported';

	if (!isNativeMacOS()) {
		return 'granted';
	}

	if (kind === 'input-monitoring') {
		const hasAccess = await electronApi.checkInputMonitoringAccess();
		return hasAccess ? 'granted' : 'denied';
	}

	if (kind === 'accessibility') {
		const isTrusted = await electronApi.checkAccessibility(true);
		return isTrusted ? 'granted' : 'denied';
	}

	const granted = await electronApi.requestMediaAccess(kind);
	return granted ? 'granted' : 'denied';
};

export const ensureNativePermission = async (kind: PermissionKind): Promise<NativePermissionResult> => {
	const current = await checkNativePermission(kind);

	if (current === 'granted' || current === 'unsupported') {
		return current;
	}

	if (current === 'not-determined') {
		return requestNativePermission(kind);
	}

	return 'denied';
};

export const openNativePermissionSettings = async (kind: PermissionKind): Promise<void> => {
	const electronApi = getElectronAPI();
	if (!electronApi) return;

	if (!isNativeMacOS()) {
		return;
	}

	switch (kind) {
		case 'accessibility':
			await electronApi.openAccessibilitySettings();
			break;
		case 'input-monitoring':
			await electronApi.openInputMonitoringSettings();
			break;
		case 'microphone':
		case 'camera':
		case 'screen':
			await electronApi.openMediaAccessSettings(kind);
			break;
	}
};
