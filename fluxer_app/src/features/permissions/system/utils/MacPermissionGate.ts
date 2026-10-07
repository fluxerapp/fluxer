// SPDX-License-Identifier: AGPL-3.0-or-later

import {openMacPermissionsModal} from '@app/features/permissions/system/commands/MacPermissionsModalCommands';
import MacPermissions, {type MacPermissionKind} from '@app/features/permissions/system/state/MacPermissions';
import {requestNativePermission} from '@app/features/permissions/system/utils/NativePermissions';
import {getNativePlatformSync, isDesktop} from '@app/features/ui/utils/NativeUtils';

export type MacPermissionGateBehavior = 'interactive' | 'passive';
export type MacPermissionGateResult = 'granted' | 'denied' | 'unsupported-platform';

interface MacPermissionGateOptions {
	behavior: MacPermissionGateBehavior;
}

export async function ensureMacPermission(
	kind: MacPermissionKind,
	options: MacPermissionGateOptions,
): Promise<MacPermissionGateResult> {
	if (!isDesktop() || getNativePlatformSync() !== 'macos') {
		return 'unsupported-platform';
	}
	const status = await MacPermissions.refreshKind(kind);
	if (status === 'unsupported') {
		return 'unsupported-platform';
	}
	if (status === 'granted') {
		return 'granted';
	}
	if (options.behavior === 'passive') {
		return 'denied';
	}
	if (status === 'not-determined') {
		const requested = await requestNativePermission(kind);
		MacPermissions.applyPermissionResult(kind, requested);
		return requested === 'granted' ? 'granted' : 'denied';
	}
	openMacPermissionsModal(kind);
	return 'denied';
}
