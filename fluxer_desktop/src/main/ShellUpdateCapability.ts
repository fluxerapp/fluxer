// SPDX-License-Identifier: AGPL-3.0-or-later

import {BUILD_CHANNEL, type BuildChannel} from '@electron/common/BuildChannel';
import {isPortableMode} from '@electron/common/UserDataPath';
import {type AppImageTarget, isRunningFromAppImage, resolveAppImageTarget} from '@electron/main/AppImageUpdate';
import {isFlatpakRuntime} from '@electron/main/LinuxSandbox';
import {app} from 'electron';

export const ShellUpdateCapability = Object.freeze({
	SELF_UPDATE: 'self-update',
	MANUAL_DOWNLOAD: 'manual-download',
	MANAGED_PACKAGE: 'managed-package',
} as const);

export type ShellUpdateCapability = (typeof ShellUpdateCapability)[keyof typeof ShellUpdateCapability];

export type ShellUpdatePlan =
	| {readonly capability: typeof ShellUpdateCapability.SELF_UPDATE; readonly updater: 'velopack' | 'electron'}
	| {
			readonly capability: typeof ShellUpdateCapability.SELF_UPDATE;
			readonly updater: 'appimage';
			readonly target: AppImageTarget;
	  }
	| {readonly capability: typeof ShellUpdateCapability.MANUAL_DOWNLOAD; readonly reason: 'platform' | 'unpackaged'}
	| {readonly capability: typeof ShellUpdateCapability.MANAGED_PACKAGE};

interface ShellUpdateRuntime {
	readonly channel: BuildChannel;
	readonly packaged: boolean;
	readonly portable: boolean;
	readonly flatpak: boolean;
	readonly appImage: AppImageTarget | null;
	readonly platform: NodeJS.Platform;
}

export function decideShellUpdatePlan(runtime: ShellUpdateRuntime): ShellUpdatePlan {
	if (!runtime.packaged || runtime.channel === 'development') {
		return {capability: ShellUpdateCapability.MANUAL_DOWNLOAD, reason: 'unpackaged'};
	}
	if (runtime.flatpak) {
		return {capability: ShellUpdateCapability.MANAGED_PACKAGE};
	}
	if (runtime.portable) {
		return {capability: ShellUpdateCapability.MANUAL_DOWNLOAD, reason: 'platform'};
	}
	if (runtime.platform === 'win32') {
		return {capability: ShellUpdateCapability.SELF_UPDATE, updater: 'velopack'};
	}
	if (runtime.platform === 'darwin') {
		return {capability: ShellUpdateCapability.SELF_UPDATE, updater: 'electron'};
	}
	if (runtime.platform === 'linux' && runtime.appImage != null) {
		return {capability: ShellUpdateCapability.SELF_UPDATE, updater: 'appimage', target: runtime.appImage};
	}
	return {capability: ShellUpdateCapability.MANUAL_DOWNLOAD, reason: 'platform'};
}

function resolveRunningAppImage(): AppImageTarget | null {
	if (process.platform !== 'linux' || !isRunningFromAppImage()) {
		return null;
	}
	const resolved = resolveAppImageTarget();
	return resolved.ok ? resolved.target : null;
}

export function resolveShellUpdatePlan(): ShellUpdatePlan {
	return decideShellUpdatePlan({
		channel: BUILD_CHANNEL,
		packaged: app.isPackaged,
		portable: isPortableMode(),
		flatpak: isFlatpakRuntime(),
		appImage: resolveRunningAppImage(),
		platform: process.platform,
	});
}
