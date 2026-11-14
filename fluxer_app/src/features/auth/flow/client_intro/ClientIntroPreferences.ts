// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type DesktopClientIntroConditions,
	type DesktopClientIntroMachineEvent,
	type DesktopClientIntroSnapshot,
	transitionDesktopClientIntroSnapshot,
} from '@app/features/auth/flow/client_intro/DesktopClientIntroStateMachine';
import {MAC_PERMISSION_KINDS, type PermissionKind} from '@app/features/permissions/system/utils/NativePermissions';
import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import AppStorage from '@app/features/platform/state/PersistentStorage';
import {getNativePlatformSync, isDesktop} from '@app/features/ui/utils/NativeUtils';

function hasCompletedDesktopClientIntro(): boolean {
	return AppStorage.getItem(AppStorageKey.AUTH_DESKTOP_CLIENT_INTRO_COMPLETED) === '1';
}

function hasSeenDesktopClientPreferences(): boolean {
	return (
		AppStorage.getItem(AppStorageKey.AUTH_DESKTOP_CLIENT_PREFERENCES_SEEN) === '1' || hasCompletedDesktopClientIntro()
	);
}

function hasSeenDesktopClientWelcome(): boolean {
	return (
		AppStorage.getItem(AppStorageKey.AUTH_DESKTOP_CLIENT_WELCOME_SEEN) === '1' || hasSeenDesktopClientPreferences()
	);
}

function readSkippedPermissions(): Array<PermissionKind> {
	const stored = (AppStorage.getItem(AppStorageKey.AUTH_DESKTOP_CLIENT_PERMISSIONS_SKIPPED) ?? '').split(',');
	return MAC_PERMISSION_KINDS.filter((kind) => stored.includes(kind));
}

export function readDesktopClientIntroConditions(enabled: boolean): DesktopClientIntroConditions {
	return {
		enabled,
		welcomeSeen: hasSeenDesktopClientWelcome(),
		preferencesSeen: hasSeenDesktopClientPreferences(),
		completed: hasCompletedDesktopClientIntro(),
		permissionKinds: isDesktop() && getNativePlatformSync() === 'macos' ? MAC_PERMISSION_KINDS : [],
		skippedPermissions: readSkippedPermissions(),
	};
}

function persistDesktopClientIntroConditions(conditions: DesktopClientIntroConditions): void {
	if (conditions.welcomeSeen) AppStorage.setItem(AppStorageKey.AUTH_DESKTOP_CLIENT_WELCOME_SEEN, '1');
	if (conditions.preferencesSeen) AppStorage.setItem(AppStorageKey.AUTH_DESKTOP_CLIENT_PREFERENCES_SEEN, '1');
	if (conditions.skippedPermissions.length > 0) {
		AppStorage.setItem(AppStorageKey.AUTH_DESKTOP_CLIENT_PERMISSIONS_SKIPPED, conditions.skippedPermissions.join(','));
	}
	if (conditions.completed) AppStorage.setItem(AppStorageKey.AUTH_DESKTOP_CLIENT_INTRO_COMPLETED, '1');
}

export function advanceDesktopClientIntro(
	snapshot: DesktopClientIntroSnapshot,
	event: DesktopClientIntroMachineEvent,
): DesktopClientIntroSnapshot {
	const next = transitionDesktopClientIntroSnapshot(snapshot, event);
	persistDesktopClientIntroConditions(next.context);
	return next;
}
