// SPDX-License-Identifier: AGPL-3.0-or-later

import MacPermissions from '@app/features/permissions/system/state/MacPermissions';
import MediaPermission from '@app/features/permissions/system/state/MediaPermission';
import {
	checkNativePermission,
	type NativePermissionResult,
} from '@app/features/permissions/system/utils/NativePermissions';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {
	createScreenSharePickerDisplayPermissionSnapshot,
	type ScreenSharePickerDisplayPermissionEvent,
	type ScreenSharePickerDisplayPermissionPrompt,
	screenRecordingPermissionAllowsPickerSources,
	selectScreenSharePickerDisplayPermissionPrompt,
	transitionScreenSharePickerDisplayPermissionSnapshot,
} from '@app/features/voice/components/modals/screen_share_picker_modal/ScreenSharePickerDisplayPermissionStateMachine';
import {
	logger,
	type ScreenSharePickerTab,
} from '@app/features/voice/components/modals/screen_share_picker_modal/shared';
import type {DisplayShareEnvironment} from '@app/features/voice/utils/ScreenShareEnvironment';
import {useCallback, useEffect, useState} from 'react';

export function shouldCheckDesktopSourceScreenRecordingPermission(
	displayShareEnvironment: DisplayShareEnvironment,
): boolean {
	return displayShareEnvironment === 'desktop-custom' && getElectronAPI()?.platform === 'darwin';
}

function shouldCheckScreenRecordingPermissionForPicker(
	activeTab: ScreenSharePickerTab,
	displayShareEnvironment: DisplayShareEnvironment,
): boolean {
	return activeTab !== 'devices' && shouldCheckDesktopSourceScreenRecordingPermission(displayShareEnvironment);
}

function updateScreenRecordingPermissionStore(permission: NativePermissionResult): void {
	if (screenRecordingPermissionAllowsPickerSources(permission)) {
		MediaPermission.updateScreenRecordingPermissionGranted();
	} else if (permission === 'denied') {
		MediaPermission.markScreenRecordingExplicitlyDenied();
	}
}

export async function readScreenSharePickerScreenRecordingPermission(reason: string): Promise<NativePermissionResult> {
	try {
		const permission = await checkNativePermission('screen');
		updateScreenRecordingPermissionStore(permission);
		return permission;
	} catch (error) {
		logger.warn('Failed to check screen recording permission for picker', {error, reason});
		return 'not-determined';
	}
}

interface UseScreenSharePickerDisplayPermissionOptions {
	activeTab: ScreenSharePickerTab;
	displayShareEnvironment: DisplayShareEnvironment;
}

interface UseScreenSharePickerDisplayPermissionResult {
	blocksDesktopSources: boolean;
	openSettings: () => void;
	prompt: ScreenSharePickerDisplayPermissionPrompt;
}

export function useScreenSharePickerDisplayPermission({
	activeTab,
	displayShareEnvironment,
}: UseScreenSharePickerDisplayPermissionOptions): UseScreenSharePickerDisplayPermissionResult {
	const [snapshot, setSnapshot] = useState(() => createScreenSharePickerDisplayPermissionSnapshot());
	const transitionDisplayPermission = useCallback((event: ScreenSharePickerDisplayPermissionEvent) => {
		setSnapshot((currentSnapshot) => transitionScreenSharePickerDisplayPermissionSnapshot(currentSnapshot, event));
	}, []);
	const shouldCheck = shouldCheckScreenRecordingPermissionForPicker(activeTab, displayShareEnvironment);
	useEffect(() => {
		if (!shouldCheck) {
			transitionDisplayPermission({type: 'permission.clear'});
			return;
		}
		let cancelled = false;
		transitionDisplayPermission({type: 'permission.check'});
		void readScreenSharePickerScreenRecordingPermission('active-tab').then((permission) => {
			if (!cancelled) {
				transitionDisplayPermission({type: 'permission.result', permission});
			}
		});
		return () => {
			cancelled = true;
		};
	}, [shouldCheck, transitionDisplayPermission]);
	const selectedPrompt = selectScreenSharePickerDisplayPermissionPrompt(snapshot);
	const prompt = shouldCheck && snapshot.matches('idle') ? 'checking' : selectedPrompt;
	const awaitingGrant = shouldCheck && (selectedPrompt === 'needs-permission' || selectedPrompt === 'restart-required');
	useEffect(() => {
		if (!awaitingGrant) return;
		const recheck = () => {
			void readScreenSharePickerScreenRecordingPermission('window-focus').then((permission) => {
				transitionDisplayPermission({type: 'permission.result', permission});
			});
		};
		window.addEventListener('focus', recheck);
		return () => window.removeEventListener('focus', recheck);
	}, [awaitingGrant, transitionDisplayPermission]);
	const openSettings = useCallback(() => {
		transitionDisplayPermission({type: 'permission.settingsOpened'});
		void MacPermissions.refreshKind('screen').then((status) =>
			status === 'not-determined' ? MacPermissions.request('screen') : MacPermissions.openSettings('screen'),
		);
	}, [transitionDisplayPermission]);
	return {
		blocksDesktopSources: shouldCheck && prompt !== 'none',
		openSettings,
		prompt,
	};
}
