// SPDX-License-Identifier: AGPL-3.0-or-later

import GlobalShortcuts from '@app/features/input/state/GlobalShortcuts';
import MacPermissions, {type MacPermissionKind} from '@app/features/permissions/system/state/MacPermissions';
import type {NativePermissionResult} from '@app/features/permissions/system/utils/NativePermissions';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useCallback, useEffect, useState} from 'react';

const MICROPHONE_NAME_DESCRIPTOR = msg({
	message: 'Microphone',
	comment:
		'Name of the macOS Microphone permission. Use the name macOS shows under System Settings, Privacy & Security in this language.',
});
const CAMERA_NAME_DESCRIPTOR = msg({
	message: 'Camera',
	comment:
		'Name of the macOS Camera permission. Use the name macOS shows under System Settings, Privacy & Security in this language.',
});
const SCREEN_RECORDING_NAME_DESCRIPTOR = msg({
	message: 'Screen Recording',
	comment:
		'Name of the macOS screen recording permission. Use the name macOS shows under System Settings, Privacy & Security in this language.',
});
const INPUT_MONITORING_NAME_DESCRIPTOR = msg({
	message: 'Input Monitoring',
	comment:
		'Name of the macOS Input Monitoring permission. Use the name macOS shows under System Settings, Privacy & Security in this language.',
});
const ALLOW_DESCRIPTOR = msg({
	message: 'Allow',
	comment: 'Button label that makes macOS ask the user for a permission.',
});
const OPEN_SYSTEM_SETTINGS_DESCRIPTOR = msg({
	message: 'Open System Settings',
	comment:
		'Button label that opens the macOS System Settings app at the pane for one permission. Use the name macOS gives System Settings in this language.',
});
export const QUIT_PROMPT_ADVICE_DESCRIPTOR = msg({
	message: 'If macOS asks to quit and reopen {productName}, choose Later.',
	comment:
		'Advice shown after the user asks for the macOS Screen Recording or Input Monitoring permission. macOS then shows its own dialog with the buttons Quit & Reopen and Later. {productName} picks up the change without restarting, so the user should choose Later. Use the button name macOS shows in this language. {productName} is the app name.',
});
export const PERMISSION_ALLOWED_DESCRIPTOR = msg({
	message: 'Allowed',
	comment: 'Short status shown once a macOS permission has been granted.',
});

const STATUS_POLL_INTERVAL_MS = 1000;

export function macPermissionNameDescriptor(kind: MacPermissionKind): MessageDescriptor {
	switch (kind) {
		case 'microphone':
			return MICROPHONE_NAME_DESCRIPTOR;
		case 'camera':
			return CAMERA_NAME_DESCRIPTOR;
		case 'screen':
			return SCREEN_RECORDING_NAME_DESCRIPTOR;
		case 'input-monitoring':
			return INPUT_MONITORING_NAME_DESCRIPTOR;
	}
}

interface MacPermissionControl {
	readonly status: NativePermissionResult;
	readonly actionDescriptor: MessageDescriptor | null;
	readonly busy: boolean;
	readonly relaunchMayHelp: boolean;
	readonly quitPromptAdvice: boolean;
	readonly runAction: () => void;
}

function relaunchMayHelp(kind: MacPermissionKind, status: NativePermissionResult): boolean {
	if (kind === 'screen') return MacPermissions.screenStillBlockedAfterReturn;
	return kind === 'input-monitoring' && status === 'granted' && GlobalShortcuts.hookError === 'start-failed';
}

export function useMacPermissionControl(kind: MacPermissionKind): MacPermissionControl {
	const [busy, setBusy] = useState(false);
	const status = MacPermissions.statuses[kind];
	const needsAction = status === 'not-determined' || status === 'denied';
	useEffect(() => {
		void MacPermissions.refreshKind(kind);
	}, [kind]);
	useEffect(() => {
		if (!needsAction) return;
		const timer = window.setInterval(() => {
			if (document.visibilityState === 'visible') void MacPermissions.refreshKind(kind);
		}, STATUS_POLL_INTERVAL_MS);
		return () => window.clearInterval(timer);
	}, [kind, needsAction]);
	const runAction = useCallback(() => {
		const action = status === 'not-determined' ? MacPermissions.request : MacPermissions.openSettings;
		setBusy(true);
		void action(kind).finally(() => setBusy(false));
	}, [kind, status]);
	return {
		status,
		actionDescriptor: needsAction
			? status === 'not-determined'
				? ALLOW_DESCRIPTOR
				: OPEN_SYSTEM_SETTINGS_DESCRIPTOR
			: null,
		busy,
		relaunchMayHelp: relaunchMayHelp(kind, status),
		quitPromptAdvice: MacPermissions.showsQuitPromptAdvice(kind),
		runAction,
	};
}
