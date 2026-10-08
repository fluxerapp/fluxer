// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PermissionKind} from '@app/features/permissions/system/utils/NativePermissions';

export const DesktopHandoffMode = Object.freeze({
	IDLE: 'idle',
	SELECTING: 'selecting',
	LOGIN: 'login',
	CODE_INPUT: 'code_input',
	FETCHING_INFO: 'fetching_info',
	APPROVING: 'approving',
	COMPLETING: 'completing',
	DONE: 'done',
	DENIED: 'denied',
	ERROR: 'error',
} as const);

export type DesktopHandoffMode = (typeof DesktopHandoffMode)[keyof typeof DesktopHandoffMode];

export function isApprovalFlowMode(mode: DesktopHandoffMode): boolean {
	switch (mode) {
		case DesktopHandoffMode.CODE_INPUT:
		case DesktopHandoffMode.FETCHING_INFO:
		case DesktopHandoffMode.APPROVING:
		case DesktopHandoffMode.COMPLETING:
		case DesktopHandoffMode.DONE:
		case DesktopHandoffMode.DENIED:
		case DesktopHandoffMode.ERROR:
			return true;
		case DesktopHandoffMode.IDLE:
		case DesktopHandoffMode.SELECTING:
		case DesktopHandoffMode.LOGIN:
			return false;
	}
}

export const AuthLoginStep = Object.freeze({
	ACCOUNT: 'account',
	DESKTOP_HANDOFF_ACCOUNT: 'desktop_handoff_account',
	CLIENT_PREFERENCES: 'client_preferences',
	CLIENT_PERMISSION_MICROPHONE: 'client_permission_microphone',
	CLIENT_PERMISSION_CAMERA: 'client_permission_camera',
	CLIENT_PERMISSION_SCREEN: 'client_permission_screen',
	CLIENT_PERMISSION_INPUT_MONITORING: 'client_permission_input_monitoring',
	INSTANCE: 'instance',
	SSO: 'sso',
	METHOD: 'method',
	BROWSER: 'browser',
	CREDENTIALS: 'credentials',
	IP_AUTHORIZATION: 'ip_authorization',
	DESKTOP_HANDOFF_APPROVAL: 'desktop_handoff_approval',
} as const);

export type AuthLoginStep = (typeof AuthLoginStep)[keyof typeof AuthLoginStep];

const CLIENT_PERMISSION_STEPS: Readonly<Record<PermissionKind, AuthLoginStep>> = Object.freeze({
	microphone: AuthLoginStep.CLIENT_PERMISSION_MICROPHONE,
	camera: AuthLoginStep.CLIENT_PERMISSION_CAMERA,
	screen: AuthLoginStep.CLIENT_PERMISSION_SCREEN,
	'input-monitoring': AuthLoginStep.CLIENT_PERMISSION_INPUT_MONITORING,
});

export const AUTH_LOGIN_STEP_ORDER: ReadonlyArray<AuthLoginStep> = Object.freeze(Object.values(AuthLoginStep));

export const AUTH_LOGIN_METHOD_QUERY_PARAM = Object.freeze({
	name: 'auth_step',
	value: AuthLoginStep.METHOD,
} as const);

export interface AuthLoginStepSelection {
	desktopHandoff: boolean;
	handoffMode: DesktopHandoffMode;
	hasIpAuthorizationChallenge: boolean;
	hasStoredAccounts: boolean;
	isSsoEnforced: boolean;
	shouldShowBrowserStep: boolean;
	clientPermission: PermissionKind | null;
	shouldShowClientPreferencesStep: boolean;
	shouldShowInstanceStep: boolean;
	shouldShowMethodStep: boolean;
	showAccountSelector: boolean;
}

export function selectAuthLoginStep({
	desktopHandoff,
	handoffMode,
	hasIpAuthorizationChallenge,
	hasStoredAccounts,
	isSsoEnforced,
	shouldShowBrowserStep,
	clientPermission,
	shouldShowClientPreferencesStep,
	shouldShowInstanceStep,
	shouldShowMethodStep,
	showAccountSelector,
}: AuthLoginStepSelection): AuthLoginStep {
	if (desktopHandoff && handoffMode === DesktopHandoffMode.SELECTING) return AuthLoginStep.DESKTOP_HANDOFF_ACCOUNT;
	if (desktopHandoff && isApprovalFlowMode(handoffMode)) return AuthLoginStep.DESKTOP_HANDOFF_APPROVAL;
	if (hasIpAuthorizationChallenge) return AuthLoginStep.IP_AUTHORIZATION;
	if (showAccountSelector && hasStoredAccounts && !desktopHandoff) return AuthLoginStep.ACCOUNT;
	if (shouldShowClientPreferencesStep) return AuthLoginStep.CLIENT_PREFERENCES;
	if (clientPermission != null) return CLIENT_PERMISSION_STEPS[clientPermission];
	if (shouldShowInstanceStep) return AuthLoginStep.INSTANCE;
	if (isSsoEnforced && !shouldShowBrowserStep) return AuthLoginStep.SSO;
	if (shouldShowMethodStep) return AuthLoginStep.METHOD;
	if (shouldShowBrowserStep) return AuthLoginStep.BROWSER;
	return AuthLoginStep.CREDENTIALS;
}
