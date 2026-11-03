// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {type RuntimeConfigSnapshot, runtimeInstanceKey} from '@app/features/app/state/InstanceSnapshotStore';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {resolveSnapshotInstanceDomain} from '@app/features/auth/AccountDisplayUtils';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {
	CONNECTING_TO_INSTANCE_DESCRIPTOR,
	INSTANCE_CONNECT_FAILED_DESCRIPTOR,
} from '@app/features/auth/flow/instance_selector/InstanceDiscoveryFailure';
import {getAuthErrorMessage, useAuthForm} from '@app/features/auth/hooks/useAuthForm';
import {
	isPasskeyCeremonyDismissed,
	runPasskeyBridgeNativeLogin,
	startPasskeyBridgePageLogin,
} from '@app/features/auth/passkey_migration/PasskeyLegacyCeremony';
import {readPasskeyLoginRoute, writePasskeyLoginRoute} from '@app/features/auth/passkey_migration/PasskeyLoginRoute';
import {isPasskeyMigrationOrigin, rpIdMatchesPage} from '@app/features/auth/passkey_migration/PasskeyMigrationOrigin';
import type {UserData} from '@app/features/auth/state/AccountStorageContract';
import {getAccountKey} from '@app/features/auth/state/AccountStorageKey';
import Accounts, {AccountReplacementRecoveryFailedError} from '@app/features/auth/state/Accounts';
import Authentication from '@app/features/auth/state/Authentication';
import {
	authenticateMfaWithWebAuthn,
	authenticateWithWebAuthn,
	getWebAuthnAuthenticationOptions,
	getWebAuthnMfaOptions,
	type IpAuthorizationChallenge,
	type LoginResult,
	type LoginSuccessPayload,
	loginWithMfaCode,
	loginWithPassword,
	type MfaChallenge,
	toLoginSuccessPayload,
} from '@app/features/auth/state/AuthFlow';
import {loginIdentifierField} from '@app/features/auth/utils/AccountSignInIdentifier';
import {describePasskeyError, describePasskeyPromptEnded} from '@app/features/auth/utils/PasskeyErrorMessages';
import {classifyPasskeyFailure} from '@app/features/auth/utils/PasskeyFailure';
import * as WebAuthnUtils from '@app/features/auth/utils/WebAuthnUtils';
import {
	ForegroundGatewayConnectionRecoverableError,
	ForegroundGatewayRecoveryExhaustedError,
} from '@app/features/gateway/transport/ForegroundGatewayConnectionFailure';
import {GatewayReadyTimeoutError} from '@app/features/gateway/transport/GatewayReadinessWaiters';
import {COULDN_T_VERIFY_WITH_PASSKEY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {type Account, SessionExpiredError} from '@app/features/platform/state/AuthSession';
import {Platform} from '@app/features/platform/types/Platform';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {browserSupportsWebAuthn} from '@simplewebauthn/browser';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const logger = Logger.create('useLoginFlow');

const LOGIN_CONNECTING_STATUS_DELAY_MS = 1500;

const PASSKEY_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'Passkey sign-in is unavailable here.',
	comment: 'Auth error shown when passkey sign-in cannot run in the current authentication environment.',
});

export const PasskeyLoginCapability = Object.freeze({
	AVAILABLE: 'available',
	BROWSER_HANDOFF: 'browser_handoff',
	UNAVAILABLE: 'unavailable',
} as const);

export type PasskeyLoginCapability = (typeof PasskeyLoginCapability)[keyof typeof PasskeyLoginCapability];

export function resolveInitialPasskeyLoginCapability(): PasskeyLoginCapability {
	if (isDesktop() || browserSupportsWebAuthn()) {
		return PasskeyLoginCapability.AVAILABLE;
	}
	return PasskeyLoginCapability.UNAVAILABLE;
}

export async function resolvePasskeyLoginCapability(): Promise<PasskeyLoginCapability> {
	if (await WebAuthnUtils.isWebAuthnSupported()) {
		return PasskeyLoginCapability.AVAILABLE;
	}
	if (isDesktop()) {
		return PasskeyLoginCapability.BROWSER_HANDOFF;
	}
	return PasskeyLoginCapability.UNAVAILABLE;
}

function requireLoginRuntimeSnapshot(runtimeSnapshot: RuntimeConfigSnapshot | null): RuntimeConfigSnapshot {
	if (runtimeSnapshot == null) {
		throw new Error('Authentication cannot start without a selected instance runtime');
	}
	return runtimeSnapshot;
}

type LegacyPasskeyLoginOutcome = LoginSuccessPayload | 'cancelled' | 'navigating';

async function runLegacyPasskeyLogin(mfa: MfaChallenge | null): Promise<LegacyPasskeyLoginOutcome> {
	if (!Platform.isElectron) {
		await startPasskeyBridgePageLogin(mfa, `${window.location.pathname}${window.location.search}`);
		return 'navigating';
	}
	const result =
		mfa === null
			? await runPasskeyBridgeNativeLogin('login')
			: await runPasskeyBridgeNativeLogin('login_mfa', mfa.ticket);
	return result.status === 'completed' ? toLoginSuccessPayload(result) : 'cancelled';
}

const handleLoginOutcome = async (
	result: LoginResult,
	onLoginSuccess?: (payload: LoginSuccessPayload) => Promise<void> | void,
	onRequireMfa?: (challenge: MfaChallenge) => void,
	onRequireIpAuthorization?: (challenge: IpAuthorizationChallenge) => void,
	redirectPath?: string,
) => {
	if (result.type === 'ip_authorization') {
		onRequireIpAuthorization?.(result.challenge);
		return;
	}
	if (result.type === 'mfa') {
		onRequireMfa?.(result.challenge);
		return;
	}
	if (result.type === 'success') {
		await onLoginSuccess?.(result.payload);
		if (redirectPath) {
			RouterUtils.replaceWith(redirectPath);
		}
	}
};

function isLoginConnectionFailure(error: unknown): boolean {
	if (error instanceof AccountReplacementRecoveryFailedError) {
		return isLoginConnectionFailure(error.errors[0]);
	}
	return (
		error instanceof ForegroundGatewayConnectionRecoverableError ||
		error instanceof ForegroundGatewayRecoveryExhaustedError ||
		error instanceof GatewayReadyTimeoutError
	);
}

interface LoginFormControllerOptions {
	inviteCode?: string;
	redirectPath?: string;
	runtimeSnapshot: RuntimeConfigSnapshot | null;
	onLoginSuccess?: (payload: LoginSuccessPayload) => Promise<void> | void;
	onRequireMfa?: (challenge: MfaChallenge) => void;
	onRequireIpAuthorization?: (challenge: IpAuthorizationChallenge) => void;
	onDesktopPasskeyHandoff: () => void;
}

export function useLoginFormController({
	inviteCode,
	redirectPath,
	runtimeSnapshot,
	onLoginSuccess,
	onRequireMfa,
	onRequireIpAuthorization,
	onDesktopPasskeyHandoff,
}: LoginFormControllerOptions) {
	const {i18n} = useLingui();
	const [isPasskeyLoading, setIsPasskeyLoading] = useState(false);
	const [connectingDomain, setConnectingDomain] = useState<string | null>(null);
	const connectingTimerRef = useRef<number | null>(null);
	const clearConnectingStatus = useCallback(() => {
		if (connectingTimerRef.current !== null) {
			window.clearTimeout(connectingTimerRef.current);
			connectingTimerRef.current = null;
		}
		setConnectingDomain(null);
	}, []);
	useEffect(() => clearConnectingStatus, [clearConnectingStatus]);
	const identifierField = loginIdentifierField(runtimeSnapshot);
	const {form, isLoading, fieldErrors, error} = useAuthForm({
		initialValues: {[identifierField]: '', password: ''},
		onSubmit: async (values) => {
			const requestRuntimeSnapshot = requireLoginRuntimeSnapshot(runtimeSnapshot);
			const identifier = values[identifierField] ?? '';
			const result = await loginWithPassword({
				...(identifierField === 'login' ? {login: identifier.trim()} : {email: identifier}),
				password: values.password,
				inviteCode,
				runtimeSnapshot: requestRuntimeSnapshot,
			});
			if (result.type !== 'success') {
				await handleLoginOutcome(result, onLoginSuccess, onRequireMfa, onRequireIpAuthorization, redirectPath);
				return;
			}
			const domain = resolveSnapshotInstanceDomain(requestRuntimeSnapshot);
			connectingTimerRef.current = window.setTimeout(() => {
				connectingTimerRef.current = null;
				setConnectingDomain(domain);
			}, LOGIN_CONNECTING_STATUS_DELAY_MS);
			try {
				await handleLoginOutcome(result, onLoginSuccess, onRequireMfa, onRequireIpAuthorization, redirectPath);
			} catch (loginError) {
				if (domain !== null && isLoginConnectionFailure(loginError)) {
					logger.error('Signed in but the live connection never became ready', loginError);
					throw new Error(i18n._(INSTANCE_CONNECT_FAILED_DESCRIPTOR, {domain}), {cause: loginError});
				}
				throw loginError;
			} finally {
				clearConnectingStatus();
			}
		},
		firstFieldName: identifierField,
		redirectPath: undefined,
	});
	const handlePasskeyLogin = useCallback(async () => {
		setIsPasskeyLoading(true);
		const migrationOrigin = isPasskeyMigrationOrigin();
		let navigating = false;
		try {
			const requestRuntimeSnapshot = requireLoginRuntimeSnapshot(runtimeSnapshot);
			let outcome: LegacyPasskeyLoginOutcome | null = null;
			if (!migrationOrigin || readPasskeyLoginRoute() === 'native') {
				const capability = await resolvePasskeyLoginCapability();
				if (capability === PasskeyLoginCapability.BROWSER_HANDOFF) {
					onDesktopPasskeyHandoff();
					return;
				}
				if (capability === PasskeyLoginCapability.UNAVAILABLE) {
					ToastCommands.error(i18n._(PASSKEY_UNAVAILABLE_DESCRIPTOR));
					return;
				}
				const options = await getWebAuthnAuthenticationOptions(requestRuntimeSnapshot);
				const credential = await WebAuthnUtils.performAuthentication(
					options,
					runtimeInstanceKey(requestRuntimeSnapshot),
				).catch((error: unknown) => {
					if (migrationOrigin && isPasskeyCeremonyDismissed(error)) {
						return null;
					}
					throw error;
				});
				if (credential !== null) {
					outcome = await authenticateWithWebAuthn({
						response: credential,
						challenge: options.challenge,
						inviteCode,
						runtimeSnapshot: requestRuntimeSnapshot,
					});
				}
			}
			if (outcome === null) {
				writePasskeyLoginRoute('legacy');
				outcome = await runLegacyPasskeyLogin(null).catch((error: unknown) => {
					writePasskeyLoginRoute('native');
					throw error;
				});
				if (outcome === 'cancelled') {
					writePasskeyLoginRoute('native');
					ToastCommands.error(describePasskeyPromptEnded(i18n));
				}
			}
			navigating = outcome === 'navigating';
			if (typeof outcome === 'string') {
				return;
			}
			await onLoginSuccess?.(outcome);
			if (redirectPath) {
				RouterUtils.replaceWith(redirectPath);
			}
		} catch (err) {
			logger.error('Passkey login failed', err);
			if (AuthenticationCommands.showAccountRestriction(err)) {
				return;
			}
			const failure = classifyPasskeyFailure(err, isDesktop() ? 'browser_handoff' : 'show_error');
			if (failure === 'aborted') {
				return;
			}
			if (failure === 'browser_handoff') {
				onDesktopPasskeyHandoff();
			} else {
				ToastCommands.error(describePasskeyError(i18n, err, null) ?? i18n._(COULDN_T_VERIFY_WITH_PASSKEY_DESCRIPTOR));
			}
		} finally {
			if (!navigating) {
				setIsPasskeyLoading(false);
			}
		}
	}, [inviteCode, onLoginSuccess, redirectPath, onDesktopPasskeyHandoff, i18n, runtimeSnapshot]);
	const connectingMessage =
		connectingDomain === null ? null : i18n._(CONNECTING_TO_INSTANCE_DESCRIPTOR, {domain: connectingDomain});
	return {
		form,
		identifierField,
		isLoading,
		fieldErrors,
		error,
		handlePasskeyLogin,
		isPasskeyLoading,
		connectingMessage,
	};
}

export interface AccountSwitchController {
	readonly isSwitching: boolean;
	readonly switchToAccount: (account: Account) => Promise<void>;
}

export interface StoredAccountLoginPayload {
	readonly token: string;
	readonly userId: string;
	readonly userData?: UserData;
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
}

interface StoredAccountSelectionHandlers {
	readonly redirectPath: string | null;
	readonly onLoginWithStoredAccount: (payload: StoredAccountLoginPayload) => Promise<void>;
	readonly onSessionExpired: (account: Account) => void;
}

async function selectStoredAccount(
	account: Account,
	{redirectPath, onLoginWithStoredAccount, onSessionExpired}: StoredAccountSelectionHandlers,
): Promise<void> {
	if (account.isValid === false) {
		onSessionExpired(account);
		return;
	}
	const accountKey = getAccountKey(account);
	try {
		if (Accounts.canSwitchAccounts) {
			await Accounts.switchToAccount(
				accountKey,
				redirectPath == null || redirectPath === '' ? Routes.ME : redirectPath,
			);
			return;
		}
		const {token, userId, runtimeSnapshot} = await Accounts.prepareAccountCredentials(accountKey);
		await onLoginWithStoredAccount({token, userId, userData: account.userData, runtimeSnapshot});
	} catch (error) {
		const updatedAccount = Accounts.getAccount(accountKey);
		if (error instanceof SessionExpiredError || updatedAccount?.isValid === false) {
			onSessionExpired(updatedAccount ?? account);
			return;
		}
		throw error;
	}
}

interface AccountSwitchControllerOptions {
	redirectPath: string | null;
	onError: (message: string) => void;
	onSessionExpired: (account: Account) => void;
	onLoginWithStoredAccount: (payload: StoredAccountLoginPayload) => Promise<void>;
}

export function useAccountSwitchController({
	redirectPath,
	onError,
	onSessionExpired,
	onLoginWithStoredAccount,
}: AccountSwitchControllerOptions): AccountSwitchController {
	const {i18n} = useLingui();
	const [isSwitching, setIsSwitching] = useState(false);
	const switchToAccount = useCallback(
		async (account: Account) => {
			if (account.isValid === false) {
				onSessionExpired(account);
				return;
			}
			setIsSwitching(true);
			try {
				await selectStoredAccount(account, {redirectPath, onLoginWithStoredAccount, onSessionExpired});
			} catch (error) {
				onError(getAuthErrorMessage(error, i18n));
			} finally {
				setIsSwitching(false);
			}
		},
		[i18n, onError, onLoginWithStoredAccount, onSessionExpired, redirectPath],
	);
	return {isSwitching, switchToAccount};
}

interface MfaControllerOptions {
	ticket: string;
	methods: {
		totp: boolean;
		webauthn: boolean;
		backupCodes: boolean;
	};
	inviteCode?: string;
	onLoginSuccess?: (payload: LoginSuccessPayload) => Promise<void> | void;
}

export function useMfaController({ticket, methods, inviteCode, onLoginSuccess}: MfaControllerOptions) {
	const {i18n} = useLingui();
	const [isWebAuthnLoading, setIsWebAuthnLoading] = useState(false);
	const preferLegacyRef = useRef(false);
	const {form, isLoading, fieldErrors} = useAuthForm({
		initialValues: {code: ''},
		onSubmit: async (values) => {
			if (!methods.totp && !methods.backupCodes) {
				return;
			}
			const normalizedCode = values.code.replace(/[\s-]/g, '');
			const response = await loginWithMfaCode({
				code: normalizedCode,
				ticket,
				inviteCode,
				runtimeSnapshot: Authentication.currentMfaRuntimeSnapshot ?? RuntimeConfig.getSnapshot(),
			});
			await onLoginSuccess?.(response);
		},
		firstFieldName: 'code',
		redirectPath: undefined,
	});
	const handleWebAuthn = useCallback(async () => {
		setIsWebAuthnLoading(true);
		let navigating = false;
		try {
			const runtimeSnapshot = Authentication.currentMfaRuntimeSnapshot ?? RuntimeConfig.getSnapshot();
			const options = await getWebAuthnMfaOptions(ticket, runtimeSnapshot);
			const migrationOrigin = isPasskeyMigrationOrigin();
			const runsOnPage =
				!migrationOrigin || (!preferLegacyRef.current && (options.rpId === undefined || rpIdMatchesPage(options.rpId)));
			let response: LoginSuccessPayload;
			if (runsOnPage) {
				const credential = await WebAuthnUtils.performAuthentication(
					options,
					runtimeInstanceKey(runtimeSnapshot),
				).catch((error: unknown) => {
					if (migrationOrigin && isPasskeyCeremonyDismissed(error)) {
						preferLegacyRef.current = true;
					}
					throw error;
				});
				response = await authenticateMfaWithWebAuthn({
					response: credential,
					challenge: options.challenge,
					ticket,
					inviteCode,
					runtimeSnapshot,
				});
			} else {
				const outcome = await runLegacyPasskeyLogin({ticket, ...methods}).catch((error: unknown) => {
					preferLegacyRef.current = false;
					throw error;
				});
				navigating = outcome === 'navigating';
				if (outcome === 'cancelled') {
					preferLegacyRef.current = false;
					ToastCommands.error(describePasskeyPromptEnded(i18n));
				}
				if (typeof outcome === 'string') {
					return;
				}
				response = outcome;
			}
			await onLoginSuccess?.(response);
		} catch (error) {
			logger.error('WebAuthn MFA failed', error);
			if (AuthenticationCommands.showAccountRestriction(error)) {
				return;
			}
			if (classifyPasskeyFailure(error, 'show_error') !== 'aborted') {
				ToastCommands.error(describePasskeyError(i18n, error, null) ?? i18n._(COULDN_T_VERIFY_WITH_PASSKEY_DESCRIPTOR));
			}
		} finally {
			if (!navigating) {
				setIsWebAuthnLoading(false);
			}
		}
	}, [i18n, inviteCode, methods, onLoginSuccess, ticket]);
	const supports = useMemo(
		() => ({totp: methods.totp, webauthn: methods.webauthn, backupCodes: methods.backupCodes}),
		[methods.totp, methods.webauthn, methods.backupCodes],
	);
	return {
		form,
		isLoading,
		fieldErrors,
		handleWebAuthn,
		isWebAuthnLoading,
		supports,
	};
}
