// SPDX-License-Identifier: AGPL-3.0-or-later

import {showBrowserLoginHandoffModal} from '@app/features/auth/flow/BrowserLoginHandoffModal';
import {useAuthForm} from '@app/features/auth/hooks/useAuthForm';
import {CaptchaCancelledError} from '@app/features/auth/hooks/useCaptcha';
import {
	isPasskeyCeremonyDismissed,
	runPasskeyBridgeNativeLogin,
	startPasskeyBridgePageLogin,
} from '@app/features/auth/passkey_migration/PasskeyLegacyCeremony';
import {readPasskeyLoginRoute, writePasskeyLoginRoute} from '@app/features/auth/passkey_migration/PasskeyLoginRoute';
import {isPasskeyMigrationOrigin, rpIdMatchesPage} from '@app/features/auth/passkey_migration/PasskeyMigrationOrigin';
import {
	authenticateMfaWithWebAuthn,
	authenticateWithWebAuthn,
	completeLoginSession,
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
import * as WebAuthnUtils from '@app/features/auth/utils/WebAuthnUtils';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {Platform} from '@app/features/platform/types/Platform';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import {useCallback, useMemo, useRef, useState} from 'react';

const logger = Logger.create('useLoginFlow');

export type LoginCompletionMode =
	| {
			type: 'redirect';
			path: string;
	  }
	| {
			type: 'callback';
			onComplete: () => void | Promise<void>;
	  };

export function useLoginCompletion(mode: LoginCompletionMode) {
	const modeRef = useRef(mode);
	modeRef.current = mode;
	const completeLogin = useCallback(async (payload: LoginSuccessPayload) => {
		await completeLoginSession(payload);
		const currentMode = modeRef.current;
		if (currentMode.type === 'redirect') {
			RouterUtils.replaceWith(currentMode.path);
		} else {
			await currentMode.onComplete();
		}
	}, []);
	return {completeLogin};
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

interface LoginFormControllerOptions {
	inviteCode?: string;
	redirectPath?: string;
	onLoginSuccess?: (payload: LoginSuccessPayload) => Promise<void> | void;
	onRequireMfa?: (challenge: MfaChallenge) => void;
	onRequireIpAuthorization?: (challenge: IpAuthorizationChallenge) => void;
}

export function useLoginFormController({
	inviteCode,
	redirectPath,
	onLoginSuccess,
	onRequireMfa,
	onRequireIpAuthorization,
}: LoginFormControllerOptions) {
	const [isPasskeyLoading, setIsPasskeyLoading] = useState(false);
	const {form, isLoading, fieldErrors, error} = useAuthForm({
		initialValues: {email: '', password: ''},
		onSubmit: async (values) => {
			const result = await loginWithPassword({
				email: values.email,
				password: values.password,
				inviteCode,
			});
			handleLoginOutcome(result, onLoginSuccess, onRequireMfa, onRequireIpAuthorization, redirectPath);
		},
		firstFieldName: 'email',
		redirectPath: undefined,
	});
	const handleDesktopPasskeyHandoff = useCallback(() => {
		showBrowserLoginHandoffModal(async (payload) => {
			await onLoginSuccess?.(payload);
			if (redirectPath) {
				RouterUtils.replaceWith(redirectPath);
			}
		});
	}, [onLoginSuccess, redirectPath]);
	const handlePasskeyLogin = useCallback(async () => {
		setIsPasskeyLoading(true);
		const migrationOrigin = isPasskeyMigrationOrigin();
		let navigating = false;
		try {
			let outcome: LegacyPasskeyLoginOutcome | null = null;
			if (!migrationOrigin || readPasskeyLoginRoute() === 'native') {
				await WebAuthnUtils.assertWebAuthnSupported();
				const options = await getWebAuthnAuthenticationOptions();
				const credential = await WebAuthnUtils.performAuthentication(options).catch((error: unknown) => {
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
			if (err instanceof CaptchaCancelledError) {
				return;
			}
			logger.error('Passkey login failed', err);
			const userCancelled =
				err instanceof DOMException && (err.name === 'NotAllowedError' || err.name === 'AbortError');
			if (isDesktop() && !userCancelled) {
				handleDesktopPasskeyHandoff();
			}
		} finally {
			if (!navigating) {
				setIsPasskeyLoading(false);
			}
		}
	}, [inviteCode, onLoginSuccess, redirectPath, handleDesktopPasskeyHandoff]);
	return {
		form,
		isLoading,
		fieldErrors,
		error,
		handlePasskeyLogin,
		handlePasskeyBrowserLogin: handleDesktopPasskeyHandoff,
		isPasskeyLoading,
	};
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
			const options = await getWebAuthnMfaOptions(ticket);
			const migrationOrigin = isPasskeyMigrationOrigin();
			const runsOnPage =
				!migrationOrigin || (!preferLegacyRef.current && (options.rpId === undefined || rpIdMatchesPage(options.rpId)));
			let response: LoginSuccessPayload;
			if (runsOnPage) {
				const credential = await WebAuthnUtils.performAuthentication(options).catch((error: unknown) => {
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
				});
			} else {
				const outcome = await runLegacyPasskeyLogin({ticket, ...methods}).catch((error: unknown) => {
					preferLegacyRef.current = false;
					throw error;
				});
				navigating = outcome === 'navigating';
				if (outcome === 'cancelled') {
					preferLegacyRef.current = false;
				}
				if (typeof outcome === 'string') {
					return;
				}
				response = outcome;
			}
			await onLoginSuccess?.(response);
		} catch (error) {
			logger.error('WebAuthn MFA failed', error);
		} finally {
			if (!navigating) {
				setIsWebAuthnLoading(false);
			}
		}
	}, [inviteCode, methods, onLoginSuccess, ticket]);
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
