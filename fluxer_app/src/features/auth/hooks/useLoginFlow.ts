// SPDX-License-Identifier: AGPL-3.0-or-later

import {showBrowserLoginHandoffModal} from '@app/features/auth/flow/BrowserLoginHandoffModal';
import {useAuthForm} from '@app/features/auth/hooks/useAuthForm';
import {CaptchaCancelledError} from '@app/features/auth/hooks/useCaptcha';
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
} from '@app/features/auth/state/AuthFlow';
import {
	describePasskeyBridgeFailure,
	runPasskeyViaBridge,
	shouldSuggestPasskeyBridge,
} from '@app/features/auth/utils/PasskeyBridge';
import * as WebAuthnUtils from '@app/features/auth/utils/WebAuthnUtils';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import {useLingui} from '@lingui/react/macro';
import type {AuthenticationResponseJSON, PublicKeyCredentialRequestOptionsJSON} from '@simplewebauthn/browser';
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
	const {i18n} = useLingui();
	const [isPasskeyLoading, setIsPasskeyLoading] = useState(false);
	const [passkeyBridgeSuggested, setPasskeyBridgeSuggested] = useState(false);
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
	const completePasskeyLogin = useCallback(
		async (options: PublicKeyCredentialRequestOptionsJSON, credential: AuthenticationResponseJSON) => {
			const response = await authenticateWithWebAuthn({
				response: credential,
				challenge: options.challenge,
				inviteCode,
			});
			await onLoginSuccess?.(response);
			if (redirectPath) {
				RouterUtils.replaceWith(redirectPath);
			}
		},
		[inviteCode, onLoginSuccess, redirectPath],
	);
	const handlePasskeyLogin = useCallback(async () => {
		setIsPasskeyLoading(true);
		try {
			await WebAuthnUtils.assertWebAuthnSupported();
			const options = await getWebAuthnAuthenticationOptions();
			const credential = await WebAuthnUtils.performAuthentication(options);
			await completePasskeyLogin(options, credential);
		} catch (err) {
			if (err instanceof CaptchaCancelledError) {
				return;
			}
			logger.error('Passkey login failed', err);
			if (shouldSuggestPasskeyBridge(err)) {
				setPasskeyBridgeSuggested(true);
				return;
			}
			if (err instanceof WebAuthnUtils.PasskeyDomainUnsupportedError) {
				ToastCommands.error(i18n._(WebAuthnUtils.PASSKEY_DOMAIN_UNSUPPORTED_DESCRIPTOR));
				return;
			}
			const userCancelled =
				err instanceof DOMException && (err.name === 'NotAllowedError' || err.name === 'AbortError');
			if (isDesktop() && !userCancelled) {
				handleDesktopPasskeyHandoff();
			}
		} finally {
			setIsPasskeyLoading(false);
		}
	}, [completePasskeyLogin, handleDesktopPasskeyHandoff, i18n]);
	const handlePasskeyBridgeLogin = useCallback(() => {
		const options = getWebAuthnAuthenticationOptions();
		const credential = runPasskeyViaBridge('authenticate', options);
		setIsPasskeyLoading(true);
		Promise.all([options, credential])
			.then(([resolvedOptions, resolvedCredential]) => completePasskeyLogin(resolvedOptions, resolvedCredential))
			.catch((err: unknown) => {
				if (err instanceof CaptchaCancelledError) {
					return;
				}
				logger.error('Passkey login in the pop-up window failed', err);
				const descriptor = describePasskeyBridgeFailure(err);
				if (descriptor) {
					ToastCommands.error(i18n._(descriptor));
				}
			})
			.finally(() => {
				setIsPasskeyLoading(false);
			});
	}, [completePasskeyLogin, i18n]);
	return {
		form,
		isLoading,
		fieldErrors,
		error,
		handlePasskeyLogin,
		handlePasskeyBrowserLogin: handleDesktopPasskeyHandoff,
		handlePasskeyBridgeLogin,
		passkeyBridgeSuggested,
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
	const {i18n} = useLingui();
	const [isWebAuthnLoading, setIsWebAuthnLoading] = useState(false);
	const [passkeyBridgeSuggested, setPasskeyBridgeSuggested] = useState(false);
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
	const completeWebAuthnMfa = useCallback(
		async (options: PublicKeyCredentialRequestOptionsJSON, credential: AuthenticationResponseJSON) => {
			const response = await authenticateMfaWithWebAuthn({
				response: credential,
				challenge: options.challenge,
				ticket,
				inviteCode,
			});
			await onLoginSuccess?.(response);
		},
		[inviteCode, onLoginSuccess, ticket],
	);
	const handleWebAuthn = useCallback(async () => {
		setIsWebAuthnLoading(true);
		try {
			const options = await getWebAuthnMfaOptions(ticket);
			const credential = await WebAuthnUtils.performAuthentication(options);
			await completeWebAuthnMfa(options, credential);
		} catch (error) {
			logger.error('WebAuthn MFA failed', error);
			if (shouldSuggestPasskeyBridge(error)) {
				setPasskeyBridgeSuggested(true);
				return;
			}
			if (error instanceof WebAuthnUtils.PasskeyDomainUnsupportedError) {
				ToastCommands.error(i18n._(WebAuthnUtils.PASSKEY_DOMAIN_UNSUPPORTED_DESCRIPTOR));
			}
		} finally {
			setIsWebAuthnLoading(false);
		}
	}, [completeWebAuthnMfa, ticket, i18n]);
	const handlePasskeyBridge = useCallback(() => {
		const options = getWebAuthnMfaOptions(ticket);
		const credential = runPasskeyViaBridge('authenticate', options);
		setIsWebAuthnLoading(true);
		Promise.all([options, credential])
			.then(([resolvedOptions, resolvedCredential]) => completeWebAuthnMfa(resolvedOptions, resolvedCredential))
			.catch((error: unknown) => {
				logger.error('WebAuthn MFA in the pop-up window failed', error);
				const descriptor = describePasskeyBridgeFailure(error);
				if (descriptor) {
					ToastCommands.error(i18n._(descriptor));
				}
			})
			.finally(() => {
				setIsWebAuthnLoading(false);
			});
	}, [completeWebAuthnMfa, ticket, i18n]);
	const supports = useMemo(
		() => ({totp: methods.totp, webauthn: methods.webauthn, backupCodes: methods.backupCodes}),
		[methods.totp, methods.webauthn, methods.backupCodes],
	);
	return {
		form,
		isLoading,
		fieldErrors,
		handleWebAuthn,
		handlePasskeyBridge,
		passkeyBridgeSuggested,
		isWebAuthnLoading,
		supports,
	};
}
