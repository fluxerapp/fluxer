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
import * as WebAuthnUtils from '@app/features/auth/utils/WebAuthnUtils';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import {useLingui} from '@lingui/react/macro';
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
		try {
			await WebAuthnUtils.assertWebAuthnSupported();
			const options = await getWebAuthnAuthenticationOptions();
			const credential = await WebAuthnUtils.performAuthentication(options);
			const response = await authenticateWithWebAuthn({
				response: credential,
				challenge: options.challenge,
				inviteCode,
			});
			await onLoginSuccess?.(response);
			if (redirectPath) {
				RouterUtils.replaceWith(redirectPath);
			}
		} catch (err) {
			if (err instanceof CaptchaCancelledError) {
				return;
			}
			logger.error('Passkey login failed', err);
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
	}, [inviteCode, onLoginSuccess, redirectPath, handleDesktopPasskeyHandoff, i18n]);
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
	const {i18n} = useLingui();
	const [isWebAuthnLoading, setIsWebAuthnLoading] = useState(false);
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
		try {
			const options = await getWebAuthnMfaOptions(ticket);
			const credential = await WebAuthnUtils.performAuthentication(options);
			const response = await authenticateMfaWithWebAuthn({
				response: credential,
				challenge: options.challenge,
				ticket,
				inviteCode,
			});
			await onLoginSuccess?.(response);
		} catch (error) {
			logger.error('WebAuthn MFA failed', error);
			if (error instanceof WebAuthnUtils.PasskeyDomainUnsupportedError) {
				ToastCommands.error(i18n._(WebAuthnUtils.PASSKEY_DOMAIN_UNSUPPORTED_DESCRIPTOR));
			}
		} finally {
			setIsWebAuthnLoading(false);
		}
	}, [inviteCode, onLoginSuccess, ticket, i18n]);
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
