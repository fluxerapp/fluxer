/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {useCallback, useMemo, useRef, useState} from 'react';
import {showBrowserLoginHandoffModal} from '~/components/auth/BrowserLoginHandoffModal';
import {useAuthForm} from '~/hooks/useAuthForm';
import RuntimeConfigStore from '~/stores/RuntimeConfigStore';
import {isDesktop} from '~/utils/NativeUtils';
import * as RouterUtils from '~/utils/RouterUtils';
import * as WebAuthnUtils from '~/utils/WebAuthnUtils';
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
	sendMfaSms,
} from '~/viewmodels/auth/AuthFlow';
import {CaptchaCancelledError} from './useCaptcha';

export type LoginCompletionMode =
	| {type: 'redirect'; path: string}
	| {type: 'callback'; onComplete: () => void | Promise<void>};

export function useLoginCompletion(mode: LoginCompletionMode) {
	const modeRef = useRef(mode);
	modeRef.current = mode;

	const completeLogin = useCallback(async ({token, userId}: LoginSuccessPayload) => {
		await completeLoginSession({token, userId});

		const currentMode = modeRef.current;
		if (currentMode.type === 'redirect') {
			RouterUtils.replaceWith(currentMode.path);
		} else {
			await currentMode.onComplete();
		}
	}, []);

	return {completeLogin};
}

export type {IpAuthorizationChallenge, LoginSuccessPayload, MfaChallenge} from '~/viewmodels/auth/AuthFlow';

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
	customApiEndpoint?: string;
}

export function useLoginFormController({
	inviteCode,
	redirectPath,
	onLoginSuccess,
	onRequireMfa,
	onRequireIpAuthorization,
	customApiEndpoint,
}: LoginFormControllerOptions) {
	const [isPasskeyLoading, setIsPasskeyLoading] = useState(false);

	const {form, isLoading, fieldErrors, error} = useAuthForm({
		initialValues: {email: '', password: ''},
		onSubmit: async (values) => {
			const result = await loginWithPassword({
				email: values.email,
				password: values.password,
				inviteCode,
				customApiEndpoint,
			});

			handleLoginOutcome(result, onLoginSuccess, onRequireMfa, onRequireIpAuthorization, redirectPath);
		},
		firstFieldName: 'email',
		redirectPath: undefined,
	});

	const handleDesktopPasskeyHandoff = useCallback(async () => {
		let targetWebAppUrl: string | undefined;

		if (customApiEndpoint) {
			try {
				await RuntimeConfigStore.connectToEndpoint(customApiEndpoint);
				targetWebAppUrl = RuntimeConfigStore.webAppEndpoint;
			} catch (err) {
				console.error('Failed to connect to custom instance:', err);
			}
		}

		showBrowserLoginHandoffModal(async ({token, userId}) => {
			await onLoginSuccess?.({token, userId});
			if (redirectPath) {
				RouterUtils.replaceWith(redirectPath);
			}
		}, targetWebAppUrl);
	}, [onLoginSuccess, redirectPath, customApiEndpoint]);

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

			await onLoginSuccess?.({token: response.token, userId: response.userId});
			if (redirectPath) {
				RouterUtils.replaceWith(redirectPath);
			}
		} catch (err) {
			if (err instanceof CaptchaCancelledError) {
				return;
			}
			console.error('Passkey login failed', err);
			const userCancelled =
				err instanceof DOMException && (err.name === 'NotAllowedError' || err.name === 'AbortError');
			if (isDesktop() && !userCancelled) {
				handleDesktopPasskeyHandoff();
			}
		} finally {
			setIsPasskeyLoading(false);
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
	methods: {sms: boolean; totp: boolean; webauthn: boolean};
	inviteCode?: string;
	onLoginSuccess?: (payload: LoginSuccessPayload) => Promise<void> | void;
}

type CodeMethod = 'totp' | 'sms';

export function useMfaController({ticket, methods, inviteCode, onLoginSuccess}: MfaControllerOptions) {
	const [selectedMethod, setSelectedMethod] = useState<CodeMethod | null>(() => {
		if (methods.totp) {
			return 'totp';
		}
		if (methods.sms) {
			return 'sms';
		}
		return null;
	});
	const [smsSent, setSmsSent] = useState(false);
	const [isWebAuthnLoading, setIsWebAuthnLoading] = useState(false);

	const {form, isLoading, fieldErrors} = useAuthForm({
		initialValues: {code: ''},
		onSubmit: async (values) => {
			if (!selectedMethod) {
				return;
			}
			const normalizedCode = values.code.split(' ').join('');
			const response = await loginWithMfaCode({
				code: normalizedCode,
				ticket,
				inviteCode,
				method: selectedMethod,
			});

			await onLoginSuccess?.({token: response.token, userId: response.userId});
		},
		firstFieldName: 'code',
		redirectPath: undefined,
	});

	const handleSendSms = useCallback(async () => {
		await sendMfaSms(ticket);
		setSmsSent(true);
	}, [ticket]);

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
			await onLoginSuccess?.({token: response.token, userId: response.userId});
		} catch (error) {
			console.error('WebAuthn MFA failed', error);
		} finally {
			setIsWebAuthnLoading(false);
		}
	}, [inviteCode, onLoginSuccess, ticket]);

	const supports = useMemo(
		() => ({sms: methods.sms, totp: methods.totp, webauthn: methods.webauthn}),
		[methods.sms, methods.totp, methods.webauthn],
	);

	return {
		form,
		isLoading,
		fieldErrors,
		selectedMethod,
		setSelectedMethod,
		smsSent,
		handleSendSms,
		handleWebAuthn,
		isWebAuthnLoading,
		supports,
	};
}
