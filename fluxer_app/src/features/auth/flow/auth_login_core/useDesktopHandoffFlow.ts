// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import type {
	DesktopHandoffInfoResponse,
	DesktopHandoffReturnMethod,
} from '@app/features/auth/commands/AuthenticationCommands';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {DesktopHandoffMode} from '@app/features/auth/flow/auth_login_core/AuthLoginStepTypes';
import type {UserData} from '@app/features/auth/state/AccountStorage';
import {type InstanceHTTPTarget, instanceTargetFromSnapshot} from '@app/features/platform/transport/InstanceHTTP';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as FormUtils from '@app/lib/forms';
import {formatDesktopHandoffCode, parseDesktopHandoffCode} from '@fluxer/schema/src/domains/auth/DesktopHandoffCode';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useCallback, useMemo, useRef, useState} from 'react';

const HANDOFF_REQUEST_EXPIRED_DESCRIPTOR = msg({
	message: 'This sign-in request expired or was cancelled in the {productName} app. Start again from the app.',
	comment:
		'Error in the browser when the sign-in request from the desktop app no longer exists, because it expired or the app cancelled it. productName is the app name, such as Fluxer.',
});
const TYPED_CODE_NOT_FOUND_DESCRIPTOR = msg({
	message: 'That code is not valid on {instanceHost}. Check the code in the {productName} app and try again.',
	comment:
		'Error in the browser when a code typed from the app is not found. instanceHost is the server the browser checked, such as api.fluxer.app. productName is the app name, such as Fluxer.',
});
const COULDN_T_VERIFY_THAT_CODE_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: "Couldn't verify that code. Try again.",
	comment: 'Desktop handoff flow error shown when verifying the pairing code fails for an unexpected reason.',
});
const COULDN_T_COMPLETE_SIGN_IN_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: "Couldn't complete sign-in. Try again.",
	comment: 'Desktop handoff flow error shown when finalizing the sign-in fails after a valid code.',
});
const INSTANCE_MISMATCH_DESCRIPTOR = msg({
	message:
		'This account is on {accountHost}, but the {productName} app is signing in to {requestHost}. Choose an account on {requestHost}.',
	comment:
		'Error in the browser when the chosen account belongs to a different server than the app that asked to sign in. accountHost and requestHost are server addresses. productName is the app name, such as Fluxer.',
});

const logger = new Logger('useDesktopHandoffFlow');

export const DesktopHandoffCodeSource = Object.freeze({
	LINK: 'link',
	TYPED: 'typed',
} as const);

export type DesktopHandoffCodeSource = (typeof DesktopHandoffCodeSource)[keyof typeof DesktopHandoffCodeSource];

export function isHandoffRequest(params: URLSearchParams): boolean {
	return params.get('handoff') === '1' || params.get('desktop_handoff') === '1';
}

export interface DesktopHandoffRequest {
	readonly code: string | null;
	readonly apiOrigin: string | null;
	readonly identifier: string | null;
}

function readOrigin(value: string | null | undefined): string | null {
	if (value == null || value.length === 0) {
		return null;
	}
	try {
		return new URL(value).origin;
	} catch {
		return null;
	}
}

export function readDesktopHandoffRequest(search: string): DesktopHandoffRequest {
	const params = new URLSearchParams(search);
	const code = parseDesktopHandoffCode(params.get('code') ?? '');
	return {
		code: code == null ? null : formatDesktopHandoffCode(code),
		apiOrigin: readOrigin(params.get('api')),
		identifier: params.get('email') ?? params.get('login'),
	};
}

function hostOf(endpoint: string): string {
	try {
		return new URL(endpoint).host;
	} catch {
		return endpoint;
	}
}

function matchesIdentifier(identifier: string, userData: UserData): boolean {
	const wanted = identifier.trim().toLowerCase();
	return wanted === userData.username.toLowerCase() || wanted === userData.email?.toLowerCase();
}

interface Options {
	enabled: boolean;
	hasStoredAccounts: boolean;
	initialMode?: DesktopHandoffMode;
}

export interface DesktopHandoffAccountCredentials {
	readonly token: string;
	readonly userId: string;
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
	readonly userData?: UserData;
}

export function useDesktopHandoffFlow({enabled, hasStoredAccounts, initialMode}: Options) {
	const {i18n} = useLingui();
	const derivedInitial = useMemo<DesktopHandoffMode>(() => {
		if (!enabled) return DesktopHandoffMode.IDLE;
		if (initialMode) return initialMode;
		return hasStoredAccounts ? DesktopHandoffMode.SELECTING : DesktopHandoffMode.LOGIN;
	}, [enabled, hasStoredAccounts, initialMode]);
	const [request] = useState(() => readDesktopHandoffRequest(window.location.search));
	const [mode, setMode] = useState<DesktopHandoffMode>(derivedInitial);
	const [error, setError] = useState<string | null>(null);
	const [clientInfo, setClientInfo] = useState<DesktopHandoffInfoResponse['client_info']>(null);
	const [handoffCode, setHandoffCode] = useState<string | null>(null);
	const [returnMethod, setReturnMethod] = useState<DesktopHandoffReturnMethod | null>(null);
	const [codeSource, setCodeSource] = useState<DesktopHandoffCodeSource | null>(null);
	const [returnUrl, setReturnUrl] = useState<string | null>(null);
	const [account, setAccount] = useState<UserData | null>(null);
	const tokenRef = useRef<string | null>(null);
	const userIdRef = useRef<string | null>(null);
	const operationRevisionRef = useRef(0);
	const targetRef = useRef<InstanceHTTPTarget | null>(null);
	const linkCodeUsedRef = useRef(false);
	const failWith = useCallback((message: string) => {
		setMode(DesktopHandoffMode.ERROR);
		setError(message);
	}, []);
	const lookUpCode = useCallback(
		async (code: string, source: DesktopHandoffCodeSource) => {
			const token = tokenRef.current;
			const target = targetRef.current;
			if (token == null || token.length === 0 || target == null) return;
			const operationRevision = ++operationRevisionRef.current;
			setMode(DesktopHandoffMode.FETCHING_INFO);
			setError(null);
			setHandoffCode(code);
			setCodeSource(source);
			try {
				const info = await AuthenticationCommands.fetchDesktopHandoffInfo({code, token, target});
				if (operationRevision !== operationRevisionRef.current) return;
				if (info.status === 'expired') {
					failWith(
						source === DesktopHandoffCodeSource.LINK
							? i18n._(HANDOFF_REQUEST_EXPIRED_DESCRIPTOR, {productName: PRODUCT_NAME})
							: i18n._(TYPED_CODE_NOT_FOUND_DESCRIPTOR, {
									instanceHost: hostOf(target.apiEndpoint),
									productName: PRODUCT_NAME,
								}),
					);
					return;
				}
				setClientInfo(info.client_info);
				setReturnMethod(
					source === DesktopHandoffCodeSource.LINK && info.return_method === 'deep_link' ? 'deep_link' : 'code',
				);
				setMode(DesktopHandoffMode.APPROVING);
			} catch (e) {
				if (operationRevision !== operationRevisionRef.current) return;
				failWith(
					e && typeof e === 'object' && 'body' in e
						? FormUtils.extractErrorMessage(i18n, e)
						: i18n._(COULDN_T_VERIFY_THAT_CODE_PLEASE_TRY_AGAIN_DESCRIPTOR),
				);
			}
		},
		[failWith, i18n],
	);
	const start = useCallback(
		({token, userId, runtimeSnapshot, userData}: DesktopHandoffAccountCredentials) => {
			if (!enabled) return;
			operationRevisionRef.current += 1;
			tokenRef.current = token;
			userIdRef.current = userId;
			targetRef.current = instanceTargetFromSnapshot(runtimeSnapshot);
			setAccount(userData ?? null);
			setError(null);
			setHandoffCode(null);
			setClientInfo(null);
			setReturnMethod(null);
			setReturnUrl(null);
			const accountOrigin = readOrigin(runtimeSnapshot.apiEndpoint);
			if (request.apiOrigin != null && accountOrigin != null && request.apiOrigin !== accountOrigin) {
				failWith(
					i18n._(INSTANCE_MISMATCH_DESCRIPTOR, {
						accountHost: hostOf(accountOrigin),
						requestHost: hostOf(request.apiOrigin),
						productName: PRODUCT_NAME,
					}),
				);
				return;
			}
			if (request.code != null && !linkCodeUsedRef.current) {
				linkCodeUsedRef.current = true;
				void lookUpCode(request.code, DesktopHandoffCodeSource.LINK);
				return;
			}
			setMode(DesktopHandoffMode.CODE_INPUT);
		},
		[enabled, failWith, i18n, lookUpCode, request],
	);
	const submitCode = useCallback((code: string) => lookUpCode(code, DesktopHandoffCodeSource.TYPED), [lookUpCode]);
	const approve = useCallback(async () => {
		const token = tokenRef.current;
		const userId = userIdRef.current;
		const target = targetRef.current;
		if (!handoffCode || !token || !userId || target == null || returnMethod == null) return;
		const operationRevision = ++operationRevisionRef.current;
		setMode(DesktopHandoffMode.COMPLETING);
		setError(null);
		try {
			const completedReturnUrl = await AuthenticationCommands.completeDesktopHandoff({
				code: handoffCode,
				token,
				userId,
				returnMethod,
				target,
			});
			if (operationRevision !== operationRevisionRef.current) return;
			setReturnUrl(completedReturnUrl);
			setMode(DesktopHandoffMode.DONE);
			if (completedReturnUrl != null) {
				window.location.assign(completedReturnUrl);
			}
		} catch (e) {
			if (operationRevision !== operationRevisionRef.current) return;
			failWith(
				e && typeof e === 'object' && 'body' in e
					? FormUtils.extractErrorMessage(i18n, e)
					: i18n._(COULDN_T_COMPLETE_SIGN_IN_PLEASE_TRY_AGAIN_DESCRIPTOR),
			);
		}
	}, [failWith, handoffCode, i18n, returnMethod]);
	const deny = useCallback(() => {
		const target = targetRef.current;
		const code = handoffCode;
		operationRevisionRef.current += 1;
		setClientInfo(null);
		setMode(DesktopHandoffMode.DENIED);
		if (code == null || target == null) return;
		AuthenticationCommands.denyDesktopHandoff({code, target}).catch((caught) => {
			logger.warn('Failed to tell the server that the sign-in request was declined', caught);
		});
	}, [handoffCode]);
	const reopenApp = useCallback(() => {
		if (returnUrl != null) {
			window.location.assign(returnUrl);
		}
	}, [returnUrl]);
	const switchToLogin = useCallback(() => {
		operationRevisionRef.current += 1;
		setMode(DesktopHandoffMode.LOGIN);
		setError(null);
	}, []);
	const retry = useCallback(() => {
		operationRevisionRef.current += 1;
		setError(null);
		setHandoffCode(null);
		setClientInfo(null);
		setReturnMethod(null);
		setReturnUrl(null);
		setAccount(null);
		tokenRef.current = null;
		userIdRef.current = null;
		targetRef.current = null;
		setMode(hasStoredAccounts ? DesktopHandoffMode.SELECTING : DesktopHandoffMode.LOGIN);
	}, [hasStoredAccounts]);
	const isWrongAccount =
		request.identifier != null && account != null && !matchesIdentifier(request.identifier, account);
	return {
		mode,
		error,
		clientInfo,
		handoffCode,
		codeSource,
		returnMethod,
		returnUrl,
		account,
		requestedIdentifier: isWrongAccount ? request.identifier : null,
		start,
		submitCode,
		approve,
		deny,
		reopenApp,
		switchToLogin,
		retry,
	};
}

export type DesktopHandoffFlow = ReturnType<typeof useDesktopHandoffFlow>;
