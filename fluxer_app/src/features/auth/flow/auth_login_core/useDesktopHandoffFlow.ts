// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import type {DesktopHandoffInfoResponse} from '@app/features/auth/commands/AuthenticationCommands';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {DesktopHandoffMode} from '@app/features/auth/flow/auth_login_core/AuthLoginStepTypes';
import {type InstanceHTTPTarget, instanceTargetFromSnapshot} from '@app/features/platform/transport/InstanceHTTP';
import * as FormUtils from '@app/lib/forms';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useCallback, useMemo, useRef, useState} from 'react';

const INVALID_OR_EXPIRED_CODE_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: 'Invalid or expired code. Try again.',
	comment: 'Desktop handoff flow error shown when the entered pairing code is invalid or expired.',
});
const COULDN_T_VERIFY_THAT_CODE_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: "Couldn't verify that code. Try again.",
	comment: 'Desktop handoff flow error shown when verifying the pairing code fails for an unexpected reason.',
});
const COULDN_T_COMPLETE_SIGN_IN_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: "Couldn't complete sign-in. Try again.",
	comment: 'Desktop handoff flow error shown when finalizing the sign-in fails after a valid code.',
});

export function isHandoffRequest(params: URLSearchParams): boolean {
	return params.get('handoff') === '1' || params.get('desktop_handoff') === '1';
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
}

export function useDesktopHandoffFlow({enabled, hasStoredAccounts, initialMode}: Options) {
	const {i18n} = useLingui();
	const derivedInitial = useMemo<DesktopHandoffMode>(() => {
		if (!enabled) return DesktopHandoffMode.IDLE;
		if (initialMode) return initialMode;
		return hasStoredAccounts ? DesktopHandoffMode.SELECTING : DesktopHandoffMode.LOGIN;
	}, [enabled, hasStoredAccounts, initialMode]);
	const [mode, setMode] = useState<DesktopHandoffMode>(derivedInitial);
	const [error, setError] = useState<string | null>(null);
	const [clientInfo, setClientInfo] = useState<DesktopHandoffInfoResponse['client_info']>(null);
	const [handoffCode, setHandoffCode] = useState<string | null>(null);
	const tokenRef = useRef<string | null>(null);
	const userIdRef = useRef<string | null>(null);
	const operationRevisionRef = useRef(0);
	const targetRef = useRef<InstanceHTTPTarget | null>(null);
	const start = useCallback(
		({token, userId, runtimeSnapshot}: DesktopHandoffAccountCredentials) => {
			if (!enabled) return;
			operationRevisionRef.current += 1;
			tokenRef.current = token;
			userIdRef.current = userId;
			targetRef.current = instanceTargetFromSnapshot(runtimeSnapshot);
			setError(null);
			setHandoffCode(null);
			setClientInfo(null);
			setMode(DesktopHandoffMode.WARNING);
		},
		[enabled],
	);
	const proceedToCodeInput = useCallback(() => {
		operationRevisionRef.current += 1;
		setMode(DesktopHandoffMode.CODE_INPUT);
		setError(null);
	}, []);
	const submitCode = useCallback(
		async (code: string) => {
			const token = tokenRef.current;
			const target = targetRef.current;
			if (token == null || token.length === 0 || target == null) return;
			const operationRevision = ++operationRevisionRef.current;
			setMode(DesktopHandoffMode.FETCHING_INFO);
			setError(null);
			setHandoffCode(code);
			try {
				const info = await AuthenticationCommands.fetchDesktopHandoffInfo({code, token, target});
				if (operationRevision !== operationRevisionRef.current) return;
				if (info.status === 'expired') {
					setMode(DesktopHandoffMode.ERROR);
					setError(i18n._(INVALID_OR_EXPIRED_CODE_PLEASE_TRY_AGAIN_DESCRIPTOR));
					return;
				}
				setClientInfo(info.client_info);
				setMode(DesktopHandoffMode.APPROVING);
			} catch (e) {
				if (operationRevision !== operationRevisionRef.current) return;
				setMode(DesktopHandoffMode.ERROR);
				setError(
					e && typeof e === 'object' && 'body' in e
						? FormUtils.extractErrorMessage(i18n, e)
						: i18n._(COULDN_T_VERIFY_THAT_CODE_PLEASE_TRY_AGAIN_DESCRIPTOR),
				);
			}
		},
		[i18n],
	);
	const approve = useCallback(async () => {
		const token = tokenRef.current;
		const userId = userIdRef.current;
		const target = targetRef.current;
		if (!handoffCode || !token || !userId || target == null) return;
		const operationRevision = ++operationRevisionRef.current;
		setMode(DesktopHandoffMode.COMPLETING);
		setError(null);
		try {
			await AuthenticationCommands.completeDesktopHandoff({
				code: handoffCode,
				token,
				userId,
				target,
			});
			if (operationRevision !== operationRevisionRef.current) return;
			setMode(DesktopHandoffMode.DONE);
		} catch (e) {
			if (operationRevision !== operationRevisionRef.current) return;
			setMode(DesktopHandoffMode.ERROR);
			setError(
				e && typeof e === 'object' && 'body' in e
					? FormUtils.extractErrorMessage(i18n, e)
					: i18n._(COULDN_T_COMPLETE_SIGN_IN_PLEASE_TRY_AGAIN_DESCRIPTOR),
			);
		}
	}, [handoffCode, i18n]);
	const deny = useCallback(() => {
		operationRevisionRef.current += 1;
		setHandoffCode(null);
		setClientInfo(null);
		setMode(DesktopHandoffMode.CODE_INPUT);
	}, []);
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
		tokenRef.current = null;
		userIdRef.current = null;
		targetRef.current = null;
		setMode(hasStoredAccounts ? DesktopHandoffMode.SELECTING : DesktopHandoffMode.LOGIN);
	}, [hasStoredAccounts]);
	return {
		mode,
		error,
		clientInfo,
		handoffCode,
		start,
		proceedToCodeInput,
		submitCode,
		approve,
		deny,
		switchToLogin,
		retry,
	};
}
