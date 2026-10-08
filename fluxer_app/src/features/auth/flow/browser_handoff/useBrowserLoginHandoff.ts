// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {
	DESKTOP_HANDOFF_DENIED_DESCRIPTOR,
	DESKTOP_HANDOFF_EXPIRED_DESCRIPTOR,
	DESKTOP_HANDOFF_UNAVAILABLE_DESCRIPTOR,
} from '@app/features/auth/flow/browser_handoff/BrowserHandoffDescriptors';
import type {
	BrowserLoginHandoffSession,
	BrowserLoginHandoffTarget,
} from '@app/features/auth/flow/browser_handoff/BrowserLoginHandoffTransport';
import {resolveBrowserLoginHandoffTransport} from '@app/features/auth/flow/browser_handoff/BrowserLoginHandoffTransport';
import {getAuthErrorMessage} from '@app/features/auth/hooks/useAuthForm';
import type {LoginSuccessPayload} from '@app/features/auth/state/AuthFlow';
import {type LoginIdentifierField, loginIdentifierField} from '@app/features/auth/utils/AccountSignInIdentifier';
import {instanceTargetFromSnapshot} from '@app/features/platform/transport/InstanceHTTP';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import {navigateToExternalURL} from '@app/features/ui/utils/NativeUtils';
import {DesktopHandoffReturnMethod, DesktopHandoffStatus} from '@fluxer/desktop_ipc/src/BrowserHandoffContract';
import {formatDesktopHandoffCode} from '@fluxer/schema/src/domains/auth/DesktopHandoffCode';
import {useLingui} from '@lingui/react/macro';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const BROWSER_LOGIN_HANDOFF_POLL_INTERVAL_MS = 2000;
const BROWSER_LOGIN_HANDOFF_COPY_FEEDBACK_MS = 2000;
const BROWSER_LOGIN_HANDOFF_COUNTDOWN_INTERVAL_MS = 500;
const MAX_HANDOFF_POLL_ERRORS = 3;

const logger = new Logger('useBrowserLoginHandoff');

export const BrowserLoginHandoffAction = Object.freeze({
	OPEN_BROWSER: 'openBrowser',
	SHOW_CODE: 'showCode',
} as const);

export type BrowserLoginHandoffAction = (typeof BrowserLoginHandoffAction)[keyof typeof BrowserLoginHandoffAction];
export type BrowserLoginHandoffPendingAction = BrowserLoginHandoffAction | null;

export interface UseBrowserLoginHandoffOptions {
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
	readonly prefillIdentifier?: string | null;
	readonly onEnded: () => void;
	readonly onSuccess: (payload: LoginSuccessPayload) => Promise<void> | void;
}

export interface BrowserLoginHandoffController {
	readonly canStart: boolean;
	readonly code: string | null;
	readonly copied: boolean;
	readonly displayCode: string;
	readonly error: string | null;
	readonly hasCode: boolean;
	readonly hasStoppedWaiting: boolean;
	readonly isExpired: boolean;
	readonly isGenerating: boolean;
	readonly pendingAction: BrowserLoginHandoffPendingAction;
	readonly remainingSeconds: number | null;
	readonly returnMethod: DesktopHandoffReturnMethod;
	readonly session: BrowserLoginHandoffSession | null;
	readonly copyCode: () => void;
	readonly openBrowser: () => Promise<boolean>;
	readonly regenerateCode: () => Promise<boolean>;
	readonly reset: () => void;
	readonly resumeWaiting: () => void;
	readonly showManualCode: () => Promise<boolean>;
}

class BrowserLoginHandoffUnavailableError extends Error {
	public constructor(message: string) {
		super(message);
		this.name = 'BrowserLoginHandoffUnavailableError';
	}
}

function resolveHandoffTarget(runtimeSnapshot: RuntimeConfigSnapshot): BrowserLoginHandoffTarget {
	const webAppEndpoint = runtimeSnapshot.webAppEndpoint.replace(/\/+$/u, '');
	return {webAppEndpoint, instance: instanceTargetFromSnapshot(runtimeSnapshot)};
}

interface BrowserLoginHandoffURLRequest {
	readonly session: BrowserLoginHandoffSession;
	readonly prefillIdentifier: string | null | undefined;
	readonly prefillField: LoginIdentifierField;
}

function buildBrowserLoginHandoffURL({
	session,
	prefillIdentifier,
	prefillField,
}: BrowserLoginHandoffURLRequest): string {
	const params = new URLSearchParams({
		handoff: '1',
		code: session.code,
		api: new URL(session.target.instance.apiEndpoint).origin,
	});
	if (prefillIdentifier != null && prefillIdentifier.length > 0) {
		params.set(prefillField, prefillIdentifier);
	}
	return `${session.target.webAppEndpoint}/login?${params.toString()}`;
}

function useBrowserLoginHandoffCountdown(expiresAt: string | null): number | null {
	const [remaining, setRemaining] = useState<number | null>(null);
	useEffect(() => {
		if (expiresAt == null) {
			setRemaining(null);
			return;
		}
		let timeoutId: number | null = null;
		const expiresAtMs = new Date(expiresAt).getTime();
		const update = () => {
			timeoutId = null;
			const nextRemaining = Math.max(0, Math.ceil((expiresAtMs - Date.now()) / 1000));
			setRemaining(nextRemaining);
			if (nextRemaining > 0) {
				timeoutId = window.setTimeout(update, BROWSER_LOGIN_HANDOFF_COUNTDOWN_INTERVAL_MS);
			}
		};
		update();
		return () => {
			if (timeoutId != null) {
				window.clearTimeout(timeoutId);
			}
		};
	}, [expiresAt]);
	return remaining;
}

export function useBrowserLoginHandoff({
	runtimeSnapshot,
	prefillIdentifier,
	onEnded,
	onSuccess,
}: UseBrowserLoginHandoffOptions): BrowserLoginHandoffController {
	const {i18n} = useLingui();
	const [session, setSession] = useState<BrowserLoginHandoffSession | null>(null);
	const [isGenerating, setIsGenerating] = useState(false);
	const [pendingAction, setPendingAction] = useState<BrowserLoginHandoffPendingAction>(null);
	const [error, setError] = useState<string | null>(null);
	const [copied, setCopied] = useState(false);
	const [hasStoppedWaiting, setHasStoppedWaiting] = useState(false);
	const [waitGeneration, setWaitGeneration] = useState(0);
	const [transport] = useState(resolveBrowserLoginHandoffTransport);
	const generationRef = useRef(0);
	const completedRef = useRef(false);
	const copyResetRef = useRef<number | null>(null);
	const onSuccessRef = useRef(onSuccess);
	const onEndedRef = useRef(onEnded);
	const i18nRef = useRef(i18n);
	onSuccessRef.current = onSuccess;
	onEndedRef.current = onEnded;
	i18nRef.current = i18n;
	const target = resolveHandoffTarget(runtimeSnapshot);
	const targetKey = `${target.instance.apiEndpoint}|${target.webAppEndpoint}`;
	const targetRef = useRef(target);
	targetRef.current = target;
	const code = session?.code ?? null;
	const expiresAt = session?.expiresAt ?? null;
	const remainingSeconds = useBrowserLoginHandoffCountdown(expiresAt);
	const displayCode = useMemo(() => (code == null ? '' : formatDesktopHandoffCode(code)), [code]);
	const clearCopyFeedbackTimer = useCallback(() => {
		if (copyResetRef.current != null) {
			window.clearTimeout(copyResetRef.current);
			copyResetRef.current = null;
		}
	}, []);
	const clearHandoffSession = useCallback(
		(errorMessage: string | null) => {
			generationRef.current += 1;
			completedRef.current = false;
			clearCopyFeedbackTimer();
			setSession(null);
			setIsGenerating(false);
			setPendingAction(null);
			setCopied(false);
			setHasStoppedWaiting(false);
			setError(errorMessage);
		},
		[clearCopyFeedbackTimer],
	);
	const reset = useCallback(() => clearHandoffSession(null), [clearHandoffSession]);
	useEffect(() => {
		reset();
	}, [reset, targetKey]);
	useEffect(() => () => clearCopyFeedbackTimer(), [clearCopyFeedbackTimer]);
	const generateSession = useCallback(async (): Promise<BrowserLoginHandoffSession | null> => {
		const generation = ++generationRef.current;
		completedRef.current = false;
		clearCopyFeedbackTimer();
		setIsGenerating(true);
		setError(null);
		setSession(null);
		setCopied(false);
		setHasStoppedWaiting(false);
		const requestTarget = targetRef.current;
		try {
			if (requestTarget.webAppEndpoint.length === 0) {
				throw new BrowserLoginHandoffUnavailableError(i18n._(DESKTOP_HANDOFF_UNAVAILABLE_DESCRIPTOR));
			}
			const nextSession = await transport.initiate(requestTarget);
			if (generation !== generationRef.current) {
				return null;
			}
			setSession(nextSession);
			return nextSession;
		} catch (caught) {
			if (generation === generationRef.current) {
				setError(getAuthErrorMessage(caught, i18n));
			}
			return null;
		} finally {
			if (generation === generationRef.current) {
				setIsGenerating(false);
			}
		}
	}, [clearCopyFeedbackTimer, i18n, transport]);
	const getCurrentSession = useCallback(async (): Promise<BrowserLoginHandoffSession | null> => {
		if (session != null && Date.parse(session.expiresAt) > Date.now()) {
			return session;
		}
		return generateSession();
	}, [generateSession, session]);
	const expireCurrentSession = useCallback(() => {
		clearHandoffSession(i18n._(DESKTOP_HANDOFF_EXPIRED_DESCRIPTOR));
		onEndedRef.current();
	}, [clearHandoffSession, i18n]);
	const expireCurrentSessionRef = useRef(expireCurrentSession);
	expireCurrentSessionRef.current = expireCurrentSession;
	const endDeniedSession = useCallback(() => {
		clearHandoffSession(i18n._(DESKTOP_HANDOFF_DENIED_DESCRIPTOR));
		onEndedRef.current();
	}, [clearHandoffSession, i18n]);
	const endDeniedSessionRef = useRef(endDeniedSession);
	endDeniedSessionRef.current = endDeniedSession;
	const openBrowser = useCallback(async (): Promise<boolean> => {
		setPendingAction(BrowserLoginHandoffAction.OPEN_BROWSER);
		try {
			const currentSession = await getCurrentSession();
			if (currentSession == null) {
				return false;
			}
			setError(null);
			await navigateToExternalURL(
				buildBrowserLoginHandoffURL({
					session: currentSession,
					prefillIdentifier,
					prefillField: loginIdentifierField(runtimeSnapshot),
				}),
			);
			return true;
		} catch (caught) {
			setError(getAuthErrorMessage(caught, i18n));
			return false;
		} finally {
			setPendingAction(null);
		}
	}, [getCurrentSession, i18n, prefillIdentifier, runtimeSnapshot]);
	const showManualCode = useCallback(async (): Promise<boolean> => {
		setPendingAction(BrowserLoginHandoffAction.SHOW_CODE);
		try {
			return (await getCurrentSession()) != null;
		} finally {
			setPendingAction(null);
		}
	}, [getCurrentSession]);
	const regenerateCode = useCallback(async (): Promise<boolean> => {
		setPendingAction(BrowserLoginHandoffAction.SHOW_CODE);
		try {
			return (await generateSession()) != null;
		} finally {
			setPendingAction(null);
		}
	}, [generateSession]);
	const resumeWaiting = useCallback(() => {
		setHasStoppedWaiting(false);
		setError(null);
		setWaitGeneration((current) => current + 1);
	}, []);
	const copyCode = useCallback(() => {
		if (code == null) {
			return;
		}
		TextCopyCommands.copy(i18n, code)
			.then((succeeded) => {
				if (!succeeded) {
					return;
				}
				setCopied(true);
				clearCopyFeedbackTimer();
				copyResetRef.current = window.setTimeout(() => {
					setCopied(false);
					copyResetRef.current = null;
				}, BROWSER_LOGIN_HANDOFF_COPY_FEEDBACK_MS);
			})
			.catch((caught) => {
				logger.warn('Failed to copy the browser sign-in code', caught);
			});
	}, [clearCopyFeedbackTimer, code, i18n]);
	useEffect(() => {
		if (session == null || completedRef.current) {
			return;
		}
		let disposed = false;
		let keepPolling = true;
		let consecutiveErrors = 0;
		let pollTimeoutId: number | null = null;
		const schedulePoll = () => {
			if (disposed || !keepPolling || completedRef.current || pollTimeoutId != null) {
				return;
			}
			pollTimeoutId = window.setTimeout(() => {
				pollTimeoutId = null;
				runPoll();
			}, BROWSER_LOGIN_HANDOFF_POLL_INTERVAL_MS);
		};
		const poll = async () => {
			if (disposed || completedRef.current) {
				return;
			}
			if (Date.parse(session.expiresAt) <= Date.now()) {
				keepPolling = false;
				expireCurrentSessionRef.current();
				return;
			}
			try {
				const result = await transport.status(session);
				consecutiveErrors = 0;
				if (result.status === DesktopHandoffStatus.COMPLETED && result.token != null && result.userId != null) {
					keepPolling = false;
					completedRef.current = true;
					try {
						await onSuccessRef.current({
							token: result.token,
							userId: result.userId,
							...(result.userData ? {userData: result.userData} : {}),
						});
					} catch (caught) {
						completedRef.current = false;
						if (!disposed) {
							setError(getAuthErrorMessage(caught, i18nRef.current));
							setHasStoppedWaiting(true);
						}
					}
					return;
				}
				if (disposed) {
					return;
				}
				if (result.status === DesktopHandoffStatus.EXPIRED) {
					keepPolling = false;
					expireCurrentSessionRef.current();
				}
				if (result.status === DesktopHandoffStatus.DENIED) {
					keepPolling = false;
					endDeniedSessionRef.current();
				}
			} catch (caught) {
				consecutiveErrors += 1;
				if (!disposed) {
					setError(getAuthErrorMessage(caught, i18nRef.current));
				}
				if (consecutiveErrors >= MAX_HANDOFF_POLL_ERRORS) {
					keepPolling = false;
					if (!disposed) {
						setHasStoppedWaiting(true);
					}
				}
			} finally {
				schedulePoll();
			}
		};
		const runPoll = () => {
			poll().catch((caught) => {
				logger.error('Browser sign-in polling failed unexpectedly', caught);
			});
		};
		schedulePoll();
		return () => {
			disposed = true;
			if (pollTimeoutId != null) {
				window.clearTimeout(pollTimeoutId);
			}
		};
	}, [session, transport, waitGeneration]);
	return {
		canStart: target.webAppEndpoint.length > 0,
		code,
		copied,
		displayCode,
		error,
		hasCode: code != null,
		hasStoppedWaiting,
		isExpired: remainingSeconds != null && remainingSeconds <= 0,
		isGenerating,
		pendingAction,
		remainingSeconds,
		returnMethod: session?.returnMethod ?? DesktopHandoffReturnMethod.CODE,
		session,
		copyCode,
		openBrowser,
		regenerateCode,
		reset,
		resumeWaiting,
		showManualCode,
	};
}
