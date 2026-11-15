// SPDX-License-Identifier: AGPL-3.0-or-later

import {getProtectedSessionStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {useEffect, useRef} from 'react';

const logger = new Logger('SsoAutoStart');

const SSO_AUTO_START_SPENT_KEY = 'fluxer.sso.auto_start_spent';

export const SSO_START_QUERY_PARAM = 'sso';

interface SsoAutoStartRequest {
	readonly allowed: boolean;
	readonly desktopApp: boolean;
	readonly desktopHandoff: boolean;
	readonly onSignInStep: boolean;
	readonly ssoAvailable: boolean;
	readonly ssoEnforced: boolean;
	readonly startRequested: boolean;
	readonly spent: boolean;
}

export function isSsoStartRequested(search: string): boolean {
	return new URLSearchParams(search).get(SSO_START_QUERY_PARAM) === '1';
}

export function withoutSsoStartRequest(pathWithSearch: string): string {
	const queryStart = pathWithSearch.indexOf('?');
	if (queryStart === -1) {
		return pathWithSearch;
	}
	const params = new URLSearchParams(pathWithSearch.slice(queryStart + 1));
	params.delete(SSO_START_QUERY_PARAM);
	const query = params.toString();
	const path = pathWithSearch.slice(0, queryStart);
	return query === '' ? path : `${path}?${query}`;
}

function shouldAutoStartSso(request: SsoAutoStartRequest): boolean {
	if (!request.allowed || request.desktopApp || !request.ssoAvailable || !request.onSignInStep) {
		return false;
	}
	if (request.startRequested) {
		return true;
	}
	return request.ssoEnforced && !request.desktopHandoff && !request.spent;
}

function isSsoAutoStartSpent(): boolean {
	try {
		const storage = getProtectedSessionStorage();
		return storage == null || storage.getItem(SSO_AUTO_START_SPENT_KEY) != null;
	} catch (error) {
		logger.warn('Failed to read the SSO auto-start state', error);
		return true;
	}
}

export function markSsoAutoStartSpent(): void {
	try {
		getProtectedSessionStorage()?.setItem(SSO_AUTO_START_SPENT_KEY, '1');
	} catch (error) {
		logger.warn('Failed to persist the SSO auto-start state', error);
	}
}

interface UseSsoAutoStartOptions {
	readonly allowed: boolean;
	readonly desktopApp: boolean;
	readonly desktopHandoff: boolean;
	readonly onSignInStep: (startRequested: boolean) => boolean;
	readonly ssoAvailable: boolean;
	readonly ssoEnforced: boolean;
	readonly pathname: string;
	readonly search: string;
	readonly start: () => Promise<void>;
}

export function useSsoAutoStart({
	allowed,
	desktopApp,
	desktopHandoff,
	onSignInStep,
	ssoAvailable,
	ssoEnforced,
	pathname,
	search,
	start,
}: UseSsoAutoStartOptions): void {
	const attemptedRef = useRef(false);
	useEffect(() => {
		if (attemptedRef.current) {
			return;
		}
		const startRequested = isSsoStartRequested(search);
		const shouldStart = shouldAutoStartSso({
			allowed,
			desktopApp,
			desktopHandoff,
			onSignInStep: onSignInStep(startRequested),
			ssoAvailable,
			ssoEnforced,
			startRequested,
			spent: isSsoAutoStartSpent(),
		});
		if (!shouldStart) {
			return;
		}
		attemptedRef.current = true;
		if (startRequested) {
			window.history.replaceState(window.history.state, '', withoutSsoStartRequest(`${pathname}${search}`));
		}
		void start();
	}, [allowed, desktopApp, desktopHandoff, onSignInStep, ssoAvailable, ssoEnforced, pathname, search, start]);
}
