// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {safeRedirectTarget} from '@app/features/auth/utils/SafeRedirect';

const SSO_SUDO_STATE_STORAGE_PREFIX = 'fluxer:sso:sudo:';

export const SSO_SUDO_COMPLETE_MESSAGE = 'fluxer:sso-sudo-complete';

interface PendingSsoSudoState {
	redirectTo?: string;
	createdAt: number;
}

function sudoStateStorageKey(state: string): string {
	return `${SSO_SUDO_STATE_STORAGE_PREFIX}${state}`;
}

function storePendingSsoSudoState(state: string, redirectTo?: string): void {
	try {
		const payload: PendingSsoSudoState = {createdAt: Date.now()};
		if (redirectTo) payload.redirectTo = redirectTo;
		window.localStorage.setItem(sudoStateStorageKey(state), JSON.stringify(payload));
	} catch {}
}

export function getPendingSsoSudoState(state: string): PendingSsoSudoState | null {
	try {
		const raw = window.localStorage.getItem(sudoStateStorageKey(state));
		if (!raw) return null;
		const parsed: unknown = JSON.parse(raw);
		if (!parsed || typeof parsed !== 'object') return null;
		const record = parsed as Record<string, unknown>;
		return {
			redirectTo: typeof record.redirectTo === 'string' ? record.redirectTo : undefined,
			createdAt: typeof record.createdAt === 'number' ? record.createdAt : 0,
		};
	} catch {
		return null;
	}
}

export function clearPendingSsoSudoState(state: string): void {
	try {
		window.localStorage.removeItem(sudoStateStorageKey(state));
	} catch {}
}

export async function startSsoSudo({
	redirectTo,
	redirectUri,
}: {
	redirectTo?: string;
	redirectUri?: string;
} = {}): Promise<{
	authorizationUrl: string;
	redirectUri: string;
	state: string;
}> {
	const safeRedirectTo = safeRedirectTarget(redirectTo);
	const result = await AuthenticationCommands.startSsoSudo({
		redirectTo: safeRedirectTo ?? undefined,
		redirectUri,
	});
	storePendingSsoSudoState(result.state, safeRedirectTo ?? undefined);
	return {authorizationUrl: result.authorization_url, redirectUri: result.redirect_uri, state: result.state};
}

export async function completeSsoSudo({code, state}: {code: string; state: string}): Promise<{sudoToken: string}> {
	const result = await AuthenticationCommands.completeSsoSudo({code, state});
	clearPendingSsoSudoState(state);
	return {sudoToken: result.sudo_token};
}
