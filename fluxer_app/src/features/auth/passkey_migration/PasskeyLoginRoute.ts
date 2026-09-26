// SPDX-License-Identifier: AGPL-3.0-or-later

import {getProtectedLocalStorage} from '@app/features/platform/state/ProtectedWebStorage';

const PASSKEY_LOGIN_ROUTE_KEY = 'fluxer:passkey-login-route';

export type PasskeyLoginRoute = 'legacy' | 'native';

export function readPasskeyLoginRoute(): PasskeyLoginRoute {
	try {
		return getProtectedLocalStorage()?.getItem(PASSKEY_LOGIN_ROUTE_KEY) === 'legacy' ? 'legacy' : 'native';
	} catch {
		return 'native';
	}
}

export function writePasskeyLoginRoute(route: PasskeyLoginRoute): void {
	try {
		if (route === 'legacy') {
			getProtectedLocalStorage()?.setItem(PASSKEY_LOGIN_ROUTE_KEY, route);
		} else {
			getProtectedLocalStorage()?.removeItem(PASSKEY_LOGIN_ROUTE_KEY);
		}
	} catch {}
}
