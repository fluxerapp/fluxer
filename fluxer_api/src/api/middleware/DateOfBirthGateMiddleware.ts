// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HonoEnv} from '@app/api/types/HonoEnv';
import {isDateOfBirthEnforced} from '@app/api/user/DateOfBirth';
import {stripApiPrefix} from '@app/api/utils/RequestPathUtils';
import {DateOfBirthRequiredError} from '@fluxer/errors/src/domains/auth/DateOfBirthRequiredError';
import {createMiddleware} from 'hono/factory';

const DATE_OF_BIRTH_EXEMPT_PREFIXES: ReadonlyArray<string> = ['/auth/', '/admin/', '/test/', '/users/@me/sudo/'];

export const DATE_OF_BIRTH_ALLOWLIST: ReadonlyArray<{method: string; path: RegExp}> = [
	{method: 'GET', path: /^\/users\/@me$/},
	{method: 'PUT', path: /^\/users\/@me\/date-of-birth$/},
	{method: 'GET', path: /^\/users\/@me\/settings$/},
	{method: 'POST', path: /^\/users\/@me\/delete$/},
	{method: 'POST', path: /^\/users\/@me\/disable$/},
	{method: 'POST', path: /^\/users\/@me\/terms-acceptance$/},
	{method: 'GET', path: /^\/users\/@me\/harvest(\/[^/]+)*$/},
	{method: 'POST', path: /^\/users\/@me\/harvest(\/[^/]+)*$/},
	{method: 'GET', path: /^\/experiments$/},
	{method: 'GET', path: /^\/gateway\/bot$/},
];

export function isDateOfBirthAllowedRoute(method: string, path: string): boolean {
	if (DATE_OF_BIRTH_EXEMPT_PREFIXES.some((prefix) => path.startsWith(prefix))) return true;
	return DATE_OF_BIRTH_ALLOWLIST.some((entry) => entry.method === method && entry.path.test(path));
}

export const DateOfBirthGateMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	const user = ctx.get('user');
	if (
		user &&
		ctx.get('authTokenType') !== 'bot' &&
		ctx.get('authTokenType') !== 'admin_api_key' &&
		isDateOfBirthEnforced(user) &&
		!isDateOfBirthAllowedRoute(ctx.req.method, stripApiPrefix(ctx.req.path))
	) {
		throw new DateOfBirthRequiredError();
	}
	await next();
});
