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

import type {Context} from 'hono';
import {getCookie, setCookie} from 'hono/cookie';
import type {HonoEnv} from '~/App';
import {Config} from '~/Config';

const SUDO_COOKIE_PREFIX = '__flx_sudo';

const SUDO_COOKIE_MAX_AGE = 5 * 60;

function getCookieDomain(): string {
	const domain = Config.cookie.domain;
	if (domain) {
		return domain;
	}

	try {
		const url = new URL(Config.endpoints.webApp);
		const hostname = url.hostname;
		const parts = hostname.split('.');

		if (parts.length >= 2) {
			return `.${parts.slice(-2).join('.')}`;
		} else {
			return hostname;
		}
	} catch {
		return '';
	}
}

function getSudoCookieOptions() {
	return {
		httpOnly: true,
		secure: Config.cookie.secure,
		sameSite: 'Lax' as const,
		domain: getCookieDomain(),
		path: '/',
		maxAge: SUDO_COOKIE_MAX_AGE,
	};
}

function sudoCookieName(userId?: string | number): string {
	if (userId === undefined || userId === null) {
		return SUDO_COOKIE_PREFIX;
	}
	return `${SUDO_COOKIE_PREFIX}_${userId}`;
}

export function setSudoCookie(ctx: Context<HonoEnv>, token: string, userId?: string | number): void {
	const cookieName = sudoCookieName(userId);
	const options = getSudoCookieOptions();

	setCookie(ctx, cookieName, token, options);
}

export function getSudoCookie(ctx: Context<HonoEnv>, userId?: string | number): string | undefined {
	const cookieName = sudoCookieName(userId);
	return getCookie(ctx, cookieName);
}
