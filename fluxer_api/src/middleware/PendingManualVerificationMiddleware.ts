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

import {createMiddleware} from 'hono/factory';
import type {HonoEnv} from '~/App';
import {UserFlags} from '~/Constants';
import {MissingAccessError} from '~/Errors';

const ALLOWED_EXACT_PATHS = new Set(['/instance', '/_health']);
const ALLOWED_PREFIXES = ['/auth'];

function stripV1Prefix(path: string): string {
	if (path.startsWith('/v1/') || path === '/v1') {
		return path === '/v1' ? '/' : path.slice(3);
	}
	return path;
}

function normalizePath(path: string): string {
	if (path.length > 1 && path.endsWith('/')) {
		return path.slice(0, -1);
	}
	return path;
}

export const PendingManualVerificationMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	const user = ctx.get('user');
	if (!user || (user.flags & UserFlags.PENDING_MANUAL_VERIFICATION) === 0n) {
		return next();
	}

	const rawPath = stripV1Prefix(ctx.req.path);
	const normalizedPath = normalizePath(rawPath);

	if (ALLOWED_EXACT_PATHS.has(normalizedPath)) {
		return next();
	}

	if (ALLOWED_PREFIXES.some((prefix) => normalizedPath === prefix || normalizedPath.startsWith(`${prefix}/`))) {
		return next();
	}

	throw new MissingAccessError();
});
