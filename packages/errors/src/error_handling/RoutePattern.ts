// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Context} from 'hono';
import {matchedRoutes} from 'hono/route';
import {METHOD_NAME_ALL} from 'hono/router';

export function resolveRoutePattern(ctx: Context): string {
	const routes = matchedRoutes(ctx);
	const endpoint = routes.findLast((route) => route.method !== METHOD_NAME_ALL);
	return (endpoint ?? routes.at(-1))?.path ?? '*';
}
