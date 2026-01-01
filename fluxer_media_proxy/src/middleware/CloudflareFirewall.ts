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
import {HTTPException} from 'hono/http-exception';
import {Logger} from '~/Logger';
import type {CloudflareIPService} from '~/lib/CloudflareIPService';
import type {HonoEnv} from '~/lib/MediaTypes';

interface CloudflareFirewallOptions {
	enabled: boolean;
	exemptPaths?: Array<string>;
}

export const createCloudflareFirewall = (
	ipService: CloudflareIPService,
	{enabled, exemptPaths = ['/_health', '/_metadata']}: CloudflareFirewallOptions,
) =>
	createMiddleware<HonoEnv>(async (ctx, next) => {
		if (!enabled) {
			await next();
			return;
		}

		const path = ctx.req.path;
		if (exemptPaths.some((prefix) => path === prefix || path.startsWith(prefix))) {
			await next();
			return;
		}

		const xff = ctx.req.header('x-forwarded-for');
		if (!xff) {
			Logger.warn({path}, 'Rejected request without X-Forwarded-For header');
			throw new HTTPException(403, {message: 'Forbidden'});
		}
		const connectingIP = xff.split(',')[0]?.trim();
		if (!connectingIP || !ipService.isFromCloudflare(connectingIP)) {
			Logger.warn({connectingIP, path}, 'Rejected request from non-Cloudflare IP');
			throw new HTTPException(403, {message: 'Forbidden'});
		}

		await next();
	});
