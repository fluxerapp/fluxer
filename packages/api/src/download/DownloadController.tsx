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

import {DESKTOP_REDIRECT_PREFIX, DOWNLOAD_PREFIX} from '@fluxer/api/src/download/DownloadService';
import {OpenAPI} from '@fluxer/api/src/middleware/ResponseTypeMiddleware';
import type {HonoEnv} from '@fluxer/api/src/types/HonoEnv';
import {Validator} from '@fluxer/api/src/Validator';
import {
	DesktopRedirectParam,
	DesktopVersionedRedirectParam,
	DesktopVersionsParam,
	DesktopVersionsQuery,
	DesktopVersionsResponse,
	VersionInfoResponse,
} from '@fluxer/schema/src/domains/download/DownloadSchemas';
import type {Hono} from 'hono';

export function DownloadController(routes: Hono<HonoEnv>): void {
	routes.get(
		`${DESKTOP_REDIRECT_PREFIX}/:channel/:plat/:arch/latest`,
		Validator('param', DesktopVersionsParam),
		OpenAPI({
			operationId: 'get_latest_desktop_version',
			summary: 'Get latest desktop version',
			responseSchema: VersionInfoResponse,
			statusCode: 200,
			security: [],
			tags: ['Downloads'],
			description:
				'Returns metadata for the latest desktop version including download URLs and SHA-256 checksums for all available formats.',
		}),
		async (ctx) => {
			const {channel, plat, arch} = ctx.req.valid('param');
			const result = await ctx.get('downloadService').getLatestDesktopVersion({
				channel,
				plat,
				arch,
				host: ctx.req.header('host') ?? '',
				forwardedProto: ctx.req.header('x-forwarded-proto') ?? '',
				requestUrl: ctx.req.url,
			});
			if (!result) {
				return ctx.text('Not Found', 404);
			}
			return ctx.json(result, 200, {
				'Cache-Control': 'public, max-age=300',
			});
		},
	);

	routes.get(
		`${DESKTOP_REDIRECT_PREFIX}/:channel/:plat/:arch/latest/:format`,
		Validator('param', DesktopRedirectParam),
		OpenAPI({
			operationId: 'redirect_latest_desktop_version',
			summary: 'Redirect to latest desktop version',
			responseSchema: null,
			statusCode: 302,
			security: [],
			tags: ['Downloads'],
			description:
				'Redirects to the latest available desktop application version for the specified platform and architecture.',
		}),
		async (ctx) => {
			const {channel, plat, arch, format} = ctx.req.valid('param');
			const dest = await ctx.get('downloadService').resolveLatestDesktopRedirect({
				channel,
				plat,
				arch,
				format,
				host: ctx.req.header('host') ?? '',
				forwardedProto: ctx.req.header('x-forwarded-proto') ?? '',
				requestUrl: ctx.req.url,
			});
			if (!dest) {
				return ctx.text('Not Found', 404);
			}
			const res = ctx.redirect(dest, 302);
			res.headers.set('Cache-Control', 'no-store');
			return res;
		},
	);

	routes.get(
		`${DESKTOP_REDIRECT_PREFIX}/:channel/:plat/:arch/versions`,
		Validator('param', DesktopVersionsParam),
		Validator('query', DesktopVersionsQuery),
		OpenAPI({
			operationId: 'list_desktop_versions',
			summary: 'List desktop versions',
			responseSchema: DesktopVersionsResponse,
			statusCode: 200,
			security: [],
			tags: ['Downloads'],
			description: 'Lists available desktop versions with pagination for the specified platform and architecture.',
		}),
		async (ctx) => {
			const {channel, plat, arch} = ctx.req.valid('param');
			const {limit, before, after} = ctx.req.valid('query');
			const {versions, hasMore} = await ctx.get('downloadService').listDesktopVersions({
				channel,
				plat,
				arch,
				limit,
				before,
				after,
				host: ctx.req.header('host') ?? '',
				forwardedProto: ctx.req.header('x-forwarded-proto') ?? '',
				requestUrl: ctx.req.url,
			});
			return ctx.json({versions, has_more: hasMore}, 200, {
				'Cache-Control': 'public, max-age=300',
			});
		},
	);

	routes.get(
		`${DESKTOP_REDIRECT_PREFIX}/:channel/:plat/:arch/:version/:format`,
		Validator('param', DesktopVersionedRedirectParam),
		OpenAPI({
			operationId: 'redirect_desktop_version',
			summary: 'Redirect to desktop version',
			responseSchema: null,
			statusCode: 302,
			security: [],
			tags: ['Downloads'],
			description: 'Redirects to a specific desktop application version for the given platform and architecture.',
		}),
		async (ctx) => {
			const {channel, plat, arch, version, format} = ctx.req.valid('param');
			const dest = await ctx.get('downloadService').resolveVersionedDesktopRedirect({
				channel,
				plat,
				arch,
				version,
				format,
				host: ctx.req.header('host') ?? '',
				forwardedProto: ctx.req.header('x-forwarded-proto') ?? '',
				requestUrl: ctx.req.url,
			});
			if (!dest) {
				return ctx.text('Not Found', 404);
			}
			const res = ctx.redirect(dest, 302);
			res.headers.set('Cache-Control', 'public, max-age=86400');
			return res;
		},
	);

	routes.get(
		`${DOWNLOAD_PREFIX}/*`,
		OpenAPI({
			operationId: 'download_file',
			summary: 'Download file',
			responseSchema: null,
			statusCode: 302,
			security: [],
			tags: ['Downloads'],
			description: 'Redirects to a presigned URL for the requested file.',
		}),
		async (ctx) => {
			const url = await ctx.get('downloadService').resolveDownloadRedirect({
				path: ctx.req.path,
			});
			if (!url) {
				return ctx.text('Not Found', 404);
			}
			const res = ctx.redirect(url, 302);
			res.headers.set('Cache-Control', 'public, max-age=300');
			return res;
		},
	);
}
