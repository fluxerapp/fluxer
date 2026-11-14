// SPDX-License-Identifier: AGPL-3.0-or-later

import {Readable} from 'node:stream';
import {ADMIN_DOWNLOAD_PATH, verifyAdminDownloadToken} from '@app/api/admin/AdminSignedDownloads';
import {Config} from '@app/api/Config';
import {StorageObjectRangeNotSatisfiableError} from '@app/api/infrastructure/IStorageService';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {AdminSignedDownloadParam, AdminSignedDownloadResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {HarvestDownloadQuery} from '@fluxer/schema/src/domains/user/UserHarvestSchemas';

export function AdminDownloadController(app: HonoApp) {
	app.get(
		`${ADMIN_DOWNLOAD_PATH}/:filename`,
		RateLimitMiddleware(RateLimitConfigs.USER_HARVEST_DOWNLOAD_FILE),
		Validator('param', AdminSignedDownloadParam),
		Validator('query', HarvestDownloadQuery, (result, ctx) =>
			result.success ? undefined : ctx.text('Not Found', 404),
		),
		OpenAPI({
			operationId: 'download_admin_signed_file',
			summary: 'Download an admin archive or report file',
			responseSchema: AdminSignedDownloadResponse,
			responseContentType: 'application/octet-stream',
			statusCode: [200, 206],
			security: [],
			tags: ['Admin'],
			description:
				'Streams an admin archive or a file kept as report evidence. Authorised by a signed, expiring token that the admin API hands out in place of a storage link. Only active when presigned harvest downloads are disabled.',
		}),
		async (ctx) => {
			if (Config.presignedHarvestDownloadsEnabled) {
				return ctx.text('Not Found', 404);
			}
			const payload = verifyAdminDownloadToken(ctx.req.valid('query').token);
			if (!payload) {
				return ctx.text('Not Found', 404);
			}
			try {
				const object = await ctx.get('storageService').streamObject({
					bucket: Config.s3.buckets[payload.bucket],
					key: payload.key,
					range: ctx.req.header('range') ?? undefined,
				});
				if (!object) {
					return ctx.text('Not Found', 404);
				}
				const headers = new Headers();
				headers.set('Content-Type', object.contentType ?? 'application/octet-stream');
				headers.set(
					'Content-Disposition',
					`attachment; filename="${encodeURIComponent(ctx.req.valid('param').filename)}"`,
				);
				headers.set('Content-Security-Policy', "sandbox; default-src 'none'");
				headers.set('Content-Length', String(object.contentLength));
				headers.set('Cache-Control', 'private, no-store');
				headers.set('X-Content-Type-Options', 'nosniff');
				headers.set('Accept-Ranges', 'bytes');
				if (object.contentRange) {
					headers.set('Content-Range', object.contentRange);
				}
				return new Response(Readable.toWeb(object.body) as ReadableStream, {
					status: object.contentRange ? 206 : 200,
					headers,
				});
			} catch (error) {
				if (error instanceof StorageObjectRangeNotSatisfiableError) {
					return ctx.text('Range Not Satisfiable', 416);
				}
				throw error;
			}
		},
	);
}
