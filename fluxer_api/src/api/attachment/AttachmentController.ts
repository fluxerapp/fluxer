// SPDX-License-Identifier: AGPL-3.0-or-later

import {signAttachmentUrl} from '@app/api/attachment/AttachmentUrls';
import {LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {
	RefreshAttachmentUrlsRequest,
	RefreshAttachmentUrlsResponse,
} from '@fluxer/schema/src/domains/message/AttachmentSchemas';

export function AttachmentController(app: HonoApp) {
	app.post(
		'/attachments/refresh-urls',
		RateLimitMiddleware(RateLimitConfigs.ATTACHMENT_URLS_REFRESH),
		LoginRequired,
		Validator('json', RefreshAttachmentUrlsRequest),
		OpenAPI({
			operationId: 'refresh_attachment_urls',
			summary: 'Refresh attachment URLs',
			responseSchema: RefreshAttachmentUrlsResponse,
			statusCode: 200,
			security: ['botToken', 'sessionToken'],
			tags: ['Messages'],
			description:
				'Reissues the expiring signature on attachment URLs. Returns one entry per requested URL, in the order they were requested, each pairing the URL exactly as it was sent with a freshly signed copy. A URL that is not an attachment URL of this instance is returned unchanged. No membership or existence check is performed.',
		}),
		async (ctx) => {
			const urls = ctx.req.valid('json').attachment_urls;
			return ctx.json({
				refreshed_urls: urls.map((original) => ({original, refreshed: signAttachmentUrl(original)})),
			});
		},
	);
}
