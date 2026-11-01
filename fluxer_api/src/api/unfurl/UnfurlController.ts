// SPDX-License-Identifier: AGPL-3.0-or-later

import {signAttachmentUrl} from '@app/api/attachment/AttachmentUrls';
import {DefaultUserOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {getUnfurlerService} from '@app/api/middleware/ServiceSingletons';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import type {
	EmbedMediaResponse,
	MessageEmbedChildResponse,
	MessageEmbedResponse,
} from '@fluxer/schema/src/domains/message/EmbedSchemas';
import {UnfurlRequest, UnfurlResponse} from '@fluxer/schema/src/domains/unfurl/UnfurlSchemas';

function signIfSet<T extends string | null | undefined>(url: T): T | string {
	return url ? signAttachmentUrl(url) : url;
}

function signMedia(media: EmbedMediaResponse | null | undefined) {
	return media && {...media, url: signAttachmentUrl(media.url), proxy_url: signIfSet(media.proxy_url)};
}

function signEmbedUrls(embed: MessageEmbedChildResponse): MessageEmbedChildResponse {
	const {author, provider, footer} = embed;
	return {
		...embed,
		url: signIfSet(embed.url),
		author: author && {
			...author,
			url: signIfSet(author.url),
			icon_url: signIfSet(author.icon_url),
			proxy_icon_url: signIfSet(author.proxy_icon_url),
		},
		provider: provider && {...provider, url: signIfSet(provider.url)},
		footer: footer && {
			...footer,
			icon_url: signIfSet(footer.icon_url),
			proxy_icon_url: signIfSet(footer.proxy_icon_url),
		},
		image: signMedia(embed.image),
		thumbnail: signMedia(embed.thumbnail),
		video: signMedia(embed.video),
		audio: signMedia(embed.audio),
	};
}

function signEmbedTree(embed: MessageEmbedResponse): MessageEmbedResponse {
	return {...signEmbedUrls(embed), children: embed.children?.map(signEmbedUrls)};
}

export function UnfurlController(app: HonoApp) {
	app.post(
		'/unfurl',
		RateLimitMiddleware(RateLimitConfigs.UNFURL_DEBUG),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'debug_unfurl',
			summary: 'Debug URL unfurl',
			responseSchema: UnfurlResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Debug'],
			description: 'Resolves a single URL through the unfurler without reading from the unfurl cache.',
		}),
		Validator('json', UnfurlRequest),
		async (ctx) => {
			const {url} = ctx.req.valid('json');
			const result = await getUnfurlerService().unfurlWithCachePolicy(url, 'block', {bypassCache: true});
			return ctx.json(result.embeds.map(signEmbedTree));
		},
	);
}
