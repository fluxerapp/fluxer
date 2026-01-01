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

import '~/instrument';

import fs from 'node:fs/promises';
import {serve} from '@hono/node-server';
import * as Sentry from '@sentry/node';
import {Hono} from 'hono';
import {HTTPException} from 'hono/http-exception';
import {logger} from 'hono/logger';
import * as v from 'valibot';
import {Config} from '~/Config';
import {createAttachmentsHandler} from '~/controllers/AttachmentsController';
import {createExternalMediaHandler} from '~/controllers/ExternalMediaController';
import {
	createGuildMemberImageRouteHandler,
	createImageRouteHandler,
	createSimpleImageRouteHandler,
} from '~/controllers/ImageController';
import {handleMetadataRequest} from '~/controllers/MetadataController';
import {handleStaticProxyRequest} from '~/controllers/StaticProxyController';
import {createStickerRouteHandler} from '~/controllers/StickerController';
import {handleThemeRequest} from '~/controllers/ThemeController';
import {handleThumbnailRequest} from '~/controllers/ThumbnailController';
import {Logger} from '~/Logger';
import {CloudflareIPService} from '~/lib/CloudflareIPService';
import {InMemoryCoalescer} from '~/lib/InMemoryCoalescer';
import type {HonoEnv} from '~/lib/MediaTypes';
import {NSFWDetectionService} from '~/lib/NSFWDetectionService';
import {InternalNetworkRequired} from '~/middleware/AuthMiddleware';
import {createCloudflareFirewall} from '~/middleware/CloudflareFirewall';
import {metricsMiddleware} from '~/middleware/MetricsMiddleware';

const app = new Hono<HonoEnv>({strict: true});
app.use(logger(Logger.info.bind(Logger)));
app.use('*', metricsMiddleware);

const coalescer = new InMemoryCoalescer();
Logger.info('Initialized in-memory request coalescer');

const cloudflareIPService = new CloudflareIPService();

if (Config.REQUIRE_CLOUDFLARE) {
	await cloudflareIPService.initialize();
	Logger.info('Initialized Cloudflare IP allowlist');
} else {
	Logger.info('Cloudflare IP allowlist disabled');
}

const cloudflareFirewall = createCloudflareFirewall(cloudflareIPService, {
	enabled: Config.REQUIRE_CLOUDFLARE,
});

app.use('*', cloudflareFirewall);

process.on('SIGTERM', async () => {
	Logger.info('Received SIGTERM, shutting down gracefully');
	try {
		process.exit(0);
	} catch (error) {
		Logger.error({error}, 'Error during shutdown');
		process.exit(1);
	}
});

app.use('*', async (ctx, next) => {
	ctx.set('tempFiles', []);
	try {
		await next();
	} finally {
		const tempFiles = ctx.get('tempFiles');
		await Promise.all(
			tempFiles.map((file) => fs.unlink(file).catch(() => Logger.error(`Failed to delete temp file: ${file}`))),
		);
	}
});

app.get('/_health', (ctx) => ctx.text('OK'));

if (Config.STATIC_MODE) {
	Logger.info('Media proxy running in STATIC MODE - proxying all requests to the static bucket');

	app.all('*', handleStaticProxyRequest);
} else {
	const nsfwDetectionService = new NSFWDetectionService();
	await nsfwDetectionService.initialize();
	Logger.info('Initialized NSFW detection service');

	const handleImageRoute = createImageRouteHandler(coalescer);
	const handleSimpleImageRoute = createSimpleImageRouteHandler(coalescer);
	const handleGuildMemberImageRoute = createGuildMemberImageRouteHandler(coalescer);
	const handleStickerRoute = createStickerRouteHandler(coalescer);
	const processExternalMedia = createExternalMediaHandler(coalescer);
	const handleAttachmentsRoute = createAttachmentsHandler(coalescer);

	app.post('/_metadata', InternalNetworkRequired, handleMetadataRequest(coalescer, nsfwDetectionService));
	app.post('/_thumbnail', InternalNetworkRequired, handleThumbnailRequest);

	app.get('/avatars/:id/:filename', async (ctx) => handleImageRoute(ctx, 'avatars'));
	app.get('/icons/:id/:filename', async (ctx) => handleImageRoute(ctx, 'icons'));
	app.get('/banners/:id/:filename', async (ctx) => handleImageRoute(ctx, 'banners'));
	app.get('/splashes/:id/:filename', async (ctx) => handleImageRoute(ctx, 'splashes'));
	app.get('/embed-splashes/:id/:filename', async (ctx) => handleImageRoute(ctx, 'embed-splashes'));
	app.get('/emojis/:id', async (ctx) => handleSimpleImageRoute(ctx, 'emojis'));
	app.get('/stickers/:id', handleStickerRoute);
	app.get('/guilds/:guild_id/users/:user_id/avatars/:filename', async (ctx) =>
		handleGuildMemberImageRoute(ctx, 'avatars'),
	);
	app.get('/guilds/:guild_id/users/:user_id/banners/:filename', async (ctx) =>
		handleGuildMemberImageRoute(ctx, 'banners'),
	);
	app.get('/attachments/:channel_id/:attachment_id/:filename', handleAttachmentsRoute);
	app.get('/themes/:id.css', handleThemeRequest);

	app.get('/external/*', async (ctx) => {
		const path = ctx.req.path.replace('/external/', '');
		return processExternalMedia(ctx, path);
	});
}

app.use(
	logger((message: string, ...rest: Array<string>) => {
		Logger.info(rest.length > 0 ? `${message} ${rest.join(' ')}` : message);
	}),
);

app.onError((err, ctx) => {
	const isExpectedError = err instanceof Error && 'isExpected' in err && err.isExpected;

	if (!(v.isValiError(err) || err instanceof SyntaxError || err instanceof HTTPException || isExpectedError)) {
		Sentry.captureException(err);
	}

	if (v.isValiError(err) || err instanceof SyntaxError) {
		return ctx.text('Bad Request', {status: 400});
	}
	if (err instanceof HTTPException) {
		return err.getResponse();
	}
	if (isExpectedError) {
		Logger.warn({err}, 'Expected error occurred');
		return ctx.text('Bad Request', {status: 400});
	}
	Logger.error({err}, 'Unhandled error occurred');
	return ctx.text('Internal Server Error', {status: 500});
});

serve({
	fetch: app.fetch,
	hostname: '0.0.0.0',
	port: Config.PORT,
});

Logger.info({port: Config.PORT}, `Fluxer Media Proxy listening on http://0.0.0.0:${Config.PORT}`);
