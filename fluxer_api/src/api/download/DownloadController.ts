// SPDX-License-Identifier: AGPL-3.0-or-later

import {DESKTOP_REDIRECT_PREFIX, DOWNLOAD_PREFIX, resolveDownloadRedirect} from '@app/api/download/DownloadRedirects';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {
	DesktopChecksumRedirectParam,
	DesktopRedirectParam,
	DesktopVersionedChecksumRedirectParam,
	DesktopVersionedRedirectParam,
	DesktopVersionedZsyncRedirectParam,
	DesktopVersionsParam,
	DesktopZsyncRedirectParam,
} from '@fluxer/schema/src/domains/download/DownloadSchemas';
import type {Context, Hono} from 'hono';

function redirectToPackageOrigin(ctx: Context<HonoEnv>): Response {
	const redirect = resolveDownloadRedirect(ctx.req.path);
	if (!redirect) {
		return ctx.text('Not Found', 404);
	}
	return new Response(null, {
		status: 302,
		headers: new Headers({
			Location: redirect.location,
			'Cache-Control': redirect.cacheControl,
			'Accept-Ranges': 'bytes',
		}),
	});
}

export function DownloadController(routes: Hono<HonoEnv>): void {
	routes.get(
		`${DESKTOP_REDIRECT_PREFIX}/:channel/:plat/:arch/latest`,
		Validator('param', DesktopVersionsParam),
		async (ctx) => redirectToPackageOrigin(ctx),
	);
	routes.on(
		['GET', 'HEAD'],
		`${DESKTOP_REDIRECT_PREFIX}/:channel/:plat/:arch/latest/:format{[a-z_]+\\.sha256}`,
		Validator('param', DesktopChecksumRedirectParam),
		async (ctx) => redirectToPackageOrigin(ctx),
	);
	routes.on(
		['GET', 'HEAD'],
		`${DESKTOP_REDIRECT_PREFIX}/:channel/:plat/:arch/latest/:format{[a-z_]+\\.zsync}`,
		Validator('param', DesktopZsyncRedirectParam),
		async (ctx) => redirectToPackageOrigin(ctx),
	);
	routes.on(
		['GET', 'HEAD'],
		`${DESKTOP_REDIRECT_PREFIX}/:channel/:plat/:arch/latest/:format`,
		Validator('param', DesktopRedirectParam),
		async (ctx) => redirectToPackageOrigin(ctx),
	);
	routes.on(
		['GET', 'HEAD'],
		`${DESKTOP_REDIRECT_PREFIX}/:channel/:plat/:arch/:version/:format{[a-z_]+\\.zsync}`,
		Validator('param', DesktopVersionedZsyncRedirectParam),
		async (ctx) => redirectToPackageOrigin(ctx),
	);
	routes.on(
		['GET', 'HEAD'],
		`${DESKTOP_REDIRECT_PREFIX}/:channel/:plat/:arch/:version/:format{[a-z_]+\\.sha256}`,
		Validator('param', DesktopVersionedChecksumRedirectParam),
		async (ctx) => redirectToPackageOrigin(ctx),
	);
	routes.on(
		['GET', 'HEAD'],
		`${DESKTOP_REDIRECT_PREFIX}/:channel/:plat/:arch/:version/:format`,
		Validator('param', DesktopVersionedRedirectParam),
		async (ctx) => redirectToPackageOrigin(ctx),
	);
	routes.on(['GET', 'HEAD'], `${DOWNLOAD_PREFIX}/*`, async (ctx) => redirectToPackageOrigin(ctx));
}
