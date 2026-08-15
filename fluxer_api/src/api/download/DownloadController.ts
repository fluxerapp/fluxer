// SPDX-License-Identifier: AGPL-3.0-or-later

import {Readable} from 'node:stream';
import {
	DesktopChecksumRedirectParam,
	DesktopRedirectParam,
	DesktopTestBuildQuery,
	DesktopVersionedChecksumRedirectParam,
	DesktopVersionedRedirectParam,
	DesktopVersionsParam,
	DesktopVersionsQuery,
	DesktopVersionsResponse,
	VersionInfoResponse,
} from '@fluxer/schema/src/domains/download/DownloadSchemas';
import type {Context, Hono} from 'hono';
import {Config} from '../Config';
import {OpenAPI} from '../middleware/ResponseTypeMiddleware';
import type {HonoEnv} from '../types/HonoEnv';
import {Validator} from '../Validator';
import type {DesktopChecksumFile, DownloadService, DownloadStreamResult} from './DownloadService';
import {DESKTOP_REDIRECT_PREFIX, DOWNLOAD_PREFIX, UnsatisfiableRangeError} from './DownloadService';

function artifactFilename(key: string, filenameOverride?: string): string {
	return filenameOverride ?? key.split('/').pop() ?? 'download';
}

function setCommonArtifactHeaders(
	headers: Headers,
	key: string,
	cacheControl: string,
	filenameOverride: string | undefined,
	contentType: string | null | undefined,
	contentDisposition: string | null | undefined,
	etag: string | null | undefined,
	lastModified: Date | null | undefined,
): void {
	const filename = artifactFilename(key, filenameOverride);
	headers.set('Content-Type', contentType ?? 'application/octet-stream');
	headers.set('Content-Disposition', contentDisposition ?? `attachment; filename="${encodeURIComponent(filename)}"`);
	headers.set('Accept-Ranges', 'bytes');
	headers.set('Cache-Control', cacheControl);
	if (etag) {
		headers.set('ETag', etag);
	}
	if (lastModified) {
		headers.set('Last-Modified', lastModified.toUTCString());
	}
}

async function headArtifactResponse(
	ctx: Context<HonoEnv>,
	downloadService: DownloadService,
	key: string,
	cacheControl: string,
	filenameOverride?: string,
): Promise<Response> {
	const metadata = await downloadService.getDownloadMetadata({key});
	if (!metadata) {
		return ctx.text('Not Found', 404);
	}
	const headers = new Headers();
	setCommonArtifactHeaders(
		headers,
		key,
		cacheControl,
		filenameOverride,
		metadata.contentType,
		undefined,
		metadata.etag,
		metadata.lastModified,
	);
	headers.set('Content-Length', String(metadata.contentLength));
	return new Response(null, {status: 200, headers});
}

async function streamArtifactResponse(
	ctx: Context<HonoEnv>,
	downloadService: DownloadService,
	key: string,
	cacheControl: string,
	filenameOverride?: string,
): Promise<Response> {
	if (ctx.req.method === 'HEAD') {
		return headArtifactResponse(ctx, downloadService, key, cacheControl, filenameOverride);
	}
	const range = ctx.req.header('range') ?? undefined;
	let result: DownloadStreamResult | null;
	try {
		result = await downloadService.streamDownload({key, range});
	} catch (error) {
		if (error instanceof UnsatisfiableRangeError) {
			const headers = new Headers();
			headers.set('Accept-Ranges', 'bytes');
			headers.set('Content-Range', `bytes */${error.totalSize}`);
			headers.set('Cache-Control', cacheControl);
			return new Response(null, {status: 416, headers});
		}
		throw error;
	}
	if (!result) {
		return ctx.text('Not Found', 404);
	}
	const headers = new Headers();
	setCommonArtifactHeaders(
		headers,
		key,
		cacheControl,
		filenameOverride,
		result.contentType,
		result.contentDisposition,
		result.etag,
		result.lastModified,
	);
	headers.set('Content-Length', String(result.contentLength));
	if (result.contentRange) {
		headers.set('Content-Range', result.contentRange);
	}
	const body = Readable.toWeb(result.body) as ReadableStream;
	return new Response(body, {status: result.contentRange ? 206 : 200, headers});
}

function checksumFileResponse(ctx: Context<HonoEnv>, checksum: DesktopChecksumFile, cacheControl: string): Response {
	const headers = new Headers();
	const body = ctx.req.method === 'HEAD' ? null : checksum.body;
	headers.set('Content-Type', 'text/plain; charset=utf-8');
	headers.set('Content-Disposition', `attachment; filename="${encodeURIComponent(`${checksum.filename}.sha256`)}"`);
	headers.set('Cache-Control', cacheControl);
	headers.set('Content-Length', String(new TextEncoder().encode(checksum.body).byteLength));
	return new Response(body, {status: 200, headers});
}

export function DownloadController(routes: Hono<HonoEnv>): void {
	routes.get(
		`${DESKTOP_REDIRECT_PREFIX}/:channel/:plat/:arch/latest`,
		Validator('param', DesktopVersionsParam),
		Validator('query', DesktopTestBuildQuery),
		OpenAPI({
			operationId: 'get_latest_desktop_version',
			summary: 'Get latest desktop version',
			responseSchema: VersionInfoResponse,
			statusCode: 200,
			security: [],
			tags: ['Downloads'],
			description:
				'Returns metadata for the latest desktop version including download URLs and SHA-256 checksums for all available formats. Pass ?test=1 to resolve against unreleased test builds.',
		}),
		async (ctx) => {
			const {channel, plat, arch} = ctx.req.valid('param');
			const {test} = ctx.req.valid('query');
			const result = await ctx.get('downloadService').getLatestDesktopVersion({
				channel,
				plat,
				arch,
				baseUrl: Config.endpoints.apiClient,
				test,
			});
			if (!result) {
				return ctx.text('Not Found', 404);
			}
			return ctx.json(result, 200, {
				'Cache-Control': 'public, max-age=300',
			});
		},
	);
	routes.on(
		['GET', 'HEAD'],
		`${DESKTOP_REDIRECT_PREFIX}/:channel/:plat/:arch/latest/:format{[a-z_]+\\.sha256}`,
		Validator('param', DesktopChecksumRedirectParam),
		Validator('query', DesktopTestBuildQuery),
		OpenAPI({
			operationId: 'download_latest_desktop_version_checksum',
			summary: 'Download latest desktop version checksum',
			responseSchema: null,
			statusCode: 200,
			security: [],
			tags: ['Downloads'],
			description:
				'Returns a plain text SHA-256 checksum file for the latest available desktop application version. The format path segment must end in .sha256, for example appimage.sha256.',
		}),
		async (ctx) => {
			const {channel, plat, arch, format} = ctx.req.valid('param');
			const {test} = ctx.req.valid('query');
			const checksum = await ctx
				.get('downloadService')
				.resolveLatestDesktopChecksumFile({channel, plat, arch, format, test});
			if (!checksum) {
				return ctx.text('Not Found', 404);
			}
			return checksumFileResponse(ctx, checksum, 'no-store');
		},
	);
	routes.on(
		['GET', 'HEAD'],
		`${DESKTOP_REDIRECT_PREFIX}/:channel/:plat/:arch/latest/:format`,
		Validator('param', DesktopRedirectParam),
		Validator('query', DesktopTestBuildQuery),
		OpenAPI({
			operationId: 'download_latest_desktop_version',
			summary: 'Download latest desktop version',
			responseSchema: null,
			statusCode: 200,
			security: [],
			tags: ['Downloads'],
			description:
				'Streams the latest available desktop application version for the specified platform and architecture. Pass ?test=1 to download an unreleased test build.',
		}),
		async (ctx) => {
			const {channel, plat, arch, format} = ctx.req.valid('param');
			const {test} = ctx.req.valid('query');
			const downloadService = ctx.get('downloadService');
			const key = await downloadService.resolveLatestDesktopKey({channel, plat, arch, format, test});
			if (!key) {
				return ctx.text('Not Found', 404);
			}
			return streamArtifactResponse(ctx, downloadService, key, 'no-store');
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
			const {limit, before, after, test} = ctx.req.valid('query');
			const {versions, hasMore} = await ctx.get('downloadService').listDesktopVersions({
				channel,
				plat,
				arch,
				limit,
				before,
				after,
				baseUrl: Config.endpoints.apiClient,
				test,
			});
			return ctx.json({versions, has_more: hasMore}, 200, {
				'Cache-Control': 'public, max-age=300',
			});
		},
	);
	routes.on(
		['GET', 'HEAD'],
		`${DESKTOP_REDIRECT_PREFIX}/:channel/:plat/:arch/:version/:format{[a-z_]+\\.sha256}`,
		Validator('param', DesktopVersionedChecksumRedirectParam),
		Validator('query', DesktopTestBuildQuery),
		OpenAPI({
			operationId: 'download_desktop_version_checksum',
			summary: 'Download desktop version checksum',
			responseSchema: null,
			statusCode: 200,
			security: [],
			tags: ['Downloads'],
			description:
				'Returns a plain text SHA-256 checksum file for a specific desktop application version. The format path segment must end in .sha256, for example appimage.sha256.',
		}),
		async (ctx) => {
			const {channel, plat, arch, version, format} = ctx.req.valid('param');
			const {test} = ctx.req.valid('query');
			const checksum = await ctx
				.get('downloadService')
				.resolveVersionedDesktopChecksumFile({channel, plat, arch, version, format, test});
			if (!checksum) {
				return ctx.text('Not Found', 404);
			}
			return checksumFileResponse(ctx, checksum, 'public, max-age=86400');
		},
	);
	routes.on(
		['GET', 'HEAD'],
		`${DESKTOP_REDIRECT_PREFIX}/:channel/:plat/:arch/:version/:format`,
		Validator('param', DesktopVersionedRedirectParam),
		Validator('query', DesktopTestBuildQuery),
		OpenAPI({
			operationId: 'download_desktop_version',
			summary: 'Download desktop version',
			responseSchema: null,
			statusCode: 200,
			security: [],
			tags: ['Downloads'],
			description:
				'Streams a specific desktop application version for the given platform and architecture. Pass ?test=1 to download an unreleased test build.',
		}),
		async (ctx) => {
			const {channel, plat, arch, version, format} = ctx.req.valid('param');
			const {test} = ctx.req.valid('query');
			const downloadService = ctx.get('downloadService');
			const key = await downloadService.resolveVersionedDesktopKey({channel, plat, arch, version, format, test});
			if (!key) {
				return ctx.text('Not Found', 404);
			}
			return streamArtifactResponse(ctx, downloadService, key, 'public, max-age=86400');
		},
	);
	routes.on(
		['GET', 'HEAD'],
		`${DOWNLOAD_PREFIX}/*`,
		Validator('query', DesktopTestBuildQuery),
		OpenAPI({
			operationId: 'download_file',
			summary: 'Download file',
			responseSchema: null,
			statusCode: 200,
			security: [],
			tags: ['Downloads'],
			description:
				'Streams the requested file from storage. Pass ?test=1 on a desktop/ path to resolve against the desktop-test/ bucket prefix instead.',
		}),
		async (ctx) => {
			const {test} = ctx.req.valid('query');
			const downloadService = ctx.get('downloadService');
			const key = await downloadService.resolveDownloadKey({path: ctx.req.path, test});
			if (!key) {
				return ctx.text('Not Found', 404);
			}
			return streamArtifactResponse(ctx, downloadService, key, 'public, max-age=300');
		},
	);
}
