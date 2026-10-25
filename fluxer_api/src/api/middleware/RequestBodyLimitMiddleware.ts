// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HonoEnv} from '@app/api/types/HonoEnv';
import {ASSET_FORMAT_POLICY, type AssetKind} from '@fluxer/constants/src/AssetFormatPolicy';
import {RequestBodyTooLargeError} from '@fluxer/errors/src/domains/core/RequestBodyTooLargeError';
import type {Context, MiddlewareHandler} from 'hono';
import {bodyLimit} from 'hono/body-limit';
import {createMiddleware} from 'hono/factory';
import {matchedRoutes} from 'hono/route';
import {COMPOSED_HANDLER} from 'hono/utils/constants';

const JSON_ASSET_ENVELOPE_BYTES = 1024 * 1024;

export function jsonAssetBodyLimit(assets: Partial<Record<Exclude<AssetKind, 'attachment'>, number>>): number {
	let rawBytes = 0;
	for (const [kind, count] of Object.entries(assets) as Array<[AssetKind, number]>) {
		rawBytes += ASSET_FORMAT_POLICY[kind].maxBytes * count;
	}
	return 4 * Math.ceil(rawBytes / 3) + JSON_ASSET_ENVELOPE_BYTES;
}

const largestJsonAssetKind = (Object.keys(ASSET_FORMAT_POLICY) as Array<AssetKind>)
	.filter((kind): kind is Exclude<AssetKind, 'attachment'> => kind !== 'attachment')
	.reduce((largest, kind) =>
		ASSET_FORMAT_POLICY[kind].maxBytes > ASSET_FORMAT_POLICY[largest].maxBytes ? kind : largest,
	);

export const DEFAULT_REQUEST_BODY_LIMIT = jsonAssetBodyLimit({[largestJsonAssetKind]: 1});

const routeBodyLimits = new WeakMap<object, number>();
const limiters = new Map<number, MiddlewareHandler>();

export function RequestBodyLimit(maxBytes: number): MiddlewareHandler<HonoEnv> {
	const marker: MiddlewareHandler<HonoEnv> = async (_ctx, next) => {
		await next();
	};
	routeBodyLimits.set(marker, maxBytes);
	return marker;
}

function resolveRouteBodyLimit(ctx: Context<HonoEnv>): number {
	for (const route of matchedRoutes(ctx)) {
		const handler = route.handler as MiddlewareHandler & {[COMPOSED_HANDLER]?: MiddlewareHandler};
		const limit = routeBodyLimits.get(handler[COMPOSED_HANDLER] ?? handler);
		if (limit !== undefined) return limit;
	}
	return DEFAULT_REQUEST_BODY_LIMIT;
}

function limiterFor(maxSize: number): MiddlewareHandler {
	let limiter = limiters.get(maxSize);
	if (!limiter) {
		limiter = bodyLimit({
			maxSize,
			onError: () => {
				throw new RequestBodyTooLargeError(maxSize);
			},
		});
		limiters.set(maxSize, limiter);
	}
	return limiter;
}

export const RequestBodyLimitMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	if (!ctx.req.raw.body) {
		await next();
		return;
	}
	if ((ctx.req.header('content-type') ?? '').includes('multipart/form-data')) {
		await next();
		return;
	}
	await limiterFor(resolveRouteBodyLimit(ctx))(ctx, next);
});
