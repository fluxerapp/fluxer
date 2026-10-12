// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import type {APIConfig} from '@app/api/config/APIConfig';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {
	extractClientIp,
	MissingClientIpError,
	requireClientIp,
	resolveClientIpHeaderName,
} from '@fluxer/ip_utils/src/ClientIp';
import type {HttpBindings} from '@hono/node-server';
import type {Context} from 'hono';
import {createMiddleware} from 'hono/factory';

export interface ClientIpResolution {
	trustClientIpHeader: boolean;
	clientIpHeaderName: string;
	ip: string | null;
}

interface ClientIpResolutionOptions {
	trustClientIpHeader: boolean;
	clientIpHeaderName: string;
}

const peerAddresses = new WeakMap<Request, string>();

export const PeerAddressMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	const remoteAddress = (ctx.env as Partial<HttpBindings> | undefined)?.incoming?.socket?.remoteAddress;
	if (remoteAddress) {
		peerAddresses.set(ctx.req.raw, remoteAddress);
	}
	await next();
});

function withPeerAddress(request: Request, options: ClientIpResolutionOptions) {
	return {...options, peerAddress: peerAddresses.get(request) ?? null};
}

export function resolveClientIpWithOptions(ctx: Context<HonoEnv>, options: ClientIpResolutionOptions): string | null {
	const clientIpHeaderName = resolveClientIpHeaderName(options.clientIpHeaderName);
	const {trustClientIpHeader} = options;
	const cached = ctx.get('clientIpResolution');
	if (
		cached &&
		cached.trustClientIpHeader === trustClientIpHeader &&
		cached.clientIpHeaderName === clientIpHeaderName
	) {
		return cached.ip;
	}
	const ip = extractClientIp(ctx.req.raw, withPeerAddress(ctx.req.raw, {trustClientIpHeader, clientIpHeaderName}));
	ctx.set('clientIpResolution', {trustClientIpHeader, clientIpHeaderName, ip});
	return ip;
}

function proxyClientIpOptions(proxy: APIConfig['proxy']): ClientIpResolutionOptions {
	return {trustClientIpHeader: proxy.trust_client_ip_header, clientIpHeaderName: proxy.client_ip_header};
}

export function extractConfiguredClientIp(request: Request): string | null {
	return extractClientIp(request, withPeerAddress(request, proxyClientIpOptions(Config.proxy)));
}

export function requireConfiguredClientIp(request: Request, proxy: APIConfig['proxy']): string {
	return requireClientIp(request, withPeerAddress(request, proxyClientIpOptions(proxy)));
}

export function getRequestClientIp(ctx: Context<HonoEnv>): string | null {
	return resolveClientIpWithOptions(ctx, proxyClientIpOptions(Config.proxy));
}

export function requireRequestClientIp(ctx: Context<HonoEnv>): string {
	const ip = getRequestClientIp(ctx);
	if (!ip) {
		throw new MissingClientIpError();
	}
	return ip;
}
