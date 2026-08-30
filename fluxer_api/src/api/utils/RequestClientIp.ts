// SPDX-License-Identifier: AGPL-3.0-or-later

import {extractClientIp, MissingClientIpError, resolveClientIpHeaderName} from '@fluxer/ip_utils/src/ClientIp';
import type {Context} from 'hono';
import {Config} from '../Config';
import type {HonoEnv} from '../types/HonoEnv';

export interface ClientIpResolution {
	trustClientIpHeader: boolean;
	clientIpHeaderName: string;
	ip: string | null;
}

interface ClientIpResolutionOptions {
	trustClientIpHeader: boolean;
	clientIpHeaderName: string;
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
	const ip = extractClientIp(ctx.req.raw, {trustClientIpHeader, clientIpHeaderName});
	ctx.set('clientIpResolution', {trustClientIpHeader, clientIpHeaderName, ip});
	return ip;
}

export function getRequestClientIp(ctx: Context<HonoEnv>): string | null {
	return resolveClientIpWithOptions(ctx, {
		trustClientIpHeader: Config.proxy.trust_client_ip_header,
		clientIpHeaderName: Config.proxy.client_ip_header,
	});
}

export function requireRequestClientIp(ctx: Context<HonoEnv>): string {
	const ip = getRequestClientIp(ctx);
	if (!ip) {
		throw new MissingClientIpError();
	}
	return ip;
}
