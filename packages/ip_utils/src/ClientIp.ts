// SPDX-License-Identifier: AGPL-3.0-or-later

import {canonicalizeIpAddress} from '@fluxer/ip_utils/src/IpAddress';

interface ClientIpExtractionOptions {
	trustClientIpHeader?: boolean;
	clientIpHeaderName?: string;
	peerAddress?: string | null;
}

export class MissingClientIpError extends Error {
	readonly code = 'FORBIDDEN';
	readonly status = 403;

	constructor() {
		super('Client IP is required');
		this.name = 'MissingClientIpError';
	}
}

const DEFAULT_CLIENT_IP_HEADER_NAME = 'x-forwarded-for';

function normalizeHeaderName(headerName: string): string {
	return headerName.trim().toLowerCase();
}

export function resolveClientIpHeaderName(clientIpHeaderName?: string): string {
	return normalizeHeaderName(clientIpHeaderName ?? DEFAULT_CLIENT_IP_HEADER_NAME);
}

export function extractClientIp(req: Request, options?: ClientIpExtractionOptions): string | null {
	if (!options?.trustClientIpHeader) {
		return options?.peerAddress ? canonicalizeIpAddress(options.peerAddress) : null;
	}
	const headerValue = req.headers.get(resolveClientIpHeaderName(options.clientIpHeaderName));
	return headerValue === null ? null : canonicalizeIpAddress(headerValue.split(',', 1)[0]);
}

export function requireClientIp(req: Request, options?: ClientIpExtractionOptions): string {
	const ip = extractClientIp(req, options);
	if (!ip) {
		throw new MissingClientIpError();
	}
	return ip;
}
