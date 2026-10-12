// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ParsedIpAddress} from '@fluxer/ip_utils/src/IpAddress';
import {parseIpAddress} from '@fluxer/ip_utils/src/IpAddress';

interface ClientIpExtractionOptions {
	trustClientIpHeader?: boolean;
	clientIpHeaderName?: string;
}

export class MissingClientIpError extends Error {
	readonly code = 'FORBIDDEN';
	readonly status = 403;

	constructor() {
		super('Client IP header is required');
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

function parseClientIpHeaderValue(value: string | null): ParsedIpAddress | null {
	if (value === null) {
		return null;
	}
	return parseIpAddress(value.split(',', 1)[0]);
}

export function extractClientIp(req: Request, options?: ClientIpExtractionOptions): string | null {
	if (!options?.trustClientIpHeader) {
		return null;
	}
	return (
		parseClientIpHeaderValue(req.headers.get(resolveClientIpHeaderName(options.clientIpHeaderName)))?.normalized ?? null
	);
}

export function requireClientIp(req: Request, options?: ClientIpExtractionOptions): string {
	const ip = extractClientIp(req, options);
	if (!ip) {
		throw new MissingClientIpError();
	}
	return ip;
}
