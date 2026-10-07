// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IpAddressFamily, ParsedIpAddress} from '@fluxer/ip_utils/src/IpAddress';
import {parseIpAddress} from '@fluxer/ip_utils/src/IpAddress';

interface ClientIpExtractionOptions {
	trustClientIpHeader?: boolean;
	clientIpHeaderName?: string;
}

type ClientIpSource = 'client-ip-header';

interface ExtractedClientIp {
	ip: string;
	source: ClientIpSource;
	ipVersion: IpAddressFamily;
}

export class MissingClientIpError extends Error {
	readonly code = 'FORBIDDEN';
	readonly status = 403;

	constructor() {
		super('Client IP header is required');
		this.name = 'MissingClientIpError';
	}
}

interface HeaderReader {
	get(name: string): string | null;
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

function extractClientIpDetailsFromReader(
	headerReader: HeaderReader,
	options?: ClientIpExtractionOptions,
): ExtractedClientIp | null {
	if (!options?.trustClientIpHeader) {
		return null;
	}
	const headerName = resolveClientIpHeaderName(options.clientIpHeaderName);
	const clientIpHeader = parseClientIpHeaderValue(headerReader.get(headerName));
	if (clientIpHeader) {
		return {
			ip: clientIpHeader.normalized,
			source: 'client-ip-header',
			ipVersion: clientIpHeader.family,
		};
	}
	return null;
}

export function extractClientIpDetails(req: Request, options?: ClientIpExtractionOptions): ExtractedClientIp | null {
	return extractClientIpDetailsFromReader(req.headers, options);
}

export function extractClientIp(req: Request, options?: ClientIpExtractionOptions): string | null {
	const extracted = extractClientIpDetails(req, options);
	return extracted?.ip ?? null;
}

export function requireClientIp(req: Request, options?: ClientIpExtractionOptions): string {
	const ip = extractClientIp(req, options);
	if (!ip) {
		throw new MissingClientIpError();
	}
	return ip;
}
