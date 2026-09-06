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

interface HeadersLike {
	[key: string]: string | Array<string> | undefined;
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

function toStringHeaderValue(value: string | Array<string> | null | undefined): string | null {
	if (Array.isArray(value)) {
		const first = value[0];
		return typeof first === 'string' ? first : null;
	}
	return typeof value === 'string' ? value : null;
}

function parseSingleIpValue(value: string): ParsedIpAddress | null {
	const trimmed = value.trim();
	if (!trimmed) {
		return null;
	}
	return parseIpAddress(trimmed);
}

function parseClientIpHeaderValue(value: string | null): ParsedIpAddress | null {
	if (value === null) {
		return null;
	}
	const [firstHop] = value.split(',');
	if (firstHop === undefined) {
		return null;
	}
	return parseSingleIpValue(firstHop);
}

function createRequestHeaderReader(request: Request): HeaderReader {
	return {
		get: (name: string): string | null => {
			return request.headers.get(name);
		},
	};
}

function getHeaderValue(headers: HeadersLike, name: string): string | null {
	const lowerName = name.toLowerCase();
	const directMatch = toStringHeaderValue(headers[lowerName]);
	if (directMatch !== null) {
		return directMatch;
	}
	for (const [key, value] of Object.entries(headers)) {
		if (key.toLowerCase() === lowerName) {
			return toStringHeaderValue(value);
		}
	}
	return null;
}

function createNodeHeaderReader(headers: HeadersLike): HeaderReader {
	return {
		get: (name: string): string | null => {
			return getHeaderValue(headers, name);
		},
	};
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
	return extractClientIpDetailsFromReader(createRequestHeaderReader(req), options);
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

export function extractClientIpDetailsFromHeaders(
	headers: HeadersLike,
	options?: ClientIpExtractionOptions,
): ExtractedClientIp | null {
	return extractClientIpDetailsFromReader(createNodeHeaderReader(headers), options);
}

export function extractClientIpFromHeaders(headers: HeadersLike, options?: ClientIpExtractionOptions): string | null {
	const extracted = extractClientIpDetailsFromHeaders(headers, options);
	return extracted?.ip ?? null;
}
