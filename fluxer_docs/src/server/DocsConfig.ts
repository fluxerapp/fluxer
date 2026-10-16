// SPDX-License-Identifier: AGPL-3.0-or-later

import {buildUrl} from '@fluxer/config/src/EndpointDerivation';

const DEFAULT_BASE_DOMAIN = 'localhost';
const DEFAULT_PUBLIC_SCHEME = 'http';
const DEFAULT_LISTEN_HOST = '0.0.0.0';
const DEFAULT_LISTEN_PORT = 8080;

function readString(name: string, fallback: string): string {
	const value = process.env[name];
	if (value == null) {
		return fallback;
	}
	if (value.length === 0) {
		return fallback;
	}
	return value;
}

function readPort(name: string): number | undefined {
	const value = process.env[name];
	if (value == null) {
		return undefined;
	}
	if (value.length === 0) {
		return undefined;
	}
	const parsed = Number.parseInt(value, 10);
	if (!Number.isInteger(parsed)) {
		return undefined;
	}
	return parsed;
}

function readScheme(name: string): 'http' | 'https' {
	if (readString(name, DEFAULT_PUBLIC_SCHEME) === 'https') {
		return 'https';
	}
	return 'http';
}

export function docsBaseDomain(): string {
	return readString('FLUXER_BASE_DOMAIN', DEFAULT_BASE_DOMAIN);
}

export function docsPublicEndpoint(): string {
	const override = process.env.FLUXER_DOCS_ENDPOINT;
	if (override != null && override.length > 0) {
		return override;
	}

	return buildUrl(readScheme('FLUXER_PUBLIC_SCHEME'), docsBaseDomain(), readPort('FLUXER_PUBLIC_PORT'));
}

export function docsListenHost(): string {
	return readString('FLUXER_DOCS_LISTEN_HOST', DEFAULT_LISTEN_HOST);
}

export function docsListenPort(): number {
	const parsed = readPort('FLUXER_DOCS_LISTEN_PORT');
	if (parsed == null) {
		return DEFAULT_LISTEN_PORT;
	}
	if (parsed < 1) {
		return DEFAULT_LISTEN_PORT;
	}
	if (parsed > 65_535) {
		return DEFAULT_LISTEN_PORT;
	}
	return parsed;
}

export function docsSearchIndexing(): boolean {
	return readString('FLUXER_DOCS_SEARCH_INDEXING', 'false') === 'true';
}
