// SPDX-License-Identifier: AGPL-3.0-or-later

import type http from 'node:http';
import type {DesktopOutboundHTTP} from '@electron/main/DesktopOutboundHTTP';
import {requireDesktopHTTPOrigin} from '@electron/main/DesktopOutboundHTTP';
import type {DesktopSelectedInstanceClient} from '@electron/main/SelectedInstanceFetch';
import {Headers as HttpHeader} from '@fluxer/constants/src/Headers';
import {MimeType} from '@fluxer/constants/src/HttpConstants';
import {
	fetchInstanceDiscovery,
	type InstanceDiscoveryDocument,
	type InstanceDiscoveryFetch,
} from '@fluxer/instance_bootstrap/src/Discovery';

const INSTANCE_VALIDATION_CACHE_TTL_MS = 5 * 60 * 1000;
const INSTANCE_VALIDATION_TIMEOUT_MS = 5000;

const API_ENDPOINT_KEYS = ['api', 'api_client', 'api_public'] as const;
const ADVERTISED_ENDPOINT_KEYS = [
	'api',
	'api_client',
	'api_public',
	'media',
	'upload_relay',
	'static_cdn',
	'marketing',
	'docs',
	'admin',
	'invite',
	'gift',
	'webapp',
] as const;

export interface ValidatedFluxerInstance {
	readonly apiBases: ReadonlyArray<URL>;
	readonly gatewayEndpoint: string;
	readonly expiresAt: number;
}

interface FetchValidatedFluxerInstanceRequest {
	readonly origin: string;
	readonly outboundHTTP: DesktopOutboundHTTP;
	readonly selectedInstanceClient: DesktopSelectedInstanceClient;
	readonly signal?: AbortSignal;
}

class InvalidFluxerInstanceDiscoveryError extends Error {
	public constructor(origin: string, reason: string) {
		super(`Instance discovery document served by ${origin} is unusable: ${reason}`);
		this.name = 'InvalidFluxerInstanceDiscoveryError';
	}
}

function headerValue(headers: http.IncomingHttpHeaders, name: string): string | null {
	const value = headers[name.toLowerCase()];
	if (typeof value === 'string') {
		return value;
	}
	if (Array.isArray(value)) {
		return value[0] ?? null;
	}
	return null;
}

function createInstanceDiscoveryFetch(
	client: DesktopSelectedInstanceClient,
	expectedOrigin: string,
): InstanceDiscoveryFetch {
	return async (url, init) => {
		const response = await client.fetch({
			expectedOrigin,
			headers: {[HttpHeader.ACCEPT]: MimeType.JSON, ...init.headers},
			method: init.method,
			signal: init.signal,
			timeoutMs: INSTANCE_VALIDATION_TIMEOUT_MS,
			url,
		});
		return {
			body: null,
			headers: {get: (name: string) => headerValue(response.headers, name)},
			status: response.status,
			text: async () => response.body?.toString('utf8') ?? '',
		};
	};
}

function readAPIBases(document: InstanceDiscoveryDocument): Array<URL> {
	const bases: Array<URL> = [];
	for (const key of API_ENDPOINT_KEYS) {
		const endpoint = document.endpoints[key];
		if (endpoint == null) {
			continue;
		}
		bases.push(new URL(endpoint));
	}
	return bases;
}

function readAdvertisedOrigins(document: InstanceDiscoveryDocument): Array<string> {
	const origins = new Set<string>();
	for (const key of ADVERTISED_ENDPOINT_KEYS) {
		const endpoint = document.endpoints[key];
		if (endpoint == null) {
			continue;
		}
		origins.add(requireDesktopHTTPOrigin(new URL(endpoint).origin));
	}
	return [...origins];
}

export async function fetchValidatedFluxerInstance({
	origin,
	outboundHTTP,
	selectedInstanceClient,
	signal,
}: FetchValidatedFluxerInstanceRequest): Promise<ValidatedFluxerInstance> {
	const anchorOrigin = requireDesktopHTTPOrigin(origin);
	const result = await fetchInstanceDiscovery({
		fetch: createInstanceDiscoveryFetch(selectedInstanceClient, anchorOrigin),
		input: anchorOrigin,
		signal,
	});
	if (result.kind !== 'ok') {
		throw new InvalidFluxerInstanceDiscoveryError(
			anchorOrigin,
			'the instance answered a conditional request it never received',
		);
	}
	const apiBases = readAPIBases(result.document);
	if (apiBases.length === 0) {
		throw new InvalidFluxerInstanceDiscoveryError(anchorOrigin, 'it declares no API endpoint');
	}
	const advertisedOrigins = readAdvertisedOrigins(result.document);
	await outboundHTTP.registerAnchoredOrigins({anchorOrigin, origins: advertisedOrigins});
	return {
		apiBases,
		gatewayEndpoint: result.document.endpoints.gateway,
		expiresAt: Date.now() + INSTANCE_VALIDATION_CACHE_TTL_MS,
	};
}
