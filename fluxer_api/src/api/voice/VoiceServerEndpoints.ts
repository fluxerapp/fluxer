// SPDX-License-Identifier: AGPL-3.0-or-later

import {lookup} from 'node:dns/promises';
import http from 'node:http';
import https from 'node:https';
import {isIP, type LookupFunction} from 'node:net';
import {Config} from '@app/api/Config';
import type {VoiceServerRecord} from '@app/api/voice/VoiceModel';
import {isPublicIpAddress} from '@fluxer/ip_utils/src/IpAddress';
import {AccessToken} from 'livekit-server-sdk';

export function toHttpUrl(wsUrl: string): string {
	return wsUrl.replace(/^wss:\/\//, 'https://').replace(/^ws:\/\//, 'http://');
}

export function bundledVoiceServerId(): string | null {
	const defaultRegion = Config.voice.defaultRegion;
	if (!defaultRegion || !Config.voice.bundled) {
		return null;
	}
	return `${defaultRegion.id}-server-1`;
}

function isConfiguredInternalServer(server: Pick<VoiceServerRecord, 'regionId' | 'serverId'>): boolean {
	const serverId = bundledVoiceServerId();
	if (!serverId || !Config.voice.internalUrl) {
		return false;
	}
	return server.regionId === Config.voice.defaultRegion?.id && server.serverId === serverId;
}

export function resolveRoomServiceEndpoint(server: VoiceServerRecord): string {
	if (server.internalEndpoint) {
		return server.internalEndpoint;
	}
	if (isConfiguredInternalServer(server)) {
		return Config.voice.internalUrl!;
	}
	return server.endpoint;
}

export function usesPublicEndpoint(server: VoiceServerRecord): boolean {
	return !server.internalEndpoint && !isConfiguredInternalServer(server);
}

export function endpointOrigin(endpoint: string): string | null {
	try {
		const url = new URL(endpoint);
		if (url.protocol !== 'wss:' && url.protocol !== 'ws:') {
			return null;
		}
		return `${url.protocol}//${url.host}`;
	} catch {
		return null;
	}
}

export class VoiceEndpointError extends Error {
	constructor(
		readonly reason: 'invalid' | 'private' | 'unreachable' | 'key_mismatch',
		message: string,
	) {
		super(message);
		this.name = 'VoiceEndpointError';
	}
}

type Resolve = (host: string) => Promise<Array<string>>;

const resolveHost: Resolve = async (host) => (await lookup(host, {all: true, verbatim: true})).map((r) => r.address);

function bareHost(url: URL): string {
	return url.hostname.replace(/^\[|\]$/g, '');
}

export function parseNodeEndpoint(endpoint: string): URL {
	let url: URL;
	try {
		url = new URL(endpoint);
	} catch {
		throw new VoiceEndpointError('invalid', 'The endpoint is not a URL');
	}
	if (url.protocol !== 'wss:') {
		throw new VoiceEndpointError('invalid', 'The endpoint must start with wss://');
	}
	if (url.username || url.password || url.search || url.hash) {
		throw new VoiceEndpointError('invalid', 'The endpoint must not have credentials, a query or a fragment');
	}
	if (isIP(bareHost(url)) === 6) {
		throw new VoiceEndpointError('invalid', 'An endpoint that is an IP address must be IPv4');
	}
	return url;
}

async function resolveCheckedAddress(url: URL, allowPrivate: boolean): Promise<string> {
	const host = bareHost(url);
	const addresses = isIP(host) ? [host] : await resolveHost(host).catch(() => []);
	if (addresses.length === 0) {
		throw new VoiceEndpointError('unreachable', `${host} does not resolve`);
	}
	if (!allowPrivate && addresses.some((address) => !isPublicIpAddress(address))) {
		throw new VoiceEndpointError('private', `${host} resolves to an address that is not public`);
	}
	return addresses[0]!;
}

interface ProbeResponse {
	status: number;
	body: string;
}

function pinnedLookup(address: string): LookupFunction {
	const family = isIP(address);
	return (_hostname, opts, callback) => {
		if (opts.all) {
			callback(null, [{address, family}]);
			return;
		}
		callback(null, address, family);
	};
}

function request(
	url: URL,
	options: {method: string; address: string | null; headers?: Record<string, string>; body?: string; timeoutMs: number},
): Promise<ProbeResponse> {
	const client = url.protocol === 'https:' ? https : http;
	const host = bareHost(url);
	return new Promise((resolve, reject) => {
		const req = client.request(
			url,
			{
				method: options.method,
				headers: {...options.headers, Host: url.host},
				servername: isIP(host) ? undefined : host,
				timeout: options.timeoutMs,
				lookup: options.address ? pinnedLookup(options.address) : undefined,
			},
			(res) => {
				let body = '';
				res.setEncoding('utf8');
				res.on('data', (chunk: string) => {
					if (body.length < 4096) body += chunk;
				});
				res.on('end', () => resolve({status: res.statusCode ?? 0, body}));
				res.on('error', reject);
			},
		);
		req.on('timeout', () => req.destroy(new Error('timed out')));
		req.on('error', reject);
		req.end(options.body);
	});
}

function withPath(base: string, path: string): URL {
	const url = new URL(toHttpUrl(base));
	url.pathname = `${url.pathname.replace(/\/+$/, '')}${path}`;
	return url;
}

function transportError(error: unknown): string {
	if (error && typeof error === 'object' && 'code' in error && typeof error.code === 'string') {
		return error.code;
	}
	return error instanceof Error ? error.message : 'connection failed';
}

export async function probeLiveKit(
	base: string,
	options: {checkAddress: boolean; allowPrivate: boolean; timeoutMs: number},
): Promise<void> {
	const url = withPath(base, '/');
	const address = options.checkAddress ? await resolveCheckedAddress(url, options.allowPrivate) : null;
	let response: ProbeResponse;
	try {
		response = await request(url, {method: 'GET', address, timeoutMs: options.timeoutMs});
	} catch (error) {
		throw new VoiceEndpointError('unreachable', transportError(error));
	}
	if (response.status !== 200 || response.body.trim() !== 'OK') {
		throw new VoiceEndpointError('unreachable', `HTTP ${response.status}`);
	}
}

export async function checkLiveKitKeys(
	base: string,
	keys: {apiKey: string; apiSecret: string},
	options: {checkAddress: boolean; allowPrivate: boolean; timeoutMs: number},
): Promise<void> {
	const url = withPath(base, '/twirp/livekit.RoomService/ListRooms');
	const address = options.checkAddress ? await resolveCheckedAddress(url, options.allowPrivate) : null;
	const token = new AccessToken(keys.apiKey, keys.apiSecret, {ttl: 60});
	token.addGrant({roomList: true});
	let response: ProbeResponse;
	try {
		response = await request(url, {
			method: 'POST',
			address,
			timeoutMs: options.timeoutMs,
			headers: {Authorization: `Bearer ${await token.toJwt()}`, 'Content-Type': 'application/json'},
			body: '{}',
		});
	} catch (error) {
		throw new VoiceEndpointError('unreachable', transportError(error));
	}
	if (response.status === 401 || response.status === 403) {
		throw new VoiceEndpointError('key_mismatch', `HTTP ${response.status}`);
	}
	if (response.status !== 200) {
		throw new VoiceEndpointError('unreachable', `HTTP ${response.status}`);
	}
}
