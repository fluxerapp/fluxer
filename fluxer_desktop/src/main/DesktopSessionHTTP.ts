// SPDX-License-Identifier: AGPL-3.0-or-later

import {Buffer} from 'node:buffer';
import type http from 'node:http';
import {Readable, type Writable} from 'node:stream';
import {net, session} from 'electron';

const CHROMIUM_REFUSED_REQUEST_HEADERS: ReadonlySet<string> = new Set([
	'connection',
	'content-length',
	'cookie2',
	'expect',
	'host',
	'keep-alive',
	'referer',
	'te',
	'trailer',
	'transfer-encoding',
	'upgrade',
]);

const DIRECT_PROXY_ROUTE = 'DIRECT';

export interface DesktopSessionHTTPRequest {
	readonly body: Uint8Array | Readable | null;
	readonly headers: Readonly<Record<string, string>>;
	readonly method: string;
	readonly signal: AbortSignal;
	readonly url: URL;
}

export interface DesktopSessionHTTPResponse {
	readonly headers: http.IncomingHttpHeaders;
	readonly message: Readable;
	readonly status: number;
	readonly statusText: string;
}

export type DesktopSessionHTTPSender = (request: DesktopSessionHTTPRequest) => Promise<DesktopSessionHTTPResponse>;

export type DesktopProxyResolver = (url: string) => Promise<string>;

class DesktopSessionHTTPAbortedError extends Error {
	public constructor(url: string) {
		super(`Desktop session request to ${url} was aborted`);
		this.name = 'DesktopSessionHTTPAbortedError';
	}
}

export function isDirectProxyRoute(route: string): boolean {
	const first = route.split(';', 1)[0] ?? '';
	return first.trim().toUpperCase() === DIRECT_PROXY_ROUTE;
}

export function resolveDesktopSessionProxy(url: string): Promise<string> {
	return session.defaultSession.resolveProxy(url);
}

function isRefusedRequestHeader(name: string): boolean {
	const normalized = name.toLowerCase();
	return CHROMIUM_REFUSED_REQUEST_HEADERS.has(normalized) || normalized.startsWith('proxy-');
}

function decodedResponseHeaders(headers: Record<string, string | Array<string>>): http.IncomingHttpHeaders {
	const result: http.IncomingHttpHeaders = {};
	const decoded = headers['content-encoding'] != null;
	for (const [name, value] of Object.entries(headers)) {
		const normalized = name.toLowerCase();
		if (decoded && (normalized === 'content-encoding' || normalized === 'content-length')) {
			continue;
		}
		if (normalized === 'set-cookie') {
			result['set-cookie'] = Array.isArray(value) ? value : [value];
			continue;
		}
		result[normalized] = Array.isArray(value) ? value.join(', ') : value;
	}
	return result;
}

export function sendThroughDesktopSession(request: DesktopSessionHTTPRequest): Promise<DesktopSessionHTTPResponse> {
	return new Promise<DesktopSessionHTTPResponse>((resolve, reject) => {
		if (request.signal.aborted) {
			reject(request.signal.reason);
			return;
		}
		const clientRequest = net.request({
			bypassCustomProtocolHandlers: true,
			cache: 'no-store',
			credentials: 'omit',
			method: request.method,
			redirect: 'manual',
			session: session.defaultSession,
			url: request.url.href,
			useSessionCookies: false,
		});
		let responded = false;
		let finished = false;
		let body: Readable | null = null;
		const fail = (error: unknown): void => {
			if (finished) {
				return;
			}
			finished = true;
			request.signal.removeEventListener('abort', onAbort);
			if (body == null) {
				reject(error);
				return;
			}
			body.destroy(error instanceof Error ? error : new DesktopSessionHTTPAbortedError(request.url.href));
		};
		const finish = (): void => {
			finished = true;
			request.signal.removeEventListener('abort', onAbort);
		};
		const onAbort = (): void => {
			clientRequest.abort();
			fail(request.signal.reason);
		};
		request.signal.addEventListener('abort', onAbort, {once: true});
		for (const [name, value] of Object.entries(request.headers)) {
			if (!isRefusedRequestHeader(name)) {
				clientRequest.setHeader(name, value);
			}
		}
		clientRequest.on('redirect', (status, _method, _location, headers) => {
			responded = true;
			finish();
			clientRequest.abort();
			resolve({
				headers: decodedResponseHeaders(headers),
				message: Readable.from([]),
				status,
				statusText: '',
			});
		});
		clientRequest.on('response', (incoming) => {
			responded = true;
			const source = incoming as unknown as Readable;
			const message = new Readable({
				read() {
					source.resume();
				},
				destroy(error, callback) {
					if (!finished) {
						finish();
						clientRequest.abort();
					}
					callback(error);
				},
			});
			body = message;
			source.on('data', (chunk: Buffer) => {
				if (!message.push(chunk)) {
					source.pause();
				}
			});
			source.on('end', () => {
				finish();
				message.push(null);
			});
			source.on('error', fail);
			incoming.on('aborted', () => fail(new DesktopSessionHTTPAbortedError(request.url.href)));
			resolve({
				headers: decodedResponseHeaders(incoming.headers),
				message,
				status: incoming.statusCode,
				statusText: incoming.statusMessage,
			});
		});
		clientRequest.on('error', fail);
		clientRequest.on('abort', () => {
			if (!responded) {
				fail(new DesktopSessionHTTPAbortedError(request.url.href));
			}
		});
		const requestBody = request.body;
		if (requestBody == null) {
			clientRequest.end();
			return;
		}
		if (requestBody instanceof Uint8Array) {
			clientRequest.end(Buffer.from(requestBody));
			return;
		}
		clientRequest.chunkedEncoding = true;
		requestBody.on('error', (error: Error) => {
			clientRequest.abort();
			fail(error);
		});
		requestBody.pipe(clientRequest as unknown as Writable);
	});
}
