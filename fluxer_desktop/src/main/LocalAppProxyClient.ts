// SPDX-License-Identifier: AGPL-3.0-or-later

import type http from 'node:http';
import {Readable, type Transform} from 'node:stream';
import zlib from 'node:zlib';
import {
	DesktopOriginTrust,
	type DesktopOutboundHTTP,
	type DesktopOutboundHTTPMessage,
	type DesktopOutboundHTTPRequest,
} from '@electron/main/DesktopOutboundHTTP';
import {LOCAL_APP_PROXY_HEAD_METHOD, requestHasBody} from '@electron/main/LocalAppProxyHTTPPolicy';
import {
	type DesktopLocalAppUploadProgress,
	LOCAL_APP_UPLOAD_PROGRESS_CHANNELS,
} from '@fluxer/desktop_ipc/src/LocalAppRouteContract';

const LOCAL_APP_PROXY_DEADLINE_MS = 5 * 60 * 1000;
const LOCAL_APP_PROXY_REQUEST_BODY_MAX_BYTES = 100 * 1024 * 1024;
export const LOCAL_APP_PROXY_ACCEPT_ENCODING = 'gzip, br';

const LOCAL_APP_PROXY_SERVICE_NAME = 'desktop_local_app_proxy';
const LOCAL_APP_PROXY_MAX_CONCURRENT_REQUESTS = 32;
const LOCAL_APP_PROXY_MAX_QUEUED_REQUESTS = 4096;
const UPLOAD_PROGRESS_MIN_BYTES = 64 * 1024;
const UPLOAD_PROGRESS_MIN_INTERVAL_MS = 100;
const MINIMUM_RESPONSE_STATUS = 200;
const MAXIMUM_RESPONSE_STATUS = 599;
const NULL_BODY_STATUSES: ReadonlySet<number> = new Set([204, 205, 304]);

class LocalAppProxyQueueOverflowError extends Error {
	public constructor() {
		super('Local app proxy has too many queued requests');
		this.name = 'LocalAppProxyQueueOverflowError';
	}
}

class LocalAppProxyConcurrencyPermit {
	private releasePermit: (() => void) | null;

	public constructor(releasePermit: () => void) {
		this.releasePermit = releasePermit;
	}

	public release(): void {
		const releasePermit = this.releasePermit;
		if (releasePermit == null) {
			return;
		}
		this.releasePermit = null;
		releasePermit();
	}
}

class LocalAppProxyResponseBodyLease {
	public readonly reader: ReadableStreamDefaultReader<Uint8Array>;
	private readonly permit: LocalAppProxyConcurrencyPermit;
	private settled = false;

	public constructor(body: ReadableStream<Uint8Array>, permit: LocalAppProxyConcurrencyPermit) {
		this.reader = body.getReader();
		this.permit = permit;
	}

	public release(): void {
		if (this.settled) {
			return;
		}
		this.settled = true;
		try {
			this.reader.releaseLock();
		} finally {
			this.permit.release();
		}
	}
}

class LocalAppProxyConcurrencyGate {
	private active = 0;
	private readonly waiting: Array<{readonly admit: () => void}> = [];

	public acquire(signal: AbortSignal): Promise<LocalAppProxyConcurrencyPermit> {
		signal.throwIfAborted();
		if (this.active < LOCAL_APP_PROXY_MAX_CONCURRENT_REQUESTS) {
			this.active += 1;
			return Promise.resolve(new LocalAppProxyConcurrencyPermit(() => this.release()));
		}
		if (this.waiting.length >= LOCAL_APP_PROXY_MAX_QUEUED_REQUESTS) {
			throw new LocalAppProxyQueueOverflowError();
		}
		return new Promise<LocalAppProxyConcurrencyPermit>((resolve, reject) => {
			let waiter: (typeof this.waiting)[number];
			const onAbort = () => {
				const index = this.waiting.indexOf(waiter);
				if (index === -1) {
					return;
				}
				this.waiting.splice(index, 1);
				reject(signal.reason);
			};
			const admit = () => {
				signal.removeEventListener('abort', onAbort);
				resolve(new LocalAppProxyConcurrencyPermit(() => this.release()));
			};
			waiter = {admit};
			this.waiting.push(waiter);
			signal.addEventListener('abort', onAbort, {once: true});
		});
	}

	private release(): void {
		const next = this.waiting.shift();
		if (next != null) {
			next.admit();
			return;
		}
		this.active -= 1;
	}
}

interface DesktopLocalAppProxyClientDependencies {
	readonly outboundHTTP: DesktopOutboundHTTP;
}

interface LocalAppProxyFetchRequest {
	readonly targetURL: string;
	readonly method: string;
	readonly headers: Headers;
	readonly body: ReadableStream<Uint8Array> | null;
	readonly signal: AbortSignal;
	readonly acceptEncoding: string | null;
	readonly uploadId: string | null;
	readonly uploadTotalBytes: number | null;
}

interface LocalAppProxyFailureSettlement {
	readonly error: unknown;
	readonly bodyCancellationFailed: boolean;
}

interface LocalAppProxyFailureSettlementRequest {
	readonly description: string;
	readonly failure: unknown;
	readonly response: Response | null;
}

interface LocalAppProxyResponseBodyRequest {
	readonly description: string;
	readonly method: string;
	readonly response: Response;
}

class LocalAppProxyHeadBodyUnusedError extends Error {
	public constructor(description: string) {
		super(`${description} body is unused for HEAD`);
		this.name = 'LocalAppProxyHeadBodyUnusedError';
	}
}

class LocalAppProxyUpstreamStatusError extends Error {
	public constructor(status: number) {
		super(`Local app proxy upstream answered with an unusable HTTP status: ${status}`);
		this.name = 'LocalAppProxyUpstreamStatusError';
	}
}

class LocalAppProxyUnsupportedContentEncodingError extends Error {
	public constructor(encoding: string) {
		super(`Local app proxy upstream answered with an unsupported content encoding: ${encoding}`);
		this.name = 'LocalAppProxyUnsupportedContentEncodingError';
	}
}

const uploadProgressSubscribers = new Set<Electron.WebContents>();

export function addLocalAppUploadProgressSubscriber(subscriber: Electron.WebContents): void {
	if (subscriber.isDestroyed() || uploadProgressSubscribers.has(subscriber)) {
		return;
	}
	uploadProgressSubscribers.add(subscriber);
	subscriber.once('destroyed', () => {
		uploadProgressSubscribers.delete(subscriber);
	});
}

export function clearLocalAppUploadProgressSubscribers(): void {
	uploadProgressSubscribers.clear();
}

export function emitLocalAppUploadFailure(uploadId: string | null): void {
	if (uploadId == null) {
		return;
	}
	emitLocalAppUploadProgress({uploadId, loaded: 0, total: null, done: true, failed: true});
}

function emitLocalAppUploadProgress(progress: DesktopLocalAppUploadProgress): void {
	for (const subscriber of [...uploadProgressSubscribers]) {
		if (subscriber.isDestroyed()) {
			uploadProgressSubscribers.delete(subscriber);
			continue;
		}
		subscriber.send(LOCAL_APP_UPLOAD_PROGRESS_CHANNELS.progress, progress);
	}
}

export class DesktopLocalAppProxyClient {
	private readonly outboundHTTP: DesktopOutboundHTTP;

	public constructor(dependencies: DesktopLocalAppProxyClientDependencies) {
		this.outboundHTTP = dependencies.outboundHTTP;
	}

	private readonly concurrency = new LocalAppProxyConcurrencyGate();

	public async fetch(request: LocalAppProxyFetchRequest): Promise<Response> {
		const permit = await this.concurrency.acquire(request.signal);
		try {
			return await this.runFetch(request, permit);
		} catch (error) {
			permit.release();
			throw error;
		}
	}

	private async runFetch(
		request: LocalAppProxyFetchRequest,
		permit: LocalAppProxyConcurrencyPermit,
	): Promise<Response> {
		const target = new URL(request.targetURL);
		const body = requestHasBody(request.method) ? request.body : null;
		const outboundRequest: DesktopOutboundHTTPRequest = {
			body: body == null ? null : instrumentedRequestBody(body, request),
			expectedOrigin: target.origin,
			headers: outboundHeaders(request.headers, request.acceptEncoding),
			maximumRequestBodyBytes: LOCAL_APP_PROXY_REQUEST_BODY_MAX_BYTES,
			method: request.method,
			originTrust: DesktopOriginTrust.REGISTERED,
			serviceName: LOCAL_APP_PROXY_SERVICE_NAME,
			signal: request.signal,
			timeoutMs: LOCAL_APP_PROXY_DEADLINE_MS,
			url: target.href,
		};
		return buildProxyResponse(await this.outboundHTTP.request(outboundRequest), request.method, permit);
	}

	public async settleFailure({
		response,
		failure,
		description,
	}: LocalAppProxyFailureSettlementRequest): Promise<LocalAppProxyFailureSettlement> {
		if (response == null || response.body == null) {
			return {error: failure, bodyCancellationFailed: false};
		}
		try {
			await response.body.cancel(failure);
			return {error: failure, bodyCancellationFailed: false};
		} catch (cleanupError) {
			return {
				error: new AggregateError(
					[failure, cleanupError],
					`${description} failed and its response body could not be cancelled`,
				),
				bodyCancellationFailed: true,
			};
		}
	}

	public async responseBodyForMethod({
		method,
		response,
		description,
	}: LocalAppProxyResponseBodyRequest): Promise<ReadableStream<Uint8Array> | null> {
		if (method !== LOCAL_APP_PROXY_HEAD_METHOD || response.body == null) {
			return response.body;
		}
		await response.body.cancel(new LocalAppProxyHeadBodyUnusedError(description));
		return null;
	}
}

function outboundHeaders(headers: Headers, acceptEncoding: string | null): Record<string, string> {
	const values: Record<string, string> = Object.create(null) as Record<string, string>;
	headers.forEach((value, name) => {
		values[name] = value;
	});
	if (acceptEncoding != null) {
		values['Accept-Encoding'] = acceptEncoding;
	}
	return values;
}

function instrumentedRequestBody(
	body: ReadableStream<Uint8Array>,
	request: LocalAppProxyFetchRequest,
): ReadableStream<Uint8Array> {
	const uploadId = request.uploadId;
	if (uploadId == null) {
		return body;
	}
	const total = request.uploadTotalBytes;
	let loaded = 0;
	let lastEmittedBytes = 0;
	let lastEmittedAt = Date.now();
	return body.pipeThrough(
		new TransformStream<Uint8Array, Uint8Array>({
			transform(chunk, controller) {
				loaded += chunk.byteLength;
				const now = Date.now();
				const elapsed = now - lastEmittedAt;
				if (loaded - lastEmittedBytes >= UPLOAD_PROGRESS_MIN_BYTES || elapsed >= UPLOAD_PROGRESS_MIN_INTERVAL_MS) {
					lastEmittedBytes = loaded;
					lastEmittedAt = now;
					emitLocalAppUploadProgress({uploadId, loaded, total, done: false, failed: false});
				}
				controller.enqueue(chunk);
			},
			flush() {
				emitLocalAppUploadProgress({uploadId, loaded, total, done: true, failed: false});
			},
		}),
	);
}

function buildProxyResponse(
	message: DesktopOutboundHTTPMessage,
	method: string,
	permit: LocalAppProxyConcurrencyPermit,
): Response {
	try {
		const status = message.status;
		if (status < MINIMUM_RESPONSE_STATUS || status > MAXIMUM_RESPONSE_STATUS) {
			throw new LocalAppProxyUpstreamStatusError(status);
		}
		const headers = responseHeaders(message.headers);
		if (method === LOCAL_APP_PROXY_HEAD_METHOD || NULL_BODY_STATUSES.has(status)) {
			message.message.resume();
			permit.release();
			return new Response(null, {status, statusText: message.statusText, headers});
		}
		const decoded = decodedMessageStream(message.message, contentEncoding(message.headers));
		const body = holdPermitForResponseBody(Readable.toWeb(decoded) as ReadableStream<Uint8Array>, permit);
		return new Response(body, {
			status,
			statusText: message.statusText,
			headers,
		});
	} catch (error) {
		message.message.destroy();
		permit.release();
		throw error;
	}
}

function holdPermitForResponseBody(
	body: ReadableStream<Uint8Array>,
	permit: LocalAppProxyConcurrencyPermit,
): ReadableStream<Uint8Array> {
	const lease = new LocalAppProxyResponseBodyLease(body, permit);
	return new ReadableStream<Uint8Array>({
		async pull(controller) {
			try {
				const chunk = await lease.reader.read();
				if (chunk.done) {
					try {
						controller.close();
					} finally {
						lease.release();
					}
					return;
				}
				controller.enqueue(chunk.value);
			} catch (error) {
				try {
					controller.error(error);
				} finally {
					lease.release();
				}
			}
		},
		async cancel(reason) {
			try {
				await lease.reader.cancel(reason);
			} finally {
				lease.release();
			}
		},
	});
}

function responseHeaders(incoming: http.IncomingHttpHeaders): Headers {
	const headers = new Headers();
	for (const [name, value] of Object.entries(incoming)) {
		if (value == null) {
			continue;
		}
		if (Array.isArray(value)) {
			for (const entry of value) {
				headers.append(name, entry);
			}
			continue;
		}
		headers.append(name, value);
	}
	return headers;
}

function contentEncoding(incoming: http.IncomingHttpHeaders): string | null {
	const value = incoming['content-encoding'];
	if (typeof value !== 'string') {
		return null;
	}
	const normalized = value.trim().toLowerCase();
	return normalized.length === 0 ? null : normalized;
}

function decodedMessageStream(message: Readable, encoding: string | null): Readable {
	if (encoding == null || encoding === 'identity') {
		return message;
	}
	const decoder = createDecoder(encoding);
	if (decoder == null) {
		message.destroy();
		throw new LocalAppProxyUnsupportedContentEncodingError(encoding);
	}
	message.on('error', (error: Error) => {
		decoder.destroy(error);
	});
	decoder.on('error', () => {
		message.destroy();
	});
	message.pipe(decoder);
	return decoder;
}

function createDecoder(encoding: string): Transform | null {
	if (encoding === 'gzip' || encoding === 'x-gzip') {
		return zlib.createGunzip();
	}
	if (encoding === 'deflate') {
		return zlib.createInflate();
	}
	if (encoding === 'br') {
		return zlib.createBrotliDecompress();
	}
	return null;
}
