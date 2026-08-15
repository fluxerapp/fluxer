// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHttpClient} from '@pkgs/http_client/src/HttpClient';
import type {HttpClient, RequestOptions, RequestUrlPolicy, StreamResponse} from '@pkgs/http_client/src/HttpClientTypes';
import {createPublicInternetRequestUrlPolicy} from '@pkgs/http_client/src/PublicInternetRequestUrlPolicy';

const requestUrlPolicy = createPublicInternetRequestUrlPolicy();
const client: HttpClient = createHttpClient({
	userAgent: 'fluxer-api',
	requestUrlPolicy,
});
const scopedClients = new Map<RequestUrlPolicy, Map<number, HttpClient>>();

interface SendRequestOptions {
	maxRedirects?: number;
	requestUrlPolicy?: RequestUrlPolicy;
}

function getHttpClientForRequest(options?: SendRequestOptions): HttpClient {
	const policy = options?.requestUrlPolicy ?? requestUrlPolicy;
	const maxRedirects = options?.maxRedirects ?? 0;
	if (policy === requestUrlPolicy && maxRedirects === 0) {
		return client;
	}
	let clientsForPolicy = scopedClients.get(policy);
	if (!clientsForPolicy) {
		clientsForPolicy = new Map<number, HttpClient>();
		scopedClients.set(policy, clientsForPolicy);
	}
	const existingClient = clientsForPolicy.get(maxRedirects);
	if (existingClient) {
		return existingClient;
	}
	const scopedClient = createHttpClient({
		userAgent: 'fluxer-api',
		...(maxRedirects > 0 ? {maxRedirects} : {}),
		requestUrlPolicy: policy,
	});
	clientsForPolicy.set(maxRedirects, scopedClient);
	return scopedClient;
}

export async function sendRequest(opts: RequestOptions, options?: SendRequestOptions) {
	const requestClient = getHttpClientForRequest(options);
	return requestClient.sendRequest(opts);
}

export class ResponseBodyTooLargeError extends Error {
	constructor(
		message: string,
		readonly maxBytes: number,
		readonly actualBytes: number | null = null,
	) {
		super(message);
		this.name = 'ResponseBodyTooLargeError';
	}
}

interface StreamToStringWithLimitOptions {
	maxBytes: number;
	headers?: Headers;
	url?: string;
	description?: string;
	signal?: AbortSignal;
}

function parseContentLength(value: string | null | undefined): number | null {
	if (!value) {
		return null;
	}
	const trimmed = value.trim();
	if (!/^\d+$/.test(trimmed)) {
		return null;
	}
	const parsed = Number.parseInt(trimmed, 10);
	return Number.isSafeInteger(parsed) ? parsed : null;
}

function createResponseBodyTooLargeError(
	options: StreamToStringWithLimitOptions,
	actualBytes: number | null,
): ResponseBodyTooLargeError {
	const description = options.description ?? 'Response body';
	const source = options.url ? ` from ${options.url}` : '';
	const actual = actualBytes == null ? 'unknown size' : `${actualBytes} bytes`;
	return new ResponseBodyTooLargeError(
		`${description}${source} exceeds the ${options.maxBytes}-byte limit (${actual})`,
		options.maxBytes,
		actualBytes,
	);
}

export async function streamToBufferWithLimit(
	stream: StreamResponse['stream'],
	options: StreamToStringWithLimitOptions,
): Promise<Uint8Array> {
	if (!stream) {
		return new Uint8Array(0);
	}
	const contentLength = parseContentLength(options.headers?.get('content-length'));
	if (contentLength != null && contentLength > options.maxBytes) {
		const error = createResponseBodyTooLargeError(options, contentLength);
		void stream.cancel(error).catch(() => {});
		throw error;
	}
	const reader = stream.getReader();
	const chunks: Array<Uint8Array> = [];
	let totalSize = 0;
	const abortError = new DOMException('Stream read aborted', 'AbortError');
	const abortReader = () => {
		void reader.cancel(abortError).catch(() => {});
	};
	const isAlreadyAborted = options.signal?.aborted === true;
	if (!isAlreadyAborted) {
		options.signal?.addEventListener('abort', abortReader, {once: true});
	}
	try {
		if (isAlreadyAborted) {
			abortReader();
			throw abortError;
		}
		while (true) {
			if (options.signal?.aborted) {
				throw abortError;
			}
			const {done, value} = await reader.read();
			if (done) {
				break;
			}
			if (!value) {
				continue;
			}
			totalSize += value.byteLength;
			if (totalSize > options.maxBytes) {
				const error = createResponseBodyTooLargeError(options, totalSize);
				await reader.cancel(error).catch(() => {});
				throw error;
			}
			chunks.push(value);
		}
	} finally {
		options.signal?.removeEventListener('abort', abortReader);
		reader.releaseLock();
	}
	if (chunks.length === 1) {
		return chunks[0]!;
	}
	const merged = new Uint8Array(totalSize);
	let offset = 0;
	for (const chunk of chunks) {
		merged.set(chunk, offset);
		offset += chunk.byteLength;
	}
	return merged;
}

export async function streamToStringWithLimit(
	stream: StreamResponse['stream'],
	options: StreamToStringWithLimitOptions,
): Promise<string> {
	const merged = await streamToBufferWithLimit(stream, options);
	return new TextDecoder().decode(merged);
}
