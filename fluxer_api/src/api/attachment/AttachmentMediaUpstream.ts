// SPDX-License-Identifier: AGPL-3.0-or-later

import {TRAILING_SLASHES_REGEX} from '@app/api/attachment/AttachmentPublicUrl';
import {signAttachmentUrl} from '@app/api/attachment/AttachmentUrls';
import {Config} from '@app/api/Config';

const UPSTREAM_HEADERS_TIMEOUT_MS = 10_000;
const MAX_DIMENSION = 8192;
const IMAGE_FORMATS = new Set([
	'png',
	'jpg',
	'jpeg',
	'webp',
	'gif',
	'apng',
	'avif',
	'heic',
	'heif',
	'jxl',
	'svg',
	'auto',
]);
const QUALITIES = new Set(['low', 'high', 'lossless', 'auto']);
const BOOLEANS = new Set(['true', 'false', '1', '0']);
const FORWARDED_REQUEST_HEADERS = ['range', 'if-range', 'if-none-match', 'if-modified-since'] as const;

type ParamCheck = (value: string) => boolean;

function isDimension(value: string): boolean {
	if (!/^[0-9]{1,5}$/u.test(value)) return false;
	const parsed = Number(value);
	return parsed >= 1 && parsed <= MAX_DIMENSION;
}

const TRANSFORM_PARAMS: ReadonlyMap<string, ParamCheck> = new Map<string, ParamCheck>([
	['width', isDimension],
	['height', isDimension],
	['format', (value) => IMAGE_FORMATS.has(value.toLowerCase())],
	['fmt', (value) => IMAGE_FORMATS.has(value.toLowerCase())],
	['quality', (value) => QUALITIES.has(value)],
	['animated', (value) => BOOLEANS.has(value.toLowerCase())],
	['effort', (value) => /^[0-9]$/u.test(value)],
	['download', (value) => BOOLEANS.has(value.toLowerCase())],
]);

export type TransformQuery = Array<[string, string]>;

export function readTransformQuery(params: URLSearchParams): TransformQuery | null {
	const query: TransformQuery = [];
	for (const [name, check] of TRANSFORM_PARAMS) {
		const values = params.getAll(name);
		if (values.length === 0) continue;
		if (values.length > 1 || !check(values[0]!)) return null;
		query.push([name, values[0]!]);
	}
	return query;
}

export function encodeStorageKey(storageKey: string): string {
	return storageKey.split('/').map(encodeURIComponent).join('/');
}

function attachmentUpstreamUrl(storageKey: string, query: TransformQuery): string {
	const mediaEndpoint = Config.endpoints.media.replace(TRAILING_SLASHES_REGEX, '');
	const search = new URLSearchParams(query).toString();
	const publicUrl = `${mediaEndpoint}/${encodeStorageKey(storageKey)}${search ? `?${search}` : ''}`;
	const signed = signAttachmentUrl(publicUrl);
	return `http://${Config.mediaProxy.host}:${Config.mediaProxy.port}${signed.slice(mediaEndpoint.length)}`;
}

export function fetchAttachmentFromMediaProxy(params: {
	storageKey: string;
	query: TransformQuery;
	method: 'GET' | 'HEAD';
	headers: Headers;
	signal: AbortSignal;
}): Promise<Response> {
	const headers = new Headers({'accept-encoding': 'identity'});
	for (const name of FORWARDED_REQUEST_HEADERS) {
		const value = params.headers.get(name);
		if (value !== null) headers.set(name, value);
	}
	const controller = new AbortController();
	const abort = () => controller.abort(params.signal.reason);
	if (params.signal.aborted) abort();
	params.signal.addEventListener('abort', abort, {once: true});
	const timeout = setTimeout(
		() => controller.abort(new Error('Media proxy did not answer in time')),
		UPSTREAM_HEADERS_TIMEOUT_MS,
	);
	return fetch(attachmentUpstreamUrl(params.storageKey, params.query), {
		method: params.method,
		headers,
		redirect: 'error',
		signal: controller.signal,
	}).finally(() => clearTimeout(timeout));
}
