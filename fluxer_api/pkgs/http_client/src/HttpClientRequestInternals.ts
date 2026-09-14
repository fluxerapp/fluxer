// SPDX-License-Identifier: AGPL-3.0-or-later

import {addAbortListener} from 'node:events';
import type {HttpErrorType} from '@pkgs/http_client/src/HttpError';

export const DEFAULT_TIMEOUT_MS = 30000;
export const DEFAULT_MAX_REDIRECTS = 5;
const MAX_TIMEOUT_MS = 2 ** 31 - 1;

const NETWORK_ERROR_CODES = new Set([
	'ENOTFOUND',
	'ECONNREFUSED',
	'ECONNRESET',
	'ETIMEDOUT',
	'EAI_AGAIN',
	'EHOSTUNREACH',
	'ENETUNREACH',
]);
const NETWORK_ERROR_MESSAGE_FRAGMENTS = [...NETWORK_ERROR_CODES, 'fetch failed'];

interface NodeErrorLike {
	code?: unknown;
	cause?: unknown;
}

interface ClassifiedRequestError {
	message: string;
	isNetworkError: boolean;
	errorType: HttpErrorType;
}

interface RequestSignalContext {
	signal: AbortSignal;
	abort(reason: unknown): void;
	cleanup(): void;
}

export function normalizeMaxRedirects(value: number | undefined): number {
	const maxRedirects = value === undefined ? DEFAULT_MAX_REDIRECTS : value;
	if (!Number.isSafeInteger(maxRedirects) || maxRedirects < 0) {
		throw new RangeError('maxRedirects must be a nonnegative safe integer');
	}
	return maxRedirects;
}

export function normalizeTimeoutMs(
	value: number | undefined,
	setting: 'defaultTimeoutMs' | 'timeout',
	defaultTimeoutMs = DEFAULT_TIMEOUT_MS,
): number {
	const timeoutMs = value === undefined ? defaultTimeoutMs : value;
	if (!Number.isInteger(timeoutMs) || timeoutMs < 1 || timeoutMs > MAX_TIMEOUT_MS) {
		throw new RangeError(`${setting} must be an integer between 1 and 2147483647`);
	}
	return timeoutMs;
}

function isNodeErrorLike(error: unknown): error is NodeErrorLike {
	return typeof error === 'object' && error !== null;
}

function resolveErrorCode(error: unknown): string | undefined {
	if (!isNodeErrorLike(error)) {
		return undefined;
	}
	if (typeof error.code === 'string') {
		return error.code;
	}
	if (isNodeErrorLike(error.cause) && typeof error.cause.code === 'string') {
		return error.cause.code;
	}
	return undefined;
}

export function buildRequestHeaders(
	defaultHeaders: Headers | Record<string, string>,
	requestHeaders?: Record<string, string>,
): Headers {
	const headers = new Headers(defaultHeaders);
	for (const [name, value] of Object.entries(requestHeaders ?? {})) {
		headers.set(name, value);
	}
	return headers;
}

export function resolveRequestBody(body: unknown, headers: Headers): string | undefined {
	if (body === null || body === undefined) {
		return undefined;
	}
	if (body instanceof URLSearchParams) {
		if (!headers.get('content-type')) {
			headers.set('content-type', 'application/x-www-form-urlencoded;charset=UTF-8');
		}
		return body.toString();
	}
	if (typeof body === 'string') {
		return body;
	}
	if (!headers.get('content-type')) {
		headers.set('content-type', 'application/json');
	}
	return JSON.stringify(body);
}

export function createRequestSignal(timeoutMs: number, inputSignal?: AbortSignal): RequestSignalContext {
	const controller = new AbortController();
	const timeoutId = setTimeout(() => {
		controller.abort('Request timed out');
	}, timeoutMs);
	timeoutId.unref();
	let inputSubscription: Disposable | undefined;
	if (inputSignal?.aborted) {
		controller.abort(inputSignal.reason);
	} else if (inputSignal) {
		inputSubscription = addAbortListener(inputSignal, () => controller.abort(inputSignal.reason));
	}
	return {
		signal: controller.signal,
		abort: (reason) => controller.abort(reason),
		cleanup: () => {
			clearTimeout(timeoutId);
			inputSubscription?.[Symbol.dispose]();
		},
	};
}

export function classifyRequestError(error: unknown): ClassifiedRequestError {
	const message = error instanceof Error ? error.message : 'Request failed';
	if (error instanceof Error && error.name === 'AbortError') {
		return {
			message,
			isNetworkError: false,
			errorType: 'aborted',
		};
	}
	const errorCode = resolveErrorCode(error);
	const containsNetworkErrorMessage = NETWORK_ERROR_MESSAGE_FRAGMENTS.some((fragment) => message.includes(fragment));
	const isNetworkError =
		(errorCode !== undefined && NETWORK_ERROR_CODES.has(errorCode)) ||
		containsNetworkErrorMessage ||
		error instanceof TypeError;
	if (isNetworkError) {
		return {
			message,
			isNetworkError: true,
			errorType: 'network_error',
		};
	}
	return {
		message,
		isNetworkError: false,
		errorType: 'unknown',
	};
}

export function statusToMetricLabel(status: number): string {
	if (status >= 200 && status < 300) {
		return '2xx';
	}
	return status.toString();
}
