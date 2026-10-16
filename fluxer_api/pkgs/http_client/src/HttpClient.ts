// SPDX-License-Identifier: AGPL-3.0-or-later

import {addAbortListener} from 'node:events';
import {finished} from 'node:stream';
import {HttpStatus, REDIRECT_STATUS_CODES} from '@fluxer/constants/src/HttpConstants';
import {
	buildRequestHeaders,
	classifyRequestError,
	createRequestSignal,
	DEFAULT_MAX_REDIRECTS,
	DEFAULT_TIMEOUT_MS,
	normalizeMaxRedirects,
	normalizeTimeoutMs,
	resolveRequestBody,
	statusToMetricLabel,
} from '@pkgs/http_client/src/HttpClientRequestInternals';
import type {HttpClientMetrics, HttpClientTelemetry} from '@pkgs/http_client/src/HttpClientTelemetryTypes';
import type {
	FetchDispatcher,
	HttpClient,
	HttpClientFactoryOptions,
	HttpMethod,
	RequestOptions,
	RequestUrlPolicy,
	RequestUrlValidationContext,
	ResponseStream,
	StreamResponse,
} from '@pkgs/http_client/src/HttpClientTypes';
import {HttpError} from '@pkgs/http_client/src/HttpError';

const DEFAULT_SERVICE_NAME = 'unknown';

interface ResolvedClientConfig {
	defaultHeaders: Headers;
	defaultTimeoutMs: number;
	maxRedirects: number;
	requestUrlPolicy?: RequestUrlPolicy;
	telemetry?: HttpClientTelemetry;
}

function createDefaultHeaders(userAgent: string, defaultHeaders?: Record<string, string>): Headers {
	return buildRequestHeaders(
		{
			Accept: '*/*',
			'User-Agent': userAgent,
			'Cache-Control': 'no-cache, no-store, must-revalidate',
			Pragma: 'no-cache',
		},
		defaultHeaders,
	);
}

function resolveClientConfig(
	userAgentOrOptions: string | HttpClientFactoryOptions,
	telemetry?: HttpClientTelemetry,
): ResolvedClientConfig {
	if (typeof userAgentOrOptions === 'string') {
		return {
			defaultHeaders: createDefaultHeaders(userAgentOrOptions),
			defaultTimeoutMs: DEFAULT_TIMEOUT_MS,
			maxRedirects: DEFAULT_MAX_REDIRECTS,
			requestUrlPolicy: undefined,
			telemetry,
		};
	}
	const maxRedirects = normalizeMaxRedirects(userAgentOrOptions.maxRedirects);
	const defaultTimeoutMs = normalizeTimeoutMs(userAgentOrOptions.defaultTimeoutMs, 'defaultTimeoutMs');
	return {
		defaultHeaders: createDefaultHeaders(userAgentOrOptions.userAgent, userAgentOrOptions.defaultHeaders),
		defaultTimeoutMs,
		maxRedirects,
		requestUrlPolicy: userAgentOrOptions.requestUrlPolicy,
		telemetry: userAgentOrOptions.telemetry,
	};
}

function createFetchInit(
	method: HttpMethod,
	headers: Headers,
	body: string | undefined,
	signal: AbortSignal,
	dispatcher: FetchDispatcher | undefined,
): RequestInit {
	return {
		method,
		headers,
		body,
		signal,
		redirect: 'manual',
		...(dispatcher ? {dispatcher} : {}),
	};
}

function parseRequestUrl(value: string, base?: string): URL {
	const url = URL.parse(value, base);
	if (!url) {
		throw new TypeError('Invalid URL');
	}
	if (url.username || url.password) {
		throw new TypeError('Request URL must not include credentials');
	}
	return url;
}

function isRedirectStatus(status: number): boolean {
	return REDIRECT_STATUS_CODES.includes(status as (typeof REDIRECT_STATUS_CODES)[number]);
}

const SENSITIVE_REDIRECT_HEADERS = new Set(['authorization', 'cookie', 'proxy-authorization']);
const BODY_RELATED_HEADERS = new Set([
	'content-encoding',
	'content-language',
	'content-length',
	'content-location',
	'content-type',
	'transfer-encoding',
]);

function shouldSwitchToGet(status: number, method: HttpMethod): boolean {
	if (status === HttpStatus.SEE_OTHER) {
		return method !== 'GET' && method !== 'HEAD';
	}
	if (status === HttpStatus.MOVED_PERMANENTLY || status === HttpStatus.FOUND) {
		return method === 'POST';
	}
	return false;
}

function buildRedirectHeaders(headers: Headers, stripSensitive: boolean, dropBodyHeaders: boolean): Headers {
	const nextHeaders = new Headers(headers);
	if (stripSensitive) {
		for (const name of SENSITIVE_REDIRECT_HEADERS) {
			nextHeaders.delete(name);
		}
	}
	if (dropBodyHeaders) {
		for (const name of BODY_RELATED_HEADERS) {
			nextHeaders.delete(name);
		}
	}
	return nextHeaders;
}

async function fetchWithRedirects(
	url: string,
	method: HttpMethod,
	headers: Headers,
	body: string | undefined,
	signal: AbortSignal,
	maxRedirects: number,
	requestUrlPolicy?: RequestUrlPolicy,
): Promise<Response> {
	let currentUrl = parseRequestUrl(url);
	let currentMethod: HttpMethod = method;
	let currentBody = body;
	let currentHeaders = headers;
	const dispatcher = requestUrlPolicy?.dispatcher;
	await validateRequestUrlPolicy(
		requestUrlPolicy,
		currentUrl,
		{
			phase: 'initial',
			redirectCount: 0,
		},
		signal,
	);
	let response = await fetch(
		currentUrl.href,
		createFetchInit(currentMethod, currentHeaders, currentBody, signal, dispatcher),
	);
	let redirectCount = 0;
	while (isRedirectStatus(response.status)) {
		await response.body?.cancel();
		if (redirectCount >= maxRedirects) {
			throw new HttpError(`Maximum number of redirects (${maxRedirects}) exceeded`);
		}
		const location = response.headers.get('location');
		if (!location) {
			throw new HttpError('Received redirect response without Location header', response.status);
		}
		const previousUrl = currentUrl;
		const nextUrl = parseRequestUrl(location, response.url || currentUrl.href);
		const switchToGet = shouldSwitchToGet(response.status, currentMethod);
		if (switchToGet) {
			currentMethod = 'GET';
			currentBody = undefined;
		}
		const previousOrigin = previousUrl.origin;
		const nextOrigin = nextUrl.origin;
		const stripSensitive = previousOrigin !== nextOrigin;
		currentHeaders = buildRedirectHeaders(currentHeaders, stripSensitive, switchToGet);
		const nextRedirectCount = redirectCount + 1;
		await validateRequestUrlPolicy(
			requestUrlPolicy,
			nextUrl,
			{
				phase: 'redirect',
				redirectCount: nextRedirectCount,
				previousUrl: previousUrl.href,
			},
			signal,
		);
		currentUrl = nextUrl;
		response = await fetch(
			currentUrl.href,
			createFetchInit(currentMethod, currentHeaders, currentBody, signal, dispatcher),
		);
		redirectCount = nextRedirectCount;
	}
	return response;
}

async function validateRequestUrlPolicy(
	requestUrlPolicy: RequestUrlPolicy | undefined,
	url: URL,
	context: RequestUrlValidationContext,
	signal: AbortSignal,
): Promise<void> {
	signal.throwIfAborted();
	if (!requestUrlPolicy) {
		return;
	}
	let abortSubscription: Disposable | undefined;
	try {
		await new Promise<void>((resolve, reject) => {
			abortSubscription = addAbortListener(signal, () => reject(signal.reason));
			void requestUrlPolicy.validate(url, context).then(resolve, reject);
		});
	} finally {
		abortSubscription?.[Symbol.dispose]();
	}
}

function recordSuccessfulRequestMetrics(
	metrics: HttpClientMetrics | undefined,
	serviceName: string,
	method: HttpMethod,
	status: number,
	durationMs: number,
): void {
	if (!metrics) {
		return;
	}
	const statusCategory = statusToMetricLabel(status);
	metrics.histogram({
		name: 'http_client.latency',
		dimensions: {service: serviceName, method},
		valueMs: durationMs,
	});
	metrics.counter({
		name: 'http_client.request',
		dimensions: {service: serviceName, method, status: statusCategory},
	});
	metrics.counter({
		name: 'http_client.response',
		dimensions: {service: serviceName, status_code: statusCategory},
	});
}

function recordHttpErrorMetrics(
	metrics: HttpClientMetrics | undefined,
	serviceName: string,
	method: HttpMethod,
	status: string,
	durationMs: number,
): void {
	if (!metrics) {
		return;
	}
	metrics.counter({
		name: 'http_client.request',
		dimensions: {service: serviceName, method, status},
	});
	metrics.histogram({
		name: 'http_client.latency',
		dimensions: {service: serviceName, method},
		valueMs: durationMs,
	});
}

function recordUnhandledErrorMetrics(
	metrics: HttpClientMetrics | undefined,
	serviceName: string,
	method: HttpMethod,
	status: string,
	errorType: string,
	durationMs: number,
): void {
	if (!metrics) {
		return;
	}
	metrics.counter({
		name: 'http_client.request',
		dimensions: {service: serviceName, method, status},
	});
	metrics.histogram({
		name: 'http_client.latency',
		dimensions: {service: serviceName, method, error_type: errorType},
		valueMs: durationMs,
	});
}

export function createHttpClient(userAgent: string, telemetry?: HttpClientTelemetry): HttpClient;
export function createHttpClient(options: HttpClientFactoryOptions): HttpClient;
export function createHttpClient(
	userAgentOrOptions: string | HttpClientFactoryOptions,
	telemetry?: HttpClientTelemetry,
): HttpClient {
	const config = resolveClientConfig(userAgentOrOptions, telemetry);
	const metrics = config.telemetry?.metrics;
	async function request(opts: RequestOptions): Promise<StreamResponse> {
		const startTime = Date.now();
		const method: HttpMethod = opts.method ?? 'GET';
		const serviceName = opts.serviceName ?? DEFAULT_SERVICE_NAME;
		const timeoutMs = normalizeTimeoutMs(opts.timeout, 'timeout', config.defaultTimeoutMs);
		const headers = buildRequestHeaders(config.defaultHeaders, opts.headers);
		const body = resolveRequestBody(opts.body, headers);
		const requestSignal = createRequestSignal(timeoutMs, opts.signal);
		let responseOwnsSignal = false;
		try {
			const response = await fetchWithRedirects(
				opts.url,
				method,
				headers,
				body,
				requestSignal.signal,
				config.maxRedirects,
				config.requestUrlPolicy,
			);
			const result: StreamResponse = {
				stream: response.body,
				headers: response.headers,
				status: response.status,
				url: response.url || opts.url,
			};
			const durationMs = Date.now() - startTime;
			recordSuccessfulRequestMetrics(metrics, serviceName, method, result.status, durationMs);
			if (result.stream) {
				const stopObserving = finished(result.stream, () => {
					stopObserving();
					requestSignal.cleanup();
				});
				responseOwnsSignal = true;
			}
			return result;
		} catch (error) {
			requestSignal.abort(error);
			const durationMs = Date.now() - startTime;
			if (error instanceof HttpError) {
				recordHttpErrorMetrics(metrics, serviceName, method, error.status?.toString() ?? 'error', durationMs);
				throw error;
			}
			const classifiedError = classifyRequestError(error);
			const status = classifiedError.isNetworkError ? 'network_error' : 'error';
			recordUnhandledErrorMetrics(metrics, serviceName, method, status, classifiedError.errorType, durationMs);
			throw new HttpError(
				classifiedError.message,
				undefined,
				undefined,
				classifiedError.isNetworkError,
				classifiedError.errorType,
			);
		} finally {
			if (!responseOwnsSignal) {
				requestSignal.cleanup();
			}
		}
	}
	async function streamToString(stream: ResponseStream): Promise<string> {
		if (!stream) {
			return '';
		}
		return new Response(stream).text();
	}
	return {
		request,
		sendRequest: request,
		streamToString,
	};
}
