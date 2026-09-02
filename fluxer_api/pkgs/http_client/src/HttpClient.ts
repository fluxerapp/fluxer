// SPDX-License-Identifier: AGPL-3.0-or-later

import {HttpStatus, REDIRECT_STATUS_CODES} from '@fluxer/constants/src/HttpConstants';
import {
	buildRequestHeaders,
	classifyRequestError,
	createRequestSignal,
	resolveRequestBody,
	statusToMetricLabel,
} from '@pkgs/http_client/src/HttpClientRequestInternals';
import type {HttpClientMetrics, HttpClientTelemetry} from '@pkgs/http_client/src/HttpClientTelemetryTypes';
import type {
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

const DEFAULT_TIMEOUT_MS = 30000;
const DEFAULT_MAX_REDIRECTS = 5;
const DEFAULT_SERVICE_NAME = 'unknown';

interface ResolvedClientConfig {
	defaultHeaders: Record<string, string>;
	defaultTimeoutMs: number;
	maxRedirects: number;
	requestUrlPolicy?: RequestUrlPolicy;
	telemetry?: HttpClientTelemetry;
}

function createDefaultHeaders(userAgent: string, defaultHeaders?: Record<string, string>): Record<string, string> {
	const headers: Record<string, string> = {
		Accept: '*/*',
		'User-Agent': userAgent,
		'Cache-Control': 'no-cache, no-store, must-revalidate',
		Pragma: 'no-cache',
	};
	if (defaultHeaders) {
		for (const [key, value] of Object.entries(defaultHeaders)) {
			headers[key] = value;
		}
	}
	return headers;
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
	const maxRedirects =
		typeof userAgentOrOptions.maxRedirects === 'number' && userAgentOrOptions.maxRedirects >= 0
			? userAgentOrOptions.maxRedirects
			: DEFAULT_MAX_REDIRECTS;
	const defaultTimeoutMs =
		typeof userAgentOrOptions.defaultTimeoutMs === 'number' && userAgentOrOptions.defaultTimeoutMs > 0
			? userAgentOrOptions.defaultTimeoutMs
			: DEFAULT_TIMEOUT_MS;
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
	headers: Record<string, string>,
	body: string | undefined,
	signal: AbortSignal,
	dispatcher: NonNullable<RequestInit['dispatcher']> | undefined,
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

function resolveRequestUrlPolicyDispatcher(
	requestUrlPolicy: RequestUrlPolicy | undefined,
): NonNullable<RequestInit['dispatcher']> | undefined {
	return (requestUrlPolicy as {dispatcher?: NonNullable<RequestInit['dispatcher']>} | undefined)?.dispatcher;
}

function isRedirectStatus(status: number): boolean {
	return REDIRECT_STATUS_CODES.includes(status as (typeof REDIRECT_STATUS_CODES)[number]);
}

const SENSITIVE_REDIRECT_HEADERS = new Set(['authorization', 'cookie', 'proxy-authorization']);
const BODY_RELATED_HEADERS = new Set(['content-type', 'content-length', 'transfer-encoding']);

function shouldSwitchToGet(status: number, method: HttpMethod): boolean {
	if (status === HttpStatus.SEE_OTHER) {
		return true;
	}
	if (status === HttpStatus.MOVED_PERMANENTLY || status === HttpStatus.FOUND) {
		return method !== 'GET' && method !== 'HEAD';
	}
	return false;
}

function buildRedirectHeaders(
	headers: Record<string, string>,
	stripSensitive: boolean,
	dropBodyHeaders: boolean,
): Record<string, string> {
	const nextHeaders: Record<string, string> = {};
	for (const [key, value] of Object.entries(headers)) {
		const lowerKey = key.toLowerCase();
		if (stripSensitive && SENSITIVE_REDIRECT_HEADERS.has(lowerKey)) {
			continue;
		}
		if (dropBodyHeaders && BODY_RELATED_HEADERS.has(lowerKey)) {
			continue;
		}
		nextHeaders[key] = value;
	}
	return nextHeaders;
}

async function fetchWithRedirects(
	url: string,
	method: HttpMethod,
	headers: Record<string, string>,
	body: string | undefined,
	signal: AbortSignal,
	maxRedirects: number,
	requestUrlPolicy?: RequestUrlPolicy,
): Promise<Response> {
	let currentUrl = new URL(url);
	let currentMethod: HttpMethod = method;
	let currentBody = body;
	let currentHeaders = {...headers};
	const dispatcher = resolveRequestUrlPolicyDispatcher(requestUrlPolicy);
	await validateRequestUrlPolicy(requestUrlPolicy, currentUrl, {
		phase: 'initial',
		redirectCount: 0,
	});
	let response = await fetch(
		currentUrl.href,
		createFetchInit(currentMethod, currentHeaders, currentBody, signal, dispatcher),
	);
	let redirectCount = 0;
	while (isRedirectStatus(response.status)) {
		if (redirectCount >= maxRedirects) {
			throw new HttpError(`Maximum number of redirects (${maxRedirects}) exceeded`);
		}
		const location = response.headers.get('location');
		if (!location) {
			throw new HttpError('Received redirect response without Location header', response.status);
		}
		const previousUrl = currentUrl;
		const nextUrl = new URL(location, response.url || currentUrl.href);
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
		await validateRequestUrlPolicy(requestUrlPolicy, nextUrl, {
			phase: 'redirect',
			redirectCount: nextRedirectCount,
			previousUrl: previousUrl.href,
		});
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
): Promise<void> {
	if (!requestUrlPolicy) {
		return;
	}
	await requestUrlPolicy.validate(url, context);
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
		const timeoutMs = typeof opts.timeout === 'number' && opts.timeout > 0 ? opts.timeout : config.defaultTimeoutMs;
		const requestSignal = createRequestSignal(timeoutMs, opts.signal);
		const headers = buildRequestHeaders(config.defaultHeaders, opts.headers);
		const body = resolveRequestBody(opts.body, headers);
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
			return result;
		} catch (error) {
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
			requestSignal.cleanup();
		}
	}
	async function sendRequest(opts: RequestOptions): Promise<StreamResponse> {
		return request(opts);
	}
	async function streamToString(stream: ResponseStream): Promise<string> {
		if (!stream) {
			return '';
		}
		return new Response(stream).text();
	}
	return {
		request,
		sendRequest,
		streamToString,
	};
}
