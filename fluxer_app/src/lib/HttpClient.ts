/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import Config from '~/Config';
import {ExponentialBackoff} from '~/lib/ExponentialBackoff';
import {HttpError} from '~/lib/HttpError';
import type {HttpMethod} from '~/lib/HttpTypes';
import {Logger} from '~/lib/Logger';
import type {SudoVerificationPayload} from '~/types/Sudo';
import {getElectronApiProxyBaseUrl, isCustomInstanceUrl, isElectronApiProxyUrl} from '~/utils/ApiProxyUtils';

const SUDO_MODE_HEADER = 'x-fluxer-sudo-mode-jwt';

interface Attachment {
	name: string;
	file: File | Blob;
	filename: string;
}

interface FormField {
	name: string;
	value: string;
}

export interface HttpRequestConfig {
	url: string;
	method?: HttpMethod;
	query?: Record<string, string | number | boolean | null> | URLSearchParams;
	body?: unknown;
	headers?: Record<string, string>;
	retries?: number;
	timeout?: number;
	signal?: AbortSignal;
	skipAuth?: boolean;
	skipParsing?: boolean;
	binary?: boolean;
	reason?: string;
	attachments?: Array<Attachment>;
	fields?: Array<FormField>;
	rejectWithError?: boolean;
	failImmediatelyWhenRateLimited?: boolean;
	interceptResponse?: ResponseInterceptor;
	onRateLimit?: (retryAfter: number, retry: () => void) => void;
	onRequestCreated?: (state: RequestState) => void;
	onRequestProgress?: (progress: ProgressEvent) => void;
	sudoRetry?: boolean;
	sudoApplied?: boolean;
}

type BodyWithoutBodyKey<T> = [T] extends [object] ? ('body' extends keyof T ? never : T) : T;

type HttpRequestBody = BodyWithoutBodyKey<HttpRequestConfig['body']>;

export interface HttpResponse<T = unknown> {
	ok: boolean;
	status: number;
	statusText?: string;
	headers: Record<string, string>;
	body: T;
	text?: string;
	hasErr?: boolean;
	err?: Error;
}

interface RequestState {
	abortController?: AbortController;
	request?: XMLHttpRequest;
	abort?: () => void;
}

interface RateLimitEntry {
	queue: Array<() => void>;
	retryAfterTimestamp: number;
	latestErrorMessage: string;
	timeoutId: number;
}

type InterceptorFn = (
	response: HttpResponse,
	retryWithHeaders: (
		headers: Record<string, string>,
		overrideInterceptor?: ResponseInterceptor,
	) => Promise<HttpResponse>,
	reject: (error: Error) => void,
) => boolean | Promise<HttpResponse> | undefined;

type ResponseInterceptor = InterceptorFn;

type PrepareRequestInterceptor = (state: RequestState) => void;

type SudoHandler = (config: HttpRequestConfig) => Promise<SudoVerificationPayload>;

type SudoTokenProvider = () => string | null;

type SudoTokenListener = (token: string | null) => void;

type SudoFailureHandler = (error: HttpError | HttpResponse | string | unknown) => void;

type AuthTokenProvider = () => string | null;

const RETRYABLE_STATUS_CODES = new Set([502, 504, 507, 598, 599, 522, 523, 524]);

function hasElectronProxy(): boolean {
	return getElectronApiProxyBaseUrl() !== null;
}

class HttpClient {
	private readonly log = new Logger('HttpClient');

	private baseUrl: string;
	private apiVersion: number;

	private defaultTimeoutMs = 30000;
	private defaultRetryCount = 0;

	private readonly rateLimitMap = new Map<string, RateLimitEntry>();

	private prepareRequestHandler?: PrepareRequestInterceptor;
	private responseInterceptor?: ResponseInterceptor;

	private sudoHandler?: SudoHandler;
	private sudoTokenProvider?: SudoTokenProvider;
	private sudoTokenListener?: SudoTokenListener;
	private sudoTokenInvalidator?: () => void;
	private sudoFailureHandler?: SudoFailureHandler;
	private authTokenProvider?: AuthTokenProvider;

	constructor() {
		this.baseUrl = Config.PUBLIC_BOOTSTRAP_API_ENDPOINT;
		this.apiVersion = Config.PUBLIC_API_VERSION;

		this.request = this.request.bind(this);
		this.get = this.get.bind(this);
		this.post = this.post.bind(this);
		this.put = this.put.bind(this);
		this.patch = this.patch.bind(this);
		this.delete = this.delete.bind(this);
	}

	setInterceptors(params: {prepareRequest?: PrepareRequestInterceptor; interceptResponse?: ResponseInterceptor}): void {
		this.prepareRequestHandler = params.prepareRequest;
		this.responseInterceptor = params.interceptResponse;
	}

	setBaseUrl(baseUrl: string, apiVersion?: number): void {
		this.baseUrl = baseUrl;
		if (typeof apiVersion === 'number') {
			this.apiVersion = apiVersion;
		}
	}

	setSudoHandler(handler?: SudoHandler): void {
		this.sudoHandler = handler;
	}

	setSudoTokenProvider(provider?: SudoTokenProvider): void {
		this.sudoTokenProvider = provider;
	}

	setSudoTokenListener(listener?: SudoTokenListener): void {
		this.sudoTokenListener = listener;
	}

	setSudoTokenInvalidator(invalidator?: () => void): void {
		this.sudoTokenInvalidator = invalidator;
	}

	setSudoFailureHandler(handler?: SudoFailureHandler): void {
		this.sudoFailureHandler = handler;
	}

	setAuthTokenProvider(provider?: AuthTokenProvider): void {
		this.authTokenProvider = provider;
	}

	setDefaults(options: {timeout?: number; retries?: number} = {}): void {
		if (typeof options.timeout === 'number') {
			this.defaultTimeoutMs = options.timeout;
		}
		if (typeof options.retries === 'number') {
			this.defaultRetryCount = options.retries;
		}
	}

	async request<T = unknown>(method: HttpMethod, urlOrConfig: string | HttpRequestConfig): Promise<HttpResponse<T>> {
		const config: HttpRequestConfig = typeof urlOrConfig === 'string' ? {url: urlOrConfig} : urlOrConfig;
		return this.executeRequest<T>(method, config);
	}

	async get<T = unknown>(urlOrConfig: string | HttpRequestConfig): Promise<HttpResponse<T>> {
		return this.request<T>('GET', urlOrConfig);
	}

	async post<T = unknown>(urlOrConfig: string | HttpRequestConfig, data?: HttpRequestBody): Promise<HttpResponse<T>> {
		return this.request<T>('POST', this.normalizeConfig(urlOrConfig, data));
	}

	async put<T = unknown>(urlOrConfig: string | HttpRequestConfig, data?: HttpRequestBody): Promise<HttpResponse<T>> {
		return this.request<T>('PUT', this.normalizeConfig(urlOrConfig, data));
	}

	async patch<T = unknown>(urlOrConfig: string | HttpRequestConfig, data?: HttpRequestBody): Promise<HttpResponse<T>> {
		return this.request<T>('PATCH', this.normalizeConfig(urlOrConfig, data));
	}

	async delete<T = unknown>(urlOrConfig: string | HttpRequestConfig): Promise<HttpResponse<T>> {
		return this.request<T>('DELETE', urlOrConfig);
	}

	private normalizeConfig(urlOrConfig: string | HttpRequestConfig, body?: HttpRequestBody): HttpRequestConfig {
		if (typeof urlOrConfig === 'string') {
			return {url: urlOrConfig, body};
		}
		return {...urlOrConfig, body: body ?? urlOrConfig.body};
	}

	private resolveRequestUrl(
		path: string,
		query?: Record<string, string | number | boolean | null> | URLSearchParams,
	): string {
		if (path.startsWith('//') || path.includes('://')) {
			return path;
		}

		const base = `${this.baseUrl}/v${this.apiVersion}${path}`;
		if (!query) return base;

		const params =
			query instanceof URLSearchParams
				? query
				: new URLSearchParams(
						Object.entries(query)
							.filter(([, value]) => value != null)
							.map(([key, value]) => [key, String(value)]),
					);

		const queryString = params.toString();
		return queryString ? `${base}?${queryString}` : base;
	}

	private buildRequestHeaders(config: HttpRequestConfig, retryCount: number): Record<string, string> {
		const headers: Record<string, string> = {...(config.headers ?? {})};

		if (!config.skipAuth && !config.url.includes('://')) {
			const authToken = this.authTokenProvider?.();
			if (authToken) {
				headers.Authorization = authToken;
			}
		}

		if (config.reason) {
			headers['X-Audit-Log-Reason'] = encodeURIComponent(config.reason);
		}

		if (retryCount > 0) {
			headers['X-Failed-Requests'] = String(retryCount);
		}

		if (config.body && !headers['Content-Type'] && !(config.body instanceof FormData)) {
			headers['Content-Type'] = 'application/json';
		}

		const sudoToken = this.sudoTokenProvider?.();
		if (sudoToken) {
			headers[SUDO_MODE_HEADER] = sudoToken;
		}

		return headers;
	}

	private buildFormData(config: HttpRequestConfig): FormData | null {
		if (!config.attachments && !config.fields) {
			return null;
		}

		const form = new FormData();

		for (const attachment of config.attachments ?? []) {
			form.append(attachment.name, attachment.file, attachment.filename);
		}

		for (const field of config.fields ?? []) {
			form.append(field.name, field.value);
		}

		return form;
	}

	private serializeBody(config: HttpRequestConfig): string | FormData | Blob | ArrayBuffer | undefined {
		const form = this.buildFormData(config);
		if (form) return form;

		const {body} = config;
		if (!body) return;

		if (typeof body === 'string' || body instanceof Blob || body instanceof ArrayBuffer || body instanceof FormData) {
			return body;
		}

		return JSON.stringify(body);
	}

	private parseXHRResponse<T>(xhr: XMLHttpRequest, config: HttpRequestConfig): {body: T; text?: string} {
		if (config.skipParsing) {
			return {body: undefined as T};
		}

		if (xhr.status === 204) {
			return {body: undefined as T};
		}

		if (config.binary) {
			return {body: xhr.response as T};
		}

		const contentType = xhr.getResponseHeader('content-type') || '';
		const text = xhr.responseText;

		if (contentType.includes('application/json')) {
			if (!text) {
				return {body: undefined as T};
			}

			try {
				return {body: JSON.parse(text) as T, text};
			} catch {
				return {body: text as T, text};
			}
		}

		return {body: text as T, text};
	}

	private parseXHRHeaders(xhr: XMLHttpRequest): Record<string, string> {
		const headerMap: Record<string, string> = {};
		const raw = xhr.getAllResponseHeaders();

		if (!raw) return headerMap;

		for (const line of raw.trim().split(/[\r\n]+/)) {
			const parts = line.split(': ');
			const name = parts.shift();
			const value = parts.join(': ');
			if (name) {
				headerMap[name.toLowerCase()] = value;
			}
		}

		return headerMap;
	}

	private shouldUseProxy(url: string): boolean {
		return hasElectronProxy() && !isElectronApiProxyUrl(url) && isCustomInstanceUrl(url);
	}

	private async executeViaProxy<T>(
		method: HttpMethod,
		fullUrl: string,
		headers: Record<string, string>,
		body: string | FormData | Blob | ArrayBuffer | undefined,
		config: HttpRequestConfig,
	): Promise<HttpResponse<T>> {
		const baseUrl = getElectronApiProxyBaseUrl();
		if (!baseUrl) {
			throw new Error('Electron API proxy is unavailable');
		}

		const proxyUrl = new URL(baseUrl.toString());
		proxyUrl.searchParams.set('target', fullUrl);

		const requestHeaders = new Headers();
		for (const [key, value] of Object.entries(headers)) {
			if (value != null) {
				requestHeaders.set(key, value);
			}
		}

		const initiator =
			typeof window !== 'undefined'
				? window.location.origin
				: Config.PUBLIC_PROJECT_ENV === 'canary'
					? 'https://web.canary.fluxer.app'
					: 'https://web.fluxer.app';
		requestHeaders.set('X-Fluxer-Proxy-Initiator', initiator);

		const controller = new AbortController();
		let timeoutId: ReturnType<typeof setTimeout> | undefined;
		if (config.timeout && config.timeout > 0) {
			timeoutId = setTimeout(() => controller.abort(), config.timeout);
		}

		let signalListener: (() => void) | null = null;
		if (config.signal) {
			if (config.signal.aborted) {
				controller.abort();
			} else {
				signalListener = () => controller.abort();
				config.signal.addEventListener('abort', signalListener);
			}
		}

		try {
			const response = await fetch(proxyUrl.toString(), {
				method,
				headers: requestHeaders,
				body,
				signal: controller.signal,
				credentials: 'include',
			});

			const responseHeaders: Record<string, string> = {};
			response.headers.forEach((value, name) => {
				responseHeaders[name.toLowerCase()] = value;
			});

			let parsedBody: T;
			let text: string | undefined;

			if (config.binary) {
				const arrayBuffer = await response.arrayBuffer();
				parsedBody = arrayBuffer as unknown as T;
			} else {
				text = await response.text();

				if (config.skipParsing) {
					parsedBody = undefined as T;
				} else if (response.status === 204) {
					parsedBody = undefined as T;
				} else {
					const contentType = responseHeaders['content-type'] || '';
					if (contentType.includes('application/json') && text) {
						try {
							parsedBody = JSON.parse(text) as T;
						} catch {
							parsedBody = text as T;
						}
					} else {
						parsedBody = text as T;
					}
				}
			}

			return {
				ok: response.ok,
				status: response.status,
				headers: responseHeaders,
				body: parsedBody,
				text,
			};
		} finally {
			if (timeoutId) {
				clearTimeout(timeoutId);
			}
			if (config.signal && signalListener) {
				config.signal.removeEventListener('abort', signalListener);
			}
		}
	}

	private parseRetryAfterSeconds(body: unknown): number {
		if (body && typeof body === 'object' && 'retry_after' in body) {
			const retryValue = (body as {retry_after?: number | string}).retry_after;
			const num = typeof retryValue === 'string' ? Number(retryValue) : retryValue;

			if (typeof num === 'number' && Number.isFinite(num)) {
				return num;
			}
		}

		return 5;
	}

	private parseRateLimitMessage(body: unknown): string {
		if (body && typeof body === 'object' && 'message' in body) {
			const message = (body as {message?: unknown}).message;
			if (typeof message === 'string') {
				return message;
			}
		}
		return '';
	}

	private updateRateLimitEntry(urlKey: string, response?: HttpResponse): void {
		const existing = this.rateLimitMap.get(urlKey);

		if (response?.status === 429) {
			const retryAfter = this.parseRetryAfterSeconds(response.body);
			const deadline = Date.now() + retryAfter * 1000;

			if (existing && existing.retryAfterTimestamp >= deadline) {
				this.log.debug('Rate limit already present for', urlKey);
				return;
			}

			if (existing) {
				this.log.debug('Extending rate limit for', urlKey);
				clearTimeout(existing.timeoutId);
			}

			this.log.debug(`Rate limit for ${urlKey}, retry in ${retryAfter}s`);

			const timeoutId = window.setTimeout(() => this.releaseRateLimitedQueue(urlKey), retryAfter * 1000);

			this.rateLimitMap.set(urlKey, {
				queue: existing?.queue ?? [],
				retryAfterTimestamp: deadline,
				latestErrorMessage: this.parseRateLimitMessage(response.body),
				timeoutId,
			});
		} else if (existing && existing.retryAfterTimestamp < Date.now()) {
			this.log.debug('Rate limit expired for', urlKey);
			this.releaseRateLimitedQueue(urlKey);
		}
	}

	private releaseRateLimitedQueue(urlKey: string): void {
		const entry = this.rateLimitMap.get(urlKey);
		if (!entry) {
			this.log.debug('Rate limit expired for', urlKey, 'but entry was already removed');
			return;
		}

		clearTimeout(entry.timeoutId);
		this.rateLimitMap.delete(urlKey);

		if (!entry.queue.length) {
			this.log.debug('Clearing rate-limit state for', urlKey, '(no queued jobs)');
			return;
		}

		const queued = entry.queue.splice(0);
		this.log.debug('Releasing', queued.length, 'queued requests for', urlKey);

		for (const fn of queued) {
			try {
				fn();
			} catch (error) {
				this.log.error('Error while executing queued rate-limited request for', urlKey, error);
			}
		}
	}

	private shouldRetryStatus(
		status: number | undefined,
		retryCount: number | undefined,
		maxRetries: number | undefined,
	): boolean {
		if (retryCount === undefined || maxRetries === undefined) return false;
		if (retryCount >= maxRetries) return false;
		return status !== undefined && RETRYABLE_STATUS_CODES.has(status);
	}

	private ensureBackoff(backoff?: ExponentialBackoff): ExponentialBackoff {
		if (backoff) return backoff;

		return new ExponentialBackoff({
			minDelay: 1000,
			maxDelay: 30000,
			jitter: true,
		});
	}

	private async executeRequest<T>(
		method: HttpMethod,
		config: HttpRequestConfig,
		retryCount = 0,
		backoff?: ExponentialBackoff,
	): Promise<HttpResponse<T>> {
		const effectiveConfig: HttpRequestConfig = {
			...config,
			method,
			timeout: config.timeout !== undefined ? config.timeout : this.defaultTimeoutMs,
			retries: config.retries !== undefined ? config.retries : this.defaultRetryCount,
		};

		const rateLimit = this.rateLimitMap.get(effectiveConfig.url);

		if (rateLimit) {
			if (effectiveConfig.failImmediatelyWhenRateLimited) {
				const secondsRemaining = Math.max(0, Math.round((rateLimit.retryAfterTimestamp - Date.now()) / 1000));

				return {
					ok: false,
					status: 429,
					headers: {},
					body: {
						message: rateLimit.latestErrorMessage,
						retry_after: secondsRemaining,
					} as T,
					text: '',
				};
			}

			this.log.debug('Queueing rate-limited request for', effectiveConfig.url);

			return new Promise<HttpResponse<T>>((resolve, reject) => {
				rateLimit.queue.push(() => {
					this.executeRequest<T>(method, effectiveConfig, retryCount, backoff).then(resolve, reject);
				});
			});
		}

		const requestState: RequestState = {};
		effectiveConfig.onRequestCreated?.(requestState);
		this.prepareRequestHandler?.(requestState);

		try {
			const headers = this.buildRequestHeaders(effectiveConfig, retryCount);
			const body = this.serializeBody(effectiveConfig);
			const fullUrl = this.resolveRequestUrl(effectiveConfig.url, effectiveConfig.query);

			const viaProxy = this.shouldUseProxy(fullUrl);
			let response: HttpResponse<T>;

			if (viaProxy) {
				response = await this.executeViaProxy<T>(method, fullUrl, headers, body, effectiveConfig);
			} else {
				response = await this.performXHRRequest<T>(method, fullUrl, headers, body, effectiveConfig, requestState);
			}

			if (this.shouldRetryStatus(response.status, retryCount, effectiveConfig.retries)) {
				const retryBackoff = this.ensureBackoff(backoff);
				await new Promise((resolve) => setTimeout(resolve, retryBackoff.next()));
				return this.executeRequest<T>(method, effectiveConfig, retryCount + 1, retryBackoff);
			}

			this.updateRateLimitEntry(effectiveConfig.url, response);

			const sudoHeader = response.headers[SUDO_MODE_HEADER];

			if (this.sudoTokenListener && response.ok) {
				if (sudoHeader) {
					this.sudoTokenListener(sudoHeader);
				} else if (effectiveConfig.sudoApplied) {
					this.sudoTokenListener(null);
				}
			}

			let chainedRequest: Promise<HttpResponse<T>> | null = null;

			const retryWithHeaders = (
				overrideHeaders: Record<string, string>,
				overrideInterceptor?: ResponseInterceptor,
			): Promise<HttpResponse<T>> => {
				const nextConfig: HttpRequestConfig = {
					...effectiveConfig,
					headers: {...effectiveConfig.headers, ...overrideHeaders},
				};

				if (overrideInterceptor) {
					nextConfig.interceptResponse = overrideInterceptor;
				}

				chainedRequest = this.executeRequest<T>(method, nextConfig, retryCount, backoff);

				return chainedRequest;
			};

			const rejectIntercepted = (error: Error) => {
				throw error;
			};

			if (effectiveConfig.interceptResponse) {
				const result = effectiveConfig.interceptResponse(response, retryWithHeaders, rejectIntercepted);

				if (result instanceof Promise) {
					return result as Promise<HttpResponse<T>>;
				}

				if (result === true) {
					return chainedRequest ?? response;
				}
			}

			if (this.responseInterceptor) {
				const result = this.responseInterceptor(response, retryWithHeaders, rejectIntercepted);

				if (result instanceof Promise) {
					return result as Promise<HttpResponse<T>>;
				}

				if (result === true) {
					return chainedRequest ?? response;
				}
			}

			if (!response.ok && effectiveConfig.rejectWithError !== false) {
				throw new HttpError({
					method,
					url: effectiveConfig.url,
					ok: response.ok,
					status: response.status,
					body: response.body,
					text: response.text,
					headers: response.headers,
				});
			}

			return response;
		} catch (error) {
			const urlKey = effectiveConfig.url;

			if (
				error instanceof HttpError &&
				error.status === 403 &&
				!effectiveConfig.sudoRetry &&
				this.isSudoRequiredError(error)
			) {
				if (this.sudoTokenInvalidator) {
					this.sudoTokenInvalidator();
				}

				if (this.sudoHandler) {
					const sudoPayload = await this.sudoHandler(effectiveConfig);

					if (sudoPayload) {
						const retryConfig = this.buildSudoRetryConfig(effectiveConfig, sudoPayload);

						try {
							return await this.executeRequest<T>(method, retryConfig, retryCount, backoff);
						} catch (retryError) {
							if (this.sudoFailureHandler) {
								this.sudoFailureHandler(retryError);
							}

							if (this.sudoHandler) {
								const nextPayload = await this.sudoHandler(effectiveConfig);
								if (nextPayload) {
									const nextConfig = this.buildSudoRetryConfig(effectiveConfig, nextPayload);
									return await this.executeRequest<T>(method, nextConfig, retryCount, backoff);
								}
							}

							throw retryError;
						}
					}
				}
			} else if (effectiveConfig.sudoApplied && this.sudoHandler) {
				if (this.sudoFailureHandler) {
					this.sudoFailureHandler(error);
				}

				const retryPayload = await this.sudoHandler(effectiveConfig);
				if (retryPayload) {
					const retryConfig = this.buildSudoRetryConfig(effectiveConfig, retryPayload);
					return this.executeRequest<T>(method, retryConfig, retryCount, backoff);
				}
			}

			this.updateRateLimitEntry(urlKey);

			if (
				!(error instanceof HttpError) &&
				error instanceof Error &&
				error.name !== 'AbortError' &&
				effectiveConfig.retries &&
				retryCount < effectiveConfig.retries
			) {
				const retryBackoff = this.ensureBackoff(backoff);
				await new Promise((resolve) => setTimeout(resolve, retryBackoff.next()));
				return this.executeRequest<T>(method, effectiveConfig, retryCount + 1, retryBackoff);
			}

			throw error;
		}
	}

	private performXHRRequest<T>(
		method: HttpMethod,
		fullUrl: string,
		headers: Record<string, string>,
		body: string | FormData | Blob | ArrayBuffer | undefined,
		config: HttpRequestConfig,
		state: RequestState,
	): Promise<HttpResponse<T>> {
		return new Promise<HttpResponse<T>>((resolve, reject) => {
			const xhr = new XMLHttpRequest();
			state.request = xhr;
			state.abort = () => xhr.abort();

			if (config.onRequestProgress) {
				xhr.upload.addEventListener('progress', (event) => {
					config.onRequestProgress?.(event);
				});
			}

			xhr.addEventListener('load', () => {
				const {body: parsedBody, text} = this.parseXHRResponse<T>(xhr, config);

				const response: HttpResponse<T> = {
					ok: xhr.status >= 200 && xhr.status < 300,
					status: xhr.status,
					statusText: xhr.statusText,
					headers: this.parseXHRHeaders(xhr),
					body: parsedBody,
					text,
				};

				resolve(response);
			});

			xhr.addEventListener('error', () => {
				reject(new Error('Network error during request'));
			});

			xhr.addEventListener('abort', () => {
				reject(new DOMException('Request aborted', 'AbortError'));
			});

			xhr.addEventListener('timeout', () => {
				reject(new DOMException('Request timeout', 'TimeoutError'));
			});

			if (config.signal) {
				const abortHandler = () => xhr.abort();
				config.signal.addEventListener('abort', abortHandler);

				xhr.addEventListener('loadend', () => {
					config.signal?.removeEventListener('abort', abortHandler);
				});
			}

			xhr.open(method, fullUrl);

			if (config.binary) {
				xhr.responseType = 'blob';
			}

			if (config.timeout && config.timeout > 0) {
				xhr.timeout = config.timeout;
			}

			for (const [name, value] of Object.entries(headers)) {
				xhr.setRequestHeader(name, value);
			}

			xhr.send(body as XMLHttpRequestBodyInit);
		});
	}

	private isSudoRequiredError(error: HttpError): boolean {
		const body = error.body as {code?: string} | undefined;
		return body?.code === 'SUDO_MODE_REQUIRED';
	}

	private buildSudoRetryConfig(config: HttpRequestConfig, payload: SudoVerificationPayload): HttpRequestConfig {
		return {
			...config,
			sudoRetry: true,
			sudoApplied: true,
			body: this.mergeSudoPayload(config.body, payload),
		};
	}

	private mergeSudoPayload(
		body: HttpRequestConfig['body'],
		payload: SudoVerificationPayload,
	): HttpRequestConfig['body'] {
		if (!body) {
			return payload;
		}

		if (
			typeof body === 'object' &&
			!(body instanceof FormData) &&
			!(body instanceof Blob) &&
			!(body instanceof ArrayBuffer)
		) {
			return {...(body as Record<string, unknown>), ...payload};
		}

		throw new Error('Cannot apply sudo verification to this request');
	}
}

export default new HttpClient();
export {HttpError};
