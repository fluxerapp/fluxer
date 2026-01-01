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

import type {Readable} from 'node:stream';
import {errors, request} from 'undici';
import {FLUXER_USER_AGENT} from '~/Constants';

interface RequestOptions {
	url: string;
	method?: 'GET' | 'POST' | 'HEAD';
	headers?: Record<string, string>;
	body?: unknown;
	signal?: AbortSignal;
	timeout?: number;
}

type UndiciResponse = Awaited<ReturnType<typeof request>>;
type ResponseBody = UndiciResponse['body'];

interface StreamResponse {
	stream: ResponseBody;
	headers: Headers;
	status: number;
	url: string;
}

interface RedirectResult {
	body: ResponseBody;
	headers: Record<string, string | Array<string>>;
	statusCode: number;
	finalUrl: string;
}

class HttpError extends Error {
	constructor(
		message: string,
		public readonly status?: number,
		public readonly response?: Response,
		public readonly isExpected = false,
	) {
		super(message);
		this.name = 'HttpError';
	}
}

// biome-ignore lint/complexity/noStaticOnlyClass: this is fine
class HttpClient {
	private static readonly DEFAULT_TIMEOUT = 30_000;
	private static readonly MAX_REDIRECTS = 5;
	private static readonly DEFAULT_HEADERS = {
		Accept: '*/*',
		'User-Agent': FLUXER_USER_AGENT,
		'Cache-Control': 'no-cache, no-store, must-revalidate',
		Pragma: 'no-cache',
	};

	private static getHeadersForUrl(_url: string, customHeaders?: Record<string, string>): Record<string, string> {
		return {...HttpClient.DEFAULT_HEADERS, ...customHeaders};
	}

	private static createCombinedController(...signals: Array<AbortSignal>): AbortController {
		const controller = new AbortController();
		for (const signal of signals) {
			if (signal.aborted) {
				controller.abort(signal.reason);
				break;
			}
			signal.addEventListener('abort', () => controller.abort(signal.reason), {once: true});
		}
		return controller;
	}

	private static createTimeoutController(timeout: number): AbortController {
		const controller = new AbortController();
		const timeoutId = setTimeout(() => controller.abort('Request timed out'), timeout);
		controller.signal.addEventListener('abort', () => clearTimeout(timeoutId), {once: true});
		return controller;
	}

	private static async handleRedirect(
		statusCode: number,
		headers: Record<string, string | Array<string>>,
		currentUrl: string,
		options: RequestOptions,
		signal: AbortSignal,
		redirectCount = 0,
	): Promise<RedirectResult> {
		if (redirectCount >= HttpClient.MAX_REDIRECTS) {
			throw new HttpError(`Maximum number of redirects (${HttpClient.MAX_REDIRECTS}) exceeded`);
		}

		if (![301, 302, 303, 307, 308].includes(statusCode)) {
			throw new HttpError(`Expected redirect status but got ${statusCode}`);
		}

		const location = headers.location;
		if (!location) {
			throw new HttpError('Received redirect response without Location header', statusCode);
		}

		const redirectUrl = new URL(Array.isArray(location) ? location[0] : location, currentUrl).toString();
		const requestHeaders = HttpClient.getHeadersForUrl(redirectUrl, options.headers);

		const redirectMethod = statusCode === 303 ? 'GET' : (options.method ?? 'GET');
		const redirectBody = statusCode === 303 ? undefined : options.body;

		const {
			statusCode: newStatusCode,
			headers: newHeaders,
			body,
		} = await request(redirectUrl, {
			method: redirectMethod,
			headers: requestHeaders,
			body: redirectBody ? JSON.stringify(redirectBody) : undefined,
			signal,
		});

		if ([301, 302, 303, 307, 308].includes(newStatusCode)) {
			return HttpClient.handleRedirect(
				newStatusCode,
				newHeaders as Record<string, string | Array<string>>,
				redirectUrl,
				options,
				signal,
				redirectCount + 1,
			);
		}

		return {
			body,
			headers: newHeaders as Record<string, string | Array<string>>,
			statusCode: newStatusCode,
			finalUrl: redirectUrl,
		};
	}

	public static async sendRequest(options: RequestOptions): Promise<StreamResponse> {
		const timeoutController = HttpClient.createTimeoutController(options.timeout ?? HttpClient.DEFAULT_TIMEOUT);
		const combinedController = options.signal
			? HttpClient.createCombinedController(options.signal, timeoutController.signal)
			: timeoutController;
		const headers = HttpClient.getHeadersForUrl(options.url, options.headers);

		try {
			const {
				statusCode,
				headers: responseHeaders,
				body,
			} = await request(options.url, {
				method: options.method ?? 'GET',
				headers,
				body: options.body ? JSON.stringify(options.body) : undefined,
				signal: combinedController.signal,
			});

			let finalBody = body;
			let finalHeaders = responseHeaders;
			let finalStatusCode = statusCode;
			let finalUrl = options.url;

			if (statusCode === 304) {
				return {
					stream: body,
					headers: new Headers(responseHeaders as Record<string, string>),
					status: statusCode,
					url: options.url,
				};
			}

			if ([301, 302, 303, 307, 308].includes(statusCode)) {
				const redirectResult = await HttpClient.handleRedirect(
					statusCode,
					responseHeaders as Record<string, string | Array<string>>,
					options.url,
					options,
					combinedController.signal,
				);
				finalBody = redirectResult.body;
				finalHeaders = redirectResult.headers;
				finalStatusCode = redirectResult.statusCode;
				finalUrl = redirectResult.finalUrl;
			}

			const headersObject = new Headers();
			for (const [key, value] of Object.entries(finalHeaders)) {
				if (Array.isArray(value)) {
					for (const v of value) {
						headersObject.append(key, v);
					}
				} else if (value) {
					headersObject.set(key, value);
				}
			}

			return {
				stream: finalBody,
				headers: headersObject,
				status: finalStatusCode,
				url: finalUrl,
			};
		} catch (error) {
			if (error instanceof HttpError) {
				throw error;
			}

			if (error instanceof errors.RequestAbortedError) {
				throw new HttpError('Request aborted', undefined, undefined, true);
			}
			if (error instanceof errors.BodyTimeoutError) {
				throw new HttpError('Request timed out', undefined, undefined, true);
			}
			if (error instanceof errors.ConnectTimeoutError) {
				throw new HttpError('Connection timeout', undefined, undefined, true);
			}
			if (error instanceof errors.SocketError) {
				throw new HttpError(`Socket error: ${error.message}`, undefined, undefined, true);
			}

			const errorMessage = error instanceof Error ? error.message : 'Request failed';
			const isNetworkError =
				error instanceof Error &&
				(error.message.includes('ENOTFOUND') ||
					error.message.includes('ECONNREFUSED') ||
					error.message.includes('ECONNRESET') ||
					error.message.includes('ETIMEDOUT') ||
					error.message.includes('EAI_AGAIN') ||
					error.message.includes('EHOSTUNREACH') ||
					error.message.includes('ENETUNREACH'));

			throw new HttpError(errorMessage, undefined, undefined, isNetworkError);
		}
	}

	public static async streamToString(stream: Readable): Promise<string> {
		const chunks: Array<Uint8Array> = [];
		for await (const chunk of stream) {
			chunks.push(new Uint8Array(Buffer.from(chunk)));
		}
		return Buffer.concat(chunks.map((chunk) => Buffer.from(chunk))).toString('utf-8');
	}
}

export const {sendRequest, streamToString} = HttpClient;
