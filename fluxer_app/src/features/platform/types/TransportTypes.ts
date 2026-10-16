// SPDX-License-Identifier: AGPL-3.0-or-later

export type HttpMethod = 'GET' | 'POST' | 'PUT' | 'PATCH' | 'DELETE';

export type RestResponseFormat = 'auto' | 'json' | 'text' | 'binary' | 'none';
export type RestAuthMode = 'session' | 'none';
type BodyVariant =
	| {body?: undefined; raw?: undefined}
	| {body: unknown; raw?: undefined}
	| {body?: undefined; raw: BodyInit};

interface RestRequestOptionsBase {
	query?: Record<string, string | number | boolean | null | undefined> | URLSearchParams;
	headers?: Record<string, string>;
	reason?: string;
	auth?: RestAuthMode;
	parse?: RestResponseFormat;
	mode?: 'strict' | 'auto-retry' | 'silent';
	timeoutMs?: number;
	retries?: number;
	signal?: AbortSignal;
	onProgress?: (event: ProgressEvent) => void;
	suppressContentBlockedModal?: boolean;
}

export type RestRequestOptions = BodyVariant & RestRequestOptionsBase;

export interface RestResponse<T = unknown> {
	ok: boolean;
	status: number;
	statusText?: string;
	headers: Record<string, string>;
	body: T;
	text?: string;
}

export type RestInterceptor = (
	reply: RestResponse,
	retry: (extraHeaders: Record<string, string>) => Promise<RestResponse>,
) => Promise<RestResponse | undefined> | undefined;

export interface SudoBindings {
	tokenProvider: () => string | null;
	tokenListener: (token: string | null) => void;
	invalidate: () => void;
	prompt: (method: HttpMethod, path: string, triggeringFailure: unknown) => Promise<Record<string, unknown> | null>;
	onFailure: (error: unknown) => void;
}

export interface RestClientHooks {
	intercept?: RestInterceptor;
}

export interface RestRequestHandle {
	readonly abortController: AbortController;
	abort(): void;
}
