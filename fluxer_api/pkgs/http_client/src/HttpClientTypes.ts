// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HttpClientTelemetry} from '@pkgs/http_client/src/HttpClientTelemetryTypes';
import type {Dispatcher} from 'undici-types';

export type ResponseStream = ReadableStream<Uint8Array> | null;
export type HttpMethod = 'GET' | 'POST' | 'HEAD' | 'PUT' | 'PATCH' | 'DELETE' | 'OPTIONS';
export type RequestUrlValidationPhase = 'initial' | 'redirect';

export interface RequestUrlValidationContext {
	phase: RequestUrlValidationPhase;
	redirectCount: number;
	previousUrl?: string;
}

export interface RequestUrlPolicy {
	readonly dispatcher?: Dispatcher;
	validate(url: URL, context: RequestUrlValidationContext): Promise<void>;
}

export interface RequestOptions {
	url: string;
	method?: HttpMethod;
	headers?: Record<string, string>;
	body?: unknown;
	signal?: AbortSignal;
	timeout?: number;
	serviceName?: string;
}

export interface StreamResponse {
	stream: ResponseStream;
	headers: Headers;
	status: number;
	url: string;
}

export interface HttpClient {
	request(opts: RequestOptions): Promise<StreamResponse>;
	sendRequest(opts: RequestOptions): Promise<StreamResponse>;
	streamToString(stream: ResponseStream): Promise<string>;
}

export interface HttpClientFactoryOptions {
	userAgent: string;
	telemetry?: HttpClientTelemetry;
	defaultHeaders?: Record<string, string>;
	defaultTimeoutMs?: number;
	maxRedirects?: number;
	requestUrlPolicy?: RequestUrlPolicy;
}
