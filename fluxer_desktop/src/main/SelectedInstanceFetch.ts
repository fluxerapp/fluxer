// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Buffer} from 'node:buffer';
import type http from 'node:http';
import {
	DesktopOriginTrust,
	type DesktopOutboundHTTP,
	getDesktopOutboundHTTP,
	readBoundedMessage,
	readMessageContentLength,
} from '@electron/main/DesktopOutboundHTTP';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';

const INSTANCE_RESPONSE_MAX_BYTES = 1024 * 1024;
const INSTANCE_RESPONSE_MAX_CHUNKS = 1024;
const INSTANCE_FETCH_TIMEOUT_MS = 15_000;

const SELECTED_INSTANCE_SERVICE_NAME = 'desktop_selected_instance';
const INSTANCE_RESPONSE_DESCRIPTION = 'Instance response';
const RESET_CONTENT_STATUS = 205;

interface SelectedInstanceFetchRequest {
	readonly body?: Uint8Array | null;
	readonly expectedOrigin: string;
	readonly headers?: Readonly<Record<string, string>> | null;
	readonly method?: string | null;
	readonly signal?: AbortSignal | null;
	readonly timeoutMs?: number | null;
	readonly url: string;
}

interface DesktopInstanceResponse {
	readonly body: Buffer | null;
	readonly headers: http.IncomingHttpHeaders;
	readonly ok: boolean;
	readonly status: number;
	readonly statusText: string;
}

class InstanceRedirectRefusedError extends Error {
	public readonly status: number;

	public constructor(url: string, status: number) {
		super(`Instance request to ${url} was answered with an HTTP ${status} redirect, which is never followed`);
		this.name = 'InstanceRedirectRefusedError';
		this.status = status;
	}
}

class InvalidInstanceTimeoutError extends RangeError {
	public constructor(timeoutMs: number) {
		super(`Instance request timeout ${timeoutMs} must be a positive safe integer`);
		this.name = 'InvalidInstanceTimeoutError';
	}
}

function hasNoBody(method: string, status: number): boolean {
	if (method === 'HEAD') {
		return true;
	}
	return status === HttpStatus.NO_CONTENT || status === RESET_CONTENT_STATUS || status === HttpStatus.NOT_MODIFIED;
}

export class DesktopSelectedInstanceClient {
	private readonly outboundHTTP: DesktopOutboundHTTP;

	public constructor(outboundHTTP: DesktopOutboundHTTP) {
		this.outboundHTTP = outboundHTTP;
	}

	public async fetch(request: SelectedInstanceFetchRequest): Promise<DesktopInstanceResponse> {
		const timeoutMs = request.timeoutMs ?? INSTANCE_FETCH_TIMEOUT_MS;
		if (!Number.isSafeInteger(timeoutMs) || timeoutMs <= 0) {
			throw new InvalidInstanceTimeoutError(timeoutMs);
		}
		const method = (request.method ?? 'GET').toUpperCase();
		const message = await this.outboundHTTP.request({
			body: request.body ?? null,
			expectedOrigin: request.expectedOrigin,
			headers: request.headers ?? null,
			method,
			originTrust: DesktopOriginTrust.BOUND,
			serviceName: SELECTED_INSTANCE_SERVICE_NAME,
			signal: request.signal ?? null,
			timeoutMs,
			url: request.url,
		});
		if (message.status >= 300 && message.status < 400) {
			message.message.destroy();
			throw new InstanceRedirectRefusedError(request.url, message.status);
		}
		let body: Buffer | null = null;
		if (hasNoBody(method, message.status)) {
			message.message.resume();
		} else {
			body = await readBoundedMessage({
				declaredBytes: readMessageContentLength(message, INSTANCE_RESPONSE_DESCRIPTION),
				description: INSTANCE_RESPONSE_DESCRIPTION,
				maxBytes: INSTANCE_RESPONSE_MAX_BYTES,
				maxChunks: INSTANCE_RESPONSE_MAX_CHUNKS,
				message: message.message,
			});
		}
		return {
			body,
			headers: message.headers,
			ok: message.status >= 200 && message.status < 300,
			status: message.status,
			statusText: message.statusText,
		};
	}
}

let sharedSelectedInstanceClient: DesktopSelectedInstanceClient | null = null;

export function getDesktopSelectedInstanceClient(): DesktopSelectedInstanceClient {
	sharedSelectedInstanceClient ??= new DesktopSelectedInstanceClient(getDesktopOutboundHTTP());
	return sharedSelectedInstanceClient;
}
