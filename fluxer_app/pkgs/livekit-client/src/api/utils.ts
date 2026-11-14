// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {SignalResponse} from '@livekit/protocol';
import {toHttpUrl, toWebsocketUrl} from '../room/utils.ts';

export function createRtcUrl(url: string, searchParams: URLSearchParams, useV0Path = false) {
	const v0Url = createV0RtcUrl(url, searchParams);
	if (useV0Path) {
		return v0Url;
	} else {
		return appendUrlPath(v0Url, 'v1');
	}
}

function createV0RtcUrl(url: string, searchParams: URLSearchParams) {
	const urlObj = new URL(toWebsocketUrl(url));
	searchParams.forEach((value, key) => {
		urlObj.searchParams.set(key, value);
	});
	return appendUrlPath(urlObj, 'rtc');
}

export function createValidateUrl(rtcWsUrl: string) {
	const urlObj = new URL(toHttpUrl(rtcWsUrl));
	return appendUrlPath(urlObj, 'validate');
}

export function ensureTrailingSlash(path: string) {
	return path.endsWith('/') ? path : `${path}/`;
}

function appendUrlPath(urlObj: URL, path: string) {
	urlObj.pathname = `${ensureTrailingSlash(urlObj.pathname)}${path}`;
	return urlObj;
}

export function parseSignalResponse(value: ArrayBuffer | string) {
	if (typeof value === 'string') {
		return SignalResponse.fromJson(JSON.parse(value), {ignoreUnknownFields: true});
	} else if (value instanceof ArrayBuffer) {
		return SignalResponse.fromBinary(new Uint8Array(value));
	}
	throw new Error(`could not decode websocket message: ${typeof value}`);
}

export function getAbortReasonAsString(signal: AbortSignal | Error | unknown, defaultMessage = 'Unknown reason') {
	if (signal instanceof Error) {
		return signal.message;
	}
	if (!(signal instanceof AbortSignal)) {
		return defaultMessage;
	}
	const reason = signal.reason;
	switch (typeof reason) {
		case 'string':
			return reason;
		case 'number':
		case 'bigint':
		case 'boolean':
		case 'symbol':
			return String(reason);
		case 'object':
			return reason instanceof Error ? reason.message : defaultMessage;
		default:
			return defaultMessage;
	}
}

export function getErrorDescription(error: unknown, errorCategory: string): string {
	if (error instanceof Error) {
		if (error.name && error.message) {
			return `${error.name}: ${error.message}`;
		}
		return error.name;
	}
	return `Encountered unknown ${errorCategory} error: ${String(error)}`;
}
