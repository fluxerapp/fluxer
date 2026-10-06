// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {FluxerError} from '@fluxer/errors/src/FluxerError';
import {HTTPException} from 'hono/http-exception';

const apiErrorCodeSet = new Set<string>(Object.values(APIErrorCodes));

const HTTP_STATUS_TO_ERROR_CODE: Partial<Record<number, string>> = {
	400: APIErrorCodes.BAD_REQUEST,
	403: APIErrorCodes.FORBIDDEN,
	404: APIErrorCodes.NOT_FOUND,
	405: APIErrorCodes.METHOD_NOT_ALLOWED,
	409: APIErrorCodes.CONFLICT,
	410: APIErrorCodes.GONE,
	500: APIErrorCodes.INTERNAL_SERVER_ERROR,
	501: APIErrorCodes.NOT_IMPLEMENTED,
	502: APIErrorCodes.BAD_GATEWAY,
	503: APIErrorCodes.SERVICE_UNAVAILABLE,
	504: APIErrorCodes.GATEWAY_TIMEOUT,
};

export function getApiErrorCodeForStatus(status: number): string {
	return HTTP_STATUS_TO_ERROR_CODE[status] ?? APIErrorCodes.GENERAL_ERROR;
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null;
}

function isApiErrorCode(value: unknown): value is string {
	return typeof value === 'string' && apiErrorCodeSet.has(value);
}

export function getErrorRecord(err: unknown): Record<string, unknown> | null {
	if (!isRecord(err)) {
		return null;
	}
	return err;
}

export function resolveApiErrorCode(err: unknown): string | null {
	if (err instanceof FluxerError) {
		return err.code;
	}
	const record = getErrorRecord(err);
	if (!record) {
		return null;
	}
	if (isApiErrorCode(record.code)) {
		return record.code;
	}
	if (isApiErrorCode(record.message)) {
		return record.message;
	}
	return null;
}

export function resolveErrorStatus(err: unknown): number | null {
	if (err instanceof FluxerError || err instanceof HTTPException) {
		return err.status;
	}
	const record = getErrorRecord(err);
	if (!record || typeof record.status !== 'number') {
		return null;
	}
	return record.status >= 100 && record.status <= 599 ? record.status : null;
}

export function resolveErrorData(err: unknown): Record<string, unknown> | undefined {
	const record = getErrorRecord(err);
	if (!record) {
		return undefined;
	}
	return isRecord(record.data) ? record.data : undefined;
}

export function resolveErrorHeaders(err: unknown): Record<string, string> | undefined {
	const record = getErrorRecord(err);
	if (!record || !isRecord(record.headers)) {
		return undefined;
	}
	const headers: Record<string, string> = {};
	for (const [key, value] of Object.entries(record.headers)) {
		if (typeof value === 'string') {
			headers[key] = value;
		}
	}
	return Object.keys(headers).length > 0 ? headers : undefined;
}

export function resolveMessageVariables(err: unknown): Record<string, unknown> | undefined {
	const record = getErrorRecord(err);
	if (!record) {
		return undefined;
	}
	return isRecord(record.messageVariables) ? record.messageVariables : undefined;
}

export function resolveErrorMessage(err: unknown): string | undefined {
	const record = getErrorRecord(err);
	if (!record || typeof record.message !== 'string') {
		return undefined;
	}
	return record.message;
}

export function hasApiErrorCode(value: string): boolean {
	return apiErrorCodeSet.has(value);
}
