// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HttpError} from '@app/features/platform/types/EndpointError';

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function parseSeconds(value: unknown): number | null {
	if (typeof value !== 'number' || !Number.isFinite(value) || value <= 0) return null;
	return value;
}

function parseNestedSeconds(body: Record<string, unknown> | undefined): number | null {
	if (body === undefined || !isRecord(body.details)) return null;
	const retry = body.details.retry;
	if (!isRecord(retry)) return null;
	return parseSeconds(retry.after_seconds);
}

function parseHeaderSeconds(value: string | undefined): number | null {
	if (value === undefined || value.trim() === '') return null;
	const numeric = Number(value);
	if (Number.isFinite(numeric)) return parseSeconds(numeric);
	const deadline = Date.parse(value);
	if (!Number.isFinite(deadline)) return null;
	return parseSeconds((deadline - Date.now()) / 1000);
}

function resolveRetryAfterSeconds(error: HttpError): number | null {
	const body = isRecord(error.body) ? error.body : undefined;
	const nestedSeconds = parseNestedSeconds(body);
	if (nestedSeconds !== null) return nestedSeconds;
	const bodySeconds = body === undefined ? null : parseSeconds(body.retry_after);
	if (bodySeconds !== null) return bodySeconds;
	const headerSeconds = parseHeaderSeconds(error.responseHeaders['retry-after']);
	if (headerSeconds !== null) return headerSeconds;
	return parseHeaderSeconds(error.responseHeaders['x-ratelimit-reset-after']);
}

export function resolveRetryAfterMs(error: HttpError): number | null {
	const retryAfterSeconds = resolveRetryAfterSeconds(error);
	if (retryAfterSeconds === null) return null;
	const retryAfterMs = Math.ceil(retryAfterSeconds * 1000);
	if (!Number.isSafeInteger(retryAfterMs)) return null;
	return retryAfterMs;
}
