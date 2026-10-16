// SPDX-License-Identifier: AGPL-3.0-or-later

export type GooglePlayApiErrorKind = 'invalid_token' | 'retryable' | 'auth' | 'rejected';

export class GooglePlayApiError extends Error {
	constructor(
		message: string,
		public readonly kind: GooglePlayApiErrorKind,
		public readonly status: number | null,
		public readonly reason: string | null,
		public readonly retryAfterMs: number | null = null,
	) {
		super(message);
		this.name = 'GooglePlayApiError';
	}
}

export function isGooglePlayApiError(error: unknown, kind?: GooglePlayApiErrorKind): error is GooglePlayApiError {
	return error instanceof GooglePlayApiError && (kind === undefined || error.kind === kind);
}

export function classifyGooglePlayStatus(status: number): GooglePlayApiErrorKind {
	if (status === 400 || status === 404 || status === 410) {
		return 'invalid_token';
	}
	if (status === 401 || status === 403) {
		return 'auth';
	}
	if (status === 408 || status === 429 || status >= 500) {
		return 'retryable';
	}
	return 'rejected';
}

export function parseRetryAfterMs(value: string | null, nowMs: number): number | null {
	if (!value) {
		return null;
	}
	const trimmed = value.trim();
	if (/^\d+$/u.test(trimmed)) {
		return Number.parseInt(trimmed, 10) * 1000;
	}
	const date = Date.parse(trimmed);
	if (Number.isNaN(date)) {
		return null;
	}
	return Math.max(0, date - nowMs);
}
