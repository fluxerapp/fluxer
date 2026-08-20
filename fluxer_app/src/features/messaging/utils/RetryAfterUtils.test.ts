// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveRetryAfterMs} from '@app/features/messaging/utils/RetryAfterUtils';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {describe, expect, it} from 'vitest';

function slowmodeRejection(body: unknown, responseHeaders: Record<string, string>): HttpError {
	return new HttpError({
		method: 'POST',
		path: '/channels/1234567890123456789/messages',
		status: 400,
		body,
		responseHeaders,
	});
}

describe('resolveRetryAfterMs', () => {
	it('uses the decimal seconds from the body instead of the Retry-After header', () => {
		const error = slowmodeRejection(
			{code: APIErrorCodes.SLOWMODE_RATE_LIMITED, retry_after: 4.44},
			{'retry-after': '5'},
		);
		expect(resolveRetryAfterMs(error)).toBe(4440);
	});

	it('never multiplies a millisecond Retry-After header into a longer window than the body', () => {
		const error = slowmodeRejection(
			{code: APIErrorCodes.SLOWMODE_RATE_LIMITED, retry_after: 4.7},
			{'retry-after': '4700'},
		);
		expect(resolveRetryAfterMs(error)).toBe(4700);
	});

	it('prefers a nested retry window over every other source', () => {
		const error = slowmodeRejection(
			{code: APIErrorCodes.SLOWMODE_RATE_LIMITED, retry_after: 30, details: {retry: {after_seconds: 2.5}}},
			{'retry-after': '30'},
		);
		expect(resolveRetryAfterMs(error)).toBe(2500);
	});

	it('falls back to the Retry-After header when the body carries no window', () => {
		const error = slowmodeRejection({code: APIErrorCodes.SLOWMODE_RATE_LIMITED}, {'retry-after': '3'});
		expect(resolveRetryAfterMs(error)).toBe(3000);
	});

	it('falls back to the reset-after header when nothing else is present', () => {
		const error = slowmodeRejection({code: APIErrorCodes.RATE_LIMITED}, {'x-ratelimit-reset-after': '1.25'});
		expect(resolveRetryAfterMs(error)).toBe(1250);
	});

	it('reads an HTTP date Retry-After header as a remaining duration', () => {
		const deadline = new Date(Date.now() + 4000).toUTCString();
		const error = slowmodeRejection({code: APIErrorCodes.SLOWMODE_RATE_LIMITED}, {'retry-after': deadline});
		const retryAfterMs = resolveRetryAfterMs(error);
		expect(retryAfterMs).not.toBeNull();
		expect(retryAfterMs!).toBeGreaterThan(0);
		expect(retryAfterMs!).toBeLessThanOrEqual(4000);
	});

	it('returns null when no retry window is advertised', () => {
		expect(resolveRetryAfterMs(slowmodeRejection({code: APIErrorCodes.SLOWMODE_RATE_LIMITED}, {}))).toBeNull();
	});
});
