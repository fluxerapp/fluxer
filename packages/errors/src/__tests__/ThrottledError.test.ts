// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {IpAuthorizationResendCooldownError} from '@fluxer/errors/src/domains/auth/IpAuthorizationResendCooldownError';
import {IpAuthorizationResendLimitExceededError} from '@fluxer/errors/src/domains/auth/IpAuthorizationResendLimitExceededError';
import {ThrottledError} from '@fluxer/errors/src/domains/core/ThrottledError';
import type {FluxerError} from '@fluxer/errors/src/FluxerError';
import {describe, expect, it} from 'vitest';

interface ThrottledResponseBody {
	code: string;
	[key: string]: unknown;
}

async function readResponse(error: FluxerError): Promise<{
	status: number;
	retryAfter: string | null;
	body: ThrottledResponseBody;
}> {
	const response = error.getResponse();
	const body = (await response.json()) as ThrottledResponseBody;
	return {status: response.status, retryAfter: response.headers.get('Retry-After'), body};
}

describe('ThrottledError', () => {
	it('answers 429 with a Retry-After header taken from the caller', async () => {
		const {status, retryAfter} = await readResponse(
			new ThrottledError({code: APIErrorCodes.RESOURCE_LOCKED, retryAfterSeconds: 1}),
		);
		expect(status).toBe(429);
		expect(retryAfter).toBe('1');
	});

	it('rounds a fractional delay up to whole seconds', async () => {
		const {retryAfter} = await readResponse(
			new ThrottledError({code: APIErrorCodes.RESOURCE_LOCKED, retryAfterSeconds: 2.1}),
		);
		expect(retryAfter).toBe('3');
	});

	it('falls back to one second when the delay is not a usable number', async () => {
		const {retryAfter} = await readResponse(
			new ThrottledError({code: APIErrorCodes.RESOURCE_LOCKED, retryAfterSeconds: Number.NaN}),
		);
		expect(retryAfter).toBe('1');
	});

	it('keeps the computed Retry-After when the caller passes other headers', async () => {
		const error = new ThrottledError({
			code: APIErrorCodes.RESOURCE_LOCKED,
			retryAfterSeconds: 4,
			headers: {'X-RateLimit-Scope': 'user'},
		});
		const response = error.getResponse();
		expect(response.headers.get('Retry-After')).toBe('4');
		expect(response.headers.get('X-RateLimit-Scope')).toBe('user');
	});
});

describe('IpAuthorizationResendCooldownError', () => {
	it('reports the cooldown as a header and in the body', async () => {
		const {status, retryAfter, body} = await readResponse(new IpAuthorizationResendCooldownError(17));
		expect(status).toBe(429);
		expect(body.code).toBe(APIErrorCodes.IP_AUTHORIZATION_RESEND_COOLDOWN);
		expect(body.resend_available_in).toBe(17);
		expect(body.retry_after).toBe(17);
		expect(retryAfter).toBe('17');
	});
});

describe('IpAuthorizationResendLimitExceededError', () => {
	it('answers 400 with no retry guidance because the allowance never refills', async () => {
		const {status, retryAfter, body} = await readResponse(new IpAuthorizationResendLimitExceededError());
		expect(status).toBe(400);
		expect(body.code).toBe(APIErrorCodes.IP_AUTHORIZATION_RESEND_LIMIT_EXCEEDED);
		expect(retryAfter).toBeNull();
	});
});
