// SPDX-License-Identifier: AGPL-3.0-or-later

import {SlowmodeRateLimitError} from '@fluxer/errors/src/domains/core/SlowmodeRateLimitError';
import {describe, expect, it} from 'vitest';

interface SlowmodeResponseBody {
	code: string;
	retry_after: number;
}

async function readSlowmodeResponse(error: SlowmodeRateLimitError): Promise<{
	status: number;
	header: string | null;
	body: SlowmodeResponseBody;
}> {
	const response = error.getResponse();
	const body = (await response.json()) as SlowmodeResponseBody;
	return {status: response.status, header: response.headers.get('Retry-After'), body};
}

describe('SlowmodeRateLimitError', () => {
	it('reports Retry-After in whole seconds and the body in decimal seconds', async () => {
		const {status, header, body} = await readSlowmodeResponse(
			new SlowmodeRateLimitError({retryAfter: 5, retryAfterDecimal: 4.7}),
		);
		expect(status).toBe(400);
		expect(body.code).toBe('SLOWMODE_RATE_LIMITED');
		expect(body.retry_after).toBe(4.7);
		expect(header).toBe('5');
	});

	it('keeps the header and the body within one second of each other', async () => {
		const {header, body} = await readSlowmodeResponse(
			new SlowmodeRateLimitError({retryAfter: 5, retryAfterDecimal: 4.997}),
		);
		const headerSeconds = Number(header);
		expect(headerSeconds - body.retry_after).toBeLessThan(1);
		expect(headerSeconds).toBeGreaterThanOrEqual(body.retry_after);
	});

	it('falls back to one second when the caller has no retry window', async () => {
		const {header, body} = await readSlowmodeResponse(new SlowmodeRateLimitError({retryAfter: undefined}));
		expect(header).toBe('1');
		expect(body.retry_after).toBe(1);
	});
});
