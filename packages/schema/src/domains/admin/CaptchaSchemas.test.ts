// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	CaptchaConfigSchema,
	CaptchaConfigUpdateRequest,
	DEFAULT_CAPTCHA_CONFIG,
} from '@fluxer/schema/src/domains/admin/CaptchaSchemas';
import {describe, expect, test} from 'vitest';

describe('captcha configuration', () => {
	test('defaults to enabled at the phone-friendly difficulty', () => {
		expect(CaptchaConfigSchema.parse({})).toEqual({enabled: true, cost: 5000, max_counter: 1000});
		expect(DEFAULT_CAPTCHA_CONFIG).toEqual({enabled: true, cost: 5000, max_counter: 1000});
	});

	test('rejects difficulty outside the supported range', () => {
		expect(CaptchaConfigUpdateRequest.safeParse({cost: 999}).success).toBe(false);
		expect(CaptchaConfigUpdateRequest.safeParse({cost: 20001}).success).toBe(false);
		expect(CaptchaConfigUpdateRequest.safeParse({max_counter: 99}).success).toBe(false);
		expect(CaptchaConfigUpdateRequest.safeParse({max_counter: 20001}).success).toBe(false);
		expect(CaptchaConfigSchema.safeParse({cost: 20001}).success).toBe(false);
	});

	test('keeps a partial update partial without filling defaults', () => {
		expect(CaptchaConfigUpdateRequest.parse({})).toEqual({});
		expect(CaptchaConfigUpdateRequest.parse({enabled: false})).toEqual({enabled: false});
		expect(CaptchaConfigUpdateRequest.parse({cost: 1000, max_counter: 100})).toEqual({cost: 1000, max_counter: 100});
	});
});
