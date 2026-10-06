// SPDX-License-Identifier: AGPL-3.0-or-later

import {WebhookTypeSchema} from '@fluxer/schema/src/primitives/WebhookValidators';
import {describe, expect, it} from 'vitest';

describe('WebhookTypeSchema', () => {
	it.each([1, 2])('accepts webhook type %i', (value) => {
		expect(WebhookTypeSchema.parse(value)).toBe(value);
	});

	it.each([0, 3, -1, 1.5, '1', null])('rejects unknown or nonnumeric webhook type %j', (value) => {
		expect(WebhookTypeSchema.safeParse(value).success).toBe(false);
	});
});
