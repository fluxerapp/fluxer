// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {getConfig} from '../../Config';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {createBuilder} from '../../test/TestRequestBuilder';
import {createAuthHarness, createTestAccount} from './AuthTestUtils';

describe('Inbound SMS challenge without a receiving number', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	it('rejects the challenge with SMS_VERIFICATION_UNAVAILABLE when no number is configured', async () => {
		const account = await createTestAccount(harness);
		const config = getConfig();
		const previousInboundNumber = config.sms.inboundChallengeNumber;
		config.sms.inboundChallengeNumber = undefined;
		try {
			const {response, json} = await createBuilder(harness, account.token)
				.post('/users/@me/phone/inbound-challenge')
				.executeRaw();
			expect(response.status).toBe(400);
			expect(json).toMatchObject({code: 'SMS_VERIFICATION_UNAVAILABLE'});
		} finally {
			config.sms.inboundChallengeNumber = previousInboundNumber;
		}
	});
	it('issues the challenge once a receiving number is configured', async () => {
		const account = await createTestAccount(harness);
		const config = getConfig();
		const previousInboundNumber = config.sms.inboundChallengeNumber;
		config.sms.inboundChallengeNumber = '+15551234567';
		try {
			const response = await createBuilder<{
				challenge_code: string;
				our_number: string;
				expires_at: string;
			}>(harness, account.token)
				.post('/users/@me/phone/inbound-challenge')
				.expect(200)
				.execute();
			expect(response.our_number).toBe('+15551234567');
			expect(response.challenge_code).toMatch(/^\d{6}$/);
		} finally {
			config.sms.inboundChallengeNumber = previousInboundNumber;
		}
	});
});
