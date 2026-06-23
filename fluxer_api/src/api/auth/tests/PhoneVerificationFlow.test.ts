// SPDX-License-Identifier: AGPL-3.0-or-later

import {PublicUserFlags, SuspiciousActivityFlags, UserFlags} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {getConfig} from '../../Config';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {createBuilder} from '../../test/TestRequestBuilder';
import {createAuthHarness, createTestAccount, createTotpSecret, totpCodeNow} from './AuthTestUtils';

describe('Phone verification flow', () => {
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
	async function allowPhoneVerification(userId: string): Promise<void> {
		await createBuilder(harness, '')
			.post(`/test/users/${userId}/security-flags`)
			.body({
				email_verified: true,
				suspicious_activity_flags: SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE,
			})
			.expect(200)
			.execute();
	}
	it('completes full phone verification flow', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, '')
			.patch(`/test/users/${account.userId}/flags`)
			.body({flags: (UserFlags.SPAMMER | UserFlags.HAS_SESSION_STARTED).toString()})
			.expect(200)
			.execute();
		const totpSecret = createTotpSecret();
		const totpCode = totpCodeNow(totpSecret);
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({
				secret: totpSecret,
				code: totpCode,
				password: account.password,
			})
			.execute();
		await allowPhoneVerification(account.userId);
		const phone = `+1555${String(Date.now()).slice(-7)}`;
		await createBuilder(harness, account.token)
			.post('/users/@me/phone/send-verification')
			.body({
				phone,
			})
			.expect(200)
			.execute();
		const verifyPhoneJson = await createBuilder<{
			verified: true;
		}>(harness, account.token)
			.post('/users/@me/phone/verify')
			.body({
				phone,
				code: '123456',
			})
			.execute();
		expect(verifyPhoneJson).toEqual({verified: true});
		const meJson = await createBuilder<{
			flags: number;
			phone: string | null;
			has_verified_phone: boolean;
		}>(harness, account.token)
			.get('/users/@me')
			.execute();
		expect(meJson.phone).toBeNull();
		expect(meJson.has_verified_phone).toBe(true);
		expect(meJson.flags & PublicUserFlags.SPAMMER).toBe(0);
	});
	it('allows a second account to verify the same phone during the encrypted marker window', async () => {
		const phone = `+1555${String(Date.now()).slice(-7)}`;
		async function verifyPhone(account: {token: string}): Promise<void> {
			await createBuilder(harness, account.token)
				.post('/users/@me/phone/send-verification')
				.body({phone})
				.expect(200)
				.execute();
			const response = await createBuilder<{
				verified: true;
			}>(harness, account.token)
				.post('/users/@me/phone/verify')
				.body({phone, code: '123456'})
				.execute();
			expect(response).toEqual({verified: true});
		}
		async function hasVerifiedPhone(token: string): Promise<boolean> {
			const me = await createBuilder<{
				has_verified_phone: boolean;
			}>(harness, token)
				.get('/users/@me')
				.execute();
			return me.has_verified_phone;
		}
		const first = await createTestAccount(harness);
		const second = await createTestAccount(harness);
		await allowPhoneVerification(first.userId);
		await allowPhoneVerification(second.userId);
		await verifyPhone(first);
		expect(await hasVerifiedPhone(first.token)).toBe(true);
		await verifyPhone(second);
		expect(await hasVerifiedPhone(first.token)).toBe(true);
		expect(await hasVerifiedPhone(second.token)).toBe(true);
	});
	it('rejects a third account from reusing the same phone number during the encrypted marker window', async () => {
		const phone = `+1555${String(Date.now()).slice(-7)}`;
		async function verifyPhone(account: {token: string}): Promise<void> {
			await createBuilder(harness, account.token)
				.post('/users/@me/phone/send-verification')
				.body({phone})
				.expect(200)
				.execute();
			const response = await createBuilder<{
				verified: true;
			}>(harness, account.token)
				.post('/users/@me/phone/verify')
				.body({phone, code: '123456'})
				.execute();
			expect(response).toEqual({verified: true});
		}
		const first = await createTestAccount(harness);
		const second = await createTestAccount(harness);
		const third = await createTestAccount(harness);
		await allowPhoneVerification(first.userId);
		await allowPhoneVerification(second.userId);
		await allowPhoneVerification(third.userId);
		await verifyPhone(first);
		await verifyPhone(second);
		await createBuilder(harness, third.token)
			.post('/users/@me/phone/send-verification')
			.body({phone})
			.expect(400, 'PHONE_ALREADY_USED')
			.execute();
	});
	it('limits each account to 3 phone verification SMS sends across any numbers per 6 hours', async () => {
		const account = await createTestAccount(harness);
		await allowPhoneVerification(account.userId);
		const phones = Array.from({length: 4}, (_, idx) => `+1555${String(1000000 + idx).padStart(7, '0')}`);
		for (const phone of phones.slice(0, 3)) {
			await createBuilder(harness, account.token)
				.post('/users/@me/phone/send-verification')
				.body({phone})
				.expect(200)
				.execute();
		}
		const {response, json} = await createBuilder(harness, account.token)
			.post('/users/@me/phone/send-verification')
			.body({phone: phones[3]})
			.executeRaw();
		expect(response.status).toBe(429);
		expect(json).toMatchObject({code: 'PHONE_RATE_LIMIT_EXCEEDED'});
	});
	it('limits a specific phone number to 3 SMS sends per 5 days across all accounts', async () => {
		const phone = '+15551234567';
		const accounts = await Promise.all([
			createTestAccount(harness),
			createTestAccount(harness),
			createTestAccount(harness),
			createTestAccount(harness),
		]);
		for (const account of accounts) {
			await allowPhoneVerification(account.userId);
		}
		for (const account of accounts.slice(0, 3)) {
			await createBuilder(harness, account.token)
				.post('/users/@me/phone/send-verification')
				.body({phone})
				.expect(200)
				.execute();
		}
		const {response, json} = await createBuilder(harness, accounts[3].token)
			.post('/users/@me/phone/send-verification')
			.body({phone})
			.executeRaw();
		expect(response.status).toBe(429);
		expect(json).toMatchObject({code: 'PHONE_RATE_LIMIT_EXCEEDED'});
	});
	it('requires an inbound challenge for expensive outbound SMS prefixes', async () => {
		const account = await createTestAccount(harness);
		await allowPhoneVerification(account.userId);
		const config = getConfig();
		const previousInboundNumber = config.sms.inboundChallengeNumber;
		const previousInboundPrefixes = config.abusePolicy.phoneVerification.inboundRequiredPrefixes;
		config.sms.inboundChallengeNumber = '+15551234567';
		config.abusePolicy.phoneVerification.inboundRequiredPrefixes = ['+998'];
		try {
			const response = await createBuilder<{
				channel: string;
				challenge_code: string;
				our_number: string;
				reason: string;
			}>(harness, account.token)
				.post('/users/@me/phone/send-verification')
				.body({phone: '+998991234567'})
				.expect(200)
				.execute();
			expect(response).toMatchObject({
				channel: 'inbound_challenge',
				our_number: '+15551234567',
				reason: 'expensive_destination',
			});
			expect(response.challenge_code).toMatch(/^\d{6}$/);
		} finally {
			config.sms.inboundChallengeNumber = previousInboundNumber;
			config.abusePolicy.phoneVerification.inboundRequiredPrefixes = previousInboundPrefixes;
		}
	});
	it('does not let clients force SMS for inbound-only prefixes', async () => {
		const account = await createTestAccount(harness);
		await allowPhoneVerification(account.userId);
		const config = getConfig();
		const previousInboundNumber = config.sms.inboundChallengeNumber;
		const previousInboundPrefixes = config.abusePolicy.phoneVerification.inboundRequiredPrefixes;
		config.sms.inboundChallengeNumber = '+15551234567';
		config.abusePolicy.phoneVerification.inboundRequiredPrefixes = ['+593'];
		try {
			const response = await createBuilder<{
				channel: string;
				challenge_code: string;
				our_number: string;
				reason: string;
			}>(harness, account.token)
				.post('/users/@me/phone/send-verification')
				.body({phone: '+593991234567', channel: 'sms'})
				.expect(200)
				.execute();
			expect(response).toMatchObject({
				channel: 'inbound_challenge',
				our_number: '+15551234567',
				reason: 'expensive_destination',
			});
		} finally {
			config.sms.inboundChallengeNumber = previousInboundNumber;
			config.abusePolicy.phoneVerification.inboundRequiredPrefixes = previousInboundPrefixes;
		}
	});
});
