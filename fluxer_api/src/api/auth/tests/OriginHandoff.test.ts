// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, randomBytes} from 'node:crypto';
import {createAuthHarness, createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestBotAccount} from '@app/api/bot/tests/BotTestUtils';
import {getConfig} from '@app/api/Config';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {SuspiciousActivityFlags} from '@fluxer/constants/src/UserConstants';
import {
	ORIGIN_HANDOFF_MAX_PAYLOAD_LENGTH,
	type OriginHandoffCreateResponse,
	type OriginHandoffRedeemResponse,
} from '@fluxer/schema/src/domains/auth/OriginHandoffSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const CREATE_PATH = '/auth/origin-handoff';
const REDEEM_PATH = '/auth/origin-handoff/redeem';
const PAYLOAD = randomBytes(96).toString('base64url');

function createNonce(): {nonce: string; nonceHash: string} {
	const nonce = randomBytes(32).toString('base64url');
	return {nonce, nonceHash: createHash('sha256').update(nonce).digest('hex')};
}

describe('Origin handoff', () => {
	let harness: ApiTestHarness;
	let webAppOrigin: string;

	beforeAll(async () => {
		harness = await createAuthHarness();
		webAppOrigin = getConfig().endpoints.webAppOrigins[0];
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterEach(() => {
		getConfig().instance.selfHosted = false;
		getConfig().endpoints.webAppOrigins = [webAppOrigin];
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	async function createHandoff(token: string, nonceHash: string): Promise<string> {
		const response = await createBuilder<OriginHandoffCreateResponse>(harness, token)
			.post(CREATE_PATH)
			.body({nonce_hash: nonceHash, payload: PAYLOAD})
			.execute();
		expect(response.handoff_id).toMatch(/^[A-Za-z0-9_-]{43}$/);
		return response.handoff_id;
	}

	it('hands the payload over once to the origin that holds the nonce', async () => {
		const account = await createTestAccount(harness);
		const {nonce, nonceHash} = createNonce();
		const handoffId = await createHandoff(account.token, nonceHash);

		const redeemed = await createBuilderWithoutAuth<OriginHandoffRedeemResponse>(harness)
			.post(REDEEM_PATH)
			.header('origin', webAppOrigin)
			.body({handoff_id: handoffId, nonce})
			.execute();
		expect(redeemed).toEqual({payload: PAYLOAD});

		await createBuilderWithoutAuth(harness)
			.post(REDEEM_PATH)
			.header('origin', webAppOrigin)
			.body({handoff_id: handoffId, nonce})
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_ORIGIN_HANDOFF)
			.execute();
	});

	it('consumes the handoff when the nonce does not match', async () => {
		const account = await createTestAccount(harness);
		const {nonce, nonceHash} = createNonce();
		const handoffId = await createHandoff(account.token, nonceHash);

		await createBuilderWithoutAuth(harness)
			.post(REDEEM_PATH)
			.header('origin', webAppOrigin)
			.body({handoff_id: handoffId, nonce: createNonce().nonce})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_ORIGIN_HANDOFF_NONCE)
			.execute();

		await createBuilderWithoutAuth(harness)
			.post(REDEEM_PATH)
			.header('origin', webAppOrigin)
			.body({handoff_id: handoffId, nonce})
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_ORIGIN_HANDOFF)
			.execute();
	});

	it('answers an unknown handoff id with its own error code', async () => {
		await createBuilderWithoutAuth(harness)
			.post(REDEEM_PATH)
			.header('origin', webAppOrigin)
			.body({handoff_id: randomBytes(32).toString('base64url'), nonce: createNonce().nonce})
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_ORIGIN_HANDOFF)
			.execute();
	});

	it('requires a logged-in user to create a handoff', async () => {
		await createBuilderWithoutAuth(harness)
			.post(CREATE_PATH)
			.body({nonce_hash: createNonce().nonceHash, payload: PAYLOAD})
			.expect(HTTP_STATUS.UNAUTHORIZED)
			.execute();
	});

	it('refuses to create a handoff for an account flagged as suspicious', async () => {
		const account = await createTestAccount(harness);
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${account.userId}/security-flags`)
			.body({suspicious_activity_flags: SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE})
			.execute();
		await createBuilder(harness, account.token)
			.post(CREATE_PATH)
			.body({nonce_hash: createNonce().nonceHash, payload: PAYLOAD})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.ACCOUNT_SUSPICIOUS_ACTIVITY)
			.execute();
	});

	it('refuses a create body larger than the payload ceiling before parsing it', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post(CREATE_PATH)
			.body({nonce_hash: createNonce().nonceHash, payload: 'a'.repeat(ORIGIN_HANDOFF_MAX_PAYLOAD_LENGTH + 2048)})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.FILE_SIZE_TOO_LARGE)
			.execute();
	});

	it('refuses to create a handoff for a bot', async () => {
		const bot = await createTestBotAccount(harness);
		await createBuilder(harness, `Bot ${bot.botToken}`)
			.post(CREATE_PATH)
			.body({nonce_hash: createNonce().nonceHash, payload: PAYLOAD})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});

	it.each([
		{name: 'an uppercase nonce hash', body: {nonce_hash: 'A'.repeat(64), payload: PAYLOAD}},
		{name: 'a short nonce hash', body: {nonce_hash: 'a'.repeat(63), payload: PAYLOAD}},
		{name: 'a payload outside base64url', body: {nonce_hash: 'a'.repeat(64), payload: 'not+base64/url='}},
		{name: 'an empty payload', body: {nonce_hash: 'a'.repeat(64), payload: ''}},
	])('rejects $name', async ({body}) => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post(CREATE_PATH)
			.body(body)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
			.execute();
	});

	it('refuses a redeem from an origin outside the first-party web origins', async () => {
		const account = await createTestAccount(harness);
		const {nonce, nonceHash} = createNonce();
		const handoffId = await createHandoff(account.token, nonceHash);

		await createBuilderWithoutAuth(harness)
			.post(REDEEM_PATH)
			.header('origin', 'https://evil.example')
			.body({handoff_id: handoffId, nonce})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.INVALID_API_ORIGIN)
			.execute();

		await createBuilderWithoutAuth(harness)
			.post(REDEEM_PATH)
			.body({handoff_id: handoffId, nonce})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.INVALID_API_ORIGIN)
			.execute();

		const redeemed = await createBuilderWithoutAuth<OriginHandoffRedeemResponse>(harness)
			.post(REDEEM_PATH)
			.header('origin', webAppOrigin)
			.body({handoff_id: handoffId, nonce})
			.execute();
		expect(redeemed.payload).toBe(PAYLOAD);
	});

	it('accepts a redeem from a configured web app origin alias', async () => {
		getConfig().endpoints.webAppOrigins = [webAppOrigin, 'https://fluxer.com'];
		const account = await createTestAccount(harness);
		const {nonce, nonceHash} = createNonce();
		const handoffId = await createHandoff(account.token, nonceHash);

		const redeemed = await createBuilderWithoutAuth<OriginHandoffRedeemResponse>(harness)
			.post(REDEEM_PATH)
			.header('origin', 'https://fluxer.com')
			.body({handoff_id: handoffId, nonce})
			.execute();
		expect(redeemed.payload).toBe(PAYLOAD);
	});

	it('skips the origin check on a self-hosted instance', async () => {
		getConfig().instance.selfHosted = true;
		const account = await createTestAccount(harness);
		const {nonce, nonceHash} = createNonce();
		const handoffId = await createHandoff(account.token, nonceHash);

		const redeemed = await createBuilderWithoutAuth<OriginHandoffRedeemResponse>(harness)
			.post(REDEEM_PATH)
			.body({handoff_id: handoffId, nonce})
			.execute();
		expect(redeemed.payload).toBe(PAYLOAD);
	});
});
