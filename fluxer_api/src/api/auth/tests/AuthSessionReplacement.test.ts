// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearTestEmails,
	createAuthHarness,
	createTestAccount,
	findLastTestEmail,
	listTestEmails,
	loginAccount,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {generateUniquePassword, HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

interface AuthSessionRow {
	id_hash: string;
	current: boolean;
}

interface ReplacementResponse {
	token?: string;
	auth_session_id_hash?: string;
}

type GatewayCall = {kind: 'terminate'; hashes: Array<string>} | {kind: 'session_change'; data: Record<string, unknown>};

async function getCurrentAuthSessionHash(harness: ApiTestHarness, token: string): Promise<string> {
	const sessions = await createBuilder<Array<AuthSessionRow>>(harness, token).get('/auth/sessions').execute();
	const current = sessions.find((session) => session.current);
	if (!current) {
		throw new Error('Current auth session not found');
	}
	return current.id_hash;
}

async function completePasswordChange(
	harness: ApiTestHarness,
	account: TestAccount,
	newPassword: string,
): Promise<ReplacementResponse> {
	const start = await createBuilder<{ticket: string}>(harness, account.token)
		.post('/users/@me/password-change/start')
		.body({})
		.execute();
	const emails = await listTestEmails(harness, {recipient: account.email});
	const record = findLastTestEmail(emails, 'password_change_verification');
	if (!record) {
		throw new Error('Password change verification email not found');
	}
	const verify = await createBuilder<{verification_proof: string}>(harness, account.token)
		.post('/users/@me/password-change/verify')
		.body({ticket: start.ticket, code: record.metadata.code})
		.execute();
	return createBuilder<ReplacementResponse>(harness, account.token)
		.post('/users/@me/password-change/complete')
		.body({ticket: start.ticket, verification_proof: verify.verification_proof, new_password: newPassword})
		.expect(HTTP_STATUS.OK)
		.execute();
}

describe('Auth session replacement on password change', () => {
	let harness: ApiTestHarness;
	let calls: Array<GatewayCall>;
	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		await clearTestEmails(harness);
		calls = [];
		vi.spyOn(NoopGatewayService.prototype, 'terminateSession').mockImplementation(async (params) => {
			calls.push({kind: 'terminate', hashes: [...params.sessionIdHashes]});
		});
		vi.spyOn(NoopGatewayService.prototype, 'dispatchPresence').mockImplementation(async (params) => {
			if (params.event === 'AUTH_SESSION_CHANGE') {
				calls.push({kind: 'session_change', data: params.data as Record<string, unknown>});
			}
		});
	});
	afterEach(() => {
		vi.restoreAllMocks();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	function expectReplacedSessionClosedBeforeEvent(oldHash: string, newHash: string | undefined): void {
		const eventIndex = calls.findIndex((call) => call.kind === 'session_change');
		const terminateIndex = calls.findIndex((call) => call.kind === 'terminate' && call.hashes.includes(oldHash));
		expect(terminateIndex).toBeGreaterThanOrEqual(0);
		expect(eventIndex).toBeGreaterThan(terminateIndex);
		const event = calls[eventIndex] as Extract<GatewayCall, {kind: 'session_change'}>;
		expect(event.data).toEqual({old_auth_session_id_hash: oldHash, new_auth_session_id_hash: newHash});
	}

	it('returns the replacement token from PATCH /users/@me', async () => {
		const account = await createTestAccount(harness);
		const otherSession = await loginAccount(harness, account);
		const oldHash = await getCurrentAuthSessionHash(harness, account.token);
		calls = [];
		const response = await createBuilder<ReplacementResponse>(harness, account.token)
			.patch('/users/@me')
			.body({password: account.password, new_password: generateUniquePassword()})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(typeof response.token).toBe('string');
		expect(typeof response.auth_session_id_hash).toBe('string');
		expectReplacedSessionClosedBeforeEvent(oldHash, response.auth_session_id_hash);
		await createBuilder(harness, account.token).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
		await createBuilder(harness, otherSession.token).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
		await createBuilder(harness, response.token!).get('/users/@me').expect(HTTP_STATUS.OK).execute();
		expect(await getCurrentAuthSessionHash(harness, response.token!)).toBe(response.auth_session_id_hash);
	});

	it('leaves the PATCH response without a token when the password is unchanged', async () => {
		const account = await createTestAccount(harness);
		const response = await createBuilder<ReplacementResponse>(harness, account.token)
			.patch('/users/@me')
			.body({global_name: 'Renamed'})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(response.token).toBeUndefined();
		expect(response.auth_session_id_hash).toBeUndefined();
		expect(calls).toEqual([]);
	});

	it('closes the replaced session before announcing the change on password-change/complete', async () => {
		const account = await createTestAccount(harness);
		const oldHash = await getCurrentAuthSessionHash(harness, account.token);
		calls = [];
		const response = await completePasswordChange(harness, account, generateUniquePassword());
		expectReplacedSessionClosedBeforeEvent(oldHash, response.auth_session_id_hash);
		await createBuilder(harness, response.token!).get('/users/@me').expect(HTTP_STATUS.OK).execute();
	});
});
