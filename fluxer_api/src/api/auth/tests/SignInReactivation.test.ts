// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearTestEmails,
	createTestAccount,
	findLastTestEmail,
	type LoginSuccessResponse,
	listTestEmails,
	loginAccount,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getKVAccountDeletionQueue, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {generateUniquePassword, HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {reschedulePendingDeletion} from '@app/api/user/services/PendingDeletionCoordinator';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface HandoffInitiateResponse {
	code: string;
}

describe('Signing in again reactivates the account on every sign-in path', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
		await clearTestEmails(harness);
	});
	afterEach(async () => {
		await harness?.shutdown();
	});

	async function readUser(account: TestAccount) {
		return (await getUserRepository().findUnique(createUserID(BigInt(account.userId))))!;
	}

	async function resetPassword(account: TestAccount): Promise<LoginSuccessResponse> {
		await createBuilderWithoutAuth(harness)
			.post('/auth/forgot')
			.body({email: account.email})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const emails = await listTestEmails(harness, {recipient: account.email});
		const token = findLastTestEmail(emails, 'password_reset')!.metadata!.token!;
		return createBuilderWithoutAuth<LoginSuccessResponse>(harness)
			.post('/auth/reset')
			.body({token, password: generateUniquePassword()})
			.execute();
	}

	test('a password reset cancels a self-requested deletion', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post('/users/@me/delete')
			.body({password: account.password})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		expect((await readUser(account)).pendingDeletionAt).not.toBeNull();
		expect(await harness.kvProvider.zcard('deletion_queue')).toBe(1);
		const reset = await resetPassword(account);
		const user = await readUser(account);
		expect(user.flags & UserFlags.SELF_DELETED).toBe(0n);
		expect(user.pendingDeletionAt).toBeNull();
		expect(user.deletionReasonCode).toBeNull();
		expect(await harness.kvProvider.zcard('deletion_queue')).toBe(0);
		await createBuilder(harness, reset.token).get('/users/@me').expect(HTTP_STATUS.OK).execute();
	});

	test('a password reset reactivates a disabled account', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post('/users/@me/disable')
			.body({password: account.password})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		expect((await readUser(account)).flags & UserFlags.DISABLED).toBe(UserFlags.DISABLED);
		await resetPassword(account);
		expect((await readUser(account)).flags & UserFlags.DISABLED).toBe(0n);
	});

	test('a device handoff from a live session cancels a pending inactivity deletion', async () => {
		const account = await createTestAccount(harness);
		const login = await loginAccount(harness, account);
		const users = getUserRepository();
		const user = await readUser(account);
		const pendingDeletionAt = new Date(Date.now() + 86_400_000);
		await users.updateDeletionSchedule(user, {
			flags: user.flags | UserFlags.SELF_DELETED,
			pending_deletion_at: pendingDeletionAt,
			deletion_reason_code: DeletionReasons.INACTIVITY,
		});
		await reschedulePendingDeletion({
			userId: user.id,
			nextPendingDeletionAt: pendingDeletionAt,
			deletionReasonCode: DeletionReasons.INACTIVITY,
			userRepository: users,
			deletionQueue: getKVAccountDeletionQueue(),
		});
		expect(await harness.kvProvider.zcard('deletion_queue')).toBe(1);
		const initiated = await createBuilderWithoutAuth<HandoffInitiateResponse>(harness)
			.post('/auth/handoff/initiate')
			.body(null)
			.execute();
		await createBuilderWithoutAuth(harness).get(`/auth/handoff/${initiated.code}/info`).execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/handoff/complete')
			.body({code: initiated.code, token: login.token, user_id: login.userId})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const after = await readUser(account);
		expect(after.flags & UserFlags.SELF_DELETED).toBe(0n);
		expect(after.pendingDeletionAt).toBeNull();
		expect(await harness.kvProvider.zcard('deletion_queue')).toBe(0);
	});

	test('a temporary ban still blocks every sign-in path', async () => {
		const account = await createTestAccount(harness);
		const users = getUserRepository();
		const user = await readUser(account);
		await users.patchUpsert(
			user.id,
			{flags: user.flags | UserFlags.DISABLED, temp_banned_until: new Date(Date.now() + 3_600_000)},
			user.toRow(),
		);
		await createBuilderWithoutAuth(harness)
			.post('/auth/forgot')
			.body({email: account.email})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const emails = await listTestEmails(harness, {recipient: account.email});
		const token = findLastTestEmail(emails, 'password_reset')!.metadata!.token!;
		const {response} = await createBuilderWithoutAuth(harness)
			.post('/auth/reset')
			.body({token, password: generateUniquePassword()})
			.executeRaw();
		expect(response.status).toBe(HTTP_STATUS.FORBIDDEN);
		expect((await readUser(account)).flags & UserFlags.DISABLED).toBe(UserFlags.DISABLED);
	});
});
