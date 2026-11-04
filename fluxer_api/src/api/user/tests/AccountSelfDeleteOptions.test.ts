// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearTestEmails,
	createTestAccount,
	findLastTestEmail,
	listTestEmails,
	loginAccount,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {acceptInvite, createChannelInvite, createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {ensureSessionStarted, sendMessage} from '@app/api/message/tests/MessageTestUtils';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {
	expectDataExists,
	setPendingDeletionAt,
	triggerDeletionWorker,
	waitForDeletionCompletion,
} from '@app/api/user/tests/UserTestUtils';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import type {UserAccountDeleteResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('Self-requested account deletion options', () => {
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

	async function sendMessagesInAnotherCommunity(account: TestAccount, count: number): Promise<void> {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Deletion options');
		const channelId = guild.system_channel_id!;
		const invite = await createChannelInvite(harness, owner.token, channelId);
		await acceptInvite(harness, account.token, invite.code);
		await ensureSessionStarted(harness, account.token);
		for (let i = 0; i < count; i++) {
			await sendMessage(harness, account.token, channelId, `Message ${i + 1}`);
		}
	}

	async function countMessages(account: TestAccount): Promise<number> {
		const json = await createBuilderWithoutAuth<{count: number}>(harness)
			.get(`/test/users/${account.userId}/messages/count`)
			.expect(HTTP_STATUS.OK)
			.execute();
		return json.count;
	}

	async function requestDeletion(
		account: TestAccount,
		options: {immediate?: boolean; delete_messages?: boolean},
	): Promise<UserAccountDeleteResponse> {
		return createBuilder<UserAccountDeleteResponse>(harness, account.token)
			.post('/users/@me/delete')
			.body({password: account.password, ...options})
			.expect(HTTP_STATUS.OK)
			.execute();
	}

	test('delete_messages deletes the messages with the account', async () => {
		const account = await createTestAccount(harness);
		await sendMessagesInAnotherCommunity(account, 3);
		expect(await countMessages(account)).toBeGreaterThanOrEqual(3);
		await requestDeletion(account, {delete_messages: true});
		await setPendingDeletionAt(harness, account.userId, new Date(Date.now() - 60_000));
		await triggerDeletionWorker(harness);
		await waitForDeletionCompletion(harness, account.userId);
		expect(await countMessages(account)).toBe(0);
	}, 60000);

	test('signing in cancels the message deletion together with the account deletion', async () => {
		const account = await createTestAccount(harness);
		await sendMessagesInAnotherCommunity(account, 2);
		const sent = await countMessages(account);
		const response = await requestDeletion(account, {delete_messages: true});
		const scheduled = await readUser(account);
		expect(scheduled.pendingBulkMessageDeletionAt?.toISOString()).toBe(response.pending_deletion_at);
		await loginAccount(harness, account);
		const reactivated = await readUser(account);
		expect(reactivated.flags & UserFlags.SELF_DELETED).toBe(0n);
		expect(reactivated.pendingDeletionAt).toBeNull();
		expect(reactivated.pendingBulkMessageDeletionAt).toBeNull();
		expect(await countMessages(account)).toBe(sent);
	}, 60000);

	test('immediate deletes without the scheduled email and confirms when done', async () => {
		const account = await createTestAccount(harness);
		const requestedAt = Date.now();
		const response = await requestDeletion(account, {immediate: true});
		expect(Math.abs(Date.parse(response.pending_deletion_at) - requestedAt)).toBeLessThan(5_000);
		const before = await listTestEmails(harness, {recipient: account.email});
		expect(findLastTestEmail(before, 'self_deletion_scheduled')).toBeNull();
		await triggerDeletionWorker(harness);
		await waitForDeletionCompletion(harness, account.userId);
		expect((await expectDataExists(harness, account.userId)).hasDeletedFlag).toBe(true);
		const after = await listTestEmails(harness, {recipient: account.email});
		expect(after.filter((email) => email.type === 'account_deletion_completed')).toHaveLength(1);
	}, 60000);

	test('immediate never deletes an App Store reviewer account', async () => {
		const account = await createTestAccount(harness);
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${account.userId}/security-flags`)
			.body({set_flags: ['APP_STORE_REVIEWER']})
			.execute();
		await requestDeletion(account, {immediate: true, delete_messages: true});
		expect((await readUser(account)).pendingBulkMessageDeletionAt).toBeNull();
		await triggerDeletionWorker(harness);
		expect((await expectDataExists(harness, account.userId)).hasDeletedFlag).toBe(false);
		const login = await loginAccount(harness, account);
		await createBuilder(harness, login.token).get('/users/@me').expect(HTTP_STATUS.OK).execute();
	}, 60000);
});
