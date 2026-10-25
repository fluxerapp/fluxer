// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createTestAccount,
	createUniqueEmail,
	listTestEmails,
	setUserACLs,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getConfig} from '@app/api/Config';
import {createApiContext} from '@app/api/CreateApiContext';
import {createChannel, createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {sendMessage} from '@app/api/message/tests/MessageTestUtils';
import {getUserActivityBuffer} from '@app/api/middleware/ServiceSingletons';
import {getUserSearchService} from '@app/api/SearchFactory';
import {convertToSearchableUser} from '@app/api/search/user/UserSearchSerializer';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {triggerDeletionWorker, waitForDeletionCompletion} from '@app/api/user/tests/UserTestUtils';
import {backfillDeletedUsers} from '@app/api/worker/tasks/BackfillDeletedUsers';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('immediate account deletion from the admin panel', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	const selfHosted = getConfig().instance.selfHosted;

	beforeAll(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});

	beforeEach(async () => {
		await harness.reset();
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.USER_DELETE,
			AdminACLs.USER_LOOKUP,
			AdminACLs.USER_VIEW_IP,
			AdminACLs.AUDIT_LOG_VIEW,
		]);
	});

	afterEach(() => {
		getConfig().instance.selfHosted = selfHosted;
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	function deleteNow(target: TestAccount) {
		return createBuilder<{user: {pending_deletion_at: string | null}}>(harness, admin.token)
			.put(`/admin/users/${target.userId}/deletion`)
			.body({days_until_deletion: 14, basis: {type: 'holder_request'}, immediate: true});
	}

	it('deletes the account at once on a self-hosted instance', async () => {
		getConfig().instance.selfHosted = true;
		const target = await createTestAccount(harness);
		const before = Date.now();
		const response = await deleteNow(target).expect(HTTP_STATUS.OK).execute();
		expect(Date.parse(response.user.pending_deletion_at!)).toBeLessThanOrEqual(Date.now());
		expect(Date.parse(response.user.pending_deletion_at!)).toBeGreaterThanOrEqual(before - 1000);
		await triggerDeletionWorker(harness);
		await waitForDeletionCompletion(harness, target.userId, 20000);
		const emails = await listTestEmails(harness, {recipient: target.email});
		expect(emails.filter((email) => email.type === 'account_deletion_requested')).toEqual([]);
	});

	async function listedUsers() {
		const result = await createBuilder<{users: Array<{id: string; username: string}>}>(harness, admin.token)
			.get('/admin/users?limit=100')
			.expect(HTTP_STATUS.OK)
			.execute();
		return result.users;
	}

	async function deleteWithLeftovers(target: TestAccount) {
		getConfig().instance.selfHosted = true;
		const guild = await createGuild(harness, target.token, 'Deleted author');
		const channel = await createChannel(harness, target.token, guild.id, 'general');
		await sendMessage(harness, target.token, channel.id, 'kept after deletion');
		await createBuilder(harness, target.token)
			.get('/users/@me')
			.header('x-forwarded-for', '203.0.113.44')
			.expect(HTTP_STATUS.OK)
			.execute();
		await getUserActivityBuffer().drainAndFlush();
		await deleteNow(target).expect(HTTP_STATUS.OK).execute();
		await triggerDeletionWorker(harness);
		await waitForDeletionCompletion(harness, target.userId, 20000);
	}

	it('leaves no deleted account in the user list and no personal data on its row', async () => {
		const target = await createTestAccount(harness);
		await deleteWithLeftovers(target);
		const listed = await listedUsers();
		expect(listed.map((user) => user.id)).toContain(admin.userId);
		expect(listed.filter((user) => user.id === target.userId || user.username === 'DeletedUser')).toEqual([]);
		const row = await createApiContext().services.users.findUniqueAssert(createUserID(BigInt(target.userId)));
		expect(row.toRow()).toMatchObject({
			email: null,
			last_active_ip: null,
			last_active_at: null,
			locale: null,
			terms_agreed_at: null,
			privacy_agreed_at: null,
		});
		await deleteNow(target).expect(HTTP_STATUS.CONFLICT, APIErrorCodes.CONFLICT).execute();
	});

	it('clears what older deletions left behind', async () => {
		const target = await createTestAccount(harness);
		await deleteWithLeftovers(target);
		const users = createApiContext().services.users;
		const targetId = createUserID(BigInt(target.userId));
		const deleted = await users.findUniqueAssert(targetId);
		const leftoverEmail = createUniqueEmail('leftover');
		const leftover = await users.patchUpsert(
			targetId,
			{
				email: leftoverEmail,
				last_active_ip: '203.0.113.45',
				last_active_at: new Date(),
				locale: 'sv-SE',
				terms_agreed_at: new Date(),
			},
			deleted.toRow(),
		);
		await getUserSearchService()!.indexDocument(convertToSearchableUser(leftover));
		expect((await listedUsers()).map((user) => user.id)).toContain(target.userId);
		expect(await backfillDeletedUsers(users, 2)).toMatchObject({scrubbed: 1});
		expect((await users.findUniqueAssert(targetId)).toRow()).toMatchObject({
			email: null,
			last_active_ip: null,
			last_active_at: null,
			locale: null,
			terms_agreed_at: null,
		});
		expect((await listedUsers()).map((user) => user.id)).not.toContain(target.userId);
		const byIp = await createBuilder<{users: Array<{id: string}>}>(harness, admin.token)
			.get(`/admin/users?last_active_ip=${encodeURIComponent('203.0.113.45')}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(byIp.users).toEqual([]);
		expect(await backfillDeletedUsers(users)).toMatchObject({scrubbed: 0});
		await createTestAccount(harness, {email: leftoverEmail});
	});

	it('keeps the waiting period on the hosted instance', async () => {
		getConfig().instance.selfHosted = false;
		const target = await createTestAccount(harness);
		await deleteNow(target).expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY).execute();
	});

	it('keeps the 14 day floor when immediate is not asked for', async () => {
		getConfig().instance.selfHosted = true;
		const target = await createTestAccount(harness);
		const response = await createBuilder<{user: {pending_deletion_at: string}}>(harness, admin.token)
			.put(`/admin/users/${target.userId}/deletion`)
			.body({days_until_deletion: 1, basis: {type: 'holder_request'}})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(Date.parse(response.user.pending_deletion_at) - Date.now()).toBeGreaterThan(13 * 24 * 3600 * 1000);
	});
});
