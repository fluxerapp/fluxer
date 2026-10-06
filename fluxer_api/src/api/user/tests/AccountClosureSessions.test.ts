// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomBytes} from 'node:crypto';
import {revokeAllAuthSessions} from '@app/api/auth/AuthSessionRevocation';
import {createTestAccount, loginAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {createApplicationID, createUserID, type UserID} from '@app/api/BrandedTypes';
import {fetchMany} from '@app/api/database/CassandraQueryExecution';
import type {AuthSessionRow} from '@app/api/database/types/AuthTypes';
import {getGatewayService} from '@app/api/middleware/ServiceRegistry';
import {getUserActivityBuffer, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {OAuth2TokenRepository} from '@app/api/oauth/repositories/OAuth2TokenRepository';
import {AuthSessions} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AuthSessionRepository} from '@app/api/user/repositories/auth/AuthSessionRepository';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {TestEmailService} from '@pkgs/email/src/TestEmailService';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

const FETCH_SESSION_ROW_CQL = AuthSessions.selectCql({where: AuthSessions.where.eq('session_id_hash')});

function sessionRow(userId: UserID): AuthSessionRow {
	const now = new Date();
	return {
		user_id: userId,
		session_id_hash: randomBytes(32),
		created_at: now,
		approx_last_used_at: now,
		client_ip: '203.0.113.20',
		client_user_agent: null,
		client_os: null,
		client_country: null,
		version: 1,
	};
}

async function readSessionRows(sessionIdHash: Buffer): Promise<Array<Record<string, unknown>>> {
	return fetchMany<Record<string, unknown>>(FETCH_SESSION_ROW_CQL, {session_id_hash: sessionIdHash});
}

async function markDeleted(userId: UserID): Promise<void> {
	const users = getUserRepository();
	const user = (await users.findUnique(userId))!;
	await users.patchUpsert(userId, {flags: user.flags | UserFlags.DELETED}, user.toRow());
}

describe('Account closure and credentials', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		vi.restoreAllMocks();
		await harness?.shutdown();
	});

	test('revoking every session of a user with 250 sessions uses bounded batches', async () => {
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		const sessions = new AuthSessionRepository();
		for (let index = 0; index < 250; index++) {
			await sessions.createAuthSession(sessionRow(userId));
		}
		const batchSpy = vi.spyOn(InMemoryCassandraQueryExecutor.prototype, 'executeBatch');
		const revoked = await revokeAllAuthSessions({users: getUserRepository(), gateway: getGatewayService()}, userId);
		expect(revoked).toBe(251);
		expect(Math.max(...batchSpy.mock.calls.map(([queries]) => queries.length))).toBeLessThanOrEqual(60);
		expect(await sessions.listAuthSessions(userId)).toHaveLength(0);
		expect(await sessions.listAuthSessionTombstones(userId)).toHaveLength(251);
		await createBuilder(harness, account.token).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
	});

	test('a session of a user with the DELETED flag is refused and revoked', async () => {
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		await markDeleted(userId);
		await createBuilder(harness, account.token).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
		await vi.waitFor(async () => {
			expect(await new AuthSessionRepository().listAuthSessions(userId)).toHaveLength(0);
		});
	});

	test('a session of a user whose deletion has started is refused', async () => {
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		const users = getUserRepository();
		const user = (await users.findUnique(userId))!;
		const pendingDeletionAt = new Date();
		await users.updateDeletionSchedule(user, {
			flags: user.flags | UserFlags.SELF_DELETED,
			pending_deletion_at: pendingDeletionAt,
		});
		expect(await users.startDeletion(userId, pendingDeletionAt)).not.toBeNull();
		await createBuilder(harness, account.token).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
	});

	test('gateway identify rejects a session of a closed account', async () => {
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		await createBuilder(harness, '')
			.post('/test/rpc-session-init')
			.body({type: 'session', token: account.token, version: 1, ip: '127.0.0.1'})
			.expect(HTTP_STATUS.OK)
			.execute();
		await markDeleted(userId);
		const response = await harness.requestJson({
			path: '/test/rpc-session-init',
			method: 'POST',
			body: {type: 'session', token: account.token, version: 1, ip: '127.0.0.1'},
		});
		expect(response.status).toBe(HTTP_STATUS.UNAUTHORIZED);
	});

	test('a session of a temporarily banned user is refused on every request and at identify', async () => {
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		const other = await createTestAccount(harness);
		const users = getUserRepository();
		const user = (await users.findUnique(userId))!;
		await users.patchUpsert(
			userId,
			{flags: user.flags | UserFlags.DISABLED, temp_banned_until: new Date(Date.now() + 3_600_000)},
			user.toRow(),
		);
		await createBuilder(harness, account.token).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
		const identify = await harness.requestJson({
			path: '/test/rpc-session-init',
			method: 'POST',
			body: {type: 'session', token: other.token, version: 1, ip: '127.0.0.1'},
		});
		expect(identify.status).toBe(HTTP_STATUS.OK);
		const otherUser = (await users.findUnique(createUserID(BigInt(other.userId))))!;
		await users.patchUpsert(
			otherUser.id,
			{flags: otherUser.flags | UserFlags.DISABLED, temp_banned_until: new Date(Date.now() + 3_600_000)},
			otherUser.toRow(),
		);
		const refused = await harness.requestJson({
			path: '/test/rpc-session-init',
			method: 'POST',
			body: {type: 'session', token: other.token, version: 1, ip: '127.0.0.1'},
		});
		expect(refused.status).toBe(HTTP_STATUS.UNAUTHORIZED);
	});

	test('a session outlives a temporary ban that has already ended', async () => {
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		const users = getUserRepository();
		const user = (await users.findUnique(userId))!;
		await users.patchUpsert(
			userId,
			{flags: user.flags | UserFlags.DISABLED, temp_banned_until: new Date(Date.now() - 60_000)},
			user.toRow(),
		);
		await createBuilder(harness, account.token).get('/users/@me').expect(HTTP_STATUS.OK).execute();
	});

	test('a disabled account can still sign in again by logging in', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post('/users/@me/disable')
			.body({password: account.password})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const login = await loginAccount(harness, account);
		await createBuilder(harness, login.token).get('/users/@me').expect(HTTP_STATUS.OK).execute();
	});

	test('the activity flush after a revoke does not recreate the session row', async () => {
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		const sessions = new AuthSessionRepository();
		const row = sessionRow(userId);
		await sessions.createAuthSession(row);
		const buffer = getUserActivityBuffer();
		expect(await buffer.recordAuthSessionActivity(row.session_id_hash, new Date())).toBe(true);
		await sessions.deleteAuthSessions(userId, [row.session_id_hash]);
		const encoded = row.session_id_hash.toString('base64url');
		expect((await harness.kvProvider.hgetall('auth_session_activity:pending'))[encoded]).toBeUndefined();
		await harness.kvProvider.hset('auth_session_activity:pending', encoded, JSON.stringify({ts: Date.now()}));
		await buffer.drainAndFlush();
		expect(await readSessionRows(row.session_id_hash)).toHaveLength(0);
	});

	test('the activity flush still records use of a live session', async () => {
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		const sessions = new AuthSessionRepository();
		const row = {...sessionRow(userId), approx_last_used_at: new Date(Date.now() - 86_400_000)};
		await sessions.createAuthSession(row);
		const usedAt = new Date();
		await getUserActivityBuffer().recordAuthSessionActivity(row.session_id_hash, usedAt);
		await getUserActivityBuffer().drainAndFlush();
		const [stored] = await sessions
			.listAuthSessions(userId)
			.then((list) => list.filter((session) => session.sessionIdHash.equals(row.session_id_hash)));
		expect(stored?.approximateLastUsedAt.getTime()).toBe(usedAt.getTime());
	});

	test('scheduling a deletion revokes every session and OAuth token', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'user:delete']);
		const target = await createTestAccount(harness);
		const userId = createUserID(BigInt(target.userId));
		const tokens = new OAuth2TokenRepository();
		const applicationId = createApplicationID(1234567890123456789n);
		for (let index = 0; index < 40; index++) {
			await tokens.createAccessToken({
				token_: `access-${index}-${target.userId}`,
				application_id: applicationId,
				user_id: userId,
				scope: new Set(['identify']),
				created_at: new Date(),
			});
			await tokens.createRefreshToken({
				token_: `refresh-${index}-${target.userId}`,
				application_id: applicationId,
				user_id: userId,
				scope: new Set(['identify']),
				created_at: new Date(),
			});
		}
		await createBuilder(harness, admin.token)
			.put(`/admin/users/${target.userId}/deletion`)
			.body({reason_code: DeletionReasons.SPAM, days_until_deletion: 60})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(await new AuthSessionRepository().listAuthSessions(userId)).toHaveLength(0);
		expect(await tokens.getAccessToken(`access-0-${target.userId}`)).toBeNull();
		expect(await tokens.listRefreshTokensForUser(userId)).toHaveLength(0);
	});

	test('self deletion revokes sessions even when the confirmation email fails', async () => {
		const account = await createTestAccount(harness);
		vi.spyOn(TestEmailService.prototype, 'sendSelfDeletionScheduledEmail').mockRejectedValue(new Error('mail down'));
		const response = await harness.requestJson({
			path: '/users/@me/delete',
			method: 'POST',
			body: {password: account.password},
			headers: {Authorization: account.token},
		});
		expect(response.status).toBeGreaterThanOrEqual(400);
		expect(await new AuthSessionRepository().listAuthSessions(createUserID(BigInt(account.userId)))).toHaveLength(0);
	});
});
