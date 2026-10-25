// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomBytes} from 'node:crypto';
import {revokeAllAuthSessions} from '@app/api/auth/AuthSessionRevocation';
import {createTestAccount, loginAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {createApplicationID, createUserID, type UserID} from '@app/api/BrandedTypes';
import {getConfig} from '@app/api/Config';
import {createApiContext} from '@app/api/CreateApiContext';
import {fetchMany} from '@app/api/database/CassandraQueryExecution';
import type {AuthSessionRow} from '@app/api/database/types/AuthTypes';
import {decisionBody} from '@app/api/enforcement/tests/EnforcementTestUtils';
import {getGatewayService} from '@app/api/middleware/ServiceRegistry';
import {
	getKVAccountDeletionQueue,
	getUserActivityBuffer,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import {OAuth2TokenRepository} from '@app/api/oauth/repositories/OAuth2TokenRepository';
import {AuthSessions} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createRecordingSmtpServer} from '@app/api/test/RecordingSmtpServer';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AuthSessionRepository} from '@app/api/user/repositories/auth/AuthSessionRepository';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {MS_PER_DAY, MS_PER_HOUR, MS_PER_MINUTE} from '@fluxer/date_utils/src/DateConstants';
import {EmailService} from '@pkgs/email/src/EmailService';
import {SmtpEmailProvider} from '@pkgs/email/src/SmtpEmailProvider';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const SESSION_HASH_BYTES = 32;
const EXTRA_SESSION_COUNT = 250;
const OAUTH_TOKEN_COUNT = 40;
const DELETION_GRACE_HOURS = 24;
const DELETION_QUEUE_LIMIT = 10;

const FETCH_SESSION_ROW_CQL = AuthSessions.selectCql({where: AuthSessions.where.eq('session_id_hash')});

function sessionRow(userId: UserID): AuthSessionRow {
	const now = new Date();
	return {
		user_id: userId,
		session_id_hash: randomBytes(SESSION_HASH_BYTES),
		created_at: now,
		approx_last_used_at: now,
		client_ip: '203.0.113.20',
		client_user_agent: null,
		client_os: null,
		client_country: null,
		app_platform: null,
		app_version: null,
		app_release_channel: null,
		app_build_number: null,
		app_beta_number: null,
		version: 1,
	};
}

async function storedUser(userId: UserID) {
	const user = await getUserRepository().findUnique(userId);
	if (!user) throw new Error('Expected stored account');
	return user;
}

async function markDeleted(userId: UserID): Promise<void> {
	const users = getUserRepository();
	const user = await storedUser(userId);
	await users.patchUpsert(userId, {flags: user.flags | UserFlags.DELETED}, user.toRow());
}

describe('Account closure and credentials', () => {
	let harness: ApiTestHarness;
	let smtpCleanup: (() => Promise<void>) | undefined;
	const deletionGraceHours = getConfig().deletionGracePeriodHours;
	beforeEach(async () => {
		harness = await createApiTestHarness();
		smtpCleanup = undefined;
	});
	afterEach(async () => {
		try {
			await smtpCleanup?.();
		} finally {
			getConfig().deletionGracePeriodHours = deletionGraceHours;
			await harness?.shutdown();
		}
	});

	test('revoking every session of a user with 250 sessions persists all tombstones', async () => {
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		const sessions = new AuthSessionRepository();
		for (let index = 0; index < EXTRA_SESSION_COUNT; index++) {
			await sessions.createAuthSession(sessionRow(userId));
		}
		const revoked = await revokeAllAuthSessions({users: getUserRepository(), gateway: getGatewayService()}, userId);
		expect(revoked).toBe(251);
		expect(await sessions.listAuthSessions(userId)).toHaveLength(0);
		expect(await sessions.listAuthSessionTombstones(userId)).toHaveLength(251);
		await createBuilder(harness, account.token).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
	});

	test('a session of a user with the DELETED flag is refused', async () => {
		const account = await createTestAccount(harness);
		await markDeleted(createUserID(BigInt(account.userId)));
		await createBuilder(harness, account.token).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
	});

	test('a session of a user whose deletion has started is refused', async () => {
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		const users = getUserRepository();
		const user = await storedUser(userId);
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
		const user = await storedUser(userId);
		await users.patchUpsert(
			userId,
			{flags: user.flags | UserFlags.DISABLED, temp_banned_until: new Date(Date.now() + MS_PER_HOUR)},
			user.toRow(),
		);
		await createBuilder(harness, account.token).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
		const identify = await harness.requestJson({
			path: '/test/rpc-session-init',
			method: 'POST',
			body: {type: 'session', token: other.token, version: 1, ip: '127.0.0.1'},
		});
		expect(identify.status).toBe(HTTP_STATUS.OK);
		const otherUser = await storedUser(createUserID(BigInt(other.userId)));
		await users.patchUpsert(
			otherUser.id,
			{flags: otherUser.flags | UserFlags.DISABLED, temp_banned_until: new Date(Date.now() + MS_PER_HOUR)},
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
		const user = await storedUser(userId);
		await users.patchUpsert(
			userId,
			{flags: user.flags | UserFlags.DISABLED, temp_banned_until: new Date(Date.now() - MS_PER_MINUTE)},
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
		expect(
			await fetchMany<Record<string, unknown>>(FETCH_SESSION_ROW_CQL, {session_id_hash: row.session_id_hash}),
		).toEqual([]);
	});

	test('the activity flush still records use of a live session', async () => {
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		const sessions = new AuthSessionRepository();
		const row = {...sessionRow(userId), approx_last_used_at: new Date(Date.now() - MS_PER_DAY)};
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
		for (let index = 0; index < OAUTH_TOKEN_COUNT; index++) {
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
			.header('X-Audit-Log-Reason', 'Spam review')
			.body({
				days_until_deletion: 74,
				basis: {
					type: 'enforcement',
					decision: decisionBody(),
				},
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(await new AuthSessionRepository().listAuthSessions(userId)).toHaveLength(0);
		expect(await tokens.getAccessToken(`access-0-${target.userId}`)).toBeNull();
		expect(await tokens.listRefreshTokensForUser(userId)).toHaveLength(0);
	});

	test('self deletion revokes sessions before a refused confirmation email completes', async () => {
		getConfig().deletionGracePeriodHours = DELETION_GRACE_HOURS;
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		const smtp = await createRecordingSmtpServer({firstDelivery: 'hold-and-reject'});
		const provider = new SmtpEmailProvider({
			host: '127.0.0.1',
			port: smtp.port,
			username: '',
			password: '',
			tlsMode: 'none',
		});
		const services = createApiContext().services;
		const previous = services.email;
		let deletionRequest: ReturnType<ApiTestHarness['requestJson']> | undefined;
		smtpCleanup = async () => {
			smtp.release();
			try {
				await deletionRequest;
			} finally {
				services.email = previous;
				try {
					await provider.close();
				} finally {
					await smtp.close();
				}
			}
		};
		services.email = new EmailService(
			{
				enabled: true,
				fromEmail: 'accounts@example.com',
				fromName: 'Closure test',
				appBaseUrl: 'https://app.example.com',
				apiBaseUrl: 'https://api.example.com',
				appealsEmail: 'appeals@example.com',
				privacyEmail: 'privacy@example.com',
				safetyEmail: null,
				supportEmail: 'support@example.com',
				productName: 'Closure test',
				marketingBaseUrl: 'https://example.com',
				staticCdnBaseUrl: 'https://static.example.com',
				legalAddress: null,
				brandLogo: false,
			},
			provider,
		);
		const requestedAt = Date.now();
		deletionRequest = harness.requestJson({
			path: '/users/@me/delete',
			method: 'POST',
			body: {password: account.password},
			headers: {Authorization: account.token},
		});
		await smtp.entered;
		expect(await new AuthSessionRepository().listAuthSessions(userId)).toEqual([]);
		expect(smtp.delivered).toEqual([]);
		expect(smtp.attempts).toHaveLength(1);
		expect(smtp.attempts[0]?.from).toBe('<accounts@example.com>');
		expect(smtp.attempts[0]?.to).toBe(`<${account.email}>`);
		smtp.release();
		const response = await deletionRequest;
		const respondedAt = Date.now();
		expect(response.status).toBe(HTTP_STATUS.NO_CONTENT);
		expect(smtp.delivered).toEqual([]);
		expect(await new AuthSessionRepository().listAuthSessions(userId)).toEqual([]);
		const user = await storedUser(userId);
		const pendingDeletionAt = user.pendingDeletionAt?.getTime() ?? 0;
		expect(pendingDeletionAt).toBeGreaterThanOrEqual(requestedAt + DELETION_GRACE_HOURS * MS_PER_HOUR);
		expect(pendingDeletionAt).toBeLessThanOrEqual(respondedAt + DELETION_GRACE_HOURS * MS_PER_HOUR);
		expect(user.flags & UserFlags.SELF_DELETED).toBe(UserFlags.SELF_DELETED);
		expect(await getKVAccountDeletionQueue().getReadyDeletions(pendingDeletionAt, DELETION_QUEUE_LIMIT)).toEqual([
			{userId, deletionReasonCode: 1},
		]);
		await createBuilder(harness, account.token).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
	});
});
