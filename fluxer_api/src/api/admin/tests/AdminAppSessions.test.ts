// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getUserActivityBuffer} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

interface AppSessionsResponse {
	sessions: Array<{
		user_id: string;
		app_version: string | null;
		app_build_number: number | null;
		app_beta_number: number | null;
	}>;
	next_cursor: string | null;
}

interface UserSessionsResponse {
	sessions: Array<{
		app_platform: string | null;
		app_version: string | null;
		app_release_channel: string | null;
		app_build_number: number | null;
		app_beta_number: number | null;
	}>;
}

async function requestFromApp(
	harness: ApiTestHarness,
	account: TestAccount,
	properties: Record<string, unknown>,
): Promise<void> {
	const debounced = await harness.kvProvider.scan('auth_session_activity:touched:*', 100);
	if (debounced.length > 0) await harness.kvProvider.del(...debounced);
	await createBuilder(harness, account.token)
		.get('/users/@me')
		.header('user-agent', 'Fluxer iOS/2026.10.100 (beta)')
		.header('x-fluxer-client-properties', Buffer.from(JSON.stringify(properties)).toString('base64'))
		.expect(HTTP_STATUS.OK)
		.execute();
}

describe('Admin app sessions', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	test('records the reported app build on the session and lists it by platform and channel', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.USER_LIST_SESSIONS]);
		const tester = await createTestAccount(harness);
		const garbage = await createTestAccount(harness);
		await requestFromApp(harness, tester, {
			os: 'ios',
			client_platform: 'ios',
			client_version: '2026.10.100',
			release_channel: 'beta',
			build_number: 412,
			beta_number: 14,
		});
		await requestFromApp(harness, garbage, {
			os: 'ios',
			client_platform: 'ios',
			client_version: '<b>2026</b>',
			release_channel: 'beta',
			build_number: '412',
			beta_number: 2 ** 40,
		});
		await getUserActivityBuffer().drainAndFlush();

		const listed = await createBuilder<AppSessionsResponse>(harness, admin.token)
			.get('/admin/app-sessions?platform=ios&release_channel=beta')
			.expect(HTTP_STATUS.OK)
			.execute();
		const byUser = new Map(listed.sessions.map((session) => [session.user_id, session]));
		expect(byUser.get(tester.userId)).toMatchObject({
			app_version: '2026.10.100',
			app_build_number: 412,
			app_beta_number: 14,
		});
		expect(byUser.get(garbage.userId)).toMatchObject({
			app_version: null,
			app_build_number: null,
			app_beta_number: null,
		});
		expect(listed.next_cursor).toBeNull();

		const stable = await createBuilder<AppSessionsResponse>(harness, admin.token)
			.get('/admin/app-sessions?platform=ios&release_channel=stable')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(stable.sessions).toEqual([]);

		const userSessions = await createBuilder<UserSessionsResponse>(harness, admin.token)
			.get(`/admin/users/${tester.userId}/sessions`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(userSessions.sessions[0]).toMatchObject({
			app_platform: 'ios',
			app_version: '2026.10.100',
			app_release_channel: 'beta',
			app_build_number: 412,
			app_beta_number: 14,
		});

		await createBuilder(harness, tester.token).post('/auth/logout').expect(HTTP_STATUS.NO_CONTENT).execute();
		const afterLogout = await createBuilder<AppSessionsResponse>(harness, admin.token)
			.get('/admin/app-sessions?platform=ios&release_channel=beta')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(afterLogout.sessions.map((session) => session.user_id)).toEqual([garbage.userId]);
	});
});
