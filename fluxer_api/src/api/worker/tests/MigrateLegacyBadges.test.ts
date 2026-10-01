// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {TEST_BADGE_ICON} from '@app/api/badge/tests/BadgeTestUtils';
import {Config} from '@app/api/Config';
import {getGatewayService, getSnowflakeService} from '@app/api/middleware/ServiceRegistry';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {server} from '@app/api/test/msw/server';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import migrateLegacyBadges from '@app/api/worker/tasks/MigrateLegacyBadges';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import type {BadgesResponse} from '@fluxer/schema/src/domains/badge/BadgeSchemas';
import type {UserProfileFullResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {HttpResponse, http} from 'msw';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const WORKER_HELPERS = {logger: new NoopLogger()} as unknown as WorkerTaskHelpers;

describe('migrateLegacyBadges', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;

	async function createFlaggedAccount(flags: bigint): Promise<TestAccount> {
		const account = await createTestAccount(harness);
		await createBuilder(harness, admin.token)
			.patch(`/admin/users/${account.userId}/flags`)
			.body({add_flags: [flags.toString()], remove_flags: []})
			.execute();
		return account;
	}

	async function profileBadges(account: TestAccount): Promise<Array<string> | undefined> {
		const profile = await createBuilder<UserProfileFullResponse>(harness, account.token)
			.get(`/users/${account.userId}/profile`)
			.execute();
		return profile.badges;
	}

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		admin = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.WILDCARD]);
		server.use(http.get(`${Config.endpoints.staticCdn}/badges/:file`, () => HttpResponse.text(TEST_BADGE_ICON)));
		setWorkerDependenciesForTest({
			userRepository: getUserRepository(),
			snowflakeService: getSnowflakeService(),
			gatewayService: getGatewayService(),
		});
	});

	afterEach(() => {
		clearWorkerDependencies();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	it('converts legacy flag badges into assigned platform badges once', async () => {
		const staff = await createFlaggedAccount(UserFlags.STAFF);
		const partner = await createFlaggedAccount(UserFlags.PARTNER | UserFlags.BUG_HUNTER);
		await migrateLegacyBadges({}, WORKER_HELPERS);
		await migrateLegacyBadges({}, WORKER_HELPERS);
		const {badges} = await createBuilderWithoutAuth<BadgesResponse>(harness).get('/badges').execute();
		const idsByName = new Map(badges.map((badge) => [badge.name, badge.id]));
		expect(badges.map((badge) => badge.name).sort()).toEqual(['Bug Hunter', 'Partner']);
		expect(badges.every((badge) => badge.type === 'user' && badge.icon.startsWith('<svg'))).toBe(true);
		expect(await profileBadges(staff)).toBeUndefined();
		expect((await profileBadges(partner))?.sort()).toEqual(
			[idsByName.get('Partner'), idsByName.get('Bug Hunter')].sort(),
		);
	});
});
