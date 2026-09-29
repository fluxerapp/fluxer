// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {getConfig} from '@app/api/Config';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {LimitConfigSnapshot} from '@fluxer/limits/src/LimitTypes';
import type {InstanceConfigResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const COMMUNITY_CREATOR_TRAIT = 'community_creator';

interface LimitConfigReadResponse {
	limit_config: LimitConfigSnapshot;
}

describe('guild creation access on a self-hosted instance', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	const asSelfHosted = async <T>(run: () => Promise<T>): Promise<T> => {
		const config = getConfig();
		const originalSelfHosted = config.instance.selfHosted;
		config.instance.selfHosted = true;
		try {
			return await run();
		} finally {
			config.instance.selfHosted = originalSelfHosted;
		}
	};

	const createAdmin = async (): Promise<TestAccount> =>
		await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
			AdminACLs.INSTANCE_LIMIT_CONFIG_VIEW,
			AdminACLs.INSTANCE_LIMIT_CONFIG_UPDATE,
		]);

	// Create members so that they start with no ACLs
	const createMember = async (): Promise<TestAccount> =>
		await setUserACLs(harness, await createTestAccount(harness), []);

	const setGuildCreateAccess = async (admin: TestAccount, enabled: boolean): Promise<void> => {
		const updated = await createBuilder<InstanceConfigResponse>(harness, admin.token)
			.patch('/admin/instance/config')
			.body({policy: {guild_create_access: enabled}})
			.execute();
		expect(updated.policy.guild_create_access).toBe(enabled);
	};

	const readInstanceConfig = async (admin: TestAccount): Promise<InstanceConfigResponse> =>
		await createBuilder<InstanceConfigResponse>(harness, admin.token).get('/admin/instance/config').execute();

	const createGuild = (account: TestAccount, name: string) =>
		createBuilder<GuildResponse>(harness, account.token).post('/guilds').body({name});

	const grantGuildCreateToTrait = async (admin: TestAccount, trait: string): Promise<void> => {
		const current = await createBuilder<LimitConfigReadResponse>(harness, admin.token)
			.get('/admin/limit-config')
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, admin.token)
			.put('/admin/limit-config')
			.body({
				limit_config: {
					traitDefinitions: [...current.limit_config.traitDefinitions, trait],
					rules: [
						...current.limit_config.rules,
						{id: `grant_${trait}`, filters: {traits: [trait]}, limits: {feature_guild_create: 1}},
					],
				},
			})
			.expect(HTTP_STATUS.OK)
			.execute();
	};

	const grantTrait = async (account: TestAccount, trait: string): Promise<void> => {
		await createBuilder(harness, '')
			.patch(`/test/users/${account.userId}/traits`)
			.body({traits: [trait]})
			.expect(HTTP_STATUS.OK)
			.execute();
	};

	it('allows guild creation while the community creation policy is at its default', async () => {
		const admin = await createAdmin();
		expect((await readInstanceConfig(admin)).policy.guild_create_access).toBe(true);

		const member = await createMember();
		await asSelfHosted(async () => {
			const guild = await createGuild(member, 'Default policy community').execute();
			expect(guild.id).toBeTruthy();
		});
	});

	it('stores a disabled policy and rejects guild creation for a member without a grant', async () => {
		const admin = await createAdmin();
		await setGuildCreateAccess(admin, false);
		expect((await readInstanceConfig(admin)).policy.guild_create_access).toBe(false);

		const member = await createMember();
		await asSelfHosted(async () => {
			await createGuild(member, 'Denied community')
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.GUILD_CREATION_PERMISSION_REQUIRED)
				.execute();
		});
	});

	it('allows guild creation only once a member holds the trait the grant rule targets', async () => {
		const admin = await createAdmin();
		await setGuildCreateAccess(admin, false);
		await grantGuildCreateToTrait(admin, COMMUNITY_CREATOR_TRAIT);

		const member = await createMember();
		await asSelfHosted(async () => {
			await createGuild(member, 'Ungranted community')
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.GUILD_CREATION_PERMISSION_REQUIRED)
				.execute();
		});

		await grantTrait(member, COMMUNITY_CREATOR_TRAIT);
		await asSelfHosted(async () => {
			const guild = await createGuild(member, 'Granted community').execute();
			expect(guild.id).toBeTruthy();
		});
	});

	it('allows guild creation for a member holding a wildcard ACL while the policy is disabled', async () => {
		const admin = await createAdmin();
		await setGuildCreateAccess(admin, false);

		const member = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.WILDCARD]);
		await asSelfHosted(async () => {
			const guild = await createGuild(member, 'Wildcard community').execute();
			expect(guild.id).toBeTruthy();
		});
	});
});
