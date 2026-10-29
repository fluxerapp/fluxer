// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAuthHarness,
	createTestAccount,
	createUniqueEmail,
	disableSso,
	enableSso,
	setUserACLs,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {addMemberRole, createGuild, createRole, getMember} from '@app/api/guild/tests/GuildTestUtils';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('SSO groups to community roles', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	let owner: TestAccount;
	let guildId: string;
	let staffRole: string;
	let devRole: string;
	let handRole: string;
	let foreignRole: string;

	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		await getInstanceConfigRepository().setAppPublicConfig({setup: {configured: true}});
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
		]);
		owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Company');
		guildId = guild.id;
		staffRole = (await createRole(harness, owner.token, guildId, {name: 'Staff'})).id;
		devRole = (await createRole(harness, owner.token, guildId, {name: 'Developers'})).id;
		handRole = (await createRole(harness, owner.token, guildId, {name: 'Given by hand'})).id;
		const other = await createGuild(harness, owner.token, 'Elsewhere');
		foreignRole = (await createRole(harness, owner.token, other.id, {name: 'Foreign'})).id;
		await createBuilder(harness, admin.token)
			.patch('/admin/instance/config')
			.body({policy: {single_community_enabled: true, single_community_guild_id: guildId}})
			.execute();
	});
	afterEach(async () => {
		await disableSso(harness, admin.token);
		getInstanceConfigRepository().clearCacheForTesting();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	function subject(label: string): string {
		return `itest-${label}-${Date.now()}-${Math.random().toString(36).slice(2)}`;
	}

	async function enableWithMappings(extra: Record<string, unknown> = {}): Promise<void> {
		await enableSso(harness, admin.token, {
			enforced: false,
			allowed_domains: [],
			group_role_mappings: [
				{group: 'staff', role_id: staffRole},
				{group: '/engineering/devs', role_id: devRole},
			],
			...extra,
		});
	}

	async function signIn(claims: Record<string, unknown>): Promise<string> {
		const {state, client_binding} = await createBuilderWithoutAuth<{state: string; client_binding: string}>(harness)
			.post('/auth/sso/start')
			.body({})
			.execute();
		const login = await createBuilderWithoutAuth<{user_id: string}>(harness)
			.post('/auth/sso/complete')
			.body({code: JSON.stringify(claims), state, client_binding})
			.expect(200)
			.execute();
		return login.user_id;
	}

	async function rolesOf(userId: string): Promise<Array<string>> {
		const member = await getMember(harness, owner.token, guildId, userId);
		return [...member.roles].sort();
	}

	it('gives a new account the roles of its groups', async () => {
		await enableWithMappings();
		const userId = await signIn({sub: subject('new'), email: createUniqueEmail('sso-groups'), groups: ['staff']});
		expect(await rolesOf(userId)).toEqual([staffRole]);
	});

	it('follows the groups on every sign-in and leaves roles it does not manage', async () => {
		await enableWithMappings();
		const claims = {sub: subject('change'), email: createUniqueEmail('sso-groups-change')};
		const userId = await signIn({...claims, groups: ['staff']});
		await addMemberRole(harness, owner.token, guildId, userId, handRole);
		await signIn({...claims, groups: ['/engineering/devs', 'unmapped']});
		expect(await rolesOf(userId)).toEqual([devRole, handRole].sort());
		await signIn({...claims, groups: []});
		expect(await rolesOf(userId)).toEqual([handRole]);
	});

	it('removes the mapped roles when the provider sends no groups claim', async () => {
		await enableWithMappings();
		const claims = {sub: subject('absent'), email: createUniqueEmail('sso-groups-absent')};
		const userId = await signIn({...claims, groups: ['staff']});
		await addMemberRole(harness, owner.token, guildId, userId, handRole);
		await signIn(claims);
		expect(await rolesOf(userId)).toEqual([handRole]);
	});

	it('removes the mapped roles when the provider has too many groups to send', async () => {
		await enableWithMappings();
		const claims = {sub: subject('overage'), email: createUniqueEmail('sso-groups-overage')};
		const userId = await signIn({...claims, groups: ['staff']});
		await signIn({...claims, _claim_names: {groups: 'src1'}, _claim_sources: {src1: {endpoint: 'https://graph'}}});
		expect(await rolesOf(userId)).toEqual([]);
	});

	it('leaves the roles alone when no groups are mapped', async () => {
		await enableWithMappings();
		const claims = {sub: subject('unmapped'), email: createUniqueEmail('sso-groups-unmapped')};
		const userId = await signIn({...claims, groups: ['staff']});
		await createBuilder(harness, admin.token)
			.patch('/admin/instance/config')
			.body({sso: {group_role_mappings: []}})
			.execute();
		await signIn(claims);
		expect(await rolesOf(userId)).toEqual([staffRole]);
	});

	it('reads the groups from the claim the admin names', async () => {
		await enableWithMappings({groups_claim: 'roles'});
		const userId = await signIn({
			sub: subject('claim'),
			email: createUniqueEmail('sso-groups-claim'),
			groups: ['staff'],
			roles: ['/engineering/devs'],
		});
		expect(await rolesOf(userId)).toEqual([devRole]);
	});

	it.each([
		['a role of another community', () => foreignRole],
		['the everyone role', () => guildId],
	])('refuses %s', async (_label, roleId) => {
		await createBuilder(harness, admin.token)
			.patch('/admin/instance/config')
			.body({sso: {group_role_mappings: [{group: 'staff', role_id: roleId()}]}})
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
	});
});
