// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {acceptInvite, createChannelInvite, createGuild, getChannel} from '@app/api/guild/tests/GuildTestUtils';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {grantPremium} from '@app/api/user/tests/UserTestUtils';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {
	DEFAULT_DOMAIN_MIGRATION_CONFIG,
	INERT_DOMAIN_MIGRATION_ASSIGNMENT,
} from '@fluxer/schema/src/domains/admin/DomainMigrationSchemas';
import {
	DEFAULT_PLUTONIUM_PAGE_CONFIG,
	INERT_PLUTONIUM_PAGE_ASSIGNMENT,
} from '@fluxer/schema/src/domains/admin/PlutoniumPageSchemas';
import {
	DEFAULT_EXPERIMENT_POLL_INTERVAL_SECONDS,
	DEFAULT_EXPERIMENT_POLL_JITTER_PERCENT,
	type ExperimentAssignmentsResponse,
	type ExperimentDeliveryConfigResponse,
	readDomainMigrationAssignment,
	readPlutoniumPageAssignment,
} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const NOT_MODIFIED = 304;
const ENDPOINT = '/experiments';

describe('GET /experiments', () => {
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

	it('rejects an unauthenticated caller', async () => {
		await createBuilderWithoutAuth(harness).get(ENDPOINT).expect(HTTP_STATUS.UNAUTHORIZED).execute();
	});

	it('returns the default delivery cadence and the inert assignment while the feature is disabled', async () => {
		const account = await createTestAccount(harness);

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(body).toEqual({
			poll_interval_seconds: DEFAULT_EXPERIMENT_POLL_INTERVAL_SECONDS,
			poll_jitter_percent: DEFAULT_EXPERIMENT_POLL_JITTER_PERCENT,
			assignments: {
				domain_migration: INERT_DOMAIN_MIGRATION_ASSIGNMENT,
				plutonium_page: INERT_PLUTONIUM_PAGE_ASSIGNMENT,
			},
		});
	});

	it('populates the domain migration assignment key even when the rollout is disabled', async () => {
		const account = await createTestAccount(harness);

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(Object.hasOwn(body.assignments, 'domain_migration')).toBe(true);
		expect(readDomainMigrationAssignment(body).enabled).toBe(false);
	});

	it('populates the plutonium page assignment key even when the rollout is disabled', async () => {
		const account = await createTestAccount(harness);

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(Object.hasOwn(body.assignments, 'plutonium_page')).toBe(true);
		expect(readPlutoniumPageAssignment(body).enabled).toBe(false);
	});

	it('resolves the domain migration caller through the allowlist', async () => {
		const targeted = await createTestAccount(harness);
		const untargeted = await createTestAccount(harness);
		await getInstanceConfigRepository().setDomainMigrationConfig({
			...DEFAULT_DOMAIN_MIGRATION_CONFIG,
			enabled: true,
			config_version: 4,
			rollout_basis_points: 0,
			included_user_ids: [targeted.userId],
		});

		const targetedBody = await createBuilder<ExperimentAssignmentsResponse>(harness, targeted.token)
			.get(ENDPOINT)
			.execute();
		expect(targetedBody.assignments.domain_migration).toEqual({enabled: true});

		const untargetedBody = await createBuilder<ExperimentAssignmentsResponse>(harness, untargeted.token)
			.get(ENDPOINT)
			.execute();
		expect(untargetedBody.assignments.domain_migration).toEqual({enabled: false});
	});

	it('keeps the domain migration exclusion ahead of a full rollout', async () => {
		const excluded = await createTestAccount(harness);
		await getInstanceConfigRepository().setDomainMigrationConfig({
			...DEFAULT_DOMAIN_MIGRATION_CONFIG,
			enabled: true,
			rollout_basis_points: 10000,
			included_user_ids: [excluded.userId],
			excluded_user_ids: [excluded.userId],
		});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, excluded.token).get(ENDPOINT).execute();

		expect(body.assignments.domain_migration).toEqual({enabled: false});
	});

	it('resolves the plutonium page caller through the allowlist and the exclusion list', async () => {
		const targeted = await createTestAccount(harness);
		const untargeted = await createTestAccount(harness);
		const excluded = await createTestAccount(harness);
		await getInstanceConfigRepository().setPlutoniumPageConfig({
			...DEFAULT_PLUTONIUM_PAGE_CONFIG,
			enabled: true,
			rollout_basis_points: 0,
			included_user_ids: [targeted.userId, excluded.userId],
			excluded_user_ids: [excluded.userId],
		});

		const targetedBody = await createBuilder<ExperimentAssignmentsResponse>(harness, targeted.token)
			.get(ENDPOINT)
			.execute();
		expect(targetedBody.assignments.plutonium_page).toEqual({enabled: true});

		const untargetedBody = await createBuilder<ExperimentAssignmentsResponse>(harness, untargeted.token)
			.get(ENDPOINT)
			.execute();
		expect(untargetedBody.assignments.plutonium_page).toEqual({enabled: false});

		const excludedBody = await createBuilder<ExperimentAssignmentsResponse>(harness, excluded.token)
			.get(ENDPOINT)
			.execute();
		expect(excludedBody.assignments.plutonium_page).toEqual({enabled: false});
	});

	it('keeps the plutonium page exclusion ahead of a full rollout', async () => {
		const excluded = await createTestAccount(harness);
		const other = await createTestAccount(harness);
		await getInstanceConfigRepository().setPlutoniumPageConfig({
			...DEFAULT_PLUTONIUM_PAGE_CONFIG,
			enabled: true,
			rollout_basis_points: 10000,
			excluded_user_ids: [excluded.userId],
		});

		const excludedBody = await createBuilder<ExperimentAssignmentsResponse>(harness, excluded.token)
			.get(ENDPOINT)
			.execute();
		expect(excludedBody.assignments.plutonium_page).toEqual({enabled: false});

		const otherBody = await createBuilder<ExperimentAssignmentsResponse>(harness, other.token).get(ENDPOINT).execute();
		expect(otherBody.assignments.plutonium_page).toEqual({enabled: true});
	});

	it('enrols members of an included guild in every experiment and leaves everyone else out', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const outsider = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Experiment Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, member.token, invite.code);
		const repository = getInstanceConfigRepository();
		await repository.setDomainMigrationConfig({
			...DEFAULT_DOMAIN_MIGRATION_CONFIG,
			enabled: true,
			included_guild_ids: [guild.id],
		});
		await repository.setPlutoniumPageConfig({
			...DEFAULT_PLUTONIUM_PAGE_CONFIG,
			enabled: true,
			included_guild_ids: [guild.id],
		});

		const memberBody = await createBuilder<ExperimentAssignmentsResponse>(harness, member.token)
			.get(ENDPOINT)
			.execute();
		expect(memberBody.assignments.domain_migration).toEqual({enabled: true});
		expect(memberBody.assignments.plutonium_page).toEqual({enabled: true});

		const outsiderBody = await createBuilder<ExperimentAssignmentsResponse>(harness, outsider.token)
			.get(ENDPOINT)
			.execute();
		expect(outsiderBody.assignments.domain_migration).toEqual({enabled: false});
		expect(outsiderBody.assignments.plutonium_page).toEqual({enabled: false});
	});

	it('enrols premium users, subscription and lifetime alike, when the switch is on', async () => {
		const subscriber = await createTestAccount(harness);
		const visionary = await createTestAccount(harness);
		const free = await createTestAccount(harness);
		await grantPremium(harness, subscriber.userId, UserPremiumTypes.SUBSCRIPTION);
		await grantPremium(harness, visionary.userId, UserPremiumTypes.LIFETIME);
		const repository = getInstanceConfigRepository();
		await repository.setDomainMigrationConfig({
			...DEFAULT_DOMAIN_MIGRATION_CONFIG,
			enabled: true,
			include_premium_users: true,
		});
		await repository.setPlutoniumPageConfig({
			...DEFAULT_PLUTONIUM_PAGE_CONFIG,
			enabled: true,
			include_premium_users: true,
		});
		for (const [account, expected] of [
			[subscriber, true],
			[visionary, true],
			[free, false],
		] as const) {
			const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();
			expect(body.assignments.domain_migration).toEqual({enabled: expected});
			expect(body.assignments.plutonium_page).toEqual({enabled: expected});
		}
	});

	it('stores the guild ids and premium switch an admin sets for each experiment', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);
		const guildIds = ['1500000000000000001', '1500000000000000002'];
		const body = await createBuilder<
			Record<'domain_migration' | 'plutonium_page', {included_guild_ids: Array<string>; include_premium_users: boolean}>
		>(harness, admin.token)
			.patch('/admin/instance/config')
			.body({
				domain_migration: {included_guild_ids: guildIds, include_premium_users: true},
				plutonium_page: {included_guild_ids: guildIds, include_premium_users: true},
			})
			.execute();
		for (const section of [body.domain_migration, body.plutonium_page]) {
			expect(section.included_guild_ids).toEqual(guildIds);
			expect(section.include_premium_users).toBe(true);
		}
	});

	it('revalidates with a strong etag and answers 304 when nothing changed', async () => {
		const account = await createTestAccount(harness);

		const first = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token)
			.get(ENDPOINT)
			.executeWithResponse();
		const etag = first.response.headers.get('etag');
		expect(etag).toMatch(/^"[0-9a-f]{64}"$/);
		expect(first.response.headers.get('cache-control')).toBe('private, no-cache');
		expect(first.response.headers.get('vary')).toBe('Authorization, Origin');

		const revalidated = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token)
			.get(ENDPOINT)
			.header('If-None-Match', etag as string)
			.expect(NOT_MODIFIED)
			.executeWithResponse();
		expect(revalidated.response.status).toBe(NOT_MODIFIED);
		expect(revalidated.json).toBeUndefined();
		expect(revalidated.response.headers.get('etag')).toBe(etag);
		expect(revalidated.response.headers.get('vary')).toBe('Authorization, Origin');
	});

	it('lets a cross-origin client send If-None-Match and read the etag back', async () => {
		const preflight = await harness.requestJson({path: ENDPOINT, method: 'OPTIONS'});

		expect(preflight.headers.get('access-control-allow-headers')).toContain('If-None-Match');
		expect(preflight.headers.get('access-control-expose-headers')).toContain('ETag');
	});

	it('serves a fresh body once the domain migration config changes', async () => {
		const account = await createTestAccount(harness);

		const first = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token)
			.get(ENDPOINT)
			.executeWithResponse();
		const staleEtag = first.response.headers.get('etag') as string;

		await getInstanceConfigRepository().setDomainMigrationConfig({
			...DEFAULT_DOMAIN_MIGRATION_CONFIG,
			enabled: true,
			config_version: 1,
			rollout_basis_points: 10000,
		});

		const refreshed = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token)
			.get(ENDPOINT)
			.header('If-None-Match', staleEtag)
			.executeWithResponse();
		expect(refreshed.response.status).toBe(HTTP_STATUS.OK);
		expect(refreshed.response.headers.get('etag')).not.toBe(staleEtag);
		expect(refreshed.json?.assignments.domain_migration).toEqual({enabled: true});
	});

	it('serves a fresh body once the delivery config changes', async () => {
		const account = await createTestAccount(harness);

		const first = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token)
			.get(ENDPOINT)
			.executeWithResponse();
		const staleEtag = first.response.headers.get('etag') as string;

		await getInstanceConfigRepository().setExperimentDeliveryConfig({
			poll_interval_seconds: 1800,
			poll_jitter_percent: 5,
		});

		const refreshed = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token)
			.get(ENDPOINT)
			.header('If-None-Match', staleEtag)
			.executeWithResponse();
		expect(refreshed.response.status).toBe(HTTP_STATUS.OK);
		expect(refreshed.response.headers.get('etag')).not.toBe(staleEtag);
		expect(refreshed.json?.poll_interval_seconds).toBe(1800);
		expect(refreshed.json?.poll_jitter_percent).toBe(5);
	});

	it('bumps the domain migration config version on every admin update without the client sending one', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);

		const afterFirst = await createBuilder<{domain_migration: {config_version: number; enabled: boolean}}>(
			harness,
			admin.token,
		)
			.patch('/admin/instance/config')
			.body({domain_migration: {enabled: true, rollout_basis_points: 10000}})
			.execute();
		expect(afterFirst.domain_migration).toMatchObject({config_version: 1, enabled: true});

		const afterSecond = await createBuilder<{
			domain_migration: {config_version: number; enabled: boolean; anonymous_rollout_basis_points: number};
		}>(harness, admin.token)
			.patch('/admin/instance/config')
			.body({domain_migration: {anonymous_rollout_basis_points: 2500}})
			.execute();
		expect(afterSecond.domain_migration).toMatchObject({
			config_version: 2,
			enabled: true,
			anonymous_rollout_basis_points: 2500,
		});

		const afterEmpty = await createBuilder<{domain_migration: {config_version: number; enabled: boolean}}>(
			harness,
			admin.token,
		)
			.patch('/admin/instance/config')
			.body({domain_migration: {}})
			.execute();
		expect(afterEmpty.domain_migration).toMatchObject({config_version: 2, enabled: true});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, admin.token).get(ENDPOINT).execute();
		expect(body.assignments.domain_migration).toEqual({enabled: true});
	});

	it('leaves the domain migration config version alone for an admin update that sets no field', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);

		const afterFirst = await createBuilder<{domain_migration: {config_version: number; enabled: boolean}}>(
			harness,
			admin.token,
		)
			.patch('/admin/instance/config')
			.body({domain_migration: {enabled: true}})
			.execute();
		expect(afterFirst.domain_migration).toMatchObject({config_version: 1, enabled: true});

		const afterEmpty = await createBuilder<{domain_migration: {config_version: number; enabled: boolean}}>(
			harness,
			admin.token,
		)
			.patch('/admin/instance/config')
			.body({domain_migration: {}})
			.execute();
		expect(afterEmpty.domain_migration).toMatchObject({config_version: 1, enabled: true});

		const afterUndefined = await createBuilder<{domain_migration: {config_version: number; enabled: boolean}}>(
			harness,
			admin.token,
		)
			.patch('/admin/instance/config')
			.body({domain_migration: {enabled: undefined}})
			.execute();
		expect(afterUndefined.domain_migration).toMatchObject({config_version: 1, enabled: true});
	});

	it('bumps the plutonium page config version on every admin update without the client sending one', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);

		const afterFirst = await createBuilder<{plutonium_page: {config_version: number; enabled: boolean}}>(
			harness,
			admin.token,
		)
			.patch('/admin/instance/config')
			.body({plutonium_page: {enabled: true, included_user_ids: [admin.userId]}})
			.execute();
		expect(afterFirst.plutonium_page).toMatchObject({config_version: 1, enabled: true});

		const afterSecond = await createBuilder<{
			plutonium_page: {config_version: number; rollout_basis_points: number};
		}>(harness, admin.token)
			.patch('/admin/instance/config')
			.body({plutonium_page: {rollout_basis_points: 2500}})
			.execute();
		expect(afterSecond.plutonium_page).toMatchObject({config_version: 2, rollout_basis_points: 2500});

		const afterEmpty = await createBuilder<{plutonium_page: {config_version: number}}>(harness, admin.token)
			.patch('/admin/instance/config')
			.body({plutonium_page: {}})
			.execute();
		expect(afterEmpty.plutonium_page).toMatchObject({config_version: 2});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, admin.token).get(ENDPOINT).execute();
		expect(body.assignments.plutonium_page).toEqual({enabled: true});
	});

	it('serves the delivery cadence an admin set through the instance config', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);

		const updated = await createBuilder<{experiment_delivery: ExperimentDeliveryConfigResponse}>(harness, admin.token)
			.patch('/admin/instance/config')
			.body({experiment_delivery: {poll_interval_seconds: 3600}})
			.execute();
		expect(updated.experiment_delivery).toEqual({
			poll_interval_seconds: 3600,
			poll_jitter_percent: DEFAULT_EXPERIMENT_POLL_JITTER_PERCENT,
		});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, admin.token).get(ENDPOINT).execute();
		expect(body.poll_interval_seconds).toBe(3600);
		expect(body.poll_jitter_percent).toBe(DEFAULT_EXPERIMENT_POLL_JITTER_PERCENT);
	});
});
