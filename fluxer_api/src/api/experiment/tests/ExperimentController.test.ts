// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {
	DEFAULT_DOMAIN_MIGRATION_CONFIG,
	INERT_DOMAIN_MIGRATION_ASSIGNMENT,
} from '@fluxer/schema/src/domains/admin/DomainMigrationSchemas';
import {
	DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG,
	INERT_VOICE_NOISE_SUPPRESSION_ASSIGNMENT,
} from '@fluxer/schema/src/domains/admin/VoiceNoiseSuppressionSchemas';
import {
	DEFAULT_EXPERIMENT_POLL_INTERVAL_SECONDS,
	DEFAULT_EXPERIMENT_POLL_JITTER_PERCENT,
	type ExperimentAssignmentsResponse,
	type ExperimentDeliveryConfigResponse,
	readDomainMigrationAssignment,
	readVoiceNoiseSuppressionAssignment,
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
				voice_noise_suppression: INERT_VOICE_NOISE_SUPPRESSION_ASSIGNMENT,
				domain_migration: INERT_DOMAIN_MIGRATION_ASSIGNMENT,
			},
		});
	});

	it('returns the inert assignment while the stored config is disabled but populated', async () => {
		const account = await createTestAccount(harness);
		await getInstanceConfigRepository().setVoiceNoiseSuppressionConfig({
			...DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG,
			enabled: false,
			config_version: 9,
			rollout_basis_points: 10000,
			included_user_ids: [account.userId],
		});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(body.assignments.voice_noise_suppression).toEqual({
			...INERT_VOICE_NOISE_SUPPRESSION_ASSIGNMENT,
			config_version: 9,
		});
	});

	it('populates the voice assignment key even when the rollout is disabled', async () => {
		const account = await createTestAccount(harness);

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(Object.hasOwn(body.assignments, 'voice_noise_suppression')).toBe(true);
		expect(readVoiceNoiseSuppressionAssignment(body).enabled).toBe(false);
	});

	it('populates the domain migration assignment key even when the rollout is disabled', async () => {
		const account = await createTestAccount(harness);

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(Object.hasOwn(body.assignments, 'domain_migration')).toBe(true);
		expect(readDomainMigrationAssignment(body).enabled).toBe(false);
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

	it('serves the delivery cadence from the delivery config and not from the voice config', async () => {
		const account = await createTestAccount(harness);
		await getInstanceConfigRepository().setExperimentDeliveryConfig({
			poll_interval_seconds: 7200,
			poll_jitter_percent: 45,
		});
		await getInstanceConfigRepository().setVoiceNoiseSuppressionConfig({
			...DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG,
			enabled: true,
			config_version: 3,
			rollout_basis_points: 10000,
		});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token).get(ENDPOINT).execute();

		expect(body.poll_interval_seconds).toBe(7200);
		expect(body.poll_jitter_percent).toBe(45);
		expect(body.assignments.voice_noise_suppression).toMatchObject({enabled: true, config_version: 3});
		expect(body.assignments.voice_noise_suppression).not.toHaveProperty('poll_interval_seconds');
		expect(body.assignments.voice_noise_suppression).not.toHaveProperty('poll_jitter_percent');
	});

	it('echoes the config version and resolves the caller through the allowlist', async () => {
		const targeted = await createTestAccount(harness);
		const untargeted = await createTestAccount(harness);
		await getInstanceConfigRepository().setVoiceNoiseSuppressionConfig({
			...DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG,
			enabled: true,
			config_version: 14,
			default_backend: 'rnnoise',
			rollout_basis_points: 0,
			included_user_ids: [targeted.userId],
		});

		const targetedBody = await createBuilder<ExperimentAssignmentsResponse>(harness, targeted.token)
			.get(ENDPOINT)
			.execute();
		expect(targetedBody.assignments.voice_noise_suppression).toMatchObject({
			enabled: true,
			config_version: 14,
			user_targeted: true,
			backend: 'rnnoise',
			source: 'user_rule',
		});

		const untargetedBody = await createBuilder<ExperimentAssignmentsResponse>(harness, untargeted.token)
			.get(ENDPOINT)
			.execute();
		expect(untargetedBody.assignments.voice_noise_suppression).toMatchObject({
			enabled: true,
			config_version: 14,
			user_targeted: false,
			backend: null,
			source: null,
		});
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

	it('serves a fresh body once the voice config changes', async () => {
		const account = await createTestAccount(harness);

		const first = await createBuilder<ExperimentAssignmentsResponse>(harness, account.token)
			.get(ENDPOINT)
			.executeWithResponse();
		const staleEtag = first.response.headers.get('etag') as string;

		await getInstanceConfigRepository().setVoiceNoiseSuppressionConfig({
			...DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG,
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
		expect(refreshed.json?.assignments.voice_noise_suppression).toMatchObject({
			enabled: true,
			config_version: 1,
			user_targeted: true,
		});
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

	it('bumps the config version on every admin update without the client sending one', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);

		const afterFirst = await createBuilder<{voice_noise_suppression: {config_version: number; enabled: boolean}}>(
			harness,
			admin.token,
		)
			.patch('/admin/instance/config')
			.body({voice_noise_suppression: {enabled: true, rollout_basis_points: 10000}})
			.execute();
		expect(afterFirst.voice_noise_suppression).toMatchObject({config_version: 1, enabled: true});

		const afterSecond = await createBuilder<{voice_noise_suppression: {config_version: number; enabled: boolean}}>(
			harness,
			admin.token,
		)
			.patch('/admin/instance/config')
			.body({voice_noise_suppression: {suppression_strength: 42}})
			.execute();
		expect(afterSecond.voice_noise_suppression).toMatchObject({config_version: 2, enabled: true});

		const body = await createBuilder<ExperimentAssignmentsResponse>(harness, admin.token).get(ENDPOINT).execute();
		expect(body.assignments.voice_noise_suppression).toMatchObject({
			enabled: true,
			config_version: 2,
			suppression_strength: 42,
		});
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

	it('leaves the config version alone for an admin update that sets no field', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);

		const afterFirst = await createBuilder<{voice_noise_suppression: {config_version: number; enabled: boolean}}>(
			harness,
			admin.token,
		)
			.patch('/admin/instance/config')
			.body({voice_noise_suppression: {enabled: true}})
			.execute();
		expect(afterFirst.voice_noise_suppression).toMatchObject({config_version: 1, enabled: true});

		const afterEmpty = await createBuilder<{voice_noise_suppression: {config_version: number; enabled: boolean}}>(
			harness,
			admin.token,
		)
			.patch('/admin/instance/config')
			.body({voice_noise_suppression: {}})
			.execute();
		expect(afterEmpty.voice_noise_suppression).toMatchObject({config_version: 1, enabled: true});

		const afterUndefined = await createBuilder<{voice_noise_suppression: {config_version: number; enabled: boolean}}>(
			harness,
			admin.token,
		)
			.patch('/admin/instance/config')
			.body({voice_noise_suppression: {enabled: undefined}})
			.execute();
		expect(afterUndefined.voice_noise_suppression).toMatchObject({config_version: 1, enabled: true});
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
