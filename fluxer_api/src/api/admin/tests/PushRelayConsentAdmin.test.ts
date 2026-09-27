// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {setCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import {PushRelayConfigPublisher} from '@app/api/instance/PushRelayConfigPublisher';
import {InstanceConfigWriteRaceExecutor} from '@app/api/instance/tests/InstanceConfigWriteRaceExecutor';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import type {InstanceConfigResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {LegacyPushServiceDeliveryWire} from '@fluxer/schema/src/domains/admin/PushRelaySchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

const PUSH_RELAY_CONFIG_KEY = 'push_service_delivery_config';
const ACCEPTED_AT = '2026-09-20T08:00:00.000Z';
const ACCEPTED_BY = '1500000000000000007';

const PROD_ROW = {
	enabled: true,
	config_version: 41,
	rollout_basis_points: 10000,
	rollout_salt: 'push-service-delivery-v1',
	included_user_ids: [],
	excluded_user_ids: [],
	relay_consent_accepted: true,
	relay_consent_accepted_at: ACCEPTED_AT,
	relay_consent_accepted_by: ACCEPTED_BY,
};

interface PushServiceDeliveryRpcResponse {
	type: 'get_push_service_delivery_config';
	data: {config: LegacyPushServiceDeliveryWire};
}

describe('push relay supplemental notice consent', () => {
	let harness: ApiTestHarness;
	let executor: InstanceConfigWriteRaceExecutor;

	beforeAll(async () => {
		harness = await createApiTestHarness();
		executor = new InstanceConfigWriteRaceExecutor(new InMemoryCassandraQueryExecutor());
		setCassandraQueryExecutorForTesting(executor);
	});

	beforeEach(async () => {
		await harness.reset();
		vi.spyOn(PushRelayConfigPublisher.prototype, 'publish').mockResolvedValue(undefined);
	});

	afterEach(() => {
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	const createAdmin = async (): Promise<TestAccount> =>
		await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);

	const patchConfig = (admin: TestAccount, body: Record<string, unknown>) =>
		createBuilder<InstanceConfigResponse>(harness, admin.token).patch('/admin/instance/config').body(body);

	const readConfig = (admin: TestAccount) =>
		createBuilder<InstanceConfigResponse>(harness, admin.token).get('/admin/instance/config');

	const readRpcConfig = async (): Promise<LegacyPushServiceDeliveryWire> => {
		const response = await createBuilder<PushServiceDeliveryRpcResponse>(harness, '')
			.post('/test/rpc-session-init')
			.body({type: 'get_push_service_delivery_config'})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(response.type).toBe('get_push_service_delivery_config');
		return response.data.config;
	};

	async function storeRow(row: Record<string, unknown>): Promise<void> {
		await executor.writeDirectly(PUSH_RELAY_CONFIG_KEY, JSON.stringify(row));
		getInstanceConfigRepository().clearCacheForTesting();
	}

	async function readStoredRow(): Promise<unknown> {
		const raw = await executor.readDirectly(PUSH_RELAY_CONFIG_KEY);
		if (raw === null) throw new Error('push relay config was never stored');
		return JSON.parse(raw);
	}

	it('reads back as unaccepted before an operator agrees', async () => {
		const admin = await createAdmin();

		const config = await readConfig(admin).execute();

		expect(config.push_relay).toEqual({
			relay_consent_accepted: false,
			relay_consent_accepted_at: null,
			relay_consent_accepted_by: null,
		});
	});

	it('keeps the consent of a stored push service delivery row', async () => {
		const admin = await createAdmin();
		await storeRow(PROD_ROW);

		const config = await readConfig(admin).execute();

		expect(config.push_relay).toEqual({
			relay_consent_accepted: true,
			relay_consent_accepted_at: ACCEPTED_AT,
			relay_consent_accepted_by: ACCEPTED_BY,
		});
	});

	it('reads a stored row without consent fields as unaccepted', async () => {
		const admin = await createAdmin();
		await storeRow({enabled: true, config_version: 3, rollout_basis_points: 10000});

		const config = await readConfig(admin).execute();

		expect(config.push_relay.relay_consent_accepted).toBe(false);
		expect(await readRpcConfig()).toMatchObject({config_version: 3, relay_consent_accepted: false});
	});

	it('stamps the acting admin and the acceptance time when consent is given', async () => {
		const admin = await createAdmin();

		const updated = await patchConfig(admin, {push_relay: {relay_consent_accepted: true}}).execute();

		expect(updated.push_relay.relay_consent_accepted).toBe(true);
		expect(updated.push_relay.relay_consent_accepted_by).toBe(admin.userId);
		expect(Date.parse(updated.push_relay.relay_consent_accepted_at ?? '')).not.toBeNaN();
	});

	it('keeps the stamp untouched when consent is re-sent unchanged', async () => {
		const admin = await createAdmin();
		const accepted = await patchConfig(admin, {push_relay: {relay_consent_accepted: true}}).execute();

		const resent = await patchConfig(admin, {push_relay: {relay_consent_accepted: true}}).execute();

		expect(resent.push_relay).toEqual(accepted.push_relay);
	});

	it('clears the stamp when an operator withdraws consent', async () => {
		const admin = await createAdmin();
		await patchConfig(admin, {push_relay: {relay_consent_accepted: true}}).execute();

		const withdrawn = await patchConfig(admin, {push_relay: {relay_consent_accepted: false}}).execute();

		expect(withdrawn.push_relay).toEqual({
			relay_consent_accepted: false,
			relay_consent_accepted_at: null,
			relay_consent_accepted_by: null,
		});
	});

	it('ignores an acceptance stamp supplied by the caller', async () => {
		const admin = await createAdmin();

		const updated = await patchConfig(admin, {
			push_relay: {
				relay_consent_accepted: true,
				relay_consent_accepted_at: '2020-01-01T00:00:00.000Z',
				relay_consent_accepted_by: '1500000000000000009',
			},
		}).execute();

		expect(updated.push_relay.relay_consent_accepted_at).not.toBe('2020-01-01T00:00:00.000Z');
		expect(updated.push_relay.relay_consent_accepted_by).toBe(admin.userId);
	});

	it('writes the full legacy document and bumps the stored config version', async () => {
		const admin = await createAdmin();
		await storeRow({
			...PROD_ROW,
			relay_consent_accepted: false,
			relay_consent_accepted_at: null,
			relay_consent_accepted_by: null,
		});

		const updated = await patchConfig(admin, {push_relay: {relay_consent_accepted: true}}).execute();

		expect(await readStoredRow()).toEqual({
			enabled: true,
			config_version: 42,
			rollout_basis_points: 10000,
			rollout_salt: 'push-service-delivery-v1',
			included_user_ids: [],
			excluded_user_ids: [],
			relay_consent_accepted: true,
			relay_consent_accepted_at: updated.push_relay.relay_consent_accepted_at,
			relay_consent_accepted_by: admin.userId,
		});

		await patchConfig(admin, {push_relay: {relay_consent_accepted: false}}).execute();

		expect(await readStoredRow()).toMatchObject({
			enabled: true,
			config_version: 43,
			rollout_basis_points: 10000,
			relay_consent_accepted: false,
			relay_consent_accepted_at: null,
			relay_consent_accepted_by: null,
		});
	});

	it('rewrites a partially enrolled stored row as full enrolment', async () => {
		const admin = await createAdmin();
		await storeRow({
			...PROD_ROW,
			enabled: false,
			rollout_basis_points: 250,
			rollout_salt: 'custom-salt',
			included_user_ids: ['1500000000000000003'],
			excluded_user_ids: ['1500000000000000004'],
		});

		await patchConfig(admin, {push_relay: {relay_consent_accepted: false}}).execute();

		expect(await readStoredRow()).toMatchObject({
			enabled: true,
			config_version: 42,
			rollout_basis_points: 10000,
			rollout_salt: 'push-service-delivery-v1',
			included_user_ids: [],
			excluded_user_ids: [],
		});
	});

	it('publishes the legacy delivery document with the consent', async () => {
		const admin = await createAdmin();
		const publish = vi.mocked(PushRelayConfigPublisher.prototype.publish);

		const updated = await patchConfig(admin, {push_relay: {relay_consent_accepted: true}}).execute();

		expect(publish).toHaveBeenCalledTimes(1);
		expect(publish).toHaveBeenCalledWith({
			enabled: true,
			config_version: 1,
			rollout_basis_points: 10000,
			rollout_salt: 'push-service-delivery-v1',
			included_user_ids: [],
			excluded_user_ids: [],
			relay_consent_accepted: true,
			relay_consent_accepted_at: updated.push_relay.relay_consent_accepted_at,
			relay_consent_accepted_by: admin.userId,
		});
	});

	it('does not write or publish for an empty push relay patch', async () => {
		const admin = await createAdmin();
		const publish = vi.mocked(PushRelayConfigPublisher.prototype.publish);

		await patchConfig(admin, {push_relay: {}}).execute();

		expect(publish).not.toHaveBeenCalled();
		expect(await executor.readDirectly(PUSH_RELAY_CONFIG_KEY)).toBeNull();
	});

	it('ignores the retired push_service_delivery section', async () => {
		const admin = await createAdmin();
		const publish = vi.mocked(PushRelayConfigPublisher.prototype.publish);

		const updated = await patchConfig(admin, {push_service_delivery: {relay_consent_accepted: true}}).execute();

		expect(updated.push_relay.relay_consent_accepted).toBe(false);
		expect(publish).not.toHaveBeenCalled();
	});

	it('answers the legacy delivery RPC with full enrolment and the stored consent', async () => {
		await storeRow(PROD_ROW);

		expect(await readRpcConfig()).toEqual(PROD_ROW);
	});

	it('answers the legacy delivery RPC with defaults before anything is stored', async () => {
		expect(await readRpcConfig()).toEqual({
			enabled: true,
			config_version: 0,
			rollout_basis_points: 10000,
			rollout_salt: 'push-service-delivery-v1',
			included_user_ids: [],
			excluded_user_ids: [],
			relay_consent_accepted: false,
			relay_consent_accepted_at: null,
			relay_consent_accepted_by: null,
		});
	});

	it('answers the legacy delivery RPC with consent given through the admin API', async () => {
		const admin = await createAdmin();
		const updated = await patchConfig(admin, {push_relay: {relay_consent_accepted: true}}).execute();

		expect(await readRpcConfig()).toMatchObject({
			enabled: true,
			config_version: 1,
			rollout_basis_points: 10000,
			relay_consent_accepted: true,
			relay_consent_accepted_at: updated.push_relay.relay_consent_accepted_at,
			relay_consent_accepted_by: admin.userId,
		});
	});
});
