// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {setCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
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
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

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

	it('reads back as unaccepted before an operator agrees', async () => {
		const admin = await createAdmin();

		const config = await readConfig(admin).execute();

		expect(config.push_relay).toEqual({
			relay_consent_accepted: false,
			relay_consent_accepted_at: null,
			relay_consent_accepted_by: null,
		});
	});

	it('reads a stored row without consent fields as unaccepted', async () => {
		const admin = await createAdmin();
		await storeRow({enabled: true, config_version: 3, rollout_basis_points: 10000});

		const config = await readConfig(admin).execute();

		expect(config.push_relay.relay_consent_accepted).toBe(false);
		expect(await readRpcConfig()).toMatchObject({config_version: 3, relay_consent_accepted: false});
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
});
