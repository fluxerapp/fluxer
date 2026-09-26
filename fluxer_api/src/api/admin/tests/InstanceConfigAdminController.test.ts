// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditLog} from '@app/api/admin/IAdminRepository';
import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {setCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import {PushServiceDeliveryConfigPublisher} from '@app/api/instance/PushServiceDeliveryConfigPublisher';
import {InstanceConfigWriteRaceExecutor} from '@app/api/instance/tests/InstanceConfigWriteRaceExecutor';
import {getAdminRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {InstanceConfigResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {
	DEFAULT_PUSH_SERVICE_DELIVERY_CONFIG,
	type PushServiceDeliveryConfig,
} from '@fluxer/schema/src/domains/admin/PushServiceDeliverySchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

const PUSH_SERVICE_DELIVERY_CONFIG_KEY = 'push_service_delivery_config';

describe('instance config admin PATCH under concurrent writes', () => {
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

	const spyOnPushDeliveryPublishes = () =>
		vi.spyOn(PushServiceDeliveryConfigPublisher.prototype, 'publish').mockResolvedValue(undefined);

	async function readStoredPushServiceDelivery(): Promise<PushServiceDeliveryConfig> {
		const raw = await executor.readDirectly(PUSH_SERVICE_DELIVERY_CONFIG_KEY);
		if (raw === null) throw new Error('push service delivery config was never stored');
		return JSON.parse(raw) as PushServiceDeliveryConfig;
	}

	async function listConfigUpdateAudits(): Promise<Array<AdminAuditLog>> {
		const logs = await getAdminRepository().listAllAuditLogsPaginated(100000);
		return logs.filter((log) => log.action === 'update_instance_config');
	}

	it('merges a standalone forwarding patch into the stored domain migration config', async () => {
		const admin = await createAdmin();
		await patchConfig(admin, {domain_migration: {enabled: true, rollout_basis_points: 250}}).execute();

		const updated = await patchConfig(admin, {domain_migration: {standalone_forwarding: true}}).execute();

		expect(updated.domain_migration).toMatchObject({
			enabled: true,
			rollout_basis_points: 250,
			standalone_forwarding: true,
			config_version: 2,
		});
	});

	it('answers with a conflict and neither writes, publishes nor audits once every attempt has lost the race', async () => {
		const publish = spyOnPushDeliveryPublishes();
		const admin = await createAdmin();
		await patchConfig(admin, {push_service_delivery: {enabled: true, rollout_basis_points: 1000}}).execute();
		publish.mockClear();
		const auditsBefore = await listConfigUpdateAudits();
		executor.watch(PUSH_SERVICE_DELIVERY_CONFIG_KEY);
		let competingWrites = 0;
		executor.competeBeforeEachWrite(async () => {
			competingWrites++;
			await executor.writeDirectly(
				PUSH_SERVICE_DELIVERY_CONFIG_KEY,
				JSON.stringify({
					...DEFAULT_PUSH_SERVICE_DELIVERY_CONFIG,
					enabled: false,
					rollout_basis_points: 1000,
					config_version: 100 + competingWrites,
				}),
			);
		});

		await patchConfig(admin, {push_service_delivery: {rollout_basis_points: 5000}})
			.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.CONFLICT)
			.execute();

		expect(executor.events).not.toContain('write');
		expect(await readStoredPushServiceDelivery()).toEqual({
			...DEFAULT_PUSH_SERVICE_DELIVERY_CONFIG,
			enabled: false,
			rollout_basis_points: 1000,
			config_version: 100 + competingWrites,
		});
		expect(publish).not.toHaveBeenCalled();
		expect(await listConfigUpdateAudits()).toHaveLength(auditsBefore.length);
	});
});
