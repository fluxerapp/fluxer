// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditLog} from '@app/api/admin/IAdminRepository';
import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {setCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import {PushRelayConfigPublisher} from '@app/api/instance/PushRelayConfigPublisher';
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
	type LegacyPushServiceDeliveryWire,
	toLegacyPushServiceDeliveryWire,
} from '@fluxer/schema/src/domains/admin/PushRelaySchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

const PUSH_RELAY_CONFIG_KEY = 'push_service_delivery_config';

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

	const spyOnPushRelayPublishes = () =>
		vi.spyOn(PushRelayConfigPublisher.prototype, 'publish').mockResolvedValue(undefined);

	async function readStoredPushRelay(): Promise<LegacyPushServiceDeliveryWire> {
		const raw = await executor.readDirectly(PUSH_RELAY_CONFIG_KEY);
		if (raw === null) throw new Error('push relay config was never stored');
		return JSON.parse(raw) as LegacyPushServiceDeliveryWire;
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
		const publish = spyOnPushRelayPublishes();
		const admin = await createAdmin();
		await patchConfig(admin, {push_relay: {relay_consent_accepted: false}}).execute();
		publish.mockClear();
		const auditsBefore = await listConfigUpdateAudits();
		executor.watch(PUSH_RELAY_CONFIG_KEY);
		const unaccepted = {
			relay_consent_accepted: false,
			relay_consent_accepted_at: null,
			relay_consent_accepted_by: null,
		};
		let competingWrites = 0;
		executor.competeBeforeEachWrite(async () => {
			competingWrites++;
			await executor.writeDirectly(
				PUSH_RELAY_CONFIG_KEY,
				JSON.stringify(toLegacyPushServiceDeliveryWire(unaccepted, 100 + competingWrites)),
			);
		});

		await patchConfig(admin, {push_relay: {relay_consent_accepted: true}})
			.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.CONFLICT)
			.execute();

		expect(executor.events).not.toContain('write');
		expect(await readStoredPushRelay()).toEqual(toLegacyPushServiceDeliveryWire(unaccepted, 100 + competingWrites));
		expect(publish).not.toHaveBeenCalled();
		expect(await listConfigUpdateAudits()).toHaveLength(auditsBefore.length);
	});
});
