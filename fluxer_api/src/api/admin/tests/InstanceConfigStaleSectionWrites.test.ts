// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {setCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import {InstanceConfigWriteRaceExecutor} from '@app/api/instance/tests/InstanceConfigWriteRaceExecutor';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {InstanceConfigResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const INSTANCE_POLICY_CONFIG_KEY = 'instance_policy_config';

describe('instance config admin PATCH against state another node changed', () => {
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

	const patchConfig = (admin: TestAccount, body: Record<string, unknown>) =>
		createBuilder<InstanceConfigResponse>(harness, admin.token).patch('/admin/instance/config').body(body);

	it('keeps an SSO field another node changed when a patch changes a different one', async () => {
		const admin = await createAdmin();
		await patchConfig(admin, {sso: {display_name: 'Before', client_id: 'client-before'}}).execute();
		await executor.writeDirectly('sso_display_name', 'Changed on another node');

		await patchConfig(admin, {sso: {client_id: 'client-after'}}).execute();

		expect(await executor.readDirectly('sso_display_name')).toBe('Changed on another node');
		expect(await executor.readDirectly('sso_client_id')).toBe('client-after');
	});

	it('refuses to disable direct messages when their lock lands between the read and the write', async () => {
		const admin = await createAdmin();
		await patchConfig(admin, {policy: {services: {gif_enabled: true}}}).execute();
		executor.watch(INSTANCE_POLICY_CONFIG_KEY);
		let competed = false;
		executor.competeBeforeEachWrite(async () => {
			if (competed) return;
			competed = true;
			await executor.writeDirectly(
				INSTANCE_POLICY_CONFIG_KEY,
				JSON.stringify({direct_messages_disabled: false, direct_messages_locked: true, gif_enabled: true}),
			);
		});

		await patchConfig(admin, {policy: {direct_messages_disabled: true}})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INSTANCE_POLICY_TRANSITION_NOT_ALLOWED)
			.execute();

		const stored = JSON.parse((await executor.readDirectly(INSTANCE_POLICY_CONFIG_KEY)) ?? 'null');
		expect(stored).toMatchObject({direct_messages_disabled: false, direct_messages_locked: true, gif_enabled: true});
	});

	it('applies the DM rule and a premium mode change from one request', async () => {
		const admin = await createAdmin();
		await patchConfig(admin, {policy: {direct_messages_disabled: true}}).execute();

		const updated = await patchConfig(admin, {
			policy: {direct_messages_disabled: false, premium_mode: 'mirror'},
		}).execute();

		expect(updated.policy).toMatchObject({
			direct_messages_disabled: false,
			direct_messages_locked: true,
			premium_mode: 'mirror',
		});
	});
});
