// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {PushServiceDeliveryConfigPublisher} from '@app/api/instance/PushServiceDeliveryConfigPublisher';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import type {InstanceConfigResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

describe('push relay supplemental notice consent', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		vi.spyOn(PushServiceDeliveryConfigPublisher.prototype, 'publish').mockResolvedValue(undefined);
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

	it('reads back as unaccepted before an operator agrees', async () => {
		const admin = await createAdmin();

		const config = await readConfig(admin).execute();

		expect(config.push_service_delivery).toMatchObject({
			relay_consent_accepted: false,
			relay_consent_accepted_at: null,
			relay_consent_accepted_by: null,
		});
	});

	it('stamps the acting admin and the acceptance time when consent is given', async () => {
		const admin = await createAdmin();

		const updated = await patchConfig(admin, {push_service_delivery: {relay_consent_accepted: true}}).execute();

		expect(updated.push_service_delivery.relay_consent_accepted).toBe(true);
		expect(updated.push_service_delivery.relay_consent_accepted_by).toBe(admin.userId);
		expect(Date.parse(updated.push_service_delivery.relay_consent_accepted_at ?? '')).not.toBeNaN();
	});

	it('keeps the first acceptance stamp when a later patch changes only the rollout', async () => {
		const admin = await createAdmin();
		const accepted = await patchConfig(admin, {push_service_delivery: {relay_consent_accepted: true}}).execute();

		const rolledOut = await patchConfig(admin, {
			push_service_delivery: {enabled: true, rollout_basis_points: 2500},
		}).execute();

		expect(rolledOut.push_service_delivery).toMatchObject({
			enabled: true,
			rollout_basis_points: 2500,
			relay_consent_accepted: true,
			relay_consent_accepted_at: accepted.push_service_delivery.relay_consent_accepted_at,
			relay_consent_accepted_by: admin.userId,
		});
	});

	it('keeps the stamp untouched when consent is re-sent unchanged', async () => {
		const admin = await createAdmin();
		const accepted = await patchConfig(admin, {push_service_delivery: {relay_consent_accepted: true}}).execute();

		const resent = await patchConfig(admin, {push_service_delivery: {relay_consent_accepted: true}}).execute();

		expect(resent.push_service_delivery.relay_consent_accepted_at).toBe(
			accepted.push_service_delivery.relay_consent_accepted_at,
		);
	});

	it('clears the stamp when an operator withdraws consent', async () => {
		const admin = await createAdmin();
		await patchConfig(admin, {push_service_delivery: {relay_consent_accepted: true}}).execute();

		const withdrawn = await patchConfig(admin, {push_service_delivery: {relay_consent_accepted: false}}).execute();

		expect(withdrawn.push_service_delivery).toMatchObject({
			relay_consent_accepted: false,
			relay_consent_accepted_at: null,
			relay_consent_accepted_by: null,
		});
	});

	it('ignores an acceptance stamp supplied by the caller', async () => {
		const admin = await createAdmin();

		const updated = await patchConfig(admin, {
			push_service_delivery: {
				relay_consent_accepted: true,
				relay_consent_accepted_at: '2020-01-01T00:00:00.000Z',
				relay_consent_accepted_by: '1500000000000000009',
			},
		}).execute();

		expect(updated.push_service_delivery.relay_consent_accepted_at).not.toBe('2020-01-01T00:00:00.000Z');
		expect(updated.push_service_delivery.relay_consent_accepted_by).toBe(admin.userId);
	});

	it('publishes the consent to the delivery services', async () => {
		const admin = await createAdmin();
		const publish = vi.mocked(PushServiceDeliveryConfigPublisher.prototype.publish);

		await patchConfig(admin, {push_service_delivery: {relay_consent_accepted: true}}).execute();

		expect(publish).toHaveBeenCalledWith(expect.objectContaining({relay_consent_accepted: true}));
	});
});
