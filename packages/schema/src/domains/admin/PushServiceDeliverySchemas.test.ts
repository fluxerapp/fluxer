// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DEFAULT_PUSH_SERVICE_DELIVERY_CONFIG,
	type PushServiceDeliveryConfig,
	PushServiceDeliveryConfigSchema,
	PushServiceDeliveryConfigUpdateRequest,
	pushServiceDeliveryEnrols,
} from '@fluxer/schema/src/domains/admin/PushServiceDeliverySchemas';
import {describe, expect, test} from 'vitest';

const ADMIN_USER_ID = '1500000000000000001';
const TARGETED_USER_ID = '1500000000000000002';

function createConfig(overrides: Partial<PushServiceDeliveryConfig> = {}): PushServiceDeliveryConfig {
	return {
		...DEFAULT_PUSH_SERVICE_DELIVERY_CONFIG,
		included_user_ids: [],
		excluded_user_ids: [],
		...overrides,
	};
}

describe('push service delivery relay consent', () => {
	test('a stored configuration that predates relay consent reads back as not accepted', () => {
		expect(
			PushServiceDeliveryConfigSchema.parse({
				enabled: true,
				config_version: 4,
				rollout_basis_points: 10000,
				rollout_salt: 'push-service-delivery-v1',
				included_user_ids: [],
				excluded_user_ids: [],
			}),
		).toMatchObject({
			relay_consent_accepted: false,
			relay_consent_accepted_at: null,
			relay_consent_accepted_by: null,
		});
	});

	test('the defaults export carries the unaccepted consent', () => {
		expect(DEFAULT_PUSH_SERVICE_DELIVERY_CONFIG.relay_consent_accepted).toBe(false);
		expect(DEFAULT_PUSH_SERVICE_DELIVERY_CONFIG.relay_consent_accepted_at).toBeNull();
		expect(DEFAULT_PUSH_SERVICE_DELIVERY_CONFIG.relay_consent_accepted_by).toBeNull();
	});

	test('an accepted consent round-trips through the stored schema', () => {
		const accepted = {
			...DEFAULT_PUSH_SERVICE_DELIVERY_CONFIG,
			relay_consent_accepted: true,
			relay_consent_accepted_at: '2026-09-27T10:11:12.000Z',
			relay_consent_accepted_by: ADMIN_USER_ID,
		};
		expect(PushServiceDeliveryConfigSchema.parse(accepted)).toEqual(accepted);
	});

	test('the update request takes the consent flag on its own', () => {
		expect(PushServiceDeliveryConfigUpdateRequest.parse({relay_consent_accepted: true})).toEqual({
			relay_consent_accepted: true,
		});
	});

	test('the update request refuses a client-supplied acceptance stamp', () => {
		expect(
			PushServiceDeliveryConfigUpdateRequest.parse({
				relay_consent_accepted: true,
				relay_consent_accepted_at: '2020-01-01T00:00:00.000Z',
				relay_consent_accepted_by: ADMIN_USER_ID,
			}),
		).toEqual({relay_consent_accepted: true});
	});

	test.each([
		{relay_consent_accepted_at: 'yesterday'},
		{relay_consent_accepted_at: '2026-09-27'},
		{relay_consent_accepted_by: 'not-an-id'},
		{relay_consent_accepted: 'yes'},
	])('rejects a malformed stored consent: %j', (value) => {
		expect(PushServiceDeliveryConfigSchema.safeParse(value).success).toBe(false);
	});

	test('consent alone enrols nobody and refusing it excludes nobody', () => {
		const withConsent = createConfig({enabled: false, relay_consent_accepted: true});
		const withoutConsent = createConfig({enabled: true, rollout_basis_points: 10000});
		expect(pushServiceDeliveryEnrols(withConsent, TARGETED_USER_ID)).toBe(false);
		expect(pushServiceDeliveryEnrols(withoutConsent, TARGETED_USER_ID)).toBe(true);
	});
});
