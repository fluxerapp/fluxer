// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	LegacyPushServiceDeliveryWire,
	PushRelayConfigSchema,
	PushRelayConfigUpdateRequest,
	toLegacyPushServiceDeliveryWire,
} from '@fluxer/schema/src/domains/admin/PushRelaySchemas';
import {describe, expect, test} from 'vitest';

const ADMIN_USER_ID = '1500000000000000001';

describe('push relay consent', () => {
	test('a stored row that predates relay consent reads back as not accepted', () => {
		expect(
			PushRelayConfigSchema.parse({
				enabled: true,
				config_version: 4,
				rollout_basis_points: 10000,
				rollout_salt: 'push-service-delivery-v1',
				included_user_ids: [],
				excluded_user_ids: [],
			}),
		).toEqual({
			relay_consent_accepted: false,
			relay_consent_accepted_at: null,
			relay_consent_accepted_by: null,
		});
	});

	test('the update request drops a client-supplied acceptance stamp', () => {
		expect(
			PushRelayConfigUpdateRequest.parse({
				relay_consent_accepted: true,
				relay_consent_accepted_at: '2020-01-01T00:00:00.000Z',
				relay_consent_accepted_by: ADMIN_USER_ID,
			}),
		).toEqual({relay_consent_accepted: true});
	});
});

describe('legacy push service delivery wire', () => {
	test('rejects a document that would take anyone off the push service', () => {
		const wire = toLegacyPushServiceDeliveryWire(PushRelayConfigSchema.parse({}), 0);
		expect(LegacyPushServiceDeliveryWire.safeParse({...wire, enabled: false}).success).toBe(false);
		expect(LegacyPushServiceDeliveryWire.safeParse({...wire, rollout_basis_points: 5000}).success).toBe(false);
		expect(LegacyPushServiceDeliveryWire.safeParse({...wire, excluded_user_ids: ['1500000000000000003']}).success).toBe(
			false,
		);
	});
});
