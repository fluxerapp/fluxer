// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	LegacyPushServiceDeliveryWire,
	PushRelayConfigSchema,
	PushRelayConfigUpdateRequest,
	toLegacyPushServiceDeliveryWire,
} from '@fluxer/schema/src/domains/admin/PushRelaySchemas';
import {describe, expect, test} from 'vitest';

const ADMIN_USER_ID = '1500000000000000001';
const ACCEPTED_AT = '2026-09-27T10:11:12.000Z';

describe('push relay consent', () => {
	test('reads back as unaccepted by default', () => {
		expect(PushRelayConfigSchema.parse({})).toEqual({
			relay_consent_accepted: false,
			relay_consent_accepted_at: null,
			relay_consent_accepted_by: null,
		});
	});

	test('a stored push service delivery row keeps its consent', () => {
		expect(
			PushRelayConfigSchema.parse({
				enabled: true,
				config_version: 12,
				rollout_basis_points: 10000,
				rollout_salt: 'push-service-delivery-v1',
				included_user_ids: [],
				excluded_user_ids: ['1500000000000000003'],
				relay_consent_accepted: true,
				relay_consent_accepted_at: ACCEPTED_AT,
				relay_consent_accepted_by: ADMIN_USER_ID,
			}),
		).toEqual({
			relay_consent_accepted: true,
			relay_consent_accepted_at: ACCEPTED_AT,
			relay_consent_accepted_by: ADMIN_USER_ID,
		});
	});

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

	test('an accepted consent round-trips', () => {
		const accepted = {
			relay_consent_accepted: true,
			relay_consent_accepted_at: ACCEPTED_AT,
			relay_consent_accepted_by: ADMIN_USER_ID,
		};
		expect(PushRelayConfigSchema.parse(accepted)).toEqual(accepted);
	});

	test('the update request takes the consent flag on its own', () => {
		expect(PushRelayConfigUpdateRequest.parse({relay_consent_accepted: true})).toEqual({
			relay_consent_accepted: true,
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

	test.each([
		{relay_consent_accepted_at: 'yesterday'},
		{relay_consent_accepted_at: '2026-09-27'},
		{relay_consent_accepted_by: 'not-an-id'},
		{relay_consent_accepted: 'yes'},
	])('rejects a malformed stored consent: %j', (value) => {
		expect(PushRelayConfigSchema.safeParse(value).success).toBe(false);
	});
});

describe('legacy push service delivery wire', () => {
	test('pins every rollout field to full enrolment and keeps the consent and version', () => {
		const wire = toLegacyPushServiceDeliveryWire(
			{relay_consent_accepted: true, relay_consent_accepted_at: ACCEPTED_AT, relay_consent_accepted_by: ADMIN_USER_ID},
			7,
		);
		expect(wire).toEqual({
			enabled: true,
			config_version: 7,
			rollout_basis_points: 10000,
			rollout_salt: 'push-service-delivery-v1',
			included_user_ids: [],
			excluded_user_ids: [],
			relay_consent_accepted: true,
			relay_consent_accepted_at: ACCEPTED_AT,
			relay_consent_accepted_by: ADMIN_USER_ID,
		});
		expect(LegacyPushServiceDeliveryWire.parse(wire)).toEqual(wire);
	});

	test('rejects a document that would take anyone off the push service', () => {
		const wire = toLegacyPushServiceDeliveryWire(PushRelayConfigSchema.parse({}), 0);
		expect(LegacyPushServiceDeliveryWire.safeParse({...wire, enabled: false}).success).toBe(false);
		expect(LegacyPushServiceDeliveryWire.safeParse({...wire, rollout_basis_points: 5000}).success).toBe(false);
		expect(LegacyPushServiceDeliveryWire.safeParse({...wire, excluded_user_ids: ['1500000000000000003']}).success).toBe(
			false,
		);
	});
});
