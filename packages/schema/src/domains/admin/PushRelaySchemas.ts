// SPDX-License-Identifier: AGPL-3.0-or-later

import {z} from 'zod';

const LEGACY_PUSH_SERVICE_DELIVERY_ROLLOUT_BASIS_POINTS = 10000;
const LEGACY_PUSH_SERVICE_DELIVERY_SALT = 'push-service-delivery-v1';

export const PushRelayConfigSchema = z.object({
	relay_consent_accepted: z.boolean().default(false),
	relay_consent_accepted_at: z.iso.datetime().nullable().default(null),
	relay_consent_accepted_by: z
		.string()
		.regex(/^\d{1,20}$/u)
		.nullable()
		.default(null),
});

export type PushRelayConfig = z.infer<typeof PushRelayConfigSchema>;

export const PushRelayConfigResponse = PushRelayConfigSchema;

export type PushRelayConfigResponse = z.infer<typeof PushRelayConfigResponse>;

export const PushRelayConfigUpdateRequest = z.object({
	relay_consent_accepted: z.boolean().optional(),
});

export type PushRelayConfigUpdateRequest = z.infer<typeof PushRelayConfigUpdateRequest>;

export const LegacyPushServiceDeliveryWire = PushRelayConfigSchema.extend({
	enabled: z.literal(true),
	config_version: z.number().int().min(0),
	rollout_basis_points: z.literal(LEGACY_PUSH_SERVICE_DELIVERY_ROLLOUT_BASIS_POINTS),
	rollout_salt: z.literal(LEGACY_PUSH_SERVICE_DELIVERY_SALT),
	included_user_ids: z.tuple([]),
	excluded_user_ids: z.tuple([]),
});

export type LegacyPushServiceDeliveryWire = z.infer<typeof LegacyPushServiceDeliveryWire>;

export function toLegacyPushServiceDeliveryWire(
	config: PushRelayConfig,
	configVersion: number,
): LegacyPushServiceDeliveryWire {
	return {
		enabled: true,
		config_version: configVersion,
		rollout_basis_points: LEGACY_PUSH_SERVICE_DELIVERY_ROLLOUT_BASIS_POINTS,
		rollout_salt: LEGACY_PUSH_SERVICE_DELIVERY_SALT,
		included_user_ids: [],
		excluded_user_ids: [],
		relay_consent_accepted: config.relay_consent_accepted,
		relay_consent_accepted_at: config.relay_consent_accepted_at,
		relay_consent_accepted_by: config.relay_consent_accepted_by,
	};
}
