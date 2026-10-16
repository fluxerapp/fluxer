// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LimitConfigSnapshot, LimitConfigWireFormat} from '@fluxer/limits/src/LimitTypes';
import type {DomainMigrationDiscoveryResponse} from '@fluxer/schema/src/domains/admin/DomainMigrationSchemas';
import type {
	GeoEntry,
	GeolocationResponse as GeolocationWireResponse,
} from '@fluxer/schema/src/domains/geolocation/GeolocationSchemas';
import type {
	InstanceAgePolicyAction,
	InstanceAppPublic,
	InstancePushDeliveryMode,
	InstanceRegistration,
	WellKnownFluxerResponse,
} from '@fluxer/schema/src/domains/instance/InstanceSchemas';

export type {GeoEntry} from '@fluxer/schema/src/domains/geolocation/GeolocationSchemas';
export type {
	InstanceAgePolicy,
	InstanceAgePolicyGeo,
	InstanceAppPublic,
	InstanceBranding,
	InstanceCaptcha,
	InstanceCommunity,
	InstanceEndpoints,
	InstanceFeatures,
	InstanceGif,
	InstancePush,
	InstanceRegistration,
	InstanceServices,
	InstanceSetup,
	InstanceSso,
} from '@fluxer/schema/src/domains/instance/InstanceSchemas';

export type InstanceLegal = InstanceAppPublic['legal'];
export type InstanceDomainMigration = DomainMigrationDiscoveryResponse;
export type InstanceAppRegistration = InstanceAppPublic['registration'];

export const CURRENT_INSTANCE_CODENAME = 'tungsten';

export const REGISTRATION_MODES = Object.freeze(['open', 'approval', 'closed'] as const) satisfies ReadonlyArray<
	InstanceRegistration['mode']
>;

export const PUSH_DELIVERY_MODES = Object.freeze([
	'direct',
	'external_relay',
	'hybrid',
] as const) satisfies ReadonlyArray<InstancePushDeliveryMode>;

export const AGE_POLICY_ACTIONS = Object.freeze([
	'restrict',
	'block',
] as const) satisfies ReadonlyArray<InstanceAgePolicyAction>;

export interface InstanceDiscoveryResponse extends Omit<WellKnownFluxerResponse, 'limits'> {
	limits: LimitConfigSnapshot | LimitConfigWireFormat;
}

export interface GeolocationResponse extends Omit<GeolocationWireResponse, 'ageRestrictedGeos' | 'ageBlockedGeos'> {
	ageRestrictedGeos: ReadonlyArray<GeoEntry>;
	ageBlockedGeos: ReadonlyArray<GeoEntry>;
}
