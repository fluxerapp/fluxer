// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LimitConfigSnapshot, LimitConfigWireFormat} from '@fluxer/limits/src/LimitTypes';
import type {
	GeoEntry,
	GeolocationResponse as GeolocationWireResponse,
} from '@fluxer/schema/src/domains/geolocation/GeolocationSchemas';
import type {WellKnownFluxerResponse} from '@fluxer/schema/src/domains/instance/InstanceSchemas';

export type {GeoEntry} from '@fluxer/schema/src/domains/geolocation/GeolocationSchemas';
export type {
	InstanceAppPublic,
	InstanceCaptcha,
	InstanceCommunity,
	InstanceEndpoints,
	InstanceFeatures,
	InstanceGif,
	InstancePush,
	InstanceRegistration,
	InstanceServices,
	InstanceSso,
} from '@fluxer/schema/src/domains/instance/InstanceSchemas';

export interface InstanceDiscoveryResponse extends Omit<WellKnownFluxerResponse, 'limits'> {
	limits: LimitConfigSnapshot | LimitConfigWireFormat;
}

export interface GeolocationResponse extends Omit<GeolocationWireResponse, 'ageRestrictedGeos' | 'ageBlockedGeos'> {
	ageRestrictedGeos: ReadonlyArray<GeoEntry>;
	ageBlockedGeos: ReadonlyArray<GeoEntry>;
}
