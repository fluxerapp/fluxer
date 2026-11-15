// SPDX-License-Identifier: AGPL-3.0-or-later

import type {InstanceCommunity, InstanceFeatures} from '@fluxer/schema/src/domains/instance/InstanceSchemas';

type DiscoveryFeatures = Pick<InstanceFeatures, 'discovery_enabled'>;

interface DiscoveryRuntime {
	readonly features: DiscoveryFeatures;
	readonly community: Pick<InstanceCommunity, 'single_community'>;
}

export function discoveryEnabledOf(features: DiscoveryFeatures): boolean {
	return features.discovery_enabled !== false;
}

export function isDiscoveryAvailable(runtime: DiscoveryRuntime): boolean {
	return !runtime.community.single_community && discoveryEnabledOf(runtime.features);
}
