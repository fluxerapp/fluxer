// SPDX-License-Identifier: AGPL-3.0-or-later

import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {Headers} from '@fluxer/constants/src/Headers';

export const CLIENT_FEATURES_HEADER = Headers.X_FLUXER_FEATURES;
const PERMISSION_BIT_CLIENT_FEATURES: ReadonlyArray<{
	bit: bigint;
	feature: string;
}> = [{bit: Permissions.VIEW_CHANNEL_MEMBERS, feature: 'view_channel_members_permission'}];

export function applyProtectedRolePermissions(
	requested: bigint,
	existing: bigint,
	clientFeatures: ReadonlySet<string>,
): bigint {
	let result = requested;
	for (const {bit, feature} of PERMISSION_BIT_CLIENT_FEATURES) {
		if (clientFeatures.has(feature)) continue;
		result = (result & ~bit) | (existing & bit);
	}
	return result;
}

export function applyProtectedOverwriteBits(
	requested: {
		allow: bigint;
		deny: bigint;
	},
	existing: {
		allow: bigint;
		deny: bigint;
	},
	clientFeatures: ReadonlySet<string>,
): {
	allow: bigint;
	deny: bigint;
} {
	let allow = requested.allow;
	let deny = requested.deny;
	for (const {bit, feature} of PERMISSION_BIT_CLIENT_FEATURES) {
		if (clientFeatures.has(feature)) continue;
		allow = (allow & ~bit) | (existing.allow & bit);
		deny = (deny & ~bit) | (existing.deny & bit);
	}
	return {allow, deny};
}

const MAX_CLIENT_FEATURES = 64;
const MAX_CLIENT_FEATURE_LENGTH = 64;
const VALID_FEATURE_NAME = /^[a-z0-9_]+$/;

export function parseClientFeaturesHeader(headerValue: string | null | undefined): ReadonlySet<string> {
	if (!headerValue) {
		return new Set();
	}
	const features = new Set<string>();
	for (const raw of headerValue.split(',')) {
		const trimmed = raw.trim().toLowerCase();
		if (trimmed.length === 0 || trimmed.length > MAX_CLIENT_FEATURE_LENGTH || !VALID_FEATURE_NAME.test(trimmed)) {
			continue;
		}
		features.add(trimmed);
		if (features.size >= MAX_CLIENT_FEATURES) {
			break;
		}
	}
	return features;
}
