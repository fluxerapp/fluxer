// SPDX-License-Identifier: AGPL-3.0-or-later

import {ALL_PERMISSIONS, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {Headers} from '@fluxer/constants/src/Headers';
import {THREAD_AWARE_ALL_PERMISSIONS, THREAD_PERMISSIONS} from '@fluxer/constants/src/ThreadPermissionUtils';

export const CLIENT_FEATURES_HEADER = Headers.X_FLUXER_FEATURES;
export const CHANNEL_THREADS_CLIENT_FEATURE = 'channel_threads';

export type ThreadPermissionMode = 'control' | 'active' | 'retired';

export interface ProtectedBitActor {
	clientFeatures: ReadonlySet<string>;
	threadBits?: {mode: ThreadPermissionMode; writer: boolean};
}

function threadBitsWritable(actor: ProtectedBitActor): boolean {
	const threadBits = actor.threadBits;
	if (!threadBits || threadBits.mode === 'control') return true;
	return threadBits.mode === 'active' && threadBits.writer;
}

export function protectedThreadBits(actor: ProtectedBitActor): bigint {
	return threadBitsWritable(actor) ? 0n : THREAD_PERMISSIONS;
}

export function permissionWriteMask(actor: ProtectedBitActor): bigint {
	return !actor.threadBits || actor.threadBits.mode === 'control' ? ALL_PERMISSIONS : THREAD_AWARE_ALL_PERMISSIONS;
}

const PERMISSION_BIT_CLIENT_FEATURES: ReadonlyArray<{
	bit: bigint;
	writable: (actor: ProtectedBitActor) => boolean;
}> = [
	{
		bit: Permissions.VIEW_CHANNEL_MEMBERS,
		writable: (actor) => actor.clientFeatures.has('view_channel_members_permission'),
	},
	{bit: THREAD_PERMISSIONS, writable: threadBitsWritable},
];

export function applyProtectedRolePermissions(requested: bigint, existing: bigint, actor: ProtectedBitActor): bigint {
	let result = requested;
	for (const {bit, writable} of PERMISSION_BIT_CLIENT_FEATURES) {
		if (writable(actor)) continue;
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
	actor: ProtectedBitActor,
): {
	allow: bigint;
	deny: bigint;
} {
	let allow = requested.allow;
	let deny = requested.deny;
	for (const {bit, writable} of PERMISSION_BIT_CLIENT_FEATURES) {
		if (writable(actor)) continue;
		allow = (allow & ~bit) | (existing.allow & bit);
		deny = (deny & ~bit) | (existing.deny & bit);
	}
	return {allow, deny};
}

const MAX_CLIENT_FEATURES = 64;
const MAX_CLIENT_FEATURE_LENGTH = 64;
const VALID_FEATURE_NAME = /^[a-z0-9_]+$/;

const NO_CLIENT_FEATURES: ReadonlySet<string> = new Set();

export function parseClientFeaturesHeader(headerValue: string | null | undefined): ReadonlySet<string> {
	if (!headerValue) {
		return NO_CLIENT_FEATURES;
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
