// SPDX-License-Identifier: AGPL-3.0-or-later

import {type RuntimeConfigSnapshot, runtimeInstanceKey} from '@app/features/app/state/InstanceSnapshotStore';
import {usesUniqueUsernames} from '@app/features/app/utils/AccountIdentityFeatures';
import {normalizeInstanceDomain} from '@app/features/auth/flow/instance_selector/InstanceDirectoryStorage';
import {parseAccountStorageKey} from '@app/features/auth/state/AccountStorageKey';
import type {Account} from '@app/features/platform/state/AuthSession';
import {formatTagForStreamerMode, getDisplayName} from '@app/features/user/utils/NicknameUtils';
import {shouldShowDiscriminator} from '@app/features/user/utils/UserTagUtils';
import {msg} from '@lingui/core/macro';

export const ACCOUNT_DETAILS_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'Account details unavailable',
	comment: 'Shown when a saved account record has no cached user profile data.',
});

export type AccountDisplayLabels =
	| {
			readonly available: true;
			readonly displayLabel: string;
			readonly tagLabel: string;
			readonly discriminatorLabel: string | null;
	  }
	| {
			readonly available: false;
	  };

export function getAccountDisplayLabels(account: Account): AccountDisplayLabels {
	const userData = account.userData;
	if (userData == null) {
		return {available: false};
	}
	const displayLabel = getDisplayName(userData);
	const discriminator = userData.discriminator;
	const uniqueUsernames = account.instance != null && usesUniqueUsernames(account.instance.features);
	if (!shouldShowDiscriminator(userData, uniqueUsernames)) {
		return {available: true, displayLabel, tagLabel: displayLabel, discriminatorLabel: null};
	}
	return {
		available: true,
		displayLabel,
		tagLabel: formatTagForStreamerMode(`${displayLabel}#${discriminator}`),
		discriminatorLabel: formatTagForStreamerMode(`#${discriminator}`),
	};
}

export function resolveSnapshotInstanceKey(snapshot: RuntimeConfigSnapshot | undefined): string | null {
	if (snapshot == null) {
		return null;
	}
	return runtimeInstanceKey(snapshot);
}

export function resolveSnapshotInstanceDomain(snapshot: RuntimeConfigSnapshot | undefined): string | null {
	if (snapshot == null) {
		return null;
	}
	const webAppEndpoint = snapshot.webAppEndpoint;
	if (typeof webAppEndpoint !== 'string' || webAppEndpoint.length === 0) {
		return null;
	}
	return normalizeInstanceDomain(webAppEndpoint);
}

export function resolveAccountInstanceDomain(account: Account): string | null {
	return resolveSnapshotInstanceDomain(account.instance);
}

export function resolveAccountInstanceKey(account: Account): string | null {
	return (
		resolveSnapshotInstanceKey(account.instance) ?? parseAccountStorageKey(account.storageKey)?.instanceKey ?? null
	);
}

export function resolveAccountInstanceLabel(account: Account): string | null {
	const domain = resolveAccountInstanceDomain(account);
	if (domain != null) {
		return domain;
	}
	const instanceKey = resolveAccountInstanceKey(account);
	if (instanceKey == null) {
		return null;
	}
	return normalizeInstanceDomain(instanceKey);
}
