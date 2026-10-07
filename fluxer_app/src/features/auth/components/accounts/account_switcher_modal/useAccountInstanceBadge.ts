// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveAccountInstanceKey, resolveAccountInstanceLabel} from '@app/features/auth/AccountDisplayUtils';
import {
	type InstanceInfo,
	normalizeInstanceName,
	resolveInstanceLabel,
} from '@app/features/auth/flow/instance_selector/InstanceDirectoryStorage';
import type {Account} from '@app/features/platform/state/AuthSession';
import {isOfficialInstanceHost, OFFICIAL_INSTANCE_NAME} from '@fluxer/instance_bootstrap/src/OfficialInstance';
import {useMemo} from 'react';

export interface AccountInstanceBadgeInfo {
	readonly label: string;
	readonly title: string;
	readonly isOfficial: boolean;
}

function findKnownInstance(instanceKey: string, knownInstances: ReadonlyArray<InstanceInfo>): InstanceInfo | null {
	for (const instance of knownInstances) {
		if (instance.instanceKey === instanceKey) {
			return instance;
		}
	}
	return null;
}

function resolveInstanceName(account: Account, knownInstance: InstanceInfo | null, domain: string): string {
	const knownName = normalizeInstanceName(knownInstance?.name);
	if (knownName != null && knownName !== knownInstance?.domain) {
		return resolveInstanceLabel(knownName, domain);
	}
	return resolveInstanceLabel(account.instance?.appPublic?.branding?.product_name, domain);
}

function getAccountInstanceBadgeInfo(
	account: Account,
	knownInstances: ReadonlyArray<InstanceInfo>,
): AccountInstanceBadgeInfo | null {
	const instanceKey = resolveAccountInstanceKey(account);
	const domain = resolveAccountInstanceLabel(account);
	if (instanceKey == null || domain == null) {
		return null;
	}
	const isOfficial = isOfficialInstanceHost(instanceKey);
	if (isOfficial) {
		return {label: OFFICIAL_INSTANCE_NAME, title: domain, isOfficial};
	}
	const knownInstance = findKnownInstance(instanceKey, knownInstances);
	return {label: resolveInstanceName(account, knownInstance, domain), title: domain, isOfficial};
}

export function useAccountInstanceBadge(
	account: Account,
	knownInstances: ReadonlyArray<InstanceInfo>,
	enabled: boolean,
): AccountInstanceBadgeInfo | null {
	return useMemo(() => {
		if (!enabled) {
			return null;
		}
		return getAccountInstanceBadgeInfo(account, knownInstances);
	}, [account, enabled, knownInstances]);
}
