// SPDX-License-Identifier: AGPL-3.0-or-later

import {getAccountKey} from '@app/features/auth/state/AccountStorageKey';
import type {Account} from '@app/features/platform/state/AuthSession';

interface BackgroundAccountSelectionRequest {
	readonly accounts: ReadonlyArray<Account>;
	readonly currentAccountKey: string | null;
	readonly maxConnections: number;
}

interface BackgroundAccountSelection {
	readonly desired: ReadonlyMap<string, Account>;
	readonly unresolved: ReadonlySet<string>;
}

export function gatewayEndpointForAccount(account: Account): string | null {
	const endpoint = account.instance?.gatewayEndpoint;
	if (typeof endpoint !== 'string' || endpoint.length === 0) {
		return null;
	}
	return endpoint;
}

export function backgroundSessionNeedsRestart(left: Account, right: Account): boolean {
	if (left.token !== right.token) {
		return true;
	}
	if (left.isValid !== right.isValid) {
		return true;
	}
	return gatewayEndpointForAccount(left) !== gatewayEndpointForAccount(right);
}

export function sumMentionCounts(counts: Iterable<number>): number {
	let total = 0;
	for (const count of counts) {
		total += count;
	}
	return total;
}

export function selectBackgroundAccounts({
	accounts,
	currentAccountKey,
	maxConnections,
}: BackgroundAccountSelectionRequest): BackgroundAccountSelection {
	const unresolved = new Set<string>();
	const eligible: Array<{accountKey: string; account: Account}> = [];
	for (const account of accounts) {
		const accountKey = getAccountKey(account);
		if (accountKey === currentAccountKey || account.token.length === 0 || !account.isValid) {
			continue;
		}
		if (gatewayEndpointForAccount(account) === null) {
			unresolved.add(accountKey);
			continue;
		}
		eligible.push({accountKey, account});
	}
	eligible.sort((left, right) => right.account.lastActive - left.account.lastActive);
	const desired = new Map<string, Account>();
	for (const {accountKey, account} of eligible.slice(0, Math.max(0, maxConnections))) {
		desired.set(accountKey, account);
	}
	return {desired, unresolved};
}
