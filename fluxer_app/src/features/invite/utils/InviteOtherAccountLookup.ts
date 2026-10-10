// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import type {Account} from '@app/features/platform/state/AuthSession';
import {
	type InstanceHTTPTarget,
	instanceRequest,
	instanceTargetFromSnapshot,
} from '@app/features/platform/transport/InstanceHTTP';
import type {Invite} from '@fluxer/schema/src/domains/invite/InviteSchemas';

const INVITE_LOOKUP_TIMEOUT_MS = 10_000;

export interface InviteLookupCandidate {
	readonly account: Account;
	readonly target: InstanceHTTPTarget;
}

export interface InviteAccountMatch {
	readonly account: Account;
	readonly instanceKey: string;
	readonly invite: Invite;
}

interface InviteLookupOrigin {
	readonly accountKey: string | null;
	readonly instanceKey: string;
}

type InviteFetcher = (code: string, target: InstanceHTTPTarget) => Promise<Invite>;

function candidateTarget(account: Account): InstanceHTTPTarget | null {
	if (account.instance == null) {
		return null;
	}
	try {
		return instanceTargetFromSnapshot(account.instance);
	} catch {
		return null;
	}
}

function inviteLookupCandidates(
	accounts: ReadonlyArray<Account>,
	origin: InviteLookupOrigin,
): Array<InviteLookupCandidate> {
	const seenInstances = new Set<string>([origin.instanceKey]);
	const candidates: Array<InviteLookupCandidate> = [];
	const byRecency = [...accounts].sort((left, right) => right.lastActive - left.lastActive);
	for (const account of byRecency) {
		if (account.storageKey === origin.accountKey || account.isValid === false) {
			continue;
		}
		const target = candidateTarget(account);
		if (target == null || seenInstances.has(target.instanceKey)) {
			continue;
		}
		seenInstances.add(target.instanceKey);
		candidates.push({account, target});
	}
	return candidates;
}

async function fetchInviteAnonymously(code: string, target: InstanceHTTPTarget): Promise<Invite> {
	const response = await instanceRequest<Invite>({
		method: 'GET',
		path: Endpoints.INVITE(code),
		target,
		auth: 'none',
		retries: 0,
		timeoutMs: INVITE_LOOKUP_TIMEOUT_MS,
	});
	return response.body;
}

export async function findInviteOnOtherAccount(
	code: string,
	origin: InviteLookupOrigin,
	accounts: ReadonlyArray<Account>,
	fetchInvite: InviteFetcher = fetchInviteAnonymously,
): Promise<InviteAccountMatch | null> {
	const candidates = inviteLookupCandidates(accounts, origin);
	const results = await Promise.allSettled(candidates.map(({target}) => fetchInvite(code, target)));
	for (const [index, result] of results.entries()) {
		if (result.status === 'fulfilled' && result.value != null) {
			const {account, target} = candidates[index];
			return {account, instanceKey: target.instanceKey, invite: result.value};
		}
	}
	return null;
}
