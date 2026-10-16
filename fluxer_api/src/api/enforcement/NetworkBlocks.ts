// SPDX-License-Identifier: AGPL-3.0-or-later

import type {BannedIpEntry} from '@app/api/admin/AdminRepository';
import {listRecentUserIps} from '@app/api/admin/services/KnownUserIps';
import type {UserID} from '@app/api/BrandedTypes';
import {isIpBanExempt} from '@app/api/ban/IpBanExemptions';
import type {User} from '@app/api/models/User';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {isSignInRefused} from '@app/api/user/UserHelpers';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {canonicalizeIpAddress, isPublicIpAddress} from '@fluxer/ip_utils/src/IpAddress';
import type {NetworkBlockCandidatesResponse} from '@fluxer/schema/src/domains/admin/EnforcementSchemas';
import {ms} from 'itty-time';

type NetworkSkipReason = 'exempt' | 'special_purpose';

export interface SkippedNetwork {
	ip: string;
	reason: NetworkSkipReason | 'already_blocked';
}

export interface NetworkBlockPlan {
	ips: Array<string>;
	skipped: Array<SkippedNetwork>;
	expiresAt: Date;
}

const RECENT_NETWORK_WINDOW = ms('30 days');
const SHARED_ACCOUNT_SCAN_LIMIT = 200;

export function latestNetworkBlockExpiry(entries: ReadonlyArray<BannedIpEntry>): Date | null {
	let latest: Date | null = null;
	for (const {expiresAt} of entries) {
		if (expiresAt && (!latest || expiresAt.getTime() > latest.getTime())) latest = expiresAt;
	}
	return latest;
}

function skipReasonOf(ip: string): NetworkSkipReason | null {
	if (isIpBanExempt(ip)) return 'exempt';
	if (!isPublicIpAddress(ip)) return 'special_purpose';
	return null;
}

async function countOtherUnrestrictedAccounts(users: UserRepository, ip: string, subjectId: UserID): Promise<number> {
	const {userIds, total} = await users.listUserIdsByLastActiveIp(ip, SHARED_ACCOUNT_SCAN_LIMIT, 0);
	const others = userIds.filter((userId) => userId !== subjectId);
	if (total > SHARED_ACCOUNT_SCAN_LIMIT) return total - (userIds.length - others.length);
	if (others.length === 0) return 0;
	const accounts = await users.listUsers(others);
	return accounts.filter((account) => !isSignInRefused(account)).length;
}

function recentSince(now: Date): Date {
	return new Date(now.getTime() - RECENT_NETWORK_WINDOW);
}

export async function listNetworkBlockCandidates(
	users: UserRepository,
	user: User,
	now: Date = new Date(),
): Promise<NetworkBlockCandidatesResponse> {
	const recent = await listRecentUserIps(users, user, recentSince(now));
	const networks = await Promise.all(
		recent.map(async ({ip, lastSeenAt}) => {
			const skipReason = skipReasonOf(ip);
			return {
				ip,
				last_seen_at: lastSeenAt.toISOString(),
				skip_reason: skipReason,
				other_unrestricted_accounts: skipReason ? 0 : await countOtherUnrestrictedAccounts(users, ip, user.id),
			};
		}),
	);
	return {networks};
}

export async function planNetworkBlocks(
	users: UserRepository,
	user: User,
	request: {ips: ReadonlyArray<string>; days: number},
	now: Date = new Date(),
): Promise<NetworkBlockPlan> {
	const recent = new Set((await listRecentUserIps(users, user, recentSince(now))).map(({ip}) => ip));
	const plan: NetworkBlockPlan = {
		ips: [],
		skipped: [],
		expiresAt: new Date(now.getTime() + ms(`${request.days} days`)),
	};
	const seen = new Set<string>();
	for (const [index, requested] of request.ips.entries()) {
		const ip = canonicalizeIpAddress(requested) ?? requested;
		if (seen.has(ip)) continue;
		seen.add(ip);
		const path = `block_networks.ips.${index}`;
		if (!recent.has(ip)) {
			throw InputValidationError.fromCode(path, ValidationErrorCodes.ENFORCEMENT_NETWORK_UNKNOWN);
		}
		const skipReason = skipReasonOf(ip);
		if (skipReason) {
			plan.skipped.push({ip, reason: skipReason});
			continue;
		}
		if ((await countOtherUnrestrictedAccounts(users, ip, user.id)) > 0) {
			throw InputValidationError.fromCode(path, ValidationErrorCodes.ENFORCEMENT_NETWORK_SHARED);
		}
		plan.ips.push(ip);
	}
	return plan;
}
