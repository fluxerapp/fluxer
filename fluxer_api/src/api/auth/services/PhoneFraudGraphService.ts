// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {Logger} from '@app/api/Logger';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

const COHORT_KEY_PREFIX = 'auth:ip24-cohort:';
const COHORT_TTL_SECONDS = 7 * 24 * 60 * 60;
const PROPAGATION_LOOKBACK_DAYS = 2;
const MAX_NEIGHBOURS_TO_FLAG = 50;

function cidr24For(ip: string): string | null {
	if (!ip.includes('.')) return null;
	const parts = ip.split('.');
	if (parts.length !== 4) return null;
	return `${parts[0]}.${parts[1]}.${parts[2]}.0/24`;
}

function dayKey(when: Date): string {
	const y = when.getUTCFullYear().toString().padStart(4, '0');
	const m = (when.getUTCMonth() + 1).toString().padStart(2, '0');
	const d = when.getUTCDate().toString().padStart(2, '0');
	return `${y}-${m}-${d}`;
}

function cohortKey(cidr24: string, day: string): string {
	return `${COHORT_KEY_PREFIX}${cidr24}:${day}`;
}

export class PhoneFraudGraphService {
	constructor(
		private readonly kv: IKVProvider,
		private readonly users: IUserRepository,
	) {}

	async recordSessionForCohortGraph(userId: UserID, clientIp: string, createdAt: Date): Promise<void> {
		const cidr = cidr24For(clientIp);
		if (!cidr) return;
		const key = cohortKey(cidr, dayKey(createdAt));
		await this.kv.sadd(key, userId.toString());
		await this.kv.expire(key, COHORT_TTL_SECONDS);
	}

	async propagateHardBlock(blockedUserId: UserID | null, clientIp: string | null): Promise<number> {
		if (!clientIp) return 0;
		const cidr = cidr24For(clientIp);
		if (!cidr) return 0;
		const now = new Date();
		const days: Array<string> = [];
		for (let i = 0; i <= PROPAGATION_LOOKBACK_DAYS; i++) {
			const d = new Date(now.getTime() - i * 24 * 60 * 60 * 1000);
			days.push(dayKey(d));
		}
		const neighbours = new Set<string>();
		for (const day of days) {
			const members = await this.kv.smembers(cohortKey(cidr, day));
			for (const m of members) neighbours.add(m);
			if (neighbours.size >= MAX_NEIGHBOURS_TO_FLAG) break;
		}
		const blockedIdStr = blockedUserId ? blockedUserId.toString() : null;
		let flaggedCount = 0;
		for (const neighbourIdStr of neighbours) {
			if (flaggedCount >= MAX_NEIGHBOURS_TO_FLAG) break;
			if (neighbourIdStr === blockedIdStr) continue;
			try {
				const neighbourId = BigInt(neighbourIdStr) as UserID;
				const user = await this.users.findUnique(neighbourId);
				if (!user) continue;
				if ((user.flags & UserFlags.FORCE_INBOUND_PHONE_VERIFICATION) !== 0n) continue;
				if ((user.flags & UserFlags.NOT_SUSPICIOUS) !== 0n) continue;
				const newFlags = user.flags | UserFlags.FORCE_INBOUND_PHONE_VERIFICATION;
				await this.users.patchUpsert(neighbourId, {flags: newFlags}, user.toRow());
				flaggedCount++;
			} catch (error) {
				Logger.warn({neighbourId: neighbourIdStr, error}, 'phone_fraud_graph.flag failed for neighbour');
			}
		}
		Logger.warn(
			{
				cidr24: cidr,
				blockedUserId: blockedIdStr,
				neighboursSeen: neighbours.size,
				neighboursFlagged: flaggedCount,
			},
			'phone_fraud_graph.propagated hard-block to /24 cohort',
		);
		return flaggedCount;
	}
}
