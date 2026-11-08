// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import type {GuildID, UserID} from '@app/api/BrandedTypes';
import type {GuildVisitRow} from '@app/api/database/types/GuildVisitTypes';
import {userGuildVisitsActive} from '@app/api/experiment/GuildVisitsGate';
import {assertNoGuildBanMatch, readGuildBanGeneration} from '@app/api/guild/services/GuildBanChecks';
import {guildVisitEndedTotal} from '@app/api/guild/visits/GuildVisitMetrics';
import {forgetGuildVisitRow, guildVisitRepository, markGuildVisitEnded} from '@app/api/guild/visits/GuildVisitorAccess';
import {bumpGuildVisitorViewGeneration} from '@app/api/guild/visits/GuildVisitorViewGeneration';
import {emitActivity} from '@app/api/infrastructure/activity/ActivityEvents';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {Logger} from '@app/api/Logger';
import {getGatewayService, getKVClient, getWorkerService} from '@app/api/middleware/ServiceRegistry';
import {getGuildRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {
	type GuildVisitEndReason,
	GuildVisitEndReasons,
	GuildVisitSources,
} from '@fluxer/constants/src/GuildVisitConstants';
import {getSameIpDecisionKey} from '@fluxer/ip_utils/src/IpAddress';

const BAN_MEMO_TTL_SECONDS = 600;
const GUILD_PAGE_SIZE = 1000;

const END_GUILD_VISITS_FOR_CODE_TASK = 'endGuildVisitsForCode';
const PRUNE_GUILD_VISITS_TASK = 'pruneGuildVisits';
const END_UNASSIGNED_GUILD_VISITS_TASK = 'endUnassignedGuildVisits';

export type GuildVisitPruneMode = 'disabled' | 'age_restricted' | 'deleted';

export async function endGuildVisit(params: {
	userId: UserID;
	guildId: GuildID;
	reason: GuildVisitEndReason;
	gateway?: IGatewayService;
	notify?: boolean;
}): Promise<void> {
	const {userId, guildId, reason} = params;
	forgetGuildVisitRow(userId, guildId);
	await markGuildVisitEnded(userId, guildId).catch((error: unknown) =>
		Logger.warn({userId: userId.toString(), guildId: guildId.toString(), error}, 'Failed to mark a visit as ended'),
	);
	await guildVisitRepository().delete(userId, guildId);
	if (params.notify !== false) {
		try {
			await (params.gateway ?? getGatewayService()).endVisit({userId, guildId});
		} catch (error) {
			Logger.warn(
				{userId: userId.toString(), guildId: guildId.toString(), error},
				'Failed to end a visit on the gateway',
			);
		}
	}
	guildVisitEndedTotal.inc(`reason="${reason}"`);
	void emitActivity('guild_visit_ended', userId.toString(), {
		user_id: userId.toString(),
		guild_id: guildId.toString(),
		cause: reason,
	});
}

function memoPart(value: string | null | undefined, toKey: (value: string) => string): string {
	return value ? toKey(value) : '-';
}

export async function assertNotBannedFromGuildVisit(params: {
	guildId: GuildID;
	user: Pick<User, 'id' | 'email' | 'moderationIp'>;
	ip: string | null;
}): Promise<void> {
	const {guildId, user, ip} = params;
	const kv = getKVClient();
	const generation = await readGuildBanGeneration(guildId);
	const ipDecisionKey = (value: string) => getSameIpDecisionKey(value) ?? value;
	const ipKey = memoPart(ip, ipDecisionKey);
	const lastActiveIpKey = memoPart(user.moderationIp, ipDecisionKey);
	const emailKey = memoPart(user.email, (value) =>
		createHash('sha256').update(value.toLowerCase()).digest('hex').slice(0, 16),
	);
	const memoKey = `guild_visit:ban_ok:${guildId}:${user.id}:${ipKey}:${lastActiveIpKey}:${emailKey}:${generation}`;
	if ((await kv.get(memoKey)) !== null) return;
	await assertNoGuildBanMatch(getGuildRepository(), {
		guildId,
		generation,
		userId: user.id,
		email: user.email,
		ips: [ip, user.moderationIp],
	});
	await kv.setex(memoKey, BAN_MEMO_TTL_SECONDS, '1');
}

export async function onGuildBanAdded(params: {
	guildId: GuildID;
	targetUserId: UserID;
	targetIp: string | null;
	gateway: IGatewayService;
}): Promise<void> {
	const {guildId, targetUserId, targetIp, gateway} = params;
	await bumpGuildVisitorViewGeneration(guildId);
	if (await guildVisitRepository().get(targetUserId, guildId)) {
		await endGuildVisit({userId: targetUserId, guildId, reason: GuildVisitEndReasons.BANNED, gateway});
	}
	const ipKey = targetIp ? getSameIpDecisionKey(targetIp) : null;
	if (!ipKey) return;
	let evicted: Array<UserID> = [];
	try {
		evicted = await gateway.evictVisitors({guildId, ipKeys: [ipKey]});
	} catch (error) {
		Logger.warn({guildId: guildId.toString(), error}, 'Failed to evict visitors on a banned address');
	}
	for (const userId of evicted) {
		if (userId === targetUserId) continue;
		await endGuildVisit({userId, guildId, reason: GuildVisitEndReasons.BANNED, gateway});
	}
}

export async function enqueueEndGuildVisitsForCode(guildId: GuildID, code: string): Promise<void> {
	try {
		await getWorkerService().addJob(END_GUILD_VISITS_FOR_CODE_TASK, {guildId: guildId.toString(), code});
	} catch (error) {
		Logger.error({guildId: guildId.toString(), error}, 'Failed to queue ending visits for an invite');
	}
}

export async function enqueuePruneGuildVisits(guildId: GuildID, mode: GuildVisitPruneMode): Promise<void> {
	try {
		await getWorkerService().addJob(PRUNE_GUILD_VISITS_TASK, {guildId: guildId.toString(), mode});
	} catch (error) {
		Logger.error({guildId: guildId.toString(), error}, 'Failed to queue pruning guild visits');
	}
}

export async function enqueueEndUnassignedGuildVisits(): Promise<void> {
	try {
		await getWorkerService().addJob(END_UNASSIGNED_GUILD_VISITS_TASK, {});
	} catch (error) {
		Logger.error({error}, 'Failed to queue ending visits outside the guild visits experiment');
	}
}

export async function endUnassignedGuildVisits(gateway: IGatewayService = getGatewayService()): Promise<number> {
	const repository = guildVisitRepository();
	let after: UserID | null = null;
	let ended = 0;
	do {
		const page: {userIds: Array<UserID>; next: UserID | null} = await repository.pageVisitorIds(after, GUILD_PAGE_SIZE);
		for (const userId of page.userIds) {
			if (userGuildVisitsActive(userId)) continue;
			for (const row of await repository.list(userId)) {
				forgetGuildVisitRow(userId, row.guild_id);
				try {
					await gateway.endVisit({userId, guildId: row.guild_id});
					ended++;
				} catch (error) {
					Logger.warn(
						{userId: userId.toString(), guildId: row.guild_id.toString(), error},
						'Failed to end a visit on the gateway',
					);
				}
			}
		}
		after = page.next;
	} while (after !== null);
	return ended;
}

export async function onGuildVisitSettingsChanged(params: {
	guildId: GuildID;
	before: {features: ReadonlySet<string>; nsfw: boolean};
	after: {features: ReadonlySet<string>; nsfw: boolean};
}): Promise<void> {
	const {guildId, before, after} = params;
	await bumpGuildVisitorViewGeneration(guildId);
	if (before.features.has(GuildFeatures.VISITS_ENABLED) && !after.features.has(GuildFeatures.VISITS_ENABLED)) {
		await enqueuePruneGuildVisits(guildId, 'disabled');
	}
	if (!before.nsfw && after.nsfw) {
		await enqueuePruneGuildVisits(guildId, 'age_restricted');
	}
}

async function forEachGuildVisit(guildId: GuildID, visit: (row: GuildVisitRow) => Promise<void>): Promise<number> {
	let after: UserID | null = null;
	let count = 0;
	do {
		const page: {rows: Array<GuildVisitRow>; next: UserID | null} = await guildVisitRepository().pageByGuild(
			guildId,
			after,
			GUILD_PAGE_SIZE,
		);
		for (const row of page.rows) {
			await visit(row);
			count++;
		}
		after = page.next;
	} while (after !== null);
	return count;
}

export async function endGuildVisitsForCode(guildId: GuildID, code: string): Promise<number> {
	let ended = 0;
	await forEachGuildVisit(guildId, async (row) => {
		if (row.invite_code !== code) return;
		await endGuildVisit({userId: row.user_id, guildId, reason: GuildVisitEndReasons.INVITE_REVOKED});
		ended++;
	});
	return ended;
}

export async function pruneGuildVisits(
	guildId: GuildID,
	mode: GuildVisitPruneMode,
	adultContentAllowed: (userId: UserID) => Promise<boolean>,
): Promise<number> {
	let ended = 0;
	await forEachGuildVisit(guildId, async (row) => {
		if (mode === 'disabled' && row.source === GuildVisitSources.OPERATOR) return;
		if (mode === 'age_restricted' && (await adultContentAllowed(row.user_id))) return;
		const reason = mode === 'age_restricted' ? GuildVisitEndReasons.AGE_RESTRICTED : GuildVisitEndReasons.DISABLED;
		await endGuildVisit({userId: row.user_id, guildId, reason});
		ended++;
	});
	return ended;
}
