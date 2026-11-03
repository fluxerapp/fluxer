// SPDX-License-Identifier: AGPL-3.0-or-later

import {createInviteCode, type UserID} from '@app/api/BrandedTypes';
import {userGuildVisitsActive} from '@app/api/experiment/GuildVisitsGate';
import type {GuildVisitService} from '@app/api/guild/visits/GuildVisitService';
import {parseInviteCodeList} from '@app/api/instance/AutoJoinInviteCodes';
import type {SingleCommunityService} from '@app/api/instance/SingleCommunityService';
import type {InviteService} from '@app/api/invite/InviteService';
import {Logger} from '@app/api/Logger';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {User} from '@app/api/models/User';
import {assertTierAllows, canReachOthers, GUILD_VISITS_CAPABILITY, type TierContext} from '@app/api/user/AccountTier';
import {isJsonRecord, parseJsonWithGuard} from '@app/api/utils/JsonBoundaryUtils';
import {TierGatedActions} from '@fluxer/constants/src/AccountTierConstants';
import type {GuildEntryResult} from '@fluxer/schema/src/domains/user/AccountSchemas';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

const PENDING_ENTRY_TTL_SECONDS = 7 * 24 * 3600;

export type PendingEntry = {kind: 'invite'; code: string} | {kind: 'operator'; code: string | null};

function pendingEntryKey(userId: UserID): string {
	return `user:pending_entry:${userId}`;
}

function isPendingEntry(value: unknown): value is PendingEntry {
	if (!isJsonRecord(value)) return false;
	if (value.kind === 'invite') return typeof value.code === 'string';
	return value.kind === 'operator' && (value.code === null || typeof value.code === 'string');
}

function isPendingEntryList(value: unknown): value is Array<PendingEntry> {
	return Array.isArray(value) && value.every(isPendingEntry);
}

export function buildPendingEntries(params: {
	inviteCode: string | null | undefined;
	autoJoinInviteCode: string | null | undefined;
	singleCommunity: boolean;
}): Array<PendingEntry> {
	const entries: Array<PendingEntry> = [];
	const inviteCode = params.inviteCode?.trim();
	if (inviteCode) entries.push({kind: 'invite', code: inviteCode});
	else {
		for (const code of parseInviteCodeList(params.autoJoinInviteCode)) entries.push({kind: 'operator', code});
	}
	if (params.singleCommunity) entries.push({kind: 'operator', code: null});
	return entries;
}

export async function storePendingEntries(kv: IKVProvider, userId: UserID, entries: Array<PendingEntry>) {
	if (entries.length === 0) return;
	await kv.setex(pendingEntryKey(userId), PENDING_ENTRY_TTL_SECONDS, JSON.stringify(entries));
}

export async function takePendingEntries(kv: IKVProvider, userId: UserID): Promise<Array<PendingEntry>> {
	const raw = await kv.getdel(pendingEntryKey(userId));
	if (raw === null) return [];
	return parseJsonWithGuard(raw, isPendingEntryList) ?? [];
}

export interface GuildEntryDeps {
	inviteService: InviteService | null;
	singleCommunityService: SingleCommunityService;
	guildVisitService: GuildVisitService | null;
}

interface EntryParams {
	user: User;
	tierContext: TierContext;
	requestCache: RequestCache;
	ip?: string | null;
}

async function startEntryVisit(
	visits: GuildVisitService,
	params: EntryParams & {entry: PendingEntry},
): Promise<GuildEntryResult | null> {
	const {user, entry, tierContext} = params;
	const context = {user, tierContext, ip: params.ip ?? null};
	const result =
		entry.kind === 'invite'
			? await visits.startFromInvite({...context, code: entry.code})
			: await visits.startForOperator({...context, code: entry.code});
	if (!result) return null;
	return {guild_id: result.guild_id, mode: result.member ? 'member' : 'visit'};
}

async function executeEntry(
	deps: GuildEntryDeps,
	params: EntryParams & {entry: PendingEntry},
): Promise<GuildEntryResult | null> {
	const {user, entry, tierContext, requestCache} = params;
	const memberOnly = entry.kind === 'operator' && canReachOthers(user);
	if (!memberOnly && userGuildVisitsActive(user.id) && tierContext.capabilities.has(GUILD_VISITS_CAPABILITY)) {
		return deps.guildVisitService ? startEntryVisit(deps.guildVisitService, params) : null;
	}
	if (entry.code === null) {
		assertTierAllows(user, TierGatedActions.GUILD_JOIN, tierContext);
		const guildId = await deps.singleCommunityService.joinStockCommunity(user.id, requestCache);
		return guildId === null ? null : {guild_id: guildId.toString(), mode: 'member'};
	}
	if (!deps.inviteService) return null;
	const invite = await deps.inviteService.acceptInvite({
		userId: user.id,
		inviteCode: createInviteCode(entry.code),
		requestCache,
		tierContext,
	});
	return invite.guildId === null ? null : {guild_id: invite.guildId.toString(), mode: 'member'};
}

export async function executeEntries(
	deps: GuildEntryDeps,
	params: EntryParams & {entries: Array<PendingEntry>},
): Promise<GuildEntryResult | null> {
	let result: GuildEntryResult | null = null;
	for (const entry of params.entries) {
		try {
			const entryResult = await executeEntry(deps, {...params, entry});
			result ??= entryResult;
		} catch (error) {
			Logger.warn({userId: params.user.id.toString(), kind: entry.kind, error}, 'Failed to run a guild entry');
		}
	}
	return result;
}
