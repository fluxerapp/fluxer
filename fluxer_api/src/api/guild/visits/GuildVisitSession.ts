// SPDX-License-Identifier: AGPL-3.0-or-later

import {createInviteCode, type GuildID} from '@app/api/BrandedTypes';
import type {GuildVisitRow} from '@app/api/database/types/GuildVisitTypes';
import {userGuildVisitsActive} from '@app/api/experiment/GuildVisitsGate';
import {assertNotBannedFromGuildVisit, endGuildVisit} from '@app/api/guild/visits/GuildVisitLifecycle';
import {
	guildVisitAuthorizedByView,
	guildVisitRepository,
	loadGuildVisitorView,
} from '@app/api/guild/visits/GuildVisitorAccess';
import {maxGuildVisitsFor} from '@app/api/guild/visits/GuildVisitService';
import {getInviteRepository, getLimitConfigService} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import {canUserAccessNsfwContent} from '@app/api/utils/AgeUtils';
import {GuildVisitEndReasons, GuildVisitSources} from '@fluxer/constants/src/GuildVisitConstants';
import {BannedFromGuildError} from '@fluxer/errors/src/domains/guild/BannedFromGuildError';
import {IpBannedFromGuildError} from '@fluxer/errors/src/domains/guild/IpBannedFromGuildError';

export interface SessionGuildVisit {
	guildId: GuildID;
	operator: boolean;
}

type SessionVisitCheck = 'keep' | 'drop' | keyof typeof GuildVisitEndReasons;

async function checkSessionVisit(
	row: GuildVisitRow,
	user: User,
	ip: string | null,
	now: number,
): Promise<SessionVisitCheck> {
	if (row.invite_expires_at !== null && row.invite_expires_at.getTime() <= now) return 'EXPIRED';
	const view = await loadGuildVisitorView(row.guild_id);
	if (!view || !guildVisitAuthorizedByView(row, view)) return 'drop';
	if (view.guild.nsfw && !canUserAccessNsfwContent(user)) return 'drop';
	if (row.source === GuildVisitSources.INVITE || row.source === GuildVisitSources.VANITY_URL) {
		const invite = row.invite_code ? await getInviteRepository().findUnique(createInviteCode(row.invite_code)) : null;
		if (!invite || invite.guildId !== row.guild_id) return 'INVITE_REVOKED';
	}
	try {
		await assertNotBannedFromGuildVisit({guildId: row.guild_id, user, ip});
	} catch (error) {
		if (error instanceof BannedFromGuildError || error instanceof IpBannedFromGuildError) {
			return 'BANNED';
		}
		throw error;
	}
	return 'keep';
}

export async function resolveSessionGuildVisits(params: {
	user: User;
	ip: string | null;
	memberGuildIds: ReadonlyArray<GuildID>;
}): Promise<Array<SessionGuildVisit>> {
	const {user, ip} = params;
	if (user.isBot || !userGuildVisitsActive(user.id)) return [];
	const members = new Set(params.memberGuildIds.map((guildId) => guildId.toString()));
	const rows = await guildVisitRepository().list(user.id);
	const now = Date.now();
	const kept: Array<SessionGuildVisit> = [];
	const cap = maxGuildVisitsFor(user, getLimitConfigService());
	const ordered = [...rows].sort((left, right) => right.opened_at.getTime() - left.opened_at.getTime());
	for (const row of ordered) {
		if (members.has(row.guild_id.toString())) {
			await guildVisitRepository().delete(user.id, row.guild_id);
			continue;
		}
		if (kept.length >= cap) break;
		const check = await checkSessionVisit(row, user, ip, now);
		if (check === 'keep') {
			kept.push({guildId: row.guild_id, operator: row.source === GuildVisitSources.OPERATOR});
		} else if (check !== 'drop') {
			await endGuildVisit({userId: user.id, guildId: row.guild_id, reason: GuildVisitEndReasons[check], notify: false});
		}
	}
	return kept;
}
