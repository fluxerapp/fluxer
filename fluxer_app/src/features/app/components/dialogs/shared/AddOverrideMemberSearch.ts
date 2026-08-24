// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildMember} from '@app/features/member/models/GuildMember';
import {buildMemberSearchRank} from '@app/features/messaging/utils/AutocompleteOptionBuilders';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {matchSorter} from 'match-sorter';

export interface ParsedOverrideMemberQuery {
	usernameQuery: string;
	tagQuery: string | null;
	hasTagSeparator: boolean;
}

export interface OverrideMemberSelection {
	cachedMembers: ReadonlyArray<GuildMember>;
	workerMemberIds: ReadonlyArray<string>;
	resolveMember: (userId: string) => GuildMember | null;
	excludedIds: ReadonlySet<string>;
	guildId: string;
	query: string;
	limit: number;
}

export function parseOverrideMemberQuery(query: string): ParsedOverrideMemberQuery {
	const hashIndex = query.indexOf('#');
	if (hashIndex === -1) {
		return {usernameQuery: query, tagQuery: null, hasTagSeparator: false};
	}
	return {
		usernameQuery: query.slice(0, hashIndex),
		tagQuery: query.slice(hashIndex + 1),
		hasTagSeparator: true,
	};
}

export function getOverrideMemberLabel(member: GuildMember, guildId: string): string {
	return NicknameUtils.getNickname(member.user, guildId);
}

function compareByLabel(a: GuildMember, b: GuildMember, guildId: string): number {
	return getOverrideMemberLabel(a, guildId)
		.toLowerCase()
		.localeCompare(getOverrideMemberLabel(b, guildId).toLowerCase());
}

function matchOverrideMembers(
	members: ReadonlyArray<GuildMember>,
	guildId: string,
	parsed: ParsedOverrideMemberQuery,
): Array<GuildMember> {
	if (parsed.hasTagSeparator) {
		const usernameLower = parsed.usernameQuery.toLowerCase();
		const tagLower = parsed.tagQuery?.toLowerCase() ?? '';
		return members.filter((member) => {
			const nick = member.nick?.toLowerCase() ?? '';
			const username = member.user.username.toLowerCase();
			const matchesUsername =
				usernameLower.length === 0 || username.startsWith(usernameLower) || nick.startsWith(usernameLower);
			const matchesTag = tagLower.length === 0 || member.user.discriminator.startsWith(tagLower);
			return matchesUsername && matchesTag;
		});
	}
	const trimmed = parsed.usernameQuery.trim();
	if (trimmed.length === 0) {
		return [...members];
	}
	return matchSorter([...members], trimmed, {
		keys: [(member: GuildMember) => getOverrideMemberLabel(member, guildId), 'nick', 'user.username', 'user.tag'],
	});
}

export function selectOverrideMembers({
	cachedMembers,
	workerMemberIds,
	resolveMember,
	excludedIds,
	guildId,
	query,
	limit,
}: OverrideMemberSelection): Array<GuildMember> {
	const candidates = new Map<string, GuildMember>();
	for (const member of cachedMembers) {
		if (excludedIds.has(member.user.id)) continue;
		candidates.set(member.user.id, member);
	}
	const workerRanked: Array<GuildMember> = [];
	for (const userId of workerMemberIds) {
		if (excludedIds.has(userId)) continue;
		const cached = candidates.get(userId);
		if (cached != null) {
			workerRanked.push(cached);
			continue;
		}
		const member = resolveMember(userId);
		if (member == null) continue;
		candidates.set(userId, member);
		workerRanked.push(member);
	}
	const trimmed = query.trim();
	if (trimmed.length === 0) {
		return [...candidates.values()].sort((a, b) => compareByLabel(a, b, guildId)).slice(0, limit);
	}
	const matched = matchOverrideMembers([...candidates.values()], guildId, parseOverrideMemberQuery(trimmed));
	const searchRank = buildMemberSearchRank(workerRanked);
	if (searchRank.size === 0) {
		return [...matched].sort((a, b) => compareByLabel(a, b, guildId)).slice(0, limit);
	}
	const noRankSentinel = Number.MAX_SAFE_INTEGER;
	return [...matched]
		.sort((a, b) => {
			const rankA = searchRank.get(a.user.id) ?? noRankSentinel;
			const rankB = searchRank.get(b.user.id) ?? noRankSentinel;
			if (rankA !== rankB) return rankA - rankB;
			return compareByLabel(a, b, guildId);
		})
		.slice(0, limit);
}
