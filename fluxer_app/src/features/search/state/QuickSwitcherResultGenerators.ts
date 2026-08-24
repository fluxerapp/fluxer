// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import Guilds from '@app/features/guild/state/Guilds';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import GuildMembers from '@app/features/member/state/GuildMembers';
import Navigation from '@app/features/navigation/state/Navigation';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import ReadStates from '@app/features/read_state/state/ReadStates';
import {buildChannelCandidate} from '@app/features/search/state/QuickSwitcherCandidateBuilder';
import {
	candidateToResult,
	createHeaderResult,
	getHeaderTitle,
	type HeaderTitleType,
} from '@app/features/search/state/QuickSwitcherResultConverters';
import type {
	Candidate,
	CandidateSets,
	QuickSwitcherExecutableResult,
	QuickSwitcherQueryMode,
	QuickSwitcherResult,
	UserCandidate,
} from '@app/features/search/state/QuickSwitcherTypes';
import {
	MAX_GENERAL_RESULTS,
	MAX_QUERY_MODE_RESULTS,
	MAX_RECENT_RESULTS,
	MAX_UNREAD_RESULTS,
} from '@app/features/search/state/QuickSwitcherTypes';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {QuickSwitcherResultTypes} from '@fluxer/constants/src/QuickSwitcherConstants';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import type {I18n} from '@lingui/core';
import {matchSorter, rankings} from 'match-sorter';

function getChannelRecency(channel: {id: string; lastMessageId: string | null}): number {
	if (channel.lastMessageId) {
		return SnowflakeUtils.extractTimestamp(channel.lastMessageId);
	}
	return SnowflakeUtils.extractTimestamp(channel.id);
}

function getExcludedChannelIds(): Set<string> {
	const excluded = new Set<string>();
	const currentChannelId = Navigation.channelId ?? SelectedChannel.currentChannelId;
	if (!currentChannelId) return excluded;
	excluded.add(currentChannelId);
	const currentChannel = Channels.getChannel(currentChannelId);
	if (currentChannel?.parentId) {
		excluded.add(currentChannel.parentId);
	}
	return excluded;
}

function matchCandidates<T extends Candidate>(candidates: Array<T>, search: string, limit: number): Array<T> {
	if (candidates.length === 0) {
		return [];
	}
	if (search.length === 0) {
		return sortCandidatesByWeight(candidates).slice(0, limit);
	}
	const results = matchSorter(candidates, search, {
		keys: [
			'title',
			{minRanking: rankings.CONTAINS, key: 'subtitle'},
			{minRanking: rankings.CONTAINS, key: (item) => item.searchValues},
		],
	});
	return results.slice(0, limit);
}

function sortCandidatesByWeight<T extends Candidate>(candidates: Array<T>): Array<T> {
	return [...candidates].sort((a, b) => {
		if (b.sortWeight !== a.sortWeight) {
			return b.sortWeight - a.sortWeight;
		}
		return a.title.localeCompare(b.title);
	});
}

function createDefaultResultFromChannel(
	channel: Channel,
	i18n: I18n,
	viewContext?: string,
): QuickSwitcherExecutableResult | null {
	const candidate = buildChannelCandidate(channel, i18n);
	return candidate ? candidateToResult(candidate, i18n, viewContext) : null;
}

export function generateDefaultResults(i18n: I18n): Array<QuickSwitcherResult> {
	const recentVisits = SelectedChannel.recentChannelVisits;
	const excludedIds = getExcludedChannelIds();
	const recentEntries: Array<{
		channelId: string;
		result: QuickSwitcherExecutableResult;
	}> = [];
	for (const visit of recentVisits) {
		if (excludedIds.has(visit.channelId)) continue;
		const channel = Channels.getChannel(visit.channelId);
		if (!channel) continue;
		const result = createDefaultResultFromChannel(channel, i18n, visit.guildId);
		if (result) {
			recentEntries.push({channelId: visit.channelId, result});
		}
	}
	const recentSlicedEntries = recentEntries.slice(0, MAX_RECENT_RESULTS);
	const recentSliced = recentSlicedEntries.map(({result}) => result);
	const recentChannelIds = new Set(recentSlicedEntries.map(({channelId}) => channelId));
	const unreadResults = generateUnreadResults(i18n, recentChannelIds);
	return [...recentSliced, ...unreadResults];
}

function generateUnreadResults(
	i18n: I18n,
	additionalExcludedChannelIds: ReadonlySet<string>,
): Array<QuickSwitcherExecutableResult> {
	const excludedIds = getExcludedChannelIds();
	const unreadChannels = ReadStates.getChannelIds()
		.filter((channelId) => {
			if (excludedIds.has(channelId) || additionalExcludedChannelIds.has(channelId)) {
				return false;
			}
			return ReadStates.isUnreadOrMentioned(channelId);
		})
		.map((channelId) => Channels.getChannel(channelId))
		.filter((channel): channel is Channel => channel != null)
		.sort((a, b) => getChannelRecency(b) - getChannelRecency(a))
		.slice(0, MAX_UNREAD_RESULTS);
	const results: Array<QuickSwitcherExecutableResult> = [];
	for (const channel of unreadChannels) {
		const result = createDefaultResultFromChannel(channel, i18n);
		if (result) {
			results.push(result);
		}
	}
	return results;
}

export function generateQueryModeResults(
	queryMode: QuickSwitcherQueryMode,
	search: string,
	sets: CandidateSets,
	i18n: I18n,
	memberSearchResults: Array<GuildMember>,
): Array<QuickSwitcherResult> {
	let candidates: Array<Candidate>;
	switch (queryMode) {
		case QuickSwitcherResultTypes.USER:
			candidates = buildUserCandidatesWithMemberSearch(sets.users, memberSearchResults);
			break;
		case QuickSwitcherResultTypes.TEXT_CHANNEL:
			candidates = sets.textChannels;
			break;
		case QuickSwitcherResultTypes.VOICE_CHANNEL:
			candidates = sets.voiceChannels;
			break;
		case QuickSwitcherResultTypes.GUILD:
			candidates = [...sets.guilds, ...sets.virtualGuilds];
			break;
		case QuickSwitcherResultTypes.VIRTUAL_GUILD:
			candidates = sets.virtualGuilds;
			break;
		case QuickSwitcherResultTypes.SETTINGS:
			candidates = sets.settings;
			break;
		default:
			candidates = [];
	}
	if (
		search.length === 0 &&
		(queryMode === QuickSwitcherResultTypes.TEXT_CHANNEL || queryMode === QuickSwitcherResultTypes.VOICE_CHANNEL)
	) {
		const excludedIds = getExcludedChannelIds();
		candidates = candidates.filter((c) => !excludedIds.has(c.id));
	}
	const matches = matchCandidates(candidates, search, MAX_QUERY_MODE_RESULTS);
	if (matches.length === 0) {
		return [];
	}
	return [
		createHeaderResult(`query-${queryMode}`, getHeaderTitle(queryMode, i18n)),
		...matches.map((c) => candidateToResult(c, i18n)),
	];
}

export function generateGeneralResults(search: string, sets: CandidateSets, i18n: I18n): Array<QuickSwitcherResult> {
	const sections: Array<{
		type: HeaderTitleType;
		headerId: string;
		candidates: Array<Candidate>;
	}> = [
		{type: QuickSwitcherResultTypes.USER, headerId: 'people', candidates: sets.users},
		{type: QuickSwitcherResultTypes.GROUP_DM, headerId: 'group-dm', candidates: sets.groupDMs},
		{type: QuickSwitcherResultTypes.TEXT_CHANNEL, headerId: 'text-channels', candidates: sets.textChannels},
		{type: QuickSwitcherResultTypes.VOICE_CHANNEL, headerId: 'voice-channels', candidates: sets.voiceChannels},
		{type: QuickSwitcherResultTypes.GUILD, headerId: 'guilds', candidates: [...sets.guilds, ...sets.virtualGuilds]},
		{type: QuickSwitcherResultTypes.SETTINGS, headerId: 'settings', candidates: sets.settings},
	];
	const results: Array<QuickSwitcherResult> = [];
	for (const section of sections) {
		const matches = matchCandidates(section.candidates, search, MAX_GENERAL_RESULTS);
		if (matches.length === 0) continue;
		results.push(createHeaderResult(`section-${section.headerId}`, getHeaderTitle(section.type, i18n)));
		results.push(...matches.map((candidate) => candidateToResult(candidate, i18n)));
	}
	return results;
}

function buildUserCandidatesWithMemberSearch(
	baseCandidates: Array<UserCandidate>,
	memberSearchResults: Array<GuildMember>,
): Array<UserCandidate> {
	if (memberSearchResults.length === 0) {
		return baseCandidates;
	}
	const candidateMap = new Map<string, UserCandidate>();
	for (const candidate of baseCandidates) {
		candidateMap.set(candidate.user.id, candidate);
	}
	const currentUserId = Users.getCurrentUser()?.id ?? null;
	for (const member of memberSearchResults) {
		const userId = member.user.id;
		if (currentUserId && userId === currentUserId) {
			continue;
		}
		if (candidateMap.has(userId)) {
			continue;
		}
		candidateMap.set(userId, createUserCandidateFromMember(member));
	}
	return Array.from(candidateMap.values());
}

function createUserCandidateFromMember(member: GuildMember): UserCandidate {
	const title = member.nick
		? NicknameUtils.formatNicknameForStreamerMode(member.nick)
		: NicknameUtils.getNickname(member.user, member.guildId);
	const subtitle = NicknameUtils.formatUserTagForStreamerMode(member.user);
	const searchValues = [title, subtitle, member.user.username, member.user.id, member.nick].filter(
		Boolean,
	) as Array<string>;
	return {
		type: QuickSwitcherResultTypes.USER,
		id: member.user.id,
		title,
		subtitle,
		user: member.user,
		dmChannelId: null,
		searchValues,
		sortWeight: member.joinedAt ? member.joinedAt.getTime() : 0,
	};
}

export function resolveTransformedMember(member: {id: string; guildIds?: Array<string>}): GuildMember | null {
	const guildIds = member.guildIds ?? [];
	for (const guildId of guildIds) {
		const record = GuildMembers.getMember(guildId, member.id);
		if (record) {
			return record;
		}
	}
	for (const guild of Guilds.getGuilds()) {
		const record = GuildMembers.getMember(guild.id, member.id);
		if (record) {
			return record;
		}
	}
	return null;
}
