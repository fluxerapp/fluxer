// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	compareSearchResults,
	type ForwardChannelCandidate,
	type ForwardSearchResult,
	type ForwardUserCandidate,
	searchChannels,
	searchGroupDMs,
	searchGuilds,
	searchUsers,
} from '@app/features/app/components/dialogs/shared/ForwardDestinationSearch';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import {compareChannels} from '@app/features/channel/utils/ChannelUtils';
import Guilds from '@app/features/guild/state/Guilds';
import {MENTIONS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import GuildMembers from '@app/features/member/state/GuildMembers';
import Messages from '@app/features/messaging/state/MessagingMessages';
import Navigation from '@app/features/navigation/state/Navigation';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import Permission from '@app/features/permissions/state/Permission';
import ReadStates from '@app/features/read_state/state/ReadStates';
import Relationships from '@app/features/relationship/state/Relationships';
import {buildChannelCandidate} from '@app/features/search/state/QuickSwitcherCandidateBuilder';
import {candidateToResult, createHeaderResult} from '@app/features/search/state/QuickSwitcherResultConverters';
import type {
	Candidate,
	CandidateSets,
	QuickSwitcherExecutableResult,
	QuickSwitcherQueryMode,
	QuickSwitcherResult,
	UserCandidate,
} from '@app/features/search/state/QuickSwitcherTypes';
import type {ForwardSearchCandidates} from '@app/features/search/utils/DestinationSearchSources';
import {createSearchTerm, scoreSearchTerm} from '@app/features/search/utils/SearchTextMatching';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {QuickSwitcherResultTypes} from '@fluxer/constants/src/QuickSwitcherConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const PREVIOUS_CHANNELS_DESCRIPTOR = msg({
	message: 'Previous channels',
	comment: 'Quick switcher section header above the channels the user visited most recently.',
});
const UNREAD_CHANNELS_DESCRIPTOR = msg({
	message: 'Unread channels',
	comment: 'Quick switcher section header above the unread channels of the current community.',
});
const SEARCHING_TEXT_CHANNELS_DESCRIPTOR = msg({
	message: 'Searching text channels',
	comment: 'Quick switcher header shown while the query starts with # and only text channels are searched.',
});
const SEARCHING_VOICE_CHANNELS_DESCRIPTOR = msg({
	message: 'Searching voice channels',
	comment: 'Quick switcher header shown while the query starts with ! and only voice channels are searched.',
});
const SEARCHING_COMMUNITIES_DESCRIPTOR = msg({
	message: 'Searching communities',
	comment: 'Quick switcher header shown while the query starts with * and only communities are searched.',
});
const SEARCHING_ALL_USERS_DESCRIPTOR = msg({
	message: 'Searching all users',
	comment: 'Quick switcher header shown while the query starts with @ outside a community, or with @@.',
});
const SEARCHING_USERS_IN_COMMUNITY_DESCRIPTOR = msg({
	message: 'Searching friends and members of {communityName}',
	comment:
		'Quick switcher header shown while the query starts with @ inside a community. communityName is the community name.',
});

const HISTORY_LIMIT = 8;
const PREVIOUS_CHANNELS_WITH_SECTIONS = 3;
const PREVIOUS_CHANNELS_ALONE = 7;
const HISTORY_HIDDEN_FROM_SECTIONS = 3;
const CATEGORY_LIMIT = 5;
const MODE_LIMIT = 100;
const RECENT_AUTHOR_LIMIT = 100;
const SCORE_SCALE = 1000;
const LISTING_TEXT_SCORE = 7;
const LISTING_VOICE_IN_TEXT_SCORE = 6;
const LISTING_FRECENCY_BONUS = 3;

export type QuickSwitcherModeVariant = 'all_users' | null;

interface RankedCandidate {
	readonly candidate: Candidate;
	readonly matchedText: string;
	readonly score: number;
}

interface SearchContext {
	readonly confusables: ReadonlyMap<string, string>;
	readonly sets: CandidateSets;
	readonly sources: ForwardSearchCandidates;
}

function getCurrentChannelId(): string | null {
	return Navigation.channelId ?? SelectedChannel.currentChannelId ?? null;
}

function getCurrentGuildId(): string | null {
	const guildId = SelectedGuild.selectedGuildId;
	if (guildId == null || guildId === ME || Guilds.getGuild(guildId) == null) return null;
	return guildId;
}

function isChannelVisible(channel: Channel): boolean {
	if (channel.guildId == null) return true;
	return Permission.can(Permissions.VIEW_CHANNEL, channel);
}

function channelResult(channel: Channel, i18n: I18n): QuickSwitcherExecutableResult | null {
	const candidate = buildChannelCandidate(channel, i18n);
	return candidate == null ? null : candidateToResult(candidate, i18n);
}

function collectChannelResults(
	channelIds: ReadonlyArray<string>,
	i18n: I18n,
	seen: Set<string>,
): Array<QuickSwitcherExecutableResult> {
	const results: Array<QuickSwitcherExecutableResult> = [];
	for (const channelId of channelIds) {
		if (seen.has(channelId)) continue;
		const channel = Channels.getChannel(channelId);
		if (channel == null || !isChannelVisible(channel)) continue;
		const result = channelResult(channel, i18n);
		if (result == null) continue;
		seen.add(channelId);
		results.push(result);
	}
	return results;
}

function isUnreadInCurrentCommunity(channel: Channel): boolean {
	if (channel.type !== ChannelTypes.GUILD_TEXT || !isChannelVisible(channel)) return false;
	if (UserGuildSettings.isMutedAtAnyLevel(channel.guildId ?? null, channel.id)) return false;
	if (!ReadStates.hasUnread(channel.id)) return false;
	const unreadLevel = UserGuildSettings.resolvedGuildUnreadBadgesLevel({
		id: channel.id,
		guildId: channel.guildId ?? undefined,
		parentId: channel.parentId ?? undefined,
		type: channel.type,
	});
	return unreadLevel === MessageNotifications.ALL_MESSAGES;
}

export function generateDefaultResults(i18n: I18n): Array<QuickSwitcherResult> {
	const currentChannelId = getCurrentChannelId();
	const history = [...new Set(SelectedChannel.sortedRecentVisits.map((visit) => visit.channelId))]
		.filter((channelId) => channelId !== currentChannelId)
		.slice(0, HISTORY_LIMIT - 1);
	const previous = collectChannelResults(history, i18n, new Set());
	const seen = new Set(previous.slice(0, HISTORY_HIDDEN_FROM_SECTIONS).map((result) => resultChannelId(result)));
	if (currentChannelId != null) seen.add(currentChannelId);
	const sections: Array<QuickSwitcherResult> = [];
	const mentions = collectChannelResults([...ReadStates.mentionChannelIds].reverse(), i18n, seen);
	if (mentions.length > 0) {
		sections.push(createHeaderResult('section-mentions', i18n._(MENTIONS_DESCRIPTOR)), ...mentions);
	}
	const guildId = getCurrentGuildId();
	if (guildId != null) {
		const unreadIds = [...Channels.getGuildChannels(guildId)]
			.filter(isUnreadInCurrentCommunity)
			.sort(compareChannels)
			.map((channel) => channel.id);
		const unread = collectChannelResults(unreadIds, i18n, seen);
		if (unread.length > 0) {
			sections.push(createHeaderResult('section-unread', i18n._(UNREAD_CHANNELS_DESCRIPTOR)), ...unread);
		}
	}
	const previousCap = sections.length > 0 ? PREVIOUS_CHANNELS_WITH_SECTIONS : PREVIOUS_CHANNELS_ALONE;
	const shownPrevious = previous.slice(0, previousCap);
	if (shownPrevious.length === 0) return sections;
	return [createHeaderResult('section-previous', i18n._(PREVIOUS_CHANNELS_DESCRIPTOR)), ...shownPrevious, ...sections];
}

function resultChannelId(result: QuickSwitcherExecutableResult): string {
	if (result.type === QuickSwitcherResultTypes.USER) return result.dmChannelId ?? result.id;
	return result.id;
}

function getModeHeader(
	prefixMode: QuickSwitcherQueryMode,
	variant: QuickSwitcherModeVariant,
	i18n: I18n,
): QuickSwitcherResult {
	switch (prefixMode) {
		case QuickSwitcherResultTypes.USER: {
			const guild = variant === 'all_users' ? null : getCurrentGuildId();
			const title =
				guild == null
					? i18n._(SEARCHING_ALL_USERS_DESCRIPTOR)
					: i18n._(SEARCHING_USERS_IN_COMMUNITY_DESCRIPTOR, {communityName: Guilds.getGuild(guild)?.name ?? ''});
			return createHeaderResult(`query-${prefixMode}`, title);
		}
		case QuickSwitcherResultTypes.TEXT_CHANNEL:
			return createHeaderResult(`query-${prefixMode}`, i18n._(SEARCHING_TEXT_CHANNELS_DESCRIPTOR));
		case QuickSwitcherResultTypes.VOICE_CHANNEL:
			return createHeaderResult(`query-${prefixMode}`, i18n._(SEARCHING_VOICE_CHANNELS_DESCRIPTOR));
		default:
			return createHeaderResult(`query-${prefixMode}`, i18n._(SEARCHING_COMMUNITIES_DESCRIPTOR));
	}
}

function buildUserCandidate(userId: string, sets: CandidateSets): UserCandidate | null {
	const existing = sets.users.find((candidate) => candidate.id === userId);
	if (existing != null) return existing;
	const user = Users.getUser(userId);
	if (user == null) return null;
	const title = NicknameUtils.getNickname(user, null);
	const subtitle = NicknameUtils.formatUserTagForStreamerMode(user);
	return {
		type: QuickSwitcherResultTypes.USER,
		id: user.id,
		title,
		subtitle,
		user,
		dmChannelId: null,
		searchValues: [title, subtitle],
		sortWeight: 0,
	};
}

function resolveSearchCandidate(result: ForwardSearchResult, sets: CandidateSets): Candidate | null {
	switch (result.type) {
		case 'user':
			return buildUserCandidate(result.id, sets);
		case 'group_dm':
			return sets.groupDMByChannelId.get(result.id) ?? sets.userByChannelId.get(result.id) ?? null;
		default:
			return sets.channelById.get(result.id) ?? null;
	}
}

function toRanked(results: ReadonlyArray<ForwardSearchResult>, sets: CandidateSets): Array<RankedCandidate> {
	const ranked: Array<RankedCandidate> = [];
	for (const result of results) {
		const candidate = resolveSearchCandidate(result, sets);
		if (candidate != null) ranked.push({candidate, matchedText: result.matchedText, score: result.score});
	}
	return ranked;
}

function compareRanked(left: RankedCandidate, right: RankedCandidate): number {
	return compareSearchResults(
		{matchedText: left.matchedText, id: left.candidate.id, score: left.score, type: rankedType(left)},
		{matchedText: right.matchedText, id: right.candidate.id, score: right.score, type: rankedType(right)},
	);
}

function rankedType(ranked: RankedCandidate): ForwardSearchResult['type'] {
	return ranked.candidate.type === QuickSwitcherResultTypes.USER ? 'user' : 'text_channel';
}

function rankedToResult(ranked: RankedCandidate, i18n: I18n): QuickSwitcherExecutableResult {
	const result = candidateToResult(ranked.candidate, i18n);
	if (result.type !== QuickSwitcherResultTypes.USER) return result;
	const {user} = result;
	const tag = NicknameUtils.formatUserTagForStreamerMode(user);
	const matched = ranked.matchedText;
	if (matched === '' || matched === user.username || matched === user.id || matched === tag) return result;
	return {...result, title: NicknameUtils.formatNicknameForStreamerMode(matched)};
}

function currentUserBlacklist(): ReadonlySet<string> {
	const currentUserId = Users.currentUserId;
	return new Set(currentUserId == null ? [] : [currentUserId]);
}

function mergeMemberSearchUsers(
	users: ReadonlyArray<ForwardUserCandidate>,
	memberSearchResults: ReadonlyArray<GuildMember>,
): ReadonlyArray<ForwardUserCandidate> {
	if (memberSearchResults.length === 0) return users;
	const known = new Set(users.map((user) => user.id));
	const extra: Array<ForwardUserCandidate> = [];
	for (const member of memberSearchResults) {
		if (known.has(member.user.id)) continue;
		known.add(member.user.id);
		extra.push({
			friendAlias: null,
			globalName: member.user.globalName,
			id: member.user.id,
			nicknames: member.nick == null ? [] : [member.nick],
			username: member.user.username,
		});
	}
	return [...users, ...extra];
}

function filterUsersToCommunity(
	users: ReadonlyArray<ForwardUserCandidate>,
	guildId: string,
): ReadonlyArray<ForwardUserCandidate> {
	return users.filter((user) => {
		if (Relationships.getRelationship(user.id)?.type === RelationshipTypes.FRIEND) return true;
		return GuildMembers.getMember(guildId, user.id) != null;
	});
}

function scoreNavigationCandidates(query: string, candidates: ReadonlyArray<Candidate>, limit: number) {
	const term = createSearchTerm(query.toLocaleLowerCase());
	const ranked: Array<RankedCandidate> = [];
	for (const candidate of candidates) {
		const score = scoreSearchTerm(candidate.title.toLocaleLowerCase(), term);
		if (score > 0) ranked.push({candidate, matchedText: candidate.title, score: SCORE_SCALE * score});
	}
	return ranked.sort((left, right) => right.score - left.score).slice(0, limit);
}

function searchGuildCandidates(
	query: string,
	context: SearchContext,
	limit: number,
	excludedIds: ReadonlySet<string>,
): Array<RankedCandidate> {
	const guildById = new Map(context.sets.guilds.map((candidate) => [candidate.id, candidate]));
	const ranked: Array<RankedCandidate> = [];
	for (const result of searchGuilds(
		query,
		context.sources.guilds,
		context.sources.weights.guilds,
		excludedIds,
		limit,
		true,
	)) {
		const candidate = guildById.get(result.id);
		if (candidate != null) ranked.push({candidate, matchedText: result.matchedText, score: result.score});
	}
	return ranked;
}

function currentGuildBlacklist(): ReadonlySet<string> {
	const guildId = getCurrentGuildId();
	return new Set(guildId == null ? [] : [guildId]);
}

export function generateGeneralResults(search: string, context: SearchContext, i18n: I18n): Array<QuickSwitcherResult> {
	const {confusables, sets, sources} = context;
	const runCategories = (limit: number): Array<Array<RankedCandidate>> => [
		toRanked(
			searchUsers(search, sources.users, sources.weights.users, currentUserBlacklist(), confusables, limit),
			sets,
		),
		toRanked(searchGroupDMs(search, sources.groupDMs, sources.weights.groupDMs, confusables, limit), sets),
		toRanked(searchChannels(search, sources.channels, 'text', sources.weights.textChannel, limit, true), sets),
		searchGuildCandidates(search, context, limit, currentGuildBlacklist()),
		scoreNavigationCandidates(search, sets.virtualGuilds, limit),
		scoreNavigationCandidates(search, sets.settings, limit),
	];
	let categories = runCategories(CATEGORY_LIMIT);
	if (categories.filter((category) => category.length > 0).length === 1) {
		const index = categories.findIndex((category) => category.length > 0);
		categories = categories.map((category, categoryIndex) =>
			categoryIndex === index ? runCategories(MODE_LIMIT)[index] : category,
		);
	}
	const seen = new Set<string>();
	return categories
		.flat()
		.filter((ranked) => {
			const key = `${ranked.candidate.type}|${ranked.candidate.id}`;
			if (seen.has(key)) return false;
			seen.add(key);
			return true;
		})
		.sort(compareRanked)
		.map((ranked) => rankedToResult(ranked, i18n));
}

function listRecentAuthors(sets: CandidateSets): Array<Candidate> {
	const channelId = getCurrentChannelId();
	if (channelId == null) return [];
	const channel = Channels.getChannel(channelId);
	const currentUserId = Users.currentUserId;
	const authors: Array<Candidate> = [];
	const seen = new Set<string>();
	Messages.getCachedMessages(channelId)?.searchFromNewest((message) => {
		const author = message.author;
		if (author.id === currentUserId || author.bot || seen.has(author.id)) return false;
		if (channel?.guildId != null && GuildMembers.getMember(channel.guildId, author.id) == null) return false;
		seen.add(author.id);
		const candidate = buildUserCandidate(author.id, sets);
		if (candidate != null) authors.push(candidate);
		return authors.length >= RECENT_AUTHOR_LIMIT;
	});
	return authors;
}

function listChannels(kind: 'text' | 'voice', context: SearchContext): Array<Candidate> {
	const guildId = getCurrentGuildId();
	const guildOrder = new Map(Guilds.getGuilds().map((guild, index) => [guild.id, index]));
	const pool: Array<ForwardChannelCandidate> = context.sources.channels.filter((channel) => {
		if (!channel.canAccess) return false;
		if (kind === 'voice' && channel.kind !== 'voice') return false;
		if (kind === 'text' && channel.kind === 'voice' && guildId != null) return false;
		if (guildId == null) return true;
		return Channels.getChannel(channel.id)?.guildId === guildId;
	});
	const scoreOf = (channel: ForwardChannelCandidate): number => {
		const base = kind === 'text' && channel.kind === 'voice' ? LISTING_VOICE_IN_TEXT_SCORE : LISTING_TEXT_SCORE;
		return channel.hasFrecency ? Math.min(base + LISTING_FRECENCY_BONUS, base >= 7 ? 10 : 7) : base;
	};
	const channelOf = (id: string): Channel | undefined => Channels.getChannel(id);
	return pool
		.map((channel) => ({channel, model: channelOf(channel.id), score: scoreOf(channel)}))
		.filter((entry): entry is {channel: ForwardChannelCandidate; model: Channel; score: number} => entry.model != null)
		.sort((left, right) => {
			if (left.score !== right.score) return right.score - left.score;
			const leftGuild = guildOrder.get(left.model.guildId ?? '') ?? 0;
			const rightGuild = guildOrder.get(right.model.guildId ?? '') ?? 0;
			if (leftGuild !== rightGuild) return leftGuild - rightGuild;
			return compareChannels(left.model, right.model);
		})
		.slice(0, MODE_LIMIT)
		.map((entry) => context.sets.channelById.get(entry.channel.id))
		.filter((candidate): candidate is NonNullable<typeof candidate> => candidate != null);
}

function searchModeUsers(
	search: string,
	variant: QuickSwitcherModeVariant,
	context: SearchContext,
	memberSearchResults: ReadonlyArray<GuildMember>,
): Array<RankedCandidate> {
	const guildId = variant === 'all_users' ? null : getCurrentGuildId();
	let users = mergeMemberSearchUsers(context.sources.users, memberSearchResults);
	if (guildId != null) users = filterUsersToCommunity(users, guildId);
	return toRanked(
		searchUsers(search, users, context.sources.weights.users, currentUserBlacklist(), context.confusables, MODE_LIMIT),
		context.sets,
	);
}

export function generateQueryModeResults(
	prefixMode: QuickSwitcherQueryMode,
	variant: QuickSwitcherModeVariant,
	search: string,
	context: SearchContext,
	i18n: I18n,
	memberSearchResults: ReadonlyArray<GuildMember>,
): Array<QuickSwitcherResult> {
	const header = getModeHeader(prefixMode, variant, i18n);
	const {sets, sources} = context;
	if (search.length === 0) {
		let listed: Array<Candidate>;
		switch (prefixMode) {
			case QuickSwitcherResultTypes.USER:
				listed = listRecentAuthors(sets);
				break;
			case QuickSwitcherResultTypes.TEXT_CHANNEL:
				listed = listChannels('text', context);
				break;
			case QuickSwitcherResultTypes.VOICE_CHANNEL:
				listed = listChannels('voice', context);
				break;
			default:
				listed = [...sets.guilds, ...sets.virtualGuilds];
				break;
		}
		return [header, ...listed.map((candidate) => candidateToResult(candidate, i18n))];
	}
	let ranked: Array<RankedCandidate>;
	switch (prefixMode) {
		case QuickSwitcherResultTypes.USER:
			ranked = searchModeUsers(search, variant, context, memberSearchResults);
			break;
		case QuickSwitcherResultTypes.TEXT_CHANNEL:
			ranked = toRanked(
				searchChannels(search, sources.channels, 'text', sources.weights.textChannel, MODE_LIMIT, true),
				sets,
			);
			break;
		case QuickSwitcherResultTypes.VOICE_CHANNEL:
			ranked = toRanked(
				searchChannels(search, sources.channels, 'voice', sources.weights.voiceChannel, MODE_LIMIT, true),
				sets,
			);
			break;
		default:
			ranked = [
				...searchGuildCandidates(search, context, MODE_LIMIT, currentGuildBlacklist()),
				...scoreNavigationCandidates(search, sets.virtualGuilds, MODE_LIMIT),
			].sort(compareRanked);
			break;
	}
	return [header, ...ranked.map((entry) => rankedToResult(entry, i18n))];
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

export type {SearchContext as QuickSwitcherSearchContext};
