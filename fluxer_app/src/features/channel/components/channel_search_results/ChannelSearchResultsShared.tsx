// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageBehaviorOverrides} from '@app/features/channel/components/ChannelMessage';
import type {SearchMachineState} from '@app/features/channel/components/SearchResultsUtils';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import type {Guild} from '@app/features/guild/models/Guild';
import GuildMatureContentAgree from '@app/features/guild/state/GuildMatureContentAgree';
import Guilds from '@app/features/guild/state/Guilds';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import type {MessageSearchParams, MessageSearchScope} from '@app/features/search/utils/SearchUtils';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import type {IconProps} from '@phosphor-icons/react';
import {
	ChatCenteredDotsIcon,
	ChatCircleIcon,
	ClockClockwiseIcon,
	ClockCounterClockwiseIcon,
	GlobeIcon,
	HashIcon,
	SparkleIcon,
	UsersIcon,
} from '@phosphor-icons/react';
import type React from 'react';

export type PaginationItem = number | 'ellipsis-left' | 'ellipsis-right';
export type ChannelSearchSortMode = 'newest' | 'oldest' | 'relevant';

export const DEFAULT_MAX_VISIBLE_PAGES = 7;
export const RESULTS_PER_PAGE = 25;
export const DETACHED_MESSAGE_BEHAVIOR: MessageBehaviorOverrides = {
	isEditing: false,
	isReplying: false,
	isHighlight: false,
	contextMenuOpen: false,
	disableContextMenuTracking: true,
} as const;
export const EMPTY_SEARCH_MESSAGES: Array<Message> = [];
export const EMPTY_SEARCH_CHANNELS: Array<Channel> = [];
export const OTHER_DESCRIPTOR = msg({
	message: '{total, plural, one {# result} other {# results}}',
	comment: 'Result count header in the channel search results panel. Plural cases describe the result count.',
});
export const INDEXING_DESCRIPTOR = msg({
	message: 'Indexing',
	comment: 'Header status shown while the channel is being indexed for the first time before search is available.',
});
export const SEARCHING_DESCRIPTOR = msg({
	message: 'Searching',
	comment: 'Header status shown while a search request is in flight in the channel search results panel.',
});
export const SEARCH_RESULTS_DESCRIPTOR = msg({
	message: 'Search results',
	comment: 'Default header title in the channel search results panel before a query has been submitted.',
});
export const AN_ERROR_OCCURRED_WHILE_SEARCHING_DESCRIPTOR = msg({
	message: 'An error occurred while searching.',
	comment: 'Fallback error message shown when the channel search request fails for an unknown reason.',
});
export const INDEXING_CHANNEL_DESCRIPTOR = msg({
	message: 'Indexing channel',
	comment: 'Heading of the indexing placeholder shown while the channel index is being built for the first time.',
});
export const WE_RE_INDEXING_THIS_CHANNEL_FOR_THE_FIRST_DESCRIPTOR = msg({
	message:
		"We're indexing this channel for the first time. This may take a moment. Results will appear here when ready.",
	comment: 'Body copy under the indexing heading explaining why search results are not yet available.',
});
export const ERROR_DESCRIPTOR = msg({
	message: 'Error',
	comment: 'Heading shown above the error message in the channel search results error state.',
});
export const NO_RESULTS_DESCRIPTOR = msg({
	message: 'No results',
	comment: 'Heading shown when the channel search query returned zero matches.',
});
export const TRY_A_DIFFERENT_SEARCH_QUERY_DESCRIPTOR = msg({
	message: 'Try a different search query.',
	comment: 'Body copy under the no-results heading suggesting the user adjust their query.',
});
export const NOTHING_TO_SEARCH_FOR_DESCRIPTOR = msg({
	message: 'Nothing to search for',
	comment: 'Heading shown when the submitted search query produced no filter and no text the server could search on.',
});
export const NO_PART_OF_THIS_QUERY_APPLIED_DESCRIPTOR = msg({
	message: 'No part of {query} could be applied. Fix the underlined value or add something to search for.',
	comment:
		'Body copy under the nothing-to-search-for heading. Preserve {query}; it is the text the user submitted and is inserted by code.',
});
export const GO_TO_PAGE_DESCRIPTOR = msg({
	message: 'Go to page {page}',
	comment: 'Accessible label of each pagination page button. page is the 1-indexed page number.',
});
export const GO_TO_PAGE_2_DESCRIPTOR = msg({
	message: 'Go to page',
	comment: 'Accessible label of the inline page-number input shown when the user opens the pagination ellipsis.',
});
export const JUMP_TO_PAGE_DESCRIPTOR = msg({
	message: 'Jump to page',
	comment: 'Accessible label of the ellipsis button in the pagination bar that reveals the page-number input.',
});
export const SEARCH_SCOPE_DESCRIPTOR = msg({
	message: 'Search scope: {label}',
	comment:
		'Status label in the channel search results header announcing the current scope. label is the selected scope name.',
});
export const SORT_MODE_DESCRIPTOR = msg({
	message: 'Sort mode: {label}',
	comment:
		'Status label in the channel search results header announcing the current sort mode. label is the selected sort.',
});
export const NEWEST_DESCRIPTOR = msg({message: 'Newest'});
export const OLDEST_DESCRIPTOR = msg({message: 'Oldest'});
export const MOST_RELEVANT_DESCRIPTOR = msg({message: 'Most relevant'});
export const getChannelGuild = (channel: Channel): Guild | null => {
	if (!channel.guildId) return null;
	return Guilds.getGuild(channel.guildId) ?? null;
};
export const getAdaptiveVisiblePageCount = (): number => {
	if (window.innerWidth <= 420) return 3;
	if (window.innerWidth <= 640) return 5;
	return DEFAULT_MAX_VISIBLE_PAGES;
};
export const buildPaginationRange = (
	currentPage: number,
	totalPages: number,
	maxVisible: number,
): Array<PaginationItem> => {
	if (totalPages <= 0) return [];
	const effectiveMax = Math.max(3, maxVisible);
	if (totalPages <= effectiveMax) {
		return Array.from({length: totalPages}, (_, index) => index + 1);
	}
	const innerSlots = Math.max(1, effectiveMax - 2);
	let start = currentPage - Math.floor(innerSlots / 2);
	let end = currentPage + Math.ceil(innerSlots / 2) - 1;
	start = Math.max(2, start);
	end = Math.min(totalPages - 1, end);
	while (end - start + 1 < innerSlots) {
		if (start > 2) {
			start -= 1;
		} else if (end < totalPages - 1) {
			end += 1;
		} else {
			break;
		}
	}
	const range: Array<PaginationItem> = [1];
	if (start > 2) range.push('ellipsis-left');
	for (let page = start; page <= end; page++) {
		range.push(page);
	}
	if (end < totalPages - 1) range.push('ellipsis-right');
	range.push(totalPages);
	return range;
};
export const getSortModeOptions = (i18n: I18n): Array<{mode: ChannelSearchSortMode; label: string}> => [
	{mode: 'newest', label: i18n._(NEWEST_DESCRIPTOR)},
	{mode: 'oldest', label: i18n._(OLDEST_DESCRIPTOR)},
	{mode: 'relevant', label: i18n._(MOST_RELEVANT_DESCRIPTOR)},
];
const SCOPE_ICON_COMPONENTS: Record<MessageSearchScope, React.ComponentType<IconProps>> = {
	current: HashIcon,
	all_dms: ChatCircleIcon,
	open_dms: ChatCenteredDotsIcon,
	all_guilds: GlobeIcon,
	all: UsersIcon,
	open_dms_and_all_guilds: UsersIcon,
};
const SORT_ICON_COMPONENTS: Record<ChannelSearchSortMode, React.ComponentType<IconProps>> = {
	newest: ClockClockwiseIcon,
	oldest: ClockCounterClockwiseIcon,
	relevant: SparkleIcon,
};
export const renderScopeIcon = (scope: MessageSearchScope, size = 16): React.ReactNode => {
	const IconComponent = SCOPE_ICON_COMPONENTS[scope] ?? HashIcon;
	return (
		<IconComponent
			size={size}
			weight="bold"
			data-flx="channel.channel-search-results.render-scope-icon.icon-component"
		/>
	);
};
export const renderSortIcon = (mode: ChannelSearchSortMode, size = 16): React.ReactNode => {
	const IconComponent = SORT_ICON_COMPONENTS[mode];
	return (
		<IconComponent
			size={size}
			weight="bold"
			data-flx="channel.channel-search-results.render-sort-icon.icon-component"
		/>
	);
};
export const applySortModeToParams = (params: MessageSearchParams, mode: ChannelSearchSortMode): void => {
	switch (mode) {
		case 'newest':
			params.sortBy = 'timestamp';
			params.sortOrder = 'desc';
			break;
		case 'oldest':
			params.sortBy = 'timestamp';
			params.sortOrder = 'asc';
			break;
		case 'relevant':
			params.sortBy = 'relevance';
			params.sortOrder = 'desc';
			break;
	}
};
const shouldShowGuildMetaForScope = (guild: Guild | null, scope: MessageSearchScope): boolean => {
	if (!guild) return false;
	switch (scope) {
		case 'all_guilds':
		case 'all':
		case 'open_dms_and_all_guilds':
			return true;
		default:
			return false;
	}
};
export const getSearchResultChannelRenderData = (
	resultChannelId: string,
	searchChannelsById: Map<string, Channel>,
	scope: MessageSearchScope,
): {messageChannel: Channel; showGuildMeta: boolean} | null => {
	const messageChannel = searchChannelsById.get(resultChannelId) ?? Channels.getChannel(resultChannelId);
	if (!messageChannel) return null;
	const channelGuild = getChannelGuild(messageChannel);
	return {messageChannel, showGuildMeta: shouldShowGuildMetaForScope(channelGuild, scope)};
};
const collectSearchMatureContentChannels = (params: MessageSearchParams, contextChannelId: string): Array<string> => {
	const matureContentChannels: Array<string> = [];
	const contextChannel = Channels.getChannel(contextChannelId);
	if (
		contextChannel &&
		GuildMatureContentAgree.isGatedContent({channelId: contextChannelId, guildId: contextChannel.guildId ?? null}) &&
		!GuildMatureContentAgree.shouldShowGate({channelId: contextChannelId, guildId: contextChannel.guildId ?? null})
	) {
		matureContentChannels.push(contextChannelId);
	}
	if (params.channelId) {
		for (const channelIdParam of params.channelId) {
			const targetChannel = Channels.getChannel(channelIdParam);
			if (
				targetChannel &&
				GuildMatureContentAgree.isGatedContent({channelId: channelIdParam, guildId: targetChannel.guildId ?? null}) &&
				!GuildMatureContentAgree.shouldShowGate({channelId: channelIdParam, guildId: targetChannel.guildId ?? null})
			) {
				matureContentChannels.push(channelIdParam);
			}
		}
	}
	return matureContentChannels;
};
export const applyMatureContentToParamsIfNeeded = (params: MessageSearchParams, contextChannelId: string): void => {
	const matureContentChannels = collectSearchMatureContentChannels(params, contextChannelId);
	if (matureContentChannels.length > 0) {
		params.includeNsfw = true;
	}
};
export const getHeaderTitleDescriptor = (machineState: SearchMachineState): MessageDescriptor => {
	switch (machineState.status) {
		case 'success':
			return {...OTHER_DESCRIPTOR, values: {total: machineState.total}};
		case 'indexing':
			return INDEXING_DESCRIPTOR;
		case 'loading':
			return SEARCHING_DESCRIPTOR;
		default:
			return SEARCH_RESULTS_DESCRIPTOR;
	}
};
