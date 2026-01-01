/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Endpoints} from '~/Endpoints';
import http from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import type {Message} from '~/records/MessageRecord';
import type {MentionFilters} from '~/stores/RecentMentionsStore';
import RecentMentionsStore from '~/stores/RecentMentionsStore';

const logger = new Logger('Mentions');

export const fetch = async (): Promise<Array<Message>> => {
	RecentMentionsStore.handleFetchPending();
	try {
		const filters = RecentMentionsStore.getFilters();
		logger.debug('Fetching recent mentions');
		const response = await http.get<Array<Message>>({
			url: Endpoints.USER_MENTIONS,
			query: {
				everyone: filters.includeEveryone,
				roles: filters.includeRoles,
				guilds: filters.includeGuilds,
				limit: 25,
			},
		});
		const data = response.body ?? [];
		RecentMentionsStore.handleRecentMentionsFetchSuccess(data);
		logger.debug(`Successfully fetched ${data.length} recent mentions`);
		return data;
	} catch (error) {
		RecentMentionsStore.handleRecentMentionsFetchError();
		logger.error('Failed to fetch recent mentions:', error);
		throw error;
	}
};

export const loadMore = async (): Promise<Array<Message>> => {
	const recentMentions = RecentMentionsStore.recentMentions;
	if (recentMentions.length === 0) {
		return [];
	}

	const lastMessage = recentMentions[recentMentions.length - 1];
	const filters = RecentMentionsStore.getFilters();

	RecentMentionsStore.handleFetchPending();
	try {
		logger.debug(`Loading more mentions before ${lastMessage.id}`);
		const response = await http.get<Array<Message>>({
			url: Endpoints.USER_MENTIONS,
			query: {
				everyone: filters.includeEveryone,
				roles: filters.includeRoles,
				guilds: filters.includeGuilds,
				limit: 25,
				before: lastMessage.id,
			},
		});
		const data = response.body ?? [];
		RecentMentionsStore.handleRecentMentionsFetchSuccess(data);
		logger.debug(`Successfully loaded ${data.length} more mentions`);
		return data;
	} catch (error) {
		RecentMentionsStore.handleRecentMentionsFetchError();
		logger.error('Failed to load more mentions:', error);
		throw error;
	}
};

export const updateFilters = (filters: Partial<MentionFilters>): void => {
	RecentMentionsStore.updateFilters(filters);
};

export const remove = async (messageId: string): Promise<void> => {
	try {
		RecentMentionsStore.handleMessageDelete(messageId);
		logger.debug(`Removing message ${messageId} from recent mentions`);
		await http.delete({url: Endpoints.USER_MENTION(messageId)});
		logger.debug(`Successfully removed message ${messageId} from recent mentions`);
	} catch (error) {
		logger.error(`Failed to remove message ${messageId} from recent mentions:`, error);
		throw error;
	}
};
