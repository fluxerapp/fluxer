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

import {Config} from '@fluxer/api/src/Config';
import {ElasticsearchSearchProvider} from '@fluxer/api/src/infrastructure/ElasticsearchSearchProvider';
import {MeilisearchSearchProvider} from '@fluxer/api/src/infrastructure/MeilisearchSearchProvider';
import {NullSearchProvider} from '@fluxer/api/src/infrastructure/NullSearchProvider';
import {Logger} from '@fluxer/api/src/Logger';
import {getInjectedSearchProvider} from '@fluxer/api/src/middleware/ServiceRegistry';
import type {IAuditLogSearchService} from '@fluxer/api/src/search/IAuditLogSearchService';
import type {IGuildMemberSearchService} from '@fluxer/api/src/search/IGuildMemberSearchService';
import type {IGuildSearchService} from '@fluxer/api/src/search/IGuildSearchService';
import type {IMessageSearchService} from '@fluxer/api/src/search/IMessageSearchService';
import type {IReportSearchService} from '@fluxer/api/src/search/IReportSearchService';
import type {ISearchProvider} from '@fluxer/api/src/search/ISearchProvider';
import type {IUserSearchService} from '@fluxer/api/src/search/IUserSearchService';
import {DEFAULT_SEARCH_CLIENT_TIMEOUT_MS} from '@fluxer/constants/src/Timeouts';

let searchProvider: ISearchProvider | null = null;

export function createSearchProvider(): ISearchProvider {
	const engine = Config.search.engine ?? 'meilisearch';

	if (engine === 'elasticsearch') {
		if (!Config.search.apiKey && !Config.search.username) {
			Logger.warn('Elasticsearch credentials are not configured; search will be unavailable');
			return new NullSearchProvider();
		}

		Logger.info({url: Config.search.url}, 'Using Elasticsearch for search');
		return new ElasticsearchSearchProvider({
			config: {
				node: Config.search.url,
				auth: Config.search.apiKey
					? {apiKey: Config.search.apiKey}
					: Config.search.username
						? {username: Config.search.username, password: Config.search.password}
						: undefined,
				requestTimeoutMs: DEFAULT_SEARCH_CLIENT_TIMEOUT_MS,
			},
			logger: Logger,
		});
	}

	if (!Config.search.apiKey) {
		Logger.warn('Search API key is not configured; search will be unavailable');
		return new NullSearchProvider();
	}

	Logger.info({url: Config.search.url}, 'Using Meilisearch for search');
	return new MeilisearchSearchProvider({
		config: {
			url: Config.search.url,
			apiKey: Config.search.apiKey,
			timeoutMs: DEFAULT_SEARCH_CLIENT_TIMEOUT_MS,
			taskWaitTimeoutMs: DEFAULT_SEARCH_CLIENT_TIMEOUT_MS,
			taskPollIntervalMs: 50,
		},
		logger: Logger,
	});
}

export function getSearchProvider(): ISearchProvider | null {
	return searchProvider;
}

export function setInjectedSearchProvider(provider: ISearchProvider | undefined): void {
	searchProvider = provider ?? null;
}

export function getMessageSearchService(): IMessageSearchService | null {
	return searchProvider?.getMessageSearchService() ?? null;
}

export function getGuildSearchService(): IGuildSearchService | null {
	return searchProvider?.getGuildSearchService() ?? null;
}

export function getUserSearchService(): IUserSearchService | null {
	return searchProvider?.getUserSearchService() ?? null;
}

export function getReportSearchService(): IReportSearchService | null {
	return searchProvider?.getReportSearchService() ?? null;
}

export function getAuditLogSearchService(): IAuditLogSearchService | null {
	return searchProvider?.getAuditLogSearchService() ?? null;
}

export function getGuildMemberSearchService(): IGuildMemberSearchService | null {
	return searchProvider?.getGuildMemberSearchService() ?? null;
}

export async function initializeSearch(): Promise<void> {
	if (searchProvider) {
		await searchProvider.shutdown();
	}

	const injectedProvider = getInjectedSearchProvider();
	if (injectedProvider) {
		searchProvider = injectedProvider;
		Logger.info('Using injected search provider (in-process mode)');
	} else {
		searchProvider = createSearchProvider();
	}

	try {
		await searchProvider.initialize();
	} catch (error) {
		Logger.error({error}, 'Search backend initialisation failed');
		try {
			await searchProvider.shutdown();
		} catch (shutdownError) {
			Logger.warn({error: shutdownError}, 'Failed to shut down search provider after initialisation failure');
		}
		searchProvider = null;
		throw error;
	}

	Logger.info('Search backend initialized successfully');
}

export async function shutdownSearch(): Promise<void> {
	if (searchProvider) {
		await searchProvider.shutdown();
		searchProvider = null;
	}
}

export function resetSearchServices(): void {
	searchProvider = null;
}
