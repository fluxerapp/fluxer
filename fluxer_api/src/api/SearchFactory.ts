// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {ElasticsearchSearchProvider} from '@app/api/infrastructure/ElasticsearchSearchProvider';
import {MeilisearchSearchProvider} from '@app/api/infrastructure/MeilisearchSearchProvider';
import {NullSearchProvider} from '@app/api/infrastructure/NullSearchProvider';
import {Logger} from '@app/api/Logger';
import {getInjectedSearchProvider} from '@app/api/middleware/ServiceRegistry';
import type {IAuditLogSearchService} from '@app/api/search/IAuditLogSearchService';
import type {IGuildMemberSearchService} from '@app/api/search/IGuildMemberSearchService';
import type {IGuildSearchService} from '@app/api/search/IGuildSearchService';
import type {IMessageSearchService} from '@app/api/search/IMessageSearchService';
import type {IReportSearchService} from '@app/api/search/IReportSearchService';
import type {ISearchProvider} from '@app/api/search/ISearchProvider';
import type {IUserSearchService} from '@app/api/search/IUserSearchService';
import {DEFAULT_SEARCH_CLIENT_TIMEOUT_MS} from '@fluxer/constants/src/Timeouts';
import type {ElasticsearchDistributedLock} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchIndexAdapter';

let searchProvider: ISearchProvider | null = null;

function createElasticsearchSearchProvider(lock?: ElasticsearchDistributedLock): ISearchProvider {
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
			tlsRejectUnauthorized: Config.search.tlsRejectUnauthorized,
		},
		logger: Logger,
		lock,
	});
}

function createMeilisearchSearchProvider(): ISearchProvider {
	Logger.info({url: Config.search.url}, 'Using Meilisearch for search');
	return new MeilisearchSearchProvider({
		config: {
			host: Config.search.url,
			apiKey: Config.search.apiKey || undefined,
			requestTimeoutMs: DEFAULT_SEARCH_CLIENT_TIMEOUT_MS,
		},
		logger: Logger,
	});
}

function createSearchProvider(lock?: ElasticsearchDistributedLock): ISearchProvider {
	if (Config.search.engine === 'meilisearch') {
		return createMeilisearchSearchProvider();
	}
	return createElasticsearchSearchProvider(lock);
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

export async function initializeSearch(lock?: ElasticsearchDistributedLock): Promise<void> {
	if (searchProvider) {
		await shutdownSearch();
	}
	const injectedProvider = getInjectedSearchProvider();
	if (injectedProvider) {
		searchProvider = injectedProvider;
		Logger.info('Using injected search provider (in-process mode)');
		await searchProvider.initialize();
		return;
	}
	searchProvider = createSearchProvider(lock);
	try {
		await searchProvider.initialize();
	} catch (error) {
		Logger.error({error}, 'Search backend initialisation failed');
		try {
			await shutdownSearch();
		} catch (shutdownError) {
			Logger.warn({error: shutdownError}, 'Failed to shut down search provider after initialisation failure');
		}
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
