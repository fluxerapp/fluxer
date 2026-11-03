// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {SearchProvider} from '@app/api/infrastructure/SearchProvider';
import {Logger} from '@app/api/Logger';
import type {IAuditLogSearchService} from '@app/api/search/IAuditLogSearchService';
import type {IGuildMemberSearchService} from '@app/api/search/IGuildMemberSearchService';
import type {IGuildSearchService} from '@app/api/search/IGuildSearchService';
import type {IMessageSearchService} from '@app/api/search/IMessageSearchService';
import type {IReportSearchService} from '@app/api/search/IReportSearchService';
import type {ISearchProvider} from '@app/api/search/ISearchProvider';
import type {IThreadSearchService} from '@app/api/search/IThreadSearchService';
import type {IUserSearchService} from '@app/api/search/IUserSearchService';
import {type MeilisearchClientConfig, MeilisearchHttpClient} from '@app/api/search/meilisearch/MeilisearchClient';
import {
	MeilisearchAuditLogAdapter,
	MeilisearchGuildAdapter,
	MeilisearchGuildMemberAdapter,
	MeilisearchMessageAdapter,
	MeilisearchReportAdapter,
	MeilisearchThreadAdapter,
	MeilisearchUserAdapter,
} from '@app/api/search/meilisearch/MeilisearchDomainAdapters';
import {
	AuditLogSearchService,
	GuildMemberSearchService,
	GuildSearchService,
	MessageSearchService,
	ReportSearchService,
	UserSearchService,
} from '@app/api/search/SearchServices';
import {ThreadSearchIndex} from '@app/api/search/thread/ThreadSearchIndex';
import {DEFAULT_SEARCH_CLIENT_TIMEOUT_MS} from '@fluxer/constants/src/Timeouts';
import type {ElasticsearchDistributedLock} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchIndexAdapter';
import type {ElasticsearchClientConfig} from '@pkgs/elasticsearch_search/src/ElasticsearchClient';

let searchProvider: ISearchProvider | null = null;
let injectedSearchProvider: ISearchProvider | undefined;

function createElasticsearchSearchProvider(lock?: ElasticsearchDistributedLock): ISearchProvider {
	if (!Config.search.apiKey && !Config.search.username) {
		Logger.warn('Elasticsearch credentials are not configured; search will be unavailable');
		return new SearchProvider(null);
	}
	Logger.info({url: Config.search.url}, 'Using Elasticsearch for search');
	const config: ElasticsearchClientConfig = {
		node: Config.search.url,
		auth: Config.search.apiKey
			? {apiKey: Config.search.apiKey}
			: {username: Config.search.username, password: Config.search.password},
		requestTimeoutMs: DEFAULT_SEARCH_CLIENT_TIMEOUT_MS,
		tlsRejectUnauthorized: Config.search.tlsRejectUnauthorized,
	};
	return new SearchProvider(
		async () => {
			const {createElasticsearchSearchServices} = await import(
				'@app/api/search/elasticsearch/ElasticsearchSearchServices'
			);
			return createElasticsearchSearchServices(config, lock);
		},
		() => Logger.info({node: config.node}, 'ElasticsearchSearchProvider initialised'),
	);
}

function createMeilisearchSearchProvider(): ISearchProvider {
	Logger.info({url: Config.search.url}, 'Using Meilisearch for search');
	const config: MeilisearchClientConfig = {
		host: Config.search.url,
		apiKey: Config.search.apiKey || undefined,
		requestTimeoutMs: DEFAULT_SEARCH_CLIENT_TIMEOUT_MS,
	};
	return new SearchProvider(
		async () => {
			const {GuildDiscoveryRepository} = await import('@app/api/guild/repositories/GuildDiscoveryRepository');
			const client = new MeilisearchHttpClient(config);
			const message = new MessageSearchService(new MeilisearchMessageAdapter({client}));
			const guild = new GuildSearchService(new MeilisearchGuildAdapter({client}), new GuildDiscoveryRepository());
			const user = new UserSearchService(new MeilisearchUserAdapter({client}));
			const report = new ReportSearchService(new MeilisearchReportAdapter({client}));
			const auditLog = new AuditLogSearchService(new MeilisearchAuditLogAdapter({client}));
			const guildMember = new GuildMemberSearchService(new MeilisearchGuildMemberAdapter({client}));
			const thread = new ThreadSearchIndex(new MeilisearchThreadAdapter({client}));
			return {message, guild, user, report, auditLog, guildMember, thread};
		},
		() => Logger.info({host: config.host}, 'MeilisearchSearchProvider initialised'),
	);
}

function createSearchProvider(lock?: ElasticsearchDistributedLock): ISearchProvider {
	if (Config.search.engine === 'none') {
		Logger.info('Search is turned off');
		return new SearchProvider(null);
	}
	if (Config.search.engine === 'meilisearch') {
		return createMeilisearchSearchProvider();
	}
	return createElasticsearchSearchProvider(lock);
}

export function setInjectedSearchProvider(provider: ISearchProvider | undefined): void {
	injectedSearchProvider = provider;
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

export function getThreadSearchService(): IThreadSearchService | null {
	return searchProvider?.getThreadSearchService() ?? null;
}

export async function initializeSearch(lock?: ElasticsearchDistributedLock): Promise<void> {
	if (searchProvider) {
		await shutdownSearch();
	}
	const injectedProvider = injectedSearchProvider;
	if (injectedProvider) {
		searchProvider = injectedProvider;
		Logger.info('Using injected search provider (in-process mode)');
		await searchProvider.initialize();
		return;
	}
	const provider = createSearchProvider(lock);
	try {
		await provider.initialize();
	} catch (error) {
		Logger.error({error}, 'Search backend initialisation failed');
		try {
			await provider.shutdown();
		} catch (shutdownError) {
			Logger.warn({error: shutdownError}, 'Failed to shut down search provider after initialisation failure');
		}
		throw error;
	}
	searchProvider = provider;
	Logger.info('Search backend initialized successfully');
}

export async function shutdownSearch(): Promise<void> {
	if (searchProvider) {
		await searchProvider.shutdown();
		searchProvider = null;
	}
}
