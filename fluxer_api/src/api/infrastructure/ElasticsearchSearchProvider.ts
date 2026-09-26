// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ILogger} from '@app/api/ILogger';
import {ElasticsearchAuditLogSearchService} from '@app/api/search/elasticsearch/ElasticsearchAuditLogSearchService';
import {ElasticsearchGuildMemberSearchService} from '@app/api/search/elasticsearch/ElasticsearchGuildMemberSearchService';
import {ElasticsearchGuildSearchService} from '@app/api/search/elasticsearch/ElasticsearchGuildSearchService';
import {ElasticsearchMessageSearchService} from '@app/api/search/elasticsearch/ElasticsearchMessageSearchService';
import {ElasticsearchReportSearchService} from '@app/api/search/elasticsearch/ElasticsearchReportSearchService';
import {ElasticsearchUserSearchService} from '@app/api/search/elasticsearch/ElasticsearchUserSearchService';
import type {IAuditLogSearchService} from '@app/api/search/IAuditLogSearchService';
import type {IGuildMemberSearchService} from '@app/api/search/IGuildMemberSearchService';
import type {IGuildSearchService} from '@app/api/search/IGuildSearchService';
import type {IMessageSearchService} from '@app/api/search/IMessageSearchService';
import type {IReportSearchService} from '@app/api/search/IReportSearchService';
import type {ISearchProvider} from '@app/api/search/ISearchProvider';
import type {IUserSearchService} from '@app/api/search/IUserSearchService';
import type {ElasticsearchDistributedLock} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchIndexAdapter';
import {
	createElasticsearchClient,
	type ElasticsearchClientConfig,
} from '@pkgs/elasticsearch_search/src/ElasticsearchClient';

interface ElasticsearchSearchProviderOptions {
	config: ElasticsearchClientConfig;
	logger: ILogger;
	lock?: ElasticsearchDistributedLock;
}

export class ElasticsearchSearchProvider implements ISearchProvider {
	private readonly logger: ILogger;
	private readonly config: ElasticsearchClientConfig;
	private readonly lock: ElasticsearchDistributedLock | undefined;
	private messageService: ElasticsearchMessageSearchService | null = null;
	private guildService: ElasticsearchGuildSearchService | null = null;
	private userService: ElasticsearchUserSearchService | null = null;
	private reportService: ElasticsearchReportSearchService | null = null;
	private auditLogService: ElasticsearchAuditLogSearchService | null = null;
	private guildMemberService: ElasticsearchGuildMemberSearchService | null = null;

	constructor(options: ElasticsearchSearchProviderOptions) {
		this.logger = options.logger;
		this.config = options.config;
		this.lock = options.lock;
	}

	async initialize(): Promise<void> {
		const client = createElasticsearchClient(this.config);
		const lock = this.lock;
		this.messageService = new ElasticsearchMessageSearchService({client, lock});
		const {GuildDiscoveryRepository} = await import('@app/api/guild/repositories/GuildDiscoveryRepository');
		this.guildService = new ElasticsearchGuildSearchService({
			client,
			lock,
			discoveryRepository: new GuildDiscoveryRepository(),
		});
		this.userService = new ElasticsearchUserSearchService({client, lock});
		this.reportService = new ElasticsearchReportSearchService({client, lock});
		this.auditLogService = new ElasticsearchAuditLogSearchService({client, lock});
		this.guildMemberService = new ElasticsearchGuildMemberSearchService({client, lock});
		await Promise.all([
			this.messageService.initialize(),
			this.guildService.initialize(),
			this.userService.initialize(),
			this.reportService.initialize(),
			this.auditLogService.initialize(),
			this.guildMemberService.initialize(),
		]);
		this.logger.info({node: this.config.node}, 'ElasticsearchSearchProvider initialised');
	}

	async shutdown(): Promise<void> {
		const services = [
			this.messageService,
			this.guildService,
			this.userService,
			this.reportService,
			this.auditLogService,
			this.guildMemberService,
		];
		await Promise.all(services.filter((s) => s != null).map((s) => s.shutdown()));
		this.messageService = null;
		this.guildService = null;
		this.userService = null;
		this.reportService = null;
		this.auditLogService = null;
		this.guildMemberService = null;
	}

	getMessageSearchService(): IMessageSearchService | null {
		return this.messageService;
	}

	getGuildSearchService(): IGuildSearchService | null {
		return this.guildService;
	}

	getUserSearchService(): IUserSearchService | null {
		return this.userService;
	}

	getReportSearchService(): IReportSearchService | null {
		return this.reportService;
	}

	getAuditLogSearchService(): IAuditLogSearchService | null {
		return this.auditLogService;
	}

	getGuildMemberSearchService(): IGuildMemberSearchService | null {
		return this.guildMemberService;
	}
}
