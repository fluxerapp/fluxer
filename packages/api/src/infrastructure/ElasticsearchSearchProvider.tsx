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

import type {ILogger} from '@fluxer/api/src/ILogger';
import {ElasticsearchAuditLogSearchService} from '@fluxer/api/src/search/elasticsearch/ElasticsearchAuditLogSearchService';
import {ElasticsearchGuildMemberSearchService} from '@fluxer/api/src/search/elasticsearch/ElasticsearchGuildMemberSearchService';
import {ElasticsearchGuildSearchService} from '@fluxer/api/src/search/elasticsearch/ElasticsearchGuildSearchService';
import {ElasticsearchMessageSearchService} from '@fluxer/api/src/search/elasticsearch/ElasticsearchMessageSearchService';
import {ElasticsearchReportSearchService} from '@fluxer/api/src/search/elasticsearch/ElasticsearchReportSearchService';
import {ElasticsearchUserSearchService} from '@fluxer/api/src/search/elasticsearch/ElasticsearchUserSearchService';
import type {IAuditLogSearchService} from '@fluxer/api/src/search/IAuditLogSearchService';
import type {IGuildMemberSearchService} from '@fluxer/api/src/search/IGuildMemberSearchService';
import type {IGuildSearchService} from '@fluxer/api/src/search/IGuildSearchService';
import type {IMessageSearchService} from '@fluxer/api/src/search/IMessageSearchService';
import type {IReportSearchService} from '@fluxer/api/src/search/IReportSearchService';
import type {ISearchProvider} from '@fluxer/api/src/search/ISearchProvider';
import type {IUserSearchService} from '@fluxer/api/src/search/IUserSearchService';
import {
	createElasticsearchClient,
	type ElasticsearchClientConfig,
} from '@fluxer/elasticsearch_search/src/ElasticsearchClient';

export interface ElasticsearchSearchProviderOptions {
	config: ElasticsearchClientConfig;
	logger: ILogger;
}

export class ElasticsearchSearchProvider implements ISearchProvider {
	private readonly logger: ILogger;
	private readonly config: ElasticsearchClientConfig;

	private messageService: ElasticsearchMessageSearchService | null = null;
	private guildService: ElasticsearchGuildSearchService | null = null;
	private userService: ElasticsearchUserSearchService | null = null;
	private reportService: ElasticsearchReportSearchService | null = null;
	private auditLogService: ElasticsearchAuditLogSearchService | null = null;
	private guildMemberService: ElasticsearchGuildMemberSearchService | null = null;

	constructor(options: ElasticsearchSearchProviderOptions) {
		this.logger = options.logger;
		this.config = options.config;
	}

	async initialize(): Promise<void> {
		const client = createElasticsearchClient(this.config);

		this.messageService = new ElasticsearchMessageSearchService({client});
		this.guildService = new ElasticsearchGuildSearchService({client});
		this.userService = new ElasticsearchUserSearchService({client});
		this.reportService = new ElasticsearchReportSearchService({client});
		this.auditLogService = new ElasticsearchAuditLogSearchService({client});
		this.guildMemberService = new ElasticsearchGuildMemberSearchService({client});

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
