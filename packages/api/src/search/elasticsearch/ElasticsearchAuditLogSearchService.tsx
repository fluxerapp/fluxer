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

import type {AdminAuditLog} from '@fluxer/api/src/admin/IAdminRepository';
import {convertToSearchableAuditLog} from '@fluxer/api/src/search/auditlog/AuditLogSearchSerializer';
import {ElasticsearchSearchServiceBase} from '@fluxer/api/src/search/elasticsearch/ElasticsearchSearchServiceBase';
import type {IAuditLogSearchService} from '@fluxer/api/src/search/IAuditLogSearchService';
import {
	ElasticsearchAuditLogAdapter,
	type ElasticsearchAuditLogAdapterOptions,
} from '@fluxer/elasticsearch_search/src/adapters/ElasticsearchAuditLogAdapter';
import type {SearchResult as SchemaSearchResult} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {AuditLogSearchFilters, SearchableAuditLog} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';

export interface ElasticsearchAuditLogSearchServiceOptions extends ElasticsearchAuditLogAdapterOptions {}

export class ElasticsearchAuditLogSearchService
	extends ElasticsearchSearchServiceBase<AuditLogSearchFilters, SearchableAuditLog, ElasticsearchAuditLogAdapter>
	implements IAuditLogSearchService
{
	constructor(options: ElasticsearchAuditLogSearchServiceOptions) {
		super(new ElasticsearchAuditLogAdapter({client: options.client}));
	}

	async indexAuditLog(log: AdminAuditLog): Promise<void> {
		await this.indexDocument(convertToSearchableAuditLog(log));
	}

	async indexAuditLogs(logs: Array<AdminAuditLog>): Promise<void> {
		if (logs.length === 0) return;
		await this.indexDocuments(logs.map(convertToSearchableAuditLog));
	}

	async updateAuditLog(log: AdminAuditLog): Promise<void> {
		await this.updateDocument(convertToSearchableAuditLog(log));
	}

	async deleteAuditLog(logId: bigint): Promise<void> {
		await this.deleteDocument(logId.toString());
	}

	async deleteAuditLogs(logIds: Array<bigint>): Promise<void> {
		await this.deleteDocuments(logIds.map((id) => id.toString()));
	}

	searchAuditLogs(
		query: string,
		filters: AuditLogSearchFilters,
		options?: {limit?: number; offset?: number},
	): Promise<SchemaSearchResult<SearchableAuditLog>> {
		return this.search(query, filters, options);
	}
}
