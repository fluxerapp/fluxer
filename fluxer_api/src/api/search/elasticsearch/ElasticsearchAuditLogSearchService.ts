// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditLog} from '@app/api/admin/IAdminRepository';
import {convertToSearchableAuditLog} from '@app/api/search/auditlog/AuditLogSearchSerializer';
import type {IAuditLogSearchService} from '@app/api/search/IAuditLogSearchService';
import {SearchAdapterServiceBase} from '@app/api/search/SearchAdapterServiceBase';
import type {SearchResult as SchemaSearchResult} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {AuditLogSearchFilters, SearchableAuditLog} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import {
	ElasticsearchAuditLogAdapter,
	type ElasticsearchAuditLogAdapterOptions,
} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchAuditLogAdapter';

interface ElasticsearchAuditLogSearchServiceOptions extends ElasticsearchAuditLogAdapterOptions {}

export class ElasticsearchAuditLogSearchService
	extends SearchAdapterServiceBase<AuditLogSearchFilters, SearchableAuditLog, ElasticsearchAuditLogAdapter>
	implements IAuditLogSearchService
{
	constructor(options: ElasticsearchAuditLogSearchServiceOptions) {
		super(new ElasticsearchAuditLogAdapter({client: options.client, lock: options.lock}));
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
		options?: {
			limit?: number;
			offset?: number;
		},
	): Promise<SchemaSearchResult<SearchableAuditLog>> {
		return this.search(query, filters, options);
	}
}
