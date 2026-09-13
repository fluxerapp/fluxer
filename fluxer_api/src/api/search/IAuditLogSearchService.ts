// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditLog} from '@app/api/admin/IAdminRepository';
import type {
	ISearchAdapter as SchemaISearchAdapter,
	SearchResult as SchemaSearchResult,
} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {AuditLogSearchFilters, SearchableAuditLog} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';

export interface IAuditLogSearchService extends SchemaISearchAdapter<AuditLogSearchFilters, SearchableAuditLog> {
	indexAuditLog(log: AdminAuditLog): Promise<void>;
	indexAuditLogs(logs: Array<AdminAuditLog>): Promise<void>;
	deleteAuditLog(logId: bigint): Promise<void>;
	searchAuditLogs(
		query: string,
		filters: AuditLogSearchFilters,
		options?: {
			limit?: number;
			offset?: number;
		},
	): Promise<SchemaSearchResult<SearchableAuditLog>>;
}
