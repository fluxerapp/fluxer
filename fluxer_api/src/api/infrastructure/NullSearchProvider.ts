// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IAuditLogSearchService} from '@app/api/search/IAuditLogSearchService';
import type {IGuildMemberSearchService} from '@app/api/search/IGuildMemberSearchService';
import type {IGuildSearchService} from '@app/api/search/IGuildSearchService';
import type {IMessageSearchService} from '@app/api/search/IMessageSearchService';
import type {IReportSearchService} from '@app/api/search/IReportSearchService';
import type {ISearchProvider} from '@app/api/search/ISearchProvider';
import type {IUserSearchService} from '@app/api/search/IUserSearchService';

export class NullSearchProvider implements ISearchProvider {
	async initialize(): Promise<void> {}

	async shutdown(): Promise<void> {}

	getMessageSearchService(): IMessageSearchService | null {
		return null;
	}

	getGuildSearchService(): IGuildSearchService | null {
		return null;
	}

	getUserSearchService(): IUserSearchService | null {
		return null;
	}

	getReportSearchService(): IReportSearchService | null {
		return null;
	}

	getAuditLogSearchService(): IAuditLogSearchService | null {
		return null;
	}

	getGuildMemberSearchService(): IGuildMemberSearchService | null {
		return null;
	}
}
