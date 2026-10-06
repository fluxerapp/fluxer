// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IAuditLogSearchService} from '@app/api/search/IAuditLogSearchService';
import type {IGuildMemberSearchService} from '@app/api/search/IGuildMemberSearchService';
import type {IGuildSearchService} from '@app/api/search/IGuildSearchService';
import type {IMessageSearchService} from '@app/api/search/IMessageSearchService';
import type {IReportSearchService} from '@app/api/search/IReportSearchService';
import type {IUserSearchService} from '@app/api/search/IUserSearchService';

export interface ISearchProvider {
	initialize(): Promise<void>;
	shutdown(): Promise<void>;
	getMessageSearchService(): IMessageSearchService | null;
	getGuildSearchService(): IGuildSearchService | null;
	getUserSearchService(): IUserSearchService | null;
	getReportSearchService(): IReportSearchService | null;
	getAuditLogSearchService(): IAuditLogSearchService | null;
	getGuildMemberSearchService(): IGuildMemberSearchService | null;
}
