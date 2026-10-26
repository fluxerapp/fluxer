// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditLog} from '@app/api/admin/AdminRepository';
import type {ChannelID, GuildID, MessageID, ReportID, UserID} from '@app/api/BrandedTypes';
import type {GuildDiscoveryRepository} from '@app/api/guild/repositories/GuildDiscoveryRepository';
import type {Guild} from '@app/api/models/Guild';
import type {GuildMember} from '@app/api/models/GuildMember';
import type {Message} from '@app/api/models/Message';
import type {User} from '@app/api/models/User';
import type {IARSubmission} from '@app/api/report/ReportModels';
import {convertToSearchableAuditLog} from '@app/api/search/auditlog/AuditLogSearchSerializer';
import {convertToSearchableGuild, type GuildDiscoveryContext} from '@app/api/search/guild/GuildSearchSerializer';
import {resolveDiscoveryContextForIndexing} from '@app/api/search/guild/LazyDiscoveryMigration';
import {convertToSearchableGuildMember} from '@app/api/search/guild_member/GuildMemberSearchSerializer';
import type {IAuditLogSearchService} from '@app/api/search/IAuditLogSearchService';
import type {IGuildMemberSearchService} from '@app/api/search/IGuildMemberSearchService';
import type {IGuildSearchService} from '@app/api/search/IGuildSearchService';
import type {IMessageSearchService} from '@app/api/search/IMessageSearchService';
import type {IReportSearchService} from '@app/api/search/IReportSearchService';
import type {IUserSearchService} from '@app/api/search/IUserSearchService';
import {
	convertMessagesToSearchableMessages,
	convertToSearchableMessage,
} from '@app/api/search/message/MessageSearchSerializer';
import {convertToSearchableReport} from '@app/api/search/report/ReportSearchSerializer';
import {SearchAdapterServiceBase} from '@app/api/search/SearchAdapterServiceBase';
import {convertToSearchableUser} from '@app/api/search/user/UserSearchSerializer';
import {isDeletionComplete} from '@app/api/user/UserHelpers';
import type {
	ISearchAdapter,
	SearchOptions as SchemaSearchOptions,
	SearchResult as SchemaSearchResult,
} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {
	AuditLogSearchFilters,
	GuildMemberSearchFilters,
	GuildSearchFilters,
	MessageSearchFilters,
	ReportSearchFilters,
	SearchableAuditLog,
	SearchableGuild,
	SearchableGuildMember,
	SearchableMessage,
	SearchableReport,
	SearchableUser,
	UserSearchFilters,
} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';

type TermSearchAdapter<TFilters, TDocument> = ISearchAdapter<TFilters, TDocument> & {
	deleteByTerm(field: string, value: string): Promise<void>;
};

const DEFAULT_HITS_PER_PAGE = 25;
const DEFAULT_MEMBER_LIMIT = 25;

function toMessageSearchOptions(options?: {hitsPerPage?: number; page?: number}): {
	limit?: number;
	offset?: number;
} {
	return {
		limit: options?.hitsPerPage,
		offset: options?.page ? (options.page - 1) * (options.hitsPerPage ?? DEFAULT_HITS_PER_PAGE) : 0,
	};
}

function toMemberSearchOptions(options?: {limit?: number; offset?: number}): {
	limit?: number;
	offset?: number;
} {
	return {
		limit: options?.limit ?? DEFAULT_MEMBER_LIMIT,
		offset: options?.offset ?? 0,
	};
}

export class MessageSearchService
	extends SearchAdapterServiceBase<
		MessageSearchFilters,
		SearchableMessage,
		TermSearchAdapter<MessageSearchFilters, SearchableMessage>
	>
	implements IMessageSearchService
{
	async indexMessage(message: Message, authorIsBot?: boolean): Promise<void> {
		await this.indexDocument(convertToSearchableMessage(message, authorIsBot));
	}

	async indexMessages(messages: Array<Message>, authorBotMap?: Map<UserID, boolean>): Promise<void> {
		if (messages.length === 0) return;
		await this.indexDocuments(convertMessagesToSearchableMessages(messages, authorBotMap));
	}

	async bulkIndexMessages(messages: Array<Message>, authorBotMap?: Map<UserID, boolean>): Promise<void> {
		if (messages.length === 0) return;
		await this.bulkIndexDocuments(convertMessagesToSearchableMessages(messages, authorBotMap));
	}

	async updateMessage(message: Message, authorIsBot?: boolean): Promise<void> {
		await this.updateDocument(convertToSearchableMessage(message, authorIsBot));
	}

	async deleteMessages(messageIds: Array<MessageID>): Promise<void> {
		await this.deleteDocuments(messageIds.map((id) => id.toString()));
	}

	async deleteChannelMessages(channelId: ChannelID): Promise<void> {
		await this.adapter.deleteByTerm('channelId', channelId.toString());
	}

	searchMessages(
		query: string,
		filters: MessageSearchFilters,
		options?: {
			hitsPerPage?: number;
			page?: number;
			cursor?: Array<string>;
		},
	): Promise<SchemaSearchResult<SearchableMessage>> {
		return this.search(query, filters, toMessageSearchOptions(options));
	}
}

export class GuildSearchService
	extends SearchAdapterServiceBase<
		GuildSearchFilters,
		SearchableGuild,
		ISearchAdapter<GuildSearchFilters, SearchableGuild>
	>
	implements IGuildSearchService
{
	private readonly discoveryRepository: GuildDiscoveryRepository | undefined;

	constructor(
		adapter: ISearchAdapter<GuildSearchFilters, SearchableGuild>,
		discoveryRepository?: GuildDiscoveryRepository,
	) {
		super(adapter);
		this.discoveryRepository = discoveryRepository;
	}

	async indexGuild(guild: Guild, discovery?: GuildDiscoveryContext): Promise<void> {
		const context = await resolveDiscoveryContextForIndexing(guild, discovery, this.discoveryRepository);
		await this.indexDocument(convertToSearchableGuild(guild, context));
	}

	async indexGuilds(guilds: Array<Guild>): Promise<void> {
		if (guilds.length === 0) return;
		const docs = await Promise.all(
			guilds.map(async (guild) =>
				convertToSearchableGuild(
					guild,
					await resolveDiscoveryContextForIndexing(guild, undefined, this.discoveryRepository),
				),
			),
		);
		await this.indexDocuments(docs);
	}

	async updateGuild(guild: Guild, discovery?: GuildDiscoveryContext): Promise<void> {
		const context = await resolveDiscoveryContextForIndexing(guild, discovery, this.discoveryRepository);
		await this.updateDocument(convertToSearchableGuild(guild, context));
	}

	async deleteGuild(guildId: GuildID): Promise<void> {
		await this.deleteDocument(guildId.toString());
	}

	searchGuilds(
		query: string,
		filters: GuildSearchFilters,
		options?: SchemaSearchOptions,
	): Promise<SchemaSearchResult<SearchableGuild>> {
		return this.search(query, filters, options);
	}
}

export class UserSearchService
	extends SearchAdapterServiceBase<UserSearchFilters, SearchableUser, ISearchAdapter<UserSearchFilters, SearchableUser>>
	implements IUserSearchService
{
	async indexUser(user: User): Promise<void> {
		if (isDeletionComplete(user)) {
			await this.deleteDocument(user.id.toString());
			return;
		}
		await this.indexDocument(convertToSearchableUser(user));
	}

	async indexUsers(users: Array<User>): Promise<void> {
		const listed = users.filter((user) => !isDeletionComplete(user));
		if (listed.length === 0) return;
		await this.indexDocuments(listed.map(convertToSearchableUser));
	}

	async updateUser(user: User): Promise<void> {
		if (isDeletionComplete(user)) {
			await this.deleteDocument(user.id.toString());
			return;
		}
		await this.updateDocument(convertToSearchableUser(user));
	}

	searchUsers(
		query: string,
		filters: UserSearchFilters,
		options?: {
			limit?: number;
			offset?: number;
		},
	): Promise<SchemaSearchResult<SearchableUser>> {
		return this.search(query, filters, options);
	}
}

export class ReportSearchService
	extends SearchAdapterServiceBase<
		ReportSearchFilters,
		SearchableReport,
		ISearchAdapter<ReportSearchFilters, SearchableReport>
	>
	implements IReportSearchService
{
	async indexReport(report: IARSubmission): Promise<void> {
		await this.indexDocument(convertToSearchableReport(report));
	}

	async indexReports(reports: Array<IARSubmission>): Promise<void> {
		if (reports.length === 0) return;
		await this.indexDocuments(reports.map(convertToSearchableReport));
	}

	async updateReport(report: IARSubmission): Promise<void> {
		await this.updateDocument(convertToSearchableReport(report));
	}

	async deleteReport(reportId: ReportID): Promise<void> {
		await this.deleteDocument(reportId.toString());
	}

	searchReports(
		query: string,
		filters: ReportSearchFilters,
		options?: {
			limit?: number;
			offset?: number;
		},
	): Promise<SchemaSearchResult<SearchableReport>> {
		return this.search(query, filters, options);
	}

	listReportsByStatus(status: number, limit?: number, offset?: number): Promise<SchemaSearchResult<SearchableReport>> {
		return this.searchReports('', {status}, {limit, offset});
	}
}

export class AuditLogSearchService
	extends SearchAdapterServiceBase<
		AuditLogSearchFilters,
		SearchableAuditLog,
		ISearchAdapter<AuditLogSearchFilters, SearchableAuditLog>
	>
	implements IAuditLogSearchService
{
	async indexAuditLog(log: AdminAuditLog): Promise<void> {
		await this.indexDocument(convertToSearchableAuditLog(log));
	}

	async indexAuditLogs(logs: Array<AdminAuditLog>): Promise<void> {
		if (logs.length === 0) return;
		await this.indexDocuments(logs.map(convertToSearchableAuditLog));
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

export class GuildMemberSearchService
	extends SearchAdapterServiceBase<
		GuildMemberSearchFilters,
		SearchableGuildMember,
		TermSearchAdapter<GuildMemberSearchFilters, SearchableGuildMember>
	>
	implements IGuildMemberSearchService
{
	async indexMember(member: GuildMember, user: User): Promise<void> {
		await this.indexDocument(convertToSearchableGuildMember(member, user));
	}

	async indexMembers(
		members: Array<{
			member: GuildMember;
			user: User;
		}>,
	): Promise<void> {
		if (members.length === 0) return;
		await this.indexDocuments(members.map(({member, user}) => convertToSearchableGuildMember(member, user)));
	}

	async updateMember(member: GuildMember, user: User): Promise<void> {
		await this.updateDocument(convertToSearchableGuildMember(member, user));
	}

	async deleteMember(guildId: GuildID, userId: UserID): Promise<void> {
		await this.deleteDocument(`${guildId}_${userId}`);
	}

	async deleteGuildMembers(guildId: GuildID): Promise<void> {
		await this.adapter.deleteByTerm('guildId', guildId.toString());
	}

	searchMembers(
		query: string,
		filters: GuildMemberSearchFilters,
		options?: {
			limit?: number;
			offset?: number;
		},
	): Promise<SchemaSearchResult<SearchableGuildMember>> {
		return this.search(query, filters, toMemberSearchOptions(options));
	}
}
