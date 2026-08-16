// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
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
import type {AdminAuditLog} from '../../admin/IAdminRepository';
import type {ChannelID, GuildID, MessageID, ReportID, UserID} from '../../BrandedTypes';
import type {IGuildDiscoveryRepository} from '../../guild/repositories/GuildDiscoveryRepository';
import type {Guild} from '../../models/Guild';
import type {GuildMember} from '../../models/GuildMember';
import type {Message} from '../../models/Message';
import type {User} from '../../models/User';
import type {IARSubmission} from '../../report/IReportRepository';
import {convertToSearchableAuditLog} from '../auditlog/AuditLogSearchSerializer';
import {convertToSearchableGuild, type GuildDiscoveryContext} from '../guild/GuildSearchSerializer';
import {resolveDiscoveryContextForIndexing} from '../guild/LazyDiscoveryMigration';
import {convertToSearchableGuildMember} from '../guild_member/GuildMemberSearchSerializer';
import type {IAuditLogSearchService} from '../IAuditLogSearchService';
import type {IGuildMemberSearchService} from '../IGuildMemberSearchService';
import type {IGuildSearchService} from '../IGuildSearchService';
import type {IMessageSearchService} from '../IMessageSearchService';
import type {IReportSearchService} from '../IReportSearchService';
import type {IUserSearchService} from '../IUserSearchService';
import {convertMessagesToSearchableMessages, convertToSearchableMessage} from '../message/MessageSearchSerializer';
import {convertToSearchableReport} from '../report/ReportSearchSerializer';
import {SearchAdapterServiceBase} from '../SearchAdapterServiceBase';
import {convertToSearchableUser} from '../user/UserSearchSerializer';
import type {MeilisearchClient} from './MeilisearchClient';
import {
	MeilisearchAuditLogAdapter,
	MeilisearchGuildAdapter,
	MeilisearchGuildMemberAdapter,
	MeilisearchMessageAdapter,
	MeilisearchReportAdapter,
	MeilisearchUserAdapter,
} from './MeilisearchDomainAdapters';
import {meiliTermFilter} from './MeilisearchFilterUtils';

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

export class MeilisearchMessageSearchService
	extends SearchAdapterServiceBase<MessageSearchFilters, SearchableMessage, MeilisearchMessageAdapter>
	implements IMessageSearchService
{
	constructor(client: MeilisearchClient) {
		super(new MeilisearchMessageAdapter({client}));
	}

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

	async deleteMessage(messageId: MessageID): Promise<void> {
		await this.deleteDocument(messageId.toString());
	}

	async deleteMessages(messageIds: Array<MessageID>): Promise<void> {
		await this.deleteDocuments(messageIds.map((id) => id.toString()));
	}

	async deleteChannelMessages(channelId: ChannelID): Promise<void> {
		await this.adapter.deleteByFilter(meiliTermFilter('channelId', channelId.toString()));
	}

	async deleteGuildMessages(guildId: GuildID): Promise<void> {
		await this.adapter.deleteByFilter(meiliTermFilter('guildId', guildId.toString()));
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

export class MeilisearchGuildSearchService
	extends SearchAdapterServiceBase<GuildSearchFilters, SearchableGuild, MeilisearchGuildAdapter>
	implements IGuildSearchService
{
	private readonly discoveryRepository: IGuildDiscoveryRepository | undefined;

	constructor(client: MeilisearchClient, discoveryRepository?: IGuildDiscoveryRepository) {
		super(new MeilisearchGuildAdapter({client}));
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

	async deleteGuilds(guildIds: Array<GuildID>): Promise<void> {
		await this.deleteDocuments(guildIds.map((id) => id.toString()));
	}

	searchGuilds(
		query: string,
		filters: GuildSearchFilters,
		options?: SchemaSearchOptions,
	): Promise<SchemaSearchResult<SearchableGuild>> {
		return this.search(query, filters, options);
	}
}

export class MeilisearchUserSearchService
	extends SearchAdapterServiceBase<UserSearchFilters, SearchableUser, MeilisearchUserAdapter>
	implements IUserSearchService
{
	constructor(client: MeilisearchClient) {
		super(new MeilisearchUserAdapter({client}));
	}

	async indexUser(user: User): Promise<void> {
		await this.indexDocument(convertToSearchableUser(user));
	}

	async indexUsers(users: Array<User>): Promise<void> {
		if (users.length === 0) return;
		await this.indexDocuments(users.map(convertToSearchableUser));
	}

	async updateUser(user: User): Promise<void> {
		await this.updateDocument(convertToSearchableUser(user));
	}

	async deleteUser(userId: UserID): Promise<void> {
		await this.deleteDocument(userId.toString());
	}

	async deleteUsers(userIds: Array<UserID>): Promise<void> {
		await this.deleteDocuments(userIds.map((id) => id.toString()));
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

export class MeilisearchReportSearchService
	extends SearchAdapterServiceBase<ReportSearchFilters, SearchableReport, MeilisearchReportAdapter>
	implements IReportSearchService
{
	constructor(client: MeilisearchClient) {
		super(new MeilisearchReportAdapter({client}));
	}

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

	async deleteReports(reportIds: Array<ReportID>): Promise<void> {
		await this.deleteDocuments(reportIds.map((id) => id.toString()));
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

	listReportsByReporter(
		reporterId: UserID,
		limit?: number,
		offset?: number,
	): Promise<SchemaSearchResult<SearchableReport>> {
		return this.searchReports('', {reporterId: reporterId.toString()}, {limit, offset});
	}

	listReportsByStatus(status: number, limit?: number, offset?: number): Promise<SchemaSearchResult<SearchableReport>> {
		return this.searchReports('', {status}, {limit, offset});
	}

	listReportsByType(
		reportType: number,
		limit?: number,
		offset?: number,
	): Promise<SchemaSearchResult<SearchableReport>> {
		return this.searchReports('', {reportType}, {limit, offset});
	}

	listReportsByReportedUser(
		reportedUserId: UserID,
		limit?: number,
		offset?: number,
	): Promise<SchemaSearchResult<SearchableReport>> {
		return this.searchReports('', {reportedUserId: reportedUserId.toString()}, {limit, offset});
	}

	listReportsByReportedGuild(
		reportedGuildId: GuildID,
		limit?: number,
		offset?: number,
	): Promise<SchemaSearchResult<SearchableReport>> {
		return this.searchReports('', {reportedGuildId: reportedGuildId.toString()}, {limit, offset});
	}

	listReportsByReportedMessage(
		reportedMessageId: MessageID,
		limit?: number,
		offset?: number,
	): Promise<SchemaSearchResult<SearchableReport>> {
		return this.searchReports('', {reportedMessageId: reportedMessageId.toString()}, {limit, offset});
	}
}

export class MeilisearchAuditLogSearchService
	extends SearchAdapterServiceBase<AuditLogSearchFilters, SearchableAuditLog, MeilisearchAuditLogAdapter>
	implements IAuditLogSearchService
{
	constructor(client: MeilisearchClient) {
		super(new MeilisearchAuditLogAdapter({client}));
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

export class MeilisearchGuildMemberSearchService
	extends SearchAdapterServiceBase<GuildMemberSearchFilters, SearchableGuildMember, MeilisearchGuildMemberAdapter>
	implements IGuildMemberSearchService
{
	constructor(client: MeilisearchClient) {
		super(new MeilisearchGuildMemberAdapter({client}));
	}

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
		await this.adapter.deleteByFilter(meiliTermFilter('guildId', guildId.toString()));
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
