// SPDX-License-Identifier: AGPL-3.0-or-later

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
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import type {MeilisearchClient} from './MeilisearchClient';
import {
	compactMeiliFilters,
	type MeilisearchFilter,
	meiliAndTerms,
	meiliExcludeAny,
	meiliExistsFilter,
	meiliNotExistsFilter,
	meiliRangeFilter,
	meiliTermFilter,
	meiliTermsFilter,
} from './MeilisearchFilterUtils';
import {MeilisearchIndexAdapter} from './MeilisearchIndexAdapter';
import {MEILISEARCH_INDEX_DEFINITIONS} from './MeilisearchIndexDefinitions';

const HAS_FIELD_MAP: Record<string, string> = {
	image: 'hasImage',
	sound: 'hasSound',
	video: 'hasVideo',
	file: 'hasFile',
	sticker: 'hasSticker',
	embed: 'hasEmbed',
	link: 'hasLink',
	poll: 'hasPoll',
	snapshot: 'hasForward',
};

interface MeilisearchAdapterOptions {
	client: MeilisearchClient;
}

function snowflakeSeconds(snowflake: string): number {
	return Math.floor(snowflakeToDate(BigInt(snowflake)).getTime() / 1000);
}

function buildMessageFilters(filters: MessageSearchFilters): Array<MeilisearchFilter | undefined> {
	const clauses: Array<MeilisearchFilter | undefined> = [];
	if (filters.guildId) clauses.push(meiliTermFilter('guildId', filters.guildId));
	if (filters.channelId) clauses.push(meiliTermFilter('channelId', filters.channelId));
	if (filters.channelIds && filters.channelIds.length > 0)
		clauses.push(meiliTermsFilter('channelId', filters.channelIds));
	if (filters.excludeChannelIds && filters.excludeChannelIds.length > 0) {
		clauses.push(...meiliExcludeAny('channelId', filters.excludeChannelIds));
	}
	if (filters.authorId && filters.authorId.length > 0) clauses.push(meiliTermsFilter('authorId', filters.authorId));
	if (filters.excludeAuthorIds && filters.excludeAuthorIds.length > 0) {
		clauses.push(...meiliExcludeAny('authorId', filters.excludeAuthorIds));
	}
	if (filters.authorType && filters.authorType.length > 0)
		clauses.push(meiliTermsFilter('authorType', filters.authorType));
	if (filters.excludeAuthorType && filters.excludeAuthorType.length > 0) {
		clauses.push(...meiliExcludeAny('authorType', filters.excludeAuthorType));
	}
	if (filters.mentions && filters.mentions.length > 0)
		clauses.push(...meiliAndTerms('mentionedUserIds', filters.mentions));
	if (filters.excludeMentions && filters.excludeMentions.length > 0) {
		clauses.push(...meiliExcludeAny('mentionedUserIds', filters.excludeMentions));
	}
	if (filters.mentionEveryone !== undefined) clauses.push(meiliTermFilter('mentionEveryone', filters.mentionEveryone));
	if (filters.pinned !== undefined) clauses.push(meiliTermFilter('isPinned', filters.pinned));
	if (filters.has && filters.has.length > 0) {
		for (const hasType of filters.has) {
			const field = HAS_FIELD_MAP[hasType];
			if (field) clauses.push(meiliTermFilter(field, true));
		}
	}
	if (filters.excludeHas && filters.excludeHas.length > 0) {
		for (const hasType of filters.excludeHas) {
			const field = HAS_FIELD_MAP[hasType];
			if (field) clauses.push(meiliTermFilter(field, false));
		}
	}
	if (filters.embedType && filters.embedType.length > 0)
		clauses.push(...meiliAndTerms('embedTypes', filters.embedType));
	if (filters.excludeEmbedTypes && filters.excludeEmbedTypes.length > 0) {
		clauses.push(...meiliExcludeAny('embedTypes', filters.excludeEmbedTypes));
	}
	if (filters.embedProvider && filters.embedProvider.length > 0)
		clauses.push(...meiliAndTerms('embedProviders', filters.embedProvider));
	if (filters.excludeEmbedProviders && filters.excludeEmbedProviders.length > 0) {
		clauses.push(...meiliExcludeAny('embedProviders', filters.excludeEmbedProviders));
	}
	if (filters.linkHostname && filters.linkHostname.length > 0)
		clauses.push(...meiliAndTerms('linkHostnames', filters.linkHostname));
	if (filters.excludeLinkHostnames && filters.excludeLinkHostnames.length > 0) {
		clauses.push(...meiliExcludeAny('linkHostnames', filters.excludeLinkHostnames));
	}
	if (filters.attachmentFilename && filters.attachmentFilename.length > 0) {
		clauses.push(...meiliAndTerms('attachmentFilenames', filters.attachmentFilename));
	}
	if (filters.excludeAttachmentFilenames && filters.excludeAttachmentFilenames.length > 0) {
		clauses.push(...meiliExcludeAny('attachmentFilenames', filters.excludeAttachmentFilenames));
	}
	if (filters.attachmentExtension && filters.attachmentExtension.length > 0) {
		clauses.push(...meiliAndTerms('attachmentExtensions', filters.attachmentExtension));
	}
	if (filters.excludeAttachmentExtensions && filters.excludeAttachmentExtensions.length > 0) {
		clauses.push(...meiliExcludeAny('attachmentExtensions', filters.excludeAttachmentExtensions));
	}
	if (filters.maxId != null) clauses.push(meiliRangeFilter('createdAt', {lt: snowflakeSeconds(filters.maxId)}));
	if (filters.minId != null) clauses.push(meiliRangeFilter('createdAt', {gt: snowflakeSeconds(filters.minId)}));
	return compactMeiliFilters(clauses);
}

function buildMessageSort(filters: MessageSearchFilters): Array<string> | undefined {
	const sortBy = filters.sortBy ?? 'timestamp';
	if (sortBy === 'relevance') return undefined;
	const sortOrder = filters.sortOrder ?? 'desc';
	return [`createdAt:${sortOrder}`, 'id:desc'];
}

function buildMessageQuery(query: string, filters: MessageSearchFilters): string {
	const terms = [...(filters.contents ?? []), query].filter((term) => term.trim().length > 0);
	const phrases = (filters.exactPhrases ?? []).map((phrase) => `"${phrase.replaceAll('"', '\\"')}"`);
	return [...terms, ...phrases].join(' ').trim();
}

function buildGuildFilters(filters: GuildSearchFilters): Array<MeilisearchFilter | undefined> {
	const clauses: Array<MeilisearchFilter | undefined> = [];
	if (filters.ownerId) clauses.push(meiliTermFilter('ownerId', filters.ownerId));
	if (filters.verificationLevel !== undefined)
		clauses.push(meiliTermFilter('verificationLevel', filters.verificationLevel));
	if (filters.mfaLevel !== undefined) clauses.push(meiliTermFilter('mfaLevel', filters.mfaLevel));
	if (filters.nsfwLevel !== undefined) clauses.push(meiliTermFilter('nsfwLevel', filters.nsfwLevel));
	if (filters.hasFeature && filters.hasFeature.length > 0)
		clauses.push(...meiliAndTerms('features', filters.hasFeature));
	if (filters.isDiscoverable !== undefined) clauses.push(meiliTermFilter('isDiscoverable', filters.isDiscoverable));
	if (filters.discoveryCategory !== undefined)
		clauses.push(meiliTermFilter('discoveryCategory', filters.discoveryCategory));
	if (filters.discoveryPrimaryLanguage !== undefined)
		clauses.push(meiliTermFilter('discoveryPrimaryLanguage', filters.discoveryPrimaryLanguage));
	if (filters.discoveryTag !== undefined && filters.discoveryTag.length > 0) {
		clauses.push(meiliTermFilter('discoveryTags', filters.discoveryTag.toLowerCase()));
	}
	return compactMeiliFilters(clauses);
}

function buildGuildSort(filters: GuildSearchFilters): Array<string> | undefined {
	const sortBy = filters.sortBy ?? 'createdAt';
	if (sortBy === 'relevance') return undefined;
	const sortOrder = filters.sortOrder ?? 'desc';
	return [`${sortBy}:${sortOrder}`, 'id:desc'];
}

function buildUserFilters(filters: UserSearchFilters): Array<MeilisearchFilter | undefined> {
	const clauses: Array<MeilisearchFilter | undefined> = [];
	if (filters.isBot !== undefined) clauses.push(meiliTermFilter('isBot', filters.isBot));
	if (filters.isSystem !== undefined) clauses.push(meiliTermFilter('isSystem', filters.isSystem));
	if (filters.emailVerified !== undefined) clauses.push(meiliTermFilter('emailVerified', filters.emailVerified));
	if (filters.emailBounced !== undefined) clauses.push(meiliTermFilter('emailBounced', filters.emailBounced));
	if (filters.hasPremium !== undefined)
		clauses.push(filters.hasPremium ? meiliExistsFilter('premiumType') : meiliNotExistsFilter('premiumType'));
	if (filters.isTempBanned !== undefined)
		clauses.push(filters.isTempBanned ? meiliExistsFilter('tempBannedUntil') : meiliNotExistsFilter('tempBannedUntil'));
	if (filters.isPendingDeletion !== undefined) {
		clauses.push(
			filters.isPendingDeletion ? meiliExistsFilter('pendingDeletionAt') : meiliNotExistsFilter('pendingDeletionAt'),
		);
	}
	if (filters.hasAcl && filters.hasAcl.length > 0) clauses.push(...meiliAndTerms('acls', filters.hasAcl));
	if (filters.minSuspiciousActivityFlags !== undefined) {
		clauses.push(meiliRangeFilter('suspiciousActivityFlags', {gte: filters.minSuspiciousActivityFlags}));
	}
	if (filters.createdAtGreaterThanOrEqual !== undefined) {
		clauses.push(meiliRangeFilter('createdAt', {gte: filters.createdAtGreaterThanOrEqual}));
	}
	if (filters.createdAtLessThanOrEqual !== undefined) {
		clauses.push(meiliRangeFilter('createdAt', {lte: filters.createdAtLessThanOrEqual}));
	}
	return compactMeiliFilters(clauses);
}

function buildUserSort(filters: UserSearchFilters): Array<string> | undefined {
	const sortBy = filters.sortBy ?? 'createdAt';
	if (sortBy === 'relevance') return undefined;
	const sortOrder = filters.sortOrder ?? 'desc';
	return [`${sortBy}:${sortOrder}`, 'id:desc'];
}

function buildReportFilters(filters: ReportSearchFilters): Array<MeilisearchFilter | undefined> {
	const clauses: Array<MeilisearchFilter | undefined> = [];
	if (filters.reporterId) clauses.push(meiliTermFilter('reporterId', filters.reporterId));
	if (filters.status !== undefined) clauses.push(meiliTermFilter('status', filters.status));
	if (filters.reportType !== undefined) clauses.push(meiliTermFilter('reportType', filters.reportType));
	if (filters.category) clauses.push(meiliTermFilter('category', filters.category));
	if (filters.reportedUserId) clauses.push(meiliTermFilter('reportedUserId', filters.reportedUserId));
	if (filters.reportedGuildId) clauses.push(meiliTermFilter('reportedGuildId', filters.reportedGuildId));
	if (filters.reportedMessageId) clauses.push(meiliTermFilter('reportedMessageId', filters.reportedMessageId));
	if (filters.guildContextId) clauses.push(meiliTermFilter('guildContextId', filters.guildContextId));
	if (filters.resolvedByAdminId) clauses.push(meiliTermFilter('resolvedByAdminId', filters.resolvedByAdminId));
	if (filters.isResolved !== undefined)
		clauses.push(filters.isResolved ? meiliExistsFilter('resolvedAt') : meiliNotExistsFilter('resolvedAt'));
	return compactMeiliFilters(clauses);
}

function buildReportSort(filters: ReportSearchFilters): Array<string> | undefined {
	const sortBy = filters.sortBy ?? 'reportedAt';
	if (sortBy === 'relevance') return undefined;
	const sortOrder = filters.sortOrder ?? 'desc';
	return [`${sortBy}:${sortOrder}`, 'id:desc'];
}

function buildAuditLogFilters(filters: AuditLogSearchFilters): Array<MeilisearchFilter | undefined> {
	const clauses: Array<MeilisearchFilter | undefined> = [];
	if (filters.adminUserId) clauses.push(meiliTermFilter('adminUserId', filters.adminUserId));
	if (filters.targetType) clauses.push(meiliTermFilter('targetType', filters.targetType));
	if (filters.targetId) clauses.push(meiliTermFilter('targetId', filters.targetId));
	if (filters.action) clauses.push(meiliTermFilter('action', filters.action));
	return compactMeiliFilters(clauses);
}

function buildAuditLogSort(filters: AuditLogSearchFilters): Array<string> | undefined {
	const sortBy = filters.sortBy ?? 'createdAt';
	if (sortBy === 'relevance') return undefined;
	const sortOrder = filters.sortOrder ?? 'desc';
	return [`createdAt:${sortOrder}`, 'id:desc'];
}

function buildGuildMemberFilters(filters: GuildMemberSearchFilters): Array<MeilisearchFilter | undefined> {
	const clauses: Array<MeilisearchFilter | undefined> = [];
	clauses.push(meiliTermFilter('guildId', filters.guildId));
	if (filters.roleIds && filters.roleIds.length > 0) clauses.push(...meiliAndTerms('roleIds', filters.roleIds));
	if (filters.joinedAtGte !== undefined) clauses.push(meiliRangeFilter('joinedAt', {gte: filters.joinedAtGte}));
	if (filters.joinedAtLte !== undefined) clauses.push(meiliRangeFilter('joinedAt', {lte: filters.joinedAtLte}));
	if (filters.joinSourceType && filters.joinSourceType.length > 0) {
		clauses.push(meiliTermsFilter('joinSourceType', filters.joinSourceType));
	}
	if (filters.sourceInviteCode && filters.sourceInviteCode.length > 0) {
		clauses.push(meiliTermsFilter('sourceInviteCode', filters.sourceInviteCode));
	}
	if (filters.userCreatedAtGte !== undefined)
		clauses.push(meiliRangeFilter('userCreatedAt', {gte: filters.userCreatedAtGte}));
	if (filters.userCreatedAtLte !== undefined)
		clauses.push(meiliRangeFilter('userCreatedAt', {lte: filters.userCreatedAtLte}));
	if (filters.isBot !== undefined) clauses.push(meiliTermFilter('isBot', filters.isBot));
	return compactMeiliFilters(clauses);
}

function buildGuildMemberSort(filters: GuildMemberSearchFilters): Array<string> | undefined {
	const sortBy = filters.sortBy ?? 'joinedAt';
	if (sortBy === 'relevance') return undefined;
	const sortOrder = filters.sortOrder ?? 'desc';
	return [`${sortBy}:${sortOrder}`, 'id:desc'];
}

export class MeilisearchMessageAdapter extends MeilisearchIndexAdapter<MessageSearchFilters, SearchableMessage> {
	constructor(options: MeilisearchAdapterOptions) {
		super({
			client: options.client,
			index: MEILISEARCH_INDEX_DEFINITIONS.messages,
			buildFilters: buildMessageFilters,
			buildSort: buildMessageSort,
			buildQuery: buildMessageQuery,
		});
	}
}

export class MeilisearchGuildAdapter extends MeilisearchIndexAdapter<GuildSearchFilters, SearchableGuild> {
	constructor(options: MeilisearchAdapterOptions) {
		super({
			client: options.client,
			index: MEILISEARCH_INDEX_DEFINITIONS.guilds,
			buildFilters: buildGuildFilters,
			buildSort: buildGuildSort,
		});
	}
}

export class MeilisearchUserAdapter extends MeilisearchIndexAdapter<UserSearchFilters, SearchableUser> {
	constructor(options: MeilisearchAdapterOptions) {
		super({
			client: options.client,
			index: MEILISEARCH_INDEX_DEFINITIONS.users,
			buildFilters: buildUserFilters,
			buildSort: buildUserSort,
		});
	}
}

export class MeilisearchReportAdapter extends MeilisearchIndexAdapter<ReportSearchFilters, SearchableReport> {
	constructor(options: MeilisearchAdapterOptions) {
		super({
			client: options.client,
			index: MEILISEARCH_INDEX_DEFINITIONS.reports,
			buildFilters: buildReportFilters,
			buildSort: buildReportSort,
		});
	}
}

export class MeilisearchAuditLogAdapter extends MeilisearchIndexAdapter<AuditLogSearchFilters, SearchableAuditLog> {
	constructor(options: MeilisearchAdapterOptions) {
		super({
			client: options.client,
			index: MEILISEARCH_INDEX_DEFINITIONS.audit_logs,
			buildFilters: buildAuditLogFilters,
			buildSort: buildAuditLogSort,
		});
	}
}

export class MeilisearchGuildMemberAdapter extends MeilisearchIndexAdapter<
	GuildMemberSearchFilters,
	SearchableGuildMember
> {
	constructor(options: MeilisearchAdapterOptions) {
		super({
			client: options.client,
			index: MEILISEARCH_INDEX_DEFINITIONS.guild_members,
			buildFilters: buildGuildMemberFilters,
			buildSort: buildGuildMemberSort,
		});
	}
}
