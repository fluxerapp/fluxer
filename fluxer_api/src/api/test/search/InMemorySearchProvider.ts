// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildDiscoveryRepository} from '@app/api/guild/repositories/GuildDiscoveryRepository';
import type {IAuditLogSearchService} from '@app/api/search/IAuditLogSearchService';
import type {IGuildMemberSearchService} from '@app/api/search/IGuildMemberSearchService';
import type {IGuildSearchService} from '@app/api/search/IGuildSearchService';
import type {IMessageSearchService} from '@app/api/search/IMessageSearchService';
import type {IReportSearchService} from '@app/api/search/IReportSearchService';
import type {ISearchProvider} from '@app/api/search/ISearchProvider';
import type {IThreadSearchService} from '@app/api/search/IThreadSearchService';
import type {IUserSearchService} from '@app/api/search/IUserSearchService';
import {
	AuditLogSearchService,
	GuildMemberSearchService,
	GuildSearchService,
	MessageSearchService,
	ReportSearchService,
	UserSearchService,
} from '@app/api/search/SearchServices';
import {ThreadSearchIndex} from '@app/api/search/thread/ThreadSearchIndex';
import type {SearchOptions, SearchResult} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
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
	SearchableThread,
	SearchableUser,
	ThreadSearchCursor,
	ThreadSearchFilters,
	UserSearchFilters,
} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';

interface SearchableDocument {
	id: string;
}

type Matcher<TDocument, TFilters> = (doc: TDocument, filters: TFilters) => boolean;
type TextCollector<TDocument> = (doc: TDocument) => Array<string | null>;
type Sorter<TDocument, TFilters> = (left: TDocument, right: TDocument, filters: TFilters, query: string) => number;

const DEFAULT_LIMIT = 25;

function normalize(value: string | null | undefined): string {
	return (value ?? '').toLowerCase();
}

function containsText(value: string | null | undefined, query: string): boolean {
	const normalizedQuery = normalize(query).trim();
	if (normalizedQuery.length === 0) {
		return true;
	}
	const normalizedValue = normalize(value);
	if (normalizedValue.includes(normalizedQuery)) {
		return true;
	}
	const terms = normalizedQuery.split(/\s+/).filter((term) => term.length > 0);
	return terms.length > 0 && terms.every((term) => normalizedValue.includes(term));
}

function arrayIncludesAny(values: ReadonlyArray<string>, needles: ReadonlyArray<string> | undefined): boolean {
	return !needles || needles.length === 0 || needles.some((needle) => values.includes(needle));
}

function arrayExcludesAll(values: ReadonlyArray<string>, needles: ReadonlyArray<string> | undefined): boolean {
	return !needles || needles.every((needle) => !values.includes(needle));
}

function stringArrayContainsText(values: ReadonlyArray<string>, query: string): boolean {
	return values.some((value) => containsText(value, query));
}

function paginate<T>(items: Array<T>, options?: SearchOptions): Array<T> {
	const limit = options?.limit ?? options?.hitsPerPage ?? DEFAULT_LIMIT;
	const offset = options?.offset ?? ((options?.page ?? 1) - 1) * limit;
	return items.slice(offset, offset + limit);
}

function relevanceScore(values: Array<string | null>, query: string): number {
	const normalizedQuery = normalize(query).trim();
	if (!normalizedQuery) {
		return 0;
	}
	let score = 0;
	for (const value of values) {
		const text = normalize(value);
		if (text === normalizedQuery) {
			score += 100;
		}
		let index = text.indexOf(normalizedQuery);
		while (index !== -1) {
			score += 1;
			index = text.indexOf(normalizedQuery, index + normalizedQuery.length);
		}
	}
	return score;
}

class InMemorySearchAdapter<TFilters, TDocument extends SearchableDocument> {
	private readonly docs = new Map<string, TDocument>();

	constructor(
		private readonly matchesFilters: Matcher<TDocument, TFilters>,
		private readonly collectText: TextCollector<TDocument>,
		private readonly sortDocuments: Sorter<TDocument, TFilters>,
	) {}

	async initialize(): Promise<void> {}

	async shutdown(): Promise<void> {
		this.docs.clear();
	}

	isAvailable(): boolean {
		return true;
	}

	async indexDocument(doc: TDocument): Promise<void> {
		this.docs.set(doc.id, doc);
	}

	async indexDocuments(docs: Array<TDocument>): Promise<void> {
		for (const doc of docs) {
			this.docs.set(doc.id, doc);
		}
	}

	async updateDocument(doc: TDocument): Promise<void> {
		this.docs.set(doc.id, doc);
	}

	async deleteDocument(id: string): Promise<void> {
		this.docs.delete(id);
	}

	async deleteDocuments(ids: Array<string>): Promise<void> {
		for (const id of ids) {
			this.docs.delete(id);
		}
	}

	async deleteByTerm(field: string, value: string): Promise<void> {
		for (const [id, doc] of this.docs) {
			if ((doc as unknown as Record<string, unknown>)[field] === value) this.docs.delete(id);
		}
	}

	async deleteAllDocuments(): Promise<void> {
		this.docs.clear();
	}

	async bulkIndexDocuments(docs: Array<TDocument>): Promise<void> {
		await this.indexDocuments(docs);
	}

	async refreshIndex(): Promise<void> {}

	async search(query: string, filters: TFilters, options?: SearchOptions): Promise<SearchResult<TDocument>> {
		const hits = Array.from(this.docs.values())
			.filter((doc) => this.matchesFilters(doc, filters))
			.filter((doc) => {
				const trimmed = query.trim();
				return trimmed.length === 0 || stringArrayContainsText(this.collectText(doc).filter(isString), trimmed);
			})
			.sort((left, right) => this.sortDocuments(left, right, filters, query));
		const facets = options?.facets;
		return {
			hits: paginate(hits, options),
			total: hits.length,
			...(facets && facets.length > 0 ? {facetCounts: countFacets(hits, facets)} : {}),
		};
	}
}

function countFacets<TDocument>(docs: Array<TDocument>, facets: Array<string>): Record<string, Record<string, number>> {
	const counts: Record<string, Record<string, number>> = {};
	for (const facet of facets) {
		const facetCounts: Record<string, number> = {};
		for (const doc of docs) {
			const value = (doc as Record<string, unknown>)[facet];
			if (value == null) {
				continue;
			}
			for (const entry of Array.isArray(value) ? value : [value]) {
				const key = String(entry);
				facetCounts[key] = (facetCounts[key] ?? 0) + 1;
			}
		}
		counts[facet] = facetCounts;
	}
	return counts;
}

function isString(value: string | null): value is string {
	return value !== null;
}

function matchesHasFilter(doc: SearchableMessage, has: string): boolean {
	switch (has) {
		case 'link':
			return doc.hasLink;
		case 'embed':
			return doc.hasEmbed;
		case 'poll':
			return doc.hasPoll;
		case 'file':
			return doc.hasFile;
		case 'video':
			return doc.hasVideo;
		case 'image':
			return doc.hasImage;
		case 'sound':
			return doc.hasSound;
		case 'sticker':
			return doc.hasSticker;
		case 'snapshot':
			return doc.hasForward;
		default:
			return false;
	}
}

function matchesMessageFilters(doc: SearchableMessage, filters: MessageSearchFilters): boolean {
	if (filters.maxId && BigInt(doc.id) >= BigInt(filters.maxId)) return false;
	if (filters.minId && BigInt(doc.id) <= BigInt(filters.minId)) return false;
	if (filters.guildId !== undefined && doc.guildId !== filters.guildId) return false;
	if (filters.channelId !== undefined && doc.channelId !== filters.channelId) return false;
	if (filters.channelIds && !filters.channelIds.includes(doc.channelId)) return false;
	if (filters.excludeChannelIds?.includes(doc.channelId)) return false;
	if (filters.authorId && (!doc.authorId || !filters.authorId.includes(doc.authorId))) return false;
	if (filters.excludeAuthorIds?.includes(doc.authorId ?? '')) return false;
	if (filters.authorType && !filters.authorType.includes(doc.authorType)) return false;
	if (filters.excludeAuthorType?.includes(doc.authorType)) return false;
	if (filters.mentions && !filters.mentions.every((id) => doc.mentionedUserIds.includes(id))) return false;
	if (filters.excludeMentions?.some((id) => doc.mentionedUserIds.includes(id))) return false;
	if (filters.mentionEveryone !== undefined && doc.mentionEveryone !== filters.mentionEveryone) return false;
	if (filters.pinned !== undefined && doc.isPinned !== filters.pinned) return false;
	if (filters.has && !filters.has.every((entry) => matchesHasFilter(doc, entry))) return false;
	if (filters.excludeHas?.some((entry) => matchesHasFilter(doc, entry))) return false;
	if (!arrayIncludesAny(doc.embedTypes, filters.embedType)) return false;
	if (!arrayExcludesAll(doc.embedTypes, filters.excludeEmbedTypes)) return false;
	if (!arrayIncludesAny(doc.embedProviders, filters.embedProvider)) return false;
	if (!arrayExcludesAll(doc.embedProviders, filters.excludeEmbedProviders)) return false;
	if (!arrayIncludesAny(doc.linkHostnames, filters.linkHostname)) return false;
	if (!arrayExcludesAll(doc.linkHostnames, filters.excludeLinkHostnames)) return false;
	if (!arrayIncludesAny(doc.attachmentFilenames, filters.attachmentFilename)) return false;
	if (!arrayExcludesAll(doc.attachmentFilenames, filters.excludeAttachmentFilenames)) return false;
	if (!arrayIncludesAny(doc.attachmentExtensions, filters.attachmentExtension)) return false;
	if (!arrayExcludesAll(doc.attachmentExtensions, filters.excludeAttachmentExtensions)) return false;
	if (filters.content && !messageContains(doc, filters.content)) return false;
	if (filters.contents && !filters.contents.some((content) => messageContains(doc, content))) return false;
	if (filters.exactPhrases && !filters.exactPhrases.every((phrase) => (doc.content ?? '').includes(phrase)))
		return false;
	return true;
}

function messageContains(doc: SearchableMessage, query: string): boolean {
	return containsText(doc.content, query) || stringArrayContainsText(doc.embedContent, query);
}

function collectMessageText(doc: SearchableMessage): Array<string | null> {
	return [doc.content, ...doc.embedContent, ...doc.attachmentFilenames, ...doc.linkHostnames];
}

function sortMessageDocs(
	left: SearchableMessage,
	right: SearchableMessage,
	filters: MessageSearchFilters,
	query: string,
): number {
	if (filters.sortBy === 'timestamp') {
		const direction = filters.sortOrder === 'asc' ? 1 : -1;
		return (left.createdAt - right.createdAt || Number(BigInt(left.id) - BigInt(right.id))) * direction;
	}
	const scoreDelta = relevanceScore(collectMessageText(right), query) - relevanceScore(collectMessageText(left), query);
	if (scoreDelta !== 0) {
		return scoreDelta;
	}
	return Number(BigInt(right.id) - BigInt(left.id));
}

function matchesUserFilters(doc: SearchableUser, filters: UserSearchFilters): boolean {
	if (filters.isBot !== undefined && doc.isBot !== filters.isBot) return false;
	if (filters.isSystem !== undefined && doc.isSystem !== filters.isSystem) return false;
	if (filters.emailVerified !== undefined && doc.emailVerified !== filters.emailVerified) return false;
	if (filters.emailBounced !== undefined && doc.emailBounced !== filters.emailBounced) return false;
	if (filters.hasPremium !== undefined && (doc.premiumType !== null) !== filters.hasPremium) return false;
	if (filters.isTempBanned !== undefined && (doc.tempBannedUntil !== null) !== filters.isTempBanned) return false;
	if (filters.isPendingDeletion !== undefined && (doc.pendingDeletionAt !== null) !== filters.isPendingDeletion) {
		return false;
	}
	if (filters.hasAcl && !filters.hasAcl.every((acl) => doc.acls.includes(acl))) return false;
	if (filters.createdAtGreaterThanOrEqual !== undefined && doc.createdAt < filters.createdAtGreaterThanOrEqual) {
		return false;
	}
	if (filters.createdAtLessThanOrEqual !== undefined && doc.createdAt > filters.createdAtLessThanOrEqual) {
		return false;
	}
	return true;
}

function collectUserText(doc: SearchableUser): Array<string | null> {
	return [doc.id, doc.username, doc.email, doc.stripeCustomerId, doc.stripeSubscriptionId];
}

function sortNumericField<TDocument extends SearchableDocument, TFilters>(
	field: keyof TDocument,
	defaultOrder: 'asc' | 'desc',
): Sorter<TDocument, TFilters> {
	return (left, right, filters, query) => {
		const sortFilters = filters as {sortBy?: string; sortOrder?: 'asc' | 'desc'};
		if (sortFilters.sortBy === 'relevance') {
			const delta = relevanceScore(collectObjectText(right), query) - relevanceScore(collectObjectText(left), query);
			if (delta !== 0) return delta;
		}
		const order = sortFilters.sortOrder ?? defaultOrder;
		const direction = order === 'asc' ? 1 : -1;
		const leftValue = Number(left[field] ?? 0);
		const rightValue = Number(right[field] ?? 0);
		return (leftValue - rightValue) * direction;
	};
}

function collectObjectText(doc: SearchableDocument): Array<string | null> {
	return Object.values(doc)
		.flatMap((value) => (Array.isArray(value) ? value : [value]))
		.filter((value): value is string => typeof value === 'string');
}

function matchesGuildFilters(doc: SearchableGuild, filters: GuildSearchFilters): boolean {
	if (filters.ownerId !== undefined && doc.ownerId !== filters.ownerId) return false;
	if (filters.verificationLevel !== undefined && doc.verificationLevel !== filters.verificationLevel) return false;
	if (filters.mfaLevel !== undefined && doc.mfaLevel !== filters.mfaLevel) return false;
	if (filters.nsfwLevel !== undefined && doc.nsfwLevel !== filters.nsfwLevel) return false;
	if (filters.hasFeature && !filters.hasFeature.every((feature) => doc.features.includes(feature))) return false;
	if (filters.isDiscoverable !== undefined && doc.isDiscoverable !== filters.isDiscoverable) return false;
	if (filters.discoveryCategory !== undefined && doc.discoveryCategory !== filters.discoveryCategory) return false;
	if (
		filters.discoveryPrimaryLanguage !== undefined &&
		doc.discoveryPrimaryLanguage !== filters.discoveryPrimaryLanguage
	) {
		return false;
	}
	if (filters.discoveryTag !== undefined && !doc.discoveryTags.includes(filters.discoveryTag)) return false;
	return true;
}

function collectGuildText(doc: SearchableGuild): Array<string | null> {
	return [doc.name, doc.vanityUrlCode, doc.discoveryDescription, ...doc.discoveryTags];
}

const sortGuildsByCreatedAt = sortNumericField<SearchableGuild, GuildSearchFilters>('createdAt', 'asc');
const sortGuildsByMemberCount = sortNumericField<SearchableGuild, GuildSearchFilters>('memberCount', 'desc');

function sortGuilds(left: SearchableGuild, right: SearchableGuild, filters: GuildSearchFilters, query: string): number {
	const sorter = filters.sortBy === 'memberCount' ? sortGuildsByMemberCount : sortGuildsByCreatedAt;
	const delta = sorter(left, right, filters, query);
	if (delta !== 0) return delta;
	const leftId = BigInt(left.id);
	const rightId = BigInt(right.id);
	if (leftId === rightId) return 0;
	return leftId > rightId ? -1 : 1;
}

function matchesReportFilters(doc: SearchableReport, filters: ReportSearchFilters): boolean {
	if (filters.reporterId !== undefined && doc.reporterId !== filters.reporterId) return false;
	if (filters.status !== undefined && doc.status !== filters.status) return false;
	if (filters.reportType !== undefined && doc.reportType !== filters.reportType) return false;
	if (filters.category !== undefined && doc.category !== filters.category) return false;
	if (filters.reason !== undefined && doc.reason !== filters.reason) return false;
	if (filters.reportedUserId !== undefined && doc.reportedUserId !== filters.reportedUserId) return false;
	if (filters.reportedWebhookId !== undefined && doc.reportedWebhookId !== filters.reportedWebhookId) return false;
	if (filters.reportedGuildId !== undefined && doc.reportedGuildId !== filters.reportedGuildId) return false;
	if (filters.reportedMessageId !== undefined && doc.reportedMessageId !== filters.reportedMessageId) return false;
	if (filters.reportedChannelId !== undefined && doc.reportedChannelId !== filters.reportedChannelId) return false;
	if (filters.guildContextId !== undefined && doc.guildContextId !== filters.guildContextId) return false;
	if (filters.resolvedByAdminId !== undefined && doc.resolvedByAdminId !== filters.resolvedByAdminId) return false;
	if (filters.isResolved !== undefined && (doc.resolvedAt !== null) !== filters.isResolved) return false;
	return true;
}

function collectReportText(doc: SearchableReport): Array<string | null> {
	return [
		doc.id,
		doc.category,
		doc.additionalInfo,
		doc.reportedGuildName,
		doc.reportedChannelName,
		doc.publicComment,
		doc.reportedUserId,
		doc.reportedWebhookId,
		doc.reportedGuildId,
		doc.reportedMessageId,
	];
}

function matchesAuditLogFilters(doc: SearchableAuditLog, filters: AuditLogSearchFilters): boolean {
	if (filters.adminUserId !== undefined && doc.adminUserId !== filters.adminUserId) return false;
	if (filters.targetType !== undefined && doc.targetType !== filters.targetType) return false;
	if (filters.targetId !== undefined && doc.targetId !== filters.targetId) return false;
	if (filters.action !== undefined && doc.action !== filters.action) return false;
	if (filters.actions !== undefined && !filters.actions.includes(doc.action)) return false;
	if (filters.excludeActions?.includes(doc.action)) return false;
	return true;
}

function collectAuditLogText(doc: SearchableAuditLog): Array<string | null> {
	return [doc.id, doc.adminUserId, doc.targetType, doc.targetId, doc.action, doc.auditLogReason];
}

function matchesGuildMemberFilters(doc: SearchableGuildMember, filters: GuildMemberSearchFilters): boolean {
	if (doc.guildId !== filters.guildId) return false;
	if (filters.roleIds && !filters.roleIds.every((roleId) => doc.roleIds.includes(roleId))) return false;
	if (filters.joinedAtGte !== undefined && doc.joinedAt < filters.joinedAtGte) return false;
	if (filters.joinedAtLte !== undefined && doc.joinedAt > filters.joinedAtLte) return false;
	if (filters.joinSourceType && !filters.joinSourceType.includes(doc.joinSourceType!)) return false;
	if (filters.sourceInviteCode && !filters.sourceInviteCode.includes(doc.sourceInviteCode ?? '')) return false;
	if (filters.userCreatedAtGte !== undefined && doc.userCreatedAt < filters.userCreatedAtGte) return false;
	if (filters.userCreatedAtLte !== undefined && doc.userCreatedAt > filters.userCreatedAtLte) return false;
	if (filters.isBot !== undefined && doc.isBot !== filters.isBot) return false;
	const memberQuery = filters.query;
	if (memberQuery && !collectGuildMemberText(doc).some((value) => containsText(value, memberQuery))) return false;
	return true;
}

function collectGuildMemberText(doc: SearchableGuildMember): Array<string | null> {
	return [doc.id, doc.userId, doc.username, doc.usernameSearch, doc.discriminator, doc.globalName, doc.nickname];
}

function sortGuildMemberDocs(
	left: SearchableGuildMember,
	right: SearchableGuildMember,
	filters: GuildMemberSearchFilters,
	query: string,
): number {
	if (filters.sortBy === 'relevance') {
		const delta =
			relevanceScore(collectGuildMemberText(right), query) - relevanceScore(collectGuildMemberText(left), query);
		if (delta !== 0) return delta;
	}
	const direction = filters.sortOrder === 'asc' ? 1 : -1;
	return (left.joinedAt - right.joinedAt) * direction;
}

function compareThreadCursor(doc: SearchableThread, cursor: ThreadSearchCursor): number {
	return doc.createdAt - cursor.createdAt || doc.idSequence - cursor.idSequence;
}

function matchesThreadFilters(doc: SearchableThread, filters: ThreadSearchFilters): boolean {
	if (doc.guildId !== filters.guildId || doc.parentId !== filters.parentId) return false;
	if (filters.publicOnly && doc.type === 12 && !filters.privateThreadIds?.includes(doc.id)) return false;
	if (filters.archived !== undefined && doc.archived !== filters.archived) return false;
	if (filters.tagIds && filters.tagIds.length > 0) {
		const matches =
			filters.tagSetting === 'match_all'
				? filters.tagIds.every((id) => doc.appliedTagIds.includes(id))
				: filters.tagIds.some((id) => doc.appliedTagIds.includes(id));
		if (!matches) return false;
	}
	if (filters.after && compareThreadCursor(doc, filters.after) <= 0) return false;
	if (filters.before && compareThreadCursor(doc, filters.before) >= 0) return false;
	return true;
}

const THREAD_SORT_FIELDS = {
	last_message_time: 'lastMessageAt',
	archive_time: 'archivedAt',
	creation_time: 'createdAt',
} as const;

function sortThreadDocs(
	left: SearchableThread,
	right: SearchableThread,
	filters: ThreadSearchFilters,
	query: string,
): number {
	const sortBy = filters.sortBy ?? 'last_message_time';
	if (sortBy === 'relevance') {
		const delta = relevanceScore([right.name], query) - relevanceScore([left.name], query);
		if (delta !== 0) return delta;
		return Number(BigInt(right.id) - BigInt(left.id));
	}
	const field = THREAD_SORT_FIELDS[sortBy];
	const direction = filters.sortOrder === 'asc' ? 1 : -1;
	return (left[field] - right[field] || Number(BigInt(left.id) - BigInt(right.id))) * direction;
}

export class InMemorySearchProvider implements ISearchProvider {
	private readonly messages = new MessageSearchService(
		new InMemorySearchAdapter<MessageSearchFilters, SearchableMessage>(
			matchesMessageFilters,
			collectMessageText,
			sortMessageDocs,
		),
	);
	private readonly guilds = new GuildSearchService(
		new InMemorySearchAdapter<GuildSearchFilters, SearchableGuild>(matchesGuildFilters, collectGuildText, sortGuilds),
		new GuildDiscoveryRepository(),
	);
	private readonly users = new UserSearchService(
		new InMemorySearchAdapter<UserSearchFilters, SearchableUser>(
			matchesUserFilters,
			collectUserText,
			sortNumericField<SearchableUser, UserSearchFilters>('createdAt', 'asc'),
		),
	);
	private readonly reports = new ReportSearchService(
		new InMemorySearchAdapter<ReportSearchFilters, SearchableReport>(
			matchesReportFilters,
			collectReportText,
			sortNumericField<SearchableReport, ReportSearchFilters>('createdAt', 'desc'),
		),
	);
	private readonly auditLogs = new AuditLogSearchService(
		new InMemorySearchAdapter<AuditLogSearchFilters, SearchableAuditLog>(
			matchesAuditLogFilters,
			collectAuditLogText,
			sortNumericField<SearchableAuditLog, AuditLogSearchFilters>('createdAt', 'desc'),
		),
	);
	private readonly guildMembers = new GuildMemberSearchService(
		new InMemorySearchAdapter<GuildMemberSearchFilters, SearchableGuildMember>(
			matchesGuildMemberFilters,
			collectGuildMemberText,
			sortGuildMemberDocs,
		),
	);
	private readonly threads = new ThreadSearchIndex(
		new InMemorySearchAdapter<ThreadSearchFilters, SearchableThread>(
			matchesThreadFilters,
			(doc) => [doc.name],
			sortThreadDocs,
		),
	);

	async initialize(): Promise<void> {
		await Promise.all([
			this.messages.initialize(),
			this.guilds.initialize(),
			this.users.initialize(),
			this.reports.initialize(),
			this.auditLogs.initialize(),
			this.guildMembers.initialize(),
		]);
	}

	async shutdown(): Promise<void> {
		await Promise.all([
			this.messages.shutdown(),
			this.guilds.shutdown(),
			this.users.shutdown(),
			this.reports.shutdown(),
			this.auditLogs.shutdown(),
			this.guildMembers.shutdown(),
			this.threads.shutdown(),
		]);
	}

	getMessageSearchService(): IMessageSearchService {
		return this.messages;
	}

	getGuildSearchService(): IGuildSearchService {
		return this.guilds;
	}

	getUserSearchService(): IUserSearchService {
		return this.users;
	}

	getReportSearchService(): IReportSearchService {
		return this.reports;
	}

	getAuditLogSearchService(): IAuditLogSearchService {
		return this.auditLogs;
	}

	getGuildMemberSearchService(): IGuildMemberSearchService {
		return this.guildMembers;
	}

	getThreadSearchService(): IThreadSearchService {
		return this.threads;
	}
}
