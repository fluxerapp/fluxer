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

export interface SearchableMessage {
	readonly [key: string]: unknown;
	id: string;
	channelId: string;
	guildId: string | null;
	authorId: string | null;
	authorType: 'user' | 'bot' | 'webhook';
	content: string | null;
	createdAt: number;
	editedAt: number | null;
	isPinned: boolean;
	mentionedUserIds: Array<string>;
	mentionEveryone: boolean;
	hasLink: boolean;
	hasEmbed: boolean;
	hasPoll: boolean;
	hasFile: boolean;
	hasVideo: boolean;
	hasImage: boolean;
	hasSound: boolean;
	hasSticker: boolean;
	hasForward: boolean;
	embedTypes: Array<string>;
	embedProviders: Array<string>;
	linkHostnames: Array<string>;
	attachmentFilenames: Array<string>;
	attachmentExtensions: Array<string>;
}

export interface MessageSearchFilters {
	maxId?: string;
	minId?: string;
	content?: string;
	contents?: Array<string>;
	exactPhrases?: Array<string>;
	guildId?: string;
	channelId?: string;
	channelIds?: Array<string>;
	excludeChannelIds?: Array<string>;
	authorId?: Array<string>;
	authorType?: Array<string>;
	excludeAuthorType?: Array<string>;
	excludeAuthorIds?: Array<string>;
	mentions?: Array<string>;
	excludeMentions?: Array<string>;
	mentionEveryone?: boolean;
	pinned?: boolean;
	has?: Array<string>;
	excludeHas?: Array<string>;
	embedType?: Array<'image' | 'video' | 'sound' | 'article'>;
	excludeEmbedTypes?: Array<'image' | 'video' | 'sound' | 'article'>;
	embedProvider?: Array<string>;
	excludeEmbedProviders?: Array<string>;
	linkHostname?: Array<string>;
	excludeLinkHostnames?: Array<string>;
	attachmentFilename?: Array<string>;
	excludeAttachmentFilenames?: Array<string>;
	attachmentExtension?: Array<string>;
	excludeAttachmentExtensions?: Array<string>;
	sortBy?: 'timestamp' | 'relevance';
	sortOrder?: 'asc' | 'desc';
	includeNsfw?: boolean;
}

export interface SearchableGuild {
	id: string;
	ownerId: string;
	name: string;
	vanityUrlCode: string | null;
	iconHash: string | null;
	bannerHash: string | null;
	splashHash: string | null;
	features: Array<string>;
	verificationLevel: number;
	mfaLevel: number;
	nsfwLevel: number;
	createdAt: number;
	discoveryDescription: string | null;
	discoveryCategory: number | null;
	isDiscoverable: boolean;
}

export interface GuildSearchFilters {
	ownerId?: string;
	verificationLevel?: number;
	mfaLevel?: number;
	nsfwLevel?: number;
	hasFeature?: Array<string>;
	isDiscoverable?: boolean;
	discoveryCategory?: number;
	sortBy?: 'createdAt' | 'relevance';
	sortOrder?: 'asc' | 'desc';
}

export interface SearchableUser {
	id: string;
	username: string;
	discriminator: number;
	email: string | null;
	phone: string | null;
	isBot: boolean;
	isSystem: boolean;
	flags: string;
	premiumType: number | null;
	emailVerified: boolean;
	emailBounced: boolean;
	suspiciousActivityFlags: number;
	acls: Array<string>;
	createdAt: number;
	lastActiveAt: number | null;
	tempBannedUntil: number | null;
	pendingDeletionAt: number | null;
	stripeSubscriptionId: string | null;
	stripeCustomerId: string | null;
}

export interface UserSearchFilters {
	isBot?: boolean;
	isSystem?: boolean;
	emailVerified?: boolean;
	emailBounced?: boolean;
	hasPremium?: boolean;
	isTempBanned?: boolean;
	isPendingDeletion?: boolean;
	hasAcl?: Array<string>;
	minSuspiciousActivityFlags?: number;
	createdAtGreaterThanOrEqual?: number;
	createdAtLessThanOrEqual?: number;
	sortBy?: 'createdAt' | 'lastActiveAt' | 'relevance';
	sortOrder?: 'asc' | 'desc';
}

export interface SearchableReport {
	id: string;
	reporterId: string;
	reportedAt: number;
	status: number;
	reportType: number;
	category: string;
	additionalInfo: string | null;
	reportedUserId: string | null;
	reportedGuildId: string | null;
	reportedGuildName: string | null;
	reportedMessageId: string | null;
	reportedChannelId: string | null;
	reportedChannelName: string | null;
	guildContextId: string | null;
	resolvedAt: number | null;
	resolvedByAdminId: string | null;
	publicComment: string | null;
	createdAt: number;
}

export interface ReportSearchFilters {
	reporterId?: string;
	status?: number;
	reportType?: number;
	category?: string;
	reportedUserId?: string;
	reportedGuildId?: string;
	reportedMessageId?: string;
	guildContextId?: string;
	resolvedByAdminId?: string;
	isResolved?: boolean;
	sortBy?: 'createdAt' | 'reportedAt' | 'resolvedAt' | 'relevance';
	sortOrder?: 'asc' | 'desc';
}

export interface SearchableAuditLog {
	id: string;
	logId: string;
	adminUserId: string;
	targetType: string;
	targetId: string;
	action: string;
	auditLogReason: string | null;
	createdAt: number;
}

export interface AuditLogSearchFilters {
	adminUserId?: string;
	targetType?: string;
	targetId?: string;
	action?: string;
	sortBy?: 'createdAt' | 'relevance';
	sortOrder?: 'asc' | 'desc';
}

export interface SearchableGuildMember {
	readonly [key: string]: unknown;
	id: string;
	guildId: string;
	userId: string;
	username: string;
	discriminator: string;
	globalName: string | null;
	nickname: string | null;
	roleIds: Array<string>;
	joinedAt: number;
	joinSourceType: number | null;
	sourceInviteCode: string | null;
	inviterId: string | null;
	userCreatedAt: number;
	isBot: boolean;
}

export interface GuildMemberSearchFilters {
	guildId: string;
	query?: string;
	roleIds?: Array<string>;
	joinedAtGte?: number;
	joinedAtLte?: number;
	joinSourceType?: Array<number>;
	sourceInviteCode?: Array<string>;
	userCreatedAtGte?: number;
	userCreatedAtLte?: number;
	isBot?: boolean;
	sortBy?: 'joinedAt' | 'relevance';
	sortOrder?: 'asc' | 'desc';
}
