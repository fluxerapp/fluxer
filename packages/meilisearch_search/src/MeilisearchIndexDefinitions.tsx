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

export type FluxerSearchIndexName = 'messages' | 'guilds' | 'users' | 'reports' | 'audit_logs' | 'guild_members';

export interface MeilisearchIndexDefinition {
	indexName: FluxerSearchIndexName;
	primaryKey: 'id';
	filterableAttributes: Array<string>;
	sortableAttributes: Array<string>;
	searchableAttributes: Array<string>;
}

export const MEILISEARCH_INDEX_DEFINITIONS: Record<FluxerSearchIndexName, MeilisearchIndexDefinition> = {
	messages: {
		indexName: 'messages',
		primaryKey: 'id',
		filterableAttributes: [
			'id',
			'channelId',
			'guildId',
			'authorId',
			'authorType',
			'createdAt',
			'editedAt',
			'isPinned',
			'mentionedUserIds',
			'mentionEveryone',
			'hasLink',
			'hasEmbed',
			'hasPoll',
			'hasFile',
			'hasVideo',
			'hasImage',
			'hasSound',
			'hasSticker',
			'hasForward',
			'embedTypes',
			'embedProviders',
			'linkHostnames',
			'attachmentFilenames',
			'attachmentExtensions',
		],
		sortableAttributes: ['createdAt'],
		searchableAttributes: ['content'],
	},
	guilds: {
		indexName: 'guilds',
		primaryKey: 'id',
		filterableAttributes: [
			'id',
			'ownerId',
			'verificationLevel',
			'mfaLevel',
			'nsfwLevel',
			'features',
			'createdAt',
			'isDiscoverable',
			'discoveryCategory',
		],
		sortableAttributes: ['createdAt'],
		searchableAttributes: ['name', 'vanityUrlCode', 'discoveryDescription'],
	},
	users: {
		indexName: 'users',
		primaryKey: 'id',
		filterableAttributes: [
			'id',
			'isBot',
			'isSystem',
			'emailVerified',
			'emailBounced',
			'premiumType',
			'tempBannedUntil',
			'pendingDeletionAt',
			'acls',
			'suspiciousActivityFlags',
			'createdAt',
			'lastActiveAt',
		],
		sortableAttributes: ['createdAt', 'lastActiveAt'],
		searchableAttributes: ['username', 'email', 'phone', 'id'],
	},
	reports: {
		indexName: 'reports',
		primaryKey: 'id',
		filterableAttributes: [
			'id',
			'reporterId',
			'reportedAt',
			'status',
			'reportType',
			'category',
			'reportedUserId',
			'reportedGuildId',
			'reportedMessageId',
			'guildContextId',
			'resolvedByAdminId',
			'resolvedAt',
			'createdAt',
		],
		sortableAttributes: ['createdAt', 'reportedAt', 'resolvedAt'],
		searchableAttributes: ['category', 'additionalInfo', 'reportedGuildName', 'reportedChannelName'],
	},
	audit_logs: {
		indexName: 'audit_logs',
		primaryKey: 'id',
		filterableAttributes: ['id', 'adminUserId', 'targetType', 'targetId', 'action', 'createdAt'],
		sortableAttributes: ['createdAt'],
		searchableAttributes: ['action', 'targetType', 'targetId', 'auditLogReason'],
	},
	guild_members: {
		indexName: 'guild_members',
		primaryKey: 'id',
		filterableAttributes: [
			'id',
			'guildId',
			'userId',
			'roleIds',
			'joinedAt',
			'joinSourceType',
			'sourceInviteCode',
			'userCreatedAt',
			'isBot',
		],
		sortableAttributes: ['joinedAt', 'userCreatedAt'],
		searchableAttributes: ['username', 'discriminator', 'globalName', 'nickname', 'userId'],
	},
};
