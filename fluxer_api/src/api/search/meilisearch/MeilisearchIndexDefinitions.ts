// SPDX-License-Identifier: AGPL-3.0-or-later

import type {FluxerSearchIndexName} from '@pkgs/elasticsearch_search/src/ElasticsearchIndexDefinitions';

export interface MeilisearchIndexDefinition {
	uid: FluxerSearchIndexName;
	primaryKey: string;
	searchableAttributes: Array<string>;
	filterableAttributes: Array<string>;
	sortableAttributes: Array<string>;
}

export const MEILISEARCH_INDEX_DEFINITIONS: Record<FluxerSearchIndexName, MeilisearchIndexDefinition> = {
	messages: {
		uid: 'messages',
		primaryKey: 'id',
		searchableAttributes: ['content', 'embedContent'],
		filterableAttributes: [
			'id',
			'createdAt',
			'channelId',
			'guildId',
			'authorId',
			'authorType',
			'mentionEveryone',
			'isPinned',
			'mentionedUserIds',
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
		sortableAttributes: ['createdAt', 'id'],
	},
	guilds: {
		uid: 'guilds',
		primaryKey: 'id',
		searchableAttributes: ['name', 'discoveryTags', 'vanityUrlCode', 'discoveryDescription'],
		filterableAttributes: [
			'ownerId',
			'verificationLevel',
			'mfaLevel',
			'nsfwLevel',
			'features',
			'isDiscoverable',
			'discoveryCategory',
			'discoveryPrimaryLanguage',
			'discoveryTags',
		],
		sortableAttributes: ['createdAt', 'memberCount', 'id'],
	},
	users: {
		uid: 'users',
		primaryKey: 'id',
		searchableAttributes: ['username', 'email', 'id'],
		filterableAttributes: [
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
		],
		sortableAttributes: ['createdAt', 'lastActiveAt', 'id'],
	},
	reports: {
		uid: 'reports',
		primaryKey: 'id',
		searchableAttributes: ['category', 'additionalInfo', 'reportedGuildName', 'reportedChannelName'],
		filterableAttributes: [
			'reporterId',
			'status',
			'reportType',
			'category',
			'reportedUserId',
			'reportedGuildId',
			'reportedMessageId',
			'guildContextId',
			'resolvedByAdminId',
			'resolvedAt',
		],
		sortableAttributes: ['createdAt', 'reportedAt', 'resolvedAt', 'id'],
	},
	audit_logs: {
		uid: 'audit_logs',
		primaryKey: 'id',
		searchableAttributes: ['action', 'targetType', 'targetId', 'auditLogReason'],
		filterableAttributes: ['adminUserId', 'targetType', 'targetId', 'action'],
		sortableAttributes: ['createdAt', 'id'],
	},
	guild_members: {
		uid: 'guild_members',
		primaryKey: 'id',
		searchableAttributes: ['username', 'usernameSearch', 'discriminator', 'globalName', 'nickname', 'userId'],
		filterableAttributes: [
			'guildId',
			'roleIds',
			'joinedAt',
			'joinSourceType',
			'sourceInviteCode',
			'userCreatedAt',
			'isBot',
		],
		sortableAttributes: ['joinedAt', 'id'],
	},
};
