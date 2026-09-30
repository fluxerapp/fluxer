// SPDX-License-Identifier: AGPL-3.0-or-later

type ElasticsearchFieldType = 'text' | 'keyword' | 'boolean' | 'long' | 'integer' | 'date' | 'float';
export type FluxerSearchIndexName = 'messages' | 'guilds' | 'users' | 'reports' | 'audit_logs' | 'guild_members';

export interface ElasticsearchFieldMapping {
	type: ElasticsearchFieldType;
	index?: boolean;
	analyzer?: string;
	search_analyzer?: string;
	fields?: Record<string, ElasticsearchFieldMapping>;
}

export interface ElasticsearchIndexSettings {
	number_of_shards?: number;
	number_of_replicas?: number;
	analysis?: Record<string, unknown>;
}

export interface ElasticsearchIndexDefinition {
	indexName: FluxerSearchIndexName;
	mappings: {
		properties: Record<string, ElasticsearchFieldMapping>;
	};
	settings?: ElasticsearchIndexSettings;
}

const GUILD_DISCOVERY_ANALYSIS: Record<string, unknown> = {
	analyzer: {
		discovery_folding: {
			type: 'custom',
			tokenizer: 'standard',
			filter: ['lowercase', 'asciifolding'],
		},
	},
};

function discoveryText(): ElasticsearchFieldMapping {
	return {
		type: 'text',
		analyzer: 'discovery_folding',
		search_analyzer: 'discovery_folding',
		fields: {keyword: {type: 'keyword'}},
	};
}

function textWithKeyword(): ElasticsearchFieldMapping {
	return {type: 'text', fields: {keyword: {type: 'keyword'}}};
}

function keyword(): ElasticsearchFieldMapping {
	return {type: 'keyword'};
}

function bool(): ElasticsearchFieldMapping {
	return {type: 'boolean'};
}

function long(): ElasticsearchFieldMapping {
	return {type: 'long'};
}

function integer(): ElasticsearchFieldMapping {
	return {type: 'integer'};
}

export const ELASTICSEARCH_INDEX_DEFINITIONS: Record<FluxerSearchIndexName, ElasticsearchIndexDefinition> = {
	messages: {
		indexName: 'messages',
		mappings: {
			properties: {
				id: keyword(),
				channelId: keyword(),
				guildId: keyword(),
				authorId: keyword(),
				authorType: keyword(),
				content: textWithKeyword(),
				createdAt: long(),
				editedAt: long(),
				isPinned: bool(),
				mentionedUserIds: keyword(),
				mentionEveryone: bool(),
				hasLink: bool(),
				hasEmbed: bool(),
				hasPoll: bool(),
				hasFile: bool(),
				hasVideo: bool(),
				hasImage: bool(),
				hasSound: bool(),
				hasSticker: bool(),
				hasForward: bool(),
				embedTypes: keyword(),
				embedProviders: keyword(),
				embedContent: {type: 'text'},
				linkHostnames: keyword(),
				attachmentFilenames: keyword(),
				attachmentExtensions: keyword(),
			},
		},
	},
	guilds: {
		indexName: 'guilds',
		settings: {analysis: GUILD_DISCOVERY_ANALYSIS},
		mappings: {
			properties: {
				id: keyword(),
				ownerId: keyword(),
				name: discoveryText(),
				vanityUrlCode: discoveryText(),
				discoveryDescription: discoveryText(),
				discoveryTags: discoveryText(),
				discoveryPrimaryLanguage: keyword(),
				iconHash: keyword(),
				bannerHash: keyword(),
				splashHash: keyword(),
				features: keyword(),
				badgeIds: keyword(),
				verificationLevel: integer(),
				mfaLevel: integer(),
				nsfwLevel: integer(),
				createdAt: long(),
				memberCount: integer(),
				discoveryCategory: integer(),
				isDiscoverable: bool(),
			},
		},
	},
	users: {
		indexName: 'users',
		mappings: {
			properties: {
				id: keyword(),
				username: textWithKeyword(),
				email: textWithKeyword(),
				phone: textWithKeyword(),
				discriminator: integer(),
				isBot: bool(),
				isSystem: bool(),
				flags: keyword(),
				premiumType: integer(),
				emailVerified: bool(),
				emailBounced: bool(),
				suspiciousActivityFlags: integer(),
				acls: keyword(),
				createdAt: long(),
				lastActiveAt: long(),
				tempBannedUntil: long(),
				pendingDeletionAt: long(),
				stripeSubscriptionId: keyword(),
				stripeCustomerId: keyword(),
			},
		},
	},
	reports: {
		indexName: 'reports',
		mappings: {
			properties: {
				id: keyword(),
				reporterId: keyword(),
				reportedAt: long(),
				status: integer(),
				reportType: integer(),
				category: textWithKeyword(),
				additionalInfo: textWithKeyword(),
				reportedUserId: keyword(),
				reportedGuildId: keyword(),
				reportedGuildName: textWithKeyword(),
				reportedMessageId: keyword(),
				reportedChannelId: keyword(),
				reportedChannelName: textWithKeyword(),
				guildContextId: keyword(),
				resolvedAt: long(),
				resolvedByAdminId: keyword(),
				publicComment: keyword(),
				createdAt: long(),
			},
		},
	},
	audit_logs: {
		indexName: 'audit_logs',
		mappings: {
			properties: {
				id: keyword(),
				logId: keyword(),
				adminUserId: keyword(),
				targetType: textWithKeyword(),
				targetId: textWithKeyword(),
				action: textWithKeyword(),
				auditLogReason: textWithKeyword(),
				createdAt: long(),
			},
		},
	},
	guild_members: {
		indexName: 'guild_members',
		mappings: {
			properties: {
				id: keyword(),
				guildId: keyword(),
				userId: textWithKeyword(),
				username: textWithKeyword(),
				usernameSearch: textWithKeyword(),
				discriminator: textWithKeyword(),
				globalName: textWithKeyword(),
				nickname: textWithKeyword(),
				roleIds: keyword(),
				joinedAt: long(),
				joinSourceType: integer(),
				sourceInviteCode: keyword(),
				inviterId: keyword(),
				userCreatedAt: long(),
				isBot: bool(),
			},
		},
	},
};
