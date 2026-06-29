// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	GIFT_CODE_DURATION_TYPE_DEFINITIONS,
	MAX_GIFT_CODES_PER_REQUEST,
	MAX_GIFT_DURATION_QUANTITY,
} from '@fluxer/constants/src/GiftCodeConstants';
import {
	CONTENT_WARNING_TEXT_MAX_LENGTH,
	SystemChannelFlags,
	SystemChannelFlagsDescriptions,
} from '@fluxer/constants/src/GuildConstants';
import {LIMIT_KEYS} from '@fluxer/constants/src/LimitConfigMetadata';
import {GuildAdminResponse} from '@fluxer/schema/src/domains/admin/AdminGuildSchemas';
import {UserAdminResponseSchema} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import {
	GatewayRolloutConfigResponse,
	GatewayRolloutConfigUpdateRequest,
} from '@fluxer/schema/src/domains/admin/GatewayRolloutSchemas';
import {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import {MessageResponseSchema} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {ChannelTypeSchema} from '@fluxer/schema/src/primitives/ChannelValidators';
import {
	ContentWarningLevelSchema,
	DefaultMessageNotificationsSchema,
	GuildExplicitContentFilterSchema,
	GuildMFALevelSchema,
	GuildVerificationLevelSchema,
	NSFWLevelSchema,
} from '@fluxer/schema/src/primitives/GuildValidators';
import {PermissionStringType} from '@fluxer/schema/src/primitives/PermissionValidators';
import {
	createBitflagInt32Type,
	createInt32EnumType,
	createNamedStringLiteralUnion,
	createStringType,
	Int32Type,
	Int64StringType,
	SnowflakeStringType,
	SnowflakeType,
	withOpenApiType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {EmailType} from '@fluxer/schema/src/primitives/UserValidators';
import {z} from 'zod';

const ReportStatusSchema = withOpenApiType(
	createInt32EnumType(
		[
			[0, 'PENDING', 'Report is pending review'],
			[1, 'RESOLVED', 'Report has been resolved'],
		],
		'The status of the report',
		'ReportStatus',
	),
	'ReportStatus',
);
const ReportTypeSchema = withOpenApiType(
	createInt32EnumType(
		[
			[0, 'MESSAGE', 'Report of a message'],
			[1, 'USER', 'Report of a user'],
			[2, 'GUILD', 'Report of a guild'],
		],
		'The type of entity being reported',
		'ReportType',
	),
	'ReportType',
);
const SortOrderEnum = createNamedStringLiteralUnion(
	[
		['asc', 'asc', 'Ascending order (oldest first)'],
		['desc', 'desc', 'Descending order (newest first)'],
	],
	'Sort order direction',
);
const AuditLogSortByEnum = createNamedStringLiteralUnion(
	[
		['createdAt', 'createdAt', 'Sort by creation timestamp'],
		['relevance', 'relevance', 'Sort by search relevance score'],
	],
	'Field to sort audit logs by',
);
const ReportSortByEnum = createNamedStringLiteralUnion(
	[
		['createdAt', 'createdAt', 'Sort by creation timestamp'],
		['reportedAt', 'reportedAt', 'Sort by report submission timestamp'],
		['resolvedAt', 'resolvedAt', 'Sort by resolution timestamp'],
	],
	'Field to sort reports by',
);
const ArchiveListSubjectTypeEnum = createNamedStringLiteralUnion(
	[
		['user', 'user', 'List user archives'],
		['guild', 'guild', 'List guild archives'],
		['all', 'all', 'List all archives'],
	],
	'Type of archives to list',
);
const SearchIndexTypeEnum = createNamedStringLiteralUnion(
	[
		['guilds', 'guilds', 'Guild search index'],
		['users', 'users', 'User search index'],
		['reports', 'reports', 'Report search index'],
		['audit_logs', 'audit_logs', 'Audit log search index'],
		['channel_messages', 'channel_messages', 'Channel message search index'],
		['guild_members', 'guild_members', 'Guild member search index'],
		['favorite_memes', 'favorite_memes', 'Favourite meme search index'],
		['discovery', 'discovery', 'Discovery guild search index'],
	],
	'Type of search index to refresh',
);
export const ListAuditLogsRequest = z.object({
	admin_user_id: SnowflakeType.optional().describe('Filter by admin user who performed the action'),
	target_type: createStringType(1, 64).optional().describe('Filter by target entity type'),
	target_id: z.string().optional().describe('Filter by target entity ID (user, channel, role, invite code, etc.)'),
	limit: z.number().int().min(1).max(200).default(50).describe('Maximum number of entries to return'),
	offset: z.number().int().min(0).default(0).describe('Number of entries to skip'),
});

export type ListAuditLogsRequest = z.infer<typeof ListAuditLogsRequest>;

export const SearchAuditLogsRequest = z.object({
	query: createStringType(1, 1024).optional().describe('Search query string'),
	admin_user_id: SnowflakeType.optional().describe('Filter by admin user who performed the action'),
	target_type: createStringType(1, 64).optional().describe('Filter by target entity type'),
	target_id: z.string().optional().describe('Filter by target entity ID (user, channel, role, invite code, etc.)'),
	sort_by: AuditLogSortByEnum.default('createdAt'),
	sort_order: SortOrderEnum.default('desc'),
	limit: z.number().int().min(1).max(200).default(50).describe('Maximum number of entries to return'),
	offset: z.number().int().min(0).default(0).describe('Number of entries to skip'),
});

export type SearchAuditLogsRequest = z.infer<typeof SearchAuditLogsRequest>;

export const SearchReportsRequest = z.object({
	query: createStringType(1, 1024).optional().describe('Search query string'),
	limit: z.number().int().min(1).max(200).default(50).describe('Maximum number of entries to return'),
	offset: z.number().int().min(0).default(0).describe('Number of entries to skip'),
	reporter_id: SnowflakeType.optional().describe('Filter by user who submitted the report'),
	status: ReportStatusSchema.optional(),
	report_type: ReportTypeSchema.optional(),
	category: createStringType(1, 128).optional().describe('Filter by report category'),
	reported_user_id: SnowflakeType.optional().describe('Filter by reported user ID'),
	reported_guild_id: SnowflakeType.optional().describe('Filter by reported guild ID'),
	reported_channel_id: SnowflakeType.optional().describe('Filter by reported channel ID'),
	guild_context_id: SnowflakeType.optional().describe('Filter by guild context where report was made'),
	resolved_by_admin_id: SnowflakeType.optional().describe('Filter by admin who resolved the report'),
	sort_by: ReportSortByEnum.default('reportedAt'),
	sort_order: SortOrderEnum.default('desc'),
});

export type SearchReportsRequest = z.infer<typeof SearchReportsRequest>;

export const ListReportsRequest = z.object({
	status: ReportStatusSchema.optional(),
	limit: z.number().int().min(1).max(200).optional().describe('Maximum number of reports to return'),
	offset: z.number().int().min(0).optional().describe('Number of reports to skip'),
});

export type ListReportsRequest = z.infer<typeof ListReportsRequest>;

export const ResolveReportRequest = z.object({
	report_id: SnowflakeType.describe('The ID of the report to resolve'),
	public_comment: createStringType(0, 512).optional().describe('Public comment to include with the resolution'),
});

export type ResolveReportRequest = z.infer<typeof ResolveReportRequest>;

export const RefreshSearchIndexRequest = z.object({
	index_type: SearchIndexTypeEnum,
	guild_id: SnowflakeType.optional().describe('Specific guild ID to reindex'),
	user_id: SnowflakeType.optional().describe('Specific user ID to reindex'),
});

export type RefreshSearchIndexRequest = z.infer<typeof RefreshSearchIndexRequest>;

export const GetIndexRefreshStatusRequest = z.object({
	job_id: createStringType(1, 128).describe('ID of the index refresh job to check'),
});

export type GetIndexRefreshStatusRequest = z.infer<typeof GetIndexRefreshStatusRequest>;

export const PurgeGuildAssetsRequest = z.object({
	ids: z.array(createStringType(1, 64)).max(100).describe('List of asset IDs to purge'),
});

export type PurgeGuildAssetsRequest = z.infer<typeof PurgeGuildAssetsRequest>;

export const TriggerUserArchiveRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user to archive'),
	include_attachments: z.boolean().default(false).describe('Whether to include attachment binaries in the archive'),
});

export type TriggerUserArchiveRequest = z.infer<typeof TriggerUserArchiveRequest>;

export const TriggerGuildArchiveRequest = z.object({
	guild_id: SnowflakeType.describe('ID of the guild to archive'),
	include_attachments: z.boolean().default(false).describe('Whether to include attachment binaries in the archive'),
});

export type TriggerGuildArchiveRequest = z.infer<typeof TriggerGuildArchiveRequest>;

export const ListArchivesRequest = z.object({
	subject_type: ArchiveListSubjectTypeEnum.default('all'),
	subject_id: SnowflakeType.optional().describe('Filter by specific subject ID'),
	requested_by: SnowflakeType.optional().describe('Filter by user who requested the archive'),
	limit: z.number().min(1).max(200).default(50).describe('Maximum number of archives to return'),
	include_expired: z.boolean().default(false).describe('Whether to include expired archives'),
});

export type ListArchivesRequest = z.infer<typeof ListArchivesRequest>;

const IP_OR_CIDR_REGEX = /^(?:(?:\d{1,3}\.){3}\d{1,3}(?:\/\d{1,2})?|(?:[a-fA-F0-9:]+)(?:\/\d{1,3})?)$/;
export const BanIpRequest = z.object({
	ip: createStringType(1, 45)
		.refine((value) => IP_OR_CIDR_REGEX.test(value), 'Must be a valid IPv4/IPv6 address or CIDR range')
		.describe('IPv4/IPv6 address or CIDR range to ban'),
});

export type BanIpRequest = z.infer<typeof BanIpRequest>;

export const BanEmailRequest = z.object({
	email: EmailType.describe('Email address to ban'),
});

export type BanEmailRequest = z.infer<typeof BanEmailRequest>;

export const SuspiciousEmailDomainRequest = z.object({
	domain: z
		.string()
		.min(1)
		.max(253)
		.regex(/^[a-zA-Z0-9][a-zA-Z0-9\-.]*\.[a-zA-Z]{2,}$/, 'Must be a valid domain name (e.g. example.com)')
		.describe(
			'Email domain to flag as suspicious (e.g. mail.ru). Registrants from this domain will be required to verify a phone number.',
		),
});

export type SuspiciousEmailDomainRequest = z.infer<typeof SuspiciousEmailDomainRequest>;

export const BanPhraseRequest = z.object({
	phrase: createStringType(1, 500).describe(
		'Phrase to ban. Matching is case-insensitive and also normalizes common bypass tricks such as inserted whitespace, punctuation, invisible characters, and compatibility glyphs.',
	),
});

export type BanPhraseRequest = z.infer<typeof BanPhraseRequest>;

export const BanUrlRequest = z.object({
	url: createStringType(1, 2048)
		.refine((v) => /^https?:\/\//i.test(v), 'Must be an absolute http(s) URL')
		.describe('Absolute URL to ban. Canonicalized before storage.'),
	category: createStringType(1, 64).optional().describe('Category / source slug (defaults to "manual")'),
	severity: z
		.number()
		.int()
		.min(0)
		.max(3)
		.optional()
		.describe('Severity: 0 allow, 1 warn, 2 block, 3 block+report (default 2)'),
	source_url: createStringType(1, 2048).optional().describe('Upstream source URL if imported from a feed'),
	notes: createStringType(1, 1024).optional().describe('Internal notes for audit trail'),
});

export type BanUrlRequest = z.infer<typeof BanUrlRequest>;

export const UnbanUrlRequest = z.object({
	url: createStringType(1, 2048).describe('URL to unban (must match the canonicalized form in storage)'),
});

export type UnbanUrlRequest = z.infer<typeof UnbanUrlRequest>;

export const BanUrlDomainRequest = z.object({
	domain: createStringType(1, 253)
		.refine(
			(v) => /^[a-z0-9]([a-z0-9-]*[a-z0-9])?(\.[a-z0-9]([a-z0-9-]*[a-z0-9])?)+$/i.test(v),
			'Must be a valid domain',
		)
		.describe('Domain to ban (e.g. example.com)'),
	match_subdomains: z.boolean().default(true).describe('If true, any subdomain rooted at this domain is also banned'),
	category: createStringType(1, 64).optional().describe('Category / source slug (defaults to "manual")'),
	severity: z
		.number()
		.int()
		.min(0)
		.max(3)
		.optional()
		.describe('Severity: 0 allow, 1 warn, 2 block, 3 block+report (default 2)'),
	source_url: createStringType(1, 2048).optional().describe('Upstream source URL if imported from a feed'),
	notes: createStringType(1, 1024).optional().describe('Internal notes for audit trail'),
});

export type BanUrlDomainRequest = z.infer<typeof BanUrlDomainRequest>;

export const UnbanUrlDomainRequest = z.object({
	domain: createStringType(1, 253).describe('Domain to unban'),
});

export type UnbanUrlDomainRequest = z.infer<typeof UnbanUrlDomainRequest>;

export const BanFileShaRequest = z.object({
	sha256_hex: createStringType(64, 64)
		.refine((v) => /^[0-9a-fA-F]{64}$/.test(v), 'Must be a 64-character hex SHA-256')
		.describe('SHA-256 in hex'),
	category: createStringType(1, 64).optional(),
	severity: z.number().int().min(0).max(3).optional(),
	content_type: createStringType(1, 128).optional().describe('Optional MIME type hint for observability'),
	source_url: createStringType(1, 2048).optional(),
	notes: createStringType(1, 1024).optional(),
});

export type BanFileShaRequest = z.infer<typeof BanFileShaRequest>;

export const UnbanFileShaRequest = z.object({
	sha256_hex: createStringType(64, 64).refine((v) => /^[0-9a-fA-F]{64}$/.test(v), 'Must be a 64-character hex SHA-256'),
});

export type UnbanFileShaRequest = z.infer<typeof UnbanFileShaRequest>;

export const CheckUrlBlocklistRequest = z.object({
	url: createStringType(1, 2048).describe('URL to check against the blocklist'),
});

export type CheckUrlBlocklistRequest = z.infer<typeof CheckUrlBlocklistRequest>;

export const CheckFileShaRequest = z.object({
	sha256_hex: createStringType(64, 64).refine((v) => /^[0-9a-fA-F]{64}$/.test(v), 'Must be a 64-character hex SHA-256'),
});

export type CheckFileShaRequest = z.infer<typeof CheckFileShaRequest>;

const AvatarHashShortType = createStringType(8, 10).refine(
	(v) => /^(a_)?[0-9a-fA-F]{8}$/.test(v),
	'Must be an 8-character MD5 prefix (with optional "a_" prefix)',
);
export const BanAvatarHashRequest = z.object({
	hashes: z.array(AvatarHashShortType).min(1).max(1000),
	category: createStringType(1, 64).optional(),
	severity: z.number().int().min(0).max(3).optional(),
	source_url: createStringType(1, 2048).optional(),
	reason: createStringType(1, 1024).optional(),
	notes: createStringType(1, 1024).optional(),
});

export type BanAvatarHashRequest = z.infer<typeof BanAvatarHashRequest>;

export const CheckAvatarHashRequest = z.object({
	hashes: z.array(AvatarHashShortType).min(1).max(1000),
});

export type CheckAvatarHashRequest = z.infer<typeof CheckAvatarHashRequest>;

const ProfileSubstringScopeSchema = z.enum(['username', 'global_name', 'nickname', 'bio', 'pronouns']);

export const BanUserAvatarRequest = z.object({
	reason: createStringType(1, 1024).optional(),
	notes: createStringType(1, 1024).optional(),
});

export type BanUserAvatarRequest = z.infer<typeof BanUserAvatarRequest>;

export const BanUserAvatarResponseSchema = z.object({
	hash_short: createStringType(1, 16),
});
export const BanProfileSubstringRequest = z.object({
	scope: ProfileSubstringScopeSchema,
	substrings: z.array(createStringType(1, 500)).min(1).max(1000),
	reason: createStringType(1, 1024).optional(),
	notes: createStringType(1, 1024).optional(),
});

export type BanProfileSubstringRequest = z.infer<typeof BanProfileSubstringRequest>;

const GiftCodeDurationTypeEnum = createNamedStringLiteralUnion(
	GIFT_CODE_DURATION_TYPE_DEFINITIONS,
	'Gift code duration unit',
);

export const GenerateGiftCodesRequest = z.object({
	count: z.number().int().min(1).max(MAX_GIFT_CODES_PER_REQUEST).describe('Number of gift codes to generate'),
	duration_type: GiftCodeDurationTypeEnum.describe('Duration unit for the generated gift codes'),
	duration_quantity: z
		.number()
		.int()
		.min(1)
		.max(MAX_GIFT_DURATION_QUANTITY)
		.describe('Duration quantity for the selected unit. Lifetime gifts are not supported.'),
});

export type GenerateGiftCodesRequest = z.infer<typeof GenerateGiftCodesRequest>;

const SsoConfigResponse = z.object({
	enabled: z.boolean(),
	enforced: z.boolean(),
	display_name: z.string().nullable(),
	issuer: z.string().nullable(),
	authorization_url: z.string().nullable(),
	token_url: z.string().nullable(),
	userinfo_url: z.string().nullable(),
	jwks_url: z.string().nullable(),
	client_id: z.string().nullable(),
	client_secret_set: z.boolean(),
	scope: z.string().nullable(),
	allowed_domains: z.array(z.string()).max(100),
	auto_provision: z.boolean(),
	redirect_uri: z.string().nullable(),
});

const RegistrationModeSchema = createNamedStringLiteralUnion(
	[
		['open', 'open', 'Anyone can register'],
		['approval', 'approval', 'Anyone can register, but admins must approve new accounts'],
		['closed', 'closed', 'Public registration is closed'],
	],
	'Registration mode',
);

const InstanceRegistrationConfigResponse = z.object({
	mode: RegistrationModeSchema,
	admin_registration_urls_enabled: z.boolean(),
});

const RegistrationUrlResponse = z.object({
	id: createStringType(1, 128),
	label: z.string().nullable(),
	created_by_user_id: SnowflakeStringType,
	created_at: z.iso.datetime(),
	expires_at: z.iso.datetime().nullable(),
	max_uses: z.number().int().min(1).nullable(),
	use_count: z.number().int().min(0),
	revoked_at: z.iso.datetime().nullable(),
	approval_required: z.boolean(),
	last_used_at: z.iso.datetime().nullable(),
	last_used_by_user_id: SnowflakeStringType.nullable(),
});

const PendingRegistrationResponse = z.object({
	user_id: SnowflakeStringType,
	username: z.string(),
	discriminator: z.number().int().min(0).max(9999),
	global_name: z.string().nullable(),
	email: z.string().nullable(),
	requested_at: z.iso.datetime(),
	registration_url_id: createStringType(1, 128).nullable(),
	client_ip: z.string().nullable(),
});

const InstanceRegistrationResponse = InstanceRegistrationConfigResponse.extend({
	urls: z.array(RegistrationUrlResponse),
	pending_registrations: z.array(PendingRegistrationResponse),
});

const AppPublicConfigResponse = z.object({
	branding: z.object({
		product_name: z.string(),
		icon_url: z.string().nullable(),
		symbol_url: z.string().nullable(),
		logo_url: z.string().nullable(),
		wordmark_url: z.string().nullable(),
		favicon_url: z.string().nullable(),
		theme_color: z.string().nullable(),
	}),
	setup: z.object({
		configured: z.boolean(),
	}),
	legal: z.object({
		terms_url: z.string().nullable(),
		privacy_url: z.string().nullable(),
	}),
	registration: z.object({
		collect_date_of_birth: z.boolean(),
	}),
});

const AppPublicConfigUpdateRequest = z.object({
	branding: z
		.object({
			product_name: z.string().trim().min(1).max(80).optional(),
			icon_url: z.string().trim().max(2048).nullish(),
			symbol_url: z.string().trim().max(2048).nullish(),
			logo_url: z.string().trim().max(2048).nullish(),
			wordmark_url: z.string().trim().max(2048).nullish(),
			favicon_url: z.string().trim().max(2048).nullish(),
			theme_color: z.string().trim().max(64).nullish(),
		})
		.nullish(),
	setup: z
		.object({
			configured: z.boolean().optional(),
		})
		.nullish(),
	legal: z
		.object({
			terms_url: z.string().trim().max(2048).nullish(),
			privacy_url: z.string().trim().max(2048).nullish(),
		})
		.nullish(),
	registration: z
		.object({
			collect_date_of_birth: z.boolean().optional(),
		})
		.nullish(),
});

const InstancePolicyResponse = z.object({
	single_community_enabled: z.boolean(),
	single_community_locked: z.boolean(),
	single_community_guild_id: z.string().nullable(),
	direct_messages_disabled: z.boolean(),
	direct_messages_locked: z.boolean(),
	premium_mode: z.enum(['mirror', 'everyone']),
	services: z.object({
		gif_enabled: z.boolean().nullable(),
		youtube_enabled: z.boolean().nullable(),
		bluesky_enabled: z.boolean().nullable(),
	}),
	services_resolved: z.object({
		gif_enabled: z.boolean(),
		youtube_enabled: z.boolean(),
		bluesky_enabled: z.boolean(),
	}),
	services_available: z.object({
		gif: z.boolean(),
		youtube: z.boolean(),
		bluesky: z.boolean(),
	}),
});

const CaptchaProviderSchema = z.enum(['hcaptcha', 'turnstile', 'none']);
const EmailProviderSchema = z.enum(['smtp', 'none']);

const AttachmentDecayEffectiveResponse = z.object({
	enabled: z.boolean(),
	min_size_mb: z.number().positive(),
	max_size_mb: z.number().positive(),
	max_eligible_size_mb: z.number().positive(),
	min_lifetime_days: z.number().int().positive(),
	max_lifetime_days: z.number().int().positive(),
	curve: z.number().min(0).max(1),
	renew_threshold_days: z.number().int().positive(),
	renew_window_days: z.number().int().positive(),
});

const InstanceMediaResponse = z.object({
	attachment_decay: z.object({
		enabled: z.boolean().nullable(),
		min_size_mb: z.number().positive().nullable(),
		max_size_mb: z.number().positive().nullable(),
		max_eligible_size_mb: z.number().positive().nullable(),
		min_lifetime_days: z.number().int().positive().nullable(),
		max_lifetime_days: z.number().int().positive().nullable(),
		curve: z.number().min(0).max(1).nullable(),
		renew_threshold_days: z.number().int().positive().nullable(),
		renew_window_days: z.number().int().positive().nullable(),
		effective: AttachmentDecayEffectiveResponse,
	}),
});

const InstanceIntegrationsResponse = z.object({
	gif: z.object({
		klipy_api_key_set: z.boolean(),
		effective_available: z.boolean(),
	}),
	youtube: z.object({
		api_key_set: z.boolean(),
		effective_available: z.boolean(),
	}),
	captcha: z.object({
		provider: CaptchaProviderSchema.nullable(),
		effective_provider: CaptchaProviderSchema,
		hcaptcha_site_key: z.string().nullable(),
		hcaptcha_secret_key_set: z.boolean(),
		turnstile_site_key: z.string().nullable(),
		turnstile_secret_key_set: z.boolean(),
		effective_enabled: z.boolean(),
	}),
	email: z.object({
		enabled: z.boolean().nullable(),
		effective_enabled: z.boolean(),
		provider: EmailProviderSchema.nullable(),
		effective_provider: EmailProviderSchema,
		from_email: z.string().nullable(),
		from_name: z.string().nullable(),
		smtp: z.object({
			host: z.string().nullable(),
			port: z.number().int().min(1).max(65535).nullable(),
			username: z.string().nullable(),
			password_set: z.boolean(),
			secure: z.boolean().nullable(),
		}),
	}),
	bluesky: z.object({
		enabled: z.boolean().nullable(),
		effective_enabled: z.boolean(),
		client_name: z.string().nullable(),
		client_uri: z.string().nullable(),
		logo_uri: z.string().nullable(),
		tos_uri: z.string().nullable(),
		policy_uri: z.string().nullable(),
		key_count: z.number().int().min(0),
	}),
});

export const InstanceConfigResponse = z.object({
	sso: SsoConfigResponse,
	gateway_rollout: GatewayRolloutConfigResponse,
	registration: InstanceRegistrationResponse,
	self_hosted: z.boolean(),
	app_public: AppPublicConfigResponse,
	policy: InstancePolicyResponse,
	integrations: InstanceIntegrationsResponse,
	media: InstanceMediaResponse,
});

export type InstanceConfigResponse = z.infer<typeof InstanceConfigResponse>;

export const InstanceConfigUpdateRequest = z.object({
	gateway_rollout: GatewayRolloutConfigUpdateRequest.nullish(),
	registration: z
		.object({
			mode: RegistrationModeSchema.optional(),
			admin_registration_urls_enabled: z.boolean().optional(),
		})
		.nullish(),
	sso: z
		.object({
			enabled: z.boolean().optional(),
			enforced: z.boolean().optional(),
			display_name: z.string().nullish(),
			issuer: z.string().nullish(),
			authorization_url: z.string().nullish(),
			token_url: z.string().nullish(),
			userinfo_url: z.string().nullish(),
			jwks_url: z.string().nullish(),
			client_id: z.string().nullish(),
			client_secret: z.string().nullish(),
			scope: z.string().nullish(),
			allowed_domains: z.array(z.string()).max(100).optional(),
			auto_provision: z.boolean().optional(),
		})
		.nullish(),
	app_public: AppPublicConfigUpdateRequest.nullish(),
	integrations: z
		.object({
			gif: z
				.object({
					klipy_api_key: z.string().trim().max(4096).nullish(),
				})
				.nullish(),
			youtube: z
				.object({
					api_key: z.string().trim().max(4096).nullish(),
				})
				.nullish(),
			captcha: z
				.object({
					provider: CaptchaProviderSchema.nullish(),
					hcaptcha_site_key: z.string().trim().max(4096).nullish(),
					hcaptcha_secret_key: z.string().trim().max(4096).nullish(),
					turnstile_site_key: z.string().trim().max(4096).nullish(),
					turnstile_secret_key: z.string().trim().max(4096).nullish(),
				})
				.nullish(),
			email: z
				.object({
					enabled: z.boolean().nullish(),
					provider: EmailProviderSchema.nullish(),
					from_email: z.string().trim().max(320).nullish(),
					from_name: z.string().trim().max(120).nullish(),
					smtp: z
						.object({
							host: z.string().trim().max(255).nullish(),
							port: z.number().int().min(1).max(65535).nullish(),
							username: z.string().trim().max(320).nullish(),
							password: z.string().trim().max(4096).nullish(),
							secure: z.boolean().nullish(),
						})
						.nullish(),
				})
				.nullish(),
			bluesky: z
				.object({
					enabled: z.boolean().nullish(),
					client_name: z.string().trim().max(120).nullish(),
					client_uri: z.string().trim().max(2048).nullish(),
					logo_uri: z.string().trim().max(2048).nullish(),
					tos_uri: z.string().trim().max(2048).nullish(),
					policy_uri: z.string().trim().max(2048).nullish(),
					keys: z
						.array(
							z.object({
								kid: z.string().trim().min(1).max(255),
								private_key: z.string().trim().max(10000).nullish(),
							}),
						)
						.max(8)
						.optional(),
				})
				.nullish(),
		})
		.nullish(),
	media: z
		.object({
			attachment_decay: z
				.object({
					enabled: z.boolean().nullish(),
					min_size_mb: z.number().positive().nullish(),
					max_size_mb: z.number().positive().nullish(),
					max_eligible_size_mb: z.number().positive().nullish(),
					min_lifetime_days: z.number().int().positive().nullish(),
					max_lifetime_days: z.number().int().positive().nullish(),
					curve: z.number().min(0).max(1).nullish(),
					renew_threshold_days: z.number().int().positive().nullish(),
					renew_window_days: z.number().int().positive().nullish(),
				})
				.nullish(),
		})
		.nullish(),
	policy: z
		.object({
			single_community_enabled: z.boolean().optional(),
			single_community_name: z.string().trim().min(1).max(100).optional(),
			direct_messages_disabled: z.boolean().optional(),
			premium_mode: z.enum(['mirror', 'everyone']).optional(),
			services: z
				.object({
					gif_enabled: z.boolean().nullish(),
					youtube_enabled: z.boolean().nullish(),
					bluesky_enabled: z.boolean().nullish(),
				})
				.nullish(),
		})
		.nullish(),
});

export type InstanceConfigUpdateRequest = z.infer<typeof InstanceConfigUpdateRequest>;

export const BrandingAssetUploadRequest = z.object({
	kind: z.enum(['icon', 'symbol', 'logo', 'wordmark', 'favicon']),
	image: z.string().max(16_000_000).nullish(),
});

export type BrandingAssetUploadRequest = z.infer<typeof BrandingAssetUploadRequest>;

export const InstanceEmailSmtpTestRequest = z.object({
	host: z.string().trim().min(1).max(255),
	port: z.number().int().min(1).max(65535),
	username: z.string().trim().min(1).max(320),
	password: z.string().trim().min(1).max(4096),
	secure: z.boolean().default(true),
});

export type InstanceEmailSmtpTestRequest = z.infer<typeof InstanceEmailSmtpTestRequest>;

export const InstanceEmailSmtpTestResponse = z.object({
	ok: z.boolean(),
	error: z.string().nullable(),
});

export type InstanceEmailSmtpTestResponse = z.infer<typeof InstanceEmailSmtpTestResponse>;

export const CreateRegistrationUrlRequest = z.object({
	label: z.string().trim().min(1).max(120).nullish(),
	expires_at: z.iso.datetime().nullish(),
	max_uses: z.number().int().min(1).max(1000000).nullish(),
	approval_required: z.boolean().default(false),
});

export type CreateRegistrationUrlRequest = z.infer<typeof CreateRegistrationUrlRequest>;

export const CreateRegistrationUrlResponse = z.object({
	registration_url: RegistrationUrlResponse,
	code: createStringType(1, 256),
	url: z.string(),
});

export type CreateRegistrationUrlResponse = z.infer<typeof CreateRegistrationUrlResponse>;

export const RegistrationUrlActionRequest = z.object({
	id: createStringType(1, 128),
});

export type RegistrationUrlActionRequest = z.infer<typeof RegistrationUrlActionRequest>;

export const PendingRegistrationActionRequest = z.object({
	user_id: SnowflakeStringType,
});

export type PendingRegistrationActionRequest = z.infer<typeof PendingRegistrationActionRequest>;

const LimitKeySchema = z.enum(LIMIT_KEYS);
const LimitFilterSchema = z.object({
	traits: z.array(z.string()).optional().describe('Trait filters that must match for the rule to apply'),
	guildFeatures: z.array(z.string()).optional().describe('Guild feature flags required for the rule to apply'),
});
const LimitRuleSchema = z.object({
	id: z.string().min(1).describe('Unique rule identifier'),
	filters: LimitFilterSchema.optional().describe('Optional filters that scope the rule'),
	limits: z
		.record(z.string(), z.number().min(0))
		.refine(
			(limits) => {
				const limitKeys = Object.keys(limits);
				return limitKeys.every((key) => (LIMIT_KEYS as ReadonlyArray<string>).includes(key));
			},
			{message: 'Invalid limit key detected'},
		)
		.describe('Per-limit key values'),
});
const LimitConfigSchema = z.object({
	traitDefinitions: z.array(z.string()).optional().describe('Trait definitions used by rules'),
	rules: z.array(LimitRuleSchema).describe('Limit rules'),
});
export const LimitConfigUpdateRequest = z.object({
	limit_config: LimitConfigSchema.describe('New limit configuration snapshot'),
});

export type LimitConfigUpdateRequest = z.infer<typeof LimitConfigUpdateRequest>;

export const SendSystemDmRequest = z.object({
	content: z.string().min(1).max(4000).describe('Message content to send to each recipient'),
	user_ids: z
		.array(SnowflakeType)
		.min(1)
		.max(10000)
		.describe('Recipient user IDs. Each receives the same content as a system DM.'),
});

export type SendSystemDmRequest = z.infer<typeof SendSystemDmRequest>;

export const SendSystemDmResponse = z.object({
	recipient_count: Int32Type.describe('Number of recipients the worker job was queued to deliver to'),
});

export type SendSystemDmResponse = z.infer<typeof SendSystemDmResponse>;

export const CreateAdminApiKeyRequest = z.object({
	name: z
		.string()
		.min(1)
		.max(100)
		.refine((value) => value.trim().length > 0, 'Name cannot be empty')
		.describe('Display name for the API key'),
	expires_in_days: z.number().int().min(1).max(365).optional().describe('Number of days until the key expires'),
	acls: z.array(z.string()).max(100).describe('List of access control permissions for the key'),
});

export type CreateAdminApiKeyRequest = z.infer<typeof CreateAdminApiKeyRequest>;

export const CreateAdminApiKeyResponse = z.object({
	key_id: z.string().describe('Unique identifier for the API key'),
	key: z.string().describe('The generated API key secret (only shown once)'),
	name: z.string().describe('Display name for the API key'),
	created_at: z.string().describe('ISO 8601 timestamp when the key was created'),
	expires_at: z.string().nullable().describe('ISO 8601 timestamp when the key expires, or null if no expiration'),
	acls: z.array(z.string()).max(100).describe('List of access control permissions for the key'),
});

export type CreateAdminApiKeyResponse = z.infer<typeof CreateAdminApiKeyResponse>;

export const ListAdminApiKeyResponse = z.object({
	key_id: z.string().describe('Unique identifier for the API key'),
	name: z.string().describe('Display name for the API key'),
	created_at: z.string().describe('ISO 8601 timestamp when the key was created'),
	last_used_at: z.string().nullable().describe('ISO 8601 timestamp when the key was last used, or null if never used'),
	expires_at: z.string().nullable().describe('ISO 8601 timestamp when the key expires, or null if no expiration'),
	created_by_user_id: SnowflakeStringType.describe('User ID of the admin who created this key'),
	acls: z.array(z.string()).max(100).describe('List of access control permissions for the key'),
});

export type ListAdminApiKeyResponse = z.infer<typeof ListAdminApiKeyResponse>;

export const SearchGuildsResponse = z.object({
	guilds: z.array(GuildAdminResponse),
	total: z.number(),
});

export type SearchGuildsResponse = z.infer<typeof SearchGuildsResponse>;

export const SearchUsersResponse = z.object({
	users: z.array(UserAdminResponseSchema),
	total: z.number(),
});

export type SearchUsersResponse = z.infer<typeof SearchUsersResponse>;

export const RefreshSearchIndexResponse = z.object({
	success: z.literal(true),
	job_id: z.string(),
});

export type RefreshSearchIndexResponse = z.infer<typeof RefreshSearchIndexResponse>;

const IndexRefreshStatusEnum = createNamedStringLiteralUnion(
	[
		['in_progress', 'in_progress', 'Index refresh is currently in progress'],
		['completed', 'completed', 'Index refresh completed successfully'],
		['failed', 'failed', 'Index refresh failed'],
	],
	'Current status of the index refresh job',
);
export const IndexRefreshStatusResponse = z.union([
	z.object({
		status: z.literal('not_found').describe('Job was not found'),
	}),
	z.object({
		status: IndexRefreshStatusEnum,
		index_type: z.string().describe('Type of index being refreshed'),
		total: z.number().optional().describe('Total number of items to index'),
		indexed: z.number().optional().describe('Number of items indexed so far'),
		started_at: z.string().optional().describe('ISO 8601 timestamp when the job started'),
		completed_at: z.string().optional().describe('ISO 8601 timestamp when the job completed'),
		failed_at: z.string().optional().describe('ISO 8601 timestamp when the job failed'),
		error: z.string().optional().describe('Error message if the job failed'),
	}),
]);

export type IndexRefreshStatusResponse = z.infer<typeof IndexRefreshStatusResponse>;

const AdminArchiveSubjectTypeSchema = createNamedStringLiteralUnion(
	[
		['user', 'user', 'User data archive'],
		['guild', 'guild', 'Guild data archive'],
	],
	'Type of subject being archived',
);
export const AdminArchiveResponseSchema = z.object({
	archive_id: SnowflakeStringType,
	subject_type: AdminArchiveSubjectTypeSchema,
	subject_id: SnowflakeStringType,
	requested_by: SnowflakeStringType,
	requested_at: z.string(),
	started_at: z.string().nullable(),
	completed_at: z.string().nullable(),
	failed_at: z.string().nullable(),
	file_size: createStringType(1, 64).nullable(),
	progress_percent: z.number(),
	progress_step: createStringType(1, 256).nullable(),
	error_message: createStringType(1, 4000).nullable(),
	download_url_expires_at: z.string().nullable(),
	expires_at: z.string().nullable(),
});
export const ListArchivesResponseSchema = z.object({
	archives: z.array(AdminArchiveResponseSchema),
});
export const GetArchiveResponseSchema = z.object({
	archive: AdminArchiveResponseSchema.nullable(),
});
export const DownloadUrlResponseSchema = z.object({
	downloadUrl: createStringType(1, 2048),
	expiresAt: z.string(),
});
const GuildAssetTypeEnum = createNamedStringLiteralUnion(
	[
		['emoji', 'emoji', 'Custom emoji asset'],
		['sticker', 'sticker', 'Custom sticker asset'],
		['unknown', 'unknown', 'Unknown asset type'],
	],
	'Type of guild asset',
);
export const PurgeGuildAssetResultSchema = z.object({
	id: SnowflakeStringType.describe('Unique identifier of the asset'),
	asset_type: GuildAssetTypeEnum,
	found_in_db: z.boolean().describe('Whether the asset was found in the database'),
	guild_id: SnowflakeStringType.nullable().describe('ID of the guild the asset belongs to'),
	guild_nsfw_level: NSFWLevelSchema.nullable().describe('NSFW level of the guild the asset belongs to'),
});

export type PurgeGuildAssetResult = z.infer<typeof PurgeGuildAssetResultSchema>;

export const PurgeGuildAssetErrorSchema = z.object({
	id: SnowflakeStringType,
	error: createStringType(1, 4000),
});

export type PurgeGuildAssetError = z.infer<typeof PurgeGuildAssetErrorSchema>;

export const PurgeGuildAssetsResponseSchema = z.object({
	processed: z.array(PurgeGuildAssetResultSchema),
	errors: z.array(PurgeGuildAssetErrorSchema),
});

export type PurgeGuildAssetsResponse = z.infer<typeof PurgeGuildAssetsResponseSchema>;

const AdminAuditLogUserSummarySchema = z.object({
	id: SnowflakeStringType,
	username: z.string(),
	discriminator: z.string(),
	global_name: z.string().nullable(),
});
const AdminAuditLogGuildSummarySchema = z.object({
	id: SnowflakeStringType,
	name: z.string(),
});
const AdminAuditLogChannelSummarySchema = z.object({
	id: SnowflakeStringType,
	name: z.string().nullable(),
	type: ChannelTypeSchema,
	guild_id: SnowflakeStringType.nullable(),
});
const AdminAuditLogResponseSchema = z.object({
	log_id: SnowflakeStringType,
	admin_user_id: SnowflakeStringType,
	admin_user: AdminAuditLogUserSummarySchema.nullable(),
	target_type: createStringType(1, 256),
	target_id: z.string().describe('The ID of the affected entity (user, channel, role, invite code, etc.)'),
	target_user: AdminAuditLogUserSummarySchema.nullable(),
	target_guild: AdminAuditLogGuildSummarySchema.nullable(),
	target_channel: AdminAuditLogChannelSummarySchema.nullable(),
	related_users: z.record(SnowflakeStringType, AdminAuditLogUserSummarySchema),
	related_guilds: z.record(SnowflakeStringType, AdminAuditLogGuildSummarySchema),
	related_channels: z.record(SnowflakeStringType, AdminAuditLogChannelSummarySchema),
	action: createStringType(1, 256),
	audit_log_reason: createStringType(1, 4000).nullable(),
	metadata: z.record(createStringType(1, 256), createStringType(0, 4000)),
	created_at: z.string(),
});
export const AuditLogsListResponseSchema = z.object({
	logs: z.array(AdminAuditLogResponseSchema),
	total: z.number(),
});
export const BanCheckResponseSchema = z.object({
	banned: z.boolean(),
});
export const BulkJobResponse = z.object({
	job_id: SnowflakeStringType,
});
export const BulkBanFileShasRequest = z.object({
	sha256_list: z.array(createStringType(64, 64)).min(1).max(10000).describe('Array of SHA-256 hex strings to ban'),
});

export type BulkBanFileShasRequest = z.infer<typeof BulkBanFileShasRequest>;

const NcmecSubmissionStatusEnum = createNamedStringLiteralUnion(
	[
		['not_submitted', 'not_submitted', 'Report has not been submitted to NCMEC'],
		['submitted', 'submitted', 'Report has been submitted to NCMEC'],
		['failed', 'failed', 'Report submission to NCMEC failed'],
	],
	'NCMEC submission status',
);
export const NcmecAttachmentSubmitResultResponse = z.object({
	success: z.literal(true),
	ncmec_report_id: createStringType(1, 256),
	audit_log_reason: createStringType(1, 4000),
});
export const CodesResponse = z.object({
	codes: z.array(z.string()),
});
export const GuildMemoryStatsResponse = z.object({
	guilds: z
		.array(
			z.object({
				node_id: createStringType(1, 256),
				guild_id: SnowflakeStringType.nullable(),
				guild_name: createStringType(1, 100),
				guild_icon: createStringType(1, 256).nullable(),
				nsfw_level: NSFWLevelSchema.nullable(),
				memory: Int64StringType,
				member_count: Int32Type,
				session_count: Int32Type,
				presence_count: Int32Type,
			}),
		)
		.max(1000),
});

export type GuildMemoryStatsResponse = z.infer<typeof GuildMemoryStatsResponse>;

export const ReloadGuildsRequest = z.object({
	guild_ids: z.array(SnowflakeType).max(1000).describe('List of guild IDs to reload'),
});

export type ReloadGuildsRequest = z.infer<typeof ReloadGuildsRequest>;

export const ReloadAllGuildsResponse = z.object({
	count: Int32Type,
});

export type ReloadAllGuildsResponse = z.infer<typeof ReloadAllGuildsResponse>;

export const NodeStatsResponse = z.object({
	status: createStringType(1, 256),
	sessions: Int32Type,
	guilds: Int32Type,
	presences: Int32Type,
	calls: Int32Type,
	memory: z.object({
		total: Int64StringType,
		processes: Int64StringType,
		system: Int64StringType,
	}),
	process_count: Int32Type,
	process_limit: Int32Type,
	uptime_seconds: Int32Type,
	node_count: Int32Type,
	nodes: z
		.array(
			z.object({
				node_id: createStringType(1, 256),
				status: createStringType(1, 256),
				sessions: Int32Type,
				guilds: Int32Type,
				presences: Int32Type,
				calls: Int32Type,
				memory: z.object({
					total: Int64StringType,
					processes: Int64StringType,
					system: Int64StringType,
				}),
				process_count: Int32Type,
				process_limit: Int32Type,
				uptime_seconds: Int32Type,
			}),
		)
		.max(1000),
});

export type NodeStatsResponse = z.infer<typeof NodeStatsResponse>;

export const GatewayVoiceStateCountsResponse = z.object({
	total_voice_states: Int32Type,
	regions: z
		.array(
			z.object({
				region_id: createStringType(1, 64),
				voice_state_count: Int32Type,
			}),
		)
		.max(1000),
	servers: z
		.array(
			z.object({
				server_id: createStringType(1, 128),
				voice_state_count: Int32Type,
			}),
		)
		.max(5000),
});

export type GatewayVoiceStateCountsResponse = z.infer<typeof GatewayVoiceStateCountsResponse>;

export const SuccessResponse = z.object({
	success: z.boolean(),
});
const AdminGuildResponseSchema = z.object({
	id: SnowflakeStringType,
	name: createStringType(1, 100),
	features: z.array(createStringType(1, 256)).max(100),
	owner_id: SnowflakeStringType,
	icon: createStringType(1, 256).nullable(),
	banner: createStringType(1, 256).nullable(),
	member_count: Int32Type,
	nsfw_level: NSFWLevelSchema.optional(),
	nsfw: z.boolean().optional(),
	content_warning_level: ContentWarningLevelSchema.optional(),
	content_warning_text: createStringType(0, CONTENT_WARNING_TEXT_MAX_LENGTH).nullable().optional(),
});
const AdminGuildChannelSummarySchema = z.object({
	id: SnowflakeStringType,
	name: createStringType(1, 100).nullable(),
	type: ChannelTypeSchema,
	position: Int32Type,
	parent_id: SnowflakeStringType.nullable(),
	nsfw: z.boolean().nullable(),
	nsfw_override: z.boolean().nullable().optional(),
	content_warning_level: ContentWarningLevelSchema.optional(),
	content_warning_text: createStringType(0, CONTENT_WARNING_TEXT_MAX_LENGTH).nullable().optional(),
	url: createStringType(1, 2048).nullable(),
});
const AdminGuildRoleSummarySchema = z.object({
	id: SnowflakeStringType,
	name: createStringType(1, 100),
	color: Int32Type,
	position: Int32Type,
	permissions: PermissionStringType.describe('fluxer:PermissionStringType The role permissions bitfield'),
	hoist: z.boolean(),
	mentionable: z.boolean(),
});
const AdminLookupGuildSchema = z.object({
	id: SnowflakeStringType,
	owner_id: SnowflakeStringType,
	owner_username: z.string().nullable(),
	owner_global_name: z.string().nullable(),
	owner_discriminator: z.string().nullable(),
	name: createStringType(1, 100),
	vanity_url_code: createStringType(1, 256).nullable(),
	icon: createStringType(1, 256).nullable(),
	banner: createStringType(1, 256).nullable(),
	splash: createStringType(1, 256).nullable(),
	embed_splash: createStringType(1, 256).nullable(),
	features: z.array(createStringType(1, 256)).max(100),
	verification_level: GuildVerificationLevelSchema,
	mfa_level: GuildMFALevelSchema,
	nsfw_level: NSFWLevelSchema,
	nsfw: z.boolean().optional(),
	content_warning_level: ContentWarningLevelSchema.optional(),
	content_warning_text: createStringType(0, CONTENT_WARNING_TEXT_MAX_LENGTH).nullable().optional(),
	explicit_content_filter: GuildExplicitContentFilterSchema,
	default_message_notifications: DefaultMessageNotificationsSchema,
	afk_channel_id: SnowflakeStringType.nullable(),
	afk_timeout: Int32Type,
	system_channel_id: SnowflakeStringType.nullable(),
	system_channel_flags: createBitflagInt32Type(
		SystemChannelFlags,
		SystemChannelFlagsDescriptions,
		'System channel message flags',
		'SystemChannelFlags',
	),
	rules_channel_id: SnowflakeStringType.nullable(),
	disabled_operations: Int32Type,
	member_count: Int32Type,
	channels: z.array(AdminGuildChannelSummarySchema).max(500),
	roles: z.array(AdminGuildRoleSummarySchema).max(250),
});
export const LookupGuildResponse = z.object({
	guild: AdminLookupGuildSchema.nullable(),
});
export const ListGuildMembersResponse = z.object({
	members: z.array(z.lazy(() => GuildMemberResponse)),
	total: Int32Type,
	limit: Int32Type,
	offset: Int32Type,
});
const GuildAssetItemSchema = z.object({
	id: SnowflakeStringType,
	name: createStringType(1, 100),
	animated: z.boolean(),
	creator_id: SnowflakeStringType,
	media_url: createStringType(1, 2048),
});

export const ListGuildEmojisResponse = z.object({
	guild_id: SnowflakeStringType,
	emojis: z.array(GuildAssetItemSchema).max(500),
});

export type ListGuildEmojisResponse = z.infer<typeof ListGuildEmojisResponse>;

export const ListGuildStickersResponse = z.object({
	guild_id: SnowflakeStringType,
	stickers: z.array(GuildAssetItemSchema).max(500),
});

export type ListGuildStickersResponse = z.infer<typeof ListGuildStickersResponse>;

export const GuildUpdateResponse = z.object({
	guild: AdminGuildResponseSchema,
});
const AdminMessageAttachmentSchema = z.object({
	id: SnowflakeStringType,
	filename: createStringType(1, 256),
	url: createStringType(1, 2048),
	nsfw: z.boolean().nullable(),
	content_type: z.string().nullable(),
	width: Int32Type.nullable(),
	height: Int32Type.nullable(),
	size: Int32Type.nullable().optional(),
	ncmec_status: NcmecSubmissionStatusEnum,
	ncmec_report_id: createStringType(1, 256).nullable(),
	ncmec_failure_reason: createStringType(1, 4000).nullable(),
});
export const AdminMessageSchema = z.object({
	id: SnowflakeStringType,
	channel_id: SnowflakeStringType,
	channel_name: z.string().nullable(),
	channel_nsfw: z.boolean().nullable(),
	channel_content_warning_level: ContentWarningLevelSchema.nullable().optional(),
	channel_content_warning_text: createStringType(0, CONTENT_WARNING_TEXT_MAX_LENGTH).nullable().optional(),
	guild_id: SnowflakeStringType.nullable(),
	guild_name: z.string().nullable(),
	guild_nsfw_level: NSFWLevelSchema.nullable(),
	guild_nsfw: z.boolean().nullable().optional(),
	guild_content_warning_level: ContentWarningLevelSchema.nullable().optional(),
	guild_content_warning_text: createStringType(0, CONTENT_WARNING_TEXT_MAX_LENGTH).nullable().optional(),
	author_id: SnowflakeStringType,
	author_username: createStringType(1, 100),
	author_global_name: z.string().nullable(),
	author_discriminator: createStringType(1, 10),
	author_avatar: z.string().nullable(),
	content: createStringType(0, 4000),
	timestamp: z.string(),
	attachments: z.array(AdminMessageAttachmentSchema).max(10),
	user_prior_ncmec_report_ids: z.array(createStringType(1, 256)).max(100).optional(),
});
export const LookupMessageResponse = z.object({
	messages: z.array(AdminMessageSchema).max(100),
	message_responses: z.array(MessageResponseSchema).max(100).optional(),
	message_id: SnowflakeStringType.nullable(),
});
export const DeleteMessageResponse = z.object({
	success: z.literal(true),
});
const MessageShredStatusNotFoundResponse = z.object({
	status: z.literal('not_found'),
});
const MessageShredStatusProgressResponse = z.object({
	status: createNamedStringLiteralUnion(
		[
			['in_progress', 'in_progress', 'Shredding is currently running'],
			['completed', 'completed', 'Shredding completed successfully'],
			['failed', 'failed', 'Shredding failed'],
		],
		'Current message shred job status',
	),
	requested: Int32Type,
	total: Int32Type,
	processed: Int32Type,
	skipped: Int32Type,
	started_at: z.string().optional(),
	completed_at: z.string().optional(),
	failed_at: z.string().optional(),
	error: createStringType(1, 4000).optional(),
});
export const MessageShredStatusResponse = z.union([
	MessageShredStatusNotFoundResponse,
	MessageShredStatusProgressResponse,
]);
const ReportMessageContextSchema = z.object({
	id: SnowflakeStringType,
	channel_id: SnowflakeStringType,
	channel_nsfw: z.boolean().nullable(),
	channel_content_warning_level: ContentWarningLevelSchema.nullable().optional(),
	channel_content_warning_text: createStringType(0, CONTENT_WARNING_TEXT_MAX_LENGTH).nullable().optional(),
	guild_id: SnowflakeStringType.nullable(),
	guild_nsfw_level: NSFWLevelSchema.nullable(),
	guild_nsfw: z.boolean().nullable().optional(),
	guild_content_warning_level: ContentWarningLevelSchema.nullable().optional(),
	guild_content_warning_text: createStringType(0, CONTENT_WARNING_TEXT_MAX_LENGTH).nullable().optional(),
	content: z.string(),
	timestamp: z.string(),
	attachments: z.array(AdminMessageAttachmentSchema),
	author_id: SnowflakeStringType,
	author_username: z.string(),
	author_global_name: z.string().nullable(),
	author_discriminator: z.string(),
	author_avatar: z.string().nullable(),
	user_prior_ncmec_report_ids: z.array(createStringType(1, 256)).max(100).optional(),
});
export const ReportAdminResponseSchema = z.object({
	report_id: SnowflakeStringType,
	reporter_id: SnowflakeStringType.nullable(),
	reporter_tag: z.string().nullable(),
	reporter_username: z.string().nullable(),
	reporter_global_name: z.string().nullable(),
	reporter_discriminator: z.string().nullable(),
	reporter_email: z.string().nullable(),
	reporter_full_legal_name: z.string().nullable(),
	reporter_country_of_residence: z.string().nullable(),
	reported_at: z.string(),
	status: ReportStatusSchema,
	report_type: ReportTypeSchema,
	category: z.string().nullable(),
	additional_info: z.string().nullable(),
	reported_user_id: SnowflakeStringType.nullable(),
	reported_user_tag: z.string().nullable(),
	reported_user_username: z.string().nullable(),
	reported_user_global_name: z.string().nullable(),
	reported_user_discriminator: z.string().nullable(),
	reported_user_avatar_hash: z.string().nullable(),
	reported_guild_id: SnowflakeStringType.nullable(),
	reported_guild_name: z.string().nullable(),
	reported_guild_icon_hash: z.string().nullable(),
	reported_message_id: SnowflakeStringType.nullable(),
	reported_channel_id: SnowflakeStringType.nullable(),
	reported_channel_name: z.string().nullable(),
	reported_channel_nsfw: z.boolean().nullable(),
	reported_guild_invite_code: z.string().nullable(),
	reported_guild_nsfw_level: NSFWLevelSchema.nullable(),
	reported_guild_nsfw: z.boolean().nullable().optional(),
	reported_guild_content_warning_level: ContentWarningLevelSchema.nullable().optional(),
	reported_guild_content_warning_text: createStringType(0, CONTENT_WARNING_TEXT_MAX_LENGTH).nullable().optional(),
	reported_channel_nsfw_override: z.boolean().nullable().optional(),
	reported_channel_content_warning_level: ContentWarningLevelSchema.nullable().optional(),
	reported_channel_content_warning_text: createStringType(0, CONTENT_WARNING_TEXT_MAX_LENGTH).nullable().optional(),
	reported_channel_effective_nsfw: z.boolean().nullable().optional(),
	reported_channel_effective_content_warning_level: ContentWarningLevelSchema.nullable().optional(),
	reported_channel_effective_content_warning_text: createStringType(0, CONTENT_WARNING_TEXT_MAX_LENGTH)
		.nullable()
		.optional(),
	resolved_at: z.string().nullable(),
	resolved_by_admin_id: SnowflakeStringType.nullable(),
	public_comment: z.string().nullable(),
	mutual_dm_channel_id: SnowflakeStringType.nullable().optional(),
	message_context: z.array(ReportMessageContextSchema).optional(),
	message_responses: z.array(MessageResponseSchema).optional(),
});
export const ListReportsResponse = z.object({
	reports: z.array(ReportAdminResponseSchema),
});
export const ResolveReportResponse = z.object({
	report_id: SnowflakeStringType,
	status: ReportStatusSchema,
	resolved_at: z.string().nullable(),
	public_comment: z.string().nullable(),
});
export const SearchReportsResponse = z.object({
	reports: z.array(ReportAdminResponseSchema),
	total: z.number(),
	offset: z.number(),
	limit: z.number(),
});
const LimitKeyMetadataSchema = z.object({
	key: z.string(),
	label: z.string(),
	description: z.string(),
	category: z.string(),
	scope: z.string(),
	isToggle: z.boolean(),
	unit: z.enum(['bytes', 'count']).optional(),
	min: z.number().optional(),
	max: z.number().optional(),
});
export const LimitConfigGetResponse = z.object({
	limit_config: LimitConfigSchema.extend({
		traitDefinitions: z.array(z.string()),
		rules: z.array(
			LimitRuleSchema.extend({
				modifiedFields: z.array(z.string()).optional(),
			}),
		),
	}),
	limit_config_json: z.string(),
	self_hosted: z.boolean(),
	defaults: z.record(z.string(), z.partialRecord(LimitKeySchema, z.number())),
	metadata: z.record(LimitKeySchema, LimitKeyMetadataSchema),
	categories: z.record(z.string(), z.string()),
	limit_keys: z.array(z.string()),
	bounds: z.record(z.string(), z.object({min: z.number(), max: z.number()})).optional(),
});
export const DeleteApiKeyResponse = z.object({
	success: z.literal(true),
});
export const HeapSnapshotResponse = z.object({
	success: z.literal(true),
	filename: z.string().describe('Name of the heap snapshot file'),
	size_bytes: z.number().describe('Size of the heap snapshot in bytes'),
});
