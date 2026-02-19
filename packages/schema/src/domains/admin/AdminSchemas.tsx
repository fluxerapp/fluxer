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

import {SystemChannelFlags, SystemChannelFlagsDescriptions} from '@fluxer/constants/src/GuildConstants';
import {LIMIT_KEYS} from '@fluxer/constants/src/LimitConfigMetadata';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {GuildAdminResponse} from '@fluxer/schema/src/domains/admin/AdminGuildSchemas';
import {UserAdminResponseSchema} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import {ChannelTypeSchema} from '@fluxer/schema/src/primitives/ChannelValidators';
import {
	DefaultMessageNotificationsSchema,
	GuildExplicitContentFilterSchema,
	GuildMFALevelSchema,
	GuildVerificationLevelSchema,
	NSFWLevelSchema,
} from '@fluxer/schema/src/primitives/GuildValidators';
import {PermissionStringType} from '@fluxer/schema/src/primitives/PermissionValidators';
import {createQueryIntegerType} from '@fluxer/schema/src/primitives/QueryValidators';
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
import {EmailType, PhoneNumberType} from '@fluxer/schema/src/primitives/UserValidators';
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
});

export type TriggerUserArchiveRequest = z.infer<typeof TriggerUserArchiveRequest>;

export const TriggerGuildArchiveRequest = z.object({
	guild_id: SnowflakeType.describe('ID of the guild to archive'),
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

export const BanPhoneRequest = z.object({
	phone: PhoneNumberType.describe('Phone number to ban'),
});

export type BanPhoneRequest = z.infer<typeof BanPhoneRequest>;

export const ListBansRequest = z.object({
	limit: z.number().int().min(1).max(500).default(200).describe('Maximum number of bans to return'),
});

export type ListBansRequest = z.infer<typeof ListBansRequest>;

export const IpBanListEntrySchema = z.object({
	ip: createStringType(1, 45).describe('Banned IPv4/IPv6 address or CIDR range'),
	reverse_dns: z.string().nullable().describe('Reverse DNS hostname for the IP, if available'),
});

export type IpBanListEntry = z.infer<typeof IpBanListEntrySchema>;

export const ListIpBansResponseSchema = z.object({
	bans: z.array(IpBanListEntrySchema).max(500),
});

export type ListIpBansResponse = z.infer<typeof ListIpBansResponseSchema>;

export const ListEmailBansResponseSchema = z.object({
	bans: z.array(EmailType).max(500),
});

export type ListEmailBansResponse = z.infer<typeof ListEmailBansResponseSchema>;

export const ListPhoneBansResponseSchema = z.object({
	bans: z.array(PhoneNumberType).max(500),
});

export type ListPhoneBansResponse = z.infer<typeof ListPhoneBansResponseSchema>;

export const LegalHoldRequest = z.object({
	expires_at: z.string().optional().describe('ISO 8601 timestamp when the legal hold expires'),
});

export type LegalHoldRequest = z.infer<typeof LegalHoldRequest>;

const MAX_CODES_PER_REQUEST = 100;

export const GiftProductTypeEnum = createNamedStringLiteralUnion(
	[
		['gift_1_month', 'gift_1_month', 'One month gift subscription'],
		['gift_1_year', 'gift_1_year', 'One year gift subscription'],
	],
	'Type of gift subscription product',
);

export type GiftProductType = z.infer<typeof GiftProductTypeEnum>;

export const GenerateGiftCodesRequest = z.object({
	count: z.number().int().min(1).max(MAX_CODES_PER_REQUEST).describe('Number of gift codes to generate'),
	product_type: GiftProductTypeEnum.describe('Type of gift subscription'),
});

export type GenerateGiftCodesRequest = z.infer<typeof GenerateGiftCodesRequest>;

export const SsoConfigResponse = z.object({
	enabled: z.boolean(),
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

export type SsoConfigResponse = z.infer<typeof SsoConfigResponse>;

export const InstanceConfigResponse = z.object({
	manual_review_enabled: z.boolean(),
	manual_review_schedule_enabled: z.boolean(),
	manual_review_schedule_start_hour_utc: Int32Type,
	manual_review_schedule_end_hour_utc: Int32Type,
	manual_review_active_now: z.boolean(),
	registration_alerts_webhook_url: z.string().nullable(),
	system_alerts_webhook_url: z.string().nullable(),
	sso: SsoConfigResponse,
	self_hosted: z.boolean(),
});

export type InstanceConfigResponse = z.infer<typeof InstanceConfigResponse>;

export const InstanceConfigUpdateRequest = z.object({
	manual_review_enabled: z.boolean().optional(),
	manual_review_schedule_enabled: z.boolean().optional(),
	manual_review_schedule_start_hour_utc: z.number().int().min(0).max(23).optional(),
	manual_review_schedule_end_hour_utc: z.number().int().min(0).max(23).optional(),
	registration_alerts_webhook_url: z.url().nullish(),
	system_alerts_webhook_url: z.url().nullish(),
	sso: z
		.object({
			enabled: z.boolean().optional(),
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
			redirect_uri: z.url().nullish(),
		})
		.optional(),
});

export type InstanceConfigUpdateRequest = z.infer<typeof InstanceConfigUpdateRequest>;

const LimitKeySchema = z.enum(LIMIT_KEYS as unknown as [string, ...Array<string>]);

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

export const SnowflakeReservationEntry = z.object({
	email: z.string().describe('Email address the snowflake is reserved for'),
	snowflake: z.string().describe('Reserved snowflake ID'),
	updated_at: z.string().nullable().describe('ISO 8601 timestamp when the reservation was last updated'),
});

export type SnowflakeReservationEntry = z.infer<typeof SnowflakeReservationEntry>;

export const ListSnowflakeReservationsResponse = z.object({
	reservations: z.array(SnowflakeReservationEntry).max(1000).describe('List of snowflake reservations'),
});

export type ListSnowflakeReservationsResponse = z.infer<typeof ListSnowflakeReservationsResponse>;

export const AddSnowflakeReservationRequest = z.object({
	email: EmailType.describe('Email address to reserve the snowflake for'),
	snowflake: SnowflakeType.transform((val) => val.toString()).describe('Snowflake ID to reserve'),
});

export type AddSnowflakeReservationRequest = z.infer<typeof AddSnowflakeReservationRequest>;

export const DeleteSnowflakeReservationRequest = z.object({
	email: EmailType.describe('Email address of the reservation to delete'),
});

export type DeleteSnowflakeReservationRequest = z.infer<typeof DeleteSnowflakeReservationRequest>;

export const CreateSystemDmJobRequest = z.object({
	content: z.string().min(1).max(4000).describe('Message content to send to users'),
	registration_start: z.string().nullish().optional().describe('Only target users registered after this date'),
	registration_end: z.string().nullish().optional().describe('Only target users registered before this date'),
	excluded_guild_ids: z.array(SnowflakeType).max(100).optional().describe('Guild IDs whose members should be excluded'),
});

export type CreateSystemDmJobRequest = z.infer<typeof CreateSystemDmJobRequest>;

export const SystemDmJobsQueryRequest = z.object({
	limit: createQueryIntegerType({minValue: 1, maxValue: 100, defaultValue: 20}).describe(
		'Maximum number of jobs to return (1-100, default 20)',
	),
	before_job_id: SnowflakeType.optional().describe('Return jobs before this job ID'),
});

export type SystemDmJobsQueryRequest = z.infer<typeof SystemDmJobsQueryRequest>;

const SystemDmJobStatusEnum = createNamedStringLiteralUnion(
	[
		['pending', 'pending', 'Job is pending approval'],
		['approved', 'approved', 'Job has been approved and is queued'],
		['running', 'running', 'Job is currently running'],
		['completed', 'completed', 'Job completed successfully'],
		['failed', 'failed', 'Job failed'],
	],
	'Current status of the system DM job',
);

export const SystemDmJobResponse = z.object({
	job_id: z.string().describe('Unique identifier for the job'),
	status: SystemDmJobStatusEnum,
	content: z.string().describe('Message content being sent'),
	target_count: Int32Type.describe('Total number of users targeted'),
	sent_count: Int32Type.describe('Number of messages successfully sent'),
	failed_count: Int32Type.describe('Number of messages that failed to send'),
	created_at: z.string().describe('ISO 8601 timestamp when the job was created'),
	approved_at: z.string().nullish().describe('ISO 8601 timestamp when the job was approved'),
	registration_start: z.string().nullish().describe('Registration date filter start'),
	registration_end: z.string().nullish().describe('Registration date filter end'),
	excluded_guild_ids: z.array(z.string()).max(100).describe('List of excluded guild IDs'),
	last_error: z.string().nullish().describe('Last error message if the job failed'),
});

export type SystemDmJobResponse = z.infer<typeof SystemDmJobResponse>;

export const ListSystemDmJobsResponse = z.object({
	jobs: z.array(SystemDmJobResponse).max(100).describe('List of system DM jobs'),
	next_cursor: z.string().nullish().describe('Pagination cursor for the next page'),
});

export type ListSystemDmJobsResponse = z.infer<typeof ListSystemDmJobsResponse>;

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

export const AdminAuditLogResponseSchema = z.object({
	log_id: SnowflakeStringType,
	admin_user_id: SnowflakeStringType,
	target_type: createStringType(1, 256),
	target_id: z.string().describe('The ID of the affected entity (user, channel, role, invite code, etc.)'),
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

export const BulkOperationFailedResponse = z.object({
	id: SnowflakeStringType,
	error: createStringType(1, 4000),
});

export const BulkOperationResponse = z.object({
	successful: z.array(SnowflakeStringType).max(1000),
	failed: z.array(BulkOperationFailedResponse).max(1000),
});

export const LegalHoldResponse = z.object({
	held: z.boolean(),
});

const NcmecSubmissionStatusEnum = createNamedStringLiteralUnion(
	[
		['not_submitted', 'not_submitted', 'Report has not been submitted to NCMEC'],
		['submitted', 'submitted', 'Report has been submitted to NCMEC'],
		['failed', 'failed', 'Report submission to NCMEC failed'],
	],
	'NCMEC submission status',
);

export const NcmecSubmissionStatusResponse = z.object({
	status: NcmecSubmissionStatusEnum,
	ncmec_report_id: createStringType(1, 256).nullable().describe('NCMEC report ID if submitted'),
	submitted_at: z.string().nullable().describe('ISO 8601 timestamp when the report was submitted'),
	submitted_by_admin_id: SnowflakeStringType.nullable().describe('ID of the admin who submitted the report'),
	failure_reason: createStringType(1, 4000).nullable().describe('Reason for submission failure if failed'),
});

export const NcmecSubmitResultResponse = z.object({
	success: z.boolean(),
	ncmec_report_id: createStringType(1, 256).nullable(),
	error: createStringType(1, 4000).nullable(),
});

export const CodesResponse = z.object({
	codes: z.array(z.string()),
});

export const GuildMemoryStatsResponse = z.object({
	guilds: z
		.array(
			z.object({
				guild_id: SnowflakeStringType.nullable(),
				guild_name: createStringType(1, 100),
				guild_icon: createStringType(1, 256).nullable(),
				memory: Int64StringType,
				member_count: Int32Type,
				session_count: Int32Type,
				presence_count: Int32Type,
			}),
		)
		.max(100),
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
});

export type NodeStatsResponse = z.infer<typeof NodeStatsResponse>;

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
});

const AdminGuildChannelSummarySchema = z.object({
	id: SnowflakeStringType,
	name: createStringType(1, 100).nullable(),
	type: ChannelTypeSchema,
	position: Int32Type,
	parent_id: SnowflakeStringType.nullable(),
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
	members: z.array(z.lazy(() => GuildMemberResponse)).max(200),
	total: Int32Type,
	limit: Int32Type,
	offset: Int32Type,
});

export const GuildAssetItemSchema = z.object({
	id: SnowflakeStringType,
	name: createStringType(1, 100),
	animated: z.boolean(),
	creator_id: SnowflakeStringType,
	media_url: createStringType(1, 2048),
});

export type GuildEmojiAsset = z.infer<typeof GuildAssetItemSchema>;
export type GuildStickerAsset = z.infer<typeof GuildAssetItemSchema>;

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

const AdminMessageSchema = z.object({
	id: SnowflakeStringType,
	channel_id: SnowflakeStringType,
	author_id: SnowflakeStringType,
	author_username: createStringType(1, 100),
	author_discriminator: createStringType(1, 10),
	content: createStringType(0, 4000),
	timestamp: z.string(),
	attachments: z
		.array(
			z.object({
				filename: createStringType(1, 256),
				url: createStringType(1, 2048),
			}),
		)
		.max(10),
});

export const LookupMessageResponse = z.object({
	messages: z.array(AdminMessageSchema).max(100),
	message_id: SnowflakeStringType.nullable(),
});

export const DeleteMessageResponse = z.object({
	success: z.literal(true),
});

export const MessageShredStatusNotFoundResponse = z.object({
	status: z.literal('not_found'),
});

export const MessageShredStatusProgressResponse = z.object({
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
	guild_id: SnowflakeStringType.nullable(),
	content: z.string(),
	timestamp: z.string(),
	attachments: z.array(
		z.object({
			filename: z.string(),
			url: z.string(),
		}),
	),
	author_id: SnowflakeStringType,
	author_username: z.string(),
	author_discriminator: z.string(),
});

export const ReportAdminResponseSchema = z.object({
	report_id: SnowflakeStringType,
	reporter_id: SnowflakeStringType.nullable(),
	reporter_tag: z.string().nullable(),
	reporter_username: z.string().nullable(),
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
	reported_user_discriminator: z.string().nullable(),
	reported_user_avatar_hash: z.string().nullable(),
	reported_guild_id: SnowflakeStringType.nullable(),
	reported_guild_name: z.string().nullable(),
	reported_message_id: SnowflakeStringType.nullable(),
	reported_channel_id: SnowflakeStringType.nullable(),
	reported_channel_name: z.string().nullable(),
	reported_guild_invite_code: z.string().nullable(),
	resolved_at: z.string().nullable(),
	resolved_by_admin_id: SnowflakeStringType.nullable(),
	public_comment: z.string().nullable(),
	mutual_dm_channel_id: SnowflakeStringType.nullable().optional(),
	message_context: z.array(ReportMessageContextSchema).optional(),
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
	defaults: z.record(z.string(), z.record(LimitKeySchema, z.number())),
	metadata: z.record(LimitKeySchema, LimitKeyMetadataSchema),
	categories: z.record(z.string(), z.string()),
	limit_keys: z.array(z.string()),
	bounds: z.record(z.string(), z.object({min: z.number(), max: z.number()})).optional(),
});

export const DeleteApiKeyResponse = z.object({
	success: z.literal(true),
});

export const VisionarySlotSchema = z.object({
	slot_index: Int32Type.describe('The slot index'),
	user_id: Int64StringType.nullable().describe(
		'User ID that reserved this slot, or null if unreserved (special value -1 is also valid)',
	),
});

export type VisionarySlotSchema = z.infer<typeof VisionarySlotSchema>;

export const ListVisionarySlotsResponse = z.object({
	slots: z.array(VisionarySlotSchema).max(10000).describe('List of all visionary slots'),
	total_count: Int32Type.describe('Total number of slots'),
	reserved_count: Int32Type.describe('Number of reserved slots'),
});

export type ListVisionarySlotsResponse = z.infer<typeof ListVisionarySlotsResponse>;

export const ExpandVisionarySlotsRequest = z.object({
	count: Int32Type.min(1).max(1000).describe('Number of new slots to create'),
});

export type ExpandVisionarySlotsRequest = z.infer<typeof ExpandVisionarySlotsRequest>;

export const ShrinkVisionarySlotsRequest = z.object({
	target_count: Int32Type.min(0)
		.max(100000)
		.describe('Target total number of slots (removes from highest indices, minimum 0 slots)'),
});

export type ShrinkVisionarySlotsRequest = z.infer<typeof ShrinkVisionarySlotsRequest>;

export const ReserveVisionarySlotRequest = z.object({
	slot_index: Int32Type.min(1).describe('Slot index to reserve (must be >= 1)'),
	user_id: Int64StringType.nullable().describe(
		'User ID to reserve the slot for, or null to unreserve (special value -1 is also valid)',
	),
});

export type ReserveVisionarySlotRequest = z.infer<typeof ReserveVisionarySlotRequest>;

export const SwapVisionarySlotsRequest = z
	.object({
		slot_index_a: Int32Type.min(1).describe('First slot index to swap (must be >= 1)'),
		slot_index_b: Int32Type.min(1).describe('Second slot index to swap (must be >= 1)'),
	})
	.refine((data) => data.slot_index_a !== data.slot_index_b, {
		message: ValidationErrorCodes.INVALID_FORMAT,
		path: ['slot_index_b'],
		params: {detail: 'Slot indices must be different'},
	});

export type SwapVisionarySlotsRequest = z.infer<typeof SwapVisionarySlotsRequest>;

export const VisionarySlotOperationResponse = z.object({
	success: z.literal(true),
});
