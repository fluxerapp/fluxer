// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DISCOVERY_DESCRIPTION_MAX_LENGTH,
	DISCOVERY_DESCRIPTION_MIN_LENGTH,
	DISCOVERY_MAX_TAGS,
	DISCOVERY_TAG_MAX_LENGTH,
	DISCOVERY_TAG_MIN_LENGTH,
	isValidDiscoveryLanguage,
	isValidDiscoveryTag,
	normalizeDiscoveryTag,
} from '@fluxer/constants/src/DiscoveryConstants';
import {NSFWLevelSchema} from '@fluxer/schema/src/primitives/GuildValidators';
import {SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const DiscoveryTagSchema = z
	.string()
	.min(DISCOVERY_TAG_MIN_LENGTH)
	.max(DISCOVERY_TAG_MAX_LENGTH)
	.refine((value) => isValidDiscoveryTag(value), {
		message: 'Tag must be alphanumeric and may contain spaces, hyphens, underscores, plus, or ampersands',
	})
	.transform((value) => normalizeDiscoveryTag(value));
const DiscoveryTagsSchema = z
	.array(DiscoveryTagSchema)
	.max(DISCOVERY_MAX_TAGS)
	.transform((tags) => Array.from(new Set(tags)))
	.describe(`Up to ${DISCOVERY_MAX_TAGS} custom discovery tags`);
const DiscoveryLanguageSchema = z
	.string()
	.refine((value) => isValidDiscoveryLanguage(value), {
		message: 'Unsupported language code',
	})
	.describe('Primary community language (BCP-47 code)');
export const DiscoveryApplicationRequest = z.object({
	description: z
		.string()
		.min(DISCOVERY_DESCRIPTION_MIN_LENGTH)
		.max(DISCOVERY_DESCRIPTION_MAX_LENGTH)
		.describe('Description for discovery listing'),
	category_type: z.number().int().min(0).max(8).describe('Discovery category type'),
	primary_language: DiscoveryLanguageSchema.optional(),
	custom_tags: DiscoveryTagsSchema.optional(),
});

export type DiscoveryApplicationRequest = z.infer<typeof DiscoveryApplicationRequest>;

export const DiscoveryApplicationPatchRequest = z.object({
	description: z
		.string()
		.min(DISCOVERY_DESCRIPTION_MIN_LENGTH)
		.max(DISCOVERY_DESCRIPTION_MAX_LENGTH)
		.optional()
		.describe('Updated description for discovery listing'),
	category_type: z.number().int().min(0).max(8).optional().describe('Updated discovery category type'),
	primary_language: DiscoveryLanguageSchema.optional(),
	custom_tags: DiscoveryTagsSchema.optional(),
});

export type DiscoveryApplicationPatchRequest = z.infer<typeof DiscoveryApplicationPatchRequest>;

export const DiscoverySearchQuery = z.object({
	query: z.string().max(100).optional().describe('Search query'),
	category: z.coerce.number().int().min(0).max(8).optional().describe('Filter by category'),
	language: z
		.string()
		.refine((value) => isValidDiscoveryLanguage(value), {message: 'Unsupported language code'})
		.optional()
		.describe('Filter by primary community language'),
	tag: z.string().max(DISCOVERY_TAG_MAX_LENGTH).optional().describe('Filter by a specific custom tag'),
	sort_by: z.enum(['member_count', 'online_count', 'relevance']).optional().describe('Sort order'),
	limit: z.coerce.number().int().min(1).max(48).optional().default(24).describe('Number of results to return'),
	offset: z.coerce.number().int().min(0).optional().default(0).describe('Pagination offset'),
});

export type DiscoverySearchQuery = z.infer<typeof DiscoverySearchQuery>;

const DiscoveryGuildResponse = z.object({
	id: SnowflakeStringType.describe('Guild ID'),
	name: z.string().describe('Guild name'),
	icon: z.string().nullish().describe('Guild icon hash'),
	banner: z.string().nullish().describe('Guild banner hash'),
	description: z.string().nullish().describe('Discovery description'),
	category_type: z.number().describe('Discovery category type'),
	primary_language: z.string().nullish().describe('Primary community language'),
	custom_tags: z.array(z.string()).describe('Custom discovery tags'),
	member_count: z.number().describe('Approximate member count'),
	online_count: z.number().describe('Approximate online member count'),
	features: z.array(z.string()).describe('Guild feature flags'),
	verification_level: z.number().describe('Verification level'),
});

const DiscoveryCategoryCountResponse = z.object({
	category_type: z.number().describe('Discovery category type'),
	count: z.number().describe('Number of matching guilds in this category'),
});

export const DiscoveryGuildListResponse = z.object({
	guilds: z.array(DiscoveryGuildResponse).describe('Discovery guild results'),
	total: z.number().describe('Total number of matching guilds'),
	category_counts: z
		.array(DiscoveryCategoryCountResponse)
		.describe('Match counts per category for the current filters, ignoring the category filter'),
});

export type DiscoveryGuildListResponse = z.infer<typeof DiscoveryGuildListResponse>;

export const DiscoveryApplicationResponse = z.object({
	guild_id: SnowflakeStringType.describe('Guild ID'),
	guild_nsfw_level: NSFWLevelSchema.nullable().optional().describe('NSFW level of the guild'),
	status: z.string().describe('Application status'),
	description: z.string().describe('Discovery description'),
	category_type: z.number().describe('Discovery category type'),
	primary_language: z.string().nullish().describe('Primary community language'),
	custom_tags: z.array(z.string()).describe('Custom discovery tags'),
	applied_at: z.string().describe('Application timestamp'),
	reviewed_at: z.string().nullish().describe('Review timestamp'),
	review_reason: z.string().nullish().describe('Review reason (approval/rejection)'),
	removed_at: z.string().nullish().describe('Removal timestamp'),
	removal_reason: z.string().nullish().describe('Removal reason'),
});

export type DiscoveryApplicationResponse = z.infer<typeof DiscoveryApplicationResponse>;

export const DiscoveryAdminPendingApplicationResponse = z.object({
	guild_id: SnowflakeStringType.describe('Guild ID'),
	guild_name: z.string().describe('Guild name'),
	guild_icon: z.string().nullable().describe('Guild icon hash'),
	guild_owner_id: SnowflakeStringType.describe('Guild owner user ID'),
	guild_owner_username: z.string().nullable().describe('Guild owner username'),
	guild_owner_global_name: z.string().nullable().describe('Guild owner display name'),
	guild_owner_discriminator: z.string().nullable().describe('Guild owner discriminator'),
	guild_member_count: z.number().describe('Approximate member count'),
	guild_nsfw_level: NSFWLevelSchema.nullable().describe('NSFW level of the guild'),
	guild_features: z.array(z.string()).describe('Guild feature flags'),
	description: z.string().describe('Discovery description'),
	category_type: z.number().describe('Discovery category type'),
	primary_language: z.string().nullable().describe('Primary community language'),
	custom_tags: z.array(z.string()).describe('Custom discovery tags'),
	applied_at: z.string().describe('Application timestamp'),
});

export type DiscoveryAdminPendingApplicationResponse = z.infer<typeof DiscoveryAdminPendingApplicationResponse>;

export const DiscoveryAdminListedGuildResponse = z.object({
	guild_id: SnowflakeStringType.describe('Guild ID'),
	guild_name: z.string().describe('Guild name'),
	guild_icon: z.string().nullable().describe('Guild icon hash'),
	guild_owner_id: SnowflakeStringType.describe('Guild owner user ID'),
	guild_owner_username: z.string().nullable().describe('Guild owner username'),
	guild_owner_global_name: z.string().nullable().describe('Guild owner display name'),
	guild_owner_discriminator: z.string().nullable().describe('Guild owner discriminator'),
	guild_member_count: z.number().describe('Approximate member count'),
	guild_nsfw_level: NSFWLevelSchema.nullable().describe('NSFW level of the guild'),
	guild_features: z.array(z.string()).describe('Guild feature flags'),
	description: z.string().describe('Discovery description'),
	category_type: z.number().describe('Discovery category type'),
	primary_language: z.string().nullable().describe('Primary community language'),
	custom_tags: z.array(z.string()).describe('Custom discovery tags'),
	applied_at: z.string().describe('Application timestamp'),
	approved_at: z.string().nullable().describe('Approval timestamp'),
});

export type DiscoveryAdminListedGuildResponse = z.infer<typeof DiscoveryAdminListedGuildResponse>;

export const DiscoveryStatusResponse = z.object({
	application: DiscoveryApplicationResponse.nullish().describe('Current discovery application, if any'),
	eligible: z.boolean().describe('Whether the guild meets the requirements to apply for discovery'),
	min_member_count: z.number().describe('Minimum member count required for discovery eligibility'),
});

export type DiscoveryStatusResponse = z.infer<typeof DiscoveryStatusResponse>;

export const DiscoveryCategoryResponse = z.object({
	id: z.number().describe('Category ID'),
	name: z.string().describe('Category display name'),
});

export type DiscoveryCategoryResponse = z.infer<typeof DiscoveryCategoryResponse>;

export const DiscoveryCategoryListResponse = z.array(DiscoveryCategoryResponse);

export type DiscoveryCategoryListResponse = z.infer<typeof DiscoveryCategoryListResponse>;

export const DiscoveryAdminReviewRequest = z.object({
	reason: z.string().max(500).optional().describe('Review reason'),
});

export type DiscoveryAdminReviewRequest = z.infer<typeof DiscoveryAdminReviewRequest>;

export const DiscoveryAdminRejectRequest = z.object({
	reason: z.string().min(1).max(500).describe('Rejection reason'),
});

export type DiscoveryAdminRejectRequest = z.infer<typeof DiscoveryAdminRejectRequest>;

export const DiscoveryAdminRemoveRequest = z.object({
	reason: z.string().min(1).max(500).describe('Removal reason'),
});

export type DiscoveryAdminRemoveRequest = z.infer<typeof DiscoveryAdminRemoveRequest>;
