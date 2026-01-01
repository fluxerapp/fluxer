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

import {
	AVATAR_MAX_SIZE,
	EMOJI_MAX_SIZE,
	GuildExplicitContentFilterTypes,
	GuildMFALevel,
	GuildSplashCardAlignment,
	GuildVerificationLevel,
	STICKER_MAX_SIZE,
	VALID_TEMP_BAN_DURATIONS,
} from '~/Constants';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {Guild, GuildBan, GuildEmoji, GuildMember, GuildRole, GuildSticker} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import {ColorType, createBase64StringType, createStringType, Int64Type, PasswordType, z} from '~/Schema';
import {getCachedUserPartialResponse, getCachedUserPartialResponses} from '~/user/UserCacheHelpers';
import {UserPartialResponse} from '~/user/UserModel';

const SplashCardAlignmentSchema = z.union([
	z.literal(GuildSplashCardAlignment.CENTER),
	z.literal(GuildSplashCardAlignment.LEFT),
	z.literal(GuildSplashCardAlignment.RIGHT),
]);

export const GuildResponse = z.object({
	id: z.string(),
	name: z.string(),
	icon: z.string().nullish(),
	banner: z.string().nullish(),
	banner_width: z.number().int().nullish(),
	banner_height: z.number().int().nullish(),
	splash: z.string().nullish(),
	splash_width: z.number().int().nullish(),
	splash_height: z.number().int().nullish(),
	splash_card_alignment: SplashCardAlignmentSchema,
	embed_splash: z.string().nullish(),
	embed_splash_width: z.number().int().nullish(),
	embed_splash_height: z.number().int().nullish(),
	vanity_url_code: z.string().nullish(),
	owner_id: z.string(),
	system_channel_id: z.string().nullish(),
	system_channel_flags: z.number().int(),
	rules_channel_id: z.string().nullish(),
	afk_channel_id: z.string().nullish(),
	afk_timeout: z.number().int(),
	features: z.array(z.string()),
	verification_level: z.number().int(),
	mfa_level: z.number().int(),
	nsfw_level: z.number().int(),
	explicit_content_filter: z.number().int(),
	default_message_notifications: z.number().int(),
	disabled_operations: z.number().int(),
	permissions: z.string().nullish(),
});

export type GuildResponse = z.infer<typeof GuildResponse>;

export const GuildPartialResponse = z.object({
	id: z.string(),
	name: z.string(),
	icon: z.string().nullish(),
	banner: z.string().nullish(),
	banner_width: z.number().int().nullish(),
	banner_height: z.number().int().nullish(),
	splash: z.string().nullish(),
	splash_width: z.number().int().nullish(),
	splash_height: z.number().int().nullish(),
	splash_card_alignment: SplashCardAlignmentSchema,
	embed_splash: z.string().nullish(),
	embed_splash_width: z.number().int().nullish(),
	embed_splash_height: z.number().int().nullish(),
	features: z.array(z.string()),
});

export type GuildPartialResponse = z.infer<typeof GuildPartialResponse>;

export const GuildCreateRequest = z.object({
	name: createStringType(1, 100),
	icon: createBase64StringType(1, AVATAR_MAX_SIZE * 1.33).nullish(),
	empty_features: z.boolean().optional(),
});

export type GuildCreateRequest = z.infer<typeof GuildCreateRequest>;

export const GuildUpdateRequest = z
	.object({
		name: createStringType(1, 100),
		icon: createBase64StringType(1, AVATAR_MAX_SIZE * 1.33).nullish(),
		system_channel_id: Int64Type.nullish(),
		system_channel_flags: z.number().int().min(0),
		afk_channel_id: Int64Type.nullish(),
		afk_timeout: z.number().int().min(60).max(3600),
		default_message_notifications: z.number().int().min(0).max(1),
		verification_level: z.union([
			z.literal(GuildVerificationLevel.NONE),
			z.literal(GuildVerificationLevel.LOW),
			z.literal(GuildVerificationLevel.MEDIUM),
			z.literal(GuildVerificationLevel.HIGH),
			z.literal(GuildVerificationLevel.VERY_HIGH),
		]),
		mfa_level: z.union([z.literal(GuildMFALevel.NONE), z.literal(GuildMFALevel.ELEVATED)]),
		explicit_content_filter: z.union([
			z.literal(GuildExplicitContentFilterTypes.DISABLED),
			z.literal(GuildExplicitContentFilterTypes.MEMBERS_WITHOUT_ROLES),
			z.literal(GuildExplicitContentFilterTypes.ALL_MEMBERS),
		]),
		banner: createBase64StringType(1, AVATAR_MAX_SIZE * 1.33).nullish(),
		splash: createBase64StringType(1, AVATAR_MAX_SIZE * 1.33).nullish(),
		embed_splash: createBase64StringType(1, AVATAR_MAX_SIZE * 1.33).nullish(),
		splash_card_alignment: SplashCardAlignmentSchema.optional(),
		features: z.array(z.string()),
	})
	.partial();

export type GuildUpdateRequest = z.infer<typeof GuildUpdateRequest>;

export const GuildMemberResponse = z.object({
	user: z.lazy(() => UserPartialResponse),
	nick: z.string().nullish(),
	avatar: z.string().nullish(),
	banner: z.string().nullish(),
	accent_color: z.number().int().nullish(),
	roles: z.array(z.string()),
	joined_at: z.iso.datetime(),
	join_source_type: z.number().int().nullish(),
	source_invite_code: z.string().nullish(),
	inviter_id: z.string().nullish(),
	mute: z.boolean(),
	deaf: z.boolean(),
	communication_disabled_until: z.iso.datetime().nullish(),
	profile_flags: z.number().int().nullish(),
});

export type GuildMemberResponse = z.infer<typeof GuildMemberResponse>;

export const GuildMemberUpdateRequest = z.object({
	nick: createStringType(1, 32).nullish(),
	roles: z
		.array(Int64Type)
		.max(100, 'Maximum 100 roles allowed')
		.optional()
		.transform((ids) => (ids ? new Set(ids) : undefined)),
	avatar: createBase64StringType(1, AVATAR_MAX_SIZE * 1.33).nullish(),
	banner: createBase64StringType(1, AVATAR_MAX_SIZE * 1.33).nullish(),
	bio: createStringType(1, 320).nullish(),
	pronouns: createStringType(1, 40).nullish(),
	accent_color: ColorType.nullish(),
	profile_flags: z.number().int().nullish(),
	mute: z.boolean().optional(),
	deaf: z.boolean().optional(),
	communication_disabled_until: z.iso.datetime().nullish(),
	timeout_reason: createStringType(1, 512).nullish(),
	channel_id: Int64Type.nullish(),
	connection_id: createStringType(1, 32).nullish(),
});

export type GuildMemberUpdateRequest = z.infer<typeof GuildMemberUpdateRequest>;

export const MyGuildMemberUpdateRequest = GuildMemberUpdateRequest.omit({roles: true}).partial();

export type MyGuildMemberUpdateRequest = z.infer<typeof MyGuildMemberUpdateRequest>;

export const GuildRoleResponse = z.object({
	id: z.string(),
	name: z.string(),
	color: z.number().int(),
	position: z.number().int(),
	hoist_position: z.number().int().nullish(),
	permissions: z.string(),
	hoist: z.boolean(),
	mentionable: z.boolean(),
});

export type GuildRoleResponse = z.infer<typeof GuildRoleResponse>;

export const GuildRoleCreateRequest = z.object({
	name: createStringType(1, 100),
	color: ColorType.default(0x000000),
	permissions: Int64Type.optional(),
});

export type GuildRoleCreateRequest = z.infer<typeof GuildRoleCreateRequest>;

export const GuildRoleUpdateRequest = z.object({
	name: createStringType(1, 100).optional(),
	color: ColorType.optional(),
	permissions: Int64Type.optional(),
	hoist: z.boolean().optional(),
	hoist_position: z.number().int().nullish(),
	mentionable: z.boolean().optional(),
});

export type GuildRoleUpdateRequest = z.infer<typeof GuildRoleUpdateRequest>;

export const GuildEmojiResponse = z.object({
	id: z.string(),
	name: z.string(),
	animated: z.boolean(),
});

export type GuildEmojiResponse = z.infer<typeof GuildEmojiResponse>;

export const GuildEmojiWithUserResponse = z.object({
	id: z.string(),
	name: z.string(),
	animated: z.boolean(),
	user: z.lazy(() => UserPartialResponse),
});

export type GuildEmojiWithUserResponse = z.infer<typeof GuildEmojiWithUserResponse>;

export const GuildEmojiCreateRequest = z.object({
	name: createStringType(2, 32).refine(
		(value) => /^[a-zA-Z0-9_]+$/.test(value),
		'Emoji name can only contain letters, numbers, and underscores',
	),
	image: createBase64StringType(1, EMOJI_MAX_SIZE * 1.33),
});

export type GuildEmojiCreateRequest = z.infer<typeof GuildEmojiCreateRequest>;

export const GuildEmojiUpdateRequest = GuildEmojiCreateRequest.pick({name: true});

export type GuildEmojiUpdateRequest = z.infer<typeof GuildEmojiUpdateRequest>;

export const GuildEmojiBulkCreateRequest = z.object({
	emojis: z
		.array(GuildEmojiCreateRequest)
		.min(1, 'At least one emoji is required')
		.max(50, 'Maximum 50 emojis per batch'),
});

export type GuildEmojiBulkCreateRequest = z.infer<typeof GuildEmojiBulkCreateRequest>;

export const GuildStickerResponse = z.object({
	id: z.string(),
	name: z.string(),
	description: z.string(),
	tags: z.array(z.string()),
	format_type: z.int(),
});

export type GuildStickerResponse = z.infer<typeof GuildStickerResponse>;

export const GuildStickerWithUserResponse = z.object({
	id: z.string(),
	name: z.string(),
	description: z.string(),
	tags: z.array(z.string()),
	format_type: z.int(),
	user: z.lazy(() => UserPartialResponse),
});

export type GuildStickerWithUserResponse = z.infer<typeof GuildStickerWithUserResponse>;

export const GuildStickerCreateRequest = z.object({
	name: createStringType(2, 30),
	description: createStringType(1, 100).nullish(),
	tags: z.array(createStringType(1, 30)).min(0).max(10).optional().default([]),
	image: createBase64StringType(1, STICKER_MAX_SIZE * 1.33),
});

export type GuildStickerCreateRequest = z.infer<typeof GuildStickerCreateRequest>;

export const GuildStickerUpdateRequest = GuildStickerCreateRequest.pick({
	name: true,
	description: true,
	tags: true,
});

export type GuildStickerUpdateRequest = z.infer<typeof GuildStickerUpdateRequest>;

export const GuildStickerBulkCreateRequest = z.object({
	stickers: z
		.array(GuildStickerCreateRequest)
		.min(1, 'At least one sticker is required')
		.max(50, 'Maximum 50 stickers per batch'),
});

export type GuildStickerBulkCreateRequest = z.infer<typeof GuildStickerBulkCreateRequest>;

export const GuildTransferOwnershipRequest = z.object({
	new_owner_id: Int64Type,
	password: PasswordType.optional(),
});

export type GuildTransferOwnershipRequest = z.infer<typeof GuildTransferOwnershipRequest>;

export const GuildBanCreateRequest = z.object({
	delete_message_days: z.number().int().min(0).max(7).default(0),
	reason: createStringType(0, 512).nullish(),
	ban_duration_seconds: z
		.number()
		.int()
		.refine((val) => val === 0 || VALID_TEMP_BAN_DURATIONS.has(val), {
			message: `Ban duration must be 0 (permanent) or one of the valid durations: ${Array.from(VALID_TEMP_BAN_DURATIONS).join(', ')} seconds`,
		})
		.optional(),
});

export type GuildBanCreateRequest = z.infer<typeof GuildBanCreateRequest>;

export const GuildBanResponse = z.object({
	user: z.lazy(() => UserPartialResponse),
	reason: z.string().nullish(),
	moderator_id: z.string(),
	banned_at: z.iso.datetime(),
	expires_at: z.iso.datetime().nullish(),
});

export type GuildBanResponse = z.infer<typeof GuildBanResponse>;

export const GuildVanityURLResponse = z.object({
	code: z.string().nullish(),
	uses: z.number().int(),
});

export type GuildVanityURLResponse = z.infer<typeof GuildVanityURLResponse>;

export const mapGuildToPartialResponse = (guild: Guild): GuildPartialResponse => ({
	id: guild.id.toString(),
	name: guild.name,
	icon: guild.iconHash,
	banner: guild.bannerHash,
	banner_width: guild.bannerWidth,
	banner_height: guild.bannerHeight,
	splash: guild.splashHash,
	splash_width: guild.splashWidth,
	splash_height: guild.splashHeight,
	embed_splash: guild.embedSplashHash,
	embed_splash_width: guild.embedSplashWidth,
	embed_splash_height: guild.embedSplashHeight,
	splash_card_alignment: guild.splashCardAlignment,
	features: Array.from(guild.features),
});

export const mapGuildToGuildResponse = (guild: Guild, options?: {permissions?: bigint | null}): GuildResponse => ({
	id: guild.id.toString(),
	name: guild.name,
	icon: guild.iconHash,
	banner: guild.bannerHash,
	banner_width: guild.bannerWidth,
	banner_height: guild.bannerHeight,
	splash: guild.splashHash,
	splash_width: guild.splashWidth,
	splash_height: guild.splashHeight,
	embed_splash: guild.embedSplashHash,
	embed_splash_width: guild.embedSplashWidth,
	embed_splash_height: guild.embedSplashHeight,
	splash_card_alignment: guild.splashCardAlignment,
	vanity_url_code: guild.vanityUrlCode,
	owner_id: guild.ownerId.toString(),
	system_channel_id: guild.systemChannelId ? guild.systemChannelId.toString() : null,
	system_channel_flags: guild.systemChannelFlags,
	rules_channel_id: guild.rulesChannelId ? guild.rulesChannelId.toString() : null,
	afk_channel_id: guild.afkChannelId ? guild.afkChannelId.toString() : null,
	afk_timeout: guild.afkTimeout,
	features: Array.from(guild.features),
	verification_level: guild.verificationLevel,
	mfa_level: guild.mfaLevel,
	nsfw_level: guild.nsfwLevel,
	explicit_content_filter: guild.explicitContentFilter,
	default_message_notifications: guild.defaultMessageNotifications,
	disabled_operations: guild.disabledOperations,
	permissions: options?.permissions != null ? options.permissions.toString() : undefined,
});

export const mapGuildRoleToResponse = (role: GuildRole): GuildRoleResponse => ({
	id: role.id.toString(),
	name: role.name,
	color: role.color,
	position: role.position,
	hoist_position: role.hoistPosition,
	permissions: role.permissions.toString(),
	hoist: role.isHoisted,
	mentionable: role.isMentionable,
});

export const mapGuildEmojiToResponse = (emoji: GuildEmoji): GuildEmojiResponse => ({
	id: emoji.id.toString(),
	name: emoji.name,
	animated: emoji.isAnimated,
});

export const mapGuildStickerToResponse = (sticker: GuildSticker): GuildStickerResponse => ({
	id: sticker.id.toString(),
	name: sticker.name,
	description: sticker.description ?? '',
	tags: sticker.tags,
	format_type: sticker.formatType,
});

const mapMemberWithUser = (
	member: GuildMember,
	userPartial: z.infer<typeof UserPartialResponse>,
): GuildMemberResponse => {
	const now = Date.now();
	const isTimedOut = member.communicationDisabledUntil != null && member.communicationDisabledUntil.getTime() > now;
	return {
		user: userPartial,
		nick: member.nickname,
		avatar: member.isPremiumSanitized ? null : member.avatarHash,
		banner: member.isPremiumSanitized ? null : member.bannerHash,
		accent_color: member.accentColor,
		roles: Array.from(member.roleIds).map((id) => id.toString()),
		joined_at: member.joinedAt.toISOString(),
		join_source_type: member.joinSourceType,
		source_invite_code: member.sourceInviteCode,
		inviter_id: member.inviterId ? member.inviterId.toString() : null,
		mute: isTimedOut ? true : member.isMute,
		deaf: member.isDeaf,
		communication_disabled_until: member.communicationDisabledUntil?.toISOString() ?? null,
		profile_flags: member.profileFlags || undefined,
	};
};

export const isGuildMemberTimedOut = (member?: GuildMemberResponse | null): boolean => {
	if (!member?.communication_disabled_until) {
		return false;
	}
	const timestamp = Date.parse(member.communication_disabled_until);
	return !Number.isNaN(timestamp) && timestamp > Date.now();
};

export async function mapGuildMemberToResponse(
	member: GuildMember,
	userCacheService: UserCacheService,
	requestCache: RequestCache,
): Promise<GuildMemberResponse> {
	const userPartial = await getCachedUserPartialResponse({userId: member.userId, userCacheService, requestCache});
	return mapMemberWithUser(member, userPartial);
}

export async function mapGuildMembersToResponse(
	members: Array<GuildMember>,
	userCacheService: UserCacheService,
	requestCache: RequestCache,
): Promise<Array<GuildMemberResponse>> {
	const userIds = [...new Set(members.map((member) => member.userId))];
	const userPartials = await getCachedUserPartialResponses({userIds, userCacheService, requestCache});
	return members.map((member) => mapMemberWithUser(member, userPartials.get(member.userId)!));
}

const mapEmojiWithUser = (
	emoji: GuildEmoji,
	userPartial: z.infer<typeof UserPartialResponse>,
): GuildEmojiWithUserResponse => ({
	id: emoji.id.toString(),
	name: emoji.name,
	animated: emoji.isAnimated,
	user: userPartial,
});

export async function mapGuildEmojisWithUsersToResponse(
	emojis: Array<GuildEmoji>,
	userCacheService: UserCacheService,
	requestCache: RequestCache,
): Promise<Array<GuildEmojiWithUserResponse>> {
	const userIds = [...new Set(emojis.map((emoji) => emoji.creatorId))];
	const userPartials = await getCachedUserPartialResponses({userIds, userCacheService, requestCache});
	return emojis.map((emoji) => mapEmojiWithUser(emoji, userPartials.get(emoji.creatorId)!));
}

const mapStickerWithUser = (
	sticker: GuildSticker,
	userPartial: z.infer<typeof UserPartialResponse>,
): GuildStickerWithUserResponse => ({
	id: sticker.id.toString(),
	name: sticker.name,
	description: sticker.description ?? '',
	tags: sticker.tags,
	format_type: sticker.formatType,
	user: userPartial,
});

export async function mapGuildStickersWithUsersToResponse(
	stickers: Array<GuildSticker>,
	userCacheService: UserCacheService,
	requestCache: RequestCache,
): Promise<Array<GuildStickerWithUserResponse>> {
	const userIds = [...new Set(stickers.map((emoji) => emoji.creatorId))];
	const userPartials = await getCachedUserPartialResponses({userIds, userCacheService, requestCache});
	return stickers.map((sticker) => mapStickerWithUser(sticker, userPartials.get(sticker.creatorId)!));
}

const mapBanWithUser = (ban: GuildBan, userPartial: z.infer<typeof UserPartialResponse>): GuildBanResponse => ({
	user: userPartial,
	reason: ban.reason,
	moderator_id: ban.moderatorId.toString(),
	banned_at: ban.bannedAt.toISOString(),
	expires_at: ban.expiresAt ? ban.expiresAt.toISOString() : null,
});

export async function mapGuildBansToResponse(
	bans: Array<GuildBan>,
	userCacheService: UserCacheService,
	requestCache: RequestCache,
): Promise<Array<GuildBanResponse>> {
	const userIds = [...new Set(bans.map((ban) => ban.userId))];
	const userPartials = await getCachedUserPartialResponses({userIds, userCacheService, requestCache});
	return bans.map((ban) => mapBanWithUser(ban, userPartials.get(ban.userId)!));
}
