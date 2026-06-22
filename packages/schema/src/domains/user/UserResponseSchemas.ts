// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DEFAULT_GUILD_FOLDER_ICON,
	FriendSourceFlags,
	FriendSourceFlagsDescriptions,
	GroupDmAddPermissionFlags,
	GroupDmAddPermissionFlagsDescriptions,
	GuildFolderFlags,
	GuildFolderFlagsDescriptions,
	GuildFolderIcons,
	IncomingCallFlags,
	IncomingCallFlagsDescriptions,
	type MentionReplyPreference,
	ProfileFieldPrivacyFlags,
	ProfileFieldPrivacyFlagsDescriptions,
	PublicUserFlags,
	PublicUserFlagsDescriptions,
} from '@fluxer/constants/src/UserConstants';
import {ConnectionResponse} from '@fluxer/schema/src/domains/connection/ConnectionSchemas';
import {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import {MessageResponseSchema} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {LocaleSchema} from '@fluxer/schema/src/primitives/LocaleSchema';
import {
	createBitflagInt32Type,
	createNamedStringLiteralUnion,
	createStringType,
	HexString32Type,
	Int32Type,
	SignedInt32Type,
	SnowflakeStringType,
	withFieldDescription,
	withOpenApiType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {
	MentionReplyPreferencesSchema,
	ProfilePrivacyLevelSchema,
	RelationshipTypesSchema,
	RenderSpoilersSchema,
	SensitiveMediaFilterLevelSchema,
	SensitiveMediaGuildFilterLevelSchema,
	StickerAnimationOptionsSchema,
	TimeFormatTypesSchema,
	UserAuthenticatorTypesSchema,
	UserNotificationSettingsSchema,
	UserPremiumTypesSchema,
} from '@fluxer/schema/src/primitives/UserSettingsValidators';
import {z} from 'zod';

export const UserPartialResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier (snowflake) for this user'),
	username: z.string().describe('The username of the user, not unique across the platform'),
	discriminator: z.string().describe('The four-digit discriminator tag of the user'),
	global_name: z.string().nullable().describe('The display name of the user, if set'),
	avatar: z.string().nullable().describe('The hash of the user avatar image'),
	avatar_color: Int32Type.nullable().describe('The dominant avatar color of the user as an integer'),
	bot: z.boolean().optional().describe('Whether the user is a bot account'),
	system: z.boolean().optional().describe('Whether the user is an official system user'),
	flags: createBitflagInt32Type(
		PublicUserFlags,
		PublicUserFlagsDescriptions,
		'The public flags on the user account',
		'PublicUserFlags',
	),
	mention_flags: withFieldDescription(MentionReplyPreferencesSchema, "The user's account-wide reply mention preference")
		.optional()
		.describe(
			"The user's account-wide reply mention preference. Omitted when the user has no preference set (treated as NO_PREFERENCE).",
		),
});

export type UserPartialResponse = z.infer<typeof UserPartialResponse>;

export const UserPrivateResponse = UserPartialResponse.extend({
	is_staff: z.boolean().describe('Whether the user has staff permissions'),
	acls: z.array(z.string()).describe('Access control list entries for the user'),
	traits: z.array(z.string()).describe('Special traits assigned to the user account'),
	email: z.string().nullable().describe('The email address associated with the account'),
	email_bounced: z
		.boolean()
		.optional()
		.describe('Whether the current email address is marked as bounced by the mail provider'),
	phone: z
		.string()
		.nullish()
		.describe(
			'Always null. Retained for old-client backward compatibility — phone numbers are no longer stored on the user record.',
		),
	has_verified_phone: z.boolean().describe('Whether this account has completed phone verification'),
	bio: z.string().nullable().describe('The user biography text'),
	pronouns: z.string().nullable().describe('The preferred pronouns of the user'),
	accent_color: Int32Type.nullable().describe('The user-selected accent color as an integer'),
	timezone: z
		.string()
		.nullable()
		.optional()
		.describe('The IANA timezone identifier saved by the user. Omitted unless the user has staff access.'),
	timezone_privacy_flags: createBitflagInt32Type(
		ProfileFieldPrivacyFlags,
		ProfileFieldPrivacyFlagsDescriptions,
		'Bitfield controlling who can see the profile timezone',
		'ProfileFieldPrivacyFlags',
	)
		.optional()
		.describe('Bitfield controlling who can see the profile timezone. Omitted unless the user has staff access.'),
	banner: z.string().nullable().describe('The hash of the user profile banner image'),
	banner_color: Int32Type.nullable().describe('The default banner color if no custom banner is set'),
	mfa_enabled: z.boolean().describe('Whether multi-factor authentication is enabled'),
	authenticator_types: z
		.array(UserAuthenticatorTypesSchema)
		.optional()
		.describe('The types of authenticators configured for MFA'),
	verified: z.boolean().describe('Whether the email address has been verified'),
	premium_type: withFieldDescription(UserPremiumTypesSchema, 'The type of premium subscription').nullable(),
	premium_since: z.string().nullable().describe('ISO8601 timestamp of when premium was first activated'),
	premium_until: z
		.string()
		.nullable()
		.describe('ISO8601 timestamp of when premium access ends, including stacked gift time'),
	premium_will_cancel: z.boolean().describe('Whether premium is set to cancel at the end of the billing period'),
	premium_billing_cycle: z.string().nullable().describe('The billing cycle for the premium subscription'),
	premium_lifetime_sequence: Int32Type.nullable().describe('The sequence number for lifetime premium subscribers'),
	premium_grace_ends_at: z
		.string()
		.nullable()
		.describe(
			'ISO8601 timestamp at which the post-cancel grace period ends. Set when the subscription is fully canceled in Stripe; perks remain active and the original premium_since is restored on resubscribe until this timestamp passes. Null when not in grace.',
		),
	premium_discriminator: z
		.boolean()
		.describe(
			'Whether the user selected a premium-only discriminator that will be rerolled when non-lifetime premium access ends',
		),
	premium_badge_hidden: z.boolean().describe('Whether the premium badge is hidden on the profile'),
	premium_badge_masked: z.boolean().describe('Whether the premium badge shows a masked appearance'),
	premium_badge_timestamp_hidden: z.boolean().describe('Whether the premium start timestamp is hidden'),
	premium_badge_sequence_hidden: z.boolean().describe('Whether the lifetime sequence number is hidden'),
	premium_purchase_disabled: z.boolean().describe('Whether premium purchases are disabled for this account'),
	premium_enabled_override: z.boolean().describe('Whether premium features are enabled via override'),
	premium_perks_disabled: z.boolean().describe('Whether premium perks are temporarily disabled for this account'),
	force_inbound_phone_verification: z
		.boolean()
		.optional()
		.describe(
			'Whether this account is forced through the inbound (expensive-destination) phone verification flow regardless of prefix, for debugging',
		),
	password_last_changed_at: z.string().nullable().describe('ISO8601 timestamp of the last password change'),
	last_voice_activity_sharing_change_at: z
		.string()
		.nullable()
		.describe(
			'ISO8601 timestamp of the last bulk voice-activity-sharing change. Drives the 24-hour cooldown for re-toggling the Active Now sharing default.',
		),
	required_actions: z.array(z.string()).describe('Actions the user must complete before full access'),
	nsfw_allowed: z.boolean().describe('Whether the user is allowed to view NSFW content'),
	has_dismissed_premium_onboarding: z.boolean().describe('Whether the user has dismissed the premium onboarding flow'),
	has_ever_purchased: z.boolean().describe('Whether the user has ever made a purchase'),
	has_unread_gift_inventory: z.boolean().describe('Whether there are unread items in the gift inventory'),
	unread_gift_inventory_count: Int32Type.describe('The number of unread gift inventory items'),
	pending_bulk_message_deletion: z
		.object({
			scheduled_at: z.string().describe('ISO8601 timestamp of when the deletion was scheduled'),
			channel_count: Int32Type.describe('The number of channels with messages to delete'),
			message_count: Int32Type.describe('The total number of messages to delete'),
		})
		.nullable()
		.describe(
			'Information about a pending bulk message deletion request. Only populated when the legacy delayed-deletion flow is in progress; the new immediate-deletion flow does not surface a pending state here.',
		),
	age_verified_adult: z
		.boolean()
		.optional()
		.describe('Whether the user has verified their age as an adult via credit card verification'),
	terms_agreed_at: z
		.string()
		.nullable()
		.describe('ISO8601 timestamp of when the user last agreed to the terms of service'),
	privacy_agreed_at: z
		.string()
		.nullable()
		.describe('ISO8601 timestamp of when the user last agreed to the privacy policy'),
});

export type UserPrivateResponse = z.infer<typeof UserPrivateResponse>;

export const EmailChangeStartResponse = z.object({
	ticket: z.string().describe('Ticket returned for email change actions'),
	require_original: z.boolean().describe('Whether verification of the original email is required'),
	original_email: z.string().nullable().describe('The original email address on record'),
	original_proof: z
		.string()
		.nullable()
		.describe('Proof token generated when original email verification is not required'),
	original_code_expires_at: z
		.string()
		.nullable()
		.describe('ISO8601 timestamp when the original verification code expires'),
	resend_available_at: z
		.string()
		.nullable()
		.describe('ISO8601 timestamp when the original verification code can be resent'),
});

export type EmailChangeStartResponse = z.infer<typeof EmailChangeStartResponse>;

export const EmailChangeVerifyOriginalResponse = z.object({
	original_proof: z.string().describe('Proof token issued after verifying the original email'),
});

export type EmailChangeVerifyOriginalResponse = z.infer<typeof EmailChangeVerifyOriginalResponse>;

export const EmailChangeRequestNewResponse = z.object({
	ticket: z.string().describe('Ticket associated with the email change attempt'),
	new_email: z.string().describe('The new email address the user wants to verify'),
	new_code_expires_at: z.string().describe('ISO8601 timestamp when the new email code expires'),
	resend_available_at: z.string().nullable().describe('ISO8601 timestamp when the new email code can be resent'),
});

export type EmailChangeRequestNewResponse = z.infer<typeof EmailChangeRequestNewResponse>;

export const PasswordChangeStartResponse = z.object({
	ticket: z.string().describe('Ticket for password change actions'),
	code_expires_at: z.string().describe('ISO8601 timestamp when the verification code expires'),
	resend_available_at: z.string().nullable().describe('ISO8601 timestamp when the code can be resent'),
});

export type PasswordChangeStartResponse = z.infer<typeof PasswordChangeStartResponse>;

export const PasswordChangeVerifyResponse = z.object({
	verification_proof: z.string().describe('Proof token issued after verifying the email code'),
});

export type PasswordChangeVerifyResponse = z.infer<typeof PasswordChangeVerifyResponse>;

export const PasswordChangeCompleteResponse = z.object({
	token: z.string().describe('Authentication token for the newly created session'),
	auth_session_id_hash: z.string().describe('Base64url-encoded hash of the newly created authentication session'),
});

export type PasswordChangeCompleteResponse = z.infer<typeof PasswordChangeCompleteResponse>;

export interface UserProfileResponse {
	bio: string | null;
	pronouns: string | null;
	banner: string | null;
	banner_color?: number | null;
	accent_color: number | null;
}

export const CustomStatusResponse = z.object({
	text: z.string().nullish().describe('The custom status message text'),
	expires_at: z.iso.datetime().nullish().describe('ISO8601 timestamp of when the custom status expires'),
	emoji_id: SnowflakeStringType.nullish().describe('The ID of the custom emoji used in the status'),
	emoji_name: z.string().nullish().describe('The name of the emoji used in the status'),
	emoji_animated: z.boolean().describe('Whether the status emoji is animated'),
});

export type CustomStatusResponse = z.infer<typeof CustomStatusResponse>;

export const UserActivityTimestamps = z
	.object({
		start: z.number().int().optional().describe('Unix timestamp in seconds when the activity started'),
		end: z.number().int().optional().describe('Unix timestamp in seconds when the activity ends'),
	})
	.describe('Activity timestamp metadata');

export type UserActivityTimestamps = z.infer<typeof UserActivityTimestamps>;

export const UserActivityAssets = z
	.object({
		large_image: z.string().max(256).optional(),
		large_text: z.string().max(128).optional(),
		small_image: z.string().max(256).optional(),
		small_text: z.string().max(128).optional(),
	})
	.describe('Activity image assets');

export type UserActivityAssets = z.infer<typeof UserActivityAssets>;

export const UserActivitySchema = z.object({
	name: z.string().max(128).describe('Activity name, usually the application or media title'),
	type: z.number().int().min(0).max(5).describe('Activity type'),
	application_id: SnowflakeStringType.optional().describe('Application ID for rich presence'),
	details: z.string().max(128).optional().describe('First line of rich presence text'),
	state: z.string().max(128).optional().describe('Second line of rich presence text'),
	timestamps: UserActivityTimestamps.optional(),
	assets: UserActivityAssets.optional(),
});

export type UserActivity = z.infer<typeof UserActivitySchema>;

const GuildFolderIconSchema = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			[GuildFolderIcons.FOLDER, 'FOLDER', 'Classic folder icon'],
			[GuildFolderIcons.STAR, 'STAR', 'Star icon'],
			[GuildFolderIcons.HEART, 'HEART', 'Heart icon'],
			[GuildFolderIcons.BOOKMARK, 'BOOKMARK', 'Bookmark icon'],
			[GuildFolderIcons.GAME_CONTROLLER, 'GAME_CONTROLLER', 'Game controller icon'],
			[GuildFolderIcons.SHIELD, 'SHIELD', 'Shield icon'],
			[GuildFolderIcons.MUSIC_NOTE, 'MUSIC_NOTE', 'Music note icon'],
		] as const,
		'Guild folder icon',
	),
	'GuildFolderIconType',
);
export const UserSettingsResponse = z.object({
	status: z.string().describe('The current online status of the user'),
	status_resets_at: z.iso.datetime().nullish().describe('ISO8601 timestamp of when the status will reset'),
	status_resets_to: z.string().nullish().describe('The status to reset to after the scheduled reset'),
	theme: z.string().describe('The UI theme preference'),
	locale: LocaleSchema,
	restricted_guilds: z.array(SnowflakeStringType).describe('Guild IDs where direct messages are restricted'),
	bot_restricted_guilds: z.array(SnowflakeStringType).describe('Guild IDs where bot direct messages are restricted'),
	default_guilds_restricted: z.boolean().describe('Whether new guilds have DM restrictions by default'),
	bot_default_guilds_restricted: z.boolean().describe('Whether new guilds have bot DM restrictions by default'),
	inline_attachment_media: z.boolean().describe('Whether to display attachments inline in chat'),
	inline_embed_media: z.boolean().describe('Whether to display embed media inline in chat'),
	gif_auto_play: z.boolean().describe('Whether GIFs auto-play in chat'),
	render_embeds: z.boolean().describe('Whether to render message embeds'),
	render_reactions: z.boolean().describe('Whether to display reactions on messages'),
	animate_emoji: z.boolean().describe('Whether to animate custom emoji'),
	animate_stickers: withFieldDescription(StickerAnimationOptionsSchema, 'Sticker animation preference setting'),
	render_spoilers: withFieldDescription(RenderSpoilersSchema, 'Spoiler rendering preference setting'),
	message_display_compact: z.boolean().describe('Whether to use compact message display mode'),
	friend_source_flags: createBitflagInt32Type(
		FriendSourceFlags,
		FriendSourceFlagsDescriptions,
		'Bitfield for friend request source permissions',
		'FriendSourceFlags',
	),
	incoming_call_flags: createBitflagInt32Type(
		IncomingCallFlags,
		IncomingCallFlagsDescriptions,
		'Bitfield for incoming call notification settings',
		'IncomingCallFlags',
	),
	group_dm_add_permission_flags: createBitflagInt32Type(
		GroupDmAddPermissionFlags,
		GroupDmAddPermissionFlagsDescriptions,
		'Bitfield for group DM add permissions',
		'GroupDmAddPermissionFlags',
	),
	guild_folders: z
		.array(
			z.object({
				id: SignedInt32Type.nullish().describe('The unique identifier for the folder (-1 for uncategorized)'),
				name: z.string().nullish().describe('The display name of the folder'),
				color: Int32Type.nullish().describe('The color of the folder as an integer'),
				flags: createBitflagInt32Type(
					GuildFolderFlags,
					GuildFolderFlagsDescriptions,
					'Bitfield for guild folder display behaviour',
					'GuildFolderFlags',
				)
					.default(0)
					.describe('Bitfield for guild folder display behaviour'),
				icon: GuildFolderIconSchema.default(DEFAULT_GUILD_FOLDER_ICON).describe('Selected icon for the guild folder'),
				guild_ids: z.array(SnowflakeStringType).describe('The IDs of guilds contained in this folder'),
			}),
		)
		.describe('The folder structure for organizing guilds in the sidebar'),
	custom_status: CustomStatusResponse.nullable().describe('The custom status set by the user'),
	afk_timeout: Int32Type.describe('The idle timeout in seconds before going AFK'),
	time_format: withFieldDescription(TimeFormatTypesSchema, 'The preferred time format setting'),
	developer_mode: z.boolean().describe('Whether developer mode is enabled'),
	trusted_domains: z.array(z.string()).describe('List of trusted external link domains'),
	default_hide_muted_channels: z.boolean().describe('Whether muted channels are hidden by default in new guilds'),
	sensitive_content_friend_dm_filter: withFieldDescription(
		SensitiveMediaFilterLevelSchema,
		'Sensitive media filter level for DMs from friends',
	),
	sensitive_content_non_friend_dm_filter: withFieldDescription(
		SensitiveMediaFilterLevelSchema,
		'Sensitive media filter level for DMs from non-friends',
	),
	sensitive_content_guild_filter: withFieldDescription(
		SensitiveMediaGuildFilterLevelSchema,
		'Sensitive media filter level for community channels',
	),
	suppress_unprivileged_self_mentions: z
		.boolean()
		.describe('Whether direct mentions and reply mentions from unprivileged users are suppressed'),
	suppress_unprivileged_self_mentions_bypass_user_ids: z
		.array(SnowflakeStringType)
		.describe('User IDs that bypass self-mention suppression'),
	staff_dm_access_user_ids: z.array(SnowflakeStringType).describe('User IDs with Staff DM Access enabled'),
	synced_preferences: z
		.string()
		.describe(
			'Account-wide client preferences as a base64-encoded protobuf snapshot. Empty string when nothing has been synced yet.',
		),
	profile_privacy: withFieldDescription(
		ProfilePrivacyLevelSchema,
		'Controls who sees the full profile: all guild members, only small-guild members, or only friends',
	),
	default_share_voice_activity: z
		.boolean()
		.describe(
			'Default value of share_voice_activity applied to newly accepted friend relationships. Read-only here; mutated via PUT /users/@me/settings/voice-activity-sharing.',
		),
});

export type UserSettingsResponse = z.infer<typeof UserSettingsResponse>;

const UserGuildMuteConfig = z
	.object({
		end_time: z.string().nullable().describe('ISO8601 timestamp of when the mute expires'),
		selected_time_window: Int32Type.describe('The selected mute duration in seconds'),
	})
	.nullable();
const UserGuildChannelOverride = z.object({
	collapsed: z.boolean().describe('Whether the channel category is collapsed in the sidebar'),
	message_notifications: withFieldDescription(
		UserNotificationSettingsSchema,
		'The notification level override for this channel',
	),
	muted: z.boolean().describe('Whether notifications are muted for this channel'),
	mute_config: UserGuildMuteConfig.describe('The mute configuration for this channel'),
	unread_badges: withFieldDescription(UserNotificationSettingsSchema, 'Unread badges level override for this channel')
		.nullish()
		.describe('Unread badges level override for this channel (null = inherit)'),
});
export const UserGuildSettingsResponse = z.object({
	guild_id: SnowflakeStringType.nullable().describe('The ID of the guild these settings apply to'),
	message_notifications: withFieldDescription(
		UserNotificationSettingsSchema,
		'The default notification level for the guild',
	),
	muted: z.boolean().describe('Whether the guild is muted'),
	mute_config: UserGuildMuteConfig.describe('The mute configuration for the guild'),
	mobile_push: z.boolean().describe('Whether mobile push notifications are enabled'),
	suppress_everyone: z.boolean().describe('Whether @everyone mentions are suppressed'),
	suppress_roles: z.boolean().describe('Whether role mentions are suppressed'),
	hide_muted_channels: z.boolean().describe('Whether muted channels are hidden in the sidebar'),
	channel_overrides: z
		.record(SnowflakeStringType, UserGuildChannelOverride)
		.nullable()
		.describe('Per-channel notification overrides'),
	unread_badges: withFieldDescription(UserNotificationSettingsSchema, 'Default unread badges level for the guild')
		.nullish()
		.describe('Default unread badges level for the guild (null = follows message_notifications)'),
	version: Int32Type.describe('The version number of these settings for sync'),
});

export type UserGuildSettingsResponse = z.infer<typeof UserGuildSettingsResponse>;

export const RelationshipResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier for the relationship'),
	type: withFieldDescription(RelationshipTypesSchema, 'The type of relationship (friend, blocked, pending, etc.)'),
	user: z.lazy(() => UserPartialResponse).describe('The user involved in this relationship'),
	since: z.iso.datetime().optional().describe('ISO8601 timestamp of when the relationship was established'),
	nickname: z.string().nullable().describe('A custom nickname set for the related user'),
	share_voice_activity: z
		.boolean()
		.describe(
			'Whether the current user has chosen to share their voice activity with this friend on the Active Now panel',
		),
	friend_shares_voice_activity: z
		.boolean()
		.describe(
			'Whether this friend has chosen to share their voice activity with the current user; for non-friend types this is always true',
		),
});

export type RelationshipResponse = z.infer<typeof RelationshipResponse>;
export type RequiredAction =
	| 'REQUIRE_VERIFIED_EMAIL'
	| 'REQUIRE_REVERIFIED_EMAIL'
	| 'REQUIRE_VERIFIED_PHONE'
	| 'REQUIRE_REVERIFIED_PHONE'
	| 'REQUIRE_VERIFIED_EMAIL_OR_VERIFIED_PHONE'
	| 'REQUIRE_REVERIFIED_EMAIL_OR_VERIFIED_PHONE'
	| 'REQUIRE_VERIFIED_EMAIL_OR_REVERIFIED_PHONE'
	| 'REQUIRE_REVERIFIED_EMAIL_OR_REVERIFIED_PHONE'
	| 'REQUIRE_INBOUND_PHONE_VERIFICATION';

export interface BackupCode {
	readonly code: string;
	readonly consumed: boolean;
}

export interface PendingBulkMessageDeletion {
	readonly scheduled_at: string;
	readonly channel_count: number;
	readonly message_count: number;
}

export interface UserProfile {
	readonly bio: string | null;
	readonly banner: string | null;
	readonly banner_color?: number | null;
	readonly pronouns: string | null;
	readonly accent_color: number | null;
}

export interface UserPartial {
	readonly id: string;
	readonly username: string;
	readonly discriminator: string;
	readonly global_name: string | null;
	readonly avatar: string | null;
	readonly avatar_color: number | null;
	readonly bot?: boolean;
	readonly system?: boolean;
	readonly flags: number;
	readonly mention_flags?: MentionReplyPreference;
}

export interface UserPrivate extends UserPartial, UserProfile {
	readonly is_staff: boolean;
	readonly email: string | null;
	readonly email_bounced?: boolean;
	readonly mfa_enabled: boolean;
	readonly phone?: string | null;
	readonly has_verified_phone: boolean;
	readonly authenticator_types: ReadonlyArray<number>;
	readonly verified: boolean;
	readonly premium_type: number | null;
	readonly premium_since: string | null;
	readonly premium_until: string | null;
	readonly premium_will_cancel: boolean;
	readonly premium_billing_cycle: string | null;
	readonly premium_lifetime_sequence: number | null;
	readonly premium_grace_ends_at: string | null;
	readonly premium_discriminator: boolean;
	readonly premium_badge_hidden: boolean;
	readonly premium_badge_masked: boolean;
	readonly premium_badge_timestamp_hidden: boolean;
	readonly premium_badge_sequence_hidden: boolean;
	readonly premium_purchase_disabled: boolean;
	readonly premium_enabled_override: boolean;
	readonly premium_perks_disabled: boolean;
	readonly force_inbound_phone_verification?: boolean;
	readonly timezone?: string | null;
	readonly timezone_privacy_flags?: number;
	readonly password_last_changed_at: string | null;
	readonly last_voice_activity_sharing_change_at: string | null;
	readonly required_actions: ReadonlyArray<RequiredAction>;
	readonly nsfw_allowed: boolean;
	readonly pending_bulk_message_deletion: PendingBulkMessageDeletion | null;
	readonly has_dismissed_premium_onboarding: boolean;
	readonly has_ever_purchased: boolean;
	readonly has_unread_gift_inventory: boolean;
	readonly unread_gift_inventory_count: number;
	readonly age_verified_adult?: boolean;
	readonly terms_agreed_at?: string | null;
	readonly privacy_agreed_at?: string | null;
	readonly traits: ReadonlyArray<string>;
}

export type User = UserPartial & Partial<UserPrivate>;

export const SavedMessageStatusSchema = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			['available', 'Available', 'The saved message is available and can be retrieved'],
			['missing_permissions', 'Missing Permissions', 'The user no longer has permission to view the message'],
		],
		'Availability status of a saved message',
	),
	'SavedMessageStatus',
);

export type SavedMessageStatus = z.infer<typeof SavedMessageStatusSchema>;

export const SavedMessageEntryResponse = z.object({
	id: SnowflakeStringType.describe('Unique identifier for the saved message entry'),
	channel_id: SnowflakeStringType.describe('ID of the channel containing the message'),
	message_id: SnowflakeStringType.describe('ID of the saved message'),
	status: SavedMessageStatusSchema.describe('Availability status of the saved message'),
	message: z
		.lazy(() => MessageResponseSchema)
		.nullable()
		.describe('The message content if available'),
});

export type SavedMessageEntryResponse = z.infer<typeof SavedMessageEntryResponse>;

export const SavedMessageEntryListResponse = z.array(SavedMessageEntryResponse);

export type SavedMessageEntryListResponse = z.infer<typeof SavedMessageEntryListResponse>;

export const EmailTokenResponse = z.object({
	email_token: createStringType(1, 256).describe('The email change token to use for updating email'),
});

export type EmailTokenResponse = z.infer<typeof EmailTokenResponse>;

export const UserTagCheckResponse = z.object({
	taken: z.boolean().describe('Whether the username/discriminator combination is already taken'),
});

export type UserTagCheckResponse = z.infer<typeof UserTagCheckResponse>;

const UserProfileDataResponse = z.object({
	bio: z.string().nullable().describe('User biography text'),
	pronouns: z.string().nullable().describe('User pronouns'),
	banner: z.string().nullable().describe('Hash of the profile banner image'),
	banner_color: Int32Type.nullable().optional().describe('Default banner color if no custom banner'),
	accent_color: Int32Type.nullable().describe('User-selected accent color'),
});

const GuildMemberProfileDataResponse = z
	.object({
		bio: z.string().nullable().describe('Guild-specific biography text'),
		pronouns: z.string().nullable().describe('Guild-specific pronouns'),
		banner: z.string().nullable().describe('Hash of the guild-specific banner image'),
		accent_color: Int32Type.nullable().describe('Guild-specific accent color'),
	})
	.nullable()
	.optional();

const MutualGuildResponse = z.object({
	id: SnowflakeStringType.describe('The ID of the mutual guild'),
	nick: z.string().nullable().describe('The nickname of the target user in this guild'),
});

export const UserProfileFullResponse = z.object({
	user: UserPartialResponse.describe('The user object'),
	user_profile: UserProfileDataResponse.describe('The user profile data'),
	guild_member: z
		.lazy(() => GuildMemberResponse)
		.optional()
		.describe('The guild member data if guild_id was provided'),
	guild_member_profile: GuildMemberProfileDataResponse.describe('Guild-specific profile data'),
	premium_type: withFieldDescription(UserPremiumTypesSchema, 'The type of premium subscription').optional(),
	premium_since: z.string().optional().describe('ISO8601 timestamp of when premium was activated'),
	premium_lifetime_sequence: Int32Type.optional().describe('Sequence number for lifetime premium'),
	mutual_friends: z.array(UserPartialResponse).optional().describe('Array of mutual friends'),
	mutual_guilds: z.array(MutualGuildResponse).optional().describe('Array of mutual guilds'),
	connected_accounts: z.array(ConnectionResponse).optional().describe('Array of verified external connections'),
	timezone_offset: SignedInt32Type.nullable().describe(
		"Current timezone offset in minutes from UTC for the target user's profile timezone, or null when hidden or unset",
	),
	profile_limited: z
		.boolean()
		.optional()
		.describe(
			'True when the target user has restricted their profile and the viewer does not meet the visibility tier; bio, pronouns, badges, and connected accounts have been stripped.',
		),
});

export type UserProfileFullResponse = z.infer<typeof UserProfileFullResponse>;

export const UserNotesRecordResponse = z
	.record(SnowflakeStringType, z.string())
	.describe('A map of user IDs to note text');

export type UserNotesRecordResponse = z.infer<typeof UserNotesRecordResponse>;

export const UserNoteResponse = z.object({
	note: z.string().describe('The note text for this user'),
});

export type UserNoteResponse = z.infer<typeof UserNoteResponse>;

export const RegisterMobileDeviceResponse = z.object({
	device_id: HexString32Type.describe('The unique identifier for the registered device'),
});

export type RegisterMobileDeviceResponse = z.infer<typeof RegisterMobileDeviceResponse>;

const MobileDeviceItemResponse = z.object({
	device_id: HexString32Type.describe('The unique identifier for the device'),
	platform: z.string().describe('The mobile push notification platform'),
	app_id: z.string().nullable().describe('Client app channel or bundle mapping identifier for this device'),
	provider_environment: z.string().nullable().describe('Push provider environment used for this device'),
	user_agent: z.string().nullable().describe('The user agent that registered this device'),
});

export const MobileDevicesListResponse = z.object({
	devices: z.array(MobileDeviceItemResponse).describe('Array of registered mobile push devices'),
});

export type MobileDevicesListResponse = z.infer<typeof MobileDevicesListResponse>;

export const PreloadMessagesResponse = z
	.record(SnowflakeStringType, z.lazy(() => MessageResponseSchema).nullable())
	.describe('A map of channel IDs to the latest message in each channel');

export type PreloadMessagesResponse = z.infer<typeof PreloadMessagesResponse>;

export const PushSubscribeResponse = z.object({
	subscription_id: z.string().describe('The unique identifier for the push subscription'),
});

export type PushSubscribeResponse = z.infer<typeof PushSubscribeResponse>;

const PushSubscriptionItemResponse = z.object({
	subscription_id: z.string().describe('The unique identifier for the push subscription'),
	user_agent: z.string().nullable().describe('The user agent that created this subscription'),
});

export const PushSubscriptionsListResponse = z.object({
	subscriptions: z.array(PushSubscriptionItemResponse).describe('Array of push notification subscriptions'),
});

export type PushSubscriptionsListResponse = z.infer<typeof PushSubscriptionsListResponse>;

export const BulkIgnoreFriendRequestsResponse = z.object({
	ignored_count: z.number().int(),
});

export type BulkIgnoreFriendRequestsResponse = z.infer<typeof BulkIgnoreFriendRequestsResponse>;
