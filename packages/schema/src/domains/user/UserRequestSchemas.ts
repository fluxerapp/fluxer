// SPDX-License-Identifier: AGPL-3.0-or-later

import {AVATAR_MAX_SIZE, MAX_GROUP_DM_OTHER_RECIPIENTS} from '@fluxer/constants/src/LimitConstants';
import {StatusTypes} from '@fluxer/constants/src/StatusConstants';
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
	ProfileFieldPrivacyFlags,
	ProfileFieldPrivacyFlagsDescriptions,
	ThemeTypes,
} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {SudoVerificationSchema} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {
	isValidSyncedPreferencesEncoding,
	SYNCED_PREFERENCES_MAX_ENCODED_LENGTH,
} from '@fluxer/schema/src/domains/user/SyncedPreferencesCodec';
import {isValidSingleUnicodeEmoji} from '@fluxer/schema/src/primitives/EmojiValidators';
import {createBase64StringType} from '@fluxer/schema/src/primitives/FileValidators';
import {LocaleSchema} from '@fluxer/schema/src/primitives/LocaleSchema';
import {createQueryIntegerType, DateTimeType, QueryBooleanType} from '@fluxer/schema/src/primitives/QueryValidators';
import {
	ColorType,
	createBitflagInt32Type,
	createNamedStringLiteralUnion,
	createStringType,
	Int32Type,
	SignedInt32Type,
	SnowflakeStringType,
	SnowflakeType,
	withFieldDescription,
	withOpenApiType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {URLType} from '@fluxer/schema/src/primitives/UrlValidators';
import {
	MentionReplyPreferencesSchema,
	ProfilePrivacyLevelSchema,
	RelationshipTypesSchema,
	RenderSpoilersSchema,
	SensitiveMediaFilterLevelSchema,
	SensitiveMediaGuildFilterLevelSchema,
	StickerAnimationOptionsSchema,
	TimeFormatTypesSchema,
	UserNotificationSettingsSchema,
} from '@fluxer/schema/src/primitives/UserSettingsValidators';
import {
	DiscriminatorType,
	EmailType,
	GlobalNameType,
	PasswordType,
	UsernameType,
} from '@fluxer/schema/src/primitives/UserValidators';
import {z} from 'zod';

export const UserUpdateRequest = z
	.object({
		username: UsernameType.describe('The username for the account (1-32 characters)'),
		discriminator: DiscriminatorType.describe('The 4-digit discriminator tag'),
		global_name: GlobalNameType.nullish().describe('The display name shown to other users'),
		email: EmailType.describe('The email address for the account'),
		new_password: PasswordType.describe('The new password to set'),
		password: PasswordType.describe('The current password for verification'),
		avatar: createBase64StringType(1, AVATAR_MAX_SIZE * 1.33)
			.nullish()
			.describe('Base64-encoded avatar image'),
		banner: createBase64StringType(1, AVATAR_MAX_SIZE * 1.33)
			.nullish()
			.describe('Base64-encoded profile banner image'),
		bio: createStringType(1, 320).nullish().describe('User biography text (max 320 characters)'),
		pronouns: createStringType(1, 40).nullish().describe('User pronouns (max 40 characters)'),
		accent_color: ColorType.nullish().describe('Profile accent color as integer'),
		timezone: createStringType(1, 128)
			.nullish()
			.describe('Staff-only IANA timezone identifier saved for profile local time. Ignored for non-staff users.'),
		timezone_privacy_flags: createBitflagInt32Type(
			ProfileFieldPrivacyFlags,
			ProfileFieldPrivacyFlagsDescriptions,
			'Bitfield controlling who can see the profile timezone',
			'ProfileFieldPrivacyFlags',
		).describe('Staff-only bitfield controlling who can see the profile timezone. Ignored for non-staff users.'),
		premium_badge_hidden: z.boolean().describe('Whether to hide the premium badge'),
		premium_badge_masked: z.boolean().describe('Whether to mask the premium badge'),
		premium_badge_timestamp_hidden: z.boolean().describe('Whether to hide premium badge timestamp'),
		premium_badge_sequence_hidden: z.boolean().describe('Whether to hide premium badge sequence'),
		premium_enabled_override: z.boolean().describe('Override premium enabled state'),
		has_dismissed_premium_onboarding: z.boolean().describe('Whether user dismissed premium onboarding'),
		has_unread_gift_inventory: z.boolean().describe('Whether user has unread gifts'),
		mention_flags: withFieldDescription(
			MentionReplyPreferencesSchema,
			'Account-wide reply mention preference (NO_PREFERENCE, PREFER_MENTION, PREFER_NO_MENTION)',
		),
	})
	.partial();

export type UserUpdateRequest = z.infer<typeof UserUpdateRequest>;

const EmailTokenType = createStringType(1, 256);
export const UserUpdateWithVerificationRequest = UserUpdateRequest.merge(
	z.object({
		email_token: EmailTokenType.optional().describe('Email change token for updating email'),
	}),
)
	.merge(SudoVerificationSchema)
	.superRefine((data, ctx) => {
		if (data.email !== undefined) {
			ctx.addIssue({
				code: 'custom',
				message: ValidationErrorCodes.EMAIL_MUST_BE_CHANGED_VIA_TOKEN,
				path: ['email'],
			});
		}
	});

export type UserUpdateWithVerificationRequest = z.infer<typeof UserUpdateWithVerificationRequest>;

export const EmailChangeTicketRequest = z.object({
	ticket: createStringType().describe('Email change ticket identifier'),
});

export type EmailChangeTicketRequest = z.infer<typeof EmailChangeTicketRequest>;

export const EmailChangeVerifyOriginalRequest = EmailChangeTicketRequest.extend({
	code: createStringType().describe('Verification code sent to the original email address'),
});

export type EmailChangeVerifyOriginalRequest = z.infer<typeof EmailChangeVerifyOriginalRequest>;

export const EmailChangeRequestNewRequest = EmailChangeTicketRequest.extend({
	new_email: EmailType.describe('New email address to switch to'),
	original_proof: createStringType().describe('Proof token obtained from verifying the original email'),
});

export type EmailChangeRequestNewRequest = z.infer<typeof EmailChangeRequestNewRequest>;

export const EmailChangeVerifyNewRequest = EmailChangeVerifyOriginalRequest.extend({
	original_proof: createStringType().describe('Proof token obtained from verifying the original email'),
});

export type EmailChangeVerifyNewRequest = z.infer<typeof EmailChangeVerifyNewRequest>;

export const EmailChangeApplyRequest = z
	.object({
		email_token: EmailTokenType.describe('Email change token returned from verify-new'),
	})
	.merge(SudoVerificationSchema);

export type EmailChangeApplyRequest = z.infer<typeof EmailChangeApplyRequest>;

export const EmailChangeBouncedRequestNewRequest = z.object({
	new_email: EmailType.describe('Replacement email address used when the current email has bounced'),
});

export type EmailChangeBouncedRequestNewRequest = z.infer<typeof EmailChangeBouncedRequestNewRequest>;

export const EmailChangeBouncedVerifyNewRequest = EmailChangeTicketRequest.extend({
	code: createStringType().describe('Verification code sent to the replacement email address'),
});

export type EmailChangeBouncedVerifyNewRequest = z.infer<typeof EmailChangeBouncedVerifyNewRequest>;

export const PasswordChangeTicketRequest = z.object({
	ticket: createStringType().describe('Password change ticket identifier'),
});

export type PasswordChangeTicketRequest = z.infer<typeof PasswordChangeTicketRequest>;

export const PasswordChangeVerifyRequest = PasswordChangeTicketRequest.extend({
	code: createStringType().describe('Verification code sent to the email address'),
});

export type PasswordChangeVerifyRequest = z.infer<typeof PasswordChangeVerifyRequest>;

export const PasswordChangeCompleteRequest = PasswordChangeTicketRequest.extend({
	verification_proof: createStringType().describe('Proof token obtained from verifying the email code'),
	new_password: PasswordType.describe('The new password to set'),
});

export type PasswordChangeCompleteRequest = z.infer<typeof PasswordChangeCompleteRequest>;

export const FriendRequestByTagRequest = z.object({
	username: UsernameType.describe('Username of the user to send friend request'),
	discriminator: DiscriminatorType.describe('Discriminator tag of the user'),
});

export type FriendRequestByTagRequest = z.infer<typeof FriendRequestByTagRequest>;

export const FriendRequestCreateRequest = z.preprocess(
	(value) => value ?? {},
	z
		.object({
			staff_force_accept: z.boolean().optional().describe('Staff-only: immediately create the friendship'),
		})
		.partial(),
);

export type FriendRequestCreateRequest = z.infer<typeof FriendRequestCreateRequest>;

export const RelationshipNicknameUpdateRequest = z.object({
	nickname: createStringType(0, 256).nullable().describe('Custom nickname for this friend (max 256 characters)'),
});

export type RelationshipNicknameUpdateRequest = z.infer<typeof RelationshipNicknameUpdateRequest>;

export const RelationshipTypePutRequest = z
	.object({
		type: withFieldDescription(RelationshipTypesSchema, 'Type of relationship to create').optional(),
	})
	.optional();

export type RelationshipTypePutRequest = z.infer<typeof RelationshipTypePutRequest>;

const CustomStatusPayloadSchema = z
	.object({
		text: createStringType(1, 128).nullish().describe('Custom status text (max 128 characters)'),
		expires_at: DateTimeType.nullish().describe('When the custom status expires'),
		emoji_id: SnowflakeType.nullish().describe('ID of custom emoji to display'),
		emoji_name: createStringType(1, 32)
			.nullish()
			.describe('Unicode emoji to display (ignored when emoji_id is provided)'),
	})
	.refine((value) => value.emoji_name == null || isValidSingleUnicodeEmoji(value.emoji_name), {
		message: 'Emoji name must be a valid Unicode emoji',
		path: ['emoji_name'],
	})
	.refine((value) => value.expires_at == null || value.expires_at.getTime() > Date.now(), {
		message: 'expires_at must be in the future',
		path: ['expires_at'],
	});
export const CustomStatusPayload = z.preprocess((value) => {
	if (value == null || typeof value !== 'object' || Array.isArray(value)) {
		return value;
	}
	const record = value as Record<string, unknown>;
	if (record.emoji_id == null) {
		return value;
	}
	const {emoji_name: _ignoredEmojiName, ...rest} = record;
	return rest;
}, CustomStatusPayloadSchema);

export type CustomStatusPayload = z.infer<typeof CustomStatusPayload>;

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
export const CreatePrivateChannelRequest = z
	.object({
		recipient_id: SnowflakeType.optional().describe('User ID for creating a DM channel'),
		recipients: z
			.array(SnowflakeType)
			.max(MAX_GROUP_DM_OTHER_RECIPIENTS)
			.optional()
			.describe(`Array of user IDs for creating a group DM (max ${MAX_GROUP_DM_OTHER_RECIPIENTS})`),
	})
	.refine(
		(data) =>
			(data.recipient_id != null && data.recipients == null) || (data.recipient_id == null && data.recipients != null),
		{
			message: 'Either recipient_id or recipients must be provided, but not both',
		},
	);

export type CreatePrivateChannelRequest = z.infer<typeof CreatePrivateChannelRequest>;

const GuildFolderSchema = z.object({
	id: SignedInt32Type.describe('Unique identifier for the folder (-1 for uncategorized)'),
	name: createStringType(0, 100).nullish().describe('Display name of the folder'),
	color: Int32Type.nullish().describe('Color of the folder as integer'),
	flags: createBitflagInt32Type(
		GuildFolderFlags,
		GuildFolderFlagsDescriptions,
		'Bitfield for guild folder display behaviour',
		'GuildFolderFlags',
	)
		.default(0)
		.describe('Bitfield for guild folder display behaviour'),
	icon: GuildFolderIconSchema.default(DEFAULT_GUILD_FOLDER_ICON).describe('Selected icon for the guild folder'),
	guild_ids: z.array(SnowflakeType).max(200).describe('Guild IDs in this folder'),
});
const UserStatusType = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			[StatusTypes.ONLINE, 'ONLINE', 'User is online and available'],
			[StatusTypes.DND, 'DND', 'Do not disturb – notifications are suppressed'],
			[StatusTypes.IDLE, 'IDLE', 'User is away or inactive'],
			[StatusTypes.INVISIBLE, 'INVISIBLE', 'User appears offline but can still receive messages'],
		] as const,
		'User online status',
	),
	'UserStatusType',
);
const UserThemeType = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			[ThemeTypes.DARK, 'DARK', 'Dark colour theme'],
			[ThemeTypes.DARK_LEGACY, 'DARK_LEGACY', 'Legacy dark colour theme (original neutral grey palette)'],
			[ThemeTypes.COAL, 'COAL', 'Coal/darker colour theme'],
			[ThemeTypes.LIGHT, 'LIGHT', 'Light colour theme'],
			[ThemeTypes.SYSTEM, 'SYSTEM', 'Follow system colour preference'],
		] as const,
		'UI theme preference',
	),
	'UserThemeType',
);
export const UserSettingsUpdateRequest = z
	.object({
		flags: createBitflagInt32Type(
			FriendSourceFlags,
			FriendSourceFlagsDescriptions,
			'Friend source flags',
			'FriendSourceFlags',
		),
		status: UserStatusType,
		status_resets_at: DateTimeType.nullish().describe('When status resets'),
		status_resets_to: UserStatusType.nullish(),
		theme: UserThemeType,
		locale: LocaleSchema,
		restricted_guilds: z.array(SnowflakeType).max(200).describe('Guilds with DM restrictions'),
		bot_restricted_guilds: z.array(SnowflakeType).max(200).describe('Guilds with bot DM restrictions'),
		default_guilds_restricted: z.boolean().describe('Default DM restriction for new guilds'),
		bot_default_guilds_restricted: z.boolean().describe('Default bot DM restriction for new guilds'),
		inline_attachment_media: z.boolean().describe('Display attachments inline'),
		inline_embed_media: z.boolean().describe('Display embed media inline'),
		gif_auto_play: z.boolean().describe('Auto-play GIFs'),
		render_embeds: z.boolean().describe('Render message embeds'),
		render_reactions: z.boolean().describe('Display reactions'),
		animate_emoji: z.boolean().describe('Animate custom emoji'),
		animate_stickers: withFieldDescription(StickerAnimationOptionsSchema, 'Sticker animation preference'),
		render_spoilers: withFieldDescription(RenderSpoilersSchema, 'Spoiler rendering preference'),
		message_display_compact: z.boolean().describe('Compact message display'),
		friend_source_flags: createBitflagInt32Type(
			FriendSourceFlags,
			FriendSourceFlagsDescriptions,
			'Friend request source permissions',
			'FriendSourceFlags',
		),
		incoming_call_flags: createBitflagInt32Type(
			IncomingCallFlags,
			IncomingCallFlagsDescriptions,
			'Incoming call settings',
			'IncomingCallFlags',
		),
		group_dm_add_permission_flags: createBitflagInt32Type(
			GroupDmAddPermissionFlags,
			GroupDmAddPermissionFlagsDescriptions,
			'Group DM add permissions',
			'GroupDmAddPermissionFlags',
		),
		guild_folders: z.array(GuildFolderSchema).max(200).describe('Guild folder organization'),
		custom_status: CustomStatusPayload.nullish().describe('Custom status'),
		afk_timeout: z.number().int().describe('AFK timeout in seconds'),
		time_format: withFieldDescription(TimeFormatTypesSchema, 'Time format preference'),
		developer_mode: z.boolean().describe('Developer mode enabled'),
		trusted_domains: z
			.array(z.string().min(1).max(253))
			.max(1000)
			.describe('Trusted external link domains. Use "*" to trust all domains.'),
		default_hide_muted_channels: z.boolean().describe('Hide muted channels by default in new guilds'),
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
			.describe('Suppress direct mentions and reply mentions from unprivileged users'),
		suppress_unprivileged_self_mentions_bypass_user_ids: z
			.array(SnowflakeType)
			.nullish()
			.describe('User IDs that bypass self-mention suppression'),
		staff_dm_access_user_ids: z.array(SnowflakeType).nullish().describe('User IDs with Staff DM Access enabled'),
		profile_privacy: withFieldDescription(
			ProfilePrivacyLevelSchema,
			'Controls who sees the full profile: all guild members, only small-guild members, or only friends',
		),
		synced_preferences: z
			.string()
			.max(SYNCED_PREFERENCES_MAX_ENCODED_LENGTH)
			.refine((value) => isValidSyncedPreferencesEncoding(value), {
				message: ValidationErrorCodes.INVALID_FORMAT,
			})
			.nullish()
			.describe(
				'Account-wide client preferences as a base64-encoded protobuf snapshot. Replaces the entire stored snapshot; pass null to clear it.',
			),
	})
	.partial();

export type UserSettingsUpdateRequest = z.infer<typeof UserSettingsUpdateRequest>;

const MuteConfigSchema = z
	.object({
		end_time: DateTimeType.nullish().describe('When the mute expires'),
		selected_time_window: z.number().int().describe('Selected mute duration'),
	})
	.nullish();
const ChannelOverrideSchema = z.object({
	collapsed: z.boolean().describe('Channel category collapsed'),
	message_notifications: withFieldDescription(UserNotificationSettingsSchema, 'Channel notification level'),
	muted: z.boolean().describe('Channel muted'),
	mute_config: MuteConfigSchema.describe('Channel mute configuration'),
	unread_badges: withFieldDescription(UserNotificationSettingsSchema, 'Unread badges level override for this channel')
		.nullish()
		.describe('Unread badges level override for this channel (null = inherit)'),
});
export const UserGuildSettingsUpdateRequest = z
	.object({
		message_notifications: withFieldDescription(UserNotificationSettingsSchema, 'Default guild notification level'),
		muted: z.boolean().describe('Guild muted'),
		mute_config: MuteConfigSchema.describe('Guild mute configuration'),
		mobile_push: z.boolean().describe('Mobile push notifications enabled'),
		suppress_everyone: z.boolean().describe('Suppress @everyone mentions'),
		suppress_roles: z.boolean().describe('Suppress role mentions'),
		hide_muted_channels: z.boolean().describe('Hide muted channels'),
		channel_overrides: z
			.record(SnowflakeStringType, ChannelOverrideSchema)
			.nullable()
			.describe('Per-channel overrides'),
		unread_badges: withFieldDescription(UserNotificationSettingsSchema, 'Default unread badges level for the guild')
			.nullish()
			.describe('Default unread badges level for the guild (null = follows message_notifications)'),
	})
	.partial();

export type UserGuildSettingsUpdateRequest = z.infer<typeof UserGuildSettingsUpdateRequest>;

export const EmptyBodyRequest = z.object({}).optional();

export type EmptyBodyRequest = z.infer<typeof EmptyBodyRequest>;

export const UserTagCheckQueryRequest = z.object({
	username: UsernameType.describe('The username to check'),
	discriminator: DiscriminatorType.describe('The discriminator to check'),
});

export type UserTagCheckQueryRequest = z.infer<typeof UserTagCheckQueryRequest>;

export const UserProfileQueryRequest = z.object({
	guild_id: SnowflakeType.optional().describe('Optional guild ID for guild-specific profile'),
	with_mutual_friends: QueryBooleanType.describe('Whether to include mutual friends'),
	with_mutual_guilds: QueryBooleanType.describe('Whether to include mutual guilds'),
});

export type UserProfileQueryRequest = z.infer<typeof UserProfileQueryRequest>;

export const UserNoteUpdateRequest = z.object({
	note: createStringType(1, 256).nullish().describe('The note text (max 256 characters)'),
});

export type UserNoteUpdateRequest = z.infer<typeof UserNoteUpdateRequest>;

const MobilePushPlatformSchema = createNamedStringLiteralUnion(
	[
		['android_fcm', 'ANDROID_FCM', 'Firebase Cloud Messaging (Android)'],
		['ios_apns', 'IOS_APNS', 'Apple Push Notification Service (iOS)'],
		['android_unified_push', 'ANDROID_UNIFIED_PUSH', 'UnifiedPush (Android without Google services)'],
	],
	'The mobile push notification platform',
);
const MobilePushProviderEnvironmentSchema = createNamedStringLiteralUnion(
	[
		['production', 'PRODUCTION', 'Production push provider environment'],
		['development', 'DEVELOPMENT', 'Development or sandbox push provider environment'],
	],
	'The push provider environment',
);
export const RegisterMobileDeviceRequest = z
	.object({
		platform: MobilePushPlatformSchema.describe('The mobile push notification platform'),
		token: createStringType(1, 4096).describe('The platform-specific push notification token or endpoint URL'),
		user_agent: createStringType(1, 1024).optional().describe('The user agent string identifying the device'),
		app_id: createStringType(1, 128)
			.optional()
			.describe('Client app channel or bundle mapping identifier, such as stable, beta, or canary'),
		provider_environment: MobilePushProviderEnvironmentSchema.optional().describe(
			'Push provider environment. For APNs, production uses api.push.apple.com and development uses api.sandbox.push.apple.com.',
		),
		encryption_key: createStringType(1, 1024)
			.optional()
			.describe('The P-256 ECDH public key for UnifiedPush encryption (base64url)'),
		auth_secret: createStringType(1, 1024)
			.optional()
			.describe('The authentication secret for UnifiedPush encryption (base64url)'),
	})
	.superRefine((value, ctx) => {
		if (value.platform !== 'android_unified_push') return;
		if (!value.encryption_key) {
			ctx.addIssue({
				code: z.ZodIssueCode.custom,
				path: ['encryption_key'],
				message: 'UnifiedPush registrations require encryption_key',
			});
		}
		if (!value.auth_secret) {
			ctx.addIssue({
				code: z.ZodIssueCode.custom,
				path: ['auth_secret'],
				message: 'UnifiedPush registrations require auth_secret',
			});
		}
	});

export type RegisterMobileDeviceRequest = z.infer<typeof RegisterMobileDeviceRequest>;

export const UnregisterMobileDeviceRequest = z.object({
	platform: MobilePushPlatformSchema.describe('The mobile push notification platform'),
	token: createStringType(1, 4096).describe('The platform-specific push notification token to unregister'),
	app_id: createStringType(1, 128)
		.optional()
		.describe('Client app channel or bundle mapping identifier, such as stable, beta, or canary'),
	provider_environment: MobilePushProviderEnvironmentSchema.optional().describe(
		'Push provider environment used for this registration',
	),
});

export type UnregisterMobileDeviceRequest = z.infer<typeof UnregisterMobileDeviceRequest>;

export const DeviceIdParam = z.object({
	device_id: createStringType(1, 256).describe('The ID of the mobile push device'),
});

export type DeviceIdParam = z.infer<typeof DeviceIdParam>;

export const PreloadMessagesRequest = z.object({
	channels: z.array(SnowflakeType).max(100).describe('Array of channel IDs to preload messages from (max 100)'),
});

export type PreloadMessagesRequest = z.infer<typeof PreloadMessagesRequest>;

export const UserMentionsQueryRequest = z.object({
	limit: createQueryIntegerType({minValue: 1, maxValue: 100, defaultValue: 25}).describe(
		'Maximum number of mentions to return (1-100, default 25)',
	),
	roles: QueryBooleanType.optional().default(true).describe('Whether to include role mentions'),
	everyone: QueryBooleanType.optional().default(true).describe('Whether to include @everyone mentions'),
	guilds: QueryBooleanType.optional().default(true).describe('Whether to include guild mentions'),
	before: SnowflakeType.optional().describe('Get mentions before this message ID'),
});

export type UserMentionsQueryRequest = z.infer<typeof UserMentionsQueryRequest>;

export const MarkMentionsReadRequest = z.object({
	message_ids: z
		.array(SnowflakeType)
		.min(1)
		.max(100)
		.describe('Recent mention message IDs to remove from the current user mention list'),
});

export type MarkMentionsReadRequest = z.infer<typeof MarkMentionsReadRequest>;

export const UserSavedMessagesQueryRequest = z.object({
	limit: createQueryIntegerType({minValue: 1, maxValue: 100, defaultValue: 25}).describe(
		'Maximum number of saved messages to return (1-100, default 25)',
	),
});

export type UserSavedMessagesQueryRequest = z.infer<typeof UserSavedMessagesQueryRequest>;

export const SaveMessageRequest = z.object({
	channel_id: SnowflakeType.describe('The ID of the channel containing the message'),
	message_id: SnowflakeType.describe('The ID of the message to save'),
});

export type SaveMessageRequest = z.infer<typeof SaveMessageRequest>;

export const PushSubscribeRequest = z.object({
	endpoint: URLType.describe('The push subscription endpoint URL'),
	keys: z
		.object({
			p256dh: createStringType(1, 256).describe('The P-256 ECDH public key (base64url)'),
			auth: createStringType(1, 256).describe('The authentication secret (base64url)'),
		})
		.describe('Encryption keys for the push subscription'),
	user_agent: createStringType(1, 1024).optional().describe('The user agent string identifying the client'),
});

export type PushSubscribeRequest = z.infer<typeof PushSubscribeRequest>;

export const PushRotateRequest = z.object({
	old_endpoint: URLType.describe('The previous push subscription endpoint URL being rotated out'),
	endpoint: URLType.describe('The new push subscription endpoint URL'),
	keys: z
		.object({
			p256dh: createStringType(1, 256).describe('The P-256 ECDH public key (base64url)'),
			auth: createStringType(1, 256).describe('The authentication secret (base64url)'),
		})
		.describe('Encryption keys for the new push subscription'),
	user_agent: createStringType(1, 1024).optional().describe('The user agent string identifying the client'),
});

export type PushRotateRequest = z.infer<typeof PushRotateRequest>;

export const SubscriptionIdParam = z.object({
	subscription_id: createStringType(1, 256).describe('The ID of the push subscription'),
});

export type SubscriptionIdParam = z.infer<typeof SubscriptionIdParam>;

export const BulkIgnoreFriendRequestsRequest = z.object({
	filter: z.enum(['all', 'new_accounts']).default('all'),
	max_account_age_seconds: z.number().int().positive().optional(),
});

export type BulkIgnoreFriendRequestsRequest = z.infer<typeof BulkIgnoreFriendRequestsRequest>;

const BulkDeleteSelfMessagesScope = createNamedStringLiteralUnion(
	[
		['selected', 'Selected', 'Delete messages matching the explicit include_* toggles and excluded_guild_ids.'],
		[
			'inaccessible_only',
			'Inaccessible Only',
			'Delete only messages in places the caller no longer has access to (guilds left or removed from, group DMs left). Direct messages are not affected by this mode since the caller can always reopen them.',
		],
	],
	'Which set of contexts the deletion targets',
);
const BulkDeleteSelfMessagesFilterShape = z.object({
	scope: BulkDeleteSelfMessagesScope.optional().default('selected'),
	include_dms: z.boolean().optional().default(true).describe('Include 1:1 direct messages the caller still has open.'),
	include_dms_closed: z
		.boolean()
		.optional()
		.default(true)
		.describe(
			'Include 1:1 direct messages the caller has previously closed. Independent of include_dms — set include_dms=false and include_dms_closed=true to target closed DMs only.',
		),
	include_group_dms: z
		.boolean()
		.optional()
		.default(true)
		.describe('Include group DMs the caller is still a member of.'),
	include_guilds: z
		.boolean()
		.optional()
		.default(true)
		.describe('Include text channels in guilds the caller is a member of.'),
	guild_filter_mode: createNamedStringLiteralUnion(
		[
			['exclude', 'Exclude', 'Apply to every guild except those listed in excluded_guild_ids.'],
			[
				'include_only',
				'Include Only',
				'Apply only to the guilds listed in included_guild_ids; all other guilds are left untouched.',
			],
		],
		'How the guild filter list is interpreted when include_guilds is true.',
	)
		.optional()
		.default('exclude'),
	excluded_guild_ids: z
		.array(SnowflakeType)
		.max(500)
		.optional()
		.default([])
		.describe(
			'Guild IDs to leave untouched. Used when include_guilds is true, guild_filter_mode is exclude, and scope is selected.',
		),
	included_guild_ids: z
		.array(SnowflakeType)
		.max(500)
		.optional()
		.default([])
		.describe(
			'The only guild IDs to apply this operation to. Used when include_guilds is true, guild_filter_mode is include_only, and scope is selected.',
		),
	start_date: z
		.string()
		.datetime()
		.nullable()
		.optional()
		.describe('Inclusive ISO8601 lower bound for message timestamps. Null/omitted means unbounded in the past.'),
	end_date: z
		.string()
		.datetime()
		.nullable()
		.optional()
		.describe('Exclusive ISO8601 upper bound for message timestamps. Null/omitted means unbounded in the future.'),
});

function applyBulkDeleteSelfMessagesRefinement<T extends z.ZodObject<z.ZodRawShape>>(schema: T) {
	return schema.superRefine((value, ctx) => {
		const cast = value as z.infer<typeof BulkDeleteSelfMessagesFilterShape> & Record<string, unknown>;
		if (cast.scope === 'selected') {
			const anyToggle = cast.include_dms || cast.include_dms_closed || cast.include_group_dms || cast.include_guilds;
			if (!anyToggle) {
				ctx.addIssue({
					code: z.ZodIssueCode.custom,
					message: 'Enable at least one of include_dms, include_dms_closed, include_group_dms, or include_guilds.',
					path: ['include_dms'],
				});
			}
		}
		if (cast.start_date && cast.end_date) {
			if (new Date(cast.start_date).getTime() >= new Date(cast.end_date).getTime()) {
				ctx.addIssue({
					code: z.ZodIssueCode.custom,
					message: 'start_date must be earlier than end_date.',
					path: ['end_date'],
				});
			}
		}
	});
}

export const BulkDeleteSelfMessagesFilter = applyBulkDeleteSelfMessagesRefinement(BulkDeleteSelfMessagesFilterShape);

export type BulkDeleteSelfMessagesFilter = z.infer<typeof BulkDeleteSelfMessagesFilter>;

export const BulkDeleteSelfMessagesRequest = applyBulkDeleteSelfMessagesRefinement(
	BulkDeleteSelfMessagesFilterShape.merge(SudoVerificationSchema),
);

export type BulkDeleteSelfMessagesRequest = z.infer<typeof BulkDeleteSelfMessagesRequest>;

export const HarvestSelfDataRequest = applyBulkDeleteSelfMessagesRefinement(BulkDeleteSelfMessagesFilterShape);

export type HarvestSelfDataRequest = z.infer<typeof HarvestSelfDataRequest>;

export const VoiceActivitySharingUpdateRequest = z
	.object({
		share_voice_activity: z
			.boolean()
			.describe(
				'New default for sharing voice activity with friends; also applied to every existing friend relationship',
			),
	})
	.describe(
		'Body for PUT /users/@me/settings/voice-activity-sharing. Mass-updates all friend relationships and starts a 24h cooldown.',
	);

export type VoiceActivitySharingUpdateRequest = z.infer<typeof VoiceActivitySharingUpdateRequest>;
