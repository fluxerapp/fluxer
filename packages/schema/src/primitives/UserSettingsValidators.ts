// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	GuildFolderIcons,
	MentionReplyPreferences,
	MentionReplyPreferencesDescriptions,
	ProfilePrivacyLevels,
	ProfilePrivacyLevelsDescriptions,
	RelationshipTypes,
	RelationshipTypesDescriptions,
	RenderSpoilers,
	RenderSpoilersDescriptions,
	SensitiveMediaFilterLevel,
	SensitiveMediaFilterLevelDescriptions,
	StickerAnimationOptions,
	StickerAnimationOptionsDescriptions,
	TimeFormatTypes,
	TimeFormatTypesDescriptions,
	UserAuthenticatorTypes,
	UserAuthenticatorTypesDescriptions,
	UserNotificationSettings,
	UserNotificationSettingsDescriptions,
	UserPremiumTypes,
	UserPremiumTypesDescriptions,
} from '@fluxer/constants/src/UserConstants';
import {
	createInt32EnumType,
	createNamedStringLiteralUnion,
	withOpenApiType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';

export const GuildFolderIconSchema = withOpenApiType(
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

export const StickerAnimationOptionsSchema = createInt32EnumType(
	[
		[StickerAnimationOptions.ALWAYS_ANIMATE, 'ALWAYS_ANIMATE', StickerAnimationOptionsDescriptions.ALWAYS_ANIMATE],
		[
			StickerAnimationOptions.ANIMATE_ON_INTERACTION,
			'ANIMATE_ON_INTERACTION',
			StickerAnimationOptionsDescriptions.ANIMATE_ON_INTERACTION,
		],
		[StickerAnimationOptions.NEVER_ANIMATE, 'NEVER_ANIMATE', StickerAnimationOptionsDescriptions.NEVER_ANIMATE],
	],
	'Sticker animation preference',
	'StickerAnimationOptions',
);
export const RenderSpoilersSchema = createInt32EnumType(
	[
		[RenderSpoilers.ALWAYS, 'ALWAYS', RenderSpoilersDescriptions.ALWAYS],
		[RenderSpoilers.ON_CLICK, 'ON_CLICK', RenderSpoilersDescriptions.ON_CLICK],
		[RenderSpoilers.IF_MODERATOR, 'IF_MODERATOR', RenderSpoilersDescriptions.IF_MODERATOR],
	],
	'Spoiler rendering preference',
	'RenderSpoilers',
);
export const TimeFormatTypesSchema = createInt32EnumType(
	[
		[TimeFormatTypes.AUTO, 'AUTO', TimeFormatTypesDescriptions.AUTO],
		[TimeFormatTypes.TWELVE_HOUR, 'TWELVE_HOUR', TimeFormatTypesDescriptions.TWELVE_HOUR],
		[TimeFormatTypes.TWENTY_FOUR_HOUR, 'TWENTY_FOUR_HOUR', TimeFormatTypesDescriptions.TWENTY_FOUR_HOUR],
	],
	'Time format preference',
	'TimeFormatTypes',
);
export const UserNotificationSettingsSchema = createInt32EnumType(
	[
		[UserNotificationSettings.ALL_MESSAGES, 'ALL_MESSAGES', UserNotificationSettingsDescriptions.ALL_MESSAGES],
		[UserNotificationSettings.ONLY_MENTIONS, 'ONLY_MENTIONS', UserNotificationSettingsDescriptions.ONLY_MENTIONS],
		[UserNotificationSettings.NO_MESSAGES, 'NO_MESSAGES', UserNotificationSettingsDescriptions.NO_MESSAGES],
		[UserNotificationSettings.INHERIT, 'INHERIT', UserNotificationSettingsDescriptions.INHERIT],
	],
	'Notification level preference',
	'UserNotificationSettings',
);
export const RelationshipTypesSchema = createInt32EnumType(
	[
		[RelationshipTypes.FRIEND, 'FRIEND', RelationshipTypesDescriptions.FRIEND],
		[RelationshipTypes.BLOCKED, 'BLOCKED', RelationshipTypesDescriptions.BLOCKED],
		[RelationshipTypes.INCOMING_REQUEST, 'INCOMING_REQUEST', RelationshipTypesDescriptions.INCOMING_REQUEST],
		[RelationshipTypes.OUTGOING_REQUEST, 'OUTGOING_REQUEST', RelationshipTypesDescriptions.OUTGOING_REQUEST],
	],
	'Relationship type',
	'RelationshipTypes',
);
export const UserPremiumTypesSchema = createInt32EnumType(
	[
		[UserPremiumTypes.NONE, 'NONE', UserPremiumTypesDescriptions.NONE],
		[UserPremiumTypes.SUBSCRIPTION, 'SUBSCRIPTION', UserPremiumTypesDescriptions.SUBSCRIPTION],
		[UserPremiumTypes.LIFETIME, 'LIFETIME', UserPremiumTypesDescriptions.LIFETIME],
	],
	'Premium subscription type',
	'UserPremiumTypes',
);
export const UserAuthenticatorTypesSchema = createInt32EnumType(
	[
		[UserAuthenticatorTypes.TOTP, 'TOTP', UserAuthenticatorTypesDescriptions.TOTP],
		[UserAuthenticatorTypes.WEBAUTHN, 'WEBAUTHN', UserAuthenticatorTypesDescriptions.WEBAUTHN],
	],
	'Authenticator type',
	'UserAuthenticatorTypes',
);
export const SensitiveMediaFilterLevelSchema = createInt32EnumType(
	[
		[SensitiveMediaFilterLevel.SHOW, 'SHOW', SensitiveMediaFilterLevelDescriptions.SHOW],
		[SensitiveMediaFilterLevel.BLUR, 'BLUR', SensitiveMediaFilterLevelDescriptions.BLUR],
		[SensitiveMediaFilterLevel.BLOCK, 'BLOCK', SensitiveMediaFilterLevelDescriptions.BLOCK],
	],
	'Sensitive media filter level',
	'SensitiveMediaFilterLevel',
);
export const SensitiveMediaGuildFilterLevelSchema = createInt32EnumType(
	[
		[SensitiveMediaFilterLevel.SHOW, 'SHOW', SensitiveMediaFilterLevelDescriptions.SHOW],
		[SensitiveMediaFilterLevel.BLUR, 'BLUR', SensitiveMediaFilterLevelDescriptions.BLUR],
	],
	'Sensitive media filter level for community channels',
	'SensitiveMediaGuildFilterLevel',
);
export const MentionReplyPreferencesSchema = createInt32EnumType(
	[
		[MentionReplyPreferences.NO_PREFERENCE, 'NO_PREFERENCE', MentionReplyPreferencesDescriptions.NO_PREFERENCE],
		[MentionReplyPreferences.PREFER_MENTION, 'PREFER_MENTION', MentionReplyPreferencesDescriptions.PREFER_MENTION],
		[
			MentionReplyPreferences.PREFER_NO_MENTION,
			'PREFER_NO_MENTION',
			MentionReplyPreferencesDescriptions.PREFER_NO_MENTION,
		],
	],
	'Reply mention preference',
	'MentionReplyPreferences',
);
export const ProfilePrivacyLevelSchema = createInt32EnumType(
	[
		[ProfilePrivacyLevels.ALL_GUILDS, 'ALL_GUILDS', ProfilePrivacyLevelsDescriptions.ALL_GUILDS],
		[ProfilePrivacyLevels.SMALL_GUILDS_ONLY, 'SMALL_GUILDS_ONLY', ProfilePrivacyLevelsDescriptions.SMALL_GUILDS_ONLY],
		[ProfilePrivacyLevels.FRIENDS_ONLY, 'FRIENDS_ONLY', ProfilePrivacyLevelsDescriptions.FRIENDS_ONLY],
	],
	'Profile privacy visibility level',
	'ProfilePrivacyLevel',
);
