// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ContentWarningLevel,
	GuildExplicitContentFilterTypes,
	GuildMFALevel,
	GuildNSFWLevel,
	GuildSplashCardAlignment,
	GuildVerificationLevel,
	JoinSourceTypes,
} from '@fluxer/constants/src/GuildConstants';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {createInt32EnumType, createNamedLiteralUnion} from '@fluxer/schema/src/primitives/SchemaPrimitives';

export const GuildVerificationLevelSchema = createInt32EnumType(
	[
		[GuildVerificationLevel.NONE, 'NONE', 'Unrestricted'],
		[GuildVerificationLevel.LOW, 'LOW', 'Must have verified email'],
		[GuildVerificationLevel.MEDIUM, 'MEDIUM', 'Registered for more than 5 minutes'],
		[GuildVerificationLevel.HIGH, 'HIGH', 'Member of the server for more than 10 minutes'],
		[GuildVerificationLevel.VERY_HIGH, 'VERY_HIGH', 'Must have a verified phone number'],
	],
	'Required verification level for members',
	'GuildVerificationLevel',
);
export const GuildMFALevelSchema = createInt32EnumType(
	[
		[GuildMFALevel.NONE, 'NONE', 'Guild has no MFA requirement'],
		[GuildMFALevel.ELEVATED, 'ELEVATED', 'Guild requires 2FA for moderation actions'],
	],
	'Required MFA level for moderation actions',
	'GuildMFALevel',
);
export const GuildExplicitContentFilterSchema = createInt32EnumType(
	[
		[GuildExplicitContentFilterTypes.DISABLED, 'DISABLED', 'Media content will not be scanned'],
		[
			GuildExplicitContentFilterTypes.MEMBERS_WITHOUT_ROLES,
			'MEMBERS_WITHOUT_ROLES',
			'Media content from members without roles will be scanned',
		],
		[GuildExplicitContentFilterTypes.ALL_MEMBERS, 'ALL_MEMBERS', 'Media content from all members will be scanned'],
	],
	'Level of content filtering for explicit media',
	'GuildExplicitContentFilter',
);
export const DefaultMessageNotificationsSchema = createInt32EnumType(
	[
		[MessageNotifications.ALL_MESSAGES, 'ALL_MESSAGES', 'Notify on all messages'],
		[MessageNotifications.ONLY_MENTIONS, 'ONLY_MENTIONS', 'Notify only on mentions'],
	],
	'Default notification level for new members',
	'DefaultMessageNotifications',
);
export const NSFWLevelSchema = createInt32EnumType(
	[
		[GuildNSFWLevel.SAFE, 'SAFE', 'Guild is safe for all ages'],
		[GuildNSFWLevel.AGE_RESTRICTED, 'AGE_RESTRICTED', 'Guild is age-restricted'],
	],
	'The NSFW level of the guild',
	'NSFWLevel',
);
export const ContentWarningLevelSchema = createInt32EnumType(
	[
		[ContentWarningLevel.INHERIT, 'INHERIT', 'No level set on this scope; channels and categories inherit from parent'],
		[ContentWarningLevel.CONTENT_WARNING, 'CONTENT_WARNING', 'Show a content warning before entering'],
	],
	'The content warning level for a guild, category, or channel',
	'ContentWarningLevel',
);
export const SplashCardAlignmentSchema = createNamedLiteralUnion(
	[
		[GuildSplashCardAlignment.CENTER, 'CENTER', 'Splash card is centred'],
		[GuildSplashCardAlignment.LEFT, 'LEFT', 'Splash card is aligned to the left'],
		[GuildSplashCardAlignment.RIGHT, 'RIGHT', 'Splash card is aligned to the right'],
	] as const,
	'Alignment of the guild splash card',
);
export const JoinSourceTypeSchema = createInt32EnumType(
	[
		[JoinSourceTypes.CREATOR, 'CREATOR', 'Member created the guild'],
		[JoinSourceTypes.INSTANT_INVITE, 'INSTANT_INVITE', 'Member joined via an instant invite'],
		[JoinSourceTypes.VANITY_URL, 'VANITY_URL', 'Member joined via the vanity URL'],
		[JoinSourceTypes.BOT_INVITE, 'BOT_INVITE', 'Member was added via a bot invite'],
		[JoinSourceTypes.ADMIN_FORCE_ADD, 'ADMIN_FORCE_ADD', 'Member was force-added by a platform administrator'],
		[JoinSourceTypes.DISCOVERY, 'DISCOVERY', 'Member joined via guild discovery'],
	],
	'How the member joined the guild',
	'JoinSourceType',
);
