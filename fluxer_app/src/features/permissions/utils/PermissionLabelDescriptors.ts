// SPDX-License-Identifier: AGPL-3.0-or-later

import {EVERYONE_MENTION, HERE_MENTION, ROLES_MENTION} from '@app/features/app/config/I18nDisplayConstants';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const MENTION_EVERYONE_HERE_AND_ROLES_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Use {everyoneMention}/{hereMention} and {rolesMention}',
	comment:
		'Permission description in the role/permission editor. Explains the Mention Everyone permission. The three placeholders render the literal @everyone, @here, and @role mention tokens and must not be translated.',
});
const ADMINISTRATOR_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Grants all permissions and bypasses channel restrictions. Highly sensitive.',
	comment:
		'Permission description in the role/permission editor for the Administrator permission. Warns that this grants every permission and overrides per-channel restrictions. Keep the warning tone explicit.',
});
const VIEW_ACTIVITY_LOG_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: "Read the community's activity log of changes and moderation actions.",
	comment:
		'Permission description in the role/permission editor for the View Activity Log permission. Refers to the per-community audit/activity log.',
});
const MANAGE_COMMUNITY_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Edit global settings like name, description, and icon.',
	comment:
		'Permission description in the role/permission editor for the Manage Community permission. Refers to top-level community settings, not per-channel.',
});
const MANAGE_ROLES_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Create, edit, or delete roles below your highest role. Also allows editing channel permission overwrites.',
	comment:
		'Permission description in the role/permission editor for the Manage Roles permission. Notes the standard role-hierarchy rule and that it also gates channel permission overwrites.',
});
const MANAGE_CHANNELS_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Create, edit, or delete channels and categories.',
	comment:
		'Permission description in the role/permission editor for the Manage Channels permission. Covers channel and category lifecycle operations.',
});
const CHANGE_OWN_NICKNAME_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Update your own nickname.',
	comment:
		'Permission description in the role/permission editor for the Change Own Nickname permission. The user can rename only themselves in this community.',
});
const MANAGE_NICKNAMES_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: "Change other members' nicknames.",
	comment:
		'Permission description in the role/permission editor for the Manage Nicknames permission. The user can rename other members in this community.',
});
const CREATE_EMOJI_AND_STICKERS_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Upload new emoji and stickers, and manage your own creations.',
	comment:
		'Permission description in the role/permission editor for the Create Emoji & Stickers permission. Lets the member upload new custom expressions and edit the ones they uploaded.',
});
const MANAGE_EMOJI_AND_STICKERS_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Edit or delete emoji and stickers created by other members.',
	comment:
		'Permission description in the role/permission editor for the Manage Emoji & Stickers permission. Lets the member moderate expressions uploaded by anyone in the community.',
});
const MANAGE_WEBHOOKS_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Create, edit, or delete webhooks.',
	comment:
		'Permission description in the role/permission editor for the Manage Webhooks permission, community-wide scope. Refers to outbound integration webhooks.',
});
const SEND_TTS_MESSAGES_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Send text-to-speech messages.',
	comment:
		'Permission description in the role/permission editor for the Send TTS Messages permission. Keep "text-to-speech" spelled out for clarity.',
});
const MANAGE_MESSAGES_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: "Delete other members' messages. Pinning is controlled separately.",
	comment:
		'Permission description in the role/permission editor for the Manage Messages permission. Notes that pinning has its own permission.',
});
const MENTION_EVERYONE_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: "Mention everyone or any role (even if the role isn't set to be mentionable).",
	comment:
		'Permission description in the role/permission editor for the Mention Everyone permission. Notes that this overrides the per-role "mentionable" flag.',
});
const USE_EXTERNAL_EMOJI_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Use emoji from other communities.',
	comment:
		'Permission description in the role/permission editor for the Use External Emoji permission. "Other communities" means custom emoji uploaded in a different Fluxer community.',
});
const ADD_REACTIONS_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Add new reactions to messages.',
	comment:
		'Permission description in the role/permission editor for the Add Reactions permission. "New" reaction means starting a new reaction emoji on a message (not stacking onto an existing one).',
});
const BYPASS_SLOWMODE_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Ignore per-channel message rate limits.',
	comment:
		'Permission description in the role/permission editor for the Bypass Slowmode permission. Slowmode is the per-channel cooldown between messages.',
});
const TIME_OUT_MEMBERS_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Prevent members from sending messages, reacting, and joining voice for a duration.',
	comment:
		'Permission description in the role/permission editor for the Time Out Members permission. Time out is a temporary moderation restriction.',
});
const VIEW_CHANNEL_MEMBERS_GUILD_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'See the member list for channels in this community.',
	comment:
		'Permission description in the role/permission editor for the View Channel Members permission at community scope. Covers all channels in the community.',
});
const USE_VOICE_ACTIVITY_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Without this permission, push-to-talk is required.',
	comment:
		'Permission description in the role/permission editor for the Use Voice Activity permission. Explains the inverse: without it, the user must use push-to-talk in voice channels.',
});
const MOVE_MEMBERS_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Drag members between channels they can access.',
	comment:
		'Permission description in the role/permission editor for the Move Members permission. The target channel must already be accessible to the member being moved.',
});
const MANAGE_CHANNEL_CHANNEL_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: "Rename and edit this channel's settings.",
	comment:
		'Permission description in the channel-scoped permissions editor for the Manage Channel permission. "This channel" refers to the channel currently being edited.',
});
const MANAGE_PERMISSIONS_CHANNEL_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Edit overwrites for roles and members in this channel.',
	comment:
		'Permission description in the channel-scoped permissions editor for the Manage Permissions permission. "Overwrites" are per-channel permission overrides.',
});
const MANAGE_WEBHOOKS_CHANNEL_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Create, edit, or delete webhooks for this channel.',
	comment:
		'Permission description in the channel-scoped permissions editor for the Manage Webhooks permission. Scoped to a single channel.',
});
const VIEW_CHANNEL_MEMBERS_CHANNEL_PERMISSION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'See the member list for this channel.',
	comment:
		'Permission description in the channel-scoped permissions editor for the View Channel Members permission. Scoped to a single channel.',
});
const COMMUNITY_WIDE_DESCRIPTOR = msg({
	message: 'Community-wide',
	comment: 'Permission category for permissions that affect the whole community.',
});
const MESSAGES_AND_MEDIA_DESCRIPTOR = msg({
	message: 'Messages & media',
	comment: 'Permission category for chat messages, uploads, embeds, reactions, and media.',
});
const MODERATION_DESCRIPTOR = msg({
	message: 'Moderation',
	comment: 'Permission category for moderation actions against members or messages.',
});
const CHANNEL_ACCESS_DESCRIPTOR = msg({
	message: 'Channel access',
	comment: 'Permission category for viewing or entering channels.',
});
const AUDIO_AND_VIDEO_DESCRIPTOR = msg({
	message: 'Audio & video',
	comment: 'Permission category for voice, video, and streaming permissions.',
});
const CHANNEL_MANAGEMENT_DESCRIPTOR = msg({
	message: 'Channel management',
	comment: 'Permission category for managing channels and channel settings.',
});
const ADMINISTRATOR_DESCRIPTOR = msg({
	message: 'Administrator',
	comment: 'Permission name: grants all permissions and bypasses restrictions.',
});
const VIEW_ACTIVITY_LOG_DESCRIPTOR = msg({
	message: 'View activity log',
	comment: 'Permission name: allows viewing community audit/activity logs.',
});
const MANAGE_COMMUNITY_DESCRIPTOR = msg({
	message: 'Manage community',
	comment: 'Permission name: allows editing global community settings.',
});
const MANAGE_ROLES_DESCRIPTOR = msg({
	message: 'Manage roles',
	comment: 'Permission name: allows role management and permission overwrite editing.',
});
const MANAGE_CHANNELS_DESCRIPTOR = msg({
	message: 'Manage channels',
	comment: 'Permission name: allows creating, editing, or deleting channels.',
});
const KICK_MEMBERS_DESCRIPTOR = msg({
	message: 'Kick members',
	comment: 'Permission name: allows removing members from a community.',
});
const BAN_MEMBERS_DESCRIPTOR = msg({
	message: 'Ban members',
	comment: 'Permission name: allows banning members from a community.',
});
const CREATE_INVITE_LINKS_DESCRIPTOR = msg({
	message: 'Create invite links',
	comment: 'Permission name: allows creating invite links.',
});
const CHANGE_OWN_NICKNAME_DESCRIPTOR = msg({
	message: 'Change own nickname',
	comment: 'Permission name: allows a member to edit their own nickname.',
});
const MANAGE_NICKNAMES_DESCRIPTOR = msg({
	message: 'Manage nicknames',
	comment: "Permission name: allows changing other members' nicknames.",
});
const CREATE_EMOJI_AND_STICKERS_DESCRIPTOR = msg({
	message: 'Create emoji & stickers',
	comment: 'Permission name: allows uploading custom emoji and stickers.',
});
const MANAGE_EMOJI_AND_STICKERS_DESCRIPTOR = msg({
	message: 'Manage emoji & stickers',
	comment: 'Permission name: allows editing or deleting custom emoji and stickers.',
});
const MANAGE_WEBHOOKS_DESCRIPTOR = msg({
	message: 'Manage webhooks',
	comment: 'Permission name: allows creating, editing, or deleting webhooks.',
});
const SEND_MESSAGES_DESCRIPTOR = msg({
	message: 'Send messages',
	comment: 'Permission name: allows sending chat messages.',
});
const SEND_TTS_MESSAGES_DESCRIPTOR = msg({
	message: 'Send TTS messages',
	comment: 'Permission name: allows sending text-to-speech messages.',
});
const MANAGE_MESSAGES_DESCRIPTOR = msg({
	message: 'Manage messages',
	comment: "Permission name: allows deleting other members' messages.",
});
const PIN_MESSAGES_DESCRIPTOR = msg({
	message: 'Pin messages',
	comment: 'Permission name: allows pinning messages in a channel.',
});
const EMBED_LINKS_DESCRIPTOR = msg({
	message: 'Embed links',
	comment: 'Permission name: allows links to unfurl/embed in messages.',
});
const ATTACH_FILES_DESCRIPTOR = msg({
	message: 'Attach files',
	comment: 'Permission name: allows uploading files to messages.',
});
const CREATE_POLLS_DESCRIPTOR = msg({
	message: 'Create polls',
	comment: 'Permission name: allows creating polls.',
});
const SEE_VOTES_ON_ANONYMOUS_POLLS_DESCRIPTOR = msg({
	message: 'See votes on anonymous polls',
	comment: 'Permission name: allows looking at votes on anonymous polls.',
});
const READ_MESSAGE_HISTORY_DESCRIPTOR = msg({
	message: 'Read message history',
	comment: 'Permission name: allows seeing earlier messages in a channel.',
});
const USE_EXTERNAL_EMOJI_DESCRIPTOR = msg({
	message: 'Use external emoji',
	comment: 'Permission name: allows using emoji from other communities.',
});
const USE_EXTERNAL_STICKERS_DESCRIPTOR = msg({
	message: 'Use external stickers',
	comment: 'Permission name: allows using stickers from other communities.',
});
const ADD_REACTIONS_DESCRIPTOR = msg({
	message: 'Add reactions',
	comment: 'Permission name: allows adding reactions to messages.',
});
const BYPASS_SLOWMODE_DESCRIPTOR = msg({
	message: 'Bypass slowmode',
	comment: 'Permission name: allows ignoring per-channel message rate limits.',
});
const TIME_OUT_MEMBERS_DESCRIPTOR = msg({
	message: 'Time out members',
	comment: 'Permission name: allows temporarily restricting members.',
});
const VIEW_CHANNEL_DESCRIPTOR = msg({
	message: 'View channel',
	comment: 'Permission name: allows seeing a channel.',
});
const VIEW_CHANNEL_MEMBERS_DESCRIPTOR = msg({
	message: 'View channel members',
	comment: 'Permission name: allows seeing the member list for a channel.',
});
const CONNECT_TO_VOICE_DESCRIPTOR = msg({
	message: 'Connect to voice',
	comment: 'Permission name: allows joining voice channels.',
});
const SPEAK_DESCRIPTOR = msg({
	message: 'Speak',
	context: 'voice-permission',
	comment: 'Permission name: allows speaking in voice channels.',
});
const STREAM_VIDEO_DESCRIPTOR = msg({
	message: 'Stream video',
	comment: 'Permission name: allows video streaming or screen sharing in voice.',
});
const USE_VOICE_ACTIVITY_DESCRIPTOR = msg({
	message: 'Use voice activity',
	comment: 'Permission name: allows voice activation instead of requiring push-to-talk.',
});
const PRIORITY_SPEAKER_DESCRIPTOR = msg({
	message: 'Priority speaker',
	comment: 'Permission name: allows a voice participant to be heard over others.',
});
const MUTE_MEMBERS_DESCRIPTOR = msg({
	message: 'Mute members',
	comment: 'Permission name: allows moderators to mute other voice participants.',
});
const DEAFEN_MEMBERS_DESCRIPTOR = msg({
	message: 'Deafen members',
	comment: 'Permission name: allows moderators to deafen other voice participants.',
});
const MOVE_MEMBERS_DESCRIPTOR = msg({
	message: 'Move members',
	comment: 'Permission name: allows moving members between voice channels.',
});
const SET_VOICE_REGION_DESCRIPTOR = msg({
	message: 'Set voice region',
	comment: 'Permission name: allows changing the voice service region.',
});
const MANAGE_CHANNEL_DESCRIPTOR = msg({
	message: 'Manage channel',
	comment: 'Channel-scoped permission name: allows editing this channel.',
});
const MANAGE_PERMISSIONS_DESCRIPTOR = msg({
	message: 'Manage permissions',
	comment: 'Channel-scoped permission name: allows editing permission overwrites.',
});

export type PermissionCategory =
	| 'communityWide'
	| 'messagesMedia'
	| 'moderation'
	| 'channelAccess'
	| 'audioVideo'
	| 'channelManagement';
export type PermissionScope = 'guild' | 'channel';

const PERMISSION_CATEGORY_DESCRIPTORS: Record<PermissionCategory, MessageDescriptor> = {
	communityWide: COMMUNITY_WIDE_DESCRIPTOR,
	messagesMedia: MESSAGES_AND_MEDIA_DESCRIPTOR,
	moderation: MODERATION_DESCRIPTOR,
	channelAccess: CHANNEL_ACCESS_DESCRIPTOR,
	audioVideo: AUDIO_AND_VIDEO_DESCRIPTOR,
	channelManagement: CHANNEL_MANAGEMENT_DESCRIPTOR,
};
const PERMISSION_TITLE_DESCRIPTORS = new Map<bigint, MessageDescriptor>([
	[Permissions.ADMINISTRATOR, ADMINISTRATOR_DESCRIPTOR],
	[Permissions.VIEW_AUDIT_LOG, VIEW_ACTIVITY_LOG_DESCRIPTOR],
	[Permissions.MANAGE_GUILD, MANAGE_COMMUNITY_DESCRIPTOR],
	[Permissions.MANAGE_ROLES, MANAGE_ROLES_DESCRIPTOR],
	[Permissions.MANAGE_CHANNELS, MANAGE_CHANNELS_DESCRIPTOR],
	[Permissions.KICK_MEMBERS, KICK_MEMBERS_DESCRIPTOR],
	[Permissions.BAN_MEMBERS, BAN_MEMBERS_DESCRIPTOR],
	[Permissions.CREATE_INSTANT_INVITE, CREATE_INVITE_LINKS_DESCRIPTOR],
	[Permissions.CHANGE_NICKNAME, CHANGE_OWN_NICKNAME_DESCRIPTOR],
	[Permissions.MANAGE_NICKNAMES, MANAGE_NICKNAMES_DESCRIPTOR],
	[Permissions.CREATE_EXPRESSIONS, CREATE_EMOJI_AND_STICKERS_DESCRIPTOR],
	[Permissions.MANAGE_EXPRESSIONS, MANAGE_EMOJI_AND_STICKERS_DESCRIPTOR],
	[Permissions.MANAGE_WEBHOOKS, MANAGE_WEBHOOKS_DESCRIPTOR],
	[Permissions.SEND_MESSAGES, SEND_MESSAGES_DESCRIPTOR],
	[Permissions.SEND_TTS_MESSAGES, SEND_TTS_MESSAGES_DESCRIPTOR],
	[Permissions.MANAGE_MESSAGES, MANAGE_MESSAGES_DESCRIPTOR],
	[Permissions.PIN_MESSAGES, PIN_MESSAGES_DESCRIPTOR],
	[Permissions.EMBED_LINKS, EMBED_LINKS_DESCRIPTOR],
	[Permissions.ATTACH_FILES, ATTACH_FILES_DESCRIPTOR],
	[Permissions.SEND_POLLS, CREATE_POLLS_DESCRIPTOR],
	[Permissions.SEE_VOTES_ON_ANONYMOUS_POLLS, SEE_VOTES_ON_ANONYMOUS_POLLS_DESCRIPTOR],
	[Permissions.READ_MESSAGE_HISTORY, READ_MESSAGE_HISTORY_DESCRIPTOR],
	[
		Permissions.MENTION_EVERYONE,
		{
			...MENTION_EVERYONE_HERE_AND_ROLES_DESCRIPTION_DESCRIPTOR,
			values: {everyoneMention: EVERYONE_MENTION, hereMention: HERE_MENTION, rolesMention: ROLES_MENTION},
		},
	],
	[Permissions.USE_EXTERNAL_EMOJIS, USE_EXTERNAL_EMOJI_DESCRIPTOR],
	[Permissions.USE_EXTERNAL_STICKERS, USE_EXTERNAL_STICKERS_DESCRIPTOR],
	[Permissions.ADD_REACTIONS, ADD_REACTIONS_DESCRIPTOR],
	[Permissions.BYPASS_SLOWMODE, BYPASS_SLOWMODE_DESCRIPTOR],
	[Permissions.MODERATE_MEMBERS, TIME_OUT_MEMBERS_DESCRIPTOR],
	[Permissions.VIEW_CHANNEL, VIEW_CHANNEL_DESCRIPTOR],
	[Permissions.VIEW_CHANNEL_MEMBERS, VIEW_CHANNEL_MEMBERS_DESCRIPTOR],
	[Permissions.CONNECT, CONNECT_TO_VOICE_DESCRIPTOR],
	[Permissions.SPEAK, SPEAK_DESCRIPTOR],
	[Permissions.STREAM, STREAM_VIDEO_DESCRIPTOR],
	[Permissions.USE_VAD, USE_VOICE_ACTIVITY_DESCRIPTOR],
	[Permissions.PRIORITY_SPEAKER, PRIORITY_SPEAKER_DESCRIPTOR],
	[Permissions.MUTE_MEMBERS, MUTE_MEMBERS_DESCRIPTOR],
	[Permissions.DEAFEN_MEMBERS, DEAFEN_MEMBERS_DESCRIPTOR],
	[Permissions.MOVE_MEMBERS, MOVE_MEMBERS_DESCRIPTOR],
	[Permissions.UPDATE_RTC_REGION, SET_VOICE_REGION_DESCRIPTOR],
]);
const PERMISSION_DESCRIPTION_DESCRIPTORS = new Map<bigint, MessageDescriptor>([
	[Permissions.ADMINISTRATOR, ADMINISTRATOR_PERMISSION_DESCRIPTION_DESCRIPTOR],
	[Permissions.VIEW_AUDIT_LOG, VIEW_ACTIVITY_LOG_PERMISSION_DESCRIPTION_DESCRIPTOR],
	[Permissions.MANAGE_GUILD, MANAGE_COMMUNITY_PERMISSION_DESCRIPTION_DESCRIPTOR],
	[Permissions.MANAGE_ROLES, MANAGE_ROLES_PERMISSION_DESCRIPTION_DESCRIPTOR],
	[Permissions.MANAGE_CHANNELS, MANAGE_CHANNELS_PERMISSION_DESCRIPTION_DESCRIPTOR],
	[Permissions.CHANGE_NICKNAME, CHANGE_OWN_NICKNAME_PERMISSION_DESCRIPTION_DESCRIPTOR],
	[Permissions.MANAGE_NICKNAMES, MANAGE_NICKNAMES_PERMISSION_DESCRIPTION_DESCRIPTOR],
	[Permissions.CREATE_EXPRESSIONS, CREATE_EMOJI_AND_STICKERS_PERMISSION_DESCRIPTION_DESCRIPTOR],
	[Permissions.MANAGE_EXPRESSIONS, MANAGE_EMOJI_AND_STICKERS_PERMISSION_DESCRIPTION_DESCRIPTOR],
	[Permissions.MANAGE_WEBHOOKS, MANAGE_WEBHOOKS_PERMISSION_DESCRIPTION_DESCRIPTOR],
	[Permissions.SEND_TTS_MESSAGES, SEND_TTS_MESSAGES_PERMISSION_DESCRIPTION_DESCRIPTOR],
	[Permissions.MANAGE_MESSAGES, MANAGE_MESSAGES_PERMISSION_DESCRIPTION_DESCRIPTOR],
	[Permissions.MENTION_EVERYONE, MENTION_EVERYONE_PERMISSION_DESCRIPTION_DESCRIPTOR],
	[Permissions.USE_EXTERNAL_EMOJIS, USE_EXTERNAL_EMOJI_PERMISSION_DESCRIPTION_DESCRIPTOR],
	[Permissions.ADD_REACTIONS, ADD_REACTIONS_PERMISSION_DESCRIPTION_DESCRIPTOR],
	[Permissions.BYPASS_SLOWMODE, BYPASS_SLOWMODE_PERMISSION_DESCRIPTION_DESCRIPTOR],
	[Permissions.MODERATE_MEMBERS, TIME_OUT_MEMBERS_PERMISSION_DESCRIPTION_DESCRIPTOR],
	[Permissions.VIEW_CHANNEL_MEMBERS, VIEW_CHANNEL_MEMBERS_GUILD_PERMISSION_DESCRIPTION_DESCRIPTOR],
	[Permissions.USE_VAD, USE_VOICE_ACTIVITY_PERMISSION_DESCRIPTION_DESCRIPTOR],
	[Permissions.MOVE_MEMBERS, MOVE_MEMBERS_PERMISSION_DESCRIPTION_DESCRIPTOR],
]);
const CHANNEL_PERMISSION_TITLE_OVERRIDES = new Map<bigint, MessageDescriptor>([
	[Permissions.MANAGE_CHANNELS, MANAGE_CHANNEL_DESCRIPTOR],
	[Permissions.MANAGE_ROLES, MANAGE_PERMISSIONS_DESCRIPTOR],
]);
const CHANNEL_PERMISSION_DESCRIPTION_OVERRIDES = new Map<bigint, MessageDescriptor>([
	[Permissions.MANAGE_CHANNELS, MANAGE_CHANNEL_CHANNEL_PERMISSION_DESCRIPTION_DESCRIPTOR],
	[Permissions.MANAGE_ROLES, MANAGE_PERMISSIONS_CHANNEL_PERMISSION_DESCRIPTION_DESCRIPTOR],
	[Permissions.MANAGE_WEBHOOKS, MANAGE_WEBHOOKS_CHANNEL_PERMISSION_DESCRIPTION_DESCRIPTOR],
	[Permissions.VIEW_CHANNEL_MEMBERS, VIEW_CHANNEL_MEMBERS_CHANNEL_PERMISSION_DESCRIPTION_DESCRIPTOR],
]);

export function getPermissionTitleDescriptor(
	permission: bigint,
	scope: PermissionScope = 'guild',
): MessageDescriptor | null {
	if (scope === 'channel') {
		const channelDescriptor = CHANNEL_PERMISSION_TITLE_OVERRIDES.get(permission);
		if (channelDescriptor != null) return channelDescriptor;
	}
	return PERMISSION_TITLE_DESCRIPTORS.get(permission) ?? null;
}

export function getPermissionDescriptionDescriptor(
	permission: bigint,
	scope: PermissionScope = 'guild',
): MessageDescriptor | null {
	if (scope === 'channel') {
		const channelDescriptor = CHANNEL_PERMISSION_DESCRIPTION_OVERRIDES.get(permission);
		if (channelDescriptor != null) return channelDescriptor;
	}
	return PERMISSION_DESCRIPTION_DESCRIPTORS.get(permission) ?? null;
}

export function formatPermissionCategoryLabel(i18n: I18n, category: PermissionCategory): string {
	return i18n._(PERMISSION_CATEGORY_DESCRIPTORS[category]);
}
