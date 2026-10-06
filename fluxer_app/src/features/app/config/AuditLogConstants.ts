// SPDX-License-Identifier: AGPL-3.0-or-later

import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const COMMUNITY_UPDATED_DESCRIPTOR = msg({
	message: 'Community updated',
	comment: 'Audit log entry label. Past-tense action describing that the community settings were changed.',
});
const CHANNEL_CREATED_DESCRIPTOR = msg({
	message: 'Channel created',
	comment: 'Audit log entry label. Past-tense action describing that a channel was created.',
});
const CHANNEL_UPDATED_DESCRIPTOR = msg({
	message: 'Channel updated',
	comment: 'Audit log entry label. Past-tense action describing that a channel was edited.',
});
const CHANNEL_DELETED_DESCRIPTOR = msg({
	message: 'Channel deleted',
	comment: 'Audit log entry label. Past-tense action describing that a channel was deleted.',
});
const PERMISSION_OVERRIDE_ADDED_DESCRIPTOR = msg({
	message: 'Permission override added',
	comment:
		'Option in the activity log "Filter by action" dropdown. It shows only entries where a new permission override was added. A permission override is a channel-level exception to the permissions of a role or member. It allows or denies chosen permissions in one channel or category only. It can be for a role, a member or @everyone. Use the same word for override as the channel permissions settings, such as Add override. Keep the word override so it does not read as a permission being granted. Word it as a short label like "Channel created", and keep it parallel with "Permission override updated" and "Permission override removed".',
});
const PERMISSION_OVERRIDE_UPDATED_DESCRIPTOR = msg({
	message: 'Permission override updated',
	comment:
		'Option in the activity log "Filter by action" dropdown. It shows only entries where an existing permission override was changed. A permission override is a channel-level exception to the permissions of a role or member. It allows or denies chosen permissions in one channel or category only. It can be for a role, a member or @everyone. Use the same word for override as the channel permissions settings, such as Add override. Keep the word override so it does not read as a permission being changed. Word it as a short label like "Channel updated", and keep it parallel with "Permission override added" and "Permission override removed".',
});
const PERMISSION_OVERRIDE_REMOVED_DESCRIPTOR = msg({
	message: 'Permission override removed',
	comment:
		'Option in the activity log "Filter by action" dropdown. It shows only entries where a permission override was removed from a channel or category. A permission override is a channel-level exception to the permissions of a role or member. It allows or denies chosen permissions in one channel or category only. It can be for a role, a member or @everyone. Use the same word for override as the channel permissions settings, such as Add override. Keep the word override so it does not read as a permission being taken away. Word it as a short label like "Channel deleted", and keep it parallel with "Permission override added" and "Permission override updated".',
});
const MEMBER_KICKED_DESCRIPTOR = msg({
	message: 'Member kicked',
	comment: 'Audit log entry label. A member was kicked (removed) from the community by a moderator.',
});
const MEMBER_BANNED_DESCRIPTOR = msg({
	message: 'Member banned',
	comment: 'Audit log entry label. A member was banned from the community.',
});
const MEMBER_UNBANNED_DESCRIPTOR = msg({
	message: 'Member unbanned',
	comment: 'Audit log entry label. A previously banned member was unbanned.',
});
const MEMBER_UPDATED_DESCRIPTOR = msg({
	message: 'Member updated',
	comment: 'Audit log entry label. A member profile was edited (nickname, etc).',
});
const MEMBER_ROLES_UPDATED_DESCRIPTOR = msg({
	message: 'Member roles updated',
	comment: 'Audit log entry label. The roles assigned to a member were changed.',
});
const MEMBER_MOVED_DESCRIPTOR = msg({
	message: 'Member moved',
	comment: 'Audit log entry label. A member was moved between voice channels by a moderator.',
});
const MEMBER_DISCONNECTED_DESCRIPTOR = msg({
	message: 'Member disconnected',
	comment: 'Audit log entry label. A member was disconnected from a voice channel by a moderator.',
});
const BOT_ADDED_DESCRIPTOR = msg({
	message: 'Bot added',
	comment: 'Audit log entry label. A bot account was added to the community.',
});
const ROLE_CREATED_DESCRIPTOR = msg({
	message: 'Role created',
	comment: 'Audit log entry label. A new role was created in the community.',
});
const ROLE_UPDATED_DESCRIPTOR = msg({
	message: 'Role updated',
	comment: 'Audit log entry label. An existing role was edited.',
});
const ROLE_DELETED_DESCRIPTOR = msg({
	message: 'Role deleted',
	comment: 'Audit log entry label. A role was deleted from the community.',
});
const INVITE_CREATED_DESCRIPTOR = msg({
	message: 'Invite created',
	comment: 'Audit log entry label. A new invite link was created.',
});
const INVITE_DELETED_DESCRIPTOR = msg({
	message: 'Invite deleted',
	comment: 'Audit log entry label. An invite link was revoked or deleted.',
});
const WEBHOOK_CREATED_DESCRIPTOR = msg({
	message: 'Webhook created',
	comment: 'Audit log entry label. A new webhook integration was created.',
});
const WEBHOOK_UPDATED_DESCRIPTOR = msg({
	message: 'Webhook updated',
	comment: 'Audit log entry label. An existing webhook integration was edited.',
});
const WEBHOOK_DELETED_DESCRIPTOR = msg({
	message: 'Webhook deleted',
	comment: 'Audit log entry label. A webhook integration was deleted.',
});
const EMOJI_CREATED_DESCRIPTOR = msg({
	message: 'Emoji created',
	comment: 'Audit log entry label. A custom emoji was added to the community.',
});
const EMOJI_UPDATED_DESCRIPTOR = msg({
	message: 'Emoji updated',
	comment: 'Audit log entry label. A custom emoji was renamed or edited.',
});
const EMOJI_DELETED_DESCRIPTOR = msg({
	message: 'Emoji deleted',
	comment: 'Audit log entry label. A custom emoji was removed from the community.',
});
const STICKER_CREATED_DESCRIPTOR = msg({
	message: 'Sticker created',
	comment: 'Audit log entry label. A custom sticker was added to the community.',
});
const STICKER_UPDATED_DESCRIPTOR = msg({
	message: 'Sticker updated',
	comment: 'Audit log entry label. A custom sticker was edited.',
});
const STICKER_DELETED_DESCRIPTOR = msg({
	message: 'Sticker deleted',
	comment: 'Audit log entry label. A custom sticker was removed from the community.',
});
const MESSAGE_DELETED_DESCRIPTOR = msg({
	message: 'Message deleted',
	comment: 'Audit log entry label. A single message was deleted by a moderator.',
});
const MESSAGES_DELETED_DESCRIPTOR = msg({
	message: 'Messages deleted',
	comment: 'Audit log entry label. Multiple messages were bulk-deleted by a moderator.',
});
const MESSAGE_PINNED_DESCRIPTOR = msg({
	message: 'Message pinned',
	comment: 'Audit log entry label. A message was pinned in a channel.',
});
const MESSAGE_UNPINNED_DESCRIPTOR = msg({
	message: 'Message unpinned',
	comment: 'Audit log entry label. A message was unpinned from a channel.',
});
const ALL_DESCRIPTOR = msg({
	message: 'All',
	comment: 'Audit log target-type filter option. Shows entries for every target type.',
});
const COMMUNITY_DESCRIPTOR = msg({
	message: 'Community',
	comment: 'Audit log target-type filter label. Filters entries to community-level actions.',
});
const MEMBER_DESCRIPTOR = msg({
	message: 'Member',
	comment: 'Audit log target-type filter label. Filters entries that target a community member.',
});
const USER_DESCRIPTOR = msg({
	message: 'User',
	comment: 'Audit log target-type filter label. Filters entries that target a user account.',
});
const ROLE_DESCRIPTOR = msg({
	message: 'Role',
	comment: 'Audit log target-type filter label. Filters entries that target a role.',
});
const CHANNEL_DESCRIPTOR = msg({
	message: 'Channel',
	comment: 'Audit log target-type filter label. Filters entries that target a channel.',
});
const EMOJI_DESCRIPTOR = msg({
	message: 'Emoji',
	comment: 'Audit log target-type filter label. Filters entries that target a custom emoji.',
});
const STICKER_DESCRIPTOR = msg({
	message: 'Sticker',
	comment: 'Audit log target-type filter label. Filters entries that target a custom sticker.',
});
const INVITE_DESCRIPTOR = msg({
	message: 'Invite',
	context: 'invite-noun',
	comment: 'Audit log target-type filter label. Filters entries that target an invite link.',
});
const WEBHOOK_DESCRIPTOR = msg({
	message: 'Webhook',
	comment: 'Audit log target-type filter label. Filters entries that target a webhook.',
});
const MESSAGE_DESCRIPTOR = msg({
	message: 'Message',
	comment: 'Audit log target-type filter label. Filters entries that target an individual message.',
});
export const AUDIT_LOG_TARGET_TYPES = {
	ALL: 'all',
	GUILD: 'guild',
	MEMBER: 'member',
	USER: 'user',
	ROLE: 'role',
	CHANNEL: 'channel',
	EMOJI: 'emoji',
	STICKER: 'sticker',
	INVITE: 'invite',
	WEBHOOK: 'webhook',
	MESSAGE: 'message',
} as const;

export type AuditLogTargetType = ValueOf<typeof AUDIT_LOG_TARGET_TYPES>;

export interface AuditLogActionDefinition {
	value: AuditLogActionType;
	label: MessageDescriptor;
	targetType: AuditLogTargetType;
}

export const AUDIT_LOG_ACTIONS: ReadonlyArray<AuditLogActionDefinition> = [
	{
		value: AuditLogActionType.GUILD_UPDATE,
		label: COMMUNITY_UPDATED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.GUILD,
	},
	{
		value: AuditLogActionType.CHANNEL_CREATE,
		label: CHANNEL_CREATED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.CHANNEL,
	},
	{
		value: AuditLogActionType.CHANNEL_UPDATE,
		label: CHANNEL_UPDATED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.CHANNEL,
	},
	{
		value: AuditLogActionType.CHANNEL_DELETE,
		label: CHANNEL_DELETED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.CHANNEL,
	},
	{
		value: AuditLogActionType.CHANNEL_OVERWRITE_CREATE,
		label: PERMISSION_OVERRIDE_ADDED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.CHANNEL,
	},
	{
		value: AuditLogActionType.CHANNEL_OVERWRITE_UPDATE,
		label: PERMISSION_OVERRIDE_UPDATED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.CHANNEL,
	},
	{
		value: AuditLogActionType.CHANNEL_OVERWRITE_DELETE,
		label: PERMISSION_OVERRIDE_REMOVED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.CHANNEL,
	},
	{
		value: AuditLogActionType.MEMBER_KICK,
		label: MEMBER_KICKED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.MEMBER,
	},
	{
		value: AuditLogActionType.MEMBER_BAN_ADD,
		label: MEMBER_BANNED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.MEMBER,
	},
	{
		value: AuditLogActionType.MEMBER_BAN_REMOVE,
		label: MEMBER_UNBANNED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.MEMBER,
	},
	{
		value: AuditLogActionType.MEMBER_UPDATE,
		label: MEMBER_UPDATED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.MEMBER,
	},
	{
		value: AuditLogActionType.MEMBER_ROLE_UPDATE,
		label: MEMBER_ROLES_UPDATED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.MEMBER,
	},
	{
		value: AuditLogActionType.MEMBER_MOVE,
		label: MEMBER_MOVED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.MEMBER,
	},
	{
		value: AuditLogActionType.MEMBER_DISCONNECT,
		label: MEMBER_DISCONNECTED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.MEMBER,
	},
	{
		value: AuditLogActionType.BOT_ADD,
		label: BOT_ADDED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.MEMBER,
	},
	{
		value: AuditLogActionType.ROLE_CREATE,
		label: ROLE_CREATED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.ROLE,
	},
	{
		value: AuditLogActionType.ROLE_UPDATE,
		label: ROLE_UPDATED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.ROLE,
	},
	{
		value: AuditLogActionType.ROLE_DELETE,
		label: ROLE_DELETED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.ROLE,
	},
	{
		value: AuditLogActionType.INVITE_CREATE,
		label: INVITE_CREATED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.INVITE,
	},
	{
		value: AuditLogActionType.INVITE_DELETE,
		label: INVITE_DELETED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.INVITE,
	},
	{
		value: AuditLogActionType.WEBHOOK_CREATE,
		label: WEBHOOK_CREATED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.WEBHOOK,
	},
	{
		value: AuditLogActionType.WEBHOOK_UPDATE,
		label: WEBHOOK_UPDATED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.WEBHOOK,
	},
	{
		value: AuditLogActionType.WEBHOOK_DELETE,
		label: WEBHOOK_DELETED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.WEBHOOK,
	},
	{
		value: AuditLogActionType.EMOJI_CREATE,
		label: EMOJI_CREATED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.EMOJI,
	},
	{
		value: AuditLogActionType.EMOJI_UPDATE,
		label: EMOJI_UPDATED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.EMOJI,
	},
	{
		value: AuditLogActionType.EMOJI_DELETE,
		label: EMOJI_DELETED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.EMOJI,
	},
	{
		value: AuditLogActionType.STICKER_CREATE,
		label: STICKER_CREATED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.STICKER,
	},
	{
		value: AuditLogActionType.STICKER_UPDATE,
		label: STICKER_UPDATED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.STICKER,
	},
	{
		value: AuditLogActionType.STICKER_DELETE,
		label: STICKER_DELETED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.STICKER,
	},
	{
		value: AuditLogActionType.MESSAGE_DELETE,
		label: MESSAGE_DELETED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.MESSAGE,
	},
	{
		value: AuditLogActionType.MESSAGE_BULK_DELETE,
		label: MESSAGES_DELETED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.MESSAGE,
	},
	{
		value: AuditLogActionType.MESSAGE_PIN,
		label: MESSAGE_PINNED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.MESSAGE,
	},
	{
		value: AuditLogActionType.MESSAGE_UNPIN,
		label: MESSAGE_UNPINNED_DESCRIPTOR,
		targetType: AUDIT_LOG_TARGET_TYPES.MESSAGE,
	},
];

export function getTranslatedAuditLogActions(i18n: I18n): Array<{
	value: AuditLogActionType;
	label: string;
	targetType: AuditLogTargetType;
}> {
	return AUDIT_LOG_ACTIONS.map((action) => ({
		...action,
		label: i18n._(action.label),
	}));
}

export const AUDIT_LOG_TARGET_LABELS: Record<AuditLogTargetType, MessageDescriptor> = {
	[AUDIT_LOG_TARGET_TYPES.ALL]: ALL_DESCRIPTOR,
	[AUDIT_LOG_TARGET_TYPES.GUILD]: COMMUNITY_DESCRIPTOR,
	[AUDIT_LOG_TARGET_TYPES.MEMBER]: MEMBER_DESCRIPTOR,
	[AUDIT_LOG_TARGET_TYPES.USER]: USER_DESCRIPTOR,
	[AUDIT_LOG_TARGET_TYPES.ROLE]: ROLE_DESCRIPTOR,
	[AUDIT_LOG_TARGET_TYPES.CHANNEL]: CHANNEL_DESCRIPTOR,
	[AUDIT_LOG_TARGET_TYPES.EMOJI]: EMOJI_DESCRIPTOR,
	[AUDIT_LOG_TARGET_TYPES.STICKER]: STICKER_DESCRIPTOR,
	[AUDIT_LOG_TARGET_TYPES.INVITE]: INVITE_DESCRIPTOR,
	[AUDIT_LOG_TARGET_TYPES.WEBHOOK]: WEBHOOK_DESCRIPTOR,
	[AUDIT_LOG_TARGET_TYPES.MESSAGE]: MESSAGE_DESCRIPTOR,
};

export function getTranslatedAuditLogTargetLabels(i18n: I18n): Record<AuditLogTargetType, string> {
	const translatedLabels: Record<AuditLogTargetType, string> = {} as Record<AuditLogTargetType, string>;
	for (const [key, descriptor] of Object.entries(AUDIT_LOG_TARGET_LABELS)) {
		translatedLabels[key as AuditLogTargetType] = i18n._(descriptor);
	}
	return translatedLabels;
}
