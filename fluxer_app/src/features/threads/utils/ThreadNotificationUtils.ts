// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import ThreadMemberships from '@app/features/threads/state/ThreadMemberships';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {ThreadMemberFlags} from '@fluxer/constants/src/ThreadConstants';

export const ThreadNotificationSetting = {
	DEFAULT: 'default',
	ALL_MESSAGES: 'all_messages',
	ONLY_MENTIONS: 'only_mentions',
	NO_MESSAGES: 'no_messages',
} as const;

export type ThreadNotificationSetting = (typeof ThreadNotificationSetting)[keyof typeof ThreadNotificationSetting];

export function getThreadParent(thread: Channel): Channel | undefined {
	return thread.parentId ? Channels.getChannel(thread.parentId) : undefined;
}

export function isThreadParentMuted(thread: Channel): boolean {
	return thread.parentId != null && UserGuildSettings.isMutedAtAnyLevel(thread.guildId ?? null, thread.parentId);
}

export function isThreadMutedInSidebar(thread: Channel): boolean {
	return (
		ThreadMemberships.isMuted(thread.id) ||
		(thread.parentId != null && UserGuildSettings.isChannelDirectlyMuted(thread.guildId ?? null, thread.parentId))
	);
}

export function isThreadMuted(thread: Channel): boolean {
	return ThreadMemberships.isMuted(thread.id) || isThreadParentMuted(thread);
}

export function getThreadNotificationSetting(threadId: string): ThreadNotificationSetting {
	const flags = ThreadMemberships.get(threadId)?.flags ?? 0;
	if ((flags & ThreadMemberFlags.ALL_MESSAGES) !== 0) return ThreadNotificationSetting.ALL_MESSAGES;
	if ((flags & ThreadMemberFlags.ONLY_MENTIONS) !== 0) return ThreadNotificationSetting.ONLY_MENTIONS;
	if ((flags & ThreadMemberFlags.NO_MESSAGES) !== 0) return ThreadNotificationSetting.NO_MESSAGES;
	return ThreadNotificationSetting.DEFAULT;
}

export function getThreadNotificationFlag(setting: ThreadNotificationSetting): number {
	switch (setting) {
		case ThreadNotificationSetting.ALL_MESSAGES:
			return ThreadMemberFlags.ALL_MESSAGES;
		case ThreadNotificationSetting.ONLY_MENTIONS:
			return ThreadMemberFlags.ONLY_MENTIONS;
		case ThreadNotificationSetting.NO_MESSAGES:
			return ThreadMemberFlags.NO_MESSAGES;
		default:
			return 0;
	}
}

export function resolveThreadNotificationLevel(thread: Channel): number {
	switch (getThreadNotificationSetting(thread.id)) {
		case ThreadNotificationSetting.ALL_MESSAGES:
			return MessageNotifications.ALL_MESSAGES;
		case ThreadNotificationSetting.ONLY_MENTIONS:
			return MessageNotifications.ONLY_MENTIONS;
		case ThreadNotificationSetting.NO_MESSAGES:
			return MessageNotifications.NO_MESSAGES;
	}
	const parent = getThreadParent(thread);
	if (!parent) return MessageNotifications.ONLY_MENTIONS;
	return UserGuildSettings.resolveEffectiveMessageNotifications({
		id: parent.id,
		guildId: parent.guildId,
		parentId: parent.parentId ?? undefined,
		type: parent.type,
	});
}
